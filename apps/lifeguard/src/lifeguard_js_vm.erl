-module(lifeguard_js_vm).
-behavior(gen_server).
-export([start_link/1,
         dispatch/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% The number of watches we run before forcing a GC run
-define(GC_TICKS, 256).

% The number of watches we run before we recycle the entire
% V8 VM just in case there are memory leaks.
-define(MAX_TICKS, 32768).

% The maximum depth that the JS to Erlang conversion recurses until
% it throws an error.
-define(MAX_CONVERT_DEPTH, 16).

-include_lib("erlv8/include/erlv8.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(vm_state, {
        id,   % ID of the VM
        vm,   % Actual V8 VM
        ticks % Number of ticks we've ran so far
    }).

%% @doc Start the JS VM under a supervision tree.
start_link(Number) ->
    gen_server:start_link({local, vmid(Number)}, ?MODULE, [Number], []).

%% @doc Dispatches a method to the proper VM.
dispatch(VMID, Message) ->
    gen_server:cast(vmid(VMID), Message).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Number]) ->
    lager:info("JS VM started: ~p", [Number]),

    % Create a new V8 VM
    {ok, VM} = erlv8_vm:start(),
    init_vm_globals(VM),

    % Create our state
    State = #vm_state{
        id = Number,
        vm = VM,
        ticks = 0
    },

    % Mark ourselves as idle, ready to receive work
    set_idle(State#vm_state.id),
    {ok, State}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({run_watch, Watch, _From}, State) ->
    {ok, Name} = lifeguard_watch:get_name(Watch),
    lager:info("Running watch in VM: ~p", [Name]),

    % Notify the manager that we're running
    lifeguard_watch_manager:vm_msg({running, Name}),

    % Call into JavaScript!
    Global = erlv8_vm:global(State#vm_state.vm),
    JS_Lifeguard = Global:get_value("Lifeguard"),
    Result = case JS_Lifeguard:get_value("_call") of
        undefined ->
            % XXX: Crash the process here?
            lager:error("SEVERE ERROR: _call method not found in JS VM."),
            error;

        JS_CallMethod ->
            % We found our call method, so we call it with the code
            % of the watch.
            {ok, Code} = lifeguard_watch:get_code(Watch),
            case JS_CallMethod:call([Code]) of
                {throw, {error, ErrVal}} ->
                    ErrMessage = ErrVal:get_value("message"),
                    ErrStack = ErrVal:get_value("stack"),
                    lager:warning("Error in JS: ~p ~p", [ErrMessage, ErrStack]),
                    error;
                {throw, Other} ->
                    lager:error("Unknown JS VM error: ~p", [Other]),
                    error;
                Object ->
                    ResultString = Object:get_value("result"),
                    ResultAtom   = binary_to_atom(ResultString, utf8),
                    lager:info("JS RESULT: ~p", [ResultAtom]),
                    ResultAtom
            end
    end,

    % Tell the watch manager the results of the run so it can do what
    % it needs with it.
    lifeguard_watch_manager:vm_msg({complete, Name, Result}),

    % Increment a tick, which controls our GC and memory usage of V8
    NewState = increment_ticks(State),

    % Notify the JS manager that we're idle and ready for more work
    set_idle(State#vm_state.id),
    {noreply, NewState}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, State) ->
    lager:info("JS VM stopped: ~p", [State#vm_state.id]),

    % Stop the V8 VM
    erlv8_vm:stop(State#vm_state.vm).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Converts an Erlang value to a value compatible with V8.
erl_to_js(Binary) when is_binary(Binary) ->
    % Binaries are just strings, so we return them as-is
    Binary;
erl_to_js(Number) when is_number(Number) ->
    % Numbers can be returned as-is as well
    Number;
erl_to_js(Atom) when is_atom(Atom) ->
    case Atom of
        % null, false, and true are special in that they turn into JavaScript
        % primitives of the same names, so just return
        false -> false;
        true -> true;
        null -> null;

        % Otherwise, convert it to a JS String
        Other -> atom_to_binary(Other, utf8)
    end;
erl_to_js(List) when is_list(List) ->
    % For our case, we want the empty list to NOT be a proplist (although
    % technically it is a valid proplist), since we want that to signal
    % an empty array.
    IsProplist = case List of
        [] -> false;
        Other -> is_proplist(Other)
    end,

    case IsProplist of
        true ->
            % This is a proplist, so we convert each item to a JS object
            % (keys and values), and then we return our own JS object
            ConvertedProps = lists:map(fun({Key, Value}) ->
                            JSKey = erl_to_js(Key),
                            JSVal = erl_to_js(Value),
                            {JSKey, JSVal}
                    end, List),

            % Build the actual V8 object
            erlv8_object:new(ConvertedProps);
        false ->
            % This is not a proplist, so we just convert it to an array.
            ConvertedList = lists:map(fun(Value) -> erl_to_js(Value) end, List),
            erlv8_array:new(ConvertedList)
    end;
erl_to_js(Other) ->
    lager:error("Unknown Erlang value to convert to JS: ~p", [Other]),
    throw({bad_value, Other}).

increment_ticks(State) ->
    NewTicks = State#vm_state.ticks + 1,
    if
        NewTicks >= ?MAX_TICKS ->
            % The MAX_TICKS are reached, which is our limit in trusting
            % erlv8 with memory management, so we just recycle the entire
            % VM, to be safe. This doesn't happen that often so its not
            % a big hit on performance.
            lager:debug("VM ~p MAX_TICKS reached. Recycling.", [State#vm_state.id]),
            erlv8_vm:stop(State#vm_state.vm),
            {ok, VM} = erlv8_vm:start(),
            init_vm_globals(VM),
            State#vm_state{vm=VM, ticks=0};

        NewTicks >= ?GC_TICKS ->
            % The GC_TICKS are reached, which is our limit before we
            % invoke the GC.
            lager:debug("VM ~p GC_TICKS reached. GC running.", [State#vm_state.id]),
            erlv8_vm:gc(State#vm_state.vm),
            State#vm_state{ticks=0};

        true ->
            % Otherwise, just increment the ticks
            State#vm_state{ticks=NewTicks}
    end.

init_vm_globals(VM) ->
    % Load our builtins
    Path = filename:join(code:priv_dir(lifeguard), "builtins.js"),
    {ok, JSData} = file:read_file(Path),

    % Add the builtins to this VM runtime
    {ok, _} = erlv8_vm:run(VM, binary_to_list(JSData)),

    % Override some of the Lifeguard object methods with Erlang
    % implementations.
    ErlangJSMethods = [
            {"_erl_get", fun(_, Args) -> js_get(Args) end}
        ],

    Global = erlv8_vm:global(VM),
    Lifeguard = Global:get_value("Lifeguard"),
    lists:foreach(fun({Name, Method}) ->
                Lifeguard:set_value(Name, Method)
        end, ErlangJSMethods).

%% @doc Returns a boolean of whether a given list is a valid proplist
%% or not.
is_proplist([]) ->
    true;
is_proplist([{_Key, _Value} | Rest]) ->
    is_proplist(Rest);
is_proplist(_Other) ->
    false.

%% @doc Converts an array of JavaScript values into Erlang values. Arrays
%% are turned into lists, objects are turned into proplists.
js_convert_args(Args) ->
    js_convert_args1([], Args).
js_convert_args1(Result, []) ->
    Result;
js_convert_args1(Result, [Arg | Args]) ->
    lager:debug("Converting JS value to Erlang: ~p", [Arg]),
    Converted = js_convert_value(Arg),
    js_convert_args1([Converted | Result], Args).

%% @doc Converts a single JavaScript value to an Erlang value. Arrays are
%% turned into lists, objects are turned into proplists. This will automatically
%% recurse into objects.
%%
%% XXX: Handle circular references by keeping track of "depth"
js_convert_value(Value) ->
    js_convert_value1(Value, 0).

js_convert_value1(_Object, ?MAX_CONVERT_DEPTH) ->
    % We recursed in too deeply, so we throw an exception!
    throw(max_convert_depth);
js_convert_value1(Atom, _Depth) when is_atom(Atom) ->
    % Atoms can be: null, false, true. We just let these be
    Atom;
js_convert_value1(Number, _Depth) when is_number(Number) ->
    % Numbers are just numbers, let them be
    Number;
js_convert_value1(String, _Depth) when is_binary(String) ->
    % Binaries are just strings, let them be
    String;
js_convert_value1(Object, Depth) when is_record(Object, erlv8_object) ->
    % Convert the V8 object into an Erlang proplist
    js_convert_value_object(Object:proplist(), Depth, []);
js_convert_value1(Object, Depth) when is_record(Object, erlv8_array) ->
    % Convert the V8 array into an Erlang list
    js_convert_value_array(Object:list(), Depth, []);
js_convert_value1(Other, _Depth) ->
    lager:error("Unknown JS type: ~p", [Other]),
    unknown.

%% @doc Converts a JavaScript array to an Erlang list.
js_convert_value_array([], _Depth, Result) ->
    % Since we fold over the array in order, its built up in reverse order,
    % so we need to reverse here to get back the order we sent in.
    lists:reverse(Result);
js_convert_value_array([Object | Rest], Depth, Result) ->
    ErlValue = js_convert_value1(Object, Depth + 1),
    js_convert_value_array(Rest, Depth, [ErlValue | Result]).

%% @doc Converts a JavaScript object to an Erlang object, recursively converting
%% keys and values as necessary.
js_convert_value_object([], _Depth, Result) ->
    Result;
js_convert_value_object([{Key, Value} | Rest], Depth, Result) ->
    ErlKey = js_convert_value1(Key, Depth + 1),
    ErlValue = js_convert_value1(Value, Depth + 1),
    js_convert_value_object(Rest, Depth, [{ErlKey, ErlValue} | Result]).

%% @doc Gets values from a data source and returns them back to JavaScript.
%% This method is the implementation of a function called from JavaScript.
js_get([DataSource, Args]) ->
    % Convert the array of JavaScript arguments to Erlang values
    ErlArgs = js_convert_args(Args:list()),
    lager:info("Get: ~p ~p", [DataSource, ErlArgs]),

    % Get the actual result
    case lifeguard_ds_manager:get(DataSource, ErlArgs) of
        {ok, Result} ->
            lager:debug("Result: ~p", [Result]),
            erl_to_js(Result);
        {error, Message} ->
            lager:debug("Error: ~p", [Message]),
            {throw, {error, atom_to_binary(Message, utf8)}}
    end.

set_idle(VMID) ->
    lifeguard_js_manager:idle_vm(VMID).

vmid(Number) ->
    list_to_atom("js_vm_" ++ integer_to_list(Number)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

erl_to_js_primitive_test() ->
    5 = erl_to_js(5),
    false = erl_to_js(false),
    true = erl_to_js(true),
    null = erl_to_js(null),
    <<"foo">> = erl_to_js(<<"foo">>),
    <<"bar">> = erl_to_js(bar).

erl_to_js_list_test() ->
    JSList = erl_to_js([1,2,3]),
    ?assert(is_record(JSList, erlv8_array)).

erl_to_js_object_test() ->
    JSObject = erl_to_js([{key, value}]),
    ?assert(is_record(JSObject, erlv8_object)).

is_proplist_test() ->
    false = is_proplist(12),
    true = is_proplist([]),
    true = is_proplist([{key, value}]),
    true = is_proplist([{key, value}, {key2, value2}]).

vm_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_js_convert_primitive_value/1,
            fun test_js_convert_array/1,
            fun test_js_convert_array_recursive/1,
            fun test_js_convert_max_depth/1,
            fun test_js_convert_object/1,
            fun test_js_convert_object_recursive/1
        ]}.

setup() ->
    % This may already be started but we just try to start it anyways
    % because we do need it for our tests.
    application:start(erlv8),

    % Start up a new V8 VM, because each test does need an isolated
    % one of these.
    {ok, VM} = erlv8_vm:start(),
    VM.

teardown(VM) ->
    erlv8_vm:stop(VM).

test_js_convert_primitive_value(_VM) ->
    fun() ->
            null = js_convert_value(null),
            true = js_convert_value(true),
            false = js_convert_value(false),
            <<"foo">> = js_convert_value(<<"foo">>),
            unknown = js_convert_value([])
    end.

test_js_convert_array(VM) ->
    fun() ->
            {ok, Object} = erlv8_vm:run(VM, "var foo = [1,2,3]; foo"),
            [1, 2, 3] = js_convert_value(Object)
    end.

test_js_convert_array_recursive(VM) ->
    fun() ->
            {ok, Object} = erlv8_vm:run(VM, "var foo = [[1,2],[3,4]]; foo"),
            [[1,2], [3,4]] = js_convert_value(Object)
    end.

%% @doc Tests to make sure that when we create a JavaScript object that
%% matches the MAX_CONVERT_DEPTH, then we get a proper exception thrown.
test_js_convert_max_depth(VM) ->
    fun() ->
            JSArrayCode = lists:foldl(fun(_Elem, Result) ->
                            "[" ++ Result ++ "]"
                    end, "1", lists:seq(1, ?MAX_CONVERT_DEPTH)),
            {ok, Object} = erlv8_vm:run(VM, "var foo = " ++ JSArrayCode ++ "; foo"),

            ok = try js_convert_value(Object) of
                _ ->
                    % Not okay because we want it throw an exception
                    not_ok
            catch
                throw:max_convert_depth -> ok
            end
    end.

test_js_convert_object(VM) ->
    fun() ->
            {ok, Object} = erlv8_vm:run(VM, "var foo = { \"a\": \"a_value\" }; foo"),
            Result = js_convert_value(Object),
            {<<"a">>, <<"a_value">>} = proplists:lookup(<<"a">>, Result)
    end.

test_js_convert_object_recursive(VM) ->
    fun() ->
            {ok, Object} = erlv8_vm:run(VM, "var foo = { \"a\": { \"b\": 3 } }; foo"),
            Result = js_convert_value(Object),

            {<<"a">>, Inner} = proplists:lookup(<<"a">>, Result),
            {<<"b">>, 3} = proplists:lookup(<<"b">>, Inner)
    end.

-endif.
