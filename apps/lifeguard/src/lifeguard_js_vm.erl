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
    {ok, _} = erlv8_vm:run(VM, binary_to_list(JSData)).

set_idle(VMID) ->
    lifeguard_js_manager:idle_vm(VMID).

vmid(Number) ->
    list_to_atom("js_vm_" ++ integer_to_list(Number)).
