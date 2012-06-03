-module(lifeguard_watch_store).
-behavior(gen_server).
-export([start_link/1,
         watch_read_code/1,
         watch_read_name/1,
         watch_read_interval/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc The record type that represents a Watch.
-record(watch, {
        name,    % Name for the watch
        code,    % Code for the watch (JavaScript)
        interval % Interval that it runs in milliseconds
    }).

-define(TABLE_NAME, table).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the watch store.
start_link(StoragePath) ->
    gen_server:start_link(?MODULE, StoragePath, []).

%% @doc Read the code of a watch.
watch_read_code(#watch{code = Code}) ->
    Code.

%% @doc Read the name of a watch.
watch_read_name(#watch{name = Name}) ->
    Name.

%% @doc Read the interval of a watch.
watch_read_interval(#watch{interval = Interval}) ->
    Interval.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(StoragePath) ->
    % Open the dets table
    {ok, ?TABLE_NAME} = dets:open_file(?TABLE_NAME, [
            {file, StoragePath},
            {auto_save, 60000}
        ]),

    % Log it out and start
    lager:info("Watch store started. Storage path: ~p", [StoragePath]),
    {ok, no_state}.

handle_call({delete, Name}, _From, State) ->
    lager:info("Delete watch: ~p~n", [Name]),
    {reply, internal_delete_watch(Name), State};
handle_call({get, Name}, _From, State) ->
    lager:info("Getting watch: ~p~n", [Name]),
    Result = case internal_get_watch(Name) of
        {ok, Watch} when is_record(Watch, watch) ->
            Name = Watch#watch.name,
            Code = Watch#watch.code,
            Interval = Watch#watch.interval,
            {ok, {Name, Code, Interval}};
        {error, Reason} -> {error, Reason}
    end,

    {reply, Result, State};
handle_call(list, _From, State) ->
    lager:info("Listing watches~n"),

    % Convert the internal list to some external format
    Result = {ok, record_to_external(internal_list_watches())},
    {reply, Result, State};
handle_call({set, Name, Code, Interval}, _From, State) ->
    lager:info("Setting watch: ~p~n", [Name]),
    Watch = #watch{
            name = Name,
            code = Code,
            interval = Interval
        },

    {reply, internal_set_watch(Watch), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Watch store terminated."),
    ok = dets:close(?TABLE_NAME).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

internal_delete_watch(Name) ->
    dets:delete(?TABLE_NAME, Name).

internal_get_watch(Name) ->
    case dets:lookup(?TABLE_NAME, Name) of
        [] -> {error, no_watch};
        [{Name, Watch}] -> {ok, Watch}
    end.

internal_list_watches() ->
    dets:foldl(fun(X, Acc) ->
                {_Name, Data} = X,
                [Data | Acc]
        end, [], ?TABLE_NAME).

internal_set_watch(#watch{name=Name} = Watch) ->
    dets:insert(?TABLE_NAME, {Name, Watch});
internal_set_watch(_) ->
    {error, invalid_watch}.

record_to_external(Watches) ->
    record_to_external1(Watches, []).

record_to_external1([], Acc) ->
    Acc;
record_to_external1([Watch | Rest], Acc) ->
    Single = {Watch#watch.name, Watch#watch.code, Watch#watch.interval},
    record_to_external1(Rest, [Single | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

% Test the referentially transparent stuff
watch_read_code_test() ->
    Watch = #watch{code = "code"},
    "code" = watch_read_code(Watch).

watch_read_interval_test() ->
    Watch = #watch{interval = 24},
    24 = watch_read_interval(Watch).

watch_read_name_test() ->
    Watch = #watch{name = "Foo"},
    "Foo" = watch_read_name(Watch).

% Test runner for testing all the methods that requires a dets table
% that stores watches.
main_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_delete_watch/1,
            fun test_delete_watch_nonexistent/1,
            fun test_get_watch_nonexistent/1,
            fun test_list_watch_empty/1,
            fun test_list_watch_single/1,
            fun test_record_to_external_list/1,
            fun test_set_watch_invalid/1,
            fun test_set_and_get_watch/1,
            fun test_update_watch/1
        ]}.

setup() ->
    % Open up a dets table
    StoragePath = ?cmd("mktemp -t lifeguard"),
    {ok, ?TABLE_NAME} = dets:open_file(?TABLE_NAME, [{file, StoragePath}]),

    % State is just the path to the table
    StoragePath.

teardown(StoragePath) ->
    ok = dets:close(?TABLE_NAME),
    ?cmd("rm " ++ StoragePath).

test_delete_watch(_) ->
    fun() ->
            Name = "key",
            Watch = #watch{name=Name},
            ok = internal_set_watch(Watch),
            ok = internal_delete_watch(Name),
            {error, no_watch} = internal_get_watch(Name)
    end.

test_delete_watch_nonexistent(_) ->
    fun() ->
            ok = internal_delete_watch("nope")
    end.

test_get_watch_nonexistent(_) ->
    fun() ->
            {error, no_watch} = internal_get_watch("no good")
    end.

test_list_watch_empty(_) ->
    fun() ->
            [] = internal_list_watches()
    end.

test_list_watch_single(_) ->
    fun() ->
            Watch = #watch{name="foo", code="baz", interval=5},
            ok = internal_set_watch(Watch),
            [Result] = internal_list_watches(),
            Watch = Result
    end.

test_record_to_external_list(_) ->
    fun() ->
            % Test the single case
            [] = record_to_external([]),

            % Test when we have a watch
            Watch1  = #watch{name="foo", code="bar", interval=1},
            [{"foo", "bar", 1}] = record_to_external([Watch1])
    end.

test_set_watch_invalid(_) ->
    fun() ->
            {error, invalid_watch} = internal_set_watch("NOT A WATCH!")
    end.

test_set_and_get_watch(_) ->
    fun() ->
            Name  = "key",
            Watch = #watch{name=Name},
            ok = internal_set_watch(Watch),
            {ok, Watch} = internal_get_watch(Name)
    end.

test_update_watch(_) ->
    fun() ->
            Name = "key",
            Watch = #watch{name=Name, interval=1},

            % Set the initial watch
            ok = internal_set_watch(Watch),

             % Update it
            ok = internal_set_watch(Watch#watch{interval=2}),

            % Check the result
            {ok, Result} = internal_get_watch(Name),

            ?assertEqual(2, Result#watch.interval)
    end.

-endif.
