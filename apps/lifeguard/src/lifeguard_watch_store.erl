-module(lifeguard_watch_store).
-behavior(gen_server).
-export([start_link/1]).
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
            {ok, record_to_model(Watch)};
        {error, Reason} -> {error, Reason}
    end,

    {reply, Result, State};
handle_call(list, _From, State) ->
    lager:info("Listing watches~n"),

    % Convert the internal list to some external format
    List   = internal_list_watches(),
    Models = lists:map(fun record_to_model/1, List),
    Result = {ok, Models},
    {reply, Result, State};
handle_call({set, Watch}, _From, State) ->
    WatchRec = model_to_record(Watch),
    lager:info("Setting watch: ~p~n", [WatchRec#watch.name]),
    {reply, internal_set_watch(WatchRec), State};
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

%% @doc Converts a model object to our internal record object.
model_to_record(Watch) ->
    {ok, Name} = lifeguard_watch:get_name(Watch),
    {ok, Code} = lifeguard_watch:get_code(Watch),
    {ok, Interval} = lifeguard_watch:get_interval(Watch),

    #watch{
        name = Name,
        code = Code,
        interval = Interval
    }.

%% @doc Converts our internal record we use to store in the DETS table to
%% the actual lifeguard model object for a watch.
record_to_model(Watch) when is_record(Watch, watch) ->
    WatchRes  = lifeguard_watch:new(),
    WatchRes2 = lifeguard_watch:set_name(WatchRes, Watch#watch.name),
    WatchRes3 = lifeguard_watch:set_code(WatchRes2, Watch#watch.code),
    lifeguard_watch:set_interval(WatchRes3, Watch#watch.interval).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

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
            fun test_model_to_record/1,
            fun test_record_to_model/1,
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

test_model_to_record(_) ->
    fun() ->
            M1 = lifeguard_watch:new(),
            M2 = lifeguard_watch:set_name(M1, "foo"),
            M3 = lifeguard_watch:set_code(M2, "bar"),
            M4 = lifeguard_watch:set_interval(M3, 5),

            Rec = model_to_record(M4),
            "foo" = Rec#watch.name,
            "bar" = Rec#watch.code,
            5     = Rec#watch.interval
    end.

test_record_to_model(_) ->
    fun() ->
            Record = #watch{name="foo", code="bar", interval=5},
            Watch  = record_to_model(Record),
            {ok, "foo"} = lifeguard_watch:get_name(Watch),
            {ok, "bar"} = lifeguard_watch:get_code(Watch),
            {ok, 5}     = lifeguard_watch:get_interval(Watch)
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
