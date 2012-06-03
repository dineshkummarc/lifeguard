%% @doc The watch manager is responsible for all changes to watches.
%% It orchestrates the changes between the various watch subsystems,
%% such as storage, the scheduler, etc.
-module(lifeguard_watch_manager).
-behavior(gen_server).
-export([start_link/1,
         delete_watch/1,
         get_watch/1,
         list_watches/0,
         set_watch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc This is the internal state of our gen_server.
-record(state, {
        store_pid, % Pid of the storage process
        watch_tab  % ETS of all the watches
    }).

%% @doc The record type that represents an in-memory watch. The manager
%% stores every watch it is responsible for in memory in this structure.
-record(watch, {
        id,        % ID of the watch
        state,     % State of the watch
        timer_ref  % Reference for the timer if it is waiting
    }).

-define(TABLE_NAME, in_memory_watches).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the watch manager.
start_link(StoragePath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StoragePath, []).

%% @doc Delete a watch. This will remove it from the underlying
%% backing storage.
-spec delete_watch(string()) -> ok.
delete_watch(Name) ->
    gen_server:call(?MODULE, {delete, Name}).

%% @doc Get a watch by name.
-spec get_watch(string()) -> ok.
get_watch(Name) ->
    gen_server:call(?MODULE, {get, Name}).

%% @doc List all the watches in the system.
-spec list_watches() -> {ok, [term()]}.
list_watches() ->
    gen_server:call(?MODULE, list).

%% @doc Add or update an existing watch.
-spec set_watch(term()) -> ok | {error, term()}.
set_watch(Watch) ->
    gen_server:call(?MODULE, {set, Watch}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(StoragePath) ->
    lager:info("Watch manager starting..."),

    % Start the storage process
    lager:debug("Starting watch store..."),
    {ok, Pid} = lifeguard_watch_store:start_link(StoragePath),

    % Get a list of all the watches and populate our list
    WatchTab = ets:new(?TABLE_NAME, [set, private]),
    {ok, Watches} = gen_server:call(Pid, list),
    populate_watch_table(WatchTab, Watches),
    lager:debug("Populated initial watch table with ~p watches", [length(Watches)]),

    % Schedule all the watches
    schedule_watch_table(WatchTab, Pid),

    % Log it out and start
    lager:info("Watch manager started."),
    {ok, #state{
            store_pid = Pid,
            watch_tab = WatchTab
        }}.

handle_call({delete, Name}, _From, #state{store_pid=StorePid}=State) ->
    lager:info("Delete watch: ~p~n", [Name]),
    Result = gen_server:call(StorePid, {delete, Name}),
    {reply, Result, State};
handle_call({get, Name}, _From, #state{store_pid=StorePid}=State) ->
    lager:info("Getting watch: ~p~n", [Name]),
    Result = gen_server:call(StorePid, {get, Name}),
    {reply, Result, State};
handle_call(list, _From, #state{store_pid=StorePid}=State) ->
    lager:info("Listing watches~n"),
    Result = gen_server:call(StorePid, list),
    {reply, Result, State};
handle_call({set, Watch}, _From, #state{store_pid=StorePid}=State) ->
    lager:info("Setting watch: ~p~n", [lifeguard_watch:get_name(Watch)]),
    Result = gen_server:call(StorePid, {set, Watch}),
    {reply, Result, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({run, ID}, State) ->
    lager:info("Run: ~p", [ID]),
    {noreply, State};
handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, #state{store_pid=StorePid, watch_tab=Tab}) ->
    lager:debug("Terminating the watch store..."),
    gen_server:call(StorePid, stop),

    lager:debug("Deleting the in-memory store of watches..."),
    ets:delete(Tab),

    lager:info("Watch manager terminated.").

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Converts a model watch object to our internal record.
model_to_record(Watch) ->
    {ok, ID} = lifeguard_watch:get_name(Watch),
    #watch{
        id = ID,
        state = idle,
        timer_ref = undefined
    }.

%% @doc Populates the in-memory watch table with a list of model
%% objects.
populate_watch_table(_Tab, []) ->
    ok;
populate_watch_table(Tab, [Watch | Rest]) ->
    Record = model_to_record(Watch),
    ets:insert(Tab, {Record#watch.id, Record}),
    populate_watch_table(Tab, Rest).

%% @doc Schedules a watch. This will return the timer reference.
schedule_watch(Tab, StorePid, ID) ->
    % Find the record in the ETS table
    [{ID, Record}] = ets:lookup(Tab, ID),
    {ok, Watch}    = gen_server:call(StorePid, {get, Record#watch.id}),

    % Schedule it
    {ok, Interval} = lifeguard_watch:get_interval(Watch),
    {ok, TRef} = timer:send_after(Interval, ?MODULE, {run, ID}),
    lager:info("Watch '~p' scheduled to run in ~p ms", [ID, Interval]),

    % Update the record and write it
    RecordNew = Record#watch{state = scheduled, timer_ref = TRef},
    ets:insert(Tab, {ID, RecordNew}),
    ok.

%% @doc Schedules an entire table of watches.
schedule_watch_table(Tab, StorePid) ->
    Key = ets:first(Tab),
    schedule_watch_table(Tab, StorePid, Key).
schedule_watch_table(_Tab, _StorePid, '$end_of_table') ->
    ok;
schedule_watch_table(Tab, StorePid, Key) ->
    % Schedule it
    schedule_watch(Tab, StorePid, Key),

    % Get the next key and loop
    NextKey = ets:next(Tab, Key),
    schedule_watch_table(Tab, StorePid, NextKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-record(test_state, {
        table, % Handle to an ETS table
        store  % Pid to a store
    }).

model_to_record_test() ->
    M1 = lifeguard_watch:new(),
    M2 = lifeguard_watch:set_name(M1, "foo"),
    M3 = lifeguard_watch:set_code(M2, "bar"),
    M4 = lifeguard_watch:set_interval(M3, 5),

    Rec = model_to_record(M4),
    "foo" = Rec#watch.id,
    idle  = Rec#watch.state,
    undefined = Rec#watch.timer_ref.

% Test runner for testing all the methods that require an ets table
ets_test_() ->
    {foreach,
        fun ets_setup/0,
        fun ets_teardown/1,
        [
            fun test_populate_watch_table/1,
            fun test_schedule_watch/1
        ]}.

ets_setup() ->
    % Create the ETS table. We do this by looking up if a previous ETS
    % table for this test has been made. If so, then we use it but first
    % clear out the objects. If not, then we create a new ETS table. The
    % final state of the tests is the ETS table reference.
    Name = test_table,
    Tab = case ets:info(Name) of
        undefined ->
            ets:new(Name, [set, public, named_table]);
        _Other ->
            ets:delete_all_objects(Name),
            Name
    end,

    {ok, StorePid} = {ok, todo},
    #test_state{table=Tab, store=StorePid}.

ets_teardown(#test_state{table=Tab}) ->
    % Delete the ETS table
    ets:delete(Tab).

test_populate_watch_table(#test_state{table=Tab}) ->
    fun() ->
            % Populate it with a list of model objects
            A1 = lifeguard_watch:new(),
            A2 = lifeguard_watch:set_name(A1, "foo"),
            B1 = lifeguard_watch:new(),
            B2 = lifeguard_watch:set_name(B1, "bar"),
            ok = populate_watch_table(Tab, [A2, B2]),

            % Find the items in the table to verify they are there
            [AResult] = ets:lookup(Tab, "foo"),
            [BResult] = ets:lookup(Tab, "bar")
    end.

test_schedule_watch(#test_state{table=Tab, store=StorePid}) ->
    fun() ->
            ok
    end.

-endif.
