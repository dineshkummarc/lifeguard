%% @doc The watch manager is responsible for all changes to watches.
%% It orchestrates the changes between the various watch subsystems,
%% such as storage, the scheduler, etc.
-module(lifeguard_watch_manager).
-behavior(gen_server).
-export([start_link/1,
         delete_watch/1,
         get_watch/1,
         list_watches/0,
         set_watch/1,
         vm_msg/1]).
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
        result,    % Result of the watch
        state,     % State of the watch
        timer_ref, % Reference for the timer if it is waiting
        timer_at   % The time when the timer should run within reasonable error bounds
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

%% @doc Sends a message that came froma JS VM.
vm_msg(Message) ->
    gen_server:cast(?MODULE, {vm_msg, Message}).

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
    table_populate_watches(WatchTab, Watches),
    lager:debug("Populated initial watch table with ~p watches", [length(Watches)]),

    % Schedule all the watches
    schedule_watch_table(WatchTab, Pid),

    % Log it out and start
    lager:info("Watch manager started."),
    {ok, #state{
            store_pid = Pid,
            watch_tab = WatchTab
        }}.

handle_call({delete, Name}, _From, State) ->
    StorePid = State#state.store_pid,
    Tab      = State#state.watch_tab,

    lager:info("Delete watch: ~p", [Name]),

    % Unschedule the watch
    lager:debug("Unscheduling watch due to delete: ~p", [Name]),
    {ok, Record} = table_get_watch(Tab, Name),
    ok = unschedule(Record#watch.timer_ref),

    % Delete the watch from the backing store
    lager:debug("Deleting watch from backing store: ~p", [Name]),
    Result = gen_server:call(StorePid, {delete, Name}),

    % Delete the watch from the table
    table_delete_watch(Tab, Name),

    {reply, Result, State};
handle_call({get, Name}, _From, State) ->
    StorePid = State#state.store_pid,
    Tab      = State#state.watch_tab,

    % Grab the watch from the backing store
    lager:info("Getting watch: ~p", [Name]),
    Result = case gen_server:call(StorePid, {get, Name}) of
        {ok, Watch} ->
            % Set the transient data on it based on our metadata
            {ok, Record} = table_get_watch(Tab, Name),
            {ok, watch_append_transient(Record, Watch)};
        Else -> Else
    end,

    {reply, Result, State};
handle_call(list, _From, State) ->
    StorePid = State#state.store_pid,
    Tab      = State#state.watch_tab,

    % Get the watches from the backing store then append transient data
    % to all of them.
    lager:info("Listing watches"),
    Result = case gen_server:call(StorePid, list) of
        {ok, Watches} ->
            Watches2 = lists:map(fun(Watch) ->
                        {ok, Name}   = lifeguard_watch:get_name(Watch),
                        {ok, Record} = table_get_watch(Tab, Name),
                        watch_append_transient(Record, Watch)
                end, Watches),
            {ok, Watches2};
        Other -> Other
    end,

    {reply, Result, State};
handle_call({set, Watch}, _From, #state{store_pid=StorePid, watch_tab=Tab} = State) ->
    ID = lifeguard_watch:get_name(Watch),

    lager:info("Set watch: ~p", [lifeguard_watch:get_name(Watch)]),

    % Save the watch to the backing store
    lager:debug("Storing watch in backing store..."),
    ok = gen_server:call(StorePid, {set, Watch}),

     % Schedule this watch if it isn't already scheduled
    Record = case table_get_watch(Tab, ID) of
        {ok, Rec} -> Rec;
        undefined ->
            % This watch is not in our in-memory table yet! Create it
            {ok, Rec} = table_add_watch_model(Tab, Watch),
            Rec
    end,
    {ok, RecordNew} = schedule_watch_if_needed(Tab, StorePid, Record),

    % Append any extra info for the result
    Result = watch_append_transient(RecordNew, Watch),

    {reply, {ok, Result}, State}.

handle_cast({vm_msg, {running, ID}}, State) ->
    Tab = State#state.watch_tab,
    {ok, Watch} = table_get_watch(Tab, ID),

    % Mark that this watch is running
    lager:info("Updating state of watch '~p' to 'running'", [ID]),
    NewWatch = Watch#watch{state = running},
    table_set_watch(Tab, NewWatch),

    {noreply, State};
handle_cast({vm_msg, {complete, ID, Result}}, State) ->
    Tab = State#state.watch_tab,
    {ok, Watch} = table_get_watch(Tab, ID),

    % Update the result state of this watch
    lager:info("Watch run completed for '~p'. Result: ~p", [ID, Result]),
    NewWatch = Watch#watch{state = scheduled, result = Result},
    table_set_watch(Tab, NewWatch),

    {noreply, State}.

handle_info({run, ID}, State) ->
    StorePid = State#state.store_pid,
    Tab = State#state.watch_tab,

    % Get the record and watch
    {ok, Record} = table_get_watch(Tab, ID),
    {ok, Watch}  = store_get_watch(StorePid, ID),

    % Reschedule immediately, despite whether we are actually going to run
    % or not, since we want to remain on the same interval if possible.
    {ok, TRef, TimerAt} = reschedule_watch(Record#watch.timer_ref, Watch),
    Record2  = Record#watch{
            timer_ref = TRef,
            timer_at  = TimerAt
        },
    {ok, Record2} = table_set_watch(Tab, Record2),

    % Now we need to determine whether we're actually going to run or not.
    % More details in the individual case clauses.
    case Record2#watch.state of
        scheduled ->
            % Okay, we're "scheduled" which means we're just waiting to run,
            % so let's run it. First, let's update our state to queued, so
            % that we don't attempt to run it again.
            Record3 = Record2#watch{state = queued},
            {ok, Record3} = table_set_watch(Tab, Record3),

            % Now actually dispatch the task to the JS VMs
            lager:info("Run: ~p", [ID]),
            lifeguard_js_manager:run_watch(Watch);
        OtherState ->
            % We're in some non-scheduled state, so we don't run, but log it out.
            lager:info("Not running watch '~p', in non-scheduled state: ~p", [ID, OtherState])
    end,

    {noreply, State};
handle_info(_Request, State) -> {noreply, State}.

terminate(Reason, #state{store_pid=StorePid, watch_tab=Tab}) ->
    lager:info("Terminating watch manager: ~p", [Reason]),
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
        result = undefined,
        state = idle,
        timer_ref = undefined,
        timer_at  = undefined
    }.

%% @doc Reschedules a watch to run. This will cancel any previous timer
%% and sets a new timer.
reschedule_watch(TRef, Watch) ->
    % Cancel the previous timer
    case TRef of
        undefined -> ignore;
        TRef -> timer:cancel(TRef)
    end,

    % Schedule it again
    schedule_watch(Watch).

%% @doc Schedules a watch. The given watch must be a full lifeguard_watch
%% model. Once scheduled, this returns the timer reference.
%%
%% NOTE: This will send the `{run, ID}` message to whatever process
%% calls this. This is a private method and as such its only meant to be
%% called from THIS gen_server process.
schedule_watch(Watch) ->
    {ok, Name}     = lifeguard_watch:get_name(Watch),
    {ok, Interval} = lifeguard_watch:get_interval(Watch),
    {ok, TRef}     = timer:send_after(Interval, {run, Name}),

    % Determine when this is supposed to run next. We ignore microseconds
    % because when converting to a timestamp because we don't consider it
    % useful from a human perspective.
    {Mega, Sec, _Micro} = os:timestamp(),
    IntervalSecFloat    = Interval / 1000,
    IntervalSec         = trunc(IntervalSecFloat),
    TimerAt             = (Mega * 1000000) + Sec + IntervalSec,

    lager:info("Watch '~p' scheduled to run in ~p ms", [Name, Interval]),
    {ok, TRef, TimerAt}.

%% @doc Schedules a watch if it needs to be scheduled.
schedule_watch_if_needed(Tab, StorePid, Record) ->
    ID = Record#watch.id,
    Needed = case Record#watch.timer_ref of
        undefined ->
            % The watch has never been scheduled, schedule it
            lager:debug("Watch '~p' never scheduled. Scheduling.", [ID]),
            true;
        _Other -> false
    end,

    case Needed of
        true ->
            % We need to schedule
            {ok, Watch} = store_get_watch(StorePid, ID),
            {ok, TRef, TimerAt}  = schedule_watch(Watch),

            % Update the record state
            RecordNew = Record#watch{
                    state = scheduled,
                    timer_ref = TRef,
                    timer_at  = TimerAt
                },
            table_set_watch(Tab, RecordNew);
        false ->
            % It doesn't need scheduling, just return
            {ok, Record}
    end.

%% @doc Schedules an entire table of watches.
schedule_watch_table(Tab, StorePid) ->
    Key = ets:first(Tab),
    schedule_watch_table(Tab, StorePid, Key).
schedule_watch_table(_Tab, _StorePid, '$end_of_table') ->
    ok;
schedule_watch_table(Tab, StorePid, Key) ->
    % Find the record in the ETS table and query the store for
    % the actual Watch itself.
    {ok, Record}   = table_get_watch(Tab, Key),
    schedule_watch_if_needed(Tab, StorePid, Record),

    % Get the next key and loop
    NextKey = ets:next(Tab, Key),
    schedule_watch_table(Tab, StorePid, NextKey).

%% @doc Gets a watch from the backing store, given an ID.
store_get_watch(StorePid, ID) ->
    gen_server:call(StorePid, {get, ID}).

%% @doc Adds a watch to the table. The watch should be a lifeguard_watch
%% model.
table_add_watch_model(Tab, Watch) ->
    Record = model_to_record(Watch),
    table_set_watch(Tab, Record).

%% @doc Deletes a watch from the in-memory table.
table_delete_watch(Tab, ID) ->
    true = ets:delete(Tab, ID),
    ok.

%% @doc Retrieves a watch from the table. The raw internal record format
%% will be returned, not a lifeguard_watch model object.
table_get_watch(Tab, ID) ->
    case ets:lookup(Tab, ID) of
        [] ->
            undefined;
        [{ID, Record}] ->
            {ok, Record}
    end.

%% @doc Populates the in-memory watch table with a list of model
%% objects.
table_populate_watches(_Tab, []) ->
    ok;
table_populate_watches(Tab, [Watch | Rest]) ->
    {ok, Name} = lifeguard_watch:get_name(Watch),
    lager:debug("Populating: ~p", [Name]),
    table_add_watch_model(Tab, Watch),
    table_populate_watches(Tab, Rest).

%% @doc Sets the watch on the table. This will insert or update the watch.
table_set_watch(Tab, Watch) ->
    true = ets:insert(Tab, {Watch#watch.id, Watch}),
    {ok, Watch}.

%% @doc Unschedules a timer reference if it is valid.
unschedule(undefined) ->
    ok;
unschedule(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

%% @doc Takes our metadata record and a watch model and appends the transient
%% data to it, returning a new watch model.
watch_append_transient(Record, Watch) ->
    Transient = [{state, Record#watch.state}, {timer_at, Record#watch.timer_at}],
    lifeguard_watch:set_transient(Watch, Transient).

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

reschedule_watch_test() ->
    % Create a watch
    M1 = lifeguard_watch:new(),
    M2 = lifeguard_watch:set_name(M1, "foo"),
    M3 = lifeguard_watch:set_interval(M2, 100),
    Watch = M3,

    % Schedule it once
    {ok, TRef, _TimerAt} = schedule_watch(Watch),

    % Reschedule it after some small amount of time
    timer:sleep(10),
    {ok, _TRef2, _TimerAt} = reschedule_watch(TRef, Watch),

    % Verify we get the message
    true = receive
        {run, "foo"} -> true
    after 150 ->
        good_timeout
    end,

    % Verify we have no other messages
    true = receive
        Anything -> Anything
    after 50 ->
        true
    end.

schedule_watch_test() ->
    % Create a watch
    M1 = lifeguard_watch:new(),
    M2 = lifeguard_watch:set_name(M1, "foo"),
    M3 = lifeguard_watch:set_interval(M2, 50),
    Watch = M3,

    % Schedule it
    {ok, _TRef, _TimerAt} = schedule_watch(Watch),

    % Verify we get it
    true = receive
        {run, "foo"} -> true
    after 100 ->
        false
    end.

unschedule_test() ->
    % Create a watch
    M1 = lifeguard_watch:new(),
    M2 = lifeguard_watch:set_name(M1, "foo"),
    M3 = lifeguard_watch:set_interval(M2, 100),
    Watch = M3,

    % Schedule it
    {ok, TRef, _TimerAt} = schedule_watch(Watch),

    % Unschedule it!
    ok = unschedule(TRef),

    % Verify we never get a message
    true = receive
        Anything -> false
    after 150 ->
        true
    end.

% Test runner for testing all the methods that require state.
stateful_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_table_add_get_delete_watch/1,
            fun test_table_get_watch_nonexistent/1,
            fun test_table_populate_watches/1
        ]}.

setup() ->
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

    {ok, StorePid} = lifeguard_watch_store:start_link(?cmd("mktemp -t lifeguard")),
    #test_state{table=Tab, store=StorePid}.

teardown(#test_state{table=Tab, store=StorePid}) ->
    % Delete the ETS table
    ets:delete(Tab),

    % Stop the store
    gen_server:call(StorePid, stop).

test_table_add_get_delete_watch(#test_state{table=Tab}) ->
    fun() ->
            % Create a model object
            W1 = lifeguard_watch:new(),
            W2 = lifeguard_watch:set_name(W1, "foo"),
            W3 = lifeguard_watch:set_code(W2, "bar"),
            Watch = W3,

            % Add it to the table
            {ok, Record} = table_add_watch_model(Tab, Watch),

            % Verify it was added
            {ok, Record} = table_get_watch(Tab, "foo"),
            "foo" = Record#watch.id,

            % Delete it
            ok = table_delete_watch(Tab, "foo"),
            undefined = table_get_watch(Tab, "foo")
    end.

test_table_get_watch_nonexistent(#test_state{table=Tab}) ->
    fun() ->
            undefined = table_get_watch(Tab, "foo")
    end.

test_table_populate_watches(#test_state{table=Tab}) ->
    fun() ->
            % Populate it with a list of model objects
            A1 = lifeguard_watch:new(),
            A2 = lifeguard_watch:set_name(A1, "foo"),
            B1 = lifeguard_watch:new(),
            B2 = lifeguard_watch:set_name(B1, "bar"),
            ok = table_populate_watches(Tab, [A2, B2]),

            % Find the items in the table to verify they are there
            {ok, AResult} = table_get_watch(Tab, "foo"),
            {ok, BResult} = table_get_watch(Tab, "bar")
    end.

-endif.
