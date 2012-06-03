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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

model_to_record_test() ->
    M1 = lifeguard_watch:new(),
    M2 = lifeguard_watch:set_name(M1, "foo"),
    M3 = lifeguard_watch:set_code(M2, "bar"),
    M4 = lifeguard_watch:set_interval(M3, 5),

    Rec = model_to_record(M4),
    "foo" = Rec#watch.id,
    idle  = Rec#watch.state,
    undefined = Rec#watch.timer_ref.

populate_watch_table_test() ->
    % Create an ETS table
    Tab = ets:new(my_table, [set, private]),

    % Populate it with a list of model objects
    A1 = lifeguard_watch:new(),
    A2 = lifeguard_watch:set_name(A1, "foo"),
    B1 = lifeguard_watch:new(),
    B2 = lifeguard_watch:set_name(B1, "bar"),
    ok = populate_watch_table(Tab, [A2, B2]),

    % Find the items in the table to verify they are there
    [AResult] = ets:lookup(Tab, "foo"),
    [BResult] = ets:lookup(Tab, "bar"),

    % Destroy the ETS table
    ets:delete(Tab).

-endif.
