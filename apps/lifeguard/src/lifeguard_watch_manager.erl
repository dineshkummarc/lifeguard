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

populate_watch_table(_Tab, []) ->
    ok;
populate_watch_table(Tab, [_Watch | Rest]) ->
    populate_watch_table(Tab, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-endif.
