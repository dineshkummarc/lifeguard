%% @doc The watch manager is responsible for all changes to watches.
%% It orchestrates the changes between the various watch subsystems,
%% such as storage, the scheduler, etc.
-module(lifeguard_watch_manager).
-behavior(gen_server).
-export([start_link/1,
         delete_watch/1,
         get_watch/1,
         list_watches/0,
         set_watch/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
-spec set_watch(string(), string(), pos_integer()) -> ok | {error, term()}.
set_watch(Name, Code, Interval) ->
    gen_server:call(?MODULE, {set, Name, Code, Interval}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(StoragePath) ->
    lager:info("Watch manager starting..."),

    % Start the storage process
    lager:debug("Starting watch store..."),
    {ok, Pid} = lifeguard_watch_store:start_link(StoragePath),

    % Log it out and start
    lager:info("Watch manager started."),
    {ok, Pid}.

handle_call({delete, Name}, _From, StorePid) ->
    lager:info("Delete watch: ~p~n", [Name]),
    Result = gen_server:call(StorePid, {delete, Name}),
    {reply, Result, StorePid};
handle_call({get, Name}, _From, StorePid) ->
    lager:info("Getting watch: ~p~n", [Name]),
    Result = gen_server:call(StorePid, {get, Name}),
    {reply, Result, StorePid};
handle_call(list, _From, StorePid) ->
    lager:info("Listing watches~n"),
    Result = gen_server:call(StorePid, list),
    {reply, Result, StorePid};
handle_call({set, Name, Code, Interval}, _From, StorePid) ->
    lager:info("Setting watch: ~p~n", [Name]),
    Result = gen_server:call(StorePid, {set, Name, Code, Interval}),
    {reply, Result, StorePid}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, StorePid) ->
    lager:debug("Terminating the watch store..."),
    gen_server:call(StorePid, stop),

    lager:info("Watch manager terminated.").

code_change(_OldVsn, State, _Extra) -> {ok, State}.
