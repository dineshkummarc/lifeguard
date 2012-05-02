-module(lifeguard_watch_manager).
-behavior(gen_server).
-export([start_link/1,
         delete_watch/1,
         get_watch/1,
         list_watches/0,
         set_watch/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(watch, {
        name,    % Name for the watch
        code,    % Code for the watch (JavaScript)
        interval % Interval that it runs on in milliseconds
    }).

-define(TABLE_NAME, table).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the watch manager.
start_link(StoragePath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StoragePath, []).

%% @doc Delete a watch out of the system.
-spec delete_watch(string()) -> ok.
delete_watch(Name) ->
    gen_server:call(?MODULE, {delete, Name}).

%% @doc Lists all the watches in the system.
-spec list_watches() -> {ok, [term()]}.
list_watches() ->
    gen_server:call(?MODULE, list).

%% @doc Add a watch to the system. This will persist the watch into the
%% backing storage. If the watch already exists then this will update it,
%% otherwise a new watch will be created.
-spec set_watch(string(), string(), pos_integer()) -> ok | {error, term()}.
set_watch(Name, Code, Interval) ->
    Watch = #watch{
            name = Name,
            code = Code,
            interval = Interval
            },
    gen_server:call(?MODULE, {set, Watch}).

%% @doc Get a watch out of the system.
-spec get_watch(string()) -> {ok, {string(), string(), pos_integer()}} | {error, term()}.
get_watch(Name) ->
    case gen_server:call(?MODULE, {get, Name}) of
        {ok, Watch} when is_record(Watch, watch) ->
            Name = Watch#watch.name,
            Code = Watch#watch.code,
            Interval = Watch#watch.interval,
            {ok, {Name, Code, Interval}};
        {error, Reason} -> {error, Reason}
    end.

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
    lager:info("Watch manager started. Storage path: ~p", [StoragePath]),
    {ok, no_state}.

handle_call({delete, Name}, _From, State) ->
    lager:info("Delete watch: ~p~n", [Name]),
    {reply, internal_delete_watch(Name), State};
handle_call({get, Name}, _From, State) ->
    lager:info("Getting watch: ~p~n", [Name]),
    {reply, internal_get_watch(Name), State};
handle_call(list, _From, State) ->
    lager:info("Listing watches~n"),
    {reply, {ok, internal_list_watches()}, State};
handle_call({set, Watch}, _From, State) when is_record(Watch, watch) ->
    lager:info("Setting watch: ~p~n", [Watch#watch.name]),
    {reply, internal_set_watch(Watch), State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Watch manager terminated."),
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
    dets:foldl(fun(X, Acc) -> [X | Acc] end, [], ?TABLE_NAME).

internal_set_watch(#watch{name=Name} = Watch) ->
    dets:insert(?TABLE_NAME, {Name, Watch});
internal_set_watch(_) ->
    {error, invalid_watch}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

main_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_delete_watch/1,
            fun test_delete_watch_nonexistent/1,
            fun test_get_watch_nonexistent/1,
            fun test_list_watch_empty/1,
            fun test_list_watch/1,
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

test_list_watch(_) ->
    fun() ->
            Watch = #watch{code="baz"},
            ok = internal_set_watch(Watch#watch{name="foo"}),
            ok = internal_set_watch(Watch#watch{name="bar"}),
            Result = internal_list_watches(),
            ?assert(length(Result) =:= 2)
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
