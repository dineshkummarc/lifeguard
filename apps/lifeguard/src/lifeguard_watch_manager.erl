%% @doc The watch manager is responsible for all changes to watches.
%% It orchestrates the changes between the various watch subsystems,
%% such as storage, the scheduler, etc.
-module(lifeguard_watch_manager).
-export([delete_watch/1,
         get_watch/1,
         list_watches/0,
         set_watch/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Delete a watch. This will remove it from the underlying
%% backing storage.
-spec delete_watch(string()) -> ok.
delete_watch(Name) ->
    lifeguard_watch_store:delete_watch(Name).

%% @doc Get a watch by name.
-spec get_watch(string()) -> ok.
get_watch(Name) ->
    lifeguard_watch_store:get_watch(Name).

%% @doc List all the watches in the system.
-spec list_watches() -> {ok, [term()]}.
list_watches() ->
    lifeguard_watch_store:list_watches().

%% @doc Add or update an existing watch.
-spec set_watch(string(), string(), pos_integer()) -> ok | {error, term()}.
set_watch(Name, Code, Interval) ->
    lifeguard_watch_store:set_watch(Name, Code, Interval).
