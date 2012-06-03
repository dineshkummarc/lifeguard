%% @doc This is a model for a watch in lifeguard. This lets the internal
%% structure and various state associated with a watch be encapsulated.
-module(lifeguard_watch).
-export([new/0,
         get_code/1,
         get_interval/1,
         get_name/1,
         set_code/2,
         set_interval/2,
         set_name/2]).

%% @doc THe record that is used as the internal structure of the watch.
-record(watch, {
        name,
        code,
        interval
    }).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Returns a new, empty watch.
new() ->
    #watch{}.

%% @doc Reads the code for a watch and returns it.
get_code(#watch{code=Code}) ->
    {ok, Code}.

%% @doc Reads the interval for a watch and returns it.
get_interval(#watch{interval=Interval}) ->
    {ok, Interval}.

%% @doc Reads the name of a watch and returns it.
get_name(#watch{name=Name}) ->
    {ok, Name}.

%% @doc Sets the code for a watch and returns a new watch with that
%% code set.
set_code(Watch, Code) when is_record(Watch, watch) ->
    Watch#watch{code=Code}.

%% @doc Sets the interval for a watch.
set_interval(Watch, Interval) when is_record(Watch, watch) ->
    Watch#watch{interval=Interval}.

%% @doc Sets the name of a watch and returns a new watch with that name
%% set.
set_name(Watch, Name) when is_record(Watch, watch) ->
    Watch#watch{name=Name}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

new_test() ->
    Watch = new().

get_code_undefined_test() ->
    Watch = new(),
    {ok, undefined} = get_code(Watch).

get_interval_undefined_test() ->
    Watch = new(),
    {ok, undefined} = get_interval(Watch).

get_name_undefined_test() ->
    Watch = new(),
    {ok, undefined} = get_name(Watch).

get_set_code_test() ->
    Watch = new(),
    Watch2 = set_code(Watch, "foo"),
    {ok, "foo"} = get_code(Watch2).

get_set_interval_test() ->
    Watch = new(),
    Watch2 = set_interval(Watch, "foo"),
    {ok, "foo"} = get_interval(Watch2).

get_set_name_test() ->
    Watch        = new(),
    Watch2       = set_name(Watch, "foo"),
    {ok, "foo"} = get_name(Watch2).

-endif.
