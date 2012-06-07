-module(lifeguard_util).
-export([to_binary/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Converts something to a binary through a best effort process.
to_binary(Binary) when is_binary(Binary) ->
    {ok, Binary};
to_binary(List) when is_list(List) ->
    {ok, list_to_binary(List)};
to_binary(_Other) ->
    {error, cant_convert}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

to_binary_test() ->
    {ok, <<"foo">>} = to_binary(<<"foo">>),
    {ok, <<"bar">>} = to_binary("bar"),
    {error, cant_convert} = to_binary(24).

-endif.
