-module(lifeguard_wm_ds_list).
-export([init/1,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, no_state}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    {ok, Lists} = lifeguard_ds_manager:list(),
    Binaries    = lists:map(fun(X) -> list_to_binary(X) end, Lists),
    {mochijson2:encode(Binaries), ReqData, Context}.
