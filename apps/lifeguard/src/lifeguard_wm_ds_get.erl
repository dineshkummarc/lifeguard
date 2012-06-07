-module(lifeguard_wm_ds_get).
-export([init/1,
         content_types_provided/2,
         malformed_request/2,
         resource_exists/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
        ds_name, % Name of the data source (string())
        ds_args, % Arguments to send to the data source ([term()])
        result   % This is the result of the ds_manager:get
        }).

init(_) ->
    {ok, #state{}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

malformed_request(ReqData, Context) ->
    DSName = list_to_binary(wrq:path_info(name, ReqData)),
    DSArgs = case wrq:get_qs_value("args", ReqData) of
        undefined -> [];
        RawArgs   ->
            mochijson2:decode(RawArgs)
    end,

    case DSArgs of
        List when is_list(List) ->
            NewState = Context#state{
                    ds_name=DSName,
                    ds_args=DSArgs
                    },
            {false, ReqData, NewState};
        _ ->
            Response = wrq:set_resp_body(<<"Args must be an array.">>, ReqData),
            {true, Response, Context}
    end.

resource_exists(ReqData, Context) ->
    case lifeguard_ds_manager:get(Context#state.ds_name, Context#state.ds_args) of
        {error, data_source_error} ->
            % This is an internal error in the data source, so we give a
            % 500 error that something went horribly wrong.
            {{error, data_source_error}, ReqData, Context};
        {error, no_data_source} ->
            BinName  = list_to_binary(Context#state.ds_name),
            Response = wrq:set_resp_body(<<"Data source not found: ", BinName/binary>>, ReqData),
            {false, Response, Context};
        {error, Other} ->
            {{error, Other}, ReqData, Context};
        Result ->
            {true, ReqData, Context#state{result=Result}}
    end.

to_json(ReqData, Context) ->
    % At this point we're sure that the state can only contain valid
    % responses, so just pattern match it out.
    {ok, Data} = Context#state.result,
    {mochijson2:encode(Data), ReqData, Context}.
