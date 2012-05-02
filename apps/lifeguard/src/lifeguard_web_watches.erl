-module(lifeguard_web_watches).
-export([init/1,
         allowed_methods/2,
         create_path/2,
         content_types_accepted/2,
         content_types_provided/2,
         get_watches/2,
         malformed_request/2,
         post_is_create/2,
         put_watch/2]).

-record(state, {
        watch_data % A proplist of the watch data
        }).

-include_lib("webmachine/include/webmachine.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_) ->
    {ok, #state{}}.

allowed_methods(ReqData, Context) ->
    Methods = ['GET', 'POST'],
    {Methods, ReqData, Context}.

malformed_request(ReqData, Context) ->
    malformed_request_by_method(ReqData, Context, wrq:method(ReqData)).

malformed_request_by_method(ReqData, Context, 'GET') ->
    {false, ReqData, Context};
malformed_request_by_method(ReqData, Context, _) ->
    ReqBody = wrq:req_body(ReqData),
    try mochijson2:decode(ReqBody) of
        {struct, Struct} ->
            case validate_struct(Struct) of
                {ok, WatchData} ->
                    {false, ReqData, Context#state{watch_data=WatchData}};
                {error, Errors} ->
                    JSONStruct = {struct, [{errors, Errors}]},
                    JSONString = list_to_binary(mochijson2:encode(JSONStruct)),
                    Response  = wrq:set_resp_body(JSONString, ReqData),
                    Response1 = wrq:set_resp_header("Content-Type", "application/json", Response),
                    {true, Response1, Context}
            end;
        _ ->
            Response = wrq:set_resp_body(<<"Content must be a JSON object.">>, ReqData),
            {true, Response, Context}
    catch
        error: {case_clause, ReqBody} ->
            % This happens when you pass invalid JSON.
            Response = wrq:set_resp_body(<<"Content must be validly formatted JSON.">>, ReqData),
            {true, Response, Context}
    end.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {name, Name} = proplists:lookup(name, Context#state.watch_data),
    BinaryPath   = <<"/api/data-sources/", Name/binary>>,
    Path         = binary_to_list(BinaryPath),
    {Path, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    Handlers = [{"application/json", get_watches}],
    {Handlers, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    Handlers = [{"application/json", put_watch}],
    {Handlers, ReqData, Context}.

get_watches(ReqData, Context) ->
    {ok, Watches} = lifeguard_watch_manager:list_watches(),
    JSONStruct    = struct_from_list(Watches),
    JSON          = mochijson2:encode(JSONStruct),
    {list_to_binary(JSON), ReqData, Context}.

put_watch(ReqData, Context) ->
    Data = Context#state.watch_data,
    {name, Name} = proplists:lookup(name, Data),
    {code, Code} = proplists:lookup(code, Data),
    {interval, Interval} = proplists:lookup(interval, Data),
    ok = lifeguard_watch_manager:set_watch(Name, Code, Interval),
    {true, ReqData, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

struct_from_list(Watches) ->
    StructList = struct_from_list1(Watches, []),
    {struct, [{watches, StructList}]}.

struct_from_list1([], Acc) ->
    Acc;
struct_from_list1([Watch | Rest], Acc) ->
    {Name, Code, Interval} = Watch,
    Struct = [{name, Name},
              {code, Code},
              {interval, Interval}],
    struct_from_list1(Rest, [Struct | Acc]).

validate_struct(Struct) ->
    % The "P" variables are the proplist, the "E" variables are
    % the errors that we accumulate.
    P1 = [],
    E1 = [],
    {P2, E2} = validate_name(Struct, P1, E1),
    {P3, E3} = validate_code(Struct, P2, E2),
    {P4, E4} = validate_interval(Struct, P3, E3),
    if
        length(E4) > 0 ->
            {error, E4};
        true ->
            {ok, P4}
    end.

validate_name(Struct, P, E) ->
    case json_get_key(<<"name">>, Struct) of
        undefined ->
            {P, [<<"'name' is required">> | E]};
        Value ->
            {[{name, Value} | P], E}
    end.

validate_code(Struct, P, E) ->
    case json_get_key(<<"code">>, Struct) of
        undefined ->
            {P, [<<"'code' is required">> | E]};
        Value ->
            {[{code, Value} | P], E}
    end.

validate_interval(Struct, P, E) ->
    case json_get_key(<<"interval">>, Struct) of
        undefined ->
            {P, [<<"'interval' is required">> | E]};
        Value when is_integer(Value) ->
            {[{interval, Value} | P], E};
        _ ->
            {P, [<<"'interval' must be an integer">> | E]}
    end.

json_get_key(_Key, []) ->
    undefined;
json_get_key(Needle, [{Key, Value} | _]) when Needle =:= Key ->
    Value;
json_get_key(Key, [{_, _} | Rest]) ->
    json_get_key(Key, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

validate_struct_ok_test() ->
    Struct = [{<<"name">>, <<"foo">>},
              {<<"code">>, <<"bar">>},
              {<<"interval">>, 5000}],
    {ok, Data} = validate_struct(Struct),
    {name, <<"foo">>} = proplists:lookup(name, Data),
    {code, <<"bar">>} = proplists:lookup(code, Data),
    {interval, 5000}  = proplists:lookup(interval, Data).

validate_name_ok_test() ->
    Struct = [{<<"name">>, <<"bar">>}],
    {[{name, <<"bar">>}], []} = validate_name(Struct, [], []).

validate_name_bad_test() ->
    Struct = [{<<"nope">>, <<"bar">>}],
    {[], Errors} = validate_name(Struct, [], []),
    ?assert(length(Errors) > 0).

validate_code_ok_test() ->
    Struct = [{<<"code">>, <<"bar">>}],
    {[{code, <<"bar">>}], []} = validate_code(Struct, [], []).

validate_code_bad_test() ->
    Struct = [{<<"nope">>, <<"bar">>}],
    {[], Errors} = validate_code(Struct, [], []),
    ?assert(length(Errors) > 0).

validate_interval_ok_test() ->
    Struct = [{<<"interval">>, 27}],
    {[{interval, 27}], []} = validate_interval(Struct, [], []).

validate_interval_bad_not_integer_test() ->
    Struct = [{<<"interval">>, <<"bar">>}],
    {[], Errors} = validate_interval(Struct, [], []),
    ?assert(length(Errors) > 0).

validate_interval_bad_test() ->
    Struct = [{<<"nope">>, <<"bar">>}],
    {[], Errors} = validate_interval(Struct, [], []),
    ?assert(length(Errors) > 0).

json_get_key_test() ->
    Struct = [{<<"foo">>, <<"bar">>}, {<<"bar">>, <<"baz">>}],
    ?assertEqual(undefined, json_get_key(<<"baz">>, Struct)),
    ?assertEqual(<<"bar">>, json_get_key(<<"foo">>, Struct)),
    ?assertEqual(<<"baz">>, json_get_key(<<"bar">>, Struct)).

-endif.
