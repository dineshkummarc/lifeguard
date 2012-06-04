%% @doc This module implements the RESTful interface for Watches. This
%% handles creating, reading, updating, and deleting.

-module(lifeguard_wm_watches).
-export([init/1,
         allowed_methods/2,
         create_path/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         get_watch/2,
         get_watches/2,
         malformed_request/2,
         post_is_create/2,
         put_watch/2,
         resource_exists/2]).

-record(state, {
        watch_name, % The name of the watch being modified, if being modified
        watch_data, % A proplist of the watch data
        watch_old_data % The old data associated with a Watch
        }).

-include_lib("webmachine/include/webmachine.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(_) ->
    {ok, #state{watch_name=undefined}}.

allowed_methods(ReqData, Context) ->
    % If we're given a name in the path, then we support modifying and
    % all the things. Otherwise, we only support listing and creating.
    case wrq:path_info(name, ReqData) of
        undefined ->
            Methods = ['GET', 'POST', 'PUT'],
            {Methods, ReqData, Context};
        Name ->
            BinName = list_to_binary(Name),
            Methods = ['GET', 'POST', 'PUT', 'DELETE'],
            State   = Context#state{watch_name=BinName},
            {Methods, ReqData, State}
    end.

malformed_request(ReqData, Context) ->
    malformed_request_by_method(ReqData, Context, wrq:method(ReqData)).

malformed_request_by_method(ReqData, Context, 'DELETE') ->
    {false, ReqData, Context};
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

resource_exists(ReqData, #state{watch_name=Name} = Context) when Name =/= undefined ->
    % If we're working with a specific watch, then we need to verify
    % that it actually exists.
    case lifeguard_watch_manager:get_watch(Name) of
        {ok, Watch} ->
            State = Context#state{watch_old_data=Watch},
            {true, ReqData, State};
        {error, no_watch} ->
            {false, ReqData, Context}
    end;
resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {name, Name} = proplists:lookup(name, Context#state.watch_data),
    BinaryPath   = <<"/api/data-sources/", Name/binary>>,
    Path         = binary_to_list(BinaryPath),
    {Path, ReqData, Context}.

content_types_provided(ReqData, #state{watch_name=Name} = Context) ->
    Handler = case Name of
        undefined ->
            get_watches;
        _ ->
            get_watch
    end,
    Handlers = [{"application/json", Handler}],
    {Handlers, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    Handlers = [{"application/json", put_watch}],
    {Handlers, ReqData, Context}.

delete_resource(ReqData, #state{watch_name=Name} = Context) ->
    ok = lifeguard_watch_manager:delete_watch(Name),
    JSONStruct = {struct, [{success, <<"true">>}]},
    JSON     = list_to_binary(mochijson2:encode(JSONStruct)),
    Response = wrq:set_resp_body(JSON, ReqData),
    {true, Response, Context}.

%% @doc Gets a single watch and returns a JSON object associated with it.
get_watch(ReqData, #state{watch_old_data=Watch} = Context) ->
    JSONStruct = struct_from_watch(Watch),
    JSON       = mochijson2:encode(JSONStruct),
    {list_to_binary(JSON), ReqData, Context}.

%% @doc Handler that renders a JSON object of every watch.
get_watches(ReqData, Context) ->
    {ok, Watches} = lifeguard_watch_manager:list_watches(),
    JSONStruct    = struct_from_list(Watches),
    JSON          = mochijson2:encode(JSONStruct),
    {list_to_binary(JSON), ReqData, Context}.

%% @doc Handler that updates an existing watch.
put_watch(ReqData, Context) ->
    Data = Context#state.watch_data,
    {name, Name} = proplists:lookup(name, Data),
    {code, Code} = proplists:lookup(code, Data),
    {interval, Interval} = proplists:lookup(interval, Data),
    W1 = lifeguard_watch:new(),
    W2 = lifeguard_watch:set_name(W1, Name),
    W3 = lifeguard_watch:set_code(W2, Code),
    W4 = lifeguard_watch:set_interval(W3, Interval),
    {ok, _Watch} = lifeguard_watch_manager:set_watch(W4),
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
    Struct = struct_members_for_watch(Watch),
    struct_from_list1(Rest, [Struct | Acc]).

struct_from_watch(Watch) ->
    {struct, struct_members_for_watch(Watch)}.

struct_members_for_watch(Watch) ->
    {ok, Name} = lifeguard_watch:get_name(Watch),
    {ok, Code} = lifeguard_watch:get_code(Watch),
    {ok, Interval} = lifeguard_watch:get_interval(Watch),
    {ok, Transient} = lifeguard_watch:get_transient(Watch),

    % Get the transient data
    {state, State}  = proplists:lookup(state, Transient),
    Result  = case proplists:lookup(result, Transient) of
        {result, undefined} -> null;
        {result, ResultValue} -> ResultValue
    end,
    TimerAt = case proplists:lookup(timer_at, Transient) of
        {timer_at, undefined} -> null;
        {timer_at, Timestamp} -> Timestamp
    end,

    [{name, Name},
     {code, Code},
     {interval, Interval},
     {state, State},
     {result, Result},
     {timer_at, TimerAt}].

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
    Key = <<"name">>,
    case lists:keyfind(Key, 1, Struct) of
        false ->
            {P, [<<"'name' is required">> | E]};
        {_Key, Value} when size(Value) =:= 0 ->
            {P, [<<"'name' must be non-empty">> | E]};
        {Key, Value} ->
            {[{name, Value} | P], E}
    end.

validate_code(Struct, P, E) ->
    Key = <<"code">>,
    case lists:keyfind(Key, 1, Struct) of
        false ->
            {P, [<<"'code' is required">> | E]};
        {Key, Value} ->
            {[{code, Value} | P], E}
    end.

validate_interval(Struct, P, E) ->
    Key = <<"interval">>,
    case lists:keyfind(Key, 1, Struct) of
        false ->
            {P, [<<"'interval' is required">> | E]};
        {Key, Value} when is_integer(Value) ->
            {[{interval, Value} | P], E};
        _ ->
            {P, [<<"'interval' must be an integer">> | E]}
    end.

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

validate_name_empty_bad_test() ->
    Struct = [{<<"name">>, <<"">>}],
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

-endif.
