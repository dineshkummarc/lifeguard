%% @doc This module serves the static resources for the web UI. This
%% works by simply finding the files in the "static" folder in the priv
%% directory, and serves it.

-module(lifeguard_wm_static).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         generate_etag/2,
         last_modified/2,
         resource_exists/2,
         to_response/2]).

-record(state, {
        directory, % Directory where to load static resources
        full_path  % Full path to the static resource
        }).

-include_lib("webmachine/include/webmachine.hrl").

init([Directory]) ->
    {ok, #state{directory=Directory}}.

allowed_methods(ReqData, Context) ->
    % Since this is the earliest called callback, setup the state
    Directory = Context#state.directory,
    ShortPath = wrq:disp_path(ReqData),
    State = Context#state{full_path=full_resource_path(Directory, ShortPath)},
    {['GET'], ReqData, State}.

content_types_provided(ReqData, Context) ->
    ContentType = webmachine_util:guess_mime(Context#state.full_path),
    {[{ContentType, to_response}], ReqData, Context}.

generate_etag(ReqData, Context) ->
    {ok, Value} = file:read_file(Context#state.full_path),
    ETag        = hash_body(Value),

    % Respond with the ETag
    {ETag, ReqData, Context}.

last_modified(ReqData, Context) ->
    % Get the last modified time for the static file
    LastMod  = filelib:last_modified(Context#state.full_path),
    {LastMod, ReqData, Context}.

resource_exists(ReqData, Context) ->
    % Expand the file path and check that it exists
    Exists   = filelib:is_regular(Context#state.full_path),
    {Exists, ReqData, Context}.

to_response(ReqData, Context) ->
    {ok, Value} = file:read_file(Context#state.full_path),

    % Push it out!
    {Value, ReqData, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec full_resource_path(string(), string()) -> string().
full_resource_path(Directory, ShortPath) ->
    filename:join([code:priv_dir(lifeguard), Directory, ShortPath]).

-spec hash_body(string()) -> string().
hash_body(Body) ->
    mochihex:to_hex(binary_to_list(crypto:sha(Body))).
