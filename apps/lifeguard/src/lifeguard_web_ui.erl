-module(lifeguard_web_ui).
-export([init/1,
         resource_exists/2,
         to_html/2]).

-record(state, {
        content % The content to send to the browser
        }).

-include_lib("webmachine/include/webmachine.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Webmachine Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    {ok, #state{}}.

resource_exists(ReqData, Context) ->
    % Replace the "/" with "_" in paths, so requests to pages such as
    % "filters/new" turns into "filters_new." Finally, "_dtl" is
    % appended onto the end of the module name since that is how it is
    % compiled.
    Path       = case wrq:path_tokens(ReqData) of
        List when length(List) =:= 0 -> "index";
        [Main | _Rest] -> Main
    end,
    Module = path_to_tpl_module(Path),

    % Attempt to call the module. If this fails, then the resource
    % doesn't actually exist.
    try apply(Module, render, [[]]) of
        {ok, Content} -> {true, ReqData, Context#state{content=Content}}
    catch
        error: undef -> {false, ReqData, Context}
    end.

to_html(ReqData, Context) ->
    % We just shuttle out the content since we already rendered
    {Context#state.content, ReqData, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec path_to_tpl_module(string()) -> atom().
path_to_tpl_module(Path) ->
    BinPath    = list_to_binary(Path),
    ModuleName = binary:replace(BinPath, [<<"/">>, <<"-">>], <<"_">>),
    binary_to_atom(<<ModuleName/binary, "_dtl">>, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

path_to_module_test() ->
    ?assertEqual(foo_dtl, path_to_tpl_module("foo")),
    ?assertEqual(foo_bar_dtl, path_to_tpl_module("foo/bar")),
    ?assertEqual(foo_bar_dtl, path_to_tpl_module("foo-bar")).

-endif.
