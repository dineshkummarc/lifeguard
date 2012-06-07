-module(lifeguard_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % If we have a LIFEGUARD_LOG environmental variable, then set the console
    % log level to whatever that is set to.
    case os:getenv("LIFEGUARD_LOG") of
        false -> ok;
        Value ->
            lager:set_loglevel(lager_console_backend, list_to_atom(Value))
    end,

    % Get the plugins we need to enable after starting the entire
    % supervision tree.
    {ok, Plugins} = application:get_env(plugins),

    % Start the main supervisor, which kicks off the entire application process
    case lifeguard_sup:start_link() of
        {ok, _Pid} = GoodResult ->
            % The supervisor started up properly, so lets start up all
            % the plugins.
            lager:info("Enabling boot-up plugins: ~p", [Plugins]),
            lists:foreach(fun(Name) ->
                        % XXX: Error handling
                        lager:debug("Enabling boot-up plugin: ~p", [Name]),
                        lifeguard_plugin_manager:enable_plugin(Name)
                end, Plugins),

            % And return the good result
            GoodResult;
        Other ->
            Other
    end.

stop(_State) ->
    ok.
