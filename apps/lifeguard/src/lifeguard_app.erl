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

    % Start the main supervisor, which kicks off the entire application process
    lifeguard_sup:start_link().

stop(_State) ->
    ok.
