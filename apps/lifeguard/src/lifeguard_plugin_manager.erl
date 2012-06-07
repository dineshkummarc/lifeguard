-module(lifeguard_plugin_manager).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        plugin_path % Path to where the plugins are
    }).

%% @doc Start the plugin manager.
start_link(PluginPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginPath], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([PluginPath]) ->
    lager:info("Plugin manager started."),
    {ok, #state{plugin_path=PluginPath}}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
