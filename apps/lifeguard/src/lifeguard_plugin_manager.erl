-module(lifeguard_plugin_manager).
-behavior(gen_server).
-export([start_link/1,
         enable_plugin/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        plugin_path % Path to where the plugins are
    }).

%% @doc Start the plugin manager.
start_link(PluginPath) ->
    % We require that the plugin path be a binary, so let's do our best
    % to convert it, since user's enter it.
    BinPluginPath = case lifeguard_util:to_binary(PluginPath) of
        {ok, BinValue} -> BinValue;
        {error, cant_convert} -> throw({not_binary, PluginPath})
    end,

    gen_server:start_link({local, ?MODULE}, ?MODULE, [BinPluginPath], []).

%% @doc Load a plugin, which is just an OTP application, with
%% the given name.
enable_plugin(Plugin) ->
    gen_server:call(?MODULE, {enable_plugin, Plugin}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([PluginPath]) ->
    lager:info("Plugin manager started."),
    {ok, #state{plugin_path=PluginPath}}.

handle_call({enable_plugin, Plugin}, _From, State) ->
    PluginPath = State#state.plugin_path,
    PluginAtom = atom_to_binary(Plugin, utf8),
    RootPath   = <<PluginPath/binary, "/", PluginAtom/binary>>,

    Result = case filelib:is_dir(RootPath) of
        false -> {error, plugin_not_found};
        true ->
            % It exists, now we traverse all the lib paths and add it
            % to the code paths. First, find all the libraries.
            LibPath = binary_to_list(<<RootPath/binary, "/lib/*/ebin">>),
            LibDirs = lists:filter(fun(X) -> filelib:is_dir(X) end, filelib:wildcard(LibPath)),

            % Add every directory to the code path
            lists:foreach(fun(Path) ->
                        lager:debug("Add to code path: ~p", [Path]),
                        true = code:add_path(Path)
                end, LibDirs),

            % Finally, start the application
            application:start(Plugin)
    end,

    {reply, Result, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
