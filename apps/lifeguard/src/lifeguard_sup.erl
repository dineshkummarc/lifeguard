-module(lifeguard_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Get the data sources from the application configuration
    {ok, DataSources}    = application:get_env(data_sources),
    {ok, JsPendingLimit} = application:get_env(js_pending_limit),
    {ok, JsVMCount}      = application:get_env(js_vm_count),
    {ok, PluginPath}     = application:get_env(plugin_path),
    {ok, Plugins}        = application:get_env(plugins),
    {ok, StoragePath}    = application:get_env(storage_path),
    {ok, HTTPIP}         = application:get_env(http_ip),
    {ok, HTTPPort}       = application:get_env(http_port),

    % Get the configuration for the web API
    DispatchPath   = filename:join(code:priv_dir(lifeguard), "dispatch.config"),
    {ok, Dispatch} = file:consult(DispatchPath),
    WebConfig = [
            {ip, HTTPIP},
            {port, HTTPPort},
            {backlog, 128},
            {dispatch, Dispatch}
            ],

    % Run the data store manager supervisor
    DSManager = {data_store_manager_sup,
        {lifeguard_ds_manager_sup, start_link, [DataSources]},
        permanent, 30000, supervisor, [lifeguard_ds_manager_sup]},

    % JS VM manager supervisor
    JsManager = {js_manager_sup,
        {lifeguard_js_manager_sup, start_link, [JsVMCount, JsPendingLimit]},
        permanent, 30000, supervisor, dynamic},

    % Plugin manager
    PluginManager = {plugin_manager,
        {lifeguard_plugin_manager, start_link, [PluginPath, Plugins]},
        permanent, 30000, worker, dynamic},

    % Watch manager
    WatchManager = {watch_manager,
        {lifeguard_watch_manager, start_link, [StoragePath]},
        permanent, 30000, worker, dynamic},

    % Web interface
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 30000, worker, dynamic},

    % Return the full spec
    Children = [PluginManager, DSManager, JsManager, WatchManager, Web],
    {ok, { {one_for_one, 5, 10}, Children} }.
