%% @doc This is the data source manager supervisor. It is responsible
%% for running the actual data source manager as well as each of the
%% actual data sources.

-module(lifeguard_ds_manager_sup).
-behavior(supervisor).
-export([start_link/1, init/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link(DataSources) ->
    % There is only ever one data source manager, so we register it
    % locally with our module name so that we can access it from anywhere.
    supervisor:start_link({local, ?MODULE}, ?MODULE, DataSources).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% supervisor callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(DataSources) ->
    % Get all the source names
    SourceNames = [Name || {Name, _, _} <- DataSources],

    % Add all the individual sources
    SourceSpecs = [data_source_spec(Source) || Source <- DataSources],

    % Define the manager...
    Manager = {data_source_manager,
        {lifeguard_ds_manager, start_link, [SourceNames]},
        permanent, 5000, worker, dynamic},

    {ok, {{one_for_one, 10, 10}, [Manager | SourceSpecs]} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_source_spec({Name, Module, Args}) ->
    % We require that the Name be a binary, but let's do our best to
    % get it there so that users can put in whatever they want..
    BinaryName = case Name of
        Binary when is_binary(Binary) -> Binary;
        List when is_list(List) -> list_to_binary(List);
        Other -> throw({not_binary, Other})
    end,

    ID = lifeguard_ds_manager:data_source_id(BinaryName),

    {{data_source, BinaryName},
        {Module, start_link, [ID, BinaryName, Args]},
        permanent, 30000, worker, [Module]}.

-ifdef(TEST).

data_source_spec_test() ->
    Name = <<"garbage">>,
    ID   = lifeguard_ds_manager:data_source_id(Name),

    Expected = {{data_source, Name},
        {my_module, start_link, [ID, Name, [arg]]},
        permanent, 30000, worker, [my_module]},
    Expected = data_source_spec({Name, my_module, [arg]}).

-endif.
