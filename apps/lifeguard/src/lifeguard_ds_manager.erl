-module(lifeguard_ds_manager).
-behavior(gen_server).
-export([start_link/1,
         data_source_id/1,
         get/2,
         list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        data_sources % List of available data source names
        }).

%% @doc Start the data source manager.
start_link(DataSources) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DataSources], []).

%% @doc Get data from a data source.
-spec get(string(), [term()]) -> {ok, term()} | {error, term()}.
get(SourceName, Args) ->
    gen_server:call(?MODULE, {get, SourceName, Args}).

%% @doc List all the available data sources.
-spec list() -> {ok, [string()]}.
list() ->
    gen_server:call(?MODULE, list).

%% @doc Returns the atom ID of a data source.
data_source_id(SourceName) ->
    binary_to_atom(<<"ds_", SourceName/binary>>, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([DataSources]) ->
    {ok, #state{data_sources=DataSources}}.

handle_call(list, _From, State) ->
    {reply, {ok, State#state.data_sources}, State};

handle_call({get, SourceName, Args}, _From, State) ->
    % Get the atom that would represent the locally registered name
    % of this data source.
    SourceRef = data_source_id(SourceName),

    % Attempt to call onto this data source. Note that if an invalid
    % data source was given, this will raise a `noproc` exception, so
    % we catch that and return a nice error.
    Response = try gen_server:call(SourceRef, {get, Args}) of
        {ok, _Result} = FullResult ->
            % A properly formatted success result, return as-is
            FullResult;
        {error, _Message} = FullError ->
            % A properly formattted error result, return as-is
            FullError;
        Other ->
            % An invalidly formatted response, so log it out.
            lager:error("Invalid response format for data source: ~p ~p", [SourceName, Other]),
            {error, invalid_response}
    catch
        exit: {noproc, _} ->
            lager:error("Invalid data source called: ~p", [SourceName]),
            {error, no_data_source};
        exit: _ ->
            lager:error("Internal error in data source: ~p", [SourceName]),
            {error, data_source_error}
    end,

    {reply, Response, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
