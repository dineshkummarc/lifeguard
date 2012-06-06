%% @doc This is an "echo" data source used for tests that simply returns the
%% arguments given to it.

-module(lifeguard_ds_echo).
-behavior(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Start the data source in a supervision tree.
start_link(ServerRef, _Name, _Args) ->
    gen_server:start_link({local, ServerRef}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    lager:info("Started the echo data source..."),
    {ok, any}.

handle_call({get, [crash]}, _From, _State) ->
    % Purposely crash if we get the "crash" arg
    ok = not_ok;
handle_call({get, [Data]}, _From, State) ->
    % Echo back the data
    {reply, {ok, Data}, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
