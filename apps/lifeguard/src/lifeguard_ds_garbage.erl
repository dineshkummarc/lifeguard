%% @doc This is a "garbage" data source for Lifeguard that just returns
%% pseudo-random numbers. This can be useful for testing new checks.

-module(lifeguard_ds_garbage).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the data source in a supervision tree.
start_link(_Config) ->
    gen_server:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) -> {ok, any}.

handle_call({get, Amount}, _From, State) ->
    {reply, get_numbers(Amount), State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_numbers(Amount) ->
    [random:uniform(50) || _ <- lists:seq(1, Amount)].

-ifdef(TEST).

handle_get_test() ->
    Result = get_numbers(5),
    ?assert(length(Result) =:= 5).

-endif.
