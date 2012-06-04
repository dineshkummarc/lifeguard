%% @doc The lifeguard JS manager is what orchestrates usage of all the underlying
%% JavaScript VMs. Job requests are sent to the JS manager and they are run
%% when an idle VM is available.
-module(lifeguard_js_manager).
-behavior(gen_server).
-export([start_link/1,
         idle_vm/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        idle_workers, % The idle VMs waiting for work
        pending_jobs, % The jobs pending, waiting for a VM
        pending_limit % The limit on the number of pending jobs we can have
    }).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the JS manager.
start_link(PendingLimit) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PendingLimit], []).

%% @doc Mark a specific JS VM as idle.
idle_vm(VMID) ->
    gen_server:cast(?MODULE, {idle_vm, VMID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(PendingLimit) ->
    lager:info("JS manager started."),
    {ok, #state{
            idle_workers = [],
            pending_jobs = [],
            pending_limit = PendingLimit
        }}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

% Signals that a JS VM has become idle. This will add that VM to the
% list of idle workers.
handle_cast({idle_vm, VMID}, State) ->
    IdleWorkers = State#state.idle_workers,
    case lists:member(VMID, IdleWorkers) of
        true ->
            % We already have this VM in the list of idle workers, so
            % we're good.
            {noreply, State};
        _ ->
            % Add the idle VM to the list
            lager:debug("Idle VM: ~p", [VMID]),
            NewIdle = [VMID | IdleWorkers],
            {noreply, State#state{idle_workers=NewIdle}}
    end.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


