-module(lifeguard_js_vm).
-behavior(gen_server).
-export([start_link/1,
         dispatch/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(vm_state, {
        id, % ID of the VM
        vm  % Actual V8 VM
    }).

%% @doc Start the JS VM under a supervision tree.
start_link(Number) ->
    gen_server:start_link({local, vmid(Number)}, ?MODULE, [Number], []).

%% @doc Dispatches a method to the proper VM.
dispatch(VMID, Message) ->
    gen_server:cast(vmid(VMID), Message).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Number]) ->
    lager:info("JS VM started: ~p", [Number]),

    % Create a new V8 VM
    {ok, VM} = erlv8_vm:start(),
    init_vm_globals(VM),

    % Create our state
    State = #vm_state{
        id = Number,
        vm = VM
    },

    % Mark ourselves as idle, ready to receive work
    set_idle(State#vm_state.id),
    {ok, State}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({run_watch, Watch, _From}, State) ->
    {ok, Name} = lifeguard_watch:get_name(Watch),
    lager:info("Running watch in VM: ~p", [Name]),

    % Tell the manager that we're ready for more work!
    set_idle(State#vm_state.id),
    {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, State) ->
    lager:info("JS VM stopped; ~d", [State#vm_state.id]),

    % Stop the V8 VM
    erlv8_vm:stop(State#vm_state.vm).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_vm_globals(VM) ->
    % Load our builtins
    Path = filename:join(code:priv_dir(lifeguard), "builtins.js"),
    {ok, JSData} = file:read_file(Path),

    % Add the builtins to this VM runtime
    {ok, _} = erlv8_vm:run(VM, binary_to_list(JSData)).

set_idle(VMID) ->
    lifeguard_js_manager:idle_vm(VMID).

vmid(Number) ->
    list_to_atom("js_vm_" ++ integer_to_list(Number)).
