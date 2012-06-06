-module(lifeguard_ds_manager_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(TEST_MODULE, lifeguard_ds_manager).

-record(test_state, {manager, source_name, source_pid}).

main_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_list/1,
            fun test_call_bad_source/1,
            fun test_call_good_source/1,
            fun test_call_good_source_crash/1
        ]}.

setup() ->
    SourceName = <<"echo">>,
    SourceID   = lifeguard_ds_manager:data_source_id(SourceName),
    ManagerPid = case ?TEST_MODULE:start_link([SourceName]) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,
    SourcePid = case lifeguard_ds_echo:start_link(SourceID, SourceName, []) of
        {ok, Pid2} -> Pid2;
        {error, {already_started, Pid2}} -> Pid2
    end,
    #test_state{
        manager=ManagerPid,
        source_name=SourceName,
        source_pid=SourcePid
    }.

teardown(#test_state{manager=ManagerPid, source_pid=SourcePid}) ->
    % Cleanup the manager
    unlink(ManagerPid),
    exit(ManagerPid, normal),

    % Cleanup the source
    unlink(SourcePid),
    exit(SourcePid, normal).

test_list(#test_state{source_name=SourceName}) ->
    fun() -> {ok, [SourceName]} = ?TEST_MODULE:list() end.

test_call_bad_source(_State) ->
    fun() -> {error, no_data_source} = ?TEST_MODULE:get(<<"this_is_a_bad_name">>, []) end.

test_call_good_source(#test_state{source_name=Source}) ->
    fun() -> {ok, result} = ?TEST_MODULE:get(Source, [result]) end.

test_call_good_source_crash(#test_state{source_name=Source}) ->
    fun() -> {error, data_source_error} = ?TEST_MODULE:get(Source, [crash]) end.
