-module(lifeguard_js_vm_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(TEST_MODULE, lifeguard_js_vm).

main_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_start_stop/1
        ]}.

setup() ->
    % Start the erlv8 application, which is required...
    application:start(erlv8).

teardown(_) ->
    % We don't worry about stopping the application because other
    % things might need it.
    ok.

%% @doc Tests that lifeguard_js_vm gen_server can start and stop properly.
test_start_stop(_) ->
    fun() ->
            % Test that it starts properly
            {ok, Pid} = ?TEST_MODULE:start_link(1),

            % Close it
            unlink(Pid),
            exit(Pid, normal)
    end.
