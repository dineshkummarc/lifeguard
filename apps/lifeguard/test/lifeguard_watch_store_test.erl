-module(lifeguard_watch_store_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(TEST_MODULE, lifeguard_watch_store).

start_stop_test() ->
    % Test starting is okay.
    {ok, Pid} = ?TEST_MODULE:start_link(?cmd("mktemp -t lifeguard")),

    % Stop it.
    unlink(Pid),
    exit(Pid, normal).

main_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_set_watch/1,
            fun test_get_watch_nonexistent/1,
            fun test_get_watch/1,
            fun test_list_watches/1,
            fun test_delete_watch_nonexistent/1,
            fun test_delete_watch/1
        ]}.

setup() ->
    % Start up the watch store.
    Pid = case ?TEST_MODULE:start_link(?cmd("mktemp -t lifeguard")) of
        {ok, P} -> P;
        {error, {already_started, P}} -> P
    end,
    Pid.

teardown(Pid) ->
    % Stop the watch store
    unlink(Pid),
    exit(Pid, normal).

test_set_watch(_) ->
    fun() ->
            ok = ?TEST_MODULE:set_watch("foo", "code", 5)
    end.

test_get_watch_nonexistent(_) ->
    fun() ->
            {error, no_watch} = ?TEST_MODULE:get_watch("i-dont-exist")
    end.

test_get_watch(_) ->
    fun() ->
            Name = "foo",
            Code = "code",
            Interval = 5,

            % Set it
            ok = ?TEST_MODULE:set_watch(Name, Code, Interval),

            % Get it
            {ok, {Name, Code, Interval}} = ?TEST_MODULE:get_watch(Name)
    end.

test_list_watches(_) ->
    fun() ->
            Code = "code",
            Interval = 5,

            % Set it
            ok = ?TEST_MODULE:set_watch("foo", Code, Interval),
            ok = ?TEST_MODULE:set_watch("bar", Code, Interval),

            % Get em
            {ok, Result} = ?TEST_MODULE:list_watches(),
            ?assert(length(Result) =:= 2)
    end.

test_delete_watch_nonexistent(_) ->
    fun() ->
            ok = ?TEST_MODULE:delete_watch("whatever")
    end.

test_delete_watch(_) ->
    fun() ->
            Name = "foo",

            % Set it
            ok = ?TEST_MODULE:set_watch(Name, "foo", "bar"),

            % Delete it
            ok = ?TEST_MODULE:delete_watch(Name),

            % Verify it is gone
            {error, no_watch} = ?TEST_MODULE:get_watch(Name)
    end.
