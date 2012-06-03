-module(lifeguard_watch_store_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(TEST_MODULE, lifeguard_watch_store).

build_watch(Name, Code, Interval) ->
    W1 = lifeguard_watch:new(),
    W2 = lifegaurd_watch:set_name(W1, Name),
    W3 = lifeguard_watch:set_code(W2, Code),
    W4 = lifeguard_watch:set_interval(W3, Interval),
    W4.

start_stop_test() ->
    % Test starting is okay.
    {ok, Pid} = ?TEST_MODULE:start_link(?cmd("mktemp -t lifeguard")),

    % Stop it.
    unlink(Pid),
    exit(Pid, normal).

main_test_FIXME() ->
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
    {ok, Pid} = ?TEST_MODULE:start_link(?cmd("mktemp -t lifeguard")),

    Pid.

teardown(Pid) ->
    % Stop the watch store
    unlink(Pid),
    exit(Pid, normal).

test_set_watch(Pid) ->
    fun() ->
            ok = gen_server:call(Pid, {set, build_watch("foo", "code", 5)})
    end.

test_get_watch_nonexistent(Pid) ->
    fun() ->
            {error, no_watch} = gen_server:call(Pid, {get, "i-dont-exist"})
    end.

test_get_watch(Pid) ->
    fun() ->
            Name = "foo",
            Code = "code",
            Interval = 5,

            % Set it
            ok = gen_server:call(Pid, {set, build_watch(Name, Code, Interval)}),

            % Get it
            {ok, {Name, Code, Interval}} = gen_server:call(Pid, {get, Name})
    end.

test_list_watches(Pid) ->
    fun() ->
            Code = "code",
            Interval = 5,

            % Set it
            ok = gen_server:call(Pid, {set, build_watch("foo", Code, Interval)}),
            ok = gen_server:call(Pid, {set, build_watch("bar", Code, Interval)}),

            % Get em
            {ok, Result} = gen_server:call(Pid, list),
            ?assert(length(Result) =:= 2)
    end.

test_delete_watch_nonexistent(Pid) ->
    fun() ->
            ok = gen_server:call(Pid, {delete, "whatever"})
    end.

test_delete_watch(Pid) ->
    fun() ->
            Name = "foo",

            % Set it
            ok = gen_server:call(Pid, {set, build_watch(Name, "foo", "bar")}),

            % Delete it
            ok = gen_server:call(Pid, {delete, Name}),

            % Verify it is gone
            {error, no_watch} = gen_server:call(Pid, {get, Name})
    end.
