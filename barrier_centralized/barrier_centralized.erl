-module(barrier_centralized).
-export([init/1, wait/1, spawn_waiters/2]).

init(N) ->
    Pid = spawn(fun () -> barrier(0, N) end),
    Pid.

spawn_waiters(0, _) ->
    ok;

spawn_waiters(N, Bar_pid) ->
    spawn(fun () -> wait(Bar_pid) end),
    spawn_waiters(N-1, Bar_pid).

wait(B_Pid) ->
    B_Pid!{self(), enter},
    io:fwrite("~w: Entered the barrier~n", [self()]),
    receive
        {ok} -> ok
    end,
    io:fwrite("~w: Exited the barrier~n", [self()]),
    timer:sleep(rand:uniform(2000)), % Task
    wait(B_Pid).

barrier(M, M) ->
    io:fwrite("Barrier opening~n"),
    release(M);

barrier(N, M) when N < M ->
    receive
        {Pid, enter} ->
            self()!{Pid, delayed},
            barrier(N+1, M)
    end.

release(M) ->
    receive
        {Pid, delayed} -> Pid!{ok},
        release(M)
    after 0 ->
        barrier(0,M)
    end.

