-module(semaphore).
-export([create/1,post/1,wait/1,destroy/1]).

create(N) ->
	Pid = spawn(fun () -> server(N) end),
	Pid.

post(Pid) ->
	Ref = make_ref(),
	Pid!{self(), Ref, post},
	receive
		{Ref, ok} -> ok;
		{Ref, error} -> error(semaphore_destroyed)
	end.

wait(Pid) ->
	Ref = make_ref(),
	Pid!{self(), Ref, wait},
	receive
		{Ref, ok} -> ok;
		{Ref, error} -> error(semaphore_destroyed)
	end.

destroy(Pid) ->
	Ref = make_ref(),
	Pid!{self(), Ref, destroy},
	receive
		{Ref, ok} -> ok;
		{Ref, error} -> ok
	end.

server(N) ->
	receive
		{Pid, Ref, post} ->
			Pid!{Ref, ok},
			server(N + 1);
		{Pid, Ref, wait} when N > 0 ->
			Pid!{Ref, ok},
			server(N - 1);
		{Pid, Ref, destroy} ->
			shutdown(),
			Pid!{Ref, ok},
			ok
		end.


shutdown() ->
	receive
		{Pid, Ref, _} ->
			Pid!{Ref, error},
			shutdown()
		after 0 ->
				ok
	end.