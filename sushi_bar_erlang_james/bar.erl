-module(bar).
-export([start/1]).

start(K) ->
	Pid = spawn(fun () -> server(K, 0) end),
	register(bar, Pid).

server(K, N) ->
	io:format("There are ~.10B seats of ~.10B left at the bar.~n", [K-N, K]),
	receive
		{Pid, sit} when N < K ->
			io:format("Customer ~w took a seat.~n", [Pid]),
			Pid!{seated},
			server(K, N+1);
		{Pid, sit} when N >= K ->
			io:format("-- Customer ~w will wait for bar to empty.~n", [Pid]),
			self()!{Pid, delayed_sit},
			server(K, N);
		{Pid, leave} when (N-1) == 0 ->
			io:format("-- Bar empty. Allowing up to ~.10B delayed customers to sit.~n", [K]),
			seat_waiting(K);
		{Pid, leave} when (N-1) > 0 ->
			server(K, N-1)
	end.

seat_waiting(K) ->
	seat_waiting(1, K).

seat_waiting(Max, Max) ->
	receive
		{Pid, delayed_sit} ->
			io:format("Customer ~w attempting delayed sit.~n", [Pid]),
			bar!{Pid, sit},
			server(Max, 0)
	after 0 ->
		server(Max, 0)
	end;

seat_waiting(I, Max) ->
	receive
		{Pid, delayed_sit} ->
			io:format("Customer ~w attempting delayed sit.~n", [Pid]),
			bar!{Pid, sit},
			seat_waiting(I+1, Max)
	after 0 ->
		server(Max, 0)
	end.