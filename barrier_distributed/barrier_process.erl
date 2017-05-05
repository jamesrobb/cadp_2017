-module(barrier_process).
-export([spawn_process/1, spawn_process/2]).

spawn_process(N) ->
	spawn(fun() -> start_discovery(N, [self()], 0) end).

spawn_process(ConnectPid, N) ->
	spawn(fun() -> connect(ConnectPid, N) end).

connect(ConnectPid, N) ->
	ConnectPid!{self(), request_pids},
	start_discovery(N, [self()], 0).

start_discovery(N, Pids, K) ->
	timer:sleep(10),
	receive
		{discovery_pids, DiscoveryPids} ->
			StrippedPids = strip_duplicates(Pids ++ DiscoveryPids),
			if
				length(StrippedPids) == N ->
					task(StrippedPids, N);
				length(StrippedPids) < N ->
					lists:nth(K+1, lists:delete(self(), StrippedPids))!{self(), request_pids},
					start_discovery(N, StrippedPids, (K+1) rem length(StrippedPids)-1)
			end;
		{RequesterPid, request_pids} ->
			StrippedPids = strip_duplicates(Pids ++ [RequesterPid]),
			RequesterPid!{discovery_pids, StrippedPids},
			if
				length(StrippedPids) == N ->
					broadcast_pids(lists:delete(self(), StrippedPids), StrippedPids),
					task(StrippedPids, N);
				length(StrippedPids) < N ->
					start_discovery(N, StrippedPids, K)
			end
	end.

task(Pids, N) ->
	io:format("Process ~w performing its task.~n", [self()]),
	timer:sleep(rand:uniform(2000)),
	broadcast_at_barrier(lists:delete(self(), Pids)),
	barrier(N, [self()]),
	io:format("Process ~w left barrier.~n", [self()]),
	task(Pids, N).

barrier(N, BarrierPids) ->
	receive
		{Pid, at_barrier} ->
			StrippedPids = strip_duplicates(BarrierPids ++ [Pid]),
			if
				length(StrippedPids) < N ->
					barrier(N, StrippedPids);
				true ->
					ok
			end
	end.

broadcast_pids(ToPids, DiscoveryPids) ->
	lists:map(fun(Pid) -> Pid!{discovery_pids, DiscoveryPids} end, ToPids). 

broadcast_at_barrier([]) ->
	ok;
broadcast_at_barrier([HPid|TPids]) ->
	HPid!{self(), at_barrier},
	broadcast_at_barrier(TPids).

strip_duplicates(List) ->
	sets:to_list(sets:from_list(List)).