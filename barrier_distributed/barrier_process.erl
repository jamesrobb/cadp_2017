-module(barrier_process).
-export([spawn_process/1, spawn_process/2]).

% To run a simulation you start the initial process by calling
% spawn_process(N) where N is the total amount of processes your 
% barrier is supposed to block.
% After that you create N-1 other processes with 
% spawn_process(ConnectPid, N) where connect pid is the pid returned 
% from the initial call to the spawn_process(N) or any other pid returned 
% from the calls to the later calls to spawn_process(ConnectPid,N) and N 
% is the total amount of processes your barrier is supposed to block.
%
% The system works in such a fashion where the initial action of the processes 
% is to discover the other nodes in the system. After discovering other nodes 
% in the system the nodes are then a part of the total system.
% This process is repeated untill there are N processes in the system which then 
% broadcast to each other that they are ready to start their "Tasks()", which could 
% be substituded with any functionality you might want to synchronize.
%
% Discovery is done through messaging other nodes inside the system that you would 
% like to join the system. When that message is processed you will be added to 
% a list which is propagated through the system in order to keep consistency within 
% nodes in the system.
%
% Some major assumptions are made in this distributed barrier. Those include 
% that we do not have any bad characters in the system, that no node will ever 
% crash or not do what it is supposed to.
%
% This implementation works only if the number of processes that are in the system 
% is exactly the amount of nodes the barrier is supposed to block.


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
