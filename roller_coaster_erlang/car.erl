-module(car).
-export([spawn_cars/2]).

% Spawns N cars, each holds C passengers
spawn_cars(N, C) ->
	spawn_cars(1, N, fun () -> spawn(fun () -> load(C, []) end) end).

spawn_cars(Max, Max, F) -> [F()];
spawn_cars(I, Max, F) -> [F() | spawn_cars(I+1, Max, F) ].

% C seats available in the car, Passengers is list of Pids of passengers in car
load(C, Passengers) ->
	receive
		{Pid, boarded} when length(Passengers) + 1 < C ->
			load(C, Passengers ++ [Pid]);
		{Pid, boarded} ->
			io:format("Cart ~w fully loaded. Running around track.~n", [self()]),
			unload(C, Passengers ++ [Pid])
	end.

% Base case with empty Passengers list
unload(C, []) ->
	conductor!{self(), ride_finished},
	load(C, []);

% C seats available in the car, Passengers is list of Pids of passengers in car
unload(C, Passengers) ->
	X = hd(Passengers),
	X!{self(), ride_finished},
	unload(C, tl(Passengers)).