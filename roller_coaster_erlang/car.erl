-module(car).
-export([spawn_cars/2]).

% N cars, each holds C passengers
spawn_cars(N, C) ->
	spawn_cars(1, N, fun () -> spawn(fun () -> load(C, []) end) end).

spawn_cars(Max, Max, F) -> [F()];
spawn_cars(I, Max, F) -> [F() | spawn_cars(I+1, Max, F) ].

% C capacity, Passenger in car
load(C, Passengers) ->
	receive
		{Pid, boarded} when length(Passengers) + 1 < C ->
			load(C, Passengers ++ [Pid]);
		{Pid, boarded} ->
			io:format("Cart ~w fully loaded. Running around track.~n", [self()]),
			unload(C, Passengers ++ [Pid])
	end.

% C capacity, Passengers in car
unload(C, []) ->
	conductor!{self(), ride_finished},
	load(C, []);

unload(C, Passengers) ->
	X = hd(Passengers),
	X!{self(), ride_finished},
	unload(C, tl(Passengers)).