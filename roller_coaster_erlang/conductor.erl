-module(conductor).
-export([start/2]).

% Pids of Cars, each car can seat C passengers
start(Cars, C) ->
	Pid = spawn(fun () -> assign_car(Cars, 0, C) end),
	register(conductor, Pid).

% Pids of Cars, N currently assigned a seat in a car, C seats in a car total
assign_car(Cars, N, C) ->
	receive
		{Pid, board} when N < C ->
			Pid!{enter_car, hd(Cars)},
			assign_car(Cars, N+1, C);
		{Pid, board} when N >= C ->
			Pid!{reenter_line},
			delay_assignment(Cars, C)
	end.

% Pids of Cars, C seats in a car total
delay_assignment(Cars, C) ->
	X = hd(Cars),
	receive
		{X, ride_finished} ->
			assign_car(tl(Cars) ++ [hd(Cars)], 0, C)
	end.