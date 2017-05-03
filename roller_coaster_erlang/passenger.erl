-module(passenger).
-export([spawn_passengers/1]).

spawn_passengers(N) ->
	spawn_passengers(1, N, fun () -> spawn(fun () -> take_ride(true) end) end).

spawn_passengers(Max, Max, F) -> [F()];
spawn_passengers(I, Max, F) -> [F() | spawn_passengers(I+1, Max, F) ].

take_ride(FirstEntry) ->
	timer:sleep(rand:uniform(2000)),
	if
		FirstEntry == true ->
			io:format("Passenger ~w enters the queue for a ride~n", [self()]);
		FirstEntry == false ->
			io:format("Passenger ~w re-enters the queue for a ride~n", [self()])
	end,
	conductor!{self(), board},
	receive
		{enter_car, Enter_Car_Pid} ->
			Enter_Car_Pid!{self(), boarded},
			receive
				{Exit_Car_Pid, ride_finished} ->
					io:format("Passenger ~w exited car ~w (entered into car ~w).~n", [self(), Exit_Car_Pid, Enter_Car_Pid])
			end;
		{reenter_line} ->
			take_ride(false)
	end.