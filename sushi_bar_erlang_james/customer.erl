-module(customer).
-export([take_seat/0,spawn_customers/1]).

spawn_customers(N) ->
	spawn_customers(1, N, fun () -> spawn(fun () -> take_seat() end) end).

spawn_customers(Max, Max, F) -> [F()];
spawn_customers(I, Max, F) -> [F() | spawn_customers(I+1, Max, F) ].
	

take_seat() ->
	timer:sleep(rand:uniform(2000)),
	io:format("Customer ~w is attempting to sit.~n", [self()]),
	bar!{self(), sit},
	receive
		{seated} ->
			io:format("Customer ~w is eating.~n", [self()]),
			timer:sleep(rand:uniform(2000)),
			bar!{self(), leave},	
			io:format("Customer ~w has left.~n", [self()])
	end.

