-module(polyverse_super).
-export([start_link/1, init/1]).
-behaviour(supervisor).
 
start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).
 
init(Args) ->
	io:format("polyverse super args: ~w ~n", [Args]),
	MaxRestart = 2,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime},
		[{polyverse_serv,
			{polyverse_serv, start_link, []},
			permanent,
			5000, % Shutdown time
			worker,
			[polyverse_serv]}]}}.