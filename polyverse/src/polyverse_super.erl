-module(polyverse_super).
-export([start_link/0, init/1]).
-behaviour(supervisor).
 
start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).
 
init([]) ->
	MaxRestart = 2,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime},
		[{polyverse_serv,
			{polyverse_serv, start_link, []},
			permanent,
			5000, % Shutdown time
			worker,
			[polyverse_serv]}]}}.