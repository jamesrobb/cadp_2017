-module(polyverse).
-behaviour(application).
-export([start/2, stop/1]).
 
start(normal, _Args) ->
	polyverse_super:start_link().
 
stop(_State) ->
	ok.