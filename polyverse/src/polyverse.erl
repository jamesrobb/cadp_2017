-module(polyverse).
-behaviour(application).
-export([start/2, stop/1, add_file/1, get_files/0, get_pid/0, test_atom/0]).
 
start(normal, _Args) ->
	io:format("application pid: ~w ~n", [self()]),
	polyverse_super:start_link().
 
stop(_State) ->
	io:format("application stopped"),
	ok.

add_file(FileName) ->
	polyverse_serv:add_file(FileName).

get_files() ->
	polyverse_serv:get_files().

get_pid() ->	
	self().

test_atom() ->
	polyverse_serv:test_atom().