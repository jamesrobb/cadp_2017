-module(polyverse).
-behaviour(application).
-export([start/2, stop/1, add_file/1, get_file/1, get_file_list/0, connect_to_node/1]).
 
start(normal, Args) ->
	io:format("application pid: ~w ~n", [self()]),
	polyverse_super:start_link(Args).
 
stop(_State) ->
	io:format("application stopped"),
	ok.

% Add FileName to network (remotely and locally)
add_file(FileName) ->
	polyverse_serv:add_file(FileName).

% Get FileName from network (checks locally first, and then remotely)
get_file(FileName) ->
	polyverse_serv:get_file(FileName).

% List files on this Polyverse node
get_file_list() ->
	polyverse_serv:get_file_list().

% Connect this node another Polyverse (Erlang) node
connect_to_node(Node) ->
	polyverse_serv:add_node(Node).