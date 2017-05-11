-module(polyverse_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, add_file/1, get_file/1, get_file_list/0, get_local_files_list/0, add_node/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {blacklist=[]}).

% --- Polyverse Functions
%
%
start_link() ->
	gen_server:start_link({local, polyverse_serv}, ?MODULE, [], []).

% API for adding file to Polyverse
add_file(FileName) ->
	io:format("add_file call~n"),
	case gen_server:call(polyverse_serv, {add_local_file, FileName}) of
		{file_added, EncryptedFileName} ->
			gen_server:call(polyverse_serv, {broadcast_file, EncryptedFileName});
		_ ->
			ok
	end.

% API for getting a file from Polyverse
get_file(FileName) ->
	case lists:member(FileName, get_local_files_list()) of
		true ->
			io:format("File ~s already in local storage.~n", [FileName]);
		false ->
			get_file_from_network(nodes(), FileName)
	end.

% Will sequentially attempt to retrive file from Nodes
get_file_from_network([], FileName) ->
	io:format("Filename ~s not found on Polyverse network.~n", [FileName]);

get_file_from_network(Nodes, FileName) ->
	RemoteNode = hd(Nodes),
	case gen_server:call({polyverse_serv, RemoteNode}, {request_file, node(), FileName}) of
		blacklisted ->
			io:format("Node ~w has blacklisted this node.~n", [RemoteNode]),
			get_file_from_network(tl(Nodes), FileName);
		file_not_found ->
			io:format("Node ~w does not have file ~s ~n", [RemoteNode, FileName]),
			get_file_from_network(tl(Nodes), FileName);
		transmitting_file ->
			io:format("Node ~w has file ~s and is transmitting it now.~n", [RemoteNode, FileName])
	end.

% API for getting list of files in local storage
get_file_list() ->
	gen_server:call(polyverse_serv, {get_file_list}).

% Attempts to connect to Node and its connected nodes
add_node(Node) ->
	case net_kernel:connect_node(Node) of
		true ->
			io:format("Connected to Polyverse via node: ~s~n", [Node]),
			timer:sleep(1000),
            sync_with_nodes();
		false ->
			io:format("Unable to connect to node: ~s~n", [Node])
	end.

sync_with_nodes() ->
    LocalFiles = get_local_files_list(),
    NodesList = nodes(),
    sync_with_node(NodesList, LocalFiles).

sync_with_node([], _) ->
    sync_done;

sync_with_node([Node|Nodes], LocalFiles) ->
    io:format("attempting sync with ~w~n", [Node]),
    RemoteFiles = gen_server:call({polyverse_serv, Node}, {get_file_list}),
    sync_lists_and_send(LocalFiles, RemoteFiles, node(), Node),
    NewLocalFiles = get_local_files_list(),
    sync_with_node(Nodes, NewLocalFiles).

sync_lists_and_send([], [], _, _) ->
    sync_done;

sync_lists_and_send([LocalHead|Local], [LocalHead|Remote], LocalNode, RemoteNode) ->
    sync_lists_and_send(Local, Remote, LocalNode, RemoteNode);

sync_lists_and_send([LocalHead|Local], [], LocalNode, RemoteNode) ->
    {ok, StorageDirectory} = application:get_env(polyverse, storage_directory),
    FileLocation = lists:concat([StorageDirectory, LocalHead]),
    polyverse_transfer:send_file(RemoteNode, FileLocation, LocalHead),
    sync_lists_and_send(Local, [], LocalNode, RemoteNode);

sync_lists_and_send([], [RemoteHead|Remote], LocalNode, RemoteNode) ->
    gen_server:call({polyverse_serv, RemoteNode}, {request_file, LocalNode, RemoteHead}),
    sync_lists_and_send([], Remote, LocalNode, RemoteNode);

sync_lists_and_send([LocalHead|Local], [RemoteHead|Remote], LocalNode, RemoteNode) ->
    LHinRemote = lists:member(LocalHead, Remote),
    RHinLocal = lists:member(RemoteHead, Local),
    case LHinRemote of
        false ->
	        {ok, StorageDirectory} = application:get_env(polyverse, storage_directory),
	        FileLocation = lists:concat([StorageDirectory, LocalHead]),
	        polyverse_transfer:send_file(RemoteNode, FileLocation, LocalHead);
	    true ->
	    	ok
        end,
    case RHinLocal of
        false ->
            gen_server:call({polyverse_serv, RemoteNode}, {request_file, LocalNode, RemoteHead});
        true ->
        	ok
        end,
    sync_lists_and_send(Local, Remote, LocalNode, RemoteNode).

get_local_files_list() ->
	{ok, StorageDirectory} = application:get_env(polyverse, storage_directory),
	{ok, FileNames} = file:list_dir(StorageDirectory),
	FileNames.

% --- gen_server Functions
%
%
init([]) ->
	io:format("Files in storage: ~n"),
	[io:format("~s ~n", [X]) || X <- get_local_files_list()],
	{ok, #state{blacklist=[]}}.

% When a file is request, spawn a thread to send it back if it exists
% A message is sent back denoting that the file is being transmitted
handle_call({request_file, Node, FileName}, _From, State) ->
	io:format("Node ~w is requesting file ~s ~n", [Node, FileName]),
	NodeBlacklisted = lists:member(Node, State#state.blacklist),
	if
		NodeBlacklisted == true ->
			{reply, blacklisted, State};
		true ->
			case lists:member(FileName, get_local_files_list()) of
				true ->
					{ok, StorageDirectory} = application:get_env(storage_directory),
					FileLocation = lists:concat([StorageDirectory, FileName]),
					spawn(fun() -> polyverse_transfer:send_file(Node, FileLocation, FileName) end),
					{reply, transmitting_file, State};
				false ->
					{reply, file_not_found, State}
			end
	end;

handle_call({receive_file, Node, FileName, Binary}, _From, State) ->
	NodeBlacklisted = lists:member(Node, State#state.blacklist),
	if
		NodeBlacklisted == true ->
			{reply, blacklisted, State};
		true ->
			NewState = case polyverse_transfer:receive_file(Node, FileName, Binary) of
				malicious_file ->
					io:format("Malicious file received. Blacklisting node ~w ~n", [Node]),
					State#state{blacklist = [Node | State#state.blacklist]};
				file_written ->
					io:format("File ~s written to storage~n", [FileName]),
					State;
				error ->
					io:format("Error receiving file.~n"),
					State
			end,
			{reply, file_transfer_done, NewState}
	end;

handle_call({broadcast_file, FileName}, _From, State) ->
	{ok, StorageDirectory} = application:get_env(storage_directory),
	FileLocation = lists:concat([StorageDirectory, FileName]),
	[spawn(fun() -> polyverse_transfer:send_file(X, FileLocation, FileName) end) || X <- nodes(), lists:member(X, State#state.blacklist) == false],
	{reply, broadcast_made, State};

% List files currently in local storage
handle_call({get_file_list}, _From, State) ->
	{reply, get_local_files_list(), State};

% Adds a file by file name to the local polyverse storage and initiates broadcast of the file to other nodes
handle_call({add_local_file, FileName}, _From, State) ->
	io:format("handle_call: add_file~n"),

	% Check if the file exists, if so attempt to encrypt it otherwise return error
	case file:read_file_info(FileName) of
		{ok, _FileInfo} ->
			{ok, StorageDirectory} = application:get_env(storage_directory),
			{ok, GpgId} = application:get_env(gpg_id),
			TempFileName = lists:concat([FileName, '.tmp']),
			polyverse_transfer:encrypt(FileName, TempFileName, GpgId),

			% Check if encryption was successful, otherwise return an error
			case file:open(TempFileName, [read, binary]) of
				{ok, TmpFileDevice} ->
					Context = crypto:hash_init(sha256),
					Digest = polyverse_transfer:produce_digest(TmpFileDevice, Context, 128),
					file:copy(TempFileName, lists:concat([StorageDirectory, Digest])),
					file:delete(TempFileName),
					{reply, {file_added, Digest}, State};
				{error, Reason} ->
					io:format("Error encrypting file. Reason: ~w ~n", [Reason]),
					{reply, error_encrypting_file, State}
			end;
		_ ->
			{reply, error_adding_file, State}
	end.

handle_cast(_Data, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used.
    {ok, State}. 

