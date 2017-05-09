-module(polyverse_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, add_local_file/1, get_files/0, add_node/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {blacklist=[]}).

% --- Polyverse Functions
start_link() ->
	gen_server:start_link({local, polyverse_serv}, ?MODULE,[], []).

% API for adding file to Polyverse
add_local_file(FileName) ->
	io:format("add_file call~n"),
	case gen_server:call(polyverse_serv, {add_local_file, FileName}) of
		{file_added, EncryptedFileName} ->
			gen_server:call(polyverse_serv, {broadcast_file, EncryptedFileName});
		_ ->
			ok
	end.

% API for getting list of files in local storage
get_files() ->
	gen_server:call(polyverse_serv, {get_files}).

% Attempts to connect to Node and its connected nodes
add_node(Node) ->
	case net_kernel:connect_node(Node) of
		true ->
			io:format("Connected to Polyverse via node: ~s~n", [Node]);
		false ->
			io:format("Unable to connect to node: ~s~n", [Node])
	end.

read_files_from_storage() ->
	{ok, StorageDirectory} = application:get_env(storage_directory),
	{ok, FileNames} = file:list_dir(StorageDirectory),
	FileNames.

% --- Gen Server Function
%
%
init([]) ->
	io:format("Files in storage: ~n"),
	[io:format("~s ~n", [X]) || X <- read_files_from_storage()],
	{ok, #state{blacklist=[]}}.

handle_call({file_transfer, Node, FileName, Binary}, _From, State) ->
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
	{reply, file_transfer_done, NewState};

handle_call({broadcast_file, FileName}, _From, State) ->
	{ok, StorageDirectory} = application:get_env(storage_directory),
	FileLocation = lists:concat([StorageDirectory, FileName]),
	[spawn(fun() -> polyverse_transfer:send_file(X, FileLocation, FileName) end) || X <- nodes()],
	{reply, broadcast_made, State};

% List files currently in local storage
handle_call({get_files}, _From, State) ->
	{reply, read_files_from_storage(), State};

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
    %% but will not be used. Only a version on the next
    {ok, State}. 

