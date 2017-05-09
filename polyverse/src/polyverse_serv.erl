-module(polyverse_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, add_file/1, get_files/0, test_atom/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, polyverse_serv}, ?MODULE,[], []).

add_file(FileName) ->
	io:format("add_file call~n"),
	gen_server:call(polyverse_serv, {add_file, FileName}).

get_files() ->
	gen_server:call(polyverse_serv, {get_files}).

test_atom() ->
	test_atom.

% --- Gen Server Function
init([]) ->
	{ok, StorageDirectory} = application:get_env(storage_directory),
	{ok, FileNames} = file:list_dir(StorageDirectory),
	io:format("Files in storage: ~n"),
	[io:format("~s ~n", [X]) || X <- FileNames],
	{ok, FileNames}.

% Adds a file by file name to the local polyverse storage and initiates broadcast of the file to other nodes
handle_call({get_files}, _From, FileNames) ->
	{reply, FileNames, FileNames};

handle_call({add_file, FileName}, _From, FileNames) ->
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
					{reply, file_added, [Digest|FileNames]};
				{error, Reason} ->
					io:format("Error encrypting file. Reason: ~w ~n", [Reason]),
					{reply, error_encrypting_file, FileNames}
			end;
		_ ->
			{reply, error_adding_file, FileNames}
	end.

handle_cast(_Data, FileNames) ->
	{noreply, FileNames}.

handle_info(_Msg, FileNames) ->
    {noreply, FileNames}.

terminate(normal, _FileNames) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 

