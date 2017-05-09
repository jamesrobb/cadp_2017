-module(polyverse_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
start_link() ->
	gen_server:start_link({local, polyverse_serv}, ?MODULE,[], []).

% Server functions
init([]) ->
	{ok, StorageDirectory} = application:get_env(storage_directory),
	{ok, Filenames} = file:list_dir(StorageDirectory),
	io:format("files in storage ~s~n", [Filenames]),
	io:format("polyverse server inited.~n"),
	{ok, GpgId} = application:get_env(gpg_id),
	polyverse_transfer:encrypt('tests.txt', 'tmp', GpgId),
	case file:open('tmp', [read, binary]) of
		{ok, TmpFileDevice} ->
			Context = crypto:hash_init(sha256),
			Digest = polyverse_transfer:produce_digest(TmpFileDevice, Context, 128),
			file:copy("tmp", lists:concat([StorageDirectory, Digest])),
			file:delete("tmp");
		{error, Reason} ->
			io:format("Error encrypting file. Reason: ~w ~n", [Reason])
	end,
	{ok, []}.

handle_call(_Data, _From, _Cats) ->
    ok.

handle_cast(_Data, _Cats) ->
	ok.

handle_info(_Msg, _Cats) ->
    ok.

terminate(normal, _Cats) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 

