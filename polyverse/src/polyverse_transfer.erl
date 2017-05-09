-module(polyverse_transfer).
-export([encrypt/3, decrypt/2, receive_file/3, send_file/3, produce_digest/3, pad/2, hex_string/1]).

encrypt(InputFileName, OutputFileName, GpgId) ->
	Command = lists:concat(['gpg --output ', OutputFileName, ' -r ', GpgId, ' -e ', InputFileName]),
	io:format("~s ~n", [Command]),
	os:cmd(Command).

decrypt(InputFileName, OutputFileName) ->
	Command = lists:concat(['gpg --output ', OutputFileName, ' -d ', InputFileName]),
	io:format("~s ~n", [Command]),
	os:cmd(Command).

receive_file(_Node, FileName, Binary) ->
	io:format("Attempting to receive file.~n"),
	{ok, StorageDirectory} = application:get_env(storage_directory),
	FileLocation = lists:concat([StorageDirectory, FileName]),
	io:format("Attempting to write received file ~s to ~s ~n", [FileName, FileLocation]),
	{ok, WriteDevice} = file:open(FileLocation, [write, binary]),
    file:write(WriteDevice, Binary), % error handling TODO
    file:close(WriteDevice),
	Return = case file:open(FileLocation, [read, binary]) of
		{ok, ReadDevice} ->
			Context = crypto:hash_init(sha256),
			Digest = produce_digest(ReadDevice, Context, 128),
			if
				Digest =/= FileName ->
					file:close(ReadDevice),
					file:delete(FileLocation),
					malicious_file;
				Digest =:= FileName ->
					file_written
			end;
		{error, Reason} ->
			file:delete(FileLocation),
			io:format("Can't verify file. Error opening received file for reason: ~s ~n", [Reason]),
			error;
		_ ->
			file:delete(FileLocation),
			error
	end,
	Return.

send_file(ToNode, FileLocation, FileName) ->
	io:format("Attempting to send file.~n"),
    case file:read_file(FileLocation) of
        {ok, Binary} ->
            % send it to another node
            gen_server:call({polyverse_serv, ToNode}, {receive_file, node(), FileName, Binary});
        {error, Reason} ->
            io:format("Something went wrong. Reason: ~w~n", [Reason])
   end.

produce_digest(IoDevice, Context, BlockSize) ->
	case file:read(IoDevice, BlockSize) of
		{ok, Data} ->
			NewContext = crypto:hash_update(Context, Data),
			produce_digest(IoDevice, NewContext, BlockSize);
		eof ->
			Digest = hex_string(crypto:hash_final(Context)),
			io:format("Digest: ~s ~n", [Digest]),
			Digest
	end.

% Pads Data to Blocksize with 0's. Last byte written is the amount of bytes added.
% Assumes size(Data) < BlockSize
pad(Data, BlockSize) ->
	if
		size(Data) >= BlockSize ->
			Data;
		size(Data) < BlockSize ->
			binary:list_to_bin(pad(binary:bin_to_list(Data), BlockSize, 0))
	end.

pad(Data, BlockSize, Added) ->
	if
		length(Data) == BlockSize-1 ->
			Data ++ [Added + 1];
		length(Data) < BlockSize-1 ->
			pad(Data ++ [0], BlockSize, Added + 1)
	end.

hex_string(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).
