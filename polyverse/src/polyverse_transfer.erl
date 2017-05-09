-module(polyverse_transfer).
-export([encrypt/6,pad/2, hexstring/1, sendFile/4]).

encrypt(IoDeviceIn, IoDeviceOut, Context, BlockSize, Key, Ivec) ->
	case file:read(IoDeviceIn, BlockSize) of
		{ok, Data} ->
			EncryptedData = crypto:block_encrypt(aes_cbc256, Key, Ivec, pad(Data, BlockSize)),
			file:write(IoDeviceOut, EncryptedData),
			NewContext = crypto:hash_update(Context, EncryptedData),
			io:format("~s~n", [Data]),
			encrypt(IoDeviceIn, IoDeviceOut, NewContext, BlockSize, Key, crypto:next_iv(aes_cbc, EncryptedData));
		eof ->
			Digest = hexstring(crypto:hash_final(Context)),
			io:format("eof. Digest: ~s ~n", [Digest]),
			{ok, Digest}
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

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).


receiveFile() ->
    receive
        {FromPid, FromNode, add, Hashedname, Binary} ->
            file:write(Hashedname, Binary), % error handling TODO
        {FromPid, FromNode}!{ok}
    % after 10 seconds time it out
    after 10000 ->
              timeout
    end.

sendFile(ToPid, ToNode, Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            {ok, Binary} = file:read(Filename),
            % encryption on binary %
            %
            % end 
            Hashedname = crypto:hash(sha512, Binary),
            % send it to another node
            {ToPid, ToNode}!{self(), node(), add, Hashedname, Binary},
            receive
                  {ok} -> ok
            after 10000 ->
                  timeout
            end;
        {error, Reason} ->
            io:format("something went wrong. Reason: ~w~n", [Reason])
   end.
