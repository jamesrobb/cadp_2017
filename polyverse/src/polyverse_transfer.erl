-module(polyverse_transfer).
-export([encrypt/6,pad/2, hexstring/1]).

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