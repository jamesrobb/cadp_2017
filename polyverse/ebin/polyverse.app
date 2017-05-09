{application, polyverse,
	[{vsn, "0.1"},
	{modules, [polyverse]},
	{mod, {polyverse, []}},
	{env, [{storage_directory, './storage1/'},
		   {gpg_id, '80C1B81A06EF6AE7B9104A932EEB5C83468631FF'},
		   {aes_key, <<"A548254SD54DY4ASDF54SDFA454R5E4E">>},
		   {aes_ivec, <<37,151,253,172,241,160,52,9,198,140,83,197,5,14,108,164>>}]}
	]
}.