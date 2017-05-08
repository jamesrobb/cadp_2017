{application, polyverse,
	[{vsn, "0.1"},
	{modules, [polyverse]},
	{mod, {polyverse, []}},
	{env, [{storage_directory, './storage1/'},
		   {aes_key, <<"A548254SD54DY4ASDF54SDFA454R5E4E">>},
		   {aes_ivec, <<37,151,253,172,241,160,52,9,198,140,83,197,5,14,108,164>>}]}
	]
}.