-module(multihash_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ID          ,16#00).
-define(SHA2_256    ,16#12).
-define(SHA2_512    ,16#13).
-define(SHA3_224    ,16#17).
-define(SHA3_256    ,16#16).
-define(SHA3_384    ,16#15).
-define(SHA3_512    ,16#14).
-define(SHAKE_128   ,16#18).
-define(SHAKE_256   ,16#19).
-define(SHA2_256_DBL,16#56).

-define(BLAKE2B_MIN ,16#b201).
-define(BLAKE2B_MAX ,16#b240).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         hash_test/1,
         encode_test/1,
         digest_fail_digest_test/1,
         digest_fail_code_test/1
        ]).

all() ->
    [
     hash_test,
     encode_test,
     digest_fail_digest_test,
     digest_fail_code_test
    ].

init_per_testcase(_, Config) ->
    Input = [{input, [{id, {?ID, "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"}},
                      {sha256, {?SHA2_256, "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"}},
                      {sha256, {?SHA2_256, "2c26b46b"}},
                      {sha256_dbl, {?SHA2_256_DBL, "2c26b46b"}},
                      {sha512, {?SHA2_512, "2c26b46b"}},
                      {sha3_224, {?SHA3_224, "2c26b46b"}},
                      {sha3_384, {?SHA3_384, "2c26b46b"}},
                      {sha3_256, {?SHA3_256, "2c26b46b"}},
                      {sha3_512, {?SHA3_512, "2c26b46b"}},
                      {sha512, {?SHA2_512, "2c26b46b"}},
                      {{blake2b, 64}, {16#b240, "2c26b46b68ffc68ff99b453c1d30413413"}},
                      {shake128, {?SHAKE_128, "f84e95cb5fbd2038863ab27d3cdeac295ad2d4ab96ad1f4b070c0bf36078ef08"}},
                      {shake256, {?SHAKE_256, "1af97f7818a28edfdfce5ec66dbdc7e871813816d7d585fe1f12475ded5b6502b7723b74e2ee36f2651a10a8eaca72aa9148c3c761aaceac8f6d6cc64381ed39"}},
                      {sha3_512, {?SHA3_512, "4bca2b137edc580fe50a88983ef860ebaca36c857b1f492839d6d7392452a63c82cbebc68e3b70a2a1480b4bb5d437a7cba6ecf9d89f9ff3ccd14cd6146ea7e7"}}]
             }],
    Input ++ Config.

end_per_testcase(_, _Config) ->
    ok.

hash_test(Config) ->
    lists:foreach(fun({HashKey, {Code, Hex}}) ->
                          Bin = hex_to_bin(Hex),
                          Hashed = multihash:hash(Bin, HashKey),
                          {ok, _, HashKey, Code} = multihash:decode(Hashed)
                  end, proplists:get_value(input, Config)),
    ok.

encode_test(Config) ->
    lists:foreach(fun({HashKey, {Code, Hex}}) ->
                          Bin = hex_to_bin(Hex),
                          Hashed = multihash:encode(Bin, Code),
                          {ok, Bin, HashKey, Code} = multihash:decode(Hashed)
                  end, proplists:get_value(input, Config)),
    ok.

digest_fail_digest_test(Config) ->
    {id, {?ID, Hex}} = hd(proplists:get_value(input, Config)),
    Bin = hex_to_bin(Hex),
    {error, digest_length} = multihash:decode(Bin),
    ok.

digest_fail_code_test(Config) ->
    {id, {?ID, Hex}} = hd(proplists:get_value(input, Config)),
    Bin = hex_to_bin(Hex),
    Encoded = multihash:encode(Bin, 16#FF),
    {error, {unknown_hash_code, _}} = multihash:decode(Encoded),
    ok.


%% Utilities
hex_to_bin(S) ->
    hex_to_bin(S, []).
hex_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hex_to_bin(T, [V | Acc]).
