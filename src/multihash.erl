-module(multihash).

-export([digest/2, hash/1, code/1]).

-include("multihash.hrl").

-type code() :: non_neg_integer().

-type hash() ::
    identity |
    sha1 |
    sha2_256 |
    sha2_512 |
    sha3_224 |
    sha3_256 |
    sha3_384 |
    sha3_512 |
    keccak224 |
    keccak256 |
    keccak384 |
    keccak512 |
    blake2b256 |
    blake2b512 |
    blake2s128 |
    blake2s256.

-export_types([hash/0, code/0]).

-spec digest(binary(), hash() | code()) -> {ok, binary()} | {error, term()}.
digest(Bin, Hash) when is_integer(Hash) ->
    multihash_nif:digest(Bin, Hash);
digest(Bin, Hash) when is_atom(Hash) ->
    digest(Bin, hash_to_code(Hash)).

-spec hash(binary()) -> {ok, hash()} | {error, term()}.
hash(Bin) ->
    case code(Bin) of
        {ok, Code} -> {ok, code_to_hash(Code)};
        {error, _} = Error -> Error
    end.

-spec code(binary()) -> {ok, non_neg_integer()} | {error, term()}.
code(Bin) ->
    multihash_nif:code(Bin).

hash_to_code(identity) -> ?identity;
hash_to_code(sha1) -> ?sha1;
hash_to_code(sha2_256) -> ?sha2_256;
hash_to_code(sha2_512) -> ?sha2_512;
hash_to_code(sha3_224) -> ?sha3_224;
hash_to_code(sha3_256) -> ?sha3_256;
hash_to_code(sha3_384) -> ?sha3_384;
hash_to_code(sha3_512) -> ?sha3_512;
hash_to_code(keccak224) -> ?keccak224;
hash_to_code(keccak256) -> ?keccak256;
hash_to_code(keccak384) -> ?keccak384;
hash_to_code(keccak512) -> ?keccak512;
hash_to_code(blake2b256) -> ?blake2b256;
hash_to_code(blake2b512) -> ?blake2b512;
hash_to_code(blake2s128) -> ?blake2s128;
hash_to_code(blake2s256) -> ?blake2s256;
hash_to_code(_) -> error(badarg).

code_to_hash(?identity) -> identity;
code_to_hash(?sha1) -> sha1;
code_to_hash(?sha2_256) -> sha2_256;
code_to_hash(?sha2_512) -> sha2_512;
code_to_hash(?sha3_224) -> sha3_224;
code_to_hash(?sha3_256) -> sha3_256;
code_to_hash(?sha3_384) -> sha3_384;
code_to_hash(?sha3_512) -> sha3_512;
code_to_hash(?keccak224) -> keccak224;
code_to_hash(?keccak256) -> keccak256;
code_to_hash(?keccak384) -> keccak384;
code_to_hash(?keccak512) -> keccak512;
code_to_hash(?blake2b256) -> blake2b256;
code_to_hash(?blake2b512) -> blake2b512;
code_to_hash(?blake2s128) -> blake2s128;
code_to_hash(?blake2s256) -> blake2s256;
code_to_hash(_) -> error(invalid_code).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hash_test() ->
    Bin = <<"foo">>,
    Hashes = [
        identity,
        sha1,
        sha2_256,
        sha2_512,
        sha3_224,
        sha3_256,
        sha3_384,
        sha3_512,
        keccak224,
        keccak256,
        keccak384,
        keccak512,
        blake2b256,
        blake2b512,
        blake2s128,
        blake2s256
    ],
    lists:foreach(
        fun(Hash) ->
            {ok, Digest} = digest(Bin, Hash),
            ?assertEqual({ok, Hash}, hash(Digest))
        end,
        Hashes
    ),
    ok.

invalid_hash_test() ->
    ?assertEqual({error, invalid_code}, digest(<<"foo">>, 16#FF)),
    ?assertError(badarg, digest(<<"foo">>, unknown)),

    ?assertEqual({error, invalid_code}, code(<<"foo">>)),
    ?assertEqual({error, invalid_code}, hash(<<"foo">>)),

    ok.

-endif.
