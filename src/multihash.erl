-module(multihash).

-export([decode/1, encode/2, hash/2]).

-type code() :: non_neg_integer().
-type hash_type() :: hash() | {len_hash(), pos_integer()}.
-type hash() :: sha256
              | sha256_dbl
              | sha512
              | sha3_224
              | sha3_256
              | sha3_384
              | sha3_512
              | shake128
              | shake256.
-type len_hash() :: blake2b.


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

-define(IS_BLAKE2B(C), C >= ?BLAKE2B_MIN andalso C =< ?BLAKE2B_MAX).
-define(BLAKE2B_BYTES(C), (C - ?BLAKE2B_MIN + 1)).

-spec decode(binary()) -> {ok, binary(), hash_type(), code()} | {error, term()}.
decode(Bin) ->
    {Code, Tail} = small_ints:decode_varint(Bin),
    {Length, Digest} = small_ints:decode_varint(Tail),
    case Length == byte_size(Digest) of
        false -> {error, digest_length};
        true ->
            case code_info(Code) of
                {error, Error} -> {error, Error};
                {ok, CodeInfo} -> {ok, Digest, CodeInfo, Code}
            end
    end.

-spec encode(binary(), code()) -> binary().
encode(Bin, Code) ->
    BinCode = small_ints:encode_varint(Code),
    BinLen = small_ints:encode_varint(byte_size(Bin)),
    <<BinCode/binary, BinLen/binary, Bin/binary>>.


-spec hash(binary(), hash_type()) -> binary().
hash(Bin, id) ->
    encode(Bin, ?ID);
hash(Bin, sha256) ->
    encode(crypto:hash(sha256, Bin), ?SHA2_256);
hash(Bin, sha512) ->
    encode(crypto:hash(sha512, Bin), ?SHA2_512);
hash(Bin, sha3_224) ->
    encode(keccakf1600:hash(sha3_224, Bin), ?SHA3_224);
hash(Bin, sha3_256) ->
    encode(keccakf1600:hash(sha3_256, Bin), ?SHA3_256);
hash(Bin, sha3_384) ->
    encode(keccakf1600:hash(sha3_384, Bin), ?SHA3_384);
hash(Bin, sha3_512) ->
    encode(keccakf1600:hash(sha3_512, Bin), ?SHA3_512);
hash(Bin, sha256_dbl) ->
    encode(crypto:hash(sha256, crypto:hash(sha256, Bin)), ?SHA2_256_DBL);
hash(Bin, shake128)  ->
    encode(keccakf1600:hash(shake128, Bin, 32), ?SHAKE_128);
hash(Bin, shake256) ->
    encode(keccakf1600:hash(shake256, Bin, 64), ?SHAKE_256);
hash(Bin, {blake2b, Length}) when Length =< ?BLAKE2B_BYTES(?BLAKE2B_MAX) ->
    encode(aeu_blake2b:blake2b(Bin, <<>>, Length), ?BLAKE2B_MIN + Length - 1).


-spec code_info(code()) -> {ok, hash_type()} | {error, term()}.
code_info(Code) ->
    case Code of
	?ID -> {ok, id};
	?SHA2_256 -> {ok, sha256};
	?SHA2_512 -> {ok, sha512};
	?SHA3_224 -> {ok, sha3_224};
	?SHA3_256 -> {ok, sha3_256};
	?SHA3_384 -> {ok, sha3_384};
	?SHA3_512 -> {ok, sha3_512};
	?SHA2_256_DBL -> {ok, sha256_dbl};
	?SHAKE_128 -> {ok, shake128};
	?SHAKE_256 -> {ok, shake256};
        Blake when ?IS_BLAKE2B(Blake) ->
            {ok, {blake2b, ?BLAKE2B_BYTES(Blake)}};
        Unknown -> {error, {unknown_hash_code, Unknown}}
    end.

