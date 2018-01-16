[![Build Status](https://travis-ci.org/helium/erlang-multihash.svg?branch=master)](https://travis-ci.org/helium/erlang-multihash)
[![Coverage Status](https://coveralls.io/repos/github/helium/erlang-multihash/badge.svg?branch=master)](https://coveralls.io/github/helium/erlang-multihash?branch=master)

# erlang-multihash

An Erlang implementation for [nultihash](https://github.com/multiformats/multihash). 

## Usage

An example using the blake2b hash:

```
Hash = multihash:hash(<<"hello world">>, {blake2b, 64}).
```

Decoding a multihash get the digest and hash used:

```
{ok, Digest, {blake2b, 64}, _} = multihash:decode(Hash).
```

