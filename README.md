[![Build Status](https://travis-ci.org/helium/erlang-multihash.svg?branch=master)](https://travis-ci.org/helium/erlang-multihash)
[![Coverage Status](https://coveralls.io/repos/github/helium/erlang-multihash/badge.svg?branch=master)](https://coveralls.io/github/helium/erlang-multihash?branch=master)

# erlang-multihash

An Erlang wrapper for [rust_multihash](https://github.com/multiformats/rust-multihash). 

## Usage

An example using the blake2b hash:

```
{ok, Digest} = multihash:digest(<<"hello world">>, blake2b256).
```

Get the hash used for a given multihash digest:

```
{ok, blake2b256} = multihash:hash(Digest).
```

