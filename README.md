![build](https://github.com/helium/erlang-multihash/workflows/Continuous%20Integration/badge.svg)
[![codecov](https://codecov.io/gh/helium/erlang-multihash/branch/master/graph/badge.svg)](https://codecov.io/gh/helium/erlang-multihash)

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

