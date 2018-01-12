[![Build Status](https://travis-ci.org/helium/erlang-multihash.svg?branch=master)](https://travis-ci.org/helium/erlang-multihash)
[![Coverage Status](https://coveralls.io/repos/github/helium/erlang-multihash/badge.svg?branch=master)](https://coveralls.io/github/helium/erlang-multihash?branch=master)

# erlang-multihash

An Lang implementation for [nultihash](https://github.com/multiformats/multihash). 

## Usage

```
multiaddr:new("/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21")
```

The encoder is idempotent:

```
Str = "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21",
Str = multiaddr:to_string(multiaddr:new("/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21")).
```

