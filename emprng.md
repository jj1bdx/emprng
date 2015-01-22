# emprng

Random number generator (RNG), which accepts multiple algorithm handlers.

## Data Types

```erlang
% This depends on the algorithm handler module
-type emprng_alg_state() :: any().
% This is the name of the algorithm handler module
-type emprng_alg_handler() :: atom().
% Internal state
-type emprng_state() :: {emprng_alg_handler(), emprng_alg_state()}.
```    

## Possible alrogithm handlers

* `emprng_as183`: AS183 (compatible with `random` module)

## Default algorithm handler

The default algorithm handler is `emprng_as183`.

## Name of the process dictionary keyword

`emprng_seed` is used as the process dictionary keyword to store and retrieve the seed.

## Exports

See `emprng.erl`.

[More to come]
