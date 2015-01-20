# emprng

Random number generator (RNG), which accepts multiple algorithm handlers.

## Data Types

```erlang
% This depends on the algorithm handler module
-type emprng_seed() :: any().
% This is the module name or atom keyword of the algorithm handler module
-type emprng_alg() :: atom(). 
% Internal state
-type emprng_state() :: {emprng_alg(), emprng_seed()}.
```    

## Possible alrogithm handlers

* `emprng_as183`: AS183 (compatible with `random` module)

## Default algorithm handler

The default algorithm handler is `emprng_as183`.

## Name of the process dictionary keyword

`emprng_seed` is used as the process dictionary keyword to store and retrieve the seed.

## Exports

```erlang
% seed/0: seeds RNG with default (fixed) state values and the algorithm handler
% in the process dictionary, and returns the old state.
% (compatible with the random module)

-spec seed() -> undefined | emprng_state().

% seed/3: seeds RNG with integer values in the process dictionary,
% and returns the old state.
% (compatible with the random module)

-spec seed(A1 :: integer(), A2 :: integer(), A3 :: integer()) ->
      undefined | emprng_state().

% seed/1: seed({A1, A2, A3}) is equivalend to seed(A1, A2, A3).
% (compatible with the random module)

-spec seed({A1 :: integer(), A2 :: integer(), A3 :: integer()}) ->
      undefined | emprng_state().

% seed0/0: returns the default state, including the state values
% and the algorithm handler.
% (compatible with the random module)

-spec seed0() -> emprng_state().

% seed0/1: returns the default state for the given algorithm handler.
% (new function)

-spec seed0(emprng_alg()) -> emprng_state().

% seed/2: seeds RNG with the given values and algorithm handler
% in the process dictionary, and returns the old state.
% Note: the type of the values depends on the algorithm handler.
% (new function)

-spec seed(X :: emprng_seed(), Alg :: emprng_alg()) ->
      undefined | emprng_state().

%%% TBD: uniform/0, uniform/1, uniform_s/1, uniform_s/2
%%% (They are expected to be semantically compatible with the random module,
%%%  i.e., the uniformity and return value ranges will not change)

%%% Note: if a process calls uniform/0 or uniform/1 without setting a seed first,
%%%       seed/0 is called automatically.
%%% (compatible with the random module)
```

[More to come]
