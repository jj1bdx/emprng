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

## API functions required for the algorithm handlers

```erlang
%% seed0/0: return the default state

-spec seed0() -> internal_state().

%% seed/3: seed random number generation, with three integers

-spec seed(A1, A2, A3) -> ran() when
      A1 :: integer(),
      A2 :: integer(),
      A3 :: integer().

%% uniform_s/1: returns a random float X where 0.0 < X < 1.0,
%% with the given internal state, and returns X with the new state.

-spec uniform_s(State0) -> {float(), State1} when
      State0 :: internal_state(),
      State1 :: internal_state().

%% uniform_s/2: given an integer N >= 1,
%% this function returns a random integer X where 1 =< X =< N,
%% with the given internal state, and returns X with the new state.

-spec uniform_s(N, State0) -> {integer(), State1} when
      N :: pos_integer(),
      State0 :: internal_state(),
      State1 :: internal_state().
```

[More to come]
