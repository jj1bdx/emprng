# new random module

Random number generator (RNG), which accepts multiple algorithm handlers.

## Data Types

```erlang
% This depends on the algorithm handler module
-type random_alg_state() :: any().
% This is the function of the algorithm handler module
-type random_alg_handler() :: fun().
% Internal state
-type random_state() :: {random_alg_handler(), random_alg_state()}.
```    

## Available alrogithm handler function in this module

* `random_as183`: AS183 (compatible with the old OTP `random` module)
* `random_exs64`: [exs64](https://github.com/jj1bdx/exs64/)
* `random_exsplus`: [exsplus](https://github.com/jj1bdx/exsplus/)
* `random_exs1024`: [exs1024](https://github.com/jj1bdx/exs1024/)
* `random_sfmt`: [sfmt-erlang](https://github.com/jj1bdx/sfmt-erlang/)
* `random_tinymt`: [tinymt-erlang](https://github.com/jj1bdx/tinymt-erlang/)

## Default algorithm handler function

The default algorithm handler function is `random:random_as183/1`.

## Name of the process dictionary keyword

`random_seed` is used as the process dictionary keyword to store and retrieve the seed.

## Exports

See `random.erl`.

## API required for the algorithm handler functions

```erlang
-spec random_handler
        (seed0) -> random_handler_state();
        ({seed, A1, A2, A3})-> random_handler_state() when
                A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
                State0 :: random_handler_state(),
                State1 :: random_handler_state();
        ({uniform_s, N, State0}) -> {integer(), State1} when
                N:: pos_integer(),
                State0 :: random_handler_state(),
                State1 :: random_handler_state().

%% seed0: initial PRNG seed

%% seed: seeding with three Integers

%% {uniform_s, State} -> {F, NewState}:
%% Returns a random float X where 0.0 < X < 1.0, and new state.

%% {uniform_s, N, State} -> {I, NewState}
%% Given an integer N >= 1,
%% returns a random integer X where 1 =< X =< N, and new state.

```

[More to come]
