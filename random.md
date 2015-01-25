# new random module

Random number generator (RNG), which accepts multiple algorithm handlers.

## Data Types

```erlang
% Definition of algorithm handler
-record(alg, {seed0, seed, uniform, uniform_n}).

% This depends on the algorithm handler module
-type alg_state() :: any().
% This is the record of functions in the algorithm handler module
-type alg_handler() :: #alg{}.
% Internal state
-type state() :: {alg_handler(), alg_state()}.
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
mk_alg(as183) ->  %% DEFAULT_ALG_HANDLER
    #alg{seed0=fun as183_seed0/0, seed=fun as183_seed/1,
     uniform=fun as183_uniform/1, uniform_n=fun as183_uniform/2};

-define(PRIME1, 30269).
-define(PRIME2, 30307).
-define(PRIME3, 30323).

%%-----------------------------------------------------------------------
%% The type of the state

%-type ran() :: {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

%% seed0: initial PRNG seed

as183_seed0() ->
    {3172, 9814, 20125}.

%% seed: seeding with three Integers

as183_seed({A1, A2, A3}) ->
    {(abs(A1) rem (?PRIME1-1)) + 1,   % Avoid seed numbers that are
     (abs(A2) rem (?PRIME2-1)) + 1,   % even divisors of the
     (abs(A3) rem (?PRIME3-1)) + 1}.  % corresponding primes.

%% {uniform_s, State} -> {F, NewState}:
%%  Returns a random float between 0 and 1, and new state.

as183_uniform({A1, A2, A3}) ->
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    R = B1/?PRIME1 + B2/?PRIME2 + B3/?PRIME3,
    {R - trunc(R), {B1,B2,B3}}.

%% {uniform_s, N, State} -> {I, NewState}
%%  Given an integer N >= 1, returns a random integer between 1 and N.

as183_uniform(N, State0) ->
    {F, State1} = as183_uniform(State0),
    {trunc(F * N) + 1, State1}.
```

## Acknowledgments

Thanks to Dan Gudmundsson @dgud for the algorithm handler proposals and implementations.

How the algorithm handler changes:

* in emprng: module
* first in random: one-for-all function
* @dgud's first implementation: case statement
* @dgud's second implementation: record of functions (current)

[More to come]
