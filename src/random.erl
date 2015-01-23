%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% =====================================================================
%% Multiple PRNG module for Erlang/OTP
%%
%% Copyright (C) 2014-2015 Kenji Rikitake
%%
%% Author contact: kenji.rikitake@acm.org
%% =====================================================================

%% NOTE: this module will replace OTP random module
-module(random).

-export([seed/0, seed/1, seed/2, seed/3, seed0/0, seed0/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2,
         random_as183/1, random_exs64/1, random_exsplus/1,
         random_exs1024/1]).

-define(DEFAULT_ALG_HANDLER, fun ?MODULE:random_as183/1).
-define(SEED_DICT, random_seed).

%% =====================================================================
%% Types
%% =====================================================================

%% This depends on the algorithm handler function
-type random_alg_state() :: any().
%% This is the algorithm handler function within this module
-type random_alg_handler() :: fun().
%% Internal state
-type random_state() :: {random_alg_handler(), random_alg_state()}.

%% =====================================================================
%% Wrapper functions for the algorithm handlers
%% =====================================================================

%%% Note: if a process calls uniform/0 or uniform/1 without setting a seed first,
%%%       seed/0 is called automatically.
%%% (compatible with the random module)

%% seed0/0: returns the default state, including the state values
%% and the algorithm handler.
%% (compatible with the random module)

-spec seed0() -> random_state().

seed0() ->
    seed0(?DEFAULT_ALG_HANDLER).

%% seed0/1: returns the default state for the given algorithm handler function.
%% usage example: seed0(fun random:random_exs64/1)
%% (new function)

-spec seed0(random_alg_handler()) -> random_state().

seed0(Alg) ->
    {Alg, Alg(seed0)}.

%% seed_put/1: internal function to put seed into the process dictionary.

-spec seed_put(random_state()) -> undefined | random_state().

seed_put(Seed) ->
    put(?SEED_DICT, Seed).

%% seed/0: seeds RNG with default (fixed) state values and the algorithm handler
%% in the process dictionary, and returns the old state.
%% (compatible with the random module)

-spec seed() -> undefined | random_state().

seed() ->
    case seed_put(seed0()) of
        undefined -> seed0();
        % no type checking here
        Old -> Old
    end.

%% seed/1:
%% seed({A1, A2, A3}) is equivalent to seed(A1, A2, A3), and
%% Seed({Alg, AS}) is equivalent to seed(Alg, AS).
%% (the 3-element tuple argument is compatible with the random module,
%% and the 2-element tuple argument is a new function.)

-spec seed({Alg :: random_alg_handler(), AS :: random_alg_state()} |
           {A1 :: integer(), A2 :: integer(), A3 :: integer()}) ->
      undefined | random_state().

seed({A1, A2, A3}) ->
    seed(A1, A2, A3);
seed({Alg, AS}) ->
    seed(Alg, AS).

%% seed/3: seeds RNG with integer values in the process dictionary,
%% and returns the old state.
%% (compatible with the random module)

-spec seed(A1 :: integer(), A2 :: integer(), A3 :: integer()) ->
      undefined | random_state().

seed(A1, A2, A3) ->
    seed_put({?DEFAULT_ALG_HANDLER,
            ?DEFAULT_ALG_HANDLER({seed, A1, A2, A3})}).

%% seed/2: seeds RNG with the algorithm handler and given values
%% in the process dictionary, and returns the old state.
%% Note: the type of the values depends on the algorithm handler.
%% (new function)

-spec seed(Alg :: random_alg_handler(), AS :: random_alg_state()) ->
      undefined | random_state().

seed(Alg, AS) ->
    % No type checking on AS
    seed_put({Alg, AS}).

%%% uniform/0, uniform/1, uniform_s/1, uniform_s/2 are all
%%% uniformly distributed random numbers.
%%% (They are expected to be semantically compatible with OTP random module,
%%%  i.e., the uniformity and return value ranges will not change.)

%% uniform/0: returns a random float X where 0.0 < X < 1.0,
%% updating the state in the process dictionary.
%% (See OTP random module.)

-spec uniform() -> float().

uniform() ->
    {Alg, AS} = case get(?SEED_DICT) of
                    undefined -> seed0();
                    % No type checking here
                    Old -> Old
               end,
    {X, AS2} = Alg({uniform_s, AS}),
    seed_put({Alg, AS2}),
    X.

%% uniform/1: given an integer N >= 1,
%% uniform/1 returns a random integer X where 1 =< X =< N,
%% updating the state in the process dictionary.

-spec uniform(N :: pos_integer()) -> pos_integer().

uniform(N) when is_integer(N), N >= 1 ->
    {Alg, AS} = case get(?SEED_DICT) of
                    undefined -> seed0();
                    % No type checking here
                    Old -> Old
               end,
    {X, AS2} = Alg({uniform_s, N, AS}),
    seed_put({Alg, AS2}),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 < X < 1.0,
%% and a new state.
%% (See OTP random module.)

-spec uniform_s(random_state()) -> {float(), NewS :: random_state()}.

uniform_s({Alg, AS}) ->
    {X, AS2} = Alg({uniform_s, AS}),
    {X, {Alg, AS2}}.

%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-spec uniform_s(N :: pos_integer(), random_state()) ->
      {pos_integer(), NewS :: random_state()}.

uniform_s(N, {Alg, AS}) when is_integer(N), N >= 1 ->
    {X, AS2} = Alg({uniform_s, N, AS}),
    {X, {Alg, AS2}}.

%% =====================================================================
%% AS183 PRNG
%% =====================================================================

%% Reasonable random number generator.
%%  The method is attributed to B. A. Wichmann and I. D. Hill
%%  See "An efficient and portable pseudo-random number generator",
%%  Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

-define(PRIME1, 30269).
-define(PRIME2, 30307).
-define(PRIME3, 30323).

%%-----------------------------------------------------------------------
%% The type of the state

-type ran() :: {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

-spec random_as183
        (seed0) -> ran();
        ({seed, A1, A2, A3})-> ran() when
                A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
                State0 :: ran(), State1 :: ran();
        ({uniform_s, N, State0}) -> {integer(), State1} when
                N:: pos_integer(), State0 :: ran(), State1 :: ran().

%% seed0: initial PRNG seed

random_as183(seed0) ->
    {3172, 9814, 20125};

%% seed: seeding with three Integers

random_as183({seed, A1, A2, A3}) ->
    {(abs(A1) rem (?PRIME1-1)) + 1,   % Avoid seed numbers that are
	 (abs(A2) rem (?PRIME2-1)) + 1,   % even divisors of the
     (abs(A3) rem (?PRIME3-1)) + 1};  % corresponding primes.

%% {uniform_s, State} -> {F, NewState}:
%%  Returns a random float between 0 and 1, and new state.

random_as183({uniform_s, {A1, A2, A3}}) ->
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    R = B1/?PRIME1 + B2/?PRIME2 + B3/?PRIME3,
    {R - trunc(R), {B1,B2,B3}};

%% {uniform_s, N, State} -> {I, NewState}
%%  Given an integer N >= 1, returns a random integer between 1 and N.

random_as183({uniform_s, N, State0}) when is_integer(N), N >= 1 ->
    {F, State1} = random_as183({uniform_s, State0}),
    {trunc(F * N) + 1, State1}.

%% =====================================================================
%% exs64 PRNG: Xorshift*64
%% =====================================================================

%% uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

%% random_exs64_state(). Internal state data type for exs64.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-type random_exs64_state() :: uint64().

-define(UINT32MASK, 16#ffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% Advance xorshift64star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift64star internal state.

-spec random_exs64_next(random_exs64_state()) ->
        {uint64(), random_exs64_state()}.

random_exs64_next(R) ->
    R1 = R bxor (R bsr 12),
    R2 = R1 bxor ((R1 bsl 25) band ?UINT64MASK),
    R3 = R2 bxor (R2 bsr 27),
    {(R3 * 2685821657736338717) band ?UINT64MASK, R3}.

%%-----------------------------------------------------------------------

-spec random_exs64
        (seed0) -> random_exs64_state();
        ({seed, A1, A2, A3})-> random_exs64_state() when
                A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
                State0 :: random_exs64_state(),
                State1 :: random_exs64_state();
        ({uniform_s, N, State0}) -> {integer(), State1} when
                N:: pos_integer(),
                State0 :: random_exs64_state(),
                State1 :: random_exs64_state().

%% seed0: initial PRNG seed
%% set the default seed value to xorshift64star state
%% in the process directory.

random_exs64(seed0) -> 1234567890123456789;

%% seed: seeding with three Integers
%% set the seed value to xorshift64star state in the process directory
%% with the given three unsigned 32-bit integer arguments
%% Multiplicands here: three 32-bit primes

random_exs64({seed, A1, A2, A3}) ->
    {V1, _} = random_exs64_next(((A1 band ?UINT32MASK) * 4294967197 + 1)),
    {V2, _} = random_exs64_next(((A2 band ?UINT32MASK) * 4294967231 + 1)),
    {V3, _} = random_exs64_next(((A3 band ?UINT32MASK) * 4294967279 + 1)),
    ((V1 * V2 * V3) rem (?UINT64MASK - 1)) + 1;

%% {uniform_s, State} -> {F, NewState}:
%% Generate float from
%% given xorshift64star internal state.
%% (Note: 0.0 &lt; result &lt; 1.0)
%% (Compatible with random:uniform_s/1)

random_exs64({uniform_s, R0}) ->
    {V, R1} = random_exs64_next(R0),
    {V / 18446744073709551616.0, R1};

%% {uniform_s, N, State} -> {I, NewState}:
%% Generate integer from given xorshift64star internal state.
%% (Note: 0 =&lt; result &lt; MAX (given positive integer))

random_exs64({uniform_s, Max, R}) when is_integer(Max), Max >= 1 ->
    {V, R1} = random_exs64_next(R),
    {(V rem Max) + 1, R1}.

%% =====================================================================
%% exsplus PRNG: Xorshift+128
%% =====================================================================

%% random_exsplus_state(). Internal state data type for exsplus.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-record(random_exsplus_state, {s0 :: uint64(), s1 :: uint64()}).

-type random_exsplus_state() :: #random_exsplus_state{}.

%% Advance xorshift128plus state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift128plus internal state.

-spec random_exsplus_next(random_exsplus_state()) ->
    {uint64(), random_exsplus_state()}.

random_exsplus_next(R) ->
    S1 = R#random_exsplus_state.s0,
    S0 = R#random_exsplus_state.s1,
    S11 = (S1 bxor (S1 bsl 23)) band ?UINT64MASK,
    S12 = S11 bxor S0 bxor (S11 bsr 17) bxor (S0 bsr 26),
    {(S0 + S12) band ?UINT64MASK,
        #random_exsplus_state{s0 = S0, s1 = S12}}.

%%-----------------------------------------------------------------------

-spec random_exsplus
        (seed0) -> random_exsplus_state();
        ({seed, A1, A2, A3})-> random_exsplus_state() when
                A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
                State0 :: random_exsplus_state(),
                State1 :: random_exsplus_state();
        ({uniform_s, N, State0}) -> {integer(), State1} when
                N:: pos_integer(),
                State0 :: random_exsplus_state(),
                State1 :: random_exsplus_state().

%% seed0: initial PRNG seed
%% Set the default seed value to xorshift128plus state
%% in the process directory

random_exsplus(seed0) ->
    #random_exsplus_state{
        s0 = 1234567890123456789, s1 = 9876543210987654321};

%% seed: seeding with three Integers
%% Set the seed value to xorshift128plus state in the process directory
%% with the given three unsigned 32-bit integer arguments
%% Multiplicands here: three 32-bit primes

random_exsplus({seed, A1, A2, A3}) ->
    {_, R1} = random_exsplus_next(
               #random_exsplus_state{
                   s0 = (((A1 * 4294967197) + 1) band ?UINT64MASK),
                   s1 = (((A2 * 4294967231) + 1) band ?UINT64MASK)}),
    {_, R2} = random_exsplus_next(
               #random_exsplus_state{
                   s0 = (((A3 * 4294967279) + 1) band ?UINT64MASK),
                   s1 = R1#random_exsplus_state.s1}),
    R2;

%% {uniform_s, State} -> {F, NewState}:
%% Generate float from
%% given xorshift128plus internal state.
%% (Note: 0.0 =&lt; result &lt; 1.0)

random_exsplus({uniform_s, R0}) ->
    {I, R1} = random_exsplus_next(R0),
    {I / 18446744073709551616.0, R1};

%% {uniform_s, N, State} -> {I, NewState}:
%% Generate integer from given xorshift128plus internal state.
%% (Note: 0 =&lt; result &lt; MAX (given positive integer))

random_exsplus({uniform_s, Max, R}) when is_integer(Max), Max >= 1 ->
    {V, R1} = random_exsplus_next(R),
    {(V rem Max) + 1, R1}.

%% =====================================================================
%% exs1024 PRNG: Xorshift*1024
%% =====================================================================

%% random_exs1024_state(). Internal state data type for exs1024.
%% Representing 16 64-bit numbers with a pair of
%% the list and a reverse list.

-type random_exs1024_state() :: {list(uint64()), list(uint64())}.

%% Calculation of xorshift1024star.
%% random_exs1024_calc(S0, S1) -> {X, NS1}.
%% X: random number output

-spec random_exs1024_calc(uint64(), uint64()) -> {uint64(), uint64()}.

random_exs1024_calc(S0, S1) ->
    S11 = S1 bxor ((S1 bsl 31) band ?UINT64MASK),
    S12 = S11 bxor (S11 bsr 11),
    S01 = S0 bxor (S0 bsr 30),
    NS1 = S01 bxor S12,
    {(NS1 * 1181783497276652981) band ?UINT64MASK, NS1}.

%% Advance xorshift1024star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift1024star internal state.

-spec random_exs1024_next(random_exs1024_state()) ->
        {uint64(), random_exs1024_state()}.

random_exs1024_next({[H], RL}) ->
    random_exs1024_next({[H|lists:reverse(RL)], []});
random_exs1024_next({L, RL}) ->
    [S0|L2] = L,
    [S1|L3] = L2,
    {X, NS1} = random_exs1024_calc(S0, S1),
    {X, {[NS1|L3], [S0|RL]}}.

%% Generate a list of 16 64-bit element list
%% of the xorshift64star random sequence
%% from a given 64-bit seed.
%% Note: dependent on random_exs64_next/1

-spec random_exs1024_gen1024(uint64()) -> list(uint64()).

random_exs1024_gen1024(R) ->
        random_exs1024_gen1024(16, R, []).

-spec random_exs1024_gen1024(
        non_neg_integer(), uint64(), list(uint64())) ->
            list(uint64()).

random_exs1024_gen1024(0, _, L) ->
    L;
random_exs1024_gen1024(N, R, L) ->
    {X, R2} = random_exs64_next(R),
    random_exs1024_gen1024(N - 1, R2, [X|L]).

%%-----------------------------------------------------------------------

-spec random_exs1024
        (seed0) -> random_exs1024_state();
        ({seed, A1, A2, A3})-> random_exs1024_state() when
                A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
                State0 :: random_exs1024_state(),
                State1 :: random_exs1024_state();
        ({uniform_s, N, State0}) -> {integer(), State1} when
                N:: pos_integer(),
                State0 :: random_exs1024_state(),
                State1 :: random_exs1024_state().

-define(UINT21MASK, 16#1fffff).

%% seed0: initial PRNG seed
%% Set the default seed value to xorshift1024star state
%% in the process directory (Compatible with random:seed0/0).

random_exs1024(seed0) ->
    {
     [
      16#0123456789abcdef,
      16#123456789abcdef0,
      16#23456789abcdef01,
      16#3456789abcdef012,
      16#456789abcdef0123,
      16#56789abcdef01234,
      16#6789abcdef012345,
      16#789abcdef0123456,
      16#89abcdef01234567,
      16#9abcdef012345678,
      16#abcdef0123456789,
      16#bcdef0123456789a,
      16#cdef0123456789ab,
      16#def0123456789abc,
      16#ef0123456789abcd,
      16#f0123456789abcde
     ], []};

%% seed: seeding with three Integers
%% Set the seed value to xorshift1024star state in the process directory
%% with the given three unsigned 21-bit integer arguments
%% Multiplicands here: three 21-bit primes.
%% TODO: this seeding has a room to improve.

random_exs1024({seed, A1, A2, A3}) ->
    B1 = (((A1 band ?UINT21MASK) + 1) * 2097131) band ?UINT21MASK,
    B2 = (((A2 band ?UINT21MASK) + 1) * 2097133) band ?UINT21MASK,
    B3 = (((A3 band ?UINT21MASK) + 1) * 2097143) band ?UINT21MASK,
    {random_exs1024_gen1024(
            (B1 bsl 43) bor (B2 bsl 22) bor (B3 bsl 1) bor 1), []};

%% {uniform_s, State} -> {F, NewState}:
%% Generate float from
%% given xorshift1024star internal state.
%% (Note: 0.0 =&lt; result &lt; 1.0)

random_exs1024({uniform_s, R0}) ->
    {V, R1} = random_exs1024_next(R0),
    {V / 18446744073709551616.0, R1};

%% {uniform_s, N, State} -> {I, NewState}:
%% @doc Generate integer from given xorshift1024star internal state.
%% (Note: 0 =&lt; result &lt; MAX (given positive integer))

random_exs1024({uniform_s,Max, R}) when is_integer(Max), Max >= 1 ->
    {V, R1} = random_exs1024_next(R),
    {(V rem Max) + 1, R1}.
