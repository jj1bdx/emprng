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
%% Copyright (c) 2010-2015 Kenji Rikitake, Kyoto University.
%% Copyright (c) 2006-2015 Mutsuo Saito, Makoto Matsumoto,
%%                         Hiroshima University, The University of Tokyo.
%%
%% Author contact: kenji.rikitake@acm.org
%% =====================================================================

%% NOTE: this module will replace OTP random module
-module(random).

-export([seed/0, seed/1, seed/2, seed/3, seed0/0, seed0/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2,
         random_as183/1, random_exs64/1, random_exsplus/1,
         random_exs1024/1, random_sfmt/1]).

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
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
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
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
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
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
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

%% =====================================================================
%% SIMD-oriented Fast Mersennt Twister (SFMT) PRNG
%% SFMT19937 (period: 2^19937 - 1)
%% Algorithm by Mutsuo Saito and Makoto Matsumoto
%% Reference URL: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/
%% =====================================================================

%% SFMT period parameters
%% details on SFMT-1.3.3 source code
%%
%% Mersenne Exponent. The period of the sequence
%%  is a multiple of 2^MEXP-1.
-define(MEXP, 19937).
%% SFMT generator has an internal state array of 128-bit integers,
%% and N is its size.
%% -define(N, ((?MEXP div 128) + 1)).
-define(N, 156).
%% N32 is the size of internal state array when regarded as an array
%% of 32-bit integers.
%% -define(N32, (?N * 4)).
-define(N32, 624).
%% for init_by_list32/1:
%% LAG =
%% 	if
%% 	    ?N32 >= 623 ->
%% 		11;
%% 	    ?N32 >= 68 ->
%% 		7;
%% 	    ?N32 >= 39 ->
%% 		5;
%% 	    ?N32 ->
%% 		3
%% 	end,
%% MID = (?N32 - LAG) div 2
-define(LAG, 11).
-define(MID, 306).
%% the pick up position of the array.
-define(POS1, 122).
%% the parameter of shift left as four 32-bit registers.
-define(SL1, 18).
%% the parameter of shift left as one 128-bit register.
%% The 128-bit integer is shifted by (SL2 * 8) bits.
-define(SL2, 1).
%% the parameter of shift right as four 32-bit registers.
-define(SR1, 11).
%% the parameter of shift right as one 128-bit register.
%% The 128-bit integer is shifted by (SL2 * 8) bits.
-define(SR2, 1).
%% A bitmask, used in the recursion.  These parameters are introduced
%% to break symmetry of SIMD.
-define(MSK1, 16#dfffffef).
-define(MSK2, 16#ddfecb7f).
-define(MSK3, 16#bffaffff).
-define(MSK4, 16#bffffff6).
%% These definitions are part of a 128-bit period certification vector.
-define(PARITY1, 16#00000001).
-define(PARITY2, 16#00000000).
-define(PARITY3, 16#00000000).
-define(PARITY4, 16#13c9e684).
%% identification string for the algorithm
-define(IDSTR, "SFMT-19937:122-18-1-11-1:dfffffef-ddfecb7f-bffaffff-bffffff6").

%% SFMT calculation masks
-define(BITMASK32, 16#ffffffff).
-define(BITMASK64, 16#ffffffffffffffff).

%% type w128(). Four-element list of 32-bit unsigned integers
%% to represent a 128-bit integer.

-type w128() :: [integer()].

%% type random_sfmt_intstate().
%% N-element list of 128-bit unsigned integers,
%% represented as a four-element list of 32-bit integers.
%% The number of N is 156.
%% Each 128-bit number is represented in little endian,
%% e.g., a 128-bit X = [X0, X1, X2, X3],
%% where represented in programming language C:
%% ```
%% /* begin */
%% union X {
%% 	uint32_t u[4];
%% };
%% /* end */
%% '''
%% And the 128-bit list is a flat concatenation of 128-bit number lists,

-type random_sfmt_intstate() :: [integer()].

%% type ran_sfmt(). N-element list of 128-bit unsigned integers,
%% represented as a list of 32-bit integers. The number of N is 156.

-type ran_sfmt() :: {[integer()], random_sfmt_intstate()}.


%% SIMD 128-bit right shift simulation for little endian SIMD
%% of Shift*8 bits.

-spec random_sfmt_rshift128(w128(), integer()) -> w128().

random_sfmt_rshift128(In, Shift) ->
    [I0, I1, I2, I3] = In,
    TH = (I3 bsl 32) bor (I2),
    TL = (I1 bsl 32) bor (I0),
    OH = (TH bsr (Shift * 8)) band ?BITMASK64,
    OL = (TL bsr (Shift * 8) bor (TH bsl (64 - (Shift * 8))))
	band ?BITMASK64,
    [OL band ?BITMASK32, OL bsr 32,
     OH band ?BITMASK32, OH bsr 32].

%% SIMD 128-bit left shift simulation for little endian SIMD
%% of Shift*8 bits.

-spec random_sfmt_lshift128(w128(), integer()) -> w128().

random_sfmt_lshift128(In, Shift) ->
    [I0, I1, I2, I3] = In,
    TH = (I3 bsl 32) bor (I2),
    TL = (I1 bsl 32) bor (I0),
    OL = (TL bsl (Shift * 8)) band ?BITMASK64,
    OH = (TH bsl (Shift * 8) bor (TL bsr (64 - (Shift * 8))))
	band ?BITMASK64,
    [OL band ?BITMASK32, OL bsr 32,
     OH band ?BITMASK32, OH bsr 32].

%% The recursion formula operation of SFMT.

-spec random_sfmt_do_recursion(w128(), w128(), w128(), w128()) -> w128().

random_sfmt_do_recursion(A, B, C, D) ->
    [A0, A1, A2, A3] = A,
    [B0, B1, B2, B3] = B,
    % [C0, C1, C2, C3] = C,
    [D0, D1, D2, D3] = D,
    [X0, X1, X2, X3] = random_sfmt_lshift128(A, ?SL2),
    [Y0, Y1, Y2, Y3] = random_sfmt_rshift128(C, ?SR2),
    [
     A0 bxor X0 bxor ((B0 bsr ?SR1) band ?MSK1) bxor Y0
        bxor ((D0 bsl ?SL1) band ?BITMASK32),
     A1 bxor X1 bxor ((B1 bsr ?SR1) band ?MSK2) bxor Y1
        bxor ((D1 bsl ?SL1) band ?BITMASK32),
     A2 bxor X2 bxor ((B2 bsr ?SR1) band ?MSK3) bxor Y2
        bxor ((D2 bsl ?SL1) band ?BITMASK32),
     A3 bxor X3 bxor ((B3 bsr ?SR1) band ?MSK4) bxor Y3
        bxor ((D3 bsl ?SL1) band ?BITMASK32)
     ].

-spec random_sfmt_gen_rand_recursion(non_neg_integer(),
    [integer()], [integer()], [integer()],
    [integer()], [integer()], w128(), w128()) -> [integer()].

random_sfmt_gen_rand_recursion(0, Acc, _, _, _, _, _, _) ->
    lists:reverse(Acc);
random_sfmt_gen_rand_recursion(K, Acc, Int, AccInt, [], AccIntP, R, Q) ->
    random_sfmt_gen_rand_recursion(K, Acc, Int, AccInt,
		       lists:reverse(AccIntP),
		       [],
		       R, Q);
random_sfmt_gen_rand_recursion(K, Acc, [], AccInt, IntP, AccIntP, R, Q) ->
    random_sfmt_gen_rand_recursion(K, Acc,
		       lists:reverse(AccInt),
		       [],
		       IntP, AccIntP, R, Q);
random_sfmt_gen_rand_recursion(K, Acc, Int,
		   AccInt, IntP, AccIntP,
		   [R0, R1, R2, R3],
		   [Q0, Q1, Q2, Q3]) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | IntPN ] = IntP,
    [X0, X1, X2, X3] = random_sfmt_do_recursion([A0, A1, A2, A3],
				    [B0, B1, B2, B3],
				    [R0, R1, R2, R3],
				    [Q0, Q1, Q2, Q3]),
    random_sfmt_gen_rand_recursion(K - 4,
		       [X3 | [X2 | [X1 | [X0 | Acc]]]],
		       IntN,
		       [X3 | [X2 | [X1 | [X0 | AccInt]]]],
		       IntPN,
		       [X3 | [X2 | [X1 | [X0 | AccIntP]]]],
		       [Q0, Q1, Q2, Q3],
		       [X0, X1, X2, X3]).

%% filling the internal state array with SFMT PRNG

-spec random_sfmt_gen_rand_all(random_sfmt_intstate()) ->
        random_sfmt_intstate().

random_sfmt_gen_rand_all(Int) ->
    [T3, T2, T1, T0, S3, S2, S1, S0 | _] = lists:reverse(Int),
    random_sfmt_gen_rand_recursion(?N32, [], Int, [],
		       lists:nthtail(?POS1 * 4, Int), [],
		       [S0, S1, S2, S3], [T0, T1, T2, T3]).

random_sfmt_period_modification_rec1(Parity, I) ->
    random_sfmt_period_modification_rec1(0, Parity, I).

random_sfmt_period_modification_rec1(true, _, I) ->
    {I, true};
random_sfmt_period_modification_rec1(32, _, I) ->
    {I, false};
random_sfmt_period_modification_rec1(X, Parity, I) ->
    Work = 1 bsl X,
    case (Work band Parity =/= 0) of
	true ->
	    random_sfmt_period_modification_rec1(true, Parity, I bxor Work);
	false ->
	    random_sfmt_period_modification_rec1(X + 1, Parity, I)
    end.

random_sfmt_period_modification(Int) ->
    [I0, I1, I2, I3 | IR ] = Int,
    {NI0, F0} = random_sfmt_period_modification_rec1(?PARITY1, I0),
    {NI1, F1} = random_sfmt_period_modification_rec1(?PARITY2, I1),
    {NI2, F2} = random_sfmt_period_modification_rec1(?PARITY3, I2),
    {NI3, F3} = random_sfmt_period_modification_rec1(?PARITY4, I3),
    % F[0-3] are true or false
    if
	F0 ->
	    [NI0, I1, I2, I3 | IR];
	F1 ->
	    [I0, NI1, I2, I3 | IR];
	F2 ->
	    [I0, I1, NI2, I3 | IR];
	F3 ->
	    [I0, I1, I2, NI3 | IR];
	true ->
	    Int
    end.

random_sfmt_period_certification(Int) ->
    [I0, I1, I2, I3 | _ ] = Int,
    In0 = (I0 band ?PARITY1) bxor
	(I1 band ?PARITY2) bxor
	(I2 band ?PARITY3) bxor	
	(I3 band ?PARITY4),
    In1 = In0 bxor (In0 bsr 16),
    In2 = In1 bxor (In1 bsr 8),
    In3 = In2 bxor (In2 bsr 4),
    In4 = In3 bxor (In3 bsr 2),
    In5 = In4 bxor (In4 bsr 1),
    Inner = In5 band 1,
    case Inner of
	1 ->
	    Int;
	0 ->
	    random_sfmt_period_modification(Int)
    end.

random_sfmt_func1(X) ->
    ((X bxor (X bsr 27)) * 1664525) band ?BITMASK32.

random_sfmt_func2(X) ->
    ((X bxor (X bsr 27)) * 1566083941) band ?BITMASK32.

random_sfmt_init_gen_rand_rec1(?N32, Acc) ->
    lists:reverse(Acc);
random_sfmt_init_gen_rand_rec1(I, Acc) ->
    [H | _] = Acc,
    random_sfmt_init_gen_rand_rec1(
      I + 1,
      [((1812433253 * (H bxor (H bsr 30))) + I) band ?BITMASK32 | Acc]).

%% @doc generates an internal state from an integer seed

-spec random_sfmt_init_gen_rand(integer()) ->
        random_sfmt_intstate().

random_sfmt_init_gen_rand(Seed) ->
    random_sfmt_period_certification(
        random_sfmt_init_gen_rand_rec1(1, [Seed])).

random_sfmt_init_by_list32_rec1(0, I, _, A) ->
    {I, A};
random_sfmt_init_by_list32_rec1(K, I, [], A) ->
    R = random_sfmt_func1(array:get(I, A) bxor
		  array:get((I + ?MID) rem ?N32, A) bxor
		  array:get((I + ?N32 - 1) rem ?N32, A)),
    A2 = array:set((I + ?MID) rem ?N32,
		   (array:get((I + ?MID) rem ?N32, A) + R) band ?BITMASK32,
		   A),
    R2 = (R + I) band ?BITMASK32,
    A3 = array:set((I + ?MID + ?LAG) rem ?N32,
		 (array:get((I + ?MID + ?LAG) rem ?N32, A2) + R2) band ?BITMASK32,
		 A2),
    A4 = array:set(I, R2, A3),
    I2 = (I + 1) rem ?N32,
    random_sfmt_init_by_list32_rec1(K - 1, I2, [], A4);
random_sfmt_init_by_list32_rec1(K, I, Key, A) ->
    R = random_sfmt_func1(array:get(I, A) bxor
		  array:get((I + ?MID) rem ?N32, A) bxor
		  array:get((I + ?N32 - 1) rem ?N32, A)),
    A2 = array:set((I + ?MID) rem ?N32,
		   (array:get((I + ?MID) rem ?N32, A) + R) band ?BITMASK32,
		   A),
    [H|T] = Key,
    R2 = (R + H + I) band ?BITMASK32,
    A3 = array:set((I + ?MID + ?LAG) rem ?N32,
		   (array:get((I + ?MID + ?LAG) rem ?N32, A2) + R2) band ?BITMASK32,
		   A2),
    A4 = array:set(I, R2, A3),
    I2 = (I + 1) rem ?N32,
    random_sfmt_init_by_list32_rec1(K - 1, I2, T, A4).

random_sfmt_init_by_list32_rec2(0, _, A) ->
    A;
random_sfmt_init_by_list32_rec2(K, I, A) ->
    R = random_sfmt_func2((array:get(I, A) +
		  array:get((I + ?MID) rem ?N32, A) +
		  array:get((I + ?N32 - 1) rem ?N32, A)) band ?BITMASK32),
    A2 = array:set((I + ?MID) rem ?N32,
		   (array:get((I + ?MID) rem ?N32, A) bxor R),
		   A),
    R2 = (R - I) band ?BITMASK32,
    A3 = array:set((I + ?MID + ?LAG) rem ?N32,
		   (array:get((I + ?MID + ?LAG) rem ?N32, A2) bxor R2),
		   A2),
    A4 = array:set(I, R2, A3),
    I2 = (I + 1) rem ?N32,
    random_sfmt_init_by_list32_rec2(K - 1, I2, A4).

%% generates an internal state from a list of 32-bit integers

-spec random_sfmt_init_by_list32([integer()]) ->
    random_sfmt_intstate().

random_sfmt_init_by_list32(Key) ->
    Keylength = length(Key),

    A = array:new(?N32, {default, 16#8b8b8b8b}),

    Count =
	if
	    Keylength + 1 > ?N32 ->
		Keylength + 1;
	    true ->
		?N32
	end,
    R = random_sfmt_func1(array:get(0, A) bxor
		  array:get(?MID, A) bxor
		  array:get(?N32 - 1, A)),
    A2 = array:set(?MID,
		   (array:get(?MID, A) + R) band ?BITMASK32,
		   A),
    R2 = (R + Keylength) band ?BITMASK32,
    A3 = array:set(?MID + ?LAG,
		   (array:get(?MID + ?LAG, A2) + R2) band ?BITMASK32,
		   A2),
    A4 = array:set(0, R2, A3),

    Count1 = Count - 1,
    {I1, A5} = random_sfmt_init_by_list32_rec1(Count1, 1, Key, A4),

    random_sfmt_period_certification(
      array:to_list(random_sfmt_init_by_list32_rec2(?N32, I1, A5))).

%% Note: ran_sfmt() -> {[integer()], random_sfmt_intstate()}

%% generates a 32-bit random number from the given ran_sfmt()

-spec random_sfmt_gen_rand32
        (random_sfmt_intstate()) -> {integer(), ran_sfmt()};
        (ran_sfmt()) -> {integer(), ran_sfmt()}.

random_sfmt_gen_rand32(L) when is_list(L), length(L) =:= ?N32 ->
    % when random_sfmt_intstate() is directly passed
    % note: given random_sfmt_intstate() is
    %       re-initialized by gen_rand_all/1
    L2 = random_sfmt_gen_rand_all(L),
    [H|T] = L2,
    {H, {T, L2}};
random_sfmt_gen_rand32({[], I}) ->
    I2 = random_sfmt_gen_rand_all(I),
    % this operation is random_sfmt_intstate() type dependent
    [H|T] = I2,
    {H, {T, I2}};
random_sfmt_gen_rand32({R, I}) ->
    [H|T] = R,
    {H, {T, I}}.

%% compatible funtions to the random module in stdlib

-spec random_sfmt
        (seed0) -> ran_sfmt();
        ({seed, A1, A2, A3})-> ran_sfmt() when
            A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
            State0 :: ran_sfmt(), State1 :: ran_sfmt();
        ({uniform_s, N, State0}) -> {integer(), State1} when
            N :: pos_integer(),
            State0 :: ran_sfmt(), State1 :: ran_sfmt().

%% seed0: initial PRNG seed
%% Returns the default internal state

random_sfmt(seed0) ->
    I = random_sfmt_init_gen_rand(1234),
    % this operation is random_sfmt_intstate() type dependent
    {I, I};

%% seed: seeding with three Integers
%% Puts the seed computed from the given integer list by init_by_list32/1
%% and puts the internal state into the process dictionary
%% and initializes the random number list with the internal state
%% and returns the old internal state (internal use only)

random_sfmt({seed, A1, A2, A3}) ->
    I = random_sfmt_init_by_list32([
            (A1 + 1) rem 4294967295,
            (A2 + 1) rem 4294967295,
            (A3 + 1) rem 4294967295]),
    % this operation is random_sfmt_intstate() type dependent
    {I, I};

%% With a given state,
%% Returns a uniformly-distributed float random number X
%% where `(X > 0.0)' and `(X < 1.0)'
%% and a new state

random_sfmt({uniform_s, RS}) ->
    {X, NRS} = random_sfmt_gen_rand32(RS),
    {(X + 0.5) * (1.0/4294967296.0), NRS};

%% Returns a uniformly-distributed integer random number X
%% where (X >= 1) and (X =< N)
%% and a new state

random_sfmt({uniform_s, N, RS}) ->
    {X, NRS} = random_sfmt_gen_rand32(RS),
    {trunc(X * (1.0/4294967296.0) * N) + 1, NRS}.

