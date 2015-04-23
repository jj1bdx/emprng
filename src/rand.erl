%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
%%
%% Author contact: kenji.rikitake@acm.org
%% =====================================================================

-module(rand).

-export([seed_s/1, seed_s/2, seed/1, seed/2,
	 export_seed/0, export_seed/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2]).

-compile({inline, [exs64_next/1, exsplus_next/1,
		   exs1024_next/1, exs1024_calc/2]}).

-define(OLD_ALG_HANDLER, as183).
-define(DEFAULT_ALG_HANDLER, exs64).
-define(SEED_DICT, rand_seed).

%% =====================================================================
%% Types
%% =====================================================================

-opaque(state). %% Implementation state

-opaque(alg_seed).  %% Algorithm dependent state

%% This depends on the algorithm handler function
-type alg_seed() :: any().
%% This is the algorithm handler function within this module
-type alg_handler() :: #{type      => alg(),
			 uniform   => fun(),
			 uniform_n => fun()}.

%% Internal state
-type state() :: {alg_handler(), alg_seed()}.

-type alg() :: as183 | exs64 | exsplus | exs1024.

%% export the alg_handler() type
-export_type([alg/0]).

%% =====================================================================
%% API
%% =====================================================================

%% Return algorithm and seed so that RNG state can be recreated with seed/1
-spec export_seed() -> undefined | {alg(), alg_seed()}.
export_seed() ->
    case seed_get() of
	{#{type:=Alg}, Seed} -> {Alg, Seed};
	_ -> undefined
    end.

-spec export_seed(state()) -> {alg(), alg_seed()}.
export_seed({#{type:=Alg}, Seed}) -> {Alg, Seed}.

%% seed(Alg) seeds RNG with runtime dependent values
%% and return the NEW state

%% seed({Alg,Seed}) setup RNG with a previously exported seed
%% and return the NEW state

-spec seed(alg() | {alg(), alg_seed()}) -> state().
seed(Alg) ->
    R = seed_s(Alg),
    _ = seed_put(R),
    R.

-spec seed_s(alg() | {alg(), alg_seed()}) -> state().
seed_s(Alg) when is_atom(Alg) ->
    seed_s(Alg, {erlang:phash2([{node(),self()}]),
		 erlang:monotonic_time(),
		 erlang:time_offset()});
seed_s({Alg0, Seed}) ->
    {Alg,_SeedFun} = mk_alg(Alg0),
    {Alg, Seed}.

%% seed/2: seeds RNG with the algorithm and given values
%% and returns the NEW state.

-spec seed(Alg :: alg(), {integer(), integer(), integer()}) -> state().
seed(Alg0, S0) ->
    State = seed_s(Alg0, S0),
    _ = seed_put(State),
    State.

-spec seed_s(Alg :: alg(), {integer(), integer(), integer()}) -> state().
seed_s(Alg0, S0 = {_, _, _}) ->
    {Alg, Seed} = mk_alg(Alg0),
    AS = Seed(S0),
    {Alg, AS}.

%%% uniform/0, uniform/1, uniform_s/1, uniform_s/2 are all
%%% uniformly distributed random numbers.

%% uniform/0: returns a random float X where 0.0 < X < 1.0,
%% updating the state in the process dictionary.

-spec uniform() -> float().

uniform() ->
    {X, Seed} = uniform_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform/1: given an integer N >= 1,
%% uniform/1 returns a random integer X where 1 =< X =< N,
%% updating the state in the process dictionary.

-spec uniform(N :: pos_integer()) -> pos_integer().
uniform(N) ->
    {X, Seed} = uniform_s(N, seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 < X < 1.0,
%% and a new state.

-spec uniform_s(state()) -> {float(), NewS :: state()}.
uniform_s({Alg = #{uniform:=Uniform}, AS0}) ->
    {X, AS} = Uniform(AS0),
    {X, {Alg, AS}}.

%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-spec uniform_s(N :: pos_integer(), state()) ->
		       {pos_integer(), NewS :: state()}.
uniform_s(N, {Alg = #{uniform_n:=Uniform}, AS0})
  when is_integer(N), N >= 1 ->
    {X, AS} = Uniform(N, AS0),
    {X, {Alg, AS}}.

%% =====================================================================
%% Internal functions

-spec seed_put(state()) -> undefined | state().

seed_put(Seed) ->
    put(?SEED_DICT, Seed).

seed_get() ->
    case get(?SEED_DICT) of
        undefined -> seed(?DEFAULT_ALG_HANDLER);
        Old -> Old  % no type checking here
    end.

%% Setup alg record
mk_alg(as183) ->  %% DEFAULT_ALG_HANDLER
    {#{type=>as183, uniform=>fun as183_uniform/1,
	  uniform_n=>fun as183_uniform/2},
     fun as183_seed/1};
mk_alg(exs64) ->
    {#{type=>exs64, uniform=>fun exs64_uniform/1,
       uniform_n=>fun exs64_uniform/2},
     fun exs64_seed/1};
mk_alg(exsplus) ->
    {#{type=>exsplus, uniform=>fun exsplus_uniform/1,
       uniform_n=>fun exsplus_uniform/2},
     fun exsplus_seed/1};
mk_alg(exs1024) ->
    {#{type=>exs1024, uniform=>fun exs1024_uniform/1,
       uniform_n=>fun exs1024_uniform/2},
     fun exs1024_seed/1}.


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

%-type ran() :: {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

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

%% =====================================================================
%% exs64 PRNG: Xorshift*64
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

%% uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

%% exs64_state(). Internal state data type for exs64.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-type exs64_state() :: uint64().

-define(UINT32MASK, 16#ffffffff).
-define(UINT39MASK, 16#0000007fffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% Advance xorshift64star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift64star internal state.

-spec exs64_next(exs64_state()) ->
        {uint64(), exs64_state()}.

exs64_next(R) ->
    R1 = R bxor (R bsr 12),
    R2 = R1 bxor ((R1 band ?UINT39MASK) bsl 25),
    R3 = R2 bxor (R2 bsr 27),
    {(R3 * 2685821657736338717) band ?UINT64MASK, R3}.

%%-----------------------------------------------------------------------

%% algorithm handler functions

%% Set the seed value to xorshift64star state in the process directory
%% with the given three unsigned 32-bit integer arguments
%% Multiplicands here: three 32-bit primes

exs64_seed({A1, A2, A3}) ->
    {V1, _} = exs64_next(((A1 band ?UINT32MASK) * 4294967197 + 1)),
    {V2, _} = exs64_next(((A2 band ?UINT32MASK) * 4294967231 + 1)),
    {V3, _} = exs64_next(((A3 band ?UINT32MASK) * 4294967279 + 1)),
    ((V1 * V2 * V3) rem (?UINT64MASK - 1)) + 1.

%% Generate float from given xorshift64star internal state.

exs64_uniform(R0) ->
    {V, R1} = exs64_next(R0),
    {V / 18446744073709551616.0, R1}.

%% Generate integer from given xorshift64star internal state.

exs64_uniform(Max, R) ->
    {V, R1} = exs64_next(R),
    {(V rem Max) + 1, R1}.

%% =====================================================================
%% exsplus PRNG: Xorshift+128
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

%% exsplus_state(). Internal state data type for exsplus.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-type exsplus_state() :: [integer()|integer()].

%% Advance xorshift128plus state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift128plus internal state.

-spec exsplus_next(exsplus_state()) ->
    {uint64(), exsplus_state()}.

%% Note: members s0 and s1 are swapped here
exsplus_next([S1|S0]) ->
    S11 = (S1 bxor (S1 bsl 23)) band ?UINT64MASK,
    S12 = S11 bxor S0 bxor (S11 bsr 17) bxor (S0 bsr 26),
    {(S0 + S12) band ?UINT64MASK,
     [S0|S12]}.

%%-----------------------------------------------------------------------

%% algorithm handler functions

%% Set the seed value to xorshift128plus state in the process directory
%% with the given three unsigned 32-bit integer arguments
%% Multiplicands here are three 32-bit primes

exsplus_seed({A1, A2, A3}) ->
    {_, R1} = exsplus_next([(((A1 * 4294967197) + 1) band ?UINT64MASK)|
			    (((A2 * 4294967231) + 1) band ?UINT64MASK)]),
    {_, R2} = exsplus_next([(((A3 * 4294967279) + 1) band ?UINT64MASK)|
			    tl(R1)]),
    R2.

%% Generate float from given xorshift128plus internal state.

exsplus_uniform(R0) ->
    {I, R1} = exsplus_next(R0),
    {I / 18446744073709551616.0, R1}.

%% Generate integer from given xorshift128plus internal state.

exsplus_uniform(Max, R) ->
    {V, R1} = exsplus_next(R),
    {(V rem Max) + 1, R1}.

%% =====================================================================
%% exs1024 PRNG: Xorshift*1024
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================

%% exs1024_state(). Internal state data type for exs1024.
%% Representing 16 64-bit numbers with a pair of
%% the list and a reverse list.

-type exs1024_state() :: {list(uint64()), list(uint64())}.

-define(UINT33MASK, 16#1ffffffff).

%% Calculation of xorshift1024star.
%% exs1024_calc(S0, S1) -> {X, NS1}.
%% X: random number output

-spec exs1024_calc(uint64(), uint64()) -> {uint64(), uint64()}.

exs1024_calc(S0, S1) ->
    S11 = S1 bxor ((S1 band ?UINT33MASK) bsl 31),
    S12 = S11 bxor (S11 bsr 11),
    S01 = S0 bxor (S0 bsr 30),
    NS1 = S01 bxor S12,
    {(NS1 * 1181783497276652981) band ?UINT64MASK, NS1}.

%% Advance xorshift1024star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift1024star internal state.

-spec exs1024_next(exs1024_state()) ->
        {uint64(), exs1024_state()}.

exs1024_next({[S0,S1|L3], RL}) ->
    {X, NS1} = exs1024_calc(S0, S1),
    {X, {[NS1|L3], [S0|RL]}};
exs1024_next({[H], RL}) ->
    exs1024_next({[H|lists:reverse(RL)], []}).

%% Generate a list of 16 64-bit element list
%% of the xorshift64star random sequence
%% from a given 64-bit seed.
%% Note: dependent on exs64_next/1

-spec exs1024_gen1024(uint64()) -> list(uint64()).

exs1024_gen1024(R) ->
        exs1024_gen1024(16, R, []).

-spec exs1024_gen1024(
        non_neg_integer(), uint64(), list(uint64())) ->
            list(uint64()).

exs1024_gen1024(0, _, L) ->
    L;
exs1024_gen1024(N, R, L) ->
    {X, R2} = exs64_next(R),
    exs1024_gen1024(N - 1, R2, [X|L]).

%%-----------------------------------------------------------------------

%% algorithm handler functions

-define(UINT21MASK, 16#1fffff).

%% Set the seed value to xorshift1024star state in the process directory
%% with the given three unsigned 21-bit integer arguments
%% Multiplicands here are three 21-bit primes.
%% TODO: this seeding has a room to improve.

exs1024_seed({A1, A2, A3}) ->
    B1 = (((A1 band ?UINT21MASK) + 1) * 2097131) band ?UINT21MASK,
    B2 = (((A2 band ?UINT21MASK) + 1) * 2097133) band ?UINT21MASK,
    B3 = (((A3 band ?UINT21MASK) + 1) * 2097143) band ?UINT21MASK,
    {exs1024_gen1024(
		(B1 bsl 43) bor (B2 bsl 22) bor (B3 bsl 1) bor 1), []}.

%% Generate float from given xorshift1024star internal state.

exs1024_uniform(R0) ->
    {V, R1} = exs1024_next(R0),
    {V / 18446744073709551616.0, R1}.

%% Generate integer from given xorshift1024star internal state.

exs1024_uniform(Max, R) ->
    {V, R1} = exs1024_next(R),
    {(V rem Max) + 1, R1}.

