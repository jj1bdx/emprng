%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2014. All Rights Reserved.
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
         random_as183/1]).

-define(DEFAULT_ALG_HANDLER, fun random_as183/1).
-define(SEED_DICT, random_seed).

%% =====================================================================
%% Types
%% =====================================================================

%% This depends on the algorithm handler function
-type random_alg_state() :: any().
%% This is the algorithm handler function within this module
-type random_alg_handler() :: function().
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
        (name) -> atom;
        (seed0) -> ran();
        ({seed, A1, A2, A3})-> ran() when
                A1 :: integer(), A2 :: integer(), A3 :: integer();
        ({uniform_s, State0}) -> {float(), State1} when
                State0 :: ran(), State1 :: ran();
        ({uniform_s, N, State0}) -> {integer(), State1} when
                N:: pos_integer(), State0 :: ran(), State1 :: ran().

%% name: return name of the function

random_as183(name) ->
    random_as183;

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

