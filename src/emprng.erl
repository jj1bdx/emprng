%% Copyright 2015 Kenji Rikitake.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emprng).

-export([seed/0, seed/1, seed/2, seed/3, seed0/0, seed0/1,
         uniform/0, uniform/1, uniform_s/1, uniform_s/2]).

-define(DEFAULT_ALG_HANDLER, emprng_as183).
-define(SEED_DICT, emprng_seed).

%% Types

%% This depends on the algorithm handler module
-type emprng_alg_state() :: any().
%% This is the name of the algorithm handler module
-type emprng_alg_handler() :: atom().
%% Internal state
-type emprng_state() :: {emprng_alg_handler(), emprng_alg_state()}.

%%% Note: if a process calls uniform/0 or uniform/1 without setting a seed first,
%%%       seed/0 is called automatically.
%%% (compatible with the random module)

%% seed0/0: returns the default state, including the state values
%% and the algorithm handler.
%% (compatible with the random module)

-spec seed0() -> emprng_state().

seed0() ->
    seed0(?DEFAULT_ALG_HANDLER).

%% seed0/1: returns the default state for the given algorithm handler.
%% (new function)

-spec seed0(emprng_alg_handler()) -> emprng_state().

seed0(Alg) ->
    {Alg, Alg:seed0()}.

%% seed_put/1: internal function to put seed into the process dictionary.

-spec seed_put(emprng_state()) -> undefined | emprng_state().

seed_put(Seed) ->
    put(?SEED_DICT, Seed).

%% seed/0: seeds RNG with default (fixed) state values and the algorithm handler
%% in the process dictionary, and returns the old state.
%% (compatible with the random module)

-spec seed() -> undefined | emprng_state().

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

-spec seed({Alg :: emprng_alg_handler(), AS :: emprng_alg_state()} |
           {A1 :: integer(), A2 :: integer(), A3 :: integer()}) ->
      undefined | emprng_state().

seed({A1, A2, A3}) ->
    seed(A1, A2, A3);
seed({Alg, AS}) ->
    seed(Alg, AS).

%% seed/3: seeds RNG with integer values in the process dictionary,
%% and returns the old state.
%% (compatible with the random module)

-spec seed(A1 :: integer(), A2 :: integer(), A3 :: integer()) ->
      undefined | emprng_state().

seed(A1, A2, A3) ->
    seed_put({?DEFAULT_ALG_HANDLER,
            ?DEFAULT_ALG_HANDLER:seed(A1, A2, A3)}).

%% seed/2: seeds RNG with the algorithm handler and given values
%% in the process dictionary, and returns the old state.
%% Note: the type of the values depends on the algorithm handler.
%% (new function)

-spec seed(Alg :: emprng_alg_handler(), AS :: emprng_alg_state()) ->
      undefined | emprng_state().

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
    {X, AS2} = Alg:uniform_s(AS),
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
    {X, AS2} = Alg:uniform_s(N, AS),
    seed_put({Alg, AS2}),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 < X < 1.0,
%% and a new state.
%% (See OTP random module.)

-spec uniform_s(emprng_state()) -> {float(), NewS :: emprng_state()}.

uniform_s({Alg, AS}) ->
    {X, AS2} = Alg:uniform_s(AS),
    {X, {Alg, AS2}}.

%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-spec uniform_s(N :: pos_integer(), emprng_state()) ->
      {pos_integer(), NewS :: emprng_state()}.

uniform_s(N, {Alg, AS}) when is_integer(N), N >= 1 ->
    {X, AS2} = Alg:uniform_s(N, AS),
    {X, {Alg, AS2}}.

