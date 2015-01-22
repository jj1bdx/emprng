%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @copyright 2014 Kenji Rikitake
%% @doc Xorshift1024star for Erlang
%% @end
%% (MIT License)
%%
%% Copyright (c) 2014 Kenji Rikitake. All rights reserved.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of
%% this software and associated documentation files (the "Software"), to deal in
%% the Software without restriction, including without limitation the rights to
%% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%

-module(emprng_exs1024).

-export([
     next/1,
     seed0/0,
     seed/3,
     uniform_s/1,
     uniform_s/2
 ]).

-export_type([
        seedval/0,
        state/0,
        uint64/0
    ]).

%% @type uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

%% @type seedval(). Internal seed data type for exs1024.
%% Representing 16 64-bit numbers with a single list.

-opaque seedval() :: list(uint64()).

%% @type state(). Internal state data type for exs1024.
%% Representing 16 64-bit numbers with a pair of 
%% the list and a reverse list.

-opaque state() :: {list(uint64()), list(uint64())}.

-define(UINT21MASK, 16#1fffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% @doc Calculation of xorshift1024star.
%% calc(S0, S1) -> {X, NS1}.
%% X: random number output

-spec calc(uint64(), uint64()) -> {uint64(), uint64()}.

calc(S0, S1) ->
    S11 = S1 bxor ((S1 bsl 31) band ?UINT64MASK),
    S12 = S11 bxor (S11 bsr 11),
    S01 = S0 bxor (S0 bsr 30),
    NS1 = S01 bxor S12,
    {(NS1 * 1181783497276652981) band ?UINT64MASK, NS1}. 

%% @doc Advance xorshift1024star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift1024star internal state.

-spec next(state()) -> {uint64(), state()}.

next({[H], RL}) ->
    next({[H|lists:reverse(RL)], []});
next({L, RL}) ->
    [S0|L2] = L,
    [S1|L3] = L2,
    {X, NS1} = calc(S0, S1),
    {X, {[NS1|L3], [S0|RL]}}.

-spec seed0() -> state().

%% @doc Set the default seed value to xorshift1024star state
%% in the process directory (Compatible with random:seed0/0).

seed0() ->
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
     ], []}.

%% @doc Generate a list of 16 64-bit element list
%% of the xorshift64star random sequence
%% from a given 64-bit seed.
%% Note: emprng_exs64:next/1 required.

-spec gen1024(uint64()) -> list(uint64()).

gen1024(R) ->
        gen1024(16, R, []).

-spec gen1024(non_neg_integer(), uint64(), list(uint64())) ->
        list(uint64()).

gen1024(0, _, L) ->
    L;
gen1024(N, R, L) ->
    {X, R2} = emprng_exs64:next(R),
    gen1024(N - 1, R2, [X|L]).

%% @doc Set the seed value to xorshift1024star state in the process directory
%% with the given three unsigned 21-bit integer arguments
%% (Compatible with random:seed/3).
%% Multiplicands here: three 21-bit primes.
%% TODO: this seeding isn't complete yet.

-spec seed(integer(), integer(), integer()) -> 'undefined' | state().

seed(A1, A2, A3) ->
    B1 = (((A1 band ?UINT21MASK) + 1) * 2097131) band ?UINT21MASK,
    B2 = (((A2 band ?UINT21MASK) + 1) * 2097133) band ?UINT21MASK,
    B3 = (((A3 band ?UINT21MASK) + 1) * 2097143) band ?UINT21MASK,
    {gen1024((B1 bsl 43) bor (B2 bsl 22) bor (B3 bsl 1) bor 1), []}.

%% @doc Generate float from
%% given xorshift1024star internal state.
%% (Note: 0.0 =&lt; result &lt; 1.0)
%% (Compatible with random:uniform_s/1)

-spec uniform_s(state()) -> {float(), state()}.

uniform_s(R0) ->
    {V, R1} = next(R0),
    {V / 18446744073709551616.0, R1}.

%% @doc Generate integer from given xorshift1024star internal state.
%% (Note: 0 =&lt; result &lt; MAX (given positive integer))
-spec uniform_s(pos_integer(), state()) -> {pos_integer(), state()}.

uniform_s(Max, R) when is_integer(Max), Max >= 1 ->
    {V, R1} = next(R),
    {(V rem Max) + 1, R1}.
