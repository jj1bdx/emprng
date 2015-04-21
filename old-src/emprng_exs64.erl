%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @copyright 2014 Kenji Rikitake
%% @doc Xorshift64star for Erlang
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

-module(emprng_exs64).

-export([
     next/1,
     seed0/0,
     seed/3,
     uniform_s/1,
     uniform_s/2
 ]).

-export_type([
        state/0,
        uint64/0
    ]).

%% @type uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

%% @type state(). Internal state data type for exs64.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-opaque state() :: uint64().

-define(UINT32MASK, 16#ffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% @doc Advance xorshift64star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift64star internal state.

-spec next(state()) -> {uint64(), state()}.

next(R) ->
    R1 = R bxor (R bsr 12),
    R2 = R1 bxor ((R1 bsl 25) band ?UINT64MASK),
    R3 = R2 bxor (R2 bsr 27),
    {(R3 * 2685821657736338717) band ?UINT64MASK, R3}.

-spec seed0() -> state().

%% @doc Set the default seed value to xorshift64star state
%% in the process directory (Compatible with random:seed0/0).

seed0() -> 1234567890123456789.

%% @doc Set the seed value to xorshift64star state in the process directory
%% with the given three unsigned 32-bit integer arguments
%% (Compatible with random:seed/3).
%% Multiplicands here: three 32-bit primes

-spec seed(integer(), integer(), integer()) -> 'undefined' | state().

seed(A1, A2, A3) ->
    {V1, _} = next(((A1 band ?UINT32MASK) * 4294967197 + 1)),
    {V2, _} = next(((A2 band ?UINT32MASK) * 4294967231 + 1)),
    {V3, _} = next(((A3 band ?UINT32MASK) * 4294967279 + 1)),
    ((V1 * V2 * V3) rem (?UINT64MASK - 1)) + 1.

%% @doc Generate float from
%% given xorshift64star internal state.
%% (Note: 0.0 &lt; result &lt; 1.0)
%% (Compatible with random:uniform_s/1)

-spec uniform_s(state()) -> {float(), state()}.

uniform_s(R0) ->
    {V, R1} = next(R0),
    {V / 18446744073709551616.0, R1}.

%% @doc Generate integer from given xorshift64star internal state.
%% (Note: 0 =&lt; result &lt; MAX (given positive integer))
-spec uniform_s(pos_integer(), state()) -> {pos_integer(), state()}.

uniform_s(Max, R) when is_integer(Max), Max >= 1 ->
    {V, R1} = next(R),
    {(V rem Max) + 1, R1}.

