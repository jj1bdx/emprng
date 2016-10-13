#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

-define(LOOP_JUMP, 1000).

gen_jump(N, State0 = {#{max:=Max}, _}, Acc) when N > 0 ->
    {_, State1} = rand:uniform_s(Max, State0),
    {Random, State2} = rand:uniform_s(Max, rand:jump(State1)),
    case N rem (?LOOP_JUMP div 100) of
    0 -> gen_jump(N-1, State2, [Random|Acc]);
    _ -> gen_jump(N-1, State2, Acc)
    end;
gen_jump(_, _, Acc) -> lists:reverse(Acc).

gen_jump(Algo) ->
    Seed = case Algo of
           exsplus -> %% Printed with orig 'C' code and this seed
           rand:seed_s({exsplus, [12345678|12345678]});
           % exs64 -> %% Printed with orig 'C' code and this seed
           % rand:seed_s({exs64, 12345678});
           exs1024 -> %% Printed with orig 'C' code and this seed
           rand:seed_s({exs1024, {lists:duplicate(16, 12345678), []}});
           _ ->
           rand:seed(Algo, {100, 200, 300})
       end,
    gen_jump(?LOOP_JUMP, Seed, []).

main(_) ->
    Testval = gen_jump(exsplus),
    io:format("~p~n", [Testval]).
