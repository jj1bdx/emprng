#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

-define(LOOP_JUMP, 1000).

gen_jump(N, Max, Acc) when N > 0 ->
    _ = rand:uniform(Max),
    _ = rand:jump(),
    Random = rand:uniform(Max),
    case N rem (?LOOP_JUMP div 100) of
    0 -> gen_jump(N-1, Max, [Random|Acc]);
    _ -> gen_jump(N-1, Max, Acc)
    end;
gen_jump(_, _, Acc) -> lists:reverse(Acc).

gen_jump(Algo) ->
    {Seedmap=#{}, _} = case Algo of
           exsplus -> %% Printed with orig 'C' code and this seed
           rand:seed({exsplus, [12345678|12345678]});
           % exs64 -> %% Printed with orig 'C' code and this seed
           % rand:seed_s({exs64, 12345678});
           exs1024 -> %% Printed with orig 'C' code and this seed
           rand:seed({exs1024, {lists:duplicate(16, 12345678), []}});
           _ ->
           rand:seed(Algo, {100, 200, 300})
       end,
    Max = maps:get(max, Seedmap),
    gen_jump(?LOOP_JUMP, Max, []).

main(_) ->
    Testval = gen_jump(exsplus),
    io:format("~p~n", [Testval]).
