#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    io:format("~s~n", [code:lib_dir(erl_docgen)]).
