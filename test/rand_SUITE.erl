%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

-module(rand_SUITE).
-export([all/0, suite/0,groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2
	]).

-export([interval_int/1, interval_float/1, seed/1,
         api_eq/1, reference/1, basic_stats/1,
	 plugin/1, measure/1
	]).

-export([test/0, gen/1]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(LOOP, 1000000).

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [seed, interval_int, interval_float,
     api_eq,
     reference,
     basic_stats,
     plugin, measure
    ].

groups() -> [].

init_per_suite(Config) ->  Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, Config) ->  Config.

%% A simple helper to test without test_server during dev
test() ->
    Tests = all(),
    lists:foreach(fun(Test) ->
			  try
			      ok = ?MODULE:Test([]),
			      io:format("~p: ok~n", [Test])
			  catch _:Reason ->
				  io:format("Failed: ~p: ~p ~p~n",
					    [Test, Reason, erlang:get_stacktrace()])
			  end
		  end, Tests).

algs() ->
    [exs64, exsplus, exs1024].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seed(doc) ->
    ["Test that seed and seed_s and export_seed/0 is working."];
seed(suite) ->
    [];
seed(Config) when is_list(Config) ->
    Algs = algs(),
    Test = fun(Alg) ->
		   try seed_1(Alg)
		   catch _:Reason ->
			   test_server:fail({Alg, Reason, erlang:get_stacktrace()})
		   end
	   end,
    [Test(Alg) || Alg <- Algs],
    ok.

seed_1(Alg) ->
    %% Check that uniform seeds automatically,
    _ = rand:uniform(),
    S00 = get(rand_seed),
    erase(),
    _ = rand:uniform(),
    false = S00 =:= get(rand_seed), %% hopefully

    %% Choosing algo and seed
    S0 = rand:seed(Alg, {0, 0, 0}),
    %% Check that (documented?) process_dict variable is correct
    S0 = get(rand_seed),
    S0 = rand:seed_s(Alg, {0, 0, 0}),
    %% Check that process_dict should not be used for seed_s functionality
    _ = rand:seed_s(Alg, {1, 0, 0}),
    S0 = get(rand_seed),
    %% Test export
    ES0 = rand:export_seed(),
    ES0 = rand:export_seed_s(S0),
    S0 = rand:seed(ES0),
    S0 = rand:seed_s(ES0),
    %% seed/1 calls should be unique
    S1 = rand:seed(Alg),
    false = (S1 =:= rand:seed_s(Alg)),
    %% Negative integers works
    _ = rand:seed_s(Alg, {-1,-1,-1}),

    %% Other term do not work
    {'EXIT', _} = (catch rand:seed_s(foobar, os:timestamp())),
    {'EXIT', _} = (catch rand:seed_s(Alg, {asd, 1, 1})),
    {'EXIT', _} = (catch rand:seed_s(Alg, {0, 234.1234, 1})),
    {'EXIT', _} = (catch rand:seed_s(Alg, {0, 234, [1, 123, 123]})),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_eq(doc) ->
    ["Check that both api's are consistent with each other."];
api_eq(suite) ->
    [];
api_eq(_Config) ->
    Algs = algs(),
    Small = fun(Alg) ->
		    Seed = rand:seed(Alg),
		    api_eq_1(Seed)
	    end,
    _ = [Small(Alg) || Alg <- Algs],
    ok.

api_eq_1(S00) ->
    Check = fun(_, Seed) ->
		    {V0, S0} = rand:uniform_s(Seed),
		    V0 = rand:uniform(),
		    {V1, S1} = rand:uniform_s(1000000, S0),
		    V1 = rand:uniform(1000000),
		    S1
	    end,
    S1 = lists:foldl(Check, S00, lists:seq(1, 200)),
    S1 = get(rand_seed),
    Exported = rand:export_seed(),
    Exported = rand:export_seed_s(S1),
    {V0, S2} = rand:uniform_s(S1),
    V0 = rand:uniform(),

    S3 = lists:foldl(Check, S2, lists:seq(1, 200)),
    S1 = rand:seed(Exported),
    S1 = rand:seed_s(Exported),

    S4 = lists:foldl(Check, S1, lists:seq(1, 200)),

    %% Verify that we do not have loops
    false = S1 =:= S2,
    false = S2 =:= S3,
    false = S3 =:= S4,

    S1 = rand:seed(Exported),
    S4 = lists:foldl(Check, S1, lists:seq(1, 200)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interval_int(doc) ->
    ["Check that uniform/1 returns values within the proper interval."];
interval_int(suite) ->
    [];
interval_int(Config) when is_list(Config) ->
    Algs = algs(),
    Small = fun(Alg) ->
		    _ = rand:seed(Alg),
		    Max = interval_int_1(100000, 7, 0),
		    Max =:= 7 orelse exit({7, Alg, Max})
	    end,
    _ = [Small(Alg) || Alg <- Algs],
    %% Test large integers
    Large = fun(Alg) ->
		    _ = rand:seed(Alg),
		    Max = interval_int_1(100000, 1 bsl 128, 0),
		    Max > 1 bsl 64 orelse exit({large, Alg, Max})
	    end,
    [Large(Alg) || Alg <- Algs],
    ok.

interval_int_1(0, _, Max) -> Max;
interval_int_1(N, Top, Max) ->
    X = rand:uniform(Top),
    if
	0 < X, X =< Top ->
	    ok;
	true ->
	    io:format("X=~p Top=~p 0<~p<~p~n", [X,Top,X,Top]),
	    exit({X, rand:export_seed()})
    end,
    interval_int_1(N-1, Top, max(X, Max)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interval_float(doc) ->
    ["Check that uniform/0 returns values within the proper interval."];
interval_float(suite) ->
    [];
interval_float(Config) when is_list(Config) ->
    Algs = algs(),
    Test = fun(Alg) ->
		   _ = rand:seed(Alg),
		   interval_float_1(100000)
	   end,
    [Test(Alg) || Alg <- Algs],
    ok.

interval_float_1(0) -> ok;
interval_float_1(N) ->
    X = rand:uniform(),
    if
	0.0 < X, X < 1.0 ->
	    ok;
	true ->
	    io:format("X=~p 0<~p<1.0~n", [X,X]),
	    exit({X, rand:export_seed()})
    end,
    interval_float_1(N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reference(doc) -> ["Check if exs64 algorithm generates the proper sequence."];
reference(suite) -> [];
reference(Config) when is_list(Config) ->
    [reference_1(Alg) || Alg <- algs()],
    ok.

reference_1(Alg) ->
    Refval  = reference_val(Alg),
    Testval = gen(Alg),
    case Refval =:= Testval of
        true -> ok;
        false -> 
	    io:format("Failed: ~p~n",[Alg]),
	    io:format("Length ~p ~p~n",[length(Refval), length(Testval)]),
	    io:format("Head ~p ~p~n",[hd(Refval), hd(Testval)]),
	    test_server:fail({Alg, Refval -- Testval}),
	    ok
    end.

gen(Algo) ->
    Seed = case Algo of
	       exsplus -> %% Printed with orig 'C' code and this seed
		   rand:seed_s({exsplus, [12345678|12345678]});
	       _ ->
		   rand:seed(Algo, {100, 200, 300})
	   end,
    gen(?LOOP, Seed, []).

gen(N, State0, Acc) when N > 0 ->
    {Random, State} = rand:uniform_s((1 bsl 58)-1, State0),
    case N rem (?LOOP div 100) of
	0 -> gen(N-1, State, [Random|Acc]);
	_ -> gen(N-1, State, Acc)
    end;
gen(_, _, Acc) -> lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This just tests the basics so we have not made any serious errors
%% when making the conversion from the original algorithms.
%% The algorithms must have good properties to begin with
%%

basic_stats(doc) -> ["Check that the algorithms generate sound values."];
basic_stats(suite) -> [];
basic_stats(Config) when is_list(Config) ->
    [basic_stats_1(?LOOP, rand:seed_s(Alg), 0.0, array:new([{default, 0}]))
     || Alg <- algs()],
    [basic_stats_2(?LOOP, rand:seed_s(Alg), 0, array:new([{default, 0}]))
     || Alg <- algs()],
    ok.

basic_stats_1(N, S0, Sum, A0) when N > 0 ->
    {X,S} = rand:uniform_s(S0),
    I = trunc(X*100),
    A = array:set(I, 1+array:get(I,A0), A0),
    basic_stats_1(N-1, S, Sum+X, A);
basic_stats_1(0, {#{type:=Alg}, _}, Sum, A) ->
    AverN = Sum / ?LOOP,
    io:format("~.10w: Average: ~.4f~n", [Alg, AverN]),
    Counters = array:to_list(A),
    Min = lists:min(Counters),
    Max = lists:max(Counters),
    io:format("~.10w: Min: ~p Max: ~p~n", [Alg, Min, Max]),

    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(0.5 - AverN) < 0.005 orelse test_server:fail({average, Alg, AverN}),
    abs(?LOOP div 100 - Min) < 1000 orelse test_server:fail({min, Alg, Min}),
    abs(?LOOP div 100 - Max) < 1000 orelse test_server:fail({max, Alg, Max}),
    ok.

basic_stats_2(N, S0, Sum, A0) when N > 0 ->
    {X,S} = rand:uniform_s(100, S0),
    A = array:set(X-1, 1+array:get(X-1,A0), A0),
    basic_stats_2(N-1, S, Sum+X, A);
basic_stats_2(0, {#{type:=Alg}, _}, Sum, A) ->
    AverN = Sum / ?LOOP,
    io:format("~.10w: Average: ~.4f~n", [Alg, AverN]),
    Counters = tl(array:to_list(A)),
    Min = lists:min(Counters),
    Max = lists:max(Counters),
    io:format("~.10w: Min: ~p Max: ~p~n", [Alg, Min, Max]),

    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(50.5 - AverN) < 0.5 orelse test_server:fail({average, Alg, AverN}),
    abs(?LOOP div 100 - Min) < 1000 orelse test_server:fail({min, Alg, Min}),
    abs(?LOOP div 100 - Max) < 1000 orelse test_server:fail({max, Alg, Max}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plugin(doc) ->   ["Test that the user can write algorithms"];
plugin(suite) -> [];
plugin(Config) when is_list(Config) ->
    _ = lists:foldl(fun(_, S0) ->
			    {V1, S1} = rand:uniform_s(10000, S0),
			    true = is_integer(V1),
			    {V2, S2} = rand:uniform_s(S1),
			    true = is_float(V2),
			    S2
		    end, crypto_seed(), lists:seq(1, 200)),
    ok.

%% Test implementation
crypto_seed() ->
    {#{type=>crypto,
       uniform=>fun crypto_uniform/1,
       uniform_n=>fun crypto_uniform_n/2},
     <<>>}.


%% Be fair and create bignums i.e. 64bits otherwise use 58bits
crypto_next(<<Num:64, Bin/binary>>) ->
    {Num, Bin};
crypto_next(_) ->
    crypto_next(crypto:rand_bytes((64 div 8)*100)).

crypto_uniform({Api, Data0}) ->
    {Int, Data} = crypto_next(Data0),
    {Int / (1 bsl 64), {Api, Data}}.

crypto_uniform_n(N, {Api, Data0}) when N < (1 bsl 64) ->
    {Int, Data} = crypto_next(Data0),
    {(Int rem N)+1, {Api, Data}};
crypto_uniform_n(N, State0) ->
    {F,State} = crypto_uniform(State0),
    {trunc(F * N) + 1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Not a test but measures the time characteristics of the different algorithms
measure(Suite) when is_atom(Suite) -> [];
measure(_Config) ->
    Algos = [crypto64|algs()],
    io:format("RNG integer performance~n",[]),
    _ = [measure_1(Algo, fun(State) -> rand:uniform_s(10000, State) end) || Algo <- Algos],
    io:format("RNG float performance~n",[]),
    _ = [measure_1(Algo, fun(State) -> rand:uniform_s(State) end) || Algo <- Algos],
    ok.

measure_1(Algo, Gen) ->
    Parent = self(),
    Seed = fun(crypto64) -> crypto_seed();
	      (Alg) -> rand:seed_s(Alg)
	   end,

    Pid = spawn_link(fun() ->
			     Fun = fun() -> measure_2(?LOOP, Seed(Algo), Gen) end,
			     {Time, ok} = timer:tc(Fun),
			     io:format("~.10w: ~pµs~n", [Algo, Time]),
			     Parent ! {self(), ok},
			     normal
		     end),
    receive
	{Pid, Msg} -> Msg
    end.

measure_2(N, State0, Fun) when N > 0 ->
    case Fun(State0) of
	{Random, State}
	  when is_integer(Random), Random >= 1, Random =< 100000 ->
	    measure_2(N-1, State, Fun);
	{Random, State} when is_float(Random), Random > 0, Random < 1 ->
	    measure_2(N-1, State, Fun);
	Res ->
	    exit({error, Res, State0})
    end;
measure_2(0, _, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data
reference_val(exs64) ->
    [30911212520271731,269553245522723446,195888530915544463,154938545395818505,
     255812719753660711,185216774736022273,192511984483695681,184063017444245074,
     61800926600707076,242313605972755674,17968415385467209,62528817549982181,
     194513424322615536,55575219967591438,154064020497807939,112409446653623047,
     15755012208417139,82357244654143532,3990340598540328,68265675554594379,
     249747458085336568,201449293731880043,223680822347807856,82540371385229731,
     89697665983068394,145446488557708754,140043602690875606,122490547865895346,
     9149268583116531,237192056516417896,85524219877107519,159403927938095577,
     192962499273313475,219473000477329008,219138349780088036,65509927899829118,
     88310927292953020,261200188348922308,127693054166216216,3075948179709149,
     137912890316851135,200690730789582048,168692709240483144,158762886886887960,
     47752962530593436,140119263944937783,76131992461046256,224699573263968162,
     206459583289936795,221499134667655299,178021718720625328,90068038109157427,
     270377724705854177,62586981324170344,52609982503404677,84880640645735069,
     66388325642115647,139454736457974263,138601632726736377,268522678976904091,
     74815178423704771,65911738253136851,190386875363774558,55882347913500420,
     13272573216693610,54247470598850611,5014589752206158,168801529908476430,
     212789816127156214,2004677985360108,53188418111300391,125257116193983249,
     46214672407701086,156706035273774406,169491714258624867,181441694053957480,
     174769250088230419,207166627358721780,201771099110915178,192983135886628429,
     135980219719810044,213941952310595301,284618066293867699,267702975966313672,
     79113833487659106,235706647656126362,175938690098607827,245164557936787717,
     171266500878962373,22952386289541709,120863342166989353,92587941225609684,
     203574619486104516,241767489252455168,34885506432035291,141899923409462676,
     35092119516010965,108488536513796077,240329621081291780,261892338476786353];

reference_val(exs1024) ->
    [78898311962576205,220662707430229960,81235715133479755,206387766307580924,
     83152736994026019,117016170083477493,93740203960358673,206483451029404095,
     25668386032366256,279279484437576970,245141000491753984,62083111856913261,
     199889455533178749,226694716667436150,259336868794040120,151099211083586084,
     175317040747707935,71543409418733636,287984118330703924,104326962425550490,
     36794011111006135,140369437065694071,270283901240701709,187888685552931739,
     189595866819716252,144734355928930047,11254870174838933,93216648194211275,
     74164408936283481,250152663633860974,85111844637268646,116231734693602528,
     18892998813582976,181509499215712372,71541276310774754,180473061878384758,
     260380967356423982,131305221664009023,2096344216176870,269791149086405412,
     219476276033189623,194151478137654466,85338788280485616,88194631575402755,
     179317958063021679,19866804393537066,193032606432531087,108390229360117172,
     236391858352747933,39998191316654952,147541743891537755,179936340522921082,
     244579036128291611,237653345381685195,18338285451254683,233917725924184482,
     2835587998346764,66741774182004938,46934097088072435,167862784776098599,
     230935665769036947,41458926762824811,268347967580105733,242888104698890425,
     32046216884785306,254311307791234754,87908349248188820,234535131788974549,
     152536678406917756,194861077342501148,218124620916108628,207293894022577452,
     257493746691383565,276747845397322589,144455864186928003,186725159573451982,
     267432199114361691,67936820749802629,70746242353598066,169740236405989739,
     68660369555413653,39452870906516907,265112805926880400,276035242371716334,
     41954055015223252,80861626618092829,280421723677071174,118192090749701880,
     277324645107298033,224805959665831624,225712380933704935,142995761359401526,
     114621240042414227,98609661940467863,264766167064381557,40339470040884273,
     189134865133802244,135910624434856837,76848571952187969,112640496269302038];

reference_val(exsplus) ->
    [16#bc76c2e638db, 16#15ede2ebb16c9fb, 16#185ee2c27d6b88d,
     16#15d5ee9feafc3a5, 16#1862e91dfce3e6b, 16#2c9744b0fb69e46,
     16#78b21bc01cef6b, 16#2d16a2fae6c76ba, 16#13dfccb8ff86bce,
     16#1d9474c59e23f4d, 16#d2f67dcd7f0dd6, 16#2b6d489d51a0725,
     16#1fa52ef484861d8, 16#1ae9e2a38f966d4, 16#2264ab1e193acca,
     16#23bbca085039a05, 16#2b6eea06a0af0e1, 16#3ad47fa8866ea20,
     16#1ec2802d612d855, 16#36c1982b134d50, 16#296b6a23f5b75e0,
     16#c5eeb600a9875c, 16#2a3fd51d735f9d4, 16#56fafa3593a070,
     16#13e9d416ec0423e, 16#28101a91b23e9dc, 16#32e561eb55ce15a,
     16#94a7dbba66fe4a, 16#2e1845043bcec1f, 16#235f7513a1b5146,
     16#e37af1bf2d63cb, 16#2048033824a1639, 16#c255c750995f7,
     16#2c7542058e89ee3, 16#204dfeefbdb62ba, 16#f5a936ec63dd66,
     16#33b3b7dbbbd8b90, 16#c4f0f79026ffe9, 16#20ffee2d37aca13,
     16#2274f931716be2c, 16#29b883902ba9df1, 16#1a838cd5312717f,
     16#2edfc49ff3dc1d6, 16#418145cbec84c2, 16#d2d8f1a17d49f,
     16#d41637bfa4cc6f, 16#24437e03a0f5df8, 16#3d1d87919b94a90,
     16#20d6997b36769b6, 16#16f9d7855cd87ca, 16#821ef7e2a062a3,
     16#2c4d11dc4a2da70, 16#24a3b27f56ed26b, 16#144b23c8b97387a,
     16#34a2ced56930d12, 16#21cc0544113a017, 16#3e780771f634fb2,
     16#146c259c02e7e18, 16#1d99e4cfad0ef1, 16#fdf3dabefc6b3a,
     16#7d0806e4d12dfb, 16#3e3ae3580532eae, 16#2456544200fbd86,
     16#f83aad4e88db85, 16#37c134779463b4d, 16#21a20bf64b6e735,
     16#1c0585ac88b69f2, 16#1b3fcea8dd30e56, 16#334bc301aefd97,
     16#37066eb7e80a946, 16#15a19a6331b570f, 16#35e67fa43c3f7d0,
     16#152a4020145fb80, 16#8d55139491dfbe, 16#21d9cba585c059d,
     16#31475f363654635, 16#2567b17acb7a104, 16#39201be3a7681c5,
     16#6bc675fd26b601, 16#334b93232b1b1e3, 16#357c402cb732c6a,
     16#362e32efe4db46a, 16#8edc7ae3da51e5, 16#31573376785eac9,
     16#6c6145ffa1169d, 16#18ec2c393d45359, 16#1f1a5f256e7130c,
     16#131cc2f49b8004f, 16#36f715a249f4ec2, 16#1c27629826c50d3,
     16#914d9a6648726a, 16#27f5bf5ce2301e8, 16#3dd493b8012970f,
     16#be13bed1e00e5c, 16#ceef033b74ae10, 16#3da38c6a50abe03,
     16#15cbd1a421c7a8c, 16#22794e3ec6ef3b1, 16#26154d26e7ea99f,
     16#3a66681359a6ab6].

