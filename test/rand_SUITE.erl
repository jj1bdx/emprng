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
        Else -> test_server:fail({Alg, Else})
    end.

gen(Algo) ->
    Seed = rand:seed(Algo, {100, 200, 300}),
    gen(?LOOP, Seed, []).

gen(N, State0, Acc) when N > 0 ->
    {Random, State} = rand:uniform_s(1 bsl 59, State0),
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
    [319141588671983412,557783621674435131,195888530915544405,154938545395818485,
     255812719753660651,473447150887733994,192511984483695649,472293393595956765,
     61800926600707014,530543982124467401,306198791537178952,350759193701693920,
     482743800474327217,55575219967591408,154064020497807909,112409446653622987,
     303985388360128878,82357244654143478,3990340598540290,356496051706306092,
     249747458085336518,201449293731880031,511911198499519569,82540371385229677,
     377928042134780113,433676864709420495,428273978842587309,122490547865895346,
     9149268583116529,237192056516417834,85524219877107465,447634304089807284,
     481192875425025208,219473000477328952,219138349780087994,65509927899829066,
     376541303444664711,549430564500634039,127693054166216214,291306324331420832,
     426143266468562826,488921106941293735,168692709240483118,446993263038599643,
     335983338682305145,140119263944937751,364362368612757945,512929949415679851,
     494689959441648478,221499134667655273,466252094872337033,378298414260869152,
     270377724705854121,350817357475882065,340840358655116380,373111016797446792,
     354618701793827338,139454736457974229,138601632726736333,268522678976904067,
     74815178423704713,65911738253136799,478617251515486289,344112724065212153,
     301502949368405325,54247470598850593,5014589752206130,168801529908476382,
     501020192278867903,290235054137071797,341418794263012126,125257116193983249,
     334445048559412829,444936411425486103,169491714258624825,469672070205669221,
     174769250088230361,495397003510433469,201771099110915128,192983135886628427,
     135980219719810034,213941952310595277,572848442445579420,267702975966313630,
     367344209639370849,235706647656126362,464169066250319512,245164557936787691,
     171266500878962313,311182762441253442,120863342166989311,380818317377321391,
     491804995637816215,241767489252455126,34885506432035243,430130299561174389,
     323322495667722704,396718912665507766,528559997233003471,550122714628498054];

reference_val(exs1024) ->
    [78898311962576191,508893083581941671,81235715133479719,206387766307580924,
     83152736994025975,117016170083477441,381970580112070378,206483451029404095,
     25668386032366202,567509860589288709,245141000491753950,62083111856913227,
     199889455533178703,514925092819147847,547567244945751803,151099211083586036,
     175317040747707911,359773785570445331,287984118330703874,104326962425550460,
     325024387262717872,140369437065694019,558514277392413412,476119061704643422,
     189595866819716212,144734355928930035,299485246326550670,381447024345922976,
     74164408936283423,538383039785572691,373342220788980345,404462110845314251,
     18892998813582976,181509499215712312,71541276310774730,180473061878384754,
     548611343508135695,419535597815720722,2096344216176830,269791149086405374,
     507706652184901324,194151478137654442,373569164432197345,376425007727114488,
     467548334214733412,308097180545248807,481262982584242768,108390229360117122,
     524622234504459654,39998191316654902,147541743891537753,468166716674632779,
     244579036128291581,525883721533396876,18338285451254677,522148102075896189,
     2835587998346702,354972150333716667,46934097088072427,456093160927810330,
     230935665769036937,329689302914536498,556578343731817438,242888104698890387,
     32046216884785296,542541683942946477,376138725399900509,522765507940686262,
     440767054558629461,194861077342501142,218124620916108594,207293894022577394,
     257493746691383527,276747845397322575,432686240338639746,474955535725163667,
     267432199114361671,356167196901514348,70746242353598030,169740236405989697,
     68660369555413625,327683247058228644,265112805926880356,564265618523428051,
     330184431166934953,80861626618092773,280421723677071136,406422466901413603,
     277324645107298007,513036335817543337,225712380933704911,142995761359401518,
     402851616194125958,386840038092179546,264766167064381501,328569846192596006,
     477365241285513929,424141000586568578,76848571952187927,112640496269301976];

reference_val(exsplus) ->
    [168241236504998120,460893759984084943,20935930353018870,155480652086635734,
     165374847636402219,90012966164290618,329298691285404930,549254811780988386,
     251206333705794336,277138073113182588,385761792983983665,405059230429236393,
     62172586296900040,427219097860164203,133289776964462082,541579823757689037,
     234980570568685071,6700678465473432,521944641972201020,17100803233654296,
     559264911572173335,123410308715705878,575107247351825536,384631434414034874,
     301359827661607166,248105550564680476,566145466956153879,180731534488414806,
     365304372651945274,366404539957529258,81043435876978954,67910418290341145,
     555309628191335857,391202892799440118,14738049209944669,302685226603635757,
     301117284809183399,60695748696387540,345713219309663418,7022637340890171,
     3543416628762297,293079002980139990,99449885999500118,11305976625461311,
     288610636627369536,151690742624861831,265508922065868644,349648686309952946,
     537023438762061676,37214584242263268,199459873565158856,220119587771730342,
     483987866289675840,99880057795297835,107614651381115287,573486182068387581,
     137472197409691707,71503089101107465,469677808265858825,226568275557467148,
     306620120450359220,10583047872887490,430931325888328050,187765962719467967,
     182666603492541378,233063445962863725,444520074432544971,192802932900378889,
     336939898517992792,143432812624081831,526412639479342561,191743351487378687,
     226411420743329300,335092435655722138,216654393834520552,250407045756033991,
     466307144241170129,575876435406443871,212589071042962533,260058023837119538,
     166835784978112567,86782662066185454,552499530807623043,559272018441785824,
     369525978578250856,549175825114620534,220758730029102905,496751066518842170,
     26019450131583595,379722894845627061,174422727381509903,126843954652143710,
     454600811477629241,550609293998899540,208283007443683687,279450053044644070,
     400241706033351589,211427847498741858,4157077915887693,483590908933799085].

