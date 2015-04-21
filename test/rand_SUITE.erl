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
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([interval_1/1, seed0/1, seed/1,
         exs64/1, exs1024/1, as183/1, exsplus/1,
         sfmt/1, tinymt/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [interval_1, seed0, seed,
        exs64, exs1024, as183, exsplus,
        sfmt, tinymt].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


seed0(doc) ->
    ["Test that seed is set implicitly, and always the same."];
seed0(suite) ->
    [];
seed0(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line _ = spawn(fun() -> Self ! random:uniform() end),
    ?line F1 = receive
		   Fa -> Fa
	  end,
    ?line _ = spawn(fun() -> random:seed(),
			     Self ! random:uniform() end),
    ?line F2 = receive
		   Fb -> Fb
	       end,
    ?line F1 = F2,
    ok.

seed(doc) ->
    ["Test that seed/1 and seed/3 is equivalent."];
seed(suite) ->
    [];
seed(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line Seed = {S1, S2, S3} = now(),
    ?line _ = spawn(fun() ->
    	random:seed(S1,S2,S3),
    	Rands = lists:foldl(fun
	    (_, Out) -> [random:uniform(10000)|Out]
	end, [], lists:seq(1,100)),
    	Self ! {seed_test, Rands}
    end),
    ?line Rands1 = receive {seed_test, R1s} -> R1s end,
    ?line _ = spawn(fun() ->
    	random:seed(Seed),
    	Rands = lists:foldl(fun
	    (_, Out) -> [random:uniform(10000)|Out]
	end, [], lists:seq(1,100)),
    	Self ! {seed_test, Rands}
    end),
    ?line Rands2 = receive {seed_test, R2s} -> R2s end,
    ?line Rands1 = Rands2,
    ok.


interval_1(doc) ->
    ["Check that uniform/1 returns values within the proper interval."];
interval_1(suite) ->
    [];
interval_1(Config) when is_list(Config) ->
    ?line Top = 7,
    ?line N = 10,
    ?line check_interval(N, Top),
    ok.

check_interval(0, _) -> ok;
check_interval(N, Top) ->
    X = random:uniform(Top),
    if
	X < 1 ->
	    test_server:fail(too_small);
	X > Top ->
	    test_server:fail(too_large);
	true ->
	    ok
    end,
    check_interval(N-1, Top).

exs64(doc) ->
    ["Check if exs64 algorithm generates the proper sequence."];
exs64(suite) ->
    [];
exs64(Config) when is_list(Config) ->
    ?line check_exs64(),
    ok.

check_exs64() ->
    Refval = check_exs64_refval(),
    Testval =
        begin
            rand:seed(exs64, {100, 200, 300}),
            lists:map(fun(_) -> rand:uniform() end,
                lists:seq(1,100))
        end,
    case Refval =:= Testval of
        true -> ok;
        Else -> test_server:fail(Else)
    end.

%%% generated by:
%%%    file:write_file("output.txt", 
%%%        io_lib:fwrite("~p.~n",[
%%%                begin
%%%                        exs64:seed(100,200,300),
%%%                        lists:map(fun(X) -> exs64:uniform() end,
%%%                            lists:seq(1,100))
%%%                end ])).

check_exs64_refval() ->
[0.986050700188432,0.5235711718301691,0.07071951099622452,0.7363897751562869,
 0.5420968628566721,0.5257427056155621,0.4537790356011092,0.4698866229745387,
 0.6198167438991624,0.17285694373440413,0.9434061205387957,0.4812567262201156,
 0.6281575017962497,0.8111284295082782,0.8100833182278215,0.812985268886191,
 0.9805518344710175,0.4055363018814699,0.9513188280136741,0.8055907677802455,
 0.7409195905739574,0.7084576228849757,0.8167573330948723,0.5223430903175182,
 0.8504362292114737,0.05387398896116299,0.6113141702063929,0.9011322737526362,
 0.46114411490569895,0.08422974392731204,0.6396618020843351,
 0.3644692027235815,0.38995406016668427,0.7740179988160041,0.9783167953428882,
 0.38291009373104906,0.6691631692952298,0.28680474029583175,
 0.42579427514515006,0.5751519358575001,0.9809623559429324,0.7572067155883307,
 0.0894736654527395,0.3940636035940241,0.47156129703009486,0.7399643402850398,
 0.5777130565715608,0.9395569227138376,0.7526534248207688,0.15289687364509572,
 0.7692833361898065,0.758151773672984,0.5439109033249026,0.017145672745556964,
 0.2256215037481,0.3095202171149242,0.2867377118343142,0.512504415209046,
 0.2943634647848587,0.23279383898691036,0.6076850132647763,
 0.20216635150687667,0.653703434002563,0.512523046503154,0.7951451277949315,
 0.5668236474334339,0.6310461513291447,0.049227237984301606,
 0.46694700636781655,0.12178348525111131,0.9120040053004944,
 0.9345003437064332,0.6984432784090616,0.017942424999008305,
 0.4530907727613225,0.9053691261963785,0.5157882198321627,
 0.021311099219010494,0.37458695028387423,0.8418521788823254,
 0.7414786617341689,0.8085671124749082,0.45501890633892306,0.594965119707906,
 0.722817389968056,0.03760395693760591,0.2190737590967537,0.8488199744106786,
 0.08252526732468703,0.01619259021405671,0.8473662177114942,0.62736910383715,
 0.13575790278125194,0.0936388246920517,0.8427475755098074,0.6778296984460942,
 0.08981184115032739,0.9449964212468592,0.42403762848634907,
 0.8504593797561433].

exs1024(doc) ->
    ["Check if exs1024 algorithm generates the proper sequence."];
exs1024(suite) ->
    [];
exs1024(Config) when is_list(Config) ->
    ?line check_exs1024(),
    ok.

check_exs1024() ->
    Refval = check_exs1024_refval(),
    Testval =
        begin
            rand:seed(exs1024, {100, 200, 300}),
            lists:map(fun(_) -> rand:uniform() end,
                lists:seq(1,100))
        end,
    case Refval =:= Testval of
        true -> ok;
        Else -> test_server:fail(Else)
    end.

check_exs1024_refval() ->
[0.22302708606176322,0.3973198649794769,0.616877332275436,0.7771375078950928,
 0.8812628235240739,0.3949244308252615,0.6064323851581476,0.49406137280478657,
 0.7126617601093003,0.6909762849767008,0.2628407452984875,0.2022097178239252,
 0.8486271284810278,0.24058398777026152,0.7941710571063774,0.9921683935492012,
 0.25296985398860267,0.7636708880293404,0.00286670445057589,
 0.7208052204747095,0.4670181636904477,0.8798105514071334,0.9947997247308423,
 0.4509850322809452,0.32707816613277935,0.6514515156874102,0.3661126889480272,
 0.7184941371843652,0.9442232253511715,0.19728861262496655,0.6913171879962102,
 0.8228105259452942,0.08111006865683819,0.31909581693718203,
 0.17902546602669675,0.8613954458453186,0.783540805977772,0.24881574388908415,
 0.34623976567876097,0.36948238100377817,0.20982311331526193,
 0.45852473708379743,0.13365695778664588,0.30501699275443517,
 0.5526488561400933,0.6809773861831849,0.707411820648709,0.9838698774027799,
 0.35802504734492135,0.0385970768815801,0.261967651931694,0.6840758212702521,
 0.8294939791337562,0.4665596462110006,0.9158254507914205,0.7291200459614908,
 0.785510282395251,0.7044491684913389,0.9383502613266707,0.9901958166448723,
 0.6088727806933868,0.1352963973403332,0.6831893306237188,0.4623206452604651,
 0.26773308704898513,0.5241054398923441,0.7278188823524236,0.8709215641501622,
 6.616700661800706e-4,0.4763015283156405,0.07712905419547943,
 0.6624590679207223,0.7274969822710255,0.004680607719587754,0.750631139762974,
 0.8953120107672551,0.09003428298612491,0.7540248936054397,0.8781097067006107,
 0.4967893668332755,0.6737979537250193,0.5860914336296202,0.8883226955960737,
 0.31016073760510166,0.9149585188314451,0.24207057160698658,
 0.8869375102938568,0.07779776470015656,0.9113958805106274,0.808980882480058,
 0.733042907948179,0.3969880172554393,0.2397003987507567,0.9722071929144935,
 0.3389032441157487,0.017876450605402654,0.4640770480432157,
 0.1317975336010899,0.6016898212995697,0.043957783085223086].

as183(doc) ->
    ["Check if as183 algorithm generates the proper sequence."];
as183(suite) ->
    [];
as183(Config) when is_list(Config) ->
    ?line check_as183(),
    ok.

check_as183() ->
    Refval = check_as183_refval(),
    Testval =
        begin
            rand:seed(as183, {100, 200, 300}),
            lists:map(fun(_) -> rand:uniform() end,
                lists:seq(1,100))
        end,
    case Refval =:= Testval of
        true -> ok;
        Else -> test_server:fail(Else)
    end.

check_as183_refval() ->
[0.3988082692470347,0.6494426672046418,0.3850156351255021,0.401934236944699,
 0.16444897006760417,0.4634412966832284,0.21492590025692193,
 0.6878560091210317,0.16137835119005062,0.7805826651615493,0.5670296570139242,
 0.8247450477305718,0.6677512630116204,0.23757680493375588,0.6290617937007752,
 0.02132639206689202,0.9095130874051798,0.9174245706449118,
 0.14514152673038883,0.9569664948194267,0.3690113829734425,0.7272939286169946,
 0.43800511053942265,0.6939380981703922,0.04077973992183015,
 0.3550604115809275,0.11402160612958201,0.214457990230827,0.07393718905781066,
 0.8374711133727466,0.23405637502057175,0.33268971643139356,
 0.8422032431171235,0.0803167844848014,0.33936334467803553,0.4481425861594337,
 0.4009202384898862,0.006063231674122704,0.44177765301675187,
 0.9823860519622407,0.7456666022676617,0.5939066104533544,0.8636603216079304,
 0.22006519425098636,0.6912437494759509,0.2035259119749342,0.898464015344044,
 0.6091193401724402,0.11877864817352979,0.7819687397605564,0.7150242266201063,
 0.9376525571303898,0.6381066112952634,0.3251445992581634,0.10277761031666444,
 0.971118736693314,0.32845451224910005,0.1820323080723747,0.22406656394367586,
 0.40744456847098576,0.2735955544383353,0.2844649517034261,0.7214314522530401,
 0.975855531889827,0.3411485170410622,0.12874409278083965,0.5091781314803088,
 0.7502205213892414,0.35274566742496116,0.1384768849610114,
 0.09897119546747146,0.7498722278150527,0.6971245270691258,
 0.26949917055173733,0.2292753107948151,0.13232200893687196,
 0.025102549918631478,0.051326654829517926,0.6222546823631472,
 0.4935322508190537,0.08401219246051217,0.764151412355651,0.28751281878394286,
 0.926671633709198,0.8108934760105946,0.09373342127875439,0.12315385203574758,
 0.5105366933035849,0.4588460084309116,0.2745251676365159,0.8298450775838443,
 0.20976428528003654,0.7583718227140195,0.6838340419751652,0.7308030081749526,
 0.06055605431485711,0.12701024501690483,0.33105417506329227,
 0.9318948932205227,0.6332296441383853].

exsplus(doc) ->
    ["Check if exsplus algorithm generates the proper sequence."];
exsplus(suite) ->
    [];
exsplus(Config) when is_list(Config) ->
    ?line check_exsplus(),
    ok.

check_exsplus() ->
    Refval = check_exsplus_refval(),
    Testval =
        begin
            rand:seed(exsplus, {100, 200, 300}),
            lists:map(fun(_) -> rand:uniform() end,
                lists:seq(1,100))
        end,
    case Refval =:= Testval of
        true -> ok;
        Else -> test_server:fail(Else)
    end.

check_exsplus_refval() ->
[0.9466203757060183,0.457438796090538,0.2674275954532104,0.561695792679895,
 0.029585313485405336,0.7732267368594427,0.583206464417943,0.8291185083026943,
 0.5848332037646896,0.8447882614184774,0.7683087869495822,0.7573383946949965,
 0.23770932700640318,0.15084131659966285,0.3124688522011774,
 0.8612271060566629,0.3900731355692434,0.18337959691394554,
 0.41909365701291784,0.8011393285606173,0.9735187552360632,0.4559297786930638,
 0.3873786794762007,0.9954958177779738,0.7166776980542559,0.6522604578032749,
 0.21472264153332948,0.8959838404405778,0.3638180801412556,
 0.29510192353260467,0.18892671209428777,0.28822208484137835,
 0.3921041094064312,0.26870604038083734,0.24505620392217758,
 0.3674959348234581,0.5639057947125191,0.6056250877855712,0.6399810008191749,
 0.48660871402411165,0.08774907590137501,0.8500407036364139,
 0.3506737163142116,0.7362250555415847,0.04010081043131724,0.9445319735562754,
 0.9109271940692084,0.4318755779170966,0.5140009464676033,0.11942568369064285,
 0.8994980809919664,0.5266536483591635,0.39018277555078107,0.8869416591629686,
 0.62868067149514,0.10221733029940284,0.8090840125850306,0.962293041444949,
 0.6019045419526857,0.2962059473030495,0.9384403651353997,0.19005607259788526,
 0.5104134837877109,0.4614926588242743,0.334269408924819,0.8200877579398219,
 0.05684355854800272,0.010334100357433366,0.6115952093213971,
 0.6929107982955988,0.7572571891538613,0.996885819853509,0.5324477491940169,
 0.1725441785787863,0.9391519915647565,0.3049621779365909,0.32236518822009086,
 0.6643515725336997,0.5359422756350475,0.7563314590148708,0.04195488136642543,
 0.16385645434526214,0.17537296416991005,0.5636755341561253,
 0.4620748210885053,0.993569604891124,0.9779762650274576,0.05312164592766295,
 0.5779384212694867,0.8606960477808911,0.6801396310865611,0.5673412996513162,
 0.13675242351047698,0.8043645483792333,0.14784147418879137,
 0.2279539020730592,0.9094061437355897,0.8818200684661817,0.5620106451459033,
 0.7992937208291677].

sfmt(doc) ->
    ["Check if sfmt algorithm generates the proper sequence."];
sfmt(suite) ->
    [];
sfmt(Config) when is_list(Config) ->
    ?line check_sfmt(),
    ok.

check_sfmt() ->
    Refval = check_sfmt_refval(),
    Testval =
        begin
            rand:seed(sfmt, {100, 200, 300}),
            lists:map(fun(_) -> rand:uniform() end,
                lists:seq(1,100))
        end,
    case Refval =:= Testval of
        true -> ok;
        Else -> test_server:fail(Else)
    end.

check_sfmt_refval() ->
[0.34095379209611565,0.9698528550798073,0.5463601154042408,0.6674096992937848,
 0.6243699203478172,0.8063167367363349,0.6355772259412333,0.49863365676719695,
 0.9673670240445063,0.14884696051012725,0.49993571208324283,
 0.5077594715403393,0.1961685427231714,0.7977193671977147,0.6616341680055484,
 0.925885287928395,0.9874838712858036,0.5518772358773276,0.25851922982838005,
 0.562986669014208,0.4505133518250659,0.7737237896071747,0.3675473592011258,
 0.6069880634313449,0.4967200291575864,0.08943050552625209,
 0.22487184626515955,0.0841617815895006,0.09039560530800372,
 0.15057343675289303,0.7545793481403962,0.3501236849697307,0.4122246803017333,
 0.7342740219319239,0.09604397846851498,0.8675743079511449,0.8790607479168102,
 0.7091529875760898,0.9482213073642924,0.7944371543126181,0.6448099928675219,
 0.5281542687444016,0.010084740701131523,0.9133521531475708,
 0.5648692845134065,0.9707729188958183,0.31165899464394897,0.4657611275324598,
 0.4726925917202607,0.49482220772188157,0.40276405902113765,
 0.06947254447732121,0.9020659952657297,0.8962375988485292,0.5820635553682223,
 0.8371632470516488,0.2593710747314617,0.8378262795740739,0.8365427987882867,
 0.43763297831173986,0.17241261096205562,0.1078114703996107,
 0.10970929486211389,0.6551123018143699,0.0038314544362947345,
 0.241009930963628,0.5276215587509796,0.5889477849705145,0.4957381187705323,
 0.059683306026272476,0.570936688571237,0.44234032288659364,
 0.8897841823054478,0.7619887838372961,0.9114388084271923,0.1902366733411327,
 0.3594078457681462,0.9435713366838172,0.13049821939785033,0.5396002534544095,
 0.38471195998135954,0.2601513817207888,0.546821205294691,0.098582181497477,
 0.5363626681501046,0.06092990993056446,0.2425411184085533,0.9575270806672052,
 0.7277637141523883,0.3575617802562192,0.5578306129900739,0.832140929880552,
 0.8531335721490905,0.9470531650586054,0.03698489011730999,0.8340912711573765,
 0.793769245618023,0.7654061765642837,0.7692602806491777,0.7681567071704194].

tinymt(doc) ->
    ["Check if tinymt algorithm generates the proper sequence."];
tinymt(suite) ->
    [];
tinymt(Config) when is_list(Config) ->
    ?line check_tinymt(),
    ok.

check_tinymt() ->
    Refval = check_tinymt_refval(),
    Testval =
        begin
            rand:seed(tinymt, {100, 200, 300}),
            lists:map(fun(_) -> rand:uniform() end,
                lists:seq(1,100))
        end,
    case Refval =:= Testval of
        true -> ok;
        Else -> test_server:fail(Else)
    end.

check_tinymt_refval() ->
[0.5703577165259048,0.1814315487863496,0.7458983560791239,0.23053909291047603,
 0.4883896171813831,0.5155433615436777,0.3505926994839683,0.0665464015910402,
 0.4378358657704666,0.821193982497789,0.30565970309544355,0.35981045837979764,
 0.6423892675666139,0.29299599572550505,0.8562644548946992,
 0.14726816990878433,0.4310209684772417,0.5643223318038508,
 0.23453634895849973,0.8065952364122495,0.20946889405604452,
 0.6843150007771328,0.043553557829000056,0.5771734161535278,
 0.8335008326685056,0.7231643946142867,0.25688639690633863,0.5971903611207381,
 0.5601814513793215,0.5287215829594061,0.7804134810576215,0.7334948532516137,
 0.5575394466286525,0.8407785516465083,0.7281161498976871,0.768241738085635,
 0.8349022268084809,0.442897088942118,0.45066577882971615,0.23923022474627942,
 0.9943235545651987,0.4069731073686853,0.7178575050784275,0.8088395759696141,
 0.7168939482653514,0.7085540754487738,0.8264842525823042,0.4498947848333046,
 0.08920374198351055,0.7427545312093571,0.462874993798323,0.5156054542167112,
 0.7805743188364431,0.7199262323556468,0.34429542312864214,0.887186341569759,
 0.5685941727133468,0.33160525641869754,0.55286214442458,0.8911545340670273,
 0.8825070121092722,0.7090685685398057,0.3421385429101065,0.7120723618427292,
 0.7726886534364894,0.6353822265518829,0.1440499365562573,0.3781527924584225,
 0.21939886861946434,0.21402402489911765,0.11829828738700598,
 0.557984653278254,0.9096598549513146,0.06266396457795054,0.4334203904727474,
 0.1795676517067477,0.7551171021768823,0.978542817174457,0.4915714069502428,
 0.14269480004440993,0.4326197992777452,0.4166521505685523,0.3223732871701941,
 0.9766591611551121,0.37870982044842094,0.1186173694441095,0.3234717993764207,
 0.5871884693624452,0.37381079385522753,0.919011707068421,0.9638229521224275,
 0.4858195282286033,0.9533235196722671,0.4170187794370577,0.5364058961858973,
 0.17174419190268964,0.004262298461981118,0.4617820450803265,
 0.2493994649266824,0.4853041664464399].
