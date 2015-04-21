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

-export([interval_1/1, seed0/1, seed/1,
         exs64/1, exs1024/1, as183/1, exsplus/1,
         sfmt/1, tinymt/1,
	 measure/1
	]).

-export([test/0]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [interval_1, seed0, seed,
     exs64, exs1024, as183, exsplus,
     sfmt, tinymt,
     measure
    ].

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

seed0(doc) ->
    ["Test that seed is set implicitly, and always the same."];
seed0(suite) ->
    [];
seed0(Config) when is_list(Config) ->
    Self = self(),
    _ = spawn(fun() -> Self ! random:uniform() end),
    F1 = receive
		   Fa -> Fa
	  end,
    _ = spawn(fun() -> random:seed(),
			     Self ! random:uniform() end),
    F2 = receive
		   Fb -> Fb
	       end,
    F1 = F2,
    ok.

seed(doc) ->
    ["Test that seed/1 and seed/3 is equivalent."];
seed(suite) ->
    [];
seed(Config) when is_list(Config) ->
    Self = self(),
    Seed = {S1, S2, S3} = now(),
    _ = spawn(fun() ->
    	random:seed(S1,S2,S3),
    	Rands = lists:foldl(fun
	    (_, Out) -> [random:uniform(10000)|Out]
	end, [], lists:seq(1,100)),
    	Self ! {seed_test, Rands}
    end),
    Rands1 = receive {seed_test, R1s} -> R1s end,
    _ = spawn(fun() ->
    	random:seed(Seed),
    	Rands = lists:foldl(fun
	    (_, Out) -> [random:uniform(10000)|Out]
	end, [], lists:seq(1,100)),
    	Self ! {seed_test, Rands}
    end),
    Rands2 = receive {seed_test, R2s} -> R2s end,
    Rands1 = Rands2,
    ok.


interval_1(doc) ->
    ["Check that uniform/1 returns values within the proper interval."];
interval_1(suite) ->
    [];
interval_1(Config) when is_list(Config) ->
    Top = 7,
    N = 10,
    check_interval(N, Top),
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
    check_exs64(),
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
        Else -> Else
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
[0.9315028441266733,0.8334831844989443,0.11568496252628103,
 0.28378835588905754,0.9253697105997255,0.5730085599015873,0.8980359774219311,
 0.5212648545786374,0.03140880406574951,0.3726555249454784,0.6806937885722142,
 0.3839392296290643,0.9257988646090305,0.7861232770458624,0.666043431850677,
 0.5721354610620599,0.8362704805115169,0.7128307428577604,0.6864634669723111,
 0.6767712742420587,0.2157319741700503,0.44513117257229873,0.5640194853929335,
 0.9910612849989962,0.3141500152180757,0.3608031250818428,0.4061524013158359,
 0.27425930517843283,0.3442089118601773,0.7514634937817765,0.9891787505421689,
 0.2839228188704109,0.3279071531642522,0.4943036173783554,0.5744880325239673,
 0.9753965491741055,0.245167230301592,0.6338009480268425,0.6724081457216872,
 0.39811998356176365,0.3368056707921709,0.13927424393909696,
 0.023419202171936177,0.38010883199862316,0.38737881368676036,
 0.13589488861934584,0.31817240468056546,0.44064018378076636,
 0.6303553889648745,0.8707538235469184,0.4180321492644248,0.507203255620123,
 0.2764692284467926,0.15278057718245025,0.22938881764111121,
 0.19666668741981397,0.2753479716125606,0.7073707950399698,
 0.31096008733368075,0.8291320471837788,0.10905765190842497,
 0.44180135982326146,0.7008570216443422,0.32388823517513543,0.661897131134675,
 0.03974070164310886,0.6957892770642927,0.7453582740539846,0.6562496976335006,
 0.6687872664185184,0.6914046971993091,0.24711025912328827,0.9251050575837373,
 0.2124559537414602,0.4425653231779131,0.9096166757294157,0.6455017506610958,
 0.48837627847292103,0.6931602020665779,0.8295871991896588,0.8159678536102996,
 0.08587673911802776,0.7700261950302376,0.2776150749091829,0.858372056652372,
 0.9191038321685437,0.18962108957266224,0.5147126295222318,
 0.44987904775412246,0.4267886362687827,0.10130095481388073,
 0.22308203372818358,0.16725499667282237,0.975623642009416,
 0.41445167956839934,0.5030773850009052,0.4847769633324178,0.6632131168114143,
 0.5150385956387612,0.2857717905845608].

exs1024(doc) ->
    ["Check if exs1024 algorithm generates the proper sequence."];
exs1024(suite) ->
    [];
exs1024(Config) when is_list(Config) ->
    check_exs1024(),
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
        Else -> Else
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
    check_as183(),
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
        Else -> Else
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
    check_exsplus(),
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
        Else -> Else
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
    check_sfmt(),
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
        Else -> Else
    end.

check_sfmt_refval() ->
[0.7587869786095776,0.5903055727459271,0.4974580035771844,0.8952079433703813,
 0.07800527756987262,0.8723923822102119,0.5508738559556365,0.412761686000219,
 0.8255964838493607,0.5538715279088057,0.1634043595202743,0.9335107831129597,
 0.1263621163383038,0.4778633894114437,0.3381076760911633,0.7708655304207619,
 0.7468154758091121,0.40240602321047475,0.5645349266856292,
 0.38769509629059934,0.7572244770725315,0.7694812816962323,0.3146910370128907,
 0.030772437814337302,0.8949746615008858,0.10837218773280555,
 0.32362535859542557,0.2117897337795677,0.45396309170265753,
 0.11694839296791432,0.5487839678648821,0.6687968307334923,
 0.49470866017386056,0.39157063453261987,0.47742852603025465,
 0.8707150544204552,0.6291056996279176,0.21910217106787072,0.7311478913135705,
 0.5232205152332831,0.4253948823142319,0.346473057369346,0.2789468789191327,
 0.8960101122260117,0.01416816586027112,0.6389374231544643,0.92268204081866,
 0.47828637237620686,0.9534407898209618,0.26583343354655276,
 0.9378194822785024,0.4028439639142817,0.44100165214412884,
 0.023406576137851593,0.13692557861491236,0.9145163362646747,
 0.03154388303671588,0.6218928726440978,0.27286446706225737,
 0.22287305100422192,0.18445405717577182,0.6441099805860105,
 0.23006938286825768,0.9218523190174839,0.02280139248417723,
 0.5795420840334944,0.7042829684690299,0.8554331145378372,0.9335100538873835,
 0.8128300022363733,0.34503586086096144,0.9278055496811414,0.3908971704987104,
 0.24329070263618852,0.04615741643266692,0.5661975251432037,
 0.2561097052544611,0.8983257962154052,0.8632283000888369,0.7837290560788776,
 0.5640030877115212,0.9654921467801305,0.8974862370867017,0.814509957054283,
 0.8577490611601968,0.5039631462432358,0.8618905506706541,0.6292506702312386,
 0.5220284186122074,0.6973345334868261,0.3404813754233721,0.4440054605817435,
 0.6331565146411667,0.7341559163141428,0.6116016655256045,0.9754066665133942,
 0.7418149494896212,0.8726682900154656,0.3119126542731916,0.3830833689735931].

tinymt(doc) ->
    ["Check if tinymt algorithm generates the proper sequence."];
tinymt(suite) ->
    [];
tinymt(Config) when is_list(Config) ->
    check_tinymt(),
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
        Else -> Else
    end.

check_tinymt_refval() ->
[0.5703577164094895,0.18143154866993427,0.7458983559627086,0.2305390927940607,
 0.48838961706496775,0.5155433614272624,0.350592699367553,0.06654640147462487,
 0.4378358656540513,0.8211939823813736,0.3056597029790282,0.3598104582633823,
 0.6423892674501985,0.29299599560908973,0.8562644547782838,0.147268169792369,
 0.4310209683608264,0.5643223316874355,0.2345363488420844,0.8065952362958342,
 0.2094688939396292,0.6843150006607175,0.043553557712584734,
 0.5771734160371125,0.8335008325520903,0.7231643944978714,0.2568863967899233,
 0.5971903610043228,0.5601814512629062,0.5287215828429908,0.7804134809412062,
 0.7334948531351984,0.5575394465122372,0.840778551530093,0.7281161497812718,
 0.7682417379692197,0.8349022266920656,0.44289708882570267,0.4506657787133008,
 0.2392302246298641,0.9943235544487834,0.40697310725227,0.7178575049620122,
 0.8088395758531988,0.716893948148936,0.7085540753323585,0.8264842524658889,
 0.44989478471688926,0.08920374186709523,0.7427545310929418,
 0.46287499368190765,0.5156054541002959,0.7805743187200278,0.7199262322392315,
 0.3442954230122268,0.8871863414533436,0.5685941725969315,0.3316052563022822,
 0.5528621443081647,0.891154533950612,0.8825070119928569,0.7090685684233904,
 0.34213854279369116,0.7120723617263138,0.7726886533200741,0.6353822264354676,
 0.14404993643984199,0.37815279234200716,0.21939886850304902,
 0.21402402478270233,0.11829828727059066,0.5579846531618387,
 0.9096598548348993,0.06266396446153522,0.43342039035633206,
 0.1795676515903324,0.755117102060467,0.9785428170580417,0.4915714068338275,
 0.1426947999279946,0.43261979916132987,0.416652150452137,0.32237328705377877,
 0.9766591610386968,0.3787098203320056,0.11861736932769418,
 0.32347179926000535,0.5871884692460299,0.3738107937388122,0.9190117069520056,
 0.9638229520060122,0.485819528112188,0.9533235195558518,0.41701877932064235,
 0.536405896069482,0.17174419178627431,0.004262298345565796,
 0.4617820449639112,0.2493994648102671,0.4853041663300246].


-define(LOOP, 1000000).

measure(Suite) when is_atom(Suite) -> [];
measure(_Config) ->
    Algos = [as183, exs64, exsplus, exs1024, sfmt, tinymt],
    _ = [measure_1(Algo) || Algo <- Algos],
    ok.

measure_1(Algo) ->
    Parent = self(),
    Pid = spawn_link(fun() ->
			     Fun = fun() -> measure_2(?LOOP, rand:seed_s(Algo)) end,
			     {Time, ok} = timer:tc(Fun),
			     io:format("~.10w: ~pµs~n", [Algo, Time]),
			     Parent ! {self(), ok},
			     normal
		     end),
    receive
	{Pid, Msg} -> Msg
    end.

measure_2(N, State0) when N > 0 ->
    case rand:uniform_s(State0) of
	{Random, State}
	  when is_integer(Random), Random >= 1, Random =< 100000 ->
	    measure_2(N-1, State);
	{Random, State} when is_float(Random), Random > 0, Random < 1 ->
	    measure_2(N-1, State);
	Res ->
	    exit({error, Res, State0})
    end;
measure_2(0, _) -> ok.
