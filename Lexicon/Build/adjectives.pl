:- expects_dialect(sicstus).

:- discontiguous
    m/3,
    a/4.

m(Stem,Tag,Surf) :-
    a(Forms,Adv,Subcat,Suffixes),
    candidate_stem(Stem0,Forms),
    (	atom(Stem0)
    ->  Stem0 = Stem1
    ;   hdrug_util:concat_all(Stem0,Stem1,' ')
    ),
    select_form(Forms,Adv,Surf1,Infl),
    select_subcat(Subcat,Infl,Tag),
    add_compounds:add_compounds(Suffixes,Stem1,Surf1,Stem,Surf).

m(teveel,  me_adjective(no_e(odet_adv)), teveel).
m(teveel,  vp_om_me_adjective(no_e(odet_adv)), teveel).

m(zoveel,  adjective(meer), zoveel).

m(meer,    adjective(meer),   meer).

m(minder,  adjective(meer),   minder).

%% todo: een minder jaartje/resultaat

m(anders,  adjective(anders),  anders).

m(extra,   post_adjective(no_e), 'extra\'s').

m(verdacht,post_adjective(no_e),verdachts).

%% obl. complement

m('op het gevaar af', adjective(pred(oadv),object_vp),[op,het,gevaar,af]).
m('op het gevaar af', adjective(pred(oadv),object_sbar),[op,het,gevaar,af]).

select_form(Forms,Adv,W,Tag) :-
    lists:member(Form,Forms),
    Form =.. [Infl,W],
    combine_infl_adv(Infl,Adv,Tag).

combine_infl_adv(stof,   _,   stof       ).
combine_infl_adv(prefix, _,   prefix     ).
combine_infl_adv(e,      _,   e          ).
combine_infl_adv(ge_e,   _,   ge_e       ).
combine_infl_adv(ere,    _,   ere        ).
combine_infl_adv(ste,    _,   ste        ).

combine_infl_adv(pred,      ADV, pred(ADV)              ).
combine_infl_adv(pred_er,   ADV, pred_er(ADV)           ).
combine_infl_adv(pp_pred,   ADV, pp_pred(ADV)           ).
combine_infl_adv(both,      ADV, both(ADV)              ).
combine_infl_adv(ge_both,   ADV, ge_both(ADV)           ).
combine_infl_adv(postn_both,ADV, postn_both(ADV)        ).
combine_infl_adv(postn_pred,ADV, postn_pred(ADV)        ).
combine_infl_adv(no_e,      ADV, no_e(ADV)              ).
combine_infl_adv(ge_no_e,   ADV, ge_no_e(ADV)           ).
combine_infl_adv(postn_no_e,ADV, postn_no_e(ADV)        ).
combine_infl_adv(er,        ADV, er(ADV)                ).
combine_infl_adv(het_st,    ADV, het_st(ADV)            ).
combine_infl_adv(st,        ADV, st(ADV)                ).
combine_infl_adv(ende,      ADV, ende(ADV)              ).
combine_infl_adv(end,       ADV, end(ADV)               ).

select_subcat(_,Infl,adjective(Infl)).
select_subcat(List,Infl,adjective(Infl,Sc)) :-
    lists:member(Sc,List),
    \+ illegal_infl_subcat_pair(Infl,Sc).

illegal_infl_subcat_pair(Infl,Sc) :-
    inflected(Infl),
    illegal_infl_subcat_pair(Sc).

inflected(e).
inflected(ere).
inflected(ste).
inflected(stof).
inflected(prefix).
inflected(ge_e).

illegal_infl_subcat_pair(object_sbar).
illegal_infl_subcat_pair(van_sbar).
illegal_infl_subcat_pair(object_vp).
illegal_infl_subcat_pair(er_pp_sbar(_)).
illegal_infl_subcat_pair(er_pp_vp(_)).
illegal_infl_subcat_pair(refl_er_pp_sbar(_)).

candidate_stem(Stem,Forms) :-
    (   lists:member(stem(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(no_e(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(ge_no_e(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(postn_no_e(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(both(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(ge_both(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(postn_both(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(postn_pred(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(stof(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(prefix(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(pred(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(pred_er(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(pp_pred(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(end(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(ende(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(e(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(ge_e(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(er(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(ere(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(st(Stem0),Forms)
    ->  Stem0=Stem
    ;   lists:member(ste(Stem0),Forms)
    ->  Stem0=Stem
    ;   format(user_error,"No stem for ~w~n",[Forms]),
        fail
    ).

a([prefix('ad-valorem'),
   prefix([ad,valorem])],nonadv,[],[]).

a([prefix('alles-of-niets'),
   prefix([alles,of,niets])],adv,[],[]).  % we speelden alles-of-niets

a([prefix('all-American'),
   prefix([all,'American'])],nonadv,[],[]).

a([both('all-in'),
   both([all,in])],adv,[],[]).

a([prefix('anti-terrorisme'),
   prefix([anti,terrorisme])],nonadv,[],[]).

a([prefix('B&B')],nonadv,[],[]).

a([prefix('bijna-dood'),
   prefix([bijna,dood])],nonadv,[],[]).

a([prefix('bonus-malus'),
   prefix('bonus/malus'),
   prefix([bonus,malus])],nonadv,[],[]).

a([prefix(breedbeeld)],nonadv,[],[]).

a([prefix(concept)],nonadv,[],[]).

a([prefix([close,up]),
   prefix('close-up')],nonadv,[],[]).

a([prefix([cross,over]),
   prefix('cross-over')],nonadv,[],[]).

a([prefix('doe-het-zelf')],nonadv,[],[]).

a([prefix('double-breasted'),
   prefix([double,breasted])],nonadv,[],[]).

a([prefix('drive-in'),
   prefix([drive,in])],nonadv,[],[]).

a([both([één,op,één]),
   both('één-op-één'),
   both([een,op,een]),
   both('een-op-een')],padv,[],[]).

a([prefix('eerste-fase'),
   prefix([eerste,fase])],nonadv,[],[]).

a([prefix('eerste-klas'),
   prefix([eerste,klas]),
   prefix(['1e',klas])],adv,[],[]).

a([prefix('fiber-optic'),
   prefix([fiber,optic])],nonadv,[],[]).

a([prefix(graf)],nonadv,[],[]).

a([prefix([haat,liefde]),
   prefix('haat-liefde')],nonadv,[],[]).

a([prefix(hardcore),
   prefix('hard-core'),
   prefix([hard,core])],nonadv,[],[]).

a([prefix([high,end])],nonadv,[],[]).

a([prefix(horror)],nonadv,[],[]).

a([prefix('houtje-touwtje'),
   prefix([houtje,touwtje])],nonadv,[],[]).

a([prefix([huis,aan,huis]),
   prefix('huis-aan-huis')],nonadv,[],[]).

a([prefix(['huis-tuin-en-keuken'])],nonadv,[],[]).

a([prefix([in,vitro])],adv,[],[]).

a([prefix(internet)],nonadv,[],[]).

a([prefix(kanker)],nonadv,[],[]).

a([prefix(kaulo),
   prefix(koulo)],nonadv,[],[]).

a([prefix('kip-ei')],nonadv,[],[]).

a([prefix([knock,out]),
   prefix('knock-out')],nonadv,[],[]).

a([prefix('kop-staart'),
   prefix([kop,staart])],nonadv,[],[]).

a([prefix([korte,termijn]),
   prefix('korte-termijn')],nonadv,[],[]).

a([prefix('kosten-baten'),
   prefix([kosten,baten])],nonadv,[],[]).

a([prefix(kut)],nonadv,[],[]).

a([prefix([lange,termijn]),
   prefix('lange-termijn')],nonadv,[],[]).

a([prefix([last,minute]),
   prefix('last-minute')],nonadv,[],[]).

a([prefix('late-night'),
   prefix([late,night])],nonadv,[],[]).

a([prefix('lichaam-geest'),
   prefix([lichaam,geest])],nonadv,[],[]).

a([prefix(lifestyle)],nonadv,[],[]).

a([prefix('low-budget'),
   prefix(lowbudget),
   prefix([low,budget])],nonadv,[],[]).

a([prefix('low-key'),
   prefix(lowkey),
   prefix([low,key])],nonadv,[],[]).

a([prefix(mainstream)],nonadv,[],[]).

a([prefix('man-vrouw'),
   prefix([man,vrouw]),
   prefix('man/vrouw')],nonadv,[],[]).

a([prefix('meester-knecht'),
   prefix([meester,knecht]),
   prefix('meester/knecht')],nonadv,[],[]).

a([prefix('moeder-kind'),
   prefix([moeder,kind]),
   prefix('moeder/kind')],nonadv,[],[]).

a([prefix([moeder,dochter]),
   prefix('moeder/dochter'),
   prefix('moeder-dochter')],nonadv,[],[]).

a([prefix([moeder,zoon]),
   prefix('moeder/zoon'),
   prefix('moeder-zoon')],nonadv,[],[]).

a([prefix(mini)],nonadv,[],[]).

a([both('mond-op-mond'),
   both([mond,op,mond])],nonadv,[],[]).

a([prefix(moslim)],nonadv,[],[]).

a([prefix(nieuwbouw)],nonadv,[],[]).

a([prefix('non-food'),
   prefix([non,food])],nonadv,[],[]).

a([prefix('non-profit'),
   prefix([non,profit])],nonadv,[],[]).

a([prefix(ontwerp)],nonadv,[],[]).

a([prefix([open,source])],nonadv,[],[]).

a([prefix([ouder,kind]),
   prefix('ouder/kind'),
   prefix('ouder-kind')],nonadv,[],[]).

a([prefix([pop,up]),
   prefix('pop-up')],nonadv,[],[]).

a([prefix('prime-time'),prefix([prime,time])],nonadv,[],[]).

a([prefix('real-life'),
   prefix([real,life])],nonadv,[],[]).

a([prefix(rijks)],nonadv,[],[]).

a([prefix([self,made]),
   prefix('self-made')],nonadv,[],[]).

a([prefix(stink)],nonadv,[],[]).

a([prefix(tering)],nonadv,[],[]).

a([prefix('tweede-fase'),
   prefix([tweede,fase])],nonadv,[],[]).

a([prefix('tweede-klas'),
   prefix([tweede,klas]),
   prefix(['2e',klas])],adv,[],[]).

a([prefix('vader-kind'),
   prefix([vader,kind]),
   prefix('vader/kind')],nonadv,[],[]).

a([prefix([vader,dochter]),
   prefix('vader/dochter'),
   prefix('vader-dochter')],nonadv,[],[]).

a([prefix([vader,zoon]),
   prefix('vader/zoon'),
   prefix('vader-zoon')],nonadv,[],[]).

a([both([welles,nietes]),
   both('welles-nietes')],nonadv,[],[]).

a([prefix('win-win'),
   prefix([win,win]),
   prefix(winwin)],nonadv,[],[]).

a([both('3-D')],nonadv,[],[]).

a([prefix(blues)],nonadv,[],[]).

a([prefix(country)],nonadv,[],[]).

a([prefix(dance)],nonadv,[],[]).

a([prefix(jazz)],nonadv,[],[]).

a([prefix(rock)],nonadv,[],[]).

a([prefix([rock,and,roll]),
   prefix([rock,'&',roll]),
   prefix([rock,'\'n',roll]),
   prefix([rock,'\'n\'',roll]),
   prefix([rock,`,n,roll]),
   prefix([rock,`,'n\'',roll]),
   prefix([rock,'\'n\'',roll]),
   prefix([rock,'\'n','\'',roll])],nonadv,[],[]).

a([prefix([tag,team])],nonadv,[],[]).

a([prefix(dames)],nonadv,[],[]).

a([prefix(heren)],nonadv,[],[]).

a([prefix(mannen)],nonadv,[],[]).

a([prefix(vrouwen)],nonadv,[],[]).

a([prefix('noord-zuid')],nonadv,[],[]).

a([prefix('oost-west')],nonadv,[],[]).

a([e('Aalsmeerse'),
   no_e('Aalsmeers')],nonadv,[],[]).

a([e('Achterhoekse'),
   no_e('Achterhoeks')],nonadv,[],[]).

a([e('Afghaanse'),
   no_e('Afghaans')],nonadv,[],[]).

a([e('Afrikaanse'),
   er('Afrikaanser'),
   ere('Afrikaansere'),
   no_e('Afrikaans'),
   st('Afrikaanst'),
   ste('Afrikaanste')],adv,[],[]).

a([both('Afrikaner')],nonadv,[],[]).

a([e('Albanese'),
   no_e('Albanees')],nonadv,[],[]).

a([e('Algerijnse'),
   no_e('Algerijns')],nonadv,[],[]).

a([e('Alkmaarse'),
   no_e('Alkmaars')],nonadv,[],[]).

a([e('Almeerse'),
   no_e('Almeers')],nonadv,[],[]).

a([e('Almelose'),
   no_e('Almeloos')],nonadv,[],[]).

a([e('Ambachtse'),
   no_e('Ambachts')],nonadv,[],[]).

a([e('Amerikaanse'),
   no_e('Amerikaans'),
   er('Amerikaanser'),
   ere('Amerikaansere'),
   st('Amerikaanst'),
   ste('Amerikaanste')],adv,[],
  [h(anti),
   h(niet),
   h(pro)]).

a([e('Amersfoortse'),
   no_e('Amersfoorts')],nonadv,[],[]).

a([e('Amstelveense'),
   no_e('Amstelveens')],nonadv,[],[]).

a([e('Amsterdamse'),
   no_e('Amsterdams')],adv,[],[]).

a([e('Angelsaksische'),
   er('Angelsaksischer'),
   ere('Angelsaksischere'),
   no_e('Angelsaksisch'),
   st('Angelsaksischt'),
   ste('Angelsaksischte')],nonadv,[],[]).

a([e('Angolese'),
   no_e('Angolees')],nonadv,[],[]).

a([e('Antilliaanse'),
   no_e('Antilliaans')],nonadv,[],[]).

a([e('Antwerpse'),
   no_e('Antwerps')],nonadv,[],[]).

a([e('Apeldoornse'),
   no_e('Apeldoorns')],nonadv,[],[]).

a([e('Arabische'),
   er('Arabischer'),
   ere('Arabischere'),
   no_e('Arabisch'),
   st('Arabischt'),
   ste('Arabischte')],nonadv,[],[]).

a([e('Aramese'),
   no_e('Aramees')],nonadv,[],[]).

a([e('Argentijnse'),
   er('Argentijnser'),
   ere('Argentijnsere'),
   no_e('Argentijns'),
   st('Argentijnst'),
   ste('Argentijnste')],nonadv,[],[]).

a([e('Arische'),
   er('Arischer'),
   ere('Arischere'),
   no_e('Arisch'),
   st('Arischt'),
   ste('Arischte')],nonadv,[],[]).

a([e('Armeense'),
   no_e('Armeens')],nonadv,[],[]).

a([e('Arnhemse'),
   no_e('Arnhems')],nonadv,[],[]).

a([e('Arubaanse'),
   no_e('Arubaans')],nonadv,[],[]).

a([e('Atlantische'),
   no_e('Atlantisch')],nonadv,[],[]).

a([e('Australische'),
   er('Australischer'),
   ere('Australischere'),
   no_e('Australisch'),
   st('Australischt'),
   ste('Australischte')],nonadv,[],[]).

a([e('Aziatische'),
   er('Aziatischer'),
   ere('Aziatischere'),
   no_e('Aziatisch'),
   st('Aziatischt'),
   ste('Aziatischte')],nonadv,[],[]).

a([e('Baarnse'),no_e('Baarns')],nonadv,[],[]).

a([e('Babylonische'),
   no_e('Babylonisch')],nonadv,[],[]).

a([e('Balinese'),no_e('Balinees')],nonadv,[],[]).

a([e('Baltische'),
   no_e('Baltisch')],nonadv,[],[]).

a([e('Baskische'),
   no_e('Baskisch')],nonadv,[],[]).

a([e('Bataafse'),
   no_e('Bataafs')],nonadv,[],[]).

a([e('Beierse'),
   no_e('Beiers')],nonadv,[],[]).

a([e('Belgische'),
   er('Belgischer'),
   ere('Belgischere'),
   no_e('Belgisch'),
   st('Belgischt'),
   ste('Belgischte')],adv,[],[]).

a([e('Belgradose'),
   no_e('Belgradoos')],nonadv,[],[]).

a([e('Bengaalse'),
   no_e('Bengaals')],nonadv,[],[]).

a([e('Bengalese'),
   no_e('Bengalees')],nonadv,[],[]).

a([e('Berberse'),
   no_e('Berbers')],nonadv,[],[]).

a([e('Berlijnse'),
   no_e('Berlijns')],nonadv,[],[]).

a([e('Birmese'),
   no_e('Birmees')],nonadv,[],[]).

a([e('Boheemse'),
   no_e('Boheems')],nonadv,[],[]).

a([e('Bossche'),
   no_e('Bossch')],nonadv,[],[]).

a([e('Boergondische'),
   e('Bourgondische'),
   no_e('Boergondisch'),
   no_e('Bourgondisch')],adv,[],[]).

a([e('Boliviaanse'),
   no_e('Boliviaans')],nonadv,[],[]).

a([e('Bosnische'),
   no_e('Bosnisch')],adv,[],[]).

a([e('Bosnisch-Servische'),
   no_e('Bosnisch-Servisch')],adv,[],[]).

a([e('Brabantse'),
   no_e('Brabants')],nonadv,[],[]).

a([e('Braziliaanse'),
   no_e('Braziliaans')],nonadv,[],[]).

a([e('Bredase'),
   no_e('Bredaas')],nonadv,[],[]).

a([e('Bretonse'),
   no_e('Bretons')],nonadv,[],[]).

a([e('Britse'),
   er('Britser'),
   ere('Britsere'),
   no_e('Brits'),
   st('Britst'),
   ste('Britste')],nonadv,[],
  [h('Amerikaans'),
   h('Nederlands')]).

a([e('Brugse'),no_e('Brugs')],nonadv,[],[]).

a([e('Brusselse'),
   no_e('Brussels')],nonadv,[],[]).

a([e('Bulgaarse'),
   no_e('Bulgaars')],nonadv,[],[]).

a([e('Burundese'),
   no_e('Burundees')],nonadv,[],[]).

a([e('Byzantijnse'),
   no_e('Byzantijns')],nonadv,[],[]).

a([e('Californische'),
   no_e('Californisch')],nonadv,[],[]).

a([e('Cambodjaanse'),
   no_e('Cambodjaans')],nonadv,[],[]).

a([e('Canadese'),
   no_e('Canadees')],nonadv,[],[]).

a([e('Caribische'),
   no_e('Caribisch'),
   e('Caraïbische'),
   no_e('Caraïbisch')],nonadv,[],[]).

a([e('Catalaanse'),
   no_e('Catalaans')],nonadv,[],[]).

a([e('Chileense'),
   no_e('Chileens')],nonadv,[],[]).

a([e('Chinese'),
   no_e('Chinees')],nonadv,[],[]).

a([e('Colombiaanse'),
   no_e('Colombiaans')],nonadv,[],[]).

a([e('Congolese'),
   no_e('Congolees')],nonadv,[],[]).

a([e('Cubaanse'),
   no_e('Cubaans')],nonadv,[],[]).

a([e('Curaçaose'),
   no_e('Curaçaos')],nonadv,[],[]).

a([e('Cypriotische'),
   no_e('Cypriotisch')],nonadv,[],
  [h('Grieks'),
   h('Turks')
  ]).
				   
a([e('Cyprische'),
   no_e('Cyprisch')],nonadv,[],
  [h('Grieks'),
   h('Turks')
  ]).
				   
a([e('Deense'),
   no_e('Deens')],nonadv,[],[]).

a([e('Delftse'),
   no_e('Delfts')],nonadv,[],[]).

a([e('Democratische'),
   no_e('Democratisch')],adv,[],[]).

a([e('Dietse'),
   no_e('Diets')],nonadv,[],[]).

a([e('Dionysische'),
   no_e('Dionysisch')],nonadv,[],[]).

a([e('Dordtse'),
   no_e('Dordts')],nonadv,[],[]).

a([e('Drentse'),
   no_e('Drents')],nonadv,[],[]).

a([both('Dubliner')],nonadv,[],[]).

a([e('Duitse'),
   no_e('Duits')],adv,[],[]).

a([e('Egyptische'),
   no_e('Egyptisch')],nonadv,[],[]).

a([e('Eindhovense'),
   no_e('Eindhovens')],nonadv,[],[]).

a([both('Emmer')],nonadv,[],[]).

a([e('Engelse'),
   no_e('Engels')],nonadv,[],[]).

a([e('Enschedese'),
   no_e('Enschedees')],nonadv,[],[]).

a([e('Eritrese'),
   no_e('Eritrees')],nonadv,[],[]).

a([e('Estse'),
   no_e('Ests')],nonadv,[],[]).

a([e('Ethiopische'),
   no_e('Ethiopisch')],nonadv,[],[]).

a([e('Europese'),
   no_e('Europees')],adv,[],[h('Arabisch'),
			     h(pan) ]).

a([e('Filipijnse'),
   no_e('Filipijns')],nonadv,[],[]).

a([e('Filippijnse'),
   no_e('Filippijns')],nonadv,[],[]).

a([e('Finse'),
   no_e('Fins')],nonadv,[],[]).

a([e('Frankische'),
   no_e('Frankisch')],nonadv,[],[]).

a([e('Franse'),
   no_e('Frans')],nonadv,[],[h('Zuid')]).

a([e('Frans-Duitse'),
   no_e('Frans-Duits')],nonadv,[],[]).

a([e('Friese'),
   no_e('Fries')],nonadv,[],[]).

a([e('Gelderse'),
   no_e('Gelders')],nonadv,[],[]).

a([e('Geneefse'),
   no_e('Geneefs')],nonadv,[],[]).

a([e('Gentse'),
   no_e('Gents')],nonadv,[],[]).

a([e('Georgische'),
   no_e('Georgisch')],nonadv,[],[]).

a([e('Germaanse'),
   er('Germaanser'),
   ere('Germaansere'),
   no_e('Germaans'),
   st('Germaanst'),
   ste('Germaanste')],nonadv,[],[]).

a([e('Ghanese'),
   no_e('Ghanees')],nonadv,[],[]).

a([e('Gorinchemse'),
   no_e('Gorinchems'),
   e('Gorcumse'),
   no_e('Gorcums')],nonadv,[],[]).

a([e('Goudse'),
   no_e('Gouds')],nonadv,[],[]).

a([e('s-Gravendeelse'),
   no_e('s-Gravendeels')],nonadv,[],[]).

a([e('Griekse'),
   no_e('Grieks')],nonadv,[],[]).

a([e('Groenlandse'),
   no_e('Groenlands')],nonadv,[],[]).

a([both('Groninger')],nonadv,[],[]).

a([e('Groningse'),
   no_e('Gronings')],nonadv,[],[]).

a([e('Haagse'),
   no_e('Haags')],nonadv,[],[]).

a([both('Haarlemmer')],nonadv,[],[]).

a([e('Haarlemse'),
   no_e('Haarlems')],nonadv,[],[]).

a([e('Hawaiiaanse'),
   no_e('Hawaiiaans')],nonadv,[],[]).

a([e('Haïtiaanse'),
   no_e('Haïtiaans')],nonadv,[],[]).

a([e('Hebreeuwse'),
   no_e('Hebreeuws')],nonadv,[],[]).

a([e('Heerlense'),
   no_e('Heerlens')],nonadv,[],[]).

a([e('Helderse'),
   no_e('Helders')],nonadv,[],[]).

a([e('Helmondse'),
   no_e('Helmonds')],nonadv,[],[]).

a([e('Hengelose'),
   no_e('Hengeloos')],nonadv,[],[]).

a([e('Hilversumse'),
   no_e('Hilversums')],nonadv,[],[]).

a([e('Hollandse'),
   no_e('Hollands'),
   e(hollandse),
   no_e(hollands)],nonadv,[],[h(oer)]).

a([e('Hongaarse'),
   no_e('Hongaars')],nonadv,[],[]).

a([e('Hongkongse'),
   no_e('Hongkongs')],nonadv,[],[]).

a([e('Iberische'),
   no_e('Iberisch')],nonadv,[],[]).

a([e('Ierse'),
   no_e('Iers')],nonadv,[],[]).

a([e('IJslandse'),
   no_e('IJslands')],nonadv,[],[]).

a([e('Indiaanse'),
   no_e('Indiaans'),
   e(indiaanse),
   no_e(indiaans)],nonadv,[],[]).

a([e('Indiase'),
   no_e('Indiaas')],nonadv,[],[]).

a([e('Indische'),
   no_e('Indisch')],nonadv,[],[]).

a([e('Indonesische'),
   no_e('Indonesisch')],nonadv,[],[h(pro)]).

a([e('Iraakse'),
   no_e('Iraaks')],nonadv,[],[]).

a([e('Irakese'),
   no_e('Irakees')],nonadv,[],[]).

a([e('Iraanse'),
   no_e('Iraans')],nonadv,[],[]).

a([e('Israëlische'),
   no_e('Israëlisch'),
   e('Israelische'),
   no_e('Israelisch')],nonadv,[],[]).

a([e('Italiaanse'),
   no_e('Italiaans')],nonadv,[],[h('Noord')]).

a([e('Ivoriaanse'),
   no_e('Ivoriaans')],nonadv,[],[]).

a([e('Jamaicaanse'),
   no_e('Jamaicaans')],nonadv,[],[]).

a([e('Japanse'),
   no_e('Japans')],nonadv,[],[]).

a([e('Javaanse'),
   no_e('Javaans')],nonadv,[],[]).

a([e('Jemenitische'),
   no_e('Jemenitisch')],nonadv,[],[]).

a([e('Joegoslavische'),
   no_e('Joegoslavisch')],nonadv,[],[]).

a([e('Joodse'),
   no_e('Joods'),
   e(joodse),
   er(joodser),
   ere(joodsere),
   no_e(joods),
   st(joodst),
   ste(joodste)],adv,[],[h(niet)]).

a([e('Jordaanse'),
   no_e('Jordaans')],nonadv,[],[]).

a([e('Keltische'),
   no_e('Keltisch')],nonadv,[],[]).

a([e('Kenyaanse'),
   no_e('Kenyaans'),
   e('Keniaanse'),
   no_e('Keniaans')],nonadv,[],[]).

a([e('Keulse'),
   no_e('Keuls')],nonadv,[],[]).

a([e('Koerdische'),
   no_e('Koerdisch'),
   e('Kurdische'),
   no_e('Kurdisch')],nonadv,[],[]).

a([e('Kongolese'),
   no_e('Kongolees')],nonadv,[],[]).

a([e('Koreaanse'),
   no_e('Koreaans')],nonadv,[],[]).

a([e('Kosovaarse'),
   no_e('Kosovaars')],nonadv,[],[]).

a([e('Kroatische'),
   no_e('Kroatisch')],nonadv,[],[]).

a([e('Latijnse'),
   er('Latijnser'),
   ere('Latijnsere'),
   no_e('Latijns'),
   st('Latijnst'),
   ste('Latijnste')],nonadv,[],[]).

a([e('Latijns-Amerikaanse'),
   no_e('Latijns-Amerikaans'),
   e('Latijnsamerikaanse'),
   no_e('Latijnsamerikaans')],nonadv,[],[]).

a([both('Leeuwarder')],nonadv,[],[]).

a([e('Leidse'),
   no_e('Leids')],nonadv,[],[]).

a([e('Letse'),
   no_e('Lets')],nonadv,[],[]).

a([e('Leuvense'),
   no_e('Leuvens')],nonadv,[],[]).

a([e('Liberiaanse'),
   no_e('Liberiaans')],nonadv,[],[]).

a([e('Libische'),
   no_e('Libisch')],nonadv,[],[]).

a([e('Libanese'),
   no_e('Libanees')],nonadv,[],[]).

a([e('Limburgse'),
   no_e('Limburgs')],nonadv,[],[]).

a([e('Litouwse'),
   no_e('Litouws')],nonadv,[],[]).

a([e('Londense'),
   no_e('Londens')],nonadv,[],[]).

a([e('Luikse'),
   no_e('Luiks')],nonadv,[],[]).

a([e('Luxemburgse'),
   no_e('Luxemburgs')],nonadv,[],[]).

a([e('Maastrichtse'),
   no_e('Maastrichts')],nonadv,[],[]).

a([e('Macedonische'),
   no_e('Macedonisch')],nonadv,[],[]).

a([e('Madrileense'),
   no_e('Madrileens')],nonadv,[],[]).

a([e('Maleisische'),
   no_e('Maleisisch')],nonadv,[],[]).

a([both('Maltezer')],nonadv,[],[]).

a([e('Marokkaanse'),
   no_e('Marokkaans')],nonadv,[],[]).

a([e('Messiaanse'),
   no_e('Messiaans')],nonadv,[],[]).

a([e('Mexicaanse'),
   no_e('Mexicaans')],nonadv,[],[]).

a([e('Milanese'),
   no_e('Milanees')],nonadv,[],[]).

a([e('Moldavische'),
   no_e('Moldavisch')],nonadv,[],[]).

a([e('Molukse'),
   no_e('Moluks')],nonadv,[],[]).

a([e('Mongoolse'),
   no_e('Mongools')],nonadv,[],[]).

a([e('Montenegrijnse'),
   no_e('Montenegrijs')],nonadv,[],[]).

a([e('Moorse'),
   no_e('Moors')],nonadv,[],[]).

a([e('Moskouse'),
   no_e('Moskous')],nonadv,[],[]).

a([e('Nederduitse'),
   no_e('Nederduits')],adv,[],[]). % de Nederduits Gereformeerde kerk

a([e('Nederlandse'),
   er('Nederlandser'),
   ere('Nederlandsere'),
   no_e('Nederlands'),
   st('Nederlandst'),
   ste('Nederlandste')],adv,[],
  [h('Belgisch'),
   h('Brits'),
   h('Duits'),
   h('Frans'),
   h('Vlaams')]).

a([e('Nedersaksische'),
   no_e('Nedersaksisch')],adv,[],[]).

a([e('Nepalese'),
   no_e('Nepalees')],nonadv,[],[]).

a([e('Newyorkse'),
   no_e('Newyorks'),
   e('New-Yorkse'),
   no_e('New-Yorks'),
   e(['New','Yorkse']),
   no_e(['New','Yorks'])],nonadv,[],[]).

a([e('Nieuwzeelandse'),
   no_e('Nieuwzeelands'),
   e('Nieuw-Zeelandse'),
   no_e('Nieuw-Zeelands')],nonadv,[],[]).

a([e('Nigeriaanse'),
   no_e('Nigeriaans')],nonadv,[],[]).

a([e('Nijmeegse'),
   no_e('Nijmeegs')],nonadv,[],[]).

a([e('Noordafrikaanse'),
   no_e('Noordafrikaans'),
   e('Noord-Afrikaanse'),
   no_e('Noord-Afrikaans')],nonadv,[],[]).

a([e('Noordatlantische'),
   no_e('Noordatlantisch'),
   e('Noord-Atlantische'),
   no_e('Noord-Atlantisch')],nonadv,[],[]).

a([e('Noordamerikaanse'),
   no_e('Noordamerikaans'),
   e('Noord-Amerikaanse'),
   no_e('Noord-Amerikaans')],nonadv,[],[]).

a([e('Noord-Brabantse'),
   no_e('Noord-Brabants')],nonadv,[],[]).

a([e('Noordeuropese'),
   no_e('Noordeuropees'),
   e('Noord-Europese'),
   no_e('Noord-Europees')],adv,[],[]).

a([e('Noordhollandse'),
   no_e('Noordhollands'),
   e('Noord-Hollandse'),
   no_e('Noord-Hollands')],nonadv,[],[]).

a([e('Noord-Ierse'),
   no_e('Noord-Iers'),
   e('Noordierse'),
   no_e('Noordiers')],nonadv,[],[]).

a([e('Noordkoreaanse'),
   no_e('Noordkoreaans'),
   e('Noord-Koreaanse'),
   no_e('Noord-Koreaans')],nonadv,[],[]).

a([e('Noord-Limburgse'),
   no_e('Noord-Limburgs')],nonadv,[],[]).

a([e('Noorse'),
   no_e('Noors')],nonadv,[],[]).

a([e('Oekraïense'),
   no_e('Oekraïens')],nonadv,[],[]).

a([e('Oegandese'),
   no_e('Oegandees')],nonadv,[],[]).

a([e('Oezbeekse'),
   no_e('Oezbeeks')],nonadv,[],[]).

a([e('Oostafrikaanse'),
   no_e('Oostafrikaans'),
   e('Oost-Afrikaanse'),
   no_e('Oost-Afrikaans')],nonadv,[],[]).

a([e('Oostduitse'),
   no_e('Oostduits'),
   e('Oost-Duitse'),
   no_e('Oost-Duits')],nonadv,[],[]).

a([e('Oostenrijkse'),
   no_e('Oostenrijks')],nonadv,[],[]).

a([e('Oosteuropese'),
   no_e('Oosteuropees'),
   e('Oost-Europese'),
   no_e('Oost-Europees')],adv,[],[]).

a([e('Oost-Timorese'),
   no_e('Oost-Timorees'),
   e('Oosttimorese'),
   no_e('Oosttimorees')],nonadv,[],[]).

a([e('Ottomaanse'),
   no_e('Ottomaans')],nonadv,[],[]).

a([e('Overijsselse'),
   no_e('Overijssels')],nonadv,[],[]).

a([e('Pakistaanse'),
   no_e('Pakistaans')],nonadv,[],[]).

a([e('Palestijnse'),
   no_e('Palestijns')],nonadv,[],[h('Israëlisch')]).

a([e('Panamese'),
   no_e('Panamees')],nonadv,[],[]).

a([e('Papendrechtse'),
   no_e('Papendrechts')],nonadv,[],[]).

a([e('Parijse'),
   no_e('Parijs')],nonadv,[],[]).

a([e('Parmezaanse'),
   no_e('Parmezaans')],nonadv,[],[]).

a([e('Pathaanse'),
   no_e('Pathaans')],nonadv,[],[]).

a([e('Peruaanse'),
   no_e('Peruaans')],nonadv,[],[]).

a([e('Perzische'),
   no_e('Perzisch')],nonadv,[],[]).

a([e('Poolse'),
   no_e('Pools')],adv,[],[]).

a([e('Portugese'),
   no_e('Portugees')],nonadv,[],[]).

a([e('Praagse'),
   no_e('Praags')],nonadv,[],[]).

a([e('Pruisische'),
   no_e('Pruisisch')],adv,[],[]).

a([e('Pyreneese'),
   no_e('Pyrenees')],nonadv,[],[]).

a([e('Republikeinse'),
   no_e('Republikeins'),
   e('Republiekeinse'),
   no_e('Republiekeins')],adv,[],[]).

a([e('Rijswijkse'),
   no_e('Rijswijks')],nonadv,[],[]).

a([e('Roemeense'),
   no_e('Roemeens')],adv,[],[]).

a([e('Romaanse'),
   no_e('Romaans')],nonadv,[],[]).

a([e('Romeinse'),
   no_e('Romeins')],nonadv,[],[]).

a([e('Rotterdamse'),
   no_e('Rotterdams')],adv,[],[]).

a([e('Russische'),
   no_e('Russisch')],nonadv,[],[]).

a([e('Rwandese'),
   no_e('Rwandees'),
   e('Ruandese'),
   no_e('Ruandees')],nonadv,[],[]).

a([e('Saksische'),
   no_e('Saksisch')],nonadv,[],[]).

a([e('Saoedische'),
   no_e('Saoedisch'),
   e('Saudische'),
   no_e('Saudisch')],nonadv,[],[]).

a([e('Scandinavische'),
   e('Skandinavische'),
   no_e('Scandinavisch'),
   no_e('Skandinavisch')],adv,[],[]).

a([e('Scheveningse'),
   no_e('Schevenings')],nonadv,[],[]).

a([e('Schiedamse'),
   no_e('Schiedams')],nonadv,[],[]).

a([e('Schotse'),
   no_e('Schots')],nonadv,[],[]).

a([e('Senegalese'),no_e('Senegalees')],nonadv,[],[]).

a([e('Servische'),
   no_e('Servisch')],nonadv,[],[h(pro)]).

a([e('Siamese'),
   no_e('Siamees')],nonadv,[],[]).

a([e('Siberische'),
   no_e('Siberisch')],adv,[],[]).

a([e('Singaporese'),
   no_e('Singaporees')],nonadv,[],[]).

a([e('Slavische'),
   no_e('Slavisch')],nonadv,[],[]).

a([e('Sliedrechtse'),
   no_e('Sliedrechts')],nonadv,[],[]).

a([e('Sloveense'),
   no_e('Sloveens')],nonadv,[],[]).

a([e('Slowaakse'),
   no_e('Slowaaks')],nonadv,[],[]).

a([e('Soedanese'),
   no_e('Soedanees')],nonadv,[],[]).

a([e('Somalische'),
   no_e('Somalisch')],nonadv,[],[]).

a([e('Spaanse'),
   no_e('Spaans')],adv,[],[]).

a([e('Srilankaanse'),
   no_e('Srilankaans')],nonadv,[],[]).

a([e('Sudanese'),
   no_e('Sudanees')],nonadv,[],[]).

a([e('Surinaamse'),
   no_e('Surinaams')],nonadv,[],[]).

a([e('Syrische'),
   no_e('Syrisch')],nonadv,[],[]).

a([e('Taiwanese'),
   no_e('Taiwanees')],nonadv,[],[]).

a([e('Tanzaniaanse'),
   no_e('Tanzaniaans')],nonadv,[],[]).

a([e('Texaanse'),
   no_e('Texaans')],nonadv,[],[]).

a([e('Thaise'),
   no_e('Thais'),
   e('Taise'),
   no_e('Tais')],nonadv,[],[]).

a([e('Tibetaanse'),
   no_e('Tibetaans')],nonadv,[],[]).

a([e('Tilburgse'),
   no_e('Tilburgs')],nonadv,[],[]).

a([e('Togolese'),
   no_e('Togolees')],nonadv,[],[]).

a([e('Trojaanse'),
   no_e('Trojaans')],nonadv,[],[]).

a([e('Tsjechische'),
   no_e('Tsjechisch')],nonadv,[],[]).

a([e('Tsjetsjeense'),
   no_e('Tsetsjeens')],nonadv,[],[]).

a([e('Tunesische'),
   no_e('Tunesisch')],nonadv,[],[]).

a([e('Tukkerse'),
   no_e('Tukkers')],nonadv,[],[]).

a([e('Turkse'),
   no_e('Turks')],nonadv,[],[]).

a([e('Turkmeense'),
   no_e('Turkmeens')],nonadv,[],[]).

a([e('Twentse'),
   no_e('Twents')],adv,[],[]).

a([e('Ugandese'),
   no_e('Ugandees')],nonadv,[],[]).

a([e('Utrechtse'),
   no_e('Utrechts')],nonadv,[],[]).

a([e('Vaticaanse'),
   no_e('Vaticaans')],nonadv,[],[]).

a([e('Veluwse'),
   no_e('Veluws')],nonadv,[],[]).

a([e('Venetiaanse'),
   no_e('Venetiaans')],nonadv,[],[]).

a([e('Venezolaanse'),
   no_e('Venezolaans')],nonadv,[],[]).

a([e('Vietnamese'),
   no_e('Vietnamees')],nonadv,[],[]).

a([e('Victoriaanse'),
   no_e('Victoriaans')],nonadv,[],[]).

a([e('Vlaamse'),
   no_e('Vlaams')],nonadv,[],[]).

a([both('Volendammer')],nonadv,[],[]).

a([e('Volendamse'),
   no_e('Volendams')],nonadv,[],[]).

a([e('Waalse'),
   no_e('Waals')],nonadv,[],[]).

a([e('Waalwijkse'),
   no_e('Waalwijks')],nonadv,[],[]).

a([e('Wageningse'),
   no_e('Wagenings')],nonadv,[],[]).

a([e('Washingtonse'),
   no_e('Washingtons')],nonadv,[],[]).

a([e('Weense'),
   no_e('Weens')],nonadv,[],[]).

a([e('Westafrikaanse'),
   no_e('Westafrikaans'),
   e('West-Afrikaanse'),
   no_e('West-Afrikaans')],nonadv,[],[]).

a([e('Westduitse'),
   no_e('Westduits'),
   e('West-Duitse'),
   no_e('West-Duits')],nonadv,[],[]).

a([e('Westerse'),
   no_e('Westers')],adv,[],
  [h(niet)]).

a([e('Westeuropese'),
   no_e('Westeuropees'),
   e('West-Europese'),
   no_e('West-Europees')],adv,[],[]).

a([e('Westindische'),
   no_e('Westindisch'),
   e('West-Indische'),
   no_e('West-Indisch')],nonadv,[],[]).

a([e('Wit-Russische'),
   no_e('Wit-Russisch'),
   e('Witrussische'),
   no_e('Witrussisch')],nonadv,[],[]).

a([e('Zaanse'),
   no_e('Zaans')],nonadv,[],[]).

a([e('Zaïrese'),
   no_e('Zaïrees')],nonadv,[],[]).

a([e('Zeeuwse'),
   no_e('Zeeuws')],nonadv,[],[]).

a([e('Zimbabwaanse'),
   no_e('Zimbabwaans'),
   e('Zimbabweaanse'),
   no_e('Zimbabweaans')],nonadv,[],[]).

a([e('Zoetermeerse'),
   no_e('Zoetermeers')],nonadv,[],[]).

a([e('Zuidafrikaanse'),
   no_e('Zuidafrikaans'),
   e('Zuid-Afrikaanse'),
   no_e('Zuid-Afrikaans')],nonadv,[],[]).

a([e('Zuidamerikaanse'),
   no_e('Zuidamerikaans'),
   e('Zuid-Amerikaanse'),
   no_e('Zuid-Amerikaans')],nonadv,[],[]).

a([e('Zuideuropese'),
   no_e('Zuideuropees'),
   e('Zuid-Europese'),
   no_e('Zuid-Europees')],adv,[],[]).

a([e('Zuidhollandse'),
   no_e('Zuidhollands'),
   e('Zuid-Hollandse'),
   no_e('Zuid-Hollands')],nonadv,[],[]).

a([e('Zuid-Jemenitische'),
   no_e('Zuid-Jemenitisch'),
   e('Zuidjemenitische'),
   no_e('Zuidjemenitisch')],nonadv,[],[]).

a([e('Zuidkoreaanse'),
   no_e('Zuidkoreaans'),
   e('Zuid-Koreaanse'),
   no_e('Zuid-Koreaans')],nonadv,[],[]).

a([e('Zuidlimburgse'),
   no_e('Zuidlimburgs'),
   e('Zuid-Limburgse'),
   no_e('Zuid-limburgs')],nonadv,[],[]).

a([e('Zweedse'),
   no_e('Zweeds')],nonadv,[],[]).

a([e('Zwitserse'),
   no_e('Zwitsers')],nonadv,[],[]).

a([e('Zwolse'),
   no_e('Zwols')],nonadv,[],[]).

a([pred([geleidelijk,aan])],adv,[],[]).

a([pred_er([geleidelijker,aan])],adv,[],[]).

a([pred([kalm,aan])],adv,[],[]).

a([pred_er([kalmer,aan])],adv,[],[]).

a([pred([rustig,aan])],adv,[],[]).

a([pred_er([rustiger,aan])],adv,[],[]).

a([pred([voorzichtig,aan])],adv,[],[]).

a([pred_er([voorzichtiger,aan])],adv,[],[]).

a([both([a,capella])],adv,[],[]).

a([pred(aan)],nonadv,[],[]).

a([pred([aan,de,bal])],padv,[],[]).

a([pred([aan,de,beurt])],padv,[],[]).

a([pred([aan,de,gang])],nonadv,[],[]).

a([pred([aan,de,hand])],nonadv,[],[]).

a([pred([aan,de,praat])],nonadv,[],[]).

a([ge_both(aanbeden)],adv,[],[]).

a([ge_both(aanbevolen)],adv,
  [so_np],[]).

a([pred([aan,het,hoofd])],nonadv,[pp(van)],[]).

a([e(aanbiddelijke),
   er(aanbiddelijker),
   ere(aanbiddelijkere),
   no_e(aanbiddelijk),
   st(aanbiddelijkst),
   ste(aanbiddelijkste)],padv,[],[]).

a([e(aandachtige),
   er(aandachtiger),
   ere(aandachtigere),
   no_e(aandachtig),
   st(aandachtigst),
   ste(aandachtigste)],adv,[],[]).

a([e(aandoenlijke),
   er(aandoenlijker),
   ere(aandoenlijkere),
   no_e(aandoenlijk),
   st(aandoenlijkst),
   ste(aandoenlijkste)],both,
  [subject_vp],[]).

a([ge_both(aaneengeregen)],nonadv,[],[]).

a([ge_both(aaneengesloten)],nonadv,[],[]).

a([ge_both(aangebeden)],adv,[],[]).

a([ge_e(aangeblafte),
   ge_no_e(aangeblaft)],padv,[],[]).

a([ge_no_e(aangebeld),
   ge_e(aangebelde)],adv,[],[]).

a([ge_both(aangeboden)],adv,
  [so_np,
   fixed([[te,koop]])],[]).

a([ge_both(aangebonden)],adv,[],[]).

a([ge_e(aangeboorde),
   ge_no_e(aangeboord)],adv,[],[]).

a([ge_both(aangeboren)],nonadv,
  [so_np,
   so_np_subject_sbar,
   so_np_subject_vp],[]).

a([ge_e(aangebouwde),
   ge_no_e(aangebouwd)],adv,[],[]).

a([ge_e(aangebrachte),
   ge_no_e(aangebracht)],adv,[],[]).

a([ge_e(aangebrande),
   ge_no_e(aangebrand)],padv,[],[]).

a([ge_both(aangebroken)],adv,[],[]).

a([ge_e(aangedane),
   ge_no_e(aangedaan)],adv,
  [pp(door),
   pp(van),
   so_np],[]).

a([ge_e(aangediende),
   ge_no_e(aangediend)],adv,[],[]).

a([ge_e(aangedikte),
   ge_no_e(aangedikt)],adv,[],[]).

a([ge_both(aangedragen)],adv,[],[]).

a([ge_both(aangedreven)],adv,[],[]).

a([ge_both(aangedrongen)],adv,[],[]).

a([ge_e(aangedrukte),
   ge_no_e(aangedrukt)],adv,[],[]).

a([ge_e(aangeduide),
   ge_no_e(aangeduid)],adv,[],[]).

a([ge_e(aangedurfde),
   ge_no_e(aangedurfd)],adv,[],[]).

a([ge_e(aangegane),
   ge_no_e(aangegaan)],adv,[],[]).

a([ge_both(aangegeven)],adv,[],[]).

a([ge_both(aangegrepen)],adv,[],[]).

a([ge_e(aangegroeide),
   ge_no_e(aangegroeid)],adv,[],[]).

a([ge_e(aangehaalde),
   ge_no_e(aangehaald)],adv,[],[]).

a([ge_both(aangehangen)],adv,[],[]).

a([ge_e(aangeharkte),
   ge_no_e(aangeharkt)],padv,[],[]).

a([ge_both(aangeheven)],adv,[],[]).

a([ge_e(aangeholde),
   ge_no_e(aangehold)],adv,[],[]).

a([ge_e(aangehoorde),
   ge_no_e(aangehoord)],adv,[],[]).

a([ge_both(aangehouden)],padv,[],[]).

a([ge_e(aangejaagde),
   ge_no_e(aangejaagd)],adv,[],[]).

a([ge_e(aangekaarte),
   ge_no_e(aangekaart)],padv,[],[]).

a([ge_both(aangekeken)],adv,[],[]).

a([ge_e(aangeklaagde),
   ge_no_e(aangeklaagd)],adv,[],[]).

a([ge_e(aangeklampte),
   ge_no_e(aangeklampt)],adv,[],[]).

a([ge_e(aangeklede),
   ge_no_e(aangekleed)],adv,
  [pp(door),
   pp(met)],[]).

a([ge_no_e(aangekleefd),
   e(aangekleefde)],adv,[],[]).

a([ge_e(aangeknoopte),
   ge_no_e(aangeknoopt)],adv,[],[]).

a([ge_e(aangekochte),
   ge_no_e(aangekocht)],adv,[],[]).

a([ge_both(aangekomen)],adv,[],[]).

a([ge_e(aangekondigde),
   ge_no_e(aangekondigd)],adv,[],[]).

a([ge_e(aangekweekte),
   ge_no_e(aangekweekt)],adv,[],[]).

a([ge_e(aangelande),
   ge_no_e(aangeland)],adv,[],[]).

a([ge_e(aangeleerde),
   ge_no_e(aangeleerd)],adv,[],[]).

a([ge_both(aangelegen)],nonadv,
  [object_vp],[]).

a([ge_e(aangelegde),
   ge_no_e(aangelegd)],adv,
  [pp(door),
   pp(voor)],[]).

a([ge_e(aangelengde),
   ge_no_e(aangelengd)],adv,[],[]).

a([ge_e(aangeleunde),
   ge_no_e(aangeleund)],padv,[],[]).

a([ge_no_e(aangeleverd),
   e(aangeleverde)],nonadv,[],[]).

a([ge_e(aangelijnde),
   ge_no_e(aangelijnd)],padv,[],[]).

a([ge_e(aangelokte),
   ge_no_e(aangelokt)],adv,[],[]).

a([ge_both(aangelopen)],adv,
  [fixed([[rood]])],[]).

a([ge_e(aangemaakte),
   ge_no_e(aangemaakt)],adv,[],[]).

a([ge_e(aangematigde),
   ge_no_e(aangematigd)],adv,[],[]).

a([ge_e(aangemeerde),
   ge_no_e(aangemeerd)],adv,[],[]).

a([ge_e(aangemelde),
   ge_no_e(aangemeld)],adv,[],[]).

a([ge_e(aangemerkte),
   ge_no_e(aangemerkt)],adv,
  [als_pred],[]).

a([ge_both(aangemeten)],adv,[],[]).

a([ge_e(aangemoedigde),
   ge_no_e(aangemoedigd)],adv,[],[]).

a([ge_e(aangename),
   er(aangenamer),
   ere(aangenamere),
   ge_no_e(aangenaam),
   st(aangenaamst),
   ste(aangenaamste)],adv,
  [pp(voor),
   subject_vp,
   subject_sbar,
   so_np
  ],[]).

a([ge_both(aangenomen)],padv,
  [object_sbar],[]).

a([ge_e(aangepakte),
   ge_no_e(aangepakt)],adv,[],[]).

a([ge_e(aangepaste),
   er(aangepaster),
   ere(aangepastere),
   ge_no_e(aangepast)],adv,
  [pp(aan),
   pp(voor)],[]).

a([ge_e(aangeplakte),
   ge_no_e(aangeplakt)],adv,[],[]).

a([ge_e(aangeplante),
   ge_no_e(aangeplant)],adv,[],[]).

a([ge_e(aangeporde),
   ge_no_e(aangepord)],padv,[],[]).

a([ge_e(aangeprate),
   ge_no_e(aangepraat)],adv,[],[]).

a([ge_both(aangeprezen)],adv,[],[]).

a([ge_e(aangeraakte),
   ge_no_e(aangeraakt)],adv,[],[]).

a([ge_both(aangeraden)],adv,[],[]).

a([ge_e(aangerande),
   ge_no_e(aangerand)],adv,[],[]).

a([ge_both(aangereden)],adv,[],[]).

a([ge_e(aangereikte),
   ge_no_e(aangereikt)],adv,
  [so_np],[]).

a([ge_e(aangerekende),
   ge_no_e(aangerekend)],adv,[],[]).

a([ge_e(aangerichte),
   ge_no_e(aangericht)],adv,[],[]).

a([ge_both(aangeroepen)],adv,[],[]).

a([ge_e(aangeroerde),
   ge_no_e(aangeroerd)],adv,[],[]).

a([ge_e(aangerolde),
   ge_no_e(aangerold)],adv,[],[]).

a([ge_e(aangerukte),
   ge_no_e(aangerukt)],adv,[],[]).

a([ge_e(aangeschafte),
   ge_no_e(aangeschaft)],adv,[],[]).

a([ge_e(aangeschakelde),
   ge_no_e(aangeschakeld)],padv,[],[]).

a([ge_e(aangescherpte),
   ge_no_e(aangescherpt)],nonadv,[],[]).

a([ge_both(aangeschoten)],adv,[],[]).

a([ge_both(aangeschoven)],adv,[],[]).

a([ge_both(aangeschreven)],adv,[],[]).

a([ge_both(aangeslagen)],padv,
  [pp(door)],[]).

a([ge_e(aangesleepte),
   ge_no_e(aangesleept)],adv,[],[]).

a([ge_e(aangeslibde),
   ge_no_e(aangeslibd)],adv,[],[]).

a([ge_both(aangesloten)],adv,[],[]).

a([ge_both(aangesneden)],adv,[],[]).

a([ge_both(aangespannen)],adv,[],[]).

a([ge_e(aangespeelde),
   ge_no_e(aangespeeld)],padv,[],[]).

a([ge_e(aangespoelde),
   ge_no_e(aangespoeld)],adv,[],[]).

a([ge_e(aangespoorde),
   ge_no_e(aangespoord)],adv,[],[]).

a([ge_both(aangesproken)],adv,[],[]).

a([ge_e(aangestaarde),
   ge_no_e(aangestaard)],adv,[],[]).

a([ge_e(aangestelde),
   ge_no_e(aangesteld)],adv,[],[]).

a([ge_e(aangestipte),
   ge_no_e(aangestipt)],adv,[],[]).

a([ge_both(aangestroken)],padv,[],[]).

a([ge_both(aangestoken)],adv,
  [pp(door)],[]).

a([ge_e(aangestormde),
   ge_no_e(aangestormd)],adv,[],[]).

a([ge_e(aangestuurde),
   ge_no_e(aangestuurd)],adv,[],[]).

a([ge_e(aangetaste),
   ge_no_e(aangetast)],adv,[],[]).

a([ge_e(aangetekende),
   ge_no_e(aangetekend)],adv,
  [pp(bij),
   pp(tegen)],[]).

a([ge_e(aangetoonde),
   ge_no_e(aangetoond)],adv,[],[]).

a([ge_both(aangetreden)],adv,[],[]).

a([ge_both(aangetroffen)],adv,[],[]).

a([ge_both(aangetrokken)],adv,[pp(tot)],[]).

a([ge_e(aangetrouwde),
   ge_no_e(aangetrouwd)],adv,
  [],[]).

a([ge_both(aangevallen)],adv,[],[]).

a([ge_both(aangevangen)],adv,[],[]).

a([ge_e(aangevatte),
   ge_no_e(aangevat)],adv,[],[]).

a([ge_e(aangevinkte),
   ge_no_e(aangevinkt)],padv,[],[]).

a([ge_both(aangevlogen)],adv,[],[]).

a([ge_both(aangevochten)],adv,[],[]).

a([ge_e(aangevoelde),
   ge_no_e(aangevoeld)],adv,[],[]).

a([ge_e(aangevoerde),
   ge_no_e(aangevoerd)],adv,[],[]).

a([ge_e(aangevraagde),
   ge_no_e(aangevraagd)],adv,[],[]).

a([ge_both(aangevreten)],adv,[],[]).

a([ge_both(aangevroren)],padv,[],[]).

a([ge_e(aangevulde),
   ge_no_e(aangevuld)],adv,[],[]).

a([ge_e(aangevuurde),
   ge_no_e(aangevuurd)],padv,
  [pp(door)],[]).

a([ge_e(aangewaaide),
   ge_no_e(aangewaaid)],adv,[],[]).

a([ge_e(aangewakkerde),
   ge_no_e(aangewakkerd)],adv,[],[]).

a([ge_e(aangewende),
   ge_no_e(aangewend)],adv,[],[]).

a([ge_both(aangewezen)],adv,
  [er_pp_sbar(op),
   subject_vp,subject_sbar,
   pp(op)],[]).

a([ge_both(aangeworven)],adv,[],[]).

a([ge_both(aangewreven)],adv,[],[]).

a([ge_e(aangezegde),
   ge_no_e(aangezegd)],adv,[],[]).

a([ge_e(aangezette),
   ge_no_e(aangezet)],adv,[],[]).

a([ge_both(aangezeten)],adv,[],[]).

a([ge_both(aangezien)],adv,[],[]).

a([ge_e(aangezochte),
   ge_no_e(aangezocht)],adv,[],[]).

a([ge_both(aangezogen)],padv,[],[]).

a([ge_e(aangezwelde),
   ge_no_e(aangezweld)],padv,[],[]).

a([ge_e(aangezwengelde),
   ge_no_e(aangezwengeld)],nonadv,[],[]).

a([ge_both(aangezwollen)],padv,[],[]).

a([ende(aangrenzende),
   end(aangrenzend)],padv,[],[]).

a([e(aangrijpende),
   er(aangrijpender),
   ere(aangrijpendere),
   no_e(aangrijpend),
   st(aangrijpendst),
   ste(aangrijpendste)],adv,[],[]).

a([e(aanhalige),
   er(aanhaliger),
   ere(aanhaligere),
   no_e(aanhalig),
   st(aanhaligst),
   ste(aanhaligste)],adv,[],[]).

a([e(aanhangige),
   no_e(aanhangig)],nonadv,[],[]).

a([e(aanhankelijke),
   er(aanhankelijker),
   ere(aanhankelijkere),
   no_e(aanhankelijk),
   st(aanhankelijkst),
   ste(aanhankelijkste)],adv,[],[]).

a([ende(aanhoudende),
   er(aanhoudender),
   ere(aanhoudendere),
   end(aanhoudend),
   st(aanhoudendst),
   ste(aanhoudendste)],adv,[],[]).

a([both(aankomend),  % de aankomend kamerleden etc.
   ende(aankomende)],adv,[],[]).

a([e(aanlokkelijke),
   er(aanlokkelijker),
   ere(aanlokkelijkere),
   no_e(aanlokkelijk),
   st(aanlokkelijkst),
   ste(aanlokkelijkste)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([ende(aanmatigende),
   end(aanmatigend)],adv,[],[]).

a([e(aanmerkelijke),
   er(aanmerkelijker),
   ere(aanmerkelijkere),
   no_e(aanmerkelijk),
   st(aanmerkelijkst),
   ste(aanmerkelijkste)],adv,[],[]).

a([e(aannemelijke),
   er(aannemelijker),
   ere(aannemelijkere),
   no_e(aannemelijk),
   st(aannemelijkst),
   ste(aannemelijkste)],nonadv,
  [subject_sbar_no_het,
   subject_vp_no_het],[]).

a([ende(aanpalende),
   end(aanpalend)],padv,[],[]).

a([ge_no_e(aanschouwd),
   ge_e(aanschouwde)],adv,[],[]).

a([e(aanschouwelijke),
   er(aanschouwelijker),
   ere(aanschouwelijkere),
   no_e(aanschouwelijk),
   st(aanschouwelijkst),
   ste(aanschouwelijkste)],adv,[],[]).

a([e(aansprakelijke),
   er(aansprakelijker),
   ere(aansprakelijkere),
   no_e(aansprakelijk),
   st(aansprakelijkst),
   ste(aansprakelijkste)],nonadv,
  [er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([e(aanspreekbare),
   no_e(aanspreekbaar)],nonadv,[],[]).

a([ende(aanstaande),
   no_e(aanstaand)],nonadv,[],[]).

a([e(aanstekelijke),
   er(aanstekelijker),
   ere(aanstekelijkere),
   no_e(aanstekelijk),
   st(aanstekelijkst),
   ste(aanstekelijkste)],adv,[],[]).

a([e(aanstellerige),
   er(aanstelleriger),
   ere(aanstellerigere),
   no_e(aanstellerig),
   st(aanstellerigst),
   ste(aanstellerigste)],adv,[],[]).

a([e(aantoonbare),
   no_e(aantoonbaar)],adv,
  [subject_sbar],[]).

a([e(aantrekkelijke),
   er(aantrekkelijker),
   ere(aantrekkelijkere),
   no_e(aantrekkelijk),
   st(aantrekkelijkst),
   ste(aantrekkelijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(door),
   pp(voor),
   pp(wegens)],[]).

a([ge_e(aanvaarde),
   ge_no_e(aanvaard)],adv,[subject_sbar % het is algemeen aanvaard dat...
                          ],[]).

a([e(aanvaardbare),
   er(aanvaardbaarder),
   ere(aanvaardbaardere),
   no_e(aanvaardbaar),
   st(aanvaardbaarst),
   ste(aanvaardbaarste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(aanvankelijke),
   no_e(aanvankelijk)],adv,[],[]).

a([e(aanvechtbare),
   er(aanvechtbaarder),
   ere(aanvechtbaardere),
   no_e(aanvechtbaar),
   st(aanvechtbaarst),
   ste(aanvechtbaarste)],nonadv,[],[]).

a([e(aanverwante),
   no_e(aanverwant)],nonadv,[],[]).

a([e(aanwezige),
   no_e(aanwezig)],padv,
  [mod_pp(bij),
   mod_pp(in)],[]).

a([e(aanwijsbare),
   no_e(aanwijsbaar)],adv,[],[]).

a([e(aanzienlijke),
   er(aanzienlijker),
   ere(aanzienlijkere),
   no_e(aanzienlijk),
   st(aanzienlijkst),
   ste(aanzienlijkste)],adv,[],[]).

a([e(aardedonkere),
   no_e(aardedonker)],nonadv,[],[]).

a([stof(aarden)],nonadv,[],[]).

a([stof(aardewerk),
   stof(aardewerken)],nonadv,[],[]).

a([e(aardige),
   er(aardiger),
   ere(aardigere),
   no_e(aardig),
   st(aardigst),
   ste(aardigste)],adv,
  [subject_vp,
   subject_vp_sbar,  % aardig om te vermelden is dat ...
   subject_sbar],[]).

a([e(aardse),
   er(aardser),
   ere(aardsere),
   no_e(aards),
   st(aardst),
   ste(aardste)],adv,[],[]).

a([e(abele),
   no_e(abel)],adv,[],[]).

%% iets wiskundigs... abelse groepen
a([e(abelse),
   no_e(abels)],adv,[],[]).

a([e(abjecte),
   no_e(abject)],adv,[],[]).

a([e(abnormale),
   er(abnormaler),
   ere(abnormalere),
   no_e(abnormaal),
   st(abnormaalst),
   ste(abnormaalste)],adv,[],[]).

a([e(abominabele),
   no_e(abominabel)],adv,[],[]).

a([both(aboriginal)],nonadv,[],[]).

a([e(abrupte),
   er(abrupter),
   ere(abruptere),
   no_e(abrupt),
   st(abruptst),
   ste(abruptste)],adv,[],[]).

a([e(absolute),
   no_e(absoluut)],adv,[],[]).

a([e(abstracte),
   e(abstrakte),
   er(abstracter),
   er(abstrakter),
   ere(abstractere),
   ere(abstraktere),
   no_e(abstract),
   no_e(abstrakt),
   st(abstractst),
   st(abstraktst),
   ste(abstractste),
   ste(abstraktste)],adv,[],[]).

a([e(absurde),
   er(absurder),
   ere(absurdere),
   no_e(absurd),
   st(absurdst),
   ste(absurdste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(abusievelijke),
   no_e(abusievelijk)],adv,[],[]).

a([e(academische),
   er(academischer),
   ere(academischere),
   no_e(academisch),
   st(academischt),
   ste(academischte)],adv,[],[]).

a([e(acceptabele),
   er(acceptabeler),
   ere(acceptabelere),
   no_e(acceptabel),
   st(acceptabelst),
   ste(acceptabelste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred(accoord),
   pred(akkoord)],nonadv,
  [pp(met),
   er_pp_sbar(met),
   er_pp_vp(met)],[]).

a([e(accurate),
   e(akkurate),
   er(accurater),
   er(akkurater),
   ere(accuratere),
   ere(akkuratere),
   no_e(accuraat),
   no_e(akkuraat),
   st(accuraatst),
   st(akkuraatst),
   ste(accuraatste),
   ste(akkuraatste)],adv,[],[]).

a([e(achteloze),
   er(achtelozer),
   ere(achtelozere),
   no_e(achteloos),
   st(achteloost),
   ste(achtelooste)],adv,[],[]).

a([e(achtenswaardige),
   er(achtenswaardiger),
   ere(achtenswaardigere),
   no_e(achtenswaardig),
   st(achtenswaardigst),
   ste(achtenswaardigste)],nonadv,[],[]).

a([pred([achter,de,rug])],padv,[],[]).

a([e(achterbakse),
   er(achterbakser),
   ere(achterbaksere),
   no_e(achterbaks),
   st(achterbakst),
   ste(achterbakste)],adv,[],[]).

a([e(achterdochtige),
   er(achterdochtiger),
   ere(achterdochtigere),
   no_e(achterdochtig),
   st(achterdochtigst),
   ste(achterdochtigste)],adv,
  [pp(tegen)],[]).

a([e(achtereenvolgende),
   no_e(achtereenvolgend)],nonadv,[],[]).

a([ge_both(achtergebleven)],adv,
  [pp(bij)],[]).

a([ge_both(achtergehouden)],adv,[],[]).

a([ge_both(achtergelaten)],adv,[],[]).

a([ge_both(achtergelegen)],adv,[],[]).

a([ge_e(achtergestelde),
   ge_no_e(achtergesteld)],adv,[],[]).

a([ge_e(achterhaalde),
   ge_no_e(achterhaald)],adv,[],[]).

a([e(achterlijke),
   er(achterlijker),
   ere(achterlijkere),
   no_e(achterlijk),
   st(achterlijkst),
   ste(achterlijkste)],adv,[],[]).

a([ge_both(achternagezeten)],adv,[],[]).

a([e(achterste),
   no_e(achterst)],nonadv,[],[]).

a([e(achterstallige),
   no_e(achterstallig)],nonadv,[],[]).

a([pred(achterstevoren),
   pred([achterste,voren])],adv,[],[]).

a([ende(achteruitgaande),
   end(achteruitgaand)],adv,[],[]).

a([ge_e(achteruitgegane),
   ge_no_e(achteruitgegaan)],adv,[],[]).

a([ge_e(achtervolgde),
   ge_no_e(achtervolgd)],adv,[],[]).

a([e(achterwaartse),
   postn_no_e(achterwaarts)],diradv,[],[]).

a([e('achttiende-eeuwse'),
   no_e('achttiende-eeuws')],nonadv,[],[]).

a([e(actieve),
   e(aktieve),
   er(actiever),
   er(aktiever),
   ere(actievere),
   ere(aktievere),
   no_e(actief),
   no_e(aktief),
   st(actiefst),
   st(aktiefst),
   ste(actiefste),
   ste(aktiefste)],adv,
  [als_pred,
   pp(binnen),
   pp(in)],[]).

a([e(actuele),
   e(aktuele),
   er(actueler),
   er(aktueler),
   ere(actuelere),
   ere(aktuelere),
   no_e(actueel),
   no_e(aktueel),
   st(actueelst),
   st(aktueelst),
   ste(actueelste),
   ste(aktueelste)],adv,[],[]).

a([e(acute),
   er(acuter),
   ere(acutere),
   no_e(acuut),
   st(acuutst),
   ste(acuutste)],adv,[],[]).

a([both([ad,hoc]),
   both('ad-hoc')],adv,[],[]).

a([both([ad,hominem])],adv,[],[]).

a([pred([ad,rem])],adv,[],[]).

a([e(additionele),
   no_e(additioneel)],nonadv,[],[]).

a([e(adellijke),
   er(adellijker),
   ere(adellijkere),
   no_e(adellijk),
   st(adellijkst),
   ste(adellijkste)],adv,[],[]). % adv: je kunt adellijk trouwen

a([ende(adembenemende),
   er(adembenemender),
   ere(adembenemendere),
   end(adembenemend),
   st(adembenemendst),
   ste(adembenemendste)],adv,[],[]).

a([e(ademloze),
   er(ademlozer),
   ere(ademlozere),
   no_e(ademloos),
   st(ademloost),
   ste(ademlooste)],padv,[],[]).

a([no_e(adequaat),
   e(adequate),
   er(adequater),
   no_e(adekwaat),
   e(adekwate),
   er(adekwater),
   ere(adekwatere),
   ere(adequatere),
   st(adekwaatst),
   st(adequaatst),
   ste(adekwaatste),
   ste(adequaatste)],adv,[],[]).

a([no_e(adjuvant),
   e(adjuvante)],adv,[],[]).

a([e(administratieve),
   no_e(administratief)],adv,[],[]).

a([e(aërobe),
   no_e(aëroob)],nonadv,[],[]).

a([pred(af)],nonadv,[],[]).

a([ge_e(afbetaalde),
   ge_no_e(afbetaald)],adv,[],[]).

a([both(afdoende),
   no_e(afdoend)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(afdwingbare),
   no_e(afdwingaar)],
  [subject_sbar,
   subject_vp],[]).

a([e(affectieve),
   e(affektieve),
   er(affectiever),
   er(affektiever),
   ere(affectievere),
   ere(affektievere),
   no_e(affectief),
   no_e(affektief),
   st(affectiefst),
   st(affektiefst),
   ste(affectiefste),
   ste(affektiefste)],adv,[],[]).

a([ende(afgebakende),
   end(afgebakend)],adv,[],[]).

a([ge_both(afgebakken)],padv,[],[]).

a([ge_e(afgebeelde),
   ge_no_e(afgebeeld)],padv,[],[]).

a([ge_both(afgebeten)],adv,[],[]).

a([ge_e(afgebeulde),
   ge_no_e(afgebeuld)],adv,[],[]).

a([ge_e(afgebladderde),
   ge_no_e(afgebladderd)],adv,[],[]).

a([ge_both(afgeblazen)],padv,[],[]).

a([ge_both(afgebleven)],adv,[],[]).

a([ge_e(afgeblufte),
   ge_no_e(afgebluft)],padv,[],[]).

a([ge_both(afgebogen)],adv,[],[]).

a([ge_both(afgebonden)],adv,[],[]).

a([ge_e(afgebouwde),
   ge_no_e(afgebouwd)],padv,[],[]).

a([ge_e(afgeboorde),
   ge_no_e(afgeboord)],padv,[],[]).

a([ge_e(afgebrachte),
   ge_no_e(afgebracht)],adv,[],[]).

a([ge_e(afgebrande),
   ge_no_e(afgebrand)],adv,[],[]).

a([ge_both(afgebroken)],adv,[],[]).

a([ge_e(afgebrokkelde),
   ge_no_e(afgebrokkeld)],adv,[],[]).

a([ge_e(afgedaalde),
   ge_no_e(afgedaald)],adv,[],[]).

a([ge_e(afgedane),
   ge_no_e(afgedaan)],adv,[],[]).

a([ge_e(afgedankte),
   ge_no_e(afgedankt)],adv,[],[]).

a([ge_e(afgedekte),
   ge_no_e(afgedekt)],adv,[],[]).

a([ge_e(afgedraaide),
   ge_no_e(afgedraaid)],adv,[],[]).

a([ge_both(afgedragen)],adv,[],[]).

a([ge_both(afgedreven)],adv,[],[]).

a([ge_e(afgedroogde),
   ge_no_e(afgedroogd)],adv,[],[]).

a([ge_both(afgedropen)],adv,[],[]).

a([ge_e(afgedrukte),
   ge_no_e(afgedrukt)],adv,[],[]).

a([ge_e(afgedwaalde),
   ge_no_e(afgedwaald)],adv,[],[]).

a([ge_both(afgedwongen)],adv,[],[]).

a([ge_e(afgegane),
   ge_no_e(afgegaan)],adv,[],[]).

a([ge_both(afgegeven)],adv,[],[]).

a([ge_both(afgegleden)],adv,[],[]).

a([ge_both(afgegoten)],nonadv,[],[]).

a([ge_both(afgegraven)],nonadv,[],[]).

a([ge_e(afgehaalde),
   ge_no_e(afgehaald)],adv,[],[]).

a([ge_e(afgegrendelde),
   ge_no_e(afgegrendeld)],nonadv,[],[]).

a([ge_e(afgegooide),
   ge_no_e(afgegooid)],nonadv,[],[]).

a([ge_e(afgehakte),
   ge_no_e(afgehakt)],adv,[],[]).

a([ge_e(afgehandelde),
   ge_no_e(afgehandeld)],adv,[],[]).

a([ge_both(afgehangen)],adv,[],[]).

a([ge_both(afgeholpen)],adv,[],[]).

a([ge_both(afgehouden)],adv,[],[]).

a([ge_both(afgehouwen)],adv,[],[]).

a([ge_e(afgekaderde),
   ge_no_e(afgekaderd)],adv,[],[]).

a([ge_e(afgekeerde),
   ge_no_e(afgekeerd)],adv,[],[]).

a([ge_both(afgekeken)],adv,[],[]).

a([ge_e(afgekeurde),
   ge_no_e(afgekeurd)],adv,[],[]).

a([ge_e(afgekickte),
   ge_no_e(afgekickt)],adv,[],[]).

a([ge_both(afgekloven)],adv,[],[]).

a([ge_e(afgeknaagde),
   ge_no_e(afgeknaagd)],padv,[],[]).

a([ge_e(afgeknapte),
   ge_no_e(afgeknapt)],adv,[],[]).

a([ge_e(afgeknelde),
   ge_no_e(afgekneld)],padv,[],[]).

a([ge_both(afgeknepen)],padv,[],[]).

a([ge_e(afgeknipte),
   ge_no_e(afgeknipt)],adv,[],[]).

a([ge_e(afgekochte),
   ge_no_e(afgekocht)],adv,[],[]).

a([ge_e(afgekoelde),
   ge_no_e(afgekoeld)],adv,[],[]).

a([ge_e(afgekolfde),
   ge_no_e(afgekolfd)],padv,[],[]).

a([ge_both(afgekomen)],adv,[],[]).

a([ge_e(afgekondigde),
   ge_no_e(afgekondigd)],adv,[],[]).

a([ge_e(afgekorte),
   ge_no_e(afgekort)],adv,[],[]).

a([ge_both(afgeladen)],adv,[],[]).

a([ge_e(afgelaste),
   ge_no_e(afgelast)],adv,[],[]).

a([ge_both(afgelaten)],adv,[],[]).

a([ge_e(afgeleefde),
   ge_no_e(afgeleefd)],padv,[],[]).

a([ge_e(afgeleerde),
   ge_no_e(afgeleerd)],adv,[],[]).

a([ge_e(afgelegde),
   ge_no_e(afgelegd)],adv,[],[]).

a([ge_both(afgelegen),
   er(afgelegener),
   ere(afgelegenere),
   st(afgelegenst),
   ste(afgelegenste)],adv,[],[]).

a([ge_e(afgeleide),
   ge_no_e(afgeleid)],padv,[],[]).

a([ge_e(afgeleverde),
   ge_no_e(afgeleverd)],adv,[],[]).

a([ge_both(afgelezen)],adv,[],[]).

a([ge_e(afgelikte),
   ge_no_e(afgelikt)],adv,[],[]).

a([ge_both(afgelopen)],adv,[],[]).

a([ge_e(afgeloste),
   ge_no_e(afgelost)],adv,[],[]).

a([ge_e(afgeluisterde),
   ge_no_e(afgeluisterd)],adv,[],[]).

a([ge_e(afgemaakte),
   ge_no_e(afgemaakt)],adv,[],[]).

a([ge_e(afgemaaide),
   ge_no_e(afgemaaid)],adv,[],[]).

a([ge_e(afgematte),
   ge_no_e(afgemat)],adv,[],[]).

a([ge_e(afgemeerde),
   ge_no_e(afgemeerd)],nonadv,[],[]).

a([ge_both(afgemeten),
   er(afgemetener),
   ere(afgemetenere),
   st(afgemetenst),
   ste(afgemetenste)],adv,[],[]).

a([ge_both(afgenomen)],adv,[],[]).

a([ge_e(afgepakte),
   ge_no_e(afgepakt)],adv,[],[]).

a([ge_e(afgepaste),
   ge_no_e(afgepast)],adv,[],[]).

a([ge_e(afgeplakte),
   ge_no_e(afgeplakt)],adv,[],[]).

a([ge_both(afgeraden)],adv,[],[]).

a([ge_e(afgeranselde),
   ge_no_e(afgeranseld)],adv,[],[]).

a([ge_e(afgereageerde),
   ge_no_e(afgereageerd)],adv,[],[]).

a([ge_both(afgereden)],adv,[],[]).

a([ge_e(afgereisde),
   ge_no_e(afgereisd)],adv,[],[]).

a([ge_e(afgeremde),
   ge_no_e(afgeremd)],adv,[],[]).

a([ge_e(afgerichte),
   ge_no_e(afgericht)],adv,[],[]).

a([ge_both(afgeroepen)],adv,[],[]).

a([ge_e(afgerolde),
   ge_no_e(afgerold)],adv,[],[]).

a([ge_e(afgeronde),
   ge_no_e(afgerond)],adv,[],[]).

a([ge_e(afgeroomde),
   ge_no_e(afgeroomd)],adv,[],[]).

a([ge_e(afgeruimde),
   ge_no_e(afgeruimd)],adv,[],[]).

a([ge_e(afgerukte),
   ge_no_e(afgerukt)],adv,[],[]).

a([ge_e(afgeschafte),
   ge_no_e(afgeschaft)],adv,[],[]).

a([ge_e(afgescheepte),
   ge_no_e(afgescheept)],adv,[],[]).

a([ge_both(afgescheiden)],adv,
  [pp(van)],[]).

a([ge_e(afgeschermde),
   ge_no_e(afgeschermd)],adv,[],[]).

a([ge_e(afgescheurde),
   ge_no_e(afgescheurd)],adv,[],[]).

a([ge_e(afgeschilderde),
   ge_no_e(afgeschilderd)],adv,[],[]).

a([ge_both(afgeschoten)],adv,[],[]).

a([ge_both(afgeschoven)],adv,[],[]).

a([ge_both(afgeschreven)],adv,[],[]).

a([ge_e(afgeschrikte),
   ge_no_e(afgeschrikt)],adv,[],[]).

a([ge_e(afgeschudde),
   ge_no_e(afgeschud)],adv,[],[]).

a([ge_e(afgeslachte),
   ge_no_e(afgeslacht)],adv,[],[]).

a([ge_both(afgeslagen)],adv,[],[]).

a([ge_e(afgeslankte),
   ge_no_e(afgeslankt)],padv,[],[]).

a([ge_both(afgesleten)],adv,[],[]).

a([ge_both(afgesloten),
   er(afgeslotener),
   ere(afgeslotenere),
   st(afgeslotenst),
   ste(afgeslotenste)],adv,
  [pp(met)],[]).

a([ge_both(afgesneden)],adv,[],[]).

a([ge_e(afgespeelde),
   ge_no_e(afgespeeld)],adv,[],[]).

a([ge_e(afgespoelde),
   ge_no_e(afgespoeld)],adv,[],[]).

a([ge_both(afgesproken)],adv,[],[]).

a([ge_both(afgesprongen)],adv,[],[]).

a([ge_e(afgestane),
   ge_no_e(afgestaan)],adv,[],[]).

a([ge_e(afgestamde),
   ge_no_e(afgestamd)],adv,[],[]).

a([ge_e(afgestapte),
   ge_no_e(afgestapt)],adv,[],[]).

a([ge_e(afgestelde),
   ge_no_e(afgesteld)],adv,[],[]).

a([ge_e(afgestemde),
   ge_no_e(afgestemd)],adv,
  [pp(op)],[]).

a([ge_e(afgestofte),
   ge_no_e(afgestoft)],adv,[],[]).

a([ge_both(afgestoken)],adv,[],[]).

a([ge_e(afgestompte),
   ge_no_e(afgestompt)],adv,[],[]).

a([ge_e(afgestormde),
   ge_no_e(afgestormd)],adv,[],[]).

a([ge_both(afgestorven)],adv,[],[]).

a([ge_both(afgestoten)],adv,[],[]).

a([ge_e(afgestrafte),
   ge_no_e(afgestraft)],adv,[],[]).

a([ge_both(afgestreken)],nonadv,[],[]).

a([ge_e(afgestroopte),
   ge_no_e(afgestroopt)],adv,[],[]).

a([ge_e(afgestudeerde),
   ge_no_e(afgestudeerd)],adv,[],[]).

a([ge_e(afgestuurde),
   ge_no_e(afgestuurd)],adv,[],[]).

a([ge_e(afgetaaide),
   ge_no_e(afgetaaid)],padv,[],[]).

a([ge_e(afgetakelde),
   ge_no_e(afgetakeld)],padv,[],[]).

a([ge_e(afgetapte),
   ge_no_e(afgetapt)],adv,[],[]).

a([ge_e(afgetaste),
   ge_no_e(afgetast)],adv,[],[]).

a([ge_e(afgetekende),
   ge_no_e(afgetekend)],adv,[],[]).

a([ge_e(afgetelde),
   ge_no_e(afgeteld)],adv,[],[]).

a([ge_e(afgetimmerde),
   ge_no_e(afgetimmerd)],padv,[],[]).

a([ge_e(afgetrapte),
   ge_no_e(afgetrapt)],padv,[],[]).

a([ge_both(afgetreden)],adv,[],[]).

a([ge_both(afgetrokken)],adv,[],[]).

a([ge_e(afgevaardigde),
   ge_no_e(afgevaardigd)],adv,[],[]).

a([ge_both(afgevallen)],adv,[],[]).

a([ge_e(afgeveegde),
   ge_no_e(afgeveegd)],adv,[],[]).

a([ge_e(afgevoerde),
   ge_no_e(afgevoerd)],adv,[],[]).

a([ge_e(afgevraagde),
   ge_no_e(afgevraagd)],adv,[],[]).

a([ge_e(afgevuurde),
   ge_no_e(afgevuurd)],adv,[],[]).

a([ge_e(afgewachte),
   ge_no_e(afgewacht)],adv,[],[]).

a([ge_both(afgewassen)],adv,[],[]).

a([ge_e(afgeweerde),
   ge_no_e(afgeweerd)],adv,[],[]).

a([ge_both(afgeweken)],adv,[],[]).

a([ge_e(afgewende),
   ge_no_e(afgewend)],padv,[],[]).

a([ge_e(afgewentelde),
   ge_no_e(afgewenteld)],adv,[],[]).

a([ge_e(afgewerkte),
   ge_no_e(afgewerkt)],adv,[pp(met)],[]).

a([ge_both(afgewezen)],adv,[],[]).

a([ge_e(afgewikkelde),
   ge_no_e(afgewikkeld)],adv,[],[]).

a([ge_e(afgewisselde),
   ge_no_e(afgewisseld)],adv,[],[]).

a([ge_e(afgewiste),
   ge_no_e(afgewist)],adv,[],[]).

a([ge_both(afgewogen)],adv,[],[]).

a([ge_both(afgeworpen)],adv,[],[]).

a([ge_e(afgezaagde),
   ge_no_e(afgezaagd)],adv,[],[]).

a([ge_e(afgezakte),
   ge_no_e(afgezakt)],adv,[],[]).

a([ge_e(afgezegde),
   ge_no_e(afgezegd)],adv,[],[]).

a([ge_e(afgezette),
   ge_no_e(afgezet)],adv,[],[]).

a([ge_both(afgezien)],adv,[pp(van),
                           er_pp_sbar(van),
                           object_sbar],[]).

a([ge_e(afgezochte),
   ge_no_e(afgezocht)],adv,[],[]).

a([ge_e(afgezonderde),
   er(afgezonderder),
   ere(afgezonderdere),
   ge_no_e(afgezonderd),
   st(afgezonderdst),
   ste(afgezonderdste)],padv,
  [pp(van)],[]).

a([ge_both(afgezonken)],padv,[],[]).

a([ge_e(afgezwakte),
   ge_no_e(afgezwakt)],adv,[],[]).

a([ge_both(afgezworen)],adv,[],[]).

a([e(afgrijselijke),
   er(afgrijselijker),
   ere(afgrijselijkere),
   no_e(afgrijselijk),
   st(afgrijselijkst),
   ste(afgrijselijkste)],adv,[],[]).

a([e(afgunstige),
   er(afgunstiger),
   ere(afgunstigere),
   no_e(afgunstig),
   st(afgunstigst),
   ste(afgunstigste)],adv,[],[]).

a([e(afhankelijke),
   er(afhankelijker),
   ere(afhankelijkere),
   no_e(afhankelijk),
   st(afhankelijkst),
   ste(afhankelijkste)],adv,
  [er_pp_sbar(van),
   er_pp_vp(van),
   pp(van),
   pp_subject_sbar(van)],  % of hij komt is afhankelijk van ...
  []).

a([e(afhankelijke),
   er(afhankelijker),
   ere(afhankelijkere),
   no_e(afhankelijk),
   st(afhankelijkst),
   ste(afhankelijkste)],adv,
  [],
  [s(inkomen)]).

a([e(afkerige),
   er(afkeriger),
   ere(afkerigere),
   no_e(afkerig),
   st(afkerigst),
   ste(afkerigste)],padv,
  [pp(van)],[]).

a([e(afkomstige),
   postn_no_e(afkomstig)],padv,
  [pp(uit),
   pp(van)],[]).

a([e(afschrikwekkende),
   er(afschrikwekkender),
   ere(afschrikwekkendere),
   no_e(afschrikwekkend),
   st(afschrikwekkendst),
   ste(afschrikwekkendste)],adv,[],[]).

a([e(afschuwelijke),
   er(afschuwelijker),
   ere(afschuwelijkere),
   no_e(afschuwelijk),
   st(afschuwelijkst),
   ste(afschuwelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(afstandelijke),
   er(afstandelijker),
   ere(afstandelijkere),
   no_e(afstandelijk),
   st(afstandelijkst),
   ste(afstandelijkste)],adv,[],[]).

a([e(afstotelijke),
   er(afstotelijker),
   ere(afstotelijkere),
   no_e(afstotelijk),
   st(afstotelijkst),
   ste(afstotelijkste)],nonadv,[],[]).

a([e(aftandse),
   no_e(aftands)],nonadv,[],[]).

a([e(aftrekbare),
   no_e(aftrekbaar)],nonadv,[],[]).

a([e(afvallige),
   no_e(afvallig)],nonadv,[],[]).

a([e(afwezige),
   no_e(afwezig)],padv,[],[]).

a([e(afzichtelijke),
   er(afzichtelijker),
   ere(afzichtelijkere),
   no_e(afzichtelijk),
   st(afzichtelijkst),
   ste(afzichtelijkste)],adv,[],[]).

a([e(afzienbare),
   er(afzienbaarder),
   ere(afzienbaardere),
   no_e(afzienbaar),
   st(afzienbaarst),
   ste(afzienbaarste)],nonadv,[],[]).

a([no_e(afzijdig),
   e(afzijdige)],nonadv,[],[]).

a([e(afzonderlijke),
   er(afzonderlijker),
   ere(afzonderlijkere),
   postn_no_e(afzonderlijk),
   st(afzonderlijkst),
   ste(afzonderlijkste)],adv,[],[]).

a([e(agogische),
   no_e(agogisch)],adv,[],[]).

a([e(agrarische),
   er(agrarischer),
   ere(agrarischere),
   no_e(agrarisch),
   st(agrarischt),
   ste(agrarischte)],nonadv,[],[]).

a([e(agressieve),
   er(agressiever),
   ere(agressievere),
   no_e(agressief),
   st(agressiefst),
   ste(agressiefste)],adv,[],[]).

a([e(aimabele),
   no_e(aimabel)],adv,[],[]).

a([e(akelige),
   er(akeliger),
   ere(akeligere),
   no_e(akelig),
   st(akeligst),
   ste(akeligste)],adv,[],[]).

a([e(akoestische),
   no_e(akoestisch)],adv,[],[]).

a([e(alcolholhoudende),
   no_e(alcoholhoudend),
   e(alkolholhoudende),
   no_e(alkoholhoudend)],nonadv,[],[]).

a([e(alcoholische),
   e(alkoholische),
   er(alcoholischer),
   er(alkoholischer),
   ere(alcoholischere),
   ere(alkoholischere),
   no_e(alcoholisch),
   no_e(alkoholisch),
   st(alcoholischt),
   st(alkoholischt),
   ste(alcoholischte),
   ste(alkoholischte)],nonadv,[],[]).

a([e(alcoholvrije),
   no_e(alcoholvrij)],nonadv,[],[]).

a([e(alerte),
   er(alerter),
   ere(alertere),
   no_e(alert),
   st(alertst),
   ste(alertste)],padv,
  [er_pp_sbar(op),
   er_pp_vp(op),
   pp(op)],[]).

a([prefix(alfa)],nonadv,[],[]).

a([e(alfabetische),
   no_e(alfabetisch)],adv,[],[]).

a([e(algehele),
   no_e(algeheel)],nonadv,[],[]).

a([e(algemene),
   er(algemener),
   ere(algemenere),
   no_e(algemeen),
   st(algemeenst),
   ste(algemeenste)],adv,[],[]).

a([pred([all,out])],nonadv,[],[]).

a([both('all-round'),
   both([all,round]),
   both(allround)],nonadv,[],[]).

a([e(alledaagse),
   er(alledaagser),
   ere(alledaagsere),
   no_e(alledaags),
   st(alledaagst),
   ste(alledaagste)],nonadv,[],[]).

a([pred(alleen)],both,[],[]).

a([e(alleenstaande),
   no_e(alleenstaand)],adv,[],[]).

a([e(allemachtige),
   no_e(allemachtig)],adv,[],[]).

%% not a superlative: een alleraardigst boek
a([e(alleraardigste),no_e(alleraardigst)],adv,[],[]).

%% not a superlative: we werden allervriendelijkst ontvangen
a([e(allervriendelijkste),no_e(allervriendelijkst)],adv,[],[]).

%% not st(), "het afscheid was allerhartelijkst"
a([no_e(allerhartelijkst),e(allerhartelijkste)],adv,[],[]).       

a([e(allergene),
   no_e(allergeen)],nonadv,[],[]).

a([e(allergische),
   er(allergischer),
   ere(allergischere),
   no_e(allergisch),
   st(allergischt),
   ste(allergischte)],padv,
  [er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([both(allerhande)],nonadv,[],[]).

a([e(allesomvattende),
   no_e(allesomvattend)],nonadv,[],[]).

a([e(allesoverheersende),
   no_e(allesoverheersend)],nonadv,[],[]).

a([e(allochtone),
   no_e(allochtoon)],nonadv,[],[]).

a([e(almachtige),
   no_e(almachtig)],nonadv,[],[]).

a([e(alomtegenwoordige),
   no_e(alomtegenwoordig)],nonadv,[],[]).

a([e(alomvattende),
   no_e(alomvattend)],nonadv,[],[]).

a([e(aloude),
   no_e(aloud)],nonadv,[],[]).

a([prefix(alpine)],nonadv,[],[]).

a([e(alpijnse),
   no_e(alpijns)],nonadv,[],[]).

a([pred([als,de,dood])],padv,
  [object_vp,
   object_sbar,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([pred(alsof)],nonadv,[],[]).

%% in misc.pl with_dt
%% a([pred([als,volgt])],adv,[],[]).

a([e(alternatieve),
   no_e(alternatief)],nonadv,[],[]).

a([stof(aluminium)],nonadv,[],[]).

a([e(alwetende),
   no_e(alwetend)],nonadv,[],[]).

a([prefix(amateur)],nonadv,[],[]).

a([e(amateuristische),
   er(amateuristischer),
   ere(amateuristischere),
   no_e(amateuristisch),
   st(amateuristischt),
   ste(amateuristischte)],adv,[],[]).

a([e(ambachtelijke),
   no_e(ambachtelijk)],adv,[],[]).

a([e(ambetante),
   no_e(ambetant)],nonadv,
  [subject_sbar],[]).

a([e(ambigue),
   no_e(ambigu)],nonadv,[],[]).

a([e(ambitieuze),
   er(ambitieuzer),
   ere(ambitieuzere),
   no_e(ambitieus),
   st(ambitieust),
   ste(ambitieuste)],padv,[subject_sbar,
			   subject_vp],[]).

a([e(ambivalente),
   no_e(ambivalent),
   er(ambivalenter),
   ere(ambivalentere)],adv, % ambivalent reageren etc
  [pp(in),
   pp(over)],[]).

a([e(ambtelijke),
   er(ambtelijker),
   ere(ambtelijkere),
   no_e(ambtelijk),
   st(ambtelijkst),
   ste(ambtelijkste)],adv,[],[]).

a([both(ambtshalve)],adv,[],[]).

a([e(ambulante),
   no_e(ambulant)],nonadv,[],[]).

a([e(amechtige),
   no_e(amechtig)],adv,[],[]).

a([e(amfibische),
   no_e(amfibisch)],adv,[],[]).

a([e(amorfe),
   no_e(amorf)],nonadv,[],[]).

a([e(amoureuze),
   er(amoureuzer),
   ere(amoureuzere),
   no_e(amoureus),
   st(amoureust),
   ste(amoureuste)],adv,[],[]).

a([e(ampele),
   no_e(ampel)],nonadv,[],[]).

a([e(amusante),
   er(amusanter),
   ere(amusantere),
   no_e(amusant),
   st(amusantst),
   ste(amusantste)],adv,[],[]).

a([e(anaërobe),
   no_e(anaëroob)],nonadv,[],[]).

a([e(anale),
   no_e(anaal)],adv,[],[]).

a([e(analfabete),
   no_e(analfabeet)],padv,[],[]).

a([e(analoge),
   no_e(analoog)],adv,
  [pp(aan)],[]).

a([e(analytische),
   no_e(analytisch)],adv,[],[]).

a([e(anarchistische),
   er(anarchistischer),
   ere(anarchistischere),
   no_e(anarchistisch),
   st(anarchistischt),
   ste(anarchistischte)],nonadv,[],[]).

a([e(anatomische),
   no_e(anatomisch)],adv,[],[]).

a([er(ander),
   ere(andere)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(anderhalve),
   no_e(anderhalf)],nonadv,[],[]).

a([e(andersdenkende),
   no_e(andersdenkend)],nonadv,[],[]).

a([pred(andersom)],adv,[],[]).

a([e(andersoortige),
   no_e(andersoortig)],nonadv,[],[]).

a([e(androgyne),
   no_e(androgyn)],nonadv,[],[]).

a([e(anekdotische),
   no_e(anekdotisch)],adv,[],[]).

a([no_e(anglicaans),
   e(anglicaanse)],nonadv,[],[]).

a([e(angstaanjagende),
   er(angstaanjagender),
   ere(angstaanjagendere),
   no_e(angstaanjagend),
   st(angstaanjagendst),
   ste(angstaanjagendste)],adv,
  [subject_vp],[]).

a([e(angstige),
   er(angstiger),
   ere(angstigere),
   no_e(angstig),
   st(angstigst),
   ste(angstigste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(angstvallige),
   er(angstvalliger),
   ere(angstvalligere),
   no_e(angstvallig),
   st(angstvalligst),
   ste(angstvalligste)],adv,[],[]).

a([e(angstwekkende),
   er(angstwekkender),
   ere(angstwekkendere),
   no_e(angstwekkend),
   st(angstwekkendst),
   ste(angstwekkendste)],adv,[],[]).

a([e(anonieme),
   er(anoniemer),
   ere(anoniemere),
   no_e(anoniem),
   st(anoniemst),
   ste(anoniemste)],padv,[],[]).

a([e(anorganische),
   no_e(anorganisch)],nonadv,[],[]).

a([e(antieke),
   er(antieker),
   ere(antiekere),
   no_e(antiek),
   st(antiekst),
   ste(antiekste)],nonadv,[],[]).

a([e(antisemitische),
   er(antisemitischer),
   ere(antisemitischere),
   no_e(antisemitisch),
   st(antisemitischt),
   ste(antisemitischte)],adv,[],[]).

a([e(antropologische),
   no_e(antropologisch)],adv,[],[]).

a([e(aparte),
   er(aparter),
   ere(apartere),
   no_e(apart),
   st(apartst),
   ste(apartste)],adv,
  [subject_sbar,
   subject_vp
  ],[]).

a([e(apathische),
   e(apatische),
   er(apathischer),
   er(apatischer),
   ere(apathischere),
   ere(apatischere),
   no_e(apathisch),
   no_e(apatisch),
   st(apathischt),
   st(apatischt),
   ste(apathischte),
   ste(apatischte)],adv,[],[]).

a([e(aperte),
   no_e(apert)],adv,[],[]).

a([e(apocalyptische),
   no_e(apocalyptisch)],nonadv,[],[]).

a([e(apocriefe),
   no_e(apocrief)],nonadv,[],[]).

a([e('a-politieke'),
   e(apolitieke),
   no_e('a-politiek'),
   no_e(apolitiek)],adv,[],[]).

a([e(apollinische),
   no_e(apollinisch)],nonadv,[],[]).

a([e(apostolische),
   no_e(apostolisch)],adv,[],[]).

% VL: aprilse grillen
a([e(aprilse),
   no_e(aprils)],nonadv,[],[]).

a([e(arbeidsintensieve),
   er(arbeidsintensiever),
   ere(arbeidsintensievere),
   no_e(arbeidsintensief),
   st(arbeidsintensiefst),
   ste(arbeidsintensiefste)],nonadv,[],[]).

a([e(arbeidsongeschikte),
   no_e(arbeidsongeschikt)],nonadv,[],[]).

a([e(arbitraire),
   no_e(arbitrair)],adv,[],[]).

a([e(archaïsche),
   no_e(archaïsch)],adv,[],[]).

a([e(archeologische),
   no_e(archeologisch)],adv,[],[]).

a([e(archetypische),
   no_e(archetypisch)],adv,[],[]).

a([e(architectonische),
   e(architektonische),
   no_e(architectonisch),
   no_e(architektonisch)],adv,[],[]).

a([stof(arduinen)],nonadv,[],[]).

a([e(argeloze),
   er(argelozer),
   ere(argelozere),
   no_e(argeloos),
   st(argeloost),
   ste(argelooste)],adv,[],[]).

a([e(argwanende),
   er(argwanender),
   ere(argwanendere),
   no_e(argwanend),
   st(argwanendst),
   ste(argwanendste)],adv,[],[]).

a([e(aristocratische),
   e(aristokratische),
   er(aristocratischer),
   er(aristokratischer),
   ere(aristocratischere),
   ere(aristokratischere),
   no_e(aristocratisch),
   no_e(aristokratisch),
   st(aristocratischt),
   st(aristokratischt),
   ste(aristocratischte),
   ste(aristokratischte)],adv,[],[]).

a([e(arme),
   er(armer),
   ere(armere),
   no_e(arm),
   st(armst),
   ste(armste)],padv,[pp(aan)],[koolstof]).

a([e(armetierige),
   er(armetieriger),
   ere(armetierigere),
   no_e(armetierig),
   st(armetierigst),
   ste(armetierigste)],adv,[],[]).

a([e(armlastige),
   no_e(armlastig)],nonadv,[],[]).

a([e(armoedige),
   er(armoediger),
   ere(armoedigere),
   no_e(armoedig),
   st(armoedigst),
   ste(armoedigste)],adv,[],[]).

a([e(armzalige),
   er(armzaliger),
   ere(armzaligere),
   no_e(armzalig),
   st(armzaligst),
   ste(armzaligste)],adv,[],[]).

a([e(aromatische),
   er(aromatischer),
   ere(aromatischere),
   no_e(aromatisch),
   st(aromatischt),
   ste(aromatischte)],nonadv,[],[]).

a([e(arrogante),
   er(arroganter),
   ere(arrogantere),
   no_e(arrogant),
   st(arrogantst),
   ste(arrogantste)],adv,[],[]).

a([e(artificiële),
   er(artificiëler),
   ere(artificielere),
   no_e(artificieel),
   st(artificieelst),
   ste(artificieelste)],adv,[],[]).

a([e(artistieke),
   er(artistieker),
   ere(artistiekere),
   no_e(artistiek),
   st(artistiekst),
   ste(artistiekste)],adv,[],[]).

a([both(arty)],adv,[],[]).

a([stof(asbest)],nonadv,[],[]).

a([e(ascetische),
   er(ascetischer),
   ere(ascetischere),
   no_e(ascetisch),
   st(ascetischt),
   ste(ascetischte)],adv,[],[]).

a([e(asgrauwe),
   no_e(asgrauw)],both,[],[]).

a([e('a-sociale'),
   e(asociale),
   er('a-socialer'),
   er(asocialer),
   ere('a-socialere'),
   ere(asocialere),
   no_e('a-sociaal'),
   no_e(asociaal),
   st('a-sociaalst'),
   st(asociaalst),
   ste('a-sociaalste'),
   ste(asociaalste)],nonadv,[],[]).

a([e(assertieve),
   no_e(assertief),
   er(assertiever),
   ere(assertievere),
   st(assertiefst),
   ste(assertiefste)],padv,[],[]).

a([e(associatieve),
   no_e(associatief)],adv,[],[]).

a([e(astrologische),
   no_e(astrologisch)],adv,[],[]).

a([prefix(aspirant),
   prefix(adspirant)],nonadv,[],[]).

a([e(astronomische),
   no_e(astronomisch)],adv,[],[]).

a([e(asymmetrische),
   no_e(asymmetrisch)],adv,[],[]).

a([e(asynchrone),
   no_e(asynchroon)],adv,[],[]).

a([e(atletische),
   er(atletischer),
   ere(atletischere),
   no_e(atletisch),
   st(atletischt),
   ste(atletischte)],adv,[],[]).

a([e(atmosferische),
   no_e(atmosferisch)],nonadv,[],[]).

a([e(attente),
   no_e(attent),
   er(attenter),
   ere(attentere),
   st(attentst),
   ste(attentste)],adv,
  [er_pp_sbar(op),
   pp(op)],[]).

a([e(attractieve),
   e(attraktieve),
   er(attractiever),
   er(attraktiever),
   ere(attractievere),
   ere(attraktievere),
   no_e(attractief),
   no_e(attraktief),
   st(attractiefst),
   st(attraktiefst),
   ste(attractiefste),
   ste(attraktiefste)],adv,
  [subject_vp],[]).

a([e(atypische),
   er(atypischer),
   ere(atypischere),
   no_e(atypisch),
   st(atypischt),
   ste(atypischte)],adv,
  [pp(voor),
   subject_sbar],[]).


a([pred([au,sérieux]),
   pred([au,serieux]),
   pred(auserieux),
   pred([au,serieus])],nonadv,[],[]).

a([e(audiovisuele),
   no_e(audiovisueel),
   e('audio-visuele'),
   no_e('audio-visueel')],nonadv,[],[]).

a([e(auditieve),
   er(auditiever),
   ere(auditievere),
   no_e(auditief),
   st(auditiefst),
   ste(auditiefste)],adv,[],[]).

a([e(autentieke),
   e(authentieke),
   er(autentieker),
   er(authentieker),
   ere(autentiekere),
   ere(authentiekere),
   no_e(autentiek),
   no_e(authentiek),
   st(autentiekst),
   st(authentiekst),
   ste(autentiekste),
   ste(authentiekste)],adv,[],[]).

a([e(auteursrechtelijke),
   no_e(auteursrechtelijk)],adv,[],[]).

a([e(autistische),
   no_e(autistisch)],adv,[],[]).

a([e(autobiografische),
   no_e(autobiografisch)],adv,[],[]).

a([e(autochtone),
   er(autochtoner),
   ere(autochtonere),
   no_e(autochtoon),
   st(autochtoonst),
   ste(autochtoonste)],nonadv,[],[]).

a([e(autocratische),
   e(autokratische),
   er(autocratischer),
   er(autokratischer),
   ere(autocratischere),
   ere(autokratischere),
   no_e(autocratisch),
   no_e(autokratisch),
   st(autocratischt),
   st(autokratischt),
   ste(autocratischte),
   ste(autokratischte)],adv,[],[]).

a([e(autoloze),
   no_e(autoloos)],nonadv,[],[]).

a([e(autoluwe),
   no_e(autoluw)],nonadv,[],[]).

a([e(automatische),
   er(automatischer),
   ere(automatischere),
   no_e(automatisch),
   st(automatischt),
   ste(automatischte)],adv,[],[]).

a([er(autonomer),
   ere(autonomere),
   e(autonome),
   no_e(autonoom)],adv,[],[]).

a([e(autoritaire),
   er(autoritairder),
   ere(autoritairdere),
   no_e(autoritair),
   st(autoritairst),
   ste(autoritairste)],adv,[],[]).

a([e(autovrije),
   no_e(autovrij)],adv,[],[]). % je kunt er autovrij recreëren

a([both('avant-garde'),
   both([avant,garde])],nonadv,[],[]).

a([e(averechtse),
   er(averechtser),
   ere(averechtsere),
   no_e(averechts),
   st(averechtst),
   ste(averechtste)],adv,[],[]).

a([e(avondlijke),
   no_e(avondlijk)],nonadv,[],[]).

a([e(avondvullende),
   no_e(avondvullend)],adv,[],[]).

a([e(avontuurlijke),
   er(avontuurlijker),
   ere(avontuurlijkere),
   no_e(avontuurlijk),
   st(avontuurlijkst),
   ste(avontuurlijkste)],adv,[],[]).

a([e(axiale),
   no_e(axiaal)],adv,[],[]).

%% scheikunde
a([no_e(azeotroop),
   no(azeotrope)],nonadv,[],[]).

a([ende(baanbrekende),
   er(baanbrekender),
   ere(baanbrekendere),
   end(baanbrekend),
   st(baanbrekendst),
   ste(baanbrekendste)],nonadv,[],[]).

a([e(baardige),
   er(baardiger),
   ere(baardigere),
   no_e(baardig),
   st(baardigst),
   ste(baardigste)],nonadv,[],[]).

a([e(baarlijke),
   no_e(baarlijk)],nonadv,[],[]).

a([pred(back)],nonadv,[],[]).

a([stof(bakelieten)],nonadv,[],[]).

a([stof(bakstenen)],nonadv,[],[]).

a([e(baldadige),
   er(baldadiger),
   ere(baldadigere),
   no_e(baldadig),
   st(baldadigst),
   ste(baldadigste)],adv,[],[]).

a([stof(bamboe)],nonadv,[],[]).

a([e(banale),
   er(banaler),
   ere(banalere),
   no_e(banaal),
   st(banaalst),
   ste(banaalste)],adv,[],[]).

a([e(bancaire),
   no_e(bancair)],nonadv,[],[inter]).

a([e(bange),
   er(banger),
   ere(bangere),
   no_e(bang),
   st(bangst),
   ste(bangste)],padv,
  [object_sbar,
   object_vp,
   van_sbar,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([no_e(bankroet),
   e(bankroete)],nonadv,[],[]).

a([e(barre),
   er(barder),
   ere(bardere),
   no_e(bar),
   st(barst),
   ste(barste)],adv,[],[]).

a([pred([bar,en,boos])],adv,[],[]).

a([e(barbaarse),
   er(barbaarser),
   ere(barbaarsere),
   no_e(barbaars),
   st(barbaarst),
   ste(barbaarste)],adv,[],[]).

a([e(barmhartige),
   er(barmhartiger),
   ere(barmhartigere),
   no_e(barmhartig),
   st(barmhartigst),
   ste(barmhartigste)],adv,[],[]).

a([e(barokke),
   er(barokker),
   ere(barokkere),
   no_e(barok),
   st(barokst),
   ste(barokste)],nonadv,[],[]).

a([e(barse),
   er(barser),
   ere(barsere),
   no_e(bars),
   st(barst),
   ste(barste)],adv,[],[]).

a([e(basale),no_e(basaal)],adv,[],[]).

a([pred(basic)],nonadv,[],[]).

a([ende(bassende),
   end(bassend)],padv,[],[]).

a([ende(battende),
   end(battend)],padv,[],[]).

a([e(bazige),
   er(baziger),
   ere(bazigere),
   no_e(bazig),
   st(bazigst),
   ste(bazigste)],adv,[],[]).

a([ge_e(beaamde),
   ge_no_e(beaamd)],adv,[],[]).

a([ende(beangstigende),
   end(beangstigend)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(beangstigde),
   ge_no_e(beangstigd)],adv,[],[]).

a([ge_e(beantwoorde),
   ge_no_e(beantwoord)],adv,[],[]).

a([ge_e(beargumenterde),
   no_no_e(beargumenteerd)],adv,[],[]).

a([ge_e(bebaarde),
   ge_no_e(bebaard)],padv,[],[]).

a([ge_e(bebloede),
   ge_no_e(bebloed)],padv,[],[]).

a([ge_e(bebloemde),
   ge_no_e(bebloemd)],padv,[],[]).

a([ge_e(beboste),
   ge_no_e(bebost)],adv,[],[]).

a([ge_e(beboete),
   ge_no_e(beboet)],padv,[],[]).

a([ge_e(beboterde),
   ge_no_e(beboterd)],padv,[],[]).

a([ge_e(bebouwde),
   ge_no_e(bebouwd)],adv,[],[]).

a([ge_e(bebrilde),
   ge_no_e(bebrild)],padv,[],[]).

a([ge_e(becijferde),
   ge_no_e(becijferd)],padv,[],[]).

a([ge_e(becommentarieerde),
   no_no_e(becommentarieerd)],adv,[],[]).

a([ge_e(beconcurreerde),
   ge_no_e(beconcurreerd)],padv,[],[]).

a([ge_e(becritiseerde),
   ge_no_e(becritiseerd)],adv,[],[]).

a([e(bedaagde),
   no_e(bedaagd)],adv,[],[]).

a([ge_e(bedaarde),
   er(bedaarder),
   ere(bedaardere),
   ge_no_e(bedaard),
   st(bedaardst),
   ste(bedaardste)],adv,[],[]).

a([ge_e(bedachte),
   ge_no_e(bedacht)],adv,
  [pp(op),
   er_pp_sbar(op),
   er_pp_vp(op)],[]).

a([e(bedachtzame),
   er(bedachtzamer),
   ere(bedachtzamere),
   no_e(bedachtzaam),
   st(bedachtzaamst),
   ste(bedachtzaamste)],adv,[],[]).

a([ge_e(bedankte),
   ge_no_e(bedankt)],adv,
  [subject_sbar],[]).

a([ge_e(bedeelde),
   ge_no_e(bedeeld)],adv,[],[]).

a([e(bedeesde),
   er(bedeesder),
   ere(bedeesdere),
   no_e(bedeesd),
   st(bedeesdst),
   ste(bedeesdste)],adv,[],[]).

a([ge_e(bedekte),
   ge_no_e(bedekt)],adv,
  [pp(door),
   pp(met)],[]).

a([e(bedenkelijke),
   er(bedenkelijker),
   ere(bedenkelijkere),
   no_e(bedenkelijk),
   st(bedenkelijkst),
   ste(bedenkelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(bederfelijke),
   no_e(bederfelijk)],nonadv,[],[]).

a([ge_e(bediende),
   ge_no_e(bediend)],padv,
  [pp(door)],[]).

a([ge_e(bediscussieerde),
   ge_no_e(bediscussieerd)],nonadv,[],[]).

a([ge_e(bedisselde),
   ge_no_e(bedisseld)],adv,[],[]).

a([ge_e(bedoelde),
   ge_no_e(bedoeld)],padv,
  [pred,
   pp(voor),
   object_vp],[]).

a([ge_both(bedolven)],padv,[],[]).

a([e(bedompte),
   er(bedompter),
   ere(bedomptere),
   no_e(bedompt),
   st(bedomptst),
   ste(bedomptste)],nonadv,[],[]).

a([ge_e(bedonderde),
   ge_no_e(bedonderd)],adv,[],[]).

a([ge_both(bedongen)],adv,[],[]).

a([ge_both(bedorven),
   er(bedorvener),
   ere(bedorvenere),
   st(bedorvenst),
   ste(bedorvenste)],adv,[],[]).

a([ge_e(bedotte),
   no_no_e(bedot)],adv,[],[]).

a([ge_e(bedreigde),
   ge_no_e(bedreigd)],adv,[],[mod_pp(door)]).

a([ende(bedreigende),
   end(bedreigdend),
   ere(bedreigendere),
   er(bedreigender)],adv,[transitive],[]).

a([e(bedremmelde),
   no_e(bedremmeld)],adv,[],[]).

a([ge_both(bedreven),
   ere(bedrevenere),
   er(bedrevener)],adv,[pp(in)],[]).

a([e(bedrieglijke),
   no_e(bedrieglijk)],adv,[],[]).

a([e(bedrijfseconomische),
   e(bedrijfsekonomische),
   no_e(bedrijfseconomisch),
   no_e(bedrijfsekonomisch)],adv,[],[]).

a([e(bedrijfsmatige),
   no_e(bedrijfsmatig)],adv,[],[]).

a([e(bedrijvige),
   er(bedrijviger),
   ere(bedrijvigere),
   no_e(bedrijvig),
   st(bedrijvigst),
   ste(bedrijvigste)],padv,[],[]).

a([ge_e(bedroefde),
   er(bedroefder),
   ere(bedroefdere),
   ge_no_e(bedroefd),
   st(bedroefdst),
   ste(bedroefdste)],both,
  [er_pp_sbar(over),
   er_pp_vp(over),
   pp(over)],[diep]).

a([ge_both(bedrogen)],adv,[],[]).

a([ge_both(bedronken)],adv,[],[]).

a([ge_e(bedrukte),
   er(bedrukter),
   ere(bedruktere),
   ge_no_e(bedrukt),
   st(bedruktst),
   ste(bedruktste)],adv,
  [pp(met)],[]).

a([ge_e(bedruppelde),
   no_no_e(bedruppeld)],adv,[],[]).

a([e(beduchte),
   no_e(beducht),
   er(beduchter),
   ere(beduchtere),
   st(beduchtst),
   ste(beduchtste)],padv,
  [object_vp,
   object_sbar,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([ge_e(beduide),
   ge_no_e(beduid)],adv,[],[]).

a([e(beduidende),
   er(beduidender),
   ere(beduidendere),
   no_e(beduidend),
   st(beduidendst),
   ste(beduidendste)],adv,[],[]).

a([ge_e(beduimelde),
   ge_no_e(beduimeld)],adv,[],[]).

a([e(beduusde),
   er(beduusder),
   ere(beduusdere),
   no_e(beduusd),
   st(beduusdst),
   ste(beduusdste)],padv,
  [er_pp_sbar(van),
   pp(van)],[]).

a([ge_e(beduvelde),
   no_no_e(beduveld)],adv,[],[]).

a([ge_e(bedwelmde),
   ge_no_e(bedwelmd)],adv,[],[]).

a([ge_both(bedwongen)],adv,[],[]).

a([ge_no_e(beëdigd),
   ge_e(beëdigde)],padv,[],[]).

a([ende(beeldende),
   end(beeldend)],adv,[],[]).

a([e(beeldige),
   no_e(beeldig)],adv,[],[]).

a([e(beeldschone),
   er(beeldschoner),
   ere(beeldschonere),
   no_e(beeldschoon),
   st(beeldschoonst),
   ste(beeldschoonste)],nonadv,[],[]).

a([e(beestachtige),
   er(beestachtiger),
   ere(beestachtigere),
   no_e(beestachtig),
   st(beestachtigst),
   ste(beestachtigste)],adv,[],[]).

a([ge_e(beetgepakte),
   ge_no_e(beetgepakt)],adv,[],[]).

a([e(befaamde),
   er(befaamder),
   ere(befaamdere),
   no_e(befaamd),
   st(befaamdst),
   ste(befaamdste)],nonadv,[],[]).

a([e(begaafde),
   er(begaafder),
   ere(begaafdere),
   no_e(begaafd),
   st(begaafdst),
   ste(begaafdste)],padv,[],[hoog,zwak]).

a([ge_e(begane),
   ge_no_e(begaan)],adv,[pp(met)],[]).

a([e(begaanbare),
   no_e(begaanbaar)],nonadv,[],[on]).

a([ge_e(begeerde),
   ge_no_e(begeerd)],adv,[],[]).

a([e(begeerlijke),
   er(begeerlijker),
   ere(begeerlijkere),
   no_e(begeerlijk),
   st(begeerlijkst),
   ste(begeerlijkste)],adv,[],[]).

a([ge_e(begeleide),
   ge_no_e(begeleid)],padv,[],[]).

a([ge_e(begenadigde),
   ge_no_e(begenadigd)],adv,[],[]).

a([e(begerenswaardige),
   er(begerenswaardiger),
   ere(begerenswaardigere),
   no_e(begerenswaardig),
   st(begerenswaardigst),
   ste(begerenswaardigste)],nonadv,[],[]).

a([e(begerige),
   er(begeriger),
   ere(begerigere),
   no_e(begerig),
   st(begerigst),
   ste(begerigste)],adv,[],[]).

a([ge_e(begiftigde),
   ge_no_e(begiftigd)],adv,[],[]).

a([ge_e(begluurde),
   ge_no_e(begluurd)],adv,[],[]).

a([e(begoede),no_e(begoed)],nonadv,[],[]). % VL

a([ge_both(begonnen)],adv,[],[]).

a([ge_both(begoten)],adv,[],[]).

a([ge_e(begraasde),
   no_no_e(begraasd)],adv,[],[]).

a([ge_both(begraven)],adv,[],[]).

a([ge_e(begrensde),
   er(begrensder),
   ere(begrensdere),
   ge_no_e(begrensd),
   st(begrensdst),
   ste(begrensdste)],adv,
  [pp(door)],[]).

a([ge_both(begrepen)],adv,[pp(in)],[]).

a([e(begrijpelijke),
   er(begrijpelijker),
   ere(begrijpelijkere),
   no_e(begrijpelijk),
   st(begrijpelijkst),
   ste(begrijpelijkste)],adv,
  [subject_sbar],[]).

a([ende(begrijpende),
   er(begrijpender),
   ere(begrijpendere),
   end(begrijpend),
   st(begrijpendst),
   ste(begrijpendste)],adv,[transitive],[]).

a([ge_e(begroeide),
   ge_no_e(begroeid)],adv,
  [pp(met)],[]).

a([ge_e(begroete),
   ge_no_e(begroet)],adv,[],[]).

a([ge_e(begrote),
   ge_no_e(begroot)],padv,[],[]).

a([ge_e(begunstigde),
   ge_no_e(begunstigd)],adv,[],[]).

a([ge_no_e(behaagd),
   ge_e(behaagde)],adv,[],[]).

a([e(behaaglijke),
   er(behaaglijker),
   ere(behaaglijkere),
   no_e(behaaglijk),
   st(behaaglijkst),
   ste(behaaglijkste)],adv,[],[]).

a([ge_e(behaalde),
   ge_no_e(behaald)],adv,[],[]).

a([ge_e(behaarde),
   er(behaarder),
   ere(behaardere),
   ge_no_e(behaard),
   st(behaardst),
   ste(behaardste)],padv,[],[]).

a([e(behandelbare),
   no_e(behandelbaar)],adv,[],[on]).

a([ge_e(behandelde),
   ge_no_e(behandeld)],adv,[],[]).

a([ge_both(behangen)],adv,[],[]).

a([ge_e(behartigde),
   ge_no_e(behartigd)],adv,[],[]).

a([ge_e(beheerde),
   ge_no_e(beheerd)],adv,[],[]).

a([e(beheersbare),
   no_e(beheersbaar)],adv,[],[]).

a([ge_e(beheerste),
   er(beheerster),
   ere(beheerstere),
   ge_no_e(beheerst)],adv,
  [pp(door)],[]).

a([ge_e(behekste),
   ge_no_e(behekst)],adv,[],[]).

a([ge_e(behelsde),
   ge_no_e(behelsd)],adv,[],[]).

a([e(behendige),
   er(behendiger),
   ere(behendigere),
   no_e(behendig),
   st(behendigst),
   ste(behendigste)],adv,[],[]).

a([e(behepte),
   no_e(behept)],nonadv,
  [er_pp_sbar(met),
   er_pp_vp(met),
   pp(met)],[]).

a([ge_e(behoede),
   ge_no_e(behoed)],adv,[],[]).

a([e(behoedzame),
   er(behoedzamer),
   ere(behoedzamere),
   no_e(behoedzaam),
   st(behoedzaamst),
   ste(behoedzaamste)],adv,[],[]).

a([ge_e(behoefde),
   ge_no_e(behoefd)],adv,[],[]).

a([e(behoeftige),
   er(behoeftiger),
   ere(behoeftigere),
   no_e(behoeftig),
   st(behoeftigst),
   ste(behoeftigste)],nonadv,[],[]).

a([ge_both(beholpen)],adv,[],[]).

a([ge_no_e(behoord),
   ge_e(behoorde)],adv,[],[]).

a([e(behoorlijke),
   er(behoorlijker),
   ere(behoorlijkere),
   no_e(behoorlijk),
   st(behoorlijkst),
   ste(behoorlijkste)],adv,[],[]).

a([end(behorend),
   ende(behorende)],padv,[pp(bij),
			  pp(in),
			  pp(tot)],[]).

a([ge_both(behouden)],adv,[],[]).

a([ende(behoudende),
   er(behoudender),
   ere(behoudendere),
   end(behoudend),
   st(behoudendst),
   ste(behoudendste)],adv,[],[]).

a([ge_e(behuisde),
   ge_no_e(behuisd)],padv,[],[]).

a([e(behulpzame),
   er(behulpzamer),
   ere(behulpzamere),
   no_e(behulpzaam),
   st(behulpzaamst),
   ste(behulpzaamste)],padv,
  [pp(bij),
   so_np],[]).

a([ende(beidende),
   end(beidend)],adv,[],[]).

a([both(beige)],nonadv,[],[]).

a([ge_e(beijverde),
   ge_no_e(beijverd)],adv,[],[]).

a([ge_e(beïnvloede),
   ge_no_e(beïnvloed)],padv,[],[]).

a([e(bejaarde),
   no_e(bejaard)],nonadv,[],[]).

a([ge_e(bejegende),
   ge_no_e(bejegend)],adv,[],[]).

a([ge_e(bejubelde),
   ge_no_e(bejubeld)],adv,[],[]).

a([e(bekaaide),
   no_e(bekaaid)],adv,[],[]).

a([ge_e(bekabelde),
   ge_no_e(bekabeld)],padv,[],[]).

a([pred(bekaf)],padv,[],[]).

a([e(bekakte),
   no_e(bekakt)],adv,[],[]).

a([ge_e(bekeerde),
   ge_no_e(bekeerd)],adv,[],[]).

a([ge_both(bekeken)],adv,[],[]).

a([e(bekende),
   er(bekender),
   ere(bekendere),
   postn_no_e(bekend),
   st(bekendst),
   ste(bekendste)],adv,
  [subject_sbar_no_het,
   er_pp_sbar(om),
   er_pp_sbar(voor),
   er_pp_vp(om),
   er_pp_vp(voor),
   so_pp(aan),
   pp(met),
   pp(om),
   pp(over),
   pp(van),
   pp_subject_sbar_no_het(van),
   so_pp_subject_sbar_no_het(aan),
   pp(voor),
   so_np
  ],[]).

a([ge_e(bekendgemaakte),
   ge_no_e(bekendgemaakt)],adv,[],[]).

a([ende(bekendstaande),
   end(bekendstaand)],padv,
  [als_pred],[]).

a([ge_e(bekeurde),
   no_no_e(bekeurd)],adv,[],[]).

a([ge_e(beklaagde),
   ge_no_e(beklaagd)],adv,[],[]).

a([ge_e(bekladde),
   ge_no_e(beklad)],padv,[],[]).

a([e(beklagenswaardige),
   er(beklagenswaardiger),
   ere(beklagenswaardigere),
   no_e(beklagenswaardig),
   st(beklagenswaardigst),
   ste(beklagenswaardigste)],adv,[],[]).

a([ge_e(beklede),
   ge_no_e(bekleed)],adv,[],[]).

a([ge_e(beklemde),
   er(beklemder),
   ere(beklemdere),
   ge_no_e(beklemd),
   st(beklemdst),
   ste(beklemdste)],adv,[],[]).

a([e(beklemmende),
   er(beklemmender),
   ere(beklemmendere),
   no_e(beklemmend),
   st(beklemmendst),
   ste(beklemmendste)],adv,[],[]).

a([ge_e(beklemtoonde),
   ge_no_e(beklemtoond)],adv,[],[]).

a([ge_e(beklijfde),
   ge_no_e(beklijfd)],adv,[],[]).

a([ge_both(beklommen)],adv,[],[]).

a([ge_both(beklonken)],padv,[],[]).

a([ge_e(beklopte),
   ge_no_e(beklopt)],adv,[],[]).

a([ge_e(beknelde),
   ge_no_e(bekneld)],nonadv,[],[]).

a([ge_e(beknibbelde),
   no_no_e(beknibbeld)],adv,[],[]).

a([e(beknopte),
   er(beknopter),
   ere(beknoptere),
   no_e(beknopt),
   st(beknoptst),
   ste(beknoptste)],adv,[],[]).

a([ge_e(beknotte),
   ge_no_e(beknot)],adv,[],[]).

a([ge_e(bekochte),
   ge_no_e(bekocht)],adv,[],[]).

a([ge_e(bekogelde),
   ge_no_e(bekogeld)],adv,[],[]).

a([ge_e(bekoelde),
   ge_no_e(bekoeld)],padv,[],[]).

a([ge_e(bekokstoofde),
   ge_no_e(bekokstoofd)],adv,[],[]).

a([ge_both(bekomen)],adv,[],[]).

a([ge_e(bekommerde),
   er(bekommerder),
   ere(bekommerdere),
   ge_no_e(bekommerd),
   st(bekommerdst),
   ste(bekommerdste)],adv,[],[]).

a([ge_e(bekoorde),
   ge_no_e(bekoord)],adv,[],[]).

a([e(bekoorlijke),
   er(bekoorlijker),
   ere(bekoorlijkere),
   no_e(bekoorlijk),
   st(bekoorlijkst),
   ste(bekoorlijkste)],adv,[],[]).

a([ge_e(bekorte),
   ge_no_e(bekort)],adv,[],[]).

a([ge_e(bekostigde),
   ge_no_e(bekostigd)],adv,[],[]).

a([ge_e(bekrachtigde),
   ge_no_e(bekrachtigd)],adv,[],[]).

a([ge_e(bekraste),
   ge_no_e(bekrast)],nonadv,[],[]).

a([ge_e(bekritiseerde),
   ge_no_e(bekritiseerd),
   ge_e(bekritizeerde),
   ge_no_e(bekritizeerd)],adv,[],[]).

a([ge_both(bekrompen),
   er(bekrompener),
   ere(bekrompenere),
   st(bekrompenst),
   ste(bekrompenste)],adv,[],[]).

a([ge_e(bekroonde),
   ge_no_e(bekroond)],adv,[pp(met)],[]).

a([ge_both(bekropen)],adv,[],[]).

a([e(bekwame),
   er(bekwamer),
   ere(bekwamere),
   no_e(bekwaam),
   st(bekwaamst),
   ste(bekwaamste)],adv,
  [pp(in)],[]).

a([ge_e(bekwaamde),
   ge_no_e(bekwaamd)],adv,[],[]).

a([ge_e(belaagde),
   ge_no_e(belaagd)],adv,[],[]).

a([e(belabberde),
   er(belabberder),
   ere(belabberdere),
   no_e(belabberd),
   st(belabberdst),
   ste(belabberdste)],adv,[],[]).

a([e(belachelijke),
   er(belachelijker),
   ere(belachelijkere),
   no_e(belachelijk),
   st(belachelijkst),
   ste(belachelijkste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_both(beladen)],adv,[],[]).

a([ge_e(belande),
   ge_no_e(beland)],adv,[],[]).

a([e(belangeloze),
   er(belangelozer),
   ere(belangelozere),
   no_e(belangeloos),
   st(belangeloost),
   ste(belangelooste)],adv,[],[]).

a([ende(belanghebbende),
   end(belanghebbend)],nonadv,[],[]).

a([e(belangrijke),
   er(belangrijker),
   ere(belangrijkere),
   no_e(belangrijk),
   st(belangrijkst),
   ste(belangrijkste)],adv,
  [subject_sbar_no_het,
   subject_vp_no_het,
   subject_vp_sbar_no_het,
   pp(voor)],[]).

a([e(belangstellende),
   er(belangstellender),
   ere(belangstellendere),
   no_e(belangstellend),
   st(belangstellendst),
   ste(belangstellendste)],adv,[],[]).

a([e(belangwekkende),
   er(belangwekkender),
   ere(belangwekkendere),
   no_e(belangwekkend),
   st(belangwekkendst),
   ste(belangwekkendste)],nonadv,[subject_sbar,
                                  subject_vp],[]).

a([ge_e(belaste),
   ge_no_e(belast)],adv,
  [er_pp_sbar(met),
   er_pp_vp(met),
   pp(met)],[]).

a([e(belastbare),
   no_e(belastbaar)],nonadv,[],[]).

a([ge_e(belasterde),
   ge_no_e(belasterd)],padv,[],[]).

a([e(belastingvrije),
   no_e(belastingvrij)],both,[],[]).

a([ge_e(belazerde),
   ge_no_e(belazerd)],adv,[],[]).

a([ge_both(beleden)],adv,[],[]).

a([ge_e(beledigde),
   ge_no_e(beledigd)],padv,[object_sbar],[]).

a([ge_e(beleefde),
   er(beleefder),
   ere(beleefdere),
   ge_no_e(beleefd),
   st(beleefdst),
   ste(beleefdste)],adv,[subject_vp],[]).

a([ge_e(beleende),
   no_no_e(beleend)],adv,[],[]).

a([ge_e(belegde),
   ge_no_e(belegd)],adv,[],[]).

a([both(belegen)],nonadv,[],[]).

a([ge_e(belegerde),
   ge_no_e(belegerd)],adv,[],[]).

a([e(beleidsmatige),
   no_e(beleidsmatig)],adv,[],[]).

a([ge_e(belemmerde),
   ge_no_e(belemmerd)],adv,[],[]).

a([e(belendende),
   no_e(belendend)],nonadv,[],[]).

a([ge_e(belette),
   ge_no_e(belet)],adv,[],[]).

a([both(belezen)],adv,[],[]).

a([ge_e(belichaamde),
   ge_no_e(belichaamd)],adv,[],[]).

a([ge_e(belichte),
   ge_no_e(belicht)],adv,[],[]).

a([ge_e(beliefde),
   ge_no_e(beliefd)],adv,[],[]).

a([ge_e(beloerde),
   ge_no_e(beloerd)],adv,[],[]).

a([ge_both(belogen)],adv,[],[]).

a([ge_e(beloofde),
   ge_no_e(beloofd)],adv,
  [so_np],[]).

a([ge_e(beloonde),
   ge_no_e(beloond)],adv,[],[]).

a([ge_both(belopen)],adv,[],[]).

a([ge_e(beluisterde),
   ge_no_e(beluisterd)],adv,[],[]).

a([e(beluste),
   no_e(belust)],padv,
  [er_pp_vp(op),
   pp(op)],[]).

a([ge_e(bemaalde),
   no_no_e(bemaald)],adv,[],[]).

a([ge_e(bemachtigde),
   ge_no_e(bemachtigd)],adv,[],[]).

a([ge_e(bemande),
   ge_no_e(bemand)],adv,[],[]).

a([ge_e(bemerkte),
   ge_no_e(bemerkt)],adv,[],[]).

a([ge_e(bemeste),
   no_no_e(bemest)],adv,[],[]).

a([both(bemeten)],nonadv,[],[]).

a([e(bemiddelbare),
   no_e(bemiddelbaar)],nonadv,[],[]).

a([ge_e(bemiddelde),
   ge_no_e(bemiddeld)],adv,[],[]).

a([ge_e(beminde),
   er(beminder),
   ere(bemindere),
   ge_no_e(bemind),
   st(bemindst),
   ste(bemindste)],adv,[],[]).

a([e(beminnelijke),
   er(beminnelijker),
   ere(beminnelijkere),
   no_e(beminnelijk),
   st(beminnelijkst),
   ste(beminnelijkste)],adv,[],[]).

a([e(bemodderde),
   no_e(bemodderd)],adv,[],[]).

a([ende(bemodderende),
   end(bemodderend)],adv,[],[]).

a([ge_e(bemoedigde),
   ge_no_e(bemoedigd)],adv,[],[]).

a([end(bemoedigend),
   ende(bemoedigende)],adv,
  [subject_sbar,
   subject_vp],
  []).

a([ge_e(bemoeide),
   ge_no_e(bemoeid)],adv,[],[]).

a([ge_e(bemoeilijkte),
   ge_no_e(bemoeilijkt)],adv,[],[]).

a([e(bemoste),
   er(bemoster),
   ere(bemostere),
   no_e(bemost)],nonadv,[],[]).

a([ge_e(benadeelde),
   ge_no_e(benadeeld)],adv,[],[]).

a([ge_e(benaderde),
   ge_no_e(benaderd)],adv,[],[]).

a([ge_e(benadrukte),
   ge_no_e(benadrukt)],adv,[],[]).

a([e(benarde),
   er(benarder),
   ere(benardere),
   no_e(benard),
   st(benardst),
   ste(benardste)],nonadv,[],[]).

a([ge_e(benauwde),
   er(benauwder),
   ere(benauwdere),
   ge_no_e(benauwd),
   st(benauwdst),
   ste(benauwdste)],both,
  [pp(voor),
   object_vp,
   object_sbar],[]).

a([e(benauwende),
   er(benauwender),
   ere(benauwendere),
   no_e(benauwend),
   st(benauwendst),
   ste(benauwendste)],adv,[],[]).

a([pred([beneden,de,maat])],nonadv,[],[]).

a([e(benedenste),
   no_e(benedenste)],nonadv,[],[]).

a([both(benepen)],adv,[],[]).

a([ge_e(benevelde),
   ge_no_e(beneveld)],adv,[],[]).

a([ge_e(benieuwde),
   er(benieuwder),
   ere(benieuwdere),
   ge_no_e(benieuwd),
   st(benieuwdst),
   ste(benieuwdste)],padv,
  [object_sbar,
   pp(naar)],[]).

a([e(benige),
   er(beniger),
   ere(benigere),
   no_e(benig),
   st(benigst),
   ste(benigste)],nonadv,[],[links,rechts]).

a([ge_e(benijde),
   ge_no_e(benijd)],adv,[],[]).

a([e(benijdenswaardige),
   er(benijdenswaardiger),
   ere(benijdenswaardigere),
   no_e(benijdenswaardig),
   st(benijdenswaardigst),
   ste(benijdenswaardigste)],adv,[],[]).

a([e(benodigde),
   er(benodigder),
   ere(benodigdere),
   no_e(benodigd),
   st(benodigdst),
   ste(benodigdste)],nonadv,[],[]).

a([ge_e(benoemde),
   ge_no_e(benoemd)],adv,[],[zelf]).

a([ge_both(benomen)],adv,[],[]).

a([ge_e(benute),
   ge_no_e(benut)],adv,[],[]).

a([ge_e(benutte),
   ge_no_e(benut)],adv,[],[]).

a([ge_e(beoefende),
   ge_no_e(beoefend)],adv,[],[]).

a([ge_e(beoogde),
   ge_no_e(beoogd)],adv,[],[]).

a([ge_e(beoordeelde),
   ge_no_e(beoordeeld)],adv,[],[]).

a([ge_e(bepaalde),
   ge_no_e(bepaald)],adv,
  [pp(door)],[wel]).

a([ge_e(bepakte),
   ge_no_e(bepakt)],padv,[],[]).

a([e(bepalende),
   er(bepalender),
   ere(bepalendere),
   no_e(bepalend),
   st(bepalendst),
   ste(bepalendste)],adv,
  [pp(voor),
   transitive,
   subject_vp_no_het,
   subject_sbar_no_het],[alles]).

a([pred([bepakt,en,bezakt])],padv,[],[]).

a([ge_e(beperkte),
   er(beperkter),
   ere(beperktere),
   ge_no_e(beperkt),
   st(beperktst),
   ste(beperktste)],adv,
  [pp(door),
   pp(tot)],[]).

a([ge_e(beplakte),
   ge_no_e(beplakt)],adv,[],[]).

a([ge_e(beplante),
   ge_no_e(beplant)],adv,[],[]).

a([e(bepleisterde),
   no_e(bepleisterd)],nonadv,[],[]).

a([ge_e(bepleite),
   ge_no_e(bepleit)],adv,[],[]).

a([e(bepoederde),
   no_e(bepoederd)],padv,[],[]).

a([ge_e(beprate),
   ge_no_e(bepraat)],adv,[],[]).

a([ge_e(beproefde),
   ge_no_e(beproefd)],adv,[],[]).

a([ge_e(beraamde),
   ge_no_e(beraamd)],adv,[],[]).

a([ge_e(berechte),
   ge_no_e(berecht)],adv,[],[]).

a([ge_both(bereden)],adv,[],[]).

a([ge_e(beredeneerde),
   ge_no_e(beredeneerd)],adv,[],[]).

a([ge_e(bereide),
   er(bereider),
   ere(bereidere),
   ge_no_e(bereid),
   st(bereidst),
   ste(bereidste)],padv,
  [object_vp,
   er_pp_vp(tot),
   pp(door),
   pp(met),
   pp(tot)],[]).

a([e(bereidwillige),
   er(bereidwilliger),
   ere(bereidwilligere),
   no_e(bereidwillig),
   st(bereidwilligst),
   ste(bereidwilligste)],adv,
  [subject_vp],[]).

a([e(bereikbare),
   no_e(bereikbaar)],nonadv,
  [pp(voor)],[]).

a([ge_e(bereikte),
   ge_no_e(bereikt)],adv,[],[]).

a([ge_e(bereisde),
   ge_no_e(bereisd)],adv,[],[]).

a([ge_e(berekende),
   er(berekender),
   ere(berekendere),
   ge_no_e(berekend),
   st(berekendst),
   ste(berekendste)],adv,
  [subject_sbar,
   er_pp_sbar(op),
   er_pp_vp(op),
   pp(op)],[]).

a([e(bergachtige),
   er(bergachtiger),
   ere(bergachtigere),
   no_e(bergachtig),
   st(bergachtigst),
   ste(bergachtigste)],nonadv,[],[]).

a([pred(bergaf)],padv,[],[]).

a([e(bergafwaartse),
   no_e(bergafwaarts)],diradv,[],[]).

a([pred(bergop)],padv,[],[]).

a([e(bergopwaartse),
   no_e(bergopwaarts)],diradv,[],[]).

a([ge_e(berichte),
   ge_no_e(bericht)],adv,[],[]).

a([ge_e(berispte),
   ge_no_e(berispt)],adv,[],[]).

a([ge_e(beroemde),
   er(beroemder),
   ere(beroemdere),
   ge_no_e(beroemd),
   st(beroemdst),
   ste(beroemdste)],adv,
  [er_pp_sbar(om),
   er_pp_vp(om),
   pp(bij),
   pp(door),
   pp(met),
   pp(om),
   pp(onder),
   pp(uit),
   pp(vanwege)],[]).

a([ge_both(beroepen)],adv,[],[]).

a([both(beroeps)],nonadv,[],[]).

a([e(beroepsmatige),
   no_e(beroepsmatig)],adv,[],[]).

a([e(beroerde),
   er(beroerder),
   ere(beroerdere),
   no_e(beroerd),
   st(beroerdst),
   ste(beroerdste)],adv,
  [subject_vp,
   subject_sbar,
   pp(voor),
   er_pp_vp(voor)],[]).

a([ende(berokkende),
   end(berokkend)],adv,[],[]).

a([ge_e(beroofde),
   ge_no_e(beroofd)],adv,[],[]).

a([e(berooide),
   er(berooider),
   ere(berooidere),
   no_e(berooid),
   st(berooidst),
   ste(berooidste)],padv,[],[]).

a([e(berouwvolle),
   er(berouwvoller),
   ere(berouwvollere),
   no_e(berouwvol),
   st(berouwvolst),
   ste(berouwvolste)],padv,[],[]).

a([e(beruchte),
   er(beruchter),
   ere(beruchtere),
   no_e(berucht),
   st(beruchtst),
   ste(beruchtste)],nonadv,
  [er_pp_vp(om),
   pp(om)],[]).

a([e(beschaafde),
   er(beschaafder),
   ere(beschaafdere),
   no_e(beschaafd),
   st(beschaafdst),
   ste(beschaafdste)],adv,[],[]).

a([ge_e(beschaamde),
   er(beschaamder),
   ere(beschaamdere),
   ge_no_e(beschaamd),
   st(beschaamdst),
   ste(beschaamdste)],padv,
  [object_sbar,
   subject_vp,
   pp(om),
   pp(over)],[]).

a([ge_e(beschadigde),
   ge_no_e(beschadigd)],adv,
  [pp(door)],[]).

a([ge_e(beschaduwde),
   ge_no_e(beschaduwd)],adv,[],[]).

a([ende(beschamende),
   er(beschamender),
   ere(beschamendere),
   end(beschamend),
   st(beschamendst),
   ste(beschamendste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([both(bescheiden),
   er(bescheidener),
   ere(bescheidenere),
   st(bescheidenst),
   ste(bescheidenste)],both,[],[]).

a([ge_both(beschenen)],adv,[],[]).

a([ge_e(beschermde),
   ge_no_e(beschermd)],both,[],[]).

a([both(bescheten)],adv,[],[]).

a([e(beschikbare),
   no_e(beschikbaar)],nonadv,
  [object_vp,
   er_pp_vp(voor),
   pp(bij),
   pp(in),
   pp(voor)],[on]).

a([ge_e(beschikte),
   ge_no_e(beschikt)],adv,[],[]).

a([ge_e(beschilderde),
   ge_no_e(beschilderd)],adv,[],[]).

a([ge_e(beschimmelde),
   ge_no_e(beschimmeld)],padv,[],[]).

a([ge_e(beschimpte),
   ge_no_e(beschimpt)],adv,[],[]).

a([both(beschonken)],padv,[],[]).

a([ge_both(beschoten)],adv,[],[]).

a([ge_e(beschouwde),
   ge_no_e(beschouwd)],padv,[als_pred,
                             fixed([svp_pp(op,keper)])],[]).

a([ge_both(beschreven)],adv,[],[]).

a([e(beschroomde),
   er(beschroomder),
   ere(beschroomdere),
   no_e(beschroomd),
   st(beschroomdst),
   ste(beschroomdste)],adv,[],[]).

a([ge_e(beschuldigde),
   ge_no_e(beschuldigd)],adv,[],[]).

a([ge_e(beschutte),
   er(beschutter),
   ere(beschuttere),
   ge_no_e(beschut),
   st(beschutst),
   ste(beschutste)],adv,
  [pp(tegen)],[]).

a([ge_e(besefte),
   ge_no_e(beseft)],adv,[],[]).

a([ge_both(beslagen)],adv,[],[]).

a([ge_e(beslechte),
   ge_no_e(beslecht)],adv,[],[]).

a([ende(beslissende),
   er(beslissender),
   ere(beslissendere),
   end(beslissend),
   st(beslissendst),
   ste(beslissendste)],adv,
  [subject_sbar,
   subject_vp,
   transitive],[]).

a([e(besliste),
   er(beslister),
   ere(beslistere),
   no_e(beslist)],adv,[],[]).

a([ge_both(beslopen)],adv,[],[]).

a([ge_both(besloten)],adv,
  [er_pp_sbar(in),
   pp(in)],[]).

a([e(besluiteloze),
   er(besluitelozer),
   ere(besluitelozere),
   no_e(besluiteloos),
   st(besluiteloost),
   ste(besluitelooste)],padv,
  [pp(over)],[]).

a([ge_e(besmeerde),
   ge_no_e(besmeerd)],adv,[],[]).

a([ge_e(besmette),
   er(besmetter),
   ere(besmettere),
   ge_no_e(besmet),
   st(besmetst),
   ste(besmetste)],adv,
  [pp(met)],[]).

a([e(besmettelijke),
   er(besmettelijker),
   ere(besmettelijkere),
   no_e(besmettelijk),
   st(besmettelijkst),
   ste(besmettelijkste)],nonadv,
  [pp(voor)],[]).

a([ge_e(besmeurde),
   ge_no_e(besmeurd)],adv,[],[]).

a([e(besmuikte),
   no_e(besmuikt)],adv,[],[]).

a([ge_both(besneden)],nonadv,[],[]).

a([e(besneeuwde),
   no_e(besneeuwd)],adv,[],[]).

a([ende(besneeuwende),
   end(besneeuwend)],adv,[],[]).

a([e(besnorde),
   no_e(besnord)],nonadv,[],[]).

a([ge_e(besnuffelde),
   ge_no_e(besnuffeld)],adv,[],[]).

a([ge_e(besodemieterde),
   no_no_e(besodemieterd)],adv,[],[]).

a([ge_e(bespaarde),
   ge_no_e(bespaard)],adv,
  [subject_sbar,
   so_np],[]).

a([ge_both(bespannen)],adv,[],[]).

a([e(bespatte),
   no_e(bespat)],adv,[],[]).

a([ende(bespattende),
   end(bespattend)],adv,[],[]).

a([ge_e(bespeelde),
   ge_no_e(bespeeld)],adv,[],[]).

a([ge_e(bespeurde),
   ge_no_e(bespeurd)],adv,[],[]).

a([ge_e(bespiede),
   ge_no_e(bespied)],adv,[],[]).

a([ge_e(bespioneerde),
   ge_no_e(bespioneerd)],adv,[],[]).

a([ge_e(bespoedigde),
   ge_no_e(bespoedigd)],adv,[],[]).

a([ge_e(bespotte),
   ge_no_e(bespot)],adv,[],[]).

a([ge_both(bespoten)],adv,[],[on]).

a([e(bespottelijke),
   er(bespottelijker),
   ere(bespottelijkere),
   no_e(bespottelijk),
   st(bespottelijkst),
   ste(bespottelijkste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(bespreekbare),
   no_e(bespreekbaar)],nonadv,[subject_sbar],[]).

a([ge_e(besprenkelde),
   ge_no_e(besprenkeld)],adv,[],[]).

a([ge_e(besproeide),
   ge_no_e(besproeid)],adv,[],[]).

a([ge_both(besproken)],adv,[],[]).

a([ge_both(besprongen)],adv,[],[]).

a([ge_e(bespuugde),
   no_no_e(bespuugd)],adv,[],[]).

a([ge_e(bespuwde),
   ge_no_e(bespuwd)],padv,[],[]).

a([e(bestande),
   er(bestander),
   ere(bestandere),
   no_e(bestand),
   st(bestandst),
   ste(bestandste)],nonadv,[pp(tegen)],[]).

%% dat is niet best
%% geen beste beurt
a([e(beste), no_e(best)],adv,[],[]).

a([ge_e(bestede),
   ge_no_e(besteed)],adv,[],[]).

a([e(besteedbare),
   no_e(besteedbaar)],nonadv,[],[]).

a([ge_both(bestegen)],adv,[],[]).

a([ge_e(bestelde),
   ge_no_e(besteld)],adv,[],[]).

a([ge_e(bestemde),
   ge_no_e(bestemd)],padv,
  [pp(voor),
   object_vp],[]).

a([ge_e(bestempelde),
   ge_no_e(bestempeld)],adv,[],[]).

a([e(bestendige),
   er(bestendiger),
   ere(bestendigere),
   no_e(bestendig),
   st(bestendigst),
   ste(bestendigste)],adv,[],[]).

a([ge_e(bestendigde),
   ge_no_e(bestendigd)],adv,[],[]).

a([ge_e(bestierde),
   ge_no_e(bestierd)],padv,[],[]).

a([e(bestofte),
   er(bestofter),
   ere(bestoftere),
   no_e(bestoft),
   st(bestoftst),
   ste(bestoftste)],nonadv,[],[]).

a([ge_both(bestolen)],adv,[],[]).

a([ge_e(bestookte),
   ge_no_e(bestookt)],adv,[],[]).

a([ge_e(bestormde),
   ge_no_e(bestormd)],adv,[],[]).

a([ge_both(bestorven)],adv,[],[]).

a([ge_both(bestoven)],adv,[],[]).

a([ge_e(bestraalde),
   ge_no_e(bestraald)],adv,[],[]).

a([ge_e(bestrafte),
   ge_no_e(bestraft)],adv,[],[on]).

a([ge_both(bestreden)],adv,[],[]).

a([ge_both(bestreken)],adv,[],[]).

a([ge_e(bestrooide),
   ge_no_e(bestrooid)],adv,[],[]).

a([ge_e(bestudeerde),
   ge_no_e(bestudeerd)],adv,[],[]).

a([ge_e(bestuurde),
   ge_no_e(bestuurd)],adv,[],[]).

a([e(bestuurlijke),
   no_e(bestuurlijk)],adv,[],[]).

a([e(bestuurskundige),
   no_e(bestuurskundig)],adv,[],[]).

a([e(bestuursrechtelijke),
   no_e(bestuursrechtelijk)],adv,[],[]).

a([prefix(bèta)],nonadv,[],[]).

a([e(betaalbare),
   no_e(betaalbaar)],adv,[],[]).

a([ge_e(betaalde),
   ge_no_e(betaald)],adv,[],[goed]).

a([ge_e(betaste),
   ge_no_e(betast)],adv,[],[]).

a([ge_e(betegelde),
   ge_no_e(betegeld)],padv,[],[]).

a([ge_e(betekende),
   ge_no_e(betekend)],adv,[],[]).

a([e(betekenisvolle),
   no_e(betekenisvol)],adv,[],[]).

a([ge_e(beteugelde),
   ge_no_e(beteugeld)],adv,[],[]).

a([e(beteuterde),
   er(beteuterder),
   ere(beteuterdere),
   no_e(beteuterd),
   st(beteuterdst),
   ste(beteuterdste)],padv,[],[]).

a([ge_e(betichte),
   ge_no_e(beticht)],adv,[],[]).

a([ge_e(betitelde),
   ge_no_e(betiteld)],adv,[],[]).

a([stof(betonnen)],nonadv,[],[]).

a([ge_e(betoogde),
   ge_no_e(betoogd)],adv,[],[]).

a([ge_e(betoonde),
   ge_no_e(betoond)],adv,[],[]).

a([ge_e(betoverde),
   ge_no_e(betoverd)],adv,[],[]).

a([e(betraande),
   er(betraander),
   ere(betraandere),
   no_e(betraand),
   st(betraandst),
   ste(betraandste)],nonadv,[],[]).

a([ge_e(betrachte),
   ge_no_e(betracht)],adv,[],[]).

a([ge_e(betrapte),
   ge_no_e(betrapt)],padv,[],[]).

a([ge_both(betreden)],adv,[],[]).

a([e(betrekkelijke),
   no_e(betrekkelijk)],adv,[],[]).

a([ge_e(betreurde),
   ge_no_e(betreurd)],adv,[],[]).

a([e(betreurenswaardige),
   er(betreurenswaardiger),
   ere(betreurenswaardigere),
   no_e(betreurenswaardig),
   st(betreurenswaardigst),
   ste(betreurenswaardigste)],nonadv,
  [subject_sbar],[]).

a([ge_both(betroffen)],adv,[],[]).

a([ge_both(betrokken)],adv,
  [pp(bij),
   pp(in)],[]).

a([e(betrouwbare),
   er(betrouwbaarder),
   ere(betrouwbaardere),
   no_e(betrouwbaar),
   st(betrouwbaarst),
   ste(betrouwbaarste)],adv,[],[]).

a([ge_e(betuigde),
   ge_no_e(betuigd)],adv,[],[]).

a([ge_e(betwijfelde),
   ge_no_e(betwijfeld)],adv,[],[]).

a([ge_e(betwiste),
   ge_no_e(betwist)],adv,[],[]).

a([e(betwistbare),
   no_e(betwistbaar)],nonadv,[subject_sbar],[]).

a([e(beursgenoteerde),
   no_e(beursgenoteerd)],nonadv,[],[]).

a([e(beurtelingse),
   no_e(beurtelings)],adv,[],[]).

a([no_e(bevaarbaar),
   e(bevaarbare)],padv,[],[]).

a([ge_both(bevallen)],adv,[],[]).

a([ge_both(bevaren)],padv,[],[]).

a([e(bevallige),
   er(bevalliger),
   ere(bevalligere),
   no_e(bevallig),
   st(bevalligst),
   ste(bevalligste)],adv,[],[]).

a([ge_both(bevangen),
   er(bevangener),
   ere(bevangenere),
   st(bevangenst),
   ste(bevangenste)],adv,[],[]).

a([ge_e(bevatte),
   ge_no_e(bevat)],adv,[],[]).

a([ge_e(beveiligde),
   ge_no_e(beveiligd)],adv,[],[zwaar]).

a([ende(bevelvoerende),
   end(bevelvoerend)],nonadv,[],[]).

a([e(beverige),
   er(beveriger),
   ere(beverigere),
   no_e(beverig),
   st(beverigst),
   ste(beverigste)],adv,[],[]).

a([ge_e(bevestigde),
   ge_no_e(bevestigd)],adv,[],[]).

a([ende(bevestigende),
   er(bevestigender),
   ere(bevestigendere),
   end(bevestigend),
   st(bevestigendst),
   ste(bevestigendste)],adv,[],[]).

a([ge_e(beviste),
   ge_no_e(bevist)],padv,[],[]).

a([ge_e(bevlekte),
   ge_no_e(bevlekt)],adv,[],[]).

a([both(bevlogen)],adv,[],[]).

a([ge_both(bevochten)],adv,[],[]).

a([ge_e(bevochtigde),
   ge_no_e(bevochtigd)],adv,[],[]).

a([e(bevoegde),
   er(bevoegder),
   ere(bevoegdere),
   postn_no_e(bevoegd),
   st(bevoegdst),
   ste(bevoegdste)],padv,
  [object_vp],[]).

a([ge_e(bevoelde),
   ge_no_e(bevoeld)],adv,[],[]).

a([ge_both(bevolen)],adv,[],[]).

a([ge_e(bevolkte),
   er(bevolkter),
   ere(bevolktere),
   ge_no_e(bevolkt),
   st(bevolktst),
   ste(bevolktste)],adv,
  [pp(door)],[dun]).

a([ge_both(bevonden)],adv,[],[]).

a([ge_e(bevoordeelde),
   ge_no_e(bevoordeeld)],adv,[],[]).

a([e(bevooroordeelde),
   er(bevooroordeelder),
   ere(bevooroordeeldere),
   no_e(bevooroordeeld),
   st(bevooroordeeldst),
   ste(bevooroordeeldste)],nonadv,[],[]).

a([ge_e(bevoorrechte),
   ge_no_e(bevoorrecht)],padv,
  [object_sbar,
   object_vp],[]).

a([ge_e(bevorderde),
   ge_no_e(bevorderd)],adv,[],[]).

a([e(bevorderlijke),
   er(bevorderlijker),
   ere(bevorderlijkere),
   no_e(bevorderlijk),
   st(bevorderlijkst),
   ste(bevorderlijkste)],nonadv,
  [pp(voor)],[]).

a([ge_e(bevraagde),
   ge_no_e(bevraagd)],padv,[],[]).

a([ge_e(bevredigde),
   ge_no_e(bevredigd)],adv,[],[]).

a([e(bevredigende),
   er(bevredigender),
   ere(bevredigendere),
   no_e(bevredigend),
   st(bevredigendst),
   ste(bevredigendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(bevreemde),
   ge_no_e(bevreemd)],adv,[],[]).

a([e(bevreesde),
   er(bevreesder),
   ere(bevreesdere),
   no_e(bevreesd),
   st(bevreesdst),
   ste(bevreesdste)],padv,
  [object_sbar,
   van_sbar,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([e(bevriende),
   no_e(bevriend)],nonadv,
  [pp(met)],[]).

a([ge_e(bevrijde),
   ge_no_e(bevrijd)],adv,[pp(van)],[]).

a([ge_e(bevroede),
   ge_no_e(bevroed)],adv,[],[]).

a([ge_both(bevroren)],adv,[],[]).

a([both(bevrozen)],adv,[],[]).

a([ge_e(bevruchte),
   ge_no_e(bevrucht)],adv,[],[]).

a([ge_e(bevuilde),
   ge_no_e(bevuild)],adv,[],[]).

a([ge_e(bewaakte),
   ge_no_e(bewaakt)],adv,[],[]).

a([ge_e(bewaarde),
   ge_no_e(bewaard)],adv,[],[]).

a([ge_e(bewaarheide),
   ge_no_e(bewaarheid)],adv,[],[]).

a([ge_e(bewandelde),
   ge_no_e(bewandeld)],adv,[],[]).

a([ge_e(bewapende),
   ge_no_e(bewapend)],adv,[],[zwaar]).

a([e(beweeglijke),
   er(beweeglijker),
   ere(beweeglijkere),
   no_e(beweeglijk),
   st(beweeglijkst),
   ste(beweeglijkste)],adv,[],[]).

a([ge_e(beweende),
   no_no_e(beweend)],adv,[],[]).

a([ge_e(beweerde),
   ge_no_e(beweerd)],adv,[],[]).

a([e(bewegingloze),
   no_e(bewegingloos)],padv,[],[]).

a([ge_e(bewerkstelligde),
   ge_no_e(bewerkstelligd)],adv,[],[]).

a([ge_e(bewerkte),
   ge_no_e(bewerkt)],adv,[],[]).

a([ge_both(bewezen)],nonadv,
  [subject_sbar_no_het],[]).

a([ge_no_e(bewierookt),
   ge_e(bewierookte)],padv,[],[]).

a([ge_both(bewogen),
   er(bewogener),
   ere(bewogenere),
   st(bewogenst),
   ste(bewogenste)],adv,[],[]).

a([ge_e(bewolkte),
   er(bewolkter),
   ere(bewolktere),
   ge_no_e(bewolkt),
   st(bewolktst),
   ste(bewolktste)],adv,[],[half,
			    licht,
			    zwaar]).

a([ge_e(bewonderde),
   ge_no_e(bewonderd)],adv,[],[]).

a([e(bewonderenswaardige),
   er(bewonderenswaardiger),
   ere(bewonderenswaardigere),
   no_e(bewonderenswaardig),
   st(bewonderenswaardigst),
   ste(bewonderenswaardigste)],adv,[],[]).

a([e(bewoonbare),
   no_e(bewoonbaar)],nonadv,[],[]).

a([ge_e(bewoonde),
   ge_no_e(bewoond)],adv,[],[]).

a([e(bewuste),
   er(bewuster),
   ere(bewustere),
   no_e(bewust)],both,
  [refl,
   refl_np,
   refl_sbar,
   refl_vp,
   refl_pp(van),
   refl_er_pp_sbar(van),
   refl_er_pp_vp(van),
   er_pp_sbar(van),
   er_pp_vp(van),
   pp(van)
  ],[wel]).

a([e(bewuste),
   er(bewuster),
   ere(bewustere),
   no_e(bewust)],both,
  [],[milieu]).

a([e(bewusteloze),
   no_e(bewusteloos)],padv,[],[]).

a([ge_e(bezaaide),
   ge_no_e(bezaaid)],adv,[],[]).

a([e(bezadigde),
   er(bezadigder),
   ere(bezadigdere),
   no_e(bezadigd),
   st(bezadigdst),
   ste(bezadigdste)],adv,[],[]).

a([ge_e(bezeerde),
   ge_no_e(bezeerd)],adv,[],[]).

a([ge_e(bezegelde),
   ge_no_e(bezegeld)],adv,[],[]).

a([ge_e(bezette),
   ge_no_e(bezet)],padv,
  [pp(door)],[]).

a([ge_both(bezeten)],padv,
  [er_pp_sbar(van),
   er_pp_vp(van),
   pp(van)],[]).

a([ge_e(bezichtigde),
   ge_no_e(bezichtigd)],adv,[],[]).

a([ge_e(bezielde),
   er(bezielder),
   ere(bezieldere),
   ge_no_e(bezield),
   st(bezieldst),
   ste(bezieldste)],adv,[],[]).

a([ge_no_e(bezien),
   ge_e(beziene)],padv,[],[]).

a([e(bezienswaardige),
   er(bezienswaardiger),
   ere(bezienswaardigere),
   no_e(bezienswaardig),
   st(bezienswaardigst),
   ste(bezienswaardigste)],nonadv,[],[]).

a([e(bezige),
   er(beziger),
   ere(bezigere),
   no_e(bezig),
   st(bezigst),
   ste(bezigste)],padv,
  [object_vp,
   er_pp_vp(met),
   pp(aan),
   pp(met)],[]).

a([ge_both(beziggehouden)],adv,[],[]).

a([ge_e(bezochte),
   ge_no_e(bezocht)],adv,[],[druk]).

a([ge_e(bezoedelde),
   ge_no_e(bezoedeld)],adv,[],[]).

a([ge_e(bezoldigde),
   ge_no_e(bezoldigd)],adv,[],[]).

a([ge_e(bezondigde),
   ge_no_e(bezondigd)],adv,[],[]).

a([ge_both(bezongen)],adv,[],[]).

a([ge_both(bezonken)],adv,[],[]).

a([ge_both(bezonnen)],adv,[],[]).

a([ge_both(bezopen)],adv,[],[]).

a([ge_e(bezorgde),
   er(bezorgder),
   ere(bezorgdere),
   ge_no_e(bezorgd),
   st(bezorgdst),
   ste(bezorgdste)],padv,
  [object_sbar,
   pp(om),
   pp(over)],[]).

a([ge_e(bezuinigde),
   ge_no_e(bezuinigd)],adv,[],[]).

a([ge_e(bezwaarde),
   ge_no_e(bezwaard)],adv,[],[]).

a([e(bezwaarlijke),
   er(bezwaarlijker),
   ere(bezwaarlijkere),
   no_e(bezwaarlijk),
   st(bezwaarlijkst),
   ste(bezwaarlijkste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(bezwangerde),
   ge_no_e(bezwangerd)],padv,
  [pp(van)],[]).

a([ge_e(bezwete),
   er(bezweter),
   ere(bezwetere),
   ge_no_e(bezweet),
   st(bezweetst),
   ste(bezweetste)],adv,[],[]).

a([ge_both(bezweken)],adv,[],[]).

a([ge_both(bezworen)],adv,[],[]).

a([ge_e(beëindigde),
   ge_no_e(beëindigd)],adv,[],[]).

a([ge_e(beïnvloede),
   ge_no_e(beïnvloed)],adv,[],[]).

a([e(bibliografische),
   no_e(bibliografisch)],nonadv,[],[]).

a([e(bijbehorende),
   no_e(bijbehorend)],nonadv,[],[]).

a([e(bijbelse),
   er(bijbelser),
   ere(bijbelsere),
   no_e(bijbels),
   st(bijbelst),
   ste(bijbelste)],nonadv,[],[]).

a([e(bijdehante),
   no_e(bijdehand),
   er(bijdehanter),
   ere(bijdehantere),
   st(bijdehantst),
   ste(bijdehantste),
   st(bijdehandst),
   ste(bijdehandste)],adv,[],[]).

a([pred(bijeen)],adv,[],[]).

a([ge_e(bijeengebrachte),
   ge_no_e(bijeengebracht)],adv,[],[]).

a([ge_both(bijeengehouden)],adv,[],[]).

a([ge_both(bijeengekomen)],adv,[],[]).

a([ge_both(bijeengenomen)],adv,[fixed([acc])],[]).

a([ge_both(bijeengeroepen)],adv,[],[]).

a([end(bijgaand),ende(bijgaande)],padv,[],[]).

a([ge_both(bijgebleven)],adv,[],[]).

a([ge_e(bijgeboekte),
   ge_no_e(bijgeboekt)],padv,[],[]).

a([ge_e(bijgebrachte),
   ge_no_e(bijgebracht)],adv,[],[]).

a([ge_both(bijgedragen)],adv,[],[]).

a([ge_both(bijgehouden)],adv,[],[]).

a([ge_both(bijgekomen)],adv,[],[]).

a([ge_e(bijgelegde),
   ge_no_e(bijgelegd)],adv,[],[]).

a([both(bijgelegen)],nonadv,[],[]).

a([e(bijgelovige),
   er(bijgeloviger),
   ere(bijgelovigere),
   no_e(bijgelovig),
   st(bijgelovigst),
   ste(bijgelovigste)],adv,[],[]).

a([ge_e(bijgenaamde),
   ge_no_e(bijgenaamd)],nonadv,
  [transitive],[]).

a([ge_e(bijgepaste),
   ge_no_e(bijgepast)],adv,[],[]).

a([ge_both(bijgeschreven)],nonadv,[],[]).

a([ge_both(bijgesloten)],padv,[],[]).

a([ge_e(bijgestane),
   ge_no_e(bijgestaan)],adv,[],[]).

a([ge_e(bijgestelde),
   ge_no_e(bijgesteld)],adv,[],[]).

a([ge_e(bijgestuurde),
   ge_no_e(bijgestuurd)],adv,[],[]).

a([ge_both(bijgevallen)],adv,[],[]).

a([ge_e(bijgevoegde),
   ge_no_e(bijgevoegd)],adv,[],[]).

a([ge_e(bijgevulde),
   ge_no_e(bijgevuld)],adv,[],[]).

a([ge_e(bijgewerkte),
   ge_no_e(bijgewerkt)],adv,[],[]).

a([ge_e(bijgewoonde),
   ge_no_e(bijgewoond)],adv,[],[]).

a([ge_e(bijgezette),
   ge_no_e(bijgezet)],adv,[],[]).

a([e(bijkomstige),
   no_e(bijkomstig)],nonadv,[],[]).

a([ende(bijpassende),
   er(bijpassender),
   ere(bijpassendere),
   end(bijpassend),
   st(bijpassendst),
   ste(bijpassendste)],adv,[],[]).

a([both(bijster)],oadv,[],[]).

a([ende(bijtende),
   er(bijtender),
   ere(bijtendere),
   end(bijtend),
   st(bijtendst),
   ste(bijtendste)],adv,[],[]).

a([e(bijtgrage),
   no_e(bijtgraag)],nonadv,[],[]).

a([ge_e(bijverdiende),
   ge_no_e(bijverdiend)],nonadv,[],[]).

a([e(bijvoeglijke),
   no_e(bijvoeglijk)],adv,[],[]).

a([ende(bijziende),
   er(bijziender),
   ere(bijziendere),
   end(bijziend),
   st(bijziendst),
   ste(bijziendste)],nonadv,[],[]).

a([e(bijzondere),
   er(bijzonderder),
   ere(bijzonderdere),
   no_e(bijzonder),
   st(bijzonderst),
   ste(bijzonderste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred([bij,de,beesten,af])],nonadv,[],[]).

a([pred([bij,machte])],padv,
  [object_vp],[]).

a([pred([bij,toverslag])],adv,[],[]).

a([e(bikkelharde),
   no_e(bikkelhard)],adv,[],[]).

a([e(bilaterale),
   no_e(bilateraal)],adv,[],[]).

a([e(billijke),
   er(billijker),
   ere(billijkere),
   no_e(billijk),
   st(billijkst),
   ste(billijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(binaire),no_e(binair)],adv,[],[]).

a([ende(bindende),
   er(bindender),
   ere(bindendere),
   end(bindend),
   st(bindendst),
   ste(bindendste)],adv,[],[]).

a([ge_e(binnengebrachte),
   ge_no_e(binnengebracht)],adv,[],[]).

a([ge_both(binnengedrongen)],adv,[],[]).

a([ge_e(binnengegane),
   ge_no_e(binnengegaan)],adv,[],[]).

a([ge_e(binnengehaalde),
   ge_no_e(binnengehaald)],adv,[],[]).

a([ge_both(binnengekomen)],adv,[],[]).

a([ge_both(binnengekregen)],adv,[],[]).

a([ge_both(binnengelaten)],adv,[],[]).

a([ge_e(binnengeleide),
   ge_no_e(binnengeleid)],adv,[],[]).

a([ge_both(binnengelopen)],adv,[],[]).

a([ge_both(binnengeslagen)],nonadv,[],[]).

a([ge_both(binnengeslopen)],adv,[],[]).

a([ge_e(binnengestapte),
   ge_no_e(binnengestapt)],adv,[],[]).

a([ge_e(binnengestormde),
   ge_no_e(binnengestormd)],adv,[],[]).

a([ge_e(binnengestroomde),
   ge_no_e(binnengestroomd)],adv,[],[]).

a([ge_both(binnengetreden)],adv,[],[]).

a([ge_both(binnengetrokken)],adv,[],[]).

a([ge_both(binnengevallen)],adv,[],[]).

a([e(binnenlandse),
   no_e(binnenlands)],adv,[],[]).

a([e(binnenste),
   no_e(binnenste)],nonadv,[],[]).

a([pred(binnenstebuiten),
   pred([binnenste,buiten])],adv,[],[]).

a([e(binnenwaartse),
   postn_no_e(binnenwaarts)],diradv,[],[]).

a([e(biochemische),
   no_e(biochemisch)],adv,[],[]).

a([e(biogene),
   no_e(biogeen)],adv,[],[a]).

a([e(biografische),
   no_e(biografisch)],nonadv,[],[]).

a([e(biologische),
   no_e(biologisch)],adv,[],[]).

a([e(biometrische),
   no_e(biometrisch)],adv,[],[]).

a([e(bipolaire),
   no_e(bipolair)],nonadv,[],[]).

a([e(bisschoppelijke),
   no_e(bisschoppelijk)],nonadv,[],[]).

a([e(bitse),
   er(bitser),
   ere(bitsere),
   no_e(bits),
   st(bitst),
   ste(bitste)],adv,[],[]).

a([e(bittere),
   er(bitterder),
   ere(bitterdere),
   no_e(bitter),
   st(bitterst),
   ste(bitterste)],adv,[],[]).

a([e(bitterzoete),
   no_e(bitterzoet)],nonadv,[],[]).

a([e(bizarre),
   er(bizarder),
   ere(bizardere),
   no_e(bizar),
   st(bizarst),
   ste(bizarste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(blanco)],adv,[],[]).

a([e(blanke),
   er(blanker),
   ere(blankere),
   no_e(blank),
   st(blankst),
   ste(blankste)],nonadv,[],[]).

a([both(blasé)],padv,[],[]).

a([e(blauwe),
   er(blauwer),
   ere(blauwere),
   no_e(blauw),
   st(blauwst),
   ste(blauwste)],nonadv,[],[]).

a([e(blauwachtige),
   er(blauwachtiger),
   ere(blauwachtigere),
   no_e(blauwachtig),
   st(blauwachtigst),
   ste(blauwachtigste)],nonadv,[],[]).

a([e('blauw-grijze'),
   e(blauwgrijze),
   er('blauw-grijzer'),
   er(blauwgrijzer),
   ere('blauw-grijzere'),
   ere(blauwgrijzere),
   no_e('blauw-grijs'),
   no_e(blauwgrijs),
   st('blauw-grijst'),
   st(blauwgrijst),
   ste('blauw-grijste'),
   ste(blauwgrijste)],nonadv,[],[]).

a([e(bleke),
   er(bleker),
   ere(blekere),
   no_e(bleek),
   st(bleekst),
   ste(bleekste)],padv,[],[]).

a([pred(bleekjes)],adv,[],[]).

a([e(bleue),
   no_e(bleu)],padv,[],[]).

a([e(blije),
   er(blijer),
   ere(blijere),
   no_e(blij),
   st(blijst),
   ste(blijste)],padv,
  [object_sbar,
   object_vp,
   er_pp_sbar(met),
   er_pp_sbar(om),
   pp(om),
   pp(met)],[]).

a([e(blijde),
   er(blijder),
   ere(blijdere),
   st(blijdst),
   ste(blijdste)],adv,[],[]).

a([e(blijkbare),
   no_e(blijkbaar)],adv,[],[]).

a([e(blijmoedige),
   er(blijmoediger),
   ere(blijmoedigere),
   no_e(blijmoedig),
   st(blijmoedigst),
   ste(blijmoedigste)],adv,[],[]).

a([stof(blikken)],nonadv,[],[]).

a([e(bliksemsnelle),
   no_e(bliksemsnel)],adv,[],[]).

a([e(blinde),
   er(blinder),
   ere(blindere),
   no_e(blind),
   st(blindst),
   ste(blindste)],padv,
  [er_pp_sbar(voor),
   pp(voor)],[]).

a([both(blindelings)],adv,[],[]).

a([e(blitse),
   no_e(blits)],nonadv,[],[]).

a([both(bloeddoorlopen)],nonadv,[],[]).

a([e(bloeddorstige),
   er(bloeddorstiger),
   ere(bloeddorstigere),
   no_e(bloeddorstig),
   st(bloeddorstigst),
   ste(bloeddorstigste)],adv,[],[]).

a([both(bloedeigen)],nonadv,[],[]).

a([e(bloedeloze),
   no_e(bloedeloos)],padv,[],[]).

a([e(bloederige),
   er(bloederiger),
   ere(bloederigere),
   no_e(bloederig),
   st(bloederigst),
   ste(bloederigste)],adv,[],[]).

a([e(bloedige),
   er(bloediger),
   ere(bloedigere),
   no_e(bloedig),
   st(bloedigst),
   ste(bloedigste)],adv,[],[koud,warm]).

a([e(bloedrode),
   er(bloedroder),
   ere(bloedrodere),
   no_e(bloedrood),
   st(bloedroodst),
   ste(bloedroodste)],adv,[],[]).

a([e(bloedserieuze),
   no_e(bloedserieus)],adv,[],[]).

a([ende(bloedstollende),
   end(bloedstollend)],adv,[],[]).

a([e(bloemrijke),
   er(bloemrijker),
   ere(bloemrijkere),
   no_e(bloemrijk),
   st(bloemrijkst),
   ste(bloemrijkste)],adv,[],[]).

a([e(blonde),
   er(blonder),
   ere(blondere),
   no_e(blond),
   st(blondst),
   ste(blondste)],nonadv,[],[]).

a([e(blote),
   er(bloter),
   ere(blotere),
   no_e(bloot),
   st(blootst),
   ste(blootste)],adv,[],[]).

a([ge_both(blootgegeven)],adv,[],[]).

a([ge_e(blootgelegde),
   ge_no_e(blootgelegd)],adv,[],[]).

a([ge_no_e(blootgestaan),
   ge_e(blootgestane)],adv,[],[]).

a([ge_e(blootgestelde),
   ge_no_e(blootgesteld)],adv,[],[]).

a([e(blootshoofdse),
   no_e(blootshoofds)],adv,[],[]).

a([e(blootsvoetse),
   no_e(blootsvoets),
   e(blootvoetse),
   no_e(blootvoets)],padv,[],[]).

a([ende(blozende),
   er(blozender),
   ere(blozendere),
   end(blozend),
   st(blozendst),
   ste(blozendste)],padv,[],[]).

a([pred(bluesy)],adv,[],[]).

a([pred(blut)],nonadv,[],[]).

a([e(bochtige),
   er(bochtiger),
   ere(bochtigere),
   no_e(bochtig),
   st(bochtigst),
   ste(bochtigste)],nonadv,[],[]).

a([e(bodemloze),
   er(bodemlozer),
   ere(bodemlozere),
   no_e(bodemloos),
   st(bodemloost),
   ste(bodemlooste)],nonadv,[],[]).

a([e(boeddhistische),
   er(boeddhistischer),
   ere(boeddhistischere),
   no_e(boeddhistisch),
   st(boeddhistischt),
   ste(boeddhistischte)],nonadv,[],[]).

a([ende(boeiende),
   er(boeiender),
   ere(boeiendere),
   end(boeiend),
   st(boeiendst),
   ste(boeiendste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(boekhoudkundige),
   no_e(boekhoudkundig)],adv,[],[]).

a([e(boerse),
   er(boerser),
   ere(boersere),
   no_e(boers),
   st(boerst),
   ste(boerste)],adv,[],[]).

a([e(bolle),
   er(boller),
   ere(bollere),
   no_e(bol),
   st(bolst),
   ste(bolste)],nonadv,[],[]).

a([e(bolsjewistische),
   er(bolsjewistischer),
   ere(bolsjewistischere),
   no_e(bolsjewistisch),
   st(bolsjewistischt),
   ste(bolsjewistischte)],adv,[],[]).

a([pred([bon,ton])],nonadv,[subject_vp],[]).

a([both([bona,fide]),
   both(bonafide)],nonadv,[],[]).

a([e(bondige),
   er(bondiger),
   ere(bondigere),
   no_e(bondig),
   st(bondigst),
   ste(bondigste)],adv,[],[]).

a([e(bonte),
   er(bonter),
   ere(bontere),
   no_e(bont),
   st(bontst),
   ste(bontste)],adv,[],[]).

a([both(booming)],nonadv,[],[]).

a([e(boze),
   er(bozer),
   ere(bozere),
   no_e(boos),
   st(boost),
   ste(booste)],padv,
  [object_sbar,
   er_pp_sbar(over),
   pp(op),
   pp(over)],[]).

a([e(boosaardige),
   er(boosaardiger),
   ere(boosaardigere),
   no_e(boosaardig),
   st(boosaardigst),
   ste(boosaardigste)],adv,[],[]).

a([e(borstelige),
   er(borsteliger),
   ere(borsteligere),
   no_e(borstelig),
   st(borsteligst),
   ste(borsteligste)],nonadv,[],[]).

a([e(bosrijke),
   er(bosrijker),
   ere(bosrijkere),
   no_e(bosrijk),
   st(bosrijkst),
   ste(bosrijkste)],nonadv,[],[]).

a([e(botte),
   er(botter),
   ere(bottere),
   no_e(bot),
   st(botst),
   ste(botste)],adv,[],[]).

a([e(botanische),
   no_e(botanisch)],adv,[],[]).

a([ge_e(botgevierde),
   ge_no_e(botgevierd)],adv,[],[]).

a([e(boude),
   no_e(boud)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([both(bourgeois)],nonadv,[],[]).

a([e(bouwkundige),
   no_e(bouwkundig)],adv,[],[]).

a([e(bouwvallige),
   er(bouwvalliger),
   ere(bouwvalligere),
   no_e(bouwvallig),
   st(bouwvalligst),
   ste(bouwvalligste)],nonadv,[],[]).

a([pred([boven,'Jan'])],nonadv,[],[]).

a([e(bovenaardse),
   er(bovenaardser),
   ere(bovenaardsere),
   no_e(bovenaards),
   st(bovenaardst),
   ste(bovenaardste)],nonadv,[],[]).

a([ge_both(bovengekomen)],adv,[],[]).

a([e(bovengenoemde),
   no_e(bovengenoemd)],nonadv,[],[]).

a([e(bovengrondse),
   no_e(bovengronds)],adv,[],[]).

a([e(bovenhandse),no_e(bovenhands)],adv,[],[]).

a([e(bovenmatige),
   no_e(bovenmatig)],adv,[],[]).

a([e(bovenmenselijke),
   er(bovenmenselijker),
   ere(bovenmenselijkere),
   no_e(bovenmenselijk),
   st(bovenmenselijkst),
   ste(bovenmenselijkste)],adv,[],[]).

a([e(bovennatuurlijke),
   er(bovennatuurlijker),
   ere(bovennatuurlijkere),
   no_e(bovennatuurlijk),
   st(bovennatuurlijkst),
   ste(bovennatuurlijkste)],adv,[],[]).

a([ste(bovenste),
   st(bovenste)],nonadv,[],[]).

a([e(bovenstaande),
   no_e(bovenstaand)],nonadv,[],[]).

a([e(bovenvermelde),
   no_e(bovenvermeld)],nonadv,[],[]).

a([e(bovenwindse),
   no_e(bovenwinds)],nonadv,[],[]).

a([e(brave),
   er(braver),
   ere(bravere),
   no_e(braaf),
   st(braafst),
   ste(braafste)],adv,[],[]).

a([ende(braakliggende),
   end(braakliggend)],padv,[],[]).

a([e(brakke),
   er(brakker),
   ere(brakkere),
   no_e(brak),
   st(brakst),
   ste(brakste)],nonadv,[],[]).

a([e(brandbare),
   er(brandbaarder),
   ere(brandbaardere),
   no_e(brandbaar),
   st(brandbaarst),
   ste(brandbaarste)],nonadv,[],[]).

a([ende(brandende),
   er(brandender),
   ere(brandendere),
   end(brandend),
   st(brandendst),
   ste(brandendste)],adv,[],[]).

a([e(branderige),
   er(branderiger),
   ere(branderigere),
   no_e(branderig),
   st(branderigst),
   ste(branderigste)],nonadv,[],[]).

a([e(brede),
   er(breder),
   ere(bredere),
   no_e(breed),
   st(breedst),
   ste(breedste)],adv,[],[]).

a([e(breedgeschouderde),
   er(breedgeschouderder),
   ere(breedgeschouderdere),
   no_e(breedgeschouderd),
   st(breedgeschouderdst),
   ste(breedgeschouderdste)],nonadv,[],[]).

a([e(breekbare),
   er(breekbaarder),
   ere(breekbaardere),
   no_e(breekbaar),
   st(breekbaarst),
   ste(breekbaarste)],adv,[],[on]).

a([e(briljante),
   er(briljanter),
   ere(briljantere),
   no_e(briljant),
   st(briljantst),
   ste(briljantste)],adv,[],[]).

a([stof(briljanten)],nonadv,[],[]).  % hoeveel jaar getrouwd is dat?

a([e(brocante),
   no_e(brocant)],nonadv,[],[]).

a([e(broederlijke),
   er(broederlijker),
   ere(broederlijkere),
   no_e(broederlijk),
   st(broederlijkst),
   ste(broederlijkste)],adv,[],[]).

a([e(broeierige),
   er(broeieriger),
   ere(broeierigere),
   no_e(broeierig),
   st(broeierigst),
   ste(broeierigste)],adv,[],[]).

a([e(brokkelige),
   er(brokkeliger),
   ere(brokkeligere),
   no_e(brokkelig),
   st(brokkeligst),
   ste(brokkeligste)],nonadv,[],[]).

a([e(bronstige),
   no_e(bronstig)],nonadv,[],[]).

a([stof(bronzen)],nonadv,[],[]).

a([e(broodmagere),
   er(broodmagerder),
   ere(broodmagerdere),
   no_e(broodmager),
   st(broodmagerst),
   ste(broodmagerste)],padv,[],[]).

a([e(broodnodige),
   er(broodnodiger),
   ere(broodnodigere),
   no_e(broodnodig),
   st(broodnodigst),
   ste(broodnodigste)],nonadv,
  [pp(voor)],[]).

a([e(broze),
   er(brozer),
   ere(brozere),
   no_e(broos),
   st(broost),
   ste(brooste)],adv,[],[]).

a([e(brosse),
   er(brosser),
   ere(brossere),
   no_e(bros),
   st(brost),
   ste(broste)],nonadv,[],[]).

a([e(bruikbare),
   er(bruikbaarder),
   ere(bruikbaardere),
   no_e(bruikbaar),
   st(bruikbaarst),
   ste(bruikbaarste)],nonadv,
  [pp(voor)],[]).

a([e(bruine),
   er(bruiner),
   ere(bruinere),
   no_e(bruin),
   st(bruinst),
   ste(bruinste)],adv,[],[rood]).

a([e(bruinachtige),
   er(bruinachtiger),
   ere(bruinachtigere),
   no_e(bruinachtig),
   st(bruinachtigst),
   ste(bruinachtigste)],nonadv,[],[]).

a([pred(brut)],nonadv,[],[]).

a([e(brutale),
   er(brutaler),
   ere(brutalere),
   no_e(brutaal),
   st(brutaalst),
   ste(brutaalste)],adv,[],[]).

a([both(bruto)],adv,[],[]).

a([e(bruuske),
   er(bruusker),
   ere(bruuskere),
   no_e(bruusk),
   st(bruuskst),
   ste(bruuskste)],adv,[],[]).

a([e(brute),
   er(bruter),
   ere(brutere),
   no_e(bruut),
   st(bruutst),
   ste(bruutste)],adv,[],[]).

a([both([brute,force])],adv,[],[]).

a([e(budgettaire),
   no_e(budgettair)],adv,[],[]).

a([e(buigzame),
   er(buigzamer),
   ere(buigzamere),
   no_e(buigzaam),
   st(buigzaamst),
   ste(buigzaamste)],nonadv,[],[]).

a([pred([buiten,kijf])],nonadv,
  [subject_sbar],[]).

a([e(buitenaardse),
   no_e(buitenaards)],nonadv,[],[]).

a([e(buitendijkse),
   no_e(buitendijks)],adv,[],[]).

a([e(buitenechtelijke),
   no_e(buitenechtelijk)],nonadv,[],[]).

a([ge_both(buitengekomen)],adv,[],[]).

a([ge_both(buitengesloten)],adv,[],[]).

a([e(buitengewone),
   er(buitengewoner),
   ere(buitengewonere),
   no_e(buitengewoon),
   st(buitengewoonst),
   ste(buitengewoonste)],adv,[],[]).

a([e(buitenissige),
   er(buitenissiger),
   ere(buitenissigere),
   no_e(buitenissig),
   st(buitenissigst),
   ste(buitenissigste)],nonadv,[],[]).

a([e(buitenlandse),
   no_e(buitenlands)],adv,[],[]).

a([e(buitenparlementaire),
   no_e(buitenparlementair)],adv,[],[]).

a([e(buitenschoolse),
   no_e(buitenschools)],nonadv,[],[]).

a([pred(buitenspel)],adv,[],[]).

a([e(buitensporige),
   er(buitensporiger),
   ere(buitensporigere),
   no_e(buitensporig),
   st(buitensporigst),
   ste(buitensporigste)],adv,
  [subject_vp],[]).

a([e(buitenste),
   no_e(buitenste)],nonadv,[],[]).

a([ge_e(buitgemaakte),
   ge_no_e(buitgemaakt)],adv,[],[]).

a([e(bultige),
   er(bultiger),
   ere(bultigere),
   no_e(bultig),
   st(bultigst),
   ste(bultigste)],nonadv,[],[]).

a([e(bureaucratische),
   e(bureaukratische),
   er(bureaucratischer),
   er(bureaukratischer),
   ere(bureaucratischere),
   ere(bureaukratischere),
   no_e(bureaucratisch),
   no_e(bureaukratisch),
   st(bureaucratischt),
   st(bureaukratischt),
   ste(bureaucratischte),
   ste(bureaukratischte)],adv,[],[]).

a([e(burgerlijke),
   er(burgerlijker),
   ere(burgerlijkere),
   no_e(burgerlijk),
   st(burgerlijkst),
   ste(burgerlijkste)],adv,[],[]).

a([both(bête)],adv,[],[]).

a([e(calvinistische),
   er(calvinistischer),
   ere(calvinistischere),
   no_e(calvinistisch),
   st(calvinistischt),
   ste(calvinistischte)],adv,[],[]).

a([e(capabele),
   no_e(capabel)],adv,
  [object_vp,
   pp(in)],[]).

a([stof([carbon,fibre]),
   stof('carbon-fibre'),
   stof([carbon,fiber]),
   stof('carbon-fiber')],nonadv,[],[]).

a([e(cardiovasculaire),
   no_e(cardiovasculair)],nonadv,[],[]).

a([e(carnivore),
   no_e(carnivoor)],nonadv,[],[]).

a([both(cash)],adv,[],[]).

a([both(casual)],adv,[],[]).

a([e(catastrofale),
   e(katastrofale),
   er(catastrofaler),
   er(katastrofaler),
   ere(catastrofalere),
   ere(katastrofalere),
   no_e(catastrofaal),
   no_e(katastrofaal),
   st(catastrofaalst),
   st(katastrofaalst),
   ste(catastrofaalste),
   ste(katastrofaalste)],adv,[],[]).

a([e(categoriale),
   e(kategoriale),
   no_e(categoriaal),
   no_e(kategoriaal)],nonadv,[],[]).

a([e(categorische),
   e(kategorische),
   er(categorischer),
   er(kategorischer),
   ere(categorischere),
   ere(kategorischere),
   no_e(categorisch),
   no_e(kategorisch),
   st(categorischt),
   st(kategorischt),
   ste(categorischte),
   ste(kategorischte)],adv,[],[]).

a([e(causale),
   e(kausale),
   no_e(causaal),
   no_e(kausaal)],nonadv,[],[]).

a([stof(cementen)],nonadv,[],[]).

a([e(centrale),
   er(centraler),
   ere(centralere),
   no_e(centraal),
   st(centraalst),
   ste(centraalste)],adv,[],[]).

a([e(centralistische),
   er(centralistischer),
   ere(centralistischere),
   no_e(centralistisch),
   st(centralistischt),
   ste(centralistischte)],adv,[],[]).

a([e('centrum-linkse'),
   no_e('centrum-links')],adv,[],[]).

a([e('centrum-rechtse'),
   no_e('centrum-rechts')],adv,[],[]).

a([e(cerebrale),
   no_e(cerebraal)],nonadv,[],[]).

a([e(ceremoniële),
   no_e(ceremonieel)],nonadv,[],[]).

a([e(chagrijnige),
   e(sjagrijnige),
   er(chagrijniger),
   er(sjagrijniger),
   ere(chagrijnigere),
   ere(sjagrijnigere),
   no_e(chagrijnig),
   no_e(sjagrijnig),
   st(chagrijnigst),
   st(sjagrijnigst),
   ste(chagrijnigste),
   ste(sjagrijnigste)],padv,[],[]).

a([e(chaotische),
   er(chaotischer),
   ere(chaotischere),
   no_e(chaotisch),
   st(chaotischt),
   ste(chaotischte)],adv,[],[]).

a([e(charismatische),
   er(charismatischer),
   ere(charismatischere),
   no_e(charismatisch),
   st(charismatischt),
   ste(charismatischte)],nonadv,[],[]).

a([e(charitatieve),
   no_e(charitatief)],adv,[],[]).

a([e(charmante),
   er(charmanter),
   ere(charmantere),
   no_e(charmant),
   st(charmantst),
   ste(charmantste)],both,[],[]).

a([e(chemische),
   no_e(chemisch)],adv,[],[]).

a([both(chique),
   e(sjieke),
   er(chiquer),
   er(sjieker),
   ere(chiquere),
   ere(sjiekere),
   no_e(chic),
   no_e(sjiek),
   st(chicst),
   st(sjiekst),
   ste(chicste),
   ste(sjiekste)],adv,[],[]).

%% Ida: vet chill!
a([e(chille),
   no_e(chill)],adv,[],[]).

a([e(chirurgische),
   no_e(chirurgisch)],adv,[],[]).

a([e(christelijke),
   e(kristelijke),
   er(christelijker),
   er(kristelijker),
   ere(christelijkere),
   ere(kristelijkere),
   no_e(christelijk),
   no_e(kristelijk),
   st(christelijkst),
   st(kristelijkst),
   ste(christelijkste),
   ste(kristelijkste)],adv,[],
  [h(joods),
   h(protestants)]).

a([e('christen-democratische'),
   no_e('christen-democratisch'),
   e([christen,democratische]),
   no_e([christen,democratisch])],adv,[],[]).

a([stof(chromen)],nonadv,[],[]).

a([e(chronische),
   er(chronischer),
   ere(chronischere),
   no_e(chronisch),
   st(chronischt),
   ste(chronischte)],adv,[],[]).

a([e(chronologische),
   no_e(chronologisch)],adv,[],[]).

a([e(cijfermatige),
   no_e(cijfermatig)],adv,[],[]).

a([e(circulaire),
   no_e(circulair)],adv,[],[]).

a([e(cilindrische),
   no_e(cilindrisch)],adv,[],[]).

a([e(citroengele),
   er(citroengeler),
   ere(citroengelere),
   no_e(citroengeel),
   st(citroengeelst),
   ste(citroengeelste)],nonadv,[],[]).

a([e(civiele),
   er(civieler),
   ere(civielere),
   no_e(civiel),
   st(civielst),
   ste(civielste)],adv,[],[]).

a([e(civielrechtelijke),
   no_e(civielrechtelijk)],adv,[],[]).

a([e(clandestiene),
   e(klandestiene),
   no_e(clandestien),
   no_e(klandestien)],adv,[],[]).

a([e(cleane),
   er(cleaner),
   ere(cleanere),
   no_e(clean),
   st(cleanst),
   ste(cleanste)],padv,[],[]).

a([e(clemente),
   e(klemente),
   er(clementer),
   er(klementer),
   ere(clementere),
   ere(klementere),
   no_e(clement),
   no_e(klement),
   st(clementst),
   st(klementst),
   ste(clementste),
   ste(klementste)],adv,[],[]).

a([e(clevere),
   no_e(clever)],adv,[],[]).

a([e(clichématige),
   no_e(clichématig)],adv,[],[]).

a([both(close)],adv,[],[]).

a([e(cognitieve),
   no_e(cognitief)],adv,[],[]).

a([e(coherente),
   e(koherente),
   no_e(coherent),
   no_e(koherent)],adv,[],[]).

a([e(collectieve),
   e(kollektieve),
   er(collectiever),
   er(kollektiever),
   ere(collectievere),
   ere(kollektievere),
   no_e(collectief),
   no_e(kollektief),
   st(collectiefst),
   st(kollektiefst),
   ste(collectiefste),
   ste(kollektiefste)],adv,[],[]).

a([e(collegiale),
   e(kollegiale),
   er(collegialer),
   er(kollegialer),
   ere(collegialere),
   ere(kollegialere),
   no_e(collegiaal),
   no_e(kollegiaal),
   st(collegiaalst),
   st(kollegiaalst),
   ste(collegiaalste),
   ste(kollegiaalste)],adv,[],[]).

a([e(comfortabele),
   e(komfortabele),
   er(comfortabeler),
   er(komfortabeler),
   ere(comfortabelere),
   ere(komfortabelere),
   no_e(comfortabel),
   no_e(komfortabel),
   st(comfortabelst),
   st(komfortabelst),
   ste(comfortabelste),
   ste(komfortabelste)],adv,[],[]).

a([e(commanditaire),
   no_e(commanditair)],nonadv,[],[]).

a([e(commerciële),
   er(commerciëler),
   ere(commercielere),
   no_e(commercieel),
   st(commercieelst),
   ste(commercieelste)],adv,[],[]).

a([no_e(commissionair),
   e(commissionaire)],adv,[],[]).

a([e(communautaire),
   no_e(communautair)],adv,[],[]).

a([e(communicatieve),
   e(kommunikatieve),
   no_e(communicatief),
   no_e(kommunikatief)],adv,[],[]).

a([e(communistische),
   e(kommunistische),
   er(communistischer),
   er(kommunistischer),
   ere(communistischere),
   ere(kommunistischere),
   no_e(communistisch),
   no_e(kommunistisch),
   st(communistischt),
   st(kommunistischt),
   ste(communistischte),
   ste(kommunistischte)],adv,[],[h(ex)]).

a([e(compacte),
   e(kompakte),
   er(compacter),
   er(kompakter),
   ere(compactere),
   ere(kompaktere),
   no_e(compact),
   no_e(kompakt),
   st(compactst),
   st(kompaktst),
   ste(compactste),
   ste(kompaktste)],adv,[],[]).

a([e(compensabele),
   no_e(compensabel)],nonadv,[],[]).

a([e(competente),
   e(kompetente),
   er(competenter),
   er(kompetenter),
   ere(competentere),
   ere(kompetentere),
   no_e(competent),
   no_e(kompetent),
   st(competentst),
   st(kompetentst),
   ste(competentste),
   ste(kompetentste)],nonadv,[],[]).

a([e(competitieve),
   no_e(competitief),
   ere(competitievere),
   er(competitiever),
   st(competititiefst),
   ste(competitiefste)],adv,[],[]).

a([e(complete),
   e(komplete),
   er(completer),
   er(kompleter),
   ere(completere),
   ere(kompletere),
   no_e(compleet),
   no_e(kompleet),
   st(compleetst),
   st(kompleetst),
   ste(compleetste),
   ste(kompleetste)],adv,[],[]).

a([e(complementaire),
   e(komplementaire),
   no_e(complementair),
   no_e(komplementair)],nonadv,[],[]).

a([e(complexe),
   er(complexer),
   ere(complexere),
   no_e(complex),
   st(complext),
   ste(complexte)],nonadv,[],[]).

a([e(computationele),
   no_e(computationeel)],adv,[],[]).

a([e(computergestuurde),
   no_e(computergestuurd)],adv,[],[]).

a([e(concave),
   no_e(concaaf)],nonadv,[],[]).

a([e(concentrische),
   e(koncentrische),
   no_e(concentrisch),
   no_e(koncentrisch)],adv,[],[]).

a([e(conceptuele),
   no_e(conceptueel)],adv,[],[]).

a([e(concertante),
   no_e(concertant)],adv,[],[]).

a([e(concrete),
   e(konkrete),
   er(concreter),
   er(konkreter),
   ere(concretere),
   ere(konkretere),
   no_e(concreet),
   no_e(konkreet),
   st(concreetst),
   st(konkreetst),
   ste(concreetste),
   ste(konkreetste)],adv,[],[]).

a([e(concurrente),
   no_e(concurrent)],nonadv,[],[]).

a([ende(concurrerende),
   ende(konkurrerende),
   er(concurrerender),
   er(konkurrerender),
   ere(concurrerendere),
   ere(konkurrerendere),
   end(concurrerend),
   end(konkurrerend),
   st(concurrerendst),
   st(konkurrerendst),
   ste(concurrerendste),
   ste(konkurrerendste)],padv,[],[]).

a([e(conditionele),
   no_e(conditioneel)],padv,[],[]).

a([e(confessionele),
   e(konfessionele),
   no_e(confessioneel),
   no_e(konfessioneel)],nonadv,[],[]).

a([e(conforme),
   er(conformer),
   ere(conformere),
   no_e(conform),
   st(conformst),
   ste(conformste)],adv,[],[markt]).

a([no_e(confuus),
   e(confuse)],padv,[],[]).

a([e(conjuncturele),
   e(konjunkturele),
   no_e(conjunctureel),
   no_e(konjunktureel)],adv,[],[]).

a([e(consequente),
   e(konsekwente),
   er(consequenter),
   er(konsekwenter),
   ere(consequentere),
   ere(konsekwentere),
   no_e(consequent),
   no_e(konsekwent),
   st(consequentst),
   st(konsekwentst),
   ste(consequentste),
   ste(konsekwentste)],adv,
  [er_pp_sbar(in),
   er_pp_vp(in),
   pp(in),
   subject_vp,
   subject_sbar],[]).

a([e(conservatieve),
   e(konservatieve),
   er(conservatiever),
   er(konservatiever),
   ere(conservatievere),
   ere(konservatievere),
   no_e(conservatief),
   no_e(konservatief),
   st(conservatiefst),
   st(konservatiefst),
   ste(conservatiefste),
   ste(konservatiefste)],adv,[],[neo]).

a([e('Conservatieve'),
   no_e('Conservatief')],nonadv,[],[]).

a([e(consistente),
   e(konsistente),
   er(consistenter),
   er(konsistenter),
   ere(consistentere),
   ere(konsistentere),
   no_e(consistent),
   no_e(konsistent),
   st(consistentst),
   st(konsistentst),
   ste(consistentste),
   ste(konsistentste)],adv,
  [pp(in),
   pp(met)],[]).

a([e(constante),
   e(konstante),
   er(constanter),
   er(konstanter),
   ere(constantere),
   ere(konstantere),
   no_e(constant),
   no_e(konstant),
   st(constantst),
   st(konstantst),
   ste(constantste),
   ste(konstantste)],adv,[],[]).

a([e(constitutionele),
   e(konstitutionele),
   no_e(constitutioneel),
   no_e(konstitutioneel)],adv,[],[]).

a([e(constructieve),
   e(konstruktieve),
   er(constructiever),
   er(konstruktiever),
   ere(constructievere),
   ere(konstruktievere),
   no_e(constructief),
   no_e(konstruktief),
   st(constructiefst),
   st(konstruktiefst),
   ste(constructiefste),
   ste(konstruktiefste)],adv,[],[]).

a([e(consulaire),
   no_e(consulair)],adv,[],[]).

a([e(consumptieve),
   e(konsumptieve),
   no_e(consumptief),
   no_e(konsumptief)],nonadv,[],[]).

a([e(contante),
   e(kontante),
   er(contanter),
   er(kontanter),
   ere(contantere),
   ere(kontantere),
   no_e(contant),
   no_e(kontant),
   st(contantst),
   st(kontantst),
   ste(contantste),
   ste(kontantste)],adv,[],[]).

a([e(contemporaine),
   no_e(contemporain)],nonadv,[],[]).

a([e(contente),
   e(kontente),
   er(contenter),
   er(kontenter),
   ere(contentere),
   ere(kontentere),
   no_e(content),
   no_e(kontent),
   st(contentst),
   st(kontentst),
   ste(contentste),
   ste(kontentste)],padv,
  [er_pp_sbar(met),
   er_pp_vp(met),
   object_sbar,
   pp(met)],[]).

a([e(continentale),
   e(kontinentale),
   no_e(continentaal),
   no_e(kontinentaal)],nonadv,[],[]).

a([e(continue),
   e(kontinue),
   no_e(continu),
   no_e(kontinu)],adv,[],[]).

a([e(contractuele),
   e(kontraktuele),
   no_e(contractueel),
   no_e(kontraktueel)],adv,[],[]).

a([no_e(contraproductief),
   e(contraproductieve)],adv,[],[]).

a([e('contra-revolutionaire'),
   e(contrarevolutionaire),
   no_e('contra-revolutionair'),
   no_e(contrarevolutionair)],adv,[],[]).

a([e(controleerbare),
   e(kontroleerbare),
   er(controleerbaarder),
   er(kontroleerbaarder),
   ere(controleerbaardere),
   ere(kontroleerbaardere),
   no_e(controleerbaar),
   no_e(kontroleerbaar),
   st(controleerbaarst),
   st(kontroleerbaarst),
   ste(controleerbaarste),
   ste(kontroleerbaarste)],adv,[],[]).

a([e(controversiële),
   er(controversiëler),
   ere(controversiëlere),
   no_e(controversieel),
   st(controversieelst),
   ste(controversieelste)],nonadv,[],[]).

a([e(conventionele),
   e(konventionele),
   er(conventioneler),
   er(konventioneler),
   ere(conventionelere),
   ere(konventionelere),
   no_e(conventioneel),
   no_e(konventioneel),
   st(conventioneelst),
   st(konventioneelst),
   ste(conventioneelste),
   ste(konventioneelste)],adv,[],[on]).

a([e(convergente),
   no_e(convergent)],adv,[],[]).

a([e(converteerbare),
   no_e(converteerbaar)],nonadv,
  [pp(naar)],[]).

a([e(coole),
   no_e(cool)],adv,[],[]).

a([e(coöperatieve),
   e(koöperatieve),
   no_e(coöperatief),
   no_e(koöperatief)],adv,[],[]).

a([stof(corduroy)],nonadv,[],[]).

a([e(corporatieve),
   e(korporatieve),
   no_e(corporatief),
   no_e(korporatief)],adv,[],[]).

a([both(corporate)],nonadv,[],[]).

a([e(corpulente),
   e(korpulente),
   er(corpulenter),
   er(korpulenter),
   ere(corpulentere),
   ere(korpulentere),
   no_e(corpulent),
   no_e(korpulent),
   st(corpulentst),
   st(korpulentst),
   ste(corpulentste),
   ste(korpulentste)],nonadv,[],[]).

a([e(correcte),
   e(korrekte),
   er(correcter),
   er(korrekter),
   ere(correctere),
   ere(korrektere),
   no_e(correct),
   no_e(korrekt),
   st(correctst),
   st(korrektst),
   ste(correctste),
   ste(korrektste)],adv,
  [subject_sbar,
   subject_vp],[in,h(politiek)]).

a([e(correctieve),
   no_e(correctief)],nonadv,[],[]).

a([e(correctionele),
   no_e(correctioneel)],nonadv,[],[]).

a([e(corrupte),
   e(korrupte),
   er(corrupter),
   er(korrupter),
   ere(corruptere),
   ere(korruptere),
   no_e(corrupt),
   no_e(korrupt),
   st(corruptst),
   st(korruptst),
   ste(corruptste),
   ste(korruptste)],adv,[],[]).

a([e(coulante),
   no_e(coulant),
   er(coulanter),
   ere(coulantere),
   st(coulantst),
   ste(coulantste)],adv,[],[]).

a([e(courante),
   no_e(courant)],adv,[],[]).

a([e(covalente),
   no_e(covalent)],adv,[],[]).

a([e(creatieve),
   e(kreatieve),
   er(creatiever),
   er(kreatiever),
   ere(creatievere),
   ere(kreatievere),
   no_e(creatief),
   no_e(kreatief),
   st(creatiefst),
   st(kreatiefst),
   ste(creatiefste),
   ste(kreatiefste)],adv,[],[]).

a([e(creoolse),
   e(kreoolse),
   er(creoolser),
   er(kreoolser),
   ere(creoolsere),
   ere(kreoolsere),
   no_e(creools),
   no_e(kreools),
   st(creoolst),
   st(kreoolst),
   ste(creoolste),
   ste(kreoolste)],nonadv,[],[]).

a([e(criminele),
   e(kriminele),
   er(crimineler),
   er(krimineler),
   ere(criminelere),
   ere(kriminelere),
   no_e(crimineel),
   no_e(krimineel),
   st(crimineelst),
   st(krimineelst),
   ste(crimineelste),
   ste(krimineelste)],adv,[],[]).

a([e(crue),
   no_e(cru)],adv,[],[]).

a([e(cruciale),
   er(crucialer),
   ere(crucialere),
   no_e(cruciaal),
   st(cruciaalst),
   ste(cruciaalste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(cryptische),
   er(cryptischer),
   ere(cryptischere),
   no_e(cryptisch),
   st(cryptischt),
   ste(cryptischte)],adv,[],[]).

a([e(culinaire),
   er(culinairder),
   ere(culinairdere),
   no_e(culinair),
   st(culinairst),
   ste(culinairste)],padv,[],[]).

a([e(culturele),
   er(cultureler),
   ere(culturelere),
   no_e(cultureel),
   st(cultureelst),
   ste(cultureelste)],adv,[],[inter]).

a([e(cumulatieve),
   er(cumulatiever),
   ere(cumulatievere),
   no_e(cumulatief),
   st(cumulatiefst),
   ste(cumulatiefste)],adv,[],[]).

a([e(curieuze),
   er(curieuzer),
   ere(curieuzere),
   no_e(curieus),
   st(curieust),
   ste(curieuste)],adv,
  [subject_sbar],[]).

a([e(cursieve),
   no_e(cursief)],adv,[],[]).

a([pred(cute)],nonadv,[],[]).

a([e(cyclische),
   no_e(cyclisch)],adv,[],[]).

a([e(cynische),
   er(cynischer),
   ere(cynischere),
   no_e(cynisch),
   st(cynischt),
   ste(cynischte)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(daadwerkelijke),
   no_e(daadwerkelijk)],adv,[],[]).

a([e(daagse),
   no_e(daags)],adv,[],[meer]).

a([ge_both(daargelaten)],adv,
  [transitive,
   object_sbar],[]).

a([e(daaropvolgende),
   no_e(daaropvolgend)],adv,[],[]).

a([e(dadelijke),
   er(dadelijker),
   ere(dadelijkere),
   no_e(dadelijk),
   st(dadelijkst),
   ste(dadelijkste)],adv,[],[]).

a([e(dagelijkse),
   er(dagelijkser),
   ere(dagelijksere),
   no_e(dagelijks),
   st(dagelijkst),
   ste(dagelijkste)],adv,[],[]).

a([e(dakloze),
   no_e(dakloos)],nonadv,[],[]).

a([e(danige),
   no_e(danig)],adv,[],[]).

a([stof(damasten)],nonadv,[],[]).

a([e(dankbare),
   er(dankbaarder),
   ere(dankbaardere),
   no_e(dankbaar),
   st(dankbaarst),
   ste(dankbaarste)],adv,
  [object_sbar,
   er_pp_sbar(voor),
   pp(voor)],[]).

a([e(dansante),
   no_e(dansant)],nonadv,[],[]).

a([e(dappere),
   er(dapperder),
   ere(dapperdere),
   no_e(dapper),
   st(dapperst),
   ste(dapperste)],adv,[subject_sbar],[]).

a([e(dartele),
   er(darteler),
   ere(dartelere),
   no_e(dartel),
   st(dartelst),
   ste(dartelste)],adv,[],[]).

a([e(daverende),
   er(daverender),
   ere(daverendere),
   no_e(daverend),
   st(daverendst),
   ste(daverendste)],adv,[],[]).

a([both([de,facto])],adv,[],[]).

a([pred(debet)],nonadv,
  [pp(aan)],[]).

a([e(debiele),
   er(debieler),
   ere(debielere),
   no_e(debiel),
   st(debielst),
   ste(debielste)],nonadv,[],[]).

a([e(decadente),
   e(dekadente),
   er(decadenter),
   er(dekadenter),
   ere(decadentere),
   ere(dekadentere),
   no_e(decadent),
   no_e(dekadent),
   st(decadentst),
   st(dekadentst),
   ste(decadentste),
   ste(dekadentste)],padv,[],[]).

a([e(decoratieve),
   e(dekoratieve),
   er(decoratiever),
   er(dekoratiever),
   ere(decoratievere),
   ere(dekoratievere),
   no_e(decoratief),
   no_e(dekoratief),
   st(decoratiefst),
   st(dekoratiefst),
   ste(decoratiefste),
   ste(dekoratiefste)],nonadv,[],[]).

a([e(deductieve),
   e(deduktieve),
   no_e(deductief),
   no_e(deduktief)],adv,[],[]).

a([e(deelachtige),
   er(deelachtiger),
   ere(deelachtigere),
   no_e(deelachtig),
   st(deelachtigst),
   ste(deelachtigste)],nonadv,[],[]).

a([ge_both(deelgenomen)],adv,[],[]).

a([ende(deelnemende),
   er(deelnemender),
   ere(deelnemendere),
   end(deelnemend),
   st(deelnemendst),
   ste(deelnemendste)],adv,[],[]).

a([prefix(deeltijd)],nonadv,[],[]).

a([both(deeltijds),e(deeltijdse)],adv,[],[]).

a([e(deemoedige),
   er(deemoediger),
   ere(deemoedigere),
   no_e(deemoedig),
   st(deemoedigst),
   ste(deemoedigste)],adv,[],[]).

a([e(deerlijke),
   er(deerlijker),
   ere(deerlijkere),
   no_e(deerlijk),
   st(deerlijkst),
   ste(deerlijkste)],adv,[],[]).

a([e(deerniswekkende),
   er(deerniswekkender),
   ere(deerniswekkendere),
   no_e(deerniswekkend),
   st(deerniswekkendst),
   ste(deerniswekkendste)],nonadv,[],[]).

a([e(defecte),
   e(defekte),
   er(defecter),
   er(defekter),
   ere(defectere),
   ere(defektere),
   no_e(defect),
   no_e(defekt),
   st(defectst),
   st(defektst),
   ste(defectste),
   ste(defektste)],nonadv,[],[]).

a([e(defensieve),
   no_e(defensief)],adv,[],[]).

a([e(definitieve),
   er(definitiever),
   ere(definitievere),
   no_e(definitief),
   st(definitiefst),
   ste(definitiefste)],adv,[],[]).

a([e(deftige),
   er(deftiger),
   ere(deftigere),
   no_e(deftig),
   st(deftigst),
   ste(deftigste)],adv,[],[]).

a([e(degelijke),
   er(degelijker),
   ere(degelijkere),
   no_e(degelijk),
   st(degelijkst),
   ste(degelijkste)],adv,[],[]).

a([e(dekselse),
   no_e(deksels)],adv,[],[]).

a([e(delicate),
   e(delikate),
   er(delicater),
   er(delikater),
   ere(delicatere),
   ere(delikatere),
   no_e(delicaat),
   no_e(delikaat),
   st(delicaatst),
   st(delikaatst),
   ste(delicaatste),
   ste(delikaatste)],adv,[],[]).

a([e(delinquente),
   no_e(delinquent)],adv,[],[]).

a([e(demente),
   no_e(dement)],nonadv,[],[]).

a([ende(dementerende),
   end(dementerend)],nonadv,[],[]).

a([e(demissionaire),
   no_e(demissionair)],adv,[],[]).

a([e(democratische),
   e(demokratische),
   er(democratischer),
   er(demokratischer),
   ere(democratischere),
   ere(demokratischere),
   no_e(democratisch),
   no_e(demokratisch),
   st(democratischt),
   st(demokratischt),
   ste(democratischte),
   ste(demokratischte)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(demografische),
   no_e(demografisch)],adv,[],[]).

a([e(demonische),
   er(demonischer),
   ere(demonischere),
   no_e(demonisch),
   st(demonischt),
   ste(demonischte)],adv,[],[]).

a([e(demonstratieve),
   er(demonstratiever),
   ere(demonstratievere),
   no_e(demonstratief),
   st(demonstratiefst),
   ste(demonstratiefste)],adv,[],[]).

a([e(denigrerende),
   no_e(denigrerend)],adv,[],[]).

a([stof(denim)],nonadv,[],[]).

a([e(denkbare),
   er(denkbaarder),
   ere(denkbaardere),
   no_e(denkbaar),
   st(denkbaarst),
   ste(denkbaarste)],nonadv,
  [subject_sbar],[]).

a([e(denkbeeldige),
   er(denkbeeldiger),
   ere(denkbeeldigere),
   no_e(denkbeeldig),
   st(denkbeeldigst),
   ste(denkbeeldigste)],nonadv,
  [subject_sbar],[]).

a([e(deontologische),
   no_e(deontologisch)],adv,[],[]).

a([e(departementale),
   no_e(departementaal)],nonadv,[],[]).

a([e(deplorabele),
   no_e(deplorabel)],nonadv,[],[]).

a([e(depressieve),
   er(depressiever),
   ere(depressievere),
   no_e(depressief),
   st(depressiefst),
   ste(depressiefste)],padv,[],[]).

a([prefix('derde-klas'),
   prefix([derde,klas]),
   prefix(['3e',klas])],adv,[],[]).

a([e(dergelijke),
   no_e(dergelijk)],adv,[],[]).

a([both(dertiger)],nonadv,[],[]).

a([pred([des,duivels]),
   pred([des,duvels])],nonadv,
  [object_sbar],[]).

a([e(desastreuse),
   e(desastreuze),
   er(desastreuzer),
   ere(desastreuzere),
   no_e(desastreus),
   st(desastreust),
   ste(desastreuste)],adv,[],[]).

a([e(desbetreffende),
   no_e(desbetreffend)],nonadv,[],[]).

a([e(desgewenste),
   no_e(desgewenst)],adv,[],[]).

a([e(deskundige),
   no_e(deskundig)],adv,[],
  [on]).

a([e(desolate),
   er(desolater),
   ere(desolatere),
   no_e(desolaat),
   st(desolaatst),
   ste(desolaatste)],padv,[],[]).

a([e(desperate),
   no_e(desperaat)],both,[],[]).

a([e(destructieve),
   e(destruktieve),
   er(destructiever),
   er(destruktiever),
   ere(destructievere),
   ere(destruktievere),
   no_e(destructief),
   no_e(destruktief),
   st(destructiefst),
   st(destruktiefst),
   ste(destructiefste),
   ste(destruktiefste)],adv,[],[]).

a([e(deterministische),
   no_e(deterministisch)],nonadv,[],[]).

a([e(deugdelijke),
   er(deugdelijker),
   ere(deugdelijkere),
   no_e(deugdelijk),
   st(deugdelijkst),
   ste(deugdelijkste)],adv,[],[]).

a([e(deugdzame),
   er(deugdzamer),
   ere(deugdzamere),
   no_e(deugdzaam),
   st(deugdzaamst),
   ste(deugdzaamste)],adv,[],[]).

a([e(devote),
   er(devoter),
   ere(devotere),
   no_e(devoot),
   st(devootst),
   ste(devootste)],adv,[],[]).

a([e(diagnostische),
   no_e(diagnostisch)],adv,[],[]).

a([e(diagonale),
   no_e(diagonaal)],adv,[],[]).

a([e(dialectische),
   e(dialektische),
   no_e(dialectisch),
   no_e(dialektisch)],adv,[],[]).

a([stof(diamanten)],nonadv,[],[]).

a([e(diametrale),
   no_e(diametraal)],adv,[],[]).

a([e(dichte),
   er(dichter),
   ere(dichtere),
   no_e(dicht),
   st(dichtst),
   ste(dichtste)],adv,[],[]).

a([e(dichtbevolkte),
   no_e(dichtbevolkt),
   er(dichterbevolkt),
   ere(dichterbevolkte),
   st(dichtstbevolkt),
   ste(dichtstbevolkte)],nonadv,[],[]).

m('dichtbij',adjective(het_st(locadv)),[het,dichtst,bij]).
m('dichtbij',adjective(het_st(locadv)),[het,dichtste,bij]).
m('dichtbij',adjective(het_st(locadv)),[het,dichtstbij]).
m('dichtbij',adjective(het_st(locadv)),[het,dichtstebij]).

a([e(dichtbije),e([dicht,bije]),
   er(dichterbij),er([dichter,bij]),
   ere(dichterbije),ere([dichter,bije]),
   no_e(dichtbij),no_e([dicht,bij]),
   st(dichtstbij),st([dichtst,bij]),
   ste(dichtstbije),ste([dichtst,bije])
  ],locadv,[],[]).

a([e(dichtbijzijnde),
   er(dichterbijzijnd),
   ere(dichterbijzijnde),
   no_e(dichtbijzijnd),
   st(dichtstbijzijnd),
   ste(dichtstbijzijnde)],adv,[],[]).

a([e(dichterlijke),
   er(dichterlijker),
   ere(dichterlijkere),
   no_e(dichterlijk),
   st(dichterlijkst),
   ste(dichterlijkste)],adv,[],[]).

a([ge_e(dichtgedane),
   ge_no_e(dichtgedaan)],adv,[],[]).

a([ge_e(dichtgegane),
   ge_no_e(dichtgegaan)],adv,[],[]).

a([ge_e(dichtgeklapte),
   ge_no_e(dichtgeklapt)],adv,[],[]).

a([ge_both(dichtgeknepen)],adv,[],[]).

a([ge_e(dichtgeknoopte),
   ge_no_e(dichtgeknoopt)],adv,[],[]).

a([ge_both(dichtgeslagen)],adv,[],[]).

a([ge_e(dichtgeslibde),
   ge_no_e(dichtgeslibd)],padv,[],[]).

a([ge_e(dichtgetimmerde),
   ge_no_e(dichtgetimmerd)],adv,[],[]).

a([ge_both(dichtgetrokken)],adv,[],[]).

a([both(dichtgevallen)],adv,[],[]).

a([   e(dichtstbijzijnde),
   no_e(dichtstbijzijnd),
      e([dichtst,bijzijnde]),
   no_e([dichtst,bijzijnd]),
      e([dichts,bijzijnde]),
   no_e([dichts,bijzijnd])],nonadv,[],[]).

a([e(dictatoriale),
   e(diktatoriale),
   er(dictatorialer),
   er(diktatorialer),
   ere(dictatorialere),
   ere(diktatorialere),
   no_e(dictatoriaal),
   no_e(diktatoriaal),
   st(dictatoriaalst),
   st(diktatoriaalst),
   ste(dictatoriaalste),
   ste(diktatoriaalste)],adv,[],[]).

a([both(dikwijls)],osentadv,[],[]).

a([e(didactische),
   e(didaktische),
   er(didactischer),
   er(didaktischer),
   ere(didactischere),
   ere(didaktischere),
   no_e(didactisch),
   no_e(didaktisch),
   st(didactischt),
   st(didaktischt),
   ste(didactischte),
   ste(didaktischte)],adv,[],[]).

a([ende(dienende),
   end(dienend)],adv,
  [fixed([[ijs,en,weder]])],[]).

a([e(dienstbare),
   er(dienstbaarder),
   ere(dienstbaardere),
   no_e(dienstbaar),
   st(dienstbaarst),
   ste(dienstbaarste)],nonadv,
  [so_pp(aan)],[]).

a([ende(dienstdoende),
   end(dienstdoend)],nonadv,[],[]).

a([e(dienstige),
   er(dienstiger),
   ere(dienstigere),
   no_e(dienstig),
   st(dienstigst),
   ste(dienstigste)],nonadv,[subject_vp,
			     subject_sbar],[]).

a([e(dienstplichtige),
   no_e(dienstplichtig)],nonadv,[],[]).

a([e(dienstverlenende),
   no_e(dienstverlenend)],nonadv,[],[]).

a([e(diepe),
   er(dieper),
   ere(diepere),
   no_e(diep),
   st(diepst),
   ste(diepste)],locadv,[],[]).

a([e(diepblauwe),
   no_e(diepblauw)],nonadv,[],[]).

a([e(diepgaande),
   no_e(diepgaand)],adv,[],[]).

a([e(diepgegane),
   no_e(diepgegaan)],adv,[],[]).

a([both(diepgevroren)],padv,[],[]).

a([e(diepgewortelde),
   no_e(diepgeworteld)],nonadv,[],[]).

a([e(diepliggende),
   no_e(diepliggend)],nonadv,[],[]).

a([prefix(diepvries)],nonadv,[],[]).

a([e(diepzinnige),
   er(diepzinniger),
   ere(diepzinnigere),
   no_e(diepzinnig),
   st(diepzinnigst),
   ste(diepzinnigste)],adv,[],[]).

a([e(diere),
   er(dierder),
   ere(dierdere),
   no_e(dier),
   st(dierst),
   ste(dierste)],nonadv,[],[]).

a([e(dierbare),
   er(dierbaarder),
   ere(dierbaardere),
   no_e(dierbaar),
   st(dierbaarst),
   ste(dierbaarste)],nonadv,
  [so_np],[]).

a([e(dierlijke),
   er(dierlijker),
   ere(dierlijkere),
   no_e(dierlijk),
   st(dierlijkst),
   ste(dierlijkste)],adv,[],[]).

a([e(differentiële),
   no_e(differentieel)],nonadv,[],[]).

a([e(diffuse),
   er(diffuser),
   ere(diffusere),
   no_e(diffuus),
   st(diffuust),
   ste(diffuuste)],nonadv,[],[]).

a([e(digitale),
   no_e(digitaal)],adv,[],[]).

a([e(dimensionale),
   no_e(dimensionaal),
   e(dimensionele),
   no_e(dimensioneel)],adv,[],[]).

a([e(dikke),
   er(dikker),
   ere(dikkere),
   no_e(dik),
   st(dikst),
   ste(dikste)],adv,[],
  [vinger,
   vuist]).

a([e(dinsdagse),
   no_e(dinsdags)],adv,[],[]).

a([e(diploïde),
   no_e(diploïd)],nonadv,[],[]).

a([e(diplomatieke),
   er(diplomatieker),
   ere(diplomatiekere),
   no_e(diplomatiek),
   st(diplomatiekst),
   ste(diplomatiekste)],adv,[],[]).

a([e(directe),
   e(direkte),
   er(directer),
   er(direkter),
   ere(directere),
   ere(direktere),
   no_e(direct),
   no_e(direkt),
   st(directst),
   st(direktst),
   ste(directste),
   ste(direktste)],adv,
  [pp(in)],[]).

a([e(disciplinaire),
   no_e(disciplinair)],adv,[],[]).

a([e(discrete),
   e(diskrete),
   er(discreter),
   er(diskreter),
   ere(discretere),
   ere(diskretere),
   no_e(discreet),
   no_e(diskreet),
   st(discreetst),
   st(diskreetst),
   ste(discreetste),
   ste(diskreetste)],adv,[],[]).

a([e(discutabele),
   e(diskutabele),
   er(discutabeler),
   er(diskutabeler),
   ere(discutabelere),
   ere(diskutabelere),
   no_e(discutabel),
   no_e(diskutabel),
   st(discutabelst),
   st(diskutabelst),
   ste(discutabelste),
   ste(diskutabelste)],nonadv,[],[]).

a([e(dissidente),
   no_e(dissident)],nonadv,[],[]).

a([both(dito)],adv,[],[]).

a([e(diverse),
   no_e(divers)],adv,[],[]).

a([both(dizzy)],padv,[],[]).

a([e(dociele),
   no_e(dociel)],adv,[],[]).

a([e(doctrinaire),
   er(doctrinairder),
   ere(doctrinairdere),
   no_e(doctrinair),
   st(doctrinairst),
   ste(doctrinairste)],adv,[],[]).

a([e(documentaire),
   no_e(documentair)],adv,[],[]).

a([e(dodelijke),
   er(dodelijker),
   ere(dodelijkere),
   no_e(dodelijk),
   st(dodelijkst),
   ste(dodelijkste)],adv,[],[]).

a([e(doelbewuste),
   er(doelbewuster),
   ere(doelbewustere),
   no_e(doelbewust)],padv,[],[]).

a([e(doelgerichte),
   er(doelgerichter),
   ere(doelgerichtere),
   no_e(doelgericht),
   st(doelgerichtst),
   ste(doelgerichtste)],adv,[],[]).

a([e(doelloze),
   er(doellozer),
   ere(doellozere),
   no_e(doelloos),
   st(doelloost),
   ste(doellooste)],adv,[],[]).

a([e(doelmatige),
   er(doelmatiger),
   ere(doelmatigere),
   no_e(doelmatig),
   st(doelmatigst),
   ste(doelmatigste)],adv,[],[]).

a([e(doeltreffende),
   er(doeltreffender),
   ere(doeltreffendere),
   no_e(doeltreffend),
   st(doeltreffendst),
   ste(doeltreffendste)],adv,[],[]).

a([ende(doende)],padv,
  [object_vp,
   pp(met),
   fixed([[terzake]])],[]).

a([e(doenlijke),
   er(doenlijker),
   ere(doenlijkere),
   no_e(doenlijk),
   st(doenlijkst),
   ste(doenlijkste)],nonadv,[],[]).

a([e(doffe),
   er(doffer),
   ere(doffere),
   no_e(dof),
   st(dofst),
   ste(dofste)],adv,[],[]).

a([e(dogmatische),
   er(dogmatischer),
   ere(dogmatischere),
   no_e(dogmatisch),
   st(dogmatischt),
   ste(dogmatischte)],adv,[],[]).

a([e(dolle),
   er(doller),
   ere(dollere),
   no_e(dol),
   st(dolst),
   ste(dolste)],adv,
  [er_pp_vp(op),
   pp(op)],[]).

a([e(dolblije),
   er(dolblijer),
   ere(dolblijere),
   no_e(dolblij),
   st(dolblijst),
   ste(dolblijste)],padv,
  [object_sbar,
   er_pp_sbar(met),
   er_pp_vp(met),
   pp(met)],[]).

a([e(dolgelukkige),
   no_e(dolgelukkig)],adv,
  [object_sbar,
   er_pp_sbar(met),
   er_pp_vp(met),
   pp(met)],[]).

a([e(domme),
   er(dommer),
   ere(dommere),
   no_e(dom),
   st(domst),
   ste(domste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(dominante),
   er(dominanter),
   ere(dominantere),
   no_e(dominant),
   st(dominantst),
   ste(dominantste)],adv,[],[]).

a([both(dominicaner)],nonadv,[],[]).

a([e(donkere),
   er(donkerder),
   ere(donkerdere),
   no_e(donker),
   st(donkerst),
   ste(donkerste)],nonadv,[],[stik]).

a([e(donkerblauwe),
   er(donkerblauwer),
   ere(donkerblauwere),
   no_e(donkerblauw),
   st(donkerblauwst),
   ste(donkerblauwste)],nonadv,[],[]).

a([e(donkerblonde),
   er(donkerblonder),
   ere(donkerblondere),
   no_e(donkerblond),
   st(donkerblondst),
   ste(donkerblondste)],nonadv,[],[]).

a([e(donkerbruine),
   er(donkerbruiner),
   ere(donkerbruinere),
   no_e(donkerbruin),
   st(donkerbruinst),
   ste(donkerbruinste)],nonadv,[],[]).

a([e(donkergrijze),
   er(donkergrijzer),
   ere(donkergrijzere),
   no_e(donkergrijs),
   st(donkergrijst),
   ste(donkergrijste)],nonadv,[],[]).

a([e(donkergroene),
   er(donkergroener),
   ere(donkergroenere),
   no_e(donkergroen),
   st(donkergroenst),
   ste(donkergroenste)],nonadv,[],[]).

a([e(donkerrode),
   er(donkerroder),
   ere(donkerrodere),
   no_e(donkerrood),
   st(donkerroodst),
   ste(donkerroodste)],nonadv,[],[]).

a([stof(donzen)],nonadv,[],[]).

a([e(donzige),
   er(donziger),
   ere(donzigere),
   no_e(donzig),
   st(donzigst),
   ste(donzigste)],nonadv,[],[]).

a([e(dode),
   no_e(dood),
   e(dooie)],padv,[],[hersen,mors]).

a([e(doodeenvoudige),
   er(doodeenvoudiger),
   ere(doodeenvoudigere),
   no_e(doodeenvoudig),
   st(doodeenvoudigst),
   ste(doodeenvoudigste)],adv,[],[]).

a([ge_e(doodgegane),
   ge_no_e(doodgegaan)],adv,[],[]).

a([ge_both(doodgelopen)],adv,[],[]).

a([ge_e(doodgemaakte),
   ge_no_e(doodgemaakt)],adv,[],[]).

a([ge_e(doodgemartelde),
   ge_no_e(doodgemarteld)],adv,[],[]).

a([ge_both(doodgereden)],adv,[],[]).

a([ge_both(doodgeschoten)],adv,[],[]).

a([ge_both(doodgeslagen)],adv,[],[]).

a([ge_both(doodgestoken)],adv,[],[]).

a([ge_both(doodgevallen)],adv,[],[]).

a([ge_both(doodgevroren)],adv,[],[]).

a([e(doodgewone),
   er(doodgewoner),
   ere(doodgewonere),
   no_e(doodgewoon),
   st(doodgewoonst),
   ste(doodgewoonste)],adv,[],[]).

a([ge_both(doodgezwegen)],adv,[],[]).

a([e(doodmoeë),
   er(doodmoeër),
   ere(doodmoeëre),
   no_e(doodmoe),
   st(doodmoest),
   ste(doodmoeste)],padv,
  [er_pp_sbar(van),
   er_pp_vp(van),
   pp(van)],[]).

a([e(doodnormale),
   no_e(doodnormaal)],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred(doodop)],padv,[],[]).

a([e(doodse),
   er(doodser),
   ere(doodsere),
   no_e(doods),
   st(doodst),
   ste(doodste)],nonadv,[],[]).

a([e(doodsbange),
   er(doodsbanger),
   ere(doodsbangere),
   no_e(doodsbang),
   st(doodsbangst),
   ste(doodsbangste)],padv,
  [object_sbar,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([e(doodsbenauwde),
   er(doodsbenauwder),
   ere(doodsbenauwdere),
   no_e(doodsbenauwd),
   st(doodsbenauwdst),
   ste(doodsbenauwdste)],padv,
  [pp(voor),
   object_sbar,
   object_vp],[]).

a([e(doodsbleke),
   er(doodsbleker),
   ere(doodsblekere),
   no_e(doodsbleek),
   st(doodsbleekst),
   ste(doodsbleekste)],padv,[],[]).

a([e(doodstille),
   er(doodstiller),
   ere(doodstillere),
   no_e(doodstil),
   st(doodstilst),
   ste(doodstilste)],padv,[],[]).

a([e(doodzieke),
   er(doodzieker),
   ere(doodziekere),
   no_e(doodziek),
   st(doodziekst),
   ste(doodziekste)],padv,
  [er_pp_sbar(van),
   er_pp_vp(van),
   pp(van)],[]).

a([pred(doodzonde)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(dove),
   er(dover),
   ere(dovere),
   no_e(doof),
   st(doofst),
   ste(doofste)],adv,
  [pp(voor)],[]).

a([e(doopsgezinde),
   no_e(doopsgezind)],nonadv,[],[]).

a([pred([door,de,war])],padv,[],[]).

a([ge_both(doorbakken)],padv,[],[]).

a([ge_e(doorbetaalde),
   ge_no_e(doorbetaald)],nonadv,[],[]).

a([ge_e(doorberekende),
   ge_no_e(doorberekend)],nonadv,[],[]).

a([ge_e(doorbladerde),
   ge_no_e(doorbladerd)],adv,[],[]).

a([no_e(doorbloed),
   e(doorbloede)],padv,[],[]).

a([ge_e(doorboorde),
   ge_no_e(doorboord)],adv,[],[]).

a([ge_both(doorbroken)],adv,[],[]).

a([ge_e(doordachte),
   er(doordachter),
   ere(doordachtere),
   ge_no_e(doordacht),
   st(doordachtst),
   ste(doordachtste)],adv,[],[]).

a([e(doordesemde),
   no_e(doordesemd)],padv,[],[]).

a([e(doordeweekse),
   no_e(doordeweeks)],adv,[],[]).

a([ge_e(doordrenkte),
   ge_no_e(doordrenkt)],adv,[],[]).

a([e(doordringende),
   er(doordringender),
   ere(doordringendere),
   no_e(doordringend),
   st(doordringendst),
   ste(doordringendste)],adv,
  [transitive],[]).

a([ge_both(doordrongen)],adv,
  [pp(van),
   er_pp_sbar(van),
   er_pp_vp(van)],[]).

a([ge_e(doorgebrachte),
   ge_no_e(doorgebracht)],adv,[],[]).

a([ge_both(doorgebroken)],adv,[],[]).

a([ge_e(doorgecomponeerde),
   ge_no_e(doorgecomponeerd)],padv,[],[]).

a([ge_no_e(doorgedacht),
   ge_e(doorgedachte)],adv,[],[]).

a([ge_e(doorgedraaide),
   ge_no_e(doorgedraaid)],adv,[],[]).

a([ge_both(doorgedreven)],adv,[],[]).

a([ge_both(doorgedrongen)],adv,[],[]).

a([ge_e(doorgegane),
   ge_no_e(doorgegaan)],adv,[],[]).

a([ge_both(doorgegeven)],adv,[],[]).

a([ge_e(doorgehakte),
   ge_no_e(doorgehakt)],padv,[],[]).

a([ge_both(doorgeklonken)],adv,[],[]).

a([ge_both(doorgekomen)],adv,[],[]).

a([ge_both(doorgeladen)],adv,[],[]).

a([ge_no_e(doorgeleid),
   ge_e(doorgeleide)],padv,[],[]).

a([ge_both(doorgelaten)],adv,[],[]).

a([ge_both(doorgelezen)],adv,[],[]).

a([ge_e(doorgelichte),
   ge_no_e(doorgelicht)],adv,[],[]).

a([ge_both(doorgelopen)],adv,[],[]).

a([ge_e(doorgemaakte),
   ge_no_e(doorgemaakt)],adv,[],[]).

a([ge_both(doorgenomen)],adv,[],[]).

a([ge_e(doorgeprate),
   ge_no_e(doorgepraat)],adv,[],[]).

a([ge_both(doorgereden)],adv,[],[]).

a([ge_e(doorgerekende),
   ge_no_e(doorgerekend)],nonadv,[],[]).

a([ge_e(doorgeroeste),
   ge_no_e(doorgeroest)],padv,[],[]).

a([ge_e(doorgerot),
   ge_no_e(doorgerotte)],padv,[],[]).

a([ge_e(doorgeschakelde),
   ge_no_e(doorgeschakeld)],padv,[],[]).

a([ge_e(doorgeschemerde),
   ge_no_e(doorgeschemerd)],adv,[],[]).

a([ge_both(doorgeschenen)],adv,[],[]).

a([ge_e(doorgescheurde),
   ge_no_e(doorgescheurd)],adv,[],[]).

a([ge_both(doorgeschoten)],nonadv,[],[]).

a([ge_both(doorgeslagen)],adv,[],[]).

a([ge_e(doorgeslikte),
   ge_no_e(doorgeslikt)],adv,[],[]).

a([ge_both(doorgesneden)],padv,[],[]).

a([ge_both(doorgesnoven)],padv,[],[]).

a([ge_e(doorgespeelde),
   ge_no_e(doorgespeeld)],adv,[],[]).

a([ge_both(doorgestoken)],padv,[],[]).

a([ge_e(doorgestuurde),
   ge_no_e(doorgestuurd)],adv,[],[]).

a([ge_both(doorgetrokken)],adv,[],[]).

a([ge_e(doorgevoerde),
   ge_no_e(doorgevoerd)],adv,[],[]).

a([ge_both(doorgevroren)],padv,[],[]).

a([ge_e(doorgewerkte),
   ge_no_e(doorgewerkt)],adv,[],[]).

a([e(doorgewinterde),
   ge_no_e(doorgewinterd)],nonadv,[],[]).

a([ge_e(doorgezakte),
   ge_no_e(doorgezakt)],adv,[],[]).

a([ge_e(doorgezette),
   ge_no_e(doorgezet)],adv,[],[]).

a([ge_e(doorgroefde),
   ge_no_e(doorgroefd)],padv,[],[]).

a([ge_e(doorgroeide),
   ge_no_e(doorgroeid)],padv,[],[]).

a([ge_e(doorgronde),
   ge_no_e(doorgrond)],adv,[],[]).

a([ge_e(doorkliefde),
   no_no_e(doorkliefd)],adv,[],[]).

a([ge_e(doorknede),
   ge_no_e(doorkneed)],padv,[],[]).

a([ge_e(doorkruiste),
   ge_no_e(doorkruist)],adv,[],[]).

a([ge_e(doorleefde),
   ge_no_e(doorleefd)],adv,[],[]).

a([ge_both(doorlezen)],adv,[],[]).

a([ge_both(doorlopen)],adv,[],[]).

a([both(doormidden)],adv,[],[]).

a([e(doornatte),
   no_e(doornat)],padv,[],[]).

a([e(doorploegde),
   no_e(doorploegd)],nonadv,[],[]).

a([both(doorregen)],padv,[],[]).

a([ge_e(doorregende),
   ge_no_e(doorregend)],padv,[],[]).

a([e(doorrookte),
   no_e(doorrookt)],nonadv,[],[]).

a([ge_both(doorschoten)],padv,[],[]).

a([e(doorschijnende),
   er(doorschijnender),
   ere(doorschijnendere),
   no_e(doorschijnend),
   st(doorschijnendst),
   ste(doorschijnendste)],adv,[],[]).

a([e(doorslaggevende),
   er(doorslaggevender),
   ere(doorslaggevendere),
   no_e(doorslaggevend),
   st(doorslaggevendst),
   ste(doorslaggevendste)],nonadv,
  [subject_sbar],[]).

a([ge_both(doorsneden)],padv,[],[]).

a([both(doorsnee)],nonadv,[],[]).

a([ge_e(doorspekte),
   ge_no_e(doorspekt)],adv,[pp(met)],[]).

a([ge_e(doorstane),
   ge_no_e(doorstaan)],adv,[],[]).

a([ge_e(doorstikte),
   ge_no_e(doorstikt)],padv,[],[]).

a([ge_e(doorstraalde),
   ge_no_e(doorstraald)],adv,[],[]).

a([ge_e(doorstreepte),
   ge_no_e(doorstreept)],adv,[],[]).

a([ge_e(doorstroomde),
   ge_no_e(doorstroomd)],adv,[],[]).

a([e(doortastende),
   er(doortastender),
   ere(doortastendere),
   no_e(doortastend),
   st(doortastendst),
   ste(doortastendste)],adv,[],[]).

a([e(doortimmerde),
   no_e(doortimmerd)],nonadv,[],[]).

a([e(doortrapte),
   er(doortrapter),
   ere(doortraptere),
   no_e(doortrapt),
   st(doortraptst),
   ste(doortraptste)],adv,[],[]).

a([ge_both(doortrokken)],adv,[],[]).

a([ge_both(doorverbonden)],adv,[],[]).

a([ge_both(doorverwezen)],adv,[],[]).

a([ge_both(doorvlochten)],adv,[],[]).

a([ge_e(doorvoede),ge_no_e(doorvoed)],padv,[],[]).

a([ge_e(doorvoelde),ge_no_e(doorvoeld)],padv,[],[]).

a([ge_e(doorwaakte),
   ge_no_e(doorwaakt)],nonadv,[],[]).

a([ge_e(doorweekte),
   er(doorweekter),
   ere(doorweektere),
   ge_no_e(doorweekt),
   st(doorweektst),
   ste(doorweektste)],adv,[],[]).

a([e(doorwrochte),
   no_e(doorwrocht)],nonadv,[],[]).

a([ge_e(doorzeefde),
   ge_no_e(doorzeefd)],padv,[],[]).

a([e(doorzichtige),
   er(doorzichtiger),
   ere(doorzichtigere),
   no_e(doorzichtig),
   st(doorzichtigst),
   ste(doorzichtigste)],nonadv,[],[]).

a([ge_both(doorzien)],adv,[],[]).

a([ge_e(doorzochte),
   ge_no_e(doorzocht)],adv,[],[]).

a([ge_both(doorzopen)],padv,[],[]).

a([ge_both(doorzwommen)],adv,[],[]).

a([e(doperse),
   no_e(dopers)],nonadv,[],[]).

a([e(dorre),
   er(dorder),
   ere(dordere),
   no_e(dor),
   st(dorst),
   ste(dorste)],nonadv,[],[]).

a([e(dorpse),
   er(dorpser),
   ere(dorpsere),
   no_e(dorps),
   st(dorpst),
   ste(dorpste)],adv,[],[]).

a([e(dorsale),no_e(dorsaal)],adv,[],[]).

a([e(dorstige),
   er(dorstiger),
   ere(dorstigere),
   no_e(dorstig),
   st(dorstigst),
   ste(dorstigste)],nonadv,[],[]).

a([pred(down)],padv,[],[]).

a([pred([down,and,out])],padv,[],[]).

a([pred([down,to,earth])],padv,[],[]).

a([no_e(draadloos),
   e(draadloze)],adv,[],[]).

a([e(draagbare),
   no_e(draagbaar)],nonadv,[],[]).

a([e(draaglijke),
   er(draaglijker),
   ere(draaglijkere),
   no_e(draaglijk),
   st(draaglijkst),
   ste(draaglijkste)],adv,[],[]).

a([ende(draaiende),
   end(draaiend)],adv,[],[]).

a([e(draconische),
   no_e(draconisch)],adv,[],[]).

a([e(dramatische),
   er(dramatischer),
   ere(dramatischere),
   no_e(dramatisch),
   st(dramatischt),
   ste(dramatischte)],adv,[subject_sbar,subject_vp],[]).

a([e(drassige),
   er(drassiger),
   ere(drassigere),
   no_e(drassig),
   st(drassigst),
   ste(drassigste)],nonadv,[],[]).

a([e(drastische),
   er(drastischer),
   ere(drastischere),
   no_e(drastisch),
   st(drastischt),
   ste(drastischte)],adv,[],[]).

a([ende(dreigende),
   er(dreigender),
   ere(dreigendere),
   end(dreigend),
   st(dreigendst),
   ste(dreigendste)],adv,
  [pp(met)],[]).

a([e(driedimensionale),
   no_e(driedimensionaal)],adv,[],[]).

a([e(driedubbele),
   no_e(driedubbel)],adv,[],[]).

a([e(drieste),
   er(driester),
   ere(driestere),
   no_e(driest)],adv,[],[]).

a([e(driftige),
   er(driftiger),
   ere(driftigere),
   no_e(driftig),
   st(driftigst),
   ste(driftigste)],adv,[],[]).

a([e(drijfnatte),
   er(drijfnatter),
   ere(drijfnattere),
   no_e(drijfnat),
   st(drijfnatst),
   ste(drijfnatste)],padv,[],[]).

a([e(dringende),
   er(dringender),
   ere(dringendere),
   no_e(dringend),
   st(dringendst),
   ste(dringendste)],adv,[],[]).

a([e(drinkbare),
   no_e(drinkbaar)],nonadv,[],[]).

a([e(droeve),
   er(droever),
   ere(droevere),
   no_e(droef),
   st(droefst),
   ste(droefste)],adv,[],[]).

a([e(droefgeestige),
   er(droefgeestiger),
   ere(droefgeestigere),
   no_e(droefgeestig),
   st(droefgeestigst),
   ste(droefgeestigste)],adv,[],[]).

a([e(droevige),
   er(droeviger),
   ere(droevigere),
   no_e(droevig),
   st(droevigst),
   ste(droevigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(dromerige),
   er(dromeriger),
   ere(dromerigere),
   no_e(dromerig),
   st(dromerigst),
   ste(dromerigste)],adv,[],[]).

a([e(drommelse),
   er(drommelser),
   ere(drommelsere),
   no_e(drommels),
   st(drommelst),
   ste(drommelste)],adv,[],[]).

a([both(dronken),
   er(dronkener),
   ere(dronkenere),
   st(dronkenst),
   ste(dronkenste)],padv,[],[]).

a([e(droge),
   er(droger),
   ere(drogere),
   no_e(droog),
   st(droogst),
   ste(droogste)],adv,[],[]).

a([ge_both(drooggevallen)],nonadv,[],[]).

a([pred(droogjes)],adv,[],[]).

a([e(drukke),
   er(drukker),
   ere(drukkere),
   no_e(druk),
   st(drukst),
   ste(drukste)],adv,
  [er_pp_vp(met),
   pp(met),
   pp(over)],[]).

a([e(drukkende),
   er(drukkender),
   ere(drukkendere),
   no_e(drukkend),
   st(drukkendst),
   ste(drukkendste)],adv,[],[]).

a([pred(dunnetjes)],adv,[],[]).

a([both(dutyfree),
   both('duty-free'),
   both([duty,free])],nonadv,[],[]).

a([both(dry)],nonadv,[],[]).

a([e(duale),no_e(duaal)],nonadv,[],[]).

a([e(dualistische),
   no_e(dualistisch)],nonadv,[],[]).

a([e(dubbele),
   no_e(dubbel)],adv,[],[]).

a([both(dubbeldeks)],nonadv,[],[]).

a([e(dubbelzinnige),
   er(dubbelzinniger),
   ere(dubbelzinnigere),
   no_e(dubbelzinnig),
   st(dubbelzinnigst),
   ste(dubbelzinnigste)],padv,[],[]).

a([e(dubieuze),
   er(dubieuzer),
   ere(dubieuzere),
   no_e(dubieus),
   st(dubieust),
   ste(dubieuste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(duchtige),
   er(duchtiger),
   ere(duchtigere),
   no_e(duchtig),
   st(duchtigst),
   ste(duchtigste)],adv,[],[]).

a([e(duffe),
   er(duffer),
   ere(duffere),
   no_e(duf),
   st(dufst),
   ste(dufste)],adv,[],[]).

a([e(duidelijke),
   er(duidelijker),
   ere(duidelijkere),
   no_e(duidelijk),
   st(duidelijkst),
   ste(duidelijkste)],adv,
  [subject_sbar_no_het,
   subject_vp_no_het,
   er_pp_sbar(over)
  ],[over]).

a([e(duistere),
   er(duisterder),
   ere(duisterdere),
   no_e(duister),
   st(duisterst),
   ste(duisterste)],adv,
  [subject_sbar],  % waarom dat zo is is duister
  []).

a([e(duivelse),
   er(duivelser),
   ere(duivelsere),
   no_e(duivels),
   st(duivelst),
   ste(duivelste)],adv,[],[]).

a([e(duvelse),
   er(duvelser),
   ere(duvelsere),
   no_e(duvels),
   st(duvelst),
   ste(duvelste)],adv,[],[]).

a([e(duizelige),
   er(duizeliger),
   ere(duizeligere),
   no_e(duizelig),
   st(duizeligst),
   ste(duizeligste)],padv,[],[]).

a([e(duizelingwekkende),
   er(duizelingwekkender),
   ere(duizelingwekkendere),
   no_e(duizelingwekkend),
   st(duizelingwekkendst),
   ste(duizelingwekkendste)],adv,[],[]).

a([e(dunne),
   er(dunner),
   ere(dunnere),
   no_e(dun),
   st(dunst),
   ste(dunste)],adv,[],[rag]).

a([e(dusdanige),
   no_e(dusdanig)],adv,
  [subject_sbar],[]).

a([e(dure),
   er(duurder),
   ere(duurdere),
   no_e(duur),
   st(duurst),
   ste(duurste)],adv,[],[]).

a([e(duurbetaalde),
   no_e(duurbetaald)],nonadv,[],[]).

a([e(duurzame),
   er(duurzamer),
   ere(duurzamere),
   no_e(duurzaam),
   st(duurzaamst),
   ste(duurzaamste)],adv,[],[]).

a([e(dwaze),
   er(dwazer),
   ere(dwazere),
   no_e(dwaas),
   st(dwaast),
   ste(dwaaste)],adv,[],[]).

a([e(dwangmatige),
   no_e(dwangmatig)],adv,[],[]).

a([e(dwarse),
   er(dwarser),
   ere(dwarsere),
   no_e(dwars),
   st(dwarst),
   ste(dwarste)],adv,[],[]).

a([ende(dwingende),
   er(dwingender),
   ere(dwingendere),
   end(dwingend),
   st(dwingendst),
   ste(dwingendste)],adv,[],[]).

a([e(dynamische),
   er(dynamischer),
   ere(dynamischere),
   no_e(dynamisch),
   st(dynamischt),
   ste(dynamischte)],adv,[],[]).

a([e(dynastieke),
   no_e(dynastiek)],nonadv,[],[]).

a([e(echte),
   er(echter),
   ere(echtere),
   no_e(echt),
   st(echtst),
   ste(echtste)],adv,[],[]).

a([e(echtelijke),
   no_e(echtelijk)],nonadv,[],[]).

a([e(eclatante),
   no_e(eclatant)],nonadv,[],[]).

a([e(ecologische),
   no_e(ecologisch)],adv,[],[]).

a([e(economische),
   e(ekonomische),
   er(economischer),
   er(ekonomischer),
   ere(economischere),
   ere(ekonomischere),
   no_e(economisch),
   no_e(ekonomisch),
   st(economischt),
   st(ekonomischt),
   ste(economischte),
   ste(ekonomischte)],adv,[],
  [h(macro),
   h(micro),
   h(financieel),
   sociaal]).

a([e(edele),
   er(edeler),
   ere(edelere),
   no_e(edel),
   st(edelst),
   ste(edelste)],adv,[],[]).

a([e(edelachtbare),
   er(edelachtbaarder),
   ere(edelachtbaardere),
   no_e(edelachtbaar),
   st(edelachtbaarst),
   ste(edelachtbaarste)],nonadv,[],[]).

a([e(edelmoedige),
   er(edelmoediger),
   ere(edelmoedigere),
   no_e(edelmoedig),
   st(edelmoedigst),
   ste(edelmoedigste)],adv,[],[]).

a([e(educatieve),
   er(educatiever),
   ere(educatievere),
   no_e(educatief),
   st(educatiefst),
   ste(educatiefste)],adv,[],[]).

a([e([een,of,andere]),
   e([één,of,andere]),
   no_e([een,of,ander]),
   no_e([één,of,ander])],nonadv,[],[]).

a([pred([één,tegen,één])],adv,[],[]).

a([no_e(eendaags),
   e(eendaagse)],nonadv,[],[]).

a([no_e(eender),
   e(eendere)],adv,[],[]).

a([e(eendrachtige),
   er(eendrachtiger),
   ere(eendrachtigere),
   no_e(eendrachtig),
   st(eendrachtigst),
   ste(eendrachtigste)],adv,[],[]).

a([e(eenduidige),
   no_e(eenduidig)],adv,[],[]).

a([e(eeneiige),
   no_e(eeneiig)],nonadv,[],[]).

a([ge_e(eengemaakte),
   ge_no_e(eengemaakt)],padv,[],[]).

a([e(eenjarige),
   no_e(eenjarig)],nonadv,[],[]).

a([e(eenmalige),
   no_e(eenmalig)],adv,[],[]).

a([e(eenparige),no_e(eenparig)],padv,[],[]).

a([both(eenpersoons)],nonadv,[],[]).

a([e(eensgezinde),
   no_e(eensgezind)],adv,
  [er_pp_sbar(over),
   er_pp_vp(over),
   pp(in),
   pp(over)],[]).

a([e(eenstemmige),
   no_e(eenstemmig)],adv,[],[]).

a([e(eentonige),
   er(eentoniger),
   ere(eentonigere),
   no_e(eentonig),
   st(eentonigst),
   ste(eentonigste)],adv,[],[]).

a([e(eenvormige),
   er(eenvormiger),
   ere(eenvormigere),
   no_e(eenvormig),
   st(eenvormigst),
   ste(eenvormigste)],nonadv,[],[]).

a([e(eenvoudige),
   er(eenvoudiger),
   ere(eenvoudigere),
   no_e(eenvoudig),
   st(eenvoudigst),
   ste(eenvoudigste)],adv,
  [subject_vp],[]).

a([e(eenzame),
   er(eenzamer),
   ere(eenzamere),
   no_e(eenzaam),
   st(eenzaamst),
   ste(eenzaamste)],padv,[],[]).

a([e(eenzelvige),
   er(eenzelviger),
   ere(eenzelvigere),
   no_e(eenzelvig),
   st(eenzelvigst),
   ste(eenzelvigste)],adv,[],[]).

a([e(eenzijdige),
   er(eenzijdiger),
   ere(eenzijdigere),
   no_e(eenzijdig),
   st(eenzijdigst),
   ste(eenzijdigste)],adv,[],[]).

a([e(eerbare),
   er(eerbaarder),
   ere(eerbaardere),
   no_e(eerbaar),
   st(eerbaarst),
   ste(eerbaarste)],adv,[],[]).

a([e(eerbiedige),
   er(eerbiediger),
   ere(eerbiedigere),
   no_e(eerbiedig),
   st(eerbiedigst),
   ste(eerbiedigste)],adv,[],[]).

a([e(eerbiedwaardige),
   er(eerbiedwaardiger),
   ere(eerbiedwaardigere),
   no_e(eerbiedwaardig),
   st(eerbiedwaardigst),
   ste(eerbiedwaardigste)],nonadv,[],[]).

a([er(eerder),
   ere(eerdere)],adv,[],[]).

a([e(eerlijke),
   er(eerlijker),
   ere(eerlijkere),
   no_e(eerlijk),
   st(eerlijkst),
   ste(eerlijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(eerstgenoemde),
   no_e(eerstgenoemd)],nonadv,[],[]).

a([ende(eerstkomende),
   end(eerstkomend)],nonadv,[],[]).

a([e(eerstvolgende),
   no_e(eerstvolgend)],nonadv,[],[]).

a([e(eertijdse),
   no_e(eertijds)],adv,[],[]).

a([e(eervolle),
   er(eervoller),
   ere(eervollere),
   no_e(eervol),
   st(eervolst),
   ste(eervolste)],adv,[subject_sbar,
                        subject_vp],[]).

a([e(eerwaarde),
   er(eerwaarder),
   ere(eerwaardere),
   no_e(eerwaard),
   st(eerwaardst),
   ste(eerwaardste)],nonadv,[],[]).

a([e(eerzame),
   no_e(eerzaam)],nonadv,[],[]).

a([e(eerzuchtige),
   er(eerzuchtiger),
   ere(eerzuchtigere),
   no_e(eerzuchtig),
   st(eerzuchtigst),
   ste(eerzuchtigste)],adv,[],[]).

a([e(eetbare),
   no_e(eetbaar)],nonadv,[],[]).

a([er(eeuwiger),
   ere(eeuwigere),
   e(eeuwige),
   no_e(eeuwig)],adv,[],[]).

a([e(eeuwigdurende),
   no_e(eeuwigdurend)],nonadv,[],[]).

a([e(eeuwse),
   no_e(eeuws)],adv,[],[]).

a([e(effectieve),
   e(effektieve),
   er(effectiever),
   er(effektiever),
   ere(effectievere),
   ere(effektievere),
   no_e(effectief),
   no_e(effektief),
   st(effectiefst),
   st(effektiefst),
   ste(effectiefste),
   ste(effektiefste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(effen)],adv,[],[]).

a([e(efficiënte),
   er(efficiënter),
   ere(efficiëntere),
   no_e(efficiënt),
   st(efficiëntst),
   ste(efficiëntste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(egale),
   er(egaler),
   ere(egalere),
   no_e(egaal),
   st(egaalst),
   ste(egaalste)],adv,[],[]).

a([e(egocentrische),
   er(egocentrischer),
   ere(egocentrischere),
   no_e(egocentrisch),
   st(egocentrischt),
   ste(egocentrischte)],adv,[],[]).

a([e(egoïstische),
   er(egoïstischer),
   ere(egoïstischere),
   no_e(egoïstisch),
   st(egoïstischt),
   ste(egoïstischte)],adv,[],[]).

a([both(eigen),
   st(eigenst),
   ste(eigenste)],nonadv,
  [so_pp(aan),
   pp(met),
   so_np],[]).

a([e(eigenaardige),
   er(eigenaardiger),
   ere(eigenaardigere),
   no_e(eigenaardig),
   st(eigenaardigst),
   ste(eigenaardigste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(eigengemaakte),
   ge_no_e(eigengemaakt)],nonadv,[],[]).

a([e(eigengereide),
   er(eigengereider),
   ere(eigengereidere),
   no_e(eigengereid),
   st(eigengereidst),
   ste(eigengereidste)],padv,[],[]).

a([e(eigenhandige),
   no_e(eigenhandig)],adv,[],[]).

a([e(eigenlijke),
   no_e(eigenlijk)],sentadv,[],[]).

a([e(eigenmachtige),
   er(eigenmachtiger),
   ere(eigenmachtigere),
   no_e(eigenmachtig),
   st(eigenmachtigst),
   ste(eigenmachtigste)],adv,[],[]).

a([e(eigentijdse),
   no_e(eigentijds),
   er(eigentijdser),
   ere(eigentijdsere),
   st(eigentijdst),
   ste(eigentijdste)],adv,
  [subject_vp],[]).

a([e(eigenwijze),
   er(eigenwijzer),
   ere(eigenwijzere),
   no_e(eigenwijs),
   st(eigenwijst),
   ste(eigenwijste)],adv,[],[]).

a([e(eigenzinnige),
   er(eigenzinniger),
   ere(eigenzinnigere),
   no_e(eigenzinnig),
   st(eigenzinnigst),
   ste(eigenzinnigste)],adv,[],[]).

a([stof(eikenhouten)],nonadv,[],[]).

a([stof(eiken)],nonadv,[],[]).

a([e(eindelijke),
   no_e(eindelijk)],adv,[],[]).

a([e(eindeloze),
   no_e(eindeloos)],adv,[],[]).

a([e(eindige),
   no_e(eindig)],nonadv,[],[]).

a([stof(elastieken)],nonadv,[],[]).

a([e(elastische),
   er(elastischer),
   ere(elastischere),
   no_e(elastisch),
   st(elastischt),
   ste(elastischte)],nonadv,[],[]).

a([e(elegante),
   er(eleganter),
   ere(elegantere),
   no_e(elegant),
   st(elegantst),
   ste(elegantste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(electorale),
   no_e(electoraal)],adv,[],[]).

a([e(electrische),
   e(elektrische),
   er(electrischer),
   er(elektrischer),
   ere(electrischere),
   ere(elektrischere),
   no_e(electrisch),
   no_e(elektrisch),
   st(electrischt),
   st(elektrischt),
   ste(electrischte),
   ste(elektrischte)],adv,[],[]).

a([e(electromagnetische),
   e(elektromagnetische),
   no_e(electromagnetisch),
   no_e(elektromagnetisch)],nonadv,[],[]).

a([e(electrotechnische),
   e(elektrotechnische),
   no_e(electrotechnisch),
   no_e(elektrotechnisch)],nonadv,[],[]).

a([e(electronische),
   e(elektronische),
   er(electronischer),
   er(elektronischer),
   ere(electronischere),
   ere(elektronischere),
   no_e(electronisch),
   no_e(elektronisch),
   st(electronischt),
   st(elektronischt),
   ste(electronischte),
   ste(elektronischte)],adv,[],[]).

a([e(elementaire),
   er(elementairder),
   ere(elementairdere),
   no_e(elementair),
   st(elementairst),
   ste(elementairste)],adv,[],[]).

a([e(elitaire),
   no_e(elitair)],nonadv,[],[]).

a([prefix(elite)],nonadv,[],[]).

a([e(ellendige),
   er(ellendiger),
   ere(ellendigere),
   no_e(ellendig),
   st(ellendigst),
   ste(ellendigste)],adv,[],[]).

a([e(emancipatorische),
   no_e(emancipatorisch)],nonadv,[],[]).

a([e(embryonale),
   no_e(embryonaal)],nonadv,[],[]).

a([both(emeritus)],nonadv,[],[]).

a([e(eminente),
   er(eminenter),
   ere(eminentere),
   no_e(eminent),
   st(eminentst),
   ste(eminentste)],adv,[],[]).

a([e(emotionele),
   er(emotioneler),
   ere(emotionelere),
   no_e(emotioneel),
   st(emotioneelst),
   ste(emotioneelste)],adv,[],[]).

a([e(empirische),
   er(empirischer),
   ere(empirischere),
   no_e(empirisch),
   st(empirischt),
   ste(empirischte)],adv,[],[]).

a([pred([en,vogue])],padv,[],[]).

a([e(endemische),no_e(endemisch)],nonadv,[],[]).

a([e(endocriene),
   no_e(endocrien)],nonadv,[],[]).

a([e(endogene),
   no_e(endogeen)],adv,[],[]).

a([e(ene)],nonadv,[],[]).

a([e(energetische),
   no_e(energetisch)],adv,[],[]).

a([e(energieke),
   er(energieker),
   ere(energiekere),
   no_e(energiek),
   st(energiekst),
   ste(energiekste)],adv,[],[]).

a([e(enerverende),
   no_e(enerverend)],adv,[],[]).

a([e(enge),
   er(enger),
   ere(engere),
   no_e(eng),
   st(engst),
   ste(engste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([stof(enige),
   no_e(enig),
   ste(enigste),
   st(enigst)],adv,
  [pp(in),
   subject_sbar,
   subject_vp],[]).

a([both(enigerlei)],nonadv,[],[]).

a([e(enkele),
   no_e(enkel)],adv,[],[]).

a([e(enkelvoudige),
   er(enkelvoudiger),
   ere(enkelvoudigere),
   no_e(enkelvoudig),
   st(enkelvoudigst),
   ste(enkelvoudigste)],adv,[],[]).

a([e(enorme),
   er(enormer),
   ere(enormere),
   no_e(enorm),
   st(enormst),
   ste(enormste)],adv,[],[]).

a([both(entartete)],nonadv,[],[]).

a([e(enthoesiaste),
   e(enthousiaste),
   e(entoesiaste),
   e(entousiaste),
   er(enthoesiaster),
   er(enthousiaster),
   er(entoesiaster),
   er(entousiaster),
   ere(enthoesiastere),
   ere(enthousiastere),
   ere(entoesiastere),
   ere(entousiastere),
   no_e(enthousiast),
   no_e(enthoesiast),
   no_e(entoesiast),
   no_e(entousiast)],adv,
  [er_pp_sbar(over),
   pp(over),
   object_sbar
  ],[]).

a([e(equivalente),
   no_e(equivalent)],nonadv,[],[]).

a([e(epische),
   no_e(episch)],adv,[],[]).

a([e(erbarmelijke),
   er(erbarmelijker),
   ere(erbarmelijkere),
   no_e(erbarmelijk),
   st(erbarmelijkst),
   ste(erbarmelijkste)],adv,[],[]).

a([e(erfelijke),
   no_e(erfelijk)],adv,[],[]).

a([e(erge),
   er(erger),
   ere(ergere),
   no_e(erg),
   st(ergst),
   ste(ergste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(ergerlijke),
   er(ergerlijker),
   ere(ergerlijkere),
   no_e(ergerlijk),
   st(ergerlijkst),
   ste(ergerlijkste)],adv,
  [subject_sbar],[]).

a([ge_e(erkende),
   ge_no_e(erkend)],adv,[],[]).

a([e(erkentelijke),
   er(erkentelijker),
   ere(erkentelijkere),
   no_e(erkentelijk),
   st(erkentelijkst),
   ste(erkentelijkste)],nonadv,
  [object_sbar,
   object_vp,
   pp(voor),
   er_pp_sbar(voor)],[]).

a([e(ernstige),
   er(ernstiger),
   ere(ernstigere),
   no_e(ernstig),
   st(ernstigst),
   ste(ernstigste)],adv,
  [subject_sbar,
   pp(voor)],[]).

a([pred([erop,of,eronder])],nonadv,[],[]).

a([e(erotische),
   er(erotischer),
   ere(erotischere),
   no_e(erotisch),
   st(erotischt),
   ste(erotischte)],adv,[],[]).

a([e(erudiete),
   no_e(erudiet)],nonadv,[],[]).

a([ge_both(ervaren),
   er(ervarener),
   ere(ervarenere),
   st(ervarenst),
   ste(ervarenste)],adv,[],[]).

a([e(eschatologische),
   no_e(eschatologisch)],adv,[],[]).

a([e(esoterische),
   no_e(esoterisch)],nonadv,[],[]).

a([e(essentiële),
   no_e(essentieel)],adv,
  [subject_sbar_no_het,
   subject_vp_no_het,
   pp(aan),
   pp(bij),
   pp(in),
   pp(voor)],[h(niet)]).

a([e(estetische),
   e(esthetische),
   er(estetischer),
   er(esthetischer),
   ere(estetischere),
   ere(esthetischere),
   no_e(estetisch),
   no_e(esthetisch),
   st(estetischt),
   st(esthetischt),
   ste(estetischte),
   ste(esthetischte)],adv,[],[]).

a([e(eterische),
   e(etherische),
   no_e(eterisch),
   no_e(etherisch)],nonadv,[],[]).

a([e(ethische),
   e(etische),
   er(ethischer),
   er(etischer),
   ere(ethischere),
   ere(etischere),
   no_e(ethisch),
   no_e(etisch),
   st(ethischt),
   st(etischt),
   ste(ethischte),
   ste(etischte)],adv,[],[]).

a([e(etnische),
   no_e(etnisch)],adv,[],[h(multi)]).

a([e(euforische),
   no_e(euforisch)],padv,[],[]).

a([e(euvele),
   er(euveler),
   ere(euvelere),
   no_e(euvel),
   st(euvelst),
   ste(euvelste)],adv,[],[]).

a([e(evangelische),
   no_e(evangelisch)],adv,[],[]).

a([both(even)],tmpadv,[],[]).

a([both(eventjes)],tmpadv,[],[]).

a([e(evenredige),
   er(evenrediger),
   ere(evenredigere),
   no_e(evenredig),
   st(evenredigst),
   ste(evenredigste)],adv,
  [pp(met)],[]).

a([e(eventuele),
   no_e(eventueel)],adv,[],[]).

a([e(evenwichtige),
   er(evenwichtiger),
   ere(evenwichtigere),
   no_e(evenwichtig),
   st(evenwichtigst),
   ste(evenwichtigste)],adv,[],[]).

a([e(evenwijdige),
   er(evenwijdiger),
   ere(evenwijdigere),
   no_e(evenwijdig),
   st(evenwijdigst),
   ste(evenwijdigste)],adv,
  [pp(aan),
   pp(met)],[]).

a([e(evenzovele),
   no_e(evenzoveel)],adv,[],[]).

a([e(evidente),
   er(evidenter),
   ere(evidentere),
   no_e(evident),
   st(evidentst),
   ste(evidentste)],adv,
  [subject_sbar],[]).

a([both([ex,vivo])],adv,[],[]).

a([e(exacte),
   er(exacter),
   ere(exactere),
   no_e(exact),
   st(exactst),
   ste(exactste)],adv,[],[]).

a([e(excellente),
   er(excellenter),
   ere(excellentere),
   no_e(excellent),
   st(excellentst),
   ste(excellentste)],adv,[],[]).

a([e(excentrieke),
   er(excentrieker),
   ere(excentriekere),
   no_e(excentriek),
   st(excentriekst),
   ste(excentriekste)],adv,[],[]).

a([e(excessieve),
   er(excessiever),
   ere(excessievere),
   no_e(excessief),
   st(excessiefst),
   ste(excessiefste)],adv,[],[]).

a([e(exclusieve),
   er(exclusiever),
   ere(exclusievere),
   no_e(exclusief),
   st(exclusiefst),
   ste(exclusiefste)],adv,[],[]).

a([e(eksemplarische),
   e(exemplarische),
   no_e(eksemplarisch),
   no_e(exemplarisch)],adv,[],[]).

a([e(existentiële),
   no_e(existentieel)],adv,[],[]).

a([e(exorbitante),
   no_e(exorbitant)],adv,[],[]).

a([e(exotische),
   er(exotischer),
   ere(exotischere),
   no_e(exotisch),
   st(exotischt),
   ste(exotischte)],adv,[],[]).

a([e(expansieve),
   er(expansiever),
   ere(expansievere),
   no_e(expansief),
   st(expansiefst),
   ste(expansiefste)],nonadv,[],[]).

a([e(experimentele),
   no_e(experimenteel)],adv,[],[]).

a([e(expliciete),
   no_e(expliciet)],adv,[],[]).

a([e(explosieve),
   er(explosiever),
   ere(explosievere),
   no_e(explosief),
   st(explosiefst),
   ste(explosiefste)],adv,[],[]).

a([e(exponentiele),
   no_e(exponentieel)],adv,[],[]).

a([e(expresse),
   no_e(expres)],adv,[],[]).

a([e(expressieve),
   er(expressiever),
   ere(expressievere),
   no_e(expressief),
   st(expressiefst),
   ste(expressiefste)],adv,[],[]).

a([e(exquise),
   no_e(exquis)],adv,[],[]).

a([e(extatische),
   er(extatischer),
   ere(extatischere),
   no_e(extatisch),
   st(extatischt),
   ste(extatischte)],adv,[],[]).

a([e(externe),
   no_e(extern)],adv,[],[]).

a([postn_both(extra)],adv,[],[]).

a([postn_both([extra,vergine])],nonadv,[],[]).

a([e(extramurale),
   no_e(extramuraal)],adv,[],[]).

a([e(extravagante),
   no_e(extravagant)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(extraverte),
   no_e(extravert)],adv,[],[]).

a([e(extreme),
   er(extremer),
   ere(extremere),
   no_e(extreem),
   st(extreemst),
   ste(extreemste)],adv,[],[]).

a([e('extreem-rechtse'),
   no_e('extreem-rechts')],adv,[],[]).

a([e(extremistische),
   no_e(extremistisch)],adv,[],
  [h(links),
   h(rechts)]).

a([e(exuberante),
   no_e(exuberant)],adv,[],[]).

a([e(fabelachtige),
   er(fabelachtiger),
   ere(fabelachtigere),
   no_e(fabelachtig),
   st(fabelachtigst),
   ste(fabelachtigste)],adv,[],[]).

a([e(failliete),
   no_e(failliet)],adv,[],[]).

a([no_e(fair),
   e(faire),
   er(fairder),
   ere(fairdere),
   st(fairst),
   ste(fairste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred(fake)],nonadv,[],[]).

a([e(faliekante),
   no_e(faliekant),
   e(falikante),
   no_e(falikant)],adv,[],[]).

a([e(fameuze),
   er(fameuzer),
   ere(fameuzere),
   no_e(fameus),
   st(fameust),
   ste(fameuste)],adv,[],[]).

a([e(familiale),
   er(familialer),
   ere(familialere),
   no_e(familiaal),
   st(familiaalst),
   ste(familiaalste)],padv,[],[]).

a([e(familiaire),
   er(familiairder),
   ere(familiairdere),
   no_e(familiair),
   st(familiairst),
   ste(familiairste)],adv,[],[]).

a([e(fanatieke),
   er(fanatieker),
   ere(fanatiekere),
   no_e(fanatiek),
   st(fanatiekst),
   ste(fanatiekste)],adv,[],[]).

a([both(fancy)],adv,[],[]).

a([e(fantasierijke),
   e(fantazierijke),
   er(fantasierijker),
   er(fantazierijker),
   ere(fantasierijkere),
   ere(fantazierijkere),
   no_e(fantasierijk),
   no_e(fantazierijk),
   st(fantasierijkst),
   st(fantazierijkst),
   ste(fantasierijkste),
   ste(fantazierijkste)],padv,[],[]).

a([e(fantastische),
   er(fantastischer),
   ere(fantastischere),
   no_e(fantastisch),
   st(fantastischt),
   ste(fantastischte)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(farmaceutische),
   no_e(farmaceutisch)],nonadv,[],[]).

a([ende(fascinerende),
   end(fascinerend)],padv,
  [transitive,
   subject_vp,
   subject_sbar],[]).

a([e(fascistische),
   er(fascistischer),
   ere(fascistischere),
   no_e(fascistisch),
   st(fascistischt),
   ste(fascistischte)],adv,[],[neo]).

a([e(fascistoïde),
   no_e(fascistoïd),
   er(fascistoïder),
   ere(fascistoïdere)],nonadv,[],[]).

a([e(fatale),
   er(fataler),
   ere(fatalere),
   no_e(fataal),
   st(fataalst),
   ste(fataalste)],adv,[],[]).

a([e(fatalistische),
   er(fatalistischer),
   ere(fatalistischere),
   no_e(fatalistisch),
   st(fatalistischt),
   ste(fatalistischte)],padv,[],[]).

a([e(fatsoenlijke),
   er(fatsoenlijker),
   ere(fatsoenlijkere),
   no_e(fatsoenlijk),
   st(fatsoenlijkst),
   ste(fatsoenlijkste)],adv,[subject_sbar,
                             subject_vp],[]).

a([e(favoriete),
   no_e(favoriet)],nonadv,
  [pp(bij)],[]).

a([e(federale),
   no_e(federaal)],adv,[],[]).

a([e(feestelijke),
   er(feestelijker),
   ere(feestelijkere),
   no_e(feestelijk),
   st(feestelijkst),
   ste(feestelijkste)],adv,[],[]).

a([er(feillozer),
   ere(feillozere),
   e(feilloze),
   no_e(feilloos)],adv,[],[]).

a([e(feitelijke),
   no_e(feitelijk)],adv,[],[]).

a([e(felle),
   er(feller),
   ere(fellere),
   no_e(fel),
   st(felst),
   ste(felste)],adv,
  [er_pp_sbar(tegen),
   er_pp_vp(tegen),
   pp(tegen)],[]).

a([ge_no_e(felbegeerd),
   ge_e(felbegeerde)],nonadv,[],[]).

a([ge_no_e(felgekleurd),
   ge_e(felgekleurde)],nonadv,[],[]).

a([e(feminiene),
   no_e(feminien)],nonadv,[],[]).

a([e(feministische),
   er(feministischer),
   ere(feministischere),
   no_e(feministisch),
   st(feministischt),
   ste(feministischte)],adv,[],[]).

a([e(fenomenale),
   er(fenomenaler),
   ere(fenomenalere),
   no_e(fenomenaal),
   st(fenomenaalst),
   ste(fenomenaalste)],adv,[],[]).

a([e(fenomenologische),
   no_e(fenomenologisch)],adv,[],[]).

a([e(feodale),
   er(feodaler),
   ere(feodalere),
   no_e(feodaal),
   st(feodaalst),
   ste(feodaalste)],adv,[],[]).

a([e(ferme),
   er(fermer),
   ere(fermere),
   no_e(ferm),
   st(fermst),
   ste(fermste)],adv,[],[]).

a([e(fervente),
   er(ferventer),
   ere(ferventere),
   no_e(fervent),
   st(ferventst),
   ste(ferventste)],adv,[],[]).

a([prefix(fictie)],nonadv,[],[non,
                              h(non)]).

a([e(fictieve),
   e(fiktieve),
   er(fictiever),
   er(fiktiever),
   ere(fictievere),
   ere(fiktievere),
   no_e(fictief),
   no_e(fiktief),
   st(fictiefst),
   st(fiktiefst),
   ste(fictiefste),
   ste(fiktiefste)],adv,[],[]).

a([e(fiere),
   er(fierder),
   ere(fierdere),
   no_e(fier),
   st(fierst),
   ste(fierste)],adv,
  [pp(op),
   er_pp_sbar(op),
   er_pp_vp(op),
   object_sbar,  % Vlaams
   object_vp     % Vlaams
  ],[]).

a([e(figuratieve),
   no_e(figuratief)],adv,[],[]).

a([e(figuurlijke),
   no_e(figuurlijk)],adv,[],[]).

a([e(fijne),
   er(fijner),
   ere(fijnere),
   no_e(fijn),
   st(fijnst),
   ste(fijnste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(fijnbesnaarde),
   no_e(fijnbesnaard)],adv,[],[]).

a([ge_e(fijngehakte),
   ge_no_e(fijngehakt)],adv,[],[]).

a([ge_both(fijngemalen)],adv,[],[]).

a([ge_both(fijngesneden)],nonadv,[],[]).

a([e(fijngeraspte),
   no_e(fijngeraspt)],nonadv,[],[]).

a([e(fijngevoelige),
   er(fijngevoeliger),
   ere(fijngevoeligere),
   no_e(fijngevoelig),
   st(fijngevoeligst),
   ste(fijngevoeligste)],adv,[],[]).

a([e(fijnmazige),
   no_e(fijnmazig),
   er(fijnmaziger),
   ere(fijnmazigere)],adv,[],[]).   

a([pred(fijntjes)],adv,[],[]).

a([e(fijnzinnige),
   er(fijnzinniger),
   ere(fijnzinnigere),
   no_e(fijnzinnig),
   st(fijnzinnigst),
   ste(fijnzinnigste)],adv,[],[]).

a([e(fikse),
   er(fikser),
   ere(fiksere),
   no_e(fiks),
   st(fikst),
   ste(fikste)],adv,[],[]).

a([e(filosofische),
   e(filozofische),
   er(filosofischer),
   er(filozofischer),
   ere(filosofischere),
   ere(filozofischere),
   no_e(filosofisch),
   no_e(filozofisch),
   st(filosofischt),
   st(filozofischt),
   ste(filosofischte),
   ste(filozofischte)],adv,[],[]).

a([e(finale),
   no_e(finaal)],adv,[],[]).

a([e(financiële),
   no_e(financieel)],adv,[],[]).

a([e(fiscale),
   e(fiskale),
   no_e(fiscaal),
   no_e(fiskaal)],adv,[],[]).

a([e(fitte),
   er(fitter),
   ere(fittere),
   no_e(fit),
   st(fitst),
   ste(fitste)],padv,[],[top]).

a([e(flagrante),
   er(flagranter),
   ere(flagrantere),
   no_e(flagrant),
   st(flagrantst),
   ste(flagrantste)],adv,[],[]). % VL iets flagrant schenden

a([e(flamboyante),
   no_e(flamboyant)],adv,[],[]).

a([both(flanellen)],nonadv,[],[]).

a([e(flauwe),
   er(flauwer),
   ere(flauwere),
   no_e(flauw),
   st(flauwst),
   ste(flauwste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_both(flauwgevallen)],adv,[],[]).

a([pred(flauwtjes)],adv,[],[]).

a([e(fletse),
   er(fletser),
   ere(fletsere),
   no_e(flets),
   st(fletst),
   ste(fletste)],adv,[],[]).

a([e(fleurige),
   er(fleuriger),
   ere(fleurigere),
   no_e(fleurig),
   st(fleurigst),
   ste(fleurigste)],adv,[],[]).

a([e(flexibele),
   er(flexibeler),
   ere(flexibelere),
   no_e(flexibel),
   st(flexibelst),
   ste(flexibelste)],adv,[],[]).

a([e(flinke),
   er(flinker),
   ere(flinkere),
   no_e(flink),
   st(flinkst),
   ste(flinkste)],adv,[],[]).

a([e(flinterdunne),
   no_e(flinterdun)],nonadv,[],[]).

a([e(florissante),
   no_e(florissant)],adv,[],[]).

a([both(fluwelen)],nonadv,[],[]).

a([e(fluwelige),
   er(fluweliger),
   ere(fluweligere),
   no_e(fluwelig),
   st(fluweligst),
   ste(fluweligste)],nonadv,[],[]).

a([e(foetale),
   no_e(foetaal)],nonadv,[],[]).

a([e(folkloristische),
   no_e(folkloristisch)],nonadv,[],[]).

a([e(forensische),
   both(forensisch)],nonadv,[],[]).

a([e(forfetaire),
   no_e(forfetair),
   e(forfaitaire),
   no_e(forfaitair)],adv,[],[]).

a([e(formele),
   er(formeler),
   ere(formelere),
   no_e(formeel),
   st(formeelst),
   ste(formeelste)],adv,[],[]).

a([stof(formica)],nonadv,[],[]).

a([e(formidabele),
   er(formidabeler),
   ere(formidabelere),
   no_e(formidabel),
   st(formidabelst),
   ste(formidabelste)],adv,[],[]).

a([e(forse),
   er(forser),
   ere(forsere),
   no_e(fors),
   st(forst),
   ste(forste)],adv,[],[]).

a([e(fortuinlijke),
   no_e(fortuinlijk)],adv,
  [subject_sbar],[]).

a([e(fossiele),
   no_e(fossiel)],nonadv,[],[]).

a([e(fotografische),
   no_e(fotografisch)],adv,[],[]).

a([e(foute),
   no_e(fout)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(foutieve),
   er(foutiever),
   ere(foutievere),
   no_e(foutief),
   st(foutiefst),
   ste(foutiefste)],adv,[],[]).

a([e(foutloze),
   no_e(foutloos)],adv,[],[]).

a([e(fraaie),
   er(fraaier),
   ere(fraaiere),
   no_e(fraai),
   st(fraaist),
   ste(fraaiste)],adv,[],[]).

a([e(fractionele),
   no_e(fractioneel)],adv,[],[]).

a([e(fragiele),
   er(fragieler),
   ere(fragielere),
   no_e(fragiel),
   st(fragielst),
   ste(fragielste)],nonadv,[],[]).

a([e(fragmentarische),
   er(fragmentarischer),
   ere(fragmentarischere),
   no_e(fragmentarisch),
   st(fragmentarischt),
   ste(fragmentarischte)],adv,[],[]).

a([pred([frank,en,vrij])],adv,[],[]).

a([both(franco),
   both(franko)],adv,[],[]).

a([e(francofone),
   no_e(francofoon)],nonadv,[],[]).

a([e(frappante),
   er(frappanter),
   ere(frappantere),
   no_e(frappant),
   st(frappantst),
   ste(frappantste)],adv,
  [subject_sbar],[]).

a([e(fraudegevoelige),
   no_e(fraudegevoelig)],nonadv,[],[]).

a([e(frauduleuze),
   no_e(frauduleus)],adv,[],[]).

a([both('free-lance'),
   both(freelance)],adv,[],[]).

a([both('free-style'),
   both(freestyle),
   both([free,style])],adv,[],[]).

a([both(frêle)],adv,[],[]).

a([e(frekwente),
   e(frequente),
   er(frekwenter),
   er(frequenter),
   ere(frekwentere),
   ere(frequentere),
   no_e(frekwent),
   no_e(frequent),
   st(frekwentst),
   st(frequentst),
   ste(frekwentste),
   ste(frequentste)],adv,[],[]).

a([e(freudiaanse),
   no_e(freudiaans)],adv,[],[]).

a([e(frisse),
   er(frisser),
   ere(frissere),
   no_e(fris),
   st(frist),
   ste(friste)],padv,[],[]).

a([pred(frisjes)],adv,[],[]).

a([e(frivole),
   er(frivoler),
   ere(frivolere),
   no_e(frivool),
   st(frivoolst),
   ste(frivoolste)],adv,[],[]).

a([e(frontale),
   no_e(frontaal)],adv,[],[]).

a([e(fruitige),
   er(fruitiger),
   ere(fruitigere),
   no_e(fruitig),
   st(fruitigst),
   ste(fruitigste)],nonadv,[],[]).

a([e(frygische),no_e(frygisch)],nonadv,[],[]).

a([prefix(fucking)],nonadv,[],[]).

a([both('full-time'),
   both(fulltime),
   both([full,time])],adv,[],[]).

a([e(functionele),
   e(funktionele),
   er(functioneler),
   er(funktioneler),
   ere(functionelere),
   ere(funktionelere),
   no_e(functioneel),
   no_e(funktioneel),
   st(functioneelst),
   st(funktioneelst),
   ste(functioneelste),
   ste(funktioneelste)],adv,[],[]).

a([e(fundamentalistische),
   no_e(fundamentalistisch)],adv,[],[h(moslim)]).

a([e(fundamentele),
   er(fundamenteler),
   ere(fundamentelere),
   no_e(fundamenteel),
   st(fundamenteelst),
   ste(fundamenteelste)],adv,
  [subject_sbar,
   pp(voor)],[]).

a([e(funeste),
   er(funester),
   ere(funestere),
   no_e(funest)],nonadv,
  [pp(voor)],[]).

a([both(funky)],adv,[],[]).

a([e(furieuze),
   er(furieuzer),
   ere(furieuzere),
   no_e(furieus),
   st(furieust),
   ste(furieuste)],adv,
  [er_pp_sbar(over),
   object_sbar,
   pp(over)],[]).

a([e(futiele),
   er(futieler),
   ere(futielere),
   no_e(futiel),
   st(futielst),
   ste(futielste)],nonadv,[],[]).

a([e(futuristische),
   no_e(futuristisch)],adv,[],[]).

a([both(fuzzy)],nonadv,[],[]).

a([e(fysieke),
   no_e(fysiek)],adv,[],[]).

a([e(fysiologische),
   no_e(fysiologisch)],adv,[],[]).

a([e(fysische),
   no_e(fysisch)],adv,[],[]).

a([e(gave),
   er(gaver),
   ere(gavere),
   no_e(gaaf),
   st(gaafst),
   ste(gaafste)],adv,[subject_sbar,subject_vp],
  [kei,
   super]).

a([e(gare),
   er(gaarder),
   ere(gaardere),
   no_e(gaar),
   st(gaarst),
   ste(gaarste)],padv,[],[beet]).

a([ge_both(gadegeslagen)],adv,[],[]).

a([e(galante),
   er(galanter),
   ere(galantere),
   no_e(galant),
   st(galantst),
   ste(galantste)],adv,[],[]).

a([e(gammele),
   er(gammeler),
   ere(gammelere),
   no_e(gammel),
   st(gammelst),
   ste(gammelste)],padv,[],[]).

a([e(gangbare),
   er(gangbaarder),
   ere(gangbaardere),
   no_e(gangbaar),
   st(gangbaarst),
   ste(gangbaarste)],nonadv,[],[subject_sbar,
				subject_vp]).

a([e(ganse),
   no_e(gans)],adv,[],[]).

a([e(gastvrije),
   er(gastvrijer),
   ere(gastvrijere),
   no_e(gastvrij),
   st(gastvrijst),
   ste(gastvrijste)],adv,[],[]).

a([e(gauwe),
   er(gauwer),
   ere(gauwere),
   no_e(gauw),
   st(gauwst),
   ste(gauwste)],adv,[],[]).

a([pred(gay)],nonadv,[],[]).

a([stof(gazen)],nonadv,[],[]).

a([ge_e(geaaide),
   ge_no_e(geaaid)],adv,[],[]).

a([ge_e(geaarde),
   ge_no_e(geaard)],adv,[],[]).

a([ge_e(geaasde),
   ge_no_e(geaasd)],adv,[],[]).

a([ge_e(geaboneerde),
   no_no_e(geaboneerd)],adv,[],[]).

a([ge_e(geaborteerde),
   no_no_e(geaborteerd)],adv,[],[]).

a([ge_e(geabsorbeerde),
   ge_no_e(geabsorbeerd)],adv,[],[]).

a([ge_e(geabstraheerde),
   ge_no_e(geabstraheerd)],adv,[],[]).

a([ge_e(geaccentueerde),
   ge_no_e(geaccentueerd)],adv,[],[]).

a([ge_e(geaccepteerde),
   er(geaccepteerder),
   ere(geaccepteerdere),
   ge_no_e(geaccepteerd),
   st(geaccepteerdst),
   ste(geaccepteerdste)],adv,
  [subject_sbar,
   pp(door)],[]).

a([ge_e(geacclimatiseerde),
   no_no_e(geacclimatiseerd)],adv,[],[]).

a([ge_e(geachte),
   ge_no_e(geacht)],adv,
  [pred],[]).

a([ge_no_e(geacteerd),
   ge_e(geacteerde)],adv,[],[]).

a([ge_e(geactiveerde),
   ge_no_e(geactiveerd)],adv,[],[]).

a([ge_e(geactualiseerde),
   ge_no_e(geactualiseerd),
   ge_e(geactualizeerde),
   ge_no_e(geactualizeerd)],adv,[],[]).

a([ge_e(geadelde),
   no_no_e(geadeld)],adv,[],[]).

a([ge_e(geademde),
   ge_no_e(geademd)],adv,[],[]).

a([ge_e(geadministreerde),
   no_no_e(geadministreerd)],adv,[],[]).

a([ge_e(geadopteerde),
   ge_no_e(geadopteerd)],adv,[],[]).

a([ge_e(geadoreerde),
   no_no_e(geadoreerd)],adv,[],[]).

a([ge_e(geadresseerde),
   ge_no_e(geadresseerd)],adv,[],[]).

a([ge_e(geadverteerde),
   ge_no_e(geadverteerd)],adv,[],[]).

a([ge_e(geadviseerde),
   ge_no_e(geadviseerd),
   ge_e(geadvizeerde),
   ge_no_e(geadvizeerd)],adv,[],[]).

a([ge_e(geaksentueerde),
   ge_no_e(geaksentueerd)],adv,[],[]).

a([ge_no_e(geakteerd),
   ge_e(geakteerde)],adv,[],[]).

a([ge_e(geaktiveerde),
   ge_no_e(geaktiveerd)],adv,[],[]).

a([ge_e(geaktualizeerde),
   ge_no_e(geaktualizeerd)],adv,[],[]).

a([ge_e(gealarmeerde),
   ge_no_e(gealarmeerd)],adv,[],[]).

a([e(geallieerde),
   no_e(geallieerd)],padv,[],[]).

a([ge_e(geambieerde),
   ge_no_e(geambieerd)],adv,[],[]).

a([ge_e(geamendeerde),
   ge_no_e(geamendeerd)],adv,[],[]).

a([ge_e(geamputeerde),
   ge_no_e(geamputeerd)],adv,[],[]).

a([ge_e(geamuseerde),
   er(geamuseerder),
   ere(geamuseerdere),
   ge_no_e(geamuseerd),
   st(geamuseerdst),
   ste(geamuseerdste)],adv,[],[]).

a([ge_e(geanalyseerde),
   ge_no_e(geanalyseerd)],adv,[],[]).

a([ge_e(geanalyzeerde),
   ge_no_e(geanalyzeerd)],adv,[],[]).

a([ge_e(geanimeerde),
   er(geanimeerder),
   ere(geanimeerdere),
   ge_no_e(geanimeerd),
   st(geanimeerdst),
   ste(geanimeerdste)],both,[],[]).

a([ge_e(geannexeerde),
   ge_no_e(geannexeerd)],adv,[],[]).

a([ge_e(geannuleerde),
   ge_no_e(geannuleerd)],adv,[],[]).

a([ge_no_e(geanticipeerd),
   ge_e(geanticipeerde)],adv,[],[]).

a([ge_e(geantwoorde),
   ge_no_e(geantwoord)],adv,[],[]).

a([ge_no_e(geappelleerd),
   ge_e(geappelleerde)],adv,[],[]).

a([ge_e(geapprecieerde),
   ge_no_e(geapprecieerd)],adv,[],[]).

a([ge_no_e(gearbeid),
   ge_e(gearbeide)],adv,[],[]).

a([ge_e(geargumenteerde),
   ge_no_e(geargumenteerd)],adv,[],[]).

a([e(geargwaande),
   no_e(geargwaand)],adv,[],[]).

a([e(gearmde),
   no_e(gearmd)],padv,[],[]).

a([ge_e(gearrangeerde),
   ge_no_e(gearrangeerd)],adv,[],[]).

a([ge_e(gearresteerde),
   ge_no_e(gearresteerd)],adv,[],[]).

a([ge_e(gearriveerde),
   ge_no_e(gearriveerd)],adv,[],[]).

a([ge_e(gearticuleerde),
   ge_no_e(gearticuleerd)],adv,[],[]).

a([ge_e(geartikuleerde),
   ge_no_e(geartikuleerd)],adv,[],[]).

a([ge_e(geassimileerde),
   ge_no_e(geassimileerd)],adv,[],[]).

a([ge_e(geassisteerde),
   ge_no_e(geassisteerd)],adv,[],[]).

a([ge_e(geassocieerde),
   ge_no_e(geassocieerd)],adv,[],[]).

a([ge_e(geattendeerde),
   ge_no_e(geattendeerd)],adv,[],[]).

a([ge_e(geautomatiseerde),
   ge_no_e(geautomatiseerd),
   ge_e(geautomatizeerde),
   ge_no_e(geautomatizeerd)],adv,[],[]).

a([ge_e(geautoriseerde),
   ge_no_e(geautoriseerd)],padv,[],[]).

a([ge_e(geavanceerde),
   er(geavanceerder),
   ere(geavanceerdere),
   ge_no_e(geavanceerd),
   st(geavanceerdst),
   ste(geavanceerdste)],adv,[],[]).

a([ge_e(gebade),
   ge_no_e(gebaad)],adv,[],[]).

a([ge_e(gebaande),
   ge_no_e(gebaand)],adv,[],[]).

a([ge_e(gebaarde),
   ge_no_e(gebaard)],adv,[],[]).

a([pred(gebaat)],padv,
  [pp(bij),
   er_pp_sbar(bij),
   er_pp_vp(bij),
   pp(met),
   er_pp_sbar(met),
   er_pp_vp(met)],[]).

a([ge_e(gebagatelliseerde),
   ge_no_e(gebagatelliseerd),
   ge_e(gebagatellizeerde),
   ge_no_e(gebagatellizeerd)],adv,[],[]).

a([ge_e(gebakende),
   ge_no_e(gebakend)],adv,[],[]).

a([ge_both(gebakken)],adv,
  [pred],[]).

a([ge_e(gebalanceerde),
   ge_no_e(gebalanceerd)],adv,[],[]).

a([ge_e(gebalde),
   ge_no_e(gebald)],adv,[],[]).

a([ge_e(gebalsemde),
   ge_no_e(gebalsemd)],padv,[],[]).

a([ge_both(gebannen)],adv,[],[]).

a([ge_e(gebarricadeerde),
   ge_no_e(gebarricadeerd)],adv,[],[]).

a([ge_e(gebarrikadeerde),
   ge_no_e(gebarrikadeerd)],adv,[],[]).

a([ge_both(gebarsten)],adv,[],[]).

a([ge_e(gebaseerde),
   ge_no_e(gebaseerd)],adv,
  [pp(op),
   er_pp_vp(op),
   er_pp_sbar(op)],[]).

a([ge_no_e(gebedeld),
   ge_e(gebedelde)],adv,[],[]).

a([ge_both(gebeden)],adv,[],[]).

a([ge_e(gebeeldhouwde),
   ge_no_e(gebeeldhouwd)],adv,[],[]).

a([ge_e(gebeende),
   ge_no_e(gebeend)],adv,[],[]).

a([ge_e(gebeide),
   ge_no_e(gebeid)],adv,[],[]).

a([ge_e(gebeitelde),
   ge_no_e(gebeiteld)],adv,[],[]).

a([ge_e(gebekte),
   ge_no_e(gebekt)],padv,[],[]).

a([ge_e(gebelde),
   ge_no_e(gebeld)],adv,[],[]).

a([e(gebelgde),
   no_e(gebelgd)],adv,[],[]).

a([ge_e(gebette),
   ge_no_e(gebet)],adv,[],[]).

a([ge_both(gebeten)],adv,[],[]).

a([ge_e(gebeterde),
   ge_no_e(gebeterd)],adv,[],[]).

a([ge_e(gebeukte),
   ge_no_e(gebeukt)],adv,
  [pred],[]).

a([ge_e(gebeurde),
   ge_no_e(gebeurd)],adv,
  [fixed([[waar]])],[]).

a([ge_e(gebezigde),
   ge_no_e(gebezigd)],adv,[],[]).

a([ge_e(gebiechte),
   ge_no_e(gebiecht)],adv,[],[]).

a([ende(gebiedende),
   er(gebiedender),
   ere(gebiedendere),
   end(gebiedend),
   st(gebiedendst),
   ste(gebiedendste)],adv,[],[]).

a([ge_e(gebiologeerde),
   ge_no_e(gebiologeerd)],adv,[],[]).

a([ge_no_e(gebladerd),
   ge_e(gebladerde)],adv,[],[]).

a([ge_e(geblakerde),
   ge_no_e(geblakerd)],adv,[],[]).

a([ge_e(geblancheerde),
   ge_no_e(geblancheerd)],padv,[],[]).

a([ge_both(geblazen)],nonadv,[],[]).

a([ge_e(gebleekte),
   ge_no_e(gebleekt)],adv,[],[]).

a([ge_both(gebleken)],adv,[],[]).

a([ge_e(geblesseerde),
   ge_no_e(geblesseerd)],adv,
  [pp(aan)],[]).

a([ge_both(gebleven)],adv,
  [pred,
   fixed([[achterwege]])],[]).

a([ge_e(gebliksemde),
   ge_no_e(gebliksemd)],adv,[],[]).

a([ge_e(geblinddoekte),
   ge_no_e(geblinddoekt)],adv,[],[]).

a([ge_e(geblindeerde),
   ge_no_e(geblindeerd)],nonadv,[],[]).

a([e(gebloemde),
   no_e(gebloemd)],nonadv,[],[]).

a([ge_e(geblokkeerde),
   ge_no_e(geblokkeerd)],adv,
  [pp(door)],[]).

a([ge_e(geblokte),
   ge_no_e(geblokt)],nonadv,[],[]).

a([ge_both(geblonken)],adv,[],[]).

a([ge_both(gebluft)],adv,[],[]).

a([ge_e(gebluste),
   ge_no_e(geblust)],adv,[],[]).

a([ge_e(gebochelde),
   ge_no_e(gebocheld)],nonadv,[],[]).

a([ge_both(geboden)],adv,
  [so_np],[]).

a([ge_e(geboeide),
   ge_no_e(geboeid)],adv,[],[]).

a([ge_e(geboekstaafde),
   ge_no_e(geboekstaafd)],adv,[],[]).

a([ge_e(geboekte),
   ge_no_e(geboekt)],adv,[],[]).

a([ge_e(geboende),
   ge_no_e(geboend)],adv,[],[]).

a([ge_e(geboete),
   ge_no_e(geboet)],adv,[],[]).

a([ge_e(geboetseerde),
   ge_no_e(geboetseerd)],adv,[],[]).

a([ge_both(gebogen),
   ere(gebogenere),
   er(gebogener)],both,[],[]).

a([ge_e(gebokste),
   ge_no_e(gebokst)],adv,[],[]).

a([ge_e(gebolde),
   ge_no_e(gebold)],adv,[],[]).

a([ge_e(gebombardeerde),
   ge_no_e(gebombardeerd)],adv,[],[]).

a([ge_both(gebonden)],adv,
  [pp(aan)],[tijd]).

a([ge_both(gebonkt)],adv,[],[]).

a([ge_e(gebonsde),
   ge_no_e(gebonsd)],adv,[],[]).

a([ge_e(geboorde),
   ge_no_e(geboord)],adv,[],[]).

a([ge_e(gebootlegde),
   ge_no_e(gebootlegd)],adv,[],[]).

a([ge_e(gebootste),
   ge_no_e(gebootst)],adv,[],[]).

a([ge_e(geborduurde),
   ge_no_e(geborduurd)],adv,[],[]).

a([ge_both(geboren)],padv,[],[dood]).

a([ge_both(geborgen)],adv,[],[]).

a([ge_e(geborstelde),
   ge_no_e(geborsteld)],adv,[],[]).

a([ge_e(gebotste),
   ge_no_e(gebotst)],adv,[],[]).

a([ge_e(gebottelde),
   ge_no_e(gebotteld)],adv,[],[]).

a([ge_e(gebouwde),
   ge_no_e(gebouwd)],both,
  [],[]).

a([ge_e(gebraakte),
   ge_no_e(gebraakt)],adv,[],[]).

a([ge_e(gebrabbelde),
   ge_no_e(gebrabbeld)],adv,[],[]).

a([ge_e(gebrachte),
   ge_no_e(gebracht)],adv,
  [fixed([[te,berde]]),
   fixed([[in,rekening]]),
   fixed([[ten,gehore]]),
   fixed([[tot,stand]])],[]).

a([ge_both(gebraden)],adv,[],[]).

a([ge_e(gebrande),
   ge_no_e(gebrand)],padv,
  [er_pp_sbar(op),
   er_pp_vp(op),
   pp(op)],[]).

a([ge_e(gebrandmerkte),
   ge_no_e(gebrandmerkt)],adv,[],[]).

a([ge_e(gebrandschilderde),
   ge_no_e(gebrandschilderd)],padv,[],[]).

a([ge_e(gebreide),
   ge_no_e(gebreid)],adv,[],[]).

a([e(gebrekkige),
   er(gebrekkiger),
   ere(gebrekkigere),
   no_e(gebrekkig),
   st(gebrekkigst),
   ste(gebrekkigste)],adv,[],[]).

a([ge_no_e(gebroed),
   ge_e(gebroede)],adv,[],[]).

a([ge_e(gebroeide),
   ge_no_e(gebroeid)],adv,[],[]).

a([ge_both(gebroken)],adv,[],[]).

a([ge_e(gebrokkelde),
   ge_no_e(gebrokkeld)],adv,[],[]).

a([ge_e(gebromde),
   ge_no_e(gebromd)],adv,[],[]).

a([ge_e(gebronsde),
   ge_no_e(gebronsd)],adv,[],[]).

a([ge_both(gebrouwen)],adv,[],[]).

a([e(gebruikelijke),
   er(gebruikelijker),
   ere(gebruikelijkere),
   no_e(gebruikelijk),
   st(gebruikelijkst),
   ste(gebruikelijkste)],nonadv,
  [subject_sbar,
   subject_vp,
   pp(bij),
   pp(in)],[]).

a([ge_e(gebruikte),
   ge_no_e(gebruikt)],adv,[object_vp],[]).

a([ge_e(gebruikte),
   ge_no_e(gebruikt)],adv,[],[h(veel)]).

a([ge_e(gebruinde),
   er(gebruinder),
   ere(gebruindere),
   ge_no_e(gebruind),
   st(gebruindst),
   ste(gebruindste)],adv,[],[]).

a([ge_e(gebrulde),
   ge_no_e(gebruld)],adv,[],[]).

a([ge_e(gebuitelde),
   ge_no_e(gebuiteld)],adv,[],[]).

a([ge_e(gebukte),
   ge_no_e(gebukt)],padv,[],[]).

a([ge_e(gebundelde),
   ge_no_e(gebundeld)],adv,[],[]).

a([ge_e(gecalculeerde),
   ge_no_e(gecalculeerd)],adv,[],[]).

a([ge_e(gecamoufleerde),
   ge_no_e(gecamoufleerd)],padv,[],[]).

a([ge_e(gecarjackte),
   ge_no_e(gecarjackt)],nonadv,[],[]).

a([ge_e(gecaste),
   ge_no_e(gecast)],nonadv,[],[]).

a([ge_e(gecastreerde),
   ge_no_e(gecastreerd)],nonadv,[],[]).

a([ge_e(gecensureerde),
   ge_no_e(gecensureerd)],nonadv,[],[]).

a([ge_e(gecentraliseerde),
   ge_no_e(gecentraliseerd),
   ge_e(gecentralizeerde),
   ge_no_e(gecentralizeerd)],adv,[],[]).

a([ge_e(gecentreerde),
   ge_no_e(gecentreerd)],adv,[],[]).

a([ge_e(gecertificeerde),
   ge_no_e(gecertificeerd)],padv,[],[]).

a([ge_e(gechanteerde),
   ge_no_e(gechanteerd)],adv,[],[]).

a([ge_e(gechargeerde),
   ge_no_e(gechargeerd)],adv,[],[]).

a([e(gecharmeerde),
   no_e(gecharmeerd)],nonadv,
  [pp(van)],[]).

a([ge_e(gecharterde),
   ge_no_e(gecharterd)],nonadv,[],[]).

a([ge_e(gecheckte),
   ge_no_e(gecheckt)],adv,[],[]).

a([ge_e(gechoqueerde),
   ge_no_e(gechoqueerd)],adv,[],[]).

a([ge_e(geciteerde),
   ge_no_e(geciteerd)],adv,[],[]).

a([ge_e(geclaimde),
   ge_no_e(geclaimd)],adv,[],[]).

a([ge_e(geclassificeerde),
   ge_no_e(geclassificeerd)],adv,[],[]).

a([ge_no_e(gecloond),
   ge_e(gecloonde)],adv,[],[]).

a([ge_e(gecoachte),
   ge_no_e(gecoacht)],padv,[],[]).

a([ge_e(gecodeerde),
   ge_no_e(gecodeerd)],adv,[],[]).

a([ge_e(gecombineerde),
   ge_e(gekombineerde),
   ge_no_e(gecombineerd),
   ge_no_e(gekombineerd)],padv,[],[]).

a([ge_e(gecommandeerde),
   ge_no_e(gecommandeerd)],padv,[],[]).

a([ge_e(gecommiteerde),
   ge_no_e(gecommiteerd)],padv,[],[]).

a([ge_e(gecommitteerde),
   ge_no_e(gecommitteerd)],padv,[],[]).

a([ge_no_e(gecommuniceerd),
   ge_e(gecommuniceerde)],adv,[],[]).

a([ge_e(gecompenseerde),
   ge_no_e(gecompenseerd)],adv,[],[]).

a([ge_e(gecompleteerde),
   ge_no_e(gecompleteerd)],adv,[],[]).

a([ge_e(gecompliceerde),
   ge_e(gekompliceerde),
   er(gecompliceerder),
   er(gekompliceerder),
   ere(gecompliceerdere),
   ere(gekompliceerdere),
   ge_no_e(gecompliceerd),
   ge_no_e(gekompliceerd),
   st(gecompliceerdst),
   st(gekompliceerdst),
   ste(gecompliceerdste),
   ste(gekompliceerdste)],adv,[],[]).

a([ge_e(gecomplimenteerde),
   ge_no_e(gecomplimenteerd)],adv,[],[]).

a([ge_e(gecomponeerde),
   ge_no_e(gecomponeerd)],adv,[],[]).

a([ge_e(gecomprimeerde),
   ge_no_e(gecomprimeerd)],adv,[],[]).

a([ge_e(gecompromitteerde),
   ge_no_e(gecompromitteerd)],adv,[],[]).

a([ge_e(geconcentreerde),
   ge_e(gekoncentreerde),
   er(geconcentreerder),
   er(gekoncentreerder),
   ere(geconcentreerdere),
   ere(gekoncentreerdere),
   ge_no_e(geconcentreerd),
   ge_no_e(gekoncentreerd),
   st(geconcentreerdst),
   st(gekoncentreerdst),
   ste(geconcentreerdste),
   ste(gekoncentreerdste)],adv,
  [pp(in),
   pp(op)],[]).

a([ge_e(geconcipieerde),
   ge_no_e(geconcipieerd)],adv,[],[]).

a([ge_e(geconcludeerde),
   ge_no_e(geconcludeerd)],adv,[],[]).

a([ge_e(geconcretiseerde),
   ge_no_e(geconcretiseerd),
   ge_e(geconcretizeerde),
   ge_no_e(geconcretizeerd)],adv,[],[]).

a([ge_e(gecondenseerde),
   ge_no_e(gecondenseerd)],adv,[],[]).

a([ge_e(geconditioneerde),
   ge_e(gekonditioneerde),
   ge_no_e(geconditioneerd),
   ge_no_e(gekonditioneerd)],padv,[],[]).

a([ge_e(gecondoleerde),
   ge_no_e(gecondoleerd)],adv,[],[]).

a([ge_e(geconformeerde),
   ge_no_e(geconformeerd)],adv,[],[]).

a([ge_e(geconfronteerde),
   ge_no_e(geconfronteerd)],adv,[],[]).

a([ge_e(geconserveerde),
   ge_no_e(geconserveerd)],adv,[],[]).

a([ge_e(geconsolideerde),
   ge_no_e(geconsolideerd)],adv,[],[]).

a([ge_e(geconstateerde),
   ge_no_e(geconstateerd)],adv,[],[]).

a([ge_e(geconstitueerde),
   ge_no_e(geconstitueerd)],adv,[],[]).

a([ge_e(geconstrueerde),
   ge_no_e(geconstrueerd)],adv,[],[]).

a([ge_e(geconsulteerde),
   ge_no_e(geconsulteerd)],adv,[],[]).

a([ge_e(geconsumeerde),
   ge_no_e(geconsumeerd)],adv,[],[]).

a([ge_e(gecontinueerde),
   ge_no_e(gecontinueerd)],adv,[],[]).

a([ge_no_e(gecontrasteerd),
   ge_e(gecontrasteerde)],adv,[],[]).

a([ge_e(gecontroleerde),
   ge_no_e(gecontroleerd)],adv,[],[]).

a([ge_e(gecopieerde),
   ge_no_e(gecopieerd)],adv,[],[]).

a([ge_e(gecorrigeerde),
   ge_no_e(gecorrigeerd)],adv,[],[]).

a([ge_e(gecorrumpeerde),
   ge_no_e(gecorrumpeerd)],adv,[],[]).

a([ge_e(gecounselde),
   ge_no_e(gecounseld)],adv,[],[]).

a([ge_e(gecoverde),
   ge_no_e(gecoverd)],padv,[],[]).

a([ge_e(gecoördineerde),
   ge_no_e(gecoördineerd)],adv,[],[]).

a([ge_e(gecrashte),
   ge_no_e(gecrasht)],nonadv,[],[]).

a([ge_e(gecrepeerde),
   ge_no_e(gecrepeerd)],adv,[],[]).

a([ge_e(gecreëerde),
   ge_no_e(gecreëerd)],adv,[],[]).

a([ge_e(gecultiveerde),
   ge_e(gekultiveerde),
   ge_no_e(gecultiveerd),
   ge_no_e(gekultiveerd)],padv,[],[]).

a([ge_e(gecumuleerde),
   ge_no_e(gecumuleerd)],adv,[],[]).

a([ge_e(gedaagde),
   ge_no_e(gedaagd)],adv,[],[]).

a([ge_e(gedaalde),
   ge_no_e(gedaald)],adv,[],[]).

a([ge_e(gedane),
   ge_no_e(gedaan)],adv,
  [pp(door),
   pp(met),
   subject_sbar,
   subject_vp
  ],[]).

a([ge_e(gedachte),
   ge_no_e(gedacht)],adv,[],[]).

a([e(gedachteloze),
   er(gedachtelozer),
   ere(gedachtelozere),
   no_e(gedachteloos),
   st(gedachteloost),
   ste(gedachtelooste)],adv,[],[]).

a([ge_e(gedagvaarde),
   ge_no_e(gedagvaard)],padv,[],[]).

a([ge_e(gedankte),
   ge_no_e(gedankt)],adv,[],[]).

a([ge_e(gedanste),
   ge_no_e(gedanst)],adv,[],[]).

a([ge_e(gedarde),
   ge_no_e(gedard)],adv,[],[]).

a([ge_e(gedateerde),
   ge_no_e(gedateerd)],adv,[],[]).

a([ge_e(gedebiteerde),
   ge_no_e(gedebiteerd)],adv,[],[]).

a([ge_e(gedeblokkeerde),
   ge_no_e(gedeblokkeerd)],adv,[],[]).

a([ge_no_e(gedebuteerd),
   ge_e(gedebuteerde)],adv,[],[]).

a([ge_e(gedecanteerde),
   ge_no_e(gedecanteerd)],adv,[],[]).

a([ge_e(gedecentraliseerde),
   ge_no_e(gedecentraliseerd),
   ge_e(gedecentralizeerde),
   ge_no_e(gedecentralizeerd)],adv,[],[]).

a([ge_e(gedecideerde),
   ge_no_e(gedecideerd)],adv,[],[]).

a([ge_e(gedecimeerde),
   ge_no_e(gedecimeerd)],padv,[],[]).

a([ge_e(gedeclameerde),
   ge_no_e(gedeclameerd)],adv,[],[]).

a([ge_e(gedeclareerde),
   ge_no_e(gedeclareerd)],padv,[],[]).

a([ge_e(gedecoreerde),
   ge_no_e(gedecoreerd)],padv,[],[]).

a([ge_e(gedecreteerde),
   ge_no_e(gedecreteerd)],adv,[],[]).

a([ge_e(gedeelde),
   ge_no_e(gedeeld)],adv,[],[]).

a([e(gedeeltelijke),
   no_e(gedeeltelijk)],adv,[],[]).

a([ge_e(gedefinieerde),
   ge_no_e(gedefinieerd)],adv,[],[]).

a([both(gedegen),
   er(gedegener),
   ere(gedegenere),
   st(gedegenst),
   ste(gedegenste)],adv,[],[]).

a([ge_e(gedegenereerde),
   ge_no_e(gedegenereerd)],adv,[],[]).

a([ge_e(gedegradeerde),
   ge_no_e(gedegradeerd)],adv,[],[]).

a([ge_e(gedeisde),
   ge_no_e(gedeisd)],nonadv,[],[]).

a([ge_e(gedeinsde),
   ge_no_e(gedeinsd)],adv,[],[]).

a([ge_e(gedeklameerde),
   ge_no_e(gedeklameerd)],adv,[],[]).

a([ge_e(gedekreteerde),
   ge_no_e(gedekreteerd)],adv,[],[]).

a([ge_e(gedekte),
   ge_no_e(gedekt)],adv,
  [pp(door)],[]).

a([ge_e(gedelegeerde),
   ge_no_e(gedelegeerd)],adv,[],[]).

a([ge_e(gedementeerde),
   ge_no_e(gedementeer)],padv,[],[]).

a([ge_e(gedemilitariseerde),
   ge_no_e(gedemilitariseerd)],padv,[],[]).

a([ge_e(gedemocratiseerde),
   ge_no_e(gedemocratiseerd),
   ge_e(gedemokratizeerde),
   ge_no_e(gedemokratizeerd)],adv,[],[]).

a([ge_e(gedemoniseerde),  % Pim zeker weer
   ge_no_e(gedemoniseerd)],padv,[],[]). 

a([ge_e(gedemonstreerde),
   ge_no_e(gedemonstreerd)],adv,[],[]).

a([ge_e(gedemonteerde),
   ge_no_e(gedemonteerd)],adv,[],[]).

a([ge_e(gedemoraliseerde),
   ge_no_e(gedemoraliseerd)],padv,[],[]).

a([ge_e(gedempte),
   ge_no_e(gedempt)],adv,[],[]).

a([e(gedenkwaardige),
   er(gedenkwaardiger),
   ere(gedenkwaardigere),
   no_e(gedenkwaardig),
   st(gedenkwaardigst),
   ste(gedenkwaardigste)],nonadv,[],[]).

a([ge_e(gedeponeerde),
   ge_no_e(gedeponeerd)],adv,[],[]).

a([ge_e(gedeporteerde),
   ge_no_e(gedeporteerd)],adv,[],[]).

a([ge_e(gedeprimeerde),
   ge_no_e(gedeprimeerd)],padv,[],[]).

a([ge_e(gedepte),
   ge_no_e(gedept)],adv,[],[]).

a([ge_e(gedeputeerde),
   ge_no_e(gedeputeerd)],adv,[],[]).

a([ge_e(gederfde),
   ge_no_e(gederfd)],nonadv,[],[]).

a([ge_e(gedeserteerde),
   ge_no_e(gedeserteerd)],adv,[],[]).

a([ge_e(gedesillusioneerde),
   ge_no_e(gedesillusioneerd)],padv,[],[]).

a([ge_e(gedesinfecteerde),
   ge_no_e(gedesinfecteerd)],adv,[],[]).

a([ge_e(gedesinfekteerde),
   ge_no_e(gedesinfekteerd)],adv,[],[]).

a([ge_e(gedesintegreerde),
   ge_no_e(gedesintegreerd)],adv,[],[]).

a([ge_e(gedesoriënteerde),
   ge_no_e(gedesoriënteerd)],padv,[],[]).

a([ge_e(gedestilleerde),
   ge_no_e(gedestilleerd)],adv,[],[]).

a([ge_e(gedetailleerde),
   er(gedetailleerder),
   ere(gedetailleerdere),
   ge_no_e(gedetailleerd),
   st(gedetailleerdst),
   ste(gedetailleerdste)],adv,[],[]).

a([e(gedetineerde),
   no_e(gedetineerd)],nonadv,[],[]).

a([ge_e(gedetermineerde),
   ge_no_e(gedetermineerd)],adv,[],[]).

a([ge_e(gedeukte),
   ge_no_e(gedeukt)],adv,[],[]).

a([ge_e(gedevalueerde),
   ge_no_e(gedevalueerd)],adv,[],[]).

a([ge_e(gediagnostiseerde),
   ge_no_e(gediagnostiseerd),
   ge_e(gediagnostizeerde),
   ge_no_e(gediagnostizeerd)],adv,[],[]).

a([ge_e(gedichte),
   ge_no_e(gedicht)],adv,[],[]).

a([ge_e(gedicteerde),
   ge_no_e(gedicteerd)],adv,[],[]).

a([ge_e(gediende),
   ge_no_e(gediend)],adv,
  [pp(bij),
   er_pp_sbar(bij),
   er_pp_vp(bij),
   pp(van),
   er_pp_vp(van),
   er_pp_sbar(van),
   pp(met),
   er_pp_vp(met),
   er_pp_sbar(met)],[]).

a([e(gedienstige),
   er(gedienstiger),
   ere(gedienstigere),
   no_e(gedienstig),
   st(gedienstigst),
   ste(gedienstigste)],adv,[],[]).

a([ge_e(gediepte),
   ge_no_e(gediept)],adv,[],[]).

a([ge_e(gedifferentieerde),
   er(gedifferentieerder),
   ere(gedifferentieerdere),
   ge_no_e(gedifferentieerd),
   st(gedifferentieerdst),
   ste(gedifferentieerdste)],adv,[],[]).

a([ge_e(gedijde),
   ge_no_e(gedijd)],adv,[],[]).

a([ge_e(gedikteerde),
   ge_no_e(gedikteerd)],adv,[],[]).

a([ge_e(gedimde),
   ge_no_e(gedimd)],padv,[],[]).

a([ge_e(gediplomeerde),
   ge_no_e(gediplomeerd)],adv,[],[]).

a([ge_e(gedirigeerde),
   ge_no_e(gedirigeerd)],adv,[],[]).

a([ge_e(gedisciplineerde),
   er(gedisciplineerder),
   ere(gedisciplineerdere),
   ge_no_e(gedisciplineerd),
   st(gedisciplineerdst),
   ste(gedisciplineerdste)],adv,[],[]).

a([ge_e(gediscrimineerde),
   ge_no_e(gediscrimineerd)],adv,[],[]).

a([no_e(gedisillusioneerd),
   e(gedisillusioneerde)],padv,[],[]).

a([ge_e(gedistantieerde),
   ge_no_e(gedistantieerd)],adv,[],[]).

a([ge_e(gedistilleerde),
   ge_no_e(gedistilleerd)],adv,[],[]).

a([ge_e(gedistingeerde),
   er(gedistingeerder),
   ere(gedistingeerdere),
   ge_no_e(gedistingeerd),
   st(gedistingeerdst),
   ste(gedistingeerdste)],adv,[],[]).

a([ge_e(gedistribueerde),
   ge_no_e(gedistribueerd)],adv,[],[]).

a([ge_e(gediversificeerde),
   ge_no_e(gediversificeerd)],nonadv,[],[]).

a([ge_e(gedoceerde),
   ge_no_e(gedoceerd)],adv,[],[]).

a([ge_e(gedocumenteerde),
   ge_e(gedokumenteerde),
   ge_no_e(gedocumenteerd),
   ge_no_e(gedokumenteerd)],padv,[],[]).

a([ge_e(gedoemde),
   ge_no_e(gedoemd)],adv,
  [object_vp],[]).

a([ge_both(gedoken)],adv,[],[]).

a([ge_e(gedokumenteerde),
   ge_no_e(gedokumenteerd)],adv,[],[]).

a([ge_no_e(gedold),
   ge_e(gedolde)],adv,[],[]).

a([ge_both(gedolven)],adv,[],[]).

a([ge_e(gedomineerde),
   ge_no_e(gedomineerd)],adv,[],[]).

a([ge_e(gedompelde),
   ge_no_e(gedompeld)],adv,[],[]).

a([ge_e(gedonderde),
   ge_no_e(gedonderd)],adv,[],[]).

a([ge_both(gedongen)],adv,[],[]).

a([ge_e(gedode),
   ge_no_e(gedood)],adv,[],[]).

a([e(gedoodverfde),
   no_e(gedoodverfd)],nonadv,[],[]).

a([ge_e(gedoofde),
   ge_no_e(gedoofd)],adv,[],[]).

a([ge_e(gedoogde),
   ge_no_e(gedoogd)],adv,[],[]).

a([ge_e(gedoopte),
   ge_no_e(gedoopt)],adv,[transitive],[]).

a([ge_e(gedopte),
   ge_no_e(gedopt)],adv,[],[]).

a([ge_e(gedoseerde),
   ge_no_e(gedoseerd)],adv,[],[]).

a([e(gedoteerde),
   no_e(gedoteerd)],nonadv,
  [pp(met)],[]).

a([ge_e(gedouchte),
   ge_no_e(gedoucht)],padv,[],[]).

a([ge_e(gedoubleerde),
   ge_no_e(gedoubleerd)],padv,[],[]).

a([ge_e(gedownloade),
   ge_no_e(gedownload)],padv,[],[]).

a([ge_e(gedraaide),
   ge_no_e(gedraaid)],adv,[],[]).

a([ge_both(gedragen)],adv,
  [fixed([[ten,grave]])],[on]).

a([ge_e(gedramatiseerde),
   ge_no_e(gedramatiseerd),
   ge_e(gedramatizeerde),
   ge_no_e(gedramatizeerd)],adv,[],[]).

a([ge_e(gedrapeerde),
   ge_no_e(gedrapeerd)],adv,[],[]).

a([ge_e(gedreigde),
   ge_no_e(gedreigd)],adv,[],[]).

a([ge_e(gedrenkte),
   ge_no_e(gedrenkt)],adv,[pp(in)],[]).

a([ge_e(gedrentelde),
   ge_no_e(gedrenteld)],adv,[],[]).

a([ge_e(gedresseerde),
   ge_no_e(gedresseerd)],adv,[],[]).

a([ge_both(gedreven),
   er(gedrevener)],both,
  [pp(door),
   object_vp],[]).

a([ge_e(gedribbelde),
   ge_no_e(gedribbeld)],adv,[],[]).

a([ge_e(gedrilde),
   ge_no_e(gedrild)],adv,[],[]).

a([ge_both(gedrongen),
   er(gedrongener),
   ere(gedrongenere),
   st(gedrongenst),
   ste(gedrongenste)],adv,[],[]).

a([ge_both(gedronken)],adv,[],[]).

a([ge_e(gedroogde),
   ge_no_e(gedroogd)],adv,[],[]).

a([ge_e(gedroomde),
   ge_no_e(gedroomd)],adv,[],[]).

a([ge_both(gedropen)],adv,[],[]).

a([ge_e(gedropte),
   ge_no_e(gedropt)],adv,[],[]).

a([ge_e(gedrukte),
   er(gedrukter),
   ere(gedruktere),
   ge_no_e(gedrukt),
   st(gedruktst),
   ste(gedruktste)],adv,
  [pp(bij),
   pp(door),
   pp(in),
   pp(op),
   pp(voor)],[]).

a([ge_e(gedruppelde),
   ge_no_e(gedruppeld)],adv,[],[]).

a([ge_e(gedrupte),
   ge_no_e(gedrupt)],adv,[],[]).

a([ge_e(geduchte),
   er(geduchter),
   ere(geduchtere),
   ge_no_e(geducht),
   st(geduchtst),
   ste(geduchtste)],adv,[],[]).

a([ge_e(geduide),
   ge_no_e(geduid)],adv,[],[]).

a([ge_e(geduikelde),
   ge_no_e(geduikeld)],adv,[],[]).

a([ge_e(gedulde),
   ge_no_e(geduld)],adv,[],[]).

a([e(geduldige),
   er(geduldiger),
   ere(geduldigere),
   no_e(geduldig),
   st(geduldigst),
   ste(geduldigste)],adv,[],[]).

a([ge_e(gedumpte),
   ge_no_e(gedumpt)],padv,[],[]).

a([ge_e(gedupeerde),
   ge_no_e(gedupeerd)],padv,
  [pp(door)],[]).

a([ge_e(gedurfde),
   er(gedurfder),
   ere(gedurfdere),
   ge_no_e(gedurfd),
   st(gedurfdst),
   ste(gedurfdste)],adv,[],[]).

a([e(gedurige),
   er(geduriger),
   ere(gedurigere),
   no_e(gedurig),
   st(gedurigst),
   ste(gedurigste)],adv,[],[]).

a([ge_e(geduwde),
   ge_no_e(geduwd)],adv,[],[]).

a([ge_e(gedwarrelde),
   ge_no_e(gedwarreld)],adv,[],[]).

a([ge_e(gedwarsboomde),
   ge_no_e(gedwarsboomd)],adv,[],[]).

a([e(gedweeë),
   er(gedweeër),
   ere(gedweeëre),
   no_e(gedwee),
   st(gedweest),
   ste(gedweeste)],adv,[],[]).

a([ge_e(gedweilde),
   ge_no_e(gedweild)],adv,[],[]).

a([ge_both(gedwongen),
   er(gedwongener),
   ere(gedwongenere),
   st(gedwongenst),
   ste(gedwongenste)],adv,
  [pp(door),
   pp(middels),
   pp(tot)],[]).

a([ge_e(geëerde),
   er(geëerder),
   ere(geëerdere),
   ge_no_e(geëerd),
   st(geëerdst),
   ste(geëerdste)],padv,[],[]).

a([ge_e(geëigende),
   er(geëigender),
   ere(geëigendere),
   ge_no_e(geëigend),
   st(geëigendst),
   ste(geëigendste)],padv,[],[]).

a([ge_e(geënterde),
   ge_no_e(geënterd)],padv,[],[]).

a([ge_e(geëxpandeerde),
   ge_no_e(geëxpandeerd)],padv,[],[]).

a([ge_e(geëxposeerde),
   ge_no_e(geëxposeerd)],padv,[],[]).

a([e(gele),
   er(geler),
   ere(gelere),
   no_e(geel),
   st(geelst),
   ste(geelste)],nonadv,[],[]).

a([e(geelachtige),
   er(geelachtiger),
   ere(geelachtigere),
   no_e(geelachtig),
   st(geelachtigst),
   ste(geelachtigste)],nonadv,[],[]).

a([e(geelbruine),
   er(geelbruiner),
   ere(geelbruinere),
   no_e(geelbruin),
   st(geelbruinst),
   ste(geelbruinste)],nonadv,[],[]).

a([ge_e(geëmancipeerde),
   er(geëmancipeerder),
   ere(geëmancipeerdere),
   ge_no_e(geëmancipeerd),
   st(geëmancipeerdst),
   ste(geëmancipeerdste)],nonadv,[],[]).

a([ge_e(geëmotioneerde),
   er(geëmotioneerder),
   ere(geëmotioneerdere),
   ge_no_e(geëmotioneerd),
   st(geëmotioneerdst),
   ste(geëmotioneerdste)],padv,[],[]).

a([ge_e(geëngageerde),
   ge_no_e(geëngageerd)],nonadv,[],[]).

a([e(geestdriftige),
   er(geestdriftiger),
   ere(geestdriftigere),
   no_e(geestdriftig),
   st(geestdriftigst),
   ste(geestdriftigste)],adv,[],[]).

a([e(geestelijke),
   er(geestelijker),
   ere(geestelijkere),
   no_e(geestelijk),
   st(geestelijkst),
   ste(geestelijkste)],adv,[],[]).

a([e(geesteszieke),
   no_e(geestesziek)],padv,[],[]).

a([e(geestige),
   er(geestiger),
   ere(geestigere),
   no_e(geestig),
   st(geestigst),
   ste(geestigste)],adv,[],[]).

a([e(geestverwante),
   no_e(geestverwant)],nonadv,[],[]).

a([ge_e(geëxtrapoleerde),
   ge_no_e(geëxtrapoleerd)],padv,[],[]).

a([ge_no_e(gefaald),
   ge_e(gefaalde)],adv,[],[]).

a([ge_e(gefabriceerde),
   ge_no_e(gefabriceerd)],adv,[],[]).

a([ge_e(gefantaseerde),
   ge_no_e(gefantaseerd)],adv,[],[]).

a([ge_e(gefantazeerde),
   ge_no_e(gefantazeerd)],adv,[],[]).

a([ge_e(gefascineerde),
   ge_no_e(gefascineerd)],adv,[],[]).

a([ge_e(gefaseerde),
   ge_no_e(gefaseerd)],adv,[],[]).

a([ge_e(gefeliciteerde),
   ge_no_e(gefeliciteerd)],adv,[],[]).

a([ge_both(gefield)],adv,[],[]).

a([ge_e(gefietste),
   ge_no_e(gefietst)],adv,[],[]).

a([ge_e(gefigureerde),
   ge_no_e(gefigureerd)],adv,[],[]).

a([ge_e(gefilmde),
   ge_no_e(gefilmd)],adv,[],[]).

a([ge_e(gefilterde),
   ge_no_e(gefilterd)],adv,[],[]).

a([ge_e(gefiltreerde),
   ge_no_e(gefiltreerd)],adv,[],[]).

a([ge_e(gefinancierde),
   ge_no_e(gefinancierd)],adv,[],[]).

a([ge_e(gefingeerde),
   ge_no_e(gefingeerd)],nonadv,[],[]).

a([ge_e(gefinishte),
   ge_no_e(gefinisht)],padv,[],[]).

a([ge_e(gefixeerde),
   ge_no_e(gefixeerd)],adv,[],[]).

a([ge_e(gefladderde),
   ge_no_e(gefladderd)],adv,[],[]).

a([ge_e(geflambeerde),
   ge_no_e(geflambeerd)],adv,[],[]).

a([ge_e(geflankeerde),
   ge_no_e(geflankeerd)],adv,[],[]).

a([ge_e(geflapte),
   ge_no_e(geflapt)],adv,[],[]).

a([ge_e(geflatteerde),
   ge_no_e(geflatteerd)],adv,[],[]).

a([ge_e(geflikkerde),
   ge_no_e(geflikkerd)],adv,[],[]).

a([ge_e(geflikte),
   ge_no_e(geflikt)],adv,[],[]).

a([ge_e(geflipte),
   ge_no_e(geflipt)],adv,[],[]).

a([ge_e(geflitste),
   ge_no_e(geflitst)],adv,[],[]).

a([ge_e(gefloepte),
   ge_no_e(gefloept)],adv,[],[]).

a([ge_e(geflopte),
   ge_no_e(geflopt)],adv,[],[]).

a([ge_both(gefloten)],adv,[],[]).

a([ge_e(gefluisterde),
   ge_no_e(gefluisterd)],adv,[],[]).

a([ge_e(gefnuikte),
   ge_no_e(gefnuikt)],nonadv,[],[]).

a([ge_e(gefocuste),
   ge_no_e(gefocust),
   ge_no_e(gefocused)],padv,[],[]).

a([ge_e(gefokte),
   ge_no_e(gefokt)],adv,[],[door]).

a([ge_e(geföhnde),
   ge_no_e(geföhnd)],padv,[],[]).

a([ge_e(gefolterde),
   ge_no_e(gefolterd)],adv,[],[]).

a([ge_e(gefopte),
   ge_no_e(gefopt)],padv,[],[]).

a([ge_e(geforceerde),
   er(geforceerder),
   ere(geforceerdere),
   ge_no_e(geforceerd),
   st(geforceerdst),
   ste(geforceerdste)],adv,[],[]).

a([ge_e(geformaliseerde),
   ge_no_e(geformaliseerd),
   ge_e(geformalizeerde),
   ge_no_e(geformalizeerd)],adv,[],[]).

a([ge_e(geformeerde),
   ge_no_e(geformeerd)],adv,[],[]).

a([ge_e(geformuleerde),
   ge_no_e(geformuleerd)],adv,[],[]).

a([e(gefortuneerde),
   no_e(gefortuneerd)],padv,[],[]).

a([e(gefossiliseerde),
   no_e(gefossiliseerd)],padv,[],[]).

a([ge_e(gefotografeerde),
   ge_no_e(gefotografeerd)],adv,[],[]).

a([ge_e(gefouilleerde),
   ge_no_e(gefouilleerd)],adv,[],[]).

a([ge_e(gefrappeerde),
   ge_no_e(gefrappeerd)],nonadv,[],[]).

a([ge_e(gefrituurde),
   ge_no_e(gefrituurd)],adv,[],[]).

a([ge_e(gefrommelde),
   ge_no_e(gefrommeld)],adv,[],[]).

a([ge_e(gefronste),
   ge_no_e(gefronst)],adv,[],[]).

a([ge_e(gefruite),
   ge_no_e(gefruit)],padv,[],[]).

a([ge_e(gefrunnikte),
   ge_no_e(gefrunnikt)],adv,[],[]).

a([ge_e(gefrustreerde),
   er(gefrustreerder),
   ere(gefrustreerdere),
   ge_no_e(gefrustreerd),
   st(gefrustreerdst),
   ste(gefrustreerdste)],adv,
  [pp(door)],[]).

a([ge_no_e(gefunctioneerd),
   ge_e(gefunctioneerde)],adv,[],[]).

a([ge_e(gefundeerde),
   er(gefundeerder),
   ere(gefundeerdere),
   ge_no_e(gefundeerd),
   st(gefundeerdst),
   ste(gefundeerdste)],adv,[],[on]).

a([ge_no_e(gefungeerd),
   ge_e(gefungeerde)],adv,[],[]).

a([ge_no_e(gefunktioneerd),
   ge_e(gefunktioneerde)],adv,[],[]).

a([ge_e(gefuseerde),
   ge_no_e(gefuseerd)],nonadv,[],[]).

a([ge_e(gefusilleerde),
   ge_no_e(gefusilleerd)],adv,[],[]).

a([ge_e(gegane),
   ge_no_e(gegaan)],nonadv,
  [ap_pred,
   part(teloor)],[]).

a([ge_e(gegalmde),
   ge_no_e(gegalmd)],adv,[],[]).

a([ge_e(gegaloppeerde),
   ge_no_e(gegaloppeerd)],adv,[],[]).

a([ge_e(gegapte),
   ge_no_e(gegapt)],adv,[],[]).

a([ge_e(gegarandeerde),
   ge_no_e(gegarandeerd)],adv,
  [pp(door),
   object_sbar],[]).

a([ge_e(gegarneerde),
   ge_no_e(gegarneerd)],adv,[],[]).

a([ge_no_e(gegeeuwd),
   ge_e(gegeeuwde)],adv,[],[]).

a([e(gegeide),
   no_e(gegeid)],adv,[],[]).

a([ge_e(gegeneerde),
   er(gegeneerder),
   ere(gegeneerdere),
   ge_no_e(gegeneerd),
   st(gegeneerdst),
   ste(gegeneerdste)],adv,[],[]).

a([ge_e(gegeneraliseerde),
   ge_no_e(gegeneraliseerd),
   ge_e(gegeneralizeerde),
   ge_no_e(gegeneralizeerd)],adv,[],[]).

a([ge_e(gegenereerde),
   ge_no_e(gegenereerd)],adv,[],[]).

a([ge_e(gegeselde),
   ge_no_e(gegeseld)],adv,[],[]).

a([ge_e(gegespte),
   ge_no_e(gegespt)],adv,[],[]).

a([ge_both(gegeten)],adv,[],[]).

a([ge_both(gegeven)],adv,
  [so_np,
   so_pp(aan),
   subject_sbar,
   subject_vp],[]).

a([ge_e(gegijzelde),
   ge_no_e(gegijzeld)],adv,[],[]).

a([ge_e(gegilde),
   ge_no_e(gegild)],adv,[],[]).

a([ge_e(gegiste),
   ge_no_e(gegist)],adv,[],[]).

a([ge_e(geglaceerde),
   ge_no_e(geglaceerd)],adv,[],[]).

a([ge_e(geglansde),
   ge_no_e(geglansd)],adv,[],[]).

a([ge_e(geglazuurde),
   ge_no_e(geglazuurd)],padv,[ap_pred],[]).

a([ge_both(gegleden)],adv,[],[]).

a([ge_e(geglibberde),
   ge_no_e(geglibberd)],adv,[],[]).

a([ge_e(geglipte),
   ge_no_e(geglipt)],adv,[],[]).

a([ge_both(geglommen)],adv,[],[]).

a([e(gegoede),
   no_e(gegoed)],nonadv,[],[]).

a([ge_both(gegolden)],adv,[],[]).

a([ge_e(gegokte),
   ge_no_e(gegokt)],nonadv,[],[]).

a([ge_e(gegolfde),
   ge_no_e(gegolfd)],nonadv,[],[]).

a([ge_e(gegooide),
   ge_no_e(gegooid)],adv,[nonp_pred],[]).

a([ge_both(gegoten)],adv,
  [pp(door),
   pp(in)],[]).

a([ge_e(gegraaide),
   ge_no_e(gegraaid)],adv,[],[]).

a([ge_e(gegrabbelde),
   ge_no_e(gegrabbeld)],adv,[],[]).

a([ge_e(gegrapte),
   ge_no_e(gegrapt)],adv,[],[]).

a([ge_e(gegraveerde),
   ge_no_e(gegraveerd)],adv,[],[]).

a([ge_both(gegraven)],adv,[],[]).

a([ge_e(gegrendelde),
   ge_no_e(gegrendeld)],adv,[],[]).

a([ge_both(gegrepen)],adv,
  [fixed([[uit,het,hart],dat])],[]).

a([ge_e(gegriefde),
   ge_no_e(gegriefd)],adv,[],[]).

a([ge_e(gegrifte),
   ge_no_e(gegrift)],adv,[],[]).

a([ge_e(gegrijsde),
   ge_no_e(gegrijsd)],adv,[],[]).

a([ge_e(gegrilde),
   ge_no_e(gegrild)],padv,[],[]).

a([ge_e(gegriste),
   ge_no_e(gegrist)],adv,[],[]).

a([ge_e(gegroefde),
   ge_no_e(gegroefd)],padv,[],[]).

a([ge_e(gegroeide),
   ge_no_e(gegroeid)],adv,[],[]).

a([ge_e(gegroepeerde),
   ge_no_e(gegroepeerd)],adv,[],[]).

a([ge_e(gegroete),
   ge_no_e(gegroet)],adv,[],[]).

a([ge_e(gegromde),
   ge_no_e(gegromd)],adv,[],[]).

a([ge_e(gegronde),
   er(gegronder),
   ere(gegrondere),
   ge_no_e(gegrond),
   st(gegrondst),
   ste(gegrondste)],adv,
  [pp(op)],[]).

a([ge_e(gegrondveste),
   ge_no_e(gegrondvest)],adv,[],[]).

a([ge_e(gegunde),
   ge_no_e(gegund)],adv,[],[]).

a([ge_e(gegutste),
   ge_no_e(gegutst)],adv,[],[]).

a([e(gehaaide),
   no_e(gehaaid)],padv,[],[]).

a([ge_e(gehaakte),
   ge_no_e(gehaakt)],adv,[],[]).

a([ge_e(gehaalde),
   ge_no_e(gehaald)],adv,
  [part(leeg)],[leeg]).

a([ge_e(gehaaste),
   er(gehaaster),
   ere(gehaastere),
   ge_no_e(gehaast)],adv,[],[]).

a([ge_e(gehate),
   er(gehater),
   ere(gehatere),
   ge_no_e(gehaat),
   st(gehaatst),
   ste(gehaatste)],adv,[],[]).

a([ge_e(gehakkelde),
   ge_no_e(gehakkeld)],adv,[],[]).

a([ge_e(gehakte),
   ge_no_e(gehakt)],adv,[],[]).

a([ge_e(gehallucineerde),
   ge_no_e(gehallucineerd)],adv,[],[]).

a([ge_e(gehalveerde),
   ge_no_e(gehalveerd)],adv,[],[]).

a([ge_e(gehamerde),
   ge_no_e(gehamerd)],adv,[],[]).

a([ge_no_e(gehandeld),
   ge_e(gehandelde)],adv,[],[]).

a([ge_e(gehandhaafde),
   ge_no_e(gehandhaafd)],adv,[],[]).

a([ge_e(gehandicapte),
   er(gehandicapter),
   ere(gehandicaptere),
   ge_no_e(gehandicapt),
   st(gehandicaptst),
   ste(gehandicaptste)],adv,[],[]).

a([e(gehandschoende),
   no_e(gehandschoend)],nonadv,[],[]).

a([ge_both(gehangen)],adv,[],[]).

a([ge_e(gehanteerde),
   ge_no_e(gehanteerd)],adv,[],[]).

a([ge_e(gehapte),
   ge_no_e(gehapt)],adv,[],[]).

a([ge_e(geharde),
   er(geharder),
   ere(gehardere),
   ge_no_e(gehard),
   st(gehardst),
   ste(gehardste)],adv,[],[]).

a([ge_e(geharkte),
   ge_no_e(geharkt)],adv,[],[]).

a([ge_e(geharmoniseerde),
   ge_no_e(geharmoniseerd)],nonadv,[],[]).

a([ge_e(gehavende),
   ge_no_e(gehavend)],padv,[],[]).

a([ge_e(gehechte),
   ge_no_e(gehecht)],adv,
  [er_pp_sbar(aan),
   er_pp_vp(aan),
   pp(aan)],[]).

a([e(gehele),
   no_e(geheel)],adv,[],[]).

a([ge_e(geheelde),
   ge_no_e(geheeld)],adv,[],[]).

a([ge_no_e(geheid),
   ge_e(geheide)],adv,
  [subject_sbar],[]).

a([ge_e(geheiligde),
   ge_no_e(geheiligd)],adv,[],[]).

a([e(geheime),
   er(geheimer),
   ere(geheimere),
   no_e(geheim),
   st(geheimst),
   ste(geheimste)],nonadv,[],[]).

a([e(geheimzinnige),
   er(geheimzinniger),
   ere(geheimzinnigere),
   no_e(geheimzinnig),
   st(geheimzinnigst),
   ste(geheimzinnigste)],adv,[],[]).

a([ge_e(gehekelde),
   ge_no_e(gehekeld)],nonadv,[],[]).

a([e(gehelmde),
   no_e(gehelmd)],padv,[],[]).

a([ge_e(geherbergde),
   ge_no_e(geherbergd)],adv,[],[]).

a([ge_e(geherstructureerde),
   ge_no_e(geherstructureerd)],padv,[],[]).

a([ge_both(gehesen)],adv,[],[]).

a([ge_both(geheten)],adv,
  [transitive,
   pred],[]).

a([ge_both(geheven)],adv,[],[]).

a([ge_e(gehinderde),
   ge_no_e(gehinderd)],adv,[],[]).

a([ge_e(gehitste),
   ge_no_e(gehitst)],adv,[],[]).

a([ge_e(gehoede),
   ge_no_e(gehoed)],adv,[],[]).

a([e(gehoekte),
   no_e(gehoekt)],adv,[],[]).

a([ge_e(gehoeste),
   ge_no_e(gehoest)],adv,[],[]).

a([ge_both(gehoeven)],adv,[],[]).

a([ge_e(gehokte),
   ge_no_e(gehokt)],adv,[],[]).

a([ge_e(geholde),
   ge_no_e(gehold)],adv,[],[]).

a([ge_both(geholpen)],adv,[],[]).

a([ge_e(gehonoreerde),
   ge_no_e(gehonoreerd)],adv,[],[]).

a([e(gehoofde),
   no_e(gehoofd)],adv,[],[]).

a([ge_e(gehoofddoekte),
   ge_no_e(gehoofddoekt)],padv,[],[]).

a([ge_e(gehoonde),
   ge_no_e(gehoond)],adv,[],[]).

a([ge_e(gehoopte),
   ge_no_e(gehoopt)],adv,[],[]).

a([ge_e(gehoorde),
   ge_no_e(gehoord)],adv,[],[]).

a([e(gehoorzame),
   er(gehoorzamer),
   ere(gehoorzamere),
   no_e(gehoorzaam),
   st(gehoorzaamst),
   ste(gehoorzaamste)],padv,
  [so_pp(aan),
   so_np],[]).

a([ge_e(gehopte),
   ge_no_e(gehopt)],adv,[],[]).

a([ge_e(gehotste),
   ge_no_e(gehotst)],adv,[],[]).

a([ge_both(gehouden)],adv,
  [er_pp_vp(aan),
   pp(aan),
   fixed([[in,stand]]),
   fixed([[instand]]),
   fixed([acc(rekening)]),
   fixed([acc(rekening),pc(met)]),
   pp(tot)],
  []).

a([ge_both(gehouden)],adv,
  [],
  [geheim]).

a([ge_both(gehouwen)],adv,[],[]).

a([ge_e(gehuichelde),
   ge_no_e(gehuicheld)],adv,[],[]).

a([ge_e(gehuisde),
   ge_no_e(gehuisd)],adv,[],[]).

a([ge_e(gehuisveste),
   ge_no_e(gehuisvest)],adv,
  [pp(in)],[]).

a([ge_e(gehulde),
   ge_no_e(gehuld)],padv,[],[]).

a([ge_e(gehuldigde),
   ge_no_e(gehuldigd)],adv,[],[]).

a([e(gehumeurde),
   no_e(gehumeurd)],padv,[],[]).

a([ge_e(gehuppelde),
   ge_no_e(gehuppeld)],adv,[],[]).

a([ge_no_e(gehurkt),
   ge_e(gehurkte)],padv,[],[]).

a([ge_e(gehuurde),
   ge_no_e(gehuurd)],adv,[],[]).

a([ge_e(gehuwde),
   ge_no_e(gehuwd)],adv,
  [pp(met)],[]).

a([ge_e(gehypnotiseerde),
   ge_no_e(gehypnotiseerd),
   ge_e(gehypnotizeerde),
   ge_no_e(gehypnotizeerd)],adv,[],[]).

a([ge_e(gehypte),
   ge_no_e(gehypt)],padv,[],[]).

a([ge_e(geijkte),
   ge_no_e(geijkt)],adv,
  [pp(op)],[]).

a([ge_e(geijlde),
   ge_no_e(geijld)],adv,[],[]).

a([e(geile),
   er(geiler),
   ere(geilere),
   no_e(geil),
   st(geilst),
   ste(geilste)],adv,[],[]).

a([e(geinige),
   no_e(geinig)],adv,[],[]).

a([ge_e(geïoniseerde),
   ge_no_e(geïoniseerd)],nonadv,[],[]).

a([ge_e(gejaagde),
   er(gejaagder),
   ere(gejaagdere),
   ge_no_e(gejaagd),
   st(gejaagdst),
   ste(gejaagdste)],adv,[],[]).

a([ge_e(gejatte),
   ge_no_e(gejat)],adv,[],[]).

a([e(gejonde),
   no_e(gejond)],adv,[],[]).

%% dat bleek te vroeg gejuicht
a([ge_e(gejuichte),
   ge_no_e(gejuicht)],adv,[],[]).

a([e(gekke),
   er(gekker),
   ere(gekkere),
   no_e(gek),
   st(gekst),
   ste(gekste)],both,
  [subject_sbar,
   subject_vp,
   er_pp_vp(op),
   pp(op),
   pp(van)],[]).

a([ge_e(gekaalde),
   ge_no_e(gekaald)],adv,[],[]).

a([ge_e(gekaapte),
   ge_no_e(gekaapt)],adv,[],[]).

a([ge_e(gekaatste),
   ge_no_e(gekaatst)],adv,[],[]).

a([ge_e(gekalkte),
   ge_no_e(gekalkt)],adv,[],[]).

a([ge_e(gekalmeerde),
   ge_no_e(gekalmeerd)],adv,[],[]).

a([ge_e(gekamde),
   ge_no_e(gekamd)],padv,[],[]).

a([ge_e(gekanaliseerde),
   ge_no_e(gekanaliseerd),
   ge_e(gekanalizeerde),
   ge_no_e(gekanalizeerd)],adv,[],[]).

a([ge_e(gekankerde),
   ge_no_e(gekankerd)],adv,[],[]).

a([ge_e(gekante),
   ge_no_e(gekant)],adv,
  [er_pp_sbar(tegen),
   er_pp_vp(tegen),
   pp(tegen)],[]).

a([ge_e(gekantelde),
   ge_no_e(gekanteld)],adv,[],[]).

a([ge_e(gekapseisde),
   ge_no_e(gekapseisd)],adv,[],[]).

a([ge_e(gekapte),
   ge_no_e(gekapt)],adv,[],[]).

a([ge_e(gekarakteriseerde),
   ge_no_e(gekarakteriseerd),
   ge_e(gekarakterizeerde),
   ge_no_e(gekarakterizeerd)],adv,[],[]).

a([ge_e(gekastreerde),
   ge_no_e(gekastreerd)],adv,[],[]).

a([ge_e(gekauwde),
   ge_no_e(gekauwd)],adv,[],[]).

a([ge_e(gekeelde),
   ge_no_e(gekeeld)],adv,[],[]).

a([ge_e(gekeerde),
   ge_no_e(gekeerd)],adv,[],[]).

a([ge_e(gekelderde),
   ge_no_e(gekelderd)],nonadv,[],[]).

a([ge_e(gekende),
   ge_no_e(gekend)],adv,[],[]).

a([ge_e(gekenmerkte),
   ge_no_e(gekenmerkt)],adv,[],[]).

a([ge_e(gekenschetste),
   ge_no_e(gekenschetst)],adv,[],[]).

a([ge_no_e(gekerfd),
   ge_e(gekerfde)],adv,[],[]).

a([ge_e(gekermde),
   ge_no_e(gekermd)],adv,[],[]).

a([ge_e(gekerstende),
   ge_no_e(gekerstend)],adv,[],[]).

a([ge_e(geketende),
   ge_no_e(geketend)],adv,[],[]).

a([ge_e(geketste),
   ge_no_e(geketst)],adv,[],[]).

a([ge_e(gekeurde),
   ge_no_e(gekeurd)],adv,[],[]).

a([ge_e(gekidnapte),
   ge_no_e(gekidnapt)],adv,[],[]).

a([ge_e(gekiemde),
   ge_no_e(gekiemd)],adv,[],[]).

a([ge_e(gekieperde),
   ge_no_e(gekieperd)],adv,[],[]).

a([ge_e(gekietelde),
   ge_no_e(gekieteld)],adv,[],[]).

a([ge_e(geklaagde),
   ge_no_e(geklaagd)],adv,[],[]).

a([ge_e(geklaarde),
   ge_no_e(geklaard)],adv,[],[]).

a([ge_e(geklampte),
   ge_no_e(geklampt)],adv,[],[]).

a([ge_no_e(geklapt),
   ge_e(geklapte)],adv,[],[]).

a([ge_e(geklasseerde),
   ge_no_e(geklasseerd)],adv,[],[]).

a([ge_e(geklassificeerde),
   ge_no_e(geklassificeerd)],adv,[],[]).

a([ge_e(geklauterde),
   ge_no_e(geklauterd)],adv,[],[]).

a([ge_e(geklauwde),
   ge_no_e(geklauwd)],adv,[],[]).

a([ge_e(geklede),
   er(gekleder),
   ere(gekledere),
   ge_no_e(gekleed),
   st(gekleedst),
   ste(gekleedste)],adv,
  [pp(in)],
  [goed,
   schaars]).

a([ge_e(gekleefde),
   ge_no_e(gekleefd)],adv,[],[]).

a([ge_e(gekleineerde),
   ge_no_e(gekleineerd)],adv,[],[]).

a([ge_e(geklemde),
   ge_no_e(geklemd)],adv,[],[]).

a([ge_e(gekletste),
   ge_no_e(gekletst)],adv,[],[]).

a([ge_e(gekleurde),
   er(gekleurder),
   ere(gekleurdere),
   ge_no_e(gekleurd),
   st(gekleurdst),
   ste(gekleurdste)],adv,[ap_pred],[]).

a([ge_e(gekliefde),
   ge_no_e(gekliefd)],adv,[],[]).

a([ge_e(geklokte),
   ge_no_e(geklokt)],adv,[],[]).

%% geklonken ijzer
a([ge_both(geklonken)],nonadv,[],[]).

a([ge_no_e(gekloond),
   ge_e(gekloonde)],adv,[],[]).

a([ge_e(geklopte),
   ge_no_e(geklopt)],adv,[],[]).

a([ge_e(gekloste),
   ge_no_e(geklost)],adv,[],[]).

a([ge_both(gekloven)],padv,[],[]).

a([ge_e(gekluisterde),
   ge_no_e(gekluisterd)],adv,[pp(aan)],[]).

a([ge_e(geknaagde),
   ge_no_e(geknaagd)],adv,[],[]).

a([ge_e(geknabbelde),
   ge_no_e(geknabbeld)],adv,[],[]).

a([ge_e(geknakte),
   ge_no_e(geknakt)],adv,[],[]).

a([ge_e(geknalde),
   ge_no_e(geknald)],adv,[],[]).

a([ge_e(geknapte),
   ge_no_e(geknapt)],adv,[],[]).

a([ge_e(geknede),
   ge_no_e(gekneed)],adv,[],[]).

a([ge_e(geknelde),
   ge_no_e(gekneld)],adv,[],[]).

a([ge_both(geknepen)],adv,[],[]).

a([ge_e(gekneusde),
   ge_no_e(gekneusd)],padv,[],[]).

a([ge_e(geknevelde),
   ge_no_e(gekneveld)],padv,[],[]).

a([ge_e(geknielde),
   ge_no_e(geknield)],padv,[],[]).

a([ge_e(geknikte),
   ge_no_e(geknikt)],adv,[],[]).

a([ge_e(geknipte),
   ge_no_e(geknipt)],adv,
  [er_pp_vp(voor),
   pp(voor)],[]).

a([ge_e(geknipte),
   ge_no_e(geknipt)],adv,
  [],[fijn]).

a([ge_no_e(geknoeid),
   ge_e(geknoeide)],adv,[],[]).

a([ge_e(geknoopte),
   ge_no_e(geknoopt)],adv,[],[]).

a([ge_e(geknotte),
   ge_no_e(geknot)],padv,[],[]).

a([ge_e(geknuffelde),
   ge_no_e(geknuffeld)],adv,[],[]).

a([ge_e(geknutselde),
   ge_no_e(geknutseld)],adv,[],[]).

a([ge_e(gekochte),
   ge_no_e(gekocht)],adv,[],[]).

a([ge_e(gekodeerde),
   ge_no_e(gekodeerd)],adv,[],[]).

a([ge_e(gekoelde),
   ge_no_e(gekoeld)],adv,[],[]).

a([ge_e(gekoerste),
   ge_no_e(gekoerst)],adv,[],[]).

a([ge_e(gekoesterde),
   ge_no_e(gekoesterd)],adv,[],[]).

a([ge_e(gekoloniseerde),
   ge_no_e(gekoloniseerd),
   ge_e(gekolonizeerde),
   ge_no_e(gekolonizeerd)],adv,[],[]).

a([ge_e(gekombineerde),
   ge_no_e(gekombineerd)],adv,[],[]).

a([ge_both(gekomen)],adv,
  [fixed([[tot,stand]]),
   fixed([[om,het,leven]]),
   fixed([[beschikbaar]])
  ],[]).

a([ge_e(gekommandeerde),
   ge_no_e(gekommandeerd)],adv,[],[]).

a([ge_no_e(gekommuniceerd),
   ge_e(gekommuniceerde)],adv,[],[]).

a([ge_e(gekompenseerde),
   ge_no_e(gekompenseerd)],adv,[],[]).

a([ge_e(gekompleteerde),
   ge_no_e(gekompleteerd)],adv,[],[]).

a([ge_e(gekompliceerde),
   ge_no_e(gekompliceerd)],adv,[],[]).

a([ge_e(gekomplimenteerde),
   ge_no_e(gekomplimenteerd)],adv,[],[]).

a([ge_e(gekomponeerde),
   ge_no_e(gekomponeerd)],adv,[],[]).

a([ge_e(gekompromitteerde),
   ge_no_e(gekompromitteerd)],adv,[],[]).

a([ge_e(gekoncentreerde),
   ge_no_e(gekoncentreerd)],adv,[],[]).

a([ge_e(gekoncipieerde),
   ge_no_e(gekoncipieerd)],adv,[],[]).

a([ge_e(gekondenseerde),
   ge_no_e(gekondenseerd)],adv,[],[]).

a([ge_e(gekonditioneerde),
   ge_no_e(gekonditioneerd)],adv,[],[]).

a([ge_e(gekondoleerde),
   ge_no_e(gekondoleerd)],adv,[],[]).

a([ge_e(gekonfronteerde),
   ge_no_e(gekonfronteerd)],adv,[],[]).

a([ge_e(gekonkludeerde),
   ge_no_e(gekonkludeerd)],adv,[],[]).

a([ge_e(gekonkretizeerde),
   ge_no_e(gekonkretizeerd)],adv,[],[]).

a([ge_e(gekonserveerde),
   ge_no_e(gekonserveerd)],adv,[],[]).

a([ge_e(gekonsolideerde),
   ge_no_e(gekonsolideerd)],adv,[],[]).

a([ge_e(gekonstateerde),
   ge_no_e(gekonstateerd)],adv,[],[]).

a([ge_e(gekonstitueerde),
   ge_no_e(gekonstitueerd)],adv,[],[]).

a([ge_e(gekonstrueerde),
   ge_no_e(gekonstrueerd)],adv,[],[]).

a([ge_e(gekonsulteerde),
   ge_no_e(gekonsulteerd)],adv,[],[]).

a([ge_e(gekonsumeerde),
   ge_no_e(gekonsumeerd)],adv,[],[]).

a([ge_e(gekontinueerde),
   ge_no_e(gekontinueerd)],adv,[],[]).

a([ge_no_e(gekontrasteerd),
   ge_e(gekontrasteerde)],adv,[],[]).

a([ge_e(gekontroleerde),
   ge_no_e(gekontroleerd)],adv,[],[]).

a([e(gekooide),
   no_e(gekooid)],padv,[],[]).

a([ge_e(gekookte),
   ge_no_e(gekookt)],padv,[],[]).

a([ge_e(gekopieerde),
   ge_no_e(gekopieerd)],adv,[],[]).

a([ge_e(gekoppelde),
   ge_no_e(gekoppeld)],adv,[],[]).

a([ge_e(gekorrigeerde),
   ge_no_e(gekorrigeerd)],adv,[],[]).

a([ge_e(gekorte),
   ge_no_e(gekort)],adv,[],[]).

a([e(gekostumeerde),
   no_e(gekostumeerd)],adv,[],[]).

a([ge_e(gekotste),
   ge_no_e(gekotst)],adv,[],[]).

a([ge_both(gekozen)],adv,[],[]).

a([ge_e(gekoördineerde),
   ge_no_e(gekoördineerd)],adv,[],[]).

a([ge_e(gekraaide),
   ge_no_e(gekraaid)],adv,[],[]).

a([ge_e(gekraakte),
   ge_no_e(gekraakt)],adv,[],[]).

a([ge_e(gekrabbelde),
   ge_no_e(gekrabbeld)],adv,[],[]).

a([ge_e(gekrabde),
   ge_no_e(gekrabd)],adv,[],[]).

a([ge_e(gekraste),
   ge_no_e(gekrast)],adv,[],[]).

a([ge_both(gekregen)],adv,[],[]).

a([ge_e(gekrenkte),
   er(gekrenkter),
   ere(gekrenktere),
   ge_no_e(gekrenkt),
   st(gekrenktst),
   ste(gekrenktste)],padv,
  [pp(door)],[]).

a([ge_e(gekrepeerde),
   ge_no_e(gekrepeerd)],adv,[],[]).

a([e(gekreukelde),
   no_e(gekreukeld)],padv,[],[]).

a([ge_e(gekriebelde),
   ge_no_e(gekriebeld)],adv,[],[]).

a([no_e(kristallijn),
   e(kristallijne)],nonadv,[],[]).

a([ge_e(gekristalliseerde),
   ge_no_e(gekristalliseerd),
   ge_e(gekristallizeerde),
   ge_no_e(gekristallizeerd)],adv,[],[]).

a([ge_e(gecritiseerde),
   ge_no_e(gecritiseerd),
   ge_e(gecritizeerde),
   ge_no_e(gecritizeerd),
   ge_e(gekritiseerde),
   ge_no_e(gekritiseerd),
   ge_e(gekritizeerde),
   ge_no_e(gekritizeerd)],adv,[],[]).

a([ge_e(gekromde),
   ge_no_e(gekromd)],adv,[],[]).

a([ge_both(gekrompen)],adv,[],[]).

a([ge_e(gekronkelde),
   ge_no_e(gekronkeld)],adv,[],[]).

a([ge_e(gekroonde),
   ge_no_e(gekroond)],adv,[],[]).

a([ge_e(gekruide),
   er(gekruider),
   ere(gekruidere),
   ge_no_e(gekruid),
   st(gekruidst),
   ste(gekruidste)],adv,
  [pp(met)],[]).

a([ge_e(gekruisigde),
   ge_no_e(gekruisigd)],adv,[],[]).

a([ge_e(gekruiste),
   ge_no_e(gekruist)],adv,[],[]).

a([ge_e(gekrulde),
   ge_no_e(gekruld)],adv,[],[]).

a([ge_e(gekuierde),
   ge_no_e(gekuierd)],adv,[],[]).

a([ge_e(gekuiste),
   ge_no_e(gekuist),
   er(gekuister),
   ere(gekuistere)],padv,[],[]).

a([ge_no_e(gekulmineerd),
   ge_e(gekulmineerde)],adv,[],[]).

a([ge_e(gekultiveerde),
   ge_no_e(gekultiveerd)],adv,[],[]).

a([e(gekunstelde),
   er(gekunstelder),
   ere(gekunsteldere),
   no_e(gekunsteld),
   st(gekunsteldst),
   ste(gekunsteldste)],adv,[],[]).

a([ge_e(gekuste),
   ge_no_e(gekust)],adv,[],[]).

a([ge_e(gekwakte),
   ge_no_e(gekwakt)],adv,[],[]).

a([ge_e(gekwalificeerde),
   ge_e(gequalificeerde),
   er(gekwalificeerder),
   er(gequalificeerder),
   ere(gekwalificeerdere),
   ere(gequalificeerdere),
   ge_no_e(gekwalificeerd),
   ge_no_e(gequalificeerd),
   st(gekwalificeerdst),
   st(gequalificeerdst),
   ste(gekwalificeerdste),
   ste(gequalificeerdste)],padv,
  [pp(voor),
   object_vp],[]).

a([ge_e(gekwantificeerde),
   ge_no_e(gekwantificeerd)],adv,[],[]).

a([ge_e(gekweekte),
   ge_no_e(gekweekt)],adv,[],[]).

a([ge_e(gekwelde),
   er(gekwelder),
   ere(gekweldere),
   ge_no_e(gekweld),
   st(gekweldst),
   ste(gekweldste)],adv,
  [pp(door)],[]).

a([ge_both(gekweten)],adv,[],[]).

a([ge_e(gekwetste),
   er(gekwetster),
   ere(gekwetstere),
   ge_no_e(gekwetst)],adv,
  [pp(door)],[]).

a([ge_e(gelaafde),
   ge_no_e(gelaafd)],adv,[],[]).

a([e(gelaagde),
   no_e(gelaagd)],adv,[],[]).

a([ge_e(gelaakte),
   ge_no_e(gelaakt)],adv,[],[]).

a([e(gelaarsde),
   no_e(gelaarsd)],padv,[],[]).

a([ge_e(gelabelde),
   ge_no_e(gelabeld)],padv,[],[]).

a([ge_both(geladen),
   er(geladener),
   ere(geladenere),
   st(geladenst),
   ste(geladenste)],adv,
  [pp(met)],[]).

a([ge_e(gelakte),
   ge_no_e(gelakt)],adv,[],[]).

a([ge_e(gelalde),
   ge_no_e(gelald)],adv,[],[]).

a([ge_e(gelanceerde),
   ge_no_e(gelanceerd)],padv,[],[]).

a([ge_e(gelande),
   ge_no_e(geland)],adv,[],[]).

a([e(gelangde),
   no_e(gelangd)],adv,[],[]).

a([ge_e(gelapte),
   ge_no_e(gelapt)],adv,[],[]).

a([ge_e(gelardeerde),
   ge_no_e(gelardeerd)],padv,
  [pp(met)],[]).

a([ge_e(gelaste),
   ge_no_e(gelast)],adv,[],[]).

a([ge_both(gelaten),
   er(gelatener),
   ere(gelatenere),
   st(gelatenst),
   ste(gelatenste)],padv,
  [fixed([[in,de,steek]]),
   fixed([[buiten,beschouwing],acc]),
   fixed([[achterwege]]),
   fixed([[daar],acc])],[]).

a([e(gelauwerde),
   no_e(gelauwerd)],padv,[],[]).

a([ge_e(gelaveerde),
   ge_no_e(gelaveerd)],adv,[],[]).

a([ge_e(gelazerde),
   ge_no_e(gelazerd)],adv,[],[]).

a([e(geldelijke),
   no_e(geldelijk)],adv,[],[]).

a([e(gelderse),
   no_e(gelders)],nonadv,[],[]).

a([e(geldige),
   er(geldiger),
   ere(geldigere),
   no_e(geldig),
   st(geldigst),
   ste(geldigste)],adv,  % adv VL: geldig vergaderen
  [pp(tot),
   pp(voor)],[]).

a([ge_e(geleaste),
   ge_no_e(geleast),
   ge_e(geleasde),
   ge_no_e(geleasd)],adv,[],[]).

a([ge_both(geleden)],tmpadv,
  [pp(door),
   subject_sbar],[]).

a([ge_e(geledigde),
   ge_no_e(geledigd)],adv,[],[]).

a([e(gelede),
   no_e(geleed)],nonadv,[],[]).

a([ge_e(geleefde),
   ge_no_e(geleefd)],adv,[],[]).

a([ge_e(geleegde),
   ge_no_e(geleegd)],adv,[],[]).

a([ge_e(geleende),
   ge_no_e(geleend)],adv,[],[]).

a([ge_e(geleerde),
   er(geleerder),
   ere(geleerdere),
   ge_no_e(geleerd),
   st(geleerdst),
   ste(geleerdste)],adv,
  [subject_sbar],[]).

a([ge_e(gelegaliseerde),
   ge_no_e(gelegaliseerd),
   ge_e(gelegalizeerde),
   ge_no_e(gelegalizeerd)],adv,[],[]).

a([ge_e(gelegde),
   ge_no_e(gelegd)],adv,
  [fixed([[ten,laste]]),
   fixed([[ten,laste],dat]),
   fixed([[tenlaste]]),
   fixed([[te,vondeling]]),
   fixed([[tenlaste],dat]),
   fixed([pp_pred(in,as)])
  ],[]).

a([ge_e(gelegde),
   ge_no_e(gelegd)],adv,
  [],[droog]).

a([ge_both(gelegen),
   er(gelegener),
   ere(gelegenere),
   st(gelegenst),
   ste(gelegenste)],adv,
  [],[naast]).

a([ge_e(gelegerde),
   ge_no_e(gelegerd)],adv,[],[]).

a([ge_e(gelegitimeerde),
   ge_no_e(gelegitimeerd)],adv,[],[]).

a([ge_e(geleide),
   ge_no_e(geleid)],padv,[ld_pp],[]).

a([e(geleidelijke),
   no_e(geleidelijk),
   er(geleidelijker),
   ere(geleidelijkere),
   st(geleidelijkst),
   ste(geleidelijkste)],adv,[],[]).

a([ge_e(gelekte),
   ge_no_e(gelekt)],adv,[],[]).

a([ge_e(gelepelde),
   ge_no_e(gelepeld)],adv,[],[]).

a([ge_e(geleste),
   ge_no_e(gelest)],adv,[],[]).

a([ge_e(gelette),
   ge_no_e(gelet)],adv,[],[]).

a([e(geletterde),
   er(geletterder),
   ere(geletterdere),
   no_e(geletterd),
   st(geletterdst),
   ste(geletterdste)],nonadv,[],[]).

a([ge_e(geleunde),
   ge_no_e(geleund)],padv,[],[]).

a([ge_e(geleverde),
   ge_no_e(geleverd)],adv,
  [so_np,
   so_pp(aan)],[]).

a([ge_both(gelezen)],adv,
  [],[]).

a([ge_e(geliberaliseerde),
   ge_no_e(geliberaliseerd)],padv,[],[]).

a([ge_e(gelichte),
   ge_no_e(gelicht)],adv,[],[]).

a([ge_e(gelieerde),
   ge_no_e(gelieerd)],padv,
  [pp(aan),
   pp(met)],[]).

a([ge_e(geliefde),
   er(geliefder),
   ere(geliefdere),
   ge_no_e(geliefd),
   st(geliefdst),
   ste(geliefdste)],adv,
  [pp(bij)],[]).

a([ge_e(geliefkoosde),
   ge_no_e(geliefkoosd)],adv,[],[]).

a([ge_e(gelifte),
   ge_no_e(gelift)],adv,[],[]).

a([e(gelige),
   er(geliger),
   ere(geligere),
   no_e(gelig),
   st(geligst),
   ste(geligste)],nonadv,[],[]).

a([e(gelijke),
   er(gelijker),
   ere(gelijkere),
   postn_no_e(gelijk),
   st(gelijkst),
   ste(gelijkste)],adv,
  [pp(aan)],[]).

a([e(gelijkaardige),
   no_e(gelijkaardig)],nonadv,[],[]).

a([ende(gelijkende),
   er(gelijkender),
   ere(gelijkendere),
   end(gelijkend),
   st(gelijkendst),
   ste(gelijkendste)],adv,[],[]).

a([ge_e(gelijkgestelde),
   ge_no_e(gelijkgesteld)],adv,[],[]).

a([ge_e(gelijkgestemde),
   ge_no_e(gelijkgestemd)],adv,[],[]).

a([ende(gelijkluidende),
   end(gelijkluidend)],adv,[],[]).

a([e(gelijkmatige),
   er(gelijkmatiger),
   ere(gelijkmatigere),
   no_e(gelijkmatig),
   st(gelijkmatigst),
   ste(gelijkmatigste)],adv,[],[]).

a([e(gelijknamige),
   no_e(gelijknamig)],nonadv,[],[]).

a([end(gelijkopgaand),
   ende(gelijkopgaande)],adv,[],[]).

a([e(gelijksoortige),
   no_e(gelijksoortig)],nonadv,[],[]).

a([e(gelijktijdige),
   no_e(gelijktijdig)],adv,
  [pp(met)],[]).

a([e(gelijkvloerse),
   no_e(gelijkvloers)],locadv,[],[]).

a([e(gelijkwaardige),
   no_e(gelijkwaardig)],padv,
  [pp(aan)],[]).

a([ge_e(gelijmde),
   ge_no_e(gelijmd)],adv,[],[]).

a([ge_e(gelijnde),
   ge_no_e(gelijnd)],adv,[],[]).

a([ge_e(gelikte),
   ge_no_e(gelikt)],adv,[],[]).

a([ge_e(gelikwideerde),
   ge_no_e(gelikwideerd)],adv,[],[]).

a([ge_e(gelimiteerde),
   ge_no_e(gelimiteerd)],adv,[],[]).

a([ge_e(gelinkte),
   ge_no_e(gelinkt)],padv,
  [pp(aan)],[]).

a([ge_e(geliquideerde),
   ge_no_e(geliquideerd)],adv,[],[]).

a([ge_e(gelispelde),
   ge_no_e(gelispeld)],adv,[],[]).

a([ge_e(gelocaliseerde),
   ge_no_e(gelocaliseerd)],adv,[],[]).

a([ge_e(gelogeerde),
   ge_no_e(gelogeerd)],adv,[],[]).

a([ge_both(gelogen)],adv,[],[]).

a([ge_e(gelogenstrafte),
   ge_no_e(gelogenstraft)],adv,[],[]).

a([ge_e(gelokaliseerde),
   ge_no_e(gelokaliseerd),
   ge_e(gelokalizeerde),
   ge_no_e(gelokalizeerd)],adv,[],[]).

a([both(geloken)],padv,[],[]).

a([ge_e(gelokte),
   ge_no_e(gelokt)],adv,[],[]).

a([ge_e(geloochende),
   ge_no_e(geloochend)],adv,[],[]).

a([ge_e(geloodste),
   ge_no_e(geloodst)],adv,[],[]).

a([ge_e(geloofde),
   ge_no_e(geloofd)],adv,[],[]).

a([e(geloofwaardige),
   er(geloofwaardiger),
   ere(geloofwaardigere),
   no_e(geloofwaardig),
   st(geloofwaardigst),
   ste(geloofwaardigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(geloogde),
   ge_no_e(geloogd)],adv,[],[]).

a([ge_e(gelooide),
   ge_no_e(gelooid)],adv,[],[]).

a([ge_e(geloonde),
   ge_no_e(geloond)],adv,[],[]).

a([ge_e(geloosde),
   ge_no_e(geloosd)],adv,[],[]).

a([ge_both(gelopen)],adv,
  [part(achterna,transitive),
   fixed([[uit,de,hand]])],[]).

a([ge_e(geloste),
   ge_no_e(gelost)],adv,[],[]).

a([ge_e(gelouterde),
   ge_no_e(gelouterd)],adv,[],[]).

a([e(gelovige),
   er(geloviger),
   ere(gelovigere),
   no_e(gelovig),
   st(gelovigst),
   ste(gelovigste)],adv,[],[]).

a([ge_e(geluchte),
   ge_no_e(gelucht)],adv,[],[]).

a([ge_e(geluide),
   ge_no_e(geluid)],adv,[],[]).

a([e(geluidloze),
   er(geluidlozer),
   ere(geluidlozere),
   no_e(geluidloos),
   st(geluidloost),
   ste(geluidlooste)],adv,[],[]).

a([ge_e(gelukgewenste),
   ge_no_e(gelukgewenst)],adv,[],[]).

a([e(gelukkige),
   er(gelukkiger),
   ere(gelukkigere),
   no_e(gelukkig),
   st(gelukkigst),
   ste(gelukkigste)],adv,
  [object_sbar,
   object_vp,
   er_pp_sbar(met),
   er_pp_vp(met),
   pp(met),
   subject_sbar,
   subject_vp],[]).

a([ge_e(gelukte),
   ge_no_e(gelukt)],adv,[],[]).

a([e(gelukzalige),
   er(gelukzaliger),
   ere(gelukzaligere),
   no_e(gelukzalig),
   st(gelukzaligst),
   ste(gelukzaligste)],padv,[],[]).

a([ge_e(geluste),
   ge_no_e(gelust)],adv,[],[]).

a([ge_e(geluwde),
   ge_no_e(geluwd)],adv,[],[]).

a([ge_e(gemaaide),
   ge_no_e(gemaaid)],adv,[],[]).

a([ge_e(gemaakte),
   ge_no_e(gemaakt)],adv,
  [pp(door),
   pp(in),
   fixed([[te,gelde]])],[]).

a([ge_no_e(gemaald),
   ge_e(gemaalde)],adv,[],[]).

a([ge_e(gemaande),
   ge_no_e(gemaand)],adv,[],[]).

a([ge_e(gemachtigde),
   ge_no_e(gemachtigd)],adv,[],[]).

a([e(gemakkelijke),
   er(gemakkelijker),
   ere(gemakkelijkere),
   no_e(gemakkelijk),
   st(gemakkelijkst),
   ste(gemakkelijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(in),
   pp(voor)],[]).

a([e(gemakzuchtige),
   no_e(gemakzuchtig)],adv,[subject_sbar,
                            subject_vp],[]).

a([ge_both(gemalen)],adv,[],[]).

a([ge_e(gemanifesteerde),
   ge_no_e(gemanifesteerd)],adv,[],[]).

a([ge_e(gemanipuleerde),
   ge_no_e(gemanipuleerd)],adv,[],[]).

a([ge_e(gemankeerde),
   ge_no_e(gemankeerd)],adv,[],[]).

a([ge_e(gemanoeuvreerde),
   ge_no_e(gemanoeuvreerd)],adv,[],[]).

a([ge_e(gemarcheerde),
   ge_no_e(gemarcheerd)],adv,[],[]).

a([e(gemarde),
   no_e(gemard)],adv,[],[]).

a([ge_e(gemarginaliseerde),
   ge_no_e(gemarginaliseerd)],padv,[],[]).

a([ge_e(gemarineerde),
   ge_no_e(gemarineerd)],padv,[],[]).

a([ge_e(gemarkeerde),
   ge_no_e(gemarkeerd)],adv,[],[]).

a([ge_e(gemartelde),
   ge_no_e(gemarteld)],adv,[],[]).

a([ge_e(gemaskeerde),
   ge_no_e(gemaskeerd)],adv,[],[]).

a([ge_e(gemaskerde),
   ge_no_e(gemaskerd)],adv,[],[]).

a([ge_e(gemasseerde),
   ge_no_e(gemasseerd)],adv,[],[]).

a([ge_e(gematigde),
   er(gematigder),
   ere(gematigdere),
   ge_no_e(gematigd),
   st(gematigdst),
   ste(gematigdste)],adv,[],[]).

a([ge_e(gemechaniseerde),
   ge_no_e(gemechaniseerd),
   ge_e(gemechanizeerde),
   ge_no_e(gemechanizeerd)],adv,[],[]).

a([ge_both(gemeden)],adv,[],[]).

a([e(gemene),
   er(gemener),
   ere(gemenere),
   no_e(gemeen),
   st(gemeenst),
   ste(gemeenste)],adv,
  [subject_sbar,
   subject_vp,
   object_sbar],[]).

a([ge_e(gemeende),
   ge_no_e(gemeend)],adv,[],[]).

a([e(gemeenschappelijke),
   postn_no_e(gemeenschappelijk)],adv,[],[]).

a([e(gemeentelijke),
   no_e(gemeentelijk)],nonadv,[],[]).

a([ge_e(gemeerde),
   ge_no_e(gemeerd)],adv,[],[]).

a([ge_e(gemeesmuilde),
   ge_no_e(gemeesmuild)],adv,[],[]).

a([ge_e(gemelde),
   ge_no_e(gemeld)],adv,[],[]).

a([e(gemelijke),
   er(gemelijker),
   ere(gemelijkere),
   no_e(gemelijk),
   st(gemelijkst),
   ste(gemelijkste)],adv,[],[]).

a([ge_e(gememoreerde),
   ge_no_e(gememoreerd)],adv,[],[]).

a([ge_e(gemende),
   ge_no_e(gemend)],adv,[],[]).

a([ge_e(gemengde),
   er(gemengder),
   ere(gemengdere),
   ge_no_e(gemengd),
   st(gemengdst),
   ste(gemengdste)],adv,
  [pp(met)],[]).

a([ge_e(gemepte),
   ge_no_e(gemept)],adv,[],[]).

a([ge_e(gemerkte),
   ge_no_e(gemerkt)],adv,[],[]).

a([ge_both(gemeten)],adv,[],[]).

a([ge_e(gemetselde),
   ge_no_e(gemetseld)],adv,[],[]).

a([ge_e(gemeubileerde),
   ge_no_e(gemeubileerd)],adv,[],[]).

a([ge_e(gemiddelde),
   ge_no_e(gemiddeld)],adv,[],[boven]).

a([ge_e(gemikte),
   ge_no_e(gemikt)],adv,[],[]).

a([ge_e(gemillimeterde),
   ge_no_e(gemillimeterd)],padv,[],[]).

a([ge_e(geminachte),
   ge_no_e(geminacht)],adv,[],[]).

a([ge_e(geminderde),
   ge_no_e(geminderd)],adv,[],[]).

a([ge_e(geminimaliseerde),
   ge_no_e(geminimaliseerd),
   ge_e(geminimalizeerde),
   ge_no_e(geminimalizeerd)],adv,[],[]).

a([ge_e(gemiste),
   ge_no_e(gemist)],adv,[],[]).

a([ge_e(gemixte),
   ge_no_e(gemixt)],adv,[],[]).

a([ge_e(gemobiliseerde),
   ge_no_e(gemobiliseerd),
   ge_e(gemobilizeerde),
   ge_no_e(gemobilizeerd)],adv,[],[]).

a([ge_e(gemodelleerde),
   ge_no_e(gemodelleerd)],adv,[],[]).

a([ge_e(gemoderniseerde),
   ge_no_e(gemoderniseerd),
   ge_e(gemodernizeerde),
   ge_no_e(gemodernizeerd)],adv,[],[]).

a([e(gemodificeerde),
   no_e(gemodificeerd)],adv,[],[]).

a([e(gemoedelijke),
   er(gemoedelijker),
   ere(gemoedelijkere),
   no_e(gemoedelijk),
   st(gemoedelijkst),
   ste(gemoedelijkste)],adv,[],[]).

a([ge_e(gemoeide),
   ge_no_e(gemoeid)],adv,
  [pp(met)],[]).

a([ge_both(gemolken)],adv,[],[]).

a([ge_e(gemompelde),
   ge_no_e(gemompeld)],adv,[],[]).

a([ge_e(gemonsterde),
   ge_no_e(gemonsterd)],adv,[],[]).

a([ge_e(gemonteerde),
   ge_no_e(gemonteerd)],adv,[],[]).

a([ge_e(gemorste),
   ge_no_e(gemorst)],adv,[],[]).

a([ge_e(gemotte),
   ge_no_e(gemot)],adv,[],[]).

a([ge_e(gemotiveerde),
   er(gemotiveerder),
   ere(gemotiveerdere),
   ge_no_e(gemotiveerd),
   st(gemotiveerdst),
   ste(gemotiveerdste)],adv,
  [object_vp,
   pp(door)],[]).

a([e(gemotoriseerde),
   no_e(gemotoriseerd)],padv,[],[]).

a([ge_e(gemuilkorfde),
   ge_no_e(gemuilkorfd)],padv,[],[]).

a([ge_e(gemummelde),
   ge_no_e(gemummeld)],padv,[],[]).

a([ge_e(gemunte),
   ge_no_e(gemunt)],adv,
  [pp(op)],[]).

a([ge_e(genaaide),
   ge_no_e(genaaid)],adv,[],[]).

a([ge_both(genaamd),
   ge_e(genaamde)],nonadv,
  [transitive],[]).

a([e(genadeloze),
   er(genadelozer),
   ere(genadelozere),
   no_e(genadeloos),
   st(genadeloost),
   ste(genadelooste)],adv,[],[]).

a([ge_e(genaderde),
   ge_no_e(genaderd)],adv,[],[]).

a([e(genadige),
   er(genadiger),
   ere(genadigere),
   no_e(genadig),
   st(genadigst),
   ste(genadigste)],adv,[],[]).

a([ge_e(genagelde),
   ge_no_e(genageld)],adv,[],[]).

a([e(gênante),
   er(gênanter),
   ere(gênantere),
   no_e(gênant),
   st(gênantst),
   ste(gênantste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(genationaliseerde),
   ge_no_e(genationaliseerd),
   ge_e(genationalizeerde),
   ge_no_e(genationalizeerd)],adv,[],[]).

a([ge_e(genaturaliseerde),
   ge_no_e(genaturaliseerd)],padv,[],[]).

a([e(genealogische),
   no_e(genealogisch)],adv,[],[]).

a([e(geneeskrachtige),
   er(geneeskrachtiger),
   ere(geneeskrachtigere),
   no_e(geneeskrachtig),
   st(geneeskrachtigst),
   ste(geneeskrachtigste)],nonadv,[],[]).

a([e(geneeskundige),
   no_e(geneeskundig)],adv,[],[dier]).

a([ge_e(genegeerde),
   ge_no_e(genegeerd)],adv,[],[]).

a([ge_both(genegen)],adv,
  [subject_vp,
   object_vp],[]).

a([ge_e(geneigde),
   ge_no_e(geneigd)],adv,
  [object_vp,
   er_pp_vp(tot),
   pp(tot)],[]).

a([ge_e(genekte),
   ge_no_e(genekt)],padv,[],[]).

a([e(generale),
   no_e(generaal)],nonadv,[],[]).

a([e(genereuze),
   er(genereuzer),
   ere(genereuzere),
   no_e(genereus),
   st(genereust),
   ste(genereuste)],adv,[subject_vp],[]).

a([e(generieke),
   no_e(generiek)],adv,[],[]).

a([ge_e(genestelde),
   ge_no_e(genesteld)],adv,[],[]).

a([e(genetische),
   no_e(genetisch)],adv,[],[]).

a([ge_e(geneukte),
   ge_no_e(geneukt)],adv,[],[]).

a([ge_e(geneuriede),
   ge_no_e(geneuried)],adv,[],[]).

a([ge_e(geneutraliseerde),
   ge_no_e(geneutraliseerd),
   ge_e(geneutralizeerde),
   ge_no_e(geneutralizeerd)],adv,[],[]).

a([ge_both(genezen)],adv,[],[]).

a([e(geniale),
   er(genialer),
   ere(genialere),
   no_e(geniaal),
   st(geniaalst),
   ste(geniaalste)],adv,[],[]).

a([e(geniepige),
   er(geniepiger),
   ere(geniepigere),
   no_e(geniepig),
   st(geniepigst),
   ste(geniepigste)],adv,[],[]).

a([e(genitale),
   no_e(genitaal)],adv,[],[]).

a([ge_e(genivelleerde),
   ge_no_e(genivelleerd)],adv,[],[]).

a([ge_e(genodigde),
   ge_no_e(genodigd)],adv,[],[]).

a([both(genoeg)],adv,
  [object_vp,
   object_sbar,
   subject_sbar,
   subject_vp],[]).

a([e(genoeglijke),
   er(genoeglijker),
   ere(genoeglijkere),
   no_e(genoeglijk),
   st(genoeglijkst),
   ste(genoeglijkste)],adv,[],[]).

a([e(genoegzame),
   er(genoegzamer),
   ere(genoegzamere),
   no_e(genoegzaam),
   st(genoegzaamst),
   ste(genoegzaamste)],adv,[],[]).

a([ge_e(genoemde),
   ge_no_e(genoemd)],adv,
  [pp(door),
   pp(naar),
   fixed([[bij,name]]),
   transitive,
   pred],[]).

a([ge_both(genomen)],adv,
  [part(gevangen),
   fixed([svp_pp(in,aanmerking),acc]),
   fixed([svp_pp(in,aanmerking),sbar]),
   fixed([svp_pp(in,acht),acc]),
   fixed([svp_pp(in,acht),sbar]),
   fixed([[in,beslag]]),
   fixed([[in,de,arm]]),
   fixed([[in,ogenschouw],acc]),
   fixed([[onder,handen]])],[]).

a([ge_e(genomineerde),
   ge_no_e(genomineerd)],padv,[],[]).

a([ge_e(genode),
   ge_no_e(genood)],adv,[],[]).

a([ge_e(genoodzaakte),
   ge_no_e(genoodzaakt)],adv,[],[]).

a([ge_e(genoopte),
   ge_no_e(genoopt)],adv,[],[]).

a([ge_e(genormaliseerde),
   ge_no_e(genormaliseerd),
   ge_e(genormalizeerde),
   ge_no_e(genormalizeerd)],adv,[],[]).

a([ge_e(genormeerde),
   ge_no_e(genormeerd)],adv,[],[]).

a([ge_e(genoteerde),
   ge_no_e(genoteerd)],adv,[],[]).

a([ge_both(genoten)],adv,[],[]).

a([ge_e(genuanceerde),
   er(genuanceerder),
   ere(genuanceerdere),
   ge_no_e(genuanceerd),
   st(genuanceerdst),
   ste(genuanceerdste)],adv,[],[]).

a([ge_e(genummerde),
   ge_no_e(genummerd)],adv,[],[]).

a([ge_e(genuttigde),
   ge_no_e(genuttigd)],adv,[],[]).

a([ge_e(geobjectiveerde),
   ge_no_e(geobjectiveerd)],adv,[],[]).

a([ge_e(geobjektiveerde),
   ge_no_e(geobjektiveerd)],adv,[],[]).

a([ge_e(geobsedeerde),
   ge_no_e(geobsedeerd)],adv,[],[]).

a([ge_e(geobserveerde),
   ge_no_e(geobserveerd)],adv,[],[]).

a([ge_e(geoefende),
   er(geoefender),
   ere(geoefendere),
   ge_no_e(geoefend),
   st(geoefendst),
   ste(geoefendste)],adv,[],[]).

a([ge_e(geofferde),
   ge_no_e(geofferd)],adv,[],[]).

a([e(geografische),
   no_e(geografisch)],adv,[],[]).

a([ge_e(geoliede),
   ge_no_e(geolied)],adv,[],[]).

a([e(geologische),
   no_e(geologisch)],adv,[],[]). % adv: geologisch gezien

a([e(geometrische),
   no_e(geometrisch)],nonadv,[],[]).

a([ge_e(geoogste),
   ge_no_e(geoogst)],adv,[],[]).

a([ge_e(geoorde),
   ge_no_e(geoord)],nonadv,[],[]).

a([ge_e(geoordeelde),
   ge_no_e(geoordeeld)],adv,[],[]).

a([ge_e(geoorloofde),
   ge_no_e(geoorloofd)],adv,
  [subject_vp],[]).

a([ge_e(geoormerkte),
   ge_no_e(geoormerkt)],nonadv,[],[]).

a([ge_e(geopenbaarde),
   ge_no_e(geopenbaard)],adv,[],[]).

a([ge_e(geopende),
   ge_no_e(geopend)],adv,[],[]).

a([ge_e(geoperationaliseerde),
   ge_no_e(geoperationaliseerd)],adv,[],[]).

a([ge_e(geopereerde),
   ge_no_e(geopereerd)],adv,[],[]).

a([ge_e(geopperde),
   ge_no_e(geopperd)],adv,[],[]).

a([ge_e(geordende),
   er(geordender),
   ere(geordendere),
   ge_no_e(geordend),
   st(geordendst),
   ste(geordendste)],adv,[],[]).

a([ge_e(georganiseerde),
   ge_e(georganizeerde),
   er(georganiseerder),
   er(georganizeerder),
   ere(georganiseerdere),
   ere(georganizeerdere),
   ge_no_e(georganiseerd),
   ge_no_e(georganizeerd),
   st(georganiseerdst),
   st(georganizeerdst),
   ste(georganiseerdste),
   ste(georganizeerdste)],padv,
  [pp(door)],[]).

a([ge_e(georiënteerde),
   ge_no_e(georiënteerd)],nonadv,[],[]).

a([ge_e(georiënteerde),
   ge_no_e(georiënteerd)],adv,[],[]).

a([ge_e(georkestreerde),
   ge_no_e(georkestreerd)],padv,[],[]).

a([ge_e(gepaaide),
   ge_no_e(gepaaid)],adv,[],[]).

a([ge_e(gepaarde),
   ge_no_e(gepaard)],adv,
  [pp(met),
   pp(aan)],[]).

a([ge_e(gepakte),
   ge_no_e(gepakt)],padv,[],[]).

a([ge_e(gepantserde),
   ge_no_e(gepantserd)],adv,[],[]).

a([ge_e(gepareerde),
   ge_no_e(gepareerd)],adv,[],[]).

a([ge_e(geparelde),
   ge_no_e(gepareld)],adv,[],[]).

a([ge_e(geparfumeerde),
   er(geparfumeerder),
   ere(geparfumeerdere),
   ge_no_e(geparfumeerd),
   st(geparfumeerdst),
   ste(geparfumeerdste)],adv,[],[]).

a([ge_e(geparkeerde),
   ge_no_e(geparkeerd)],adv,[],[]).

a([ge_e(gepasseerde),
   ge_no_e(gepasseerd)],adv,[],[]).

a([e(gepassioneerde),
   no_e(gepassioneerd)],adv,[],[]).

a([ge_e(gepaste),
   er(gepaster),
   ere(gepastere),
   ge_no_e(gepast)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(gepasteuriseerde),
   ge_no_e(gepasteuriseerd),
   ge_e(gepasteurizeerde),
   ge_no_e(gepasteurizeerd)],adv,[],[]).

a([ge_e(gepatenteerde),
   ge_no_e(gepatenteerd)],adv,[],[]).

a([ge_e(gepeddelde),
   ge_no_e(gepeddeld)],adv,[],[]).

a([ge_e(gepeilde),
   ge_no_e(gepeild)],adv,[],[]).

a([ge_e(gepeinsde),
   ge_no_e(gepeinsd)],adv,[],[]).

a([ge_e(gepelde),
   ge_no_e(gepeld)],padv,[],[]).

a([ge_e(gepenetreerde),
   ge_no_e(gepenetreerd)],adv,[],[]).

a([e(gepenseelde),
   no_e(gepenseeld)],padv,[],[]).

a([ge_e(gepensioneerde),
   ge_no_e(gepensioneerd)],adv,[],[]).

a([ge_e(gepeperde),
   ge_no_e(gepeperd)],padv,[],[]).

a([ge_e(geperfectioneerde),
   ge_no_e(geperfectioneerd)],adv,[],[]).

a([ge_e(geperfektioneerde),
   ge_no_e(geperfektioneerd)],adv,[],[]).

a([e(geperforeerde),
   no_e(geperforeerd)],padv,[],[]).

a([ge_e(gepermitteerde),
   ge_no_e(gepermitteerd)],adv,[],[]).

a([ge_e(gepersonifieerde),
   ge_no_e(gepersonifieerd)],padv,[],[]).

a([ge_e(geperste),
   ge_no_e(geperst)],adv,[],[]).

a([ge_e(gepeste),
   ge_no_e(gepest)],adv,[],[]).

a([ge_e(gepiepte),
   ge_no_e(gepiept)],adv,[],[]).

a([ge_e(gepijnigde),
   ge_no_e(gepijnigd)],adv,[],[]).

a([e(gepikeerde),
   no_e(gepikeerd)],padv,
  [pp(over),
   object_sbar,
   er_pp_sbar(over)],[]).

a([ge_e(gepikte),
   ge_no_e(gepikt)],adv,[],[]).

a([ge_e(gepinde),
   ge_no_e(gepind)],padv,[],[vast]).

a([ge_e(gepiste),
   ge_no_e(gepist)],adv,[],[]).

a([ge_e(gepitte),
   ge_no_e(gepit)],adv,[],[]).

a([ge_e(geplaagde),
   ge_no_e(geplaagd)],adv,[],[]).

a([ge_e(geplaatste),
   ge_no_e(geplaatst)],adv,
  [pp(bij),
   pp(voor)],[]).

a([ge_e(geplakte),
   ge_no_e(geplakt)],adv,[],[]).

a([ge_e(geplande),
   ge_no_e(gepland)],adv,[],[]).

a([ge_e(geplante),
   ge_no_e(geplant)],adv,[],[]).

a([ge_e(geplaste),
   ge_no_e(geplast)],adv,[],[]).

a([ge_e(geplaveide),
   ge_no_e(geplaveid)],padv,[],[]).

a([ge_e(gepleegde),
   ge_no_e(gepleegd)],adv,[],[]).

a([ge_e(gepleisterde),
   ge_no_e(gepleisterd)],adv,[],[]).

a([ge_e(gepleite),
   ge_no_e(gepleit)],adv,[],[]).

a([ge_e(geplensde),
   ge_no_e(geplensd)],adv,[],[]).

a([ge_e(geplette),
   ge_no_e(geplet)],adv,[],[]).

a([ge_e(geploegde),
   ge_no_e(geploegd)],adv,[],[]).

a([ge_e(geplofte),
   ge_no_e(geploft)],adv,[],[]).

a([ge_e(geplonsde),
   ge_no_e(geplonsd)],adv,[],[]).

a([ge_both(geplozen)],nonadv,[],[]).

a([ge_e(geplooide),
   ge_no_e(geplooid)],adv,[],[]).

a([ge_e(geplukte),
   ge_no_e(geplukt)],adv,[],[]).

a([ge_e(geplunderde),
   ge_no_e(geplunderd)],adv,[],[]).

a([ge_e(gepocheerde),
   ge_no_e(gepocheerd)],padv,[],[]).

a([ge_e(gepochte),
   ge_no_e(gepocht)],adv,[],[]).

a([ge_e(gepoederde),
   ge_no_e(gepoederd)],adv,[],[]).

a([ge_e(gepoetste),
   ge_no_e(gepoetst)],adv,[],[]).

a([ge_e(gepofte),
   ge_no_e(gepoft)],padv,[],[]).

a([e([gepokte,en,gemazelde]),
   no_e([gepokt,en,gemazeld])],padv,[],[]).

a([ge_e(gepolariseerde),
   ge_no_e(gepolariseerd),
   ge_e(gepolarizeerde),
   ge_no_e(gepolarizeerd)],adv,[],[]).

a([ge_e(gepolijste),
   ge_no_e(gepolijst)],adv,[],[]).

a([ge_e(gepolste),
   ge_no_e(gepolst)],adv,[],[]).

a([ge_e(gepompte),
   ge_no_e(gepompt)],adv,[],[]).

a([ge_e(geponeerde),
   ge_no_e(geponeerd)],adv,[],[]).

a([ge_e(geporde),
   ge_no_e(gepord)],adv,[],[]).

a([e(geporteerde),
   no_e(geporteerd)],nonadv,
  [pp(voor),
   pp(van)],[]).

a([ge_no_e(geposeerd),
   ge_e(geposeerde)],adv,[],[]).

a([ge_e(geposte),
   ge_no_e(gepost)],adv,[],[]).

a([ge_e(geposteerde),
   ge_no_e(geposteerd)],adv,[],[]).

a([ge_e(gepostuleerde),
   ge_no_e(gepostuleerd)],adv,[],[]).

a([ge_e(geprangde),
   ge_no_e(geprangd)],adv,[],[]).

a([ge_e(gepreciseerde),
   ge_no_e(gepreciseerd),
   ge_e(geprecizeerde),
   ge_no_e(geprecizeerd)],adv,[],[]).

a([ge_e(gepredikte),
   ge_no_e(gepredikt)],adv,[],[]).

a([ge_e(gepreekte),
   ge_no_e(gepreekt)],adv,[],[]).

a([ge_e(geprefereerde),
   ge_no_e(geprefereerd)],adv,[],[]).

a([ge_e(geprente),
   ge_no_e(geprent)],adv,[],[]).

a([ge_e(geprepareerde),
   ge_no_e(geprepareerd)],adv,[],[]).

a([ge_e(gepresenteerde),
   ge_no_e(gepresenteerd)],adv,[],[]).

a([ge_e(gepreste),
   ge_no_e(geprest)],adv,[],[]).

a([ge_e(gepresteerde),
   ge_no_e(gepresteerd)],adv,[],[]).

a([ge_e(gepretendeerde),
   ge_no_e(gepretendeerd)],adv,[],[]).

a([ge_e(geprevaleerde),
   ge_no_e(geprevaleerd)],adv,[],[]).

a([ge_e(geprevelde),
   ge_no_e(gepreveld)],adv,[],[]).

a([ge_both(geprezen)],adv,[],[]).

a([ge_e(geprezenteerde),
   ge_no_e(geprezenteerd)],adv,[],[]).

a([ge_e(gepriemde),
   ge_no_e(gepriemd)],adv,[],[]).

a([ge_e(geprijsde),
   ge_no_e(geprijsd)],adv,[],[]).

a([ge_e(geprikkelde),
   er(geprikkelder),
   ere(geprikkeldere),
   ge_no_e(geprikkeld),
   st(geprikkeldst),
   ste(geprikkeldste)],padv,
  [object_vp,
   object_sbar],[]).

a([ge_e(geprikte),
   ge_no_e(geprikt)],adv,[],[]).

a([ge_e(geprivatiseerde),
   ge_no_e(geprivatiseerd)],adv,[],[]).

a([ge_e(geprobeerde),
   ge_no_e(geprobeerd)],adv,[],[]).

a([ge_e(geproclameerde),
   ge_no_e(geproclameerd)],adv,[],[]).

a([ge_e(geproduceerde),
   ge_no_e(geproduceerd)],adv,[],[]).

a([ge_e(geproefde),
   ge_no_e(geproefd)],adv,[],[]).

a([ge_e(geprofeteerde),
   ge_no_e(geprofeteerd)],adv,[],[]).

a([ge_e(geprofileerde),
   ge_no_e(geprofileerd)],adv,[],[]).

a([ge_e(geprofiteerde),
   ge_no_e(geprofiteerd)],adv,[],[]).

a([ge_e(geprogrammeerde),
   ge_no_e(geprogrammeerd)],adv,[],[]).

a([ge_e(geprojecteerde),
   ge_no_e(geprojecteerd)],adv,[],[]).

a([ge_e(geprojekteerde),
   ge_no_e(geprojekteerd)],adv,[],[]).

a([ge_e(geproklameerde),
   ge_no_e(geproklameerd)],adv,[],[]).

a([ge_e(geprolongeerde),
   ge_no_e(geprolongeerd)],padv,[],[]).

a([ge_e(gepromoveerde),
   ge_no_e(gepromoveerd)],adv,
  [pp(naar),
   pp(op),
   pp(tot)],[]).

a([e(geprononceerde),
   er(geprononceerder),
   ere(geprononceerdere),
   no_e(geprononceerd),
   st(geprononceerdst),
   ste(geprononceerdste)],adv,[],[]).

a([ge_e(gepropageerde),
   ge_no_e(gepropageerd)],adv,[],[]).

a([ge_e(gepropte),
   ge_no_e(gepropt)],adv,[],[]).

a([ge_e(geprotesteerde),
   ge_no_e(geprotesteerd)],adv,[],[]).

a([ge_e(geprovoceerde),
   ge_no_e(geprovoceerd)],adv,[],[]).

a([ge_e(gepruimde),
   ge_no_e(gepruimd)],adv,[],[]).

a([ge_e(gepruttelde),
   ge_no_e(geprutteld)],adv,[],[]).

a([ge_e(gepubliceerde),
   ge_no_e(gepubliceerd)],adv,[],[]).

a([ge_e(gepunte),
   ge_no_e(gepunt)],nonadv,[],[]).

a([ge_e(gepureerde),
   ge_no_e(gepureerd)],padv,[],[]).

a([ge_e(gepushte),
   ge_no_e(gepusht)],padv,[],[]).

a([ge_e(geputte),
   ge_no_e(geput)],adv,[],[]).

a([ge_e(gequalificeerde),
   ge_no_e(gequalificeerd)],adv,[],[]).

a([ge_e(gequantificeerde),
   ge_no_e(gequantificeerd)],adv,[],[]).

a([ge_e(geraadpleegde),
   ge_no_e(geraadpleegd)],adv,[],[]).

a([ge_e(geraakte),
   er(geraakter),
   ere(geraaktere),
   ge_no_e(geraakt),
   st(geraaktst),
   ste(geraaktste)],adv,
  [pred,
   fixed([[in,de,vergetelheid]])],[]).

a([ge_e(geraamde),
   ge_no_e(geraamd)],adv,[],[]).

a([ge_e(geraapte),
   ge_no_e(geraapt)],adv,[],[]).

a([ge_both(geraden)],adv,[],[]).

a([ge_e(gerafelde),
   ge_no_e(gerafeld)],adv,[],[]).

a([ge_e(geraffineerde),
   er(geraffineerder),
   ere(geraffineerdere),
   ge_no_e(geraffineerd),
   st(geraffineerdst),
   ste(geraffineerdste)],adv,[],[]).

a([ge_e(geramde),
   ge_no_e(geramd)],adv,[],[]).

a([ge_e(gerande),
   ge_no_e(gerand)],nonadv,[],[]).

a([ge_e(gerangschikte),
   ge_no_e(gerangschikt)],adv,[],[]).

a([ge_e(geranselde),
   ge_no_e(geranseld)],adv,[],[]).

a([ge_e(gerapporteerde),
   ge_no_e(gerapporteerd)],adv,[],[]).

a([ge_e(geraspte),
   ge_no_e(geraspt)],adv,[],[]).

a([ge_e(gerationaliseerde),
   ge_no_e(gerationaliseerd),
   ge_e(gerationalizeerde),
   ge_no_e(gerationalizeerd)],adv,[],[]).

a([ge_e(gerealiseerde),
   ge_no_e(gerealiseerd),
   ge_e(gerealizeerde),
   ge_no_e(gerealizeerd)],adv,[],[]).

a([ge_e(gerechte),
   ge_no_e(gerecht)],adv,[],[]).

a([e(gerechtelijke),
   er(gerechtelijker),
   ere(gerechtelijkere),
   no_e(gerechtelijk),
   st(gerechtelijkst),
   ste(gerechtelijkste)],adv,[],[]).

a([ge_e(gerechtigde),
   er(gerechtigder),
   ere(gerechtigdere),
   ge_no_e(gerechtigd),
   st(gerechtigdst),
   ste(gerechtigdste)],padv,[object_vp],
  []).

a([ge_e(gerechtigde),
   er(gerechtigder),
   ere(gerechtigdere),
   ge_no_e(gerechtigd),
   st(gerechtigdst),
   ste(gerechtigdste)],padv,[],
  [s(bijstand),
   pensioen,
   s(pensioen),
   speel,
   stem,
   s(uitkering)]).

a([ge_e(gerechtvaardigde),
   er(gerechtvaardigder),
   ere(gerechtvaardigdere),
   ge_no_e(gerechtvaardigd),
   st(gerechtvaardigdst),
   ste(gerechtvaardigdste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(gereciteerde),
   ge_no_e(gereciteerd)],adv,[],[]).

a([ge_e(gereconstrueerde),
   ge_no_e(gereconstrueerd)],adv,[],[]).

a([ge_e(gerecruteerde),
   ge_no_e(gerecruteerd)],adv,[],[]).

a([ge_e(gerecupereerde),
   ge_no_e(gerecupereerd)],padv,[],[]).

a([ge_e(gerecyclede),
   ge_no_e(gerecycled)],nonadv,[],[]).

a([ge_e(geredde),
   ge_no_e(gered)],adv,[],[]).

a([ge_both(gereden)],adv,[],[]).

a([ge_e(geredeneerde),
   ge_no_e(geredeneerd)],adv,[],[]).

a([ge_e(geredigeerde),
   ge_no_e(geredigeerd)],adv,[],[]).

a([ge_e(geredoubleerde),
   ge_no_e(geredoubleerd)],padv,[],[]).

a([ge_e(gereduceerde),
   ge_no_e(gereduceerd)],adv,[],[]).

a([e(gerede),
   er(gereder),
   ere(geredere),
   no_e(gereed),
   st(gereedst),
   ste(gereedste)],padv,
  [object_vp,
   er_pp_vp(voor),
   pp(voor)],[]).

a([e(gereedstaande),
   no_e(gereedstaand)],nonadv,[],[]).

a([ge_both(gereedgekomen)],padv,[],[]).

a([ge_e(gereedgemaakte),
   ge_no_e(gereedgemaakt)],adv,[],[]).

a([ge_no_e(gerefereerd),
   ge_e(gerefereerde)],adv,[],[]).

a([ge_e(gereflecteerde),
   ge_no_e(gereflecteerd)],adv,[],[]).

a([ge_e(gereflekteerde),
   ge_no_e(gereflekteerd)],adv,[],[]).

a([ge_e(gereformeerde),
   ge_no_e(gereformeerd)],adv,[],[]).

a([e(geregde),
   no_e(geregd)],adv,[],[]).

a([ge_e(geregeerde),
   ge_no_e(geregeerd)],adv,[],[]).

a([ge_e(geregelde),
   er(geregelder),
   ere(geregeldere),
   ge_no_e(geregeld),
   st(geregeldst),
   ste(geregeldste)],adv,[],[]).

a([ge_both(geregen)],adv,[],[]).

a([ge_e(geregende),
   ge_no_e(geregend)],adv,[],
  [nat,
   f([nat])]).

a([ge_e(geregisseerde),
   ge_no_e(geregisseerd)],adv,[],[]).

a([ge_e(geregistreerde),
   ge_no_e(geregistreerd)],adv,[],[]).

a([ge_e(gereguleerde),
   ge_no_e(gereguleerd)],adv,[],[]).

a([ge_e(gerehabiliteerde),
   ge_no_e(gerehabiliteerd)],adv,[],[]).

a([ge_e(gereikte),
   ge_no_e(gereikt)],adv,[],[]).

a([ge_e(gereinigde),
   ge_no_e(gereinigd)],adv,[],[]).

a([ge_e(gereisde),
   ge_no_e(gereisd)],adv,[],[]).

a([ge_e(gerekende),
   ge_no_e(gerekend)],padv,[],[]).

a([ge_e(gerekonstrueerde),
   ge_no_e(gerekonstrueerd)],adv,[],[]).

a([ge_e(gerekruteerde),
   ge_no_e(gerekruteerd)],adv,[],[]).

a([ge_e(gerekte),
   ge_no_e(gerekt)],adv,[],[]).

a([ge_e(gerelateerde),
   ge_no_e(gerelateerd)],adv,[],
  [s(arbeid),
   armoede]).

a([ge_e(gerelativeerde),
   ge_no_e(gerelativeerd)],adv,[],[]).

a([ge_e(geremde),
   er(geremder),
   ere(geremdere),
   ge_no_e(geremd),
   st(geremdst),
   ste(geremdste)],adv,[],[]).

a([ge_e(geremixte),
   ge_no_e(geremixt)],nonadv,[],[]).

a([ge_e(gerende),
   ge_no_e(gerend)],adv,[],[]).

a([e(gerenommeerde),
   er(gerenommeerder),
   ere(gerenommeerdere),
   no_e(gerenommeerd),
   st(gerenommeerdst),
   ste(gerenommeerdste)],padv,[],[]).

a([ge_e(gerenoveerde),
   ge_no_e(gerenoveerd)],nonadv,[],[]).

a([ge_e(gereorganiseerde),
   ge_no_e(gereorganiseerd),
   ge_e(gereorganizeerde),
   ge_no_e(gereorganizeerd)],adv,[],[]).

a([ge_e(gerepareerde),
   ge_no_e(gerepareerd)],adv,[],[]).

a([ge_e(gerepeteerde),
   ge_no_e(gerepeteerd)],adv,[],[]).

a([ge_e(gerepliceerde),
   ge_no_e(gerepliceerd)],adv,[],[]).

a([ge_e(gerepresenteerde),
   ge_no_e(gerepresenteerd)],adv,[],[]).

a([ge_e(gereproduceerde),
   ge_no_e(gereproduceerd)],adv,[],[]).

a([ge_e(gerepte),
   ge_no_e(gerept)],adv,[],[]).

a([ge_e(gereserveerde),
   er(gereserveerder),
   ere(gereserveerdere),
   ge_no_e(gereserveerd),
   st(gereserveerdst),
   ste(gereserveerdste)],adv,
  [pp(voor)],[]).

a([ge_e(gerespecteerde),
   ge_no_e(gerespecteerd)],adv,[],[]).

a([ge_e(gerespekteerde),
   ge_no_e(gerespekteerd)],adv,[],[]).

a([ge_e(gerestaureerde),
   ge_no_e(gerestaureerd)],adv,[],[]).

a([ge_e(geresteerde),
   ge_no_e(geresteerd)],adv,[],[]).

a([ge_e(gerestylde),
   ge_no_e(gerestyld)],padv,[],[]).

a([ge_no_e(geresulteerd),
   ge_e(geresulteerde)],adv,[],[]).

a([ge_e(geresumeerde),
   ge_no_e(geresumeerd)],adv,[],[]).

a([ge_both(gereten)],adv,[],[]).

a([ge_e(geretireerde),
   ge_no_e(geretireerd)],nonadv,[],[]).

a([ge_both(gerevalideerd)],padv,[],[]).

a([ge_both(gerezen)],adv,[],[]).

a([e(geriatrische),
   no_e(geriatrisch)],nonadv,[],[]).

a([ge_e(gerichte),
   er(gerichter),
   ere(gerichtere),
   ge_no_e(gericht),
   st(gerichtst),
   ste(gerichtste)],adv,
  [er_pp_vp(op),
   er_pp_sbar(op),
   pp(op)],
  [s(bedrijf),
   s(beroep)]).

a([ge_e(geridderde),
   ge_no_e(geridderd)],nonadv,[],[]).

a([e(geriefelijke),
   er(geriefelijker),
   ere(geriefelijkere),
   no_e(geriefelijk),
   st(geriefelijkst),
   ste(geriefelijkste)],adv,[],[]).

a([e(gerieflijke),
   er(gerieflijker),
   ere(gerieflijkere),
   no_e(gerieflijk),
   st(gerieflijkst),
   ste(gerieflijkste)],adv,[],[]).

a([ge_e(gerijde),
   ge_no_e(gerijd)],adv,[],[]).

a([ge_e(gerijmde),
   ge_no_e(gerijmd)],adv,[],[]).

a([ge_e(gerijpte),
   er(gerijpter),
   ere(gerijptere),
   ge_no_e(gerijpt),
   st(gerijptst),
   ste(gerijptste)],adv,[],[]).

a([ge_e(gerilde),
   ge_no_e(gerild)],adv,[],[]).

a([ge_e(gerimpelde),
   er(gerimpelder),
   ere(gerimpeldere),
   ge_no_e(gerimpeld),
   st(gerimpeldst),
   ste(gerimpeldste)],adv,[],[]).

a([e(geringe),
   er(geringer),
   ere(geringere),
   no_e(gering),
   st(geringst),
   ste(geringste)],adv,[],[]).

a([ge_e(geripte),
   ge_no_e(geript)],padv,[],[]).

a([ge_e(geriskeerde),
   ge_no_e(geriskeerd)],adv,[],[]).

a([ge_e(gerisqueerde),
   ge_no_e(gerisqueerd)],adv,[],[]).

a([ge_e(geritselde),
   ge_no_e(geritseld)],adv,[],[]).

a([ge_e(geritste),
   ge_no_e(geritst)],adv,[],[]).

a([ge_e(gerodeerde),
   ge_no_e(gerodeerd)],padv,[],[]).

a([ge_e(geroeide),
   ge_no_e(geroeid)],adv,[],[]).

a([ge_e(geroemde),
   ge_no_e(geroemd)],adv,[],[]).

a([ge_both(geroepen)],adv,
  [fixed([[te,hulp]])],[]).

a([ge_e(geroerde),
   ge_no_e(geroerd)],adv,[],[]).

a([ge_e(geroeste),
   ge_no_e(geroest)],adv,[],[]).

a([ge_e(geroffelde),
   ge_no_e(geroffeld)],adv,[],[]).

a([ge_both(geroken)],adv,[],[]).

a([ge_e(gerolde),
   ge_no_e(gerold)],adv,[],[]).

a([ge_e(geromantiseerde),
   ge_no_e(geromantiseerd)],nonadv,[],[]).

a([ge_e(geronde),
   ge_no_e(gerond)],adv,[],[]).

a([both(geronnen)],nonadv,[],[]).

a([ge_e(geronselde),
   ge_no_e(geronseld)],adv,[],[]).

a([ge_e(geroofde),
   ge_no_e(geroofd)],adv,[],[]).

a([ge_e(gerooide),
   ge_no_e(gerooid)],adv,[],[]).

a([ge_e(gerookte),
   ge_no_e(gerookt)],adv,[],[]).

a([ge_e(geroosterde),
   ge_no_e(geroosterd)],adv,[],[]).

a([ge_e(gerotte),
   ge_no_e(gerot)],adv,[],[]).

a([ge_no_e(geroteerd),
   ge_e(geroteerde)],adv,[],[]).

a([ge_no_e(geroyeerd),
   ge_e(geroyeerde)],padv,[],[]).

a([e(geroutineerde),
   er(geroutineerder),
   ere(geroutineerdere),
   no_e(geroutineerd),
   st(geroutineerdst),
   ste(geroutineerdste)],adv,[],[]).

a([e(geruchtmakende),
   er(geruchtmakender),
   ere(geruchtmakendere),
   no_e(geruchtmakend),
   st(geruchtmakendst),
   ste(geruchtmakendste)],nonadv,[],[]).

a([ge_e(geruggesteunde),
   ge_no_e(geruggesteund)],padv,[],[]).

a([ge_e(geruilde),
   ge_no_e(geruild)],adv,[],[]).

a([e(geruime),
   er(geruimer),
   ere(geruimere),
   no_e(geruim),
   st(geruimst),
   ste(geruimste)],nonadv,[],[]).

a([ge_e(geruimde),
   ge_no_e(geruimd)],adv,[],[]).

a([e(geruisloze),
   er(geruislozer),
   ere(geruislozere),
   no_e(geruisloos),
   st(geruisloost),
   ste(geruislooste)],adv,[],[]).

a([ge_e(geruite),
   ge_no_e(geruit)],adv,[],[]).

a([ge_e(gerukte),
   ge_no_e(gerukt)],adv,[],[]).

a([ge_e(gerunde),
   ge_no_e(gerund)],adv,[],[]).

a([ge_e(geruste),
   er(geruster),
   ere(gerustere),
   ge_no_e(gerust)],adv,
  [object_sbar,                 % de Europese burgers kunnen gerust zijn dat ...
   pp(op),
   er_pp_sbar(op),
   pp(in),                      % Vlaams
   er_pp_sbar(in)               % Vlaams
  ],[]).

a([ge_e(gerustgestelde),
   ge_no_e(gerustgesteld)],adv,[],[]).

a([e(geruststellende),
   er(geruststellender),
   ere(geruststellendere),
   no_e(geruststellend),
   st(geruststellendst),
   ste(geruststellendste)],adv,
  [pp(voor),
   subject_sbar],[]).

a([ge_e(geruïneerde),
   ge_no_e(geruïneerd)],adv,[],[]).

a([ge_e(gesaboteerde),
   ge_no_e(gesaboteerd)],adv,[],[]).

a([ge_e(gesanctioneerde),
   ge_no_e(gesanctioneerd)],adv,[],[]).

a([ge_e(gesarde),
   ge_no_e(gesard)],adv,[],[]).

a([ge_e(gescande),
   ge_no_e(gescand)],padv,[],[]).

a([ge_e(gescandeerde),
   ge_no_e(gescandeerd)],adv,[],[]).

a([ge_e(geschade),
   ge_no_e(geschaad)],adv,[],[]).

a([ge_e(geschaafde),
   ge_no_e(geschaafd)],adv,[],[]).

a([ge_e(geschaakte),
   ge_no_e(geschaakt)],adv,[],[]).

a([ge_e(geschaamde),
   ge_no_e(geschaamd)],adv,[],[]).

a([ge_e(geschaarde),
   ge_no_e(geschaard)],adv,[],[]).

a([ge_no_e(geschaatst),
   ge_e(geschaatste)],adv,[],[]).

a([ge_e(geschaduwde),
   ge_no_e(geschaduwd)],adv,[],[]).

a([ge_e(geschafte),
   ge_no_e(geschaft)],adv,[],[]).

a([ge_e(geschakeerde),
   ge_no_e(geschakeerd)],padv,[],[]).

a([ge_e(geschakelde),
   ge_no_e(geschakeld)],adv,[],[]).

a([ge_both(geschapen)],adv,
  [object_vp,
   er_pp_vp(voor),
   pp(voor)],[]).

a([ge_e(gescharrelde),
   ge_no_e(gescharreld)],adv,[],[]).

a([ge_e(geschatte),
   ge_no_e(geschat)],adv,[],[]).

a([ge_e(gescheerde),
   ge_no_e(gescheerd)],adv,[],[]).

a([ge_both(gescheiden)],adv,
  [pp(door),
   pp(van)],[]).

a([ge_e(geschepte),
   ge_no_e(geschept)],adv,[],[]).

a([ge_e(gescherpte),
   ge_no_e(gescherpt)],adv,[],[]).

a([ge_e(geschetste),
   ge_no_e(geschetst)],adv,[],[]).

a([ge_e(gescheurde),
   ge_no_e(gescheurd)],adv,[],[]).

a([ge_e(geschiede),
   ge_no_e(geschied)],adv,[],[]).

a([ge_e(geschifte),
   ge_no_e(geschift),
   er(geschifter),
   ere(geschiftere),
   st(geschiftst),
   ste(geschiftste)],adv,[],[]).

a([ge_e(geschikte),
   er(geschikter),
   ere(geschiktere),
   ge_no_e(geschikt),
   st(geschiktst),
   ste(geschiktste)],adv,
  [object_vp,
   subject_vp,
   er_pp_vp(voor),
   pp(voor)],[]).

a([ge_e(geschilde),
   ge_no_e(geschild)],adv,[],[]).

a([ge_e(geschilderde),
   ge_no_e(geschilderd)],adv,[],[]).

a([ge_e(geschminkte),
   ge_no_e(geschminkt)],adv,[],[]).

a([ge_e(geschoeide),
   ge_no_e(geschoeid)],adv,[],[]).

a([ge_e(geschoffeerde),
   ge_no_e(geschoffeerd)],nonadv,[],[]).

a([ge_both(geschoven)],nonadv,
  [pp(tegen)],[]).

a([ge_e(geschokte),
   ge_no_e(geschokt)],both,
  [object_vp,
   object_sbar],[]).

a([ge_both(gescholden)],adv,[],[]).

a([ge_both(gescholen)],adv,[],[]).

a([ge_both(geschonden)],adv,[],[]).

a([ge_both(geschonken)],adv,[],[]).

a([ge_e(geschoolde),
   er(geschoolder),
   ere(geschooldere),
   ge_no_e(geschoold),
   st(geschooldst),
   ste(geschooldste)],adv,[],[]).

a([ge_e(geschoonde),
   ge_no_e(geschoond)],padv,
  [pp(van)],[]).

a([ge_e(geschopte),
   ge_no_e(geschopt)],adv,[],[]).

a([ge_both(geschoren)],adv,[],[]).

a([ge_e(geschorste),
   ge_no_e(geschorst)],adv,[],[]).

a([ge_e(geschorte),
   ge_no_e(geschort)],adv,[],[]).

a([ge_both(geschoten)],adv,[],[]).

a([ge_e(geschouwde),
   ge_no_e(geschouwd)],adv,[],[]).

a([ge_e(geschraagde),
   ge_no_e(geschraagd)],adv,[],[]).

a([ge_e(geschraapte),
   ge_no_e(geschraapt)],adv,[],[]).

a([ge_e(geschrankte),
   ge_no_e(geschrankt)],nonadv,[],[]).

a([ge_e(geschrapte),
   ge_no_e(geschrapt)],adv,[],[]).

a([ge_e(geschreeuwde),
   ge_no_e(geschreeuwd)],adv,[],[]).

a([ge_e(geschreide),
   ge_no_e(geschreid)],adv,[],[]).

a([ge_both(geschreven)],adv,[fixed([[op,het,lijf],dat])],[]).

a([ge_both(geschreven)],adv,[],[hand]).

a([ge_e(geschrobde),
   ge_no_e(geschrobd)],adv,[],[]).

a([ge_e(geschroefde),
   ge_no_e(geschroefd)],adv,[],[]).

a([ge_e(geschroeide),
   ge_no_e(geschroeid)],adv,[],[]).

a([ge_both(geschrokken)],padv,[],[]).

a([ge_e(geschrokte),
   ge_no_e(geschrokt)],adv,[],[]).

a([ge_e(geschrompelde),
   ge_no_e(geschrompeld)],adv,[],[]).

a([ge_e(geschroomde),
   ge_no_e(geschroomd)],adv,[],[]).

a([ge_e(geschudde),
   ge_no_e(geschud)],adv,[],[]).

a([ge_e(geschuifelde),
   ge_no_e(geschuifeld)],adv,[],[]).

a([no_e(geschuimbekt),
   e(geschuimbekte)],padv,[],[]).

a([ge_e(geschuimde),
   ge_no_e(geschuimd)],adv,[],[]).

a([ge_e(geschuurde),
   ge_no_e(geschuurd)],adv,[],[]).

a([ge_e(geschuwde),
   ge_no_e(geschuwd)],adv,[],[]).

a([ge_e(gescoorde),
   ge_no_e(gescoord)],adv,[],[]).

a([ge_e(geseculariseerde),
   ge_no_e(geseculariseerd)],padv,[],[]).

a([ge_e(geseinde),
   ge_no_e(geseind)],adv,[],[]).

a([ge_e(geselecteerde),
   ge_no_e(geselecteerd)],adv,[],[]).

a([ge_e(geselekteerde),
   ge_no_e(geselekteerd)],adv,[],[]).

a([ge_e(gesegmenteerde),
   ge_no_e(gesegmenteerd)],adv,[],[]).

a([ge_e(geserveerde),
   ge_no_e(geserveerd)],adv,[],[]).

a([ge_e(gesettelde),
   ge_no_e(gesetteld)],padv,[],[]).

a([ge_e(geshockeerde),
   ge_no_e(geshockeerd)],adv,
  [subject_sbar],[]).

a([ge_e(gesidderde),
   ge_no_e(gesidderd)],adv,[],[]).

a([ge_e(gesierde),
   ge_no_e(gesierd)],adv,[],[]).

a([ge_e(gesignaleerde),
   ge_no_e(gesignaleerd)],adv,[],[]).

%% een schilderij, gesigneerd Jacob de Vries, ..
a([ge_e(gesigneerde),
   ge_no_e(gesigneerd)],adv,[transitive],[]).

a([ge_e(gesijpelde),
   ge_no_e(gesijpeld)],adv,[],[]).

a([ge_e(gesimuleerde),
   ge_no_e(gesimuleerd)],adv,[],[]).

a([ge_e(gesiste),
   ge_no_e(gesist)],adv,[],[]).

a([ge_e(gesitueerde),
   ge_no_e(gesitueerd)],adv,[],[]).

a([e(gesjeesde),
   no_e(gesjeesd)],padv,[],[]).

a([ge_e(gesjokte),
   ge_no_e(gesjokt)],adv,[],[]).

a([ge_e(gesjorde),
   ge_no_e(gesjord)],adv,[],[]).

a([ge_e(gesjouwde),
   ge_no_e(gesjouwd)],adv,[],[]).

a([ge_e(geskandeerde),
   ge_no_e(geskandeerd)],adv,[],[]).

a([ge_e(geskiede),
   ge_no_e(geskied)],adv,[],[]).

a([ge_e(geslaagde),
   ge_no_e(geslaagd),
   ere(geslaagdere),
   er(geslaagder)],padv,
  [pp(voor)],[]).

a([ge_e(geslaakte),
   ge_no_e(geslaakt)],adv,[],[]).

a([ge_e(geslachte),
   ge_no_e(geslacht)],adv,[],[]).

a([e(geslachtelijke),
   no_e(geslachtelijk)],adv,[],[]).  % adv: geslachtelijk voortplanten

a([e(geslachtsrijpe),
   no_e(geslachtsrijp)],nonadv,[],[]).

a([ge_both(geslagen)],both,
  [fixed([[op,hol]]),
   fixed([svp_pp(op,tilt)]),
   fixed([[uit,het,veld]]),
   fixed([[uit,het,veld],mod_pp(door)]),
   pred],[]).

a([ge_e(geslechte),
   ge_no_e(geslecht)],adv,[],[]).

a([ge_e(gesleepte),
   ge_no_e(gesleept)],adv,[],[]).

a([ge_e(geslenterde),
   ge_no_e(geslenterd)],adv,[],[]).

a([ge_both(geslepen),
   er(geslepener),
   ere(geslepenere),
   st(geslepenst),
   ste(geslepenste)],adv,[],[]).

a([ge_both(gesleten)],adv,[],[]).

a([ge_e(gesleurde),
   ge_no_e(gesleurd)],adv,[],[]).

a([ge_e(geslikte),
   ge_no_e(geslikt)],adv,[],[]).

a([ge_e(geslingerde),
   ge_no_e(geslingerd)],adv,[],[]).

a([ge_e(geslipte),
   ge_no_e(geslipt)],adv,[],[]).

a([ge_e(geslobberde),
   ge_no_e(geslobberd)],adv,[],[]).

a([ge_e(geslofte),
   ge_no_e(gesloft)],adv,[],[]).

a([ge_e(geslokte),
   ge_no_e(geslokt)],adv,[],[]).

a([ge_both(geslonken)],adv,[],[]).

a([ge_e(gesloopte),
   ge_no_e(gesloopt)],adv,[],[]).

a([ge_both(geslopen)],adv,[],[]).

a([ge_both(gesloten),
   er(geslotener),
   ere(geslotenere),
   st(geslotenst),
   ste(geslotenste)],adv,
  [pp(voor)],[]).

a([e(gesluierde),
   no_e(gesluierd)],padv,[],[]).

a([ge_e(geslurpte),
   ge_no_e(geslurpt)],adv,[],[]).

a([ge_e(gesmaakte),
   ge_no_e(gesmaakt)],adv,[],[]).

a([e(gesmade),
   no_e(gesmaad)],padv,[],[]).

a([ge_e(gesmakte),
   ge_no_e(gesmakt)],adv,[],[]).

a([ge_e(gesmede),
   ge_no_e(gesmeed)],adv,[],[]).

a([ge_e(gesmeekte),
   ge_no_e(gesmeekt)],adv,[],[]).

a([ge_e(gesmeerde),
   er(gesmeerder),
   ere(gesmeerdere),
   ge_no_e(gesmeerd),
   st(gesmeerdst),
   ste(gesmeerdste)],adv,[],[]).

a([ge_both(gesmeten)],adv,[nonp_pred],[]).

a([ge_e(gesmokkelde),
   ge_no_e(gesmokkeld)],adv,[],[]).

a([ge_both(gesmolten)],adv,[],[]).

a([ge_e(gesmoorde),
   ge_no_e(gesmoord)],adv,[],[]).

a([ge_e(gesnapte),
   ge_no_e(gesnapt)],adv,[],[]).

a([ge_e(gesnauwde),
   ge_no_e(gesnauwd)],adv,[],[]).

a([ge_both(gesneden)],padv,
  [nonp_pred,
   fixed([svp_pp(uit,hout)])],[]).

a([ge_both(gesneden)],padv,
  [],[open]).

a([ge_e(gesneefde),
   ge_no_e(gesneefd)],nonadv,[],[]).

a([ge_e(gesnelde),
   ge_no_e(gesneld)],adv,
  [fixed([[te,hulp]]),
   fixed([[te,hulp],dat])],[]).

a([ge_e(gesnerpte),
   ge_no_e(gesnerpt)],adv,[],[]).

a([ge_e(gesneuvelde),
   ge_no_e(gesneuveld)],adv,[],[]).

a([ge_e(gesnipperde),
   ge_no_e(gesnipperd)],padv,[],[]).

a([ge_e(gesnoeide),
   ge_no_e(gesnoeid)],adv,[],[]).

a([ge_e(gesnoepte),
   ge_no_e(gesnoept)],adv,[],[]).

a([ge_e(gesnoerde),
   ge_no_e(gesnoerd)],adv,[],[]).

a([ge_e(gesnapte),
   ge_no_e(gesnopen)],adv,[],[]).

a([ge_e(gesnorde),
   ge_no_e(gesnord)],adv,[],[]).

a([ge_both(gesnoten)],adv,[],[]).

a([ge_both(gesnoven)],adv,[],[]).

a([ge_e(gesocialiseerde),
   ge_no_e(gesocialiseerd),
   ge_e(gesocializeerde),
   ge_no_e(gesocializeerd)],adv,[],[]).

a([ge_e(gesodemieterde),
   ge_no_e(gesodemieterd)],adv,[],[]).

a([ge_e(gesoigneerde),
   ge_no_e(gesoigneerd)],padv,[],[]).

a([ge_e(gesolde),
   ge_no_e(gesold)],adv,[],[]).

a([ge_e(gesopte),
   ge_no_e(gesopt)],adv,[],[]).

a([ge_e(gesorteerde),
   ge_no_e(gesorteerd)],adv,[],[]).

a([ge_e(gespaarde),
   ge_no_e(gespaard)],adv,[],[]).

a([ge_both(gespannen),
   er(gespannener),
   ere(gespannenere),
   st(gespannenst),
   ste(gespannenste)],adv,[],[]).

a([ge_e(gespatte),
   ge_no_e(gespat)],adv,[],[]).

a([ge_e(gespecialiseerde),
   er(gespecialiseerder),
   ere(gespecialiseerdere),
   ge_no_e(gespecialiseerd),
   st(gespecialiseerdst),
   ste(gespecialiseerdste)],adv,
  [pp(in)],[]).

a([ge_e(gespecificeerde),
   ge_no_e(gespecificeerd)],adv,[],[]).

a([ge_e(gespeelde),
   ge_no_e(gespeeld)],adv,[],[]).

a([ge_e(gespeende),
   ge_no_e(gespeend)],adv,
  [pp(van)],[]).

a([ge_e(gespelde),
   ge_no_e(gespeld)],adv,[],[]).

a([ge_e(gespendeerde),
   ge_no_e(gespendeerd)],adv,[],[]).

a([ge_e(gesperde),
   ge_no_e(gesperd)],adv,[],[]).

a([ge_e(gespeurde),
   ge_no_e(gespeurd)],adv,[],[]).

a([ge_e(gespiegelde),
   ge_no_e(gespiegeld)],adv,[],[]).

a([e(gespierde),
   er(gespierder),
   ere(gespierdere),
   no_e(gespierd),
   st(gespierdst),
   ste(gespierdste)],padv,[],[]).

a([ge_e(gespietste),
   ge_no_e(gespietst)],adv,[],[]).

a([ge_e(gespijkerde),
   ge_no_e(gespijkerd)],adv,[],[]).

a([ge_e(gespikkelde),
   ge_no_e(gespikkeld)],nonadv,[],[]).

a([ge_e(gespitte),
   ge_no_e(gespit)],adv,[],[]).

a([ge_e(gespitste),
   ge_no_e(gespitst)],adv,
  [pp(op),
   er_pp_sbar(op),
   er_pp_vp(op)],[]).

a([ge_both(gespleten),
   er(gespletener),
   ere(gespletenere),
   st(gespletenst),
   ste(gespletenste)],adv,[],[]).

a([ge_e(gesplitste),
   ge_no_e(gesplitst)],adv,[],[]).

a([ge_e(gespoede),
   ge_no_e(gespoed)],adv,[],[]).

a([ge_e(gespoelde),
   ge_no_e(gespoeld)],adv,[],[]).

a([ge_both(gespogen)],adv,[],[]).

a([ge_both(gesponnen)],adv,[],[]).

a([ge_e(gesponsorde),
   ge_no_e(gesponsord)],adv,[],[]).

a([ge_e(gespoorde),
   ge_no_e(gespoord)],adv,[],[]).

a([ge_e(gespotte),
   ge_no_e(gespot)],adv,[],[]).

a([ge_both(gespoten)],adv,[],[]).

a([ge_e(gespreide),
   ge_no_e(gespreid)],adv,[],[]).

a([ge_e(gesprenkelde),
   ge_no_e(gesprenkeld)],adv,[],[]).

a([ge_e(gesproeide),
   ge_no_e(gesproeid)],adv,[],[]).

a([ge_both(gesproken)],adv,
  [fixed([[te,na],dat])
  ],[]).

a([ge_no_e(gesprokkeld),
   ge_e(gesprokkelde)],adv,[],[]).

a([ge_both(gesprongen)],adv,[fixed([svp_pp(op,tilt)])],[]).

a([ge_both(gesproten)],adv,[],[]).

a([ge_e(gespuide),
   ge_no_e(gespuid)],adv,[],[]).

a([ge_e(gespuwde),
   ge_no_e(gespuwd)],adv,[],[]).

a([ge_e(gestaafde),
   ge_no_e(gestaafd)],adv,[],[]).

a([ge_e(gestaalde),
   ge_no_e(gestaald)],adv,[],[]).

a([e(gestage),
   no_e(gestaag)],adv,[],[]).

a([ge_e(gestaakte),
   ge_no_e(gestaakt)],adv,[],[]).

a([ge_e(gestane),
   ge_no_e(gestaan)],adv,[],[]).

a([ge_e(gestabiliseerde),
   ge_no_e(gestabiliseerd),
   ge_e(gestabilizeerde),
   ge_no_e(gestabilizeerd)],adv,[],[]).

a([e(gestadige),
   no_e(gestadig)],adv,[],[]).

a([ge_e(gestalde),
   ge_no_e(gestald)],adv,[],[]).

a([ge_e(gestamde),
   ge_no_e(gestamd)],adv,[],[]).

a([ge_e(gestamelde),
   ge_no_e(gestameld)],adv,[],[]).

a([ge_e(gestampte),
   ge_no_e(gestampt)],adv,[],[]).

a([ge_e(gestandaardiseerde),
   ge_no_e(gestandaardiseerd),
   ge_e(gestandaardizeerde),
   ge_no_e(gestandaardizeerd)],adv,[],[]).

a([ge_e(gestapelde),
   ge_no_e(gestapeld)],adv,[],[]).

a([ge_e(gestapte),
   ge_no_e(gestapt)],adv,[],[]).

a([ge_e(gestarte),
   ge_no_e(gestart)],padv,[],[]).

a([ge_e(gestationeerde),
   ge_no_e(gestationeerd)],adv,[],[]).

a([ge_e(gesteelde),
   ge_no_e(gesteeld)],nonadv,[],[on]).

a([ge_both(gestegen)],adv,[],[]).

a([ge_e(gestekte),
   ge_no_e(gestekt)],adv,[],[]).

a([ge_e(gestelde),
   ge_no_e(gesteld)],adv,
  [so_np,  % de mij gestelde vragen
   pp(in),
   pp(op),
   object_sbar,
   pp(met),
   pred],[]).

a([ge_e(gestelde),
   ge_no_e(gesteld)],adv,
  [],[tewerk]).

a([ge_e(gestelpte),
   ge_no_e(gestelpt)],adv,[],[]).

a([ge_e(gestemde),
   ge_no_e(gestemd)],adv,[],[]).

a([ge_e(gestempelde),
   ge_no_e(gestempeld)],adv,[],[]).

a([e(gestencilde),
   no_e(gestencild)],nonadv,[],[]).

a([ge_e(gestenigde),
   ge_no_e(gestenigd)],nonadv,[],[]).

a([ge_e(gesteriliseerde),
   ge_no_e(gesteriliseerd),
   ge_e(gesterilizeerde),
   ge_no_e(gesterilizeerd)],adv,[],[]).

a([ge_e(gesterkte),
   ge_no_e(gesterkt)],adv,[],[]).

a([ge_e(gesteunde),
   ge_no_e(gesteund)],adv,[],[]).

a([ge_both(gesteven)],adv,[],[]).

a([ge_e(gestevende),
   ge_no_e(gestevend)],adv,[],[]).

a([ge_e(gestichte),
   ge_no_e(gesticht)],adv,[],[]).

a([ge_e(gestifte),
   ge_no_e(gestift)],padv,[transitive],[]).

a([ge_e(gestikte),
   ge_no_e(gestikt)],adv,[],[]).

a([ge_e(gestilde),
   ge_no_e(gestild)],adv,[],[]).

a([ge_e(gestileerde),
   er(gestileerder),
   ere(gestileerdere),
   ge_no_e(gestileerd),
   st(gestileerdst),
   ste(gestileerdste)],adv,[],[]).

a([ge_e(gestimuleerde),
   ge_no_e(gestimuleerd)],adv,[],[]).

a([ge_e(gestippelde),
   ge_no_e(gestippeld)],adv,[],[]).

a([ge_no_e(gestoeld),
   ge_e(gestoelde)],adv,[],[]).

a([ge_both(gestoken)],adv,[],[]).

a([ge_e(gestokte),
   ge_no_e(gestokt)],adv,[],[]).

a([ge_e(gestolde),
   ge_no_e(gestold)],adv,[],[]).

a([ge_both(gestolen)],adv,[],[]).

a([ge_e(gestompte),
   ge_no_e(gestompt)],adv,[],[]).

a([ge_e(gestoofde),
   ge_no_e(gestoofd)],adv,[],[]).

a([ge_e(gestookte),
   ge_no_e(gestookt)],adv,[],[]).

a([ge_e(gestoomde),
   ge_no_e(gestoomd)],adv,[],[]).

a([ge_e(gestoorde),
   er(gestoorder),
   ere(gestoordere),
   ge_no_e(gestoord),
   st(gestoordst),
   ste(gestoordste)],adv,[],[]).

a([ge_e(gestopte),
   ge_no_e(gestopt)],adv,[],[]).

a([ge_e(gestormde),
   ge_no_e(gestormd)],adv,[],[]).

a([ge_e(gestorte),
   ge_no_e(gestort)],adv,[],[]).

a([ge_both(gestorven)],adv,[],[]).

a([ge_both(gestoten)],adv,[],[]).

a([ge_e(gestotterde),
   ge_no_e(gestotterd)],adv,[],[]).

a([ge_both(gestoven)],adv,[],[]).

a([ge_e(gestraalde),
   ge_no_e(gestraald)],adv,[],[]).

a([ge_e(gestrafte),
   ge_no_e(gestraft)],adv,[],[]).

a([ge_e(gestrande),
   ge_no_e(gestrand)],adv,[],[]).

a([ge_both(gestreden)],adv,[],[]).

a([ge_e(gestreelde),
   ge_no_e(gestreeld)],adv,[],[]).

a([ge_e(gestreepte),
   ge_no_e(gestreept)],adv,[],[]).

a([ge_both(gestreken)],adv,[],[]).

a([ge_e(gestrekte),
   ge_no_e(gestrekt)],adv,[],[]).

a([ge_e(gestremde),
   ge_no_e(gestremd)],adv,[],[]).

a([ge_e(gestrengelde),
   ge_no_e(gestrengeld)],adv,[],[]).

a([ge_e(gestresste),
   ge_no_e(gestresst)],padv,[],[]).

a([ge_e(gestriemde),
   ge_no_e(gestriemd)],adv,[],[]).

a([ge_e(gestrikte),
   ge_no_e(gestrikt)],adv,[],[]).

a([ge_e(gestripte),
   ge_no_e(gestript)],adv,[],[]).

a([ge_e(gestrompelde),
   ge_no_e(gestrompeld)],adv,[],[]).

a([ge_e(gestrooide),
   ge_no_e(gestrooid)],adv,[],[]).

a([ge_e(gestroomde),
   ge_no_e(gestroomd)],adv,[],[]).

a([ge_e(gestroomlijnde),
   ge_no_e(gestroomlijnd)],adv,[],[]).

a([ge_e(gestroopte),
   ge_no_e(gestroopt)],adv,[],[]).

a([ge_e(gestructureerde),
   ge_e(gestruktureerde),
   ge_no_e(gestructureerd),
   ge_no_e(gestruktureerd)],padv,[],[]).

a([ge_e(gestruikelde),
   ge_no_e(gestruikeld)],adv,[],[]).

a([ge_e(gestruktureerde),
   ge_no_e(gestruktureerd)],adv,[],[]).

a([ge_e(gestudeerde),
   ge_no_e(gestudeerd)],adv,[],[]).

a([ge_e(gestuite),
   ge_no_e(gestuit)],adv,[],[]).

a([ge_e(gestuiterde),
   ge_no_e(gestuiterd)],adv,[],[]).

a([ge_e(gestulpte),
   ge_no_e(gestulpt)],adv,[],[]).

a([ge_e(gestutte),
   ge_no_e(gestut)],padv,[],[]).

a([ge_e(gestuurde),
   ge_no_e(gestuurd)],adv,[],[]).

a([ge_e(gestuwde),
   ge_no_e(gestuwd)],adv,[],[]).

a([ge_e(gesublimeerde),
   ge_no_e(gesublimeerd)],adv,[],[]).

a([ge_e(gesubsidieerde),
   ge_no_e(gesubsidieerd)],adv,[],[on]).

a([ge_no_e(gesudderd),
   ge_e(gesudderde)],adv,[],[]).

a([ge_e(gesuggereerde),
   ge_no_e(gesuggereerd)],adv,[],[]).

a([ge_e(gesuikerde),
   ge_no_e(gesuikerd)],padv,[],[]).

a([ge_e(gesukkelde),
   ge_no_e(gesukkeld)],adv,[],[]).

a([ge_e(gesuste),
   ge_no_e(gesust)],adv,[],[]).

a([ge_e(gesymboliseerde),
   ge_no_e(gesymboliseerd),
   ge_e(gesymbolizeerde),
   ge_no_e(gesymbolizeerd)],adv,[],[]).

a([ge_e(gesynchroniseerde),
   ge_no_e(gesynchroniseerd)],adv,[],[]).

a([ge_e(gesystematiseerde),
   ge_no_e(gesystematiseerd),
   ge_e(gesystematizeerde),
   ge_no_e(gesystematizeerd)],adv,[],[]).

a([ge_e(getaande),
   ge_no_e(getaand)],adv,[],[]).

a([e(getalenteerde),
   no_e(getalenteerd)],both,[],[]).

a([e(getande),
   no_e(getand)],padv,[],[]).

a([ge_e(getankte),
   ge_no_e(getankt)],adv,[],[]).

a([ge_e(getapete),
   ge_no_e(getapet)],adv,[],[vast]).

a([ge_e(getapte),
   ge_no_e(getapt)],adv,[],[]).

a([ge_e(getarte),
   ge_no_e(getart)],adv,[],[]).

a([ge_e(getaste),
   ge_no_e(getast)],adv,[],[]).

a([ge_e(getatoeëerde),
   ge_no_e(getatoeëerd)],padv,[],[]).

a([ge_e(getaxeerde),
   ge_no_e(getaxeerd)],adv,[],[]).

a([ge_e(geteelde),
   ge_no_e(geteeld)],adv,[],[]).

a([ge_e(geteerde),
   ge_no_e(geteerd)],adv,[],[]).

a([ge_e(geteisterde),
   ge_no_e(geteisterd)],adv,[],[]).

a([ge_e(getekende),
   ge_no_e(getekend)],padv,
  [pp(door)],[]).

a([ge_e(getelde),
   ge_no_e(geteld)],adv,[],[]).

a([ge_e(getelefoneerde),
   ge_no_e(getelefoneerd)],adv,[],[]).

a([ge_e(getemde),
   ge_no_e(getemd)],adv,[],[]).

a([ge_e(getemperde),
   ge_no_e(getemperd)],adv,[],[]).

a([ge_e(getergde),
   ge_no_e(getergd)],adv,[],[]).

a([ge_e(geterroriseerde),
   ge_no_e(geterroriseerd),
   ge_e(geterrorizeerde),
   ge_no_e(geterrorizeerd)],adv,[],[]).

a([ge_e(geteste),
   ge_no_e(getest)],adv,[],[]).

a([ge_e(getikte),
   ge_no_e(getikt)],adv,[],[]).

a([ge_e(getilde),
   ge_no_e(getild)],adv,[],[]).

a([ge_e(getimede),
   ge_no_e(getimed)],adv,[],[]).

a([ge_e(getimmerde),
   ge_no_e(getimmerd)],adv,[],[]).

a([ge_e(getinte),
   ge_no_e(getint)],adv,[ap_pred],[]).

a([ge_e(getipte),
   ge_no_e(getipt)],adv,[],[]).

a([ge_e(getitelde),
   ge_no_e(getiteld)],adv,
  [transitive],[]).

%% "getoast witbrood"
a([ge_e(getoaste),
   ge_no_e(getoast)],adv,[],[]).

a([ge_e(getoeterde),
   ge_no_e(getoeterd)],adv,[],[]).

a([ge_e(getoetste),
   ge_no_e(getoetst)],adv,[],[]).

a([ge_both(getogen)],adv,
  [pp(in)],[]).

a([ge_e(getolde),
   ge_no_e(getold)],adv,[],[]).

a([ge_e(getolereerde),
   ge_no_e(getolereerd)],adv,[],[]).

a([ge_e(getooide),
   ge_no_e(getooid)],adv,[],[]).

a([ge_e(getoonde),
   ge_no_e(getoond)],adv,[so_np,
			  so_pp(aan)],[]).

a([e(getoonzette),
   no_e(getoonzet)],nonadv,[],[]).

a([e(getopte),
   no_e(getopt)],adv,[],[]).

a([ge_e(getornde),
   ge_no_e(getornd)],adv,[],[]).

a([ge_e(getorpedeerde),
   ge_no_e(getorpedeerd)],adv,[],[]).

a([ge_e(getorste),
   ge_no_e(getorst)],adv,[],[]).

a([ge_e(getoverde),
   ge_no_e(getoverd)],adv,[],[]).

a([ge_no_e(getraand),
   ge_e(getraande)],adv,[],[]).

a([ge_e(getrachte),
   ge_no_e(getracht)],adv,[],[]).

a([ge_e(getracteerde),
   ge_no_e(getracteerd)],adv,[],[]).

a([ge_e(getrainde),
   er(getrainder),
   ere(getraindere),
   ge_no_e(getraind),
   st(getraindst),
   ste(getraindste)],adv,
  [pp(door)],[]).

a([ge_e(getrakteerde),
   ge_no_e(getrakteerd)],adv,[],[]).

a([ge_e(getranscendeerde),
   ge_no_e(getranscendeerd)],adv,[],[]).

a([ge_e(getransformeerde),
   ge_no_e(getransformeerd)],adv,[],[]).

a([ge_e(getrapte),
   ge_no_e(getrapt)],adv,[],[]).

a([e(getraumatiseerde),
   no_e(getraumatiseerd)],adv,[],[]).

a([ge_both(getreden)],adv,[pp_pred(in,werking),
                           pp_pred(in,dienst)],[]).

a([ge_e(getreiterde),
   ge_no_e(getreiterd)],adv,[],[]).

a([ge_e(getrimde),
   ge_no_e(getrimd)],padv,[],[]).

a([ge_e(getrippelde),
   ge_no_e(getrippeld)],adv,[],[]).

a([ge_e(getroefde),
   ge_no_e(getroefd)],adv,[],[]).

a([ge_both(getroffen)],adv,
  [pp(door)],[]).

a([ge_both(getrokken)],adv,
  [pp(door),
   pp(uit)],[]).

a([ge_e(getrommelde),
   ge_no_e(getrommeld)],adv,[],[]).

a([ge_e(getroonde),
   ge_no_e(getroond)],adv,[],[]).

a([ge_e(getrooste),
   er(getrooster),
   ere(getroostere),
   ge_no_e(getroost)],adv,
  [pp(door)],[]).

a([ge_e(getrotseerde),
   ge_no_e(getrotseerd)],adv,[],[]).

a([e(getrouwe),
   er(getrouwer),
   ere(getrouwere),
   no_e(getrouw),
   st(getrouwst),
   ste(getrouwste)],both,[transitive],[]).

a([ge_e(getrouwde),
   ge_no_e(getrouwd)],adv,
  [pp(met)],[]).

a([ge_e(getructe),
   ge_no_e(getruct)],nonadv,[],[]).

a([ge_e(getufte),
   ge_no_e(getuft)],nonadv,[],[]).

a([ge_e(getuigde),
   ge_no_e(getuigd)],adv,[],[]).

a([ge_e(getuimelde),
   ge_no_e(getuimeld)],adv,[],[]).

a([ge_e(getuite),
   ge_no_e(getuit)],adv,[],[]).

a([ge_e(getutoyeerde),
   ge_no_e(getutoyeerd)],adv,[],[]).

a([ge_e(getypeerde),
   ge_no_e(getypeerd)],adv,[],[]).

a([ge_e(getypte),
   ge_no_e(getypt)],adv,[],[]).

a([ge_e(geuite),
   ge_no_e(geuit)],adv,[],[]).

a([ge_e(geüniformeerde),
   ge_no_e(geüniformeerd)],padv,[],[]).

a([ge_e(geüploade),
   ge_no_e(geüpload)],padv,[],[]).

a([e(geurige),
   er(geuriger),
   ere(geurigere),
   no_e(geurig),
   st(geurigst),
   ste(geurigste)],nonadv,[],[]).

a([ge_e(gevaagde),
   ge_no_e(gevaagd)],adv,[],[]).

a([e(gevaarlijke),
   er(gevaarlijker),
   ere(gevaarlijkere),
   no_e(gevaarlijk),
   st(gevaarlijkst),
   ste(gevaarlijkste)],adv,
  [subject_vp,
   subject_sbar,
   pp(voor)],
  [brand,
   s(staat),
   vlucht]).

a([ge_e(gevaccineerde),
   ge_no_e(gevaccineerd)],padv,[],[]).

a([ge_both(gevallen)],adv,
  [fixed([[door,de,mand]]),
   fixed([[ten,deel]]),
   fixed([[ten,deel],dat]),
   fixed([[ten,prooi],dat]),
   fixed([[ten,prooi],dat_pp(aan)])],[]).

a([ge_both(gevangen)],adv,
  [pp(in)],[]).

a([ge_both(gevangengenomen)],padv,[],[]).

a([ge_e(gevangengezette),
   ge_no_e(gevangengezet)],padv,[],[]).

a([ge_both(gevaren)],adv,[],[]).

a([ge_e(gevarieerde),
   er(gevarieerder),
   ere(gevarieerdere),
   ge_no_e(gevarieerd),
   st(gevarieerdst),
   ste(gevarieerdste)],adv,[],[]).

a([e(gevatte),
   no_e(gevat),
   er(gevatter),
   ere(gevattere),
   st(gevatst),
   ste(gevatste)],adv,[],[]).

a([e(gevederde),
   no_e(gevederd)],nonadv,[],[]).

a([ge_e(geveegde),
   ge_no_e(geveegd)],adv,[],[]).

a([ge_e(geveelde),
   ge_no_e(geveeld)],adv,[],[]).

a([ge_e(geveerde),
   ge_no_e(geveerd)],adv,[],[]).

a([ge_e(geveilde),
   ge_no_e(geveild)],adv,[],[]).

a([ge_e(geveinsde),
   ge_no_e(geveinsd)],adv,[],[]).

a([ge_e(gevelde),
   ge_no_e(geveld)],adv,[],[]).

a([ge_e(geventileerde),
   ge_no_e(geventileerd)],adv,[],[]).

a([ge_e(geverfde),
   ge_no_e(geverfd)],adv,[],[]).

a([ge_e(gevergde),
   ge_no_e(gevergd)],adv,[],[]).

a([ge_e(geverifieerde),
   ge_no_e(geverifieerd)],adv,[],[]).

a([ge_e(geverniste),
   ge_no_e(gevernist)],adv,[],[]).

a([ge_e(gevestigde),
   ge_no_e(gevestigd)],adv,[],[]).

a([ge_e(gevibreerde),
   ge_no_e(gevibreerd)],adv,[],[]).

a([ge_e(gevierde),
   er(gevierder),
   ere(gevierdere),
   ge_no_e(gevierd),
   st(gevierdst),
   ste(gevierdste)],adv,
  [pp(in),
   pp(met)],[]).

a([ge_e(gevijzelde),
   ge_no_e(gevijzeld)],padv,[],[]).

a([ge_e(gevilde),
   ge_no_e(gevild)],adv,[],[]).

a([ge_e(gevingerde),
   ge_no_e(gevingerd)],adv,[],[]).

a([ge_e(geviste),
   ge_no_e(gevist)],adv,[],[]).

a([ge_e(gevlamde),
   ge_no_e(gevlamd)],adv,[],[]).

a([ge_e(gevleide),
   ge_no_e(gevleid)],padv,[],[]).

a([e(gevlekte),
   no_e(gevlekt)],nonadv,[],[]).

a([e(gevleugelde),
   no_e(gevleugeld)],nonadv,[],[]).

a([ge_e(gevlijde),
   ge_no_e(gevlijd)],adv,[],[]).

a([ge_both(gevlochten)],adv,[],[]).

a([ge_e(gevloeide),
   ge_no_e(gevloeid)],adv,[],[]).

a([ge_e(gevloerde),
   ge_no_e(gevloerd)],padv,[],[]).

a([ge_both(gevlogen)],adv,[],[]).

a([ge_e(gevluchte),
   ge_no_e(gevlucht)],adv,[],[]).

a([ge_both(gevochten)],adv,[],[]).

a([ge_e(gevoede),
   ge_no_e(gevoed)],adv,[],[]).

a([ge_e(gevoederde),
   ge_no_e(gevoederd)],adv,[],[]).

a([ge_e(gevoegde),
   ge_no_e(gevoegd)],adv,[],[]).

a([ge_e(gevoelde),
   ge_no_e(gevoeld)],adv,[],[]).

a([e(gevoelige),
   er(gevoeliger),
   ere(gevoeligere),
   no_e(gevoelig),
   st(gevoeligst),
   ste(gevoeligste)],adv,
  [pp(voor)],[diep,
	      koers]).

a([e(gevoelloze),
   er(gevoellozer),
   ere(gevoellozere),
   no_e(gevoelloos),
   st(gevoelloost),
   ste(gevoellooste)],adv,[],[]).

a([e(gevoelsmatige),
   no_e(gevoelsmatig)],adv,[],[]).

a([ge_e(gevoerde),
   ge_no_e(gevoerd)],adv,
  [pp(door),
   fixed([[ten,tonele]]),
   pp(met),
   pp(over),
   pp(tegen),
   pp(voor)],[]).

a([ge_e(gevolgde),
   ge_no_e(gevolgd)],adv,
  [fixed([[op,de,voet]])],
  []).

a([e(gevolmachtigde),
   no_e(gevolmachtigd)],padv,
  [object_vp],[]).

a([ge_both(gevonden)],adv,[],[]).

a([ge_e(gevorderde),
   er(gevorderder),
   ere(gevorderdere),
   ge_no_e(gevorderd),
   st(gevorderdst),
   ste(gevorderdste)],adv,[],[]).

a([ge_e(gevormde),
   er(gevormder),
   ere(gevormdere),
   ge_no_e(gevormd),
   st(gevormdst),
   ste(gevormdste)],adv,
  [pp(door)],[]).

a([ge_both(gevouwen)],adv,[],[]).

a([ge_e(gevraagde),
   er(gevraagder),
   ere(gevraagdere),
   ge_no_e(gevraagd),
   st(gevraagdst),
   ste(gevraagdste)],padv,
  [pp(door),
   so_np,
   object_vp,
   object_sbar],[]).

a([ge_e(gevreesde),
   er(gevreesder),
   ere(gevreesdere),
   ge_no_e(gevreesd),
   st(gevreesdst),
   ste(gevreesdste)],adv,[],[]).

a([ge_both(gevreten)],adv,[],[]).

a([ge_e(gevriesdroogde),
   ge_no_e(gevriesdroogd)],adv,[],[]).

a([ge_e(gevrijwaarde),
   ge_no_e(gevrijwaard)],adv,[pp(van)],[]).

a([ge_e(gevulde),
   ge_no_e(gevuld)],adv,
  [pp(met)],[]).

a([ge_no_e(gevuurd),
   ge_e(gevuurde)],adv,[],[]).

a([ge_e(gewade),
   ge_no_e(gewaad)],adv,[],[]).

a([ge_e(gewaagde),
   er(gewaagder),
   ere(gewaagdere),
   ge_no_e(gewaagd),
   st(gewaagdst),
   ste(gewaagdste)],adv,
  [subject_vp,
   pp(aan)],[]).

a([ge_e(gewaaide),
   ge_no_e(gewaaid)],adv,[],[]).

a([ge_e(gewaande),
   ge_no_e(gewaand)],adv,
  [ap_pred],[]).  % verloren gewaande

a([ge_e(gewaarborgde),
   ge_no_e(gewaarborgd)],adv,[],[]).

a([ge_e(gewaardeerde),
   ge_no_e(gewaardeerd)],adv,[],[]).

a([ge_e(gewaarschuwde),
   ge_no_e(gewaarschuwd)],padv,[],[]).

a([ge_e(gewachte),
   ge_no_e(gewacht)],adv,[],[]).

a([ge_e(gewaggelde),
   ge_no_e(gewaggeld)],adv,[],[]).

a([e(gewalde),
   no_e(gewald)],adv,[],[]).

a([ge_e(gewalste),
   ge_no_e(gewalst)],adv,[],[]).

a([ge_e(gewantrouwde),
   ge_no_e(gewantrouwd)],adv,[],[]).

a([ge_e(gewapende),
   ge_no_e(gewapend)],both,
  [pp(met)],[]).

a([ge_e(gewarmde),
   ge_no_e(gewarmd)],adv,[],[]).

a([ge_both(gewassen)],adv,[pp(tegen)],[]). % er niet tegenop gewassen
				% cf opgewassen

a([e(gewaste),
   no_e(gewast)],adv,[],[]).

a([ge_e(gewaterde),
   ge_no_e(gewaterd)],adv,[],[]).

a([ge_e(gewedde),
   ge_no_e(gewed)],adv,[],[]).

a([ge_e(geweekte),
   ge_no_e(geweekt)],adv,[],[]).

a([ge_e(geweerde),
   ge_no_e(geweerd)],adv,[],[]).

a([ge_e(geweide),
   ge_no_e(geweid)],adv,[],[]).

a([ge_e(geweigerde),
   ge_no_e(geweigerd)],adv,[],[]).

a([ge_both(geweken)],adv,[],[]).

a([ge_e(gewekte),
   ge_no_e(gewekt)],adv,[],[]).

a([ge_e(gewelde),
   ge_no_e(geweld)],adv,[],[]).

a([e(gewelddadige),
   er(gewelddadiger),
   ere(gewelddadigere),
   no_e(gewelddadig),
   st(gewelddadigst),
   ste(gewelddadigste)],adv,[],[]).

a([e(geweldige),
   er(geweldiger),
   ere(geweldigere),
   no_e(geweldig),
   st(geweldigst),
   ste(geweldigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(geweldloze),
   no_e(geweldloos)],adv,[],[]).

a([ge_e(gewelfde),
   er(gewelfder),
   ere(gewelfdere),
   ge_no_e(gewelfd),
   st(gewelfdst),
   ste(gewelfdste)],adv,[],[]).

a([ge_no_e(gewend),
   ge_e(gewende)],padv,
  [object_vp,
   object_sbar,
   er_pp_sbar(aan),
   er_pp_vp(aan),
   pp(aan)],[]).

a([ge_e(gewenkte),
   ge_no_e(gewenkt)],adv,[],[]).

a([ge_e(gewenste),
   er(gewenster),
   ere(gewenstere),
   ge_no_e(gewenst)],adv,
  [subject_sbar,
   subject_vp,
   pp(bij),
   pp(door),
   pp(in),
   pp(voor)],[]).

a([ge_e(gewentelde),
   ge_no_e(gewenteld)],adv,[],[]).

a([ge_no_e(gewerkt),
   ge_e(gewerkte)],adv,[],[]).

a([ge_no_e(gewerveld),
   ge_e(gewervelde)],adv,[],[]).

a([e(gewestelijke),
   no_e(gewestelijk)],adv,[],[]).

a([ge_both(geweten)],adv,[],[]).

a([e(gewetenloze),
   er(gewetenlozer),
   ere(gewetenlozere),
   no_e(gewetenloos),
   st(gewetenloost),
   ste(gewetenlooste)],adv,[],[]).

a([e(gewetensvolle),
   er(gewetensvoller),
   ere(gewetensvollere),
   no_e(gewetensvol),
   st(gewetensvolst),
   ste(gewetensvolste)],adv,[],[]).

a([ge_e(gewette),
   ge_no_e(gewet)],nonadv,[],[]).

a([ge_e(gewettigde),
   ge_no_e(gewettigd)],adv,[],[]).

a([ge_both(geweven)],adv,[],[]).

a([ge_both(gewezen)],adv,[],[]).

a([e(gewichtige),
   er(gewichtiger),
   ere(gewichtigere),
   no_e(gewichtig),
   st(gewichtigst),
   ste(gewichtigste)],adv,[],[]).

a([ge_e(gewiebelde),
   ge_no_e(gewiebeld)],adv,[],[]).

a([ge_e(gewiede),
   ge_no_e(gewied)],adv,[],[]).

a([ge_e(gewiegde),
   ge_no_e(gewiegd)],adv,[],[]).

a([ge_e(gewiegelde),
   ge_no_e(gewiegeld)],adv,[],[]).

a([e(gewiekste),
   er(gewiekster),
   ere(gewiekstere),
   no_e(gewiekst)],padv,[],[]).

a([e(gewiekte),
   no_e(gewiekt)],adv,[],[]).

a([ge_e(gewijde),
   ge_no_e(gewijd)],adv,
  [so_pp(aan),
   so_np],[]).

a([ge_e(gewijzigde),
   ge_no_e(gewijzigd)],adv,[],[]).

a([ge_e(gewikkelde),
   ge_no_e(gewikkeld)],padv,[],[]).

a([ge_e(gewikte),
   ge_no_e(gewikt)],adv,[],[]).

a([ge_e(gewilde),
   er(gewilder),
   ere(gewildere),
   ge_no_e(gewild),
   st(gewildst),
   ste(gewildste)],adv,
  [pp(bij)],[]).

a([e(gewillige),
   er(gewilliger),
   ere(gewilligere),
   no_e(gewillig),
   st(gewilligst),
   ste(gewilligste)],adv,[],[]).

a([ge_e(gewipte),
   ge_no_e(gewipt)],adv,[],[]).

a([ge_e(gewisselde),
   ge_no_e(gewisseld)],adv,[],[]).

a([ge_e(gewiste),
   ge_no_e(gewist)],adv,[],[]).

a([ge_e(gewitte),
   ge_no_e(gewit)],adv,[],[]).

a([ge_no_e(gewoed),
   ge_e(gewoede)],adv,[],[]).

a([ge_e(gewoelde),
   ge_no_e(gewoeld)],adv,[],[]).

a([ge_both(gewogen)],adv,[],[]).

a([ge_e(gewonde),
   er(gewonder),
   ere(gewondere),
   ge_no_e(gewond),
   st(gewondst),
   ste(gewondste)],padv,
  [],[f([licht]),
      f([zwaar])]).

a([ge_both(gewonnen)],adv,
  [pp(door),
   pp(van)],[]).

a([e(gewone),
   er(gewoner),
   ere(gewonere),
   no_e(gewoon),
   st(gewoonst),
   ste(gewoonste)],adv,
  [subject_sbar,
   subject_vp,
   pp(aan)],[]).

a([e(gewoonlijke),
   er(gewoonlijker),
   ere(gewoonlijkere),
   no_e(gewoonlijk),
   st(gewoonlijkst),
   ste(gewoonlijkste)],adv,[],[]).

a([pred(gewoontjes)],adv,[],[]).

a([ge_both(geworden)],padv,
  [pred],[]).

a([ge_both(geworpen)],padv,
  [ld_pp(in)],[]).  % de in de schoot geworpen macht

a([ge_e(gewortelde),
   ge_no_e(geworteld)],adv,
  [pp(in)],[]).

a([ge_both(geworven)],adv,[],[]).

a([ge_both(gewoven)],adv,[],[]).

a([ge_e(gewraakte),
   ge_no_e(gewraakt)],adv,[],[]).

a([ge_both(gewreven)],adv,[],[]).

a([ge_e(gewriemelde),
   ge_no_e(gewriemeld)],adv,[],[]).

a([ge_e(gewrikte),
   ge_no_e(gewrikt)],adv,[],[]).

a([ge_e(gewroete),
   ge_no_e(gewroet)],adv,[],[]).

a([ge_both(gewroken)],adv,[],[]).

a([ge_both(gewrongen)],adv,[],[]).

a([ge_e(gewurgde),
   ge_no_e(gewurgd)],adv,[],[]).

a([ge_e(gewurmde),
   ge_no_e(gewurmd)],adv,[],[]).

a([ge_e(gezaagde),
   ge_no_e(gezaagd)],adv,
  [pp(in)],[]).

a([ge_e(gezaaide),
   ge_no_e(gezaaid)],adv,[],[]).

a([ge_e(gezadelde),
   ge_no_e(gezadeld)],adv,[],[]).

a([e(gezaghebbende),
   no_e(gezaghebbend)],nonadv,[],[]).

a([ge_e(gezakte),
   ge_no_e(gezakt)],adv,
  [pp(door),
   pp(in),
   fixed([[in,elkaar]]),
   pp(naar)],[]).

a([ge_e(gezalfde),
   ge_no_e(gezalfd)],adv,[],[]).

a([e(gezamenlijke),
   postn_no_e(gezamenlijk)],padv,[],[]).

a([e(gezapige),
   er(gezapiger),
   ere(gezapigere),
   no_e(gezapig),
   st(gezapigst),
   ste(gezapigste)],adv,[],[]).

a([ge_e(gezeefde),
   ge_no_e(gezeefd)],adv,[],[]).

a([ge_e(gezeepte),
   ge_no_e(gezeept)],adv,[],[]).

a([ge_e(gezegde),
   ge_no_e(gezegd)],adv,[],[]).

a([ge_e(gezegende),
   er(gezegender),
   ere(gezegendere),
   ge_no_e(gezegend),
   st(gezegendst),
   ste(gezegendste)],adv,
  [pp(met)],[]).

a([ge_e(gezeilde),
   ge_no_e(gezeild)],adv,[],[]).

a([e(gezellige),
   er(gezelliger),
   ere(gezelligere),
   no_e(gezellig),
   st(gezelligst),
   ste(gezelligste)],adv,
  [subject_vp,
   subject_sbar],[super]).

a([ge_e(gezette),
   er(gezetter),
   ere(gezettere),
   ge_no_e(gezet),
   st(gezetst),
   ste(gezetste)],adv,[pred],[]).

a([ge_e(gezetelde),
   ge_no_e(gezeteld)],padv,[],[]).

a([ge_both(gezeten)],padv,[part(achterna)],[]).

a([ge_e(geziene),
   er(geziener),
   ere(gezienere),
   ge_no_e(gezien),
   st(gezienst),
   ste(gezienste)],adv,
  [pp(bij),
   fixed([[tegemoet]])],[]).

a([ge_e(gezigzagde),
   ge_no_e(gezigzagd)],adv,[],[]).

a([ge_e(gezinde),
   ge_no_e(gezind)],padv,
  [pred_so_np],[s(behoud),
		goed]).

a([ge_e(gezochte),
   er(gezochter),
   ere(gezochtere),
   ge_no_e(gezocht),
   st(gezochtst),
   ste(gezochtste)],adv,
  [pp(voor)],[]).

a([ge_e(gezoende),
   ge_no_e(gezoend)],adv,[],[]).

a([ge_e(gezoete),
   ge_no_e(gezoet)],adv,[],[]).

a([ge_both(gezogen)],adv,[],[]).

a([e(gezonde),
   er(gezonder),
   ere(gezondere),
   no_e(gezond),
   st(gezondst),
   ste(gezondste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_both(gezonden)],adv,[],[]).

a([ge_both(gezongen)],adv,[],[]).

a([ge_both(gezonken)],adv,[],[]).

a([ge_e(gezoogde),
   ge_no_e(gezoogd)],adv,[],[]).

%% gezoomd blauw ??
a([ge_e(gezoomde),
   ge_no_e(gezoomd)],adv,[],[]).

a([ge_both(gezouten),
   er(gezoutener),
   ere(gezoutenere),
   st(gezoutenst),
   ste(gezoutenste)],adv,[],[on]).

a([ge_e(gezuiverde),
   ge_no_e(gezuiverd)],adv,[],[]).

a([e(gezuurde),
   no_e(gezuurd)],nonadv,[],[]).

a([ge_e(gezwaaide),
   ge_no_e(gezwaaid)],adv,[],[]).

a([ge_e(gezwabberde),
   ge_no_e(gezwabberd)],adv,[],[]).

a([e(gezwarte),
   no_e(gezwart)],adv,[],[]).

a([ge_e(gezweefde),
   ge_no_e(gezweefd)],adv,[],[]).

a([ge_e(gezweepte),
   ge_no_e(gezweept)],adv,[],[]).

a([ge_e(gezwete),
   ge_no_e(gezweet)],adv,[],[]).

a([ge_e(gezwenkte),
   ge_no_e(gezwenkt)],adv,[],[]).

a([ge_e(gezwichte),
   ge_no_e(gezwicht)],adv,[],[]).

a([ge_e(gezwiepte),
   ge_no_e(gezwiept)],adv,[],[]).

a([ge_e(gezwinde),
   ge_no_e(gezwind)],adv,[],[]).

a([ge_both(gezwollen),
   er(gezwollener),
   ere(gezwollenere),
   st(gezwollenst),
   ste(gezwollenste)],adv,[],[]).

a([ge_both(gezwommen)],adv,[],[]).

a([ge_both(gezworen)],adv,[],[]).

a([ge_e(geëchode),
   ge_no_e(geëchood)],adv,[],[]).

a([ge_e(geëerbiedigde),
   ge_no_e(geëerbiedigd)],adv,[],[]).

a([ge_e(geëerde),
   ge_no_e(geëerd)],adv,[],[]).

a([ge_e(geëffectueerde),
   ge_no_e(geëffectueerd)],adv,[],[]).

a([ge_e(geëffektueerde),
   ge_no_e(geëffektueerd)],adv,[],[]).

a([ge_e(geëffende),
   ge_no_e(geëffend)],adv,[],[]).

a([ge_e(geëigende),
   ge_no_e(geëigend)],nonadv,
  [pp(tot),
   so_np],[]).

a([ge_e(geëindigde),
   ge_no_e(geëindigd)],adv,[],[]).

a([ge_e(geëiste),
   ge_no_e(geëist)],adv,[],[]).

a([ge_e(geëkskuseerde),
   ge_no_e(geëkskuseerd)],adv,[],[]).

a([ge_e(geëlimineerde),
   ge_no_e(geëlimineerd)],adv,[],[]).

a([ge_e(geëmancipeerde),
   ge_no_e(geëmancipeerd)],adv,[],[]).

a([ge_e(geëmigreerde),
   ge_no_e(geëmigreerd)],adv,[],[]).

a([ge_e(geëngageerde),
   ge_no_e(geëngageerd)],adv,[],[]).

a([ge_e(geënsceneerde),
   ge_no_e(geënsceneerd)],adv,[],[]).

a([ge_e(geënte),
   ge_no_e(geënt)],adv,[],[]).

a([ge_e(geërfde),
   ge_no_e(geërfd)],adv,[],[]).

a([ge_e(geërgerde),
   ge_no_e(geërgerd)],padv,[object_sbar],[]).

a([ge_e(geëscorteerde),
   ge_no_e(geëscorteerd)],adv,[],[]).

a([ge_e(geëtaleerde),
   ge_no_e(geëtaleerd)],adv,[],[]).

a([ge_e(geëtiketteerde),
   ge_no_e(geëtiketteerd)],adv,[],[]).

a([ge_e(geëtste),
   ge_no_e(geëtst)],adv,[],[]).

a([ge_e(geëvacueerde),
   ge_no_e(geëvacueerd)],adv,[],[]).

a([ge_e(geëvalueerde),
   ge_no_e(geëvalueerd)],adv,[],[]).

a([ge_e(geëvenaarde),
   ge_no_e(geëvenaard)],adv,[],[]).

a([ge_e(geëvolueerde),
   ge_no_e(geëvolueerd)],adv,[],[]).

a([ge_e(geëxcuseerde),
   ge_no_e(geëxcuseerd)],adv,[],[]).

a([ge_e(geëxecuteerde),
   ge_no_e(geëxecuteerd)],adv,[],[]).

a([ge_e(geëxpliciteerde),
   ge_no_e(geëxpliciteerd)],adv,[],[]).

a([ge_e(geëxplodeerde),
   ge_no_e(geëxplodeerd)],adv,[],[]).

a([ge_e(geëxploiteerde),
   ge_no_e(geëxploiteerd)],adv,[],[]).

a([ge_e(geëxploreerde),
   ge_no_e(geëxploreerd)],adv,[],[]).

a([ge_e(geëxporteerde),
   ge_no_e(geëxporteerd)],adv,[],[]).

a([ge_e(geïdealiseerde),
   ge_no_e(geïdealiseerd),
   ge_e(geïdealizeerde),
   ge_no_e(geïdealizeerd)],adv,[],[]).

a([ge_e(geïdentificeerde),
   ge_no_e(geïdentificeerd)],adv,[],[]).

a([ge_e(geïllustreerde),
   ge_no_e(geïllustreerd)],adv,
  [pp(door),
   pp(met)],[]).

a([ge_e(geïmiteerde),
   ge_no_e(geïmiteerd)],adv,[],[]).

a([ge_e(geïmplementeerde),
   ge_no_e(geïmplementeerd)],adv,[],[]).

a([ge_e(geïmpliceerde),
   ge_no_e(geïmpliceerd)],adv,[],[]).

a([ge_e(geïmponeerde),
   ge_no_e(geïmponeerd)],adv,[],[]).

a([ge_e(geïmporteerde),
   ge_no_e(geïmporteerd)],adv,[],[]).

a([ge_e(geïmproviseerde),
   ge_no_e(geïmproviseerd),
   ge_e(geïmprovizeerde),
   ge_no_e(geïmprovizeerd)],adv,[],[]).

a([ge_e(geïncasseerde),
   ge_no_e(geïncasseerd)],adv,[],[]).

a([ge_e(geïnde),
   ge_no_e(geïnd)],adv,[],[]).

a([ge_e(geïndiceerde),
   ge_no_e(geïndiceerd)],adv,[],[]).

a([ge_e(geïndividualiseerde),
   ge_no_e(geïndividualiseerd),
   ge_e(geïndividualizeerde),
   ge_no_e(geïndividualizeerd)],adv,[],[]).

a([ge_e(geïnduceerde),
   ge_no_e(geïnduceerd)],adv,[],[]).

a([ge_e(geïndustrialiseerde),
   ge_no_e(geïndustrialiseerd),
   ge_e(geïndustrializeerde),
   ge_no_e(geïndustrializeerd)],adv,[],[]).

a([ge_e(geïnfecteerde),
   ge_no_e(geïnfecteerd)],adv,[],[]).

a([ge_e(geïnfekteerde),
   ge_no_e(geïnfekteerd)],adv,[],[]).

a([ge_e(geïnfiltreerde),
   ge_no_e(geïnfiltreerd)],adv,[],[]).

a([ge_e(geïnformeerde),
   ge_no_e(geïnformeerd)],adv,[],[]).

a([ge_e(geïnhaleerde),
   ge_no_e(geïnhaleerd)],adv,[],[]).

a([ge_e(geïnitieerde),
   ge_no_e(geïnitieerd)],adv,[],[]).

a([ge_e(geïnkasseerde),
   ge_no_e(geïnkasseerd)],adv,[],[]).

a([ge_e(geïnsinueerde),
   ge_no_e(geïnsinueerd)],adv,[],[]).

a([ge_e(geïnspecteerde),
   ge_no_e(geïnspecteerd)],adv,[],[]).

a([ge_e(geïnspekteerde),
   ge_no_e(geïnspekteerd)],adv,[],[]).

a([ge_e(geïnspireerde),
   er(geïnspireerder),
   ere(geïnspireerdere),
   ge_no_e(geïnspireerd),
   st(geïnspireerdst),
   ste(geïnspireerdste)],adv,[],[]).

a([ge_e(geïnstalleerde),
   ge_no_e(geïnstalleerd)],adv,[],[]).

a([ge_e(geïnstitutionaliseerde),
   ge_no_e(geïnstitutionaliseerd),
   ge_e(geïnstitutionalizeerde),
   ge_no_e(geïnstitutionalizeerd)],adv,[],[]).

a([ge_e(geïnstrueerde),
   ge_no_e(geïnstrueerd)],adv,[],[]).

a([ge_e(geïntegreerde),
   ge_no_e(geïntegreerd)],adv,
  [pp(in)],[]).

a([ge_e(geïntensiveerde),
   ge_no_e(geïntensiveerd)],adv,[],[]).

a([ge_e(geïnteresseerde),
   er(geïnteresseerder),
   ere(geïnteresseerdere),
   ge_no_e(geïnteresseerd),
   st(geïnteresseerdst),
   ste(geïnteresseerdste)],adv,
  [er_pp_sbar(in),
   er_pp_vp(in),
   pp(in),
   object_vp],[]).

a([ge_e(geïnterneerde),
   ge_no_e(geïnterneerd)],adv,[],[]).

a([ge_e(geïnterpreteerde),
   ge_no_e(geïnterpreteerd)],adv,[],[]).

a([ge_e(geïnterviewde),
   ge_no_e(geïnterviewd)],adv,[],[]).

a([ge_e(geïntimideerde),
   ge_no_e(geïntimideerd)],padv,[],[]).

a([ge_e(geïntrigeerde),
   ge_no_e(geïntrigeerd)],adv,[],[]).

a([ge_e(geïntroduceerde),
   ge_no_e(geïntroduceerd)],adv,[],[]).

a([ge_e(geïnventariseerde),
   ge_no_e(geïnventariseerd),
   ge_e(geïnventarizeerde),
   ge_no_e(geïnventarizeerd)],adv,[],[]).

a([ge_e(geïnvesteerde),
   ge_no_e(geïnvesteerd)],adv,[],[]).

a([ge_e(geïnviteerde),
   ge_no_e(geïnviteerd)],adv,[],[]).

a([ge_e(geïrriteerde),
   er(geïrriteerder),
   ere(geïrriteerdere),
   st(geïrriteerdst),
   ste(geïrriteerdste),
   ge_no_e(geïrriteerd)],padv,
  [er_pp_sbar(over),
   pp(over)],[]).

a([ge_e(geïsoleerde),
   ge_no_e(geïsoleerd)],adv,[],[]).

a([ge_e(geüniformeerde),
   ge_no_e(geüniformeerd)],adv,[],[]).

a([ende(giebelende),
   end(giebelend)],padv,[],[]).

a([e(gierige),
   er(gieriger),
   ere(gierigere),
   no_e(gierig),
   st(gierigst),
   ste(gierigste)],nonadv,[],[leer]).

a([stof(gietijzeren)],nonadv,[],[]).

a([e(giftige),
   er(giftiger),
   ere(giftigere),
   no_e(giftig),
   st(giftigst),
   ste(giftigste)],padv,
  [object_vp],[]).

a([e(gigantische),
   er(gigantischer),
   ere(gigantischere),
   no_e(gigantisch),
   st(gigantischt),
   ste(gigantischte)],adv,[],[]).

a([e(gindse),
   no_e(ginds)],adv,[],[]).

a([stof(gipsen)],nonadv,[],[]).

a([e(gisse),
   no_e(gis),
   er(gisser),
   ere(gissere),
   st(gist),
   ste(giste)],adv,[],[]).

a([e(gladde),
   er(gladder),
   ere(gladdere),
   no_e(glad),
   st(gladst),
   ste(gladste)],adv,[],[spek]).

a([ge_both(gladgestreken)],adv,[],[]).

a([pred(gladjes)],adv,[],[]).

a([e(glansrijke),
   no_e(glansrijk)],adv,[],[]).

a([e(glasheldere),
   er(glashelderder),
   ere(glashelderdere),
   no_e(glashelder),
   st(glashelderst),
   ste(glashelderste)],adv,
  [subject_sbar],[]).

a([prefix([glas,in,lood]),
   prefix('glas-in-lood')],nonadv,[],[]).

a([stof(glazen)],nonadv,[],[]).

a([e(glazige),
   no_e(glazig)],adv,[],[]).

a([e(glibberige),
   er(glibberiger),
   ere(glibberigere),
   no_e(glibberig),
   st(glibberigst),
   ste(glibberigste)],nonadv,[],[]).

a([ende(glinsterende),
   er(glinsterender),
   ere(glinsterendere),
   end(glinsterend),
   st(glinsterendst),
   ste(glinsterendste)],padv,[],[]).

a([e(globale),
   er(globaler),
   ere(globalere),
   no_e(globaal),
   st(globaalst),
   ste(globaalste)],adv,[],[]).

a([e(gloednieuwe),
   er(gloednieuwer),
   ere(gloednieuwere),
   no_e(gloednieuw),
   st(gloednieuwst),
   ste(gloednieuwste)],padv,[],[]).

a([e(gloedvolle),
   no_e(gloedvol)],adv,[],[]).

a([e(gloeiende),
   er(gloeiender),
   ere(gloeiendere),
   no_e(gloeiend),
   st(gloeiendst),
   ste(gloeiendste)],adv,[],[]).

a([e(glooiende),
   er(glooiender),
   ere(glooiendere),
   no_e(glooiend),
   st(glooiendst),
   ste(glooiendste)],adv,[],[]).

a([e(glorierijke),
   er(glorierijker),
   ere(glorierijkere),
   no_e(glorierijk),
   st(glorierijkst),
   ste(glorierijkste)],adv,[],[]).

a([e(glorieuze),
   er(glorieuzer),
   ere(glorieuzere),
   no_e(glorieus),
   st(glorieust),
   ste(glorieuste)],adv,[],[]).

a([both(glossy)],nonadv,[],[]).

a([e(gnostische),
   no_e(gnostisch)],nonadv,[],[]).

a([e(goddelijke),
   er(goddelijker),
   ere(goddelijkere),
   no_e(goddelijk),
   st(goddelijkst),
   ste(goddelijkste)],adv,[],[]).

a([e(goddeloze),
   er(goddelozer),
   ere(goddelozere),
   no_e(goddeloos),
   st(goddeloost),
   ste(goddelooste)],adv,[],[]).

a([e(godganse),
   no_e(godgans)],nonadv,[],[]).

a([e(godsdienstige),
   er(godsdienstiger),
   ere(godsdienstigere),
   no_e(godsdienstig),
   st(godsdienstigst),
   ste(godsdienstigste)],adv,[],[]).

a([both(godvergeten),
   er(godvergetener),
   ere(godvergetenere),
   st(godvergetenst),
   ste(godvergetenste)],adv,[],[]).

a([e(goede),
   er(beter),
   ere(betere),
   no_e(goed),
   st(best),
   ste(beste)],adv,
  [subject_sbar,
   subject_vp,
   subject_vp_sbar,  % goed om te weten is dat ...
   er_pp_vp(in),
   pp(in),
   pp(met),
   pp(voor)],[]).

a([e(goedaardige),
   er(goedaardiger),
   ere(goedaardigere),
   no_e(goedaardig),
   st(goedaardigst),
   ste(goedaardigste)],adv,[],[]).

a([e(goedbedoelde),
   no_e(goedbedoeld)],adv,[],[]).

a([ge_e(goedgekeurde),
   ge_no_e(goedgekeurd)],adv,[],[]).

a([e(goedgelovige),
   er(goedgeloviger),
   ere(goedgelovigere),
   no_e(goedgelovig),
   st(goedgelovigst),
   ste(goedgelovigste)],padv,[],[]).

a([ge_e(goedgemaakte),
   ge_no_e(goedgemaakt)],adv,[],[]).

a([ge_e(goedgemutste),
   ge_no_e(goedgemutst)],adv,[],[]).

a([ge_both(goedgevonden)],adv,[],[]).

a([e(goedhartige),
   er(goedhartiger),
   ere(goedhartigere),
   no_e(goedhartig),
   st(goedhartigst),
   ste(goedhartigste)],adv,[],[]).

a([e(goedige),
   er(goediger),
   ere(goedigere),
   no_e(goedig),
   st(goedigst),
   ste(goedigste)],adv,[],[]).

a([e(goedkope),
   er(goedkoper),
   ere(goedkopere),
   no_e(goedkoop),
   st(goedkoopst),
   ste(goedkoopste)],adv,
  [subject_vp],[]).

a([e(goedlachse),
   er(goedlachser),
   ere(goedlachsere),
   no_e(goedlachs),
   st(goedlachst),
   ste(goedlachste)],padv,[],[]).

a([e(goedmoedige),
   er(goedmoediger),
   ere(goedmoedigere),
   no_e(goedmoedig),
   st(goedmoedigst),
   ste(goedmoedigste)],adv,[],[]).

a([e(goedschikse),
   no_e(goedschiks)],adv,[],[]).

a([e(goedwillende),
   er(goedwillender),
   ere(goedwillendere),
   no_e(goedwillend),
   st(goedwillendst),
   ste(goedwillendste)],adv,[],[]).

a([both(golfplaten)],nonadv,[],[]).

a([prefix([good,old])],nonadv,[],[]).

a([e(gore),
   er(goorder),
   ere(goordere),
   no_e(goor),
   st(goorst),
   ste(goorste)],adv,[],[]).

a([e(gotische),
   no_e(gotisch)],nonadv,[],[]).

a([e(goudblonde),
   er(goudblonder),
   ere(goudblondere),
   no_e(goudblond),
   st(goudblondst),
   ste(goudblondste)],nonadv,[],[]).

a([e(goudbruine),
   er(goudbruiner),
   ere(goudbruinere),
   no_e(goudbruin),
   st(goudbruinst),
   ste(goudbruinste)],nonadv,[],[]).

a([stof(gouden)],nonadv,[],[]).

a([e('goud-gele'),
   e(goudgele),
   er('goud-geler'),
   er(goudgeler),
   ere('goud-gelere'),
   ere(goudgelere),
   no_e('goud-geel'),
   no_e(goudgeel),
   st('goud-geelst'),
   st(goudgeelst),
   ste('goud-geelste'),
   ste(goudgeelste)],nonadv,[],[]).

a([e(goudkleurige),
   er(goudkleuriger),
   ere(goudkleurigere),
   no_e(goudkleurig),
   st(goudkleurigst),
   ste(goudkleurigste)],nonadv,[],[]).

a([e(gouvernementele),
   no_e(gouvernementeel)],nonadv,[],[h(niet),h(non)]).

a([both(graag)],osentadv,[],[]).

a([e(gracieuze),
   er(gracieuzer),
   ere(gracieuzere),
   no_e(gracieus),
   st(gracieust),
   ste(gracieuste)],adv,[],[]).

a([e(graduele),
   no_e(gradueel)],adv,[],[]).

a([e(grafische),
   er(grafischer),
   ere(grafischere),
   no_e(grafisch),
   st(grafischt),
   ste(grafischte)],adv,[],[]).

a([e(grammaticale),
   e(grammatikale),
   no_e(grammaticaal),
   no_e(grammatikaal)],adv,[],[on]).

a([e(grandioze),
   er(grandiozer),
   ere(grandiozere),
   no_e(grandioos),
   st(grandioost),
   ste(grandiooste)],adv,
  [subject_vp],[]).

a([stof(granieten)],nonadv,[],[]).

a([e(grappige),
   er(grappiger),
   ere(grappigere),
   no_e(grappig),
   st(grappigst),
   ste(grappigste)],adv,
  [subject_sbar,
   subject_vp_sbar,
   subject_vp],[]).

a([both(gratis)],adv,
  [pp(voor)],[]).

a([e(gratuite),
   no_e(gratuit)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(grauwe),
   er(grauwer),
   ere(grauwere),
   no_e(grauw),
   st(grauwst),
   ste(grauwste)],nonadv,[],[]).

a([stof(grenen)],nonadv,[],[]).

a([e(grensoverschrijdende),
   no_e(grensoverschrijdend)],adv,[],[]).

a([e(grenzeloze),
   er(grenzelozer),
   ere(grenzelozere),
   no_e(grenzeloos),
   st(grenzeloost),
   ste(grenzelooste)],adv,[],[]).

a([e(gretige),
   er(gretiger),
   ere(gretigere),
   no_e(gretig),
   st(gretigst),
   ste(gretigste)],adv,[],[]).

a([e(grieperige),
   no_e(grieperig)],padv,[],[]).

a([e(griezelige),
   er(griezeliger),
   ere(griezeligere),
   no_e(griezelig),
   st(griezeligst),
   ste(griezeligste)],adv,[],[]).

a([e(griffe),
   er(griffer),
   ere(griffere),
   no_e(grif),
   st(grifst),
   ste(grifste)],adv,[],[]).

a([e(grijpgrage),
   no_e(grijpgraag)],nonadv,[],[]).

a([e(grijze),
   er(grijzer),
   ere(grijzere),
   no_e(grijs),
   st(grijst),
   ste(grijste)],nonadv,[],[]).

a([e('grijs-blauwe'),
   e(grijsblauwe),
   er('grijs-blauwer'),
   er(grijsblauwer),
   ere('grijs-blauwere'),
   ere(grijsblauwere),
   no_e('grijs-blauw'),
   no_e(grijsblauw),
   st('grijs-blauwst'),
   st(grijsblauwst),
   ste('grijs-blauwste'),
   ste(grijsblauwste)],nonadv,[],[]).

a([e(grijsgroene),
   er(grijsgroener),
   ere(grijsgroenere),
   no_e(grijsgroen),
   st(grijsgroenst),
   ste(grijsgroenste)],nonadv,[],[]).

a([e(grijzige),
   er(grijziger),
   ere(grijzigere),
   no_e(grijzig),
   st(grijzigst),
   ste(grijzigste)],nonadv,[],[]).

a([e(grillige),
   er(grilliger),
   ere(grilligere),
   no_e(grillig),
   st(grilligst),
   ste(grilligste)],adv,[],[]).

a([e(grimmige),
   er(grimmiger),
   ere(grimmigere),
   no_e(grimmig),
   st(grimmigst),
   ste(grimmigste)],adv,[],[]).

a([e(groene),
   er(groener),
   ere(groenere),
   no_e(groen),
   st(groenst),
   ste(groenste)],nonadv,[],
  [brons,
   paars,h(paars),
   rood,h(rood)]).

a([e(groenachtige),
   er(groenachtiger),
   ere(groenachtigere),
   no_e(groenachtig),
   st(groenachtigst),
   ste(groenachtigste)],nonadv,[],[]).

a([e(groenige),
   er(groeniger),
   ere(groenigere),
   no_e(groenig),
   st(groenigst),
   ste(groenigste)],nonadv,[],[]).

a([no_e(groepsgewijs),
   e(groepsgewijze)],adv,[],[]).

a([e(groezelige),
   er(groezeliger),
   ere(groezeligere),
   no_e(groezelig),
   st(groezeligst),
   ste(groezeligste)],nonadv,[],[]).

a([e(grove),
   er(grover),
   ere(grovere),
   no_e(grof),
   st(grofst),
   ste(grofste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(grondeloze),
   no_e(grondeloos)],nonadv,[],[]).

a([e(grondige),
   er(grondiger),
   ere(grondigere),
   no_e(grondig),
   st(grondigst),
   ste(grondigste)],adv,[],[]).

a([e(grondwettelijke),
   no_e(grondwettelijk)],adv,[],[]).

a([e(grote),
   er(groter),
   ere(grotere),
   no_e(groot),
   st(grootst),
   ste(grootste)],adv,
  [pp(in)],[pagina]).

a([ge_e(grootgebrachte),
   ge_no_e(grootgebracht)],adv,[],[]).

a([e(grootmoedige),
   er(grootmoediger),
   ere(grootmoedigere),
   no_e(grootmoedig),
   st(grootmoedigst),
   ste(grootmoedigste)],adv,[],[]).

a([e(grootse),
   er(grootser),
   ere(grootsere),
   no_e(groots)],adv,[],[]).

a([e(grootschalige),
   er(grootschaliger),
   ere(grootschaligere),
   no_e(grootschalig),
   st(grootschaligst),
   ste(grootschaligste)],adv,[],[]).

a([e(grootscheepse),
   er(grootscheepser),
   ere(grootscheepsere),
   no_e(grootscheeps),
   st(grootscheepst),
   ste(grootscheepste)],adv,[],[]).

a([e(grootstedelijke),
   no_e(grootstedelijk)],adv,[],[]).

a([e(grootsteedse),
   no_e(grootsteeds)],adv,[],[]).

a([e(grotelijkse),
   no_e(grotelijks)],adv,[],[]).

a([e(groteske),
   er(grotesker),
   ere(groteskere),
   no_e(grotesk),
   st(groteskst),
   ste(groteskste)],adv,[],[]).

a([e(gruwelijke),
   er(gruwelijker),
   ere(gruwelijkere),
   no_e(gruwelijk),
   st(gruwelijkst),
   ste(gruwelijkste)],adv,[],[]).

a([e(guitige),
   er(guitiger),
   ere(guitigere),
   no_e(guitig),
   st(guitigst),
   ste(guitigste)],adv,[],[]).

a([e(gulle),
   er(guller),
   ere(gullere),
   no_e(gul),
   st(gulst),
   ste(gulste)],adv,[],[]).

a([prefix(gulden)],nonadv,[],[]).

a([e(gulzige),
   er(gulziger),
   ere(gulzigere),
   no_e(gulzig),
   st(gulzigst),
   ste(gulzigste)],adv,[],[]).

a([e(gunstige),
   er(gunstiger),
   ere(gunstigere),
   no_e(gunstig),
   st(gunstigst),
   ste(gunstigste)],adv,
  [pp(voor),
   subject_sbar],[]).

a([e(guste),
   no_e(gust)],nonadv,[],[]).

a([e(gure),
   er(guurder),
   ere(guurdere),
   no_e(guur),
   st(guurst),
   ste(guurste)],adv,[],[]).

a([no_e(haaks),
   e(haakse)],adv,[pp(op)],[]).

a([e(haalbare),
   no_e(haalbaar)],nonadv,[subject_sbar,
                           subject_vp],[]).

a([e(haarfijne),
   er(haarfijner),
   ere(haarfijnere),
   no_e(haarfijn),
   st(haarfijnst),
   ste(haarfijnste)],adv,[],[]).

a([e(haarscherpe),
   no_e(haarscherp)],adv,[],[]).

a([e(haastige),
   er(haastiger),
   ere(haastigere),
   no_e(haastig),
   st(haastigst),
   ste(haastigste)],adv,[],[]).

a([e(haatdragende),
   er(haatdragender),
   ere(haatdragendere),
   no_e(haatdragend),
   st(haatdragendst),
   ste(haatdragendste)],padv,[],[]).

a([e(hachelijke),
   er(hachelijker),
   ere(hachelijkere),
   no_e(hachelijk),
   st(hachelijkst),
   ste(hachelijkste)],nonadv,[],[]).

a([e(hagelwitte),
   er(hagelwitter),
   ere(hagelwittere),
   no_e(hagelwit),
   st(hagelwitst),
   ste(hagelwitste)],nonadv,[],[]).

a([both(halal)],adv,[],[]).

a([e(hallucinogene),
   no_e(hallucinogeen)],nonadv,[],[]).

a([e(halve),
   no_e(half)],adv,[],[]).

a([both('half-time'),
   both(halftime),
   both([half,time])],adv,[],[]).

a([both(halfbakken)],nonadv,[],[]).

a([both(halfbloed)],nonadv,[],[]).

a([e(halfdode),
   no_e(halfdood)],padv,[],[]).

a([e(halfnaakte),
   no_e(halfnaakt)],adv,[],[]).

a([both(halfopen)],padv,[],[]).

a([e(halfronde),
   no_e(halfrond)],nonadv,[],[]).

a([e(halfslachtige),
   er(halfslachtiger),
   ere(halfslachtigere),
   no_e(halfslachtig),
   st(halfslachtigst),
   ste(halfslachtigste)],adv,[],[]).

%% met de vlag halfstok -> therefore not predm_adverb
%% de vlag hangt halfstok -> predc?
%% wij hangen de vlag halfstok -> predc??? (no such frame)
%% de vlag wappert halfstok -> mod?
a([pred(halfstok)],both,[],[]).

a([e(halftijdse),
   no_e(halftijds)],adv,[],[]).

a([e(halsstarrige),
   er(halsstarriger),
   ere(halsstarrigere),
   no_e(halsstarrig),
   st(halsstarrigst),
   ste(halsstarrigste)],adv,[],[]).

a([postn_pred([hand,in,hand])],padv,[pp(met)],[]).

a([end(handelsverstorend),
   ende(handelsverstorende)],padv,[],[]).

a([both(handheld)],nonadv,[],[]).

a([e(handige),
   er(handiger),
   ere(handigere),
   no_e(handig),
   st(handigst),
   ste(handigste)],adv,
  [subject_sbar,
   subject_vp_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(handige),
   no_e(handig)],adv,[],
  [links,
   rechts]).

a([e(handmatige),
   no_e(handmatig)],adv,[],[]).

a([both(handsfree),
   both('hands-free'),
   both([hands,free])],adv,[],[]).

a([e(handzame),
   no_e(handzaam)],nonadv,[],[]).

a([ende(hangende),
   end(hangend)],padv,[],[]).

a([e(hanteerbare),
   er(hanteerbaarder),
   ere(hanteerbaardere),
   no_e(hanteerbaar),
   st(hanteerbaarst),
   ste(hanteerbaarste)],nonadv,[],[]).

a([e(haploïde),
   no_e(haploïd)],nonadv,[],[]).

a([e(happige),no_e(happig)],nonadv,
  [pp(op),
   er_pp_vp(op),
   er_pp_sbar(op)],[]).

a([both(happy)],nonadv,[],[]).

a([e(harde),
   er(harder),
   ere(hardere),
   no_e(hard),
   st(hardst),
   ste(hardste)],adv,[],
  [knoeper,
   knoert,
   snoei]).

a([e(hardhandige),
   er(hardhandiger),
   ere(hardhandigere),
   no_e(hardhandig),
   st(hardhandigst),
   ste(hardhandigste)],adv,[],[]).

a([e(hardleerse),
   no_e(hardleers)],padv,[],[]).

a([e(hardnekkige),
   er(hardnekkiger),
   ere(hardnekkigere),
   no_e(hardnekkig),
   st(hardnekkigst),
   ste(hardnekkigste)],adv,[],[]).

a([e(hardvochtige),
   er(hardvochtiger),
   ere(hardvochtigere),
   no_e(hardvochtig),
   st(hardvochtigst),
   ste(hardvochtigste)],adv,[],[]).

a([ende(hardwerkende),
   end(hardwerkend)],padv,[],[]).

a([e(harige),
   er(hariger),
   ere(harigere),
   no_e(harig),
   st(harigst),
   ste(harigste)],nonadv,[],[glad]).

a([e(harmonieuze),
   er(harmonieuzer),
   ere(harmonieuzere),
   no_e(harmonieus),
   st(harmonieust),
   ste(harmonieuste)],adv,[],[]).

a([e(harmonische),
   er(harmonischer),
   ere(harmonischere),
   no_e(harmonisch),
   st(harmonischt),
   ste(harmonischte)],adv,[],[]).

a([e(hartelijke),
   er(hartelijker),
   ere(hartelijkere),
   no_e(hartelijk),
   st(hartelijkst),
   ste(hartelijkste)],adv,[subject_sbar,
                           subject_vp],[]).

a([e(harteloze),
   er(hartelozer),
   ere(hartelozere),
   no_e(harteloos),
   st(harteloost),
   ste(hartelooste)],nonadv,[subject_sbar,
                             subject_vp],[]).

a([e(hartgrondige),
   er(hartgrondiger),
   ere(hartgrondigere),
   no_e(hartgrondig),
   st(hartgrondigst),
   ste(hartgrondigste)],adv,[],[]).

a([e(hartige),
   er(hartiger),
   ere(hartigere),
   no_e(hartig),
   st(hartigst),
   ste(hartigste)],adv,[],[]).

a([e(hartstochtelijke),
   er(hartstochtelijker),
   ere(hartstochtelijkere),
   no_e(hartstochtelijk),
   st(hartstochtelijkst),
   ste(hartstochtelijkste)],adv,[],[]).

a([e(hartverscheurende),
   er(hartverscheurender),
   ere(hartverscheurendere),
   no_e(hartverscheurend),
   st(hartverscheurendst),
   ste(hartverscheurendste)],adv,[],[]).

a([e(hartverwarmende),
   no_e(hartverwarmend)],adv,
  [subject_sbar],[]).

a([e(hatelijke),
   er(hatelijker),
   ere(hatelijkere),
   no_e(hatelijk),
   st(hatelijkst),
   ste(hatelijkste)],adv,[],[]).

a([e(hautaine),
   er(hautainer),
   ere(hautainere),
   no_e(hautain),
   st(hautainst),
   ste(hautainste)],adv,[],[]).

a([e(haveloze),
   er(havelozer),
   ere(havelozere),
   no_e(haveloos),
   st(haveloost),
   ste(havelooste)],nonadv,[],[]).

a([e(hebzuchtige),
   er(hebzuchtiger),
   ere(hebzuchtigere),
   no_e(hebzuchtig),
   st(hebzuchtigst),
   ste(hebzuchtigste)],padv,[],[]).

a([e(hechte),
   er(hechter),
   ere(hechtere),
   no_e(hecht),
   st(hechtst),
   ste(hechtste)],adv,[],[]).

a([e(hectische),
   no_e(hectisch)],adv,[],[]).

a([e(hedendaagse),
   no_e(hedendaags)],adv,[],[]).

a([e(hedonistische),
   no_e(hedonistisch)],adv,[],[]).

a([e(hele),
   no_e(heel)],nonadv,[],[]).  % heel is adverb too

a([e(heelhuidse),
   no_e(heelhuids)],padv,[],[]).

a([ge_both(heengebroken)],adv,
  [ld_pp(door),
   ld_pp(langs),
   ld_pp(om),
   ld_pp(over)],[]).

a([ge_e(heengegane),
   ge_no_e(heengegaan)],adv,[],[]).

a([e(heerlijke),
   er(heerlijker),
   ere(heerlijkere),
   no_e(heerlijk),
   st(heerlijkst),
   ste(heerlijkste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(hese),
   er(heser),
   ere(hesere),
   no_e(hees),
   st(heest),
   ste(heeste)],padv,[],[]).

a([e(hete),
   er(heter),
   ere(hetere),
   no_e(heet),
   st(heetst),
   ste(heetste)],adv,[],[]).

a([e(heetgebakerde),
   no_e(heetgebakerd)],padv,[],[]).

a([e(heftige),
   er(heftiger),
   ere(heftigere),
   no_e(heftig),
   st(heftigst),
   ste(heftigste)],adv,[],[]).

a([e(heidense),
   er(heidenser),
   ere(heidensere),
   no_e(heidens),
   st(heidenst),
   ste(heidenste)],adv,[],[]).

a([e(heikele),
   no_e(heikel)],nonadv,[],[]).

a([e(heilige),
   er(heiliger),
   ere(heiligere),
   no_e(heilig),
   st(heiligst),
   ste(heiligste)],adv,[],[]).

a([e(heilloze),
   er(heillozer),
   ere(heillozere),
   no_e(heilloos),
   st(heilloost),
   ste(heillooste)],adv,[],[]).

a([e(heilzame),
   er(heilzamer),
   ere(heilzamere),
   no_e(heilzaam),
   st(heilzaamst),
   ste(heilzaamste)],adv,[],[]).

a([e(heimelijke),
   er(heimelijker),
   ere(heimelijkere),
   no_e(heimelijk),
   st(heimelijkst),
   ste(heimelijkste)],adv,[],[]).

a([both(helaas)],osentadv,
  [pp(voor)],[]).

a([e(helle),
   er(heller),
   ere(hellere),
   no_e(hel),
   st(helst),
   ste(helste)],adv,[],[]).

a([e(heldere),
   er(helderder),
   ere(helderdere),
   no_e(helder),
   st(helderst),
   ste(helderste)],adv,
  [subject_sbar_no_het],[kraak]).

a([e(helderblauwe),
   no_e(helderblauw)],nonadv,[],[]).

a([e(helderziende),
   er(helderziender),
   ere(helderziendere),
   no_e(helderziend),
   st(helderziendst),
   ste(helderziendste)],nonadv,[],[]).

a([e(heldhaftige),
   er(heldhaftiger),
   ere(heldhaftigere),
   no_e(heldhaftig),
   st(heldhaftigst),
   ste(heldhaftigste)],adv,[],[]).

a([e(hellenistische),
   er(hellenistischer),
   ere(hellenistischere),
   no_e(hellenistisch),
   st(hellenistischt),
   ste(hellenistischte)],nonadv,[],[]).

a([e(helse),
   er(helser),
   ere(helsere),
   no_e(hels),
   st(helst),
   ste(helste)],adv,[],[]).

a([e(hematocriete),
   no_e(hematocriet)],nonadv,[],[]).

a([e(hemelse),
   er(hemelser),
   ere(hemelsere),
   no_e(hemels),
   st(hemelst),
   ste(hemelste)],adv,[],[]).

a([e(hemelsblauwe),
   er(hemelsblauwer),
   ere(hemelsblauwere),
   no_e(hemelsblauw),
   st(hemelsblauwst),
   ste(hemelsblauwste)],nonadv,[],[]).

a([e(hemelsbrede),
   er(hemelsbreder),
   ere(hemelsbredere),
   no_e(hemelsbreed),
   st(hemelsbreedst),
   ste(hemelsbreedste)],adv,[],[]).

a([e(hemelwaartse),
   postn_no_e(hemelwaarts)],diradv,[],[]).

a([ge_both(herbegonnen)],adv,[],[]).

a([ge_e(herbevestigde),
   ge_no_e(herbevestigd)],adv,[],[]).

a([ge_e(herbewapende),
   ge_no_e(herbewapend)],padv,[],[]).

a([e(herbivore),
   no_e(herbivoor)],nonadv,[],[]).

a([both(herboren)],padv,[],[]).

a([ge_e(herbouwde),
   ge_no_e(herbouwd)],adv,[],[]).

a([ge_e(herdachte),
   ge_no_e(herdacht)],adv,[],[]).

a([ge_e(herdrukte),
   ge_no_e(herdrukt)],adv,[],[]).

a([ge_e(herenigde),
   ge_no_e(herenigd)],padv,[],[]).

a([ge_e(hergebruikte),
   ge_no_e(hergebruikt)],padv,[],[]).

a([ge_e(herhaalde),
   ge_no_e(herhaald)],adv,[],[]).

a([e(herhaaldelijke),
   no_e(herhaaldelijk)],adv,[],[]).

a([ge_e(herinnerde),
   ge_no_e(herinnerd)],adv,[],[]).

a([end(herintredend),
   ende(herintredende)],padv,[],[]).

a([ge_e(herkauwde),
   ge_no_e(herkauwd)],adv,[],[]).

a([e(herkenbare),
   er(herkenbaarder),
   ere(herkenbaardere),
   no_e(herkenbaar),
   st(herkenbaarst),
   ste(herkenbaarste)],padv,
  [pp(aan)],[]).

a([ge_e(herkende),
   ge_no_e(herkend)],adv,[],[]).

a([ge_e(herkeurde),
   ge_no_e(herkeurd)],padv,[],[]).

a([ge_both(herkozen)],padv,[],[]).

a([ge_both(herkregen)],adv,[],[]).

a([ge_e(herleefde),
   ge_no_e(herleefd)],adv,[],[]).

a([ge_e(herleide),
   ge_no_e(herleid)],adv,[],[]).

a([e(hermetische),
   er(hermetischer),
   ere(hermetischere),
   no_e(hermetisch),
   st(hermetischt),
   ste(hermetischte)],adv,[],[]).

a([e(hernieuwbare),
   no_e(hernieuwbaar)],adv,[],[]).

a([ge_e(hernieuwde),
   ge_no_e(hernieuwd)],adv,[],[]).

a([ge_e(hernoemde),
   ge_no_e(hernoemd)],adv,[pp(naar)],[]).

a([ge_both(hernomen)],adv,[],[]).

a([e(heroïsche),
   er(heroïscher),
   ere(heroïschere),
   no_e(heroïsch),
   st(heroïscht),
   ste(heroïschte)],adv,[],[]).

a([ge_e(herontdekte),
   ge_no_e(herontdekt)],nonadv,[],[]).

a([ge_e(heropende),
   ge_no_e(heropend)],nonadv,[],[]).

a([ge_e(heroverde),
   ge_no_e(heroverd)],adv,[],[]).

a([ge_e(herplaatste),
   ge_no_e(herplaatst)],adv,[],[]).

a([ge_both(herrezen)],adv,[],[]).

a([ge_both(herroepen)],adv,[],[]).

a([ge_both(herschapen)],adv,[],[]).

a([ge_e(herschikte),
   ge_no_e(herschikt)],nonadv,[],[]).

a([ge_both(herschreven)],adv,[],[]).

a([ge_e(herstelde),
   ge_no_e(hersteld)],adv,
  [pp(van)],[]).

a([ge_e(hertekende),
   ge_no_e(hertekend)],padv,[],[]).

a([ge_e(hertrouwde),
   ge_no_e(hertrouwd)],adv,[],[]).

a([ge_e(hervatte),
   ge_no_e(hervat)],adv,[],[]).

a([ge_both(hervonden)],adv,[],[]).

a([ge_e(hervormde),
   ge_no_e(hervormd)],adv,[],[]).

a([e(hervormingsgezinde),
   no_e(hervormingsgezind)],nonadv,[],[]).

a([ge_both(herwonnen)],adv,[],[]).

a([ge_no_e(herzien),
   ge_e(herziene)],adv,[],[]).

a([pred([het,van,het]),
   pred([hét,van,het]),
   pred([het,van,hét]),
   pred([hét,van,hét])],nonadv,[],[]).		%VL

a([e(heterogene),
   er(heterogener),
   ere(heterogenere),
   no_e(heterogeen),
   st(heterogeenst),
   ste(heterogeenste)],nonadv,[],[]).

a([e(heteroseksuele),
   e(heterosexuele),
   no_e(heteroseksueel),
   no_e(heterosexueel)],nonadv,[],[]).

a([pred([hetzelfde,laken,een,pak])],nonadv,[],[]).

a([e(heuse),
   er(heuser),
   ere(heusere),
   no_e(heus),
   st(heust),
   ste(heuste)],adv,[],[on]).

a([e(heuvelachtige),
   er(heuvelachtiger),
   ere(heuvelachtigere),
   no_e(heuvelachtig),
   st(heuvelachtigst),
   ste(heuvelachtigste)],nonadv,[],[]).

a([e(heuvelafwaartse),
   no_e(heuvelafwaarts)],diradv,[],[]).

a([e(heuvelopwaartse),
   no_e(heuvelopwaarts)],diradv,[],[]).

a([e(hevige),
   er(heviger),
   ere(hevigere),
   no_e(hevig),
   st(hevigst),
   ste(hevigste)],adv,[],[]).

a([e(hiërarchische),
   er(hiërarchischer),
   ere(hiërarchischere),
   no_e(hiërarchisch),
   st(hiërarchischt),
   ste(hiërarchischte)],adv,[],[]).

a([e(highe),
   no_e(high)],nonadv,[],[]).

a([both([high,level]),
   both(['high-level']),
   both(highlevel)],nonadv,[],[]).

a([both([high,tech]),
   both('high-tech'),
   both(hightech),
   both('hi-tech')],nonadv,[],[]).

a([e(hilarische),
   no_e(hilarisch)],adv,[],[]).

a([e(hinderlijke),
   er(hinderlijker),
   ere(hinderlijkere),
   no_e(hinderlijk),
   st(hinderlijkst),
   ste(hinderlijkste)],adv,[],[]).

a([e(hindoestaanse),
   no_e(hindoestaans)],adv,[],[]).

a([e(hippe),
   no_e(hip),
   er(hipper),
   ere(hippere),
   st(hipst),
   ste(hipste)],adv,[],[]).

a([e(hippische),
   no_e(hippisch)],nonadv,[],[]).

a([e(historische),
   er(historischer),
   ere(historischere),
   no_e(historisch),
   st(historischt),
   ste(historischte)],adv,[],[h(kunst),kunst,
                              h(cultuur),cultuur]).

a([e(hitsige),
   er(hitsiger),
   ere(hitsigere),
   no_e(hitsig),
   st(hitsigst),
   ste(hitsigste)],adv,[],[]).

a([e(hobbelige),
   er(hobbeliger),
   ere(hobbeligere),
   no_e(hobbelig),
   st(hobbeligst),
   ste(hobbeligste)],nonadv,[],[]).

a([ende(hoekende),
   end(hoekend)],adv,[],[]).

a([e(hoekige),
   er(hoekiger),
   ere(hoekigere),
   no_e(hoekig),
   st(hoekigst),
   ste(hoekigste)],adv,[],[]).

a([e(hoffelijke),
   er(hoffelijker),
   ere(hoffelijkere),
   no_e(hoffelijk),
   st(hoffelijkst),
   ste(hoffelijkste)],adv,[],[]).

a([e(holle),
   er(holler),
   ere(hollere),
   no_e(hol),
   st(holst),
   ste(holste)],adv,[],[]).

a([e(holistische),
   no_e(holistisch)],nonadv,[],[]).

a([e(homeopathische),
   e(homeopatische),
   e(homoeopathische),
   e(homoeopatische),
   no_e(homeopathisch),
   no_e(homeopatisch),
   no_e(homoeopathisch),
   no_e(homoeopatisch)],adv,[],[]).

a([e(homeostatische),
   no_e(homeostatisch)],nonadv,[],[]).

a([e(homerische),
   no_e(homerisch)],nonadv,[],[]).

a([e(homofiele),
   no_e(homofiel)],nonadv,[],[]).

a([e(homofobe),
   no_e(homofoob)],adv,[],[]).

a([e(homogene),
   er(homogener),
   ere(homogenere),
   no_e(homogeen),
   st(homogeenst),
   ste(homogeenste)],nonadv,[],[]).

a([e(homoseksuele),
   e(homosexuele),
   no_e(homoseksueel),
   no_e(homosexueel)],nonadv,[],[]).

a([e(hondse),
   no_e(honds)],adv,[],[]).

a([ende(honende),
   er(honender),
   ere(honendere),
   end(honend),
   st(honendst),
   ste(honendste)],adv,[],[]).

a([e(hongerige),
   er(hongeriger),
   ere(hongerigere),
   no_e(hongerig),
   st(hongerigst),
   ste(hongerigste)],padv,[],[]).

a([e(hoofdelijke),
   no_e(hoofdelijk)],adv,[],[]).

a([e(hoofdschuddende),
   no_e(hoofdschuddend)],padv,[],[]).

a([e(hoofdstedelijke),
   no_e(hoofdstedelijk)],nonadv,[],[]).

a([no_e(hoofdzakelijk),
   e(hoofdzakelijke)],adv,[],[]).

a([e(hoofse),
   er(hoofser),
   ere(hoofsere),
   no_e(hoofs),
   st(hoofst),
   ste(hoofste)],adv,[],[]).

a([e(hoge),
   er(hoger),
   ere(hogere),
   no_e(hoog),
   st(hoogst),
   ste(hoogste)],adv,[],
  [s(meter),
   middel]).

a([pred(hogerop)],dir_locadv,[],[]).

a([pred([hoog,tijd])],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(hoogbejaarde),
   no_e(hoogbejaard)],padv,[],[]).

a([e(hoogdravende),
   er(hoogdravender),
   ere(hoogdravendere),
   no_e(hoogdravend),
   st(hoogdravendst),
   ste(hoogdravendste)],padv,[],[]).

a([e(hooggeleerde),
   er(hooggeleerder),
   ere(hooggeleerdere),
   no_e(hooggeleerd),
   st(hooggeleerdst),
   ste(hooggeleerdste)],nonadv,[],[]).

a([e(hooggeplaatste),
   no_e(hooggeplaatst)],nonadv,[],[]).

a([both(hooggespannen)],nonadv,[],[]).

a([e(hooggestemde),
   er(hooggestemder),
   ere(hooggestemdere),
   no_e(hooggestemd),
   st(hooggestemdst),
   ste(hooggestemdste)],nonadv,[],[]).

a([e(hooggewaardeerde),
   no_e(hooggewaardeerd)],nonadv,[],[]).

a([e(hooghartige),
   er(hooghartiger),
   ere(hooghartigere),
   no_e(hooghartig),
   st(hooghartigst),
   ste(hooghartigste)],adv,[],[]).

a([e(hoogmoedige),
   er(hoogmoediger),
   ere(hoogmoedigere),
   no_e(hoogmoedig),
   st(hoogmoedigst),
   ste(hoogmoedigste)],adv,[],[]).

a([e(hoogmogende),
   no_e(hoogmogend)],nonadv,[],[]).

a([e(hoognodige),
   no_e(hoognodig)],adv,[],[]).

a([e(hoogstnodige),
   no_e(hoogstnodig)],adv,[],[]).

a([e(hoogopgeleide),
   no_e(hoogopgeleid)],adv,[],[]).

a([e(hoogrode),
   er(hoogroder),
   ere(hoogrodere),
   no_e(hoogrood),
   st(hoogroodst),
   ste(hoogroodste)],nonadv,[],[]).

a([e(hoogstaande),
   no_e(hoogstaand)],nonadv,[],[]).

a([e(hoogstpersoonlijke),
   no_e(hoogstpersoonlijk)],both,[],[]).

a([no_e(hoogstwaarschijnlijk),
   e(hoogstwaarschijnlijke)],adv,[],[]).

a([e(hoogwaardige),
   er(hoogwaardiger),
   ere(hoogwaardigere),
   no_e(hoogwaardig),
   st(hoogwaardigst),
   ste(hoogwaardigste)],nonadv,[],[]).

a([e(hoopgevende),
   er(hoopgevender),
   ere(hoopgevendere),
   no_e(hoopgevend),
   st(hoopgevendst),
   ste(hoopgevendste)],padv,[subject_sbar],[]).

a([e(hoopvolle),
   er(hoopvoller),
   ere(hoopvollere),
   no_e(hoopvol),
   st(hoopvolst),
   ste(hoopvolste)],adv,
  [object_sbar],[]).

a([e(hoorbare),
   er(hoorbaarder),
   ere(hoorbaardere),
   no_e(hoorbaar),
   st(hoorbaarst),
   ste(hoorbaarste)],adv,[],[]).

a([e(hopelijke),
   no_e(hopelijk)],adv,[],[]).

a([e(hopeloze),
   er(hopelozer),
   ere(hopelozere),
   no_e(hopeloos),
   st(hopeloost),
   ste(hopelooste)],adv,[],[]).

a([e(horizontale),
   er(horizontaler),
   ere(horizontalere),
   no_e(horizontaal),
   st(horizontaalst),
   ste(horizontaalste)],adv,[],[]).

a([e(hormonale),
   no_e(hormonaal)],nonadv,[],[]).

a([e([hortende,en,stotende]),
   no_e([hortend,en,stotend])],adv,[],[]).

a([e(hotte),
   no_e(hot),
   er(hotter),
   ere(hottere),
   st(hotst),
   ste(hotste)],nonadv,[],[]).

a([e(houdbare),
   er(houdbaarder),
   ere(houdbaardere),
   no_e(houdbaar),
   st(houdbaarst),
   ste(houdbaarste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([stof(houten)],nonadv,[],[]).

a([e(houterige),
   er(houteriger),
   ere(houterigere),
   no_e(houterig),
   st(houterigst),
   ste(houterigste)],adv,[],[]).

a([e(huichelachtige),
   er(huichelachtiger),
   ere(huichelachtigere),
   no_e(huichelachtig),
   st(huichelachtigst),
   ste(huichelachtigste)],adv,[],[]).

a([e(huidige),
   no_e(huidig)],nonadv,[],[]).

a([e(huilerige),
   er(huileriger),
   ere(huilerigere),
   no_e(huilerig),
   st(huilerigst),
   ste(huilerigste)],adv,[],[]).

a([e(huiselijke),
   er(huiselijker),
   ere(huiselijkere),
   no_e(huiselijk),
   st(huiselijkst),
   ste(huiselijkste)],adv,[],[]).

a([e(huishoudelijke),
   er(huishoudelijker),
   ere(huishoudelijkere),
   no_e(huishoudelijk),
   st(huishoudelijkst),
   ste(huishoudelijkste)],nonadv,[],[]).

a([e(huiswaartse),
   no_e(huiswaarts)],diradv,[],[]).

a([e(huiverige),
   er(huiveriger),
   ere(huiverigere),
   no_e(huiverig),
   st(huiverigst),
   ste(huiverigste)],adv,
  [object_vp,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([e(huiverachtige),
   no_e(huiverachtig)],nonadv,
  [object_vp,
   er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([e(huiveringwekkende),
   er(huiveringwekkender),
   ere(huiveringwekkendere),
   no_e(huiveringwekkend),
   st(huiveringwekkendst),
   ste(huiveringwekkendste)],adv,[],[]).

a([e(hulpbehoevende),
   er(hulpbehoevender),
   ere(hulpbehoevendere),
   no_e(hulpbehoevend),
   st(hulpbehoevendst),
   ste(hulpbehoevendste)],padv,[],[]).

a([e(hulpeloze),
   er(hulpelozer),
   ere(hulpelozere),
   no_e(hulpeloos),
   st(hulpeloost),
   ste(hulpelooste)],adv,[],[]).

a([e(hulpvaardige),
   er(hulpvaardiger),
   ere(hulpvaardigere),
   no_e(hulpvaardig),
   st(hulpvaardigst),
   ste(hulpvaardigste)],padv,[],[]).

a([e(humane),
   er(humaner),
   ere(humanere),
   no_e(humaan),
   st(humaanst),
   ste(humaanste)],adv,[],[]).

a([e(humanistische),
   er(humanistischer),
   ere(humanistischere),
   no_e(humanistisch),
   st(humanistischt),
   ste(humanistischte)],nonadv,[],[]).

a([e(humanitaire),
   er(humanitairder),
   ere(humanitairdere),
   no_e(humanitair),
   st(humanitairst),
   ste(humanitairste)],adv,[],[]).

a([e(humeurige),
   er(humeuriger),
   ere(humeurigere),
   no_e(humeurig),
   st(humeurigst),
   ste(humeurigste)],adv,[],[]).

a([e(humoristische),
   er(humoristischer),
   ere(humoristischere),
   no_e(humoristisch),
   st(humoristischt),
   ste(humoristischte)],adv,[],[]).

a([e(huwelijkse),
   no_e(huwelijks)],nonadv,[],[]).

a([both(hybride)],nonadv,[],[]).

a([e(hygiënische),
   er(hygiënischer),
   ere(hygiënischere),
   no_e(hygiënisch),
   st(hygiënischt),
   ste(hygiënischte)],adv,[],[]).

a([e(hypnotische),
   no_e(hypnotisch)],adv,[],[]).

a([e(hypocriete),
   e(hypokriete),
   er(hypocrieter),
   er(hypokrieter),
   ere(hypocrietere),
   ere(hypokrietere),
   no_e(hypocriet),
   no_e(hypokriet),
   st(hypocrietst),
   st(hypokrietst),
   ste(hypocrietste),
   ste(hypokrietste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(hypothecaire),
   no_e(hypothecair)],nonadv,[],[]).

a([e(hypotetische),
   e(hypothetische),
   er(hypotetischer),
   er(hypothetischer),
   ere(hypotetischere),
   ere(hypothetischere),
   no_e(hypotetisch),
   no_e(hypothetisch),
   st(hypotetischt),
   st(hypothetischt),
   ste(hypotetischte),
   ste(hypothetischte)],adv,
  [subject_vp],[]).

a([e(hysterische),
   er(hysterischer),
   ere(hysterischere),
   no_e(hysterisch),
   st(hysterischt),
   ste(hysterischte)],adv,[],[]).

a([e(ideale),
   er(idealer),
   ere(idealere),
   no_e(ideaal),
   st(ideaalst),
   ste(ideaalste)],adv,
  [subject_vp,
   subject_sbar,
   pp(voor)],[]).

a([e(idealistische),
   er(idealistischer),
   ere(idealistischere),
   no_e(idealistisch),
   st(idealistischt),
   ste(idealistischte)],adv,[],[]).

a([e(ideële),
   er(ideëler),
   ere(ideëlere),
   no_e(ideëel),
   st(ideëelst),
   ste(ideëelste)],nonadv,[],[]).

%% identiek dezelfde vorm
%% identiek gelijke vorm
a([e(identieke),
   no_e(identiek)],adv,
  [pp(aan)],[]).

a([e(ideologische),
   no_e(ideologisch)],adv,[],[]).

a([e(idiote),
   er(idioter),
   ere(idiotere),
   no_e(idioot),
   st(idiootst),
   ste(idiootste)],adv,[subject_vp,
                        subject_sbar],[]).

a([e(idyllische),
   er(idyllischer),
   ere(idyllischere),
   no_e(idyllisch),
   st(idyllischt),
   ste(idyllischte)],adv,[],[]).

a([e(iele),
   er(ieler),
   ere(ielere),
   no_e(iel),
   st(ielst),
   ste(ielste)],adv,[],[]).

a([e(ijdele),
   er(ijdeler),
   ere(ijdelere),
   no_e(ijdel),
   st(ijdelst),
   ste(ijdelste)],adv,[],[]).

a([e(ijle),
   er(ijler),
   ere(ijlere),
   no_e(ijl),
   st(ijlst),
   ste(ijlste)],nonadv,[],[]).

a([e(ijlingse),
   no_e(ijlings)],oadv,[],[]).

a([e(ijverige),
   er(ijveriger),
   ere(ijverigere),
   no_e(ijverig),
   st(ijverigst),
   ste(ijverigste)],adv,[],[]).

a([stof(ijzeren)],nonadv,[],[]).

a([e(ijzersterke),
   er(ijzersterker),
   ere(ijzersterkere),
   no_e(ijzersterk),
   st(ijzersterkst),
   ste(ijzersterkste)],adv,[],[]).

a([e(ijzige),
   er(ijziger),
   ere(ijzigere),
   no_e(ijzig),
   st(ijzigst),
   ste(ijzigste)],adv,[],[]).

a([e(illegale),
   er(illegaler),
   ere(illegalere),
   no_e(illegaal),
   st(illegaalst),
   ste(illegaalste)],adv,[],[]).

a([e(illusoire),
   er(illusoirder),
   ere(illusoirdere),
   no_e(illusoir),
   st(illusoirst),
   ste(illusoirste)],nonadv,[],[]).

a([e(illustere),
   er(illusterder),
   ere(illusterdere),
   no_e(illuster),
   st(illusterst),
   ste(illusterste)],nonadv,[],[]).

a([e(illustratieve),
   er(illustratiever),
   ere(illustratievere),
   no_e(illustratief),
   st(illustratiefst),
   ste(illustratiefste)],nonadv,
  [pp(voor),
   subject_sbar],[]).

a([e(imaginaire),
   er(imaginairder),
   ere(imaginairdere),
   no_e(imaginair),
   st(imaginairst),
   ste(imaginairste)],adv,[],[]).

a([e(immanente),
   no_e(immanent)],nonadv,[],[]).

a([e(immateriële),
   er(immateriëler),
   ere(immaterielere),
   no_e(immaterieel),
   st(immaterieelst),
   ste(immaterieelste)],nonadv,[],[]).

a([e(immense),
   no_e(immens)],adv,[],[]).

a([e(immorele),
   er(immoreler),
   ere(immorelere),
   no_e(immoreel),
   st(immoreelst),
   ste(immoreelste)],adv,
  [subject_vp,
   subject_sbar
  ],[]).

a([e(immune),
   er(immuner),
   ere(immunere),
   no_e(immuun),
   st(immuunst),
   ste(immuunste)],nonadv,
  [pp(voor)],[]).

a([e(imperialistische),
   er(imperialistischer),
   ere(imperialistischere),
   no_e(imperialistisch),
   st(imperialistischt),
   ste(imperialistischte)],adv,[],[]).

a([e(impliciete),
   no_e(impliciet)],adv,[],[]).

a([e(impopulaire),
   no_e(impopulair)],adv,[],[]).

a([e(importante),
   no_e(important)],nonadv,[],[]).

a([e(imposante),
   er(imposanter),
   ere(imposantere),
   no_e(imposant),
   st(imposantst),
   ste(imposantste)],adv,[],[]).

a([e(impotente),
   er(impotenter),
   ere(impotentere),
   no_e(impotent),
   st(impotentst),
   ste(impotentste)],nonadv,[],[]).

a([e(impulsieve),
   er(impulsiever),
   ere(impulsievere),
   no_e(impulsief),
   st(impulsiefst),
   ste(impulsiefste)],adv,[],[]).

%% ik ben er voor in
a([pred(in)],nonadv,[pp(voor)],[]).

a([pred([in,contact])],padv,
  [pp(met)],[]).

a([pred([in,de,maak])],nonadv,[],[]).

a([pred([in,de,war])],padv,[],[]).

a([pred([in,duigen])],nonadv,[],[]).

a([pred([in,dubio])],padv,
  [subject_sbar,
   object_sbar],[]).

a([pred([in,elkaar]),
   pred(inelkaar)],nonadv,[],[]).

a([pred([in,mekaar])],nonadv,[],[]).

a([pred([in,gebruik])],padv,[],[]).

a([pred([in,productie])],nonadv,[],[]).

a([pred([in,de,weer])],padv,
  [object_vp],[]).

%% Niet in geschil is dat ..
a([pred([in,geschil])],nonadv,[subject_sbar],[]).

a([pred([in,het,oog])],nonadv,[],[]). %% alleen lexicaal geselecteerd?

a([pred([in,het,spel])],nonadv,[],[]).

a([pred([in,kannen,en,kruiken])],nonadv,[],[]).

a([pred([in,orde])],nonadv,[],[]).

a([pred([in,staat])],padv,
  [object_vp,
   er_pp_vp(tot),
   pp(tot),
   pp(van)],[]).

a([pred([in,strijd])],adv,
  [pp(met)],[]).

a([pred([in,tel])],nonadv,[],[]).

a([pred([in,verbinding])],padv,
  [pp(met)],[]).

a([both([in,vivo])],adv,[],[]).

a([pred([in,zak,en,as])],adv,[],[]).

a([pred([in,zoverre])],adv,[],[]).

%% sterk in zwang
a([pred([in,zwang])],padv,[],[]).

a([e(inactieve),
   no_e(inactief)],nonadv,[],[]).

a([e(inadekwate),
   e(inadequate),
   er(inadekwater),
   er(inadequater),
   ere(inadekwatere),
   ere(inadequatere),
   no_e(inadekwaat),
   no_e(inadequaat),
   st(inadekwaatst),
   st(inadequaatst),
   ste(inadekwaatste),
   ste(inadequaatste)],nonadv,[],[]).

a([ge_both(inbegrepen)],nonadv,
  [transitive,
   pp(bij)],[]).

a([e(incidentele),
   no_e(incidenteel)],adv,[],[]).

a([pred(incluis)],adv,[transitive],[]).

a([e(inclusieve),
   no_e(inclusief)],adv,[transitive],[]).

a([both(incognito)],padv,[],[]).

a([both(incrowd)],nonadv,[],[]).

a([e(incongruente),
   no_e(incongruent)],nonadv,[],[]).

a([e(inconsequente),
   e(inkonsekwente),
   er(inconsequenter),
   er(inkonsekwenter),
   ere(inconsequentere),
   ere(inkonsekwentere),
   no_e(inconsequent),
   no_e(inkonsekwent),
   st(inconsequentst),
   st(inkonsekwentst),
   ste(inconsequentste),
   ste(inkonsekwentste)],adv,[],[]).

a([e(incourante),
   no_e(incourant)],nonadv,[],[]).

a([pred(indachtig)],padv,[transitive],[]).

a([e(indicatieve),
   e(indikatieve),
   no_e(indicatief),
   no_e(indikatief)],nonadv,[],[]).

a([e(indirecte),
   e(indirekte),
   er(indirecter),
   er(indirekter),
   ere(indirectere),
   ere(indirektere),
   no_e(indirect),
   no_e(indirekt),
   st(indirectst),
   st(indirektst),
   ste(indirectste),
   ste(indirektste)],adv,[],[]).

a([e(individualistische),
   er(individualistischer),
   ere(individualistischere),
   no_e(individualistisch),
   st(individualistischt),
   ste(individualistischte)],adv,[],[]).

a([e(individuele),
   er(individueler),
   ere(individuelere),
   no_e(individueel),
   st(individueelst),
   ste(individueelste)],adv,[],[]).

a([both(indoor)],adv,[],[]).

a([end(indraaiend),
   ende(indraaiende)],padv,[],[]).

a([e(indringende),
   er(indringender),
   ere(indringendere),
   no_e(indringend),
   st(indringendst),
   ste(indringendste)],adv,[],[]).

a([e(indrukwekkende),
   er(indrukwekkender),
   ere(indrukwekkendere),
   no_e(indrukwekkend),
   st(indrukwekkendst),
   ste(indrukwekkendste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(induceerbare),
   no_e(induceerbaar)],adv,[],[]).

a([e(inductieve),
   e(induktieve),
   er(inductiever),
   er(induktiever),
   ere(inductievere),
   ere(induktievere),
   no_e(inductief),
   no_e(induktief),
   st(inductiefst),
   st(induktiefst),
   ste(inductiefste),
   ste(induktiefste)],adv,[],[]).

a([e(industriële),
   no_e(industrieel)],adv,[],[]).

a([both(ineengedoken),
   er(ineengedokener),
   ere(ineengedokenere),
   st(ineengedokenst),
   ste(ineengedokenste)],padv,[],[]).

a([ge_both(ineengekrompen)],padv,[],[]).

a([both(ingeengeslagen)],padv,[],[]).

a([ge_e(ineengestorte),
   ge_no_e(ineengestort)],adv,[],[]).

a([ge_e(ineengezakte),
   ge_no_e(ineengezakt)],adv,[],[]).

a([e(inefficiënte),
   er(inefficiënter),
   ere(inefficiëntere),
   no_e(inefficiënt),
   st(inefficiëntst),
   ste(inefficiëntste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(inerte),
   no_e(inert)],adv,[],[]).

a([e(infantiele),
   er(infantieler),
   ere(infantielere),
   no_e(infantiel),
   st(infantielst),
   ste(infantielste)],adv,[],[]).

a([e(inferieure),
   er(inferieurder),
   ere(inferieurdere),
   no_e(inferieur),
   st(inferieurst),
   ste(inferieurste)],nonadv,[],[]).

a([e(informatieve),
   er(informatiever),
   ere(informatievere),
   no_e(informatief),
   st(informatiefst),
   ste(informatiefste)],adv,[],[]).

a([e(informele),
   er(informeler),
   ere(informelere),
   no_e(informeel),
   st(informeelst),
   ste(informeelste)],adv,[],[]).

a([e(infrarode),
   no_e(infrarood)],nonadv,[],[]).

a([e(infrastructurele),
   no_e(infrastructureel)],nonadv,[],[]).

a([ge_e(ingeademde),
   ge_no_e(ingeademd)],adv,[],[]).

a([both(ingebakken)],nonadv,[],[]).

a([ge_e(ingebedde),
   ge_no_e(ingebed)],adv,[],[]).

a([ge_e(ingebeelde),
   ge_no_e(ingebeeld)],adv,[],[]).

a([ge_both(ingeblazen)],adv,[],[]).

a([ge_e(ingeblikte),
   ge_no_e(ingeblikt)],adv,[],[]).

a([ge_e(ingeboete),
   ge_no_e(ingeboet)],adv,[],[]).

a([ge_e(ingeboezemde),
   ge_no_e(ingeboezemd)],adv,[],[]).

a([ge_both(ingebonden)],nonadv,[],[]).

a([both(ingeboren)],nonadv,[],[]).

a([ge_e(ingebouwde),
   ge_no_e(ingebouwd)],adv,[],[]).

a([ge_e(ingebrachte),
   ge_no_e(ingebracht)],adv,[],[]).

a([ge_both(ingebroken)],adv,[],[]).

a([ge_e(ingeburgerde),
   er(ingeburgerder),
   ere(ingeburgerdere),
   ge_no_e(ingeburgerd),
   st(ingeburgerdst),
   ste(ingeburgerdste)],adv,[],[]).

a([ge_e(ingecalculeerde),
   ge_no_e(ingecalculeerd)],nonadv,[],[]).

a([ge_e(ingedaalde),
   ge_no_e(ingedaald)],adv,[],[]).

a([ge_e(ingedachte),
   ge_no_e(ingedacht)],adv,[],[]).

a([ge_e(ingedeelde),
   ge_no_e(ingedeeld)],adv,[],[]).

a([ge_e(ingedeukte),
   ge_no_e(ingedeukt)],adv,[],[]).

a([ge_e(ingediende),
   ge_no_e(ingediend)],adv,[],[]).

a([ge_e(ingedijkte),
   ge_no_e(ingedijkt)],adv,[],[]).

a([ge_e(ingedikte),
   ge_no_e(ingedikt)],adv,[],[]).

a([ge_both(ingedoken)],adv,[],[]).

a([ge_both(ingedrongen)],adv,[],[]).

a([ge_e(ingedruiste),
   ge_no_e(ingedruist)],adv,[],[]).

a([ge_e(ingedrukte),
   ge_no_e(ingedrukt)],adv,[],[]).

a([ge_e(ingedutte),
   ge_no_e(ingedut)],nonadv,[],[]).

a([ge_e(ingefluisterde),
   ge_no_e(ingefluisterd)],adv,[],[]).

a([ge_e(ingegane),
   ge_no_e(ingegaan)],adv,[],[]).

a([ge_both(ingegeven)],adv,[],[]).

a([ge_e(ingegooide),
   ge_no_e(ingegooid)],adv,[],[]).

a([ge_e(ingegroeide),
   ge_no_e(ingegroeid)],adv,[],[]).

a([ge_both(ingegoten)],padv,[],[]).

a([ge_both(ingegraven)],adv,[],[]).

a([ge_both(ingegrepen)],adv,[],[]).

a([ge_e(ingehaakte),
   ge_no_e(ingehaakt)],adv,[],[]).

a([ge_e(ingehaalde),
   ge_no_e(ingehaald)],adv,[],[]).

a([ge_both(ingehouden)],adv,[],[]).

a([ge_e(ingehuldigde),
   ge_no_e(ingehuldigd)],padv,[],[]).

a([ge_e(ingehuurde),
   ge_no_e(ingehuurd)],adv,[],[]).

a([ge_e(ingejaagde),
   ge_no_e(ingejaagd)],adv,[],[]).

a([ge_e(ingekaderde),
   ge_no_e(ingekaderd)],adv,[],[]).

a([ge_e(ingekapselde),
   ge_no_e(ingekapseld)],adv,[],[]).

a([ge_e(ingekeerde),
   ge_no_e(ingekeerd)],padv,[],[]).

a([ge_both(ingekeken)],adv,[],[]).

a([ge_e(ingekerfte),
   ge_no_e(ingekerft)],padv,[],[]).

a([ge_e(ingeklapte),
   ge_no_e(ingeklapt)],padv,[],[]).

a([ge_e(ingeklemde),
   ge_no_e(ingeklemd)],adv,[],[]).

a([ge_e(ingekleurde),
   ge_no_e(ingekleurd)],padv,[ap_pred],[]).

a([ge_e(ingeknelde),
   ge_no_e(ingekneld)],padv,[],[]).

a([ge_e(ingekochte),
   ge_no_e(ingekocht)],adv,[],[]).

a([ge_both(ingekomen)],adv,[],[]).

a([ge_e(ingekookte),
   ge_no_e(ingekookt)],padv,[],[]).

a([ge_e(ingekorte),
   ge_no_e(ingekort)],adv,[],[]).

a([ge_both(ingekorven)],nonadv,[],[]).  % vleermuizen

a([ge_e(ingekraste),
   ge_no_e(ingekrast)],padv,[],[]).

a([ge_both(ingekrompen)],adv,[],[]).

a([ge_e(ingekuilde),
   ge_no_e(ingekuild)],padv,[],[]).

a([ge_e(ingekwartierde),
   ge_no_e(ingekwartierd)],adv,[],[]).

a([ge_both(ingeladen)],adv,[],[]).

a([ge_e(ingelaste),
   ge_no_e(ingelast)],adv,[],[]).

a([ge_both(ingelaten)],adv,[],[]).

a([e(gelauwerde),
   no_e(gelauwerd)],padv,[],[]).

a([ge_e(ingeleefde),
   ge_no_e(ingeleefd)],adv,[],[]).

a([ge_e(ingelegde),
   ge_no_e(ingelegd)],adv,[],[]).

a([ge_e(ingeleide),
   ge_no_e(ingeleid)],adv,[],[]).

a([ge_e(ingeleverde),
   ge_no_e(ingeleverd)],adv,[],[]).

a([ge_e(ingelichte),
   ge_no_e(ingelicht)],adv,[],[]).

a([ge_e(ingelijfde),
   ge_no_e(ingelijfd)],adv,[],[]).

a([ge_e(ingelijste),
   ge_no_e(ingelijst)],padv,[],[]).

a([ge_e(ingelogde),
   ge_no_e(ingelogd)],padv,[],[]).

a([ge_both(ingelopen)],adv,[],[]).

a([ge_e(ingeluide),
   ge_no_e(ingeluid)],adv,[],[]).

a([ge_e(ingenaaide),
   ge_no_e(ingenaaid)],padv,[],[]).

a([e(ingenieuze),
   er(ingenieuzer),
   ere(ingenieuzere),
   no_e(ingenieus),
   st(ingenieust),
   ste(ingenieuste)],adv,[],[]).

a([ge_both(ingenomen),
   er(ingenomener),
   ere(ingenomenere),
   st(ingenomenst),
   ste(ingenomenste)],adv,
  [pp(met),
   er_pp_sbar(met)],[]).

a([ge_e(ingepakte),
   ge_no_e(ingepakt)],adv,[],[]).

a([ge_e(ingepalmde),
   ge_no_e(ingepalmd)],adv,[],[]).

a([ge_e(ingepompte),
   ge_no_e(ingepompt)],padv,[],[]).

a([ge_e(ingepaste),
   ge_no_e(ingepast)],adv,[],[]).

a([ge_e(ingeperkte),
   ge_no_e(ingeperkt)],adv,[],[]).

a([ge_e(ingepikte),
   ge_no_e(ingepikt)],adv,[],[]).

a([ge_e(ingeplante),
   ge_no_e(ingeplant)],padv,[],[]).

a([ge_e(ingeplugde),
   ge_no_e(ingeplugd)],padv,[],[]).

a([ge_e(ingeprente),
   ge_no_e(ingeprent)],adv,[],[]).

a([ge_both(ingereden)],adv,[],[]).

a([ge_e(ingerekende),
   ge_no_e(ingerekend)],adv,[],[]).

a([ge_e(ingerichte),
   ge_no_e(ingericht)],adv,[],[]).

a([ge_both(ingeroepen)],adv,[],[]).

a([ge_e(ingeruilde),
   ge_no_e(ingeruild)],adv,[],[]).

a([ge_e(ingeruimde),
   ge_no_e(ingeruimd)],adv,[],[]).

a([ge_e(ingeschaalde),
   ge_no_e(igeschaald)],nonadv,[],[]).

a([ge_e(ingeschakelde),
   ge_no_e(ingeschakeld)],adv,[],[]).

a([ge_e(ingeschatte),
   ge_no_e(ingeschat)],adv,[],[]).

a([ge_e(ingescheepte),
   ge_no_e(ingescheept)],adv,[],[]).

a([ge_e(ingescheurde),
   ge_no_e(ingescheurd)],padv,[],[]).

a([ge_both(ingeschonken)],adv,[],[]).

a([ge_both(ingeschoten)],adv,[],[]).

a([ge_both(ingeschreven)],adv,
  [pp(aan),
   pp(bij),
   pp(in),
   pp(voor)],[]).

a([ge_both(ingeslagen)],adv,[],[]).

a([ge_both(ingeslapen)],adv,[],[]).

a([ge_e(ingeslikte),
   ge_no_e(ingeslikt)],adv,[],[]).

a([ge_both(ingeslopen)],adv,[],[]).

a([ge_both(ingesloten)],adv,[],[]).

a([ge_e(ingesluimerde),
   ge_no_e(ingesluimerd)],nonadv,[],[]).

a([ge_e(ingesmeerde),
   ge_no_e(ingesmeerd)],adv,[],[]).

a([ge_both(ingesneden)],adv,[],[]).

a([ge_e(ingesneeuwde),
   ge_no_e(ingesneeuwd)],padv,[],[]).

a([ge_both(ingespannen),
   er(ingespannener),
   ere(ingespannenere),
   st(ingespannenst),
   ste(ingespannenste)],adv,[],[]).

a([ge_e(ingespeelde),
   ge_no_e(ingespeeld)],adv,[],[]).

a([ge_both(ingespoten)],adv,[],[]).

a([ge_both(ingesproken)],adv,[],[]).

a([ge_both(ingesprongen)],adv,[],[]).

a([ge_e(ingestelde),
   ge_no_e(ingesteld)],adv,[],[]).

a([ge_both(ingestoken)],adv,[],[]).

a([ge_e(ingestopte),
   ge_no_e(ingestopt)],adv,[],[]).

a([ge_e(ingestorte),
   ge_no_e(ingestort)],adv,[],[]).

a([ge_e(ingestrooide),
   ge_no_e(ingestrooid)],padv,[],[]).

a([ge_e(ingestudeerde),
   ge_no_e(ingestudeerd)],adv,[],[]).

a([ge_e(ingestuurde),
   ge_no_e(ingestuurd)],adv,[],[]).

a([ge_both(ingetogen),
   er(ingetogener),
   ere(ingetogenere),
   st(ingetogenst),
   ste(ingetogenste)],adv,[],[]).

a([ge_e(ingetrapte),
   ge_no_e(ingetrapt)],adv,[],[]).

a([ge_both(ingetreden)],adv,[],[]).

a([ge_both(ingetrokken)],adv,[],[]).

a([ge_e(ingesukkelde),ge_no_e(ingesukkeld)],nonadv,[],[]).

a([ge_both(ingevallen)],adv,[],[]).

a([ge_both(ingevangen)],adv,[],[]).

a([ge_e(ingevette),
   ge_no_e(ingevet)],adv,[],[]).

a([ge_e(ingevoegde),
   ge_no_e(ingevoegd)],adv,[],[]).

a([ge_e(ingevoerde),
   ge_no_e(ingevoerd)],adv,
  [pp(bij),
   pp(door)],[]).

a([ge_both(ingevlochten)],padv,[],[]).

a([ge_both(ingevlogen)],padv,[],[]).

a([ge_both(ingevroren)],nonadv,[],[]).

a([ge_e(ingevulde),
   ge_no_e(ingevuld)],adv,[],[]).

a([ge_e(ingewerkte),
   ge_no_e(ingewerkt)],adv,[],[]).

a([ge_both(ingeweven)],padv,[],[]).

a([ge_e(ingewijde),
   ge_no_e(ingewijd)],adv,[],[]).

a([e(ingewikkelde),
   er(ingewikkelder),
   ere(ingewikkeldere),
   no_e(ingewikkeld),
   st(ingewikkeldst),
   ste(ingewikkeldste)],adv,
  [subject_vp,
   subject_sbar,
   pp(door),
   pp(vanwege),
   pp(voor)],[]).

a([ge_e(ingewilligde),
   ge_no_e(ingewilligd)],adv,[],[]).

a([ge_both(ingewonnen)],adv,[],[]).

a([ge_both(ingeworpen)],nonadv,[],[]).

a([ge_both(ingewoven)],padv,[],[]).

a([ge_both(ingewreven)],adv,[],[]).

a([ge_e(ingezakte),
   ge_no_e(ingezakt)],adv,[],[]).

a([ge_e(ingezamelde),
   ge_no_e(ingezameld)],adv,[],[]).

a([ge_e(ingezette),
   ge_no_e(ingezet)],adv,
  [pp(bij),
   pp(door),
   pp(op),
   pp(tegen),
   pp(voor)],[]).

a([ge_both(ingezeten)],adv,[],[]).

a([ge_no_e(ingezien),
   ge_e(ingeziene)],adv,[],[]).

a([ge_both(ingezogen)],nonadv,[],[]).

a([ge_both(ingezonden)],adv,
  [pp(door)],[]).

a([ge_e(ingeënte),
   ge_no_e(ingeënt)],adv,[],[]).

a([e(ingrijpende),
   er(ingrijpender),
   ere(ingrijpendere),
   no_e(ingrijpend),
   st(ingrijpendst),
   ste(ingrijpendste)],adv,
  [subject_vp],[diep]).

a([e(inheemse),
   no_e(inheems)],adv,[],[]).

a([e(inherente),
   no_e(inherent)],nonadv,
  [pp(aan),
   subject_sbar],[]).

a([e(inhoudelijke),
   er(inhoudelijker),
   ere(inhoudelijkere),
   no_e(inhoudelijk),
   st(inhoudelijkst),
   ste(inhoudelijkste)],adv,[],[]).

a([e(inhumane),
   er(inhumaner),
   ere(inhumanere),
   no_e(inhumaan),
   st(inhumaanst),
   ste(inhumaanste)],adv,[],[]).

a([e(inlandse),
   no_e(inlands)],nonadv,[],[]).

a([end(inlevend),
   ende(inlevende)],both,
  [refl,
   refl_pp(in)],[]).

a([e(innemende),
   er(innemender),
   ere(innemendere),
   no_e(innemend),
   st(innemendst),
   ste(innemendste)],adv,[],[]).

a([e(innerlijke),
   no_e(innerlijk)],adv,[],[]).

a([e(innige),
   er(inniger),
   ere(innigere),
   no_e(innig),
   st(innigst),
   ste(innigste)],adv,[],[]).

a([e(innovatieve),
   no_e(innovatief)],adv,[],[]).

a([e(inpasbare),
   no_e(inpasbaar)],padv,[],[]).

a([e(inschikkelijke),
   er(inschikkelijker),
   ere(inschikkelijkere),
   no_e(inschikkelijk),
   st(inschikkelijkst),
   ste(inschikkelijkste)],adv,[],[]).

a([ende(inschuivende),
   end(inschuivend)],nonadv,[],[]).

a([e(instabiele),
   er(instabieler),
   ere(instabielere),
   no_e(instabiel),
   st(instabielst),
   ste(instabielste)],adv,[],[]).

a([both(instant)],nonadv,[],[]).

a([e(instemmende),
   er(instemmender),
   ere(instemmendere),
   no_e(instemmend),
   st(instemmendst),
   ste(instemmendste)],padv,[],[]).

a([e(instinctieve),
   e(instinktieve),
   er(instinctiever),
   er(instinktiever),
   ere(instinctievere),
   ere(instinktievere),
   no_e(instinctief),
   no_e(instinktief),
   st(instinctiefst),
   st(instinktiefst),
   ste(instinctiefste),
   ste(instinktiefste)],adv,[],[]).

a([e(instinctmatige),
   e(instinktmatige),
   er(instinctmatiger),
   er(instinktmatiger),
   ere(instinctmatigere),
   ere(instinktmatigere),
   no_e(instinctmatig),
   no_e(instinktmatig),
   st(instinctmatigst),
   st(instinktmatigst),
   ste(instinctmatigste),
   ste(instinktmatigste)],adv,[],[]).

a([e(institutionele),
   no_e(institutioneel)],adv,[],[inter]).

a([ende(instormende),
   end(instormend)],adv,[],[]).

a([e(instrumentale),
   no_e(instrumentaal)],adv,[],[]).

a([e(intacte),
   e(intakte),
   er(intacter),
   er(intakter),
   ere(intactere),
   ere(intaktere),
   no_e(intact),
   no_e(intakt),
   st(intactst),
   st(intaktst),
   ste(intactste),
   ste(intaktste)],padv,[],[]).

a([e(integere),
   er(integerder),
   ere(integerdere),
   no_e(integer),
   st(integerst),
   ste(integerste)],adv,[],[]).

a([e(integrale),
   no_e(integraal)],adv,[],[]).

a([e(intellectuele),
   e(intellektuele),
   er(intellectueler),
   er(intellektueler),
   ere(intellectuelere),
   ere(intellektuelere),
   no_e(intellectueel),
   no_e(intellektueel),
   st(intellectueelst),
   st(intellektueelst),
   ste(intellectueelste),
   ste(intellektueelste)],adv,[],[]).

a([e(intelligente),
   er(intelligenter),
   ere(intelligentere),
   no_e(intelligent),
   st(intelligentst),
   ste(intelligentste)],adv,[],[]).

a([e(intense),
   er(intenser),
   ere(intensere),
   no_e(intens),
   st(intenst),
   ste(intenste)],adv,[],[]).

a([e(intensieve),
   er(intensiever),
   ere(intensievere),
   no_e(intensief),
   st(intensiefst),
   ste(intensiefste)],adv,[],[]).

a([e(intentionele),
   no_e(intentioneel)],adv,[],[]).

a([e(interactieve),
   e(interaktieve),
   no_e(interactief),
   no_e(interaktief)],adv,[],[]).

a([e(intercommunale),
   no_e(intercommunaal)],adv,[],[]).

a([e(intercontinentale),
   e(interkontinentale),
   no_e(intercontinentaal),
   no_e(interkontinentaal)],adv,[],[]).

a([e(interdepartementale),
   no_e(interdepartementaal)],adv,[],[]).

a([e(interdisciplinaire),
   no_e(interdisciplinair)],adv,[],[]).

a([e(interessante),
   er(interessanter),
   ere(interessantere),
   no_e(interessant),
   st(interessantst),
   ste(interessantste)],adv,
  [subject_sbar,
   subject_vp_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(intergouvernementele),
   no_e(intergouvernementeel)],nonadv,[],[]).

a([e(intermediaire),
   no_e(intermediair)],nonadv,[],[]).

a([e(intermenselijke),
   no_e(intermenselijk)],nonadv,[],[]).

a([e(interne),
   no_e(intern)],adv,[],[s(bedrijf)]).

a([e(internationale),
   no_e(internationaal)],adv,[],[]).

a([e(interplanetaire),
   no_e(interplanetair)],nonadv,[],[]).

a([e(interstellaire),
   no_e(interstellair)],nonadv,[],[]).

a([e(intersubjectieve),
   e(intersubjektieve),
   no_e(intersubjectief),
   no_e(intersubjektief)],nonadv,[],[]).

a([e(intieme),
   er(intiemer),
   ere(intiemere),
   no_e(intiem),
   st(intiemst),
   ste(intiemste)],adv,[],[]).

a([e(intraveneuze),
   no_e(intraveneus)],adv,[],[]).

a([e(intrinsieke),
   no_e(intrinsiek)],adv,[],[]).

a([e(introverte),
   er(introverter),
   ere(introvertere),
   no_e(introvert),
   st(introvertst),
   ste(introvertste)],padv,[],[]).

a([e(intuïtieve),
   er(intuïtiever),
   ere(intuïtievere),
   no_e(intuïtief),
   st(intuïtiefst),
   ste(intuïtiefste)],adv,[],[]).

a([both(invalide)],nonadv,[],[]).

a([e(inventieve),
   er(inventiever),
   ere(inventievere),
   no_e(inventief),
   st(inventiefst),
   ste(inventiefste)],both,[],[]).

a([e(inverse),
   no_e(invers)],nonadv,[],[]).

a([e(invloedrijke),
   er(invloedrijker),
   ere(invloedrijkere),
   no_e(invloedrijk),
   st(invloedrijkst),
   ste(invloedrijkste)],nonadv,[],[]).

a([e(inwendige),
   no_e(inwendig)],adv,[],[]).

a([e(inwisselbare),
   no_e(inwisselbaar)],adv,[],[]).

a([e(inzetbare),
   no_e(inzetbaar)],nonadv,[],[]).

a([e(inzichtelijke),
   er(inzichtelijker),
   ere(inzichtelijkere),
   no_e(inzichtelijk),
   st(inzichtelijkst),
   ste(inzichtelijkste)],nonadv,[subject_sbar],[]).

a([e(ironische),
   er(ironischer),
   ere(ironischere),
   no_e(ironisch),
   st(ironischt),
   ste(ironischte)],padv,
  [subject_sbar,
   subject_vp],[]).

a([e(irrationele),
   er(irrationeler),
   ere(irrationelere),
   no_e(irrationeel),
   st(irrationeelst),
   ste(irrationeelste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(irreële),
   er(irreëler),
   ere(irreëlere),
   no_e(irreëel),
   st(irreëelst),
   ste(irreëelste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(irrelevante),
   er(irrelevanter),
   ere(irrelevantere),
   no_e(irrelevant),
   st(irrelevantst),
   ste(irrelevantste)],nonadv,
  [subject_sbar],[]).

a([e(irritante),
   er(irritanter),
   ere(irritantere),
   no_e(irritant),
   st(irritantst),
   ste(irritantste)],adv,
  [subject_sbar],[]).

a([e(islamitische),
   no_e(islamitisch),
   e('Islamitische'),
   no_e('Islamitisch')],adv,[],[]).

a([e(islamistische),
   no_e(islamistisch),
   e('Islamistische'),
   no_e('Islamistisch')],adv,[],[]).

a([stof(ivoren)],nonadv,[],[]).

a([e(jaarlijkse),
   no_e(jaarlijks)],adv,[],[half]).

a([both(jaars)],nonadv,[],[]).

a([e(jachtige),
   er(jachtiger),
   ere(jachtigere),
   no_e(jachtig),
   st(jachtigst),
   ste(jachtigste)],adv,[],[]).

a([e(jaloerse),
   er(jaloerser),
   ere(jaloersere),
   no_e(jaloers),
   st(jaloerst),
   ste(jaloerste)],adv,
  [er_pp_sbar(op),
   object_sbar,
   pp(op)],[]).

a([no_e(jammer),
   e(jammere)],nonadv,
  [subject_sbar,
   subject_vp,
   pp(van)],[]).

a([e(jammerlijke),
   er(jammerlijker),
   ere(jammerlijkere),
   no_e(jammerlijk),
   st(jammerlijkst),
   ste(jammerlijkste)],adv,[],[]).

a([e(jarige),
   no_e(jarig)],nonadv,[],[]).

a([both(jazzy)],padv,[],[]).

a([pred([je,van,het]),
   pred([je,van,hét]),
   pred([jé,van,hét]),
   pred([jé,van,het])],nonadv,[],[]).

a([e(jeugdige),
   er(jeugdiger),
   ere(jeugdigere),
   no_e(jeugdig),
   st(jeugdigst),
   ste(jeugdigste)],adv,[],[]).

a([e(jiddische),
   no_e(jiddisch)],nonadv,[],[]).

a([e(jihadistische),
   no_e(jihadistisch)],nonadv,[],[]).

a([e(jolige),
   er(joliger),
   ere(joligere),
   no_e(jolig),
   st(joligst),
   ste(joligste)],adv,[],[]).

a([e(jonge),
   er(jonger),
   ere(jongere),
   no_e(jong),
   st(jongst),
   ste(jongste)],padv,[],[piep]).

a([both(jongstleden)],nonadv,[],[]).

a([e(jongensachtige),
   er(jongensachtiger),
   ere(jongensachtigere),
   no_e(jongensachtig),
   st(jongensachtigst),
   ste(jongensachtigste)],adv,[],[]).

a([e(journalistieke),
   no_e(journalistiek),
   ere(journalistiekere),
   er(journalistieker)],adv,[],[]).

a([e(joviale),
   er(jovialer),
   ere(jovialere),
   no_e(joviaal),
   st(joviaalst),
   ste(joviaalste)],adv,[],[]).

a([e(joyeuze),
   no_e(joyeus)],adv,[],[]).

a([e(juiste),
   er(juister),
   ere(juistere),
   no_e(juist)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(junior),both(junioren)],nonadv,[],[]).

a([e(juridische),
   no_e(juridisch)],adv,[],[]).

a([e(justitiële),
   no_e(justitieel)],adv,[],[]).

a([stof(juten)],nonadv,[],[]).

a([e(kale),
   er(kaler),
   ere(kalere),
   no_e(kaal),
   st(kaalst),
   ste(kaalste)],adv,[],[]).

a([ge_both(kaalgeschoren)],adv,[],[]).

a([e(kaarsrechte),
   er(kaarsrechter),
   ere(kaarsrechtere),
   no_e(kaarsrecht),
   st(kaarsrechtst),
   ste(kaarsrechtste)],adv,[],[]).

a([stof(kaki)],nonadv,[],[]).

a([stof(kalksteen),
   stof(kalkstenen)],nonadv,[],[]).

a([e(kalme),
   er(kalmer),
   ere(kalmere),
   no_e(kalm),
   st(kalmst),
   ste(kalmste)],adv,[],[]).

a([pred(kalmpjes)],adv,[],[]).

a([pred([kalmpjes,aan])],adv,[],[]).

a([e(kameraadschappelijke),
   er(kameraadschappelijker),
   ere(kameraadschappelijkere),
   no_e(kameraadschappelijk),
   st(kameraadschappelijkst),
   ste(kameraadschappelijkste)],adv,[],[]).

a([e(kankerverwekkende),
   no_e(kankerverwekkend)],nonadv,[],[]).

a([e(kansarme),
   no_e(kansarm)],adv,[],[]).

a([e(kansloze),
   no_e(kansloos)],adv,[],[]).

a([e(kansrijke),
   no_e(kansrijk),
   er(kansrijker),
   ere(kansrijkere),
   st(kansrijkst),
   ste(kansrijkste)],adv,[subject_vp],[]).

a([e('kant-en-klare'),
   both('kant-en-klaar'),
   e([kant,en,klare]),
   both([kant,en,klaar])],padv,[],[]).

a([e(kantelbare),
   no_e(kantelbaar)],padv,[],[]).

a([stof(kanten)],nonadv,[],[]).

a([e(kapitale),
   no_e(kapitaal)],nonadv,[],[]).

a([e(kapitaalkrachtige),
   er(kapitaalkrachtiger),
   ere(kapitaalkrachtigere),
   no_e(kapitaalkrachtig),
   st(kapitaalkrachtigst),
   ste(kapitaalkrachtigste)],nonadv,[],[]).

a([e(kapitalistische),
   er(kapitalistischer),
   ere(kapitalistischere),
   no_e(kapitalistisch),
   st(kapitalistischt),
   ste(kapitalistischte)],nonadv,[],[]).

a([e(kapotte),
   er(kapotter),
   ere(kapottere),
   no_e(kapot),
   st(kapotst),
   ste(kapotste)],padv,
  [pp(van),
   er_pp_sbar(van)],[]).

a([ge_e(kapotgemaakte),
   ge_no_e(kapotgemaakt)],adv,[],[]).

a([ge_both(kapotgeslagen)],adv,[],[]).

a([e(karakteristieke),
   er(karakteristieker),
   ere(karakteristiekere),
   no_e(karakteristiek),
   st(karakteristiekst),
   ste(karakteristiekste)],nonadv,[],[]).

a([e(kardinale),
   no_e(kardinaal)],nonadv,[],[]).

a([e(karige),
   er(kariger),
   ere(karigere),
   no_e(karig),
   st(karigst),
   ste(karigste)],adv,[],[]).

a([stof(kartonnen)],nonadv,[],[]).

a([stof(kasjmier)],nonadv,[],[]).

a([e(kastanjebruine),
   er(kastanjebruiner),
   ere(kastanjebruinere),
   no_e(kastanjebruin),
   st(kastanjebruinst),
   ste(kastanjebruinste)],nonadv,[],[]).

a([e(katachtige),
   er(katachtiger),
   ere(katachtigere),
   no_e(katachtig),
   st(katachtigst),
   ste(katachtigste)],nonadv,[],[]).

a([e(kathaarse),
   no_e(kathaars)],nonadv,[],[]).

a([e(kathedrale),
   no_e(kathedraal)],nonadv,[],[]).

a([e(katholieke),
   e(katolieke),
   er(katholieker),
   er(katolieker),
   ere(katholiekere),
   ere(katoliekere),
   no_e(katholiek),
   no_e(katoliek),
   st(katholiekst),
   st(katoliekst),
   ste(katholiekste),
   ste(katoliekste)],both,[],[]).  % katholiek gedoopte joden

a([stof(katoenen)],nonadv,[],[]).

a([prefix(kaolo)],nonadv,[],[]).

a([e(keigoede),
   no_e(keigoed)],adv,[],[]).

a([e(keiharde),
   er(keiharder),
   ere(keihardere),
   no_e(keihard),
   st(keihardst),
   ste(keihardste)],adv,[],[]).

a([e(keizerlijke),
   er(keizerlijker),
   ere(keizerlijkere),
   no_e(keizerlijk),
   st(keizerlijkst),
   ste(keizerlijkste)],adv,[],[]).

a([e(kekke),no_e(kek)],nonadv,[],[]).

a([e(kenbare),
   er(kenbaarder),
   ere(kenbaardere),
   no_e(kenbaar),
   st(kenbaarst),
   ste(kenbaarste)],nonadv,[],[]).

a([ende(kenmerkende),
   er(kenmerkender),
   ere(kenmerkendere),
   end(kenmerkend),
   st(kenmerkendst),
   ste(kenmerkendste)],adv,
  [subject_sbar,
   transitive,
   pp(voor)],[]).

a([e(kennelijke),
   er(kennelijker),
   ere(kennelijkere),
   no_e(kennelijk),
   st(kennelijkst),
   ste(kennelijkste)],adv,[],[]).

a([e(keramieke),
   no_e(keramiek),
   stof(keramieken)],nonadv,[],[]).

a([e(keramische),
   no_e(keramisch)],nonadv,[],[]).

a([e(kerkelijke),
   er(kerkelijker),
   ere(kerkelijkere),
   no_e(kerkelijk),
   st(kerkelijkst),
   ste(kerkelijkste)],adv,[],[]).

a([e(kerkse),
   no_e(kerks)],nonadv,[],
  [h(anti),
   h(niet)]).

a([e(kernachtige),
   er(kernachtiger),
   ere(kernachtigere),
   no_e(kernachtig),
   st(kernachtigst),
   ste(kernachtigste)],adv,[],[]).

a([e(kerngezonde),
   er(kerngezonder),
   ere(kerngezondere),
   no_e(kerngezond),
   st(kerngezondst),
   ste(kerngezondste)],padv,[],[]).

a([e(kernwapenvrije),
   no_e(kernwapenvrij)],padv,[],[]).

a([e(kersverse),
   er(kersverser),
   ere(kersversere),
   no_e(kersvers),
   st(kersverst),
   ste(kersverste)],adv,[],[]).

a([e(ketterse),
   er(ketterser),
   ere(kettersere),
   no_e(ketters),
   st(ketterst),
   ste(ketterste)],adv,[],[]).

a([e(keurige),
   er(keuriger),
   ere(keurigere),
   no_e(keurig),
   st(keurigst),
   ste(keurigste)],adv,[],[]).

a([e(kiene),
   no_e(kien)],adv,[],[]).

a([e(kiese),
   no_e(kies)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(kieskeurige),
   er(kieskeuriger),
   ere(kieskeurigere),
   no_e(kieskeurig),
   st(kieskeurigst),
   ste(kieskeurigste)],adv,[],[]).

a([e(kille),
   er(killer),
   ere(killere),
   no_e(kil),
   st(kilst),
   ste(kilste)],adv,[],[]).

a([e(kinderachtige),
   er(kinderachtiger),
   ere(kinderachtigere),
   no_e(kinderachtig),
   st(kinderachtigst),
   ste(kinderachtigste)],adv,[],[]).

a([e(kinderlijke),
   er(kinderlijker),
   ere(kinderlijkere),
   no_e(kinderlijk),
   st(kinderlijkst),
   ste(kinderlijkste)],adv,[],[]).

a([e(kinderloze),
   no_e(kinderloos)],padv,[],[]).

a([e(kinderrijke),
   no_e(kinderrijk)],nonadv,[],[]).

a([e(kindse),
   er(kindser),
   ere(kindsere),
   no_e(kinds),
   st(kindst),
   ste(kindste)],nonadv,[],[]).

a([both(kinky)],adv,[],[]).

a([e(klaaglijke),
   er(klaaglijker),
   ere(klaaglijkere),
   no_e(klaaglijk),
   st(klaaglijkst),
   ste(klaaglijkste)],adv,[],[]).

a([e(klare),
   er(klaarder),
   ere(klaardere),
   no_e(klaar),
   st(klaarst),
   ste(klaarste)],adv,
  [object_vp,
   er_pp_vp(voor),
   pp(voor),
   pp(met)],
  [s(bedrijf)]).

a([e(klaarblijkelijke),
   no_e(klaarblijkelijk)],adv,[],[]).

a([ge_both(klaargekomen)],adv,[],[]).

a([ge_e(klaargelegde),
   ge_no_e(klaargelegd)],adv,[],[]).

a([ge_both(klaargelegen)],adv,[],[]).

a([ge_e(klaargemaakte),
   ge_no_e(klaargemaakt)],adv,[],[]).

a([ge_e(klaargespeelde),
   ge_no_e(klaargespeeld)],adv,[],[]).

a([ge_e(klaargestoomde),
   ge_no_e(klaargestoomd)],adv,[],[]).

a([ge_e(klaargezette),
   ge_no_e(klaargezet)],adv,[],[]).

a([e(klaarlichte),
   er(klaarlichter),
   ere(klaarlichtere),
   no_e(klaarlicht),
   st(klaarlichtst),
   ste(klaarlichtste)],nonadv,[],[]).

a([e(klaarwakkere),
   no_e(klaarwakker)],padv,[],[]).

a([ende(klagende),
   er(klagender),
   ere(klagendere),
   end(klagend),
   st(klagendst),
   ste(klagendste)],padv,[],[]).

a([e(klakkeloze),
   er(klakkelozer),
   ere(klakkelozere),
   no_e(klakkeloos),
   st(klakkeloost),
   ste(klakkelooste)],adv,[],[]).

a([e(klamme),
   er(klammer),
   ere(klammere),
   no_e(klam),
   st(klamst),
   ste(klamste)],adv,[],[]).

a([e(klassieke),
   er(klassieker),
   ere(klassiekere),
   no_e(klassiek),
   st(klassiekst),
   ste(klassiekste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(klassikale),
   no_e(klassikaal)],adv,[],[]).

a([e(kleffe),
   er(kleffer),
   ere(kleffere),
   no_e(klef),
   st(klefst),
   ste(klefste)],nonadv,[],[]).

a([e(kleine),
   er(kleiner),
   ere(kleinere),
   no_e(klein),
   st(kleinst),
   ste(kleinste)],adv,
  [pp(in)],[piep]).

a([e(kleinburgerlijke),
   er(kleinburgerlijker),
   ere(kleinburgerlijkere),
   no_e(kleinburgerlijk),
   st(kleinburgerlijkst),
   ste(kleinburgerlijkste)],nonadv,[],[]).

a([e(kleinschalige),
   er(kleinschaliger),
   ere(kleinschaligere),
   no_e(kleinschalig),
   st(kleinschaligst),
   ste(kleinschaligste)],adv,[],[]).

a([e(kleinsteedse),
   no_e(kleinsteeds)],adv,[],[]).

a([pred(kleintjes)],adv,[],[]).

a([e(kleinzielige),
   er(kleinzieliger),
   ere(kleinzieligere),
   no_e(kleinzielig),
   st(kleinzieligst),
   ste(kleinzieligste)],adv,[],[]).

a([pred(klem)],nonadv,[],[]).

a([e(klemmende),
   er(klemmender),
   ere(klemmendere),
   no_e(klemmend),
   st(klemmendst),
   ste(klemmendste)],adv,[],[]).

a([e(clericale),
   e(klerikale),
   er(clericaler),
   er(klerikaler),
   ere(clericalere),
   ere(klerikalere),
   no_e(clericaal),
   no_e(klerikaal),
   st(clericaalst),
   st(klerikaalst),
   ste(clericaalste),
   ste(klerikaalste)],nonadv,[],[]).

a([e(kletsnatte),
   er(kletsnatter),
   ere(kletsnattere),
   no_e(kletsnat),
   st(kletsnatst),
   ste(kletsnatste)],padv,[],[]).

a([e(kleurenblinde),
   no_e(kleurenblind)],padv,[],[]).

a([e(kleurige),
   er(kleuriger),
   ere(kleurigere),
   no_e(kleurig),
   st(kleurigst),
   ste(kleurigste)],adv,[],[]).

a([e(kleurloze),
   er(kleurlozer),
   ere(kleurlozere),
   no_e(kleurloos),
   st(kleurloost),
   ste(kleurlooste)],adv,[],[]).

a([e(kleurrijke),
   er(kleurrijker),
   ere(kleurrijkere),
   no_e(kleurrijk),
   st(kleurrijkst),
   ste(kleurrijkste)],padv,
  [subject_vp],[]).

a([e(kleverige),
   er(kleveriger),
   ere(kleverigere),
   no_e(kleverig),
   st(kleverigst),
   ste(kleverigste)],nonadv,[],[]).

a([e(klimatologische),
   no_e(klimatologisch)],nonadv,[],[]).

a([e(klinische),
   no_e(klinisch)],adv,[],[]).

a([pred([klip,en,klaar]),
   pred('klip-en-klaar')],adv,[subject_sbar],[]).

a([e(kloeke),
   er(kloeker),
   ere(kloekere),
   no_e(kloek),
   st(kloekst),
   ste(kloekste)],adv,[],[]).

a([both(klote),
   both(kloten)],adv,[],[]).

a([e(knalrode),
   er(knalroder),
   ere(knalrodere),
   no_e(knalrood),
   st(knalroodst),
   ste(knalroodste)],nonadv,[],[]).

a([e(knappe),
   er(knapper),
   ere(knappere),
   no_e(knap),
   st(knapst),
   ste(knapste)],adv,
  [object_vp,
   subject_sbar],[]).

a([e(knapperige),
   er(knapperiger),
   ere(knapperigere),
   no_e(knapperig),
   st(knapperigst),
   ste(knapperigste)],adv,[],[]).

a([pred(knetter)],padv,[],[]).

a([e(kneuterige),
   no_e(kneuterig)],nonadv,[],[]).

a([pred([knock,out]),
   prefix([knock,out])],adv,[],[]).

a([e(knoestige),
   er(knoestiger),
   ere(knoestigere),
   no_e(knoestig),
   st(knoestigst),
   ste(knoestigste)],nonadv,[],[]).

a([e(knokige),
   er(knokiger),
   ere(knokigere),
   no_e(knokig),
   st(knokigst),
   ste(knokigste)],nonadv,[],[]).

a([e(knorrige),
   er(knorriger),
   ere(knorrigere),
   no_e(knorrig),
   st(knorrigst),
   ste(knorrigste)],adv,[],[]).

a([e(knullige),
   no_e(knullig)],adv,[],[]).

a([e(knusse),
   er(knusser),
   ere(knussere),
   no_e(knus),
   st(knust),
   ste(knuste)],adv,[],[]).

a([e(koddige),
   er(koddiger),
   ere(koddigere),
   no_e(koddig),
   st(koddigst),
   ste(koddigste)],adv,[],[]).

a([e(koele),
   er(koeler),
   ere(koelere),
   no_e(koel),
   st(koelst),
   ste(koelste)],adv,[],[]).

a([e(koelbloedige),
   er(koelbloediger),
   ere(koelbloedigere),
   no_e(koelbloedig),
   st(koelbloedigst),
   ste(koelbloedigste)],adv,[],[]).

a([pred(koeltjes)],both,[],[]).

a([e(koene),
   er(koener),
   ere(koenere),
   no_e(koen),
   st(koenst),
   ste(koenste)],adv,[],[]).

a([pred(koest)],nonadv,[],[]).

a([e(kokette),
   er(koketter),
   ere(kokettere),
   no_e(koket),
   st(koketst),
   ste(koketste)],padv,[],[]).

a([e(koloniale),
   no_e(koloniaal)],adv,[],[]).

a([e(kolossale),
   er(kolossaler),
   ere(kolossalere),
   no_e(kolossaal),
   st(kolossaalst),
   ste(kolossaalste)],adv,[],[]).

a([e(komische),
   er(komischer),
   ere(komischere),
   no_e(komisch),
   st(komischt),
   ste(komischte)],adv,[subject_sbar],[]).

a([e(koninklijke),
   er(koninklijker),
   ere(koninklijkere),
   no_e(koninklijk),
   st(koninklijkst),
   ste(koninklijkste)],adv,[],[]).

a([stof(koolstof)],nonadv,[],[]).

a([e(koortsachtige),
   er(koortsachtiger),
   ere(koortsachtigere),
   no_e(koortsachtig),
   st(koortsachtigst),
   ste(koortsachtigste)],adv,[],[]).

a([e(koortsige),
   er(koortsiger),
   ere(koortsigere),
   no_e(koortsig),
   st(koortsigst),
   ste(koortsigste)],adv,[],[]).

a([e(koosjere),
   e(koshere),
   e(kosjere),
   no_e(koosjer),
   no_e(kosher),
   no_e(kosjer)   ],adv,
  [subject_sbar,
   subject_vp],[]).

a([stof(koperen)],nonadv,[],[]).

a([e(koperkleurige),
   er(koperkleuriger),
   ere(koperkleurigere),
   no_e(koperkleurig),
   st(koperkleurigst),
   ste(koperkleurigste)],nonadv,[],[]).

a([e(koppige),
   er(koppiger),
   ere(koppigere),
   no_e(koppig),
   st(koppigst),
   ste(koppigste)],adv,[],[]).

a([e(kordate),
   er(kordater),
   ere(kordatere),
   no_e(kordaat),
   st(kordaatst),
   ste(kordaatste)],adv,[],[]).

a([e(korrelige),
   er(korreliger),
   ere(korreligere),
   no_e(korrelig),
   st(korreligst),
   ste(korreligste)],nonadv,[],[]).

a([e(korte),
   er(korter),
   ere(kortere),
   no_e(kort),
   st(kortst),
   ste(kortste)],tmpadv,[],[]).

a([e(kortaffe),
   no_e(kortaf)],adv,[],[]).

a([e(kortgewiekte),
   no_e(kortgewiekt)],padv,[],[]).

a([e(kortlopende),
   no_e(kortlopend)],nonadv,[],[]).

a([e(kortstondige),
   no_e(kortstondig)],adv,[],[]).

a([e(kortzichtige),
   er(kortzichtiger),
   ere(kortzichtigere),
   no_e(kortzichtig),
   st(kortzichtigst),
   ste(kortzichtigste)],adv,[],[]).

a([e(korzelige),
   er(korzeliger),
   ere(korzeligere),
   no_e(korzelig),
   st(korzeligst),
   ste(korzeligste)],adv,[],[]).

a([e(cosmetische),
   no_e(cosmetisch)],adv,[],[]).

a([e(cosmische),
   e(kosmische),
   er(cosmischer),
   er(kosmischer),
   ere(cosmischere),
   ere(kosmischere),
   no_e(kosmisch),
   no_e(cosmisch),
   st(cosmischt),
   st(kosmischt),
   ste(cosmischte),
   ste(kosmischte)],nonadv,[],[]).

a([e(kosmologische),
   no_e(kosmologisch)],adv,[],[]).

a([e(cosmopolitische),
   e(kosmopolitische),
   er(cosmopolitischer),
   er(kosmopolitischer),
   ere(cosmopolitischere),
   ere(kosmopolitischere),
   no_e(cosmopolitisch),
   no_e(kosmopolitisch),
   st(cosmopolitischt),
   st(kosmopolitischt),
   ste(cosmopolitischte),
   ste(kosmopolitischte)],adv,[],[]).

a([e(kostbare),
   er(kostbaarder),
   ere(kostbaardere),
   no_e(kostbaar),
   st(kostbaarst),
   ste(kostbaarste)],adv,[],[]).

a([e(kostelijke),
   er(kostelijker),
   ere(kostelijkere),
   no_e(kostelijk),
   st(kostelijkst),
   ste(kostelijkste)],adv,[],[]).

a([e(kosteloze),
   no_e(kosteloos)],adv,[],[]).

a([ende(kostenbesparende),
   end(kostenbesparend)],both,[],[]).

a([ende(kostendekkende),
   end(kostendekkend)],both,[],[]).

a([e(koude),
   er(kouder),
   ere(koudere),
   no_e(koud),
   st(koudst),
   ste(koudste),
   e(kouwe)],adv,[],[ijs]).

a([e(krachtdadige),
   er(krachtdadiger),
   ere(krachtdadigere),
   no_e(krachtdadig),
   st(krachtdadigst),
   ste(krachtdadigste)],adv,[],[]).

a([e(krachteloze),
   er(krachtelozer),
   ere(krachtelozere),
   no_e(krachteloos),
   st(krachteloost),
   ste(krachtelooste)],padv,[],[]).

a([e(krachtige),
   er(krachtiger),
   ere(krachtigere),
   no_e(krachtig),
   st(krachtigst),
   ste(krachtigste)],adv,[],[daad]).

a([e(krampachtige),
   er(krampachtiger),
   ere(krampachtigere),
   no_e(krampachtig),
   st(krampachtigst),
   ste(krampachtigste)],adv,[],[]).

a([e(kranige),
   er(kraniger),
   ere(kranigere),
   no_e(kranig),
   st(kranigst),
   ste(kranigste)],adv,[],[]).

a([e(krankjoreme),
   no_e(krankjorem)],adv,[],[]).

a([e(krankzinnige),
   er(krankzinniger),
   ere(krankzinnigere),
   no_e(krankzinnig),
   st(krankzinnigst),
   ste(krankzinnigste)],adv,[],[]).

a([e(krappe),
   er(krapper),
   ere(krappere),
   no_e(krap),
   st(krapst),
   ste(krapste)],adv,[],[]).

a([e(krasse),
   er(krasser),
   ere(krassere),
   no_e(kras),
   st(krast),
   ste(kraste)],adv,[],[]).

a([e(kreupele),
   er(kreupeler),
   ere(kreupelere),
   no_e(kreupel),
   st(kreupelst),
   ste(kreupelste)],adv,[],[]).

a([e(kribbige),
   er(kribbiger),
   ere(kribbigere),
   no_e(kribbig),
   st(kribbigst),
   ste(kribbigste)],adv,[],[]).

a([pred(kriegel)],adv,[],[]).

a([both(krijgsgevangen)],nonadv,[],[]).

a([e(krijgshaftige),
   er(krijgshaftiger),
   ere(krijgshaftigere),
   no_e(krijgshaftig),
   st(krijgshaftigst),
   ste(krijgshaftigste)],adv,[],[]).

a([e(kristalheldere),
   er(kristalhelderder),
   ere(kristalhelderdere),
   no_e(kristalhelder),
   st(kristalhelderst),
   ste(kristalhelderste)],nonadv,[],[]).

a([stof(kristallen)],nonadv,[],[]).

a([e(kritieke),
   er(kritieker),
   ere(kritiekere),
   no_e(kritiek),
   st(kritiekst),
   ste(kritiekste)],nonadv,[],[]).

a([e(kritiekloze),
   no_e(kritiekloos)],adv,[],[]).

a([e(kritische),
   er(kritischer),
   ere(kritischere),
   no_e(kritisch),
   st(kritischt),
   ste(kritischte)],adv,[],[]).

a([e(krolse),
   er(krolser),
   ere(krolsere),
   no_e(krols),
   st(krolst),
   ste(krolste)],adv,[],[]).

a([e(kromme),
   er(krommer),
   ere(krommere),
   no_e(krom),
   st(kromst),
   ste(kromste)],adv,[],[]).

a([e(kronkelige),
   er(kronkeliger),
   ere(kronkeligere),
   no_e(kronkelig),
   st(kronkeligst),
   ste(kronkeligste)],nonadv,[],[]).

a([e(kruidige),
   er(kruidiger),
   ere(kruidigere),
   no_e(kruidig),
   st(kruidigst),
   ste(kruidigste)],nonadv,[],[]).

%% ijs
a([e(kruiende),
   no_e(kruiend)],nonadv,[],[]).

a([ende(kruipende),
   er(kruipender),
   ere(kruipendere),
   end(kruipend),
   st(kruipendst),
   ste(kruipendste)],padv,[],[]).

a([e(kruiselingse),
   no_e(kruiselings),
   e(kruislingse),
   no_e(kruislings)],adv,[],[]).

a([e(kubieke),
   no_e(kubiek)],nonadv,[],[]).

a([e(kuise),
   er(kuiser),
   ere(kuisere),
   no_e(kuis),
   st(kuist),
   ste(kuiste)],adv,[],[]).

a([e(kundige),
   er(kundiger),
   ere(kundigere),
   no_e(kundig),
   st(kundigst),
   ste(kundigste)],adv,[],
  [oordeel,
   l(on_oordeel,onoordeel)]).

a([e(kunstige),
   er(kunstiger),
   ere(kunstigere),
   no_e(kunstig),
   st(kunstigst),
   ste(kunstigste)],adv,[],[]).

a([e(kunstmatige),
   er(kunstmatiger),
   ere(kunstmatigere),
   no_e(kunstmatig),
   st(kunstmatigst),
   ste(kunstmatigste)],adv,[],[]).

a([both(kunststof)],nonadv,[],[]).

a([both(kunststoffen)],nonadv,[],[]).

a([e(kunstzinnige),
   er(kunstzinniger),
   ere(kunstzinnigere),
   no_e(kunstzinnig),
   st(kunstzinnigst),
   ste(kunstzinnigste)],adv,[],[]).

a([e(kurkdroge),
   er(kurkdroger),
   ere(kurkdrogere),
   no_e(kurkdroog),
   st(kurkdroogst),
   ste(kurkdroogste)],nonadv,[],[]).

a([e(kwade),
   er(kwader),er(kwaaier),
   ere(kwadere),ere(kwaaiere),
   no_e(kwaad),
   st(kwaadst),st(kwaaist),
   ste(kwaadste),ste(kwaaiste)],padv,
  [er_pp_sbar(over),
   pp(op),
   pp(over),
   object_sbar],[]).

a([e(kwaadaardige),
   er(kwaadaardiger),
   ere(kwaadaardigere),
   no_e(kwaadaardig),
   st(kwaadaardigst),
   ste(kwaadaardigste)],adv,[],[]).

a([e(kwaadschikse),
   no_e(kwaadschiks)],adv,[],[]).

a([e(kwaadwillende),
   no_e(kwaadwillend)],padv,[],[]).

a([e(kwalijke),
   er(kwalijker),
   ere(kwalijkere),
   no_e(kwalijk),
   st(kwalijkst),
   ste(kwalijkste)],adv,
  [subject_sbar],[]).

a([e(kwalitatieve),
   e(qualitatieve),
   er(kwalitatiever),
   er(qualitatiever),
   ere(kwalitatievere),
   ere(qualitatievere),
   no_e(kwalitatief),
   no_e(qualitatief),
   st(kwalitatiefst),
   st(qualitatiefst),
   ste(kwalitatiefste),
   ste(qualitatiefste)],adv,[],[]).

a([e(kwantificeerbare),
   e(quantificeerbare),
   no_e(kwantificeerbaar),
   no_e(quantificeerbaar)],nonadv,[],[]).

a([e(kwantitatieve),
   e(quantitatieve),
   er(kwantitatiever),
   er(quantitatiever),
   ere(kwantitatievere),
   ere(quantitatievere),
   no_e(kwantitatief),
   no_e(quantitatief),
   st(kwantitatiefst),
   st(quantitatiefst),
   ste(kwantitatiefste),
   ste(quantitatiefste)],adv,[],[]).

a([e(kwetsbare),
   er(kwetsbaarder),
   ere(kwetsbaardere),
   no_e(kwetsbaar),
   st(kwetsbaarst),
   ste(kwetsbaarste)],padv,
  [pp(voor)],[]).

a([e(kwieke),
   er(kwieker),
   ere(kwiekere),
   no_e(kwiek),
   st(kwiekst),
   ste(kwiekste)],adv,[],[]).

a([pred(kwijt)],nonadv,[],[]).

a([ge_e(kwijtgeraakte),
   ge_no_e(kwijtgeraakt)],adv,[],[]).

a([e(kwistige),
   er(kwistiger),
   ere(kwistigere),
   no_e(kwistig),
   st(kwistigst),
   ste(kwistigste)],adv,[],[]).

a([e(lage),
   er(lager),
   ere(lagere),
   no_e(laag),
   st(laagst),
   ste(laagste)],adv,[],[]).

a([e(laaggeschoolde),
   no_e(laaggeschoold)],adv,[],[]).

a([e(laaghangende),
   er(laaghangender),
   ere(laaghangendere),
   no_e(laaghangend),
   st(laaghangendst),
   ste(laaghangendste)],nonadv,[],[]).

a([e(laagstaande),
   no_e(laagstaand)],padv,[],[]).

a([e(laagopgeleide),
   no_e(laagopgeleid)],adv,[],[]).

a([e(laaiende),
   er(laaiender),
   ere(laaiendere),
   no_e(laaiend),
   st(laaiendst),
   ste(laaiendste)],adv,[object_sbar],[]).

a([e(laakbare),
   er(laakbaarder),
   ere(laakbaardere),
   no_e(laakbaar),
   st(laakbaarst),
   ste(laakbaarste)],adv,[],[]).

a([e(late),
   er(later),
   ere(latere),
   no_e(laat),
   st(laatst)
  % ste(laatste) already number: "we staan laatste"
  ],tmpadv,[],[]).

a([e(laatdunkende),
   er(laatdunkender),
   ere(laatdunkendere),
   no_e(laatdunkend),
   st(laatdunkendst),
   ste(laatdunkendste)],adv,[],[]).

a([both(laatstejaars)],nonadv,[],[]).

a([e(laatstgenoemde),
   no_e(laatstgenoemd)],nonadv,[],[]).

a([e(labiele),
   er(labieler),
   ere(labielere),
   no_e(labiel),
   st(labielst),
   ste(labielste)],nonadv,[],[]).

a([e(lacherige),
   er(lacheriger),
   ere(lacherigere),
   no_e(lacherig),
   st(lacherigst),
   ste(lacherigste)],adv,[],[]).

a([e(lachwekkende),
   er(lachwekkender),
   ere(lachwekkendere),
   no_e(lachwekkend),
   st(lachwekkendst),
   ste(lachwekkendste)],nonadv,
  [subject_sbar],[]).

a([e(laconieke),
   e(lakonieke),
   er(laconieker),
   er(lakonieker),
   ere(laconiekere),
   ere(lakoniekere),
   no_e(laconiek),
   no_e(lakoniek),
   st(laconiekst),
   st(lakoniekst),
   ste(laconiekste),
   ste(lakoniekste)],adv,[],[]).

a([e(laffe),
   er(laffer),
   ere(laffere),
   no_e(laf),
   st(lafst),
   ste(lafste)],adv,[],[]).

a([e(lakse),
   no_e(laks)],adv,[],[]).

a([e(lamme),
   er(lammer),
   ere(lammere),
   no_e(lam),
   st(lamst),
   ste(lamste)],adv,[],[]).

a([e(landelijke),
   er(landelijker),
   ere(landelijkere),
   no_e(landelijk),
   st(landelijkst),
   ste(landelijkste)],adv,[],[]).

a([e(landinwaartse),
   postn_no_e(landinwaarts)],diradv,[],[]).

a([e(landschappelijke),
   no_e(landschappelijk)],adv,[],[]).

a([e(lange),
   er(langer),
   ere(langere),
   no_e(lang),
   st(langst),
   ste(langste)],tmpadv,[],
  [boom,
   i(dag,dagen),
   i(decennium,decennia),
   i(el,ellen),
   i(eeuw,eeuwen),
   i(jaar,jaren),
   s(leven),
   i(maand,maanden),
   middel,
   i(minuut,minuten),
   i(uur,uren),
   i(week,weken)
   ]).  % hm, not tmp in that case!

a([e(langdurige),
   er(langduriger),
   ere(langdurigere),
   no_e(langdurig),
   st(langdurigst),
   ste(langdurigste)],adv,[],[]).

a([e(langgerekte),
   er(langgerekter),
   ere(langgerektere),
   no_e(langgerekt),
   st(langgerektst),
   ste(langgerektste)],nonadv,[],[]).

a([e(langharige),
   er(langhariger),
   ere(langharigere),
   no_e(langharig),
   st(langharigst),
   ste(langharigste)],nonadv,[],[]).

a([e(langlopende),
   no_e(langlopend)],nonadv,[],[]).

a([ge_both(langsgekomen)],adv,[],[]).

a([ge_both(langsgelopen)],adv,[],[]).

a([no_e(langverwacht),
   e(langverwachte)],padv,[],[]).

a([e(langwerpige),
   er(langwerpiger),
   ere(langwerpigere),
   no_e(langwerpig),
   st(langwerpigst),
   ste(langwerpigste)],adv,[],[]).

a([e(langzame),
   er(langzamer),
   ere(langzamere),
   no_e(langzaam),
   st(langzaamst),
   ste(langzaamste)],adv,[],[]).

a([e(larmoyante),
   no_e(larmoyant)],adv,[],[]).

a([e(lasterlijke),
   no_e(lasterlijk)],adv,[],[s(god)]).

a([e(lastige),
   er(lastiger),
   ere(lastigere),
   no_e(lastig),
   st(lastigst),
   ste(lastigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(latente),
   er(latenter),
   ere(latentere),
   no_e(latent),
   st(latentst),
   ste(latentste)],adv,[],[]).

a([both(latex)],nonadv,[],[]).

a([e(latijnse),
   no_e(latijns)],nonadv,[],[]).

a([e(lauwe),
   er(lauwer),
   ere(lauwere),
   no_e(lauw),
   st(lauwst),
   ste(lauwste)],adv,[],[]).

a([pred(lauwtjes)],adv,[],[]).

a([e(lauwwarme),
   er(lauwwarmer),
   ere(lauwwarmere),
   no_e(lauwwarm),
   st(lauwwarmst),
   ste(lauwwarmste)],padv,[],[]).

a([e(lawaaierige),
   er(lawaaieriger),
   ere(lawaaierigere),
   no_e(lawaaierig),
   st(lawaaierigst),
   ste(lawaaierigste)],adv,[],[]).

a([e(lawaaiige),
   er(lawaaiiger),
   ere(lawaaiigere),
   no_e(lawaaiig),
   st(lawaaiigst),
   ste(lawaaiigste)],adv,[],[]).

a([stof(lederen)],nonadv,[],[]).

a([e(ledige),
   er(lediger),
   ere(ledigere),
   no_e(ledig),
   st(ledigst),
   ste(ledigste)],adv,[],[]).

a([e(lede),
   er(leder),
   ere(ledere),
   no_e(leed),
   st(leedst),
   ste(leedste)],adv,[],[]).

a([e(leefbare),
   er(leefbaarder),
   ere(leefbaardere),
   no_e(leefbaar),
   st(leefbaarst),
   ste(leefbaarste)],nonadv,[],[on]).

a([e(lege),
   er(leger),
   ere(legere),
   no_e(leeg),
   st(leegst),
   ste(leegste)],adv,[],[]).

a([ende(leegdrinkende),
   end(leegdrinkend)],adv,
  [transitive],[]).

a([both(leeggedronken)],adv,[],[]).

a([ge_e(leeggegooide),
   ge_no_e(leeggegooid)],padv,[],[]).

a([ge_both(leeggelopen)],adv,[],[]).

a([ge_e(leeggeroofde),
   ge_no_e(leeggeroofd)],padv,[],[]).

a([e(lepe),
   er(leper),
   ere(lepere),
   no_e(leep),
   st(leepst),
   ste(leepste)],adv,[],[]).

a([e(leerachtige),
   er(leerachtiger),
   ere(leerachtigere),
   no_e(leerachtig),
   st(leerachtigst),
   ste(leerachtigste)],nonadv,[],[]).

a([e(leerplichtige),
   no_e(leerplichtig)],nonadv,[],[]).

a([e(leerzame),
   er(leerzamer),
   ere(leerzamere),
   no_e(leerzaam),
   st(leerzaamst),
   ste(leerzaamste)],adv,[],[]).

a([e(leesbare),
   er(leesbaarder),
   ere(leesbaardere),
   no_e(leesbaar),
   st(leesbaarst),
   ste(leesbaarste)],adv,[],[af]).

a([e(legale),
   er(legaler),
   ere(legalere),
   no_e(legaal),
   st(legaalst),
   ste(legaalste)],adv,[],[]).

a([e(legendarische),
   er(legendarischer),
   ere(legendarischere),
   no_e(legendarisch),
   st(legendarischt),
   ste(legendarischte)],nonadv,[],[]).

% a([both(legio)],odet_adv,[],[]). % legio kansen

a([both(legio)],adv,[],[]).   % de legio kansen

a([e(legitieme),
   no_e(legitiem)],adv,
  [subject_vp],[]).

a([ende(leidinggevende),
   end(leidinggevend)],padv,[],[]).

a([stof(leien)],nonadv,[],[]).

a([e(lekke),
   no_e(lek),
   st(lekst),
   ste(lekste)],nonadv,[],[]).

a([e(lekkere),
   er(lekkerder),
   ere(lekkerdere),
   no_e(lekker),
   st(lekkerst),
   ste(lekkerste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(lelijke),
   er(lelijker),
   ere(lelijkere),
   no_e(lelijk),
   st(lelijkst),
   ste(lelijkste)],adv,[],[]).

a([both(lemen)],nonadv,[],[]).

a([e(lenige),
   er(leniger),
   ere(lenigere),
   no_e(lenig),
   st(lenigst),
   ste(lenigste)],adv,[],[]).

a([stof(leren)],nonadv,[],[]).

a([e(lesbische),
   no_e(lesbisch)],adv,[],[]).  % lesbisch dansen op top-40 muziek...

a([e(leste),
   no_e(lest)],adv,[],[]).

a([e(letterkundige),
   no_e(letterkundig)],adv,[],[]).

a([e(letterlijke),
   er(letterlijker),
   ere(letterlijkere),
   no_e(letterlijk),
   st(letterlijkst),
   ste(letterlijkste)],adv,[],[]).

a([e(leugenachtige),
   er(leugenachtiger),
   ere(leugenachtigere),
   no_e(leugenachtig),
   st(leugenachtigst),
   ste(leugenachtigste)],nonadv,[],[]).

a([e(leuke),
   er(leuker),
   ere(leukere),
   no_e(leuk),
   st(leukst),
   ste(leukste)],adv,
  [subject_sbar,
   subject_vp,
   subject_vp_sbar,
   pp(voor)],[super]).

a([ende(levende),
   er(levender),
   ere(levendere),
   end(levend),
   st(levendst),
   ste(levendste)],both,
  [pp(in)],[]).

a([e(levendige),
   er(levendiger),
   ere(levendigere),
   no_e(levendig),
   st(levendigst),
   ste(levendigste)],adv,[],[]).

a([e(levenloze),
   er(levenlozer),
   ere(levenlozere),
   no_e(levenloos),
   st(levenloost),
   ste(levenlooste)],padv,[],[]).

a([ende(levensbedreigende),
   end(levensbedreigend)],nonadv,[],[]).

a([e(levensbeschouwelijke),
   no_e(levensbeschouwelijk)],adv,[],[]).

a([e(levensechte),
   er(levensechter),
   ere(levensechtere),
   no_e(levensecht),
   st(levensechtst),
   ste(levensechtste)],adv,[],[]).

a([e(levensgevaarlijke),
   er(levensgevaarlijker),
   ere(levensgevaarlijkere),
   no_e(levensgevaarlijk),
   st(levensgevaarlijkst),
   ste(levensgevaarlijkste)],adv,
  [subject_vp,
   pp(voor)],[]).

a([e(levensgrote),
   no_e(levensgroot)],padv,[],[]).

a([e(levenslustige),
   er(levenslustiger),
   ere(levenslustigere),
   no_e(levenslustig),
   st(levenslustigst),
   ste(levenslustigste)],padv,[],[]).

a([e(levensvatbare),
   er(levensvatbaarder),
   ere(levensvatbaardere),
   no_e(levensvatbaar),
   st(levensvatbaarst),
   ste(levensvatbaarste)],nonadv,[],[]).

a([e(leverbare),
   no_e(leverbaar)],nonadv,
  [pp(in)],[]).

a([e(liberale),
   er(liberaler),
   ere(liberalere),
   no_e(liberaal),
   st(liberaalst),
   ste(liberaalste)],adv,[],
  [h(sociaal),
   neo]).

a([e(libertijnse),
   no_e(libertijns)],nonadv,[],[]).

a([e(lichamelijke),
   no_e(lichamelijk)],adv,[],[]).

a([e(lichte),
   er(lichter),
   ere(lichtere),
   no_e(licht),
   st(lichtst),
   ste(lichtste)],adv,[],[]).

a([e(lichtblauwe),
   er(lichtblauwer),
   ere(lichtblauwere),
   no_e(lichtblauw),
   st(lichtblauwst),
   ste(lichtblauwste)],nonadv,[],[]).

a([e(lichtbruine),
   er(lichtbruiner),
   ere(lichtbruinere),
   no_e(lichtbruin),
   st(lichtbruinst),
   ste(lichtbruinste)],adv,[],[]).

a([e(lichtelijke),
   no_e(lichtelijk)],adv,[],[]).

a([e(lichtende),
   er(lichtender),
   ere(lichtendere),
   no_e(lichtend),
   st(lichtendst),
   ste(lichtendste)],adv,[],[]).

a([e(lichtgele),
   er(lichtgeler),
   ere(lichtgelere),
   no_e(lichtgeel),
   st(lichtgeelst),
   ste(lichtgeelste)],nonadv,[],[]).

a([e(lichtgevende),
   er(lichtgevender),
   ere(lichtgevendere),
   no_e(lichtgevend),
   st(lichtgevendst),
   ste(lichtgevendste)],nonadv,[],[]).

a([both(lichtgewicht)],nonadv,[],[]).

a([e(lichtgewonde),
   no_e(lichtgewond)],padv,[],[]).

a([e(lichtgrijze),
   er(lichtgrijzer),
   ere(lichtgrijzere),
   no_e(lichtgrijs),
   st(lichtgrijst),
   ste(lichtgrijste)],nonadv,[],[]).

a([e(lichtgroene),
   er(lichtgroener),
   ere(lichtgroenere),
   no_e(lichtgroen),
   st(lichtgroenst),
   ste(lichtgroenste)],nonadv,[],[]).

a([pred(lichtjes)],adv,[],[]).

a([e(lichtvaardige),
   er(lichtvaardiger),
   ere(lichtvaardigere),
   no_e(lichtvaardig),
   st(lichtvaardigst),
   ste(lichtvaardigste)],adv,[],[]).

a([e(lichtvoetige),
   no_e(lichtvoetig)],adv,[],[]).

a([e(lichtzinnige),
   er(lichtzinniger),
   ere(lichtzinnigere),
   no_e(lichtzinnig),
   st(lichtzinnigst),
   ste(lichtzinnigste)],adv,[],[]).

a([e(liederlijke),
   er(liederlijker),
   ere(liederlijkere),
   no_e(liederlijk),
   st(liederlijkst),
   ste(liederlijkste)],adv,[],[]).

a([e(lieve),
   er(liever),
   ere(lievere),
   no_e(lief),
   st(liefst),
   ste(liefste)],adv,
  [pp(voor),
   subject_vp,
   subject_sbar],
  [super]).

a([e(liefderijke),
   er(liefderijker),
   ere(liefderijkere),
   no_e(liefderijk),
   st(liefderijkst),
   ste(liefderijkste)],adv,[],[]).

a([e(liefdevolle),
   er(liefdevoller),
   ere(liefdevollere),
   no_e(liefdevol),
   st(liefdevolst),
   ste(liefdevolste)],adv,[],[]).

a([e(liefelijke),
   er(liefelijker),
   ere(liefelijkere),
   no_e(liefelijk),
   st(liefelijkst),
   ste(liefelijkste)],adv,[],[]).

a([pred(liefjes)],adv,[],[]).

a([e(lieflijke),
   er(lieflijker),
   ere(lieflijkere),
   no_e(lieflijk),
   st(lieflijkst),
   ste(lieflijkste)],adv,[],[]).

a([e(liefste),
   no_e(liefst)],adv,[],[]).

a([e(lieftallige),
   er(lieftalliger),
   ere(lieftalligere),
   no_e(lieftallig),
   st(lieftalligst),
   ste(lieftalligste)],adv,[],[]).

a([both(light)],adv,[],[]).

a([ende(lijdende),
   er(lijdender),
   ere(lijdendere),
   end(lijdend),
   st(lijdendst),
   ste(lijdendste)],padv,
  [pp(aan),
   transitive],[]).

a([e(lijdzame),
   er(lijdzamer),
   ere(lijdzamere),
   no_e(lijdzaam),
   st(lijdzaamst),
   ste(lijdzaamste)],adv,[],[]).

a([e(lijfelijke),
   no_e(lijfelijk)],adv,[],[]).

a([e(lijkbleke),
   no_e(lijkbleek)],padv,[],[]).

a([e(lijnrechte),
   er(lijnrechter),
   ere(lijnrechtere),
   no_e(lijnrecht),
   st(lijnrechtst),
   ste(lijnrechtste)],adv,[],[]).

a([e(lijvige),
   er(lijviger),
   ere(lijvigere),
   no_e(lijvig),
   st(lijvigst),
   ste(lijvigste)],nonadv,[],[]).

a([e(lijzige),
   er(lijziger),
   ere(lijzigere),
   no_e(lijzig),
   st(lijzigst),
   ste(lijzigste)],adv,[],[]).

a([both(lila)],nonadv,[],[]).

a([e(limitatieve),
   no_e(limitatief)],nonadv,[],[]).

a([e(lineaire),
   er(lineairder),
   ere(lineairdere),
   no_e(lineair),
   st(lineairst),
   ste(lineairste)],both,[],[]).

a([e(linke),
   er(linker),
   ere(linkere),
   no_e(link),
   st(linkst),
   ste(linkste)],nonadv,[],[]).

a([both(linker)],nonadv,[],[]).

a([e(linkse),
   er(linkser),
   ere(linksere),
   postn_no_e(links),
   st(linkst),
   ste(linkste)],dir_locadv,[],[h(ultra),ultra]).

a([stof(linnen)],nonadv,[],[]).

a([both(liquide)],nonadv,[],[]).

a([e(listige),
   er(listiger),
   ere(listigere),
   no_e(listig),
   st(listigst),
   ste(listigste)],adv,[],[]).

a([e(literaire),
   er(literairder),
   ere(literairdere),
   no_e(literair),
   st(literairst),
   ste(literairste)],adv,[],[]).

a([e(liturgische),
   no_e(liturgisch)],nonadv,[],[]).

a([both(live)],adv,[],[]).

a([e(locale),
   no_e(locaal)],adv,[],[]).

a([stof(loden)],nonadv,[],[]).

a([e(logge),
   er(logger),
   ere(loggere),
   no_e(log),
   st(logst),
   ste(logste)],adv,[],[]).

a([e(logische),
   er(logischer),
   ere(logischere),
   no_e(logisch),
   st(logischt),
   ste(logischte)],adv,
  [subject_sbar,
   subject_vp,
   pp(in),
   pp(voor)],[]).

a([e(logistieke),
   no_e(logistiek)],nonadv,[],[]).

a([e(lokale),
   no_e(lokaal)],padv,[],[]).

a([e(lollige),
   er(lolliger),
   ere(lolligere),
   no_e(lollig),
   st(lolligst),
   ste(lolligste)],adv,[subject_vp,
                        subject_sbar],[]).

a([e(lompe),
   er(lomper),
   ere(lompere),
   no_e(lomp),
   st(lompst),
   ste(lompste)],adv,[],[]).

a([e(longitudinale),
   no_e(longitudinaal)],adv,[],[]).

a([e(loodrechte),
   er(loodrechter),
   ere(loodrechtere),
   postn_no_e(loodrecht),
   st(loodrechtst),
   ste(loodrechtste)],adv,[],[]).

a([e(loodvrije),
   no_e(loodvrij)],padv,[],[]).

a([e(loodzware),
   er(loodzwaarder),
   ere(loodzwaardere),
   no_e(loodzwaar),
   st(loodzwaarst),
   ste(loodzwaarste)],adv,[],[]).

a([e(lome),
   er(lomer),
   ere(lomere),
   no_e(loom),
   st(loomst),
   ste(loomste)],adv,[],[]).

a([e(loopse),
   no_e(loops)],nonadv,[],[]).

a([e(loze),
   er(lozer),
   ere(lozere),
   no_e(loos),
   st(loost),
   ste(looste)],nonadv,[],[]).

a([e(losse),
   er(losser),
   ere(lossere),
   no_e(los),
   st(lost),
   ste(loste)],adv,
  [pp(van)],[]).

a([e(losbandige),
   er(losbandiger),
   ere(losbandigere),
   no_e(losbandig),
   st(losbandigst),
   ste(losbandigste)],adv,[],[]).

a([ge_both(losgebarsten)],adv,[],[]).

a([ge_both(losgebroken)],adv,[],[]).

a([ge_both(losgehangen)],adv,[],[]).

a([ge_e(losgeknoopte),
   ge_no_e(losgeknoopt)],adv,[],[]).

a([ge_both(losgekomen)],adv,[],[]).

a([ge_e(losgekoppelde),
   ge_no_e(losgekoppeld)],adv,[],[]).

a([ge_both(losgelaten)],adv,[],[]).

a([ge_both(losgelopen)],adv,[],[]).

a([ge_e(losgemaakte),
   ge_no_e(losgemaakt)],adv,[],[]).

a([ge_e(losgeraakte),
   ge_no_e(losgeraakt)],adv,[],[]).

a([ge_e(losgeroerde),
   ge_no_e(losgeroerd)],padv,[],[]).

a([ge_e(losgerukte),
   ge_no_e(losgerukt)],adv,[],[]).

a([ge_e(losgescheurde),
   ge_no_e(losgescheurd)],adv,[],[]).

a([ge_both(losgeslagen)],adv,[],[]).

a([ge_e(losgetrilde),
   ge_no_e(losgetrild)],padv,[],[]).

a([pred(losjes)],adv,[],[]).

a([pred([losjes,aan])],adv,[],[]).

a([both(louche)],nonadv,[],[]).

a([e(loutere),
   er(louterder),
   ere(louterdere),
   no_e(louter),
   st(louterst),
   ste(louterste)],adv,[],[]).

a([e(lovenswaardige),
   no_e(lovenswaardig)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([pred(low)],nonadv,[],[]).

a([both([low,key])],adv,[],[]).

a([both([low,level]),
   both(['low-level']),
   both(lowlevel)],nonadv,[],[]).

a([both([low,tech]),
   both('low-tech'),
   both(lowtech)],nonadv,[],[]).

a([e(loyale),
   er(loyaler),
   ere(loyalere),
   no_e(loyaal),
   st(loyaalst),
   ste(loyaalste)],adv,
  [so_pp(aan),
   so_np],[]).

a([e(luchthartige),
   er(luchthartiger),
   ere(luchthartigere),
   no_e(luchthartig),
   st(luchthartigst),
   ste(luchthartigste)],adv,[],[]).

a([e(luchtige),
   er(luchtiger),
   ere(luchtigere),
   no_e(luchtig),
   st(luchtigst),
   ste(luchtigste)],adv,[],[]).

a([pred(luchtigjes)],adv,[],[]).

a([e(luchtledige),
   er(luchtlediger),
   ere(luchtledigere),
   no_e(luchtledig),
   st(luchtledigst),
   ste(luchtledigste)],nonadv,[],[]).

a([e(luchtmobiele),
   no_e(luchtmobiel)],nonadv,[],[]).

a([both(lucide)],nonadv,[],[]).

a([e(lucratieve),
   e(lukratieve),
   er(lucratiever),
   er(lukratiever),
   ere(lucratievere),
   ere(lukratievere),
   no_e(lucratief),
   no_e(lukratief),
   st(lucratiefst),
   st(lukratiefst),
   ste(lucratiefste),
   ste(lukratiefste)],adv,[],[]).

a([e(ludieke),
   er(ludieker),
   ere(ludiekere),
   no_e(ludiek),
   st(ludiekst),
   ste(ludiekste)],adv,[],[]).

a([e(lugubere),
   er(luguberder),
   ere(luguberdere),
   no_e(luguber),
   st(luguberst),
   ste(luguberste)],adv,[],[]).

a([e(luie),
   er(luier),
   ere(luiere),
   no_e(lui),
   st(luist),
   ste(luiste)],adv,[],[]).

a([e(luide),
   er(luider),
   ere(luidere),
   no_e(luid),
   st(luidst),
   ste(luidste)],adv,[],[]).

a([e(luidkeelse),
   no_e(luidkeels)],adv,[],[]).

a([e(luidruchtige),
   er(luidruchtiger),
   ere(luidruchtigere),
   no_e(luidruchtig),
   st(luidruchtigst),
   ste(luidruchtigste)],both,[],[]).

a([e(luisterrijke),
   er(luisterrijker),
   ere(luisterrijkere),
   no_e(luisterrijk),
   st(luisterrijkst),
   ste(luisterrijkste)],adv,[],[]).

a([e(lukrake),
   er(lukraker),
   ere(lukrakere),
   no_e(lukraak),
   st(lukraakst),
   ste(lukraakste)],adv,[],[]).

a([e(lullige),
   er(lulliger),
   ere(lulligere),
   no_e(lullig),
   st(lulligst),
   ste(lulligste)],adv,[subject_sbar],[]).

a([e(lusteloze),
   er(lustelozer),
   ere(lustelozere),
   no_e(lusteloos),
   st(lusteloost),
   ste(lustelooste)],padv,[],[]).

a([e(lustige),
   er(lustiger),
   ere(lustigere),
   no_e(lustig),
   st(lustigst),
   ste(lustigste)],adv,[],[]).

a([e(lutherse),
   no_e(luthers)],nonadv,[],[]).

a([e(luttele),
   er(lutteler),
   ere(luttelere),
   no_e(luttel),
   st(luttelst),
   ste(luttelste)],adv,[],[]).

a([both(luxe),
   no_e(lux),
   er(luxer),
   ere(luxere)],adv,[],[super,
                        i(super_de,superde)]).

a([e(luxueuze),
   er(luxueuzer),
   ere(luxueuzere),
   no_e(luxueus),
   st(luxueust),
   ste(luxueuste)],adv,[],[]).

a([e(lyrische),
   er(lyrischer),
   ere(lyrischere),
   no_e(lyrisch),
   st(lyrischt),
   ste(lyrischte)],adv,[],[]).

a([e(maagdelijke),
   er(maagdelijker),
   ere(maagdelijkere),
   no_e(maagdelijk),
   st(maagdelijkst),
   ste(maagdelijkste)],adv,[],[]).

a([e(maakbare),
   no_e(maakbaar)],padv,[],[]).

a([e(maandagse),
   no_e(maandags)],adv,[],[]).

a([e(maandelijkse),
   no_e(maandelijks)],adv,[],[]).

a([e(maartse),no_e(maarts)],nonadv,[],[]).

a([end(maatgevend),
   ende(maatgevende)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([e(maatschappelijke),
   no_e(maatschappelijk)],adv,[],[]).

a([e('maatschappij-kritische'),
   e(maatschappijkritische),
   er('maatschappij-kritischer'),
   er(maatschappijkritischer),
   ere('maatschappij-kritischere'),
   ere(maatschappijkritischere),
   no_e('maatschappij-kritisch'),
   no_e(maatschappijkritisch),
   st('maatschappij-kritischt'),
   st(maatschappijkritischt),
   ste('maatschappij-kritischte'),
   ste(maatschappijkritischte)],nonadv,[],[]).

a([e(macabere),
   er(macaberder),
   ere(macaberdere),
   no_e(macaber),
   st(macaberst),
   ste(macaberste)],adv,[],[]).

a([e(machinale),
   no_e(machinaal)],adv,[],[]).

a([both(macho)],adv,[],[]).

a([e(machteloze),
   er(machtelozer),
   ere(machtelozere),
   no_e(machteloos),
   st(machteloost),
   ste(machtelooste)],adv,[],[]).

a([e(machtige),
   er(machtiger),
   ere(machtigere),
   no_e(machtig),
   st(machtigst),
   ste(machtigste)],adv,[],[]).

a([e(maffe),
   er(maffer),
   ere(maffere),
   no_e(maf),
   st(mafst),
   ste(mafste)],nonadv,[],[]).

a([e(maffiose),
   e(mafiose),
   no_e(maffioos),
   no_e(mafioos)],adv,[],[]).

a([e(magere),
   er(magerder),
   ere(magerdere),
   no_e(mager),
   st(magerst),
   ste(magerste)],adv,[],[]).

a([pred(magertjes)],adv,[],[]).

a([e(magische),
   er(magischer),
   ere(magischere),
   no_e(magisch),
   st(magischt),
   ste(magischte)],adv,[],[]).

a([e(magistrale),
   no_e(magistraal)],adv,[],[]).

a([e(magnetische),
   er(magnetischer),
   ere(magnetischere),
   no_e(magnetisch),
   st(magnetischt),
   ste(magnetischte)],nonadv,[],[]).

a([e(magnifieke),
   er(magnifieker),
   ere(magnifiekere),
   no_e(magnifiek),
   st(magnifiekst),
   ste(magnifiekste)],adv,[],[]).

a([stof(mahoniehouten)],nonadv,[],[]).

a([e(majestueuze),
   er(majestueuzer),
   ere(majestueuzere),
   no_e(majestueus),
   st(majestueust),
   ste(majestueuste)],adv,[],[]).

a([e(majeure),
   no_e(majeur)],nonadv,[],[]).

a([e(makke),
   er(makker),
   ere(makkere),
   no_e(mak),
   st(makst),
   ste(makste)],adv,[],[]).

a([e(makkelijke),
   er(makkelijker),
   ere(makkelijkere),
   no_e(makkelijk),
   st(makkelijkst),
   ste(makkelijkste)],adv,
  [subject_vp,
   subject_sbar,
   pp(voor)],[]).

a([e(malle),
   er(maller),
   ere(mallere),
   no_e(mal),
   st(malst),
   ste(malste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(malafide),
   both([mala,fide])],nonadv,[],[]).

a([e(malse),
   er(malser),
   ere(malsere),
   no_e(mals),
   st(malst),
   ste(malste)],adv,[],[]).

a([e(manifeste),
   no_e(manifest)],nonadv,[],[]).

a([e(manische),
   no_e(manisch)],padv,[],[]).

a([e(manke),
   no_e(mank)],adv,
  [pp(aan)],[]).

a([e(manlijke),
   er(manlijker),
   ere(manlijkere),
   no_e(manlijk),
   st(manlijkst),
   ste(manlijkste)],adv,[],[]).

a([e(mannelijke),
   er(mannelijker),
   ere(mannelijkere),
   no_e(mannelijk),
   st(mannelijkst),
   ste(mannelijkste)],adv,[],[]).

a([e(manshoge),
   no_e(manshoog)],adv,[],[]).

a([e(manuele),
   no_e(manueel)],adv,[],[]).

a([e(maoïstische),
   no_e(maoïstisch)],adv,[],[]).

a([e(mariene),
   no_e(marien)],nonadv,[],[]).

a([e(marginale),
   no_e(marginaal)],adv,[],[]).

a([e(maritieme),
   er(maritiemer),
   ere(maritiemere),
   no_e(maritiem),
   st(maritiemst),
   ste(maritiemste)],nonadv,[],[]).

a([e(markante),
   er(markanter),
   ere(markantere),
   no_e(markant),
   st(markantst),
   ste(markantste)],nonadv,
  [subject_sbar],[]).

a([stof(marmeren)],nonadv,[],[]).

a([e(marxistische),
   er(marxistischer),
   ere(marxistischere),
   no_e(marxistisch),
   st(marxistischt),
   ste(marxistischte)],nonadv,[],[]).

a([e(masculiene),
   no_e(masculien)],adv,[],[]).

a([e(masochistische),
   no_e(masochistisch)],nonadv,[],[]).

a([e(massale),
   no_e(massaal),
   er(massaler),
   ere(massalere),
   st(massaalst),
   ste(massaalste)],adv,[],[]).

a([e(massieve),
   er(massiever),
   ere(massievere),
   no_e(massief),
   st(massiefst),
   ste(massiefste)],adv,[],[]).

a([e(matte),
   er(matter),
   ere(mattere),
   no_e(mat),
   st(matst),
   ste(matste)],adv,[],[schaak]).

a([e(mateloze),
   er(matelozer),
   ere(matelozere),
   no_e(mateloos),
   st(mateloost),
   ste(matelooste)],adv,[],[]).

a([e(materialistische),
   er(materialistischer),
   ere(materialistischere),
   no_e(materialistisch),
   st(materialistischt),
   ste(materialistischte)],adv,[],[]).

a([e(materiële),
   er(materiëler),
   ere(materielere),
   no_e(materieel),
   st(materieelst),
   ste(materieelste)],adv,[],[]). % materieel verkeren ze in goeden doen

a([stof(matglazen)],nonadv,[],[]).

a([e(matematische),
   e(mathematische),
   no_e(matematisch),
   no_e(mathematisch)],adv,[],[]).

a([e(matige),
   er(matiger),
   ere(matigere),
   no_e(matig),
   st(matigst),
   ste(matigste)],adv,[],[]).

a([both(mauve)],nonadv,[],[]).

a([e(maximale),
   no_e(maximaal)],adv,[],[]).

a([both(maximum)],nonadv,[],[]).

a([e(mechanische),
   no_e(mechanisch)],adv,[],[]).

a([ge_e(medegedeelde),
   ge_no_e(medegedeeld)],adv,[],[]).

a([e(medelevende),
   no_e(medelevend)],adv,[],[]).

a([e(medelijdende),
   er(medelijdender),
   ere(medelijdendere),
   no_e(medelijdend),
   st(medelijdendst),
   ste(medelijdendste)],adv,[],[]).

a([e(medeplichtige),
   er(medeplichtiger),
   ere(medeplichtigere),
   no_e(medeplichtig),
   st(medeplichtigst),
   ste(medeplichtigste)],nonadv,
  [er_pp_sbar(aan),
   pp(aan)],[]).

a([e(medeverantwoordelijke),
   postn_no_e(medeverantwoordelijk)],adv,
  [er_pp_sbar(voor),
   pp(voor)],[]).

a([ende(medewerkende),
   end(medewerkend)],adv,[],[]).

a([e(mediamieke),
   no_e(mediamiek)],nonadv,[],[]).

a([e(medicamenteuze),
   e(medikamenteuze),
   no_e(medicamenteus),
   no_e(medikamenteus)],nonadv,[],[]).

a([e(medicinale),
   no_e(medicinaal)],adv,[],[]).

a([e(medische),
   no_e(medisch)],adv,[],[sport]).

a([e(mediterrane),
   no_e(mediterraan),
   e(mediterraanse),
   no_e(mediterraans)],adv,[],[]).

a([both(medium)],adv,[],[]).  % medium gebakken; ik wil de biefstuk medium

a([both([medium,dry])],nonadv,[],[]).

a([e(meedogenloze),
   er(meedogenlozer),
   ere(meedogenlozere),
   no_e(meedogenloos),
   st(meedogenloost),
   ste(meedogenlooste)],adv,[],[]).

a([ge_e(meegebrachte),
   ge_no_e(meegebracht)],adv,[],[]).

a([ge_e(meegedeelde),
   ge_no_e(meegedeeld)],adv,[],[]).

a([ge_both(meegedragen)],adv,[],[]).

a([ge_e(meegegane),
   ge_no_e(meegegaan)],adv,[],[]).

a([ge_both(meegegeven)],adv,[],[]).

a([ge_both(meegekomen)],adv,[],[]).

a([ge_both(meegekregen)],adv,[],[]).

a([ge_e(meegeleverde),
   ge_no_e(meegeleverd)],adv,[],[]).

a([ge_both(meegelopen)],padv,[],[]).

a([ge_e(meegemaakte),
   ge_no_e(meegemaakt)],adv,[],[]).

a([ge_both(meegenomen)],adv,
  [subject_sbar],[]).

a([e(meegereisde),
   no_e(meegereisd)],nonadv,[],[]).

a([ge_e(meegerekende),
   ge_no_e(meegerekend)],adv,
  [transitive],[]).

a([ge_e(meegesleepte),
   ge_no_e(meegesleept)],adv,[],[]).

a([ge_e(meegesleurde),
   ge_no_e(meegesleurd)],adv,[],[]).

a([ge_e(meegetelde),
   ge_no_e(meegeteld)],adv,[],[]).

a([ge_both(meegetrokken)],adv,[],[]).

a([ge_e(meegetroonde),
   ge_no_e(meegetroond)],adv,[],[]).

a([ge_e(meegevoelde),
   ge_no_e(meegevoeld)],adv,[],[]).

a([ge_e(meegevoerde),
   ge_no_e(meegevoerd)],adv,[],[]).

a([ge_both(meegezogen)],padv,[],[]).

a([ge_both(meegezongen)],padv,[],[]).

a([e(meelijwekkende),
   er(meelijwekkender),
   ere(meelijwekkendere),
   no_e(meelijwekkend),
   st(meelijwekkendst),
   ste(meelijwekkendste)],adv,[],[]).

a([e(meerdere),
   no_e(meerder)],nonadv,[],[]).

a([e(meerderjarige),
   no_e(meerderjarig)],nonadv,[],[]).

a([e(meerjarige),
   no_e(meerjarig)],adv,[],[]).

a([e(meervoudige),
   no_e(meervoudig)],adv,[],[]).

a([e(meeslepende),
   er(meeslepender),
   ere(meeslependere),
   no_e(meeslepend),
   st(meeslependst),
   ste(meeslependste)],adv,[],[]).

a([st(meest),
   ste(meeste)],adv,[],[]).

a([e(meesterlijke),
   er(meesterlijker),
   ere(meesterlijkere),
   no_e(meesterlijk),
   st(meesterlijkst),
   ste(meesterlijkste)],adv,[],[]).

a([e(meetbare),
   no_e(meetbaar)],nonadv,[],[]).

a([e(meetkundige),
   no_e(meetkundig)],adv,[],[]).

a([e(meewarige),
   er(meewariger),
   ere(meewarigere),
   no_e(meewarig),
   st(meewarigst),
   ste(meewarigste)],adv,[],[]).

a([both(mega)],nonadv,[],[]).

a([e(megalomane),
   no_e(megalomaan)],adv,[],[]).

a([e(meisjesachtige),
   er(meisjesachtiger),
   ere(meisjesachtigere),
   no_e(meisjesachtig),
   st(meisjesachtigst),
   ste(meisjesachtigste)],adv,[],[]).

a([e(melancholieke),
   er(melancholieker),
   ere(melancholiekere),
   no_e(melancholiek),
   st(melancholiekst),
   ste(melancholiekste)],adv,[],[]).

a([e(melancholische),
   er(melancholischer),
   ere(melancholischere),
   no_e(melancholisch),
   st(melancholischt),
   ste(melancholischte)],adv,[],[]).

a([e(melige),no_e(melig)],adv,[],[]).

a([e(melkachtige),
   er(melkachtiger),
   ere(melkachtigere),
   no_e(melkachtig),
   st(melkachtigst),
   ste(melkachtigste)],nonadv,[],[]).

a([e(melkwitte),
   er(melkwitter),
   ere(melkwittere),
   no_e(melkwit),
   st(melkwitst),
   ste(melkwitste)],nonadv,[],[]).

a([e(melodieuze),
   er(melodieuzer),
   ere(melodieuzere),
   no_e(melodieus),
   st(melodieust),
   ste(melodieuste)],adv,[],[]).

a([no_e(memorabel),
   e(memorabele)],nonadv,[],[]).

a([pred(menens)],nonadv,[],[]).

a([e(menselijke),
   er(menselijker),
   ere(menselijkere),
   no_e(menselijk),
   st(menselijkst),
   ste(menselijkste)],adv,[],[]).

a([e(menslievende),
   er(menslievender),
   ere(menslievendere),
   no_e(menslievend),
   st(menslievendst),
   ste(menslievendste)],adv,[],[]).

a([e(mensonterende),
   er(mensonterender),
   ere(mensonterendere),
   no_e(mensonterend),
   st(mensonterendst),
   ste(mensonterendste)],nonadv,[],[]).

a([e(menswaardige),
   er(menswaardiger),
   ere(menswaardigere),
   no_e(menswaardig),
   st(menswaardigst),
   ste(menswaardigste)],adv,[],[]).

a([e(mentale),
   no_e(mentaal)],adv,[],[]).

a([e(merkbare),
   er(merkbaarder),
   ere(merkbaardere),
   no_e(merkbaar),
   st(merkbaarst),
   ste(merkbaarste)],adv,
  [subject_sbar_no_het],[]).

a([e(merkloze),
   no_e(merkloos)],padv,[],[]).

a([e(merkwaardige),
   er(merkwaardiger),
   ere(merkwaardigere),
   no_e(merkwaardig),
   st(merkwaardigst),
   ste(merkwaardigste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(messcherpe),
   no_e(messcherp)],adv,[],[]).

a([stof(messing)],nonadv,[],[]).

a([e(metaalachtige),
   er(metaalachtiger),
   ere(metaalachtigere),
   no_e(metaalachtig),
   st(metaalachtigst),
   ste(metaalachtigste)],adv,[],[]).

a([e(metafysische),
   no_e(metafysisch)],adv,[],[]).

a([e(metamorfe),no_e(metamorf)],adv,[],[]).

a([stof(metalen)],nonadv,[],[licht]).

a([e(methodische),
   e(metodische),
   no_e(methodisch),
   no_e(metodisch)],adv,[],[]).

a([e(methodologische),
   no_e(methodologisch)],adv,[],[]).

a([e(metropolitane),
   no_e(metropolitaan)],nonadv,[],[]).

a([e(microscopische),
   e(mikroskopische),
   er(microscopischer),
   er(mikroskopischer),
   ere(microscopischere),
   ere(mikroskopischere),
   no_e(microscopisch),
   no_e(mikroskopisch),
   st(microscopischt),
   st(mikroskopischt),
   ste(microscopischte),
   ste(mikroskopischte)],adv,[],[]).

a([e(midweekse),
   no_e(midweeks)],adv,[],[]).

a([e(middelbare),
   no_e(middelbaar)],adv,[],[]).

a([e(middeleeuwse),
   er(middeleeuwser),
   ere(middeleeuwsere),
   no_e(middeleeuws),
   st(middeleeuwst),
   ste(middeleeuwste)],nonadv,[],[]).

a([e(middelgrote),
   no_e(middelgroot)],nonadv,[],[]).

a([e(middellandse),
   no_e(middellands)],nonadv,[],[]).

a([e(middelmatige),
   er(middelmatiger),
   ere(middelmatigere),
   no_e(middelmatig),
   st(middelmatigst),
   ste(middelmatigste)],adv,[],[]).

a([e(middelste),
   no_e(middelste)],nonadv,[],[]).

a([e(miezerige),
   er(miezeriger),
   ere(miezerigere),
   no_e(miezerig),
   st(miezerigst),
   ste(miezerigste)],nonadv,[],[]).

a([e(milde),
   er(milder),
   ere(mildere),
   no_e(mild),
   st(mildst),
   ste(mildste)],adv,
  [pp(voor)],[]).

a([e(milieuhygiënische),
   no_e(milieuhygiënisch)],adv,[],[]).

a([e(milieuvriendelijke),
   no_e(milieuvriendelijk),
   ere(milieuvriendelijkere),
   er(milieuvriendelijker),
   ste(milieuvriendelijkste),
   st(milieuvriendelijkst)],adv,[],[]).

a([e(militaire),
   no_e(militair)],adv,[],[]).

a([e(militante),
   er(militanter),
   ere(militantere),
   no_e(militant),
   st(militantst),
   ste(militantste)],adv,[],[]).

a([e(minne),
   ere(mindere),
   no_e(min),
   st(minst),
   ste(minste)],adv,[subject_sbar,
                     subject_vp],[]).

a([e(minachtende),
   er(minachtender),
   ere(minachtendere),
   no_e(minachtend),
   st(minachtendst),
   ste(minachtendste)],adv,[],[]).

a([e(minderjarige),
   no_e(minderjarig)],nonadv,[],[]).

a([e(minderwaardige),
   er(minderwaardiger),
   ere(minderwaardigere),
   no_e(minderwaardig),
   st(minderwaardigst),
   ste(minderwaardigste)],nonadv,
  [pp(aan)],[]).

a([e(minerale),
   no_e(mineraal)],nonadv,[],[]).

a([both(miniatuur)],nonadv,[],[]).

a([e(minieme),
   er(miniemer),
   ere(miniemere),
   no_e(miniem),
   st(miniemst),
   ste(miniemste)],adv,[],[]).

a([e(minimale),
   er(minimaler),
   ere(minimalere),
   no_e(minimaal),
   st(minimaalst),
   ste(minimaalste)],adv,[],[]).

a([both(minimum)],nonadv,[],[]).

a([e(ministeriële),
   no_e(ministerieel)],nonadv,[],[]).

a([pred(minnetjes)],adv,[],[]).

a([e(minuscule),
   e(minuskule),
   no_e(minuscuul),
   no_e(minuskuul),
   er(minusculer),
   er(minuskuler),
   ere(minusculere),
   ere(minuskulere),
   st(minuscuulst),
   st(minuskuulst),
   ste(minuskuulste),
   ste(minuscuulste)
  ],adv,[],[]).

a([e(minutieuze),
   er(minutieuzer),
   ere(minutieuzere),
   no_e(minutieus),
   st(minutieust),
   ste(minutieuste)],adv,[],[]).

a([e(minzame),
   er(minzamer),
   ere(minzamere),
   no_e(minzaam),
   st(minzaamst),
   ste(minzaamste)],adv,[],[]).

a([e(mirakelse),
   no_e(mirakels)],nonadv,[],[]).

a([e(miraculeuze),
   e(mirakuleuze),
   er(miraculeuzer),
   er(mirakuleuzer),
   ere(miraculeuzere),
   ere(mirakuleuzere),
   no_e(miraculeus),
   no_e(mirakuleus),
   st(miraculeust),
   st(mirakuleust),
   ste(miraculeuste),
   ste(mirakuleuste)],adv,[],[]).

%% er bleek van alles/niets/iets mee mis
a([pred(mis)],nonadv,[pp(met)],[]).

a([ge_e(misbruikte),
   ge_no_e(misbruikt)],adv,[],[]).

a([ge_e(misdane),
   ge_no_e(misdaan)],adv,[],[]).

a([e(misdadige),
   er(misdadiger),
   ere(misdadigere),
   no_e(misdadig),
   st(misdadigst),
   ste(misdadigste)],adv,
  [subject_sbar],[]).

a([e(miserabele),
   er(miserabeler),
   ere(miserabelere),
   no_e(miserabel),
   st(miserabelst),
   ste(miserabelste)],adv,[],[]).

a([e(misgane),
   no_e(misgaan)],adv,[],[]).

a([ge_e(misgegane),
   ge_no_e(misgegaan)],adv,[],[]).

a([ge_both(misgelopen)],adv,[],[]).

a([ge_e(mishandelde),
   ge_no_e(mishandeld)],adv,[],[]).

a([ge_e(miskende),
   ge_no_e(miskend)],adv,[],[]).

a([ge_e(misleide),
   ge_no_e(misleid)],adv,[],[]).

a([ge_e(mislukte),
   ge_no_e(mislukt)],adv,[],[]).

a([ge_e(mismaakte),
   ge_no_e(mismaakt)],adv,[],[]).

a([e(mismoedige),
   er(mismoediger),
   ere(mismoedigere),
   no_e(mismoedig),
   st(mismoedigst),
   ste(mismoedigste)],padv,[],[]).

a([e(misplaatste),
   er(misplaatster),
   ere(misplaatstere),
   no_e(misplaatst)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(misselijke),
   er(misselijker),
   ere(misselijkere),
   no_e(misselijk),
   st(misselijkst),
   ste(misselijkste)],both,[],[]).

a([e(missionaire),
   no_e(missionair)],nonadv,[],[]).

a([e(mistige),
   er(mistiger),
   ere(mistigere),
   no_e(mistig),
   st(mistigst),
   ste(mistigste)],nonadv,[],[]).

a([e(mistroostige),
   er(mistroostiger),
   ere(mistroostigere),
   no_e(mistroostig),
   st(mistroostigst),
   ste(mistroostigste)],padv,[],[]).

a([ge_e(misvormde),
   ge_no_e(misvormd)],adv,[],[]).

a([e(mobiele),
   er(mobieler),
   ere(mobielere),
   no_e(mobiel),
   st(mobielst),
   ste(mobielste)],adv,[],[]).

a([e(modale),
   no_e(modaal)],adv,[],[]).

a([e(modderige),
   er(modderiger),
   ere(modderigere),
   no_e(modderig),
   st(modderigst),
   ste(modderigste)],nonadv,[],[]).

a([both(model)],adv,[],[]).

a([e(moderne),
   er(moderner),
   ere(modernere),
   no_e(modern),
   st(modernst),
   ste(modernste)],adv,
  [subject_vp,
   pp(in)],
  [hyper,
   vroeg]).

a([e(modieuze),
   er(modieuzer),
   ere(modieuzere),
   no_e(modieus),
   st(modieust),
   ste(modieuste)],adv,[],[]).

a([e(moeë),
   no_e(moe),
   er(moeër),
   er(moeëre)],padv,
  [object_vp,
   pp(moe)],[]).

a([e(moede)],adv,[],[]).

a([e(moedeloze),
   er(moedelozer),
   ere(moedelozere),
   no_e(moedeloos),
   st(moedeloost),
   ste(moedelooste)],padv,[],[]).

a([e(moederlijke),
   er(moederlijker),
   ere(moederlijkere),
   no_e(moederlijk),
   st(moederlijkst),
   ste(moederlijkste)],adv,[],[]).

a([pred([moederziel,alleen])],both,[],[]).

a([e(moedige),
   er(moediger),
   ere(moedigere),
   no_e(moedig),
   st(moedigst),
   ste(moedigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(moedwillige),
   er(moedwilliger),
   ere(moedwilligere),
   no_e(moedwillig),
   st(moedwilligst),
   ste(moedwilligste)],adv,[],[]).

a([both(moegestreden)],padv,[],[]).

a([e(moeilijke),
   er(moeilijker),
   ere(moeilijkere),
   no_e(moeilijk),
   st(moeilijkst),
   ste(moeilijkste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(moeiteloze),
   no_e(moeiteloos)],adv,[],[]).

a([e(moeizame),
   er(moeizamer),
   ere(moeizamere),
   no_e(moeizaam),
   st(moeizaamst),
   ste(moeizaamste)],adv,[],[]).

a([e(moerassige),
   er(moerassiger),
   ere(moerassigere),
   no_e(moerassig),
   st(moerassigst),
   ste(moerassigste)],nonadv,[],[]).

a([e(mogelijke),
   er(mogelijker),
   ere(mogelijkere),
   no_e(mogelijk),
   st(mogelijkst),
   ste(mogelijkste)],adv,
  [subject_sbar_no_het,
   subject_vp_no_het,
   pp(bij),
   pp(met),
   pp(voor)],[]).

a([e(mohammedaanse),
   no_e(mohammedaans)],nonadv,[],[]).

a([e(moleculaire),
   no_e(moleculair)],nonadv,[],[]).

a([e(mollige),
   er(molliger),
   ere(molligere),
   no_e(mollig),
   st(molligst),
   ste(molligste)],nonadv,[],[]).

a([e(momentele),
   no_e(momenteel)],adv,[],[]).

a([e(mondaine),
   er(mondainer),
   ere(mondainere),
   no_e(mondain),
   st(mondainst),
   ste(mondainste)],nonadv,[],[]).

a([pred(monddood)],padv,[],[]).

a([e(mondelinge),
   no_e(mondeling)],adv,[],[]).

a([e(mondiale),
   er(mondialer),
   ere(mondialere),
   no_e(mondiaal),
   st(mondiaalst),
   ste(mondiaalste)],adv,[],[]).

a([e(mondige),
   er(mondiger),
   ere(mondigere),
   no_e(mondig),
   st(mondigst),
   ste(mondigste)],nonadv,[],[]).

a([e(mongoloïde),
   no_e(mongoloïd)],nonadv,[],[]).

a([e(monetaire),
   no_e(monetair)],nonadv,[],[]).

a([e(monochrome),
   no_e(monochroom)],adv,[],[]).

a([e(monogame),
   no_e(monogaam)],nonadv,[],[]).

a([e(monomane),
   no_e(monomaan)],adv,[],[]).

a([e(monopolistische),
   er(monopolistischer),
   ere(monopolistischere),
   no_e(monopolistisch),
   st(monopolistischt),
   ste(monopolistischte)],adv,[],[]).

a([e(monotone),
   er(monotoner),
   ere(monotonere),
   no_e(monotoon),
   st(monotoonst),
   ste(monotoonste)],adv,[],[]).

a([e(monsterachtige),
   er(monsterachtiger),
   ere(monsterachtigere),
   no_e(monsterachtig),
   st(monsterachtigst),
   ste(monsterachtigste)],adv,[],[]).

a([e(monsterlijke),
   er(monsterlijker),
   ere(monsterlijkere),
   no_e(monsterlijk),
   st(monsterlijkst),
   ste(monsterlijkste)],adv,[],[]).

a([e(montere),
   er(monterder),
   ere(monterdere),
   no_e(monter),
   st(monterst),
   ste(monterste)],adv,[],[]).

a([e(monumentale),
   er(monumentaler),
   ere(monumentalere),
   no_e(monumentaal),
   st(monumentaalst),
   ste(monumentaalste)],adv,[],[]).

a([e(mooie),
   er(mooier),
   ere(mooiere),
   no_e(mooi),
   st(mooist),
   ste(mooiste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(moorddadige),
   er(moorddadiger),
   ere(moorddadigere),
   no_e(moorddadig),
   st(moorddadigst),
   ste(moorddadigste)],adv,[],[]).

a([e(moordende),
   er(moordender),
   ere(moordendere),
   no_e(moordend),
   st(moordendst),
   ste(moordendste)],adv,[],[]).

a([e(moralistische),
   er(moralistischer),
   ere(moralistischere),
   no_e(moralistisch),
   st(moralistischt),
   ste(moralistischte)],adv,[],[]).

a([e(morbide),
   no_e(morbide)],adv,[],[]).

a([e(morele),
   er(moreler),
   ere(morelere),
   no_e(moreel),
   st(moreelst),
   ste(moreelste)],adv,[],[]).

a([e(morfologische),
   no_e(morfologisch)],adv,[],[]).

a([e(mormoonse),
   no_e(mormoons)],nonadv,[],[]).

a([e(morsige),
   er(morsiger),
   ere(morsigere),
   no_e(morsig),
   st(morsigst),
   ste(morsigste)],adv,[],[]).

a([e(motorische),
   no_e(motorisch)],adv,[],[]).

a([prefix(mpp)],nonadv,[],[]).

a([e(muffe),
   er(muffer),
   ere(muffere),
   no_e(muf),
   st(mufst),
   ste(mufste)],nonadv,[],[]).

a([e(muisstille),
   er(muisstiller),
   ere(muisstillere),
   no_e(muisstil),
   st(muisstilst),
   ste(muisstilste)],padv,[],[]).

a([e(mulle),
   er(muller),
   ere(mullere),
   no_e(mul),
   st(mulst),
   ste(mulste)],nonadv,[],[]).

a([e(multiculturele),
   no_e(multicultureel),
   e('multi-culturele'),
   no_e('multi-cultureel')],adv,[],[]).

a([e(multidisciplinaire),
   no_e(multidisciplinair)],nonadv,[],[]).

a([e(multifunctionele),
   no_e(multifunctioneel),
   e('multi-functionele'),
   no_e('multi-functioneel')],adv,[],[]).

a([e(multilaterale),
   no_e(multilateraal)],adv,[],[]).

a([both(multimedia),
   both([multi,media]),
   both('multi-media')],nonadv,[],[]).

a([e(multinationale),
   no_e(multinationaal)],nonadv,[],[]).

a([both([multi,purpose]),
   both('multi-purpose')],nonadv,[],[]).

a([e(murwe),
   er(murwer),
   ere(murwere),
   no_e(murw),
   st(murwst),
   ste(murwste)],nonadv,[pp(van)],[]).

a([e(museale),
   no_e(museaal)],nonadv,[],[]).

a([e(muurvaste),
   er(muurvaster),
   ere(muurvastere),
   no_e(muurvast)],nonadv,[],[]).

a([e(muzikale),
   er(muzikaler),
   ere(muzikalere),
   no_e(muzikaal),
   st(muzikaalst),
   ste(muzikaalste)],adv,[],[]).

a([e(mysterieuze),
   er(mysterieuzer),
   ere(mysterieuzere),
   no_e(mysterieus),
   st(mysterieust),
   ste(mysterieuste)],adv,[],[]).

a([e(mystieke),
   er(mystieker),
   ere(mystiekere),
   no_e(mystiek),
   st(mystiekst),
   ste(mystiekste)],nonadv,[],[]).

a([e(mythische),
   e(mytische),
   no_e(mythisch),
   no_e(mytisch)],nonadv,[],[]).

a([e(mythologische),
   e(mytologische),
   no_e(mythologisch),
   no_e(mytologisch)],nonadv,[],[]).

a([pred(na)],nonadv,[],[]).

a([e('na-oorlogse'),
   no_e('na-oorlogs')],nonadv,[],[]).

a([e(naadloze),
   no_e(naadloos)],adv,[],[]).

a([e(naakte),
   er(naakter),
   ere(naaktere),
   no_e(naakt),
   st(naaktst),
   ste(naaktste)],adv,[],[]).

a([e(naamloze),
   no_e(naamloos)],adv,[],[]).

a([e(nare),
   er(naarder),
   ere(naardere),
   no_e(naar),
   st(naarst),
   ste(naarste)],adv,[subject_sbar],[]).

a([e(naargeestige),
   er(naargeestiger),
   ere(naargeestigere),
   no_e(naargeestig),
   st(naargeestigst),
   ste(naargeestigste)],adv,[],[]).

a([pred(naargelang),
   pred([al,naargelang])],adv,[],[]).

a([e(naarstige),
   er(naarstiger),
   ere(naarstigere),
   no_e(naarstig),
   st(naarstigst),
   ste(naarstigste)],adv,[],[]).

a([e(naaste),
   no_e(naast)],adv,[],[]).

a([e(nabije),
   er(nabijer),
   er(naderbij),
   ere(nabijere),
   ere(naderbije),
   no_e(nabij),
   st(naastbij),
   ste(naastbije)],locadv,[],[]).

a([both(nabijgelegen)],nonadv,[],[]).

a([e(naburige),
   no_e(naburig)],nonadv,[],[]).

a([e(nachtelijke),
   no_e(nachtelijk)],adv,[],[]).

a([e(nadelige),
   er(nadeliger),
   ere(nadeligere),
   no_e(nadelig),
   st(nadeligst),
   ste(nadeligste)],adv,
  [pp(voor),
   subject_vp,
   subject_sbar],[]).

a([ende(nadenkende),
   er(nadenkender),
   ere(nadenkendere),
   end(nadenkend),
   st(nadenkendst),
   ste(nadenkendste)],padv,[],[]).

a([ere(nadere),
   er(nader)],adv,[],[]).

a([e(nadrukkelijke),
   er(nadrukkelijker),
   ere(nadrukkelijkere),
   no_e(nadrukkelijk),
   st(nadrukkelijkst),
   ste(nadrukkelijkste)],adv,[],[]).

a([ge_e(nagebootste),
   ge_no_e(nagebootst)],adv,[],[]).

a([ge_e(nagebouwde),
   ge_no_e(nagebouwd)],padv,[],[]).

a([ge_e(nagedane),
   ge_no_e(nagedaan)],adv,[],[]).

a([ge_e(nagegane),
   ge_no_e(nagegaan)],adv,[],[]).

a([ge_both(nagegeven)],adv,[],[]).

a([ge_both(nagehouden)],adv,[],[]).

a([ge_e(nagejaagde),
   ge_no_e(nagejaagd)],adv,[],[]).

a([ge_both(nagekeken)],adv,[],[]).

a([ge_both(nagekomen)],adv,[],[]).

a([ge_both(nagelaten)],adv,[],[]).

a([ge_e(nageleefde),
   ge_no_e(nageleefd)],adv,[],[]).

a([ge_both(nagelopen)],adv,[],[]).

a([ge_e(nagemaakte),
   ge_no_e(nagemaakt)],adv,[],[]).

a([ge_e(nageprate),
   ge_no_e(nagepraat)],adv,[],[]).

a([ge_e(nagestaarde),
   ge_no_e(nagestaard)],adv,[],[]).

a([ge_e(nagespeelde),
   ge_no_e(nagespeeld)],adv,[],[]).

a([ge_e(nagestreefde),
   ge_no_e(nagestreefd)],adv,[],[]).

a([ge_e(nagesynchroniseerde),
   ge_no_e(nagesynchroniseerd)],padv,[],[]).

a([ge_both(nagetrokken)],adv,[],[]).

a([ge_e(nagevolgde),
   ge_no_e(nagevolgd)],adv,[],[]).

a([e(naïeve),
   er(naïever),
   ere(naïevere),
   no_e(naïef),
   st(naïefst),
   ste(naïefste)],adv,[],[]).

a([e(nakende),
   no_e(nakend)],nonadv,[],[]).

a([e(naoorlogse),
   no_e(naoorlogs)],nonadv,[],[]).

a([e(narcistische),
   er(narcistischer),
   ere(narcistischere),
   no_e(narcistisch),
   st(narcistischt),
   ste(narcistischte)],nonadv,[],[]).

a([e(nasale),
   er(nasaler),
   ere(nasalere),
   no_e(nasaal),
   st(nasaalst),
   ste(nasaalste)],nonadv,[],[]).

a([e(naschoolse),
   no_e(naschools)],nonadv,[],[]).

a([e(natte),
   er(natter),
   ere(nattere),
   no_e(nat),
   st(natst),
   ste(natste)],adv,[],[]).

a([ge_e(natgemaakte),
   ge_no_e(natgemaakt)],adv,[],[]).

a([er(nationaler),
   ere(nationalere),
   e(nationale),
   no_e(nationaal)],adv,[],[trans]).

a([e('nationaal-socialistische'),
   no_e('nationaal-socialistisch')],nonadv,[],[]).

a([e(nationalistische),
   er(nationalistischer),
   ere(nationalistischere),
   no_e(nationalistisch),
   st(nationalistischt),
   ste(nationalistischte)],adv,[],[]).

a([e(naturalistische),
   er(naturalistischer),
   ere(naturalistischere),
   no_e(naturalistisch),
   st(naturalistischt),
   ste(naturalistischte)],adv,[],[]).

a([e(naturelle),
   no_e(naturel)],adv,[],[]).

a([e(natuurkundige),
   no_e(natuurkundig)],adv,[],[]).

a([e(natuurlijke),
   er(natuurlijker),
   ere(natuurlijkere),
   no_e(natuurlijk),
   st(natuurlijkst),
   ste(natuurlijkste)],sentadv,[],[]).

a([e(natuurwetenschappelijke),
   no_e(natuurwetenschappelijk)],adv,[],[]).

a([e(nauwe),
   er(nauwer),
   ere(nauwere),
   no_e(nauw),
   st(nauwst),
   ste(nauwste)],adv,[],[]).

a([e(nauwgezette),
   er(nauwgezetter),
   ere(nauwgezettere),
   no_e(nauwgezet),
   st(nauwgezetst),
   ste(nauwgezetste)],adv,[],[]).

a([e(nauwkeurige),
   er(nauwkeuriger),
   ere(nauwkeurigere),
   no_e(nauwkeurig),
   st(nauwkeurigst),
   ste(nauwkeurigste)],adv,
  [pp(bij)],[]).

a([e(nauwlettende),
   er(nauwlettender),
   ere(nauwlettendere),
   no_e(nauwlettend),
   st(nauwlettendst),
   ste(nauwlettendste)],adv,[],[]).

a([e(nauwsluitende),
   no_e(nauwsluitend)],nonadv,[],[]).

a([e(navenante),
   no_e(navenant)],adv,[],[]).

a([ge_e(navertelde),
   ge_no_e(naverteld)],adv,[],[]).

a([e(navrante),
   no_e(navrant)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(naïeve),
   no_e(naïef)],padv,
  [subject_vp],[]).

a([e(nederige),
   er(nederiger),
   ere(nederigere),
   no_e(nederig),
   st(nederigst),
   ste(nederigste)],adv,[],[]).

a([end(neergaand),ende(neergaande)],nonadv,[],[]).

a([ge_both(neergebogen)],adv,[],[]).

a([ge_e(neergedaalde),
   ge_no_e(neergedaald)],adv,[],[]).

a([ge_e(neergedrukte),
   ge_no_e(neergedrukt)],adv,[],[]).

a([ge_e(neergegooide),
   ge_no_e(neergegooid)],adv,[],[]).

a([ge_e(neergehaalde),
   ge_no_e(neergehaald)],adv,[],[]).

a([ge_both(neergehangen)],adv,[],[]).

a([ge_e(neergehurkte),
   ge_no_e(neergehurkt)],adv,[],[]).

a([ge_both(neergekeken)],adv,[],[]).

a([ge_no_e(neergeklapt),
   ge_e(neergeklapte)],adv,[],[]).

a([ge_no_e(neergeknield),
   ge_e(neergeknielde)],adv,[],[]).

a([ge_both(neergekomen)],adv,[],[]).

a([ge_both(neergelaten)],adv,[],[]).

a([ge_e(neergelegde),
   ge_no_e(neergelegd)],adv,[],[]).

a([ge_both(neergeschoten)],adv,[],[]).

a([ge_both(neergeschreven)],adv,[],[]).

a([ge_both(neergeslagen)],adv,[],[]).

a([ge_both(neergestoken)],adv,[],[]).

a([ge_e(neergestorte),
   ge_no_e(neergestort)],adv,[],[]).

a([ge_both(neergestreken)],adv,[],[]).

a([ge_e(neergetelde),
   ge_no_e(neergeteld)],adv,[],[]).

a([ge_both(neergevallen)],adv,[],[]).

a([ge_e(neergezette),
   ge_no_e(neergezet)],adv,[],[]).

a([ge_both(neergezeten)],adv,[],[]).

a([e(neerslachtige),
   er(neerslachtiger),
   ere(neerslachtigere),
   no_e(neerslachtig),
   st(neerslachtigst),
   ste(neerslachtigste)],adv,[],[]).

a([e(neerwaartse),
   postn_no_e(neerwaarts)],diradv,[],[]).

a([e(nefaste),
   no_e(nefast)],adv,[],[]).

a([e(negatieve),
   er(negatiever),
   ere(negatievere),
   no_e(negatief),
   st(negatiefst),
   ste(negatiefste)],adv,
  [subject_sbar,
   er_pp_sbar(over),
   pp(over)],[]).

a([e('negentiende-eeuwse'),
   no_e('negentiende-eeuws')],nonadv,[],[]).

a([both(negentiger)],nonadv,[],[]).

a([both([nek,aan,nek]),
   both('nek-aan-nek')],nonadv,[],[]).

a([e('neo-klassieke'),
   e(neoklassieke),
   no_e('neo-klassiek'),
   no_e(neoklassiek)],nonadv,[],[]).

a([both(nep),
   e(neppe)],adv,[],[]).

a([e(nerveuze),
   er(nerveuzer),
   ere(nerveuzere),
   no_e(nerveus),
   st(nerveust),
   ste(nerveuste)],adv,[],[]).

a([e(nette),
   er(netter),
   ere(nettere),
   no_e(net),
   st(netst),
   ste(netste)],adv,
  [subject_sbar],
  [kraak,  % VL?
   on]).

a([e(netelige),
   er(neteliger),
   ere(neteligere),
   no_e(netelig),
   st(neteligst),
   ste(neteligste)],nonadv,[],[]).

a([pred(netjes)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(netto)],adv,[],[]).

a([e(neurologische),
   no_e(neurologisch)],adv,[],[]).

a([e(neuronale),
   no_e(neuronaal)],adv,[],[]).

a([e(neurotische),
   er(neurotischer),
   ere(neurotischere),
   no_e(neurotisch),
   st(neurotischt),
   ste(neurotischte)],padv,[],[]).

a([e(neutrale),
   er(neutraler),
   ere(neutralere),
   no_e(neutraal),
   st(neutraalst),
   ste(neutraalste)],padv,[],[s(begroting)]).

a([e(nevelige),
   er(neveliger),
   ere(neveligere),
   no_e(nevelig),
   st(neveligst),
   ste(neveligste)],padv,[],[]).  % de dag begint nevelig

a([e(nevenstaande),
   no_e(nevenstaand)],nonadv,[],[]).

a([both(nice)],nonadv,[],[]).

a([both('niet-gebonden')],nonadv,[],[]).

a([e(nietige),
   er(nietiger),
   ere(nietigere),
   no_e(nietig),
   st(nietigst),
   ste(nietigste)],nonadv,[],[]).

a([ende(nietsvermoedende),
   end(nietsvermoedend)],padv,[],[]).

a([ende(nietszeggende),
   er(nietszeggender),
   ere(nietszeggendere),
   end(nietszeggend),
   st(nietszeggendst),
   ste(nietszeggendste)],padv,
  [pp(voor)],[]).

a([e(nieuwe),
   er(nieuwer),
   ere(nieuwere),
   no_e(nieuw),
   st(nieuwst),
   ste(nieuwste)],both,
  [subject_sbar],[]).

a([both(nieuwbakken)],nonadv,[],[]).

a([e(nieuwerwetse),
   no_e(nieuwerwets)],nonadv,[],[]).

a([e(nieuwsgierige),
   er(nieuwsgieriger),
   ere(nieuwsgierigere),
   no_e(nieuwsgierig),
   st(nieuwsgierigst),
   ste(nieuwsgierigste)],padv,
  [er_pp_sbar(naar),
   object_sbar,
   pp(naar)],[]).

a([e(nihilistische),
   er(nihilistischer),
   ere(nihilistischere),
   no_e(nihilistisch),
   st(nihilistischt),
   ste(nihilistischte)],adv,[],[]).

a([e(nijdige),
   er(nijdiger),
   ere(nijdigere),
   no_e(nijdig),
   st(nijdigst),
   ste(nijdigste)],adv,
  [object_sbar],[]).

a([e(nijvere),
   er(nijverder),
   ere(nijverdere),
   no_e(nijver),
   st(nijverst),
   ste(nijverste)],adv,[],[]).

a([e(nipte),
   no_e(nipt)],adv,[],[]).

a([e(nobele),
   er(nobeler),
   ere(nobelere),
   no_e(nobel),
   st(nobelst),
   ste(nobelste)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([e(nodeloze),
   er(nodelozer),
   ere(nodelozere),
   no_e(nodeloos),
   st(nodeloost),
   ste(nodelooste)],adv,[subject_vp],[]).

a([e(nodige),
   er(nodiger),
   ere(nodigere),
   no_e(nodig),
   st(nodigst),
   ste(nodigste)],adv,
  [object_vp,
   subject_sbar_no_het,
   subject_vp_no_het,
   pp(voor),
   mod_pp(bij),
   er_pp_vp(voor)],[]).

a([e(noemenswaardige),
   er(noemenswaardiger),
   ere(noemenswaardigere),
   no_e(noemenswaardig),
   st(noemenswaardigst),
   ste(noemenswaardigste)],adv,[],[]).

a([e(noeste),no_e(noest)],adv,[],[]).

a([pred([nogal,wiedes])],nonadv,
  [subject_sbar],[]).

a([e(nomadische),
   no_e(nomadisch)],nonadv,[],[]).

a([e(nominale),
   postn_no_e(nominaal)],nonadv,[],[]).

a([both('non-stop'),
   both([non,stop])],adv,[],[]).

a([e('non-verbale'),
   no_e('non-verbaal')],adv,[],[]).

a([e(nonchalante),
   er(nonchalanter),
   ere(nonchalantere),
   no_e(nonchalant),
   st(nonchalantst),
   ste(nonchalantste)],adv,[],[]).

a([both('non-profit'),
   both([non,profit])],nonadv,[],[]).

a([both(noodgedwongen)],adv,[],[]).

a([e(noodlijdende),
   no_e(noodlijdend)],nonadv,[],[]).

a([e(noodlottige),
   er(noodlottiger),
   ere(noodlottigere),
   no_e(noodlottig),
   st(noodlottigst),
   ste(noodlottigste)],nonadv,[],[]).

a([e(noodzakelijke),
   er(noodzakelijker),
   ere(noodzakelijkere),
   no_e(noodzakelijk),
   st(noodzakelijkst),
   ste(noodzakelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred(noord)],nonadv,[],[]).

a([e(noordelijke),
   er(noordelijker),
   ere(noordelijkere),
   no_e(noordelijk),
   st(noordelijkst),
   ste(noordelijkste)],adv,[],[]).

a([e(noordoostelijke),
   er(noordoostelijker),
   ere(noordoostelijkere),
   no_e(noordoostelijk),
   st(noordoostelijkst),
   ste(noordoostelijkste)],adv,[],[]).

a([e(noordse),
   no_e(noords)],adv,[],[]).

a([e(noordwaartse),
   postn_no_e(noordwaarts)],diradv,[],[]).

a([e(noordwestelijke),
   er(noordwestelijker),
   ere(noordwestelijkere),
   no_e(noordwestelijk),
   st(noordwestelijkst),
   ste(noordwestelijkste)],adv,[],[]).

a([e(normale),
   er(normaler),
   ere(normalere),
   no_e(normaal),
   st(normaalst),
   ste(normaalste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(normatieve),
   no_e(normatief)],padv,[],[]).

a([e(norse),
   er(norser),
   ere(norsere),
   no_e(nors),
   st(norst),
   ste(norste)],adv,[],[]).

a([e(nostalgische),
   er(nostalgischer),
   ere(nostalgischere),
   no_e(nostalgisch),
   st(nostalgischt),
   ste(nostalgischte)],adv,[],[]).

a([pred([not,amused])],padv,[],[]).

a([pred([not,done])],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([pred([not,out])],nonadv,[],[]).

a([e(notariële),
   no_e(notarieel)],adv,[],[]).

a([e(notoire),
   no_e(notoir)],adv,[],[]).

a([e(nuchtere),
   er(nuchterder),
   ere(nuchterdere),
   no_e(nuchter),
   st(nuchterst),
   ste(nuchterste)],adv,[],[]).

a([e(nucleaire),
   er(nucleairder),
   ere(nucleairdere),
   no_e(nucleair),
   st(nucleairst),
   ste(nucleairste)],adv,[],[]).

a([e(nukkige),
   er(nukkiger),
   ere(nukkigere),
   no_e(nukkig),
   st(nukkigst),
   ste(nukkigste)],adv,[],[]).

a([e(numerieke),
   no_e(numeriek)],adv,[],[]).

a([e(nurkse),
   no_e(nurks)],padv,[],[]).

a([e(nutteloze),
   er(nuttelozer),
   ere(nuttelozere),
   no_e(nutteloos),
   st(nutteloost),
   ste(nuttelooste)],adv,[],[]).

a([e(nuttige),
   er(nuttiger),
   ere(nuttigere),
   no_e(nuttig),
   st(nuttigst),
   ste(nuttigste)],adv,
  [subject_vp,
   er_pp_vp(voor),
   pp(voor)],[]).

a([both(nylon)],nonadv,[],[]).

a([e(nymfomane),
   no_e(nymfomaan)],adv,[],[]).

a([e(objectieve),
   e(objektieve),
   er(objectiever),
   er(objektiever),
   ere(objectievere),
   ere(objektievere),
   no_e(objectief),
   no_e(objektief),
   st(objectiefst),
   st(objektiefst),
   ste(objectiefste),
   ste(objektiefste)],adv,[],[]).

a([e(obligate),
   no_e(obligaat)],adv,[],[]).

a([e(obscene),
   er(obscener),
   ere(obscenere),
   no_e(obsceen),
   st(obsceenst),
   ste(obsceenste)],nonadv,[],[]).

a([e(obsessieve),
   no_e(obsessief)],adv,[],[]).

a([e(obscure),
   e(obskure),
   er(obscuurder),
   er(obskuurder),
   ere(obscuurdere),
   ere(obskuurdere),
   no_e(obscuur),
   no_e(obskuur),
   st(obscuurst),
   st(obskuurst),
   ste(obscuurste),
   ste(obskuurste)],nonadv,[],[]).

a([e(obstinate),no_e(obstinaat)],nonadv,[],[]).

a([both(ocharme)],nonadv,[],[]).

a([e(occulte),
   e(okkulte),
   no_e(occult),
   no_e(okkult)],nonadv,[],[]).

a([e(oecumenische),
   no_e(oecumenisch)],adv,[],[]).

a([e(oestrogene),
   no_e(oestrogeen)],nonadv,[],[]).

a([e(oeverloze),
   no_e(oeverloos)],adv,[],[]).

a([pred([off,the,record])],adv,[],[]).

a([pred([off,and,on,the,record])],adv,[],[]).

a([pred([off,en,on,the,record])],adv,[],[]).

a([e(offensieve),
   no_e(offensief),
   er(offensiever),
   ere(offensievere),
   st(offensiefst),
   ste(offensiefste)],adv,[],[]).

a([e(officiële),
   er(officiëler),
   ere(officielere),
   no_e(officieel),
   st(officieelst),
   ste(officieelste)],adv,[],[]).

a([e(officieuze),
   er(officieuzer),
   ere(officieuzere),
   no_e(officieus),
   st(officieust),
   ste(officieuste)],adv,[],[]).

a([both(offline),
   both([off,line])],adv,[],[]).

a([both(offshore),
   both([off,shore])],adv,[],[]).

a([e(ogenblikkelijke),
   er(ogenblikkelijker),
   ere(ogenblikkelijkere),
   no_e(ogenblikkelijk),
   st(ogenblikkelijkst),
   ste(ogenblikkelijkste)],adv,[],[]).

a([e(ogenschijnlijke),
   er(ogenschijnlijker),
   ere(ogenschijnlijkere),
   no_e(ogenschijnlijk),
   st(ogenschijnlijkst),
   ste(ogenschijnlijkste)],adv,[],[]).

a([pred(oké),
   pred(oke),
   pred(ok),
   pred(okay),
   pred('o.k.')],nonadv,
  [subject_sbar,
   subject_vp],
  []).

a([stof(okeren)],nonadv,[],[]).

a([e(okergele),
   er(okergeler),
   ere(okergelere),
   no_e(okergeel),
   st(okergeelst),
   ste(okergeelste)],nonadv,[],[]).

a([e(olijfgroene),
   er(olijfgroener),
   ere(olijfgroenere),
   no_e(olijfgroen),
   st(olijfgroenst),
   ste(olijfgroenste)],nonadv,[],[]).

a([e(olijke),
   er(olijker),
   ere(olijkere),
   no_e(olijk),
   st(olijkst),
   ste(olijkste)],adv,[],[]).

a([e(olympische),
   no_e(olympisch),
   e('Olympische'),
   no_e('Olympisch')],adv,[],[]).  %adv: het olympisch zeilen

a([pred(om)],nonadv,[],[]).

a([pred([om,het,even])],adv,
  [subject_sbar,
   object_sbar],[]).

a([pred([om,over,naar,huis,te,schrijven])],nonadv,[],[]).

%% ik doe dit werk om niet = voor niets
%% dat is om niet
a([pred([om,niet])],sentadv,[],[]).

a([ge_e(omarmde),
   ge_no_e(omarmd)],adv,[],[]).

a([ge_e(omdijkte),
   ge_no_e(omdijkt)],padv,[],[]).

a([ge_both(omgebogen)],adv,[],[]).

a([ge_e(omgebouwde),
   ge_no_e(omgebouwd)],adv,[],[]).

a([ge_e(omgebrachte),
   ge_no_e(omgebracht)],adv,[],[]).

a([ge_e(omgedane),
   ge_no_e(omgedaan)],adv,[],[]).

a([ge_e(omgedoopte),
   ge_no_e(omgedoopt)],adv,[],[]).

a([ge_e(omgedraaide),
   ge_no_e(omgedraaid)],adv,[],[]).

a([ge_e(omgegane),
   ge_no_e(omgegaan)],adv,[],[]).

a([ge_e(omgegooide),
   ge_no_e(omgegooid)],adv,[],[]).

a([ge_e(omgehakte),
   ge_no_e(omgehakt)],adv,[],[]).

a([ge_e(omgekeerde),
   ge_no_e(omgekeerd)],adv,[],[]).

a([ge_both(omgekeken)],adv,[],[]).

a([ge_no_e(omgeklapt),
   ge_e(omgeklapte)],nonadv,[],[]).

a([ge_e(omgeklede),
   ge_no_e(omgekleed)],adv,[],[]).

a([ge_e(omgekochte),
   ge_no_e(omgekocht)],adv,[],[]).

a([ge_both(omgekomen)],adv,[],[]).

a([ge_e(omgelegde),
   ge_no_e(omgelegd)],padv,[],[]).

a([ge_both(omgelegen)],adv,[],[]).

a([ge_e(omgeploegde),
   ge_no_e(omgeploegd)],adv,[],[]).

a([ge_e(omgerekende),
   ge_no_e(omgerekend)],adv,[],[]).

a([ge_both(omgeschreven)],nonadv,[],[]).

a([ge_both(omgeslagen)],adv,[],[]).

a([ge_no_e(omgespit),
   ge_e(omgespitte)],padv,[],[]).

a([ge_both(omgesprongen)],adv,[],[]).

a([ge_e(omgetoverde),
   ge_no_e(omgetoverd)],adv,[],[]).

a([ge_both(omgetrokken)],padv,[],[]).

a([ge_both(omgevallen)],adv,[],[]).

a([ge_both(omgeven)],adv,[],[]).

a([ge_e(omgevormde),
   ge_no_e(omgevormd)],adv,[],[]).

a([ge_e(omgewoelde),
   ge_no_e(omgewoeld)],adv,[],[]).

a([ge_e(omgezette),
   ge_no_e(omgezet)],adv,[],[]).

a([ge_e(omheinde),
   ge_no_e(omheind)],adv,[],[]).

a([ge_e(omhelsde),
   ge_no_e(omhelsd)],adv,[],[]).

a([ge_both(omhooggestoken)],padv,[],[]).

a([ge_both(omhooggevallen)],padv,[],[]).

a([ge_both(omhooggeworpen)],padv,[],[]).

a([ge_e(omhulde),
   ge_no_e(omhuld)],adv,[],[]).

a([prefix(oming),
   prefix(omin)],nonadv,[],[]).

a([e(omkeerbare),
   no_e(omkeerbaar)],nonadv,[],[]).

a([e(omklapbare),
   no_e(omklapbaar)],nonadv,[],[]).

a([ge_e(omklede),
   ge_no_e(omkleed)],adv,[],[]).

a([ge_e(omklemde),
   ge_no_e(omklemd)],adv,[],[]).

a([ende(omliggende),
   end(omliggend)],padv,[transitive],[]).

a([ge_e(omlijnde),
   ge_no_e(omlijnd)],adv,[],[]).

a([ge_e(omlijste),
   ge_no_e(omlijst)],adv,[],[]).

a([ge_e(ommuurde),
   ge_no_e(ommuurd)],adv,[],[]).

a([e(omnivore),
   no_e(omnivoor)],nonadv,[],[]).

a([e(omrande),
   no_e(omrand)],nonadv,[],[]).

a([ge_e(omringde),
   ge_no_e(omringd)],adv,[],[]).

a([ge_both(omschreven)],adv,[],[]).

a([ge_e(omsingelde),
   ge_no_e(omsingeld)],adv,[],[]).

a([e(omslachtige),
   er(omslachtiger),
   ere(omslachtigere),
   no_e(omslachtig),
   st(omslachtigst),
   ste(omslachtigste)],adv,[],[]).

a([ge_both(omsloten)],adv,[],[]).

a([ge_both(omspannen)],adv,[],[]).

a([ge_e(omspoelde),
   ge_no_e(omspoeld)],padv,[],[]).

a([e(omstandige),
   er(omstandiger),
   ere(omstandigere),
   no_e(omstandig),
   st(omstandigst),
   ste(omstandigste)],adv,[],[]).

a([both(omstreden)],nonadv,
  [subject_sbar],[]).

a([ge_e(omstrengelde),
   ge_no_e(omstrengeld)],adv,[],[]).

a([e(omvangrijke),
   er(omvangrijker),
   ere(omvangrijkere),
   no_e(omvangrijk),
   st(omvangrijkst),
   ste(omvangrijkste)],nonadv,[],[]).

a([ge_e(omvatte),
   ge_no_e(omvat)],adv,[],[]).

a([ge_both(omvergelopen)],nonadv,[],[]).

a([ge_both(omvergereden)],nonadv,[],[]).

a([ge_both(omvergeworpen)],nonadv,[],[]).

a([ge_e(omwalde),
   ge_no_e(omwald)],adv,[],[]).

a([ge_e(omwikkelde),
   ge_no_e(omwikkeld)],adv,[],[]).

a([e(omwonende),
   no_e(omwonend)],nonadv,[],[]).

a([ge_e(omzeilde),
   ge_no_e(omzeild)],adv,[],[]).

a([e(omzichtige),
   er(omzichtiger),
   ere(omzichtigere),
   no_e(omzichtig),
   st(omzichtigst),
   ste(omzichtigste)],adv,[],[]).

a([ge_e(omzoomde),
   ge_no_e(omzoomd)],adv,[],[]).

a([pred([on,the,record])],adv,[],[]).

a([pred([on,and,off,the,record])],adv,[],[]).

a([pred([on,en,off,the,record])],adv,[],[]).

a([e(onaangedane),
   no_e(onaangedaan)],padv,[],[]).

a([e(onaangekondigde),
   no_e(onaangekondigd)],padv,[],[]).

a([e(onaangename),
   er(onaangenamer),
   ere(onaangenamere),
   no_e(onaangenaam),
   st(onaangenaamst),
   ste(onaangenaamste)],adv,[],[]).

a([e(onaangepaste),
   er(onaangepaster),
   ere(onaangepastere),
   no_e(onaangepast)],nonadv,[],[]).

a([e(onaangeroerde),
   no_e(onaangeroerd)],padv,[],[]).

a([e(onaangetaste),
   er(onaangetaster),
   ere(onaangetastere),
   no_e(onaangetast)],padv,[],[]).

a([e(onaannemelijke),
   er(onaannemelijker),
   ere(onaannemelijkere),
   no_e(onaannemelijk),
   st(onaannemelijkst),
   ste(onaannemelijkste)],nonadv,
  [subject_sbar_no_het],[]).

a([e(onaantastbare),
   er(onaantastbaarder),
   ere(onaantastbaardere),
   no_e(onaantastbaar),
   st(onaantastbaarst),
   ste(onaantastbaarste)],nonadv,[],[]).

a([e(onaantrekkelijke),
   er(onaantrekkelijker),
   ere(onaantrekkelijkere),
   no_e(onaantrekkelijk),
   st(onaantrekkelijkst),
   ste(onaantrekkelijkste)],adv,
  [pp(voor)],[]).

a([e(onaanvaardbare),
   er(onaanvaardbaarder),
   ere(onaanvaardbaardere),
   no_e(onaanvaardbaar),
   st(onaanvaardbaarst),
   ste(onaanvaardbaarste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(onaanzienlijke),
   er(onaanzienlijker),
   ere(onaanzienlijkere),
   no_e(onaanzienlijk),
   st(onaanzienlijkst),
   ste(onaanzienlijkste)],adv,[],[]).

a([e(onaardige),
   er(onaardiger),
   ere(onaardigere),
   no_e(onaardig),
   st(onaardigst),
   ste(onaardigste)],adv,[],[]).

a([e(onacceptabele),
   no_e(onacceptabel)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onachtzame),
   no_e(onachtzaam)],nonadv,[],[]).

a([e(onaffe),
   no_e(onaf),
   er(onaffer),
   ere(onaffere),
   st(onafst),
   ste(onafste)],nonadv,[],[]).

a([both(onafgebroken)],adv,[],[]).

a([e(onafhankelijke),
   er(onafhankelijker),
   ere(onafhankelijkere),
   no_e(onafhankelijk),
   st(onafhankelijkst),
   ste(onafhankelijkste)],adv,
  [pp(van)],[]).

a([e(onafscheidelijke),
   er(onafscheidelijker),
   ere(onafscheidelijkere),
   no_e(onafscheidelijk),
   st(onafscheidelijkst),
   ste(onafscheidelijkste)],adv,
  [pp(van)],[]).

a([e(onafwendbare),
   no_e(onafwendbaar)],adv,[subject_sbar],[]).

a([e(onafzienbare),
   er(onafzienbaarder),
   ere(onafzienbaardere),
   no_e(onafzienbaar),
   st(onafzienbaarst),
   ste(onafzienbaarste)],adv,[],[]).

a([e(onbaatzuchtige),
   er(onbaatzuchtiger),
   ere(onbaatzuchtigere),
   no_e(onbaatzuchtig),
   st(onbaatzuchtigst),
   ste(onbaatzuchtigste)],adv,[],[]).

a([e(onbarmhartige),
   er(onbarmhartiger),
   ere(onbarmhartigere),
   no_e(onbarmhartig),
   st(onbarmhartigst),
   ste(onbarmhartigste)],adv,[],[]).

a([e(onbeantwoorde),
   no_e(onbeantwoord)],nonadv,[],[]).

a([e(onbedaarlijke),
   er(onbedaarlijker),
   ere(onbedaarlijkere),
   no_e(onbedaarlijk),
   st(onbedaarlijkst),
   ste(onbedaarlijkste)],adv,[],[]).

a([e(onbedekte),
   no_e(onbedekt)],adv,[],[]).

a([e(onbedoelde),
   no_e(onbedoeld)],adv,[],[]).

a([both(onbedorven),
   er(onbedorvener),
   ere(onbedorvenere),
   st(onbedorvenst),
   ste(onbedorvenste)],padv,[],[]).

a([e(onbeduidende),
   er(onbeduidender),
   ere(onbeduidendere),
   no_e(onbeduidend),
   st(onbeduidendst),
   ste(onbeduidendste)],adv,[],[]).

a([e(onbedwingbare),
   er(onbedwingbaarder),
   ere(onbedwingbaardere),
   no_e(onbedwingbaar),
   st(onbedwingbaarst),
   ste(onbedwingbaarste)],nonadv,[],[]).

a([both(onbegonnen)],nonadv,[],[]).

a([ge_e(begraasde),
   ge_no_e(begraasd)],padv,[],[]).

a([e(onbegrensde),
   er(onbegrensder),
   ere(onbegrensdere),
   no_e(onbegrensd),
   st(onbegrensdst),
   ste(onbegrensdste)],adv,[],[]).

a([both(onbegrepen),
   er(onbegrepener),
   ere(onbegrepenere),
   st(onbegrepenst),
   ste(onbegrepenste)],nonadv,[],[]).

a([e(onbegrijpelijke),
   er(onbegrijpelijker),
   ere(onbegrijpelijkere),
   no_e(onbegrijpelijk),
   st(onbegrijpelijkst),
   ste(onbegrijpelijkste)],adv,
  [subject_sbar],[]).

a([e(onbehaaglijke),
   er(onbehaaglijker),
   ere(onbehaaglijkere),
   no_e(onbehaaglijk),
   st(onbehaaglijkst),
   ste(onbehaaglijkste)],adv,[],[]).

a([e(onbeheerste),
   er(onbeheerster),
   ere(onbeheerstere),
   no_e(onbeheerst)],adv,[],[]).

a([both(onbeholpen),
   er(onbeholpener),
   ere(onbeholpenere),
   st(onbeholpenst),
   ste(onbeholpenste)],adv,[],[]).

a([e(onbehoorlijke),
   er(onbehoorlijker),
   ere(onbehoorlijkere),
   no_e(onbehoorlijk),
   st(onbehoorlijkst),
   ste(onbehoorlijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(onbehouwen),
   er(onbehouwener),
   ere(onbehouwenere),
   st(onbehouwenst),
   ste(onbehouwenste)],adv,[],[]).

a([e(onbekende),
   er(onbekender),
   ere(onbekendere),
   postn_no_e(onbekend),
   st(onbekendst),
   ste(onbekendste)],nonadv,
  [subject_sbar_no_het,
   so_np],[]).

a([e(onbekommerde),
   er(onbekommerder),
   ere(onbekommerdere),
   no_e(onbekommerd),
   st(onbekommerdst),
   ste(onbekommerdste)],adv,[],[]).

a([e(onbekwame),
   er(onbekwamer),
   ere(onbekwamere),
   no_e(onbekwaam),
   st(onbekwaamst),
   ste(onbekwaamste)],nonadv,[],[]).

a([e(onbelangrijke),
   er(onbelangrijker),
   ere(onbelangrijkere),
   no_e(onbelangrijk),
   st(onbelangrijkst),
   ste(onbelangrijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onbelaste),
   no_e(onbelast)],adv,[],[]).

a([e(onbeleefde),
   er(onbeleefder),
   ere(onbeleefdere),
   no_e(onbeleefd),
   st(onbeleefdst),
   ste(onbeleefdste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onbelemmerde),
   er(onbelemmerder),
   ere(onbelemmerdere),
   no_e(onbelemmerd),
   st(onbelemmerdst),
   ste(onbelemmerdste)],adv,[],[]).

a([e(onbemande),
   no_e(onbemand)],padv,[],[]).

a([e(onbeminde),
   no_e(onbemind)],padv,[],[]).

a([e(onbenoemde),
   no_e(onbenoemd)],padv,[],[]).

a([e(onbenullige),
   er(onbenulliger),
   ere(onbenulligere),
   no_e(onbenullig),
   st(onbenulligst),
   ste(onbenulligste)],adv,[],[]).

a([e(onbenutte),
   no_e(onbenut)],nonadv,[],[]).

a([e(onbepaalde),
   er(onbepaalder),
   ere(onbepaaldere),
   no_e(onbepaald),
   st(onbepaaldst),
   ste(onbepaaldste)],adv,[],[]).

a([e(onbeperkte),
   er(onbeperkter),
   ere(onbeperktere),
   no_e(onbeperkt),
   st(onbeperktst),
   ste(onbeperktste)],adv,[],[]).

a([e(onbereikbare),
   er(onbereikbaarder),
   ere(onbereikbaardere),
   no_e(onbereikbaar),
   st(onbereikbaarst),
   ste(onbereikbaarste)],nonadv,
  [pp(voor)],[]).

a([e(onberekenbare),
   er(onberekenbaarder),
   ere(onberekenbaardere),
   no_e(onberekenbaar),
   st(onberekenbaarst),
   ste(onberekenbaarste)],adv,[],[]).

a([e(onberispelijke),
   er(onberispelijker),
   ere(onberispelijkere),
   no_e(onberispelijk),
   st(onberispelijkst),
   ste(onberispelijkste)],adv,[],[]).

a([e(onberoerde),
   er(onberoerder),
   ere(onberoerdere),
   no_e(onberoerd),
   st(onberoerdst),
   ste(onberoerdste)],adv,[],[]).

a([e(onbeschaafde),
   er(onbeschaafder),
   ere(onbeschaafdere),
   no_e(onbeschaafd),
   st(onbeschaafdst),
   ste(onbeschaafdste)],adv,[],[]).

a([e(onbeschaamde),
   er(onbeschaamder),
   ere(onbeschaamdere),
   no_e(onbeschaamd),
   st(onbeschaamdst),
   ste(onbeschaamdste)],adv,[],[]).

a([e(onbeschadigde),
   no_e(onbeschadigd)],padv,[],[]).

a([both(onbescheiden),
   er(onbescheidener),
   ere(onbescheidenere),
   st(onbescheidenst),
   ste(onbescheidenste)],adv,[],[]).

a([e(onbeschermde),
   no_e(onbeschermd)],both,[],[]).

a([e(onbeschofte),
   er(onbeschofter),
   ere(onbeschoftere),
   no_e(onbeschoft),
   st(onbeschoftst),
   ste(onbeschoftste)],adv,[],[]).

a([e(onbeschrijfelijke),
   er(onbeschrijfelijker),
   ere(onbeschrijfelijkere),
   no_e(onbeschrijfelijk),
   st(onbeschrijfelijkst),
   ste(onbeschrijfelijkste)],adv,[],[]).

a([e(onbeschrijflijke),
   er(onbeschrijflijker),
   ere(onbeschrijflijkere),
   no_e(onbeschrijflijk),
   st(onbeschrijflijkst),
   ste(onbeschrijflijkste)],adv,[],[]).

a([e(onbesliste),
   no_e(onbeslist)],adv,[],[]).

a([e(onbespreekbare),
   no_e(onbespreekbaar)],nonadv,[subject_sbar],[]).

a([both(onbesproken)],adv,[subject_sbar],[]).

a([e(onbestaanbare),
   no_e(onbestaanbaar)],nonadv,[subject_sbar,subject_vp],[]).

a([e(onbestaande),
   no_e(onbestaand)],nonadv,[],[]).

a([e(onbestemde),
   er(onbestemder),
   ere(onbestemdere),
   no_e(onbestemd),
   st(onbestemdst),
   ste(onbestemdste)],adv,[],[]).

a([e(onbesuisde),
   no_e(onbesuisd)],adv,[],[]).

a([e(onbetaalbare),
   er(onbetaalbaarder),
   ere(onbetaalbaardere),
   no_e(onbetaalbaar),
   st(onbetaalbaarst),
   ste(onbetaalbaarste)],nonadv,[],[]).

a([e(onbetaalde),
   no_e(onbetaald)],padv,[],[]).

a([e(onbetekenende),
   er(onbetekenender),
   ere(onbetekenendere),
   no_e(onbetekenend),
   st(onbetekenendst),
   ste(onbetekenendste)],nonadv,[],[]).

a([e(onbetrouwbare),
   er(onbetrouwbaarder),
   ere(onbetrouwbaardere),
   no_e(onbetrouwbaar),
   st(onbetrouwbaarst),
   ste(onbetrouwbaarste)],adv,[],[]).

a([e(onbetuigde),
   no_e(onbetuigd)],nonadv,[],[]).

a([e(onbetwijfelbare),
   er(onbetwijfelbaarder),
   ere(onbetwijfelbaardere),
   no_e(onbetwijfelbaar),
   st(onbetwijfelbaarst),
   ste(onbetwijfelbaarste)],adv,[],[]).

a([e(onbetwiste),
   er(onbetwister),
   ere(onbetwistere),
   no_e(onbetwist)],adv,[subject_sbar],[]).

a([e(onbetwistbare),
   er(onbetwistbaarder),
   ere(onbetwistbaardere),
   no_e(onbetwistbaar),
   st(onbetwistbaarst),
   ste(onbetwistbaarste)],adv,[],[]).

a([both(onbevangen),
   er(onbevangener),
   ere(onbevangenere),
   st(onbevangenst),
   ste(onbevangenste)],adv,[],[]).

a([e(onbevoegde),
   er(onbevoegder),
   ere(onbevoegdere),
   no_e(onbevoegd),
   st(onbevoegdst),
   ste(onbevoegdste)],adv,
  [object_vp],[]).

a([e(onbevooroordeelde),
   er(onbevooroordeelder),
   ere(onbevooroordeeldere),
   no_e(onbevooroordeeld),
   st(onbevooroordeeldst),
   ste(onbevooroordeeldste)],adv,[],[]).

a([e(onbevredigde),
   er(onbevredigder),
   ere(onbevredigdere),
   no_e(onbevredigd),
   st(onbevredigdst),
   ste(onbevredigdste)],adv,[],[]).

a([e(onbevredigende),
   er(onbevredigender),
   ere(onbevredigendere),
   no_e(onbevredigend),
   st(onbevredigendst),
   ste(onbevredigendste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(onbevreesde),
   er(onbevreesder),
   ere(onbevreesdere),
   no_e(onbevreesd),
   st(onbevreesdst),
   ste(onbevreesdste)],adv,[],[]).

a([e(onbewaakte),
   er(onbewaakter),
   ere(onbewaaktere),
   no_e(onbewaakt),
   st(onbewaaktst),
   ste(onbewaaktste)],adv,[],[]).

a([e(onbeweeglijke),
   er(onbeweeglijker),
   ere(onbeweeglijkere),
   no_e(onbeweeglijk),
   st(onbeweeglijkst),
   ste(onbeweeglijkste)],adv,[],[]).

a([both(onbewezen)],padv,[],[]).

a([both(onbewogen),
   er(onbewogener),
   ere(onbewogenere),
   st(onbewogenst),
   ste(onbewogenste)],adv,[],[]).

a([e(onbewoonbare),
   er(onbewoonbaarder),
   ere(onbewoonbaardere),
   no_e(onbewoonbaar),
   st(onbewoonbaarst),
   ste(onbewoonbaarste)],nonadv,[],[]).

a([e(onbewoonde),
   no_e(onbewoond)],nonadv,[],[]).

a([e(onbewuste),
   er(onbewuster),
   ere(onbewustere),
   no_e(onbewust)],both,[],[]).

a([e(onbezette),
   no_e(onbezet)],nonadv,[],[]).

a([both(onbezonnen),
   er(onbezonnener),
   ere(onbezonnenere),
   st(onbezonnenst),
   ste(onbezonnenste)],adv,[],[]).

a([e(onbezorgde),
   er(onbezorgder),
   ere(onbezorgdere),
   no_e(onbezorgd),
   st(onbezorgdst),
   ste(onbezorgdste)],adv,[],[]).

a([e(onbillijke),
   er(onbillijker),
   ere(onbillijkere),
   no_e(onbillijk),
   st(onbillijkst),
   ste(onbillijkste)],adv,[],[]).

a([e(onbruikbare),
   er(onbruikbaarder),
   ere(onbruikbaardere),
   no_e(onbruikbaar),
   st(onbruikbaarst),
   ste(onbruikbaarste)],nonadv,[],[]).

a([e(onbuigzame),
   er(onbuigzamer),
   ere(onbuigzamere),
   no_e(onbuigzaam),
   st(onbuigzaamst),
   ste(onbuigzaamste)],padv,[],[]).

a([pred(onklaar)],nonadv,[],[]).

a([e(oncontroleerbare),
   e(onkontroleerbare),
   er(oncontroleerbaarder),
   er(onkontroleerbaarder),
   ere(oncontroleerbaardere),
   ere(onkontroleerbaardere),
   no_e(oncontroleerbaar),
   no_e(onkontroleerbaar),
   st(oncontroleerbaarst),
   st(onkontroleerbaarst),
   ste(oncontroleerbaarste),
   ste(onkontroleerbaarste)],adv,[],[]).

a([e(ondankbare),
   er(ondankbaarder),
   ere(ondankbaardere),
   no_e(ondankbaar),
   st(ondankbaarst),
   ste(ondankbaarste)],adv,[],[]).

a([e(ondeelbare),
   no_e(ondeelbaar)],adv,[],[]).

a([e(ondefinieerbare),
   no_e(ondefinieerbaar)],adv,[],[]).

a([e(ondemocratische),
   e(ondemokratische),
   no_e(ondemocratisch),
   no_e(ondemokratisch)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(ondenkbare),
   er(ondenkbaarder),
   ere(ondenkbaardere),
   no_e(ondenkbaar),
   st(ondenkbaarst),
   ste(ondenkbaarste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred([on,speaking,terms])],nonadv,[],[]).

%% erg onder de indruk waren zij niet
a([pred([onder,de,indruk])],nonadv,[pp(van),er_pp_sbar(van)],[]).

a([pred([onder,de,maat])],adv,[],[]).  % de brandweer presteert onder de maat

a([e(onderaardse),
   no_e(onderaards)],nonadv,[],[]).

a([e(onderbelichte),
   no_e(onderbelicht)],adv,
  [subject_sbar],[]).

a([ge_e(onderbetaalde),
   ge_no_e(onderbetaald)],padv,
  [],[]).

a([e(onderbewuste),
   no_e(onderbewust)],adv,[],[]).

a([ge_e(onderbouwde),
   ge_no_e(onderbouwd)],adv,[],[]).

a([ge_both(onderbroken)],padv,[],[]).

a([e(onderdanige),
   er(onderdaniger),
   ere(onderdanigere),
   no_e(onderdanig),
   st(onderdanigst),
   ste(onderdanigste)],adv,[],[]).

a([ge_e(onderdrukte),
   ge_no_e(onderdrukt)],adv,[],[]).

a([ge_e(ondergane),
   ge_no_e(ondergaan)],adv,[],[]).

a([ge_e(ondergebrachte),
   ge_no_e(ondergebracht)],adv,[],[]).

a([ge_e(ondergedane),
   ge_no_e(ondergedaan)],adv,[],[]).

a([ge_both(ondergedoken)],adv,[],[]).

a([ge_e(ondergedompelde),
   ge_no_e(ondergedompeld)],adv,[],[]).

a([ge_e(ondergegane),
   ge_no_e(ondergegaan)],adv,[],[]).

a([ge_both(ondergehouden)],adv,[],[]).

a([ge_both(ondergekomen)],adv,[],[]).

a([ge_both(ondergelegen)],adv,[],[]).

a([ge_both(ondergelopen)],nonadv,[],[]).

a([e(ondergeschikte),
   no_e(ondergeschikt)],nonadv,
  [pp(aan)],[]).

a([ge_both(ondergeschoven)],nonadv,[],[]).

a([ge_e(ondergesneeuwde),
   ge_no_e(ondergesneeuwd)],padv,[],[]).

a([ge_e(ondergewaardeerde),
   ge_no_e(ondergewaardeerd)],padv,[],[]).

a([ge_both(ondergraven)],adv,[],[]).

a([e(ondergrondse),
   no_e(ondergronds)],nonadv,[],[]).

a([ge_e(onderhandelde),
   ge_no_e(onderhandeld)],nonadv,[],[]).

a([both(onderhanden)],nonadv,[],[]).

a([e(onderhandse),
   no_e(onderhands)],adv,[],[]).

a([e(onderhavige),
   no_e(onderhavig)],nonadv,[],[]).

a([e(onderhevige),
   no_e(onderhevig)],nonadv,
  [pp(aan)],[]).

a([ge_both(onderhouden)],adv,[],[]).

a([e(onderhuidse),
   no_e(onderhuids)],adv,[],[]).

a([ge_e(onderkende),
   ge_no_e(onderkend)],adv,[],[]).

a([e(onderkoelde),
   no_e(onderkoeld)],adv,[],[]).

a([e(onderlegde),
   no_e(onderlegd)],padv,[],[]).

a([e(onderlinge),
   postn_no_e(onderling)],adv,
  [pp(tussen)],[]).

a([e(ondermaatse),
   no_e(ondermaats)],adv,[],[]).

a([ge_e(ondermijnde),
   ge_no_e(ondermijnd)],adv,[],[]).

a([e(ondernemende),
   er(ondernemender),
   ere(ondernemendere),
   no_e(ondernemend),
   st(ondernemendst),
   ste(ondernemendste)],adv,[],[]).

a([ge_both(ondernomen)],adv,[],[]).

a([e(onderontwikkelde),
   er(onderontwikkelder),
   ere(onderontwikkeldere),
   no_e(onderontwikkeld),
   st(onderontwikkeldst),
   ste(onderontwikkeldste)],nonadv,[],[]).

a([ge_e(onderrichte),
   ge_no_e(onderricht)],adv,[],[]).

a([ge_e(onderschatte),
   ge_no_e(onderschat)],adv,[],[]).

a([ge_both(onderscheiden),
   e(onderscheidene)],adv,
  [pp(door),
   pp(in),
   pp(met),
   pp(naar),
   pp(tot),
   pp(van),
   pp(voor),
   pp(wegens)],[]).

a([ge_e(onderschepte),
   ge_no_e(onderschept)],adv,[],[]).

a([ge_both(onderschreven)],adv,[],[]).

a([e(onderste),
   no_e(onderste)],nonadv,[],[]).

a([e(onderstaande),
   no_e(onderstaand)],adv,[],[]).

a([pred(ondersteboven),
   pred([onderste,boven])],adv,
  [pp(van)],[]).

a([ge_e(onderstelde),
   ge_no_e(ondersteld)],adv,[],[]).

a([ge_e(ondersteunde),
   ge_no_e(ondersteund)],adv,[],[]).

a([ge_e(onderstreepte),
   ge_no_e(onderstreept)],adv,[],[]).

a([ge_e(ondertekende),
   ge_no_e(ondertekend)],adv,[],[]).

a([ge_e(ondertitelde),
   ge_no_e(ondertiteld)],padv,[],[]).

a([ge_e(ondertunnelde),
   ge_no_e(ondertunneld)],padv,[],[]).

a([ge_e(onderuitgezakte),
   ge_no_e(onderuitgezakt)],adv,[],[]).

a([ge_e(onderverdeelde),
   ge_no_e(onderverdeeld)],adv,[],[]).

a([ge_e(ondervertegenwoordigde),
   ge_no_e(ondervertegenwoordigd)],padv,[],[]).

a([e(ondervoede),
   no_e(ondervoed)],nonadv,[],[]).

a([ge_both(ondervonden)],adv,[],[]).

a([ge_e(ondervraagde),
   ge_no_e(ondervraagd)],adv,[],[]).

a([pred(onderweg)],locadv,
  [pp(naar)],[]).

a([ge_both(onderwezen)],adv,[],[]).

a([e(onderwijskundige),
   no_e(onderwijskundig)],adv,[],[]).

a([ge_both(onderworpen)],adv,[],[]).

a([ge_e(onderzochte),
   ge_no_e(onderzocht)],adv,[],[]).

a([e(ondeugdelijke),
   er(ondeugdelijker),
   ere(ondeugdelijkere),
   no_e(ondeugdelijk),
   st(ondeugdelijkst),
   ste(ondeugdelijkste)],adv,[],[]).

a([e(ondeugende),
   er(ondeugender),
   ere(ondeugendere),
   no_e(ondeugend),
   st(ondeugendst),
   ste(ondeugendste)],adv,[],[]).

a([e(ondiepe),
   er(ondieper),
   ere(ondiepere),
   no_e(ondiep),
   st(ondiepst),
   ste(ondiepste)],locadv,[],[]).

a([e(ondoenlijke),
   er(ondoenlijker),
   ere(ondoenlijkere),
   no_e(ondoenlijk),
   st(ondoenlijkst),
   ste(ondoenlijkste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(ondoordachte),
   er(ondoordachter),
   ere(ondoordachtere),
   no_e(ondoordacht),
   st(ondoordachtst),
   ste(ondoordachtste)],adv,[],[]).

a([e(ondoordringbare),
   er(ondoordringbaarder),
   ere(ondoordringbaardere),
   no_e(ondoordringbaar),
   st(ondoordringbaarst),
   ste(ondoordringbaarste)],nonadv,
  [pp(voor)],[]).

a([e(ondoorgrondelijke),
   er(ondoorgrondelijker),
   ere(ondoorgrondelijkere),
   no_e(ondoorgrondelijk),
   st(ondoorgrondelijkst),
   ste(ondoorgrondelijkste)],adv,[],[]).

a([er(ondoorzichtiger),
   ere(ondoorzichtigere),
   e(ondoorzichtige),
   no_e(ondoorzichtig)],nonadv,[],[]).

a([e(ondraaglijke),
   er(ondraaglijker),
   ere(ondraaglijkere),
   no_e(ondraaglijk),
   st(ondraaglijkst),
   ste(ondraaglijkste),
   e(ondragelijke),
   er(ondragelijker),
   ere(ondragelijkere),
   no_e(ondragelijk),
   st(ondragelijkst),
   ste(ondragelijkste)  ],adv,[],[]).

a([e(ondubbelzinnige),
   er(ondubbelzinniger),
   ere(ondubbelzinnigere),
   no_e(ondubbelzinnig),
   st(ondubbelzinnigst),
   ste(ondubbelzinnigste)],adv,[],[]).

a([e(onduidelijke),
   er(onduidelijker),
   ere(onduidelijkere),
   no_e(onduidelijk),
   st(onduidelijkst),
   ste(onduidelijkste)],adv,
  [pp(voor),
   subject_vp_no_het,
   subject_sbar_no_het],[]).

a([e(onechte),
   er(onechter),
   ere(onechtere),
   no_e(onecht),
   st(onechtst),
   ste(onechtste)],nonadv,[],[]).

a([pred(oneens)],nonadv,
  [er_pp_sbar(met),
   er_pp_vp(met),
   pp(met)],[]).

a([e(oneerbare),
   no_e(oneerbaar)],adv,[],[]).

a([e(oneerbiedige),
   er(oneerbiediger),
   ere(oneerbiedigere),
   no_e(oneerbiedig),
   st(oneerbiedigst),
   ste(oneerbiedigste)],adv,[],[]).

a([e(oneerlijke),
   er(oneerlijker),
   ere(oneerlijkere),
   no_e(oneerlijk),
   st(oneerlijkst),
   ste(oneerlijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(oneffen),
   er(oneffener),
   ere(oneffenere),
   st(oneffenst),
   ste(oneffenste)],nonadv,[],[]).

a([e(oneigenlijke),
   no_e(oneigenlijk)],adv,[],[]).

a([e(oneindige),
   er(oneindiger),
   ere(oneindigere),
   no_e(oneindig),
   st(oneindigst),
   ste(oneindigste)],adv,[],[]).

a([both(onervaren),
   er(onervarener),
   ere(onervarenere),
   st(onervarenst),
   ste(onervarenste)],adv,[],[]).

a([both(oneven)],nonadv,[],[]).

a([e(onevenredige),
   no_e(onevenredig)],adv,[],[]).

a([e(onevenwichtige),
   er(onevenwichtiger),
   ere(onevenwichtigere),
   no_e(onevenwichtig),
   st(onevenwichtigst),
   ste(onevenwichtigste)],padv,[],[]).

a([e(onfatsoenlijke),
   er(onfatsoenlijker),
   ere(onfatsoenlijkere),
   no_e(onfatsoenlijk),
   st(onfatsoenlijkst),
   ste(onfatsoenlijkste)],adv,[],[]).

a([e(onfeilbare),
   er(onfeilbaarder),
   ere(onfeilbaardere),
   no_e(onfeilbaar),
   st(onfeilbaarst),
   ste(onfeilbaarste)],adv,[],[]).

a([e(onfortuinlijke),
   no_e(onfortuinlijk)],adv,
  [subject_sbar],[]).

a([both(ongebonden)],padv,[],[]).

a([both(ongeboren)],nonadv,[],[]).

a([e(ongebreidelde),
   no_e(ongebreideld)],adv,[],[]).

a([e(ongebruikelijke),
   er(ongebruikelijker),
   ere(ongebruikelijkere),
   no_e(ongebruikelijk),
   st(ongebruikelijkst),
   ste(ongebruikelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(ongebruikte),
   no_e(ongebruikt)],padv,[],[]).

a([e(ongecompliceerde),
   e(ongekompliceerde),
   er(ongecompliceerder),
   er(ongekompliceerder),
   ere(ongecompliceerdere),
   ere(ongekompliceerdere),
   no_e(ongecompliceerd),
   no_e(ongekompliceerd),
   st(ongecompliceerdst),
   st(ongekompliceerdst),
   ste(ongecompliceerdste),
   ste(ongekompliceerdste)],adv,[],[]).

a([e(ongecontroleerde),
   e(ongekontroleerde),
   no_e(ongecontroleerd),
   no_e(ongekontroleerd)],padv,[],[]).

a([e(ongedane),
   no_e(ongedaan)],nonadv,[],[]).

a([e(ongedeelde),
   no_e(ongedeeld)],padv,[],[]).

a([e(ongedeerde),
   no_e(ongedeerd)],padv,[],[]).

a([e(ongedifferentieerde),
   no_e(ongedifferentieerd)],adv,[],[]).

a([e(ongeduldige),
   er(ongeduldiger),
   ere(ongeduldigere),
   no_e(ongeduldig),
   st(ongeduldigst),
   ste(ongeduldigste)],adv,[],[]).

a([e(ongedurige),
   er(ongeduriger),
   ere(ongedurigere),
   no_e(ongedurig),
   st(ongedurigst),
   ste(ongedurigste)],adv,[],[]).

a([both(ongedwongen),
   er(ongedwongener),
   ere(ongedwongenere),
   st(ongedwongenst),
   ste(ongedwongenste)],adv,[],[]).

a([e(ongeëvenaarde),
   no_e(ongeëvenaard)],adv,[],[]).

a([e(ongegeneerde),
   er(ongegeneerder),
   ere(ongegeneerdere),
   no_e(ongegeneerd),
   st(ongegeneerdst),
   ste(ongegeneerdste)],adv,[],[]).

a([e(ongegronde),
   er(ongegronder),
   ere(ongegrondere),
   no_e(ongegrond),
   st(ongegrondst),
   ste(ongegrondste)],adv,[],[]).

a([e(ongehinderde),
   no_e(ongehinderd)],padv,
  [pp(door)],[]).

a([e(ongehoorde),
   er(ongehoorder),
   ere(ongehoordere),
   no_e(ongehoord),
   st(ongehoordst),
   ste(ongehoordste)],adv,
  [subject_sbar],[]).

a([e(ongehoorzame),
   er(ongehoorzamer),
   ere(ongehoorzamere),
   no_e(ongehoorzaam),
   st(ongehoorzaamst),
   ste(ongehoorzaamste)],padv,[],[]).

a([e(ongehuwde),
   no_e(ongehuwd)],both,[],[]).

a([e(ongeïnteresseerde),
   er(ongeïnteresseerder),
   ere(ongeïnteresseerdere),
   no_e(ongeïnteresseerd),
   st(ongeïnteresseerdst),
   ste(ongeïnteresseerdste)],adv,[],[]).

a([e(ongekende),
   er(ongekender),
   ere(ongekendere),
   no_e(ongekend),
   st(ongekendst),
   ste(ongekendste)],adv,[],[]).

a([ge_e(ongeklede),
   ge_no_e(ongekleed)],padv,[],[]).

a([e(ongekookte),
   no_e(ongekookt)],nonadv,[],[]).

a([e(ongeldige),
   er(ongeldiger),
   ere(ongeldigere),
   no_e(ongeldig),
   st(ongeldigst),
   ste(ongeldigste)],nonadv,[],[]).

a([both(ongelegen),
   er(ongelegener),
   ere(ongelegenere),
   st(ongelegenst),
   ste(ongelegenste)],adv,[],[]).

a([e(ongeletterde),
   er(ongeletterder),
   ere(ongeletterdere),
   no_e(ongeletterd),
   st(ongeletterdst),
   ste(ongeletterdste)],nonadv,[],[]).

a([ge_both(ongelezen)],padv,[],[]).

a([e(ongelijke),
   er(ongelijker),
   ere(ongelijkere),
   no_e(ongelijk),
   st(ongelijkst),
   ste(ongelijkste)],adv,[],[]).

a([e(ongelimiteerde),
   no_e(ongelimiteerd)],padv,[],[]).

a([e(ongelode),
   no_e(ongelood)],nonadv,[],[]).

a([e(ongelooflijke),
   er(ongelooflijker),
   ere(ongelooflijkere),
   no_e(ongelooflijk),
   st(ongelooflijkst),
   ste(ongelooflijkste),
   e(ongelofelijke),
   er(ongelofelijker),
   ere(ongelofelijkere),
   no_e(ongelofelijk),
   st(ongelofelijkst),
   ste(ongelofelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(ongeloofwaardige),
   er(ongeloofwaardiger),
   ere(ongeloofwaardigere),
   no_e(ongeloofwaardig),
   st(ongeloofwaardigst),
   ste(ongeloofwaardigste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(ongelovige),
   er(ongeloviger),
   ere(ongelovigere),
   no_e(ongelovig),
   st(ongelovigst),
   ste(ongelovigste)],adv,[],[]).

a([e(ongelukkige),
   er(ongelukkiger),
   ere(ongelukkigere),
   no_e(ongelukkig),
   st(ongelukkigst),
   ste(ongelukkigste)],adv,
  [er_pp_sbar(met),
   pp(met),
   object_sbar,
   object_vp,
   subject_sbar,
   subject_vp],[]).

a([e(ongemakkelijke),
   er(ongemakkelijker),
   ere(ongemakkelijkere),
   no_e(ongemakkelijk),
   st(ongemakkelijkst),
   ste(ongemakkelijkste)],adv,[],[]).

a([e(ongemene),
   er(ongemener),
   ere(ongemenere),
   no_e(ongemeen),
   st(ongemeenst),
   ste(ongemeenste)],adv,[],[]).

a([e(ongemerkte),
   er(ongemerkter),
   ere(ongemerktere),
   no_e(ongemerkt),
   st(ongemerktst),
   ste(ongemerktste)],adv,[],[]).

a([e(ongemoeide),
   no_e(ongemoeid)],padv,[],[]).

a([e(ongenaakbare),
   er(ongenaakbaarder),
   ere(ongenaakbaardere),
   no_e(ongenaakbaar),
   st(ongenaakbaarst),
   ste(ongenaakbaarste)],adv,[],[]).

a([e(ongenadige),
   er(ongenadiger),
   ere(ongenadigere),
   no_e(ongenadig),
   st(ongenadigst),
   ste(ongenadigste)],adv,[],[]).

a([e(ongeneeslijke),
   er(ongeneeslijker),
   ere(ongeneeslijkere),
   no_e(ongeneeslijk),
   st(ongeneeslijkst),
   ste(ongeneeslijkste)],adv,[],[]).

a([e(ongenuanceerde),
   er(ongenuanceerder),
   ere(ongenuanceerdere),
   no_e(ongenuanceerd),
   st(ongenuanceerdst),
   ste(ongenuanceerdste)],adv,
  [pp(in)],[]).

a([e(ongenode),
   no_e(ongenood)],nonadv,[],[]).

a([e(ongeoorloofde),
   no_e(ongeoorloofd)],adv,[],[]).

a([e(ongeorganiseerde),
   e(ongeorganizeerde),
   er(ongeorganiseerder),
   er(ongeorganizeerder),
   ere(ongeorganiseerdere),
   ere(ongeorganizeerdere),
   no_e(ongeorganiseerd),
   no_e(ongeorganizeerd),
   st(ongeorganiseerdst),
   st(ongeorganizeerdst),
   ste(ongeorganiseerdste),
   ste(ongeorganizeerdste)],adv,[],[]).

a([e(ongepaste),
   er(ongepaster),
   ere(ongepastere),
   no_e(ongepast)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(ongeregelde),
   er(ongeregelder),
   ere(ongeregeldere),
   no_e(ongeregeld),
   st(ongeregeldst),
   ste(ongeregeldste)],adv,[],[]).

a([e(ongeremde),
   er(ongeremder),
   ere(ongeremdere),
   no_e(ongeremd),
   st(ongeremdst),
   ste(ongeremdste)],adv,[],[]).

a([e(ongerepte),
   no_e(ongerept)],nonadv,[],[]).

a([e(ongerichte),
   no_e(ongericht)],adv,[],[]).

a([e(ongerijmde),
   no_e(ongerijmd)],adv,[],[]).

a([e(ongeruste),
   er(ongeruster),
   ere(ongerustere),
   no_e(ongerust)],padv,
  [pp(over),
   object_sbar],[]). % VL

a([e(ongeschikte),
   er(ongeschikter),
   ere(ongeschiktere),
   no_e(ongeschikt),
   st(ongeschiktst),
   ste(ongeschiktste)],adv,
  [object_vp,
   er_pp_vp(voor),
   pp(voor)],[]).

a([both(ongeschonden)],padv,[],[]).

a([e(ongeschoolde),
   er(ongeschoolder),
   ere(ongeschooldere),
   no_e(ongeschoold),
   st(ongeschooldst),
   ste(ongeschooldste)],nonadv,[],[]).

a([both(ongeschoren)],padv,[],[]).

a([both(ongeschreven)],nonadv,[],[]).

a([both(ongeslagen)],adv,[],[]).

a([e(ongestelde),
   er(ongestelder),
   ere(ongesteldere),
   no_e(ongesteld),
   st(ongesteldst),
   ste(ongesteldste)],nonadv,[],[]).

a([e(ongestoorde),
   no_e(ongestoord)],both,[],[]).

a([e(ongestrafte),
   no_e(ongestraft)],both,[],[]).

a([e(ongestructureerde),
   no_e(ongestructureerd)],adv,[],[]).

a([e(ongetrouwde),
   no_e(ongetrouwd)],nonadv,[],[]).

a([e(ongevaarlijke),
   er(ongevaarlijker),
   ere(ongevaarlijkere),
   no_e(ongevaarlijk),
   st(ongevaarlijkst),
   ste(ongevaarlijkste)],nonadv,
  [subject_vp,
   subject_sbar,
   pp(voor)],[]).

a([e(ongevoelige),
   er(ongevoeliger),
   ere(ongevoeligere),
   no_e(ongevoelig),
   st(ongevoeligst),
   ste(ongevoeligste)],adv,
  [pp(voor)],[]).

a([ge_e(ongevraagde),
   ge_no_e(ongevraagd)],padv,[],[]).

a([ge_e(ongewapende),
   ge_no_e(ongewapend)],padv,[],[]).

a([ge_both(ongewassen),
   er(ongewassener),
   ere(ongewassenere),
   st(ongewassenst),
   ste(ongewassenste)],adv,[],[]).

a([ge_e(ongewenste),
   er(ongewenster),
   ere(ongewenstere),
   ge_no_e(ongewenst)],both,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(ongewijzigde),
   ge_no_e(ongewijzigd)],padv,[],[]).

a([ge_e(ongewilde),
   ge_no_e(ongewild)],adv,[],[]).

a([e(ongewisse),
   no_e(ongewis)],nonadv,
  [subject_sbar],[]).

a([e(ongewone),
   er(ongewoner),
   ere(ongewonere),
   no_e(ongewoon),
   st(ongewoonst),
   ste(ongewoonste)],adv,[],[]).

a([e(ongezellige),
   er(ongezelliger),
   ere(ongezelligere),
   no_e(ongezellig),
   st(ongezelligst),
   ste(ongezelligste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_both(ongezien)],adv,[],[]).

a([e(ongezonde),
   er(ongezonder),
   ere(ongezondere),
   no_e(ongezond),
   st(ongezondst),
   ste(ongezondste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(ongezuurde),
   ge_no_e(ongezuurd)],nonadv,[],[]).

a([e(ongrijpbare),
   er(ongrijpbaarder),
   ere(ongrijpbaardere),
   no_e(ongrijpbaar),
   st(ongrijpbaarst),
   ste(ongrijpbaarste)],nonadv,[],[]).

a([e(ongunstige),
   er(ongunstiger),
   ere(ongunstigere),
   no_e(ongunstig),
   st(ongunstigst),
   ste(ongunstigste)],adv,
  [pp(voor),
   subject_sbar],[]).

a([e(ongure),
   er(onguurder),
   ere(onguurdere),
   no_e(onguur),
   st(onguurst),
   ste(onguurste)],nonadv,[],[]).

a([e(onhaalbare),
   no_e(onhaalbaar)],nonadv,[subject_sbar,
                             subject_vp],[]).

a([e(onhandelbare),
   er(onhandelbaarder),
   ere(onhandelbaardere),
   no_e(onhandelbaar),
   st(onhandelbaarst),
   ste(onhandelbaarste)],nonadv,[],[]).

a([e(onhandige),
   er(onhandiger),
   ere(onhandigere),
   no_e(onhandig),
   st(onhandigst),
   ste(onhandigste)],adv,[subject_sbar,
			  subject_vp],[]).

a([e(onheilspellende),
   er(onheilspellender),
   ere(onheilspellendere),
   no_e(onheilspellend),
   st(onheilspellendst),
   ste(onheilspellendste)],adv,[],[]).

a([e(onherbergzame),
   er(onherbergzamer),
   ere(onherbergzamere),
   no_e(onherbergzaam),
   st(onherbergzaamst),
   ste(onherbergzaamste)],adv,[],[]).

a([e(onherkenbare),
   er(onherkenbaarder),
   ere(onherkenbaardere),
   no_e(onherkenbaar),
   st(onherkenbaarst),
   ste(onherkenbaarste)],adv,
  [pp(door)],[]).

a([e(onherroepelijke),
   er(onherroepelijker),
   ere(onherroepelijkere),
   no_e(onherroepelijk),
   st(onherroepelijkst),
   ste(onherroepelijkste)],adv,[],[]).

a([e(onherstelbare),
   er(onherstelbaarder),
   ere(onherstelbaardere),
   no_e(onherstelbaar),
   st(onherstelbaarst),
   ste(onherstelbaarste)],adv,[],[]).

a([e(onhoorbare),
   no_e(onhoorbaar)],adv,[],[]).

a([e(onhoudbare),
   er(onhoudbaarder),
   ere(onhoudbaardere),
   no_e(onhoudbaar),
   st(onhoudbaarst),
   ste(onhoudbaarste)],adv,
  [pp(voor),
   subject_sbar,
   subject_vp],[]).

a([e(oninbare),
   no_e(oninbaar)],adv,[],[]).

a([e(oninteressante),
   er(oninteressanter),
   ere(oninteressantere),
   no_e(oninteressant),
   st(oninteressantst),
   ste(oninteressantste)],nonadv,
  [pp(voor)],[]).

a([e(onjuiste),
   er(onjuister),
   ere(onjuistere),
   no_e(onjuist)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onkiese),
   no_e(onkies)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(onkundige),
   er(onkundiger),
   ere(onkundigere),
   no_e(onkundig),
   st(onkundigst),
   ste(onkundigste)],padv,
  [er_pp_sbar(van),
   pp(van)],[]).

a([e(onkwetsbare),
   er(onkwetsbaarder),
   ere(onkwetsbaardere),
   no_e(onkwetsbaar),
   st(onkwetsbaarst),
   ste(onkwetsbaarste)],padv,[],[]).

a([e(onleesbare),
   er(onleesbaarder),
   ere(onleesbaardere),
   no_e(onleesbaar),
   st(onleesbaarst),
   ste(onleesbaarste)],nonadv,[],[]).

a([both(online),
   both([on,line]),
   both('on-line')],adv,[],[]).

a([e(onlogische),
   er(onlogischer),
   ere(onlogischere),
   no_e(onlogisch),
   st(onlogischt),
   ste(onlogischte)],adv,
  [subject_sbar],[]).

a([e(onlosmakelijke),
   er(onlosmakelijker),
   ere(onlosmakelijkere),
   no_e(onlosmakelijk),
   st(onlosmakelijkst),
   ste(onlosmakelijkste)],adv,[],[]).

a([e(onmachtige),
   er(onmachtiger),
   ere(onmachtigere),
   no_e(onmachtig),
   st(onmachtigst),
   ste(onmachtigste)],adv,
  [object_vp],[]).

a([e(onmenselijke),
   er(onmenselijker),
   ere(onmenselijkere),
   no_e(onmenselijk),
   st(onmenselijkst),
   ste(onmenselijkste)],adv,[],[subject_vp,
				subject_sbar]).

a([e(onmerkbare),
   er(onmerkbaarder),
   ere(onmerkbaardere),
   no_e(onmerkbaar),
   st(onmerkbaarst),
   ste(onmerkbaarste)],adv,[],[]).

a([e(onmetelijke),
   er(onmetelijker),
   ere(onmetelijkere),
   no_e(onmetelijk),
   st(onmetelijkst),
   ste(onmetelijkste)],adv,[],[]).

a([e(onmiddellijke),
   er(onmiddellijker),
   ere(onmiddellijkere),
   no_e(onmiddellijk),
   st(onmiddellijkst),
   ste(onmiddellijkste)],adv,[],[]).

a([e(onmisbare),
   er(onmisbaarder),
   ere(onmisbaardere),
   no_e(onmisbaar),
   st(onmisbaarst),
   ste(onmisbaarste)],adv,
  [pp(voor)],[]).

a([e(onmiskenbare),
   er(onmiskenbaarder),
   ere(onmiskenbaardere),
   no_e(onmiskenbaar),
   st(onmiskenbaarst),
   ste(onmiskenbaarste)],adv,[],[]).

a([e(onmogelijke),
   er(onmogelijker),
   ere(onmogelijkere),
   no_e(onmogelijk),
   st(onmogelijkst),
   ste(onmogelijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([er(onmondiger),
   ere(onmondigere),
   e(onmondige),
   no_e(onmondig)],nonadv,[],[]).

a([e(onnatuurlijke),
   er(onnatuurlijker),
   ere(onnatuurlijkere),
   no_e(onnatuurlijk),
   st(onnatuurlijkst),
   ste(onnatuurlijkste)],adv,[],[]).

a([e(onnavolgbare),
   er(onnavolgbaarder),
   ere(onnavolgbaardere),
   no_e(onnavolgbaar),
   st(onnavolgbaarst),
   ste(onnavolgbaarste)],adv,[],[]).

a([e(onneembare),
   no_e(onneembaar)],nonadv,[],[]).

a([e(onnodige),
   er(onnodiger),
   ere(onnodigere),
   no_e(onnodig),
   st(onnodigst),
   ste(onnodigste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(onnoemelijke),
   er(onnoemelijker),
   ere(onnoemelijkere),
   no_e(onnoemelijk),
   st(onnoemelijkst),
   ste(onnoemelijkste)],adv,[],[]).

a([e(onnozele),
   er(onnozeler),
   ere(onnozelere),
   no_e(onnozel),
   st(onnozelst),
   ste(onnozelste)],adv,[],[]).

a([e(onofficiele),
   no_e(onofficieel)],adv,[],[]).

a([e(onomkeerbare),
   no_e(onomkeerbaar)],adv,[],[]).

a([e(onomstotelijke),
   er(onomstotelijker),
   ere(onomstotelijkere),
   no_e(onomstotelijk),
   st(onomstotelijkst),
   ste(onomstotelijkste)],adv,[],[]).

a([both(onomstreden)],nonadv,[subject_sbar],[]).

a([both(onomwonden),
   er(onomwondener),
   ere(onomwondenere),
   st(onomwondenst),
   ste(onomwondenste)],adv,[],[]).

a([both(ononderbroken)],adv,[],[]).

a([e(onontbeerlijke),
   er(onontbeerlijker),
   ere(onontbeerlijkere),
   no_e(onontbeerlijk),
   st(onontbeerlijkst),
   ste(onontbeerlijkste)],nonadv,
  [pp(voor),
   subject_sbar,
   subject_vp],[]).

a([e(onontkoombare),
   er(onontkoombaarder),
   ere(onontkoombaardere),
   no_e(onontkoombaar),
   st(onontkoombaarst),
   ste(onontkoombaarste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onooglijke),
   er(onooglijker),
   ere(onooglijkere),
   no_e(onooglijk),
   st(onooglijkst),
   ste(onooglijkste)],adv,[],[]).

a([e(onopgeloste),
   no_e(onopgelost)],nonadv,[],[]).

a([e(onopgemerkte),
   no_e(onopgemerkt)],adv,[],[]).

a([e(onopgesmukte),
   no_e(onopgesmukt)],padv,[],[]).

a([e(onophoudelijke),
   er(onophoudelijker),
   ere(onophoudelijkere),
   no_e(onophoudelijk),
   st(onophoudelijkst),
   ste(onophoudelijkste)],adv,[],[]).

a([e(onoplosbare),
   er(onoplosbaarder),
   ere(onoplosbaardere),
   no_e(onoplosbaar),
   st(onoplosbaarst),
   ste(onoplosbaarste)],nonadv,[],[]).

a([e(onoprechte),
   er(onoprechter),
   ere(onoprechtere),
   no_e(onoprecht),
   st(onoprechtst),
   ste(onoprechtste)],nonadv,[],[]).

a([e(onopvallende),
   er(onopvallender),
   ere(onopvallendere),
   no_e(onopvallend),
   st(onopvallendst),
   ste(onopvallendste)],adv,[],[]).

a([e(onorthodoxe),
   e(onortodoxe),
   er(onorthodoxer),
   er(onortodoxer),
   ere(onorthodoxere),
   ere(onortodoxere),
   no_e(onorthodox),
   no_e(onortodox),
   st(onorthodoxt),
   st(onortodoxt),
   ste(onorthodoxte),
   ste(onortodoxte)],adv,[],[]).

a([e(onoverbrugbare),
   no_e(onoverbrugbaar)],nonadv,[],[]).

a([e(onoverkomelijke),
   er(onoverkomelijker),
   ere(onoverkomelijkere),
   no_e(onoverkomelijk),
   st(onoverkomelijkst),
   ste(onoverkomelijkste)],nonadv,[],[]).

a([both(onovertroffen)],nonadv,[],[]).

a([e(onoverwinnelijke),
   er(onoverwinnelijker),
   ere(onoverwinnelijkere),
   no_e(onoverwinnelijk),
   st(onoverwinnelijkst),
   ste(onoverwinnelijkste)],nonadv,[],[]).

a([e(onoverzichtelijke),
   er(onoverzichtelijker),
   ere(onoverzichtelijkere),
   no_e(onoverzichtelijk),
   st(onoverzichtelijkst),
   ste(onoverzichtelijkste)],adv,[],[]).

a([e(onpartijdige),
   er(onpartijdiger),
   ere(onpartijdigere),
   no_e(onpartijdig),
   st(onpartijdigst),
   ste(onpartijdigste)],adv,[],[]).

a([e(onpeilbare),
   er(onpeilbaarder),
   ere(onpeilbaardere),
   no_e(onpeilbaar),
   st(onpeilbaarst),
   ste(onpeilbaarste)],nonadv,[],[]).

a([e(onpersoonlijke),
   er(onpersoonlijker),
   ere(onpersoonlijkere),
   no_e(onpersoonlijk),
   st(onpersoonlijkst),
   ste(onpersoonlijkste)],adv,[],[]).

a([e(onplezierige),
   er(onplezieriger),
   ere(onplezierigere),
   no_e(onplezierig),
   st(onplezierigst),
   ste(onplezierigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onpractische),
   e(onpraktische),
   er(onpractischer),
   er(onpraktischer),
   ere(onpractischere),
   ere(onpraktischere),
   no_e(onpractisch),
   no_e(onpraktisch),
   st(onpractischt),
   st(onpraktischt),
   ste(onpractischte),
   ste(onpraktischte)],adv,[],[]).

a([e(onprettige),
   er(onprettiger),
   ere(onprettigere),
   no_e(onprettig),
   st(onprettigst),
   ste(onprettigste)],adv,
  [subject_sbar],[]).

a([e(onrealistische),
   er(onrealistischer),
   ere(onrealistischere),
   no_e(onrealistisch),
   st(onrealistischt),
   ste(onrealistischte)],adv,
  [subject_vp],[]).

a([e(onrechtmatige),
   er(onrechtmatiger),
   ere(onrechtmatigere),
   no_e(onrechtmatig),
   st(onrechtmatigst),
   ste(onrechtmatigste)],adv,[subject_sbar,
                              subject_vp],[]).

a([e(onrechtvaardige),
   er(onrechtvaardiger),
   ere(onrechtvaardigere),
   no_e(onrechtvaardig),
   st(onrechtvaardigst),
   ste(onrechtvaardigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onredelijke),
   er(onredelijker),
   ere(onredelijkere),
   no_e(onredelijk),
   st(onredelijkst),
   ste(onredelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onregelmatige),
   er(onregelmatiger),
   ere(onregelmatigere),
   no_e(onregelmatig),
   st(onregelmatigst),
   ste(onregelmatigste)],adv,[],[]).

a([e(onreglementaire),
   no_e(onreglementair)],adv,[],[]).

a([e(onreine),
   er(onreiner),
   ere(onreinere),
   no_e(onrein),
   st(onreinst),
   ste(onreinste)],adv,[],[]).

a([e(onrendabele),
   no_e(onrendabel)],adv,[],[]).

a([e(onrijpe),
   er(onrijper),
   ere(onrijpere),
   no_e(onrijp),
   st(onrijpst),
   ste(onrijpste)],nonadv,[],[]).

a([e(onroerende),
   no_e(onroerend)],nonadv,[],[]).

a([e(onrustbarende),
   er(onrustbarender),
   ere(onrustbarendere),
   no_e(onrustbarend),
   st(onrustbarendst),
   ste(onrustbarendste)],adv,[],[]).

a([e(onrustige),
   er(onrustiger),
   ere(onrustigere),
   no_e(onrustig),
   st(onrustigst),
   ste(onrustigste)],adv,[],[]).

a([e(onsamenhangende),
   er(onsamenhangender),
   ere(onsamenhangendere),
   no_e(onsamenhangend),
   st(onsamenhangendst),
   ste(onsamenhangendste)],adv,[],[]).

a([e(onschadelijke),
   er(onschadelijker),
   ere(onschadelijkere),
   no_e(onschadelijk),
   st(onschadelijkst),
   ste(onschadelijkste)],adv,
  [pp(voor)],[]).

a([e(onschatbare),
   er(onschatbaarder),
   ere(onschatbaardere),
   no_e(onschatbaar),
   st(onschatbaarst),
   ste(onschatbaarste)],adv,[],[]).

a([e(onschendbare),
   no_e(onschendbaar)],nonadv,[],[]).

a([e(onschuldige),
   er(onschuldiger),
   ere(onschuldigere),
   no_e(onschuldig),
   st(onschuldigst),
   ste(onschuldigste)],padv,
  [object_sbar,
   object_vp],[]).

a([e(onsmakelijke),
   er(onsmakelijker),
   ere(onsmakelijkere),
   no_e(onsmakelijk),
   st(onsmakelijkst),
   ste(onsmakelijkste)],adv,[],[]).

a([e(onsportieve),
   er(onsportiever),
   ere(onsportievere),
   no_e(onsportief),
   st(onsportiefst),
   ste(onsportiefste)],adv,[],[]).

a([e(onsterfelijke),
   er(onsterfelijker),
   ere(onsterfelijkere),
   no_e(onsterfelijk),
   st(onsterfelijkst),
   ste(onsterfelijkste)],adv,[],[]).

a([e(onstoffelijke),
   er(onstoffelijker),
   ere(onstoffelijkere),
   no_e(onstoffelijk),
   st(onstoffelijkst),
   ste(onstoffelijkste)],nonadv,[],[]).

a([e(onstuimige),
   er(onstuimiger),
   ere(onstuimigere),
   no_e(onstuimig),
   st(onstuimigst),
   ste(onstuimigste)],adv,[],[]).

a([e(onstuitbare),
   no_e(onstuitbaar)],padv,[],[]).

a([e(onsympathieke),
   e(onsympatieke),
   er(onsympathieker),
   er(onsympatieker),
   ere(onsympathiekere),
   ere(onsympatiekere),
   no_e(onsympathiek),
   no_e(onsympatiek),
   st(onsympathiekst),
   st(onsympatiekst),
   ste(onsympathiekste),
   ste(onsympatiekste)],adv,[],[]).

a([ge_e(ontaarde),
   ge_no_e(ontaard)],adv,
  [pp(in)],[]).

a([ge_e(ontbeerde),
   ge_no_e(ontbeerd)],adv,[],[]).

a([ge_e(ontblote),
   ge_no_e(ontbloot)],adv,[],[]).

a([ge_both(ontboden)],adv,[],[]).

a([ge_both(ontbonden)],adv,[],[]).

a([ge_e(ontbrande),
   ge_no_e(ontbrand)],adv,[],[]).

a([ge_e(ontcijferde),
   ge_no_e(ontcijferd)],adv,[],[]).

a([ge_e(ontdane),
   ge_no_e(ontdaan)],both,
  [er_pp_sbar(over),
   pp(over)],[]).

a([ge_e(ontdekte),
   ge_no_e(ontdekt)],adv,[],[]).

a([ge_both(ontdoken)],adv,[],[]).

a([ge_e(ontdooide),
   ge_no_e(ontdooid)],adv,[],[]).

a([ge_e(onteerde),
   ge_no_e(onteerd)],adv,[],[]).

a([e(ontegenzeglijke),
   er(ontegenzeglijker),
   ere(ontegenzeglijkere),
   no_e(ontegenzeglijk),
   st(ontegenzeglijkst),
   ste(ontegenzeglijkste)],adv,[],[]).

a([e(ontegenzeggelijke),
   er(ontegenzeggelijker),
   ere(ontegenzeggelijkere),
   no_e(ontegenzeggelijk),
   st(ontegenzeggelijkst),
   ste(ontegenzeggelijkste)],adv,[],[]).

a([ge_e(onteigende),
   ge_no_e(onteigend)],adv,[],[]).

a([e(ontelbare),
   no_e(ontelbaar)],adv,[],[]).

a([e(ontembare),
   er(ontembaarder),
   ere(ontembaardere),
   no_e(ontembaar),
   st(ontembaarst),
   ste(ontembaarste)],nonadv,[],[]).

a([e(onterechte),
   er(onterechter),
   ere(onterechtere),
   no_e(onterecht),
   st(onterechtst),
   ste(onterechtste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(onterfde),
   ge_no_e(onterfd)],padv,[],[]).

a([both(ontevreden),
   er(ontevredener),
   ere(ontevredenere),
   st(ontevredenst),
   ste(ontevredenste)],both,
  [er_pp_sbar(over),
   object_sbar,
   pp(met),
   pp(over)],[]).

a([ge_e(ontfermde),
   ge_no_e(ontfermd)],adv,[],[]).

a([ge_e(ontfutselde),
   ge_no_e(ontfutseld)],adv,[],[]).

a([ge_e(ontgane),
   ge_no_e(ontgaan)],adv,[],[]).

a([ge_e(ontglipte),
   ge_no_e(ontglipt)],adv,[],[]).

a([ge_both(ontgonnen)],adv,[],[]).

a([ge_e(ontgoochelde),
   er(ontgoochelder),
   ere(ontgoocheldere),
   ge_no_e(ontgoocheld),
   st(ontgoocheldst),
   ste(ontgoocheldste)],adv,
  [er_pp_sbar(over),
   pp(over),
   object_sbar],[]).

a([ge_e(ontgroeide),
   ge_no_e(ontgroeid)],adv,[],[]).

a([ge_e(onthaalde),
   ge_no_e(onthaald)],adv,[],[]).

a([ge_e(onthaarde),
   ge_no_e(onthaard)],adv,[],[]).

a([ge_e(onthechte),
   ge_no_e(onthecht)],nonadv,[],[]).

a([e(ontheemde),
   no_e(ontheemd)],padv,[],[]).

a([ge_e(ontheiligde),
   ge_no_e(ontheiligd)],nonadv,[],[]).

a([ge_both(ontheven)],adv,[],[]).

a([ge_e(onthoofde),
   ge_no_e(onthoofd)],adv,[],[]).

a([ge_both(onthouden)],nonadv,
  [so_np],[]).

a([ge_e(onthulde),
   ge_no_e(onthuld)],adv,[],[]).

a([e(onthutsende),
   no_e(onthutsend)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(onthutste),
   er(onthutster),
   ere(onthutstere),
   ge_no_e(onthutst)],adv,[],[]).

a([ge_e(ontkende),
   ge_no_e(ontkend)],adv,[],[]).

a([ge_e(ontketende),
   ge_no_e(ontketend)],adv,[],[]).

a([ge_e(ontkiemde),
   ge_no_e(ontkiemd)],adv,[],[]).

a([ge_e(ontklede),
   ge_no_e(ontkleed)],adv,[],[]).

a([ge_e(ontkoppelde),
   ge_no_e(ontkoppeld)],padv,[],[]).

a([ge_e(ontkrachte),
   ge_no_e(ontkracht)],adv,[],[]).

a([ge_e(ontkurkte),
   ge_no_e(ontkurkt)],adv,[],[]).

a([ge_both(ontladen)],adv,[],[]).

a([ge_e(ontlaste),
   ge_no_e(ontlast)],adv,[],[]).

a([ge_e(ontlede),
   ge_no_e(ontleed)],adv,[],[]).

a([ge_e(ontleende),
   ge_no_e(ontleend)],adv,
  [pp(aan)],[]).

a([ge_e(ontleesde),
   ge_no_e(ontleesd)],adv,
  [],[]).

a([ge_both(ontloken)],adv,[],[]).

a([ge_e(ontlokte),
   ge_no_e(ontlokt)],adv,[],[]).

a([ge_both(ontlopen)],adv,[],[]).

a([ge_e(ontluisterde),
   ge_no_e(ontluisterd)],adv,[],[]).

a([ge_e(ontmantelde),
   ge_no_e(ontmanteld)],adv,[],[]).

a([ge_e(ontmaskerde),
   ge_no_e(ontmaskerd)],adv,[],[]).

a([ge_e(ontmoedigde),
   ge_no_e(ontmoedigd)],adv,[],[]).

a([ge_e(ontmoete),
   ge_no_e(ontmoet)],adv,[],[]).

a([ge_both(ontnomen)],adv,[],[]).

a([ge_e(ontnuchterde),
   ge_no_e(ontnuchterd)],adv,[],[]).

a([e(ontoegankelijke),
   er(ontoegankelijker),
   ere(ontoegankelijkere),
   no_e(ontoegankelijk),
   st(ontoegankelijkst),
   ste(ontoegankelijkste)],adv,
  [pp(voor)],[]).

a([e(ontoelaatbare),
   er(ontoelaatbaarder),
   ere(ontoelaatbaardere),
   no_e(ontoelaatbaar),
   st(ontoelaatbaarst),
   ste(ontoelaatbaarste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(ontoereikende),
   er(ontoereikender),
   ere(ontoereikendere),
   no_e(ontoereikend),
   st(ontoereikendst),
   ste(ontoereikendste)],adv,
  [subject_vp,
   pp(voor)],[]).

a([e(ontologische),
   no_e(ontologisch)],nonadv,[],[]).

a([ge_e(ontpitte),
   ge_no_e(ontpit)],padv,[],[]).

a([ge_e(ontplofte),
   ge_no_e(ontploft)],adv,[],[]).

a([ge_e(ontplooide),
   ge_no_e(ontplooid)],adv,[],[]).

a([ge_e(ontpopte),
   ge_no_e(ontpopt)],adv,[],[]).

a([ge_both(ontraden)],adv,[],[]).

a([ge_e(ontrafelde),
   ge_no_e(ontrafeld)],padv,[],[]).

a([e(ontredderde),
   er(ontredderder),
   ere(ontredderdere),
   no_e(ontredderd),
   st(ontredderdst),
   ste(ontredderdste)],padv,[],[]).

a([ge_e(ontregelde),
   ge_no_e(ontregeld)],adv,[],[]).

a([ge_e(ontroerde),
   er(ontroerder),
   ere(ontroerdere),
   ge_no_e(ontroerd),
   st(ontroerdst),
   ste(ontroerdste)],adv,[],[]).

a([e(ontroerende),
   er(ontroerender),
   ere(ontroerendere),
   no_e(ontroerend),
   st(ontroerendst),
   ste(ontroerendste)],adv,
  [transitive,
   subject_sbar,
   subject_vp],[]).

a([ge_e(ontrolde),
   ge_no_e(ontrold)],adv,[],[]).

a([no_e(ontroostbaar),
   e(ontroostbare)],adv,[],[]).

a([e(ontrouwe),
   er(ontrouwer),
   ere(ontrouwere),
   no_e(ontrouw),
   st(ontrouwst),
   ste(ontrouwste)],nonadv,[],[]).

a([ge_e(ontruimde),
   ge_no_e(ontruimd)],adv,[],[]).

a([ge_e(ontrukte),
   ge_no_e(ontrukt)],adv,[],[]).

a([ge_e(ontsierde),
   ge_no_e(ontsierd)],adv,[],[]).

a([ge_both(ontslagen)],adv,[],[]).

a([ge_both(ontsloten)],adv,[],[]).

a([ge_e(ontsluierde),
   ge_no_e(ontsluierd)],adv,[],[]).

a([ge_e(ontsmette),
   ge_no_e(ontsmet)],adv,[],[]).

a([ge_e(ontsnapte),
   ge_no_e(ontsnapt)],padv,[],[]).

a([ge_both(ontspannen),
   er(ontspannener),
   ere(ontspannenere)],adv,[],[]).

a([ge_e(ontspoorde),
   ge_no_e(ontspoord)],adv,[],[]).

a([ge_e(ontspiegelde),
   ge_no_e(ontspiegeld)],adv,[],[]).

a([ge_both(ontsprongen)],adv,[],[]).

a([ge_both(ontsproten)],adv,[],[]).

a([ge_e(ontstane),
   ge_no_e(ontstaan)],adv,[],[]).

a([ge_both(ontstegen)],adv,[],[]).

a([ge_e(ontstelde),
   er(ontstelder),
   ere(ontsteldere),
   ge_no_e(ontsteld),
   st(ontsteldst),
   ste(ontsteldste)],adv,
  [pp(over)],[]).

a([ge_e(ontstemde),
   er(ontstemder),
   ere(ontstemdere),
   ge_no_e(ontstemd),
   st(ontstemdst),
   ste(ontstemdste)],adv,
  [er_pp_sbar(over),
   object_sbar,
   pp(over)],[]).

a([ge_both(ontstoken)],padv,[],[]).

a([ge_both(ontstolen)],adv,[],[]).

a([ge_e(onttakelde),
   ge_no_e(onttakeld)],padv,[],[]).

a([ge_both(onttrokken)],adv,[],[]).

a([ge_e(onttroonde),
   ge_no_e(onttroond)],padv,[],[]).

a([ge_both(ontvallen)],adv,[],[]).

a([ge_both(ontvangen)],adv,[],[]).

a([e(ontvankelijke),
   no_e(ontvankelijk),
   er(ontvankelijker),
   ere(ontvankelijkere),
   st(ontvankelijkst),
   ste(ontvankelijkste)],nonadv,
  [pp(voor)],[h(niet)]).

a([ge_e(ontvelde),
   ge_no_e(ontveld)],nonadv,[],[]).

a([ge_e(ontvluchte),
   ge_no_e(ontvlucht)],adv,[],[]).

a([ge_e(ontvoerde),
   ge_no_e(ontvoerd)],adv,[],[]).

a([ge_both(ontvouwd)],adv,[],[]).

a([ge_both(ontvouwen)],adv,[],[]).

a([ge_e(ontvreemde),
   ge_no_e(ontvreemd)],adv,[],[]).

a([ge_e(ontwaakte),
   ge_no_e(ontwaakt)],adv,[],[]).

a([ge_e(ontwaarde),
   ge_no_e(ontwaard)],adv,[],[]).

a([e(ontwapenende),
   no_e(ontwapenend)],adv,[subject_sbar],[]).

a([ge_e(ontwapende),
   ge_no_e(ontwapend)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(ontwarde),
   ge_no_e(ontward)],adv,[],[]).

a([ge_both(ontweken)],adv,[],[]).

a([e(ontwijkende),
   er(ontwijkender),
   ere(ontwijkendere),
   no_e(ontwijkend),
   st(ontwijkendst),
   ste(ontwijkendste)],adv,[so_np],[]).

a([ge_e(ontwikkelde),
   er(ontwikkelder),
   ere(ontwikkeldere),
   ge_no_e(ontwikkeld),
   st(ontwikkeldst),
   ste(ontwikkeldste)],adv,
  [pp(door),
   transitive],[]).

a([ge_both(ontworpen)],adv,[],[]).

a([ge_e(ontworstelde),
   ge_no_e(ontworsteld)],adv,[],[]).

a([ge_e(ontwortelde),
   ge_no_e(ontworteld)],adv,[],[]).

a([ge_e(ontwrichte),
   ge_no_e(ontwricht)],adv,[],[]).

a([e(ontzade),
   no_e(ontzaad)],nonadv,[],[]).

a([e(ontzaglijke),
   er(ontzaglijker),
   ere(ontzaglijkere),
   no_e(ontzaglijk),
   st(ontzaglijkst),
   ste(ontzaglijkste)],adv,[],[]).

a([e(ontzagwekkende),
   er(ontzagwekkender),
   ere(ontzagwekkendere),
   no_e(ontzagwekkend),
   st(ontzagwekkendst),
   ste(ontzagwekkendste)],adv,[],[]).

a([ge_e(ontzegde),
   ge_no_e(ontzegd)],adv,[],[]).

a([ge_e(ontzenuwde),
   ge_no_e(ontzenuwd)],adv,[],[]).

a([ge_e(ontzette),
   er(ontzetter),
   ere(ontzettere),
   ge_no_e(ontzet),
   st(ontzetst),
   ste(ontzetste)],adv,[],[]).

a([e(ontzettende),
   no_e(ontzettend)],adv,[],[]).

a([ge_e(ontzielde),
   ge_no_e(ontzield)],nonadv,[],[]).

a([ge_e(ontzuilde),
   ge_no_e(ontzuild)],nonadv,[],[]).

a([both(onuitgesproken)],nonadv,[],[]).

a([e(onuitputtelijke),
   er(onuitputtelijker),
   ere(onuitputtelijkere),
   no_e(onuitputtelijk),
   st(onuitputtelijkst),
   ste(onuitputtelijkste)],nonadv,[],[]).

a([e(onuitroeibare),
   no_e(onuitroeibaar)],nonadv,[],[]).

a([e(onuitsprekelijke),
   er(onuitsprekelijker),
   ere(onuitsprekelijkere),
   no_e(onuitsprekelijk),
   st(onuitsprekelijkst),
   ste(onuitsprekelijkste)],adv,[],[]).

a([e(onuitstaanbare),
   er(onuitstaanbaarder),
   ere(onuitstaanbaardere),
   no_e(onuitstaanbaar),
   st(onuitstaanbaarst),
   ste(onuitstaanbaarste)],adv,[],[]).

a([e(onuitvoerbare),
   no_e(onuitvoerbaar)],nonadv,[subject_vp],[]).

a([e(onuitwisbare),
   er(onuitwisbaarder),
   ere(onuitwisbaardere),
   no_e(onuitwisbaar),
   st(onuitwisbaarst),
   ste(onuitwisbaarste)],nonadv,[],[]).

a([e(onvaste),
   er(onvaster),
   ere(onvastere),
   no_e(onvast)],adv,[],[]).

a([e(onveilige),
   er(onveiliger),
   ere(onveiligere),
   no_e(onveilig),
   st(onveiligst),
   ste(onveiligste)],adv,[],[]).

a([e(onveranderde),
   er(onveranderder),
   ere(onveranderdere),
   no_e(onveranderd),
   st(onveranderdst),
   ste(onveranderdste)],padv,[],[]).

a([e(onveranderlijke),
   er(onveranderlijker),
   ere(onveranderlijkere),
   no_e(onveranderlijk),
   st(onveranderlijkst),
   ste(onveranderlijkste)],adv,[],[]).

a([e(onverantwoorde),
   er(onverantwoorder),
   ere(onverantwoordere),
   no_e(onverantwoord),
   st(onverantwoordst),
   ste(onverantwoordste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(onverantwoordelijke),
   er(onverantwoordelijker),
   ere(onverantwoordelijkere),
   no_e(onverantwoordelijk),
   st(onverantwoordelijkst),
   ste(onverantwoordelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onverbeterlijke),
   er(onverbeterlijker),
   ere(onverbeterlijkere),
   no_e(onverbeterlijk),
   st(onverbeterlijkst),
   ste(onverbeterlijkste)],adv,[],[]).

a([e(onverbiddelijke),
   er(onverbiddelijker),
   ere(onverbiddelijkere),
   no_e(onverbiddelijk),
   st(onverbiddelijkst),
   ste(onverbiddelijkste)],adv,
  [pp(voor)],[]).

a([e(onverbloemde),
   er(onverbloemder),
   ere(onverbloemdere),
   no_e(onverbloemd),
   st(onverbloemdst),
   ste(onverbloemdste)],adv,[],[]).

a([e(onverbrekelijke),
   er(onverbrekelijker),
   ere(onverbrekelijkere),
   no_e(onverbrekelijk),
   st(onverbrekelijkst),
   ste(onverbrekelijkste)],adv,[],[]).

a([e(onverdeelde),
   no_e(onverdeeld)],adv,[],[]).

a([e(onverdiende),
   no_e(onverdiend)],adv,[],[]).

a([e(onverdraaglijke),
   er(onverdraaglijker),
   ere(onverdraaglijkere),
   no_e(onverdraaglijk),
   st(onverdraaglijkst),
   ste(onverdraaglijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(onverdraagzame),
   er(onverdraagzamer),
   ere(onverdraagzamere),
   no_e(onverdraagzaam),
   st(onverdraagzaamst),
   ste(onverdraagzaamste)],nonadv,[],[]).

a([both(onverdroten),
   er(onverdrotener),
   ere(onverdrotenere),
   st(onverdrotenst),
   ste(onverdrotenste)],adv,[],[]).

a([e(onverenigbare),
   er(onverenigbaarder),
   ere(onverenigbaardere),
   no_e(onverenigbaar),
   st(onverenigbaarst),
   ste(onverenigbaarste)],nonadv,
  [pp(met)],[]).

a([e(onvergankelijke),
   er(onvergankelijker),
   ere(onvergankelijkere),
   no_e(onvergankelijk),
   st(onvergankelijkst),
   ste(onvergankelijkste)],nonadv,[],[]).

a([e(onvergeeflijke),
   er(onvergeeflijker),
   ere(onvergeeflijkere),
   postn_no_e(onvergeeflijk),
   st(onvergeeflijkst),
   ste(onvergeeflijkste)],adv,
  [subject_sbar,
   pp(voor)],[]).

a([e(onvergelijkelijke),
   er(onvergelijkelijker),
   ere(onvergelijkelijkere),
   postn_no_e(onvergelijkelijk),
   st(onvergelijkelijkst),
   ste(onvergelijkelijkste)],adv,[],[]).

a([e(onvergetelijke),
   er(onvergetelijker),
   ere(onvergetelijkere),
   no_e(onvergetelijk),
   st(onvergetelijkst),
   ste(onvergetelijkste)],adv,[],[]).

a([e(onverhoedse),
   er(onverhoedser),
   ere(onverhoedsere),
   no_e(onverhoeds),
   st(onverhoedst),
   ste(onverhoedste)],adv,[],[]).

a([both(onverholen)],adv,[],[]).

a([e(onverhoopte),
   no_e(onverhoopt)],adv,[],[]).

a([e(onverkiesbare),
   no_e(onverkiesbaar)],adv,[],[]).

a([e(onverklaarbare),
   er(onverklaarbaarder),
   ere(onverklaarbaardere),
   no_e(onverklaarbaar),
   st(onverklaarbaarst),
   ste(onverklaarbaarste)],adv,[],[]).

a([e(onverkoopbare),
   no_e(onverkoopbaar)],adv,[],[]).

a([e(onverkorte),
   no_e(onverkort)],adv,[],[]).

a([e(onverlette),
   no_e(onverlet)],nonadv,[],[]).

a([e(onvermelde),
   no_e(onvermeld)],nonadv,
  [subject_sbar],[]).

a([e(onvermijdelijke),
   er(onvermijdelijker),
   ere(onvermijdelijkere),
   no_e(onvermijdelijk),
   st(onvermijdelijkst),
   ste(onvermijdelijkste)],adv,
  [subject_sbar],[]).

a([e(onverminderde),
   no_e(onverminderd)],adv,[],[]).

a([e(onvermoede),
   no_e(onvermoed)],adv,[],[]).

a([e(onvermoeibare),
   er(onvermoeibaarder),
   ere(onvermoeibaardere),
   no_e(onvermoeibaar),
   st(onvermoeibaarst),
   ste(onvermoeibaarste)],padv,[],[]).

a([no_e(onversaagd),
   e(onversaagde)],adv,[],[]).

a([e(onverschillige),
   er(onverschilliger),
   ere(onverschilligere),
   no_e(onverschillig),
   st(onverschilligst),
   ste(onverschilligste)],adv,
  [er_pp_sbar(voor),
   pp(voor)],[]).

a([both(onverschrokken),
   er(onverschrokkener),
   ere(onverschrokkenere),
   st(onverschrokkenst),
   ste(onverschrokkenste)],adv,[],[]).

a([e(onverslaanbare),
   no_e(onverslaanbaar)],padv,[],[]).

a([ge_both(onversneden)],nonadv,[],[]).

a([e(onverstaanbare),
   no_e(onverstaanbaar)],adv,[],[]).

a([e(onverstandige),
   er(onverstandiger),
   ere(onverstandigere),
   no_e(onverstandig),
   st(onverstandigst),
   ste(onverstandigste)],adv,
  [subject_vp,
   subject_sbar,
   object_vp],[]).

a([e(onverstoorbare),
   er(onverstoorbaarder),
   ere(onverstoorbaardere),
   no_e(onverstoorbaar),
   st(onverstoorbaarst),
   ste(onverstoorbaarste)],adv,[],[]).

a([e(onverteerbare),
   er(onverteerbaarder),
   ere(onverteerbaardere),
   no_e(onverteerbaar),
   st(onverteerbaarst),
   ste(onverteerbaarste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([both(onvertogen)],nonadv,[],[]).

a([e(onvervaarde),
   er(onvervaarder),
   ere(onvervaardere),
   no_e(onvervaard),
   st(onvervaardst),
   ste(onvervaardste)],padv,[],[]).

a([e(onvervalste),
   no_e(onvervalst)],adv,[],[]).

a([e(onvervangbare),
   no_e(onvervangbaar)],nonadv,[],[]).

a([e(onvervreemdbare),
   no_e(onvervreemdbaar)],adv,[],[]).

a([e(onvervulde),
   no_e(onvervuld)],nonadv,[],[]).

a([e(onverwachte),
   er(onverwachter),
   ere(onverwachtere),
   no_e(onverwacht),
   st(onverwachtst),
   ste(onverwachtste)],adv,
  [pp(voor),
   subject_sbar],[]).

a([e(onverwachtse),
   no_e(onverwachts)],adv,[],[]).

a([e(onverwijlde),
   no_e(onverwijld)],adv,[],[]).

a([e(onverwoestbare),
   no_e(onverwoestbaar)],nonadv,[],[]).

a([e(onverzadigbare),
   er(onverzadigbaarder),
   ere(onverzadigbaardere),
   no_e(onverzadigbaar),
   st(onverzadigbaarst),
   ste(onverzadigbaarste)],nonadv,[],[]).

a([e(onverzettelijke),
   er(onverzettelijker),
   ere(onverzettelijkere),
   no_e(onverzettelijk),
   st(onverzettelijkst),
   ste(onverzettelijkste)],adv,[],[]).

a([e(onverzoenlijke),
   er(onverzoenlijker),
   ere(onverzoenlijkere),
   no_e(onverzoenlijk),
   st(onverzoenlijkst),
   ste(onverzoenlijkste)],adv,[],[]).

a([ge_e(onverzorgde),
   er(onverzorgder),
   ere(onverzorgdere),
   ge_no_e(onverzorgd),
   st(onverzorgdst),
   ste(onverzorgdste)],nonadv,[],[]).

a([e(onvindbare),
   no_e(onvindbaar)],nonadv,
  [pp(voor)],[]).

a([both(onvoldoende)],adv,
  [object_vp,
   object_sbar,
   subject_sbar,
   subject_vp],[]).

a([ge_both(onvoldragen)],nonadv,[],[]).

a([ge_both(onvolprezen)],nonadv,[],[]).

a([e(onvolledige),
   er(onvollediger),
   ere(onvolledigere),
   no_e(onvolledig),
   st(onvolledigst),
   ste(onvolledigste)],adv,[],[]).

a([e(onvolmaakte),
   er(onvolmaakter),
   ere(onvolmaaktere),
   no_e(onvolmaakt),
   st(onvolmaaktst),
   ste(onvolmaaktste)],nonadv,[],[]).

a([e(onvoltooide),
   no_e(onvoltooid)],nonadv,[],[]).

a([both(onvolwassen)],adv,[],[]).

a([ge_e(onvoorbereide),
   er(onvoorbereider),
   ere(onvoorbereidere),
   ge_no_e(onvoorbereid),
   st(onvoorbereidst),
   ste(onvoorbereidste)],adv,
  [pp(op)],[]).

a([e(onvoorspelbare),
   no_e(onvoorspelbaar)],adv,
  [subject_sbar],[]).

a([e(onvoorstelbare),
   er(onvoorstelbaarder),
   ere(onvoorstelbaardere),
   no_e(onvoorstelbaar),
   st(onvoorstelbaarst),
   ste(onvoorstelbaarste)],adv,
  [subject_sbar],[]).

a([e(onvoorwaardelijke),
   er(onvoorwaardelijker),
   ere(onvoorwaardelijkere),
   no_e(onvoorwaardelijk),
   st(onvoorwaardelijkst),
   ste(onvoorwaardelijkste)],adv,[],[]).

a([e(onvoorzichtige),
   er(onvoorzichtiger),
   ere(onvoorzichtigere),
   no_e(onvoorzichtig),
   st(onvoorzichtigst),
   ste(onvoorzichtigste)],adv,
  [pp(met)],[]).

a([e(onvoorziene),
   no_e(onvoorzien)],adv,
  [subject_sbar],[]).

a([e(onvriendelijke),
   er(onvriendelijker),
   ere(onvriendelijkere),
   no_e(onvriendelijk),
   st(onvriendelijkst),
   ste(onvriendelijkste)],adv,[],[]).

a([e(onvrijwillige),
   er(onvrijwilliger),
   ere(onvrijwilligere),
   no_e(onvrijwillig),
   st(onvrijwilligst),
   ste(onvrijwilligste)],adv,[],[]).

a([e(onvruchtbare),
   er(onvruchtbaarder),
   ere(onvruchtbaardere),
   no_e(onvruchtbaar),
   st(onvruchtbaarst),
   ste(onvruchtbaarste)],nonadv,[],[]).

a([e(onware),
   er(onwaarder),
   ere(onwaardere),
   no_e(onwaar),
   st(onwaarst),
   ste(onwaarste)],nonadv,[subject_sbar],[]).

a([e(onwaardige),
   er(onwaardiger),
   ere(onwaardigere),
   no_e(onwaardig),
   st(onwaardigst),
   ste(onwaardigste)],adv,
  [so_np,
   so_np_subject_sbar,
   so_np_subject_vp],[]).

a([e(onwaarschijnlijke),
   er(onwaarschijnlijker),
   ere(onwaarschijnlijkere),
   no_e(onwaarschijnlijk),
   st(onwaarschijnlijkst),
   ste(onwaarschijnlijkste)],adv,
  [subject_sbar],[]).

a([e(onwankelbare),
   er(onwankelbaarder),
   ere(onwankelbaardere),
   no_e(onwankelbaar),
   st(onwankelbaarst),
   ste(onwankelbaarste)],adv,[],[]).

a([e(onweerlegbare),
   er(onweerlegbaarder),
   ere(onweerlegbaardere),
   no_e(onweerlegbaar),
   st(onweerlegbaarst),
   ste(onweerlegbaarste)],adv,[],[]).

a([both(onweersproken)],padv,
  [subject_sbar],[]).

a([e(onweerstaanbare),
   er(onweerstaanbaarder),
   ere(onweerstaanbaardere),
   no_e(onweerstaanbaar),
   st(onweerstaanbaarst),
   ste(onweerstaanbaarste)],adv,[],[]).

a([pred(onwel)],padv,[],[]).

a([e(onwelgevallige),
   no_e(onwelgevallig)],nonadv,
  [so_np],[]).

a([e(onwelgezinde),
   no_e(onwelgezind)],nonadv,
  [so_np],[]).

a([e(onwelkome),
   no_e(onwelkom)],nonadv,
  [so_np],[]).

a([e(onwennige),
   no_e(onwennig)],adv,
  [subject_vp,
   pp(in),
   pp(voor)],[]).

a([e(onwenselijke),
   no_e(onwenselijk)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(onwerkelijke),
   er(onwerkelijker),
   ere(onwerkelijkere),
   no_e(onwerkelijk),
   st(onwerkelijkst),
   ste(onwerkelijkste)],adv,   % onwerkelijk lang
  [subject_sbar,
   subject_vp],[]).		

a([ende(onwetende),
   er(onwetender),
   ere(onwetendere),
   end(onwetend),
   st(onwetendst),
   ste(onwetendste)],padv,[],[]).

a([e(onwetenschappelijke),
   er(onwetenschappelijker),
   ere(onwetenschappelijkere),
   no_e(onwetenschappelijk),
   st(onwetenschappelijkst),
   ste(onwetenschappelijkste)],adv,[],[]).

a([e(onwettige),
   er(onwettiger),
   ere(onwettigere),
   no_e(onwettig),
   st(onwettigst),
   ste(onwettigste)],adv,[],[]).

a([e(onwezenlijke),
   er(onwezenlijker),
   ere(onwezenlijkere),
   no_e(onwezenlijk),
   st(onwezenlijkst),
   ste(onwezenlijkste)],nonadv,[subject_sbar,
				subject_vp],[]).

a([e(onwillekeurige),
   er(onwillekeuriger),
   ere(onwillekeurigere),
   no_e(onwillekeurig),
   st(onwillekeurigst),
   ste(onwillekeurigste)],adv,[],[]).

a([e(onwillige),
   er(onwilliger),
   ere(onwilligere),
   no_e(onwillig),
   st(onwilligst),
   ste(onwilligste)],adv,[],[]).

a([e(onwrikbare),
   er(onwrikbaarder),
   ere(onwrikbaardere),
   no_e(onwrikbaar),
   st(onwrikbaarst),
   ste(onwrikbaarste)],adv,[],[]).

a([e(onzalige),
   er(onzaliger),
   ere(onzaligere),
   no_e(onzalig),
   st(onzaligst),
   ste(onzaligste)],adv,[],[]).

a([e(onzedige),
   er(onzediger),
   ere(onzedigere),
   no_e(onzedig),
   st(onzedigst),
   ste(onzedigste)],adv,[],[]).

a([e(onzekere),
   er(onzekerder),
   ere(onzekerdere),
   no_e(onzeker),
   st(onzekerst),
   ste(onzekerste)],adv,
  [subject_sbar_no_het,
   object_sbar,
   er_pp_sbar(over),
   pp(door),
   pp(over),
   pp(voor)],[]).

a([e(onzichtbare),
   er(onzichtbaarder),
   ere(onzichtbaardere),
   no_e(onzichtbaar),
   st(onzichtbaarst),
   ste(onzichtbaarste)],padv,
  [pp(voor)],[]).

a([e(onzijdige),
   no_e(onzijdig)],nonadv,[],[]).

a([e(onzinnige),
   er(onzinniger),
   ere(onzinnigere),
   no_e(onzinnig),
   st(onzinnigst),
   ste(onzinnigste)],adv,
  [subject_vp],[]).

a([e(onzorgvuldige),
   er(onzorgvuldiger),
   ere(onzorgvuldigere),
   no_e(onzorgvuldig),
   st(onzorgvuldigst),
   ste(onzorgvuldigste)],adv,
  [subject_vp,
   pp(bij),
   pp(in),
   pp(met)],[]).

a([e(onzuivere),
   er(onzuiverder),
   ere(onzuiverdere),
   no_e(onzuiver),
   st(onzuiverst),
   ste(onzuiverste)],adv,[],[]).

a([pred([oog,in,oog])],padv,
  [pp(met)],[]).

a([ende(oogluikende),
   end(oogluikend)],adv,[],[]).

a([e(oogverblindende),
   er(oogverblindender),
   ere(oogverblindendere),
   no_e(oogverblindend),
   st(oogverblindendst),
   ste(oogverblindendste)],adv,[],[]).

a([e(oorspronkelijke),
   er(oorspronkelijker),
   ere(oorspronkelijkere),
   no_e(oorspronkelijk),
   st(oorspronkelijkst),
   ste(oorspronkelijkste)],adv,[],[]).

a([e(oorverdovende),
   er(oorverdovender),
   ere(oorverdovendere),
   no_e(oorverdovend),
   st(oorverdovendst),
   ste(oorverdovendste)],adv,[],[]).

a([e(oorzakelijke),
   no_e(oorzakelijk)],nonadv,[],[]).

a([pred(oost)],nonadv,[],[]).

a([e(oostelijke),
   er(oostelijker),
   ere(oostelijkere),
   no_e(oostelijk),
   st(oostelijkst),
   ste(oostelijkste)],adv,[],[]).

a([e(oosterse),
   no_e(oosters)],adv,[],[]).

a([e(oostwaartse),
   postn_no_e(oostwaarts)],diradv,[],[]).

a([pred(op)],nonadv,[],[]).

a([pred([op,de,hoogte])],padv,
  [pp(van),
   er_pp_sbar(van),
   object_sbar],[]).

a([pred([op,handen])],nonadv,[],[]).

a([pred(ophanden)],nonadv,[],[]).

a([pred([op,dreef])],nonadv,[],[]).

a([pred([op,en,top])],nonadv,[],[]).

a([pred([op,jacht])],padv,
  [pp(naar)],[]).

a([pred([op,komst])],nonadv,[],[]). % met de baby op komst

a([pred([op,losse,schroeven])],nonadv,[],[]).

a([pred([op,inactief])],nonadv,[],[]).

a([pred([op,'non-actief']),
   pred([op,nonactief])],nonadv,[],[]).

a([pred([op,scherp])],nonadv,[],[]).

a([pred([op,streek])],nonadv,[],[]).

a([pred([op,slot])],nonadv,[],[]).

a([pred([op,til])],nonadv,[],[]).

a([pred([op,weg])],dir_locadv,
  [object_vp],[]).

a([pred([op,zoek]),
   pred(opzoek)],padv,
  [pp(naar)],[]).

a([e(opdringerige),
   er(opdringeriger),
   ere(opdringerigere),
   no_e(opdringerig),
   st(opdringerigst),
   ste(opdringerigste)],adv,[],[]).

a([ge_e(opeengevolgde),
   ge_no_e(opeengevolgd)],adv,[],[]).

a([both(open),
   er(opener),
   ere(openere)],adv,[],[]).

a([e(openbare),
   er(openbaarder),
   ere(openbaardere),
   no_e(openbaar),
   st(openbaarst),
   ste(openbaarste)],adv,[],[]).

a([ge_both(opengebarsten)],adv,[],[]).

a([ge_both(opengebroken)],adv,[],[]).

a([ge_e(opengedane),
   ge_no_e(opengedaan)],adv,[],[]).

a([ge_e(opengedraaide),
   ge_no_e(opengedraaid)],adv,[],[]).

a([ge_e(opengegane),
   ge_no_e(opengegaan)],adv,[],[]).

a([ge_e(opengegooide),
   ge_no_e(opengegooid)],adv,[],[]).

a([ge_both(opengehouden)],adv,[],[]).

a([ge_e(opengeklapte),
   ge_no_e(opengeklapt)],padv,[],[]).

a([ge_both(opengelaten)],adv,[],[]).

a([ge_e(opengemaakte),
   ge_no_e(opengemaakt)],adv,[],[]).

a([ge_both(opengereten)],adv,[],[]).

a([ge_e(opengescheurde),
   ge_no_e(opengescheurd)],adv,[],[]).

a([ge_both(opengeschoven)],adv,[],[]).

a([ge_both(opengeslagen)],adv,[],[]).

a([ge_e(opengesperde),
   ge_no_e(opengesperd)],adv,[],[]).

a([ge_both(opengesprongen)],adv,[],[]).

a([ge_e(opengestelde),
   ge_no_e(opengesteld)],adv,[],[]).

a([ge_both(opengetrokken)],adv,[],[]).

a([ge_both(opengevallen)],adv,[],[]).

a([ge_both(opengevouwen)],adv,[],[]).

a([ge_e(opengezette),
   ge_no_e(opengezet)],adv,[],[]).

a([e(openhartige),
   er(openhartiger),
   ere(openhartigere),
   no_e(openhartig),
   st(openhartigst),
   ste(openhartigste)],adv,[],[]).

a([e(openlijke),
   er(openlijker),
   ere(openlijkere),
   no_e(openlijk),
   st(openlijkst),
   ste(openlijkste)],adv,[],[]).

a([e(operatieve),
   no_e(operatief)],adv,[],[]).

a([e(operationele),
   no_e(operationeel)],adv,[],[]).

a([ge_e(opgebaarde),
   ge_no_e(opgebaard)],adv,[],[]).

a([ge_e(opgebelde),
   ge_no_e(opgebeld)],adv,[],[]).

a([ge_e(opgebiechte),
   ge_no_e(opgebiecht)],adv,[],[]).

a([ge_both(opgeblazen),
   er(opgeblazener),
   ere(opgeblazenere),
   st(opgeblazenst),
   ste(opgeblazenste)],adv,[],[]).

a([ge_both(opgebleven)],adv,[],[]).

a([ge_e(opgebloeide),
   ge_no_e(opgebloeid)],adv,[],[]).

a([ge_e(opgebolde),
   ge_no_e(opgebold)],adv,[],[]).

a([ge_both(opgeborgen)],adv,[],[]).

a([ge_e(opgeborrelde),
   ge_no_e(opgeborreld)],adv,[],[]).

a([ge_e(opgebouwde),
   ge_no_e(opgebouwd)],adv,[],[]).

a([ge_e(opgebrachte),
   ge_no_e(opgebracht)],adv,[],[]).

a([ge_e(opgebrande),
   ge_no_e(opgebrand)],adv,[],[]).

a([ge_both(opgebroken)],adv,[],[]).

a([ge_e(opgebruikte),
   ge_no_e(opgebruikt)],adv,[],[]).

a([ge_e(opgedaagde),
   ge_no_e(opgedaagd)],adv,[],[]).

a([ge_e(opgedane),
   ge_no_e(opgedaan)],adv,[],[]).

a([ge_e(opgedeelde),
   ge_no_e(opgedeeld)],adv,[],[]).

a([ge_no_e(opgediend),
   ge_e(opgediende)],adv,[],[]).

a([ge_e(opgediepte),
   ge_no_e(opgediept)],adv,[],[]).

a([ge_e(opgediste),
   ge_no_e(opgedist)],adv,[],[]).

a([ge_e(opgedoekte),
   ge_no_e(opgedoekt)],padv,[],[]).

a([ge_e(opgedoemde),
   ge_no_e(opgedoemd)],adv,[],[]).

a([ge_e(opgedofte),
   ge_no_e(opgedoft)],padv,[],[]).

a([ge_both(opgedoken)],adv,[],[]).

a([ge_e(opgedraafde),
   ge_no_e(opgedraafd)],adv,[],[]).

a([ge_e(opgedraaide),
   ge_no_e(opgedraaid)],adv,[],[]).

a([ge_both(opgedragen)],adv,
  [so_np,
   so_pp(aan)],[]).

a([ge_both(opgedreven)],adv,[],[]).

a([ge_both(opgedrongen)],adv,
  [so_np],[]).

a([ge_both(opgedronken)],adv,[],[]).

a([ge_e(opgedroogde),
   ge_no_e(opgedroogd)],adv,[],[]).

a([ge_e(opgefleurde),
   ge_no_e(opgefleurd)],padv,[],[]).

a([ge_e(opgefokte),
   ge_no_e(opgefokt)],adv,[],[]).

a([ge_e(opgefriste),
   ge_no_e(opgefrist)],padv,[],[]).

a([ge_e(opgegane),
   ge_no_e(opgegaan)],adv,[],[]).

a([ge_both(opgegeten)],adv,[],[]).

a([ge_both(opgegeven)],adv,[],[]).

a([ge_both(opgegraven)],adv,[],[]).

a([ge_e(opgegroeide),
   ge_no_e(opgegroeid)],adv,[],[]).

a([ge_e(opgehaalde),
   ge_no_e(opgehaald)],adv,[],[]).

a([ge_both(opgehangen)],adv,[],[]).

a([ge_e(opgehelderde),
   ge_no_e(opgehelderd)],adv,[],[]).

a([ge_both(opgehesen)],padv,[],[]).

a([ge_both(opgeheven)],adv,[],[]).

a([ge_e(opgehitste),
   ge_no_e(opgehitst)],adv,[],[]).

a([ge_e(opgehoeste),
   ge_no_e(opgehoest)],adv,[],[]).

a([ge_e(opgehoopte),
   ge_no_e(opgehoopt)],adv,[],[]).

a([ge_both(opgehouden)],adv,[],[]).

a([ge_e(opgejaagde),
   ge_no_e(opgejaagd)],adv,[],[]).

a([ge_e(opgejutte),
   ge_no_e(opgejut)],padv,[],[]).

a([ge_both(opgekeken)],adv,[],[]).

a([ge_e(opgeklaarde),
   ge_no_e(opgeklaard)],adv,[],[]).

a([ge_both(opgeklommen)],adv,[],[]).

a([ge_no_e(opgeklopt),
   ge_e(opgeklopte)],adv,[],[]).

a([ge_e(opgeknapte),
   ge_no_e(opgeknapt)],adv,[],[]).

a([ge_e(opgekochte),
   ge_no_e(opgekocht)],adv,[],[]).

a([ge_both(opgekomen)],adv,[],[]).

a([ge_e(opgekrikte),
   ge_no_e(opgekrikt)],adv,[],[]).

a([ge_e(opgekropte),
   ge_no_e(opgekropt)],adv,[],[]).

a([ge_e(opgekweekte),
   ge_no_e(opgekweekt)],adv,[],[]).

a([ge_e(opgelaaide),
   ge_no_e(opgelaaid)],adv,[],[]).

a([ge_both(opgeladen)],adv,[],[]).

a([ge_e(opgelapte),
   ge_no_e(opgelapt)],adv,[],[]).

a([ge_both(opgelaten)],padv,[],[]).

a([ge_e(opgeleefde),
   ge_no_e(opgeleefd)],adv,[],[]).

a([ge_e(opgelegde),
   ge_no_e(opgelegd)],adv,
  [so_pp(aan),
   pp(door),
   so_np],[]).

a([ge_e(opgeleide),
   ge_no_e(opgeleid)],adv,[],[]).

a([ge_both(opgelet)],adv,[],[]).

a([ge_e(opgeleverde),
   ge_no_e(opgeleverd)],adv,[],[]).

a([ge_e(opgelichte),
   ge_no_e(opgelicht)],adv,[],[]).

a([ge_both(opgelopen)],adv,[],[]).

a([ge_e(opgeloste),
   ge_no_e(opgelost)],adv,[],[]).

a([ge_e(opgeluchte),
   er(opgeluchter),
   ere(opgeluchtere),
   ge_no_e(opgelucht),
   st(opgeluchtst),
   ste(opgeluchtste)],padv,
  [object_sbar],[]).

a([ge_e(opgeluisterde),
   ge_no_e(opgeluisterd)],padv,[],[]).

a([ge_e(opgemaakte),
   ge_no_e(opgemaakt)],adv,[],[]).

a([ge_e(opgemerkte),
   ge_no_e(opgemerkt)],adv,[],[]).

a([ge_both(opgemeten)],adv,[],[]).

a([ge_e(opgenoemde),
   ge_no_e(opgenoemd)],adv,[],[]).

a([ge_both(opgenomen)],adv,[],[]).

a([ge_e(opgeofferde),
   ge_no_e(opgeofferd)],adv,[],[]).

a([ge_e(opgepakte),
   ge_no_e(opgepakt)],adv,[],[]).

a([ge_e(opgepepte),
   ge_no_e(opgepept)],adv,[],[]).

a([ge_e(opgepikte),
   ge_no_e(opgepikt)],adv,[],[]).

a([ge_e(opgeplakte),
   ge_no_e(opgeplakt)],adv,[],[]).

a([ge_e(opgepoetste),
   ge_no_e(opgepoetst)],adv,[],[]).

a([ge_e(opgepompte),
   ge_no_e(opgepompt)],padv,[],[]).

a([ge_e(opgepotte),
   ge_no_e(opgepot)],adv,[],[]).

a([ge_e(opgepropte),
   ge_no_e(opgepropt)],padv,[],[]).

a([ge_e(opgeraapte),
   ge_no_e(opgeraapt)],adv,[],[]).

a([ge_e(opgerakelde),
   ge_no_e(opgerakeld)],nonadv,[],[]).

a([ge_both(opgereden)],adv,[],[]).

a([ge_no_e(opgerekt),
   ge_e(opgerekte)],padv,[],[]).

a([ge_both(opgerezen)],adv,[],[]).

a([ge_e(opgerichte),
   ge_no_e(opgericht)],adv,[],[]).

a([ge_both(opgeroepen)],adv,[],[]).

a([ge_e(opgerolde),
   ge_no_e(opgerold)],adv,[],[]).

a([ge_e(opgeruimde),
   er(opgeruimder),
   ere(opgeruimdere),
   ge_no_e(opgeruimd),
   st(opgeruimdst),
   ste(opgeruimdste)],adv,[],[]).

a([ge_e(opgerukte),
   ge_no_e(opgerukt)],adv,[],[]).

a([ge_e(opgescheepte),
   ge_no_e(opgescheept)],adv,[],[]).

a([ge_e(opgeschepte),
   ge_no_e(opgeschept)],adv,[],[]).

a([ge_e(opgeschorte),
   ge_no_e(opgeschort)],adv,[],[]).

a([ge_both(opgeschoten)],adv,[],[]).

a([ge_both(opgeschoven)],adv,[],[]).

a([ge_both(opgeschreven)],adv,[],[]).

a([ge_e(opgeschrikte),
   ge_no_e(opgeschrikt)],adv,[],[]).

a([ge_e(opgeschroefde),
   ge_no_e(opgeschroefd)],nonadv,[],[]).

a([ge_e(opgeschudde),
   ge_no_e(opgeschud)],nonadv,[],[]).

a([ge_e(opgesierde),
   ge_no_e(opgesierd)],padv,[],[]).

a([ge_both(opgeslagen)],adv,[],[]).

a([ge_e(opgeslokte),
   ge_no_e(opgeslokt)],adv,[],[]).

a([ge_e(opgeslorpte),
   ge_no_e(opgeslorpt)],adv,[],[]).

a([ge_both(opgesloten)],adv,[],[]).

a([e(opgesmukte),
   no_e(opgesmukt)],padv,[],[]).

a([ge_both(opgesnoven)],adv,[],[]).

a([ge_e(opgesomde),
   ge_no_e(opgesomd)],adv,[],[]).

a([ge_e(opgespaarde),
   ge_no_e(opgespaard)],adv,[],[]).

a([ge_e(opgespatte),
   ge_no_e(opgespat)],adv,[],[]).

a([ge_e(opgesplitste),
   ge_no_e(opgesplitst)],adv,[],[]).

a([ge_e(opgespoorde),
   ge_no_e(opgespoord)],adv,[],[]).

a([ge_both(opgespoten)],adv,[],[]).

a([ge_both(opgesprongen)],adv,[],[]).

a([ge_e(opgestane),
   ge_no_e(opgestaan)],adv,[],[]).

a([ge_e(opgestapelde),
   ge_no_e(opgestapeld)],adv,[],[]).

a([ge_e(opgestapte),
   ge_no_e(opgestapt)],adv,[],[]).

a([ge_both(opgestegen)],adv,[],[]).

a([ge_e(opgestelde),
   ge_no_e(opgesteld)],adv,[],[]).

a([ge_both(opgestoken)],adv,[],[]).

a([ge_e(opgestookte),
   ge_no_e(opgestookt)],adv,[],[]).

a([ge_both(opgestreken)],adv,[],[]).

a([ge_e(opgestroopte),
   ge_no_e(opgestroopt)],adv,[],[]).

a([ge_both(opgestoven)],adv,[],[]).

a([ge_e(opgestuurde),
   ge_no_e(opgestuurd)],adv,[],[]).

a([ge_e(opgestuwde),
   ge_no_e(opgestuwd)],adv,[],[]).

a([ge_e(opgetakelde),
   ge_no_e(opgetakeld)],nonadv,[],[]).

a([ge_e(opgetaste),
   ge_no_e(opgetast)],adv,[],[]).

a([ge_e(opgetekende),
   ge_no_e(opgetekend)],adv,[],[]).

a([ge_e(opgetelde),
   ge_no_e(opgeteld)],adv,[],[]).

a([ge_e(opgetilde),
   ge_no_e(opgetild)],adv,[],[]).

a([both(opgetogen),
   er(opgetogener),
   ere(opgetogenere),
   st(opgetogenst),
   ste(opgetogenste)],padv,
  [er_pp_sbar(over),
   pp(over),
   object_vp,
   object_sbar],[]).

a([ge_both(opgetreden)],adv,[],[]).

a([ge_both(opgetrokken)],adv,[],[]).

a([ge_e(opgetrommelde),
   ge_no_e(opgetrommeld)],adv,[],[]).

a([ge_e(opgetuigde),
   ge_no_e(opgetuigd)],adv,[],[]).

a([ge_e(opgetutte),
   ge_no_e(opgetut)],padv,[],[]).

a([ge_both(opgevallen)],adv,[],[]).

a([ge_both(opgevangen)],adv,[],[]).

a([ge_e(opgevatte),
   ge_no_e(opgevat)],adv,[],[]).

a([ge_e(opgeviste),
   ge_no_e(opgevist)],adv,[],[]).

a([ge_e(opgevlamde),
   ge_no_e(opgevlamd)],adv,[],[]).

a([ge_both(opgevlogen)],adv,[],[]).

a([ge_e(opgevoede),
   ge_no_e(opgevoed)],adv,[],[]).

a([ge_e(opgevoerde),
   ge_no_e(opgevoerd)],adv,[],[]).

a([ge_e(opgevolgde),
   ge_no_e(opgevolgd)],adv,[],[]).

a([ge_both(opgevouwen)],adv,[],[]).

a([ge_e(opgevraagde),
   ge_no_e(opgevraagd)],adv,[],[]).

a([ge_both(opgevreten)],adv,[],[]).

a([ge_e(opgevrolijkte),
   ge_no_e(opgevrolijkt)],adv,[],[]).

a([ge_both(opgevroren)],adv,[],[]).

a([ge_e(opgevulde),
   ge_no_e(opgevuld)],adv,[],[]).

a([ge_e(opgewaaide),
   ge_no_e(opgewaaid)],adv,[],[]).

a([ge_e(opgewaardeerde),
   ge_no_e(opgewaardeerd)],padv,[],[]).

a([ge_e(opgewachte),
   ge_no_e(opgewacht)],adv,[],[]).

a([ge_e(opgewarmde),
   ge_no_e(opgewarmd)],adv,[],[]).

a([ge_both(opgewassen)],adv,
  [pp(tegen)],[]).

a([ge_e(opgewekte),
   er(opgewekter),
   ere(opgewektere),
   ge_no_e(opgewekt),
   st(opgewektst),
   ste(opgewektste)],adv,[],[]).

a([ge_e(opgewelde),
   ge_no_e(opgeweld)],adv,[],[]).

a([ge_e(opgewerkte),
   ge_no_e(opgewerkt)],adv,[],[]).

a([ge_both(opgewogen)],adv,[],[]).

a([ge_both(opgewonden),
   er(opgewondener),
   ere(opgewondenere),
   st(opgewondenst),
   ste(opgewondenste)],adv,
  [pp(over)],[]).

a([ge_both(opgeworpen)],adv,[],[]).

a([ge_e(opgezadelde),
   ge_no_e(opgezadeld)],adv,[],[]).

a([ge_e(opgezegde),
   ge_no_e(opgezegd)],adv,[],[]).

a([ge_e(opgezette),
   ge_no_e(opgezet)],adv,
  [pp(met),          % VL?
   er_pp_sbar(met)   % VL?
  ],
  []).

a([ge_both(opgezeten)],adv,[],[]).

a([ge_both(opgezien)],adv,[],[]).

a([ge_e(opgezochte),
   ge_no_e(opgezocht)],adv,[],[]).

a([ge_both(opgezogen)],adv,[],[]).

a([ge_e(opgezweepte),
   ge_no_e(opgezweept)],adv,[],[]).

a([ge_e(opgezwelde),
   ge_no_e(opgezweld)],padv,[],[]).

a([ge_both(opgezwollen)],padv,[],[]).

a([ge_e(opgeëiste),
   ge_no_e(opgeëist)],adv,[],[]).

a([e(oplettende),
   er(oplettender),
   ere(oplettendere),
   no_e(oplettend),
   st(oplettendst),
   ste(oplettendste)],adv,[],[]).

a([ende(oplopende),
   er(oplopender),
   ere(oplopendere),
   end(oplopend),
   st(oplopendst),
   ste(oplopendste)],adv,
  [pp(in),
   pp(met),
   pp(naar),
   pp(tot)],[]).

a([e(oplosbare),
   er(oplosbaarder),
   ere(oplosbaardere),
   no_e(oplosbaar),
   st(oplosbaarst),
   ste(oplosbaarste)],nonadv,[],[]).

a([e(opmerkelijke),
   er(opmerkelijker),
   ere(opmerkelijkere),
   no_e(opmerkelijk),
   st(opmerkelijkst),
   ste(opmerkelijkste)],adv,
  [subject_sbar_no_het,
   pp(aan),
   pp(bij),
   pp(in)],[]).

a([e(opmerkzame),
   er(opmerkzamer),
   ere(opmerkzamere),
   no_e(opmerkzaam),
   st(opmerkzaamst),
   ste(opmerkzaamste)],adv,
  [er_pp_sbar(op),
   pp(op)],[]).

a([e(opperbeste),
   no_e(opperbest)],adv,[],[]).

a([e(oppermachtige),
   er(oppermachtiger),
   ere(oppermachtigere),
   no_e(oppermachtig),
   st(oppermachtigst),
   ste(oppermachtigste)],adv,[],[]).

a([e(opperste),
   no_e(opperst)],nonadv,[],[]).

a([e(oppervlakkige),
   er(oppervlakkiger),
   ere(oppervlakkigere),
   no_e(oppervlakkig),
   st(oppervlakkigst),
   ste(oppervlakkigste)],adv,[],[]).

a([e(opportunistische),
   er(opportunistischer),
   ere(opportunistischere),
   no_e(opportunistisch),
   st(opportunistischt),
   ste(opportunistischte)],adv,[],[]).

a([e(opportune),
   no_e(opportuun)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([no_e(oppositioneel),
   e(oppositionele)],adv,[],[]).

a([e(oprechte),
   er(oprechter),
   ere(oprechtere),
   no_e(oprecht),
   st(oprechtst),
   ste(oprechtste)],adv,[],[]).

a([e(oprolbare),
   no_e(oprolbaar)],adv,[],[]).

a([e(opstandige),
   er(opstandiger),
   ere(opstandigere),
   no_e(opstandig),
   st(opstandigst),
   ste(opstandigste)],padv,
  [pp(tegen)],[]).

a([prefix('opt-out')],nonadv,[],[]).

a([e(optimale),
   no_e(optimaal)],adv,[],[]).

a([e(optimistische),
   er(optimistischer),
   ere(optimistischere),
   no_e(optimistisch),
   st(optimistischt),
   ste(optimistischte)],adv,
  [object_sbar,
   subject_vp,
   er_pp_sbar(over),
   er_pp_vp(over),
   pp(over)],[]).

a([e(optionele),
   no_e(optioneel)],adv,[],[]).

a([e(optische),
   no_e(optisch)],adv,[],[]).

a([e(opvallende),
   er(opvallender),
   ere(opvallendere),
   no_e(opvallend),
   st(opvallendst),
   ste(opvallendste)],adv,
  [subject_vp,
   subject_vp_sbar,
   subject_sbar],[]).

a([e(opvoedbare),
   no_e(opvoedbaar)],nonadv,[],[]).

a([e(opvoedkundige),
   no_e(opvoedkundig)],adv,[],[]).

a([e(opvraagbare),
   no_e(opvraagbaar)],nonadv,[],[]).

a([e(opwaartse),
   postn_no_e(opwaarts)],diradv,[],[]).

a([e(opwekkende),
   er(opwekkender),
   ere(opwekkendere),
   no_e(opwekkend),
   st(opwekkendst),
   ste(opwekkendste)],adv,[transitive],[]).

a([e(opwindende),
   er(opwindender),
   ere(opwindendere),
   no_e(opwindend),
   st(opwindendst),
   ste(opwindendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(opzettelijke),
   er(opzettelijker),
   ere(opzettelijkere),
   no_e(opzettelijk),
   st(opzettelijkst),
   ste(opzettelijkste)],adv,[],[]).

a([e(opzichtige),
   er(opzichtiger),
   ere(opzichtigere),
   no_e(opzichtig),
   st(opzichtigst),
   ste(opzichtigste)],adv,[],[]).

a([e(opzienbarende),
   er(opzienbarender),
   ere(opzienbarendere),
   no_e(opzienbarend),
   st(opzienbarendst),
   ste(opzienbarendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(orale),
   no_e(oraal)],adv,[],[]).

a([both(oranje)],nonadv,[],[]).

a([e(ordelijke),
   er(ordelijker),
   ere(ordelijkere),
   no_e(ordelijk),
   st(ordelijkst),
   ste(ordelijkste)],adv,[],[]).

a([e(ordeloze),
   er(ordelozer),
   ere(ordelozere),
   no_e(ordeloos),
   st(ordeloost),
   ste(ordelooste)],adv,[],[]).

a([e(ordinaire),
   er(ordinairder),
   ere(ordinairdere),
   no_e(ordinair),
   st(ordinairst),
   ste(ordinairste)],adv,[],[]).

a([e(organisatorische),
   e(organizatorische),
   no_e(organisatorisch),
   no_e(organizatorisch)],adv,[],[]).

a([e(organische),
   no_e(organisch)],adv,[],[]).

a([e('Oriëntaalse'),
   no_e('Oriëntaals')],adv,[],[]).

a([e(originele),
   er(origineler),
   ere(originelere),
   no_e(origineel),
   st(origineelst),
   ste(origineelste),
   e(orginele),
   er(orgineler),
   ere(orginelere),
   no_e(orgineel),
   st(orgineelst),
   ste(orgineelste)],adv,[],[]).

a([e(oriëntaalse),
   no_e(oriëntaals)],adv,[],[]).

a([e(orthodoxe),
   e(ortodoxe),
   er(orthodoxer),
   er(ortodoxer),
   ere(orthodoxere),
   ere(ortodoxere),
   no_e(orthodox),
   no_e(ortodox),
   st(orthodoxt),
   st(ortodoxt),
   ste(orthodoxte),
   ste(ortodoxte)],adv,[],[h(ultra)]).

a([e(orthopedagogische),
   no_e(orthopedagogisch)],nonadv,[],[]).

a([e(ostentatieve),
   er(ostentatiever),
   ere(ostentatievere),
   no_e(ostentatief),
   st(ostentatiefst),
   ste(ostentatiefste)],adv,[],[]).

a([e(oubollige),
   no_e(oubollig)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(oude),
   er(ouder),
   ere(oudere),
   no_e(oud),
   st(oudst),
   ste(oudste)],adv,[],
  [i(eeuw,eeuwen),
   oer,
   stok]).

a([e(ouderlijke),
   no_e(ouderlijk)],nonadv,[],[]).

a([e(ouderwetse),
   er(ouderwetser),
   ere(ouderwetsere),
   no_e(ouderwets),
   st(ouderwetst),
   ste(ouderwetste)],adv,[],[]).

a([e(oudtestamentische),
   no_e(oudtestamentisch)],nonadv,[],[]).

a([pred(out)],nonadv,[],[]).  % Vlaams 

a([both([out,of,competition])],padv,[],[]).

a([pred([out,of,control])],nonadv,[],[]).

a([pred([out,of,the,question])],nonadv,[],[]).

a([both(outdoor)],adv,[],[]).

a([e(ovale),
   stof(ovalen),
   er(ovaler),
   ere(ovalere),
   no_e(ovaal),
   st(ovaalst),
   ste(ovaalste)],nonadv,[],[]).

a([e(ovationele),
   no_e(ovationeel)],nonadv,[],[]).

a([e(ovenvaste),
   no_e(ovenvast)],nonadv,[],[]).

a([pred(over)],nonadv,
  [pp(van)],[]).

a([pred([over,the,top])],padv,[],[]).

a([e(overbekende),
   postn_no_e(overbekend)],nonadv,[],[]).

a([ge_e(overbelaste),
   ge_no_e(overbelast)],adv,[],[]).

a([e(overbelichte),
   no_e(overbelicht)],adv,
  [subject_sbar],[]).

a([e(overbevolkte),
   er(overbevolkter),
   ere(overbevolktere),
   no_e(overbevolkt),
   st(overbevolktst),
   ste(overbevolktste)],nonadv,[],[]).

a([ge_e(overblufte),
   ge_no_e(overbluft)],adv,[],[]).

a([e(overbodige),
   er(overbodiger),
   ere(overbodigere),
   no_e(overbodig),
   st(overbodigst),
   ste(overbodigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(overboekte),
   ge_no_e(overboekt)],padv,[],[]).

a([ge_e(overbrugde),
   ge_no_e(overbrugd)],adv,[],[]).

a([ge_e(overdachte),
   ge_no_e(overdacht)],adv,[],[]).

a([e(overdadige),
   er(overdadiger),
   ere(overdadigere),
   no_e(overdadig),
   st(overdadigst),
   ste(overdadigste)],adv,[],[]).

a([ge_e(overdekte),
   ge_no_e(overdekt)],adv,[],[]).

a([ge_e(overdonderde),
   ge_no_e(overdonderd)],adv,[],[]).

a([e(overdraagbare),
   no_e(overdraagbaar)],nonadv,[],[]).

a([ge_both(overdreven),
   er(overdrevener),
   ere(overdrevenere),
   st(overdrevenst),
   ste(overdrevenste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_both(overeengekomen)],adv,[],[]).

a([e(overeenkomstige),
   no_e(overeenkomstig)],padv,[pp(met)],[]).

a([pred(overeind)],diradv,[],[]).

a([ende(overeindstaande),
   end(overeindstaand)],nonadv,[],[]).

a([ge_e(overerfde),
   ge_no_e(overerfd)],adv,[],[]).

a([ge_both(overgebleven)],adv,[],[]).

a([ge_e(overgebrachte),
   ge_no_e(overgebracht)],adv,[],[]).

a([ge_e(overgedane),
   ge_no_e(overgedaan)],adv,[],[]).

a([ge_both(overgedragen)],adv,[],[]).

a([ge_both(overgedreven)],adv,[],[]).

a([ge_e(overgegane),
   ge_no_e(overgegaan)],adv,[],[]).

a([ge_both(overgegeven),
   er(overgegevener),
   ere(overgegevenere),
   st(overgegevenst),
   ste(overgegevenste)],adv,[],[]).

a([ge_e(overgehaalde),
   ge_no_e(overgehaald)],adv,[],[]).

a([ge_both(overgehangen)],adv,[],[]).

a([ge_e(overgehevelde),
   ge_no_e(overgeheveld)],adv,[],[]).

a([ge_both(overgehouden)],adv,[],[]).

a([ge_both(overgekomen)],adv,[],[]).

a([ge_both(overgelaten)],adv,[],[]).

a([ge_e(overgelegde),
   ge_no_e(overgelegd)],adv,[],[]).

a([ge_e(overgeleverde),
   ge_no_e(overgeleverd)],adv,[],[]).

a([ge_both(overgelopen)],adv,[],[]).

a([ge_e(overgemaakte),
   ge_no_e(overgemaakt)],adv,[],[]).

a([ge_both(overgenomen)],adv,[],[]).

a([ge_e(overgeplaatste),
   ge_no_e(overgeplaatst)],adv,[],[]).

a([ge_both(overgeschoten)],adv,[],[]).

a([ge_both(overgeschreven)],adv,[],[]).

a([ge_both(overgeslagen)],adv,[],[]).

a([ge_both(overgesneden)],adv,[],[]).  % VL polsen

a([ge_both(overgesprongen)],adv,[],[]).

a([ge_e(overgestapte),
   ge_no_e(overgestapt)],adv,[],[]).

a([ge_both(overgestoken)],adv,[],[]).

a([ge_e(overgetroefde),
   ge_no_e(overgetroefd)],adv,[],[]).

a([ge_e(overgevloeide),
   ge_no_e(overgevloeid)],adv,[],[]).

a([ge_both(overgevlogen)],adv,[],[]).

a([e(overgevoelige),
   er(overgevoeliger),
   ere(overgevoeligere),
   no_e(overgevoelig),
   st(overgevoeligst),
   ste(overgevoeligste)],nonadv,
  [er_pp_sbar(voor),
   pp(voor)],[]).

a([ge_e(overgewaaide),
   ge_no_e(overgewaaid)],adv,[],[]).

a([ge_e(overgewaardeerde),
   ge_no_e(overgewaardeerd)],padv,[],[]).

a([ge_e(overgeërfde),
   ge_no_e(overgeërfd)],adv,[],[]).

a([ge_both(overgoten)],adv,[],[]).

a([e(overgroeide),
   no_e(overgroeid)],nonadv,[],[]).

a([e(overgrote),
   er(overgroter),
   ere(overgrotere),
   no_e(overgroot),
   st(overgrootst),
   ste(overgrootste)],nonadv,[],[]).

a([ge_e(overhaaste),
   ge_no_e(overhaast)],adv,[],[]).

a([ge_e(overhandigde),
   ge_no_e(overhandigd)],adv,[],[]).

a([ge_e(overheerste),
   ge_no_e(overheerst)],adv,[],[]).

a([pred(overhoop)],nonadv,[],[]).

a([ge_e(overhoorde),
   ge_no_e(overhoord)],adv,[],[]).

a([e(overige),
   no_e(overig)],nonadv,[],[]).

a([e(overjarige),
   no_e(overjarig)],nonadv,[],[]).

a([ge_e(overkapte),
   ge_no_e(overkapt)],padv,[],[]).

a([ge_e(overkoepelde),
   ge_no_e(overkoepeld)],adv,[],[]).

a([ge_e(overladene),
   ge_both(overladen)],padv,[],[]).

a([e(overlangse),
   no_e(overlangs)],adv,[],[]).

a([ge_e(overlapte),
   ge_no_e(overlapt)],adv,[],[]).

a([ge_both(overleden)],adv,[],[]).

a([ge_e(overleefde),
   ge_no_e(overleefd)],adv,[],[]).

a([ge_e(overlegde),
   ge_no_e(overlegd)],adv,[],[]).

a([ge_e(overmande),
   ge_no_e(overmand)],adv,[],[]).

a([e(overmatige),
   no_e(overmatig)],adv,[],[]).

a([ge_e(overmeesterde),
   ge_no_e(overmeesterd)],adv,[],[]).

a([e(overmoedige),
   er(overmoediger),
   ere(overmoedigere),
   no_e(overmoedig),
   st(overmoedigst),
   ste(overmoedigste)],adv,[],[]).

a([ge_both(overreden)],adv,[],[]).

a([ge_e(overrede),
   ge_no_e(overreed)],adv,[],[]).

a([ge_both(overroepen)],padv,[],[]).  % Vlaams

a([ge_e(overrompelde),
   ge_no_e(overrompeld)],adv,[],[]).

a([ge_e(overschaduwde),
   ge_no_e(overschaduwd)],adv,[],[]).

a([ge_e(overschatte),
   ge_no_e(overschat)],adv,[],[]).

a([ge_both(overschreden)],adv,[],[]).

a([both(oversized)],padv,[],[]).

a([ge_both(overspannen),
   er(overspannener),
   ere(overspannenere),
   st(overspannenst),
   ste(overspannenste)],adv,[],[]).

a([e(overspelige),
   no_e(overspelig)],adv,[],[]).

a([ge_e(overspoelde),
   ge_no_e(overspoeld)],adv,[],[]).

a([pred(overstag)],nonadv,[],[]).

a([ge_both(overstegen)],adv,[],[]).

a([ge_e(overstelpte),
   ge_no_e(overstelpt)],adv,[],[]).

a([ge_e(overstemde),
   ge_no_e(overstemd)],adv,[],[]).

a([ge_e(overstroomde),
   ge_no_e(overstroomd)],adv,[],[]).

a([pred(overstuur)],padv,[],[]).

a([no_e(overstuurd),
   e(overstuurde)],padv,[],[]).

a([ge_e(overtekende),
   ge_no_e(overtekend)],padv,[],[]).

a([e(overtollige),
   er(overtolliger),
   ere(overtolligere),
   no_e(overtollig),
   st(overtolligst),
   ste(overtolligste)],adv,[],[]).

a([ge_no_e(overtraind),
   ge_e(overtrainde),
   ge_no_e(overtrained),
   ge_e(overtrainede)],padv,[],[]).

a([ge_both(overtreden)],adv,[],[]).

a([ge_e(overtroefde),
   ge_no_e(overtroefd)],adv,[],[]).

a([ge_both(overtroffen)],adv,[],[]).

a([ge_both(overtrokken)],adv,[],[]).

a([ge_e(overtuigde),
   ge_no_e(overtuigd)],adv,
  [er_pp_sbar(van),
   pp(van),
   object_sbar,
   object_vp],[]).

a([ende(overtuigende),
   er(overtuigender),
   ere(overtuigendere),
   end(overtuigend),
   st(overtuigendst),
   ste(overtuigendste)],adv,[],[]).

a([ge_both(overvallen)],adv,[],[]).

a([e(oververhitte),
   no_e(oververhit)],padv,[],[]).

a([e(oververmoeide),
   no_e(oververmoeid)],padv,[],[]).

a([ge_e(oververtegenwoordigde),
   ge_no_e(oververtegenwoordigd)],padv,[],[]).

a([ge_e(overvleugelde),
   ge_no_e(overvleugeld)],adv,[],[]).

a([e(overvloedige),
   er(overvloediger),
   ere(overvloedigere),
   no_e(overvloedig),
   st(overvloedigst),
   ste(overvloedigste)],adv,[],[]).

a([ge_e(overvoerde),
   ge_no_e(overvoerd)],padv,[],[]).

a([ge_e(overweldigde),
   ge_no_e(overweldigd)],adv,[],[]).

a([e(overweldigende),
   er(overweldigender),
   ere(overweldigendere),
   no_e(overweldigend),
   st(overweldigendst),
   ste(overweldigendste)],adv,[],[]).

a([ge_e(overwerkte),
   ge_no_e(overwerkt)],padv,[],[]).

a([ge_e(overwinterde),
   ge_no_e(overwinterd)],adv,[],[]).

a([ge_e(overwoekerde),
   ge_no_e(overwoekerd)],adv,[],[]).

a([ge_both(overwogen)],adv,[],[]).

a([ge_both(overwonnen)],adv,[],[]).

a([e(overzeese),
   no_e(overzees)],nonadv,[],[]).

a([e(overzichtelijke),
   er(overzichtelijker),
   ere(overzichtelijkere),
   no_e(overzichtelijk),
   st(overzichtelijkst),
   ste(overzichtelijkste)],adv,[],[]).

a([e(paarse),
   er(paarser),
   ere(paarsere),
   no_e(paars),
   st(paarst),
   ste(paarste)],nonadv,[],[]).

a([e(pafferige),
   er(pafferiger),
   ere(pafferigere),
   no_e(pafferig),
   st(pafferigst),
   ste(pafferigste)],adv,[],[]).

a([e(palliatieve),
   no_e(palliatief)],nonadv,[],[]).

a([e(paniekerige),
   er(paniekeriger),
   ere(paniekerigere),
   no_e(paniekerig),
   st(paniekerigst),
   ste(paniekerigste)],adv,[],[]).

a([e(panische),
   er(panischer),
   ere(panischere),
   no_e(panisch),
   st(panischt),
   ste(panischte)],adv,[],[]).

a([stof(papieren)],nonadv,[],[]).

a([e(parate),
   er(parater),
   ere(paratere),
   no_e(paraat),
   st(paraatst),
   ste(paraatste)],adv,[],[]).

a([e(paradijselijke),
   er(paradijselijker),
   ere(paradijselijkere),
   no_e(paradijselijk),
   st(paradijselijkst),
   ste(paradijselijkste)],adv,[],[]).

a([e(paradoxale),
   er(paradoxaler),
   ere(paradoxalere),
   no_e(paradoxaal),
   st(paradoxaalst),
   ste(paradoxaalste)],adv,
  [subject_sbar],[]).

a([e(parallelle),
   postn_no_e(parallel)],adv,[pp(aan)],[]).

a([e(paralympische),
   no_e(paralympisch)],nonadv,[],[]).

a([e(paramilitaire),
   no_e(paramilitair)],nonadv,[],[]).

a([both(paranoïde)],padv,[],[]).

a([e(paranormale),
   no_e(paranormaal)],adv,[],[]).

a([stof(parelmoeren)],nonadv,[],[]).

a([e(parlementaire),
   no_e(parlementair)],adv,[],[]).

a([e(parmantige),
   er(parmantiger),
   ere(parmantigere),
   no_e(parmantig),
   st(parmantigst),
   ste(parmantigste)],adv,[],[]).

a([e(parmezaanse),
   no_e(parmezaans)],nonadv,[],[]).

a([both('part-time'),
   both(parttime),
   both([part,time])],adv,[],[]).

a([e(particuliere),
   e(partikuliere),
   no_e(particulier),
   no_e(partikulier)],adv,[],[]).

a([e(partiële),
   er(partiëler),
   ere(partielere),
   no_e(partieel),
   st(partieelst),
   ste(partieelste)],adv,[],[]).

a([e(partijdige),
   er(partijdiger),
   ere(partijdigere),
   no_e(partijdig),
   st(partijdigst),
   ste(partijdigste)],adv,[],[]).

a([e('partij-politieke'),
   e(partijpolitieke),
   no_e('partij-politiek'),
   no_e(partijpolitiek)],adv,[],[]).

a([both(parttime)],adv,[],[]).

a([no_e(passeerbaar),
   e(passeerbare)],adv,[],[on]).

a([both(pasgeboren)],nonadv,[],[]).

a([e(pasklare),
   no_e(pasklaar)],nonadv,[],[]).

a([pred(passé)],nonadv,[],[]).

a([ende(passende),
   er(passender),
   ere(passendere),
   end(passend),
   st(passendst),
   ste(passendste)],adv,
  [subject_sbar,
   subject_vp,
   pp(bij)],[]).

a([e(passieve),
   er(passiever),
   ere(passievere),
   no_e(passief),
   st(passiefst),
   ste(passiefste)],padv,[],[]).

a([e(pastorale),
   er(pastoraler),
   ere(pastoralere),
   no_e(pastoraal),
   st(pastoraalst),
   ste(pastoraalste)],adv,[],[]).

a([e(patente),
   no_e(patent)],nonadv,[],[]).

a([e(patetische),
   e(pathetische),
   er(patetischer),
   er(pathetischer),
   ere(patetischere),
   ere(pathetischere),
   no_e(patetisch),
   no_e(pathetisch),
   st(patetischt),
   st(pathetischt),
   ste(patetischte),
   ste(pathetischte)],adv,[],[]).

a([e(pathologische),
   e(patologische),
   no_e(pathologisch),
   no_e(patologisch)],adv,[],[]).

a([e(patriarchale),
   er(patriarchaler),
   ere(patriarchalere),
   no_e(patriarchaal),
   st(patriarchaalst),
   ste(patriarchaalste)],adv,[],[]).

a([e(pauselijke),
   no_e(pauselijk)],adv,[],[]).

a([e(pedagogische),
   er(pedagogischer),
   ere(pedagogischere),
   no_e(pedagogisch),
   st(pedagogischt),
   ste(pedagogischte)],adv,[],[]).

a([e(pedante),
   er(pedanter),
   ere(pedantere),
   no_e(pedant),
   st(pedantst),
   ste(pedantste)],nonadv,[],[]).

a([e(pedofiele),
   no_e(pedofiel)],nonadv,[],[]).

a([e(peilloze),
   no_e(peilloos)],adv,[],[]).

a([e(penetrante),
   no_e(penetrant)],adv,[],[]).

a([e(penitentiaire),
   no_e(penitentiair)],nonadv,[],[]).

a([e(peperdure),
   no_e(peperduur)],nonadv,
  [subject_vp],[]).

a([e(peperige),
   no_e(peperig)],adv,[],[]).

a([e(perfecte),
   e(perfekte),
   er(perfecter),
   er(perfekter),
   ere(perfectere),
   ere(perfektere),
   no_e(perfect),
   no_e(perfekt),
   st(perfectst),
   st(perfektst),
   ste(perfectste),
   ste(perfektste)],adv,[],[]).

a([both(perfide)],adv,[],[]).

a([e(performante),
   no_e(performant)],adv,[],[]).

a([e(perifere),
   no_e(perifeer)],nonadv,[],[ultra]).

a([both(perimeter)],nonadv,[],[]).

a([e(periodieke),
   no_e(periodiek)],adv,[],[]).

a([stof(perkamenten)],nonadv,[],[]).

a([e(permanente),
   no_e(permanent)],adv,[],[]).

a([e(perplexe),
   er(perplexer),
   ere(perplexere),
   no_e(perplex),
   st(perplext),
   ste(perplexte)],nonadv,[pp(van)],[]).

a([e(persistente),
   no_e(persistent)],padv,[],[]).

a([e(personalistische),
   no_e(personalistisch)],nonadv,[],[]).

a([e(personele),
   no_e(personeel)],nonadv,[],[]).

a([e(persoonlijke),
   er(persoonlijker),
   ere(persoonlijkere),
   no_e(persoonlijk),
   st(persoonlijkst),
   ste(persoonlijkste)],both,[],[]).

a([both(persoonsgebonden)],padv,[],[]).

a([e(pertinente),
   er(pertinenter),
   ere(pertinentere),
   no_e(pertinent),
   st(pertinentst),
   ste(pertinentste)],adv,[],[]).

a([e(perverse),
   er(perverser),
   ere(perversere),
   no_e(pervers),
   st(perverst),
   ste(perverste)],adv,[],[]).

a([e(pessimistische),
   er(pessimistischer),
   ere(pessimistischere),
   no_e(pessimistisch),
   st(pessimistischt),
   ste(pessimistischte)],adv,
  [er_pp_vp(over),
   pp(over)],[]).

a([e(pezige),
   er(peziger),
   ere(pezigere),
   no_e(pezig),
   st(pezigst),
   ste(pezigste)],nonadv,[],[]).

a([pred([pico,bello]),pred(picobello)],adv,[],[]).

a([e(pientere),
   er(pienterder),
   ere(pienterdere),
   no_e(pienter),
   st(pienterst),
   ste(pienterste)],adv,[],[]).

a([e(piepkleine),
   no_e(piepklein)],nonadv,[],[]).

a([e(pijlsnelle),
   no_e(pijlsnel)],adv,[],[]).

a([e(pijnlijke),
   er(pijnlijker),
   ere(pijnlijkere),
   no_e(pijnlijk),
   st(pijnlijkst),
   ste(pijnlijkste)],adv,
  [subject_vp,
   subject_sbar,
   pp(voor)],[]).

a([e(pijnloze),
   er(pijnlozer),
   ere(pijnlozere),
   no_e(pijnloos),
   st(pijnloost),
   ste(pijnlooste)],adv,[],[]).

a([e(pijnstillende),
   er(pijnstillender),
   ere(pijnstillendere),
   no_e(pijnstillend),
   st(pijnstillendst),
   ste(pijnstillendste)],nonadv,[],[]).

a([e(pikante),
   er(pikanter),
   ere(pikantere),
   no_e(pikant),
   st(pikantst),
   ste(pikantste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(pissige),
   no_e(pissig)],padv,
  [object_sbar,
   pp(over),
   er_pp_sbar(over)],[]).

a([e(pittige),
   er(pittiger),
   ere(pittigere),
   no_e(pittig),
   st(pittigst),
   ste(pittigste)],adv,[],[]).

a([e(pittoreske),
   er(pittoresker),
   ere(pittoreskere),
   no_e(pittoresk),
   st(pittoreskst),
   ste(pittoreskste)],adv,[],[]).

a([e(plaatselijke),
   no_e(plaatselijk)],adv,[],[]).

a([e(plaatsvervangende),
   no_e(plaatsvervangend)],padv,[],[]).

a([e(plagerige),
   er(plageriger),
   ere(plagerigere),
   no_e(plagerig),
   st(plagerigst),
   ste(plagerigste)],adv,[],[]).

a([e(plakkerige),
   er(plakkeriger),
   ere(plakkerigere),
   no_e(plakkerig),
   st(plakkerigst),
   ste(plakkerigste)],adv,[],[]).

a([e(planetaire),
   no_e(planetair)],nonadv,[],[]).

a([stof(planken)],nonadv,[],[]).

a([e(planmatige),
   er(planmatiger),
   ere(planmatigere),
   no_e(planmatig),
   st(planmatigst),
   ste(planmatigste)],adv,[],[]).

a([e(planologische),
   no_e(planologisch)],adv,[],[]).

a([e(plantaardige),
   no_e(plantaardig)],nonadv,[],[]).

a([both(plastic)],nonadv,[],[]).

a([no_e(plastiek),
   stof(plastieken)],nonadv,[],[]).

a([e(plastische),
   no_e(plastisch)],adv,[],[]).

a([stof(platina),stof(platinum),stof(platinium)],nonadv,[],[]).

a([e(platte),
   er(platter),
   ere(plattere),
   no_e(plat),
   st(platst),
   ste(platste)],adv,[],[]).

a([ge_e(platgedrukte),
   ge_no_e(platgedrukt)],adv,[],[]).

a([ge_e(platgetrapte),
   ge_no_e(platgetrapt)],adv,[],[]).

a([e(platonische),
   er(platonischer),
   ere(platonischere),
   no_e(platonisch),
   st(platonischt),
   ste(platonischte)],nonadv,[],[]).

a([e(plattelandse),
   no_e(plattelands)],nonadv,[],[]).

a([e(platvloerse),
   no_e(platvloers)],adv,[],[]).

a([e(plausibele),
   er(plausibeler),
   ere(plausibelere),
   no_e(plausibel),
   st(plausibelst),
   ste(plausibelste)],both,
  [subject_sbar],[]).

a([e(plechtige),
   er(plechtiger),
   ere(plechtigere),
   no_e(plechtig),
   st(plechtigst),
   ste(plechtigste)],adv,[],[]).

a([e(plechtstatige),
   er(plechtstatiger),
   ere(plechtstatigere),
   no_e(plechtstatig),
   st(plechtstatigst),
   ste(plechtstatigste)],adv,[],[]).

a([e(plenaire),
   no_e(plenair)],padv,[],[]).

%a([both(plenty)],odet_adv,[],[]).

a([both(plenty)],adv,[],[]).

a([e(plezante),
   no_e(plezant),
   er(plezanter),
   ere(plezantere),
   st(plezantst),
   ste(plezantste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(plezierige),
   er(plezieriger),
   ere(plezierigere),
   no_e(plezierig),
   st(plezierigst),
   ste(plezierigste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(plichtmatige),
   no_e(plichtmatig)],adv,[],[]).

a([e(plichtsgetrouwe),
   er(plichtsgetrouwer),
   ere(plichtsgetrouwere),
   no_e(plichtsgetrouw),
   st(plichtsgetrouwst),
   ste(plichtsgetrouwste)],adv,[],[]).

a([e(plompe),
   er(plomper),
   ere(plompere),
   no_e(plomp),
   st(plompst),
   ste(plompste)],adv,[],[]).

a([both(plompverloren)],adv,[],[]).

a([no_e(plots),
   e(plotse)],adv,[],[]).

a([e(plotselinge),
   no_e(plotseling)],adv,[],[]).

a([e(plotsklapse),
   no_e(plotsklaps)],adv,[],[]).

a([stof(pluche),
   stof(pluchen)],nonadv,[],[]).

a([pred(pluis)],nonadv,[],[]).

a([e(pluizige),
   er(pluiziger),
   ere(pluizigere),
   no_e(pluizig),
   st(pluizigst),
   ste(pluizigste)],nonadv,[],[]).

a([e(pluralistische),
   no_e(pluralistisch)],adv,[],[]).

a([e(pluriforme),
   no_e(pluriform)],nonadv,[],[]).

a([e(poëtische),
   er(poëtischer),
   ere(poëtischere),
   no_e(poëtisch),
   st(poëtischt),
   ste(poëtischte)],adv,[],[]).

a([e(pokdalige),
   er(pokdaliger),
   ere(pokdaligere),
   no_e(pokdalig),
   st(pokdaligst),
   ste(pokdaligste)],nonadv,[],[]).

a([e(polaire),
   no_e(polair)],adv,[],[]).

a([e(polemische),
   no_e(polemisch)],nonadv,[],[]).

a([e(poliklinische),
   no_e(poliklinisch)],adv,[],[]).

a([e(politieke),
   er(politieker),
   ere(politiekere),
   no_e(politiek),
   st(politiekst),
   ste(politiekste)],adv,[],[geo]).

a([e(politionele),
   no_e(politioneel)],adv,[],[]).

a([both(polyester)],nonadv,[],[]).

a([e(polyfone),no_e(polyfoon)],adv,[],[]).

a([e(polygame),no_e(polygaam)],nonadv,[],[]).

a([e(polyvalente),no_e(polyvalent)],nonadv,[],[]).

a([e(pompeuze),
   er(pompeuzer),
   ere(pompeuzere),
   no_e(pompeus),
   st(pompeust),
   ste(pompeuste)],adv,[],[]).

a([e(pontificale),
   no_e(pontificaal)],adv,[],[]).

a([e(populaire),
   er(populairder),
   ere(populairdere),
   no_e(populair),
   st(populairst),
   ste(populairste)],adv,
  [pp(bij)],[]).

a([e(populistische),
   no_e(populistisch)],adv,[],[]).

a([e(poreuze),
   no_e(poreus)],adv,[],[]).

a([e(pornografische),
   er(pornografischer),
   ere(pornografischere),
   no_e(pornografisch),
   st(pornografischt),
   ste(pornografischte)],adv,[],[]).

a([stof(porseleinen),
   stof(porceleinen)],nonadv,[],[]).

a([e(positieve),
   er(positiever),
   ere(positievere),
   no_e(positief),
   st(positiefst),
   ste(positiefste)],adv,
  [subject_sbar],[sero]).

a([e(positionele),
   no_e(positioneel)],adv,[],[]).

a([e(positivistische),
   er(positivistischer),
   ere(positivistischere),
   no_e(positivistisch),
   st(positivistischt),
   ste(positivistischte)],adv,[],[]).

a([e(postale),
   no_e(postaal)],nonadv,[],[]).

a([ge_no_e(postgevat),
   ge_e(postgevatte)],adv,[],[]).

a([no_e(postmodern),
   e(postmoderne)],adv,[],[]).

a([e(postume),
   no_e(postuum),
   e(posthume),
   no_e(posthuum)],padv,[],[]).

a([e(potente),
   no_e(potent)],adv,[],[]).

a([e(potentiële),
   no_e(potentieel)],adv,[],[]).

a([e(potige),
   er(potiger),
   ere(potigere),
   no_e(potig),
   st(potigst),
   ste(potigste)],nonadv,[],[]).

a([e(potsierlijke),
   er(potsierlijker),
   ere(potsierlijkere),
   no_e(potsierlijk),
   st(potsierlijkst),
   ste(potsierlijkste)],adv,
  [subject_vp,
   subject_sbar],
  []).

a([e(povere),
   er(poverder),
   ere(poverdere),
   no_e(pover),
   st(poverst),
   ste(poverste)],adv,[],[]).

a([pred(povertjes)],adv,[],[]).

a([e(praatgrage),
   no_e(praatgraag)],padv,[],[]).

a([e(prachtige),
   er(prachtiger),
   ere(prachtigere),
   no_e(prachtig),
   st(prachtigst),
   ste(prachtigste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(pragmatische),
   er(pragmatischer),
   ere(pragmatischere),
   no_e(pragmatisch),
   st(pragmatischt),
   ste(pragmatischte)],adv,[],[]).

a([e(practische),
   e(praktische),
   er(practischer),
   er(praktischer),
   ere(practischere),
   ere(praktischere),
   no_e(practisch),
   no_e(praktisch),
   st(practischt),
   st(praktischt),
   ste(practischte),
   ste(praktischte)],adv,
  [subject_vp],[]).

a([e(prangende),
   no_e(prangend)],nonadv,[],[]).

a([e(pratte),
   er(pratter),
   ere(prattere),
   no_e(prat),
   st(pratst),
   ste(pratste)],adv,[],[]).

a([e(precaire),
   er(precairder),
   ere(precairdere),
   no_e(precair),
   st(precairst),
   ste(precairste)],adv,[],[]).

a([e(precieze),
   er(preciezer),
   ere(preciezere),
   postn_no_e(precies),
   st(preciest),
   ste(precieste)],adv,
  [pp(in)],[]).

a([e(precieuze),
   er(precieuzer),
   ere(precieuzere),
   no_e(precieus),
   st(precieust),
   ste(precieuste)],nonadv,[],[]).

a([e(preferente),
   no_e(preferent)],nonadv,[],[]).

a([e(preferentiële),
   no_e(preferentieel)],nonadv,[],[]).

a([e(pregnante),
   er(pregnanter),
   ere(pregnantere),
   no_e(pregnant),
   st(pregnantst),
   ste(pregnantste)],adv,[],[]).

a([e(praehistorische),
   e(prehistorische),
   no_e(praehistorisch),
   no_e(prehistorisch)],nonadv,[],[]).

a([e(premature),
   no_e(prematuur)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([e(prenatale),
   no_e(prenataal)],adv,
  [],[]).

a([e(presente),
   e(prezente),
   er(presenter),
   er(prezenter),
   ere(presentere),
   ere(prezentere),
   no_e(present),
   no_e(prezent),
   st(presentst),
   st(prezentst),
   ste(presentste),
   ste(prezentste)],nonadv,[],[]).

a([e(presidentiële),
   no_e(presidentieel)],nonadv,[],[]).

a([e(prestigieuze),
   no_e(prestigieus)],nonadv,[],[]).

a([e(pretentieuze),
   er(pretentieuzer),
   ere(pretentieuzere),
   no_e(pretentieus),
   st(pretentieust),
   ste(pretentieuste)],adv,[],[]).

a([e(pretentieloze),
   no_e(pretentieloos)],adv,[],[]).

a([e(prettige),
   er(prettiger),
   ere(prettigere),
   no_e(prettig),
   st(prettigst),
   ste(prettigste)],adv,
  [subject_sbar,
   subject_vp,
   pp(bij),
   pp(voor)],[]).

a([e(preutse),
   er(preutser),
   ere(preutsere),
   no_e(preuts),
   st(preutst),
   ste(preutste)],adv,[],[]).

a([e(preventieve),
   no_e(preventief)],adv,[],[]).

a([e(priesterlijke),
   no_e(priesterlijk)],adv,[],[]).

a([ge_both(prijsgegeven)],adv,[],[]).

a([end(prijswinnend),
   ende(prijswinnende)],padv,[],[]).

a([e(prijzige),
   no_e(prijzig)],adv,[],[]).

a([e(prikkelbare),
   er(prikkelbaarder),
   ere(prikkelbaardere),
   no_e(prikkelbaar),
   st(prikkelbaarst),
   ste(prikkelbaarste)],padv,[],[]).

a([e(prille),
   er(priller),
   ere(prillere),
   no_e(pril),
   st(prilst),
   ste(prilste)],adv,[],[]).

a([both(prima)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(primaire),
   no_e(primair)],adv,[],[]).

a([e(primitieve),
   er(primitiever),
   ere(primitievere),
   no_e(primitief),
   st(primitiefst),
   ste(primitiefste)],adv,[],[]).

a([e(principiële),
   no_e(principieel),
   er(principiëler),
   ere(principiëler),
   st(principieelst),
   ste(principieelste)
  ],adv,[],[]).

a([e(prinselijke),
   no_e(prinselijk)],adv,[],[]).

a([e(prioritaire),
   no_e(prioritair)],adv,[],[]).

a([e(private),
   no_e(privaat)],adv,[],[h(publiek)]).

a([e(privaatrechtelijke),
   no_e(privaatrechtelijk)],adv,[],[]).

a([both(privé)],adv,[subject_sbar],[]).

a([pred(pro)],nonadv,[],[]).

a([both([pro,deo]),
   both('pro-deo'),
   both([pro,'Deo'])],adv,[],[]).

a([both([pro,forma]),
   both('pro-forma')],adv,[],[]).

a([e(probate),
   no_e(probaat)],adv,[],[]).

a([e(probleemloze),
   no_e(probleemloos)],adv,[],[]).

a([e(problematische),
   er(problematischer),
   ere(problematischere),
   no_e(problematisch),
   st(problematischt),
   ste(problematischte)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(procedurele),
   no_e(procedureel)],adv,[],[]).

a([e(procentuele),
   no_e(procentueel)],adv,[],[]).

a([e(productieve),
   e(produktieve),
   er(productiever),
   er(produktiever),
   ere(productievere),
   ere(produktievere),
   no_e(productief),
   no_e(produktief),
   st(productiefst),
   st(produktiefst),
   ste(productiefste),
   ste(produktiefste)],adv,
  [pp(voor),
   subject_sbar,
   subject_vp],[contra]).

a([e(profane),
   er(profaner),
   ere(profanere),
   no_e(profaan),
   st(profaanst),
   ste(profaanste)],adv,[],[]).

a([e(professionele),
   er(professioneler),
   ere(professionelere),
   no_e(professioneel),
   st(professioneelst),
   ste(professioneelste)],adv,[],[]).

a([e(profetische),
   er(profetischer),
   ere(profetischere),
   no_e(profetisch),
   st(profetischt),
   ste(profetischte)],adv,[],[]).

a([e(profijtelijke),
   no_e(profijtelijk)],adv,
  [pp(voor),
   subject_vp],[]).

a([e(programmatische),
   no_e(programmatisch)],adv,[],[]).

a([e(progressieve),
   er(progressiever),
   ere(progressievere),
   no_e(progressief),
   st(progressiefst),
   ste(progressiefste)],adv,[],[]).

a([e(proletarische),
   er(proletarischer),
   ere(proletarischere),
   no_e(proletarisch),
   st(proletarischt),
   ste(proletarischte)],adv,[],[]).

a([e(prominente),
   er(prominenter),
   ere(prominentere),
   no_e(prominent),
   st(prominentst),
   ste(prominentste)],adv,[],[]).

a([e(promiscue),
   no_e(promiscue)],adv,[],[]).

a([e(promotionele),
   no_e(promotioneel)],adv,[],[]).

a([e(prompte),
   no_e(prompt)],adv,[],[]).

a([e(pronte),
   no_e(pront)],adv,[],[]).

a([e(propagandistische),
   er(propagandistischer),
   ere(propagandistischere),
   no_e(propagandistisch),
   st(propagandistischt),
   ste(propagandistischte)],adv,[],[]).

a([e(propere),
   er(properder),
   ere(properdere),
   no_e(proper),
   st(properst),
   ste(properste)],nonadv,[],[]).

a([e(proportionele),
   no_e(proportioneel)],adv,[],[]).

a([e(protestante),
   no_e(protestant)],nonadv,[],[]).

a([e(protestantse),
   er(protestantser),
   ere(protestantsere),
   no_e(protestants),
   st(protestantst),
   ste(protestantste)],adv,[],[]).

a([e(protserige),
   er(protseriger),
   ere(protserigere),
   no_e(protserig),
   st(protserigst),
   ste(protserigste)],adv,[],[]).

a([e(provinciale),
   no_e(provinciaal)],adv,[],[]).

a([e(provinciaalse),
   no_e(provinciaals)],adv,[],[]).

a([no_e(provocatief),
   e(provocatieve),
   er(provocatiever),
   ere(provocatievere),
   st(provocatiefst),
   ste(provocatiefste)],adv,[],[]).

a([ende(provocerende),
   er(provocerender),
   ere(provocerendere),
   end(provocerend),
   st(provocerendst),
   ste(provocerendste)],adv,
  [transitive],[]).

a([e(prozaïsche),
   no_e(prozaïsch)],adv,[],[]).

a([e(prudente),
   no_e(prudent)],adv,[],[]).

a([e(psychiatrische),
   no_e(psychiatrisch)],adv,[],[]).

a([e(psychische),
   no_e(psychisch)],adv,[],[]).

a([e(psychoanalytische),
   no_e(psychoanalytisch)],adv,[],[]).

a([e(psychologische),
   no_e(psychologisch)],adv,[],[]).

a([e(psychopathische),
   e(psychopatische),
   no_e(psychopathisch),
   no_e(psychopatisch)],adv,[],[]).

a([e(psychosociale),
   no_e(psychosociaal)],adv,[],[]).

a([e(psychosomatische),
   no_e(psychosomatisch)],adv,[],[]).

a([e(psychoterapeutische),
   e(psychotherapeutische),
   no_e(psychoterapeutisch),
   no_e(psychotherapeutisch)],adv,[],[]).

a([e(psychotische),
   er(psychotischer),
   ere(psychotischere),
   no_e(psychotisch),
   st(psychotischt),
   ste(psychotischte)],nonadv,[],[]).

e([e(psychotrope),
   no_e(psychotroop)],nonadv,[],[]).

a([e(publicitaire),
   no_e(publicitair)],padv,[],[]).

a([e(publieke),
   no_e(publiek)],adv,[],[]).

a([e(publiekelijke),
   no_e(publiekelijk)],adv,[],[]).

a([e(publiekrechtelijke),
   no_e(publiekrechtelijk)],adv,[],[]).

a([e(puike),
   no_e(puik)],adv,[],[]).

a([e(puissante),
   no_e(puissant)],adv,[],[]).

a([e(puntige),
   er(puntiger),
   ere(puntigere),
   no_e(puntig),
   st(puntigst),
   ste(puntigste)],adv,[],[]).

a([both([pur,sang])],nonadv,[],[]).

a([e(puriteinse),
   er(puriteinser),
   ere(puriteinsere),
   no_e(puriteins),
   st(puriteinst),
   ste(puriteinste)],adv,[],[]).

a([stof(purperen)],nonadv,[],[]).

a([e(pure),
   er(puurder),
   ere(puurdere),
   no_e(puur),
   st(puurst),
   ste(puurste)],adv,[],[]).

a([prefix(kwart)],nonadv,[],[]).

a([e(kwartaire),
   e(quartaire),
   no_e(kwartair),
   no_e(quartair)],nonadv,[],[]).

a([both(kwasi),
   both(quasi)],adv,[],[]).

a([pred(quitte)],adv,[],[]).

a([e(raadselachtige),
   er(raadselachtiger),
   ere(raadselachtigere),
   no_e(raadselachtig),
   st(raadselachtigst),
   ste(raadselachtigste)],adv,[subject_sbar],[]).

a([e(raadzame),
   er(raadzamer),
   ere(raadzamere),
   no_e(raadzaam),
   st(raadzaamst),
   ste(raadzaamste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(rake),
   er(raker),
   ere(rakere),
   no_e(raak),
   st(raakst),
   ste(raakste)],adv,[],[]).

a([e(rare),
   er(raarder),
   ere(raardere),
   no_e(raar),
   st(raarst),
   ste(raarste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(rabbijnse),
   no_e(rabbijns)],nonadv,[],[]).

a([e(rabiate),
   no_e(rabiaat)],adv,[],[]).

a([e(raciale),
   no_e(raciaal)],nonadv,[],[]).

a([e(racistische),
   er(racistischer),
   ere(racistischere),
   no_e(racistisch),
   st(racistischt),
   ste(racistischte)],adv,[],[]).

a([e(radeloze),
   er(radelozer),
   ere(radelozere),
   no_e(radeloos),
   st(radeloost),
   ste(radelooste)],adv,[],[]).

a([e(radicale),
   e(radikale),
   er(radicaler),
   er(radikaler),
   ere(radicalere),
   ere(radikalere),
   no_e(radicaal),
   no_e(radikaal),
   st(radicaalst),
   st(radikaalst),
   ste(radicaalste),
   ste(radikaalste)],adv,[],
  [h(links),
   h(rechts)]).

a([e(radioactieve),
   e(radioaktieve),
   er(radioactiever),
   er(radioaktiever),
   ere(radioactievere),
   ere(radioaktievere),
   no_e(radioactief),
   no_e(radioaktief),
   st(radioactiefst),
   st(radioaktiefst),
   ste(radioactiefste),
   ste(radioaktiefste)],adv,[],[]).

a([e(radiologische),
   no_e(radiologisch)],adv,[],[]).

a([e(rafelige),
   er(rafeliger),
   ere(rafeligere),
   no_e(rafelig),
   st(rafeligst),
   ste(rafeligste)],nonadv,[],[]).

a([e(ragfijne),
   er(ragfijner),
   ere(ragfijnere),
   no_e(ragfijn),
   st(ragfijnst),
   ste(ragfijnste)],nonadv,[],[]).

a([e(rampzalige),
   er(rampzaliger),
   ere(rampzaligere),
   no_e(rampzalig),
   st(rampzaligst),
   ste(rampzaligste)],adv,[],[]).

a([e(rancuneuze),
   er(rancuneuzer),
   ere(rancuneuzere),
   no_e(rancuneus),
   st(rancuneust),
   ste(rancuneuste)],both,[],[]).

a([e(ranke),
   er(ranker),
   ere(rankere),
   no_e(rank),
   st(rankst),
   ste(rankste)],nonadv,[],[]).

a([e(ranzige),
   er(ranziger),
   ere(ranzigere),
   no_e(ranzig),
   st(ranzigst),
   ste(ranzigste)],nonadv,[],[]).

a([e(rappe),
   er(rapper),
   ere(rappere),
   no_e(rap),
   st(rapst),
   ste(rapste)],adv,[],[]).

a([e(rasse),
   er(rasser),
   ere(rassere),
   no_e(ras),
   st(rast),
   ste(raste)],adv,[],[]).

a([e(rationalistische),
   er(rationalistischer),
   ere(rationalistischere),
   no_e(rationalistisch),
   st(rationalistischt),
   ste(rationalistischte)],adv,[],[]).

a([e(rationele),
   er(rationeler),
   ere(rationelere),
   no_e(rationeel),
   st(rationeelst),
   ste(rationeelste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(rauwe),
   er(rauwer),
   ere(rauwere),
   no_e(rauw),
   st(rauwst),
   ste(rauwste)],padv,[],[]).

a([e(rauwmelkse),
   no_e(rauwmelks)],nonadv,[],[]).

a([e(razende),
   er(razender),
   ere(razendere),
   no_e(razend),
   st(razendst),
   ste(razendste)],adv,
  [er_pp_sbar(over),
   pp(op),
   pp(over)],[]).

a([e(razendsnelle),
   no_e(razendsnel)],adv,[],[]).

a([e(reactieve),
   e(reaktieve),
   no_e(reactief),
   no_e(reaktief)],nonadv,[],[]).

a([e(reactionaire),
   e(reaktionaire),
   er(reactionairder),
   er(reaktionairder),
   ere(reactionairdere),
   ere(reaktionairdere),
   no_e(reactionair),
   no_e(reaktionair),
   st(reactionairst),
   st(reaktionairst),
   ste(reactionairste),
   ste(reaktionairste)],adv,[],[]).

a([both('real-time'),
   both(realtime),
   both([real,time])],adv,[],[]).

a([e(realistische),
   er(realistischer),
   ere(realistischere),
   no_e(realistisch),
   st(realistischt),
   ste(realistischte)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(rebelse),
   er(rebelser),
   ere(rebelsere),
   no_e(rebels),
   st(rebelst),
   ste(rebelste)],adv,[],[]).

a([e(recalcitrante),
   no_e(recalcitrant)],adv,[],[]).

a([e(recente),
   no_e(recent),
   ere(recentere),
   er(recenter),
   ste(recentste),
   st(recentst)],tmpadv,[],[]).

a([e(recentelijke),
   no_e(recentelijk)],tmpadv,[],[]).

a([e(receptieve),
   er(receptiever),
   ere(receptievere),
   no_e(receptief),
   st(receptiefst),
   ste(receptiefste)],adv,[],[]).

a([e(recessieve),
   no_e(recessief)],adv,[],[]).

a([e(rechte),
   er(rechter),
   ere(rechtere),
   no_e(recht),
   st(rechtst),
   ste(rechtste)],adv,[],[]).

a([e(rechtelijke),
   no_e(rechtelijk)],adv,[],
  [s(arbeid),
   veterinair]).

a([both(rechter)],nonadv,[],[]).

a([e(rechterlijke),
   no_e(rechterlijk)],adv,[],[]).

a([e(rechtgeaarde),
   no_e(rechtgeaard)],nonadv,[],[]).

a([ge_e(rechtgestane),
   ge_no_e(rechtgestaan)],adv,[],[]).

a([ge_both(rechtgetrokken)],adv,[],[]).

a([ge_e(rechtgezette),
   ge_no_e(rechtgezet)],adv,[],[]).

a([e(rechthebbende),
   no_e(rechthebbend)],nonadv,[],[]).

a([e(rechthoekige),
   no_e(rechthoekig)],adv,[],[]).

a([er(rechtlijniger),
   ere(rechtlijnigere),
   e(rechtlijnige),
   no_e(rechtlijnig)],adv,
  [pp(over)],[]).

a([e(rechtmatige),
   er(rechtmatiger),
   ere(rechtmatigere),
   no_e(rechtmatig),
   st(rechtmatigst),
   ste(rechtmatigste)],adv,[],[]).

a([pred(rechtop)],adv,[],[]).

a([e(rechtopstaande),
   no_e(rechtopstaand)],nonadv,[],[]).

a([e(rechtse),
   postn_no_e(rechts)],dir_locadv,[],
  [ultra,h(ultra),
   extreem]).

a([both(rechtschapen),
   er(rechtschapener),
   ere(rechtschapenere),
   st(rechtschapenst),
   ste(rechtschapenste)],adv,[],[]).

a([e(rechtsgeldige),
   no_e(rechtsgeldig)],adv,[],[]).

a([e(rechtspositionele),
   no_e(rechtspositioneel)],adv,[],[]).

a([e(rechtstreekse),
   no_e(rechtstreeks),
   ere(rechtstreeksere),
   er(rechtstreekser)],adv,[],[on]).

a([both([rechttoe,rechtaan]),
   both('recht-toe-recht-aan')],adv,[],[]).

a([e(rechtvaardige),
   er(rechtvaardiger),
   ere(rechtvaardigere),
   no_e(rechtvaardig),
   st(rechtvaardigst),
   ste(rechtvaardigste)],adv,
  [subject_sbar],[]).

a([e(rechtvleugelige),
   no_e(rechtvleugelig)],nonadv,[],[]).

a([e(rechtzinnige),
   er(rechtzinniger),
   ere(rechtzinnigere),
   no_e(rechtzinnig),
   st(rechtzinnigst),
   ste(rechtzinnigste)],adv,[],[]).

a([e(recreatieve),
   e(rekreatieve),
   er(recreatiever),
   er(rekreatiever),
   ere(recreatievere),
   ere(rekreatievere),
   no_e(recreatief),
   no_e(rekreatief),
   st(recreatiefst),
   st(rekreatiefst),
   ste(recreatiefste),
   ste(rekreatiefste)],adv,[],[]).

a([e(redactionele),
   e(redaktionele),
   no_e(redactioneel),
   no_e(redaktioneel)],nonadv,[],[hoofd]).

a([e(reddeloze),
   no_e(reddeloos)],adv,[],[]).

a([e(redelijke),
   er(redelijker),
   ere(redelijkere),
   no_e(redelijk),
   st(redelijkst),
   ste(redelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(redeloze),
   er(redelozer),
   ere(redelozere),
   no_e(redeloos),
   st(redeloost),
   ste(redelooste)],adv,[],[]).

a([e(reële),
   er(reëler),
   ere(reëlere),
   no_e(reëel),
   st(reëelst),
   ste(reëelste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(reformatorische),
   no_e(reformatorisch)],nonadv,[],[]).

a([e(reformistische),
   no_e(reformistisch)],nonadv,[],[]).

a([e(regelmatige),
   er(regelmatiger),
   ere(regelmatigere),
   no_e(regelmatig),
   st(regelmatigst),
   ste(regelmatigste)],adv,[],[]).

a([e(regelrechte),
   er(regelrechter),
   ere(regelrechtere),
   no_e(regelrecht),
   st(regelrechtst),
   ste(regelrechtste)],adv,[],[]).

a([e(regenachtige),
   er(regenachtiger),
   ere(regenachtigere),
   no_e(regenachtig),
   st(regenachtigst),
   ste(regenachtigste)],adv,[],[]).  % adv: de ochtend begint regenachtig

a([e(regenteske),
   no_e(regentesk)],adv,[],[]).

a([e(regionale),
   no_e(regionaal)],adv,[],[]).

a([e(reguliere),
   no_e(regulier)],adv,[],[]).

a([e(reine),
   er(reiner),
   ere(reinere),
   no_e(rein),
   st(reinst),
   ste(reinste)],adv,[],[]).

a([e(rekbare),
   er(rekbaarder),
   ere(rekbaardere),
   no_e(rekbaar),
   st(rekbaarst),
   ste(rekbaarste)],nonadv,[],[]).

a([e(relatieve),
   er(relatiever),
   ere(relatievere),
   no_e(relatief),
   st(relatiefst),
   ste(relatiefste)],adv,[],[]).

a([e(relaxte),
   no_e(relaxt),
   er(relaxter),
   ere(relaxtere),
   no_e(relaxed),
   e(relaxe),
   no_e(relax)],padv,[],[]).

a([e(relevante),
   er(relevanter),
   ere(relevantere),
   no_e(relevant),
   st(relevantst),
   ste(relevantste)],nonadv,
  [pp(voor),
   subject_sbar,
   subject_vp],[]).

a([e(religieuze),
   er(religieuzer),
   ere(religieuzere),
   no_e(religieus),
   st(religieust),
   ste(religieuste)],adv,[],[]).

a([e(remonstrantse),
   no_e(remonstrants)],nonadv,[],[]).

a([both(remote)],nonadv,[],[]).

a([e(rendabele),
   er(rendabeler),
   ere(rendabelere),
   no_e(rendabel),
   st(rendabelst),
   ste(rendabelste)],adv,[],[]).

a([ende(rentedragende),
   end(rentedragend)],nonadv,[],[]).

a([e(representatieve),
   er(representatiever),
   ere(representatievere),
   no_e(representatief),
   st(representatiefst),
   ste(representatiefste)],nonadv,
  [pp(voor)],[]).

a([e(repressieve),
   no_e(repressief),
   er(repressiever),
   ere(repressievere),
   st(repressiefst),
   ste(repressiefste)],adv,[],[]).

a([e(reproductieve),
   no_e(reproductief)],adv,[],[]).

a([e(republikeinse),
   er(republikeinser),
   ere(republikeinsere),
   no_e(republikeins),
   st(republikeinst),
   ste(republikeinste)],adv,[],[]).

a([e(resistente),
   no_e(resistent)],padv,[],[]).

a([e(resolute),
   er(resoluter),
   ere(resolutere),
   no_e(resoluut),
   st(resoluutst),
   ste(resoluutste)],adv,[],[]).

a([e(respectabele),
   e(respektabele),
   er(respectabeler),
   er(respektabeler),
   ere(respectabelere),
   ere(respektabelere),
   no_e(respectabel),
   no_e(respektabel),
   st(respectabelst),
   st(respektabelst),
   ste(respectabelste),
   ste(respektabelste)],adv,[],[]).

a([e(respectloze),
   no_e(respectloos)],adv,[],[]).

a([e(respectvolle),
   no_e(respectvol)],adv,[],[]).

a([e(respectieve),
   e(respektieve),
   no_e(respectief),
   no_e(respektief)],nonadv,[],[]).

a([e(respectievelijke),
   e(respektievelijke),
   no_e(respectievelijk),
   no_e(respektievelijk)],adv,[],[]).

a([e(restitueerbare),
   no_e(restitueerbaar)],nonadv,[],[]).

a([e(retorische),
   no_e(retorisch)],adv,[],[]).

a([e(retrograde),
   no_e(retrograad)],nonadv,[],[]).

a([e(retrospectieve),
   no_e(retrospectief)],nonadv,[],[]).

a([e(reumatische),
   no_e(reumatisch)],nonadv,[],[]).

a([e(reusachtige),
   er(reusachtiger),
   ere(reusachtigere),
   no_e(reusachtig),
   st(reusachtigst),
   ste(reusachtigste)],adv,[],[]).

a([both(reuze)],adv,[],[]).

a([e(revolutionaire),
   er(revolutionairder),
   ere(revolutionairdere),
   no_e(revolutionair),
   st(revolutionairst),
   ste(revolutionairste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(riante),
   er(rianter),
   ere(riantere),
   no_e(riant),
   st(riantst),
   ste(riantste)],adv,[],[]).

a([e(richtinggevende),
   no_e(richtinggevend)],nonadv,[],[]).

a([e(ridicule),
   e(ridikule),
   er(ridiculer),
   er(ridikuler),
   ere(ridiculere),
   ere(ridikulere),
   no_e(ridicuul),
   no_e(ridikuul),
   st(ridicuulst),
   st(ridikuulst),
   ste(ridicuulste),
   ste(ridikuulste)],adv,[],[]).

a([stof(rieten)],nonadv,[],[]).

a([both(rigide),
   er(rigider),
   ere(rigidere),
   st(rigiedst),
   ste(rigiedste)],adv,[],[]).

a([e(rigoureuze),
   er(rigoureuzer),
   ere(rigoureuzere),
   no_e(rigoureus),
   st(rigoureust),
   ste(rigoureuste)],adv,[],[]).

a([e(rijke),
   er(rijker),
   ere(rijkere),
   no_e(rijk),
   postn_no_e(rijk),
   st(rijkst),
   ste(rijkste)],adv,
  [pp(aan)],
  [olie]).

a([e(rijke),
   er(rijker),
   ere(rijkere),
   no_e(rijk),
   st(rijkst),
   ste(rijkste)],adv,
  [],
  [lommer,
   steen,
   succes]).

a([e(rijkelijke),
   er(rijkelijker),
   ere(rijkelijkere),
   no_e(rijkelijk),
   st(rijkelijkst),
   ste(rijkelijkste)],adv,[],[]).

a([e(rijklare),no_e(rijklaar)],padv,[],[]).

a([e(rijpe),
   er(rijper),
   ere(rijpere),
   no_e(rijp),
   st(rijpst),
   ste(rijpste)],nonadv,
  [object_vp,
   er_pp_vp(voor),
   pp(voor)],
  [vroeg]).

a([e(rijzige),
   er(rijziger),
   ere(rijzigere),
   no_e(rijzig),
   st(rijzigst),
   ste(rijzigste)],adv,[],[]).

a([e(rimpelige),
   er(rimpeliger),
   ere(rimpeligere),
   no_e(rimpelig),
   st(rimpeligst),
   ste(rimpeligste)],nonadv,[],[]).

a([e(rimpelloze),
   no_e(rimpelloos)],adv,[],[]).

a([e(rinse),
   er(rinser),
   ere(rinsere),
   no_e(rins),
   st(rinst),
   ste(rinste)],nonadv,[],[]).

a([e(risicovolle),
   no_e(risicovol)],adv,[],[]).

a([e(riskante),
   e(risquante),
   er(riskanter),
   er(risquanter),
   ere(riskantere),
   ere(risquantere),
   no_e(riskant),
   no_e(risquant),
   st(riskantst),
   st(risquantst),
   ste(riskantste),
   ste(risquantste)],adv,
  [subject_vp,
   pp(voor)],[]).

a([e(ritmische),
   er(ritmischer),
   ere(ritmischere),
   no_e(ritmisch),
   st(ritmischt),
   ste(ritmischte)],adv,[],[]).

a([e(rituele),
   no_e(ritueel)],adv,[],[]).

a([ende(rivaliserende),
   end(rivaliserend)],nonadv,[],[]).

a([e(robuuste),
   er(robuuster),
   ere(robuustere),
   no_e(robuust)],adv,[],[]).

a([both(rocky)],adv,[],[]).

a([e(roekeloze),
   er(roekelozer),
   ere(roekelozere),
   no_e(roekeloos),
   st(roekeloost),
   ste(roekelooste)],adv,[],[]).

a([e(roemloze),
   no_e(roemloos)],adv,[],[]).

a([e(roemrijke),
   er(roemrijker),
   ere(roemrijkere),
   no_e(roemrijk),
   st(roemrijkst),
   ste(roemrijkste)],adv,[],[]).

a([e(roemruchte),
   er(roemruchter),
   ere(roemruchtere),
   no_e(roemrucht),
   st(roemruchtst),
   ste(roemruchtste)],nonadv,[],[]).

a([ende(roerende),
   end(roerend)],both,[],[]).

a([e(roerige),
   er(roeriger),
   ere(roerigere),
   no_e(roerig),
   st(roerigst),
   ste(roerigste)],nonadv,[],[]).

a([er(roerlozer),
   ere(roerlozere),
   e(roerloze),
   no_e(roerloos)],padv,[],[]).

a([e(roestbruine),
   er(roestbruiner),
   ere(roestbruinere),
   no_e(roestbruin),
   st(roestbruinst),
   ste(roestbruinste)],nonadv,[],[]).

a([e(roestige),
   er(roestiger),
   ere(roestigere),
   no_e(roestig),
   st(roestigst),
   ste(roestigste)],nonadv,[],[]).

a([stof(roestvrijstalen),
   stof([roestvrij,stalen]),
   stof(roestvrijstaal)],nonadv,[],[]).



a([e(rokerige),
   er(rokeriger),
   ere(rokerigere),
   no_e(rokerig),
   st(rokerigst),
   ste(rokerigste)],nonadv,[],[]).

a([e(romantische),
   er(romantischer),
   ere(romantischere),
   no_e(romantisch),
   st(romantischt),
   ste(romantischte)],adv,[],[]).

a([e(romige),
   er(romiger),
   ere(romigere),
   no_e(romig),
   st(romigst),
   ste(romigste)],nonadv,[],[]).

a([e(rommelige),
   er(rommeliger),
   ere(rommeligere),
   no_e(rommelig),
   st(rommeligst),
   ste(rommeligste)],adv,[],[]).

a([e(ronde),
   er(ronder),
   ere(rondere),
   no_e(rond),
   st(rondst),
   ste(rondste)],adv,[],[]).

a([ge_e(rondgedraaide),
   ge_no_e(rondgedraaid)],adv,[],[]).

a([ge_e(rondgegane),
   ge_no_e(rondgegaan)],adv,[],[]).

a([ge_both(rondgekomen)],adv,[],[]).

a([ge_e(rondgeleide),
   ge_no_e(rondgeleid)],adv,[],[]).

a([ge_e(rondgereisde),
   ge_no_e(rondgereisd)],adv,[],[]).

a([ge_e(rondgeslingerde),
   ge_no_e(rondgeslingerd)],adv,[],[]).

a([ge_e(rondgewandelde),
   ge_no_e(rondgewandeld)],adv,[],[]).

a([ende(rondrazende),
   end(rondrazend)],padv,[],[]).

a([ende(rondzwevende),
   end(rondzwevend)],padv,[],[]).

a([e(rode),
   e(rooie),
   er(roder),
   ere(rodere),
   no_e(rood),
   st(roodst),
   ste(roodste)],adv,[],[]).

a([e('rood-groene'),
   no_e('rood-groen')],nonadv,[],[]).

a([e(roodachtige),
   er(roodachtiger),
   ere(roodachtigere),
   no_e(roodachtig),
   st(roodachtigst),
   ste(roodachtigste)],nonadv,[],[]).

a([e(roodbruine),
   er(roodbruiner),
   ere(roodbruinere),
   no_e(roodbruin),
   st(roodbruinst),
   ste(roodbruinste)],padv,[],[]).

a([e(roodgloeiende),
   er(roodgloeiender),
   ere(roodgloeiendere),
   no_e(roodgloeiend),
   st(roodgloeiendst),
   ste(roodgloeiendste)],nonadv,[],[]).

a([e(roodharige),
   er(roodhariger),
   ere(roodharigere),
   no_e(roodharig),
   st(roodharigst),
   ste(roodharigste)],nonadv,[],[]).

a([e(roomse),
   no_e(rooms),
   er(roomser),
   ere(roomsere)],nonadv,[],[]).

a([e('rooms-katholieke'),
   e('rooms-katolieke'),
   e([rooms,katolieke]),
   e([rooms,katholieke]),
   no_e('rooms-katholiek'),
   no_e('rooms-katoliek'),
   no_e([rooms,katoliek]),
   no_e([rooms,katholiek]),
   e('Rooms-katholieke'),
   e('Rooms-katolieke'),
   e(['Rooms',katolieke]),
   e(['Rooms',katholieke]),
   no_e('Rooms-katholiek'),
   no_e('Rooms-katoliek'),
   no_e(['Rooms',katoliek]),
   no_e(['Rooms',katholiek])],nonadv,[],[]).

a([e(rooskleurige),
   er(rooskleuriger),
   ere(rooskleurigere),
   no_e(rooskleurig),
   st(rooskleurigst),
   ste(rooskleurigste)],adv,[],[]).

a([e(roostervrije),
   no_e(roostervrij)],padv,[],[]).

a([e(rosse),
   er(rosser),
   ere(rossere),
   no_e(ros),
   st(rost),
   ste(roste)],nonadv,[],[]).

a([e(rossige),
   er(rossiger),
   ere(rossigere),
   no_e(rossig),
   st(rossigst),
   ste(rossigste)],nonadv,[],[]).

a([e(rotte),
   er(rotter),
   ere(rottere),
   no_e(rot),
   st(rotst),
   ste(rotste)],nonadv,[subject_sbar],[]).

a([e(rotsachtige),
   er(rotsachtiger),
   ere(rotsachtigere),
   no_e(rotsachtig),
   st(rotsachtigst),
   ste(rotsachtigste)],nonadv,[],[]).

a([e(rotsvaste),
   er(rotsvaster),
   ere(rotsvastere),
   no_e(rotsvast)],adv,[],[]).

a([e(rottige),
   er(rottiger),
   ere(rottigere),
   no_e(rottig),
   st(rottigst),
   ste(rottigste)],adv,[],[]).

a([pred(rouwig)],padv,
  [pp(om),
   pp(over),
   er_pp_vp(om),
   er_pp_sbar(om),
   er_pp_vp(over),
   er_pp_sbar(over),
   object_sbar],[]).

a([e(royale),
   er(royaler),
   ere(royalere),
   no_e(royaal),
   st(royaalst),
   ste(royaalste)],adv,[],[]).

a([both(rose),
   both(roze)],nonadv,[],[i(zuur_stok,zuurstok)]).

a([e(rozige),
   er(roziger),
   ere(rozigere),
   no_e(rozig),
   st(rozigst),
   ste(rozigste)],nonadv,[],[]).

a([stof(rubberen),
   prefix(rubber)],nonadv,[],[]).

a([e(rudimentaire),
   no_e(rudimentair)],nonadv,[],[]).

a([e(ruggelingse),
   no_e(ruggelings)],adv,[],[]).

a([e(ruige),
   er(ruiger),
   ere(ruigere),
   no_e(ruig),
   st(ruigst),
   ste(ruigste)],adv,[],[]).

a([e(ruime),
   er(ruimer),
   ere(ruimere),
   no_e(ruim),
   st(ruimst),
   ste(ruimste)],adv,[],[]).

a([e(ruimhartige),
   no_e(ruimhartig)],adv,[],[]).

a([e(ruimschootse),
   no_e(ruimschoots)],adv,[],[]).

a([e(ruimtelijke),
   er(ruimtelijker),
   ere(ruimtelijkere),
   no_e(ruimtelijk),
   st(ruimtelijkst),
   ste(ruimtelijkste)],adv,[],[]).

a([e(ruiterlijke),
   no_e(ruiterlijk)],adv,[],[]).

a([e(rulle),
   er(ruller),
   ere(rullere),
   no_e(rul),
   st(rulst),
   ste(rulste)],nonadv,[],[]).

a([e(rumoerige),
   er(rumoeriger),
   ere(rumoerigere),
   no_e(rumoerig),
   st(rumoerigst),
   ste(rumoerigste)],adv,[],[]).

a([e(rurale),
   no_e(ruraal)],nonadv,[],[]).

a([e(rusteloze),
   er(rustelozer),
   ere(rustelozere),
   no_e(rusteloos),
   st(rusteloost),
   ste(rustelooste)],padv,[],[]).

a([e(rustgevende),
   er(rustgevender),
   ere(rustgevendere),
   no_e(rustgevend),
   st(rustgevendst),
   ste(rustgevendste)],nonadv,[],[]).

a([e(rustieke),
   er(rustieker),
   ere(rustiekere),
   no_e(rustiek),
   st(rustiekst),
   ste(rustiekste)],nonadv,[],[]).

a([e(rustige),
   er(rustiger),
   ere(rustigere),
   no_e(rustig),
   st(rustigst),
   ste(rustigste)],adv,[],[]).

a([e(ruwe),
   er(ruwer),
   ere(ruwere),
   no_e(ruw),
   st(ruwst),
   ste(ruwste)],adv,[],[]).

a([e(rücksichtlose),
   no_e(rücksichtlos),
   e(rücksichtslose),
   no_e(rücksichtslos)],adv,[],[]).

a([both(rvs)],nonadv,[],[]).

a([e(saaie),
   er(saaier),
   ere(saaiere),
   no_e(saai),
   st(saaist),
   ste(saaiste)],adv,
  [subject_vp],[]).

a([e(sacrale),
   e(sakrale),
   no_e(sacraal),
   no_e(sakraal)],nonadv,[],[]).

a([pred([sadder,and,wiser])],padv,[],[]).

a([e(sadistische),
   er(sadistischer),
   ere(sadistischere),
   no_e(sadistisch),
   st(sadistischt),
   ste(sadistischte)],adv,[],[]).

a([no_e(saillant),
   e(saillante)],nonadv,
  [subject_sbar],[]).

a([postn_pred(samen)],padv,[pp(met)],[]).

a([e(samenbindende),
   no_e(samenbindend)],adv,[],[]).

a([ge_e(samengebalde),
   ge_no_e(samengebald)],adv,[],[]).

a([ge_both(samengebonden)],adv,[],[]).

a([ge_e(samengebrachte),
   ge_no_e(samengebracht)],adv,[],[]).

a([ge_e(samengegane),
   ge_no_e(samengegaan)],adv,[],[]).

a([ge_both(samengeknepen)],adv,[],[]).

a([ge_both(samengekomen)],adv,[],[]).

a([ge_e(samengepakte),
   ge_no_e(samengepakt)],nonadv,[],[]).

a([ge_e(samengeperste),
   ge_no_e(samengeperst)],adv,[],[]).

a([ge_both(samengesmolten)],adv,[],[]).

a([ge_both(samengespannen)],adv,[],[]).

a([ge_e(samengestelde),
   ge_no_e(samengesteld)],adv,
  [pp(door),
   pp(met),
   pp(uit)],[]).

a([ge_both(samengetrokken)],adv,[],[]).

a([ge_both(samengevallen)],adv,[],[]).

a([ge_e(samengevatte),
   ge_no_e(samengevat)],adv,[],[]).

a([ge_e(samengevloeide),
   ge_no_e(samengevloeid)],adv,[],[]).

a([ge_e(samengevoegde),
   ge_no_e(samengevoegd)],adv,[],[]).

a([e(sanitaire),no_e(sanitair)],nonadv,[],[]).

a([pred(sappel)],nonadv,[],[]).

a([e(sappige),
   er(sappiger),
   ere(sappigere),
   no_e(sappig),
   st(sappigst),
   ste(sappigste)],adv,[],[]).

a([e(sarcastische),
   e(sarkastische),
   er(sarcastischer),
   er(sarkastischer),
   ere(sarcastischere),
   ere(sarkastischere),
   no_e(sarcastisch),
   no_e(sarkastisch),
   st(sarcastischt),
   st(sarkastischt),
   ste(sarcastischte),
   ste(sarkastischte)],adv,[],[]).

a([e(satanische),
   no_e(satanisch)],adv,[],[]).

a([stof(satijnen)],nonadv,[],[]).

a([e(satirische),
   er(satirischer),
   ere(satirischere),
   no_e(satirisch),
   st(satirischt),
   ste(satirischte)],adv,[],[]).

a([e(sceptische),
   e(skeptische),
   er(sceptischer),
   er(skeptischer),
   ere(sceptischere),
   ere(skeptischere),
   no_e(sceptisch),
   no_e(skeptisch),
   st(sceptischt),
   st(skeptischt),
   ste(sceptischte),
   ste(skeptischte)],adv,
  [er_pp_vp(over),
   pp(over)],[]).

a([e(schaamteloze),
   er(schaamtelozer),
   ere(schaamtelozere),
   no_e(schaamteloos),
   st(schaamteloost),
   ste(schaamtelooste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(schaapachtige),
   er(schaapachtiger),
   ere(schaapachtigere),
   no_e(schaapachtig),
   st(schaapachtigst),
   ste(schaapachtigste)],adv,[],[]).

a([e(schaarse),
   er(schaarser),
   ere(schaarsere),
   no_e(schaars),
   st(schaarst),
   ste(schaarste)],adv,[],[]).

a([e(schadelijke),
   er(schadelijker),
   ere(schadelijkere),
   no_e(schadelijk),
   st(schadelijkst),
   ste(schadelijkste)],adv,
  [pp(voor)],[]).

a([pred(schadeloos)],nonadv,[],[]).

a([e(schaduwrijke),
   er(schaduwrijker),
   ere(schaduwrijkere),
   no_e(schaduwrijk),
   st(schaduwrijkst),
   ste(schaduwrijkste)],nonadv,[],[]).

a([e(schalkse),
   er(schalkser),
   ere(schalksere),
   no_e(schalks),
   st(schalkst),
   ste(schalkste)],adv,[],[]).

a([e(schamele),
   er(schameler),
   ere(schamelere),
   no_e(schamel),
   st(schamelst),
   ste(schamelste)],adv,[],[]).

a([e(schampere),
   er(schamperder),
   ere(schamperdere),
   no_e(schamper),
   st(schamperst),
   ste(schamperste)],adv,[],[]).

a([e(schandalige),
   er(schandaliger),
   ere(schandaligere),
   no_e(schandalig),
   st(schandaligst),
   ste(schandaligste)],adv,
  [subject_sbar],[]).

a([e(schandelijke),
   er(schandelijker),
   ere(schandelijkere),
   no_e(schandelijk),
   st(schandelijkst),
   ste(schandelijkste)],adv,
  [subject_sbar],[]).

a([stof(scharlaken)],nonadv,[],[]).

a([e(schatrijke),
   er(schatrijker),
   ere(schatrijkere),
   no_e(schatrijk),
   st(schatrijkst),
   ste(schatrijkste)],nonadv,[],[]).

a([e(schattige),
   er(schattiger),
   ere(schattigere),
   no_e(schattig),
   st(schattigst),
   ste(schattigste)],adv,[],[]).

a([e(scheve),
   er(schever),
   ere(schevere),
   no_e(scheef),
   st(scheefst),
   ste(scheefste)],adv,[],[]).

a([e(schele),
   er(scheler),
   ere(schelere),
   no_e(scheel),
   st(scheelst),
   ste(scheelste)],adv,[],[]).

a([e(scheikundige),
   no_e(scheikundig)],adv,[],[]).

a([e(schelle),
   er(scheller),
   ere(schellere),
   no_e(schel),
   st(schelst),
   ste(schelste)],adv,[],[]).

a([e(schematische),
   er(schematischer),
   ere(schematischere),
   no_e(schematisch),
   st(schematischt),
   ste(schematischte)],adv,[],[]).

a([e(schemerige),
   er(schemeriger),
   ere(schemerigere),
   no_e(schemerig),
   st(schemerigst),
   ste(schemerigste)],adv,[],[]).

a([e(scherpe),
   er(scherper),
   ere(scherpere),
   no_e(scherp),
   st(scherpst),
   ste(scherpste)],adv,[],[]).

a([e(scherpzinnige),
   er(scherpzinniger),
   ere(scherpzinnigere),
   no_e(scherpzinnig),
   st(scherpzinnigst),
   ste(scherpzinnigste)],adv,[],[]).

a([ende(schertsende),
   er(schertsender),
   ere(schertsendere),
   end(schertsend),
   st(schertsendst),
   ste(schertsendste)],adv,[],[]).

a([e(schichtige),
   er(schichtiger),
   ere(schichtigere),
   no_e(schichtig),
   st(schichtigst),
   ste(schichtigste)],adv,[],[]).

a([e(schielijke),
   er(schielijker),
   ere(schielijkere),
   no_e(schielijk),
   st(schielijkst),
   ste(schielijkste)],adv,[],[]).

a([e(schietgrage),
   no_e(schietgraag)],padv,[],[]).

a([e(schijnbare),
   no_e(schijnbaar)],adv,[],[]).

a([e(schijnheilige),
   er(schijnheiliger),
   ere(schijnheiligere),
   no_e(schijnheilig),
   st(schijnheiligst),
   ste(schijnheiligste)],adv,[],[]).

a([e(schilderachtige),
   er(schilderachtiger),
   ere(schilderachtigere),
   no_e(schilderachtig),
   st(schilderachtigst),
   ste(schilderachtigste)],adv,[],[]).

a([e(schimmige),
   er(schimmiger),
   ere(schimmigere),
   no_e(schimmig),
   st(schimmigst),
   ste(schimmigste)],adv,[],[]).  % ze handelden schimmig

a([e(schitterende),
   er(schitterender),
   ere(schitterendere),
   no_e(schitterend),
   st(schitterendst),
   ste(schitterendste)],adv,[subject_sbar,
                             subject_vp],[]).

a([e(schizofrene),
   no_e(schizofreen)],adv,[],[]).

a([both(schizoïde)],nonadv,[],[]).

a([e(schlemielige),
   no_e(schlemielig)],adv,[],[]).

a([e(schokkende),
   er(schokkender),
   ere(schokkendere),
   no_e(schokkend),
   st(schokkendst),
   ste(schokkendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([end(schoolgaand),
   ende(schoolgaande)],nonadv,[],[]).

a([e(schoolse),
   er(schoolser),
   ere(schoolsere),
   no_e(schools),
   st(schoolst),
   ste(schoolste)],adv,[],
  [na,
   voor]).

a([e(schone),
   er(schoner),
   ere(schonere),
   no_e(schoon),
   st(schoonst),
   ste(schoonste)],adv,[],[]).

a([ge_both(schoongehouden)],adv,[],[]).

a([ge_e(schoongemaakte),
   ge_no_e(schoongemaakt)],adv,[],[]).

a([ge_e(schoongeveegde),
   ge_no_e(schoongeveegd)],adv,[],[]).

a([e(schoorvoetende),
   er(schoorvoetender),
   ere(schoorvoetendere),
   no_e(schoorvoetend),
   st(schoorvoetendst),
   ste(schoorvoetendste)],adv,[],[]).

a([e(schorre),
   er(schorder),
   ere(schordere),
   no_e(schor),
   st(schorst),
   ste(schorste)],adv,[],[]).

a([ende(schouderophalende),
   end(schouderophalend)],padv,[],[]).

a([e(schrale),
   er(schraler),
   ere(schralere),
   no_e(schraal),
   st(schraalst),
   ste(schraalste)],adv,[],[]).

a([e(schrandere),
   er(schranderder),
   ere(schranderdere),
   no_e(schrander),
   st(schranderst),
   ste(schranderste)],adv,[],[]).

a([e(schreefloze),
   no_e(schreefloos)],adv,[],[]).

a([e(schrepele),no_e(schrepel)],adv,[],[]).

a([e(schriele),
   er(schrieler),
   ere(schrielere),
   no_e(schriel),
   st(schrielst),
   ste(schrielste)],adv,[],[]).

a([e(schriftelijke),
   no_e(schriftelijk)],adv,[],[]).

a([both(schrijlings)],adv,[],[]).

a([e(schrijnende),
   ere(schrijnendere),
   ste(schrijnendste),
   er(schrijnender),
   st(schrijnendst),
   no_e(schrijnend)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(schrikbarende),
   er(schrikbarender),
   ere(schrikbarendere),
   no_e(schrikbarend),
   st(schrikbarendst),
   ste(schrikbarendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(schrikwekkende),
   er(schrikwekkender),
   ere(schrikwekkendere),
   no_e(schrikwekkend),
   st(schrikwekkendst),
   ste(schrikwekkendste)],adv,[],[]).

a([e(schrille),
   er(schriller),
   ere(schrillere),
   no_e(schril),
   st(schrilst),
   ste(schrilste)],adv,[],[]).

a([e(schromelijke),
   er(schromelijker),
   ere(schromelijkere),
   no_e(schromelijk),
   st(schromelijkst),
   ste(schromelijkste)],adv,[],[]).

a([e(schuchtere),
   er(schuchterder),
   ere(schuchterdere),
   no_e(schuchter),
   st(schuchterst),
   ste(schuchterste)],adv,[],[]).

a([ge_e(schuilgegane),
   ge_no_e(schuilgegaan)],adv,[],[]).

a([ge_both(schuilgehouden)],adv,[],[]).

a([ende(schuimbekkende),
   end(schuimbekkend)],padv,[],[]).

a([ende(schuimende),
   er(schuimender),
   ere(schuimendere),
   end(schuimend),
   st(schuimendst),
   ste(schuimendste)],adv,[],[]).

a([e(schuine),
   er(schuiner),
   ere(schuinere),
   no_e(schuin),
   st(schuinst),
   ste(schuinste)],adv,[],[]).

a([e(schuldbewuste),
   er(schuldbewuster),
   ere(schuldbewustere),
   no_e(schuldbewust)],adv,[],[]).

a([e(schuldige),
   er(schuldiger),
   ere(schuldigere),
   no_e(schuldig),
   st(schuldigst),
   ste(schuldigste)],padv,
  [er_pp_vp(aan),
   pp(aan),
   np_np,
   transitive,
   object_sbar,
   object_vp],[]).

a([e(schunnige),
   er(schunniger),
   ere(schunnigere),
   no_e(schunnig),
   st(schunnigst),
   ste(schunnigste)],adv,[],[]).

a([e(schurftige),
   er(schurftiger),
   ere(schurftigere),
   no_e(schurftig),
   st(schurftigst),
   ste(schurftigste)],nonadv,[],[]).

a([e(schuwe),
   er(schuwer),
   ere(schuwere),
   no_e(schuw),
   st(schuwst),
   ste(schuwste)],adv,[],
  [kop]).

a([both([science,fiction])],nonadv,[],[]).

a([e(secce),
   no_e(sec)],nonadv,[],[]).

a([e(sectorale),
   e(sektorale),
   no_e(sectoraal),
   no_e(sektoraal)],adv,[],[]).

a([e(seculiere),
   no_e(seculier)],adv,[],[]).

a([e(secundaire),
   no_e(secundair)],adv,[],[]).

a([e(secure),
   e(sekure),
   er(secuurder),
   er(sekuurder),
   ere(secuurdere),
   ere(sekuurdere),
   no_e(secuur),
   no_e(sekuur),
   st(secuurst),
   st(sekuurst),
   ste(secuurste),
   ste(sekuurste)],adv,[],[]).

a([e(segmentale),
   no_e(segmentaal)],nonadv,[],[]).

a([e(seksuele),
   e(sexuele),
   no_e(seksueel),
   no_e(sexueel)],adv,[],[]).

a([e(sectarische),
   e(sektarische),
   er(sectarischer),
   er(sektarischer),
   ere(sectarischere),
   ere(sektarischere),
   no_e(sectarisch),
   no_e(sektarisch),
   st(sectarischt),
   st(sektarischt),
   ste(sectarischte),
   ste(sektarischte)],adv,[],[]).

a([e(selecte),
   e(selekte),
   no_e(select),
   no_e(selekt)],nonadv,[],[]).

a([e(selectieve),
   e(selektieve),
   er(selectiever),
   er(selektiever),
   ere(selectievere),
   ere(selektievere),
   no_e(selectief),
   no_e(selektief),
   st(selectiefst),
   st(selektiefst),
   ste(selectiefste),
   ste(selektiefste)],adv,[],[]).

a([e(semantische),
   no_e(semantisch)],adv,[],[]).

a([both(senang)],nonadv,[],[]).

a([e(seniele),
   er(senieler),
   ere(senielere),
   no_e(seniel),
   st(senielst),
   ste(senielste)],nonadv,[],[]).

a([both(senior),both(senioren)],nonadv,[],[]).

a([e(sensationele),
   er(sensationeler),
   ere(sensationelere),
   no_e(sensationeel),
   st(sensationeelst),
   ste(sensationeelste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(sensuele),
   er(sensueler),
   ere(sensuelere),
   no_e(sensueel),
   st(sensueelst),
   ste(sensueelste)],adv,[],[]).

a([e(sentimentele),
   er(sentimenteler),
   ere(sentimentelere),
   no_e(sentimenteel),
   st(sentimenteelst),
   ste(sentimenteelste)],adv,[],[]).

a([e(separate),
   no_e(separaat)],adv,[],[]).

a([e(separatistische),
   no_e(separatistisch)],adv,[],[]).

a([e(serene),
   er(serener),
   ere(serenere),
   no_e(sereen),
   st(sereenst),
   ste(sereenste)],padv,[],[]).

a([e(serieuze),
   er(serieuzer),
   ere(serieuzere),
   no_e(serieus),
   st(serieust),
   ste(serieuste)],adv,[],[]).

a([both(sexy),
   er(sexyer),er(sexier),
   ere(sexyere),ere(sexiere),
   st(sexyst),st(sexiest),
   ste(sexyste),ste(sexieste)],adv,[],[]).

a([e(sfeervolle),
   er(sfeervoller),
   ere(sfeervollere),
   no_e(sfeervol),
   st(sfeervolst),
   ste(sfeervolste)],adv,[],[]).

a([e(sierlijke),
   er(sierlijker),
   ere(sierlijkere),
   no_e(sierlijk),
   st(sierlijkst),
   ste(sierlijkste)],adv,[],[]).

a([e(significante),
   e(signifikante),
   er(significanter),
   er(signifikanter),
   ere(significantere),
   ere(signifikantere),
   no_e(significant),
   no_e(signifikant),
   st(significantst),
   st(signifikantst),
   ste(significantste),
   ste(signifikantste)],adv,[],[]).

a([stof(silicium)],nonadv,[],[]).

a([e(simpele),
   er(simpeler),
   ere(simpelere),
   no_e(simpel),
   st(simpelst),
   ste(simpelste)],adv,[],[]).

a([e(simplistische),
   no_e(simplistisch)],adv,[],[]).

a([e(simultane),
   no_e(simultaan)],adv,[],[]).

a([both(single)],nonadv,[],[]).

a([e(sinistere),
   er(sinisterder),
   ere(sinisterdere),
   no_e(sinister),
   st(sinisterst),
   ste(sinisterste)],adv,[],[]).

a([e(sippe),
   no_e(sip),
   er(sipper),
   ere(sippere),
   st(sipst),
   ste(sipste)],adv,[],[]).

a([e(sjieke),
   no_e(sjiek)],adv,[],[]).

a([e('sji\'itische'),
   no_e('sji\'itisch'),
   e('sji\'ietische'),
   no_e('sji\'ietisch'),
   e(sjiitische),
   no_e(sjiitisch),
   e('shi\'itische'),
   no_e('shi\'itisch'),
   e(shiitische),
   no_e(shiitisch)],adv,[],[]).

a([e(sjofele),
   er(sjofeler),
   ere(sjofelere),
   no_e(sjofel),
   st(sjofelst),
   ste(sjofelste)],adv,[],[]).

a([e(slaafse),
   er(slaafser),
   ere(slaafsere),
   no_e(slaafs),
   st(slaafst),
   ste(slaafste)],adv,[],[]).

a([pred(slaags)],nonadv,
  [pp(met)],[]).

a([both(slaapdronken),
   er(slaapdronkener),
   ere(slaapdronkenere),
   st(slaapdronkenst),
   ste(slaapdronkenste)],padv,[],[]).

a([e(slaapverwekkende),
   er(slaapverwekkender),
   ere(slaapverwekkendere),
   no_e(slaapverwekkend),
   st(slaapverwekkendst),
   ste(slaapverwekkendste)],nonadv,[],[]).

a([e(slagvaardige),
   er(slagvaardiger),
   ere(slagvaardigere),
   no_e(slagvaardig),
   st(slagvaardigst),
   ste(slagvaardigste)],adv,[],[]).

a([e(slanke),
   er(slanker),
   ere(slankere),
   no_e(slank),
   st(slankst),
   ste(slankste)],adv,[],[vol]).  % adv: slank gebouwd

a([e(slappe),
   er(slapper),
   ere(slappere),
   no_e(slap),
   st(slapst),
   ste(slapste)],adv,[],[]).

a([e(slapeloze),
   er(slapelozer),
   ere(slapelozere),
   no_e(slapeloos),
   st(slapeloost),
   ste(slapelooste)],adv,[],[]).

a([e(slaperige),
   er(slaperiger),
   ere(slaperigere),
   no_e(slaperig),
   st(slaperigst),
   ste(slaperigste)],adv,[],[]).

a([pred(slapjes)],adv,[],[]).

a([both(sleazy)],nonadv,[],[]).

a([e(slechte),
   no_e(slecht),
   er(slechter),
   ere(slechtere),
   st(slechtst),
   ste(slechtste)],adv,
  [subject_vp,
   subject_sbar,
   er_pp_vp(in),
   pp(in),
   pp(met),
   pp(voor)],[]).

a([e(slechtziende),
   no_e(slechtziend)],padv,[],[]).

a([e(sleetse),
   no_e(sleets)],padv,[],[]).

a([ende(slepende),
   er(slepender),
   ere(slependere),
   end(slepend),
   st(slependst),
   ste(slependste)],padv,[],[]).

a([e(slijmerige),
   er(slijmeriger),
   ere(slijmerigere),
   no_e(slijmerig),
   st(slijmerigst),
   ste(slijmerigste)],adv,[],[]).

a([e(slimme),
   er(slimmer),
   ere(slimmere),
   no_e(slim),
   st(slimst),
   ste(slimste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(slinkse),
   er(slinkser),
   ere(slinksere),
   no_e(slinks),
   st(slinkst),
   ste(slinkste)],adv,[],[]).

a([e(slome),
   er(slomer),
   ere(slomere),
   no_e(sloom),
   st(sloomst),
   ste(sloomste)],adv,[],[]).

a([e(slordige),
   er(slordiger),
   ere(slordigere),
   no_e(slordig),
   st(slordigst),
   ste(slordigste)],adv,[subject_sbar,
                         subject_vp],[]).

a([e(sluike),
   no_e(sluik)],nonadv,[],[]).

a([e(sluikse),
   no_e(sluiks)],adv,[],[]).

a([e(sluwe),
   er(sluwer),
   ere(sluwere),
   no_e(sluw),
   st(sluwst),
   ste(sluwste)],adv,[],[]).

a([e(smaakvolle),
   er(smaakvoller),
   ere(smaakvollere),
   no_e(smaakvol),
   st(smaakvolst),
   ste(smaakvolste)],adv,[],[]).

a([e(smadelijke),
   er(smadelijker),
   ere(smadelijkere),
   no_e(smadelijk),
   st(smadelijkst),
   ste(smadelijkste)],adv,[],[]).

a([e(smakelijke),
   er(smakelijker),
   ere(smakelijkere),
   no_e(smakelijk),
   st(smakelijkst),
   ste(smakelijkste)],adv,[],[]).

a([e(smakeloze),
   er(smakelozer),
   ere(smakelozere),
   no_e(smakeloos),
   st(smakeloost),
   ste(smakelooste)],adv,[subject_sbar,
			  subject_vp],[]).

a([e(smalle),
   er(smaller),
   ere(smallere),
   no_e(smal),
   st(smalst),
   ste(smalste)],nonadv,[],[]).

a([e(smalende),
   er(smalender),
   ere(smalendere),
   no_e(smalend),
   st(smalendst),
   ste(smalendste)],adv,[],[]).

a([e(smartelijke),
   er(smartelijker),
   ere(smartelijkere),
   no_e(smartelijk),
   st(smartelijkst),
   ste(smartelijkste)],adv,[],[]).

a([stof(smeedijzeren)],nonadv,[],[]).

a([e(smerige),
   er(smeriger),
   ere(smerigere),
   no_e(smerig),
   st(smerigst),
   ste(smerigste)],adv,[],[]).

a([e(smetteloze),
   er(smettelozer),
   ere(smettelozere),
   no_e(smetteloos),
   st(smetteloost),
   ste(smettelooste)],adv,[],[]).

a([e(smeuïge),
   no_e(smeuïg)],adv,[],[]).

a([e(smoezelige),
   er(smoezeliger),
   ere(smoezeligere),
   no_e(smoezelig),
   st(smoezeligst),
   ste(smoezeligste)],nonadv,[],[]).

a([e(snaakse),
   no_e(snaaks)],adv,[],[]).

a([both(sneaky)],adv,[],[]).

a([e(sneeuwwitte),
   er(sneeuwwitter),
   ere(sneeuwwittere),
   no_e(sneeuwwit),
   st(sneeuwwitst),
   ste(sneeuwwitste)],nonadv,[],[]).

a([e(snelle),
   er(sneller),
   ere(snellere),
   no_e(snel),
   st(snelst),
   ste(snelste)],adv,[],[super]).

a([e(sneue),
   no_e(sneu)],nonadv,
  [subject_sbar],[]).

a([e(snijdende),
   er(snijdender),
   ere(snijdendere),
   no_e(snijdend),
   st(snijdendst),
   ste(snijdendste)],adv,
  [transitive],[twee]).

a([e(snikhete),
   no_e(snikheet)],adv,[],[]).

a([e(snode),
   no_e(snood)],nonadv,[],[]).

a([e(snotterige),
   no_e(snotterig)],adv,[],[]).

a([e(snuggere),
   no_e(snugger)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(sobere),
   er(soberder),
   ere(soberdere),
   no_e(sober),
   st(soberst),
   ste(soberste)],adv,[],[]).

a([pred(sobertjes)],adv,[],[]).

a([e(sociale),
   er(socialer),
   ere(socialere),
   no_e(sociaal),
   st(sociaalst),
   ste(sociaalste)],adv,[],[]).

a([e('sociaal-culturele'),
   er('sociaal-cultureler'),
   ere('sociaal-culturelere'),
   no_e('sociaal-cultureel'),
   st('sociaal-cultureelst'),
   ste('sociaal-cultureelste')],adv,[],[]).

a([e('sociaal-democratische'),
   e('sociaal-demokratische'),
   er('sociaal-democratischer'),
   er('sociaal-demokratischer'),
   ere('sociaal-democratischere'),
   ere('sociaal-demokratischere'),
   no_e('sociaal-democratisch'),
   no_e('sociaal-demokratisch'),
   st('sociaal-democratischt'),
   st('sociaal-demokratischt'),
   ste('sociaal-democratischte'),
   ste('sociaal-demokratischte')],nonadv,[],[]).

a([e('sociaal-economische'),
   e('sociaal-ekonomische'),
   er('sociaal-economischer'),
   er('sociaal-ekonomischer'),
   ere('sociaal-economischere'),
   ere('sociaal-ekonomischere'),
   no_e('sociaal-economisch'),
   no_e('sociaal-ekonomisch'),
   st('sociaal-economischt'),
   st('sociaal-ekonomischt'),
   ste('sociaal-economischte'),
   ste('sociaal-ekonomischte')],adv,[],[]).

a([e(socialistische),
   er(socialistischer),
   ere(socialistischere),
   no_e(socialistisch),
   st(socialistischt),
   ste(socialistischte)],adv,[],[]).

a([e(sociologische),
   no_e(sociologisch)],adv,[],[]).

a([e(soennitische),
   no_e(soennitisch),
   e(soennietische),
   no_e(soennietisch),
   e(sunnitische),
   no_e(sunnitisch)],nonadv,[],[]).

a([e(soepele),
   er(soepeler),
   ere(soepelere),
   no_e(soepel),
   st(soepelst),
   ste(soepelste)],adv,[],[]).

a([pred(soepeltjes)],adv,[],[]).

a([e(soevereine),
   no_e(soeverein)],adv,[],[]).

a([e(softe),
   no_e(soft)],adv,[],[]).

a([e(solidaire),
   no_e(solidair)],adv,
  [pp(met)],[]).

a([both(solide),
   er(solider),
   ere(solidere)],adv,[],[]).

a([e(solitaire),
   no_e(solitair)],adv,[],[]).

a([e(somatische),
   no_e(somatisch)],nonadv,[],[]).

a([e(sombere),
   er(somberder),
   ere(somberdere),
   no_e(somber),
   st(somberst),
   ste(somberste)],adv,
  [er_pp_vp(over),
   pp(over)],[]).

a([e(sonore),
   er(sonoorder),
   ere(sonoordere),
   no_e(sonoor),
   st(sonoorst),
   ste(sonoorste)],adv,[],[]).

a([e(soortgelijke),
   no_e(soortgelijk)],nonadv,[],[]).

a([both(sophisticated)],adv,[],[]).

a([e(souvereine),
   no_e(souverein)],padv,[],[]).

%% een sovjet paar
a([prefix(soviet),
   prefix('Soviet'),
   prefix(sovjet),
   prefix('Sovjet'),
   prefix(sowjet),
   prefix('Sowjet')],nonadv,[],[]).

a([e(spaarzame),
   er(spaarzamer),
   ere(spaarzamere),
   no_e(spaarzaam),
   st(spaarzaamst),
   ste(spaarzaamste)],adv,
  [pp(met)],[]).

a([e(spannende),
   er(spannender),
   ere(spannendere),
   no_e(spannend),
   st(spannendst),
   ste(spannendste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(spastische),
   er(spastischer),
   ere(spastischere),
   no_e(spastisch),
   st(spastischt),
   ste(spastischte)],adv,[],[]).

a([e(speciale),
   no_e(speciaal)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(specialistische),
   er(specialistischer),
   ere(specialistischere),
   no_e(specialistisch),
   st(specialistischt),
   ste(specialistischte)],adv,[],[]).

a([e(specifieke),
   no_e(specifiek)],adv,[],[]).

a([e(spectaculaire),
   no_e(spectaculair),
   er(spectaculairder),
   ere(spectaculairdere),
   st(spectaculairst),
   ste(spectaculairste)],adv,
  [subject_vp],[]).

a([e(speculatieve),
   e(spekulatieve),
   er(speculatiever),
   er(spekulatiever),
   ere(speculatievere),
   ere(spekulatievere),
   no_e(speculatief),
   no_e(spekulatief),
   st(speculatiefst),
   st(spekulatiefst),
   ste(speculatiefste),
   ste(spekulatiefste)],adv,[],[]).

a([e(speelbare),
   no_e(speelbaar)],padv,[],[aan]).

a([e(speelklare),no_e(speelklaar)],padv,[],[]).

a([e(speelse),
   er(speelser),
   ere(speelsere),
   no_e(speels),
   st(speelst),
   ste(speelste)],adv,[],[]).

a([e(spichtige),
   er(spichtiger),
   ere(spichtigere),
   no_e(spichtig),
   st(spichtigst),
   ste(spichtigste)],adv,[],[]).

a([e(spiegelgladde),
   er(spiegelgladder),
   ere(spiegelgladdere),
   no_e(spiegelglad),
   st(spiegelgladst),
   ste(spiegelgladste)],nonadv,[],[]).

a([e(spiernaakte),
   no_e(spiernaakt)],padv,[],[]).

a([e(spierwitte),
   er(spierwitter),
   ere(spierwittere),
   no_e(spierwit),
   st(spierwitst),
   ste(spierwitste)],padv,[],[]).

a([e(spijtige),
   er(spijtiger),
   ere(spijtigere),
   no_e(spijtig),
   st(spijtigst),
   ste(spijtigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(spirituele),
   er(spiritueler),
   ere(spirituelere),
   no_e(spiritueel),
   st(spiritueelst),
   ste(spiritueelste)],adv,[],[]).

a([e(spitse),
   er(spitser),
   ere(spitsere),
   no_e(spits),
   st(spitst),
   ste(spitste)],adv,[],[]).

a([e(splinternieuwe),
   er(splinternieuwer),
   ere(splinternieuwere),
   no_e(splinternieuw),
   st(splinternieuwst),
   ste(splinternieuwste)],padv,[],[]).

a([e(spoedeisende),
   no_e(spoedeisend)],nonadv,[],[]).

a([e(spoedige),
   er(spoediger),
   ere(spoedigere),
   no_e(spoedig),
   st(spoedigst),
   ste(spoedigste)],adv,[],[]).

a([e(spontane),
   er(spontaner),
   ere(spontanere),
   no_e(spontaan),
   st(spontaanst),
   ste(spontaanste)],adv,[],[]).

a([e(spookachtige),
   er(spookachtiger),
   ere(spookachtigere),
   no_e(spookachtig),
   st(spookachtigst),
   ste(spookachtigste)],adv,[],[]).

a([e(spoorloze),
   er(spoorlozer),
   ere(spoorlozere),
   no_e(spoorloos),
   st(spoorloost),
   ste(spoorlooste)],adv,[],[]).

a([e(sporadische),
   er(sporadischer),
   ere(sporadischere),
   no_e(sporadisch),
   st(sporadischt),
   ste(sporadischte)],adv,[],[]).

a([e(sportieve),
   er(sportiever),
   ere(sportievere),
   no_e(sportief),
   st(sportiefst),
   ste(sportiefste)],adv,[],[]).

a([e(spraakmakende),
   no_e(spraakmakend)],nonadv,[],[]).

a([e(spraakzame),
   er(spraakzamer),
   ere(spraakzamere),
   no_e(spraakzaam),
   st(spraakzaamst),
   ste(spraakzaamste)],padv,[],[]).

a([e(sprakeloze),
   er(sprakelozer),
   ere(sprakelozere),
   no_e(sprakeloos),
   st(sprakeloost),
   ste(sprakelooste)],padv,[],[]).

a([e(sprankelende),
   no_e(sprankelend)],adv,[],[]).

a([e(spreekwoordelijke),
   no_e(spreekwoordelijk)],adv,[],[]).

a([ende(sprekende),
   end(sprekend),
   ere(sprekendere),
   er(sprekender)],padv,
  [transitive,
   subject_sbar],[]).

a([ende(sprekende),
   end(sprekend),
   ere(sprekendere),
   er(sprekender)],padv,
  [],['Nederlands',h('Nederlands'),nederlands,
      'Engels',h('Engels'),
      'Spaans',h('Spaans'),
      'Duits',h('Duits'),
      'Frans',h('Frans')]).

a([e(springlevende),
   no_e(springlevend)],padv,[],[]).

a([e(sprookjesachtige),
   er(sprookjesachtiger),
   ere(sprookjesachtigere),
   no_e(sprookjesachtig),
   st(sprookjesachtigst),
   ste(sprookjesachtigste)],adv,[],[]).

a([e(staatkundige),
   no_e(staatkundig)],adv,[],[]).

a([e(staatse),
   no_e(staats)],nonadv,[],[]).

a([e(staatsrechtelijke),
   no_e(staatsrechtelijk)],adv,[],[]).

a([e(stabiele),
   er(stabieler),
   ere(stabielere),
   no_e(stabiel),
   st(stabielst),
   ste(stabielste)],adv,[],[on]).

a([both(staccato)],adv,[],[]).

a([e(stadse),
   no_e(stads)],adv,[],[]).

a([stof(stalen)],nonadv,[],[]).

a([e(stalinistische),
   no_e(stalinistisch)],adv,[],[]).

a([both([stand,by]),
   both(standby),
   both('stand-by')],padv,[],[]).

a([prefix([stand,up]),
   prefix('stand-up'),
   prefix(standup)],nonadv,[],[]).

a([both(standaard)],adv,[],[]).

a([ge_both(standgehouden)],adv,[],[]).

a([e(standvastige),
   er(standvastiger),
   ere(standvastigere),
   no_e(standvastig),
   st(standvastigst),
   ste(standvastigste)],adv,[],[]).

a([e(stapelgekke),
   er(stapelgekker),
   ere(stapelgekkere),
   no_e(stapelgek),
   st(stapelgekst),
   ste(stapelgekste)],adv,[],[]).

a([e(stapsgewijze),
   no_e(stapsgewijs)],adv,[],[]).

a([e(stapvoetse),
   no_e(stapvoets)],adv,[],[]).

a([e(starre),
   er(starder),
   ere(stardere),
   no_e(star),
   st(starst),
   ste(starste)],adv,[],[]).

a([no_e(startensklaar),
   e(startensklare)],padv,[],[]).

a([e(statige),
   er(statiger),
   ere(statigere),
   no_e(statig),
   st(statigst),
   ste(statigste)],adv,[],[]).

a([e(statische),
   er(statischer),
   ere(statischere),
   no_e(statisch),
   st(statischt),
   ste(statischte)],nonadv,[],[]).

a([e(statistische),
   no_e(statistisch)],adv,[],[]).

a([e(stedebouwkundige),
   no_e(stedebouwkundig),
   e(stedenbouwkundige),
   no_e(stedenbouwkundig)],adv,[],[]).

a([e(stedelijke),
   no_e(stedelijk)],nonadv,[],[]).

a([e(steelse),
   no_e(steels)],adv,[],[]).

a([e(steevaste),
   no_e(steevast)],adv,[],[]).

a([e(steile),
   er(steiler),
   ere(steilere),
   no_e(steil),
   st(steilst),
   ste(steilste)],adv,[],[]).

a([e(stekelige),
   er(stekeliger),
   ere(stekeligere),
   no_e(stekelig),
   st(stekeligst),
   ste(stekeligste)],adv,[],[]).

a([e(stekende),
   er(stekender),
   ere(stekendere),
   no_e(stekend),
   st(stekendst),
   ste(stekendste)],adv,[],[]).

a([e(stellige),
   er(stelliger),
   ere(stelligere),
   no_e(stellig),
   st(stelligst),
   ste(stelligste)],adv,[],[]).

a([e(stelselmatige),
   er(stelselmatiger),
   ere(stelselmatigere),
   no_e(stelselmatig),
   st(stelselmatigst),
   ste(stelselmatigste)],adv,[],[]).

a([e(stemmige),
   er(stemmiger),
   ere(stemmigere),
   no_e(stemmig),
   st(stemmigst),
   ste(stemmigste)],adv,[],[]).

a([stof(stenen)],nonadv,[],[natuur]).

a([e(stenige),
   no_e(stenig)],nonadv,[],[]).

a([both(stereo)],nonadv,[],[]).

a([e(stereotiepe),
   er(stereotieper),
   ere(stereotiepere),
   no_e(stereotiep),
   st(stereotiepst),
   ste(stereotiepste),
   e(stereotype),
   er(stereotyper),
   ere(stereotypere),
   no_e(stereotyp),
   st(stereotypst),
   ste(stereotypste)],adv,[],[]).

a([e(sterfelijke),
   no_e(sterfelijk)],nonadv,[],[]).

a([e(steriele),
   er(sterieler),
   ere(sterielere),
   no_e(steriel),
   st(sterielst),
   ste(sterielste)],adv,[],[]).

a([e(sterke),
   er(sterker),
   ere(sterkere),
   no_e(sterk),
   st(sterkst),
   ste(sterkste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(sterrenkundige),
   no_e(sterrenkundig)],adv,[],[]).

a([e(stevige),
   er(steviger),
   ere(stevigere),
   no_e(stevig),
   st(stevigst),
   ste(stevigste)],adv,[],[]).

a([e(stichtelijke),
   er(stichtelijker),
   ere(stichtelijkere),
   no_e(stichtelijk),
   st(stichtelijkst),
   ste(stichtelijkste)],adv,[],[]).

a([no_e(stief)],nonadv,[],[]).

a([e(stiekeme),
   no_e(stiekem)],adv,[],[]).

a([e(stijve),
   er(stijver),
   ere(stijvere),
   no_e(stijf),
   st(stijfst),
   ste(stijfste)],adv,[],[]).

a([pred(stijfjes)],adv,[],[]).

a([e(stijlvolle),
   er(stijlvoller),
   ere(stijlvollere),
   no_e(stijlvol),
   st(stijlvolst),
   ste(stijlvolste)],adv,[],[]).

a([e(stille),
   er(stiller),
   ere(stillere),
   no_e(stil),
   st(stilst),
   ste(stilste)],adv,[],[]).

a([ge_both(stilgehouden)],adv,[],[]).

a([ge_e(stilgelegde),
   ge_no_e(stilgelegd)],adv,[],[]).

a([ge_both(stilgevallen)],adv,[],[]).

a([ge_both(stilgezwegen)],adv,[],[]).

a([e(stilistische),
   no_e(stilistisch)],adv,[],[]).

a([pred(stilletjes)],adv,[],[]).

a([pred([stilletjes,aan])],adv,[],[]).

a([e(stipte),
   er(stipter),
   ere(stiptere),
   no_e(stipt),
   st(stiptst),
   ste(stiptste)],adv,[],[]).

a([e(stoere),
   er(stoerder),
   ere(stoerdere),
   no_e(stoer),
   st(stoerst),
   ste(stoerste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(stoffelijke),
   no_e(stoffelijk)],adv,[],[]).

a([stof(stoffen)],nonadv,[],[]).

a([e(stoffige),
   er(stoffiger),
   ere(stoffigere),
   no_e(stoffig),
   st(stoffigst),
   ste(stoffigste)],nonadv,[],[]).

a([e(stoïcijnse),
   er(stoïcijnser),
   ere(stoïcijnsere),
   no_e(stoïcijns),
   st(stoïcijnst),
   ste(stoïcijnste)],adv,[],[]).

a([e(stokstijve),
   no_e(stokstijf)],adv,[],[]).

a([e(stomme),
   er(stommer),
   ere(stommere),
   no_e(stom),
   st(stomst),
   ste(stomste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([both(stomdronken),
   er(stomdronkener),
   ere(stomdronkenere),
   st(stomdronkenst),
   ste(stomdronkenste)],padv,[],[]).

a([e(stompe),
   er(stomper),
   ere(stompere),
   no_e(stomp),
   st(stompst),
   ste(stompste)],adv,[],[]).

a([e(stompzinnige),
   er(stompzinniger),
   ere(stompzinnigere),
   no_e(stompzinnig),
   st(stompzinnigst),
   ste(stompzinnigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(stomverbaasde),
   no_e(stomverbaasd)],padv,
  [object_sbar,
   object_vp,
   er_pp_sbar(over),
   er_pp_vp(over),
   pp(over)],[]).

a([pred(stoned)],padv,[],[]).

a([ge_e(stopgezette),
   ge_no_e(stopgezet)],adv,[],[]).

a([e(storende),
   er(storender),
   ere(storendere),
   no_e(storend),
   st(storendst),
   ste(storendste)],adv,
  [subject_sbar,
   transitive],[]).

a([e(stormachtige),
   er(stormachtiger),
   ere(stormachtigere),
   no_e(stormachtig),
   st(stormachtigst),
   ste(stormachtigste)],adv,[],[]).

a([e(stoute),
   er(stouter),
   ere(stoutere),
   no_e(stout),
   st(stoutst),
   ste(stoutste)],adv,[],[]).

a([e(stoutmoedige),
   er(stoutmoediger),
   ere(stoutmoedigere),
   no_e(stoutmoedig),
   st(stoutmoedigst),
   ste(stoutmoedigste)],adv,[],[]).

a([e(straatarme),
   er(straatarmer),
   ere(straatarmere),
   no_e(straatarm),
   st(straatarmst),
   ste(straatarmste)],padv,[],[]).

a([e(straffe),
   er(straffer),
   ere(straffere),
   no_e(straf),
   st(strafst),
   ste(strafste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(strafbare),
   er(strafbaarder),
   ere(strafbaardere),
   no_e(strafbaar),
   st(strafbaarst),
   ste(strafbaarste)],adv,[subject_vp,
			   subject_sbar],[]).

a([e(straffeloze),
   no_e(straffeloos)],adv,[],[]).

a([e(strafrechtelijke),
   no_e(strafrechtelijk)],adv,[],[]).

a([e(strakke),
   er(strakker),
   ere(strakkere),
   no_e(strak),
   st(strakst),
   ste(strakste)],adv,
  [pp(in)],[]).

a([e(stralende),
   er(stralender),
   ere(stralendere),
   no_e(stralend),
   st(stralendst),
   ste(stralendste)],adv,[],[]).

a([e(stramme),
   er(strammer),
   ere(strammere),
   no_e(stram),
   st(stramst),
   ste(stramste)],adv,[],[]).

a([e(strategische),
   no_e(strategisch)],adv,[],[]).

a([e(strenge),
   er(strenger),
   ere(strengere),
   no_e(streng),
   st(strengst),
   ste(strengste)],adv,
  [pp(voor)],[]).

a([e(strijdbare),
   er(strijdbaarder),
   ere(strijdbaardere),
   no_e(strijdbaar),
   st(strijdbaarst),
   ste(strijdbaarste)],padv,[],[]).

a([e(strijdige),
   er(strijdiger),
   ere(strijdigere),
   no_e(strijdig),
   st(strijdigst),
   ste(strijdigste)],nonadv,
  [pp(met)],[]).

a([e(strijdlustige),
   er(strijdlustiger),
   ere(strijdlustigere),
   no_e(strijdlustig),
   st(strijdlustigst),
   ste(strijdlustigste)],padv,[],[]).

a([e(strikte),
   er(strikter),
   ere(striktere),
   no_e(strikt),
   st(striktst),
   ste(striktste),
   e(stricte),
   er(stricter),
   ere(strictere),
   no_e(strict),
   st(strictst),
   ste(strictste)],adv,[],[]).

a([e(stringente),
   no_e(stringent),
   ere(stringentere),
   er(stringenter),
   st(stringentst),
   ste(stringentste)],nonadv,[],[]).

a([stof(strobalen)],nonadv,[],[]).

a([e(stroeve),
   er(stroever),
   ere(stroevere),
   no_e(stroef),
   st(stroefst),
   ste(stroefste)],adv,[],[]).

a([stof(strooien)],nonadv,[],[]).

a([e(stroomafwaartse),
   postn_no_e(stroomafwaarts)],diradv,[],[]).

a([e(stroomopwaartse),
   postn_no_e(stroomopwaarts)],diradv,[],[]).

a([e(structurele),
   e(strukturele),
   no_e(structureel),
   no_e(struktureel),
   er(structureler),
   er(struktureler),
   ere(structurelere),
   ere(strukturelere),
   st(structureelst),
   st(struktureelst),
   ste(structureelste),
   ste(struktureelste)],adv,[],[]).

a([e(struise),
   er(struiser),
   ere(struisere),
   no_e(struis),
   st(struist),
   ste(struiste)],adv,[],[]).

a([e(studentikoze),
   no_e(studentikoos)],adv,[],[]).

a([e(stugge),
   er(stugger),
   ere(stuggere),
   no_e(stug),
   st(stugst),
   ste(stugste)],adv,[],[]).

a([pred(stuk)],nonadv,[],[]).

a([ge_both(stukgelopen)],nonadv,[],[]).

a([ge_both(stukgeslagen)],adv,[],[]).

a([e(stuntelige),
   er(stunteliger),
   ere(stunteligere),
   no_e(stuntelig),
   st(stunteligst),
   ste(stunteligste)],adv,[],[]).

a([both(stupide)],adv,[],[]).

a([e(stuurloze),
   no_e(stuurloos)],adv,[],[]).

a([e(stuurse),
   er(stuurser),
   ere(stuursere),
   no_e(stuurs),
   st(stuurst),
   ste(stuurste)],adv,[],[]).

a([e(subjectieve),
   e(subjektieve),
   er(subjectiever),
   er(subjektiever),
   ere(subjectievere),
   ere(subjektievere),
   no_e(subjectief),
   no_e(subjektief),
   st(subjectiefst),
   st(subjektiefst),
   ste(subjectiefste),
   ste(subjektiefste)],adv,[],[]).

a([e(sublieme),
   er(subliemer),
   ere(subliemere),
   no_e(subliem),
   st(subliemst),
   ste(subliemste)],adv,[],[]).

a([e(substantiële),
   no_e(substantieel)],adv,[],[]).

a([e(subtiele),
   er(subtieler),
   ere(subtielere),
   no_e(subtiel),
   st(subtielst),
   ste(subtielste)],adv,[],[]).

a([e(subtropische),
   no_e(subtropisch)],nonadv,[],[]).

a([e(subversieve),
   er(subversiever),
   ere(subversievere),
   no_e(subversief),
   st(subversiefst),
   ste(subversiefste)],nonadv,[],[]).

a([e(succesvolle),
   e(suksesvolle),
   er(succesvoller),
   er(suksesvoller),
   ere(succesvollere),
   ere(suksesvollere),
   no_e(succesvol),
   no_e(suksesvol),
   st(succesvolst),
   st(suksesvolst),
   ste(succesvolste),
   ste(suksesvolste)],adv,
  [subject_sbar,
   pp(bij),
   pp(in),
   pp(met),
   pp(voor)],[on]).

a([e(suffe),
   er(suffer),
   ere(suffere),
   no_e(suf),
   st(sufst),
   ste(sufste)],adv,[],[]).

a([e(suggestieve),
   er(suggestiever),
   ere(suggestievere),
   no_e(suggestief),
   st(suggestiefst),
   ste(suggestiefste)],adv,[],[]).

a([e(summiere),
   er(summierder),
   ere(summierdere),
   no_e(summier),
   st(summierst),
   ste(summierste)],adv,[],[]).

a([both(super)],adv,[subject_sbar],[]).

a([e(superbe),
   no_e(superb)],nonadv,[],[]).

a([e(superieure),
   no_e(superieur)],adv,[],[]).

a([e(supranationale),
   no_e(supranationaal)],nonadv,[],[]).

a([e(surrealistische),
   er(surrealistischer),
   ere(surrealistischere),
   no_e(surrealistisch),
   st(surrealistischt),
   ste(surrealistischte)],adv,[],[]).

a([stof(suède)],nonadv,[],[]).

a([e(symbiotische),
   no_e(symbiotisch)],nonadv,[],[]).

a([e(symbolische),
   no_e(symbolisch)],adv,[],[]).

a([e(symbolistische),
   no_e(symbolistisch)],nonadv,[],[]).

a([e(symfonische),
   no_e(symfonisch)],adv,[],[]).

a([e(symmetrische),
   no_e(symmetrisch)],adv,[],[]).

a([e(sympathieke),
   e(sympatieke),
   er(sympathieker),
   er(sympatieker),
   ere(sympathiekere),
   ere(sympatiekere),
   no_e(sympathiek),
   no_e(sympatiek),
   st(sympathiekst),
   st(sympatiekst),
   ste(sympathiekste),
   ste(sympatiekste)],adv,[],[]).

a([e(symptomatische),
   no_e(symptomatisch)],adv,[],[]).

a([e(synchrone),
   no_e(synchroon)],adv,[],[]).

a([e(synonieme),
   no_e(synoniem)],nonadv,[],[]).

a([e(syntactische),
   no_e(syntactisch)],adv,[],[]).

a([e(syntetische),
   e(synthetische),
   no_e(syntetisch),
   no_e(synthetisch)],adv,[],[]).

a([e(systematische),
   er(systematischer),
   ere(systematischere),
   no_e(systematisch),
   st(systematischt),
   ste(systematischte)],adv,[],[]).

a([e(taaie),
   er(taaier),
   ere(taaiere),
   no_e(taai),
   st(taaist),
   ste(taaiste)],adv,[],[]).

a([e(taalkundige),
   no_e(taalkundig)],adv,[],[]).

a([pred(taboe)],nonadv,[],[]).

a([both(tachtiger)],nonadv,[],[]).

a([e(tactische),
   e(taktische),
   er(tactischer),
   er(taktischer),
   ere(tactischere),
   ere(taktischere),
   no_e(tactisch),
   no_e(taktisch),
   st(tactischt),
   st(taktischt),
   ste(tactischte),
   ste(taktischte)],adv,[],[]).

a([e(tactvolle),
   e(taktvolle),
   er(tactvoller),
   er(taktvoller),
   ere(tactvollere),
   ere(taktvollere),
   no_e(tactvol),
   no_e(taktvol),
   st(tactvolst),
   st(taktvolst),
   ste(tactvolste),
   ste(taktvolste)],adv,[],[]).

a([e(talentvolle),
   er(talentvoller),
   ere(talentvollere),
   no_e(talentvol),
   st(talentvolst),
   ste(talentvolste)],adv,[],[]).

a([e(talige),
   no_e(talig)],nonadv,[],
  [anders,
   'Nederlands',h('Nederlands'),nederlands,
   'Engels',h('Engels'),engels,
   'Spaans',h('Spaans'),spaans,
   'Duits',h('Duits'),duits,
   'Frans',h('Frans'),frans]).

a([e(talloze),
   no_e(talloos)],adv,[],[]).

a([e(talrijke),
   er(talrijker),
   ere(talrijkere),
   no_e(talrijk),
   st(talrijkst),
   ste(talrijkste)],adv,[],[]).

a([e(tamme),
   er(tammer),
   ere(tammere),
   no_e(tam),
   st(tamst),
   ste(tamste)],adv,[],[]).

a([e(tamelijke),
   no_e(tamelijk)],adv,[],[]).

a([e(tandeloze),
   no_e(tandeloos)],padv,[],[]).

a([e(tandheelkundige),
   no_e(tandheelkundig)],nonadv,[],[]).

a([e(tanige),
   er(taniger),
   ere(tanigere),
   no_e(tanig),
   st(tanigst),
   ste(tanigste)],nonadv,[],[]).

a([e(tapse),
   no_e(taps)],adv,[],[]).

a([e(tastbare),
   er(tastbaarder),
   ere(tastbaardere),
   no_e(tastbaar),
   st(tastbaarst),
   ste(tastbaarste)],adv,[],[]).

a([both(taxfree),
   both('tax-free'),
   both([tax,free])],adv,[],[]).

a([e(taxonomische),
   no_e(taxonomisch)],nonadv,[],[]).

a([pred([te,barsten])],nonadv,[],[]).

a([e([te,doen,gebruikelijke]),
   no_e([te,doen,gebruikelijk])],adv,[],[]).

a([pred([te,gast])],padv,[],[]).

a([pred([te,geef])],nonadv,[],[]).

a([pred([te,goeder,trouw])],adv,[],[]).

a([pred([te,gortig])],nonadv,[],[]).

a([pred([te,gronde])],nonadv,[],[]).

a([pred([te,huur])],nonadv,[],[]).

a([pred([te,kijk])],nonadv,[],[]).

a([pred([te,koop])],nonadv,[],[]).

a([pred([te,krijg])],nonadv,[],[]). %VL

a([pred([te,kwader,trouw])],adv,[],[]).

a([pred([te,leen])],nonadv,[],[]).

a([pred([te,na])],nonadv,[transitive],[]).

a([pred([te,over])],nonadv,[],[]).

a([pred([te,pletter])],adv,[],[]).

a([pred([te,ruil])],nonadv,[],[]).

a([e(technische),
   no_e(technisch)],adv,[],[s(begroting)]).

a([e(technocratische),
   e(technokratische),
   no_e(technocratisch),
   no_e(technokratisch)],adv,[],[]).

a([e(technologische),
   no_e(technologisch)],adv,[],[]).

a([e(tedere),
   er(tederder),
   ere(tederdere),
   no_e(teder),
   st(tederst),
   ste(tederste)],adv,[],[]).

a([e(tere),
   er(teerder),
   ere(teerdere),
   no_e(teer),
   st(teerst),
   ste(teerste)],adv,[],[]).

a([stem(te),
   pred(tè),
   pred(té)],nonadv,[],[]).

a([pred(tegelijk)],adv,
  [pp(met)],[]).

a([pred(tegelijkertijd)],adv,
  [pp(met)],[]).

a([pred(tegen)],nonadv,
  [pp(op),
   er_pp_sbar(op),
   er_pp_vp(op)],[]).

a([e(tegendraadse),
   no_e(tegendraads)],both,[],[]).

a([ge_e(tegengegane),
   ge_no_e(tegengegaan)],adv,[],[]).

a([ge_both(tegengehouden)],adv,[],[]).

a([ge_both(tegengekomen)],adv,[],[]).

a([ge_both(tegengesproken)],adv,[],[]).

a([e(tegengestelde),
   no_e(tegengesteld)],adv,
  [pp(aan)],[]).

a([ge_both(tegengevallen)],adv,[],[]).

a([ge_e(tegengewerkte),
   ge_no_e(tegengewerkt)],adv,[],[]).

a([ge_both(tegengeworpen)],adv,[],[]).

a([e(tegennatuurlijke),
   er(tegennatuurlijker),
   ere(tegennatuurlijkere),
   no_e(tegennatuurlijk),
   st(tegennatuurlijkst),
   ste(tegennatuurlijkste)],adv,[],[]).

a([ge_e(tegenovergestelde),
   ge_no_e(tegenovergesteld)],adv,[],[]).

a([e(tegenoverliggende),
   no_e(tegenoverliggend)],nonadv,[],[]).

a([e(tegenstrijdige),
   er(tegenstrijdiger),
   ere(tegenstrijdigere),
   no_e(tegenstrijdig),
   st(tegenstrijdigst),
   ste(tegenstrijdigste)],adv,[],[subject_sbar]).

a([e(tegenwoordige),
   no_e(tegenwoordig)],tmpadv,[],[]).

a([ge_e(tekeergegane),
   ge_no_e(tekeergegaan)],adv,[],[]).

a([ende(tekenende),
   er(tekenender),
   ere(tekenendere),
   end(tekenend),
   st(tekenendst),
   ste(tekenendste)],padv,
  [subject_sbar],[]).

a([ge_both(tekortgeschoten)],padv,[],[]).

a([e(tekstuele),
   no_e(tekstueel)],adv,[],[]).

a([e(telefonische),
   no_e(telefonisch)],adv,[],[]).

a([e(teleologische),
   no_e(teleologisch)],nonadv,[],[]).

a([e(telepathische),
   e(telepatische),
   no_e(telepathisch),
   no_e(telepatisch)],adv,[],[]).

a([ge_e(teleurgestelde),
   ge_no_e(teleurgesteld)],padv,
  [object_sbar],[]).

a([e(teleurstellende),
   er(teleurstellender),
   ere(teleurstellendere),
   no_e(teleurstellend),
   st(teleurstellendst),
   ste(teleurstellendste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([ge_e(teloorgegane),
   ge_no_e(teloorgegaan)],nonadv,[],[]).

a([stof(tempex)],nonadv,[],[]).

a([e(temporiserende),
   no_e(temporiserend)],padv,[],[]).

a([pred([ten,onrechte])],adv,
  [subject_sbar,
   subject_vp],[]).

a([pred([ten,spijt])],adv,
  [transitive],[]).

a([e(tendentieuze),
   no_e(tendentieus)],adv,[],[]).

a([both(teneergeslagen)],padv,[],[]).

a([pred([ten,einde]),
   pred(teneinde)],nonadv,[],[]).

a([pred([ten,einde,raad])],padv,[],[]).

a([e(tengere),
   er(tengerder),
   ere(tengerdere),
   no_e(tenger),
   st(tengerst),
   ste(tengerste)],nonadv,[],[]).

a([ge_e(tentoongestelde),
   ge_no_e(tentoongesteld)],adv,[],[]).

a([pred([ter,beschikking])],nonadv,[pp(van)],[]).

a([pred([ter,ziele])],nonadv,[],[]).

a([e(terechte),
   no_e(terecht)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(terechtgestelde),
   ge_no_e(terechtgesteld)],adv,[],[]).

a([ge_both(terechtgewezen)],adv,[],[]).

a([e(tergende),
   er(tergender),
   ere(tergendere),
   no_e(tergend),
   st(tergendst),
   ste(tergendste)],adv,[],[]).

a([no_e(terloops),
   e(terloopse)],adv,[],[]).

a([e(terminale),no_e(terminaal)],nonadv,[],[]).

a([ge_both(terneergeslagen),
   er(terneergeslagener),
   ere(terneergeslagenere),
   st(terneergeslagenst),
   ste(terneergeslagenste)],adv,[],[]).

a([prefix(terracotta)],nonadv,[],[]).

a([e(territoriale),
   no_e(territoriaal)],nonadv,[],[]).

a([e(terroristische),
   er(terroristischer),
   ere(terroristischere),
   no_e(terroristisch),
   st(terroristischt),
   ste(terroristischte)],adv,[],[]).

a([e(tersluikse),
   no_e(tersluiks)],adv,[],[]).

a([e(tertiaire),
   no_e(tertiair)],nonadv,[],[]).

a([pred([terug,van,weggeweest])],nonadv,[],[]).

a([ge_e(terugbetaalde),
   ge_no_e(terugbetaald)],adv,[],[]).

a([ge_e(teruggebrachte),
   ge_no_e(teruggebracht)],adv,[],[]).

a([ge_e(teruggedachte),
   ge_no_e(teruggedacht)],adv,[],[]).

a([ge_e(teruggedeinsde),
   ge_no_e(teruggedeinsd)],adv,[],[]).

a([ge_e(teruggedraaide),
   ge_no_e(teruggedraaid)],adv,[],[]).

a([ge_both(teruggedrongen)],adv,[],[]).

a([ge_both(teruggefloten)],adv,[],[]).

a([ge_e(teruggegane),
   ge_no_e(teruggegaan)],adv,[],[]).

a([ge_both(teruggegeven)],adv,[],[]).

a([ge_both(teruggegrepen)],adv,[],[]).

a([ge_e(teruggehaalde),
   ge_no_e(teruggehaald)],adv,[],[]).

a([ge_both(teruggehouden)],adv,[],[]).

a([ge_e(teruggekaatste),
   ge_no_e(teruggekaatst)],adv,[],[]).

a([ge_e(teruggekeerde),
   ge_no_e(teruggekeerd)],adv,[],[]).

a([ge_both(teruggekeken)],adv,[],[]).

a([ge_both(teruggekomen)],adv,[],[]).

a([ge_both(teruggekregen)],adv,[],[]).

a([ge_both(teruggelopen)],adv,[],[]).

a([ge_both(teruggenomen)],adv,[],[]).

a([ge_no_e(teruggepakt),
   ge_e(teruggepakte)],padv,[],[]).

a([ge_both(teruggereden)],adv,[],[]).

a([ge_both(teruggeroepen)],adv,[],[]).

a([ge_both(teruggeslagen)],adv,[],[]).

a([ge_e(teruggestuurde),
   ge_no_e(teruggestuurd)],adv,[],[]).

a([ge_both(teruggetreden)],nonadv,[],[]).

a([ge_both(teruggetrokken),
   er(teruggetrokkener),
   ere(teruggetrokkenere),
   st(teruggetrokkenst),
   ste(teruggetrokkenste)],padv,
  [pp(uit)],[]).

a([ge_both(teruggevallen)],adv,[],[]).

a([ge_e(teruggevoerde),
   ge_no_e(teruggevoerd)],adv,[],[]).

a([ge_both(teruggevonden)],adv,[],[]).

a([ge_both(teruggeweken)],adv,[],[]).

a([ge_both(teruggeworpen)],adv,[],[]).

a([ge_e(teruggezette),
   ge_no_e(teruggezet)],adv,[],[]).

a([e(terughoudende),
   er(terughoudender),
   ere(terughoudendere),
   no_e(terughoudend),
   st(terughoudendst),
   ste(terughoudendste)],adv,
  [pp(met)],[]).

a([pred(terzake)],adv,[],[]).

a([no_e(tevergeefs),
   e(tevergeefse)],adv,[],[]).

a([both(tevreden),
   er(tevredener),
   ere(tevredenere),
   st(tevredenst),
   ste(tevredenste)],padv,
  [object_sbar,
   er_pp_sbar(met),
   er_pp_vp(met),
   pp(met),
   pp(over)],[]).

a([ge_e(teweeggebrachte),
   ge_no_e(teweeggebracht)],adv,[],[]).

a([e(textiele),
   no_e(textiel)],nonadv,[],[]).

a([e(teatrale),
   e(theatrale),
   er(teatraler),
   er(theatraler),
   ere(teatralere),
   ere(theatralere),
   no_e(teatraal),
   no_e(theatraal),
   st(teatraalst),
   st(theatraalst),
   ste(teatraalste),
   ste(theatraalste)],adv,[],[]).

a([e(tematische),
   e(thematische),
   no_e(tematisch),
   no_e(thematisch)],adv,[],[]).

a([no_e(theologisch),
   e(theologische),
   e(teologische),
   no_e(teologisch)
   ],adv,[],[]).

a([e(theoretische),
   e(teoretische),
   er(teoretischer),
   er(theoretischer),
   ere(teoretischere),
   ere(theoretischere),
   no_e(theoretisch),
   no_e(teoretisch),
   st(teoretischt),
   st(theoretischt),
   ste(teoretischte),
   ste(theoretischte)],adv,[],[]).

a([e(terapeutische),
   e(therapeutische),
   no_e(terapeutisch),
   no_e(therapeutisch)],adv,[],[]).

a([e(thermische),no_e(thermisch)],nonadv,[],[]).

a([pred(tezelfdertijd)],adv,[],[]).

a([ge_no_e(thuisbezorgd),
   ge_e(thuisbezorgde)],padv,[],[]).

a([ge_both(thuisgebleven)],adv,[],[]).

a([ge_e(thuisgebrachte),
   ge_no_e(thuisgebracht)],adv,[],[]).

a([ge_both(thuisgekomen)],adv,[],[]).

a([e(thuisloze),
   no_e(thuisloos)],nonadv,[],[]).

a([pred([tien,tegen,één])],nonadv,[subject_sbar],[]).

a([e(tijdelijke),
   no_e(tijdelijk)],adv,[],[]).

a([e(tijdeloze),
   no_e(tijdeloos)],adv,[],[]).

a([e(tijdige),
   er(tijdiger),
   ere(tijdigere),
   no_e(tijdig),
   st(tijdigst),
   ste(tijdigste)],adv,[],[]).

a([er(tijdlozer),
   ere(tijdlozere),
   e(tijdloze),
   no_e(tijdloos)],adv,[],[]).

a([e(tijdrovende),
   er(tijdrovender),
   ere(tijdrovendere),
   no_e(tijdrovend),
   st(tijdrovendst),
   ste(tijdrovendste)],nonadv,[],[]).

a([both(timide),
   er(timider),
   ere(timidere)],adv,[],[]).

a([stof(tinnen)],nonadv,[],[]).

a([e(tirannieke),
   er(tirannieker),
   ere(tiranniekere),
   no_e(tiranniek),
   st(tiranniekst),
   ste(tiranniekste)],adv,[],[]).

a([pred([to,the,point])],adv,[],[]).

a([e(tochtige),
   er(tochtiger),
   ere(tochtigere),
   no_e(tochtig),
   st(tochtigst),
   ste(tochtigste)],nonadv,[],[]).

a([ge_e(toebedachte),
   ge_no_e(toebedacht)],nonadv,
  [so_np],[]).

a([ge_e(toebedeelde),
   ge_no_e(toebedeeld)],nonadv,
  [so_np],[]).

a([both(toebemeten)],nonadv,
  [so_np],[]).

a([ge_e(toebereide),
   ge_no_e(toebereid)],adv,[],[]).

a([e(toegankelijke),
   er(toegankelijker),
   ere(toegankelijkere),
   no_e(toegankelijk),
   st(toegankelijkst),
   ste(toegankelijkste)],adv,
  [pp(voor)],[]).

a([ge_e(toegebrachte),
   ge_no_e(toegebracht)],adv,
  [so_np],[]).

a([ge_e(toegedane),
   er(toegedaner),
   ere(toegedanere),
   ge_no_e(toegedaan),
   st(toegedaanst),
   ste(toegedaanste)],adv,
  [transitive],[]).

a([ge_e(toegedachte),
   ge_no_e(toegedacht)],adv,
  [so_np],[]).

a([ge_e(toegedekte),
   ge_no_e(toegedekt)],adv,
  [],[]).

a([ge_e(toegedeelde),
   ge_no_e(toegedeeld)],adv,
  [so_np],[]).

a([ge_e(toegedichte),
   ge_no_e(toegedicht)],adv,
  [so_np],[]).

a([ge_e(toegediende),
   ge_no_e(toegediend)],adv,
  [so_np],[]).

a([ge_both(toegedragen)],adv,[],[]).

a([e(toegeeflijke),
   er(toegeeflijker),
   ere(toegeeflijkere),
   no_e(toegeeflijk),
   st(toegeeflijkst),
   ste(toegeeflijkste)],padv,[],[]).

a([ge_e(toegefluisterde),
   ge_no_e(toegefluisterd)],adv,[],[]).

a([ge_e(toegegane),
   ge_no_e(toegegaan)],adv,[],[]).

a([ge_both(toegegeven)],adv,[],[]).

a([ge_e(toegegroeide),
   ge_no_e(toegegroeid)],adv,[],[]).

a([ge_e(toegejuichte),
   ge_no_e(toegejuicht)],adv,[],[]).

a([ge_e(toegekeerde),
   ge_no_e(toegekeerd)],adv,
  [so_np],[]).

a([ge_e(toegekende),
   ge_no_e(toegekend)],adv,
  [so_np],[]).

a([ge_both(toegeknepen)],adv,[],[]).

a([ge_both(toegekomen)],adv,[],[]).

a([ge_both(toegelachen)],adv,[],[]).

a([ge_both(toegelaten)],adv,[],[]).

a([ge_e(toegelegde),
   ge_no_e(toegelegd)],adv,[],[]).

a([ge_e(toegelichte),
   ge_no_e(toegelicht)],adv,[],[]).

a([ge_both(toegelopen)],adv,[],[]).

a([ge_both(toegemeten)],adv,
  [so_np],[]).

a([ge_both(toegenomen)],adv,[],[]).

a([ge_e(toegepaste),
   ge_no_e(toegepast)],adv,
  [pp(op)],[]).

a([ge_e(toegereikte),
   ge_no_e(toegereikt)],adv,
  [so_np],[]).

a([ge_e(toegerekende),
   ge_no_e(toegerekend)],adv,[],[]).

a([ge_both(toegeroepen)],adv,
  [so_np],[]).

a([ge_e(toegeruste),
   ge_no_e(toegerust)],adv,
  [pp(op),
   object_vp],[]).

a([ge_both(toegeschenen)],adv,[],[]).

a([ge_both(toegeschoten)],adv,[],[]).

a([ge_both(toegeschoven)],adv,[],[]).

a([ge_both(toegeschreven)],adv,
  [so_np],[]).

a([ge_both(toegeslagen)],adv,[],[]).

a([ge_both(toegesneden)],padv,
  [pp(op)],[]).

a([ge_e(toegesnelde),
   ge_no_e(toegesneld)],adv,[],[]).

a([ge_e(toegespeelde),
   ge_no_e(toegespeeld)],adv,
  [so_np],[]).

a([ge_e(toegespitste),
   er(toegespitster),
   ere(toegespitstere),
   ge_no_e(toegespitst)],adv,
  [pp(op)],[]).

a([ge_both(toegesproken)],adv,[],[]).

a([ge_e(toegestane),
   ge_no_e(toegestaan)],adv,[],[]).

a([ge_both(toegestoken)],adv,[],[]).

a([ge_e(toegestopte),
   ge_no_e(toegestopt)],adv,[],[]).

a([ge_e(toegestroomde),
   ge_no_e(toegestroomd)],adv,[],[]).

a([ge_e(toegestuurde),
   ge_no_e(toegestuurd)],adv,
  [so_np],[]).

a([ge_e(toegetakelde),
   ge_no_e(toegetakeld)],adv,[],[]).

a([ge_both(toegetreden)],adv,[],[]).

a([ge_both(toegetrokken)],adv,[],[]).

a([ge_both(toegevallen)],adv,
  [so_np],[]).

a([ge_e(toegevoegde),
   ge_no_e(toegevoegd)],adv,
  [so_pp(aan),
   so_np],[]).

a([ge_e(toegewenste),
   ge_no_e(toegewenst)],adv,
  [so_np],[]).

a([ge_both(toegewezen)],adv,
  [so_np],[]).

a([ge_e(toegewijde),
   ge_no_e(toegewijd)],adv,
  [so_pp(aan),
   so_np],[]).

a([ge_both(toegeworpen)],adv,
  [so_np],[]).

a([ge_e(toegezegde),
   ge_no_e(toegezegd)],adv,
  [so_np],[]).

a([ge_both(toegezonden)],adv,
  [so_np],[]).

a([ge_e(toegeëigende),
   ge_no_e(toegeëigend)],adv,[],[]).

a([e(toekomende),
   no_e(toekomend)],adv,
  [so_np],[]).

%% het toekomstig functioneren van de raad
a([e(toekomstige),
   no_e(toekomstig)],adv,[],[]).

a([e(toelaatbare),
   er(toelaatbaarder),
   ere(toelaatbaardere),
   no_e(toelaatbaar),
   st(toelaatbaarst),
   ste(toelaatbaarste)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([e(toenmalige),
   no_e(toenmalig)],nonadv,[],[]).

a([e(toepasbare),
   er(toepasbaarder),
   ere(toepasbaardere),
   no_e(toepasbaar),
   st(toepasbaarst),
   ste(toepasbaarste)],nonadv,[],[]).

a([e(toepasselijke),
   er(toepasselijker),
   ere(toepasselijkere),
   no_e(toepasselijk),
   st(toepasselijkst),
   ste(toepasselijkste)],adv,
  [pp(voor)],[]).

a([e(toereikende),
   er(toereikender),
   ere(toereikendere),
   no_e(toereikend),
   st(toereikendst),
   ste(toereikendste)],adv,
  [subject_vp,
   pp(voor)],[]).

a([e(toerekeningsvatbare),
   no_e(toerekeningsvatbaar)],padv,[],[on]).

a([e(toeristische),
   no_e(toeristisch)],adv,[],[]).

a([e(toeschietelijke),
   er(toeschietelijker),
   ere(toeschietelijkere),
   no_e(toeschietelijk),
   st(toeschietelijkst),
   ste(toeschietelijkste)],adv,[],[]).

a([e(toevallige),
   er(toevalliger),
   ere(toevalligere),
   no_e(toevallig),
   st(toevalligst),
   ste(toevalligste)],adv,
  [subject_sbar],[]).

a([ge_e(toevertrouwde),
   ge_no_e(toevertrouwd)],adv,
  [so_np],[]).

a([e(toezichthoudende),
   no_e(toezichthoudend)],nonadv,[],[]).

a([e(toffe),
   er(toffer),
   ere(toffere),
   no_e(tof),
   st(tofst),
   ste(tofste)],adv,[subject_sbar,
                     subject_vp],[]).

a([e(tolerante),
   er(toleranter),
   ere(tolerantere),
   no_e(tolerant),
   st(tolerantst),
   ste(tolerantste)],adv,[],[]).

a([e(tomeloze),
   er(tomelozer),
   ere(tomelozere),
   no_e(tomeloos),
   st(tomeloost),
   ste(tomelooste)],adv,[],[]).

a([e(toonaangevende),
   er(toonaangevender),
   ere(toonaangevendere),
   no_e(toonaangevend),
   st(toonaangevendst),
   ste(toonaangevendste)],nonadv,[],[]).

a([e(toonloze),
   er(toonlozer),
   ere(toonlozere),
   no_e(toonloos),
   st(toonloost),
   ste(toonlooste)],adv,[],[]).

a([both(topless),
   both('top-less')],padv,[],[]).

a([e(torenhoge),
   er(torenhoger),
   ere(torenhogere),
   no_e(torenhoog),
   st(torenhoogst),
   ste(torenhoogste)],adv,[],[]).

a([pred([total,loss])],padv,[],[]).

a([pred([tot,daar,aan,toe]),
   pred([tot,daaraan,toe])],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(totale),
   no_e(totaal)],adv,[],[]).

a([ge_both(totstandgekomen)],adv,[],[]).

a([e(totalitaire),
   er(totalitairder),
   ere(totalitairdere),
   no_e(totalitair),
   st(totalitairst),
   ste(totalitairste)],adv,[],[]).

a([e(trage),
   er(trager),
   ere(tragere),
   no_e(traag),
   st(traagst),
   ste(traagste)],adv,[],[]).

a([e(traditiegetrouwe),
   er(traditiegetrouwer),
   ere(traditiegetrouwere),
   no_e(traditiegetrouw),
   st(traditiegetrouwst),
   ste(traditiegetrouwste)],adv,[],[]).

a([e(traditionele),
   er(traditioneler),
   ere(traditionelere),
   no_e(traditioneel),
   st(traditioneelst),
   ste(traditioneelste)],adv,
  [subject_sbar],[]).

a([e(tragische),
   er(tragischer),
   ere(tragischere),
   no_e(tragisch),
   st(tragischt),
   ste(tragischte)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(transatlantische),
   no_e(transatlantisch),
   e('trans-Atlantische'),
   no_e('trans-Atlantisch')],nonadv,[],[]).

a([e(transcendente),
   er(transcendenter),
   ere(transcendentere),
   no_e(transcendent),
   st(transcendentst),
   ste(transcendentste)],nonadv,[],[]).

a([e(transfervrije),
   no_e(transfervrij)],padv,[],[]).

a([e(transgene),
   no_e(transgeen)],nonadv,[],[]).

a([e(transparante),
   er(transparanter),
   ere(transparantere),
   no_e(transparant),
   st(transparantst),
   ste(transparantste)],adv,[subject_sbar,
                             subject_vp],[]).

a([e(trapsgewijze),
   no_e(trapsgewijs)],adv,[],[]).

a([e(traumatische),
   no_e(traumatisch)],nonadv,[],[]).

a([e(treffende),
   er(treffender),
   ere(treffendere),
   no_e(treffend),
   st(treffendst),
   ste(treffendste)],adv,[],[]).

a([e(trefzekere),
   er(trefzekerder),
   ere(trefzekerdere),
   no_e(trefzeker),
   st(trefzekerst),
   ste(trefzekerste)],adv,[],[]).

a([both(trendy)],nonadv,[],[]).

a([e(treurige),
   er(treuriger),
   ere(treurigere),
   no_e(treurig),
   st(treurigst),
   ste(treurigste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(tribale),
   no_e(tribaal)],nonadv,[],[]).

a([e(trieste),
   er(triester),
   ere(triestere),
   no_e(triest)],padv,
  [subject_sbar,
   subject_vp],[diep,in]).

a([e('in-en-in-trieste'),
   no_e('in-en-in-triest')],padv,
  [subject_sbar,
   subject_vp],[]).

a([e(triestige),
   er(triestiger),
   ere(triestigere),
   no_e(triestig),
   st(triestigst),
   ste(triestigste)],adv,[],[]).

a([e(triomfantelijke),
   er(triomfantelijker),
   ere(triomfantelijkere),
   no_e(triomfantelijk),
   st(triomfantelijkst),
   ste(triomfantelijkste)],adv,[],[]).

a([both(tripartite),
   e(tripartiete),
   no_e(tripartiet)],nonadv,[],[]).

a([e(triviale),
   er(trivialer),
   ere(trivialere),
   no_e(triviaal),
   st(triviaalst),
   ste(triviaalste)],nonadv,[],[]).

a([e(troebele),
   er(troebeler),
   ere(troebelere),
   no_e(troebel),
   st(troebelst),
   ste(troebelste)],nonadv,[],[]).

a([e(troosteloze),
   er(troostelozer),
   ere(troostelozere),
   no_e(troosteloos),
   st(troosteloost),
   ste(troostelooste)],adv,[],[]).

a([end(troostend),
   ende(troostende)],padv,[],[]).

a([e(tropische),
   no_e(tropisch)],adv,[],[]).

a([e(trotse),
   er(trotser),
   ere(trotsere),
   no_e(trots),
   st(trotst),
   ste(trotste)],padv,
  [object_sbar,
   object_vp,
   er_pp_sbar(op),
   er_pp_vp(op),
   pp(op)],
  [i(aap,ape)]).

a([e(trouwe),
   er(trouwer),
   ere(trouwere),
   no_e(trouw),
   st(trouwst),
   ste(trouwste)],adv,
  [so_pp(aan),
   so_np],[s(hond)]).  % en niet hond-stro-uwe

a([e(trouwhartige),
   er(trouwhartiger),
   ere(trouwhartigere),
   no_e(trouwhartig),
   st(trouwhartigst),
   ste(trouwhartigste)],adv,[],[]).

a([pred(true)],padv,[pp(met)],[]).  % twitter: ik ben true met me babyy

a([pred(tuk)],padv,[pp(op),
		    er_pp_vp(op),
		    er_pp_sbar(op)],[]).

a([e(tumultueuze),
   no_e(tumultueus)],adv,[],[]).

a([e(turbulente),
   er(turbulenter),
   ere(turbulentere),
   no_e(turbulent),
   st(turbulentst),
   ste(turbulentste)],nonadv,[],[]).

a([pred(tureluurs)],padv,[],[]).

a([ge_both(tussengelegen)],nonadv,[],[]).

a([e(tussenliggende),
   no_e(tussenliggend)],nonadv,[],[]).

a([e(tussentijdse),
   no_e(tussentijds)],adv,[],[]).

a([e(tuttige),
   no_e(tuttig)],adv,[],[]).

a([both(tweeërlei)],nonadv,[],[]).

a([e(tweeslachtige),
   no_e(tweeslachtig)],nonadv,[],[]).

a([e(tweezijdige),
   no_e(tweezijdig)],padv,[],[]).

a([e(twijfelachtige),
   er(twijfelachtiger),
   ere(twijfelachtigere),
   no_e(twijfelachtig),
   st(twijfelachtigst),
   ste(twijfelachtigste)],adv,
  [subject_sbar],[]).

a([both(twintiger)],nonadv,[],[]).

a([ende(typerende),
   er(typerender),
   ere(typerendere),
   end(typerend),
   st(typerendst),
   ste(typerendste)],adv,
  [subject_sbar,
   transitive,
   pp(voor)],[]).

a([e(typische),
   er(typischer),
   ere(typischere),
   no_e(typisch),
   st(typischt),
   ste(typischte)],adv,
  [pp(voor),
   subject_sbar],[]).

a([pred(uit)],adv,[],[]).

a([pred([uit,den,boze])],nonadv,
  [subject_vp],[]).

a([pred([uit,elkaar]),pred(uitelkaar)],nonadv,[],[]).

a([pred([uit,mekaar])],nonadv,[],[]).

a([ge_e(uitbestede),
   ge_no_e(uitbesteed)],padv,[],[]).

a([ge_e(uitbetaalde),
   ge_no_e(uitbetaald)],adv,[],[]).

a([e(uitbundige),
   er(uitbundiger),
   ere(uitbundigere),
   no_e(uitbundig),
   st(uitbundigst),
   ste(uitbundigste)],adv,[],[]).

a([ende(uitdagende),
   er(uitdagender),
   ere(uitdagendere),
   end(uitdagend),
   st(uitdagendst),
   ste(uitdagendste)],adv,[],[]).

a([e(uitdrukkelijke),
   er(uitdrukkelijker),
   ere(uitdrukkelijkere),
   no_e(uitdrukkelijk),
   st(uitdrukkelijkst),
   ste(uitdrukkelijkste)],adv,[],[]).

a([e(uitdrukkingsloze),
   no_e(uitdrukkingsloos)],adv,[],[]).

a([ge_both(uiteengelopen)],adv,[],[]).

a([ge_both(uiteengereten)],padv,[],[]).

a([ge_e(uiteengespatte),
   ge_no_e(uiteengespat)],adv,[],[]).

a([ge_both(uiteengevallen)],adv,[],[]).

a([ge_e(uiteengezette),
   ge_no_e(uiteengezet)],adv,[],[]).

a([e(uiteindelijke),
   no_e(uiteindelijk)],adv,[],[]).

a([e(uiterlijke),
   no_e(uiterlijk)],adv,[],[]).

a([e(uiterste),
   no_e(uiterst)],adv,[],[]).

a([ende(uitgaande),
   no_e(uitgaand)],padv,
  [pp(van),
   er_pp_sbar(van)],[]).

a([ge_e(uitgeademde),
   ge_no_e(uitgeademd)],adv,[],[]).

a([ge_both(uitgebakken)],padv,[],[]).

a([ge_e(uitgebalanceerde),
   ge_no_e(uitgebalanceerd)],adv,[],[]).

a([ge_both(uitgebannen)],adv,[],[]).

a([ge_both(uitgebarsten)],adv,[],[]).

a([ge_e(uitgebeelde),
   ge_no_e(uitgebeeld)],adv,[],[]).

a([ge_e(uitgebeende),
   ge_no_e(uitgebeend)],adv,[],[]).

a([ge_both(uitgebeten)],padv,[],[]).

a([ge_both(uitgeblazen)],adv,[],[]).

a([ge_both(uitgebleven)],adv,[],[]).

a([ge_e(uitgebloeide),
   ge_no_e(uitgebloeid)],adv,[],[]).

a([ge_both(uitgeblonken)],adv,[],[]).

a([ge_e(uitgebluste),
   ge_no_e(uitgeblust)],adv,[],[]).

a([ge_e(uitgebouwde),
   ge_no_e(uitgebouwd)],adv,[],[]).

a([ge_e(uitgebraakte),
   ge_no_e(uitgebraakt)],adv,[],[]).

a([ge_e(uitgebrachte),
   ge_no_e(uitgebracht)],padv,
  [so_pp(aan),
   so_np],[]).

a([ge_e(uitgebrande),
   ge_no_e(uitgebrand)],adv,[],[]).

a([ge_e(uitgebreide),
   er(uitgebreider),
   ere(uitgebreidere),
   ge_no_e(uitgebreid),
   st(uitgebreidst),
   ste(uitgebreidste)],adv,
  [pp(met),
   pp(tot)],[]).

a([ge_e(uitgebroede),
   ge_no_e(uitgebroed)],adv,[],[]).

a([ge_both(uitgebroken)],adv,[],[]).

a([ge_e(uitgebuite),
   ge_no_e(uitgebuit)],adv,[],[]).

a([ge_e(uitgedaagde),
   ge_no_e(uitgedaagd)],adv,[],[]).

a([ge_e(uitgedane),
   ge_no_e(uitgedaan)],adv,[],[]).

a([ge_e(uitgedachte),
   ge_no_e(uitgedacht)],adv,[],[]).

a([ge_e(uitgedeelde),
   ge_no_e(uitgedeeld)],adv,[],[]).

a([ge_e(uitgediepte),
   ge_no_e(uitgediept)],adv,[],[]).

a([ge_e(uitgedijde),
   ge_no_e(uitgedijd)],adv,[],[]).

a([ge_e(uitgedokterde),
   ge_no_e(uitgedokterd)],padv,[],[]).

a([ge_e(uitgedoofde),
   ge_no_e(uitgedoofd)],adv,[],[]).

a([ge_e(uitgedoste),
   ge_no_e(uitgedost)],adv,[],[]).

a([ge_e(uitgedraaide),
   ge_no_e(uitgedraaid)],adv,[],[]).

a([ge_both(uitgedragen)],adv,[],[]).

a([ge_both(uitgedreven)],adv,[],[]).

a([ge_e(uitgedroogde),
   ge_no_e(uitgedroogd)],adv,[],[]).

a([ge_both(uitgedropen)],adv,[],[]).

a([ge_e(uitgedrukte),
   ge_no_e(uitgedrukt)],padv,[],[]).

a([ge_e(uitgedunde),
   ge_no_e(uitgedund)],adv,[],[]).

a([ge_both(uitgefloten)],padv,[],[]).

a([ge_e(uitgegane),
   ge_no_e(uitgegaan)],adv,[],[]).

a([ge_both(uitgegeven)],adv,[],[her]).

a([ge_both(uitgegleden)],adv,[],[]).

a([ge_e(uitgegooide),
   ge_no_e(uitgegooid)],adv,[],[]).

a([ge_both(uitgegraven)],adv,[],[]).

a([ge_e(uitgegroeide),
   ge_no_e(uitgegroeid)],adv,[],[]).

a([ge_e(uitgehaalde),
   ge_no_e(uitgehaald)],adv,[],[]).

a([ge_e(uitgehakte),
   ge_no_e(uitgehakt)],adv,[],[]).

a([ge_both(uitgehangen)],adv,[],[]).

a([ge_e(uitgeholde),
   ge_no_e(uitgehold)],adv,[],[]).

a([ge_e(uitgehongerde),
   er(uitgehongerder),
   ere(uitgehongerdere),
   ge_no_e(uitgehongerd),
   st(uitgehongerdst),
   ste(uitgehongerdste)],padv,[],[]).

a([ge_e(uitgehoorde),
   ge_no_e(uitgehoord)],adv,[],[]).

a([ge_both(uitgehouden)],adv,[],[]).

a([ge_both(uitgehouwen)],adv,[],[]).

a([ge_e(uitgehuilde),
   ge_no_e(uitgehuild)],adv,[],[]).

a([ge_e(uitgejouwde),
   ge_no_e(uitgejouwd)],padv,[],[]).

a([ge_e(uitgekauwde),
   ge_no_e(uitgekauwd)],padv,[],[]).

a([ge_e(uitgekeerde),
   ge_no_e(uitgekeerd)],adv,[],[]).

a([ge_both(uitgekeken)],adv,
  [pp(op),
   er_pp_vp(op),
   er_pp_sbar(op)],[]).

a([ge_e(uitgekiende),
   er(uitgekiender),
   ere(uitgekiendere),
   ge_no_e(uitgekiend),
   st(uitgekiendst),
   ste(uitgekiendste)],adv,[],[]).

a([ge_e(uitgeklaarde),
   ge_no_e(uitgeklaard)],padv,[],[]).

a([ge_e(uitgeklapte),
   ge_no_e(uitgeklapt)],padv,[],[]).

a([ge_e(uitgeklede),
   ge_no_e(uitgekleed)],adv,[],[]).

a([ge_e(uitgekletst),
   ge_no_e(uitgekletste)],padv,[],[]).

a([ge_both(uitgeknepen)],padv,[],[]).

a([ge_e(uitgeknipte),
   ge_no_e(uitgeknipt)],adv,[],[]).

a([ge_both(uitgekomen)],adv,[],[]).

a([e(uitgekookte),
   er(uitgekookter),
   ere(uitgekooktere),
   no_e(uitgekookt),
   st(uitgekooktst),
   ste(uitgekooktste)],nonadv,[],[]).

a([ge_e(uitgekotste),
   ge_no_e(uitgekotst)],padv,[],[]).

a([ge_both(uitgekozen)],adv,[],[]).

a([ge_e(uitgekraamde),
   ge_no_e(uitgekraamd)],adv,[],[]).

a([ge_e(uitgekristalliseerde),
   ge_no_e(uitgekristalliseerd),
   ge_e(uitgekristallizeerde),
   ge_no_e(uitgekristallizeerd)],adv,[],[]).

a([ge_both(uitgelachen)],adv,[],[]).

a([ge_both(uitgeladen)],adv,[],[]).

a([ge_both(uitgelaten),
   er(uitgelatener),
   ere(uitgelatenere),
   st(uitgelatenst),
   ste(uitgelatenste)],padv,[],[]).

a([ge_e(uitgeleefde),
   ge_no_e(uitgeleefd)],adv,[],[]).

a([ge_e(uitgeleende),
   ge_no_e(uitgeleend)],adv,[],[]).

%% omdat je noot uitgeleerd raakt
a([ge_e(uitgeleerde),
   ge_no_e(uitgeleerd)],adv,[],[]).

a([ge_e(uitgelegde),
   ge_no_e(uitgelegd)],adv,[],[]).

a([ge_e(uitgeleide),
   ge_no_e(uitgeleid)],adv,[],[]).

a([ge_e(uitgelekte),
   ge_no_e(uitgelekt)],adv,[],[]).

a([ge_e(uitgelengde),
   ge_no_e(uitgelengd)],adv,[],[]).

a([ge_e(uitgeleverde),
   ge_no_e(uitgeleverd)],adv,[],[]).

a([ge_both(uitgelezen),
   er(uitgelezener),
   ere(uitgelezenere),
   st(uitgelezenst),
   ste(uitgelezenste)],adv,[],[]).

a([ge_e(uitgelichte),
   ge_no_e(uitgelicht)],padv,[],[]).

a([ge_e(uitgelokte),
   ge_no_e(uitgelokt)],adv,[],[]).

a([ge_both(uitgelopen)],adv,[],[]).

a([ge_e(uitgelote),
   ge_no_e(uitgeloot)],padv,[],[]).

a([ge_e(uitgeluisterde),
   ge_no_e(uitgeluisterd)],padv,[],[]).

a([ge_e(uitgemaakte),
   ge_no_e(uitgemaakt)],adv,
  [pp(voor)],[]).

a([e(uitgemergelde),
   no_e(uitgemergeld)],padv,[],[]).

a([ge_both(uitgemeten)],adv,[],[]).

a([ge_both(uitgemolken)],adv,[],[]).

a([ge_e(uitgemonde),
   ge_no_e(uitgemond)],adv,[],[]).

a([ge_e(uitgemoorde),
   ge_no_e(uitgemoord)],adv,[],[]).

a([ge_e(uitgenodigde),
   ge_no_e(uitgenodigd)],adv,[],[]).

a([ge_both(uitgenomen)],adv,[],[]).

a([ge_e(uitgeoefende),
   ge_no_e(uitgeoefend)],adv,[],[]).

a([ge_e(uitgepakte),
   ge_no_e(uitgepakt)],adv,[],[]).

a([ge_e(uitgeperste),
   ge_no_e(uitgeperst)],adv,[],[]).

a([ge_e(uitgepoepte),
   ge_no_e(uitgepoept)],adv,[],[]).

a([ge_e(uitgeprate),
   ge_no_e(uitgepraat)],adv,
  [pp(over)],[]).

a([ge_e(uitgeprobeerde),
   ge_no_e(uitgeprobeerd)],adv,[],[]).

a([ge_e(uitgeprocedeerde),
   ge_no_e(uitgeprocedeerd)],nonadv,[],[]).

a([ge_e(uitgepuilde),
   ge_no_e(uitgepuild)],adv,[],[]).

a([ge_e(uitgeputte),
   er(uitgeputter),
   ere(uitgeputtere),
   ge_no_e(uitgeput),
   st(uitgeputst),
   ste(uitgeputste)],adv,[],[]).

a([ge_e(uitgeraasde),
   ge_no_e(uitgeraasd)],padv,[],[]).

a([ge_e(uitgerangeerde),
   ge_no_e(uitgerangeerd)],padv,[],[]).

a([ge_both(uitgereden)],adv,[],[]).

a([ge_e(uitgereikte),
   ge_no_e(uitgereikt)],adv,[],[]).

a([ge_e(uitgerekende),
   er(uitgerekender),
   ere(uitgerekendere),
   ge_no_e(uitgerekend),
   st(uitgerekendst),
   ste(uitgerekendste)],adv,[],[]).

a([ge_e(uitgerekte),
   ge_no_e(uitgerekt)],adv,[],[]).

a([ge_e(uitgerichte),
   ge_no_e(uitgericht)],adv,[],[]).

a([ge_e(uitgeroeide),
   ge_no_e(uitgeroeid)],adv,[],[]).

a([ge_both(uitgeroepen)],adv,[],[]).

a([ge_e(uitgerolde),
   ge_no_e(uitgerold)],adv,[],[]).

a([ge_e(uitgerukte),
   ge_no_e(uitgerukt)],adv,[],[]).

a([ge_e(uitgeruste),
   ge_no_e(uitgerust)],adv,
  [pp(met)],[]).

a([ge_e(uitgeschakelde),
   ge_no_e(uitgeschakeld)],adv,[],[]).

a([ge_both(uitgescheiden)],adv,[],[]).

a([ge_both(uitgescholden)],adv,[],[]).

a([ge_both(uitgeschoten)],adv,[],[]).

a([ge_e(uitgeschreeuwde),
   ge_no_e(uitgeschreeuwd)],adv,[],[]).

%% de kranten raakten er niet over uitgeschreven
a([ge_both(uitgeschreven)],adv,[pp(over)],[]).

a([ge_both(uitgeslagen)],adv,[],[]).

a([ge_both(uitgeslapen)],adv,[],[]).

a([ge_both(uitgesleten)],adv,[],[]).

a([ge_e(uitgesloofde),
   ge_no_e(uitgesloofd)],adv,[],[]).

a([ge_e(uitgesmeerde),
   ge_no_e(uitgesmeerd)],padv,[],[]).

a([ge_both(uitgesloten)],adv,
  [subject_sbar],[]).

a([ge_both(uitgesneden)],adv,[],[]).

a([ge_e(uitgespaarde),
   ge_no_e(uitgespaard)],adv,[],[]).

a([ge_e(uitgespeelde),
   ge_no_e(uitgespeeld)],adv,[],[]).

a([ge_both(uitgesponnen)],nonadv,[],[]).

a([ge_e(uitgespookte),
   ge_no_e(uitgespookt)],adv,[],[]).

a([ge_e(uitgespreide),
   ge_no_e(uitgespreid)],adv,[],[]).

a([ge_e(uitgesplitste),
   ge_no_e(uitgesplitst)],padv,
  [pp(naar)],[]).

a([ge_both(uitgesproken),
   ere(uitgesprokenere),
   er(uitgesprokener)],adv,
  [pp(bij),
   pp(door),
   pp(met)],[]).

a([ge_e(uitgespuwde),
   ge_no_e(uitgespuwd)],adv,[],[]).

a([ge_e(uitgestane),
   ge_no_e(uitgestaan)],adv,[],[]).

a([ge_e(uitgestalde),
   ge_no_e(uitgestald)],adv,[],[]).

a([ge_e(uitgestapte),
   ge_no_e(uitgestapt)],adv,[],[]).

a([ge_both(uitgestegen)],adv,[],[]).

a([ge_e(uitgestelde),
   ge_no_e(uitgesteld)],adv,[],[]).

a([ge_e(uitgestippelde),
   ge_no_e(uitgestippeld)],adv,[],[]).

a([ge_both(uitgestoken)],adv,[],[]).

a([ge_e(uitgestorte),
   ge_no_e(uitgestort)],adv,[],[]).

a([ge_both(uitgestorven)],adv,[],[]).

a([ge_both(uitgestoten)],adv,[],[]).

a([ge_e(uitgestraalde),
   ge_no_e(uitgestraald)],adv,[],[]).

a([ge_both(uitgestreken)],nonadv,[],[]).

a([ge_e(uitgestrekte),
   er(uitgestrekter),
   ere(uitgestrektere),
   ge_no_e(uitgestrekt),
   st(uitgestrektst),
   ste(uitgestrektste)],adv,[],[]).

a([ge_e(uitgestrooide),
   ge_no_e(uitgestrooid)],adv,[],[]).

a([ge_e(uitgestroomde),
   ge_no_e(uitgestroomd)],adv,[],[]).

a([ge_e(uitgestudeerde),
   ge_no_e(uitgestudeerd)],padv,[],[]).

a([ge_e(uitgestuurde),
   ge_no_e(uitgestuurd)],adv,[],[]).

a([ge_e(uitgeteerde),
   ge_no_e(uitgeteerd)],adv,[],[]).

a([ge_e(uitgetekende),
   ge_no_e(uitgetekend)],adv,[],[]).

a([e(uitgetelde),
   no_e(uitgeteld)],padv,[],[]).

a([ge_e(uitgeteste),
   ge_no_e(uitgetest)],adv,[],[]).

a([ge_both(uitgetreden)],adv,[],[]).

a([ge_both(uitgetrokken)],adv,[],[]).

a([ge_e(uitgevaardigde),
   ge_no_e(uitgevaardigd)],adv,[],[]).

a([ge_both(uitgevallen)],adv,[],[]).

a([ge_both(uitgevaren)],adv,[],[]).

a([ge_both(uitgevlogen)],adv,[],[]).

a([ge_both(uitgevochten)],adv,[],[]).

a([ge_e(uitgevoerde),
   ge_no_e(uitgevoerd)],adv,[],[]).

a([ge_both(uitgevonden)],adv,[],[]).

a([ge_both(uitgewaaierd)],adv,[],[]).

a([ge_both(uitgeweid)],adv,[],[]).

a([ge_both(uitgeweken)],adv,[],[]).

a([ge_e(uitgewerkte),
   ge_no_e(uitgewerkt)],adv,[],[]).

a([ge_both(uitgewezen)],adv,[],[]).

a([ge_e(uitgewisselde),
   ge_no_e(uitgewisseld)],adv,[],[]).

a([ge_e(uitgewiste),
   ge_no_e(uitgewist)],adv,[],[]).

a([e(uitgewoede),
   no_e(uigewoed)],padv,[],[]).

a([ge_both(uitgewogen)],adv,[],[]).

a([ge_e(uitgewoonde),
   ge_no_e(uitgewoond)],padv,[],[]).

a([ge_both(uitgeworpen)],adv,[],[]).

a([ge_both(uitgewreven)],padv,
  [transitive],[]).

a([ge_both(uitgewrongen)],padv,[],[]).

a([ge_e(uitgezaaide),
   ge_no_e(uitgezaaid)],adv,[],[]).

a([ge_e(uitgezakte),
   ge_no_e(uitgezakt)],padv,[],[]).

a([ge_e(uitgezette),
   ge_no_e(uitgezet)],adv,[],[]).

a([ge_both(uitgezeten)],adv,[],[]).

a([ge_both(uitgezien)],adv,[],[]).

a([ge_e(uitgezochte),
   er(uitgezochter),
   ere(uitgezochtere),
   ge_no_e(uitgezocht),
   st(uitgezochtst),
   ste(uitgezochtste)],adv,[],[]).

a([ge_both(uitgezogen)],adv,[],[]).

a([ge_both(uitgezonden)],adv,[],[]).

a([ge_e(uitgezonderde),
   ge_no_e(uitgezonderd)],adv,[transitive],[]).

a([e(uitheemse),
   no_e(uitheems)],adv,[],[]).

a([ge_e(uitonderhandelde),
   ge_no_e(uitonderhandeld)],nonadv,[],[]).

a([e(uitstekende),
   er(uitstekender),
   ere(uitstekendere),
   no_e(uitstekend),
   st(uitstekendst),
   ste(uitstekendste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(uitvergrote),
   ge_no_e(uitvergroot)],adv,[],[]).

a([ge_e(uitverkochte),
   ge_no_e(uitverkocht)],nonadv,[],[]).

a([ge_both(uitverkoren)],adv,
  [pp(tot)],[]).

a([ge_both(uitverkozen)],adv,[],[]).

a([e(uitvoerbare),
   er(uitvoerbaarder),
   ere(uitvoerbaardere),
   no_e(uitvoerbaar),
   st(uitvoerbaarst),
   ste(uitvoerbaarste)],nonadv,[subject_vp],[]).

a([e(uitvoerige),
   er(uitvoeriger),
   ere(uitvoerigere),
   no_e(uitvoerig),
   st(uitvoerigst),
   ste(uitvoerigste)],adv,[],[]).

a([e(uitwendige),
   no_e(uitwendig)],adv,[],[]).

a([e(uitwisselbare),
   no_e(uitwisselbaar)],adv,[],[]).

a([e(uitzichtloze),
   er(uitzichtlozer),
   ere(uitzichtlozere),
   no_e(uitzichtloos),
   st(uitzichtloost),
   ste(uitzichtlooste)],adv,[],[]).

a([e(uitzinnige),
   er(uitzinniger),
   ere(uitzinnigere),
   no_e(uitzinnig),
   st(uitzinnigst),
   ste(uitzinnigste)],adv,[],[]).

a([e(uitzonderlijke),
   er(uitzonderlijker),
   ere(uitzonderlijkere),
   no_e(uitzonderlijk),
   st(uitzonderlijkst),
   ste(uitzonderlijkste)],adv,
  [subject_sbar],[]).

a([e(ultieme),
   no_e(ultiem)],adv,[],[]). % adv: VL

a([e(ultraviolette),
   no_e(ultraviolet)],nonadv,[],[]).

a([e(unanieme),
   no_e(unaniem)],adv,
  [pp(in),
   pp(over),
   pp(voor)],[]).

a([both(undercover),
   both('under-cover'),
   both([under,cover])],adv,[],[]).

a([e(unieke),
   er(unieker),
   ere(uniekere),
   no_e(uniek),
   st(uniekst),
   ste(uniekste)],adv,
  [subject_sbar],[]).

a([e(uniforme),
   er(uniformer),
   ere(uniformere),
   no_e(uniform),
   st(uniformst),
   ste(uniformste)],nonadv,[],[]).

a([e(unilaterale),
   no_e(unilateraal)],adv,[],[]).

a([er(universeler),
   ere(universelere),
   e(universele),
   no_e(universeel)],adv,[],[]).

a([e(universitaire),
   no_e(universitair)],adv,[],[]).

a([both([up,to,date]),
   both('up-to-date')],adv,[],[]).

a([both(urban)],nonadv,[],[]).

a([e(urbane),
   er(urbaner),
   ere(urbanere),
   no_e(urbaan),
   st(urbaanst),
   ste(urbaanste)],nonadv,[],[]).

a([e(urgente),
   er(urgenter),
   ere(urgentere),
   no_e(urgent),
   st(urgentst),
   ste(urgentste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(utopische),
   er(utopischer),
   ere(utopischere),
   no_e(utopisch),
   st(utopischt),
   ste(utopischte)],adv,[],[]).

a([e(utopistische),
   er(utopistischer),
   ere(utopistischere),
   no_e(utopistisch),
   st(utopistischt),
   ste(utopistischte)],adv,[],[]).

a([e(vage),
   er(vager),
   ere(vagere),
   no_e(vaag),
   st(vaagst),
   ste(vaagste)],adv,[],[]).

a([e(vake),
   er(vaker),
   ere(vakere),
   no_e(vaak),
   st(vaakst),
   ste(vaakste)],adv,[],[]).

a([e(vale),
   er(valer),
   ere(valere),
   no_e(vaal),
   st(vaalst),
   ste(vaalste)],nonadv,[],[]).

a([e(vare),
   no_e(vaar)],nonadv,[],[]).

a([e(vaardige),
   er(vaardiger),
   ere(vaardigere),
   no_e(vaardig),
   st(vaardigst),
   ste(vaardigste)],adv,
  [pp(in)],[besluit]).

a([e(vacante),
   no_e(vacant)],nonadv,
  [pp(bij)],[]).

a([both(vacuüm)],nonadv,[],[]).

a([e(vaderlandse),
   no_e(vaderlands)],nonadv,[],[]).

a([e(vaderlijke),
   er(vaderlijker),
   ere(vaderlijkere),
   no_e(vaderlijk),
   st(vaderlijkst),
   ste(vaderlijkste)],adv,[],[]).

a([e(vadsige),
   er(vadsiger),
   ere(vadsigere),
   no_e(vadsig),
   st(vadsigst),
   ste(vadsigste)],adv,[],[]).

a([no_e(vagelijk),
   e(vagelijke)],adv,[],[]).

a([e(vakkundige),
   er(vakkundiger),
   ere(vakkundigere),
   no_e(vakkundig),
   st(vakkundigst),
   ste(vakkundigste)],adv,[],[]).

a([e(vakspecifieke),
   no_e(vakspecifiek)],adv,[],[]).

a([both(valide)],adv,[],[]).

a([e(valse),
   er(valser),
   ere(valsere),
   no_e(vals),
   st(valst),
   ste(valste)],adv,[],[]).

a([pred([van,de,gekke])],nonadv,
  [subject_sbar],[]).

a([pred([van,de,wijs])],padv,[],[]).

a([pred([van,de,zotte])],nonadv,
  [subject_sbar],[]).

a([pred([van,dien,aard])],nonadv,[],[]).

a([pred([van,dik,hout,zaagt,men,planken])],nonadv,[],[]).

a([pred([van,kwaad,naar,erger])],nonadv,[],[]).

a([pred([van,kwaad,tot,erger])],nonadv,[],[]).

a([pred([van,hetzelfde,laken,een,pak])],nonadv,[],[]).

a([pred([van,kracht])],nonadv,[],[]).

a([pred([van,streek])],padv,[],[]).

a([pred([van,toepassing])],nonadv,
  [pp(op)],[]).

a([pred([van,tel])],nonadv,[],[]).

a([pred([van,zins])],padv,[object_vp],[]).

a([e(vanzelfsprekende),
   no_e(vanzelfsprekend)],adv,
  [subject_sbar,
   pp(voor)],[]).

a([e(variabele),
   er(variabeler),
   ere(variabelere),
   no_e(variabel),
   st(variabelst),
   ste(variabelste)],nonadv,[],[]).

a([e(vaste),
   er(vaster),
   ere(vastere),
   no_e(vast)],adv,[],
  [honk,
   klem,
   merk,
   rente]).

a([both(vastberaden),
   ere(vastberadenere),
   er(vastberadener)],adv,
  [pp(over),
   object_sbar,
   object_vp],[]).

a([both(vastbesloten),
   ere(vastbeslotenere),
   er(vastbeslotener)],padv,
  [object_sbar,
   object_vp],[]).

a([ge_both(vastgebeten)],adv,[],[]).

a([ge_both(vastgebonden)],adv,[],[]).

a([ge_both(vastgegrepen)],adv,[],[]).

a([ge_both(vastgehouden)],adv,[],[]).

a([ge_e(vastgeklampte),
   ge_no_e(vastgeklampt)],adv,[],[]).

a([ge_e(vastgeklemde),
   ge_no_e(vastgeklemd)],adv,[],[]).

a([ge_both(vastgeklonken)],padv,[],[]).

a([ge_e(vastgeknoopte),
   ge_no_e(vastgeknoopt)],adv,[],[]).

a([ge_e(vastgelegde),
   ge_no_e(vastgelegd)],adv,[],[]).

a([ge_both(vastgelegen)],adv,[],[]).

a([ge_both(vastgelopen)],adv,[],[]).

a([ge_e(vastgemaakte),
   ge_no_e(vastgemaakt)],adv,[],[]).

a([ge_e(vastgenagelde),
   ge_no_e(vastgenageld)],adv,[],[]).

a([ge_e(vastgepakte),
   ge_no_e(vastgepakt)],adv,[],[]).

a([ge_e(vastgeplakte),
   ge_no_e(vastgeplakt)],adv,[],[]).

a([ge_e(vastgeroeste),
   ge_no_e(vastgeroest)],adv,[],[]).

a([ge_e(vastgestelde),
   ge_no_e(vastgesteld)],adv,[],[]).

a([ge_both(vastgevroren)],adv,[],[]).

a([ge_e(vastgezette),
   ge_no_e(vastgezet)],adv,[],[]).

a([ge_both(vastgezeten)],adv,[],[]).

a([e(vasthoudende),
   er(vasthoudender),
   ere(vasthoudendere),
   no_e(vasthoudend),
   st(vasthoudendst),
   ste(vasthoudendste)],adv,[],[]).

a([e(vastomlijnde),
   no_e(vastomlijnd)],nonadv,[],[]).

a([both(vastomschreven)],padv,[],[]).

a([e(vastrentende),
   no_e(vastrentend)],padv,[],[]).

a([e(vatbare),
   er(vatbaarder),
   ere(vatbaardere),
   no_e(vatbaar),
   st(vatbaarst),
   ste(vatbaarste)],nonadv,
  [pp(voor)],[]).

a([e(vederlichte),
   no_e(vederlicht)],adv,[],[]).

a([e(vege),
   er(veger),
   ere(vegere),
   no_e(veeg),
   st(veegst),
   ste(veegste)],adv,[],[]).

a([no_e(veel),e(vele)],odet_adv,[],[kei]).

a([e(veelbelovende),
   no_e(veelbelovend)],adv,[],[]).

a([both(veelbesproken)],nonadv,[],[]).

a([e(veelbetekenende),
   no_e(veelbetekenend)],adv,[subject_sbar],[]).

a([e(veeleisende),
   no_e(veeleisend),
   er(veeleisender),
   ere(veeleisendere)],nonadv,
  [pp(voor)],[]).

a([ge_e(veelgehoorde),
   no_ge_e(veelgehoord)],padv,[],[]).

a([ge_both(veelgeprezen)],nonadv,[],[]).

a([ge_both(veelgelezen)],nonadv,[],[]).

a([er(veelkleuriger),
   ere(veelkleurigere),
   e(veelkleurige),
   no_e(veelkleurig)],nonadv,[],[]).

a([e(veelomvattende),
   no_e(veelomvattend)],nonadv,[],[]).

a([e(veelsoortige),
   no_e(veelsoortig)],nonadv,[],[]).

a([e(veelvoudige),
   no_e(veelvoudig)],adv,[],[]).

a([er(veelvuldiger),
   ere(veelvuldigere),
   e(veelvuldige),
   no_e(veelvuldig)],adv,
  [pp(in)],[]).

a([e(veelzeggende),
   no_e(veelzeggend)],padv,
  [subject_sbar],[]).

a([er(veelzijdiger),
   ere(veelzijdigere),
   e(veelzijdige),
   no_e(veelzijdig)],adv,[],[]).

a([e(veerkrachtige),
   er(veerkrachtiger),
   ere(veerkrachtigere),
   no_e(veerkrachtig),
   st(veerkrachtigst),
   ste(veerkrachtigste)],padv,[],[]).

a([both(veertiger)],nonadv,[],[]).

a([e(vegetarische),
   no_e(vegetarisch)],adv,[],[]).

a([e(vegetatieve),
   no_e(vegetatief)],adv,[],[]).

a([e(veilige),
   er(veiliger),
   ere(veiligere),
   no_e(veilig),
   st(veiligst),
   ste(veiligste)],adv,
  [subject_vp,
   pp(voor)],[]).

a([both(velerlei)],nonadv,[],[]).

a([e(venijnige),
   er(venijniger),
   ere(venijnigere),
   no_e(venijnig),
   st(venijnigst),
   ste(venijnigste)],adv,[],[]).

a([e(verre),
   er(verder),
   ere(verdere),
   no_e(ver),
   st(verst),
   ste(verste)],locadv,
  [pp(van)],[i(mijl,mijlen)]).

a([pred([ver,heen]),
   pred_er([verder,heen])],padv,[],[]).

% a([pred([ver,weg])],locadv,[],[]).

a([pred(veraf)],locadv,[],[]).

a([pred(verderop)],locadv,[],[]).

a([ge_e(verachte),
   er(verachter),
   ere(verachtere),
   ge_no_e(veracht),
   st(verachtst),
   ste(verachtste)],adv,[],[]).

a([e(verachtelijke),
   er(verachtelijker),
   ere(verachtelijkere),
   no_e(verachtelijk),
   st(verachtelijkst),
   ste(verachtelijkste)],adv,[],[]).

a([ge_e(verafschuwde),
   ge_no_e(verafschuwd)],adv,[],[]).

a([ge_e(veranderde),
   ge_no_e(veranderd)],adv,[],[]).

a([e(veranderlijke),
   er(veranderlijker),
   ere(veranderlijkere),
   no_e(veranderlijk),
   st(veranderlijkst),
   ste(veranderlijkste)],nonadv,[],[]).

a([ge_e(verankerde),
   ge_no_e(verankerd)],adv,[],[]).

a([ge_e(verantwoorde),
   er(verantwoorder),
   ere(verantwoordere),
   ge_no_e(verantwoord),
   st(verantwoordst),
   ste(verantwoordste)],adv,
  [subject_vp],[]).

a([e(verantwoordelijke),
   er(verantwoordelijker),
   ere(verantwoordelijkere),
   postn_no_e(verantwoordelijk),
   st(verantwoordelijkst),
   ste(verantwoordelijkste)],both,
  [er_pp_sbar(voor),
   er_pp_vp(voor),
   pp(voor)],[]).

a([ge_e(verarmde),
   ge_no_e(verarmd)],adv,[],[]).

a([e(verbale),
   no_e(verbaal)],adv,[],[]).

a([ge_e(verbaasde),
   er(verbaasder),
   ere(verbaasdere),
   ge_no_e(verbaasd),
   st(verbaasdst),
   ste(verbaasdste)],padv,
  [object_sbar,
   object_vp,
   er_pp_vp(over),
   er_pp_sbar(over),
   pp(over)],[]).

a([ge_both(verbannen)],adv,[],[]).

a([ge_e(verbasterde),
   ge_no_e(verbasterd)],adv,[],[]).

a([ende(verbazende),
   er(verbazender),
   ere(verbazendere),
   end(verbazend),
   st(verbazendst),
   ste(verbazendste)],adv,
  [subject_sbar,
   subject_vp,
   refl],[]).

a([e(verbazingwekkende),
   er(verbazingwekkender),
   ere(verbazingwekkendere),
   no_e(verbazingwekkend),
   st(verbazingwekkendst),
   ste(verbazingwekkendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(verbeelde),
   ge_no_e(verbeeld)],adv,[],[]).

a([e(verbeide),
   no_e(verbeid)],adv,[],[]).

a([ge_both(verbeten),
   er(verbetener),
   ere(verbetenere),
   st(verbetenst),
   ste(verbetenste)],adv,[],[]).

a([ge_e(verbeterde),
   ge_no_e(verbeterd)],adv,[],[]).

a([ge_e(verbijsterde),
   er(verbijsterder),
   ere(verbijsterdere),
   ge_no_e(verbijsterd),
   st(verbijsterdst),
   ste(verbijsterdste)],padv,
  [er_pp_sbar(over),
   object_sbar,
   pp(over)],[]).

a([e(verbijsterende),
   er(verbijsterender),
   ere(verbijsterendere),
   no_e(verbijsterend),
   st(verbijsterendst),
   ste(verbijsterendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_e(verbitterde),
   er(verbitterder),
   ere(verbitterdere),
   ge_no_e(verbitterd),
   st(verbitterdst),
   ste(verbitterdste)],adv,
  [er_pp_sbar(over),
   object_sbar,
   pp(over)],[]).

a([ge_e(verbleekte),
   ge_no_e(verbleekt)],adv,[],[]).

a([ge_both(verbleven)],adv,[],[]).

a([ge_e(verblijde),
   ge_no_e(verblijd)],adv,[],[]).

a([ge_e(verblinde),
   er(verblinder),
   ere(verblindere),
   ge_no_e(verblind),
   st(verblindst),
   ste(verblindste)],adv,
  [pp(door)],[]).

a([e(verbloemde),
   er(verbloemder),
   ere(verbloemdere),
   no_e(verbloemd),
   st(verbloemdst),
   ste(verbloemdste)],adv,[],[]).

a([e(verbluffende),
   er(verbluffender),
   ere(verbluffendere),
   no_e(verbluffend),
   st(verbluffendst),
   ste(verbluffendste)],adv,
  [subject_vp],[]).

a([ge_e(verblufte),
   er(verblufter),
   ere(verbluftere),
   ge_no_e(verbluft),
   st(verbluftst),
   ste(verbluftste)],adv,
  [pp(door),
   pp(over)],[]).

a([ge_both(verboden)],adv,
  [subject_vp],[]).

a([ge_both(verbogen)],adv,[],[]).

a([both(verbolgen),
   er(verbolgener),
   ere(verbolgenere),
   st(verbolgenst),
   ste(verbolgenste)],padv,
  [pp(over),
   object_sbar,
   er_pp_sbar(over)],[]).

a([ge_both(verbonden)],adv,
  [pp(aan),
   pp(met)],[]).

a([ge_both(verborgen),
   er(verborgener),
   ere(verborgenere),
   st(verborgenst),
   ste(verborgenste)],adv,[],[]).

a([ge_e(verbouwde),
   ge_no_e(verbouwd)],adv,[],[]).

a([e(verbouwereerde),
   er(verbouwereerder),
   ere(verbouwereerdere),
   no_e(verbouwereerd),
   st(verbouwereerdst),
   ste(verbouwereerdste)],padv,[],[]).

a([ge_e(verbrande),
   ge_no_e(verbrand)],adv,[],[]).

a([ge_e(verbrede),
   ge_no_e(verbreed)],adv,[],[]).

a([ge_e(verbreide),
   er(verbreider),
   ere(verbreidere),
   ge_no_e(verbreid),
   st(verbreidst),
   ste(verbreidste)],adv,[],[]).

a([ge_e(verbrijzelde),
   ge_no_e(verbrijzeld)],adv,[],[]).

a([ge_e(verbroederde),
   ge_no_e(verbroederd)],padv,[],[]).

a([ge_both(verbroken)],adv,[],[]).

a([ge_e(verbrokkelde),
   ge_no_e(verbrokkeld)],adv,[],[]).

a([ge_e(verbruikte),
   ge_no_e(verbruikt)],adv,[],[]).

a([ge_e(verchroomde),
   ge_no_e(verchroomd)],adv,[],[]).

a([ge_e(verdane),
   ge_no_e(verdaan)],adv,[],[]).

a([ge_e(verdachte),
   er(verdachter),
   ere(verdachtere),
   ge_no_e(verdacht),
   st(verdachtst),
   ste(verdachtste)],padv,
  [pp(op),
   er_pp_sbar(op),
   er_pp_vp(op),
   pp(van),
   subject_sbar],[]).

a([ge_e(verdampte),
   ge_no_e(verdampt)],adv,[],[]).

a([e(verdedigbare),
   er(verdedigbaarder),
   ere(verdedigbaardere),
   no_e(verdedigbaar),
   st(verdedigbaarst),
   ste(verdedigbaarste)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(verdedigde),
   ge_no_e(verdedigd)],adv,[],[]).

a([ge_e(verdeelde),
   ge_no_e(verdeeld)],adv,
  [pp(in),
   pp(over)],[]).

a([e(verdekte),
   no_e(verdekt)],padv,[],[]).

a([ge_e(verdelgde),
   ge_no_e(verdelgd)],adv,[],[]).

a([e(verderfelijke),
   er(verderfelijker),
   ere(verderfelijkere),
   no_e(verderfelijk),
   st(verderfelijkst),
   ste(verderfelijkste)],nonadv,[],[]).

a([e(verdergaande),
   no_e(verdergaand)],nonadv,[],[]).

a([ge_e(verdichte),
   ge_no_e(verdicht)],adv,[],[]).

a([ge_e(verdiende),
   ge_no_e(verdiend)],adv,
  [pp(in)],[laatst]).

a([e(verdienstelijke),
   er(verdienstelijker),
   ere(verdienstelijkere),
   no_e(verdienstelijk),
   st(verdienstelijkst),
   ste(verdienstelijkste)],adv,
  [pp(voor)],[on]).

a([ge_e(verdiepte),
   ge_no_e(verdiept)],adv,[],[]).

a([ge_e(verdikte),
   ge_no_e(verdikt)],adv,[],[]).

a([ge_e(verdisconteerde),
   ge_no_e(verdisconteerd)],adv,[],[]).

a([ge_e(verdiskonteerde),
   ge_no_e(verdiskonteerd)],adv,[],[]).

a([ge_e(verdoemde),
   er(verdoemder),
   ere(verdoemdere),
   ge_no_e(verdoemd),
   st(verdoemdst),
   ste(verdoemdste)],adv,[],[]).

a([ge_e(verdoezelde),
   ge_no_e(verdoezeld)],adv,[],[]).

a([ge_both(verdoken)],padv,[],[]).

a([ge_e(verdomde),
   er(verdomder),
   ere(verdomdere),
   ge_no_e(verdomd),
   st(verdomdst),
   ste(verdomdste)],adv,[],[]).

a([ge_e(verdonkerde),
   ge_no_e(verdonkerd)],adv,[],[]).

a([ge_e(verdonkeremaande),
   ge_no_e(verdonkeremaand)],adv,[],[]).

a([ge_e(verdoofde),
   ge_no_e(verdoofd)],adv,[],[]).

a([ge_e(verdorde),
   er(verdorder),
   ere(verdordere),
   ge_no_e(verdord),
   st(verdordst),
   ste(verdordste)],adv,[],[]).

a([ge_both(verdorven),
   er(verdorvener),
   ere(verdorvenere),
   st(verdorvenst),
   ste(verdorvenste)],adv,[],[]).

a([e(verdraagzame),
   er(verdraagzamer),
   ere(verdraagzamere),
   no_e(verdraagzaam),
   st(verdraagzaamst),
   ste(verdraagzaamste)],padv,[],[]).

a([ge_e(verdraaide),
   er(verdraaider),
   ere(verdraaidere),
   ge_no_e(verdraaid),
   st(verdraaidst),
   ste(verdraaidste)],adv,[],[]).

a([ge_both(verdragen)],adv,[],[]).

a([ende(verdragsluitende),
   end(verdragsluitend)],nonadv,[],[]).

a([ge_both(verdreven)],adv,[],[]).

a([e(verdrietige),
   er(verdrietiger),
   ere(verdrietigere),
   no_e(verdrietig),
   st(verdrietigst),
   ste(verdrietigste)],padv,
  [subject_sbar],[]).

a([ge_both(verdrongen)],adv,[],[]).

a([ge_both(verdronken)],adv,
  [pp(in)],[]).

a([ge_e(verdroogde),
   ge_no_e(verdroogd)],adv,[],[]).

a([ge_e(verdrukte),
   ge_no_e(verdrukt)],adv,[],[]).

a([ge_e(verdubbelde),
   ge_no_e(verdubbeld)],adv,[],[]).

a([ge_e(verduidelijkte),
   ge_no_e(verduidelijkt)],adv,[],[]).

a([ge_e(verduisterde),
   ge_no_e(verduisterd)],adv,[],[]).

a([e(verduivelde),
   er(verduivelder),
   ere(verduiveldere),
   no_e(verduiveld),
   st(verduiveldst),
   ste(verduiveldste)],adv,[],[]).

a([ge_e(verdunde),
   ge_no_e(verdund)],adv,[],[]).

a([ge_e(verduurde),
   ge_no_e(verduurd)],adv,[],[]).

a([ge_e(verduurdzaamde),
   ge_no_e(verduurzaamd)],adv,[],[]).

a([ge_e(verdwaalde),
   er(verdwaalder),
   ere(verdwaaldere),
   ge_no_e(verdwaald),
   st(verdwaaldst),
   ste(verdwaaldste)],adv,[],[]).

a([ge_e(verdwaasde),
   er(verdwaasder),
   ere(verdwaasdere),
   ge_no_e(verdwaasd),
   st(verdwaasdst),
   ste(verdwaasdste)],adv,[],[]).

a([ge_both(verdwenen)],adv,[],[]).

a([ge_e(veredelde),
   ge_no_e(veredeld)],adv,[],[]).

a([ge_e(vereende),
   ge_no_e(vereend)],adv,[],[]).

a([ge_e(vereenvoudigde),
   ge_no_e(vereenvoudigd)],adv,[],[]).

a([ge_e(vereenzaamde),
   ge_no_e(vereenzaamd)],adv,[],[]).

a([ge_e(vereenzelvigde),
   ge_no_e(vereenzelvigd)],adv,[],[]).

a([ge_e(vereerde),
   ge_no_e(vereerd)],adv,
  [object_sbar,
   object_vp],[]).

a([ge_e(vereeuwigde),
   ge_no_e(vereeuwigd)],adv,[],[]).

a([ge_e(vereffende),
   ge_no_e(vereffend)],adv,[],[]).

a([ge_e(vereiste),
   ge_no_e(vereist)],adv,[],[]).

%% veren shuttles
a([stof(veren)],nonadv,[],[]).

a([e(verenigbare),
   no_e(verenigbaar)],nonadv,
  [pp(met)],[]).

a([ge_e(verenigde),
   ge_both(verenigd)],adv,[],[]).

a([ge_e(verergerde),
   ge_no_e(verergerd)],adv,[],[]).

a([ge_e(verfijnde),
   ge_no_e(verfijnd),
   ere(verfijndere),
   er(verfijnder)],adv,[],[]).

a([ge_e(verfilmde),
   ge_no_e(verfilmd)],adv,[],[]).

a([ge_e(verflauwde),
   ge_no_e(verflauwd)],adv,[],[]).

a([ge_e(verfoeide),
   ge_no_e(verfoeid)],adv,[],[]).

a([ge_e(verfomfaaide),
   ge_no_e(verfomfaaid)],adv,[],[]).

a([ge_e(verfraaide),
   ge_no_e(verfraaid)],adv,[],[]).

a([e(verfrissende),
   er(verfrissender),
   ere(verfrissendere),
   no_e(verfrissend),
   st(verfrissendst),
   ste(verfrissendste)],adv,
  [subject_vp,
   subject_sbar
  ],[]).

a([ge_e(verfriste),
   ge_no_e(verfrist)],adv,[],[]).

a([ge_e(verfrommelde),
   ge_no_e(verfrommeld)],adv,[],[]).

a([ge_e(vergane),
   ge_no_e(vergaan)],adv,[],[]).

a([ge_e(vergaapte),
   ge_no_e(vergaapt)],adv,[],[]).

a([ge_e(vergaarde),
   ge_no_e(vergaard)],adv,[],[]).

a([ge_e(vergaderde),
   ge_no_e(vergaderd)],adv,[],[]).

a([ge_e(vergalde),
   ge_no_e(vergald)],adv,[],[]).

a([e(vergankelijke),
   er(vergankelijker),
   ere(vergankelijkere),
   no_e(vergankelijk),
   st(vergankelijkst),
   ste(vergankelijkste)],nonadv,[],[]).

a([ge_e(vergaste),
   ge_no_e(vergast)],adv,[],[]).

a([e(vergeefse),
   no_e(vergeefs)],adv,[],[]).

a([ge_e(vergeelde),
   ge_no_e(vergeeld)],adv,[],[]).

a([ge_both(vergeleken)],adv,
  [pp(met)],[]).

a([e(vergelijkbare),
   postn_no_e(vergelijkbaar)],adv,
  [pp(met)],[on]).

a([ge_e(vergemakkelijkte),
   ge_no_e(vergemakkelijkt)],adv,[],[]).

a([ge_e(vergenoegde),
   er(vergenoegder),
   ere(vergenoegdere),
   ge_no_e(vergenoegd),
   st(vergenoegdst),
   ste(vergenoegdste)],adv,[],[]).

a([ge_both(vergeten)],adv,[],[]).

a([ge_both(vergeven)],adv,
  [pp(van)],[]).  % heel Kenia is vergeven van de wapens

a([e(vergevorderde),
   no_e(vergevorderd)],nonadv,[],[]).

a([ge_e(vergewiste),
   ge_no_e(vergewist)],adv,[],[]).

a([ge_e(vergezelde),
   ge_no_e(vergezeld)],padv,
  [pp(van)],[]).

a([ge_no_e(vergezocht),
   ge_e(vergezochte)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([e(vergiftige),
   no_e(vergiftig)],adv,[],[]).  % in europarl!?

a([ge_e(vergiftigde),
   ge_no_e(vergiftigd)],adv,[],[]).

a([ge_e(vergiste),
   ge_no_e(vergist)],adv,[],[]).

a([ge_both(vergleden)],adv,[],[]).

a([ge_e(vergoede),
   ge_no_e(vergoed)],adv,[],[]).

a([ge_e(vergoelijkte),
   ge_no_e(vergoelijkt)],adv,[],[]).

a([ge_e(vergokte),
   ge_no_e(vergokt)],adv,[],[]).

a([ge_e(vergooide),
   ge_no_e(vergooid)],adv,[],[]).

a([ge_both(vergoten)],adv,[],[]).

a([ge_e(vergrendelde),
   ge_no_e(vergrendeld)],adv,[],[]).

a([ge_both(vergrepen)],adv,[],[]).

a([ge_e(vergrijsde),
   ge_no_e(vergrijsd)],adv,[],[]).

a([ge_e(vergroeide),
   ge_no_e(vergroeid)],adv,[],[]).

a([ge_e(vergrote),
   ge_no_e(vergroot)],adv,[],[]).

a([ge_e(verguisde),
   ge_no_e(verguisd)],adv,[],[]).

a([ge_e(vergulde),
   ge_no_e(verguld)],adv,
  [er_pp_sbar(met),
   pp(met)],[]).

a([ge_e(vergunde),
   ge_no_e(vergund)],adv,[],[]).

a([ge_e(verhaalde),
   ge_no_e(verhaald)],adv,[],[]).

a([ge_e(verhaaste),
   ge_no_e(verhaast)],adv,[],[]).

a([ge_e(verhakkelde),
   ge_no_e(verhakkeld)],padv,[],[]).

a([e(verhandelbare),
   no_e(verhandelbaar)],adv,[],[]).

a([ge_e(verhandelde),
   ge_no_e(verhandeld)],adv,[],[]).

a([ge_both(verhangen)],adv,[],[]).

a([ge_e(verharde),
   ge_no_e(verhard)],adv,
  [pp(met)],[]).

a([ge_e(verheelde),
   ge_no_e(verheeld)],adv,[],[]).

a([ge_e(verheerlijkte),
   ge_no_e(verheerlijkt)],adv,[],[]).

a([ge_e(verhelderde),
   ge_no_e(verhelderd)],adv,[],[]).

a([ge_e(verheugde),
   er(verheugder),
   ere(verheugdere),
   ge_no_e(verheugd),
   st(verheugdst),
   ste(verheugdste)],adv,
  [object_sbar,
   object_vp,
   er_pp_sbar(over),
   er_pp_vp(over),
   pp(met),
   pp(over)],[]).

a([ende(verheugende),
   er(verheugender),
   ere(verheugendere),
   end(verheugend),
   st(verheugendst),
   ste(verheugendste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ge_both(verheven),
   er(verhevener),
   ere(verhevenere),
   st(verhevenst),
   ste(verhevenste)],adv,
  [pp(tot)],[]).

a([ge_e(verhevigde),
   ge_no_e(verhevigd)],adv,[],[]).

a([ge_e(verhinderde),
   ge_no_e(verhinderd)],adv,[],[]).

a([ge_e(verhitte),
   er(verhitter),
   ere(verhittere),
   ge_no_e(verhit),
   st(verhitst),
   ste(verhitste)],adv,[],[]).

a([both(verholen)],padv,[],[]).

a([ge_both(verholpen)],adv,[],[]).

a([ge_e(verhongerde),
   ge_no_e(verhongerd)],padv,[],[]).

a([ge_e(verhoogde),
   er(verhoogder),
   ere(verhoogdere),
   ge_no_e(verhoogd),
   st(verhoogdst),
   ste(verhoogdste)],adv,
  [pp(met)],[]).

a([ge_e(verhoopte),
   ge_no_e(verhoopt)],adv,[],[]).

a([ge_e(verhoorde),
   ge_no_e(verhoord)],adv,[],[]).

a([ge_both(verhouden)],adv,[],[]).

a([no_e(verhoudingsgewijs),
   e(verhoudingsgewijze)],adv,[],[]).

a([ge_e(verhoute),
   ge_no_e(verhout)],nonadv,[],[]).

a([ge_e(verhuisde),
   ge_no_e(verhuisd)],adv,[],[]).

a([ge_e(verhulde),
   ge_no_e(verhuld)],adv,[],[]).

a([ge_e(verhuurde),
   ge_no_e(verhuurd)],adv,[],[]).

a([ge_e(verijdelde),
   ge_no_e(verijdeld)],adv,[],[]).

a([ge_e(verjaagde),
   ge_no_e(verjaagd)],adv,[],[]).

a([ge_e(verjaarde),
   ge_no_e(verjaard)],nonadv,[],[]).

a([ge_e(verjongde),
   ge_no_e(verjongd)],nonadv,[],[]).

a([ge_e(verkalkte),
   ge_no_e(verkalkt)],nonadv,[],[]).

a([e(verkapte),
   no_e(verkapt)],adv,[],[]).

a([pred([verkeerd,om])],adv,[],[]).

a([e(verkeerde),
   er(verkeerder),
   ere(verkeerdere),
   no_e(verkeerd),
   st(verkeerdst),
   ste(verkeerdste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_both(verkeken)],adv,[],[]).

a([ge_e(verkende),
   ge_no_e(verkend)],adv,[],[]).

a([ge_e(verketterde),
   ge_no_e(verketterd)],padv,[],[]).

a([e(verkiesbare),
   no_e(verkiesbaar)],nonadv,[],[]).

a([e(verkieselijke),
   no_e(verkieselijk),
   er(verkieselijker),
   ere(verkieselijker),
   st(verkieselijkst),
   ste(verkieselijkste)],nonadv,[],[]).

a([e(verkieslijke),
   no_e(verkieslijk),
   er(verkieslijker),
   ere(verkieslijkere),
   st(verkieslijkst),
   ste(verkieslijkste)],nonadv,[],[]).

a([ge_e(verkilde),
   ge_no_e(verkild)],adv,[],[]).

a([e(verklaarbare),
   er(verklaarbaarder),
   ere(verklaarbaardere),
   no_e(verklaarbaar),
   st(verklaarbaarst),
   ste(verklaarbaarste)],nonadv,
  [subject_sbar],[]).

a([ge_e(verklaarde),
   ge_no_e(verklaard)],adv,
  [pred],[]).

a([ge_e(verklapte),
   ge_no_e(verklapt)],adv,[],[]).

a([ge_e(verklede),
   ge_no_e(verkleed)],adv,[],[]).

a([ge_e(verkleinde),
   ge_no_e(verkleind)],adv,[],[]).

a([ge_e(verkleumde),
   er(verkleumder),
   ere(verkleumdere),
   ge_no_e(verkleumd),
   st(verkleumdst),
   ste(verkleumdste)],adv,[],[]).

a([ge_e(verkleurde),
   ge_no_e(verkleurd)],adv,[],[]).

a([ge_both(verklonken)],nonadv,[],[]).

a([e(verknipte),
   no_e(verknipt)],padv,[],[]).

a([e(verknochte),
   no_e(verknocht)],nonadv,
  [er_pp_vp(aan),
   pp(aan)],[]).

a([ge_e(verknoeide),
   ge_no_e(verknoeid)],adv,[],[]).

a([ge_e(verkochte),
   ge_no_e(verkocht)],adv,[],[]).

a([ge_e(verkokerde),
   ge_no_e(verkokerd)],adv,[],[]).

a([ge_e(verkommerde),
   ge_no_e(verkommerd)],adv,[],[]).

a([ge_e(verkondigde),
   ge_no_e(verkondigd)],adv,[],[]).

a([ge_e(verkoolde),
   ge_no_e(verkoold)],adv,[],[]).

a([ge_both(verkoren)],adv,[],[]).

a([ge_e(verkorte),
   ge_no_e(verkort)],adv,[],[]).

a([both(verkouden)],padv,[],[]).

a([ge_both(verkozen)],adv,[],[]).

a([ge_e(verkrachte),
   ge_no_e(verkracht)],adv,[],[]).

a([ge_e(verkrampte),
   er(verkrampter),
   ere(verkramptere),
   ge_no_e(verkrampt),
   st(verkramptst),
   ste(verkramptste)],both,[],[]).

a([ge_both(verkregen)],adv,[],[]).

a([ge_e(verkreukelde),
   ge_no_e(verkreukeld)],adv,[],[]).

a([e(verkrijgbare),
   postn_no_e(verkrijgbaar)],nonadv,
  [pp(bij)],[]).

a([ge_e(verkropte),
   ge_no_e(verkropt)],adv,[],[]).

a([ge_e(verkruimelde),
   ge_no_e(verkruimeld)],padv,[],[]).

a([ge_e(verkwikte),
   ge_no_e(verkwikt)],adv,[],[]).

a([ge_e(verkwiste),
   ge_no_e(verkwist)],adv,[],[]).

a([ge_e(verlaagde),
   ge_no_e(verlaagd)],adv,[],[]).

a([ge_e(verlate),
   ge_no_e(verlaat)],adv,[],[]).

a([ge_e(verlamde),
   ge_no_e(verlamd)],adv,[],[]).

a([ge_e(verlangde),
   ge_no_e(verlangd)],adv,[],[]).

a([ge_both(verlaten),
   er(verlatener),
   ere(verlatenere),
   st(verlatenst),
   ste(verlatenste)],adv,
  [pp(door)],[]).

a([ge_both(verleden)],nonadv,[],[]).

a([ge_e(verleende),
   ge_no_e(verleend)],adv,
  [so_np],[]).

a([ge_e(verleerde),
   ge_no_e(verleerd)],adv,[],[]).

a([ge_e(verlegde),
   ge_no_e(verlegd)],adv,[],[]).

a([ge_both(verlegen),
   er(verlegener),
   ere(verlegenere),
   st(verlegenst),
   ste(verlegenste)],padv,
  [pp(met),
   pp(om)],[]).

a([ge_e(verleide),
   ge_no_e(verleid)],adv,[],[]).

a([e(verleidelijke),
   er(verleidelijker),
   ere(verleidelijkere),
   no_e(verleidelijk),
   st(verleidelijkst),
   ste(verleidelijkste)],adv,
  [subject_vp],[]).

a([e(verlekkerde),
   no_e(verlekkerd)],padv,[],[]).

a([ge_e(verlengde),
   ge_no_e(verlengd)],adv,[],[]).

a([e(verlepte),
   no_e(verlept)],padv,[],[]).

a([ge_e(verlevendigde),
   ge_no_e(verlevendigd)],adv,[],[]).

a([ge_e(verlichte),
   er(verlichter),
   ere(verlichtere),
   ge_no_e(verlicht),
   st(verlichtst),
   ste(verlichtste)],padv,[],[]).

a([e(verliefde),
   er(verliefder),
   ere(verliefdere),
   no_e(verliefd),
   st(verliefdst),
   ste(verliefdste)],adv,
  [pp(op)],[]).

a([e(verliesgevende),
   no_e(verliesgevend)],nonadv,[],[]).

a([e(verlieslijdende),
   no_e(verlieslijdend)],adv,[],[]).

a([e(verlievende),
   no_e(verlievend)],adv,[],[]).

a([ge_e(verloederde),
   ge_no_e(verloederd)],nonadv,[],[]).

a([ge_e(verlokte),
   ge_no_e(verlokt)],adv,[],[]).

a([ge_e(verloochende),
   ge_no_e(verloochend)],adv,[],[]).

a([ge_e(verloofde),
   ge_no_e(verloofd)],adv,
  [pp(met)],[]).

a([ge_both(verlopen)],adv,[],[]).

a([ge_both(verloren)],padv,[],[]).

a([e(verloskundige),
   no_e(verloskundig)],adv,[],[]).

a([ge_e(verloste),
   ge_no_e(verlost)],adv,[],[]).

a([ge_e(verluchtigde),
   ge_no_e(verluchtigd)],padv,[],[]).

a([ge_e(verluide),
   ge_no_e(verluid)],adv,[],[]).

a([ge_e(verlustigde),
   ge_no_e(verlustigd)],adv,[],[]).

a([ge_e(vermaakte),
   ge_no_e(vermaakt)],adv,[],[]).

a([ge_e(vermaande),
   ge_no_e(vermaand)],adv,[],[]).

a([e(vermaarde),
   er(vermaarder),
   ere(vermaardere),
   no_e(vermaard),
   st(vermaardst),
   ste(vermaardste)],nonadv,
  [er_pp_sbar(om),
   er_pp_vp(om),
   pp(om)],[]).

a([ge_e(vermagerde),
   ge_no_e(vermagerd)],adv,[],[]).

a([e(vermakelijke),
   er(vermakelijker),
   ere(vermakelijkere),
   no_e(vermakelijk),
   st(vermakelijkst),
   ste(vermakelijkste)],adv,[],[]).

a([e(vermaledijde),
   no_e(vermaledijd)],nonadv,[],[]).

a([ge_both(vermalen)],adv,[],[]).

a([ge_e(vermande),
   ge_no_e(vermand)],adv,[],[]).

a([ge_both(vermeden)],adv,[],[]).

a([e(vermeende),
   no_e(vermeend)],adv,[],[]).

a([ge_e(vermeerderde),
   ge_no_e(vermeerderd)],adv,[],[]).

a([ge_e(vermelde),
   ge_no_e(vermeld)],adv,[],[]).

a([e(vermeldingswaarde),
   no_e(vermeldingswaard)],adv,[object_sbar],[]).

a([e(vermeldingswaardige),
   no_e(vermeldingswaardig)],adv,[object_sbar],[]).

a([ge_e(vermengde),
   ge_no_e(vermengd)],adv,[],[]).

a([ge_e(vermenigvuldigde),
   ge_no_e(vermenigvuldigd)],adv,[],[]).

a([e(vermetele),
   er(vermeteler),
   ere(vermetelere),
   no_e(vermetel),
   st(vermetelst),
   ste(vermetelste)],adv,[],[]).

a([stof(vermiljoenen)],nonadv,[],[]).

a([ge_e(verminderde),
   ge_no_e(verminderd)],adv,[],[]).

a([ge_e(verminkte),
   ge_no_e(verminkt)],padv,[],[]).

a([ge_e(vermiste),
   ge_no_e(vermist)],adv,[],[]).

a([ge_e(vermochte),
   ge_no_e(vermocht)],adv,[],[]).

a([ge_e(vermoede),
   ge_no_e(vermoed)],adv,[],[]).

a([e(vermoedelijke),
   no_e(vermoedelijk)],adv,[],[]).

a([ge_e(vermoeide),
   er(vermoeider),
   ere(vermoeidere),
   ge_no_e(vermoeid),
   st(vermoeidst),
   ste(vermoeidste)],both,[],[]).

a([e(vermoeiende),
   er(vermoeiender),
   ere(vermoeiendere),
   no_e(vermoeiend),
   st(vermoeiendst),
   ste(vermoeiendste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([e(vermolmde),
   no_e(vermolmd)],padv,[],[]).

a([ge_e(vermomde),
   ge_no_e(vermomd)],adv,[],[]).

a([ge_e(vermoorde),
   ge_no_e(vermoord)],adv,[],[]).

a([ge_e(vermorzelde),
   ge_no_e(vermorzeld)],adv,[],[]).

a([ge_e(vermurwde),
   ge_no_e(vermurwd)],adv,[],[]).

a([ge_e(vernauwde),
   ge_no_e(vernauwd)],adv,[],[]).

a([ge_e(vernederde),
   ge_no_e(vernederd)],adv,[],[]).

a([ende(vernederende),
   er(vernederender),
   ere(vernederendere),
   end(vernederend),
   st(vernederendst),
   ste(vernederendste)],adv,[],[subject_sbar,
				subject_vp]).

a([ge_e(vernederlandste),
   ge_no_e(vernederlandst)],padv,[],[]).

a([e(verneukeratieve),
   no_e(verneukeratief)],nonadv,[subject_sbar,
                                 subject_vp],[]).

a([ge_e(verneukte),
   ge_no_e(verneukt)],padv,[],[]).

a([ge_e(vernielde),
   ge_no_e(vernield)],adv,[],[]).

a([ge_e(vernietigde),
   ge_no_e(vernietigd)],adv,[],[]).

a([e(vernietigende),
   er(vernietigender),
   ere(vernietigendere),
   no_e(vernietigend),
   st(vernietigendst),
   ste(vernietigendste)],adv,[],[alles]).

a([ge_e(vernieuwde),
   ge_no_e(vernieuwd)],adv,[],[]).

a([ge_e(vernoemde),
   ge_no_e(vernoemd)],adv,[],[]).

a([ge_both(vernomen)],adv,[],[]).

a([e(vernuftige),
   er(vernuftiger),
   ere(vernuftigere),
   no_e(vernuftig),
   st(vernuftigst),
   ste(vernuftigste)],adv,[],[]).

a([ge_e(veronachtzaamde),
   ge_no_e(veronachtzaamd)],adv,[],[]).

a([ge_e(veronderstelde),
   ge_no_e(verondersteld)],adv,
  [pred],[]).

a([ende(veronderstellende),
   end(veronderstellend)],padv,
  [pred_np],[]).

a([e(verongelijkende),
   no_e(verongelijkend)],adv,[],[]).

a([e(verongelijkte),
   no_e(verongelijkt)],adv,[],[]).

a([ge_e(verongelukte),
   ge_no_e(verongelukt)],adv,[],[]).

a([ge_e(verontreinigde),
   ge_no_e(verontreinigd)],adv,[],[]).

a([ge_e(verontruste),
   er(verontruster),
   ere(verontrustere),
   ge_no_e(verontrust)],adv,
  [pp(over),
   subject_sbar],[]).

a([e(verontrustende),
   er(verontrustender),
   ere(verontrustendere),
   no_e(verontrustend),
   st(verontrustendst),
   ste(verontrustendste)],adv,
  [subject_sbar],[]).

a([ge_e(verontschuldigde),
   ge_no_e(verontschuldigd)],adv,[],[]).

a([ge_e(verontwaardigde),
   er(verontwaardigder),
   ere(verontwaardigdere),
   ge_no_e(verontwaardigd),
   st(verontwaardigdst),
   ste(verontwaardigdste)],adv,
  [pp(over),
   object_sbar],[]).

a([ge_e(veroordeelde),
   ge_no_e(veroordeeld)],adv,
  [fixed([[ter,dood]])],[]).

a([ge_e(veroorloofde),
   ge_no_e(veroorloofd)],adv,[],[]).

a([ge_e(veroorzaakte),
   ge_no_e(veroorzaakt)],adv,[],[]).

a([ge_e(verorberde),
   ge_no_e(verorberd)],adv,[],[]).

a([ge_e(verordonneerde),
   ge_no_e(verordonneerd)],nonadv,[],[]).

a([ge_e(verouderde),
   er(verouderder),
   ere(verouderdere),
   ge_no_e(verouderd),
   st(verouderdst),
   ste(verouderdste)],adv,[],[]).

a([ge_e(veroverde),
   ge_no_e(veroverd)],adv,[],[]).

a([ge_e(verpakte),
   ge_no_e(verpakt)],adv,[],[]).

a([ge_e(verpande),
   ge_no_e(verpand)],adv,[],[]).

a([ge_e(verpauperde),
   ge_no_e(verpauperd)],padv,[],[]).

a([ge_e(verpeste),
   ge_no_e(verpest)],adv,[],[]).

a([ge_e(verplaatste),
   ge_no_e(verplaatst)],adv,[],[]).

a([ge_e(verpleegde),
   ge_no_e(verpleegd)],adv,[],[]).

a([e(verpleegkundige),
   no_e(verpleegkundig)],adv,[],[wijk]).

a([ge_e(verpletterde),
   ge_no_e(verpletterd)],adv,[],[]).

a([e(verpletterende),
   er(verpletterender),
   ere(verpletterendere),
   no_e(verpletterend),
   st(verpletterendst),
   ste(verpletterendste)],adv,[],[]).

a([ge_e(verplichte),
   ge_no_e(verplicht)],adv,
  [subject_vp,
   object_vp,
   er_pp_vp(tot),
   pp(tot),
   so_np,
   np_np],[]).

a([ge_e(verpotte),
   ge_no_e(verpot)],adv,[],[]).

a([ge_e(verpulverde),
   ge_no_e(verpulverd)],adv,[],[]).

a([ge_both(verraden)],adv,[],[]).

a([e(verraderlijke),
   er(verraderlijker),
   ere(verraderlijkere),
   no_e(verraderlijk),
   st(verraderlijkst),
   ste(verraderlijkste)],adv,[],[]).

a([ende(verrassende),
   er(verrassender),
   ere(verrassendere),
   end(verrassend),
   st(verrassendst),
   ste(verrassendste)],adv,
  [subject_sbar],[]).

a([ge_e(verraste),
   ge_no_e(verrast)],adv,
  [object_sbar,
   er_pp_sbar(door),
   er_pp_sbar(over),
   pp(door),
   pp(met),
   pp(over)],[]).

a([ge_both(verreden)],padv,[],[]).

a([e(verregaande),
   no_e(verregaand)],adv,[],[]).

a([ge_e(verregende),
   ge_no_e(verregend)],padv,[],[]).

a([ge_e(verrekende),
   ge_no_e(verrekend)],adv,[],[]).

a([ge_e(verrekte),
   er(verrekter),
   ere(verrektere),
   ge_no_e(verrekt),
   st(verrektst),
   ste(verrektste)],adv,[],[]).

a([ge_both(verrezen)],adv,[],[]).

a([ge_e(verrichte),
   ge_no_e(verricht)],adv,[],[]).

a([ge_e(verrijkte),
   ge_no_e(verrijkt)],adv,[],[]).

a([ge_e(verroerde),
   ge_no_e(verroerd)],adv,[],[]).

a([ge_e(verroeste),
   ge_no_e(verroest)],adv,[],[]).

a([ge_e(verrotte),
   er(verrotter),
   ere(verrottere),
   ge_no_e(verrot),
   st(verrotst),
   ste(verrotste)],adv,[],[]).

a([ge_e(verruimde),
   ge_no_e(verruimd)],adv,[],[]).

a([e(verrukkelijke),
   er(verrukkelijker),
   ere(verrukkelijkere),
   no_e(verrukkelijk),
   st(verrukkelijkst),
   ste(verrukkelijkste)],adv,[],[]).

a([ge_e(verrukte),
   er(verrukter),
   ere(verruktere),
   ge_no_e(verrukt),
   st(verruktst),
   ste(verruktste)],adv,
  [pp(over)],[]).

a([e(verse),
   er(verser),
   ere(versere),
   no_e(vers),
   st(verst),
   ste(verste)],adv,[],[]).

a([ge_e(verschaalde),
   ge_no_e(verschaald)],adv,[],[]).

a([ge_e(verschafte),
   ge_no_e(verschaft)],adv,[],[]).

a([ge_e(verschalkte),
   ge_no_e(verschalkt)],adv,[],[]).

a([ge_e(verschanste),
   ge_no_e(verschanst)],adv,[],[]).

a([ge_e(verscheepte),
   ge_no_e(verscheept)],adv,[],[]).

a([e(verscheidene),
   no_e(verscheiden)],nonadv,[],[]).

a([ge_both(verschenen)],adv,[],[]).

a([ge_e(verscherpte),
   ge_no_e(verscherpt)],adv,[],[]).

a([ge_e(verscheurde),
   ge_no_e(verscheurd)],adv,[],[]).

a([ge_both(verscholen)],adv,[],[]).

a([ge_e(verschoonde),
   ge_no_e(verschoond)],adv,[],[]).

a([ge_both(verschoten)],adv,[],[]).

a([ge_both(verschoven)],adv,[],[]).

a([ge_e(verschraalde),
   ge_no_e(verschraald)],adv,[],[]).

a([e(verschrikkelijke),
   er(verschrikkelijker),
   ere(verschrikkelijkere),
   no_e(verschrikkelijk),
   st(verschrikkelijkst),
   ste(verschrikkelijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([ge_e(verschrikte),
   ge_no_e(verschrikt)],adv,[],[]).

a([ge_e(verschroeide),
   ge_no_e(verschroeid)],adv,[],[]).

a([ge_both(verschrokken)],adv,[],[]).

a([ge_e(verschrompelde),
   ge_no_e(verschrompeld)],adv,[],[]).

a([e(verschuifbare),
   no_e(verschuifbaar)],nonadv,[],[]).

a([ge_e(verschuldigde),
   ge_no_e(verschuldigd)],nonadv,
  [so_pp(aan),
   np_np,
   pp(voor),
   pp(op),
   pp(over),
   object_vp,
   so_np],[]).

a([both(versgemalen)],padv,[],[]).

a([ge_e(versierde),
   ge_no_e(versierd)],adv,[],[]).

a([ge_e(versimpelde),
   ge_no_e(versimpeld)],padv,[],[]).

a([ge_e(verslaafde),
   er(verslaafder),
   ere(verslaafdere),
   ge_no_e(verslaafd),
   st(verslaafdst),
   ste(verslaafdste)],adv,
  [er_pp_vp(aan),
   pp(aan)],[alcolhol,
	     drugs,
	     game,
	     gok,
	     nicotine,
	     sex]).

a([ge_both(verslagen),
   er(verslagener),
   ere(verslagenere),
   st(verslagenst),
   ste(verslagenste)],adv,
  [pp(door),
   pp(met)],[]).

a([ge_e(verslapte),
   ge_no_e(verslapt)],adv,[],[]).

a([ende(verslavende),
   end(verslavend)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(verslechterde),
   ge_no_e(verslechterd)],adv,[],[]).

a([ge_both(versleten),
   er(versletener),
   ere(versletenere),
   st(versletenst),
   ste(versletenste)],adv,[],[]).

a([ge_e(versleutelde),
   ge_no_e(versleuteld)],padv,[],[]).

a([ge_e(verslikte),
   ge_no_e(verslikt)],adv,[],[]).

a([e(verslingerde),
   no_e(verslingerd)],padv,
  [pp(aan)],[]).

a([ge_both(verslonden)],adv,[],[]).

a([ge_e(verslonsde),
   ge_no_e(verslonsd)],padv,[],[]).

a([ge_e(versluierde),
   ge_no_e(versluierd)],adv,[],[]).

a([ge_e(versmade),
   ge_no_e(versmaad)],adv,[],[]).

a([ge_e(versmalde),
   ge_no_e(versmald)],adv,[],[]).

a([ge_both(versmolten)],adv,[],[]).

a([ge_both(versneden)],adv,[],[]).

a([ge_e(versnelde),
   ge_no_e(versneld)],adv,[],[]).

a([ge_e(versnipperde),
   ge_no_e(versnipperd)],adv,[],[]).

a([ge_e(versoberde),
   ge_no_e(versoberd)],padv,[],[]).

a([ge_e(versoepelde),
   ge_no_e(versoepeld)],nonadv,[],[]).

a([ge_e(verspeelde),
   ge_no_e(verspeeld)],adv,[],[]).

a([ge_e(versperde),
   ge_no_e(versperd)],adv,[],[]).

a([ge_e(verspilde),
   ge_no_e(verspild)],adv,[],[]).

a([ge_e(versplinterde),
   ge_no_e(versplinterd)],adv,[],[]).

a([ge_e(verspreide),
   er(verspreider),
   ere(verspreidere),
   ge_no_e(verspreid),
   st(verspreidst),
   ste(verspreidste)],adv,
  [pp(over)],[]).

a([ge_both(versprongen)],adv,[],[]).

a([e(verste),
   no_e(verst)],adv,[],[]).

a([ge_e(verstane),
   ge_no_e(verstaan)],adv,[],[]).

a([e(verstaanbare),
   er(verstaanbaarder),
   ere(verstaanbaardere),
   no_e(verstaanbaar),
   st(verstaanbaarst),
   ste(verstaanbaarste)],adv,[],[]).

a([e(verstandelijke),
   er(verstandelijker),
   ere(verstandelijkere),
   no_e(verstandelijk),
   st(verstandelijkst),
   ste(verstandelijkste)],adv,[],[]).

a([e(verstandige),
   er(verstandiger),
   ere(verstandigere),
   no_e(verstandig),
   st(verstandigst),
   ste(verstandigste)],adv,
  [subject_vp,
   subject_sbar],[]).

a([ge_e(verstarde),
   ge_no_e(verstard)],adv,[],[]).

a([ge_e(verstedelijkte),
   ge_no_e(verstedelijkt)],nonadv,[],[]).

a([ge_e(versteende),
   ge_no_e(versteend)],adv,[],[]).

a([e(verstelbare),
   no_e(verstelbaar)],nonadv,[],[]).

a([ge_e(verstelde),
   ge_no_e(versteld)],adv,
  [pp(van),
   object_sbar,
   er_pp_sbar(van)],[]).

a([e(versterkende),
   er(versterkender),
   ere(versterkendere),
   no_e(versterkend),
   st(versterkendst),
   ste(versterkendste)],adv,
  [transitive],[spier]).

a([ge_e(versterkte),
   ge_no_e(versterkt)],adv,[],[]).

a([ge_e(verstevigde),
   ge_no_e(verstevigd)],adv,[],[]).

a([ge_e(verstijfde),
   ge_no_e(verstijfd)],adv,[],[]).

a([ge_e(verstikte),
   ge_no_e(verstikt)],adv,[],[]).

a([ge_e(verstilde),
   ge_no_e(verstild)],adv,[],[]).

a([ge_e(verstofte),
   ge_no_e(verstoft)],nonadv,[],[]).

a([e(verstokte),
   no_e(verstokt)],nonadv,[],[]).

a([ge_e(verstomde),
   ge_no_e(verstomd)],adv,[],[]).

a([ge_e(verstookte),
   ge_no_e(verstookt)],adv,[],[]).

a([ge_e(verstoorde),
   er(verstoorder),
   ere(verstoordere),
   ge_no_e(verstoord),
   st(verstoordst),
   ste(verstoordste)],adv,
  [pp(door)],[]).

a([ge_e(verstopte),
   ge_no_e(verstopt)],adv,[],[]).

a([ge_both(verstoten)],adv,[],[]).

a([ge_e(verstrakte),
   ge_no_e(verstrakt)],adv,[],[]).

a([ge_both(verstreken)],adv,[],[]).

a([e(verstrekkende),
   er(verstrekkender),
   ere(verstrekkendere),
   no_e(verstrekkend),
   st(verstrekkendst),
   ste(verstrekkendste)],adv,[],[]).

a([ge_e(verstrekte),
   ge_no_e(verstrekt)],adv,
  [so_np],[]).

a([ge_e(verstrengde),
   ge_no_e(verstrengd)],adv,[],[]).

a([ge_e(verstrengelde),
   ge_no_e(verstrengeld)],adv,[],[]).

a([ge_e(verstrikte),
   ge_no_e(verstrikt)],adv,[],[]).

a([ge_e(verstrooide),
   er(verstrooider),
   ere(verstrooidere),
   ge_no_e(verstrooid),
   st(verstrooidst),
   ste(verstrooidste)],adv,[],[]).

a([ge_e(verstuikte),
   ge_no_e(verstuikt)],nonadv,[],[]).

a([ge_e(verstuurde),
   ge_no_e(verstuurd)],adv,[],[]).

a([ge_e(versufte),
   er(versufter),
   ere(versuftere),
   ge_no_e(versuft),
   st(versuftst),
   ste(versuftste)],adv,[],[]).

a([ge_e(vertaalde),
   ge_no_e(vertaald)],adv,[],[]).

a([ge_e(vertakte),
   ge_no_e(vertakt)],adv,[],[]).

a([ge_e(vertederde),
   ge_no_e(vertederd)],adv,[],[]).

a([e(verteerbare),
   er(verteerbaarder),
   ere(verteerbaardere),
   no_e(verteerbaar),
   st(verteerbaarst),
   ste(verteerbaarste)],nonadv,[],[]).

a([ge_e(verteerde),
   ge_no_e(verteerd)],adv,[],[]).

a([ge_e(vertegenwoordigde),
   ge_no_e(vertegenwoordigd)],adv,[],[]).

a([ge_e(vertekende),
   ge_no_e(vertekend)],adv,[],[]).

a([ge_e(vertelde),
   ge_no_e(verteld)],adv,[],[]).

a([e(verticale),
   e(vertikale),
   er(verticaler),
   er(vertikaler),
   ere(verticalere),
   ere(vertikalere),
   no_e(verticaal),
   no_e(vertikaal),
   st(verticaalst),
   st(vertikaalst),
   ste(verticaalste),
   ste(vertikaalste)],adv,[],[]).

a([ge_e(vertikte),
   ge_no_e(vertikt)],adv,[],[]).

a([ge_e(vertimmerde),
   ge_no_e(vertimmerd)],padv,[],[]).

a([ge_e(vertolkte),
   ge_no_e(vertolkt)],adv,[],[]).

a([ge_e(vertoonde),
   ge_no_e(vertoond)],adv,[],[]).

a([ge_e(vertraagde),
   ge_no_e(vertraagd)],adv,[],[]).

a([ge_e(vertrapte),
   ge_no_e(vertrapt)],adv,[],[]).

a([ge_e(vertroebelde),
   ge_no_e(vertroebeld)],adv,[],[]).

a([ge_e(vertroetelde),
   ge_no_e(vertroeteld)],adv,[],[]).

a([ge_both(vertrokken)],adv,[],[]).

a([ge_e(vertrouwde),
   ge_e(vertrouwdere),
   ge_no_e(vertrouwd),
   ge_no_e(vertrouwder)],padv,
  [er_pp_vp(met),
   pp(met),
   so_np],[]).

a([e(vertrouwelijke),
   er(vertrouwelijker),
   ere(vertrouwelijkere),
   no_e(vertrouwelijk),
   st(vertrouwelijkst),
   ste(vertrouwelijkste)],adv,[],[]).

a([ge_e(vertwijfelde),
   er(vertwijfelder),
   ere(vertwijfeldere),
   ge_no_e(vertwijfeld),
   st(vertwijfeldst),
   ste(vertwijfeldste)],adv,[],[]).

a([ge_e(vervaagde),
   ge_no_e(vervaagd)],adv,[],[]).

a([ge_e(vervaardigde),
   ge_no_e(vervaardigd)],adv,[],[]).

a([e(vervaarlijke),
   er(vervaarlijker),
   ere(vervaarlijkere),
   no_e(vervaarlijk),
   st(vervaarlijkst),
   ste(vervaarlijkste)],adv,[],[]).

a([ge_both(vervallen),
   er(vervallener),
   ere(vervallenere),
   st(vervallenst),
   ste(vervallenste)],adv,[],[]).

a([ge_e(vervalste),
   ge_no_e(vervalst)],adv,[],[]).

a([ge_both(vervangen)],adv,[],[]).

a([ge_e(vervatte),
   ge_no_e(vervat)],adv,[],[]).

a([ge_e(verveelde),
   er(verveelder),
   ere(verveeldere),
   ge_no_e(verveeld),
   st(verveeldst),
   ste(verveeldste)],padv,[],[]).

a([ge_e(vervelde),
   ge_no_e(verveld)],nonadv,[],[]).

a([e(vervelende),
   er(vervelender),
   ere(vervelendere),
   no_e(vervelend),
   st(vervelendst),
   ste(vervelendste)],adv,
  [subject_sbar,
   subject_vp,
   refl],[]).

a([e(verveloze),
   er(vervelozer),
   ere(vervelozere),
   no_e(verveloos),
   st(verveloost),
   ste(vervelooste)],padv,[],[]).

a([ge_e(ververste),
   ge_no_e(ververst)],adv,[],[]).

a([ge_both(vervlochten)],adv,[],[]).

a([ge_e(vervloeide),
   ge_no_e(vervloeid)],adv,[],[]).

a([ge_e(vervloekte),
   er(vervloekter),
   ere(vervloektere),
   ge_no_e(vervloekt),
   st(vervloektst),
   ste(vervloektste)],adv,[],[]).

a([ge_both(vervlogen)],adv,[],[]).

a([ge_e(vervluchtigde),
   ge_no_e(vervluchtigd)],adv,[],[]).

a([ge_e(vervoegde),
   ge_no_e(vervoegd)],adv,[],[]).

a([ge_e(vervoerde),
   ge_no_e(vervoerd)],adv,[],[]).

a([ge_e(vervolgde),
   ge_no_e(vervolgd)],adv,[],[]).

a([ge_e(vervolmaakte),
   ge_no_e(vervolmaakt)],adv,[],[]).

a([ge_e(vervormde),
   ge_no_e(vervormd)],adv,[],[]).

a([ge_e(vervreemde),
   ge_no_e(vervreemd)],adv,[],[]).

a([ge_e(vervroegde),
   ge_no_e(vervroegd)],adv,[],[]).

a([ge_e(vervuilde),
   er(vervuilder),
   ere(vervuildere),
   ge_no_e(vervuild),
   st(vervuildst),
   ste(vervuildste)],adv,[],[]).

a([ge_e(vervulde),
   ge_no_e(vervuld)],adv,[],[]).

a([ge_e(verwaaide),
   ge_no_e(verwaaid)],adv,[],[]).

a([e(verwaande),
   er(verwaander),
   ere(verwaandere),
   no_e(verwaand),
   st(verwaandst),
   ste(verwaandste)],adv,[],[]).

a([ge_e(verwaardigde),
   ge_no_e(verwaardigd)],adv,[],[]).

a([e(verwaarloosbare),
   no_e(verwaarloosbaar)],adv,[],[]).

a([ge_e(verwaarloosde),
   ge_no_e(verwaarloosd)],adv,[],[]).

a([ge_e(verwachte),
   ge_no_e(verwacht)],adv,[],[]).

a([e(verwachtingsvolle),
   no_e(verwachtingsvol)],adv,[],[]).

a([e(verwante),
   no_e(verwant)],nonadv,
  [pp(aan),
   pp(met)],[]).

a([ge_e(verwarde),
   er(verwarder),
   ere(verwardere),
   ge_no_e(verward),
   st(verwardst),
   ste(verwardste)],adv,
  [pp(met)],[]).

a([ge_e(verwarmde),
   ge_no_e(verwarmd)],adv,[],[]).

a([both(verwaten)],adv,[],[]).

a([ge_e(verwaterde),
   ge_no_e(verwaterd)],nonadv,[],[]).

a([ge_e(verweerde),
   er(verweerder),
   ere(verweerdere),
   ge_no_e(verweerd),
   st(verweerdst),
   ste(verweerdste)],adv,[],[]).

a([e(verweesde),
   no_e(verweesd)],padv,[],[]).

a([ge_e(verwekte),
   ge_no_e(verwekt)],adv,[],[]).

a([ge_e(verwelkomde),
   ge_no_e(verwelkomd)],adv,[],[]).

a([ge_e(verwelkte),
   ge_no_e(verwelkt)],adv,[],[]).

a([ge_e(verwende),
   er(verwender),
   ere(verwendere),
   ge_no_e(verwend),
   st(verwendst),
   ste(verwendste)],adv,
  [pp(door),
   pp(met)],[]).

a([ge_e(verwenste),
   ge_no_e(verwenst)],adv,[],[]).

a([ge_e(verwerkelijkte),
   ge_no_e(verwerkelijkt)],adv,[],[]).

a([ge_e(verwerkte),
   ge_no_e(verwerkt)],adv,[],[]).

a([e(verwerpelijke),
   er(verwerpelijker),
   ere(verwerpelijkere),
   no_e(verwerpelijk),
   st(verwerpelijkst),
   ste(verwerpelijkste)],nonadv,
  [subject_sbar],[]).

a([ge_e(verwesterde),
   ge_no_e(verwesterd)],padv,[],[]).

a([ge_both(verweten)],adv,[],[]).

a([ge_both(verweven)],adv,[],[]).

a([ge_both(verwezen)],adv,[],[]).

a([ge_e(verwezenlijkte),
   ge_no_e(verwezenlijkt)],adv,[],[]).

a([ge_e(verwijde),
   ge_no_e(verwijd)],adv,[],[]).

a([ge_e(verwijderde),
   ge_no_e(verwijderd)],adv,
  [pp(met),
   pp(van)],[]).

a([ende(verwijtende),
   er(verwijtender),
   ere(verwijtendere),
   end(verwijtend),
   st(verwijtendst),
   ste(verwijtendste)],adv,[],[]).

a([ge_e(verwikkelde),
   ge_no_e(verwikkeld)],adv,
  [pp(in)],[]).

a([ge_e(verwilderde),
   er(verwilderder),
   ere(verwilderdere),
   ge_no_e(verwilderd),
   st(verwilderdst),
   ste(verwilderdste)],adv,[],[]).

a([e(verwisselbare),
   no_e(verwisselbaar)],nonadv,[],[]).

a([ge_e(verwisselde),
   ge_no_e(verwisseld)],adv,[],[]).

a([ge_e(verwittigde),
   ge_no_e(verwittigd)],adv,[],[]).

a([e(verwoede),
   er(verwoeder),
   ere(verwoedere),
   no_e(verwoed),
   st(verwoedst),
   ste(verwoedste)],adv,[],[]).

a([ge_e(verwoeste),
   ge_no_e(verwoest)],adv,[],[]).

a([ende(verwoestende),
   er(verwoestender),
   ere(verwoestendere),
   end(verwoestend),
   st(verwoestendst),
   ste(verwoestendste)],adv,
  [transitive],[alles]).

a([ge_e(verwonde),
   ge_no_e(verwond)],adv,[],[]).

a([ge_e(verwonderde),
   er(verwonderder),
   ere(verwonderdere),
   ge_no_e(verwonderd),
   st(verwonderdst),
   ste(verwonderdste)],adv,
  [pp(over),
   object_sbar % Vlaams
  ],[]).

a([e(verwonderlijke),
   er(verwonderlijker),
   ere(verwonderlijkere),
   no_e(verwonderlijk),
   st(verwonderlijkst),
   ste(verwonderlijkste)],adv,
  [subject_sbar],[]).

a([ge_e(verwoorde),
   ge_no_e(verwoord)],adv,[],[]).

a([ge_both(verworden)],adv,[],[]).

a([ge_both(verworpen),
   er(verworpener),
   ere(verworpenere),
   st(verworpenst),
   ste(verworpenste)],adv,
  [pp(door)],[]).

a([ge_both(verworven)],adv,[],[]).

a([ge_both(verwrongen)],adv,[],[]).

a([ge_e(verzaakte),
   ge_no_e(verzaakt)],adv,[],[]).

a([ge_e(verzachte),
   ge_no_e(verzacht)],adv,[],[]).

a([e(verzachtende),
   er(verzachtender),
   ere(verzachtendere),
   no_e(verzachtend),
   st(verzachtendst),
   ste(verzachtendste)],adv,[],[]).

a([ge_e(verzadigde),
   er(verzadigder),
   ere(verzadigdere),
   ge_no_e(verzadigd),
   st(verzadigdst),
   ste(verzadigdste)],adv,[],[]).

a([ge_e(verzakelijkte),
   ge_no_e(verzakelijkt)],padv,[],[]).

a([ge_e(verzakte),
   ge_no_e(verzakt)],adv,[],[]).

a([ge_e(verzamelde),
   ge_no_e(verzameld)],adv,[],[]).

a([ge_e(verzande),
   ge_no_e(verzand)],adv,[],[]).

a([ge_e(verzegelde),
   ge_no_e(verzegeld)],adv,[],[]).

a([ge_e(verzeilde),
   ge_no_e(verzeild)],adv,[],[]).

a([e(verzekerbare),
   no_e(verzekerbaar)],nonadv,[],[]).

a([ge_e(verzekerde),
   ge_no_e(verzekerd)],padv,
  [er_pp_sbar(van),
   pp(van),
   pp(voor)],[her]).

a([ge_e(verzelfstandigde),
   ge_no_e(verzelfstandigd)],nonadv,[],[]).

a([ge_e(verzengde),
   ge_no_e(verzengd)],adv,[],[]).

a([ge_e(verzette),
   ge_no_e(verzet)],adv,[],[]).

a([ge_both(verzeten)],adv,[],[]).

a([ge_e(verziekte),
   ge_no_e(verziekt)],adv,[],[]).

a([ge_e(verzilte),
   ge_no_e(verzilt)],padv,[],[]).

a([ge_e(verzilverde),
   ge_no_e(verzilverd)],adv,[],[]).

a([ge_e(verzochte),
   ge_no_e(verzocht)],adv,[],[]).

a([ge_e(verzoende),
   ge_no_e(verzoend)],adv,[],[]).

a([ende(verzoenende),
   er(verzoenender),
   ere(verzoenendere),
   end(verzoenend),
   st(verzoenendst),
   ste(verzoenendste)],adv,[],[]).

a([ge_both(verzonden)],adv,[],[]).

a([ge_both(verzonken)],adv,[],[]).

a([ge_both(verzonnen)],adv,[],[]).

a([ge_both(verzopen)],adv,[],[]).

a([ge_e(verzorgde),
   er(verzorgder),
   ere(verzorgdere),
   ge_no_e(verzorgd),
   st(verzorgdst),
   ste(verzorgdste)],adv,[],[]).

a([e(verzotte),
   er(verzotter),
   ere(verzottere),
   no_e(verzot),
   st(verzotst),
   ste(verzotste)],adv,[],[]).

a([e(verzottende),
   no_e(verzottend)],adv,[],[]).

a([ge_e(verzuchte),
   ge_no_e(verzucht)],adv,[],[]).

a([e(verzuilde),
   no_e(verzuild)],nonadv,[],[]).

a([ge_e(verzuimde),
   ge_no_e(verzuimd)],adv,[],[]).

a([ge_e(verzuurde),
   ge_no_e(verzuurd)],adv,[],[]).

a([ge_e(verzwaarde),
   ge_no_e(verzwaard)],adv,[],[]).

a([ge_e(verzwakte),
   ge_no_e(verzwakt)],adv,[],[]).

a([ge_both(verzwegen)],adv,[],[]).

a([ge_e(verzwikte),
   ge_no_e(verzwikt)],padv,[],[]).

a([ge_both(verzwolgen)],adv,[],[]).

a([e(vette),
   er(vetter),
   ere(vettere),
   no_e(vet),
   st(vetst),
   ste(vetste)],adv,[],[]).

a([e(veterinaire),
   no_e(veterinair)],nonadv,[],[]).

a([ge_e(vetgemeste),
   ge_no_e(vetgemest)],padv,[],[]).

a([e(vettige),
   er(vettiger),
   ere(vettigere),
   no_e(vettig),
   st(vettigst),
   ste(vettigste)],adv,[],[]).

a([both(vice)],nonadv,[],[]).

a([e(vicieuze),
   er(vicieuzer),
   ere(vicieuzere),
   no_e(vicieus),
   st(vicieust),
   ste(vicieuste)],nonadv,[],[]).

a([e(vieve),no_e(vief)],adv,[],[]).

a([prefix('vierde-klas'),
   prefix([vierde,klas]),
   prefix(['4e',klas])],nonadv,[],[]).

a([e(vierkante),
   no_e(vierkant)],adv,[],[]).

a([e(vieze),
   er(viezer),
   ere(viezere),
   no_e(vies),
   st(viest),
   ste(vieste)],adv,
  [pp(van)],[]).

a([e(vijandelijke),
   er(vijandelijker),
   ere(vijandelijkere),
   no_e(vijandelijk),
   st(vijandelijkst),
   ste(vijandelijkste)],adv,[],[]).

a([e(vijandige),
   er(vijandiger),
   ere(vijandigere),
   no_e(vijandig),
   st(vijandigst),
   ste(vijandigste)],adv,
  [pp(aan),
   so_np],[]).

a([both(vijftiger)],nonadv,[],[]).

a([e(vileine),
   no_e(vilein),
   er(vileiner),
   ere(vileinere),
   st(vileinst),
   ste(vileinste)],adv,[],[]).

a([stof(vilten)],nonadv,[],[]).

a([e(vindingrijke),
   er(vindingrijker),
   ere(vindingrijkere),
   no_e(vindingrijk),
   st(vindingrijkst),
   ste(vindingrijkste)],adv,[],[]).

a([e(vinnige),
   er(vinniger),
   ere(vinnigere),
   no_e(vinnig),
   st(vinnigst),
   ste(vinnigste)],adv,[],[straal]).

a([both(vintage)],nonadv,[],[]).

a([stof(vinyl)],nonadv,[],[]).

a([e(violette),
   er(violetter),
   ere(violettere),
   no_e(violet),
   st(violetst),
   ste(violetste)],nonadv,[],[]).

a([e(virale),
   no_e(viraal)],nonadv,[],[]).

a([e(viriele),
   er(virieler),
   ere(virielere),
   no_e(viriel),
   st(virielst),
   ste(virielste)],nonadv,[],[]).

a([e(virtuele),
   no_e(virtueel)],adv,[],[]).

a([e(virtuoze),
   er(virtuozer),
   ere(virtuozere),
   no_e(virtuoos),
   st(virtuoost),
   ste(virtuooste)],adv,[],[]).

a([e(virulente),
   no_e(virulent)],adv,[],[]).

a([e(visionaire),
   no_e(visionair)],nonadv,[],[]).

a([e(visuele),
   no_e(visueel)],adv,[],[]).

a([e(vitale),
   er(vitaler),
   ere(vitalere),
   no_e(vitaal),
   st(vitaalst),
   ste(vitaalste)],nonadv,
  [pp(bij),
   pp(voor)],[]).

a([pred(vlakbij)],locadv,[],[]).

a([e(vlakke),
   er(vlakker),
   ere(vlakkere),
   no_e(vlak),
   st(vlakst),
   ste(vlakste)],adv,[],[]).

a([both(vleesgeworden)],nonadv,[],[]).

a([e(vleiende),
   er(vleiender),
   ere(vleiendere),
   no_e(vleiend),
   st(vleiendst),
   ste(vleiendste)],adv,
  [pp(over),
   pp(voor)],[]).

a([e(vlekkeloze),
   no_e(vlekkeloos)],adv,[],[]).

a([e(vlekkerige),
   er(vlekkeriger),
   ere(vlekkerigere),
   no_e(vlekkerig),
   st(vlekkerigst),
   ste(vlekkerigste)],adv,[],[]).

a([e(vleselijke),
   no_e(vleselijk)],adv,[],[]).

a([e(vlezige),
   er(vleziger),
   ere(vlezigere),
   no_e(vlezig),
   st(vlezigst),
   ste(vlezigste)],nonadv,[],[]).

a([e(vliesvleugelige),
   no_e(vliesvleugelig)],nonadv,[],[]).

a([e(vliegensvlugge),
   no_e(vliegensvlug)],adv,[],[]).

a([e(vlijmscherpe),
   er(vlijmscherper),
   ere(vlijmscherpere),
   no_e(vlijmscherp),
   st(vlijmscherpst),
   ste(vlijmscherpste)],adv,[],[]).

a([e(vlijtige),
   er(vlijtiger),
   ere(vlijtigere),
   no_e(vlijtig),
   st(vlijtigst),
   ste(vlijtigste)],adv,[],[]).

a([e(vloeibare),
   er(vloeibaarder),
   ere(vloeibaardere),
   no_e(vloeibaar),
   st(vloeibaarst),
   ste(vloeibaarste)],padv,[],[]).

a([e(vloeiende),
   er(vloeiender),
   ere(vloeiendere),
   no_e(vloeiend),
   st(vloeiendst),
   ste(vloeiendste)],adv,[],[]).

a([e(vlotte),
   er(vlotter),
   ere(vlottere),
   no_e(vlot),
   st(vlotst),
   ste(vlotste)],adv,[],[]).

a([pred(vlotjes)],adv,[],[]).

a([e(vluchtige),
   er(vluchtiger),
   ere(vluchtigere),
   no_e(vluchtig),
   st(vluchtigst),
   ste(vluchtigste)],adv,[],[]).

a([e(vlugge),
   er(vlugger),
   ere(vluggere),
   no_e(vlug),
   st(vlugst),
   ste(vlugste)],adv,[],[]).

a([e(vocale),
   no_e(vocaal)],adv,[],[]).

a([e(vochtige),
   er(vochtiger),
   ere(vochtigere),
   no_e(vochtig),
   st(vochtigst),
   ste(vochtigste)],nonadv,[],[]).

a([e(voedzame),
   er(voedzamer),
   ere(voedzamere),
   no_e(voedzaam),
   st(voedzaamst),
   ste(voedzaamste)],nonadv,[],[]).

a([e(voelbare),
   er(voelbaarder),
   ere(voelbaardere),
   no_e(voelbaar),
   st(voelbaarst),
   ste(voelbaarste)],adv,[],[subject_sbar]).

a([pred(voetbalmoe)],padv,[],[]).

a([e(vogelvrije),
   no_e(vogelvrij)],padv,[],[]).

a([e(volgeboekte),
   no_e(volgeboekt)],padv,[],[]).

a([e(volgebouwde),
   no_e(volgebouwd)],padv,[],[]).

a([e(volgekladde),
   no_e(volgeklad)],padv,[],[]).

a([ge_both(volgeschreven)],padv,[],[]).

a([ge_e(volgestouwde),
   ge_no_e(volgestouwd)],nonadv,[],[]).

a([e(volgroeide),
   no_e(volgroeid)],nonadv,[],[]).

a([e(volle),
   er(voller),
   ere(vollere),
   postn_no_e(vol),  % een boek vol met verhalen
   st(volst),
   ste(volste)],both,
  [pp(met)],
  [barstens,
   boorde,
   bom,
   ei,
   half,
   mud,
   over,
   prop,
   stamp,
   tjok]).

a([e(volle),
   no_e(vol)],both,
  [pp(van)],
  []).

a([e(volle),
   no_e(vol)],both,
  [],
  [belofte]).

a([both(volbloed)],nonadv,[],[]).

a([ge_e(volbrachte),
   ge_no_e(volbracht)],adv,[],[]).

a([ge_e(voldane),
   ge_no_e(voldaan)],adv,[],[]).

a([both(voldoende)],adv,
  [object_sbar,
   object_vp,
   subject_sbar,
   subject_vp],[]).

a([both(voldongen)],nonadv,[],[]).

a([ge_both(voldragen)],adv,[],[]).

a([ge_both(volgehouden)],adv,[],[]).

a([ge_both(volgeladen)],adv,[],[]).

a([ge_both(volgelopen)],adv,[],[]).

a([ge_e(volgepakte),
   ge_no_e(volgepakt)],padv,[],[]).

a([ge_e(volgepropte),
   ge_no_e(volgepropt)],adv,[],[]).

a([e(volgzame),
   er(volgzamer),
   ere(volgzamere),
   no_e(volgzaam),
   st(volgzaamst),
   ste(volgzaamste)],nonadv,[],[]).

a([e(volharde),
   no_e(volhard)],nonadv,[],[]).

a([both(volkomen),
   er(volkomener),
   ere(volkomenere),
   st(volkomenst),
   ste(volkomenste)],adv,[],[]).

a([e(volkse),
   er(volkser),
   ere(volksere),
   no_e(volks),
   st(volkst),
   ste(volkste)],nonadv,[],[]).

a([er(vollediger),
   ere(volledigere),
   e(volledige),
   no_e(volledig)],adv,[],[]).

a([e(volleerde),
   no_e(volleerd)],nonadv,[],[]).

a([ge_e(volmaakte),
   er(volmaakter),
   ere(volmaaktere),
   ge_no_e(volmaakt),
   st(volmaaktst),
   ste(volmaaktste)],adv,[],[]).

a([e(volmondige),
   er(volmondiger),
   ere(volmondigere),
   no_e(volmondig),
   st(volmondigst),
   ste(volmondigste)],adv,[],[]).

a([both(volslagen)],adv,[],[]).

a([e(volstrekte),
   no_e(volstrekt)],adv,[],[]).

a([e(voltallige),
   no_e(voltallig)],padv,[],[]).

a([both(voltijds),e(voltijdse)],adv,[],[]).

a([ge_e(voltooide),
   ge_no_e(voltooid)],adv,[],[]).

a([ge_both(voltrokken)],adv,[],[]).

a([ge_e(volvoerde),
   ge_no_e(volvoerd)],adv,[],[]).

a([er(volwaardiger),
   ere(volwaardigere),
   e(volwaardige),
   no_e(volwaardig)],adv,[],[]).

a([both(volwassen),
   er(volwassener),
   ere(volwassenere),
   st(volwassenst),
   ste(volwassenste)],adv,[],[]).

a([pred([voor,de,hand])],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([pred([voor,het,eerst]),
   pred([voor,'\'t',eerst])],adv,
  [subject_sbar],[]).

a([pred([voor,het,laatst]),
   pred([voor,'\'t',laatst])],adv,
  [subject_sbar],[]).

a([pred(voor),
   pred(vóór)],locadv,[],[]).

a([e(vooraanstaande),
   no_e(vooraanstaand)],nonadv,[],[]).

a([e(voorafgaandelijke),
   no_e(voorafgaandelijk)],adv,[],[]).  % europarl

a([ende(voorafgaande),
   end(voorafgaand)],adv,[pp(aan)],[]).

a([ge_e(voorafgegane),
   ge_no_e(voorafgegaan)],adv,[],[]).

a([e(voorbarige),
   er(voorbariger),
   ere(voorbarigere),
   no_e(voorbarig),
   st(voorbarigst),
   ste(voorbarigste)],adv,[],
  [subject_sbar,
   subject_vp]).

a([e(voorbeeldige),
   er(voorbeeldiger),
   ere(voorbeeldigere),
   no_e(voorbeeldig),
   st(voorbeeldigst),
   ste(voorbeeldigste)],adv,[],[]).

a([ge_both(voorbehouden)],adv,
  [transitive,
   pp(aan)],[]).

a([ge_e(voorbereide),
   ge_no_e(voorbereid)],adv,
  [pp(op),
   er_pp_sbar(op)],[]).

a([ge_e(voorbestemde),
   ge_no_e(voorbestemd)],adv,[],[]).

a([e(voorbije),
   both(voorbij) % VLAAMS: de voorbij weken 
  ],adv,[],[]).

a([ge_e(voorbijgegane),
   ge_no_e(voorbijgegaan)],adv,[],[]).

a([ge_both(voorbijgekomen)],adv,[],[]).

a([ge_both(voorbijgelopen)],adv,[],[]).

a([ge_both(voorbijgereden)],adv,[],[]).

a([ge_both(voorbijgeschoten)],adv,[],[]).

a([ge_e(voorbijgestreefde),
   ge_no_e(voorbijgestreefd)],adv,[],[]).

a([ge_both(voorbijgetrokken)],adv,[],[]).

a([e(voordelige),
   er(voordeliger),
   ere(voordeligere),
   no_e(voordelig),
   st(voordeligst),
   ste(voordeligste)],adv,
  [pp(voor),
   subject_sbar,
   subject_vp],[]).

a([ge_e(voorgedane),
   ge_no_e(voorgedaan)],adv,[],[]).

a([ge_both(voorgedragen)],adv,[],[]).

a([ge_e(voorgegane),
   ge_no_e(voorgegaan)],adv,[],[]).

a([ge_both(voorgegeven)],adv,[],[]).

a([ge_both(voorgehouden)],adv,[],[]).

a([ge_both(voorgekomen)],adv,[],[]).

a([ge_e(voorgelegde),
   ge_no_e(voorgelegd)],adv,
  [so_np],[]).

a([ge_both(voorgelegen)],adv,[],[]).

a([ge_e(voorgeleide),
   ge_no_e(voorgeleid)],nonadv,[],[]).

a([ge_both(voorgelezen)],adv,[],[]).

a([ge_e(voorgelichte),
   ge_no_e(voorgelicht)],adv,[],[]).

a([ge_both(voorgenomen)],adv,[],[]).

a([ge_both(voorgereden)],adv,[],[]).

a([ge_e(voorgeschotelde),
   ge_no_e(voorgeschoteld)],adv,[so_np],[]).

a([ge_both(voorgeschreven)],adv,[],[]).

a([ge_e(voorgespeelde),
   ge_no_e(voorgespeeld)],adv,[],[]).

a([ge_e(voorgespiegelde),
   ge_no_e(voorgespiegeld)],adv,[],[]).

a([ge_no_e(voorgestaan),
   ge_e(voorgestane)],adv,[],[]).

a([ge_e(voorgestelde),
   ge_no_e(voorgesteld)],adv,
  [so_np,
   so_pp(aan)],[]).

a([ge_both(voorgevallen)],adv,[],[]).

a([ge_e(voorgevulde),
   ge_no_e(voorgevuld)],adv,[],[]).

a([ge_e(voorgewende),
   ge_no_e(voorgewend)],adv,[],[]).

a([ge_e(voorgezette),
   ge_no_e(voorgezet)],adv,[],[]).

a([ge_both(voorgezeten)],adv,[],[]).

a([both(voorhanden)],nonadv,
  [subject_vp],[]).

a([e(voorhistorische),
   no_e(voorhistorisch)],nonadv,[],[]).

a([both(vooringenomen)],padv,[],[]).

a([ge_both(voorkomen)],adv,[],[]).

a([%  ste(voorlaatste), already number, "we staan voorlaatste"
   st(voorlaatst)],adv,[],[]).

a([e(voorlopige),
   no_e(voorlopig)],adv,[],[]).

a([e(voormalige),
   both(voormalig)],nonadv,[],[]).

a([e(voormelde),no_e(voormeld)],nonadv,[],[]).

a([e(voorname),
   er(voornamer),
   ere(voornamere),
   no_e(voornaam),
   st(voornaamst),
   ste(voornaamste)],padv,
  [subject_sbar,
   subject_vp],[]).

a([pred(voornemens)],nonadv,
  [object_vp],[]).

a([ge_e(voornoemde),
   ge_no_e(voornoemd)],nonadv,[],[]).

a([ge_e(vooronderstelde),
   ge_no_e(voorondersteld)],adv,[],[]).

a([e(vooroorlogse),
   no_e(vooroorlogs)],nonadv,[],[]).

a([ge_e(vooropgestelde),
   ge_no_e(vooropgesteld)],adv,
  [object_sbar,
   transitive],[]).

a([ge_e(vooropgezette),
   ge_no_e(vooropgezet)],padv,
  [transitive],[]).

a([e(voorradige),
   no_e(voorradig)],nonadv,[],[]).

a([e(voorspelbare),
   er(voorspelbaarder),
   ere(voorspelbaardere),
   no_e(voorspelbaar),
   st(voorspelbaarst),
   ste(voorspelbaarste)],adv,
  [subject_sbar],[]).

a([ge_e(voorspelde),
   ge_no_e(voorspeld)],adv,[],[]).

a([e(voorspoedige),
   er(voorspoediger),
   ere(voorspoedigere),
   no_e(voorspoedig),
   st(voorspoedigst),
   ste(voorspoedigste)],adv,[],[]).

a([e(voorste),
   no_e(voorste)],adv,[],[]).

a([e(voorstelbare),
   er(voorstelbaarder),
   ere(voorstelbaardere),
   no_e(voorstelbaar),
   st(voorstelbaarst),
   ste(voorstelbaarste)],nonadv,
  [subject_sbar],[]).

a([ge_both(voortbewogen)],adv,[],[]).

a([ge_e(voortgebrachte),
   ge_no_e(voortgebracht)],adv,[],[]).

a([ge_both(voortgedreven)],both,[],[]).

a([ge_e(voortgeduwde),
   ge_no_e(voortgeduwd)],adv,[],[]).

a([ge_e(voortgegane),
   ge_no_e(voortgegaan)],adv,[],[]).

a([ge_both(voortgekomen)],adv,[],[]).

a([ge_e(voortgeplante),
   ge_no_e(voortgeplant)],adv,[],[]).

a([ge_both(voortgeschreden)],adv,[],[]).

a([ge_e(voortgesleepte),
   ge_no_e(voortgesleept)],adv,[],[]).

a([ge_both(voortgesproten)],adv,[],[]).

a([ge_e(voortgestuwde),
   ge_no_e(voortgestuwd)],adv,[],[]).

a([ge_both(voortgetrokken)],adv,[],[]).

a([ge_e(voortgevloeide),
   ge_no_e(voortgevloeid)],adv,[],[]).

a([ge_e(voortgewoekerde),
   ge_no_e(voortgewoekerd)],adv,[],[]).

a([ge_e(voortgezette),
   ge_no_e(voortgezet)],adv,[],[]).

a([e(voortijdige),
   no_e(voortijdig)],adv,[],[]).

a([e(voortreffelijke),
   er(voortreffelijker),
   ere(voortreffelijkere),
   no_e(voortreffelijk),
   st(voortreffelijkst),
   ste(voortreffelijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ende(voortschreidende),
   end(voortschreidend)],nonadv,[],[]).

a([e(voortvarende),
   er(voortvarender),
   ere(voortvarendere),
   no_e(voortvarend),
   st(voortvarendst),
   ste(voortvarendste)],adv,[],[]).

a([e(voortvluchtige),
   no_e(voortvluchtig)],nonadv,[],[]).

a([ge_e(vooruitgegane),
   ge_no_e(vooruitgegaan)],adv,[],[]).

a([ge_both(vooruitgelopen)],adv,[],[]).

a([ge_both(vooruitgeschoven)],padv,[],[]).

a([both(vooruitgestoken)],adv,[],[]).

a([e(vooruitgestreefde),
   no_e(vooruitgestreefd)],adv,[],[]).

a([e(vooruitstekende),
   no_e(vooruitstekend)],adv,
  [subject_sbar,
   subject_vp],[]).

a([ende(vooruitstrevende),
   er(vooruitstrevender),
   ere(vooruitstrevendere),
   end(vooruitstrevend),
   st(vooruitstrevendst),
   ste(vooruitstrevendste)],adv,
  [subject_sbar,
   subject_vp,
   pp(in)],[]).

a([ge_e(voorverwarmde),
   ge_no_e(voorverwarmd)],adv,[],[]).

a([ge_e(voorvoelde),
   ge_no_e(voorvoeld)],adv,[],[]).

a([e(voorwaardelijke),
   postn_no_e(voorwaardelijk)],adv,[],[]).

a([e(voorwaartse),
   postn_no_e(voorwaarts)],diradv,[],[]).

a([e(voorzichtige),
   er(voorzichtiger),
   ere(voorzichtigere),
   no_e(voorzichtig),
   st(voorzichtigst),
   ste(voorzichtigste)],adv,
  [pp(met)],[]).

a([ge_no_e(voorzien),
   ge_e(voorziene)],padv,
  [pp(van),
   pp(voor)],[]).

a([e(voze),
   er(vozer),
   ere(vozere),
   no_e(voos),
   st(voost),
   ste(vooste)],nonadv,[],[]).

a([e(vorenstaande),
   no_e(vorenstaand)],nonadv,[],[]).

a([e(vorige),
   no_e(vorig)],nonadv,[],[]).

a([e(vormelijke),
   er(vormelijker),
   ere(vormelijkere),
   no_e(vormelijk),
   st(vormelijkst),
   ste(vormelijkste)],adv,[],[]).

a([e(vormeloze),
   er(vormelozer),
   ere(vormelozere),
   no_e(vormeloos),
   st(vormeloost),
   ste(vormelooste)],nonadv,[],[]).

a([ge_both(vormgegeven)],nonadv,[],[]).

%% only with compounds????
a([e(vormige),
   no_e(vormig)],padv,[],
  [bol,
   cirkel,
   ei,h(ei),
   hart,
   peer,
   piramide,
   spiraal,
   ster,
   ui,h(ui),
   h('T'),h(t)]).

a([e(vormloze),
   er(vormlozer),
   ere(vormlozere),
   no_e(vormloos),
   st(vormloost),
   ste(vormlooste)],nonadv,[],[]).

a([e(vorstelijke),
   er(vorstelijker),
   ere(vorstelijkere),
   no_e(vorstelijk),
   st(vorstelijkst),
   ste(vorstelijkste)],adv,[],[]).

a([ende(vredelievende),
   er(vredelievender),
   ere(vredelievendere),
   end(vredelievend),
   st(vredelievendst),
   ste(vredelievendste)],adv,[],[]).

a([e(vredige),
   er(vrediger),
   ere(vredigere),
   no_e(vredig),
   st(vredigst),
   ste(vredigste)],adv,[],[]).

a([e(vreedzame),
   er(vreedzamer),
   ere(vreedzamere),
   no_e(vreedzaam),
   st(vreedzaamst),
   ste(vreedzaamste)],adv,[],[]).

a([e(vreemde),
   er(vreemder),
   ere(vreemdere),
   no_e(vreemd),
   st(vreemdst),
   ste(vreemdste)],adv,
  [subject_sbar,
   subject_vp,
   so_np,
   so_pp(aan),
   pp(aan)],[]).

a([e(vreemde),
   no_e(vreemd)],adv,
  [],[wild]).

a([e(vreemdsoortige),
   er(vreemdsoortiger),
   ere(vreemdsoortigere),
   no_e(vreemdsoortig),
   st(vreemdsoortigst),
   ste(vreemdsoortigste)],nonadv,[],[]).

a([e(vreselijke),
   er(vreselijker),
   ere(vreselijkere),
   no_e(vreselijk),
   st(vreselijkst),
   ste(vreselijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(tegen),
   pp(voor)],[]).

a([e(vreugdeloze),
   er(vreugdelozer),
   ere(vreugdelozere),
   no_e(vreugdeloos),
   st(vreugdeloost),
   ste(vreugdelooste)],adv,[],[]).

a([e(vreugdevolle),
   er(vreugdevoller),
   ere(vreugdevollere),
   no_e(vreugdevol),
   st(vreugdevolst),
   ste(vreugdevolste)],nonadv,[subject_sbar],[]).

a([e(vriendelijke),
   er(vriendelijker),
   ere(vriendelijkere),
   no_e(vriendelijk),
   st(vriendelijkst),
   ste(vriendelijkste)],adv,[subject_sbar],[]).

a([e(vriendelijke),
   er(vriendelijker),
   ere(vriendelijkere),
   no_e(vriendelijk),
   st(vriendelijkst),
   ste(vriendelijkste)],adv,[],
  [s(bedrijf),
   s(gebruik)]).

a([e(vriendschappelijke),
   er(vriendschappelijker),
   ere(vriendschappelijkere),
   no_e(vriendschappelijk),
   st(vriendschappelijkst),
   ste(vriendschappelijkste)],adv,[],[]).

a([pred(vrijaf)],nonadv,[],[]).

a([e(vrije),
   er(vrijer),
   ere(vrijere),
   no_e(vrij),
   st(vrijst),
   ste(vrijste)],both,
  [object_vp,
   pp(van),
   pp(voor)],
  []).

a([e(vrije),
   no_e(vrij)],both,
  [],
  [ijs,
   premie,
   rook,
   sneeuw,
   taks,tax,
   water,
   zwavel]).

a([e(vrijblijvende),
   no_e(vrijblijvend)],adv,
  [subject_vp],[]).

a([e(vrijdagse),
   no_e(vrijdags)],adv,[],[]).

a([e(vrijelijke),
   no_e(vrijelijk)],adv,[],[]).

a([ge_both(vrijgegeven)],adv,[],[]).

a([ge_both(vrijgekomen)],adv,[],[]).

a([ge_both(vrijgelaten)],adv,[],[]).

a([ge_e(vrijgemaakte),
   ge_no_e(vrijgemaakt)],adv,[],[]).

a([ge_e(vrijgepleite),
   ge_no_e(vrijgepleit)],adv,[],[]).

a([ge_both(vrijgesproken)],adv,[],[]).

a([ge_e(vrijgestelde),
   ge_no_e(vrijgesteld)],adv,[],[]).

a([both(vrijgevochten)],nonadv,[],[]).

a([e(vrijgezelle),
   no_e(vrijgezel)],nonadv,[],[]).

a([e(vrijmoedige),
   er(vrijmoediger),
   ere(vrijmoedigere),
   no_e(vrijmoedig),
   st(vrijmoedigst),
   ste(vrijmoedigste)],adv,
  [pp(in)],[]).

a([e(vrijstaande),
   no_e(vrijstaand)],padv,[],[]).

a([pred(vrijuit)],adv,[],[]).

a([e(vrijwillige),
   er(vrijwilliger),
   ere(vrijwilligere),
   no_e(vrijwillig),
   st(vrijwilligst),
   ste(vrijwilligste)],adv,[],[]).

a([e(vrijzinnige),
   er(vrijzinniger),
   ere(vrijzinnigere),
   no_e(vrijzinnig),
   st(vrijzinnigst),
   ste(vrijzinnigste)],adv,[],[]).

a([e(vroege),
   er(vroeger),
   ere(vroegere),
   no_e(vroeg),
   st(vroegst),
   ste(vroegste)],tmpadv,[],[]).

a([e(vroegtijdige),
   er(vroegtijdiger),
   ere(vroegtijdigere),
   no_e(vroegtijdig),
   st(vroegtijdigst),
   ste(vroegtijdigste)],adv,[],[]).

a([e(vrolijke),
   er(vrolijker),
   ere(vrolijkere),
   no_e(vrolijk),
   st(vrolijkst),
   ste(vrolijkste)],adv,[],[]).

a([e(vrome),
   er(vromer),
   ere(vromere),
   no_e(vroom),
   st(vroomst),
   ste(vroomste)],adv,[],[]).

a([e(vrouwelijke),
   er(vrouwelijker),
   ere(vrouwelijkere),
   no_e(vrouwelijk),
   st(vrouwelijkst),
   ste(vrouwelijkste)],nonadv,[],[]).

a([e(vrouwvriendelijke),
   no_e(vrouwvriendelijk)],nonadv,[subject_vp],[]).

a([e(vruchtbare),
   er(vruchtbaarder),
   ere(vruchtbaardere),
   no_e(vruchtbaar),
   st(vruchtbaarst),
   ste(vruchtbaarste)],nonadv,[],[]).

a([e(vruchteloze),
   er(vruchtelozer),
   ere(vruchtelozere),
   no_e(vruchteloos),
   st(vruchteloost),
   ste(vruchtelooste)],adv,[],[]).

a([e(vuige),
   no_e(vuig)],adv,[],[]).

a([e(vuile),
   er(vuiler),
   ere(vuilere),
   no_e(vuil),
   st(vuilst),
   ste(vuilste)],adv,[],[]).

a([e(vulgaire),
   er(vulgairder),
   ere(vulgairdere),
   no_e(vulgair),
   st(vulgairst),
   ste(vulgairste)],adv,[],[]).

a([e(vulkanische),
   no_e(vulkanisch)],adv,[],[]).  % adv: vulkanisch actieve gebieden

a([e(vurige),
   er(vuriger),
   ere(vurigere),
   no_e(vurig),
   st(vurigst),
   ste(vurigste)],adv,[],[]).

a([e(vuurrode),
   er(vuurroder),
   ere(vuurrodere),
   no_e(vuurrood),
   st(vuurroodst),
   ste(vuurroodste)],nonadv,[],[]).

a([e(vuurvaste),
   no_e(vuurvast)],nonadv,[],[]).

a([e(waakzame),
   er(waakzamer),
   ere(waakzamere),
   no_e(waakzaam),
   st(waakzaamst),
   ste(waakzaamste)],adv,
  [object_sbar],[]).

a([e(waanzinnige),
   er(waanzinniger),
   ere(waanzinnigere),
   no_e(waanzinnig),
   st(waanzinnigst),
   ste(waanzinnigste)],adv,[subject_sbar,
                            subject_vp],[]).

a([e(ware),
   no_e(waar)],nonadv,
  [subject_sbar,
   pp(voor)],[]).

a([e(waarachtige),
   er(waarachtiger),
   ere(waarachtigere),
   no_e(waarachtig),
   st(waarachtigst),
   ste(waarachtigste)],adv,[],[]).

a([both(waarde)],nonadv,[],[]).

a([e(waardeloze),
   er(waardelozer),
   ere(waardelozere),
   no_e(waardeloos),
   st(waardeloost),
   ste(waardelooste)],adv,
  [subject_sbar],[]).

a([e(waarderende),
   er(waarderender),
   ere(waarderendere),
   no_e(waarderend),
   st(waarderendst),
   ste(waarderendste)],adv,[],[]).

a([e(waardevolle),
   er(waardevoller),
   ere(waardevollere),
   no_e(waardevol),
   st(waardevolst),
   ste(waardevolste)],nonadv,
  [pp(voor),
   subject_vp,
   subject_sbar],[]).

a([e(waardevrije),
   no_e(waardevrij)],nonadv,[],[]).

a([e(waardige),
   er(waardiger),
   ere(waardigere),
   no_e(waardig),
   st(waardigst),
   ste(waardigste)],adv,[],
  [s(behartigen),
   zee]).

a([e(waardige),
   er(waardiger),
   ere(waardigere),
   no_e(waardig),
   st(waardigst),
   ste(waardigste)],adv,
  [so_np,
   so_np_subject_vp,
   so_np_subject_sbar],
  []).

a([e(waargebeurde),
   no_e(waargebeurd)],nonadv,[],[]).

a([ge_e(waargemaakte),
   ge_no_e(waargemaakt)],adv,[],[]).

a([ge_both(waargenomen)],adv,[],[]).

a([e(waarheidsgetrouwe),
   er(waarheidsgetrouwer),
   ere(waarheidsgetrouwere),
   no_e(waarheidsgetrouw),
   st(waarheidsgetrouwst),
   ste(waarheidsgetrouwste)],adv,[],[]).

a([e(waarlijke),
   no_e(waarlijk)],adv,[],[]).

a([e(waarneembare),
   er(waarneembaarder),
   ere(waarneembaardere),
   no_e(waarneembaar),
   st(waarneembaarst),
   ste(waarneembaarste)],adv,[],[]).

a([e(waarschijnlijke),
   er(waarschijnlijker),
   ere(waarschijnlijkere),
   no_e(waarschijnlijk),
   st(waarschijnlijkst),
   ste(waarschijnlijkste)],adv,
  [subject_sbar],[]).

a([e(wagenwijde),
   no_e(wagenwijd)],adv,[],[]).

a([e(wakkere),
   er(wakkerder),
   ere(wakkerdere),
   no_e(wakker),
   st(wakkerst),
   ste(wakkerste)],adv,[],[]).

a([e(walgelijke),
   er(walgelijker),
   ere(walgelijkere),
   no_e(walgelijk),
   st(walgelijkst),
   ste(walgelijkste)],adv,
  [subject_sbar],[]).

a([e(wanhopige),
   er(wanhopiger),
   ere(wanhopigere),
   no_e(wanhopig),
   st(wanhopigst),
   ste(wanhopigste)],adv,[],[]).

a([e(wankele),
   er(wankeler),
   ere(wankelere),
   no_e(wankel),
   st(wankelst),
   ste(wankelste)],adv,[],[]).

a([e(wanordelijke),
   er(wanordelijker),
   ere(wanordelijkere),
   no_e(wanordelijk),
   st(wanordelijkst),
   ste(wanordelijkste)],adv,[],[]).

a([e(wantrouwende),
   er(wantrouwender),
   ere(wantrouwendere),
   no_e(wantrouwend),
   st(wantrouwendst),
   ste(wantrouwendste)],adv,[],[]).

a([e(wantrouwige),
   er(wantrouwiger),
   ere(wantrouwigere),
   no_e(wantrouwig),
   st(wantrouwigst),
   ste(wantrouwigste)],adv,
  [pp(tegen),
   pp(tegenover)],[]).

a([e(warme),
   er(warmer),
   ere(warmere),
   no_e(warm),
   st(warmst),
   ste(warmste)],adv,[],[]).

a([ge_both(warmgelopen)],padv,[],[]).

a([pred(warmpjes)],adv,[],[]).

a([e(warrige),
   er(warriger),
   ere(warrigere),
   no_e(warrig),
   st(warrigst),
   ste(warrigste)],padv,[],[]).

a([pred(wars)],padv,
  [er_pp_vp(van),
   pp(van)],[]).

a([stof(wassen)],nonadv,[],[]).

a([e(waterdichte),
   no_e(waterdicht)],adv,[],[]). % adv: iets waterdicht bewijzen

a([e(waterige),
   er(wateriger),
   ere(waterigere),
   no_e(waterig),
   st(waterigst),
   ste(waterigste)],nonadv,[],[]).

a([pred(waterpas)],adv,[],[]).

a([e(waterstaatkundige),
   no_e(waterstaatkundig)],adv,[],[]).

a([e(wazige),
   er(waziger),
   ere(wazigere),
   no_e(wazig),
   st(wazigst),
   ste(wazigste)],adv,[],[]).

a([e(wederkerige),
   no_e(wederkerig)],adv,[],[]).

a([e(wederrechtelijke),
   no_e(wederrechtelijk)],adv,[],[]).

a([ge_both(wedervaren)],adv,[],[]).

a([e(wederzijdse),
   no_e(wederzijds)],adv,[],[]).

a([e(weeë),
   no_e(wee)],adv,[],[]).

a([e(weeïge),no_e(weeïg)],adv,[],[]).

a([e(weke),
   er(weker),
   ere(wekere),
   no_e(week),
   st(weekst),
   ste(weekste)],nonadv,[],[]).

a([e(weelderige),
   er(weelderiger),
   ere(weelderigere),
   no_e(weelderig),
   st(weelderigst),
   ste(weelderigste)],adv,[],[]).

a([e(weemoedige),
   er(weemoediger),
   ere(weemoedigere),
   no_e(weemoedig),
   st(weemoedigst),
   ste(weemoedigste)],adv,[],[]).

a([e(weerbare),
   er(weerbaarder),
   ere(weerbaardere),
   no_e(weerbaar),
   st(weerbaarst),
   ste(weerbaarste)],nonadv,
  [pp(tegen)],[]).

a([e(weerbarstige),
   er(weerbarstiger),
   ere(weerbarstigere),
   no_e(weerbarstig),
   st(weerbarstigst),
   ste(weerbarstigste)],adv,[],[]).

a([e(weergaloze),
   no_e(weergaloos)],adv,[],[]).

a([ge_both(weergegeven)],adv,[],[]).

a([ge_e(weergekeerde),
   ge_no_e(weergekeerd)],adv,[],[]).

a([ge_both(weerhouden)],padv,[],[]).

a([ge_e(weerkaatste),
   ge_no_e(weerkaatst)],adv,[],[]).

a([e(weerkundige),
   no_e(weerkundig)],adv,[],[]).

a([ge_e(weerlegde),
   ge_no_e(weerlegd)],adv,[],[]).

a([e(weerloze),
   er(weerlozer),
   ere(weerlozere),
   no_e(weerloos),
   st(weerloost),
   ste(weerlooste)],padv,
  [er_pp_sbar(tegen),
   pp(tegen)],[]).

a([e(weerspannige),
   er(weerspanniger),
   ere(weerspannigere),
   no_e(weerspannig),
   st(weerspannigst),
   ste(weerspannigste)],adv,[],[]).

a([ge_e(weerspiegelde),
   ge_no_e(weerspiegeld)],adv,[],[]).

a([ge_e(weerstane),
   ge_no_e(weerstaan)],adv,[],[]).

a([e(weerzinwekkende),
   er(weerzinwekkender),
   ere(weerzinwekkendere),
   no_e(weerzinwekkend),
   st(weerzinwekkendst),
   ste(weerzinwekkendste)],adv,[],[]).

a([ge_e(wegbezuinigde),
   ge_no_e(wegbezuinigd)],padv,[],[]).

a([ge_both(weggeblazen)],adv,[],[]).

a([ge_both(weggebleven)],adv,[],[]).

a([ge_both(weggeborgen)],adv,[],[]).

a([ge_e(weggebrachte),
   ge_no_e(weggebracht)],adv,[],[]).

a([ge_e(weggedane),
   ge_no_e(weggedaan)],adv,[],[]).

a([ge_both(weggedoken)],adv,[],[]).

a([ge_both(weggedragen)],adv,[],[]).

a([ge_both(weggedreven)],adv,[],[]).

a([ge_e(weggedrukte),
   ge_no_e(weggedrukt)],adv,[],[]).

a([ge_e(weggeduwde),
   ge_no_e(weggeduwd)],adv,[],[]).

a([ge_e(weggegane),
   ge_no_e(weggegaan)],adv,[],[]).

a([ge_both(weggegeven)],adv,[],[]).

a([ge_both(weggegleden)],adv,[],[]).

a([ge_e(weggegooide),
   ge_no_e(weggegooid)],adv,[],[]).

a([ge_e(weggehaalde),
   ge_no_e(weggehaald)],adv,[],[]).

a([ge_both(weggehouden)],adv,[],[]).

a([ge_e(weggejaagde),
   ge_no_e(weggejaagd)],adv,[],[]).

a([ge_e(weggehoonde),
   ge_no_e(weggehoond)],adv,[],[]).

a([ge_both(weggekomen)],adv,[],[]).

a([ge_both(weggekropen)],adv,[],[]).

a([ge_e(weggekwijnde),
   ge_no_e(weggekwijnd)],adv,[],[]).

a([ge_both(weggelaten)],adv,[],[]).

a([ge_e(weggelegde),
   ge_no_e(weggelegd)],adv,[],[]).

a([e(weggelekte),
   no_e(weggelekt)],padv,[],[]).

a([ge_both(weggelopen)],adv,[],[]).

a([ge_e(weggemoffelde),
   ge_no_e(weggemoffeld)],adv,[],[]).

a([ge_both(weggenomen)],adv,[],[]).

a([ge_e(weggepeste),
   ge_no_e(weggepest)],padv,[],[]).

a([ge_both(weggereden)],adv,[],[]).

a([ge_e(weggerende),
   ge_no_e(weggerend)],adv,[],[]).

a([ge_e(weggerukte),
   ge_no_e(weggerukt)],adv,[],[]).

a([ge_both(weggeschoten)],adv,[],[]).

a([ge_both(weggeschoven)],adv,[],[]).

a([ge_both(weggeslagen)],adv,[],[]).

a([ge_e(weggesleepte),
   ge_no_e(weggesleept)],adv,[],[]).

a([ge_both(weggesmolten)],adv,[],[]).

a([ge_both(weggesneden)],adv,[],[]).

a([ge_e(weggespeelde),
   ge_no_e(weggespeeld)],adv,[],[]).

a([ge_e(weggespoelde),
   ge_no_e(weggespoeld)],adv,[],[]).

a([ge_e(weggestemde),
   ge_no_e(weggestemd)],padv,[],[]).

a([ge_e(weggestopte),
   ge_no_e(weggestopt)],adv,[],[]).

a([ge_both(weggestorven)],adv,[],[]).

a([ge_e(weggestuurde),
   ge_no_e(weggestuurd)],adv,[],[]).

a([ge_both(weggetrokken)],adv,[],[]).

a([ge_e(weggevaagde),
   ge_no_e(weggevaagd)],adv,[],[]).

a([ge_both(weggevallen)],adv,[],[]).

a([both(weggevaren)],adv,[],[]).

a([ge_e(weggeveegde),
   ge_no_e(weggeveegd)],adv,[],[]).

a([ge_e(weggevloeide),
   ge_no_e(weggevloeid)],adv,[],[]).

a([ge_both(weggevlogen)],adv,[],[]).

a([ge_e(weggevluchte),
   ge_no_e(weggevlucht)],adv,[],[]).

a([ge_e(weggevoerde),
   ge_no_e(weggevoerd)],adv,[],[]).

a([ge_e(weggewerkte),
   ge_no_e(weggewerkt)],adv,[],[]).

a([ge_both(weggeworpen)],adv,[],[]).

a([ge_e(weggezakte),
   ge_no_e(weggezakt)],padv,[],[]).

a([ge_e(weggezette),
   ge_no_e(weggezet)],adv,[],[]).

a([ge_both(weggezogen)],padv,[],[]).

a([ge_both(weggezonken)],padv,[],[]).

a([ge_e(weggeëbde),
   ge_no_e(weggeëbd)],adv,[],[]).

a([ende(wegvarende),
   end(wegvarend)],adv,[],[]).

a([both(wegwerp)],nonadv,[],[]).

a([e(wegwijze),
   no_e(wegwijs)],nonadv,[],[]).

a([e(weidse),
   er(weidser),
   ere(weidsere),
   no_e(weids),
   st(weidst),
   ste(weidste)],adv,[],[]).

a([no_e(weinig),e(weinige)],odet_adv,[],[]).

a([e(wekelijkse),
   no_e(wekelijks)],adv,[],[]).

a([pred(wel)],adv,[],[]).

a([both(welbegrepen)],nonadv,[],[]).

a([e(welbekende),
   postn_no_e(welbekend)],nonadv,
  [subject_sbar,
   pp(bij),
   so_np],[]).

a([e(welbeschouwde),
   no_e(welbeschouwd)],adv,[],[]).

a([e(welbespraakte),
   no_e(welbespraakt)],adv,[],[]).

a([e(weldadige),
   er(weldadiger),
   ere(weldadigere),
   no_e(weldadig),
   st(weldadigst),
   ste(weldadigste)],adv,[],[]).

a([e(weldenkende),
   no_e(weldenkend)],nonadv,[],[]).

a([e(weldoordachte),
   no_e(weldoordacht)],nonadv,[],[]).

a([e(weldoorvoede),
   no_e(weldoorvoed)],nonadv,[],[]).

a([e(welgemeende),
   no_e(welgemeend)],adv,[],[]).

a([e(welgestelde),
   no_e(welgesteld)],nonadv,[],[]).

a([e(welgevallige),
   er(welgevalliger),
   ere(welgevalligere),
   no_e(welgevallig),
   st(welgevalligst),
   ste(welgevalligste)],nonadv,
  [so_np],[]).

a([both(welgevaren)],adv,[],[]).

a([e(welgevormde),
   no_e(welgevormd)],nonadv,[],[]).

a([e(welgezinde),
   no_e(welgezind)],nonadv,
  [so_np],[]).

a([e(welige),
   er(weliger),
   ere(weligere),
   no_e(welig),
   st(weligst),
   ste(weligste)],adv,[],[]).

a([e(welkome),
   no_e(welkom)],nonadv,
  [subject_sbar,  % het is welkom dat ..
   object_vp],    % je bent welkom om ..
  []).

a([pred(welletjes)],nonadv,[],[]).

a([e(welluidende),
   no_e(welluidend)],adv,[],[]).

a([e(wellustige),
   er(wellustiger),
   ere(wellustigere),
   no_e(wellustig),
   st(wellustigst),
   ste(wellustigste)],adv,[],[]).

a([e(welopgevoede),
   no_e(welopgevoed)],nonadv,[],[]).

a([both(weloverwogen)],adv,[],[]).

a([e(welsprekende),
   er(welsprekender),
   ere(welsprekendere),
   no_e(welsprekend),
   st(welsprekendst),
   ste(welsprekendste)],adv,[],[]).

a([e(welvarende),
   no_e(welvarend)],adv,[],[]).

a([e(welverdiende),
   no_e(welverdiend)],nonadv,[],[]).

a([ende(welvende),
   end(welvend)],both,
  [refl],[]).

a([e(welwillende),
   er(welwillender),
   ere(welwillendere),
   no_e(welwillend),
   st(welwillendst),
   ste(welwillendste)],adv,[subject_sbar,
			    subject_vp],[on]).

a([e(wendbare),
   no_e(wendbaar)],nonadv,[],[]).

a([e(wenselijke),
   er(wenselijker),
   ere(wenselijkere),
   no_e(wenselijk),
   st(wenselijkst),
   ste(wenselijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(wereldberoemde),
   no_e(wereldberoemd)],nonadv,
  [pp(om)],[]).

a([e(wereldkundige),
   no_e(wereldkundig)],nonadv,[],[]).

a([e(wereldlijke),
   no_e(wereldlijk)],nonadv,[],[]).

a([e(wereldomvattende),
   no_e(wereldomvattend)],nonadv,[],[]).

a([e(wereldse),
   no_e(werelds),
   er(wereldser),
   ere(wereldsere),
   st(wereldst),
   ste(wereldste)],nonadv,[],[]).

a([e(wereldvreemde),
   er(wereldvreemder),
   ere(wereldvreemdere),
   no_e(wereldvreemd),
   st(wereldvreemdst),
   ste(wereldvreemdste)],nonadv,[],[]).

a([e(wereldwijde),e([wereld,wijde]),
   postn_no_e(wereldwijd),postn_no_e([wereld,wijd])
  ],adv,[],[]).

a([e(werkbare),
   no_e(werkbaar)],nonadv,[],[on]).

a([e(werkelijke),
   no_e(werkelijk)],adv,[],[]).

a([e(werkeloze),
   no_e(werkeloos)],padv,[],[]).

a([e(werkloze),
   no_e(werkloos)],padv,[],[]).

a([e(werktuiglijke),
   er(werktuiglijker),
   ere(werktuiglijkere),
   no_e(werktuiglijk),
   st(werktuiglijkst),
   ste(werktuiglijkste)],adv,[],[]).

a([e(werkwillige),
   no_e(werkwillig)],padv,[],[]).

a([e(werkzame),
   er(werkzamer),
   ere(werkzamere),
   no_e(werkzaam),
   st(werkzaamst),
   ste(werkzaamste)],adv,
  [subject_vp],[]).

a([ende(werkzoekende),
   end(werkzoekend)],nonadv,[],[]).

a([pred(west)],nonadv,[],[]).

a([e(westelijke),
   er(westelijker),
   ere(westelijkere),
   no_e(westelijk),
   st(westelijkst),
   ste(westelijkste)],adv,[],[]).

a([e(westerse),
   er(westerser),
   ere(westersere),
   no_e(westers),
   st(westerst),
   ste(westerste)],adv,[],
  [h(niet),
   h(pro),
   h(anti)]).

a([e(westwaartse),
   postn_no_e(westwaarts)],diradv,[],[]).

a([e(wetenschappelijke),
   er(wetenschappelijker),
   ere(wetenschappelijkere),
   no_e(wetenschappelijk),
   st(wetenschappelijkst),
   ste(wetenschappelijkste)],adv,[],[h(populair)]).

a([e(wetgevende),
   no_e(wetgevend)],padv,[],[]).

a([e(wetmatige),
   no_e(wetmatig)],adv,[],[]).

a([e(wettelijke),
   no_e(wettelijk)],adv,[],[boven]).

a([e(wettige),
   no_e(wettig)],adv,[],[]).

a([e(wezenlijke),
   ere(wezenlijkere),
   no_e(wezenlijk),
   er(wezenlijker)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(wezenloze),
   er(wezenlozer),
   ere(wezenlozere),
   no_e(wezenloos),
   st(wezenloost),
   ste(wezenlooste)],adv,[],[]).

a([e(wijde),
   er(wijder),
   ere(wijdere),
   no_e(wijd),
   st(wijdst),
   ste(wijdste)],adv,[],
  []).

a([e(wijdbeense),
   no_e(wijdbeens)],padv,[],[]).

a([e(wijdse),
   no_e(wijds)],nonadv,[],[]).

a([e(wijdverbreide),
   no_e(wijdverbreid)],adv,[],[]).

a([both(wijlen)],nonadv,[],[]).

a([e(wijnrode),
   er(wijnroder),
   ere(wijnrodere),
   no_e(wijnrood),
   st(wijnroodst),
   ste(wijnroodste)],nonadv,[],[]).

a([e(wijze),
   er(wijzer),
   ere(wijzere),
   no_e(wijs),
   st(wijst),
   ste(wijste)],adv,
  [subject_vp],[]).

a([ge_e(wijsgemaakte),
   ge_no_e(wijsgemaakt)],adv,[],[]).

a([e(wijsgerige),
   no_e(wijsgerig)],adv,[],[]).

a([e(wilde),
   er(wilder),
   ere(wildere),
   no_e(wild),
   st(wildst),
   ste(wildste)],adv,
  [er_pp_vp(op),
   pp(op),
   pp(van), % ze zijn er wild van
   pp(over)],[]).

a([e(willekeurige),
   er(willekeuriger),
   ere(willekeurigere),
   no_e(willekeurig),
   st(willekeurigst),
   ste(willekeurigste)],adv,[],[]).

a([e(willige),
   er(williger),
   ere(willigere),
   no_e(willig),
   st(willigst),
   ste(willigste)],adv,[],[]).

a([e(willoze),
   er(willozer),
   ere(willozere),
   no_e(willoos),
   st(willoost),
   ste(willooste)],adv,[],[]).

a([e(winderige),
   er(winderiger),
   ere(winderigere),
   no_e(winderig),
   st(winderigst),
   ste(winderigste)],nonadv,[],[]).

a([e(windstille),
   no_e(windstil)],nonadv,[],[]).

a([e(winstgevende),
   er(winstgevender),
   ere(winstgevendere),
   no_e(winstgevend),
   st(winstgevendst),
   ste(winstgevendste)],padv,[],[]).

a([e(winterharde),
   no_e(winterhard)],nonadv,[],[]).

a([pred(winterklaar)],nonadv,[],[]). % met neutronenkorrels

a([e(winterse),
   no_e(winters)],adv,[],[]).

a([e(wisse),
   er(wisser),
   ere(wissere),
   no_e(wis),
   st(wist),
   ste(wiste)],adv,[],[]).

a([e(wiskundige),
   no_e(wiskundig)],adv,[],[]).

a([e(wispelturige),
   er(wispelturiger),
   ere(wispelturigere),
   no_e(wispelturig),
   st(wispelturigst),
   ste(wispelturigste)],adv,[],[]).

a([e(wisselende),
   er(wisselender),
   ere(wisselendere),
   no_e(wisselend),
   st(wisselendst),
   ste(wisselendste)],adv,[],[]).

a([e(wisselvallige),
   er(wisselvalliger),
   ere(wisselvalligere),
   no_e(wisselvallig),
   st(wisselvalligst),
   ste(wisselvalligste)],adv,[],[]).

a([e(witte),
   er(witter),
   ere(wittere),
   no_e(wit),
   st(witst),
   ste(witste)],adv,[],
  [h(rood),
   h(blauw),
   h(zwart)]).

a([e(witgekalkte),
   no_e(witgekalkt)],nonadv,[],[]).

a([ge_both(witgewassen)],padv,[],[]).

a([e(woedende),
   er(woedender),
   ere(woedendere),
   no_e(woedend),
   st(woedendst),
   ste(woedendste)],padv,
  [er_pp_sbar(over),
   object_sbar,
   pp(op),
   pp(over)],[]).

a([e(woelige),
   er(woeliger),
   ere(woeligere),
   no_e(woelig),
   st(woeligst),
   ste(woeligste)],adv,[],[]).

a([e(woensdagse),
   no_e(woensdags)],adv,[],[]).

a([e(woeste),
   er(woester),
   ere(woestere),
   no_e(woest)],adv,
  [object_sbar,
   er_pp_sbar(over),
   pp(om),
   pp(op),
   pp(over)],[]).

a([stof(wollen)],nonadv,[],[]).

a([e(wollige),
   er(wolliger),
   ere(wolligere),
   no_e(wollig),
   st(wolligst),
   ste(wolligste)],adv,[],[]).

a([e(wondere),
   no_e(wonder)],adv,[],[]).

a([e(wonderbare),
   er(wonderbaarder),
   ere(wonderbaardere),
   no_e(wonderbaar),
   st(wonderbaarst),
   ste(wonderbaarste)],adv,[],[]).

a([e(wonderbaarlijke),
   er(wonderbaarlijker),
   ere(wonderbaarlijkere),
   no_e(wonderbaarlijk),
   st(wonderbaarlijkst),
   ste(wonderbaarlijkste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(wonderlijke),
   er(wonderlijker),
   ere(wonderlijkere),
   no_e(wonderlijk),
   st(wonderlijkst),
   ste(wonderlijkste)],adv,
  [subject_sbar],[]).

a([e(wondermooie),
   no_e(wondermooi)],adv,[],[]).

a([e(wonderschone),
   no_e(wonderschoon)],adv,[],[]).

a([ende(woningzoekende),
   end(woningzoekend)],nonadv,[],[]).

a([e(woonachtige),
   postn_no_e(woonachtig)],nonadv,[],[]).

a([e(woordelijke),
   no_e(woordelijk)],adv,[],[]).

a([e(woordeloze),
   no_e(woordeloos)],adv,[],[]).

a([stof([would,be]),
   stof('would-be')],nonadv,[],[]).

a([e(wraakzuchtige),
   er(wraakzuchtiger),
   ere(wraakzuchtigere),
   no_e(wraakzuchtig),
   st(wraakzuchtigst),
   ste(wraakzuchtigste)],adv,[],[]).

a([e(wrange),
   er(wranger),
   ere(wrangere),
   no_e(wrang),
   st(wrangst),
   ste(wrangste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(wrede),
   er(wreder),
   ere(wredere),
   no_e(wreed),
   st(wreedst),
   ste(wreedste)],adv,
  [pp(tegen),
   subject_sbar,
   subject_vp],[]).

a([e(wrevelige),
   er(wreveliger),
   ere(wreveligere),
   no_e(wrevelig),
   st(wreveligst),
   ste(wreveligste)],adv,
  [pp(over)],[]).

a([e(wufte),
   no_e(wuft)],nonadv,[],[]).

a([e(wulpse),
   er(wulpser),
   ere(wulpsere),
   no_e(wulps),
   st(wulpst),
   ste(wulpste)],adv,[],[]).

a([e(xenofobe),
   no_e(xenofoob)],adv,[],[]).

a([e(zachte),
   er(zachter),
   ere(zachtere),
   no_e(zacht),
   st(zachtst),
   ste(zachtste)],adv,[],[]).

a([e(zachtaardige),
   er(zachtaardiger),
   ere(zachtaardigere),
   no_e(zachtaardig),
   st(zachtaardigst),
   ste(zachtaardigste)],adv,[],[]).

a([pred(zachtjes)],adv,[],[]).

a([pred([zachtjes,aan])],adv,[],[]).

a([e(zachtmoedige),
   er(zachtmoediger),
   ere(zachtmoedigere),
   no_e(zachtmoedig),
   st(zachtmoedigst),
   ste(zachtmoedigste)],adv,[],[]).

a([e(zachtzinnige),
   er(zachtzinniger),
   ere(zachtzinnigere),
   no_e(zachtzinnig),
   st(zachtzinnigst),
   ste(zachtzinnigste)],adv,[],[]).

a([e(zakelijke),
   er(zakelijker),
   ere(zakelijkere),
   no_e(zakelijk),
   st(zakelijkst),
   ste(zakelijkste)],adv,[],[]).

a([e(zalige),
   er(zaliger),
   ere(zaligere),
   no_e(zalig),
   st(zaligst),
   ste(zaligste)],adv,
  [subject_vp],[]).

a([e(zaligmakende),
   no_e(zaligmakend)],nonadv,
  [subject_vp,
   subject_sbar],[]).

a([e(zanderige),
   er(zanderiger),
   ere(zanderigere),
   no_e(zanderig),
   st(zanderigst),
   ste(zanderigste)],nonadv,[],[]).

a([e(zandige),
   er(zandiger),
   ere(zandigere),
   no_e(zandig),
   st(zandigst),
   ste(zandigste)],adv,[],[]).

a([e(zangerige),
   er(zangeriger),
   ere(zangerigere),
   no_e(zangerig),
   st(zangerigst),
   ste(zangerigste)],adv,[],[]).

a([e(zatte),
   no_e(zat)],both,[],[ladder]).

a([e(zaterdagse),
   no_e(zaterdags)],adv,[],[]).

a([e(zedelijke),
   er(zedelijker),
   ere(zedelijkere),
   no_e(zedelijk),
   st(zedelijkst),
   ste(zedelijkste)],adv,[],[on]).

a([e(zedige),
   er(zediger),
   ere(zedigere),
   no_e(zedig),
   st(zedigst),
   ste(zedigste)],adv,[],[]).

a([e(zeegroene),
   er(zeegroener),
   ere(zeegroenere),
   no_e(zeegroen),
   st(zeegroenst),
   ste(zeegroenste)],nonadv,[],[]).

a([e(zeewaartse),
   postn_no_e(zeewaarts)],diradv,[],[]).

a([e(zere),
   er(zeerder),
   ere(zeerdere),
   no_e(zeer),
   st(zeerst),
   ste(zeerste)],nonadv,[],[]).

a([both(zelden),
   er(zeldener)],osentadv,[],[]).

a([e(zekere),
   er(zekerder),
   ere(zekerdere),
   no_e(zeker),
   st(zekerst),
   ste(zekerste)],adv,
  [subject_sbar_no_het,
   object_sbar,
   er_pp_sbar(van),
   er_pp_vp(van),
   pp(van)],[]).

a([e(zeldzame),
   er(zeldzamer),
   ere(zeldzamere),
   no_e(zeldzaam),
   st(zeldzaamst),
   ste(zeldzaamste)],adv,[],[]).

a([e('zelf-bewuste'),
   e(zelfbewuste),
   er('zelf-bewuster'),
   er(zelfbewuster),
   ere('zelf-bewustere'),
   ere(zelfbewustere),
   no_e('zelf-bewust'),
   no_e(zelfbewust)],padv,[],[]).

a([stof(zelfde)],nonadv,[],[]).

a([both(zelfgekozen)],nonadv,[],[]).

a([no_e(zelfgemaakt),
   e(zelfgemaakte)],nonadv,[],[]).

a([end(zelfgenezend),
   ende(zelfgenezende)],padv,[],[]).

a([e(zelfgenoegzame),
   er(zelfgenoegzamer),
   ere(zelfgenoegzamere),
   no_e(zelfgenoegzaam),
   st(zelfgenoegzaamst),
   ste(zelfgenoegzaamste)],padv,[],[]).

a([e(zelfingenomene),
   er(zelfingenomener),
   ere(zelfingenomenere),
   no_e(zelfingenomen),
   st(zelfingenomenst),
   ste(zelfingenomenste)],padv,[],[]).

a([e(zelfstandige),
   er(zelfstandiger),
   ere(zelfstandigere),
   no_e(zelfstandig),
   st(zelfstandigst),
   ste(zelfstandigste)],adv,[],[]).

a([ge_e(zelfverklaarde),
   ge_no_e(zelfverklaard)],nonadv,
  [],[]).

a([e(zelfverzekerde),
   er(zelfverzekerder),
   ere(zelfverzekerdere),
   no_e(zelfverzekerd),
   st(zelfverzekerdst),
   ste(zelfverzekerdste)],adv,[],[]).

a([e(zelfvoldane),
   er(zelfvoldaner),
   ere(zelfvoldanere),
   no_e(zelfvoldaan),
   st(zelfvoldaanst),
   ste(zelfvoldaanste)],padv,[],[]).

a([e(zelfzuchtige),
   er(zelfzuchtiger),
   ere(zelfzuchtigere),
   no_e(zelfzuchtig),
   st(zelfzuchtigst),
   ste(zelfzuchtigste)],adv,[],[]).

a([stof(zemen)],nonadv,[],[]).

a([e(zenuwachtige),
   er(zenuwachtiger),
   ere(zenuwachtigere),
   no_e(zenuwachtig),
   st(zenuwachtigst),
   ste(zenuwachtigste)],adv,
  [er_pp_sbar(over),
   er_pp_sbar(van),
   er_pp_vp(over),
   pp(om),
   pp(over),
   pp(voor)],[]).

a([e(zenuwslopende),
   er(zenuwslopender),
   ere(zenuwslopendere),
   no_e(zenuwslopend),
   st(zenuwslopendst),
   ste(zenuwslopendste)],nonadv,[],[]).

a([both(zestiger)],nonadv,[],[]).

a([e(zeurderige),
   er(zeurderiger),
   ere(zeurderigere),
   no_e(zeurderig),
   st(zeurderigst),
   ste(zeurderigste)],adv,[],[]).

a([both(zeventiger)],nonadv,[],[]).

a([e(zichtbare),
   er(zichtbaarder),
   ere(zichtbaardere),
   no_e(zichtbaar),
   st(zichtbaarst),
   ste(zichtbaarste)],adv,
  [pp(voor),
   subject_sbar_no_het],[]).

a([e(zieke),
   er(zieker),
   ere(ziekere),
   no_e(ziek),
   st(ziekst),
   ste(ziekste)],padv,[pp(van)],[zee]).

a([e(ziekelijke),
   er(ziekelijker),
   ere(ziekelijkere),
   no_e(ziekelijk),
   st(ziekelijkst),
   ste(ziekelijkste)],adv,[],[]).

a([e(ziekmakende),
   no_e(ziekmakend)],nonadv,[],[]).

a([pred(ziekjes)],adv,[],[]).

a([e(zielige),
   er(zieliger),
   ere(zieligere),
   no_e(zielig),
   st(zieligst),
   ste(zieligste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([both(zigzag)],adv,[],[]).

a([e(zijdeachtige),
   er(zijdeachtiger),
   ere(zijdeachtigere),
   no_e(zijdeachtig),
   st(zijdeachtigst),
   ste(zijdeachtigste)],nonadv,[],[]).

a([e(zijdelingse),
   no_e(zijdelings)],adv,[],[]).

a([stof(zijden)],nonadv,[],[]).

a([e(zijwaartse),
   postn_no_e(zijwaarts)],diradv,[],[]).

a([e(zilte),
   no_e(zilt)],nonadv,[],[]).

a([e(zilverachtige),
   er(zilverachtiger),
   ere(zilverachtigere),
   no_e(zilverachtig),
   st(zilverachtigst),
   ste(zilverachtigste)],nonadv,[],[]).

a([stof(zilveren)],nonadv,[],[]).

a([e(zilverkleurige),
   er(zilverkleuriger),
   ere(zilverkleurigere),
   no_e(zilverkleurig),
   st(zilverkleurigst),
   ste(zilverkleurigste)],nonadv,[],[]).

a([e(zilverwitte),
   no_e(zilverwit)],nonadv,[],[]).

a([e(zindelijke),
   er(zindelijker),
   ere(zindelijkere),
   no_e(zindelijk),
   st(zindelijkst),
   ste(zindelijkste)],adv,[],[]).

a([stof(zinken)],nonadv,[],[]).

a([e(zinloze),
   er(zinlozer),
   ere(zinlozere),
   no_e(zinloos),
   st(zinloost),
   ste(zinlooste)],adv,
  [subject_sbar,
   subject_vp],[]).

a([e(zinnelijke),
   er(zinnelijker),
   ere(zinnelijkere),
   no_e(zinnelijk),
   st(zinnelijkst),
   ste(zinnelijkste)],adv,[],[]).

a([e(zinnige),
   er(zinniger),
   ere(zinnigere),
   no_e(zinnig),
   st(zinnigst),
   ste(zinnigste)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(zintuigelijke),
   no_e(zintuigelijk)],nonadv,[],[]).

a([e(zintuiglijke),
   er(zintuiglijker),
   ere(zintuiglijkere),
   no_e(zintuiglijk),
   st(zintuiglijkst),
   ste(zintuiglijkste)],adv,[],[]).

a([e(zinvolle),
   er(zinvoller),
   ere(zinvollere),
   no_e(zinvol),
   st(zinvolst),
   ste(zinvolste)],adv,
  [subject_sbar,
   subject_vp,
   pp(voor)],[]).

a([e(zionistische),
   er(zionistischer),
   ere(zionistischere),
   no_e(zionistisch),
   st(zionistischt),
   ste(zionistischte)],nonadv,[],[]).

a([pred(zo)],nonadv,
  [subject_sbar],[]).

a([e(zodanige),
   no_e(zodanig)],adv,[],[]).

a([pred(zoek)],nonadv,[],[]).

a([ende(zoekende),
   end(zoekend)],adv,
  [pp(naar),
   transitive],[]).

a([ge_e(zoekgeraakte),
   ge_no_e(zoekgeraakt)],padv,[],[]).

a([e(zoele),
   er(zoeler),
   ere(zoelere),
   no_e(zoel),
   st(zoelst),
   ste(zoelste)],adv,[],[]).

a([e(zoete),
   er(zoeter),
   ere(zoetere),
   no_e(zoet),
   st(zoetst),
   ste(zoetste)],nonadv,[],[]).

a([e(zoetgevooisde),
   no_e(zoetgevooisd)],nonadv,[],[]).

a([e(zoetige),
   er(zoetiger),
   ere(zoetigere),
   no_e(zoetig),
   st(zoetigst),
   ste(zoetigste)],nonadv,[],[]).

a([ge_both(zogeheten)],nonadv,[],[]).

a([ge_e(zogenaamde),
   ge_no_e(zogenaamd)],adv,[],[]).

a([ge_e(zogenoemde),
   ge_no_e(zogenoemd)],nonadv,[],[]).

a([ge_e(zogezegde),
   ge_no_e(zogezegd)],adv,[],[]).

a([e(zomerse),
   er(zomerser),
   ere(zomersere),
   no_e(zomers)],adv,[],[]).

a([e(zondagse),
   no_e(zondags)],adv,[],[]).

a([pred(zonde)],nonadv,
  [subject_sbar,
   subject_vp],[]).

a([e(zonderlijke),
   no_e(zonderlijk)],adv,[],[]).

a([e(zonderlinge),
   er(zonderlinger),
   ere(zonderlingere),
   no_e(zonderling),
   st(zonderlingst),
   ste(zonderlingste)],adv,[],[]).

a([e(zondige),
   er(zondiger),
   ere(zondigere),
   no_e(zondig),
   st(zondigst),
   ste(zondigste)],adv,[],[]).

a([e(zonneklare),
   no_e(zonneklaar)],adv,
  [subject_sbar_no_het],[]).

a([e(zonnige),
   er(zonniger),
   ere(zonnigere),
   no_e(zonnig),
   st(zonnigst),
   ste(zonnigste)],adv,[],[]).

a([both(zonovergoten)],nonadv,[],[]).

a([e(zorgelijke),
   er(zorgelijker),
   ere(zorgelijkere),
   no_e(zorgelijk),
   st(zorgelijkst),
   ste(zorgelijkste)],adv,
  [subject_sbar,
   subject_vp,
   pp(over)],[]).

a([e(zorgeloze),
   er(zorgelozer),
   ere(zorgelozere),
   no_e(zorgeloos),
   st(zorgeloost),
   ste(zorgelooste)],adv,[],[]).

a([e(zorgvuldige),
   er(zorgvuldiger),
   ere(zorgvuldigere),
   no_e(zorgvuldig),
   st(zorgvuldigst),
   ste(zorgvuldigste)],adv,
  [subject_vp,
   pp(bij),
   pp(in),
   pp(met)],[]).

a([e(zorgwekkende),
   no_e(zorgwekkend),
   er(zorgwekkender),
   ere(zorgwekkendere),
   st(zorgwekkendst),
   ste(zorgwekkendste)],adv,
  [subject_sbar],[]).

a([e(zorgzame),
   er(zorgzamer),
   ere(zorgzamere),
   no_e(zorgzaam),
   st(zorgzaamst),
   ste(zorgzaamste)],adv,
  [pp(voor)],[]).

a([e(zotte),
   er(zotter),
   ere(zottere),
   no_e(zot),
   st(zotst),
   ste(zotste)],adv,[],[]).

a([e(zoute),
   er(zouter),
   ere(zoutere),
   no_e(zout),
   st(zoutst),
   ste(zoutste)],nonadv,[],[]).

a([e(zouteloze),
   no_e(zouteloos)],nonadv,[],[]).

a([no_e(zoveel),
   e(zovele)],odet_adv,[],[]).

a([pred(zover),
   pred([zo,ver])],locadv,[],[]).

a([pred(zuid)],nonadv,[],[]).

a([e(zuidelijke),
   er(zuidelijker),
   ere(zuidelijkere),
   no_e(zuidelijk),
   st(zuidelijkst),
   ste(zuidelijkste)],adv,[],[]).

a([e(zuidwaartse),
   postn_no_e(zuidwaarts)],diradv,[],[]).

a([e(zuidwestelijke),
   er(zuidwestelijker),
   ere(zuidwestelijkere),
   no_e(zuidwestelijk),
   st(zuidwestelijkst),
   ste(zuidwestelijkste)],adv,[],[]).

a([e(zuinige),
   er(zuiniger),
   ere(zuinigere),
   no_e(zuinig),
   st(zuinigst),
   ste(zuinigste)],adv,
  [pp(met),
   pp(op)],[]).

a([pred(zuinigjes)],adv,[],[]).

a([e(zuivere),
   er(zuiverder),
   ere(zuiverdere),
   no_e(zuiver),
   st(zuiverst),
   ste(zuiverste)],adv,[],[ras]).

a([e(zurige),
   er(zuriger),
   ere(zurigere),
   no_e(zurig),
   st(zurigst),
   ste(zurigste)],nonadv,[],[]).

a([e(zure),
   er(zuurder),
   ere(zuurdere),
   no_e(zuur),
   st(zuurst),
   ste(zuurste)],adv,[],[]).

a([e(zware),
   er(zwaarder),
   ere(zwaardere),
   no_e(zwaar),
   st(zwaarst),
   ste(zwaarste)],adv,[subject_vp],[super]).

a([both(zwaarbevochten)],nonadv,[],[]).

a([e(zwaargebouwde),
   no_e(zwaargebouwd)],nonadv,[],[]).

a([e(zwaargewonde),
   no_e(zwaargewond)],padv,[],[]).

a([e(zwaarlijvige),
   er(zwaarlijviger),
   ere(zwaarlijvigere),
   no_e(zwaarlijvig),
   st(zwaarlijvigst),
   ste(zwaarlijvigste)],nonadv,[],[]).

a([e(zwaarmoedige),
   er(zwaarmoediger),
   ere(zwaarmoedigere),
   no_e(zwaarmoedig),
   st(zwaarmoedigst),
   ste(zwaarmoedigste)],adv,
  [pp(over)],[]).

a([e(zwaarwegende),
   er(zwaarwegender),
   ere(zwaarwegendere),
   no_e(zwaarwegend),
   st(zwaarwegendst),
   ste(zwaarwegendste)],nonadv,[],[]).

a([e(zwaarwichtige),
   er(zwaarwichtiger),
   ere(zwaarwichtigere),
   no_e(zwaarwichtig),
   st(zwaarwichtigst),
   ste(zwaarwichtigste)],adv,[],[]).

a([pred(zwakjes)],adv,[],[]).

a([e(zwakke),
   er(zwakker),
   ere(zwakkere),
   no_e(zwak),
   st(zwakst),
   ste(zwakste)],adv,
  [subject_sbar],[]).

a([e(zwakzinnige),
   er(zwakzinniger),
   ere(zwakzinnigere),
   no_e(zwakzinnig),
   st(zwakzinnigst),
   ste(zwakzinnigste)],nonadv,[],[]).

a([e(zwangere),
   no_e(zwanger)],padv,
  [pp(van)],[hoog]).

a([e(zwarte),
   er(zwarter),
   ere(zwartere),
   no_e(zwart),
   st(zwartst),
   ste(zwartste)],adv,[],
  [blauw,
   git,
   inkt,
   kool,
   pik]).

a([both('zwart-wit'),
   e('zwart-witte')],adv,[],[]).

a([e(zwartgeblakerde),
   no_e(zwartgeblakerd)],padv,[],[]).

a([e(zweterige),
   er(zweteriger),
   ere(zweterigere),
   no_e(zweterig),
   st(zweterigst),
   ste(zweterigste)],padv,[],[]).

a([e(zweverige),
   er(zweveriger),
   ere(zweverigere),
   no_e(zweverig),
   st(zweverigst),
   ste(zweverigste)],padv,[],[]).

a([e(zwierige),
   er(zwieriger),
   ere(zwierigere),
   no_e(zwierig),
   st(zwierigst),
   ste(zwierigste)],adv,[],[]).

a([e(zwijgzame),
   er(zwijgzamer),
   ere(zwijgzamere),
   no_e(zwijgzaam),
   st(zwijgzaamst),
   ste(zwijgzaamste)],padv,
  [er_pp_sbar(over),
   pp(over)],[]).

a([e(zwoele),
   er(zwoeler),
   ere(zwoelere),
   no_e(zwoel),
   st(zwoelst),
   ste(zwoelste)],adv,[],[]).
