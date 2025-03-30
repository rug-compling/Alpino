:- expects_dialect(sicstus).

%% exceptions:

m(wacht_af,
  noun(het,mass,sg,subject_sbar),
  afwachten).                   % of dat echt lukt, is afwachten

m(bekvecht,ge_v_noun(intransitive),gebekvecht).
m(chat,ge_v_noun(intransitive),gechat).
m(dut,ge_v_noun(intransitive),gedut).
m(feest,ge_v_noun(intransitive),gefeest).
m(hand_klap,ge_v_noun(intransitive),handgeklap). % niet hand_gek_lap
m(keet,ge_v_noun(intransitive),gekeet).
m(praat,ge_v_noun(intransitive),gepraat).
m(proest,ge_v_noun(intransitive),geproest).

%% normal cases:
m(Stem,Tag,Surf) :-
    n(Forms,Gender,Subcat,Suffixes),
    n0(Stem,Tag,Surf,Forms,Gender,Subcat,Suffixes),
    \+ more_general_entry(Stem,Tag,Surf,Forms,Gender,Subcat,Suffixes).

more_general_entry(Stem,Tag,Surf,Forms,Gender,Subcat,Suffixes) :-
    n0(Stem,GeneralTag,Surf,Forms,Gender,Subcat,Suffixes),
    more_general(GeneralTag,Tag).

more_general(tmp_noun(De,Count,Sg),          noun(De,Count,Sg)).
more_general(mod_noun(De,Count,Sg),          noun(De,Count,Sg)).
more_general(meas_mod_noun(De,Count,Sg),     noun(De,Count,Sg)).
more_general(mod_noun(De,Count,Sg),          meas_mod_noun(De,Count,Sg)).
more_general(mod_noun(De,Count,Sg),          tmp_noun(De,Count,Sg)).

more_general(tmp_noun(De,Count,Sg,Sc),       noun(De,Count,Sg,Sc)).
more_general(mod_noun(De,Count,Sg,Sc),       noun(De,Count,Sg,Sc)).
more_general(meas_mod_noun(De,Count,Sg,Sc),  noun(De,Count,Sg,Sc)).
more_general(mod_noun(De,Count,Sg,Sc),       meas_mod_noun(De,Count,Sg,Sc)).
more_general(mod_noun(De,Count,Sg,Sc),       tmp_noun(De,Count,Sg,Sc)).

%%% NOW: choose stem with same number of parts
%%% so, 'pop up' does not get stem 'pop-up' but rather 'pop up'?
n0(Stem,Tag,Surf,Forms,Gender,Subcat,Suffixes) :-    
    select_form(Forms,Gender,Surf1,Tag0),
    candidate_stem(Stem0,Surf1,Forms),
    (	atom(Stem0)
    ->  Stem0 = Stem1
    ;   hdrug_util:concat_all(Stem0,Stem1,' ')
    ),
    select_subcat(Subcat,Tag0,Tag1),
    select_mod(Subcat,Tag1,Tag2),
    add_dim(Suffixes,Tag2,Tag,Stem1,Surf1,Stem2,Surf2),
    add_compounds:add_compounds(Suffixes,Stem2,Surf2,Stem,Surf),
    ignore(Forms,m(Stem,Tag,Surf)).

ignore(List,m(Root,Cat,Surf)) :-
    \+ lists:member(ignore(m(Root,Cat,Surf)),List),
    \+ lists:member(ignore_stem(Root),List).

add_dim(_,Tag,Tag,Stem,Surf,Stem,Surf).
add_dim(List,Tag0,Tag,Stem0,_Surf0,Stem,Surf) :-
    lists:member(dim(Surf),List),
    add_dim_tag(Tag0,Tag,sg),
    atom_concat(Stem0,'_DIM',Stem).
add_dim(List,Tag0,Tag,Stem0,_Surf0,Stem,Surf) :-
    lists:member(dim(Surf1),List),
    atom_concat(Surf1,s,Surf),
    add_dim_tag(Tag0,Tag,pl),
    atom_concat(Stem0,'_DIM',Stem).

add_dim_tag(Tag0,Tag,Num) :-
    add_dim_tag(Tag0,_,Tag,Num).

%% NB: identical to the version in unknowns.pl, but that
%% is not available during lexicon compilation
add_dim_tag(noun(_,_,O),O,              noun(het,count,Num),    Num).
add_dim_tag(noun(_,_,O,Sc),O,           noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).
add_dim_tag(tmp_noun(_,_,O),O,          tmp_noun(het,count,Num),    Num).
add_dim_tag(tmp_noun(_,_,O,Sc),O,       tmp_noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).
add_dim_tag(mod_noun(_,_,O),O,          mod_noun(het,count,Num),    Num).
add_dim_tag(mod_noun(_,_,O,Sc),O,       mod_noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).
add_dim_tag(meas_mod_noun(_,_,O),O,     meas_mod_noun(het,count,Num),    Num).
add_dim_tag(meas_mod_noun(_,_,O,Sc),O,  meas_mod_noun(het,count,Num,Sc), Num) :- check_pred_pp(Sc,Num).

%% NB: identical to the version in unknowns.pl, but that
%% is not available during lexicon compilation
check_pred_pp(SC,Agr) :-
    (  (   SC = pred_pp(_)
       ;   SC = pred_pp(_,_)
       )
    -> Agr \= pl
    ;  (   SC = pred_pp_pl(_)
       ;   SC = pred_pp_pl(_,_)
       )
    -> Agr = pl
    ;  true
    ).    

select_form(Forms,Gen,W,noun(Gen,count,both)   ) :-
    lists:member(sg(W),Forms),
    lists:member(pl(W),Forms).
select_form(Forms,Gen,W,noun(Gen,both,sg)      ) :-
    lists:member(sg(W),Forms),
    lists:member(mass(W),Forms).
select_form(Forms,Gen,W,noun(Gen,count,sg)     ) :-
    lists:member(sg(W),Forms),
    \+ lists:member(pl(W),Forms),
    \+ lists:member(mass(W),Forms).
select_form(Forms,Gen,W,noun(Gen,count,pl)     ) :-
    lists:member(pl(W),Forms),
    \+ lists:member(sg(W),Forms).
select_form(Forms,Gen,W,noun(Gen,count,meas)     ) :-
    lists:member(meas(W),Forms).
select_form(Forms,Gen,W,noun(Gen,count,bare_meas)     ) :-
    lists:member(bare_meas(W),Forms).
select_form(Forms,Gen,W,noun(Gen,mass,sg)      ) :-
    lists:member(mass(W),Forms),
    \+ lists:member(sg(W),Forms).

select_subcat(_List,Tag,Tag).
select_subcat(List,noun(A,B,C), noun(A,B,C,D)) :-
    lists:member(D,List),
    D \= meas_mod,
    D \= temp_mod,
    D \= mod,
    check_pred_pp(D,C).

select_mod(List,Tag,Tag) :-
    \+ lists:member(temp_mod,List),
    \+ lists:member(meas_mod,List),
    \+ lists:member(mod,List).

select_mod(List,Tag0,Tag) :-
    Tag0 =.. [noun|Args],
    lists:member(El,List),
    adapt_mod_fun(El,Fun),
    Tag  =.. [Fun|Args].

adapt_mod_fun(mod,       mod_noun).
adapt_mod_fun(meas_mod,  meas_mod_noun).
adapt_mod_fun(temp_mod,  tmp_noun).


candidate_stem(Stem,Surf,Forms) :-
    (   lists:member(stem(Stem0),Forms),
	same_len(Stem0,Surf)
    ->  Stem0=Stem
    ;   lists:member(sg(Stem0),Forms),
	same_len(Stem0,Surf)
    ->  Stem0=Stem
    ;   lists:member(mass(Stem0),Forms),
	same_len(Stem0,Surf)
    ->  Stem0=Stem
    ;   lists:member(meas(Stem0),Forms),
	same_len(Stem0,Surf)
    ->  Stem0=Stem
    ;   lists:member(bare_meas(Stem0),Forms),
	same_len(Stem0,Surf)
    ->  Stem0=Stem
    ;   lists:member(pl(Stem0),Forms),
	same_len(Stem0,Surf)
    ->  Stem0=Stem
    ;   format(user_error,"No stem for ~w ~w~n",[Surf,Forms]),
        fail
    ).

same_len(X,Y) :-
    atom(X),
    atom(Y),
    !.
same_len([_|T],[_|T2]) :-
    same_len(T,T2).

:- discontiguous
    n/3,
    n/4.

n(A,B,C,[]) :-
    n(A,B,C).

%%% zucht; sommige politici kunnen dit productief denk ik
n([mass([buitenlands,en,defensiebeleid])],het,[]).

n([mass([buitenlands,en,veiligheidsbeleid])],het,[]).
n([mass(['buitenlands-',en,veiligheidsbeleid])],het,[]).

n([mass([buitenlands,',','veiligheids-',en,defensiebeleid])],het,[]).

n([mass([burgerlijk,en,handelsrecht])],het,[]).

n([mass([cultureel,en,milieugebied])],het,[]).

n([mass([economisch,en,begrotingsbeleid])],het,[]).

n([mass([economisch,en,handelsgebied])],het,[]).

n([mass([economisch,en,handelsbeleid])],het,[]).

n([mass([economisch,en,milieugebied])],het,[]).

n([mass([economisch,en,werkgelegenheidsbeleid])],het,[]).

n([mass([financieel,en,begrotingsbeleid])],het,[]).

n([mass([regionaal,en,cohesiebeleid])],het,[]).

n([mass([regionaal,en,landbouwbeleid])],het,[]).

n([mass([regionaal,en,structuurbeleid])],het,[]).

n([mass([sociaal,en,arbeidsmarktbeleid])],het,[]).

n([mass([sociaal,en,milieubeleid])],het,[]).

n([mass([sociaal,en,milieugebied])],het,[]).

n([mass([sociaal,en,werkgelegenheidsbeleid])],het,[]).

n([mass([technisch,en,beroepsonderwijs])],het,[]).

n([mass([voortgezet,en,beroepsonderwijs])],het,[]).

n([mass([hoger,en,beroepsonderwijs])],het,[]).

n([mass([speciaal,',',voortgezet,en,beroepsonderwijs])],het,[]).

n([mass(['basis-',',',voortgezet,en,beroepsonderwijs])],het,[]).

n([mass(['basis-',',',bijzonder,en,beroepsonderwijs])],het,[]).

n([sg([olympisch,en,wereldkampioene]),
   sg(['Olympisch',en,wereldkampioene])],de,[app_measure]).

n([sg([olympisch,en,wereldkampioen]),
   sg(['Olympisch',en,wereldkampioen])],de,[app_measure]).

n([sg([europees,en,wereldkampioen]),
   sg(['Europees',en,wereldkampioen])],de,[app_measure]).

n([sg([europees,en,wereldkampioene]),
   sg(['Europees',en,wereldkampioene])],de,[app_measure]).

%% ik heb een 7 voor geschiedenis maar een 1 voor grieks

n([sg('#')],both,[]).  % een hashtag is een woord, voorafgegaan door een #

n([sg('1')],de,[]).

n([sg('2')],de,[]).

n([sg('3')],de,[]).

n([sg('4')],de,[]).

n([sg('5')],de,[]).

n([sg('6')],de,[]).

n([sg('7')],de,[]).

n([sg('8')],de,[]).

n([sg('9')],de,[]).

n([sg('0')],de,[]).


n([mass('3-D')],both,[]).


n([sg(['50cc-klasse']),
   sg(['50cc','klasse']),
   sg(['50',cc,klasse])],de,[]).

n([sg(['125cc-klasse']),
   sg(['125cc','klasse']),
   sg(['125',cc,klasse])],de,[]).

n([sg(['250cc-klasse']),
   sg(['250cc','klasse']),
   sg(['250',cc,klasse])],de,[]).

n([sg(['500cc-klasse']),
   sg(['500cc','klasse']),
   sg(['500',cc,klasse])],de,[]).

n([sg('A4tje'),sg('A4-tje'),sg('A4\'tje'),
   pl('A4tjes'),pl('A4-tjes'),pl('A4\'tjes')],het,[]).

n([pl('Abchazen'),
   pl('Abchaziërs'),
   sg('Abchaziër')],de,[]).

n([sg('Achterhoeker'),pl('Achterhoekers')],de,[]).

n([mass('Achterhoeks')],het,[]).

n([sg('Afghaan'),pl('Afghanen')],de,[]).

n([mass('Afghaans')],het,[]).

n([pl('Afrikanen'),sg('Afrikaan')],de,[],
  [hh('Zuid'), i('Zuid-',zuid),  i('zuid-',zuid),
   hh('Noord'),i('Noord-',noord),i('noord-',noord),
   hh('Oost'), i('Oost-',oost),  i('oost-',oost),
   hh('West'), i('West-',west),  i('west-',west)
  ]).

n([mass('Afrikaans')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Afrikaner'),pl('Afrikaners')],de,[]).

n([sg('Ajacied'),pl('Ajacieden'),
   sg(ajacied),pl(ajacieden)],de,[],[h(oud)]).

n([sg('Albanees'),pl('Albanezen')],de,[],[h('Kosovo')]).

n([mass('Albanees')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Alphenaar'),pl('Alphenaren')],de,[]).

n([sg('Algerijn'),pl('Algerijnen')],de,[]).

n([mass('Algerijns')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Alkmaarder'),pl('Alkmaarders')],de,[]).

n([pl('Amerikanen'),sg('Amerikaan')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Amerikaans')],het,[]).

n([pl('Amsterdammers'),sg('Amsterdammer')],de,[]).

n([mass('Amsterdams')],het,[]).

n([sg('Anglicaan'),pl('Anglicanen'),
   sg(anglicaan),  pl(anglicanen)],de,[]).

n([sg(anw),pl(anw)],de,[]). %% het vak algemene natuurwetenschappen

n([sg('Angolees'),pl('Angolezen')],de,[]).

n([sg('AOW\'er'),pl('AOW\'ers'),
   sg('aow\'er'),pl('aow\'ers')],de,[]).

n([sg('Antilliaan'),pl('Antillianen')],de,[]).

n([sg('Antwerpenaar'),pl('Antwerpenaren')],de,[]).

n([mass('Antwerps')],het,[]).

n([pl('Arabieren'),sg('Arabier'),
   pl(arabieren),sg(arabier)],de,[]).

n([mass('Arabisch')],het,[]).

n([mass('Aramees')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Argentijn'),pl('Argentijnen')],de,[]).

n([sg('Armeniër'),pl('Armeniërs')],de,[]).

n([mass('Armeens')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Arnhemmer'),pl('Arnhemmers')],de,[]).

n([sg('Assenaar'),pl('Assenaren')],de,[]).

n([sg('Atjeeër'),pl('Atjeeërs')],de,[]).

n([sg('Australiër'),pl('Australiërs')],de,[]).

n([mass('Azerbeidzjaans')],het,[]).

n([pl('Aziaten'),sg('Aziaat')],de,[]).

n([mass('Babylonisch')],het,[]).

n([sg('Balinees'),pl('Balinezen')],de,[]).

n([mass('Baltisch')],het,[],[]).

n([sg('Bask'),pl('Basken')],de,[]).

n([mass('Baskisch')],het,[]).

n([sg('Bataaf'),pl('Bataven')],de,[]).

n([sg('Batavier'),pl('Batavieren')],de,[]).

n([sg('Beier'),pl('Beiers')],de,[]).

n([mass('Beiers')],het,[]).

n([pl('Belgen'),sg('Belg')],de,[]).

n([sg('Bengaal'),pl('Bengalen')],de,[]).

n([mass('Bengaals')],het,[]).

n([sg('Bengalees'),pl('Bengalezen')],de,[]).

n([sg('Berber'),pl('Berbers')],de,[]).

n([mass('Berbers')],het,[]).

n([sg('Berlijner'),pl('Berlijners')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Berlijns')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Birmees'),pl('Birmezen')],het,[]).

n([mass('Birmees')],het,[]).

n([sg('Bosniër'),pl('Bosniërs')],de,[]).

n([mass('Bosnisch')],het,[]).

n([sg('Bosschenaar'),pl('Bosschenaren')],de,[]).

n([pl('Brabanders'),sg('Brabander')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Brabants')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([pl('Brazilianen'),sg('Braziliaan')],de,[]).

n([sg('Braziliaans')],het,[]).

n([sg('Bredanaar'),pl('Bredanaars')],de,[]).

n([pl('Bretons'),sg('Breton')],de,[]).

n([mass('Bretons')],het,[]).

n([pl('Britten'),sg('Brit')],de,[]).

n([mass('Brits')],het,[]).

n([mass('Brugs')],het,[]).

n([sg('Brusselaar'),pl('Brusselaren'),pl('Brusselaars')],de,[]).

n([mass('Brussels')],het,[]).

n([pl('Bulgaren'),sg('Bulgaar')],de,[]).

n([mass('Bulgaars')],het,[]).

n([sg('Burundiër'),pl('Burundiërs')],de,[]).

n([sg('Bussumer'),pl('Bussumers')],de,[]).

n([sg('Byzantijn'),pl('Byzantijnen')],de,[]).

n([pl('Canadezen'),sg('Canadees')],de,[]).

n([sg('Catalaan'),pl('Catalanen')],de,[]).

n([mass('Catalaans')],het,[]).

n([sg('CDA\'er'),pl('CDA\'ers')],de,[]).

n([mass('Celsius')],de,[]).

n([sg('Chetnik'),pl('Chetniks')],de,[]).

n([sg('Chileen'),pl('Chilenen')],de,[]).

n([pl('Chinezen'),pl(chinezen),
   sg('Chinees'),sg(chinees)],de,[]).

n([mass('Chinees'),mass(chinees)],het,[]).

n([sg('Columbiaan'),pl('Columbianen')],de,[]).

n([sg('Colombiaan'),pl('Colombianen')],de,[]).

n([pl('Congolezen'),sg('Congolees')],de,[]).

n([mass('Corsicaans')],het,[]).

n([pl('Cubanen'),sg('Cubaan')],de,[]).

n([sg('Cyprioot'),pl('Cyprioten')],de,[],
  [h('Grieks'),f(['Grieks']),
   h('Turks'), f(['Turks'])
  ]).

n([sg('Dayak'),pl('Dayaks')],de,[]).

n([sg('D66\'er'),pl('D66\'ers'),
   sg('D\'66\'er'),pl('D\'66\'ers')],de,[]).

n([sg('Delftenaar'),pl('Delftenaren')],de,[]).

n([pl('Democraten'),sg('Democraat')],de,[]).

n([pl('Denen'),sg('Deen')],de,[]).

n([mass('Deens')],het,[]).

n([mass('Deventers')],het,[]).

n([sg('Doetinchemmer'),
   pl('Doetinchemmers')],de,[]).

n([sg('Dordtenaar'),pl('Dordtenaren')],de,[]).

n([mass('Dordts')],het,[]).

n([mass('Drents')],het,[]).

n([sg('Dubliner'),pl('Dubliners')],de,[]).

n([mass('Duits')],het,[]).

n([pl('Duitsers'),sg('Duitser')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Edenaar'),pl('Edenaren')],de,[]).

n([sg('Egyptenaar'),pl('Egyptenaren')],de,[]).

n([mass('Egyptisch')],het,[]).

n([sg('Eindhovenaar'),pl('Eindhovenaren')],de,[]).

n([pl('EK'),pl('EK\'s'),sg('EK'),
   pl(ek),pl('ek\'s'),sg(ek)],both,
  [app_measure]).

n([sg('EK-dressuur')],both,[]).

n([sg('EK-honkbal')],both,[]).

n([sg('EK-hockey')],both,[]).

n([sg('EK-paren')],both,[]).

n([sg('EK-schaatsen')],both,[]).

n([sg('EK-voetbal')],both,[]).

n([sg('Emmenaar'),pl('Emmenaren')],de,[]).

n([mass('Engels')],het,[]).

n([pl('Engelsen')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Engelsman')],de,[]).

n([pl('Eskimo\'s'),sg('Eskimo')],de,[]).

n([sg('Est'),pl('Esten')],de,[]).

n([mass('Ests')],het,[]).

n([sg('Ethiopiër'),
   pl('Ethiopiërs')],de,[]).

n([mass('Ethiopisch')],het,[]).

n([sg('Etrusk'),pl('Etrusken')],de,[]).

n([pl(['Europa','Cups']),sg(['Europa','Cup'])],de,[app_measure]).

n([sg(['Europa','Cup','1'])],de,[]).

n([sg(['Europa','Cup','2'])],de,[]).

n([pl('Europeanen'),sg('Europeaan')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('F-16'),pl('F-16\'s')],de,[]).

n([pl('Feijenoorders'),sg('Feijenoorder')],de,[]).

n([pl('Feyenoorders'),sg('Feyenoorder')],de,[]).

n([sg('Filipijn'),pl('Filipijnen')],de,[]).

n([sg('Filippijn'),pl('Filippijnen')],de,[]).

n([mass('Filipijns'),mass('Fillipijns')],het,[]).

n([pl('Finnen'),sg('Fin')],de,[]).

n([mass('Fins')],het,[]).

n([sg('Fokker'),pl('Fokkers')],de,[]).

n([mass('Frankisch')],het,[]).

n([mass('Frans')],het,[]).

n([sg('Frank'),pl('Franken')],de,[]).

n([pl('Fransen')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Fransman')],de,[]).

n([pl('Françaises'),sg('Française')],de,[]).

n([pl('Friezen'),sg('Fries')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Fries')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Geallieerde'),pl('Geallieerden')],de,[]).

n([pl('Gelderlanders'),sg('Gelderlander')],de,[]).

n([sg('Gentenaar'),pl('Gentenaren'),pl('Gentenaars')],de,[]).

n([mass('Gents')],het,[]).

n([sg('Georgiër'),pl('Georgiërs')],de,[]).

n([mass('Georgisch')],het,[]).

n([pl('Germanen'),sg('Germaan')],de,[]).

n([mass('Germaans')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Ghanees'),pl('Ghanezen')],de,[]).

n([mass('Godsrijk')],het,[]).

n([sg('Goot'),pl('Goten')],de,[]).

n([mass('Gotisch')],het,[]).

n([pl('Grieken'),sg('Griek')],de,[]).

n([mass('Grieks')],het,[]).

n([mass('Groenlands')],het,[]).

n([pl('Groningers'),sg('Groninger')],de,[]).

n([mass('Gronings')],het,[]).

n([mass('Haags')],het,[]).

n([pl('Haarlemmers'),sg('Haarlemmer')],de,[]).

n([sg('Habsburger'),pl('Habsburgers')],de,[]).

n([pl('Hagenaren'),pl('Hagenaars'),sg('Hagenaar')],de,[]).

n([sg('Haïtiaan'),pl('Haïtianen')],de,[]).

n([sg('Hazara'),pl('Hazara'),pl('Hazara\'s')],de,[]).

n([mass('Hebreeuws')],het,[]).

n([mass('Helmonds')],het,[]).

n([sg('Hengeloër'),pl('Hengeloërs')],de,[]).

n([sg('Hoogevener'),pl('Hoogeveners')],de,[]).

n([pl('Hollanders'),sg('Hollander')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Hollands')],het,[]).

n([pl('Hongaren'),sg('Hongaar')],de,[]).

n([mass('Hongaars')],het,[]).

n([stem('Hun'),
   pl('Hunnen')],de,[]).

n([sg('Hutu'),pl('Hutu\'s'),pl('Hutus')],de,[]).

n([pl('Ieren'),sg('Ier')],de,[]).

n([mass('Iers')],het,[]).

n([mass('IJslands')],het,[]).

n([stem(indiaan),
   pl('Indianen'),sg('Indiaan'),
   pl(indianen),sg(indiaan)],de,[],
  [dim('Indiaantje'),
   dim(indiaantje)]).

n([pl('Indiërs'),sg('Indiër')],de,[]).

n([sg('Indo'),pl('Indo\'s')],de,[]).

n([pl('Indonesiërs'),sg('Indonesiër')],de,[]).

n([mass('Indonesisch')],het,[]).

n([pl('Irakezen'),sg('Irakees')],de,[]).

n([pl('Iraki\'s'),sg('Iraki')],de,[]).

n([pl('Iraniërs'),sg('Iraniër')],de,[]).

n([pl('Israëli\'s'),sg('Israëli'),
   pl('Israeli\'s'),sg('Israeli')],de,[]).

n([sg('Israëliër'),
   pl('Israeliërs'),sg('Israeliër'),
   pl('Israeliers'),sg('Israelier'),
   pl('Israëliers'),sg('Israëlier'),
   pl('Israëliërs')],de,[]).

n([pl('Italianen'),sg('Italiaan')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Italiaans')],het,[]).

n([sg('Jakobijn'),pl('Jakobijnen')],de,[]).

n([pl('Japanners'),sg('Japanner')],de,[]).

n([mass('Japans')],het,[]).

n([pl('Javanen'),sg('Javaan')],de,[]).

n([mass('Javaans')],het,[]).

n([sg(['Jehovah\'s','Getuige']),
   pl(['Jehovah\'s','Getuigen'])],de,[]).

n([sg('Joegoslaaf'),pl('Joegoslaven')],de,[]).

n([sg('Jordaniër'),pl('Jordaniërs')],de,[]).

n([sg('Karmeliet'),pl('Karmelieten')],de,[]).

n([sg('Karmelietesse'),pl('Karmelietessen')],de,[]).

n([sg('KWR')],de,[]). % de kwalitatieve woningregistratie

n([pl('Kelten'),sg('Kelt')],de,[]).

n([mass('Keltisch')],het,[]).

n([sg('Kenyaan'),pl('Kenyanen'),sg('Keniaan'),pl('Kenianen')],de,[]).

n([stem('Kerstmis'),sg('Kerstmis'),sg(kerstmis)],both,[temp_mod]).

n([sg('Koerd'),pl('Koerden'),sg('Kurd'),pl('Kurden'),
   sg(koerd),pl(koerden)],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Koerdisch'),mass('Kurdisch')],het,[]).

n([sg('Kongolees'),pl('Kongolezen')],de,[]).

n([mass('Koreaans')],het,[]).

n([sg('Koreaan'),pl('Koreanen')],de,[]).

n([sg('Kosovaar'),pl('Kosovaren')],de,[]).

n([sg('Labadist'),pl('Labadisten')],de,[]).

n([mass('Latijn')],het,[]).

n([sg('Leeuwarder'),pl('Leeuwarders')],de,[]).

n([mass('Leids')],het,[]).

n([sg('Let'),pl('Letten')],de,[]).

n([mass('Lets')],het,[]).

n([mass('Leuvens')],het,[]).

n([sg('Libanees'),pl('Libanezen')],de,[]).

n([sg('Liberaal'),pl('Liberalen')],de,[]).

n([sg('Liberiaan'),pl('Liberianen')],de,[]).

n([sg('Libiër'),pl('Libiërs')],de,[]).

n([mass('Liers')],het,[]).

n([sg('Limburger'),pl('Limburgers')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([mass('Limburgs')],het,[],[h('West'),
			     h('Zuid'),
			     h('Noord'),
			     h('Oost')]).

n([sg('Litouwer'),pl('Litouwers')],de,[]).

n([mass('Litouws')],het,[]).

n([sg('Londenaar'),pl('Londenaren')],de,[]).

n([sg('LPF\'er'),pl('LPF\'ers')],de,[]).

n([sg('Luxemburger'),pl('Luxemburgers')],de,[]).

n([mass('Luxemburgs')],het,[]).

n([sg('Maastrichtenaar'),pl('Maastrichtenaren')],de,[]).

n([mass('Maastrichts')],het,[]).

n([sg('Macedoniër'),pl('Macedoniërs')],de,[]).

n([mass('Macedonisch')],het,[]).

n([sg('Madurees'),pl('Madurezen')],de,[]).

n([sg('Madrileen'),pl('Madrilenen')],de,[]).

n([sg('Magyaar'),pl('Magyaren')],de,[]).

n([mass('Maleis')],het,[]).

n([mass('Maleisisch')],het,[]).

n([mass('Maltees')],het,[]).

n([sg('Maltezer'),pl('Maltezers')],de,[]).

n([mass('Markens')],het,[]).

n([pl('Marokkanen'),sg('Marokkaan')],de,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West'),
   dim('Marokkaantje')]).

n([mass('Marokkaans')],het,[]).

n([sg('Meppelaar'),pl('Meppelaren')],de,[]).

n([pl('Mexicanen'),sg('Mexicaan')],de,[],[dim('Mexicaantje')]).

n([sg('Milanees'),pl('Milanezen')],de,[]).

n([mass('Moldavisch')],het,[]).

n([sg('Molukker'),pl('Molukkers')],de,[]).

n([pl('Mongolen'),sg('Mongool')],de,[]).

n([mass('Mongools')],het,[]).

n([sg('Montenegrijn'),pl('Montenegrijnen')],de,[]).

n([pl('Moren'),sg('Moor')],de,[]).

n([sg('Moedjahidien'),
   pl('Moedjahidien')],de,[]).

n([sg('Mozabiet'),pl('Mozabieten')],de,[]).

n([mass('Nederduits')],het,[]).

n([pl('Nederlanders'),sg('Nederlander')],de,[]).

n([mass('Nederlanderschap')],het,[]).

n([mass('Nederlands')],het,[]).

n([mass('Nedersaksisch')],het,[]).

n([mass('Nepalees')],het,[]).

n([sg(['New','Yorker']),
   pl(['New','Yorkers'])],de,[]).

n([sg('Nieuw-Zeelander'),pl('Nieuw-Zeelanders')],de,[]).

n([sg('Nigeriaan'),pl('Nigerianen')],de,[]).

n([sg('Nijmegenaar'),pl('Nijmegenaren')],de,[]).

n([pl('NK'),sg('NK'),pl('NK\'s'),
   pl('N.K'),sg('N.K'),pl('N.K\'s'),
   pl('N.K.'),sg('N.K.'),pl('N.K.\'s'),
   pl(nk),sg(nk),pl('nk\'s')],both,[app_measure]).

n([sg('NK-viertallen'),
   pl('NK-viertallen')],het,[]).

n([pl('Nobelprijzen'),sg('Nobelprijs')],de,[app_measure]).

n([stem('Nobelprijs_winnaar'),pl('Nobelprijswinnaars'),sg('Nobelprijswinnaar')],de,[app_measure]).

n([pl('Noren'),sg('Noor')],de,[]).

n([pl('Noord-Hollanders'),sg('Noord-Hollander'),
   pl('Noordhollanders'),sg('Noordhollander')],de,[]).

n([pl('Noordkoreanen'),sg('Noord-Koreaan'),sg('Noordkoreaan'),
   pl('Noord-Koreanen')],de,[]).

n([pl('Noormannen'),sg('Noorman')],de,[]).

n([mass('Noors')],het,[]).

n([sg('Oegandees'),pl('Oegandezen')],de,[]).

n([sg('Oeigoer'),pl('Oeigoeren')],de,[]).

n([sg('Oekraïner'),pl('Oekraïners')],de,[]).

n([mass('Oekraïens')],het,[]).

n([sg('Oezbeek'),pl('Oezbeken')],de,[]).

n([mass('Oezbeeks')],het,[]).

n([mass('Oost-Brabants')],het,[]).

n([pl('Oostduitsers'),sg('Oost-Duitser'),sg('Oostduitser'),
   pl('Oost-Duitsers')],de,[]).

n([pl('Oostenrijkers'),sg('Oostenrijker')],de,[]).

n([sg('Oost-Europeaan'),sg('Oosteuropeaan'),pl('Oosteuropeanen'),
   pl('Oost-Europeanen')],de,[]).

n([sg('Oost-Timorees'),sg('Oosttimorees'),pl('Oosttimorezen'),
   pl('Oost-Timorezen')],de,[]).

n([sg('Osseet'),pl('Osseten')],de,[]).

n([mass('Ossetisch')],het,[]).

n([sg('Pakistaan'),pl('Pakistanen')],de,[]).

n([sg('Panamees'),pl('Panamezen')],de,[]).

n([pl('Papoea\'s'),sg('Papoea'),
   sg('Papua'),pl('Papua\'s')],de,[]).

n([sg('Parijzenaar'),pl('Parijzenaren')],de,[]).

n([mass('Pasen')],de,[]).

n([sg('Pathaan'),pl('Pathanen')],de,[]).

n([mass('Pathaans')],het,[]).

n([sg('Peruaan'),pl('Peruanen')],de,[]).

n([sg('Pers'), pl('Perzen')],de,[]).

n([mass('Perzisch')],het,[]).

n([mass('Pinksteren'),mass(pinksteren)],de,[]).

n([sg('Pool'),pl('Polen')],de,[]).

n([mass('Pools')],het,[]).

n([pl('Portugezen'),sg('Portugees')],de,[]).

n([mass('Portugees')],het,[]).

n([pl('Pruisen'),sg('Pruis')],de,[]).

n([sg('PSV\'er'),pl('PSV\'ers')],de,[]).

n([sg('PvdA\'er'),pl('PvdA\'ers')],de,[]).

n([mass('Quechua')],het,[]).  % taal in de Andes met veel q's

n([sg('Republikein'),pl('Republikeinen')],de,[]).

n([sg('Roemeen'),pl('Roemenen')],de,[]).

n([mass('Roemeens')],het,[]).

n([pl('Roma'),pl('Roma\'s')],de,[]).

n([mass('Romaans')],het,[]).

n([pl('Romeinen'),sg('Romein')],de,[]).

n([pl('Rotterdammers'),sg('Rotterdammer')],de,[]).

n([mass('Rotterdams')],het,[]).

n([pl('Russen'),sg('Rus')],de,[]).

n([pl('Russinnen'),sg('Russin')],de,[]).

n([mass('Russisch')],het,[]).

n([sg('Rwandees'),pl('Rwandezen'),sg('Ruandees'),pl('Ruandezen')],de,[]).

n([sg('Saoedi'),sg('Saoediër'),pl('Saoediërs'),pl('Saoedi\'s')],de,[]).

n([pl('Saksen'),sg('Saks')],de,[]).

n([pl('Saracenen'),sg('Saraceen')],de,[]).

n([sg('Scandinaviër'),pl('Scandinaviërs')],de,[]).

n([sg('Schiedammer'),pl('Schiedammers')],de,[]).

n([mass('Schiedams')],het,[]).

n([pl('Schotten'),sg('Schot')],de,[]).

n([mass('Schots')],het,[]).

n([sg('Senegalees'),pl('Senegalezen')],de,[]).

n([mass('Senegalees')],het,[]).

n([pl('Serviërs'),sg('Serviër')],de,[]).

n([mass('Servisch')],het,[]).

n([pl('Siamezen'),sg('Siamees')],de,[]).

n([mass('Siciliaans')],het,[]).

n([pl('Silvaners'),sg('Silvaner')],de,[]).

n([sg('Singaporees'),pl('Singaporezen')],de,[]).

%% zigeunervolk
n([sg('Sinto'),pl('Sinti')],de,[]).

%% de taal
n([mass('Sinti')],het,[]).

n([mass('Slavisch')],het,[]).

n([sg('Sloveen'),pl('Slovenen')],de,[]).

n([mass('Sloveens')],het,[]).

n([sg('Slowaak'),pl('Slowaken'),
   sg('Slovaak'),pl('Slovaken')],de,[]).

n([mass('Slowaaks'),mass('Slovaaks')],het,[]).

n([sg('Soedanees'),pl('Soedanezen')],de,[]).

n([sg('Soenniet'),pl('Soennieten')],de,[]).

n([sg('Somaliër'),pl('Somaliërs')],de,[]).

n([mass('Somalisch')],het,[]).

n([mass('Spaans')],het,[]).

n([pl('Spanjaarden'),pl('Spanjaards'),sg('Spanjaard')],de,[]).

n([sg('Sudanees'),pl('Sudanezen'),
   sg('Soedanees'),pl('Soedanezen')],de,[]).

n([mass('Sudanees'),mass('Soedanees')],het,[]).

n([mass('Surinaams')],het,[]).

n([sg('Surinamer'),pl('Surinamers')],de,[]).

n([sg('Syriër'),pl('Syriërs')],de,[]).

n([mass('Syrisch')],het,[]).

n([pl('T-shirts'),sg('T-shirt'),pl('t-shirts'),sg('t-shirt')],both,[],
  [dim('T-shirtje'),
   dim('t-shirtje')]).

n([sg('Taiwanees'),pl('Taiwanezen')],de,[]).

n([mass('Taiwanees')],het,[]).

n([sg('Tamil'),pl('Tamils')],de,[]).

n([mass('Tamil')],het,[]).

n([mass('Tataars')],het,[]).

n([mass('Tegels')],het,[]).

n([sg('Thai'),pl('Thai'),pl('Thais')],de,[]).

n([mass('Thais'),
   mass('Thai')],het,[]).

n([mass('Tibetaans')],het,[]).

n([sg('Tilburger'),pl('Tilburgers')],de,[]).

n([mass('Tilburgs')],het,[]).

n([sg('Timorees'),pl('Timorezen')],de,[]).

n([sg('Togolees'),pl('Togolezen')],de,[]).

n([pl('Tsjechen'),sg('Tsjech'),
   pl('Tjechen'),sg('Tjech')],de,[]).

n([mass('Tsjechisch')],het,[]).

n([sg('Tsjetsjeen'),pl('Tsjetsjenen')],de,[]).

n([sg('Tsjetjeen'),pl('Tsjetjenen')],de,[]).

n([sg('Tukker'),pl('Tukkers')],de,[]).

n([sg('Tunesiër'),pl('Tunesiërs')],de,[]).

n([mass('Tunesisch')],het,[]).

n([pl('Turken'),sg('Turk')],de,[]).

n([pl('Turkmenen'),sg('Turkmeen')],de,[]).

n([mass('Turks')],het,[]).

n([mass('Turkmeens')],het,[]).

n([sg('Tutsi'),pl('Tutsi\'s'),pl('Tutsis')],de,[]).

n([sg('Twent'),pl('Twenten')],de,[]).

n([sg('Twentenaar'),pl('Twentenaren')],de,[]).

n([mass('Twents')],het,[]).

n([sg('Udenaar'),pl('Udenaren')],de,[]).

n([sg('Unionist'),pl('Unionisten')],de,[]).

n([sg('Utrechter'),pl('Utrechters')],de,[]).

n([mass('Utrechts')],het,[]).

n([mass('Vaticaan')],het,[]).

n([mass('Venetiaans')],het,[]).

n([mass('Venloos')],het,[]).

n([sg('Vietnamees'),pl('Vietnamezen')],de,[]).

n([mass('Vietnamees')],het,[]).

n([sg('Volendammer'),pl('Volendammers')],de,[]).

n([mass('Volendams')],het,[]).

n([mass('Vlaams')],het,[],
  [hh('Zuid'),
   hh('Noord'),
   hh('Oost'),
   hh('West')]).

n([sg('Vlaardinger'),pl('Vlaardingers')],de,[]).

n([pl('Vlamingen'),sg('Vlaming')],de,[]).

n([sg('Vlielander'),pl('Vlielanders')],de,[]).

n([sg('VVD\'er'),pl('VVD\'ers')],de,[]).

n([pl('Walen'),sg('Waal')],de,[]).

n([sg('Waalwijker'),pl('Waalwijkers')],de,[]).

n([mass('Welsh')],het,[]).

n([sg('West-Duitser'),
   pl('West-Duitsers'),
   pl('Westduitsers'),sg('Westduitser')],de,[]).

n([mass('Westfaals')],het,[]).

n([mass('Wit-Russisch'),
   mass('Witrussisch')],het,[]).

n([pl('WK'),pl('WK\'s'),sg('WK'),
   pl('W.K'),pl('W.K\'s'),sg('W.K'),
   pl('W.K.'),pl('W.K.\'s'),sg('W.K.'),
   pl(wk),pl('wk\'s'),sg(wk)],both,[app_measure]).

n([stem('WK_afstand'),
   sg('WK-afstanden')],both,[]).

n([stem('WK_allround'),
   sg('WK-allround')],both,[]).

n([stem('WK_atletiek'),
   sg('WK-atletiek')],both,[]).

n([stem('WK_jeugd'),
   sg('WK-jeugd')],both,[app_measure]).

n([stem('WK_junior'),
   sg('WK-junioren')],both,[app_measure]).

n([stem('WK_spring'),
   sg('WK-sprint')],both,[]).

n([stem('WK_Superbike'),
   sg('WK-Superbike')],both,[]).

n([stem('WK_veldrijden'),
   sg('WK-veldrijden')],both,[]).

n([stem('WK_voetbal'),
   sg('WK-voetbal')],both,[]).

n([mass('Zaans')],het,[]).

n([sg('Zaïrees'),pl('Zaïrezen')],de,[]).

n([pl('Zeeuwen'),sg('Zeeuw')],de,[]).

n([mass('Zeeuws')],het,[]).

n([sg('Zimbabwaan'),sg('Zimbabweaan'),pl('Zimbabweanen'),
   pl('Zimbabwanen')],de,[]).

n([pl('Zweden'),sg('Zweed')],de,[]).

n([mass('Zweeds')],het,[]).

n([sg('Zuid-Afrikaan'),pl('Zuid-Afrikanen'),
   sg('Zuidafrikaan'),pl('Zuidafrikanen')],de,[]).

n([mass('Zuid-Afrikaans'),mass('Zuidafrikaans')],het,[]).

n([sg('Zuid-Amerikaan'),pl('Zuid-Amerikanen'),
   sg('Zuidamerikaan'),pl('Zuidamerikanen')],de,[]).

n([sg('Zuid-Koreaan'),pl('Zuid-Koreanen'),
   sg('Zuidkoreaan'),pl('Zuidkoreanen')],de,[]).

n([sg('Zulu'),pl('Zulu\'s')],de,[]).

n([pl('Zwitsers'),sg('Zwitser')],de,[]).

n([sg('Zwollenaar'),pl('Zwollenaren')],de,[]).

n([mass('Zwols')],het,[]).

n([pl('a\'s'),sg(a)],de,[],[dim('a\'tje')]).

n([mass([a,capella])],het,[]).

n([sg(aa)],de,[]).

n([pl(aaien),sg(aai)],de,[]).

n([pl(alen),sg(aal)],de,[],[dim(aaltje)]).

n([pl(aalmoezen),sg(aalmoes)],de,[]).

n([pl(aalmoezeniers),sg(aalmoezenier)],de,[]).

n([sg(aalscholver),pl(aalscholvers)],de,[]).

n([pl(aambeelden),sg(aambeeld)],het,[]).

n([pl(aambeien),sg(aambei)],de,[]).

n([sg(aanbeet),pl(aanbeten)],de,[]).

n([pl(aanbestedingen),sg(aanbesteding)],de,[]).

n([sg(aanbetaling),pl(aanbetalingen)],de,[]).

n([pl(aanbevelingen),sg(aanbeveling)],de,
  [sbar,
   vp]).

n([pl(aanbidders),sg(aanbidder)],de,[]).

n([pl(aanbiddingen),sg(aanbidding)],de,[]).

n([sg(aanbieder),pl(aanbieders)],de,[],
  [telecom,
   zorg]).

n([pl(aanbiedingen),sg(aanbieding)],de,
  [sbar,
   vp,
   pred_pp(in)]).

n([sg(aanblik)],de,[]).

n([sg(aanbod)],het,
  [vp,
   measure],   % het aanbod groene stroom is groter dan ...
  []).

n([sg(aanbod)],het,
  [],
  [s(arbeid),
   nieuws,
   over]).

n([mass(aanbouw),pl(aanbouwen)],de,
  [pred_pp(in)]).

n([mass(aanbreng)],de,[]).

n([mass(aandacht)],de,
  [pred_pp(in),
   pred_pp(buiten)],[]).

n([mass(aandacht)],de,[],
  [h(media)]).

n([pl(aandelen),sg(aandeel)],het,
  [measure]).

n([pl(aandelen),sg(aandeel)],het,[],
  [internet, % intern_taan_deel
   h(technologie),
   technologie,
   telecom]).

n([pl(aandeelhouders),sg(aandeelhouder)],de,[],[groot]).

n([pl(aandenkens),sg(aandenken)],het,[]).

n([pl(aandoeningen),sg(aandoening)],de,
  [sbar]).

n([pl(aandoeningen),sg(aandoening)],de,[],
  [hart,
   hersen,
   huid]).

n([mass(aandrang)],de,[vp]).

n([mass(aandrift)],de,[]).

n([pl(aandrijvingen),sg(aandrijving)],de,[],
  [i(achter_wiel,achterwiel),
   vierwiel]).

n([pl(aanduidingen),sg(aanduiding)],de,
  [sbar,
   vp,
   app_measure,
   start_app_measure]).

n([pl(aanduidingen),sg(aanduiding)],de,
  [sbar,
   vp,
   app_measure,
   start_app_measure],
  [soort,
   type]).

n([pl(aaneenschakelingen),sg(aaneenschakeling)],de,[]).

n([mass(aanfluiting)],de,[subject_sbar,subject_vp]).

n([pl(aangelegenheden),sg(aangelegenheid)],de,[]).

n([sg(aangever),pl(aangevers)],de,[]).

n([sg(aangeefster),pl(aangeefsters)],de,[]).

n([pl(aangezichten),sg(aangezicht)],het,[]).

n([pl(aangiften),pl(aangiftes),sg(aangifte)],de,
  [app_measure]).

n([pl(aangiften),pl(aangiftes),sg(aangifte)],de,[],
  [belasting]).

n([sg(aangooi)],de,[]).

n([mass(aangroei)],de,[]).

n([sg(aanhaling),pl(aanhalingen)],de,[]).

n([pl(aanhalingstekens),sg(aanhalingsteken)],het,[]).

n([mass(aanhang)],de,[]).

n([pl(aanhangers),sg(aanhanger)],de,[]).

n([pl(aanhangselen),pl(aanhangsels),sg(aanhangsel)],het,[]).

n([mass(aanhankelijkheid)],de,[]).

n([mass(aanhef)],de,[]).

n([pl(aanhoudingen),sg(aanhouding)],de,[]).

n([pl(aanklachten),sg(aanklacht)],de,[sbar,subject_sbar]).

%% betaald voetbal
n([pl(aanklagers),sg(aanklager),
   pl(aanklaagsters),sg(aanklaagster)],de,
  [app_measure],
  [hoofd]).

n([pl(aankledingen),sg(aankleding)],de,[]).

n([pl(aanknopingspunten),sg(aanknopingspunt)],het,
  [vp]).

n([mass(aankomst),pl(aankomsten),sg(aankomst)],de,[]).

n([pl(aankondigingen),sg(aankondiging)],de,[sbar,vp]).

n([pl(aankopen),sg(aankoop)],de,[],[steun]).

n([sg(aanleg)],de,[],[her]).  % in eerste aanleg

n([pl(aanlegsteigers),sg(aanlegsteiger)],de,[]).

n([pl(aanleidingen),sg(aanleiding)],de,
  [vp,
   sbar,
   subject_sbar]).

n([pl(aanlopen),sg(aanloop)],de,[]).

n([mass(aanmaak)],de,[]).

n([pl(aanmaningen),sg(aanmaning)],de,[sbar,vp]).

n([pl(aanmeldingen),sg(aanmelding)],de,[]).

n([pl(aanmerkingen),sg(aanmerking)],de,[sbar,vp]).

n([pl(aanmoedigingen),sg(aanmoediging)],de,[vp]).

n([pl(aannamen),pl(aannames),sg(aanname)],de,
  [sbar,subject_sbar],
  [basis,
   standaard]).

n([pl(aannemers),sg(aannemer)],de,[],[onder]).

n([pl(aannemingen),sg(aanneming)],de,[sbar]).

n([mass(aanpak)],de,[]).

n([pl(aanpassingen),sg(aanpassing)],de,[sbar,vp,subject_sbar,subject_vp]).

n([mass(aanplant),pl(aanplanten),sg(aanplant)],de,[]).

n([sg(aanrader),pl(aanraders)],de,[subject_vp]).

n([pl(aanrakingen),sg(aanraking)],de,[pred_pp(in)]).

n([pl(aanrandingen),sg(aanranding)],de,[]).

n([pl(aanrechten),sg(aanrecht)],both,[]). % celex: both

n([pl(aanrijdingen),sg(aanrijding)],de,[]).

n([sg(aanroep),pl(aanroepen)],de,[]).

n([mass(aanschaf)],de,[]).

n([sg(aanscherping),pl(aanscherpingen)],de,[]).

n([mass(aanschijn)],het,[]).

n([pl(aanschouwingen),sg(aanschouwing)],de,[]).

n([pl(aanschrijvingen),sg(aanschrijving)],de,[]).

n([pl(aanslagen),sg(aanslag)],de,[app_measure]).

n([pl(aanslagen),sg(aanslag)],de,[],
  [bom,
   moord,
   terreur,
   zelfmoord]).

n([pl(aansluitingen),sg(aansluiting)],de,[],[internet]).

n([pl(aanspanningen),sg(aanspanning)],de,[]).  % van de spieren

n([pl(aansporingen),sg(aansporing)],de,[vp]).

n([pl(aanspraken),sg(aanspraak)],de,[sbar,vp]).

n([pl(aansprakelijkheden),sg(aansprakelijkheid)],de,[]).

n([pl(aanstalten)],de,[vp]).

n([pl(aanstekers),sg(aansteker)],de,[]).

n([pl(aanstellerijen),sg(aanstellerij)],de,[vp]).

n([pl(aanstellingen),sg(aanstelling)],de,[]).

n([pl(aanstichters),sg(aanstichter)],de,[]).

n([mass(aanstoot)],de,[]).

n([pl(aantallen),sg(aantal)],het,[measure],
  [h(x),
   record]).

n([pl(aantallen),sg(aantal)],het,[],
  [s(bevolking),
   inwoner,
   i(student,studenten)]).

n([pl(aantastingen),sg(aantasting)],de,[]).

n([pl(aantekeningen),sg(aantekening)],de,[sbar]).

n([pl(aantijgingen),sg(aantijging)],de,[sbar]).

n([mass(aantocht)],de,[pred_pp(in)]). % met de verkiezingen in aantocht

n([pl(aantrekkelijkheden),sg(aantrekkelijkheid)],de,[]).

n([mass(aantrekking)],de,[]).

n([pl(aanvaardingen),sg(aanvaarding)],de,[]).

n([pl(aanvallen),sg(aanval)],de,[sbar]).

n([pl(aanvallen),sg(aanval)],de,[],
  [cyber,
   h('NAVO'),
   paniek,
   raket,
   tegen]).

n([pl(aanvallers),sg(aanvaller)],de,[]).

n([pl(aanvalsters),sg(aanvalster)],de,[]).

n([mass(aanvang)],de,[]).

n([pl(aanvaringen),sg(aanvaring)],de,[pred_pp(in)]).

n([pl(aanvechtingen),sg(aanvechting)],de,[vp]).

n([pl(aanvoeren),sg(aanvoer)],de,[]).

n([pl(aanvoerders),sg(aanvoerder),
   pl(aanvoersters),sg(aanvoerster)],de,[],
  [leger,
   lijst]).

n([mass(aanvoering)],de,[]).

n([pl(aanvragen),sg(aanvraag),sg(aanvrage)],de,
  [vp,
   app_measure],
  [advies,
   asiel]).

n([pl(aanvragers),sg(aanvrager)],de,[]).

n([pl(aanvullingen),sg(aanvulling)],de,[sbar]).

n([pl(aanwassen),sg(aanwas)],de,[]).

n([pl(aanwendingen),sg(aanwending)],de,[]).

n([mass(aanwezen)],het,[]).   % ouderwets

n([mass(aanwezigheid)],de,[]).

n([pl(aanwijzingen),sg(aanwijzing)],de,[sbar,subject_sbar,vp]).

n([pl(aanwinsten),sg(aanwinst)],de,[]).

n([sg(aanzegging),pl(aanzeggingen)],de,[sbar]).

n([pl(aanzetten),sg(aanzet)],de,[]).

n([pl(aanzichten),sg(aanzicht)],het,[]).

n([mass(aanzien)],het,[]).

n([sg(aanzoek),pl(aanzoeken)],het,[]).

n([pl(apen),sg(aap)],de,[],[dim(aapje)]).

n([pl(aren),sg(aar)],de,[]).

n([meas(are),pl(aren)],de,[meas_mod,measure]).

n([mass(aard)],de,[pred_pp(van)]).

n([mass(aard)],de,[],[s(volk)]).

n([pl(aardappelen),pl(aardappels),sg(aardappel)],de,[],[dim(aardappeltje)]).

n([pl(aardbeien),sg(aardbei)],de,[]).

n([pl(aardbevingen),sg(aardbeving)],de,[]).

n([mass(aardbodem)],de,[]).

n([pl(aardbollen),sg(aardbol)],de,[]).

n([pl(aarden),sg(aarde)],de,[]).

n([mass(aardewerk)],het,[]).

n([pl(aardigheden),sg(aardigheid)],de,[vp],[dim(aardigheidje)]).

n([mass(aardkorst)],de,[]).

n([mass(aardolie)],de,[]).

n([mass(aardoppervlak)],het,[]).

n([mass(aardrijkskunde)],de,[]).

n([pl(aardverschuivingen),sg(aardverschuiving)],de,[]).

n([pl(aarzen),sg(aars)],de,[]).

n([pl(aarzelingen),sg(aarzeling)],de,[vp]).

n([pl(azen),sg(aas)],both,[]).

n([mass(aas)],het,[]).

n([pl(abattoirs),sg(abattoir)],het,[]).

n([pl(abcessen),sg(abces)],het,[]).

n([pl(abdijen),sg(abdij)],de,[]).

n([pl(abonnees),sg(abonnee),pl(abbonnees),sg(abbonnee)],de,[],
  [dim(abonneetje)]).

n([pl(abonnementen),sg(abonnement)],het,[]).

n([sg(aboriginal),pl(aboriginals)],de,[]).

n([mass(abortus),pl(abortussen),sg(abortus)],de,[]).

n([mass([abortus,provocatus])],de,[]).

n([pl(abrikozen),sg(abrikoos)],de,[]).

n([sg(absence),pl(absences)],de,[]).

n([mass(absentie)],de,[]).

n([sg(absint)],de,[]).

n([mass(absolutisme)],het,[]).

n([sg(abstract),pl(abstracts)],both,[]).

n([pl(abstracties),sg(abstractie),
   pl(abstrakties),sg(abstraktie)],de,[]).

n([pl(absurditeiten),sg(absurditeit)],de,[sbar]).

n([pl(abten),sg(abt)],de,[]).

n([sg(abuis)],het,[]).

n([pl('acacia\'s'),sg(acacia)],de,[],[dim(acaciaatje)]).

n([pl(academici),sg(academicus)],de,[]).

n([pl(academies),pl(academiën),sg(academie)],de,[app_measure]).

n([pl(accenten),sg(accent)],het,[]).

n([sg([accent,aigue]),
   pl([accents,aigue]),
   pl([accenten,aigu])],both,[]).

n([sg([accent,circonflexe]),
   pl([accents,circonflexe]),
   pl([accenten,circonflexe])],both,[]).

n([sg([accent,grave]),
   pl([accents,grave]),
   pl([accenten,grave])],both,[]).

n([pl(acceptaties),sg(acceptatie)],de,[sbar]).

n([sg(accessoire),pl(accessoires)],both,[]).

n([pl(accijnzen),sg(accijns)],de,[]).

n([pl(accommodaties),sg(accommodatie)],de,[]).

n([sg(accordeon),pl(accordeons)],both,[]).

n([sg(accordeonist),pl(accordeonisten)],de,[]).

n([sg(account),pl(accounts)],both,[]).

n([pl(accountants),sg(accountant)],de,[]).

n([sg(accreditatie),pl(accreditaties)],de,[]).

n([sg(accreditering),pl(accrediteringen)],de,[]).

n([pl('accu\'s'),sg(accu)],de,[],[dim(accuutje)]).

n([pl(accumulaties),sg(accumulatie)],de,[]).

n([mass(accuratesse)],de,[]).

n([mass(accusativus),mass(acc),mass('acc.')],de,[]).

n([pl(aces),sg(ace)],de,[]).

n([mass([ach,en,wee])],het,[]).

n([pl(achilleshielen),sg(achilleshiel)],de,[]).

n([pl(achillespezen),sg(achillespees)],de,[]).

n([pl(achten),sg(acht)],de,[],[dim(achtje)]).

n([pl(achtsten)],de,[]).

n([pl(achterassen),sg(achteras)],de,[]).

n([mass(achterban)],de,[]).

n([pl(achterblijvers),sg(achterblijver)],de,[]).

n([pl(achterdekken),sg(achterdek)],het,[]).

n([mass(achterdocht)],de,[]).

n([pl(achterflappen),sg(achterflap)],de,[]).

n([pl(achtergronden),sg(achtergrond)],de,[pred_pp(tegen)]).

n([pl(achterhoeden),pl(achterhoedes),sg(achterhoede)],de,[]).

n([pl(achterhoedegevechten),sg(achterhoedegevecht)],het,[]).

n([pl(achterhoeken),sg(achterhoek)],de,[]).

n([pl(achterhoofden),sg(achterhoofd)],het,[]).

n([pl(achterhuizen),sg(achterhuis)],het,[]).

n([pl(achterkanten),sg(achterkant)],de,[]).

n([mass(achterklap)],de,[]).

n([sg(achterklep),pl(achterkleppen)],de,[]).

n([pl(achterlanden),sg(achterland)],het,[]).

n([sg(achterlijk),pl(achterlijken)],het,[]).  % deel van het grootzeil

n([sg(achterom)],de,[]). %% het huis heeft een achterom

n([pl(achterpoten),sg(achterpoot)],de,[]).

n([pl(achterruiten),sg(achterruit)],de,[]).

n([pl(achterstanden),sg(achterstand)],de,[],[taal]).

n([pl(achterstes),sg(achterste)],het,[]).

n([mass(achterstelling)],de,[]).

n([pl(achterstevens),sg(achtersteven)],both,[]).

n([sg(achteruit)],de,[]).

n([sg(achteruitgang),pl(achteruitgangen)],de,[]).

n([pl(achtervoegsels),sg(achtervoegsel)],het,[app_measure]).

n([pl(achtervolgers),sg(achtervolger)],de,[]).

n([pl(achtervolgingen),sg(achtervolging)],de,[]).

n([pl(achterwerken),sg(achterwerk)],het,[]).

n([pl(achterzakken),sg(achterzak)],de,[]).

n([mass(achting)],de,[]).

n([bare_meas(achtmaal),pl(achtmalen)],both,[temp_mod,measure,sbar]).

n([sg(acoliet),pl(acolieten)],de,[]).

n([sg([acquis,communautaire])],het,[]).

n([pl(acquisities),sg(acquisitie)],de,[]).

n([mass(acquit)],both,[]).

n([pl(acts),sg(act)],de,[]).

n([sg([acte,de,présence])],de,[]).

n([pl(acteurs),sg(acteur),pl(akteurs),sg(akteur)],de,[],
  [film]).

n([mass(actief)],het,[]).

n([pl(acties),pl(actiën),sg(actie),
   pl(akties),pl(aktiën),sg(aktie)],de,
  [pred_pp(in),
   subject_sbar,
   subject_vp
  ]).

n([pl(acties),pl(actiën),sg(actie),
   pl(akties),pl(aktiën),sg(aktie)],de,
  [],
  [s(bezetting),
   s(gijzeling),
   i(handtekening,handtekeningen),
   hulp,
   s(inzameling),
   lucht,
   h('NAVO'),h('Navo'),'NAVO','Navo',
   politie,
   protest,
   s(redding),
   televisie,h(tv),tv,i(tv,'TV-'),f([tv]),
   s(vergelding),
   wraak,
   zelfmoord,
   zoek]).

n([pl(actievoerders),sg(actievoerder)],de,[]).

n([pl(actievoersters),sg(actievoerster)],de,[]).

n([sg(activator),pl(activatoren)],de,[]). % en niet activa_toren

n([mass(activisme)],het,[]).

n([pl(activisten),sg(activist),
   pl(aktivisten),sg(aktivist)],de,[],
  [i(consument,consumenten),
   i(dier,dieren),
   i(mens_recht,mensenrechten),
   milieu]).

n([pl(activiteiten),sg(activiteit),
   pl(aktiviteiten),sg(aktiviteit)],de,[app_measure],
  []).

n([pl(activiteiten),sg(activiteit),
   pl(aktiviteiten),sg(aktiviteit)],de,[],
  [internet,
   kern,
   neven  % geen neef_activiteit
  ]).

n([pl(activa)],het,[]).

n([sg(activatie),pl(activaties)],de,[]).

n([sg(actor),pl(actoren)],de,[]).

n([pl(actrices),sg(actrice),pl(aktrices),sg(aktrice)],de,[],
  [film]).

n([pl(actualiteiten),sg(actualiteit),
   pl(aktualiteiten),sg(aktualiteit)],de,[pred_pp(van)]).

n([pl(acupuncturen),sg(acupunctuur),
   pl(akupunkturen),sg(akupunktuur)],de,[]).

n([sg([ad,fundum])],het,[]).

n([pl(adagia),sg(adagium)],het,
  [np_app_measure,
   start_app_measure,
   subject_sbar,
   sbar]).

n([pl(adamsappels),sg(adamsappel)],de,[]).

n([sg(adapter),pl(adapters)],de,[]).

n([sg('add-on'),pl('add-ons')],de,[]).

n([pl(adderen),pl(adders),sg(adder)],de,[],[dim(addertje)]).

n([sg(additief),pl(additieven)],het,[],[brandstof]).

n([mass(adel)],de,[]).

n([pl(adelaars),pl(adelaren),sg(adelaar)],de,[]).

n([sg(adelstand)],de,[]).

n([mass(adem)],de,[]).

n([pl(ademhalingen),sg(ademhaling)],de,[],[buik]).

n([mass(ademnood)],de,[]).

n([pl(adempauzen),pl(adempauzes),sg(adempauze)],de,[]).

n([mass(ademtocht)],de,[]).

n([pl(aderen),pl(aders),sg(ader)],de,[],
  [s(verkeer),
   dim(adertje)]).

n([mass(aderlaten)],het,[]).

n([pl(aderlatingen),sg(aderlating)],de,[]).

n([mass(aderverkalking)],de,[]).

n([mass(adieu)],het,[]).

n([pl(adjectieven),sg(adjectief),
   pl(adjektieven),sg(adjektief)],het,[]).

n([pl(adjudanten),sg(adjudant)],de,[]).

n([pl(administrateuren),pl(administrateurs),sg(administrateur)],de,[]).

n([pl(administraties),sg(administratie)],de,[],[salaris]).

n([pl(admiraals),pl(admiralen),sg(admiraal)],de,[],[h(vice),vice]).

n([pl(admiraliteiten),sg(admiraliteit)],de,[]).

n([pl(adolescenten),sg(adolescent)],de,[]).

n([mass(adolescentie)],de,[]).

n([sg(adoptant),pl(adoptanten)],de,[]).

n([pl(adopties),sg(adoptie)],de,[],
  [i(stief_ouder,stiefouder)]).  % en niet stief_oud_rad_optie

n([mass(adrenaline)],de,[]).

n([pl(adressen),sg(adres)],het,[],
  ['e-mail', h('e-mail'), f(['e-mail']),
   email,h(email),f([email]),
   internet,
   onderduik,
   vakantie,
   dim(adresje)]).

n([pl(adverteerders),sg(adverteerder)],de,[]).

n([pl(advertenties),sg(advertentie)],de,[],[s(personeel)]).

n([pl(adviezen),sg(advies)],het,
  [sbar,
   vp,
   app_measure,
   np_app_measure],
  [studie]).

n([pl(adviezen),sg(advies)],het,
  [app_measure,
   np_app_measure],
  [s(belegging),
   koop,
   reis,
   h('SER')]).

n([sg(advisering),pl(adviseringen)],de,[]).

n([pl(adviseurs),sg(adviseur),pl(advizeurs),sg(advizeur)],de,
  [app_measure],
  [belasting,
   s(belegging),
   spaar,
   s(veiligheid)]).

n([pl(advocaten),sg(advocaat),pl(advokaten),sg(advokaat)],de,[],
  [s(land)]).

n([sg('advocaat-generaal')],de,[]).

n([mass(advocatuur),mass(advokatuur)],de,[]).

n([pl(advocates),sg(advocate),pl(advokates),sg(advokate)],de,[]).

n([pl(afbakeningen),sg(afbakening)],de,[]).

n([pl(afbeeldingen),sg(afbeelding)],de,[],[dim(afbeeldinkje)]).

n([pl(afbetalingen),sg(afbetaling)],de,[]).

n([mass(afbouw)],de,[]).

n([mass(afbraak)],de,[]).

n([mass(afbreuk)],de,[]).

n([pl(afdaken),sg(afdak)],het,[],[dim(afdakje)]).

n([pl(afdalingen),sg(afdaling)],de,[]).

n([sg(afdanker),pl(afdankers)],de,[],[dim(afdankertje)]).

n([pl(afdelingen),sg(afdeling)],de,[app_measure],
  [onder,
   verpleeg,
   dim(afdelinkje)]).

n([pl(afdoeningen),sg(afdoening)],de,[]).

n([pl(afdrachten),sg(afdracht)],de,[],[belasting]).

n([mass(afdronk)],de,[]).

n([pl(afdrukken),sg(afdruk)],de,[],[voet]).

n([pl(affaires),sg(affaire)],de,[],[h('IRT')]).

n([pl(affecties),sg(affectie),
   pl(affekties),sg(affektie)],de,[]).

n([pl(affiches),sg(affiche)],both,[]).

n([pl(affiniteiten),sg(affiniteit)],de,[]).

n([sg(affix),pl(affixen)],both,[]).

n([sg(affront),pl(affronten)],het,[]).

n([pl(afgangen),sg(afgang)],de,
  [subject_sbar,
   subject_vp]).

n([pl(afgelastingen),sg(afgelasting)],de,[]).

n([sg(afgeleide),pl(afgeleides)],de,[pred_pp(van)]).

% al adjectief dat zelfstandig gebruikt kan worden
% n([pl(afgevaardigden),sg(afgevaardigde)],de,[]).

n([pl(afgezanten),sg(afgezant)],de,[]).

n([mass(afgifte)],de,[]).

n([pl(afgoden),sg(afgod)],de,[app_measure]).  % de afgod globalisering

n([mass(afgrijzen)],het,[]).

n([pl(afgronden),sg(afgrond)],de,[]).

n([mass(afgunst)],de,[]).

n([sg(afhaal)],de,[]).

n([sg(afhandeling),pl(afhandelingen)],de,[],
  [schade]).

n([mass(afhankelijkheid)],de,[]).

n([pl(afkalvingen),sg(afkalving)],de,[]).

n([mass(afkap)],de,[]).

n([mass(afkeer)],de,[]).

n([mass(afkeur)],de,[]).

n([pl(afkeuringen),sg(afkeuring)],de,[]).

n([sg(afkick)],de,[]).

n([mass(afkoeling)],de,[]).

n([mass(afkomst)],de,[pred_pp(van)]).

n([pl(afkondigingen),sg(afkondiging)],de,[sbar]).

n([pl(afkooksels),sg(afkooksel)],het,[]).

n([pl(afkopen),sg(afkoop)],de,[]).

n([pl(afkoopsommen),sg(afkoopsom)],de,[]).

n([pl(afkortingen),sg(afkorting)],de,[app_measure]).

n([pl(aflaten),sg(aflaat)],de,[]).

n([pl(afleidingen),sg(afleiding)],de,[],[dim(afleidinkje)]).

n([pl(afleveringen),sg(aflevering)],de,[temp_mod]).

n([pl(aflopen),sg(afloop)],de,[]).

n([sg(aflos)],de,[]).

n([pl(aflossingen),sg(aflossing)],de,[]).

n([pl(afmetingen),sg(afmeting)],de,[]).

n([mass(afname)],de,[]).

n([pl(afnemers),sg(afnemer)],de,[]).

n([pl(aforismen),sg(aforisme)],het,[]).

n([sg(afperser),pl(afpersers)],de,[]).

n([pl(afpersingen),sg(afpersing)],de,[]).

n([pl(afrasteringen),sg(afrastering)],de,[]).

n([sg(afreis),pl(afreizen)],de,[]).

n([pl(afrekeningen),sg(afrekening)],de,[]).

n([pl(afritten),sg(afrit)],de,[]).

n([mass(afroep)],de,[]).

n([pl(afrondingen),sg(afronding)],de,[]).

n([sg(afruil),pl(afruilen)],de,[]).

n([pl(afschaffingen),sg(afschaffing)],de,[]).

n([mass(afschaf)],de,[]). %VL

n([sg(afschamper),pl(afschampers)],de,[]). %VL

n([mass(afscheid)],het,[]).

n([pl(afscheidingen),sg(afscheiding)],de,[]).

n([pl(afscheidsbrieven),sg(afscheidsbrief)],de,[]).

n([sg(afscherming),pl(afschermingen)],de,[]).

n([pl(afschriften),sg(afschrift)],het,[],
  [bank]). % niet ban_kaf_schrift

n([pl(afschrijvingen),sg(afschrijving)],de,[]).

n([mass(afschrikking)],de,[]).

n([mass(afschuw)],de,[]).

n([pl(afslagen),sg(afslag)],de,[]).

n([mass(afslanking)],de,[]).

n([sg(afsluiter),pl(afsluiters)],de,[]).

n([pl(afsluitingen),sg(afsluiting)],de,[],[dim(afsluitinkje)]).

n([pl(afspiegelingen),sg(afspiegeling)],de,[]).

n([sg(afsplitsing),pl(afsplitsingen)],de,[]).

n([sg(afsnijdingen),pl(afsnijdingen)],de,[]).

n([pl(afspraken),sg(afspraak)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp]).

n([pl(afspraken),sg(afspraak)],de,
  [],
  [prijs]).

n([pl(afspraakjes),sg(afspraakje)],het,[]).

n([sg(afsprong),pl(afsprongen)],de,[]).

n([pl(afstammelingen),sg(afstammeling)],de,[]).

n([pl(afstammelinges),sg(afstammelinge)],de,[]).

n([mass(afstamming)],de,[]).

n([pl(afstanden),sg(afstand)],de,[meas_mod],
  []).

n([pl(afstanden),sg(afstand),ignore_stem(afstand)],de,[],
  [s(gehoor),
   loop,
   midden,f([midden]),
   dim(afstandje)
  ]).

n([mass(afstandelijkheid)],de,[]).

n([pl(afstandsbedieningen),sg(afstandsbediening)],de,[]).

n([mass(afstel)],het,[]).

n([pl(afstellingen),sg(afstelling)],de,[]).

n([pl(afstemmingen),sg(afstemming)],de,[]).

%% niet af_stom_ping
n([sg(afstomping),pl(afstompingen)],de,[]).

n([pl(afstoten),sg(afstoot)],de,[]).

n([pl(afstraffingen),sg(afstraffing)],de,[]).

n([sg(afstreek)],de,[]). % viool?

n([mass(aftakeling)],de,[]).

n([mass(aftershave)],de,[]).

n([pl(aftochten),sg(aftocht)],de,[]).

n([pl(aftrappen),sg(aftrap)],de,[]).

n([mass(aftrek)],de,[],
  [belasting,
   hypotheekrente,
   interest,
   intrest,
   rente,h(rente),
   i(oud,ouderen),
   i(zelfstandig,zelfstandigen)]).

n([pl(aftreksels),sg(aftreksel)],het,[],[dim(aftrekseltje)]).

n([pl(aftrekposten),sg(aftrekpost)],de,[]).

n([pl(afvaardigingen),sg(afvaardiging)],de,[]).

n([sg(afvaart),pl(afvaarten)],de,[]).

n([mass(afval)],both,[],
  [kern,
   rest,
   slacht,
   tuin,
   zwerf]).

n([sg(afvaller),pl(afvallers)],de,[]).

n([mass(afvang)],de,[],[]).

n([mass(afvloei)],de,[],[]).

n([pl(afvloeiingen),sg(afvloeiing)],de,[]).

n([pl(afvoeren),sg(afvoer)],de,[]).

n([mass(afwachting)],de,[pred_pp(in),
                         pred_pp(in,sbar)]).

n([mass(afwas)],de,[]).

n([pl(afwateringen),sg(afwatering)],de,[]).

n([mass(afweer)],de,[],[lucht]).

n([mass(afweging),pl(afwegingen),sg(afweging)],de,[sbar,vp]).

n([mass(afwerking)],de,[]).

n([mass(afwezigheid)],de,[]).

n([pl(afwijkingen),sg(afwijking)],de,[app_measure]).

n([pl(afwijkingen),sg(afwijking)],de,[],
  [hart,
   hersen
  ]).

n([pl(afwijzingen),sg(afwijzing)],de,[vp]).

n([pl(afwikkelingen),sg(afwikkeling)],de,[]).

n([pl(afwisselingen),sg(afwisseling)],de,[]).

n([pl(afzenders),sg(afzender)],de,[]).

n([mass(afzet)],de,[]).

n([sg(afzetter),pl(afzetters)],de,[]).

n([sg(afzetster),pl(afzetsters)],de,[]).

n([pl(afzettingen),sg(afzetting)],de,[]).

n([sg(afzink)],de,[]).

n([mass(afzondering)],de,[]).

n([mass(afzwakking)],de,[]).

n([pl('agenda\'s'),sg(agenda)],both,[],
  [nieuws,
   ontwerp,
   dim(agendaatje)]).

n([sg(agendapunt),pl(agendapunten)],het,[app_measure]).

n([pl(agenten),sg(agent)],de,[],
  [f([geheim]),
   hoofd,
   motor,
   politie,h(politie),
   reis,
   s(veiligheid),
   wijk,
   dim(agentje)]).

n([pl(agentes),sg(agente)],de,[],
  [f([geheim]),
   hoofd,
   motor,
   politie,h(politie),
   s(veiligheid),
   wijk,
   dim(agentje)]).

n([pl(agentschappen),sg(agentschap)],het,[],
  [pers]).

n([pl(agglomeraties),sg(agglomeratie)],de,[]).

n([sg(aggregaat),pl(aggregaten)],het,[]).

n([pl(agitaties),sg(agitatie)],de,[]).

n([sg(agrariër),pl(agrariërs)],de,[]).

n([pl(agressies),sg(agressie)],de,[]).

n([mass(agressiviteit)],de,[]).

n([mass(aids),mass('Aids')],de,[]).

n([sg(aio),pl('aio\'s'),
   sg('AIO'),pl('AIO\'s')],de,[]).

n([pl(airs),sg(air)],het,[],[dim(airtje)]).

n([sg([air,marshall]),pl([air,marshalls]),
   sg([air,marshal]),pl([air,marshals])],de,[]).

n([sg(airco),pl('airco\'s')],de,[]).

n([pl(airmiles),pl([air,miles]),pl('air-miles')],de,[]).

n([sg(airbag),pl(airbags)],de,[]).

n([pl(airbussen),sg(airbus)],de,[]).

n([mass(airconditioning)],de,[]).

n([stem(akkefietje_DIM),sg(akkefietje),pl(akkefietjes)],het,[]).

n([pl(akkers),sg(akker)],de,[],[dim(akkertje)]).

n([mass(akkerbouw)],de,[]).

n([stem(akkoord),pl(akkoorden),sg(akkoord),sg(accoord),pl(accoorden)],het,
  [sbar,
   vp]).

n([stem(akkoord),pl(akkoorden),sg(akkoord),sg(accoord),pl(accoorden)],het,[],
  [h(autonomie),
   concept,
   s(handel),
   kader,
   klimaat,
   loon,
   s(najaar),
   h(principe),
   principe,
   h(regeer),regeer,
   slot,
   s(vrede),
   dim(akkoordje)]).

n([mass(akoestiek)],de,[]).

n([pl(akten),pl(aktes),sg(akte),
   sg(acte),pl(actes),pl(acten)],de,[app_measure],[]).  % acte L.O. tekenen

n([pl(akten),pl(aktes),sg(akte),
   sg(acte),pl(actes),pl(acten)],de,[],[slot]).

n([pl(aktentassen),sg(aktentas)],de,[]).

n([sg(alarm)],het,[],                          % not mass: het tiende alarm
  [bom,
   smog,
   weer]). 

n([pl(albatrossen),sg(albatros)],de,[]).

n([pl(albums),sg(album)],het,
  [np_app_measure,
   app_measure],
  [debuut,
   foto,
   live,
   muziek,
   studio,f([studio]),
   solo,
   verzamel
  ]).

n([mass(alchemie)],de,[]).

n([pl(alchemisten),sg(alchemist)],de,[]).

n([mass(alchimie)],de,[]).

n([pl(alchimisten),sg(alchimist)],de,[]).

n([pl(alcoholen),sg(alcohol),pl(alkoholen),sg(alkohol)],both,[]).

n([mass(alcoholisme),mass(alkoholisme)],het,[]).

n([pl(alcoholisten),sg(alcoholist),pl(alkoholisten),sg(alkoholist)],de,[]).

n([mass(alcoholvrij)],het,[]).  % ik wil een alcoholvrij

n([sg(ale)],de,[],
  [f(pale),
   f(special)
  ]).

n([sg(alert),pl(alerts)],de,[]).

n([sg(aleviet),pl(alevieten)],de,[]).

n([pl('alfa\'s'),sg(alfa)],de,[]).

n([pl(alfabetten),sg(alfabet)],het,[]).

%% wat is juist: alge of alg?
n([pl(algen),sg(alge),sg(alg)],de,[]).

n([sg(algebra),pl('algebra\'s')],de,[]).

n([mass(algemeen)],het,[]).

n([pl(algemeenheden),sg(algemeenheid)],de,[]).

n([sg(algoritme),sg(algorithme),pl(algoritmen),pl(algoritmes),
   pl(algorithmen),pl(algorithmes)],het,[]).

n([sg(alian),pl(alians)],de,[]).  % ?

n([sg(alien),pl(aliens)],de,[]).

n([sg(alias)],both,[]).

n([pl('alibi\'s'),sg(alibi)],het,[sbar]).

n([mass(alimentatie)],de,[]).

n([pl('alinea\'s'),sg(alinea)],de,[]).

n([sg(allee),pl(alleeën)],de,[]).

n([pl(allegaartjes),sg(allegaartje)],het,[measure]).

n([pl(allegorieën),sg(allegorie)],de,[]).

n([sg(allel),pl(allelen)],het,[]).  %  varianten van een gen

n([sg(allergeen),pl(allergenen)],het,[]).

n([pl(allergieën),sg(allergie)],de,[]).

n([mass(alles)],het,[subject_sbar]).  % alles wat ik weet is dat jij werkt voor ...

n([mass(allesbinder)],de,[]).

n([pl(allianties),sg(alliantie)],de,[]).

n([pl(allocaties),sg(allocatie)],de,[]).

n([pl(allochtonen),sg(allochtoon)],de,[]).

%% taal van allochtonen ??!
n([mass(allochtoons)],het,[]).

n([mass(allooi)],het,[pred_pp(van)]).

n([mass(allround),
   mass('all-round'),
   mass([all,round])],both,[]).

n([pl(allrounders),sg(allrounder)],de,[]).

n([pl(allures),sg(allure)],de,[pred_pp(van)]).

n([mass(almacht)],de,[]).

n([pl(almanakken),sg(almanak)],de,[]).

n([mass(alpine)],both,[]).

n([sg(alt),pl(alten)],de,[]).

n([pl(altaren),sg(altaar)],het,[],[dim(altaartje)]).

n([sg([alter,ego]),pl([alter,'ego\'s'])],both,[]).

n([pl(alternatieven),sg(alternatief)],het,
  [vp,
   subject_sbar,
   subject_vp]).

n([pl(altruïsten),sg(altruïst)],de,[]).

n([mass(aluminium)],het,[]).

n([sg(alumnus),pl(alumni)],de,[]).

n([sg(ama),pl('ama\'s')],de,[]).

n([pl(amandelen),pl(amandels),sg(amandel)],de,[],[dim(amandeltje)]).

n([pl(amateurs),sg(amateur)],de,[]).

n([mass(amateurisme)],het,[]).

n([pl(amazonen),pl(amazones),sg(amazone)],de,[]).

n([pl(ambachten),sg(ambacht)],het,[]).

n([pl(ambachtslieden),pl(ambachtslui),sg(ambachtsman)],de,[]).

n([pl(ambassades),sg(ambassade)],de,[]).

n([pl(ambassadeuren),pl(ambassadeurs),sg(ambassadeur)],de,[],
  [h('NAVO'),h('Navo'),'NAVO','Navo']).

n([sg(ambiance)],de,[]).

n([sg(ambiguïteit),pl(ambiguïteiten)],de,[]).

n([pl(ambities),sg(ambitie)],de,[vp,subject_vp,subject_sbar,sbar]).

n([pl(ambivalenties),sg(ambivalentie)],de,[]).

n([pl(ambten),sg(ambt)],het,[]).

n([pl(ambtenaars),pl(ambtenaren),sg(ambtenaar)],de,[app_measure],
  [s(bestuur),
   i(leer_plicht,leerplicht),
   s(opsporing),
   politie,
   s(rijk),
   top,
   trouw]).

n([mass(ambtenarij)],de,[]).

n([pl(ambulancen),pl(ambulances),sg(ambulance)],de,[],
  [i(dier,dieren)]).

n([mass(amen)],het,[]).  % je mag alleen ja en amen zeggen

n([pl(amendementen),sg(amendement)],het,[sbar,vp]).

n([sg(ameublement),pl(ameublementen)],het,[]).

n([pl(amfetaminen),pl(amfetamines),sg(amfetamine)],de,[]).

n([sg(amfibie),pl(amfibieën)],de,[]).

n([pl(aminozuren),sg(aminozuur)],het,[]).

n([mass(ammoniak)],de,[]).

n([pl(amnestieën),sg(amnestie)],de,[]).

n([sg(ampul),pl(ampullen)],de,[measure]).

n([sg(amputatie),pl(amputaties)],de,[]).

n([pl(amuletten),sg(amulet)],both,[]).

n([pl(amusementen),sg(amusement)],het,[]).

n([pl([anabole,steroïden]),sg([anabole,steroïde])],both,[app_measure]).

n([pl(anabolica)],de,[]).

n([pl(anachronismen),sg(anachronisme)],het,[]).

n([pl(analfabeten),sg(analfabeet)],de,[]).

n([pl(analfabetismen),sg(analfabetisme)],het,[]).

n([pl(analisten),sg(analist)],de,[]).

n([pl(analitici),sg(analiticus)],de,[]).

%% de analogie mens/aap
n([pl(analogieën),sg(analogie)],de,[sbar,app_measure]).

n([pl(analysen),pl(analyses),sg(analyse)],de,[sbar,subject_sbar]).

n([pl(analysten),sg(analyst)],de,[]).

n([pl(analytici),sg(analyticus)],de,[]).

n([pl(anamneses),sg(anamnese)],de,[]).

n([pl(ananassen),sg(ananas)],de,[]).

n([mass(anarchie)],de,[]).

n([mass(anarchisme)],het,[]).

n([pl(anarchisten),sg(anarchist)],de,[]).

n([pl(anarchistes),sg(anarchiste)],de,[]).

n([mass(anatomie)],de,[]).

n([sg([ancien,régime]),pl([anciens,régimes])],het,[]).

n([sg(ander)],both,[]).

n([mass(andijvie)],de,[]).

n([stem(anekdote),
   pl(anekdoten),pl(anekdotes),sg(anekdote),
   pl(anecdoten),pl(anecdotes),sg(anecdote)],de,[sbar]).

n([mass(anemie)],de,[]).

n([sg(aneurisma),pl('aneurisma\'s')],het,[]).

n([pl(angels),sg(angel)],de,[],[dim(angeltje)]).

n([mass(angina)],de,[]).

n([mass([angina,pectoris])],de,[]).

n([pl(angsten),sg(angst)],de,[sbar,vp]). 

n([pl(angsten),sg(angst)],de,[],
  [faal,
   i(knop,knoppen)
  ]). 

n([mass([angst,en,beven])],both,[]).

n([pl(angstgevoelens),sg(angstgevoelen)],het,[]).

n([mass(anima)],de,[]).

n([sg(animatie),pl(animaties)],de,[]).

n([mass(animo)],both,[vp]).  % celex: de

n([pl(anjers),sg(anjer)],de,[]).

n([pl(ankers),sg(anker)],het,[],[pred_pp(voor)]).

n([pl(ankers),sg(anker)],het,[measure],[dim(ankertje)]).

n([pl(annalen)],both,[]).

n([sg(annex),pl(annexen)],de,[]).

n([pl(annexaties),sg(annexatie)],de,[]).

n([sg(annotatie),pl(annotaties)],de,[]).

n([sg(annulering),pl(annuleringen)],de,[]).

n([mass(anonimiteit)],de,[]).

n([mass(anorexia)],de,[]).

n([mass([anorexia,nervosa])],de,[]).

n([pl(ansjovissen),sg(ansjovis)],de,[]).

n([pl(antecedenten),sg(antecedent)],het,[]).

n([pl(antennen),pl(antennes),sg(antenne)],de,[],
  [schotel]).

n([stem(anti_RSI_oefening),
   sg('anti-RSI-oefening'),
   pl('anti-RSI-oefeningen')],de,[]).

n([pl(antibiotica),sg(antibioticum)],het,[]).

n([pl(antichristen),sg(antichrist)],de,[]).

n([pl(anticipaties),sg(anticipatie)],de,[]).

n([mass(anticonceptie)],de,[]).

n([stem(anti_conceptivum),
   sg('anti-conceptivum'),pl('anti-conceptiva')],het,[]).

n([sg(antidepressivum),pl(antidepressiva)],het,[]).

n([stem(anti_depressivum),
   sg('anti-depressivum'),pl('anti-depressiva')],het,[]).

n([stem(anti_epilepticum),
   pl('anti-epileptica'),sg('anti-epilepticum')],het,[]).

n([mass(antiek)],het,[]).

n([pl(antikristen),sg(antikrist)],de,[]).

n([pl(antilichamen),sg(antilichaam)],het,[]).

n([sg(antilope),pl(antilopen)],de,[]).  % en niet anti_loop

n([stem(anti_oxidant),
   sg('anti-oxidant'),pl('anti-oxidanten')],de,[]).

n([pl(antipathieën),sg(antipathie)],de,[]).

n([pl(antipatieën),sg(antipatie)],de,[]).

n([pl(antipsychotica)],de,[]).

n([pl(antiquairs),sg(antiquair)],de,[]).

n([pl(antisemieten),sg(antisemiet),pl('anti-semieten'),sg('anti-semiet')],de,[]).

n([mass(antisemitisme),mass('anti-semitisme')],het,[]).

n([pl(antistoffen),sg(antistof)],de,[]).

n([pl(antithesen),pl(antitheses),sg(antithese)],de,[]).

n([mass(antrax)],both,[]).

n([mass(antropologie)],de,[]).

n([pl(antropologen),sg(antropoloog)],de,[]).

n([pl(antwoorden),sg(antwoord)],het,
  [sbar,
   van_sbar,
   vp,
   subject_sbar,
   subject_vp,
   start_app_measure],
  [standaard]).

n([sg(anus),pl(anussen)],de,[]).

n([sg(aorta),pl('aorta\'s')],de,[]).

n([pl(apachen),pl(apaches),sg(apache)],de,[]).

n([mass(apartheid)],de,[]).

n([mass(apathie)],de,[]).

n([mass(apatie)],de,[]).

n([sg('Apeldoorner'),pl('Apeldoorners')],de,[]).

n([pl(aperitieven),sg(aperitief)],both,[],[dim(aperitiefje)]).

n([mass(apezuur)],both,[]).

n([mass(aplomb)],het,[]).

n([pl(apostelen),pl(apostels),sg(apostel)],de,[]).

n([pl(apotheken),sg(apotheek)],de,[]).

n([pl(apothekers),sg(apotheker)],de,[]).

n([pl(apotheosen),sg(apotheose)],de,[]).

n([sg(app),pl(apps)],de,[],
  [dim(appje)]).

n([pl(apparaten),sg(apparaat)],het,[],
  [i(ambtenaar,ambtenaren),
   antwoord,
   s(bestuur),   % en niet bestuur_sap_paraat
   gehoor,
   i(koffie_zet,koffiezet),koffie,
   kopieer,
   s(opsporing),                % en niet opsporing_sap_paraat
   s(overheid),			% en niet overheid_sap_paraat
   politie,
   scheer,
   s(staat),			% en niet staat_sap_paraat
   s(veiligheid),		% en niet veiligheid-sap-paraat
   s(voeding),                  % en niet voeding-sap-paraat
   s(werving),
   dim(apparaatje)]).

n([mass(apparatuur),
   pl(apparaturen)],de,[],
  [afluister,
   computer,
   meet,
   rand,
   telecom]).

n([pl(appartementen),sg(appartement)],het,[],[dim(appartementje)]).

n([pl(appelen),pl(appels),sg(appel)],de,[],[dim(appeltje)]).

n([pl(appels),sg(appel),sg(appèl),pl(appèls)],het,[]).

n([sg('Appel'),pl('Appels')],de,[]).  % hij had echte Appels aan de muur

n([pl(appelbomen),sg(appelboom)],de,[]).

n([mass(appelmoes)],de,[]).

n([mass(applaus)],het,[]).

n([sg(applicatie),pl(applicaties)],de,[]).

n([pl(appreciaties),sg(appreciatie)],de,[]).

n([sg(april)],de,[temp_mod,sbar]).

n([mass([apron,plus])],both,[]).

n([mass(apropos)],het,[pred_pp(van)]).

n([sg([aqua,swing])],de,[]). % rsi related

n([pl(aquarellen),sg(aquarel),
   pl(akwarellen),sg(akwarel)],de,[],[dim(aquarelletje),
				      dim(akwarelletje)]).

n([pl(aquaria),pl(aquariums),sg(aquarium)],het,[]).

n([mass(arbeid)],de,
  [pred_pp(aan)],
  [deeltijd,
   dwang,
   gast,
   i(hand,handen),
   i(kind,kinder),
   s(training),
   i(zend_uit,uitzend)]).

n([pl(arbeiders),sg(arbeider)],de,[],
  [dwang,
   s(fabriek),
   gast,
   haven,
   land]).

n([mass(arbeidsongeschiktheid)],de,[]).

n([pl(arbiters),sg(arbiter)],de,[]).

n([mass(arbitrage)],de,[]).

n([pl(arcaden),pl(arcades),sg(arcade)],de,[]).

n([mass(archeologie)],de,[]).

n([pl(archeologen),sg(archeoloog)],de,[]).

n([pl(archetypen),pl(archetypes),sg(archetype)],het,[]).

n([pl(archieven),sg(archief)],het,[],
  [s(rijk),
   sport]).

n([pl(archipels),sg(archipel)],de,[]).

n([pl(architecten),sg(architect)],de,[]).

n([pl(architectes),sg(architecte)],de,[]).

n([pl(architecturen),sg(architectuur)],de,[],[tempel]).

n([pl(archivarissen),sg(archivaris)],de,[]).

n([pl('area\'s'),sg(area)],de,[]).

n([pl(arealen),sg(areaal)],het,[measure]).

n([pl('arena\'s'),sg(arena)],de,[]).

n([pl(arenden),sg(arend)],de,[],[dwerg,
				 i(slang,slangen),
				 vis,
				 zee]).

n([mass(argeloosheid)],de,[]).

n([pl(argumenten),sg(argument)],het,
  [sbar,subject_sbar,subject_vp,vp],
  [klets,
   tegen]).

n([pl(argumentaties),sg(argumentatie)],de,[sbar]).

n([pl(argusogen)],het,[]).

n([mass(argwaan)],de,[]).

n([pl('aria\'s'),sg(aria)],de,[]).

n([pl(aristocraten),sg(aristocraat)],de,[]).

n([pl(aristocratieën),sg(aristocratie)],de,[]).

n([pl(aristokraten),sg(aristokraat)],de,[]).

n([pl(aristokratieën),sg(aristokratie)],de,[]).

n([pl(arken),sg(ark)],de,[]).

n([pl(armen),sg(arm)],de,[],
  [boven,
   draag,
   linker,
   onder,
   rechter,
   dim(armpje)]).

n([pl('armada\'s'),sg(armada)],de,[]).

n([pl(armbanden),sg(armband)],de,[]).

n([pl(armgebaren),sg(armgebaar)],het,[]).

n([mass(armoe)],de,[]).

n([mass(armoede)],de,[pred_pp(van)]).

n([sg(armoedzaaier),pl(armoedzaaiers)],de,[]).

n([mass(armslag)],de,[]).

n([pl('aroma\'s'),sg(aroma)],het,[]).

n([pl(arrangementen),sg(arrangement)],het,[]).

n([sg(arrangeur),pl(arrangeurs)],de,[]).

n([pl(arresten),sg(arrest)],het,[],
  [huis,
   h('Bosman'),
   wh(['Pikmeer','II']),
   wh(['Pikmeer','2']),
   h('Securitel')]).

n([pl(arresten),sg(arrest)],het,
  [start_app_measure,
   app_measure,
   np_app_measure],
  []).

n([pl(arrestanten),sg(arrestant)],de,[]).

n([pl(arrestaties),sg(arrestatie)],de,[]).

n([mass(arrogantie)],de,[vp]).

n([pl(arrondissementen),sg(arrondissement)],het,[]).

n([pl(arsenalen),sg(arsenaal)],het,[measure]).

n([mass(art)],de,[],
  [f([aboriginal]),
   f([body]),
   f([conceptual]),
   f([high]),
   f([low]),
   f([minimal]),
   f([op]),
   f([outsider]),
   f([pop]),
   f([street])]).

n([mass([art,brut])],de,[]).

n([mass([art,deco])],de,[]).

n([mass([art,direction])],de,[]).

n([sg([art,director]),pl([art,directors])],de,[]).

n([mass([art,nouveau])],de,[]).

n([sg(artefact),pl(artefacten)],both,[]).

n([pl(artiesten),sg(artiest),sg(artieste)],de,[]).

n([pl(artikelen),pl(artikels),sg(artikel)],het,
  [],
  [feest,
   hoofd,
   kantoor,
   i(krant,kranten),
   i(weg_werp,wegwerp),
   dim(artikeltje)]).

n([pl(artikelen),pl(artikels),sg(artikel)],het,
  [start_app_measure,
   app_measure,
   np_app_measure],
  [dim(artikeltje)]).

n([mass(artillerie)],de,[]).

n([sg(artisjok),pl(artisjokken)],de,[]).

n([sg(artritis)],de,[]).

n([mass(artrose)],de,[]).

n([pl(artsen),sg(arts)],de,[],
  [s(bedrijf),
   s(bond),
   club, % vdh
   i(dier,dieren),
   huis,
   s(keuring),
   i(kind,kinder),
   lijf,
   natuur,
   oog,
   oor,
   ploeg,
   schouw,
   school,
   sport,
   tand,
   tropen,
   vastgoed,
   vee,
   s(verzekering)]).

n([pl(assen),sg(as)],de,[],[aandrijf]).

%% het as:
%% https://www.neerlandistiek.nl/2018/10/het-as/
n([mass(as)],both,[pred_pp(in)],[]).

n([pl(asbakken),sg(asbak)],de,[]).

n([mass(asbest)],both,[]).

n([pl(asceten),sg(asceet)],de,[]).

n([mass(ascese)],de,[]).

n([mass(asfalt)],het,[]).

n([pl(asielen),sg(asiel),mass(asiel)],het,[]).

n([pl(aspecten),sg(aspect)],het,[sbar]).

n([pl(asperges),sg(asperge)],de,[]).

n([pl(aspiranten),sg(aspirant),
   pl(adspiranten),sg(adspirant)],de,[]).

n([pl(aspiraties),sg(aspiratie)],de,[vp]).

n([pl(aspirines),sg(aspirine)],de,[]).

n([pl(assemblages),sg(assemblage)],de,[]).

n([sg(asshole),pl(assholes)],de,[]).

n([pl(assimilaties),sg(assimilatie)],de,[]).

n([sg(assist),pl(assists)],de,[]).

n([pl(assistenten),sg(assistent)],de,[app_measure],
  [h(student)]).

n([pl(assistenten),sg(assistent)],de,[],
  [politie]).

n([pl(assistentes),sg(assistente)],de,[app_measure]).

n([mass(assistentie)],de,[]).

n([pl(associaties),sg(associatie)],de,[sbar]).

n([pl(assortimenten),sg(assortiment)],het,[measure]).

n([sg(aster),pl(asters)],de,[],
  [zee]).

n([mass(astma)],both,[]).

n([mass(astrologie)],de,[]).

n([pl(astrologen),sg(astroloog)],de,[]).

n([pl(astronauten),sg(astronaut)],de,[],[dim(astronautje)]).

n([mass(astronomie)],de,[]).

n([pl(astronomen),sg(astronoom)],de,[]).

n([pl(asylen),sg(asyl)],het,[]).

n([pl(ateliers),sg(atelier)],het,[]).

n([pl(athenea),pl(atheneums),sg(atheneum)],het,[]).

n([pl(atheïsten),sg(atheïst)],de,[]).

n([pl(atlassen),sg(atlas)],de,[]).

n([pl(atleten),sg(atleet)],de,[]).

n([pl(atletes),sg(atlete)],de,[]).

n([mass(atletiek)],de,[],[weg]).

%% een druk van vijf atmosfeer
n([pl(atmosferen),meas(atmosfeer)],de,[]).

n([pl(atollen),sg(atol)],both,[]).

n([pl(atomen),sg(atoom)],het,[],[dim(atoompje)]).

n([pl(atoomwapens),sg(atoomwapen)],het,[]).

n([pl(attachés),pl('attaché\'s'),sg(attaché)],de,[]).

n([pl(attenties),sg(attentie)],de,[]).

n([sg(attest),pl(attesten)],both,[]).

n([pl(attituden),pl(attitudes),sg(attitude)],de,[]).

n([sg([attorney,general]),sg('attorney-general')],de,[]).

n([pl(attracties),sg(attractie)],de,[]).

n([pl(attributen),sg(attribuut)],het,[sbar,vp]).

%% ik hoorde iemand au schreeuwen...
n([mass(au)],het,[]).

n([pl([au,pairs]),sg([au,pair])],de,[]).

n([pl(aubergines),sg(aubergine)],de,[]).

n([mass(audio)],de,[]).

n([sg([audio,cd]),
   pl([audio,'cd\'s'])],de,[]).

n([sg(audit),pl(audits)],de,[]).

n([sg(auditeur),pl(auditeuren)],de,[]).

n([pl(audities),sg(auditie)],de,[]).

n([pl(auditoria),pl(auditoriums),sg(auditorium)],het,[]).

n([pl(audiënties),sg(audiëntie)],de,[]).

n([mass(audiëntiezaal)],de,[]).

n([pl(augurken),sg(augurk)],de,[],[dim(augurkje)]).

n([sg(augustus)],de,[temp_mod,sbar]).

n([pl('aula\'s'),sg(aula)],de,[]).

n([pl('aura\'s'),sg(aura)],both,[]).    % celex: de

n([sg(aurelia),pl('aurelia\'s')],de,[]).

n([pl(aureolen),sg(aureool)],both,[]).  % celex: de

n([mass(aurora)],de,[]).

n([pl(auspiciën)],de,[]).

n([mass(autenticiteit),
   mass(authenticiteit)],de,[]).

n([pl(auteurs),sg(auteur)],de,[],[thriller]).

n([mass(autisme)],het,[]).

n([pl('auto\'s'),sg(auto)],de,[],
  [ambulance,
   bestel,
   brandweer,
   dienst,
   diesel,
   hof,
   huur,
   kampeer,
   kiep,
   i(persoon,personen),
   politie,
   race,
   test,
   trap,
   vlucht,
   vracht,
   vuilnis,
   i(ziek,zieken),
   dim(autootje)]).

n([pl(autobanden),sg(autoband)],de,[]).

n([pl(autobiografieën),sg(autobiografie)],de,[]).

n([sg(autochtoon),pl(autochtonen)],de,[]).

n([sg([automated,people,mover]),pl([automated,people,movers])],de,[]).

n([pl(automaten),sg(automaat)],de,[],
  [bank,
   betaal,
   geld,
   gok,
   pin,
   speel]).

n([sg(automatiseerder),pl(automatiseerders)],de,[]).

n([mass(automatisering)],de,[]).

n([pl(automatismen),sg(automatisme)],het,[sbar,vp]).

n([mass(automatizering)],de,[]).

n([pl(automerken),sg(automerk)],het,[]).

n([pl(automobielen),sg(automobiel)],both,[]).

n([pl(automobilisten),sg(automobilist)],de,[]).

n([pl(automobilistes),sg(automobiliste)],de,[]).

n([mass(autonomie)],de,[],
  [s(bestuur)]).

n([mass(autopsie)],de,[]).

n([pl(autoriteiten),sg(autoriteit)],de,
  [measure,
   vp],[]).

n([pl(autoriteiten),sg(autoriteit)],de,[],
  [beurs,
   douane,
   s(mededinging)]).

n([mass(autoverkeer)],het,[]).

n([sg(ava)],de,[]).

n([pl(avances),sg(avance)],de,[]).

n([pl('avant-gardes'),sg('avant-garde'),
   sg([avant,garde]),pl([avant,gardes]),
   sg(avantgarde),pl(avantgardes)],de,[]).

n([pl(avenues),sg(avenue)],de,[]).

n([mass(averij)],de,[]).

n([pl(aversies),sg(aversie)],de,[]).

n([sg(avocado),pl('avocado\'s')],de,[]).

n([sg(avond),pl(avonden)],de,[measure,temp_mod,sbar],
  [zondag,
   maandag,
   dinsdag,
   woensdag,
   donderdag,
   vrijdag,
   zaterdag,
   feest,
   film,
   inspraak,
   kerst,
   klaverjas,
   koop,
   s(oudejaar),
   s(oudjaar),
   s(verkiezing),
   winter,
   zomer,
   dim(avondje)]).

n([mass(avondeten)],het,[]).

n([pl(avondklokken),sg(avondklok)],de,[]).

n([mass(avondlucht)],de,[]).

n([mass(avondmaal)],het,[]).

n([sg(avondspits),pl(avondspitsen)],de,[]).

n([pl(avonduren),sg(avonduur)],het,[]).

n([pl(avonturiers),sg(avonturier)],de,[]).

n([pl(avonturen),sg(avontuur)],het,[],[dim(avontuurtje)]).

n([sg(award),pl(awards),
   sg('Award'),pl('Awards')],de,[]).

n([pl('axioma\'s'),pl(axiomata),sg(axioma)],het,[sbar]).

n([pl(ayatollahs),sg(ayatollah)],de,[]).

n([sg(azc),pl('azc\'c')],het,[]).

%% scheikunde
n([sg(azeotroop),pl(azeotropen)],de,[]).

n([mass(azijn)],de,[]).

n([pl('b\'s'),sg(b)],de,[],[dim('b\'tje')]).

n([sg('B&B'),pl('B&Bs'),pl('B&B\'s')],de,[]).

n([sg(ba)],de,[]).  % de ziel van Ra

n([pl(baaien),sg(baai)],de,[],[dim(baaitje)]).

n([pl(balen),sg(baal)],de,[measure],[dim(baaltje)]).

n([pl(banen),sg(baan)],de,[pred_pp(van),measure]).  % een baan voorsprong

n([pl(banen),sg(baan)],de,[],
  [acht,
   bij,
   binnen,
   buiten,
   bus,
   deeltijd,
   droom,
   glij,
   h('Melkert'),
   roei,
   schaats,
   sintel,
   tram,
   wieler,
   zenuw,
   zweef,
   dim(baantje)]).

n([pl(baren),sg(baar)],de,[measure],[dim(baartje)]).  % ook: een baar goud / in baar goud

n([pl(baarden),sg(baard)],de,[],
  [punt,
   stoppel,
   dim(baardje)]).

n([pl(baarmoeders),sg(baarmoeder)],de,[]).

n([pl(baarzen),sg(baars)],de,[],
  [kardinaal,
   nijl,
   snoek]).

n([pl(bazen),sg(baas)],de,[],
  [café,
   kroeg,
   maffia,
   i(plaat,platen),
   ploeg,
   politie,
   team,
   zet,
   dim(baasje)]).

n([pl(baten),sg(baat)],de,[]).

n([mass(bate)],de,[]).

n([pl('baba\'s'),sg(baba)],de,[]).

n([pl(babbels),sg(babbel)],de,[],[dim(babbeltje)]).

n([sg(babe),pl(babes)],de,[]).

n([mass([babi,pangang])],de,[]).

n([pl('baby\'s'),sg(baby)],de,[],[dim('baby\'tje'),dim(babietje)]).

n([sg(babyboomer),pl(babyboomers)],de,[]).

n([sg(babysit),pl(babysits)],de,[]).

n([mass(baccarat)],het,[]).

n([sg(bacchant),pl(bacchanten)],de,[]).

n([sg(bachelor)],de,[]).

n([sg(back),pl(backs)],de,[]).  % Laseroms

n([sg(backgammon)],het,[]).

n([pl(backhands),sg(backhand)],de,[]).

n([sg(backstage)],de,[]).

n([sg('back-up'),pl('back-ups'),
   sg(backup),pl(backups)],de,[]).

n([mass(bacon)],both,[]).

n([pl(bacteriën),sg(bacterie)],de,[],
  [legionella,
   miltvuur]).

n([pl(baden),sg(bad)],het,[],
  [bubbel,
   dim(badje)]).

n([sg([bad,boy]),pl([bad,boys])],de,[]).

n([sg([bad,guy]),pl([bad,guys])],de,[]).

n([sg([bad,trip]),pl([bad,trips])],de,[]).

n([sg(badge),pl(badges)],de,[]).

n([pl(badhanddoeken),sg(badhanddoek)],de,[]).

n([pl(badjassen),sg(badjas)],de,[]).

n([pl(badkamers),sg(badkamer)],de,[]).

n([pl(badkuipen),sg(badkuip)],de,[]).

n([pl(badmeesters),sg(badmeester)],de,[]).

n([mass(badminton)],het,[]).

n([sg(badmintonner),pl(badmintonners)],de,[]).

n([sg(badmintonster),pl(badmintonsters)],de,[]).

n([pl(badpakken),sg(badpak)],het,[]).

n([mass(badwater)],het,[]).

n([mass(bagage)],de,[],
  [hand,
   over,
   ruim]).

n([pl(bagagedragers),sg(bagagedrager)],de,[]).

n([pl(bagatelles),sg(bagatelle)],de,[]).

n([mass(bagger)],de,[]).

n([sg(baggeraar),pl(baggeraars)],de,[]).

n([meas(baht)],de,[meas_mod,measure]). % munt Thailand

n([sg(bajes)],de,[]).

n([pl(bajonetten),sg(bajonet)],de,[]).

n([pl(bakken),sg(bak)],de,[measure],
  [dim(bakje)]).

n([pl(bakken),sg(bak)],de,[],
  [achter,
   afval,
   i(bal,ballen),
   koffer,
   licht,
   s(versnelling)]).

n([mass(bakboord)],het,[]).

n([pl(bakens),sg(baken)],het,[]).

n([sg(baker),pl(bakers)],de,[]).

n([mass(bakermat)],de,[]).

n([pl(bakfietsen),sg(bakfiets)],de,[]).

n([pl(bakkebaarden),sg(bakkebaard)],de,[]).

n([pl(bakkers),sg(bakker)],de,[]).

n([pl(bakkerijen),sg(bakkerij)],de,[],[dim(bakkerijtje)]).

n([pl(bakstenen),sg(baksteen)],both,[]).

n([pl(ballen),sg(bal)],de,[measure],  % een bal gehakt; mokkaijs cdb/3422
  [dim(balletje)]).

n([pl(ballen),sg(bal)],de,[],
  [biljart,
   sneeuw,
   straf,
   zaad,
   dim(balletje)]).

n([pl(bals),sg(bal)],het,[]).

n([pl(balansen),sg(balans)],de,[pred_pp(in)],
  [tussen]).

n([pl(balansen),sg(balans)],de,[],
  [s(betaling),
   s(handel),
   week]).

n([pl(baldakijnen),pl(baldakijns),sg(baldakijn),sg('Baldakijn')],het,[]).

n([pl(balies),sg(balie)],de,[],
  [incheck,
   ticket,
   transfer]).

n([sg(balg),pl(balgen)],de,[]).

n([pl(baljuws),sg(baljuw)],de,[]).

n([pl(balken),sg(balk)],de,[],
  [dim(balkje),
   adres,
   hijs,
   taak]).

n([pl(balkons),sg(balkon)],het,[],[dim(balkonnetje)]).

n([pl(balladen),pl(ballades),sg(ballade)],de,[]).

n([mass(ballast)],de,[]).

n([pl(balletten),sg(ballet)],het,[]).

n([sg([ballet,'d\'action'])],het,[]).

n([pl(ballingen),sg(balling)],de,[]).

n([mass(ballingschap)],de,[pred_pp(in)]).

n([pl(ballons),sg(ballon),pl(ballonnen)],de,[],[dim(ballonnetje)]).

n([sg(ballonvaarder),pl(ballonvaarders)],de,[]).

n([pl(ballpoints),sg(ballpoint)],de,[]).

n([pl(balpennen),sg(balpen)],de,[]).

n([pl(balsems),sg(balsem)],de,[]).

n([pl(balustraden),pl(balustrades),sg(balustrade)],de,[]).

n([pl(bamboes),sg(bamboe)],both,[]).

n([pl(bannen),sg(ban)],de,[pred_pp(in)]).

n([pl(bananen),sg(banaan)],de,[],[dim(banaantje)]).

n([pl(banaliteiten),sg(banaliteit)],de,[sbar,pred_pp(van)]).

n([mass(banco)],het,[]).

n([pl(banden),sg(band)],de,[],
  [s(aanvoerder),
   familie,
   kruis,
   lei,
   pols, % en niet pol_band_DIM
   winter,
   dim(bandje)]).

n([sg(bande),pl(bandes)],de,[]).

n([sg(band),pl(bands)],de,[],
  [boy,
   rock,
   brass,
   marching,
   dim(bandje)]).

n([sg(bandoneón),pl(bandoneóns)],de,[]).

n([pl(bandbreedten),pl(bandbreedtes),sg(bandbreedte)],de,[]).

n([pl(bandieten),sg(bandiet)],de,[],[dim(bandietje)]).

n([mass(banditisme)],het,[],[]).

n([pl(bandopnamen),pl(bandopnames),sg(bandopname)],de,[]).

n([pl(bandrecorders),sg(bandrecorder)],de,[]).

n([sg(banger),pl(bangers)],de,[]).

n([pl(banieren),sg(banier)],both,[],[dim(baniertje)]).

n([sg(banjo),pl('banjo\'s')],de,[]).

%% geld-instelling
n([pl(banken),sg(bank)],de,[],
  [i(effect,effecten),
   hypotheek,
   s(investering),
   s(ontwikkeling),
   landbouw,
   spaar,
   s(staat),
   i(zaak,zaken)]).

%% zit-gelegenheid
n([pl(banken),sg(bank)],de,[],
  [achter,
   i(beklagen,beklaagden),
   reserve,
   rust,
   school,
   slacht,
   straf,
   toon,
   venster,
   voor,
   werk,
   dim(bankje)]).

%% verzameling
n([pl(banken),sg(bank)],de,[],
  [bloed,
   data,
   sperma,
   i(gegeven,gegevens)]).

%% geografisch
n([pl(banken),sg(bank)],de,[],
  [mist,
   zand,
   zee
  ]).

n([pl(banketten),sg(banket)],het,[]).

n([pl(banketbakkers),sg(banketbakker)],de,[]).

n([sg(banker),pl(bankers)],de,[]).

n([pl(bankiers),sg(bankier)],de,[],[huis]).

n([pl(bankovervallen),sg(bankoverval)],de,[]).

n([pl(bankrekeningen),sg(bankrekening)],de,[]).

n([mass(bankroet)],het,[]).

n([pl(bankstellen),sg(bankstel)],het,[]).

n([sg(bankverzekeraar),pl(bankverzekeraars)],de,[]).

n([mass(bankwezen)],het,[]).

n([mass(banning)],de,[]).

n([pl(bars),sg(bar)],de,[],
  [disco,
   koffie,
   mini
  ]).

% luchtdruk
n([meas(bar)],de,[meas_mod,measure],
  [milli]).

n([pl(barakken),sg(barak)],de,[]).

%% dorp Filippijnen
n([sg(barangay),pl(barangays)],de,[]).

n([pl(barbaren),sg(barbaar)],de,[]).

n([mass(barbarij)],de,[]).

n([pl(barbecues),sg(barbecue)],de,[]).

n([pl(barbituraten),sg(barbituraat)],het,[]).

n([pl(baretten),sg(baret)],de,[]).

n([pl(baritons),sg(bariton)],de,[]).

n([pl(barkeepers),sg(barkeeper)],de,[]).

n([pl(barkeepsters),sg(barkeepster)],de,[]).

n([pl(barkrukken),sg(barkruk)],de,[]).

n([pl(barmannen),sg(barman)],de,[]).

n([pl(barmhartigheden),sg(barmhartigheid)],de,[]).

n([mass(barnsteen)],het,[]).

n([mass(barok)],de,[]).

n([sg(barokker),pl(barokkers)],de,[]).

n([sg(barometer),pl(barometers)],de,[],[beurs]).

n([pl(baronnen),pl(barons),sg(baron)],de,[],[partij]).

n([pl(baronessen),sg(barones)],de,[]).

n([pl(barrages),sg(barrage)],de,[]).

n([pl(barricaden),pl(barricades),sg(barricade)],de,[]).

n([pl(barrikaden),pl(barrikades),sg(barrikade)],de,[]).

n([pl(barrières),sg(barrière)],de,[]).

n([pl(barsten),sg(barst)],de,[]).

n([sg(bartender),pl(bartenders)],de,[]).

n([pl(bassen),sg(bas)],de,[],[contra]).

n([mass(basalt)],both,[]).

n([pl(basen),sg(base)],de,[]).

n([sg(baseball),pl(baseballen)],both,[]).

n([sg(baseline),pl(baselines)],de,[]).

%% de basket: ring etc
%% het basket: basketbal (vlaams?)
n([sg(basket),pl(baskets)],both,[]).

n([pl(basketballen),sg(basketbal)],de,[],
  []).

n([mass(basketbal)],het,[],
  [prof]).

n([sg(basketballer),pl(basketballers)],de,[],
  [prof]).

n([mass(basketbal)],het,[]).

n([pl(basilieken),sg(basiliek)],de,[]).

n([mass(basilicum)],de,[]).

n([pl(bases),pl(basissen),sg(basis)],de,[vp]).

n([pl(bases),pl(basissen),sg(basis)],de,[],
  [huur,
   leger,
   luchtmacht,
   s(macht),
   marine,
   thuis,
   uitval, s(uitval),
   vlieg,
   water]).

n([sg([basis,uurtarief]),pl([basis,uurtarieven])],het,[]).

n([pl(bassins),sg(bassin)],het,[]).

n([pl(bassisten),sg(bassist)],de,[]).

n([pl(basten),sg(bast)],de,[]).

n([pl(bastaarden),pl(bastaards),sg(bastaard)],de,[]).

n([sg(bastion),pl(bastions)],het,[]).

n([sg('BAT'),pl('BAT\'s')],de,[]).

n([sg(batakker),pl(batakkers),sg('Batakker'),pl('Batakkers')],de,[]).

n([pl(bataljons),sg(bataljon)],het,[measure]).

n([pl(batterijen),sg(batterij)],de,[measure],[dim(batterijtje)]).

n([pl(batterijen),sg(batterij)],de,[],
  [leg,
   dim(batterijtje)]).

n([pl(bazaars),sg(bazaar)],de,[],[dim(bazaartje)]).

n([pl(bazars),sg(bazar)],de,[],[dim(bazartje)]).

n([pl(bazinnen),sg(bazin)],de,[],[dim(bazinnetje)]).

n([sg(bazuin),pl(bazuinen)],de,[]).

n([sg(bbp)],het,[]).

n([pl(beambten),sg(beambte)],de,[],[]).

n([mass(beantwoording)],de,[]).

n([sg(beat),pl(beats)],de,[]).

n([mass(beaujolais)],de,[]).

n([pl('beauty\'s'),sg(beauty)],de,[]).

n([pl(bebouwingen),sg(bebouwing)],de,[]).

n([pl(bedden),sg(bed)],het,[],
  [dek,
   kraam,
   lucht,
   sterf,
   i(twee_persoon,tweepersoons),
   veld,
   water,
   ziek,
   ziekenhuis,
   dim(bedje)]).

n([sg([bed,'&',breakfast]),pl([bed,'&',breakfasts])],both,[]).

n([mass(beddegoed)],het,[]).

n([pl(beddingen),sg(bedding)],de,[],
  [rivier]).

n([pl(beden),sg(bede)],de,[vp]).

n([sg(bedel),pl(bedels)],de,[]).  % voor aan je armband

n([pl(bedelaars),sg(bedelaar)],de,[],[dim(bedelaartje)]).

n([sg(bedenker),pl(bedenkers)],de,[]).

n([pl(bedenkingen),sg(bedenking)],de,[sbar]).

n([pl(bedenksels),sg(bedenksel)],het,[]).

n([mass(bederf)],het,[]).

n([pl(bedevaarten),sg(bedevaart)],de,[]).

n([pl(bedienden),pl(bediendes),sg(bediende)],de,[],
  [kantoor]).

n([pl(bedieningen),sg(bediening)],de,[]).

n([pl(bedingen),sg(beding)],het,[sbar],
  ['anti-speculatie',
   concurrentie,
   s(verblijving)
  ]).

n([sg(bedoeïen),pl(bedoeïenen)],de,[]).

n([pl(bedoelingen),sg(bedoeling)],de,
  [sbar,
   subject_sbar,
   subject_vp,
   vp]).

n([pl(bedoeningen),sg(bedoening)],de,[]).

n([pl(bedragen),sg(bedrag)],het,[],
  [geld,
   norm,
   overname,
   record,
   schade,
   totaal,
   transfer]).

%% to avoid bed_reiger
n([sg(bedreiger),pl(bedreigers)],de,[]).

n([pl(bedreigingen),sg(bedreiging)],de,[sbar,vp]).

n([pl(bedriegers),sg(bedrieger)],de,[]).

n([pl(bedrijven),sg(bedrijf)],het,[pred_pp(in), pred_pp(buiten)]).

n([pl(bedrijven),sg(bedrijf)],het,[],
  [auto,
   s(automatisering),
   bagger,
   bank,
   bel,
   s(berging),
   beurs,
   biotechnologie,
   s(beveiliging),
   i(boer,boeren),
   bouw,
   bus,
   caravan,
   chemie,
   computer,
   container,
   i(container_overslag,containeroverslag),
   diamant,
   distributie,
   dochter,
   s(elektriciteit),i(elektriciteit,electriciteits),
   i(elektronica,electronica),elektronica,
   energie,
   familie,
   fusie,
   garage,
   gas,
   groothandel,
   s(groothandel),
   i(groot_winkel,grootwinkel),
   haven,
   i(hoek_man,hoekmans),
   installatie,
   internet,'Internet',
   kabel,
   klein,
   s(koerier),
   landbouw,
   media,
   metro,
   moeder,
   nuts,
   olie,
   omroep,
   f([openbaar,vervoer]),
   [openbaar,vervoer],
   f([openbaar,vervoers]),
   [openbaar,vervoers],
   s(overheid),
   overslag,
   pluimvee,
   post,
   postorder,
   productie,i(productie,produktie),
   recreatie,
   reïntegratie,
   schade,
   schoonmaak,
   software,
   spoor,
   i(spraak_technologie,spraaktechnologie),
   staal,
   s(staat),
   stroom,
   supermarkt,
   i(taal_technologie,taaltechnologie),
   taxi,
   tech,
   technologie,
   telecom,
   telecommunicatie,
   telefonie,
   telefoon,
   touringcar,
   transport,
   s(varken),
   i(vee_voeder,veevoeder),
   s(vervoer),vervoer,
   s(verzekering),
   s(voeding),
   vuurwerk,
   water,
   waterleiding,
   zuivel,
   dim(bedrijfje)]).

n([mass(bedrijfskunde)],de,[]).

n([mass(bedrijfsleven)],het,[]).

n([pl(bedrijfspoten),sg(bedrijfspoot)],de,[app_measure]).

n([pl(bedrijfsschappen),sg(bedrijfsschap)],het,[app_measure]).

n([pl(bedrijfstakken),sg(bedrijfstak)],de,[app_measure]).

n([mass(bedrijfsuitoefening)],de,[]).

n([mass(bedrijfsvoering)],de,[]).

n([sg(bedrijver),pl(bedrijvers)],de,[]).

n([mass(bedrijvigheid)],de,[]).

n([mass(bedrog)],het,[],
  [s(kiezer),
   zelf]).

n([pl(bedsteden),sg(bedstee)],de,[],[dim(bedsteetje)]).

n([mass(bedwang)],het,[]).

n([mass(beef)],both,[]).

n([pl(beken),sg(beek)],de,
  [np_app_measure],
  [dim(beekje)]).

n([pl(beelden),sg(beeld)],het,[pred_pp(in)],[]).

n([pl(beelden),sg(beeld)],het,
  [sbar,
   vp],
  [amateur,
   camera,
   cliché,
   droom,
   ideaal,
   mens,
   schrik,
   spook,
   s(tijd),
   toekomst,
   toon,
   totaal,
   wereld,
   zelf,h(zelf),
   zinne,
   dim(beeldje)]).

n([pl(beelden),sg(beeld)],het,[],
  ['Maria',
   archief,
   boeddha,
   boeg,
   borst,
   film,
   s(god),
   klank,
   kruis,
   mode,
   spel,
   spiegel,
   s(stad),
   stand,
   straat,
   toneel,
   televisie,h(tv),tv,i(tv,'TV-'),f([tv]),
   video,
   weer,
   ziekte,
   dim(beeldje)]).


n([pl(beeldbuizen),sg(beeldbuis)],de,[]).

n([pl(beeldhouwers),sg(beeldhouwer)],de,[]).

n([pl(beeldschermen),sg(beeldscherm)],het,[]).

n([mass(beeldschermwerkplek),pl(beeldschermwerkplekken)],de,[]).

n([pl(beeldspraken),sg(beeldspraak)],de,[]).

n([mass(beeldvorming)],de,[]).

n([pl(beeltenissen),sg(beeltenis)],de,[]).

n([pl(beenderen),pl(benen),sg(been)],het,[],
  [achter,
   borst,
   boven,
   dij,
   juk,
   kaak,
   kraak,
   linker,
   onder,
   rechter,
   scheen,
   sleutel,
   spaak,
   dim(beentje)]).

n([mass(beenmerg)],het,[]).

n([pl(beren),sg(beer)],de,[],
  [panda,
   steun,
   dim(beertje)]).

n([pl(beesten),sg(beest)],het,[],[dim(beestje)]).

n([pl(beten),sg(beet)],de,[]).

n([pl(beetjes),sg(beetje)],het,[mod,measure]).

n([sg(bef),pl(beffen)],de,[]).

n([pl(begaafdheden),sg(begaafdheid)],de,[vp]).

n([pl(begeerten),pl(begeertes),sg(begeerte)],de,[vp]).

n([pl(begeleiders),sg(begeleider)],de,[],
  [s(sterven),
   trein]).

n([pl(begeleidsters),sg(begeleidster)],de,[]).

n([pl(begeleidingen),sg(begeleiding)],de,[]).

n([pl(begijnen),sg(begijn)],de,[]).

n([sg(begin)],het,[temp_mod,sbar],[dim(beginnetje)]).

n([pl(beginfasen),pl(beginfases),sg(beginfase)],de,[]).

n([pl(beginnelingen),sg(beginneling)],de,[]).

n([pl(beginners),sg(beginner)],de,[]).

n([pl(beginselen),pl(beginsels),sg(beginsel)],het,[sbar],
  [s(subsidiariteit),
   s(voorzorg),
   i(woon_land,woonland)]).

n([pl(beginselprograms),sg(beginselprogram)],het,[]).

n([pl(beginselverklaringen),sg(beginselverklaring)],de,[]).

n([pl('begonia\'s'),sg(begonia)],de,[]).

n([pl(begrafenissen),sg(begrafenis)],de,[]).

n([sg(begrenzer),pl(begrenzers)],de,[],[s(snelheid)]).

n([pl(begrenzingen),sg(begrenzing)],de,[]).

n([pl(begrippen),sg(begrip)],het,[app_measure,sbar,vp]).

n([pl(begroeiingen),sg(begroeiing)],de,[]).

n([pl(begroetingen),sg(begroeting)],de,[]).

n([pl(begrotingen),sg(begroting)],de,[],[ontwerp,s(rijk)]).

n([pl(begrotingstekorten),sg(begrotingstekort)],het,[]).

n([pl('beha\'s'),sg(beha)],de,[]).

n([mass(behagen)],het,[]).

n([sg(behandelaar),pl(behandelaars)],de,[],
  [hoofd]).

n([pl(behandelingen),sg(behandeling)],de,[pred_pp(in),
					  pred_pp(onder)]).

n([pl(behandelingen),sg(behandeling)],de,[],
  [s(begroting),
   dag,
   dwang,
   klacht,
   s(voorkeur)]).

n([pl(behangen),sg(behang)],het,[],[dim(behangetje)]).

n([mass(beharing)],de,[],
  [over]). % en niet over_beha_ring

n([mass(behartiging)],de,[],
  [i(belang,belangen)]).

n([sg(behartiger),pl(behartigers)],de,[],
  [i(belang,belangen)]).

n([mass(beheer)],het,[],
  [keten,  % en geen keet_beheer
   milieu,
   natuur,
   s(vermogen),
   wan,
   water % WA
  ]).

n([pl(beheerders),sg(beheerder)],de,[],
  [fonds,
   korps,
   net,
   spoor,
   s(vermogen)]).

n([mass(beheersing)],de,[],
  [kosten,
   taal]).

n([sg(beheerster),pl(beheersters)],de,[]).

n([pl(behendigheden),sg(behendigheid)],de,[vp]).

n([mass(behoef)],het,[]).

n([pl(behoeften),pl(behoeftes),sg(behoefte)],de,[vp]).

n([pl(behoeften),pl(behoeftes),sg(behoefte)],de,[],
  [s(financiering),
   zorg]).

n([mass(behoud)],het,[],
  [s(lijf)]).

n([pl(behuizingen),sg(behuizing)],de,[],[computer]).

n([pl(beiaardiers),sg(beiaardier)],de,[]).

n([mass(beige)],het,[]).

n([sg([beige,book])],het,[]).

n([pl(beitels),sg(beitel)],de,[],[dim(beiteltje)]).

n([sg(beits)],de,[]).

n([pl(bejaardenhuizen),sg(bejaardenhuis)],het,[np_app_measure]).

n([pl(bejaardentehuizen),sg(bejaardentehuis)],het,[]).

n([pl(bejegeningen),sg(bejegening)],de,[]).

n([pl(bekken),sg(bek)],de,[]).

n([pl(bekeerlingen),sg(bekeerling)],de,[]).

n([mass(bekendheid),pl(bekendheden),sg(bekendheid)],de,[]).

n([pl(bekendmakingen),sg(bekendmaking)],de,[sbar,vp]).

n([pl(bekentenissen),sg(bekentenis)],de,[sbar,vp]).

n([pl(bekers),sg(beker)],de,[measure],
  [dim(bekertje)]).

n([pl(bekers),sg(beker)],de,[],
  [h('KNVB'),
   voetbal]).

n([pl(bekeringen),sg(bekering)],de,[]).

n([pl(bekeuringen),sg(bekeuring)],de,[]).

n([pl(bekkens),sg(bekken)],het,[]).

n([sg(bekladding),pl(bekladdingen)],de,[]).

n([mass(beklag)],het,[]).

n([sg(bekleder),pl(bekleders)],de,[]).

n([pl(bekledingen),sg(bekleding)],de,[]).

n([pl(beklemmingen),sg(beklemming)],de,[]).

n([sg(beklimmer),pl(beklimmers)],de,[],
  [berg]).

n([pl(beklimmingen),sg(beklimming)],de,[]).

n([pl(bekommernissen),sg(bekommernis)],de,[sbar,vp,subject_sbar,subject_vp]).

n([mass(bekomst)],both,[]).

n([pl(bekoringen),sg(bekoring)],de,[vp]).

n([sg(bekostiging),pl(bekostigingen)],de,[]).

n([mass(bekrachtiging)],de,[],[stuur]).

n([sg(bekrompenheid),pl(bekrompenheden)],de,[]).

n([pl(bekroningen),sg(bekroning)],de,[]).

n([pl(bekwaamheden),sg(bekwaamheid)],de,[]).

n([pl(bellen),sg(bel)],de,[],
  [alarm,
   deur,
   fiets,
   slot, % beurs
   dim(belletje)]).

n([pl(belagers),sg(belager)],de,[]).

n([pl(belangen),sg(belang)],het,
  [pred_pp(van),
   pred_pp(van,subject_sbar_no_het),
   pred_pp(van,subject_vp_no_het),
   pred_pp(in),
   pred_pp(in,subject_sbar),
   pred_pp(in,subject_vp),
   subject_sbar],
  []).

n([pl(belangen),sg(belang)],het,
  [],
  [i(aandeel,aandelen),
   s(handel),
   kapitaal,
   s(land),
   s(minderheid),
   s(staat),
   team]).

n([mass(belangenbehartiging)],de,[]).

n([mass(belangrijkheid)],de,[]).

n([mass(belangstelling)],de,
  [vp,
   pred_pp(in)]).

n([pl(belastingen),sg(belasting)],de,[],
  [bron,
   dividend,
   i(inkomst,inkomsten),
   loon,
   luchthaven,
   s(macht),
   milieu,
   i(motorrijtuig,motorrijtuigen),
   i(onroerend_zaak,'onroerende-zaak'),
   f([onroerend,zaak]),
   f([onroerende,zaak]),
   s(overdracht),
   i(toerist,toeristen),
   s(vennootschap),
   s(verbruiker),
   s(vermogen),
   vlieg,
   i(weg,wegen)]).

n([pl(belastingbetalers),sg(belastingbetaler)],de,[]).

n([mass(belastingdruk)],de,[]).

n([pl(beledigingen),sg(belediging)],de,[subject_sbar]).

n([pl(beleefdheden),sg(beleefdheid)],de,[vp]).

n([mass(beleg)],het,[pred_pp(onder)]).

n([pl(belegeringen),sg(belegering)],de,[]).

n([pl(beleggers),sg(belegger)],de,[],
  [super]).

n([pl(beleggingen),sg(belegging)],de,[]).

n([mass(beleid)],het,
  [subject_sbar,
   subject_vp,
   vp,
   sbar],
  [asiel,
   s(begroting),
   cohesie,
   cookie,
   cultuur,i(cultuur,kultuur),
   defensie,
   drug,s(drug),
   emancipatie,
   energie,
   gedoog,
   gender,
   glasnost,
   grond,
   s(handel),
   immigratie,
   industrie,
   s(inkomen),
   integratie,
   s(kabinet),
   klimaat,
   landbouw,
   media,
   mest,
   migratie,
   milieu,
   i(minderheid,minderheden),
   s(natuurschap),
   onderwijs,
   s(ontwikkeling),
   i(oud,ouderen),
   s(overheid),
   parkeer,
   s(personeel),
   prijs,
   s(regering),
   rente,
   rook,
   s(speler),
   s(spreiding),
   structuur,
   taal,
   terugkeer,
   s(toelating),
   uitzet,
   s(uitzetting),
   s(veiligheid),
   s(vervoer),
   visserij,
   s(voorkeur),
   i(vreemdeling,vreemdelingen),
   wan,
   s(welzijn),
   s(werkgelegenheid)]).

n([pl(belemmeringen),sg(belemmering)],de,[sbar,subject_sbar,subject_vp,vp]).

n([sg(belening),pl(beleningen)],de,[]).

%% VL "bij belet: 050 3637812"
n([sg(belet)],both,[]).

n([pl(beletselen),pl(beletsels),sg(beletsel)],het,[subject_vp,vp]).

n([pl(belevenissen),sg(belevenis)],de,[]).

n([pl(belevingen),sg(beleving)],de,[]).

n([sg(belfort),pl(belforten)],het,[]).

n([mass(belichaming),pl(belichamingen),sg(belichaming)],de,[]).

n([pl(belichtingen),sg(belichting)],de,[]).

n([mass(believen)],het,[]).

n([pl(belijdenissen),sg(belijdenis)],de,[sbar,vp]).

n([sg(belijder),pl(belijders)],de,[]).

n([pl(belles),sg(belle)],de,[]).

n([sg(beller),pl(bellers)],de,[]).

n([pl(beloften),pl(beloftes),sg(belofte)],de,
  [sbar,vp],
  [s(verkiezing)]).

n([pl(beloningen),sg(beloning)],de,[],
  [prestatie,
   dim(beloninkje)]).

n([pl(belopen),sg(beloop)],het,[pred_pp(op)]).

n([sg(belt),pl(belten)],de,[]).

n([pl(bemanningen),sg(bemanning)],de,[]).

n([pl(bemestingen),sg(bemesting)],de,[]).

n([pl(bemiddelaars),sg(bemiddelaar)],de,[],
  []).

n([mass(bemiddeling)],de,[],
  [s(arbeid)]).

n([pl(bemoeienissen),sg(bemoeienis)],de,[],[s(overheid)]).

n([pl(bemoeiingen),sg(bemoeiing)],de,[]).

n([mass(bemoeizucht)],de,[]).

n([pl(benaderingen),sg(benadering)],de,[],
  [systeem,h(systeem)]).

n([pl(benamingen),sg(benaming)],de,[sbar,app_measure,np_app_measure]).

n([pl(benauwdheden),sg(benauwdheid)],de,[]).

n([pl(benden),pl(bendes),sg(bende)],de,[measure]).

n([pl(benden),pl(bendes),sg(bende)],de,[],
  [drug,s(drug)]).

n([sg(bengel),pl(bengels)],de,[]).

n([pl(benodigdheden)],de,[],
  [school,
   tuin]).

n([pl(benoemingen),sg(benoeming)],de,[]).

n([mass(benul)],het,[vp,sbar]).

n([mass(benzine)],de,[]).

n([pl(benzinepompen),sg(benzinepomp)],de,[]).

n([pl(benzinetanks),sg(benzinetank)],de,[]).

n([pl(beoefenaars),pl(beoefenaren),sg(beoefenaar)],de,[],
  [s(beroep)]).

n([mass(beoefening)],de,[]).

n([pl(beoordelaars),pl(beoordelaren),sg(beoordelaar)],de,[],
  [krediet]).

n([pl(beoordelingen),sg(beoordeling)],de,
  [sbar]).

n([pl(beoordelingen),sg(beoordeling)],de,[],
  [effect]).

n([mass(bepaaldheid)],de,[]).

n([pl(bepalingen),sg(bepaling)],de,
  [vp,
   sbar],
  [s(uitvoering),
   s(verbod),
   waarde,
   s(wet)]).

n([pl(beperkingen),sg(beperking)],de,[vp,sbar]).

n([pl(beperkingen),sg(beperking)],de,[],
  [geboorte,
   s(snelheid)]).

n([mass(beperktheid),pl(beperktheden),sg(beperktheid)],de,[]).

n([pl(beplantingen),sg(beplanting)],de,[]).

n([pl(beproevingen),sg(beproeving)],de,[]).

n([mass(beraad)],het,[],
  [crisis,
   s(kabinet),
   spoed]).

n([pl(beraadslagingen),sg(beraadslaging)],de,[]).

n([pl(berbers),sg(berber)],de,[]).

n([pl(berden),pl(berderen),sg(berd)],het,[]).

n([pl(berechtingen),sg(berechting)],de,[]).

n([sg(bereider),pl(bereiders)],de,[]).

n([mass(bereidheid)],de,[vp]).

n([pl(bereidingen),sg(bereiding)],de,[]).

n([mass(bereik)],het,[]).

n([mass(bereikbaarheid)],de,[]).

n([pl(berekeningen),sg(berekening)],de,[sbar]).

n([pl(bergen),sg(berg)],de,[measure],[dim(bergje)]).

n([sg(berger),pl(bergers)],de,[]).

n([sg(berging),pl(bergingen)],de,[]).

n([mass(bergland)],het,[]).

n([pl(berichten),sg(bericht)],het,[sbar,vp],
  [s(ambt),
   s(handel),
   s(overlijden),
   push,
   sport,
   dim(berichtje)]).

n([pl(berichtgevingen),sg(berichtgeving)],de,[]).

n([pl(berijders),sg(berijder)],de,[]).

n([pl(berispingen),sg(berisping)],de,[vp]).

n([pl(berken),sg(berk)],de,[]).

n([pl(bermen),sg(berm)],de,[],
  [midden,
   weg]).

n([pl(beroemdheden),sg(beroemdheid)],de,[]).

n([pl(beroepen),sg(beroep)],het,[app_measure,sbar,vp]).

n([sg(beroeps),pl(beroeps)],het,[]).  % VL wielrennen bij de beroeps...;
                                      % VL lesgeven in het beroeps

n([mass(beroepsbevolking)],de,[]).

n([mass(beroepskeuze)],de,[]).

n([pl(beroeringen),sg(beroering)],de,[]).

n([pl(beroerten),pl(beroertes),sg(beroerte)],de,[]).

n([mass(berouw)],het,[]).

n([pl(berovingen),sg(beroving)],de,[]).

n([mass(berusting)],de,[]).

n([pl(bessen),sg(bes)],de,[]).

n([pl(beschadigingen),sg(beschadiging)],de,[],
  [hersen]).

n([pl(beschavingen),sg(beschaving)],de,[]).

n([pl(bescheiden),sg(bescheid)],het,[vp]).

n([mass(bescheidenheid)],de,[]).

n([pl(beschermelingen),sg(beschermeling)],de,[]).

n([pl(beschermers),sg(beschermer)],de,[],
  [i(dier,dieren),
   i(kind,kinder),
   natuur]).

n([pl(beschermsters),sg(beschermster)],de,[],
  [i(dier,dieren),
   i(kind,kinder),
   natuur]).

n([pl(beschermheren),sg(beschermheer)],de,[]).

n([pl(beschermingen),sg(bescherming)],de,[],
  [i(consument,consumenten),
   i(dier,dieren),
   i(gegeven,gegevens),
   milieu,
   natuur]).

n([pl(beschietingen),sg(beschieting)],de,[],[artillerie]).

n([mass(beschikbaarheid)],de,[]).

n([sg(beschikbaarstelling),
   pl(beschikbaarstellingen)],de,[]).

n([pl(beschikkingen),sg(beschikking)],de,[sbar,vp,pred_pp(tot)]).

n([pl(beschilderingen),sg(beschildering)],de,[]).

n([pl(beschouwers),sg(beschouwer)],de,[]).

n([pl(beschouwingen),sg(beschouwing)],de,[sbar]).

n([pl(beschouwingen),sg(beschouwing)],de,[],
  [na,
   voor]).

n([pl(beschrijvers),sg(beschrijver)],de,[]).

n([pl(beschrijvingen),sg(beschrijving)],de,[],
  [functie,
   route]).

n([pl(beschuiten),sg(beschuit)],de,[]).

n([pl(beschuldigingen),sg(beschuldiging)],de,[sbar,subject_sbar]).

n([pl(beschuttingen),sg(beschutting)],de,[]).

n([mass(besef)],het,[sbar],[s(verantwoordelijkheid)]).

n([pl(beslagen),sg(beslag)],het,[vp]).

n([sg(beslaglegging),pl(beslagleggingen)],de,[]).

n([sg(beslisser),pl(beslissers)],de,[]).  % snelle?

n([pl(beslissingen),sg(beslissing)],de,[sbar,vp],
  [mede]).

n([mass(beslistheid)],de,[]).

n([pl(beslommeringen),sg(beslommering)],de,[]).

n([mass(beslotenheid)],de,[]).

n([pl(besluiten),sg(besluit)],het,
  [sbar,
   vp],
  [s(kabinet),
   s(raad)]).

n([pl(besluiten),sg(besluit)],het,
  [],
  [s(aanwijzing),
   kader,
   rente,s(rente)]).

n([mass(besluiteloosheid)],de,[]).

n([mass(besluitvorming)],de,[]).

n([mass(besmetting),pl(besmettingen),sg(besmetting)],de,[],
  [legionella]).

n([sg(besnijdenis),pl(besnijdenissen)],de,[],
  [i(vrouw,vrouwen)]).

n([pl(besparingen),sg(besparing)],de,[],
  [energie]).

n([pl(bespiegelingen),sg(bespiegeling)],de,[]).

n([pl(besprekingen),sg(bespreking)],de,[],
  [boek,
   formatie,
   fusie,
   s(handel),
   s(vrede)]).

n([mass(best)],both,[]).  % hij doet zijn best; ons best

n([sg([best,of])],de,[]).  % hij speelde niet alleen een best of

n([mass(bestaan)],het,[]).

n([pl(bestaanszekerheden),sg(bestaanszekerheid)],de,[]).

n([pl(bestanden),sg(bestand)],het,[],
  [i(lid,leden),
   s(personeel),
   tekst,
   vis]).

n([pl(bestanddelen),sg(bestanddeel)],het,[app_measure]).

n([pl(bestedingen),sg(besteding)],de,[],
  [i(consument,consumenten)]).

n([pl(bestekken),sg(bestek)],het,[]).

n([mass(bestel)],het,[]).

n([sg(besteller),pl(bestellers)],de,[]).

n([sg(bestelster),pl(bestelsters)],de,[]).

n([pl(bestellingen),sg(bestelling)],de,[],[dim(bestellinkje)]).

n([sg(bestemming),pl(bestemmingen)],de,[],
  [eind,
   reis,
   vakantie,
   zon]).

%% bestuur / het bestieren
n([sg(bestier)],het,[]).

n([pl(bestormingen),sg(bestorming)],de,[]).

n([pl(bestraffingen),sg(bestraffing)],de,[]).

n([pl(bestralingen),sg(bestraling)],de,[]).

n([pl(bestratingen),sg(bestrating)],de,[]).

n([pl(bestrijders),sg(bestrijder)],de,[]).

n([mass(bestrijding)],de,[],
  [armoede,
   s(criminaliteit),
   drug,s(drug),
   fraude,
   misdaad,
   pijn,
   i(ramp,rampen),
   symptoom,
   terreur,
   terrorisme]).

n([sg(bestseller),pl(bestsellers)],de,[]).

n([mass(bestudering)],de,[]).

n([sg(bestuiver),pl(bestuivers)],de,[]).

n([sg(bestuiving),pl(bestuivingen)],de,[]).

n([pl(besturingen),sg(besturing)],de,[]).

n([pl(besturen),sg(bestuur)],both,  % VL: de bestuur
  [app_measure],
  [s(afdeling),
   beurs,
   s(bond),
   fractie,
   gemeente,
   hoofd,
   i(interim),
   s(land),
   s(onderneming),
   partij,
   provincie,
   regio,
   school,
   sectie,
   s(stad),
   stadsdeel,
   s(stichting),
   synode,
   s(universiteit),
   s(vakbond),
   wan,
   zelf]).

n([pl(bestuurders),sg(bestuurder),
   sg(bestuurster),pl(bestuursters)],de,[],
  [auto,
   h('FNV'),
   gemeente,
   partij,
   stadsdeel,
   top,
   tram,
   trein,
   s(vakbond)]).

n([mass(bestuursdwang)],de,[]).

n([sg(bestuurskunde)],de,[]).

n([sg(bestuurskundige),pl(bestuurskundigen)],de,[]).

n([sg(bestuursrechter),pl(bestuursrechters)],de,[]).

n([pl(bestuursvormen),sg(bestuursvorm)],de,[]).

n([mass(bestwil)],both,[]).

n([sg(bèta),pl('bèta\'s')],de,[]).

n([sg(betaler),pl(betalers)],de,[]).

n([pl(betalingen),sg(betaling)],de,[],
  [herstel,
   vooruit]).

n([pl(betekenissen),sg(betekenis)],de,
  [pred_pp(van),
   pred_pp(van,subject_sbar),
   pred_pp(van,subject_vp),
   subject_sbar
  ]).

n([mass(beterschap)],de,[]).

n([pl(betimmeringen),sg(betimmering)],de,[]).

n([sg(betiteling),pl(betitelingen)],de,
  [app_measure,
   np_app_measure,
   start_app_measure]).

n([pl(betogers),sg(betoger)],de,[]).

n([pl(betogingen),sg(betoging)],de,[]).

n([mass(beton)],both,[]).

n([pl(betogen),sg(betoog)],het,[sbar,vp]).

n([mass(betoon)],het,[]).

n([pl(betoveringen),sg(betovering)],de,[]).

n([mass(betrekkelijkheid),pl(betrekkelijkheden),sg(betrekkelijkheid)],de,[]).

n([pl(betrekkingen),sg(betrekking)],de,[],
  [dienst,
   s(handel)]).

n([mass(betrokkenheid)],de,[]).

n([mass(betrouwbaarheid)],de,[]).

n([mass(betrouwen)],het,[]).  % voor het Wilhelmus

n([pl(beugels),sg(beugel)],de,[],
  [boks,
   dim(beugeltje)]).

n([pl(beuken),sg(beuk)],de,[]).

n([pl(beulen),sg(beul)],de,[]).

n([sg(beun),pl(beunen)],both,[]).

n([pl(beurzen),sg(beurs)],de,[],
  [i(aandeel,aandelen),
   antiek,
   basis,
   i(boek,boeken),
   i(effect,effecten),
   optie,
   prestatie,
   ruil,
   i(scherm,schermen),
   studie,
   technologie,
   tempo,
   vak,
   voor]).

n([sg(beursgang),pl(beursgangen)],de,[]).

n([sg(beursgraadmeter),pl(beursgraadmeters)],de,[]).

n([pl(beursnoteringen),sg(beursnotering)],de,[]).

n([sg(beurstoezichthouder),pl(beurstoezichthouders)],de,[]).

n([sg(beurswaarde),pl(beurswaardes),pl(beurswaarden)],de,[]).

n([pl(beurten),sg(beurt)],de,[vp]).

n([pl(beurten),sg(beurt)],de,[],
  [inval,
   opfris,
   opknap]).

n([pl(bevallingen),sg(bevalling)],de,[],[thuis]).

n([sg(beveiliger),pl(beveiligers)],de,[]).

n([pl(beveiligingen),sg(beveiliging)],de,[]).

n([pl(bevelen),sg(bevel)],het,[sbar,vp,pred_pp(onder)]).

n([pl(bevelen),sg(bevel)],het,[],
  [s(aanhouding),
   arrestatie,
   s(huiszoeking)]).

n([pl(bevelhebbers),sg(bevelhebber)],de,[]).

n([pl(bevers),sg(bever)],de,[],[dim(bevertje)]).

n([pl(bevestigingen),sg(bevestiging)],de,[sbar,vp]).

n([mass(bevind)],het,[]).  % van zaken

n([pl(bevindingen),sg(bevinding)],de,[sbar]).

n([pl(bevingen),sg(beving)],de,[],
  [dim(bevinkje),
   zee]).

n([pl(bevliegingen),sg(bevlieging)],de,[vp]).

n([pl(bevoegdheden),sg(bevoegdheid)],de,[vp]).

n([pl(bevoegdheden),sg(bevoegdheid)],de,[],
  [s(beslissing),
   s(opsporing),
   rij]).

n([pl(bevolkingen),sg(bevolking)],de,[],[burger]).

n([pl(bevolkingsregisters),sg(bevolkingsregister)],het,[]).

n([mass(bevoorrading)],de,[]).

n([pl(bevorderingen),sg(bevordering)],de,[]).

n([mass(bevrediging)],de,[]).

n([mass(bevreemding)],de,[]).

n([mass(bevriezing),pl(bevriezingen),sg(bevriezing)],de,[]).

n([pl(bevrijders),sg(bevrijder)],de,[]).

n([mass(bevrijding)],de,[]).

n([pl(bevruchtingen),sg(bevruchting)],de,[]).

n([pl(bewaaksters),sg(bewaakster)],de,[]).

n([pl(bewaarders),sg(bewaarder)],de,[],
  [gevangen]).

n([pl(bewakers),sg(bewaker)],de,[]).

n([mass(bewaking)],de,[]).

n([mass(bewapening)],de,[]).

n([mass(bewapeningswedloop)],de,[]).

n([mass(bewaring)],de,[pred_pp(in)]).

n([mass(beweeglijkheid)],de,[]).

n([pl(beweegredenen),sg(beweegreden)],de,[vp]).

n([pl(bewegingen),sg(beweging)],de,[vp,pred_pp(in)],[]).

n([pl(bewegingen),sg(beweging)],de,[],
  [s(afscheiding),
   s(arbeider),
   s(bevrijding),
   emancipatie,
   golf,
   guerrilla,
   hand,
   hoofd,
   inhaal,
   jeugd,
   koers,
   kraak,
   s(lichaam),
   milieu,
   i(rebel,rebellen),
   terreur,
   vak,
   s(verzet),
   vlieg,
   s(volk),
   s(vrede),
   i(vrouw,vrouwen),
   dim(beweginkje)
  ]).

n([pl(beweringen),sg(bewering)],de,
  [sbar,
   vp, % de bewering de goedkoopste te zijn
   start_app_measure]).

n([pl(bewerkingen),sg(bewerking)],de,[]).

n([pl(bewijzen),sg(bewijs)],het,[sbar]).

n([pl(bewijzen),sg(bewijs)],het,[],
  [kenteken,
   legitimatie,
   start,
   s(toegang),
   vaar]).

n([stem(bewijs_last),
   mass(bewijslast)],de,[sbar]).

n([pl(bewijsvoeringen),sg(bewijsvoering)],de,[sbar]).

n([mass(bewind)],het,[pred_pp(aan),pred_pp(onder)]).

n([mass(bewind)],het,[],
  [schrik,
   h('Sovjet'),i('Sovjet',sovjet)]).

n([pl(bewindhebbers),sg(bewindhebber)],de,[]).

n([pl(bewindslieden),sg(bewindsman)],de,[]).

n([pl(bewindvoerders),sg(bewindvoerder)],de,[]).

n([pl(bewindvoersters),sg(bewindvoerster)],de,[]).

n([mass(bewogenheid)],de,[]).

n([mass(bewolking)],de,[]).

n([pl(bewonderaars),sg(bewonderaar)],de,[]).

n([pl(bewonderaarsters),sg(bewonderaarster)],de,[]).

n([mass(bewondering)],de,[]).

n([pl(bewoners),sg(bewoner)],de,[],
  ['Bijlmer',
   buurt,
   s(dorp),
   eiland,
   kamp,
   wijk,
   woestijn,
   woonwagen]).

n([mass(bewoning)],de,[]).

n([pl(bewoonsters),sg(bewoonster)],de,[]).

n([pl(bewoordingen),sg(bewoording)],de,[]).

n([mass(bewusteloosheid)],de,[]).

n([mass(bewustheid)],de,[]).

n([mass(bewustwording)],de,[]).

n([mass(bewustzijn)],het,[pred_pp(buiten),
                          pred_pp(bij),
			  sbar]).

n([pl(bewustzijnstoestanden),sg(bewustzijnstoestand)],de,[]).

n([pl(bezems),sg(bezem)],de,[],[dim(bezempje)]).

n([pl(bezemstelen),sg(bezemsteel)],de,[]).

n([mass(bezetenheid)],de,[]).

n([pl(bezetters),sg(bezetter)],de,[]).

n([pl(bezettingen),sg(bezetting)],de,[],
  [s(personeel),
   rol]).

n([pl(bezettingsgraden),sg(bezettingsgraad)],de,[]).

n([mass(bezieling)],de,[]).

n([pl(bezienswaardigheden),sg(bezienswaardigheid)],de,[]).

n([pl(bezigheden),sg(bezigheid)],de,[]).

n([mass(bezinning)],de,[]).

n([mass(bezit)],het,[pred_pp(in)]).

n([mass(bezit)],het,[],
  [bal,
   grond,
   kunst,
   privé,
   wapen]).

n([pl(bezitters),sg(bezitter)],de,[],
  [auto,
   grootgrond,
   i(huis,huizen)]).

n([pl(bezitsters),sg(bezitster)],de,[],
  [auto,
   grootgrond,
   i(huis,huizen)]).

n([pl(bezittingen),sg(bezitting)],de,[]).

n([pl(bezoeken),sg(bezoek)],het,
  [temp_mod,
   pred_pp(op)],
  [familie,
   huis,
   s(staat),
   werk]).

n([pl(bezoeken),sg(bezoek)],het,
  [temp_mod],
  [dim(bezoekje),
   bliksem,
   school,
   tegen]).

n([pl(bezoekers),sg(bezoeker)],de,[],
  [café]).

n([sg(bezoekersaantal),pl(bezoekersaantallen)],het,[]).

n([pl(bezoeksters),sg(bezoekster)],de,[]).

n([pl(bezoekuren),sg(bezoekuur)],het,[]).

n([pl(bezoldigingen),sg(bezoldiging)],de,[]).

n([mass(bezorgdheid),pl(bezorgdheden)],de,[sbar]).

n([pl(bezorgers),sg(bezorger)],de,[]).

n([pl(bezorgsters),sg(bezorgster)],de,[]).

n([mass(bezorging)],de,[]).

n([pl(bezuinigingen),sg(bezuiniging)],de,[]).

n([pl(bezwaren),sg(bezwaar)],het,[sbar,vp,subject_sbar,subject_vp]).

n([pl(bezwaarschriften),sg(bezwaarschrift)],het,[]).

n([pl(bezweringen),sg(bezwering)],de,[sbar,vp]).

n([pl(beëdigingen),sg(beëdiging)],de,[]).

n([mass(beëindiging)],de,[]).

n([mass(beïnvloeding),pl(beïnvloedingen),sg(beïnvloeding)],de,[]).

n([sg(bh),pl('bh\'s')],de,[]).

n([sg(bibber),pl(bibbers)],de,[]).

n([pl(bibliografieën),sg(bibliografie)],de,[]).

n([pl(bibliothecarissen),sg(bibliothecaris)],de,[]).

n([pl(bibliotheken),sg(bibliotheek)],de,[],
  [i(blind,blinden),
   s(universiteit)]).

n([sg(bid),pl(bids)],het,[]).

n([sg([bid,book]),pl([bid,books])],both,[]).

n([sg(bidon),pl(bidons),pl(bidonnen)],de,[]).

n([pl(biechten),sg(biecht)],de,[]).

n([sg(bieder),pl(bieders)],de,[]).

n([sg(bieding),pl(biedingen)],de,[]).

n([pl(biefstukken),sg(biefstuk)],de,[]).

n([pl(bieren),sg(bier)],het,[],
  [i(trappist,trappisten),
   speciaal,
   wit,
   dim(biertje)]).

n([pl(biezen),sg(bies)],de,[],[dim(biesje)]).

n([mass(bieslook)],both,[]).

n([pl(bieten),sg(biet)],de,[]).

n([pl(biezonderheden),sg(biezonderheid)],de,[sbar]).

n([sg([big,band]),pl([big,bands])],de,[]).

n([sg([big,bang]),pl([big,bangs])],de,[]).

n([mass([big,beat])],de,[]).

n([sg([big,brother])],de,[]).

n([sg([big,shot]),pl([big,shots])],de,[]).

n([sg([big,spender]),pl([big,spenders])],de,[]).

n([pl(biggen),sg(big)],de,[],[dim(biggetje)]).

n([pl(bijen),sg(bij)],de,[],[dim(bijtje)]).

n([pl(bijbedoelingen),sg(bijbedoeling)],de,[]).

n([pl(bijbels),sg(bijbel),sg('Bijbel')],de,[],[dim(bijbeltje)]).

n([pl(bijbelteksten),sg(bijbeltekst)],de,[start_app_measure]).

n([pl(bijdragen),pl(bijdrages),sg(bijdrage)],de,[],
  [ouder,
   s(rijk)
  ]).

n([pl(bijeenkomsten),sg(bijeenkomst)],de,[],
  [s(herdenking),
   partij,
   pers,
   s(voorlichting)]).

n([pl(bijenkorven),sg(bijenkorf)],de,[]).

n([pl(bijgedachten),sg(bijgedachte)],de,[]).

n([mass(bijgeloof)],het,[]).

n([pl(bijkeukens),sg(bijkeuken)],de,[]).

n([pl(bijkomstigheden),sg(bijkomstigheid)],de,[sbar,subject_sbar]).

n([pl(bijlen),sg(bijl)],de,[],
  [strijd,
   dim(bijltje)]).

n([pl(bijlagen),sg(bijlage)],de,[],
  [i(boek,boeken)]).

n([pl(bijnamen),sg(bijnaam)],de,[np_app_measure]).

n([pl(bijnierschorshormonen),sg(bijnierschorshormoon)],het,[]).

n([sg(bijrol),pl(bijrollen)],de,[]).

n([sg(bijschrift),pl(bijschriften)],het,
  [start_app_measure,
   app_measure,
   np_app_measure]).

n([sg(bijsluiter),pl(bijsluiters)],de,[]).

n([mass(bijsmaak)],de,[]).

n([mass(bijstand)],de,[]).

n([pl(bijstellingen),sg(bijstelling)],de,[]).

n([pl(bijsturingen),sg(bijsturing)],de,[]).

n([pl(bijten),sg(bijt)],de,[]).

n([mass(bijval)],de,[]).

n([pl(bijverdiensten),sg(bijverdienste)],de,[]).

n([pl(bijverschijnselen),pl(bijverschijnsels),sg(bijverschijnsel)],het,[sbar]).

n([pl(bijvoegselen),pl(bijvoegsels),sg(bijvoegsel)],het,[]).

n([pl(bijwerkingen),sg(bijwerking)],de,[subject_sbar]).

n([sg(bijzetting),pl(bijzettingen)],de,[]).

n([mass(bijzijn)],het,[]).

n([pl(bijzonderheden),sg(bijzonderheid)],de,
  [subject_sbar,
   sbar]).

n([pl('bikini\'s'),sg(bikini)],de,[],[mini,h(mini)]).

n([pl(billen),sg(bil)],de,[],[dim(billetje)]).

n([pl(biljarten),pl(biljarts),sg(biljart)],both,[]).  % VL: de biljart

n([pl(biljarters),sg(biljarter)],de,[]).

n([pl(biljetten),sg(biljet)],het,[app_measure],
  [aangifte,
   dim(biljetje)]).

n([pl(biljetten),sg(biljet)],het,[],
  [aangifte,
   aanplak,
   bank,
   euro,
   stem,
   dim(biljetje)]).

n([pl(biljoenen),meas(biljoen)],both,[meas_mod,measure]).

n([sg(billboard),pl(billboards)],both,[]).

n([mass(billijkheid)],de,[]).

n([pl(bindingen),sg(binding)],de,[]).

n([mass(bingo)],both,[]).

n([pl(binken),sg(bink)],de,[]).

n([pl(binnenhoven),sg(binnenhof)],both,[]).

n([pl(binnenkanten),sg(binnenkant)],de,[]).

n([mass(binnenkomst)],de,[]).

n([pl(binnenlanden),sg(binnenland)],het,[]).

n([sg(binnenstaat),pl(binnenstaten)],de,[]).

%%n([mass(binnenste)],het,[]).

n([mass(binnenvaart)],de,[]).

n([pl(binnenwateren),pl(binnenwaters),sg(binnenwater)],het,[]).

n([pl(binnenzakken),sg(binnenzak)],de,[]).

n([sg(bintje),pl(bintjes)],both,[]).  % kwekers zeggen: de bintje

n([mass('bio-energetica')],de,[]).

n([pl(biografen),sg(biograaf)],de,[]).

n([pl(biografieën),sg(biografie)],de,[]).

n([mass(biologie)],de,[]).

n([pl(biologen),sg(bioloog)],de,[],
  []).

n([pl(bioscopen),sg(bioscoop),
   pl(bioskopen),sg(bioskoop),
   sg(bios)],de,[],
  [mega,
   dim(bioscoopje),
   dim(bioskoopje)]).

n([pl(biotopen),sg(biotoop)],both,[]).

n([sg(bips),pl(bipsen)],de,[]).

%% ter bis van die verordening
n([sg(bis)],both,[]).

n([pl(bisdommen),sg(bisdom)],het,[],[aarts]).

n([pl(bisschoppen),sg(bisschop)],de,[],[aarts,hulp]).

n([meas(bit),pl(bits)],both,[meas_mod,measure],
  [giga,
   kilo,
   mega]).

n([sg(bitch),pl(bitches)],de,[]).

n([sg(bitcoin),pl(bitcoins)],de,[]).

n([sg(bitter),pl(bitters)],de,[]).

n([pl(bitterheden),sg(bitterheid)],de,[]).

n([sg(bitterzoet)],both,[]).

n([mass([bla,bla])],de,[]).

n([pl(blaadjes),pl(bladertjes),sg(blaadje)],het,[measure]). % sla/peterselie

n([mass(blaam)],de,[]).

n([pl(blaren),sg(blaar)],de,[],[dim(blaartje)]).

n([pl(blazen),sg(blaas)],de,[]).

n([mass(blazoenering)],de,[]).

n([sg(blackberry),pl(blackberries),
   sg(bb)],de,[]).

n([pl([black,outs]),sg([black,out])],de,[]).

n([pl(bladen),sg(blad)],het,[measure],
  []).  % een blad bier

n([pl(bladen),sg(blad)],het,[],
  [bureau,
   dien,
   tafel,
   vloei,
   werk]).  

n([pl(bladen),pl(bladeren),sg(blad)],het,[np_app_measure],
  [boulevard,
   dag,
   maand,
   i(man,mannen),
   nieuws,
   ochtend,
   opinie,
   roddel,
   schouder,
   straf,
   tab,
   vak,
   i(vrouw,vrouwen),
   week,
   i(zaak,zaken),
   s(zondag)]).

n([sg(bladhaantje),pl(bladhaantjes)],het,[]).

n([sg(bladroller),pl(bladrollers)],de,[]).

n([sg(bladzijde),sg(bladzij),pl(bladzijden),pl(bladzijdes)],de,
  [meas_mod,
   measure]).

n([sg(blaf),pl(blaffen)],de,[]).

n([sg(blamage),pl(blamages)],de,[]).

n([mass(blank)],het,[]).

n([mass(blauw)],het,[],
  [diep,
   donker,
   licht]).

n([pl(blauwdrukken),sg(blauwdruk)],de,[]).

n([pl(blauwhelmen),sg(blauwhelm)],de,[]).

n([sg(blazer),pl(blazers)],de,[]).

n([sg(blazoen),pl(blazoenen)],het,[]).

n([mass(bleek)],de,[]).

n([mass(bleekheid)],de,[]).

n([mass(bleekselderij)],de,[]).

n([sg(blend),pl(blends)],de,[]).

n([sg([blended,whiskey]),
   sg([blended,whisky]),
   pl([blended,'whiskey\'s']),
   pl([blended,'whisky\'s'])],de,[]).


n([sg(blender),pl(blenders)],de,[]).

n([sg([blessing,in,disguise])],de,[]).

n([pl(blessuren),pl(blessures),sg(blessure)],de,[],
  [achillespees,
   been,
   dijbeen,
   elleboog,
   enkel,
   hamstring,
   heup,
   knie,
   kuit,
   lies,
   pols,
   rug,
   schouder,
   spier,
   voet]).

n([mass(blessureleed)],het,[]).

n([pl(blieken),sg(bliek)],de,[]).

n([mass(blijdschap)],de,[sbar]).

n([mass(blijheid)],de,[]).

n([pl(blijken),sg(blijk)],het,[]).  % een blijk van ..

n([pl(blijspelen),sg(blijspel)],het,[]).

n([sg(blijver),pl(blijvers)],de,[],
  [thuis, % niet thuis_blij_vers
   dim(blijvertje)]).

n([pl(blikken),sg(blik)],de,[van_sbar]).

n([pl(blikken),sg(blik)],het,[measure],[dim(blikje)]).

n([pl(bliksems),sg(bliksem)],de,[]).

n([pl(bliksemflitsen),sg(bliksemflits)],de,[]).

n([pl(bliksemschichten),sg(bliksemschicht)],de,[]).

n([sg(blikvanger),pl(blikvangers)],de,[]).

n([mass(blikveld)],het,[]).

n([pl(blinddoeken),sg(blinddoek)],de,[]).

n([mass(blindheid)],de,[]).

n([pl(blocnotes),sg(blocnote)],de,[]).

n([mass(bloed)],het,[]).

n([mass(bloedarmoede)],de,[]).

n([mass(bloedbad),pl(bloedbaden),sg(bloedbad)],het,[]).

n([mass(bloeddruk)],de,[]).

n([pl(bloedingen),sg(bloeding)],de,[]).

n([pl(bloedlichaampjes),sg(bloedlichaampje)],het,[]).

n([mass(bloedsomloop)],de,[]).

n([mass(bloedvergieten)],het,[]).

n([pl(bloedverwanten),sg(bloedverwant)],de,[]).

n([pl(bloedvlekken),sg(bloedvlek)],de,[],[dim(bloedvlekje)]).

n([pl(bloedworsten),sg(bloedworst)],de,[]).

n([pl(bloedzuigers),sg(bloedzuiger)],de,[]).

n([mass(bloei)],de,[pred_pp(in)]).

n([pl(bloeiwijzen),sg(bloeiwijze)],de,[]).

n([pl(bloemen),sg(bloem)],de,[],
  [koren,
   pinkster,			% niet pink_ster_bloem
   snij,
   teunis,
   dim(bloemetje),dim(bloempje)]).

n([pl(bloemkolen),sg(bloemkool)],de,[]).

n([pl(bloemlezingen),sg(bloemlezing)],de,[]).

n([pl(bloemperken),sg(bloemperk)],het,[]).

n([pl(bloempotten),sg(bloempot)],de,[]).

n([sg(bloes),pl(bloezen)],de,[],[dim(bloesje)]).

n([pl(bloesems),sg(bloesem)],de,[],[dim(bloesempje)]).

n([pl(bloezes),sg(bloeze)],de,[]).

n([sg(blog),pl(blogs)],both,[],
  [live,
   loo]).

n([sg(blogger),pl(bloggers)],de,[]).

n([sg(blok)],de,[],[]).  % Vlaams "rondje om de blok" 

n([pl(blokken),sg(blok)],het,[measure,meas_mod],
  [dim(blokje)]).

n([pl(blokken),sg(blok)],het,[],
  [basalt,
   beton,
   bouillon,
   i(huis,huizen),
   motor,
   notitie,
   rots,
   schrijf,
   start,
   woon,
   dim(blokje)]).

n([pl(blokkades),sg(blokkade)],de,[],
  [weg]).

n([pl(blokkers),sg(blokker)],de,[]).

n([pl(bloknoots),sg(bloknoot)],de,[]).

n([pl(blommen),sg(blom)],de,[],[dim(blommetje)]).

n([mass(blond)],het,[]).

n([pl(blondines),sg(blondine)],de,[],[dim(blondinetje)]).

n([pl(blondjes),sg(blondje)],het,[]).

n([mass(bloot)],het,[]).

n([mass(blootje)],het,[pred_pp(in)]).

n([mass(blootstelling)],de,[]).

n([sg(blos)],de,[],[dim(blosje)]).

n([pl(blouses),sg(blouse)],de,[]).

n([sg(blow),pl(blows)],de,[]).

n([sg([blow,job]),pl([blow,jobs])],de,[]).

n([mass(blubber)],de,[]).

n([pl([blue,chips])],de,[]).

n([mass(blues)],de,[],
  [f(delta),
   f(talking)]).

n([mass(bluf)],de,[]).

n([pl(blunders),sg(blunder)],de,[subject_sbar,subject_vp]).

n([sg(blusser),pl(blussers)],de,[],[brand]).

n([pl('boa\'s'),sg(boa)],de,[]).

n([sg([boa,constrictor]),pl([boa,constrictors])],de,[]).

n([sg(board),pl(boards)],both,[]).

n([sg(bob),pl(bobs)],de,[]).  % bobslee

n([pl(bobbels),sg(bobbel)],de,[],[dim(bobbeltje)]).

n([pl('bobby\'s'),sg(bobby)],de,[]).

n([sg(bobo),pl('bobo\'s')],de,[]).

n([mass(bobsleeën)],het,[]).

n([pl(bochels),sg(bochel)],de,[],[dim(bocheltje)]).

n([pl(bochten),sg(bocht)],de,[],[dim(bochtje)]).

n([mass(bod)],het,[],
  [eind,
   loon,
   overname,
   zwakte]).

n([pl(boden),pl(bodes),sg(bode)],de,[]).

n([pl('bodega\'s'),sg(bodega)],de,[]).

n([pl(bodems),sg(bodem)],de,[measure],[dim(bodempje)]).

n([pl(bodems),sg(bodem)],de,[],[bekken,  % en niet bek_bodem
				dim(bodempje)]).

n([pl('body\'s'),sg(body)],both,[]).

n([mass(bodybuilding)],both,[]).

n([sg(bodyguard),pl(bodyguards)],de,[]).

n([mass([boe,of,bah]),
   mass([boe,of,ba])],both,[]).

n([pl('boeddha\'s'),sg(boeddha)],de,[]).

n([mass(boeddhisme)],het,[]).

n([pl(boeddhisten),sg(boeddhist)],de,[]).

n([pl(boedels),sg(boedel)],de,[],[dim(boedeltje)]).

n([pl(boeven),sg(boef)],de,[]).

n([pl(boegen),sg(boeg)],de,[]).

n([pl(boeien),sg(boei)],de,[]).

n([pl(boeken),sg(boek)],het,
  [app_measure,
   np_app_measure],
  [adres,
   dag,
   foto,
   i(gast,gasten),
   geschiedenis,
   hand,
   jaar,
   jeugd,
   s(jongen),
   i(kind,kinder),
   kook,
   leer,
   i(lijst_DIM,lijstjes),
   notitie,
   order,
   i(prent,prenten),i(prent,prente),
   programma,
   school,
   strip,
   studie,
   wit,
   zwart,
   dim(boekje)]).

n([pl(boeken),sg(boek)],het,[],
  [aanteken,
   draai,
   groen,
   kas,
   log,
   telefoon,
   wet,
   dim(boekje)]).

n([pl(boeketten),sg(boeket)],both,[measure]).

n([pl(boekhouders),sg(boekhouder)],de,[]).

n([pl(boekhoudsters),sg(boekhoudster)],de,[]).

n([pl(boekhoudingen),sg(boekhouding)],de,[],
  [i(mineraal,mineralen),
   schaduw]).

n([pl(boekingen),sg(boeking)],de,[]).

n([pl(boekrollen),sg(boekrol)],de,[]).

n([mass(boel)],de,[]).

n([pl(boeltjes),sg(boeltje)],het,[measure]).

n([sg(boem),pl(boemen)],de,[]). % VL voor bom?

n([pl(boemannen),sg(boeman)],de,[]).

n([sg(boemel),pl(boemels)],de,[]).

n([pl(boeren),sg(boer)],de,[],
  [coca,
   graan,
   groente,
   i(groente,groenten),
   hobby,
   katoen,
   i(kip,kippen),
   melk,
   s(varken),
   wijn,
   dim(boertje)]).

n([sg(boerde)],de,[]).  % klucht?

n([pl(boerderijen),sg(boerderij)],de,[],[dim(boerderijtje)]).

n([pl(boerinnen),sg(boerin)],de,[],[dim(boerinnetje)]).

n([sg(boeroe),pl(boeroes)],de,[]).

n([sg(boet)],de,[]).  % schuur voor hooi? ouderwets? meervoud?

n([pl(boeten),pl(boetes),sg(boete)],de,[],[geld]).

n([pl(boetedoeningen),sg(boetedoening)],de,[]).

n([sg(boetekleed)],het,[]).

n([pl(boetieks),sg(boetiek),pl(boutiques),sg(boutique)],de,[],[dim(boetiekje),
							       dim(boutiqueje)]).

n([mass([boeuf,bourguignon])],de,[]).

n([pl(boezems),sg(boezem)],de,[],[dim(boezempje)]).

n([mass(bof)],de,[sbar,subject_sbar]).

n([sg(bogey),pl(bogeys)],de,[]).

n([sg(boiler),pl(boilers)],de,[]).

n([pl(bokken),sg(bok)],de,[],[dim(bokje)]).

n([pl(bokalen),sg(bokaal)],de,[],[dim(bokaaltje)]).

n([sg(boks),pl(boksen)],de,[]). 

n([pl(boksers),sg(bokser)],de,[],[prof]).

n([pl(bollen),sg(bol)],de,[measure],[dim(bolletje)]). % cocaine

n([pl(bolhoeden),sg(bolhoed)],de,[]).

n([pl(boliden),pl(bolides),sg(bolide)],de,[],
  [wh(['Formule','1'])]).

n([sg(bolletjesslikker),pl(bolletjesslikkers)],de,[]).

n([pl(bolsjevisten),sg(bolsjevist)],de,[]).

n([pl(bolsjewieken),sg(bolsjewiek)],de,[]).

n([sg(bolster),pl(bolsters)],de,[]). % ruwe

n([pl(bolwerken),sg(bolwerk)],het,[]).

n([pl(bommen),sg(bom)],de,[measure]). % duiten

n([pl(bommen),sg(bom)],de,[],
  [atoom,
   auto,
   brand,
   cluster,
   kern,
   tijd,
   dim(bommetje)]).

n([pl(bombardementen),sg(bombardement)],het,[],
  [h('NAVO'),
   h('Navo'),
   'NAVO',
   'Navo']).

n([pl(bommenwerpers),sg(bommenwerper)],de,[]).

n([pl(bonnen),pl(bons),sg(bon)],de,[],
  [i(boek,boeken),
   i(plaat,platen),
   dim(bonnetje)]).

n([pl(bonnen),pl(bons),sg(bon)],de,[pred_pp(op)],
  []).

%% voor: het is zo simpel als bonjour
n([mass(bonjour)],both,[]).

n([sg([bon,mot]),pl([bon,mots])],de,[]).

n([sg([bon,vivant]),pl([bon,vivants])],de,[]).

n([pl(bonbons),sg(bonbon)],de,[]).

n([pl(bonden),sg(bond)],de,[],
  [i(ambtenaar,ambtenaren),
   atletiek,
   basketbal,
   biljart,
   i(boer,boeren),
   bouw,
   bridge,
   h('CNV'),
   dam,
   i(dienst,diensten),
   h('FNV'),
   handbal,
   hockey,
   honkbal,
   horeca,
   ijshockey,
   industrie,
   judo,
   karate,
   onder,
   onderwijs,
   i(oud,ouderen),
   politie,
   roei,
   schaak,
   schaats,
   scherm,
   ski,
   s(speler),
   sport,
   tafeltennis,
   tennis,
   turn,
   s(voeding),
   voetbal,
   volleybal,
   i(wereld_voetbal,wereldvoetbal),
   wieler,
   zwem]).

n([sg(bondgenoot),pl(bondgenoten)],de,[]).

n([pl(bondgenootschappen),sg(bondgenootschap)],het,[]).

n([pl(bondscoaches),sg(bondscoach)],de,[]).

n([pl(bondskanseliers),sg(bondskanselier)],de,[],
  [h(ex),
   h(oud)]).

n([pl(bondsrepublieken),sg(bondsrepubliek)],de,[]).

n([pl(bonken),sg(bonk)],de,[measure]).

n([pl(bonzen),sg(bons)],de,[]).

n([pl(bonten),sg(bont)],het,[]).

n([pl(bontjassen),sg(bontjas)],de,[]).

n([pl(bontmantels),sg(bontmantel)],de,[]).

n([pl(bonussen),sg(bonus)],de,[]).

n([sg([bonus,track]),pl([bonus,tracks])],de,[]).

n([pl(boodschappen),sg(boodschap)],de,
  [sbar,
   vp,
   subject_sbar],
  [kerst,
   televisie,
   video]).

n([pl(boodschappentassen),sg(boodschappentas)],de,[]).

n([pl(boodschappers),sg(boodschapper)],de,[]).

n([pl(bogen),sg(boog)],de,[],
  [rond,
   s(spanning),
   dim(boogje)]).

n([mass([boogie,woogie])],both,[]).

n([sg(bookmaker),pl(bookmakers)],de,[]).

n([pl(bomen),sg(boom)],de,[],
  [schiet,
   spoor,
   dim(boompje)]).

n([pl(boomgaarden),sg(boomgaard)],de,[]).

n([pl(boomkruinen),sg(boomkruin)],de,[]).

n([pl(boomstammen),sg(boomstam)],de,[]).

n([pl(boomstronken),sg(boomstronk)],de,[]).

n([pl(boomtakken),sg(boomtak)],de,[]).

n([pl(bonen),sg(boon)],de,[],
  [sperzie,
   tuin,
   dim(boontje)]).

n([pl(boren),sg(boor)],de,[]).

n([pl(boorden),sg(boord)],both,[]).

n([pl(booreilanden),sg(booreiland)],het,[]).

n([pl(boosaardigheden),sg(boosaardigheid)],de,[]).

n([pl(boosdoeners),sg(boosdoener)],de,[]).

n([pl(boosheden),sg(boosheid)],de,[]).

n([sg(boost)],de,[]).

n([sg(booswicht),pl(booswichten)],de,[]).

n([pl(boten),sg(boot)],de,[np_app_measure],
  [s(huwelijk),
   kanonneer,
   mail,
   redding,
   s(redding),
   speed,
   woon,
   h('X'),
   dim(bootje)]).

n([pl(bootslieden),pl(bootslui),sg(bootsman)],de,[]).

n([pl(boots)],de,[]). % schoenen?

n([sg(borough),pl(boroughs)],de,[]).   % plaats in USA

%% er hing een bord dat roken verboden is
n([pl(borden),sg(bord)],het,[sbar],
  [s(verkeer),
   s(waarschuwing),
   dim(bordje)
  ]).

%% een bord rijst
n([pl(borden),sg(bord)],het,[measure],[dim(bordje)]).

n([pl(borden),sg(bord)],het,[],
  [klem,
   nummer,
   prik,
   reclame,
   score,
   school,
   dim(bordje)]).

n([pl(bordeauxs),sg(bordeaux)],de,[]).

n([pl(bordelen),sg(bordeel)],het,[]).

n([sg([border,collie]),pl([border,collies])],de,[]).

n([pl(borders),sg(border)],de,[]).

n([pl(bordessen),sg(bordes)],het,[]).

n([sg(borduur),pl(borduren)],de,[]).

n([pl(borgen),sg(borg)],de,[]).

n([pl(borgsommen),sg(borgsom)],de,[]).

n([mass(borgtocht)],de,[]).

n([pl(boringen),sg(boring)],de,[],[gas]).

n([pl(borrels),sg(borrel)],de,[],
  [kerst,
   s(nieuwjaar),
   dim(borreltje)]).

n([pl(borsten),sg(borst)],de,[],
  [linker,
   rechter,
   dim(borstje)]).

n([pl(borstels),sg(borstel)],de,[],[dim(borsteltje)]).

n([pl(borstkassen),sg(borstkas)],de,[]).

n([pl(borstzakken),sg(borstzak)],de,[],[dim(borstzakje)]).

n([mass([bossa,nova])],de,[]).

n([pl(bossen),sg(bos)],de,[measure],[dim(bosje)]).

n([pl(bossen),sg(bos)],de,[],
  [sleutel,
   dim(bosje)]).

n([pl(bossen),sg(bos)],het,[],
  [bamboe,
   i(berk,berken),
   i(beuk,beuken),
   braam,
   i(den,dennen),
   i(eik,eiken),
   loof,
   mangrove,
   moeras,
   naald,
   natuur,
   oer,
   i(populier,populieren),
   productie,
   i(spar,sparren),
   i(tak,takken),
   dim(bosje)]).

n([pl(bosbessen),sg(bosbes)],de,[]).

n([pl(bosbranden),sg(bosbrand)],de,[]).

n([pl(bosranden),sg(bosrand)],de,[]).

n([pl(boswachters),sg(boswachter)],de,[]).

n([pl(bots),pl(botten),sg(bot)],de,[]).

n([pl(botten),sg(bot)],het,[],[dim(botje)]).

n([sg(botanicus),pl(botanici)],de,[]).

n([pl(botters),sg(botter)],de,[]).

n([pl(bottels),sg(bottel)],de,[]).

n([mass(boter)],de,[],
  [bak,
   cacao,
   room]).

n([pl(boterhammen),sg(boterham)],de,[],[dim(boterhammetje)]).

n([pl(botertjes),sg(botertje)],het,[]).

n([pl(botsingen),sg(botsing)],de,[],
  [ketting]).

n([sg([bottom,line]),
   sg('bottom-line')],
  de,[subject_sbar]).

n([pl(boudoirs),sg(boudoir)],het,[]).

n([pl(bouillons),sg(bouillon)],de,[]).

n([pl(boulevards),sg(boulevard)],de,[]).

n([pl(bouquetten),sg(bouquet)],het,[measure]).

n([sg([bouquet,garni])],het,[]).

n([pl(bourbons),sg(bourbon)],de,[]).

n([pl(bourgeois),sg(bourgeois)],de,[]).

n([mass(bourgeoisie)],de,[]).

n([pl(bourgognes),sg(bourgogne)],de,[]).

n([pl(bouten),sg(bout)],de,[]).

n([pl(bouviers),sg(bouvier)],de,[]).

n([mass(bouw)],de,[],
  [bos,
   i(stad,steden),
   vliegtuig]).

n([pl(bouwers),sg(bouwer)],de,[],
  [akker,
   auto,
   i(schip,scheeps)]).

n([sg(bouwkundige),pl(bouwkundigen)],de,[],
  [i(stad,steden)]).

n([pl(bouwlanden),sg(bouwland)],het,[]).

n([pl(bouwmeesters),sg(bouwmeester)],de,[]).

n([pl(bouwpakketten),sg(bouwpakket)],het,[]).

n([pl(bouwsels),sg(bouwsel)],het,[]).

n([pl(bouwstenen),sg(bouwsteen)],de,[]).

n([mass(bouwvak)],both,[]).

n([pl(bouwvakkers),sg(bouwvakker)],de,[]).

n([pl(bouwvallen),sg(bouwval)],both,[]).

n([mass(bovenbouw)],de,[]).

n([pl(bovenkanten),sg(bovenkant)],de,[],[linker,rechter]).

n([pl(bovenlagen),sg(bovenlaag)],de,[]).

n([pl(bovenleidingen),sg(bovenleiding)],de,[]).

n([pl(bovenlichamen),sg(bovenlichaam)],het,[]).

n([pl(bovenlippen),sg(bovenlip)],de,[]).

n([pl(boventonen),sg(boventoon)],de,[]).

n([sg(bowl),pl(bowls)],de,[]).

n([pl(boxen),sg(box)],de,[],[mail]).

n([pl(boys),sg(boy)],de,[]).

n([pl(boycots),sg(boycot)],de,[],
  [i(consument,consumenten),
   olie]).

% n([mass(boze)],het,[]).

n([pl(braadpannen),sg(braadpan)],de,[]).

n([sg(braak)],de,[]).

n([sg(braaklegging),pl(braakleggingen)],de,[]).

n([mass(braaksel)],het,[]).

n([sg(braille)],het,[]).

n([sg(brainstorm)],de,[]).

n([pl(bramen),sg(braam)],de,[]).

n([pl(brammen),sg(bram)],de,[]).

n([pl(brancards),sg(brancard)],de,[]).

n([pl(branches),sg(branche)],de,[],
  [auto,
   kleding,
   nieuws,
   reis,
   taxi,
   uitzend]).

n([pl(branden),sg(brand)],de,[pred_pp(in)],[]).

n([pl(branden),sg(brand)],de,[],
  [binnen,
   café,
   dim(brandje)]).

n([sg(brandbrief),pl(brandbrieven)],de,[]).

n([pl(branders),sg(brander)],de,[]).

n([mass(brandewijn)],de,[]).

n([pl(brandhaarden),sg(brandhaard)],de,[]).

n([pl(brandhouten),sg(brandhout)],het,[]).

n([mass(branding)],de,[]).

n([pl(brandkasten),sg(brandkast)],de,[]).

n([sg(brandmelder),pl(brandmelders)],de,[]).

%% het brandmerk pedofiel
n([sg(brandmerk),pl(brandmerken)],het,[],[app_measure]).

n([pl(brandnetels),sg(brandnetel)],de,[]).

n([pl(brandpunten),sg(brandpunt)],het,[]).

n([pl(brandstapels),sg(brandstapel)],de,[]).

n([pl(brandstichters),sg(brandstichter)],de,[]).

n([pl(brandstichtingen),sg(brandstichting)],de,[]).

n([pl(brandstoffen),sg(brandstof)],de,
  [app_measure],
  [auto,
   bio]).

n([pl(brandstoftanks),sg(brandstoftank)],de,[]).

n([mass(brandweer)],de,[]).

n([pl(brandweerlieden),pl(brandweermannen),sg(brandweerman)],de,[]).

n([mass(brandy)],de,[]).

n([pl(brasems),sg(brasem)],de,[],[dim(brasempje)]).

n([sg(brasserie),pl(brasseries)],de,[],[]).

n([mass(bravoure)],de,[]).

%% twee breaks voorsprong
n([pl(breaks),sg(break),
   pl(breeks),sg(breek)],de,[measure,meas_mod]).

n([sg(breedbeeld)],both,[]).

n([pl(breedten),pl(breedtes),sg(breedte)],de,[]).

n([pl(breekijzers),sg(breekijzer)],het,[]).

n([mass(brei)],both,[]).

n([mass(brein),pl(breinen),sg(brein)],het,[]).

n([pl(breinaalden),sg(breinaald)],de,[]).

n([pl(brekers),sg(breker)],de,[],[ijs]).

n([pl(bressen),sg(bres)],de,[]).

n([pl(bretellen),pl(bretels),sg(bretel)],de,[]).

n([pl(breuken),sg(breuk)],de,[],
  [been,
   bot,
   contract,
   enkel,
   s(vertrouwen),
   trend
  ]).

n([pl(brevetten),sg(brevet)],het,[]).

n([sg(brexit)],de,[]).

n([mass(bridge)],het,[]).

n([sg(bridger),pl(bridgers)],de,[]).

n([mass(brie)],de,[],[room]).

n([pl(brieven),sg(brief)],de,
  [measure,sbar],
  [dreig,
   dim(briefje)]).

n([pl(brieven),sg(brief)],de,[],
  [werk,
   dim(briefje)]).

n([pl(brieven),sg(brief)],de,[],
  [s(beleid),
   bom,
   s(geloof),
   ketting,
   kogel,
   s(lezer),
   omzend,
   ontslag,
   rondzend]).

n([pl(briefings),sg(briefing)],de,[]).

n([mass(briefpapier)],het,[]).

n([pl(briefschrijvers),sg(briefschrijver)],de,[]).

n([stem(brief_wisseling),
   pl(briefwisselingen),sg(briefwisseling)],de,[]).

n([sg(bries)],de,[],[dim(briesje)]).

n([pl(brievenbussen),sg(brievenbus)],de,[]).

n([pl(brigaden),pl(brigades),sg(brigade)],de,[measure]).

n([pl(brigadiers),sg(brigadier)],de,[]).

n([sg([bright,spot]),pl([bright,spots])],de,[]).

n([pl(brijen),sg(brij)],de,[]).

n([pl(brillen),sg(bril)],de,[],
  [eclips,			% alleen in augustus 1999
   lees,
   ski,
   wh([virtual,reality]),
   dim(brilletje)]).

n([sg(briljant),pl(briljanten)],both,[]).

n([pl(brilleglazen),sg(brilleglas)],het,[]).

n([pl(brinken),sg(brink)],de,[]).

n([pl('Britsen'),pl(britsen),sg('Brits'),sg(brits)],de,[]).

n([sg(brocant),sg(brocante)],de,[]).

n([mass(broccoli)],de,[]).

n([pl(broches),sg(broche)],de,[]).

n([pl(brochures),sg(brochure)],de,[],
  [jaar,
   maand,
   s(najaar),
   s(seizoen),
   vakantie,
   s(voorjaar),
   week,
   winter,
   zomer]).

n([sg(broed)],het,[]).

n([pl(broederen),pl(broeders),sg(broeder)],de,[],
  [schoon,
   dim(broedertje)]).

n([pl(broederschappen),sg(broederschap)],de,[]).

n([mass(broederschap)],het,[]).

n([pl(broedsels),sg(broedsel)],het,[]).

n([mass(broei)],de,[]).

n([pl(broeikassen),sg(broeikas)],de,[]).

%% type landschap
n([sg(broek),pl(broeken)],het,[],[]).

n([pl(broeken),sg(broek)],de,[],
  [jogging,
   knie,
   yoga,
   dim(broekje)]).

n([pl(broekriemen),sg(broekriem)],de,[]).

n([pl(broekspijpen),sg(broekspijp)],de,[]).

n([pl(broekzakken),sg(broekzak)],de,[]).

n([pl(broers),sg(broer)],de,[],
  [half,
   schoon,
   dim(broertje)]).

n([pl(brokken),sg(brok)],both,[measure],[dim(brokje)]).

n([pl(brokstukken),sg(brokstuk)],het,[]).

n([mass(brol)],de,[]).

n([sg(brom),pl(brommen)],de,[]).

n([pl(bromfietsen),sg(bromfiets)],de,[]).

n([sg(bromfietser),pl(bromfietsers)],de,[]).

n([pl(brommers),sg(brommer)],de,[]).

n([pl(bronnen),sg(bron)],de,[],
  [energie,
   i(inkomst,inkomsten),
   kracht,
   olie,
   voedsel
  ]).

n([mass(bronchitis)],de,[]).

n([pl(bronzen),sg(brons)],het,[]).

n([mass(bronst)],de,[]).

n([pl(broden),sg(brood)],het,[],
  [bruin,
   rogge,
   stok,
   wit]).

n([stem(brood_DIM),
   pl(broodjes),sg(broodje)],het,[measure]).

n([pl(broodwinningen),sg(broodwinning)],de,[]).

n([pl(brouwers),sg(brouwer)],de,[],[bier]).

n([pl(brouwerijen),sg(brouwerij)],de,[],[bier]).

n([pl(brouwsels),sg(brouwsel)],het,[]).

n([sg(browser),pl(browsers)],de,[]).

n([pl(bruggen),sg(brug)],de,[],
  [fiets,
   dim(bruggetje),
   dim(brugje)]).

n([sg([brug,ongelijk])],de,[]).

n([pl(brugklassen),sg(brugklas)],de,[]).

n([mass(brui)],de,[]).

n([pl(bruiden),sg(bruid)],de,[]).

n([pl(bruidegoms),sg(bruidegom)],de,[]).

n([pl(bruidsparen),sg(bruidspaar)],het,[]).

n([pl(bruidsschatten),sg(bruidsschat)],de,[]).

n([mass(bruikbaarheid)],de,[]).

n([pl(bruiklenen),sg(bruikleen)],both,[]).

n([pl(bruiloften),sg(bruiloft)],de,[]).

n([mass(bruin)],het,[],
  [donker,
   licht,
   rood]).

n([pl(bruinvissen),sg(bruinvis)],de,[]).

n([mass(bruis)],both,[]).

n([sg(brul),pl(brullen)],de,[]).

n([pl(brutaliteiten),sg(brutaliteit)],de,[vp]).

n([pl(bruten),sg(bruut)],de,[]).

n([sg(bubbel),pl(bubbels)],de,[],[internet]).

n([pl(budgets),pl(budgetten),sg(budget)],het,[],
  [zorg]).

n([pl(buffels),sg(buffel)],de,[],[dim(buffeltje)]).

n([pl(buffers),sg(buffer)],de,[]).

n([pl(buffetten),sg(buffet)],het,[]).

n([sg(bug),pl(bugs)],de,[],
  [millennium,
   h(millennium),
   f([millennium])]).

n([pl(buien),sg(bui)],de,[measure],
  [dim(buitje)]).

n([pl(buien),sg(bui)],de,[],
  [drift,
   hagel,
   hoest,
   hoos,
   huil,
   lach,
   s(onweer),
   regen,
   sneeuw,
   dim(buitje)]).

n([pl(buidels),sg(buidel)],de,[measure],[dim(buideltje)]).

n([pl(buigingen),sg(buiging)],de,[],
  [knie,
   dim(buiginkje)]).

n([pl(buiken),sg(buik)],de,[],[dim(buikje)]).

n([pl(buikwanden),sg(buikwand)],de,[]).

n([pl(builen),sg(buil)],de,[measure]). % buil shag

n([pl(buizen),sg(buis)],de,[],
  [urine,
   dim(buisje)]).

n([mass(buit)],de,[]).

n([sg(buiten)],both,[]).  % VL: de buiten
                          % NL: het buiten 

n([pl(buitenbeentjes),sg(buitenbeentje)],het,[]).

n([pl(buitenhuizen),sg(buitenhuis)],het,[]).

n([pl(buitenkansen),sg(buitenkans)],de,[sbar],[dim(buitenkansje)]).

n([pl(buitenkanten),sg(buitenkant)],de,[]).

n([mass(buitenland)],het,[]).

n([pl(buitenlanders),sg(buitenlander)],de,[]).

n([mass(buitenlucht)],de,[]).

n([mass(buitenspel)],het,[]).

n([pl(buitenstaanders),sg(buitenstaander)],de,[]).

%%n([pl(buitensten),sg(buitenste)],het,[measure]).

n([pl(buitenverblijven),sg(buitenverblijf)],het,[]).

n([mass(buitenwacht)],de,[]).

n([sg(buizerd),pl(buizerds)],de,[]).

n([sg(buks),pl(buksen)],de,[],[lucht]).

n([pl(bullen),sg(bul)],de,[]).

n([sg(bulk)],de,[measure]).

n([pl(bulldozers),sg(bulldozer)],de,[]).

n([pl(bulletins),sg(bulletin)],het,[]).

n([sg([bulletin,board]),pl([bulletin,boards])],de,[]).

n([mass(bullshit)],both,[]).

n([pl(bulten),sg(bult)],de,[]).

n([pl(bumpers),sg(bumper)],de,[]).

n([pl(bundels),sg(bundel)],de,
  [measure],
  [dim(bundeltje)]).

n([pl(bundels),sg(bundel)],de,
  [np_app_measure,
   start_app_measure],
  [dicht,
   essay,
   feest,
   i(verhaal,verhalen),
   dim(bundeltje)]).

n([sg(bundeling),pl(bundelingen)],de,[app_measure],[]).

n([sg(bundeling),pl(bundelingen)],de,[],
  [i(kracht,krachten)]).

n([sg([bundeltak,blok])],het,[]). % linker of rechter; hartafwijking

n([pl(bungalows),sg(bungalow)],de,[]).

n([pl(bunkers),sg(bunker)],de,[]).

n([pl(burchten),sg(burcht)],de,[]).

n([pl(bureaus),pl('bureau\'s'),sg(bureau),sg(buro),pl('buro\'s')],both,
  [np_app_measure],
  [s(accountant),
   advies,
   s(arbeid),
   i(architect,architecten),
   s(bond),
   consultancy,
   consultatie,
   i(consultatie,konsultatie),
   controle,
   hoofd,
   incasso,
   s(ingenieur),
   s(onderzoek),
   organisatie,
   partij,
   pers,
   plan,
   politie,
   project,
   rating,
   reclame,i(reclame,reklame),
   redactie,
   reis,
   schrijf,
   staf,
   statistiek,
   stem,
   studie,
   uitzend,
   dim(bureautje),dim(burootje)]).

n([pl(bureaucraten),sg(bureaucraat)],de,[]).

n([mass(bureaucratie)],de,[]).

n([pl(bureaukraten),sg(bureaukraat)],de,[]).

n([mass(bureaukratie)],de,[]).

n([pl(burelen),sg(bureel)],het,[]).

n([sg(burg),pl(burgen)],de,[]).

n([pl(burgemeesters),sg(burgemeester)],de,[],
  [h(loco),loco,
   h(oud),oud]).

n([mass(burgemeesterschap)],het,[]).

n([pl(burgers),sg(burger)],de,[],
  [s(staat),
   wereld]).

n([mass(burgerij)],de,[]).

n([pl(burgerlieden),pl(burgerlui),sg(burgerman)],de,[]).

n([mass(burgerschap)],het,[],
  [ere,
   s(staat)]).

n([sg(burleske),pl(burlesken)],de,[]).

n([sg([burn,out]),pl([burn,outs]),
   sg('burn-out'),pl('burn-outs')],de,[]).

n([pl(bussen),sg(bus)],de,[measure],
  [auto,
   bestel,
   s(dubbeldek),
   s(stad),
   streek,
   trolley,
   dim(busje)]).

n([pl(bushalten),pl(bushaltes),sg(bushalte)],de,[]).

n([mass(business)],de,[]).

n([sg([business,case]),pl([business,cases])],de,[]).

n([sg([business,club]),
   pl([business,clubs])],de,[]).

n([sg([business,development,manager]),
   pl([business,development,managers])],de,[]).

n([sg([business,model]),pl([business,models])],het,[]).

n([sg([business,plan])],het,[]).

n([sg([business,unit]),pl([business,units])],de,[app_measure]).

n([sg(bussel),pl(bussels)],both,[measure]).

n([pl(busten),pl(bustes),sg(buste)],de,[]).

n([pl(butlers),sg(butler)],de,[]).

n([sg(button),pl(buttons)],de,[]).

n([pl(buren),sg(buur)],de,[],
  [ooster,
   noorder,
   wester,
   zuider]).

n([pl(buurten),sg(buurt)],de,[],
  [achter,
   s(achterstand),
   s(volk),
   dim(buurtje)]).

n([pl(buurtschappen),sg(buurtschap)],both,[]).

n([pl('buy-outs'),sg('buy-out')],de,[]).

n([sg(bv),pl('bv\'s'),sg('b.v.'),pl('b.v.\'s'),
   sg('BV'),pl('BV\'s'),sg('B.V.'),pl('B.V.\'s')],de,[]).

n([sg(bye)],de,[]).

n([meas(byte),pl(bytes)],de,[meas_mod,measure],
  [giga,
   kilo,
   mega]).

n([pl(bètablokkers),sg(bètablokker)],de,[]).

n([sg(bühne)],de,[]).

n([pl('c\'s'),sg(c)],de,[],[dim('c\'tje')]).

n([pl(cabarets),sg(cabaret)],het,[]).

n([pl(cabaretiers),sg(cabaretier)],de,[]).

n([sg(cabernet),pl(cabernet)],de,[]).

n([mass([cabernet,blanc])],de,[]).

n([mass([cabernet,franc])],de,[]).

n([mass([cabernet,sauvignon]),
   mass([cabernet,sau,vignon])],de,[]).

n([pl(cabines),sg(cabine)],de,[]).

n([sg(cabrio),pl('cabrio\'s')],de,[]).

n([sg(cabriolet),pl(cabriolets)],de,[]).

n([mass(cacao)],de,[]).

n([sg(cache)],de,[]).

n([pl(cactussen),sg(cactus)],de,[],[dim(cactusje)]).

n([pl(cadansen),sg(cadans),pl(kadansen),sg(kadans)],de,[]).

n([pl(cadeaus),sg(cadeau)],het,[],[dim(cadeautje)]).

n([pl(cadetten),sg(cadet)],de,[]).

n([pl(caesars),sg(caesar)],de,[]).

n([pl('cafetaria\'s'),sg(cafetaria)],both,[]).

n([pl(cafés),pl('café\'s'),sg(café)],het,[np_app_measure],
  [eet,
   euro,
   internet,
   i(jong,jongeren),
   muziek,
   dim(cafeetje),
   dim(cafeeke)]).

n([sg([café,au,lait])],de,[]).

n([pl(cahiers),sg(cahier)],het,[],[dim(cahiertje)]).

n([pl(cakes),sg(cake)],de,[],
  [space]).

n([sg(calamiteit),pl(calamiteiten)],de,[]).

n([mass(calcium)],het,[]).

n([sg(calculator),pl(calculators),pl(calculatoren)],de,[]).

n([sg(call),pl(calls)],de,[]).

n([sg([call,centre]),pl([call,centres]),
   sg([call,center]),pl([call,centers]),
   sg(callcenter),   pl(callcenters)],both,[]).

n([sg([call,girl]),pl([call,girls])],de,[]).

n([sg([call,screener]),pl([call,screeners])],de,[]).

n([pl(calorieën),sg(calorie)],de,[]).

n([mass(calvados)],de,[]).

n([mass(calvinisme)],het,[]).

n([pl(calvinisten),sg(calvinist)],de,[]).

n([mass(camembert)],de,[]).

n([pl('camera\'s'),sg(camera)],de,[],
  [s(bewaking),
   f([high,speed]),
   f(['high-speed']),
   televisie,tv,f([tv]),h(tv),i(tv,'TV-'),
   video]).

n([sg([camera,obscura])],de,[]).

n([pl(cameralieden),pl(cameramannen),sg(cameraman)],de,[]).

n([sg(camionette),pl(camionettes)],de,[]).

n([pl(camouflages),sg(camouflage)],de,[]).

n([mass(camp)],both,[]).

n([pl(campagnes),sg(campagne)],de,
  [start_app_measure,
   app_measure],
  [i(biet,bieten),
   reclame,
   s(werving)
  ]).

n([sg(camper),pl(campers)],de,[]).

n([pl(campings),sg(camping)],de,[np_app_measure]).

n([pl(campussen),sg(campus)],de,[]).

n([pl(canapés),sg(canapé)],de,[],[dim(canapeetje)]).

n([pl(candidaten),sg(candidaat)],de,[]).

n([pl(candidaturen),sg(candidatuur)],de,[]).

n([mass(cannabis)],both,[]).

n([pl(canons),sg(canon)],de,[sbar]).

n([sg(cantate),pl(cantates)],de,[]).

n([pl(cantines),sg(cantine)],de,[]).

n([mass(canvas)],both,[]).

n([sg('CAO'),sg(cao),pl('cao\'s'),
   pl('CAO\'s')],de,[app_measure]).

n([sg(cap),pl(caps)],de,[]).  % VL: aantal interlands?

n([pl(capaciteiten),sg(capaciteit)],de,[vp]).

n([pl(capaciteiten),sg(capaciteit)],de,[],
  [s(betaling),
   import,
   productie,i(productie,produktie)]).

n([pl(capes),sg(cape)],de,[]).

n([sg('Capellenaar'),pl('Capellenaren')],de,[]).

n([sg(capita),pl(capita)],both,[]).

n([pl([capita,selecta])],both,[]).

n([pl(capitulaties),sg(capitulatie)],de,[]).

n([pl(capriolen),sg(capriool)],de,[]).

n([pl(capsules),sg(capsule)],de,[measure],[dim(capsuletje)]).

n([pl(captains),sg(captain)],de,[]).

n([pl([captains,of,industry]),sg([captain,of,industry])],de,[]).

n([pl(capuchons),sg(capuchon)],de,[]).

n([pl(caravans),sg(caravan)],de,[],[sta]).

n([mass(carbamazepine)],both,[]).

n([mass([carbon,fibre]),mass('carbon-fibre'),
   mass([carbon,fiber]),mass('carbon-fiber')],het,[]).

n([sg(carcinoom),pl(carcinomen)],both,[]).

n([sg(cardioloog),pl(cardiologen)],de,[]).

n([mass(care)],de,[]).

n([pl(caricaturen),sg(caricatuur)],de,[]).

n([pl(carillons),sg(carillon)],both,[]).

n([mass(cariës)],de,[]).

n([pl(carnavals),sg(carnaval)],both,[]). % also: tijdens de carnaval

n([sg(carnivoor),pl(carnivoren)],de,[]).

n([sg([carpaal,tunnel,syndroom]),
   sg(carpaaltunnelsyndroom),
   sg([carpaaltunnel,syndroom])],het,[]).

n([mass([carrier,select])],both,[]).

n([pl(carrières),sg(carrière)],de,[],
  [interland,
   solo,
   s(trainer),
   voetbal
  ]).

n([pl(carrosserieën),sg(carrosserie)],de,[]).

n([sg(carrousel),pl(carrousellen)],de,[]).

n([pl(carrés),sg(carré)],het,[]).

n([pl(cartels),sg(cartel)],het,[]).

n([sg(cartograaf),pl(cartografen)],de,[]).

n([pl(cartoons),sg(cartoon)],de,[]).

n([pl('casco\'s'),sg(casco)],het,[]).

n([mass(caseïne)],de,[]).

n([mass(cash)],de,[]).

n([sg([cash,'&',carry]),pl([cash,'&','carry\'s'])],de,[]).

n([sg([cash,flow]),pl([cash,flows]),
   sg(cashflow),pl(cashflows)],de,[]).

n([pl('casino\'s'),sg(casino)],het,[]).

n([pl('cassa\'s'),sg(cassa)],de,[]).

n([pl(cassaties),sg(cassatie)],de,[]).

n([pl(cassetten),pl(cassettes),sg(cassette)],de,[]).

n([pl(cassettebanden),sg(cassetteband)],de,[]).

n([pl(cassetterecorders),sg(cassetterecorder)],de,[]).

n([pl(casts),sg(cast)],de,[]).

n([pl(castraties),sg(castratie)],de,[]).

n([sg(casual),pl(casuals)],de,[]). % kleding

n([pl(casus),pl(casussen),sg(casus)],de,[]).

n([pl(catacomben),sg(catacombe)],de,[]).

n([pl(catalogi),pl(catalogussen),sg(catalogus)],de,[]).

n([pl(catastrofen),pl(catastrofes),sg(catastrofe),
   pl(katastrofen),pl(katastrofes),sg(katastrofe)],de,[subject_sbar]).

n([pl(catechismussen),sg(catechismus),
   pl(katechismussen),sg(katechismus)],de,[]).

n([pl(categorieën),sg(categorie)],de,
  [app_measure, % de categorie "zanger"       ("type")
   measure],	% een grote categorie mensen  ("groep")
  [s(leeftijd)
  ]).

n([pl(caterings),sg(catering)],de,[]).

n([pl(catharses),pl(catharsissen),sg(catharsis)],de,[]).

n([sg(catwalk),pl(catwalks)],de,[]).

n([mass(causaliteit)],de,[]).

n([mass(cavalerie)],de,[]).

n([pl(cavaleristen),sg(cavalerist)],de,[]).

n([pl(cavaliers),sg(cavalier)],de,[]).

n([pl('cavia\'s'),sg(cavia)],de,[]).

n([sg(cc),pl('cc\'s')],het,[np_app_measure]). % Vlaams "cultureel centrum"?

n([meas(cc),pl('cc\'s')],de,[meas_mod,measure]).

n([pl('cd\'s'),pl('CD\'s'),sg(cd),sg('CD')],de,[],[dim('CD\'tje'),
                                                   dim('cd\'tje')]).

n([pl('cd-i\'s'),pl('CD-i\'s'),sg('cd-i'),sg('CD-i')],de,[]).

n([pl('cd-roms'),sg('cd-rom'),
   pl(cdroms),sg(cdrom),
   pl('CD-roms'),sg('CD-rom'),
   pl('CDroms'),sg('CDrom'),
   pl('CD-Roms'),sg('CD-Rom'),
   pl('CD-ROMs'),sg('CD-ROM')],de,[]).

n([pl(ceinturen),pl(ceintuurs),sg(ceintuur)],de,[],[dim(ceintuurtje)]).

%% VL: de cel vermiste personen van de federale politie
n([pl(cellen),sg(cel)],de,[app_measure]).

n([pl(cellen),sg(cel)],de,[],
  [h(b),h('B'),
   bloed,
   brandstof,
   douche,
   s(geslacht),
   glia,
   hersen,
   i(dood,doden),
   isoleer,
   kanker,
   s(lichaam),
   i(plant,planten),
   politie,
   stam,
   h(t),h('T'),
   zenuw,
   i(zon,zonne),
   dim(celletje)]).

n([mass(celibaat)],het,[]).

n([pl(cellisten),sg(cellist)],de,[]).

n([sg(cello),pl('cello\'s')],de,[]).

n([mass(cellofaan)],het,[]).

n([pl(cementen),sg(cement)],het,[]).

n([sg(census)],de,[]).

n([mass(censuur)],de,[],
  [zelf]).

n([sg(center),pl(centers)],both,[],[f([shared,service,center])]).

n([meas(cent),
   meas('¢'),
   pl(centen),
   pl(cents)],de,[meas_mod,measure],
  [dollar,
   euro,
   dim(centje)]).

%% de vijftig centiemen opslag
n([sg(centiem),
   pl(centiemen)],de,[measure]).

n([pl(centrales),sg(centrale)],de,[],
  [alarm,
   i(elektriciteit,electriciteit),
   i(elektriciteit,electriciteits),
   elektriciteit,s(elektriciteit),
   kern,
   i(kool,kolen),
   mest,
   taxi,
   telefoon,
   vak,
   i(water_kracht,waterkracht)
  ]).

n([pl(centralisaties),sg(centralisatie),
   pl(centralizaties),sg(centralizatie)],de,[]).

n([pl(centra),pl(centrums),sg(centrum)],het,
  [app_measure,np_app_measure],
  [aanmeld,
   afkick,
   asiel,
   i(asiel_zoeker,asielzoeker),
   i(asiel_zoeker,asielzoekers),
   s(bezoeker),
   i(brand_wond,brandwonden),
   confectie,
   controle,
   crisis,
   cultuur,
   epi,
   expertise,
   s(gezondheid),
   s(handel),
   informatie,
   jeugd,
   i(jong,jongeren),
   kennis,
   s(kunstenaar),
   onderwijs,
   s(onderzoek),
   s(ontmoeting),
   s(ontwikkeling),
   opvang,
   reken,
   revalidatie,
   i(seks,sex),seks,
   sport,
   s(stad),
   stilte,
   studie,
   tuin,
   s(uitgaan),
   uitzet,
   vertrek,
   wijk,
   winkel,
   zorg,
   [woon,en,zorg]
  ]).

n([sg([centre,court]),pl([centre,courts]),
   sg([center,court]),pl([center,courts])],het,[]).

n([mass('centrum-links')],both,[]).

n([mass('centrum-rechts')],both,[]).

n([sg('CEO'), pl('CEO\'s'),
   sg('ceo'), pl('ceo\'s')],de,[]).

n([pl(ceremonies),pl(ceremoniën),sg(ceremonie)],de,[],
  [s(opening),
   s(sluiting)]).

n([mass(ceremonieel)],het,[]).

n([pl(ceremoniemeesters),sg(ceremoniemeester)],de,[]).

n([pl(certificaten),sg(certificaat)],het,[],
  [invoer,
   h('ISO'),
   uitvoer]).

n([sg(certificering)],de,[]).

n([mass(cetera)],het,[]).

n([sg(chador),pl(chadors)],de,[]).

n([pl(chalets),sg(chalet)],both,[]).

n([pl(champagnes),sg(champagne)],de,[]).

n([pl(champignons),sg(champignon)],de,[]).

n([mass(chance)],de,[sbar,vp]).

n([pl(changes),sg(change)],de,[]).

n([pl(chansons),sg(chanson)],het,[]).

n([sg(chansonnier),pl(chansonniers)],de,[]).

n([mass(chantage)],de,[]).

n([mass(chaos)],de,[],[s(verkeer)]).

n([pl(charges),sg(charge)],de,[]).

n([pl('charisma\'s'),sg(charisma)],het,[vp]).

n([pl(charlatans),sg(charlatan)],de,[]).

n([pl(charmes),sg(charme)],de,[]).

n([sg(charter),pl(charters)],both,[]).

n([pl(chassis),sg(chassis)],het,[]).

n([sg(chat)],de,[]).

n([pl(chauffeurs),sg(chauffeur)],de,[],
  [bus,
   taxi,
   i(vracht_wagen,vrachtwagen)]).

n([sg(chauvinisme)],het,[]).

n([pl(checks),sg(check)],de,
  [sbar]).

n([sg([check,in]),
   sg('check-in')],de,[]).

n([sg(checklist),pl(checklists)],de,[]).

n([sg(checkpoint),pl(checkpoints)],both,[]).

n([pl([checks,and,balances]),pl([checks,'&',balances])],de,[]).

n([mass(cheddar)],de,[]).

n([pl(chefs),sg(chef)],de,[app_measure],[]).

n([sg('chef-staf')],de,[],[]).

n([pl(chefs),sg(chef)],de,[],
  [kabinet, s(kabinet),
   leger,
   pers,
   s(personeel),
   politie,
   staf,
   s(station)]).

n([sg([chef,de,bureau]),pl([chefs,de,bureau])],de,[]).

n([sg([chef,de,mission]),pl([chefs,de,mission]),
   sg(['Chef',de,'Mission']),pl(['Chefs',de,'Mission'])],de,[]).

n([pl(chemicaliën)],de,[]).

n([pl(chemici),sg(chemicus)],de,[],[bio]).

n([mass(chemie)],de,[]).

n([mass([chemin,de,fer])],de,[]).

n([pl(cheques),sg(cheque)],de,[],
  [s(traveller)]).

n([mass(chianti)],de,[]).

n([mass(chic)],de,[]).

n([sg([chief,executive]),pl([chief,executives])],de,[]).

n([sg([chief,executive,officer]),pl([chief,executive,officers])],de,[]).

n([sg([chief,financial,officer]),pl([chief,financial,officers])],de,[]).

n([sg([chief,operating,officer]),pl([chief,operating,officers])],de,[]).

n([sg(chick),pl(chicks)],de,[]).

n([sg(chill)],de,[]).

n([pl(chimpansees),sg(chimpansee)],de,[],[dim(chimpanseetje)]).

n([pl(chips),sg(chip)],de,[]).

n([sg(chipknip),pl(chipknippen),
   sg([chip,knip]),pl([chip,knippen])],de,[]).

n([sg(chipper),pl(chippers)],de,[]).

n([pl(chirurgen),sg(chirurg)],de,[],
  [boom,		     % GvN: dat wilde ik worden toen ik 16 was, maar met hoogtevrees niet handig
   i(bort_kanker,borstkanker),
   kaak,
   hart,
   neuro]).

n([mass(chirurgie)],de,[],[hart]).

n([mass(chlamydia)],de,[]).

n([mass(chloor)],both,[]).

n([mass(chloormequat)],both,[]).

n([mass(chocola)],de,[],[dim(chocolaatje)]).

n([mass(chocolade)],de,[]).

n([mass(chocomel)],de,[]).

n([mass(cholera)],de,[]).

n([mass(cholesterol)],both,[]).

n([pl(choreografen),sg(choreograaf)],de,[]).

n([pl(choreografes),sg(choreografe)],de,[]).

n([pl(choreografieën),sg(choreografie)],de,[]).

n([pl(christenen),sg(christen)],de,[]).

n([pl('christen-democraten'),sg('christen-democraat'),
   pl('Christen-democraten'),sg('Christen-democraat'),
   pl(christendemocraten),sg(christendemocraat),
   pl('Christendemocraten'),sg('Christendemocraat')   ],de,[]).

n([pl(christendemocraten),sg(christendemocraat)],de,[]).

n([mass(christendom)],het,[]).

n([mass(christenheid)],de,[]).

n([pl(chromosomen),sg(chromosoom)],het,[],
  [h(x), h('X'),
   h(y), h('Y')]).

n([sg(chroniqueur),pl(chroniqueurs)],de,[]).

n([pl(chronologieën),sg(chronologie)],de,[]).

n([mass(chroom)],het,[]).

n([mass(cider)],de,[]).

n([pl(cijfers),sg(cijfer)],het,[app_measure],
  [dim(cijfertje)]).

n([pl(cijfers),sg(cijfer)],het,[],
  [s(bedrijf),
   geboorte,
   groei,
   i(half_jaar,halfjaar),
   inflatie,
   jaar,
   kijk,
   kwartaal,
   omzet,
   oplage,
   rapport,
   sterfte,
   streef,
   verkoop,
   s(werkgelegenheid),
   s(werkeloosheid),s(werkloosheid),
   winst,
   dim(cijfertje)]).

n([pl(cilinders),sg(cilinder)],de,
  [measure],   % 5000 cilinders chloorgas
  [dim(cilindertje)]).

n([pl(cineasten),sg(cineast)],de,[]).

n([pl('cinema\'s'),sg(cinema)],de,[]).

n([pl(cipiers),sg(cipier)],de,[]).

n([pl(cipressen),sg(cipres)],de,[]).

n([pl(circuits),sg(circuit)],het,[]).

n([pl(circulaires),sg(circulaire)],de,[]).

n([mass(circulatie)],de,[]).

n([pl(circussen),sg(circus)],both,[]).

n([pl(cirkels),sg(cirkel)],de,[],
  [graan,
   midden,
   dim(cirkeltje)]).

n([pl(citaten),sg(citaat)],het,[]).

n([sg('CITO-toets'),pl('CITO-toetsen')],de,[]).

n([pl(citroenen),sg(citroen)],de,[],[dim(citroentje)]).

n([mass(citroengeel)],het,[]).

n([pl(citroenschillen),sg(citroenschil)],de,[]).

n([pl('city\'s'),sg(city)],de,[]).

n([sg([civil,parish]),pl([civil,parishes])],de,[]).   % soortement gemeente in UK

n([sg([civil,service])],de,[]).

n([sg([civil,society])],de,[]).

n([pl(civilisaties),sg(civilisatie),
   pl(civilizaties),sg(civilizatie)],de,[]).

n([pl(claims),sg(claim)],de,[sbar]).

n([pl(clans),sg(clan)],de,[]).

n([sg(classic),pl(classics)],de,[]).

n([pl(classici),sg(classicus)],de,[]).

n([pl(classificaties),sg(classificatie)],de,[app_measure]).

n([pl(clausules),sg(clausule)],de,
  [sbar],
  [s(wijziging)]).

n([pl(claxons),sg(claxon)],de,[]).

n([sg([clean,room]),pl([clean,rooms])],de,[]).

n([mass(clementie)],de,[]).

n([mass(clerus)],de,[]).

n([pl(clichés),
   pl('cliché\'s'),
   sg(cliché)],het,
  [subject_sbar,
   sbar],
  [dim(clicheetje)]).

n([mass(clientèle),
   mass(cliëntèle)],de,[]).

n([sg(cliffhanger),pl(cliffhangers)],de,[]).

n([pl(climaxen),sg(climax)],de,[]).

n([mass(clinch)],de,[pred_pp(in)]).

n([sg('Cliniclown'),pl('Cliniclowns')],de,[]).

n([sg(clip),pl(clips)],de,[],[video]).

n([pl(clitores),sg(clitoris)],de,[]).

n([pl(cliënten),sg(cliënt)],de,[]).

n([pl(cliëntes),sg(cliënte)],de,[]).

n([pl('close-ups'),sg('close-up'),
   pl([close,ups]),sg([close,up])],de,[]).

n([pl(clous),sg(clou)],de,[subject_sbar]).

n([sg(cloud),pl(clouds)],de,[]).

n([pl(clowns),sg(clown)],de,[],[dim(clowntje)]).

n([pl(clubs),sg(club)],de,
  [measure],
  [dim(clubje)]).               % een grote club mensen
  
n([pl(clubs),sg(club)],de,[np_app_measure],
  [amateur,
   basketbal,
   [american,football],
   ['American',football],
   [betaald,voetbal],
   f([country]),
   droom,
   eredivisie,
   fan,
   fusie,
   golf,
   handbal,
   hockey,
   honkbal,
   lees,
   motor,
   nacht,
   prof,
   sateliet,
   seks,
   i(seks,sex),
   sport,
   tennis,
   thuis,
   top,
   turn,
   voetbal,
   i(amateur_voetbal,amateurvoetbal),
   volleybal,
   i(vriend,vrienden),
   zwem,
   dim(clubje)]).

n([sg([club,card]),pl([club,card])],de,[]).

%% alleen bij golf
n([sg([club,house]),
   sg('club-house'),
   sg(clubhouse)],het,[]).

n([pl(clusters),sg(cluster)],both,[]).

n([pl(coaches),sg(coach)],de,[app_measure],
  [h(assistent),
   hoofd,
   h(interim),
   schaats,
   sprint,
   i(trainer,'trainer/'),
   h(trainer),
   voetbal]).

n([mass(coaching)],de,[]).

n([pl(coalities),sg(coalitie)],de,[],[s(regering)]).

n([mass(coca)],de,[]).

n([mass(cocaïne)],de,[]).

n([pl(cockpits),sg(cockpit)],de,[]).

n([sg([cockpit,voice,recorder]),pl([cockpit,voice,recorders])],de,[]).

n([pl(cocktails),sg(cocktail)],de,[measure]).

n([pl(cocktails),sg(cocktail)],de,[measure],[molotov]).

n([sg(cocon),pl(cocons)],de,[]).

n([pl(codes),sg(code)],de,[sbar],[]).

n([pl(codes),sg(code)],de,[app_measure], % code rood, code oranje
  [dress]).

n([pl(codes),sg(code)],de,[],
  [s(activering),
   bar,
   s(boeking),
   bron,
   pin]).

n([sg(coderekening),pl(coderekeningen)],de,[]).

n([sg(codering),pl(coderingen)],de,[]).

n([pl(codices),sg(codex)],de,[]).

n([sg(codicil),pl(codicillen)],het,[]).

n([pl(codificaties),sg(codificatie)],de,[]).

n([sg(coëfficiënt),pl(coëfficiënten)],de,[]).

n([sg(coffeeshop),pl(coffeeshops),
   sg([coffee,shop]),pl([coffee,shops]),
   sg(koffieshop),pl(koffieshops),
   sg([koffie,shop]),pl([koffie,shops])],de,[]).

n([mass(cognac)],de,[]).

n([pl(cognities),sg(cognitie)],de,[vp]).

n([mass(coherentie)],de,[]).

n([mass(cohesie)],de,[]).

n([sg(coke),pl(coke)],de,[]).

n([pl(cols),sg(col)],de,[]).

n([pl('cola\'s'),sg(cola)],de,[]).

n([pl(colberts),sg(colbert)],het,[],
  [dim(colbertje)]).

n([pl(collaborateurs),sg(collaborateur)],de,[]).

n([mass(collaboratie)],de,[]).

n([pl(collages),sg(collage)],de,[]).

n([pl(collecten),pl(collectes),sg(collecte)],de,[]).

n([pl(collecties),sg(collectie)],de,[measure,
                                     np_app_measure]).

n([pl(collecties),sg(collectie)],de,[],
  [kunst,
   winter,
   zomer]).

n([pl(collectieven),sg(collectief)],het,[],
  [s(personeels)]).

n([pl(collectiviteiten),sg(collectiviteit)],de,[]).

n([sg(['collector\'s',item]),pl(['collector\'s',items]),
   sg([collectors,item]),pl([collectors,items])],both,[]).

n([pl('collega\'s'),pl(collegae),sg(collega)],de,[]).

n([pl(colleges),sg(college)],het,[app_measure],
  [advies,
   kies,
   s(recht),
   schepen,
   tucht]).

n([pl(colleges),sg(college)],het,[],
  [hoor]).

n([pl(collegezalen),sg(collegezaal)],de,[]).

n([sg(collier),pl(colliers)],het,[],[parel]).

n([pl(colloquia),sg(colloquium)],het,[]).

n([mass(colon)],het,[]).

n([pl(colonnes),sg(colonne)],de,[measure]).

n([pl(colts),sg(colt)],de,[]).

n([pl(coltruien),sg(coltrui)],de,[]).

n([pl(columns),sg(column)],de,[]).

n([pl(columnisten),sg(columnist)],de,[]).

n([pl(columnistes),sg(columniste)],de,[]).

n([pl('coma\'s'),sg(coma)],both,[pred_pp(in)]).

n([pl(combinaties),sg(combinatie)],de,
  [app_measure,
   subject_sbar  % het was een vervelende combinatie (van factoren) dat ...
  ]).

n([pl(combinaties),sg(combinatie)],de,
  [],[i(toets,toetsen)]).

n([pl(comebacks),sg(comeback),
   pl([come,backs]),sg([come,back]),
   sg('come-back'),pl('come-backs')],de,[]).

n([sg(comedy),pl(comedies)],de,[],
  [f([screwball]),
   f([stand,up]),
   f(['stand-up'])
  ]).

n([mass(comfort)],het,[],[zit]).

n([sg(comic),pl(comics)],de,[]).

n([sg(comitaat),pl(comitaten)],both,[]).
 
n([pl(comités),
   pl('comité\'s'),
   sg(comité)],het,
  [app_measure],
  [actie,
   s(bemiddeling),
   buurt,
   directie,
   kandidaat,
   organisatie,
   referendum,
   dim(comiteetje)]).

n([sg(comitologie)],de,[]).

n([pl(commandanten),sg(commandant),
   pl(kommandanten),sg(kommandant)],de,[],
  [brandweer,
   kamp,
   leger,
   sectie,
   wacht
  ]).

n([pl(commandeurs),sg(commandeur)],de,[]).

n([pl('commando\'s'),sg(commando)],de,[],[moord]).

n([pl('commando\'s'),sg(commando)],het,[start_app_measure,
					sbar,vp]).

n([sg([commedia,'dell\'arte'])],both,[]).

n([pl(commentaren),sg(commentaar)],both,[sbar]). 

n([pl(commentatoren),pl(commentators),sg(commentator)],de,[]).

n([pl(commercials),sg(commercial)],de,[]).

n([mass(commercialisering)],de,[]).

n([mass(commercie)],de,[]).

n([pl(commiezen),sg(commies)],de,[]).

n([pl(commissariaten),sg(commissariaat)],het,[],
  [post_h(generaal)]).

n([pl(commissarissen),sg(commissaris)],de,[],
  [hoofd,
   landbouw,
   politie,
   h(president),
   h(rechter),
   f(rechter),
   'Euro',euro,
   h('EU'),
   s(rijk),
   sport
  ]).

n([pl(commissies),sg(commissie),
   pl(kommissies),sg(kommissie)],de,
  [app_measure],  % de Commissie juridische zaken
  [advies,
   arbitrage,
   i(atleet,atleten),
   post_h('Bakker'),
   s(beroep),
   beurs,
   post_h('Blok'),
   post_wh(['De','Beer']),
   post_wh(['De','Boer']),
   post_wh(['De','Rooy']),
   post_wh(['De','Ruiter']),
   post_wh(['De','Vries']),
   post_h('Donner'),
   enquête,
   i(geschil,geschillen),
   huur,
   post_h('Kalsbeek'),
   kamer,i(kamer,'Kamer'),
   kies,
   i(klacht,klachten),
   i(mens_recht,mensenrechten),
   milieu,
   s(onderzoek),
   post_h('Oosting'),
   s(parlement),
   s(raad),
   selectie,
   sport,
   s(staat),
   post_h('Tabaksblat'),
   s(toetsing),
   tucht,
   post_wh([van,'Dijk']), post_wh(['Van','Dijk']),
   post_wh([van,'Es']),   post_wh(['Van','Es']),
   post_wh([van,'Kemenade']),   post_wh(['Van','Kemenade']),
   post_wh([van,'Traa']), post_wh(['Van','Traa']),
   s(vertrouwen),
   visitatie,
   s(waarheid)
  ]).

n([pl(commissionairs),sg(commissionair)],de,[]).

n([pl(commodes),sg(commode)],de,[]).

n([pl(commoties),sg(commotie)],de,[]).

n([pl(communes),sg(commune)],de,[]).

n([sg([commune,nouvelle]),pl([communes,nouvelles])],de,[]).

n([pl(communicaties),sg(communicatie)],de,[],
  [s(bedrijf),
   marketing]).

n([pl('communicatie-processen'),sg('communicatie-proces')],het,[]).

n([pl(communicatiekanalen),sg(communicatiekanaal)],het,[]).

n([pl(communies),pl(communiën),sg(communie)],de,[]).

n([pl(communiqués),
   pl('communiqué\'s'),
   sg(communiqué)],het,[],
  [dim(communiqueetje)]).

n([sg([communis,opinio])],de,[subject_sbar]).

n([mass(communisme)],het,[]).

n([pl(communisten),sg(communist)],de,[]).

n([pl(compagnies),pl(compagnieën),sg(compagnie)],de,[measure]).

n([pl(compagnons),sg(compagnon)],de,[]).

n([mass(compassie)],de,[]).

n([pl(compartimenten),sg(compartiment)],het,[]).

n([pl(compensaties),sg(compensatie)],de,[sbar]).

n([pl(compensaties),sg(compensatie)],de,[],[prijs]).

n([pl(competenties),sg(competentie)],de,[vp]).

n([pl(competities),sg(competitie)],de,
  [temp_mod,
   sbar,
   app_measure],
  [klaverjas,
   na,h(na),
   s(najaar),
   s(voorjaar),
   zaterdag]).

n([pl(competities),sg(competitie),
   ignore_stem(competitie)],de,[],
  [basketbal,
   club,
   prof,
   voetbal]).

n([pl(compilaties),sg(compilatie)],de,[]).

n([pl(complexen),sg(complex)],het,[],
  [i(appartement,appartementen),
   i(cel,cellen),
   i(gebouw,gebouwen),
   i(kantoor,kantoren),
   sport,
   s(training),
   woon]).

n([mass([complex,instruction,set,computing])],both,[]).

n([mass(complexiteit)],de,[]).

n([pl(complicaties),sg(complicatie)],de,[subject_sbar]).

n([pl(complimenten),sg(compliment)],het,[sbar],[dim(complimentje)]).

n([pl(complotten),sg(complot),
   pl(komplotten),sg(komplot)],het,[vp]).

n([pl(componenten),sg(component)],de,[app_measure]).

n([pl(componisten),sg(componist)],de,[],
  [gast,
   opera]).

n([pl(componistes),sg(componiste)],de,[]).

n([pl(composities),sg(compositie)],de,[]).

n([pl(compromis),pl(compromissen),sg(compromis)],het,[sbar]).

n([pl(computers),sg(computer)],de,[],
  [boord,
   micro,i(micro,mikro),
   quantum,
   schaak,
   schoot,
   spel,
   tafel,
   vertaal,
   dim(computertje)
  ]).

n([sg(comédienne),pl(comédiennes)],de,[]).

n([pl(concentraties),sg(concentratie)],de,[measure]).

n([pl(concepten),sg(concept)],het,
  [sbar,
   app_measure]).

n([pl(concepties),sg(conceptie)],de,[sbar,vp]).

n([pl(concerns),sg(concern)],het,[],
  [auto,
   bagger,
   bouw,
   chemie,
   computer,
   s(detailhandel),
   i(elektronika,electronica),
   elektronica,
   energie,
   farmacie,
   frisdrank,i(frisdrank,frisdranken),
   media,
   moeder,
   olie,
   staal,
   supermarkt,
   technologie,
   telecom,h(telecom),
   transport,
   s(uitgever),
   uitzend,
   wh([van,der,'Valk']),
   verzekering,s(verzekering),
   s(voeding),
   voedingsmiddelen,
   zuivel]).

n([pl(concerten),sg(concert)],het,[],
  [benefiet,
   fluit,
   s(opening),
   openlucht,
   orgel,
   piano,
   pop,
   slot,
   viool]).

n([pl(concerti),pl('concerto\'s'),sg(concerto)],het,[]).

n([pl(concessies),sg(concessie)],de,[subject_vp,
                                     subject_sbar]).

n([pl(concilies),pl(conciliën),sg(concilie)],het,[]).

n([pl(conciërges),sg(conciërge)],de,[]).

n([sg(conclaaf),pl(conclaven)],het,[]).

n([pl(conclusies),sg(conclusie)],de,
  [sbar,
   vp,
   subject_sbar],
  [eind]).

n([pl(concoursen),sg(concours)],both,[]).

n([sg([concours,hippique])],het,[]).

n([pl(concretiseringen),sg(concretisering)],de,[]).

n([pl(concubines),sg(concubine)],de,[]).

n([pl(concurrenten),sg(concurrent)],de,[]).

n([pl(concurrentes),sg(concurrente)],de,[]).

n([mass(concurrentie)],de,[],
  [s(beleid),
   prijs]).

n([pl('concurrentie-posities'),sg('concurrentie-positie')],de,[]).

n([sg(concurrentiestrijd)],de,[]).

n([pl(condities),pl(conditiën),sg(conditie)],de,
  [pred_pp(in),
   pred_pp(uit),
   sbar]).

n([pl(condooms),sg(condoom)],het,[]).

n([pl(conducteurs),sg(conducteur)],de,[],
  [tram,
   trein]).

n([pl(conductrices),sg(conductrice)],de,[],
  [tram,
   trein]).

n([pl(confederaties),sg(confederatie)],de,[]).

n([pl(conferenties),sg(conferentie)],de,[],
  [donor,
   klimaat,
   top]).

n([pl(configuraties),sg(configuratie)],de,[]).

n([pl(conflicten),sg(conflict)],het,[pred_pp(in)],
  [s(arbeid),
   i(belang,belangen),
   s(handel)]).

n([mass(conformisme)],het,[]).

n([pl(confrontaties),sg(confrontatie)],de,[]).

n([pl(congesties),sg(congestie)],de,[]).

n([pl(conglomeraten),sg(conglomeraat)],het,[]).

n([pl(congregaties),pl(congregatiën),sg(congregatie)],de,[]).

n([pl(congressen),sg(congres)],het,[],[partij]).

n([pl(coniferen),sg(conifeer)],de,[]).

n([pl(conjuncturen),sg(conjunctuur),sg(conjuctuur),pl(conjuncturen)],de,[],
  [hoog,
   laag]).

n([pl(connecties),sg(connectie)],de,[]).

n([pl(conrectoren),pl(conrectors),sg(conrector)],de,
  [app_measure]).               % conrector onderbouw

n([mass(consensus)],de,[sbar,subject_sbar]).

n([pl(consenten),sg(consent)],het,[sbar,vp]).

n([pl(consequenties),sg(consequentie)],de,
  [subject_sbar,
   sbar]).

n([pl(conservatieven),sg(conservatief)],de,[],[neo]).

n([pl('Conservatieven'),sg('Conservatief')],de,[]).

n([mass(conservatisme)],het,[]).

n([pl(conservatoren),pl(conservators),sg(conservator)],de,[measure]).

n([pl(conservatoria),pl(conservatoriums),sg(conservatorium)],het,[]).

n([pl(conserven)],de,[],[vlees]).

n([pl(conservenblikken),sg(conservenblik)],het,[],[dim(conservenblikje)]).

n([mass(conservering)],de,[]).

n([pl(consideraties),sg(consideratie)],de,[sbar,vp]).

n([sg(consigne),pl(consignes)],het,
  [subject_sbar,
   subject_vp,
   sbar,
   vp]).

n([mass(consistentie)],de,[]).

n([sg(console)],de,[]).

n([mass(consolidatie)],de,[]).

n([pl(consorten)],de,[]).

n([pl(consortia),pl(consortiums),sg(consortium)],het,[]).

n([pl(constanten),sg(constante)],de,[measure]).

n([pl(constateringen),sg(constatering)],de,[sbar,vp]).

n([pl(constellaties),sg(constellatie)],de,[]).

n([mass(consternatie)],de,[]).

n([pl(constituties),sg(constitutie)],de,[]).

n([sg(contructeur),pl(constructeurs)],de,[]).

n([pl(constructies),sg(constructie)],de,[],
  [s(bescherming)]).

n([pl(consuls),sg(consul)],de,[]).

n([pl(consulaten),sg(consulaat)],het,[]).

n([pl('consulaten-generaal'),sg('consulaat-generaal')],het,[]).

n([pl(consulenten),sg(consulent)],de,[]).

n([pl(consulten),sg(consult)],het,[]).

n([sg(consultant),pl(consultants)],de,[]).

n([pl(consultaties),sg(consultatie)],de,[]).

n([pl(consumenten),sg(consument)],de,[]).

n([mass(consumentenvertrouwen)],het,[]).

n([pl(consumpties),sg(consumptie)],de,[],[alcohol]).

n([pl(contacten),sg(contact),
   pl(kontakten),sg(kontakt)],het,[pred_pp(in)],
  []).

n([pl(contacten),sg(contact),
   pl(kontakten),sg(kontakt)],het,[],
  [bal,
   oog]).

n([pl(contactpersonen),sg(contactpersoon)],de,[]).

n([pl(containers),sg(container)],de,[measure],
  [afval,
   vuil,
   zee]).

n([pl(contanten)],de,[]).

n([mass(content),
   pl(contents)],de,[]).                    % web inhoud

n([sg(context),pl(contexten)],de,[]).

n([pl(continenten),sg(continent)],het,[]).

n([pl(contingenten),sg(contingent)],het,[measure],[tarief]).

n([mass(continuïteit)],de,[]).

n([sg(conto)],het,[]).

n([pl(contouren),sg(contour)],de,[]).

%% VL: met pro en contra
n([mass(contra)],both,[]).

n([pl(contracten),sg(contract),
   pl(kontrakten),sg(kontrakt)],het,[],
  [s(arbeid),
   flex,
   huur,
   s(inburgering),
   f([love]),
   i(plaat,platen),
   prestatie,
   termijn,
   voetbal]).

n([pl(contracties),sg(contractie)],de,[]).

n([pl(contradicties),sg(contradictie)],de,[sbar]).

n([pl(contraspionages),sg(contraspionage)],de,[]).

n([pl(contrasten),sg(contrast)],het,[pred_pp(in)]).

n([pl(contreien)],de,[]).

n([pl(contributies),sg(contributie)],de,[]).

n([pl(controles),sg(controle)],de,
  [pred_pp(onder),
   sbar]).

n([pl(controles),sg(controle)],de,[],
  [alcohol,
   s(begroting),
   doping,
   grens,
   paspoort,
   s(snelheid),
   s(spelling),
   s(veiligheid),
   zelf,h(zelf)]).

n([pl(controleurs),sg(controleur)],de,[],
  []).

n([pl(controversen),pl(controverses),sg(controverse)],de,[sbar]).

n([pl(convenants),pl(convenanten),sg(convenant)],het,[app_measure]).

n([sg(convent)],het,[]).

n([pl(conventies),sg(conventie)],de,[]).

n([sg(convergentie),pl(convergenties)],de,[]).

n([pl(conversaties),sg(conversatie)],de,[]).

n([pl(conversies),sg(conversie)],de,[]).

n([sg(cooky),pl(cookies)],both,[]).

n([mass(cool)],both,[]).  % toppunt/summum/definitie van cool

n([pl(copieën),sg(copie)],de,[],[dim(copietje)]).

n([sg(copyright)],both,[]).

n([mass([coq,au,vin])],de,[]).

n([pl(cordons),sg(cordon)],het,[]).

n([sg([cordon,blue]),pl([cordon,blues])],de,[]).

n([mass(corduroy)],both,[]).

n([pl([corn,flakes])],de,[]).

n([mass([corned,beef])],de,[]).

n([pl(corners),sg(corner)],de,[],[straf]).

n([mass([corporate,communicatie]),
   mass([corporate,communication]),
   mass([corporate,communications])],both,[]).

n([mass([corporate,finance])],both,[]).

n([mass([corporate,governance])],both,[]).

n([mass([corporate,governance,code])],de,[]).  %Tabaksblat

n([mass([corporate,identity])],both,[]).

n([pl(corporaties),sg(corporatie)],de,[],
  [woning,
   woningbouw]).

n([sg([corps,de,ballet])],het,[]).

n([pl(corpora),sg(corps)],het,[]).

n([sg([corps,diplomatique])],het,[]).

n([pl(corpora),sg(corpus)],het,[app_measure]).

n([pl(correcties),sg(correctie)],de,[sbar]).

n([mass(correctheid)],de,[]).

n([pl(correlaties),sg(correlatie)],de,[]).

n([pl(correspondenten),sg(correspondent)],de,[]).

n([pl(correspondentes),sg(correspondente)],de,[]).

n([pl(correspondenties),sg(correspondentie)],de,[]).

n([pl(corridors),sg(corridor)],de,[]).

n([pl(corrupties),sg(corruptie)],de,[]).

n([sg(corticosteroïde),pl(corticosteroïden)],de,[]).

n([sg(coryfee),pl(coryfeeën)],de,[]).

n([mass(cosmetica)],de,[]).

n([mass(cosmos)],de,[]).

n([pl(costuums),sg(costuum)],het,[],[dim(costuumpje)]).

n([pl(coulissen),pl(coulisses),sg(coulisse)],de,[]).

n([sg(counsel),pl(counsels)],de,[]).

n([pl(counselors),sg(counselor)],de,[]).

n([pl(counters),sg(counter)],de,[]).

n([sg(county),pl('county\'s')],de,[]).

n([mass(country)],de,[]).

n([mass([country,'&',western]),
   mass([country,and,western])],de,[]).

n([mass([country,music])],de,[]).

n([pl(coups),sg(coup)],de,[]).

n([pl(coupletten),sg(couplet)],het,[]).

n([sg(coupe),pl(coupes)],de,[measure]).

n([pl(coupés),
   pl('coupé\'s'),
   sg(coupé)],de,[],
  [dim(coupeetje)]).

n([sg(coupon),pl(coupons)],de,[]).

n([sg(coupure),pl(coupures)],de,[],
  [dijk]).

n([pl(couranten),sg(courant)],de,[]).

n([sg(coureur),pl(coureurs)],de,[],
  [auto,
   wh(['Formule','1']),
   motor]).

n([sg(courgette),pl(courgettes)],de,[]).

n([mass(couscous)],de,[]).

n([sg(couturier),pl(couturiers)],de,[]).

n([pl(covers),sg(cover)],de,[]).

n([sg([cover,up])],de,[]).

n([pl(cowboys),sg(cowboy)],de,[],[dim(cowboytje)]).

n([sg(coïtus)],de,[]).

n([pl(coöperaties),sg(coöperatie)],de,[],
  [s(verbruiker)]).

n([pl(coördinaten),sg(coördinaat)],de,[],
  [hemel,
   net,
   pool]).

n([pl(coördinaties),sg(coördinatie)],de,[]).

n([pl(coördinatoren),sg(coördinator)],de,
  [app_measure],
  [regio,
   taal]).

n([sg(crack),pl(cracks)],de,[]).

n([pl(crashes),sg(crash)],de,[],[vliegtuig]).

n([pl(creaties),sg(creatie)],de,[]).

n([mass(creativiteit)],de,[]).

n([pl(creaturen),sg(creatuur)],het,[]).

n([sg(crèche),pl(crèches),
   sg(chreche),pl(chreches)
  ],de,[]).

n([mass(credit)],het,[]).

n([pl(crediteuren),pl(crediteurs),sg(crediteur)],de,[]).

n([stem(creditcard),
   pl([credit,cards]),sg([credit,card]),
   pl('credit-cards'),sg('credit-card'),
   pl(creditcards),sg(creditcard)],de,[]).

n([pl('credo\'s'),sg(credo)],het,
  [np_app_measure,
   start_app_measure,
   subject_sbar,
   sbar]).

n([pl(crematies),sg(crematie)],de,[]).

n([pl(crematoria),pl(crematoriums),sg(crematorium)],het,[]).

n([pl(creolen),sg(creool)],de,[]).

n([mass(creools)],het,[]).

n([mass(crepe)],de,[]).

n([sg(crew),pl(crews)],de,[]).

n([mass(cricket)],het,[]).

n([sg(cricketer),pl(cricketers)],de,[]).

n([mass(crime)],de,[sbar,vp]).

n([mass(criminaliteit)],de,[],
  [computer,
   jeugd]).

n([pl(criminelen),sg(crimineel)],de,[],
  [i(draai_deur,draaideur),
   top]).

n([pl(criminologen),sg(criminoloog)],de,[]).

n([pl(crises),pl(crisissen),sg(crisis)],de,[],
  [i(bank,banken),
   s(bestuur),
   h('BSE'),
   corona,
   energie,
   s(identiteit),
   h('Kosovo'),
   s(kabinet),
   krediet,
   ['mond-',en,klauwzeer],
   h(mkz),h('MKZ'),
   olie,
   i(schuld,schulden),
   'Suez',h('Suez'),
   valuta,
   s(vertrouwen),
   i(vluchteling,vluchtelingen),
   vorm
  ]).

n([mass('crisis-interventie')],de,[]).

n([pl(criteria),pl(criteriums),sg(criterium)],het,
  [app_measure,
   subject_sbar,
   subject_vp,
   sbar,
   vp]).

n([pl(criteria),pl(criteriums),sg(criterium)],het,[],
  [convergentie,
   s(gunning),
   selectie]).

n([sg(criticaster),pl(criticasters)],de,[]).

n([pl(critici),sg(criticus)],de,[]).

n([pl(croissants),sg(croissant)],de,[]).

n([sg(cross),pl(crosses),pl(crossen)],both,[],
  [motor]).

n([sg('cross-country')],de,[]).

n([sg([cross,country])],de,[]).

n([sg([cross,over]),pl([cross,overs])],de,[]).

n([sg('cross-over'),pl('cross-overs')],de,[]).

n([sg(crossing),pl(crossings)],de,[]).

n([pl(croupiers),sg(croupier)],de,[]).

n([mass(crowdfunding)],de,[]).

n([pl('cru\'s'),pl(crus),sg(cru)],de,[]).

n([sg(cruise),pl(cruises)],de,[]).

n([mass([cruise,control])],de,[]).

n([sg(crux)],de,[subject_sbar,subject_vp]).

n([sg(cryptogram),pl(cryptogrammen)],het,[]).

n([pl(crèches),sg(crèche)],de,[]).

n([pl(crèmes),sg(crème)],de,[]).

n([mass([crème,fraîche]),
   mass([crême,fraîche]),
   mass([crème,fraiche]),
   mass([crême,fraiche])],de,[]).

n([mass([crème,brulée]),
   mass([crême,brulée]),
   mass([crême,brûlée]),
   mass([crème,brûlée]),
   mass([crême,brûleé]),
   mass([crème,brûleé])],de,[]).

n([sg([crème,de,la,crème]),
   sg([crême,de,la,crême])],de,[]).

n([mass([crème,de,menthe]),
   mass([crême,de,menthe])],de,[]).

n([pl(cuisines),sg(cuisine)],de,[]).

n([mass(cult)],de,[]).

n([pl(cultivars),sg(cultivar)],de,[]).

n([pl(culten),sg(cultus)],de,[],
  [i(persoon,personen)]).

n([pl(culturen),sg(cultuur)],de,[],
  [aqua,
   beeld,
   s(bedrijf),
   s(bestuur),
   claim,
   film,
   gedoog,
   jeugd,
   i(jong,jongeren),
   massa,
   mono,
   pop,
   wieler
  ]).

n([mass([cum,laude])],both,[]).

n([pl(cumulaties),sg(cumulatie)],de,[]).

n([pl(curatelen),sg(curatele)],de,[]).

n([pl(curatoren),pl(curators),sg(curator)],de,[]).

n([sg([curriculum,vitae]),
   pl([curricula,vitae])],het,[]).

n([mass(curie)],de,[]).

n([pl(curiositeiten),sg(curiositeit)],de,[sbar]).

n([pl(curiosa),sg(curiosum)],het,[]).

n([pl(curricula),sg(curriculum)],het,[]).

n([sg(cursief),pl(cursieven)],both,[]).

n([pl(cursisten),sg(cursist)],de,[app_measure]).

n([pl(cursussen),sg(cursus)],de,[app_measure],
  [spoed,
   dim(cursusje)]).

n([pl(cursussen),sg(cursus)],de,[],
  [s(inburgering),
   opfris,
   dim(cursusje)]).

n([pl(curven),sg(curve)],de,[]).

n([sg(custom),pl(customs)],both,[]).

n([sg([customer,care])],de,[]).

n([sg([customer,service])],de,[]).

n([sg([customer,support])],de,[]).

n([sg(cv),sg('CV'),pl('cv\'s'),pl('CV\'s')],both,
  [],[dim('cv\'tje'),
      dim('CV\'tje'),
      dim('cv-tje'),
      dim('CV-tje')]).

n([mass(cyanide)],both,[]).

n([sg(cycloon),pl(cyclonen)],de,[]).

n([pl(cycli),pl(cyclussen),sg(cyclus)],de,[app_measure]).

n([pl(cylinders),sg(cylinder)],de,[]).

n([sg(cynicus),pl(cynici)],de,[]).

n([pl(cynismen),sg(cynisme)],het,[]).

n([pl(cypressen),sg(cypres)],de,[]).

n([pl('d\'s'),sg(d)],de,[],[dim('d\'tje')]).

n([pl(daden),sg(daad)],de,
  [subject_sbar,
   subject_vp],
  [terreur]).

n([sg(daadkracht)],de,[]).

n([pl(daagjes),pl(dagjes),sg(daagje)],het,[measure]).

n([pl(daalders),sg(daalder)],de,[measure],[dim(daaldertje)]).

n([pl(dadels),sg(dadel)],de,[],[dim(dadeltje)]).

n([pl(daders),sg(dader)],de,[]).

n([sg(dag),pl(dagen)],de,
  [measure,
   temp_mod,
   sbar],
  [wh(['1000',soorten]),
   beurs,
   s(bevrijding),
   feest,
   geboorte,
   s(handel),
   hoogtij,
   s(huwelijk),
   kerst,i(kerst,'Kerst'),
   land,
   s(leven),
   moeder,
   s(nieuwjaar),
   s(oudejaar),
   s(oudjaar),
   s(opening),
   paas,i(paas,'Paas'),
   pinkster,i(pinkster,'Pinkster'),
   i(prins_DIM,prinsjes),
   s(publiek),
   s(rijk),
   rust,
   school,
   slot,
   speel,
   sterf,
   studie,
   trouw,
   vader,
   vakantie,
   verjaar,
   s(verkiezing),
   wedstrijd,
   werk,
   zomer,
   dim(dagje)]).

n([mass(dagbladpers)],de,[]).

n([pl(dagdromen),sg(dagdroom)],de,[]).

n([mass(dageraad)],de,[]).

n([mass(dagindeling)],de,[]).

n([pl([dagjes,uit]),sg([dagje,uit])],het,[]).

n([sg(dagpauwoog),pl(dagpauwogen)],de,[]).

n([mass(dagtaak)],de,[]).

n([pl(dagvaardingen),sg(dagvaarding)],de,[]).

n([pl(dagverblijven),sg(dagverblijf)],het,[]).

n([pl(daken),sg(dak)],het,[],
  [zadel,
   dim(dakje)]).

n([pl(['dak-',en,thuislozen])],de,[]).

n([pl(dakgoten),sg(dakgoot)],de,[]).

n([pl(dakpannen),sg(dakpan)],de,[]).

n([pl(dakramen),sg(dakraam)],het,[]).

n([pl(dakterrassen),sg(dakterras)],het,[]).

n([sg([dalai,lama])],de,[]).

n([pl(dalen),sg(dal)],het,[]).

n([sg(daler),pl(dalers)],de,[]).

n([pl(dalingen),sg(daling)],de,[],
  [bodem,
   koers,
   omzet,
   prijs,
   rente,
   temperatuur,
   waarde,
   winst]).

n([sg(dalit),pl(dalits)],de,[]).  % kastelozen/onaanraakbaren in India etc

n([pl(dammen),sg(dam)],de,[],[dim(dammetje)]).

%% het damspel:
n([mass(dammen)],het,[]).

n([pl(dames),sg(dame)],de,[],[dim(dametje)]).

n([pl([dames,dubbelspelen]),sg([dames,dubbelspel])],het,[]).

n([pl([dames,enkelspelen]),sg([dames,enkelspel])],het,[]).

n([pl(dampen),sg(damp)],de,[]).

n([pl(dampkringen),sg(dampkring)],de,[]).

n([mass(dance)],de,[]).

n([pl(dancings),sg(dancing)],de,[]).

n([pl('dandy\'s'),sg(dandy)],de,[]).

n([mass(dank)],de,[sbar,subject_sbar]).

n([mass(dankbaarheid)],de,[]).

n([mass(dankjewel)],both,[]).

n([mass(dankuwel)],both,[]).

n([pl(dansen),sg(dans)],de,[],
  [dim(dansje)]).

n([pl(dansers),sg(danser)],de,[]).

n([pl(danseressen),sg(danseres)],de,[],[dim(danseresje)]).

n([pl(dansvloeren),sg(dansvloer)],de,[]).

n([mass(dapperheid)],de,[]).

n([sg(dar),pl(darren)],de,[]).

n([sg([dark,room]),pl([dark,rooms])],de,[]).

n([pl(darmen),sg(darm)],de,[],
  [i(blind,blinde),
   endel,
   dim(darmpje)]).

n([sg(dart),pl(darts)],de,[]).

n([mass(darts)],het,[]).

n([sg(darter),pl(darters)],de,[]).

n([pl(dassen),sg(das)],de,[]).

n([pl(dashboarden),pl(dashboards),sg(dashboard)],het,[]).

n([sg(daver)],de,[]). % Vlaams: met de daver op het lijf
                      % iemand de daver op het lijf jagen

n([sg(databank),pl(databanken)],de,[],[schaak]).

n([sg(database),pl(databases)],de,[]).

n([sg(date)],de,[]).

n([pl(dateringen),sg(datering)],de,[]).

n([pl(datjes),sg(datje)],het,[]).

%% informatie
n([sg(data),pl(data)],de,[]).

%% tijdstip
n([pl(data),pl(datums),sg(datum)],de,[temp_mod,sbar]).

%% tijdsip
n([pl(data),pl(datums),sg(datum),
   ignore_stem(datum)],de,[],
  [geboorte,
   s(houdbaarheid),
   peil,
   reis,
   vertrek,
   verval]).

n([sg(datsja),pl('datsja\'s')],de,[]).

n([mass(dauw)],de,[]).

n([sg(deadhead),pl(deadheads)],de,[]).

n([pl(deadlines),sg(deadline)],de,[]).

n([pl(deals),sg(deal)],de,[sbar,subject_sbar],[]).

n([pl(dealers),sg(dealer)],de,[],
  [drug,s(drug)]).

n([sg([dealing,room]),pl([dealing,rooms])],de,[]).

n([pl(debatten),sg(debat)],het,[],
  [i(actualiteit,actualiteiten),
   integratie,
   h('IRT'),
   kamer,
   i(kamer,'Kamer'),
   i(lijsttrekker,lijsttrekkers),
   s(raad),
   spoed]).

n([sg(debiel),pl(debielen)],de,[]).

n([pl(debiteuren),pl(debiteurs),sg(debiteur)],de,[]).

n([pl(debutanten),sg(debutant)],de,[]).

n([pl(debuten),sg(debuut)],het,
  [start_app_measure,
   np_app_measure
  ],
  [film,
   regie,
   i(speel_film,speelfilm),
   thriller,
   toneel]).

n([stem(debacle),
   pl(debâcles),sg(debâcle),pl(debacles),sg(debacle)],both,[]).

n([pl(decanen),sg(decaan)],de,[]).

n([mass(decadentie)],de,[]).

n([sg(december)],de,[temp_mod,sbar]).

n([sg(decennium),pl(decennia),pl(decenniën)],het,[temp_mod,sbar]).

n([pl(decentralisaties),sg(decentralisatie),
   pl(decentralizaties),sg(decentralizatie)],de,[]).

n([sg(deceptie),pl(decepties)],de,[]).

n([meas(decibel),pl(decibels),pl(decibellen)],de,[meas_mod,measure]).

n([sg(decimaal),pl(decimalen)],de,[]).

n([pl(declaraties),sg(declaratie)],de,[]).

n([sg(declinatie),pl(declinaties)],de,[]).

n([sg(decoder),pl(decoders)],de,[]).

n([pl(decolletés),
   pl('decolleté\'s'),
   sg(decolleté)],het,[]).

n([pl(deconcentraties),sg(deconcentratie)],de,[]).

n([pl(deconfitures),sg(deconfiture)],de,[]).

n([pl(decors),sg(decor)],het,[]).

n([pl(decoraties),sg(decoratie)],de,[]).

n([mass(decoratief)],het,[]).

n([pl(decreten),sg(decreet),
   pl(dekreten),sg(dekreet)],het,[]).

n([mass(dédain)],both,[]).

n([pl(deducties),sg(deductie)],de,[sbar]).

n([pl(degen),sg(deeg)],het,[]).

n([sg([déjà,vu]),
   sg([déja,vu])],both,[]).

n([pl(delen),sg(deel),sg('dl.'), pl('dln.')],het,[measure,mod],
  [dim(deeltje)]).

n([pl(delen),sg(deel),ignore_stem(deel)],het,[],
  [i(boeken,boek),
   boek,
   bouw,
   eiland,
   s(gebied),                   % overzeese
   stof,
   dim(deeltje)]).

n([sg(deel)],de,[]).  % boerderij

n([mass(deelgenootschap)],het,[]).

n([mass(deelname),pl(deelnames)],de,[],
  [s(arbeid),
   s(regering)]).

n([pl(deelneemsters),sg(deelneemster)],de,[]).

n([pl(deelnemers),sg(deelnemer)],de,[],[markt]).

n([mass(deelneming),pl(deelnemingen),sg(deelneming)],de,[]).

n([pl(deelstaten),sg(deelstaat)],de,[]).

n([mass(deeltijd)],de,[]).

n([sg(deeltijder),pl(deeltijders)],de,[]).

n([mass(deemoed)],de,[]).

n([mass(deernis)],de,[]).

n([mass(deet)],de,[]).

n([pl(defecten),sg(defect)],het,[]).

n([mass(defensie)],de,[]).

n([pl(defensieven),sg(defensief)],het,[]).

n([pl(definities),sg(definitie)],de,[sbar]).

n([sg(deflatie),pl(deflaties)],de,[]).

n([mass(degelijkheid)],de,[]).

n([pl(degens),sg(degen)],de,[],[dim(degentje)]).

n([mass(degeneratie)],de,[]).

n([pl(degradaties),sg(degradatie)],de,[measure]).

n([pl(deiningen),sg(deining)],de,[]).

n([pl(dekken),sg(dek)],het,[],
  [parkeer]).

n([pl(dekanen),sg(dekaan)],de,[]).

n([mass(dekadentie)],de,[]).

n([sg(dekbedovertrek),pl(dekbedovertrekken)],het,[]).

n([pl(dekens),sg(deken)],de,[],
  [blus,
   i(lap,lappen),
   dim(dekentje)]).

n([pl(dekkers),sg(dekker)],de,[],
  [dak,
   man]).

n([pl(dekkingen),sg(dekking)],de,[]).

n([pl(dekmantels),sg(dekmantel)],de,[sbar]).

n([sg(dekolonisatie),pl(dekolonisaties)],de,[]).

n([pl(deksels),sg(deksel)],both,[],
  [koffer,
   dim(dekseltje)]).

n([pl(delegaties),sg(delegatie)],de,[measure]).
%% een delegatie architecten

n([pl(delicatessen),sg(delicatesse)],de,[]).

n([pl(delicten),sg(delict)],het,[],[s(geweld)]).

n([pl(delikatessen),sg(delikatesse)],de,[]).

n([pl(delingen),sg(deling)],de,[],
  [cel,
   i(punt,punten),
   winst,
   dim(delinkje)]).

n([pl(delinquenten),sg(delinquent),
   pl(delinkwenten),sg(delinkwent)],de,[],[i(zede,zeden)]).

n([pl(deliriums),sg(delirium)],het,[]).

n([sg([delirium,tremens])],het,[]).

n([pl('delta\'s'),sg(delta)],de,[]).

n([mass(demagogie)],de,[]).

n([sg(demarrage),pl(demarrages)],de,[]).

n([mass(dementie)],de,[]).

n([pl('demo\'s'),sg(demo)],de,[]).

n([pl(democraten),sg(democraat)],de,[]).

n([pl(democratieën),sg(democratie)],de,[],
  [h(christen)]).

n([pl(democratiseringen),sg(democratisering)],de,[]).

n([mass(demografie)],de,[]).

n([pl(demonen),pl(demons),sg(demon)],de,[]).

n([mass(demonisering)],de,[]). % van Pim zeker

n([pl(demonstranten),sg(demonstrant)],de,[]).

n([pl(demonstraties),sg(demonstratie)],de,[sbar,measure]).

n([mass(demotie)],de,[]).

n([pl(dempingen),sg(demping)],de,[]).

n([pl(dennen),sg(den)],de,[],[dim(dennetje)]).

n([mass(denim)],both,[]).

n([pl(denkbeelden),sg(denkbeeld)],het,[sbar,vp]).

n([pl(denkers),sg(denker)],de,[]).

n([sg(denktank),pl(denktanks)],de,[]).

n([pl(denkwijzen),sg(denkwijze)],de,[sbar]).

n([pl(dennebomen),sg(denneboom)],de,[]).

n([pl(departementen),sg(departement)],het,[app_measure]).

n([pl(dependances),sg(dependance)],de,[]).

n([pl(deportaties),sg(deportatie)],de,[]).

n([sg(deposito),pl('deposito\'s')],het,[]).

n([pl(depots),sg(depot)],both,[],[dim(depootje)]).

n([pl(depressies),sg(depressie)],de,[]).

n([pl(deprivaties),sg(deprivatie)],de,[]).

n([pl(deputaties),sg(deputatie)],de,[]).

n([sg(derailleur),pl(derailleurs)],de,[]).

n([sg(derby),pl('derby\'s')],de,[],[s(stad)]).

n([sg([derde,kerstdag]),
   sg(['Derde','Kerstdag'])],de,[temp_mod,sbar]).

n([sg([derde,paasdag]),
   sg(['Derde','Paasdag'])],de,[temp_mod,sbar]).

n([sg([derde,pinksterdag]),
   sg(['Derde','Pinksterdag'])],de,[temp_mod,sbar]).

n([pl(derden),pl(derdes)],both,[]).

n([mass(deregulering)],de,[]).

n([sg(derivaat),pl(derivaten)],het,[]).

n([sg(dermatoloog),pl(dermatologen)],de,[]).

n([pl(dertigers),sg(dertiger)],de,[]).

n([sg(derving),pl(dervingen)],de,[],
  [i(inkomst,inkomsten),
   winst
  ]).

n([pl(deserteurs),sg(deserteur)],de,[]).

n([sg(desideratum),pl(desiderata)],both,[]).

n([pl(designs),sg(design)],de,[]).

n([mass(design)],het,[]).

n([pl(designers),sg(designer)],de,[]).

n([pl(desillusies),sg(desillusie)],de,[]).

n([pl(desilluzies),sg(desilluzie)],de,[]).

n([pl(desintegraties),sg(desintegratie)],de,[]).

n([pl(desinteresses),sg(desinteresse)],de,[]).

n([stem(deskundig),
   sg(deskundige),pl(deskundigen)],de,[],
  [s(arbeid),
   s(belegging),
   s(ervaring)]).

n([pl(deskundigheden),sg(deskundigheid)],de,[]).

n([pl(despoten),sg(despoot)],de,[]).

n([pl(desserten),pl(desserts),sg(dessert)],het,[]).

n([pl(detachementen),sg(detachement)],het,[measure]).

n([sg(detachering),pl(detacheringen)],de,[]).

n([pl(details),sg(detail)],het,[sbar]).

n([pl(detailleringen),sg(detaillering)],de,[]).

n([pl(detaillisten),sg(detaillist)],de,[]).

n([pl(detectives),sg(detective)],de,[],
  [privé]).

n([sg(detector),pl(detectoren)],de,[],
  [leugen]).

n([mass(detentie)],de,[],[jeugd]).

n([pl(determinanten),sg(determinant)],de,[]).

n([mass(determinisme)],het,[]).

n([pl(deugden),sg(deugd)],de,[]).

n([mass(deugdelijkheid)],de,[]).

n([mass(deugdzaamheid)],de,[]).

n([pl(deugnieten),sg(deugniet)],de,[]).

n([pl(deuken),sg(deuk)],de,[],[dim(deukje)]).

n([pl(deunen),sg(deun)],de,[],[dim(deuntje)]).

n([pl(deuren),sg(deur)],de,[],
  [achter,
   i(bad_kamer,badkamer),
   balkon,
   buiten,
   cel,
   draai,
   hor,
   huis,
   kamer,
   kast,
   kelder,
   keuken,
   lift,
   schuif,
   i(slaap_kamer,slaapkamer),
   tuin,
   tussen,
   voor,
   zij,
   dim(deurtje)]).

n([pl(deurknoppen),sg(deurknop)],de,[]).

n([pl(deuropeningen),sg(deuropening)],de,[]).

n([pl(deurposten),sg(deurpost)],de,[]).

n([pl(deurwaarders),sg(deurwaarder)],de,[]).

n([pl(devaluaties),sg(devaluatie)],de,[]).

n([pl(deviezen),sg(devies)],het,
  [sbar,
   subject_sbar,
   np_app_measure,
   start_app_measure]).

n([pl(devoties),sg(devotie)],de,[]).

n([pl('dia\'s'),sg(dia)],de,[]).

n([mass(diabetes)],de,[]).

n([mass([diabetes,insipidus])],de,[]).

n([mass([diabetes,mellitus])],de,[]).

n([pl(diagnosen),pl(diagnoses),sg(diagnose)],de,[sbar,app_measure]).

n([mass(diagnostiek)],de,[]).

n([sg(diagonaal),pl(diagonalen)],de,[]).

n([pl(diagrammen),sg(diagram)],het,[],[dim(diagrammetje)]).

n([pl(dialecten),sg(dialect),pl(dialekten),sg(dialekt)],het,[]).

n([mass(dialectiek),mass(dialektiek)],de,[]).

n([pl(dialogen),sg(dialoog)],de,[]).

n([pl(diamanten),sg(diamant)],de,[]).

n([sg(diamantair),pl(diamantairs)],de,[]).

n([mass(diamant)],het,[]).

n([pl(diameters),sg(diameter)],de,[]).

n([mass(diarree)],de,[]).

n([pl(dichters),sg(dichter)],de,[]).

n([sg(['Dichter',des,'Vaderlands']),pl(['Dichters',des,'Vaderlands'])],de,[]).

n([pl(dichteressen),sg(dichteres)],de,[]).

n([mass(dichterschap)],het,[]).

n([mass(dichtheid)],de,[],
  [s(bevolking),
   kijk]).

n([sg(dictaat),pl(dictaten)],het,[]).

n([pl(dictatoren),pl(dictators),sg(dictator)],de,[],
  [h(ex)]).

n([pl(dictaturen),sg(dictatuur)],de,[]).

n([sg(dictee),pl(dictees)],het,[]).

n([mass(didactiek),
   mass(didaktiek)],de,[]).

n([sg([die,hard]),pl([die,hards])],de,[]).

n([pl(diëten),sg(dieet)],het,[]).

n([pl(dieven),sg(dief)],de,[],
  [auto,
   benzine,
   kruimel,
   winkel]).

n([pl(diefstallen),sg(diefstal)],de,[],
  [auto,
   benzine,
   kruimel,
   winkel]).

n([pl(dienaars),pl(dienaren),sg(dienaar)],de,[]).

n([pl(dienaressen),sg(dienares)],de,[]).

n([pl(dienders),sg(diender)],de,[]).

n([pl(diensten),sg(dienst)],de,
  [pred_pp(in),
   pred_pp(van),  % we zijn u graag van dienst
   app_measure],
  [s(overheid),
   s(rijk)]).

n([pl(diensten),sg(dienst)],de,
  [pred_pp(in)],
  [loon,
   i(ploegen,ploeg)
  ]).

n([pl(diensten),sg(dienst)],de,[],
  [arbo,
   belasting,
   s(gezondheid),
   hulp,
   immigratie,
   i(inlichting,inlichtingen),
   internet,
   s(keuring),
   lijn,
   milieu,
   nacht,
   nieuws,
   s(opsporing),
   orde,
   politie,
   post,
   s(reiniging),
   streaming,
   taxi,
   uitvaart,
   veer,
   s(veiligheid),
   s(voorlichting),
   i(vreemdeling,vreemdelingen)]).

n([pl(diensten),sg(dienst)],de,[],
  [ere,
   kerk,
   weder
  ]).

n([mass(dienstbaarheid)],de,[]).

n([pl(dienstboden),pl(dienstbodes),sg(dienstbode)],de,[]).

n([pl(diensters),sg(dienster)],de,[],[dim(dienstertje)]).

n([pl(dienstmaagden),sg(dienstmaagd)],de,[]).

n([pl(dienstmeiden),sg(dienstmeid)],de,[]).

n([pl(dienstmeisjes),sg(dienstmeisje)],het,[]).

n([mass(dienstverband),pl(dienstverbanden),sg(dienstverband)],het,[]).

n([pl(dienstweigeringen),sg(dienstweigering)],de,[]).

n([pl(diepen),sg(diep)],het,[]).

n([mass(diepgang)],de,[]).

n([mass(diepst)],het,[]).  % in het diepst van hun ziel

n([pl(diepten),pl(dieptes),sg(diepte)],de,[]).

n([sg(diepvries)],de,[]).

n([pl(dieren),sg(dier)],het,[],
  [s(gezelschap),
   hobby,
   land,
   prooi,
   schelp,
   water,
   i(zoet_water,zoetwater),
   i(zout_water,zoutwater),
   week,
   zee,
   dim(diertje)]).

n([mass(dierenriem)],de,[]).

n([mass(dierenrijk)],het,[]).

n([pl(dierentuinen),sg(dierentuin)],de,[]).

n([pl(diergaarden),pl(diergaardes),sg(diergaarde)],de,[]).

n([mass(diesel)],de,[],[turbo]).

n([pl(differentiaties),sg(differentiatie)],de,[]).

n([sg(differentieel),pl(differentieels)],both,[]).

n([sg(diffuser),pl(diffusers)],de,[]). %% een ding voor je haren

n([pl(diggelen),pl(diggels),sg(diggel)],de,[]).

n([pl(dijen),sg(dij)],de,[]).

n([pl(dijken),sg(dijk)],de,[],
  [haven,
   kanaal,
   nood,
   rivier,
   spoor,
   zee,
   dim(dijkje)]).

n([pl(dijkdoorbraken),sg(dijkdoorbraak)],de,[]).

n([pl(dijkgraven),sg(dijkgraaf)],de,[]).

n([pl(dikkerds),sg(dikkerd)],de,[]).

n([pl(diktators),sg(diktator)],de,[]).

n([pl(diktaturen),sg(diktatuur)],de,[]).

n([pl(dikten),pl(diktes),sg(dikte)],de,[]).

n([pl('dilemma\'s'),sg(dilemma)],het,[subject_sbar]).

n([mass(dille)],de,[]).

n([mass([dim,sum])],de,[]).

n([pl(dimensies),sg(dimensie)],de,[]).

n([meas(dinar),pl(dinars)],de,[meas_mod,measure]).

n([pl(diners),sg(diner)],het,[],
  [kerst,
   dim(dineetje),
   dim(dinertje)]).             % frans

n([sg(diner)],de,[]).           % engels

n([sg([diner,dansant]),pl([diners,dansants])],het,[]).

n([pl(dingen),sg(ding)],het,
  [sbar,
   subject_sbar  % dat te herkennen is één ding
  ],
  [dim(dingetje)]).

n([sg(dinges),pl(dinges),sg(dinge),pl(dinge)],both,[]).

n([pl('dingo\'s'),sg(dingo)],de,[]).

n([sg(dinosauriër),pl(dinosauriërs)],de,[]).

n([sg(dinosaurus),pl(dinosaurussen)],de,[]).

n([sg(dinsdag),pl(dinsdagen)],de,[temp_mod,sbar]).

n([sg(dioxine),pl(dioxines)],de,[]).

n([sg(dip),pl(dippen)],de,[]).

n([pl('diploma\'s'),sg(diploma)],het,
  [app_measure],
  [dim(diplomaatje)]).

n([pl('diploma\'s'),sg(diploma)],het,[],
  [zwem,
   dim(diploomatje)]).

n([pl(diplomaten),sg(diplomaat)],de,[],
  [dim(diplomaatje)]).

n([mass(diplomatie)],de,[]).

n([mass(diplomatiek)],de,[]).

n([sg(diplomering)],de,[]).

n([pl(directeuren),pl(directeurs),sg(directeur)],de,[app_measure],
  [h(adjunct),
   bank,
   h(interim),
   koers,
   museum,
   muziek,
   onder,
   h(oud),
   h(president),
   programma,
   school,
   sport,
   toernooi
  ]).

n([pl('directeuren-generaal'),pl('directeurs-generaal'),
   sg('directeur-generaal'),sg([directeur,generaal]),pl([directeuren,generaal])],de,[app_measure]).

n([mass(directheid)],de,[]).

n([pl(directies),sg(directie)],de,[app_measure],[hoofd,
						 h('NS')]).

n([sg(directief),pl(directieven)],het,[]).

n([pl(directoraten),sg(directoraat)],het,[app_measure]).

n([pl('directoraten-generaal'),sg('directoraat-generaal')],het,[app_measure]).

n([pl(directrices),sg(directrice)],de,[app_measure]).

n([pl(dirigenten),sg(dirigent)],de,[],
  [h(chef),chef,
   gast]).

n([sg(dis),pl(dissen)],de,[]).

n([pl(discipelen),pl(discipels),sg(discipel)],de,[]).

n([pl(disciplines),sg(discipline)],de,[app_measure]).

n([mass(discipline)],de,[],
  [s(begroting)]).

n([sg(disfunctie),pl(disfuncties)],de,[]).

n([sg(discjockey),pl(discjockeys)],de,[]).

n([pl('disco\'s'),sg(disco)],de,[]).

n([sg(discografie),pl(discografieën)],de,[]).

n([sg(disconto)],het,[]).

n([pl(discotheken),sg(discotheek)],de,[np_app_measure]).

n([sg(discount),pl(discounts)],both,[]).

n([pl(discoursen),sg(discours)],het,[]).

n([mass(discrediet)],het,[pred_pp(in)]).

n([pl(discrepanties),sg(discrepantie)],de,[]).

n([mass(discretie)],de,[]).

n([pl(discriminaties),sg(discriminatie)],de,[],[s(leeftijd)]).

n([pl(discussies),sg(discussie)],de,
  [sbar],
  [noodzaak]).

n([mass(disharmonie)],de,[]).

n([pl(diskettes),sg(diskette)],de,[]).

n([pl('disko\'s'),sg(disko)],de,[]).

n([mass(diskrediet)],het,[]).

n([mass(diskretie)],de,[]).

n([pl(diskussies),sg(diskussie)],de,[]).

n([sg(diskwalificatie),pl(diskwalificaties)],de,[]).

n([pl(dispensaties),sg(dispensatie)],de,[]).

n([sg(dispersie),pl(dispersies)],de,[]).

n([pl(disposities),sg(dispositie)],de,[vp]).

n([pl(disputen),sg(dispuut)],het,[]).

n([pl(dissertaties),sg(dissertatie)],de,[]).

n([pl(dissidenten),sg(dissident)],de,[]).

n([pl(dissonanties),sg(dissonantie)],de,[]).

n([pl(distanties),sg(distantie)],de,[]).

n([pl(distels),sg(distel)],de,[],[dim(disteltje)]).

n([sg(distributeur),pl(distributeurs)],de,[]).

n([pl(distributies),sg(distributie)],de,[]).

n([pl(distributiecentra),sg(distributiecentrum)],het,[]).

n([pl(districten),sg(district),pl(distrikten),sg(distrikt)],het,
  [app_measure],
  [kies,
   polder,
   wijn]).

n([sg([district,attorney])],de,[]).

n([pl([ditjes,en,datjes])],de,[]).

n([sg(diva),pl('diva\'s')],de,[]).

n([pl(divans),sg(divan)],de,[]).

n([sg(diversificatie),pl(diversivicaties)],de,[]).

n([mass(diversiteit)],de,[],[bio]).

n([pl(dividenden),sg(dividend)],het,[],[interim]).

n([pl(divisies),pl(divisiën),sg(divisie)],de,[measure]).

n([pl(divisies),pl(divisiën),sg(divisie)],de,[],[keuken]).

n([pl('dj\'s'),sg(dj),
   sg(deejay),pl(deejays),
   sg(diskjockey),pl(diskjockeys)],de,[]).

n([mass([djeroek,poeroet])],both,[]).  % citroenbladeren

n([mass('DNA'),
   mass('dna')],het,[]).

n([pl('do\'s'),sg(do)],de,[]).

n([sg(dobbel),pl(dobbels)],de,[]).

n([pl(dobbelstenen),sg(dobbelsteen)],de,[]).

n([pl(dobbers),sg(dobber)],de,[],[dim(dobbertje)]).

n([pl(docenten),sg(docent)],de,
  [app_measure],
  [gast,
   hoofd]
 ).

n([pl(docentes),sg(docente)],de,[app_measure],[gast]).

n([pl(dochteren),pl(dochters),sg(dochter)],de,[],
  [internet,
   s(koning),
   schoon,
   dim(dochtertje)]).

n([sg([docking,station]),pl([docking,stations])],het,[]).

n([pl(doctoren),pl(doctors),sg(doctor)],de,[]).

n([sg(doctoraat),pl(doctoraten)],het,[],[ere]).

n([pl(doctorandi),pl(doctorandussen),sg(doctorandus)],de,[]).

n([pl(doctrines),sg(doctrine)],de,[sbar]).

n([pl(documenten),sg(document),
   pl(dokumenten),sg(dokument)],het,[np_app_measure]).

n([pl(documenten),sg(document),
   pl(dokumenten),sg(dokument)],het,[],
  [reis,
   slot,
   tekst,
   f([tekst])]).

n([pl(documentaires),sg(documentaire)],de,[np_app_measure]).

n([mass(documentatie)],de,[]).

n([sg(dode),pl(doden),stem(dood),ignore_stem(dood)],de,[],
  [burger,
   s(verkeer)]
 ).

n([pl(dodenherdenkingen),sg(dodenherdenking)],de,[]).

n([mass('doe-het-zelf')],de,[]).

n([pl(doeanen),sg(doeane)],de,[]).

n([pl(doeanebeambten),sg(doeanebeambte)],de,[]).

n([pl(doeaniers),sg(doeanier)],de,[]).

n([sg(doejong),pl(doejongs)],de,[]).

n([pl(doeken),sg(doek)],both,[],
  [dim(doekje)]).

n([pl(doeken),sg(doek),ignore(m(doek,noun(de,count,sg),doek))],de,[],
  [hoofd,
   dim(doekje)]).

n([pl(doelen),sg(doel)],het,
  [sbar,
   subject_sbar,
   subject_vp,
   vp],
  [eind,
   hoofd,
   kern,
   koers,
   neven,
   s(ontwikkeling),
   reis,
   streef]).

n([pl(doelen),sg(doel)],het,[],
  [burger]).

n([pl(doeleinden),sg(doeleinde)],het,[sbar,vp]).

n([pl(doellieden),pl(doelmannen),sg(doelman)],de,[],
  [reserve]).

n([mass(doelmatigheid)],de,[]).

n([pl(doelpunten),sg(doelpunt)],het,[measure,meas_mod]).

n([pl(doelstellingen),sg(doelstelling)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp],
  [hoofd,
   millennium,
   s(ontwikkeling)
  ]).

n([sg(doeltrap),pl(doeltrappen)],de,[]).

n([mass(doeltreffendheid)],de,[]).

n([sg(doelwit),pl(doelwitten)],het,[]).

n([mass(doem)],de,[]).

n([sg(doema)],de,[],[s(staat)]).

n([pl(doezen),sg(does)],de,[]).

n([pl(doffen),sg(dof)],de,[]).

n([sg(doffer),pl(doffers)],de,[]).

n([pl(doggen),sg(dog)],de,[]).

n([pl('dogma\'s'),pl(dogmata),sg(dogma)],het,[sbar]).

n([mass(dogmatiek)],de,[]).

n([pl(dokken),sg(dok)],het,[]).

n([pl(dokters),pl(doktoren),sg(dokter)],de,[],
  [huis,
   i(kruid,kruiden)]).

n([pl(dokumenten),sg(dokument)],het,[]).

n([pl(dokumentaires),sg(dokumentaire)],de,[]).

n([mass(dokumentatie)],de,[]).

n([pl(dokwerkers),sg(dokwerker)],de,[]).

n([sg(dol),pl(dollen)],de,[]).

n([pl(dolfijnen),sg(dolfijn)],de,[],[dim(dolfijntje)]).

n([pl(dolken),sg(dolk)],de,[]).

n([meas(dollar),pl(dollars)],de,[meas_mod,measure],
  [f(['US'])]).

n([sg(dolmen),pl(dolmens)],de,[]).

n([mass(dom)],de,[]).

n([pl(domeinen),sg(domein)],het,[],
  [s(beleid),
   kroon,'Kroon'
  ]).

n([pl(domheden),sg(domheid)],de,[]).

n([sg(dominant),pl(dominanten)],de,[]).

n([pl(dominanties),sg(dominantie)],de,[]).

n([pl(dominees),sg(dominee)],de,[]).

n([pl(dominicanen),sg(dominicaan)],de,[]).

n([pl(dominikanen),sg(dominikaan)],de,[]).

n([pl(domkoppen),sg(domkop)],de,[]).

%% De lezer moet opschrikken uit zijn zelfgenoegzame dommel .
n([sg(dommel)],de,[]).

n([pl(domoren),sg(domoor)],de,[]).

n([mass(domotica)],de,[]).

n([pl(dompers),sg(domper)],de,[],[dim(dompertje)]).

n([pl(dons),sg(don)],de,[]).

n([pl(donateurs),sg(donateur)],de,[]).

n([sg(donatie),pl(donaties)],de,[],[orgaan]).

n([sg(donderdag),pl(donderdagen)],de,[temp_mod,sbar]).

n([pl(donders),sg(donder)],de,[]).

n([pl(donderslagen),sg(donderslag)],de,[]).

n([sg(dong)],de,[meas_mod,measure]).  % vietnamese munteenheid

%% dongles =/= dong_les
n([sg(dongle),pl(dongles)],de,[]).

n([mass(donker)],het,[],[aarde]).

n([mass(donkerte)],de,[]).

n([pl('donna\'s'),sg(donna)],de,[]).

n([pl(donors),sg(donor),pl(donoren)],de,[],
  [orgaan]).

n([mass(dons)],het,[]).

n([sg(dodental),pl(dodentallen)],het,[]).

n([sg(dood)],de,[],
  [hersen,
   honger]).

n([sg(dooddoener),pl(dooddoeners)],de,[sbar,subject_sbar,vp,subject_vp]).

n([pl(doodgravers),sg(doodgraver)],de,[]).

n([pl(doodkisten),sg(doodkist)],de,[]).

n([pl(doodsangsten),sg(doodsangst)],de,[]).

n([pl(doodshoofden),sg(doodshoofd)],het,[]).

n([pl(doodskisten),sg(doodskist)],de,[]).

n([pl(doodskoppen),sg(doodskop)],de,[]).

n([pl(doodslagen),sg(doodslag)],de,[]).

n([mass(doodsnood)],de,[]).

n([mass(doodsstrijd)],de,[]).

n([pl(doodsteken),sg(doodsteek)],de,[]).

n([stem(dood_vonnis),
   pl(doodsvonnissen),pl(doodvonnissen),
   sg(doodvonnis),sg(doodsvonnis)],het,[]).

n([pl(doodzonden),sg(doodzonde)],de,[subject_sbar,subject_vp]).

n([mass(doofheid)],de,[]).

n([pl(doofpotten),sg(doofpot)],de,[]).

n([sg(dooi)],de,[]).

n([pl(dooiers),sg(dooier)],de,[],[dim(dooiertje)]).

n([mass(dool)],de,[pred_pp(op)]). % Vlaams

n([pl(doolhoven),sg(doolhof)],both,[]).

n([pl(dopen),sg(doop)],de,[]).

n([pl(doopsels),sg(doopsel)],het,[]).

n([sg(doopvont),pl(doopvonten)],de,[]).

n([mass(doorbloeding)],de,[]).

n([pl(doorbraken),sg(doorbraak)],de,[subject_sbar]).

n([mass(doordraai)],de,[]).

n([sg(doordruk),pl(doordrukken)],de,[]).

n([pl(doorgangen),sg(doorgang)],de,[]).

n([mass(doorgifte)],de,[]).

n([sg(doorgroei)],de,[]).

n([mass(doorkijk)],de,[]).

n([sg(doorlaat),pl(doorlaten)],de,[]).

n([sg(doorlichting),pl(doorlichtingen)],de,[]).

n([mass(doorloop)],de,[]).

n([pl(doornen),pl(doorns),sg(doorn)],de,[],
  [gaspel,
   mei,
   dim(doorntje)]).

n([pl(doornstruiken),sg(doornstruik)],de,[]).

n([sg(doorpas),pl(doorpassen)],de,[]).  % iets met modeshows

n([pl(doorreizen),sg(doorreis)],de,[]).

n([pl(doorslagen),sg(doorslag)],de,[]).

n([sg(doorsnee),pl(doorsneden),sg(doorsnede),pl(doorsneeën)],de,[measure]).

n([sg(doorstap),pl(doorstappen)],de,[]).

n([sg(doorstart),pl(doorstarten)],de,[]).

n([sg(doorsteek),pl(doorsteken)],de,[]).

n([sg(doorstoot),pl(doorstoten)],de,[]).

n([mass(doorstroming)],de,[]).

n([mass(doorstroom)],de,[]).

n([mass(doortrek)],de,[pred_pp(op)]).  % de vogels zijn op doortrek

n([mass(doorvaart)],de,[]).

n([mass(doorverkoop)],de,[]).

n([pl(doorvoeren),sg(doorvoer)],de,[]).

n([mass(doorwerking)],de,[]).

n([pl(dozen),sg(doos)],de,[measure],
  [s(lucifer),
   i(schoen,schoenen),
   dim(doosje)]).

n([pl(doppen),sg(dop)],de,[],
  [i(noot,noten),
   oor,
   dim(dopje)]).

n([pl(dopes),sg(dope)],de,[]).

n([pl(dopers),sg(doper)],de,[]).

n([pl(doperwten),sg(doperwt)],de,[],[dim(doperwtje)]).

n([mass(doping)],de,[],
  [bloed]).

n([pl(dorpen),sg(dorp)],het,[],
  [berg,
   i(boer,boeren),
   geboorte,
   dim(dorpje)]).

n([pl(dorpelingen),sg(dorpeling)],de,[]).

n([pl(dorpspleinen),sg(dorpsplein)],het,[]).

n([mass(dorst)],de,[]).

n([sg(dos),pl(dossen)],de,[]).  % haardos

n([pl(doseringen),sg(dosering)],de,[measure]).

n([pl(doses),pl(dosissen),sg(dosis)],de,[measure],
  [s(aanvang)]).

n([pl(dossiers),sg(dossier)],het,[app_measure],
  [i(cliënt,cliënten),
   s(onderzoek),
   straf]).

n([pl(dotten),sg(dot)],de,[measure]).

n([sg(dotatie),pl(dotaties)],de,[]).

n([pl(douanen),sg(douane)],de,[]).

n([pl(douanebeambten),sg(douanebeambte)],de,[]).

n([pl(douaniers),sg(douanier)],de,[]).

n([sg([double,bogey]),pl([double,bogeys])],de,[]).

n([sg(doublet),pl(doubletten)],het,[]).

n([sg(doubleton),pl(doubletons)],de,[measure]). % bridge

n([pl(douches),sg(douche)],de,[]).

n([sg(douw),pl(douwen)],de,[]).

n([pl(dovemansoren)],het,[]).

n([sg(download),pl(downloads)],de,[]).

n([meas(dozijn),pl(dozijnen)],het,[meas_mod,measure],
  [dim(dozijntje)]).

n([pl('doña\'s'),sg(doña)],de,[]).

n([sg(draad),pl(draden)],de,[measure],
  [dim(draadje)]).   % een draadje saffraan

n([sg(draad),pl(draden)],de,[],
  [prikkel, % Vlaams
   dim(draadje)]).

n([mass(draad)],het,[],
  [prikkel,
   staal]).

n([sg(draadloos)],het,[]).

n([sg(draagbaar),pl(draagbaars)],de,[]).

n([mass(draagmoederschap)],het,[]).

n([pl(draagvlakken),sg(draagvlak)],het,[]).

n([pl(draaien),sg(draai)],de,[sbar,vp],[dim(draaitje)]).

n([pl(draaiingen),sg(draaiing)],de,[]).

n([pl(draken),sg(draak)],de,[]).

n([meas(drachme),pl(drachmes),pl(drachmen)],de,[meas_mod,measure]).

n([pl(drachten),sg(dracht)],de,[]).

n([mass(draf)],de,[],
  [dim(drafje)]).

n([pl(dragers),sg(drager)],de,[],
  [s(gegeven),
   s(gezag)]).

n([pl(draagsters),sg(draagster)],de,[],
  [s(gezag)]).

n([meas(dram)],de,[meas_mod,measure]).

n([pl('drama\'s'),sg(drama)],het,[subject_sbar],[]).

n([pl('drama\'s'),sg(drama)],het,[],
  [familie,
   film,
   televisie,
   dim(dramaatje)]).

n([mass(dramatiek)],de,[]).

%% de spin Sebastiaan
%% daar gaat hij met zijn drang...
n([mass(drang)],de,[vp,sbar],
  [i(daad,daden)]).

n([pl(dranken),sg(drank)],de,[],
  [dim(drankje)]).

n([sg(draver),pl(dravers)],de,[]).

n([sg(draverij),pl(draverijen)],de,[]).

n([pl(dreven),sg(dreef)],de,[vp]).

n([sg(dreg),pl(dreggen)],de,[]).

n([pl(dreigementen),sg(dreigement)],het,[sbar,vp]).

n([pl(dreigingen),sg(dreiging)],de,[sbar,vp],[]).

n([pl(dreigingen),sg(dreiging)],de,[],
  [s(oorlog),
   terreur]).

n([mass(drek)],de,[]).

n([pl(drempels),sg(drempel)],de,
  [vp],
  [dim(drempeltje)]).

n([pl(drempels),sg(drempel)],de,[],
  [kies,
   s(verkeer),
   dim(drempeltje)]).

n([pl(drenkelingen),sg(drenkeling)],de,[]).

n([sg(drent),pl(drenten)],de,[]).

n([sg([dress,code])],de,[]).

n([sg(dressing)],de,[]).

n([pl(dressoirs),sg(dressoir)],het,[]).

n([mass(dressuur)],de,[]).

n([pl(dreunen),sg(dreun)],de,[]).

n([sg(dribbel),pl(dribbels)],de,[]).

n([pl(drieën),sg(drie)],de,[],[dim(drietje)]).

n([bare_meas(driemaal),pl(driemalen)],both,[temp_mod,measure,sbar]).

n([sg(driekwartier)],de,[temp_mod,measure,sbar]).

n([sg(drieluik),pl(drieluiken)],het,[]).

n([pl(driemanschappen),sg(driemanschap)],het,[]).

n([mass(drieëenheid)],de,[]).

n([sg(driepunter),pl(driepunters)],de,[]).

n([sg(driewerf)],both,[np_app_measure,
                       start_app_measure,
                       app_measure]).

n([pl(driften),sg(drift)],de,[vp,pred_pp(op)],[]).

n([pl(drijfveren),sg(drijfveer)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp]).

n([pl(drijvers),sg(drijver)],de,[],[]).

n([sg(dril)],both,[]).

n([sg(drink),pl(drinks)],de,[]). % Vlaams

n([pl(drinkers),sg(drinker)],de,[],
  [h(niet),
   f(niet)
  ]).

n([mass(drive)],de,[vp]).

n([sg(driver),pl(drivers)],de,[]).

n([sg('drive-in'),sg([drive,in])],de,[]).

n([mass(droefenis)],de,[]).

n([mass(droefheid)],de,[]).

n([sg(droger),pl(drogers)],de,[]).

n([mass(droging)],de,[]).

n([pl(drogisten),sg(drogist)],de,[]).

n([pl(drogisterijen),sg(drogisterij)],de,[]).

n([pl(drollen),sg(drol)],de,[],[dim(drolletje)]).

n([pl(drommen),sg(drom)],de,[measure]).

n([sg(dromedaris),pl(dromedarissen)],de,[]).

n([pl(dromers),sg(dromer)],de,[]).

n([pl(drommels),sg(drommel)],de,[]).

n([sg(drone),pl(drones)],de,[]).

n([pl(dronken),sg(dronk)],de,[]).

n([pl(dronkaards),sg(dronkaard)],de,[]).

n([pl(dronkelappen),sg(dronkelap)],de,[]).

n([mass(dronkenschap)],de,[]).

n([sg(droogje)],het,[]).

n([mass(droogte)],de,[]).

n([mass(droogval)],de,[]).

n([pl(dromen),sg(droom)],de,
  [subject_sbar,
   sbar,
   subject_vp,
   vp],
  [s(jongen),
   dim(droompje)]).

n([mass(drop)],de,[],[dim(dropje)]).

n([pl([drop,outs]),pl('drop-outs'),
   sg([drop,out]),sg('drop-out')],de,[]).

n([pl(drugs),sg(drug),mass(drugs)],de,[],[smart]).

n([pl(druiven),sg(druif)],de,[],[dim(druifje)]).

n([pl(drukken),sg(druk)],de,
  [sbar,
   vp,
   pred_pp(onder),
   pred_pp(onder,vp)]).

n([pl(drukken),sg(druk)],de,[],
  [i(last,lasten),
   olie,
   regel,
   spuit,
   s(tijd),
   water]).

n([pl(drukken),sg(druk)],de,[],
  [mis,
   zeef]).

n([pl(drukkers),sg(drukker)],de,[]).

n([pl(drukkerijen),sg(drukkerij)],de,[]).

n([pl(drukpersen),sg(drukpers)],de,[]).

n([mass(druks)],both,[]).  % 3440 pagina's druks.  drukwerk?

n([mass(drukte)],de,[]).

n([pl(drums),sg(drum)],de,[]).

n([mass([drum,'\'n',bass]),mass([drum,'\'n\'',bass]),
   mass([drum,'&',bass])],de,[]). % modern type of music?

n([sg(drummer),pl(drummers)],de,[]).

n([pl(druppen),sg(drup)],de,[measure],[dim(drupje)]).

n([pl(druppels),sg(druppel)],de,[measure],
  [dim(druppeltje)]).

n([pl(druppels),sg(druppel)],de,[],
  [zweet,
   dim(druppeltje)]).

n([mass(dualisme)],het,[]).

n([mass(dub)],de,[]).

n([pl(dubbels),sg(dubbel)],both,[]).

n([pl(dubbeldekkers),sg(dubbeldekker)],de,[]).

n([pl(dubbelgangers),sg(dubbelganger)],de,[]).

n([sg(dubbelleven),pl(dubbellevens)],het,[]).

n([pl(dubbelspelen),sg(dubbelspel)],het,[]).

n([stem(dubbel_DIM),
   pl(dubbeltjes),sg(dubbeltje)],het,
  [measure,
   meas_mod],
  [euro]).

n([sg(dubbeltwee),pl(dubbeltwees)],de,[measure]).

n([sg(dubbelvier),pl(dubbelviers)],de,[measure]).

n([pl(dubbelzinnigheden),sg(dubbelzinnigheid)],de,[pred_pp(van)]).

n([sg(dude),pl(dudes)],de,[]).

n([pl(duellen),pl(duels),sg(duel)],het,[measure,temp_mod],
  [beker,
   competitie,			% twee duels schorsing
   'Davis-Cup',
   'Europa-Cup',
   wh(['Europa','cup']),
   wh(['Europa','Cup']),
   'UEFA-Cup',
   'UEFA-cup',
   wh(['UEFA','cup']),
   wh(['UEFA','Cup']),
   'Champions-League',
   'Champions-League',
   wh(['Champions','League']),
   wh(['Champions','league']),
   kop,
   kwalificatie, 'EK-kwalificatie',
   oefen,
   thuis,
   uit]).

n([sg(duet),pl(duetten)],het,[]).

n([sg('dug-out'),pl('dug-outs'),
   sg([dug,out]),pl([dug,outs])],de,[]).

n([mass(duidelijkheid)],de,[]).

n([pl(duiders),sg(duider)],de,[]).

n([mass(duiding)],de,[]).

n([pl(duiven),sg(duif)],de,[],
  [post,
   tortel, % niet tor_tel_duif
   dim(duifje)]).

n([pl(duiken),sg(duik)],de,[]).

n([pl(duikboten),sg(duikboot)],de,[]).

n([pl(duikers),sg(duiker)],de,[],
  [parel,
   dim(duikertje)]).

n([pl(duimen),sg(duim)],de,[measure],[dim(duimpje)]).

n([sg(duimbreed)],both,[mod]).

n([sg(duimschroef),pl(duimschroeven)],de,[]).

n([pl(duinen),sg(duin)],both,[]).

n([mass(duin)],het,[]).

n([mass(duister)],het,[]).

n([pl(duisternissen),sg(duisternis)],de,[]).

n([pl(duiten),sg(duit)],de,[],[dim(duitje)]).

n([sg(duvel),pl(duvels)],de,[],[dim(duveltje)]).

n([pl(duivelen),pl(duivels),sg(duivel)],de,[],[dim(duiveltje)]).

n([mass(duizeligheid)],de,[]).

n([pl(duizelingen),sg(duizeling)],de,[]).

n([stem(duizend),pl(duizenden)],de,[measure]).

n([sg(dummy),pl('dummy\'s')],de,[]).

n([sg(dump),pl(dumps)],de,[]).

n([sg(dumping),pl(dumpingen)],de,[]).

n([mass(dunk)],de,[]).

n([pl('duo\'s'),sg(duo)],both,[measure],[dim(duootje)]).

n([sg(dupe)],de,[]).

n([mass(duplo)],de,[]).

n([mass(durf)],de,[vp]).

n([sg(dutchbatter),pl(dutchbatters),
   sg('Dutchbatter'),pl('Dutchbatters')],de,[]).

n([pl(dutjes),sg(dutje)],het,[]).

n([sg(dutyfree),
   sg('duty-free'),
   sg([duty,free])],de,[]).

n([mass(duur)],de,[],
  []).

n([mass(duur)],de,[pred_pp(van)],
  [accu,
   s(arbeid),
   s(bedrijf),
   s(geldigheid),
   speel,
   studie,
   s(verblijf)]).

n([mass(duurzaamheid)],de,[]).

n([pl(duwen),sg(duw)],de,[],[dim(duwtje)]).

n([sg(dvd),pl('dvd\'s'),sg('DVD'),pl('DVD\'s')],de,[]).

n([pl(dwaalsporen),sg(dwaalspoor)],het,[pred_pp(op)]).

n([pl(dwazen),sg(dwaas)],de,[]).

n([pl(dwaasheden),sg(dwaasheid)],de,[]).

n([pl(dwalingen),sg(dwaling)],de,[sbar]).

n([mass(dwang)],de,[],
  [tijd]).

n([pl(dwangbuizen),sg(dwangbuis)],both,[]).  % celex het

n([pl(dwangsommen),sg(dwangsom)],de,[]).

n([mass(dwangverpleging)],de,[]).

n([pl(dwarsbalken),sg(dwarsbalk)],de,[]).

n([pl(dwarsliggers),sg(dwarsligger)],de,[]).

n([pl(dweilen),sg(dweil)],de,[]).

n([pl(dwergen),sg(dwerg)],de,[],[dim(dwergje)]).

n([mass(dwingelandij)],de,[]).

n([mass(dynamiek)],de,[]).

n([mass(dynamiet)],het,[]).

n([pl(dynastieën),sg(dynastie)],de,[]).

n([pl('e\'s'),sg(e)],de,[],[dim('e\'tje')]).

n([sg(ebola)],de,[]).

n([mass('e-commerce')],de,[]).

n([stem('e-mail'),
   pl(emails),sg(email),
   pl('e-mails'),sg('e-mail'),
   pl('E-mails'),sg('E-mail')],de,
  [sbar],
  [dim(emailtje),
   dim('e-mailtje'),
   dim('E-mailtje')]).

n([pl([email,accounts]),sg([email,account]),
   pl(['e-mail',accounts]),sg(['e-mail',account])],het,[]).

n([pl([email,adressen]),sg([email,adres]),
   pl(['e-mail',adressen]),sg(['e-mail',adres])],het,[]).

n([sg('e-ticket'),
   pl('e-tickets')],both,[]).

n([sg(eagle),pl(eagles)],de,[]).

n([sg([earnings,yield,ratio]),sg(['earnings-yield',ratio])],de,[]).

n([mass([eau,de,cologne])],de,[]).

n([mass([eau,de,parfum])],de,[]).

n([pl([eaux,de,toilette]),
   mass([eau,de,toilette])],de,[]).

n([mass([eau,de,vie])],de,[]).

n([mass(eb)],de,[]).

n([mass('e-commerce')],de,[]).

n([pl(echecs),sg(echec)],het,[]).

n([pl('echo\'s'),sg(echo)],de,[],[dim(echootje)]).

n([mass(echt)],de,[]).

n([pl(echtelieden)],de,[]).

n([mass(echtheid)],de,[]).

n([pl(echtparen),sg(echtpaar)],het,[]).

n([pl(echtscheidingen),sg(echtscheiding)],de,[]).

n([sg(eclips),pl(eclipsen)],de,[]).

n([mass(ecologie)],de,[]).

n([sg(ecoloog),pl(ecologen)],de,[],
  [s(stad)]).

n([mass(economie),pl(economieën)],de,[],
  [kennis,
   markt,
   overleg,
   wereld]).

n([pl(economisten),sg(economist)],de,[]).

n([pl(economen),sg(econoom)],de,[]).

n([mass(ecotax)],de,[]).

n([meas(ecu),pl('ecu\'s')],de,[meas_mod,measure]).

n([pl(eczemen),sg(eczeem)],het,[]).

n([pl(edellieden),sg(edelman)],de,[]).

n([mass(edelmoedigheid)],de,[]).

n([pl(edelstenen),sg(edelsteen)],de,[app_measure]).

n([pl(edicten),sg(edict)],het,[]).

n([pl(edikten),sg(edikt)],het,[]).

n([sg(edit),pl(edits)],de,[]).

n([pl(edities),sg(editie)],de,[temp_mod]).

n([pl(educaties),sg(educatie)],de,[]).

n([pl(eden),sg(eed)],de,[]).

%% de ee is een klinker
n([sg(ee)],de,[]).

n([sg(eedaflegging),pl(eedafleggingen)],de,[]).

n([pl(eekhoorns),sg(eekhoorn)],de,[],[dim(eekhoorntje)]).

n([mass(eelt)],het,[]).

n([mass([een,en,ander])],het,[]).

n([mass([één,en,ander])],het,[]).

n([mass([een,of,ander])],both,[]).

n([mass([één,of,ander])],both,[]).

n([pl(eenden),sg(eend)],de,[],[dim(eendje)]).

n([mass(eendracht)],de,[]).

n([pl(eenheden),sg(eenheid)],de,
  [measure,
   app_measure,
   vp],
  [reken,
   maat,
   ondersteun,f([ondersteun])]).

n([pl(eenheden),sg(eenheid)],de,[],
  [leger]).

n([pl(eenlingen),sg(eenling)],de,[]).

n([sg(eenmaal)],both,[temp_mod,sbar]).

n([mass(eenrichtingsverkeer)],het,[]).

n([mass(eensgezindheid)],de,[]).

n([mass(eenstemmigheid)],de,[]).

% eentje is al een pronoun
n([stem(één),pl(eentjes)],het,[]).

n([mass(eentonigheid)],de,[]).

n([mass(eenvoud)],de,[pred_pp(van)]).

n([mass(eenwording)],de,[]).

n([mass(eenzaamheid)],de,[]).

n([mass(eenzijdigheid)],de,[]).

n([mass(eer)],de,[sbar,subject_sbar,vp,subject_vp]).

n([mass(eerbetoon)],het,[subject_sbar,subject_vp]).

n([pl(eerbewijzen),sg(eerbewijs)],het,[]).

n([mass(eerbied)],de,[]).

n([mass(eerbiediging)],de,[]).

n([mass(eerherstel)],het,[]).

n([mass(eerlijkheid)],de,[pred_pp(van)]).

n([stem(één),pl(eersten),pl(eerstes)],both,[]).

n([sg([eerste,kerstdag]),
   sg(['Eerste','Kerstdag'])],de,[temp_mod,sbar]).

n([sg([eerste,paasdag]),
   sg(['Eerste','Paasdag'])],de,[temp_mod,sbar]).

n([sg([eerste,pinksterdag]),
   sg(['Eerste','Pinksterdag'])],de,[temp_mod,sbar]).

n([sg('eerste-divisieclub'),
   sg([eerste,divisieclub]),
   pl('eerste-divisieclubs'),
   pl([eerste,divisieclubs])],de,[]).

n([sg('eerste-fase')],de,[]).

n([pl(eerstejaars),sg(eerstejaars)],de,[]).

n([sg(eerstgenoemde),pl(eerstgenoemden),
   sg([eerst,genoemde]),pl([eerst,genoemden])],de,[]).

n([mass(eerzucht)],de,[]).

n([pl(eethuizen),sg(eethuis)],het,[],[dim(eethuisje)]).

n([mass(eetlust)],de,[]).

n([sg(eetster),pl(eetsters)],de,[]).

n([pl(eetzalen),sg(eetzaal)],de,[]).

n([sg(eeuw),pl(eeuwen)],de,[measure,temp_mod,sbar],
  [kwart]).

n([pl(eeuwigheden),sg(eeuwigheid)],de,[temp_mod]).

n([mass(efedrine)],de,[]).

n([pl(effecten),sg(effect),
   pl(effekten),sg(effekt)],het,[sbar,subject_sbar],
  [broeikas,
   i(wissel_koers,wisselkoers)]).

n([mass(effectbejag)],het,[]).

n([mass(effectiviteit)],de,[]).

n([pl(effectueringen),sg(effectuering)],de,[]).

n([mass(efficiency)],de,[]).

n([mass(efficiëntie)],de,[]).

n([pl(egels),sg(egel)],de,[],[dim(egeltje)]).

n([mass(ego),pl('ego\'s')],het,[]).

n([mass(egoïsme)],het,[]).

n([pl(egoïsten),sg(egoïst)],de,[]).

n([pl(eieren),sg(ei)],het,[],
  [broed,
   dim(eitje)]).

n([pl(eicellen),sg(eicel)],de,[]).

n([pl(eierdooiers),sg(eierdooier)],de,[]).

n([pl(eierstokken),sg(eierstok)],de,[]).

n([mass(eigeel)],het,[]).

n([pl(eigenaars),pl(eigenaren),sg(eigenaar),
   sg(eigenaresse),pl(eigenaresses)],de,[],
  [huis,
   land,
   h(mede)]).

n([pl(eigenaardigheden),sg(eigenaardigheid)],de,[sbar]).

n([pl(eigenaressen),sg(eigenares)],de,[]).

n([mass(eigenbelang)],het,[]).

n([mass(eigendom)],de,[]).

n([pl(eigendommen),sg(eigendom)],het,[pred_pp(in)]).

n([mass(eigene)],het,[]).

n([pl(eigenheden),sg(eigenheid)],de,[sbar]).

n([mass(eigenliefde)],de,[]).

n([pl(eigennamen),sg(eigennaam)],de,[]).

n([pl(eigenschappen),sg(eigenschap)],de,
  [vp,sbar]).

n([pl(eigenschappen),sg(eigenschap)],de,[],
  [quantum,
   h(rij),rij]).

n([mass(eigenwaarde)],de,[]).

n([pl(eiken),sg(eik)],de,[]).

n([mass(eikehout)],het,[]).

n([mass(eiken)],het,[]).

n([pl(eikels),sg(eikel)],de,[],[dim(eikeltje)]).

n([pl(eilanden),sg(eiland)],het,[],
  [koraal,
   dim(eilandje)]).

n([sg(eilander),pl(eilanders)],de,[]).

n([sg(eind),pl(einden)],het,
  [measure,
   subject_sbar,
   temp_mod,
   sbar]).

n([sg(einde),pl(eindes)],het,
  [measure,
   subject_sbar,
   temp_mod,
   sbar]).

n([sg(einde),pl(eindes)],het,[],
  [s(leven)]).

n([pl(einders),sg(einder)],de,[]).

n([mass(eindigheid)],de,[]).

n([pl(eindjes),sg(eindje)],het,[measure,meas_mod]).

n([pl(eindresultaten),sg(eindresultaat)],het,[sbar,subject_sbar]).

n([pl(eindronden),pl(eindrondes),sg(eindronde)],de,[]).

n([sg(eindstreep),pl(eindstrepen)],de,[]).

n([sg(eindstrijd),pl(eindstrijden)],de,[]).

n([pl(eisen),sg(eis)],de,
  [sbar,
   vp]).

n([pl(eisen),sg(eis)],de,[],
  [i(brand_veiligheid,brandveiligheids),
   functie,h(functie),
   s(inkomen),
   s(kwaliteit),
   loon,
   h(milieu),
   minimum,
   straf,
   s(toelating),
   s(veiligheid),
   s(zorgvuldigheid)]).

n([pl(eisers),sg(eiser)],de,[]).

n([pl(eiseressen),sg(eiseres)],de,[]).

n([pl(eiwitten),sg(eiwit)],het,[app_measure]).

n([pl(ejaculaties),sg(ejaculatie)],de,[]).

n([pl(eksters),sg(ekster)],de,[],[dim(ekstertje),schol]).

n([pl(ellen),sg(el)],de,[measure]).

n([mass(elan)],both,[]).  % VL onze elan

n([mass(elasticiteit)],de,[]).

n([pl(elastieken),sg(elastiek)],both,[],[dim(elastiekje)]).

n([sg([elder,statesman]),pl([elder,statesmen])],de,[]).

n([pl('eldorado\'s'),sg(eldorado)],het,[],[dim(eldoradootje)]).

n([mass(electoraat)],het,[]).

n([stem(elektricien),
   pl(elektriciens),sg(elektricien),
   pl(electriciens),sg(electricien)],de,[]).

n([stem(elektriciteit),
   pl(electriciteiten),sg(electriciteit),
   pl(elektriciteiten),sg(elektriciteit)],de,[]).

n([stem(elektrode),
   pl(electroden),pl(electrodes),sg(electrode),
   pl(elektroden),pl(elektrodes),sg(elektrode)],de,[]).

n([stem(elektron),
   pl(electronen),sg(electron),
   pl(elektronen),sg(elektron)],het,[]).

n([stem(elektronica),
   mass(electronica),
   mass(elektronica),
   mass(elektronika)],de,[],
  [i(consument,consumenten),
   i(consument,'consumenten-'),
   h(micro)
  ]).

n([mass(elegantie)],de,[]).

n([pl(elementen),sg(element)],het,
  [app_measure,
   subject_sbar]).  % een nieuw element in de besprekingen is dat ...

n([pl(elfen),pl(elven),sg(elf)],de,[],
  [half]). % halfelven zijn sprookjesfiguren blijkbaar

n([sg(elfmeter),pl(elfmeters)],de,[]).

n([sg(elftal),pl(elftallen)],het,[measure],
  [dames,
   jeugd,
   voetbal,
   i(vrouw,vrouwen)
  ]).

n([pl(elites),sg(elite)],de,[]).

n([pl(ellebogen),sg(elleboog)],de,[]).

n([pl(ellenden),pl(ellendes),sg(ellende)],de,[sbar,vp]).

n([pl(ellendelingen),sg(ellendeling)],de,[]).

n([pl(elpees),sg(elpee)],de,[]).

n([pl(emancipaties),sg(emancipatie)],de,[]).

n([pl('embargo\'s'),sg(embargo)],het,[],[s(handel)]).

n([pl(emblemen),sg(embleem)],het,[],[dim(embleempje)]).

n([pl('embryo\'s'),sg(embryo)],both,[]).

n([sg([emerging,market]),pl([emerging,markets])],de,[]).

n([sg(emeritaat)],het,[]).

n([pl(emigranten),sg(emigrant)],de,[]).

n([pl(emigrantes),sg(emigrante)],de,[]).

n([pl(emigraties),sg(emigratie)],de,[]).

n([pl(emirs),sg(emir)],de,[]).

n([pl(emiraten),sg(emiraat)],het,[]).

n([pl(emissies),sg(emissie)],de,[],
  [i(aandeel,aandelen),
   claim]).

n([pl(emmers),sg(emmer)],de,[measure],
  [dim(emmertje)]).

n([pl(emmers),sg(emmer)],de,[],
  [as,
   dim(emmertje)]).

n([pl(emoties),sg(emotie)],de,[]).

n([mass(emotionaliteit)],de,[]).

n([mass(empirisme)],het,[]).

n([pl(emplooien),sg(emplooi)],het,[]).

n([pl(employés),
   pl('employé\'s'),
   sg(employé)],de,[],[dim(employeetje)]).

n([pl(emulsies),sg(emulsie)],de,[]).

n([pl(enclaves),sg(enclave)],de,[],[h(moslim)]).

n([pl(encyclieken),sg(encycliek)],de,[]).

n([pl(encyclopedieën),sg(encyclopedie)],de,[]).

n([pl(enden),sg(end),sg(ende)],het,[]).

n([pl(energieën),sg(energie)],de,[vp]).

n([pl(energieën),sg(energie)],de,[],
  [kern,
   wind,
   i(zon,'zonne-')]).

n([pl(engagementen),sg(engagement)],het,[]).

n([pl(engelen),sg(engel)],de,[],[aarts,
                                 dim(engeltje)]).

n([pl(enkels),sg(enkel)],de,[],
  [dim(enkeltje),
   linker,
   rechter]).

n([pl(enkelingen),sg(enkeling)],de,[]).

n([pl(enkelspelen),sg(enkelspel)],het,[]).

n([sg(enkelvoud),pl(enkelvouden)],het,[]).

n([pl(enquêtes),sg(enquête)],de,[app_measure],[h('Bijlmer')]).

n([pl(ensceneringen),sg(enscenering)],de,[]).

n([pl(ensembles),sg(ensemble)],het,[],
  [barok,
   i(ster,sterren)
  ]).

n([pl(ensembles),sg(ensemble)],het,[measure],
  []).

n([mass(enter)],both,[]).  % druk op enter

n([pl(entertainers),sg(entertainer)],de,[]).

n([mass(entertainment)],both,[]).

n([mass(enthousiasme),mass(enthoesiasme),
   mass(entoesiasme),mass(entousiasme)],het,[]).

n([pl(entiteiten),sg(entiteit)],de,[]).

n([sg(entomoloog),pl(entomologen)],de,[]).

n([pl(entourages),sg(entourage)],de,[]).

n([pl(entrees),sg(entree)],both,[],[dim(entreetje)]).

n([pl(enveloppen),sg(envelop),
   pl(enveloppes),sg(enveloppe)],de,[measure],[dim(envelopje)]).

n([pl(enzymen),sg(enzym)],het,[app_measure]).

%% kleine lp
n([sg(ep),sg('EP'),pl('ep\'s'),pl('EP\'s')],de,[]).

%% europarlement
n([sg('EP')],het,[]).

n([pl(epauletten),sg(epaulet)],de,[]).

n([pl(epidemieën),sg(epidemie)],de,[],
  [griep,
   wh(['mond-',en,klauwzeer]),['mond-',en,klauwzeer],
   h('Sars')
  ]).

n([mass(epilepsie)],de,[]).

n([pl(epilogen),sg(epiloog)],de,[]).

n([pl(episoden),pl(episodes),sg(episode)],de,[]).

n([pl(epen),pl(epossen),sg(epos)],het,[]).

n([sg(equipe),pl(equipes)],de,[]).

n([pl(equivalenten),sg(equivalent)],both,[]).

n([mass(ere)],de,[pred_pp(in)]).

n([pl(erecties),sg(erectie)],de,[]).

n([pl(eredivisies),sg(eredivisie)],de,[measure]).

n([mass(erewoord)],het,[]).

n([pl(erven),sg(erf)],het,[],
  [i(boer,boeren)]).

n([pl(erfdelen),sg(erfdeel)],het,[]).

n([mass(erfelijkheid)],de,[]).

n([pl(erfenissen),sg(erfenis)],de,[]).

n([pl(erfgenamen),sg(erfgenaam)],de,[]).

n([pl(erfgenames),sg(erfgename)],de,[]).

n([pl(erfgoederen),sg(erfgoed)],het,[]).

n([pl(erflaters),sg(erflater)],de,[]).

%% both de and het, therefore not a compound
n([pl(erfpachten),sg(erfpacht)],both,[],[]).

n([mass(erfzonde)],de,[]).

n([pl(ergernissen),sg(ergernis)],de,[sbar]).

n([mass(erkenning),pl(erkenningen),sg(erkenning)],de,[sbar]).

n([mass(erkentelijkheid)],de,[]).

n([pl(erkers),sg(erker)],de,[]).

n([mass(ernst)],de,[]).

n([pl(erosies),sg(erosie)],de,[]).

n([mass(erotiek)],de,[]).

n([sg(error),pl(errors)],de,[]).

n([pl(ertsen),sg(erts)],both,[]).

n([sg(erudiet),pl(erudieten)],de,[]).

n([mass(eruditie)],de,[]).

n([pl(erupties),sg(eruptie)],de,[]).

n([pl(ervaringen),sg(ervaring)],de,[sbar,vp,subject_sbar,subject_vp],[]).

n([pl(ervaringen),sg(ervaring)],de,[],
  [lees,
   luister,
   werk]).

n([pl(erwten),sg(erwt)],de,[]).

n([pl(essen),sg(es)],de,[]).

n([sg(escalatie),pl(escalaties)],de,[]).

n([pl(escapades),sg(escapade)],de,[],[dim(escapadetje)]).

n([pl(escortes),sg(escorte)],both,[]).

n([pl(eskaders),sg(eskader)],het,[],
  [s(dood),
   dim(eskadertje)]).

n([sg(eskadron),pl(eskadrons)],het,[measure]).

n([stem('Esperanto'),
   mass(esperanto),
   mass('Esperanto')],het,[]).

n([sg(espresso)],de,[]).

n([mass(esprit)],de,[]).

n([pl(essays),sg(essay)],het,[]).

n([pl(essayisten),sg(essayist)],de,[]).

n([pl(essencen),pl(essences),sg(essence)],de,[]).

n([pl(essenties),sg(essentie)],de,[sbar]).

n([mass(establishment)],het,[]).

n([sg(estafette),pl(estafettes)],de,[app_measure]).

n([pl(esters),sg(ester)],de,[]).

n([pl(estheten),sg(estheet)],de,[]).

n([mass(esthetiek)],de,[]).

n([sg(estuarium),pl(estuariën),pl(estuaria)],het,[]).

n([pl(etablissementen),sg(etablissement)],het,[]).

n([pl(etages),sg(etage)],de,[]).

n([pl(etalages),sg(etalage)],de,[]).

n([pl(etappen),pl(etappes),sg(etappe)],de,[temp_mod],[berg]).

n([mass(eten)],het,[],
  [avond,
   s(lieveling),
   middag
  ]).

n([pl(etenswaren),sg(etenswaar)],de,[]).

n([pl(etentjes),sg(etentje)],het,[]).

n([pl(eters),sg(eter)],de,[],[vlees]).

n([pl(ethers),sg(ether)],de,[]).

n([mass(ethiek)],de,[]).

n([mass(ethologie)],de,[]).

n([mass(ethos)],het,[]).

n([pl(etiketten),sg(etiket)],het,
  [start_app_measure,
   np_app_measure,
   app_measure],
  [dim(etiketje)]).

n([mass(etikettering)],de,[]).

n([mass(etiquette)],de,[]).

n([sg(etmaal),pl(etmalen)],het,[temp_mod,sbar,measure],[dim(etmaaltje)]).

n([pl(etsen),sg(ets)],de,[]).

n([pl(etters),sg(etter)],de,[]).

n([pl(etudes),sg(etude)],de,[]).

n([pl(etuis),sg(etui)],het,[],[dim(etuitje)]).

n([mass(eucharistie)],de,[]).

n([mass(euforie)],de,[]).

n([pl(eunuchen),sg(eunuch)],de,[]).

n([stem(euro),
   bare_meas(euro),
   bare_meas(eur),pl('euro\'s'),
   bare_meas('€'),
   bare_meas(eu)  % frequent in Sonar
  ],de,[meas_mod,measure]).

n([mass([euro,ongelood])],de,[]).

n([mass([euro,loodvrij])],de,[]).

n([sg(eurokit),pl(eurokits)],de,[]).

n([sg(euroland),pl(eurolanden)],het,[]).

n([sg('Europacup'),pl('Europacups'),
   sg(europacup), pl(europacups)],de,[app_measure]).

n([sg('Europarlementariër'),pl('Europarlementariërs')],de,[]).

n([sg(['Europees','Commissaris'])],de,[app_measure]).

n([mass(eutanasie)],de,[]).

n([mass(euthanasie)],de,[]).

n([pl(euvelen),pl(euvels),sg(euvel)],het,[],[dim(euveltje)]).

n([pl(evacuaties),sg(evacuatie)],de,[]).

n([sg(evacué),
   sg(evacuée),
   pl(evacués),
   pl('evacu\'s'),
   pl(evacuées)],de,[]).

n([pl(evaluaties),sg(evaluatie)],de,[]).

n([pl(evangelies),pl(evangeliën),sg(evangelie)],het,[sbar]).

n([pl(evangelisten),sg(evangelist)],de,[]).

n([pl(evenaars),sg(evenaar)],de,[]).

n([pl(evenbeelden),sg(evenbeeld)],het,[]).

n([pl(evenementen),sg(evenement)],het,[sbar]).

n([pl(evenementen),sg(evenement)],het,[],
  [dans,
   muziek,
   sport,
   top,
   voetbal,
   wieler,
   zeil]).

n([sg(evenknie)],de,[]).

n([pl(evenredigheden),sg(evenredigheid)],de,[]).

n([mass(evenwicht)],het,[pred_pp(uit),
                         pred_pp(in)]).

n([mass(evenwicht)],het,[],
  [s(begroting)]).

n([pl(evers),sg(ever)],de,[],[dim(evertje)]).

n([pl(evidenties),sg(evidentie)],de,[sbar]).

n([pl(evoluties),sg(evolutie)],de,[]).

n([pl(exen),sg(ex)],de,[]).

n([sg([ex,libris]),pl([ex,libris])],het,[]).

n([pl(examens),pl(examina),sg(examen)],het,[app_measure],
  [eind,
   her,
   s(inburgering),
   school,
   s(toelating),
   dim(examentje)]).

n([sg(examinator),pl(examinatoren)],de,[app_measure]).

n([sg([executive,director]),pl([executive,directors])],de,[]).

n([sg([executive,producer]),pl([executive,producers])],de,[]).

n([pl(excellenties),sg(excellentie)],de,[]).

n([sg(exceptie),pl(excepties)],de,[]).

n([pl(excessen),sg(exces)],het,[]).

n([sg(exclusiviteit),pl(exclusiviteiten)],de,[]).

n([pl(excursies),sg(excursie)],de,[sbar]).

n([pl(excuses),sg(excuus)],het,[sbar,vp]).

n([pl(executies),sg(executie)],de,[]).

n([pl(exegeses),sg(exegese)],de,[sbar]).

n([pl(exemplaren),sg(exemplaar)],het,[],[dim(exemplaartje)]).

n([sg(exercitie),pl(exercities)],de,[]).

n([mass(existentialisme)],het,[]).

n([pl(existenties),sg(existentie)],de,[]).

n([pl(exkursies),sg(exkursie)],de,[sbar]).

n([mass(exodus)],de,[]).

n([mass(expansie)],de,[]).

n([pl(expedities),sg(expeditie)],de,[]).

n([pl(experimenten),sg(experiment)],het,[]).

n([pl(experts),sg(expert),pl(experten)],de,[],[wapen]).

n([mass(expertise)],de,[],[h(contra)]).

n([pl(expliciteringen),sg(explicitering)],de,[]).

n([pl(exploitanten),sg(exploitant)],de,[],[kabel]).

n([pl(exploitaties),sg(exploitatie)],de,[]).

n([pl(exploitatietekorten),sg(exploitatietekort)],het,[]).

n([pl(exploraties),sg(exploratie)],de,[]).

n([pl(explosies),sg(explosie)],de,[],[gas]).

n([pl(explosieven),sg(explosief)],both,[]).

n([sg(expo)],de,[]).

n([pl(exponenten),sg(exponent)],de,[]).

n([sg(export),pl(exporten)],de,[]).

n([pl(exporteurs),sg(exporteur)],de,[]).

n([pl(exposities),sg(expositie)],de,
  [sbar,
   np_app_measure]).

n([pl(expressies),sg(expressie)],de,
  [sbar,
   start_app_measure,
   app_measure,
   np_app_measure]).

n([mass(expressionisme)],het,[]).

n([pl(extases),sg(extase)],de,[]).

n([pl(extazes),sg(extaze)],de,[]).

n([pl('extra\'s'),pl(extras),
   sg(extra)],het,[],[dim(extraatje)]).

%% inv VL veel gebruikt voor verlenging
%% "een extra time"
n([sg([extra,time]),pl([extra,times])],de,[]).

n([pl(extracten),sg(extract)],het,[]).

n([pl(extremen),sg(extreem)],het,[]).

n([mass(extremisme)],het,[],
  [h(links),
   moslim,h(moslim),
   h(rechts)]).

n([pl(extremisten),sg(extremist)],de,[],
  [h(links),
   moslim,h(moslim),
   h(rechts)]).

n([sg([eye,opener]),pl([eye,openers])],de,[]).

n([pl(ezels),sg(ezel)],de,[],[dim(ezeltje)]).

n([pl('f\'s'),sg(f)],de,[],[dim('f\'je')]).

n([sg(fa),pl('fa\'s')],de,[]).

n([mass(faam)],de,[]).

n([pl(fabelen),pl(fabels),sg(fabel)],de,[sbar,subject_sbar],[dim(fabeltje)]).

n([mass(fabricage)],de,[]).

n([pl(fabrieken),sg(fabriek),ignore(m(fabriek,noun(de,count,sg),fabriek))],de,[],
  [auto,
   i(haard,haarden),
   machine,
   meubel,
   s(opwerking),
   staal,
   steiger,
   suiker,
   textiel,
   uranium,
   vuurwerk,
   wapen,
   dim(fabriekje)]).

n([sg(fabriek)],both,[]). % het: Vlaams

n([pl(fabriekshallen),sg(fabriekshal)],de,[]).

n([mass(fabrikaat),pl(fabrikaten)],het,[],[half]).

n([stem(fabricage),
   mass(fabrikage)],de,[]).

n([pl(fabrikanten),sg(fabrikant)],de,[],
  [auto,
   i(band,banden),
   s(chip),chip,
   i(chip_machine,chipmachine),
   computer,
   condoom,
   i(sigaret,sigaretten),
   s(tabak),
   vliegtuig]).

n([sg([face,lift]),pl([face,lifts]),
   sg('face-lift'),pl('face-lifts'),
   sg(facelift),pl(facelifts)],de,[]).

n([pl(facetten),sg(facet)],het,[sbar]).

n([pl(faciliteiten),sg(faciliteit)],de,[sbar]).

n([pl(faciliteiten),sg(faciliteit)],de,[],
  [s(investering),
   productie]).

n([pl(facties),pl(factiën),sg(factie)],de,[]).

n([pl(factoren),sg(factor),
   pl(faktoren),sg(faktor)],de,
  [sbar,
   subject_sbar,
   app_measure],
  [productie,
   i(productie,produktie),
   risico]).

n([pl(factoren),sg(factor),
   pl(faktoren),sg(faktor)],de,[],
  [s(aaibaarheid)]).

n([pl(factorijen),sg(factorij)],de,[],[dim(factorijtje)]).

n([pl(facturen),sg(factuur)],de,[],[dim(factuurtje)]).

n([pl(faculteiten),sg(faculteit)],de,[app_measure,vp]).

n([pl(faculteiten),sg(faculteit)],de,[app_measure],
  [sub]).

n([sg(fado),pl('fado\'s')],de,[]).

n([sg(fagot),pl(fagotten)],de,[]).

n([sg(failliet)],both,[]).

n([sg(fair),pl(fairs)],de,[]).

n([pl(faillissementen),sg(faillissement)],het,[]).

n([sg([fait,divers]),pl([faits,divers])],both,[]).

n([mass(fake)],both,[]).

n([pl(fakkels),sg(fakkel)],de,[],[dim(fakkeltje)]).

%% door eigen falen
n([mass(falen)],het,[],[hart]).

n([mass([fall,out])],de,[]).

n([pl(families),sg(familie)],de,
  [],
  [schoon]).

n([pl(families),sg(familie)],de,
  [app_measure],
  [i(gras,grassen),
   onder,
   i(plant,planten),
   taal
  ]).

n([pl(fans),sg(fan)],de,[],[voetbal]).

n([sg(fanaat),pl(fanaten)],de,[]).

n([pl(fanatici),sg(fanaticus)],de,[]).

n([mass(fanatisme)],het,[]).

n([sg([fancy,fair]),pl([fancy,fairs])],de,[]).

n([pl(fanfaren),pl(fanfares),sg(fanfare)],de,[]).

n([pl(fantasieën),sg(fantasie),
   pl(fantazieën),sg(fantazie)],de,[sbar],[dim(fantasietje),
					   dim(fantazietje)]).

n([mass(fantasy)],de,[]).

n([pl(fantasten),sg(fantast)],de,[]).

n([sg(fantoom),pl(fantomen)],het,[]).

n([pl('farao\'s'),sg(farao)],de,[]).

n([pl(farcen),pl(farces),sg(farce)],de,[]).

n([pl(farms),sg(farm)],de,[]).

n([pl(farmaca),sg(farmacon)],het,[]).

n([sg(farmaceut),pl(farmaceuten)],de,[]).

n([mass(farmacie)],de,[]).

n([pl(fascinaties),sg(fascinatie)],de,[]).

n([mass(fascisme)],het,[]).

n([pl(fascisten),sg(fascist)],de,[],[h(neo),neo]).

n([stem(fase),pl(fasen),pl(fases),sg(fase),
   sg(faze),pl(fazen)],de,[sbar,temp_mod],
  [alarm,
   eind,
   s(groep),
   s(ontwikkeling),
   s(overgang),
   slot]).

n([pl(faseringen),sg(fasering)],de,[]).

n([mass([fast,food])],both,[]).

n([mass(fatalisme)],het,[]).

n([pl(fatsoenen),sg(fatsoen)],het,[sbar,vp],[dim(fatsoentje)]).

n([pl(fata),sg(fatum)],het,[]).

n([sg([fata,morgana])],both,[]).

n([pl('fauna\'s'),sg(fauna)],de,[]).

n([pl(fauteuils),sg(fauteuil)],de,[],[dim(fauteuiltje)]).

n([sg([faux,pas])],de,[]).

n([pl(favorieten),sg(favoriet)],de,[],
  [top]).

n([pl(favorietes),sg(favoriete)],de,[],
  [top]).

n([pl(faxen),sg(fax)],de,[]).

n([pl(fazanten),sg(fazant)],de,[]).

n([pl(façaden),pl(façades),sg(façade)],de,[sbar],[dim(façadetje)]).

n([sg(februari)],de,[temp_mod,sbar]).

n([mass(federalisme)],het,[]).

n([pl(federaties),sg(federatie)],de,[],
  [atletiek,
   voetbal]).

n([pl(feeën),sg(fee)],de,[],[dim(feetje)]).

n([sg(feedback)],de,[sbar]).

n([pl(feeksen),sg(feeks)],de,[]).

n([pl(feesten),sg(feest)],het,
  [subject_sbar,
   subject_vp]).

n([pl(feesten),sg(feest)],het,[],
  [dans, % en niet dan_feest
   eeuw,
   familie,
   s(kampioen),
   kerst,
   offer,
   school,
   sinterklaas,
   tuin,
   s(volk),
   dim(feestje)]).

n([pl([zo,en,feestdagen])],de,[]).

n([pl(feestelijkheden),sg(feestelijkheid)],de,[]).

n([pl(feestmalen),sg(feestmaal)],het,[]).

n([sg(feestvierder),pl(feestvierders)],de,[]).

n([pl(feiten),sg(feit)],het,
  [sbar,subject_sbar],
  [dim(feitje),
   nieuws,
   wapen]).

n([pl(feitelijkheden),sg(feitelijkheid)],de,[sbar]).

n([mass(felheid)],de,[]).

n([pl(felicitaties),sg(felicitatie)],de,[]).

n([mass(feminisme)],het,[]).

n([pl(feministen),sg(feminist)],de,[]).

n([pl(feministes),sg(feministe)],de,[]).

n([sg([femme,fatale]),pl([femmes,fatales])],de,[]).

n([pl(fenomenen),sg(fenomeen)],het,
  [sbar,
   subject_sbar,
   app_measure],
  [dim(fenomeentje)]).

n([mass(fenomenologie)],de,[]).

n([sg(festijn),pl(festijen)],het,[]).

n([pl(festivals),sg(festival)],het,[start_app_measure],
  [s(bevrijding),
   s(dichter),
   film,
   metal,
   muziek,
   pop,
   rock,
   song,
   theater]).

n([pl(festiviteiten),sg(festiviteit)],de,[]).

n([pl(feuilletons),sg(feuilleton)],het,[],[dim(feuilletonnetje)]).

n([sg(feut),pl(feuten)],de,[]).

n([meas(ffr),pl(ffr)],de,[meas_mod,measure]).

n([pl('fiasco\'s'),sg(fiasco),
   pl('fiasko\'s'),sg(fiasko)],het,[]).

n([sg(fiat)],het,[]).

n([pl(fiches),sg(fiche)],both,[start_app_measure]).

n([sg([fiche,'d\'impact'])],de,[]).

n([pl(ficties),sg(fictie)],de,[sbar],
  [non,
   h(non)]).

n([sg([field,goal]),pl([field,goals])],de,[]).

n([sg([field,ranger]),pl([field,rangers])],de,[]).

n([pl(fietsen),sg(fiets)],de,[],
  [i(kind,kinder),		% niet kin_derf_iets
   motor,
   race,
   snor,
   s(stad),
   vouw,
   dim(fietsje)]).

n([pl(fietsers),sg(fietser)],de,[],[dim(fietsertje)]).

n([pl(fietspaden),sg(fietspad)],het,[]).

n([pl(figuranten),sg(figurant)],de,[]).

n([pl(figuren),sg(figuur)],both,[],
  [hoofd,
   neven,			% en niet neef_figuur
   sleutel,
   s(sprookje),
   stijl,
   strip,
   i(vrouw,vrouwen),
   dim(figuurtje)]).

n([pl(fijnproevers),sg(fijnproever)],de,[]).

n([pl(fikken),sg(fik)],de,[],[dim(fikkie)]).

n([sg(filantroop),pl(filantropen)],de,[]).

n([pl(files),sg(file)],de,[]).

n([sg(filet),pl(filets)],de,[],
  [s(kalf),
   s(lam),
   kip,
   zalm]).

n([pl(filialen),sg(filiaal)],het,[],[dim(filiaaltje)]).

n([pl(films),sg(film)],de,[],
  [actie,
   animatie,
   avonturen,
   h('B'),
   bioscoop,
   dans,
   debuut,
   drama,
   examen,
   i(eind_examen,eindexamen),
   familie,
   horror,
   jeugd,
   wh([low,budget]),
   micro,i(micro,mikro),
   misdaad,
   s(oorlog),
   porno,
   i(ramp,rampen),
   seks,
   i(seks,sex),
   speel,
   teken,
   televisie,
   televisie,tv,h(tv),i(tv,'TV-'),f([tv]),
   video,
   dim(filmpje)]).

n([mass([film,noir])],de,[]).

n([sg(filmmaker),pl(filmmakers)],de,[]).

n([pl(filmers),sg(filmer)],de,[]).

n([pl(filmhuizen),sg(filmhuis)],het,[]).

n([pl(filologen),sg(filoloog)],de,[]).

n([sg(filologie)],de,[]).

n([pl(filosofieën),sg(filosofie)],de,[sbar],[dim(filosofietje)]).

n([pl(filosofen),sg(filosoof)],de,[]).

%% rijkeluiszoontjex
n([sg([fils,à,papa])],de,[]).

n([pl(filters),sg(filter)],both,[],[dim(filtertje)]).

n([pl(finales),sg(finale)],de,
  [app_measure],
  [beker,
   kwart,
   'Europa-Cup',
   i(wereld_beker,wereldbeker),
   h('WK')]).

n([pl(finalisten),sg(finalist)],de,[]).

n([pl(finalistes),sg(finaliste)],de,[]).

n([pl(financiën)],de,[],[s(overheid)]).

n([pl(financiers),sg(financier)],de,[]).

n([pl(financieringen),sg(financiering)],de,[]).

n([mass(financieringstekort),pl(financieringstekorten),sg(financieringstekort)],het,[]).

n([pl(finesses),sg(finesse)],de,[]).

n([mass(finish)],de,[]).

n([pl('firma\'s'),sg(firma)],de,[],
  [export,
   i(plaat,platen),
   reclame,
   dim(firmaatje)]).

n([sg([first,lady])],de,[]).

n([mass(fiscus)],de,[]).

n([mass(fiskus)],de,[]).

n([pl([fish,and,chips]),pl([fish,'&',chips])],de,[]).

%% bridge
n([sg(fit),pl(fits)],de,[]).

n([mass(fitness)],both,[]).

n([sg(fitter),pl(fitters)],de,[]).

n([sg(fix),pl(fixes)],de,[]).

n([pl(fixaties),sg(fixatie)],de,[]).

n([mass(fixeer)],de,[]).

n([pl(fjorden),sg(fjord)],both,[]).

n([pl(flacons),sg(flacon)],de,[measure],
  [injectie,
   dim(flaconnetje)]).

n([mass(flair)],de,[]).

n([sg(flamingant),pl(flaminganten)],de,[]).

n([sg(flamingo),pl('flamingo\'s')],de,[]).

n([sg(flan),pl(flans)],de,[]).  % Spaanse pudding

n([pl(flanken),sg(flank)],de,[],
  [rechter,
   linker]).

n([pl(flappen),sg(flap)],de,[],[dim(flapje)]).

n([pl(flarden),sg(flard)],de,[measure,
			      pred_pp_pl(aan)]).

n([sg(flashback),pl(flashbacks)],de,[]).

n([pl(flats),sg(flat)],de,[],
  [service,
   toren,
   dim(flatje)]).

n([sg([flat,screen]),pl([flat,screens])],de,[]).

n([sg([flat,tax])],de,[]).

n([pl(flaters),sg(flater)],de,[],[dim(flatertje)]).

n([mass(flauwekul)],de,[subject_sbar,subject_vp]).

n([sg(flensje),pl(flensjes)],het,[]).

n([pl(flessen),sg(fles)],de,
  [measure],
  [dim(flesje),
   bier,
   gas,
   melk,
   pet,
   thermos,
   veld,
   wijn]).

n([pl(flessen),sg(fles)],de,
  [pred_pp(op)]).

n([mass(fleur)],de,[]).

n([mass([fleur,de,sel])],de,[]).

n([mass(flexibilisering)],de,[]).

n([mass(flexibiliteit)],de,[]).

n([sg([flight,data,recorder]),pl([flight,data,recorders])],de,[]).

n([sg(flik),pl(flikken)],de,[]).

n([pl(flikkers),sg(flikker)],de,[],[dim(flikkertje)]).

n([sg(flinter),pl(flinters)],de,[measure],[dim(flintertje)]).

n([sg(flip),pl(flippen),pl(flips)],de,[]).

n([sg(flipper),pl(flippers)],de,[]).

n([sg(flippo),pl('flippo\'s')],de,[]).

n([sg(flirt),pl(flirten)],de,[]).

n([pl(flitsen),sg(flits)],de,[],
  [licht]).

n([sg(flop),pl(floppen)],de,[]).

n([pl('flora\'s'),sg(flora)],de,[]).

n([pl(flottieljes),sg(flottielje)],both,[]).  % de

n([mass([flottielje,zeilen])],het,[]).

n([pl(fluctuaties),sg(fluctuatie)],de,[]).

n([sg(fluim),pl(fluimen)],de,[measure]).

n([pl(fluiten),sg(fluit)],de,[],
  [dwars,
   s(herder),
   pan,
   riet,
   dim(fluitje)]).

n([pl(fluittonen),sg(fluittoon)],de,[]).

n([pl(fluwelen),sg(fluweel)],het,[],[dim(fluweeltje)]).

n([pl(fobieën),sg(fobie)],de,[]).

n([pl(focussen),sg(focus)],de,[],[subject_sbar,
                                  subject_vp]).

n([sg(foedraal),pl(foedralen)],het,[]).

n([pl(foefen),sg(foef)],de,[],[dim(foefje)]).

n([pl(foetussen),sg(foetus)],de,[]).

n([mass([foie,gras])],de,[]).

n([mass(fok)],de,[]).

n([pl(fokkers),sg(fokker)],de,[]).

n([pl(foksen),sg(foks)],de,[]).

n([pl(folders),sg(folder)],de,
  [np_app_measure],[dim(foldertje)]).

n([pl(folies),sg(folie)],both,[],[aluminium]).

n([pl('folio\'s'),sg(folio)],het,[],[dim(foliootje)]).

n([mass(folk)],de,[]).

n([mass(folklore)],de,[]).

n([sg(follikel),pl(follikels)],de,[]).

%% bedankt voor de follow
n([sg(follow),pl(follows)],de,[]).

n([stem('follow-up'),
   sg([follow,up]),
   sg('follow-up'),
   pl('follow-ups'),
   pl([follow,ups])],de,[]).

n([sg(follower),pl(followers)],de,[]).

n([mass(folter)],de,[]).

n([pl(folteringen),sg(foltering)],de,[]).

n([pl(fondsen),sg(fonds)],het,[app_measure],
  [h('AEX'),
   i(aandeel,aandelen),
   s(belegging),
   beurs,
   chemie,
   click,
   gemeente,
   hedge,
   hoofd,
   hightech,[high,tech],wh([high,tech]),
   internet,
   s(investering),
   nood,
   olie,
   pensioen,
   piramide,
   i(ramp,rampen),
   spaar, % en niet pensioen_paar_fonds
   structuur,
   techno,
   technologie,
   telecom,
   vastgoed]).

n([mass(fonetiek)],de,[]).

n([mass(fonologie)],de,[]).

n([pl(fonteinen),sg(fontein)],de,[],[dim(fonteintje)]).

n([sg(foodprocessor),pl(foodprocessors)],de,[]).

n([pl(fooien),sg(fooi)],de,[],[dim(fooitje)]).

n([meas(foot),pl(feet)],de,[meas_mod,measure]).

n([mass(force)],de,[]).

n([mass([force,de,frappe])],de,[]).

n([pl(fords),sg(ford)],de,[]).

n([pl(forehands),sg(forehand)],de,[]).

n([pl(forellen),sg(forel)],de,[],[dim(forelletje)]).

n([sg(forens),pl(forenzen),pl(forensen)],de,[]).

n([sg(forfait),pl(forfaits)],het,[]).

n([meas(forint),pl(forinten)],de,[measure]).

n([mass(forma)],de,[]).

n([pl(formaten),sg(formaat)],het,[pred_pp(van)]).

n([pl(formaliteiten),sg(formaliteit)],de,[]).

n([sg(format),pl(formats)],both,[]).

n([pl(formateurs),sg(formateur)],de,[]).

n([pl(formaties),sg(formatie)],de,[measure]).

n([pl(formaties),sg(formatie)],de,[],
  [s(kabinet),
   i(minderheid_kabinet,minderheidskabinets)
  ]).

n([mass(formica)],both,[]).

n([pl(formules),sg(formule)],de,[sbar,vp],
  [bruto,
   succes,
   dim(formuletje)]).

n([pl(formuleringen),sg(formulering)],de,[sbar,vp]).

n([pl(formulieren),sg(formulier)],het,[app_measure],
  [aangifte,
   aanvraag,
   inschrijf,
   invul,
   i(vraag,vragen),
   schade]).

n([pl(fornuizen),sg(fornuis)],het,[]).

n([pl(forten),sg(fort)],het,[]).

n([sg(fortificatie),pl(fortificaties)],de,[]).

n([mass(fortuin)],both,[sbar]).

n([pl(fortuinen),sg(fortuin)],het,[sbar,vp],[dim(fortuintje)]).

n([pl(fora),pl(forums),sg(forum)],het,[]).

n([sg(forward),pl(forwards)],de,[]). % basketbal

n([sg([forward,air,controller]),pl([forward,air,controllers])],de,[]).

n([pl(fosfaten),sg(fosfaat)],het,[]).

n([mass(fosfor)],both,[]).

n([pl(fossielen),sg(fossiel)],het,[]).

n([pl('foto\'s'),sg(foto)],de,[],
  [naakt,
   profiel,
   trouw,
   i(zwart_wit,'zwart-wit'),
   dim(fotootje)]).

n([pl('foto-albums'),sg('foto-album')],het,[]).

n([pl(fotocopieën),sg(fotocopie)],de,[]).

n([pl(fotografen),sg(fotograaf)],de,[],[mode,pers,top]).

n([pl(fotografes),sg(fotografe)],de,[],[mode,pers,top]).

n([pl(fotografies),pl(fotografieën),sg(fotografie)],de,[],
  [mode,
   pers,
   dim(fotografietje)]).

n([pl(fotokopieën),sg(fotokopie)],de,[]).

n([pl(fotomodellen),sg(fotomodel)],het,[]).

n([sg(foton),pl(fotonen)],both,[]).

n([sg(fotoshoot),pl(fotoshoots)],de,[]).

n([sg([founding,father]),pl([founding,fathers])],de,[]).

n([pl(fouten),sg(fout)],de,
  [subject_sbar,
   subject_vp,
   vp,
   sbar],
  [denk,
   dim(foutje)]).

n([pl(fouten),sg(fout)],de,
  [],
  [net,
   procedure,
   reken,
   spel,
   spelling,
   type,
   vorm,
   dim(foutje)]).

n([pl(foxen),sg(fox)],de,[]).

n([pl(foyers),sg(foyer)],both,[]).

n([mass(fraais)],het,[]).

n([sg(fractie),pl(fracties),
   pl(frakties),sg(fraktie)],de,[temp_mod,sbar]).

n([sg(fractie),pl(fracties),
   pl(frakties),sg(fraktie),
   ignore_stem(fractie)],de,[],
  [h('CD'),
   h('CDA'),
   coalitie,
   i('D66','D\'66-'),h('D66'),
   i(één_man,eenmans),
   h('GroenLinks'),
   kamer,
   i(kamer,'Kamer'),
   'Tweede-Kamer',['Tweede','Kamer'],
   'Eerste-Kamer',['Eerste','Kamer'],
   h('LPF'),
   h('PvdA'),
   s(regering),
   h('SP'),
   i(staat,staten),
   h('VVD')]).

n([sg(fractuur),pl(fracturen)],de,[]).

n([pl(fragmenten),sg(fragment)],het,[measure],[dim(fragmentje)]).

n([pl(fragmenten),sg(fragment)],het,[],
  [beeld,
   film,
   s(geluid),
   dim(fragmentje)]).

n([pl(frambozen),sg(framboos)],de,[]).

n([pl(frames),sg(frame)],het,[],[dim(framepje)]).

n([meas(franc),pl(francs)],de,[meas_mod,measure]).

n([sg(française),pl(françaises),
   sg(francaise),pl(francaises)],de,[]).

n([pl(franciscanen),sg(franciscaan)],de,[]).

n([pl(franciskanen),sg(franciskaan)],de,[]).

n([pl(franjes),sg(franje)],de,[]).

n([meas(frank),meas('fr.'),
   pl(franken),pl(franks)],de,[meas_mod,measure]).

n([pl(frasen),pl(frases),sg(frase)],de,[sbar],[dim(frasetje)]).

n([sg(frater),pl(fraters)],de,[]).

n([pl(fratsen),sg(frats)],de,[]).

n([pl(fraudes),sg(fraude)],de,[],
  [belasting,
   beurs,
   s(bijstand),
   boekhoud,
   bouw]).

n([sg(fraudeur),pl(fraudeurs)],de,[]).

n([pl(frazen),pl(frazes),sg(fraze)],de,[sbar]).

n([sg(frazione),pl(frazioni)],de,[]).

n([mass([free,fight])],de,[]).

n([mass([free,jazz])],de,[]).

n([sg(freak),pl(freaks)],de,[]).

n([sg(freelancer),pl(freelancers)],de,[]).

n([sg(freeware)],de,[]).

n([pl(fregatten),sg(fregat)],het,[np_app_measure]).

n([pl(frequenties),sg(frequentie),
   pl(frekwenties),sg(frekwentie)],de,[],
  [puls,  % en niet pul_frequentie
   radio]).

n([pl('fresco\'s'),sg(fresco)],both,[]).

n([pl(freules),sg(freule)],de,[]).

n([pl(fricties),sg(frictie)],de,[]).

n([pl(frieten),sg(friet),sg(frites)],de,[],
  [dim(frietje),
   oven]).

n([sg(frigo),pl('frigo\'s')],de,[]).

n([sg(frikadel),  pl(frikadellen),
   sg(frikandel), pl(frikandellen)],de,[]).

n([mass(fris)],het,[]).  % ik drink vandaag alleen fris!

n([pl(frisdranken),sg(frisdrank)],de,[]).

n([mass(frisheid)],de,[pred_pp(van)]).

n([sg(frituur),pl(frituren)],de,[]).  % Chinezen nemen de frituren over (Wablieft)

n([pl(fronsen),pl(fronzen),sg(frons)],de,[],[dim(fronsje)]).

n([pl(fronten),sg(front)],het,[],
  [kou,
   oost,
   thuis]).

n([sg([front,lady]),pl([front,ladies])],de,[]).

n([sg([front,office]),pl([front,offices])],both,[]).

n([sg([front,runner]),pl([front,runners])],de,[]).

n([mass(fruit)],het,[]).

n([sg([fruit,de,mer]),pl([fruits,de,mer])],de,[]).

n([pl(fruitvliegen),sg(fruitvlieg)],de,[]).

n([sg(frummel),pl(frummels)],de,[]).

n([pl(frustraties),sg(frustratie)],de,[sbar]).

n([pl('fte\'s'),meas(fte)],de,[measure]).

n([mass(fuck)],both,[]).

n([sg(fuga),pl('fuga\'s')],de,[]).

n([pl(fuiven),sg(fuif)],de,[]).

n([pl(fuiken),sg(fuik)],de,[]).

n([sg([full,prof]),pl([full,profs])],de,[]).

n([mass(fun)],de,[]).

n([pl(functies),sg(functie),
   pl(funkties),sg(funktie)],de,
  [pred_pp(in),
   subject_vp
  ],[]).

n([pl(functies),sg(functie),
   pl(funkties),sg(funktie)],de,
  [app_measure],
  [s(bestuur),
   top,
   neven]).

n([pl(functies),sg(functie),
   pl(funkties),sg(funktie)],de,[],
  [hersen,
   top,
   voorbeeld]).

n([mass(functionaliteit)],de,[]).

n([pl(functionarissen),sg(functionaris),
   pl(funktionarissen),sg(funktionaris)],de,[],
  [politie,
   s(regering),
   staf,
   s(veiligheid)]).

n([sg(fundamentalist),pl(fundamentalisten)],de,[],
  [moslim,h(moslim)]).

n([mass(fundamentalisme)],het,[],
  [moslim,h(moslim)]).

n([pl(fundamenten),sg(fundament)],het,[sbar]).

n([pl(funderingen),sg(fundering)],de,[],[dim(funderinkje)]).

n([mass(funding)],de,[]).

n([mass(funk)],de,[]).

n([pl(furies),pl(furiën),sg(furie)],de,[]).

n([mass(furore)],de,[]).

n([pl(fusies),sg(fusie)],de,[],[kern]).

n([pl(fusten),sg(fust)],het,[]).

n([mass(fut)],de,[]).

n([pl(futiliteiten),sg(futiliteit)],de,[]).

n([mass(fysica),mass(fysika)],de,[]).

n([pl(fysici),sg(fysicus)],de,[]).

n([mass(fysiek)],both,[]).

n([mass(fysiologie)],de,[],
  [s(ontwikkeling)]).

n([pl(fysiologen),sg(fysioloog)],de,[]).

n([sg(föhn),pl(föhns)],de,[]).

n([pl('g\'s'),sg(g)],de,[],[dim('g\'tje')]).

n([pl(gaaien),sg(gaai)],de,[],[dim(gaaitje)]).

n([pl(gaanderijen),sg(gaanderij)],de,[]).

n([sg(gaap),pl(gapen)],de,[]).

n([sg(gaard),sg(gaarde),pl(gaarden)],de,[]).

n([pl(gaarkeukens),sg(gaarkeuken)],de,[]).

n([pl(gazen),sg(gaas)],het,[]).

n([sg(gabber),pl(gabbers)],de,[]).

n([pl(gaden),sg(gade)],de,[]).

n([sg(gadget),pl(gadgets)],both,[]).

n([mass(gading)],de,[]).

n([pl(gages),sg(gage)],de,[]).

n([mass(gal)],both,[]).

n([sg(gala),pl('gala\'s')],het,[],
  [boks,			% niet bok_gala
   dans,			% niet dan_gala
   kerst]).

n([pl(galblazen),sg(galblaas)],de,[]).

n([pl(galeries),pl(galerieën),sg(galerie)],de,[],
  [foto]).

n([pl(galerijen),sg(galerij)],de,[],[dim(galerijtje)]).

n([pl(galgen),sg(galg)],de,[]).

n([sg(galm)],de,[]).

n([pl(galops),sg(galop)],de,[]).

n([sg(game),pl(games)],de,
  [temp_mod,
   measure],
  [video]).

n([pl('gamma\'s'),sg(gamma)],both,[measure]). % het: Vlaams

n([sg([gamma,ray,waarde]),pl([gamma,ray,waardes]),
                          pl([gamma,ray,waarden])],de,[]).

n([sg(gammel),pl(gammels)],de,[]).

n([pl(gangen),pl(gangs),sg(gang)],de,[pred_pp(op),   % we trekken de sprint op gang
				      pred_pp(in)]). % in volle gang

n([pl(gangen),pl(gangs),sg(gang)],de,[],
  [gal,
   martel,
   s(ontwikkeling),
   s(recht),
   stembus,
   wandel,
   zij,
   dim(gangetje)]).

n([sg([gang,bang]),pl([gang,bangs]),
   sg('gang-bang'),pl('gang-bangs'),
   sg(gangbang),pl(gangbangs)],de,[]).

n([sg(ganger),pl(gangers)],de,[],
  [bedevaart,
   beurs,
   congres,
   feest,
   kerk,
   vakantie]).

n([pl(gangmakers),sg(gangmaker)],de,[]).

n([pl(gangpaden),sg(gangpad)],het,[]).

n([pl(gangsters),sg(gangster)],de,[]).

n([pl(ganzen),sg(gans)],de,[]).

n([sg(gap),pl(gaps)],de,[]).

n([sg([gap,year]),pl([gap,years])],het,[]).

n([pl(garages),sg(garage)],de,[],[parkeer]).

n([mass([garam,masala])],both,[]).

n([pl(garanten),sg(garant)],de,[]).

n([pl(garanties),sg(garantie)],de,[sbar,vp,subject_sbar,subject_vp]).

n([pl(garanties),sg(garantie)],de,[],
  [bank,
   bijkoop,f([bijkoop]),
   s(veiligheid)
  ]).

n([pl(gardes),sg(garde)],de,[]).

n([pl(garderobes),sg(garderobe)],de,[]).

n([pl(garelen),sg(gareel)],het,[pred_pp(in),
                                pred_pp(uit)]).

n([pl(garens),sg(garen)],het,[]).

n([pl(garnalen),sg(garnaal)],de,[],[dim(garnaaltje)]).

n([pl(garneringen),sg(garnering)],de,[]).

n([sg(garnituur),pl(garnituren)],both,[measure]).

n([pl(garnizoenen),sg(garnizoen)],het,[]).

n([pl(gassen),sg(gas)],het,[app_measure],
  [aard,
   broeikas,
   gif,
   tegen,
   traan,
   uitlaat,
   zenuw]).

n([pl(gaskamers),sg(gaskamer)],de,[]).

n([pl(gaspedalen),sg(gaspedaal)],both,[]).

n([pl(gasten),sg(gast)],de,[],
  [bad,
   camping,
   ere,
   hotel,
   stam,
   dim(gastje)]).

n([pl(gastgezinnen),sg(gastgezin)],het,[]).

n([pl(gastheren),sg(gastheer)],de,[]).

n([pl(gasthuizen),sg(gasthuis)],het,[]).

n([pl(gastlanden),sg(gastland)],het,[]).

n([mass(gastvrijheid)],de,[]).

n([sg(gat)],het,[pred_pp(op)]).  % het ligt op zijn gat

n([pl(gaten),sg(gat)],het,[],
  [ozon,
   zee,
   dim(gatje),
   dim(gaatje)]).

n([sg(gate),pl(gates)],de,[]).

n([pl(gaullisten),sg(gaullist)],de,[]).

n([pl(gaven),sg(gave)],de,[vp]).

n([sg(gay),pl(gays)],de,[]).

n([pl(gazetten),sg(gazet)],de,[]).

n([pl(gazons),sg(gazon)],het,[],[dim(gazonnetje)]).

n([pl(geaardheden),sg(geaardheid)],de,[]).

n([pl(gebaren),sg(gebaar)],het,
  [van_sbar,
   sbar,
   vp],
  [dim(gebaartje)]).

n([pl(gebaren),sg(gebaar)],het,[],
  [wegwerp,
   dim(gebaartje)]).

n([mass(gebak)],het,[],
  [i(kersen,kers)]).  % niet kers_enge_bak

n([pl(gebakjes),sg(gebakje)],het,[]).

n([pl(gebeden),sg(gebed)],het,[],[dim(gebedje)]).

n([pl(gebeenten),sg(gebeente)],het,[]).

n([pl(gebergten),pl(gebergtes),sg(gebergte)],het,[],
  [laag,
   midden,
   hoog]).

n([pl(gebeurens),sg(gebeuren)],het,[],
  [s(groep)]).

n([pl(gebeurtenissen),sg(gebeurtenis)],de,[]).

n([pl(gebieden),sg(gebied)],het,[np_app_measure,app_measure],
  [afzet,
   bos,
   broed,
   buiten,
   deel,
   euro,
   expulsie,
   grens,
   grond,
   haven,
   i(hoog_druk,hogedruk),
   industrie,
   kust,
   i(laag_druk,lagedruk),
   landbouw,
   leef,
   s(macht),
   milieu,
   natuur,
   s(oorlog),
   s(platteland),
   probleem,
   ramp,
   rand,
   s(recht),
   recreatie,
   regen,
   i(rivier,rivieren),
   ski,
   s(station),
   strafschop,
   stroom,
   s(toepassing),
   taal,
   vak,
   veen,
   s(verspreiding),
   i(wad,wadden),
   weide,
   wijn,
   woon]).

n([pl(gebitten),sg(gebit)],het,[]).

n([mass(geblaat)],het,[]).

n([mass(gebladerte)],het,[]).

n([pl(geboden),sg(gebod)],het,[sbar,vp,start_app_measure]).

n([mass(gebondenheid)],de,[]).

n([mass(geboomte)],het,[]).

n([pl(geboorten),pl(geboortes),sg(geboorte)],de,[]).

n([mass(geborgenheid)],de,[]).

n([pl(gebouwen),sg(gebouw)],het,[],
  [s(appartement),
   s(bedrijf),
   bij,
   concert,
   congres,
   flat,
   s(gerecht),
   hoofd,
   kantoor,
   kerk,
   s(overheid),
   s(parlement),
   s(regering),
   school,
   s(station),
   toren,
   s(universiteit),
   dim(gebouwtje)]).

n([pl(gebreken),sg(gebrek)],het,
  [vp,
   subject_vp,
   subject_sbar]).

n([pl(gebreken),sg(gebrek)],het,
  [],
  [geld,
   s(personeel),
   plaats,
   slaap,
   spraak,
   ruimte,
   tijd,
   voedsel,
   water,
   zuurstof]).

n([pl(gebroeders)],de,[]).

n([pl(gebruiken),sg(gebruik)],het,
  [vp,
   sbar,
   subject_vp,
   subject_sbar],
  []).

n([pl(gebruiken),sg(gebruik)],het,
  [],
  [alcohol,i(alcohol,alkohol),
   auto,
   doping,
   drank,
   drug,s(drug),
   energie,
   grond,
   her,
   internet,
   medicijn,
   ruimte,
   spraak,
   taal,
   water,
   woord]).

n([pl(gebruikers),sg(gebruiker)],de,[],
  [drug,s(drug),
   eind,
   internet,
   taal
  ]).

n([mass(gebruikmaking)],de,[]).

n([pl(gebruiksaanwijzingen),sg(gebruiksaanwijzing)],de,[]).

n([pl(gebruiksvoorwerpen),sg(gebruiksvoorwerp)],het,[]).

n([pl(gebruikswaarden),sg(gebruikswaarde)],de,[]).

n([mass(gebulder)],het,[]).

n([sg(gecommitteerde),pl(gecommitteerden)],de,[]).

n([sg(gedaagde),pl(gedaagden)],de,[],[mede]).

n([pl(gedaanten),pl(gedaantes),sg(gedaante)],de,[]).

n([pl(gedachten),pl(gedachtes),sg(gedachte)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp,
   start_app_measure],
  [basis,
   grond,
   s(uitgang)]).

n([sg(gedacht)],het,[]). % VL

n([mass(gedachtegoed),mass(gedachtengoed)],het,[]).  % van Pim zeker...

n([lemma(gedachtengang),
   pl(gedachtegangen),sg(gedachtegang),pl(gedachtengangen),sg(gedachtengang)],de,[sbar,vp]).

n([pl(gedachtewisselingen),sg(gedachtewisseling),pl(gedachtenwisselingen),sg(gedachtenwisseling)],de,[]).

n([mass(gedag)],both,[]).

n([pl(gedeelten),pl(gedeeltes),sg(gedeelte)],het,[temp_mod,  % ze woonde het grootste gedeelte van haar leven in Japan
						  measure]).

n([pl(gedeelten),pl(gedeeltes),sg(gedeelte)],het,[],
  [woon]).

n([pl(gedenktekens),sg(gedenkteken)],het,[]).

n([stem(detineren),sg(gedetineerde),pl(gedetineerden)],de,[],[mede]).

n([sg(gedeputeerde),pl(gedeputeerden)],de,[]).

n([mass(gedestilleerd)],both,[]).

n([pl(gedichten),sg(gedicht)],het,[],
  [dim(gedichtje)]).

n([pl(gedierten),pl(gediertes),sg(gedierte)],het,[]).

n([pl(gedingen),sg(geding)],het,[pred_pp(in)]).

n([mass(gedistilleerd)],het,[]).

n([mass(gedoe)],het,[sbar,vp],[dim(gedoetje)]).

n([mass(gedoog)],het,[]).

n([mass(gedrag)],het,[],
  [consumptie,
   declaratie,
   koop,
   lees,
   rij,
   stem,
   vuurwerk,
   weg]).

n([pl(gedragingen),sg(gedraging)],de,[]).

n([pl(gedragscodes),sg(gedragscode)],de,[]).

n([pl(gedragsdeterminanten),sg(gedragsdeterminant)],de,[]).

n([mass(gedragsmodificatie)],de,[]).

n([pl(gedragsregelen),pl(gedragsregels),sg(gedragsregel)],de,
  [sbar,
   vp]).

n([pl(gedragsterapieën),sg(gedragsterapie)],de,[]).

n([pl(gedragswetenschappen),sg(gedragswetenschap)],de,[]).

n([mass(gedrang)],het,[]).

n([mass(gedrevenheid)],de,[]).

n([pl(gedrochten),sg(gedrocht)],het,[app_measure]).

n([mass(gedruis)],het,[],[feest]).

n([mass(geduld)],het,[]).

n([mass(geel)],het,[],[licht]).

n([mass(geelzucht)],de,[]).

n([sg(geep),pl(gepen)],de,[]).  % visfamilie

n([pl(geesten),sg(geest)],de,
  [sbar,
   van_sbar,
   vp]).

n([pl(geesten),sg(geest)],de,
  [],
  [kwel,
   team]).

n([mass(geestdrift)],de,[]).

n([mass(geestelijkheid)],de,[]).

n([pl(geestesgesteldheden),sg(geestesgesteldheid)],de,[]).

n([sg(geestelijke),pl(geestelijken)],de,[]).

n([pl(geesteskinderen),sg(geesteskind)],het,[]).

n([mass(geestesleven)],het,[]).

n([mass(geestesoog)],het,[]).

n([pl(geestigheden),sg(geestigheid)],de,[]).

n([pl(geestverwanten),sg(geestverwant)],de,[]).

n([pl(geestverwantes),sg(geestverwante)],de,[]).

n([pl(geeuwen),sg(geeuw)],de,[],[dim(geeuwtje)]).

n([mass(geflirt)],het,[]).

n([mass(gefluit)],het,[]).

n([pl(gegadigden),sg(gegadigde)],de,[],
  [eerst,f([eerst])]).

n([pl(gegevens),sg(gegeven)],het,[sbar,subject_sbar]).

n([pl(gegevens),sg(gegeven)],het,[],
  [contact,
   gebruiker,
   klant,
   netwerk,
   s(passagier),
   s(persoon),
   programma
  ]).

n([pl(gegevenselementen),sg(gegevenselement)],het,[]).

n([sg([gehakkelde,aurelia]),pl([gehakkelde,'aurelia\'s'])],de,[]).

n([mass(gehakt)],both,[]).

n([pl(gehalten),pl(gehaltes),sg(gehalte)],het,
  [],
  [alcohol,
   cholesterol,cholestorol,
   suiker,
   vet,
   water,  % niet wat-erge-halte
   zout,
   zuur,
   i(zuur,zuren)]).

n([pl(gehalten),pl(gehaltes),sg(gehalte)],het,
  [measure,
   pred_pp(van)],
  []).

n([mass(geharrewar)],het,[]).

n([mass(gehechtheid)],de,[]).

n([pl(gehelen),sg(geheel)],het,[]).

n([pl(geheimen),sg(geheim)],het,[sbar,subject_sbar],[dim(geheimpje)]).

n([pl(geheimen),sg(geheim)],het,[],
  [s(ambt),
   bank,
   s(beroep)]).

n([pl(geheimenissen),sg(geheimenis)],de,[]).

n([mass(geheimhouding)],de,[]).

n([pl(geheimzinnigheden),sg(geheimzinnigheid)],de,[]).

n([pl(gehemelten),pl(gehemeltes),sg(gehemelte)],het,[]).

n([pl(geheugens),sg(geheugen)],het,[],[werk]).

n([mass(gehoest)],het,[]).

n([mass(gehoor)],het,[]).

n([mass(gehoorzaamheid)],de,[]).

n([pl(gehuchten),sg(gehucht)],het,[],[dim(gehuchtje)]).

n([pl(geien),sg(gei)],de,[]).

n([mass(geil)],het,[]).

n([mass(geilheid)],de,[]).

n([pl(geintjes),sg(geintje)],het,[]).

n([mass(gein)],de,[]).

n([sg(geiser),pl(geisers)],de,[]).

n([sg(geisha),pl('geisha\'s')],de,[]).

n([pl(geiten),sg(geit)],de,[],[dim(geitje)]).

n([mass(gekat)],het,[]).

n([pl(gekken),sg(gek)],de,[],[voetbal]).

n([pl(gekheden),sg(gekheid)],de,[]).

n([pl(gekkenhuizen),sg(gekkenhuis)],het,[]).

n([mass(gekloot)],het,[]).

n([mass(geknars)],het,[],[i(tand,tanden)]).

n([sg(gekrakeel)],het,[]).

n([sg(gekte)],de,[]).

n([sg(gekwebbel)],het,[]).

n([mass(gel)],de,[]).

n([mass(gelach)],het,[],[hoon]).

n([pl(gelaten),sg(gelaat)],het,[]).

n([mass(gelag)],het,[]).

n([mass(gelagzaal)],de,[]).

n([mass(gelang)],het,[]).

n([mass(gelatenheid)],de,[]).

n([mass(gelatine)],de,[]).

n([pl(gelden),sg(geld)],het,[],
  [belasting,
   college,
   drug,s(drug),
   s(gemeenschap),
   inschrijf,
   kost,
   leer,
   les,
   los,
   munt,
   op,
   s(overheid),
   papier,
   i(prijs,prijzen),
   school,
   i(smart,smarte),i(smart,smarten),
   smeer,
   spaar,
   start,
   statie,
   vakantie,
   wacht,
   wissel,
   zak]).

n([mass(geldigheid)],de,[]).

n([mass(gelding)],de,[]).

n([mass(geldontwaarding)],de,[]).

n([pl(geldschieters),sg(geldschieter)],de,[]).

n([pl(geldstukken),sg(geldstuk)],het,[]).

n([pl(geledingen),sg(geleding)],de,[]).

n([sg(geleerde),pl(geleerden)],de,[],
  [atoom,
   kern,
   s(recht)]).

n([pl(geleerdheden),sg(geleerdheid)],de,[]).

n([pl(gelegenheden),sg(gelegenheid)],de,
  [sbar,
   subject_vp,
   vp,
   pred_pp(in),
   pred_pp(in,sbar),
   pred_pp(in,vp)]).

n([pl(gelegenheden),sg(gelegenheid)],de,[],
  [horeca,
   s(uitgaan)]).

n([pl(geleien),sg(gelei)],de,[]).

n([pl(geleiders),sg(geleider)],de,[app_measure],[half]).

n([pl(geleidingen),sg(geleiding)],de,[],
  [voor]). % niet voor_gelei_ding

n([pl(gelederen),sg(gelid)],het,[]).

n([pl(gelieven)],de,[]).

n([mass(gelijk)],het,[]).

n([pl(gelijkenissen),sg(gelijkenis)],de,[sbar]).

n([pl(gelijkheden),sg(gelijkheid)],de,[],
  [gender]).

n([pl(gelijkmakers),sg(gelijkmaker)],de,[]).

n([pl([gelijke,spelen]),sg(gelijkspel)],het,[]).

n([pl(gelijkstellingen),sg(gelijkstelling)],de,[]).

n([sg(gelijkvloers)],het,[]). %Vlaams

n([mass(gelijkwaardigheid)],de,[]).

n([pl(geloften),pl(geloftes),sg(gelofte)],de,[sbar]).

n([pl(geloven),sg(geloof)],het,[sbar,vp]).

n([pl(geloofsbelijdenissen),sg(geloofsbelijdenis)],de,[]).

n([pl(geloofsgemeenschappen),sg(geloofsgemeenschap)],de,[]).

n([pl(geloofsovertuigingen),sg(geloofsovertuiging)],de,[sbar,vp]).

n([mass(geloofwaardigheid)],de,[]).

n([pl(geluiden),sg(geluid)],het,[sbar,vp]).

n([pl(geluiden),sg(geluid)],het,[],
  [achtergrond,
   motor,
   stem,
   dim(geluidje)]).

n([pl(geluidsbanden),sg(geluidsband)],de,[]).

n([mass(geluk)],het,
  [sbar,
   vp,
   subject_sbar]).

n([pl(gelukwensen),sg(gelukwens)],de,[]).

n([pl(gelukzaligheden),sg(gelukzaligheid)],de,[]).

n([pl(gemaals),pl(gemalen),sg(gemaal)],both,[],[hulp]).

n([pl(gemakken),sg(gemak)],het,
  [vp,
   pred_pp(op)]).

n([pl(gemakken),sg(gemak)],het,[],
  [s(bediening)]).

n([mass(gemakzucht)],de,[]).

n([pl(gemalinnen),sg(gemalin)],de,[]).

n([mass(gember)],de,[]).

n([mass(gemeengoed)],het,[]).

%% in het gemeen
n([mass(gemeen)],het,[]).

n([pl(gemeenheden),sg(gemeenheid)],de,[vp]).

n([pl(gemeenplaatsen),sg(gemeenplaats)],de,[]).

n([pl(gemeenschappen),sg(gemeenschap)],de,[],
  [klooster,
   moslim,
   i(plant,planten),
   i(school,scholen),
   wereld
  ]).

n([mass(gemeenschapszin)],de,[]).

n([pl(gemeenten),pl(gemeentes),sg(gemeente)],de,[],
  [buur,
   deel,
   fusie,
   land,
   rand,
   wijk]).

n([pl(gemeentehuizen),sg(gemeentehuis)],het,[]).

n([pl(gemenebesten),sg(gemenebest)],het,[]).

n([pl(gemiddelden),pl(gemiddeldes),sg(gemiddelde)],het,[],
  [etmaal,
   koers,
   uur]).

n([mass(gemis)],het,[subject_sbar, subject_vp]).

n([pl(gemoederen),sg(gemoed)],het,[]).

n([pl(gemoedstoestanden),sg(gemoedstoestand)],de,[]).

n([pl(genen),sg(gen)],het,[]).

n([mass(genade),pl(genaden),pl(genades),sg(genade)],de,[]).

n([pl(gendarmen),pl(gendarmes),sg(gendarme)],de,[]).

n([mass(gendarmerie)],de,[]).

n([mass(gender)],both,[]).

n([pl(genealogieën),sg(genealogie)],de,[]).

n([pl(geneesheren),sg(geneesheer)],de,[]).

n([mass(geneeskunde)],de,[]).

n([sg(geneeskundige),pl(geneeskundigen)],de,[]).

n([pl(geneeswijzen),sg(geneeswijze)],de,[]).

n([pl(genegenheden),sg(genegenheid)],de,[sbar,vp]).

n([pl(generaals),sg(generaal)],de,[],[brigade]).

n([pl('generaal-majoors'),sg('generaal-majoor')],de,[]).

n([sg([general,manager]),pl([general,managers])],de,[]).

n([pl(generalisaties),sg(generalisatie),
   pl(generalizaties),sg(generalizatie)],de,[sbar]).

n([mass(generaliteit)],de,[]).

n([pl(generaties),sg(generatie)],de,[measure,
				     temp_mod,  % zij wonen al generaties in dit gebied
				     pred_pp(van)]).

n([pl(generatoren),pl(generators),sg(generator)],de,[]).

n([sg(generiek),pl(generieken)],de,[]). % Vlaams

n([mass(genesis)],de,[]).

n([sg(geneticus),pl(genetici)],de,[]).

n([pl(geneugten),pl(geneugtes),sg(geneugte)],de,[]).

n([pl(genezers),sg(genezer)],de,[]).

n([pl(genezingen),sg(genezing)],de,[]).

n([mass(genialiteit)],de,[]).

n([pl(genieën),sg(genie)],het,[]).

n([mass(genie)],de,[]).

n([mass(geniep)],het,[]).

n([sg(genieter),pl(genieters)],de,[],[dim(genietertje)]).

n([sg(genius)],de,[]). % de kwade g.

n([sg(genocide),pl(genocides)],de,[]).

n([mass(genoegdoening)],de,[]).

n([pl(genoegens),sg(genoegen)],het,[vp]).

n([sg(genoom),pl(genomen)],het,[],[fruitvlieg]).

n([sg(genoot),pl(genoten)],de,[],
  [ambt,
   branche,
   club,
   coalitie,
   deel,
   s(dorp),
   echt,
   i(ex_echt,'ex-echt'),
   fractie,i(fractie,fraktie),
   s(geloof),
   generatie,
   huis,
   kamer,
   klas,
   land,
   s(leeftijd),leeftijd,
   lot,
   partij,
   plaats,
   ploeg,
   provincie,
   reis,
   s(rijk),
   soort,
   stad,s(stad),
   team,
   tijd,
   vak,
   dim(genootje)]).

n([sg(genote),pl(genotes)],de,[],
  [ambt,
   bond,
   branche,
   club,
   coalitie,
   deel,
   s(dorp),
   echt,
   i(ex_echt,'ex-echt'),
   fractie,i(fractie,fraktie),
   s(geloof),
   generatie,
   huis,
   klas,
   land,
   s(leeftijd),leeftijd,
   lot,
   partij,
   plaats,
   ploeg,
   provincie,
   reis,
   s(rijk),
   soort,
   stad,s(stad),
   team,
   tijd,
   vak]).

n([pl(genootschappen),sg(genootschap)],het,[],[]).

n([pl(genietingen),sg(genot)],het,
  [subject_sbar,
   subject_vp,
   vp]).

n([pl(genres),sg(genre)],het,[measure]).

n([sg(genrestuk),pl(genrestukken)],het,[]).

n([pl(gensters),sg(genster)],de,[]).

n([pl(gentlemen),sg(gentleman)],de,[]).

n([sg(['gentlemen\'s',agreement])],het,[]).

n([pl(genera),sg(genus)],het,[app_measure]).

n([mass(geografie)],de,[]).

n([mass(geologie)],de,[]).

n([pl(geologen),sg(geoloog)],de,[]).

n([mass(gepeupel)],het,[]).

n([mass(gepluk)],het,[]).

n([pl(geraamten),pl(geraamtes),sg(geraamte)],het,[]).

n([pl(geraniums),sg(geranium)],de,[]).

n([pl(geranten),pl(gerants),sg(gerant)],de,[]).

n([pl(gerechten),sg(gerecht)],het,[],
  [bij,
   hoofd,
   na,
   tussen,
   voor]).

n([pl(gerechtigheden),sg(gerechtigheid)],de,
  [sbar,
   vp,
   subject_sbar]).

n([pl(gerechtshoven),sg(gerechtshof)],het,[]).

n([mass(gereedheid)],de,[]).

n([pl(gereedschappen),sg(gereedschap)],het,[vp]).

n([sg([gereformeerde,kerk,vrijgemaakt]),
   pl([gereformeerde,kerken,vrijgemaakt])],de,[]).

n([mass(gerichtheid)],de,[]).

n([mass(gerief)],het,[]).

n([mass(gerinkel)],het,[],
  [glas]).

n([mass(geroffel)],het,[],
  [trom]).

n([mass(geroep)],het,[],
  [boe,
   hulp]).

n([mass(geronk)],het,[]).

n([mass(gerst)],de,[]).

n([pl(geruchten),sg(gerucht)],het,[sbar,subject_sbar]).

n([pl(geruchtenstromen),sg(geruchtenstroom)],de,[sbar]).

n([mass(geruis)],het,[]).

n([mass(geruststelling),pl(geruststellingen),sg(geruststelling)],de,[sbar,subject_sbar]).

n([pl(geschenken),sg(geschenk)],het,[],
  [i(boek_week,boekenweek),
   welkomst  % waarom geen welkomgeschenk ????
  ]).

n([pl(geschiedenissen),sg(geschiedenis)],de,[sbar]).

n([pl(geschiedenissen),sg(geschiedenis)],de,[],
  [s(bewerking),
   cultuur,
   familie,
   film,
   kerk,
   kunst,
   s(leven),
   s(liefde),
   literatuur,
   s(ontstaan),
   s(ontwikkeling),
   voetbal,
   voor,
   wereld]).

n([pl(geschiedschrijvers),sg(geschiedschrijver)],de,[]).

n([mass(geschiedschrijving)],de,[]).

n([mass(geschiedwetenschap)],de,[]).

n([mass(geschiktheid)],de,[]).

n([pl(geschillen),sg(geschil)],het,
  [pred_pp(in),
   pred_pp(in,subject_sbar)
  ]).

n([pl(geschillen),sg(geschil)],het,[],[dim(geschilletje)]).

n([mass(geschrevene)],het,[]).

n([pl(geschriften),sg(geschrift)],het,[],[dim(geschriftje)]).

n([mass(geschut)],het,[],[afweer]).

n([mass(geschutter)],het,[]).

n([pl(geselen),pl(gesels),sg(gesel)],de,[]).

n([mass(gesjoemel)],het,[]).

n([pl(geslachten),sg(geslacht)],het,[pred_pp(van)],
  [s(acteur)]).

n([pl(geslachten),sg(geslacht)],het,[],
  [onder,
   i(plant,planten)]).

n([pl(geslachtsdelen),sg(geslachtsdeel)],het,[]).

n([mass(geslachtsgemeenschap)],de,[]).

n([mass(geslachtsleven)],het,[]).

n([mass(geslachtsverkeer)],het,[]).

n([mass(geslotenheid)],de,[]).

n([mass(gesoebat)],het,[]).

n([pl(gespen),sg(gesp)],de,[]).

n([mass(gespannenheid)],de,[]).

n([mass(gespletenheid)],de,[]).

n([pl(gesprekken),sg(gesprek)],het,[pred_pp(in)],
  []).

n([pl(gesprekken),sg(gesprek)],het,[],
  [s(functionering),
   intake,
   sollicitatie,
   telefoon,
   twee,
   vraag,
   voor,
   dim(gesprekje)]).

n([pl(gesprekspartners),sg(gesprekspartner)],de,[]).

n([mass(gespuis)],het,[]).

n([pl(gestalten),pl(gestaltes),sg(gestalte)],de,[]).

n([pl(gestes),sg(geste)],de,[sbar,vp]).

n([pl(gesteenten),pl(gesteentes),sg(gesteente)],het,[app_measure]).

n([pl(gestellen),sg(gestel)],het,[]).

% n([mass(gestelde)],het,[]).

n([pl(gesteldheden),sg(gesteldheid)],de,[]).

n([mass(gesternte)],het,[]).

n([pl(gestichten),sg(gesticht)],het,[]).

n([mass(gestunt)],het,[]).

n([pl(getallen),sg(getal)],het,[app_measure],
  [ken,
   s(ongeluk),
   priem,
   reserve,
   dim(getalletje)]).

n([pl(getijden),sg(getij)],het,[]).

n([pl('getto\'s'),sg(getto)],both,[]).

n([pl(getuigen),sg(getuige)],both,[sbar]).

n([pl(getuigenissen),sg(getuigenis)],both,[sbar]).

n([pl(getuigschriften),sg(getuigschrift)],het,[app_measure]).

n([mass(getut)],het,[]).

n([mass(getwist)],het,[]).

n([pl(geulen),sg(geul)],de,[],[dim(geultje)]).

n([pl(geuren),sg(geur)],de,[],
  [s(eten),
   s(lichaam),
   dim(geurtje)]).

n([pl(geuzen),sg(geus)],de,[]).

n([pl(gevaren),sg(gevaar)],het,
  [sbar,
   subject_sbar,
   subject_vp,
   vp,
   pred_pp(in),pred_pp(buiten),
   pred_pp(zonder),
   pred_pp(zonder,subject_vp)],
  []).

n([pl(gevaren),sg(gevaar)],het,
  [],
  [inflatie]).

n([pl(gevaarten),pl(gevaartes),sg(gevaarte)],het,[]).

n([pl(gevallen),sg(geval)],both,[sbar],[dim(gevalletje)]).  % in elke/iedere geval (jaja)

n([pl(gevallen),sg(geval)],both,[],
  [ziekte,
   dim(gevalletje)]).           

n([stem(vangen),
   pl(gevangenen),sg(gevangene)],de,[],
  [s(krijg)]).

n([pl(gevangenissen),sg(gevangenis)],de,[],
  [jeugd,
   s(staat)]).

n([mass(gevangeniswezen)],het,[]).

n([mass(gevangenneming)],de,[]).

n([mass(gevangenschap)],both,[],
  [s(krijg)]).

n([pl(gevechten),sg(gevecht)],het,[pred_pp(in)],
  []).

n([pl(gevechten),sg(gevecht)],het,[],
  [i(dier,dieren),
   i(stier,stieren),
   vuur]).

n([pl(gevels),sg(gevel)],de,[],[dim(geveltje)]).

n([pl(gevers),sg(gever)],de,[]).

n([mass(gevit)],het,[]).

n([mass(gevlucht)],het,[]).  % deel molen oid?

n([sg(gevoel)],het,
  [sbar,
   van_sbar,
   vp]).

n([sg(gevoel)],het,
  [sbar,
   vp
  ],
  [eer,
   s(leven),
   lust,
   s(minderwaardigheid),
   onderbuik,
   rechts,
   schuld,
   s(verantwoordelijkheid),
   voor,
   h(wij),
   h(zij),
   wraak]).

n([pl(gevoelens),sg(gevoelen)],het,
  [sbar,
   van_sbar,
   vp]).

n([pl(gevoelens),sg(gevoelen)],het,
  [sbar,
   vp
  ],
  [eer,
   s(leven),
   lust,
   s(minderwaardigheid),
   onderbuik,
   rechts,
   schuld,
   s(verantwoordelijkheid),
   wraak]).

n([pl(gevoeligheden),sg(gevoeligheid)],de,[]).

n([mass(gevoelsleven)],het,[]).

n([mass(gevogelte)],het,[]).

n([pl(gevolgen),sg(gevolg)],het,
  [sbar,subject_sbar],
  [pensioen]).

n([pl(gevolgtrekkingen),sg(gevolgtrekking)],de,[sbar]).

n([sg(gevolmachtigde),pl(gevolmachtigden)],de,[]).

n([pl(gewaden),sg(gewaad)],het,[]).

n([pl(gewaarwordingen),sg(gewaarwording)],de,[subject_vp,vp,sbar,subject_sbar]).

n([mass(gewacht)],het,[]).

n([mass(gewag)],het,[]).

n([pl(gewassen),sg(gewas)],het,[]).

n([mass(geweeklaag)],het,[]).

n([pl(geweren),sg(geweer)],het,[],
  [machine,f([machine]),
   dim(geweertje)]).

n([pl(geweerkolven),sg(geweerkolf)],de,[]).

n([mass(geweervuur)],het,[]).

n([sg(gewei),pl(geweien)],het,[],
  [i(hert,herten)]).

n([mass(geweld)],het,[],
  [natuur,
   s(oorlog),
   politie,
   straat,
   s(supporter)]).

n([pl(gewelddaden),sg(gewelddaad)],de,[]).

n([pl(gewelddadigheden),sg(gewelddadigheid)],de,[]).

n([pl(geweldplegingen),sg(geweldpleging)],de,[]).

n([pl(gewelven),sg(gewelf)],het,[]).

n([mass(gewenning)],de,[]).

n([pl(gewesten),sg(gewest)],het,[]).

n([pl(gewetens),sg(geweten)],het,[]).

n([pl(gewetensbezwaren),sg(gewetensbezwaar)],het,[]).

n([pl(gewichten),sg(gewicht)],both,[pred_pp(van),
				    pred_pp(van,subject_sbar)],[]).

n([pl(gewichten),sg(gewicht)],both,[],
  [bantam,
   halfzwaar,
   s(lichaam),
   licht,
   midden,
   onder,
   over,
   supermidden,
   superveder,
   superwelter,
   veder,
   vlieg,
   welter,
   zwaar,
   zwaarvlieg,
   zwaarwelter]).

n([mass(gewichtheffen)],het,[]).

n([pl(gewijsden),sg(gewijsde)],het,[]).

n([mass(gewin)],het,[]).

n([mass(gewoel)],het,[],[strijd]).

n([pl(gewoonten),pl(gewoontes),sg(gewoonte)],de,
  [sbar,
   subject_sbar,
   subject_vp,
   vp]).

n([pl(gewoonten),pl(gewoontes),sg(gewoonte)],de,[],
  [drink,
   eet]).

n([pl(gewrichten),sg(gewricht)],het,[],
  [s(tijd)]).

n([pl(gewrochten),sg(gewrocht)],het,[],[]).

n([mass(gewroet)],het,[]).

n([pl(gezagen),sg(gezag)],het,[sbar,vp]).

n([mass(gezagsuitoefening)],de,[]).

n([pl(gezagvoerders),sg(gezagvoerder)],de,[]).

n([pl(gezagvoersters),sg(gezagvoerster)],de,[]).

n([pl(gezangen),sg(gezang)],het,[]).

n([pl(gezanten),sg(gezant)],de,[],
  [s(handel),
   h('VN')]).

n([pl(gezantschappen),sg(gezantschap)],het,[]).

n([pl(gezegden),pl(gezegdes),sg(gezegde)],het,[sbar,app_measure,start_app_measure]).

n([pl(gezellen),sg(gezel)],de,[],
  [s(leven)]).

n([mass(gezelligheid)],de,[]).

n([pl(gezelschappen),sg(gezelschap)],het,[measure]).

n([pl(gezelschappen),sg(gezelschap)],het,[],
  [drink,
   toneel]).

n([pl(gezichten),sg(gezicht)],het,[sbar,van_sbar]).

n([pl(gezichten),sg(gezicht)],het,[],
  [bleek, % in oude romans
   s(dorp),
   s(stad),
   dim(gezichtje)]).

n([pl(gezichtskringen),sg(gezichtskring)],de,[]).

n([pl(gezichtspunten),sg(gezichtspunt)],het,[sbar]).

n([pl(gezichtsvelden),sg(gezichtsveld)],het,[]).

n([pl(gezinnen),sg(gezin)],het,[],
  [i(boer,boeren),
   eenouder,
   pleeg,
   dim(gezinnetje)  % niet gezin_net_DIM
  ]).

n([pl(gezindheden),sg(gezindheid)],de,[sbar,vp]).

n([pl(gezindten),pl(gezindtes),sg(gezindte)],de,[]).

n([mass(gezinsleven)],het,[]).

n([mass(gezinsterapie)],de,[]).

n([mass(gezinsverzorging)],de,[]).

n([mass(gezondheid)],de,[pred_pp(in)]).

n([mass(gezondheid)],de,[],[dier]).

n([pl(gezwellen),sg(gezwel)],het,[],[dim(gezwelletje)]).

n([mass(gezucht)],het,[]).

n([sg('GGO'),pl('GGO\'s')],de,[]).

n([pl(gibbons),sg(gibbon)],de,[]).

n([pl(gidsen),sg(gids)],de,[],
  [berg,
   dim(gidsje)]).

n([sg(giechel),pl(giechels)],de,[]).

n([mass(gier),pl(gieren),sg(gier)],de,[],[aas]).

n([mass(gierst)],de,[]).

n([pl(gieters),sg(gieter)],de,[],[dim(gietertje)]).

n([pl(giffen),sg(gif)],het,[],[landbouw]).

n([pl(giften),sg(gift)],de,[]).

n([pl(giganten),sg(gigant)],de,[],
  [software,
   uitgeef]).

n([pl(gijzelaars),sg(gijzelaar)],de,[]).

n([pl(gijzelingen),sg(gijzeling)],de,[pred_pp(in)],[]).

n([sg(gijzelnemer),pl(gijzelnemers)],de,[]).

n([pl(gillen),sg(gil)],de,[],[dim(gilletje)]).

n([pl(gilden),sg(gilde),sg(gild)],both,[]).

n([mass(gin)],de,[]).

n([sg(ginseng)],de,[]).

n([pl(gipsen),sg(gips)],both,[],[loop]).  %VL: de

n([sg(giraf),sg(giraffe),pl(giraffen)],de,[]).

n([sg(giro)],de,[],[accept]).

n([sg(gis),pl(gissen)],de,[]).

%% het is/blijft gissen of hij komt
n([mass(gissen)],het,[subject_sbar]).

n([sg(gisser),pl(gissers)],de,[]).

n([pl(gissingen),sg(gissing)],de,[sbar]).

n([pl(gisten),sg(gist)],de,[]).

n([pl(gistingen),sg(gisting)],de,[]).

n([pl(gitaren),sg(gitaar)],de,[],
  [bas,
   f([bas])]).

n([pl(gitaristen),sg(gitarist)],de,[],
  [bas]).

n([mass(glamour)],de,[]).

n([pl(glansen),pl(glanzen),sg(glans)],de,[]).

n([pl(glazen),sg(glas)],het,[measure],
  [beker,
   bier,
   i(bril,brillen),
   dim(glaasje),
   wijn]).

n([mass([glas,in,lood]),mass('glas-in-lood')],het,[]).

n([pl(glasvezels),sg(glasvezel)],de,[]).

n([pl(glazuren),sg(glazuur)],both,[]).

n([pl(gletschers),sg(gletscher)],de,[]).

n([pl(gletsjers),sg(gletsjer)],de,[]).

n([pl(gleuven),sg(gleuf)],de,[],[dim(gleufje)]).

n([sg(glibber),pl(glibbers)],de,[]).

n([mass(glim)],de,[]).

n([sg(glimlach)],de,[],[]).

n([pl(glimpen),sg(glimp)],de,[sbar,vp],[dim(glimpje)]).

n([pl(glinsteringen),sg(glinstering)],de,[]).

n([sg(glitter),pl(glitters)],de,[]).

n([mass(globalisering)],de,[]).

n([pl(globen),pl(globes),sg(globe)],de,[]).

n([mass(gloed)],de,[]).

n([pl(gloeilampen),sg(gloeilamp)],de,[]).

n([pl(glooiingen),sg(glooiing)],de,[]).

n([mass(gloor)],de,[]). % ouderwets

n([pl('gloria\'s'),sg(gloria)],de,[]).

n([pl(glories),pl(gloriën),sg(glorie)],de,[]).

n([pl('glossy\'s'),sg(glossy)],de,[]).

n([mass(glucose)],de,[]).

n([mass(glycerine)],de,[]).

n([mass(glycogeen)],both,[]).

n([sg('GMO'),pl('GMO\'s')],de,[]).

n([sg(gniffel),pl(gniffels)],de,[]).

n([mass(gnosis)],de,[]).

n([mass(go)],both,[]).

n([pl(goals),sg(goal)],de,[measure],[dim(goaltje)]).

n([sg(goalgetter),pl(goalgetters)],de,[]).

n([sg(goalie),pl(goalies)],de,[]).

n([pl(gobelins),sg(gobelin)],het,[]).

n([sg('God'),pl('Goden')],de,[]).

n([pl(goden),sg(god)],de,[]).

n([mass(goddelijkheid)],de,[]).

n([pl(godheden),sg(godheid)],de,[]).

n([pl(godinnen),sg(godin)],de,[],
  [wraak,
   dim(godinnetje)]).

n([pl(godsbegrippen),sg(godsbegrip)],het,[]).

n([pl(godsdiensten),sg(godsdienst)],de,[],
  [s(staat)]).

n([mass(godsdienstigheid)],de,[]).

n([pl(godshuizen),sg(godshuis)],het,[]).

n([pl(godslasteringen),sg(godslastering)],de,[sbar]).

n([mass(goeddunken)],het,[]).

n([sg(goedenmiddag),sg(goedemiddag),sg(goeiemiddag),sg(goeienmiddag)],het,[]).

n([sg(goedenmorgen),sg(goedemorgen),sg(goeiemorgen),sg(goeienmorgen)],het,[]).  % zeggen

n([sg(goedenavond),sg(goedeavond),sg(goeienavond),sg(goeienavond)],het,[]).

n([sg(goedendag),sg(goeiendag),sg(goededag),sg(goeiedag)],de,[]).  

n([sg(goed)],het,[],
  [i(bed,bedden),
   leen,
   dim(goedje)]).

n([pl(goederen),sg(goed)],het,[],[cultuur,
				  consumptie,
				  kapitaal]).

n([pl(goedheden),sg(goedheid)],de,[vp]).

n([pl(goedkeuringen),sg(goedkeuring)],de,[sbar,vp]).

n([mass(goeds)],het,[]).

%% n([mass(goedvinden)],het,[]).

n([pl(goeroes),sg(goeroe)],de,[]).

n([mass(goesting)],de,[]).

n([pl(goevernantes),sg(goevernante)],de,[]).

n([pl(goevernementen),sg(goevernement)],het,[]).

n([pl(goeverneurs),sg(goeverneur)],de,[]).

n([pl('goeverneurs-generaal'),sg('goeverneur-generaal')],de,[]).

n([sg(goffer),pl(goffers)],de,[],
  [vallei]).

n([mass(gok)],de,
  [subject_sbar,
   sbar],
  [dim(gokje)]).

n([pl(gokkers),sg(gokker)],de,[]).

n([sg([golden,boy]),pl([golden,boys])],de,[]).

n([sg([golden,goal]),pl([golden,goals])],de,[]).

n([sg([golden,oldie]),pl([golden,oldies])],de,[]).

n([sg([golden,retriever]),pl([golden,retrievers])],de,[]).

n([sg([golden,share]),pl([golden,shares])],de,[]).

n([pl(golems),sg(golem)],de,[]).

n([pl(golven),sg(golf)],de,[measure],[]).

n([pl(golven),sg(golf)],de,[],
  [hitte,
   migratie]).

n([mass(golf)],het,[]).

n([sg(golfbaan),pl(golfbanen)],de,[]).

n([sg(golfer),pl(golfers)],de,[]).

n([pl(golflengten),pl(golflengtes),sg(golflengte)],de,[]).

n([mass(golfslag)],de,[]).

n([pl(goliaths),sg(goliath)],de,[]).

n([sg(gom),pl(gommen)],both,[]).

n([sg(gondel),pl(gondels)],de,[]).

n([pl(gongs),sg(gong)],de,[]).

n([mass(gonorroe)],de,[]).

n([pl(goochelaars),sg(goochelaar)],de,[]).

n([pl([good,guys]),sg([good,guy])],de,[]).

n([mass(goodwill)],de,[]).

n([sg(gooi)],de,[]).

n([mass(['gooi-',en,smijtwerk])],het,[]).

n([pl(goten),sg(goot)],de,[]).

n([pl(gootstenen),sg(gootsteen)],de,[]).

n([pl(gordels),sg(gordel)],de,[],
  [i(gracht,grachten),
   s(veiligheid),
   dim(gordeltje)]).

n([pl(gordijnen),sg(gordijn)],het,[],[dim(gordijntje)]).

n([pl('gorilla\'s'),sg(gorilla)],de,[]).

n([mass(gort)],de,[]).

n([pl(gorters),sg(gorter)],de,[]).

n([mass(gospel)],de,[]).

n([mass(gotiek)],de,[]).

n([sg(gotspe)],de,[subject_sbar,
                   subject_vp]).

n([mass(goud)],het,[]).

n([mass([goud,geld])],het,[]).

n([mass(goudkust)],de,[]).

n([sg(goudrenet),pl(goudrenetten)],de,[]).

n([sg(goudstuk),pl(goudstukken)],het,[]).

n([pl(goudvissen),sg(goudvis)],de,[]).

n([sg(gourmet)],de,[]).

n([pl(gouvernantes),sg(gouvernante)],de,[]).

n([pl(gouvernementen),sg(gouvernement)],het,[]).

n([pl(gouverneurs),sg(gouverneur)],de,[]).

n([pl('gouverneurs-generaal'),sg('gouverneur-generaal')],de,[]).

n([sg('Gouwenaar'),pl('Gouwenaren')],de,[]).

n([sg([gouwe,ouwe]),pl([gouwe,ouwen]),pl([gouwe,ouwes])],de,[]).

n([mass(governance)],de,[]).

n([pl(gozers),sg(gozer)],de,[]).

n([sg(gps)],de,[]).

%% zes graden vorst is koud; singular?
%% hij behaalde zijn graad/*graden medicijnen
n([meas(graad),
   meas(graden),
   pl(graden)],
  de,
  [meas_mod,
   measure],
  [dim(graadje)]).

n([meas([graad,'Celsius']),
   meas([graden,'Celsius']),
   pl([graden,'Celsius'])
  ],de,
  [meas_mod,
   measure],
  [dim(graadje)]).

n([sg(graad),pl(graden),
   ignore_stem(graad)],de,[],
  [s(alfabetisering),
   s(belading),
   breedte,
   s(dekking),
   dim(graadje),
   isolatie]).

n([pl(graadmeters),sg(graadmeter)],de,[vp],[]).

n([pl(graadmeters),sg(graadmeter)],de,[],
  [technologie]).

n([sg(graai),pl(graaien)],de,[]).

n([mass(gravel)],both,[]).

%% baron, hertog, graaf
n([pl(graven),sg(graaf)],de,[]).

%% wiskundig, (on)gerichte grafen
n([sg(graaf),pl(grafen)],de,[]).

n([pl(graafschappen),sg(graafschap)],both,[]).

n([mass(graagte)],de,[]).

n([mass(graal)],de,[]).

n([pl(granen),sg(graan)],het,[],
  [winter,
   zomer,
   dim(graantje)]).

n([pl(graten),sg(graat)],de,[]).

n([pl(grachten),sg(gracht)],de,[]).

n([pl(gradaties),sg(gradatie)],de,[]).

n([sg([graduate,school]),pl([graduate,schools])],de,[]).

n([pl(graven),sg(graf)],het,[]).

n([sg(graficus),pl(grafici)],de,[]).

n([pl(grafieken),sg(grafiek)],de,[]).

n([mass(graffiti)],de,[]).

n([mass(grafschennis)],de,[]).

n([sg(grafschrift),pl(grafschriften)],het,[]).

n([pl(grafstenen),sg(grafsteen)],de,[]).

n([pl(graftomben),pl(graftombes),sg(graftombe)],de,[]).

n([mass(gram)],de,[]). % woede; grammen are measure_noun

n([meas(gram),
   pl(grammen),
   pl(grams),
   meas(g),
   meas('g.')],both,[meas_mod,measure],
  [micro,
   milli,
   kilo,
   nano,
   pico]).

n([pl('grammatica\'s'),sg(grammatica)],de,[]).

n([pl('grammatika\'s'),sg(grammatika)],de,[]).

n([pl(grammetjes),sg(grammetje)],het,[measure]).

n([pl(grammofonen),pl(grammofoons),sg(grammofoon)],de,[],[dim(grammofoontje)]).

n([mass(gramschap)],de,[]).

n([pl(granaten),sg(granaat)],de,[],
  [mortier]).

n([sg([grand,café]),
   pl([grand,cafés]),
   pl([grand,'café\'s'])
  ],het,[]).

n([sg([grand,dessert]),pl([grand,desserts])],het,[]).

n([sg([grand,jury]),pl([grand,jury])],de,[]).

n([sg([grand,prix]),pl([grand,prixs]),
   sg('grand-prix'),pl('grand-prixs')],de,[app_measure]).

n([sg(grandslam),pl(grandslams),
   sg('grand-slam'),pl('grans-slams'),
   sg([grand,slam]),pl([grand,slams])],both,[]).

n([sg([grand,old,lady]),
   pl([grand,old,ladies])],de,[]).

n([sg([grand,old,man]),
   pl([grand,old,men])],de,[]).

n([pl(grandes),sg(grande)],de,[]).

n([sg([grande,dame])],de,[]).

n([mass(grandeur)],de,[]).

n([pl([grands,crus])],de,[]).

n([mass(graniet)],het,[]).

n([pl(grappen),sg(grap)],de,
  [subject_sbar,
   subject_vp,
   sbar
  ],
  [dim(grapje)]).

n([pl(grappen),sg(grap)],de,
  [],
  [poep,
   pies,
   ['1',april]]).

n([pl(graphics),sg(graphic)],de,[],[f([computer])]).

n([pl(grappenmakers),sg(grappenmaker)],de,[]).

n([pl(grassen),sg(gras)],het,[],[kunst]).

n([pl(graslanden),sg(grasland)],het,[]).

n([sg(grasmat),pl(grasmatten)],de,[]).

n([pl(grasvelden),sg(grasveld)],het,[],
  [kunst,
   dim(grasveldje)]).

n([pl(grasvlakten),pl(grasvlaktes),sg(grasvlakte)],de,[]).

n([pl(gratiën),sg(gratie)],de,[vp]).

n([sg(gratificatie),pl(gratificaties)],de,[]).

n([mass(grauw)],het,[]).

n([pl(gravinnen),sg(gravin)],de,[],[dim(gravinnetje)]).

n([pl(gravuren),pl(gravures),sg(gravure)],de,[]).

%golf
n([sg(green),pl(greens)],de,[]).

n([pl(grepen),sg(greep)],de,[measure]).

n([pl(grepen),sg(greep)],de,[],
  [hand,
   houd,
   s(macht),
   wurg]).

n([pl(greinen),sg(grein)],het,[measure],[dim(greintje)]).

n([pl(grendels),sg(grendel)],de,[],[dim(grendeltje)]).

n([pl(grenzen),sg(grens)],de,[],
  [armoede,
   binnen,
   buiten,
   s(geluid),
   s(land),
   leeftijd,s(leeftijd),
   s(stad),
   s(toelating),
   taal]).

n([pl(grensovergangen),sg(grensovergang)],de,[]).

n([pl(grensposten),sg(grenspost)],de,[]).

n([sg(grensrechter),pl(grensrechters)],de,[]).

n([pl(grenswachten),sg(grenswacht)],de,[]).

n([pl(greppels),sg(greppel)],de,[],[dim(greppeltje)]).

n([mass(gretigheid)],de,[]).

%% wilgenstruiken
n([sg(griend),pl(grienden)],het,[]).

%% walvissen
n([sg(griend),pl(grienden)],de,[]).

n([pl(grieven),sg(grief),sg(grieve)],de,
  [sbar,
   vp,
   subject_sbar]).

n([mass(griep),pl(griepen)],de,[],
  [i(koe,koeien),
   vogel,
   dim(griepje)]).

n([pl(grieten),sg(griet)],de,[],[dim(grietje)]).

n([pl(griezels),sg(griezel)],de,[],[dim(griezeltje)]).

n([mass(griffie)],de,[]).

n([pl(griffiers),sg(griffier)],de,[],
  [s(raad)]).

n([pl(grijnzen),sg(grijns)],de,[]).

n([mass(grijs),pl(grijzen)],het,[],
  [donker,
   licht]).

n([pl(grijsaards),sg(grijsaard)],de,[]).

n([pl(grillen),sg(gril)],de,[],[dim(grilletje)]).

n([sg(grill),pl(grills)],de,[]).

n([sg(grille),pl(grilles)],de,[]). % van auto

n([pl(grimassen),sg(grimas)],de,[]).

n([mass(grind)],het,[]).

n([sg(grinnik)],both,[]).

n([mass(grint)],het,[]).

n([mass(grip)],de,[]).

n([pl(groeven),sg(groef)],de,[],[dim(groefje)]).

n([mass(groei)],de,[pred_pp(in)],
  []).

n([mass(groei)],de,[],
  [i(baan,banen),
   s(bevolking),
   geld,
   s(welvaart),
   wild]).

n([pl(groeikernen),sg(groeikern)],de,[]).

n([mass(groen)],het,[],
  [paars,h(paars),
   rood,h(rood),
   donker,
   licht,
   dim(groentje)]).

n([pl(groenten),pl(groentes),sg(groente)],de,[]).

n([pl(groepen),sg(groep)],de,[measure],[dim(groepje)]).

n([pl(groepen),sg(groep)],de,[app_measure,np_app_measure],
  [s(achterstand),
   actie,i(actie,aktie),
   i(belang,belangen),
   s(beroep),
   s(bevolking),
   bloed,
   blues,
   cabaret,
   controle,
   contact,
   dans,
   discussie,
   doel,
   i(eiland,eilanden),
   euro,
   s(gesprek),
   haplo,
   hoofd,
   s(inkomen),
   s(jongen),
   kern,
   kop,
   s(leeftijd),
   lobby,
   i(mens_recht,mensenrechten), % en niet mens_recht_eng_roe_pen
   midden,
   muziek,
   nieuws,
   s(onderzoek),
   pop,
   praat,
   pressie,
   project,
   i(project,projekt),
   punk,
   rock,
   s(speler),
   stuur,
   s(supporter),
   tarief,
   terreur,
   theater,
   toneel,
   tussen,
   vak,
   wereld,
   werk,
   woon,
   woord,
   zang,
   i(zelf_hulp,zelfhulp),
   dim(groepje)]).

n([pl(groeperingen),sg(groepering)],de,[]).

n([pl(groepsbelangen),sg(groepsbelang)],het,[]).

n([mass(groepsdynamica)],de,[]).

n([mass(groepsverband)],het,[]).

n([pl(groeten),sg(groet)],de,[],[dim(groetje)]).

n([pl(groeven),sg(groeve)],de,[],
  [steen]).

n([pl(grofheden),sg(grofheid)],de,[sbar,vp]).

n([sg(grol),pl(grollen)],de,[]).

n([sg(grom),pl(grommen)],de,[]).

%% reden
n([pl(gronden),sg(grond)],de,[vp,sbar,subject_vp,subject_sbar],
  [s(recht),
   s(rechtvaardiging)]).

%% aarde
n([pl(gronden),sg(grond)],de,[],
  [bouw,
   geboorte,
   krijt,
   landbouw,
   pot,
   weide,
   zand]).

n([pl(grondbeginselen),pl(grondbeginsels),sg(grondbeginsel)],het,[sbar,vp]).

n([pl(grondlagen),sg(grondlaag)],de,[]).

n([pl(grondleggers),sg(grondlegger)],de,[]).

n([pl(grondlegsters),sg(grondlegster)],de,[]).

n([pl(grondregelen),pl(grondregels),sg(grondregel)],de,[sbar,vp]).

n([pl(grondslagen),sg(grondslag)],de,[sbar,vp],[s(recht)]).

n([pl(grondstoffen),sg(grondstof)],de,[app_measure]).

n([pl(grondtonen),sg(grondtoon)],de,[sbar]).

n([pl(grondtroepen)],de,[]).

n([pl(grondvesten),sg(grondvest)],de,[]).

n([mass(grondwater)],het,[]).

n([pl(grootgrondbezitters),sg(grootgrondbezitter)],de,[]).

n([sg(groothandel),pl(groothandels)],de,[]).

n([pl(grootheden),sg(grootheid)],de,[sbar,vp]).

n([pl(grootjes),sg(grootje)],het,[]).

n([pl(grootmeesters),sg(grootmeester)],de,[]).

n([pl(grootmoeders),sg(grootmoeder)],de,[]).

n([pl(grootouders),sg(grootouder)],de,[]).

n([mass(grootsheid)],de,[]).

n([mass(grootspraak)],de,[subject_sbar]).

n([pl(grootten),pl(groottes),sg(grootte)],de,[vp]).

n([pl(grootten),pl(groottes),sg(grootte)],de,[],
  [s(gezin),
   schaal]).

n([pl(grootvaders),sg(grootvader)],de,[]).

n([sg(grootverdiener),pl(grootverdieners)],de,[]).

n([sg(gros),pl(grossen)],het,[meas_mod,measure]).

n([pl(grossen),sg(grosse)],de,[]).

n([pl(grotten),sg(grot)],de,[]).

n([sg(['Grote','Keurvorst']),
   sg([grote,keurvorst])],de,[np_app_measure]).

n([sg(groupie),pl(groupies)],de,[]).

n([mass(gruis)],het,[]).

n([sg(grutto),pl('grutto\'s')],de,[]).

n([pl(gruwelen),sg(gruwel)],de,[]).

n([pl(gruweldaden),sg(gruweldaad)],de,[]).

n([mass(gruwelijks)],het,[]).

n([pl(gruzelementen)],both,[pred_pp_pl(aan),
			    pred_pp_pl(in)]).

n([sg(gsm),pl('gsm\'s')],de,[]).

n([pl('guerrilla\'s'),sg(guerrilla)],de,[]).

n([sg([guest,host])],de,[]).

n([pl(guirlandes),sg(guirlande)],de,[]).

n([meas(gulden),pl(guldens)],de,[meas_mod,measure],[dim(guldentje)]).

n([pl(gulpen),sg(gulp)],de,[]).

n([mass(gulzigheid)],de,[]).

n([sg(gum),pl(gums)],both,[]).

n([sg(gun),pl(guns)],de,[]).

n([pl(gunsten),sg(gunst)],de,[vp]).

n([sg(guts),pl(gutsen)],de,[vp]).

n([sg([gutt,feeling]),pl([gut,feelings])],de,[]).

n([pl(guts)],de,[]).

n([mass(gym)],de,[]).  % gymnastiek

n([sg(gym)],het,[]).   % gymnasium

n([pl(gymnasia),pl(gymnasiums),sg(gymnasium)],het,[]).

n([mass(gymnastiek)],de,[]).

n([pl(gympen)],both,[]).  % en niet gym_pen

n([pl(gynaecologen),sg(gynaecoloog),
   pl(gynecologen), sg(gynecoloog),
   pl(gynacologen), sg(gynacoloog),
   pl(gynekologen), sg(gynekoloog)],de,[]).

n([mass(gêne)],de,[]).

n([pl('h\'s'),sg(h)],de,[],[dim('h\'tje')]).

n([pl(hagen),sg(haag)],de,[]).

n([pl(haaien),sg(haai)],de,[],[dim(haaitje)]).

n([pl(haken),sg(haak)],de,[],[dim(haakje)]).

n([pl(halen),sg(haal)],de,[],[dim(haaltje)]).

n([mass(haalbaarheid)],de,[]).

n([pl(hanen),sg(haan)],de,[],
  [kemp,
   dim(haantje)]).

n([sg([haantje,de,voorste])],het,[]).

n([pl(haren),sg(haar)],de,[],[dim(haartje)]).
n([mass(haar)],het,[]).

n([sg(hare),pl(hare)],both,[]).

n([pl(haarden),sg(haard)],de,[]).

n([sg(haardos),pl(haardossen)],de,[]).

n([mass(haardvuur)],het,[]).

n([pl(haarlokken),sg(haarlok)],de,[]).

n([pl(haarspelden),sg(haarspeld)],de,[]).

n([mass(haaruitval)],de,[]).

n([pl(hazen),sg(haas)],de,[],[dim(haasje)]).

n([mass(haast)],de,
  [vp]).

n([mass(haat)],de,[],
  [homo,
   i(vreemdeling,vreemdelingen)]).

n([sg(habitat),pl(habitats)],de,[]).

n([sg(hachje)],het,[]).

n([sg(hack),pl(hacks)],de,[]).

n([sg(hacker),pl(hackers)],de,[]).

n([pl(hagedissen),sg(hagedis)],de,[]).

n([pl(hagels),sg(hagel)],de,[]).

n([pl(hakken),sg(hak)],de,[]).

n([pl(hakenkruisen),sg(hakenkruis)],het,[]).

n([pl(hakkers),sg(hakker)],de,[]).

n([pl(hallen),sg(hal)],de,[],
  [vertrek,
   dim(halletje)]).

n([sg(halfbloed),pl(halfbloeds)],de,[]).

n([pl(halfbroers),sg(halfbroer)],de,[]).

n([mass(halfduister)],het,[]).

n([sg(halfjaar),pl(halfjaren)],het,[temp_mod,sbar,measure]).

n([sg(halfje),pl(halfjes)],het,[measure]).

n([sg(halfkoers)],both,[]).

n([pl(halfronden),sg(halfrond)],het,[]).

n([sg(halfuur),pl(halfuren)],het,[temp_mod,sbar,measure],[dim(halfuurtje)]).

n([sg(halfweg)],both,[]).

n([pl(halls),sg(hall)],de,[]).

n([mass(hallo)],het,[]).

n([pl(hallucinaties),sg(hallucinatie)],de,[]).

n([pl(halmen),sg(halm)],de,[],[dim(halmpje)]).

n([mass(halogeen)],both,[]).

n([pl(halzen),sg(hals)],de,[],
  [baarmoeder,
   h('V')]).

n([pl(halsbanden),sg(halsband)],de,[]).

n([pl(halssnoeren),sg(halssnoer)],het,[]).

n([mass(halt)],het,[]).

n([pl(halten),pl(haltes),sg(halte)],de,[]).

n([sg(halter),pl(halters)],de,[]).

n([pl(halveringen),sg(halvering)],de,[]).

n([pl(hammen),sg(ham)],de,[],[dim(hammetje)]).

n([pl(hamburgers),sg(hamburger)],de,[]).

n([pl(hamers),sg(hamer)],de,[],[dim(hamertje)]).

n([pl(hamsters),sg(hamster)],de,[]).

n([sg(hamstring),pl(hamstrings)],de,[]).

n([pl(handen),sg(hand)],de,
  [measure,
   pred_pp_pl(in),
   pred_pp_pl(uit),
   pred_pp(aan)],                           % aan de (winnende) hand
  [dim(handje)]). 

n([pl(handen),sg(hand)],de,[],
  [linker,
   i(man,mannen),
   rechter,
   i(vrouw,vrouwen),
   was,
   dim(handje)]). 

n([pl(['hand-',en,spandiensten])],de,[]).

n([sg(handbal),pl(handballen)],de,[]).

n([mass(handbal)],het,[]).

n([sg(handballer),pl(handballers)],de,[]).

n([sg(handbalster),pl(handbalsters)],de,[]).

n([mass(handbereik)],het,[]).

n([pl(handboeien),sg(handboei)],de,[]).

n([pl(handdoeken),sg(handdoek)],de,[]).

n([pl(handdrukken),sg(handdruk)],de,[]).

n([pl(handels),sg(handel)],de,[],
  [i(aandeel,aandelen),
   beurs,
   boek,
   cocaïne,
   detail,
   drug,s(drug),
   i(effect,effecten),
   hasj,
   koop,
   kunst,
   i(mens,mensen),
   optie,
   orgaan,
   ruil,
   i(slaaf,slaven),
   i(tijdschrift,tijdschriften),
   valuta,
   i(vrouw,vrouwen),
   wapen,
   wereld,
   zwart,
   dim(handeltje)]).

n([pl(handelaars),pl(handelaren),sg(handelaar)],de,[],
  [beurs,
   boek,
   drug,s(drug),
   energie,
   groot,
   kunst,
   melk,
   wapen,
   zwart]).

n([pl(handelingen),sg(handeling)],de,[]).

n([pl(handelshuizen),sg(handelshuis)],het,[]).

n([pl(handelsmerken),sg(handelsmerk)],het,[]).

n([pl(handelsregisters),sg(handelsregister)],het,[]).

n([pl(handelsreizigers),sg(handelsreiziger)],de,[]).

n([pl(handelstekorten),sg(handelstekort)],het,[]).

n([pl(handelsverdragen),sg(handelsverdrag)],het,[]).

n([pl(handelwijzen),sg(handelwijs),sg(handelwijze)],de,[]).

n([mass(handenvol)],both,[measure]).  % handenvol geld wordt verspild ..

n([pl(handgebaren),sg(handgebaar)],het,[]).

n([mass(handgemeen)],het,[]).

n([pl(handgranaten),sg(handgranaat)],de,[]).

n([mass(handhaving)],de,[],[s(recht)]).

n([sg(handheld),pl(handhelds)],de,[]).

n([pl(handicaps),sg(handicap)],de,[sbar,vp]).

n([pl(handicaps),sg(handicap)],de,[],[lees]).

n([pl(handigheden),sg(handigheid)],de,[vp],[dim(handigheidje)]).

n([mass(handjeklap)],both,[]).

n([mass(handjevol)],het,[measure]).

n([pl(handkarren),sg(handkar)],de,[]).

n([pl(handlangers),sg(handlanger)],de,[]).

n([pl(handleidingen),sg(handleiding)],de,[]).

n([mass(handomdraai)],de,[]).

n([pl(handpalmen),sg(handpalm)],de,[]).

n([pl(handreikingen),sg(handreiking)],de,[]).

n([mass(hands)],both,[]). % het was hands; hij maakte/constateerde hands;

n([pl(handschoenen),sg(handschoen)],de,[],[boks]).

n([pl(handschriften),sg(handschrift)],het,[]).

n([pl(handtassen),sg(handtas)],de,[],[dim(handtasje)]).

n([pl(handtekeningen),sg(handtekening)],de,[]).

n([mass(handvaardigheid)],de,[]).

n([pl(handvatten),sg(handvat)],het,[]).

n([pl(handvesten),sg(handvest)],het,[]).

n([pl(handvollen),sg(handvol)],both,[measure]). % VL het handvol

n([mass(handwerk)],het,[]).

n([pl(handwerkslieden),pl(handwerkslui),sg(handwerksman)],de,[]).

n([pl(hangen),sg(hang)],de,[]).

n([mass(['hang-',en,sluitwerk])],het,[]).

n([pl(hangars),sg(hangar)],de,[],[dim(hangartje)]).

n([pl(hangers),sg(hanger)],de,[],[dim(hangertje)]).

n([sg(hangijzer),pl(hangijzers)],het,[]).

n([pl(hangmatten),sg(hangmat)],de,[]).

n([pl(hanteringen),sg(hantering)],de,[]).

n([pl(happen),sg(hap)],de,[measure],[dim(hapje)]).

n([sg(hapering),pl(haperingen)],de,[]).

n([sg([happy,birthday])],both,[]).

n([sg([happy,end]),pl([happy,ends])],both,[]).

n([sg([happy,ending]),pl([happy,endings])],both,[]).

n([sg([happy,hour])],both,[]).

%% en niet hap_ton_oom
n([sg(haptonoom),pl(haptonomen)],de,[]). % Ted Troost is de enige toch?

n([sg([hard,core]),
   sg('hard-core'),
   sg(hardcore)],de,[]).

n([sg(hardcourt),
   pl(hardcourts),
   sg('hard-court'),
   pl('hard-courts'),
   sg([hard,court]),
   pl([hard,courts])],both,[]).

n([sg('hard-disc'),sg('hard-disk'),sg([hard,disk]),sg([hard,disc]),
   pl('hard-discs'),pl('hard-disks'),pl([hard,disks]),pl([hard,discs])],de,[]).

n([sg('hard-drive'),sg([hard,drive]),
   pl('hard-drives'),pl([hard,drives])],de,[]).

n([stem(harddrug),
   pl([hard,drugs]),sg([hard,drug]),
   pl('hard-drugs'),sg('hard-drug'),
   pl(harddrugs),sg(harddrug)],de,[]).

n([sg(harder),pl(harders)],de,[]).

n([pl(hardheden),sg(hardheid)],de,[vp,pred_pp(van)]).

n([mass(hardhout)],both,[],[f([tropisch])]).

n([pl(hardlopers),sg(hardloper)],de,[]).

n([mass(hardnekkigheid)],de,[]).

n([mass(hardware)],de,[]).

n([pl(harems),sg(harem)],de,[]).

n([pl([haricots,verts])],de,[]).

n([pl(haringen),sg(haring)],de,[],[dim(harinkje)]).

n([pl(harken),sg(hark)],de,[]).

n([pl(harlekijns),sg(harlekijn)],de,[]).

n([sg(harmonica),pl('harmonica\'s')],de,[],
  [mond,
   trek]).

n([pl(harmonies),pl(harmonieën),sg(harmonie)],de,[]).

n([pl(harmonisaties),sg(harmonisatie)],de,[]).

n([pl(harmoniseringen),sg(harmonisering)],de,[]).

n([pl(harnassen),sg(harnas)],het,[]).

n([pl(harpen),sg(harp)],de,[]).

n([pl(harsen),sg(hars)],both,[]).

n([pl(harten),sg(hart)],het,[],
  [s(stad),
   dim(hartje)]).

n([sg(harten),pl(harten),pl(hartens)],de,[]).

n([pl(hartaanvallen),sg(hartaanval)],de,[]).

n([mass(hartelijkheid)],de,[]).

n([mass(hartelust)],de,[]).

n([pl(harten),sg(harten)],de,[]).

n([stem(hart_DIM), sg(hartje)],het,[measure]).  % hartje stad/winter/Rotterdam

n([pl(hartkloppingen),sg(hartklopping)],de,[]).

n([pl(hartslagen),sg(hartslag)],de,[]).

n([mass(hartstilstand)],de,[]).

n([pl(hartstochten),sg(hartstocht)],de,[]).

n([mass(hartstreek)],de,[]).

n([sg(hashtag),pl(hashtags)],de,[]).

n([mass(hasj)],de,[]).

n([mass(hasjiesj)],de,[]).

n([sg(hatchback),pl(hatchbacks)],de,[]).

n([sg(hater),pl(haters)],de,[]).

n([sg(hattrick),pl(hattricks)],de,[]).

n([pl(hausses),sg(hausse)],de,[]).

n([mass(have)],de,[]).

n([sg('have-not'),pl('have-not\'s'),
   sg([have,not]),pl([have,nots])],de,[]).

n([pl(havens),sg(haven)],de,[],
  [thuis,
   dim(haventje)]).

n([mass(haver)],de,[]).

n([mass(haverklap)],de,[]).

n([mass(havermout)],de,[]).

n([pl(havezaten),sg(havezate)],de,[]).

n([pl(haviken),sg(havik)],de,[]).

%% pl: ze zit in 3 havo
n([mass(havo),pl(havo)],both,[]).

n([mass('havo/vwo'),pl('havo/vwo')],both,[]).

n([pl(hazelaars),pl(hazelaren),sg(hazelaar)],de,[]).

n([pl(hazelnoten),sg(hazelnoot)],de,[]).

n([sg(hazenlip),pl(hazenlippen)],de,[]).

n([sg(hazepad)],het,[]).

n([mass(hbo)],both,[]).

n([mass(hbs)],both,[]).

n([mass(heao)],both,[]).

n([sg(heat),pl(heats)],de,[]).

n([mass(hebzucht)],de,[]).

n([mass(hechtenis)],de,[],
  [voor]). % geen voor_hecht_nis

n([pl(hechtingen),sg(hechting)],de,[]).

n([meas(hectare),pl(hectaren),pl(hectares)],de,[meas_mod,measure]).

n([mass(hectiek)],de,[]).

n([mass(heden)],het,[]).

n([sg([hedge,fund]),pl([hedge,funds])],both,[]).

n([pl(heels)],de,[]).

n([mass(heelal)],het,[]).

n([mass(heenkomen)],het,[]).

n([sg(heer),pl(heren)],de,[app_measure],  % de heren politici
                                          % APP: leuven_yellow 12
  []).

n([sg(heer),pl(heren)],de,[],
  [keuze,
   s(krijg),
   dim(heertje)]).
				% de heren politici only plural?

n([pl([heren,dubbelspelen]),sg([heren,dubbelspel])],het,[]).

n([pl([heren,enkelspelen]),sg([heren,enkelspel])],het,[]).

n([pl(heerlijkheden),sg(heerlijkheid)],de,[]).

n([mass(heerlijks)],het,[]).

n([pl(heerschappen),sg(heerschap)],het,[]).

n([pl(heerschappijen),sg(heerschappij)],de,[]).

n([pl(heersers),sg(heerser)],de,[],[alleen]).

n([mass(heerszucht)],de,[]).

n([pl(heesters),sg(heester)],de,[],[dim(heestertje)]).

n([mass(heetst)],het,[]).  % van de strijd

n([pl(hefbomen),sg(hefboom)],de,[]).

n([pl(heffingen),sg(heffing)],de,
  [app_measure],  % de naheffing loonbeslasting
  [na,
   voor]).

n([pl(heffingen),sg(heffing)],de,[],
  [belasting,
   energie,
   import,
   i(kilo_meter,kilometer),
   milieu,
   s(rendement),
   tol,
   s(verontreiniging)]).

n([pl(heften),sg(heft)],het,[]).

n([mass(heftigheid)],de,[]).

n([pl(heggen),sg(heg),sg(hegge)],de,[],
  [dim(heggetje),
   dim(hegje)]).

n([mass(hegemonie)],de,[],[]).

n([mass(heibel)],de,[],[]).

n([pl(heiden),sg(hei),sg(heide)],de,[]).

n([pl(heidenen),sg(heiden)],de,[]).

n([mass(heidendom)],het,[]).

n([pl(heidevelden),sg(heideveld)],het,[]).

n([sg(heiland),pl(heilanden)],de,[]).

n([pl(heils),sg(heil)],het,[sbar]).

n([sg(heilbot),pl(heilbotten)],de,[]).

n([pl(heiligdommen),sg(heiligdom)],het,[]).

%% often used without determiner,
%% not used with adverbs?
%% ?* de erg heilige vertrok
n([sg(heilige),pl(heiligen)],de,[]).

n([mass(heiligheid)],de,[]).

n([pl(heiligschennissen),sg(heiligschennis)],de,[]).

n([mass(heimwee)],both,[]).

n([mass(heisa)],both,[]).

n([pl(hekken),sg(hek)],het,[],
  [drang,
   s(toegang),
   dim(hekje)]).

n([pl(hekels),sg(hekel)],de,[],
  [bloed]).

n([pl(hekkensluiters),sg(hekkensluiter),
   pl(hekkesluiters),sg(hekkesluiter)],de,[]).

n([pl(heksen),sg(heks)],de,[]).

n([pl(heksenjachten),sg(heksenjacht)],de,[]).

n([sg(heksenketel),pl(heksenketels)],de,[]).

n([pl(hekserijen),sg(hekserij)],de,[]).

n([pl(hektaren),pl(hektares),sg(hektare)],de,[measure]).

n([mass(hel),pl(hellen)],de,[]).

n([pl(helden),sg(held)],de,[],
  [sport,
   strip,
   super,
   wieler,
   zee,
   s(volk)]).

n([pl(heldendaden),sg(heldendaad)],de,[]).

n([mass(helderheid)],de,[pred_pp(van)]).

n([sg(helderziende),pl(helderzienden)],de,[]).

n([mass(helderziendheid)],de,[]).

n([pl(heldinnen),sg(heldin)],de,[],
  [strip]).

n([sg(heleboel)],de,[]).

n([sg(heletijd)],de,[temp_mod]).

n([mass([hele,hebben,en,houwen])],het,[]).

n([sg(helft),pl(helften)],de,[temp_mod,sbar,measure],
  [jaar,
   s(seizoen)]).

n([stem(helikopter),
   pl(helicopters),sg(helikopter),
   pl(helikopters),sg(helicopter)],de,[],
  [s(gevecht),
   trauma]).

n([mass(heling)],de,[]).

n([mass(helium)],both,[]).

n([pl(hellingen),sg(helling)],de,[pred_pp(op)]).

n([pl(hellingen),sg(helling)],de,[],[berg]).

n([pl(helmen),sg(helm)],de,[],[dim(helmpje)]).

n([sg(helpdesk),pl(helpdesks)],de,[]).

n([pl(helpers),sg(helper)],de,[],[dim(helpertje)]).

n([mass(hematocriet)],both,[]).

n([sg(hematocrietwaarde),pl(hematocrietwaarden),pl(hematocrietwaardes)],de,[]).

n([pl(hemden),sg(hemd)],het,[],
  [nacht,
   dim(hempje),
   dim(hemdje),
   dim(hemmetje)]).

n([pl(hemdsmouwen),sg(hemdsmouw)],de,[]).

n([pl(hemelen),pl(hemels),sg(hemel)],de,[],[dim(hemeltje)]).

n([pl(hemellichamen),sg(hemellichaam)],het,[]).

n([mass(hemoglobine)],de,[]).

n([pl(hennen),sg(hen)],de,[],[dim(hennetje)]).

n([mass(hennep)],de,[]).

% celex:het?
n([pl(hendels),sg(hendel)],both,[],[dim(hendeltje)]).

n([pl(hengels),sg(hengel)],de,[],[dim(hengeltje),werp]).
% werp-hengel en geen werp-hen-gel

n([pl(hengelaars),sg(hengelaar)],de,[],[dim(hengelaartje)]).

n([pl(hengsels),sg(hengsel)],het,[],[dim(hengseltje)]).

n([pl(hengsten),sg(hengst)],de,[],[dek]).

n([sg(hepatitis),
   sg([hepatitis,'A']),
   sg([hepatitis,'B']),
   sg([hepatitis,'E'])
  ],both,[]).

n([pl(herauten),sg(heraut)],de,[]).

n([pl(herbenoemingen),sg(herbenoeming)],de,[]).

n([pl(herbergen),sg(herberg)],de,[np_app_measure]).

n([pl(herbergiers),sg(herbergier)],de,[]).

n([mass(herbezinning)],de,[]).

n([mass(herbouw)],de,[]).

n([mass(hercules)],de,[]).

n([pl(herdenkingen),sg(herdenking)],de,[]).

n([pl(herders),sg(herder)],de,[],[dim(herdertje)]).

n([pl(herdershonden),sg(herdershond)],de,[]).

n([pl(herdrukken),sg(herdruk)],de,[]).

n([pl(herenhuizen),sg(herenhuis)],het,[]).

n([pl(herenigingen),sg(hereniging)],de,[],[s(gezin)]).

n([sg(herfst)],de,[temp_mod,sbar]).

n([pl(hergroeperingen),sg(hergroepering)],de,[]).

n([pl(herhalingen),sg(herhaling)],de,[]).

n([sg(herijking),pl(herijkingen)],de,[]).

n([pl(herindelingen),sg(herindeling)],de,[]).

n([pl(herinneringen),sg(herinnering)],de,[sbar,vp]).

n([sg(herintreder),pl(herintreders)],de,[]).

n([sg(herkansing),pl(herkansingen)],de,[]).

n([mass(herkenbaarheid)],de,[]).

n([pl(herkenningen),sg(herkenning)],de,[],
  [spraak]).

n([pl(herkenningstekens),sg(herkenningsteken)],het,[]).

n([pl(herkeuringen),sg(herkeuring)],de,[]).

n([mass(herkomst)],de,[pred_pp(herkomst)]).

n([mass(herleving)],de,[]).

n([pl(hermitages),sg(hermitage)],de,[]).

n([pl('hernia\'s'),sg(hernia)],de,[]).

n([pl(heroveringen),sg(herovering)],de,[]).

n([mass(heroïne)],de,[]).

n([mass(herrie)],de,[]).

n([pl(herrieschoppers),sg(herrieschopper)],de,[]).

n([pl(herrieschopsters),sg(herrieschopster)],de,[]).

n([mass(herpes)],both,[]).

n([mass([herpes,simplex])],both,[]).

n([sg([herpes,simplex,virus])],het,[]).

n([pl(herplaatsingen),sg(herplaatsing)],de,[]).

n([pl(herschikkingen),sg(herschikking)],de,[]).

n([pl(hersenbloedingen),sg(hersenbloeding)],de,[]).

n([pl(hersenen)],de,[]).

n([pl(hersenpannen),sg(hersenpan)],de,[]).

n([pl(hersens)],de,[]).

n([pl(hersenschimmen),sg(hersenschim)],de,[]).

n([pl(hersenschuddingen),sg(hersenschudding)],de,[]).

n([pl(hersenspoelingen),sg(hersenspoeling)],de,[]).

n([mass(herstel)],het,[],
  [koers,
   s(recht),
   winst]).

n([sg(herstelling),pl(herstellingen)],de,[]).

n([pl(herstructureringen),sg(herstructurering)],de,[]).

n([pl(herten),sg(hert)],het,[]).

n([pl(hertogen),sg(hertog)],de,[],[aarts]).

n([sg(hertogdom),pl(hertogdommen)],het,[],[aarts]).

n([pl(hertoginnen),sg(hertogin)],de,[],[aarts]).

%% VL: recidive?
n([mass(herval)],de,[]).

n([pl(hervattingen),sg(hervatting)],de,[]).

n([pl(herverzekeraars),sg(herverzekeraar)],de,[]).

n([pl(hervestigingen),sg(hervestiging)],de,[]).

n([pl(hervormers),sg(hervormer)],de,[]).

n([pl(hervormingen),sg(hervorming)],de,[],
  [land,
   politie,
   s(staat)]).

n([pl(herwaarderingen),sg(herwaardering)],de,[]).

n([pl(herzieningen),sg(herziening)],de,[sbar]).

n([pl(herzieningen),sg(herziening)],de,[],
  [belasting,
   i(grond_wet,grondwets),
   stelsel]).

n([pl(hessen),sg(hes)],de,[]).

n([pl(hespen),sg(hesp)],de,[]).

n([sg(hetero),pl('hetero\'s')],de,[]).

n([sg(heteroseksueel),pl(heteroseksuelen),
   sg(heterosexueel),pl(heterosexuelen)],de,[]).

n([pl(hetzes),sg(hetze)],de,[]).

n([mass(heul)],both,[]).  % ouderwets (hulp), "heul en troost"

n([pl(heupen),sg(heup)],de,[]).

n([pl(heuvelen),pl(heuvels),sg(heuvel)],de,[],
  [vlucht,
   dim(heuveltje)]).

n([sg(hevel),pl(hevels)],de,[]).

n([mass(hevigheid)],de,[]).

n([meas(hPa)],both,[meas_mod,measure]).

n([meas(hertz)],both,[meas_mod,measure],
  [kilo,
   mega]).

n([pl(hiaten),sg(hiaat)],both,[]).

n([pl(hielen),sg(hiel)],de,[],[dim(hieltje)]).

n([mass(hiernamaals)],het,[]).

n([sg([high,school]),pl([high,schools])],de,[]).

n([sg([high,tea]),pl([high,teas])],de,[]).

n([stem(hightech),
   mass([high,tech]),mass('high-tech'),mass(hightech),mass('hi-tech'),
   pl([high,techs]),pl('high-techs'),pl(hightechs),pl('hi-techs')],de,[]).

n([sg(hijs)],de,[]).

n([mass(hik)],de,[]).

n([mass(hilariteit)],de,[]).

n([mass(hinder)],de,[],
  [s(geluid),geluid,
   s(verkeer)]).

n([pl(hinderlagen),sg(hinderlaag)],de,[]).

n([pl(hindernissen),sg(hindernis)],de,[]).

n([pl(hinderpalen),sg(hinderpaal)],de,[]).

n([pl(hindoes),sg(hindoe)],de,[]).

n([mass(hindoeïsme)],het,[]).

n([pl(hindoestanen),sg(hindoestaan)],de,[]).

n([pl(hints),sg(hint)],de,[sbar,vp]).

n([mass([hip,hop]),mass(hiphop)],de,[]).

n([pl(hippies),sg(hippie)],de,[]).

n([pl('hippy\'s'),sg(hippy)],de,[]).

n([pl(historici),sg(historicus)],de,[],[kunst]).

n([pl(histories),pl(historiën),sg(historie)],de,[],
  [club,
   kunst,
   dim(historietje)]).

n([mass(historisme)],het,[]).

n([pl(hits),sg(hit)],de,[],[zomer]).

n([mass(hitte)],de,[]).

n([stem(hiv),
   sg('Hiv'),sg(hiv)],both,[]).

n([pl(hiërarchieën),sg(hiërarchie)],de,[]).

n([pl(hobbels),sg(hobbel)],de,[]).

n([pl('hobby\'s'),sg(hobby)],de,[]).

n([mass(hockey)],both,[],
  [ijs,
   zaal]).

n([sg(hockeyer),pl(hockeyers)],de,[],[ijs,zaal]).

n([sg(hockeyster),pl(hockeysters)],de,[],[ijs,zaal]).

n([mass([hocus,pocus])],both,[]).

n([mass(hoe)],het,[]).

n([pl(hoeden),sg(hoed)],de,[],
  [vinger,
   dim(hoedje)]).

n([pl(hoedanigheden),sg(hoedanigheid)],de,[sbar,vp]).

n([mass(hoede)],de,[pred_pp(onder),
                    pred_pp(op)]).

n([sg(hoeder),pl(hoeders)],de,[]).

n([mass(hoedna)],de,[]).

n([pl(hoeven),sg(hoef)],de,[]).

n([pl(hoefijzers),sg(hoefijzer)],het,[]).

n([pl(hoeken),sg(hoek)],de,[],
  [beneden,
   i(link_beneden,linkerbeneden),
   i(recht_beneden,rechterbeneden),
   boven,
   i(link_boven,linkerboven),
   i(recht_boven,rechterboven),
   drie,
   vier,
   vijf,
   zes,
   zeven,
   zit,
   acht,
   s(gezicht),
   s(inval),
   mond,
   oog,
   recht,
   schuil,
   straat,
   verdom,
   dim(hoekje)]).

n([pl(hoekschoppen),sg(hoekschop)],de,[]).

n([pl(hoekstenen),sg(hoeksteen)],de,[]).

n([pl(hoektanden),sg(hoektand)],de,[]).

n([sg(hoen),pl(hoenders),pl(hoenderen)],both,[],[dim(hoentje)]).

n([pl(hoepels),sg(hoepel)],de,[],[dim(hoepeltje)]).

n([pl(hoeren),sg(hoer)],de,[],[dim(hoertje)]).

%% hoera roepen
n([mass(hoera)],both,[]).

n([pl(hoezen),sg(hoes)],de,[],[dim(hoesje)]).

n([mass(hoest)],de,[]).

n([pl(hoeven),sg(hoeve)],de,[]).

n([pl(hoeveelheden),sg(hoeveelheid)],de,
  [measure]).

n([pl(hoeveelheden),sg(hoeveelheid)],de,[],
  [geld]).

n([pl(hoven),sg(hof)],both,[],
  [dim(hofje)]).

n([pl(hoven),sg(hof)],both,[],
  [straf]).

n([pl(hofdames),sg(hofdame)],de,[]).

n([pl(hoffelijkheden),sg(hoffelijkheid)],de,[vp]).

n([pl(hofhoudingen),sg(hofhouding)],de,[]).

n([pl(hofmeesters),sg(hofmeester)],de,[]).

n([pl(hogepriesters),sg(hogepriester)],de,[]).

n([mass(hogerhand)],de,[]).

n([sg(hogeschool),pl(hogescholen)],de,[]).

n([pl(hokken),sg(hok)],het,[measure],
  [dim(hokje),
   materiaal,
   tussen]).

n([pl(hokken),sg(hok)],het,[],
  [i(kip,kippen),
   i(hond,honden),
   stem,
   tussen,
   dim(hokje)]).

n([sg(holding),pl(holdings)],de,[]).

n([pl(holen),sg(hol)],both,[],[dim(holletje)]).

n([pl(holes),sg(hole)],de,[]).

n([sg([hole,in,one])],de,[]).

n([sg(holebi), pl('holebi\'s')],both,[]).

n([sg(holler),pl(hollers)],de,[]).

n([stem('Holocaust'),sg('Holocaust'),sg(holocaust)],de,[]).

n([sg(holotype),pl(holotypen),pl(holotypes)],het,[]).

n([sg(holst)],het,[]).  % van de nacht

n([pl(holsters),sg(holster)],both,[]).

n([pl(holten),pl(holtes),sg(holte)],de,[],
  [bij,
   buik]).

n([pl(homes),sg(home)],het,[]).

n([sg(homeopaat),pl(homeopaten)],de,[]).

n([mass(homeopathie),mass(homeopatie)],de,[]).

n([stem(homepage),
   sg([home,page]),pl([home,pages]),
   sg('home-page'),pl('home-pages'),
   sg(homepage),pl(homepages)],de,[]).

n([pl(hommages),sg(hommage)],de,[]).

n([sg(hommel),pl(hommels)],de,[]).

n([pl(homines),pl('homo\'s'),sg(homo)],de,[]).

n([sg([homo,economicus])],de,[]).

n([sg([homo,erectus])],de,[]).

n([sg([homo,ludens])],de,[]).

n([sg([homo,novus])],de,[]).

n([sg([homo,sapiens,sapiens])],de,[]).

n([sg([homo,sapiens])],de,[]).

n([sg([homo,universalis])],de,[]).

n([mass(homoeopathie)],de,[]).

n([mass(homoeopatie)],de,[]).

n([pl(homofielen),sg(homofiel)],de,[]).

n([mass(homogeniteit)],de,[]).

n([mass(homoseksualiteit),mass(homosexualiteit)],de,[]).

n([pl(homoseksuelen),sg(homoseksueel),
   pl(homosexuelen),sg(homosexueel)],de,[]).

n([pl(hompen),sg(homp)],de,[measure]).

%% in de cervelaat
n([pl(honden),sg(hond)],de,[],
  [s(redding),
   speur,
   dim(hondje)]).

n([stem(honderd),pl(honderden)],de,[measure]). 

n([stem(honderd_duizend),pl(honderdduizenden)],de,[measure]). 

n([mass(hondsdolheid)],de,[]).

n([mass(honger)],de,[]).

n([stem(honger_nood),
   mass(hongersnood)],de,[]).

n([stem(honger_staking),
   pl(hongerstakingen),sg(hongerstaking)],de,[pred_pp(in)]).

n([mass(hongerwinter),pl(hongerwinters),sg(hongerwinter)],de,[]).

n([mass(honing)],de,[]).

n([sg(honingraat),pl(honingraten)],de,[]).

n([sg(honk),pl(honken)],het,[]).

n([sg(honkbal),pl(honkballen)],both,[]).

n([sg(honkballer),pl(honkballers)],de,[]).

n([sg(honneur),pl(honneurs)],de,[]).

n([pl(honoraria),pl(honorariums),sg(honorarium)],het,[]).

n([mass(honorering),pl(honoreringen),sg(honorering)],de,[]).

n([pl(hoofden),sg(hoofd)],het,[app_measure],[dim(hoofdje)]).  % Hoofd opleidingen 

n([pl(hoofden),sg(hoofd)],het,[app_measure],
  [s(afdeling),
   reeks,
   school]).

n([pl(hoofdbrekens)],both,[]).

n([pl(hoofddeksels),sg(hoofddeksel)],het,[]).

n([pl(hoofdeinden),sg(hoofdeinde)],het,[]).

n([sg(hoofdklasser),pl(hoofdklassers)],de,[]).

n([pl(hoofdknikken),sg(hoofdknik)],de,[],[dim(hoofdknikje)]).

n([pl(hoofdkussens),sg(hoofdkussen)],het,[]).

n([pl(hoofdkwartieren),sg(hoofdkwartier)],het,[]).

n([pl(hoofdlieden),pl(hoofdmannen),sg(hoofdman)],de,[]).

n([pl(hoofdsteden),sg(hoofdstad)],de,[]).

n([pl(hoofdstukken),sg(hoofdstuk)],het,[app_measure,
					start_app_measure]).

n([sg(hoog)],het,[]). % hoge-drukgebied

n([mass(hoogachting)],de,[]).

n([mass(hoogbouw)],de,[]).

n([pl(hooggerechtshoven),sg(hooggerechtshof)],het,[]).

n([pl(hoogheden),sg(hoogheid)],de,[]).

n([sg(['Zijne','Koninklijke','Hoogheid']),
   sg([zijne,koninklijke,hoogheid])],de,[np_app_measure]).   % zijne koninklijke hoogheid de Prins van Oranje

n([sg(['Zijne','Koninklijke',en,'Keizerlijke','Hoogheid']),
   sg([zijne,koninklijke,en,keizerlijke,hoogheid])],de,[np_app_measure]).  

n([sg(['Hare','Koninklijke','Hoogheid']),
   sg([hare,koninklijke,hoogheid])],de,[np_app_measure]).   

n([sg(['Hare','Koninklijke',en,'Keizerlijke','Hoogheid']),
   sg([hare,koninklijke,en,keizerlijke,hoogheid])],de,[np_app_measure]).   

n([pl(hooglanden),sg(hoogland)],het,[]).

n([pl(hoogleraars),pl(hoogleraren),sg(hoogleraar)],de,[app_measure],
  [h(emeritus)]).

n([pl(hoogmissen),sg(hoogmis)],de,[]).

n([mass(hoogmoed)],de,[]).

n([stem(hoogstand_DIM),
   sg(hoogstandje),pl(hoogstandjes)],het,[]).

n([sg(hoogspringer),pl(hoogspringers)],de,[],[polsstok]).

n([sg(hoogspringster),pl(hoogspringsters)],de,[],[polsstok]).

n([pl(hoogten),pl(hoogtes),sg(hoogte)],de,[],
  [record,
   dim(hoogtetje)]).

n([mass(hoogtij)],both,[]).

n([pl(hoogvlakten),pl(hoogvlaktes),sg(hoogvlakte)],de,[]).

n([pl(hoogwaardigheidsbekleders),sg(hoogwaardigheidsbekleder)],de,[]).

n([mass(hooi)],het,[]).

n([pl(hooibergen),sg(hooiberg)],de,[]).

n([mass(hooibroei)],de,[]).

n([mass(hooikoorts)],de,[]).

n([sg(hooligan),pl(hooligans)],de,[]).

n([mass(hoon)],de,[]).

n([mass(hoop)],de,
  [sbar,
   pred_pp(in,sbar),            % hij verkeerde in de hoop ...
   pred_pp(in,vp),              % hij verkeerde in de hoop ...
   pred_pp(in,subject_sbar),    % het was in de hoop ... dat ..
   subject_sbar,
   subject_vp,
   vp]).

n([pl(hopen),sg(hoop)],de,[measure],
  [dim(hoopje)]).

n([pl(hopen),sg(hoop)],de,[],
  [compost,
   schroot,
   zand,
   dim(hoopje)]).

n([sg([hoor,en,wederhoor])],het,[]).

n([pl(hoorders),sg(hoorder)],de,[]).

n([pl(hoornen),pl(hoorns),sg(hoorn)],de,[measure],[dim(hoorntje)]).

n([mass(hoorngeschal)],het,[]).

n([pl(hoorns),sg(hoorn)],de,[],
  [basset,
   dim(hoorntje)]).

n([sg(hoorspel),pl(hoorspelen)],het,[]).

n([pl(hoorzittingen),sg(hoorzitting)],de,[]).

n([pl(hoppen),sg(hop)],de,[]).

n([pl(hoplieden),pl(hopmans),sg(hopman)],de,[]).

n([pl(hoppers),sg(hopper)],de,[]).

n([pl(horden),pl(hordes),sg(horde)],de,[measure]).

n([sg(horden)],het,[]).

n([sg(hordenlopen)],het,[]).

n([mass(horeca)],de,[]).

n([pl(horens),sg(horen)],de,[measure],[dim(horentje)]).

n([pl(horigen),sg(horige)],de,[]).

n([pl(horizonnen),pl(horizons),sg(horizon)],de,[]).

n([pl(horizonten),sg(horizont)],de,[]).

n([pl(horloges),sg(horloge)],het,[],[dim(horlogetje)]).

n([pl(horlogemakers),sg(horlogemaker)],de,[]).

n([pl(hormonen),sg(hormoon)],het,
  [app_measure],
  [s(geslacht),
   groei]).

n([pl(horoscopen),sg(horoscoop)],de,[]).

n([pl(horoskopen),sg(horoskoop)],de,[]).

n([mass(horror)],de,[]).

n([sg([hors,'d\'oeuvre']),pl([hors,'d\'oeuvres'])],de,[]).

n([mass(horse)],de,[]).

n([pl(horsten),sg(horst)],de,[]).

n([pl(horten),sg(hort)],de,[]).

n([pl(horzels),sg(horzel)],de,[],[dim(horzeltje)]).

n([pl('hospita\'s'),sg(hospita)],de,[]).

n([pl(hospitalen),sg(hospitaal)],het,[],[dim(hospitaaltje)]).

n([sg(host),pl(hosts)],de,[]).

n([sg(hostess),pl(hostesses)],de,[]).

n([pl(hosties),pl(hostiën),sg(hostie)],de,[]).

n([sg([hot,dog]),pl([hot,dogs])],de,[]).

n([sg([hot,issue]),pl([hot,issues])],both,[]).

n([sg([hot,item]),pl([hot,items])],both,[]).

n([sg([hot,line]),pl([hot,lines])],de,[]).

n([pl([hot,pants])],de,[]).

n([sg([hot,shot]),pl([hot,shots])],de,[]).

n([sg([hot,spot]),pl([hot,spots])],de,[]).

n([sg([hot,topic]),pl([hot,topics])],de,[]).

n([pl(hotels),sg(hotel)],het,[np_app_measure],
  [dim(hotelletje)]).

n([pl(hotels),sg(hotel)],het,[np_app_measure],
  [i(speler,spelers),
   dim(hotelletje)]).

n([mass(houdbaarheid)],de,[]).

n([pl(houders),sg(houder)],de,[],
  [bank,
   beker,
   bordeel,
   buste,
   café,
   galerie,
   i(giro_rekening,girorekening),
   melkvee,
   obligatie,
   pluimvee,
   polis,
   pomp,
   portefeuille,
   praktijk,
   rekening,
   restaurant,
   stal,
   stand,
   status,
   sterk,
   s(varken),
   vee]).

n([sg(houderij),pl(houderijen)],de,[],
  [melkvee,
   pluimvee,
   stal,
   s(varken),
   vee]).

n([pl(houdingen),sg(houding)],de,
  [sbar,
   van_sbar]).

n([pl(houdingen),sg(houding)],de,[],
  [zit]).

n([pl(houdsters),sg(houdster)],de,[]).

n([mass(house)],de,[]).

n([sg([house,party]),pl([house,parties]),pl([house,'party\'s']),
   sg('house-party'),pl('house-parties'),pl('house-party\'s'),
   sg(houseparty),   pl(houseparties),   pl('houseparty\'s')],de,[]).

n([pl(houten),sg(hout)],het,[],[dim(houtje)]).

n([pl(houthakkers),sg(houthakker)],de,[]).

n([pl(houtskolen),sg(houtskool)],both,[]).

n([pl(houtvesters),sg(houtvester)],de,[]).

n([pl(houvasten),sg(houvast)],both,[sbar,subject_sbar]).

n([sg(houw),pl(houwen)],de,[]). % van een zwaard

n([pl(hovelingen),sg(hoveling)],de,[]).

n([sg(howitzer),pl(howitzers)],de,[]).

n([mass(hts)],both,[]).

n([pl(hufters),sg(hufter)],de,[]).

n([pl(hugenoten),sg(hugenoot)],de,[]).

n([pl(huichelaars),sg(huichelaar)],de,[]).

n([mass(huichelarij)],de,[]).

n([pl(huiden),sg(huid)],de,[],
  [donor,
   hoofd,
   opper]).

n([pl(huifkarren),sg(huifkar)],de,[]).

n([pl(huizen),sg(huis)],het,[np_app_measure],
  [bel,
   i(blijf_van_mijn_lijf,'blijf-van-mijn-lijf-'),
   i(boot,boten),
   buurt,			% en niet buur_thuis
   club,
   s(dorp),
   droom,
   i(effect,effecten),
   s(gebed),
   geboorte,
   s(gemeenschap),
   huur,
   jeugd,
   i(kaart,kaarten),
   koffie,
   i(pop,poppen),i(pop,poppe),
   productie,
   provincie,
   i(rij_DIM,rijtjes),
   rust,
   software,
   sterf,
   i(student,studenten),
   thee,
   tuin,
   tussen,
   vakantie,
   verpleeg,
   s(verzorging),
   voor,
   i(vorst,vorsten),
   wijn,
   dim(huisje)]).

n([sg(huize)],both,[pred_pp(van)]). % van goede huize

n([pl(huisbazen),sg(huisbaas)],de,[]).

n([pl(huisdieren),sg(huisdier)],het,[]).

n([pl(huisgezinnen),sg(huisgezin)],het,[]).

n([pl(huishoudens),sg(huishouden)],het,[],[dim(huishoudentje)]).

n([pl(huishoudingen),sg(huishouding)],de,[],[water,
					     dim(huishoudinkje)]).

n([pl(huishoudsters),sg(huishoudster)],de,[],[dim(huishoudstertje)]).

n([mass(huisraad)],both,[]).

n([mass(huisvesting)],de,[],
  [i(oud,ouderen)]).

n([pl(huisvrouwen),sg(huisvrouw)],de,[]).

n([mass(huisvuil)],het,[]).

n([pl(huiszoekingen),sg(huiszoeking)],de,[]).

n([mass(huiver)],de,[]).

n([pl(huiveringen),sg(huivering)],de,[vp]).

n([mass(huize)],het,[pred_pp(van)]).

n([mass(hulde)],de,[]).

n([pl(huldigingen),sg(huldiging)],de,[]).

n([pl(hulpen),sg(hulp)],de,[],
  ['Marshall',h('Marshall'),
   nood,
   s(ontwikkeling),
   s(recht),
   voedsel,
   dim(hulpje)]).

n([pl(hulpen),sg(hulp)],de,[pred_pp(tot)],
  []).

n([pl(hulpbronnen),sg(hulpbron)],de,[]).

n([mass(hulpeloosheid)],de,[]).

n([stem(hulp_goed),
   pl(hulpgoederen)],het,[]).

n([pl(hulzen),sg(huls)],de,[]).

n([pl(hulsten),sg(hulst)],de,[]).

n([pl([human,resources])],de,[]).

n([sg(hum)],both,[pred_pp(uit)]).

n([mass(humanisme)],het,[]).

n([pl(humanisten),sg(humanist)],de,[]).

n([mass(humaniteit)],de,[]).

n([pl(humeuren),sg(humeur)],het,[vp],[dim(humeurtje)]).

n([pl(humeuren),sg(humeur)],het,
  [pred_pp(in),
   pred_pp(uit)],
  [rot]).

n([pl(hummels),sg(hummel)],de,[],[dim(hummeltje)]).

n([mass(humor)],de,[]).

n([sg(hunebed),pl(hunebedden)],het,[]).

n([mass(hunker)],de,[vp]).

n([pl(hunkeringen),sg(hunkering)],de,[vp]).

n([sg(hunne),pl(hunnen)],both,[]).  % de hunnen/Hunnen winnen keer op keer

n([pl(hutten),sg(hut)],de,[],
  [berg,
   blok,
   dim(hutje)]).

n([pl(huren),sg(huur)],de,[],
  [kamer,
   onder
  ]).

n([pl(hurken)],de,[]).

n([pl(huurders),sg(huurder)],de,[]).

n([pl(huursters),sg(huurster)],de,[]).

n([sg(huurwaardeforfait)],het,[]).

n([pl(huurlingen),sg(huurling)],de,[]).

n([pl(huwelijken),sg(huwelijk)],het,[],
  [homo,
   schijn]).

n([pl(huwelijksgemeenschappen),sg(huwelijksgemeenschap)],de,[]).

n([mass(huwelijksleven)],het,[]).

n([pl(huwelijksnachten),sg(huwelijksnacht)],de,[]).

n([pl(huzaren),sg(huzaar)],de,[]).

n([pl(hybriden),pl(hybrides),sg(hybride)],de,[]).

n([pl('hyena\'s'),sg(hyena)],de,[]).

n([mass(hygiëne)],de,[]).

n([pl(hymnen),pl(hymnes),sg(hymne)],de,[]).

n([sg(hype),pl(hypes)],de,[]).

n([mass(hypertensie)],de,[]).

n([mass(hypnose)],de,[]).

n([pl(hypnotiseurs),sg(hypnotiseur)],de,[]).

n([pl(hypnotizeurs),sg(hypnotizeur)],de,[]).

n([mass(hypocrisie)],de,[]).

n([sg(hypocriet),pl(hypocrieten)],de,[]).

n([pl(hypofysen),pl(hypofyses),sg(hypofyse)],de,[]).

n([pl(hypotesen),pl(hypoteses),sg(hypotese)],de,[sbar]).

n([pl(hypotheken),sg(hypotheek)],de,[],
  [s(belegging)]).

n([pl(hypothesen),pl(hypotheses),sg(hypothese)],de,[sbar]).

n([mass(hysterie)],de,[]).

n([pl('i\'s'),sg(i)],de,[],[dim('i\'tje')]).

n([mass('i-mode')],both,[]).

n([pl(iconen),sg(icoon)],both,[],[sport]).

n([pl(idealen),sg(ideaal)],het,
  [sbar,
   subject_vp,
   subject_sbar,
   vp]).

n([pl('ideaal-beelden'),sg('ideaal-beeld')],het,[sbar]).

n([mass(idealisme)],het,[]).

n([pl(idealisten),sg(idealist)],de,[]).

n([pl(idealistes),sg(idealiste)],de,[]).

n([pl(idees),pl(ideeën),sg(idee)],both,[sbar,
					van_sbar,
                                        app_measure,
					subject_sbar,
					subject_vp,
					vp],
  [basis,
   hoofd,
   dim(ideetje)]).

n([pl(identificaties),sg(identificatie)],de,[]).

n([pl(identifikaties),sg(identifikatie)],de,[]).

n([sg(identiteit),pl(identiteiten)],de,[]).

n([pl(identiteitsbewijzen),sg(identiteitsbewijs)],het,[]).

n([pl(ideologieën),sg(ideologie)],de,[sbar]).

n([pl(idiomen),sg(idioom)],het,[]).

n([pl(idioten),sg(idioot)],de,[]).

n([pl(idolen),sg(idool)],het,[],
  [pop]).

n([pl(idyllen),pl(idylles),sg(idylle)],de,[]).

n([sg(iemand),pl(iemanden)],de,[]).

n([sg(iep),pl(iepen)],de,[]).

n([mass(iets)],het,[meas_mod]).

n([sg(ietsje)],het,[mod,measure]).

n([pl('ij\'s'),sg(ij)],de,[],[dim('ij\'tje')]).

n([pl(ijdelheden),sg(ijdelheid)],de,[subject_sbar]).

n([mass(ijs)],het,[],
  [kunst,
   natuur,
   room,
   water,
   zee]).

n([sg(ijsbaan),pl(ijsbanen)],de,[]).

n([pl(ijsberen),sg(ijsbeer)],de,[]).

n([pl(ijsbergen),sg(ijsberg)],de,[]).

n([pl(ijsblokjes),sg(ijsblokje)],het,[]).

n([stem(ijs_DIM),pl(ijsjes),sg(ijsje)],het,[]).

n([pl(ijskappen),sg(ijskap)],de,[]).

n([pl(ijskasten),sg(ijskast)],de,[]).

n([mass(ijver)],de,[vp]).

n([mass(ijzel)],de,[]).

n([pl(ijzers),sg(ijzer)],het,[],[dim(ijzertje)]).

n([mass(ijzerdraad)],het,[]).

n([stem(ik_figuur),
   pl('ik-figuren'),sg('ik-figuur'),
   pl(ikfiguren),sg(ikfiguur)],de,[]).

n([pl(ikonen),sg(ikoon)],de,[]).

n([pl(illegalen),sg(illegaal)],de,[]).

n([pl(illegaliteiten),sg(illegaliteit)],de,[]).

n([pl(illusies),sg(illusie)],de,
  [sbar,
   subject_sbar,
   subject_vp,
   vp]).

n([pl(illustraties),sg(illustratie)],de,[sbar]).
%% een duidelijke illustratie, dat ...

n([pl(illustratoren),pl(illustrators),sg(illustrator)],de,[]).

n([pl(illuzies),sg(illuzie)],de,[sbar,vp]).

n([sg('iMac'),pl('iMacs')],de,[]).

n([pl(images),sg(image)],both,[]).

n([pl('imago\'s'),sg(imago)],het,[sbar]).

n([pl(imams),sg(imam)],de,[]).

n([pl(imbecielen),sg(imbeciel)],de,[]).

n([sg(imker),pl(imkers)],de,[]).

n([pl(imitaties),sg(imitatie)],de,[]).

n([pl(immigranten),sg(immigrant)],de,[]).

n([pl(immigrantes),sg(immigrante)],de,[]).

n([pl(immigraties),sg(immigratie)],de,[]).

n([sg(immunisatie),pl(immunisaties)],de,[]).

n([pl(immuniteiten),sg(immuniteit)],de,[]).

n([pl(imps)],both,[]).  % punten in bridge

n([pl(impacts),sg(impact)],de,[]).

n([pl(impassen),pl(impasses),sg(impasse)],de,[]).

n([mass(imperialisme)],het,[]).

n([pl(imperia),pl(imperiums),sg(imperium)],het,[]).

n([sg(imperialist),pl(imperialisten)],de,[]).  % not imperium_list

n([sg(implantaat),pl(implantaten)],het,[],[borst]).

n([sg(implementatie),pl(implementaties)],de,[]).

n([pl(implicaties),sg(implicatie)],de,[]).

n([pl(importen),sg(import)],de,[],[parallel]).

n([pl(importeurs),sg(importeur)],de,[]).

n([mass(impotentie)],de,[]).

n([pl('impresario\'s'),sg(impresario)],de,[]).

n([pl(impressies),sg(impressie)],de,[]).

n([mass(impressionisme)],het,[]).

n([pl(improvisaties),sg(improvisatie)],de,[],[dim(improvisatietje)]).

n([pl(impulsen),sg(impuls)],de,[vp]).

n([sg([in,memoriam]),pl([in,memoriams])],het,[]).

n([mass(inachtneming)],de,[]).

n([mass(inademing)],de,[]).

n([sg(inbedding),pl(inbeddingen)],de,[]).

n([mass(inbegrip)],het,[]).

n([sg(inbeslagname),pl(inbeslagnames),
   sg([in,beslagname]),pl([in,beslagnames])],de,[]).

n([pl(inboedels),sg(inboedel)],de,[]).

n([pl(inboorlingen),sg(inboorling)],de,[]).

n([sg(inborst),pl(inborsten)],de,[]).

n([mass(inbouw)],de,[]).

n([sg(inbox)],de,[]).

n([pl(inbraken),sg(inbraak)],de,[],[dim(inbraakje)]).

n([pl(inbrekers),sg(inbreker)],de,[]).

n([mass(inbreng),pl(inbrengen),sg(inbreng)],de,[]).

n([pl(inbreuken),sg(inbreuk)],de,[]).

n([mass(inburgering)],de,[]).

n([pl(incarnaties),sg(incarnatie)],de,[]).

n([mass(incest)],de,[]).

n([meas(inch),pl(inches)],de,[meas_mod,measure]).

n([sg(incheck)],de,[]).

n([pl(incidenten),sg(incident)],het,[],
  [grens,
   schiet]).

n([mass(incontinentie)],de,[],
  [stress,h(stress),
   urine,h(urine),
   i(stress_urine,stressurine),i(stress_urine,'stressurine-')
  ]).

n([pl(inconveniënten),sg(inconveniënt)],het,[]).

n([pl(indelingen),sg(indeling)],de,[],
  [stoel,
   dim(indelinkje)]).

n([pl(indexen),pl(indices),sg(index)],de,[app_measure],
  [beurs,
   hoofd,
   prijs,
   wh([purchasing,manager]),f([purchasing,manager]),
   h(technologie)]).

n([pl([index,of,leading,indicators])],de,[]).

n([sg(indexatie),pl(indexaties)],de,[]).

n([sg(indexering),pl(indexeringen)],de,[]).

n([pl(indicaties),sg(indicatie)],de,[sbar,vp]).

n([sg(indicator),pl(indicatoren)],de,[]).

n([pl(indieners),sg(indiener)],de,[]).

n([mass(indiening)],de,[]).

n([mass(indienstname)],de,[]).

n([mass(indigo)],het,[]).

n([pl(indikaties),sg(indikatie)],de,[sbar,vp]).

n([pl('individu\'s'),pl(individuen),sg(individu)],both,[]). % het

n([mass(individualisering)],de,[]).

n([mass(individualisme)],het,[]).

n([pl(individualisten),sg(individualist)],de,[]).

n([mass(individualiteit)],de,[]).

n([mass(individualisering)],de,[]).

n([pl(indoctrinaties),sg(indoctrinatie)],de,[sbar]).

n([mass(indool)],both,[]).

n([sg(indoor),pl(indoors)],de,[]).

n([pl(indringers),sg(indringer)],de,[]).

n([pl(indrukken),sg(indruk)],de,[vp,
                                 sbar,
                                 subject_sbar]).

n([pl(inducties),sg(inductie)],de,[]).

n([pl(industrialisatiën),sg(industrialisatie),
   pl(industrializatiën),sg(industrializatie)],de,[]).

n([pl(industrieën),sg(industrie)],de,[],
  [auto,
   h(auto),
   automobiel,
   bio,h(bio),
   chip,
   film,
   h(defensie),
   leem,
   luchtvaart,
   metaal,
   muziek,
   h(olie),
   i(plaat,platen),
   staal,
   s(tabak),
   textiel,
   i(toerist,toeristen),
   vliegtuig,
   s(voeding)]).

n([pl(industriëlen),sg(industrieel)],de,[]).

n([pl(ineenstortingen),sg(ineenstorting)],de,[]).

n([pl(inentingen),sg(inenting)],de,[]).

n([mass(infanterie)],de,[]).

n([sg([infanterie,divisie]),
   pl([infanterie,divisies]),
   sg(['inf.','div.']),
   sg(['inf.',divisie])],de,[]).

n([pl(infanteristen),sg(infanterist)],de,[]).

n([sg(infarct),pl(infarcten)],het,[],
  [hart,
   hersen]).

n([pl(infecties),sg(infectie),pl(infekties),sg(infektie)],de,[],
  [virus]).

n([sg(infiltrant),pl(infiltranten)],de,[]).

n([pl(infiltraties),sg(infiltratie)],de,[]).

n([sg(infinitief),pl(infinitieven)],de,[]).

n([pl(inflaties),sg(inflatie)],de,[]).

n([sg([inflight,magazine]),
   pl([inflight,magazines])],de,[]).

n([mass(info)],de,[]).

n([sg(infolijn),pl(infolijnen)],de,[]).

n([pl(informanten),sg(informant)],de,[]).

n([pl(informateurs),sg(informateur)],de,[]).

n([mass(informatica)],de,[]).

n([sg(informaticus),pl(informatici)],de,[]).

n([pl(informaties),sg(informatie)],de,[sbar,vp]).

n([sg(informatie)],de,[],
  [achtergrond,
   koers,
   reis,
   trein,
   s(verkeer),
   vlucht]).

n([mass(informatiekunde)],de,[]).

n([mass([information,retrieval])],de,[]).

n([mass(infrarood)],het,[]).

n([mass(infrastructuur),pl(infrastructuren),sg(infrastructuur)],de,[]).

n([sg(infusie),pl(infusies)],de,[]).

n([mass(infuus)],het,[]).

n([pl(ingangen),sg(ingang)],de,[],
  [hoofd,
   zij]).

n([pl(ingenieurs),sg(ingenieur)],de,
  [app_measure]).

n([pl(ingenieurs),sg(ingenieur)],de,
  [],
  []).

n([sg(ingeland),pl(ingelanden)],de,[]).

n([sg(ingenomenheid)],de,[]).

n([pl(ingevingen),sg(ingeving)],de,[vp]).

n([pl(ingewanden),sg(ingewand)],het,[]).

n([mass(ingewikkeldheid)],de,[]).

n([pl(ingezetenen),sg(ingezetene)],de,[]).

n([sg(ingooi)],de,[]).

n([pl(ingrediënten),sg(ingrediënt)],het,[]).

n([pl(ingrepen),sg(ingreep)],de,[],[dim(ingreepje)]).

n([mass(ingroei)],de,[]).

n([pl(inhaalslagen),sg(inhaalslag)],de,[]).

n([pl(inhammen),sg(inham)],de,[]).

n([pl(inhouden),sg(inhoud)],de,[],
  [cilinder,
   tank]).

n([mass(inhuur)],de,[]).

n([pl(initialen),sg(initiaal)],de,[]).

n([sg(initiatie),pl(initiaties)],de,[]).

n([pl(initiatieven),sg(initiatief)],het,[vp]).

n([pl(initiatieven),sg(initiatief)],het,[],
  [burger,
   s(vrede)]).

n([pl(initiatiefnemers),sg(initiatiefnemer)],de,[]).

n([pl(initiatiefneemsters),sg(initiatiefneemster)],de,[]).

n([pl(injecties),sg(injectie)],de,[],
  [brandstof,
   kapitaal, s(kapitaal)
  ]).

n([pl(injectiespuiten),sg(injectiespuit)],de,[]).

n([pl(injekties),sg(injektie)],de,[]).

n([pl(injektiespuiten),sg(injektiespuit)],de,[]).

n([pl(inkarnaties),sg(inkarnatie)],de,[]).

n([mass(inkeer)],de,[]).

n([mass(inkijk)],de,[],[dim(inkijkje)]).

n([sg(inkom)],de,[]). % Vlaams voor entree

n([pl(inkomens),sg(inkomen)],het,[],
  [basis,
   bruto,h(bruto),
   jaar,
   maand,
   midden,
   minimum,
   netto,h(netto),
   h(premie),
   top,
   week
  ]).

n([stem(inkomst),
   pl(inkomsten),sg(inkomst)],de,[],
  [h(advertentie),
   belasting,
   h(reclame),
   h(rente)]).

n([pl(inkonveniënten),sg(inkonveniënt)],het,[]).

n([pl(inkopen),sg(inkoop)],de,[]).

n([pl(inkrimpingen),sg(inkrimping)],de,[]).

n([pl(inkten),sg(inkt)],de,[]).

n([pl(inktvissen),sg(inktvis)],de,[]).

n([sg(inlaag),pl(inlagen)],de,[]). % gebied tussen land en zee in Zeeland?

n([mass(inlaat)],de,[]).

n([pl(inlanders),sg(inlander)],de,[]).

n([sg(inlas),pl(inlassen)],de,[]).

n([pl(inleggen),sg(inleg)],de,[]).

n([pl(inleidingen),sg(inleiding)],de,[]).

n([mass(inlek)],de,[]).

n([pl(inleveringen),sg(inlevering)],de,[]).

n([pl(inlichtingen),sg(inlichting)],de,[]).

n([pl(inlijvingen),sg(inlijving)],de,[]).

n([sg(inlog),pl(inlogs)],de,[]).

n([sg(inloop),pl(inlopen)],de,[]).

n([pl(inmengingen),sg(inmenging)],de,[]).

n([sg(inname),pl(innames),pl(innamen)],de,[]).

n([mass(innerlijk)],het,[]).

n([pl(inningen),sg(inning),pl(innings)],de,[]).

n([pl(innovaties),sg(innovatie)],de,[]).

n([sg(innovator),pl(innovatoren),pl(innovators)],de,[]).

n([sg(inpakker),pl(inpakkers)],de,[]).

n([sg(inpakster),pl(inpaksters)],de,[]).

n([pl(inperkingen),sg(inperking)],de,[]).

n([sg(inpoldering),pl(inpolderingen)],de,[]).

n([sg(inplant)],de,[]).

n([mass(input),pl(inputs)],de,[]).

n([pl(inquisiteurs),sg(inquisiteur)],de,[]).

n([mass(inquisitie)],de,[]).

n([sg(inreis)],de,[]).

n([sg(inrichter),pl(inrichters)],de,[],
  [kantoor]).

n([pl(inrichtingen),sg(inrichting)],de,[],
  [her,
   jeugd,
   straf,
   werkplek,
   zenuw]).

n([mass(inruil)],de,[]).

n([pl([ins,and,outs]),pl([ins,en,outs])],de,[]).

n([pl(inschakelingen),sg(inschakeling)],de,[]).

n([sg(inschatting),pl(inschattingen)],de,[sbar,vp]).

n([sg(inscheping),pl(inschepingen)],de,[]).

n([sg(inschrijfster),pl(inschrijfsters)],de,[]).

n([sg(inschrijver),pl(inschrijvers)],de,[]).

n([pl(inschrijvingen),sg(inschrijving)],de,[]).

n([pl(inscripties),sg(inscriptie)],de,[]).

n([pl(insecten),sg(insect),
   pl(insekten),sg(insekt)],het,[]).

n([sg(inseminatie),pl(inseminaties)],de,[]).

n([pl(insiders),sg(insider)],de,[]).

n([pl(insignes),sg(insigne)],het,[]).

n([pl(insinuaties),sg(insinuatie)],de,[sbar]).

n([pl(inskripties),sg(inskriptie)],de,[]).

n([pl(inslagen),sg(inslag)],de,[sbar]).

n([pl(inslagen),sg(inslag)],de,[],[bliksem]).

n([mass(insleep)],de,[]).

n([sg(insnijding),pl(insnijdingen)],de,[]).  % niet in_snijd_ding

n([pl(inspanningen),sg(inspanning)],de,[vp]).

n([pl(inspanningen),sg(inspanning)],de,[],[s(kracht)]).

n([pl(inspecteurs),sg(inspecteur),
   pl(inspekteurs),sg(inspekteur)],de,[],
  [belasting,
   hoofd,
   wapen]).

n([pl(inspecties),pl(inspectiën),sg(inspectie)],de,
  [app_measure],
  [s(rijk)]).

n([pl(inspecties),pl(inspectiën),sg(inspectie)],de,[],
  [s(arbeid),
   onderwijs,
   wapen]).

n([pl('inspectoraten-generaal'),sg('inspectoraat-generaal')],het,[]).

n([pl(inspiraties),sg(inspiratie)],de,[vp]).

n([pl(inspiratiebronnen),sg(inspiratiebron)],de,[]).

n([mass(inspraak)],de,[]).

n([pl(inspuitingen),sg(inspuiting)],de,[]).

n([mass(instabiliteit)],de,[]).

n([sg(installateur),pl(installateurs)],de,[]).

n([pl(installaties),sg(installatie)],de,[],
  [geluid,
   muziek]).

n([mass(instandhouden),mass([in,standhouden])],het,[]).

n([mass(instandhouding)],de,[]).

n([pl(instanties),sg(instantie)],de,[vp]).

n([pl(instanties),sg(instantie)],de,[],
  [s(overheid),
   s(uitkering)]).

n([sg(instap)],de,[]).

n([mass(insteek)],de,[subject_sbar,subject_vp],[]).

n([pl(instellingen),sg(instelling)],de,[sbar,vp]).

n([pl(instellingen),sg(instelling)],de,[],
  [s(fabriek),
   krediet,
   kunst,
   onderwijs,
   s(overheid),
   s(uitvoering)]).

n([mass(instemming)],de,[]).

n([pl(instincten),sg(instinct)],het,[vp]).

n([pl(instinkten),sg(instinkt)],het,[vp]).

n([pl(instituties),sg(institutie)],de,[]).

n([pl(institutionaliseringen),sg(institutionalisering)],de,[]).

n([pl(institutionalizeringen),sg(institutionalizering)],de,[]).

n([pl(instituten),sg(instituut)],het,
  [app_measure,
   np_app_measure],
  [beeld,
   s(onderzoek),
   s(opleiding)]).

n([pl(instortingen),sg(instorting)],de,[]).

n([sg(instroom),pl(instromen)],de,[]).

n([sg(instromer),pl(instromers)],de,[],
  [h(zij)]).

n([pl(instructeurs),sg(instructeur)],de,[]).

n([pl(instructies),sg(instructie)],de,
  [vp,
   sbar,
   app_measure],
  [s(geweld)]).

n([pl(instrumenten),sg(instrument)],het,[app_measure],
  [s(beleid),
   s(financiering),
   meet,
   muziek,
   toets,
   dim(instrumentje)]).

n([pl(instrumentaria),pl(instrumentariums),sg(instrumentarium)],het,[]).

n([sg(instuif),pl(instuiven)],de,[]).

n([mass(insuline)],de,[]).

n([mass(inteelt)],de,[]).

n([sg(integer),pl(integers)],de,[]).

n([sg(integraal),pl(integralen)],de,[]).

n([pl(integraties),sg(integratie)],de,[]).

n([mass(integriteit)],de,[]).

n([pl(intellecten),sg(intellect)],het,[]).

n([pl(intellectuelen),sg(intellectueel),
   pl(intellektuelen),sg(intellektueel)],de,[]).

n([pl(intelligenties),sg(intelligentie)],de,[]).

n([pl(intelligentietests),sg(intelligentietest)],de,[]).

n([mass(intelligentsia)],de,[]).

n([pl(intendanten),sg(intendant)],de,[]).

n([mass(intensiteit),pl(intensiteiten),sg(intensiteit)],de,[]).

n([sg([intensive,care])],de,[]).

n([pl(intensiveringen),sg(intensivering)],de,[]).

n([pl(intenties),sg(intentie)],de,[vp]).

n([mass(interactie),pl(interacties),sg(interactie)],de,[]).

n([sg(interactiviteit)],de,[]).

n([stem(interbellum),
   sg(interbellum),sg('Interbellum')],het,[]).

n([sg(intercity),pl('intercity\'s')],de,[]).

n([pl(intercoms),sg(intercom)],de,[]).

n([sg(intercommunale),pl(intercommunales)],de,[]).

n([pl(interdependenties),sg(interdependentie)],de,[]).

n([pl(interesses),sg(interesse)],de,[sbar,vp]).

n([pl(interesten),sg(interest)],de,[]).

n([sg(interface),pl(interfaces)],de,[],
  [s(gebruiker)]).

n([pl(interieurs),sg(interieur)],het,[],[dim(interieurtje)]).

n([pl(interlands),sg(interland)],de,[],
  [oefen,
   voetbal]).

n([sg(interliner),pl(interliners)],de,[]).

n([sg(intermediair),pl(intermediairs)],both,[]).

n([pl(intermezzi),pl('intermezzo\'s'),sg(intermezzo)],het,[],[dim(intermezzootje)]).

n([pl(internaten),sg(internaat)],het,[]).

n([pl(internationals),sg(international)],de,[],
  [h(ex),
   hockey,
   jeugd,
   h(oud),oud,
   voetbal]).

n([sg([international,matchpoint]),pl([international,matchpoints]),
   sg([international,match,point]),pl([international,match,points])],both,[]).

n([pl(internationaliseringen),sg(internationalisering)],de,[]).

n([mass(internet)],het,[],
  [breedband,
   kabel]).

n([sg(internetaanbieder),pl(internetaanbieders)],de,[]).

n([sg(internetprovider),pl(internetproviders)],de,[]).

n([sg(internetter),pl(internetters)],de,[]).

n([pl(internisten),sg(internist)],de,[]).

n([sg(interoperabiliteit),pl(interoperabiliteiten)],de,[]).

n([sg(interpellatie),pl(interpellaties)],de,[]).

n([pl(interpretaties),sg(interpretatie)],de,[sbar]).

n([sg(interpunctie),pl(interpuncties)],de,[]).

n([sg(interruptie),pl(interrupties)],de,[]).

n([pl(intervallen),sg(interval)],het,[]).

n([pl(interventies),sg(interventie)],de,[],
  [crisis]).

n([pl(interviews),sg(interview)],het,[],
  [achtergrond % niet acht_erg_rond_interview
  ]).

n([pl(interviewers),sg(interviewer)],de,[]).

n([sg(intifada),sg(intifadah)],de,[]).

n([pl(intimi)],de,[]).  % intimus?

n([pl(intimidaties),sg(intimidatie)],de,[]).

n([pl(intimiteiten),sg(intimiteit)],de,[]).

n([pl(intochten),sg(intocht)],de,[]).

n([pl(intonaties),sg(intonatie)],de,[]).

n([sg(intrap),pl(intrappen)],de,[]).   % de intrap vervangt de ingooi

n([mass(intrede)],de,[]).

n([mass(intrek)],de,[]).

n([pl(intrekkingen),sg(intrekking)],de,[]).

n([pl(intriges),sg(intrige)],de,[]).

n([sg(intro),pl('intro\'s')],both,[]).

n([pl(introducties),sg(introductie),
   pl(introdukties),sg(introduktie)],de,[],[beurs]).

n([pl(introspecties),sg(introspectie),
   pl(introspekties),sg(introspektie)],de,[]).

n([pl(intuïties),sg(intuïtie)],de,[sbar,vp]).

n([sg(invaart)],de,[]).

n([pl(invallen),sg(inval)],de,[sbar,vp]).

n([mass(invaliditeit)],de,[]).

n([pl(invallers),sg(invaller)],de,[]).

n([pl(invasies),sg(invasie)],de,[measure]).

n([pl(inventarissen),sg(inventaris)],de,[]).

n([pl(inventarisaties),sg(inventarisatie),
   pl(inventarizaties),sg(inventarizatie)],de,[]).

n([mass(inventiviteit)],de,[]).

n([sg(inverse),pl(inversen)],de,[]).

n([pl(investeerders),sg(investeerder)],de,[]).

n([pl(investeringen),sg(investering)],de,[]).

n([sg([investment,banker]),pl([investment,bankers])],de,[]).

n([mass([investment,banking])],both,[]).

n([pl(invitaties),sg(invitatie)],de,[vp]).

n([pl(invloeden),sg(invloed),mass(invloed)],de,
  [pred_pp(van),
   pred_pp(onder)]).

n([pl(invloeden),sg(invloed),mass(invloed)],de,
  [],
  [s(seizoen)]).

n([pl(invoeren),sg(invoer)],de,[]).

n([mass(invoering)],de,[],[her]).

n([pl(invullingen),sg(invulling)],de,[sbar]).

n([sg(inwerkingtreding),
   sg([in,werking,treding])],de,[]).

n([pl(inwerkingen),sg(inwerking)],de,[]).

n([pl(inwijdingen),sg(inwijding)],de,[]).

n([sg(inwijkeling),pl(inwijkelingen)],de,[]).

n([pl(inwoners),sg(inwoner)],de,[]).

n([pl(inwonertallen),sg(inwonertal)],het,[]).

n([mass(inwoon)],de,[]).

n([mass(inwoning)],de,[]).

n([pl(inwoonsters),sg(inwoonster)],de,[]).

n([sg(inworp),pl(inworpen)],de,[]).

n([sg(inzaai)],de,[]).

n([mass(inzage)],de,[]).

n([pl(inzamelingen),sg(inzameling)],de,[]).

n([pl(inzenders),sg(inzender)],de,[]).

n([pl(inzendingen),sg(inzending)],de,[]).

n([pl(inzetten),sg(inzet)],de,[subject_sbar,subject_vp]).

n([pl(inzichten),sg(inzicht)],het,[sbar,vp]).

n([pl(inzichten),sg(inzicht)],het,[],
  [koers,
   wedstrijd]).

n([pl(inzinkingen),sg(inzinking)],de,[]).

n([sg(inzittende),pl(inzittenden)],de,[]).

n([sg(ion),pl(ionen)],both,[]).

n([sg(iPad),pl(iPads),
   sg(ipad),pl(ipads)],de,[]).

n([sg([iPad,mini]),pl([iPads,mini]),
   sg([ipad,mini]),pl([ipads,mini])],de,[]).

n([sg(iPhone),pl(iPhones),
   sg(iphone),pl(iphones)],de,[]).

n([sg(iPod),pl(iPods),
   sg(ipod),pl(ipods)],de,[]).

n([pl(irissen),sg(iris)],de,[]).

n([mass(ironie),pl(ironieën),sg(ironie)],de,[subject_sbar]).

n([pl(irrigaties),sg(irrigatie)],de,[]).

n([pl(irritaties),sg(irritatie)],de,[sbar]).

n([mass(islam),
   mass('Islam')],de,[]).

n([sg(islamiet),pl(islamieten)],de,[]).

n([mass(islamisering)],de,[]).

n([pl(islamisten),sg(islamist)],de,[]).

n([pl(isolaties),sg(isolatie)],de,[]).

n([sg(isoleer)],de,[]).

n([mass(isolement)],het,[]).

n([sg(isotoop),pl(isotopen)],both,[]).

n([sg(issue),pl(issues)],both,[]).

n([mass('IT')],de,[]). % informatietechnologie

n([sg(item),pl(items)],het,[app_measure]).

n([pl(ivoren),sg(ivoor)],both,[]).

n([pl('j\'s'),sg(j)],de,[],[dim('j\'tje')]).

n([mass(ja)],het,[]).

n([mass([ja,mits]),
   mass([ja,',',mits])],het,[]).

%% jaar cel voor oplichter
%% P. krijgt jaar cel
n([bare_meas(jaar),pl(jaren)],het,
  [temp_mod,
   sbar,
   measure,
   pred_pp(van)]).

n([sg(jaar),pl(jaren),
   ignore_stem(jaar)],het,
  [temp_mod,
   sbar],
  [academie,
   s(arbeid),
   begin,
   s(begroting),
   s(beoordeling),
   beurs,
   boek,
   bouw,
   brug,
   crisis,
   dienst,
   examen,
   geboorte,
   glorie,
   jeugd,
   jubel,
   jubileum,
   kalender,
   i(kind,kinder),
   leer,
   s(leven),
   licht,
   man,
   oogst,
   s(oorlog),
   ramp,
   record,
   s(verkiezing),
   school,
   schrikkel,
   studie,
   tiener,
   top,
   tropen,
   ziekte,
   dim(jaartje)]).

n([sg(jaar),pl(jaren),
   ignore_stem(jaar)],het,
  [],
  ['Mondriaan']
 ).

n([sg(jaarbasis)],de,[]).

n([pl(jaargangen),sg(jaargang)],de,[temp_mod,sbar]).

n([sg(jaargetijde),pl(jaargetijden)],het,[temp_mod,sbar]).

n([pl(jaarrekeningen),sg(jaarrekening)],de,[]).

n([pl(jaartallen),sg(jaartal)],het,[]).

n([pl(jaartellingen),sg(jaartelling)],de,[]).

n([pl(jaarverslagen),sg(jaarverslag)],het,[]).

n([pl(jaarwisselingen),sg(jaarwisseling)],de,[temp_mod,sbar]).

n([mass(jacht)],de,[],
  [drijf,
   klop,
   i(staat,staten),
   i(talent,talenten),
   i(vos,vossen)]).

n([pl(jachten),sg(jacht)],het,[]).

n([pl(jachtgeweren),sg(jachtgeweer)],het,[]).

n([pl(jachthavens),sg(jachthaven)],de,[]).

n([pl(jachthonden),sg(jachthond)],de,[]).

n([pl(jacks),sg(jack)],both,[]). % celex: de?

n([sg(jackpot),pl(jackpots)],de,[]).

n([pl(jacquets),pl(jacquetten),sg(jacquet)],het,[]).

n([mass(jade)],het,[]).

n([pl(jagers),sg(jager)],de,[],
  [aan,
   dim(jagertje),
   i(koop_DIM,koopjes),
   premie]).

n([pl(jaguars),sg(jaguar)],de,[],[dim(jaguartje)]).

n([pl(jakhalzen),sg(jakhals)],de,[]).

n([mass(jaloersheid)],de,[]).

n([pl(jaloezieën),sg(jaloezie)],de,[]).

n([pl(jams),sg(jam)],de,[]).

n([pl(jammers),sg(jammer)],de,[]).

n([sg(januari)],de,[temp_mod,sbar]).

n([pl(japonnen),sg(japon)],de,[],
  [nacht,
   dim(japonnetje)]).

n([pl(jargons),sg(jargon)],het,[]).

n([pl(jassen),sg(jas)],de,[],
  [kamer,
   ochtend,
   regen,
   winter,
   zomer,
   dim(jasje)]).

n([pl(jasmijnen),sg(jasmijn)],de,[]).

n([pl(jaszakken),sg(jaszak)],de,[]).

n([mass(jazz)],de,[]).

n([mass(jeans),pl(jeans)],de,[]).

n([pl(jeeps),sg(jeep)],de,[]).

n([pl(jekkers),sg(jekker)],de,[]).

n([pl(jenevers),sg(jenever)],de,[]).

n([pl(jeneverbessen),sg(jeneverbes)],de,[]).

n([sg(jengel),pl(jengels)],de,[]).

n([pl(jerseys),sg(jersey)],de,[]).

n([sg([jet,lag]),pl([jet,lags]),
   sg([jet,leg]),pl([jet,legs])],de,[]).

n([mass([jet,set])],de,[]).

n([mass([jeu,de,boules])],het,[]).

%% Ik heb mijn hele jeugd en puberteit naar Japan terugverlangd .
n([mass(jeugd)],de,[temp_mod]).

n([pl(jeugdherbergen),sg(jeugdherberg)],de,[]).

n([pl(jeugdherinneringen),sg(jeugdherinnering)],de,[]).

n([mass(jeuk)],de,[]).

n([pl(jezuïeten),sg(jezuïet),
   pl(jezuïeten),sg(jezuïet)],de,[]).

n([mass(jicht)],de,[]).

n([mass(jiddisch)],het,[]).

n([sg(jihad)],de,[]).

n([sg(jihadisme)],het,[]).

n([sg(jihadist),pl(jihadisten)],de,[]).

n([sg(jingle),pl(jingles)],de,[]).

n([pl(jobs),sg(job)],de,[]).

n([mass(joch)],het,[]).

n([pl(jochies),sg(jochie)],het,[]).

n([sg(jodel)],de,[]).

n([mass(jodendom)],het,[]).

n([pl(jodenvervolgingen),sg(jodenvervolging)],de,[]).

n([mass(jodium)],het,[]).

n([mass(joghurt)],de,[]).

n([pl(joints),sg(joint)],de,[]).

n([sg([joint,implementation])],de,[]).

n([pl([joint,ventures]),sg([joint,venture]),
   pl('joint-ventures'),sg('joint-venture')],de,[]).

n([sg(jojo),pl('jojo\'s')],de,[]).

n([sg(joker),pl(jokers)],de,[]).

n([pl(jongen),sg(jong)],het,[],[]).

n([pl(jongedames),sg(jongedame)],de,[]).

n([pl(jongeheren),sg(jongeheer)],de,[]).

n([pl(jongelingen),sg(jongeling)],de,[]).

n([pl(jongemannen),sg(jongeman)],de,[]).

n([pl(jongens),sg(jongen)],de,[],
  [buur,
   loop,
   neger,			% excuus, komt in oude romans zo voor
   i(schip,scheeps),
   school,
   straat,
   dim(jongetje)]).

n([stem(jong),
   pl(jongeren),sg(jongere)],de,[],
  [hang,
   moslim,
   probleem,
   school,
   zwerf]).

n([stem(jongelui),pl(jongelieden),pl(jongelui)],both,[]).

n([sg(jongmens)],het,[]).

n([pl(jonkers),sg(jonker)],de,[],[dim(jonkertje)]).

n([pl(jonkheren),sg(jonkheer)],de,[]).

n([sg(jonkie),pl(jonkies)],het,[]).

n([pl(jonkvrouwen),sg(jonkvrouw)],de,[]).

n([pl(jonkmannen),sg(jonkman)],de,[]).

n([pl('Joden'),sg('Jood')],de,[]).

n([pl(joden),sg(jood)],de,[]).

n([meas(joule),pl(joules)],de,[meas_mod,measure]).

n([pl(journaals),pl(journalen),sg(journaal)],het,[],
  [i(acht_uur,'acht-uur-'),
   jeugd]).

n([pl(journalisten),sg(journalist)],de,[],
  [foto,
   internet,
   sport]).

n([pl(journalistes),sg(journaliste)],de,[],
  [foto,
   internet,
   sport]).

n([mass(journalistiek)],de,[],[sport]).

n([sg(jouwe),pl(jouwen),pl(jouwe)],both,[]).

n([mass(jubel)],de,[]).

n([pl(jubilea),pl(jubileums),sg(jubileum)],het,[]).

n([mass(judo)],both,[]).

n([sg(judoka),pl('judoka\'s')],de,[]).

n([pl(juffen),pl(jufs),sg(juf)],de,[]).

n([pl(juffers),sg(juffer)],de,[],[dim(juffertje)]).

n([pl(juffrouwen),sg(juffrouw)],de,[]).

n([mass(juistheid)],de,[]).

n([pl(jukken),sg(juk)],het,[]).

n([sg(juli)],de,[temp_mod,sbar]).

n([pl(jungles),sg(jungle)],de,[]).

n([sg(juni)],de,[temp_mod,sbar]).

n([pl(junioren),sg(junior),pl(juniors),pl(juniores)],de,[]).

n([pl(junks),sg(junk)],de,[]).

n([pl([junk,bonds])],de,[]).

n([pl(junkies),sg(junkie)],de,[]).

n([pl('junta\'s'),sg(junta)],de,[]).

n([mass(juridisering)],de,[]).

n([pl(jurisdicties),pl(jurisdictiën),sg(jurisdictie)],de,[]).

n([mass(jurisprudentie)],de,[]).

n([pl(juristen),sg(jurist)],de,[],
  []).

n([pl(juristes),sg(juriste)],de,[]).

n([pl(jurken),sg(jurk)],de,[],
  [avond,
   doop,
   feest,
   trouw,
   dim(jurkje)]).

n([pl('jury\'s'),sg(jury)],de,[],
  [vak]).

n([mass(jus)],de,[]).

n([mass([jus,'d\'orange'])],de,[]).

n([mass(justitie)],de,[],[i(klas,klassen)]).

n([mass(jute)],both,[]).

n([pl(jutezakken),sg(jutezak)],de,[]).

n([pl(juwelen),sg(juweel)],het,[],[dim(juweeltje)]).

n([pl(juweliers),sg(juwelier)],de,[]).

n([pl('k\'s'),sg(k)],de,[],[dim('k\'tje')]).

n([pl(kaaien),sg(kaai)],de,[],[dim(kaaitje)]).

n([pl(kaken),sg(kaak)],de,[],[beneden,boven,onder]).

n([pl(kapen),sg(kaap)],de,[]).

n([mass(kaapvaart)],de,[]).

n([pl(karen),sg(kaar)],de,[]).

n([pl(kaarsen),sg(kaars)],de,[measure],[dim(kaarsje)]).

n([pl(kaarten),sg(kaart)],de,[],
  [ansicht,
   bij,
   brief,
   chip,
   dag,
   s(identiteit),h('ID'),
   geheugen,
   s(geluid),
   instap,
   jaar,
   kerst,
   land,
   maand,
   menu,
   netwerk,
   h('OV'),
   i('OV_student','OV-studenten'),
   pers,
   prijs,
   route,
   seizoen,s(seizoen),
   speel,
   staal,
   stempel,
   i(strip,strippen),
   s(toegang),
   telefoon,
   trein,
   troef,
   week,
   weekend,
   i(weg,wegen),
   wereld,
   wijn,
   dim(kaartje)]).

n([pl(kazen),sg(kaas)],de,[],
  [i(gat,gaten),
   i(geit,geiten),i(geit,geite),
   room,
   smeer,
   dim(kaasje)]).

n([sg(kaats),pl(kaatsen)],de,[]).

n([mass(kabaal)],het,[]).

n([pl(kabels),sg(kabel)],de,[],
  [dim(kabeltje),
   s(elektriciteit),
   i(elektriciteit,electriciteits),
   internet,
   netwerk,
   s(toelevering)
  ]).

n([sg(kabelaar),pl(kabelaars)],de,[]).

n([sg(kabelbaan),pl(kabelbanen)],de,[]).

n([pl(kabeljauwen),sg(kabeljauw)],de,[],[dim(kabeljauwtje)]).

n([sg(kabine),pl(kabines)],de,[]).

n([pl(kabinetten),sg(kabinet)],het,[],
  [post_h('Balkenende'),
   post_wh(['Balkenende','I']),post_wh(['Balkenende','II']),post_wh(['Balkenende','III']),
   post_wh(['Balkenende','1']),post_wh(['Balkenende','2']),post_wh(['Balkenende','3']),
   post_h('Beel'),
   post_h('Biesheuvel'),
   post_h('Cals'),
   post_h('Cals/Vondeling'),
   post_h('Colijn'),
   post_wh(['Cort',van,der,'Linden']),
   post_wh(['De','Geer']), post_wh([de,'Geer']),
   post_wh(['De','Jong']), post_wh([de,'Jong']),
   post_wh(['De','Quay']), post_wh([de,'Quay']),
   post_wh(['Den','Uyl']), post_wh([den,'Uyl']),
   post_h('Drees'),post_wh(['Drees/Van','Schaik']),
   post_h('Gerbrancy'),
   post_h('Heemskerk'),
   post_h('Kok'),
   post_wh(['Kok','I']),post_wh(['Kok','II']),
   post_wh(['Kok','1']),post_wh(['Kok','2']),
   post_h('Lubbers'),
   post_h('Lubbers/Kok'),
   post_wh(['Lubbers','I']),post_wh(['Lubbers','II']),post_wh(['Lubbers','III']),
   post_wh(['Lubbers','1']),post_wh(['Lubbers','2']),post_wh(['Lubbers','3']),
   post_h('Marijnen'),
   post_h('Thorbecke'),
   post_wh(['Van','Agt']), post_wh([van,'Agt']),post_wh([van,'Agt/Den','Uyl']),
   post_wh(['Van','Agt','1']), post_wh([van,'Agt','I']),
   post_wh(['Van','Agt','2']), post_wh([van,'Agt','II']),
   post_wh(['Van','Agt','3']), post_wh([van,'Agt','III']),
   post_wh([van,'Agt','1']), post_wh(['Van','Agt','I']),
   post_wh([van,'Agt','2']), post_wh(['Van','Agt','II']),
   post_wh([van,'Agt','3']), post_wh(['Van','Agt','III']),

   coalitie,
   s(eenheid),
   kern,
   s(meerderheid),
   s(minderheid),
   nood,
   s(oorlog),
   s(overgang),
   i(prent,prenten),
   i(rariteit,rariteiten),
   romp,
   schaduw,
   vecht,
   s(veiligheid),
   i(zaak,zaken)
  ]).

n([pl(kabouters),sg(kabouter)],de,[],[dim(kaboutertje)]).

n([pl(kachels),sg(kachel)],de,[],
  [gas,
   hout,
   dim(kacheltje)]).

n([sg(kadaster),pl(kadasters)],het,[]).

n([pl(kadavers),sg(kadaver),pl(cadavers),sg(cadaver)],het,[]).

n([pl(kaden),pl(kades),sg(kade)],de,[]).

n([pl(kaders),sg(kader)],both,[],  % VL: de kader
  [s(beleid),
   partij  % en niet part_ijk_ader
  ]).

n([pl(kadetten),sg(kadet)],de,[]).

n([sg(kado),pl('kado\'s')],het,[],[dim(kadootje)]).

n([mass(kaf)],het,[]).

n([pl(kaften),sg(kaft)],both,[]).  % celex: het

n([pl(kajuiten),sg(kajuit)],de,[]).

n([mass(kak)],de,[]).

n([sg(kaketoe),pl(kaketoes)],de,[],
  [geelkuif]).

n([mass(kaki)],het,[]).

n([sg(kakker),pl(kakkes)],de,[]).

n([pl(kakkerlakken),sg(kakkerlak)],de,[]).

n([pl(kalebassen),sg(kalebas)],de,[]).

n([pl(kalenders),sg(kalender)],de,[],
  [wedstrijd,
   dim(kalendertje)]).

n([pl(kalveren),sg(kalf)],het,[],
  [kist,
   koe,
   dim(kalfje),
   dim(kalvertje)]).

n([pl(kalibers),sg(kaliber)],het,[pred_pp(van)]).

n([sg(kalief),pl(kalieven)],de,[]).

n([sg(kalifaat),pl(kalifaten)],het,[]).

n([mass(kalium)],het,[]).

n([mass(kalk)],both,[]).

n([pl(kalkoenen),sg(kalkoen)],de,[],[dim(kalkoentje)]).

n([mass(kalksteen)],both,[]).

n([mass(kalmte)],de,[]).

n([pl(kalorieën),sg(kalorie)],de,[]).

n([pl(kammen),sg(kam)],de,[],[dim(kammetje)]).

n([pl(kamelen),sg(kameel)],de,[],[dim(kameeltje)]).

n([pl(kamers),sg(kamer)],de,[],
  [achter,
   baby,
   s(bestuur),
   binnen,
   controle,
   doorzon,
   eet,
   gelag,
   hotel,
   huis,
   s(jongen),
   i(kind,kinder),
   kleed,
   logeer,
   machine,
   s(meisje),
   meld,
   operatie,
   raad,s(raad),
   s(rederijker),
   schat,
   slaap,
   speel,
   spreek,
   studeer,
   tuin,
   verhoor,
   voor,
   voorraad,
   wacht,
   werk,
   woon,
   i(ziek,zieken),
   zit,
   zolder,
   dim(kamertje)]).

n([pl(kameraads),pl(kameraden),sg(kameraad)],de,[],[dim(kameraadje)]).

n([mass(kameraadschap)],de,[]).

n([pl(kamermeisjes),sg(kamermeisje)],het,[]).

n([sg(kamikaze)],de,[]).

n([pl(kamillen),sg(kamille)],de,[]).

n([pl(kampen),sg(kamp)],de,[]).

n([pl(kampen),sg(kamp)],het,[],
  [basis,
   concentratie,
   i(vangen,gevangenen),
   s(internering), % geen intern_ring_kamp
   i('Jap','Jappen'),
   leger,
   opvang,
   straf,
   i(tent,tenten),
   s(training),
   s(vernietiging),
   i(vluchteling,vluchtelingen),
   h('VN'),
   werk,
   i(woon_wagen,woonwagen)]).

n([pl(kampementen),sg(kampement)],het,[]).

n([sg(kamper),pl(kampers)],de,[]). % kampbewoner

n([pl(kampioenen),sg(kampioen)],de,
  [app_measure],
  [s(land),
   f([olympisch]),  % de olympisch kampioen
   f(['Olympisch']),		% de Olympisch kampioen
   vice,
   weg,
   wereld
  ]).

n([pl(kampioenes),sg(kampioene)],de,
  [app_measure],
  [s(land),
   f([olympisch]),  % de olympisch kampioene
   f(['Olympisch']),		% de Olympisch kampioene
   vice,
   weg,
   wereld
  ]).

n([pl(kampioenschappen),sg(kampioenschap)],het,[app_measure],
  [s(land),
   wereld]).

n([pl(kampioenschappen),sg(kampioenschap)],het,[],
  [dam,
   tennis,
   voetbal
  ]).

n([pl(kampongs),sg(kampong)],de,[]).

n([pl(kampvuren),sg(kampvuur)],het,[]).

n([pl(kannen),sg(kan)],de,[measure],
  [koffie,
   thermos,
   dim(kannetje)]).

%% twee kanalen porno/amusement..
n([pl(kanalen),sg(kanaal)],het,[app_measure,np_app_measure],
  [dim(kanaaltje)]).

n([pl(kanalen),sg(kanaal)],het,[],
  [toevoer,
   televisie,tv,h(tv),f([tv]),i(tv,'TV-'),
   dim(kanaaltje)]).

n([pl(kanapees),sg(kanapee)],de,[],[dim(kanapeetje)]).

n([pl(kanaries),sg(kanarie)],de,[]).

n([pl(kanarietjes),sg(kanarietje)],het,[]).

n([pl(kandelaars),pl(kandelaren),sg(kandelaar)],de,[],[dim(kandelaartje)]).

%% hij is de belangrijkste kandidaat hem op te volgen
n([pl(kandidaten),sg(kandidaat)],de,
  [vp],[]).

n([pl(kandidates),sg(kandidate)],de,
  [vp],[]).

n([pl(kandidaten),sg(kandidaat)],de,[],
  [degradatie,
   s(kanselier),
   medaille,
   overname,
   oppositie,
   s(president),
   tegen,
   titel]).

n([pl(kandidaturen),sg(kandidatuur)],de,[]).

n([mass(kaneel)],de,[]).

n([sg(kangoeroe),pl(kangoeroes),
   sg(kangaroe),pl(kangaroes)],de,[]).

n([pl(kanjers),sg(kanjer)],de,[]).

n([mass(kanker),pl(kankers)],de,[],
  [baarmoederhals,
   borst,
   darm,
   huid,
   lever,
   long,
   ovarium,
   prostaat]).

n([sg(kannibaal),pl(kannibalen)],de,[]).

n([mass(kannibalisme)],het,[]).

n([pl(kannonades),sg(kannonade)],de,[]).

n([pl('kano\'s'),sg(kano)],de,[],[dim(kanootje)]).

n([pl(kanonnen),pl(kanons),sg(kanon)],het,[],
  [water,
   dim(kanonnetje)]).

n([sg(kanovaarder),pl(kanovaarders)],de,[]).

n([pl(kansen),sg(kans)],de,[sbar,vp],[dim(kansje)]).

n([pl(kansen),sg(kans)],de,[],
  [i(bui,buien),
   neerslag,
   s(overleving),
   pak,
   s(slaging),
   winst]).

n([pl(kansels),sg(kansel)],de,[],[dim(kanseltje)]).

n([pl(kanselieren),pl(kanseliers),sg(kanselier)],de,[],
  [h(ex),
   h(oud),
   s(rijk)]).

n([pl(kanshebbers),sg(kanshebber)],de,[]).

n([pl(kanshebsters),sg(kanshebster)],de,[]).

n([pl(kansspelen),sg(kansspel)],het,[]).

n([pl(kanten),sg(kant)],de,[],
  [h(a), h('b'),
   h(b), h('B'),
   s(bestuurder),
   dim(kantje),
   s(passagier)
  ]).

n([pl(kanten),sg(kant)],de,[measure,pred_pp(aan)],
  [dim(kantje)]).

n([sg(kanteel),pl(kantelen)],de,[]).

n([sg(kanteling),pl(kantelingen)],de,[]).

n([pl(kantines),sg(kantine)],de,[],
  [s(bedrijf),
   sport]).

n([pl(kantons),sg(kanton)],het,[]).

n([sg(kantongerecht),pl(kantongerechten)],het,[]).

n([pl(kantonrechters),sg(kantonrechter)],de,[]).

n([pl(kantoren),sg(kantoor)],het,[],
  [s(accountant),
   administratie,
   i(advocaat,advocaten),i(advocaat,advokaten),
   bij,
   distributie,
   i(effect,effecten),
   hoofd,
   politie,
   post,
   regio,
   stadsdeel,
   verkoop,
   wissel,
   dim(kantoortje)]).

n([mass(kantoorautomatisering)],de,[]).

n([pl(kanttekeningen),sg(kanttekening)],de,[sbar]).

n([pl(kanunniken),sg(kanunnik)],de,[]).

n([mass(kanvas)],het,[]).

n([pl(kappen),sg(kap)],de,[],
  [hout,
   dim(kapje)]).

n([pl(kapellen),sg(kapel)],de,
  [np_app_measure],
  [i(boer,boeren),
   dim(kapelletje)]).

n([pl(kapelaans),sg(kapelaan)],de,[]).

n([pl(kapers),sg(kaper)],de,[],
  [vliegtuig,
   dim(kapertje)]).

n([pl(kapingen),sg(kaping)],de,[],
  [vliegtuig]).

n([pl(kapitalen),sg(kapitaal)],het,[],
  [i(aandeel,aandelen),
   durf,
   start,
   dim(kapitaaltje)]).

n([sg(kapitalisatie),pl(kapitalisaties)],de,[]).

n([mass(kapitalisme)],het,[]).

n([pl(kapitalisten),sg(kapitalist)],de,[]).

n([pl(kapitelen),sg(kapiteel)],het,[],[dim(kapiteeltje)]).

n([pl(kapiteins),sg(kapitein)],de,[]).

n([pl(kapittelen),pl(kapittels),sg(kapittel)],het,[],[dim(kapitteltje)]).

n([pl(kapitulaties),sg(kapitulatie)],de,[]).

n([pl(kappers),sg(kapper)],de,[]).

n([pl(kapsters),sg(kapster)],de,[]).

n([pl(kapriolen),sg(kapriool)],de,[]).

n([pl(kapsels),sg(kapsel)],het,[]).

n([pl(kapstokken),sg(kapstok)],de,[]).

n([pl(karren),sg(kar)],de,[measure],[dim(karretje)]).

n([meas(karaat)],de,[meas_mod,measure]).

n([pl(karabijnen),sg(karabijn)],de,[],[dim(karabijntje)]).

n([pl(karaffen),sg(karaf)],de,[measure]).

n([pl(karakters),sg(karakter)],het,[sbar,vp],
  [dim(karaktertje)]).

n([pl(karakters),sg(karakter)],het,[],
  [s(volk)]).

n([mass(karakterisering),pl(karakteriseringen),sg(karakterisering)],de,[]).

n([pl(karakteristieken),sg(karakteristiek)],de,[sbar,vp]).

n([mass(karakterstructuur)],de,[]).

n([pl(karaktertrekken),sg(karaktertrek)],de,[],[dim(karaktertrekje)]).

n([mass(karate)],het,[]).

n([pl(karavanen),sg(karavaan)],de,[],[dim(karavaantje)]).

n([pl(kardinalen),sg(kardinaal)],de,[],
  [curie]).

n([pl(karikaturen),sg(karikatuur)],de,[subject_sbar,subject_vp]).

n([pl(karkassen),sg(karkas)],het,[]).

n([mass(karma)],het,[]).

n([pl(karnavals),sg(karnaval)],het,[]).

n([mass(karnemelk)],de,[]).

n([sg(karosserie)],de,[]).

n([pl(karpers),sg(karper)],de,[],
  [tand,
   dim(karpertje)]).

n([sg(karpet),pl(karpetten)],het,[]).

n([pl(kartels),sg(kartel)],het,[],[olie]).

n([pl(kartons),sg(karton)],het,[],
  [bord,
   stro,
   dim(kartonnetje)]).

n([pl(karwatsen),sg(karwats)],de,[]).

n([pl(karweien),sg(karwei)],both,[],[dim(karweitje)]).

n([pl(kassen),sg(kas)],de,[],
  [s(oorlog),
   s(staat)]).

n([mass(kasjmier)],both,[]).

n([pl('kassa\'s'),sg(kassa)],de,[]).

n([pl(kasseien),sg(kassei)],de,[],[dim(kasseitje)]).

n([pl(kassiers),sg(kassier)],de,[]).

n([pl(kasten),sg(kast)],de,[],
  [archief,
   i(boek,boeken),
   dashboard,
   flipper,
   i(glas,glazen),
   i(handschoen,handschoenen),
   hang,
   gok,
   kleding,
   lade,
   nacht,
   i(pop,poppen),
   dim(kastje)]).

n([pl(kastanjes),sg(kastanje)],de,[]).

n([pl(kastanjebomen),sg(kastanjeboom)],de,[]).

n([pl(kasten),sg(kaste)],de,[]).

n([pl(kastelen),sg(kasteel)],het,[],[dim(kasteeltje)]).

n([pl(kasteleins),sg(kastelein)],de,[]).

n([pl(katten),sg(kat)],de,[],
  [zwerf,
   dim(katje)]).

n([pl(katalysatoren),pl(katalysators),sg(katalysator)],de,[]).

n([pl(katers),sg(kater)],de,[sbar],[dim(katertje)]).

n([pl(katernen),sg(katern)],both,[]). % CELEX: de?

n([pl(kathedralen),sg(kathedraal)],de,[],[dim(kathedraaltje)]).

n([mass(katholicisme)],het,[]).

n([pl(katholieken),sg(katholiek)],de,[]).

n([pl(katoenen),sg(katoen)],both,[]).

n([sg(katrol),pl(katrollen)],de,[]).

n([mass(kattekwaad),
   mass(kattenkwaad)],het,[]).

n([pl(kauwen),sg(kauw)],de,[]).

n([mass(kauwgom)],both,[]).

n([mass(kauwgum)],both,[]).

n([sg(kavel),pl(kavels)],both,[measure]).  % kavels bouwgrond

n([mass(kaviaar)],de,[]).

n([sg(kayak),pl(kayaks)],de,[]).

n([pl(kazernen),pl(kazernes),sg(kazerne)],de,[],
  [brandweer,
   woon]).

n([sg('KB'),sg(kb)],het,[]).  % VLAAMS: koninklijk besluit

n([meas(kcal),meas(kCal)],de,[meas_mod,measure]).

%% voetballer: ik heb hem bewust een keek gegeven
n([sg(keek),pl(keken)],de,[]).

n([sg(keel),pl(kelen)],de,[],[dim(keeltje)]).

n([pl(keelgaten),sg(keelgat)],het,[]).

n([sg(keep),pl(kepen)],de,[]). % vogel

n([pl(keepers),sg(keeper)],de,[],
  [voetbal]).

n([meas(keer),pl(keren)],de,
  [temp_mod,
   subject_sbar,
   sbar,
   np_measure],
  [dim(keertje)]).

n([pl(keerzijden),sg(keerzijde),sg(keerzij)],de,
  [subject_sbar]).

n([mass(ketchup)],de,[]).

n([pl(keten),sg(keet)],de,[]).

n([mass(ketjap)],de,[]).

n([mass([ketjap,manis])],de,[]).

n([pl(kegels),sg(kegel)],de,[],[dim(kegeltje)]).

n([pl(keien),sg(kei)],de,[],[dim(keitje)]).

n([pl(keizers),sg(keizer)],de,[],[dim(keizertje)]).

n([pl(keizerinnen),sg(keizerin)],de,[],[dim(keizerinnetje)]).

n([pl(keizerrijken),sg(keizerrijk)],het,[]).

n([sg(keizersnede),pl(keizersnedes)],de,[]).

n([pl(kelders),sg(kelder)],de,[],
  [graf,
   dim(keldertje),
   provisie,
   schuil,
   wijn]).

n([pl(kelken),sg(kelk)],de,[measure],[dim(kelkje)]).

n([pl(kelners),sg(kelner)],de,[]).

n([pl(kenmerken),sg(kenmerk)],het,
  [sbar,
   subject_sbar
  ]).

n([pl(kennels),sg(kennel)],de,[measure]).

n([pl(kenners),sg(kenner)],de,[],[kunst]).

n([sg(kennis)],de,[sbar]).

n([sg(kennis),pl(kennissen)],de,[]).

n([sg(kennis)],de,[],
  [basis,
   film,
   i(mens,mensen),
   sport,
   vak,
   voor,
   zelf]).

n([pl(kennisgevingen),sg(kennisgeving)],de,[sbar,vp],[dim(kennisgevinkje)]).

n([pl(kennismakingen),sg(kennismaking)],de,[]).

n([mass(kennisneming)],de,[]).

n([sg(kenschets)],de,[]).

n([pl(kentekenen),pl(kentekens),sg(kenteken)],het,[]).

n([pl(kenteringen),sg(kentering)],de,[]).

n([pl(kepers),sg(keper)],de,[],[dim(kepertje)]).

n([pl(kepies),sg(kepie)],de,[],[dim(kepietje)]).

n([sg(keramiek),mass(keramiek),pl(keramieken)],both,[]).

n([sg(keramist),pl(keramisten)],de,[]).

n([pl(kerels),sg(kerel)],de,[],[dim(kereltje)]).

n([sg(kerf),pl(kerven)],de,[]).

n([pl(kerken),sg(kerk)],de,[],
  [parochie,
   pinkster,			% niet pink_ster_kerk
   h(rk),
   'Samen-op-weg',h('Samen-op-weg'),h('Sow'),
   dim(kerkje)]).

n([pl(kerkasielen),sg(kerkasiel)],het,[]).

n([pl(kerkers),sg(kerker)],de,[],[dim(kerkertje)]).

n([pl(kerkgenootschappen),sg(kerkgenootschap)],het,[]).

n([pl(kerkhoven),sg(kerkhof)],het,[]).

n([pl(kermissen),sg(kermis)],de,[]).

n([pl(kernen),sg(kern)],de,[subject_sbar,
                            measure]).  % de harde kern werkelozen

n([pl(kernen),sg(kern)],de,[],
  [h('A'),
   h('B'),
   atoom,
   s(dorp)]).

n([mass(kerosine)],de,[]).

n([pl(kersen),sg(kers)],de,[]).

n([mass(kerst),pl(kersten)],de,[temp_mod]).  % vier witte kersten op rij

n([pl(kerstbomen),sg(kerstboom)],de,[]).

n([pl(kerstversieringen),sg(kerstversiering)],de,[]).

n([mass(kervel)],de,[]).

n([pl(ketels),sg(ketel)],de,[measure],[dim(keteltje)]).

n([pl(ketels),sg(ketel)],de,[],[stoom]).

n([pl(ketenen),pl(ketens),sg(keten)],de,[measure],[]).

n([pl(ketenen),pl(ketens),sg(keten)],de,[],
  [berg,
   hotel,
   kleding,
   supermarkt,
   h(zend_uit,uitzend),
   voedsel,
   warenhuis,
   winkel,
   dim(ketentje)]).

n([pl(ketters),sg(ketter)],de,[]).

n([pl(ketterijen),sg(ketterij)],de,[]).

n([pl(kettingen),sg(ketting)],de,[],
  [hals,
   dim(kettinkje)]).

n([pl(keukens),sg(keuken)],de,[],[dim(keukentje)]).

n([pl(keukenmeiden),sg(keukenmeid)],de,[]).

n([pl(keuren),sg(keur)],both,[]).

n([pl(keuringen),sg(keuring)],de,[]).

n([pl(keurmeesters),sg(keurmeester)],de,[]).

n([sg(keurmerk),pl(keurmerken)],het,[]).

n([pl(keurslijven),sg(keurslijf)],het,[]).

n([pl(keuzen),sg(keus)],de,[vp,sbar,subject_vp,subject_sbar]).

n([pl(keuzen),pl(keuzes),sg(keuze)],de,[sbar,vp]).

n([pl(keuzen),pl(keuzes),sg(keuze)],de,[],
  [school]).

n([pl('keuze-alternatieven'),sg('keuze-alternatief')],het,[]).

n([pl(keuzealternatieven),sg(keuzealternatief)],het,[]).

n([pl(kevers),sg(kever)],de,[],
  [beek,
   loop,
   olie,
   i(snoer_hals,snoerhals),
   spartel,
   i(water_roof,waterroof),
   i(week_schild,weekschild),
   dim(kevertje)]).

n([pl(khans),sg(khan)],de,[]).

n([sg(kibboets),pl(kibboetsen)],de,[]).

n([pl(kicks),sg(kick)],de,[subject_sbar,subject_vp]).

n([sg(kid),pl(kids)],de,[]).

n([sg(kidnap)],de,[]).

n([sg(kidnapper),pl(kidnappers)],de,[]).

n([sg(kiek)],de,[],[dim(kiekje)]).

n([pl(kielen),sg(kiel)],de,[],[dim(kieltje)]).

n([pl(kielleggingen),sg(kiellegging)],de,[]).

n([mass(kielzog)],het,[]).

n([pl(kiemen),sg(kiem)],de,[],[dim(kiempje)]).

n([pl(kieren),sg(kier)],de,[pred_pp(op)],[dim(kiertje)]).

n([pl(kiezen),sg(kies)],de,[],[s(verstand)]).

n([pl(kieviten),sg(kievit)],de,[]).

n([pl(kiezels),sg(kiezel)],de,[],[dim(kiezeltje)]).

n([pl(kiezelstenen),sg(kiezelsteen)],de,[]).

n([pl(kiezers),sg(kiezer)],de,[]).

n([mass(kijk)],de,[],[dim(kijkje)]).

n([sg(['kijk-',en,luistergeld])],het,[]).

n([pl(kijkers),sg(kijker)],de,[pred_pp(in)]).

n([pl(kijkers),sg(kijker)],de,[],
  [dim(kijkertje),
   f([tv]),h(tv),tv,televisie,i(tv,'TV-')
  ]).

n([pl(kikken),sg(kik)],de,[]).

n([pl(kikkers),sg(kikker)],de,[],[dim(kikkertje)]).

n([sg(kikvors),pl(kikvorsen)],de,[],[]).

n([sg(killer),pl(killers)],de,[]).

n([meas(kilo),pl('kilo\'s')],de,[meas_mod,measure],
  [dim(kilootje)]).

n([meas(kilowattuur),pl(kilowatturen)],de,[meas_mod,measure]).

n([mass(kilte)],de,[]).

n([pl(kimmen),sg(kim)],de,[]).

n([pl('kimono\'s'),sg(kimono)],de,[],[dim(kimonootje)]).

n([pl(kinnen),sg(kin)],de,[],[dim(kinnetje)]).

n([pl(kinderen),sg(kind)],het,[],
  [adoptie,
   s(dominee),
   pleeg,
   school,
   straat,
   zwerf,
   dim(kindje),dim(kindertje)
  ]).

n([mass(kinderbescherming)],de,[]).

n([pl(kinderbijslagen),sg(kinderbijslag)],de,[]).

n([sg(kinderboerderij),pl(kinderboerderijen)],de,[np_app_measure]).

n([pl(kinderdagverblijven),sg(kinderdagverblijf)],het,[np_app_measure]).

n([pl(kindermeisjes),sg(kindermeisje)],het,[]).

n([mass(kindsheid)],de,[]).

n([sg(kinesist),pl(kinesisten)],de,[]).

n([mass(kinkhoest)],de,[]).

n([pl(kiosken),sg(kiosk)],de,[]).

n([pl(kippen),sg(kip)],de,[],
  [hobby,
   dim(kippetje),
   wip]).

n([mass(kippenvel),
   mass(kippevel)],het,[]).

n([mass(kir)],de,[]).

n([mass([kir,royal])],de,[]).

n([pl(kisten),sg(kist)],de,
  [measure],
  [dim(kistje)]).

n([pl(kisten),sg(kist)],de,
  [],
  [s(gereedschap),
   laad,
   dim(kistje)]).

n([mass(kit)],both,[]).

%% jong poesje
n([sg(kitten),pl(kittens)],de,[]).

n([mass(kitsch)],de,[]).

n([pl(kittelaars),sg(kittelaar)],de,[]).

n([pl('kiwi\'s'),sg(kiwi)],de,[],[dim(kiwietje)]).

n([meas(kjoule),pl(kjoules)],de,[meas_mod,measure]).

n([pl(klaagsters),sg(klaagster)],de,[]).

n([mass(klaarheid)],de,[]).

n([mass(klaarte)],de,[]).

n([pl(klazen),sg(klaas)],de,[]).

n([pl(klachten),sg(klacht)],de,
  [sbar,
   subject_sbar]).

n([pl(klachten),sg(klacht)],de,[],
  [s(gezondheid),
   hart,
   maag,
   rug,
   straf,
   wee]).

n([mass(klad)],de,[]).   % de klad komt er in

n([mass(klad)],het,[]).  % we schrijven de brief eerst in het klad

n([pl(kladden)],de,[]).

n([pl(klagers),sg(klager)],de,[]).

n([sg(klak),pl(klakken)],de,[]).

n([pl(klaksons),sg(klakson)],de,[]).

n([sg(klamp),pl(klampen)],de,[]).

n([mass(klandizie)],de,[]).

n([pl(klanken),sg(klank)],de,[]).

n([pl(klankborden),sg(klankbord)],het,[]).

n([pl(klanten),sg(klant)],de,[]).

n([pl(klappen),sg(klap)],de,
  [subject_sbar,
   subject_vp],
  [genade]).

n([pl(klappers),sg(klapper)],de,[],[dim(klappertje)]).

n([sg(klarinet),pl(klarinetten)],de,[],
  [bas]).

n([sg(klarinettist),
   sg(klarinettiste),
   pl(klarinettisten),
   pl(klarinettistes)],de,[]).

n([pl(klassen),sg(klas)],de,[measure],
  [kleuter,
   school,
   dim(klasje)]).

n([pl(klasseringen),sg(klassering)],de,[]).

n([pl(klassen),sg(klasse)],de,
  [pred_pp(van),
   subject_sbar,
   subject_vp]).

n([pl(klassen),sg(klasse)],de,
  [app_measure],
  [h(a),h('A'),
   h(b),h('B'),
   s(boeking),
   hoofd,
   s(koning),
   onder,
   s(overgang),
   promotie,
   reis,
   top]).

n([pl(klassen),sg(klasse)],de,[],
  [s(arbeider),
   midden,
   prijs,
   voetbal
  ]).

n([pl(klassementen),sg(klassement)],het,[],
  [eind]).

n([mass(klassenstrijd)],de,[]).

n([mass(klassiek)],het,[]).

n([pl(klassiekers),sg(klassieker)],de,[],
  [wieler]).

n([pl(klassifikaties),sg(klassifikatie)],de,[]).

n([pl(klauwen),sg(klauw)],de,[],[dim(klauwtje)]).

n([sg(klavecimbel),pl(klavecimbels)],de,[]).

n([pl(klavers),sg(klaver)],de,[],[dim(klavertje)]).

n([pl(klaveren),sg(klaveren)],de,[]).

n([pl(klavieren),sg(klavier)],het,[],[dim(klaviertje)]).

n([pl(klederdrachten),sg(klederdracht)],de,[]).

n([mass(kledij)],de,[]).

n([mass(kleding)],de,[],
  [s(bedrijf),
   merk]).

n([pl(kledingstukken),sg(kledingstuk)],het,[]).

n([pl(kleren)],het,
  [pred_pp_pl(uit)]).

n([pl(kleren)],het,[],
  [s(schaap),
   i(man,mannen),
   i(vrouw,vrouwen),
   dim(kleertje)
  ]).

n([pl(kleden),pl(klederen),sg(kleed)],het,[],
  [dim(kleedje)]).

n([mass(kleef)],de,[]).

n([pl(kleerkasten),sg(kleerkast)],de,[]).

n([pl(kleermakers),sg(kleermaker)],de,[]).

n([pl(kleerscheuren)],de,[]).

n([pl(kleien),sg(klei)],de,[]).

n([pl(kleindochters),sg(kleindochter)],de,[]).

n([mass(kleingeld)],het,[]).

n([mass(kleinheid)],de,[]).

n([pl(kleinigheden),sg(kleinigheid)],de,[sbar],[dim(kleinigheidje)]).

n([pl(kleinkinderen),sg(kleinkind)],het,[]).

n([pl(kleinoden),pl(kleinodiën),sg(kleinood)],het,[]).

n([pl(kleintjes),sg(kleintje)],het,[]).

n([pl(kleinzonen),pl(kleinzoons),sg(kleinzoon)],de,[]).

n([pl(klemmen),sg(klem)],de,[],
  [wiel,
   dim(klemmetje)]).

n([pl(klemtonen),sg(klemtoon)],de,[]).

n([sg(klemzet),pl(klemzetten)],de,[]).  % dammen

n([pl(kleppen),sg(klep)],de,[],
  [hart,
   oog,
   uitlaat]).

n([sg(klepper),pl(kleppers)],de,[]).

n([pl(klerenkasten),sg(klerenkast)],de,[]).

n([pl(klerken),sg(klerk)],de,[]).

n([mass(klerus)],de,[]).

n([sg(klets),pl(kletsen)],de,[]).

n([pl(kleuren),sg(kleur)],de,
  [app_measure,  % de kleur rood
   measure],     % in welk kleur shirt ...
  [dim(kleurtje)]).

n([pl(kleuren),sg(kleur)],de,[],
  [club,
   drie,
   s(gelaat),
   s(huid),
   klank,
   dim(kleurtje)]).

n([pl('kleurenfoto\'s'),sg(kleurenfoto)],de,[]).

n([pl(kleurlingen),sg(kleurling)],de,[]).

n([pl(kleurschakeringen),sg(kleurschakering)],de,[]).

n([pl(kleurstoffen),sg(kleurstof)],de,[app_measure]).

n([pl(kleuters),sg(kleuter)],de,[],[dim(kleutertje)]).

n([pl(kleuterleidsters),sg(kleuterleidster)],de,[]).

n([pl(klieken),sg(kliek)],de,[measure],[dim(kliekje)]).

n([pl(klieren),sg(klier)],de,[],
  [alvlees,
   borst,
   lymfe,
   s(geslacht),
   dim(kliertje),
   zweet]).

n([pl(klikken),pl(kliks),sg(klik)],de,[],
  [dubbel]).

n([mass(klim)],de,[],
  [slot]).

n([pl(klimaten),sg(klimaat)],het,[],
  [beurs,
   s(investering),
   diepwater,
   land,
   zee]).

%% niet klim_atol_oog
n([sg(klimatoloog),pl(klimatologen)],de,[]).

n([sg(klimmer),pl(klimmers)],de,[]).

n([mass(klimop)],de,[]).

n([sg(klingel),pl(klingels)],de,[]).

n([pl(klinieken),sg(kliniek)],de,[],
  [abortus,
   i(kick_af,afkick),
   privé,h(privé),
   h(tbs),i(tbs,'TBS-')]).

n([pl(klinken),sg(klink)],de,[],
  [deur]).

n([pl(klinkers),sg(klinker)],de,[],[dim(klinkertje)]).

n([pl(klippen),sg(klip)],de,[]).

n([pl(klitten),sg(klit)],de,[]).

n([pl(kliënten),sg(kliënte),sg(kliënt)],de,[]).

n([pl(klodders),sg(klodder)],de,[measure],[dim(kloddertje)]).

n([pl(klokken),sg(klok)],de,[],
  [kerk,
   nood,
   toren,
   dim(klokje)]).

n([pl(klokhuizen),sg(klokhuis)],het,[]).

n([stem(klok_luider),
   sg(klokkenluider),pl(klokkenluiders),
   sg(klokkeluider),pl(klokkeluiders)],de,[]).

n([sg(klokker),pl(klokkers)],de,[]).

n([pl(klokslagen),sg(klokslag)],de,[measure]).

n([pl(klompen),sg(klomp)],de,[measure],[dim(klompje)]).

n([pl(klonten),sg(klont)],de,[measure],[dim(klontje)]).

n([sg(klonter),pl(klonters)],de,[]).

n([pl(kloven),sg(kloof)],de,[],[dim(kloofje)]).

n([pl(klonen),sg(kloon)],de,[]).

n([pl(kloosters),sg(klooster)],het,[],[dim(kloostertje)]).

n([pl(kloten),sg(kloot)],de,[]).

n([mass(klote),mass(kloten)],de,[pred_pp(naar)]).

n([pl(klootzakken),sg(klootzak)],de,[]).

n([pl(kloppen),sg(klop)],de,[],[dim(klopje)]).

n([sg(klopper),pl(kloppers)],de,[]).

n([pl(kloppingen),sg(klopping)],de,[]).

n([pl(klossen),sg(klos)],de,[measure]).

n([pl(kluchten),sg(klucht)],de,[]).

n([pl(kluiven),sg(kluif)],both,[]).

n([pl(kluizen),sg(kluis)],de,[],
  [dim(kluisje)]).

n([pl(kluiten),sg(kluit)],de,[measure],
  [dim(kluitje)]).

n([pl(kluizenaars),pl(kluizenaren),sg(kluizenaar)],de,[]).

n([sg(klungel),pl(klungels)],de,[]).

n([sg(kluns),pl(klunzen)],de,[]).

n([pl(klussen),sg(klus)],de,
  [subject_vp],  % het is een hele klus om...
  [dim(klusje)]).

n([pl(klutsen),sg(kluts)],de,[measure]).

n([pl(kluwens),sg(kluwen)],both,[measure]).  %celex: het

n([pl(knaagdieren),sg(knaagdier)],het,[]).

n([sg(knaak),pl(knaken)],de,[],[medicijn]).

n([pl(knapen),sg(knaap)],de,[],
  [schild,
   dim(knaapje)]).

n([sg(knak),pl(knakken)],de,[]).

n([pl(knallen),sg(knal)],de,[],
  [oer,
   dim(knalletje)]).

n([mass(knalrood)],het,[]).

n([sg(knars)],de,[]).

n([sg(knauw)],de,[]).

n([pl(knechten),pl(knechts),sg(knecht)],de,[],
  [i(boer,boeren),
   dienst,
   huis
  ]).

n([pl(knepen),sg(kneep)],de,[vp]).

n([pl(knellen),sg(knel)],de,[]).

n([sg(kneus),pl(kneuzen)],de,[]).

n([pl(kneuzingen),sg(kneuzing)],de,[],[dim(kneuzinkje)]).

n([pl(knevels),sg(knevel)],de,[],[dim(kneveltje)]).

n([pl(knevelarijen),sg(knevelarij)],de,[]).

n([pl(knieën),sg(knie)],de,[],
  [linker,
   rechter,
   dim(knietje)]).

n([pl(knieholten),pl(knieholtes),sg(knieholte)],de,[]).

n([pl(knieschijven),sg(knieschijf)],de,[]).

n([sg(kniesoor)],de,[]).

n([sg(knieval)],de,[subject_sbar,subject_vp]).

n([pl(knikken),sg(knik)],de,[],[dim(knikje)]).

n([pl(knikkers),sg(knikker)],de,[],[dim(knikkertje)]).

n([pl(knippen),sg(knip)],de,[]).

n([mass(['knip-',en,plakwerk])],het,[]).

n([pl(knipogen),sg(knipoog)],de,[],[dim(knipoogje)]).

n([sg(knipper),pl(knippers)],de,[]).

n([pl(knipsels),sg(knipsel)],het,[],
  [i(krant,krante),
   i(krant,kranten),
   dim(knipseltje)]).

%% de knobbels t3 en t4 (tandarts...)
n([pl(knobbels),sg(knobbel)],de,[app_measure],[dim(knobbeltje)]).

n([pl([knock,downs]),sg([knock,down]),
   pl('knock-downs'),sg('knock-down'),
   pl(knockdowns),sg(knockdown)
  ],de,[]).

n([pl([knock,outs]),sg([knock,out]),
   pl('knock-outs'),sg('knock-out'),
   pl(knockouts),sg(knockout)
  ],de,[]).

n([mass(knoei)],de,[pred_pp(in)]).

n([mass(knoflook)],both,[]).

n([pl(knokkels),sg(knokkel)],de,[],[dim(knokkeltje)]).

n([mass(knokkelkoorts)],de,[]).

n([pl(knollen),sg(knol)],de,[],[dim(knolletje)]).

n([pl(knopen),sg(knoop)],de,[],
  [manchet,
   dim(knoopje)]).

n([pl(knooppunten),sg(knooppunt)],het,[]).

n([pl(knoopsgaten),sg(knoopsgat)],het,[]).

n([pl(knoppen),sg(knop)],de,[app_measure],[]).

n([pl(knoppen),sg(knop)],de,[],
  [dim(knopje),
   muis,
   f([delete]),
   i(link_muis,linkermuis),
   i(recht_muis,rechtermuis),
   verzend]).

n([pl(knotten),sg(knot)],de,[measure],[dim(knotje)]).

n([pl(knotsen),sg(knots)],de,[]).

n([pl(knotwilgen),sg(knotwilg)],de,[]).

n([mass('know-how'),
   mass([know,how]),
   mass(knowhow)],de,[]).

n([pl(knuffels),sg(knuffel)],de,[]).

n([pl(knuisten),sg(knuist)],de,[]).

n([pl(knullen),sg(knul)],de,[],[dim(knulletje)]).

n([pl(knuppels),sg(knuppel)],de,[],
  [honkbal,
   dim(knuppeltje)]).

n([sg(knutsel),pl(knutsels)],de,[]).

n([pl(koalities),sg(koalitie)],de,[]).

n([pl(kodes),sg(kode)],de,[sbar]).

n([pl(koeien),sg(koe)],de,[],[dim(koetje),melk]).

n([pl(koeken),sg(koek)],de,[],[dim(koekje)]).

n([sg(koekepan),sg(koekenpan),pl(koekepannen),pl(koekenpannen)],de,[]).

n([sg(koekie),pl(koekies)],het,[]).

n([pl(koekoeken),sg(koekoek)],de,[]).

n([mass(koelbloedigheid)],de,[]).

n([sg(koeler),pl(koelers)],de,[]).

n([pl(koelies),sg(koelie)],de,[]).

n([sg(koeling),pl(koelingen)],de,[]).

n([pl(koelkasten),sg(koelkast)],de,[]).

n([mass(koelte)],de,[]).

n([pl(koepels),sg(koepel)],de,[],
  [sport,
   dim(koepeltje)]).

n([pl(koepletten),sg(koeplet)],het,[]).

n([pl(koeren),sg(koer)],de,[],[dim(koertje)]).

n([pl(koeriers),sg(koerier)],de,[],
  [drug,s(drug),
   fiets]).

n([pl(koeriersters),sg(koerierster)],de,[],
  [drug,s(drug),
   fiets]).

n([pl(koersen),sg(koers)],de,[],
  [i(aandeel,aandelen),
   avond,
   beurs,
   dollar,
   etappe,
   euro,
   introductie,
   jaar,
   kermis,
   obligatie,
   i(punt,punten),
   i(rit,ritten),
   slot,
   tussen,
   uitgifte,
   wissel]).

n([pl(koetsen),sg(koets)],de,[],
  [lijk,
   dim(koetsje)]).

n([pl(koetshuizen),sg(koetshuis)],het,[]).

n([pl(koetsiers),sg(koetsier)],de,[]).

n([pl(koffers),sg(koffer)],de,[],
  [hut,
   dim(koffertje)]).

n([mass(koffie)],de,[]).

n([mass(koffiedik)],het,[]).

n([pl(kogels),sg(kogel)],de,[],[dim(kogeltje)]).

n([sg(kogelstoter),pl(kogelstoters)],de,[]).

n([sg(kogelstootster),pl(kogelstootsters)],de,[]).

n([pl(koks),sg(kok)],de,[],
  [h(chef),
   xthuis]).

n([pl(kokers),sg(koker)],de,[],
  [water,
   dim(kokertje)]).

n([sg(kokkel),pl(kokkels)],de,[]).

n([pl(kokkinnen),sg(kokkin)],de,[]).

n([mass(kokos)],de,[]).

n([pl(kokosnoten),sg(kokosnoot)],de,[]).

n([pl(kols),sg(kol)],de,[]).

n([pl(kolberts),sg(kolbert)],het,[]).

n([mass(kolder)],de,[]).

n([sg(kolk),pl(kolken)],de,[],
  [draai]).

n([pl(kolven),sg(kolf)],de,[measure]).

n([pl(kolven),sg(kolf)],de,[],[geweer]).

n([pl(kolieken),sg(koliek)],both,[]).

n([pl(kolommen),sg(kolom)],de,[measure],[dim(kolommetje)]).

n([pl(kolommen),sg(kolom)],de,[],
  [i(link,linker),
   i(recht,rechter),
   dim(kolommetje)]).

n([pl(kolonels),sg(kolonel)],de,[]).

n([sg(koloniaal),pl(kolonialen)],de,[]).

n([mass(kolonialisme)],het,[]).

n([pl(kolonies),pl(koloniën),sg(kolonie)],de,[measure],[dim(kolonietje)]).

n([pl(kolonies),pl(koloniën),sg(kolonie)],de,[],
  [dim(kolonietje),
   kroon]).

n([pl(kolonisaties),sg(kolonisatie),
   pl(kolonizaties),sg(kolonizatie)],de,[]).

n([pl(kolonisten),sg(kolonist)],de,[]).

n([pl(kolonnes),sg(kolonne)],de,[measure]).

n([pl(kolossen),sg(kolos)],de,[]).

n([pl(kommen),sg(kom)],de,[measure],[dim(kommetje)]).

n([mass(komaf)],de,[pred_pp(van)]).

n([pl(kombinaties),sg(kombinatie)],de,[]).

n([pl(kombuizen),sg(kombuis)],both,[]).

n([pl(komedies),sg(komedie)],de,[],
  [tragi]).

n([pl(kometen),sg(komeet)],de,[]).

n([sg(komiek),pl(komieken)],de,[]).

n([pl(komkommers),sg(komkommer)],de,[],
  [zee,
   dim(komkommertje)]).

n([pl('komma\'s'),sg(komma)],both,[],[dim(kommaatje)]).

n([pl(kommandeurs),sg(kommandeur)],de,[]).

n([pl('kommando\'s'),sg(kommando)],het,[sbar,vp]).

n([mass(kommer)],de,[]).

n([mass([kommer,en,kwel])],de,[]).

n([pl(kommiezen),sg(kommies)],de,[]).

n([sg(kompaan),pl(kompanen)],de,[]).

n([pl(kompassen),sg(kompas)],het,[]).

n([sg(kompel),pl(kompels)],de,[]).

n([mass(komst)],de,[]).

n([pl(konijnen),sg(konijn)],het,[],
  [proef,
   dim(konijntje)]).

n([pl(koningen),sg(koning),sg('Koning')],de,[],
  [i(boef,boeven),
   dim(koninkje)]).

n([pl(koninginnen),sg(koningin),sg('Koningin')],de,[],[dim(koninginnetje)]).

n([mass(koningschap)],het,[]).

n([pl(koningshuizen),sg(koningshuis)],het,[]).

n([pl(koninkrijken),sg(koninkrijk)],het,[],[dim(koninkrijkje)]).

n([pl(konten),sg(kont)],de,[],[dim(kontje)]).

n([pl(konvooien),sg(konvooi),
   pl(convooien),sg(convooi)],het,[measure]).

n([pl(kooien),sg(kooi)],de,[],[dim(kooitje)]).

n([mass(kook)],de,[pred_pp(aan),pred_pp(van)]).

n([sg(kookpunt),pl(kookpunten)],het,[]).

n([pl(kolen),sg(kool)],de,[],
  [i(boer,boeren),
   dim(kooltje)]).

n([mass(kooldioxyde),
   mass(kooldioxide)],both,[]).

n([pl(koolhydraten),sg(koolhydraat)],both,[]).

n([stem(koolmonoxide),
   mass(koolmonoxyde),
   mass(koolmonoxide)],both,[]).

n([mass(koolstof)],de,[]).

n([mass(koolwaterstof),pl(koolwaterstoffen)],both,[]).

n([mass(koolzaad)],both,[]).

n([mass(koolzuur)],het,[]).

n([pl(konen),sg(koon)],de,[],[dim(koontje)]).

n([pl(kopen),sg(koop)],de,[],[dim(koopje)]).

n([pl(kooplieden),pl(kooplui),sg(koopman)],de,[],
  [markt]).

n([pl(koopsommen),sg(koopsom)],de,[]).

n([mass(koopvaardij)],de,[]).  % niet koop_vaar_dij

n([pl(koopwaren),sg(koopwaar)],de,[]).

n([pl(koren),sg(koor)],het,[],
  [s(jongen),
   i(man,mannen),
   dim(koortje)]).

n([pl(koorden),sg(koord)],both,[],[dim(koordje)]).  % VL: de koord

n([pl(koortsen),sg(koorts)],de,[vp]).

n([sg(kop)],de,[pred_pp(op),
                pred_pp(aan)]).    % de boel staat op zijn kop
                                   % hij gaat aan kop

n([pl(koppen),sg(kop)],de,
  [start_app_measure,
   np_app_measure,
   measure,
   vp],
  [i(krant,kranten),
   tussen,
   dim(kopje)]).

n([pl(koppen),sg(kop)],de,[],
  [koffie,
   rond, % vissenfamiliesoort
   thee,
   douche,
   kern,
   dim(kopje)]).

n([sg([kop,van,jut]),pl([koppen,van,jut])],de,[]).

n([pl(kopballen),sg(kopbal)],de,[]).

n([pl(kopers),sg(koper)],de,[]).

n([mass(koper)],het,[]).

n([pl(kopieën),sg(kopie)],de,[],[dim(kopietje)]).

n([pl(kopijen),sg(kopij)],de,[]).

n([pl(koplampen),sg(koplamp)],de,[]).

n([pl(koplopers),sg(koploper)],de,[]).

n([pl(koppels),sg(koppel)],both,[measure],[dim(koppeltje)]).

n([pl(koppelbazen),sg(koppelbaas)],de,[]).

n([pl(koppelingen),sg(koppeling)],de,[app_measure],[snel]).

n([sg(koppie),pl(koppies)],het,[]).

n([mass(koppigheid)],de,[]).

n([pl(kopstukken),sg(kopstuk)],het,[]).

n([pl(kopten)],de,[]).

n([pl(koralen),sg(koraal)],het,[]).

n([mass(koran)],de,[]).

n([pl(kordons),sg(kordon)],het,[]).

n([mass(koren)],het,[]).

n([pl(korven),sg(korf)],de,[measure]).

n([sg(korfbal),pl(korfballen)],both,[],
  [dames]).

n([sg(korfballer),pl(korfballers)],de,[]).

n([mass(koriander)],de,[]).

n([pl(kornuiten),sg(kornuit)],de,[]).

n([pl(korporaals),sg(korporaal)],de,[],[dim(korporaaltje)]).

n([pl(korporaties),sg(korporatie)],de,[]).

n([pl(korpsen),sg(korps)],het,[app_measure],
  [brandweer,
   keur,
   leger,
   politie,
   regio]).

n([sg([korps,mariniers])],het,[]).

n([sg(korpschef),pl(korpschefs)],de,[]).

n([pl(korrels),sg(korrel)],de,[measure],
  [dim(korreltje)]).

n([pl(korrels),sg(korrel)],de,[],
  [graan,
   tarwe,
   zand,
   dim(korreltje)]).

n([pl(korsetten),sg(korset)],het,[]).

n([pl(korsten),sg(korst)],de,[],[dim(korstje)]).

n([sg(korstmos),pl(korstmossen)],both,[]).

n([pl([kort,gedingen]),sg([kort,geding]),
   pl(kortgedingen),sg(kortgeding)],het,[]).

n([mass(kortebaan)],de,[]).

n([pl(kortingen),sg(korting)],de,[sbar,vp]).

n([pl(kortingen),sg(korting)],de,[],
  [s(arbeid),
   belasting,
   s(heffing),
   i(oud,ouderen)]).

n([pl(kortsluitingen),sg(kortsluiting)],de,[]).

n([mass(kosmos)],de,[]).

n([sg(kosmonaut),pl(kosmonauten),
   sg(kosmonout),pl(kosmonouten)],de,[]).

n([sg(kost)],de,[]).

n([stem(kost),
   pl(kosten)],de,[],
  [administratie,
   s(arbeid),
   auto,
   s(beheer),
   bouw,
   brandstof,
   loon,
   materiaal,
   s(ontwikkeling),
   s(personeel),
   praktijk,
   proces,
   productie,i(productie,produktie),
   reis,
   reorganisatie,
   service,
   studie,
   telefoon,
   verzend,
   i(voor_rij,voorrij),
   ziekte,
   zorg
  ]).

n([pl([kosten,koper]),pl('k.k.')],both,[]).

n([sg(kostenbesparing),pl(kostenbesparingen)],de,[]).

n([sg(kostenpost),pl(kostenposten)],de,[]).

n([pl(kosters),sg(koster)],de,[],[dim(kostertje)]).

n([pl(kostuums),sg(kostuum)],het,[],[dim(kostuumpje)]).

n([pl(kostwinners),sg(kostwinner)],de,[]).

n([pl(kotten),sg(kot)],het,[],[friet]).

n([mass(kots)],both,[]).

n([mass(kou)],de,[],
  [vries,
   winter]).

n([mass(koude)],de,[],
  [vries,
   winter]).

n([sg(koukleum),pl(koukleumen)],de,[]).

n([pl(kousen),sg(kous)],de,[]).

n([pl(kozakken),sg(kozak)],de,[]).

n([pl(kozijnen),sg(kozijn)],het,[],[dim(kozijntje)]).

n([pl(kragen),sg(kraag)],de,[]).

n([pl(kraaien),sg(kraai)],de,[],[dim(kraaitje)]).

n([sg(kraj),pl(krajs)],both,[]). % Oosteuropese geografische eenheid?

n([pl(kraken),sg(kraak)],de,[]).

n([pl(kralen),sg(kraal)],de,[],[dim(kraaltje)]).

n([pl(kramen),sg(kraam)],both,[],[dim(kraampje)]).

n([pl(kranen),sg(kraan)],de,[],
  [gas,
   geld,
   hijs,
   olie,
   dim(kraantje)]).

n([pl(kraanvogels),sg(kraanvogel)],de,[]).

n([pl(krabben),sg(krab)],de,[],[dim(krabbetje)]).

n([pl(krabbels),sg(krabbel)],de,[],[dim(krabbeltje)]).

n([sg(krach),pl(krachs)],de,[],[beurs]).

%% power
n([sg(kracht),pl(krachten)],de,
  [subject_sbar,
   subject_vp,
   sbar,
   vp]).

n([sg(kracht),pl(krachten)],de,
  [subject_sbar,
   subject_vp,
   sbar,
   vp],
  [s(aantrekking),
   bewijs,
   concurrentie,
   daad,
   draag,
   geest,
   groei,
   koop,
   s(leven),
   man,
   natuur,
   s(overreding),
   s(overtuiging),
   slag,
   spier,
   stuw,
   trek,
   veer,
   s(verbeelding),
   s(wil),
   water,
   wind,
   s(zegging),
   zwaarte ]).

%% arbeiders
n([pl(krachten),sg(kracht)],de, [],
  [s(arbeid),
   s(beroep),
   productie,i(productie,produktie),
   uitzend,
   werk ]).

n([pl(krachtenvelden),sg(krachtenveld)],het,[]).

n([sg(krachthonk),pl(krachthonken)],het,[]).

n([mass(krachtmeting),pl(krachtmetingen),sg(krachtmeting)],de,[]).

n([sg(krachtpatser),pl(krachtpatsers)],de,[]).

n([pl(krakers),sg(kraker)],de,[],[dim(krakertje)]).

n([pl(kramers),sg(kramer)],de,[]).

n([pl(krampen),sg(kramp)],de,[]).

n([mass(krankzinnigheid)],de,[]).

n([pl(kransen),sg(krans)],de,[]).

n([pl(kranten),sg(krant)],de,[np_app_measure],
  [i(dakloos,daklozen),
   s(kwaliteit),
   sport,
   s(stad),
   i(zaak,zaken),
   dim(krantje),
   zaterdag,
   s(zondag)]).

n([mass(krapte)],de,[]).

n([pl(krassen),sg(kras)],de,[]).

n([sg(krasser),pl(krassers)],de,[]).

n([pl(kratten),sg(krat)],both,[measure],[dim(kratje)]).

n([pl(kraters),sg(krater)],de,[],[dim(kratertje)]).

n([pl(kredieten),sg(krediet)],het,[vp]).

n([pl(kredieten),sg(krediet)],het,[],
  [s(betaling),
   s(overbrugging),
   rommel,
   spaar]).
 
n([pl(kreeften),sg(kreeft)],de,[]).

n([sg(kreeftachige),pl(kreefachtigen)],de,[]).

n([pl(kreken),sg(kreek)],de,[]).

n([pl(kreten),sg(kreet)],de,[sbar,vp,start_app_measure]).

n([pl(krekels),sg(krekel)],de,[],[dim(krekeltje)]).

n([pl(krengen),sg(kreng)],het,[],[dim(krengetje)]).

n([sg(krent),pl(krenten)],de,[]).

n([pl(kreolen),sg(kreool)],de,[]).

n([pl(kreukels),sg(kreukel)],de,[],[dim(kreukeltje)]).

n([sg(kreun),pl(kreunen)],de,[]).

n([mass(kreupelhout)],het,[]).

n([pl(kribben),sg(krib)],de,[],[dim(kribje),dim(kribbetje)]).

n([sg(kriebel),pl(kriebels)],de,[]).

n([sg(kriek),pl(krieken)],de,[]).  % vlaams: kersen

n([sg(krieken)],het,[]).  % van de dag

n([sg(krijg)],de,[]).  % oorlog

n([pl(krijgers),sg(krijger)],de,[]).

n([mass(krijgsmacht)],de,[]).

n([pl(krijgslieden),sg(krijgsman)],de,[]).

n([pl(krijgsraden),sg(krijgsraad)],de,[]).

n([mass(krijt)],het,[],
  [dim(krijtje)]).

n([sg(krik),pl(krikken)],de,[]).

n([mass(krimp)],de,[]).

n([pl(kringen),sg(kring)],de,
  [measure],
  [dim(kringetje)]).            % kringetjes rook

n([pl(kringen),sg(kring)],de,
  [],
  [familie,
   kennis,
   i(kennis,kennissen),
   kies,
   lees,
   i(klant,klanten)]).

n([pl(kringlopen),sg(kringloop)],de,[]).

n([pl(kristallen),sg(kristal)],het,[],[dim(kristalletje)]).

n([pl(kritieken),sg(kritiek)],de,[sbar,vp,subject_sbar]).

n([pl(kritieken),sg(kritiek)],de,[],[kunst]).

n([sg('Kroaat'),pl('Kroaten')],de,[]).

n([mass('Kroatisch')],het,[]).

n([sg(krocht),pl(krochten)],de,[]).

n([pl(kroegen),sg(kroeg)],de,[],[dim(kroegje)]).

n([pl(kroezen),sg(kroes)],de,[],[smelt]).

n([sg(kroket),pl(kroketten)],de,[],
  [aardappel,
   vlees]).

n([pl(krokodillen),sg(krokodil)],de,[],[dim(krokodilletje)]).

n([pl(krommingen),sg(kromming)],de,[]).

n([pl(kronieken),sg(kroniek)],de,[sbar]).

n([pl(kroniekschrijvers),sg(kroniekschrijver)],de,[]).

n([pl(kroningen),sg(kroning)],de,[sbar,vp]).

n([pl(kronkels),sg(kronkel)],de,[],[dim(kronkeltje)]).

n([pl(kronen),sg(kroon)],de,[],[dim(kroontje)]).

n([meas(kroon),pl(kronen)],de,[meas_mod,measure]).

n([pl(kroongetuigen),sg(kroongetuige)],both,[sbar]).

n([pl(kroonjuwelen),sg(kroonjuweel)],het,[]).

n([pl(kroonluchters),sg(kroonluchter)],de,[]).

n([mass(kroost)],both,[]).  %Vlaams: de

n([pl(kroppen),sg(krop)],de,[measure]).

n([pl(krotten),sg(krot)],het,[]).

n([pl(kruiden),sg(kruid)],het,[],[tuin]).

n([pl(kruideniers),sg(kruidenier)],de,[]).

n([mass(kruidentee)],de,[]).

n([mass(kruidenthee)],de,[]).

n([pl(kruidnagels),sg(kruidnagel)],de,[]).

n([pl(kruiers),sg(kruier)],de,[]).

n([pl(kruiken),sg(kruik)],de,[measure]).

n([mass(kruim)],het,[],[brood]).

n([pl(kruimels),sg(kruimel)],de,[measure],[brood,dim(kruimeltje)]).

n([pl(kruinen),sg(kruin)],de,[],[dim(kruintje)]).

%% soort materiaalmoeheid oid?
n([mass(kruip)],both,[]).

n([sg(kruiper),pl(kruipers)],de,[]).

n([pl(kruisen),pl(kruizen),sg(kruis)],het,[],[dim(kruisje)]).

n([pl(kruisers),sg(kruiser)],de,[]).

n([pl(kruisigingen),sg(kruisiging)],de,[]).

n([pl(kruisingen),sg(kruising)],de,[],[h(t),i(t,'T-')]).

n([mass(kruisleger)],het,[]).

n([pl(kruispunten),sg(kruispunt)],het,[]).

n([pl(kruistekens),sg(kruisteken)],het,[]).

n([pl(kruistochten),sg(kruistocht)],de,[]).

n([pl(kruisvaarders),sg(kruisvaarder)],de,[]).

n([mass(kruit)],het,[]).

n([pl(kruiwagens),sg(kruiwagen)],de,[measure]).

n([pl(krukken),sg(kruk)],de,[],
  [deur,
   dim(krukje)]).

n([pl(krullen),sg(krul)],de,[],
  [dim(krulletje)]).

n([mass(krulhaar)],het,[]).

n([pl(kubussen),sg(kubus)],de,[]).

n([pl(kuchen),sg(kuch)],de,[],
  [dim(kuchje)]).

n([pl(kudden),pl(kuddes),sg(kudde)],de,[measure],[dim(kuddetje)]).

n([pl(kuiven),sg(kuif)],de,[]).

n([pl(kuikens),sg(kuiken)],het,[],[dim(kuikentje)]).

n([pl(kuilen),sg(kuil)],de,[]).

n([pl(kuilers),sg(kuiler)],de,[],[dim(kuiltje)]).

n([pl(kuipen),sg(kuip)],de,[measure],[dim(kuipje)]).

n([pl(kuipers),sg(kuiper)],de,[]).

n([sg(kuis)],de,[]).  % VL schoonmaak

n([mass(kuisheid)],de,[]).

n([pl(kuiten),sg(kuit)],de,[]).

n([pl(kulturen),sg(kultuur)],de,[]).

n([mass(kultuursociologie)],de,[]).

n([mass(kunde)],de,[],
  [bouw]).

n([pl(kundigheden),sg(kundigheid)],de,[sbar,vp]).

n([mass([kung,fu])],de,[]).

n([pl(kunsten),sg(kunst),ignore(m(kunst,noun(de,count,sg),kunst))],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp
  ],
  [dim(kunstje)]).

n([mass(kunst)],de,[],
  [beeldhouw,
   bouw,
   dicht,
   genees,
   klein,
   kook,
   media,
   schilder,
   spraak,
   toon,
   video]).

n([mass(['kunst-',en,vliegwerk]),
   mass([kunst,en,vliegwerk])],het,[]).

n([pl(kunstacademies),sg(kunstacademie)],de,[]).

n([pl(kunstenaars),sg(kunstenaar)],de,[],
  []).

n([pl(kunstenaressen),sg(kunstenares)],de,[]).

n([sg(kunstenaarschap),pl(kunstenaarschappen)],het,[]).

n([pl(kunstgebitten),sg(kunstgebit)],het,[]).

n([pl(kunstgrepen),sg(kunstgreep)],de,[sbar,vp]).

n([mass(kunstmest)],de,[]).

n([sg(kunstrijder),pl(kunstrijders)],de,[]).

n([sg(kunstrijdster),pl(kunstrijdsters)],de,[]).

n([pl(kunstschilders),sg(kunstschilder)],de,[]).

n([pl(kunststoffen),sg(kunststof)],both,[app_measure]).

n([mass(kunstuitleen)],de,[]).

n([pl(kunstvoorwerpen),sg(kunstvoorwerp)],het,[]).

n([pl(kunstvormen),sg(kunstvorm)],de,[]).

n([pl(kurken),sg(kurk)],de,[]).

n([mass(kurk)],het,[]).

n([pl(kurketrekkers),sg(kurketrekker)],de,[]).

n([pl(kursisten),sg(kursist)],de,[]).

n([pl(kussen),sg(kus)],de,[],[dim(kusje)]).

n([pl(kussens),sg(kussen)],het,[],[dim(kussentje)]).

n([pl(kusten),sg(kust)],de,[],
  [noord,
   oost,
   west,
   zuid]).

n([pl(kuststreken),sg(kuststreek)],de,[]).

n([sg(kustwacht),pl(kustwachten)],de,[]).

n([pl(kutten),sg(kut)],de,[],[dim(kutje)]).

n([meas(kuub),pl(kuubs)],both,[meas_mod,measure]).

n([pl(kuren),sg(kuur)],de,[],[dim(kuurtje)]).

n([pl(kwaden),sg(kwaad)],het,[]).

n([mass(kwaadaardigheid)],de,[]).

n([mass(kwaadheid)],de,[]).

n([mass(kwaads)],het,[]).

n([pl(kwalen),sg(kwaal)],de,[],
  [hart,
   maag,
   s(oudedag),
   dim(kwaaltje)]).

n([sg(kwab),pl(kwabben)],de,[]).

n([pl(kwadraten),sg(kwadraat)],het,[]).

n([pl(kwadranten),sg(kwadrant)],het,[]).

n([pl(kwajongens),sg(kwajongen)],de,[]).

n([sg(kwak),pl(kwakken)],de,[measure],
  [dim(kwakje)]).                            % een kwak mayonaise

n([sg(kwakkel),pl(kwakkels)],de,[]).

n([pl(kwakzalvers),sg(kwakzalver)],de,[]).

n([pl(kwallen),sg(kwal)],de,[],[dim(kwalletje)]).

n([pl(kwalificaties),sg(kwalificatie),
   pl(kwalifikaties),sg(kwalifikatie)],de,
  [app_measure,
   vp],
  []).

n([pl(kwalificaties),sg(kwalificatie),
   pl(kwalifikaties),sg(kwalifikatie)],de,
  [],
  [h('WK')]).

n([pl(kwaliteiten),sg(kwaliteit)],de,
  [sbar,
   vp,
   measure,  % de hoogste kwaliteit koffiebonen krijg je door ...
   pred_pp(van)],[]).

n([pl(kwaliteiten),sg(kwaliteit)],de,[],
  [s(geluid),
   s(leven),
   lucht,
   rij]).

n([pl(kwantiteiten),sg(kwantiteit)],de,[]).

n([mass(kwark)],de,[]).

n([meas(kwartier),pl(kwartieren)],het,
  [temp_mod,
   measure,
   vp,
   sbar],
  [dim(kwartiertje)]).

n([pl(kwarten),sg(kwart),ignore(m(kwart,noun(het,count,sg),kwart))],het,[measure]).
n([sg(kwart)],both,[]).

n([sg(kwartaal),pl(kwartalen)],het,[temp_mod,sbar,measure]).

n([pl(kwartetten),sg(kwartet)],het,[measure]).

n([stem(kwart_DIM),
   pl(kwartjes),sg(kwartje)],het,[measure,meas_mod]).

n([mass(kwarts)],both,[]).

n([pl(kwartslagen),sg(kwartslag)],de,[meas_mod]).

n([pl(kwasten),sg(kwast)],de,[measure],[dim(kwastje)]).
%% een kwastje verf

n([sg(kwebbel),pl(kwebbels)],de,[]).

n([pl(kweken),sg(kweek)],de,[]).

n([pl(kwekers),sg(kweker)],de,[]).

n([pl(kwekerijen),sg(kwekerij)],de,[],
  [hennep,
   dim(kwekerijtje)]).

n([mass(kwel)],de,[]).

n([pl(kwellingen),sg(kwelling)],de,[sbar,vp]).

n([pl(kwesties),sg(kwestie)],de,
  [sbar,
   vp,
   subject_sbar,
   van_sbar],
  [dim(kwestietje)]).

n([pl(kwesties),sg(kwestie)],de,[],
  [portefeuille]).

n([mass(kwetsbaarheid)],de,[subject_sbar]).

n([pl(kwetsuren),sg(kwetsuur)],de,[]).

n([mass(kwijl)],het,[]).

n([mass(kwijting)],de,[]).

n([sg(kwijtschelding),pl(kwijtscheldingen)],de,[]).

n([mass(kwik)],het,[]).

n([pl(kwinkslagen),sg(kwinkslag)],de,[]).

n([pl(kwinten),sg(kwint)],de,[]).

n([pl(kwintetten),sg(kwintet)],het,[measure]).

n([sg(kwispel),pl(kwispels)],de,[]).  % wijwaterkwast

n([sg(kür)],de,[]).

n([pl('l\'s'),sg(l)],de,[],[dim('l\'etje')]).

n([pl('la\'s'),pl(laas),sg(la)],de,[],
  [bureau,
   dim(laatje)]).

n([sg(laad)],de,[]).

n([pl(laadbakken),sg(laadbak)],de,[]).

n([sg(laaf),pl(laven)],de,[]).

n([pl(lagen),sg(laag)],de,
  [measure],
  [dim(laagje)]).

n([pl(lagen),sg(laag)],de,
  [],
  [s(bestuur),
   lak,
   ozon,
   vuil,
   dim(laagje)
  ]).

n([sg(laag)],het,[]).

n([pl(laaglanden),sg(laagland)],het,[]).

n([sg(laagte),pl(laagtes)],de,[]).

n([pl(lanen),sg(laan)],de,[],[dim(laantje)]).

n([pl(laarzen),sg(laars)],de,[],[rijg]).

n([pl(laatstejaars),sg(laatstejaars)],de,[]).

n([pl(laatstgenoemden),sg(laatstgenoemde)],de,[measure]).

n([pl(labs),sg(lab)],het,[]).

n([pl(labels),sg(label)],both,[],
  [bagage,
   erfgoed,
   i(plaat,platen),
   s(waarschuwing),
   dim(labeltje)]).

n([pl(laboratoria),pl(laboratoriums),sg(laboratorium)],het,[]).

n([pl(labyrinten),sg(labyrint)],het,[]).

n([sg(lach)],de,[],[schater,
                    dim(lachje)]).

n([sg(lachertje),pl(lachertjes)],het,[]).

n([mass(lactoferrine)],de,[]).

n([pl(lacunes),sg(lacune)],de,[]).

n([pl(ladders),sg(ladder)],de,[],
  [toon,
   dim(laddertje)]).

n([pl(laden),sg(lade)],de,[]).

n([pl(ladingen),sg(lading)],de,[measure],
  [boot,
   bus,
   dek,
   koffer,
   magazijn,
   i(schip,scheeps),
   vliegtuig,
   vracht,
   wagon,
   dim(ladinkje)]).

n([pl('lady\'s'),sg(lady)],de,[],[dim('lady\'tje')]).

n([pl(lafaards),sg(lafaard)],de,[]).

n([pl(lafheden),sg(lafheid)],de,[vp]).

n([pl(lagers),sg(lager)],de,[]).

n([pl(lagunen),pl(lagunes),sg(lagune)],de,[]).

n([sg(laguna)],de,[]).

n([mass(lak)],de,[],
  [nagel]).

n([pl(lakken),sg(lak)],het,[]).

n([pl(lakeien),sg(lakei)],de,[]).

n([pl(lakens),sg(laken)],het,[],[dim(lakentje)]).

n([sg(lakmoes)],both,[]).

n([pl(lammeren),sg(lam)],het,[],[dim(lammetje)]).

n([pl('lama\'s'),sg(lama)],de,[]).

n([pl(lambrizeringen),sg(lambrizering)],de,[]).

n([sg(lamel),pl(lamellen)],de,[]).

n([pl(lampen),sg(lamp)],de,[],
  [binnen,
   buiten,
   bureau,
   mist,
   nacht,
   olie,
   petroleum,
   radio,
   spaar,
   zak,
   zwaai,
   dim(lampje)]).

n([pl(lampions),sg(lampion)],de,[]).

n([pl(lanceringen),sg(lancering)],de,[]).

n([mass(lancé)],het,[]).

n([pl(landen),sg(land)],het,[],
  [h('ACS'),
   buur,
   donor,
   i(droom,dromen),
   geboorte,
   industrie,
   i(laag_loon,'lage-lonen'),i(laag_loon,'lagelonen'),
   h(kandidaat),
   moeder,
   moslim,
   h('EU'),
   h('G7'),
   h('G8'),
   h('NAVO'),'NAVO',h('Navo'),'Navo',
   h('OESO'),
   s(ontwikkeling),
   remigratie,h(remigratie),
   h('Schengen'),'Schengen',
   thuis,
   tussen,
   wintersport,
   dim(landje)]).

n([mass(landjepik)],het,[]).
n([mass('landje-pik')],het,[]).

n([mass(['land-',en,tuinbouw])],de,[]). % troonrede: land- en tuinbouw zijn innovatief

n([mass(landbouw)],de,[]).

n([pl(landbouwers),sg(landbouwer)],de,[]).

n([pl(landbouwschappen),sg(landbouwschap)],het,[]).

n([pl(landerijen),sg(landerij)],de,[]).

n([pl(landgoederen),sg(landgoed)],het,[]).

n([pl(landheren),sg(landheer)],de,[]).

n([pl(landhuizen),sg(landhuis)],het,[]).

n([pl(landingen),sg(landing)],de,[],[nood]).

n([pl(landingsbanen),sg(landingsbaan)],de,[]).

n([pl(landlopers),sg(landloper)],de,[]).

n([sg(landmacht),pl(landmachten)],de,[]).

n([sg(landmijn),pl(landmijnen)],de,[]).

n([pl(landschappen),sg(landschap)],het,[],
  [media,
   winter,
   dim(landschapje)]).

n([pl(landsheren),sg(landsheer)],de,[]).

n([pl(landstreken),sg(landstreek)],de,[]).

n([sg(landstitel),pl(landstitels)],de,[]).

n([pl(landtongen),sg(landtong)],de,[]).

n([pl(landverraders),sg(landverrader)],de,[]).

n([pl(landvoogden),sg(landvoogd)],de,[]).

n([mass(langebaan)],de,[]).

n([mass(langlauf)],both,[]).

n([sg(langlijf),pl(langlijven)],de,[]).

n([pl(lansen),sg(lans)],de,[]).

n([pl(lantaarns),sg(lantaarn),
   pl(lantarens),sg(lantaren)],de,[],
  [zak,
   dim(lantaarntje),
   dim(lantarentje)]).

n([pl(lantaarnpalen),sg(lantaarnpaal)],de,[]).

n([pl(lappen),sg(lap)],de,[measure],[dim(lapje)]).

n([sg(laptop),pl(laptops)],de,[]).

n([stem(larf),pl(larven),sg(larve),sg(larf)],de,[]).

n([sg(las),pl(lassen)],de,[]).

n([mass(lasagne),mass(lasanga)],de,[]).

n([pl(lasers),sg(laser)],de,[]).

n([pl([laser,guns]),sg([laser,gun])],de,[]).

n([sg(lasser),pl(lassers)],de,[]).

n([pl('lasso\'s'),sg(lasso)],de,[],[dim(lassootje)]).

n([pl(lasten),sg(last)],de,
  [sbar,
   vp,
   pred_pp(tot),
   pred_pp(in),
   measure % de last gele kaarten
  ]).

n([pl(lasten),sg(last)],de,
  [],
  [pensioen]).

n([sg([last,minute]),sg('last-minute'),sg(lastminute)],de,[]).

n([mass(laster)],de,[]).

n([sg(lastpak),pl(lastpakken)],de,[]).

n([sg(lastpost),pl(lastposten)],de,[]).

n([pl(latten),sg(lat)],de,[]).

n([mass(latex)],both,[]).

n([pl(laureaten),sg(laureaat)],de,[]).

n([pl(laurieren),sg(laurier)],de,[]).

n([pl(laurierbladen),pl(laurierbladeren),pl(laurierblaren),sg(laurierblad)],het,[],[dim(laurierblaadje)]).

n([pl(lauweren)],both,[]).

n([mass(lava)],de,[]).

n([mass(lavendel)],de,[]).

n([sg([law,lord]),pl([law,lords])],de,[]).

n([mass(lawaai)],het,[],
  [vliegtuig]).

n([pl(lawinen),pl(lawines),sg(lawine)],de,[]).

n([stem('lay-out'),
   sg(layout),sg('lay-out'),
   pl(layouts),pl('lay-outs')],de,[]).

n([sg(lazer)],both,[]).  % pak op mijn lazer

n([sg([leading,indicator]),pl([leading,indicators])],de,[]).

n([mass(lease)],de,[],
  [i(aandeel,aandelen)]).

n([pl(lectoren),pl(lectors),sg(lector)],de,[app_measure]).

n([mass(lectuur)],de,[]).

n([pl(ledentallen),sg(ledental)],het,[]).

n([mass(leder)],het,[]).

n([mass(ledigheid)],de,[]).

n([pl(ledikanten),sg(ledikant)],het,[]).

n([mass(leed)],het,[],
  [i(dier,dieren)]).

n([mass(leedvermaak)],het,[]).

n([mass(leedwezen)],het,[]).

n([mass(leefbaarheid)],de,[]).

n([pl(leefgemeenschappen),sg(leefgemeenschap)],de,[]).

n([stem(leef_milieu),
   pl(leefmilieus),sg(leefmilieu)],het,[]).

n([mass(leefomgeving)],de,[]).

n([pl(leefregels),sg(leefregel)],de,[sbar,vp]).

n([pl(leeftijden),sg(leeftijd)],de,[sbar]).

n([pl(leeftijden),sg(leeftijd)],de,[],
  [h('AOW'),
   pensioen]).

n([pl(leefwijzen),sg(leefwijze)],de,[]).

n([mass(leegloop)],de,[]).

n([mass(leegstand)],de,[]).

n([pl(leegten),pl(leegtes),sg(leegte)],de,[]).

n([pl(leken),sg(leek)],de,[]).

n([mass(leem)],both,[]).

n([pl(leemten),pl(leemtes),sg(leemte)],de,[sbar,vp]).

n([pl(lenen),sg(leen)],het,[pred_pp(in)]).

n([pl(leren),sg(leer)],de,[sbar]).

n([pl(leren),sg(leer)],de,[],
  [evolutie,
   maatschappij,
   s(zin)]).

n([pl(leren),sg(leer)],het,[],[dim(leertje)]).

n([pl(leergangen),sg(leergang)],de,[]).

n([sg(leerkracht),pl(leerkrachten)],de,[app_measure]).

n([pl(leerlingen),sg(leerling)],de,[],
 [s(achterstand),
  mede,
  h(oud)]).

n([pl(leerlinges),sg(leerlinge)],de,[],
 [s(achterstand),
  mede,
  h(oud)]).

n([mass(leerlingwezen)],het,[]).

n([pl(leermeesters),sg(leermeester)],de,[]).

n([pl(leerstellingen),sg(leerstelling)],de,[sbar,vp]).

n([pl(leerstoelen),sg(leerstoel)],de,[app_measure]).

n([pl(leerstukken),sg(leerstuk)],het,[]).

n([pl(leesten),sg(leest)],de,[]).

n([pl(leeszalen),sg(leeszaal)],de,[]).

n([pl(leeuwen),sg(leeuw)],de,[],
  [zee,
   dim(leeuwtje)]).

n([pl(leeuwendelen),sg(leeuwendeel),sg(leeuwedeel),pl(leeuwedelen)],het,[]).

n([pl(leeuweriken),sg(leeuwerik)],de,[]).

n([mass(lef)],both,[vp]).

n([mass(leg)],de,[pred_pp(aan),pred_pp(van)]).

n([sg(leg),pl(legs)],de,[]).  % dart

%% notarieel: het
%% pauselijk vertegenwoordiger: de
n([pl(legaten),sg(legaat)],both,[]).

n([pl(legaliseringen),sg(legalisering)],de,[]).

n([pl(legenden),pl(legendes),sg(legende)],de,[sbar]).

n([pl(legers),sg(leger)],het,[measure],
  [dim(legertje)]).

n([pl(legers),sg(leger)],het,[],
  [s(bevrijding),
   expeditie,
   s(regering),
   dim(legertje)]).

n([sg(['Het','Leger',des,'Heils'])],het,[]).

n([sg(legering),pl(legeringen)],de,[app_measure]).

n([sg(legger),pl(leggers)],de,[],
  [i(bom,bommen)]).

n([sg(legging),pl(leggings)],de,[]).

n([pl(legioenen),sg(legioen)],het,[measure]).

n([sg(legionair),pl(legionairs)],de,[]).

n([mass(legionella)],de,[]).

n([mass(legislatuur)],de,[temp_mod]).   % VL De voorbije legislatuur zetelde de VLD in de oppositie .


n([pl(legitimaties),sg(legitimatie)],de,[]).

n([pl(legitimeringen),sg(legitimering)],de,[]).

n([mass(legitimiteit)],de,[]).

n([mass(lego)],de,[]).

n([pl(leien),sg(lei)],de,[],[dim(leitje)]).

n([pl(leiders),sg(leider)],de,[],
  [h('ANC'),
   s(bedrijf),
   campagne,
   h('CD'),
   h('CDA'),
   i('D66','D\'66-'),h('D66'),
   delegatie,
   ei,
   fractie,
   i(fractie,fraktie),
   i('CD_fractie','CD-fractie'),
   i('CDA_fractie','CDA-fractie'),
   i('D66_fractie','D66-fractie'),
   i('D66_fractie','D\'66-fractie'),
   i('GroenLinks_fractie','GroenLinks-fractie'),
   i('LPF_fractie','LPF-fractie'),
   i('PvdA_fractie','PvdA-fractie'),
   i('SP_fractie','SP-fractie'),
   i('VVD_fractie','VVD-fractie'),
   i('CD_fractie','CD-fraktie'),
   i('CDA_fractie','CDA-fraktie'),
   i('D66_fractie','D66-fraktie'),
   i('D66_fractie','D\'66-fraktie'),
   i('GroenLinks_fractie','GroenLinks-fraktie'),
   i('LPF_fractie','LPF-fraktie'),
   i('PvdA_fractie','PvdA-fraktie'),
   i('SP_fractie','SP-fraktie'),
   i('VVD_fractie','VVD-fraktie'),
   [geestelijk],
   gewest,
   h('GroenLinks'),
   s(groep),
   h('Inkatha'),
   s(klassement),
   h('LPF'),
   leger,
   i(oud_leger,'oud-leger'),
   i(ex_leger,'ex-leger'),
   markt,
   i(wereld_markt,wereldmarkt),
   militie,
   oppositie,
   partij,
   h('PKK'),
   h('PLO'),
   ploeg,
   project,
   i(rebel,rebellen),
   h('PvdA'),
   h('PVV'),
   s(regering),
   reis,
   school,
   h('SP'),
   s(staking),
   team,
   i(terrorist,terroristen),
   s(vakbond),
   s(verkeer),
   s(verzet),
   h('VVD'),
   wereld]).

n([mass(leiderschap)],het,[],[partij]).

n([pl(leidingen),sg(leiding)],de,[pred_pp(aan)],[]).

n([pl(leidingen),sg(leiding)],de,[],
  [s(bedrijf),
   club,
   concern,
   fractie,
   korps,
   leger,
   i(lucht_verkeer,luchtverkeers),
   partij,
   ploeg,
   school,
   s(verkeer),
   wedstrijd]).

n([pl(leidingen),sg(leiding)],de,[],
  [gas,
   s(elektriciteit),
   i(elektriciteit,electriciteits),
   s(hoogspanning),
   olie,
   pijp,
   i(olie_pijp,oliepijp),
   water,
   dim(leidinkje)]).

n([mass(leidinggeven)],het,[]).

n([pl(leidraden),sg(leidraad)],de,[]).

n([pl(leidslieden),sg(leidsman)],de,[]).

n([pl(leidsters),sg(leidster)],de,[],
  [oppositie]).

n([pl(lekken),sg(lek)],both,[],[gas]).

n([sg(lekkage),pl(lekkages)],de,[]).

n([pl(lekkernijen),sg(lekkernij)],de,[],[dim(lekkernijtje)]).

n([mass(lekkers)],het,[]).

n([pl(lektoren),pl(lektors),sg(lektor)],de,[]).

n([mass(lektuur)],de,[]).

n([pl(lelies),pl(leliën),sg(lelie)],de,[],[dim(lelietje)]).

n([mass(lelijkheid)],de,[]).

n([sg(lemma),pl('lemma\'s')],het,[app_measure,
				  np_app_measure]).

n([pl(lemmeten),sg(lemmet)],het,[]).

n([pl(lenden),pl(lendenen),sg(lende)],de,[]).

n([pl(lendendoeken),sg(lendendoek)],de,[]).

n([sg(leng),pl(lengen)],de,[]).  % soort vis. 

n([pl(lengten),pl(lengtes),sg(lengte)],de,[],
  [s(lichaam),
   oor,
   staart]).

n([pl(leningen),sg(lening)],de,[],
  [obligatie,
   s(staat)]).

n([pl(lenzen),sg(lens)],de,[]).

n([pl(lentes),sg(lente)],de,[temp_mod,sbar]).

n([pl(lepels),sg(lepel)],de,[meas_mod,measure],
  [dessert,
   eet,
   pap,
   soep,
   thee,
   dim(lepeltje)]).

n([pl(lepelaars),pl(lepelaren),sg(lepelaar)],de,[]).

n([pl(leraars),pl(leraren),sg(leraar)],de,[app_measure]).

n([pl(leraressen),sg(lerares)],de,[app_measure]).

n([pl(leringen),sg(lering)],de,[sbar,vp]).

n([sg([les,bleus]),
   pl([les,bleus])],both,[]).

n([sg(lesbienne),pl(lesbiennes)],de,[]).

n([sg(lesbo),pl('lesbo\'s')],de,[]).

n([pl(lessen),sg(les)],de,
  [sbar,
   vp,
   subject_sbar
  ],
  [s(leven),
   dim(lesje)]).

n([pl(lessen),sg(les)],de,
  [app_measure],
  [bij,
   dim(lesje)]).

n([pl(lessen),sg(les)],de,[],
  [dans,
   piano,
   rij,
   taal,
   s(verkeer),
   zang,
   zwem]).

n([pl(lessenaars),sg(lessenaar)],de,[],[dim(lessenaartje)]).

n([sg(let)],de,[]).

n([pl(letsels),sg(letsel)],het,[],
  [arm,
   been,
   buik,
   hersen,
   hoofd
  ]).

n([pl(letters),sg(letter)],de,
  [sbar,
   app_measure],
  [hoofd,
   dim(lettertje)]).

n([pl(lettergrepen),sg(lettergreep)],de,[app_measure]).

n([mass(letterkunde)],de,[]).

n([pl(letteren)],de,[]).

n([sg(letterkundige),pl(letterkundigen)],de,[]).

n([sg(leu),pl(lei)],de,[]).  % Roemeense munt

n([pl(leugens),sg(leugen)],de,[subject_sbar,subject_vp,sbar,vp],[dim(leugentje)]).

n([pl(leugenaars),sg(leugenaar)],de,[]).

n([mass(leukemie)],de,[]).

n([mass(leuks)],het,[]).

n([mass(leukte)],de,[]).

n([pl(leuningen),sg(leuning)],de,[],
  [i(achter_bank,achterbank),
   arm,
   bank,
   rug,
   trap]).

n([sg(leuter),pl(leuters)],de,[]).
%% duoman: laat ze toch met hun leuter zwaaien

n([sg(leus),pl(leuzen),sg(leuze)],de,[start_app_measure]).

n([meas(leva)],de,[meas_mod,measure]).

n([sg(level),pl(levels)],het,[]).  % 'op welk level ben je?'

n([sg([level,playing,field])],het,[]).

n([sg(leven),pl(levens)],het,
  [temp_mod,
   sbar,
   pred_pp(in)],
  [dim(leventje)]).

n([sg(leven),pl(levens),
   ignore_stem(leven),
   ignore_stem(leven_DIM)],het,
  [],
  [i(boer,boeren),
   buiten,
   s(gemoed),
   s(gezin),
   s(liefde),
   muziek,
   privé,
   h(privé),
   s(stad),
   s(vereniging),
   i(zaak,zaken),
   i(ziel,ziele),
   dim(leventje)]).

n([mass(levendigheid)],de,[]).

n([sg(levenlang)],both,[temp_mod],[]).

n([stem(leven_beëindiging),
   sg(levensbeëindiging),pl(levensbeëindigingen)],de,[]).

n([pl(levensbehoeften),sg(levensbehoefte)],de,[sbar,vp]).

n([pl(levensbelangen),sg(levensbelang)],het,
  [sbar,
   vp,
   pred_pp(van),
   pred_pp(van,subject_sbar),
   pred_pp(van,subject_vp)
  ]).

n([pl(levensbeschouwingen),sg(levensbeschouwing)],de,[sbar,vp]).

n([pl(levensbeschrijvingen),sg(levensbeschrijving)],de,[]).

n([mass(levensduur)],de,[]).

n([mass(levenservaring)],de,[]).

n([pl(levensfasen),pl(levensfases),sg(levensfase)],de,[]).

n([pl(levensfazen),pl(levensfazes),sg(levensfaze)],de,[]).

n([mass(levensgevaar)],het,
  [pred_pp(in),
   pred_pp(buiten)
  ]).

n([pl(levenshoudingen),sg(levenshouding)],de,[sbar,vp]).

n([pl(levenskunstenaars),sg(levenskunstenaar)],de,[]).

n([stem(leven_lang),
   mass(levenslang)],het,[]).	% veroordelen tot -; hij krijgt -

n([mass(levensloop)],de,[]).

n([mass(levenslust)],de,[]).

n([mass(levensonderhoud)],het,[]).

n([pl(levensopvattingen),sg(levensopvatting)],de,[sbar]).

n([pl(levensovertuigingen),sg(levensovertuiging)],de,[sbar,vp]).

n([pl(levenspatronen),sg(levenspatroon)],het,[]).

n([mass(levenspeil)],het,[]).

n([pl(levensverhalen),sg(levensverhaal)],het,[]).

n([mass(levensverwachting),pl(levensverwachtingen),sg(levensverwachting)],de,[]).

n([mass(levensvisie)],de,[]).

n([pl(levensvormen),sg(levensvorm)],de,[]).

n([mass(levensvreugde)],de,[]).

n([mass(levenswandel)],de,[]).

n([pl(levenswijzen),sg(levenswijze)],de,[sbar,vp]).

n([pl(levers),sg(lever)],de,[],
  [kunst,
   dim(levertje)]).

n([pl(leveranciers),sg(leverancier)],de,[],[hof]).

n([pl(leveranties),sg(leverantie)],de,[],
  [wapen]).

n([pl(leveringen),sg(levering)],de,[]).

n([mass(levertraan)],de,[]).

n([sg(lexicograaf),pl(lexicografen)],de,[]).

n([mass(lexicografie)],de,[]).

n([sg(lexicoloog),pl(lexicologen)],de,[]).

n([mass(lexicologie)],de,[]).

n([sg(lexicon),pl(lexicons),pl(lexica)],het,[]).

n([pl(lezers),sg(lezer)],de,[],[dim(lezertje)]).

n([pl(lezeressen),sg(lezeres)],de,[]).

n([pl(lezingen),sg(lezing)],de,[sbar,vp],
  [['5',mei],
   dim(lezinkje)]).

n([pl(lezingen),sg(lezing)],de,[],
  [her,
   'Schrift']).

n([pl('li\'s'),sg(li)],de,[]).

n([pl(lianen),sg(liane)],de,[]).

n([pl(libellen),sg(libel)],de,[],[dim(libelletje)]).

n([pl(liberalen),sg(liberaal)],de,[],[h(sociaal)]).

n([pl(liberaliseringen),sg(liberalisering)],de,[],
  [visum]).

n([pl(liberalisaties),sg(liberalisatie)],de,[],[]).

n([mass(liberalisme)],het,[],[neo]).

n([sg(libero)],de,[]).

n([mass(libido)],both,[]). % celex: de

n([mass(libre)],het,[]).

n([sg(libretto),pl(libretti),pl('libretto\'s')],both,[]).

n([sg(licentiaat),pl(licentiaten)],both,[app_measure]).

n([pl(licenties),sg(licentie)],de,[]).

n([pl(lichamen),sg(lichaam)],het,[],[dim(lichaampje)]).

n([mass(lichaamsbouw)],de,[]).

n([pl(lichaamsdelen),sg(lichaamsdeel)],het,[]).

n([mass(lichamelijkheid)],de,[]).

n([pl(lichten),sg(licht)],het,[],
  [achter,
   binnen,
   buiten,
   dag,
   dim,
   kaars,
   knipper,
   lamp,
   s(leven),
   maan,
   i(mist_achter,mistachter),
   morgen,
   ochtend,
   pool,
   schemer,
   stop,
   s(verkeer),
   voet,
   voor,
   zoek,
   zon,
   zwaai,
   dim(lichtje)]).

n([pl(lichtbronnen),sg(lichtbron)],de,[]).

n([pl(lichtbundels),sg(lichtbundel)],de,[]).

n([sg(lichter),pl(lichters)],de,[]).

n([pl(lichtingen),sg(lichting)],de,[pred_pp(van),measure]).

n([pl(lichtjaren),sg(lichtjaar)],het,[measure]).

n([pl(leden),sg(lid)],het,[app_measure],
  [s(bestuur),
   commissie,
   directie]).

n([pl(leden),sg(lid)],het,[],
  [band,
   s(bemanning),
   bende,
   h('CD'),
   h('CDA'),
   congres,
   i(congres,'Congres'),
   i('D66','D\'66-'),h('D66'),
   s(deelraad),
   'Doema',
   ere, % uit Diever
   familie,
   fractie,
   i(fractie,fraktie),
   i('CD_fractie','CD-fractie'),
   i('CDA_fractie','CDA-fractie'),
   i('D66_fractie','D66-fractie'),
   i('D66_fractie','D\'66-fractie'),
   i('GroenLinks_fractie','GroenLinks-fractie'),
   i('LPF_fractie','LPF-fractie'),
   i('PvdA_fractie','PvdA-fractie'),
   i('SP_fractie','SP-fractie'),
   i('VVD_fractie','VVD-fractie'),
   i('CD_fractie','CD-fraktie'),
   i('CDA_fractie','CDA-fraktie'),
   i('D66_fractie','D66-fraktie'),
   i('D66_fractie','D\'66-fraktie'),
   i('GroenLinks_fractie','GroenLinks-fraktie'),
   i('LPF_fractie','LPF-fraktie'),
   i('PvdA_fractie','PvdA-fraktie'),
   i('SP_fractie','SP-fraktie'),
   i('VVD_fractie','VVD-fraktie'),
   gemeente,
   i(gemeente_raad,gemeenteraads),
   s(gezin),
   h('GroenLinks'),
   s(groep),
   h('IOC'),
   jury,
   kader,
   kamer,h(kamer),
   i(kamer,'Kamer'),
   i(kamer,'Kamer-'),
   ['1e','Kamer'],
   ['2e','Kamer'],
   ['1e','kamer'],
   ['2e','kamer'],
   wh(['1e','Kamer']),
   wh(['2e','Kamer']),
   wh(['1e','kamer']),
   wh(['2e','kamer']),
   'Eerste-Kamer',['Eerste','Kamer'],
   'Tweede-Kamer',['Tweede','Kamer'],
   i('CD_kamer','CD-kamer'),
   i('CDA_kamer','CDA-kamer'),
   i('D66_kamer','D66-kamer'),
   i('D66_kamer','D\'66-kamer'),
   i('GroenLinks_kamer','GroenLinks-kamer'),
   i('LPF_kamer','LPF-kamer'),
   i('PvdA_kamer','PvdA-kamer'),
   i('SP_kamer','SP-kamer'),
   i('VVD_kamer','VVD-kamer'),
   i('CD_kamer','CD-Kamer'),
   i('CDA_kamer','CDA-Kamer'),
   i('D66_kamer','D66-Kamer'),
   i('D66_kamer','D\'66-Kamer'),
   i('GroenLinks_kamer','GroenLinks-Kamer'),
   i('LPF_kamer','LPF-Kamer'),
   i('PvdA_kamer','PvdA-Kamer'),
   i('SP_kamer','SP-Kamer'),
   i('VVD_kamer','VVD-Kamer'),
   kerk,
   'Lagerhuis',
   h('LPF'),
   militie,
   oog,
   orkest,
   s(parlement),
   i(parlement,'parlements-'),
   partij,
   panel,
   s(personeel),
   s(raad),
   i('CD_raad','CD-raad'),
   i('CDA_raad','CDA-raad'),
   i('D66_raad','D66-raad'),
   i('D66_raad','D\'66-raad'),
   i('GroenLinks_raad','GroenLinks-raad'),
   i('LPF_raad','LPF-raad'),
   i('PvdA_raad','PvdA-raad'),
   h('PvdA'),
   i('SP_raad','SP-raad'),
   h('SP'),
   sekte,
   staf,
   i(staat,staten),i(staat,'Staten'),
   s(vakbond),
   h('VVD'),
   dim(lidje)
  ]).

n([pl(ledematen),sg(lidmaat),sg(ledemaat)],het,[]).

n([pl(lidmaatschappen),sg(lidmaatschap)],het,[],
  [h('EU'),
   kamer,'Kamer']).

n([pl(lidstaten),sg(lidstaat)],de,[],
  [h('EU'),
   h(kandidaat),kandidaat]).

n([pl(lieden),pl(lui)],both,[],
  [brandweer,
   buur,
   s(handel),
   top]).

n([pl(liederen),sg(lied)],het,[start_app_measure],
  [club,
   kerst,
   i(kind,kinder),
   klaag,
   s(liefde),
   lijf,
   lof,
   s(leven),
   s(opening),
   protest,
   slaap,
   slot,
   strijd,
   welkomst,
   zigeuner,
   dim(liedje)]).

n([pl(lieven),sg(lief)],het,[],
  [dochter,
   man,
   vrouw,
   zoon,
   dim(liefje)]).

n([mass(liefdadigheid)],de,[]).

n([pl(liefden),pl(liefdes),sg(liefde)],de,[]).

n([pl(liefdesbrieven),sg(liefdesbrief)],de,[]).

n([sg(liefdespaar),pl(liefdesparen)],het,[]).

n([mass(liefdesverdriet)],het,[]).

n([mass([liefdewerk,oud,papier])],both,[]).

n([pl(liefhebbers),sg(liefhebber)],de,[],
  [koffie,
   voetbal]).

n([pl(liefhebsters),sg(liefhebster)],de,[],
  [koffie,
   voetbal]).

n([pl(liefhebberijen),sg(liefhebberij)],de,[]).

n([pl(liefkozingen),sg(liefkozing)],de,[]).

n([mass(liefs)],het,[]).

n([pl(lieren),sg(lier)],de,[]).

n([pl(liezen),sg(lies)],de,[],[dim(liesje)]).

n([sg(lieveheersbeestje),
   pl(lieveheersbeestjes)],het,[]).

n([pl(lievelingen),sg(lieveling)],de,[],
  [s(publiek),
   dim(lievelingetje)]).

n([pl(lieverds),sg(lieverd)],de,[]).

n([mass(lifestyle)],de,[]).

n([pl(liften),sg(lift)],de,[]).

n([pl('liga\'s'),sg(liga)],de,[]).

n([sg(ligbad),pl(ligbaden)],het,[]).

n([pl(liggingen),sg(ligging)],de,[]).

n([sg([light,future]),pl([light,futures])],de,[]).

n([sg([light,rail]),pl([light,rails])],de,[]).

n([sg([light,verse])],de,[]).

n([mass(lijden)],het,[]).

n([pl(lijders),sg(lijder)],de,[]).

n([pl(lijven),sg(lijf)],het,[],
  [achter,
   boven,
   onder,
   dim(lijfje)]).

n([pl(lijfeigenen),sg(lijfeigene)],de,[]).

n([pl(lijfwachten),sg(lijfwacht)],de,[]).

n([pl(lijken),sg(lijk)],het,[]).

n([pl(lijkenhuizen),sg(lijkenhuis)],het,[],[dim(lijkenhuisje)]).

n([pl(lijkenpikkers),sg(lijkenpikker)],de,[]).

n([pl(lijkschouwers),sg(lijkschouwer)],de,[]).

n([mass(lijm)],de,[]).

n([pl(lijnen),sg(lijn)],de,
  [subject_sbar,
   subject_vp,
   sbar,
   vp],
  [s(beleid),
   s(begroting),
   s(bestand),
   bodem,
   s(gedrag),
   hoofd,
   i(kader_richt,kaderricht)]).

n([pl(lijnen),sg(lijn)],de,[],
  [achter,
   breuk,
   bus,
   doel,
   finish,
   front,
   i(hoog_snelheid,hogesnelheids),
   i(hoog_snelheid,'hoge-snelheids'),
   hoogte,
   kust,
   metro,
   middel,midden,
   'Noord-Zuid',
   nul,
   pijp,  % pipeline :-(
   productie,i(productie,produktie),
   raak,
   s(scheid),
   spoor,
   stroom,
   telefoon,
   tram,
   verhaal,
   was,
   uit,
   zij]).

n([mass(lijnolie)],de,[]).

n([pl(lijnrechters),sg(lijnrechter)],de,[]).

n([stem(lijn_DIM),pl(lijntjes),sg(lijntje)],het,[measure]). % cocaine

n([pl(lijsten),sg(lijst)],de,[measure],
  [ere,
   i(kandidaat,kandidaten),
   kies,
   s(plaatsing),
   s(opsporing),
   presentie,
   i(vraag,vragen),
   was,
   dim(lijstje)]).

n([pl(lijsten),sg(lijst)],de,[],
  [i(boek,boeken),
   cijfer,
   i(gast,gasten),
   hit,
   loon,
   prijs,
   i(tarief,tarieven),
   verlang,
   wacht,
   i(woord,woorden),
   zaad,
   dim(lijstje)]).

n([pl(lijsters),sg(lijster)],de,[],[dim(lijstertje)]).

n([pl(lijsttrekkers),sg(lijsttrekker)],de,[],
  [h('VVD'),
   h('LPF'),
   h('CD'),
   h('CDA'),
   h(duo),
   h('GroenLinks'),
   h('PvdA'),
   h('SP'),
   i('D66','D\'66-'),h('D66')]).

n([pl(lijsttreksters),sg(lijsttrekster)],de,[],
  [h('VVD'),
   h('LPF'),
   h('CD'),
   h('CDA'),
   h(duo),
   h('GroenLinks'),
   h('PvdA'),
   h('SP'),
   i('D66','D\'66-'),h('D66')]).

n([sg(lijsttrekkerschap),pl(lijsttrekkerschappen)],het,[]).

n([pl(likken),sg(lik)],de,[measure],[dim(likje)]).

n([sg(like),pl(likes)],de,[]).

n([pl(likeuren),sg(likeur)],de,[],[dim(likeurtje)]).

n([pl(likwidaties),sg(likwidatie)],de,[]).

n([pl(limieten),sg(limiet)],de,[],[s(tijd)]).

n([pl(limonades),sg(limonade)],de,[]).

n([pl(limousines),sg(limousine)],de,[]).

n([pl(linden),sg(linde)],de,[]).

n([pl(lindebomen),sg(lindeboom)],de,[]).

n([mass([line,dance])],de,[]).

n([sg('line-up')],de,[]).

n([mass(lingerie)],de,[]).

n([pl(linialen),sg(liniaal)],de,[],[dim(liniaaltje)]).

n([pl(linies),sg(linie)],de,[meas_mod],  % schoof een linie naar voren
  []).

n([pl(linies),sg(linie),ignore_stem(linie)],de,[],
  [front]).

n([sg([lingua,franca])],de,[]).

n([pl(links),sg(link),pl(linken)],de,[]).

n([mass(links)],het,[],
  [extreem,h(extreem),
   ultra,h(ultra)
  ]).

n([sg(linker)],both,[]).

n([pl(linkerkanten),sg(linkerkant)],de,[]).

n([pl(linkervleugels),sg(linkervleugel)],de,[]).

n([pl(linksachters),sg(linksachter)],de,[]).

n([pl(linksbacks),sg(linksback)],de,[]).

n([sg(linksbinnen),pl(linksbinnens)],de,[]).

n([sg(linksbuiten),pl(linksbuitens)],de,[]).

n([sg(linkshalf),pl(linkshalven)],de,[]).

n([sg(linksmidden),pl(linksmiddens)],de,[]).

n([pl(linksvoors),sg(linksvoor)],de,[]).

n([pl(linnens),sg(linnen)],het,[]).

n([mass(linnengoed)],het,[]).

n([pl(linnenkasten),sg(linnenkast)],de,[]).

n([mass(linoleum)],het,[]).

n([pl(linten),sg(lint)],het,[],[dim(lintje)]).

n([pl(linzen),sg(linze)],de,[]).

n([pl(lippen),sg(lip)],de,[]).

n([pl(lippenstiften),sg(lippenstift)],de,[]).

n([pl(lipsticks),sg(lipstick)],de,[]).

n([pl(liquidaties),sg(liquidatie)],de,[]).

n([meas(lira)],de,[meas_mod,measure]).

n([meas(lire),pl(lires)],de,[meas_mod,measure]).

n([pl(listen),sg(list)],de,[]).

n([pl(litanieën),sg(litanie)],de,[]).

n([meas(liter),pl(liters),
   meas(l),
   meas('l.')],de,[meas_mod,measure],
  [centi,
   deci,
   hecto,
   milli]).

n([pl(literatoren),sg(literator)],de,[]).

n([pl(literaturen),sg(literatuur)],de,[],[jeugd]).

n([mass(literatuurwetenschap)],de,[]).

n([pl(litertjes),sg(litertje)],het,[measure]).

n([mass(lithium)],both,[]).

n([pl('litho\'s'),sg(litho)],de,[]).

n([pl(littekenen),pl(littekens),sg(litteken)],het,[],[dim(littekentje)]).

n([pl(liturgieën),sg(liturgie)],de,[]).

n([pl(livings),sg(living)],de,[]).

n([sg('L.O.')],de,[app_measure]).

n([sg(lob),pl(lobs),pl(lobben)],de,[]).

n([pl('lobby\'s'),pl(lobbies),sg(lobby)],de,[]).

n([pl(lobbyisten),sg(lobbyist)],de,[]).

n([stem(locatie),
   pl(locaties),sg(locatie),
   pl(lokaties),sg(lokatie)],de,[]).

n([pl(locomotieven),sg(locomotief)],de,[]).

n([pl(loeders),sg(loeder)],het,[]).

n([sg(loef),pl(loeven)],de,[]).

n([sg(loei),pl(loeien)],de,[]).

n([pl(loepen),sg(loep)],de,[]).

n([mass(loer)],de,[]).

n([mass(lof)],de,[]).

n([sg([lof,der,zotheid])],de,[]).

n([pl(loftuitingen),sg(loftuiting)],de,[sbar]).

n([pl(lofzangen),sg(lofzang)],de,[]).

n([sg(log),pl(logs)],both,[]).

n([sg(logaritme),pl(logaritmen),pl(logaritmes)],both,[]).

n([pl(loges),sg(loge)],de,[]).

n([pl(logementen),sg(logement)],het,[]).

n([sg(logger),pl(loggers)],de,[]).

n([mass(logica)],de,[]).

n([mass(logies)],het,[]).

n([mass(logika)],de,[]).

n([sg(login)],de,[]).

n([mass(logistiek)],de,[],
  [haven,
   materieel]).

n([pl('logo\'s'),sg(logo)],het,[]).

n([mass(logos)],de,[]).

n([pl(lokken),sg(lok)],de,[measure]).

n([pl(lokalen),sg(lokaal)],het,[np_app_measure],
  [klas,
   proef,
   dim(lokaaltje)]).

n([pl(lokazen),sg(lokaas)],het,[]).

n([pl(loketten),sg(loket)],het,[]).

n([sg(lokker),pl(lokkers)],de,[],
  [i(kinder,kind)]).

n([pl(lokomotieven),sg(lokomotief)],de,[]).

n([mass(lol)],de,[],[dim(lolletje)]).

n([sg(lollie),pl(lollies)],de,[]).

n([pl(lompen),sg(lomp)],de,[]).

n([pl(longen),sg(long)],de,[],[dim(longetje)]).

n([sg(longhouse),pl(longhouses)],het,[]).

n([pl(lonten),sg(lont)],both,[]).

n([pl(loden),sg(lood)],het,[measure]).

n([pl(loodgieters),sg(loodgieter)],de,[]).

n([stem(lood_DIM),pl(loodjes),sg(loodje)],het,[]).

n([pl(loodsen),sg(loods)],de,[]).

n([sg(loodswezen)],het,[]).

n([mass(loof)],het,[]).

n([mass(loog)],het,[]).

n([mass(look)],de,[]). 

n([mass([look,and,feel]),
   mass([look,en,feel]),
   mass([look,'&',feel]),
   mass([look,en,de,feel])],de,[]). 

% Thaise country muziek
n([mass([look,thung])],de,[]).

n([pl(lonen),sg(loon)],het,
  [vp,
   subject_sbar  % het was zijn verdiende loon dat ...
  ]).

n([pl(lonen),sg(loon)],het,[],
  [bruto,
   dag,
   minimum,
   i(minimum_jeugd,minimum_jeugd),
   netto,
   prestatie,
   spaar,
   uur]).

n([mass(loonmatiging)],de,[]).

n([pl(loonsommen),sg(loonsom)],de,[]).

n([pl(loontjes),sg(loontje)],het,[vp]).

n([pl(lopen),sg(loop)],de,[sbar,pred_pp(aan),pred_pp(op)]).

n([pl(lopen),sg(loop)],de,[],
  [beneden,
   boven,
   veld,
   water]).

n([sg(loop),pl(loops)],de,[]).

n([sg(loopbaan),pl(loopbanen)],de,
  [temp_mod,
   sbar],
  [s(speler),
   voetbal,
   wieler]).

n([pl(loopgraven),sg(loopgraaf)],de,[]).

n([pl(loopjes),sg(loopje)],het,[]).

n([mass(looppas)],de,[]).

n([pl(loopplanken),sg(loopplank)],de,[]).

n([sg(loopster),pl(loopsters)],de,[]).

n([pl(looptijden),sg(looptijd)],de,[]).

n([pl(loten),sg(loot)],de,[]).

n([pl(lopers),sg(loper)],de,[],
  [geld,
   marathon,
   dim(lopertje)]).

n([pl(lorren),sg(lor)],het,[]).

n([pl(lords),sg(lord)],de,[]).

n([sg([lord,chancellor])],de,[]).

n([sg([lord,mayor])],de,[]).

n([pl(lorretjes),sg(lorretje)],het,[]).

n([sg([los,hoes])],both,[]).  % Saksische boerderij

n([sg(loser),pl(losers)],de,[]).

n([sg(losser),pl(lossers)],de,[]).  % laders en

n([pl(loten),sg(lot)],het,[sbar,vp]).

n([pl(loten),sg(lot)],het,[],
  [kras,
   s(staat),
   dim(lootje),
   dim(lotje)]).

n([pl(loterijen),sg(loterij)],de,[],
  [kras,
   s(staat),
   dim(loterijtje)]).

n([pl(lotgevallen),sg(lotgeval)],het,[sbar,vp]).

n([pl(lotingen),sg(loting)],de,[]).

n([pl(lotions),sg(lotion)],de,[]).

n([pl(lounges),sg(lounge)],de,[]).

n([pl(loupen),sg(loupe)],de,[]).

n([pl(louteringen),sg(loutering)],de,[]).

n([mass(love)],de,[]).

n([sg([love,baby]),pl([love,babies])],de,[]).

n([sg([love,hut]),pl([love,huts])],de,[]).

n([sg([love,parade]),pl([love,parades])],de,[]).

n([sg([love,scene]),pl([love,scenes])],de,[]).

n([sg([love,story]),pl([love,stories])],de,[]).

n([mass(lover)],het,[]).

n([mass([low,budget]),mass('low-budget'),mass(lowbudget)],de,[]).

n([mass([low,key]),mass('low-key'),mass(lowkey)],de,[]).

n([mass([low,tech]),mass('low-tech'),mass(lowtech)],de,[]).

n([sg([loya,jirga])],de,[]).  % vergadering van Afghaanse stamhoofden

n([mass(loyaliteit)],de,[]).

n([sg(lozer),pl(lozers)],de,[]).

n([pl(lozingen),sg(lozing)],de,[]).

n([stem(lp),
   sg(lp),
   pl('lp\'s'),
   sg('LP'),
   pl('LP\'s')
  ],de,[],['12"']).  % "

n([mass(lts)],both,[]).

n([pl(luchten),sg(lucht)],de,[],
  [dim(luchtje),
   nacht]).

n([pl(luchtaanvallen),sg(luchtaanval)],de,[]).

n([mass(luchtalarm)],het,[]).

n([pl(luchtbellen),sg(luchtbel)],de,[]).

n([pl(luchtbruggen),sg(luchtbrug)],de,[]).

n([mass(luchtdruk)],de,[]).

n([pl('luchtfoto\'s'),sg(luchtfoto)],de,[]).

n([pl(luchthavens),sg(luchthaven)],de,[]).

n([sg(luchtledig)],het,[]).

n([pl(luchtmachten),sg(luchtmacht)],de,[]).

n([mass(luchtmobiel)],both,[]).

n([pl(luchtpijpen),sg(luchtpijp)],de,[]).

n([mass(luchtruim)],het,[]).

n([mass(luchtvaart)],de,[],[burger]).

n([mass(luchtverkeer)],het,[]).

n([mass(luchtverontreiniging)],de,[]).

n([mass(luchtvervuiling)],de,[]).

n([mass(luchtvochtigheid)],de,[]).

n([pl(lucifers),sg(lucifer)],de,[],[dim(lucifertje)]).

n([sg([lucky,loser]),pl([lucky,losers])],de,[]).

n([pl(luiaards),sg(luiaard)],de,[]).

n([sg(luider),pl(luiders)],de,[]).  % klokkenluiders

n([pl(luidsprekers),sg(luidspreker)],de,[]).

n([pl(luiers),sg(luier)],de,[],[dim(luiertje)]).

n([pl(luifels),sg(luifel)],de,[],[dim(luifeltje)]).

n([mass(luiheid)],de,[]).

n([pl(luiken),sg(luik)],het,[],
  [dim(luikje),
   baby,
   ski,
   veel]).

n([sg(luilekkerland)],het,[]).

n([mass(luim)],de,[pred_pp(in),
		   pred_pp(van)],[]).

n([pl(luipaarden),sg(luipaard)],both,[],
  [jacht  % met dank aan Vincent Vandeghinste, en niet jacht_lieden_paard
  ]).

n([pl(luizen),sg(luis)],de,[]).

n([mass(luister)],de,[]).

n([pl(luisteraars),sg(luisteraar)],de,[]).

n([mass(luit)],de,[]).

n([pl(luitenants),sg(luitenant)],de,[]).

n([pl('luitenant-generaals'),sg('luitenant-generaal')],de,[]).

n([pl('luitenant-kolonels'),sg('luitenant-kolonel')],de,[]).

n([pl(lullen),sg(lul)],de,[],[dim(lulletje)]).

n([sg([lulletje,rozewater]),pl([lulletjes,rozewater]),
   sg([lulletje,rozenwater]),pl([lulletjes,rozenwater])],het,[]).

n([pl(lummels),sg(lummel)],de,[]).

n([pl(lunchen),pl(lunches),sg(lunch)],de,[]).

n([sg(lunch)],het,[]).  % "het lunch" ouderwets?

n([mass(lurex)],de,[]).

n([pl(lussen),sg(lus)],de,[],[dim(lusje)]).

n([pl(lusten),sg(lust)],de,[vp]).

n([sg(lust)],de,[],[koop]).

n([pl(lustrums),sg(lustrum)],het,[]).

n([pl(luren),sg(luur)],de,[]).

n([sg(lutheraan),pl(lutheranen)],de,[]).

n([pl(luwten),sg(luwte)],de,[]).

n([meas(lux)],both,[meas_mod,measure]).

n([mass(luxe),pl(luxes)],de,[sbar,vp]).

n([pl(lycea),pl(lyceums),sg(lyceum)],het,[],
  ['Montessori']).

n([sg(lyric),pl(lyrics)],de,[]).

n([mass(lyriek)],de,[]).

n([mass(löss)],de,[]).

%% de letter m.
%% afkorting van meter via lex.pl abbreviation
n([pl('m\'s'),sg(m)],de,[],[dim('m\'etje')]).

n([sg('M.O.'),
   sg('M.O'),
   sg('MO')],de,[app_measure]).

n([sg('M.O.-A'),
   sg('M.O-A'),
   sg('MO-A'),
   sg('M.O.-a'),
   sg('M.O-a'),
   sg('MO-a')
  ],de,[app_measure]).

n([sg('M.O.-B'),
   sg('M.O-B'),
   sg('MO-B'),
   sg('M.O.-b'),
   sg('M.O-b'),
   sg('MO-b')
  ],de,[app_measure]).

n([pl('ma\'s'),sg(ma)],de,[]).

n([pl('maîtresses'),pl('maîtressen'),sg('maîtresse'),
   pl('maitresses'),pl('maitressen'),sg('maitresse'),
   pl('ma^itressen'),pl('ma^itresses'),sg('ma^itresse')],de,[]).

n([pl(magen),sg(maag)],de,[]).

n([pl(maagden),sg(maagd)],de,[]).

n([mass(maagdelijkheid)],de,[]).

n([mass(maagstreek)],de,[]).

n([mass(maagzuur)],het,[]).

n([pl(maagzweren),sg(maagzweer)],de,[]).

n([mass(maak)],de,[]).

n([meas(maal),pl(malen)],de,
  [temp_mod,
   subject_sbar,
   sbar,
   np_measure
  ]).

n([pl(malen),sg(maal)],het,[measure],[dim(maaltje)]).

n([pl(maalstromen),sg(maalstroom)],de,[]).

n([pl(maaltijden),sg(maaltijd)],de,[],[diepvries]).

n([pl(manen),sg(maan)],de,[],[dim(maantje)]).

n([bare_meas(maand),pl(maanden)],de,
  [temp_mod,sbar,measure,app_measure],
  [dim(maandje)]).

n([sg(maand),pl(maanden),
   ignore_stem(maand),
   ignore_stem(maand_DIM)],de,
  [temp_mod,sbar,app_measure],
  [feest,
   vakantie,
   vasten,
   dim(maandje)]).

n([sg(maandag),pl(maandagen)],de,
  [temp_mod,sbar],
  [paas,
   pinkster]).

n([pl(maarschalken),sg(maarschalk)],de,[],[veld]).

n([sg(maart)],de,[temp_mod,sbar]).

n([pl(mazen),sg(maas)],de,[]).

n([pl(maats),pl(maten),sg(maat)],de,
  [measure,                     % een grote maat herenschoen
   pred_pp(op),     % op maat
   pred_pp(beneden) % beneden de maat
  ],
  [dim(maatje)]).

n([pl(maats),pl(maten),sg(maat)],de,[],
  [ploeg,
   straf,
   dim(maatje)]).

n([pl(maatregelen),pl(maatregels),sg(maatregel)],de,[sbar,vp],
  [antidumping,
   s(beleid),
   s(bezuiniging),
   steun,
   straf,
   tegen,
   s(veiligheid),
   s(verkeer),
   s(voorzorg)
  ]).

n([pl(maatregelen),pl(maatregels),sg(maatregel)],de,[],
  [loon]).

n([pl(maatschappen),sg(maatschap)],de,[measure,sbar,vp]).

n([pl(maatschappijen),sg(maatschappij)],de,[],
  [s(belegging),
   burger,
   charter,
   consumptie,i(consumptie,konsumptie),
   dochter,
   film,
   houdster,
   informatie,
   s(investering),
   kabel,
   luchtvaart,
   moeder,
   olie,
   participatie,
   i(plaat,platen),
   productie,
   spoorweg,
   s(staat),
   telefoon,
   s(vervoer),vervoer,
   verzekering,s(verzekering),
   s(verzorging),
   werk]).

n([pl('maatschappij-visies'),sg('maatschappij-visie')],de,[]).

n([mass(maatschappijkritiek)],de,[]).

n([pl(maatschappijvisies),sg(maatschappijvisie)],de,[]).

n([pl(maatschappijvormen),sg(maatschappijvorm)],de,[sbar]).

n([pl(maatstaven),sg(maatstaf)],de,[]).

n([sg(mac)],de,[]).

n([mass(macaroni)],de,[]).

n([sg(machinatie),pl(machinaties)],de,[]).

n([pl(machines),sg(machine)],de,[],
  [chip,
   s(codering),
   keuken,
   type,
   vlieg,
   zoek,
   dim(machientje),
   dim(machinetje)]).

n([pl(machinepistolen),sg(machinepistool)],het,[]).

n([pl(machinerieën),sg(machinerie)],de,[]).

n([pl(machinisten),sg(machinist)],de,[]).

n([sg(macho),pl('macho\'s')],de,[]).

n([pl(machten),sg(macht)],de,
  [measure,
   vp,
   pred_pp(in),
   pred_pp(in,subject_vp),
   pred_pp(aan)]).

n([pl(machten),sg(macht)],de,[],
  [s(bezetting),
   groot,
   hoofd,
   interventie,
   leger,
   politie,
   reactie,
   s(staat),
   stabilisatie,
   super,
   i(troep,troepen),
   s(vrede),
   h('VN'),
   i('VN_vrede','VN-vredes'),
   zee  % niet niet zeem-achten
  ]).

n([mass(machteloosheid)],de,[]).

n([pl(machthebbers),sg(machthebber),
   pl(machtshebbers),sg(machtshebber)
   ],de,[]).

n([pl(machtigingen),sg(machtiging)],de,[]).

n([pl(madammen),pl(madams),sg(madam),
   pl(madames),pl(mesdames),sg(madame)],de,[],[dim(madammetje),
					       dim(madammeke),
					       dim(madammeken)]).

n([sg(mademoiselle),pl(mademoiselles)],de,[],[]).

n([pl(maden),sg(made)],de,[]).

n([pl('madeira\'s'),sg(madeira)],de,[],[dim(madeiraatje)]).

n([pl(madelieven),sg(madelief)],de,[],[dim(madeliefje)]).

n([pl('madonna\'s'),sg(madonna)],de,[],[dim(madonnaatje)]).

n([sg(maestro),pl('maestro\'s')],de,[]).

n([sg(maffia),sg(mafia),pl('maffia\'s'),pl('mafia\'s')],de,[]).

n([sg(mafioso),pl(mafiosi),sg(maffioso),pl(maffiosi)],de,[]).

n([sg(maffer),pl(maffers)],de,[]).

n([sg(mafkees),pl(mafkezen)],de,[]).

n([pl(magazijnen),sg(magazijn)],het,[],[dim(magazijntje)]).

n([pl(magazines),sg(magazine)],het,[],[actua]).

n([mass(magie)],de,[subject_sbar]).

n([pl(magistraten),sg(magistraat)],de,[]).

n([sg(magistratuur),pl(magistraturen)],de,[]).

n([pl(magiërs),sg(magiër)],de,[]).

n([mass(magma)],het,[]).

n([sg(magnaat),pl(magnaten)],de,[],
  [media]).

n([pl(magneten),sg(magneet)],de,[]).

n([pl(magneetbanden),sg(magneetband)],de,[]).

n([mass(magneetveld)],het,[]).

n([mass(magnetisme)],het,[]).

n([sg(magnetron),pl(magnetrons)],de,[]).

n([sg([magnum,opus])],het,[]).

n([sg(mail),pl(mails)],de,[sbar],
  [f([hate]),
   dim(mailtje)]).

n([sg(mailbox)],de,[]).

n([sg(maillot),pl(maillots)],de,[]).

n([mass(mais)],de,[]).

n([sg(mainport),pl(mainports)],de,[]).

n([mass(mainstream)],de,[]).

n([mass(maison)],de,[]).

n([sg([maitre,'d\'hotel'])],de,[]).

n([mass(maizena)],de,[]).

n([pl(majesteiten),sg(majesteit)],de,[]).

n([sg(['Hare','Majesteit']),
   sg([hare,majesteit])],de,[np_app_measure]).

n([sg(['Zijne','Majesteit']),
   sg([zijne,majesteit])],de,[np_app_measure]).

n([mass(majeur)],both,[]).

n([pl(majoors),sg(majoor)],de,[]).

n([pl(majors),sg(major)],de,[]).

n([pl('Majors'),sg('Major')],de,[]).

n([sg(majorette),pl(majorettes)],de,[]).

n([pl('make-ups'),sg('make-up'),sg([make,up]),pl([make,ups])],de,[]).

n([pl(makelaars),pl(makelaren),sg(makelaar)],de,[],
  [beurs,
   s(speler)]).

n([pl(makelaardijen),sg(makelaardij)],de,[],
  [beurs,
   s(speler)]).

n([mass(makelij)],de,[pred_pp(van)]).

n([pl(makers),sg(maker),
   pl(maaksters),sg(maakster)],de,[],
  [af,
   auto,
   s(beleid),
   chip,
   documentaire,
   i(doelpunt,doelpunten),
   i(fiets,fietsen),i(fiets,fietse),
   meubel,
   opinie,
   i(plan,plannen),
   reclame,
   software,
   spel,
   televisie,tv,h(tv),f([tv]),i(tv,'TV-'),
   theater,
   week]).

n([sg([making,of])],de,[]).

n([sg(makke)],de,[subject_sbar]).

n([pl(makkers),sg(makker)],de,[],
  [ploeg,
   strijd,
   dim(makkertje)]).

n([sg(makkie),pl(makkies)],het,[]).

n([pl(makrelen),sg(makreel)],de,[],[dim(makreeltje)]).

n([sg(mal),pl(mallen)],de,[]).

n([mass(malaise)],de,[],
  [beurs]).

n([mass(malaria)],de,[]).

n([mass(maling)],de,[]).

n([sg(mall),pl(malls)],de,[],
  [f([shopping])]).

n([sg([malt,whiskey]),
   sg([malt,whisky]),
   pl([malt,'whiskey\'s']),
   pl([malt,'whisky\'s'])],de,[]).

n([sg(malus)],de,[]).

n([sg(malversatie),pl(malversaties)],de,[]).

n([mass(malware)],de,[]).

n([sg(mam),sg(mams)],de,[]).

n([pl('mama\'s'),sg(mama)],de,[],
  [dim(mamaatje)]).

n([pl('mamma\'s'),sg(mamma)],de,[],
  [dim(mammaatje)]).

n([sg(mammoet),pl(mammoeten)],de,[]).

n([bare_meas(man),pl(mannen)],de,[measure]).

n([sg(man),pl(mannen),ignore(m(man,noun(de,count,sg),man))],de,[],
  [buur,
   front,
   overbuur,
   hoek,
   kerst,i(kerst,'Kerst'),
   kies,
   i(klus_DIM,klusjes),
   kop,
   s(vakbond),
   s(verzet),
   vuilnis,
   dim(mannetje),
   dim(manneke),
   dim(manneken)]).

n([mass(management)],het,[app_measure]).

n([mass(management)],het,[],
  [mis,
   top,
   water  %W-A
  ]). 

n([pl([management,'buy-outs']),sg([management,'buy-out'])],de,[]).

n([sg([management,consultant]),pl([management,consultants])],de,[]).

n([mass('man-vrouw'),
   mass([man,vrouw]),
   mass('man/vrouw')],de,[]).	% de relatie/verhouding/... man-vrouw

n([pl(managers),sg(manager)],de,
  [app_measure],		% manager voetbalzaken
  [h(interim),
   project,
   team,
   top]).

n([pl(managers),sg(manager)],de,[],
  [account,f([account]),
   s(fond),
   s(kwaliteit),
   locatie]).

n([sg([managing,director]),pl([managing,directors])],de,[]).

n([pl(manches),sg(manche)],de,[app_measure]).

n([pl(manchetten),sg(manchet)],de,[]).

n([sg(manco),pl('manco\'s')],het,[subject_sbar,subject_vp]).

n([pl(manden),sg(mand)],de,[measure],
  [was,
   dim(mandje)]).

n([pl(mandaten),sg(mandaat)],het,[sbar,vp]).

n([pl(mandarijnen),sg(mandarijn)],de,[],[dim(mandarijntje)]).

n([sg(mandataris),pl(mandatarissen)],de,[]).

n([pl(maneges),sg(manege)],de,[np_app_measure]).

n([mass(maneschijn)],de,[]).

n([pl(maneuvers),sg(maneuver)],de,[]).

n([mass(mangaan)],het,[]).

n([sg(mangel),pl(mangels)],de,[]).

n([pl('mango\'s'),sg(mango)],de,[]).

n([pl(maniakken),sg(maniak)],de,[]).

n([pl(manicheeërs),sg(manicheeër)],de,[]).

n([pl(manieën),sg(manie)],de,[vp]).

n([pl(manieren),sg(manier)],de,[vp,
				sbar,
				van_sbar],
  [tafel,
   dim(maniertje)]).

n([pl(manifesten),sg(manifest)],het,[sbar]).

n([sg(manifestant),pl(manifestanten)],de,[]).

n([pl(manifestaties),sg(manifestatie)],de,[]).

n([pl(manipulaties),sg(manipulatie)],de,[]).

n([pl(mankementen),sg(mankement)],het,[sbar]).

n([pl(mankepoten),sg(mankepoot)],de,[]).

n([mass(mannelijkheid)],de,[]).

n([pl(mannenstemmen),sg(mannenstem)],de,[]).

n([pl(mannequins),sg(mannequin)],de,[]).

n([pl(manoeuvres),sg(manoeuvre)],both,
  []).				% Vlaams: het

n([pl(manoeuvres),sg(manoeuvre)],both,
  [],
  [s(afleiding),
   inhaal]).			% Vlaams: het

n([stem(manschap), pl(manschappen)],de,[]).

n([pl(mantels),sg(mantel)],de,[],[dim(manteltje)]).

n([pl(mantelpakken),sg(mantelpak)],het,[]).

n([mass(mantelzorg)],de,[]).

n([sg(mantelzorger),pl(mantelzorgers)],de,[]).

n([sg(mantra),pl('mantra\'s')],both,[]).

n([pl(manuscripten),sg(manuscript)],het,[]).

n([pl(manuskripten),sg(manuskript)],het,[]).

n([pl(mappen),sg(map)],de,[],[dim(mapje)]).

n([pl(maquettes),sg(maquette)],de,[]).

n([pl(marathons),sg(marathon)],de,[]).

n([pl(marconisten),sg(marconist)],de,[]).

n([pl(maren),sg(mare)],de,[sbar]).

n([mass(marechaussee),pl(marechaussees),sg(marechaussee)],de,[]).

n([pl(margarines),sg(margarine)],de,[]).

n([pl(marges),sg(marge)],de,[],[verlies,
				winst]).

n([pl(margrieten),sg(margriet)],de,[]).

n([mass(marihuana)],de,[]).

n([sg(marinade),pl(marinades)],de,[]).

n([pl(marines),sg(marine),sg('Marine')],de,[]).

n([pl(mariniers),sg(marinier)],de,[]).

n([pl(marionetten),sg(marionet)],de,[]).

n([mass(marjolein)],de,[]).

n([meas(mark),pl(marken),
   meas('D-mark'),
   meas('Dmark'),
   meas('DMark'),
   meas('DM')],de,[meas_mod,measure]).

n([sg(marker),pl(markers)],de,[]).

n([pl(markeringen),sg(markering)],de,[]).

n([sg(marketentster),pl(marketentsters)],de,[]).

n([mass(marketing)],de,[]).

n([sg([marketing,manager]),pl([marketing,managers])],de,[]).

n([sg([market,maker]),pl([market,makers])],de,[]).

n([pl(markiezen),sg(markies)],de,[]).

n([pl(markten),sg(markt)],de,[],
  [i(aandeel,aandelen),
   advertentie,
   afzet,
   s(arbeid),
   auto,
   i(baan,banen),
   benzine,
   bier,
   i(boek,boeken),
   bouw,
   i(consument,consumenten),
   energie,
   flex,
   gas,
   geld,
   groei,
   i(huis,huizen),
   jaar,
   i(kantoor,kantoren),
   kapitaal,
   kerst,
   obligatie,
   olie,
   s(overheid),
   parallel,
   rommel,
   staal,
   stroom,
   taxi,
   telecom,
   termijn,
   thuis,
   transfer,
   valuta,
   vee,
   s(verzekering),
   vrij,
   wereld,
   wissel,
   woning]).

n([mass(marktaandeel),pl(marktaandelen),sg(marktaandeel)],het,[]).


n([pl(marktpleinen),sg(marktplein)],het,[]).

n([sg(marktwaarde)],de,[]).

n([mass(marktwerking)],de,[]).

n([pl(marmeladen),pl(marmelades),sg(marmelade)],de,[]).

n([pl(marmers),sg(marmer)],het,[]).

n([pl(marsen),sg(mars)],de,[]).

n([pl(martelaars),pl(martelaren),sg(martelaar)],de,[]).

n([mass(martelaarschap)],het,[]).

n([pl(martelingen),sg(marteling)],de,[sbar,vp]).

n([pl(marters),sg(marter)],de,[],[dim(martertje)]).

n([mass(marxisme)],het,[]).

n([pl(marxisten),sg(marxist)],de,[]).

n([pl(mascottes),sg(mascotte)],de,[]).

n([pl(maskers),sg(masker)],het,[],[dim(maskertje)]).

n([pl('massa\'s'),sg(massa)],de,[measure],[]).

n([pl('massa\'s'),sg(massa)],de,[],
  [bio,
   lucht,
   spier]).

n([pl(massages),sg(massage)],de,[]).

n([pl(massagraven),sg(massagraf)],het,[]).

n([pl(massamedia),sg(massamedium)],het,[]).

n([sg(massief)],het,[]).

n([pl(masten),sg(mast)],de,[],
  [radio,
   televisie,
   zend]).

n([sg(master),pl(masters)],de,[]).

n([mass(mastiek)],de,[]).

n([pl(masturbaties),sg(masturbatie)],de,[]).

n([pl(matten),sg(mat)],de,[],
  [deur,
   dim(matje)]).

n([mass(mat)],het,[],[schaak]).

n([pl(matchen),pl(matches),sg(match)],de,[temp_mod],
  [heen,
   i(kandidaat,kandidaten),
   oefen,
   sleutel,
   terug,
   thuis]).

n([sg([match,maker]),pl([match,makers])],de,[]).

n([sg(matchpoint),pl(matchpoints),
   sg([match,point]),pl([match,points])],both,[]).

n([mass(mate)],de,[sbar]).

n([pl(materialen),sg(materiaal)],het,[],
  [archief,
   beeld,
   bewijs,
   bouw,
   cel,
   cijfer,
   i(feit,feiten),
   film,
   les,
   reclame,
   risico,
   teelt,
   s(vergelijking)]).

n([mass(materialisme)],het,[]).

n([pl(materies),pl(materiën),sg(materie)],de,[]).

n([mass(materieel)],het,[],[vervoer]).

n([mass(matglas)],het,[]).

n([mass(matiging)],de,[]).

n([pl(matrassen),sg(matras)],both,[]).

n([pl(matrices),sg(matrix)],de,[]).

n([pl(matrijzen),sg(matrijs)],de,[]).

n([pl(matrozen),sg(matroos)],de,[]).

n([sg(mattie),pl(matties)],both,[]).

n([pl(mausolea),pl(mausoleums),sg(mausoleum)],het,[]).

%% pl: ze zit in 2 mavo
n([mass(mavo),pl(mavo)],both,[]).

n([mass('mavo/havo'),pl('mavo/havo')],both,[]).

n([mass('mavo/havo/vwo'),pl('mavo/havo/vwo')],both,[]).

n([mass(maxi)],both,[]).

n([pl(maxima),sg(maximum)],het,[measure]).

n([meas(maxwell),pl(maxwells)],de,[meas_mod,measure]).

n([pl(mayonaises),sg(mayonaise)],de,[]).

n([sg(mazelen),pl(mazelen)],de,[]).

n([pl(mazzels),sg(mazzel)],de,[sbar,subject_sbar,vp],[dim(mazzeltje)]).

n([mass(maïs)],both,[]).

n([mass(maïzena)],de,[]).

n([mass(mbo)],het,[app_measure]).

n([sg('ME\'er'),
   pl('ME\'ers')],de,[]).

n([sg(meander),pl(meanders)],de,[]).

n([mass(meao)],both,[]).

n([pl(mecaniciens),sg(mecanicien)],de,[]).

n([mass(mechanica)],de,[],[kwantum]).

n([mass(mechaniek)],both,[],
  [lucht,
   dim(mechaniekje)]).		% celex: de

n([mass(mechanika)],de,[]).

n([sg(mechanisatie),pl(mechanisaties)],de,[]).

n([pl(mechanismen),pl(mechanismes),sg(mechanisme)],het,
  [sbar],
  [afweer,
   i(anti_hanteerbaarheid,antihanteerbaarheids),
   evaluatie,
   markt,
   prijs]).

n([pl(medailles),sg(medaille)],de,[]).

n([pl(medaillons),sg(medaillon)],het,[],[dim(medaillonnetje)]).

n([mass(mede)],de,[]).

n([pl(medeburgers),sg(medeburger)],de,[]).

n([pl(mededelingen),sg(mededeling)],de,[sbar,vp]).

n([pl(mededingers),sg(mededinger)],de,[]).

n([mass(mededinging)],de,[],[s(beleid)]).

n([mass(mededogen)],het,[]).

n([sg(medeklinker),pl(medeklinkers)],de,[]).

n([mass(medeleven)],het,[]).

n([mass(medelijden)],het,[]).

n([pl(medemensen),sg(medemens)],de,[]).

n([pl(medeplichtigen),sg(medeplichtige)],de,[]).

n([mass(medeplichtigheid)],de,[]).

n([pl(medestanders),sg(medestander)],de,[]).

n([pl(medestandsters),sg(medestandster)],de,[]).

n([pl(medestudenten),sg(medestudent)],de,[]).

n([pl(medewerkers),sg(medewerker)],de,[app_measure],
  [s(beleid),
   i(land,landen),
   spoorweg]).

n([mass(medewerking)],de,[]).

n([pl(medewerksters),sg(medewerkster)],de,[]).

n([mass(medeweten)],het,[]).

n([mass(medezeggenschap)],de,[]).

n([mass(mediation)],de,[]).

n([sg(mediator),pl(mediators)],de,[]).

n([pl(medicamenten),sg(medicament)],het,[]).

n([mass(medicatie)],de,[]).

n([pl(medicijnen),sg(medicijn)],both,[app_measure]).

n([pl(medicijnmannen),sg(medicijnman)],de,[]).

n([pl(medici),sg(medicus)],de,[],[para]).

n([pl(medikamenten),sg(medikament)],het,[app_measure]).

n([pl(meditaties),pl(meditatiën),sg(meditatie)],de,[]).

n([pl(media),pl(mediums),sg(medium)],het,[measure]).

n([sg([medium,care])],de,[]).

n([mass(meel)],het,[],
  [i(been,beender),
   dier,
   i(kind,kinder),
   paneer,
   vis]).

n([pl(meelopers),sg(meeloper)],de,[]).

n([sg(meent),pl(meenten)],de,[]).

n([pl(meren),sg(meer)],het,[],
  [berg,
   binnen,
   zee,
   i(zoet_water,zoetwater),
   zout,
   dim(meertje)]).

n([pl(meerderheden),sg(meerderheid)],de,[],
  [kamer,
   'Kamer']).

n([pl(meerderheden),sg(meerderheid)],de,[pred_pp(in)],
  []).

n([pl(meerderheidsbelangen),sg(meerderheidsbelang)],het,[]).

n([pl(meerjarenramingen),sg(meerjarenraming)],de,[]).

n([sg(meerkamp),pl(meerkamps)],de,[]).

n([sg(meerkamper),pl(meerkampers)],de,[]).

n([sg(meerkampster),pl(meerkampsters)],de,[]).

n([pl(meerminnen),sg(meermin)],de,[]).

n([sg(meerval),pl(meervallen)],de,[]).

n([pl(meervouden),sg(meervoud)],het,[]).

n([mass(meerwaarde)],de,[]).

n([pl(mezen),sg(mees)],de,[]).

n([pl(meesters),sg(meester)],de,[],
  [boven,
   concert,
   hoofd,
   ijs,
   kapel,
   school,
   dim(meestertje)]).

n([pl(meesteressen),sg(meesteres)],de,[]).

n([mass(meesterschap)],het,[]).

n([pl(meten),sg(meet)],de,[]).

n([pl(meetings),sg(meeting)],de,[]).

n([mass(meetkunde)],de,[]).

n([pl(meeuwen),sg(meeuw)],de,[],
  [drieteen,
   kok,
   mantel,
   storm,
   zilver,
   dim(meeuwtje)]).

%% vlaams
n([mass(meeval)],de,[sbar]).

n([pl(meevallers),sg(meevaller)],de,
  [sbar],
  [dim(meevallertje)]).

n([pl(meevallers),sg(meevaller)],de,[],
  [belasting,
   dim(meevallertje)]).

n([sg(mei)],de,[temp_mod,sbar]).

n([pl(meiden),sg(meid)],de,[],[dim(meidje)]).

n([mass(meineed)],both,[]).

n([pl(meisjes),sg(meisje),
   pl(meiskes),sg(meiske)],het,[],
  [bier,  % ?
   buur,
   school,
   tiener]).

n([pl(mejuffrouwen),sg(mejuffrouw)],de,[]).

n([pl(melaatsen),sg(melaatse)],de,[]).

n([mass(melancholie)],de,[]).

n([sg(melanoom),pl(melanomen)],het,[]).

n([pl(meldingen),sg(melding)],de,
  [sbar,
   vp,
   np_app_measure,
   start_app_measure,
   app_measure],
  [fout]).

n([pl(meldingen),sg(melding)],de,
  [],
  [bom,
   file]).

n([pl(meldpunten),sg(meldpunt)],het,[app_measure]).

n([mass(melk)],de,[],
  [chocolade,
   koe,
   koffie,
   moeder,
   i(schaap,schapen),i(schaap,schape),
   school
  ]).

n([sg('Melkertbaan'),pl('Melkertbanen')],de,[]).

n([pl(melktanden),sg(melktand)],de,[]).

n([mass(melkvee)],het,[]).

n([pl(melodieën),sg(melodie)],de,[],[dim(melodietje)]).

n([sg(melodrama),pl('melodrama\'s')],het,[]).

n([pl(meloenen),sg(meloen)],de,[],[dim(meloentje)]).

n([sg(memo),pl('memo\'s')],both,[]).

n([pl(memoires)],de,[]).

n([pl(memoranda),pl(memorandums),sg(memorandum)],het,[]).

n([pl(memories),sg(memorie)],de,[]).

n([pl(meneren),sg(meneer)],de,
  [np_app_measure,		% meneer de pastoor
   app_measure			% meneer pastoor
  ],
  [dim(meneertje)]).

n([pl(mengelingen),sg(mengeling)],de,[]).

n([mass(mengelmoes)],both,[]).

n([pl(mengingen),sg(menging)],de,[]).

n([pl(mengsels),sg(mengsel)],het,[measure],[dim(mengseltje)]).

n([pl(mengvormen),sg(mengvorm)],de,[]).

n([pl(menigten),pl(menigtes),sg(menigte)],de,[measure]).

n([pl(meningen),sg(mening)],de,[sbar,vp],[dim(meninkje)]).

n([mass(meningsuiting)],de,[]).

n([pl(meningsverschillen),sg(meningsverschil)],het,[sbar,vp]).

n([mass(meningsvorming)],de,[]).

n([sg(meniscus)],de,[]).

n([pl(mensen),sg(mens)],both,[],             % added 'het stomme mens'
  [i(dag_DIM,dagjes),
   politie,
   sport,
   s(stad),
   top,
   s(wetenschap),
   i(zaak,zaken),
   dim(mensje)]).

n([mass('mens-zijn')],het,[]).

n([pl(mensapen),sg(mensaap)],de,[]).

n([mass(mensdom)],het,[]).

n([mass(menselijkheid)],de,[]).

n([pl(mensenhanden),sg(mensenhand)],de,[]).

n([mass(mensenheugenis),mass(mensenheugnis)],de,[]).

n([pl(mensenkinderen),sg(mensenkind)],het,[]).

n([pl(mensenlevens),sg(mensenleven)],het,[]).

n([pl('mensenmassa\'s'),sg(mensenmassa)],de,[]).

n([pl(mensenzielen),sg(mensenziel)],de,[]).

n([mass(mensheid)],de,[]).

n([pl(mensjewieken),sg(mensjewiek)],de,[]).

n([pl(menstruaties),sg(menstruatie)],de,[]).

n([sg([mental,coach]),pl([mental,coaches])],de,[]).

n([pl(mentaliteiten),sg(mentaliteit)],de,[vp],[]).

n([pl(mentaliteiten),sg(mentaliteit)],de,[],
  [s(killer)]).

n([pl(mentoren),pl(mentors),sg(mentor)],de,[]).

n([pl('menu\'s'),sg(menu)],both,[app_measure],[dim(menuutje)]).

n([pl(meppen),sg(mep)],de,[]).

n([sg([mer,à,boire])],both,[]).

n([mass(merchandising)],de,[]).

%% een dikke merci aan iedereen die heeft meegeholpen
n([sg(merci)],de,[]).

n([pl(merels),sg(merel)],de,[],[dim(mereltje)]).

%% mod: hij woont het merendeel van het jaar in Turkije
n([mass(merendeel)],het,[mod]).

n([mass(merg)],het,[],[i(rug,ruggen)]).

n([mass(mergel)],both,[]).

n([pl(meridianen),sg(meridiaan)],de,[]).

n([pl(merites),sg(merite)],de,[sbar]).

n([pl(merken),sg(merk)],het,[measure],
  [huis,
   h('A')]).

n([pl(merknamen),sg(merknaam)],de,[]).

n([pl(merktekenen),pl(merktekens),sg(merkteken)],het,[sbar]).

n([pl(merries),sg(merrie)],de,[],[dim(merrietje)]).

n([pl(messen),sg(mes)],het,[],
  [brood,
   hak,
   scheer,
   vlees,
   zak,
   dim(mesje)
  ]).

n([sg(mespunt),pl(mespunt)],both,[measure],[dim(mespuntje)]).

n([pl(messes),sg(mess)],de,[]).

n([sg(messias),pl(messiassen)],de,[]).

n([mass(mest)],de,[],
  [i(koe,koeien)]).

n([pl(mesthopen),sg(mesthoop)],de,[]).

n([pl(mestiezen),sg(mesties)],de,[]).

n([pl(mestvaalten),sg(mestvaalt)],de,[]).

%% onderhandelingen in de metaal (sector)
n([mass(metaal)],de,[]).

n([mass(metal)],both,[],
  [f([black]),
   f([death]),
   f([doom]),
   f([folk]),
   f([gothic]),
   f([heavy]),
   f([trash]),
   f([viking])]).

n([pl(metalen),sg(metaal)],het,[app_measure],[edel]). % het metaal antimoon

n([pl(metaforen),sg(metafoor)],de,[sbar]).

n([mass(metafysica)],de,[]).

n([mass(metafysika)],de,[]).

n([pl(metamorfosen),pl(metamorfoses),sg(metamorfose)],de,[sbar]).

n([pl(metamorfozen),pl(metamorfozes),sg(metamorfoze)],de,[sbar]).

n([pl(meteorieten),sg(meteoriet)],de,[]).

n([sg(meteoroloog),pl(meteorologen)],de,[]).

%% ook: peter en meter, dus peettante
n([sg(meter),pl(meters)],de,[],
  [stroom,
   dim(metertje)]).

n([meas(meter),pl(meters)],de,[meas_mod,measure],
  [centi,
   deci,
   hecto,
   kilo,
   micro,
   milli,
   dim(metertje),
   nano]).

n([pl(metgezellen),sg(metgezel)],de,[]).

n([mass(methaan)],both,[]).

n([mass(methadon)],both,[]).

n([mass(methanol)],both,[]).

n([pl(methoden),pl(methodes),sg(methode)],de,[vp]).

n([pl(methoden),pl(methodes),sg(methode)],de,[],
  [behandel,
   s(behandeling),
   s(opsporing),
   productie,i(productie,produktie),
   werk]).

n([mass(methodiek)],de,[]).

n([mass(methodologie)],de,[]).

n([pl(metingen),sg(meting)],de,[sbar]).

n([mass(metodiek)],de,[]).

n([mass(metodologie)],de,[]).

n([pl('metro\'s'),sg(metro)],de,[]).

n([pl(metropolen),sg(metropool)],de,[]).

n([sg(metropoliet),pl(metropolieten)],de,[]).

n([pl(metra),pl(metrums),sg(metrum)],het,[]).

n([pl(metselaars),sg(metselaar)],de,[],
  [dim(metselaartje)]).

n([pl(meubelen),pl(meubels),sg(meubel)],het,[],[dim(meubeltje)]).

n([pl(meubelstukken),sg(meubelstuk)],het,[]).

n([mass(meubilair)],het,[]).

n([pl(meuten),pl(meutes),sg(meute)],de,[measure]).

n([pl(mevrouwen),sg(mevrouw)],de,[np_app_measure],[dim(mevrouwtje)]).

n([sg(mi),pl('mi\'s')],de,[]).

n([pl('micro-organismen'),sg('micro-organisme')],het,[]).

n([sg(micro),pl('micro\'s')],de,[]). %% vlaams microfoon

n([pl(microfonen),pl(microfoons),sg(microfoon)],de,[],[dim(microfoontje)]).

n([meas(micron)],de,[meas_mod,measure]).

n([pl(microscopen),sg(microscoop)],both,[]).

n([sg(middag),pl(middagen)],de,[temp_mod,sbar,measure],
  [dim(middagje)
  ]).

n([sg(middag),pl(middagen)],de,[temp_mod,sbar],
  [zondag,
   maandag,
   dinsdag,
   woensdag,
   donderdag,
   vrijdag,
   zaterdag,
   dim(middagje)
  ]).

n([pl(middagmalen),sg(middagmaal)],het,[]).

n([pl(middaguren),sg(middaguur)],het,[]).

n([mass(middagzon)],de,[]).

n([pl(middelen),sg(middel)],het,
  [app_measure],
  [afslank,
   s(bedrijf),
   s(begroting),
   betaal,
   s(bestrijding),
   bind,
   communicatie,
   dwang,
   h('EU'),
   garantie,
   genees,
   genot,
   s(gewasbescherming),
   hulp,
   s(kalmering),
   laxeer,
   leer,
   s(leven),
   s(macht),
   oplos,
   productie,i(productie,produktie),
   s(recht),
   s(reiniging),   % en niet reiniging_smid_delen
   slaap,
   spaar,
   s(toevoeging),
   s(verdrag),
   vervoer,s(vervoer),
   s(voeding),
   voorbehoed,
   s(voorbehoed),
   was,
   wonder,
   zoet,
   dim(middeltje)]).

n([sg(middelbaar),pl(middelbaren)],het,[]).

n([pl(middeleeuwen)],de,[]).

n([pl(middeleeuwers),sg(middeleeuwer)],de,[]).

n([mass(middelmaat)],de,[]).

n([pl(middelmatigheden),sg(middelmatigheid)],de,[sbar,vp]).

n([pl(middelpunten),sg(middelpunt)],het,[]).

n([pl(middelvingers),sg(middelvinger)],de,[]).

n([pl(middens),sg(midden)],het,[]).

n([mass(['midden-',en,kleinbedrijf]),mass(['midden-en',kleinbedrijf])],het,[]).

n([sg(middenmoot),pl(middenmoten)],de,[]).

n([sg(middenmoter),pl(middenmoters)],de,[]).

n([mass(middenpad)],het,[]).

n([pl(middenriffen),sg(middenrif)],het,[]).

n([mass(middenstand)],de,[]).

n([pl(middenstanders),sg(middenstander)],de,[]).

n([sg(middenveld)],het,[]).

n([pl(middenvelders),sg(middenvelder)],de,[]).

n([mass(middernacht)],de,[]).

n([mass(midi)],het,[]).

n([sg([midlife,crisis]),
   sg(['mid-life',crisis])],de,[]).

n([sg([midterm,review]),
   pl([midterm,reviews])],de,[]).

n([pl(midzomers),sg(midzomer)],de,[]).

n([pl(mieren),sg(mier)],de,[],
  [dim(miertje),
   schub]).

n([pl(mietjes),sg(mietje)],het,[]).

n([pl(migraines),sg(migraine)],de,[]).

n([pl(migranten),sg(migrant)],de,[],[s(arbeid)]).

n([pl(migrantes),sg(migrante)],de,[]).

n([pl(migraties),sg(migratie)],de,[],
  [s(arbeid),
   s(huwelijk)]).

n([meas(mijl),pl(mijlen)],de,
  [meas_mod,
   measure],
  [dim(mijltje),
   zee]).

n([pl(mijlpalen),sg(mijlpaal)],de,[sbar,vp]).

n([pl(mijnen),sg(mijn)],de,[],
  [goud,
   kolen,
   steenkolen,
   steenkool
  ]).

n([sg(mijne),pl(mijne)],both,[]).

n([mass(mijnbouw)],de,[]).

n([pl(mijnenvelden),sg(mijnenveld)],het,[]).

n([pl(mijnheren),sg(mijnheer)],de,[np_app_measure],[dim(mijnheertje)]).

n([pl(mijnwerkers),sg(mijnwerker)],de,[]).

n([sg(mijt),pl(mijten)],de,[]).

n([sg(mik),pl(mikken)],de,[]).

n([pl(mikpunten),sg(mikpunt)],het,[]).

n([pl(miles)],de,[]).

n([sg([miles,calculator])],de,[]).

n([sg([miles,finder])],de,[]).

n([pl(milieus),sg(milieu)],het,[]).

n([sg([milieu,actie,plan]),pl([milieu,actie,plannen])],het,[]).

n([mass(milieuhygiëne)],de,[]).

n([pl(militairen),sg(militair)],de,[]).

n([sg(militant),pl(militanten)],de,[]).

n([mass(militarisme)],het,[]).

n([sg(military)],de,[]).

n([pl(milities),sg(militie)],de,[],[h('Hutu')]).

n([bare_meas(miljard),pl(miljarden)],both,[meas_mod,measure]).

n([pl(miljardairs),sg(miljardair)],de,[],[multi]).

n([bare_meas(miljoen),pl(miljoenen)],both,[meas_mod,measure]).

n([pl(miljonairs),sg(miljonair)],de,[],[multi]).

n([meas(mille)],het,[meas_mod,measure]).

n([pl(millennia),sg(millennium)],het,[temp_mod]).

n([pl(milten),sg(milt)],de,[]).

n([mass(miltvuur)],het,[]).

n([mass(mimiek)],de,[]).

n([pl(minnen),sg(min)],de,[pred_pp(in)]).

n([pl(minnen),sg(min)],de,[],
  [dim(minnetje)]).

n([mass(minachting)],de,[]).

n([sg(minaret),pl(minaretten)],de,[]).

n([pl(minderheden),sg(minderheid)],de,[pred_pp(in)]).

n([pl(minderingen),sg(mindering)],de,[]).

n([mass(minderwaardigheid)],de,[]).

n([pl(minderwaardigheidscomplexen),sg(minderwaardigheidscomplex)],het,[]).

n([pl(mineralen),sg(mineraal)],het,[app_measure]).

n([pl(mineraalwateren),pl(mineraalwaters),sg(mineraalwater)],het,[]).

n([sg(mineur),pl(mineurs),pl(mineuren)],both,[pred_pp(in)]).

n([mass(mini)],both,[]).

n([pl(miniaturen),sg(miniatuur)],both,[],[dim(miniatuurtje)]).

n([sg(minidisc),pl(minidiscs)],de,[]).

n([mass([minimal,music])],de,[]).

n([pl(minima),sg(minimum)],het,[]).

n([pl(ministers),sg(minister)],de,[],
  [h('CD'),
   h('CDA'),
   i('D66','D\'66-'),h('D66'),
   h('EU'),
   h(ex),
   h('GroenLinks'),
   justitie,
   landbouw,
   h('LPF'),
   milieu,
   onder,
   onderwijs,
   h(oud),oud,
   h('PvdA'),
   h('SP'),
   top,
   s(verkeer),
   h('VVD')]).

n([sg(ministerschap),pl(ministerschappen)],het,[]).

n([pl(ministeries),sg(ministerie)],het,[]).

n([pl(minnaars),pl(minnaren),sg(minnaar)],de,[]).

n([pl(minnaressen),sg(minnares)],de,[]).

n([pl(minors),sg(minor)],de,[]).

n([pl(minsten),sg(minste)],de,[]).

n([mass(mint)],de,[]).

n([sg(minus)],both,[]).

n([sg(minuut),pl(minuten)],de,[measure,
                               temp_mod,
                               sbar],
  [slot,
   speel,
   dim(minuutje)]).

n([pl(mirakelen),pl(mirakels),sg(mirakel)],het,
  [sbar,
   subject_sbar,
   subject_vp,
   vp]).

n([pl(missen),sg(mis)],de,[],
  [trouw]).

n([mass(misbaar)],het,[]).

n([sg(misbaksel),pl(misbaksels)],het,[app_measure]).

n([pl(misbruiken),sg(misbruik)],both,[],
  [alcohol,
   drank,
   i(kind,kinder),
   s(macht),
   i(vrouw,vrouwen)
  ]).

n([pl(misdaden),sg(misdaad)],de,[sbar,vp]).

n([pl(misdaden),sg(misdaad)],de,[],
  [s(oorlog)]).

n([pl(misdadigers),sg(misdadiger)],de,[],
  [s(oorlog)]).

n([pl(misdienaars),pl(misdienaren),sg(misdienaar)],de,[]).

n([sg(misdraging),pl(misdragingen)],de,[]).

n([pl(misdrijven),sg(misdrijf)],het,
  [sbar,vp]).

n([pl(misdrijven),sg(misdrijf)],het,[],
  [s(geweld),
   vlucht]).

n([pl(miseries),sg(miserie)],de,[]).

n([sg(misgreep),pl(misgrepen)],de,[]).

n([pl(mishandelingen),sg(mishandeling)],de,[sbar,vp]).

n([pl(mishandelingen),sg(mishandeling)],de,[],
  [i(kind,kinder)]).

n([pl(miskenningen),sg(miskenning)],de,[sbar,vp]).

n([sg(miskleun),pl(miskleunen)],de,[]).

n([pl(miskramen),sg(miskraam)],de,[]).

n([pl(misleidingen),sg(misleiding)],de,[sbar,vp]).

n([pl(mislukkelingen),sg(mislukkeling)],de,[]).

n([pl(mislukkingen),sg(mislukking)],de,[sbar,vp]).

n([sg(mismatch),pl(mismatchen)],de,[]).

n([sg(misrekening),pl(misrekeningen)],de,
  [subject_sbar,
   subject_vp]).

n([pl(missen),pl(misses),sg(miss)],de,[],
  [ronde]).

n([pl(misselijkheden),sg(misselijkheid)],de,[]).

n([pl(missers),sg(misser)],de,[]).

n([pl(missies),pl(missiën),sg(missie)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp
  ]).

n([pl(missies),pl(missiën),sg(missie)],de,[],
  [s(handel),
   s(veiligheid),
   s(vrede)]).

n([sg([missing,link]),pl([missing,links])],de,[]).

n([pl(missionarissen),sg(missionaris)],de,[]).

n([mass(misnoegen)],het,[]).

n([pl(misstanden),sg(misstand)],de,[subject_sbar]).

n([pl(misstappen),sg(misstap)],de,[sbar,vp],[dim(misstapje)]).

n([pl(misten),sg(mist)],de,[]).

n([mass(mister)],de,[]).

n([sg(mistrap),pl(mistrappen)],de,[]).

n([mass(mistrouwen)],het,[]).

n([pl(misvattingen),sg(misvatting)],de,[sbar,vp]).

n([pl(misverstanden),sg(misverstand)],het,[sbar,subject_sbar,subject_vp,vp]).

n([pl(misvormingen),sg(misvorming)],de,[]).

n([pl(misères),sg(misère)],de,[sbar,vp]).

n([pl(mitrailleurs),sg(mitrailleur)],de,[]).

n([pl([mitsen,en,maren])],both,[]).

n([pl(mixen),sg(mix)],de,[]).

n([sg([mixed,zone]),pl([mixed,zones])],de,[]).

n([sg(mmm)],de,[]).
%% de medische mallemolen
%% wordt veel gebruikt in Internet fora over zwanger worden...

n([sg(mobiel),pl(mobielen),pl(mobiels)],de,[],[dim(mobieltje)]).

n([pl(mobilisaties),sg(mobilisatie),
   pl(mobilizaties),sg(mobilizatie)],de,[]).

n([pl(mobiliteiten),sg(mobiliteit)],de,[]).

n([mass(modaal)],both,[]).   % inkomen boven modaal.

n([pl(modaliteiten),sg(modaliteit)],de,[]).

n([mass(modder)],de,[]).

n([pl(modderpoelen),sg(modderpoel)],de,[]).

n([pl(modes),sg(mode)],de,[],
  [s(najaar),
   s(voorjaar),
   winter,
   zomer]).

n([pl(modehuizen),sg(modehuis)],het,[]).

n([pl(modellen),sg(model)],het,[measure],[dim(modelletje)]).  % dat model steentjes

n([pl(modellen),sg(model)],het,[],
  [s(ontwikkeling),
   reken,
   rol,
   top]).

n([sg(modem),pl(modems)],both,[]).

n([sg(moderamen),pl(moderamina)],het,[]).

n([sg(moderator),pl(moderatoren)],de,[]).

n([mass(modernisering),pl(moderniseringen),sg(modernisering)],de,[]).

n([mass(modernisme)],het,[]).

n([sg(moderniteit),pl(moderniteiten)],de,[]).

n([mass(modernizering)],de,[]).

n([pl(modificaties),sg(modificatie)],de,[sbar,vp]).

n([pl(modifikaties),sg(modifikatie)],de,[sbar,vp]).

n([pl(modi),sg(modus)],de,[]).

n([sg(modulator),pl(modulatoren),pl(modulators)],de,[]).

n([sg(module),pl(modules)],de,[app_measure]).

n([sg([modus,operandi])],de,[]).

n([sg([modus,vivendi])],de,[]).

n([sg(moe)],de,[],[dim(moeke),dim(moeken),dim(moetje)]).

n([mass(moed)],de,[vp,sbar]).

n([mass(moedeloosheid)],de,[]).

n([pl(moeders),sg(moeder)],de,[],
  [s(bijstand),
   draag,
   oer,
   pleeg,
   schoon,
   stief,
   tiener,
   dim(moedertje)]).

n([sg(['Moeder',des,'Vaderlands']),pl(['Moeders',des,'Vaderlands'])],de,[]).

n([pl('moeder-oversten'),sg('moeder-overste')],de,[]).

n([sg(moederbord),pl(moederborden)],het,[]).

n([mass(moederschap)],het,[]).

n([mass(moederschoot)],de,[]).

n([pl(moedervlekken),sg(moedervlek)],de,[]).

n([mass(moeheid)],de,[]).

n([pl(moeilijkheden),sg(moeilijkheid)],de,
  [subject_sbar,
   sbar,
   subject_vp,
   vp,
   pred_pp_pl(uit),
   pred_pp_pl(in)]).

n([pl(moeilijkheidsgraden),sg(moeilijkheidsgraad)],de,[]).

n([mass(moeite),pl(moeiten),sg(moeite)],de,[sbar,subject_vp,vp]).

n([pl(moetjes),sg(moetje)],het,[]).

n([pl(moeren),sg(moer)],de,[],[dim(moertje)]).

n([pl(moerassen),sg(moeras)],het,[]).

n([mass(moes)],both,[]).  % celex het

n([pl(moestuinen),sg(moestuin)],de,[]).

n([pl(moffen),sg(mof)],de,[]).

n([pl(mogelijkheden),sg(mogelijkheid)],de,
  [sbar,vp,
   subject_sbar,
   subject_vp],
  [s(beroep),
   groei,
   h(keuze),keuze,
   s(toepassing)]).

n([pl(mogendheden),sg(mogendheid)],de,[]).

n([pl(mohammedanen),sg(mohammedaan)],de,[]).

n([pl(mokken),sg(mok)],de,[measure]).

n([pl(mokers),sg(moker)],de,[]).

n([pl(mollen),sg(mol)],de,[],[dim(molletje)]).

n([meas(mol)],de,[meas_mod]).

n([pl(moleculen),sg(molecule),sg(molecuul),
   pl(molekulen),sg(molekule),sg(molekuul)],both,[measure]).

n([pl(molens),sg(molen)],de,[np_app_measure],
  [beton,
   draai,
   koren,
   dim(molentje),
   water,
   wind]).

n([pl(molenaars),sg(molenaar)],de,[]).

n([pl(mommen),sg(mom)],both,
  [van_sbar,
   vp]).			% celex: de; 'onder het mom van:'

n([sg(moment),pl(momenten)],both,[measure,temp_mod,sbar],[dim(momentje)]).

n([sg([moment,suprême]),
   sg([moment,suprème])],het,[]).

n([pl(momentopnamen),pl(momentopnames),sg(momentopname)],de,[sbar]).

n([pl(monarchen),sg(monarch)],de,[]).

n([pl(monarchieën),sg(monarchie)],de,[]).

n([sg(monarchist),pl(monarchisten)],de,[]).

n([pl(monden),sg(mond)],de,[measure],[dim(mondje)]).

n([mass(['mond-',en,klauwzeer]),
   mass([mond,en,klauwzeer]),
   mass(mkz),
   mass('MKZ')],both,[]).

n([pl(mondholten),pl(mondholtes),sg(mondholte)],de,[]).

n([mass(mondialisering)],de,[]).

n([mass(mondigheid)],de,[]).

n([pl(mondingen),sg(monding)],de,[]).

n([pl(mondstukken),sg(mondstuk)],het,[]).

n([mass(money)],de,[]).

n([sg([money,transfer]),pl([money,transfers])],de,[]).

n([sg(mongool),pl(mongolen)],de,[]).

n([mass(monisme)],het,[]).

n([pl(monitors),sg(monitor),pl(monitoren)],de,[app_measure]).

n([mass(monitoring)],de,[]).

n([pl(monniken),sg(monnik)],de,[]).

n([pl(monografieën),sg(monografie)],de,[]).

n([pl(monologen),sg(monoloog)],de,[]).

n([pl(monopolies),pl(monopoliën),sg(monopolie)],het,[]).

n([sg(monopolist),pl(monopolisten)],de,[]).

n([mass(monotonie)],de,[]).

n([pl(monseigneurs),sg(monseigneur)],de,[]).

n([pl(messieurs),sg(monsieur)],de,[]).

n([mass(messing)],het,[]).

n([pl(messteken),sg(messteek)],de,[]).

n([pl(monsters),sg(monster)],het,[measure],
  [urine,
   dim(monstertje)]).

n([sg([monstre,sacré])],het,[]).

n([pl(montages),sg(montage)],de,[]).

n([pl(monteurs),sg(monteur)],de,[],
  [elektro,
   i(elektro,electro)
  ]).

n([pl(monturen),sg(montuur)],het,[]).

n([pl(monumenten),sg(monument)],het,[],
  [graf,
   s(oorlog),
   s(rijk),
   slavernij
  ]).

n([mass(monumentenzorg)],de,[]).

n([mass(mood)],de,[pred_pp(in)]).

n([mass(moois)],het,[]).

n([pl(moorden),sg(moord)],de,[],
  [baby,
   'December',december,
   i(kind,kinder),
   massa,
   i(volk,volkeren),
   i(volk,volken)]).

n([pl(moordenaars),sg(moordenaar)],de,[],
  [huur,
   serie,
   zelf]).

n([pl(moten),sg(moot)],de,[measure]).

n([pl(moten),sg(moot)],de,[],[hoofd]).

n([pl(moppen),sg(mop)],de,[sbar],[dim(mopje)]).

n([pl(moppies),sg(moppie)],het,[]).

n([mass(moraal)],both,[subject_sbar,subject_vp]).

n([mass(moralisme)],het,[]).

n([pl(moralisten),sg(moralist)],de,[]).

n([mass(moraliteit)],de,[]).

n([sg(moratorium)],het,[]).

n([mass(moreel)],het,[]).

n([pl(mores)],both,[]).

n([mass(morfine)],de,[]).

n([sg(morgen),pl(morgens)],de,[temp_mod,sbar],[zondag,
				      maandag,
				      dinsdag,
				      woensdag,
				      donderdag,
				      vrijdag,
				      zaterdag]).

n([pl(morgensterren),sg(morgenster)],de,[]).

n([pl(mormels),sg(mormel)],het,[],[dim(mormeltje)]).

n([sg(mormoon),pl(mormonen)],de,[]).

n([mass(mortel)],de,[]).

n([pl(mortieren),sg(mortier)],both,[]).

n([pl(mossen),sg(mos)],het,[]).

n([pl(moskeeën),sg(moskee)],de,[],
  [mega,
   dim(moskeetje)]).

n([pl(moslims),sg(moslim),
   pl('moslima\'s'),sg(moslima),
   pl('Moslims'),sg('Moslim')
  ],de,[],[h(niet)]).

n([pl(mosselen),pl(mossels),sg(mossel)],de,[],[dim(mosseltje)]).

n([mass(most)],de,[]).

n([mass(mosterd)],de,[]).

n([pl(motten),sg(mot)],de,[],
  [gras,
   mineer,
   schiet,
   sikkel,
   snuit,
   taster
  ]).

n([pl(motels),sg(motel)],het,[]).

n([pl(moties),sg(motie)],de,[vp]).

n([pl(motieven),sg(motief)],het,[sbar,vp],[dim(motiefje)]).

n([pl(motivaties),sg(motivatie)],de,[sbar,vp]).

n([pl(motiveringen),sg(motivering)],de,[sbar,vp]).

n([pl(motoren),pl(motors),sg(motor)],de,[],
  [benzine,
   buitenboord,
   diesel,
   elektro,i(elektro,electro),
   lijn,
   straal, % niet straal_mot_oor
   dim(motortje)]).

n([pl(motorboten),sg(motorboot)],de,[]).

n([mass(motoriek)],de,[]).

n([pl(motorkappen),sg(motorkap)],de,[]).

n([pl(motregens),sg(motregen)],de,[]).

n([sg(motte)],de,[]).  % kunstmatige heuvel

n([pl('motto\'s'),sg(motto)],het,
  [start_app_measure,
   np_app_measure,
   app_measure,
   subject_sbar]).

n([sg(mountainbike),pl(mountainbikes)],de,[]).

n([mass(mountainbike)],het,[]).

n([mass(mout)],both,[]).

n([pl(mouwen),sg(mouw)],de,[],[dim(mouwtje)]).

n([sg(movie),pl(movies)],de,[],
  [f([feel,good]),
   f([road])]).

n([pl(mozaïeken),sg(mozaïek)],both,[]).

n([mass(msn),mass('MSN')],both,[]).

n([mass(mts)],both,[]).

n([meas(mud)],both,[meas_mod,measure]).

n([pl(muggen),sg(mug)],de,[],
  [dans,
   gal,
   langpoot,
   paddenstoel,
   steek,
   stelt]).

n([pl(muilen),sg(muil)],de,[],[dim(muiltje)]).

n([pl(muildieren),sg(muildier)],het,[]).

n([pl(muilezels),sg(muilezel)],de,[]).

n([sg(muilkorf),pl(muilkorven)],de,[]).

n([pl(muizen),sg(muis)],de,[],
  [spits,  % geen spit_muis
   woel,
   dim(muisje)]).

n([sg(muisarm),pl(muisarmen)],de,[]).

n([pl(muiterijen),sg(muiterij)],de,[]).

n([pl(mujahedeen),sg(mujahed)],de,[]).

n([sg(mul),pl(mullen)],de,[]).  % vis

n([pl(mulatten),sg(mulat)],de,[]).

n([pl(mulattinnen),sg(mulattin)],de,[]).

n([pl(multinationals),sg(multinational)],de,[]).

n([sg(multimedia),pl(multimedia),
   sg('multi-media'),pl('multi-media'),
   sg([multi,media]),pl([multi,media])],de,[]).

n([sg(multiple),pl(multiples)],de,[]).

n([mass([multiple,choice]),
   mass('multiple-choice')],de,[]).

n([mass([multiple,sclerose]),
   mass([multipele,sclerose])],de,[]).

n([sg(mum)],de,[]).  % van tijd

n([pl(mummies),pl(mummiën),sg(mummie)],de,[],[dim(mummietje)]).

n([mass(munitie)],de,[]).

n([pl(munten),sg(munt)],de,[app_measure,np_app_measure],
  [s(eenheid),
   euro,
   festival,
   dim(muntje)]).

n([pl(munten),sg(munt)],de,[],
  [euro,
   peper
  ]).

n([sg(munteenheid),pl(munteenheden)],de,
  [app_measure,
   np_app_measure]).

n([pl(muntstukken),sg(muntstuk)],het,[]).

n([pl(mussen),sg(mus)],de,[],
  [i(heg,hegge),i(heg,heggen),
   dim(musje)]).

n([pl(musea),pl(museums),sg(museum)],het,[],
  [film,
   openlucht,
   s(rijk),
   strip,
   scheepvaart
  ]).

n([pl(musicals),sg(musical)],de,[]).

n([pl(musici),sg(musicus)],de,[],
  [pop]).

n([pl(muskieten),sg(muskiet)],de,[]).

n([mass(must)],de,[subject_sbar,subject_vp]).

n([pl(mutaties),sg(mutatie)],de,[]).

n([pl(mutsen),sg(muts)],de,[],[dim(mutsje)]).

n([pl(muren),sg(muur)],de,[],
  [buiten,
   spouw,
   s(stad),
   zij,
   dim(muurtje)]).

n([pl(muurschilderingen),sg(muurschildering)],de,[]).

n([pl(muzen),sg(muze)],de,[]).

n([pl(muzelmannen),sg(muzelman)],de,[]).

n([mass(muziek)],de,[],
  [achtergrond,
   barok,
   blad,
   dans,
   doedelzak,
   film,
   huse,
   jazz,
   kamer,
   orgel,
   piano,
   pop,
   punk,
   rock,
   straat,
   theater,
   toekomst,
   toneel,
   s(volk),
   wereld,
   zigeuner,
   dim(muziekje)]).

n([pl(muziekkritieken),sg(muziekkritiek)],de,[]).

n([pl(muziekstukken),sg(muziekstuk)],het,[]).

n([sg(muziekzender),pl(muziekzenders)],de,[]).

n([pl(muzikanten),sg(muzikant)],de,[],
  [pop,
   sessie,
   studio]).

n([mass([myeloïde,leukemie])],de,[]).

n([pl(mysteries),pl(mysteriën),sg(mysterie)],het,[sbar]).

n([sg([mystery,man])],de,[]).

n([pl(mystici),sg(mysticus)],de,[]).

n([mass(mystiek)],de,[]).

n([pl(mythen),
   pl(mythes),
   sg(mythe)],de,
  [sbar,
   subject_sbar]).

n([pl(mythologieën),sg(mythologie)],de,[]).

n([pl('n\'s'),sg(n)],de,[],[dim('n\'etje')]).

n([pl(naden),sg(naad)],de,[]).

n([pl(naaimachines),sg(naaimachine)],de,[]).

n([pl(naaisters),sg(naaister)],de,[],[dim(naaistertje)]).

n([pl(naakten),sg(naakt)],het,[]).

n([mass(naaktheid)],de,[]).

n([pl(naalden),sg(naald)],de,[],
  [kiezel]).

n([pl(namen),sg(naam)],de,[sbar,
			   vp,
			   app_measure,
			   np_app_measure]).

n([pl(namen),sg(naam)],de,[app_measure],
  [achter,
   i(artiest,artiesten),
   s(auteur),
   s(bedrijf),
   club,
   code,
   domein,
   familie,
   s(geslacht),
   i(geus,geuzen),
   s(god),s('God'),
   s(jongen),
   koos,
   s(meisje),
   plaats,
   roep,
   sponsor,
   spot,
   straat,
   toe,
   vogel,
   werk,
   dim(naampje)]).

n([mass(naamgeving)],de,[]).

n([pl(naamsbekendheden),sg(naamsbekendheid)],de,[]).

n([sg(naamval),pl(naamvallen)],de,[]).

n([mass(naastenliefde)],de,[]).

n([pl(nabestaanden),sg(nabestaande)],de,[]).

n([mass(nabijheid)],de,[]).

n([pl(nabootsingen),sg(nabootsing)],de,[sbar,vp]).

n([sg(nacht),pl(nachten)],de,
  [measure,temp_mod,sbar],
  [zondag,
   maandag,
   dinsdag,
   woensdag,
   donderdag,
   vrijdag,
   zaterdag,
   kerst,
   millennium,
   s(nieuwjaar),
   s(oudejaar),
   s(oudjaar),
   winter,
   zomer,
   dim(nachtje)]).

n([pl(nachtegalen),sg(nachtegaal)],de,[],[dim(nachtegaaltje)]).

n([mass(nachtleven)],het,[]).

n([pl(nachtmerries),sg(nachtmerrie)],de,[sbar,vp]).

n([pl(nachtponnen),sg(nachtpon)],de,[]).

n([pl(nachtwakers),sg(nachtwaker)],de,[]).

n([pl(nadagen)],both,[]).

n([pl(nadelen),sg(nadeel)],het,[sbar,
                                vp,
                                subject_sbar,
                                subject_vp,
                                pred_pp(in),
                                pred_pp(in,subject_sbar),
                                pred_pp(in,subject_vp)]).

n([mass(nadering)],de,[]).

n([pl(nadrukken),sg(nadruk)],de,[]).

n([sg(naga)],de,[]).

n([sg(nagalm)],de,[]).

n([mass(nagedachtenis)],de,[]).

n([pl(nagelen),pl(nagels),sg(nagel)],de,[],
  [kunst,
   vinger,
   dim(nageltje)]).

n([pl(nageslachten),sg(nageslacht)],het,[]).

n([mass(naijver)],de,[]).

n([sg(najaar),pl(najaren)],het,[temp_mod,sbar]).

n([pl(nakomelingen),sg(nakomeling)],de,[],
  [dim(nakomelingetje)]).

n([pl(nakomelinges),sg(nakomelinge)],de,[],[]).

n([pl(nalatenschappen),sg(nalatenschap)],both,
  [subject_sbar]).

n([pl(nalatigheden),sg(nalatigheid)],de,[]).

n([mass(naleving)],de,[]).

n([mass(naloop)],de,[]).

n([mass(namaak)],de,[]).

n([pl(namiddagen),sg(namiddag)],de,
  [temp_mod,sbar,measure],
  [zondag,
   maandag,
   dinsdag,
   woensdag,
   donderdag,
   vrijdag,
   zaterdag,
   zondag]).

n([mass(nandrolon)],both,[]).

n([mass(napalm)],both,[]).

n([pl(napoleons),sg(napoleon)],de,[]).

n([pl(narcissen),sg(narcis)],de,[]).

n([mass(narcisme)],het,[]).

n([mass(narcose)],de,[]).

n([pl(narigheden),sg(narigheid)],de,[sbar,vp]).

n([sg(narratief),pl(narratieven)],het,[]).

n([mass(nasleep)],de,[]).

n([sg(nasporing),pl(nasporingen)],de,[]).

n([mass(nat)],het,[],[dim(natje)]).

n([pl(naties),pl(natiën),sg(natie)],de,[]).

n([sg([nationaal,park])],het,[]).

n([mass('nationaal-socialisme')],het,[]).

n([pl('nationaal-socialisten'),sg('nationaal-socialist')],de,[]).

n([pl(nationaalsocialisten),sg(nationaalsocialist)],de,[]).

n([sg(nationaler),pl(nationalers)],de,[]).   % Vlaams

n([pl(nationalisaties),sg(nationalisatie)],de,[]).

n([mass(nationalisme)],het,[]).

n([pl(nationalisten),sg(nationalist)],de,[]).

n([pl(nationaliteiten),sg(nationaliteit)],de,[pred_pp(van)]).

n([sg(natrap)],de,[]).

n([mass(natrium)],het,[]).

n([mass(nattigheid)],de,[]).

n([mass(natura)],de,[]).

n([sg(naturalisatie),pl(naturalisaties)],de,[]).

n([mass(naturalisme)],het,[]).

n([sg(naturel)],both,[]).  % de natural=>ouderwets

n([pl(naturen),sg(natuur)],de,[]).

n([mass(natuurkunde)],de,[]).

n([sg(natuurkundige),pl(natuurkundigen)],de,[]).

n([mass(natuurlijkheid)],de,[]).

n([pl(natuurverschijnselen),pl(natuurverschijnsels),sg(natuurverschijnsel)],het,[]).

n([pl(natuurwetenschappen),sg(natuurwetenschap)],de,[]).

n([mass(nauw)],het,[]).

n([mass(nauwgezetheid)],de,[]).

n([mass(nauwkeurigheid)],de,[]).

n([pl(navels),sg(navel)],de,[],[dim(naveltje)]).

n([pl(navelstrengen),sg(navelstreng)],de,[]).

n([mass(navigatie)],de,[]).

n([pl(navolgers),sg(navolger)],de,[]).

n([pl(navolgingen),sg(navolging)],de,[]).

n([mass(navraag)],de,[]).

n([pl(naweeën),sg(nawee)],het,[]).

n([pl(nawerkingen),sg(nawerking)],de,[sbar,vp]).

n([pl(nazaten),sg(nazaat)],de,[]).

n([pl('nazi\'s'),sg(nazi),
   pl('Nazi\'s'),sg('Nazi')],de,[],
  [neo,
   h(neo)]).

n([mass(nazisme)],het,[]).

n([pl(nazomers),sg(nazomer)],de,[]).

n([pl(nazorgen),sg(nazorg)],de,[sbar,vp]).

n([pl(naïviteiten),sg(naïviteit)],de,[]).

n([sg([near,banker]),pl([near,bankers])],de,[]).

n([sg([nectar])],de,[]).

n([mass(nederigheid)],de,[]).

n([pl(nederlagen),sg(nederlaag)],de,[sbar,vp]).

n([pl(nederlagen),sg(nederlaag)],de,[],
  [thuis,
   uit]).

n([pl(nederzettingen),sg(nederzetting)],de,[]).

n([mass(nee)],het,[]).

n([pl(neven),sg(neef)],de,[],[dim(neefje)]).

n([mass(neen)],het,[]).

n([mass(neergang)],de,[]).

n([pl(neerlandici),sg(neerlandicus)],de,[]).

n([mass(neerlandistiek)],de,[]).

n([mass(neerslachtigheid)],de,[]).

n([pl(neerslagen),sg(neerslag)],de,[]).

n([pl(negaties),sg(negatie)],de,[]).

n([sg(negatief),pl(negatieven)],het,[]).

n([pl(negens),sg(negen)],de,[]).

n([pl(negenden)],both,[]).  %??

n([bare_meas(negenmaal),pl(negenmalen)],both,[temp_mod,measure,sbar]).

n([pl(negentigers),sg(negentiger)],de,[]).

n([pl(negers),sg(neger)],de,[],[dim(negertje)]).

n([pl(negerinnen),sg(negerin)],de,[],[dim(negerinnetje)]).

n([pl(negerslaven),sg(negerslaaf)],de,[]).

n([pl(neigingen),sg(neiging)],de,[sbar,vp]).

n([pl(nekken),sg(nek)],de,[]).

n([pl(nekharen),sg(nekhaar)],het,[]).

n([mass(nekkramp)],de,[]).

n([mass(nekvel)],het,[]).

n([pl(nellen),sg(nel)],de,[]).

n([sg(neologisme),pl(neologismen)],het,[]).

n([mass(nep)],de,[]).

n([mass(nephaar)],het,[]).

n([sg(nephaar),pl(nepharen)],de,[]).

n([sg(nerd),pl(nerds)],de,[]).

n([sg(nerts),pl(nertsen)],de,[]).

n([pl(nerven),sg(nerf)],de,[]).

n([pl(neringen),sg(nering)],de,[],[dim(nerinkje)]).

n([mass(nervositeit)],de,[]).

n([sg([nervous,breakdown])],de,[]).

n([pl(nesten),sg(nest)],het,[measure],[dim(nestje)]).

n([pl(nesten),sg(nest)],het,[],
  [s(adelaar),  % Ade_laars_nest
   dim(nestje)]).

n([pl(nestors),sg(nestor)],de,[]).

n([pl(netten),sg(net)],het,[],
  [aansluit,
   aardgas,
   i(auto_snel_weg,autosnelwegen),
   bagage,
   bus,
   dealer,
   distributie,
   drijf,
   s(elektriciteit),
   i(elektriciteit,electriciteits),
   energie,
   ether,
   familie,
   gas,
   glasvezel,
   hoofd,
   i(hoofd_rail,hoofdrail),
   i(hoofd_weg,hoofdwegen),
   i(hoog_snelheid,hogesnelheids),
   s(hoogspanning),
   intercity,
   intra,
   jeugd,
   kabel,
   i(kantoor,kantoren),
   kennis,
   kern,
   i(kind,kinder),
   kraak,
   i(laag_spanning,laagspannings),
   leiding,
   licht,
   i(lijn,lijnen),
   meet,
   metro,
   i(muskiet,muskieten),
   nacht,
   nood,
   onderwijs,
   radio,
   rail,
   s(riolering),
   i(rijk_weg,rijkswegen),
   route,
   schep,
   sleep,
   i(snel_weg,snelwegen),
   spoor,
   spoorweg,
   i(spoorweg,spoorwegen),
   s(stad),
   stroom,
   telecom,
   telefoon,
   televisie,h(tv),
   thuis,
   tram,
   transport,
   vang,
   s(veiligheid),
   s(vervoer),
   vis,
   vlinder,
   s(voorstad),
   waterleiding,
   i(weg,wegen),
   zender,
   zij,
   dim(netje)]).

n([mass(netheid)],de,[]).

n([sg(netter),pl(netters)],de,[]).

n([pl(netvleugeligen),sg(netvleugelige)],de,[]).

n([pl(netvliezen),sg(netvlies)],het,[]).

n([pl(netwerken),sg(netwerk)],het,[],
  [computer,
   kabel,
   i(spoorweg,spoorwegen),
   terreur,
   i(terrorist,terroristen)]).

n([sg(netwerking),pl(netwerkingen)],de,[]).

n([sg(neuker),pl(neukers)],de,[],[dim(neukertje)]).

n([pl(neurologen),sg(neuroloog)],de,[]).

n([sg(neuron),pl(neuronen)],both,[]).

n([pl(neurosen),pl(neuroses),sg(neurose)],de,[]).

n([pl(neurotici),sg(neuroticus)],de,[]).

n([sg(neurotransmitter),pl(neurotransmitters)],de,[]).

n([pl(neuzen),sg(neus)],de,[],
  [loop,
   dim(neusje)]).

n([pl(neusgaten),sg(neusgat)],het,[]).

n([sg(neushoorn),pl(neushoorns)],de,[]).

n([pl(neusvleugels),sg(neusvleugel)],de,[]).

%% beoordeling/rating/

n([mass('neutraal+'),mass([neutraal,'+'])],both,[]).

n([mass('neutraal-'),mass([neutraal,'-'])],both,[]).

n([mass(neutraliteit)],de,[]).

n([sg(neutron),pl(neutronen)],het,[],[anti]).

n([pl(nevelen),pl(nevels),sg(nevel)],de,[],
  [ochtend,
   dim(neveltje)]).

n([pl(neveneffecten),sg(neveneffect),
   pl(neveneffekten),sg(neveneffekt)],het,
  [sbar,
   vp,
   subject_sbar,
   subject_vp]).

n([mass([new,wave])],de,[]).

n([meas(newton)],de,[meas_mod,measure]).

n([stem(ngo),
   sg(ngo),pl('ngo\'s'),
   sg('NGO'),pl('NGO\'s')],de,[]).

n([sg(niche),pl(niches)],de,[]).

n([pl(nichten),sg(nicht)],de,[],[dim(nichtje)]).

n([mass(nicotine)],de,[]).

n([mass(niemandsland)],het,[]).

n([mass(niemendal)],het,[]).

n([pl(nieren),sg(nier)],de,[],
  [bij,
   dim(niertje)]).

n([sg(nies),pl(niesen)],de,[]).

n([mass([niet,ontvankelijkheid]),mass('niet-ontvankelijkheid')],de,[]).

n([sg(['Niet','Nixer']),
   pl(['Niet','Nixers'])],de,[]).

n([pl(nietigheden),sg(nietigheid)],de,[sbar,vp]).

n([mass(niets)],het,
  [meas_mod,
   pred_pp(voor),
   subject_sbar,
   subject_vp]).                % uit het niets

n([mass(nietsdoen)],het,[]).

n([pl(nietsnutten),sg(nietsnut)],de,[]).

n([mass(nieuwbouw)],de,[]).

n([pl(nieuwelingen),sg(nieuweling)],de,[]).

n([mass('Nieuwgrieks')],het,[]).

n([pl(nieuwigheden),sg(nieuwigheid)],de,[sbar,vp]).

n([pl(nieuwjaars),sg(nieuwjaar)],het,[temp_mod,sbar]).

n([pl(nieuwjaarstoespraken),sg(nieuwjaarstoespraak)],de,[]).

n([pl(nieuwkomers),sg(nieuwkomer)],de,[]).

n([mass(nieuwlichterij)],de,[]).

n([mass(nieuws)],het,[pred_pp(in)],
  []).

n([mass(nieuws)],het,[subject_sbar,sbar],
  [nep,
   voorpagina,
   wereld]).

n([mass(nieuws)],het,[],
  [s(bedrijf),
   radio,
   sport]).

n([pl(nieuwsberichten),sg(nieuwsbericht)],het,[sbar]).

n([pl(nieuwsbrieven),sg(nieuwsbrief)],de,[]).

n([mass(nieuwsgaring)],de,[]).

n([mass(nieuwsgierigheid)],de,[]).

n([pl(nieuwslezers),sg(nieuwslezer)],de,[]).

n([sg(nieuwszender),pl(nieuwszenders)],de,[]).

n([pl(nieuwtjes),sg(nieuwtje)],het,[sbar]).

n([mass(nihil)],both,[]).

n([mass(nihilisme)],het,[]).

n([mass(nijd)],de,[]).

n([pl(nijlpaarden),sg(nijlpaard)],het,[]).

n([mass(nijverheid)],de,[],
  [bouw,
   kunst,
   metaal]).

n([mass(niks)],het,
  [meas_mod,
   pred_pp(voor),
   subject_sbar,
   subject_vp]). 

n([pl(nimfen),sg(nimf)],de,[],
  [bos,
   zee]).

n([sg(nippertje)],het,[]).

n([pl(nissen),sg(nis)],de,[]).

n([sg(nitraat),pl(nitraten)],het,[]).

n([sg(nitromusk),pl(nitromusks)],de,[]).

n([pl(niveaus),sg(niveau)],het,[pred_pp(onder),pred_pp(onder,subject_vp),
				pred_pp(op),
				app_measure,
				pred_pp(beneden),pred_pp(beneden,subject_vp),
				pred_pp(boven),pred_pp(boven,subject_vp),
				pred_pp(van)],
  [s(opleiding),
   top]).

n([pl(niveaus),sg(niveau)],het,[],
  [s(deelnemer),
   s(dreiging),
   s(geluid),
   prijs,
   zee]).

n([pl(nivelleringen),sg(nivellering)],de,[]).

n([sg('NGO'),pl('NGO\'s')],de,[]).

n([mass([no,claim])],de,[]).

n([sg('no-nonsense')],de,[]).

n([sg([no,go,area]),  pl([no,go,areas]),
   sg(['no-go',area]),pl(['no-go',areas])],de,[]).

n([sg([no,show]),pl([no,shows])],de,[]).

n([sg(node)],de,[pred_pp(van)]).

n([pl(noemers),sg(noemer)],de,[sbar,app_measure,start_app_measure]).

n([pl(nokken),sg(nok)],de,[]).

n([pl(nollen),sg(nol)],de,[]).

n([pl(nomaden),sg(nomade)],de,[],
  [s(stad)]).

n([sg(nomenclatuur),pl(nomenclaturen)],de,[]).

n([pl(nominaties),sg(nominatie)],de,[]).

n([pl(nonnen),sg(non)],de,[],[dim(nonnetje)]).

n([mass([non,food]),mass('non-food')],de,[]).

n([sg([non,playing,captain]), pl([non,playing,captains]),
   sg(['non-playing',captain]), pl(['non-playing',captains])],de,[]).

n([mass([non,profit]),
   mass('non-profit')],de,[]).

n([mass(nonchalance)],de,[]).

n([pl(nonkels),sg(nonkel)],de,[]).

n([mass(nonsens)],de,[]).

n([pl(noden),sg(nood)],de,[sbar,vp],[geld,
				     tijd]).

n([pl(noodgevallen),sg(noodgeval)],het,[sbar,vp]).

n([pl(noodkreten),sg(noodkreet)],de,[sbar]).

n([mass(noodlot)],het,[subject_sbar,
		       subject_vp]).

n([pl(noodtoestanden),sg(noodtoestand)],de,[]).

n([sg(nooduitgang),pl(nooduitgangen)],de,[]).

n([sg(noodverordening),pl(noodverordeningen)],de,[]).

n([mass(noodweer)],het,[]).

n([mass(noodzaak)],de,[vp,sbar]).

n([pl(noodzakelijkheden),sg(noodzakelijkheid)],de,[sbar,vp]).

n([mass(noord),
   mass('Noord')],both,[]).

n([sg('noord-zuid'),
   pl('noord-zuid')],both,[]).

n([mass(noorden),
   mass('Noorden')],het,[]).

n([mass(noordkant)],de,[]).

n([mass(noordoosten)],het,[]).

n([mass(noordpool)],de,[]).

n([mass(noordwesten)],het,[]).

n([mass(noordzee)],de,[]).

%% je moet de melodie een halve noot verhogen
n([pl(noten),sg(noot)],de,
  [sbar,
   vp,
   meas_mod],
  [dim(nootje)]).

n([pl(noten),sg(noot)],de,
  [],
  [peper,
   dim(nootje)]).

n([mass(nootmuskaat)],de,[]).

n([pl(nopjes)],de,[pred_pp(in)]).

n([sg(nor),pl(norren)],de,[]).

%% wormen in Naarden
n([pl(normen),sg(norm)],de,
  [subject_sbar,
   subject_vp,
   sbar,
   vp]).

n([pl(normen),sg(norm)],de,[],
  [s(geluid),
   milieu,
   minimum,
   s(veiligheid),
   parkeer,
   'Zalm',h('Zalm')]).

n([pl(normalen),sg(normaal)],de,[]).

n([pl(normaliseringen),sg(normalisering)],de,[]).

n([pl(normeringen),sg(normering)],de,[sbar,vp]).

n([mass(nostalgie)],de,[]).

n([mass([no,time])],de,[]).  % binnen/in no time

n([pl('nota\'s'),sg(nota)],de,[sbar,
			       vp,
			       app_measure,
			       np_app_measure]).

n([pl('nota\'s'),sg(nota)],de,[app_measure,
                               np_app_measure],
  [s(beleid),
   credit,
   debet,
   i(contour,contouren),
   drug,s(drug),
   i(miljoen,miljoenen),
   s(voorjaar),
   s(welzijn)]).

n([pl(notabelen),sg(notabele)],de,[]).

n([pl(notarissen),sg(notaris)],de,[]).

n([sg(notatie)],de,[]).

n([sg('note-book'),pl('note-books'),
   sg(notebook),pl(notebooks)],
  de,[]).

n([pl(noteringen),sg(notering)],de,[]).

n([pl(noties),sg(notie)],de,[sbar,vp,app_measure]).

n([sg(notificatie),pl(notificaties)],de,[]).

n([pl(notities),sg(notitie)],de,[sbar,vp]).

n([pl(notulen)],both,[]).

n([mass(nougat)],de,[]).

n([sg([nouveau,riche]),pl([nouveaux,riches])],de,[]).

n([pl(novellen),sg(novelle)],de,[np_app_measure]).

n([sg(nova)],de,[]).

n([sg(november)],de,[temp_mod,sbar]).

n([pl(nova),sg(novum)],het,[]).

n([sg(noviteit),pl(noviteiten)],de,[]).

n([sg('NRA'),pl('NRA\'s')],de,[]).

n([sg('NRI'),pl('NRI\'s')],de,[]).

n([pl(nuancen),pl(nuances),sg(nuance)],de,[sbar,vp]).

n([pl(nuanceringen),sg(nuancering)],de,[sbar,vp]).

n([mass(nuchterheid)],de,[]).

n([pl(nullen),sg(nul)],de,[],[dim(nulletje)]).

n([mass(nulpunt)],het,[]).

n([pl(nummers),sg(nummer)],both,[meas_mod]).

n([pl(nummers),sg(nummer),
   ignore_stem(nummer)],het,[],
  [h('06'),
   alarm,
   atoom,
   s(boeking),
   i(burger_service,burgerservice),
   'e-ticket',
   giro,
   order,
   referentie,
   rekening,
   rug,
   sofi,h(sofi),
   stam,  % hebben voetbalclubs bij de bond
   telefoon,
   thema,
   volg,
   zomer]).

n([sg([nummer,laatst]),pl([nummers,laatst])],de,[]).

n([sg([nummer,voorlaatst]),pl([nummers,voorlaatst])],de,[]).

n([sg(numero),sg(nummero)],both,[]).

n([pl(nummertjes),sg(nummertje)],het,[measure]).  % een nummertje Xen

n([mass(nut)],het,[]).

n([sg(nutriënt),pl(nutriënten)],de,[]).

n([mass(nutteloosheid)],de,[]).

n([sg(nv),pl('nv\'s')],de,[]).

n([pl(nylons),sg(nylon)],de,[]).

n([mass(nylon)],het,[]).

n([pl('o\'s'),sg(o)],de,[],[dim('o\'tje')]).

n([pl(oasen),pl(oases),sg(oase)],de,[]).

n([pl(obelisken),sg(obelisk)],de,[]).

n([pl(obers),sg(ober)],de,[]).

n([pl(objecten),sg(object),
   pl(objekten),sg(objekt)],het,[],
  [hemel]).

n([sg(objectief),pl(objectieven)],het,[]).

n([mass(objectiviteit),
   mass(objektiviteit)],de,[]).

n([sg(oblast),pl(oblasten)],both,[]). % Oosteuropese geografische eenheid?

n([pl(obligaties),sg(obligatie)],de,[],
  [s(bedrijf),
   s(staat)]).

n([pl(observaties),sg(observatie)],de,[sbar]).

n([pl(obsessies),sg(obsessie)],de,[sbar,vp]).

n([pl(obstakels),sg(obstakel)],het,[sbar,vp,subject_sbar,subject_vp]).

n([mass(obstipatie)],de,[]).

n([pl(obstructies),sg(obstructie)],de,[]).

n([mass(occultisme)],het,[]).

n([pl(oceanen),sg(oceaan)],de,[]).

n([sg(ochtend),pl(ochtenden)],de,[temp_mod,sbar,measure],
  [zondag,
   maandag,
   dinsdag,
   woensdag,
   donderdag,
   vrijdag,
   zaterdag,
   dim(ochtendje)]).

n([mass(ochtendgloren)],het,[]).

n([sg(ochtendspits),pl(ochtendspitsen)],de,[]).

n([pl(ochtenduren)],het,[]).

n([mass(ochtendzon)],de,[]).

n([pl(octaven),sg(octaaf)],het,[]).

n([pl(octrooien),sg(octrooi)],het,[]).

n([pl(oden),pl(odes),sg(ode)],de,[]).

n([mass(oecumene)],de,[]).

n([pl(oefeningen),sg(oefening)],de,[vp],[dim(oefeningetje)]).

n([pl(oefeningen),sg(oefening)],de,[],
  [godsdienst,
   loop,
   dim(oefeningetje)]).

n([pl(oefenmeesters),sg(oefenmeester)],de,[]).

n([sg(oelewapper),pl(oelewappers)],de,[]).

n([sg(oenologie)],de,[]).  % wijnbouwtechnische wetenschap

n([pl(oerwouden),sg(oerwoud)],het,[]).

n([pl(oesters),sg(oester)],de,[],[dim(oestertje)]).

n([pl(oestrogenen),sg(oestrogeen)],het,[]).

n([pl(oeuvres),sg(oeuvre)],het,[]).

n([pl(oevers),sg(oever)],de,[],
  [noord,
   linker,
   rechter,
   zuid]).

n([pl(offensieven),sg(offensief)],het,[],
  [grond,
   tegen]).

n([pl(offers),sg(offer)],het,[],
  [loon,
   stuk,
   dim(offertje)
  ]).

n([pl(offerten),pl(offertes),sg(offerte)],de,[],[]).

n([sg([office,manager]),pl([office,managers])],de,[]).

n([sg(official),pl(officials)],de,[]).

n([pl(officieren),pl(officiers),sg(officier)],de,[],
  [hoofd,
   hulp,
   leger,
   onder,
   pers,
   s(verbinding),
   dim(officiertje)]).

n([sg('officier-fiscaal'),pl('officieren-fiscaal')],de,[]).

n([sg(offshore)],de,[]).

n([sg(ogenblik),pl(ogenblikken)],het,[measure,temp_mod,sbar],[dim(ogenblikje)]).

n([meas(ohm)],both,[meas_mod,measure]).

n([sg(ok)],de,[]).  % operatiekamer

n([pl(okers),sg(oker)],de,[]).

n([mass(okkultisme)],het,[]).

n([pl(oksels),sg(oksel)],de,[],[dim(okseltje)]).

n([pl(oktaven),sg(oktaaf)],het,[]).

n([sg(oktober),sg(october)],de,[temp_mod,sbar]).

n([mass('oktober-revolutie'), mass(oktoberrevolutie)],de,[]).

n([pl(oktrooien),sg(oktrooi)],het,[]).

n([sg(oldtimer),pl(oldtimers)],de,[]).

n([sg([old,boys,network]),
   sg([old,boys,netwerk]),
   pl([old,boys,networks]),
   pl([old,boys,netwerken]),
   sg([old,'boys-network']),
   pl([old,'boys-networks'])],het,[]).

n([pl(olies),pl(oliën),sg(olie)],de,[],
  ['Brent',
   diesel,
   i(haarlem,haarlemmer),
   stook]).

n([pl(oliebollen),sg(oliebol)],de,[]).

n([pl(olifanten),sg(olifant)],de,[],
  [dim(olifantje)]).

n([sg(oligarch),pl(oligarchen)],de,[]).

n([pl(olijven),sg(olijf)],de,[]).

n([pl(olijfbomen),sg(olijfboom)],de,[]).

n([mass(olijfolie)],de,[]).

n([pl(olympiaden),pl(olympiades),sg(olympiade)],de,[]).

n([pl('oma\'s'),sg(oma)],de,[],[dim(omaatje)]).

n([pl(omarmingen),sg(omarming)],de,[]).

n([mass(ombouw)],de,[]).

n([pl(ombudsmannen),sg(ombudsman)],de,[]).

n([pl(ombuigingen),sg(ombuiging)],de,[sbar,vp]).

n([sg(omcirkel),pl(omcirkels)],de,[]).

n([pl(omes),sg(ome)],de,[]).

n([pl(omeletten),sg(omelet)],de,[]).

n([pl(omgangen),sg(omgang)],de,[]).

n([sg(omgangsvorm),pl(omgangsvormen)],de,[]).

n([mass(omgeving),pl(omgevingen)],de,[],
  [klant,
   school]).

n([mass(omgevingsveld)],het,[]).

n([pl(omhalen),sg(omhaal)],de,[sbar,vp]).

n([pl(omheiningen),sg(omheining)],de,[],[dim(omheininkje)]).

n([pl(omhelzingen),sg(omhelzing)],de,[]).

n([pl(omhulsels),sg(omhulsel)],het,[]).

n([pl(omissies),sg(omissie)],de,[]).

n([sg(omkadering),pl(omkaderingen)],de,[]).  % geen om_kade_ring

n([mass(omkeer)],de,[]).

n([pl(omkeringen),sg(omkering)],de,[]).

n([pl(omkopingen),sg(omkoping)],de,[]).

n([pl(omleidingen),sg(omleiding)],de,[]).

n([pl(omlijstingen),sg(omlijsting)],de,[]).

n([pl(omlopen),sg(omloop)],de,[temp_mod]).

n([pl(omloopbanen),sg(omloopbaan)],de,[]).

n([mass(ommekeer)],de,[]).

n([pl(ommezwaaien),sg(ommezwaai)],de,[]).

n([sg(omnivoor),pl(omnivoren)],de,[]).

n([pl(omroepen),sg(omroep)],de,[],[s(staat),
				   s(stad),
				   wijk]).

n([mass(omroepbestel)],het,[]).

n([pl(omroepbijdragen),sg(omroepbijdrage)],de,[]).

% en niet om_roe_pers
n([sg(omroeper),pl(omroepers)],de,[]).

n([sg(omroepster),pl(omroepsters)],de,[]).

n([sg(omruil)],de,[]).

n([sg(omschakeling),pl(omschakelingen)],de,[]).

n([sg(omscholing),pl(omscholingen)],de,[]).

n([pl(omschrijvingen),sg(omschrijving)],de,
  [sbar,
   vp,
   app_measure]).

n([pl(omschrijvingen),sg(omschrijving)],de,
  [app_measure],
  [functie,
   taak]).

n([pl(omsingelingen),sg(omsingeling)],de,[]).

n([pl(omslagen),sg(omslag)],both,[sbar]).

n([pl(omslagen),sg(omslag)],both,[],
  [cultuur]).

%% belastingzaken?
n([mass([omslag,gebouwd])],de,[]).

n([pl(omslagdoeken),sg(omslagdoek)],de,[]).

n([pl(omstanders),sg(omstander)],de,[]).

n([pl(omstandigheden),sg(omstandigheid)],de,[sbar,vp]).

n([pl(omstandigheden),sg(omstandigheid)],de,[],
  [s(arbeid),
   leef,
   s(leven),
   markt,
   s(weer),
   weer,
   werk]).

n([pl(omstreken),sg(omstreek)],de,[]).

n([pl(omtrekken),sg(omtrek)],de,[]).

n([mass(omvaart)],de,[]). % VL de lange omvaart

n([mass(omvang)],de,
  [pred_pp(van)]).

n([mass(omvang)],de,[],
  [s(bevolking)]).

n([pl(omvormingen),sg(omvorming)],de,[]).

n([pl(omwentelingen),sg(omwenteling)],de,[]).

n([pl(omwisselingen),sg(omwisseling)],de,[]).

n([sg(omwonende),pl(omwonenden)],de,[]).

n([pl(omzetten),sg(omzet)],de,[],
  [concern,
   jaar,
   kwartaal]).

n([mass(omzetgroei)],de,[]).

n([pl(omzettingen),sg(omzetting)],de,[sbar,vp]).

n([pl(omzichtigheden),sg(omzichtigheid)],de,[]).

n([pl(omzwervingen),sg(omzwerving)],de,[]).

n([mass(onaanvaardbaar)],het,[]).

n([mass(onafhankelijkheid)],de,[]).

n([pl(onafhankelijkheidsverklaringen),sg(onafhankelijkheidsverklaring)],de,[]).

n([mass(onbegrip)],het,[]).

n([mass(onbehagen)],het,[]).

n([mass(onbekendheid),pl(onbekendheden),sg(onbekendheid)],de,[]).

n([mass(onbekwaamheid)],de,[]).

n([pl(onbenullen),sg(onbenul)],both,[]).

n([pl(onbeschaamdheden),sg(onbeschaamdheid)],de,[sbar,vp]).

n([mass(onbetrouwbaarheid)],de,[]).

n([mass(onbevangenheid)],de,[]).

n([mass(onbeweeglijkheid)],de,[]).

n([mass(onbewuste)],het,[]).

n([mass(onbruik)],het,[]).

n([sg(oncoloog),pl(oncologen)],de,[]).

n([mass(onderbewuste)],het,[]).

n([mass(onderbewustzijn)],het,[]).

n([mass(onderbouw)],de,[]).

n([sg(onderbouwing),pl(onderbouwingen)],de,[]).

n([pl(onderbrekingen),sg(onderbreking)],de,
  [sbar,vp]).

n([pl(onderbrekingen),sg(onderbreking)],de,
  [],
  [loopbaan]).

n([pl(onderbroeken),sg(onderbroek)],de,[]).

n([pl(onderbuiken),sg(onderbuik)],de,[]).

n([pl(onderdanen),sg(onderdaan)],de,[]).

n([mass(onderdak)],het,[]).

n([mass(onderdanigheid)],de,[]).

n([pl(onderdelen),sg(onderdeel)],het,[subject_sbar,
                                      subject_vp,
                                      app_measure]).

n([pl(onderdelen),sg(onderdeel)],het,[app_measure],
  [s(bedrijf)]).

n([pl(onderdelen),sg(onderdeel)],het,[],
  [h(auto),
   leger  % niet lege-ronder-delen
  ]).

n([sg(onderdruk)],de,[]).

n([pl(onderdrukkers),sg(onderdrukker)],de,[]).

n([pl(onderdrukkingen),sg(onderdrukking)],de,[sbar,vp]).

n([mass(onderduik)],de,[]).

n([pl(onderduikers),sg(onderduiker)],de,[]).

n([sg(onderfamilie),pl(onderfamilies)],de,[]).

n([mass(ondergang)],de,[]).

n([sg(ondergeschikte),pl(ondergeschikten)],de,[]).

n([sg(ondergetekende),pl(ondergetekenden)],de,[]).

n([mass(ondergoed)],het,[]).

n([pl(ondergrenzen),sg(ondergrens)],de,[]).

n([pl(ondergronden),sg(ondergrond)],de,[]).

n([mass(ondergrondse)],de,[]).

n([pl(onderhandelaars),pl(onderhandelaren),sg(onderhandelaar)],de,[],
  [h('FNV')]).

n([pl(onderhandelingen),sg(onderhandeling)],de,[pred_pp(in)],
  []).

n([pl(onderhandelingen),sg(onderhandeling)],de,[],
  [h(cao),h('CAO'),
   s(toetreding)]).

n([mass(onderhoud)],both,[pred_pp(in)]).  %VL de onderhoud

n([pl(onderjurken),sg(onderjurk)],de,[],[dim(onderjurkje)]).

n([pl(onderkaken),sg(onderkaak)],de,[]).

n([pl(onderkanten),sg(onderkant)],de,[]).

n([pl(onderkomens),sg(onderkomen)],het,[]).

n([pl(onderkoningen),sg(onderkoning)],de,[]).

%% not onder_krui_pers
n([pl(onderkruipers),sg(onderkruiper)],de,[]).

n([pl(onderlagen),sg(onderlaag)],de,[]).

n([pl(onderlichamen),sg(onderlichaam)],het,[]).

n([pl(onderlippen),sg(onderlip)],de,[]).

n([pl(ondermijningen),sg(ondermijning)],de,[]).

n([pl(ondernemers),sg(ondernemer)],de,[],
  [bouw,
   horeca,
   hout,
   transport]).

n([pl(ondernemingen),sg(onderneming)],de,[],
  [bouw,
   dochter,
   s(handel),
   hout,
   transport]).

n([mass(ondernemerschap)],het,[]).

n([pl(onderonsjes),sg(onderonsje)],het,[]).

n([sg(onderorde),pl(onderorden),pl(onderordes)],de,[app_measure]).

n([pl(onderpanden),sg(onderpand)],het,[]).

n([mass(onderricht)],het,[]).

n([mass(onderschatting)],de,[]).

n([mass(onderscheid)],het,[subject_sbar]).

n([pl(onderscheidingen),sg(onderscheiding)],de,[sbar]).

n([pl(onderschriften),sg(onderschrift)],het,[start_app_measure]).

n([mass(onderspit)],het,[]).

n([pl(onderstellen),sg(onderstel)],het,[]).

n([pl(onderstellingen),sg(onderstelling)],de,[sbar,vp]).

n([pl(ondersteuningen),sg(ondersteuning)],de,[sbar,vp]).

n([pl(onderstrepingen),sg(onderstreping)],de,[]).

n([pl(ondertekenaars),pl(ondertekenaren),sg(ondertekenaar)],de,[]).

n([pl(ondertekeningen),sg(ondertekening)],de,[]).

n([pl(ondertitels),sg(ondertitel)],de,
  [start_app_measure,
   app_measure,
   np_app_measure]).

n([mass(ondertiteling)],de,[]).

n([pl(ondertoezichtstellingen),sg(ondertoezichtstelling)],de,[]).

n([pl(ondertonen),sg(ondertoon)],de,[sbar,van_sbar],[dim(ondertoontje)]).

n([pl(ondervindingen),sg(ondervinding)],de,[sbar,vp]).

n([mass(ondervoeding)],de,[]).

n([pl(ondervragers),sg(ondervrager)],de,[]).

n([pl(ondervragingen),sg(ondervraging)],de,[]).

n([pl(onderwerpen),sg(onderwerp)],het,[sbar,app_measure],
  [s(gesprek)]).

n([pl(onderwerpingen),sg(onderwerping)],de,[]).

n([mass(onderwijs)],het,[],
  [basis,
   s(beroep),
   s(gemeenschap),
   geschiedenis,
   s(godsdienst),
   i(volwassen,volwassenen),
   kleuter,
   kunst,
   literatuur,
   omroep,
   praktijk,
   taal,
   vak,
   i(zin_opbouw,zinsopbouw), % niet zin_sop_bouw_onderwijs  
   zwem]).

n([pl(onderwijsgevenden),sg(onderwijsgevende)],de,[]).

n([mass(onderwijskunde)],de,[]).

n([pl(onderwijzers),sg(onderwijzer)],de,[],[hoofd]).

n([pl(onderwijzeressen),sg(onderwijzeres)],de,[],[hoofd]).

n([mass(onderworpenheid)],de,[]).

n([pl(onderzeeboten),sg(onderzeeboot)],de,[]).

n([pl(onderzeeërs),sg(onderzeeër)],de,[],
  [kern]).

n([pl(onderzoeken),sg(onderzoek)],het,
  [pred_pp(in),
   sbar  % Iedereen kent de onderzoeken dat 60 procent van de fusies mislukt .
  ]).

n([pl(onderzoeken),sg(onderzoek)],het,[],
  [s(bevolking),
   buurt,
   h('DNA'),h(dna),
   markt,
   moord,
   politie,h(politie),
   i(spoor,sporen),
   vervolg]).

n([pl(onderzoekers),sg(onderzoeker)],de,[],
  [bodem,
   hoofd,
   markt]).

n([pl(onderzoekingen),sg(onderzoeking)],de,[]).

n([pl(onderzoeksters),sg(onderzoekster)],de,[]).

n([pl(ondeugden),sg(ondeugd)],de,[]).

n([pl(onduidelijkheden),sg(onduidelijkheid)],de,[]).

n([sg([one,man,show]),pl([one,man,shows])],de,[]).

n([sg([one,way,screen]),pl([one,way,screens]),
   sg([oneway,screen]), pl([oneway,screens]),
   sg(['one-way',screen]), pl(['one-way',screens])],de,[]).

n([pl(oneffenheden),sg(oneffenheid)],de,[sbar]).

n([mass(oneindigheid)],de,[]).

n([sg(oneliner),pl(oneliners)],de,[]).

n([pl(onenigheden),sg(onenigheid)],de,[sbar]).

n([pl(onevenwichtigheden),sg(onevenwichtigheid)],de,[]).

n([mass(ongedierte)],het,[]).

n([mass(ongeduld)],het,[]).

n([mass(ongebondenheid)],de,[]).

n([pl(ongehoorzaamheden),sg(ongehoorzaamheid)],de,[sbar,vp]).

n([mass(ongelijk)],het,[]).

n([pl(ongelijkheden),sg(ongelijkheid)],de,[],
  [s(recht)]).

n([mass(ongeloof)],het,[sbar]).

% n([mass(ongelooflijke)],het,[]).

n([pl(ongelukken),sg(ongeluk)],het,[sbar,vp],[dim(ongelukje)]).

n([pl(ongelukken),sg(ongeluk)],het,[],
  [h(auto),auto,
   bus,
   trein,
   s(verkeer),
   vliegtuig]).

n([pl(ongemakken),sg(ongemak)],het,[sbar,vp]).

n([mass(ongenade)],de,[]).

n([pl(ongenoegens),sg(ongenoegen)],het,[sbar]).

n([pl(ongerechtigheden),sg(ongerechtigheid)],de,[sbar,vp]).

n([sg(ongerede)],het,[pred_pp(in)]).

n([sg(ongeregeldheid),
   pl(ongeregeldheden)],de,[]).

n([mass(ongerief)],het,[]).

n([mass(ongerustheid)],de,[]).

n([pl(ongevallen),sg(ongeval)],het,[],
  [s(verkeer),
   dim(ongevalletje)]).

n([pl(onhandigheden),sg(onhandigheid)],de,[vp]).

n([pl(onheilen),sg(onheil)],het,[]).

n([pl(onjuistheden),sg(onjuistheid)],de,[sbar,vp]).

n([pl(onkels),sg(onkel)],de,[],[dim(onkeltje)]).

n([pl(onkosten)],both,[]).

n([pl(onkruiden),sg(onkruid)],het,[]).

n([mass(onkunde)],de,[]).

n([pl(onlusten),sg(onlust)],de,[]).

n([mass(onmacht)],de,[vp]).

n([mass(onmin)],de,[pred_pp(in)]).

n([pl(onmogelijkheden),sg(onmogelijkheid)],de,[sbar,vp]).

n([pl(onnozelheden),sg(onnozelheid)],de,[sbar,vp]).

n([mass(onpartijdigheid)],de,[]).

n([mass(onraad)],het,[]).

n([mass(onrecht)],het,[]).

n([pl(onrechtvaardigheden),sg(onrechtvaardigheid)],de,[sbar,vp]).

n([pl(onregelmatigheden),sg(onregelmatigheid)],de,[]).

n([sg([onroerend,goed]),pl([onroerende,goederen])],het,[]).

n([pl(onrusten),sg(onrust)],de,[],[s(arbeid)]).

n([meas(ons)],both,[meas_mod,measure],
  [dim(onsje)]).

n([mass(onschendbaarheid)],de,[]).

n([mass(onschuld)],de,[]).

n([mass(onsterfelijkheid)],de,[]).

n([pl(ontberingen),sg(ontbering)],de,[sbar,vp]).

n([pl(ontbijten),sg(ontbijt)],het,[],
  [kerst,
   paas,
   dim(ontbijtje)]).

n([pl(ontbindingen),sg(ontbinding)],de,[]).

n([mass(ontbladering)],de,[]).

n([pl(ontboezemingen),sg(ontboezeming)],de,[sbar,vp]).

n([mass(ontbossing)],de,[]).

n([pl(ontdekkers),sg(ontdekker)],de,[]).

n([pl(ontdekkingen),sg(ontdekking)],de,[sbar,vp],[dim(ontdekkinkje)]).

n([pl(ontdekkingsreizen),sg(ontdekkingsreis)],de,[]).

n([pl(ontdekkingsreizigers),sg(ontdekkingsreiziger)],de,[]).

n([pl(ontduikingen),sg(ontduiking)],de,[],[belasting]).

n([pl(onteigeningen),sg(onteigening)],de,[]).

n([mass(onterving)],de,[]).

n([mass(ontevredenheid)],de,[sbar]).

n([pl(ontginningen),sg(ontginning)],de,[]).

n([pl(ontgoochelingen),sg(ontgoocheling)],de,[]).

n([sg(ontgroening),pl(ontgroeningen)],de,[]).

n([mass(onthaal)],both,[]).  % was 'het'

n([mass(onthaasting)],de,[]).

n([mass(ontij)],het,[]).

n([pl(ontheffingen),sg(ontheffing)],de,[vp]).

n([pl(onthoofdingen),sg(onthoofding)],de,[]).

n([pl(onthoudingen),sg(onthouding)],de,[]).

n([pl(onthullingen),sg(onthulling)],de,[sbar,vp]).

n([sg(ontkenner),pl(ontkenners)],de,[]).

n([pl(ontkenningen),sg(ontkenning)],de,[sbar]).

n([pl(ontknopingen),sg(ontknoping)],de,[sbar]).

n([mass(ontkoppeling)],de,[]).

n([pl(ontladingen),sg(ontlading)],de,[sbar,vp]).

n([pl(ontlastingen),sg(ontlasting)],de,[sbar,vp]).

n([pl(ontledingen),sg(ontleding)],de,[]).

n([sg(ontmanteling),pl(ontmantelingen)],de,[]).

n([pl(ontmaskeringen),sg(ontmaskering)],de,[]).

n([sg(ontmijning),pl(ontmijningen)],de,[]).

n([pl(ontmoetingen),sg(ontmoeting)],de,[],
  ['Davis-Cup',
   top]).

n([mass(ontnuchtering)],de,[]).

n([pl(ontploffingen),sg(ontploffing)],de,[],
  [gas,dim(ontploffinkje)]).

n([mass(ontplooiing)],de,[]).

n([mass(ontreddering)],de,[]).

n([pl(ontroeringen),sg(ontroering)],de,[]).

n([mass(ontrouw)],de,[]).

n([pl(ontruimingen),sg(ontruiming)],de,[]).

n([pl(ontslagen),sg(ontslag)],het,[],[deeltijd]).

n([pl(ontsluitingen),sg(ontsluiting)],de,[]).

n([pl(ontsnappingen),sg(ontsnapping)],de,[]).

n([pl(ontspanningen),sg(ontspanning)],de,[sbar,vp]).

n([pl(ontsporingen),sg(ontsporing)],de,[]).

n([pl(ontstekingen),sg(ontsteking)],de,[],
  [blaas,
   hersenvlies,
   keel,
   long,
   pees]).

n([mass(ontsteltenis)],de,[]).

n([mass(ontstentenis)],de,[]).

n([mass(ontucht)],de,[]).

n([pl(ontvangers),sg(ontvanger)],de,[],
  [s(bijstand),
   dim(ontvangertje)]).

n([pl(ontvangsten),sg(ontvangst)],both,[],[order]).

n([mass(ontvankelijkheid)],de,[]).

n([pl(ontvluchtingen),sg(ontvluchting)],de,[]).

n([pl(ontvoerders),sg(ontvoerder)],de,[]).

n([pl(ontvoeringen),sg(ontvoering)],de,[]).

n([sg(ontvoogding),pl(ontvoogdingen)],de,[]).  % en niet on_tv_oog_ding

n([mass(ontwaarding)],de,[]).

n([mass(ontwapening)],de,[]).

n([pl(ontwerpen),sg(ontwerp)],het,
  [sbar,
   vp,
   app_measure],
  [voor,
   i(initiatief_wet,initiatiefwets),
   i(initiatief_wet,'initiatief-wets'),
   s(wet)]).

n([pl(ontwerpen),sg(ontwerp)],het,[],
  [s(wet),
   woning]).

n([pl(ontwerpers),sg(ontwerper)],de,[],[mode]).

n([pl(ontwerpsters),sg(ontwerpster)],de,[]).

n([sg(ontwikkelaar),pl(ontwikkelaars)],de,[]).

n([pl(ontwikkelingen),sg(ontwikkeling)],de,
  [sbar,
   subject_sbar, % het is een nieuwe ontwikkeling, dat ...
   pred_pp(in)]).

n([pl(ontwikkelingen),sg(ontwikkeling)],de,[],
  [s(beleid),
   door,
   koers,
   loon,
   natuur,
   s(platteland),
   produkt,
   rook,
   winst]).

n([pl(ontwrichtingen),sg(ontwrichting)],de,[]).

n([mass(ontzag)],het,[]).

n([mass(ontzet)],het,[]).

n([pl(ontzettingen),sg(ontzetting)],de,[]).

n([mass(ontzuiling)],de,[]).

n([mass(onveiligheid)],de,[]).

n([mass(onveranderlijkheid)],de,[]).

n([mass(onverdraagzaamheid)],de,[]).

n([sg(onverhard)],het,[]). % VLaams? "Op het onverhard nam hij een grote voorsprong" "van het asfalt naar het onverhard"

n([pl(onvermijdelijkheden),sg(onvermijdelijkheid)],de,[sbar,vp]).

n([mass(onverschilligheid)],de,[]).

n([pl(onvoldoendes),sg(onvoldoende)],de,[]).

n([pl(onvolkomenheden),sg(onvolkomenheid)],de,[]).

n([pl(onvolmaaktheden),sg(onvolmaaktheid)],de,[]).

n([mass(onvrede)],de,[]).

n([mass(onvruchtbaarheid)],de,[]).

n([pl(onwaarheden),sg(onwaarheid)],de,[sbar,vp]).

n([pl(onweren),sg(onweer)],het,[],[dim(onweertje)]).

n([mass(onwetendheid)],de,[]).

n([mass(onwil)],de,[vp]).

n([pl(onzekerheden),sg(onzekerheid)],de,[sbar]).

n([pl(onzen),sg(onze)],both,[]).

n([sg(onzevader),pl(onzevaders)],het,[]).

n([mass(onzin)],de,[subject_sbar,subject_vp]).

n([sg(oo)],de,[]).

n([pl(ogen),sg(oog)],het,[sbar,vp],[dim(oogje)]).

n([pl(ogen),sg(oog)],het,[],
  [linker,
   rechter]).

n([pl(oogappels),sg(oogappel)],de,[]).

n([pl(ooggetuigen),sg(ooggetuige)],de,[]).

n([pl(oogharen),sg(ooghaar)],het,[],[dim(ooghaartje)]).

n([mass(ooghoogte)],de,[]).

n([pl(oogkassen),sg(oogkas)],de,[]).

n([pl(oogmerken),sg(oogmerk)],het,
  [vp,
   sbar,
   subject_vp,
   subject_sbar]).

n([sg(oogopslag)],de,[]).

n([pl(oogpunten),sg(oogpunt)],het,[vp]).

n([pl(oogsten),sg(oogst)],de,[measure]). % de nieuwe oogst films

n([mass(oogwenk)],de,[]).

n([pl(ooievaars),pl(ooievaren),sg(ooievaar)],de,[],[dim(ooievaartje)]).

n([pl(ooms),sg(oom)],de,[],
  [oud,
   suiker,
   dim(oompje)]).

n([pl(oren),sg(oor)],het,[],
  [dim(oortje),
   linker,
   rechter]).

n([pl(oorbellen),sg(oorbel)],de,[]).

n([pl(oorden),sg(oord)],het,[np_app_measure],
  [s(bedevaart),
   i(bejaard,bejaarden),
   s(herstelling),
   kuur,
   recreatie,
   rust,
   vakantie,
   woon]).

n([pl(oordelen),sg(oordeel)],het,
  [sbar,
   vp,
   subject_sbar,
   subject_vp],
  [i(deskundig,deskundigen),
   eind
  ]).

n([pl(oorkonden),pl(oorkondes),sg(oorkonde)],de,[]).

n([sg(oorkussen),pl(oorkussens)],het,[]).

n([pl(oorlellen),sg(oorlel)],de,[],[dim(oorlelletje)]).

%% ouderwets, "toestemming"
n([sg(oorlof)],both,[vp],[]).

n([pl(oorlogen),sg(oorlog),mass(oorlog)],de,[],
  [broeder,
   burger,
   cyber,
   grond,
   s(handel),
   h('Irak'),
   kern,
   h('Kosovo'),
   lucht,
   prijs,i(prijs,prijzen),
   supermarkt,
   taxi,
   wereld,
   dim(oorlogje)
  ]).

n([sg(oormerk),pl(oormerken)],het,[]).

n([pl(oorschelpen),sg(oorschelp)],de,[]).

n([pl(oorsprongen),sg(oorsprong)],de,[pred_pp(van)]).

n([mass(oorspronkelijkheid)],de,[]).

n([pl(oorvijgen),sg(oorvijg)],de,[]).

n([pl(oorzaken),sg(oorzaak)],de,[sbar,subject_sbar],
  [hoofd]).

n([pl(oorzaken),sg(oorzaak)],de,[],
  [s(dood)]).

n([mass(oost),
   mass('Oost')],both,[]).

n([sg('oost-west'),
   pl('oost-west')],both,[]).

n([mass(oostblok)],het,[]).

n([mass(oosten),
   mass('Oosten')
  ],het,[]).

n([pl(oosterlingen),sg(oosterling)],de,[]).

n([mass(oostkant)],de,[]).

n([pl('opa\'s'),sg(opa)],de,[],
  [dim(opaatje)]).

n([mass(opbloei)],de,[]).

%% alleen met "per"?
n([mass(opbod)],het,[]).

n([mass(opbouw)],de,[],
  [her,
   leeftijd,s(leeftijd),
   s(leven),
   pensioen]).

n([sg(opbouwer),pl(opbouwers)],de,[]).

n([pl(opbrengsten),sg(opbrengst)],de,[]).

n([sg(opdeling),pl(opdelingen)],de,[]).

n([sg(opdonder),pl(opdonders)],de,[]).

n([pl(opdrachten),sg(opdracht)],de,
  [sbar,vp,subject_sbar,subject_vp]).

n([pl(opdrachten),sg(opdracht)],de,[],
  [s(overheid),
   zoek]).

n([pl(opdrachtgevers),sg(opdrachtgever)],de,[]).

n([pl(opdrachtgeefsters),sg(opdrachtgeefster)],de,[]).

n([pl(opdrachtnemers),sg(opdrachtnemer)],de,[]).

n([mass(opdroging)],de,[]).

n([sg(opdruk),pl(opdrukken)],de,[]).

n([pl(opeenhopingen),sg(opeenhoping)],de,[]).

n([pl(opeenstapelingen),sg(opeenstapeling)],de,[]).

n([mass(opeenvolging),pl(opeenvolgingen),sg(opeenvolging)],de,[]).

n([sg([open,mind]),pl([open,minds])],de,[]).

n([mass([open,source])],both,[]).

n([mass(openbaar)],het,[]).

n([mass(openbaarheid)],de,[]).

n([pl(openbaarmakingen),sg(openbaarmaking)],de,[]).

n([pl(openbaringen),sg(openbaring)],de,[sbar]).

n([sg(opener),pl(openers)],de,[]).

n([mass(openhartigheid)],de,[]).

n([mass(openheid)],de,[]).

n([pl(openingen),sg(opening)],de,[],
  [her,
   mond,
   schaak,
   dim(openingetje)]).

n([mass(openlucht)],de,[]).

n([sg(openstelling),pl(openstellingen)],de,[]).

n([pl('opera\'s'),sg(opera)],de,[],[dim(operaatje)]).

n([pl(operateurs),sg(operateur)],de,[]).

n([pl(operatoren),pl(operators),sg(operator)],de,[]).

n([pl(operaties),sg(operatie)],de,[],
  [s(bezuiniging),
   hart,
   inkijk,
   kijk,
   knie,
   s(redding),
   steun,
   s(vrede)]).

n([sg(operator),pl(operators)],de,[]).

n([pl(operettes),sg(operette)],de,[]).

n([sg(opfok)],de,[]).

n([stem(opgave),
   pl(opgaven),sg(opgaaf),sg(opgave)],de,
  [sbar,vp]).

n([stem(opgave),
   pl(opgaven),sg(opgaaf),sg(opgave)],de,[],
  [jaar]).

n([pl(opgangen),sg(opgang)],de,[]).

n([mass(opgewektheid)],de,[]).

n([mass(opgewondenheid)],de,[]).

n([sg(opgooi)],de,[]).

n([pl(opgravingen),sg(opgraving)],de,[]).

n([pl(ophangingen),sg(ophanging)],de,[],[wiel]).

n([mass(ophef)],de,[]).

n([pl(opheffingen),sg(opheffing)],de,[]).

n([pl(ophelderingen),sg(opheldering)],de,[sbar,vp]).

n([pl(ophopingen),sg(ophoping)],de,[]).

n([pl(opinies),sg(opinie)],de,[sbar,vp]).

n([sg(opiniepeiler),pl(opiniepeilers)],de,[]).

n([pl(opiniepeilingen),sg(opiniepeiling)],de,[]).

n([mass(opium)],de,[]).

n([sg(opkikker),pl(opkikkers)],de,[]).

n([sg(opklaring),pl(opklaringen)],de,[]).

n([mass(opknap)],de,[]).

n([mass(opkomst)],de,[]).

n([mass(opkoop)],de,[]).

n([mass(opkuis)],de,[]).

n([mass(opkweek)],de,[]).

n([sg(oplader),pl(opladers)],de,[]).

n([pl(oplagen),pl(oplages),sg(oplage)],de,[]).

n([sg(oplawaai),pl(oplawaaien)],de,[]).

n([sg(opleg)],de,[]).

n([sg(oplegger),pl(opleggers)],de,[]).

n([sg(oplazer),pl(oplazers)],de,[]).

n([pl(opleidingen),sg(opleiding)],de,
  [pred_pp(in),
   app_measure]).

n([pl(opleidingen),sg(opleiding)],de,
  [app_measure],
  [acteer,
   bachelor,h(bachelor),
   s(beroep),
   h('HBO'),
   imam,
   jeugd,
   i(leraar,leraren),
   master,h(master),
   h('MBO'),
   rij,
   school,
   vak,
   vervolg,
   voltijd,
   voor,
   h('VWO'),
   h('WO')
   ]).

n([pl(oplettendheden),sg(oplettendheid)],de,[]).

n([pl(opleveringen),sg(oplevering)],de,[]).

n([mass(opleving),pl(oplevingen),sg(opleving)],de,[],[her]).

n([pl(oplichters),sg(oplichter)],de,[]).

n([mass(oplichterij)],de,[]).

n([mass(oplichting)],de,[]).

n([sg(oploop),pl(oplopen)],de,[]).

n([pl(oplossingen),sg(oplossing)],de,
  [sbar,
   vp,
   subject_vp,
   subject_sbar]).

n([pl(oplossingen),sg(oplossing)],de,[],
  [internet,
   zoutzuur]).

n([mass(opluchting)],de,[subject_sbar,subject_vp,sbar]).

n([mass(opluistering)],de,[]).

n([pl(opmaken),sg(opmaak)],de,[]).

n([sg(opmaat),pl(opmaten)],de,[]).

n([pl(opmarsen),sg(opmars)],de,[]).

n([pl(opmerkingen),sg(opmerking)],de,[sbar,
				      vp,
				      van_sbar,
				      start_app_measure]).

n([pl(opnamen),pl(opnames),sg(opname)],de,[],
  [s(geluid),			% niet geluid-sop-namen
   plaat,
   h(video)]).

n([pl(opnemingen),sg(opneming)],de,[]).

n([pl(opoes),sg(opoe)],de,[],[dim(opoetje)]).

n([pl(opofferingen),sg(opoffering)],de,[vp]).

n([mass(oponthoud)],het,[]).

n([pl(oppassen),sg(oppas)],de,[]).

n([pl(oppassers),sg(oppasser)],de,[]).

n([mass(opperbevel)],het,[]).

n([sg(opper),pl(oppers)],de,[]).

n([pl(opperbevelhebbers),sg(opperbevelhebber)],de,[]).

n([pl(opperhoofden),sg(opperhoofd)],het,[]).

n([pl(oppervlakken),sg(oppervlak)],het,[],[contact]).

n([pl(oppervlakkigheden),sg(oppervlakkigheid)],de,[]).

n([pl(oppervlakten),pl(oppervlaktes),sg(oppervlakte)],both,[measure]).

n([pl(oppervlakten),pl(oppervlaktes),sg(oppervlakte)],both,[],[land]).

n([pl(opperwachtmeesters),sg(opperwachtmeester)],de,[]).

n([pl(opponenten),sg(opponent)],de,[]).

n([pl(opponentes),sg(opponente)],de,[]).

n([mass(opportunisme)],het,[]).

n([pl(opposities),sg(oppositie)],de,[]).

n([mass(oprechtheid)],de,[]).

n([pl(oprichters),sg(oprichter)],de,[],
  [mede,h(mede)]).

n([pl(oprichtingen),sg(oprichting)],de,[]).

n([pl(oprijlanen),sg(oprijlaan)],de,[]).

n([pl(oprispingen),sg(oprisping)],de,[sbar,vp]).

n([pl(opritten),sg(oprit)],de,[]).

n([pl(oproepen),sg(oproep)],de,[vp,sbar]).

n([pl(oproepingen),sg(oproeping)],de,[]).

n([pl(oproeren),sg(oproer)],both,[],[s(volk)]).  % en niet volk-sop-roer

n([sg(opruiing),pl(opruiingen)],de,[]).

n([pl(opruimingen),sg(opruiming)],de,[]).

n([pl(opscheppers),sg(opschepper)],de,[]).

n([pl(opschortingen),sg(opschorting)],de,[]).

n([pl(opschriften),sg(opschrift)],het,
  [start_app_measure,
   app_measure,
   np_app_measure
  ]).

n([pl(opschuddingen),sg(opschudding)],de,[]).

n([pl(opslagen),sg(opslag)],de,[],
  [gas,
   tank,
   vuurwerk]).

n([mass(opsluiting)],de,[]).

n([mass(opsmuk)],de,[]).

n([pl(opsommingen),sg(opsomming)],de,[]).

n([sg(opsplitsing),pl(opsplitsingen)],de,[]).

n([pl(opsporingen),sg(opsporing)],de,[]).

n([mass(opspraak)],de,[pred_pp(in)]).

n([sg(opsprong)],de,[]).

n([pl(opstallen),sg(opstal)],de,[]).

n([pl(opstanden),sg(opstand)],de,[],
  [s(volk)]).

n([pl(opstandelingen),sg(opstandeling)],de,[]).

n([mass(opstandigheid)],de,[]).

n([mass(opstanding)],de,[]).

n([pl(opstappen),sg(opstap)],de,[],[dim(opstapje)]).

n([sg(opstart)],de,[]).

n([pl(opstekers),sg(opsteker)],de,[subject_sbar,sbar]).

n([pl(opstellen),sg(opstel)],het,[]).

n([pl(opstellers),sg(opsteller)],de,[]).

n([pl(opstellingen),sg(opstelling)],de,[sbar,vp]).

n([sg(opstoot),pl(opstoten)],de,[],[dim(opstootje)]).

n([sg(opstopping),pl(opstoppingen)],de,[]).

n([sg(opstreek)],de,[]). % viool?

n([sg('opt-out')],both,[]).

n([pl(optelsommen),sg(optelsom)],de,[]).

n([sg(opticien),pl(opticiens)],de,[]).

n([pl(opties),sg(optie)],de,
  [subject_sbar,
   subject_vp,
   vp,
   sbar,
   app_measure]).

n([pl(opties),sg(optie)],de,[],
  [s(personeel)]).

n([mass(optiek)],both,[]).

n([sg(optimalisatie),pl(optimalisaties)],de,[]).

n([mass(optimisme)],het,[]).

n([pl(optimisten),sg(optimist)],de,[]).

n([pl(optochten),sg(optocht)],de,[]).

n([pl(optredens),sg(optreden)],het,[],
  [gast,
   h(live)]).

n([pl(opussen),sg(opus)],het,[]).

n([sg([opus,magnum])],het,[]).

n([sg(opvaart)],de,[]).

n([sg(opvang)],de,[],
  [i(kind,kinder),
   nacht,
   nood]).			% de eerste opvang, hence countable

n([sg([opvang,asielzoekers])],de,[]).

n([pl(opvanghuizen),sg(opvanghuis)],het,[]).

n([pl(opvarenden),sg(opvarende)],de,[]).

n([pl(opvattingen),sg(opvatting)],de,
  [sbar,
   subject_sbar,
   subject_vp,
   vp],
 [taak]).

n([sg(opvlucht)],de,[]).

n([pl(opvoeders),sg(opvoeder)],de,[]).

n([pl(opvoedsters),sg(opvoedster)],de,[]).

n([pl(opvoedingen),sg(opvoeding)],de,[]).

n([pl(opvoeringen),sg(opvoering)],de,[]).

n([pl(opvolgers),sg(opvolger)],de,[],[troon]).

n([pl(opvolgingen),sg(opvolging)],de,[],[troon]).

n([pl(opvolgsters),sg(opvolgster)],de,[],[troon]).

n([mass(opwaardering),pl(opwaarderingen)],de,[]).

n([mass(opwachting)],de,[]).

n([mass(opwarming)],de,[]).

n([pl(opwekkingen),sg(opwekking)],de,[]).

n([pl(opwellingen),sg(opwelling)],de,[sbar,vp]).

n([mass(opwerking)],de,[]).

n([mass(opwinding),pl(opwindingen),sg(opwinding)],de,[]).

n([pl(opzeggingen),sg(opzegging)],de,[]).

n([pl(opzetten),sg(opzet)],both,[sbar,vp,subject_sbar,subject_vp]).

n([sg(opzeg)],de,[]).

n([pl(opzichten),sg(opzicht)],het,[]).

n([pl(opzichters),sg(opzichter)],de,[],[dim(opzichtertje)]).

n([mass(opzien)],het,[]).

n([sg(or)],de,[]). % Ondernemingsraad

n([pl(orakelen),pl(orakels),sg(orakel)],het,[sbar]).

n([sg(oranje),pl(oranjes)],het,[]).

n([pl(oranjeries),pl(oranjerieën),sg(oranjerie)],de,[]).

n([sg(oratie),pl(oraties)],de,[]).  % wanneer?

n([pl(orchideeën),sg(orchidee)],de,[]).

n([pl(orden),pl(ordes),sg(orde)],de,
  [pred_pp(aan,subject_sbar),
   pred_pp(aan),
   pred_pp(van),
   pred_pp(op),
   app_measure]).

n([pl(orden),pl(ordes),sg(orde)],de,[],
  [kerk,
   klooster,
   pik,
   slagorde,
   wereld]).

n([pl(ordeningen),sg(ordening)],de,[],[markt]).

n([pl(orders),sg(order)],both,[measure,sbar,vp],[dim(ordertje)]).

n([sg(ordinantie),pl(ordinanties)],de,[]).

n([pl(ordonnansen),sg(ordonnans)],de,[]).

n([pl(ordonnanties),pl(ordonnantiën),sg(ordonnantie)],de,[sbar,vp]).

n([pl(organen),sg(orgaan)],het,[],
  [advies,
   s(bestuur),
   s(geslacht),
   s(overheid),
   overleg,h(overleg),
   pers,
   s(staat),
   s(uitvoering),
   dim(orgaantje)]).

n([pl(organisaties),sg(organisatie),
   pl(organizaties),sg(organizatie)],de,
  [np_app_measure],  % de organisatie de Internationale Socialisten
  [s(arbeid),
   f([autosport]),
   s(bedrijf),
   i(belang,belangen),
   [betaald,voetbal],
   i(boer,boeren),
   branche,h(branche),
   i(consument,consumenten),
   s(handel),
   hulp,
   s(huurder),
   jeugd,
   i(jong,jongeren),
   koepel,
   landbouw,
   lijn,
   milieu,h(milieu),
   i(mens_recht,mensenrechten),
   moslim,
   omroep,
   s(ontwikkeling),
   i(oud,ouderen),  % niet oud_ren_organisaties
   s(overheid),
   proces,
   project,
   reis,
   ruimtevaart,
   staf,
   terreur,
   vak,
   s(verzet),
   i(vluchteling,vluchtelingen),
   i('VN_vluchteling','VN-vluchtelingen'),
   h('VN'),
   i(volk,volkeren),
   vrijhandel,
   s(vrijwilliger),
   i(vrouw,vrouwen),
   i(werkgever,'werkgevers-'),
   s(werkgever),
   s(werknemer)]).

n([pl('organisatie-strukturen'),sg('organisatie-struktuur')],de,[]).

n([pl(organisatiemodellen),sg(organisatiemodel)],het,[]).

n([pl(organisatoren),pl(organisators),sg(organisator)],de,[],[reis]).

n([pl(organismen),pl(organismes),sg(organisme)],het,[]).

n([pl(organisten),sg(organist)],de,[]).

n([pl(organizatoren),pl(organizators),sg(organizator)],de,[]).

n([sg(organogram),pl(organogrammen)],het,[]).

n([pl(orgasmen),pl(orgasmes),sg(orgasme)],het,[]).

n([pl(orgels),sg(orgel)],het,[],
  [hammond,
   dim(orgeltje)]).

n([pl(orgies),pl(orgieën),pl(orgiën),sg(orgie)],de,[]).

n([mass(originaliteit),
   mass(orginaliteit)],de,[]).

n([mass(origine)],de,[pred_pp(van)]).

n([pl(originelen),sg(origineel),
   pl(orginelen),sg(orgineel)],het,[]).

n([pl(oriëntaties),sg(oriëntatie)],de,[],[her]).

n([pl(orkanen),sg(orkaan)],de,[]).

n([pl(orkesten),sg(orkest)],het,[],
  [s(begeleiding),
   harmonie,
   kamer,
   omroep,
   strijk]).

n([pl(ornamenten),sg(ornament)],het,[]).

n([sg(ornitholoog),pl(ornithologen)],de,[]).

n([sg(orthodox),sg(ortodox),
   pl(orthodoxen),pl(ortodoxen)],de,[]).

n([mass(orthodoxie),mass(ortodoxie)],de,[]).

n([sg(oscar),pl(oscars)],de,[]).

n([pl(ossen),sg(os)],de,[]).

n([pl(ossekoppen),sg(ossekop)],de,[]).

n([sg(otter),pl(otters)],de,[]).

n([sg([oud,en,nieuw]),
   sg(['Oud',en,'Nieuw']),
   sg([oud,'&',nieuw])],
  both,[]).

n([sg([oud,op,nieuw])],both,[]).

n([mass([oud,zeer])],het,[]).

n([pl(ouders),sg(ouder)],de,[],
  [adoptie,
   pleeg,
   oer,
   overblijf,
   schoon]).

n([mass(ouderdom)],de,[]).

n([pl(ouderlingen),sg(ouderling)],de,[]).

n([mass(ouderschap)],het,[]).

n([sg(oudgediende),pl(oudgedienden)],de,[]).

n([mass('Oudgrieks')],het,[]).

n([pl(oudheden),sg(oudheid)],de,[]).

n([sg(oudjaar),sg(oudejaar)],het,[temp_mod,sbar]).

n([stem(oud_DIM),pl(oudjes),sg(oudje)],het,[]).

n([sg(outfit),pl(outfits)],de,[]).

n([sg(outlier),pl(outliers)],de,[]).

n([mass(output)],de,[]).

n([sg(outsider),pl(outsiders)],de,[]).

n([pl(ouverturen),pl(ouvertures),sg(ouverture)],de,[]).

n([sg(ouwe),pl(ouwen),pl(ouwes)],de,[]).

n([sg(ouwehoer),pl(ouwehoeren)],de,[]).

n([pl(ouwetjes),sg(ouwetje)],het,[]).

%% de ov: ov-kaart
%% het ov: openbaar vervoer
n([sg(ov)],both,[]).

n([sg(ovaal),pl(ovalen)],both,[]).

n([pl(ovaties),sg(ovatie)],de,[]).

n([pl(ovens),sg(oven)],de,[],
  [s(verbranding),
   dim(oventje)]).

n([%sg(over),  % only introduces errors
   pl(overs)],de,[]).

n([pl(overalls),sg(overall)],de,[]).

n([mass(overbelasting)],de,[]).

n([mass(overbevissing)],de,[]).

n([mass(overbevolking)],de,[]).

n([mass(overblijf)],de,[]).

n([pl(overblijfselen),pl(overblijfsels),sg(overblijfsel)],het,[],[dim(overblijfseltje)]).

n([pl(overbrengingen),sg(overbrenging)],de,[]).

n([pl(overbruggingen),sg(overbrugging)],de,[]).

n([mass(overcapaciteit)],de,[]).

n([mass(overdaad)],de,[]).

n([pl(overdoses),pl(overdosissen),sg(overdosis)],de,[measure]).

n([pl(overdrachten),sg(overdracht)],de,[],
  [kennis,
   s(macht)]).

n([pl(overdrijvingen),sg(overdrijving)],de,[sbar,vp,subject_sbar]).

n([sg(overdruk),pl(overdrukken)],de,[]).

n([pl(overeenkomsten),sg(overeenkomst)],de,
  [sbar,
   vp]).

n([pl(overeenkomsten),sg(overeenkomst)],de,
  [],
  [s(arbeid),
   associatie,
   beheers,h(beheers),
   s(handel),
   huur,
   partnerschap,
   s(samenwerking),
   visserij,
   s(vrijhandel)
  ]).

n([mass(overeenstemming),pl(overeenstemmingen),sg(overeenstemming)],de,
  [pred_pp(in),
   pred_pp(in,subject_sbar),
   sbar]).

n([pl(overgangen),sg(overgang)],de,
  [subject_sbar,
   subject_vp]).

n([pl(overgangen),sg(overgang)],de,[],
  [spoorweg]).

n([mass(overgave)],de,[]).

n([mass(overgevoeligheid),pl(overgevoeligheden),sg(overgevoeligheid)],de,[]).

n([pl(overgordijnen),sg(overgordijn)],het,[]).

n([mass(overgroei)],de,[]).

n([pl(overgrootmoeders),sg(overgrootmoeder)],de,[]).

n([pl(overgrootouders),sg(overgrootouder)],de,[]).

n([pl(overgrootvaders),sg(overgrootvader)],de,[]).

n([sg(overhaal)],de,[]).

n([mass(overhand)],de,[]).

n([sg(overhang)],de,[]).

n([sg(overheerser),pl(overheersers)],de,[]).

n([pl(overheersingen),sg(overheersing)],de,[]).

n([pl(overheden),sg(overheid)],de,[],[s(rijk)]).

n([pl(overhemden),sg(overhemd)],het,[]).

n([sg(overheveling),pl(overhevelingen)],de,[]).

n([sg(overhoring),pl(overhoringen)],de,[app_measure]).

n([pl(overjassen),sg(overjas)],de,[]).

n([mass(overkant)],de,[]).

n([pl(overkappingen),sg(overkapping)],de,[]).

n([sg(overlaat),pl(overlaten)],de,[]).

n([mass(overlap)],de,[]).

n([pl(overlappingen),sg(overlapping)],de,[]).

n([mass(overlast)],de,[],
  [drug,s(drug),
   s(geluid),
   stank,
   water]).

n([pl(overledenen),sg(overledene)],de,[]).

n([mass(overleg)],het,[],
  [h('CAO'),
   s(najaar),
   spoed,
   top,
   voor,
   s(voorjaar),
   s(vrede)]).

n([pl(overleggingen),sg(overlegging)],de,[]).

n([pl(overlevers),sg(overlever)],de,[]).

n([pl(overleveringen),sg(overlevering)],de,[]).

n([mass(overleving)],de,[]).

n([stem(overlijden),pl(overlijdens)],het,[]).

n([pl(overlopen),sg(overloop)],de,[]).

n([pl(overlopers),sg(overloper)],de,[]).

n([mass(overmaat)],de,[]).

n([mass(overmacht)],de,[]).

n([sg(overman)],de,[]). % In 1622 werd hij overman van de Voetboogdoelen .

n([mass(overmoed)],de,[]).

n([pl(overnachtingen),sg(overnachting)],de,[]).

n([pl(overnames),pl(overnamen),sg(overname)],de,[],
  [s(macht)]).

n([sg(overnamesom),pl(overnamesommen)],de,[]).

n([pl(overnemingen),sg(overneming)],de,[]).

n([pl(overpeinzingen),sg(overpeinzing)],de,[sbar,vp]).

n([pl(overplaatsingen),sg(overplaatsing)],de,[]).

n([mass(overreding)],de,[]).

n([mass(overschatting)],de,[],
  [zelf]).

n([sg(overschot)],de,[]).  % VL

n([pl(overschotten),sg(overschot)],het,[],
  [dim(overschotje),
   s(begroting),
   s(handel),
   mest]).

n([pl(overschrijdingen),sg(overschrijding)],de,[]).

n([pl(overslagen),sg(overslag)],de,[]).

n([mass(overspel)],het,[]).

n([mass(overstaan)],het,[]).

n([pl(overstappen),sg(overstap)],de,[]).

n([pl(oversten),sg(overste)],de,[]).

n([pl(oversteken),sg(oversteek)],de,[]).

n([mass(overstort),pl(overstorten)],de,[]).

n([pl(overstromingen),sg(overstroming)],de,[]).

n([pl(overtochten),sg(overtocht)],de,[]).

n([pl(overtreders),sg(overtreder)],de,[]).

n([pl(overtredingen),sg(overtreding)],de,[sbar,vp]).

n([sg(overtrek)],de,[]).

n([pl(overtuigingen),sg(overtuiging)],de,[sbar,vp,subject_sbar,
					  pred_pp(uit,subject_sbar)]).

n([pl(overuren),sg(overuur)],het,[]).

n([mass(overvaart)],de,[]).

n([pl(overvallen),sg(overval)],de,[],
 [roof]).

n([pl(overvallers),sg(overvaller)],de,[]).

n([mass(oververhitting)],de,[]).

n([mass(overvloed),pl(overvloeden),sg(overvloed)],de,[]).

n([pl(overwegen),sg(overweg)],de,[]).

n([pl(overwegingen),sg(overweging)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp]).

n([pl(overwegingen),sg(overweging)],de,[],
  [her,
   s(veiligheid)]).

n([mass(overwicht)],het,[],[veld]).

n([pl(overwinnaars),pl(overwinnaren),sg(overwinnaar)],de,[]).

n([pl(overwinningen),sg(overwinning)],de,
  [sbar,
   vp,
   subject_sbar
  ]).

n([pl(overwinningen),sg(overwinning)],de,[],
  [eind,
   s(verkiezing)]).

%% otherwise Alpino guesses "[overwin-tering-s-gebied]"
n([sg(overwintering),pl(overwinteringen)],de,[]).

n([sg(overzet),pl(overzetten)],de,[]).

n([pl(overzichten),sg(overzicht)],het,[],
  [jaar,
   i(land,landen)]).

n([pl(oxydaties),sg(oxydatie)],de,[]).

n([sg(ozb)],de,[]).

n([mass(ozon)],both,[]).

n([pl('p\'s'),sg(p)],de,[],[dim('p\'tje')]).

n([pl('pa\'s'),sg(pa)],de,[],[dim(paatje)]).

n([sg(paai)],de,[]).

n([pl(palen),sg(paal)],de,[],
  [dim(paaltje),
   flits,
   grens,
   kilometer,
   laad,
   piket,
   praat,
   telefoon,
   totem]).

n([meas(paar),pl(paren)],het,[meas_mod,measure]).

n([sg(paar),pl(paren),
   ignore_stem(paar)],het,[],
  [homo,
   jeugd,
   s(koning),
   ouder
  ]).

n([pl(paarden),sg(paard)],het,[],
  [hobbel,
   parade,
   dim(paardje)]).

n([sg('paard-en-wagen'),
   pl('paard-en-wagens'),
   stem(paard_en_wagen)],both,[]).

n([stem(paardenbloem),
   sg(paardenbloem),pl(paardenbloemen),
   pl(paardebloemen),sg(paardebloem)],de,[]).

n([stem(paard_hoef),
   sg(paardenhoef),pl(paardenhoeven),
   pl(paardehoeven),sg(paardehoef)],de,[]).

n([stem(paard_kop),
   sg(paardenkop),pl(paardenkoppen),
   pl(paardekoppen),sg(paardekop)],de,[]).

n([stem(paardenkracht),
   mass(paardenkracht),pl(paardenkrachten),mass(paardekracht),pl(paardekrachten)],de,[]).

n([stem(paard_staart),
   sg(paardenstaart),pl(paardenstaarten),
   pl(paardestaarten),sg(paardestaart)],de,[]).

n([stem(paard_stal),
   sg(paardenstal),pl(paardenstallen),
   pl(paardestallen),sg(paardestal)],de,[]).

n([mass(paars)],het,[]).

n([pl(paartjes),sg(paartje)],het,[measure]).

n([pl(paaseieren),sg(paasei)],het,[]).

n([sg(pacemaker),pl(pacemakers)],de,[]).

n([pl(pachten),sg(pacht)],de,[],[]).

n([pl(pachters),sg(pachter)],de,[],[dim(pachtertje)]).

n([pl(pacificaties),pl(pacificatiën),sg(pacificatie)],de,[]).

n([pl(pacifikaties),pl(pacifikatiën),sg(pacifikatie)],de,[]).

n([pl(pacten),sg(pact),
   sg(pakt)],het,[sbar,vp],[]).

n([pl(pacten),sg(pact),
   sg(pakt)],het,[],
  [groei,
   s(stabiliteit)]).

n([pl(padden),sg(pad)],de,[],
  [dim(padje)]).

n([pl(paden),sg(pad)],het,[pred_pp(op)],
  [overname]).

n([pl(paden),sg(pad)],het,[],
  [bos,
   grind,
   zand,
   zebra,
   dim(paadje)]).

n([stem(paddenstoel),
   pl(paddestoelen),sg(paddestoel)],de,[],[dim(paddestoeltje)]).

n([stem(paddenstoel),
   pl(paddenstoelen),sg(paddenstoel)],de,[],[dim(paddenstoeltje)]).

n([pl(padvinders),sg(padvinder)],de,[]).

n([mass(paella)],de,[]).

n([sg(pageturner),pl(pageturners)],de,[]).

% vele pagina's tekst
n([pl('pagina\'s'),sg(pagina)],de,[measure],
  [dim(paginaatje)]).

n([pl('pagina\'s'),sg(pagina)],de,[],
  ['Facebook',
   internet,
   opinie,
   overleg,
   start,
   web,
   dim(paginaatje)]).

%% VL: een bak punten
n([pl(pakken),sg(pak)],het,[measure],[dim(pakje)]).

n([mass([pak,slaag])],het,[]).

n([pl(pakhuizen),sg(pakhuis)],het,[]).

n([pl(pakketten),sg(pakket)],het,
  [measure],
  [dim(pakketje)]).

n([pl(pakketten),sg(pakket)],het,
  [],
  [i(aandeel,aandelen),
   basis,
   i(basis_taak,basistaken),
   i(bloem,bloemen),
   kerst,
   steun,  % voor Griekenland
   i(taak,taken),
   i(basis_taak,basistaken),
   ziekenfonds,
   dim(pakketje)]).

n([mass([pakkie,an])],het,[subject_sbar,subject_vp]).

n([pl(paleizen),sg(paleis)],het,[],
  [justitie,
   sport]).

n([mass('PPS')],de,[]).		% publiek-private samenwerking

n([sg('Palestijn'),pl('Palestijnen')],de,[]).

n([sg(paleontologie)],de,[]).

n([sg(paleontoloog),pl(paleontologen)],de,[]).

n([pl(paletten),sg(palet)],both,[]).

n([sg(palindroom),pl(palindromen)],both,[app_measure]).

n([pl(palingen),sg(paling)],de,[],[dim(palinkje)]).

n([pl(pallets), pl(palletten),sg(pallet)],both,[]).

n([pl(palmen),sg(palm)],de,[],[dim(palmpje)]).

n([sg(palmares),pl(palmaressen)],both,[]).

n([pl(palmbomen),sg(palmboom)],de,[]).

n([pl(pamfletten),sg(pamflet)],het,[],[dim(pamfletje)]).

n([pl(pannen),sg(pan)],de,[measure,pred_pp_pl(onder)],
  [dim(pannetje)]).				      % een pan soep
n([pl(pannen),sg(pan)],de,[],
  [stoom,
   dim(pannetje)]). 

n([sg([panchen,lama])],de,[]).  % boeddhisme...

n([pl(panden),sg(pand)],het,[np_app_measure],
  [s(bedrijf),
   drug,s(drug),
   kraak]).

n([pl('panda\'s'),sg(panda)],de,[]).

n([sg(pandemie),pl(pandemieën)],de,[],[corona]).

n([pl(panelen),sg(paneel)],het,[],
  [i(zon,zonne),
   i(zon,zonnen),
   dim(paneeltje)]).

n([pl(panels),sg(panel)],het,[]).

n([mass(paniek)],de,[pred_pp(in)]).

n([pl(pannes),sg(panne)],de,[sbar,vp]).

n([sg(pannenkoek),pl(pannenkoeken),pl(pannekoeken),sg(pannekoek)],de,
  [],
  [dim(pannenkoekje),dim(pannekoekje)]).

n([pl('panorama\'s'),sg(panorama)],het,[]).

n([pl(pantalons),sg(pantalon)],de,[],[dim(pantalonnetje)]).

n([mass(panteon)],het,[]).

n([pl(panters),sg(panter)],de,[],[dim(pantertje)]).

n([mass(pantheon)],het,[]).

n([pl(pantoffels),sg(pantoffel)],de,[],[dim(pantoffeltje)]).

n([pl(pantsers),sg(pantser)],het,[],[dim(pantsertje)]).

n([pl(panties),pl('panty\'s'),sg(panty)],de,[]).

n([pl(pappen),sg(pap)],de,[],[dim(papje)]).

n([pl('papa\'s'),sg(papa)],de,[],[dim(papaatje)]).

n([pl('pappa\'s'),sg(pappa)],de,[],[dim(pappaatje)]).

n([pl(paparazzi)],de,[]).

n([pl(papavers),sg(papaver)],de,[]).

n([pl(papegaaien),sg(papegaai)],de,[],[dim(papegaaitje)]).

n([pl(papers),sg(paper)],both,[]).

%% en niet paper-assen
n([pl(paperassen)],both,[]). 

n([sg(paperback),pl(paperbacks)],de,[]).

n([pl(papieren),sg(papier)],het,[],
  [s(identiteit),
   keuken,
   i(krant,kranten),
   i(krant,krante),
   pak,
   schuld,
   toilet,
   vracht,
   waarde,
   h('WC'),
   dim(papiertje)
  ]).

n([sg(papil),pl(papillen)],de,[]).

n([mass([pappen,en,nathouden])],het,[]).

n([pl('paprika\'s'),sg(paprika)],de,[]).

n([mass(paps)],de,[]).

n([mass(par)],de,[]).

n([pl('para\'s'),sg(para)],de,[]).

n([pl(parabelen),pl(parabels),sg(parabel)],de,[]).

n([pl(parachutes),sg(parachute)],de,[]).

n([pl(parachutisten),sg(parachutist)],de,[]).

n([pl(parades),sg(parade)],de,[],[hit]).

n([pl('paradigma\'s'),pl(paradigmata),sg(paradigma)],het,[sbar]).

n([pl(paradijzen),sg(paradijs)],het,[],[belasting]).

n([pl(paradoksen),sg(paradoks)],de,[sbar]).

n([pl(paradoxen),sg(paradox)],de,[sbar]).

n([pl(parafernalia)],de,[]).

n([pl(parafrasen),pl(parafrases),sg(parafrase)],de,[sbar]).

n([pl(paragrafen),sg(paragraaf)],de,[]).

n([sg(§)],de,[]).

n([pl(parallellen),sg(parallel)],de,[]).

n([sg(parallellogram),pl(parallellogrammen)],de,[]).

n([sg(paramilitair),pl(paramilitairen)],de,[]).

n([pl(parameters),sg(parameter)],de,[]).

n([mass(paranoia)],de,[]).

n([pl('paraplu\'s'),sg(paraplu)],de,[sbar],[dim(parapluutje)]).

n([mass(parapsychologie)],de,[]).

n([pl(parasieten),sg(parasiet)],de,[app_measure]).

n([pl(parasols),sg(parasol)],de,[],[dim(parasolletje)]).

n([mass(pardon)],het,[]).

n([pl(parelen),pl(parels),sg(parel)],de,[],[dim(pareltje)]).

n([mass(parelmoer)],both,[]).

n([pl(parfums),sg(parfum)],both,[]).

n([sg(parfumerie),pl(parfumeries)],de,[]).

n([pl(paringen),sg(paring)],de,[]).

n([sg(pariteit),pl(pariteiten)],de,[]).

n([pl(parken),sg(park)],het,[np_app_measure],
  [attractie,
   i(beeld,beelden),
   bungalow,
   container,
   i(dier,dieren),
   distri, % tja
   natuur,
   pret,
   speel,
   sport,
   i(sprook_DIM,sprookjes),
   thema,
   wagen,
   wild]).

n([pl(parketten),sg(parket)],het,[]).

n([sg('parket-generaal'),sg([parket,generaal])],het,[]).

n([pl(parketvloeren),sg(parketvloer)],de,[]).

n([pl(parkieten),sg(parkiet)],de,[],[dim(parkietje)]).

n([sg(parking),pl(parkings)],de,[]).

%% beide schrijfwijzen zijn correct
n([pl(parkoersen),sg(parkoers)],het,[]).

n([sg(parcours),pl(parcoursen)],het,[]).

n([pl(parlementen),sg(parlement)],het,[],
  [euro,
   school]).

n([sg('parlement-in-ballingschap'),
   sg([parlement,in,ballingschap])],het,[]).

n([pl(parlementariërs),sg(parlementariër)],de,[],
  [euro,'Euro']).

n([pl(parochianen),sg(parochiaan)],de,[],[dim(parochiaantje)]).

n([pl(parochies),pl(parochiën),sg(parochie)],de,[],[dim(parochietje)]).

n([pl(parodies),pl(parodieën),sg(parodie)],de,[]).

n([pl(parolen),sg(parool)],het,[sbar]).

n([pl(parten),sg(part)],de,[]).

n([pl(parten),sg(part)],het,[measure],[dim(partje)]).

n([pl(parterres),sg(parterre)],both,[]).  % celex het

n([pl(participanten),sg(participant)],de,[]).

n([pl(participaties),sg(participatie)],de,[],[s(arbeid)]).

n([pl(particulieren),sg(particulier),
   pl(partikulieren),sg(partikulier)],de,[]).

%% wedstrijd
%% een partij(tje) sjoelen

n([sg(partij),pl(partijen)],de,
  [measure,
   temp_mod,
   sbar
  ],
  [s(groep),
   s(training),
   schaak,
   dim(partijtje)]).

%% hoeveelheid
%% een partij bureaustoelen

n([sg(partij),pl(partijen),
   ignore(m(partij,noun(de,count,sg),partij)),
   ignore(m(partij,noun(de,count,sg,measure),partij)),
   ignore(m(partij,noun(de,count,pl),partijen)),
   ignore(m(partij,noun(de,count,pl,measure),partijen))
  ],de,
  [measure],
  [dim(partijtje)]).

%% groepering

n([sg(partij),pl(partijen),
   ignore(m(partij,noun(de,count,sg),partij)),
   ignore(m(partij,noun(de,count,sg,measure),partij)),
   ignore(m(partij,noun(de,count,pl),partijen)),
   ignore(m(partij,noun(de,count,pl,measure),partijen))
  ],de,[np_app_measure],
  [s(arbeider),
   coalitie,
   college,
   markt,
   s(meerderheid),
   s(oorlog),
   oppositie,
   i(oud,ouderen),
   s(regering),
   tegen,
   s(volk),
   weder]).

%% "hele gebeurtenis"

n([sg(partij),pl(partijen),
   ignore(m(partij,noun(de,count,sg),partij)),
   ignore(m(partij,noun(de,count,pl),partijen))
  ],de,[],
  [moord,
   rots,  % en niet rot_spar_tij
   scheld,
   schiet,
   slacht,
   steek,
   val,
   vecht,
   dim(partijtje)]).


n([mass(partijdigheid)],de,[]).

n([sg(partikel),pl(partikels)],both,[]).

n([pl(partituren),sg(partituur)],de,[]).

n([pl(partizanen),sg(partizaan)],de,[]).

n([pl(partners),sg(partner)],de,[],
  [f([airline]),
   coalitie,
   fusie,
   s(handel),
   s(leven),
   f(['non-airline']),
   f([sparring]),
   i(zaak,zaken)]).

n([sg(partnerschap),pl(partnerschappen)],het,[]).

n([pl('party\'s'),sg(party)],de,[],
  [f([cocktail])]).

n([pl(passen),sg(pas)],both,[],
  [meas_mod]).   % we liepen steeds twee passen achter

n([pl(passen),sg(pas)],both,[],
  [betaal,
   dans,
   pin,
   uit,      % bridge; ook wellicht pasje om uit te gaan
   water,
   wissel,
   zwembad,  % ook bij Boudewijn de Groot
   dim(pasje)]).

n([pl('pasfoto\'s'),sg(pasfoto)],de,[]).

n([sg('paso-doble'),pl('paso-dobles')],de,[]).

n([pl(paspoorten),sg(paspoort)],het,[]).

n([pl(passes),sg(pass)],de,[]).

n([pl(passages),sg(passage)],de,[]).

n([pl(passagiers),sg(passagier)],de,[],[mede]).

n([pl(passanten),sg(passant)],de,[]).

n([sg('passe-partout'),sg([passe,partout]),
   pl('passe-partouts'),pl([passe,partouts])],both,[]).

n([pl(passers),sg(passer)],de,[],[dim(passertje)]).

n([pl(passies),sg(passie)],de,[]).

n([sg(passief),pl(passieven)],both,[]).  % VL: het passief (vermogen)

n([mass(passiviteit)],de,[]).

n([pl('pasta\'s'),sg(pasta)],de,[]).

n([sg(pastei),pl(pasteien)],de,[]).

n([pl(pastellen),sg(pastel)],het,[],[dim(pastelletje)]).

n([sg(pastiche),pl(pastiches)],de,[]).

n([sg(pastoraal)],de,[]).

n([pl(pastoors),sg(pastoor)],de,[],[dim(pastoortje)]).

n([pl(pastores),pl(pastors),sg(pastor)],de,[]).

n([mass(pastoraat)],het,[]).

n([pl(pastorieën),sg(pastorie)],de,[]).

n([sg(paswoord),pl(paswoorden)],het,[]). 

n([mass([patat,frites])],de,[]).

n([mass([patat,met]),mass([patat,mét]),mass([patat,mèt])],de,[]).

n([mass([patat,oorlog])],de,[]).

n([mass([patat,speciaal])],de,[]).

n([pl(patatten),sg(patat)],de,[]).

n([sg([patatje,met]),pl([patatjes,met]),
   sg([patatje,mét]),pl([patatjes,mét]),
   sg([patatje,mèt]),pl([patatjes,mèt])],het,[]).

n([sg([patatje,oorlog]),pl([patatjes,oorlog])],het,[]).

n([sg([patatje,speciaal]),pl([patatjes,speciaal])],het,[]).

n([pl(patenten),sg(patent)],het,[]).

n([pl(paters),sg(pater)],de,[],
  [dim(patertje)]).

n([pl([paters,'Montfortaan']),sg([pater,'Montfortaan'])],de,[],
  [dim(patertje)]).

n([sg([pater,familias])],de,[]).

n([mass(pathologie)],de,[]).

n([sg(patholoog),pl(pathologen)],de,[]).

n([sg('patholoog-anatoom')],de,[]).

n([mass(pathos)],both,[]).

n([pl('patio\'s'),sg(patio)],de,[]).

n([pl(patiënten),sg(patiënt),sg(patient),pl(patienten)],de,[],
  [aids,h(aids),
   alzheimer,
   hart,
   hemofilie,
   i(hoofd_pijn,hoofdpijn),
   kanker,
   reuma
  ]).

n([pl(patiëntes),sg(patiënte)],de,[],
  [aids,h(aids),
   alzheimer,
   hart,
   hemofilie,
   kanker,
   reuma
  ]).

n([mass(patologie)],de,[]).

n([mass(patos)],het,[]).

n([pl(patriarchen),sg(patriarch)],de,[]).

n([pl(patrijzen),sg(patrijs)],de,[]).

n([pl(patrijspoorten),sg(patrijspoort)],de,[]).

n([sg(patrimonium)],het,[]).

n([pl(patriotten),sg(patriot)],de,[]).

n([mass(patriottisme)],het,[]).

n([sg(patronaat),pl(patronaten)],het,[]).

n([pl(patronen),pl(patroons),sg(patroon)],de,[]).

n([pl(patronen),sg(patroon)],het,[],
  [cultuur,
   s(gedrag),
   dim(patroontje)]).

n([pl(patrouilles),sg(patrouille)],de,[],[politie]).

n([pl(patstellingen),sg(patstelling)],de,[]).

n([pl(pausen),sg(paus),sg('Paus')],de,[]).

n([pl(pauwen),sg(pauw)],de,[],[dim(pauwtje)]).

%% geen compound met -oog want +de
n([sg(pauwoog),pl(pauwogen)],de,[],
  [dag,
   nacht]).

n([pl(pauzen),pl(pauzes),sg(pauze)],de,[],
  [koffie,
   lunch]).

n([pl(paviljoenen),pl(paviljoens),sg(paviljoen)],het,[],[dim(paviljoentje)]).

n([pl([pay,outs]),sg([pay,out])],de,[]).

n([sg('PCB'),pl('PCB\'s'),pl('PCBs')],het,[]).

n([sg(pgb),pl('pgb\'s')],het,[]).

n([pl('PC\'s'),sg('PC')],de,[]).  % personal computer in qtleap spelling

n([pl('pc\'s'),sg(pc)],de,[]).  % personal computer

n([pl('pc\'s'),sg(pc)],het,[]). % VL party centrum?

n([mass(pech)],de,[vp,sbar,subject_sbar]).

n([pl(pedalen),sg(pedaal)],both,[],[dim(pedaaltje)]).

n([pl(pedagogen),sg(pedagoog)],de,[],[muziek]).

n([pl(pedagoges),sg(pedagoge)],de,[]).

n([mass(pedagogie)],de,[]).

n([mass(pedagogiek)],de,[]).

n([sg(peddel),pl(peddels)],de,[]).

n([mass(pediatrie)],de,[]).

n([sg(pedofiel),pl(pedofielen)],de,[]).

n([mass(pedofilie)],de,[]).

n([pl(pees),pl(peeën),sg(pee)],de,[],[dim(peetje)]).

n([pl(penen),sg(peen)],de,[],[dim(peentje)]).

n([pl(peren),sg(peer)],de,[],[dim(peertje)]).

n([sg([peer,group]),
   pl([peer,groups])],de,[]).

n([mass([peer,review])],de,[]).

n([sg(peetoom),pl(peetooms)],de,[]).

n([sg(peettante),pl(peettantes)],de,[]).

n([sg(peetvader),pl(peetvaders)],de,[]).

n([pl(pezen),sg(pees)],de,[]).

n([pl(peignoirs),sg(peignoir)],de,[]).

n([pl(peilen),sg(peil)],het,[pred_pp(op)]).

n([pl(peilen),sg(peil)],het,[],
  [prijs,
   water]).

n([pl(peilingen),sg(peiling)],de,[],
  [opinie]).

n([mass(pek)],het,[]).

n([mass(pekel)],de,[]).

%% VL, schil?
n([sg(pel)],de,[]).

n([sg(pellet),pl(pellets)],de,[],[hout]).

n([pl(pelotons),sg(peloton)],het,[measure]).

n([pl(pelotons),sg(peloton)],het,[],
  [wieler]).

n([pl(pelgrims),sg(pelgrim)],de,[]).

n([sg(pelgrimage),pl(pelgrimages)],de,[]).

n([pl(pelikanen),sg(pelikaan)],de,[],[dim(pelikaantje)]).

n([pl(pelzen),sg(pels)],de,[],[dim(pelsje)]).

n([sg(penalty),pl('penalty\'s')],de,[]).

n([pl(pennen),sg(pen)],de,[],
  [bal,
   stuur,
   dim(pennetje)]).

n([pl(pendanten),sg(pendant)],both,[]).  % celex: het

n([sg(pendel),pl(pendels)],de,[]).

n([pl(pendelaars),sg(pendelaar)],de,[]).

n([pl(pendules),sg(pendule)],de,[]).

n([pl(penetraties),sg(penetratie)],de,[]).

n([pl(penicillines),sg(penicilline)],de,[]).

n([sg(penis),pl(penissen)],de,[]).

n([sg(pennenlikker),pl(pennenlikkers)],de,[]).

n([pl(penningen),sg(penning)],de,[],[dim(penninkje)]).

n([pl(penningmeesters),sg(penningmeester)],de,[]).

n([pl(pence),pl('penny\'s'),sg(penny)],de,[meas_mod]).

n([pl(pensen),sg(pens)],de,[]).

n([pl(penselen),sg(penseel)],both,[],[dim(penseeltje)]).

n([pl(penseelstreken),sg(penseelstreek)],de,[]).

n([pl(pensioenen),sg(pensioen)],het,[],
  [brug,
   s(ouderdom),
   pre,
   vroeg,
   dim(pensioentje)]).

n([pl(pensions),sg(pension)],het,[],[dim(pensionnetje)]).

n([pl(pensionaten),sg(pensionaat)],het,[]).

n([pl(pensioneringen),sg(pensionering)],de,[]).

n([sg(pentagram),pl(pentagrammen)],both,[]).

n([sg(penthouse),pl(penthouses),
   sg('pent-house'),pl('pent-houses'),
   sg([pent,house]),pl([pent,houses])],both,[]).

n([sg([people,mover]),pl([people,movers])],de,[]).

n([mass(pep)],de,[]).

n([pl(pepers),sg(peper)],de,[],[cayenne]).

n([pl(peperkorrels),sg(peperkorrel)],de,[]).

n([pl(pepermunten),sg(pepermunt)],de,[]).

n([mass(pepperspray)],de,[]).

n([pl(percelen),sg(perceel)],het,[measure],[dim(perceeltje)]).

n([pl(percentages),sg(percentage)],het,[measure]).

n([pl(percentages),sg(percentage)],het,[],
  [alcohol,
   groei,
   opkomst,
   rente,
   h(rsi),h('RSI'),
   i(werkeloosheid,werkloosheids),s(werkeloosheid),
   winst]).

n([pl(percepties),sg(perceptie)],de,[sbar]).

n([mass(percussie)],de,[]).

n([pl(perebomen),sg(pereboom),
   pl(perenbomen),sg(perenboom)],de,[]).

n([pl(perfecties),sg(perfectie)],de,[pred_pp(van)]).

n([sg(perfectionist),pl(perfectionisten)],de,[]).

n([sg(performance),pl(performances)],de,[]).

n([pl(periferieën),sg(periferie)],de,[]).

n([pl(perikelen),pl(perikels),sg(perikel)],het,[]).

n([sg(perimeter),pl(perimeters)],de,[]).

n([sg(periode),pl(perioden),pl(periodes)],de,
  [temp_mod,
   sbar,
   app_measure,
   van_sbar],
  [s(ambt),
   begin,
   s(beoordeling),
   bloei,
   s(contingent),
   s(kabinet),
   meet,
   s(overgang),
   proef,
   regeer,
   s(rijping),
   rust,
   sub,
   transfer,
   vakantie,
   vergader,
   verslag,
   s(zitting)]).

n([pl(periodieken),sg(periodiek)],both,[]).

n([pl(perken),sg(perk)],het,[]).

n([pl(perkamenten),sg(perkament)],het,[]).

n([pl(permissies),sg(permissie)],de,[vp]).

n([pl(perrons),sg(perron)],het,[],[dim(perronnetje)]).

n([sg([perpetuum,mobile])],both,[]).

n([pl(persen),sg(pers)],de,[],
  [fruit]).

n([pl(persen),sg(pers)],de,[],
  [roddel]).

n([pl(persberichten),sg(persbericht)],het,[]).

n([pl(persconferenties),sg(persconferentie)],de,[]).

n([pl(persiflages),sg(persiflage)],de,[]).

n([sg([persona,non,grata])],de,[]).

n([pl(personages),sg(personage)],both,[],
  [hoofd,
   roman]).

n([sg([personal,coach]),pl([personal,coaches])],de,[]).

n([sg([personal,computer]),pl([personal,computers])],de,[]).

n([pl(personalia)],de,[]).

n([mass(personeel)],het,[],
  [ambulance,
   cabine,
   horeca,
   h('NS'),
   s(overheid),
   winkel]).

n([pl(personificaties),sg(personificatie)],de,[]).

n([pl(personen),sg(persoon)],de,[],
  [s(bewind),
   hoofd,
   dim(persoontje),
   s(vertrouwen)]).

n([pl(persoonlijkheden),sg(persoonlijkheid)],de,[]).

n([pl(persoonsbewijzen),sg(persoonsbewijs)],het,[]).

n([sg(persoonsvorm),pl(persoonsvormen)],de,[]).

n([pl(perspectieven),sg(perspectief),
   pl(perspektieven),sg(perspektief)],both,[]).

n([pl(perziken),sg(perzik)],de,[]).

n([pl('peseta\'s'),meas(peseta)],de,[meas_mod,measure]).

n([meas(peso),pl(pesos),pl('peso\'s')],de,[meas_mod,measure]).

n([mass(pessimisme)],het,[]).

n([pl(pessimisten),sg(pessimist)],de,[]).

n([mass(pest)],de,[subject_sbar]).

n([mass(pest)],de,[],
  [long,
   s(varken),
   vogel]).

n([pl(pesterijen),sg(pesterij)],de,[sbar,vp]).

n([sg(pesticide),pl(pesticides),pl(pesticiden)],both,[]).

n([mass(pesto)],de,[]).

n([pl(petten),sg(pet)],de,[],
  [honkbal,
   dim(petje)]).

n([mass(peterselie)],de,[]).

n([pl(petities),pl(petitiën),sg(petitie)],de,[]).

n([mass(petroleum)],de,[]).

n([pl(peuken),sg(peuk)],de,[],[dim(peukje)]).

n([sg(peul),pl(peulen)],de,[],[dim(peultje)]).

n([pl(peuters),sg(peuter)],de,[],[dim(peutertje)]).

n([pl(peuterspeelzalen),sg(peuterspeelzaal)],de,[]).

n([sg(pg),pl('pg\'s')],de,[]).

n([sg(pi)],both,[]).

n([pl(pianisten),sg(pianist)],de,[]).

n([pl(pianistes),sg(pianiste)],de,[]).

n([pl('piano\'s'),sg(piano)],de,[],[dim(pianootje)]).

n([pl('piazza\'s'),sg(piazza)],de,[]).

n([pl('piccolo\'s'),sg(piccolo)],de,[],[dim(piccolootje)]).

n([pl('pick-ups'),sg('pick-up')],de,[]).

n([pl(picknicks),sg(picknick)],de,[]).

n([sg(pictogram),pl(pictogrammen)],both,[app_measure]).

n([sg([pièce,de,résistance]),
   sg([piece,de,resistance])],both,[]).

n([sg([pied,à,terre]),pl([pieds,à,terre])],both,[]).

n([pl(pieken),meas(piek)],de,[meas_mod,measure]).

n([pl(piemels),sg(piemel)],de,[],[dim(piemeltje)]).

n([sg(piep),pl(piepen)],de,[],[dim(piepje)]).

n([pl(piepers),sg(pieper)],de,[],[dim(piepertje)]).

n([pl(pieren),sg(pier)],de,[],[dim(piertje)]).

n([sg(piercing),pl(piercings)],de,[]).

n([pl(pierrots),sg(pierrot)],de,[]).

n([mass(pies)],de,[]).

n([sg(piet),pl(pieten)],de,[]). % zwarte

n([sg(piëteit),pl(piëteiten)],de,[]).

n([pl(pietjes),sg(pietje)],het,[]).

n([sg(pietsje)],het,[measure,mod]).

n([mass(pigment)],het,[]).

n([pl(pijen),sg(pij)],de,[]).

n([pl(pijlen),sg(pijl)],de,[],[dim(pijltje)]).

n([pl(pijlers),sg(pijler)],de,[],[dim(pijlertje)]).

n([sg(pijlstaart),pl(pijlstaarten)],de,[]).

n([pl(pijnen),sg(pijn)],de,[],
  [buik,
   hoofd,
   keel,
   kies,
   kop,
   maag,
   nek,
   rug,
   spier]).

n([pl(pijnbomen),sg(pijnboom)],de,[]).

n([pl(pijnscheuten),sg(pijnscheut)],de,[]).

n([pl(pijnstillers),sg(pijnstiller)],de,[]).

n([pl(pijpen),sg(pijp)],de,[measure],
  [dim(pijpje)]).

n([pl(pijpen),sg(pijp)],de,[],
  [s(vrede),
   water,
   dim(pijpje)]).

n([pl(pijpers),sg(pijper)],de,[]).

n([pl(pikken),sg(pik)],de,[]).

n([pl(pikniks),sg(piknik)],de,[]).

n([pl(pillen),sg(pil)],de,[],
  [abortus,
   slaap,
   h('XTC'),h(xtc),
   zet,
   dim(pilletje)]).

n([pl(pilaren),sg(pilaar)],de,[],[dim(pilaartje)]).

n([sg(pilaster),pl(pilasters)],de,[]).

n([pl(piloten),sg(piloot)],de,[],
  [co,
   leger,  % en niet leeg_piloot
   zelfmoord]).

n([sg(pilot),pl(pilots)],de,[]).

n([mass(pils)],both,[],[dim(pilsje)]).

n([pl(pinnen),sg(pin)],de,[],[dim(pinnetje)]).

n([pl(pincetten),sg(pincet)],het,[]).

n([pl('pinda\'s'),sg(pinda)],de,[],[dim(pindaatje)]).

n([mass(pindakaas)],de,[]).

n([sg(ping)],both,[]).

n([sg(pingel),pl(pingels)],de,[]).

n([mass(pingpong),mass([ping,pong]),mass('ping-pong')],het,[]).

n([pl(pinken),sg(pink)],de,[]).

n([pl(pinots),sg(pinot)],de,[]).

n([sg([pinot,blanc])],de,[]).

n([sg([pinot,gris])],de,[]).

n([sg([pinot,noir])],de,[]).

n([pl(pinten),sg(pint)],de,[measure],
  [dim(pintje)]).

n([pl(pionnen),pl(pions),sg(pion)],de,
  [measure,
   meas_mod],  % Timman kwam een pion achter
  [dim(pionnetje)]).

n([pl(pioniers),sg(pionier)],de,[]).

n([pl(piraten),sg(piraat)],de,[]).

n([pl(piramiden),pl(piramides),sg(piramide)],de,[]).

n([mass(piraterij)],de,[]).

n([mass(pis)],de,[]).

n([sg(pisang)],de,[]).

n([mass([pisang,ambon])],de,[]).

n([mass([pisang,batu])],de,[]).

n([mass([pisang,goreng])],de,[]).

n([sg(pissebed),pl(pissebedden)],both,[]).

n([pl(pistes),sg(piste),pl(pisten)],de,[],
  [ijs,
   tussen]).

n([pl(pistolen),sg(pistool)],het,[],[dim(pistooltje)]).

n([sg(pistolet),pl(pistolets)],het,[],[dim(pistoletje)]).  % geen pis_tol_ets

n([mass(pit)],het,[]).

n([sg(pit),pl(pitten)],de,[],
  [pijnboom,
   dim(pitje)]).

n([sg(pitbull),pl(pitbulls)],de,[]).

n([sg(pitch),pl(pitches)],de,[]).

n([sg(pitcher),pl(pitchers)],de,[]).

n([sg(pixel),pl(pixels)],de,[],
  [giga,
   mega]).

n([pl('pizza\'s'),sg(pizza)],de,[]).

n([sg(pizzeria),pl('pizzeria\'s')],de,[]).

n([meas(pk)],both,[meas_mod,measure]).

n([sg(placebo),pl('placebo\'s')],de,[]).

n([sg(placemat),pl(placemats)],de,[]).

n([sg(plag),pl(plaggen)],de,[]).

n([pl(plagen),sg(plaag)],de,[],
  [dim(plaagje)]).

n([pl(platen),sg(plaat)],de,[measure],
  [dim(plaatje)]).

n([pl(platen),sg(plaat)],de,[],
  [bak,
   beeld,
   bloed,
   gevel,
   glas,
   golf,
   grammofoon,
   kenteken,
   naam,
   nummer,
   studio,
   wijzer,
   dim(plaatje)]).

n([pl(plaatsen),sg(plaats)],de,
  [meas_mod,			% drie plaatsen klimmen/zakken
   pred_pp(op),
   pred_pp(op,subject_sbar),
   pred_pp(op,subject_vp)],
  [dim(plaatsje)]).

n([pl(plaatsen),sg(plaats),
   ignore_stem(plaats),
   ignore_stem(plaats_DIM)],de,[],
  [aanleg,
   s(arbeid),
   bad,
   basis,
   begraaf,
   berg,
   binnen,
   bouw,
   broed,
   buiten,
   ere,
   finale,
   geboorte,
   grens,
   hoofd,
   kust,
   s(landing),
   leger,
   lig,
   los,
   markt,
   s(ontmoeting),
   s(opleiding),
   opslag,
   opvang,
   parkeer,
   pleister,
   podium,
   rust,
   schuil,
   slaap,
   speel,
   i(staan,sta),
   stage,
   stand,
   start,
   stop,
   stort,
   verblijf,s(verblijf),
   s(verzorging),
   s(vestiging),
   vind,
   vrij,
   werk,
   woon,
   zit,
   dim(plaatsje)]).

n([pl(plaatsbepalingen),sg(plaatsbepaling)],de,[]).

n([pl(plaatsingen),sg(plaatsing)],de,[]).

n([pl('placenta\'s'),sg(placenta)],de,[]).

n([pl(plafonds),pl(plafons),sg(plafond),sg(plafon)],het,[]).

n([pl(plagerijen),sg(plagerij)],de,[sbar,vp],[dim(plagerijtje)]).

n([mass(plagiaat)],both,[]).

n([mass(plakband)],both,[]).

n([pl(plakken),sg(plak)],de,[measure],[dim(plakje)]).

n([sg(plakker),pl(plakkers)],de,[],[dim(plakkertje)]).

n([pl(plakkaten),sg(plakkaat)],het,[]).

%% VL de plan
%% NNL het plan
n([pl(plannen),sg(plan)],both,
  [vp,
   subject_sbar,
   subject_vp
  ],
  [actie,f([actie]),
   i(baan,banen),
   belasting,
   s(beleid),
   s(bestemming),
   s(bezuiniging),
   bouw,
   business,
   fusie,
   grond,
   herstel,
   s(hervorming),
   s(kabinet),
   leer,
   h('Marshall'),'Marshall',
   master,
   i(meer_jaar,meerjaren),
   i(mest_actie,mestactie),
   i(milieu_actie,milieuactie),f([milieu,actie]),
   s(mobiliteit),
   s(onderneming),
   nood,
   i(ramp,rampen),
   s(redding),
   reorganisatie,
   s(sanering),
   streek,
   structuur,
   toekomst,
   s(uitbreiding),
   vakantie,
   s(veiligheid),
   s(vrede),
   woningbouw,
   dim(plannetje)]).

n([sg([plan,de,campagne])],het,[]).

n([pl(planeten),sg(planeet)],de,[app_measure],[dwerg]).

n([pl(planetoïden),sg(planetoïde)],de,[app_measure]).

n([pl(planken),sg(plank)],de,[measure],[dim(plankje)]).

n([pl(planken),sg(plank)],de,[],
  [i(boeken,boek),
   duik,
   spring,
   dim(plankje)]).

n([pl(plankenvloeren),sg(plankenvloer)],de,[]).

n([pl(plankieren),sg(plankier)],het,[]).

n([mass(plankton)],both,[]).

n([pl(planners),sg(planner)],de,[],
  [route]).

n([mass(planning)],de,[subject_sbar, subject_vp],[s(gezin)]).

n([mass(planologie)],de,[]).

n([pl(planologen),sg(planoloog)],de,[]).

n([pl(planten),sg(plant)],de,
  [app_measure],
  [hennep,
   kamer,
   klim,
   moeras,
   oever,
   pot,
   sier,
   waard,
   water,
   dim(plantje)]).

n([pl(plantages),sg(plantage)],de,[],
  [koffie,
   thee]).

n([pl(planters),sg(planter)],de,[]).

n([pl(plantsoenen),sg(plantsoen)],het,[],
  [wandel,
   dim(plantsoentje)]).

n([pl(plassen),sg(plas)],de,[measure],[dim(plasje)]).

n([pl('plasma\'s'),sg(plasma)],het,[],[bloed]).

n([pl(plastieken),sg(plastiek)],both,[]).

n([pl(plastics),sg(plastic)],het,[]).

n([sg(plat),pl(platten)],het,[]).  % continentaal

n([pl(platanen),sg(plataan)],de,[]).

n([pl(plateaus),sg(plateau)],het,[],
  [berg,
   heuvel,
   dim(plateautje)]).

n([pl(platformen),pl(platforms),sg(platform)],het,[],
  [boor,
   olie,
   dim(platformpje)]).

n([mass(platina),mass(platinum),mass(platinium)],both,[]).

n([pl(plattegronden),sg(plattegrond)],de,[]).

n([mass(platteland)],het,[]).

n([sg(platter),pl(platters)],de,[]).

n([sg(plavei),pl(plaveien)],both,[]). 

n([pl(plaveisels),sg(plaveisel)],het,[]).

n([pl(plavuizen),sg(plavuis)],de,[]).

n([pl('play-offs'),sg('play-off'),
   pl(playoffs),sg(playoff),
   pl([play,offs]),sg([play,off])],de,[]).

n([mass(playback),
   mass('play-back'),
   mass([play,back])],both,[]).

n([pl(playboys),sg(playboy)],de,[]).

n([pl(plechtigheden),sg(plechtigheid)],de,[]).

n([pl(plees),sg(plee)],de,[],[dim(pleetje)]).

n([sg(pleger),pl(plegers)],de,[],
  [veel]).

n([pl(pleidooien),sg(pleidooi)],het,[vp],
  [s(opening)]).

n([pl(pleinen),sg(plein)],het,[],
  [school,
   speel,
   dim(pleintje)]).

n([pl(pleisters),sg(pleister)],de,[],
  [trek,
   dim(pleistertje)]).

n([stem(pleister_laag),
   pl(pleisterlagen),sg(pleisterlaag)],de,[]).

n([pl(pleiten),sg(pleit)],het,[]).

n([pl(pleitbezorgers),sg(pleitbezorger)],de,[]).

n([pl(pleiters),sg(pleiter)],de,[],[straf]).

n([pl(plekken),sg(plek)],de,
  [meas_mod,
   measure,
   sbar,  % mijn zwakke plek is dat ...
   vp],
  [dim(plekje)]).

n([pl(plekken),sg(plek)],de,[],
  [parkeer,
   ramp,
   dim(plekje)]).

n([sg(plens),pl(plenzen)],de,[measure]).

%% een bakkie pleur
n([mass(pleur)],de,[]).

n([mass(pleuris)],de,[]).

n([pl(plezieren),sg(plezier)],het,
  [sbar,vp],
  [dim(pleziertje)]).

n([pl(plezieren),sg(plezier)],het,[],
  [lees]).

n([pl(plichten),sg(plicht)],de,
  [subject_vp,
   subject_sbar,
   vp],
  [burger]).

n([pl(plichten),sg(plicht)],de,[],
  [bewaar,
   dienst,
   s(geheimhouding),
   identificatie,
   leer,
   meld,
   s(melding),
   ophok,
   opkomst,
   premie,
   sollicitatie,
   visum,
   zorg,
   zwijg]).

n([pl(plichtplegingen),sg(plichtpleging)],de,[sbar,vp]).

n([mass(plichtsverzuim)],het,[]).

n([sg(plint),pl(plinten)],de,[]).

n([pl(ploegen),sg(ploeg)],de,[measure]). % een ploeg Japanners

n([pl(ploegen),sg(ploeg)],de,[],
  [h(a),h('A'),
   h(b),h('B'),
   camera,
   cricket,
   estafette,
   kern,
   knok,
   i(land,landen),
   i(man,mannen),
   s(minister),
   nacht,
   rugby,
   schaats,
   televisie,
   thuis,
   turn,
   volleybal,
   i(vrouw,vrouwen),
   uit,h(uit),
   voetbal,
   wieler,
   zeil,
   zwem]).

n([pl(ploerten),sg(ploert)],de,[]).

n([pl(ploffen),sg(plof)],de,[]).

n([mass(plomp)],de,[]).

n([pl(plonsen),pl(plonzen),sg(plons)],de,[measure]).

n([pl(plooien),sg(plooi)],de,[],[dim(plooitje)]).

n([pl(plots),sg(plot)],both,[]).   % was 'de'

n([sg(pluche)],het,[]).

n([sg(plug),pl(pluggen)],de,[]).

n([sg(plugin),sg('plug-in'),
   pl(plugins),pl('plug-ins')],de,[]).

n([pl(pluimen),sg(pluim)],de,[sbar,vp],[dim(pluimpje)]).

n([mass(pluimvee)],het,[]).

n([mass(pluis)],both,[]).

n([pl(plukken),sg(pluk)],de,[measure,sbar,vp],[dim(plukje)]).

n([sg(plukker),pl(plukkers)],de,[]).

n([sg(plukster),pl(pluksters)],de,[]).

n([sg(plunderaar),pl(plunderaars)],de,[]).

n([pl(plunderingen),sg(plundering)],de,[]).

n([pl(plunjezakken),sg(plunjezak)],de,[]).

n([mass(pluralisme)],het,[]).

n([mass(pluraliteit)],de,[]).

n([mass(pluriformiteit)],de,[]).

n([pl(plussen),sg(plus)],both,[],[dim(plusje)]).

n([pl(pluspunten),sg(pluspunt)],het,[sbar,vp]).

n([mass(plutonium)],het,[]).

n([pl('po\'s'),sg(po)],de,[]).

n([pl(pockets),sg(pocket)],de,[]).

n([sg(podcast),pl(podcasts)],de,[]).

n([pl(podia),pl(podiums),sg(podium)],het,[],[ere]).

n([pl(poedels),sg(poedel)],de,[],[dim(poedeltje)]).

n([pl(poeders),sg(poeder)],both,[],
  [bak,
   cacao,
   curry,
   gember,
   kaneel,
   kerrie,
   knoflook,
   melk,
   i(baby_melk,babymelk),
   paprika,
   was,
   wortel,
   dim(poedertje)]).

n([sg(poeier),pl(poeiers)],de,[]).

n([pl(poelen),sg(poel)],de,[measure],[dim(poeltje)]).
%% een poel water

n([sg(poema),pl('poema\'s')],de,[]).

n([mass(poen)],de,[]).

n([mass(poep)],de,[],
  [i(hond,honden),
   dim(poepje)]).

n([pl(poesen),pl(poezen),sg(poes)],de,[],[dim(poesje)]).

n([mass(poespas)],de,[]).

n([mass(poëtica)],de,[]).

n([sg(poets),pl(poetsen)],de,[]).

n([mass(pof)],de,[]).

n([pl(pogingen),sg(poging)],de,[vp]).

n([pl(pogingen),sg(poging)],de,[],
  [s(bemiddeling),
   coup,
   doel,
   lijm,
   zelfmoord]).

n([sg(pogrom),pl(pogroms)],de,[]).

n([pl(points),sg(point)],het,[],
  [f([cash]),
   f([victory])
  ]).

n([pl(pointes),sg(pointe)],de,[sbar,vp]).

n([pl(pokken),sg(pok)],de,[],[pest]).

n([mass(poker)],het,[]).

n([sg([poker,face])],de,[]).

n([pl(pollen),sg(pol)],de,[measure]).  % gras, hei

n([sg(polka),pl('polka\'s')],de,[]).

n([mass(pollen)],het,[]).  % het pollen = het zaad

n([mass(polarisatie),mass(polarizatie)],de,[]).

n([mass(polariteit)],de,[]).

n([pl(poldermodellen),sg(poldermodel)],het,[]).

n([pl(polders),sg(polder)],de,[],[dim(poldertje)]).

n([sg([pole,position]),
   pl([pole,positions])],de,[]).

n([pl(polemieken),sg(polemiek)],de,[]).

n([mass(policy)],de,[subject_vp,subject_sbar,vp,sbar]).

n([pl(poliklinieken),sg(polikliniek)],de,[]).

n([mass(polio)],de,[]).

n([pl(polissen),sg(polis)],de,[],
  [koopsom,
   s(verzekering)]).

n([mass(politicologie)],de,[]).

n([pl(politicologen),sg(politicoloog)],de,[]).

n([sg(politicologe),pl(politicologes)],de,[]).

n([pl(politici),sg(politicus)],de,[],
  [s(beroep),
   burger,
   s(macht)]).

n([pl(politicae),sg(politica)],de,[],
  [s(beroep),
   burger,
   s(macht)]).

n([mass(politie)],de,[],
  [gemeente,
   grens,
   oproer,
   parket,
   regio,
   s(rijk),
   rivier,
   spoorweg,
   s(verkeer),
   h('VN'),
   i(vreemdeling,vreemdelingen),
   i(zede,zeden)]).

n([mass(politiek)],de,[],
  [s(bezetting),
   gemeente,
   geo,
   grond, % welk kabinet viel daarop?
   s(macht),
   i(nederzetting,nederzettingen),
   partij,
   s(vrede),
   wereld]).

n([sg(politieker),pl(politiekers)],de,[]).

n([stem(politie_man),
   pl(politiemannen),sg(politieman)],de,[]).

n([pl(politierechters),sg(politierechter)],de,[]).

n([pl(polls),sg(poll)],de,[]).

n([sg(polonaise),pl(polonaises)],de,[]).

n([pl(polsen),sg(pols)],de,[]).

n([pl(polshorloges),sg(polshorloge)],het,[]).

n([pl(polsslagen),sg(polsslag)],de,[]).

n([sg(polsstok),pl(polsstokken)],de,[]).

n([sg(polyester),pl(polyesters)],het,[]).

n([sg(polygraaf),pl(polygrafen)],de,[]).

n([sg(polymeer),pl(polymeren)],both,[app_measure]).

n([pl(pompen),sg(pomp)],de,[]).

n([pl(pompoenen),sg(pompoen)],de,[],[dim(pompoentje)]).

n([meas(pond),pl(ponden)],het,
  [meas_mod,
   measure],
  [dim(pondje)]).

n([meas([pond,sterling]),pl([ponden,sterling])],both,[meas_mod,measure]).

n([pl(ponten),sg(pont)],de,[],[dim(pontje)]).

n([sg(pontifex)],both,[]).

n([sg(ponton),pl(pontons)],both,[]).

n([mass(pony),pl('pony\'s'),sg(pony)],de,[],[dim('pony\'tje')]).

n([pl(pooiers),sg(pooier)],de,[]).

n([sg(pook),pl(poken)],de,[]).

n([pl(polen),pl(pools),sg(pool)],de,[],
  [s(arbeid),
   haven]).

n([sg(pooler),pl(poolers)],de,[],
  [i(baan,banen)
  ]).

n([mass(poolshoogte)],de,[]).

n([sg(poolster)],de,[]).

n([pl(poorten),sg(poort)],de,[],
  [detectie,
   s(stad),
   s(toegang),
   tol,
   dim(poortje)
  ]).

n([sg(poos),pl(pozen)],de,[measure,temp_mod,sbar],[dim(poosje)]).

n([pl(poten),sg(poot)],de,[],[dim(pootje)]).

n([sg(popart)],de,[],[]).

n([pl(poppen),sg(pop)],de,[],[sneeuw,
			      dim(poppetje),
			      dim(popje)]).

n([sg('pop-up'),pl('pop-ups'),sg([pop,up]),pl([pop,ups])],de,[]).

n([pl(popen),pl(popes),sg(pope)],de,[]).
 
n([mass(populariteit)],de,[]).

n([pl(populaties),sg(populatie)],de,[measure]).

n([pl(populieren),sg(populier)],de,[],[dim(populiertje)]).

n([mass(populisme)],het,[]).

n([sg(populist),pl(populisten)],de,[]).

n([pl(porren),sg(por)],de,[],[dim(porretje)]).

n([pl(poriën),sg(porie)],de,[]).

n([pl('porno\'s'),sg(porno)],de,[],
  [i(kind,kinder)]).

n([mass(pornografie)],de,[]).

n([pl(porseleinen),sg(porselein),
   pl(porceleinen),sg(porcelein)],het,[]).

n([mass(port)],de,[]).

n([pl(portalen),sg(portaal)],both,[],[dim(portaaltje)]).

n([sg(portal),pl(portals)],both,[]).

n([pl(portefeuilles),sg(portefeuille)],de,[app_measure]).

n([pl(portefeuilles),sg(portefeuille)],de,[],
  [s(belegging),
   order,
   vastgoed]).

n([pl(portemonnees),sg(portemonnee)],de,[],[dim(portemonneetje)]).

n([sg(portfolio),pl('portfolio\'s')],both,[]).

n([pl(porties),sg(portie)],de,[measure]).

n([pl(portieken),sg(portiek)],both,[]).

n([pl(portiers),sg(portier)],de,[]).

n([pl(portieren),sg(portier)],het,[]).

n([pl(porti),pl('porto\'s'),sg(porto)],het,[]).

n([pl(portretten),sg(portret)],het,[sbar],[dim(portretje)]).

n([pl(posen),pl(poses),sg(pose)],de,[vp]).

n([pl(posities),sg(positie)],de,
  [sbar,
   vp,
   meas_mod], % hij zakte een positie op de wereldranglijst
  []).

n([pl(posities),sg(positie),
   ignore_stem(positie)
   ],de,
  [],
  [i(arbeid_markt,arbeidsmarkt),
   concurrentie,
   s(handel),
   s(inkomen),
   kop,
   luxe,
   s(macht),
   markt,
   monopolie,h(monopolie),
   s(onderhandeling),
   order,
   s(recht),
   sleutel,
   top,
   s(uitgang),
   s(uitzondering),
   s(vermogen),
   zit]).

n([mass(positivisme)],het,[]).

n([pl(positivisten),sg(positivist)],de,[]).

n([pl(posten),sg(post)],de,[app_measure]).

n([pl(posts),sg(post)],de,[],[dim(postje)]).  % op internet fora

n([pl(posten),sg(post)],de,[],
  [s(begroting),
   controle,
   i(huis_arts,huisartsen),
   leger,
   s(minister),
   observatie,
   politie,
   sluit,
   uit,
   dim(postje)]).

n([pl(postboden),pl(postbodes),sg(postbode)],de,[]).

n([pl(postbussen),sg(postbus)],de,[]).

n([pl(posters),sg(poster)],de,[],
  [tuin]).

n([pl(posterijen)],de,[]).

n([pl(postulaten),sg(postulaat)],het,[sbar,vp]).

n([pl(posturen),sg(postuur)],het,[],[dim(postuurtje)]).

n([pl(postzegels),sg(postzegel)],de,[]).

%% we hebben twee potjes gevoetbald
%% het potje innovatie was nog niet leeg
n([pl(potten),sg(pot)],de,
  [temp_mod,
   measure,
   app_measure],
  [dim(potje)]).

n([pl(potten),sg(pot)],de,
  [],
  [inkt,
   koffie,
   i(strop,stroppen),
   thee,
   toilet,
   dim(potje)]).

n([pl(potenties),sg(potentie)],de,[vp]).

n([pl(potentiëlen),sg(potentieel)],het,[]).

n([pl(potloden),sg(potlood)],het,[],[dim(potloodje)]).

n([pl(pottenbakkers),sg(pottenbakker)],de,[]).

n([pl(potters),sg(potter)],de,[]).

n([sg(potvis),pl(potvissen)],de,[]).

n([pl(poules),sg(poule)],de,[]).

n([mass('pousse-café'),
   mass([pousse,café]),
   mass(poussecafé)],de,[]).

n([mass(power)],de,[]).

n([sg(powerpoint),
   pl(powerpoints)],both,[]).

n([sg(powiat),pl(powiats)],de,[]).  % Pools district

n([pl(poëten),sg(poëet)],de,[]).

n([mass(poëzie)],de,[]).

n([mass(pr)],de,[]).

n([mass(praal)],de,[],[zege]).

n([mass(praat)],de,[]).

n([stem(praat_DIM),
   pl(praatjes),sg(praatje)],het,[sbar]).

n([mass([praat,voor,de,vaak])],de,[]).

n([sg([praatje,voor,de,vaak]),pl([praatjes,voor,de,vaak])],het,[]).

n([mass(pracht)],de,[]).

n([pl(practijken),sg(practijk)],de,[sbar,vp]).

n([sg(pragmaticus),pl(pragmatici)],de,[]).

n([mass(pragmatisme)],het,[]).

n([pl(prairies),pl(prairiën),sg(prairie)],de,[]).

n([mass(prak)],de,[]).

n([pl(praktijken),sg(praktijk)],de,[sbar,vp]).

n([pl(praktijken),sg(praktijk)],de,[],
  [s(afpersing),
   i(huis_arts,huisartsen)]).

n([sg(prang),pl(prangen)],de,[]).

n([pl(praters),sg(prater)],de,[],[dim(pratertje)]).

n([mass(praxis)],de,[]).

%% ik vind het een pré dat hij ...
n([sg(pré)],both,[subject_sbar]).

n([pl('pre-occupaties'),sg('pre-occupatie')],de,[]).

n([pl(precedenten),sg(precedent)],het,[]).

n([mass(precisie)],de,[]).

n([stem(predicaat),  % tot 2006 predikaat... vooruitgang!
   pl(predikaten),sg(predikaat),
   pl(predicaten),sg(predicaat)],het,
  [app_measure,
   np_app_measure,
   sbar,
   start_app_measure]).

n([pl(predikanten),sg(predikant)],de,[],
  [h(emeritus),
   gevangenis]).

n([pl(predikers),sg(prediker)],de,[]).

n([mass(prediking)],de,[]).

n([pl(preken),sg(preek)],de,[]).

n([pl(prefecten),sg(prefect),
   pl(prefekten),sg(prefekt)],de,[]).

n([sg(prefectuur),pl(prefecturen)],both,[],[sub]).

n([sg(preferentie),pl(preferenties)],de,[],[s(handel)]).

n([sg(prefix),pl(prefixen)],both,[]).

n([sg(prehistorie)],de,[]).

n([pl(preien),sg(prei)],de,[],[dim(preitje)]).

n([pl(prelaten),sg(prelaat)],de,[]).

n([sg(prelatuur),pl(prelaturen)],de,[]).

n([pl(preludes),sg(prelude)],de,[]).

n([pl(premies),sg(premie)],de,[app_measure]).

n([pl(premies),sg(premie)],de,[],
  [h('AOW'),h(aow),
   pensioen,
   vertrek,
   winst,
   ziekenfonds]).

n([pl(premiers),sg(premier)],de,[],
  [h(co),
   h(ex),
   h(oud),f([oud]),
   h(vice),vice]).

n([sg(premierschap),pl(premierschappen)],het,[]).

n([sg([premier,league])],de,[]).

n([pl([premiers,crus])],de,[]).

n([pl(premissen),sg(premisse)],de,[sbar]).

n([pl(premières),sg(première)],de,[pred_pp(in)],[wereld]).

n([pl(prenten),sg(prent)],de,[],[dim(prentje)]).

n([pl(preoccupaties),sg(preoccupatie)],de,[]).

n([pl(preokkupaties),sg(preokkupatie)],de,[]).

n([pl(preparaten),sg(preparaat)],het,[],
  [i(kruid,kruiden)]).

n([pl(presentaties),sg(presentatie)],de,[]).

n([pl(presentatoren),sg(presentator)],de,[],
  [h(tv),televisie,tv,f([tv]),i(tv,'TV-')
  ]).

n([pl(presentatrices),sg(presentatrice)],de,[],
  [h(tv),televisie,tv,f([tv]),i(tv,'TV-')]).

n([sg(preses),sg(praeses)],de,[],
  [h(ex),
   h(oud),oud,
   h(interim)]).

n([pl(presidenten),sg(president)],de,[],
  [bank,
   s(bond),
   h(ex),
   minister,h(minister),i(minister,'ministers-'),s(minister),f([minister]),
   h(oud),oud,
   rechtbank,
   vice,h(vice)]).

n([pl(presidentes),sg(presidente)],de,[],[h(ex)]).

n([pl(presidentschappen),sg(presidentschap)],het,[]).

n([sg(presidium),pl(presidiums),pl(presidia)],het,[]).

n([sg([presse,papier]),sg('presse-papier')],de,[]).

n([pl(pressies),sg(pressie)],de,[sbar,vp]).

n([pl(prestaties),sg(prestatie)],de,[subject_sbar,subject_vp],
  [richt,
   tegen,
   top,
   wieler]).

n([mass(prestige)],both,[]).

n([mass(pret)],de,
  [subject_vp],
  [dim(pretje),
   voor]).

n([sg(pretendent),pl(pretendenten)],de,[]).

n([pl(pretenties),sg(pretentie)],de,[sbar,vp]).

n([pl(preventies),sg(preventie)],de,[],
  [brand,
   conflict]).

n([sg(preview),pl(previews)],de,[]).

n([pl(prezentaties),sg(prezentatie)],de,[]).

n([pl(priëlen),sg(prieel)],het,[],[dim(prieeltje)]).

n([sg(priem),pl(priemen)],de,[]).

n([pl(priesters),sg(priester)],de,[]).

n([pl(priesteressen),sg(priesteres)],de,[]).

n([mass(priesterschap)],het,[]).

%% @ Maar de prijs was dat het Vaticaan Hitlers bewind erkende .
n([pl(prijzen),sg(prijs)],de,[subject_sbar],[]).

n([pl(prijzen),sg(prijs)],de,[app_measure],
  [s(aanmoediging),
   advies,
   afzet,
   benzine,
   i(boek,boeken),
   brandstof,
   bruto,
   i(consument,consumenten),
   consumptie,
   dag,
   diesel,
   energie,
   film,
   gas,
   s(gevatheid),
   goud,
   grond,
   grondstof,
   i(grondstof,grondstoffen),
   hoofd,
   i(huis,huizen),
   huur,
   inkoop,s(inkoop),
   inteken,
   introductie,
   kost,
   literatuur,
   markt,
   meer,
   melk,
   netto,
   olie,
   oeuvre,
   overname,
   s(publiek),
   i(producent,producenten),
   richt,
   s(schoonheid),
   s(toegang),
   troost,
   uitgifte,
   uitvoer,
   vanaf,
   verkoop,s(verkoop),
   voedsel,
   vraag,
   dim(prijsje)]).

n([mass(prijskompensatie)],de,[]).

n([sg(prijsstop),pl(prijsstops)],de,[]).

n([sg(prijsvraag),pl(prijsvragen)],de,[]).

n([pl(prikken),sg(prik)],de,
  [],
  [griep,
   i(rug,rugge),
   i(rug,ruggen)]).

n([sg(prik)],de,
  [subject_sbar], % het is vaste prik dat ...
  []).

n([pl(prikkelen),pl(prikkels),sg(prikkel)],de,[sbar,vp],[dim(prikkeltje)]).

n([mass(prikkelbaarheid)],de,[]).

n([pl(prikkelingen),sg(prikkeling)],de,[]).

n([sg([prima,ballerina])],de,[]).

n([sg(primas),pl(primassen)],de,[]). % van Dale: eerste violist in zigeunerorkest

n([pl(primaten),sg(primaat)],de,[]).

n([mass(primaat)],het,[]).

n([sg('prime-time')],de,[temp_mod,sbar]).

n([sg([prime,time])],de,[temp_mod,sbar]).

n([pl(primeurs),sg(primeur)],de,[sbar,vp],[dim(primeurtje)]).

n([sg([primeur,card]),pl([primeur,cards])],de,[]).

n([pl(principes),sg(principe)],het,
  [sbar,
   vp,
   subject_sbar,
   np_app_measure,
   start_app_measure
  ],
  [basis,h(basis),
   kern,
   s(voorzorg)]).

n([pl(princiepen),sg(princiep)],het,
  [sbar,
   vp,
   subject_sbar,
   np_app_measure,
   start_app_measure
  ],
  [basis,h(basis),
   kern,
   s(voorzorg)]).

n([pl(prinsen),sg(prins),sg('Prins')],de,[],
  [dim(prinsje),
   kroon]).

n([pl(prinsessen),sg(prinses),sg('Prinses')],de,[],
  [dim(prinsesje),
   kroon]).

n([sg(print),pl(prints)],de,[]).

n([pl(printers),sg(printer)],de,[]).

n([sg(prioriteit),pl(prioriteiten)],de,
  [subject_vp,subject_sbar],
  [hoofd,
   top]).

n([pl('prisma\'s'),pl(prismata),sg(prisma)],het,[]).

n([sg(privaat),pl(privaten)],het,[]).   % de privaten moeten naar seksen gescheiden

n([mass(privacy)],de,[]).

n([sg([private,banker]),pl([private,bankers])],de,[]).

n([pl(privatiseringen),sg(privatisering)],de,[]).

n([pl(privileges),sg(privilege)],het,[sbar,vp,subject_sbar,subject_vp]).

n([mass(privé)],both,[]).  % prive en openbaar; werk en prive

n([pl('pro\'s'),sg(pro)],de,[]).

n([sg([pro,forma]),pl([pro,'forma\'s']),
   sg('pro-forma'),pl('pro-forma\'s')],de,[]).

n([sg([pro,formazaak]),pl([pro,formazaken])],de,[]).

n([sg([pro,formazitting]),pl([pro,formazittingen])],de,[]).

n([pl(problemen),sg(probleem)],het,
  [pred_pp_pl(in),
   pred_pp_pl(uit),
   sbar,
   subject_sbar_no_het,
   vp],
  [dim(probleempje)]).

n([pl(problemen),sg(probleem)],het,[],
  [aanloop,
   s(aanpassing),
   ademhaling,s(ademhaling),
   s(betaling),
   s(capaciteit),
   drug,s(drug),
   s(gedrag),
   geld,
   s(gezondheid),
   hart,
   s(huwelijk),
   imago,
   s(liquiditeit),
   milieu,
   millennium,f([millennium]),
   s(personeel),
   relatie,
   taal,
   s(verkeer)]).

n([mass(probleemstelling)],de,[]).

n([mass(problematiek)],de,[]).

n([pl(procedures),sg(procedure)],de,
  [vp,
   sbar,
   subject_vp,
   subject_sbar],
  [standaard]).

n([pl(procedures),sg(procedure)],de,[],
  [s(aanbesteding),
   aanvraag,
   s(afzetting),
   asiel,
   s(begroting),
   s(bemiddeling),
   bodem,
   inschrijf,
   inspraak,
   i(klacht,klachten),
   s(medebeslissing),
   selectie,
   s(wetgeving)
  ]).

n([meas(procent),
   pl(procenten)],both,
  [meas_mod,
   measure],
  [s(gewicht),
   dim(procentje)]).

n([meas(percent),
   pl(percenten)],both,
  [meas_mod,
   measure],
  []).

n([meas('%')],both,
  [meas_mod,
   measure],
  []).

%%% % kinderen met overgewicht
n([sg('%')],both,
  [measure],
  []).

n([pl(processen),sg(proces)],het,
  [sbar,
   vp],
  [['8',december],  % van Bouterse
   s(arbeid),
   s(beleid),
   s(besluitvorming),
   s(communicatie),
   s(democratisering),
   denk,
   s(genezing),
   s(groep),
   integratie,
   leer,
   s(leven),
   s(onderhandeling),
   s(ontwikkeling),
   s(planning),
   productie,i(productie,produktie),
   proef,
   rouw,
   s(sterven),
   straf,
   s(uitbreiding),
   s(verandering),
   s(verwerking),
   s(vorming),
   s(vrede),
   ziekte]).

n([pl('processen-verbaal'),sg('proces-verbaal'),
   pl(processenverbaal),   sg(procesverbaal),
   pl([processen,verbaal]), sg([proces,verbaal])],het,[]).

n([pl(processies),sg(processie)],de,[]).

n([pl(processors),sg(processor)],de,[]).

n([pl(proclamaties),sg(proclamatie)],de,[sbar]).

n([pl(procureurs),sg(procureur)],de,[app_measure]).

n([pl('procureurs-generaal'),sg('procureur-generaal'),sg('PG'),pl('PG\'s')],de,[]).

n([pl(procédés),
   pl('procédé\'s'),
   sg(procédé)],het,[sbar,vp]).

n([pl(producenten),sg(producent)],de,[],
  [auto,
   bier,
   film,
   kurk,
   muziek,
   software,
   stroom,
   zuivel]).

n([pl(producers),sg(producer)],de,[]).

n([pl(producten),sg(product),
   pl(produkten),sg(produkt)],het,[app_measure],
  [bij,
   i(consument,consumenten),
   s(belegging),
   eind,
   kip,
   landbouw,
   metaal,
   olie,
   textiel,
   visserij,
   zuivel]).

n([pl(producties),sg(productie),
   pl(produkties),sg(produktie)],de,[pred_pp(in)],
  []).

n([pl(producties),sg(productie),
   pl(produkties),sg(produktie)],de,[],
  [co,
   massa,h(massa),
   olie,
   over,
   waren]).

n([mass(productiviteit),
   mass(produktiviteit)],de,[],
  [s(arbeid)]).

n([pl(productschappen),sg(productschap),
   pl(produktschappen),sg(produktschap)],het,[app_measure]).

n([pl(proeven),sg(proef),sg(proeve)],de,[]).

n([pl(proeven),sg(proef)],de,[],
  [atoom,
   dier,
   door,
   geur,
   kern,
   vuur]).

n([pl(proefboringen),sg(proefboring)],de,[]).

n([pl(proefdieren),sg(proefdier)],het,[]).

n([pl(proefnemingen),sg(proefneming)],de,[sbar],[dim(proefneminkje)]).

n([pl(proefpersonen),sg(proefpersoon)],de,[]).

n([pl(proefschriften),sg(proefschrift)],het,[]).

n([mass(proeftijd)],de,[]).

n([pl(proefwerken),sg(proefwerk)],het,
  [app_measure]).

n([sg(proever),pl(proevers)],de,[]).

n([sg(proeverij),pl(proeverijen)],de,[]).

n([pl(proffen),pl(profs),sg(prof)],de,[]).

n([pl(profeten),sg(profeet)],de,[],[radio,televisie]).

n([pl(professies),sg(professie)],de,[]).

n([pl(professionals),sg(professional)],de,[],[zorg]).

n([mass(professionaliteit)],de,[]).

n([mass(professionalisering),mass(professionalizering)],de,[]).

n([pl(professoren),pl(professors),sg(professor)],de,[app_measure]).

n([pl(profetieën),sg(profetie)],de,[sbar]).

n([sg(proficiat)],both,[]).

n([pl(profielen),sg(profiel)],het,[],
  [h('DNA'),
   dim(profieltje)]).

n([sg(profile),pl(profile)],both,[]).

n([pl(profijten),sg(profijt)],het,[sbar]).

n([pl(profileringen),sg(profilering)],de,[]).

n([pl(profiteurs),sg(profiteur)],de,[]).

n([mass(progesteron)],both,[]).

n([pl(prognoses),sg(prognose)],de,[sbar]).

n([pl(prognoses),sg(prognose)],de,[],
  [winst]).

n([pl(programs),sg(program)],het,[],
  [s(verkiezing)]).

n([pl('programma\'s'),sg(programma)],het,
  [app_measure,
   np_app_measure,
   start_app_measure],
  [actie,
   i(actualiteit,actualiteiten),
   beginsel,
   s(beleid),
   i(boek,boeken),
   computer,
   kader,
   kernwapen,
   i(kind,kinder),
   les,
   meerjaren,
   i(koop_op,opkoop),
   partij,
   praat,
   radio,
   reis,
   steun,
   televisie,f([televisie]),
   tv,h(tv),f([tv]),i(tv,'TV-'),
   s(verkiezing),
   voetbal,
   voor,
   wapen,
   werk,
   dim(programmaatje)]).

n([pl(programmamakers),sg(programmamaker)],de,[]).

n([mass(programmatuur)],de,[]).

n([mass(programmering),pl(programmeringen),sg(programmering)],de,[]).

n([pl(programmeurs),sg(programmeur)],de,[]).

n([pl(progressies),sg(progressie)],de,[sbar]).

n([pl(projecten),sg(project),
   pl(projekten),sg(projekt)],het,
  [app_measure],
  [bouw,
   s(omscholing),
   s(ontwikkeling),
   proef,
   s(samenwerking),
   web,f([web])]).

n([pl(projecties),sg(projectie)],de,[]).

n([pl(projectielen),sg(projectiel)],het,[],[dim(projectieltje)]).

n([pl(projectontwikkelaars),sg(projectontwikkelaar),
   pl(projektontwikkelaars),sg(projektontwikkelaar)],de,[]).

n([pl(projectors),sg(projector),pl(projectoren)],de,[]).
%% niet project_oor

n([pl(proleten),sg(proleet)],de,[]).

n([mass(proletariaat)],het,[]).

n([pl(proletariërs),sg(proletariër)],de,[]).

n([pl(prologen),sg(proloog)],de,[]).

n([sg(prolongatie),pl(prolongaties)],de,[]).

n([pl(promenades),sg(promenade)],de,[]).

n([meas(promille),pl(promillen),
   meas([pro,mille])],both,
  [meas_mod,
   measure]).

n([sg(promillage),pl(promillages)],het,
  [measure]).

n([sg(promillage),pl(promillages)],het,
  [],
  [alcohol]).

n([pl(promoties),sg(promotie)],de,[]).

n([pl(promotoren),pl(promotors),sg(promotor),
   pl(promotores)],de,[]).

n([pl(promovendi),sg(promovendus)],de,[],[beurs]).

n([mass(pronk)],de,[]).

n([pl(pronkstukken),sg(pronkstuk)],het,[]).

n([sg(pronomen),pl(pronomina)],het,[]).

n([pl(prooien),sg(prooi)],de,[],[dim(prooitje)]).

n([sg(proosdij)],de,[]).

n([sg(proost),pl(proosten)],de,[]).

n([pl(proppen),sg(prop)],de,[measure],[dim(propje)]).

n([mass(propaganda)],de,[]).

n([sg(propedeuse),pl(propedeuses),
   sg(propaedeuse),pl(propaedeuses)],de,[app_measure]).

n([pl(propagandisten),sg(propagandist)],de,[]).

n([pl(propellers),sg(propeller)],de,[],[dim(propellertje)]).

n([pl(proporties),sg(proportie)],de,
  [pred_pp(buiten),
   pred_pp_pl(buiten)]).

n([pl(proposities),sg(propositie)],de,[vp]).

% knappe jongens en meisjes die klanten een discotheek in moeten lokken
n([sg(propper),pl(proppers)],de,[]). 

n([sg(prospectus),pl(prospectussen)],both,[]).

n([sg(prostaat),pl(prostaten)],de,[]).

n([pl(prostituees),sg(prostituee)],de,[]).

n([mass(prostitutie)],de,[],[tippel]).

n([pl(prostituées),sg(prostituée)],de,[]).

n([mass(protectie)],de,[]).

n([mass(protectionisme)],het,[]).

n([sg(protectoraat),pl(protectoraten)],het,[]).

n([sg(protegé),pl(protegés)],de,[]).

n([pl(protesten),sg(protest)],het,[]).

n([pl(protestanten),sg(protestant)],de,[]).

n([mass(protestantisme)],het,[]).

n([pl(prothesen),pl(protheses),sg(prothese)],de,[]).

n([pl(protocollen),sg(protocol)],het,[],[h('Kyoto')]).

n([pl(protonen),sg(proton)],het,[]).

n([pl(prototypen),pl(prototypes),sg(prototype)],het,[app_measure]).

n([mass(proviand)],de,[]).

n([sg(provider),pl(providers)],de,[]).

n([sg(provinciaal),pl(provincialen)],de,[]).

n([pl(provincies),pl(provinciën),sg(provincie)],de,[],
  [kerk,
   kust,
   dim(provincietje)]).

n([pl(provisies),sg(provisie)],de,[]).

n([pl('provo\'s'),sg(provo)],de,[]).

n([pl(provocaties),sg(provocatie)],de,[vp]).

n([sg(proxy),pl(proxies)],de,[]).

n([mass(proza)],het,[]).

n([pl(pruiken),sg(pruik)],de,[]).

n([pl(pruimen),sg(pruim)],de,[],[dim(pruimpje)]).

n([pl(prullen),sg(prul)],het,[],[dim(prulletje)]).

n([sg(prullenbak),pl(prullenbakken)],de,[]).

n([pl(prullenmanden),sg(prullenmand)],de,[]).

n([pl(psalmen),sg(psalm)],de,[],[dim(psalmpje)]).

n([pl(pseudoniemen),sg(pseudoniem)],het,[]).

n([mass(psoriasis)],de,[]).

n([mass(psyche)],de,[]).

n([pl(psychiaters),sg(psychiater)],de,[]).

n([mass(psychiatrie)],de,[]).

n([pl(psychoanalysen),pl(psychoanalyses),sg(psychoanalyse)],de,[]).

n([pl(psychoanalytici),sg(psychoanalyticus)],de,[]).

n([sg(psychologe)],de,[]).

n([mass(psychologie)],de,[]).

n([pl(psychologen),sg(psycholoog)],de,[],[neuro]).

n([pl(psychopaten),sg(psychopaat)],de,[]).

n([pl(psychosen),pl(psychoses),sg(psychose)],de,[]).

n([pl(pubs),sg(pub)],de,[]).

n([pl(pubers),sg(puber)],de,[]).

%% Ik heb mijn hele jeugd en puberteit naar Japan terugverlangd .
n([mass(puberteit)],de,[temp_mod]).

n([pl([public,relations])],de,[]).

n([pl([public,'relations-adviseurs']),sg([public,'relation-adviseur'])],de,[]).

n([pl(publicaties),sg(publicatie)],de,[]).

n([pl(publicisten),sg(publicist)],de,[]).

n([mass(publiciteit)],de,[]).

n([mass(publiek)],het,[],
  [televisie]).

n([stem(publicatie),pl(publikaties),sg(publikatie)],de,[]).

n([pl(puddingen),pl(puddings),sg(pudding)],de,[],[dim(puddinkje)]).

n([mass(puf)],de,[]).   % ik heb geen puf meer

n([pl(puien),sg(pui)],de,[],
  [schuif]).

n([mass(puin)],both,[pred_pp(in)]).

n([pl(puinhopen),sg(puinhoop)],de,[]).

n([pl(puisten),sg(puist)],de,[],[dim(puistje)]).

n([pl(pukken),sg(puk)],de,[]).

n([pl(pukkels),sg(pukkel)],de,[],[dim(pukkeltje)]).

n([pl(pullen),sg(pul)],de,[measure],[dim(pulletje)]).

n([pl(pullovers),sg(pullover)],de,[]).

n([mass(pulp)],both,[]).

n([pl(punaises),sg(punaise)],de,[]).

n([mass(punch)],de,[]).

n([sg(punctie),pl(puncties)],de,[]).

n([mass(punctualiteit)],de,[]).

n([sg(punk),pl(punks)],de,[]).

n([sg(punker),pl(punkers)],de,[]).

n([bare_meas(punt),pl(punten)],both,
  [meas_mod,
   measure],
  [basis,
   ijk,
   licht,
   procent,
   straf,
   studie,
   verlies,
   wedstrijd,
   winst,
   dim(puntje)]).

n([sg(punt),pl(punten),
   ignore(m(punt,noun(both,count,sg),punt)),
   ignore(m(punt,noun(both,count,pl),punten))],both,
  [pred_pp(op)
  ],
  [diepte,
   hoogte,
   omslag,
   top
  ]
 ).

n([sg(punt),pl(punten),
   ignore(m(punt,noun(both,count,sg),punt)),
   ignore(m(punt,noun(both,count,pl),punten)),
   ignore(m(punt,noun(both,count,sg,pred_pp(op)),punt)),
   ignore(m(punt,noun(both,count,pl,pred_pp(op)),punten))
  ],both,
  [sbar,
   subject_sbar,
   subject_vp
  ],
  [s(aandacht),
   s(aangrijping),
   aanspreek,
   aansluit,			% en niet aan_luit_punt
   actie,
   begin,
   breek,
   diepte,
   discussie,
   eind,
   geschil,
   hoofd,
   hoogte,
   keer,
   kern,
   knel,
   licht,
   min,
   omslag,
   speer,
   start,
   top,
   s(verlichting),
   vertrek,
   zwaarte,
   dim(puntje)]).

n([sg(punter),pl(punters)],de,[]).

n([pl(pups),sg(pup)],de,[]).

n([pl(puppies),sg(puppy),sg(puppie)],de,[]).

n([pl(pupillen),sg(pupil)],de,[]).

n([mass(puree)],de,[],[aardappel]).

n([pl(puriteinen),sg(puritein)],de,[]).

n([mass(purper)],het,[]).

n([sg(push),pl(pushes)],de,[]).

n([pl(putten),sg(put)],de,[],
  [beer,
   bouw,
   gier]).

n([sg(putto),pl(putti)],de,[]).

n([pl(puzzels),sg(puzzel),
   pl(puzzles),sg(puzzle)],de,[],
  [leg,
   dim(puzzeltje)]).

n([pl(puzzels),sg(puzzel),
   pl(puzzles),sg(puzzle)],de,[sbar,vp],[dim(puzzeltje)]).

n([mass(pvc),mass('PVC'),pl('PVC\'s')],both,[]).

n([pl('pyjama\'s'),sg(pyjama)],de,[],[dim(pyjamaatje)]).

n([sg(pyromaan),pl(pyromanen)],de,[]).

n([pl(pâtés),
   pl('pâté\'s'),
   sg(pâté)],de,[]).

n([mass(père)],de,[]).

n([pl('q\'s'),sg(q)],de,[],[dim('q\'tje')]).

n([mass(qat)],de,[]).

n([sg('QI')],both,[]).  % index voor lichaamsgewicht/lengte

n([pl(quadraten),sg(quadraat)],het,[]).

n([sg(quaestor),pl(quaestoren)],de,[]).

n([sg(qualifier),pl(qualifiers)],de,[]).

n([sg(quantificatie),pl(quantificaties)],de,[]).

n([mass(quarantaine)],de,[pred_pp(in)]).

n([sg(quark),pl(quarks)],both,[]).

n([sg([qui,vive])],both,[pred_pp(op)]).

n([pl(quinten),sg(quint)],de,[]).

n([bare_meas(quintiljoen),pl(quintiljoenen)],both,[meas_mod,measure]).

n([sg(quiz),pl(quizzen)],de,[],  % niet quiz_zen
  [televisie]).

n([pl(quota),sg(quotum)],het,[],
  [kabeljauw,
   melk]).

n([sg(quote),pl(quotes)],de,[]).

n([pl('r\'s'),sg(r)],de,[],[dim('r\'etje')]).

n([pl(raden),sg(raad)],de,[sbar,vp]).

n([pl(raden),sg(raad)],de,[app_measure],
  [advies,
   s(bestuur),
   s(bond),
   i(cliënt,cliënten),
   deel,
   s(dorp),
   gemeente,
   jeugd,
   i(kerk,kerken),i(kerk,kerke),
   kies,
   i(lid,leden),
   minister,
   s(onderneming),
   partij,
   programma,
   provincie,
   redactie,
   s(regering),
   stadsdeel,
   tucht,
   s(universiteit),
   s(veiligheid),
   wijk]).

n([pl(raadgevers),sg(raadgever)],de,[]).

n([pl(raadgeefsters),sg(raadgeefster)],de,[]).

n([pl(raadgevingen),sg(raadgeving)],de,[sbar,vp]).

n([pl(raadhuizen),sg(raadhuis)],het,[]).

n([pl(raadjes),pl(radertjes),pl(radjes),
   sg(raadje),sg(radje),sg(radertje)],het,[]).

n([pl(raadpensionarissen),sg(raadpensionaris)],de,[]).

n([pl(raadplegingen),sg(raadpleging)],de,[],[s(volk)]).

n([pl(raadselen),pl(raadsels),sg(raadsel)],het,
  [app_measure, % het grote raadsel pi
   sbar,
   subject_sbar],
  [dim(raadseltje)]).

n([pl(raadselen),pl(raadsels),sg(raadsel)],het,
  [],
  [kruiswoord,
   dim(raadseltje)]).

n([pl(raadsheren),sg(raadsheer)],de,[]).

n([pl(raadslieden),sg(raadsman)],de,[]).

n([pl(raadszalen),sg(raadszaal)],de,[]).

n([pl(raadzalen),sg(raadzaal)],de,[]).

n([pl(raven),sg(raaf)],de,[]).

n([pl(raakvlakken),sg(raakvlak)],het,[]).

n([pl(ramen),sg(raam)],both,[],
  [keuken,
   i(slaap_kamer,slaapkamer),
   dim(raampje)]).		% VL: de raam

n([pl(raamkozijnen),sg(raamkozijn)],het,[]).

n([pl(rapen),sg(raap)],de,[]).

n([mass(rabarber)],de,[]).

n([pl('rabbi\'s'),sg(rabbi)],de,[]).

n([pl(rabbijnen),sg(rabbijn)],de,[]).

n([sg(rabbinaat),pl(rabbinaten)],het,[]).

n([mass(rabiës)],de,[]).

n([pl(races),sg(race)],de,[measure,temp_mod,sbar,pred_pp(in)],
  []).

n([pl(races),sg(race),
   ignore_stem(race)],de,[],
  [wh(['Formule','1']),
   inhaal                     % races schorsing ???
  ]).

%% niet race_rij
n([sg(racerij)],de,[],[auto]).

n([mass(racisme)],het,[]).

n([sg(racist),pl(racisten)],de,[]).

n([sg(racket),pl(rackets)],both,[]).

n([pl(raderen),sg(rad)],het,[]).

n([pl(radars),sg(radar)],de,[]).

n([sg(raddraaier),pl(raddraaiers)],de,[]).

n([mass(radeloosheid)],de,[]).

n([pl(radens),sg(raden)],de,[]).

n([pl(radiatoren),pl(radiators),sg(radiator)],de,[]).

n([pl(radicalen),sg(radicaal)],both,[]).

n([mass(radicalisering)],de,[]).

n([mass(radicalisme)],het,[]).

n([sg(radijs),pl(radijzen)],de,[],[dim(radijsje)]).

n([pl('radio\'s'),sg(radio)],de,[],
  [auto,
   s(staat),
   dim(radiootje)]).

n([mass(radioactiviteit),mass(radioaktiviteit)],de,[]).

n([sg(radioloog),pl(radiologen)],de,[]).

n([sg(radioluisteraar),pl(radioluisteraars)],de,[]).

n([sg(radon)],both,[]).

n([pl(rafels),sg(rafel)],de,[],[dim(rafeltje)]).

n([pl(raffinaderijen),sg(raffinaderij)],de,[]).

n([sg(raffinage)],de,[]).

n([mass(raffinement)],het,[]).

n([pl(rages),sg(rage)],de,
  [subject_sbar,
   subject_vp]).

n([pl(rails),sg(rail)],de,[],[vang]).

n([sg(raïs)],de,[]).  % Arabische bestuurder

n([mass([raison,d,'\'être']),
   mass([raison,'d\'','être']),
   mass([raison,'d\'être'])],de,[]).

n([pl(raketten),sg(raket)],de,[],[kruis]).

n([pl(rakkers),sg(rakker)],de,[],[dim(rakkertje)]).

n([sg(rally),pl('rally\'s')],de,[]).

n([meas(ram)],de,[meas_mod,measure]).

n([pl(rammen),sg(ram)],de,[],[dim(rammetje)]).

n([sg(ramadan),pl(ramadans)],de,[temp_mod,sbar]).

n([pl(ramingen),sg(raming)],de,[vp,sbar]).

n([pl(rammels),sg(rammel)],de,[],[dim(rammeltje)]).

n([pl(rampen),sg(ramp)],de,
  [subject_sbar]).

n([pl(rampen),sg(ramp)],de,[],
  [h('Bijlmer'),
   milieu,
   natuur,
   s(scheep),
   trein,
   vlieg,
   vuurwerk,
   watersnood]).

n([pl(rampspoeden),sg(rampspoed)],de,[sbar,vp]).

n([mass(ramsj)],de,[]).

n([pl(ranches),sg(ranch)],de,[]).

n([pl(rancunes),sg(rancune)],de,[]).

n([meas(rand),pl(rands)],de,[meas_mod,measure]).

n([pl(randen),sg(rand)],de,[],
  [s(stad),
   dim(randje)]).

n([pl(randsteden),sg(randstad)],de,[]).

n([pl(rangen),sg(rang)],de,[]).

n([pl(ranglijsten),sg(ranglijst)],de,[app_measure],[wereld]).

n([mass(rangorde)],de,[]).

n([pl(rangschikkingen),sg(rangschikking)],de,[]).

n([pl(ranken),sg(rank)],de,[]).

n([sg(ranking),pl(rankings)],de,[]).

n([pl(ransels),sg(ransel)],de,[],[dim(ranseltje)]).

n([pl(rantsoenen),sg(rantsoen)],het,[measure],[dim(rantsoentje)]).

n([pl(raps),sg(rap)],de,[]).

n([pl(rappers),sg(rapper)],de,[]).

n([pl(rapporten),sg(rapport)],het,[np_app_measure],
  [i(baan,banen),
   s(beoordeling),
   eind,
   jury,
   h('NIOD'),h('Niod'),
   s(onderzoek),
   politie
  ]).

n([pl(rapportages),sg(rapportage)],de,[],
  [i(milieu_effect,'milieu-effect')]).

%% erg lelijk woord:
n([sg(rapportering),pl(rapporteringen)],de,[]).

n([pl(rapporteurs),sg(rapporteur)],de,[],[schaduw]).  % europarl

%VL: haast
n([sg(rapte)],de,[]).

%% er bestaan vele rassen rijst
n([pl(rassen),sg(ras)],het,[measure],
  []).

n([pl(rassen),sg(ras)],het,[app_measure],
  [i(hond,honden),
   i(kat,katten)]).

n([pl(rashonden),sg(rashond)],de,[]).

n([sg(rasp),pl(raspen)],de,[]).

n([sg(raster),pl(rasters)],het,[]).

n([mass(ratatouille)],de,[]).

n([sg(ratel),pl(ratels)],de,[]).

n([pl(ratten),sg(rat)],de,[],
  [water,
   muskus]).			% geen mus_kus_rat

n([pl(ratificaties),sg(ratificatie)],de,[]).

n([sg(ratificering),pl(ratificeringen)],de,[]).

n([pl(ratifikaties),sg(ratifikatie)],de,[]).

n([sg(rating),pl(ratings)],de,[]).

n([mass(ratio)],de,[subject_sbar]).

n([pl(rationalisaties),sg(rationalisatie),
   pl(rationalizaties),sg(rationalizatie)],de,[vp]).

n([mass(rationalisering),mass(rationalizering)],de,[]).

n([mass(rationalisme)],het,[]).

n([pl(rationalisten),sg(rationalist)],de,[]).

n([mass(rationaliteit)],de,[]).

n([mass(rauwkost)],de,[]).

n([pl(ravages),sg(ravage)],de,[]).

n([pl(ravijnen),sg(ravijn)],both,[]). % VL: de

n([pl(razernijen),sg(razernij)],de,[]).

n([pl('razzia\'s'),sg(razzia)],de,[]).

n([pl('re\'s'),sg(re)],de,[]).

n([pl(reacties),sg(reactie),pl(reakties),sg(reaktie)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp,
   start_app_measure],
  [standaard]).

n([pl(reacties),sg(reactie),pl(reakties),sg(reaktie)],de,
  [],
  [ketting]).

n([sg(reactionair),pl(reactionairen)],de,[]).

n([sg(reactor),pl(reactoren)],de,[],[kern]).

n([sg(reader),pl(readers)],de,[]).

%% brasil
n([meas(real),pl(reals)],de,[meas_mod,measure]).

n([mass([real,life]),mass('real-life')],de,[]).

n([sg([real,life,soap]),sg(['real-life',soap]),sg('real-life-soap'),
   pl([real,life,soaps]),pl(['real-life',soaps]),pl('real-life-soaps')],
  de,[]).

n([sg([real,life,tv]),mass(['real-life',tv]),mass('real-life-tv')],
  de,[]).

n([mass([real,time])],de,[]).  % binnen/in real time

n([pl(realisaties),sg(realisatie),pl(realizaties),sg(realizatie)],de,[]).

n([mass(realisering),mass(realizering)],de,[]).

n([mass(realisme)],het,[]).

n([pl(realisten),sg(realist)],de,[],
  []).

n([pl(realiteiten),sg(realiteit)],de,[sbar,vp]).

n([mass('real-time'),mass(realtime)],both,[]).

n([sg(rebbe)],de,[]).

n([pl(rebellen),sg(rebel)],de,[],
  [moslim]).

n([pl(rebellieën),sg(rebellie)],de,[]).

n([sg(rebound),pl(rebounds)],de,[]).

n([mass([rebound,ace])],both,[]). %% type tennisbaan ondergrond

n([sg(recapitulatie),pl(recapitulaties)],de,[]).

n([pl(recensenten),sg(recensent)],de,[],
  [film]).

n([pl(recensies),sg(recensie)],de,[],
  [film]).

n([pl(recepten),sg(recept)],het,[]).

n([pl(recepties),pl(receptiën),sg(receptie)],de,[],[s(nieuwjaar)]).

n([pl(receptionisten),sg(receptionist)],de,[]).

n([sg(receptioniste),pl(receptionistes)],de,[]).

n([sg(receptor),pl(receptoren)],de,[]). % geen recept_oor

n([sg(reces)],het,[],[zomer]).

n([mass(recessie)],de,[]).

n([sg(recette),pl(recettes)],de,[]).

n([pl(recherches),sg(recherche)],de,[],[s(rijk)]).

n([pl(rechercheurs),sg(rechercheur)],de,[]).

n([mass(recht),pl(rechten),sg(recht)],het,
  [sbar,
   subject_sbar,
   subject_vp,
   vp],
  [alleen,
   grond,
   i(mens,mensen)]).

n([mass(recht),pl(rechten),sg(recht)],het,[],
  [[ad,valorem],
   s(arbeid),
   asiel,
   s(auteur),
   s(bestaan),
   s(bestuur),
   burger,
   i(consument,consumenten),
   douane,
   s(eigendom),
   emissie,
   energie,
   erf,
   film,
   i(versterf_erf,versterferf),
   s(gemeenschap),
   gewoonte,
   s(gezag),
   s(herroeping),
   insolventie,
   invoer,
   kies,
   s(landing),
   s(omgang),
   s(omgeving),
   s(oorlog),
   pensioen,
   privaat,
   publiek,
   publicatie,
   riool,
   i(schip,scheeps),
   snel,
   s(staat),
   s(stad),
   stam,
   stem,
   straf,
   successie,
   h(tv),tv,televisie,f([tv]),i(tv,'TV-'),
   uitzend,
   s(varken),
   i(verbintenis,verbintenissen),
   s(verschoning),
   veto,
   voetbal,
   i(volk,volken),
   i(volk,volkeren),
   i(vreemdeling,vreemdelingen),
   i(vrouw,vrouwen),
   s(zelfbeschikking),
   zwijg]).

n([pl(rechtbanken),sg(rechtbank)],de,[]).

n([sg(rechte)],de,[]).

n([pl(rechters),sg(rechter)],de,[],
  [kamp,
   jeugd,
   i(kind,kinder),
   s(onderzoek),
   opper,
   straf
  ]).

n([pl(rechterkanten),sg(rechterkant)],de,[]).

n([pl(rechtervleugels),sg(rechtervleugel)],de,[]).

n([mass(rechtmatigheid)],de,[]).

n([mass(rechts)],het,[],
  [extreem,h(extreem),
   ultra,h(ultra)
  ]).

n([sg(rechtsachter),pl(rechtsachters)],de,[]).

n([pl(rechtsbacks),sg(rechtsback)],de,[]).

n([sg(rechtsbinnen),pl(rechtsbuitens)],de,[]).

n([sg(rechtsbuiten),pl(rechtsbuitens)],de,[]).

n([pl(rechtsbeginselen),pl(rechtsbeginsels),sg(rechtsbeginsel)],het,[sbar]).

n([sg(rechtsmidden),pl(rechtsmiddens)],de,[]).

n([mass(rechtsbescherming)],de,[]).

n([mass(rechtsbewustzijn)],het,[]).

n([mass(rechtsbijstand)],de,[]).

n([mass(rechtsgeleerdheid)],de,[]).

n([pl(rechtsgevolgen),sg(rechtsgevolg)],het,[]).

n([sg(rechtshalf),pl(rechtshalven)],de,[]).

n([pl(rechtshandelingen),sg(rechtshandeling)],de,[]).

n([mass(rechtsorde)],de,[]).

n([pl(rechtspersonen),sg(rechtspersoon)],de,[]).

n([mass(rechtspersoonlijkheid)],de,[]).

n([mass(rechtspleging)],de,[]).

n([mass(rechtspraak)],de,[],[jury]).

n([pl(rechtsregelen),pl(rechtsregels),sg(rechtsregel)],de,[sbar]).

n([mass(rechtsvinding)],de,[]).

n([sg(rechtsvoor),pl(rechtsvoors)],de,[]).

n([pl(rechtsvorderingen),sg(rechtsvordering)],de,[]).

n([pl(rechtswetenschappen),sg(rechtswetenschap)],de,[]).

n([pl(rechtszalen),sg(rechtszaal),sg(rechtzaal),pl(rechtzalen)],de,[]).

n([mass(rechtszekerheid)],de,[]).

n([mass(rechtvaardigheid)],de,[]).

n([mass(rechtvaardiging),pl(rechtvaardigingen),sg(rechtvaardiging)],de,[sbar]).

n([sg(rechtszaak),pl(rechtszaken),
   sg(rechtzaak),pl(rechtzaken)],de,[]).

n([mass(recidive)],de,[]).

n([pl(recitals),sg(recital)],het,[],[piano]).

n([pl(reclames),sg(reclame)],de,[],
  [sluik,
   s(tabak),
   h(tv)]).

n([mass(reclassering)],de,[]).

n([pl(reconstructies),sg(reconstructie)],de,[]).

n([pl(records),sg(record)],both,
  [app_measure],
  [baan,
   club,
   wereld,
   i(wereld_uur,werelduur)]).

n([pl(recorders),sg(recorder)],de,[]).

n([sg(recordhouder),pl(recordhouders)],de,[app_measure]).

n([sg(recreant),pl(recreanten)],de,[]).

n([pl(recreaties),sg(recreatie)],de,[]).

n([pl(recruten),sg(recruut)],de,[]).

n([pl(rectificaties),sg(rectificatie)],de,[]).

n([sg(rectiole),pl(rectiolen)],de,[]).

n([pl(rectoren),pl(rectors),sg(rector)],de,[]).

n([sg([rector,magnificus])],de,[]).

n([mass(recycling)],de,[]).

n([sg([red,light,district]),pl([red,light,districts])],het,[]).

n([mass([red,mercury])],de,[]).

n([sg([red,snapper]),pl([red,snappers])],de,[]).  % vissoort

n([pl(redacteuren),pl(redacteurs),sg(redacteur),
   pl(redakteuren),pl(redakteurs),sg(redakteur)],de,
  [app_measure],
  [binnenland,
   buitenland,
   eind,
   hoofd,
   kunst,
   sport,
   televisie]).

n([pl(redacties),sg(redactie),pl(redakties),sg(redaktie)],de,
  [app_measure],
  [binnenland,
   buitenland,
   eind,
   hoofd,
   kunst,
   sport,
   televisie]).

n([sg(redactioneel),sg(redaktioneel)],het,[]).

n([sg(redactrice),pl(redactrices)],de,[app_measure]).

n([pl(redders),sg(redder)],de,[]).

n([pl(reddingen),sg(redding)],de,
  [subject_sbar]).

n([pl(redetwisten),sg(redetwist)],de,[]).

n([pl(reden),pl(redes),sg(rede)],de,[],
  [s(afscheid),
   troon,
   voor  % ouderwets =inleiding?
  ]).

n([mass(redelijkheid)],de,[]).

%% sbar + subject_sbar at the same time!
%% de enige reden dat hij komt is , dat ...
%% de enige reden om te komen is , dat ...
%% de enige reden dat hij komt is , om ...
%% de enige reden om te komen is , om ...
n([pl(redenen),sg(reden)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp,
   subject_sbar_sbar,
   subject_sbar_vp,
   subject_vp_sbar,
   subject_vp_vp],
  [drog,
   hoofd]).

n([pl(redenen),sg(reden)],de,[],
  [s(gezondheid),
   s(veiligheid)]).

n([pl(redenaars),sg(redenaar)],de,[]).

n([sg(redenatie),pl(redenaties)],de,[sbar,subject_sbar]).

n([pl(redeneringen),sg(redenering)],de,[sbar,subject_sbar]).

n([pl(reders),sg(reder)],de,[]).

n([pl(rederijen),sg(rederij)],de,[]).

n([sg(rederijker),pl(rederijkers)],de,[]).

n([pl(redevoeringen),sg(redevoering)],de,[]).

n([sg(redirect),pl(redirects)],de,[]).

n([mass([reduced,instruction,set,computing])],both,[]).

n([pl(reducties),sg(reductie)],de,[],[tekort]).

n([pl(reeën),sg(ree)],both,[],[dim(reetje)]).

n([pl(reeksen),sg(reeks)],de,[measure]).

n([pl(reeksen),sg(reeks)],de,[],
  [kwalificatie,
   strip,
   televisie,
   zege]).

n([pl(repen),sg(reep)],de,[measure],[dim(reepje)]).

n([pl(repen),sg(reep)],de,[],
  [chocolade,
   dim(reepje)]).

n([pl(reten),sg(reet)],de,[],[dim(reetje)]).

n([pl(referenda),pl(referendums),sg(referendum)],het,[]).

n([pl(referenties),pl(referentiën),sg(referentie)],de,[]).

n([pl(reflecties),sg(reflectie)],de,[sbar]).

n([pl(reflexen),sg(reflex)],de,[sbar,vp,subject_sbar,subject_vp]).

n([pl(reflexies),sg(reflexie)],de,[sbar]).

n([pl(reformaties),sg(reformatie)],de,[]).

n([pl(refreinen),sg(refrein)],het,[],[dim(refreintje)]).

n([pl(regeerders),sg(regeerder)],de,[]).

n([pl(regelen),pl(regels),sg(regel)],de,
  [start_app_measure,
   measure,
   sbar, subject_sbar,
   vp],
  [s(beleid),
   s(begroting),
   beurs,
   boekhoud,
   s(gebruik),
   hoofd,
   huis,
   milieu,
   privacy,
   straf,
   dim(regeltje)]).

n([sg(regelaar),pl(regelaars)],de,[]).

n([pl(regelgevingen),sg(regelgeving)],de,[sbar,vp]).

n([pl(regelingen),sg(regeling)],de,
  [app_measure,
   sbar,
   vp]).

n([pl(regelingen),sg(regeling)],de,[],
  [s(afvloeiing),
   amnestie,
   combi,
   compensatie,
   dienst,
   levensloop,
   s(omgang),
   opkoop,
   optie,
   i(oud,ouderen),
   s(overgang),
   pardon,
   pensioen,
   prepensioen,
   i(spaar_loon,spaarloon),
   schade,
   steun,
   subsidie,
   verlof,
   vertrek,
   s(vrede),
   h('VUT'),i('VUT','vut-'),i('VUT',vut),
   wachtgeld,
   witwas]).

n([mass(regelmaat)],de,[]).

n([pl(regens),sg(regen)],de,[measure]).

n([pl(regens),sg(regen)],de,[],[moesson]).

n([pl(regenbogen),sg(regenboog)],de,[]).

n([pl(regendruppels),sg(regendruppel)],de,[]).

n([pl(regenten),sg(regent)],de,[]).

n([sg(regentaat),pl(regentaten)],both,[]).

n([sg(regentschap),pl(regentschappen)],het,[]).

n([mass(regenwater)],het,[]).

n([pl(regenwouden),sg(regenwoud)],het,[]).

n([pl(regeringen),sg(regering)],de,[],
  [post_h('Aznar'),
   post_h('Berlusconi'),
   post_h('Blair'),
   post_h('Bush'),
   post_h('Clinton'),
   coalitie,
   deel,
   s(eenheid),
   h(interim),interim,
   post_h('Jospin'),
   s(minderheid),
   s(overgang),
   post_h('Schröder'),
   post_h('Sharon'),
   post_h('Wijdenbosch')
  ]).

n([pl(regeringstroepen)],de,[]).

n([pl(regeringsverklaringen),sg(regeringsverklaring)],de,[]).

n([mass(reggae)],de,[]).

n([pl(regies),sg(regie)],de,[]).

n([pl(regimes),sg(regime)],het,[],
  [s(apartheid),
   h('Taliban')]).

n([sg([regime,change]),pl([regime,changes])],de,[]).

n([pl(regimenten),pl(regiments),sg(regiment)],het,[measure]).

n([pl('regio\'s'),pl(regionen),pl(regiones),sg(regio)],de,[],
  [micro,
   meso,
   s(veiligheid)]).

n([mass(regionalisering)],de,[]).

n([pl(regisseurs),sg(regisseur)],de,[],
  [buurt,
   film]).

n([pl(regisseuses),sg(regisseuse)],de,[]).

n([pl(registers),sg(register)],het,[],
  [condoleance,
   rouw,
   monument,
   i(rijk_monument,rijksmonumenten),
   dim(registertje)]).

n([pl(registraties),sg(registratie)],de,[],
  [gemeente,
   s(persoon)]).

n([pl(reglementen),sg(reglement)],het,[app_measure]).

n([sg(reglementering),pl(reglementeringen)],de,[]).

n([pl(regressies),sg(regressie)],de,[]).

n([sg(regularisatie),pl(regularisaties)],de,[]).

n([mass(regulering),pl(reguleringen),sg(regulering)],de,[],
  [zelf]).

n([pl(rehabilitaties),sg(rehabilitatie)],de,[]).

n([pl(reigers),sg(reiger)],de,[],
  [dim(reigertje),
   purper,
   zilver]).

n([mass(reine)],het,[]).

n([mass(reinheid)],de,[]).

n([sg(reiniger),pl(reinigers)],de,[]).

n([pl(reinigingen),sg(reiniging)],de,[]).

n([mass(reïntegratie)],de,[]).

n([pl(reizen),sg(reis)],de,
  [temp_mod,sbar,measure],
  [bus,
   dienst,
   heen,
   s(huwelijk),
   rond,
   school,
   terug,
   trein,
   vakantie,
   vlieg,
   voet,
   wereld,
   zee,
   dim(reisje)]).

n([pl(reisgidsen),sg(reisgids)],de,[],[vakantie]).

n([pl(reistassen),sg(reistas)],de,[]).

n([pl(reizigers),sg(reiziger)],de,[],
  [trein,
   i(zaak,zaken)]).

n([mass(rek)],both,[],[klim]).

n([pl(rekken),sg(rek)],het,[measure]).

n([pl(rekeningen),sg(rekening)],de,[sbar,vp,pred_pp(voor)],[]).

n([pl(rekeningen),sg(rekening)],de,[],
  [betaal,
   energie,
   reserve,
   spaar]).

n([pl([rekeningen,courant]),sg([rekening,courant])],de,[]).

n([sg(rekenkundige),pl(rekenkundigen)],de,[]).

n([pl(rekenmachines),sg(rekenmachine)],de,[]).

n([pl(rekenmeesters),sg(rekenmeester)],de,[]).

n([mass(rekenschap)],de,[]).

n([pl(rekesten),sg(rekest)],het,[]).

n([mass(reklassering)],de,[]).

n([pl(rekords),sg(rekord)],het,[sbar]).

n([pl(rekreaties),sg(rekreatie)],de,[]).

n([pl(rekruten),sg(rekruut)],de,[]).

n([sg(rekrutering),pl(rekruteringen)],de,[]).

n([pl(rellen),sg(rel)],de,[],
  [dim(relletje),
   voetbal]).

n([pl(relazen),sg(relaas)],het,[sbar]).

n([pl(relaties),sg(relatie)],de,[measure]).

n([pl(relaties),sg(relatie)],de,[],
  [s(handel),
   s(koninkrijk),
   s(liefde),
   s(onzekerheid),
   s(vertrouwen),
   i(zaak,zaken)]).

n([pl(relativeringen),sg(relativering)],de,[sbar,vp]).

n([mass(relativisme)],het,[]).

n([sg(release),pl(releases)],de,[]).

n([mass(relevantie)],de,[]).

n([pl(religies),pl(religiën),sg(religie)],de,[app_measure]).

n([mass(religiositeit)],de,[]).

n([pl(relikwieën),sg(relikwie)],both,[]).

n([pl(relingen),sg(reling)],de,[]).

n([pl(reliëfs),sg(reliëf)],het,[sbar]).

n([pl(relschoppers),sg(relschopper)],de,[]).

n([pl(relschopsters),sg(relschopster)],de,[]).

n([pl(remmen),sg(rem)],de,
  [vp],
  [dim(remmetje)]).

n([pl(remmen),sg(rem)],de,
  [],
  [hand,
   nood,
   schijf,
   dim(remmetje)]).

n([sg(remake),pl(remakes)],de,[]).

n([sg([remedial,teacher]),pl([remedial,teachers])],de,[]).

n([pl(remedies),sg(remedie)],de,[subject_sbar,subject_vp]).

n([sg(remigrant),pl(remigranten)],de,[]).

n([sg(remigratie),pl(remigraties)],de,[]).

n([pl(remises),sg(remise)],de,[]).

n([sg(remix),pl(remixen)],de,[]).

n([sg(remmer),pl(remmers)],de,[],[aids]).

n([pl(remmingen),sg(remming)],de,[]).

n([pl(remonstranten),sg(remonstrant)],de,[]).

n([sg(ren),pl(rennen)],de,[]).

n([mass(renaissance)],de,[]).

n([pl(renbanen),sg(renbaan)],de,[]).

n([pl(rendementen),sg(rendement)],het,[sbar,vp]).

n([pl(rendementen),sg(rendement)],het,[],
  [dividend]).

n([pl('rendez-vous'),sg('rendez-vous')],het,[],[dim('rendez-voustje')]).

n([pl(rendieren),sg(rendier)],het,[]).

n([pl(renners),sg(renner)],de,[],[s(beroep)]).

n([pl(rensters),sg(renster)],de,[],[s(beroep)]).

n([pl(renovaties),sg(renovatie)],de,[]).

n([sg(renstal),pl(renstallen)],de,[]).

n([sg(renster),pl(rensters)],de,[]).

n([mass(rentabiliteit)],de,[]).

n([pl(renten),pl(rentes),sg(rente)],de,[],
  [s(belening),
   i(dag_geld,daggeld),
   i(geld_markt,geldmarkt),
   hypotheek,
   i(kapitaal_markt,kapitaalmarkt),
   lijf,
   spaar
  ]).

n([pl(rentelasten)],both,[]).

n([sg(rentenier),pl(renteniers)],de,[]).

n([pl(rentmeesters),sg(rentmeester)],de,[]).

n([mass(rentmeesterschap)],het,[]).

n([pl(rentrees),sg(rentree)],de,[]).

n([pl(reorganisaties),sg(reorganisatie),
   pl(reorganizaties),sg(reorganizatie)],de,[sbar,vp]).

n([mass([rep,en,roer])],both,[pred_pp(in)]).

n([pl(reparaties),sg(reparatie)],de,[pred_pp(in)],[dim(reparatietje)]).

n([mass(repatriëring)],de,[]).

n([pl(repercussies),sg(repercussie)],de,[]).

n([pl(repertoires),sg(repertoire)],het,[]).

n([pl(repetities),sg(repetitie)],de,[],[dim(repetitietje)]).

n([sg([repetitive,strain,injury]),
   pl([repetitive,strain,injuries])],de,[]).

n([pl('replica\'s'),sg(replica)],de,[]).

n([pl(replieken),sg(repliek)],de,[sbar]).

n([sg(reply),pl(replies)],de,[]).

n([pl(reportages),sg(reportage)],de,[],
  [h(tv),televisie,tv,f([tv]),i(tv,'TV-')
  ]).

n([pl(reporters),sg(reporter)],de,[]).

n([pl(represailles),sg(represaille)],de,[]).

n([pl(representanten),sg(representant)],de,[]).

n([pl(representaties),sg(representatie)],de,[]).

n([mass(representativiteit)],de,[]).

n([pl(repressies),sg(repressie)],de,[sbar,vp]).

n([sg(reprimande),pl(reprimandes)],de,[]).

n([sg(reprise),pl(reprises)],de,[]).

n([pl(reproducties),sg(reproductie),
   pl(reprodukties),sg(reproduktie)],de,[]).

n([pl(reptielen),sg(reptiel)],het,[]).

n([pl(republieken),sg(republiek)],de,[],
  [deel,
   h('Sovjet'), 
   i('Sovjet',sovjet)]).

n([pl(republikeinen),sg(republikein)],de,[]).

n([pl(reputaties),sg(reputatie)],de,[sbar,vp]).

n([pl(requiems),sg(requiem)],het,[]).

n([mass(research)],de,[]).

n([mass([research,'&',development])],de,[]).

n([sg(resem),pl(resems)],de,[measure]).

n([pl(reservaten),sg(reservaat)],het,
  [np_app_measure],
  [natuur]).

n([sg(reservatie)],de,[]).

n([sg(reservering),pl(reserveringen)],de,[],
  [hotel]).

n([pl(reserves),sg(reserve),pl(reserven)],de,[pred_pp(in)]).

n([pl(reserves),sg(reserve),pl(reserven)],de,[],
  [gas,
   kas,
   olie,
   zuurstof]).

n([sg(reservist),pl(reservisten)],de,[]).

n([pl(reservoirs),sg(reservoir)],het,
  [measure],
  [dim(reservoirtje)]).

n([pl(residenten),sg(resident)],de,[],[h(assistent)]).

n([pl(residenties),sg(residentie)],de,[]).

n([sg(residue),pl(residuen)],het,[]).

n([mass(resistentie)],de,[]).

n([pl(resoluties),sg(resolutie)],de,
  [sbar,
   vp,
   app_measure],
  [ontwerp,
   h('VN'),
   s(wetgeving)]).

n([pl(resoluties),sg(resolutie)],de,
  [],
  [beeldscherm,
   scherm]).

n([mass(respect),mass(respekt)],het,[],
  [h(zelf),zelf]).

n([mass(respijt)],het,[]).

n([pl(respondenten),sg(respondent)],de,[]).

n([mass(respons)],both,[]).

n([pl(ressentimenten),sg(ressentiment)],het,[]).

n([sg(rest),pl(resten)],de,[temp_mod,sbar,measure],[dim(restje)]).  % de rest van het jaar

n([pl(restanten),sg(restant)],het,[]).

n([pl(restaurants),sg(restaurant)],het,[np_app_measure],
  [weg,
   dim(restaurantje)]).

n([pl(restaurateurs),sg(restaurateur)],de,[]).

n([pl(restauratoren),sg(restaurator)],de,[]). % http://www.restauratorenverenigingnoord.nl/

n([pl(restauraties),sg(restauratie)],de,[]).

n([sg(restitutie),pl(restituties)],de,[]).

n([pl(restricties),sg(restrictie)],de,[sbar,vp]).

n([pl(resultaten),sg(resultaat)],het,[sbar,subject_sbar],
  [eind,
   s(onderzoek),
   tussen]).

n([pl(resultaten),sg(resultaat)],het,[],
  [s(bedrijf),
   s(belegging),
   bruto,
   kwartaal,
   netto,
   s(onderhandeling),
   zoek
  ]).

n([pl(resultanten),sg(resultante)],de,[]).

n([sg(resumé),pl(resumés)],het,[]).

n([sg(retail)],de,[]).

n([pl(retenties),sg(retentie)],de,[]).

n([pl('retorica\'s'),sg(retorica)],de,[]).

n([mass(retoriek)],de,[]).

n([pl('retorika\'s'),sg(retorika)],de,[]).

n([pl(retouches),sg(retouche)],de,[]).

%% een retourtje heelal
n([pl(retours),sg(retour)],both,
  [app_measure],
  [dim(retourtje)]).

%% hij is op zijn retour
n([pl(retours),sg(retour)],both,
  [pred_pp(op)],
  []).

n([pl(retrospectieven),sg(retrospectief)],het,[]).

n([pl(returns),sg(return)],de,[]).

n([pl(reuen),sg(reu)],de,[]).

n([pl(reuken),sg(reuk)],de,[]).

n([mass(reuma)],both,[]).

n([mass(reumatiek)],de,[]).

n([sg(reumatoloog)],de,[]).

n([sg(reutel),pl(reutels)],de,[]).

n([pl(reuzen),sg(reus)],de,[]).

n([mass(reuzel)],de,[]).

n([sg(revalidatie)],de,[]).

n([pl(revanches),sg(revanche)],de,[sbar,vp]).

n([mass(reveil)],het,[]).

n([pl(revers),sg(revers)],both,[]).

n([sg(revelatie),pl(revelaties)],de,[]).

n([pl(reviews),sg(review)],de,[]).

n([pl(revisies),sg(revisie)],de,[]).

n([pl(revivals),sg(revival)],de,[]).

n([pl(revolten),pl(revoltes),sg(revolte)],de,[]).

n([pl(revoluties),sg(revolutie)],de,[]).

n([pl(revolutionairen),pl(revolutionairs),sg(revolutionair)],de,[]).

n([pl(revolvers),sg(revolver)],de,[],[dim(revolvertje)]).

n([pl(revues),sg(revue)],de,[],[dim(revuetje)]).

n([pl(rexen),sg(rex)],de,[]).

n([pl(reïncarnaties),sg(reïncarnatie)],de,[]).

n([pl(reünieën),sg(reünie)],de,[]).

n([mass([rhythm,and,blues]),
   mass([rhythm,'&',blues]),
   mass([r,'&',b]),
   mass('r&b')],de,[]).

%% iran
n([meas(rial),pl(rials)],de,[meas_mod,measure]).

n([pl(ribben),sg(rib)],de,[]).

n([mass([rib,cord])],both,[]).

n([mass(ribbenkast)],de,[]).

n([sg(ribosoom),pl(ribosomen)],het,[]).

n([pl(richels),sg(richel)],de,[],[dim(richeltje)]).

n([pl(richteren),pl(richters),sg(richter)],de,[]).

n([pl(richtingen),sg(richting)],de,[app_measure],
  [kunst]).

n([pl(richtingen),sg(richting)],de,[],
  [denk]).

n([sg(richtlijn),pl(richtlijnen)],de,
  [app_measure,
   subject_sbar,
   subject_vp,
   sbar,
   vp]).

n([sg(richtlijn),pl(richtlijnen)],de,[],
  [i(dienst,diensten),
   ontwerp]).

n([pl(richtsnoeren),sg(richtsnoer)],both,[sbar,vp]).

n([pl(ridders),sg(ridder)],de,[]).

n([sg(ridderorde),pl(ridderorden),pl(ridderordes)],de,[]).

n([pl(ridderschappen),sg(ridderschap)],both,[]).

n([sg(riek),pl(rieken)],de,[]).

n([pl(riemen),sg(riem)],de,[],[dim(riempje)]).

n([pl(rieslings),sg(riesling)],de,[]).

n([pl(rieten),sg(riet)],het,[],[dim(rietje)]).

n([pl(riffen),sg(rif)],het,[]).

n([pl(riffen),sg(rif)],de,[]).

n([pl(rijen),sg(rij)],de,
  [measure],
  [dim(rijtje)]).

n([pl(rijen),sg(rij)],de,[],
  [zit,
   dim(rijtje)]).

n([pl(rijbanen),sg(rijbaan)],de,[]).

n([pl(rijbewijzen),sg(rijbewijs)],het,[],
  [i(punt,punten)]).

n([pl(rijbroeken),sg(rijbroek)],de,[]).

n([pl(rijders),sg(rijder)],de,[],
  [bij,
   motor,
   test,
   toer,
   veld,
   vracht,
   wedstrijd,
   zwart]).

n([pl(rijdieren),sg(rijdier)],het,[]).

n([sg(rijdster),pl(rijdsters)],de,[],
  [i(schaats,schaatsen)]).

n([pl(rijken),sg(rijk)],het,[],
  [schaduw,
   wereld]).

n([pl(rijkaards),sg(rijkaard)],de,[]).

n([pl(rijkdommen),sg(rijkdom)],de,[pred_pp(van)]).

n([pl(rijksdaalders),sg(rijksdaalder)],de,[]).

n([pl(rijksuniversiteiten),sg(rijksuniversiteit)],de,[]).

n([mass(rijkswacht)],de,[]).

n([pl(rijkswachters),sg(rijkswachter)],de,[]).

n([sg(rijkswaterstaat)],de,[]).

n([pl(rijmen),sg(rijm)],het,[],[dim(rijmpje)]).

n([mass(rijp)],de,[]).

n([mass([rijp,en,groen])],het,[]).

n([pl(rijpaarden),sg(rijpaard)],het,[]).

n([mass(rijpheid)],de,[]).

n([pl(rijpingen),sg(rijping)],de,[]).

n([mass(rijst)],de,[],
  [pandan,
   plak,
   risotto
  ]).

n([sg(rijstrook),pl(rijstroken)],de,[]).  % rij_strook of rijst_rook

n([pl(rijtuigen),sg(rijtuig)],het,[],
  [huur,
   dim(rijtuigje)]).

n([pl(rijwielen),sg(rijwiel)],het,[]).

n([pl(riksen),sg(riks)],de,[]).

n([pl(rillingen),sg(rilling)],de,[]).

n([pl(rimboes),sg(rimboe)],de,[]).

n([pl(rimpels),sg(rimpel)],de,[],[dim(rimpeltje)]).

n([pl(ringen),sg(ring)],de,[measure],
  [dim(ringetje)]).		% de ringetjes ui

n([pl(ringen),sg(ring)],de,[],
  [boks, % geen bok_ring
   s(verloving),
   trouw,
   dim(ringetje)]).  

n([meas(ringgit)],de,[meas_mod,measure]). % munt Maleisie

n([pl(ringvingers),sg(ringvinger)],de,[]).

n([pl(rioleringen),sg(riolering)],de,[]).

n([pl(riolen),sg(riool)],both,[],[dim(riooltje)]). % Vlaams: de

n([sg(rip),pl(rips)],de,[]).

n([pl('risico\'s'),sg(risico)],het,[sbar,vp,subject_sbar,subject_vp]).
 
n([pl('risico\'s'),sg(risico)],het,[pred_pp(zonder)],
  [s(gezondheid)]).

n([sg(rist)],de,[measure]). % Vlaams

n([pl(ritten),sg(rit)],de,[temp_mod],
  [berg,
   i(koningin,koninginne),
   slot,
   test,
   veld,
   dim(ritje)]).

n([pl(riten),pl(rites),sg(rite)],de,[sbar,vp]).

n([pl(ritmen),pl(ritmes),sg(ritme)],het,[pred_pp(in),pred_pp(uit)],
  [wedstrijd]).

n([pl(ritsen),sg(rits)],de,[measure]).

n([pl(ritssluitingen),sg(ritssluiting)],de,[]).

n([pl(rituelen),sg(ritueel)],het,[sbar,vp]).

n([pl(rivalen),sg(rivaal)],de,[],[aarts]).

n([pl(rivales),sg(rivale)],de,[]).

n([mass(rivaliteit),pl(rivaliteiten),sg(rivaliteit)],de,[]).

n([pl(rivieren),sg(rivier)],de,
  [np_app_measure],
  [bij,
   zij,
   dim(riviertje)]).

n([mass('RNA'),
   mass('rna')],het,[]).

n([pl([roaring,twenties])],both,[]).

n([sg(robbertje),pl(robbertjes)],het,[measure]).

n([pl(robijnen),sg(robijn)],de,[],[dim(robijntje)]).

n([pl(robots),sg(robot)],de,[]).

n([sg(roc),pl('roc\'s'),
   sg('Roc'),pl('Roc\'s'),
   sg('ROC'),pl('ROC\'s')],both,[]).

n([sg(rochel),pl(rochels)],de,[]).

n([mass(rock)],de,[]).

n([mass([rock,and,roll]),
   mass([rock,'’',n,roll]),
   mass([rock,'&',roll]),
   mass([rock,'\'n',roll]),
   mass([rock,`,n,roll]),
   mass([rock,`,'n\'',roll]),
   mass([rock,'\'n\'',roll]),
   mass([rock,'\'n','\'',roll]),
   mass('rock-\'n-roll')],de,[]).

%todo: these are compounds
n([sg([rock,'&','roll-zanger']),
   pl([rock,'&','roll-zangers']),
   sg([rock,'\'n','roll-zanger']),
   pl([rock,'\'n','roll-zangers']),
   sg([rock,'\'n','\'','roll-zanger']),
   pl([rock,'\'n','\'','roll-zangers']),
   sg([rock,'\'n\'','roll-zanger']),
   pl([rock,'\'n\'','roll-zangers'])],de,[]).

n([sg([rock,'&','roll-band']),
   pl([rock,'&','roll-bands']),
   sg([rock,'\'n','roll-band']),
   pl([rock,'\'n','roll-bands']),
   sg([rock,'\'n','\'','roll-band']),
   pl([rock,'\'n','\'','roll-bands']),
   sg([rock,'\'n\'','roll-band']),
   pl([rock,'\'n\'','roll-bands'])],de,[]).

n([sg(rocker),pl(rockers)],de,[],
  [hard,
   h(hard)]).

n([sg(rococo)],de,[]).

n([pl(roddels),sg(roddel)],de,[sbar,vp]).

n([pl(rododendrons),sg(rododendron)],de,[]).

n([sg(roe)],de,[]).

n([pl(roebels),meas(roebel)],de,[meas_mod,measure]).

n([sg(roedel),pl(roedels)],de,[measure]).

n([pl(roeden),sg(roede)],de,[measure]).

n([pl(roeiboten),sg(roeiboot)],de,[]).

n([sg(roeier),pl(roeiers)],de,[]).

n([sg(roeister),pl(roeisters)],de,[]).

n([mass(roem)],de,[]).

n([pl(roemers),sg(roemer)],de,[],[dim(roemertje)]).

n([pl(roepen),sg(roep)],de,[sbar,vp]).

%% pakistan; india
n([meas(roepie),pl(roepies)],de,[meas_mod,measure]).

n([pl(roepingen),sg(roeping)],de,[vp]).

n([pl(roeren),pl(roers),sg(roer)],het,[],[dim(roertje)]).

n([pl(roergangers),sg(roerganger)],de,[]).

n([pl(roezen),sg(roes)],de,[]).

n([pl(roesten),sg(roest)],de,[]).

n([mass(roestvrijstaal)],het,[]).

n([mass(roet)],het,[]).

n([pl(roffels),sg(roffel)],de,[],[dim(roffeltje)]).

n([sg(rog),pl(roggen)],de,[]).

n([mass(rogge)],de,[]).

n([pl(rokken),sg(rok)],de,[],
  [mini,
   dim(rokje)]).

n([pl(rokers),sg(roker)],de,[],
  [dim(rokertje),
   f(niet),
   h(niet)]).

n([pl(rollen),sg(rol)],de,
  [measure,
   subject_vp],
  [dim(rolletje)]).

n([pl(rollen),sg(rol)],de,[],
  [dubbel,
   film,
   gast,
   hoofd,
   sleutel,
   titel,
   s(voortrekker)
  ]).

n([sg([role,model]),pl([role,models])],both,[]).

n([sg([role,playing,game]),pl([role,playing,games])],de,[]).

n([mass(rollenspel)],het,[]).

n([sg(rollator),pl(rollators)],de,[]).

n([pl(rolpatronen),sg(rolpatroon)],het,[sbar,vp]).

n([pl(romans),sg(roman)],de,
  [np_app_measure,
   start_app_measure],
  [debuut,
   detective,h(detective),
   misdaad,
   dim(romannetje)]).

n([pl(romancen),pl(romances),sg(romance)],de,[]).

n([pl(romanciers),sg(romancier)],de,[]).

n([pl(romantici),sg(romanticus)],de,[]).

n([mass(romantiek)],de,[]).

n([pl(romers),sg(romer)],de,[],[dim(romertje)]).

n([mass(rommel)],de,[],[dim(rommeltje)]).

n([pl(rompen),sg(romp)],de,[]).

n([mass(rompslomp)],de,[]).

n([sg(rond)],het,[]).

n([sg(ronde),pl(ronden),pl(rondes)],de,[temp_mod,sbar,measure],
  [ere,
   heen,
   s(opening),
   slot,
   terug,
   wieler,
   dim(rondje)]).

n([pl(rondedansen),sg(rondedans)],de,[]).

n([pl(rondgangen),sg(rondgang)],de,[]).

n([pl(rondingen),sg(ronding)],de,[]).

n([pl(rondleidingen),sg(rondleiding)],de,[]).

n([sg(rondschrijven)],het,[]).

n([sg(rondte)],de,[]).

n([pl(rondvaarten),sg(rondvaart)],de,[]).

n([sg(rondzit)],de,[]).

n([sg(ronselaar),pl(ronselaars)],de,[]).

n([mass(rood)],het,[],
  [bloed,
   donker,
   licht,
   schaam]).

n([mass('rood-groen')],het,[]).

n([pl(roodkapjes),sg(roodkapje)],het,[]).

n([mass(roof),pl(roven)],de,[],[straat]).

n([pl(roofdieren),sg(roofdier)],het,[]).

n([mass(rook)],de,[],
  [i(sigaar,sigare),     i(sigaar,sigaren),
   i(sigaret,sigarette), i(sigaret,sigaretten),
  s(tabak)]).

n([sg(rookmelder),pl(rookmelders)],de,[]).

n([pl(rookpluimen),sg(rookpluim)],de,[]).

n([pl(rookwolken),sg(rookwolk)],de,[]).

n([mass(room)],de,[],[chat]).

n([pl('rooms-katholieken'),sg('rooms-katholiek'),
   pl('rooms-katolieken'),sg('rooms-katoliek')],de,[]).

n([pl(rozen),sg(roos)],de,[pred_pp(in),
			   measure],[dim(roosje)]).

n([pl(roosters),sg(rooster)],both,[],
  [dienst,
   les,
   dim(roostertje)]).

n([pl(roots)],de,[]).

n([pl(rossen),sg(ros)],het,[]).

n([mass(rosbief)],de,[]).

n([mass(rose)],het,[]).

n([mass(rosé),
   pl(rosés),
   pl('rosé\'s')],de,[]).

n([sg(rostrum)],het,[]). % podium voor dirigent

n([pl(rotten),sg(rot)],de,[]).

n([pl(rotaties),sg(rotatie)],de,[]).

n([sg(rotje),pl(rotjes)],het,[]).

n([pl(rotonden),pl(rotondes),sg(rotonde)],both,[]).  %VL het rotonde

n([pl(rotsen),sg(rots)],de,[]).

n([pl(rotterdammers),sg(rotterdammer)],de,[]).

n([mass(rotting)],de,[]).

n([sg(rottweiler),pl(rottweilers)],de,[]).

n([pl(rotzakken),sg(rotzak)],de,[]).

n([mass(rotzooi)],de,[]).

n([mass(rouge)],de,[]).

n([mass(roulatie)],de,[]).

n([pl(roulettes),sg(roulette)],de,[]).

n([pl(routen),pl(routes),sg(route)],de,[np_app_measure],
  [fiets,
   wandel]).

n([sg(router),pl(routers)],de,[]).

n([pl(routines),sg(routine)],de,[]).

n([sg(routinier),pl(routiniers)],de,[]).

n([mass(rouw)],de,[pred_pp(in)]).

n([pl(rovers),sg(rover)],de,[],[dim(rovertje)]).

n([sg(royal),pl(royals)],de,[]).

n([sg(royalist),pl(royalisten)],de,[]).

n([pl(royementen),sg(royement)],het,[]).

n([mass(roze)],het,[]).

n([pl(rozemarijnen),sg(rozemarijn)],de,[]).

n([pl(rozenkransen),sg(rozenkrans)],de,[]).

n([pl(rozetten),sg(rozet)],both,[]).

n([pl(rozijnen),sg(rozijn)],de,[],[dim(rozijntje)]).

n([mass(rsi),mass('RSI')],de,[]).

n([mass('RSI-bestrijding'),
   mass('rsi-bestrijding')],de,[]).

n([pl('RSI-klachten'), pl('rsi-klachten'),
   sg('RSI-klacht'), sg('rsi-klacht')],de,[]).

n([pl('RSI-patiënten'), pl('rsi-patiënten'),
   sg('RSI-patiënt'),   sg('rsi-patiënt')],de,[]).

n([pl(rubbers),sg(rubber)],both,[]).

n([pl(rubrieken),sg(rubriek)],de,[app_measure],
  [dim(rubriekje)]).

n([pl(rubrieken),sg(rubriek)],de,[],
  [i(actualiteit,actualiteiten)]).

n([mass(ruchtbaarheid)],de,[]).

n([mass(rugby)],both,[]).

n([pl(ruggen),sg(rug)],de,[],
  [berg,
   onder,
   dim(rugje),dim(ruggetje)]).

n([mass(ruggespraak),mass(ruggenspraak)],de,[]).

n([mass(ruggenmerg),
   mass(ruggemerg)],het,[]).

n([pl(ruggengraten),sg(ruggengraat),
   pl(ruggegraten),sg(ruggegraat)],de,[vp]).

n([mass(rugslag)],de,[]).

n([pl(rugzakken),sg(rugzak)],de,[]).

n([sg(rugzakker),pl(rugzakkers)],de,[]).

n([pl(ruikers),sg(ruiker)],de,[],[dim(ruikertje)]).

n([pl(ruilen),sg(ruil)],de,[],
  [i(aandeel,aandelen),
   woning]).

n([pl(ruilverkavelingen),sg(ruilverkaveling)],de,[]).

n([mass(ruilwaarde)],de,[]).

n([pl(ruimen),sg(ruim)],het,[]).

n([sg(ruimer),pl(ruimers)],de,[]).

n([sg(ruiming),pl(ruimingen)],de,[]).

n([pl(ruimten),pl(ruimtes),sg(ruimte)],de,[vp]).

n([pl(ruimten),pl(ruimtes),sg(ruimte)],de,[],
  [bagage,
   been,
   s(bedrijf),
   s(beleid),
   berg,
   expositie,
   s(gebruiker),
   hoofd,
   kantoor,
   laad,
   opslag,
   s(screening),
   speel,
   toilet,
   tussen,
   werk,
   woon,
   zit]).

n([mass(ruimtevaart)],de,[]).

n([pl(ruimtevaartuigen),sg(ruimtevaartuig)],het,[]).

n([pl(ruimteveren),sg(ruimteveer)],het,[]).

n([pl(ruinen),sg(ruin)],de,[]).

n([pl(ruizen),sg(ruis)],de,[]).

n([sg(ruisseau)],de,[np_app_measure]).

n([pl(ruiten),sg(ruit)],de,[],
  [etalage,
   winkel,
   zij,
   dim(ruitje)]).

n([pl(ruiten),sg(ruiten)],de,[]).

n([pl(ruiters),sg(ruiter)],de,[],[dim(ruitertje)]).

n([mass(ruiterij)],de,[]).

n([pl(rukken),sg(ruk)],de,[]).

n([mass(rum)],de,[]).

n([pl(rumoeren),sg(rumoer)],het,[]).

n([pl(runs),sg(run)],de,[measure]).  % met 90 runs verschil

n([pl(runderen),sg(rund)],het,[],
  [dim(rundje),
   dim(rundertje)]).

n([mass(rundvee)],het,[]).

n([sg(running)],de,[pred_pp(in),
                    pred_pp(uit)]).

n([sg([running,mate]),pl([running,mates])],de,[]).

%% indonesie
n([meas(rupiah)],both,[meas_mod,measure]).

n([pl(rupsen),sg(rups)],de,[],[eikenprocessie]).

n([pl(rupsbanden),sg(rupsband)],de,[]).

n([pl(rusten),sg(rust)],de,[vp]).

n([pl(rusten),sg(rust)],de,[],
  [bed,
   s(gemoed),
   nacht,
   s(zondag)]).

n([mass(rusteloosheid)],de,[]).

n([sg(ruststand),pl(ruststanden)],de,[]).

n([pl(ruzies),sg(ruzie)],de,[],[dim(ruzietje)]).

n([pl(ruïnen),pl(ruïnes),sg(ruïne)],de,[]).

%% roestvrijstaal
n([mass(rvs)],both,[]).

n([pl('röntgenfoto\'s'),sg(röntgenfoto)],de,[]).

n([mass(röntgenstraling)],de,[]).

n([pl('s\'en'),pl('s\'s'),sg(s)],de,[]).

n([pl('s\'jes'),sg('s\'je')],het,[]).

n([mass(saamhorigheid)],de,[]).

n([pl(sabbatten),sg(sabbat)],de,[]).

n([pl(sabels),sg(sabel)],de,[],[dim(sabeltje)]).

n([mass(sabotage)],de,[]).

n([pl(sacramenten),sg(sacrament)],het,[]).

n([pl(sacristieën),sg(sacristie)],de,[]).

n([mass(sadisme)],het,[]).

n([pl(sadisten),sg(sadist)],de,[]).

n([pl(safes),sg(safe)],de,[]).

n([sg([safe,area]),pl([safe,areas])],de,[]).

n([sg([safe,house]),pl([safe,houses])],both,[]).

n([pl(saffieren),sg(saffier)],de,[],[dim(saffiertje)]).

n([mass(saffraan)],de,[]).

n([pl(sagen),pl(sages),sg(sage)],de,[]).

n([sg(saillant),pl(saillanten)],de,[]). % punten van een bunker oid

n([pl(sakramenten),sg(sakrament)],het,[]).

n([pl(sakristieën),sg(sakristie)],de,[]).

n([sg([salad,bar]),pl([salad,bars])],de,[]).

n([pl(salades),sg(salade)],de,[],
  [krab]).

n([pl(salamanders),sg(salamander)],de,[],[dim(salamandertje)]).

n([pl(salarissen),sg(salaris)],het,[],
  [i(ambtenaar,ambtenaren),
   basis,
   jaar,
   maand,
   top,
   week]).

n([mass(salariëring)],de,[]).

n([pl(saldi),pl('saldo\'s'),sg(saldo)],het,[],[doel]).

n([mass(salie)],de,[]).

n([mass(salmonella)],de,[]).

n([pl(salons),sg(salon)],both,[],
  [auto,
   kap,
   dim(salonnetje)]).

n([sg(salto),pl('salto\'s')],de,[]).

n([sg([salto,mortale]),pl(['salto\'s',mortale])],de,[]).

n([sg(saluut),pl(saluten)],het,[]).

n([pl('salvo\'s'),sg(salvo)],het,[measure],[dim(salvootje)]).

n([sg(samba),pl('samba\'s')],de,[]).

n([pl(sambals),sg(sambal)],de,[]).

n([mass([sambal,badjak])],de,[]).

n([mass([sambal,goreng])],de,[]).

n([mass([sambal,oelek])],de,[]).

n([mass(samengaan)],het,[]). % een samengaan

n([mass(samenhang),pl(samenhangen)],de,[]).

n([pl(samenkomsten),sg(samenkomst)],de,[]).

n([sg(samenleving),pl(samenlevingen)],de,[]).

n([mass(samenloop)],de,
  [subject_sbar]). % vervelende samenloop (van omstandigheden) dat ..

n([sg(samenscholing),pl(samenscholingen)],de,[]).

n([pl(samensmeltingen),sg(samensmelting)],de,[]).

n([mass(samenspel)],het,[]).

n([pl(samenspraken),sg(samenspraak)],de,[]).

n([mass(samenstel)],het,[]).

n([pl(samenstellers),sg(samensteller)],de,[]).

n([pl(samenstellingen),sg(samenstelling)],de,
  [sbar,
   vp]).

n([pl(samenstellingen),sg(samenstelling)],de,[],
  [s(bevolking)]).

n([pl(samentrekkingen),sg(samentrekking)],de,[]).

n([pl(samenvattingen),sg(samenvatting)],de,[sbar]).

n([pl(samenvloeiingen),sg(samenvloeiing)],de,[]).

n([pl(samenvoegingen),sg(samenvoeging)],de,[]).

n([mass(samenwerking),pl(samenwerkingen)],de,[],
  [defensie,
   s(ontwikkeling)]).

n([mass(samenwoning)],de,[]).

n([mass(samenzang)],de,[]).

n([mass(samenzijn)],het,[]).

n([pl(samenzweerders),sg(samenzweerder)],de,[]).

n([pl(samenzweringen),sg(samenzwering)],de,[]).

n([sg(sample),pl(samples)],both,[measure]).

n([pl(sanatoria),pl(sanatoriums),sg(sanatorium)],het,[]).

n([pl(sancties),sg(sanctie)],de,[sbar,vp,subject_sbar,subject_vp]).

n([mass(sanctionering),pl(sanctioneringen),sg(sanctionering)],de,[]).

n([pl(sandalen),sg(sandaal)],de,[],
  [sport,
   dim(sandaaltje)]).

n([pl(sandwiches),sg(sandwich)],de,[]).

n([pl(saneringen),sg(sanering)],de,[],
  [bodem,
   schuld,
   i(schuld,schulden)]).

n([mass(sanitair)],het,[]).

n([pl(santen),sg(sant)],de,[]).

n([pl(sappen),sg(sap)],both,[],
  [appel,
   citroen,
   limoen,
   sinaasappel,
   i(vrucht,vruchten),
   i(vrucht,vruchte)
   ]).

n([pl(sarcasmen),sg(sarcasme),
   pl(sarkasmen),sg(sarkasme)],het,[sbar,vp]).

n([sg(sarcofaag),pl(sarcofagen)],de,[]).

n([mass(sarin)],both,[]).  % zenuwgas

n([pl(sarongs),sg(sarong)],de,[]).

n([stem('SARS'),
   mass(sars),mass('SARS')],de,[]).

n([sg(sas)],both,[pred_pp(in)]).

n([pl(satans),sg(satan)],de,[]).

n([pl(satellieten),sg(satelliet)],de,[]).

n([mass(satijn)],het,[]).

n([pl(satiren),pl(satires),sg(satire)],de,[sbar,vp]).

n([pl(satrapen),sg(satraap)],de,[]).

n([pl('sauna\'s'),sg(sauna)],de,[]).

n([pl(sausen),pl(sauzen),sg(saus)],de,[],
  [chili,
   room,
   soja,
   i(tomaat,tomaten),
   dim(sausje)]).

n([mass(sauternes)],de,[]).

n([pl(savannen),pl(savannes),sg(savanne)],de,[]).

n([pl(saxen),sg(sax)],de,[]).

n([sg(saxofoon),pl(saxofoons)],de,[],[alt]).

n([sg(saxofonist),pl(saxofonisten)],de,[],[alt]).

n([pl('scala\'s'),sg(scala)],both,[]).

n([sg(scan),pl(scans)],de,[]).

n([sg(scanner),pl(scanners)],de,[]).

n([pl('scenario\'s'),sg(scenario)],het,
  [subject_sbar,
   subject_vp,
   sbar,
   vp],
  [doem]).

n([sg(scenarist),pl(scenaristen)],de,[]).

%% deel van toneelstuk/film; scène schoppen
n([mass(scène)],de,[]).

%% "sien", sociale omgeving
n([mass(scene)],de,[],
  [muziek]).

n([mass(scepsis)],de,[sbar]).

n([pl(scepters),sg(scepter)],de,[]).

n([mass(scepticisme)],het,[]).

n([pl(sceptici),sg(scepticus)],de,[],
  [euro]).

n([sg(schaaf),pl(schaven)],de,[],[kaas]).

n([mass(schaak)],het,[],
  [correspondentie]).

n([pl(schaakborden),sg(schaakbord)],het,[]).

n([pl(schalen),sg(schaal)],de,[measure],
  [oven,
   dim(schaaltje)]).

n([pl(schalen),sg(schaal)],de,[],
  [loon]).

n([pl(schaaldieren),sg(schaaldier)],het,[]).

n([pl(schaamharen),sg(schaamhaar)],het,[]).

n([mass(schaamte)],de,[sbar]).

n([pl(schapen),sg(schaap)],het,[],[dim(schaapje)]).

n([pl(schaapherders),sg(schaapherder)],de,[]).

n([pl(scharen),sg(schaar)],de,[],
  [nagel,
   ploeg,
   dim(schaartje)]).

n([mass(schaarste)],de,[],
  [water]).

n([pl(schaatsen),sg(schaats)],de,[],
  [klap,
   kunst,
   rol]).

n([pl(schaatsers),sg(schaatser)],de,[],
  [kunst,
   marathon,
   top]).

n([stem(schaatsster),
   pl(schaatssters),sg(schaatsster),
   pl(schaatsters),sg(schaatster)],de,[],
  [kunst,
   marathon,
   top]).

n([pl(schachten),sg(schacht)],de,[]).

n([pl(schaden),pl(schades),sg(schade)],de,[],
  [plan,
   rook,
   water]).

n([pl(schadeclaims),sg(schadeclaim)],de,[]).

n([pl(schadeloosstellingen),sg(schadeloosstelling)],de,[]).

n([pl(schadeposten),sg(schadepost)],de,[]).

n([pl(schaduwen),sg(schaduw)],de,[],
  [slag]).

n([pl(schaduwzijden),sg(schaduwzijde)],de,[sbar]).

n([mass(schaft)],de,[]).

n([pl(schakels),sg(schakel)],de,[],[dim(schakeltje)]).

n([mass(schaken)],het,[]).  % het schaakspel; ik leer hem schaken =/= ik leer hem_i schaken(su=i)  ?

n([pl(schakelaars),sg(schakelaar)],de,[],
  [licht,
   dim(schakelaartje)]).

n([pl(schakelingen),sg(schakeling)],de,[]).

n([pl(schakers),sg(schaker)],de,[]).

n([pl(schakeringen),sg(schakering)],de,[]).

n([pl(schandalen),sg(schandaal)],het,[sbar,vp,subject_sbar],
  [dim(schandaaltje)]).				% grof!

n([pl(schandalen),sg(schandaal)],het,[],
  ['Ahold',
   beurs,
   boekhoud,
   corruptie,
   diesel,
   doping,
   'Enron',
   omkoop]).

n([mass(schande)],de,[subject_sbar,subject_vp]).

n([sg(schandpaal),pl(schandpalen)],de,[]).

n([sg(schans),pl(schansen)],de,[]).

n([pl(schappen),sg(schap)],het,[]).

n([pl(schapers),sg(schaper)],de,[]).

n([pl(scharen),sg(schare)],de,[measure]).

n([pl(scharnieren),sg(scharnier)],both,[]).

n([pl(scharniertjes),sg(scharniertje)],het,[]).

n([sg(scharrel),pl(scharrels)],de,[]).

n([pl(schatten),sg(schat)],de,[],
  [kunst,
   dim(schatje)]).

n([pl(schatkisten),sg(schatkist)],de,[]).

n([pl(schattingen),sg(schatting)],de,[sbar,vp]).

n([pl(schavotten),sg(schavot)],het,[]).

n([pl(scheden),pl(schedes),sg(schede)],de,[]).

n([pl(schedels),sg(schedel)],de,[]).

n([pl(schenen),sg(scheen)],de,[]).

n([mass(scheepsbouw)],de,[]).

n([pl(scheepswerven),sg(scheepswerf)],de,[]).

n([mass(scheepvaart)],de,[],[zee]).

n([pl(scheten),sg(scheet)],de,[]).

n([pl(scheien),sg(schei)],de,[]).

n([pl(scheidingen),sg(scheiding)],de,[],
  [flits,
   dim(scheidinkje)]).

n([pl(scheidsrechters),sg(scheidsrechter)],de,[],[voetbal]).

n([mass(scheikunde)],de,[]).

n([sg(scheikundige),pl(scheikundigen)],de,[]).

n([mass(scheit)],de,[]).

n([sg(schel),pl(schellen)],de,[]).

n([pl(schelpen),sg(schelp)],de,[]).

n([pl('schema\'s'),pl(schemata),sg(schema)],het,[],[dim(schemaatje)]).

n([mass(schemer)],both,[]).

n([mass(schemerdonker)],het,[]).

n([pl(schemeringen),sg(schemering)],de,[],
  [avond,
   morgen,
   ochtend]).

n([pl(schemerlampen),sg(schemerlamp)],de,[]).

n([pl(schendingen),sg(schending)],de,[sbar,vp]).

n([pl(schendingen),sg(schending)],de,[],
  [i(mens_recht,mensenrechten)]).

n([pl(schenkers),sg(schenker)],de,[]).

n([pl(schenkingen),sg(schenking)],de,[]).

n([sg(schenkkan),pl(schenkkannen)],de,[]).

n([pl(scheppen),sg(schep)],de,[measure],[dim(schepje)]).

n([pl(schepenen),sg(schepen)],de,[]).

n([pl(scheppers),sg(schepper)],de,[]).

n([pl(scheppingen),sg(schepping)],de,[]).

n([pl(scheppingsverhalen),sg(scheppingsverhaal)],het,[]).

n([pl(schepselen),pl(schepsels),sg(schepsel)],het,[],[dim(schepseltje)]).

n([pl(scherven),sg(scherf)],de,[pred_pp_pl(aan),
                                pred_pp_pl(in)]).

n([pl(scherven),sg(scherf)],de,[],[glas]).

n([pl(scheringen),sg(schering)],de,[]).

n([pl(schermen),sg(scherm)],het,[measure],   % een scherm (vol) reclames
  [computer,
   s(geluid),
   projectie,
   h(tv),televisie,tv,f([tv]),i(tv,'TV-'),
   video,
   dim(schermpje)]).

n([pl(schermutselingen),sg(schermutseling)],de,[]).

n([mass(scherp)],het,[]).

n([mass(scherpst)],het,[]).   % het scherpst van de snede (sometimes others)

n([pl(scherpschutters),sg(scherpschutter)],de,[]).

n([pl(scherpten),pl(scherptes),sg(scherpte)],de,[]).

n([pl(scherpzinnigheden),sg(scherpzinnigheid)],de,[sbar,vp]).

n([mass(scherts)],de,[]).

n([pl(schetsen),sg(schets)],de,
  [sbar]).

n([pl(schetsen),sg(schets)],de,[],
  [profiel]).

n([pl(scheuren),sg(scheur)],de,[],
  [spier,
   dim(scheurtje)]).

n([pl(scheuringen),sg(scheuring)],de,[],
  [spier]).

n([pl(scheuten),sg(scheut)],de,[measure],[dim(scheutje)]).

n([pl(schiereilanden),sg(schiereiland)],het,[]).

n([sg(schifting),pl(schiftingen)],de,[]).

n([pl(schijven),sg(schijf)],both,
  [measure],
  [dim(schijfje)]).

n([pl(schijven),sg(schijf)],both,[],
  [i(tussen_wervel,tussenwervel)]).

n([mass(schijn)],de,[sbar,vp]).

n([pl(schijngestalten),sg(schijngestalte)],de,[]).

n([mass(schijnheiligheid)],de,[]).

n([sg(schijnoffer),pl(schijnoffers)],het,[]).

n([pl(schijnsels),sg(schijnsel)],het,[]).

n([pl(schijntjes),sg(schijntje)],het,[]).

n([pl(schijnwerpers),sg(schijnwerper)],de,[]).

n([mass(schijt)],de,[]).

n([mass(schik)],de,[]).

n([pl(schikkingen),sg(schikking)],de,[sbar,vp]).

n([pl(schillen),sg(schil)],de,[],
  [appel,
   i(mandarijn,mandarijnen),
   sinaasappel,
   dim(schilletje)]).

n([pl(schilden),sg(schild)],het,[],
  [raket,
   wapen]).

n([pl(schilders),sg(schilder)],de,[],
  [huis,
   klad,
   portret
  ]).

n([pl(schilderessen),sg(schilderes)],de,[]).

n([pl(schilderijen),sg(schilderij)],both,[],[dim(schilderijtje)]).

n([pl(schilderingen),sg(schildering)],de,[]).

n([pl(schildklieren),sg(schildklier)],de,[]).

n([pl(schildpadden),sg(schildpad)],de,[]).

n([pl(schildwachten),pl(schildwachts),sg(schildwacht)],de,[]).

n([pl(schilfers),sg(schilfer)],de,[],[dim(schilfertje)]).

n([pl(schillers),sg(schiller)],de,[]).

n([pl(schimmen),sg(schim)],de,[]).

n([pl(schimmels),sg(schimmel)],de,[]).

n([sg(schimp)],de,[]).

n([pl(schepen),sg(schip)],het,[np_app_measure],
  [container,
   cruise,
   lucht,
   marine,
   midden,
   moeder,
   s(onderzoek),
   s(oorlog),oorlog,
   s(passagier),
   ruimte,
   slag,
   stoom,
   turf,
   s(visser),
   i(vlag,vlaggen),   i(vlag,vlagge),
   vliegdek,
   h('VOC'),
   vracht,
   woon,
   zee,
   zeil,
   zuster,
   dim(scheepje)]).

n([pl(schipbreuken),sg(schipbreuk)],de,[]).

n([pl(schipbreukelingen),sg(schipbreukeling)],de,[]).

n([pl(schippers),sg(schipper)],de,[]).

n([pl('schisma\'s'),pl(schismata),sg(schisma)],het,[]).

n([mass(schitter)],de,[]).

n([pl(schitteringen),sg(schittering)],de,[]).

n([sg(schizofreen),pl(schizofrenen)],de,[]).

n([mass(schizofrenie)],de,[]).

n([sg(schlager),pl(schlagers)],de,[]).

n([mass(schmink)],de,[]).

n([sg(schnabbel),pl(schnabbels)],de,[]).

n([sg(schnitzel),pl(schnitzels)],de,[]).

n([pl(schoeisels),sg(schoeisel)],het,[]).

n([pl(schoenen),sg(schoen)],de,[],
  [gym,
   i(hard_loop,hardloop),
   i(kind,kinder),
   sport,
   tennis,
   voetbal,
   zaal,
   dim(schoentje)]).

n([pl(schoenmakers),sg(schoenmaker)],de,[]).

n([sg(schoffel),pl(schoffels)],de,[]).

n([pl(schoften),sg(schoft)],de,[]).

n([pl(schokken),sg(schok)],de,
  [subject_vp,
   subject_sbar],
  [dim(schokje)]).

n([pl(schokken),sg(schok)],de,[],
  [aard,
   na,
   dim(schokje)]).

n([sg(schokdemper),pl(schokdempers)],de,[]).

n([pl(schollen),sg(schol)],de,[],[dim(scholletje)]).

n([sg(schollevaar),pl(schollevaars)],de,[]).

n([pl(scholieren),sg(scholier)],de,[]).

n([pl(scholieres),sg(scholiere)],de,[]).

n([mass(scholing)],de,[],[bij,na]).

n([pl(schommels),sg(schommel)],de,[],[dim(schommeltje)]).

n([pl(schommelingen),sg(schommeling)],de,[]).

n([mass(schompes)],both,[]).

n([pl(schonen),sg(schone)],de,[]).

n([pl(schoven),sg(schoof)],de,[measure]).

n([pl(schooiers),sg(schooier)],de,[]).

n([pl(scholen),sg(school)],de,[measure]). % vis

n([pl(scholen),sg(school)],de,[np_app_measure],
  [s(achterstand),
   s(ambacht),
   avond,
   basis,
   f(business),
   gemeente,
   s(handel),
   kleuter,
   kost,
   kweek,
   landbouw,
   leer,
   midden,
   muziek,
   rij,
   s(rijk),
   sport,
   toneel,
   tussen,
   s(volk),
   dim(schooltje)]).

n([pl(schoolonderzoeken),sg(schoolonderzoek)],het,[]).

n([mass(schoolslag)],de,[]).

n([mass(schoolstrijd)],de,[]).

n([pl(schooltassen),sg(schooltas)],de,[]).

n([pl(schoolverlaters),sg(schoolverlater)],de,[]).

n([mass(schoon)],het,[],
  [natuur]).

n([pl(schoonheden),sg(schoonheid)],de,[pred_pp(van),
				       pred_pp(van,subject_sbar)]).

n([pl(schoonmaken),sg(schoonmaak)],de,[]).

n([sg(schoonmaker),pl(schoonmakers)],de,[]).

n([sg(schoonmaakster),pl(schoonmaaksters)],de,[]).

n([mass(schoons)],het,[]).

n([pl(schoorstenen),sg(schoorsteen)],de,[]).

n([pl(schoorsteenmantels),sg(schoorsteenmantel)],de,[]).

n([pl(schoten),sg(schoot)],de,[]).

n([pl(schoppen),sg(schop)],de,[measure]).

n([pl(schoppen),sg(schop)],de,[pred_pp(op)],[]).

n([pl(schoppen),sg(schop)],de,[],[vrij]).

n([pl(schoppen),pl(schoppens),sg(schoppen)],de,[]).

n([sg(schor),pl(schorren)],de,[]).

n([pl(schorpioenen),sg(schorpioen)],de,[]).

n([pl(schorsen),sg(schors)],de,[]).

n([pl(schorsingen),sg(schorsing)],de,[]).

n([pl(schorten),sg(schort)],both,[],[dim(schortje)]).

n([mass(shorttrack),
   mass('short-track'),
   mass([short,track])],both,[]).

n([sg(shorttracker),
   pl(shorttrackers)],de,[]).

n([pl(schoten),pl(schotten),sg(schot)],het,
  [pred_pp(buiten),
   measure]).

n([pl(schotten),sg(schot)],het,[],[tussen]).

n([pl(schoten),sg(schot)],het,[],
  [s(afstand),
   geweer,
   start,
   s(waarschuwing)]).

n([pl('Schotten'),sg('Schot')],het,[]).

n([pl(schotels),sg(schotel)],de,[measure],
  [hoofd,
   oven,
   stoof,
   dim(schoteltje)]).

n([pl(schotsen),sg(schots)],de,[],[ijs]).

n([pl(schouders),sg(schouder)],de,[],
  [linker,
   rechter]).

n([pl(schouderklopjes),sg(schouderklopje)],het,[]).

n([mass(schouderophalen)],het,[]).

n([pl(schouten),sg(schout)],de,[]).

n([pl(schouwen),sg(schouw)],de,[]).

n([pl(schouwburgen),sg(schouwburg)],de,[]).

n([pl(schouwspelen),sg(schouwspel)],het,[]).

n([sg(schraag),pl(schragen)],de,[]).

n([pl(schrammen),sg(schram)],de,[],[dim(schrammetje)]).

n([pl(schreden),sg(schrede),sg(schred)],de,[]).

n([sg(schreef),pl(schreven)],de,[]).

n([pl(schreeuwen),sg(schreeuw)],de,[]).

n([sg(schreeuwlelijk),pl(schreeuwlelijken)],de,[]).

n([sg(schrepel),pl(schrepels)],de,[]).

n([pl('Schriften'),pl(schriften),sg(schrift),sg('Schrift')],both,[],
  [dim(schriftje)]).

n([sg(schriftelijk)],het,[]).

n([pl(schrijfmachines),sg(schrijfmachine)],de,[]).

n([pl(schrijfsters),sg(schrijfster)],de,[]).

n([sg(schrijn),pl(schrijnen)],both,[]).

n([sg(schrijven)],het,[]). % ik heb een schrijven ontvangen van ...x

n([pl(schrijvers),sg(schrijver)],de,[],
  [roman,
   scenario,
   sciencefiction,'science-fiction',f([science,fiction]),
   s(sprookje),
   toneel
  ]).

n([mass(schrijverschap)],het,[]).

n([pl(schrikken),sg(schrik)],de,
  [sbar,vp]).

n([pl(schroeven),sg(schroef)],de,[],[dim(schroefje)]).

n([pl(schroevendraaiers),sg(schroevendraaier)],de,[]).

n([mass(schroom)],de,[]).

n([mass(schroot)],both,[]).

n([sg(schub),pl(schubben)],de,[]).

n([sg(schuif),pl(schuiven)],de,[]).

n([pl(schuilnamen),sg(schuilnaam)],de,[]).

n([mass(schuim)],both,[],[piep]).

n([sg(schuimspaan),pl(schuimspanen)],de,[]).

n([pl(schuiten),sg(schuit)],de,[],[dim(schuitje)]).

n([sg(schuiver),pl(schuivers)],de,[],
  [sneeuw]).

n([pl(schulden),sg(schuld)],de,[subject_sbar]).

n([pl(schulden),sg(schuld)],de,[],
  [belasting,
   s(overheid),
   studie,
   s(staat)]).

n([pl(schuldeisers),sg(schuldeiser)],de,[]).

n([pl(schuldenaars),pl(schuldenaren),sg(schuldenaar)],de,[]).

n([pl(schuldenlasten),sg(schuldenlast)],de,[]).

n([sg(schulp),pl(schulpen)],de,[]).

n([mass(schurft)],de,[]).

n([pl(schurken),sg(schurk)],de,[]).

n([pl(schutten),sg(schut)],het,[]).

n([pl(schutters),sg(schutter)],de,[],
  [boog,
   handboog,
   sluip,
   top]).

n([sg(schutterij),pl(schutterijen)],de,[]).

n([pl(schuttingen),sg(schutting)],de,[]).

n([pl(schuren),sg(schuur)],de,[],[dim(schuurtje)]).

n([mass(schuurpapier)],het,[]).

n([sg(schwalbe),pl(schwalbes)],de,[]).

n([mass('science-fiction'),
   mass(sciencefiction)],de,[]).

n([mass([science,fiction])],de,[]).

n([pl(scooters),sg(scooter)],de,[]).

n([sg(scootmobiel),pl(scootmobielen)],de,[]).

n([pl(scores),sg(score),pl(scoren)  % VL: de scoren openen
  ],de,[],
  [h('OK')]).

n([sg(scout),pl(scouts)],de,[]).

n([mass(scouting)],de,[]).

n([sg([screen,saver]),pl([screen,savers])],de,[]).

n([sg(screening),pl(screeningen)],de,[]).

n([pl(scripts),sg(script)],both,[app_measure]).

n([pl(scripties),sg(scriptie)],de,[app_measure],
  [afstudeer,
   doctoraal]).

n([mass(scrotum)],het,[]).

n([pl(scrupules),sg(scrupule)],de,[sbar,vp]).

n([pl(sculpturen),sg(sculptuur)],both,[]).

n([pl(scènes),sg(scène)],de,[],
  [actie,
   slot]).

n([mass(scrapie)],de,[]). % schapenziekte

n([pl(seances),sg(seance)],de,[]).

n([pl([second,opinions]),sg([second,opinion])],de,[]).

n([meas(seconde),meas(sekonde),pl(secondes),pl(secondes),pl(seconden),pl(sekonden)],de,
  [temp_mod,measure,sbar]).

n([pl(secretaressen),pl(secretaresses),sg(secretaresse),
   pl(sekretaressen),pl(sekretaresses),sg(sekretaresse)],de,[],
  [directie]).

n([pl(secretariaten),sg(secretariaat),
   pl(sekretariaten),sg(sekretariaat)],het,[]).

n([sg('secretariaat-generaal'),pl('secretariaten-generaal')],het,[]).

n([pl(secretarissen),sg(secretaris),
   pl(sekretarissen),sg(sekretaris)],de,[],
  [directie,
   gemeente,
   partij,
   stadsdeel]).

n([stem(secretaris_generaal),
   pl('secretarissen-generaal'),sg('secretaris-generaal'),
   pl('sekretarissen-generaal'),sg('sekretaris-generaal')],de,[]).

n([pl(secten),pl(sectes),sg(secte)],de,[]).

n([pl(secties),pl(sectiën),sg(sectie),
   pl(sekties),pl(sektiën),sg(sektie)],de,[app_measure]).

n([pl(sectoren),pl(sectors),sg(sector),
   pl(sektoren),pl(sektors),sg(sektor)],de,[app_measure]).

n([pl(sectoren),pl(sectors),sg(sector),
   pl(sektoren),pl(sektors),sg(sektor)],de,[],
  [auto,
   bank,
   i(bank,banken),
   bouw,
   chip,
   cultuur,
   i(dagblad,dagbladen),
   i(dienst,diensten),
   energie,
   film,
   groei,
   huur,
   internet,
   koop,
   kunst,
   landbouw,
   luchtvaart,
   markt,
   media,
   metaal,
   olie,
   onderwijs,
   pluimvee,
   rundvlees,
   soep,			% Toon
   staal,
   technologie,
   telecom,
   telecommunicatie,
   transport,
   tuinbouw,
   s(varken),
   s(vervoer),
   vlees,
   visserij,
   zorg,
   zuivel]).

n([mass(secundair)],het,[]).

n([sg(sedan),pl(sedans)],de,[]).

n([sg(sedatie)],de,[]).

n([sg(sediment),pl(sedimenten)],het,[]).

n([sg(sedimentatie),pl(sedimentaties)],de,[]).

n([sg('Sefardim'),pl('Sefardim')],de,[]).

n([pl(segmenten),sg(segment)],het,[app_measure]).

n([mass(segregatie)],de,[]).

n([pl(seinen),sg(sein)],het,[sbar,vp],
  [dim(seintje)]).

n([pl(seinen),sg(sein)],het,[],
  [start]).

n([pl(seinpalen),sg(seinpaal)],de,[]).

n([sg(seismoloog),pl(seismologen)],de,[]).

%% het seizoen
%% de seizoen VL
n([sg(seizoen),pl(seizoenen)],both,
  [temp_mod,
   measure,  % na drie seizoenen ellende bij Ajax ...
   sbar],
  [wh(['Formule','1']),
   hoog,
   na,
   tussen,
   verkoop,
   voetbal,
   voor,
   wieler,
   winter,
   zomer]).

n([stem(seks),
   mass(seks),mass(sex)],de,[],
  [h(safe),f([safe])]).

n([pl(seksen),sg(sekse),pl(sexen),sg(sexe)],de,[]).

n([mass(seksleven),mass(sexleven)],het,[]).

n([pl(seksualiteiten),sg(seksualiteit),
   pl(sexualiteiten),sg(sexualiteit)],de,[]).

n([pl(sekten),pl(sektes),sg(sekte)],de,[]).

n([mass(selderij)],de,[]).

n([pl(selecties),sg(selectie),
   pl(selekties),sg(selektie)],de,[],
  [h('A'),
   h('B'),
   markt,
   voor,
   wedstrijd
  ]).

n([sg(selfie),pl(selfies)],de,[]).

n([sg(semafoon),pl(semafoons)],de,[]).

n([mass(semantiek)],de,[]).

n([sg(semester),pl(semesters)],het,[temp_mod]).

n([sg(seminar),pl(seminars)],both,[]).

n([pl(seminaries),sg(seminarie)],het,[]).

n([mass(semiotiek)],de,[]).

n([pl(sens),sg(sen)],de,[]).

n([pl(senaten),sg(senaat)],de,[]).

n([pl(senatoren),pl(senators),sg(senator)],de,[]).

n([pl(senioren),sg(senior),pl(seniors)],de,[]).

n([sg(señor),sg(senor)],de,[]).

n([pl('senorita\'s'),sg(senorita)],de,[]).

n([pl(sensaties),sg(sensatie)],de,[sbar,vp]).

n([sg(sensibilisatie),pl(sensibilisaties)],de,[]).

n([sg(sensibilisering),pl(sensibilisering)],de,[]).

n([sg(sensor),pl(sensoren)],de,[]).

n([pl(sentimenten),sg(sentiment)],het,[sbar]).

n([mass(sentimentaliteit)],de,[]).

n([sg(separatist),pl(separatisten)],de,[]).

n([mass(separatisme)],het,[]).

n([sg(september)],de,[temp_mod,sbar]).

n([mass(sereniteit)],de,[]).

n([pl(sergeanten),pl(sergeants),sg(sergeant)],de,[]).

n([pl('sergeant-majoors'),sg('sergeant-majoor')],de,[]).

n([pl(sergeantmajoors),sg(sergeantmajoor)],de,[]).

n([sg([serial,killer]),pl([serial,killers])],de,[]).

n([pl(series),pl(seriën),sg(serie)],de,[measure]).

n([pl(series),pl(seriën),sg(serie)],de,[],
  [animatie,
   documentaire,
   drama,
   foto,
   hit,
   mini,
   politie,
   reality,
   soap,
   i(strafschop,strafschoppen),
   i(teken_film,tekenfilm),
   televisie,tv,h(tv),f([tv]),i(tv,'TV-')
  ]).

n([pl(seringen),sg(sering)],de,[]).

n([pl(serres),sg(serre)],de,[]).

n([pl(sera),pl(serums),sg(serum)],het,[]).

n([pl(serveersters),sg(serveerster)],de,[]).

n([sg(server),pl(servers)],de,[]).

n([pl(servetten),sg(servet)],het,[]).

n([sg(service)],de,[],
  [self,
   h(self),
   i(klant,klanten),
   i(patiënt,patiënten),
   room
  ]).

n([sg([service,provider]),pl([service,providers])],de,[]).

n([pl(serviezen),sg(servies)],het,[]).

n([mass(serviesgoed)],het,[]).

n([mass(sesamolie)],de,[]).

n([pl(sessies),sg(sessie)],de,
  [measure,
   temp_mod]).

n([pl(sets),sg(set)],de,
  [measure,
   temp_mod],
  [dim(setje)]).

n([sg([set,top,box]),pl([set,top,boxen])],de,[]).

n([mass(setting)],de,[]).

n([pl(sferen),sg(sfeer)],de,[van_sbar,sbar],[dim(sfeertje)]).

n([pl(sferen),sg(sfeer)],de,[],
  [invloed,s(invloed),
   s(leven),
   werk,
   s(werking)]).

n([pl(sfinksen),sg(sfinks)],de,[]).

n([pl(sfinxen),sg(sfinx)],de,[]).

n([mass(shag)],de,[]).

n([mass('shari\'a'),mass(sharia)],de,[]).

n([sg([shaken,baby,syndrome]),sg([shaken,baby,syndroom])],het,[]).

n([sg([shake,out])],de,[]).

n([pl(shampoos),sg(shampoo)],de,[]).

n([sg(shareholder),pl(shareholders)],de,[]).

n([sg(sheet),pl(sheets)],both,[]).

% israel
n([meas(shekel),pl(shekels)],de,[meas_mod,measure]).

%% bommen
n([sg(shell),pl(shells)],de,[]).

n([pl(sheriffs),sg(sheriff)],de,[],
  [hulp]).

n([mass(sherry)],de,[]).

n([pl(shirts),sg(shirt)],both,[],
  [dim(shirtje),
   polo,
   h('T'),h(t)
  ]).

n([mass(shit)],de,[]).

n([pl(shocks),sg(shock)],de,
  [pred_pp(in),
   pred_pp(in,subject_sbar)
  ]).

n([sg(shocktoestand),pl(shocktoestanden)],de,[]).

n([sg(shop),pl(shops)],de,[],[web]).

n([sg(shopper),pl(shoppers)],de,[],[fun]).  

n([sg([short,list]),pl([short,lists])],de,[]).

n([pl(shots),sg(shot)],both,[measure],[dim(shotje)]).

n([pl(shows),sg(show)],de,[],
  [auto,
   middag,
   mode,
   ochtend,
   televisie,tv,h(tv),f([tv]),i(tv,'TV-'),
   f([ver,van,mijn,bed]),
   f([ver,van,'m\'n',bed]),
   f([ver,van,ons,bed]),
   f([ver,van,hun,bed])
  ]).

n([sg(showman),pl(showmannen)],de,[]).

n([sg(showroom),pl(showrooms)],de,[]).

n([sg(shuttle),pl(shuttles)],de,[],[]).

n([pl('si\'s'),sg(si)],de,[]).

n([sg(siddering),pl(sidderingen)],de,[]).

n([sg([side,kick]),pl([side,kicks])],de,[]).

n([sg([side,letter]),pl([side,letters])],de,[]).

n([pl([side,outs]),pl([side,outs]),sg([side,out]),sg([side,out])],de,[]).

n([mass(sier)],de,[]).

n([pl(sieraden),sg(sieraad)],het,[]).

n([pl(sigaren),sg(sigaar)],de,[],[dim(sigaartje)]).

n([pl(sigaretten),sg(sigaret)],de,[],
  [dim(sigaretje)]).

n([pl(sigarettepeuken),sg(sigarettepeuk)],de,[],[dim(sigarettepeukje)]).

n([pl(signalen),sg(signaal)],het,[sbar,vp]).

n([pl(signalen),sg(signaal)],het,[],
  [eind,
   fluit]).

n([pl(signalementen),sg(signalement)],het,[sbar]).

n([sg(signalering),pl(signaleringen)],de,[]).

n([pl(signaturen),sg(signatuur)],de,
  [vp,
   app_measure,
   pred_pp(van)]).

n([stem(sikh),
   sg(sikh),pl(sikhs),
   sg('Sikh'),pl('Sikhs')],de,[]).

n([pl(sikken),sg(sik)],de,[]).

n([pl(sikkels),sg(sikkel)],de,[],[dim(sikkeltje)]).

n([pl(silhouetten),sg(silhouet)],both,[]).

n([mass(silicium)],het,[]).

n([pl(simulaties),sg(simulatie)],de,[sbar,vp]).

n([sg(simulator),pl(simulatoren),pl(simulators)],de,[]).

n([sg(simultaan),pl(simultaans)],de,[]).

n([pl(sinaasappelen),pl(sinaasappels),sg(sinaasappel)],de,[]).

n([sg(sinecure)],de,[vp]).

n([pl(singels),sg(singel)],de,[],[dim(singeltje)]).

n([pl(singles),sg(single)],de,[],
  ['7"',             % "
   dim(singletje)
  ]).

n([sg([single,malt]),pl([single,malts])],de,[]).

n([sg([single,malt,whiskey]),
   sg([single,malt,whisky]),
   pl([single,malt,'whiskey\'s']),
   pl([single,malt,'whisky\'s'])],de,[]).

n([sg(singleton),pl(singletons)],de,[measure]).  % bridge

n([pl(sinterklazen),sg(sinterklaas)],de,[]).

n([sg(sir),pl(sirs)],de,[]).

n([pl(sirenen),pl(sirenes),sg(sirene)],de,[]).

n([pl(siropen),sg(siroop)],de,[]).

n([sg(sisser),pl(sissers)],de,[]).

n([pl(sites),sg(site)],de,[app_measure],
  [club,
   fan,f([fan]),h(fan),
   [fan,web],
   f([hate]),
   internet,
   nieuws,
   porno,
   web,f([web]),h(web)]).

n([pl(situaties),pl(situatiën),sg(situatie)],de,[sbar],
  [conflict,h(conflict),
   crisis,h(crisis),
   i(mens_recht,mensenrechten),
   nood,
   probleem,
   rot,
   s(uitgang),
   s(verkeer)]).

n([pl(situaties),pl(situatiën),sg(situatie)],de,[],
  [leef,
   s(leven),
   s(oorlog),
   s(veiligheid),
   werk]).

n([mass(situering)],de,[]).

n([pl('siërra\'s'),sg(siërra)],de,[]).

n([pl('siësta\'s'),sg(siësta)],de,[]).

n([pl(sjaals),sg(sjaal)],de,[],[dim(sjaaltje)]).

n([pl(sjablonen),sg(sjabloon)],both,[]).

n([pl(sjahs),sg(sjah)],de,[]).

n([sg(sjarief),sg(sjarif),sg(sharif)],de,[]).

n([pl(sjalotten),sg(sjalot)],de,[],[dim(sjalotje)]).

n([pl(sjamanen),sg(sjamaan)],de,[]).

n([sg(sjees),pl(sjezen)],de,[]).

n([pl(sjeiks),sg(sjeik)],de,[]).

n([pl(sjerpen),sg(sjerp)],de,[]).

n([stem(sjiiet),
   sg('Shiiet'),pl('Shiieten'),
   sg(shiiet),pl(shiieten)],de,[]).

n([stem(sjiiet),
   sg('Shi\'iet'),pl('Shi\'ieten'),
   sg('shi\'iet'),pl('shi\'ieten')],de,[]).

n([stem(sjiiet),
   sg('Shi-iet'),pl('Shi-ieten'),
   sg('shi-iet'),pl('shi-ieten')],de,[]).

n([stem(sjiiet),
   sg('Sjiiet'),pl('Sjiieten'),
   sg(sjiiet),pl(sjiieten)],de,[]).

n([stem(sjiiet),
   sg('Sji\'iet'),pl('Sji\'ieten'),
   sg('sji\'iet'),pl('sji\'ieten')],de,[]).

n([stem(sjiiet),
   sg('Sji-iet'),pl('Sji-ieten'),
   sg('sji-iet'),pl('sji-ieten')],de,[]).

n([mass(sjoege)],both,[]).

n([sg(sjoel),pl(sjoels)],de,[]).

n([mass(sjouw)],both,[]).

n([sg(skate),pl(skates)],de,[],
  [inline,
   h(inline),
   f([inline])]).

n([sg(skateboard),pl(skateboards)],het,[]).

n([sg([skate,off]),pl([skate,offs]),sg('skate-off'),pl('skate-offs')],de,[]).

n([sg(skater),pl(skaters)],de,[]).

n([sg(skeeler),pl(skeelers)],de,[]).

n([pl(skeletten),sg(skelet)],het,[]).

n([mass(skepsis)],de,[]).

n([pl(skepters),sg(skepter)],de,[]).

n([mass(skepticisme)],het,[]).

n([pl(skeptici),sg(skepticus)],de,[]).

n([sg(sketch),pl(sketches)],de,[]).

n([pl('ski\'s'),sg(ski),sg(skie),pl(skies)],de,[]).

n([sg(skiff),pl(skiffs)],de,[]).

n([sg(skill),pl(skills)],de,[]).

n([sg(skinhead),pl(skinheads)],de,[]).

n([pl(skiërs),sg(skiër)],de,[]).

n([sg(skip),pl(skips)],de,[]).  % iets van aanvoerder bij curling

n([sg(skipper),pl(skippers)],de,[],['Belgacom']).  % iets van aanvoerder bij curling/zeilen

n([pl(skisters),sg(skister)],de,[]).

n([pl(skripties),sg(skriptie)],de,[]).

n([pl(skrupules),sg(skrupule)],de,[sbar,vp]).

n([pl(skylabs),sg(skylab)],het,[]).

n([sg([sky,line]),
   sg(skyline),
   sg('sky-line')],de,[]).

n([pl('sla\'s'),sg(sla)],de,[]).

n([pl(slaven),sg(slaaf)],de,[],
  [galei]).

n([mass(slaag)],de,[]).

n([mass(slaap),pl(slapen),sg(slaap)],de,[pred_pp(in)]).

n([pl(slaapwandelaars),sg(slaapwandelaar)],de,[]).

n([pl(slaapzakken),sg(slaapzak)],de,[]).

n([stem(sla_DIM),
   pl(slaatjes),sg(slaatje)],het,[]).

n([sg(slab),pl(slabben)],de,[],
  [dim(slabbetje),
   dim(slabje)]).

n([mass(slacht)],de,[]).

n([pl(slachters),sg(slachter)],de,[]).

n([sg(slachterij),pl(slachterijen)],de,[]).

n([pl(slachthuizen),sg(slachthuis)],het,[]).

n([pl(slachtingen),sg(slachting)],de,[]).

%% VL: ook "de slachtoffer"?
n([pl(slachtoffers),sg(slachtoffer)],het,[],
  [asbest,
   burger,
   s(oorlog),
   s(verkeer)]).

n([sg(slachtparij),pl(slachtpartijen)],de,[]).

n([pl(slagen),sg(slag)],de,
  [measure,
   meas_mod,
   pred_pp(van),
   pred_pp(aan)]).

n([pl(slagen),sg(slag),
   ignore_stem(slag)],de,[],
  [concurrentie,
   honk,
   i(twee_honk,tweehonk),
   i(drie_honk,driehonk),
   kaak,
   kaal,
   nek,
   i(prijs,prijzen),
   stok,
   s(uitputting),
   veld,
   vuist,
   zweep]).

n([mass(slag)],het,[measure]).  % het slag volk

n([pl(slagaderen),pl(slagaders),sg(slagader)],de,[],
  [hoofd]).

n([pl(slagbomen),sg(slagboom)],de,[]).

n([pl(slagers),sg(slager)],de,[]).

n([pl(slagerijen),sg(slagerij)],de,[]).

n([mass(slagroom)],de,[]).

n([pl(slagvelden),sg(slagveld)],het,[]).

n([mass(slagwerk)],het,[]).

n([pl(slagwerkers),sg(slagwerker)],de,[]).

n([pl(slagzinnen),sg(slagzin)],de,[sbar,start_app_measure]).

n([pl(slakken),sg(slak)],de,[],
  [naakt  % niet naakt_lakken
  ]).

n([mass(slalom)],de,[],[i(reus,reuzen)]).

n([pl(slangen),sg(slang)],de,[],
  [lanspunt,
   tuin,
   dim(slangetje)]).

n([mass(slapeloosheid)],de,[]).

n([pl(slapers),sg(slaper)],de,[],
  ['Dam'  % en niet Dam_sla_pers
  ]).

n([pl(slappelingen),sg(slappeling)],de,[]).

n([mass(slapstick)],de,[]).

n([mass(slavernij)],de,[]).

n([pl(slavinnen),sg(slavin)],de,[],[dim(slavinnetje)]).

n([sg(slechterik),pl(slechterikken)],de,[]).

n([pl(slechtheden),sg(slechtheid)],de,[]).

n([pl(sleden),sg(slede),pl(sledes),pl(sleeën),sg(slee)],de,[],
  [bob,
   dim(sleetje)]).

n([sg(sleep),pl(slepen)],de,[]).

n([pl(sleepboten),sg(sleepboot)],de,[]).

n([pl(sleeptouwen),sg(sleeptouw)],het,[]).

n([mass(sleet)],both,[]).   % celex: het; vk2001: de

n([sg(slem),pl(slems)],both,[],
  [f([klein]),
   f([groot])]).

n([pl(sletten),sg(slet)],de,[],[dim(sletje)]).

n([sg(sleuf),pl(sleuven)],de,[]).

n([mass(sleur)],de,[]).

n([pl(sleutels),sg(sleutel)],de,
  [subject_sbar], % de sleutel tot het succes is dat we heel hard werken
  []).

n([pl(sleutels),sg(sleutel)],de,[],
  [auto,			% waarom altijd meervoud???
   huis,
   muziek,
   verdeel,
   dim(sleuteltje)]).

n([pl(sleutelgaten),sg(sleutelgat)],het,[]).

n([mass(slib)],het,[]).

n([pl(slierten),sg(sliert)],de,[measure]).

n([mass(slijk)],het,[]).

n([pl(slijmen),sg(slijm)],het,[]).

n([pl(slijmvliezen),sg(slijmvlies)],het,[]).

n([mass(slijtage)],de,[]).

n([sg(slijter),pl(slijters)],de,[]).

n([sg(slijterij),pl(slijterijen)],de,[]).

n([mass(slik),pl(slikken)],het,[]).

n([sg(slikker),pl(slikkers)],de,[]).

n([pl(slingers),sg(slinger)],de,[measure],
  [dim(slingertje)]).		% slingers papier/spaghetti

n([pl(slips),sg(slip),pl(slippen)],de,[],[dim(slipje)]).

n([sg(slipper),pl(slippers)],de,[]).

n([pl(sloepen),sg(sloep)],de,[]).

n([pl(sloffen),sg(slof)],de,[measure]).

n([pl(slogans),sg(slogan)],de,[sbar,start_app_measure],
  [s(verkiezing)]).

n([pl(slokken),sg(slok)],de,[measure],[dim(slokje)]).

n([pl(slokdarmen),sg(slokdarm)],de,[]).

n([sg(sloof),pl(sloven)],de,[]).

n([mass(sloop),pl(slopen),sg(sloop)],both,[],
  [kussen % en niet kus_sloop
  ]).

n([pl(sloten),sg(sloot)],de,[measure],
  []).

n([pl(sloten),sg(sloot)],de,[],
  [tussen,
   dim(slootje)]).

n([pl(sloppen),sg(slop)],het,[]).

n([pl(slordigheden),sg(slordigheid)],de,[sbar,vp]).

n([sg(slot)],het,[pred_pp(op)],
  []).

n([pl(sloten),sg(slot)],het,[],
  [alcohol,
   cijfer,
   cilinder,
   hang,
   nacht,
   dim(slotje)]).

n([pl(slotsommen),sg(slotsom)],de,[sbar]).

n([sg([slow,motion])],de,[]).

n([sg([slow,starter]),pl([slow,starters])],de,[]).

n([pl(sluiers),sg(sluier)],de,[],
  [grauw  % en niet grauw_luier
  ]).

n([mass(sluimer)],de,[]).

n([pl(sluizen),sg(sluis)],de,[],
  [hemel,  % not to be lemmatized as: "hemel_luis"
   schut,
   zee]).

n([pl(sluitingen),pl(sluitings),sg(sluiting)],de,[]).

n([pl(sluitstukken),sg(sluitstuk)],het,[]).

n([pl(slungels),sg(slungel)],de,[]).

n([mass(slurp)],de,[]).

n([pl(slurven),sg(slurf)],de,[]).

n([pl(sluwheden),sg(sluwheid)],de,[sbar,vp]).

n([mass(smaad)],de,[]).

n([pl(smaken),sg(smaak)],de,[sbar,vp],[bij]).

n([pl(smaakmakers),sg(smaakmaker)],de,[app_measure]).

n([mass(smacht)],de,[]).  % "ik kijk al dagen met smacht uit naar ..."

n([pl(smakken),sg(smak)],de,[measure]).

n([pl(smaragden),sg(smaragd)],de,[]).

n([pl(smarten),sg(smart)],de,[vp]).

n([sg([smart,agent]),pl([smart,agents])],de,[]).

n([sg([smart,card]),pl([smart,cards])],de,[]).

n([sg(smartphone),pl(smartphones),
   sg('smart-phone'),pl('smart-phones'),
   sg([smart,phone]),pl([smart,phones])],de,[]).

n([sg(smartshop),pl(smartshops),
   sg('smart-shop'), pl('smart-shops'),
   sg([smart,shop]),pl([smart,shops])],de,[]).

n([sg(smartwatch),pl(smartwatches),
   sg('smart-watch'),pl('smart-watches'),
   sg([smart,watch]),pl([smart,watches])],de,[]).

n([sg(smartlap),pl(smartlappen)],de,[]).

n([pl(smeekbeden),pl(smeekbedes),sg(smeekbede)],de,[vp]).

n([mass(smeer)],both,[]).  % VL?

n([pl(smeerlappen),sg(smeerlap)],de,[]).

n([pl(smeersels),sg(smeersel)],het,[],[dim(smeerseltje)]).

n([sg(smeerwortel),pl(smeerwortels)],de,[]).  % geen samenstelling, dit is een wilde plant

n([mass(smelt)],de,[]).  % niet alleen een ijsbaan in Assen

n([sg(smeltpunt),pl(smeltpunten)],het,[]).

n([pl(smerissen),sg(smeris)],de,[]).

n([pl(smetten),sg(smet)],de,
  [subject_sbar, % dat ze niet vaker scoorde was het enige smetje
   sbar,
   vp],
  [dim(smetje)]).

n([pl(smeden),sg(smid)],de,[],
  [goud,
   wapen,
   zilver]).

n([pl(smidsen),sg(smidse)],de,[]).

n([pl(smoelen),sg(smoel)],de,[],[dim(smoeltje)]).

n([pl(smoezen),sg(smoes)],de,[sbar,subject_vp,vp],[dim(smoesje)]).

n([mass(smog)],de,[]).

n([pl(smokings),sg(smoking)],de,[]).

n([sg([smoking,gun]),pl([smoking,guns])],de,[]).

n([mass(smokkel)],de,[],
  [cocaïne,
   drug,s(drug),
   i(mens,mensen),
   i(vrouw,vrouwen)]).

n([pl(smokkelaars),sg(smokkelaar)],de,[],
  [cocaïne,
   drug,s(drug),
   i(mens,mensen),
   i(vrouw,vrouwen)]).

n([mass(smoor)],de,[]).

n([mass(sms),mass([short,message,service])],de,[],
  [dim(smsje),
   dim('sms-je'),
   dim('sms\'je'),
   dim('SMS\'je'),
   dim('SMS-je')]).

n([sg(smurf),pl(smurfen),
   sg('Smurf'),pl('Smurfen')],de,[]).

n([pl(snaren),sg(snaar)],de,[],[dim(snaartje)]).

n([pl(snackbars),sg(snackbar)],de,[]).

n([sg(snack),pl(snacks)],de,[]).

n([sg(snapper),pl(snappers)],de,[]). % vissoort

n([sg(snater),pl(snaters)],de,[]).

n([sg(snauw),pl(snauwen)],de,[]).

n([pl(snavels),sg(snavel)],de,[],[dim(snaveltje)]).

n([pl(sneden),sg(snede)],de,[]).

n([pl(sneeën),sg(snee),pl(sneden)],de,[measure],[dim(sneetje)]).

n([sg(sneer),pl(sneren)],de,[]).

n([mass(sneeuw)],de,[]).

n([sg(sneeuwman),pl(sneeuwmannen)],de,[]). % verschrikkelijke

n([pl(sneeuwstormen),sg(sneeuwstorm)],de,[]).

n([pl(sneeuwvlokken),sg(sneeuwvlok)],de,[],[dim(sneeuwvlokje)]).

n([pl(snelheden),sg(snelheid)],de,[vp]).

n([pl(snelheden),sg(snelheid)],de,[],
  [licht,
   maximum,
   top,
   wind]).

n([mass(snert)],de,[]).

n([pl(snijtanden),sg(snijtand)],de,[]).

n([pl(snikken),sg(snik)],de,[]).

n([pl(snippers),sg(snipper)],de,[measure],[dim(snippertje)]).

n([pl(snitten),sg(snit)],de,[pred_pp(van)]).

n([mass(snobisme)],het,[]).

n([pl(snoeken),sg(snoek)],de,[]).

n([mass(snoei)],de,[]).

n([mass(snoep)],both,[],[dim(snoepje)]).

n([mass(snoepgoed)],het,[]).

n([pl(snoeren),sg(snoer)],het,[measure],[dim(snoertje)]).

n([pl(snoeten),sg(snoet)],de,[],[dim(snoetje)]).

n([sg(snooker),pl(snookers)],both,[]).

n([pl(snorren),sg(snor)],de,[],[dim(snorretje)]).

n([mass(snot)],het,[]).

n([pl(snotneuzen),sg(snotneus)],de,[]).

n([sg(snotter),pl(snotters)],de,[]). % VL snotneus

n([sg(snowboard),pl(snowboards)],both,[]).

n([sg(snowboarder),pl(snowboarders)],de,[]).

n([sg(snowboardster),pl(snowboardsters)],de,[]).

n([sg(snuf),pl(snuffen)],de,[measure,sbar],[dim(snufje)]).

n([sg(snuif),pl(snuiven)],de,[measure],[dim(snuifje)]).  % cocaine

n([pl(snuisterijen),sg(snuisterij)],de,[],dim([snuisterijtje])).

n([pl(snuiten),sg(snuit)],de,[],[dim(snuitje)]).

n([pl(snuiters),sg(snuiter)],de,[],[dim(snuitertje)]).

n([sg(snurk)],de,[]).

%% ook: schriftelijke overhoring aardrijkskunde
n([sg(so),pl('so\'s')],de,[app_measure]).

n([sg(soap),pl(soaps)],de,[]).

n([sg([soap,opera]),pl([soap,'opera\'s'])],de,[]).

n([mass(soberheid)],de,[]).

n([stem(sociaal_democraat),
   pl('sociaal-democraten'),sg('sociaal-democraat'),
   pl([sociaal,democraten]),sg([sociaal,democraat]),
   pl('sociaal-demokraten'),sg('sociaal-demokraat'),
   pl([sociaal,demokraten]),sg([sociaal,demokraat]),
   pl(sociaaldemocraten),sg(sociaaldemocraat),
   pl(sociaaldemokraten),sg(sociaaldemokraat)],de,[]).

n([stem(sociaal_democratie),
   mass('sociaal-democratie'),
   mass([sociaal,democratie]),
   mass('sociaal-demokratie'),
   mass([sociaal,demokratie]),
   mass(sociaaldemocratie),
   mass(sociaaldemokratie)],de,[]).

n([stem(sociaal_psycholoog),
   pl('sociaal-psychologen'),sg('sociaal-psycholoog'),
   pl([sociaal,psychologen]),sg([sociaal,psycholoog])],de,[]).

n([stem(sociaal_raadslid),
   pl('sociaal-raadslieden'),sg('sociaal-raadslid'),
   pl([sociaal,raadslieden]),sg([sociaal,raadslid])],de,[]).

n([stem(sociaal_rechercheur),
   pl('sociaal-rechercheurs'),sg('sociaal-rechercheur'),
   pl([sociaal,rechercheurs]),sg([sociaal,rechercheur])],de,[]).

n([pl(socialisaties),sg(socialisatie),
   pl(socializaties),sg(socializatie)],de,[]).

n([mass(socialisme)],het,[]).

n([pl(socialisten),sg(socialist)],de,[]).

n([pl(socialistes),sg(socialiste)],de,[]).

n([sg(societeit),pl(societeiten)],de,[]).

n([mass(society)],de,[]).

n([mass(sociologie)],de,[],
  [cultuur]).

n([pl(sociologen),sg(socioloog)],de,[]).

n([pl(sociëteiten),sg(sociëteit)],de,[]).

n([mass(soda)],de,[]).

n([pl(sodemieters),sg(sodemieter)],de,[]).

n([mass(soelaas)],het,[]).

n([sg(soenniet),pl(soennieten),
   sg(sunniet),pl(sunnieten)],de,[]).

n([pl(soepen),sg(soep)],de,[],
  [i(erwt,erwten),
   i(kip,kippen),i(kip,kippe),
   pinda,
   i(tomaat,tomaten),
   i(ui,uien),
   vis,
   dim(soepje)]).

n([mass(soepelheid)],de,[]).

n([sg(soes),pl(soezen)],de,[]).

n([sg(soeverein),pl(soevereinen)],de,[]).

n([mass(soevereiniteit)],de,[]).

n([pl('sofa\'s'),sg(sofa)],de,[],[dim(sofaatje)]).

n([stem(softdrug),
   pl([soft,drugs]),sg([soft,drug]),
   pl('soft-drugs'),sg('soft-drug'),
   pl(softdrugs),sg(softdrug)],de,[]).

n([mass(software)],de,[],
  [pauze,
   pauzeer,
   sjoemel]).

n([sg([software,engineer]),
   pl([software,engineers])],de,[]).

n([mass([software,engineering])],de,[]).

n([mass(soja)],de,[]).

n([pl(sokken),sg(sok)],de,[]).

n([sg(sokkel),pl(sokkels)],de,[]).

%% wikipedia: Een sokpop is de benaming voor een kloon of tweede identiteit of account op een site of forum van één gebruiker.
n([sg(sokpop),pl(sokpoppen)],de,[]).

n([pl(sols),sg(sol)],de,[]).

n([pl(soldaten),sg(soldaat)],de,[],
  [kind,
   s(regering),
   dim(soldaatje)]).

n([mass(soldeer)],both,[]).

n([pl(solden)],both,[]). % VL uitverkoop?

n([pl(soldijen),sg(soldij)],de,[]).

n([mass(solidariteit)],de,[]).

n([pl(solisten),sg(solist)],de,[]).

n([pl(sollicitanten),sg(sollicitant)],de,[]).

n([pl(sollicitaties),sg(sollicitatie)],de,[]).

n([pl('solo\'s'),sg(solo)],both,[]).

n([sg(solvabiliteit),pl(solvabiliteiten)],de,[]).

n([pl(sommen),sg(som)],de,[measure],
  [dim(sommetje)]).

n([pl(sommen),sg(som)],de,[],
  [etmaal,
   reken,
   waarborg]).

n([mass(somberheid)],de,[]).

n([pl('sombrero\'s'),sg(sombrero)],de,[]).

n([sg(sommatie),pl(sommaties)],de,[vp,sbar]).

n([pl(sonaten),pl(sonates),sg(sonate)],de,[]).

n([pl(sondes),sg(sonde)],de,[],
  [ruimte]).

n([pl(songs),sg(song)],de,[],
  [folk,
   f([love]),
   rock]).

n([pl(sonnetten),sg(sonnet)],het,[],[dim(sonnetje)]).

n([pl(soorten),sg(soort)],both,[measure,pred_pp(van)],[]).

n([pl(soorten),sg(soort)],both,
  [app_measure],
  [dier,
   hout,
   kever,
   koraal,
   i(krab,krabben),
   i(libel,libellen),  % en geen libelle
   i(mier,mieren),
   i(mug,muggen),
   onder,
   i(plant,planten),
   i(slak,slakken),
   i(spin,spinnen),
   spons,
   type,
   i(vis,vissen),vis,
   i(vlieg,vliegen),
   vogel,
   werk]).

n([sg(soos),pl(sozen)],de,[]).

n([pl(soppen),sg(sop)],het,[]).

n([sg(sopraan),pl(sopranen)],de,[],
  [mezzo]).

n([sg(sorbet),pl(sorbets)],de,[]).

n([mass(sores),pl(sores)],de,[]).

n([mass(sorry)],het,[]).  % je moet even sorry zeggen; duizend keer sorry voor ..

n([sg(soufflé),pl(soufflés)],de,[]).

n([mass(soul)],de,[]).

n([pl(sounds),sg(sound)],de,[],
  [paling]).

n([sg(soundtrack),pl(soundtracks)],de,[]).

n([pl(soupers),sg(souper)],het,[],[dim(soupeetje)]).

n([mass(souplesse)],de,[]).

n([pl(souterrains),sg(souterrain)],het,[]).

n([pl(souvenirs),sg(souvenir)],het,[],[dim(sourveniertje)]).

n([mass(souvereiniteit)],de,[]).

n([stem('Sovjet'),
   pl(sovjets),sg(sovjet),
   pl(sowjets),sg(sowjet),
   pl('Sovjets'),sg('Sovjet'),
   pl('Sowjets'),sg('Sowjet')],de,[]).

n([sg(spaceshuttle),
   pl(spaceshuttles),
   sg('space-shuttle'),
   pl('space-shuttles'),
   sg([space,shuttle]),
   pl([space,shuttles])],de,[]).

n([pl(spaken),sg(spaak)],de,[]).

n([sg(spaarder),pl(spaarders)],de,[]).

n([pl(spaarpotten),sg(spaarpot)],de,[]).

n([pl(spaden),sg(spade)],de,[]).

n([sg(spagaat),pl(spagaten)],de,[]).

n([mass(spagetti)],de,[]).

n([mass(spaghetti)],de,[]).

n([mass(spam)],both,[]).

n([pl(spannen),sg(span)],het,[measure],[dim(spannetje)]).

n([pl(spandoeken),sg(spandoek)],both,[]).

n([sg(spanner),pl(spanners)],de,[]).  % insect

n([pl(spanningen),sg(spanning)],de,[]).

n([sg(spant),pl(spanten)],het,[]).

n([pl(sparren),sg(spar)],de,[],[dim(sparretje)]).

n([sg('Spakenburger'),pl('Spakenburgers')],de,[]).

n([sg('Spartaan'),pl('Spartanen')],de,[]).

n([pl(spatten),sg(spat)],de,[measure],[dim(spatje)]).

n([pl(spataderen),pl(spataders),sg(spatader)],de,[]).

n([sg(spatel),pl(spatels)],de,[]).

n([sg(speaker),pl(speakers)],de,[]).

n([pl(specerijen),sg(specerij)],de,[app_measure]).

n([pl(spechten),sg(specht)],de,[]).

n([pl([special,effects])],de,[]).

n([sg([special,committee])],het,[]).

n([pl(specials),sg(special)],de,[]).

n([sg([special,effect]),pl([special,effects])],het,[]).

n([pl(specialisaties),sg(specialisatie),
   pl(specializaties),sg(specializatie)],de,[vp,app_measure]).

n([pl(specialismen),sg(specialisme)],het,[sbar,vp,app_measure]).

n([pl(specialisten),sg(specialist)],de,[app_measure],
  [fractie]).

n([pl(specialistes),sg(specialiste)],de,[app_measure]).

n([pl(specialiteiten),sg(specialiteit)],de,[app_measure]).

n([pl(species),sg(specie)],de,[],[bagger]).

n([pl(specificaties),pl(specificatiën),sg(specificatie)],de,[]).

n([pl(specifikaties),pl(specifikatiën),sg(specifikatie)],de,[]).

n([pl(specimens),pl(specimina),sg(specimen)],het,[]).

n([pl(spectra),pl(spectrums),sg(spectrum)],het,[]).

n([mass(speculaas)],both,[],[dim(speculaasje)]).

n([pl(speculanten),sg(speculant)],de,[]).

n([pl(speculaties),sg(speculatie)],de,[sbar]).

n([pl(speechen),pl(speeches),sg(speech)],de,[]).

n([pl(speeds),sg(speed)],de,[]).

n([mass(speeksel)],het,[]).

n([pl(speelballen),sg(speelbal)],de,[]).

n([pl(speelgoederen),sg(speelgoed)],het,[]).

n([sg(speelronde),pl(speelronden),pl(speelrondes)],de,[]).

n([mass(speelsheid)],de,[]).

n([pl(speelsters),sg(speelster)],de,[],
  [badminton,
   cirkel,
   hoek,
   i(hoofd_rol,hoofdrol),
   jeugd,
   mede,
   ster,
   tegen,
   tennis,
   toneel,
   top]).

n([sg(speeltje),pl(speeltjes)],het,[]).

n([pl(speeltuinen),sg(speeltuin)],de,[np_app_measure]).

n([pl(speelwijzen),sg(speelwijze)],de,[]).

n([pl(speelzalen),sg(speelzaal)],de,[np_app_measure]).

n([pl(spenen),sg(speen)],de,[],[dim(speentje)]).

n([pl(speren),sg(speer)],de,[],[dim(speertje)]).

n([mass(spek)],both,[],[ontbijt]).

n([pl(spektakels),sg(spektakel)],het,[vp],
  [theater]). % en niet theater_pek_takel

n([sg(spektakelstuk),pl(spektakelstukken)],het,[]).

n([pl(spektra),pl(spektrums),sg(spektrum)],het,[]).

n([pl(spekulanten),sg(spekulant)],de,[]).

n([pl(spekulaties),sg(spekulatie)],de,[sbar]).

n([pl(spelen),pl(spellen),sg(spel)],het,
  [measure,			% een spel kaarten
   pred_pp(buiten),               % dat is toch duidelijk buiten spel
   vp],
  [dim(spelletje)]).

n([pl(spelen),pl(spellen),sg(spel)],het,[app_measure],
  [bal,
   bord,
   computer,
   eind,
   kaart,
   i(kind,kinder),
   simulatie,
   treur,
   h(tv),
   video,
   voetbal,
   dim(spelletje)]).

n([pl(spelen),pl(spellen),sg(spel)],het,[],
  [indoor,
   piramide,
   positie,
   schaak,
   steek
  ]).

n([pl(spelden),sg(speld)],de,[],[dim(speldje)]).

n([pl(spelers),sg(speler)],de,[],
  [cassette,
   h(cd),i(cd,'CD-'),
   compactdisc,h(compactdisc),
   h(dvd),i(dvd,'DVD-'),
   lang,
   i(plaat,platen)]).

n([pl(spelers),sg(speler)],de,[],
  [achter,
   basis,
   buiten,
   'Davis-Cup',
   i(hoofd_rol,hoofdrol),
   jeugd,
   mede,
   h(oud),oud,
   selectie,
   ster,
   tegen,
   tennis,
   toneel,
   top,
   voor,
   wit,
   zwart]).

n([pl(spelingen),sg(speling)],de,[]).

n([pl(spellingen),sg(spelling)],de,[]).

n([pl(spelonken),sg(spelonk)],de,[]).

n([pl(spelregels),sg(spelregel)],de,[sbar,vp]).

n([mass(spelt)],de,[]).

n([pl(spelverdelers),sg(spelverdeler)],de,[]).

n([pl(spelverdeelsters),sg(spelverdeelster)],de,[]).

n([pl(spencers),sg(spencer)],de,[]).

n([mass(sperma)],het,[]).

n([sg(sperwer),pl(sperwers)],de,[]).

n([sg(spetter),pl(spetters)],de,[]).

n([pl(speurders),sg(speurder)],de,[]).

n([pl(spiegels),sg(spiegel)],de,[],
  [achteruitkijk,
   binnen,
   buiten,
   dodehoek,
   'make-up',
   medaille,
   water,
   zee,
   zij,
   dim(spiegeltje)]).

n([pl(spieren),sg(spier)],de,[],
  [borst, % en geen borst_pier
   buik,
   hart,
   kaak,
   kuit,
   lach,
   dim(spiertje)]).

n([sg(spierversterker),pl(spierversterkers)],de,[app_measure]).

n([pl(spiervezels),sg(spiervezel)],de,[]).

n([sg(spijbelaar),pl(spijbelaars)],de,[]).

n([pl(spijkers),sg(spijker)],de,[],[dim(spijkertje)]).

n([pl(spijkerbroeken),sg(spijkerbroek)],de,[]).

n([pl(spijlen),sg(spijl)],de,[],[dim(spijltje)]).

n([pl(spijzen),sg(spijs)],de,[]).

n([mass(spijsvertering)],de,[]).

n([mass(spijt)],de,[sbar]).

n([sg(spijtbetuiging),pl(spijtbetuigingen)],de,[]).

n([sg(spijtoptant),pl(spijtoptanten)],de,[]).

n([pl(spikkels),sg(spikkel)],de,[],[dim(spikkeltje)]).

n([pl(spillen),sg(spil)],de,[],[dim(spilletje)]).

n([pl(spinnen),sg(spin)],de,[],
  [i(bodem_jacht,bodemjacht),
   hangmat,
   i(jacht_krab,jachtkrab),
   i(klap_deur,klapdeur),
   kogel,
   i(kraam_web,kraamweb),
   krab,
   i(pantser_zak,pantserzak),
   spring,
   suiker,
   tril,
   i(val_deur,valdeur),
   wielweb,
   wolf,
   dim(spinnetje)]).

n([sg([spin,doctor]),pl([spin,doctors])],de,[]).

n([sg('spin-off'),pl('spin-offs')],de,[]).

n([mass(spinazie)],de,[]).

n([pl(spinnenwebben),sg(spinnenweb),pl(spinnewebben),sg(spinneweb)],het,[]).

n([sg(spint)],de,[]).  % oude oppervlaktemaat 

n([sg(spint)],het,[]).  % hout

n([pl(spionnen),sg(spion)],de,[]).

n([pl(spionnes),sg(spionne)],de,[]).

n([mass(spionage)],de,[],[cyber]).

n([pl(spiralen),sg(spiraal)],both,[],[dim(spiraaltje)]).

n([mass(spiritualiteit)],de,[]).

n([mass(spiritus)],de,[]).

n([pl(speten),pl(spitten),sg(spit)],het,[]).

n([pl(spitsen),sg(spits)],both,[],
  [centrum,
   linker,
   rechter,
   schaduw]).

n([pl(spitsuren),sg(spitsuur)],het,[]).

n([pl(spleten),sg(spleet)],de,[],[dim(spleetje)]).

n([pl(splijtstoffen),sg(splijtstof)],de,[]).

n([pl(splinters),sg(splinter)],de,[measure],[dim(splintje)]).

n([sg(split),pl(splitten)],de,[]).

n([sg([split,screen]),pl([split,screens])],both,[]).

n([sg(splits),pl(splitsen)],de,[]).

n([pl(splitsingen),sg(splitsing)],de,[],[h('T')]).

n([mass(spoed)],de,[]).

n([pl(spoedzittingen),sg(spoedzitting)],de,[]).

n([pl(spoelen),sg(spoel)],de,[],[dim(spoeltje)]).

n([pl(spoetniks),sg(spoetnik)],de,[]).

n([sg(spoiler),pl(spoilers)],de,[]).

n([pl(spokerijen),sg(spokerij)],de,[]).

n([pl(sponsen),pl(sponzen),sg(spons)],de,[]).

n([pl(sponsors),pl(sponsoren),sg(sponsor)],de,[],
  [hoofd]).

n([mass(sponsoring)],de,[]).

n([mass(spontaniteit)],de,[]).

n([pl(spoken),sg(spook)],het,[]).

n([pl(spookhuizen),sg(spookhuis)],het,[]).

n([sg(spoor),pl(sporen)],de,[]). % van een bloem

n([pl(sporen),sg(spoor)],het,
  [pred_pp(op),
   measure],
  []).

n([pl(sporen),sg(spoor)],het,
  [],
  [bever,
   bloed,
   i(kar,karren),
   dim(spoortje)]).

n([pl(spoorbanen),sg(spoorbaan)],de,[]).

n([pl(spoorbruggen),sg(spoorbrug)],de,[]).

n([pl(spoorwegen),sg(spoorweg)],de,[]).

n([sg(spouw)],de,[]).

n([pl(sporten),sg(sport)],de,[app_measure],
  [amateur,
   atletiek,
   auto,
   bal,
   berg,
   biljart,
   binnen,
   i(bob_slee,bobslee),
   boks,
   breedte,
   bridge,
   buiten,
   competitie,
   dam,
   dans,
   dart,
   demonstratie,
   denk,
   draf,
   dressuur,
   i(duif,duiven),
   duik,
   gevecht,s(gevecht),
   golf,
   handboog,
   hengel,
   hockey,
   i(hond,honden),
   honkbal,
   kijk,
   klim,
   korfbal,
   kracht,
   medaille,
   men, % paardrijen
   motor,
   i(paard,paarden),
   i(paard,paarde),
   rally,
   ren,
   roei,
   rugby,
   ruiter,
   schaak,
   schaats,
   scherm,
   ski,
   snelheid,
   spring,
   super,
   tafeltennis,
   team,
   tennis,
   theater,
   top,
   vecht,
   vlieg,
   voetbal,
   s(volk),
   volleybal,
   water,
   wedstrijd,
   wieler,
   winter,
   zeil,
   zwem]).

n([pl(sporters),sg(sporter)],de,[],
  [top,
   vecht,
   water,
   winter]).

n([pl(sportsters),sg(sportster)],de,[],
  [top,
   vecht,
   water,
   winter]).

n([pl(sporthallen),sg(sporthal)],de,[]).

n([pl(sportlieden),pl(sportlui),pl(sportmannen),sg(sportman)],de,[]).

n([pl(sportvelden),sg(sportveld)],het,[]).

n([sg(sportvrouw),sg(sportvrouwe),pl(sportvrouwen)],de,[]).

n([mass(spot),pl(spots),sg(spot)],de,[],[reclame]).

n([sg(spotje),pl(spotjes)],het,[],
  [radio,
   reclame,
   televisie,
   tv,
   dim(spotje)]).

n([sg(spotter),pl(spotters)],de,[],
  [vliegtuig,
   vogel]).

n([mass(spraak)],de,[]).

n([mass(sprake)],de,[]).

n([pl(sprankels),sg(sprankel)],both,[measure],[dim(sprankeltje)]).

n([pl(sprankjes),sg(sprankje)],het,[measure]).

n([pl(spreekbeurten),sg(spreekbeurt)],de,[]).

n([pl(spreekbuizen),sg(spreekbuis)],de,[]).

n([pl(spreekgestoelten),pl(spreekgestoeltes),sg(spreekgestoelte)],het,[]).

n([pl(spreekkoren),sg(spreekkoor)],het,[]).

n([sg(spreekster),pl(spreeksters)],de,[]).

n([pl(spreekuren),sg(spreekuur)],het,[]).

n([pl(spreekwoorden),sg(spreekwoord)],het,[start_app_measure]).

n([pl(spreeuwen),sg(spreeuw)],de,[],[dim(spreeuwtje)]).

n([pl(spreien),sg(sprei)],de,[],[dim(spreitje)]).

n([mass(spreiding)],de,[]).

n([pl(sprekers),sg(spreker)],de,[]).

n([pl(spreuken),sg(spreuk)],de,[sbar,start_app_measure]).

n([pl(sprieten),sg(spriet)],de,[measure],
  [dim(sprietje)]).

n([pl(sprieten),sg(spriet)],de,[],
  [gras,
   voel,
   dim(sprietje)]).

n([pl(springruiters),sg(springruiter)],de,[]).

n([pl(sprinkhanen),sg(sprinkhaan)],de,[],
  [hoorn,
   sabel,
   veld]).

n([pl(sprinten),pl(sprints),sg(sprint)],de,[],
  [bonificatie,
   eind,
   massa,
   tussen]).

n([pl(sprinters),sg(sprinter)],de,[]).

n([pl(sprintsters),sg(sprintster)],de,[]).

n([pl(sproeten),sg(sproet)],de,[]).

n([pl(sprongen),sg(sprong)],de,[],
  [i(bok,bokken),
   koers,
   parachute,
   tussen,
   dim(sprongetje)]).

n([pl(sprookjes),sg(sprookje)],het,[],
  [s(volk)]).

n([sg(spruit),pl(spruiten)],de,[],[dim(spruitje)]).

n([sg(spuigat),pl(spuigaten)],het,[]).

n([pl(spuiten),sg(spuit)],de,[],[dim(spuitje)]).

n([pl(spuitbussen),sg(spuitbus)],de,[],[dim(spuitbusje)]).

n([pl(spullen),sg(spul)],het,[],[dim(spulletje)]).

n([pl(spurts),pl(spurten),sg(spurt)],de,[],[massa]).

n([mass(spuug)],both,[]).

n([mass(squash)],het,[]).

n([sg([sta,in,de,weg]),sg('sta-in-de-weg')],de,[]).

n([pl(staven),sg(staaf)],de,
  [measure],
  [dim(staafje)]).		% een staaf tin

n([pl(staven),sg(staaf)],de,
  [],
  [brandstof,
   drop,
   dim(staafje)]).

n([pl(staken),sg(staak)],de,[]).

n([mass('staakt-het-vuren'),
   mass(['staakt-het',vuren]),
   mass('staakt-hetvuren'),
   mass([staakt,het,vuren])],both,[]).

n([pl(stalen),sg(staal)],both,[measure],
  [i(vee_voeder,veevoeder),
   dim(staaltje)]).

n([mass(staar)],de,
  [],
  [na]).

n([pl(staarten),sg(staart)],de,[measure],
  [dim(staartje)]).

n([pl(staarten),sg(staart)],de,[],
  [een,  % eenstaartjes
   kam,  % kamstaartjes
   i(paard,paarden),
   dim(staartje)]).

n([pl(staten),sg(staat)],de,
  [vp,
   pred_pp(in)],[]).

n([pl(staten),sg(staat)],de,
  [],
  [dim(staatje)]).

n([pl(staten),sg(staat)],de,[],
  [s(eenheid),
   h(lid),
   recht,s(recht),
   i(schurk,schurken),
   stad,
   s(verzorging),
   s(welvaart)]).

n([sg(staatkunde)],de,[]).

n([sg(state)],de,[]).

n([mass(staatsbestel)],het,[]).

n([pl(staatsgrepen),sg(staatsgreep)],de,[]).

n([pl(staatshoofden),sg(staatshoofd)],het,[]).

n([pl(staatslieden),pl(staatslui),pl(staatsmannen),sg(staatsman)],de,[]).

n([pl(staatssecretarissen),sg(staatssecretaris),
   pl(staatsecretarissen),sg(staatsecretaris)],de,[app_measure],
  [h(oud)]).

n([sg(staatssecretariaat),
   pl(staatssecretariaten),
   sg(staatsecretariaat),
   pl(staatsecretariaten)],het,[app_measure]).

n([pl(stabilisaties),sg(stabilisatie),
   pl(stabilizaties),sg(stabilizatie)],de,[]).

n([mass(stabilisering),pl(stabiliseringen),sg(stabilisering)],de,[]).

n([mass(stabiliteit)],de,[],[prijs]).

n([pl(steden),sg(stad),
   ignore(m(stad,noun(de,count,sg),stad))],de,[],
  [beneden,
   binnen,
   boven,
   dubbel,
   s(fabriek),
   geboorte,
   industrie, % vond Braks van Tilburg en was er trots op
   haven,
   provincie,
   speel,
   voor,
   wereld,
   dim(stadje)]).

n([sg(stad)],both,[]).  % het: VL

n([pl(stadhouders),sg(stadhouder)],de,[]).

n([pl(stadhuizen),sg(stadhuis)],het,[]).

n([pl(stadions),sg(stadion)],het,[],
  [voetbal,
   dim(stadionnetje)]).

n([pl(stadia),pl(stadiums),sg(stadium)],het,
  [pred_pp(in)],
  [s(ontwikkeling)]).

n([pl(stadsdelen),sg(stadsdeel)],het,[app_measure],[]).

n([pl(stadsgewesten),sg(stadsgewest)],het,[app_measure],[]).

n([sg(stadsprovincie),pl(stadsprovincies)],de,[app_measure]).

n([pl(staven),sg(staf)],de,[],
  [defensie,
   dim(stafje)]).

n([pl(stages),sg(stage)],de,[],
  [hoogte,
   s(training)]).

n([pl(stagiaires),sg(stagiaire),sg(stagiair)],de,[]).

n([pl(stagnaties),sg(stagnatie)],de,[]).

%% weer zo'n woord dat verboden zou moeten worden!
n([sg(stakeholder),pl(stakeholders)],de,[]).

n([pl(stakers),sg(staker)],de,[],[honger]).

n([pl(stakingen),sg(staking)],de,[pred_pp(in)]).

n([pl(stakingen),sg(staking)],de,[],
  [seks,
   i(seks,sex),				% Aristophanes
   spoor,
   train]).

n([pl(stakkers),sg(stakker)],de,[],[dim(stakkertje)]).

n([pl(stallen),sg(stal)],de,[],
  [kerst,
   i(koe,koeien),
   i(paard,paarden),
   ren,
   s(varken),
   dim(stalletje)]).

n([sg(stalker),pl(stalkers)],de,[]).

n([pl(stalknechten),pl(stalknechts),sg(stalknecht)],de,[]).

n([pl(stallingen),sg(stalling)],de,[],
  [i(fiets,fietsen),
   dim(stallinkje)]).

n([pl(stammen),sg(stam)],de,[],[dim(stammetje)]).

n([pl(stambomen),sg(stamboom)],de,[]).

n([pl(stamhoofden),sg(stamhoofd)],het,[]).

n([sg(stamp),pl(stampen)],de,[]). % Vlaams

n([sg(stamper),pl(stampers)],de,[]).

n([pl(standen),sg(stand)],de,
  [pred_pp(beneden),
   pred_pp(beneden,subject_vp),
   pred_pp(beneden,subject_sbar),
   vp,
   sbar]).

n([pl(standen),sg(stand)],de,[],
  [diagram,
   eind,
   grondwater,
   rente,
   slot,
   totaal,
   vis,
   h('WK'),
   wild,
   dim(standje)]).

n([pl(stands),sg(stand)],de,[],[dim(standje)]).  % op een beurs ("stent")

n([sg('stand-in'),pl('stand-ins')],de,[]).

n([mass([stand,up]),mass('stand-up')],de,[]).

n([sg([stand,up,comedian]),pl([stand,up,comedians]),
   sg(['stand-up',comedian]),pl(['stand-up',comedians])],de,[]).

n([sg([standing,force]),pl([standing,forces])],de,[]).

n([pl(standaarden),pl(standaards),sg(standaard)],de,
  [measure,
   sbar,
   vp,
   subject_sbar,
   subject_vp
  ]).

n([pl(standaarden),pl(standaards),sg(standaard)],de,[],
  [goud,
   s(leven)
  ]).

n([mass(standaardisatie),mass(standaardizatie)],de,[]).

n([sg(standplaats),pl(standplaatsen)],de,[],
  [taxi]).

n([pl(standpunten),sg(standpunt)],het,
  [sbar,
   subject_sbar,
   subject_vp,
   vp]).

n([pl(stangen),sg(stang)],de,[],[dim(stangetje)]).

n([pl(stanken),sg(stank)],de,[]).

n([pl(stappen),sg(stap)],de,
  [vp,
   subject_sbar],
  [dim(stapje)]).

n([pl(stappen),sg(stap)],de,
  [],
  [rente,
   vervolg
  ]).

n([pl(stapels),sg(stapel)],de,[measure],
  [dim(stapeltje)]).

n([pl(stapels),sg(stapel)],de,[],
  [s(varken)]).

n([sg(stapelwolk),pl(stapelwolken)],de,[]).

n([sg(stapper),pl(stappers)],de,[]).  % en niet stap_pers

n([sg(star),pl(stars)],de,[]).

n([mass(starheid)],de,[]).

n([pl([stars,and,stripes]),
   pl([stars,en,stripes]),
   pl([stars,'&',stripes])],de,[]).

n([pl(starts),sg(start)],de,[],
  [i(band,banden),
   competitie,
   her,
   in,
   seizoen,s(seizoen)]).

n([pl(startbanen),sg(startbaan)],de,[]).

n([pl(starters),sg(starter)],de,[]).

n([sg(startup),sg('start-up'),pl(startups),pl('start-ups')],de,[]).

n([pl(statements),sg(statement)],het,[]).

n([pl(stations),sg(station)],het,[],
  [benzine,
   bus,
   eind,
   metro,
   h('NS'),
   pomp,
   radio,h(radio),
   ruimte,
   spoorweg,
   tank,
   televisie,
   trein,
   tussen,
   televisie,tv,h(tv),f([tv]),i(tv,'TV-'),
   dim(stationnetje)]).

n([sg(stationering),pl(stationeringen)],de,[]).

n([sg(stationwagon),pl(stationwagons)],de,[]).

n([pl(statistieken),sg(statistiek)],de,[]).

%% dat heeft de status rijksmonument
n([mass(status)],de,[app_measure,np_app_measure],
  []).

n([mass(status)],de,[],
  [h('A'),
   f([artikel,'12'])]).

n([mass([status,aparte])],de,[]).

n([mass([status,quo]),mass('status-quo')],both,[]).

n([mass([status,quo,ante])],de,[]).

n([pl(statuten),sg(statuut)],het,[]).

n([mass(stedebouw)],de,[]).

n([pl(stedelingen),sg(stedeling)],de,[]).

n([pl(steeën),sg(stee)],de,[]).

n([pl(stegen),sg(steeg)],de,[],
  [dim(steegje)]).

n([pl(steken),sg(steek)],de,[measure],
  [i(bij,bijen),
   i(wesp,wespen),
   dim(steekje)]).

n([pl(steekpenningen),sg(steekpenning)],de,[]).

n([pl(steekproeven),sg(steekproef)],de,[]).

n([pl(stelen),sg(steel)],de,[],
  [dim(steeltje),
   i(pijp,pijpen),
   i(pijp,pijpe)]).

n([pl(stenen),sg(steen)],both,[],
  [dim(steentje),
   gal,
   gedenk,
   gevel,
   lego,
   maal,
   molen,
   natuur,
   zand
  ]).

n([sg([steen,des,aanstoots]),
   pl([stenen,des,aanstoots])],de,[subject_sbar]).

n([pl(steenhouwers),sg(steenhouwer)],de,[]).

n([mass(steenkool),pl(steenkolen)],de,[]).

n([pl(steenpuisten),sg(steenpuist)],de,[]).

n([pl(steenworpen),sg(steenworp)],de,[measure]).

n([sg(steeple)],de,[]).

n([sg([steeple,chase])],de,[]).

n([pl(steigers),sg(steiger)],de,[],
  [aanleg,
   dim(steigertje)]).

n([pl(stekken),sg(stek)],de,[]).

n([pl(stekels),sg(stekel)],de,[],[dim(stekeltje)]).

n([sg(stekelbaars),pl(stekelbaarzen)],de,[]).

n([pl(stekkers),sg(stekker)],de,[],[dim(stekkertje)]).

n([pl(stellen),sg(stel)],het,[measure],[dim(stelletje)]).

n([pl(stellen),sg(stel)],het,[],
  [trein]).

n([pl(stellages),sg(stellage)],de,[]).

n([mass(stelligheid)],de,[]).

n([pl(stellingen),sg(stelling)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp],
  [dim(stellinkje)]).

n([pl(stellingen),sg(stelling)],de,[],
  [limiet,f([limiet]),h(limiet),
   kandidaat,
   kleur,
   prijs,
   [onder,bewind],
   dim(stellinkje)]).

n([pl(stellingnames),sg(stellingname)],de,[sbar,vp]).

n([pl(stelregels),sg(stelregel)],de,[sbar,vp]).

n([pl(stelsels),sg(stelsel)],het,[],
  [i(balk_spiraal,balkspiraal),  % iets met sterrenbeelden
   basis,
   belasting,
   cultuur,
   i(kultuur,cultuur),
   i(district,districten),
   i(district,distrikten),
   kies,
   i(leerling,leerlingen),
   melkweg,
   mini,h(mini),
   pensioen,
   s(recht),
   spiraal,
   i(ster,sterren),
   i(vergunning,vergunningen),
   zenuw,
   i(zon,zonne),
   i(zon,zonnen),
   zorg,
   dim(stelseltje)]).

n([pl(stelten),sg(stelt)],de,[]).

n([pl(stemmen),sg(stem)],de,[measure],   % zes stemmen meerderheid
  [dim(stemmetje)
  ]).

n([pl(stemmen),sg(stem)],de,[],
  [i(kind,kinder),
   tegen,
   voor,
   s(voorkeur),
   zang,
   dim(stemmetje)
  ]).

n([pl(stembanden),sg(stemband)],de,[]).

n([pl(stembussen),sg(stembus)],de,[]).

n([sg(stemmer),pl(stemmers)],de,[],
  [tegen,
   voor]).

n([pl(stemmingen),sg(stemming)],de,[],[eind]).

n([pl(stemmingen),sg(stemming)],de,[van_sbar]).

n([mass(stemmingmakerij)],de,[]).

n([pl(stempels),sg(stempel)],both,[],[dim(stempeltje)]).

n([pl(stemverheffingen),sg(stemverheffing)],de,[]).

n([pl(stens),sg(sten)],de,[]).

n([pl(stencils),sg(stencil)],het,[],[dim(stenciltje)]).

%% stengel bleekselderij
n([pl(stengels),sg(stengel)],de,[measure],[dim(stengeltje)]).

n([pl(stenguns),sg(stengun)],de,[]).

n([sg(steppe)],de,[]).

n([pl(steppen),sg(step)],de,[]).

n([pl(sterren),sg(ster)],de,[app_measure]).

n([pl(sterren),sg(ster)],de,[],
  [film,
   'Michelin',
   pop,
   reality,
   super,
   wereld,
   zee,
   dim(sterretje)]).

n([sg(stereo)],de,[]).

n([pl(stereotypen),pl(stereotypes),sg(stereotype),
   pl(stereotiepen),pl(stereotiepes),sg(stereotiepe)],both,[sbar,vp]).

n([pl(sterfgevallen),sg(sterfgeval)],het,[]).

n([mass(sterfte)],de,[]).

n([pl(sterilisaties),sg(sterilisatie),
   pl(sterilizaties),sg(sterilizatie)],de,[]).

n([pl(sterkten),pl(sterktes),sg(sterkte)],de,[subject_sbar],
  [s(geluid),
   wind]).

n([pl(sternen),pl(sterns),sg(stern)],de,[]).

n([pl(sterrenbeelden),sg(sterrenbeeld),
   pl(sterrebeelden),sg(sterrebeeld)],het,[np_app_measure]).

n([mass(sterrenhemel)],de,[]).

n([sg(sterrenkundige),pl(sterrenkundigen)],de,[]).

n([pl(stervelingen),sg(sterveling)],de,[]).

n([pl(stethoscopen),sg(stethoscoop)],de,[]).

n([pl(stethoskopen),sg(stethoskoop)],de,[]).

n([pl(stetoskopen),sg(stetoscoop)],de,[]).

n([pl(stetoscopen),sg(stetoskoop)],de,[]).

n([pl(steunen),sg(steun)],de,[],
  [h('EU'),
   export,
   hoofd,
   s(inkomen),
   lucht,			% niet in S.
   s(overheid),
   prijs,
   s(staat),
   dim(steuntje)]).

n([pl(steunen),sg(steun)],de,[subject_sbar,
			      subject_vp],
  []).

n([sg(steunbetuiging),pl(steunbetuigingen)],de,[]).

n([pl(steunpilaren),sg(steunpilaar)],de,[]).

n([pl(steunpunten),sg(steunpunt)],het,[sbar,vp,app_measure]).

n([pl(steuren),sg(steur)],de,[],[dim(steurtje)]).

n([pl(stevens),sg(steven)],de,[]).

n([mass(stevigheid)],de,[]).

n([sg(steward),pl(stewards)],de,[]).

n([pl(stewardessen),sg(stewardess)],de,[]).

n([pl(stichters),sg(stichter)],de,[]).

n([pl(stichtingen),sg(stichting)],de,
  [app_measure,
   np_app_measure],
  [s(welzijn),
   woning]).

n([sg(stick),pl(sticks)],de,[]).

n([pl(stickers),sg(sticker)],de,[],
  [h('ja-ja'),
   ['ja-ja'],
   h('Ja-Ja'),
   ['Ja-ja'],
   [ja,ja],
   ['Ja','Ja'],
   h('nee-nee'),
   ['nee-nee'],
   h('Nee-Nee'),
   ['Nee-nee'],
   [nee,nee],
   ['Nee','Nee'],
   h('ja-nee'),
   ['ja-nee'],
   h('Ja-Nee'),
   ['Ja-nee'],
   [ja,nee],
   ['Ja','Nee'],
   h('nee-ja'),
   ['nee-ja'],
   h('Nee-Ja'),
   ['Nee-ja'],
   [nee,ja],
   ['Nee','Ja']
  ]).

n([sg([sticky,tape,syndrome])],het,[]).

n([pl(stieren),sg(stier)],de,[],[dim(stiertje)]).

n([pl(stiften),sg(stift)],de,[]).

n([pl(stiften),sg(stift)],het,[]).  % iets van een klooster of zo?

n([pl('stigma\'s'),pl(stigmata),sg(stigma)],het,[sbar,vp]).

n([pl(stijfheden),sg(stijfheid)],de,[vp]).

n([pl(stijgbeugels),sg(stijgbeugel)],de,[]).

n([pl(stijgers),sg(stijger)],de,[]).

n([pl(stijgingen),sg(stijging)],de,[],
  [huur,
   koers,
   kosten,
   loon,
   omzet,
   prijs,
   rente,
   temperatuur,
   waarde,
   winst]).

n([pl(stijlen),sg(stijl)],de,[van_sbar],
  []).

n([pl(stijlen),sg(stijl)],de,[],
  [barok,
   bouw,
   huis,
   leef,
   s(leven),
   muziek,
   rij,
   schrijf,
   speel,
   teken,  % en niet teek_stijl
   dim(stijltje)]).

n([pl(stikkers),sg(stikker)],de,[]).

n([mass(stikstof)],both,[]).

n([pl(stillevens),sg(stilleven)],het,[]).

n([mass(stilstand)],de,[]).

n([pl(stilten),pl(stiltes),sg(stilte)],de,[],
  [media]).

n([mass(stilzwijgen)],het,[]).

n([pl(stimulansen),pl(stimulantia),sg(stimulans)],de,[vp]).

n([pl(stimulaties),sg(stimulatie)],de,[sbar,vp]).

n([mass(stimulering),pl(stimuleringen),sg(stimulering)],de,[]).

n([pl(stimuli),sg(stimulus)],de,[sbar,vp]).

n([pl(stippen),sg(stip)],de,[],
  [dim(stipje),
   midden]).

n([pl(stippels),sg(stippel)],de,[],[dim(stippeltje)]).

n([pl(stocks),sg(stock)],de,[]).

n([pl(stoelen),sg(stoel)],de,[],
  [achter,
   arm,
   biecht,
   bureau,
   s(chauffeur),
   draag,
   keuken,
   i(kind,kinder),
   leun,
   leuning,
   lig,
   s(passagier),
   preek,
   rol,
   schommel,
   strand,
   tuin,
   voor,
   dim(stoeltje)]).

n([mass(stoelgang)],de,[]).

n([pl(stoeltjesliften),sg(stoeltjeslift)],de,[]).

n([pl(stoepen),sg(stoep)],de,[]).

n([pl(stoepranden),sg(stoeprand)],de,[]).

n([pl(stoeten),sg(stoet)],de,[measure]).

n([pl(stoeten),sg(stoet)],de,[],[rouw]).

n([pl(stoffen),sg(stof)],de,
  [app_measure],
  [afval,
   bad,
   bouw,
   delf,
   diamant,  % en niet diamant_tof
   ent,
   fijn,
   s(gesprek),
   geur,
   gif,
   hulp,
   leer,
   lees,
   les,
   mest,
   oefen,
   smaak,
   spring,
   s(voeding),
   zoet,
   dim(stofje)]).

n([mass(stof)],het,[]).

n([sg([stoffelijk,overschot]),
   sg([stoffelijke,overschot]),
   pl([stoffelijke,overschotten]),
   pl([stoffelijk,overschotten])],het,[]).

n([pl(stofjassen),sg(stofjas)],de,[]).

n([mass(stofwisseling)],de,[]).

n([pl(stofwolken),sg(stofwolk)],de,[]).

n([pl(stofzuigers),sg(stofzuiger)],de,[]).

%% een stokje citroengras
n([pl(stokken),sg(stok)],de,[measure],
  [dim(stokje)]).

n([pl(stokken),sg(stok)],de,[],
  [tover,
   wapen,
   zuur]).

n([pl(stokers),sg(stoker)],de,[]).

n([pl(stokpaardjes),sg(stokpaardje)],het,[]).

n([pl(stokvissen),sg(stokvis)],de,[]).

n([pl(stollen),pl(stols),sg(stol)],de,[],[kerst]).

n([sg(stolling),pl(stollingen)],de,[],[bloed]).

n([pl(stolpen),sg(stolp)],de,[]).

n([sg('Stolperstein'),pl('Stolpersteine')],de,[]).

n([sg(stolsel),pl(stolsels)],both,[],[bloed]).

n([pl(stomheden),sg(stomheid)],de,[sbar,vp]).

n([pl(stommelingen),sg(stommeling)],de,[]).

n([pl(stommiteiten),sg(stommiteit)],de,[sbar,vp]).

n([pl(stompen),sg(stomp)],de,[],[dim(stompje)]).

n([mass(stond)],de,[]).  % van stond af

n([pl(stoven),sg(stoof)],de,[]).

n([pl(stoofpotten),sg(stoofpot)],de,[]).

n([mass(stoom)],de,[pred_pp(op)]).

n([pl(stoomboten),sg(stoomboot)],de,[]).

n([pl(stoommachines),sg(stoommachine)],de,[]).

n([pl(stopen),sg(stoop)],de,[]).

n([pl(stoornissen),sg(stoornis)],de,[],
  [angst,
   s(beweging),
   eet,
   erectie,
   s(persoonlijkheid),
   ritme]).

n([pl(stoten),sg(stoot)],de,[measure]).

n([pl(stoten),sg(stoot)],de,[],
  [elleboog,
   kop,
   dim(stootje)]).

n([pl(stoppen),pl(stops),sg(stop)],de,[],
  [bombardement,
   campagne,
   i(prijs,prijzen),
   vacature,
   zomer
  ]).

n([pl(stopcontacten),sg(stopcontact),
   pl(stopkontakten),sg(stopkontakt)],het,[]).

n([pl(stoppelen),pl(stoppels),sg(stoppel)],de,[],[dim(stoppeltje)]).

n([sg(stopper),pl(stoppers)],de,[]).

n([pl(stopwatches),sg(stopwatch)],de,[]).

n([sg(stopzetting),pl(stopzettingen)],de,[]).

n([sg(store),pl(stores)],de,[],
  [f([app]),
   f([apple]),
   f(['Apple']),
   f(['Mac']),
   f(['Mac',app])
  ]).

n([pl(storingen),sg(storing)],de,[],
  [computer,
   stroom]).

n([pl(stormen),sg(storm)],de,[],[dim(stormpje)]).

n([pl(stormlopen),sg(stormloop)],de,[]).

n([pl(stormvloedkeringen),sg(stormvloedkering)],de,[]).

n([sg(stort)],both,[]).

n([pl(stortingen),sg(storting)],de,[]).

n([pl(stortvloeden),sg(stortvloed)],de,[]).

n([pl('story\'s'),sg(story)],de,[]).

n([pl(stralen),sg(straal)],de,[measure],[dim(straaltje)]).

n([pl(stralen),sg(straal)],de,[],
  [h('UV'),
   aard,
   bliksem,
   licht,
   röntgen,
   dim(straaltje)]).

n([pl(straaljagers),sg(straaljager)],de,[]).

n([pl(straten),sg(straat)],de,[],
  [s(dorp),
   hoofd,
   was,
   winkel,
   zij,
   dim(straatje)]).

%% hij woonde maar een paar straten/*wegen van stadion
n([pl(straten),sg(straat)],de,[temp_mod],
  []).

n([sg(straatlengte),sg(straatlengten),sg(straatlengtes)],de,[measure]).

n([pl(straatstenen),sg(straatsteen)],de,[]).

n([pl(straffen),sg(straf)],de,
  [subject_sbar,subject_vp]).

n([pl(straffen),sg(straf)],de,[],
  [cel,
   dood,
   s(dood),
   gevangenis,
   maximum,
   minimum,
   taak,
   tijd,
   s(vrijheid),
   werk]).

n([mass(strafbaarstelling)],de,[]).

n([pl(strafschoppen),sg(strafschop)],de,[]).

n([sg(strafvermindering),pl(strafverminderingen)],de,[]).

n([sg(strafvervolging),pl(strafvervolgingen)],de,[]).

n([pl(strafvorderingen),sg(strafvordering)],de,[]).

n([pl(stralingen),sg(straling)],de,[]).

n([mass(stramien)],het,[subject_sbar]).

n([pl(stranden),sg(strand)],het,[],
  [naakt,
   zand]).

n([pl(strategen),sg(strateeg)],de,[]).

n([pl(strategieën),sg(strategie)],de,[sbar,vp,subject_sbar,subject_vp]).

n([sg(stratificatie),pl(stratificaties)],de,[]).

n([sg(stream),pl(streams)],de,[],[f([live])]).

n([mass([streaming,audeo]),
   mass('streaming-audeo')],both,[]).

n([mass([streaming,video]),
   mass('streaming-video')],both,[]).

n([pl(streken),sg(streek)],de,[],
  [grens]).

n([pl(streken),sg(streek)],de,[subject_sbar],
  [rot]).

n([mass(streekvervoer)],het,[]).

n([pl(strepen),sg(streep)],de,
  [measure],
  [dim(streepje)]).

n([pl(strepen),sg(streep)],de,[],
  [krijt,
   licht,  % en niet licht_reep
   dim(streepje)]).

n([pl(strekkingen),sg(strekking)],de,[sbar,vp,subject_sbar,subject_vp]).

n([pl(strelingen),sg(streling)],de,[]).

n([pl(stremsels),sg(stremsel)],het,[]).

n([pl(strengen),sg(streng)],de,[measure],[dim(strengetje)]).

n([pl(strengheden),sg(strengheid)],de,[vp]).

n([mass(stress)],de,[]).

n([mass(streven)],het,[sbar,subject_sbar,subject_vp,vp]).

n([pl(strevingen),sg(streving)],de,[]).

n([pl(striemen),sg(striem)],de,[],[dim(striempje)]).

n([pl(strijden),sg(strijd)],de,[],
  [s(macht),macht,
   overname]).

n([pl(strijders),sg(strijder)],de,[],
  [h('Al-Kaida'),
   guerrilla,
   macht,s(macht),
   moslim,
   h(oud)]).

n([pl(strijdsters),sg(strijdster)],de,[],
  [h('Al-Kaida'),
   guerrilla,
   moslim,
   h(oud)]).

n([stem(strijdkracht),
   pl(strijdkrachten)],de,[]).

n([mass(strijdlust)],de,[]).

n([mass(strijdmacht)],de,[]).

n([pl(strijdtonelen),sg(strijdtoneel)],het,[]).

n([mass(strijk)],de,[]).  % dat wat nog gestreken moet worden

n([sg(strijker),pl(strijkers)],de,[]).

n([sg(strijkijzer),pl(strijkijzers)],both,[]).

n([pl(strijkkwartetten),sg(strijkkwartet)],het,[]).

n([pl(strijkstokken),sg(strijkstok)],de,[]).

n([pl(strikken),sg(strik)],de,[],[dim(strikje)]).

n([pl(strippen),pl(strips),sg(strip)],de,[],
  [magneet]).

n([sg(stripper),pl(strippers)],de,[]).

n([sg(stripverhaal),pl(stripverhalen)],het,[]).

n([mass(stro)],het,[],[dim(strootje)]).

n([sg(strobaal),pl(strobalen)],de,[]).

n([sg(strobreed)],both,[]).

n([pl(strofen),sg(strofe)],de,[]).

n([pl(strohalmen),sg(strohalm)],de,[]).

n([pl(strohoeden),sg(strohoed)],de,[]).

n([sg([stroke,unit]),
   pl([stroke,units])],de,[]).

n([pl(stromingen),sg(stroming)],de,[]).

n([pl(stronken),sg(stronk)],de,[]).

n([pl(stronten),sg(stront)],de,[]).

n([pl(stroken),sg(strook)],de,[measure]).

n([pl(stroken),sg(strook)],de,[],
  [betaal,
   carpool,
   pech,
   spits,
   vlucht]).

n([pl(stromen),sg(stroom)],de,
  [measure,
   np_app_measure,
   pred_pp(onder)],
  [dim(stroompje)]).

n([sg(stroom)],de,[],
  [gelijk,
   wissel]).			% electriciteit

n([pl(stromen),sg(stroom)],de,[],
  [geld,
   informatie,
   kapitaal,
   kas,
   lucht,
   modder,
   onder,
   i(toerist,toeristen),
   i(vluchteling,vluchtelingen),
   i(zon,zonne), % en niet zon_nest_room
   i(zon,zonnen)]).

n([pl(stroomversnellingen),sg(stroomversnelling)],de,[]).

n([pl(stropen),sg(stroop)],de,[]).

n([pl(stroppen),sg(strop)],de,[],[dim(stropje)]).

n([pl(stropdassen),sg(stropdas)],de,[]).

n([pl(stropers),sg(stroper)],de,[]).

n([pl(strotten),sg(strot)],de,[]).

n([stem(strottenhoofd),
   pl(strottehoofden),sg(strottehoofd),
   pl(strottenhoofden),sg(strottenhoofd)],het,[]).

n([pl(strozakken),sg(strozak)],de,[]).

n([pl(strubbelingen),sg(strubbeling)],de,[sbar,vp],[dim(strubbelingetje)]).

n([mass(structurering),pl(structureringen),sg(structurering)],de,[]).

n([pl(structuren),sg(structuur)],de,[],
  [s(bestuur),
   organisatie,
   s(persoonlijkheid)]).

n([pl('structuurschema\'s'),sg(structuurschema)],het,[]).

n([pl(struiken),sg(struik)],de,[measure],[dim(struikje)]).

n([pl(struikelblokken),sg(struikelblok)],both,[sbar,vp]).

n([pl(struikgewassen),sg(struikgewas)],het,[]).

n([pl(struikrovers),sg(struikrover)],de,[]).

n([mass(strukturering)],de,[]).

n([pl(strukturen),sg(struktuur)],de,[]).

n([pl('struktuurschema\'s'),sg(struktuurschema)],het,[]).

n([pl(studenten),sg(student)],de,[measure]).

n([sg(studente),pl(studentes)],de,[measure]).

n([pl(studies),pl(studiën),sg(studie)],de,[app_measure]).

n([pl(studies),pl(studiën),sg(studie)],de,[],
  [avond,
   bijbel,
   s(haalbaarheid),
   taal,
   i(taal,talen),
   zelf]).

n([mass(studiefinanciering)],de,[]).

n([mass(studiehuis)],het,[]).

n([pl(studierichtingen),sg(studierichting)],de,[measure]).

n([pl('studio\'s'),sg(studio)],de,[],[dim(studiootje)]).

n([mass(stuff)],de,[]).

n([mass(stuifmeel)],het,[]).

n([pl(stuipen),sg(stuip)],de,[],
  [koorts]).

n([pl(stuiptrekkingen),sg(stuiptrekking)],de,[]).

n([sg(stuit),pl(stuiten)],de,[]).

n([sg(stuiter),pl(stuiters)],de,[]).

n([pl(stuivelingen),sg(stuiveling)],de,[]).

n([pl(stuivers),sg(stuiver)],de,[],[dim(stuivertje)]).

n([pl(stukken),sg(stuk)],het,
  [measure,
   meas_mod,
   pred_pp_pl(in)],
  [dim(stukje)]).

n([stem(stuk),pl(stuks)],het,
  [measure,
   meas_mod],
  []).

n([pl(stukken),sg(stuk),
   ignore_stem(stuk),
   ignore_stem(stuk_DIM)],het,
  [],
  [bewijs,
   hamer,
   post,
   puzzel,
   meester,
   staart,
   theater,
   top,
   wrak,
   dim(stukje)]).

n([sg(stukadoor),pl(stukadoors)],de,[]).

n([sg(stulp),pl(stulpen)],de,[]).

n([pl(stumpers),sg(stumper)],de,[],[dim(stumpertje)]).

n([pl(stunts),sg(stunt),pl(stunten)],de,[sbar,vp],[dim(stuntje)]).

n([sg(stuntel),pl(stuntels)],de,[]).

n([mass(sturing)],de,[]).

n([pl(sturen),sg(stuur)],het,[],[dim(stuurtje)]).

n([sg(stut),pl(stutten)],de,[]).

n([mass(stuurboord)],het,[]).

n([pl(stuurhutten),sg(stuurhut)],de,[]).

n([pl(stuurlieden),pl(stuurlui),sg(stuurman)],de,[]).

n([pl(stuurwielen),sg(stuurwiel)],het,[]).

n([sg(stuw),pl(stuwen)],de,[]).

n([sg(stuwadoor),pl(stuwadoren)],de,[]).

n([pl(stuwdammen),sg(stuwdam)],de,[]).

n([pl(stuwmeren),sg(stuwmeer)],het,[]).

n([sg(sub),pl(subs)],de,[],[super]).

n([pl(subculturen),sg(subcultuur),
   pl(subkulturen),sg(subkultuur)],de,[]).

n([pl(subjecten),sg(subject)],het,[]).

n([mass(subjectiviteit)],de,[]).

n([pl(suborden),pl(subordes),sg(suborde)],de,[app_measure]).

n([pl(subsidies),sg(subsidie)],both,[],
  [export,
   huur,
   landbouw,
   s(overheid),
   s(rijk),
   rente]).

n([mass(subsidiariteit)],de,[]).

n([mass(subsidiëring)],de,[]).

n([pl(substanties),sg(substantie)],de,[],
  [i(kruid,kruiden)]).

n([sg(substituut),pl(substituten)],both,[]).

n([pl(substraten),sg(substraat)],het,[]).

n([pl(subtiliteiten),sg(subtiliteit)],de,[sbar,vp]).

n([pl(successen),sg(succes)],het,[sbar,vp,subject_sbar],[dim(succesje)]).

n([pl(successen),sg(succes)],het,[],
  [kas,
   dim(succesje)]).

n([mass(successie)],both,[]).

n([pl(sufferds),sg(sufferd)],de,[]).

n([sg(suffix),pl(suffixen)],both,[]).

n([pl(suggesties),sg(suggestie)],de,[sbar,vp]).

n([pl(suggesties),sg(suggestie)],de,[],[menu]).

n([pl(suikers),sg(suiker)],de,[app_measure]).

n([pl(suikers),sg(suiker)],de,[],
  [basterd,
   kandij, % niet kan_dij_suiker
   poeder,
   vannille]).

n([pl(suikerbieten),sg(suikerbiet)],de,[]).

n([mass(suikergoed)],het,[]).

n([mass(suikerriet)],het,[]).

n([pl(suites),sg(suite)],de,[],
  [s(bruid)]).

n([pl(sukkels),sg(sukkel)],de,[],[dim(sukkeltje)]).

n([pl(sukkelaars),sg(sukkelaar)],de,[],[dim(sukkelaartje)]).

n([pl(suksessen),sg(sukses)],het,[sbar,vp]).

n([pl(sultans),sg(sultan)],de,[]).

n([sg(summum)],het,[]).

n([mass(super)],de,[]).		% benzine

n([sg(super),pl(supers)],de,[],[buurt]).  % winkels

n([sg([super,audio,cd]),
   pl([super,audio,'cd\'s'])],de,[]).

n([sg('super-G'),pl('super-G\'s')],de,[]).   % type skiwedstrijd

n([mass([super,ongelood])],de,[]).	% benzine

n([mass([super,loodvrij])],de,[]).	% benzine

n([mass([super,plus,loodvrij])],de,[]).	% benzine

n([mass([super,plus,ongelood])],de,[]).	% benzine

n([sg(supercup),pl(supercups)],de,[app_measure]).

n([mass(superego)],het,[]).

n([sg(supergeleider),pl(supergeleiders)],de,[]).

n([sg([hoge,temperatuur,supergeleider]),pl([hoge,temperatuur,supergeleiders])],de,[]).

n([pl(superieuren),sg(superieur)],de,[]).

n([pl(superieures),sg(superieure)],de,[]).

n([mass(superioriteit)],de,[]).

n([sg(superlatief),pl(superlatieven)],both,[]).

n([sg(supermarkt),pl(supermarkten)],de,[],
  [f(service),
   f(discount)]).

n([mass(supervisie)],de,[]).

% geen super_vis_oren
n([sg(supervisor),pl(supervisors),pl(supervisoren)],de,[]).

n([pl(supplementen),sg(supplement)],het,[],[s(voeding)]).

n([sg(suppletie),pl(suppleties)],de,[]).

n([pl(suppoosten),sg(suppoost)],de,[]).

n([sg(support)],de,[]).

n([pl(supporters),sg(supporter)],de,[]).

n([mass(suprematie)],de,[]).

n([mass(surrealisme)],het,[]).

n([pl(surrogaten),sg(surrogaat)],het,[]).

n([sg(surseance),sg(surséance)],both,[]).

n([sg(surveillant),pl(surveillanten)],de,[]).

n([mass(sushi)],de,[]).

n([mass(suède)],both,[]).

n([sg(swing),pl(swings)],de,[]).

n([sg(switch),pl(switches)],de,[]).

n([mass(syfilis)],de,[]).

n([sg(syllabe),pl(syllabes)],de,[]).

n([pl(syllogismen),sg(syllogisme)],het,[]).

n([mass(symbiose)],de,[]).

n([mass(symboliek)],de,[]).

n([mass(symbolisme)],het,[]).

n([pl(symbolisten),sg(symbolist)],de,[]).

n([pl(symbolen),sg(symbool)],het,[measure]).

n([pl(symbolen),sg(symbool)],het,[],[status]).

n([pl(symfonieën),sg(symfonie)],de,[]).

n([mass(symmetrie)],de,[]).

n([pl(sympathieën),sg(sympathie),sg(symphatie),pl(symphatieën)],de,[]).

n([pl(sympathisanten),sg(sympathisant)],de,[]).

n([pl(sympathizanten),sg(sympathizant)],de,[]).

n([pl(sympatieën),sg(sympatie)],de,[]).

n([pl(sympatisanten),sg(sympatisant)],de,[]).

n([pl(sympatizanten),sg(sympatizant)],de,[]).

n([pl(symposia),pl(symposiums),sg(symposium)],het,[]).

n([pl(symptomen),sg(symptoom)],het,[]).

n([pl(synagogen),sg(synagoge)],de,[]).

n([sg(synaps),pl(synapsen)],de,[]).

n([sg(syndicaat),pl(syndicaten)],het,[]).

n([sg(syndicalisme)],het,[]).

n([pl(syndromen),sg(syndroom)],het,[]).

n([mass(synergie)],de,[]).

n([pl(synoden),pl(synodes),sg(synode)],de,[]).

n([pl(synoniemen),sg(synoniem)],het,[]).

n([mass(syntaxis)],de,[]).

n([pl(synthesen),pl(syntheses),sg(synthese)],de,[],[foto]).

n([pl(systemen),sg(systeem)],het,[],
  [aandrijf,
   afweer,
   belasting,
   s(besturing),
   computer,
   controle,
   demotica,
   eco,
   s(handel),
   s(honorering),
   immuun,
   informatie,f([informatie]),
   kaart,
   koel,
   meet,
   navigatie,
   onderwijs,
   ophang,
   productie,i(productie,produktie),
   s(recht),
   registratie,
   transfer,
   wapen,
   dim(systeempje)]).

n([mass(systematiek)],de,[]).

n([mass(systematisering)],de,[]).

n([pl('t\'s'),sg(t)],de,[],[dim('t\'tje')]).

n([pl('t-groepen'),sg('t-groep')],de,[]).

n([pl(taken),sg(taak)],de,[subject_vp,vp],
  [hoofd,
   kern]).

n([pl(taken),sg(taak)],de,[],
  [studie,
   zorg]).

n([pl(taakstellingen),sg(taakstelling)],de,[]).

n([pl(talen),sg(taal)],de,[app_measure],
  [beeld,
   brabbel,
   bron,
   cultuur,
   doel,
   i(gebaar,gebaren),
   hulp,
   'jip-en-janneke',
   h('Jip-en-Janneke'),
   i(jong,jongeren),
   kerk,
   kinder,
   klank,
   krom,
   kunst,
   s(land),
   s(lichaam),
   machine,
   marketing,
   h(markup),
   i(mens,mensen),
   s(minderheid),
   h(msn),
   h('MSN'),
   moeder,
   muziek,
   oer,
   s(omgang),
   opmaak,
   programmeer,
   promo,h(promo),
   schrijf,
   schutting,
   spreek,
   standaard,
   straat,
   streek,
   toon,
   turbo,
   tussen,
   vak,
   voer,
   s(volk),
   dim(taaltje)]).

n([mass(taaleigen)],het,[]).

n([mass(taalkunde)],de,[]).

n([sg(taalkundige),pl(taalkundigen)],de,[]).

n([pl(taarten),sg(taart)],de,[],
  [appel,
   dim(taartje)]).

n([sg(tab),sg('Tab'),pl(tabs)],de,[]).  % otherwise Tab is chess-move only

n([pl(tabakken),sg(tabak)],de,[]).

n([pl(tabellen),sg(tabel)],de,[],[dim(tabelletje)]).

n([pl(tableaus),sg(tableau)],het,[],[dim(tableautje)]).

n([pl(tabletten),sg(tablet)],both,[measure]).

n([pl(tabletten),sg(tablet)],both,[],
  [slaap,
   dim(tabletje)]).

n([stem(tablet),pl(tablets)],both,[]).

n([sg(tabloid),pl(tabloids)],both,[]).

n([pl(taboes),sg(taboe)],het,[]).

n([pl(tachtigers),sg(tachtiger)],de,[]).

n([sg(tackle),pl(tackles)],de,[]).

n([mass(tact)],de,[]).

n([mass(tactiek),pl(tactieken),sg(tactiek),sg(taktiek)],de,
  [subject_sbar,
   subject_vp]).

n([mass([tae,bo]),
   mass(['Tae','Bo'])],both,[]).

n([pl(tafelen),pl(tafels),sg(tafel)],de,[measure],
  [s(bestuur), % niet bestuur_sta_fel
   i(bij_zet,bijzet),
   eet,
   s(inhoud),  % niet inhoud_sta_fel
   keuken,
   koffie,
   massage,
   s(onderhandeling),
   ontbijt,
   salon,
   schrijf,
   speel,
   teken,
   toilet,
   vergader,
   was,
   werk,
   dim(tafeltje)]).

n([pl(tafelkleden),sg(tafelkleed)],het,[]).

n([pl(tafellakens),sg(tafellaken)],het,[]).

n([mass(tafeltennis)],het,[]).

n([sg(tafeltennisser),pl(tafeltennissers)],de,[]).

n([pl(taferelen),sg(tafereel)],het,[],[dim(tafereeltje)]).

n([sg(tag),pl(tags)],de,[]).

n([sg([tag,team]),pl([tag,teams])],both,[]).

n([sg([tag,team,match]),pl([tag,team,matches])],de,[]).

n([mass(tahoe)],de,[]).

n([pl(tailles),sg(taille)],de,[]).

%% takje peterselie ?
n([pl(takken),sg(tak)],de,[measure],
  [dim(takje)]).

n([pl(takken),sg(tak)],de,[],
  [noord,
   oost,
   west,
   zuid]).

n([pl(takes),sg(take)],de,[]).

n([sg([take,over,panel]),pl([take,over,panels])],het,[]).

n([sg(takel),pl(takels)],de,[]).

n([mass(takt)],de,[]).

n([mass(tal)],de,[]).		% VL de tal van bedrijven

n([sg([tale,'Kanaäns'])],de,[]).

n([mass(talg)],de,[]).

n([sg(talkshow),pl(talkshows)],de,[]).

n([sg([talkshow,host]),pl([talkshow,hosts])],de,[]).

%% is now a determiner
%% n([pl(tallen),sg(tal)],het,[measure]).

n([pl(talenten),sg(talent)],het,
  [subject_sbar,
   subject_vp],
  []).

n([pl(talenten),sg(talent)],het,[],
  [natuur,
   voetbal]).

n([sg('Taliban')],de,[]).  % de Taliban John Walker Lindh arriveerde in de USA

n([pl(talismans),sg(talisman)],de,[]).

n([sg([tall,ship]),pl([tall,ships])],de,[]).

n([pl(tanden),sg(tand)],de,[],[dim(tandje)]).

n([mass(tandbederf)],het,[]).

n([sg(tandem),pl(tandems)],both,[]).

n([pl(tandenborstels),sg(tandenborstel)],de,[]).

n([mass(tandheelkunde)],de,[]).

n([pl('tandpasta\'s'),sg(tandpasta)],de,[]).

n([sg(tandwiel),pl(tandwielen)],het,[]).

n([pl(tangen),sg(tang)],de,[],[dim(tangetje)]).

n([pl('tango\'s'),sg(tango)],de,[]).

n([pl(tanks),sg(tank)],de,[],
  [brandstof]).

n([pl(tankers),sg(tanker)],de,[],
  [olie,h(olie)]).

n([mass(tannine)],de,[]).

n([pl(tantes),sg(tante)],de,[],
  [suiker,
   dim(tantetje)]).

n([pl(tappen),sg(tap),pl(taps)],de,[]).

n([pl(tapes),sg(tape)],de,[]).

n([pl(tapijten),sg(tapijt)],het,[]).

n([pl(tapkasten),sg(tapkast)],de,[]).

n([sg(tapper),pl(tappers)],de,[]).

n([pl(tarieven),sg(tarief)],het,[app_measure],
  [belasting,
   bodem,
   douane,
   i(geld_markt,geldmarkt),
   import,
   maximum,
   piek,
   rente,
   spits,
   top,
   uur]).

n([pl(tarots),sg(tarot)],de,[]).

n([mass([tarte,tatin])],de,[]).

n([mass(tarwe)],both,[]).

n([pl(tassen),sg(tas)],de,
  [measure],
  [dim(tasje)]).

n([pl(tassen),sg(tas)],de,
  [],
  [i(boek,boeken),
   sport,
   weekend,
   dim(tasje)]).

n([sg([task,force]),pl([task,forces])],de,[app_measure]).

n([mass(tast)],de,[]).

n([mass(tastzin)],de,[]).

n([sg(tattoo),pl(tattoos),pl('tattoo\'s')],de,[]).

n([sg(tatouage),pl(tatouages),
   sg(tatoeage),pl(tatoeages)],de,[]).

n([sg(taverne),pl(tavernes)],de,[]).

n([pl(taxaties),pl(taxatiën),sg(taxatie)],de,[sbar]).

n([mass(taxfree)],de,[]).

n([pl('taxi\'s'),sg(taxi)],de,[],[trein]).

n([mass(taxol)],de,[]).  % kankerremmer

n([sg(taxonomie),pl(taxonomieën)],de,[]).

n([mass(tbc)],de,[]).

n([mass(tbs),mass('TBS')],de,[]).

n([sg('tbs\'er'),pl('tbs\'ers'),
   sg('TBS\'er'),pl('TBS\'ers')],de,[],
  [h(ex)]).

n([pl(teams),sg(team)],het,[measure,app_measure],[]). % een team verkopers, het team Beleidsondersteuning

n([pl(teams),sg(team)],het,[],
  [arrestatie,
   basis,
   basketbal,
   i(belofte,beloften),
   campagne,
   club,
   cricket,
   crisis,
   'Davis-Cup',
   i(dame,dames),
   wh(['Formule','1']),
   handbal,
   i(heer,heren),
   hockey,
   honkbal,
   ijshockey,
   jeugd,
   i(land,landen),
   i(man,mannen),
   management,
   s(onderzoek),
   prof,
   project,
   recherche,
   voetbal,
   volleybal,
   i(vrouw,vrouwen)
  ]).

n([mass(teambuilding)],de,[]).

n([sg([team,de,mission]),pl([teams,de,mission])],both,[]).

n([sg([team,sprint]),pl([team,sprints])],de,[]).

n([pl(technici),sg(technicus)],de,[]).

n([pl(technieken),sg(techniek)],de,[],[schilder]).

n([mass(techno)],de,[]). % muziek

n([pl(technocraten),sg(technocraat)],de,[]).

n([pl(technokraten),sg(technokraat)],de,[]).

n([mass(technolease)],de,[]).

n([sg(technologie),pl(technologieën)],de,[],
  [bio,
   communicatie,
   gen,
   informatie,
   nano,
   spraak,
   taal]).

n([pl(teckels),sg(teckel)],de,[]).

n([mass(teddy)],de,[]).

n([pl(tederheden),sg(tederheid)],de,[]).

n([pl(teeën),sg(tee)],de,[]).

n([pl(teven),sg(teef)],de,[],[dim(teefje)]).

n([pl(teken),sg(teek)],de,[],
  [i(hond,honden)]).

n([pl(teelballen),sg(teelbal)],de,[]).

n([pl(teelten),sg(teelt)],de,[],
  [i(bij,bijen),
   i(plant,planten)]).		% en niet plan_tent_eelt

n([pl(tenen),sg(teen)],de,[measure],[dim(teentje)]).

n([pl(teepotten),sg(teepot)],de,[]).

n([mass(teer)],de,[]).

n([pl(teevees),sg(teevee)],de,[]).

n([pl(tegels),sg(tegel)],de,[],
  [stoep,
   vloer,
   dim(tegeltje)]).

n([pl(tegelvloeren),sg(tegelvloer)],de,[]).

n([pl(tegemoetkomingen),sg(tegemoetkoming)],de,
  [sbar,
   vp,
   app_measure]).

n([pl(tegendelen),sg(tegendeel)],het,[]).

n([pl(tegengiffen),sg(tegengif)],het,[]).

n([pl(tegenhangers),sg(tegenhanger)],de,[]).

% niet tegen_hang_ster
n([pl(tegenhangsters),sg(tegenhangster)],de,[]).

n([pl(tegenpolen),sg(tegenpool)],de,[]).

n([pl(tegenslagen),sg(tegenslag)],de,[sbar,vp]).

n([mass(tegenspel)],het,[]).

n([pl(tegenspoeden),sg(tegenspoed)],de,[sbar,vp]).

n([sg(tegenspraak),pl(tegenspraken)],de,[pred_pp(in)]).

n([mass(tegenstand)],de,[]).

n([pl(tegenstanders),sg(tegenstander)],de,[]).

n([pl(tegenstandsters),sg(tegenstandster)],de,[]).

n([pl(tegenstellingen),sg(tegenstelling)],de,[app_measure,sbar,subject_sbar]).

n([pl(tegenstellingen),sg(tegenstelling)],de,[],
  [i(belang,belangen),
   i(klasse,klassen)]).

n([sg(tegenstrever),pl(tegenstrevers)],de,[]).

n([sg(tegenstreefster),pl(tegenstreefsters)],de,[]).

n([pl(tegenstrijdigheden),sg(tegenstrijdigheid)],de,[sbar,vp]).

n([pl(tegenvallers),sg(tegenvaller)],de,[sbar,vp]).

n([mass(tegenwerk)],het,[]).

n([pl(tegenwerkingen),sg(tegenwerking)],de,[vp]).

n([pl(tegenwerpingen),sg(tegenwerping)],de,[sbar,vp]).

n([pl(tegenwichten),sg(tegenwicht)],het,[]).

n([mass(tegenwoordigheid)],de,[]).

n([mass(tegenzin)],de,[]).

n([pl(tegoeden),sg(tegoed)],het,[],
  [bel,
   spaar]).

n([pl(tehuizen),sg(tehuis)],het,[],
  [i(kind,kinder),
   s(verzorging)
  ]).

n([pl(teilen),sg(teil)],de,[measure],[dim(teiltje)]).

n([mass(teint)],de,[]).

n([pl(tekens),pl(tekenen),sg(teken)],het,
  [sbar,subject_sbar,vp],
  [dim(tekentje)]).

n([pl(tekens),sg(teken)],het,
  [],
  [dollar,
   strijd
  ]).

n([pl(tekenaars),sg(tekenaar)],de,[],
  [strip]).

n([pl(tekeningen),sg(tekening)],de,[],
  [rots,
   wh([naar,het,leven]),
   dim(tekeningetje)]).

n([pl(tekkels),sg(tekkel),
   sg(tackel),pl(tackels)],de,[]).

n([pl(tekorten),sg(tekort)],het,[],
  [i(cel,cellen),
   i(leraar,leraren),
   s(overheid),
   s(personeel),
   slaap,
   vocht
  ]).

n([pl(tekortkomingen),sg(tekortkoming)],de,[]).

n([pl(teksten),sg(tekst)],de,
  [app_measure,
   np_app_measure,
   start_app_measure,
   sbar],
  [flap,
   lied,
   ontwerp]).

n([pl(tekstschrijvers),sg(tekstschrijver)],de,[]).

n([pl(tekstverwerkers),sg(tekstverwerker)],de,[]).

n([meas(tel),pl(tellen)],de,[temp_mod,measure,sbar]).

n([mass(telecom)],de,[]).

n([mass(telecommunicatie)],de,[]).

n([mass(telefonie)],de,[]).

n([sg(teler),pl(telers)],de,[],[h(tomaat,tomaten)]).

n([pl(telefonisten),sg(telefonist)],de,[]).

n([pl(telefonistes),sg(telefoniste)],de,[]).

n([pl(telefonen),pl(telefoons),sg(telefoon)],de,[],
  [auto,
   hoofd,
   i(kind,kinder),
   kop,
   oor,
   zak]).

n([stem(telefoon_DIM),pl(telefoontjes),sg(telefoontje)],het,[sbar]).

n([pl(telefooncellen),sg(telefooncel)],de,[]).

n([mass(telefoonverkeer)],het,[]).

n([pl(telegrafen),sg(telegraaf)],de,[]).

n([pl(telegrammen),pl(telegrams),sg(telegram)],het,[],[dim(telegrammetje)]).

n([mass(telepathie)],de,[]).

n([pl(telescopen),sg(telescoop)],de,[]).

n([pl(teleskopen),sg(teleskoop)],de,[]).

n([pl(teleteksten),sg(teletekst)],de,[]).

n([pl(teleurstellingen),sg(teleurstelling)],de,[subject_sbar,
						subject_vp,
						sbar]).

n([pl(televisies),sg(televisie)],de,[],
  [abonnee,h(abonnee),
   i(kleur,kleuren),
   s(staat)]).

n([pl(telexen),sg(telex)],de,[]).

n([pl(telgen),sg(telg)],de,[]).

n([pl(tellers),sg(teller)],de,[],
  [dag,
   i(toer,toeren)]).

n([pl(tellingen),sg(telling)],de,[],
  [h('INSEE'),
   bij,
   dubbel,
   her]).

n([sg(telomeer),pl(telomeren)],het,[]).

n([sg(teloorgang)],de,[]).

n([pl(tempelen),pl(tempels),sg(tempel)],de,[],[dim(tempeltje)]).

n([pl(tempelieren),pl(tempeliers),sg(tempelier)],de,[]).

n([mass(tempera)],de,[]).

n([pl(temperamenten),sg(temperament)],het,[]).

n([pl(temperaturen),sg(temperatuur)],de,[pred_pp(op)]).

n([pl(temperaturen),sg(temperatuur)],de,[],
  [buiten,
   s(gevoel),
   kamer,
   s(lichaam),
   maximum,
   middag,
   minimum]).

n([mass(tempex)],both,[]).

n([pl(tempi),pl('tempo\'s'),sg(tempo)],het,[],
  [groei,
   werk]).

n([sg([ten,geleide]),pl([ten,geleides])],het,[]).

n([pl(tendensen),sg(tendens)],de,[sbar,vp]).

n([pl(tendenties),sg(tendentie)],de,[sbar,vp]).

n([mass(teneur)],de,[subject_sbar,subject_vp,sbar]).

n([sg(tenlastelegging),pl(tenlasteleggingen)],de,[]).

n([mass(tennis)],both,[],[i(vrouw,vrouwen)]).

n([sg(tennisarm),pl(tennisarmen)],de,[]).

n([sg(tenniselleboog),pl(tennisellebogen)],de,[]).

n([pl(tennisbanen),sg(tennisbaan)],de,[]).

n([pl(tenniscourts),sg(tenniscourt)],het,[]).

n([pl(tennissers),sg(tennisser)],de,[]).

n([pl(tennissters),sg(tennisster)],de,[]).

n([pl(tenoren),pl(tenors),sg(tenor)],de,[]).

n([pl(tenten),sg(tent)],de,[np_app_measure],
  [feest,
   strand,
   dim(tentje)]).

n([pl(tenten),sg(tent)],de,[],
  [feest,
   i(twee_persoon,tweepersoons),
   i(drie_persoon,driepersoons),
   i(vier_persoon,vierpersoons),
   voor,
   dim(tentje)]).

n([pl(tentakels),sg(tentakel)],de,[]).

n([pl(tentamens),pl(tentamina),sg(tentamen)],het,[app_measure]).

n([pl(tentoonstellingen),sg(tentoonstelling)],de,
  [np_app_measure],
  [foto,
   s(opening),
   s(overzicht),
   wereld]).

n([sg(tenue),pl(tenues)],het,[]).

n([pl(tenuitvoerleggingen),sg(tenuitvoerlegging)],de,[]).

n([pl(tepels),sg(tepel)],de,[],[dim(tepeltje)]).

n([sg(terdoodveroordeelde),pl(terdoodveroordeelden)],de,[]).

n([sg(terdoodveroordeling),pl(terdoodveroordelingen)],de,[]).

n([pl(terechtstellingen),sg(terechtstelling)],de,[]).

n([pl(terechtwijzingen),sg(terechtwijzing)],de,[sbar,vp]).

n([mass(tering)],de,[]).

n([sg([ter,inzage,legging]),pl([ter,inzage,leggingen])],de,[]).

n([pl(termen),sg(term)],de,[sbar,
			    vp,
			    app_measure,
			    start_app_measure]).

n([pl(termijnen),sg(termijn)],de,
  [sbar,
   temp_mod],
  [s(aanmelding),
   s(ambt),
   bedenk,
   s(levering),
   opzeg,
   s(uitvoering),
   s(verjaring),
   s(zitting)]).  % en niet zitting_ster_mijn

n([pl(terminals),sg(terminal)],de,[],
  [computer,
   container]).

n([pl(terminologieën),sg(terminologie)],de,[]).

n([pl(terpen),sg(terp)],de,[]).

n([mass(terpentijn)],de,[]).

n([mass(terracotta)],de,[]).

n([pl(terrassen),sg(terras)],het,[],[dim(terrasje)]).

n([pl(terreinen),sg(terrein)],het,[],
  [s(bedrijf),i(bedrijf,bedrijven),
   s(beleid),
   bouw,
   exercitie,
   festival,
   industrie,
   kampeer,
   oefen,
   parkeer,
   werk,
   s(voorlichting),
   dim(terreintje)]).

n([mass(terreur)],de,[]).

n([pl(territoria),pl(territoriums),sg(territorium)],het,[]).

n([mass(terrorisme)],het,[]).

n([pl(terroristen),sg(terrorist)],de,[],
  [zelfmoord]).

n([pl(terugbetalingen),sg(terugbetaling)],de,[]).

n([mass(terugblik),pl(terugblikken)],de,[]).

n([mass(terugdringing)],de,[]).

n([mass(teruggang)],de,[]).

n([sg(teruggave),pl(teruggaven)],de,[]).

n([mass(terughoudendheid)],de,[]).

n([mass(terugkeer)],de,[]).

n([mass(terugkomst)],de,[]).

n([mass(terugkoop)],de,[]).

n([sg(terugkoppeling)],de,[]).

n([sg(terugloop)],de,[]).

n([sg(terugname)],de,[]).

n([sg(terugruil)],de,[]).

n([sg(terugslag),pl(terugslagen)],de,[]).

n([sg(terugtocht),pl(terugtochten)],de,[]).

n([mass(terugtrekking)],de,[]).

n([pl(terugvorderingen),sg(terugvordering)],de,[]).

n([pl(testen),pl(tests),sg(test)],de,[],
  ['Cooper',h('Cooper'),
   h('DNA'),
   h(dna),
   doping,
   s(zwangerschap)
  ]).

n([pl(testamenten),sg(testament)],het,[]).

n([sg(testamenticus),pl(testamentici)],de,[],
  [h(oud),
   h(nieuw)]).

n([sg(tester),pl(testers)],de,[]).

n([pl(testikels),sg(testikel)],de,[]).

n([mass(testosteron)],both,[]).

%% afmeting van containers "Twenty feet Equivalent Unit"
n([meas(teu)],both,[meas_mod,measure]).

n([pl(teugen),sg(teug)],de,[measure],[dim(teugje)]).

n([pl(teugels),sg(teugel)],de,[],[dim(teugeltje)]).

n([mass(teveel)],het,[],[meas_mod]).  % het teveel aan stikstof...

n([mass(tevredenheid)],de,[sbar]).

n([pl(tewerkstellingen),sg(tewerkstelling)],de,[]).

n([mass(textiel)],both,[]).  % celex: de

n([pl(texturen),sg(textuur)],de,[]).

n([sg(tgv)],de,[]).

n([pl(theaters),sg(theater)],het,[np_app_measure],
  [amfi,
   dans, %% niet dan_theater
   muziek,
   straat]).

n([sg(theatermaker),pl(theatermakers)],de,[]).

n([pl(theeën),sg(thee)],de,[],
  [ijs]).

n([pl(theedoeken),sg(theedoek)],de,[]).

n([pl('thema\'s'),sg(thema)],het,
  [sbar,
   vp,
   subject_sbar,
   subject_vp,
   app_measure,
   start_app_measure],
  [hoofd,
   kern,
   neven]).

n([mass(thematiek)],de,[]).

n([mass(theologie)],de,[]).

n([pl(theologen),sg(theoloog)],de,[]).

n([pl(theoretici),sg(theoreticus)],de,[]).

n([pl(theorieën),sg(theorie)],de,[sbar],
  [complot]).

n([pl(theorieën),sg(theorie)],de,[],
  [evolutie,
   praktijk,  % vol-ledig!
   s(relativiteit),
   systeem]).

n([pl(theorievormingen),sg(theorievorming)],de,[]).

n([pl(therapeuten),sg(therapeut)],de,[],
  [fysio,
   f([manueel]),
   podo,
   psycho]).

n([pl(therapieën),sg(therapie)],de,[sbar,vp]).

n([pl(therapieën),sg(therapie)],de,[],
  [chemo,
   fysio,
   s(gedrag),
   gen,
   s(gezin),
   s(groep),
   muziek,
   psycho,
   substitutie
  ]).

n([pl(thermometers),sg(thermometer)],de,[]).

n([sg(thermostaat),pl(thermostaten)],de,[]).

n([pl(thesen),pl(theses),sg(these)],de,[sbar]).

n([sg(thesis)],de,[]).

n([sg(thread),pl(threads)],de,[]).

n([pl(thrillers),sg(thriller)],de,[]).

n([mass(thuis)],both,[]).

n([mass(thuiskomst)],de,[]).

n([pl(thuislanden),sg(thuisland)],het,[]).

n([sg(ti),pl('ti\'s')],de,[]).

n([pl(tics),sg(tic)],de,[vp]).

n([pl(tickets),sg(ticket)],both,[],[vlieg]).

n([sg(tiebreak),pl(tiebreaks),
   sg([tie,break]),pl([tie,breaks]),
   sg('tie-break'),pl('tie-breaks')],de,[]).

n([pl(tienen),sg(tien)],de,[]).

n([stem(tiende), pl(tienden)],both,[meas_mod]).

n([stem(tien_duizend),pl(tienduizenden)],de,[measure]).

n([pl(tieners),sg(tiener)],de,[]).

n([bare_meas(tienmaal),pl(tienmalen)],both,[temp_mod,measure,sbar]).

n([stem(tien_DIM),
   pl(tientjes),sg(tientje)],het,[measure]). % een tientje korting

n([sg(tier),pl(tiers)],de,[]).  % vooral voor tier 1-instrumenten

n([pl(tieten),sg(tiet)],de,[measure], % measure?
  []).

n([pl(tieten),sg(tiet)],de,[],
  [dim(tietje)]).

n([pl(tifosi)],de,[]).

n([pl(tijen),sg(tij)],het,[]).

n([sg(tijd),pl(tijden)],de,
  [vp,
   app_measure,
   pred_pp(van),
   pred_pp(op),
   pred_pp(uit),
   pred_pp(van,subject_sbar),
   pred_pp(van,subject_vp),
   subject_vp,                  % de hoogste tijd
   subject_sbar,                % de hoogste tijd
   temp_mod,
   sbar]).

n([sg(tijd),pl(tijden)],de,
  [temp_mod,
   sbar],
  [s(arbeid),
   bedenk,
   bed,
   begin,
   s(belichting),
   s(bezetting),
   blessure,
   bloei,
   broed,
   brons, % en niet bron_tijd
   dienst,
   eind,
   s(eten),
   glorie,
   s(halvering),
   incheck,
   ijs,
   i(kind,kinder),
   latentie,
   lever,
   lunch,
   oer,
   omloop,
   s(oorlog),
   s(opening),
   i(ramp,rampen),
   record,
   reis,
   school,
   s(sluiting),
   speel,
   sper,
   spreek,
   straf,
   i(student,studenten),
   studie,
   top,
   tussen,
   s(verkiezing),
   vertrek,
   vlieg,
   s(voorbereiding),
   s(vrede),
   wacht,
   werk,
   winkel,
   winter,
   zend,
   zomer]).

n([mass(tijdgeest)],de,[]).

n([sg(tijding),pl(tijdingen)],de,[sbar]).

n([stem(tijd_DIM),sg(tijdje),pl(tijdjes)],het,[measure,temp_mod,sbar]).

n([sg(tijdlang)],both,[temp_mod,sbar]).

n([pl(tijdperken),sg(tijdperk)],het,[]).

n([sg(tijdrit),pl(tijdritten)],de,[],[i(ploeg,ploegen)]).

n([mass(tijdsbestek)],het,[]).

n([pl('tijdschema\'s'),sg(tijdschema)],het,[]).

n([stem(tijd_spanne),sg(tijdspanne)],de,[]).

n([pl(tijdschriften),sg(tijdschrift)],het,
  [np_app_measure],
  [dim(tijdschriftje),
   vak]).

n([mass(tijdsduur)],de,[]).

n([pl(tijdstippen),sg(tijdstip)],het,[sbar,vp]).

n([pl(tijdvakken),sg(tijdvak)],het,[],
  [onderzoek]).

n([mass(tijdverdrijf)],het,[]).

n([pl(tijgers),sg(tijger)],de,[],[dim(tijgertje)]).

n([mass(tijm)],de,[]).

n([pl(tikken),pl(tiks),sg(tik)],de,[vp]).

n([stem(tik_DIM),
   pl(tikjes),sg(tikje),sg(tikkie)],het,
  [measure,
   mod]).

n([pl(tikken),sg(tik)],de,[],
  [telefoon]).

n([sg(tikkeltje)],het,[measure,
                       mod]).

n([sg(til),pl(tillen)],de,[],
  [i(duif,duiven)]).

n([sg(tilde),pl(tildes)],de,[]).

n([mass(tilt)],both,[pred_pp(op)]).

n([sg('time-out'),pl('time-outs'),
   sg([time,out]),pl([time,outs])],de,[]).

n([mass(timing)],de,[]).

n([pl(timmerlieden),pl(timmerlui),sg(timmerman)],de,[]).

n([mass(tin)],het,[]).

%% tinten rood en groen
n([pl(tinten),sg(tint)],de,[measure],[dim(tintje)]).

n([pl(tintelingen),sg(tinteling)],de,[]).

n([mass(tinto)],de,[]).

n([pl(tips),sg(tip)],de,
  [vp,
   sbar]).

n([pl(tippen)],de,[]).  % Vlaams: de tippen van mijn tenen

n([pl(tipgevers),sg(tipgever)],de,[]).

n([sg(tippel),pl(tippels)],de,[]).

n([pl(tirades),sg(tirade)],de,[]).

n([pl(tirannen),sg(tiran)],de,[]).

n([pl(tirannieën),sg(tirannie)],de,[sbar,vp]).

n([sg(tissue),pl(tissues)],de,[]).

n([pl(titanen),pl(titans),sg(titan)],de,[]).

n([pl(titels),sg(titel)],de,
  [start_app_measure,
   app_measure,
   np_app_measure],
  [ere,
   indoor,
   periode,
   wereld,
   ['Grand','Slam'],
   wh(['Grand','Slam'])
  ]).

n([pl(titelhouders),sg(titelhouder)],de,[app_measure]).

n([sg(titelrol),pl(titelrollen)],de,[]).

n([sg(titelstrijd),pl(titelstrijden)],de,[measure]).

n([pl(toasten),sg(toast)],de,[]).

n([pl(tobben),pl(tobbes),sg(tobbe)],de,[measure]).

n([pl(tochten),sg(tocht)],de,[pred_pp(op)]).  % op de tocht komen/staan

n([pl(tochten),sg(tocht)],de,[],
  [boot,
   door,
   fiets,
   s(pelgrim),
   roof,
   speur,
   triomf,
   s(verkenning),
   wandel,
   zege,
   zoek,
   zwerf,
   dim(tochtje)]).

n([mass(toebehoren),pl(toebehoren)],het,[]).

n([mass(toediening),pl(toedieningen),sg(toediening)],de,[]).

%% door eigen toedoen; "eigen" is nonadv, cannot combine with v_noun
%% n([mass(toedoen)],het,[]).

n([mass(toedracht)],de,[]).

n([pl(toefen),sg(toef)],de,[measure],[dim(toefje)]).

n([pl(toegangen),sg(toegang)],de,[],
  [markt]).

n([mass(toegankelijkheid)],de,[]).

n([mass(toegeeflijkheid)],de,[]).

n([sg(toegeving),pl(toegevingen)],de,[]).

n([pl(toegiften),sg(toegift)],de,[sbar,vp]).

n([pl(toehoorders),sg(toehoorder)],de,[]).

n([sg(toekan),pl(toekans)],de,[]).

n([pl(toekenningen),sg(toekenning)],de,[sbar,vp]).

n([mass(toekomst)],de,[]).

n([pl(toekomstperspectieven),sg(toekomstperspectief),
   pl(toekomstperspektieven),sg(toekomstperspektief)],het,[sbar,vp]).

n([pl(toelagen),pl(toelages),sg(toelage)],de,[]).

n([pl(toelatingen),sg(toelating)],de,[]).

n([mass(toeleg)],de,[]).

n([sg(toeleverancier),pl(toeleveranciers)],de,[]).  % ugly word!

n([pl(toelichtingen),sg(toelichting)],de,[sbar]).

n([mass(toeloop)],de,[]).

n([pl(toenaderingen),sg(toenadering)],de,[sbar,vp]).

n([mass(toename)],de,[]).

n([mass(toeneming)],de,[]).

n([mass(toepasbaarheid)],de,[]).

n([pl(toepassingen),sg(toepassing)],de,[vp,
                                        pred_pp(van)]).

n([pl(toeren),sg(toer)],de,[vp,
                            subject_vp],
  [rij,
   dim(toertje)]).

n([mass(toerisme)],het,[],
  [drug,s(drug)]).

n([pl(toeristen),sg(toerist)],de,[],
  [berm,
   drug,s(drug),
   wieler]).

n([pl(toernees),sg(toernee)],de,[]).

n([pl(toernooien),sg(toernooi),
   pl(tornooien),sg(tornooi)],both,
  [app_measure,
   temp_mod,
   sbar],
  [['10.000',dollar],
   h('ATP'),
   beker,
   'Davis-Cup',
   eind,
   'Europa-Cup',
   h('Grand-Slam'),'Grand-Slam',grandslam,'grand-slam',
   wh(['Grand','Slam']),
   hoofd,
   korfbal,
   kwalificatie,
   i(kwalificatie,kwalifikatie),
   schaak,
   tennis,
   h('Uefa-Cup'),'Uefa-Cup',h('UEFA-Cup'),'UEFA-Cup',
   i(vier_land,vierlanden),
   voetbal]).

n([pl(toeschouwers),sg(toeschouwer)],de,[]).

n([pl(toeslagen),sg(toeslag)],de,[],
  [koude,
   i(kind_opvang,kinderopvang),
   s(onregelmatigheid),
   s(overheveling),
   zorg]).

n([pl(toespelingen),sg(toespeling)],de,[sbar,vp]).

n([mass(toespoor)],both,[]). % iets met banden van auto's

n([pl(toespraken),sg(toespraak)],de,[]).

n([pl(toestanden),sg(toestand)],de,[pred_pp(in)]).

n([pl(toestanden),sg(toestand)],de,[],
  [s(gezondheid)]).

n([pl(toestellen),sg(toestel)],het,[],
  [foto,
   ramp,
   radio,
   speel,
   telefoon,
   televisie]).

n([pl(toestemmingen),sg(toestemming)],de,[sbar,vp]).

n([mass(toestroom)],de,[]).

n([sg(toet),pl(toeten)],de,[]).

n([pl(toeters),sg(toeter)],de,[]).

n([pl(toetjes),sg(toetje)],het,[]).

n([pl(toetredingen),sg(toetreding)],de,[]).

n([pl(toetsen),sg(toets)],de,[app_measure],
  [h('Cito'),
   f([delete]),
   eind,
   kleuter,
   i(pijl,pijltjes),
   luister,
   snel
  ]).

n([pl(toetsenborden),sg(toetsenbord)],het,[]).

n([sg(toetsenist),pl(toetsenisten)],de,[]).

n([pl(toetsingen),sg(toetsing)],de,[]).

n([pl(toetsstenen),sg(toetssteen)],de,[]).

n([pl(toevallen),sg(toeval)],het,[sbar,subject_sbar,vp]).

n([pl(toevalligheden),sg(toevalligheid)],de,[sbar,vp]).

n([sg(toeverlaat)],both,[]).

n([sg(toevloed)],de,[]).

n([mass(toevlucht)],de,[]).

n([pl(toevluchtsoorden),sg(toevluchtsoord)],het,[]).

n([pl(toevoegingen),sg(toevoeging)],de,[sbar,vp]).

n([mass(toevoer)],de,[],[bloed]).

n([mass(toewijding)],de,[]).

n([pl(toewijzingen),sg(toewijzing)],de,[sbar,vp]).

n([pl(toezeggingen),sg(toezegging)],de,[sbar,vp]).

n([mass(toezicht)],het,
  [pred_pp(onder)],
  []).

n([mass(toezicht)],het,[],
  [camera,
   douane,
   woning]).

n([sg(toezichthouder),pl(toezichthouders)],de,[app_measure]).

n([pl('toga\'s'),sg(toga)],de,[]).

n([pl(toiletten),sg(toilet)],het,[],
  [i(dame,dames),
   i(heer,heren)
  ]).

n([pl(tollen),sg(tol)],de,[],[brom]).

n([pl(toleranties),sg(tolerantie)],de,[],[in]).

n([pl(tolken),sg(tolk)],de,[]).

n([pl([tolken,gebarentaal]),sg([tolk,gebarentaal])],de,[]).

n([pl(tomaten),sg(tomaat)],de,[],[dim(tomaatje)]).

n([pl(tomahawks),sg(tomahawk)],de,[]).

n([pl(tomben),pl(tombes),sg(tombe)],de,[]).

n([meas(ton),pl(tonnen)],de,[meas_mod,measure],
  [giga,
   kilo,
   mega]).

n([pl(tonelen),sg(toneel)],het,[],[wereld]).

n([pl(toneelspelen),sg(toneelspel)],het,[]).

n([pl(toneelstukken),sg(toneelstuk)],het,[],[dim(toneelstukje)]).

n([pl(tongen),sg(tong)],de,[],[dim(tongetje)]).

n([sg(tonijn),pl(tonijnen)],de,[],[blauwvin]).

n([pl(tonnetjes),sg(tonnetje)],het,[measure]).

n([pl(togen),sg(toog)],de,[]).

n([pl(tooien),sg(tooi)],de,[]).

n([sg(tool),pl(tools)],de,[]).

n([pl(tomen),sg(toom)],de,[]).

n([pl(tonen),sg(toon)],de,[vp],
  [dim(toontje)]).

n([pl(tonen),sg(toon)],de,[],
  [fluister,
   dim(toontje)]).

n([pl(toonaarden),sg(toonaard)],de,[]).

n([sg(toonder),pl(toonders)],de,[]).

n([pl(toonhoogten),pl(toonhoogtes),sg(toonhoogte)],de,[]).

n([pl(toonzettingen),sg(toonzetting)],de,[]).

n([mass(toorn)],de,[]).

n([pl(toortsen),sg(toorts)],de,[]).

n([pl(toosten),sg(toost)],de,[]).

n([pl(toppen),sg(top)],de,[],
  [berg,
   heuvel,
   klimaat,
   landmacht,
   leger,
   partij,
   h('EU'),
   h('G7'),
   h('G8'),
   h('NAVO'),h('Navo'),'Navo','NAVO',
   h('NS'),
   sub,
   vinger,
   wereld,
   dim(topje)]).

n([sg([top,level,domain])],het,[]).

n([sg(topic),pl(topics)],both,[]).

n([pl(topmannen),sg(topman),pl(toplui)],de,[]).

n([pl(toppers),sg(topper)],de,[],[sub]).

n([pl(topscorers),sg(topscorer)],de,[],[club]).

n([pl(topscoorsters),sg(topscoorster)],de,[],[club]).

n([sg(toptien)],de,[]).

n([mass(topvorm)],de,[pred_pp(in)]).

n([pl(torren),sg(tor)],de,[],
  [bok,
   knip,
   dim(torretje)]).

n([pl(torens),sg(toren)],de,[],
  [kerk,
   i(klok,klokke),i(klok,klokken),
   i(kijk_uit,uitkijk),
   televisie,
   vuur,
   wacht,
   water,
   dim(torentje)]).

n([sg(tornado),pl('tornado\'s')],de,[]).

n([pl('torpedo\'s'),sg(torpedo)],de,[]).

n([sg(tors),pl(torsen)],de,[]).

%% geen tor_til_la
n([sg(tortilla),pl('tortilla\'s')],de,[]).

n([sg(tosti),pl('tosti\'s')],de,[]).

n([pl(totalen),sg(totaal)],het,[],
  [balans,
   i(punt,punten)]).

n([mass(totaliteit)],de,[]).

n([sg(toto)],de,[]).

n([mass(totstandbrenging)],de,[]).

n([mass(totstandkoming)],de,[]).

n([pl(tours),sg(tour)],de,[]).

n([sg(touringcar),pl(touringcars)],de,[]).

n([pl(tournees),sg(tournee)],de,[],[dim(tourneetje)]).

n([pl(tournooien),sg(tournooi)],het,[],[dim(tournooitje)]).

n([pl(touroperators),sg(touroperator),
   pl([tour,operators]),sg([tour,operator])],de,[]).

n([pl(touwen),sg(touw)],het,[],[dim(touwtje)]).

n([pl(tovenaars),pl(tovenaren),sg(tovenaar)],de,[],[dim(tovenaartje)]).

n([mass(tover)],de,[]).

n([pl(toverijen),sg(toverij)],de,[sbar,vp]).

n([mass(toverslag)],de,[]).

n([sg(township),pl(townships)],both,[]).

n([sg(toxiciteit),pl(toxiciteiten)],de,[]).

n([mass(traagheid)],de,[]).

n([pl(tranen),sg(traan)],de,[],[dim(traantje)]).

n([sg(tracé),pl('tracé\'s')],het,[]).

n([sg(track),pl(tracks)],de,[]).

n([pl(tractaten),sg(tractaat)],het,[sbar,vp]).

n([sg(tractie)],de,[]).

n([pl(tractoren),pl(tractors),sg(tractor)],de,[]).

n([pl(tradities),sg(traditie)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp]).

n([pl(tradities),sg(traditie)],de,[],
  [schilder]).

n([pl(tragedies),pl(tragediën),sg(tragedie)],de,
  [sbar,
   subject_sbar]).

n([mass(tragiek)],de,[subject_sbar]).

n([sg(trail),pl(trails)],de,[]).

n([sg(trailer),pl(trailers)],de,[]).

n([sg(trainee),pl(trainees)],de,[]).

n([pl(trainers),sg(trainer)],de,[],
  [h(assistent),
   hoofd,
   hulp,
   jeugd,
   s(keeper),
   h('Feyenoord'),h('PSV'),h('Ajax'),
   schaats,
   voetbal]).

n([pl(trainsters),sg(trainster)],de,[],
  [h(assistent),
   hoofd,
   hulp,
   jeugd,
   s(keeper),
   h('Feyenoord'),h('PSV'),h('Ajax'),
   schaats,
   voetbal]).

n([mass(training),pl(trainingen),sg(training)],de,
  [app_measure,
   temp_mod
  ],
  []).
n([mass(training),pl(trainingen),sg(training)],de,
  [temp_mod],
  [s(groep),
   kracht]).

n([pl(trainingspakken),sg(trainingspak)],het,[]).

n([pl(trajecten),sg(traject),
   pl(trajekten),sg(trajekt)],het,[]).

n([pl(traktaten),sg(traktaat)],het,[sbar,vp]).

n([pl(traktementen),sg(traktement)],het,[]).

n([pl(traktoren),pl(traktors),sg(traktor)],de,[]).

n([pl(tralies),pl(traliën),sg(tralie)],de,[]).

n([pl(trammen),pl(trams),sg(tram)],de,[],
  [dim(trammetje),
   snel]).

n([pl(tramhalten),pl(tramhaltes),sg(tramhalte)],de,[]).

n([pl(trances),sg(trance)],de,[]).

n([pl(tranches),sg(tranche)],de,[measure]).

n([pl(tranquillizers),sg(tranquillizer)],de,[]).

n([pl(transacties),pl(transactiën),sg(transactie),
   pl(transakties),pl(transaktiën),sg(transaktie)],de,[sbar,vp]).

n([mass(transcendentie)],de,[]).

n([pl(transfers),sg(transfer)],both,[]).

n([pl(transfersommen),sg(transfersom)],de,[]).

n([pl(transformaties),sg(transformatie)],de,[]).

n([sg(transformator),pl(transformatoren)],de,[]).

n([sg(transgender),pl(transgenders)],de,[]).

n([pl(transistors),pl(transistoren),sg(transistor)],de,[]).

n([sg(transitie),pl(transities)],de,[]).

n([mass(transmissie)],de,[]).

n([mass(transparantie)],de,[]).

n([sg(transplantatie),pl(transplantaties)],de,[],[xeno]).

n([pl(transpiraties),sg(transpiratie)],de,[]).

n([pl(transporten),sg(transport)],het,[measure]).

n([pl(transporten),sg(transport)],het,[],[weg]).

n([sg(transporteur),pl(transporteurs)],de,[]).

n([pl(transseksuelen),sg(transseksueel),
   pl(transsexuelen),sg(transsexueel)],de,[]).

n([mass(trant)],de,[van_sbar]).

n([mass(trant)],de,[],
  [vertel]).

n([pl(trappen),sg(trap)],de,[],
  [achter,
   binnen,
   brand,
   buiten,
   kelder,
   mistrap,
   rol,
   tussen,
   voor,  % voortrap =/= rap_voort
   wentel,
   dim(trappetje),
   dim(trapje)]).

n([sg([trap,na])],de,[]).

n([pl(trappehuizen),sg(trappehuis)],het,[]).

n([pl(trappenhuizen),sg(trappenhuis)],het,[]).

n([sg(trapper),pl(trappers)],de,[]). % en niet trap_pers

n([pl(traptreden),sg(traptrede)],de,[]).

n([pl('trauma\'s'),pl(traumata),sg(trauma)],both,[]).

n([sg(traumatoloog),pl(traumatologen)],de,[]).

n([mass(traumatologie)],de,[]).

n([sg([traveller,cheque]),pl([traveller,cheques]),
   sg([traveler,cheque]),pl([traveler,cheques]),
   sg([travellers,cheque]),pl([travellers,cheques]),
   sg([travelers,cheque]),pl([travelers,cheques])],de,[]).

n([pl(trawanten),sg(trawant)],de,[]).

n([pl(trechters),sg(trechter)],de,[],[dim(trechtertje)]).

n([pl(treden),sg(tred)],de,[]).

n([pl(treden),sg(trede)],de,[meas_mod]).  % hij zakt een trede

n([pl(treeën),sg(tree)],de,[meas_mod],[dim(treetje)]).  % zakt een treetje

n([pl(treeplanken),sg(treeplank)],de,[]).

n([mass(treffen)],het,[]).

n([pl(treffers),sg(treffer)],de,[],
  [s(aansluiting),
   s(opening),
   tegen,
   vol]).

n([pl(trefpunten),sg(trefpunt)],het,[]).

n([pl(treinen),sg(trein)],de,[],
  [dim(treintje),
   i(goed,goederen),
   i(hoog_snelheid,'hoge-snelheids'),
   kabel,
   risico,
   stoom,
   stop,
   zweef]).

n([pl(trekken),sg(trek)],de,[vp,subject_vp,sbar,subject_sbar],[dim(trekje)]).

n([pl(trekken),sg(trek)],de,[pred_pp(in)]).

n([pl(trekken),sg(trek)],de,[],
  [s(gelaat)]).

n([pl(trekkers),sg(trekker)],de,[],
  [s(publiek),
   s(uitkering)]).

n([pl(trekkingen),sg(trekking)],de,[]).

n([pl(trends),sg(trend)],de,[sbar,subject_sbar,vp,subject_vp]).

n([sg(trendsetter),pl(trendsetters)],de,[]).

n([pl(treurigheden),sg(treurigheid)],de,[sbar,vp]).

n([mass(treurnis)],de,[]).

n([sg(trial),pl(trials)],de,[]).

n([sg([trial,and,error]),sg([trial,en,error]),sg('trial-and-error')],de,[]).

n([sg(trialoog),pl(trialogen)],de,[]).

n([sg(triatlon),pl(triatlons),
   sg(triathlon),pl(triathlons)],both,[]).

n([pl(tribunalen),sg(tribunaal)],het,[]).

n([pl(tribunes),sg(tribune)],de,[]).

n([sg(trigger),pl(triggers)],de,[]).

n([meas(triljoen),pl(triljoenen)],both,
  [meas_mod,measure]).

%% retroflexe tril, uvulaire tril, ... biologie?
n([sg(tril)],de,[]).

n([pl(trillingen),sg(trilling)],de,[],
  [s(geluid)]).

n([sg(trilogie),pl(trilogieën)],de,[]).

n([sg(triloog),pl(trilogen)],de,[]).

n([mass(trim)],de,[]).

n([pl(trimesters),sg(trimester)],het,[temp_mod]).

n([pl('trio\'s'),sg(trio)],het,[measure],[dim(triootje)]).

n([pl(triomfen),sg(triomf)],de,[]).

n([pl(trips),sg(trip)],de,[temp_mod,sbar],[dim(tripje)]).

n([sg(tripartite),pl(tripartites)],de,[]).

n([mass(triplex)],het,[]).

n([sg(trits)],de,[measure]).

n([pl(trivia)],both,[]).

n([sg(triviant)],both,[],[dim(triviantje)]).

n([pl(troeven),sg(troef)],de,[subject_sbar]).

n([pl(troepen),sg(troep)],de,
  [measure],
  [dim(troepje)]).  % een troepje bezoekers
n([pl(troepen),sg(troep)],de,[],
  [s(bezetting),  % niet bezetting_stro_epos
   coalitie,
   elite,
   s(vrede),
   h('NAVO'),h('Navo'),'NAVO','Navo',
   dim(troepje)]).

n([pl(trofeeën),sg(trofee)],de,[]).

n([pl(troggen),sg(trog)],de,[]).

n([pl(trommen),pl(troms),sg(trom)],de,[]).

n([sg(trombone),pl(trombones)],de,[]).

n([sg(trombonist),sg(tromboniste),
   pl(trombonisten),pl(trombonistes)],de,[]).

n([sg(trombose),pl(tromboses)],de,[]).

n([pl(trommels),sg(trommel)],de,[],
  [brood,
   droog,
   dim(trommeltje)]).

n([pl(trommelaars),sg(trommelaar)],de,[]).

n([pl(trommelremmen),sg(trommelrem)],de,[]).

n([pl(trommelvliezen),sg(trommelvlies)],het,[],
  [dim(trommelvliesje)]).

n([pl(trompetten),sg(trompet)],de,[],[lof]).

n([pl(trompettes),
   pl([trompettes,de,mort]),
   pl([trompettes,des,mort]),
   pl([trompettes,de,la,mort]),
   pl([trompettes,des,morts]),
   pl([trompettes,de,morts])],de,[]).

n([pl(trompettisten),sg(trompettist)],de,[]).

n([pl(tronies),sg(tronie)],de,[]).

n([pl(tronen),sg(troon)],de,[],[dim(troontje)]).

n([mass(troost)],de,[subject_sbar,subject_vp,sbar,vp]).

n([pl(tropen)],de,[]).

n([pl(trossen),sg(tros)],de,[measure],[dim(trosje)]).

n([mass(trots)],de,[subject_sbar,sbar]).  % hij glimt van trots dat ...

n([pl(trottoirs),sg(trottoir)],het,[]).

n([pl(troubadours),sg(troubadour)],de,[]).

n([mass(trouw)],de,[],[onder]).

n([sg(trouwer),pl(trouwers)],de,[]).

n([sg(trouwerij),pl(trouwerijen)],de,[]).

n([pl(trucs),sg(truc),
   pl(truuks),sg(truuk)],de,
  [vp,sbar,subject_sbar,subject_vp],
  [dim(trucje),
   dim(truukje)]).

n([sg(trucker),pl(truckers)],de,[]).

n([pl(trucks),sg(truck)],de,[],[hef]).

n([pl(truffels),sg(truffel)],de,[],[dim(truffeltje)]).

n([pl(truien),sg(trui)],de,[],
  [s(leider),
   regenboog,
   dim(truitje)]).

n([pl(trusts),sg(trust)],de,[]).

n([pl(trutten),sg(trut)],de,[]).

n([pl([try,outs]),pl('try-outs'),
   sg([try,out]),sg('try-out')],de,[]).

n([pl(tsaren),sg(tsaar)],de,[]).

n([sg(tsunami),
   sg(tsoenami)],de,[]).

n([sg(tuba),pl('tuba\'s')],de,[]).

n([pl(tuben),pl(tubes),sg(tube)],de,[measure],[dim(tubetje)]).

n([mass(tuberculose),mass(tuberkulose)],de,[]).

n([mass(tucht)],de,[]).

n([sg(tuft)],de,[]).

n([pl(tuigen),sg(tuig)],het,[]).

n([pl(tuilen),sg(tuil)],de,[measure],[dim(tuiltje)]).

n([pl(tuinen),sg(tuin)],de,[],
  [achter,
   i(beeld,beelden),
   dim(tuintje),
   voor
  ]).

n([mass(tuinbouw)],de,[],[glas]).

n([sg(tuinbouwer),pl(tuinbouwers)],de,[],[]).

n([pl(tuinders),sg(tuinder)],de,[]).

n([pl(tuiniers),sg(tuinier)],de,[]).

n([pl(tuinlieden),pl(tuinlui),sg(tuinman)],de,[]).

n([pl(tuinpaden),sg(tuinpad)],het,[]).

n([pl(tuiten),sg(tuit)],de,[]).

%% fietstaxi
n([sg([tuk,tuk]), sg('tuk-tuk'),
   pl([tuk,tuks]),sg('tuk-tuks')],de,[]).

n([pl(tulbanden),sg(tulband)],de,[]).

n([pl(tulpen),sg(tulp)],de,[]).

n([pl(tumoren),pl(tumors),sg(tumor)],de,[],
  [hersen]).

n([pl(tumulten),sg(tumult)],het,[]).

n([sg(tune),pl(tunes)],de,[]).

n([pl(tunieken),sg(tuniek)],de,[]).

n([pl(tunnels),sg(tunnel)],de,[],
  [bus,
   fiets,
   spoor,
   tol,
   tram,
   dim(tunneltje)]).

n([sg(turbulentie),pl(turbulenties)],de,[]).

n([pl(turven),sg(turf)],de,[]).

n([mass(turfmolm)],de,[]).

n([sg(turn),pl(turns)],de,[]).

n([pl(turners),sg(turner)],de,[]).

n([pl(turnsters),sg(turnster)],de,[]).

n([sg(tussenbouw)],de,[]). % VL?

n([sg(tussendoortje),pl(tussendoortjes)],het,[]).

n([pl(tussenkomsten),sg(tussenkomst)],de,[]).

n([sg(tussenlaag),pl(tussenlagen)],de,[]).

n([pl(tussenpersonen),sg(tussenpersoon)],de,[]).

n([pl(tussenpozen),sg(tussenpoos)],de,[]).

n([sg(tussenschakel),pl(tussenschakels)],de,[]).

n([sg(tussenspel),pl(tussenspelen)],het,[]).

n([pl(tussenstanden),sg(tussenstand)],de,[]).

n([sg(tussenstap),pl(tussenstappen)],de,[]).

n([sg(tussenstop),pl(tussenstops),pl(tussenstoppen)],de,[]).

n([sg(tussenzwaai),pl(tussenzwaaien)],de,[]).

n([sg(tut),pl(tutten)],de,[]).

n([mass([tutti,frutti])],de,[]).

n([sg(tuut),pl(tuten)],de,[]).  % zak/geluidje

n([sg(tv),pl('tv\'s')],de,[]).

n([pl(twaalven)],de,[]).

n([bare_meas(tweemaal),pl(tweemalen)],both,[temp_mod,measure,sbar]).

n([pl(tweeën),sg(twee)],de,[],[dim(tweetje)]).

n([sg([tweede,kerstdag]),
   sg(['Tweede','Kerstdag'])],de,[temp_mod,sbar]).

n([sg([tweede,paasdag]),
   sg(['Tweede','Paasdag'])],de,[temp_mod,sbar]).

n([sg([tweede,pinksterdag]),
   sg(['Tweede','Pinksterdag'])],de,[temp_mod,sbar]).

n([sg('tweede-fase')],de,[]).

n([pl(tweeden),pl(tweedes)],both,[]).

n([mass(tweedehands)],both,[]).

n([sg(tweedejaars),pl(tweedejaars)],de,[]).

n([sg(tweedeling),pl(tweedelingen)],de,[]).

n([pl(tweelingen),sg(tweeling)],de,[]).

n([pl(tweelingbroers),sg(tweelingbroer)],de,[]).

n([mass(tweeslachtigheid)],de,[]).

n([mass(tweespalt)],de,[]).

n([mass(tweestrijd)],de,[]).

n([sg(tweep),pl(tweeps)],de,[]).  % iemand die tweet?

n([sg(tweet),pl(tweets)],de,[],[re]).

n([sg(tweeverdiener),pl(tweeverdieners)],de,[]).

n([pl(twijfels),sg(twijfel)],de,[sbar,
				 subject_sbar,
				 van_sbar,
				 pred_pp(buiten),
                                 pred_pp(in),
				 pred_pp_pl(zonder),  % ?????
                                 pred_pp(in,subject_sbar_no_het),
				 pred_pp(buiten,subject_sbar)]).

n([pl(twijfelaars),sg(twijfelaar)],de,[]).

n([sg(twijfelgeval),pl(twijfelgevallen)],het,[subject_sbar]).

n([pl(twijgen),sg(twijg)],de,[],[dim(twijgje)]).

n([pl(twintigers),sg(twintiger)],de,[]).

n([pl(twisten),sg(twist)],de,[]).

n([pl(twistgesprekken),sg(twistgesprek)],het,[sbar]).

n([sg(twitter)],both,[]).

n([sg(tycoon),pl(tycoons)],de,[]).

n([sg(tyfoon),pl(tyfoons)],de,[]).

n([mass(tyfus)],de,[]).

n([pl(typen),pl(types),sg(type)],het,
  [measure,
   pred_pp(van)],
  [letter,
   school,
   dim(typetje)]).

n([pl(typeringen),sg(typering)],de,[sbar,vp]).

n([pl(typistes),sg(typiste)],de,[]).

n([pl(typisten),sg(typist)],de,[]).

n([mass(typologie)],de,[]).

n([pl('u\'s'),sg(u)],de,[],[dim('u\'tje')]).

n([sg(ugg),pl(uggs)],de,[]).

n([pl(uien),sg(ui)],de,[],
  [bos,
   lente,
   dim(uitje)]).

n([sg(uier),pl(uiers)],de,[]).

n([pl(uilen),sg(uil)],de,[],
  [kerk,
   sneeuw,
   spinner, % insect
   steen,
   dim(uiltje)]).

n([sg(uitbanning),pl(uitbanningen)],de,[]).

n([pl(uitbarstingen),sg(uitbarsting)],de,
  [sbar,vp]).

n([pl(uitbarstingen),sg(uitbarsting)],de,[],
  [s(geweld),
   vulkaan]).

n([pl(uitbaters),sg(uitbater)],de,[]).

n([pl(uitbaatsters),sg(uitbaatster)],de,[]).

n([pl(uitbeeldingen),sg(uitbeelding)],de,[]).

n([sg(uitbesteding),pl(uitbestedingen)],de,[]).

n([pl(uitbetalingen),sg(uitbetaling)],de,[]).

n([pl(uitblinkers),sg(uitblinker)],de,[]).

n([pl(uitblinksters),sg(uitblinkster)],de,[]).

n([pl(uitbouwen),sg(uitbouw)],de,[]).

n([pl(uitbraken),sg(uitbraak)],de,[]).

n([pl(uitbranders),sg(uitbrander)],de,[]).

n([pl(uitbreidingen),sg(uitbreiding)],de,[]).

n([mass(uitbreng)],de,[]).

n([pl(uitbuiters),sg(uitbuiter)],de,[]).

n([pl(uitbuitingen),sg(uitbuiting)],de,[]).

n([pl(uitbundigheden),sg(uitbundigheid)],de,[]).

n([pl(uitdagers),sg(uitdager)],de,[]).

n([pl(uitdaagsters),sg(uitdaagster)],de,[]).

n([pl(uitdagingen),sg(uitdaging)],de,[sbar,vp]).

n([mass(uitdienstname)],de,[]).

n([sg(uitdraai),pl(uitdraaien)],de,[]).

n([mass(uitdroging)],de,[]).

n([pl(uitdrukkingen),sg(uitdrukking)],de,
  [],
  [s(gelaat),
   s(gezicht)]).

n([pl(uitdrukkingen),sg(uitdrukking)],de,
  [sbar,
   vp,
   app_measure,
   start_app_measure,
   np_app_measure]).

n([pl(uitdrukkingsvormen),sg(uitdrukkingsvorm)],de,[measure]).

n([pl(uiteenzettingen),sg(uiteenzetting)],de,[]).

n([pl(uiteinden),sg(uiteinde)],both,[]).

n([pl(uiterlijks),sg(uiterlijk)],het,[]).

n([sg(uiterwaarde),pl(uiterwaarden)],de,[]).

n([mass(uitfasering)],de,[]).

n([mass(uitgaansleven)],het,[]).

n([pl(uitgangen),sg(uitgang)],de,[]).

n([pl(uitgangspunten),sg(uitgangspunt)],het,
  [sbar,
   vp,
   subject_vp,
   subject_sbar]).

n([pl(uitgaven),sg(uitgave)],de,[],
  [belasting,
   her,
   landbouw,
   s(overheid)]).

n([pl(uitgebreidheden),sg(uitgebreidheid)],de,[]).

n([sg(uitgeefster),pl(uitgeefsters)],de,[]).

n([pl(uitgestrektheden),sg(uitgestrektheid)],de,[]).

n([pl(uitgevers),sg(uitgever)],de,[],
  [i(krant,kranten)]).

n([pl(uitgeverijen),sg(uitgeverij)],de,[]).

n([pl(uitgiften),pl(uitgiftes),sg(uitgifte)],de,[]).

n([sg(uitgooi)],de,[]).

n([mass(uitgroei)],de,[]).

n([pl(uithalen),sg(uithaal)],de,[sbar,vp]).

n([pl(uithangborden),sg(uithangbord)],het,[]).

n([pl(uithoeken),sg(uithoek)],de,[]).

n([pl(uithollingen),sg(uitholling)],de,[]).

n([pl(uitingen),sg(uiting)],de,[sbar]).

n([pl(uitjes),sg(uitje)],het,[],
  [najaar,
   s(personeel),
   s(voorjaar)]).  

n([pl(uitkeringen),sg(uitkering)],de,[],
  [s(bijstand),
   s(eindejaar),
   i(nabestaande,nabestaanden),
   s(overlijden),
   remigratie,h(remigratie),
   slot,
   h('AOW'),h(aow),
   vervolg,
   h('WAO'),
   h('WW'),
   i(wees,wezen),
   i(weduwe,weduwen),
   s(werkeloosheid),s(werkloosheid),
   zwangerschap]).

n([sg(uitkeringsgerechtigde),pl(uitkeringsgerechtigden)],de,[]).

n([pl(uitkijken),sg(uitkijk)],de,[]).

n([pl(uitkijkposten),sg(uitkijkpost)],de,[]).

n([pl(uitkomsten),sg(uitkomst)],de,
  [sbar,
   vp,
   subject_vp,
   subject_sbar]).

n([sg(uitkoop)],de,[]).

n([pl(uitlaten),sg(uitlaat)],de,[sbar,vp]).

n([pl(uitlatingen),sg(uitlating)],de,[sbar,vp]).

n([mass(uitleen)],de,[]).

n([mass(uitleg)],de,[sbar]).

n([sg(uitlegger),pl(uitleggers)],de,[]).

n([pl(uitleggingen),sg(uitlegging)],de,[]).

n([pl(uitleveringen),sg(uitlevering)],de,[]).

n([pl(uitlokkingen),sg(uitlokking)],de,[]).

n([pl(uitlopen),sg(uitloop)],de,[]).

n([pl(uitlopers),sg(uitloper)],de,[]).

n([sg(uitnemendheid)],de,[]).

n([pl(uitnodigingen),sg(uitnodiging)],de,[vp]).

n([mass(uitoefening)],de,[]).

n([sg(uitprint),pl(uitprinten)],de,[]).  % VL?

n([mass(uitputting)],de,[]).

n([pl(uitreikingen),sg(uitreiking)],de,[],[prijs]).

n([sg(uitreis)],de,[]).

n([mass(uitroeiing)],de,[]).

n([pl(uitroepen),sg(uitroep)],de,[start_app_measure]).

n([pl(uitroeptekens),sg(uitroepteken)],het,[]).

n([sg(uitrol)],de,[]).

n([sg(uitruil)],de,[]).

n([sg(uitruk)],de,[]).

n([pl(uitrustingen),sg(uitrusting)],de,[],
  [duik,
   golf,
   sport,
   standaard]).

n([mass(uitschakeling)],de,[]).

n([pl(uitschieters),sg(uitschieter)],de,[sbar,vp]).

n([pl(uitslagen),sg(uitslag)],de,[sbar,vp],[huid]).

%% en niet uit_lover
n([sg(uitslover),pl(uitslovers)],de,[]).

n([sg(uitsloofster),pl(uitsloofsters)],de,[]).

n([pl(uitsluitingen),sg(uitsluiting)],de,[]).

n([sg(uitsluitsel)],het,[sbar]).

n([pl(uitsmijters),sg(uitsmijter)],de,[]).

n([sg(uitsnede),pl(uitsneden)],de,[]).

n([pl(uitspanningen),sg(uitspanning)],de,[]).

n([mass(uitspansel)],het,[]).

n([pl(uitspattingen),sg(uitspatting)],de,[sbar,vp]).

n([mass(uitspoor)],both,[]).  % iets met banden van auto's

n([pl(uitspraken),sg(uitspraak)],de,
  [sbar,
   vp,
   app_measure,
   np_app_measure,
   start_app_measure]).

n([sg(uitsprong),pl(uitsprongen)],de,[]).

n([pl(uitstallingen),sg(uitstalling)],de,[]).

n([pl(uitstappen),sg(uitstap)],de,[],[dim(uitstapje)]).

n([pl(uitsteeksels),sg(uitsteeksel)],het,[],[dim(uitsteekseltje)]).

n([pl(uitstekken),sg(uitstek)],het,[]).

n([mass(uitstel)],both,[]).

n([pl(uitstoten),sg(uitstoot)],de,[],
  [h('CO2')]).

n([pl(uitstralingen),sg(uitstraling)],de,[van_sbar]).

n([mass(uitstroom)],de,[]).

n([pl(uitstulpingen),sg(uitstulping)],de,[]).

n([pl(uittochten),sg(uittocht)],de,[]).

n([sg(uittrap),pl(uittrappen)],de,[]).

n([pl(uittredingen),sg(uittreding)],de,[]).

n([pl(uittreksels),sg(uittreksel)],het,[],[dim(uittrekseltje)]).

n([pl(uitvaarten),sg(uitvaart)],de,[]).

n([pl(uitvallen),sg(uitval)],de,[sbar,vp]).

n([pl(uitvallen),sg(uitval)],de,[],[stroom]).

n([sg(uitvaller),pl(uitvallers)],de,[]).

n([pl(uitverkopen),sg(uitverkoop)],de,[]).

n([pl(uitvinders),sg(uitvinder)],de,[]).

n([pl(uitvindsters),sg(uitvindster)],de,[]).

n([pl(uitvindingen),sg(uitvinding)],de,[]).

n([pl(uitvloeiselen),pl(uitvloeisels),sg(uitvloeisel)],het,[sbar,vp]).

n([pl(uitvluchten),sg(uitvlucht)],de,[sbar,vp]).

n([pl(uitvoeren),sg(uitvoer)],de,[]).

n([pl(uitvoerders),sg(uitvoerder)],de,[]).

n([pl(uitvoeringen),sg(uitvoering)],de,[],[dim(uitvoerinkje)]).

n([sg(uitvraag)],de,[]).

n([pl(uitwassen),sg(uitwas)],de,[sbar]).

n([pl(uitweidingen),sg(uitweiding)],de,[]).

n([pl(uitwerkingen),sg(uitwerking)],de,[sbar]).

n([pl(uitwerpselen),pl(uitwerpsels),sg(uitwerpsel)],het,[]).

n([sg(uitwijk)],de,[]).

n([pl(uitwijzingen),sg(uitwijzing)],de,[]).

n([pl(uitwisselingen),sg(uitwisseling)],de,[]).

n([mass(uitzaai)],de,[]).

n([pl(uitzaaiingen),sg(uitzaaiing)],de,[]).

n([pl(uitzendingen),sg(uitzending)],de,[],
  [nieuws,
   televisie,tv,h(tv),f([tv]),i(tv,'TV-')
  ]).

n([sg(uitzet),pl(uitzetten)],de,[]).

n([pl(uitzettingen),sg(uitzetting)],de,[]).

n([pl(uitzichten),sg(uitzicht)],het,[sbar,vp]).

n([pl(uitzonderingen),sg(uitzondering)],de,[sbar,vp]).

n([pl(ultimatums),sg(ultimatum)],het,[sbar,vp]).

n([sg(ultimo)],de,[]).  % iets met beurskoersen

n([mass(ultramarijn)],het,[]).

n([mass(ultraviolet)],het,[]).

n([sg(umpire),pl(umpires)],de,[]).

n([mass(unanimiteit)],de,[]).

n([sg(undercover),pl(undercovers)],de,[]).

n([sg(underdog),pl(underdogs)],de,[]).

n([sg(understatement),pl(understatements)],both,
  [subject_sbar,
   sbar,
   subject_vp]).

n([sg(unicum)],het,
  [subject_sbar,
   subject_vp
  ]).

n([pl(unies),sg(unie)],de,[],
  [atletiek,
   h(douane),
   munt,
   wielren]).

n([pl(uniformen),pl(uniforms),sg(uniform)],both,[]).

n([mass(uniformiteit)],de,[]).

n([pl(uniformjassen),sg(uniformjas)],de,[]).

n([mass(unilateralisme)],het,[]).

n([sg(unionist),pl(unionisten)],de,[]).

n([pl(units),sg(unit)],de,[]).

n([mass(universaliteit)],de,[]).

n([pl(universiteiten),sg(universiteit)],de,[]).

n([pl(universums),sg(universum)],het,[]).

n([sg(update),pl(updates)],de,[],
  [nieuws,
   software,
   h(software),
   f([software])]).

n([sg(upgrade),pl(upgrades)],de,[]).

n([sg(upload),pl(uploads)],de,[]).

n([sg(uppie)],het,[]).  % in mijn uppie

n([pl([ups,and,downs]),pl([ups,en,downs])],de,[]).

n([mass(uranium)],het,[]).

n([sg([urban,legend]),pl([urban,legends])],de,[]).

n([mass(urbanisatie),mass(urbanizatie)],de,[]).

n([pl(urgenties),sg(urgentie)],de,[sbar,vp]).

n([mass(urine)],de,[]).

n([sg(url)],de,[]).

n([sg(urn),pl(urnen)],de,[]).

n([pl(utopieën),sg(utopie)],de,[subject_vp,subject_sbar]).

n([pl(utopisten),sg(utopist)],de,[]).

n([sg(uu)],de,[]).

n([meas(uur),pl(uren)],het,[temp_mod,measure,sbar],
  [les,
   mentor,
   tussen,
   dim(uurtje)]).

n([pl(uren),sg(uur),
   ignore_stem(uur),
   ignore_stem(uur_DIM)],het,[],
  [kantoor,
   s(opening),
   i(vraag,vragen),
   dim(uurtje)]).

n([sg(uwe),pl(uwen)],both,[]).

n([pl('v\'s'),sg(v)],de,[],[dim('v\'tje')]).

n([pl(vaagheden),sg(vaagheid)],de,[sbar,vp]).

n([pl(vanen),sg(vaan)],de,[],[dim(vaantje)]).

n([pl(vaandels),sg(vaandel)],both,[],[dim(vaandeltje)]).

n([pl(vaandrigs),sg(vaandrig)],de,[]).

n([pl(vaardigheden),sg(vaardigheid)],de,[vp]).

n([pl(vaardigheden),sg(vaardigheid)],de,[],
  [deel,
   rij,
   spreek,
   taal]).

n([pl(vaargeulen),sg(vaargeul)],de,[]).

n([pl(vaarten),sg(vaart)],de,[],
  [sneltrein,
   dim(vaartje)]).

n([sg(vaars),pl(vaarzen)],de,[]).

n([pl(vaartuigen),sg(vaartuig)],het,[],
  [s(visser)]).

n([pl(vaarwateren),pl(vaarwaters),sg(vaarwater)],het,[]).

n([pl(vaarwels),sg(vaarwel)],het,[]).

n([pl(vazen),sg(vaas)],de,[measure],[dim(vaasje)]).

n([mass(vaat)],de,[]).

n([pl(vacatures),sg(vacature)],de,[]).

n([pl(vaccins),sg(vaccin)],het,[]).

n([pl(vaccinaties),sg(vaccinatie)],de,[]).

n([pl(vachten),sg(vacht)],de,[]).

n([pl(vacua),sg(vacuüm)],het,[sbar]).

n([pl(vacua),sg(vacuüm)],het,[],
  [s(macht)]).

n([pl(vaders),sg(vader),pl(vaderen)],de,[],
  [aarts,
   biecht,
   buurt,
   huis,
   kerk,
   oer,
   peet,
   pleeg,
   schoon,
   stam,
   stief,
   voor,
   dim(vadertje)]).

n([sg(['Vader',des,'Vaderlands']),pl(['Vaders',des,'Vaderlands'])],de,[]).

n([pl(vaderlanden),sg(vaderland)],het,[]).

n([mass(vaderlandsliefde)],de,[]).

n([mass(vaderschap)],het,[]).

n([pl(vagebonden),sg(vagebond)],de,[]).

n([mass(vagevuur)],het,[]).

n([pl('vagina\'s'),sg(vagina)],de,[]).

n([mass(vaginisme)],het,[]).

n([pl(vakken),sg(vak)],het,[app_measure],
  [zoek,
   keuze,
   h(keuze),
   dim(vakje)]).

n([pl(vakanties),sg(vakantie)],de,
  [temp_mod,
   sbar],
  [herfst,
   kerst,
   krokus,
   mei,
   paas,
   pinkster,
   s(voorjaar),
   zomer]).

n([pl(vakatures),sg(vakature)],de,[]).

n([pl(vakbonden),sg(vakbond)],de,[],
  [politie,
   s(speler),
   spoorweg]).

n([mass(vakliteratuur)],de,[]).

n([pl(vaklieden),pl(vaklui),pl(vakmannen),pl(vakmensen),sg(vakman)],de,[]).

n([mass(vakmanschap)],het,[]).

n([pl(vallen),sg(val)],de,[],
  [armoede,
   knie,
   koers,
   regen,
   sneeuw,
   terug,
   winst]).

n([mass(validiteit)],de,[]).

n([sg(validering),pl(valideringen)],de,[]).

n([pl(valiezen),sg(valies)],both,[]).  %VL: de

n([mass(valium)],both,[]).

n([pl(valken),sg(valk)],de,[]).

n([pl(valkuilen),sg(valkuil)],de,[sbar,vp]).

n([pl(valleien),sg(vallei)],de,[],[dim(valleitje)]).

n([pl(valrepen),sg(valreep)],de,[]).

n([mass([valsheid,in,geschrifte])],de,[]).

n([pl(valstrikken),sg(valstrik)],de,[sbar,vp]).

n([pl('valuta\'s'),sg(valuta),pl(valuta)],de,[]).

n([pl(vampiers),sg(vampier),pl(vampieren)],de,[]).

n([pl(vandalen),sg(vandaal)],de,[],[voetbal]).

n([mass(vandalisme)],het,[],[voetbal]).

n([sg(vang)],de,[]).  % deel van molen

n([pl(vangsten),sg(vangst)],de,[]).

n([mass(vanille)],de,[]).

n([mass(vanzelfsprekendheid),pl(vanzelfsprekendheden),sg(vanzelfsprekendheid)],de,
  [sbar,subject_sbar]).

n([pl(varens),sg(varen)],de,[],[dim(varentje)]).

n([sg(variabele),pl(variabelen)],de,[]).

n([pl(varianten),sg(variant)],de,[subject_sbar, subject_vp]).

n([pl(variaties),sg(variatie)],de,[pred_pp(op)]).

n([pl(variëteiten),sg(variëteit)],de,[measure]).

n([pl(varkens),sg(varken)],het,[],[dim(varkentje)]).

n([mass(vastberadenheid)],de,[]).

n([mass(vasteland)],het,[]).

n([mass(vasten)],de,[]).

n([sg(vaster),pl(vasters)],de,[]).

n([mass(vastgoed)],het,[]).

n([mass(vasthoudendheid)],de,[]).

n([pl(vaststellingen),sg(vaststelling)],de,[sbar,vp]).

%% "houvast"
n([mass(vat)],de,[]).

n([pl(vaten),sg(vat)],het,
  [measure],
  [dim(vaatje)]).

n([pl(vaten),sg(vat)],het,[],
  [dim(vaatje),
   bloed,
   kruit]).

n([mass(vatbaarheid)],de,[]).

n([pl(vazallen),sg(vazal)],de,[]).

n([mass(vbo)],het,[]).

n([pl(vechters),sg(vechter)],de,[],[prijs]).

n([pl(vechtersbazen),sg(vechtersbaas)],de,[]).

n([mass(vechtlust)],de,[]).

n([sg(vector),pl(vectoren)],de,[]).

n([pl(vedetten),pl(vedettes),sg(vedette)],de,[]).

n([mass(vee)],het,[],[]).

n([mass(veestapel)],de,[]).

n([mass(veevoer)],both,[]).

n([pl(vegen),sg(veeg)],de,[],
  [voet]).

n([mass(veelheid)],de,[]).

n([pl(veelvouden),sg(veelvoud)],both,[]).

n([mass(veelzijdigheid)],de,[]).

n([sg(veger),pl(vegers)],de,[]).

n([pl(venen),sg(veen)],het,[],[hoog,laag]).

n([pl(veren),sg(veer)],de,[],
  [schroef,
   dim(veetje)]).

n([pl(veren),sg(veer)],het,[]).

n([pl(veerboten),sg(veerboot)],de,[]).

n([sg(veerpont),pl(veerponten)],both,[np_app_measure]).

n([pl(veertigers),sg(veertiger)],de,[]).

n([mass(veeteelt)],de,[]).

n([pl(vegetariërs),sg(vegetariër)],de,[]).

n([pl(vegetaties),sg(vegetatie)],de,[]).

n([pl(vehikels),sg(vehikel)],het,[]).

n([pl(veiligheden),sg(veiligheid)],de,[],
  [s(betaling),
   brand,
   s(staat),
   s(verkeer),
   voedsel]).

n([pl(veiligheidstroepen)],de,[]).

n([pl(veilingen),sg(veiling)],de,[],
  [i(bloem,bloemen),
   h('UMTS')]).

n([pl(veilinghuizen),sg(veilinghuis)],het,[]).

n([sg(velg),pl(velgen)],de,[]).

n([pl(vellen),sg(vel)],het,[measure],[dim(velletje)]).

n([pl(velden),sg(veld)],het,[],
  [bij,
   s(deelnemer),
   graan,
   hoofd,
   koren,
   maai,
   olie,
   onderwijs,
   rijst,
   speel,
   s(training),
   dim(veldje)]).

n([pl(velden),sg(veld)],het,
  [measure,			% een veld korenbloemen
   app_measure],		% veld op een computerscherm
  [dim(veldje)]).

n([pl(veldheren),sg(veldheer)],de,[]).

n([pl(veldtochten),sg(veldtocht)],de,[]).

n([pl(veldwachters),sg(veldwachter)],de,[]).

n([pl(vennen),sg(ven)],het,[],[dim(vennetje)]).

n([mass(venijn)],het,[]).

n([mass(venkel)],de,[]).

n([pl(vennoten),sg(vennoot)],de,[]).

n([pl(vennootschappen),sg(vennootschap)],de,[]).

n([pl(vensters),sg(venster)],both,[],[dim(venstertje)]). %  why both? 

n([pl(venten),sg(vent)],de,[],[dim(ventje)]).

n([mass(ventilatie)],de,[]).

n([pl(ventilatoren),pl(ventilators),sg(ventilator)],de,[]).

n([sg(verachter),pl(verachters)],de,[]).  % grefo speak

n([mass(verachting)],de,[]).

n([mass(verademing)],de,[subject_sbar,subject_vp]).

n([mass(verafgoding)],de,[]).

n([pl('veranda\'s'),sg(veranda)],de,[]).

n([pl(veranderingen),sg(verandering)],de,
  [vp,
   subject_sbar,
   van_sbar]).

n([pl(veranderingen),sg(verandering)],de,
  [],
  [s(gedrag),
   klimaat,
   s(mentaliteit),
   s(naam),naam
  ]).

n([pl(verantwoordelijkheden),
   sg(verantwoordelijkheid)],de,
  [subject_vp,
   subject_sbar]).

n([pl(verantwoordingen),sg(verantwoording)],de,[sbar,vp]).

n([mass(verarming)],de,[]).

n([sg(verbaal),pl(verbalen)],het,[]).

n([pl(verbanden),sg(verband)],het,[sbar],[]).

n([pl(verbanden),sg(verband)],het,[],
  [competitie,
   dwars,
   s(samenwerking),
   s(zin)]).

n([pl(verbanningen),sg(verbanning)],de,[]).

n([mass(verbazing)],de,[]).

n([pl(verbeeldingen),sg(verbeelding)],de,[sbar,vp]).

n([pl(verbeteringen),sg(verbetering)],de,[sbar,vp]).

n([pl(verbeteringen),sg(verbetering)],de,[],
  [s(kwaliteit),
   winst]).

n([mass(verbijstering)],de,[]).

n([pl(verbindingen),sg(verbinding)],de,
  [pred_pp(in)]).

n([pl(verbindingen),sg(verbinding)],de,[],
  [internet,
   oever]).

n([pl(verbintenissen),sg(verbintenis)],de,[sbar,vp]).

n([mass(verbittering)],de,[]).

n([pl(verblijven),sg(verblijf)],het,[],
  [i(gast,gasten),
   nacht
  ]).

n([pl(verboden),sg(verbod)],het,
  [sbar,vp]).

n([pl(verboden),sg(verbod)],het,[],
  [alcohol,
   bordeel,
   export,
   fok,
   import,
   inhaal,
   inreis,
   reclame,
   reis,
   rij,
   rook,
   spreek,
   stadion,
   start,
   straat,
   s(uitgaan),
   s(vervoer),
   vlieg,
   winkel]).

n([pl(verbonden),sg(verbond)],het,[]).

n([pl(verbondenheden),sg(verbondenheid)],de,[],
  [s(lot)]).

n([pl(verborgenheden),sg(verborgenheid)],de,[]).

n([mass(verbouw)],de,[]).

n([pl(verbouwingen),sg(verbouwing)],de,[]).

n([pl(verbrandingen),sg(verbranding)],de,[]).

n([pl(verbredingen),sg(verbreding)],de,[]).

n([sg(verbreider),pl(verbreiders)],de,[]).

n([mass(verbreiding)],de,[]).

n([mass(verbreking)],de,[]).

n([pl(verbroederingen),sg(verbroedering)],de,[]).

n([mass(verbruik)],het,[],[brandstof,
			   energie]).

n([pl(verbruikers),sg(verbruiker)],de,[],
  [groot,
   klein]).

n([pl(verbruiksters),sg(verbruikster)],de,[]).

n([stem(verdenken),
   pl(verdachten),sg(verdachte)],de,[],
  [hoofd,
   mede,
   terreur]).

n([pl(verdachtmakingen),sg(verdachtmaking)],de,[sbar,vp]).

n([mass(verdamping)],de,[]).

n([pl(verdedigers),sg(verdediger)],de,[],
  [centrum,
   doel,
   titel]).

n([pl(verdedigingen),sg(verdediging)],de,[sbar,vp]).

n([pl(verdedigsters),sg(verdedigster)],de,[],[titel]).

n([mass([verdeel,en,heers])],both,[]).

n([mass(verdeeldheid)],de,[]).

n([sg(verdelging),pl(verdelgingen)],de,[]).

n([pl(verdelingen),sg(verdeling)],de,
  [sbar],
  [s(arbeid),
   her,
   s(inkomen),
   onder,
   rol,
   taak]).

n([pl(verdenkingen),sg(verdenking)],de,[sbar,vp]).

n([mass(verderf)],het,[]).

n([pl(verdichtingen),sg(verdichting)],de,[sbar]).

n([sg(verdict),pl(verdicten)],het,[]).

n([pl(verdiensten),sg(verdienste)],de,[sbar,subject_sbar,subject_vp]).

n([sg(verdiep),pl(verdiepen)],de,[]).

n([pl(verdiepingen),sg(verdieping)],de,[],
  [beneden,
   boven,
   onder]).

n([mass(verdoemenis)],de,[]).

n([mass(verdommenis)],de,[]).

n([pl(verdovingen),sg(verdoving)],de,[]).

n([mass(verdraagzaamheid)],de,[]).

n([pl(verdragen),sg(verdrag)],het,[],
  [h('ABM'),
   associatie,
   kernstop,
   vrijhandel]).

n([mass(verdriet)],het,[]).

n([mass(verdringing)],de,[]).

n([mass(verdrinking)],de,[]).

n([mass(verdroging)],de,[]).

n([pl(verdrijvingen),sg(verdrijving)],de,[]).

n([pl(verdrukkingen),sg(verdrukking)],de,[]).

n([pl(verdubbelingen),sg(verdubbeling)],de,[]).

n([pl(verduidelijkingen),sg(verduidelijking)],de,[sbar,vp]).

n([pl(verduisteringen),sg(verduistering)],de,[],
  [s(maan),
   s(zon)]).

n([pl(verdunningen),sg(verdunning)],de,[]).

n([pl(verdwijningen),sg(verdwijning)],de,[]).

n([sg(veredeling),pl(veredelingen)],de,[],[i(plant,planten)]).

n([pl(vereenvoudigingen),sg(vereenvoudiging)],de,[sbar,vp]).

n([mass(vereenzaming)],de,[]).

n([mass(vereenzelviging)],de,[]).

n([sg(vereffenaar),pl(vereffenaars)],de,[]).

n([pl(vereisten),sg(vereiste)],both,[sbar,vp,subject_sbar,subject_vp]).

n([pl(verenigingen),sg(vereniging)],de,
  [np_app_measure],
  [s(bedrijf),
   i(belang,belangen),
   branche,
   s(carnaval),
   i(journalist,journalisten),
   kruis,
   kunst,
   i(omni_sport,omnisport),
   omroep,
   s(ondernemer),
   sport,
   i(student,studenten),
   studie,
   s(supporter),
   tafeltennis,
   vak,
   voetbal,
   volleybal,
   s(werkgever),
   woningbouw]).

n([pl(vereringen),sg(verering)],de,[]).

n([pl(verven),sg(verf)],de,[pred_pp(in)]).

n([pl(verven),sg(verf)],de,[],
  [olie,
   water]).

n([mass(verve)],both,[]).

n([pl(verfijningen),sg(verfijning)],de,[sbar,vp]).

n([pl(verfilmingen),sg(verfilming)],de,[]).

n([pl(verflagen),sg(verflaag)],de,[]).

n([pl(vergaderingen),sg(vergadering)],de,
  [temp_mod,
   app_measure],
  [s(aandeelhouder),
   s(beleid),
   s(bestuur),
   commissie,
   fractie,
   jaar,
   s(kabinet),
   i(lid,leden),
   s(personeel),
   s(raad),
   i(staat,staten),
   dim(vergaderingetje)]).

n([pl(vergaderzalen),sg(vergaderzaal)],de,[]).

n([mass(vergankelijkheid)],de,[]).

n([pl(vergeldingen),sg(vergelding)],de,[]).

n([pl(vergelijken),sg(vergelijk)],het,[]).

n([pl(vergelijkingen),sg(vergelijking)],de,[sbar,vp],[dim(vergelijkinkje)]).

n([mass(vergetelheid)],de,[]).

n([mass(vergeving)],de,[]).

n([pl(vergezichten),sg(vergezicht)],het,[]).

n([pl(vergieten),sg(vergiet)],het,[]).

n([mass(vergif)],het,[]).

n([mass(vergiffenis)],de,[]).

n([pl(vergiften),sg(vergift)],het,[sbar,vp]).

n([pl(vergiftigingen),sg(vergiftiging)],de,[],
  [rook,
   voedsel,
   s(zwangerschap)]).

n([pl(vergissingen),sg(vergissing)],de,[vp, subject_vp, sbar, subject_sbar],
  [dim(vergissinkje)]).

n([pl(vergoedingen),sg(vergoeding)],de,
  [sbar,vp]).

n([pl(vergoedingen),sg(vergoeding)],de,[],
  [i(kilo_meter,kilometer),
   onkosten,
   ontslag,
   rente,
   schade,
   uur
  ]).

n([sg(vergrendeling),pl(vergrendelingen)],de,[]).

n([pl(vergrijpen),sg(vergrijp)],het,[sbar,vp]).

n([mass(vergrijzing)],de,[]).

n([pl(vergrootglazen),sg(vergrootglas)],het,[]).

n([pl(vergrotingen),sg(vergroting)],de,[],
  [borst,
   prostaat,
   schaal]).

n([pl(vergunningen),sg(vergunning)],de,[vp],[]).

n([pl(vergunningen),sg(vergunning)],de,[],
  [bouw,
   export,
   s(gebruik),
   s(handel),
   milieu,
   s(verblijf),
   werk]).

n([pl(verhalen),sg(verhaal)],het,
  [sbar,
   van_sbar,
   subject_sbar   % of hij komt is een ander verhaal
  ],
  [dim(verhaaltje)]).

n([pl(verhalen),sg(verhaal)],het,
  [start_app_measure,
   np_app_measure,
   app_measure
  ],
  [beeld,
   bijbel,
   kerst,
   s(liefde),
   nieuws,
   s(volk),
   dim(verhaaltje)]).

n([pl(verhalen),sg(verhaal)],het,
  [],
  [achtergrond,
   succes,
   verkoop,
   dim(verhaaltje)]).

n([pl(verhandelingen),sg(verhandeling)],de,[sbar,vp]).

n([pl(verhardingen),sg(verharding)],de,[]).

n([sg(verhang)],het,[]).

n([mass(verheerlijking)],de,[],[zelf]).

n([pl(verheffingen),sg(verheffing)],de,[]).

n([mass(verheldering)],de,[]).

n([pl(verhemelten),pl(verhemeltes),sg(verhemelte)],het,[]).

n([mass(verhevene),pl(verhevenen)],both,[]).

n([sg(verhevenheid),pl(verhevenheden)],de,[]).

n([sg(verhindering),pl(verhinderingen)],de,[]).

n([mass(verhitting)],de,[]).

n([pl(verhogingen),sg(verhoging)],de,[],
  [accijns,
   belasting,
   huur,
   kapitaal, s(kapitaal),
   loon, s(loon),
   prijs,
   productie,
   rente,
   salaris]).

n([sg(verhoog),pl(verhogen)],het,[]).

n([pl(verhoren),sg(verhoor)],het,[],
  [i(getuige,getuigen),
   kruis]).

n([pl(verhoudingen),sg(verhouding)],de,[measure,pred_pp(in)]).

n([pl(verhoudingen),sg(verhouding)],de,[],
  [s(arbeid),
   concurrentie,
   s(getal),
   s(kracht),
   s(macht),
   s(recht),
   rente]).

n([pl(['haat-liefde',verhouding]),sg(['haat-liefde',verhouding])],de,[]).

n([mass(verhuis)],de,[]).

n([sg(verhuizer),pl(verhuizers)],de,[]).

n([pl(verhuizingen),sg(verhuizing)],de,[]).

n([mass(verhuur)],de,[],[auto,film]).

n([pl(verhuurders),sg(verhuurder)],de,[],[auto]).

n([pl(verificaties),sg(verificatie)],de,[]).

n([pl(verifikaties),sg(verifikatie)],de,[]).

n([mass(vering)],de,[]).

n([pl(verjongingen),sg(verjonging)],de,[]).

n([mass(verkeer)],het,[],
  [s(betaling),
   data,
   i(goed,goederen),
   s(handel),
   spoor,
   trein,
   weg,
   [woon,'/',werk],
   i(woon_werk,'woon/werk'),
   i(woon_werk,'woon-werk')]).

n([mass(verkaveling)],de,[]).

n([pl(verkenners),sg(verkenner)],de,[]).

n([pl(verkenningen),sg(verkenning)],de,[]).

n([pl(verkeringen),sg(verkering)],de,[]).

n([pl(verkiezingen),sg(verkiezing)],de,
  [sbar,vp],
  [uit]).

n([pl(verkiezingen),sg(verkiezing)],de,
  [temp_mod],
  [i(gemeente_raad,gemeenteraads),
   her,
   kamer,i(kamer,'Kamer'),
   s(parlement),
   s(president),
   s(raad),
   i(staat,staten),i(staat,'Staten'),
   voor,
   'Tweede-Kamer',['Tweede','Kamer'],
   s(waterschap)
  ]).

n([pl(verkiezingsbijeenkomsten),sg(verkiezingsbijeenkomst)],de,[]).

n([pl(verkiezingscampagnes),sg(verkiezingscampagne)],de,[]).

n([pl(verkiezingsnederlagen),sg(verkiezingsnederlaag)],de,[]).

n([sg(verkiezingsstrijd),pl(verkiezingsstrijden)],de,[]).

n([pl(verkiezingsuitslagen),sg(verkiezingsuitslag)],de,[]).

n([pl(verklaringen),sg(verklaring)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp],
  [s(detachering),
   i(getuige,getuigen),
   intentie,
   pers,
   slot,
   stem,
   urgent,
   urgentie
  ]).

n([pl(verklaringen),sg(verklaring)],de,[],
  [s(oorlog),
   heilig]).

n([pl(verklaringsmodellen),sg(verklaringsmodel)],het,[]).

n([pl(verkleiningen),sg(verkleining)],de,[],
    [i(klas,klassen)]).

n([pl(verklikkers),sg(verklikker)],de,[],[dim(verklikkertje)]).

n([sg(verkoeling),pl(verkoelingen)],de,[]).

n([sg(verkondiger),pl(verkondigers)],de,[]).

n([pl(verkondigingen),sg(verkondiging)],de,[sbar,vp]).

n([pl(verkopen),sg(verkoop)],de,[],
  [auto,
   i(detail_handel,detailhandel),
   i(detail_handel,detailhandels),
   kaart,
   koppel,
   wapen,
   winkel
  ]).

n([pl(verkooppunten),sg(verkooppunt)],het,[]).

n([pl(verkoopsters),sg(verkoopster)],de,[]).

n([pl(verkopers),sg(verkoper)],de,[]).

n([pl(verkortingen),sg(verkorting)],de,[sbar,vp],
  [i(arbeid_duur,arbeidsduur),
   i(arbeid_tijd,arbeidstijd),
   i(werk_tijd,werktijd)
  ]).

n([pl(verkoudheden),sg(verkoudheid)],de,[]).

n([sg(verkrachter),pl(verkrachters)],de,[],[serie]).

n([pl(verkrachtingen),sg(verkrachting)],de,[]).

n([mass(verkrijging)],de,[]).

n([pl(verkwistingen),sg(verkwisting)],de,[sbar,vp]).

n([sg(verlader),pl(verladers)],de,[]).

n([sg(verlager),pl(verlagers)],de,[],[cholesterol]).

n([pl(verlagingen),sg(verlaging)],de,[sbar,vp]).

n([pl(verlagingen),sg(verlaging)],de,[],
  [belasting,
   i(last,lasten),
   prijs,
   rente]).

n([pl(verlammingen),sg(verlamming)],de,[]).

n([pl(verlangens),sg(verlangen)],het,[vp,sbar]).

n([mass(verlatenheid)],de,[]).

n([pl(verledens),sg(verleden)],het,[],
  [s(arbeid),
   s(oorlog),
   slavernij
  ]).

n([mass(verledene)],het,[],[]).  % ouderwets

n([mass(verlegenheid)],de,[pred_pp(in)]).

n([pl(verleiders),sg(verleider)],de,[]).

n([pl(verleidsters),sg(verleidster)],de,[]).

n([pl(verleidingen),sg(verleiding)],de,[vp]).

n([sg(verlener),pl(verleners)],de,[],
  [dienst,
   hulp,
   i(jeugd_hulp,jeugdhulp),
   krediet,
   steun,
   zorg]).

n([sg(verleenster),pl(verleensters)],de,[],
  [dienst,
   hulp,
   i(jeugd_hulp,jeugdhulp),
   krediet,
   steun,
   zorg]).

% n([mass(verlengde)],het,[]).

n([pl(verlengingen),sg(verlenging)],de,[],
  [contract]).

n([pl(verlengstukken),sg(verlengstuk)],het,[]).

n([sg(verlening),pl(verleningen)],de,[],
  [dienst,
   hulp,
   i(jeugd_hulp,jeugdhulp),
   krediet,
   steun,
   zorg]).

n([sg(verlichter),pl(verlichters)],de,[]).

n([pl(verlichtingen),sg(verlichting)],de,[sbar,vp]).

n([pl(verlichtingen),sg(verlichting)],de,[],
  [feest,
   kerst,
   i(last,lasten)
  ]).

n([pl(verliefdheden),sg(verliefdheid)],de,[]).

n([pl(verliezen),sg(verlies)],het,[sbar,vp]).

n([pl(verliezen),sg(verlies)],het,[],
  [aanloop,
   i(baan,banen),
   bal,
   bloed,
   bruto,
   geheugen,
   s(gewicht),
   s(gezicht),
   koers,
   i(koop_kracht,koopkracht),
   i(koop_kracht,koopkrachts),
   netto,
   omzet,
   punt,i(punt,punten),
   record,
   tijd,
   warmte
  ]).

n([pl(verliezers),sg(verliezer)],de,[]).

n([mass(verloedering)],de,[]).

n([pl(verloven),sg(verlof)],het,[vp,pred_pp(met)],
  [s(ouderschap),
   proef,
   s(vaderschap),
   ziekte,
   s(zwangerschap)]).

n([pl(verlokkingen),sg(verlokking)],de,[vp]).

n([sg(verloofde),pl(verloofden)],de,[]).

n([sg(verloop)],both,[]).

n([mass(verloop)],het,[],
  [koers,
   score,
   s(tijd)]).

n([sg(verloskundige),pl(verloskundigen)],de,[]).

n([pl(verlossers),sg(verlosser)],de,[]).

n([pl(verlossingen),sg(verlossing)],de,[sbar,vp]).

n([pl(verlovingen),sg(verloving)],de,[]).

n([mass(vermaan)],het,[]).

n([pl(vermaken),sg(vermaak)],het,[sbar,vp,pred_pp(ter)]).

n([mass(vermaatschappelijking)],de,[]).

n([pl(vermaningen),sg(vermaning)],de,[sbar,vp]).

n([pl(vermeerderingen),sg(vermeerdering)],de,[]).

n([pl(vermeldingen),sg(vermelding)],de,
  [app_measure,
   np_app_measure,
   start_app_measure,
   sbar]).

n([pl(vermeldingen),sg(vermelding)],de,
  [],
  [bron]).

n([pl(vermengingen),sg(vermenging)],de,[]).

n([pl(vermenigvuldigingen),sg(vermenigvuldiging)],de,[]).

n([mass(vermijding)],de,[]).

n([pl(verminderingen),sg(vermindering)],de,[sbar,vp]).

n([pl(verminkingen),sg(verminking)],de,[sbar,vp]).

n([sg(vermissing),pl(vermissingen)],de,[]).

n([pl(vermoedens),sg(vermoeden)],het,[sbar,subject_sbar]).

n([mass(vermoeidheid)],de,[]).

n([pl(vermoeienissen),sg(vermoeienis)],de,[]).

n([pl(vermogens),mass(vermogen)],het,[sbar,vp],
  [on]).

n([pl(vermogens),mass(vermogen)],het,[],
  [s(aanpassing),
   concurrentie,
   denk,
   s(doorzetting),
   s(gezicht),
   s(incassering),
   s(inleving),
   s(onderscheiding),
   s(relativering),
   s(uithouding),
   s(voorstelling),
   s(waarneming)]).

n([pl(vermommingen),sg(vermomming)],de,[sbar,vp]).

n([sg(vernauwing),pl(vernauwingen)],de,[]).

n([pl(vernederingen),sg(vernedering)],de,[]).

n([pl(vernielingen),sg(vernieling)],de,[]).

n([pl(vernietigingen),sg(vernietiging)],de,[],
  [waarde]).

n([pl(vernieuwers),sg(vernieuwer)],de,[]).

n([pl(vernieuwingen),sg(vernieuwing)],de,[sbar,vp]).

n([pl(vernieuwingen),sg(vernieuwing)],de,[],[s(stad)]).

n([pl(vernissen),sg(vernis)],both,[sbar,vp]).  % celex:het???

n([pl(vernuften),sg(vernuft)],het,[vp]).

n([pl(veronderstellingen),sg(veronderstelling)],de,[sbar,subject_sbar,vp]).

n([pl(verontreinigingen),sg(verontreiniging)],de,[]).

n([pl(verontrustingen),sg(verontrusting)],de,[]).

n([pl(verontschuldigingen),sg(verontschuldiging)],de,[sbar,vp]).

n([mass(verontwaardiging)],de,[]).

n([pl(veroordelingen),sg(veroordeling)],de,[sbar,vp]).

n([pl(veroorzakers),sg(veroorzaker)],de,[]).

n([pl(verordeningen),sg(verordening)],de,[sbar,vp]).

n([mass(veroudering)],de,[]).

n([pl(veroveraars),sg(veroveraar)],de,[]).

n([pl(veroveringen),sg(verovering)],de,[]).

n([pl(verpakkingen),sg(verpakking)],de,[]).

n([mass(verpaupering)],de,[]).

n([pl(verpersoonlijkingen),sg(verpersoonlijking)],de,[]).

n([pl(verplaatsingen),sg(verplaatsing)],de,[],[s(bedrijf)]).

n([mass(verpleegkunde)],de,[]).

n([sg(verpleegkundige),pl(verpleegkundigen)],de,[],[wijk]).

n([pl(verpleegsters),sg(verpleegster)],de,[],[wijk,dim(verpleegstertje)]).

n([sg(verpleegtehuis),pl(verpleegtehuizen)],het,[]).

n([pl(verplegers),sg(verpleger)],de,[]).

n([mass(verpleging)],de,[]).

n([pl(verplichtingen),sg(verplichting)],de,[sbar,vp]).

n([pl(verplichtingen),sg(verplichting)],de,[],
  [interland,
   pension]).

n([mass(verraad)],het,[subject_vp,subject_sbar],[hoog]).

n([pl(verraders),sg(verrader)],de,[]).

n([pl(verrassingen),sg(verrassing)],de,[subject_sbar]).

n([pl(verrekeningen),sg(verrekening)],de,[]).

n([pl(verrekijkers),sg(verrekijker)],de,[]).

n([sg(verrekking),pl(verrekkingen)],de,[]).

n([pl(verrichtingen),sg(verrichting)],de,[]).

n([mass(verrijking)],de,[],
  [zelf]).

n([mass(verrijzenis)],de,[]).

n([mass(verrotting)],de,[]).

n([mass(verruiming)],de,[]).

n([pl(verrukkingen),sg(verrukking)],de,[sbar,vp]).

n([pl(verzen),sg(vers)],het,[],[dim(versje)]).

n([sg([vers,twee])],het,[subject_sbar]).

n([sg(verschaffer),pl(verschaffers)],de,[]).

n([pl(verschaffingen),sg(verschaffing)],de,[]).

n([pl(verschansingen),sg(verschansing)],de,[]).

n([mass(verscheiden)],het,[]).

n([pl(verscheidenheden),sg(verscheidenheid)],de,[]).

n([pl(verscherpingen),sg(verscherping)],de,[]).

n([pl(verschieten),sg(verschiet)],het,[]).

n([pl(verschijningen),sg(verschijning)],de,[]).

n([pl(verschijningsvormen),sg(verschijningsvorm)],de,[]).

n([pl(verschijnselen),pl(verschijnsels),sg(verschijnsel)],het,
  [sbar,vp,app_measure],
  [neven] % en niet neef_verschijnsel 
 ).

n([pl(verschijnselen),pl(verschijnsels),sg(verschijnsel)],het,[],
  [s(ontwenning),
   s(onthouding)  
  ]).

n([pl(verschillen),sg(verschil)],het,[sbar,subject_sbar,vp]).

n([pl(verschillen),sg(verschil)],het,[],
  [cultuur,
   hoogte,
   s(inkomen),
   s(leeftijd),
   sekse]).

n([sg(verschraling),pl(verschralingen)],de,[subject_sbar]).

n([pl(verschrikkingen),sg(verschrikking)],de,[sbar,vp]).

n([pl(verschuivingen),sg(verschuiving)],de,[],
  [dijk]).

n([pl(versies),sg(versie)],de,
  [subject_sbar],
  [oer]).

n([pl(versieringen),sg(versiering)],de,[]).

n([pl(versierselen),pl(versiersels),sg(versiersel)],het,[],
  [dim(versierseltje)]).

n([pl(verslagen),sg(verslag)],het,[sbar]).

n([pl(verslagen),sg(verslag)],het,[],
  [audit,
   boek,
   initiatief,
   reis]).

n([mass(verslagenheid)],de,[]).

n([pl(verslaggeefsters),sg(verslaggeefster)],de,[],
  [misdaad,
   radio,
   h(tv),televisie,tv,f([tv]),i(tv,'TV-'),
   sport]).

n([pl(verslaggevers),sg(verslaggever)],de,[],
  [misdaad,
   radio,
   h(tv),televisie,tv,f([tv]),i(tv,'TV-'),
   sport]).

n([mass(verslaggeving)],de,[]).

% niet vers_lap_pers
n([sg(verslapper),pl(verslappers)],de,[],[spier]).

n([sg(verslagweek),pl(verslagweken)],de,[]).

n([mass(verslaving),pl(verslavingen),sg(verslaving)],de,[],
  [alcohol,
   drug,s(drug),
   gok]).

n([mass(verslechtering),pl(verslechteringen),sg(verslechtering)],de,[]).

n([pl(versmeltingen),sg(versmelting)],de,[]).

n([pl(versnaperingen),sg(versnapering)],de,[]).

n([pl(versnellingen),sg(versnelling)],de,[]).

n([sg(versnipperaar),pl(versnipperaars)],de,[]).

n([pl(versnipperingen),sg(versnippering)],de,[]).

n([sg(versobering),pl(versoberingen)],de,[]).

n([sg(versoepeling),pl(versoepelingen)],de,[]).

n([pl(versperringen),sg(versperring)],de,[],[weg]).

n([pl(verspillingen),sg(verspilling)],de,
  [sbar,
   vp],
  [tijd]).

n([sg(versplintering),pl(versplinteringen)],de,[]).

n([pl(verspreiders),sg(verspreider)],de,[]).

n([mass(verspreiding)],de,[],[diepte]).

n([sg(verspringer),pl(verspringers)],de,[]).

n([sg(verspringster),pl(verspringsters)],de,[]).

n([pl(versregels),sg(versregel)],de,[]).

n([pl(verstanden),sg(verstand)],het,[vp]).

n([mass(verstandhouding)],de,[]).

n([mass(verstarring)],de,[]).

n([mass(verstedelijking)],de,[]).

n([sg(verstekeling),pl(verstekelingen)],de,[]).

n([pl(verstekken),sg(verstek)],het,[]).

n([sg(verstelling),pl(verstellingen)],de,[],
  [hoogte]).

n([mass(versterf)],both,[]).

n([pl(versterkers),sg(versterker)],de,[]).

n([pl(versterkingen),sg(versterking)],de,[],
  [dijk]).

n([pl(verstoorders),sg(verstoorder)],de,[],[orde]).

n([pl(verstoppingen),sg(verstopping)],de,[],
  [dim(verstoppinkje)]).

n([pl(verstoringen),sg(verstoring)],de,[],
  [orde]).

n([sg(verstrekker),pl(verstrekkers)],de,[],
  [geld,
   informatie, % en niet informatieve-strek-kers
   krediet]). % en niet krediet_verstrek_kers

n([pl(verstrekkingen),sg(verstrekking)],de,[],
  [geld,
   informatie,
   krediet
  ]).

n([mass(verstrengeling)],de,[],
  [i(belang,belangen)]).

n([pl(verstrooidheden),sg(verstrooidheid)],de,[]).

n([pl(verstrooiingen),sg(verstrooiing)],de,[]).

n([sg(vertaalster),pl(vertaalsters)],de,[]).

n([pl(vertakkingen),sg(vertakking)],de,[]).

n([pl(vertalers),sg(vertaler)],de,[app_measure]).

n([pl(vertalingen),sg(vertaling)],de,[],[bijbel]).

n([pl(verten),pl(vertes),sg(verte)],de,[]).

n([pl(vertederingen),sg(vertedering)],de,[sbar,vp]).

n([pl(vertegenwoordigers),sg(vertegenwoordiger)],de,[],
  [s(regering)]).

n([pl(vertegenwoordigingen),sg(vertegenwoordiging)],de,[]).

n([pl(vertegenwoordigsters),sg(vertegenwoordigster)],de,[]).

n([mass(vertelinstantie)],de,[]).

n([pl(vertellers),sg(verteller)],de,[]).

n([pl(vertellingen),sg(vertelling)],de,[sbar],[dim(vertellinkje)]).

n([pl(vertelsters),sg(vertelster)],de,[]).

n([pl(verteringen),sg(vertering)],de,[]).

n([mass(vertier)],het,[]).

n([pl(vertolkers),sg(vertolker)],de,[]).

n([pl(vertolkingen),sg(vertolking)],de,[]).

n([pl(vertolksters),sg(vertolkster)],de,[]).

n([pl(vertoningen),sg(vertoning)],de,[],[dim(vertoninkje)]).

n([pl(vertogen),sg(vertoog)],het,[]).

n([mass(vertoon)],het,[],
  [s(macht)]).

n([pl(vertragingen),sg(vertraging)],de,[],
  [groei]).

n([pl(vertrekken),sg(vertrek)],het,[],
  [slaap]).

n([sg(vertrekker),pl(vertrekkers)],de,[]).

n([mass(vertrouwdheid)],de,[]).

n([pl(vertrouwelijkheden),sg(vertrouwelijkheid)],de,[]).

n([pl(vertrouwelingen),sg(vertrouweling)],de,[]).

n([mass(vertrouwen)],het,[sbar]).

n([mass(vertrouwen)],het,[],
  [i(consument,consumenten),
   i(producent,producenten)]).

n([pl(vertrouwenslieden),pl(vertrouwensmannen),sg(vertrouwensman)],de,[]).

n([mass(vertwijfeling)],de,[]).

n([mass(vervaardiging)],de,[]).

n([pl(vervallen),sg(verval)],het,[pred_pp(in)]).

n([sg(vervalser),pl(vervalsers)],de,[]).

n([pl(vervalsingen),sg(vervalsing)],de,[],
  [concurrentie]).

n([pl(vervangers),sg(vervanger)],de,[],
  [lood,
   plaats,
   vlees]).

n([pl(vervangsters),sg(vervangster)],de,[],
  [plaats]).

n([pl(vervangingen),sg(vervanging)],de,[]).

n([mass(verveling)],de,[]).

n([sg(verver),pl(ververs)],de,[]).   

n([pl(vervloekingen),sg(vervloeking)],de,[sbar,vp]).

n([mass(vervoer)],het,[],
  [bus,
   i(goed,goederen),
   i(leerling,leerlingen),
   lucht,
   mest,
   i(persoon,personen),
   s(passagier),
   spoorweg,
   trein,
   zee
  ]).

n([sg(vervoerder),pl(vervoerders)],de,[],[mest]).

n([pl(vervoeringen),sg(vervoering)],de,[]).

n([pl(vervolgen),sg(vervolg)],het,[]).

n([pl(vervolgers),sg(vervolger)],de,[]).

n([pl(vervolgingen),sg(vervolging)],de,[],[s(recht)]).

n([pl(vervormingen),sg(vervorming)],de,[]).

n([pl(vervreemdingen),sg(vervreemding)],de,[]).

n([sg(vervuiler),pl(vervuilers)],de,[]).

n([mass(vervuiling)],de,[],[milieu]).

n([mass(vervulling)],de,[pred_pp(in)]).

n([mass(verwaarlozing)],de,[]).

n([pl(verwachtingen),sg(verwachting)],de,
  [pred_pp(in)],
  []).

n([pl(verwachtingen),sg(verwachting)],de,
  [sbar,
   vp,
   subject_sbar_no_het],
  [toekomst]).

n([pl(verwachtingen),sg(verwachting)],de,
  [],
  [winst]).

n([pl(verwachtingspatronen),sg(verwachtingspatroon)],het,[sbar,vp]).

n([pl(verwanten),sg(verwante),sg(verwant)],de,[]).

n([mass(verwantschap)],de,[]).

n([pl(verwarmingen),sg(verwarming)],de,[]).

n([pl(verwarringen),sg(verwarring)],de,[pred_pp(in)]).

n([sg(verweerder),pl(verweerders)],de,[]).

n([pl(verweren),sg(verweer)],het,[sbar,vp,subject_sbar]).

n([pl(verwekkers),sg(verwekker)],de,[]).

n([pl(verwensingen),sg(verwensing)],de,
  [start_app_measure,
   app_measure,
   np_app_measure,
   sbar,
   vp]).

n([mass(verwerkelijking)],de,[]).

n([mass(verwerking)],de,[]).

n([mass(verwerping)],de,[]).

n([mass(verwerving)],de,[]).

n([pl(verwezenlijkingen),sg(verwezenlijking)],de,[]).

n([pl(verwijderingen),sg(verwijdering)],de,[]).

n([sg(verwijding),pl(verwijdingen)],de,[]).

n([pl(verwijten),sg(verwijt)],het,[sbar,subject_sbar,vp]).

n([pl(verwijzingen),sg(verwijzing)],de,[sbar,vp]).

n([pl(verwijzingen),sg(verwijzing)],de,[],[terug]).

n([pl(verwikkelingen),sg(verwikkeling)],de,[sbar,vp]).

n([pl(verwoestingen),sg(verwoesting)],de,[]).

n([mass(verwondering)],de,[]).

n([pl(verwondingen),sg(verwonding)],de,[]).

n([pl(verwoordingen),sg(verwoording)],de,[]).

n([pl(verworvenheden),sg(verworvenheid)],de,[sbar,vp,
                                             subject_sbar,subject_vp]).

n([pl(verzachtingen),sg(verzachting)],de,[sbar,vp]).

n([sg(verzadiging),pl(verzadigingen)],de,[]).

n([sg(verzakking),pl(verzakkingen)],de,[]).

n([pl(verzamelaars),sg(verzamelaar)],de,[],
  [kunst]).

n([pl(verzamelaarsters),sg(verzamelaarster)],de,[],
  [kunst]).

n([pl(verzamelingen),sg(verzameling)],de,[measure],[dim(verzamelingetje)]).

n([pl(verzamelnamen),sg(verzamelnaam)],de,[]).

n([pl(verzekeraars),sg(verzekeraar)],de,[],
  [ s(leven),
    i(ziekte_kost,ziektekosten) ]).

n([pl(verzekeringen),sg(verzekering)],de,
  [sbar,
   vp]).

n([pl(verzekeringen),sg(verzekering)],de,[],
  [s(annulering),
   auto,
   basis,
   her,
   kapitaal,
   s(leven),
   reis,
   i(senior,senioren),
   schade,
   studie,
   s(volk),
   s(werknemer),
   ziekenfonds,
   ziekte,
   i(ziekte_kost,ziektekosten),
   zorg]).

n([mass(verzelfstandiging)],de,[]).

n([pl(verzendingen),sg(verzending)],de,[]).

n([mass(verzet)],het,[]).

n([pl(verzetsstrijders),sg(verzetsstrijder)],de,[]).

n([pl(verzinselen),pl(verzinsels),sg(verzinsel)],het,[sbar,vp],
  [dim(verzinseltje)]).

n([pl(verzoeken),sg(verzoek)],het,
  [vp,sbar]).

n([pl(verzoeken),sg(verzoek)],het,[],
  [asiel,
   gratie,
   terugbel,
   s(uitlevering)]).

n([pl(verzoekers),sg(verzoeker)],de,[]).

n([pl(verzoeksters),sg(verzoekster)],de,[]).

n([pl(verzoekingen),sg(verzoeking)],de,[sbar,vp]).

n([pl(verzoekschriften),sg(verzoekschrift)],het,[]).

n([pl(verzoeningen),sg(verzoening)],de,[]).

n([pl(verzorgers),sg(verzorger)],de,[],
  []).

n([mass(verzorging)],de,[]).

n([pl(verzorgsters),sg(verzorgster)],de,[]).

n([pl(verzuchtingen),sg(verzuchting)],de,[sbar]).

n([mass(verzuiling)],de,[]).

n([pl(verzuimen),sg(verzuim)],het,[sbar,subject_sbar,subject_vp,vp]).

n([mass(verzuring)],de,[]).

n([pl(verzwakkingen),sg(verzwakking)],de,[]).

n([pl(verzwaringen),sg(verzwaring)],de,[],
  [dijk,
   i(last,lasten)]).

n([pl(vespers),sg(vesper)],de,[]).

n([pl(vesten),sg(vest)],het,[],[spring]).

n([pl(vestibules),sg(vestibule)],de,[]).

n([pl(vestigingen),sg(vestiging)],de,[]).

n([pl(vestingen),sg(vesting)],de,[]).

n([pl(vetten),sg(vet)],het,[],
  [buik,
   frituur,
   kaars,
   s(lichaam)]).

n([pl(veten),pl(vetes),sg(vete)],de,[]).

n([pl(veters),sg(veter)],de,[],[dim(vetertje)]).

n([pl(veteranen),sg(veteraan)],de,[],[s(oorlog)]).

n([sg(veterinair),pl(veterinairs)],de,[]).

n([pl('veto\'s'),sg(veto)],het,[]).

n([sg(vetpot)],de,[]).

n([pl(vetzuren),sg(vetzuur)],het,[]).

n([pl(veulens),sg(veulen)],het,[],[dim(veulentje)]).

n([pl(vezels),sg(vezel)],de,[],[dim(vezeltje)]).

n([sg([via,ferrata])],de,[]).  % term uit alpinisme, met staalkabels uitgezet parcours langs een rotswand

n([pl(viaducten),sg(viaduct)],both,[]).

n([pl(viadukten),sg(viadukt)],both,[]).

n([pl(vibraties),sg(vibratie)],de,[]).

n([sg(vibrator),pl(vibrators)],de,[]).

n([sg(victorie)],de,[]).

n([sg(vide),pl(vides)],de,[]).

n([pl('video\'s'),sg(video)],de,[],[nieuws]).

n([mass([video,on,demand])],both,[]).

n([pl(videobanden),sg(videoband)],de,[]).

n([sg(videoloog),pl(videologen)],de,[]).

n([sg(videorecorder),pl(videorecorders)],de,[]).

n([sg(videotheek),pl(videotheken)],de,[]).

n([bare_meas(viermaal),pl(viermalen)],both,[temp_mod,measure,sbar]).

n([pl(vieren),sg(vier)],de,[],[dim(viertje)]).

n([pl(vierden),pl(vierdes)],both,[]).

n([pl(vieringen),sg(viering)],de,[],[kerst]).

n([pl(vierkanten),sg(vierkant)],het,[]).

n([pl(viervoeters),sg(viervoeter)],de,[]).

n([mass(vieux)],de,[]).

n([pl(viezeriken),sg(viezerik)],de,[]).

n([sg(vignet),pl(vignetten)],het,[],
  [euro,
   spits,
   tol,
   i(weg,wegen)]).

n([pl(vijanden),sg(vijand)],de,[],[aarts]).

n([pl(vijandelijkheden)],de,[]).

n([pl(vijandigheden),sg(vijandigheid)],de,[sbar,vp]).

n([pl(vijandschappen),sg(vijandschap)],de,[]).

n([bare_meas(vijfmaal),pl(vijfmalen)],both,[temp_mod,measure,sbar]).

n([pl(vijven),sg(vijf)],de,[],[dim(vijfje)]).

n([sg(vijfbak),pl(vijfbakken)],de,[]).

n([pl(vijfden),pl(vijfdes)],de,[]).

n([pl(vijftigers),sg(vijftiger)],de,[]).

n([pl(vijgen),sg(vijg)],de,[]).

n([sg(vijl),pl(vijlen)],de,[]).

n([pl(vijvers),sg(vijver)],de,[],
  [kano,
   kweek,
   dim(vijvertje)]).

n([sg(vijzel),pl(vijzels)],de,[]).

n([pl(vikingen),pl(vikings),sg(viking)],de,[]).

n([mass(vilein)],het,[]).

%% villa de Eikenhorst
n([pl('villa\'s'),sg(villa)],de,[np_app_measure],[dim(villaatje)]).

n([mass(vilt)],het,[],[dim(viltje)]).

n([pl(viltstiften),sg(viltstift)],de,[]).

n([pl(vinnen),sg(vin)],de,[]).

n([pl(vindingen),sg(vinding)],de,[sbar,vp]).

n([pl(vindingen),sg(vinding)],de,[],[s(waarheid)]).

n([mass(vindingrijkheid)],de,[]).

n([pl(vingeren),pl(vingers),sg(vinger)],de,[measure],[dim(vingertje)]).

n([pl(vingerafdrukken),sg(vingerafdruk)],de,[]).

n([pl(vingerwijzingen),sg(vingerwijzing)],de,[sbar,vp]).

n([mass(vinificatie),
   mass(vinifikatie)],de,[]).

n([pl(vinken),sg(vink)],de,[],[dim(vinkje)]).

n([pl(vintages),sg(vintage)],de,[]).

n([mass(vinyl)],het,[]).

n([sg([viola,da,gamba])],de,[]). % muziekinstrument

n([pl(violets),sg(violet)],het,[]).

n([pl(violisten),sg(violist)],de,[]).

n([pl(violistes),sg(violiste)],de,[]).

n([pl(violen),sg(viool)],de,[],
  [alt,
   dim(viooltje)]).

n([pl(vioolconcerten),sg(vioolconcert)],het,[]).

n([sg(viroloog),pl(virologen)],de,[]).

n([mass([virtual,reality])],de,[]).

n([sg(virtuoos),pl(virtuozen)],de,[]).

n([mass(virtuositeit)],de,[]).

n([pl(virussen),sg(virus)],both,[], % VL: de
  [aids,
   computer,
   ['mond-',en,klauwzeer],mkz,
   'Ebola',f(['Ebola']),h('Ebola'),
   griep,
   i(vogel_griep,vogelgriep),
   i(vogel_pest,vogelpest)]).

n([pl(vissen),sg(vis)],de,[],
  [korenaar,
   koornaar,
   lancet,
   schel,
   telescoop,
   vin,
   i(zoet_water,zoetwater),
   i(zout_water,zoutwater),
   dim(visje)]).

n([pl(visies),sg(visie)],de,[sbar,vp],
  [toekomst]).

n([pl(visioenen),sg(visioen)],het,[sbar,vp]).

n([sg(visionair),pl(visionairs)],de,[]).

n([pl(visites),sg(visite)],de,[]).

n([pl(visitekaartjes),sg(visitekaartje)],het,[]).

n([pl(vissers),sg(visser)],de,[],[kokkel]).

n([pl(visserijen),sg(visserij)],de,[],
  [kokkel,
   zee]).

n([pl(vissersboten),sg(vissersboot)],de,[]).

n([pl(visa),pl(visums),sg(visum)],het,[]).

n([pl(visvangsten),sg(visvangst)],de,[]).

n([mass(vitaliteit)],de,[]).

n([pl(vitaminen),pl(vitamines),sg(vitamine)],both,[]).

n([pl(vitrages),sg(vitrage)],de,[]).

n([pl(vitrines),sg(vitrine)],de,[]).

n([pl(vizieren),sg(vizier)],both,[]).

n([pl(vizioenen),sg(vizioen)],het,[sbar,vp]).

n([pl('vj\'s'),sg(vj)],de,[]).

n([pl(vlagen),sg(vlaag)],de,[measure]).

n([pl(vlaggen),sg(vlag)],de,[],
  [dim(vlaggetje),
   dim(vlagje)]).

n([pl(vlakken),sg(vlak)],het,[],
  [s(helling),
   dim(vlakje)]).

n([pl(vlakten),pl(vlaktes),sg(vlakte)],de,[]).

n([pl(vlammen),sg(vlam)],de,[],[dim(vlammetje)]).

n([mass(vla)],de,[]).

n([mass(vlas)],het,[]).

n([pl(vlechten),sg(vlecht)],de,[],
  [dim(vlechtje)]).

n([pl(vleermuizen),sg(vleermuis)],de,[]).

n([mass(vlees)],het,[],
  [i(kip,kippen),i(kip,kippe),
   s(lam),
   pekel,
   rook,
   rund,s(rund),
   scharrel,
   stoof,
   tand,
   s(varken),
   vrucht]).

n([pl(vleesgerechten),sg(vleesgerecht)],het,[]).

n([pl(vleeswaren)],de,[]).

n([pl(vlegels),sg(vlegel)],de,[],[dim(vlegeltje)]).

n([pl(vlekken),sg(vlek)],both,[measure],
  [i(zon,zonne),
   dim(vlekje)]).

n([pl(vlekken),sg(vlek)],both,[],
  [olie,
   dim(vlekje)]).

n([pl(vlerken),sg(vlerk)],de,[]).

n([pl(vleugelen),pl(vleugels),sg(vleugel)],de,[],[dim(vleugeltje)]).

n([pl(vleugen),sg(vleug)],de,[measure],[dim(vleugje)]).

n([pl(vliegen),sg(vlieg)],de,[],
  [brom,
   s(eendag),
   roof,
   i(slang_poot,slangpoot),
   sluip,
   steen,
   vuur,
   zweef,
   dim(vliegje)]).

n([mass(vliegangst)],de,[]).

n([sg([vliegende,keep]),
   sg([vliegende,kiep])],de,[]).

n([pl(vliegers),sg(vlieger)],de,[]).

n([sg(vliegeraar),pl(vliegeraars)],de,[]).

n([pl(vliegtuigen),sg(vliegtuig)],het,[],
  [s(gevecht),
   h('NAVO'),h('Navo'),'NAVO','Navo',
   s(passagier),
   patrouille,
   spionage,
   transport,
   s(verkeer),
   vracht,
   zweef,
   dim(vliegtuigje)]).

n([sg(vliegtuigbouwer),pl(vliegtuigbouwers)],de,[]).

n([pl(vliegvelden),sg(vliegveld)],het,[]).

n([mass(vliegverkeer)],het,[]).

n([pl(vlieren),sg(vlier)],de,[]).

n([pl(vlieringen),sg(vliering)],de,[]).

n([pl(vliezen),sg(vlies)],het,[],
  [buik,
   hoorn,
   dim(vliesje)]).

n([mass(vlijt)],de,[]).

n([pl(vlinders),sg(vlinder)],de,[app_measure],[]).

n([pl(vlinders),sg(vlinder)],de,[],
  [beer,
   dons,
   s(koning),
   nacht,
   pracht,
   tand,
   wesp,
   dim(vlindertje)
  ]).

n([mass(vlinderslag)],de,[]).

n([sg(vlog),pl(vloggen)],de,[]).

n([pl(vlooien),sg(vlo)],de,[],[dim(vlootje),
			       dim(vlooitje)]).

n([pl(vloeden),sg(vloed)],de,[]).

n([pl(vloedgolven),sg(vloedgolf)],de,[]).

n([mass(vloeipapier)],het,[],[dim(vloeipapiertje)]).

n([mass(vloei)],het,[],[dim(vloeitje)]).

n([pl(vloeistoffen),sg(vloeistof)],de,
  [app_measure],
  [koel,
   rem]).

n([pl(vloeken),sg(vloek)],de,[subject_vp, subject_sbar]).

n([pl(vloeren),sg(vloer)],de,[],
  [beurs,
   keuken,
   laad,
   laminaat,
   speel]).

n([mass(vloerbedekking)],de,[]).

n([pl(vloerkleden),sg(vloerkleed)],het,[]).

n([pl(vlokken),sg(vlok)],de,[measure],[dim(vlokje)]).

n([pl(vloten),sg(vloot)],de,[measure],[dim(vlootje)]).

n([pl(vlotten),sg(vlot)],het,[],[dim(vlotje)]).

n([sg(vlotter),pl(vlotters)],de,[]).

n([pl(vluchten),
   sg(vlucht),
   ignore(m(vlucht,noun(de,count,sg),vlucht)),
   ignore(m(vluchten,noun(de,count,pl),vlucht))
   ],de,
  [measure,
   pred_pp(op)],[]).		% regenwulpen

n([pl(vluchten),sg(vlucht)],de,[temp_mod],
  [f([award]),
   avond,
   belasting,
   binnen,
   charter,
   dag,
   duik,
   glij,
   heen,
   hulp,
   kapitaal,
   lijn,
   nacht,
   oefen,
   over,
   proef,
   ramp,
   retour,
   rond,
   routine,
   ruimte,
   terug,
   test,
   tussen,
   vakantie,
   s(verkenning),
   h('VN'),
   voedsel,
   vogelvlucht,
   vracht]).

n([pl(vluchtelingen),sg(vluchteling)],de,[],
  [boot,
   h('Hutu')]).

%% vooral bij wielrennen
n([sg(vluchter),pl(vluchters)],de,[],
  [mede]).

n([mass(vmbo)],het,[]).

n([mass('vmbo-t')],het,[]).

n([stem('VN-secretaris_generaal'),
   sg('VN-secretaris-generaal'),pl('VN-secretarissen-generaal')],de,[]).

n([sg('VN-militair'),pl('VN-militairen')],de,[]).

n([sg('VN-missie'),pl('VN-missies')],de,[]).

n([sg('VN-soldaat'),pl('VN-soldaten')],de,[]).

n([pl('VN-troepen')],both,[]).

n([sg(vo)],het,[]).  % voortgezet onderwijs...

n([sg(vocaal),pl(vocalen)],de,[]).

n([pl(vocabulaires),sg(vocabulaire)],both,[]).   % celex: het

n([mass(vocht),pl(vochten),sg(vocht)],het,[],[kook]).

n([mass(vochtigheid)],de,[]).

n([pl(vodden),sg(vod)],het,[measure],[dim(vodje)]).

n([pl(vodden),sg(vodde)],de,[]).

n([sg(voeder),pl(voeders)],both,[],
  [dier,
   vee]).

n([mass(voeding),pl(voedingen),sg(voeding)],de,[],
  [baby,
   borst,
   dwang,
   fles,
   sonde]).

n([pl(voedingsbodems),sg(voedingsbodem)],de,[]).

n([mass(voedsel)],het,[]).

n([sg(voedster),pl(voedsters)],de,[]).

n([pl(voegen),sg(voeg)],de,[]).

n([pl(voeren),sg(voer)],het,[]).

n([pl(voeringen),sg(voering)],de,[],
  [oorlog,s(oorlog)]).

n([pl(voerlieden),pl(voerlui),sg(voerman)],de,[]).

n([pl(voertuigen),sg(voertuig)],het,[],
  [motor,
   pantser]).

n([meas(voet)],de,[meas_mod,
                   measure]).

n([pl(voeten),sg(voet)],de,[pred_pp_pl(op), pred_pp(op)]).  % op vrije voeten/op goede voet

n([pl(voeten),sg(voet)],de,[],
  [linker,
   opbrengst,
   rechter,
   rente,
   winst,
   zweet,
   dim(voetje)]).

n([pl(voetbaden),sg(voetbad)],het,[]).

n([pl(voetballen),sg(voetbal)],de,[],[]).

n([mass(voetbal)],het,[],
  [amateur,
   beker,
   f([betaald]),
   club,
   combinatie,
   counter,
   jeugd,
   paniek,
   prof,
   top,
   i(vrouw,vrouwen),
   zaal,
   zaterdag
  ]).

n([pl(voetballers),sg(voetballer)],de,[],
  [degradatie,
   prof,
   top]).

n([pl(voetbalsters),sg(voetbalster)],de,[],
  [prof,
   top]).

n([mass(voetballerij)],de,[]).

n([sg(voetbalsupporter),pl(voetbalsupporters)],de,[]).

n([pl(voetbalvelden),sg(voetbalveld)],het,[]).

n([pl(voeteneinden),sg(voeteneinde),sg(voeteneind)],het,[]).

n([pl(voetgangers),sg(voetganger)],de,[],[dim(voetgangertje)]).

n([pl(voetnoten),sg(voetnoot)],de,[sbar,vp]).

n([pl(voetpaden),sg(voetpad)],het,[]).

n([pl(voetsporen),sg(voetspoor)],het,[]).

n([pl(voetstappen),sg(voetstap)],de,[]).

n([pl(voetstukken),sg(voetstuk)],het,[]).

n([pl(voetzolen),sg(voetzool)],de,[]).

n([pl(vogelen),pl(vogels),sg(vogel)],de,[],
  [s(geluk),
   s(ongeluk),
   ijs,
   roof,
   trek,
   waad,
   water,
   weide,
   zang,
   dim(vogeltje)]).

n([pl(vogelaars),sg(vogelaar)],de,[]).

n([mass(vogue)],de,[]).

n([mass(voicemail),mass('voice-mail'),mass([voice,mail])],de,[]).

n([sg([voice,over]),sg('voice-over')],de,[]).

n([sg([voice,recorder]),pl([voice,recorders])],de,[]).

n([sg(volatiliteit),pl(volatiliteiten)],de,[]).

n([sg(volbloed),pl(volbloeden),pl(volbloeds)],de,[]).

n([pl(voldoendes),sg(voldoende)],de,[]).

n([mass(voldoening)],de,
  [sbar,
   vp]).

n([pl(volgelingen),sg(volgeling)],de,[]).

n([sg(volger),pl(volgers)],de,[]).

n([pl(volgorden),pl(volgordes),sg(volgorde)],de,[sbar,vp]).

n([mass(volharding)],de,[]).

n([mass(volheid)],de,[]).

n([pl(volières),sg(volière)],de,[]).

n([pl(volken),pl(volkeren),sg(volk)],het,[],
  [i(indiaan,'Indianen'),
   i(indiaan,indianen),
   s(krijg),
   dim(volkje)]).

n([mass(volkenkunde)],de,[]).

n([pl(volksdelen),sg(volksdeel)],het,[]).

n([mass(volksgeloof)],het,[]).

n([mass(volksgezondheid)],de,[]).

n([mass(volkshuisvesting)],de,[]).

n([mass(volksleven)],het,[]).

n([pl(volksliederen),sg(volkslied)],het,[],[dim(volksliedje)]).

n([mass(volksmond)],de,[]).

n([pl(volksrepublieken),sg(volksrepubliek)],de,[]).

n([pl(volksstammen),sg(volksstam)],de,[measure]).

n([pl(volkstellingen),sg(volkstelling)],de,[]).

n([pl(volkstuinen),sg(volkstuin)],de,[],[dim(volkstuintje)]).

n([pl(volksvertegenwoordigers),sg(volksvertegenwoordiger)],de,[]).

n([pl(volksvertegenwoordigingen),sg(volksvertegenwoordiging)],de,[]).

n([sg(volksvijand),pl(volksvijanden)],de,[]).

n([mass(volledigheid)],de,[]).

n([sg(volley),pl(volleys)],de,[]).

n([mass(volley)],het,[]).

n([pl(volleyballen),sg(volleybal)],de,[]).

n([mass(volleybal)],het,[]).

n([sg(volleyballer),pl(volleyballers)],de,[]).

n([sg(volleybalster),pl(volleybalsters)],de,[]).

n([pl(volmaaktheden),sg(volmaaktheid)],de,[]).

n([pl(volmachten),sg(volmacht)],de,[sbar,vp]).

n([meas(volt)],de,[meas_mod,measure],
  [kilo]).

n([pl(voltooiingen),sg(voltooiing)],de,[]).

n([pl(voltrekkingen),sg(voltrekking)],de,[]).

n([pl(volumen),pl(volumes),sg(volume)],het,[measure]).

n([stem(volwassen),pl(volwassenen),sg(volwassene)],de,[]).

n([mass(volwassenheid)],de,[]).

n([pl(vondelingen),sg(vondeling)],de,[]).

n([pl(vondsten),sg(vondst)],de,[sbar],[dim(vondstje)]).

n([pl(vonken),sg(vonk)],de,[measure],[dim(vonkje)]).

n([pl(vonnissen),sg(vonnis)],het,[]).

n([pl(voogden),sg(voogd)],de,[],
  [s(gezin)]).

n([pl(voogdijen),sg(voogdij)],de,[],
  [kerk]).

n([sg([voor,en,tegen]),pl([voors,en,tegens])],het,[]).

n([pl(voorarresten),sg(voorarrest)],het,[pred_pp(in)]).

n([pl(voorassen),sg(vooras)],de,[]).

n([pl(vooravonden),sg(vooravond)],de,[]).

n([mass([voorbedachten,rade])],de,[]).

n([pl(voorbeelden),sg(voorbeeld)],het,[app_measure,sbar,vp],
  [school,
   dim(voorbeeldje)]).

n([sg(voorbehoud),pl(voorbehouden)],het,[sbar,vp]).

n([pl(voorbereidingen),sg(voorbereiding)],de,[pred_pp(in), vp]).

n([pl(voorbijgangers),sg(voorbijganger)],de,[]).

n([pl(voorbijgangsters),sg(voorbijgangster)],de,[]).

n([pl(voorboden),pl(voorbodes),sg(voorbode)],de,[]).

n([pl(voordelen),sg(voordeel)],het,[sbar,
                                    vp,
                                    subject_sbar,
                                    subject_vp,
				    pred_pp(in),
				    pred_pp(in,subject_sbar),
				    pred_pp(in,subject_vp)]).

n([pl(voordelen),sg(voordeel)],het,[],
  [belasting,
   thuis,
   dim(voordeeltje)]).

n([pl(voordrachten),sg(voordracht)],de,[sbar,vp]).

n([pl(voorgangers),sg(voorganger)],de,[]).

n([pl(voorgangsters),sg(voorgangster)],de,[]).

n([pl(voorgeslachten),sg(voorgeslacht)],het,[]).

n([pl(voorgevels),sg(voorgevel)],de,[]).

n([pl(voorgronden),sg(voorgrond)],de,[pred_pp(op)]).

n([pl(voorhanden),sg(voorhand)],de,[]).

n([pl(voorhoeden),pl(voorhoedes),sg(voorhoede)],de,[]).

n([pl(voorhoofden),sg(voorhoofd)],het,[]).

n([pl(voorhuiden),sg(voorhuid)],de,[]).

n([pl(voorjaren),sg(voorjaar)],het,[temp_mod,sbar]).

n([pl(voorkanten),sg(voorkant)],de,[]).

n([pl(voorkeuren),sg(voorkeur)],de,[sbar,subject_sbar,
                                    vp,subject_vp]).

n([pl(voorkeurstemmen),sg(voorkeurstem)],de,[]).

n([pl(voorkomens),sg(voorkomen)],het,[]).

n([pl(voorkomingen),sg(voorkoming)],de,[]).

n([pl(voorlichters),sg(voorlichter)],de,[]).

n([pl(voorlichtsters),sg(voorlichtster)],de,[]).

n([pl(voorlichtingen),sg(voorlichting)],de,[]).

n([pl(voorlichtingscampagnes),sg(voorlichtingscampagne)],de,[]).

n([mass(voorliefde),pl(voorliefdes),sg(voorliefde)],de,[]).

n([pl(voorlopers),sg(voorloper)],de,[]).

n([pl(voormannen),sg(voorman)],de,[]).

n([pl(voormiddagen),sg(voormiddag)],de,
  [temp_mod,sbar,measure],
  [zondag,
   maandag,
   dinsdag,
   woensdag,
   donderdag,
   vrijdag,
   zaterdag,
   zondag]).

n([pl(voornamen),sg(voornaam)],de,[]).

n([pl(voornemens),sg(voornemen)],het,[sbar,vp]).

n([sg(vooronder),pl(vooronders)],het,[]).

n([pl(vooronderstellingen),sg(vooronderstelling)],de,[sbar,subject_sbar,vp]).

n([pl(vooronderzoeken),sg(vooronderzoek)],het,[]).

n([pl(vooroordelen),sg(vooroordeel)],het,[sbar,subject_sbar,vp]).

n([pl(voorouders),sg(voorouder)],de,[]).

n([pl('voorpagina\'s'),sg(voorpagina)],de,[]).

n([pl(voorpleinen),sg(voorplein)],het,[]).

n([pl(voorpoten),sg(voorpoot)],de,[]).

n([pl(voorportalen),sg(voorportaal)],het,[]).

n([sg(voorproefje),pl(voorproefjes)],het,[]).

n([pl(voorraden),sg(voorraad)],de,[measure]).

n([pl(voorraden),sg(voorraad)],de,[],
  [energie,
   goud,
   olie,
   wapen,
   woning]).

n([mass(voorrang)],de,[]).

n([pl(voorrechten),sg(voorrecht)],het,[sbar,subject_vp,vp]).

n([pl(voorronden),pl(voorrondes),sg(voorronde)],de,[]).

n([pl(voorruiten),sg(voorruit)],de,[]).

n([pl(voorschoten),sg(voorschoot)],de,[]).

n([pl(voorschotten),sg(voorschot)],het,[]).

n([pl(voorschriften),sg(voorschrift)],het,[sbar,vp],
  [s(dokter),
   s(gezondheid),
   s(veiligheid)]).

n([pl(voorspelen),sg(voorspel)],het,[]).

n([pl(voorspelbaarheden),sg(voorspelbaarheid)],de,[sbar]).

n([pl(voorspellers),sg(voorspeller)],de,[]).

n([pl(voorspellingen),sg(voorspelling)],de,[sbar,vp]).

n([mass(voorspoed)],de,[]).

n([pl(voorspraken),sg(voorspraak)],de,[]).

n([pl(voorsprongen),sg(voorsprong)],de,[sbar,pred_pp(op)]).

n([pl(voorstanders),sg(voorstander)],de,[]).

n([pl(voorstandsters),sg(voorstandster)],de,[]).

n([pl(voorstellen),sg(voorstel)],het,
  [sbar,
   vp,
   subject_vp,
   subject_sbar,
   app_measure],
  [commissie,
   compromis,
   i(initiatief_wet,initiatiefwets),
   s(kabinet),
   i(kind,kinder),
   s(regering),
   s(schikking),
   tegen,
   s(vrede),
   s(wet),
   s(wetgeving),
   s(wijziging)]).

n([pl(voorstellingen),sg(voorstelling)],de,[sbar,vp]).

n([pl(voorstellingen),sg(voorstelling)],de,[np_app_measure],
  [dans,
   familie,
   i(kind,kinder),
   i(muziek_theater,muziektheater),
   solo,
   theater,
   toneel]).

n([pl(voorstevens),sg(voorsteven)],de,[]).

n([pl(voorstoppers),sg(voorstopper)],de,[]).

n([pl(voorstopsters),sg(voorstopster)],de,[]).

n([pl(voortanden),sg(voortand)],de,[]).

% already nominalized verb
%n([mass(voortbestaan)],het,[]).

n([sg(voortbrenging),pl(voortbrengingen)],de,[]).

n([pl(voortbrengselen),pl(voortbrengsels),sg(voortbrengsel)],het,[]).

n([mass(voortduring)],de,[]).

n([pl(voortekenen),pl(voortekens),sg(voorteken)],het,[sbar]).

n([pl(voortgangen),sg(voortgang)],de,[]).

n([sg(voortouw)],het,[]).

n([mass(voortplanting)],de,[]).

%% niet voor_trek_kers
n([sg(voortrekker),pl(voortrekkers)],de,[]).

n([sg(voortrekster),pl(voortreksters)],de,[]).

n([mass(voortvarendheid)],de,[]).

n([pl(voortzettingen),sg(voortzetting)],de,[]).

n([sg(vooruitblik)],de,[]).

n([sg(vooruitgang)],de,[sbar]).

n([pl(vooruitzichten),sg(vooruitzicht)],het,[sbar,vp,subject_sbar]).

n([pl(voorvallen),sg(voorval)],het,[]).

n([pl(voorvechters),sg(voorvechter)],de,[]).

n([mass(voorverkoop)],de,[]).

n([pl(voorvoegsels),sg(voorvoegsel)],het,[app_measure]).

n([pl(voorwaarden),sg(voorwaarde)],de,
  [sbar,
   subject_sbar,
   subject_vp,
   vp],
  [s(annulering),
   s(arbeid),
   basis,
   s(leven),
   licentie,
   polis,
   prijs,
   rand,
   s(vervoer),
   tarief]).

n([pl(voorwendselen),pl(voorwendsels),sg(voorwendsel)],het,
  [sbar,
   subject_sbar,
   subject_vp,
   vp]).

n([pl(voorwerpen),sg(voorwerp)],het,[],[dim(voorwerpje)]).

n([mass(voorwetenschap)],de,[]).

n([pl(voorzetten),sg(voorzet)],de,[]).

n([sg(voorzetsel),pl(voorzetsel)],het,[]).

n([mass(voorzichtigheid)],de,[]).

n([mass(voorzienigheid)],de,[]).

n([pl(voorzieningen),sg(voorziening)],de,[sbar,vp],[]).

n([pl(voorzieningen),sg(voorziening)],de,[],
  [s(arbeid),
   basis,
   energie,
   informatie,
   oudedag,s(oudedag),
   remigratie,h(remigratie),
   s(pensioen),
   stroom,
   voedsel,
   water,
   zorg]).

n([pl(voorzitters),sg(voorzitter)],de,[],
  [s(bestuur),bestuur,
   s(bond),
   commissie,
   h('CD'),
   h('CDA'),
   h('CNV'),
   college,
   i('D66','D\'66-'),h('D66'),
   directie,
   ere,
   h('EU'),
   h('FNV'),
   i(fractie,fraktie),fractie,
   i('CD_fractie','CD-fractie'),
   i('CDA_fractie','CDA-fractie'),
   i('D66_fractie','D66-fractie'),
   i('D66_fractie','D\'66-fractie'),
   i('GroenLinks_fractie','GroenLinks-fractie'),
   i('LPF_fractie','LPF-fractie'),
   i('PvdA_fractie','PvdA-fractie'),
   i('SP_fractie','SP-fractie'),
   i('VVD_fractie','VVD-fractie'),
   i('CD_fractie','CD-fraktie'),
   i('CDA_fractie','CDA-fraktie'),
   i('D66_fractie','D66-fraktie'),
   i('D66_fractie','D\'66-fraktie'),
   i('GroenLinks_fractie','GroenLinks-fraktie'),
   i('LPF_fractie','LPF-fraktie'),
   i('PvdA_fractie','PvdA-fraktie'),
   i('SP_fractie','SP-fraktie'),
   i('VVD_fractie','VVD-fraktie'),
   h('GroenLinks'),
   h('LPF'),
   h('IOC'),
   jury,
   i(jong,jongeren),
   'Kamer',
   ['Tweede','Kamer'],
   ['Eerste','Kamer'],
   kamer,
   omroep,
   onder,
   oud,
   partij,
   s(parlement),
   h('PvdA'),
   h('SP'),
   stadsdeel,
   h(vice),vice,
   h('VVD'),
   s(werkgever)
  ]).

n([sg(voorzitster),pl(voorzitsters)],de,[],
  [s(bestuur),bestuur,
   s(bond),
   commissie,
   h('CD'),
   h('CDA'),
   h('CNV'),
   college,
   i('D66','D\'66-'),h('D66'),
   directie,
   ere,
   h('EU'),
   h('FNV'),
   i(fractie,fraktie),fractie,
   i('CD_fractie','CD-fractie'),
   i('CDA_fractie','CDA-fractie'),
   i('D66_fractie','D66-fractie'),
   i('D66_fractie','D\'66-fractie'),
   i('GroenLinks_fractie','GroenLinks-fractie'),
   i('LPF_fractie','LPF-fractie'),
   i('PvdA_fractie','PvdA-fractie'),
   i('SP_fractie','SP-fractie'),
   i('VVD_fractie','VVD-fractie'),
   i('CD_fractie','CD-fraktie'),
   i('CDA_fractie','CDA-fraktie'),
   i('D66_fractie','D66-fraktie'),
   i('D66_fractie','D\'66-fraktie'),
   i('GroenLinks_fractie','GroenLinks-fraktie'),
   i('LPF_fractie','LPF-fraktie'),
   i('PvdA_fractie','PvdA-fraktie'),
   i('SP_fractie','SP-fraktie'),
   i('VVD_fractie','VVD-fraktie'),
   h('GroenLinks'),
   h('LPF'),
   h('IOC'),
   jury,
   'Kamer',
   kamer,
   omroep,
   onder,
   oud,
   partij,
   s(parlement),
   h('PvdA'),
   h('SP'),
   stadsdeel,
   h(vice),vice,
   h('VVD'),
   s(werkgever)
  ]).

n([pl(voorzitterschappen),sg(voorzitterschap)],het,[],
  [fractie]).

n([pl(voorzomers),sg(voorzomer)],de,[]).

n([pl(voorzorgen),sg(voorzorg)],de,[sbar,vp]).

n([pl(vorderingen),sg(vordering)],de,[sbar,vp]).

n([sg(vore),pl(voren)],de,[]).

%% Russische maffia?
n([mass([vory,v,zakone]),
   mass([vori,v,zakone]),
   mass([vor,v,zakone])],de,[]).

n([pl(vorken),sg(vork)],de,[]).

n([pl(vormen),sg(vorm)],de,
  [measure, % verschillende vormen voelsprieten
   pred_pp(uit)]).

n([pl(vormen),sg(vorm)],de,
  [pred_pp(in)],
  [f([ik]),h(ik),
   poeder]).

n([pl(vormen),sg(vorm)],de,[],
  [boek,
   dans,			% niet dan_vorm
   s(beleid),
   s(regering),
   tussen,
   dim(vormpje)]).

n([mass(vormbehoud)],het,[]).

n([sg(vormer),pl(vormers)],de,[]). % vooral in compounds opinievormers beeldvormers


n([pl(vormgevers),sg(vormgever)],de,[]).

n([pl(vormgeefsters),sg(vormgeefster)],de,[]).

n([mass(vormgeving)],de,[]).

n([pl(vormingen),sg(vorming)],de,[],
  [basis,
   coalitie,
   college,
   s(gezin),
   kartel,
   regio]).

n([sg(vorser),pl(vorsers)],de,[],
  [markt]).

n([pl(vorsten),sg(vorst)],de,[],
  [groot,
   keur,
   kerk]).

n([mass(vorst)],de,[],
  [nacht]).

n([pl(vorstendommen),sg(vorstendom)],het,[]).

n([pl(vorstinnen),sg(vorstin)],de,[]).

n([pl(vossen),sg(vos)],de,[],
  [pool]).

n([sg(voucher),pl(vouchers)],both,[],
  [credit]).

n([pl(vouwen),sg(vouw)],de,[],
  [dim(vouwtje)]).

n([pl(voyeurs),sg(voyeur)],de,[]).

n([pl(vragen),sg(vraag)],de,
  [sbar,
   subject_sbar_no_het,   % dan is de vraag of hij komt ; dan is de vraag : komt hij
   vp,
   start_app_measure],
  [bonus,
   s(geweten),
   ham,
   kern,
   strik,
   weder,
   dim(vraagje)]).

n([pl(vragen),sg(vraag)],de,
  [],
  [kamer,
   i(kamer,'Kamer'),
   rond,
   schuld,
   terug,
   h(waarom),
   waarom,
   f([waarom])]).

n([pl(vraagbaken),sg(vraagbaak)],de,[]).

n([pl(vraagstellingen),sg(vraagstelling)],de,[sbar,vp]).

n([pl(vraagstukken),sg(vraagstuk)],het,
  [sbar,vp, subject_sbar]).

n([pl(vraagtekens),sg(vraagteken)],het,
  [sbar,vp, subject_sbar]).

n([pl(vrachten),sg(vracht)],de,[measure],[dim(vrachtje)]).

n([sg(vrachtbrief),pl(vrachtbrieven)],de,[]).

n([pl(vrachtvaarders),sg(vrachtvaarder)],de,[]).

n([mass(vrachtverkeer)],het,[]).

n([mass(vrachtvervoer)],het,[]).

n([pl(vragers),sg(vrager)],de,[]).

n([pl(vredes),sg(vrede)],de,[],
  [godsdienst]).

n([pl(vredesconferenties),sg(vredesconferentie)],de,[]).

n([pl(vredesonderhandelingen),sg(vredesonderhandeling)],de,[]).

n([pl(vredesverdragen),sg(vredesverdrag)],het,[]).

n([pl(vreemdelingen),sg(vreemdeling)],de,[]).

n([mass(vreemdelingenlegioen)],het,[]).

n([pl(vrezen),sg(vrees)],de,
  [sbar,
   vp,
   subject_sbar,
   subject_vp
  ]).

n([pl(vrezen),sg(vrees)],de,[],
  [examen,
   hoogte,
   koudwater,
   plein]).

n([mass(vreten)],het,[]).

n([pl(vreugden),pl(vreugdes),
   sg(vreugde),sg(vreugd)],de,
  [sbar,vp],
  [feest]).

n([pl(vrienden),sg(vriend)],de,
  [np_app_measure],
  [boezem,
   h(ex),
   jeugd,
   dim(vriendje)]).

n([pl(vriendelijkheden),sg(vriendelijkheid)],de,[]).

n([pl(vriendenkringen),sg(vriendenkring)],de,[]).

n([pl(vriendinnen),sg(vriendin)],de,
  [np_app_measure],
  [h(ex),
   dim(vriendinnetje)]).

n([mass(vriendjespolitiek)],de,[]).

n([pl(vriendschappen),sg(vriendschap)],de,[]).

n([sg(vriespunt),pl(vriespunten)],het,[]).

n([sg(vriezer),pl(vriezers)],de,[]).

n([mass(vrij)],both,[]).  % (geen) vrij krijgen/nemen/hebben/vragen

n([mass(vrijaf)],both,[]).  % (geen) vrijaf krijgen/nemen/hebben/vragen

n([pl(vrijbrieven),sg(vrijbrief)],de,[vp]).

n([sg(vrijbuiter),pl(vrijbuiters)],de,[]).

n([sg(vrijdag),pl(vrijdagen)],de,[temp_mod,sbar]).

n([pl(vrijdenkers),sg(vrijdenker)],de,[]).

n([pl(vrijers),sg(vrijer)],de,[]).

n([mass(vrijetijdsbesteding)],de,[]).

n([sg(vrijgeleide),pl(vrijgeleides)],de,[]).

n([pl(vrijgezellen),sg(vrijgezel)],de,[]).

n([mass(vrijhandel)],de,[]).

n([pl(vrijheden),sg(vrijheid)],de,[vp],
  [h(keuze),keuze
  ]).

n([pl(vrijheden),sg(vrijheid)],de,[],
  [s(beleid),
   s(beweging),
   godsdienst,
   on,
   pers,
   s(uiting)
  ]).

n([mass([vrijheid,blijheid])],both,[]).

n([pl(vrijlatingen),sg(vrijlating)],de,[]).

n([pl(vrijmetselaars),pl(vrijmetselaren),sg(vrijmetselaar)],de,[]).

n([mass(vrijmetselarij)],de,[]).

n([pl(vrijspraken),sg(vrijspraak)],de,[]).

n([sg(vrijstaat),pl(vrijstaten)],de,[]).

n([pl(vrijstellingen),sg(vrijstelling)],de,[sbar,vp,app_measure]).

n([pl(vrijstellingen),sg(vrijstelling)],de,[],[bagage]).

n([pl(vrijsters),sg(vrijster)],de,[]).

n([pl(vrijwaringen),sg(vrijwaring)],de,[]).

n([pl(vrijwilligers),sg(vrijwilliger)],de,[]).

n([mass(vrijwilligheid)],de,[]).

n([pl(vrijwilligsters),sg(vrijwilligster)],de,[]).

n([pl(vrinden),sg(vrind)],de,[]).

n([sg(vroedschap),pl(vroedschappen)],both,[]).

n([pl(vroedvrouwen),sg(vroedvrouw)],de,[]).

n([mass(vroegte)],de,[]).

n([pl(vrolijkheden),sg(vrolijkheid)],de,[]).

n([mass(vroomheid)],de,[]).

n([pl(vrouwen),sg(vrouw)],de,[],
  [s(burgemeester),
   s(bewind),
   buur,
   h(ex),
   gast,
   hockey,
   kop,
   moslim,
   i(zaak,zaken),
   s(zeg),
   dim(vrouwtje)]).

n([sg(vrouwe)],de,[],[s(raad)]).

n([mass(vrouwelijkheid)],de,[]).

n([mass(vrouwenemancipatie)],de,[]).

n([pl(vrouwenstemmen),sg(vrouwenstem)],de,[]).

n([pl(vrouwlui),pl(vrouwmensen),sg(vrouwmens)],het,[]).

n([mass(vrouwvolk)],het,[]).

n([pl(vruchten),sg(vrucht)],de,[],[dim(vruchtje)]).

n([mass(vruchtbaarheid)],de,[]).

n([pl(vruchtbomen),sg(vruchtboom)],de,[]).

n([mass(vruchtwater)],het,[]).

n([mass(vuil)],het,[],
  [zwerf,
   dim(vuiltje)]).

n([pl(vuiligheden),sg(vuiligheid)],de,[sbar,vp]).

n([mass(vuilnis)],both,[]).

n([pl(vuilnisbakken),sg(vuilnisbak)],de,[]).

n([pl(vuilnisbelten),sg(vuilnisbelt)],de,[]).

n([pl(vuilniszakken),sg(vuilniszak)],de,[]).

n([pl(vuisten),sg(vuist)],de,[],[dim(vuistje)]).

n([pl(vuistregels),sg(vuistregel)],de,[sbar, subject_sbar]).

n([pl(vulkanen),sg(vulkaan)],de,
  [np_app_measure]).

n([pl(vullingen),sg(vulling)],de,[]).

n([pl(vulpennen),sg(vulpen)],de,[]).

n([pl('vulva\'s'),sg(vulva)],de,[]).

n([pl(vuren),sg(vuur)],het,[],
  [sper,
   dim(vuurtje)]).

n([stem('VUT'),
   sg(vut),sg('VUT')],de,[]).

n([pl(vuurpelotons),sg(vuurpeloton)],het,[]).

n([pl(vuurpijlen),sg(vuurpijl)],de,[]).

n([pl(vuurwapenen),pl(vuurwapens),sg(vuurwapen)],het,[]).

n([mass(vuurwerk)],het,[],
  [i(consument,consumenten)]).

n([pl(vuurzeeën),sg(vuurzee)],de,[]).

%% pl: ze zit in 3 vwo
n([mass(vwo),pl(vwo)],het,[]).

n([mass('vwo/havo'),pl('vwo/havo')],both,[]).

n([sg(vzw)],de,[np_app_measure]). % VL

n([pl('w\'s'),sg(w)],de,[],[dim('w\'tje')]).

n([sg(waag)],de,[]).

n([pl(waagschalen),sg(waagschaal)],de,[]).

n([pl(waagstukken),sg(waagstuk)],het,[subject_sbar,subject_vp]).

n([pl(waaiers),sg(waaier)],de,[measure],[dim(waaiertje)]).

n([pl(waakhonden),sg(waakhond)],de,[],
  [beurs,
   kartel]).

n([mass(waakzaamheid)],de,[]).

n([mass(waan),pl(wanen)],de,[sbar,vp]).

n([pl(waanvoorstellingen),sg(waanvoorstelling)],de,[sbar]).

n([mass(waanzin)],de,[subject_sbar,subject_vp]).

n([mass(waar)],de,[],[s(handel)]).		% waar voor je geld geven/krijgen/..

n([mass(waarachtigheid)],de,[]).

n([pl(waarborgen),sg(waarborg)],de,[sbar]).

n([sg(waard)],de,[]).

n([pl(waarden),pl(waardes),sg(waarde)],de,[pred_pp(van)]).

n([pl(waarden),pl(waardes),sg(waarde)],de,[],
  [boek,
   grens,
   invoer,
   minimum,
   maximum,
   over,
   straat,
   woord]).  %scrabble

n([pl(waardeoordelen),sg(waardeoordeel)],het,[sbar,vp]).

n([pl(waarderingen),sg(waardering)],de,[],
  [af,
   onder,
   over]).

n([pl(waardeverminderingen),sg(waardevermindering)],de,[]).

n([pl(waardigheden),sg(waardigheid)],de,
  [pred_pp(beneden),
   sbar,
   vp
  ]).

n([pl(waardigheden),sg(waardigheid)],de,[],
  [krediet]).

n([pl(waardinnen),sg(waardin)],de,[]).

n([pl(waarheden),sg(waarheid)],de,
  [sbar,
   vp,
   subject_sbar_no_het]).

n([pl(waarnemers),sg(waarnemer)],de,[],
  [h('OVSE'),
   h('VN'),
   zaak]).

n([pl(waarneemsters),sg(waarneemster)],de,[],
  [h('OVSE'),
   h('VN'),
   zaak]).

n([pl(waarnemingen),sg(waarneming)],de,[sbar]).

n([mass(waarom)],het,[]).

n([pl(waarschijnlijkheden),sg(waarschijnlijkheid)],de,[sbar]).

n([pl(waarschuwingen),sg(waarschuwing)],de,
  [sbar,
   vp]).

n([pl(waarschuwingen),sg(waarschuwing)],de,
  [],
  [weer]).

n([pl(waarzegsters),sg(waarzegster)],de,[]).

n([pl(waarzeggers),sg(waarzegger)],de,[]).

n([mass(waarzeggerij)],de,[]).  % waar_zegge_rij

n([mass(waas)],both,[]).

n([pl(wachten),sg(wacht)],de,[],
  [burger,
   kust,
   s(stad)]).

n([pl(wachters),sg(wachter)],de,[]).

n([sg(wachtgelder),pl(wachtgelders)],de,[]).

n([pl(wachtmeesters),sg(wachtmeester)],de,[]).

n([pl(wachtposten),sg(wachtpost)],de,[]).

n([pl(wadden),sg(wad)],het,[]).

n([pl(waddeneilanden),sg(waddeneiland)],het,[]).

n([pl(wafelen),pl(wafels),sg(wafel)],de,[],[dim(wafeltje)]).

n([pl(wagens),sg(wagen)],de,[],
  [aanhang,
   s(bedrijf),
   bestel,
   commando,
   wh(['Formule','1']),
   hooi,
   kampeer,
   i(kind,kinder),
   kraan,
   lijk,
   pantser,
   patrouille,
   i(persoon,personen),
   politie,
   ruimte,
   sport,
   strijd,
   takel,
   tank,
   terrein,
   verhuis,
   vracht,
   vuilnis,
   woon,
   i(ziek,zieken),
   dim(wagentje)]).

%% drie vrachtwacgens vuilnis
n([pl(wagens),sg(wagen)],de,[measure],
  [aanhang,
   bestel,
   verhuis,
   vracht,
   vuilnis,
   dim(wagentje)]).

n([pl(wagons),sg(wagon)],de,[measure],[dim(wagonnetje)]).

n([pl(wakken),sg(wak)],het,[]).

n([sg(wake),pl(wakes)],de,[]).

n([pl(wallen),sg(wal)],de,[]).

n([mass(walg)],de,[]).

n([mass(walging)],de,[]).

n([pl(walkers),sg(walker)],de,[]).

n([pl('walkie-talkies'),sg('walkie-talkie')],de,[]).

n([sg(walkman),pl(walkmen),pl(walkmans)],de,[]).

n([pl(walmen),sg(walm)],de,[]).

n([pl(walnoten),sg(walnoot)],de,[]).

n([pl(walsen),sg(wals)],de,[]).

n([mass(walstro)],het,[]).

n([pl(walvissen),sg(walvis)],de,[]).

n([sg(wanbetaler),pl(wanbetalers)],de,[]).

n([pl(wanden),sg(wand)],de,[],
  [achter,
   berg,
   darm,
   rots,
   dim(wandje)]).

n([pl(wandaden),sg(wandaad)],de,
  [sbar,vp]).

n([mass(wandel)],de,[]).

n([pl(wandelaars),sg(wandelaar)],de,[]).

n([pl(wandelingen),sg(wandeling)],de,[],
  [ruimte,
   dim(wandelingetje)]).

n([pl(wandelstokken),sg(wandelstok)],de,[]).

n([pl(wangen),sg(wang)],de,[],[dim(wangetje)]).

n([pl(wangedragingen),sg(wangedrag)],het,[sbar,vp]).

n([mass(wanhoop)],de,[]).

n([mass(wanorde)],de,[]).

n([pl(wanprestaties),sg(wanprestatie)],de,[]).

n([sg(want)],het,[]).

n([sg(want),pl(wanten)],de,[]).

n([pl(wantoestanden),sg(wantoestand)],de,[]).

n([mass(wantrouwen)],het,[]).

n([sg(wants),pl(wantsen)],de,[]).

n([sg('wao\'er'),pl('wao\'ers')],de,[]).

n([sg('WAO\'er'),pl('WAO\'ers')],de,[]).

n([mass(wap)],both,[]).

n([pl(wapenen),pl(wapens),sg(wapen)],het,[],
  [dienst,
   gemeente,
   kern,
   moord,
   steek,
   i(massa_vernietiging,massavernietigings)]).

n([pl('wapenembargo\'s'),sg(wapenembargo)],het,[]).

n([mass(wapengekletter)],het,[]).

n([pl(wapenstilstanden),sg(wapenstilstand)],de,[]).

n([mass(wapentuig)],het,[]).

n([sg(wapper),pl(wappers)],de,[]).

n([sg([war,room]),pl([war,rooms])],de,[]).

n([mass(warboel)],de,[]).

n([pl(waren)],de,[]).

n([sg(warehouse),pl(warehouses)],both,[]).

n([pl(warenhuizen),sg(warenhuis)],het,[]).

n([pl(warhoofden),sg(warhoofd)],het,[]).

n([sg(warmer),pl(warmers)],de,[]).

n([sg([warming,up]),pl([warming,ups]),sg('warming-up'),pl('warming-ups')],de,[]).

n([mass(warmte)],de,[]).

n([pl(warmtebronnen),sg(warmtebron)],de,[]).

n([sg(warrant),pl(warrants)],de,[]).

n([mass(wartaal)],de,[]).

n([mass(was)],de,[]).

n([pl(wasbakken),sg(wasbak)],de,[]).

n([sg(wasem)],de,[]).

n([mass(wasgoed)],het,[]).

n([pl(wasmachines),sg(wasmachine)],de,[]).

n([pl(wassenaars),sg(wassenaar)],de,[]).

n([pl(wasserijen),sg(wasserij)],de,[]).

n([mass(water),pl(wateren),pl(waters),sg(water)],het,[],
  [afval,
   bron,
   drink,
   hoog,
   leiding,
   oppervlakte,
   rivier,
   suiker,
   zoet,
   zout,
   zwem]).

n([pl(waterdampen),sg(waterdamp)],de,[]).

n([pl(waterdruppels),sg(waterdruppel)],de,[]).

n([pl(waterkanten),sg(waterkant)],de,[]).

n([sg(waterkering),pl(waterkeringen)],de,[]).

n([pl(waterlelies),pl(waterleliën),sg(waterlelie)],de,[]).

n([pl(wateroppervlakken),sg(wateroppervlak)],het,[]).

n([mass(waterpolo)],het,[]).

n([sg(waterpoloër),pl(waterpoloërs)],de,[]).

n([sg(waterpoloster),pl(waterpolosters)],de,[]).

n([pl(waterputten),sg(waterput)],de,[]).

n([pl(waterschappen),sg(waterschap)],het,[]).

n([pl(waterscheidingen),sg(waterscheiding)],de,[]).

n([mass(watersnood)],de,[]).

n([mass(waterstaat)],de,[]).

n([pl(waterstanden),sg(waterstand)],de,[]).

n([mass(waterstof)],both,[]).

n([pl(waterstralen),sg(waterstraal)],de,[]).

n([pl(watervallen),sg(waterval)],de,[]).

n([pl(watjes),sg(watje)],het,[]).

n([pl(watts),meas(watt)],de,
  [meas_mod,
   measure],
  [kilo,
   mega]).

n([pl(watten)],de,[]).

n([mass(wax)],de,[]).

%% VL: het wc
n([sg(wc),pl('wc\'s'),pl('WC\'s')],both,[]).

n([pl(webben),sg(web)],het,[],
  [i(spin,spinnen)]).

n([sg(webcam),pl(webcams)],de,[]).

n([sg(weblog),pl(weblogs)],both,[app_measure]).

n([sg(website),pl(websites)],both,[app_measure]).

n([sg(webstek),pl(webstekken)],both,[app_measure]).

n([pl(wedden),sg(wedde)],de,[]).

n([pl(weddenschappen),sg(weddenschap)],de,[sbar]).

n([pl(wedergeboorten),sg(wedergeboorte)],de,[]).

n([mass(wederhoor)],de,[]).

n([mass(wederkeer)],de,[]).

n([mass(wederkerigheid)],de,[]).

n([mass(wederopbouw)],de,[]).

n([mass(wederopstanding)],de,[]).

n([mass(wedervaren)],het,[]).

n([pl(wederwaardigheden),sg(wederwaardigheid)],de,[]).

n([mass(wedijver)],de,[]).

n([pl(wedlopen),sg(wedloop)],de,[],
  [wapen]).

n([sg([heen,'-en',terugwedstrijd]),
   pl([heen,'-en',terugwedstrijden])],de,[]).

n([sg(wedstrijd),pl(wedstrijden)],de,
  [temp_mod,
   measure, % twee wedstrijden schorsing
   app_measure, % de wedstrijd schermen
   sbar],
  [s(afscheid),
   beker,
   boks,
   s(beslissing),
   competitie,
   'Davis-Cup',
   'Europa-cup',
   'Europa-Cup',
   wh(['Europa','cup']),
   wh(['Europa','Cup']),
   'UEFA-cup',
   'UEFA-Cup',
   wh(['UEFA','cup']),
   wh(['UEFA','Cup']),
   'Champions-League',
   wh(['Champions','League']),
   s(groep),
   hardloop,
   heen,
   i(land,landen),
   oefen,
   s(opening),
   poule,
   record,
   kwalificatie,
   i('EK_kwalificatie','EK-kwalificatie'),
   i('WK_kwalificatie','WK-kwalificatie'),
   terug,
   thuis,
   uit,
   voetbal,
   weg,
   i(wereld_beker,wereldbeker),
   wieler,
   dim(wedstrijdje)]).

n([pl(weduwen),pl(weduwes),sg(weduwe)],de,[]).

n([pl(weduwnaars),sg(weduwnaar)],de,[]).

n([pl(weeën),sg(wee)],both,[]).

n([pl(weefselen),pl(weefsels),sg(weefsel)],het,[],
  [bind,
   klier,
   dim(weefseltje)]).

n([pl(weegschalen),sg(weegschaal)],de,[]).

n([sg(week),pl(weken)],de,[temp_mod,sbar,measure],
  [les,
   s(handel),
   school,
   dim(weekje)]).

n([sg(week),pl(weken)],de,[temp_mod,sbar],
  [i(boek,boeken),
   i(kind_boek,kinderboeken),
   dim(weekje)]).

n([sg(weekeinde),sg(weekeind),
   pl(weekeindes),pl(weekeinden)],het,
  [temp_mod,sbar,measure],
  [paas]).

%% VL: de weekend
n([sg(weekend),pl(weekenden),pl(weekends)],both,[measure,temp_mod,sbar],
  [paas,
   pinkster,
   dim(weekendje)]).

n([sg(weeklang)],both,[temp_mod,sbar]).

n([mass(weelde)],de,[]).

n([mass(weemoed)],de,[]).

n([mass(weer),mass(weder)],het,[],
  [herfst,
   kut,
   lente,
   winter,
   zomer,
   dim(weertje)]).

n([mass(weerbaarheid)],de,[]).

n([pl(weerberichten),sg(weerbericht)],het,[sbar]).

n([mass(weerga)],de,[]).

n([mass(weergalm)],de,[]).

n([sg(weergave),pl(weergaven)],de,[]).

n([pl(weergegevens),sg(weergegeven)],het,[]).

n([pl(weerkaatsingen),sg(weerkaatsing)],de,[]).

n([mass(weerklank)],de,[]).

n([sg(weerkundige),pl(weerkundigen)],de,[]).

n([pl(weermannen),sg(weerman)],de,[]).

n([mass(weeromstuit)],de,[]).

n([mass(weerschijn)],de,[]).

%% aan weerskanten, naar weerskanten
n([pl(weerskanten)],both,[]).

n([pl(weerslagen),sg(weerslag)],de,[]).

n([pl(weerspiegelingen),sg(weerspiegeling)],de,[]).

n([pl(weerstanden),sg(weerstand)],de,[vp]).

n([pl(weersverwachtingen),sg(weersverwachting)],de,[]).

n([stem(weerszijde),
   pl(weerszijden)],both,[]).

n([mass(weerwerk)],het,[]).

n([mass(weerwil)],de,[]).

n([pl(weerwolven),sg(weerwolf)],de,[]).

n([mass(weerzien)],het,[]).

n([mass(weerzin)],de,[]).

n([pl(wezen),sg(wees)],de,[],
  [aids,i(aids,'Aids')]).

n([pl(weeshuizen),sg(weeshuis)],het,[]).

n([pl(weeskinderen),sg(weeskind)],het,[]).

n([pl(weetjes),sg(weetje)],het,[sbar,vp]).

n([pl(wegen),sg(weg)],de,
  [subject_vp,
   pred_pp(op),
   temp_mod],                           % het was een moeizame weg om...
  []).			

n([pl(wegen),sg(weg),
   ignore_stem(weg)],de,
  [pred_pp(op),
   temp_mod],                    % hij zat de hele (heen)weg te klieren
  [heen,
   terug]).			

n([pl(wegen),sg(weg),
   ignore_stem(weg)],de,[],
  [asfalt,
   auto,
   i(auto_snel,autosnel),
   berg,
   grind,
   hoofd,
   land,
   s(leven),
   s(lijden),
   leer,
   lucht,
   melk,
   midden,
   om,
   rij,
   ring,
   s(rijk),
   rond,
   snel,
   straat,
   s(toegang),
   tussen,
   uit,
   urine,
   s(verbinding),
   s(verkeer),
   vlucht,
   water,
   zand,
   zij,
   dim(weggetje),
   dim(wegje)]).

n([pl(wegdekken),sg(wegdek)],het,[]).

n([sg(weggebruiker),pl(weggebruikers)],de,[]).

n([mass(weging)],de,[]).

n([mass(wegligging)],de,[]).

%% Later maakte hij de overstap naar het wegrace
n([sg(wegrace)],het,[]).

n([sg(wegval)],de,[]). % VL

n([mass(wegvervoer)],het,[]).

n([mass(wegwerp)],both,[]).

n([pl(wegwijzers),sg(wegwijzer)],de,[]).

n([pl(weiden),sg(weide),sg(wei)],de,[],[dim(weitje)]).

n([pl(weigeringen),sg(weigering)],de,[vp]).

n([pl(weilanden),sg(weiland)],het,[]).

n([sg(weinigje)],het,[mod,measure]).  %ouderwets

n([sg(weit)],het,[]). % zoals boekweit

n([pl(wekkers),sg(wekker)],de,[],[dim(wekkertje)]).

n([mass([wel,en,wee])],het,[]).

n([mass(welbehagen)],het,[]).

n([mass(welbevinden)],het,[]).

n([pl(weldaden),sg(weldaad)],de,[sbar,vp]).

n([pl(weldoeners),sg(weldoener)],de,[]).

n([pl(weldoensters),sg(weldoenster)],de,[]).

n([mass(welgevallen)],het,[]).

n([mass(welkom),pl(welkoms)],het,[]).

n([pl(wellingtons),sg(wellington)],de,[]).

n([mass(wellness)],both,[]).

n([pl(wellusten),sg(wellust)],de,[vp]).

n([pl(welpen),sg(welp)],de,[]).

n([mass(welslagen)],het,[]).

n([mass(welsprekendheid)],de,[]).

n([mass(welstand)],de,[]).

n([mass(welterusten)],both,[]).  % welterusten zeggen ???

n([mass(welvaart)],de,[]).

n([pl(welvingen),sg(welving)],de,[]).

n([pl(welwillendheden),sg(welwillendheid)],de,[sbar,vp]).

n([mass(welzijn)],het,[],[i(dier,dieren)]).

n([sg(wende),pl(wendes)],de,[]).

n([pl(wendingen),sg(wending)],de,[],[plot]).

n([pl(wenken),sg(wenk)],de,[sbar,vp]).

n([pl(wenkbrauwen),sg(wenkbrauw)],de,[]).

n([pl(wensen),sg(wens)],de,[sbar,subject_sbar,subject_vp,vp]).

n([pl(wensen),sg(wens)],de,[],
  [i(kind,kinder)]).

n([pl(wensdromen),sg(wensdroom)],de,[sbar,vp]).

n([mass(wenselijkheid),pl(wenselijkheden),sg(wenselijkheid)],de,[vp]).

n([pl(werelden),sg(wereld)],de,[],
  [bank,
   boks,
   s(bedrijf),
   s(beleving),
   bouw,
   boven,
   buiten,
   denk,
   i(dier,dieren),
   droom,
   s(ervaring),
   film,
   gedachte,i(gedachte,gedachten),
   kunst,
   leef,
   i(man,mannen),
   mode,
   muziek,
   onder,
   reclame,
   reis,
   schaak,
   sport,
   voetbal,
   wieler,
   i(zaak,zaken),
   zwem,
   dim(wereldje)]).

n([stem(wereld_beker),
   pl(wereldbekers),sg(wereldbeker)],de,[app_measure]).

n([pl(wereldbeschouwingen),sg(wereldbeschouwing)],de,[sbar,vp]).

n([mass(wereldbevolking)],de,[]).

n([sg(wereldcup), pl(wereldcups)],de,[app_measure]).

n([pl(werelddelen),sg(werelddeel)],het,[]).

n([mass(wereldgebeuren)],het,[]).

n([mass(wereldliteratuur)],de,[]).

n([mass(wereldluchtvaart)],de,[]).

n([stem(wereld_record_houder),
   sg(wereldrecordhouder),pl(wereldrecordhouders)],de,[app_measure]).

n([stem(wereld_titel),
   pl(wereldtitels),sg(wereldtitel)],de,[app_measure]).

n([stem(wereld_titel_wedstrijd),
   sg(wereldtitelstrijd),pl(wereldtitelstrijden)],de,[app_measure]).

n([mass(wereldvrede)],de,[]).

n([pl(werven),sg(werf)],de,[],
  [bouw]).

n([sg([werk,aan,de,winkel])],het,[],[]).

n([pl(werken),sg(werk)],het,[pred_pp(aan)],[]).

n([pl(werken),sg(werk)],het,[],
  [acteer,
   advies,
   beeldscherm,
   i(bejaard,bejaarden),
   bij,
   borduur, % niet bord_uur_werk
   camera,
   deeltijd,
   denk,
   foto,
   i(gek,gekken),
   s(groep),
   huis,
   jeugd,
   i(jong,jongeren),
   s(liefdadigheid),
   i(mens,mensen),
   i(monnik,monniken),
   papier,
   politie,
   s(ontwikkeling),
   opzoek,
   s(opzoeking),
   over,
   s(pionier),
   s(redding),
   reken,
   schilder,
   straf,
   thuis,
   uitzend,
   vakantie,
   veld,
   voor,
   i(vluchteling,vluchtelingen),
   s(vorming),
   s(vrijwilliger),
   s(welzijn),
   s(zending),
   zwart]).

n([pl(werken),sg(werk)],het,[],
  [beeldhouw,
   binnen,
   boek,
   bouw,
   brei,
   broddel,
   dicht,
   druk,
   glas,
   hek,
   hout,
   i(hout_snij,houtsnij),
   kunst,
   s(leven),
   maat,
   meester,
   metsel,
   naslag,
   opbouw,
   orkest,
   raam,
   rader,
   i(slinger_uur,slingeruur),
   speur,
   standaard,
   tralie,
   uur,
   vaat,
   vak,
   i(voet,voeten),
   weg,   i(weg,wegen),
   dim(werkje)]).

n([mass(werkdruk)],de,[]).

n([mass(werkelijkheid)],de,[subject_sbar]).

n([mass(werkeloosheid)],de,[]).

n([pl(werkers),sg(werker)],de,[],
  [beeldscherm,
   deeltijd,
   flex,
   haven,
   hoog,
   opbouw,
   s(redding),
   thuis,
   s(welzijn)]).

n([pl(werkgeefstrs),sg(werkgeefster)],de,[]).

n([mass(werkgelegenheid)],de,[]).

n([pl(werkgemeenschappen),sg(werkgemeenschap)],de,[]).

n([pl(werkgevers),sg(werkgever)],de,[]).

n([pl(werkgeverslasten)],de,[]).

n([pl(['werkgevers-',en,werknemersorganisaties])],de,[]).

n([mass(werkhouding)],de,[]).

n([pl(werkhypothesen),pl(werkhypotheses),sg(werkhypothese)],de,[sbar,vp]).

n([pl(werkingen),sg(werking)],de,[pred_pp(in),pred_pp(buiten)]).

n([pl(werkingen),sg(werking)],de,[],[jeugd]).

n([pl(werkkringen),sg(werkkring)],de,[]).

n([mass(werkloosheid)],de,[],
  [deeltijd,
   jeugd]).

n([mass(werklust)],de,[]).

n([pl(werklieden),pl(werklui),sg(werkman)],de,[]).

n([pl(werkmetoden),pl(werkmetodes),sg(werkmetode)],de,[]).

n([pl(werknemers),sg(werknemer)],de,[]).

n([pl(werkneemsters),sg(werkneemster)],de,[]).

n([pl(werkonderbrekingen),sg(werkonderbreking)],de,[]).

n([sg(werkplek),pl(werkplekken)],de,[]).

n([pl('werkschema\'s'),sg(werkschema)],het,[]).

n([pl(werksters),sg(werkster)],de,[]).

n([pl(werkstukken),sg(werkstuk)],het,[]).

n([pl(werktuigen),sg(werktuig)],het,[vp]).

n([pl(werkuren),sg(werkuur)],het,[]).

n([pl(werkvelden),sg(werkveld)],het,[]).

n([sg(werkvloer)],de,[]).

n([mass(werkvolk)],het,[]).

n([pl(werkvormen),sg(werkvorm)],de,[]).

n([pl(werkweken),sg(werkweek)],de,[]).

n([pl(werkwijzen),sg(werkwijze)],de,[sbar,vp]).

n([pl(werkzaamheden),sg(werkzaamheid)],de,[sbar,vp]).

n([pl(werkzaamheden),sg(werkzaamheid)],de,[],[weg]).

n([pl(werpers),sg(werper)],de,[],[speer]).

n([pl(werpsters),sg(werpster)],de,[],[speer]).

n([pl(wervels),sg(wervel)],de,[],
  [hals,
   i(rug,ruggen),
   staart,
   dim(werveltje),
   tussen]).

n([pl(wervelkolommen),sg(wervelkolom)],de,[]).

n([pl(wervelstormen),sg(wervelstorm)],de,[]).

n([sg(werver),pl(wervers)],de,[],[i(fonds,fondsen)]).

n([pl(wervingen),sg(werving)],de,[]).

n([pl(wespen),sg(wesp)],de,[],
  [brons,
   schild,
   sluip]).

n([sg(wespennest),pl(wespennesten)],het,[]).

n([mass(west),
   mass('West')],both,[]).

n([mass(westen),
   mass('Westen')],het,[]).

n([pl(westerlingen),sg(westerling)],de,[]).

n([pl(westerns),sg(western)],de,[]).

n([mass(westkant)],de,[]).

n([pl(wetten),sg(wet)],de,
  [app_measure,
   sbar,
   vp,
   subject_sbar,
   subject_vp],
  [natuur]).

n([pl(wetten),sg(wet)],de,
  [app_measure],
  [herstel,
   nood]).

n([pl(wetten),sg(wet)],de,[],
  [s(arbeid),
   s(bijstand),
   concessie,
   euthanasie,
   flex,
   gemeente,
   genocide,
   grond,
   hinder,
   horeca,
   immigratie,
   kies,
   loon,
   mammoet,
   media,
   milieu,
   omroep,
   pers,
   taal,
   taxi,
   s(varken),
   i(vreemdeling,vreemdelingen),
   i(winkel_sluiting,winkelsluitings),
   woning,
   ziekte]).

n([pl(wetenschappen),sg(wetenschap)],de,[sbar]).

n([pl(wetenschappen),sg(wetenschap)],de,[],
  [communicatie,
   godsdienst,
   informatie,
   neuo,
   taal]).

n([mass(wetenschappelijkheid)],de,[]).

n([pl(wetenschappers),sg(wetenschapper)],de,[]).

n([pl(wetenschapsters),sg(wetenschapster)],de,[]).

n([mass(wetenschapsbeoefening)],de,[]).

n([pl(wetenschapslieden),pl(wetenschapsmannen),sg(wetenschapsman)],de,[]).

n([pl(wetenswaardigheden),sg(wetenswaardigheid)],de,[]).

n([pl(wetgevers),sg(wetgever)],de,[]).

n([pl(wetgevingen),sg(wetgeving)],de,[],
  [belasting,
   douane,
   milieu,
   pornografie,
   privacy,
   s(zedelijkheid)]).

n([pl(wethouders),sg(wethouder)],de,
  [app_measure],
  [h('CD'),
   h('CDA'),
   i('D66','D\'66-'),h('D66'),
   h(ex),
   h('GroenLinks'),
   h('LPF'),
   h(oud),oud,
   h('PvdA'),
   h('SP'),
   h('VVD')]).

n([pl(wetmatigheden),sg(wetmatigheid)],de,[sbar]).

n([pl(wetteksten),sg(wettekst)],de,[]).

n([pl(wettigingen),sg(wettiging)],de,[]).

n([pl(wevers),sg(wever)],de,[]).

n([pl(wezels),sg(wezel)],de,[],[dim(wezeltje)]).

n([pl(wezens),sg(wezen)],het,[sbar],[]).

n([pl(wezens),sg(wezen)],het,[],
  [hotel,
   school,
   dim(wezentje)]).

n([sg(whiplash),pl(whiplashes)],de,[]).

n([sg(whisky),pl('whisky\'s'),
   sg(whiskey),pl('whiskey\'s')],de,[]).

n([pl(wichten),sg(wicht)],het,[],[dim(wichtje)]).

n([sg(wicket),pl(wickets)],both,[]).

n([pl(wiegen),sg(wieg)],de,[],[dim(wiegje)]).

n([mass(wiegedood),mass(wiegendood)],de,[]).

n([pl(wieken),sg(wiek)],de,[]).

n([pl(wielen),sg(wiel)],het,[],
  [achter,
   muis,
   voor,
   dim(wieltje)]).

n([pl(wielewalen),sg(wielewaal)],de,[]).

n([pl(wielrenners),sg(wielrenner),
   sg(wielrenster),pl(wielrensters)],de,[]).

n([pl(wieren),sg(wier)],het,[]).

n([sg(wierde),pl(wierden),pl(wierdes)],de,[]).

n([mass(wierook)],de,[]).

n([mass(wiet)],de,[],
  [neder]).

n([sg(wifey),pl(wifeys)],het,[]).

n([mass('WiFi'),
   mass('Wi-Fi'),
   mass(wifi),
   mass('wi-fi'),
   mass('Wi-fi'),
   mass('Wifi'),
   mass('WIFI')],de,[]).   

n([pl(wiggen),sg(wig)],de,[]).

n([pl(wijdingen),sg(wijding)],de,[]).

n([sg(wijdte),pl(wijdtes)],de,[],
  [draag,
   reik,
   span,
   vleugel]).

n([pl(wijven),sg(wijf)],het,[],
  [kut,
   rot,
   dim(wijfje)]).

n([pl(wijken),sg(wijk)],de,[np_app_measure],
  [s(achterstand),
   asbest,
   buiten,
   gif,
   i(krot,krotten),
   nieuwbouw,
   probleem,
   i(slop,sloppen),
   s(stad),
   h('Vinex'),
   h('Vogelaar'),'Vogelaar',
   s(volk),
   woon]).

n([pl(wijlen),sg(wijle)],de,[temp_mod]).

n([mass(wijn),pl(wijnen),sg(wijn)],de,[],
  [dessert,
   i(hoofd_pijn,hoofdpijn),
   huis,
   s(kwaliteit),
   rijst,
   tafel,
   dim(wijntje)]).

n([pl(wijnbergen),sg(wijnberg)],de,[]).

n([mass(wijnbouw)],de,[]).

n([pl(wijnbouwers),sg(wijnbouwer)],de,[]).

n([pl('wijnfirma\'s'),sg(wijnfirma)],de,[]).

n([pl(wijngaarden),sg(wijngaard)],de,[]).

n([pl(wijnstokken),sg(wijnstok)],de,[]).

n([sg(wijs)],de,[],[dim(wijsje)]).

n([mass(wijsbegeerte)],de,[]).

n([pl(wijsgeren),sg(wijsgeer)],de,[]).

n([pl(wijsheden),sg(wijsheid)],de,[sbar]).

n([pl(wijsvingers),sg(wijsvinger)],de,[]).

n([mass(wijwater)],het,[]).

n([sg(wijze)],de,[sbar]). % de wijze waarop

n([sg(wijze)],de,[],
  [s(gunning),
   schrijf,
   s(toediening)
  ]). 

n([pl(wijzers),sg(wijzer)],de,[],[dim(wijzertje)]).

n([pl(wijzigingen),sg(wijziging)],de,
  [sbar,
   vp,
   subject_sbar],
  [s(beleid),
   i(grond_wet,grondwets),
   koers,
   s(naam),
   structuur,
   s(wet)]).

n([sg(wikkel),pl(wikkels)],de,[]).

%% van goede wil
n([mass(wil)],de,[sbar,vp,pred_pp(van)],[dim(willetje)]).

n([mass(wild)],het,[]).

n([sg([wild,card]),pl([wild,cards]),
   sg(wildcard),pl(wildcards),
   sg('wild-card'),pl('wild-cards')],de,[]).

n([sg(wildeman),pl(wildemannen)],de,[]).

n([pl(wildernissen),sg(wildernis)],de,[]).

n([pl(wilgen),sg(wilg)],de,[]).

n([mass(willekeur)],de,[]).

n([pl(wimpels),sg(wimpel)],de,[],[dim(wimpeltje)]).

n([pl(wimpers),sg(wimper)],de,[],[dim(wimpertje)]).

n([sg('win-win')],both,[]).

n([pl(winden),sg(wind)],de,[],
  [mee,
   rij,
   rug,
   ruk,
   storm,
   tegen,
   wervel,
   zee,
   zuidwesten]).

n([pl(windeieren),sg(windei)],het,[]).

n([sg(winding),pl(windingen)],de,[]).

n([pl(windrichtingen),sg(windrichting)],de,[]).

n([pl(windselen),pl(windsels),sg(windsel)],het,[],[dim(windseltje)]).

n([pl(windstilten),pl(windstiltes),sg(windstilte)],de,[]).

n([pl(windstoten),sg(windstoot)],de,[]).

n([pl(windstreken),sg(windstreek)],de,[]).

n([pl(windvlagen),sg(windvlaag)],de,[]).

n([pl(winkels),sg(winkel)],de,[],
  [avond,
   bel,
   boek,
   i(dier,dieren),
   nacht,
   i(sigaar,sigaren),
   speelgoed,
   vis,
   web,
   dim(winkeltje)]).

n([pl(winkeliers),sg(winkelier)],de,[]).

n([pl(winnaars),sg(winnaar)],de,[app_measure]).

n([pl(winnaars),sg(winnaar)],de,[],
  [beker,
   eind,
   [gouden,medaille],
   [zilveren,medaille],
   [bronzen,medaille],
   s(groep),
   medaille,
   'Tour']).

n([sg(winnares),pl(winnaressen)],de,[]).

n([pl(winningen),sg(winning)],de,[],
  [gas,
   land,
   melk,
   olie,
   zand]).

n([pl(winsten),sg(winst)],de,[subject_sbar]).

n([pl(winsten),sg(winst)],de,[],
  [s(bedrijf),
   boek,
   bruto,
   concern,
   i(half_jaar,halfjaar),
   jaar,
   koers,
   kwartaal,
   netto,
   record,
   terrein,
   tijd,
   dim(winstje)]).

n([sg(['winst-',en,verliesrekening])],de,[]).

n([mass(winstgevendheid)],de,[]).

n([mass(winstgroei)],de,[]).

n([sg(winstneming),pl(winstnemingen)],de,[]).

n([sg(winstwaarschuwing),pl(winstwaarschuwingen)],de,[]).

n([sg(winter),pl(winters)],de,[temp_mod,sbar],[dim(wintertje)]).

n([pl(winterjassen),sg(winterjas)],de,[]).

n([sg(wintermaand),pl(wintermaanden)],de,[temp_mod,sbar]).

n([mass(winterslaap)],de,[]).

n([sg(winterstop),pl(winterstoppen)],de,[]).

n([pl(wippen),sg(wip)],de,[]).

n([mass(wirwar)],de,[]).

n([mass(wiskunde)],de,[]).

n([sg(wiskundige),pl(wiskundigen)],de,[]).

n([pl(wissels),sg(wissel)],de,[],[dim(wisseltje)]).

%% geen wissel_aars
n([sg(wisselaar),pl(wisselaars)],de,[],[geld]).

n([mass(wisselbouw)],de,[]).

n([pl(wisselingen),sg(wisseling)],de,[],
  [eeuw,
   s(macht),
   millennium]).

n([mass(wisselslag)],de,[]).

n([pl(wisselwerkingen),sg(wisselwerking)],de,[]).

n([sg(wisser),pl(wissers)],de,[],[i(ruit,ruiten),
				  i(ruit,ruite)]).

n([mass(wit)],het,[],
  [h(blauw),
   h(rood),
   h(zwart)]).

n([mass(without)],het,[]).

n([mass(witkalk)],de,[]).

n([mass(witlof)],both,[]).

n([mass(witwas)],both,[]).

n([mass(wodka),mass(vodka)],de,[]).

n([mass(woede),pl(woedes),sg(woede)],de,[],
  [s(volk)]).

n([mass(woeker)],het,[]).

n([sg(woekering),pl(woekeringen)],de,[]).

n([sg(woensdag),pl(woensdagen)],de,[temp_mod,sbar]).

n([pl(woerden),sg(woerd)],de,[]).

n([pl(woestelingen),sg(woesteling)],de,[]).

n([pl(woestenijen),sg(woestenij)],de,[]).

n([pl(woestijnen),sg(woestijn)],de,[]).

n([sg(woiwode),pl(woiwodes)],de,[]).

n([sg(woiwodschap),pl(woiwodschappen)],both,[]).

n([pl(wokken),sg(wok)],de,[]).

n([mass(wol)],de,[]).

n([pl(wolven),sg(wolf)],de,[],[koren]).

n([pl(wolvinnen),sg(wolvin)],de,[],[]).  % en niet wol_vin

n([mass(wolfram)],het,[]).

n([pl(wolken),sg(wolk)],de,[measure],
  [gif,
   dim(wolkje)]).

n([pl(wolkendekken),sg(wolkendek)],het,[]).

n([pl(wolkenkrabbers),sg(wolkenkrabber)],de,[]).

n([pl(wolkenvelden),sg(wolkenveld)],het,[]).

%% munt
n([meas(won)],both,[meas_mod,measure]).

n([pl(wonden),sg(wond),sg(wonde)],de,[],
  [brand,
   i(door_lig,doorlig),
   schaaf,
   schot,
   snij
  ]).

n([pl(wonderen),sg(wonder)],het,
  [sbar,
   subject_sbar],
  [s(god)]).

n([pl(wonderkinderen),sg(wonderkind)],het,[]).

n([mass(wonderschoons)],het,[]).

n([pl(woningen),sg(woning)],de,[],
  [i(leun_aan,aanleun),
   s(ambt),
   dienst,
   doorzon,
   s(eengezin),i(eengezin,eensgezins),
   huur,
   koop,
   nieuwbouw,
   recreatie,
   rots,
   i(student,studenten),
   i(woning_wet,woningwet),
   dim(woninkje)]).

n([mass(woningbouw)],de,[]).

n([mass(woningnood)],de,[]).

n([pl(woonhuizen),sg(woonhuis)],het,[]).

n([pl(woonkeukens),sg(woonkeuken)],de,[]).

n([pl(woonlasten)],de,[]).

n([mass(woonomgeving)],de,[]).

n([sg(woonst),pl(woonsten)],de,[]).  % VL

n([pl(woorden),sg(woord)],het,
  [pred_pp(aan)],
  []).

n([pl(woorden),sg(woord)],het,
  [start_app_measure,           % met de woorden "...."
   pred_pp(aan),                % aan het woord zijn/blijven/komen
   measure],			% geen woord Spaans
                                % een woordje uitleg
  [dim(woordje)]).

n([pl(woorden),sg(woord)],het,
  [app_measure],
  [bij,
   deel,
   i(hulp_werk,hulpwerk),
   leen,
   lid,
   naam,
   scheld,
   sleutel,
   tref,
   tover,
   voeg,
   voornaam,
   wacht,
   werk,
   i(twee_letter,tweeletter),
   i(drie_letter,drieletter),
   i(vier_letter,vierletter),
   i(vijf_letter,vijfletter),
   i(zes_letter,zesletter),
   i(zeven_letter,zevenletter),
   i(acht_letter,achtletter),
   i(negen_letter,negenletter),
   dim(woordje)]).

n([pl(woorden),sg(woord)],het,[],
  [dank,
   ja,h(ja),
   s(macht)]).

n([pl(woorden),sg(woord)],het,
  [subject_sbar,
   sbar],
  [na,
   voor,
   weer,
   dim(woordje)]).

n([sg(woordenboek),pl(woordenboeken)],het,[],[leen]).

n([mass(woordenschat)],de,[]).

n([pl(woordenwisselingen),sg(woordenwisseling)],de,[]).

n([mass(woordkeus)],de,[]).

n([pl(woordspelingen),sg(woordspeling)],de,[vp]).

n([pl(woordvoerders),sg(woordvoerder)],de,[app_measure],
  [leger,
   politie,
   s(regering),
   h('VN')]).

n([pl(woordvoersters),sg(woordvoerster)],de,[],
  [leger,
   politie,
   s(regering),
   h('VN')]).

n([mass(wording)],de,[]).

n([pl(workshops),sg(workshop)],de,
  [app_measure,
   start_app_measure,
   np_app_measure]).

n([sg(workspace)],de,[]).

n([mass([world,wide,web])],het,[]).

n([pl(wormen),sg(worm)],de,[],
  [aard,
   borstel,
   lint,
   plat,
   regen,
   dim(wormpje)]).

n([pl(worpen),sg(worp)],de,[measure]).  %% een worp jongen

n([pl(worsten),sg(worst)],de,[],
  [dim(worstje),
   s(eenheid),
   lever,
   rook,
   smeer]).

n([pl(worstelaars),sg(worstelaar)],de,[]).

n([pl(worstelingen),sg(worsteling)],de,[]).

n([sg(wort)],het,[]).  %iets met gist en bier

n([pl(wortelen),pl(wortels),sg(wortel)],de,[],
  [gember,
   dim(worteltje)]).

n([pl(wouden),sg(woud)],het,[measure]).

n([pl(woudlopers),sg(woudloper)],de,[]).

n([mass([wow,'&',flutter])],de,[]).

n([mass(wraak)],de,[],
  [bloed,
   eer]).

n([mass(wraakzucht)],de,[]).

n([pl(wrakken),sg(wrak)],het,[],
  [auto,
   i(schip,scheeps)
  ]).

n([pl(wratten),sg(wrat)],de,[],[dim(wratje)]).

n([pl(wreedheden),sg(wreedheid)],de,[]).

n([pl(wreven),sg(wreef)],de,[]).

n([pl(wrevels),sg(wrevel)],de,[]).

n([pl(wrijvingen),sg(wrijving)],de,[]).

n([pl(wroegingen),sg(wroeging)],de,[]).

n([mass(wrok)],de,[]).

n([pl(wrongen),sg(wrong)],de,[]).

n([sg([wrongful,birth])],de,[]).

n([sg([wrongful,birth,claim]),
   sg([wrongful,'birth-claim'])],de,[]).

n([sg([wrongful,life])],de,[]).

n([sg([wrongful,life,claim]),
   sg([wrongful,'life-claim'])],de,[]).

n([mass(wrongel)],de,[]).

n([sg(wurm),pl(wurmen)],both,[]).

n([meas(x)],de,[temp_mod]).

n([pl('x\'en'),sg(x)],de,[]).

n([pl('x\'jes'),sg('x\'je')],het,[]).

n([pl('y\'s'),sg(y)],de,[],[dim('y\'tje')]).

n([mass(yang)],het,[]).

n([pl(yankees),sg(yankee)],de,[]).

n([sg(yard),pl(yards)],de,[meas_mod,measure]).

n([pl(yens),meas(yen)],de,[meas_mod,measure]).

n([mass(yin)],het,[]).

n([mass(yoga)],de,[]).

n([mass(yoghurt)],de,[]).

n([meas(yuan),pl(yuans)],de,[meas_mod,measure]).

n([sg(yup),pl(yuppen)],de,[]).

n([pl('yogi\'s'),pl(yogin),sg(yogi)],de,[]).

n([pl('z\'s'),sg(z)],de,[]).

n([pl('z\'jes'),sg('z\'je')],het,[]).

n([pl(zaden),sg(zaad)],het,[],
  [gras,
   maan,
   zaai,
   dim(zaadje)]).

n([pl(zaadcellen),sg(zaadcel)],de,[]).

n([pl(zaadlozingen),sg(zaadlozing)],de,[]).

n([pl(zagen),sg(zaag)],de,[]).

n([sg(zaagbek),pl(zaagbekken)],de,[]).

n([mass(zaagsel)],het,[]).

n([mass(zaai)],de,[]).

n([sg(zaaier),pl(zaaiers)],de,[]).

n([sg(zaakgelastigde),
   pl(zaakgelastigden)],de,[]).

n([pl(zaken),sg(zaak)],de,
  [subject_sbar,subject_vp],
  [bij,
   ere,
   hals,
   hoofd,
   prestige,
   principe,
   rand,
   dim(zaakje)]).

n([pl(zaken),sg(zaak)],de,
  [],
  [i(ambtenaar,ambtenaren),
   i(bloem,bloemen),
   i(broodje,broodjes),
   'doe-het-zelf',
   elektronika,
   i(elektronika,electronica),
   i(fiets,fietsen),
   fotozaak,
   geld,
   groente,
   horeca,
   s(juwelier),
   s(kapper),
   kleding,
   s(kruidenier),
   meubel,
   mode,
   i(plaat,platen),
   i(schoen,schoenen),
   shoarma,
   i(sigaar,sigaren),
   speciaal,
   speelgoed,
   sport,
   s(tabak),
   dim(zaakje)]).

n([pl(zaken),sg(zaak)],de,[],
  [arbitrage,
   i(beurs_fraude,beursfraude),
   post_h('Brongersma'),
   post_h('Bouterse'),
   corruptie,
   doping,
   drug,s(drug),
   post_h('Dutroux'),
   fraude,
   s(handel),
   i(kind_porno,kinderporno),
   moord,
   ontucht,
   s(ontvoering),
   post_h('Peper'),
   s(personeel),
   post_h('Pinochet'),
   s(recht),recht,
   straf,
   s(staat),
   post_h('Vaatstra'),
   post_wh(['Van','Cotthem']),
   post_wh(['Van',der,'G.']),
   post_wh(['Van','Driel']),
   post_wh(['Van','E']),
   post_wh(['Van','Oijen']),
   s(verkrachting),
   voetbal,
   i(voor_kennis,voorkennis),
   i(vreemdeling,vreemdelingen),
   i(zede,zeden),
   dim(zaakje)]).

n([sg(zaakvoerder),pl(zaakvoerders)],de,[]).

n([sg(zaakvoerster),pl(zaakvoersters)],de,[]).

n([pl(zalen),sg(zaal)],de,[np_app_measure],
  [bal,
   concert,
   dans,			% en geen dan_zaal
   feest,
   fuif,
   gym,
   muziek,
   parochie,
   slaap,
   studie,
   toon,
   troon,
   dim(zaaltje)]).

n([mass(zachtheid)],de,[]).

n([pl(zadels),sg(zadel)],both,[pred_pp(in)],[dim(zadeltje)]).

n([pl(zakken),sg(zak)],de,[measure],
  [dim(zakje)]).

n([pl(zakken),sg(zak)],de,[],
  [bal,
   zand,
   dim(zakje)]).

n([pl(zakdoeken),sg(zakdoek)],de,[],[dim(zakdoekje)]).

n([mass(zakelijkheid)],de,[]).

n([pl(zakenlieden),pl(zakenlui),sg(zakenman)],de,[]).

n([sg(zakkenroller),pl(zakkenrollers)],de,[]).

n([pl(zalven),sg(zalf)],de,[],[dim(zalfje)]).

n([pl(zaligheden),sg(zaligheid)],de,[]).

n([pl(zalmen),sg(zalm)],de,[],[dim(zalmpje)]).

n([mass(zand),pl(zanden),sg(zand)],het,[],
  [beton,
   metsel,
   drijf]).

n([pl(zandbakken),sg(zandbak)],de,[]).

n([pl(zangen),sg(zang)],de,[],
  [achtergrond,f([achtergrond]),
   klaag,
   f([lead]),
   i(zwaan,zwane),
   i(zwaan,zwanen)]).

n([pl(zangers),sg(zanger)],de,[],
  [pop,
   s(volk)]).

n([pl(zangeressen),sg(zangeres)],de,[],
  [pop,
   s(volk)]).

n([sg(zaterdag),pl(zaterdagen)],de,[temp_mod,sbar],
  [paas]).

n([mass(zaterdagvoetbal)],het,[]).

n([sg(zebra),pl('zebra\'s')],de,[]).

n([pl(zeden),sg(zede)],de,[sbar,vp]).

n([mass(zedelijkheid)],de,[]).

n([pl(zeeën),sg(zee)],de,[],[dim(zeetje)]).

n([pl(zeebodems),sg(zeebodem)],de,[]).

n([pl(zeven),sg(zeef)],de,[],[dim(zeefje)]).

n([sg(zeeg)],de,[]).  % iets van een oud schip?

n([pl(zeehavens),sg(zeehaven)],de,[]).

n([pl(zeehonden),sg(zeehond)],de,[]).

n([mass(zeelucht)],de,[]).

n([pl(zeelieden),pl(zeelui),sg(zeeman)],de,[]).

n([sg(zeem),pl(zemen)],de,[]).

n([pl(zeemeerminnen),sg(zeemeermin)],de,[]).

n([pl(zepen),sg(zeep)],de,[]).

n([pl(zeepbellen),sg(zeepbel)],de,[sbar,vp]).

n([mass(zeer)],het,[]).  % oud zeer; mijn voeten doen zo'n zeer

n([pl(zeerovers),sg(zeerover)],de,[]).

n([pl(zeevaarders),sg(zeevaarder)],de,[]).

n([mass(zeevaart)],de,[]).

n([mass(zeewater)],het,[]).

n([mass(zeewier)],het,[]).

n([mass(zeg)],de,[],[dim(zegje)]). % Vlaams: hun zeggen hebben/doen over

n([pl(zeges),sg(zege)],de,[sbar,vp]).

n([pl(zeges),sg(zege)],de,[],
  [eind,
   etappe,
   rit,
   toernooi]).

n([pl(zegels),sg(zegel)],both,[],[dim(zegeltje)]).

n([pl(zegelringen),sg(zegelring)],de,[]).

n([mass(zegen)],de,[subject_sbar]).

n([pl(zegeningen),sg(zegening)],de,[sbar,vp]).

n([sg(zegge),pl(zeggen),pl(zegges)],de,[]).

n([mass(zeggenschap)],de,[]).

n([pl(zegslieden),pl(zegslui),sg(zegsman)],de,[]).

n([pl(zegswijzen),sg(zegswijze)],de,[sbar]).

n([mass(zeik)],de,[pred_pp(over)]).

n([sg(zeikerd),pl(zeikerds)],de,[]).

n([pl(zeilen),sg(zeil)],het,[],[dim(zeiltje)]).

n([pl(zeilboten),sg(zeilboot)],de,[]).

n([mass(zeildoek)],het,[]).

n([sg(zeiler),pl(zeilers)],de,[],[solo]).

%% een zeilster is niet altijd een zeil_ster
n([sg(zeilster),pl(zeilsters)],de,[],[solo]).

n([pl(zeisen),sg(zeis)],de,[]).

n([pl(zekerheden),sg(zekerheid)],de,
  [sbar,
   subject_sbar,
   vp]).

n([pl(zekerheden),sg(zekerheid)],de,[],
  [energie,
   voedsel]).

n([pl(zeldzaamheden),sg(zeldzaamheid)],de,[sbar,vp]).

n([mass(zelfbeheersing)],de,[]).

n([mass(zelfbehoud)],het,[]).

n([mass(zelfbeklag)],het,[]).

n([mass(zelfbeschikking)],de,[]).

n([mass(zelfbevrediging)],de,[]).

n([mass(zelfbewustzijn)],het,[]).

n([mass(zelfdiscipline)],de,[]).

n([mass(zelfdoding)],de,[]).

n([mass(zelfgenoegzaamheid)],de,[]).

n([pl(zelfkanten),sg(zelfkant)],de,[]).

n([mass(zelfkontrole)],de,[]).

n([mass(zelfkritiek)],de,[]).

n([mass(zelfmedelijden)],het,[]).

n([pl(zelfmoorden),sg(zelfmoord)],de,[subject_vp]).

n([mass(zelfonderzoek)],het,[]).

n([mass(zelfontplooiing)],de,[]).

n([pl(zelfportretten),sg(zelfportret)],het,[]).

n([mass(zelfrespect)],het,[]).

n([mass(zelfrespekt)],het,[]).

n([mass(zelfspot)],de,[]).

n([pl(zelfstandigen),sg(zelfstandige)],de,[]).

n([pl(zelfstandigheden),sg(zelfstandigheid)],de,[]).

n([mass(zelfverdediging)],de,[]).

n([mass(zelfvernietiging)],de,[]).

n([mass(zelfvertrouwen)],het,[]).

n([mass(zelfverwerkelijking)],de,[]).

n([sg(zelfverwijt),pl(zwelfverwijten)],het,[]).

n([mass(zelfverzekerdheid)],de,[]).

n([mass(zelfwerkzaamheid)],de,[]).

n([mass(zelfzucht)],de,[]).

n([mass(zen)],both,[]).

n([pl(zendelingen),sg(zendeling)],de,[]).

n([pl(zenders),sg(zender)],de,[],
  [radio,
   satelliet,
   televisie,tv,h(tv),f([tv]),i(tv,'TV-'),
   zee,
   dim(zendertje)]).

n([pl(zendgemachtigden),sg(zendgemachtigde)],de,[]).

n([pl(zendingen),sg(zending)],de,[measure]).

n([pl(zendingen),sg(zending)],de,[],[terug]).

n([pl(zenuwen),sg(zenuw)],de,[],[dim(zenuwtje)]).

n([mass(zenuwachtigheid)],de,[]).

n([pl(zerken),sg(zerk)],de,[]).

n([sg(zes),pl(zessen)],de,[],[dim(zesje)]).

n([sg(zesbak),pl(zesbakken)],de,[]).

n([pl(zesden),pl(zesdes)],both,[]).

n([bare_meas(zesmaal),pl(zesmalen)],both,[temp_mod,measure,sbar]).

n([pl(zestigers),sg(zestiger)],de,[]).

n([pl(zetten),sg(zet)],de,[subject_sbar],
  []).

n([pl(zetten),sg(zet)],de,[],
  [begin,
   dam,
   schaak,
   tekst,
   tussen,
   dim(zetje)]).

n([pl(zetels),sg(zetel)],de,[measure,
			     meas_mod  % dalen twee zetels in de peiling
			    ],
  [hoofd,
   kamer,'Kamer',
   s(parlement),
   dim(zeteltje),
   s(raad)]).				% zes zetels winst / verlies (????)

n([mass(zetmeel)],both,[],[tarwe]).

n([sg(zetter),pl(zetters)],de,[]).

n([pl(zeugen),sg(zeug)],de,[]).

n([pl(zeuren),sg(zeur)],de,[]).

n([mass(zever)],de,[]). % Vlaams

n([pl(zevens),sg(zeven)],de,[],[dim(zeventje)]).

n([bare_meas(zevenmaal),pl(zevenmalen)],both,[temp_mod,measure,sbar]).

n([pl(zevenden),pl(zevendes)],de,[]).

n([pl(zeventigers),sg(zeventiger)],de,[]).

n([mass(zicht)],het,[pred_pp(in),
		     pred_pp(uit)]).

n([mass(zicht)],het,[],
  [tuin,
   zee]).

n([pl(ziekenboegen),sg(ziekenboeg)],de,[]).

n([sg(ziekenbroeder),pl(ziekenbroeders)],de,[]).

n([pl(ziekenfondsen),sg(ziekenfonds)],het,[]).

n([pl(ziekenhuizen),sg(ziekenhuis)],het,[],
  [i(kind,kinder),
   dim(ziekenhuisje)]).

n([sg(ziekenomroep),pl(ziekenomroepen)],de,[]).

n([pl(ziekenzalen),sg(ziekenzaal)],de,[]).

n([pl(ziekten),pl(ziektes),sg(ziekte)],de,[app_measure],
  [s(beroep),
   i(blaas_DIM,blaasjes),
   dier,
   i(geest,geestes),
   i(gek_koe,gekkekoeien),i(gek_koe,'gekke-koeien'),
   s(geslacht),
   hart,
   hersen,
   huid,
   infectie,i(infectie,infektie),
   i(kind,kinder),
   long,
   slaap,
   spier,
   suiker,
   vaat,
   vee,
   i(veteraan,veteranen),
   virus]).

n([pl(ziektekiemen),sg(ziektekiem)],de,[]).

n([pl(ziekteverschijnselen),pl(ziekteverschijnsels),sg(ziekteverschijnsel)],het,[sbar]).

n([pl(ziekteverwekkers),sg(ziekteverwekker)],de,[]).

n([mass(ziekteverzuim)],het,[]).

n([pl(zielen),sg(ziel)],de,[],[dim(zieltje)]).

n([pl(zieners),sg(ziener)],de,[]).

n([pl(zienswijzen),sg(zienswijze)],de,[sbar]).

n([mass(zier)],de,[measure],[dim(ziertje)]).

n([pl(zigeuners),sg(zigeuner)],de,[]).

n([sg(zij)],de,[]).  % met je handen in de zij

n([pl(zijden),sg(zijde)],de,[],
  [achter,
   binnen,
   boven,
   buiten,
   credit,
   debet,
   linker,
   noord,
   onder,
   oost,
   over,
   portiek,
   rechter,
   voor,
   west,
   zuid]).

n([pl(zijkanten),sg(zijkant)],de,[]).

n([pl(zijnen),pl(zijne),sg(zijne)],both,[]).

n([pl(zijramen),sg(zijraam)],het,[]).

n([sg(zijspan),pl(zijspannen)],de,[]).

n([pl(zijsporen),sg(zijspoor)],het,[]).

n([pl(zijwanden),sg(zijwand)],de,[]).

n([mass(zilver)],het,[]).

n([pl(zinnen),sg(zin)],de,
  [sbar,
   vp,
   pred_pp(naar),
   pred_pp(naar,subject_sbar),
   pred_pp_pl(bij),             % hij is weer bij zinnen gekomen
   van_sbar]).                  % in de zin van: buurman doodt buurman

n([pl(zinnen),sg(zin)],de,
  [start_app_measure,
   sbar,
   vp],
  [begin,
   bij,
   hoofd,
   slot,
   vol,
   dim(zinnetje)]).

n([sg(zin)],de,[],
  [s(realiteit)]).

n([pl(zingevingen),sg(zingeving)],de,[]).

n([mass(zink)],both,[]).

n([mass(zinkwit)],het,[]).

n([pl(zinloosheden),sg(zinloosheid)],de,[sbar,vp]).

n([mass(zinnelijkheid)],de,[]).

n([sg(zinsdeel),pl(zinsdelen)],het,[start_app_measure]).

n([pl(zinsneden),sg(zinsnede)],de,[sbar,start_app_measure]).

n([pl(zinspelingen),sg(zinspeling)],de,[sbar,vp]).

n([pl(zintuigen),sg(zintuig)],het,[]).

n([mass(zionisme)],het,[]).

n([pl(zionisten),sg(zionist)],de,[]).

n([mass(zit)],de,[],
  [s(kleermaker),
   dim(zitje)]).

n([pl(zittingen),sg(zitting)],de,[],
  [i(achter_bank,achterbank),
   s(recht)]).

n([pl(zitvlakken),sg(zitvlak)],het,[]).

%% zelfstandig naamwoord (GCRAMP)
n([sg(zn)],het,[]).

n([pl(zoden),sg(zode)],de,[]).

n([sg(zoeaaf),pl(zoeaven),
   sg(zouaaf),pl(zouaven)],de,[]).

n([sg(zoef),pl(zoeven)],de,[]).

n([pl(zoekers),sg(zoeker)],de,[],
  [asiel,
   goud]).

n([pl(zoeksters),sg(zoekster)],de,[],
  [asiel,
   goud]).

n([sg(zoem),pl(zoemen)],de,[]).

n([sg(zoemer),pl(zoemers)],de,[]).

n([pl(zoenen),sg(zoen)],de,[],
  [tong,
   dim(zoentje)]).

n([mass(zoet)],het,[],[dim(zoetje)]).  % van de overwinning

n([pl(zoetheden),sg(zoetheid)],de,[]).

n([pl(zoetigheden),sg(zoetigheid)],de,[]).

n([mass(zog)],het,[]).

n([pl(zolders),sg(zolder)],de,[],[hooi]).

n([pl(zolderingen),sg(zoldering)],de,[]).

n([sg(zombie),pl(zombies)],de,[]).

n([sg(zomer),pl(zomers)],de,[temp_mod,sbar]).

n([pl(zomerhuizen),sg(zomerhuis)],het,[],[dim(zomerhuisje)]).

n([pl(zomerjurken),sg(zomerjurk)],de,[],[dim(zomerjurkje)]).

n([sg(zomermaand),pl(zomermaanden)],de,[temp_mod,sbar]).

n([pl(zonnen),sg(zon)],de,[],[dim(zonnetje)]).

n([pl(zondaars),pl(zondaren),sg(zondaar)],de,[]).

n([sg(zondag),pl(zondagen)],de,
  [temp_mod,
   sbar],
  [koop,
   paas,
   pinkster]).

n([pl(zonden),sg(zonde)],de,[subject_sbar,subject_vp]).

n([pl(zondebokken),sg(zondebok)],de,[]).

n([pl(zonderlingen),sg(zonderling)],de,[]).

n([mass(zondeval)],de,[]).

n([mass(zondvloed)],de,[]).

n([pl(zones),sg(zone)],de,[],
  [buffer,
   euro,
   gedoog,
   i(gevaar,gevaren),
   'no-fly',f(['no-fly']),
   s(landing),
   politie,
   regen,
   tippel,
   s(veiligheid),
   s(vrijhandel)]).

n([pl(zonnebaden),sg(zonnebad)],het,[]).

n([pl(zonnebloemen),sg(zonnebloem)],de,[]).

n([pl(zonnebrillen),sg(zonnebril)],de,[]).

n([pl(zonnegoden),sg(zonnegod)],de,[]).

n([pl(zonneschermen),sg(zonnescherm)],het,[]).

n([mass(zonneschijn)],de,[]).

n([pl(zonnesteken),sg(zonnesteek)],de,[]).

n([pl(zonnestralen),sg(zonnestraal)],de,[]).

n([pl(zonsondergangen),sg(zonsondergang)],de,[]).

n([pl(zonsopgangen),sg(zonsopgang)],de,[]).

n([sg(zoo)],de,[]).

n([pl(zoogdieren),sg(zoogdier)],het,[]).

n([mass(zooi)],de,[measure],[dim(zooitje),
			     dim(zootje)]).

n([mass(zooi)],de,[],
  [kut,
   dim(zooitje),
   dim(zootje)]).

n([pl(zolen),sg(zool)],de,[],
  [schoen,
   dim(zooltje)]).

n([pl(zomen),sg(zoom)],de,[],[dim(zoompje)]).

n([pl(zonen),pl(zoons),sg(zoon)],de,[],
  [i(boer,boere),i(boer,boeren),
   schoon,
   dim(zoontje)]).

n([mass([zootje,ongeregeld])],het,[]).

n([pl(zorgen),sg(zorg)],de,
  [sbar,
   subject_sbar,
   subject_vp,
   vp],
  [kop]).

n([sg(zorg)],de,[],
  [i(arm,armen),
   i(gehandicapt,gehandicapten),
   s(gezondheid),
   i(jeugd_gezondheid,jeugdgezondheids),
   i(huis_arts,huisartsen),
   jeugd,
   kraam,
   s(kwaliteit),
   mantel,
   i(oud,ouderen),
   i(patiënt,patiënten),
   pleeg,
   politie,
   thuis,
   s(verslaving),
   ziekenhuis]).

n([pl(zorgeloosheden),sg(zorgeloosheid)],de,[vp]).

n([sg(zorgenkind),pl(zorgenkinderen)],het,[app_measure]).

n([sg(zorginstelling),pl(zorginstellingen)],de,[]).

n([sg(zorgverlof),pl(zorgverloven)],het,[]).

n([sg(zorgverzekeraar),pl(zorgverzekeraars)],de,[]).

n([mass(zorgvuldigheid)],de,[]).

n([pl(zotten),sg(zot)],de,[]).

n([pl(zottinnen),sg(zottin)],de,[]).  %VL

n([pl(zouten),sg(zout)],het,[],
  [keuken,
   strooi,
   zee,
   dim(zoutje)]).

n([pl(zoutkoepels),sg(zoutkoepel)],de,[]).

n([mass(zucht),pl(zuchten),sg(zucht)],de,[measure],[dim(zuchtje)]).

n([mass(zuid),
   mass('Zuid')],both,[]).

n([mass(zuiden),
   mass('Zuiden')],het,[]).

n([sg(zuiderling),pl(zuiderlingen)],de,[]).

n([mass(zuidkant)],de,[]).

n([mass(zuidoosten)],het,[]).

n([mass(zuidpool)],de,[]).

n([mass(zuidwesten)],het,[]).

n([mass(zuidzee)],de,[]).

n([pl(zuigelingen),sg(zuigeling)],de,[]).

n([sg(zuiger),pl(zuigers)],de,[],[zand]).

n([pl(zuilen),sg(zuil)],de,[],[dim(zuiltje)]).

n([mass(zuinigheid)],de,[pred_pp(van)]).

n([mass(zuip)],de,[]).

n([mass(zuivel)],both,[]).

n([mass(zuiverheid)],de,[]).

n([pl(zuiveringen),sg(zuivering)],de,[]).

n([mass(zult)],de,[]).

n([pl(zussen),sg(zus)],de,[],
  [half,
   schoon,
   dim(zusje)]).

n([pl(zusteren),pl(zusters),sg(zuster)],de,[],
  [hoofd,
   nacht,
   schoon,
   dim(zustertje)]).

n([pl(zuren),sg(zuur)],het,[],
  [folium,
   gallus,
   linol,
   i(mier,mieren),
   salpeter]).

n([pl(zuurgraden),sg(zuurgraad)],de,[]).

n([mass(zuurkool)],de,[]).

n([mass(zuurstof)],both,[]).

n([pl(zwaaien),sg(zwaai)],de,[]).

n([pl(zwanen),sg(zwaan)],de,[],[dim(zwaantje)]).

n([pl(zwaarden),sg(zwaard)],het,[],
  [krom]).

n([mass(zwaarmoedigheid)],de,[]).

n([mass(zwaarte)],de,[]).

n([sg(zwabber),pl(zwabbers)],de,[]).

n([pl(zwagers),sg(zwager)],de,[],[dim(zwagertje)]).

n([pl(zwakken),sg(zwak)],het,[]).

n([pl(zwakheden),sg(zwakheid)],de,[sbar,vp]).

n([mass(zwakte),pl(zwakten),pl(zwaktes),sg(zwakte)],de,[subject_sbar]).

n([pl(zwaluwen),sg(zwaluw)],de,[],[dim(zwaluwtje)]).

n([sg(zwam),pl(zwammen)],de,[],
  [oester]).

n([mass(zwang)],de,[]).

n([mass(zwangerschap),pl(zwangerschappen)],de,[],
  [f([mola])]).

n([mass(zwart)],het,[],
  [blauw,
   git,
   inkt,
   kool,
   pik]).

n([mass('zwart-wit')],het,[]).

n([sg(zwartepiet),pl(zwartepieten)],de,[]).

n([mass(zwavel)],de,[]).

n([mass(zwavelzuur)],het,[]).

n([sg(zweem)],de,[measure],[dim(zwempje)]).

n([pl(zwepen),sg(zweep)],de,[]).

n([sg(zweer),pl(zweren)],de,[],[dim(zweertje)]).

n([mass(zweet)],het,[]).

n([pl(zweetdruppels),sg(zweetdruppel)],de,[]).

n([pl(zwellingen),sg(zwelling)],de,[]).

n([pl(zwembaden),sg(zwembad)],het,[],
  [openlucht]).

n([pl(zwembroeken),sg(zwembroek)],de,[]).

n([pl(zwemmers),sg(zwemmer)],de,[]).

n([sg(zwemslag),pl(zwemslagen)],de,[]).

n([pl(zwemsters),sg(zwemster)],de,[]).

n([mass(zwendel)],de,[]).

n([sg(zwengel),pl(zwengels)],de,[]).

n([sg(zwenk),pl(zwenken)],de,[]).

n([pl(zwerfsters),sg(zwerfster)],de,[]).

n([pl(zwermen),sg(zwerm)],de,[measure],[dim(zwermpje)]).

n([pl(zwervers),sg(zwerver)],de,[]).

n([sg(zwiep),pl(zwiepen)],de,[]).

n([mass(zwier)],de,[]).

n([pl(zwijgers),sg(zwijger)],de,[]).

n([mass(zwijgzaamheid)],de,[]).

n([pl(zwijnen),sg(zwijn)],het,[sbar,vp],[dim(zwijntje)]).

n([sg(zwik),pl(zwikken)],both,[measure]).

n([mass(zwoerd)],het,[]).

n([sg('zzp\'er'),pl('zzp\'ers')],de,[]).

n([mass(élégance)],de,[]).

n([mass(öre)],de,[]).

%% weird measure stuff

n([meas(H)],both,[meas_mod,measure]) :-
    measure_noun(H).

measure_noun('Bq').
measure_noun('Bq.').

measure_noun('BRT').   % Bruto register ton

measure_noun(cl).
measure_noun('cl.').

measure_noun(cm2).
measure_noun('cm²').
measure_noun('cm³').
measure_noun(cm3).

measure_noun(dm2).
measure_noun('dm²').
measure_noun('dm³').
measure_noun(dm3).

measure_noun(mAh).
measure_noun('m-Ah').

measure_noun(mHz).

measure_noun('kJ').
measure_noun('kJ/mol').

measure_noun('kb/s').

measure_noun('kg/m<^>3').

measure_noun('km/h').
measure_noun('km/s').
measure_noun('km/u').
measure_noun('km/uur').
measure_noun('km<^>2').
measure_noun('km²').
measure_noun('km³').
measure_noun('km<^>3').
measure_noun(km2).
measure_noun(km3).

measure_noun('mijl²').
measure_noun(mph).

measure_noun(m2).
measure_noun('m²').
measure_noun(m3).
measure_noun('m³').
measure_noun('m/s').

measure_noun('mg/g').
measure_noun('mg/ml').
measure_noun('mg/km').

measure_noun(mA).

measure_noun('MHz').
measure_noun('Mhz').

measure_noun(mm2).
measure_noun('mm²').
measure_noun(mm3).

measure_noun(mtoe).  % mega ton olie equivalent

%% mega-watt-uur
measure_noun('MWh').
measure_noun('Mwh').
measure_noun('mWh').

measure_noun('µl').
measure_noun('µg').

measure_noun('£').

measure_noun('$').

measure_noun(ppm).  % parts per million

measure_noun(tpm).

measure_noun('BEF').
measure_noun('Bfr').
measure_noun('DEM').
measure_noun('EUR').
measure_noun('FF').
measure_noun('FRF').
measure_noun('GBP').
measure_noun('NLG').
measure_noun('SRG').  % Surinaamse gulden?
measure_noun('USD').

measure_noun('°').
measure_noun('°C').
measure_noun('°F').
measure_noun(['°','Celsius']).
measure_noun(['°','C']).
measure_noun(['°','C.']).
measure_noun(['°','C','.']).
measure_noun(['°','F']).
measure_noun(['°','Fahrenheit']).
measure_noun(['°','F.']).
measure_noun(['°','F','.']).
measure_noun(['°','NB']).
measure_noun(['°','OL']).
measure_noun(['°','WL']).
measure_noun(['°','ZB']).

measure_noun([o,'C']).
measure_noun([o,'C.']).

%% these are different characters (?):

measure_noun('˚').
measure_noun('˚C').
measure_noun('˚F').
measure_noun(['˚','Celsius']).
measure_noun(['˚','C']).
measure_noun(['˚','C.']).
measure_noun(['˚','C','.']).
measure_noun(['˚','Fahrenheit']).
measure_noun(['˚','F']).
measure_noun(['˚','F.']).
measure_noun(['˚','F','.']).
measure_noun(['˚','NB']).
measure_noun(['˚','OL']).
measure_noun(['˚','WL']).
measure_noun(['˚','ZB']).

%% these are different characters (?):

measure_noun('&#730;').
measure_noun('&#730;C').
measure_noun('&#730;F').
measure_noun(['&#730;','Celsius']).
measure_noun(['&#730;','C']).
measure_noun(['&#730;','C.']).
measure_noun(['&#730;','C','.']).
measure_noun(['&#730;','Fahrenheit']).
measure_noun(['&#730;','F']).
measure_noun(['&#730;','F.']).
measure_noun(['&#730;','F','.']).
measure_noun(['&#730;','NB']).
measure_noun(['&#730;','OL']).
measure_noun(['&#730;','WL']).
measure_noun(['&#730;','ZB']).



