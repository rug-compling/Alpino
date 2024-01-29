%%% lexicon of phrases
%%% times
%%% dates
%%% numbers
%%% amounts
%%%
%%% badly needs documentation
%%% badly needs good lemmatization

:- expects_dialect(sicstus).

:- discontiguous
    phrasal_entry/4,
    phrasal_entry/5.

:- use_module(alpino('src/latin1')).

%% phrasal_entry/5
phrasal_entry(Cat,Label,His,In,Out) :-
    phrasal_entry(Cat,His,In,Out),
    lists:append(Used,Out,In),
    (   Cat = with_dt(Tag,Dt)
    ->  generate_with_dt_stem(with_dt(Tag,Dt),Label)
    ;   rewrite_stem(Used,Label0),
	hdrug_util:concat_all(Label0,Label,' ')
    ).

phrasal_entry(number(rang),Stem,number_rang) -->
    { hdrug_util:debug_message(4,"numbere~n",[]) },
    n_word(NumberE),
    { dif(NumberE, drieste),
      numbere(NumberE, Stem) }.

%% de zesmiljardste aardbewoner
phrasal_entry(number(rang),Stem,number_rang) -->
    { hdrug_util:debug_message(4,"zesmiljardste~n",[]) },
    zesmiljardste(Stem).

phrasal_entry(adjective(E),Root,numberjarig) -->
    { hdrug_util:debug_message(4,"numberjarig",[]) },
    n_word(NumberJarig),
    { numberjarig(NumberJarig,E,Root) }.

phrasal_entry(nominalized_adjective,Root,numberjarig) -->
    { hdrug_util:debug_message(4,"numberjarig~n",[]) },
    n_word(NumberJarig),
    { numberjarigen(NumberJarig,Root) }.

phrasal_entry(adjective(e),Root,numbereeuwse) -->
    { hdrug_util:debug_message(4,"numbereeuwse~n",[]) },
    n_word(NumberPersoons),
    { rangnumbereeuwse(NumberPersoons,Root) }.

%% In wikipedia is formula_23 meta-notation which stands for some
%% text typeset as a mathematical symbol/formula
phrasal_entry(proper_name(sg),Word,formula_) -->
    n_word(Word),
    { formula(Word) }.

phrasal_entry(noun(het,count,pl),Stem,numbercilinders) -->
    { hdrug_util:debug_message(4,"numbercilinders~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,cilinders,_,Stem,cilinder) }.

phrasal_entry(noun(de,count,pl),Stem,numberklassers) -->
    { hdrug_util:debug_message(4,"numberklasser~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,klassers,_,Stem,klasser) }.

phrasal_entry(noun(het,count,pl),Stem,numberluik) -->
    { hdrug_util:debug_message(4,"numberluik~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,luiken,_,Stem,luik) }.

phrasal_entry(noun(het,count,pl,measure),Stem,numbertallen) -->
    { hdrug_util:debug_message(4,"numbertallen~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,tallen,_,Stem,tal) }.

phrasal_entry(noun(het,count,pl),Stem,numbertallen) -->
    { hdrug_util:debug_message(4,"numbertallen~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,tallen,_,Stem,tal) }.

phrasal_entry(noun(de,count,pl,measure),Stem,numberkaarten) -->
    { hdrug_util:debug_message(4,"numberkaarten~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,kaarten,_,Stem,kaart) }.

phrasal_entry(noun(de,count,pl),Stem,numberplussers) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,minners,_,Stem,minner) }.

phrasal_entry(noun(de,count,pl),Stem,numberplussers) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,kampen,_,Stem,kamp) }.

phrasal_entry(noun(de,count,pl),Stem,numberplussers) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,sprongen,_,Stem,sprong) }.

phrasal_entry(noun(de,count,pl),Stem,numberplussers) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,setters,_,Stem,setter) }.

phrasal_entry(noun(de,count,pl),Stem,numberwielers) -->
    { hdrug_util:debug_message(4,"numberwielers~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,wielers,_,Stem,wieler) }.

phrasal_entry(noun(de,count,pl),Stem,numberplussers) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,plussers,_,Stem,plusser) }.

phrasal_entry(noun(de,count,pl),Stem,numberplussers) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaarten),
    { numbersuffix(NumberKaarten,setters,_,Stem,setter) }.

phrasal_entry(noun(de,count,sg),Label,topnumber) -->
    { hdrug_util:debug_message(4,"topnumber~n",[]) },
    n_word(TopTien),
    { toptien(TopTien,Label) }.

phrasal_entry(Tag,Stem,num_meter_loper,[NumberMeterLoper|L],L) :-
    hdrug_util:debug_message(4,"num_meter_loper 1~n",[]),
    atom(NumberMeterLoper),
    once(alpino_unknowns:atom_split(NumberMeterLoper,'-',Number,MeterLoper)),
    atom_length(Number,Length), Length < 12,
    num_meter_loper_entry(Tag,Stem,[Number,MeterLoper|L],L,'_').

phrasal_entry(Tag,Stem,num_meter_loper,L0,L) :-
    hdrug_util:debug_message(4,"num_meter_loper 2~n",[]),
    num_meter_loper_entry(Tag,Stem,L0,L,' ').

%% verNvoudig
phrasal_entry(verb(hebben,sg1,transitive),Label,verNvoudig) -->
    { hdrug_util:debug_message(4,"verNvoudig~n",[]) },
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudig,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(hebben,sg3,transitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigt,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(hebben,inf,transitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigen,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(hebben,pl,transitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigen,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(hebben,psp,transitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigd,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(hebben,past(sg),transitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigde,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(hebben,past(pl),transitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigden,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.

phrasal_entry(verb(unacc,sg1,intransitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudig,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(unacc,sg3,intransitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigt,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(unacc,inf,intransitive),Label,verNvoudig) -->
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigen,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(unacc,psp,intransitive),Label,verNvoudig) -->
    { hdrug_util:debug_message(4,"verNvoudig~n",[]) },
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigd,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(unacc,past(sg),intransitive),Label,verNvoudig) -->
    { hdrug_util:debug_message(4,"verNvoudig~n",[]) },
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigde,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(verb(unacc,past(pl),intransitive),Label,verNvoudig) -->
    { hdrug_util:debug_message(4,"verNvoudig~n",[]) },
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigden,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.

phrasal_entry(adjective(no_e(adv)),Label,verNvoudig) -->
    { hdrug_util:debug_message(4,"verNvoudig~n",[]) },
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigd,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.
phrasal_entry(adjective(e),Label,verNvoudig) -->
    { hdrug_util:debug_message(4,"verNvoudig~n",[]) },
    n_word(VerNVoudig),
    {  atom(VerNVoudig),
       atom_concat(ver,NVoudig,VerNVoudig),
       atom_concat(N,voudigde,NVoudig),
       simple_convert_number(N,_),
       concat_stems([ver,N,voudig],Label,'')
    }.

%% huis voor huis, meter voor meter etc.
phrasal_entry(sentence_adverb,Label,x_voor_x) -->
    { hdrug_util:debug_message(4,"x voor x~n",[]) },
    n_word(X),
    n_word_preposition(Voor),
    n_word(X),
    { xl(X,TAG,Root,[],[]),
      sg_noun(TAG,_),
      hdrug_util:concat_all([Root,Voor,Root],Label,' ')
    }.

phrasal_entry(Tag,Label,spaced_letters) -->
    { hdrug_util:debug_message(4,"spaced letters~n",[]) },
    long_single_letter_sequence(Letters),
    {  (  capitals(Letters,Letters2)
       ;  Letters = Letters2
       ),
       hdrug_util:concat_all(Letters2,Atom,''),
       lexicon_(Tag,Label,[Atom],[],_,[])
    }.


%% phrasal_entry/4
phrasal_entry(postnp_adverb,bracketed_year) -->
    bracketed_year.

phrasal_entry(tmp_np,temporal_expression) -->
    { hdrug_util:debug_message(4,"temporal_expression~n",[]) },
    temporal_expression,
    opt_temporal_expression_suffix.

phrasal_entry(tmp_np,date_expression) -->
    { hdrug_util:debug_message(4,"date_expression~n",[]) },
    date_expression.

phrasal_entry(noun(de,count,sg),zwem_slag) -->
    { hdrug_util:debug_message(4,"zwem_slag~n",[]) },
    zwem_slag.

phrasal_entry(number(hoofd(Agr)),number_expression) -->
    { hdrug_util:debug_message(4,"number_expression~n",[]) },
    number_expression(Agr).

phrasal_entry(number(hoofd(Agr)),number_expression) -->
    { hdrug_util:debug_message(4,"minus,number_expression~n",[]) },
    minus,
    number_expression(Agr).

phrasal_entry(number(hoofd(pl_num)),number_sequence) -->
    { hdrug_util:debug_message(4,"number_sequence~n",[]) },
    number_sequence.

phrasal_entry(mod_noun(both,count,bare_meas),number_tiende) -->
    number_expression_breuk_tiende.

phrasal_entry(mod_noun(both,count,bare_meas),number_procent) -->
    number_expression_procent.


%% met vier nul
%% Boeing zeven zes zeven
phrasal_entry(number(hoofd(both)),number_sequence) -->
    { hdrug_util:debug_message(4,"number_sequence~n",[]) },
    small_number,
    small_number,
    small_number_seq(0).

%% WSUEA 3-komma-2
phrasal_entry(number(hoofd(pl_num)),number_komma_number) -->
    n_word(L),
    { atom(L),
      atom_concat(Num1,Rest,L),
      atom_concat('-komma-',Num2,Rest),
      atom_codes(Num1,Num1Codes),
      number_codes_silent(_,Num1Codes),
      atom_codes(Num2,Num2Codes),
      number_codes_silent(_,Num2Codes)
    }.

%% WSUEA 3-tiende
phrasal_entry(number(hoofd(pl_num)),number_tiende) -->
    n_word(L),
    { atom(L),
      atom_concat(Num1,'-tiende',L),
      atom_codes(Num1,Num1Codes),
      number_codes_silent(_,Num1Codes)
    }.

number_expression_procent -->
    n_word(VierProcent),
    {  atom(VierProcent),
       atom_concat(Vier,'%',VierProcent),
       isa_number(_,[Vier],[])
    }.

number_sequence -->
    first_number,
    middle_number,
    number_sequence_rest(0).

number_sequence_rest(N0) -->
    {  N0 < 5,
       N is N0 + 1 },
    middle_number,
    number_sequence_rest(N).
number_sequence_rest(_) --> [].

first_number -->
    n_word(L),
    { atom(L),
      atom_codes(L,Codes), length(Codes,Len), Len < 4,
      first_number(Codes) }.

middle_number -->
    n_word(L),
    { atom(L),
      atom_codes(L,[A,B,C]), 
      middle_number([A,B,C]) }.

first_number([I]) :-
    isdigit(I).
first_number([I,J]) :-
    isdigit(I),
    isdigit(J).
first_number([I,J,K]) :-
    isdigit(I),
    isdigit(J),
    isdigit(K).

middle_number([I,J,K]) :-
    isdigit(I),
    isdigit(J),
    isdigit(K).

minus --> n_word(min).
minus --> n_word(minus).
minus --> n_word(plus).
minus --> n_word('+').

%% in artikel 12 bis wordt uitgelegd ...
phrasal_entry(number(hoofd(Agr)),number_expression) -->
    { hdrug_util:debug_message(4,"number_expression,number_rest~n",[]) },
    number_expression(Agr),
    number_rest.

%% gebruikt in P-P-G (wetsteksten etc)

number_rest -->
    n_word(Word),
    {  number_rest(Word) }.

number_rest(semel).
number_rest(bis).
number_rest(quater).
number_rest(quinquies).
number_rest(sexies).
number_rest(septies).
number_rest(octies).
number_rest(nonies).
number_rest(decies).

%% lijn 12B
phrasal_entry(number(hoofd(both)),number_expression) -->
    { hdrug_util:debug_message(4,"number_expression lijn 12B~n",[]) },
    n_word(NumberA),
    { atom(NumberA),
      atom_codes(NumberA,NumberCodes), 
      all_digits_but_last(NumberCodes)
    }.

all_digits_but_last([D,D1|Ds]) :-
    isdigit(D),
    all_digits_but_last(Ds,D1).

all_digits_but_last([],D) :-
    \+ isdigit(D).
all_digits_but_last([D|Ds],D0) :-
    isdigit(D0),
    all_digits_but_last(Ds,D).

%% HACK
%% HACK; todo: 2 meter bij 20 centimeter
%% alleen lengtematen?
phrasal_entry(with_dt(number(hoofd(pl_num)),
                      dt(conj,
                         [crd=l(bij,conj(en),vg,P1,P2),
                          cnj=l(N1,number(hoofd(pl_num)),detp,0,P1),
                          cnj=l(N2,number(hoofd(pl_num)),detp,P2,P)
                          ])),
                      number_expression) -->
    { hdrug_util:debug_message(4,"x bij x~n",[]) },
    number_with_words(N1,0,P1),
    n_word(bij),
    {  P2 is P1 + 1 },
    number_with_words(N2,P2,P).

phrasal_entry(with_dt(number(hoofd(pl_num)),
                      dt(conj,
                         [crd=l(bij,conj(en),vg,P1,P2),
                          crd=l(bij,conj(en),vg,P3,P4),
                          cnj=l(N1,number(hoofd(pl_num)),detp,0,P1),
                          cnj=l(N2,number(hoofd(pl_num)),detp,P2,P3),
                          cnj=l(N2,number(hoofd(pl_num)),detp,P4,P)
                          ])),
                      number_expression) -->
    { hdrug_util:debug_message(4,"x bij x 2~n",[]) },
    number_with_words(N1,0,P1),
    n_word(bij),
    {  P2 is P1 + 1 },
    number_with_words(N2,P2,P3),
    n_word(bij),
    {  P4 is P3 + 1 },
    number_with_words(N2,P4,P).

phrasal_entry(with_dt(number(hoofd(pl_num)),
		      dt(pp,[hd=l(tussen,preposition(tussen,[]),0,1),
			obj1=dt(conj,[crd=l(en,conj(en),vg,P2,P3),
				      cnj=l(N1,number(hoofd(pl_num)),detp,1,P2),
				      cnj=l(N2,number(hoofd(pl_num)),detp,P3,P4)
				      ])])),tussen_number) -->
    { hdrug_util:debug_message(4,"tussen x en y~n",[]) },
    n_word(tussen),
    number_with_words_no_year(N1,1,P2),
    n_word(en),
    { P3 is P2 + 1 },
    number_with_words_no_year(N2,P3,P4).

phrasal_entry(
  with_dt(number(hoofd(pl_num)),
	  dt(pp,[hd=l(tussen,preposition(tussen,[]),0,1),
		 obj1=dt(conj,
			 [crd=l(en,conj(en),vg,P2,P3),
			  cnj=dt(np,[det=l(de,determiner(de),detp,1,2),
				     hd=l(N1,number(hoofd(pl_num)),2,P2)]),
			  cnj=dt(np,[det=l(de,determiner(de),detp,P3,P4),
				     hd=l(N2,number(hoofd(pl_num)),P4,P5)])
			 ])])),tussen_number) -->
    { hdrug_util:debug_message(4,"tussen de x en y~n",[]) },
    n_word(tussen),
    n_word(de),
    number_with_words(N1,2,P2),
    n_word(en),
    n_word(de),
    { P3 is P2 + 1,
      P4 is P3 + 1 },
    number_with_words(N2,P4,P5).

phrasal_entry(
  with_dt(number(hoofd(pl_num)),
	  dt(pp,[hd=l(tussen,preposition(tussen,[]),0,1),
		 obj1=dt(conj,
			 [crd=l(en,conj(en),vg,P2,P3),
			  cnj=dt(np,[det=ix(X,l(de,determiner(de),detp,1,2)),
				     hd=l(N1,number(hoofd(pl_num)),2,P2)]),
			  cnj=dt(np,[det=ix(X),
				     hd=l(N2,number(hoofd(pl_num)),P3,P5)])
			 ])])),tussen_number) -->
    { hdrug_util:debug_message(4,"tussen de x en y 2~n",[]) },
    n_word(tussen),
    n_word(de),
    number_with_words(N1,2,P2),
    n_word(en),
    { P3 is P2 + 1 },
    number_with_words(N2,P3,P5).

phrasal_entry(with_dt(number(hoofd(pl_num)),
		      dt(conj,[cnj=l(N1,number(hoofd(pl_num)),detp,0,1),
			       cnj=l(N2,number(hoofd(pl_num)),detp,2,3)
			      ])),num_dash_num) -->
    n_word(N1),
    { parse_number(N1,_) },
    n_word('-'),
    n_word(N2),
    { parse_number(N2,_) }.	    

number_with_words(Ws,P0,P,W0,W) :-
    findall(W0/W,number_expression(_,W0,W),List0),
    sort(List0,List),
    lists:member(W0/W,List),
    lists:append(Ws0,W,W0),
    length(Ws0,Len),
    hdrug_util:concat_all(Ws0,Ws,' '),
    P is Len+P0.

number_with_words_no_year(Ws,P0,P,W0,W) :-
    number_with_words(Ws,P0,P,W0,W),
    \+ date_year(W0,W).

phrasal_entry(np(year),date_year) -->
    { hdrug_util:debug_message(4,"date_year~n",[]) },
    date_year.

phrasal_entry(np(year),date_year) -->
    { hdrug_util:debug_message(4,"date_double_year~n",[]) },
    date_double_year.

phrasal_entry(np(year),date_year) -->
    { hdrug_util:debug_message(4,"date_year voor_chr~n",[]) },
    number_expression(_),
    voor_chr.

phrasal_entry(np,opening_hours) -->
    { hdrug_util:debug_message(4,"date_opening_hours~n",[]) },
    date_opening_hours.

phrasal_entry(with_dt(pp(tussen),
		      dt(pp,[hd=l(tussen,preposition(tussen,[]),pp,0,1),
			     obj1=dt(conj,[cnj=l(nu,tmp_adverb,advp,1,2),
					   crd=l(en,conj(en),conj,2,3),
					   cnj=l(Year,np(year),np,3,4)
					  ])
			    ]
			)
		     ), tussen_nu_en_year) -->
    { hdrug_util:debug_message(4,"date_opening_hours~n",[]) },
    n_word(tussen),
    n_word(nu),
    n_word(en),
    simple_year(Year).

phrasal_entry(Tag,amount) -->
    { hdrug_util:debug_message(4,"amount~n",[]) },
    amount,
    {   Tag = amount_meas_mod_noun(both,count,bare_meas)
    ;   Tag = amount_meas_mod_noun(both,count,bare_meas,measure)
    }.

phrasal_entry(Tag,num_meter_num) -->
    { hdrug_util:debug_message(4,"num_meter_num~n",[]) },
    num_meter_num,
    {   Tag = meas_mod_noun(both,count,bare_meas)
    ;   Tag = meas_mod_noun(both,count,bare_meas,measure)
    }.

%% de nummer 1 hit
%% de top 10 notering
%% should this be its own category instead of score_cat?
phrasal_entry(pre_np_adverb,pre_np_adverb) -->
    { hdrug_util:debug_message(4,"nummer 1 hit~n",[]) },
    n_word(nummer),
    number_expression(_).

phrasal_entry(pre_np_adverb,pre_np_adverb) -->
    { hdrug_util:debug_message(4,"nummer één hit~n",[]) },
    n_word(nummer),
    n_word(één).

phrasal_entry(pre_np_adverb,pre_np_adverb) -->
    { hdrug_util:debug_message(4,"nummer een hit~n",[]) },
    n_word(nummer),
    n_word(een).

phrasal_entry(pre_np_adverb,pre_np_adverb) -->
    { hdrug_util:debug_message(4,"top 10~n",[]) },
    n_word(top),
    number_expression(_).

phrasal_entry(pre_np_adverb,pre_np_adverb) -->
    { hdrug_util:debug_message(4,"pre_np_adverb~n",[]) },
    n_word(type),
    (  number_expression(_)
    ;  enumeration
    ).

phrasal_entry(pre_np_adverb,pre_np_adverb) -->
    { hdrug_util:debug_message(4,"toptien~n",[]) },
    n_word(TopTien),
    { toptien(TopTien,_Label) }.

phrasal_entry(score_cat,score) -->
    { hdrug_util:debug_message(4,"number_dash_number~n",[]) },
    n_word(Score),
    { number_dash_number(Score) }.

% often mis-tokenized...
phrasal_entry(score_cat,score) -->
    { hdrug_util:debug_message(4,"score_cat~n",[]) },
    n_word(Score),
    { number_dash(Score) },
    isa_number(_).

phrasal_entry(score_cat,score) -->
    { hdrug_util:debug_message(4,"score_cat 2~n",[]) },
    n_word(Home),
    n_word('-'),
    n_word(Away),
    {  parse_number_simple(Home),
       parse_number_simple(Away)
    }.

phrasal_entry(np,telephone) -->
    { hdrug_util:debug_message(4,"telephone~n",[]) },
    telephone.

phrasal_entry(np,chess) -->
    { hdrug_util:debug_message(4,"chess_game~n",[]) },
    chess_game.

phrasal_entry(np,chess) -->
    { hdrug_util:debug_message(4,"chess_move~n",[]) },
    chess_move.

phrasal_entry(np,bridge) -->
    { hdrug_util:debug_message(4,"bridge_hand~n",[]) },
    bridge_hand.

phrasal_entry(np,bridge) -->
    { hdrug_util:debug_message(4,"bridge_hand,bridge_bidding~n",[]) },
    bridge_hand,
    bridge_bidding.

phrasal_entry(np,bridge) -->
    { hdrug_util:debug_message(4,"bridge_bidding~n",[]) },
    bridge_bidding.

phrasal_entry(np,bridge) -->
    { hdrug_util:debug_message(4,"bridge_bit_outcome~n",[]) },
    bridge_bid_outcome.

phrasal_entry(np,bridge) -->
    { hdrug_util:debug_message(4,"bridge_play_card~n",[]) },
    bridge_play_card.

%% start of diagram; VK 200?
phrasal_entry(np,bridge) -->
    n_word(west),
    n_word(noord),
    n_word(oost),
    n_word(zuid).

phrasal_entry(np,bridge) -->
    n_word(westnoordoostzuid).

%% met de troeven drie-drie
%% met de drie-drie verdeelde troeven
%% de troeven zitten drie-drie
phrasal_entry(adjective(no_e(adv)),bridge) -->
    { hdrug_util:debug_message(4,"drie-drie~n",[]) },
    n_word(NumNum),
    {  atom(NumNum),
       atom_codes(NumNum,String), length(String,Len), Len < 25,
       drie_drie(String,[])
    }.

drie_drie -->
    drie,
    "-",
    drie.

drie --> "nul".
drie --> "een".
drie --> "twee".
drie --> "drie".
drie --> "vier".
drie --> "vijf".
drie --> "zes".
drie --> "zeven".

%% twaalf miljoenste bezoeker
phrasal_entry(number(rang),number_rang) -->
    { hdrug_util:debug_message(4,"twaalf miljoenste~n",[]) },
    number_expression(_),
    [Rang],
    { hoge_rang(Rang) }.

phrasal_entry(number(rang),number_rang) -->
    { hdrug_util:debug_message(4,"16e~n",[]) },
    [Rang],
    [e],
    { parse_number_simple(Rang) }.

zesmiljardste(Stem) -->
    [Word],
    { atom(Word),
      hoge_rang(Miljardste),
      atom_concat(Zes,Miljardste,Word),
      number_expression(_,[Zes],[]),
      concat_stems([Zes,Miljardste],Stem,'_')
    }.

%% TODO: add proper stems to some!

phrasal_entry(adjective(no_e(nonadv)),numberjarig) -->
    { hdrug_util:debug_message(4,"number jarig~n",[]) },
    number_expression_word,
    n_word(Jarig),
    { jarig(Jarig) }.

phrasal_entry(adjective(no_e(nonadv)),numberjarig) -->
    { hdrug_util:debug_message(4,"16 jarig~n",[]) },
    number_expression(_),
    n_word(Jarig),
    { jarig(Jarig) }.

phrasal_entry(adjective(e),Stem,numberjarig,P0,P) :-
    hdrug_util:debug_message(4,"16 jarige~n",[]),
    number_expression_word(P0,P1),
    lists:append(Pref,P1,P0),
    n_word(Jarige,P1,P),
    jarige(Jarige,Jarig),
    lists:append(Pref,[Jarig],Stems),
    hdrug_util:concat_all(Stems,Stem,' ').

phrasal_entry(adjective(both(nonadv)),numberpersoons) -->
    { hdrug_util:debug_message(4,"numberpersoons~n",[]) },
    n_word(NumberPersoons),
    { numberpersoons(NumberPersoons) }.

phrasal_entry(adjective(both(nonadv)),numberpersoons) -->
    { hdrug_util:debug_message(4,"numberpersoons 2~n",[]) },
    n_word(NumberPersoons),
    { rangnumberpersoons(NumberPersoons) }.

phrasal_entry(adjective(both(nonadv)),numberpersoons) -->
    { hdrug_util:debug_message(4,"numberpersoons 3~n",[]) },
    n_word(NumberPersoons),
    { rangnumberpersoons(NumberPersoons) }.

phrasal_entry(nominalized_adjective,numberpersoons) -->
    { hdrug_util:debug_message(4,"numberpersoons 4~n",[]) },
    n_word(Number),
    { rang(Number) },
    n_word(Persoons),
    { rangpersoons(Persoons) }.

phrasal_entry(adjective(both(nonadv)),numberpersoons) -->
    { hdrug_util:debug_message(4,"numberpersoons 5~n",[]) },
    n_word(Number),
    { rang(Number) },
    n_word(Persoons),
    { rangpersoons(Persoons) }.

phrasal_entry(adjective(both(adv)),numberklas) -->
    { hdrug_util:debug_message(4,"numberklas 1~n",[]) },
    n_word(NumberKlas),
    { rangnumberklas(NumberKlas) }.

phrasal_entry(adjective(both(adv)),numberklas) -->
    { hdrug_util:debug_message(4,"numberklas 2~n",[]) },
    n_word(NumberKlas),
    { rangnumberklas(NumberKlas) }.

phrasal_entry(adjective(both(adv)),numberklas) -->
    { hdrug_util:debug_message(4,"numberklas 3~n",[]) },
    n_word(Number),
    { rang(Number) },
    n_word(Klas),
    { rangklas(Klas) }.

phrasal_entry(adjective(both(adv)),numberklas) -->
    n_word(Economy),
    { economy(Economy) },
    n_word(class).

phrasal_entry(adjective(no_e(nonadv)),numbereeuws) -->
    { hdrug_util:debug_message(4,"numbereeuws~n",[]) },
    n_word(NumberPersoons),
    { rangnumbereeuws(NumberPersoons) }.

phrasal_entry(adjective(no_e(nonadv)),numbereeuws) -->
    { hdrug_util:debug_message(4,"numbereeuws~n",[]) },
    n_word(Rang),
    { rang(Rang) },
    n_word(Eeuws),
    { rangeeuws(Eeuws,_) }.

phrasal_entry(adjective(e),numbereeuwse) -->
    { hdrug_util:debug_message(4,"numbereeuwse~n",[]) },
    n_word(Rang),
    { rang(Rang) },
    n_word(Eeuwse),
    { rangeeuws(_,Eeuwse) }.

phrasal_entry(noun(De,count,sg),numberklasser) -->
    { hdrug_util:debug_message(4,"numberklasser~n",[]) },
    n_word(RangKlasser),
    {  rangklasser(RangKlasser,De)  }.

phrasal_entry(noun(De,count,pl),numberklassers) -->
    { hdrug_util:debug_message(4,"numberklassers~n",[]) },
    n_word(RangKlasser),
    {  rangklassers(RangKlasser,De)  }.

phrasal_entry(noun(het,count,sg,measure),numbertal) -->
    { hdrug_util:debug_message(4,"numbertal~n",[]) },
    n_word(NumberTal),
    { NumberTal \== elftal,
      numbersuffix(NumberTal,tal) }.

phrasal_entry(noun(het,count,sg),numbertal) -->
    { hdrug_util:debug_message(4,"numbertal~n",[]) },
    n_word(NumberTal),
    { NumberTal \== elftal,
      numbersuffix(NumberTal,tal) }.

phrasal_entry(with_dt(determiner(pl_num,nwh,nmod,pro,yparg),
                      dt(np,[det=l(een,determiner(een),detp,0,1),
                             hd=l(NumberTal,noun(het,count,sg),1,2)])),
              numbertal) -->
    { hdrug_util:debug_message(4,"een drie tal~n",[]) },
    n_word(een),
    n_word(NumberTal),
    { NumberTal \== elftal,
      numbersuffix(NumberTal,tal) }.

phrasal_entry(noun(de,count,sg),numberdeurs) -->
    { hdrug_util:debug_message(4,"numberdeurs~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,deurs) }.

phrasal_entry(noun(de,count,sg),numberdeurs) -->
    { hdrug_util:debug_message(4,"numberdeurs~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,drs) }.

phrasal_entry(noun(both,count,sg),numbercilinder) -->
    { hdrug_util:debug_message(4,"numbercilinder~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,cilinder) }.

phrasal_entry(noun(de,count,sg),numberklasser) -->
    { hdrug_util:debug_message(4,"numberklasser~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,klasser) }.

phrasal_entry(noun(het,count,sg),numberluik) -->
    { hdrug_util:debug_message(4,"numberluik~n",[]) },
    n_word(NumberTal),
    { numbersuffix(NumberTal,luik) }.

phrasal_entry(noun(de,count,sg,measure),numberkaart) -->
    { hdrug_util:debug_message(4,"numberkaart~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,kaart) }.

phrasal_entry(noun(de,count,sg),numberkaart) -->
    { hdrug_util:debug_message(4,"numberkaart~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,kaart) }.

phrasal_entry(noun(de,count,sg),numberwieler) -->
    { hdrug_util:debug_message(4,"numberwieler~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,wieler) }.

phrasal_entry(noun(de,count,sg),numberplusser) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,plusser) }.

phrasal_entry(noun(de,count,sg),numberplusser) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,minner) }.

phrasal_entry(noun(de,count,sg),numberplusser) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,kamp) }.

phrasal_entry(noun(de,count,sg),numberplusser) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,sprong) }.

phrasal_entry(noun(de,count,sg),numberplusser) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,setter) }.

phrasal_entry(noun(de,count,sg),numberplusser) -->
    { hdrug_util:debug_message(4,"numberplusser~n",[]) },
    n_word(NumberKaart),
    { numbersuffix(NumberKaart,klapper) }.

phrasal_entry(nominalized_adjective,numberjarig) -->
    { hdrug_util:debug_message(4,"numberjarig~n",[]) },
    number_expression_word,
    n_word(Jarigen),
    { jarigen(Jarigen,_) }.

phrasal_entry(nominalized_adjective,numberjarig) -->
    { hdrug_util:debug_message(4,"numberjarig~n",[]) },
    number_expression(_),
    n_word(Jarigen),
    { jarigen(Jarigen,_) }.

wekelijks(wekelijks).
wekelijks(jaarlijks).
wekelijks(talig).  % +ADV: tweetalig opvoeden
wekelijks(voudig). % +ADV: tweevoudig gelinkte lijsten

jarig(daags).
jarig(delig).
jarig(dimensionaal).
jarig(hoekig).
jarig(jarig).
jarig(koppig).
jarig(ledig).
jarig(maands).
jarig(motorig).
jarig(regelig).
jarig(urig).
jarig(vleugelig).
jarig(weeks).
jarig(zijdig).

jarige(daagse,daags).
jarige(delige,delig).
jarige(dimensionale,dimensionaal).
jarige(hoekige,hoekig).
jarige(jaarlijkse,jaarlijks).
jarige(jarige,jarig).
jarige(koppige,koppig).
jarige(ledige,ledig).
jarige(maandse,maands).
jarige(motorige,motorig).
jarige(regelige,regelig).
jarige(talige,talig).
jarige(urige,urig).
jarige(vleugelige,vleugelig).
jarige(voudige,voudig).
jarige(weekse,weeks).
jarige(wekelijkse,wekelijks).
jarige(zijdige,zijdig).

jarigen(jarigen,jarig).

persoons(baans).
persoons(cilinder).
persoons(deurs).
persoons(drs).  % 3drs voor driedeurs etc
persoons(draads).
persoons(jaars).
persoons(karaats).
persoons(kleps).
persoons(lijns).
persoons(mans).
persoons(pits).
persoons(persoons).
persoons(traps).

persoons(sterren).

%% as persoons, but can be used as noun

toptien(TopTien,Label) :-
    atom(TopTien),
    atom_concat('top-',Tien,TopTien),!,
    isa_number(_,[Tien],[]),
    concat_stems([top,Tien],Label,'_').

toptien(TopTien,Label) :-
    atom(TopTien),
    atom_concat('Top-',Tien,TopTien),!,
    isa_number(_,[Tien],[]),
    concat_stems([top,Tien],Label,'_').

toptien(TopTien,Label) :-
    atom(TopTien),
    atom_concat(top,Tien,TopTien),
    \+ atom_concat('_',_,Tien),
    isa_number(_,[Tien],[]),
    concat_stems([top,Tien],Label,'_').

toptien(TopTien,Label) :-
    atom(TopTien),
    atom_concat('Top',Tien,TopTien),
    isa_number(_,[Tien],[]),
    concat_stems([top,Tien],Label,'_').

rangnumbersuffix(Word,Suffix) :-
    atom(Word),
    atom_concat(Number,Suffix,Word),
    rang(Number).
rangnumbersuffix(Word,Suffix) :-
    atom(Word),
    atom_concat('-',Suffix,Persoonsen),
    atom_concat(Number,Persoonsen,Word),
    rang(Number).

numbersuffix(Word,Suffix) :-
    numbersuffix(Word,Suffix,_).

numbersuffix(Word,Suffix,Number) :-
    atom(Word),
    atom_concat('-',Suffix,Persoonsen),
    atom_concat(Number,Persoonsen,Word),
    isa_number(_,[Number],[]),
    !.
numbersuffix(Word,Suffix,Number) :-
    atom(Word),
    atom_concat(Number,Suffix,Word),
    isa_number(_,[Number],[]).

numbersuffix(Word,Suffix,Number,Stem,StemSuffix) :-
    atom(Word),
    alpino_util:split_atom(Word,"-",[Number,Suffix]),
    isa_number(_,[Number],[]),
    !,
    hdrug_util:concat_all([Number,StemSuffix],Stem,'-').
numbersuffix(Word,Suffix,Number,Stem,StemSuffix) :-
    atom(Word),
    atom_concat(Number,Suffix,Word),
    isa_number(_,[Number],[]),
    atom_concat(Number,StemSuffix,Stem).

numberpersoons(Persoons) :-
    persoons(Suffix),
    numbersuffix(Persoons,Suffix).

%numberklas(Persoons) :-
%    klas(Suffix),
%    numbersuffix(Persoons,Suffix).

rangnumberpersoons(Persoons) :-
    rangpersoons(Suffix),
    rangnumbersuffix(Persoons,Suffix).

rangnumberklas(Persoons) :-
    rangklas(Suffix),
    rangnumbersuffix(Persoons,Suffix).

rangklasser(Form,De) :-
    klasser(Suffix,_,De),
    rangnumbersuffix(Form,Suffix).

rangklassers(Form,De) :-
    klasser(_,Suffix,De),
    rangnumbersuffix(Form,Suffix).
rangklassers(Form,de) :-
    rangnumbersuffix(Form,jaars). % only PL

klasser(klasser,klassers,de).

rangpersoons(graads).
rangpersoons(hands).
rangpersoons(jaars).
rangpersoons(lijns).
rangpersoons(rangs).

rangklas(klas).

rangnumbereeuws(Persoons) :-
    rangeeuws(Suffix,_),
    rangnumbersuffix(Persoons,Suffix).

rangnumbereeuwse(Persoons,Stem) :-
    rangeeuws(SuffixL,Suffix),
    rangnumbersuffix(Persoons,Suffix),
    atom_concat(Prefix,Suffix,Persoons),
    atom_concat(Prefix,SuffixL,Stem).

rangeeuws(eeuws,eeuwse).

numberjarigen(Jarig,Root) :-
    jarigen(Jarigen,R1),
    numbersuffix(Jarig,Jarigen,R2),
    concat_stems([R2,R1],Root,'_').

numberjarig(Jarig,e,Root) :-
    jarige(Jarige,R1),
    numbersuffix(Jarig,Jarige,R2),
    concat_stems([R2,R1],Root,'_').

numberjarig(Jarig,no_e(nonadv),Root) :-
    jarig(JarigWord),
    numbersuffix(Jarig,JarigWord,R2),
    concat_stems([R2,JarigWord],Root,'_').

numberjarig(Jarig,no_e(adv),Root) :-
    wekelijks(JarigWord),
    numbersuffix(Jarig,JarigWord,R2),
    concat_stems([R2,JarigWord],Root,'_').

numbere(NumberE) :-
    numbere(NumberE,_).

numbere(NumberE,Number) :-
    atom(NumberE),
    atom_concat(Number,e,NumberE),
    isa_number(_,[Number],[]).

numbere(NumberE,Number) :-
    atom(NumberE),
    atom_concat(Number,de,NumberE),
    isa_number(_,[Number],[]).

numbere(NumberE,Number) :-
    atom(NumberE),
    atom_concat(Number,ste,NumberE),
    isa_number(_,[Number],[]).

amount -->
    n_word(Flo),
    { start_amount(Flo) },
    number_expression(_),
    opt_miljoen.

amount -->
    n_word(Flo),
    { atom(Flo),
      start_amount(Pref),
      atom_concat(Pref,Suffix,Flo),
      number_expression(_,[Suffix],[])
    },
    opt_miljoen.

%% 14 euro 20
amount -->
    number_expression(_),
    n_word(Amount),
    { mid_amount(Amount) },
    number_expression(_).

amount -->
    n_word(Word),
    { atom(Word),
      graad(Graad),
      atom_concat(Pref,Graad,Word), 
      number_expression(_,[Pref],[])
    }.

graad('°C').
graad('°C.').
graad('°c').
graad('°c.').

%% Mediargus:
graad('^dln').
graad('gr.').
graad('^kg').
graad('^km').
graad('^m').
graad('^ptn').
graad('^u').

%% ad2004
graad(kg).
graad(gr).
graad(ltr).
graad(ons).
graad(st).
graad(ml).

%% guldens
graad(',-').
graad(',--').

opt_miljoen -->
    n_word(Miljoen),
    {  opt_miljoen(Miljoen)  }.
opt_miljoen --> [].

opt_miljoen(mln).
opt_miljoen('mln.').
opt_miljoen(miljoen).
opt_miljoen(mld).
opt_miljoen('mld.').
opt_miljoen('mrd.').
opt_miljoen('mrd').
opt_miljoen(miljard).

mid_amount(euro).
mid_amount(gulden).

start_amount('â').
start_amount(dll).
start_amount('dll.').
start_amount(flo).
start_amount('fl.').
start_amount('f.').
start_amount(f).
start_amount(fl).
start_amount('Fl').
start_amount('Flo').
start_amount('Fl.').
start_amount('F.').
start_amount('F').
start_amount('FL').
start_amount('FL.').
start_amount('&#402;').   % florin sign in unicode
start_amount('ƒ').  % florin sign in unicode
start_amount('$').
start_amount('US $').
start_amount('DM').
start_amount('USD').
start_amount('£').
start_amount('C=').      % ad2000 ???
start_amount('EURO=').   % ad2000 ???
start_amount(e).         % parool 2003
start_amount('E').       % more euro's...
start_amount('&#8364;'). % unicode euro
start_amount('€'). % unicode euro
start_amount('¤').       % if the input is latin-9
start_amount('Euro').
start_amount(euro).      % dcoi
start_amount(eu).      % twitter
start_amount('NLG').
start_amount('BEF').
start_amount('BF').
start_amount('EUR').
start_amount('').  % gulden in volkskrant 1997

:- dynamic found_number_expression/3.
:- thread_local found_number_expression/3.
number_expression(Type,Ws0,Ws) :-
    term_hash(Ws0,Ix),
    (   found_number_expression(Ix,Ws0,List)
    ->  true
    ;   findall(Type0/Ws,number_expressionXX(Type0,Ws0,Ws),List0),
	sort(List0,List),
	noclp_assertz(found_number_expression(Ix,Ws0,List))
    ),
    lists:member(Type/Ws,List).

%% 30 duizend
number_expressionXX(Agr) -->
    isa_number(Agr0),
    number_continue(Agr0,Agr).

number_expressionXX(pl_num) -->
    number_expression_small_honderd,
    komma,
    number_expression_tien(_),
    n_word(W),
    { duizend(W) }.

%% WS-U...
%% 30-duizend
number_expressionXX(pl_num) -->
    n_word(Xduizend),
    {  atom(Xduizend),
       atom_concat(X,'-duizend',Xduizend),
       number_expression_small_honderd([X],[])
    }.

number_expressionXX(pl_num) -->
    n_word(Xduizend),
    {  atom(Xduizend),
       atom_concat(X,'-honderd',Xduizend),
       number_expression_tien(_,[X],[])
    }.

%% driekwart van de mensen heeft/hebben geen geld
number_expressionXX(both) -->
    number_expression_breuk.

%% veertig duizend
number_expressionXX(pl_num,L,M) :-
    number_expression_word(L,M),
    L \== [een|M],   % reduce ambiguities, too often it's just a determiner...
    L \== [één|M],
    L \== ['1'|M],
    L \== ['eén'|M],
    L \== ['Eén'|M].

number_continue(Agr,Agr) --> [].

number_continue(_,pl_num) -->
    n_word(W),
    { duizend(W) }.

number_continue(_,both) -->
    n_word(X),
    {  n_word_x(X)  },  %% een huis van 3 x 3
    isa_number(_).

duizend(duizend).
duizend('000').
duizend('500').

n_word_x(x).
n_word_x(-).
n_word_x(×).
n_word_x(*).

small_number_seq(_) --> [].
small_number_seq(N0) -->
    {  N0 < 5,
       N is N0 + 1
    },
    small_number,
    small_number_seq(N).

small_number --> 
    n_word(Nr1),
    {   Nr1=nul
    ;   convert_number(Nr1,N1),
	N1 < 10
    }.				% only digits

:- dynamic found_number_expression_word/3.
:- thread_local found_number_expression_word/3.

number_expression_word(Ws0,Ws) :-
    term_hash(Ws0,Ix),
    (   found_number_expression_word(Ix,Ws0,List)
    ->  true
    ;   findall(Ws,number_expression_wordXX(Ws0,Ws),List0),
	sort(List0,List),
	noclp_assertz(found_number_expression_word(Ix,Ws0,List))
    ),
    lists:member(Ws,List).

number_expression_wordXX(P0,P) :-
    number_expression_duizend(P0,P1),
    number_expression_small_honderd(P1,P2),
    P0 \= P2,
    number_expression_komma_opt(P2,P).
number_expression_wordXX(P0,P) :-
    number_expression_large_honderd(P0,P1),
    P0 \= P1,
    number_expression_komma_opt(P1,P).

number_expression_komma_opt --> [].
number_expression_komma_opt -->
    number_expression_komma.

number_expression_komma -->
    komma,
    number_expression_tien_ne(_).
number_expression_komma -->
    number_expression_breuk.

number_expression_breuk -->
    number_expression_tien_ne_or_een,
    n_word(Derde),
    { rang_or_kwart(Derde) }.

number_expression_breuk -->
    n_word(Eenderde),
    { atom(Eenderde),
      word_rang_or_kwart(Derde),
      atom_concat(Een,Derde,Eenderde),
      number_expression_tien_ne_or_een([Een],[])
    }.

%% hij finishte een tiende achter de winnaar
number_expression_breuk_tiende -->
    number_expression_tien_ne_or_een,
    n_word(Derde),
    { word_rang(Derde) }.

%% hij finishte drietiende achter de winnaar
number_expression_breuk_tiende -->
    n_word(Eenderde),
    { atom(Eenderde),
      atom_concat(Een,Derde,Eenderde),
      word_rang(Derde),
      number_expression_tien_ne_or_een([Een],[])
    }.



number_expression_tien_ne_or_een --> 
    [een].
number_expression_tien_ne_or_een -->
    number_expression_tien_ne(_).

rang_or_kwart(Derde) :-
    rang(Derde).
rang_or_kwart(kwart).

word_rang_or_kwart(Derde) :-
    word_rang(Derde).
word_rang_or_kwart(kwart).

rang(Xde) :-
    atom(Xde),
    (  atom_concat(X,de,Xde)
    ;  atom_concat(X,e,Xde)
    ;  atom_concat(X,ste,Xde)
    ),
    convert_number(X,_Number).

rang('Ide').
rang('IIde').
rang('IIIde').
rang('IVde').
rang('Vde').
rang('VIde').
rang('VIIde').
rang('VIIde').
rang('IXde').
rang('Xde').
rang('XIde').
rang('XIIde').
rang('XIIIde').
rang('XIVde').
rang('XVde').
rang('XVIde').
rang('XVIIde').
rang('XVIIIde').
rang('XIXde').
rang('XXde').

rang('Ie').
rang('IIe').
rang('IIIe').
rang('IVe').
rang('Ve').
rang('VIe').
rang('VIIe').
rang('VIIe').
rang('IXe').
rang('Xe').
rang('XIe').
rang('XIIe').
rang('XIIIe').
rang('XIVe').
rang('XVe').
rang('XVIe').
rang('XVIIe').
rang('XVIIIe').
rang('XIXe').
rang('XXe').

rang(X) :-
    word_rang(X).

word_rang(eerste).
word_rang(tweede).
word_rang(derde).
word_rang(vierde).
word_rang(vijfde).
word_rang(zesde).
word_rang(zevende).
word_rang(achtste).
word_rang(negende).
word_rang(tiende).
word_rang(elfde).
word_rang(twaalfde).
word_rang(dertiende).
word_rang(veertiende).
word_rang(vijftiende).
word_rang(zestiende).
word_rang(zeventiende).
word_rang(achttiende).
word_rang(negentiende).
word_rang(twintigste).
word_rang(eenentwintigste).
word_rang(tweeëntwintigste).
word_rang(drieëntwintigste).
word_rang(vierentwintigste).
word_rang(vijfentwintigste).
word_rang(zesentwintigste).
word_rang(zevenentwintigste).
word_rang(achtentwintigste).
word_rang(negenentwintigste).
word_rang(dertigste).
word_rang(eenendertigste).
word_rang(veertigste).
word_rang(vijftigste).
word_rang(zestigste).
word_rang(zeventigste).
word_rang(tachtigste).
word_rang(negentigste).
word_rang(honderdste).
word_rang(duizendste).
word_rang(zoveelste).
word_rang(miljoenste).
word_rang(miljardste).
word_rang(triljoenste).

hoge_rang(honderste).
hoge_rang(duizendste).
hoge_rang(miljoenste).
hoge_rang(miljardste).

komma -->
    n_word(Komma),
    { komma(Komma) }.

komma(komma).
komma(punt).
%% todo: twee derde

number_expression_duizend --> [].
number_expression_duizend -->
    n_word(Tweeduizend),
    { atom(Tweeduizend),
      atom_concat(Twee,duizend,Tweeduizend),
      convert_number(Twee,TweeN),
      TweeN < 1000
    }.
number_expression_duizend -->
    number_expression_small_honderd,
    n_word(duizend).
%% specific for CGN 'achthonderd vijfentwintigduizend'
number_expression_duizend -->
    n_word(Zeshonderd),
    { atom(Zeshonderd),
      atom_concat(Zes,honderd,Zeshonderd),
      convert_number(Zes,ZesN),
      ZesN < 10
    },
    n_word(Twintigduizend),
    { atom(Twintigduizend),
      atom_concat(Twintig,duizend,Twintigduizend),
      convert_number(Twintig,TwintigN),
      TwintigN < 100
    }.
    
number_expression_small_honderd -->
    number_expression_tien(_).
number_expression_small_honderd -->
    n_word(honderd),
    number_expression_tien(_).
number_expression_small_honderd -->
    n_word(Tweehonderd),
    { atom(Tweehonderd),
      atom_concat(Twee,honderd,Tweehonderd),
      convert_number(Twee,TweeN),
      TweeN < 10
    },
    number_expression_tien(_).
number_expression_small_honderd -->
    n_word(Twee),n_word(honderd),
    { convert_number(Twee,TweeN),
      TweeN < 10
    },
    number_expression_tien(_).

% subsumed by next one
%number_expression_small_honderd -->
%    n_word(TweehonderdTachtig),
%    { atom(TweehonderdTachtig),
%      atom_concat(Tweehonderd,Tachtig,TweehonderdTachtig),
%      atom_concat(Twee,honderd,Tweehonderd),
%      convert_number(Tachtig,TachtigN),
%      TachtigN < 100,
%      convert_number(Twee,TweeN),
%      TweeN < 10
%    }.

number_expression_small_honderd -->
    n_word(DertienhonderdTachtig),
    { atom(DertienhonderdTachtig),
      alpino_unknowns:atom_split(DertienhonderdTachtig,honderd,Dertien,Tachtig),
      convert_number(Tachtig,TachtigN),
      TachtigN < 100,
      convert_number(Dertien,DertienN),
      DertienN < 100,
      (   integer(DertienN)
      ->  0 =\= mod(DertienN,10)
      ;   true
      )
    }.

% ??
% number_expression_large_honderd_or_een -->
%     n_word(een).
% number_expression_large_honderd_or_een -->
%     number_expression_large_honderd.

number_expression_large_honderd -->
    number_expression_tien(_).
number_expression_large_honderd -->
    n_word(honderd),
    number_expression_tien(_).
number_expression_large_honderd -->
    n_word(Dertienhonderd),
    { atom(Dertienhonderd),
      atom_concat(Dertien,honderd,Dertienhonderd),
      convert_number(Dertien,DertienN),
      DertienN < 100,
      (   integer(DertienN)
      ->  0 =\= mod(DertienN,10)	% *twintig honderd
      ;   true
      )
    },
    number_expression_tien(_).
number_expression_large_honderd -->
    number_expression_tien(Nr),
    {  (   integer(Nr)
       ->  0 =\= mod(Nr,10)
       ;   true
       )},
    n_word(honderd),
    number_expression_tien(_).

number_expression_tien(0) --> [].
number_expression_tien(N) -->
    number_expression_tien_ne(N).

number_expression_tien_ne(TwaalfN) -->
    n_word(Twaalf),
    { convert_number(Twaalf,TwaalfN),
      TwaalfN < 100
    }.

%% 15
isa_number(Agr) -->
    n_word(W),
    { parse_number(W,Agr) },
    opt_breuk.

% is an adjective?

isa_number(Agr) -->
    n_word(W0),
    { atom(W0),
      (  atom_concat(W,eneenhalf,W0)
      ;  atom_concat(W,'en-een-half',W0)
      ;  atom_concat(W,enhalf,W0)
      ;  atom_concat(W,ëneenhalf,W0)
      ;  atom_concat(W,ënhalf,W0)
      ),
      parse_number(W,Agr)
    }.

opt_breuk --> [].
opt_breuk --> n_word(U),
    { is_u_breuk(U) }.
opt_breuk -->
    n_word(en),
    n_word(een),
    n_word(half).

%% non-det atom_concat
opt_breuk --> 
    n_word(Atom), % 1/3
    {  atom(Atom),
       once(alpino_unknowns:atom_split(Atom,'/',Prefix,Suffix)),
       parse_number_simple(Prefix),
       parse_number_simple(Suffix)
    }.

is_u_breuk('¼').  % 1/4
is_u_breuk('½').  % 1/2
is_u_breuk('¾').  % 3/4
is_u_breuk('¼').  % 1/4
is_u_breuk('½').  % 1/2
is_u_breuk('¾').  % 3/4

%% includes weird stuff
parse_number(W,Agr) :-
    \+ punctuation(W),
    \+ punct(W,_),
    \+ alpino_lexical_analysis:hesitation(W),
    (	atom(W)
    ->	\+ special_number_atom(W),
	atom_length(W,Len),Len < 50,
	atom_codes_silent(W,Codes),
        \+ all_alpha(Codes),
	(   number_codes_silent(Num,Codes)
	->  (   (  Num=:=  1
		;  Num=:= -1  % het was -1 graad.
		)
	    ->   Agr=sg_num
	    ;    Agr=pl_num
	    )
	;   isa_number_(Codes),
	    Codes = [_,_|_],
	    Agr=both
	)
    ;	number(W),
	(    (  W=:=  1
	     ;  W=:= -1	% het was -1 graad.
	     )
	->   Agr=sg_num
	;    Agr=pl_num
	)
    ),!.
    % if single character, then it must be a real number

%% 3x2
parse_number(W,pl_num) :-
    atom(W),
    once(alpino_unknowns:atom_split(W,x,NumA,NumB)),
    parse_number_simple(NumA),
    parse_number_simple(NumB).

%% vijftien
parse_number(W,pl_num) :-
    convert_number(W,Nr),
    Nr > 1.

parse_number_simple(Atom) :-
    atom_codes_silent(Atom,Codes),
    all_alpha(Codes), !, fail.

parse_number_simple(Atom) :-
    parse_number_simple(Atom,_Number).

bracketed_parse_number_simple(Atom,Nr) :-
    atom_codes_silent(Atom,Codes),
    Codes = [40|Codes1],
    lists:append(Codes2,[41],Codes1),
    number_codes_silent(Nr,Codes2).

parse_number_simple(Atom,Number) :-
    atom_length(Atom,Len), Len < 25,
    \+ special_number_atom(Atom),
    atom_codes_silent(Atom,Codes), 
    number_codes_silent(Number,Codes).

punctuation(W) :-
    xl(W,punct(_),_,[],[]).

%% solely non-alphabetic
isa_number_([H|T]) :-
    \+ isalpha(H),
    \+ [H] = "(",
    isa_number__(T).

isa_number_(Codes) :-
    atom_codes(W0,Codes),
    atom_concat(Twee,EnEenHalfDuizend,W0),
    atom_concat(EnEenHalf,Duizend,EnEenHalfDuizend),
    duizend_na_half(Duizend),
    eneenhalf(EnEenHalf),
    parse_number(Twee,_Agr).

eneenhalf(eneenalf).
eneenhalf('en-een-half').
eneenhalf('-en-een-half').
eneenhalf('en-half').
eneenhalf('-en-half').
eneenhalf(enhalf).
eneenhalf(ëneenhalf).
eneenhalf(ënhalf).

duizend_na_half(duizend).
duizend_na_half(miljoen).
duizend_na_half(miljard).
duizend_na_half(biljoen).
duizend_na_half('').

isa_number__(List) :-
    length(List,Len), Len < 25,
    atom_codes(U,List), 
    is_u_breuk(U).

isa_number__([]).
isa_number__([H|T]) :-
    \+ isalpha(H),
    isa_number__(T).

date_expression -->
    date_day,
    date_month,
    date_opt_year.

% date_expression -->
%     date_month,
%     date_year.

date_expression -->           % we zien elkaar de twaalfde
    n_word(de),
    n_word(Rang),
    { rang(Rang) }.

%% 7/7
%% 7/7/2010
date_expression -->
    [Word],
    { atom(Word),
      atom_length(Word,Len), Len < 25,
      atom_codes(Word,Codes), 
      date_expression_codes(Codes,[])
    }.

date_expression_codes -->
    day_codes,
    "/",
    month_codes,
    "/",
    year_codes_short.

date_expression_codes -->
    day_codes,
    "-",
    month_codes,
    "-",
    year_codes_short.

date_expression_codes -->
    day_codes,
    "/",
    month_codes.

day_codes -->
    number_code(A),
    number_code(B),
    {  number_codes_silent(Nr,[A,B]),
       Nr > 0, Nr < 31
    }.
day_codes -->
    number_code(_).

month_codes -->
    number_code(A),
    number_code(B),
    {  number_codes_silent(Nr,[A,B]),
       Nr > 0, Nr < 13
    }.
month_codes -->
    number_code(_).

number_code(A) -->
    [A], { A =< 57, 48 =< A }.


date_month -->
    n_word(Month0),
    { alpino_unknowns:decap(Month0,Month),
      date_month(Month) }.

date_month(januari).
date_month(februari).
date_month(maart).
date_month(april).
date_month(mei).
date_month(juni).
date_month(juli).
date_month(augustus).
date_month(september).
date_month(oktober).
date_month(november).
date_month(december).
date_month('jan.').
date_month('feb.').
date_month('febr.').
date_month('mrt.').
date_month('aug.').
date_month('sep.').
date_month('sept.').
date_month('okt.').
date_month('nov.').
date_month('dec.').
date_month(jan).
date_month(feb).
date_month(febr).
date_month(mrt).
date_month(apr).
date_month(jun).
date_month(jul).
date_month(aug).
date_month(sep).
date_month(sept).
date_month(okt).
date_month(nov).
date_month(dec).

date_day -->
    date_a_day.

date_day -->
    date_a_day,
    n_word('-'),
    date_a_day.

date_day -->
    n_word(Expr),
    {  once(alpino_unknowns:atom_split(Expr,'-',Day1,Day2)),
       convert_number(Day1,Num1),
       Num1 < 32,
       convert_number(Day2,Num2),
       Num2 < 32,
       Num1 < Num2
    }.

date_day -->
    date_a_day,
    date_a_day_conj.

date_day -->
    n_word(de),
    n_word(Rang),
    { rang_day(Rang) }.

date_a_day -->
    n_word(Number),
    { convert_number(Number,Num),
      integer(Num),
      Num < 32
    }.

date_a_day_conj -->
    date_conj,
    date_a_day,
    date_a_day_conj0.

date_a_day_conj0 -->
    date_conj,
    date_a_day,
    date_a_day_conj0.
date_a_day_conj0 --> [].

date_conj -->
    n_word(Word),
    date_conj(Word).

date_conj(',')    --> [].
date_conj(en)     --> [].
date_conj('t/m')  --> [].
date_conj(tot)    --> n_word(en), n_word(met).
date_conj(t)      --> n_word('/'), n_word(m).
     
rang_day(eerste).
rang_day(tweede).
rang_day(derde).
rang_day(vierde).
rang_day(vijfde).
rang_day(zesde).
rang_day(zevende).
rang_day(achtste).
rang_day(negende).
rang_day(tiende).
rang_day(elfde).
rang_day(twaalfde).
rang_day(dertiende).
rang_day(veertiende).
rang_day(vijftiende).
rang_day(zestiende).
rang_day(zeventiende).
rang_day(achttiende).
rang_day(negentiende).
rang_day(twintigste).
rang_day(eenentwintigste).
rang_day(tweeentwintigste).
rang_day(tweeëntwintigste).
rang_day(drieentwintigste).
rang_day(vierentwintigste).
rang_day(vijfentwintigste).
rang_day(zesentwintigste).
rang_day(zevenentwintigste).
rang_day(achtentwintigste).
rang_day(negentwintigste).
rang_day(dertigste).
rang_day(eenendertigste).
rang_day(L) :-
    numbere(L).

date_opt_year --> [].
date_opt_year --> date_year.
date_opt_year --> date_year_part.

simple_year(Number) -->
    n_word(Number),
    {  parse_number_simple(Number,Nr),
       Nr > 1000,
       Nr < 2100
    }.

bracketed_year -->
    n_word(Number),
    {  bracketed_parse_number_simple(Number,Nr),
       Nr > 1000,
       Nr < 2100
    }.

%% 1968
date_year -->
    n_word(Number),
    {  parse_number_simple(Number,Nr),
       Nr > 1000,
       Nr < 2100
    }.

%% dertienhonderd
date_year -->
    n_word(Dertienhonderd),
    { atom(Dertienhonderd),
      atom_concat(Dertien,honderd,Dertienhonderd),
      convert_number(Dertien,DertienN),
      DertienN > 1, DertienN < 20, DertienN =\= 10  % * tienhonderd
    }.

%% dertien honderd
date_year -->
    number_expression_tien(Nr),
    { Nr > 1, Nr < 20, Nr =\= 10 }, % * tien honderd
    n_word(honderd).

%% '23
date_year -->
    n_word(Atom),
    {  atom(Atom),
       atom_codes_silent(Atom,[39,Num1,Num2]),
       isdigit(Num1),
       isdigit(Num2)
    }.

%% negentien dertien
date_year -->
    n_word(Negentien),
    { date_year_word_begin(Negentien) },
    n_word(Negenennegentig),
    { simple_convert_number(Negenennegentig,N),
      N > 0, N < 100
    }.

%% too many false hits; only if part of larger
%% date expression
date_year_part -->
    n_word(Negenennegentig),
    { simple_convert_number(Negenennegentig,N),
      N > 20,  % arbitrary?
      N < 100
    }.

date_double_year -->
    [Word],
    {   atom(Word),
	atom_codes(Word,Codes), length(Codes,Len), Len < 25,
	date_double_year_codes(Codes,[])
    }.

phrasal_entry(with_dt(np(year),
                      dt(conj,[cnj=l(Year1,np(year),np,0,1),
                               cnj=l(Year2,np(year),np,2,3)])),
              date_year,[Year1,'-',Year2|Xs],Xs) :-
    hdrug_util:debug_message(4,"year-year~n",[]),
    date_year([Year1],[]),
    date_year([Year2],[]).

date_double_year_codes -->
    year_codes,
    separator_year_codes,
    year_codes_short.

year_codes -->
    [A,B,C,D],
    { \+ special_number_code([A,B,C,D]),
      number_codes_silent(Nr,[A,B,C,D]),
      Nr > 1000, Nr < 2100
    }.

year_codes_short -->
    year_codes.
year_codes_short -->
    [A,B],
    {  A =< 57, 48 =< A,
       B =< 57, 48 =< B
    }.

separator_year_codes -->
    "-".
separator_year_codes -->
    "/".

date_year_word_begin(twaalf).
date_year_word_begin(dertien).
date_year_word_begin(veertien).
date_year_word_begin(vijftien).
date_year_word_begin(zestien).
date_year_word_begin(zeventien).
date_year_word_begin(achttien).
date_year_word_begin(negentien).

tmp_uur_uur -->
    n_word(Uur),
    {  tmp_uur_uur(Uur) }.

tmp_uur_uur(uur).
tmp_uur_uur('u.').
tmp_uur_uur(u).
tmp_uur_uur(ure).

opt_temporal_expression_suffix --> [].
opt_temporal_expression_suffix -->
    n_word('GMT').
opt_temporal_expression_suffix -->
    n_word('CEST').
opt_temporal_expression_suffix -->
    n_word('CET').
opt_temporal_expression_suffix -->
    n_word('UTC').
%% there are many more, of course...

temporal_expression -->
    tmp_uur_num,
    tmp_uur_uur.

temporal_expression -->
    tmp_uur_num,
    tmp_uur_num_conj,
    tmp_uur_uur.

temporal_expression -->
    n_word(een),
    n_word(uur),
    n_word(of),
    tmp_uur_num.

temporal_expression -->
    n_word(een),
    n_word(uur),
    n_word(of),
    n_word(half),
    tmp_voor_uur_num.

temporal_expression -->
    n_word(een),
    n_word(uur),
    n_word(of),
    halfvier.

temporal_expression -->
    tmp_uur_num_u.

temporal_expression -->
    tmp_uur_num_u,
    tmp_uur_num_u_conj.
temporal_expression -->
    tmp_uur_num,
    tmp_uur_uur,
    tmp_minuut_num.

temporal_expression -->
    tmp_minuut_num,
    tmp_voor,
    tmp_optional_half.

temporal_expression -->
    n_word(even),
    tmp_voor,
    tmp_optional_half.

temporal_expression -->
    n_word(half),
    tmp_voor_uur_num.

temporal_expression -->
    halfvier.

temporal_expression -->
    n_word(kwart),
    tmp_voor,
    tmp_voor_uur_num.

temporal_expression -->
    n_word(NuN),
    {num_u_num(NuN)}.

tmp_uur_num_u_conj -->
    date_conj,
    tmp_uur_num_u,
    tmp_uur_num_u_conj0.

tmp_uur_num_u_conj0 -->
    date_conj,
    tmp_uur_num_u,
    tmp_uur_num_u_conj0.
tmp_uur_num_u_conj0 --> [].

tmp_uur_num_conj -->
    date_conj,
    tmp_uur_num,
    tmp_uur_num_conj0.

tmp_uur_num_conj0 -->
    date_conj,
    tmp_uur_num,
    tmp_uur_num_conj0.
tmp_uur_num_conj0 --> [].

%% cgn spelling...
halfvier -->
    n_word(Half),
    {  halfvier(Half) }.

halfvier(halfeen).
halfvier(halftwee).
halfvier(halfdrie).
halfvier(halfvier).
halfvier(halfvijf).
halfvier(halfzes).
halfvier(halfzeven).
halfvier(halfacht).
halfvier(halfnegen).
halfvier(halftien).
halfvier(halfelf).
halfvier(halftwaalf).
%% te middernacht
halfvier(middernacht).

tmp_minuut_num -->
    n_word(Number),
    { convert_number(Number,Num),
      integer(Num),
      Num < 60
    },
    tmp_minuut(Num).

tmp_minuut(_) --> [].
tmp_minuut(1) --> n_word(minuut).
tmp_minuut(N) --> n_word(minuten), { N > 1 }.

tmp_voor --> n_word(voor).
tmp_voor --> n_word(over).

tmp_optional_half -->
    tmp_voor_uur_num.
tmp_optional_half -->
    n_word(half),
    tmp_voor_uur_num.
tmp_optional_half -->
    halfvier.
tmp_optional_half -->       % de trein van drie over half
    n_word(half).
tmp_optional_half --> [].   % de trein van twaalf over

tmp_voor_uur_num -->
    n_word(Number),
    { convert_number(Number,Num),
      integer(Num),
      Num < 13
    }.

tmp_voor_uur_num -->
    n_word(een).

tmp_uur_num -->
    n_word(een).
tmp_uur_num -->
    n_word(Number),
    { convert_number(Number,Num),
      integer(Num),
      Num < 25
    }, !.

tmp_uur_num -->
    n_word(Number),
    { num_dot_num(_,Number)
    }, !.

tmp_uur_num_u -->
    n_word(Number),
    { num_dot_num(u,Number)
    }, !.

tmp_uur_num_u -->
    n_word(Number),
    { num_dot_num(dot,Number)
    }, !.

num_dot_num(U,Number) :-
    atom(Number),
    atom_codes_silent(Number,Codes), length(Codes,Len), Len < 25,
    \+ all_alpha(Codes),
    num_dot_num_codes(U,Codes,[]).

num_dot_num_dot --> 
    ".".
num_dot_num_dot --> 
    ":".

num_dot_num_codes(U) -->
    num_dot_num_codes_num(0,24),
    num_dot_num_dot,
    num_dot_num_codes_num_two_digits(0,59),
    num_dot_num_codes_u(no_u,U1),
    num_dot_num_codes_dash(U1,U).

num_dot_num_codes(dot) -->
    num_dot_num_codes_num(0,24),
    num_dot_num_dot,
    num_dot_num_codes_num_two_digits(0,59),
    num_dot_num_codes_u(no_u,_),
    num_dot_num_codes_dash(_,_).

num_dot_num_codes(U) -->
    num_dot_num_codes_num(0,24),
    num_dot_num_codes_u(no_u,U).

num_dot_num_codes(U) -->
    num_dot_num_codes_num(0,24),
    num_dot_num_codes_u(no_u,U0),
    num_dot_num_codes_dash(U0,U).

num_dot_num_codes_u(_,u) --> "u".
num_dot_num_codes_u(_,u) --> "u.".
num_dot_num_codes_u(U,U) --> "".

num_dot_num_codes_dash(U0,U) -->
    "-",
    num_dot_num_codes_num(0,24),
    ".",
    num_dot_num_codes_num_two_digits(0,59),
    num_dot_num_codes_u(U0,U).
num_dot_num_codes_dash(U0,U) -->
    "-",
    num_dot_num_codes_num(0,24),
    num_dot_num_codes_u(U0,U).
num_dot_num_codes_dash(U0,U) -->
    "-",
    num_dot_num_codes_num(0,24),
    num_dot_num_codes_u(U0,U),
    num_dot_num_codes_num_two_digits(0,59).
num_dot_num_codes_dash(U,U) --> [].

num_dot_num_codes_num_two_digits(Min,Max) -->
    [A,B],
    { number_codes_silent(Uur,[A,B]),
      Min =< Uur,
      Uur =< Max
    }.
    
num_dot_num_codes_num(Min,Max) -->
    [A],
    { number_codes_silent(Uur,[A]),
      Min =< Uur,
      Uur =< Max
    }.
num_dot_num_codes_num(Min,Max) -->
    [A,B],
    { number_codes_silent(Uur,[A,B]),
      Min =< Uur,
      Uur =< Max
    }.

num_u_num(Number) :-
    atom(Number),
    atom_codes_silent(Number,Codes), length(Codes,Len), Len < 25,
    \+ all_alpha(Number),
    lists:append(Pref,[117|D3D4],Codes),
    \+ special_number_code(Pref),
    number_codes_silent(Uur,Pref),
    Uur < 25, Uur >= 0,
    (	D3D4 == []
    ;	\+ special_number_code(D3D4),
        number_codes_silent(Min,D3D4),
	Min < 60, Min >= 0
    ).

convert_number(Input,Number) :-
    simple_convert_number(Input,Number).
%%%this is removed, because simple cases such as
%%% "Vijfendertig kinderen moesten naar het ziekenhuis"
%%% assigns wrong lemma to Vijfendertig
%%%convert_number(Input,Number) :-
%%%    alpino_unknowns:decap_some(Input,Input1),
%%%    simple_convert_number(Input1,Number).
convert_number(Input,Number) :-
    complex_convert_number(Input,Number).

simple_convert_number(één,1).
simple_convert_number(twee,2).
simple_convert_number(drie,3).
simple_convert_number(vier,4).
simple_convert_number(vijf,5).
simple_convert_number(zes,6).
simple_convert_number(zeven,7).
simple_convert_number(acht,8).
simple_convert_number(negen,9).
simple_convert_number(tien,10).
simple_convert_number(elf,11).
simple_convert_number(twaalf,12).
simple_convert_number(dertien,13).
simple_convert_number(veertien,14).
simple_convert_number(vijftien,15).
simple_convert_number(zestien,16).
simple_convert_number(zeventien,17).
simple_convert_number(achttien,18).
simple_convert_number(negentien,19).
simple_convert_number(twintig,20).
simple_convert_number(eenentwintig,21).
simple_convert_number(tweeëntwintig,22).
simple_convert_number(tweeentwintig,22).
simple_convert_number(drieëntwintig,23).
simple_convert_number(drieentwintig,23).
simple_convert_number(vierentwintig,24).
simple_convert_number(vijfentwintig,25).
simple_convert_number(zesentwintig,26).
simple_convert_number(zevenentwintig,27).
simple_convert_number(achtentwintig,28).
simple_convert_number(negenentwintig,29).
simple_convert_number(dertig,30).
simple_convert_number(eenendertig,31).
simple_convert_number(tweeendertig,32).
simple_convert_number(tweeëndertig,32).
simple_convert_number(drieëndertig,33).
simple_convert_number(drieendertig,33).
simple_convert_number(vierendertig,34).
simple_convert_number(vijfendertig,35).
simple_convert_number(zesendertig,36).
simple_convert_number(zevenendertig,37).
simple_convert_number(achtendertig,38).
simple_convert_number(negenendertig,39).
simple_convert_number(veertig,40).
simple_convert_number(eenenveertig,41).
simple_convert_number(tweeënveertig,42).
simple_convert_number(tweeenveertig,42).
simple_convert_number(drieënveertig,43).
simple_convert_number(drieenveertig,43).
simple_convert_number(vierenveertig,44).
simple_convert_number(vijfenveertig,45).
simple_convert_number(zesenveertig,46).
simple_convert_number(zevenenveertig,47).
simple_convert_number(achtenveertig,48).
simple_convert_number(negenenveertig,49).
simple_convert_number(vijftig,50).
simple_convert_number(eenenvijftig,51).
simple_convert_number(tweeënvijftig,52).
simple_convert_number(tweeenvijftig,52).
simple_convert_number(drieënvijftig,53).
simple_convert_number(drieenvijftig,53).
simple_convert_number(vierenvijftig,54).
simple_convert_number(vijfenvijftig,55).
simple_convert_number(zesenvijftig,56).
simple_convert_number(zevenenvijftig,57).
simple_convert_number(achtenvijftig,58).
simple_convert_number(negenenvijftig,59).
simple_convert_number(zestig,60).
simple_convert_number(eenenzestig,61).
simple_convert_number(tweeënzestig,62).
simple_convert_number(drieënzestig,63).
simple_convert_number(vierenzestig,64).
simple_convert_number(vijfenzestig,65).
simple_convert_number(zesenzestig,66).
simple_convert_number(zevenenzestig,67).
simple_convert_number(achtenzestig,68).
simple_convert_number(negenenzestig,69).
simple_convert_number(zeventig,70).
simple_convert_number(eenenzeventig,71).
simple_convert_number(tweeënzeventig,72).
simple_convert_number(drieënzeventig,73).
simple_convert_number(vierenzeventig,74).
simple_convert_number(vijfenzeventig,75).
simple_convert_number(zesenzeventig,76).
simple_convert_number(zevenenzeventig,77).
simple_convert_number(achtenzeventig,78).
simple_convert_number(negenenzeventig,79).
simple_convert_number(tachtig,80).
simple_convert_number(eenentachtig,81).
simple_convert_number(tweeëntachtig,82).
simple_convert_number(drieëntachtig,83).
simple_convert_number(vierentachtig,84).
simple_convert_number(vijfentachtig,85).
simple_convert_number(zesentachtig,86).
simple_convert_number(zevenentachtig,87).
simple_convert_number(achtentachtig,88).
simple_convert_number(negenentachtig,89).
simple_convert_number(negentig,90).
simple_convert_number(eenennegentig,91).
simple_convert_number(tweeënnegentig,92).
simple_convert_number(drieënnegentig,93).
simple_convert_number(vierennegentig,94).
simple_convert_number(vijfennegentig,95).
simple_convert_number(zesennegentig,96).
simple_convert_number(zevenennegentig,97).
simple_convert_number(achtennegentig,98).
simple_convert_number(negenennegentig,99).

simple_convert_number(honderd,100).
simple_convert_number(honderdtien,110).
simple_convert_number(honderdtwintig,120).
simple_convert_number(honderddertig,130).
simple_convert_number(honderdveertig,140).
simple_convert_number(honderdvijftig,150).
simple_convert_number(honderdzestig,160).
simple_convert_number(honderdzeventig,170).
simple_convert_number(honderdtachtig,180).
simple_convert_number(honderdnegentig,190).

simple_convert_number(tweehonderdvijftig,250).

simple_convert_number(duizend,1000).

complex_convert_number(Atomic,Number) :-
    (	atom(Atomic)
    ->	parse_number_simple(Atomic,Number)
    ;   number(Atomic),
	Atomic=Number
    ).

complex_convert_number(Tweehonderd,Number) :-
    atom(Tweehonderd),
    atom_concat(Twee,honderd,Tweehonderd),
    convert_number(Twee,TweeN),
    TweeN < 100,
    (   integer(TweeN)
    ->  0 =\= mod(TweeN,10)
    ;   true
    ),				% *twintig honderd
    Number is TweeN*100.

%% has been broken for a long time !?!?!?
complex_convert_number(TweehonderdTachtig,Number) :-
    atom(TweehonderdTachtig),
    alpino_unknowns:atom_split(TweehonderdTachtig,honderd,Twee,Tachtig),
    convert_number(Twee,TweeN),
    TweeN < 100,
    (   integer(TweeN)
    ->  0 =\= mod(TweeN,10)
    ;   true
    ),				% *twintig honderd
    convert_number(Tachtig,TachtigN),
    TachtigN < 100,
    Number is TweeN*100+TachtigN.

%atom_codes_silent(Atom,Codes) :-
%    prolog:'$atom_elems'(Atom,Codes,character_code).

atom_codes_silent(Atom,Codes) :-
    atom_codes(Atom,Codes).   % what could go wrong?
%    catch(atom_codes(Atom,Codes),_,fail).

%number_codes_silent(Number,Codes) :-
%    prolog:'$number_elems'(Number, Codes, character_code, true).

%% TODO: use efficient version above for Sicstus, and the
%% the next one for swi?
number_codes_silent(Number,[H|Codes]) :-
    starts_number(H),
    catch(number_codes(Number,[H|Codes]),_,fail).

starts_number(43).  % +
starts_number(45).  % -
starts_number(H) :-
    isdigit(H).

all_alpha([]).
all_alpha([H|T]) :-
    isalpha(H),
    all_alpha(T).

special_number_code("+inf").
special_number_code("-inf").
special_number_code("+nan").
special_number_code("-nan").

special_number_atom('+inf').
special_number_atom('-inf').
special_number_atom('+nan').
special_number_atom('-nan').

number_dash_number(Atom) :-
    atom(Atom),
    once(alpino_unknowns:atom_split(Atom,'-',NumHome,NumAway)),
    parse_number_simple(NumHome),
    parse_number_simple(NumAway).

number_dash(Atom) :-
    atom(Atom),
    atom_concat(NumHome,'-',Atom),
    parse_number_simple(NumHome).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zwem_slag -->
    afstand_meter_slag,
    estafette.

afstand_meter_slag --> afstand_meter, slag.
afstand_meter_slag --> afstand_x_meter.

afstand_meter --> afstand, meter.
afstand_meter -->
    n_word(Meter),
    {  afstand_meter(Meter) }.
afstand_meter --> afstand_x_meter.

afstand_meter('50m').
afstand_meter('60m').
afstand_meter('110m').
afstand_meter('100m').
afstand_meter('200m').
afstand_meter('400m').
afstand_meter('800m').
afstand_meter('1500m').
afstand_meter('3000m').

afstand_x_meter -->
    n_word(Meter),
    {  afstand_x_meter(Meter) }.

afstand_x_meter --> n_word('4'), n_word(x), n_word('50m').
afstand_x_meter --> n_word('4'), n_word(x), n_word('100m').
afstand_x_meter --> n_word('4'), n_word(x), n_word('200m').
afstand_x_meter --> n_word('4'), n_word(x), n_word('400m').
afstand_x_meter --> afstand_x, meter.

afstand_x_meter('4x50m').
afstand_x_meter('4x100m').
afstand_x_meter('4x200m').
afstand_x_meter('4x400m').

afstand -->
    n_word(A),
    {  afstand(A) }.
afstand --> afstand_x.

afstand('50').
afstand('60').
afstand('100').
afstand('110').
afstand('200').
afstand('400').
afstand('800').
afstand('1500').
afstand('3000').
afstand(vijftig).
afstand(zestig).
afstand(honderd).
afstand(honderdtien).
afstand(honderdentien).
afstand(tweehonderd).
afstand(vierhonderd).
afstand(achthonderd).
afstand(vijftienhonderd).
afstand(drieduizend).

afstand_x -->
    n_word(A),
    {  afstand_x(A) }.
afstand_x --> n_word('4'), n_word(x), n_word('50').
afstand_x --> n_word('4'), n_word(x), n_word('100').
afstand_x --> n_word('4'), n_word(x), n_word('200').
afstand_x --> n_word('4'), n_word(x), n_word('400').

afstand_x('4x50').
afstand_x('4x100').
afstand_x('4x200').
afstand_x('4x400').

meter --> n_word(M),
    { meter(M) }.
meter --> [].

meter(meter).
meter(m).
meter('m.').

slag -->
    n_word(A),
    { slag(A) }.

slag --> n_word(vrije),   n_word(slag).
slag --> n_word(steeple), n_word(chase).

slag(rug).
slag(rugslag).
slag(school).
slag(schoolslag).
slag(vlinder).
slag(vlinderslag).
slag(vrij).
slag(vrijeslag).
slag(wissel).
slag(wisselslag).
slag(horden).
slag(sprint).
slag(steeple).

estafette --> mv, n_word(estafette).
estafette --> n_word(estafette), mv.
estafette --> n_word(estafette).
estafette --> [].

mv -->
    n_word(M),
    {  mv(M) }.

mv(jongens).
mv(mannen).
mv(heren).
mv(dames).
mv(vrouwen).
mv(meisjes).

%% chess

chess_game -->
    chess_move,
    chess_game1.

chess_game1 -->
    opt_comma,
    chess_move,!,
    chess_game1.
chess_game1 --> [].

opt_comma --> [','].
opt_comma --> [';'].
opt_comma --> [].

chess_move -->
    is_chess_optional_number,
    [Move],
    {  is_chess_move(Move) },
    is_chess_optional_plus,
    is_chess_optional_comment.

is_chess_optional_comment --> ['!'].
is_chess_optional_comment --> ['!!'].
is_chess_optional_comment --> ['?'].
is_chess_optional_comment --> ['??'].
is_chess_optional_comment --> ['!?'].
is_chess_optional_comment --> ['?!'].
is_chess_optional_comment --> [].

is_chess_move(Move) :-
    atom(Move),
    atom_codes(Move,Codes),
    is_chess_move(Codes,[]).

is_chess_move -->
    is_chess_optional_number_codes,
    is_chess_normal_or_roccade_or_checkers.

is_chess_normal_or_roccade_or_checkers -->
    is_chess_optional_piece,
    is_chess_position,
    is_chess_transition,
    is_chess_position,
    is_chess_optional_plus_codes.
is_chess_normal_or_roccade_or_checkers -->
    is_chess_optional_piece,
    is_chess_short_combination,
    is_chess_optional_plus_codes.
is_chess_normal_or_roccade_or_checkers -->
    is_chess_optional_piece,
    is_chess_optional_short_position_transition,
    is_chess_position,
    is_chess_optional_plus_codes.
is_chess_normal_or_roccade_or_checkers --> "O-O".
is_chess_normal_or_roccade_or_checkers --> "O-O-O".
is_chess_normal_or_roccade_or_checkers --> "0-0".
is_chess_normal_or_roccade_or_checkers --> "0-0-0".
is_chess_normal_or_roccade_or_checkers -->
    is_checkers_position,
    is_chess_transition,
    is_more_checkers_position,
    is_chess_optional_plus_codes.


is_chess_short_combination --> "ab".
is_chess_short_combination --> "bc".
is_chess_short_combination --> "cd".
%%%is_chess_short_combination --> "de".  too many false hits
is_chess_short_combination --> "ef".
is_chess_short_combination --> "fg".
is_chess_short_combination --> "gh".
is_chess_short_combination --> "ba".
is_chess_short_combination --> "cb".
is_chess_short_combination --> "dc".
is_chess_short_combination --> "ed".
is_chess_short_combination --> "fe".
is_chess_short_combination --> "gf".
is_chess_short_combination --> "hg".

is_more_checkers_position -->
    is_checkers_position.
is_more_checkers_position -->
    is_checkers_position,
    is_chess_transition,
    is_more_checkers_position.

is_chess_optional_plus --> ['++'].
is_chess_optional_plus --> ['+'].
is_chess_optional_plus --> [].

is_chess_optional_plus_codes --> "++".
is_chess_optional_plus_codes --> "+".
is_chess_optional_plus_codes --> "++".
is_chess_optional_plus_codes --> "!+".
is_chess_optional_plus_codes --> [].

is_chess_optional_short_position_transition -->
    is_chess_row,
    is_chess_transition.
is_chess_optional_short_position_transition -->
    is_chess_row.
is_chess_optional_short_position_transition -->
    is_chess_transition.
is_chess_optional_short_position_transition --> [].

is_checkers_position -->
    is_chess_number.

is_chess_optional_piece --> "T".
is_chess_optional_piece --> "L".
is_chess_optional_piece --> "P".
is_chess_optional_piece --> "K".
is_chess_optional_piece --> "D".
is_chess_optional_piece --> [].
   
is_chess_transition --> "-".
is_chess_transition --> "x".

is_chess_position -->
    is_chess_row,
    is_chess_column.

is_chess_row -->
    [C],
    { C >= 97, C =< 104 }.

is_chess_column -->
    [C],
    {  C >= 49, C =< 56 }.
    
is_chess_optional_number --> [W],
    { atom(W),
      atom_codes(W,WCodes),
      is_chess_optional_number_codes(WCodes,[])
    }.    
is_chess_optional_number --> [].

is_chess_optional_number_codes -->
    is_chess_number,
    ".".
is_chess_optional_number_codes --> [].

is_chess_number -->
    [C],
    {  C >= 49, C =< 57 },
    is_chess_number1.

is_chess_number1 -->
    [C],
    {  C >= 48, C =< 57 },
    is_chess_number1.
is_chess_number1 --> [].

bridge_hand -->
    bridge_play_card,!,
    bridge_hand1.
bridge_hand -->
    bridge_player,
    bridge_hand.
bridge_hand -->
    [gever],
    bridge_hand.
bridge_hand -->
    [W],
    { atom(W),
      atom_codes(W,Codes),
      bridge_gever_kwetsbaar(Codes,[])
    },
    bridge_hand.
bridge_hand -->
    bridge_kwetsbaar,
    bridge_hand.

bridge_gever_kwetsbaar -->
    bridge_gever_codes,
    "/",
    bridge_kwetsbaar_codes.

bridge_kwetsbaar_codes --> "Allen".
bridge_kwetsbaar_codes --> "NZ".
bridge_kwetsbaar_codes --> "OW".
bridge_kwetsbaar_codes --> "Niemand".
bridge_kwetsbaar_codes --> "Niem.".
bridge_kwetsbaar_codes --> "-".

bridge_gever_codes --> "N".
bridge_gever_codes --> "O".
bridge_gever_codes --> "Z".
bridge_gever_codes --> "W".
bridge_gever_codes --> "Noord".
bridge_gever_codes --> "Oost".
bridge_gever_codes --> "Zuid".
bridge_gever_codes --> "West".
bridge_gever_codes --> "WestOost".  % NH1994

bridge_kwetsbaar -->
    ['OW'], bridge_kwetsbaar_word.
bridge_kwetsbaar -->
    ['NZ'], bridge_kwetsbaar_word.
bridge_kwetsbaar -->
    [niemand], bridge_kwetsbaar_word.
bridge_kwetsbaar -->
    ['Niemand'], bridge_kwetsbaar_word.
bridge_kwetsbaar -->
    ['Niem.'], bridge_kwetsbaar_word.
bridge_kwetsbaar -->
    [allen], bridge_kwetsbaar_word.
bridge_kwetsbaar -->
    ['Allen'], bridge_kwetsbaar_word.

bridge_kwetsbaar_word --> [kwetsbaar].
bridge_kwetsbaar_word --> ['Kwetsbaar'].
bridge_kwetsbaar_word --> ['kw.'].

bridge_hand1 -->
    bridge_play_card, !,
    bridge_hand1.
bridge_hand1 -->
    bridge_player,!,
    bridge_hand1.
bridge_hand1 --> [].

bridge_players -->
    bridge_player,
    bridge_player,
    bridge_player,
    bridge_player.

% ??
% capitalized(Cap) :-
%     atom(Cap),
%     atom_codes(Cap,[C|_]),
%     isupper(C).

bridge_player --> ['West'].
bridge_player --> ['Oost'].
bridge_player --> ['Noord'].
bridge_player --> ['Zuid'].
bridge_player --> ['W'].
bridge_player --> ['O'].
bridge_player --> ['N'].
bridge_player --> ['Z'].

bridge_color_one --> ['Klaver'].
bridge_color_one --> ['Klaveren'].
bridge_color_one --> ['Ruiten'].
bridge_color_one --> ['Harten'].
bridge_color_one --> ['Schoppen'].
bridge_color_one --> ['-klaver'].
bridge_color_one --> ['-klaveren'].
bridge_color_one --> ['-ruiten'].
bridge_color_one --> ['-harten'].
bridge_color_one --> ['-schoppen'].
bridge_color_one --> ['SA'].
bridge_color_one --> [k].
bridge_color_one --> [r].
bridge_color_one --> [h].
bridge_color_one --> [s].
bridge_color_one --> ['ß7'].
bridge_color_one --> ['ß6'].
bridge_color_one --> ['ß5'].
bridge_color_one --> ['-ruitendiam',';'].   % ????
bridge_color_one --> ['ß4'].
bridge_color_one --> ['&#9824;'].  % schoppen
bridge_color_one --> ['♠'].  % schoppen
bridge_color_one --> ['&#9827;'].  % klaveren
bridge_color_one --> ['♣'].  % klaveren
bridge_color_one --> ['&#9829;'].  % harten
bridge_color_one --> ['♥'].  % harten
bridge_color_one --> ['&#9830;'].  % ruiten
bridge_color_one --> ['♦'].  % ruiten


bridge_color_one_codes --> "Klaver".
bridge_color_one_codes --> "Klaveren".
bridge_color_one_codes --> "Ruiten".
bridge_color_one_codes --> "Harten".
bridge_color_one_codes --> "Schoppen".
bridge_color_one_codes --> "-klaver".
bridge_color_one_codes --> "-klaveren".
bridge_color_one_codes --> "-ruiten".
bridge_color_one_codes --> "-harten".
bridge_color_one_codes --> "-schoppen".
bridge_color_one_codes --> "SA".
bridge_color_one_codes --> "ß7".
bridge_color_one_codes --> "ß6".
bridge_color_one_codes --> "ß5".
bridge_color_one_codes --> "ß4".
bridge_color_one_codes --> "s".
bridge_color_one_codes --> "h".
bridge_color_one_codes --> "r".
bridge_color_one_codes --> "k".
bridge_color_one_codes --> "&#9824;".
bridge_color_one_codes --> "♠".
bridge_color_one_codes --> "&#9827;".
bridge_color_one_codes --> "♣".
bridge_color_one_codes --> "&#9829;".
bridge_color_one_codes --> "♥".
bridge_color_one_codes --> "&#9830;".
bridge_color_one_codes --> "♦".
%% vk2004:
% but this only adds additional np's for things that are also names
% bridge_color_one_codes --> "H".
% bridge_color_one_codes --> "I".
% bridge_color_one_codes --> "J".
% bridge_color_one_codes --> "K".

bridge_bidding -->
    bridge_players,
    bridge_bidding.

bridge_bidding -->
    bridge_bid, 
    bridge_bidding1.

bridge_bidding1 -->
    bridge_bid,
    bridge_bidding2.

bridge_bidding2 -->
    bridge_bid,
    bridge_bidding4.

bridge_bidding4 -->
    bridge_bid,
    bridge_bidding4.
bridge_bidding4 --> [].

bridge_bid --> [pas].
bridge_bid --> [doublet].
bridge_bid --> [redoublet].
bridge_bid --> [dbl].
bridge_bid --> [redbl].
bridge_bid --> ['--'].
bridge_bid --> bridge_number, bridge_color_one.
bridge_bid --> [Word], { atom(Word),
			 atom_codes(Word,Codes),
			 bridge_bid_codes(Codes,[]) }.

bridge_bid_codes -->
    bridge_number_codes,
    optional_dash,
    bridge_color_one_codes.

bridge_number --> ['7'].
bridge_number --> ['6'].
bridge_number --> ['5'].
bridge_number --> ['4'].
bridge_number --> ['3'].
bridge_number --> ['2'].
bridge_number --> ['1'].

bridge_number_codes --> "7".
bridge_number_codes --> "6".
bridge_number_codes --> "5".
bridge_number_codes --> "4".
bridge_number_codes --> "3".
bridge_number_codes --> "2".
bridge_number_codes --> "1".

bridge_trailing_players --> [].
bridge_trailing_players -->
    "Oost",
    bridge_trailing_players.
bridge_trailing_players -->
    "West",
    bridge_trailing_players.
bridge_trailing_players -->
    "Noord",
    bridge_trailing_players.
bridge_trailing_players -->
    "Zuid",
    bridge_trailing_players.


bridge_play_card_rest_codes_series -->
    bridge_play_card_rest_codes,
    bridge_play_card_rest_codes_series1.

bridge_play_card_rest_codes_series1 -->
    bridge_play_card_rest_codes,
    bridge_play_card_rest_codes_series1.
bridge_play_card_rest_codes_series1 --> [].

bridge_cards --> ['-'].
bridge_cards -->
    bridge_card, !,
    bridge_cards1.

bridge_cards --> [W], { atom(W),
			atom_codes(W,Codes),
			bridge_play_card_rest_codes_series(Codes,CodesRest),
			bridge_trailing_players(CodesRest,[])
		      }, bridge_cards1. % AHV 10 9

bridge_cards1 -->
    bridge_card, !,
    bridge_cards1.
bridge_cards1 --> [].

bridge_card --> ['A'].
bridge_card --> ['H'].
bridge_card --> ['V'].
bridge_card --> ['B'].
bridge_card --> ['10'].
bridge_card --> ['9'].
bridge_card --> ['8'].
bridge_card --> ['7'].
bridge_card --> ['6'].
bridge_card --> ['5'].
bridge_card --> ['4'].
bridge_card --> ['3'].
bridge_card --> ['2'].
bridge_card --> [x].
bridge_card --> [aas].
bridge_card --> [heer].
bridge_card --> [vrouw].
bridge_card --> [boer].

bridge_bid_outcome -->
    n_word(Word),
    {  bridge_bid_outcome_word(Word) }.

bridge_bid_outcome_word(Word) :-
    atom(Word),
    bridge_bid_outcome_aantal(Aantal),
    atom_concat(Aantal,Kleur,Word),
    bridge_bid_outcome_kleur(Kleur).

bridge_bid_outcome_word(Word) :-
    atom(Word),
    bridge_bid_outcome_aantal(Aantal),
    atom_concat(Aantal,DashKleur,Word), 
    atom_concat('-',Kleur,DashKleur),
    bridge_bid_outcome_kleur(Kleur).

bridge_bid_outcome_aantal('1').
bridge_bid_outcome_aantal('2').
bridge_bid_outcome_aantal('3').
bridge_bid_outcome_aantal('4').
bridge_bid_outcome_aantal('5').
bridge_bid_outcome_aantal('6').
bridge_bid_outcome_aantal('7').

bridge_bid_outcome_kleur('Schoppen').
bridge_bid_outcome_kleur('Klaver').
bridge_bid_outcome_kleur('Klaveren').
bridge_bid_outcome_kleur('Ruiten').
bridge_bid_outcome_kleur('Harten').
bridge_bid_outcome_kleur('SA').
bridge_bid_outcome_kleur('schoppen').
bridge_bid_outcome_kleur('klaver').
bridge_bid_outcome_kleur('klaveren').
bridge_bid_outcome_kleur('ruiten').
bridge_bid_outcome_kleur('harten').
bridge_bid_outcome_kleur('♠').
bridge_bid_outcome_kleur('♣').
bridge_bid_outcome_kleur('♥').
bridge_bid_outcome_kleur('♦').

bridge_play_card_codes -->
    bridge_color_one_codes,
    optional_dash,
    bridge_play_card_rest_codes_series.

optional_dash --> "-".
optional_dash --> [].

bridge_play_card_rest_codes --> "A".
bridge_play_card_rest_codes --> "H".
bridge_play_card_rest_codes --> "V".
bridge_play_card_rest_codes --> "B".
bridge_play_card_rest_codes --> "10".
bridge_play_card_rest_codes --> "9".
bridge_play_card_rest_codes --> "8".
bridge_play_card_rest_codes --> "7".
bridge_play_card_rest_codes --> "6".
bridge_play_card_rest_codes --> "5".
bridge_play_card_rest_codes --> "4".
bridge_play_card_rest_codes --> "3".
bridge_play_card_rest_codes --> "2".
bridge_play_card_rest_codes --> "x".


bridge_play_card -->
    [Word],
    { atom(Word),
      atom_codes(Word,Codes),
      bridge_play_card_codes(Codes,[])
    }.

bridge_play_card -->
    bridge_color_one,
    bridge_cards.

%% typically found in announcements etc
date_opening_hours -->
    date_opening_hours_day,
    date_opening_hours_vanaf,
    date_opening_hours_hours,
    date_opening_hours_more.

date_opening_hours_vanaf --> n_word(om).
date_opening_hours_vanaf --> n_word(van).
date_opening_hours_vanaf --> n_word(vanaf).
date_opening_hours_vanaf --> n_word('v.a.').
date_opening_hours_vanaf --> [].

date_opening_hours_more --> n_word(','), date_opening_hours.
date_opening_hours_more --> n_word(';'), date_opening_hours.
date_opening_hours_more --> date_opening_hours.
date_opening_hours_more --> [].


date_opening_hours_day_simple -->
    n_word(Word),
    { date_opening_hours_day_simple(Word) }.
date_opening_hours_day_simple -->
    n_word('zon.'),
    n_word(en),
    n_word(feestdagen).
    
date_opening_hours_day_simple(ma).
date_opening_hours_day_simple(di).
date_opening_hours_day_simple(wo).
date_opening_hours_day_simple(do).
date_opening_hours_day_simple(vr).
date_opening_hours_day_simple(vrij).
date_opening_hours_day_simple(za).
date_opening_hours_day_simple(zo).
date_opening_hours_day_simple('ma.').
date_opening_hours_day_simple('di.').
date_opening_hours_day_simple('wo.').
date_opening_hours_day_simple('do.').
date_opening_hours_day_simple('vr.').
date_opening_hours_day_simple('vrij.').
date_opening_hours_day_simple('za.').
date_opening_hours_day_simple('zo.').
date_opening_hours_day_simple(feestdagen).

date_opening_hours_day -->
    date_opening_hours_day_simple,
    date_opening_hours_day_more.
date_opening_hours_day -->
    date_opening_hours_day_simple,
    n_word('t/m'),
    date_opening_hours_day_simple.
date_opening_hours_day -->
    date_opening_hours_day_simple,
    n_word('-'),
    date_opening_hours_day_simple.
date_opening_hours_day -->
    n_word(Word),
    { atom(Word),
      atom_codes(Word,Codes),
      date_opening_hours_day_codes(Codes,[])
    }.

date_opening_hours_day_more -->
    n_word(','),
    date_opening_hours_day.
date_opening_hours_day_more -->
    n_word(en),
    date_opening_hours_day.
date_opening_hours_day_more -->
    date_opening_hours_day.
date_opening_hours_day_more --> [].

date_opening_hours_day_codes -->
    date_opening_hours_day_simple_codes,
    "-",
    date_opening_hours_day_simple_codes.

date_opening_hours_day_simple_codes --> "ma".
date_opening_hours_day_simple_codes --> "di".
date_opening_hours_day_simple_codes --> "wo".
date_opening_hours_day_simple_codes --> "do".
date_opening_hours_day_simple_codes --> "vr".
date_opening_hours_day_simple_codes --> "vrij".
date_opening_hours_day_simple_codes --> "za".
date_opening_hours_day_simple_codes --> "zo".
date_opening_hours_day_simple_codes --> "Ma".
date_opening_hours_day_simple_codes --> "Di".
date_opening_hours_day_simple_codes --> "Wo".
date_opening_hours_day_simple_codes --> "Do".
date_opening_hours_day_simple_codes --> "Vr".
date_opening_hours_day_simple_codes --> "Vrij".
date_opening_hours_day_simple_codes --> "Za".
date_opening_hours_day_simple_codes --> "Zo".

date_opening_hours_hours -->
    n_word(Word),
    { atom(Word),
      atom_codes(Word,Codes),
      date_opening_hours_hours_codes(Codes,[])
    },
    date_opening_hours_uur,
    date_opening_hours_hours_more.


date_opening_hours_hours_more -->
    n_word(','),
    date_opening_hours_hours.
date_opening_hours_hours_more -->
    n_word(';'),
    date_opening_hours_hours.
date_opening_hours_hours_more -->
    date_opening_hours_hours.
date_opening_hours_hours_more --> [].

date_opening_hours_uur --> n_word(uur).
date_opening_hours_uur --> n_word(u).
date_opening_hours_uur --> n_word('u.').
date_opening_hours_uur --> [].

date_opening_hours_hours_codes -->
    date_opening_hours_hours_simple.

date_opening_hours_hours_codes -->
    date_opening_hours_hours_simple,
    "-",
    date_opening_hours_hours_simple.

date_opening_hours_hours_simple -->
    date_opening_hours_hours_simple_hour,
    date_opening_hours_hours_simple_minutes,
    date_opening_hours_codes_uur.

date_opening_hours_codes_uur --> "u".
date_opening_hours_codes_uur --> "u.".
date_opening_hours_codes_uur --> "uur".
date_opening_hours_codes_uur --> [].

date_opening_hours_hours_simple_hour --> "1".
date_opening_hours_hours_simple_hour --> "2".
date_opening_hours_hours_simple_hour --> "3".
date_opening_hours_hours_simple_hour --> "4".
date_opening_hours_hours_simple_hour --> "5".
date_opening_hours_hours_simple_hour --> "6".
date_opening_hours_hours_simple_hour --> "7".
date_opening_hours_hours_simple_hour --> "8".
date_opening_hours_hours_simple_hour --> "9".
date_opening_hours_hours_simple_hour --> "10".
date_opening_hours_hours_simple_hour --> "11".
date_opening_hours_hours_simple_hour --> "12".
date_opening_hours_hours_simple_hour --> "13".
date_opening_hours_hours_simple_hour --> "14".
date_opening_hours_hours_simple_hour --> "15".
date_opening_hours_hours_simple_hour --> "16".
date_opening_hours_hours_simple_hour --> "17".
date_opening_hours_hours_simple_hour --> "18".
date_opening_hours_hours_simple_hour --> "19".
date_opening_hours_hours_simple_hour --> "20".
date_opening_hours_hours_simple_hour --> "21".
date_opening_hours_hours_simple_hour --> "22".
date_opening_hours_hours_simple_hour --> "23".
date_opening_hours_hours_simple_hour --> "24".

date_opening_hours_hours_simple_minutes --> ".", "0".
date_opening_hours_hours_simple_minutes --> ".", "00".
date_opening_hours_hours_simple_minutes --> ".", "15".
date_opening_hours_hours_simple_minutes --> ".", "30".
date_opening_hours_hours_simple_minutes --> ".", "45".
date_opening_hours_hours_simple_minutes --> "u", "0".
date_opening_hours_hours_simple_minutes --> "u", "00".
date_opening_hours_hours_simple_minutes --> "u", "15".
date_opening_hours_hours_simple_minutes --> "u", "30".
date_opening_hours_hours_simple_minutes --> "u", "45".
date_opening_hours_hours_simple_minutes --> [].

%% CGN: mwu, but that's weird if unconnected...
%% een man of zes
%% TODO: can be modified with PP: 'een dag of vier na de WK'...

sg_noun(noun(_,_,sg),np).
sg_noun(noun(_,_,both),np).
sg_noun(noun(_,_,meas),np).
sg_noun(noun(_,_,bare_meas),np).
sg_noun(tmp_noun(_,_,sg),tmp_np).
sg_noun(tmp_noun(_,_,both),tmp_np).
sg_noun(tmp_noun(_,_,meas),tmp_np).
sg_noun(tmp_noun(_,_,bare_meas),tmp_np).
sg_noun(meas_mod_noun(_,_,sg),np).
sg_noun(meas_mod_noun(_,_,both),np).
sg_noun(meas_mod_noun(_,_,meas),np).
sg_noun(meas_mod_noun(_,_,bare_meas),np).
sg_noun(mod_noun(_,_,sg),np).
sg_noun(mod_noun(_,_,both),np).
sg_noun(mod_noun(_,_,meas),np).
sg_noun(mod_noun(_,_,bare_meas),np).

phrasal_entry(with_dt(NP,dt(np,[hd=l(N,TAG,1,2),
				det=DT])),een_N_of_NUM) -->
    { hdrug_util:debug_message(4,"een_N_of_NUM~n",[]) },
    een_of_een(R),
    n_word(N),
    { N \= stuk,
      xl(N,TAG,_L,[],[]),
      sg_noun(TAG,NP)},
    n_word(of),
    een_of_number_with_words(3,dt(mwu,[mwp=l(R,determiner(een),detp,0,1),
				       mwp=l(of,conj(of),vg,2,3)]),DT).

een_of_number_with_words(P0,MOD,dt(detp,[hd=l(Num,number(hoofd(pl_num)),P0,P),
					 mod=MOD])) -->
    number_with_words(Num,3,P).

%% een week of drie vier (todo: half vijf)
een_of_number_with_words(P0,MOD,dt(conj,
				   [cnj=dt(detp,[hd=l(Num1,number(hoofd(pl_num)),P0,P),
						 mod=ix(A,MOD)]),
				    cnj=dt(detp,[hd=l(Num2,number(hoofd(pl_num)),P, Q),
						 mod=ix(A)])])) -->
    number_with_words(Num1,3,P),
    number_with_words(Num2,P,Q).

%% een week of drie vier (todo: half vijf)
een_of_number_with_words(P0,MOD,dt(conj,
				   [cnj=dt(detp,[hd=l(Num1,number(hoofd(pl_num)),P0,P),
						 mod=ix(A,MOD)]),
				    cnj=dt(detp,[hd=l(Num2,number(hoofd(pl_num)),Q0,Q),
						 mod=ix(A)])])) -->
    number_with_words(Num1,3,P),
    n_word(','),
    { Q0 is P+1 },    
    number_with_words(Num2,Q0,Q).

een_of_number_with_words(P0,MOD,dt(detp,[hd=l(wat,determiner(wat,nwh,mod,pro,nparg,ntopicpro),P0,P),
					 mod=MOD])) -->
    n_word(wat),
    { P is P0+1 }.

%% een stuk of tien => number
phrasal_entry(with_dt(number(hoofd(pl_num)),
		      dt(detp,[hd=l(Num,number(hoofd(pl_num)),3,P),
			       mod=l('een stuk of',adverb,advp,0,3)])
		     ),een_stuk_of_NUM) -->
    { hdrug_util:debug_message(4,"een stuk of N~n",[]) },
    een_of_een(_R),
    n_word(stuk),
    n_word(of),
    number_with_words(Num,3,P).

%% een stuk of wat
phrasal_entry(with_dt(number(hoofd(pl_num)),
		      dt(detp,[hd=l(wat,determiner(wat,nwh,mod,pro,nparg,ntopicpro),3,4),
			       mod=l('een stuk of',adverb,advp,0,3)])
		     ),een_stuk_of_NUM) -->
    { hdrug_util:debug_message(4,"een stuk of wat~n",[]) },
    een_of_een(_R),
    n_word(stuk),
    n_word(of),
    n_word(wat).

%% een stuk of drie vier
phrasal_entry(with_dt(number(hoofd(pl_num)),
      dt(conj,[cnj=dt(detp,[hd=l(Num1,number(hoofd(pl_num)),3,P),
			    mod=ix(A,l('een stuk of',adverb,advp,0,3))]),
	       cnj=dt(detp,[hd=l(Num2,number(hoofd(pl_num)),P,Q),
			    mod=ix(A)])])),een_stuk_of_NUM) -->
    { hdrug_util:debug_message(4,"een stuk of~n",[]) },
    een_of_een(_R),
    n_word(stuk),
    n_word(of),
    number_with_words(Num1,3,P),
    number_with_words(Num2,P,Q).

een_of_een(een) -->
    n_word(een).
een_of_een(een) -->
    n_word('\'n').
een_of_een('zo\'n') -->
    n_word('zo\'n').

phrasal_entry(adjective(prefix),procents) -->
    { hdrug_util:debug_message(4,"procents~n",[]) },
    number_expression(_),
    n_word(Procents),
    { procents(Procents) }.

phrasal_entry(adjective(prefix),procents) -->
    { hdrug_util:debug_message(4,"procents~n",[]) },
    n_word(ZesProcents),
    {  atom(ZesProcents),
       once(alpino_unknowns:atom_split(ZesProcents,'-',Zes,Procents)),
       procent(Procents),
       parse_number_simple(Zes)
    }.

phrasal_entry(adjective(prefix),procents) -->
    { hdrug_util:debug_message(4,"procents~n",[]) },
    n_word(ZesProcents),
    {  atom(ZesProcents),
       procent(Procents),
       atom_concat(Zes,Procents,ZesProcents),
       parse_number_simple(Zes)
    }.
 
phrasal_entry(noun(both,count,bare_meas),procents) -->
    { hdrug_util:debug_message(4,"procents~n",[]) },
    n_word(ZesProcents),
    {  atom(ZesProcents),
       procent(Procents),
       atom_concat(Zes,Procents,ZesProcents),
       parse_number_simple(Zes)
    }.

procent(procent).
procent('pct.').
procent('%').
procent('‰').
procent(pct).
procent(X) :-
    procents(X).

procents(procents).
procents(uurs).
procents(jaars).
procents(maands).
procents(sterren).  % een vijfsterren restaurant

%% nondet atom_concat
num_meter_loper_entry(noun(A,B,C),MStem,[Number,MeterLoper|L],L,Sep) :-
    number_expression(_,[Number],[]),
    atom(MeterLoper),
    atom_concat(Meter,Loper,MeterLoper),
    Meter \== '',
    Meter \== g,     % 25 gram =/= 25g-ram
    Meter \== m,     % 25 meter =/= 25m-eter
    Meter \== kilom, % 25 kilometer =/= 25 kilom-eter
    \+ never_compound_part(Loper),
    meter_loper_meter(Meter,MeterStem),
    xl(Loper,noun(A,B,C),LoperStem,[],[]),
    concat_stems([Number,Sep,MeterStem,'_',LoperStem],MStem,'').

num_meter_loper_entry(noun(A,B,C),MStem,[Number1,NumberMeterLoper|L],L,Sep) :-
    atom(NumberMeterLoper),
    once(alpino_unknowns:atom_split(NumberMeterLoper,'-',Number,MeterLoper)),
    number_expression(_,[Number1,Number],[]),
    atom(MeterLoper),
    atom_concat(Meter,Loper,MeterLoper),
    Meter \== '',
    Meter \== g,     % 25 gram =/= 25g-ram
    Meter \== m,     % 25 meter =/= 25m-eter
    Meter \== kilom, % 25 kilometer =/= 25 kilom-eter
    \+ never_compound_part(Loper),
    meter_loper_meter(Meter,MeterStem),
    xl(Loper,noun(A,B,C),LoperStem,[],[]),
    concat_stems([Number1,Sep,Number,Sep,MeterStem,'_',LoperStem],MStem,'').

meter_loper_meter(letter,letter).  % een drie letterwoord; een vijf lettercode
meter_loper_meter(punten,punt).
meter_loper_meter(pixel,pixel).
meter_loper_meter(megapixel,mega_pixel).
meter_loper_meter('letter-',letter).  % een drie letterwoord; een vijf lettercode
meter_loper_meter('punten-',punt).
meter_loper_meter('pixel-',pixel).
meter_loper_meter('megapixel-',mega_pixel).

meter_loper_meter(December,December) :-
    date_month(December).
meter_loper_meter(December,Dec) :-
    alpino_unknowns:decap(December,Dec),
    date_month(Dec).
meter_loper_meter(December0,December) :-
    atom(December0),
    atom_concat(December,'-',December0),
    date_month(December).
meter_loper_meter(December,Dec) :-
    atom(December0),
    atom_concat(December,'-',December0),
    alpino_unknowns:decap(December,Dec),
    date_month(Dec).

meter_loper_meter(Word,Stem) :-
    measure_tag(MeasureTag),
    xl(Word,MeasureTag,Stem,[],[]).

meter_loper_meter(Words,Stem) :-
    atom(Words),
    atom_concat(Word,s,Words),
    measure_tag(MeasureTag),
    xl(Word,MeasureTag,Stem,[],[]).

meter_loper_meter(Words,Stem) :-
    atom(Words),
    atom_concat(Word,'-',Words),
    measure_tag(MeasureTag),
    xl(Word,MeasureTag,Stem,[],[]).

meter_loper_meter(Words,Stem) :-
    atom(Words),
    atom_concat(Word,'s-',Words),
    measure_tag(MeasureTag),
    xl(Word,MeasureTag,Stem,[],[]).

meter_loper_meter(Words,Stem) :-
    atom(Words),
    atom_concat(Word,'-',Words),
    abbreviation(Word,Abb),
    atom(Abb),
    xl(Abb,MeasureTag,Stem,[],[]),
    measure_tag(MeasureTag).

meter_loper_meter(holes,hole).

measure_tag(meas_mod_noun(_,_,meas)).
measure_tag(mod_noun(_,_,meas)).
measure_tag(tmp_noun(_,_,meas)).
measure_tag(noun(_,_,meas)).
measure_tag(meas_mod_noun(_,_,bare_meas)).
measure_tag(mod_noun(_,_,bare_meas)).
measure_tag(tmp_noun(_,_,bare_meas)).
measure_tag(noun(_,_,bare_meas)).

%phrasal_entry(tag,enumeration) -->
%    { hdrug_util:debug_message(4,"enumeration~n",[]) },
%    enumeration_punct.


phrasal_entry(tag,hash_tag) -->
    n_word(hashtagblank).

phrasal_entry(tag,hash_tag) -->
    { hdrug_util:debug_message(4,"hash_tag~n",[]) },
    n_word(W),
    {  hash_tag(W) }.

phrasal_entry(proper_name(both),hash_tag) -->
    { hdrug_util:debug_message(4,"hash_tag~n",[]) },
    n_word(W),
    {  hash_tag(W) }.

phrasal_entry(tag,at_tag) -->
    { hdrug_util:debug_message(4,"at_tag~n",[]) },
    n_word(W),
    {  atom(W),
       atom_concat('@',_,W)
    }.

phrasal_entry(proper_name(both,'PER'),at_tag) -->
    { hdrug_util:debug_message(4,"at_tag~n",[]) },
    n_word(W),
    {  atom(W),
       atom_concat('@',Rest,W),
       Rest \= ''
    }.

phrasal_entry(tag,at_tag) -->
    { hdrug_util:debug_message(4,"at_tag~n",[]) },
    n_word('@'),
    n_word(_).

phrasal_entry(proper_name(both,'PER'),at_tag) -->
    { hdrug_util:debug_message(4,"at_tag~n",[]) },
    n_word('@'),
    n_word(_).

phrasal_entry(tag,retweet) -->
    { hdrug_util:debug_message(4,"retweet~n",[]) },
    n_word('RT').

/*
enumeration_punct -->
    n_word('●').
enumeration_punct -->
    n_word('*').
enumeration_punct -->
    n_word('#').
enumeration_punct -->
    n_word('°').
enumeration_punct -->
    n_word('•').  % dot
enumeration_punct -->
    n_word('˜').  % ~
enumeration_punct -->
    n_word('·').
enumeration_punct -->
    n_word('-ruiten'). % volkskrant?
enumeration_punct -->
    n_word('↑').
*/

phrasal_entry(Tag,enumeration) -->
    { hdrug_util:debug_message(4,"enumeration~n",[]) },
    enumeration,
    {  enumeration_tag(Tag) }.

enumeration_tag(tag).
enumeration_tag(enumeration).
enumeration_tag(proper_name(both)). % dat vind u in lid a ) t/m e )  

enumeration -->
    n_word('('),
    enumeration_core,
    n_word(')').

enumeration -->
    n_word('['),
    enumeration_core,
    n_word(']').

enumeration -->
    n_word('{'),
    enumeration_core,
    n_word('}').

enumeration -->
    enumeration_core,
    (  n_word(')')
    ;  n_word('°')
%%    ;  n_word('.')   now in rules (?)
    ).

enumeration -->
    n_word(W),
    {  atom(W),
       atom_codes(W,String),
       stand_alone_enumeration(String)
    }.

enumeration_core -->
    n_word(W),
    {  atom(W),
       atom_codes(W,String),
       enumeration(String)
    }.

%% not analyzable as ordinary number, so
%% these don't need additional punctuation
%% in order to be analyzed as enumeration

%% (XXX)
stand_alone_enumeration([40|String]) :-
    lists:append(String1,[41],String),
    enumeration(String1).

%% XXX.
stand_alone_enumeration(String) :-
    lists:append(String1,[46],String),
    enumeration(String1).

%% XXX)
stand_alone_enumeration(String) :-
    lists:append(String1,[41],String),
    enumeration(String1).

%% XXX°
stand_alone_enumeration(String) :-
    lists:append(String1,[176],String),
    enumeration(String1).

%% 4.3.2
stand_alone_enumeration(String) :-
    digit_dot_digit(String).

stand_alone_enumeration([46,H1]) :-
    isdigit(H1).
stand_alone_enumeration([H1,46]) :-
    isdigit(H1).
stand_alone_enumeration([H1,45]) :-
    isdigit(H1).
stand_alone_enumeration([H1,46,H2]) :-
    isdigit(H1),
    isdigit(H2).
stand_alone_enumeration([H1,46,H2]) :-
    isdigit(H1),
    isalpha(H2).
stand_alone_enumeration([H1,45,H2]) :-
    isdigit(H1),
    isalpha(H2).
stand_alone_enumeration([H1,H2,46,H3]) :-
    isdigit(H1),
    isdigit(H2),
    isalpha(H3).
stand_alone_enumeration([H1,H2,45,H3]) :-
    isdigit(H1),
    isdigit(H2),
    isalpha(H3).
stand_alone_enumeration([H1,H2]) :-
    isdigit(H1),
    isalpha(H2).
stand_alone_enumeration([H1,H2,H3]) :-
    isdigit(H1),
    isdigit(H2),
    isalpha(H3).

stand_alone_enumeration("u") :-
    !, fail.
stand_alone_enumeration("U") :-
    !, fail.
stand_alone_enumeration([Alpha|Tail]) :-
    isalpha(Alpha),
    \+ isaccented(Alpha),
    (   Tail = []
    ;   Tail = [46]
    ).

stand_alone_enumeration(Str) :-
    roman_number(Str).

roman_number("ii").
roman_number("iii").
roman_number("iv").
roman_number("v").
roman_number("vi").
roman_number("vii").
roman_number("viii").
roman_number("ix").
roman_number("xi").
roman_number("xii").
roman_number("xiii").
roman_number("xiv").
roman_number("xv").
roman_number("xvi").
roman_number("xvii").
roman_number("xviii").
roman_number("xix").
roman_number("xx").

roman_number("II").
roman_number("III").
roman_number("IV").
roman_number("V").
roman_number("VI").
roman_number("VII").
roman_number("VIII").
roman_number("IX").
roman_number("XI").
roman_number("XII").
roman_number("XIII").
roman_number("XIV").
roman_number("XV").
roman_number("XVI").
roman_number("XVII").
roman_number("XVIII").
roman_number("XIX").
roman_number("XX").

digit_dot_digit([H|T]) :-
    isdigit(H),
    digit_dot_digit0(T).

digit_dot_digit0([]).  % only digits is also tag now
digit_dot_digit0([46|T]) :-
    digits(T).
digit_dot_digit0([H|T]) :-
    isdigit(H),
    digit_dot_digit0(T).

digits([H|T]) :-
    isdigit(H),
    digits0(T).

digits0([]).
digits0([46|T]) :-
    digits(T).
digits0([H|T]) :-
    isdigit(H),
    digits0(T).


enumeration(String) :-
    stand_alone_enumeration(String).
enumeration([H]) :-
    isdigit(H).
enumeration([H]) :-
    isalpha(H),
    \+ isaccented(H).
enumeration([H1,H2]) :-
    isdigit(H1),
    isdigit(H2).


% ??
% optional_n_word(X) -->
%     n_word(X).
% optional_n_word(_) --> [].

%% de drie na mooiste ..
phrasal_entry(with_dt(num_na,
                      dt(pp,[hd=l(na,particle(na),P0,P),
			     obj1=l(Num,number(hoofd(pl_num)),detp,0,P0)])
		     ),num_na) -->
    { hdrug_util:debug_message(4,"de drie na Xste~n",[]) },
    number_with_words_or_een(Num,0,P0),
    n_word(na),
    {  P is P0 + 1 }.

number_with_words_or_een(Num,P0,P) -->
    number_with_words(Num,P0,P).
number_with_words_or_een(een,P0,P) -->
    n_word(een),
    {  P is P0 + 1 }.

phrasal_entry(adverb,ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

phrasal_entry(complementizer(np),ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

phrasal_entry(complementizer(pp),ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

phrasal_entry(complementizer(vp),ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

phrasal_entry(complementizer(sbar),ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

phrasal_entry(complementizer(adv),ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

phrasal_entry(complementizer(a),ten_xste) -->
    { hdrug_util:debug_message(4,"ten_xste~n",[]) },
    n_word(ten),
    n_word(Rang),
    { rang(Rang) }.

num_meter_num -->
    number_expression(_),
    n_word(meter),
    number_expression(_).

phrasal_entry(adjective(e),eneenhalve) -->
    { hdrug_util:debug_message(4,"Neneenhalve~n",[]) },
    number_expression(pl_num),
    n_word(en),
    n_word(een),
    n_word(halve).

phrasal_entry(adjective(e),eneenhalve) -->
    { hdrug_util:debug_message(4,"Neneenhalve~n",[]) },
    n_word(ZesEnEenHalve),
    {   atom(ZesEnEenHalve),
	atom_concat(Zes,eneenhalve,ZesEnEenHalve),
	\+ atom_concat(_,e,Zes),
        number_expression(pl_num,[Zes],[])
    }.

phrasal_entry(adjective(e),eneenhalve) -->
    { hdrug_util:debug_message(4,"Neneenhalve~n",[]) },
    n_word(ZesEnEenHalve),
    {   atom(ZesEnEenHalve),
	atom_concat(Zes,'-en-een-halve',ZesEnEenHalve), 
        number_expression(pl_num,[Zes],[])
    }.

phrasal_entry(adjective(e),eneenhalve) -->
    { hdrug_util:debug_message(4,"Neneenhalve~n",[]) },
    n_word(TweeEnEenHalve),
    {   atom(TweeEnEenHalve),
	atom_concat(TweeAcc,neenhalve,TweeEnEenHalve),
	atom_concat(Twee,ë,TweeAcc),
        number_expression(pl_num,[Twee],[])
    }.

phrasal_entry(adjective(e),enhalve) -->
    { hdrug_util:debug_message(4,"Nenhalve~n",[]) },
    number_expression(pl_num),
    n_word(en),
    n_word(halve).

phrasal_entry(adjective(e),enhalve) -->
    { hdrug_util:debug_message(4,"Nenhalve~n",[]) },
    n_word(ZesEnEenHalve),
    {   atom(ZesEnEenHalve),
	atom_concat(Zes,enhalve,ZesEnEenHalve),
	\+ atom_concat(_,e,Zes),
        number_expression(pl_num,[Zes],[])
    }.

phrasal_entry(adjective(e),enhalve) -->
    { hdrug_util:debug_message(4,"Nenhalve~n",[]) },
    n_word(ZesEnEenHalve),
    {   atom(ZesEnEenHalve),
	atom_concat(Zes,'-en-halve',ZesEnEenHalve),
	\+ atom_concat(_,e,Zes),
        number_expression(pl_num,[Zes],[])
    }.

phrasal_entry(adjective(e),enhalve) -->
    { hdrug_util:debug_message(4,"Nenhalve~n",[]) },
    n_word(TweeEnEenHalve),
    {   atom(TweeEnEenHalve),
	atom_concat(TweeAcc,nhalve,TweeEnEenHalve),
	atom_concat(Twee,ë,TweeAcc),
        number_expression(pl_num,[Twee],[])
    }.

never_compound_part(L) :-
    atom(L),
    atom_length(L,1).
never_compound_part(L) :-
    alpino_unknowns:never_compound_part(L).
never_compound_part(L) :-
    alpino_unknowns:contains_never_compound_part(L).

n_word_preposition(Voor) -->
    n_word(Prep),
    {  xl(Prep,preposition(_,_),Voor,[],[]) }.

voor_chr -->
    voor_chr_voor,
    voor_chr_chr.

voor_chr --> n_word('v.Chr').
voor_chr --> n_word('v.Chr.').
voor_chr --> n_word('v.C.').
voor_chr --> n_word('n.Chr').
voor_chr --> n_word('n.Chr.').
voor_chr --> n_word('n.C.').

voor_chr_voor -->
    n_word(v).
voor_chr_voor -->
    n_word('v.').
voor_chr_voor -->
    n_word('voor').

voor_chr_voor -->
    n_word(n).
voor_chr_voor -->
    n_word('n.').
voor_chr_voor -->
    n_word(na).

voor_chr_chr -->
    n_word('Chr.').
voor_chr_chr -->
    n_word('C.').
voor_chr_chr -->
    n_word('Chr').
voor_chr_chr -->
    n_word('Christus').

%% VVV Urk
%% Calvé Nederland
phrasal_entry(proper_name(both,'ORG'),vvv(H0,H1),Ws0,Ws) :-
    hdrug_util:debug_message(4,"VVV Urk~n",[]),
    n_word(VVV,Ws0,Ws1),
    in_names_dictionary(proper_name(both,'ORG'),VVV,_,Ws1,Ws2,H0),
    n_word(Urk,Ws2,Ws3),
    in_names_dictionary(proper_name(both,'LOC'),Urk,_,Ws3,Ws,H1),
    \+ in_names_dictionary(_,VVV,_,Ws1,Ws,_).

phrasal_entry(np,toonsoort) -->
    { hdrug_util:debug_message(4,"toonsoort~n",[]) },
    toon,
    toon_aanvulling.

toon -->
    n_word(Toon),
    { toon(Toon) }.

toon(a).
toon(b).
toon(c).
toon(d).
toon(e).
toon(f).
toon(g).
toon('A').
toon('B').
toon('C').
toon('D').
toon('E').
toon('F').
toon('G').
toon(ais).
toon(as).
toon(bes).
toon(bis).
toon(ces).
toon(cis).
toon(des).
toon(dis).
toon(es).
toon(fis).
toon(gis).

toon_aanvulling --> n_word('kl.'), n_word('t.').
toon_aanvulling --> n_word('kl.t.').
toon_aanvulling --> n_word(kl).
toon_aanvulling --> n_word(mineur).
toon_aanvulling --> n_word(majeur).
toon_aanvulling --> n_word(groot).
toon_aanvulling --> n_word(klein).


phrasal_entry(Tag,'Nx') -->
    { hdrug_util:debug_message(4,"Nx~n",[]) },
    n_word(Nx),
    {  atom(Nx),
       atom_concat(NAtom,x,Nx),
       parse_number_simple(NAtom),
       nx_tag(Tag)
    }.

nx_tag(tmp_noun(both,count,bare_meas)).
nx_tag(tmp_noun(both,count,bare_meas,measure)).

capitals([],[]).
capitals([C|Cs],[D|Ds]):-
    alpino_unknowns:decap_first(C,D),
    capitals(Cs,Ds).

long_single_letter_sequence([A,B,C|Tail]) -->
    single_letter(A),
    single_letter(B),
    single_letter(C),
    single_letter_sequence(Tail).

single_letter_sequence([H|T]) -->
    single_letter(H),!,
    single_letter_sequence(T).
single_letter_sequence([H]) -->
    single_letter_dot(H),!.
single_letter_sequence([]) --> [].

single_letter(X) -->
    [X], { single_letter(X) }.

single_letter(ch).
single_letter(ck).
single_letter(ff).
single_letter(Atom) :-
    atom(Atom),
    atom_codes(Atom,[Code]),
    isalpha(Code).

single_letter_dot(Atom0) -->
    [Atom],
    {  atom(Atom),
       atom_codes(Atom,[Code,46]), %% 46 = '.'
       isalpha(Code),
       atom_codes(Atom0,[Code])
    }.

phrasal_entry(proper_name(both),url) -->
    n_word(uriblank).

phrasal_entry(proper_name(both),url) -->
    n_word(emailblank).

phrasal_entry(proper_name(both),url, Ws0, Ws) :-
    hdrug_util:debug_message(4,"url~n",[]),
    n_word(URL,Ws0,Ws),
    (   atom(URL),
	atom_concat('http://',URL1,URL)
    ;   URL=URL1
    ),
    atom_codes(URL1,Codes),
    url_codes(Codes).


%% for wrongly tokenized URL in SONAR500

%% www. ad. nl.
%% www. tate. org. uk
phrasal_entry(proper_name(both),url) -->
    fully_qualified_domain_name.

%% hugo @ ad. nl
phrasal_entry(proper_name(both),url) -->
    n_word(_),
    n_word('@'),
    fully_qualified_domain_name.

fully_qualified_domain_name -->
    ends_with_dot_seq1,
    n_word(Domain),
    { tld_domain(Domain) }.

fully_qualified_domain_name -->
    ends_with_dot_seq,
    n_word(Domain0),
    { ends_with_dot(Domain0,Domain),
      tld_domain(Domain)
    }.

ends_with_dot_seq --> [].
ends_with_dot_seq -->
    n_word(Word),
    {  ends_with_dot(Word,_) },
    ends_with_dot_seq.

ends_with_dot_seq1 -->
    n_word(Word),
    {  ends_with_dot(Word,_) },
    ends_with_dot_seq.

ends_with_dot(Atom,Atom1) :-
    atom(Atom),
    atom_concat(Atom1,'.',Atom),
    \+ alpino_lex:lexicon__(Atom,_,_,[],[],_,[]).

url_codes(Codes) :-
    alpino_util:split_string(Codes,"/",[Domain|_]),
    alpino_util:split_string(Domain,".",List),
    url_codes_list(List).

url_codes_list([_A,H|T]) :-
    url_codes_list1(T,H).

url_codes_list1([],H):-
    url_end(H).
url_codes_list1([H|T],_):-
    url_codes_list1(T,H).

url_end(TLD) :-
    atom_codes(TLD_Atom,TLD),
    tld_domain(TLD_Atom).

tld_domain(at).
tld_domain(au).
tld_domain(be).
tld_domain(biz).
tld_domain(ca).
tld_domain(ch).
tld_domain(cn).
tld_domain(com).
tld_domain(de).
tld_domain(dk).
tld_domain(edu).
tld_domain(es).
tld_domain(fi).
tld_domain(fr).
tld_domain(gr).
tld_domain(gov).
tld_domain(hr).
tld_domain(hu).
tld_domain(info).
tld_domain(int).
tld_domain(ir).
tld_domain(it).
tld_domain(jp).
tld_domain(mil).
tld_domain(mt).
tld_domain(net).
tld_domain(nl).
tld_domain('nl.').  % also an abbreviation
tld_domain(no).
tld_domain(nu).
tld_domain(org).
tld_domain(pt).
tld_domain(ro).
tld_domain(ru).
tld_domain(se).
tld_domain(sl).
tld_domain(sk).
tld_domain(to).
tld_domain(tr).
tld_domain(uk).
tld_domain(va).  % Vatican

economy(business).
economy(coach).
economy(economy).
economy(standard).
economy(tourist).


phrasal_entry(punct(Type),strange_punct) -->
    n_word(Word),
    {  \+ punctuation(Word),  % ordinary punct in dictionary
       punct(Word,Type)
    }.

hash_tag(Atom) :-
    atom(Atom),
    atom_concat('#',_A,Atom).


rewrite_stem([],[]).
rewrite_stem([H0|T0],[H|T]) :-
    rewrite_stem0(H0,H),
    !,
    rewrite_stem(T0,T).

rewrite_stem0(duizend,duizend).  % not _duizend
rewrite_stem0(AtomDuizend,Atom_Duizend) :-
    atom(AtomDuizend),
    atom_concat(Atom,'-duizend',AtomDuizend),
    atom_concat(Atom,'_duizend',Atom_Duizend).
rewrite_stem0(AtomDuizend,Atom_Duizend) :-
    atom(AtomDuizend),
    atom_concat(Atom,duizend,AtomDuizend),
    atom_concat(Atom,'_duizend',Atom_Duizend).
rewrite_stem0(A,A).

formula(Word) :-
    atom(Word),
    atom_concat(formula_,DigitAtom,Word),
    atom_codes(DigitAtom,Codes),
    number_codes_silent(_,Codes).

telephone -->
    tel_land,
    tel_word,
    opt_dash,
    tel_word,
    telephone_rest.

opt_dash --> [].
opt_dash --> n_word('-').

telephone_rest --> [].
telephone_rest -->
    tel_word,
    telephone_rest.

tel_word -->
    n_word(Atom),
    {  tel_digits(Atom) }.

tel_digits(Atom) :-
    atom(Atom),
    atom_codes(Atom,Codes),
    isdigits(Codes).

tel_land --> [].

tel_land -->
    n_word('('),
    tel_land,
    n_word(')').

tel_land -->
    n_word('+'),
    n_word(Atom),
    {  tel_digits(Atom) }.

tel_land -->
    n_word(Atom),
    {  tel_digits(Atom) }.

tel_land -->
    n_word(Atom),
    {  atom_concat('+',Atom1,Atom),
       tel_digits(Atom1)
    }.

isdigits([]).
isdigits([H|T]) :-
    isdigit(H),
    isdigits(T).
