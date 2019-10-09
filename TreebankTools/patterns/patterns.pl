:- use_module(library(lists)).
:- initialize_flag(analyse_triples_mode,sterf).

:- initialize_flag(analyse_triples_format,text).  % or prolog
%:- initialize_flag(analyse_triples_format,prolog).  % or text

:- multifile option/3.
option(analyse_triples) -->
    !,
    no_dash(Files),
    !,
    { set_flag(batch_command,analyse_triples_files(Files)) }.

analyse_triples_files([]).
analyse_triples_files([H|T]) :-
    lists:last([H|T],Last),
    debug_message(1,"checking files ~w to ~w~n",[H,Last]),
    analyse_triples_files_([H|T]).

analyse_triples_files_([]).
analyse_triples_files_([H|T]) :-
    (   analyse_triples_file(H),
	fail
    ;   true
    ),
    analyse_triples_files_(T).

analyse_triples_file(File) :-
    debug_message(2,"checking file ~w~n",[File]),
    xml_file_to_dt(File,DT,Sentence,_),
    debug_message(2,"   ~s~n",[Sentence]),
    dt_to_relations_with_full_postags(DT,Rels),
    hdrug_flag(analyse_triples_mode,Mode),
    analyse_triples(Mode,Rels,File,Sentence,Tuple),
    hdrug_flag(analyse_triples_format,FormatMode),
    analyse_triples_format(FormatMode,Tuple).

analyse_triples_format(prolog,Tuple) :-
    format("~q.~n",Tuple).
analyse_triples_format(text,Tuple) :-
    Tuple =.. [_|T],
    analyse_triples_format_text_list(T).

analyse_triples_format_text_list([]).
analyse_triples_format_text_list([H|T]) :-
    format("~w",[H]),
    analyse_triples_format_text_list2(T).

analyse_triples_format_text_list2([]) :-
    format("~n",[]).
analyse_triples_format_text_list2([H|T]) :-
    format("|~w",[H]),
    analyse_triples_format_text_list2(T).

%% reconstruct phrase from head
projection(HowHd,Sentence,Rels,How) :-
    findall(Q,connected(HowHd,Rels,[HowHd],Q),Ps0),
    sort(Ps0,[P0|Ps]),
    append(_,[P],Ps),
    sentence_to_words(Sentence,Words),
    alpino_treebank:get_root(P0,P,Words,How).

%% reconstruct phrase from head
lexical_projection(HowHd,Sentence,Rels,How) :-
    findall(Q,lexical_connected(HowHd,Rels,[HowHd],Q),Ps0),
    sort(Ps0,[P0|Ps]),
    append(_,[P],Ps),
    sentence_to_words(Sentence,Words),
    alpino_treebank:get_root(P0,P,Words,How).

connected(_:_/[P0,_],_,_,P0).
connected(_:_/[_,P],_,_,P).
connected(Hd,Rels,His,P) :-
    member(deprel(Hd,Type,Arg),Rels),
    \+ wrong_connected_type(Type),
    \+ member(Arg,His),
    connected(Arg,Rels,[Arg|His],P).

lexical_connected(_:_/[P0,_],_,_,P0).
lexical_connected(_:_/[_,P],_,_,P).
lexical_connected(Hd,Rels,His,P) :-
    member(deprel(Hd,mwp/mwp,Arg),Rels),
    lexical_connected(Arg,Rels,[Arg|His],P).

wrong_connected_type(rhd/body).
wrong_connected_type(whd/body).
wrong_connected_type(dp/dp).

sentence_to_words([],[]).
sentence_to_words([StrH|StrT],[Word|Words]) :-
    first_word([StrH|StrT],WordCodes,[],StringRest),
    atom_codes(Word,WordCodes),
    sentence_to_words(StringRest,Words).

first_word([],W,W,[]).
first_word([32|Rest],W,W,Rest) :-
    !.
first_word([H|T],[H|W0],W,Rest) :-
    first_word(T,W0,W,Rest).

%% get the name from an NP (either its head, or the name inside 
%% an apposition NP, or the name inside a conjunct
name_in_np(Frame:Name/[P,Q],Sentence,Rels,FullName) :-
    alpino_postags:attribute_of_frame(Frame,neclass,name,'PER'),
    lexical_projection(Frame:Name/[P,Q],Sentence,Rels,FullName).
name_in_np(Su,Sentence,Rels,Name) :-
    member(deprel(Su,hd/app,App),Rels),
    name_in_np(App,Sentence,Rels,Name).
name_in_np(Su,Sentence,Rels,Name) :-
    member(deprel(Su,crd/cnj,Cnj),Rels),
    name_in_np(Cnj,Sentence,Rels,Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VARIOUS PATTERNS COULD BE INCLUDED HERE %%%
%%% selected with analyse_triples_mode flag %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% MODE: STERF
%% WHO DIED HOW

analyse_triples(detless_pp,Rels,File,Sentence,Triples) :-
    analyse_triples_detless_pp(Rels,File,Sentence,Triples).

analyse_triples(sterf,Rels,File,Sentence,sterf(Who,How,File,Id)) :-
    analyse_triples_sterf(Rels,Sentence,Who,How,Id).

%% N gaat dood / overlijdt / sterft / bezwijkt aan ZIEKTE
analyse_triples_sterf(Rels,Sentence,Who,How,Sterf-aan) :-
    member(deprel(Verb:Sterf/P,hd/su,Su),Rels),
    (  Sterf = sterf ; Sterf = ga_dood ; Sterf = overlijd ; Sterf = bezwijk ), 
    member(deprel(Verb:Sterf/P,hd/pc,Prep:aan/Aan),Rels),
    member(deprel(Prep:aan/Aan,hd/obj1,HowHd),Rels),
    name_in_np(Su,Sentence,Rels,Who),
    projection(HowHd,Sentence,Rels,How).

%% N gaat dood / overlijdt / sterft / bezwijkt aan ZIEKTE
analyse_triples_sterf(Rels,Sentence,Who,How,Sterf-door) :-
    member(deprel(Verb:Sterf/P,hd/su,Su),Rels),
    (  Sterf = sterf ; Sterf = ga_dood ; Sterf = overlijd ; Sterf = bezwijk ), 
    member(deprel(Verb:Sterf/P,hd/mod,Prep:door/Aan),Rels),
    member(deprel(Prep:door/Aan,hd/obj1,HowHd),Rels),
    name_in_np(Su,Sentence,Rels,Who),
    projection(HowHd,Sentence,Rels,How).

%% N pleegde zelfmoord
analyse_triples_sterf(Rels,Sentence,Who,zelfmoord,pleeg_zelfmoord) :-
    member(deprel(Verb:pleeg/P,hd/su,Su),Rels),
    member(deprel(Verb:pleeg/P,hd/obj1,_Noun:zelfmoord/_),Rels),
    name_in_np(Su,Sentence,Rels,Who).

%% N hing zich op
analyse_triples_sterf(Rels,Sentence,Who,zelfmoord,zich_hang_op) :-
    member(deprel(Verb:hang_op/P,hd/su,Su),Rels),
    member(deprel(Verb:hang_op/P,hd/_,_Noun:zich/_),Rels),
    name_in_np(Su,Sentence,Rels,Who).

%% de zelfmoord van N
analyse_triples_sterf(Rels,Sentence,Who,zelfmoord,zelfmoord_van) :-
    member(deprel(_Noun:zelfmoord/_,hd/mod,Prep:van/Q),Rels),
    member(deprel(Prep:van/Q,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% de moord op N
analyse_triples_sterf(Rels,Sentence,Who,vermoord,moord_op) :-
    member(deprel(_Noun:moord/_,hd/mod,Prep:op/Q),Rels),
    member(deprel(Prep:op/Q,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% N komt om het leven bij/door/toen/tijdens/nadat ONGELUK
analyse_triples_sterf(Rels,Sentence,Who,How,kom_om_het_leven) :-
    member(deprel(Verb:kom/P,hd/su,Su),Rels),
    member(deprel(Verb:kom/P,hd/svp,OmHetLeven),Rels),
    lexical_projection(OmHetLeven,Sentence,Rels,'om het leven'),
    name_in_np(Su,Sentence,Rels,Who),
    member(deprel(Verb:kom/P,hd/mod,Prep:Q/Ps),Rels),
    (   (  Q = bij  ; Q = door  ; Q = tijdens )
    ;   (  Q = toen ; Q = nadat )
    ),
    projection(Prep:Q/Ps,Sentence,Rels,How).

%% N komt om bij/door/toen/tijdens/nadat ONGELUK
analyse_triples_sterf(Rels,Sentence,Who,How,kom_om) :-
    member(deprel(Verb:kom_om/P,hd/su,Su),Rels),
    name_in_np(Su,Sentence,Rels,Who),
    member(deprel(Verb:kom_om/P,hd/mod,Prep:Q/Ps),Rels),
    (   (  Q = bij  ; Q = door  ; Q = tijdens )
    ;   (  Q = toen ; Q = nadat )
    ),
    projection(Prep:Q/Ps,Sentence,Rels,How).

%% N is vermoord
analyse_triples_sterf(Rels,Sentence,Who,vermoord,vermoord) :-
    member(deprel(_:ben/_,hd/vc,VerbB:vermoord/V),Rels),
    member(deprel(VerbB:vermoord/V,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% N verongelukt
analyse_triples_sterf(Rels,Sentence,Who,verongelukt,verongelukt) :-
    member(deprel(_Verb:verongeluk/_,hd/su,Su),Rels),
    name_in_np(Su,Sentence,Rels,Who).

%% N om het leven brengen
analyse_triples_sterf(Rels,Sentence,Who,vermoord,breng_om_het_leven) :-
    member(deprel(Verb:breng/P,hd/obj1,Obj1),Rels),
    member(deprel(Verb:breng/P,hd/svp,OmHetLeven),Rels),
    lexical_projection(OmHetLeven,Sentence,Rels,'om het leven'),
    name_in_np(Obj1,Sentence,Rels,Who).

%% N dood schieten
analyse_triples_sterf(Rels,Sentence,Who,schiet_dood,schiet_dood) :-
    member(deprel(_Verb:schiet_dood/_,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% N dood martelen
analyse_triples_sterf(Rels,Sentence,Who,martel_dood,martel_dood) :-
    member(deprel(_Verb:martel_dood/_,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% N dood slaan
analyse_triples_sterf(Rels,Sentence,Who,sla_dood,sla_dood) :-
    member(deprel(_Verb:sla_dood/_,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% N dood slaan
analyse_triples_sterf(Rels,Sentence,Who,steek_dood,steek_dood) :-
    member(deprel(_Verb:steek_dood/_,hd/obj1,Obj1),Rels),
    name_in_np(Obj1,Sentence,Rels,Who).

%% de vermoorde N
analyse_triples_sterf(Rels,Sentence,Who,vermoord,vermoorde) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:vermoord/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% N verdronk
%% todo: there should be no object
analyse_triples_sterf(Rels,Sentence,Who,verdrink,verdrink) :-
    member(deprel(_Verb:verdrink/_,hd/su,Su),Rels),
    name_in_np(Su,Sentence,Rels,Who).

%% de verdronken N
analyse_triples_sterf(Rels,Sentence,Who,verdrink,verdronken) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:verdronken/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% de verongelukte N
analyse_triples_sterf(Rels,Sentence,Who,verongelukt,verongelukte) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:verongelukt/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% de doodgestoken N
analyse_triples_sterf(Rels,Sentence,Who,doodgestoken,doodgestoken) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:doodgestoken/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% de doodgeslagen N
analyse_triples_sterf(Rels,Sentence,Who,doodgeslagen,doodgeslagen) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:doodgeslagen/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% de doodgeschoten N
analyse_triples_sterf(Rels,Sentence,Who,doodgeschoten,doodgeschoten) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:doodgeschoten/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% de doodgemarteld N
analyse_triples_sterf(Rels,Sentence,Who,doodgemarteld,doodgemarteld) :-
    member(deprel(Noun:President/P,hd/mod,_Adj:doodgemarteld/_),Rels),
    name_in_np(Noun:President/P,Sentence,Rels,Who).

%% X vond de dood toen/bij/omdat...
analyse_triples_sterf(Rels,Sentence,Who,How,vind_de_dood) :-
    member(deprel(Verb:vind/P,hd/su,Su),Rels),
    name_in_np(Su,Sentence,Rels,Who),
    member(deprel(Verb:vind/P,hd/obj1,Noun:dood/Q),Rels),
    member(deprel(Noun:dood/Q,hd/det,_Det:de/_),Rels),
    member(deprel(Verb:vind/P,hd/mod,Prep:Q/Ps),Rels),
    (   (  Q=bij ; Q=door; Q=tijdens )
    ;   (  Q=toen; Q=omdat; Q=doordat; Q=terwijl; Q=nadat )
    ),
    projection(Prep:Q/Ps,Sentence,Rels,How).

%% MODE: WIN
%% WHO WON WHAT

analyse_triples(win,Rels,File,Sentence,win(Who,What,File,Id)) :-
    analyse_triples_win(Rels,Sentence,Who,What,Id).

%% Name won X
analyse_triples_win(Rels,Sentence,Who,What,win) :-
    Win = win,
    member(deprel(Verb:Win/P,hd/su,Su),Rels),
    member(deprel(Verb:Win/P,hd/obj1,WhatHd),Rels),
    name_in_np(Su,Sentence,Rels,Who),
    projection(WhatHd,Sentence,Rels,What).

%% Name is gewonnen door X
analyse_triples_win(Rels,Sentence,Who,What,is_gewonnen_door) :-
    Win = win,
    ( Is = ben ; Is = word ),
    member(deprel(VerbA:Is/P,hd/su,WhatHd),Rels),
    member(deprel(VerbB:Win/Q,hd/obj1,WhatHd),Rels),
    member(deprel(VerbA:Is/P,hd/vc,VerbB:Win/Q),Rels),
    member(deprel(VerbB:Win/Q,hd/mod,Prep:door/R),Rels),
    member(deprel(Prep:door/R,hd/obj1,Door),Rels),
    name_in_np(Door,Sentence,Rels,Who),
    projection(WhatHd,Sentence,Rels,What).

%% Name , winnaar van X
analyse_triples_win(Rels,Sentence,Who,What,winnaar) :-
    member(deprel(NounA:Wie/P,hd/app,NounB:winnaar/Q),Rels),
    name_in_np(NounA:Wie/P,Sentence,Rels,Who),
    member(deprel(NounB:winnaar/Q,hd/mod,Prep:van/R),Rels),
    member(deprel(Prep:van/R,hd/obj1,WhatHd),Rels),
    projection(WhatHd,Sentence,Rels,What).

%% Name werd [ winnaar van X ]
%% Name werd [ winnaar ] van X
analyse_triples_win(Rels,Sentence,Who,What,is_winnaar_van) :-
    ( Is = ben ; Is = word ; Is = bleek ),
    member(deprel(Verb:Is/P,hd/su,WhoNP),Rels),
    member(deprel(Verb:Is/P,hd/predc,Noun:winnaar/Q),Rels),
    (   member(deprel(Noun:winnaar/Q,hd/mod,Prep:van/R),Rels)
    ;   member(deprel(Verb:Is/P,hd/mod,Prep:van/R),Rels)
    ),
    member(deprel(Prep:van/R,hd/obj1,WhatHd),Rels),
    name_in_np(WhoNP,Sentence,Rels,Who),
    projection(WhatHd,Sentence,Rels,What).
    
%% Name won de finale van X
%% Name sleept (hoofd)prijs in de wacht
%% Name behaalde de 1e plaats van/bij X
%% todo....


analyse_triples(krijgen_passive,Rels,File,Sentence,Triples) :-
    analyse_triples_krijgen_passive(Rels,File,Sentence,Triples).

analyse_triples_krijgen_passive(Rels,File,Sentence,
				s('SU',Subject,Tag,Head,File)) :-
    member(deprel(_:krijg/_,hd/su,Su),Rels),
    Su = Tag:Head/_,
    projection(Su,Sentence,Rels,Subject).

analyse_triples_krijgen_passive(Rels,File,_Sentence,s('VERB',Verb,File)) :-
    member(deprel(_:krijg/_,hd/vc,_:Verb/_),Rels).


%%
analyse_triples(v_pp,Rels,File,Sentence,Triples) :-
    analyse_triples_v_pp(Rels,File,Sentence,Triples).

analyse_triples_v_pp(Rels,_File,_Sentence,v_pp(P,Noun)) :-
    member(deprel(Verb:_,hd/mod,Prep:P/Ps),Rels),
    alpino_postags:postag_of_frame(Verb,verb),
    alpino_postags:postag_of_frame(Prep,prep),
    member(deprel(Prep:P/Ps,hd/obj1,NounPos:Noun/_),Rels),
    alpino_postags:postag_of_frame(NounPos,noun).

%%
analyse_triples(v_ld_pp,Rels,File,Sentence,Triples) :-
    analyse_triples_v_ld_pp(Rels,File,Sentence,Triples).

analyse_triples_v_ld_pp(Rels,_File,_Sentence,v_ld_pp(P,Noun)) :-
    member(deprel(Verb:_,hd/ld,Prep:P/Ps),Rels),
    alpino_postags:postag_of_frame(Verb,verb),
    alpino_postags:postag_of_frame(Prep,prep),
    member(deprel(Prep:P/Ps,hd/obj1,NounPos:Noun/_),Rels),
    alpino_postags:postag_of_frame(NounPos,noun).

analyse_triples(n_pp,Rels,File,Sentence,Triples) :-
    analyse_triples_n_pp(Rels,File,Sentence,Triples).

analyse_triples_n_pp(Rels,_File,_Sentence,n_pp(NounVal1,P,NounVal2)) :-
    member(deprel(NounPos1:NounVal1/_,hd/mod,Prep:P/Ps),Rels),
    alpino_postags:postag_of_frame(NounPos1,noun),
    alpino_postags:postag_of_frame(Prep,prep),
    member(deprel(Prep:P/Ps,hd/obj1,NounPos2:NounVal2/_),Rels),
    alpino_postags:postag_of_frame(NounPos2,noun).

