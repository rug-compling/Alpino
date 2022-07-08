%           -*-Mode: prolog;-*-

:- module(alpino_penalties, [ assign_score/4,
			      count_maxent_features/5
			    ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(hdrug(hdrug_util)).
:- use_module(alpino('src/utils')).
:- use_module(library(charsio)).
:- use_module(library(lists)).

%% NB: sorting is used to get the best value first. However, beware:
%% | ?- sort([0,0.0,3,3.0,5,5.0],L).
%%
%% L = [0.0,3.0,5.0,0,3,5] ? 
%% 

assign_score(Cat,Tree,Frames,Score) :-
    hdrug_flag(list_all_maxent_features,Bool),
    assign_score(Bool,Cat,Tree,Frames,Score).

assign_score(on,Cat,Tree,Frames,p(Score,His)) :-
    count_maxent_features(Cat,Tree,Frames,His,on),
    penalty_weights(His,Score).

assign_score(off,Cat,Tree,Frames,p(Score,[])) :-
    count_maxent_features(Cat,Tree,Frames,His,off),
    penalty_weights(His,Score).

count_maxent_features(Cat,Tree,Frames,Pens,Bool) :-
    alpino_data:cat_to_result(Cat,Result),
    alpino_dt:result_to_dt_simple(Result,DT),
    construction_features(Tree,DT,Frames,PensInitial,Pens1,Bool),
    output_features(Tree,DT,Pens1,Pens2),
    hdrug_flag(include_gen_output_features,BoolGen),
    include_gen_output_features(BoolGen,Tree,Pens2,[]),
    hdrug_util:hdrug_flag(application_type,Domain),
    (   Domain == undefined
    ->  PensInitial = Pens
    ;   domain_features(PensInitial,Pens,Domain)
    ).

domain_features([],[],_).
domain_features([H|T],[H|Features0],Domain) :-
    domain_feature(H,Domain,Features0,Features),
    domain_features(T,Features,Domain).

domain_feature(Domain:_,Domain,F0,F):-
    !,
    F0=F.
domain_feature(weight(_W),_,F0,F) :-
    !,
    F0=F.
domain_feature(Feature-Count,Domain,F0,F) :-
    !,
    F0=[(Domain:Feature)-Count|F].
domain_feature(Feature,Domain,[Domain:Feature|F],F).

:- initialize_flag(include_gen_output_features,off).
include_gen_output_features(on,Tree,Pens0,Pens) :-
    alpino_fluency_maxent:output_features(Tree,Pens0,Pens).
include_gen_output_features(off,_,P,P).
include_gen_output_features(undefined,_,P,P).

output_features(_,DT,Pens0,Pens) :-
    deprel_features(DT,Pens0,Pens).

%% USED BY alpino_fluency_maxent module!
construction_features(Tree,DT,Frames,P0,P) :-
   construction_features(Tree,DT,Frames,P0,P,on).

construction_features(Tree,DT,Frames,Pens0,Pens,Bool) :-    
    tree_penalties(Bool,Tree,Pens0,Pens1),
    global_tree_penalties(Tree,Pens1,Pens2),
    distance_score(DT,Pens2,Pens3),
    frames_features(Frames,Pens3,Pens).

frames_features(Frames,P0,P) :-
    findall(Pen,frame_feature(Frames,Pen),P0,P).

frame_feature(Frames,Pen) :-
    nv_member(frame(_P0,_P,Q0,Q,Stem,Frame,Surf,His),Frames),
    debug_message(6,"frame features for frame ~w:~n",[Frame]),
    alpino_dt:somewhat_simplify_frame(Frame,Frame1),
    (   nonvar(Q),
	nonvar(Q0)	% for generation these are not instantiated...
    ->  Len is Q - Q0
    ;   true
    ),
    lexical_penalty_frame(His,Stem,Surf,Frame,Frame1,Len,Pen),
    debug_message(6,"    ~w:~n",[Pen]).


%%% TODO!    

g_subtree(Tree,Tree).
g_subtree(tree(_,_,Ds,_),Tree) :-
    lists:member(El,Ds),
    g_subtree(El,Tree).

global_tree_penalties(Tree,His0,His) :-
    findall(H,global_tree_penalty(Tree,H),His0,His).

global_tree_penalty(Tree,H) :-
    g_subtree(Tree,SubTree),
    global_tree_penalty1(SubTree,H).

global_tree_penalty1(tree(Cat,_,_,_),H):-
    global_tree_penalty_cat(Cat,H).

global_tree_penalty_cat(Cat,coord(Conj,Agr)) :-
    coord_agr(Cat,Conj,Agr).


nv_member(El,List) :-
    nonvar(List),
    List = [H|T],
    nv_member_([H|T],El).

nv_member_([El|_],El).
nv_member_([_|Tail],El) :-
    nv_member(El,Tail).

deprel_features(DT,H0,H) :-
    (   alpino_dt:dt_to_relations_with_somewhat_simplified_postags(DT,[Tr|Triples])
    ->  findall(Fea,triples_to_feature([Tr|Triples],Fea),His0),
	integrate_corpus_frequency_features(His0,His),
	append(His,H1,H0)
    ;   H0=H1
    ),
    (   alpino_dt:dt_to_relations_with_full_postags(DT,[Tr1|Triples1])
    ->  findall(Fea,full_triples_to_feature([Tr1|Triples1],Fea),H1,H2)
    ;   H1 = H2
    ),
    findall(Feature,deprel_tree_feature(DT,Feature),H2,H).

deprel_tree_feature(tree(r(top,p(Cat)),_,_),topcat(Cat)).

full_triples_to_feature(List,rel_mismatch(Rel)) :-
    member(deprel(noun(De,Count,Sg):_/_,hd/mod,rel_pronoun(RelDe,Case):Rel/_),List),
    rel_mismatch(De,Count,Sg,RelDe,Case).

rel_mismatch(het,_,sg,de,_).
rel_mismatch(de,_,sg,het,_).

triple_to_feature(DEPREL,FEATURE) :-
    hdrug_flag(dep_with_pos,Val),
    triple_to_feature(Val,DEPREL,FEATURE).

:- initialize_flag(dep_with_pos,off).

triple_to_feature(_,deprel(ArgPos:Arg/_,Rel,_),dep1(Rel,ArgPos,Arg)).
triple_to_feature(_,deprel(_,Rel,ArgPos:Arg/_),dep2(Rel,ArgPos,Arg)).
triple_to_feature(_,deprel(HdPos:_Hd/_,Rel,ArgPos:_Arg  /_),dep23(    ArgPos,Rel,HdPos   )) :- Rel \= cnj/cnj.
triple_to_feature(off,deprel(HdPos: Hd/_,Rel,ArgPos: Arg  /_),dep35(Arg,ArgPos,Rel,HdPos,Hd)).
triple_to_feature(_,deprel(HdPos:_Hd/_,Rel,ArgPos: Arg  /_),dep34(Arg,ArgPos,Rel,HdPos   )) :- Rel \= cnj/cnj.

triple_to_feature(on,deprel(HdPos: Hd/HdP,Rel,ArgPos: Arg  /ArgP),dep35(Arg/ArgP,ArgPos,Rel,HdPos,Hd/HdP)).

triple_member([TrH|TrT],Triple) :-
    member(Triple,[TrH|TrT]).
triple_member([TrH|TrT],deprel(verb:Head,Rel,noun:Noun)) :-
    member(deprel(noun:Noun,hd/mod,pron(rel):Pron/Ps), [TrH|TrT]),
    die_rel_pron(Pron),
    member(deprel(verb:Head,Rel,pron(rel):Pron/Ps),    [TrH|TrT]).
triple_member([TrH|TrT],deprel(verb(SC):Head,Rel,noun:Noun)) :-
    member(deprel(noun:Noun,hd/mod,pron(rel):Pron/Ps), [TrH|TrT]),
    die_rel_pron(Pron),
    member(deprel(verb(SC):Head,Rel,pron(rel):Pron/Ps),[TrH|TrT]).
triple_member([TrH|TrT],deprel(HD,REL,CNJ)) :-
    member(deprel(HD,REL,VG:En/EnPs), [TrH|TrT]),
    vg(VG),
    member(deprel(VG:En/EnPs,crd/cnj,CNJ),[TrH|TrT]).
triple_member([TrH|TrT],deprel(CNJ1,cnj/cnj,CNJ2)) :-
    select(deprel(VG:En/EnPs,crd/cnj,CNJ1),[TrH|TrT],Triples1),
    vg(VG),
    member(deprel(VG:En/EnPs,crd/cnj,CNJ2),Triples1),
    ordered(CNJ1,CNJ2).

/*
- too infrequent?
- wier also for plural?
- wiens is "wie" in LassyLarge !?

%% de moeder wier kinderen ongelukkig waren -> moeder+wier hd/rel
triple_member([TrH|TrT],deprel(noun:Noun/PosN,hd/rel,det(rwh):Wier/WierPos)) :-
    select(deprel(noun:NounX/PosX,rhd/body,_),[TrH|TrT],[TrH2|TrT2]),
    select(deprel(noun:NounX/PosX,hd/det,det(rwh):Wier/WierPos),[TrH2|TrT2],[TrH3|TrT3]),
    Wier \= welk,
    member(deprel(noun:Noun/PosN,hd/mod,noun:NounX/PosX),[TrH3|TrT3]).

%% de moeder met wier kinderen we naar Artis gingen -> moeder+wier hd/rel
triple_member([TrH|TrT],deprel(noun:Noun/PosN,hd/rel,det(rwh):Wier/WierPos)) :-
    select(deprel(prep:Prep/PosPrep,rhd/body,_),[TrH|TrT],[TrH2|TrT2]),
    select(deprel(prep:Prep/PosPrep,hd/obj1,noun:NounX/PosX),[TrH2|TrT2],[TrH3|TrT3]),
    select(deprel(noun:NounX/PosX,hd/det,det(rwh):Wier/WierPos),[TrH3|TrT3],[TrH4|TrT4]),
    Wier \= welk,
    member(deprel(noun:Noun/PosN,hd/mod,prep:Prep/PosPrep),[TrH4|TrT4]).
*/

%% komt te staan/zitten/hangen/liggen/weten/spreken/overlijden/vervallen
%% does not yield anything yet, but I believe in it!
triple_member([TrH|TrT],deprel(HD,hd/vc_body,V)) :-
    select(deprel(HD,hd/vc,comp:te/Te),[TrH|TrT],Triples1),
    member(deprel(comp:te/Te,cmp/body,V),Triples1).

ordered(_:_/Pos1,_:_/Pos2) :-
    Pos1 @< Pos2.
%    CNJ1 @< CNJ2,  % generate each pair only once

vg(vg).
vg(vg(left)).
                
die_rel_pron(dat).
die_rel_pron(die).
die_rel_pron(hetgeen).
die_rel_pron(hetwelk).
die_rel_pron(wat).
die_rel_pron(welk).
die_rel_pron(wie).

triples_to_feature(List,Feature) :-
    triple_member(List,Triple),
    triple_to_feature(Triple,Feature).

%% learn that "van X tot Y" usually attach to the same head
triples_to_feature(Triples,van_tot(Prep1,Prep2)) :-
    triple_member(Triples,deprel(Hd,hd/mod,prep:Prep1/[Pos0,_])),
    triple_member(Triples,deprel(Hd,hd/mod,prep:Prep2/[Pos2,_])),
    Pos0 @< Pos2.

%% learn that 'om kwart over twee' typically is *not* a PC or LD
triples_to_feature(Triples,sdep(HdPos,Rel,Pos)) :-
    triple_member(Triples,deprel(HdPos:_,_/Rel,prep:Prep)),
    triple_member(Triples,deprel(prep:Prep,hd/obj1,Pos:_)).

%% learn that 'om kwart over twee' typically is *not* a PC or LD
triples_to_feature(Triples,sdep(Rel,Pos)) :-
    triple_member(Triples,deprel(_,_/Rel,prep:Prep)),
    triple_member(Triples,deprel(prep:Prep,hd/obj1,Pos:_)).

%% where do temporal phrases such as 'in 1999' attach to?
%% useful for questions
triples_to_feature(Triples,in_year(Verb)) :-
    triple_member(Triples,deprel(Pos:_/_,hd/mod,prep:in/Ps)),
    triple_member(Triples,deprel(prep:in/Ps,hd/obj1,noun(year):_/_)),
    functor(Pos,Verb,_).

%% where do temporal phrases such as 'in mei' attach to?
triples_to_feature(Triples,in_tmp(Verb)) :-
    triple_member(Triples,deprel(Pos:_/_,hd/mod,prep:in/Ps)),
    triple_member(Triples,deprel(prep:in/Ps,hd/obj1,noun(tmp):_/_)),
    functor(Pos,Verb,_).

%% 'buitenlandse zaken Fisher' is not a constituent
triples_to_feature(Triples,appos_person(TYPE,Noun)) :-
    triple_member(Triples,deprel(Hd,hd/app,Name)),
    noun_root(Hd,Noun),
    name_type(Name,TYPE,Triples).

triples_to_feature(Triples,appos_person(TYPE,Noun)) :-
    triple_member(Triples,deprel(Name,hd/app,Hd)),
    noun_root(Hd,Noun),
    name_type(Name,TYPE,Triples).

%% it is a wh question
triples_to_feature(Triples,q(Feature)) :-
    tf(Triples,q(Feature)).

tf(Triples,q(Feature)) :-
    member(deprel(WH,whd/body,Body),Triples),
    find_wh_trace(Body,WH,Arg,Rel,Triples,[Body]),
    find_wh_word(WH,Triples,WHword,[WH]),
    Arg = _:VERB/_,
    WH = WHPOS:_/_,
    (   Feature = wh_role(Rel)
    ;   Feature = wh_role(WHPOS,Rel)
    ;   Feature = wh_role(WHPOS,WHword,Rel)
    ;   Feature = wh_role(WHPOS,WHword,Rel,VERB)
    ;   % welke nationaliteit heeft (voorzitter) Jan
	Rel = hd/su,
	Arg = _:VERB/_,
	(   triple_member(Triples,deprel(Arg,hd/obj1,NAME:_/_))
	;   triple_member(Triples,deprel(Arg,hd/obj1,NOUN)),
	    NOUN=_:NOUNNAME/_,
	    NOUNNAME \= naam,
	    NOUNNAME \= bijnaam,
	    NOUNNAME \= afkorting,
	    NOUNNAME \= type,
	    NOUNNAME \= uitspraak,
	    triple_member(Triples,deprel(NOUN,hd/app,NAME:_/_))
	),
	functor(NAME,name,_),	    
	(  Feature=wat_jan(VERB)
	;  Feature=wat_jan
	)
    ).

%% op .. wijze,
%% in ... mate
%% op .. termijn
%% binnen .. tijd
%% op .. schaal
%% uit angst [..]
%% in .. hevigheid
%% are typically verbal modifiers; but this is not really picked up by the model :-(

triples_to_feature(Triples,depprep(HdPos,Rel,Prep,Wijze)) :-
    triple_member(Triples,deprel(HdPos:_,Rel,prep:Prep/Pos)),
    triple_member(Triples,deprel(prep:Prep/Pos,hd/obj1,_:Wijze/_)).

%% same as depprep2, but with more info so that we can do norm.mut.inf.
triples_to_feature(Triples,Feature) :-
    hdrug_flag(dep_with_pos,Boolean),
    hd_pp_feature(Boolean,Triples,Feature).

hd_pp_feature(off,Triples,hdpp(Hd,HdPos,Rel,Prep,Noun,NounPos)) :-
    triple_member(Triples,deprel(HdPos:Hd/_,Rel,prep:Prep/Positions)),
    triple_member(Triples,deprel(prep:Prep/Positions,hd/obj1,NounPos:Noun/_)).

hd_pp_feature(on,Triples,hdpp(Hd/HdP,HdPos,Rel,Prep,Noun/NounP,NounPos)) :-
    triple_member(Triples,deprel(HdPos:Hd/HdP,Rel,prep:Prep/Positions)),
    triple_member(Triples,deprel(prep:Prep/Positions,hd/obj1,NounPos:Noun/NounP)).

noun_root(noun:Noun/_,Noun).
noun_root(noun(_):Noun/_,Noun).

name_type(name:_/_,'MISC',_Triples).
name_type(name(TYPE):_/_,TYPE,_Triples).

find_wh_word(Pos:WHword/_,_,[WHword],_) :-
    wh_pos(Pos).
find_wh_word(WH0,Rels,[WHword1|WHword],His) :-
    WH0=(_:WHword1/_),
    member(deprel(WH0,_,WH1),Rels),
    \+ member(WH1,His),
    find_wh_word(WH1,Rels,WHword,[WH1|His]).
% special case for "wat voor (een)" N
find_wh_word(WH0,Rels,[W1,W2],_His) :-
    member(deprel(WH0,hd/det,prep:W1/W1P),Rels),
    member(deprel(prep:W1/W1P,hd/obj1,det(ywh):W2/_),Rels).

wh_pos(pron(ywh)).
wh_pos(pron(_,ywh)).
wh_pos(adj(ywh)).
wh_pos(adv(ywh)).
wh_pos(adv(_,ywh)).
wh_pos(det(wh)).
wh_pos(det(rwh)).
wh_pos(noun(_,ywh)).
wh_pos(pp(waar)).

/*
find_wh_trace(Body,WH,Body,Rel,Triples,_) :-
    member(deprel(Body,Rel,WH),Triples).
find_wh_trace(Body,WH,Arg,Rel,Triples,His) :-
    member(deprel(Body,_,Body2),Triples),
    \+ member(Body2,His),
    find_wh_trace(Body2,WH,Arg,Rel,Triples,[Body2|His]).
*/

find_wh_trace(_,WH,Arg,Rel,Triples,_):-
    member(deprel(Arg,Rel,WH),Triples).

integrate_corpus_frequency_features(His0, His) :-
    findall(Feature,integrate_corpus_frequency_feature(His0,Feature),His1),
    append(His0,His1,His).

integrate_corpus_frequency_feature([H|T],Feature) :-
    member(Feature0,[H|T]),
    integrate_corpus_freq(Feature0,Feature).

integrate_corpus_freq(Feature0,Feature-Count) :-
    corpus_frequency_feature(Feature0),
    score_corpus_feature(Feature0,Feature,Count).

/* method 1: always add w2v feature 
integrate_corpus_freq(dep35(S1,_P1,Rel,P2,S2),w2v(P2,Rel)-Count) :-
    score_named_dep(w2v(S2,P2,Rel,S1),Count),
    debug_message(2,"~w ~w ~w ~w ~n",[S2,w2v(P2,Rel),S1,Count]).
*/

score_corpus_feature(dep35(A,B,C,D,E),z_dep35(D,C),Val) :-
    decompound(dep35(A,B,C,D,E),Feature),
    corpus_frequency_lookup(Feature,Val0),
    Val is Val0/10000,
    !,			       % if z_dep35, then do not use w2v
    debug_message(3,"~w ~w~n",[Feature,Val]).

/*
score_corpus_feature(dep35(S1,_,Rel,P2,S2),w2v(P2,Rel),Count) :-
    score_named_dep(w2v(S2,P2,Rel,S1),Count),
    debug_message(3,"~w ~w ~w ~w ~n",[S2,w2v(P2,Rel),S1,Count]).
*/

/* old method of backoff, try decompounding
score_corpus_feature(dep35(A,B,C,D,E),z_dep35(D,C),Val) :-
    decompound(dep35(A,B,C,D,E),Feature),
    corpus_frequency_lookup(Feature,Val0),
    !,			       % first solution of decompound suffices
    Val is Val0/10000,
    (   dep35(A,B,C,D,E) == Feature
    ->  true
    ;   debug_message(4,"decompound_zdep|~w|~w~n",[dep35(A,B,C,D,E),Feature])
    ),
    debug_message(3,"~w ~w~n",[dep35(A,B,C,D,E),Val]).
*/


score_corpus_feature(hdpp(A,B,C,D,E,F),z_hdpp(B,C),Val) :-	
    corpus_frequency_lookup(hdpp(A,B,C,D,E,F),Val),
    debug_message(3,"~w ~w~n",[hdpp(A,B,C,D,E,F),Val]).

score_corpus_feature(appos_person(TYPE,Word),z_appos_person(TYPE),Val) :-
    corpus_frequency_lookup(appos_person(TYPE,Word),Val),
    debug_message(3,"~w ~w~n",[appos_person(TYPE,Word),Val]).

decompound(dep35(R1,P1,Rel,P2,R2),dep35(S1,P1,Rel,P2,S2)) :-
    decompound_atom(P1,R1,S1),
    decompound_atom(P2,R2,S2).

decompound_atom(_,R,R).
decompound_atom(noun,R1,S1) :-
    decompound_atom_noun(R1,S1).
decompound_atom(noun(_),R1,S1) :-
    decompound_atom_noun(R1,S1).

decompound_atom_noun(R1,S1) :-
    atom(R1),
    sub_atom(R1,Bef,1,_,'_'),
    Bef2 is Bef+1,
    sub_atom(R1,Bef2,_,0,S1),
    S1 \== 'DIM'.

corpus_frequency_feature(dep35(_,_,_,_,_)).
corpus_frequency_feature(hdpp(_,_,_,_,_,_)).
corpus_frequency_feature(appos_person(_,_)).
corpus_frequency_feature(depprep(_,_,_,_)).

/*
%% NB. used from treebank.pl!!!
marginals([],[]).
marginals([F1|Fs],M) :-
    (   marginal(F1,List)
    ->  append(List,M1,M)
    ;   M = M1
    ),
    marginals(Fs,M1).

marginal(dep35(Arg,ArgPos,Rel,HeadPos,Head),
         [tdep35,
          ldep35(Arg,ArgPos),
          rdep35(Rel,HeadPos,Head)]).

marginal(appos_person(TYPE,Word),
	 [appos,
	  apposl(TYPE),
	  apposr(Word)]).

% not required here, all marginals are already included
% becuase of previous clause
%marginal(hdpp(Head,HeadPos,Rel,Prep,Noun,NounPos),
%	 [tdep35,
%	  dep35(Noun,NounPos,hd/obj1,prep,Prep),
%	  rdep35(Rel,HeadPos,Head)]
%	).
*/

penalty_weights(List,Score) :-
    penalty_weights(List,0.0,Score). % should always be a real

penalty_weights([],S,S).
penalty_weights([P|Ps],S0,S) :-
    get_feature_weight(P,Weight),
    S1 is S0 + Weight,
    penalty_weights(Ps,S1,S).

get_feature_weight(weight(Weight),W) :-
    !,
    Weight=W.
get_feature_weight(Feature-Count,Weight) :-
    !,
    try_penalty_weight(Feature,S1),
%    try_additional_weight(Feature,S2),
    Weight is (Count*S1). % +(Count*S2).
get_feature_weight(Feature,Weight) :-
    try_penalty_weight(Feature,S1),
%    try_additional_weight(Feature,S2),
    Weight is S1.  %+S2.

try_penalty_weight(P,S1) :-
    (   alpino_disambiguation_weights:feature_weight(P,S)
    ->  S=S1
    ;   S1=0.0
    ).

% try_additional_weight(P,S1) :-
%     (   additional_weight(P,S)
%     ->  S=S1
%     ;   S1=0.0
%     ).

%% additional weights are set by hand, to prefer readings which cannot be
%% distinguished from competing readings by mapping to the treebank; for
%% instance in the case of wrong lexical sense (really, the treebank should
%% have more sense distinctions, and we should be compatible with those,
%% and check for them...)

%% only punish "bad" readings

%%additional_weight(f2(Word,Pos),Val) :-
%%    additional_weight_f2(Word,Pos,Val).

%% is now learned because postag is taken into account
%%additional_weight(subjunctive(ga),0.000000001).

%% should now be learned because lemma is taken into account
%additional_weight(stem_best(best),                0.4).   % prefer goed over best
%additional_weight(stem_best(v_root(las,lassen)),  0.1).   % prefer lezen over lassen
%additional_weight(stem_best(v_root(zaag,zagen)),  0.1).   % prefer zien over zagen
%additional_weight(stem_best(v_root(eet,eten)),    0.1).   % prefer eten/noun over eten/v_noun

%% could be learned, but typically are not
%%additional_weight(bal(bal,het),0.2).
%%additional_weight(bal(blik,het),0.2).
%%additional_weight(bal(broek,het),0.2).
%%additional_weight(bal(kamp,de),0.2).

%% is now learned
%%additional_weight(van_tot(van,tot),-1).

%% is now learned
%% prefer [[eerder gevonden] oplossingen] over [eerder gevonden oplossingen]
%%additional_weight(adjective_er_plural,1).

%% should be learned
%%additional_weight_f2(hen,noun,0.2).
%%additional_weight_f2(haar,noun,0.2).
%%additional_weight_f2(zij,noun,0.2).
%%additional_weight_f2(u,noun,0.2).
%%additional_weight_f2(kies,noun,0.2).
%%additional_weight_f2(armen,noun,1).
%%additional_weight_f2(rijken,noun,1).

%% TODO:
%% prefer 'door-PP' attach to embedded verb in passive

%% given the way in which beam-search is used, it is perhaps best to
%% associate penalties with nodes in the tree as low as possible...

%% switch to "on" if you need features, rather than weights, e.g.
%% during training
:- initialize_flag(list_all_maxent_features,off).

tree_penalties(on,tree(A,B,Ds,List),His0,His) :-
    (   var(List)
    ->  tree_penalties_with_domain(A,B,Ds,List)
    ;   true 
    ),
    append(List,His,His0).

tree_penalties(off,tree(A,B,Ds,Cache),[Cache|His],His) :-
    (   var(Cache)
    ->  tree_penalties_with_domain(A,B,Ds,List),
	penalty_weights(List,Weight)
    ;   true
    ),
    Cache=weight(Weight).

tree_penalties_with_domain(A,B,Ds,List) :-
    hdrug_util:hdrug_flag(application_type,Domain),
    tree_penalties_with_domain(Domain,A,B,Ds,List).

tree_penalties_with_domain(undefined,A,B,Ds,List) :-
    !,
    findall(H, tree_penalty(A,B,Ds,H), List).
tree_penalties_with_domain(Domain,A,B,Ds,List) :-
    findall(H, tree_penalty(A,B,Ds,H), List0),
    domain_features(List0,List,Domain).

tree_penalty(Node,Rule,Ds,H) :-
    syntactic_penalty(Ds,Node,Rule,H).

tree_penalty(_Node,_Rule,Ds,H) :-
    member(tree(N,R,D,Cache),Ds),
    (   var(Cache)    % does this happen? yes. we only memoize and score
		      % maximal projections
    ->  tree_penalty(N,R,D,H)
		      % don't instantiate Cache, because
    ;                 % in scope of findall anyway
	member_or_id(Cache,H)
    ).

% tree_penalty(Node,_,_,extra_question_mark):-
%     alpino_data:top_cat(Node),  % otherwise we get the feature many times
%     alpino_data:dt(Node,DT),
%     alpino_data:dt(DT,_,_,_,_,_,Attrs),
%     \+ member(stype=_,Attrs),
%     \+ \+ (   alpino_data:ynquestion(DT)  ;   alpino_data:whquestion(DT)   ).

member_or_id(weight(W),weight(W)).
member_or_id([H|T],El) :-
    member(El,[H|T]).

syntactic_penalty(lex(_Ref),Node,_,s1(His)) :-
    alpino_data:syntactic_penalty_cat(Node,His).
syntactic_penalty([],Node,Rule,His) :-
    syntactic_penalty_nl(Rule,Node,[],His).
syntactic_penalty([H|T],Node,Rule,His) :-
    syntactic_penalty_nl(Rule,Node,[H|T],His).
syntactic_penalty(_,Node,_,adjective_er_plural) :-
    alpino_data:adjective_er_plural(Node,Agr,Sg),
    \+ Agr = Sg.  % if agreement cannot unify with singular, it must be plural

syntactic_penalty_nl(_,Cat,_,s1(Name)) :-
    alpino_data:syntactic_penalty_cat(Cat,Name).
syntactic_penalty_nl(_,_,Ds,s1(Name)) :-
    member(tree(Node,_,_,_),Ds),
    alpino_data:syntactic_penalty_cat_d(Node,Name).

syntactic_penalty_nl(_,Cat,_,s1(extra_from_topic)) :-
    alpino_data:sv1_extra(Cat,[_|_]).
syntactic_penalty_nl(_,Cat,_,s1(extra_from_topic,comparative)) :-
    alpino_data:sv1_extra(Cat,[Comparative|_]),
    alpino_data:comparative_cat(Comparative).
syntactic_penalty_nl(_,Cat,_,s1(mextra_from_topic)) :-
    alpino_data:sv1_mextra(Cat,[H|T]),
    nonvar_member(El,[H|T]),
    alpino_data:mexs_cat_mods(El,[_]).
syntactic_penalty_nl(Id,_,_,r1(Id)).
syntactic_penalty_nl(Id,_,Ds,Feature) :-
    nth(N,Ds,D),
    nth_syntactic_penalty(Id,N,D,Feature).
syntactic_penalty_nl(vp_arg_v(pred),_,[Pred,tree(Vproj,_,_,_)],subj_pred_agree(Num1,Num2)) :-
    Pred = tree(_,pred_np,[tree(NP,_,_,_)],_),
    alpino_data:np_agr(NP,Agr1),
    alpino_data:subj_agr(Vproj,Agr2),
    num(Agr1,Num1),
    num(Agr2,Num2).
syntactic_penalty_nl(_,Cat,[D,_|_],center_embedding) :-
    \+ \+ alpino_data:vproj_without_eps3(Cat),
				% eps3=yes iff empty vc in vproj
    contains_vp(D).
syntactic_penalty_nl(_,Cat,[Punct,D,_|_],center_embedding) :-
    alpino_data:punct(Punct),
    \+ \+ alpino_data:vproj_without_eps3(Cat),
				% eps3=yes iff empty vc in vproj
    contains_vp(D).

%% punish some ungrammatical constructions which the grammar cannot rule out
%% "een [bomalarm en demonstraties]
syntactic_penalty_nl(np_det_n,_NP,[tree(Det,_,_,_), Conj],p2(det_n_agr)) :-
    det_conj_n_no_agr(Det,Conj).
syntactic_penalty_nl(np_det_n_q,_NP,[_, tree(Det,_,_,_), Conj, _],p2(det_n_agr)) :-
    det_conj_n_no_agr(Det,Conj).

%% ?waar is dat nodig voor/waar is dat voor nodig
%% waar is dat belangrijk voor/?waar is dat voor belangrijk

syntactic_penalty_nl(a_a_er_pp_comp,_,[NodigTree,VoorTree],a_er(Nodig,Voor)) :-
    NodigTree = tree(NodigCat,_,_,_),
    VoorTree = tree(VoorCat,_,_,_),
    alpino_data:prep(VoorCat,Voor),
    alpino_data:hstem(NodigCat,Nodig).

%% we want to recognize non-standard orderings in the `middle field'. For
%% instance:
%% pronoun nps < non-pronoun nps
%% adverbs < pc complements
%% obj2 < obj1
%%
%% so each property mf(C1,C2) indicates that C1 precedes C2 in the middle-field
syntactic_penalty_nl(_,Cat,_,mf(C1cat,C2cat)) :-
    alpino_data:vp_cat(Cat,Mf),
    precedes(C1,C2,Mf),	% Mf is not fully known, so precedes must allow var!
    mf_pos(C1,C1cat),
    mf_pos(C2,C2cat).

%% find out whether conjunction is parallel or not.
%% For now: conjunction is parallel if each conjunct is constructed
%% by the same rule or if each conjunct is a lexical entry
syntactic_penalty_nl(start_coord(A,B),_,Ds,p1(Par)) :-
    get_conj_ds(start_coord(A,B),Ds,Cjs),
    determine_parallel(Cjs,Par).

syntactic_penalty_nl(start_coord(A,B,C),_,Ds,p1(Par)) :-
    get_conj_ds(start_coord(A,B,C),Ds,Cjs),
    determine_parallel(Cjs,Par).

syntactic_penalty_nl(wh_topicalization(_),_,_,q(direct)).

%% wie hoor ik daar? -> wie =/= vc.
syntactic_penalty_nl(non_wh_topicalization(sbar),Cat,_,q(nonq)) :-
    alpino_data:puncttype(Cat,Vraag1),
    alpino_data:vraag_puncttype(Vraag2),
    Vraag1 == Vraag2.

syntactic_penalty_nl(wh_topicalization(_),Cat,_,q(q)) :-
    alpino_data:puncttype(Cat,Vraag1),
    alpino_data:vraag_puncttype(Vraag2),
    Vraag1 == Vraag2.

syntactic_penalty_nl(max_xp(sv1),Cat,_,q(yesno)) :-
    alpino_data:puncttype(Cat,Vraag1),
    alpino_data:vraag_puncttype(Vraag2),
    Vraag1 == Vraag2.

syntactic_penalty_nl(a_pp_comp_a,_,[tree(PP,_,_,_),tree(Adj,_,_,_)],meebezig(Prep,Stem)) :-
    alpino_data:prep(PP,Prep), nonvar(Prep),
    alpino_data:hstem(Adj,Stem), nonvar(Stem).

syntactic_penalty_nl(vp_v_komma_arg(pp),_,[_,_,tree(PP,_,_,_)],s(ld_pp_extra)) :-
    alpino_data:ld_pp(PP).

%% prevent "we wachten jaar" but generate "we wachten jaren"
syntactic_penalty_nl(np_n,_,[N],np_n_bare(Surf)) :-
    rulename_surf(N,Surf).

nth_syntactic_penalty(Id,N,D,r2(Id,N,DId)) :-
    rulename(D,DId).

nth_syntactic_penalty(Id,N,D,r2(Id,N,DId)) :-
    rulename_lex(D,DId).

num(Agr,sg) :-
    \+ alpino_data:pl(Agr).
num(Agr,pl) :-
    \+ alpino_data:sg(Agr).

coord_agr(Cat,Conj,Val) :-
    alpino_data:conj(Cat,Conj,Agr),
    num(Agr,Val).

det_conj_n_no_agr(Det,Conj) :-
    alpino_data:det_agr(Det, Agr),
    get_embedded_conj_ds(Conj,Ds),
    member(tree(El,_,_,_),Ds),
    alpino_data:n_agr(El,Agr2),
    \+ Agr = Agr2.


%% number of words per daughter
%nth_syntactic_penalty(Id,N,D,rn(Id,N)-Count) :-
%    count_words(D,0,Count),
%    Count > 0.
%
%count_words(tree(_,_,Ds,_),C0,C) :-
%    count_words_ds(Ds,C0,C).
%
%count_words_ds(lex(_),C0,C) :-
%    C is C0 + 1.
%
%count_words_ds([],C,C).
%count_words_ds([H|T],C0,C) :-
%    count_words(H,C0,C1),
%    count_words_ds(T,C1,C).

%rulename_lex_postag(tree(_,_,lex(ref(Tag,_,Root,_,_,_,_,_,_,_,_)),_),Root,Tag).

rulename_lex(tree(_,_,lex(ref(_,_,Root,_,_,_,_,_,_,_,_)),_),Root).
rulename_lex(tree(_,_,[D],_),RuleName) :-
    rulename_lex(D,RuleName).

rulename_surf(tree(_,_,lex(ref(_,_,_,Root,_,_,_,_,_,_,_)),_),Root).
rulename_surf(tree(_,_,[D],_),RuleName) :-
    rulename_surf(D,RuleName).

rulename(tree(_,Name,Ds,_),RuleName) :-
    rulename(Ds,Name,RuleName).

rulename(lex(_),_,l).
rulename([_|_],Name,Name).
rulename([],Name,Name).

mf_pos(Cat,Val) :-
    functor(Cat,Fun,_),
    mf_pos(Fun,Cat,Val).
mf_pos(np,Cat,Type) :-
    !,
    alpino_data:case(Cat,CaseVal),
    nonvar(CaseVal),
    hdrug_feature:give_boolean_type(CaseVal,Case),
    alpino_data:postag(Cat,PosTagValTerm),
    nonvar(PosTagValTerm),
    alpino_dt:somewhat_simplify_frame(PosTagValTerm,Pos0),
    simplify_name(Pos0,Pos),
    add_men(np(Case,Pos),Type,Cat).
mf_pos(modifier,Cat,Type) :-
    !,
    alpino_data:modifier(Cat,DtrCat),
    nonvar(DtrCat),
    hdrug_feature:give_boolean_type(DtrCat,Type0),
    add_niet(Type0,Type,Cat).
mf_pos(pred,Cat,Type) :-
    !,
    alpino_data:predicative(Cat,DtrCat),
    nonvar(DtrCat),
    hdrug_feature:give_boolean_type(DtrCat,Type).
mf_pos(F,_Cat,F).

add_niet(mcat_adv,mcat_niet,Cat) :-
    alpino_data:dt(Cat,DT),
    alpino_data:dt(DT,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Niet,_,_,_,_),
    Niet == niet.
add_niet(mcat_adv,mcat_meer,Cat) :-
    alpino_data:dt(Cat,DT),
    alpino_data:dt(DT,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Niet,_,_,_,_),
    Niet == meer.
add_niet(mcat_adv,mcat_er,Cat) :-
    alpino_data:dt(Cat,DT),
    alpino_data:dt(DT,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Niet,_,_,_,_),
    Niet == er.
add_niet(Type,Type,_).

add_men(np(nom,pron(nwh)),men,Cat) :-
    alpino_data:dt(Cat,DT),
    alpino_data:dt(DT,Hwrd,_,_,_),
    alpino_data:label(Hwrd,Men,_,_,_,_),
    Men == men.
add_men(np(A,B),np(A,B,def),Cat) :-
    \+ alpino_data:indef(Cat).
add_men(np(A,B),np(A,B,indef),Cat) :-
    \+ alpino_data:def(Cat).
add_men(Type,Type,_).

simplify_name(name(_),X) :-
    !,
    X = name.
simplify_name(verb(v_noun,_),X) :-
    !,
    X = v_noun.
simplify_name(X,X).

get_embedded_conj_ds(tree(_A,B,Ds,_),Cjs) :-
    get_conj_ds(B,Ds,Cjs).
get_embedded_conj_ds(tree(_,Rule,Ds,_),Cjs) :-
    get_embedded_conj_ds(Rule,Ds,Cjs).

get_embedded_conj_ds(n_adj_n,[_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_adj_n_marked,[_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_comma_adj_comma_n,[_,_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_comma_adj_n,[_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_comma_adj_n_marked,[_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_bracketed_adj_n,[_,_,_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_dashed_adj_n,[_,_,_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_bracketed_mod_n,[_,_,_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_dashed_mod_n,[_,_,_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_score_n,[_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_num_n,[_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).
get_embedded_conj_ds(n_bracketed_num_n,[_,_,_,_,N],Cjs) :-
    get_embedded_conj_ds(N,Cjs).

get_conj_ds(start_coord(_,zowel_als),[_,C,tree(_,Mid,Ds,_)],[C|Cjs]) :-
    get_conj_ds_mid(Mid,Ds,Cjs).
get_conj_ds(start_coord(_,en),       [C,tree(_,Mid,Ds,_)],[C|Cjs]) :-
    get_conj_ds_mid(Mid,Ds,Cjs).
get_conj_ds(start_coord(dip,root,en),[C,_,_,tree(_,Mid,Ds,_)],[C|Cjs]) :-
    get_conj_ds_mid(Mid,Ds,Cjs).
get_conj_ds(start_coord(dipq,root,en),[_,C,_,_,_,_,_,tree(_,Mid,Ds,_),_],[C|Cjs]) :-
    get_conj_ds_mid(Mid,Ds,Cjs).

get_conj_ds_mid(mid_coord(_),[_,C,tree(_,Mid,Ds,_)],[C|Cjs]) :-
    get_conj_ds_mid(Mid,Ds,Cjs).
get_conj_ds_mid(mid_coord(_,_),[_,_,C,tree(_,Mid,Ds,_)],[C|Cjs]) :-
    get_conj_ds_mid(Mid,Ds,Cjs).

get_conj_ds_mid(end_coord(_,conj),[_,_,_,C,_],[C]).
get_conj_ds_mid(end_coord(_,noconj),  [_,C,_],[C]).
get_conj_ds_mid(end_coord(_,conj,mod),[_,_,_,C,_],[C]).
get_conj_ds_mid(end_coord(_,conj,c_mod),[_,_,_,_,_,C],[C]).
get_conj_ds_mid(end_coord(_),_,[]).
get_conj_ds_mid(bracketed_end_coord(_,conj),  [_,_,C,_,_],[C]).
get_conj_ds_mid(bracketed_end_coord(_,conj,mod),[_,_,_,C,_,_],[C]).
get_conj_ds_mid(bracketed_end_coord(_,conj,c_mod),[_,_,_,_,_,C,_],[C]).

depth_parallel([H|T],Min,Max) :-
    depth(H,Depth),
    depth_parallel(T,Depth,Min,Depth,Max).

depth_parallel([],Min,Min,Max,Max).
depth_parallel([H|T],Min0,Min,Max0,Max) :-
    depth(H,D),
    Min1 is min(Min0,D),
    Max1 is max(Max0,D),
    depth_parallel(T,Min1,Min,Max1,Max).

depth(tree(_,_,lex(_),_),D) :-
    !,
    D = 1.
depth(tree(_,_,[D],_),Depth) :-
    !,
    depth(D,Depth).
depth(tree(_,_,Ds,_),Depth) :-
    max_depth(Ds,0,Max),
    Depth is Max+1.

max_depth([],M,M).
max_depth([H|T],M0,M) :-
    depth(H,M1),
    M2 is max(M0,M1),
    max_depth(T,M2,M).

determine_parallel([H|T],Par) :-
    (	parallel([H|T])
    ->	Par=par
    ;	Par=nopar
    ).

determine_parallel(List,pardepth):-
    depth_parallel(List,Min,Max),
    Min1 is Min+1,
    between(Min1,Max,_).
    % generate as many penalties as the difference is big

determine_parallel([H|T],strict_par) :-
   strict_parallel([H|T]).

parallel([H|T]) :-
    rulename(H,His),
    parallel(T,His).

parallel([],_).
parallel([H|T],His0) :-
    rulename(H,His1),
    ppp(His0,His1,His),
    parallel(T,His).

strict_parallel([H|T]) :-
    deriv(H,His),
    strict_parallel(T,His).

strict_parallel([],_).
strict_parallel([H|T],His0) :-
    deriv(H,His1),
    ppp(His0,His1,His),
    strict_parallel(T,His).

ppp(H,H,H).

deriv(tree(_,Name,Ds,_),Deriv) :-
    deriv_ds(Ds,Name,Deriv).

deriv_ds(lex(_),_,l).
deriv_ds([],Name,d(Name,[])).
deriv_ds([H|T],Name,d(Name,DsDeriv)) :-
    deriv_ds([H|T],DsDeriv).

deriv_ds([],[]).
deriv_ds([H|T],[NH|NT]) :-
    deriv(H,NH),
    deriv_ds(T,NT).

%% lexical_penalty_frame(gen,Stem,Word,_,_,_,lexical_choice(Stem,Word)).

lexical_penalty_frame(_,Noun,_,noun(Lid,count,sg),noun,_,bal(Noun,Lid)).
lexical_penalty_frame(_,Stem,_,_,_,_,stem_best(Stem)).
lexical_penalty_frame(_,_,Surf,verb(_,subjunctive,_),_,_,subjunctive(Surf)).
lexical_penalty_frame(_,_,Surf,_,Frame,_,f2(Surf,Frame)) :-
    prettyvars(Frame).
lexical_penalty_frame(_,_,_,_,Frame,_,f1(Frame)).
lexical_penalty_frame(_,_,Surf,_,Frame,_,z_f2-Score) :-
    z_f2(Surf,Frame,Score).
lexical_penalty_frame(His,_,_,_,_,_,_):-
    var(His),
    !,
    fail.
lexical_penalty_frame(His,_,_,_,_Frame,Len,h1(His)) :-
    nonvar(Len), % for generation!
    generate_i(Len).
lexical_penalty_frame(compound(_),_,_,_,Tag,_,compound(Fun)) :-
    functor(Tag,Fun,_).
lexical_penalty_frame(form_of_suffix(_),_,_,_,_,_,form_of_suffix).
lexical_penalty_frame(form_of_suffix(X),_,_,_,_,_,form_of_suffix(X)).
lexical_penalty_frame(skip(_,_,_,_),_,_,_,_,    _,skip).
lexical_penalty_frame(skip(_,[H|T],_,_),_,_,_,_,_,skip(W)) :-
    member(W,[H|T]).
lexical_penalty_frame(skip(_,_,[H|T],_),_,_,_,_,_,skip(W)) :-
    member(W,[H|T]).

%lexical_penalty_frame(gen,Stem,Surf,Frame,_,_,para) :- 
%    alpino_paraphrase:add_lex(Stem,Surf,Frame).

generate_i(_).
generate_i(Len) :-
    Len > 1,
    Len1 is Len - 1,
    generate_i(Len1).

%z_f2_simplify_frame(with_dt(Frame0,_),Frame) :-
%    !,
%    Frame0=Frame.
%z_f2_simplify_frame(X,X).

%%% Distance features

distance_score(tree(_,_,Ds),Dist0,Dist) :-
    findall(Pen,a_dist(Ds,Pen),Dist0,Dist).

%% relative attached to a noun that is not the closest one
%%
%% todo also e.g. for nouns with sbar complement:
%% "omdat die vraag leidde tot de vraag of hij sliep"
%%    no distinction!

a_dist([H|T],dist(Cat,Val)) :-
    subtree([H|T],tree(NP,_,NPDS)),
    parts_of_nonterminal(NP,_,np),
    rel_ncn(NPDS,[H|T],Val,Cat).

extraposition_candidate(rel).
% adding these does not lead to improved accuracy:
%extraposition_candidate(pp).
%extraposition_candidate(whsub).
%extraposition_candidate(oti).
%extraposition_candidate(ti).
%extraposition_candidate(cp).

start_position(Tree,Start) :-
    findall(P0,subnode(Tree,P0),Starts0),
    sort(Starts0,[Start|_Starts]).

subnode(tree(Node,_,[]),P) :-
    parts_of_terminal(Node,_,_,_,P,_).
subnode(tree(_,_,List),P) :-
    member(Tree,List),
    subnode(Tree,P).

%% todo (?): identify clefs which are genuine dis(rel,far)
rel_ncn(NPDS0,[H|T],Val,Cat) :-
    select(tree(ExtraNode,_,ExtraDs),NPDS0,NPDS1),
    parts_of_nonterminal(ExtraNode,_,Cat),
    nonvar(Cat),
    extraposition_candidate(Cat),
    hd_d(NPDS1,_,_,N),
    start_position(tree(ExtraNode,_,ExtraDs),P0),
    nonvar(P0),                  % unknown during cg beam search
    (   np_d([H|T],Q0),
        N =< Q0, Q0 < P0
    ->  Val=far                  % extraposition, and an NP intervenes
    ;   Val=close                % extraposition, but no NP intervenes
    ).

np_d([H|T],Q0) :-
    subtree([H|T],tree(Node,_,SubSub)),
    parts_of_nonterminal(Node,Rel,NP),
    nonvar(NP), NP=np,
    nonvar(Rel),
    Rel \= mod,
    Rel \= app,
    Rel \= cnj,                 % then it's really the same NP perhaps
    start_position(tree(Node,_,SubSub),Q0).

subtree(List,Tree) :-
    member(Tree0,List),
    subtree_(Tree0,Tree).

subtree_(Tree,Tree).
subtree_(tree(_,_,List),Tree):-
    subtree(List,Tree).

head_rel(hd).
head_rel(cmp).
head_rel(rhd).
head_rel(whd).
head_rel(crd).

hd_d(List0,List,P0,P) :-
    hd_d(List0,List,P0,P,_Pos).

hd_d([H|T],List1,P0,P,Tag) :-
    select(tree(Node,_,_),[H|T],List1),
    parts_of_terminal(Node,Rel,Tag,_,P0,P),
    head_rel(Rel).

parts_of_nonterminal(r(Rel,Label),Rel,Cat) :-
    nonvar(Label),  % 'een soort' etc..
    parts_of_nonterminal_(Label,Cat).

parts_of_nonterminal_(p(Cat),Cat).
parts_of_nonterminal_(i(_,p(Cat)),Cat).

parts_of_terminal(r(Rel,Label),Rel,Tag,Word,P0,P) :-
    parts_of_terminal_(Label,Tag0,Word,P0,P),
    alpino_postags:postag_of_frame(Tag0,Tag,_).

parts_of_terminal_(l(Tag,_,Word/[P0,P]),     Tag,Word,P0,P).
parts_of_terminal_(i(_,l(Tag,_,Word/[P0,P])),Tag,Word,P0,P).

%% List is potentially variable-tailed!
precedes(El1,El2,List) :-
    nonvar(List),
    precedes_(List,El1,El2).

precedes_([H|T],H,El2) :-
    nonvar(T),
    nonvar_member(El2,T).
precedes_([_|T],El1,El2) :-
    nonvar(T),
    precedes_(T,El1,El2).

nonvar_member(Element, [Head|Tail]) :-
    nonvar_member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
nonvar_member_(_, Element, Element).
nonvar_member_([Head|Tail], _, Element) :-
    nonvar(Tail),
    nonvar_member_(Tail, Head, Element).


%% back-off logic:
%% dep35(Arg,ArgPos,Rel,HeadPos,Head) ==>
%%     dep34(Arg,ArgPos,Rel,HeadPos) ==>
%%         dep23(ArgPos,Rel,HeadPos)
%%
%% depprep2(Hd,Rel,Prep,NounHd) ==>
%%     depprep(HdPos,Rel,Prep,NounHd)
%%
%% h1(form_of_suffix(Heur)) ==> h1(form_of_suffix)
%%
%% f2(Stem,Frame) ==> f1(Frame)

%feature_term_to_codes(dep35(A,B,C1/C2,D,E),Codes) :-
%    format_to_chars('dep35(~q,~q,~q/~q,~q,~q)',[A,B,C1,C2,D,E],Codes).
%
%feature_term_to_codes(depprep2(A,B,C,D),Codes) :-
%    format_to_chars('depprep2(~q,~q,~q,~q)',[A,B,C,D],Codes).


feature_term_to_codes(Feature,Codes) :-
    format_to_chars('~q',[Feature],Codes).

corpus_frequency_lookup(TripleTerm,Score) :-
    time(1,initialize_corpus_frequency(DictNo)),
    feature_term_to_codes(TripleTerm,Triple),
    pro_fadd:associate_word_integer(Triple,DictNo,Score),
    Score > 0.  % associate_word_integer always succeeds, with Score=0
                % if the word is not in the dictionary

initialize_corpus_frequency(No) :-
    hdrug_flag(initialized_corpus_frequency,Init),
    initialize_corpus_frequency(Init,No).

initialize_corpus_frequency(undefined,No) :-
    !,
    hdrug_flag(corpus_frequency,File),
    pro_fadd:init_morph(File,0,0,0,0,No),
    debug_message(1,"initialized corpus_frequency ~w (~w)~n",[File,No]),
    set_flag(initialized_corpus_frequency,initialized(No)).
initialize_corpus_frequency(initialized(No),No).

%%% called from treebank - extract z_ features from large treebanks
:- public triples_to_features_which_have_freq_feature/2.

triples_to_features_which_have_freq_feature(Rels,Features) :-
    findall(Fea,triples_to_freq_feature(Rels,Fea),Features).

triples_to_freq_feature(Rels,Feature) :-
    triples_to_feature(Rels,Feature),
    corpus_frequency_feature(Feature).

:- use_module(frames).

contains_vp(tree(C,_,_,_)) :-
    alpino_data:vp(C).
contains_vp(tree(_,_,Ds,_)) :-
    lists:member(D,Ds),
    contains_vp(D).


