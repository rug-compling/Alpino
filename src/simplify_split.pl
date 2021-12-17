:- module(alpino_simplify_split, [ apply_split_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%


%% TODO:
%% de vraag die we ons moeten stellen is wie er komt
%% => wie komt er. Deze moeten we stellen.
%% requires frame for "stellen" requires "vraag", but we only have a pronoun
%% 

apply_split_transformations(Tree0,Tree) :-
    split_transformations(Tree0,Tree1),
    alpino_cg:collapse_all(Tree1,Tree2,List),
				% if the relative is removed, we need to ensure
                                % its contents end up at the co-indexed node
    expand_all(Tree2,Tree,List).  % if i(I) occurs in a split as first mention, it must become a pronoun

split_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    split_transformation(Cat0,Ds0,Cat1,Ds1),
    !,
    split_transformations(tree(Cat1,Ds1),tree(Cat,Ds)).
split_transformations(tree(Cat,Ds0),tree(Cat,Ds)) :-
    split_transformations_list(Ds0,Ds).

split_transformations_list([],[]).
split_transformations_list([H0|T0],[H|T]) :-
    split_transformations(H0,H),
    split_transformations_list(T0,T).

split_transformation(r(top,p(top)),Ds0,r(split,p(split)),Ds) :-
    select_replace(Ds0,D,X1,X2,Ds),
    split_transformation(D,X1,X2).

expand_all(tree(r(split,p(split)),Ds0), tree(r(split,p(split)),Ds), Params) :-
    !,
    expand_split_ds(Ds0,Ds,Params).
expand_all(tree(r(Rel,Cat),Ds), tree(r(Rel,Cat),Ds),_Params).

%% expand_ds(DsIn,DsOut,AllParams,ParamsOfThisSplit)
expand_ds([],[],_,_).
expand_ds([H0|T0],[H|T],Ps,Ms) :-
    expand_all(H0,H,Ps,Ms),
    expand_ds(T0,T,Ps,Ms).

%% expand_ds(DsIn,DsOut,AllParams)
expand_split_ds([],[],_).
expand_split_ds([H0|T0],[H|T],Ps) :-
    expand_all(H0,H,Ps,_),
    expand_split_ds(T0,T,Ps).

expand_all(tree(r(split,p(split)),Ds0),tree(r(split,p(split)),Ds), Params, _Ms) :-
    !,
    expand_split_ds(Ds0,Ds,Params).
expand_all(tree(r(Rel,Cat0),Ds0), tree(r(Rel,Cat),Ds), All, Locals) :-
    expand_one(Cat0,Ds0,Cat,Ds1,All,Locals),
    expand_ds(Ds1,Ds,All,Locals).

expand_one(i(Ix,Cat),Ds,i(Ix,Cat),Ds,_All,Locals) :-
    !,
    lists:memberchk(Ix=s(Cat,Ds),Locals).
expand_one(i(Ix),[],i(Ix),[],_,Locals) :-
    lists:memberchk(Ix=Var,Locals),
    nonvar(Var),
    !.
expand_one(i(Ix),[],i(Ix,Cat),Ds,All,Locals) :-
    lists:memberchk(Ix=s(Cat,Ds),Locals),
    lists:memberchk(Ix=s(Prev,PrevDs),All), nonvar(Prev),
    (   generate_pronoun(Prev,PrevDs,Cat,Ds)
    ->  true
    ;   Prev=Cat,
	PrevDs=Ds
    ).

expand_one(p(Cat),Ds,p(Cat),Ds,_,_).
expand_one(adt_lex(A,B,C,D,E),[],adt_lex(A,B,C,D,E),[],_,_).

generate_rel_pronoun(adt_lex(_,B,B,_,Atts0),[],Die,RelAtts,Atts,Pronoun) :-
    lists:append(RelAtts,Atts0,Atts1),
    sort(Atts1,Atts2),
    pronoun(B,Die,Atts2,Atts,Pronoun).
generate_rel_pronoun(p(mwu(Lemma,_)),Ds,Die,RelAtts,Atts,Pronoun) :-
    Hd = tree(r(mwp,adt_lex(np,_,_,_,Atts0)),[]),  % attributes are identical for all mwp's, right?
    lists:member(Hd,Ds),
    lists:append(RelAtts,Atts0,Atts1),
    sort(Atts1,Atts2),
    pronoun(Lemma,Die,Atts2,Atts,Pronoun).

%% don't touch it if it is a pronoun already
generate_pronoun(adt_lex(np,B,B,pron,Atts),[],adt_lex(np,B,B,pron,Atts),[]).

%% a new pronoun
generate_pronoun(adt_lex(np,B,B,_,Atts0),[],adt_lex(np,Pronoun,Pronoun,_,Atts),[]) :-
    pronoun(B,_Dat,Atts0,Atts,Pronoun).

%% long np: find pronoun for its head
generate_pronoun(p(_),Ds0,Pronoun,Ds) :-
    Hd = tree(r(hd,HdCat),[]),
    lists:member(Hd,Ds0),
    generate_pronoun(HdCat,[],Pronoun,Ds).

%% long name: find pronoun
generate_pronoun(p(mwu(Lemma,_)),Ds0,adt_lex(np,Pronoun,Pronoun,_,Atts),[]) :-
    Hd = tree(r(mwp,adt_lex(np,_,_,_,Atts0)),[]),  % attributes are identical for all mwp's, right?
    lists:member(Hd,Ds0),
    pronoun(Lemma,_,Atts0,Atts,Pronoun).

%% replace element by two elements
select_replace([H|T],H,X1,X2,[X1,X2|T]).
select_replace([H|T],D,X1,X2,[H|NT]) :-
    select_replace(T,D,X1,X2,NT).

%% smain and smain ==> smain. smain.
split_transformation(tree(r('--',p(conj)),Ds0),X1,X2) :-
    D1 = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    D2 = tree(r(cnj,p(P1)),L1),
    D3 = tree(r(cnj,P2),L2),
    lists:select(D1,Ds0,Ds1),
    lists:member(P1,[smain,sv1,whq]),
    lists:select(D2,Ds1,Ds2),
    lists:select(D3,Ds2,[]),
    X1 = tree(r(top,p(top)),[tree(r('--',p(P1)),L1)]),
    X2 = tree(r(top,p(top)),[tree(r('--',P2),L2)]).

%% SU [rel die/dat Rest] VP  ==> SU VP. SU Rest.
split_transformation(tree(r('--',p(smain)),Ds0),X1,X2) :-
    (   Su1 = tree(r(su,i(II,p(np))), SuDs1),
        Su  = tree(r(su,i(II,SuCat)),SuDs)
    ;   Su1 = tree(r(su,p(np)), SuDs1),
        Su  = tree(r(su,SuCat),SuDs)
    ),
    lists:select(Su1,Ds0,Ds),
    lists:member(tree(r(hd,HdCat),HdDs),SuDs1),
    \+ forbid_rel_split(HdCat),
    Rel = tree(r(mod,p(rel)), RelDs),
    lists:select(Rel,SuDs1,SuDs2),
    Rhd = tree(r(rhd,i(Ix,adt_lex(_,Die,Die,_,RelAtts))),[]),
    Body= tree(r(body,p(ssub)),BodyDs0),
    lists:select(Rhd,RelDs,RelDs1),
    lists:member(Die,[die,dat]),
    lists:select(Body,RelDs1,[]),
    generate_rel_pronoun(HdCat,HdDs,Die,RelAtts,Atts,Pronoun),!,
    lex_replace(i(Ix),i(Ix,adt_lex(_,Pronoun,Pronoun,_,Atts)),BodyDs0,BodyDs1),
    BHd0 = tree(r(hd,adt_lex(ssub,B,C,D,E)),[]),
    BHd  = tree(r(hd,adt_lex(smain,B,C,D,E)),[]),
    replace(BHd0,BHd,BodyDs1,BodyDs),
    new_su(SuDs2,SuCat,SuDs),
    X1 = tree(r(top,p(top)),[tree(r('--',p(smain)),[Su|Ds])]),
    X2 = tree(r(top,p(top)),[tree(r('--',p(smain)),BodyDs)]).

new_su([D1,D2|Ds],p(np),[D1,D2|Ds]).
new_su([tree(r(hd,HdCat),HdDs)],HdCat,HdDs).

forbid_rel_split(adt_lex(_,HdLem,HdLem,_,_)):-
    forbid_lemma_rel_split(HdLem).

forbid_lemma_rel_split(degeen).
forbid_lemma_rel_split(één).
forbid_lemma_rel_split(eerste).
forbid_lemma_rel_split(tweede).
forbid_lemma_rel_split(derde).
forbid_lemma_rel_split(iets).
forbid_lemma_rel_split(niets).
forbid_lemma_rel_split(dat).
forbid_lemma_rel_split(alles).
forbid_lemma_rel_split(weinig).
forbid_lemma_rel_split(ieder).
forbid_lemma_rel_split(iedereen).

%% don't check for sg, because in pronoun_() this feature
%% is only used for sg anyway
guess_die_dat(Lemma,_,Dat) :-
    (   Lemma = Word
    ;   alpino_lex:inv_lex(Lemma,Word)
    ),
    (   alpino_lex:lexicon(noun(het,_,_),_,[Word],[],normal),
	Dat = dat
    ;   alpino_lex:lexicon(noun(de,_,_),_,[Word],[],normal),
	Dat = die
    ).
guess_die_dat(_,_,_).

pronoun(HdLem,DieDat,Atts0,Atts,Pron) :-
    (   var(DieDat)
    ->  guess_die_dat(HdLem,Atts0,DieDat)
    ;   true
    ),
    (   lists:member(neclass='PER',Atts0)
    ->  Per = per
    ;   Per = nonper
    ),
    (   lists:member(rnum=Num,Atts0)
    ->  true
    ;   Num = sg
    ),
    pronoun_(HdLem,DieDat,Num,Per,Pron,Atts0,Atts1),
    limit_to_pronoun_atts(Atts1,Atts).

pronoun_att(rnum).
pronoun_att(per).
pronoun_att(def).


limit_to_pronoun_atts(Atts0,Atts) :-
    findall(rnum=Val,(pronoun_att(Att), lists:member(Att=Val,Atts0)),Atts1),
    sort(Atts1,Atts).


:- use_module(gadata).

pronoun_(L,DieDat,A,nonper,B,Atts0,Atts) :-
    atom_concat(Lem,'_DIM',L),
    pronoun_(Lem,DieDat,A,nonper,B,Atts0,Atts).
pronoun_(L,DieDat,A,nonper,B,Atts0,Atts) :-
    atom_concat(_,Right,L),
    atom_concat('_',Lem,Right),
    pronoun_(Lem,DieDat,A,nonper,B,Atts0,Atts).

pronoun_(Lemma,_,sg,_,{[hij,hem]},A,A) :-
    human(Lemma,m).
pronoun_(Lemma,_,sg,_,{[zij,ze,haar]},A,A) :-
    human(Lemma,f).
pronoun_(Lemma,_,sg,_,{[hij,zij,ze,hem,haar]},A,A) :-
    human(Lemma,'').
pronoun_(Lemma,_,sg,_,{[ze,hen,hun]},A0,A) :-
    human(Lemma,p),
    replace(rnum=sg,rnum=pl,A0,A).
pronoun_(_Lemma,_,sg,per,{[hij,zij,ze,hem,haar]},A,A).
pronoun_(Lemma,_,pl,_,{[ze,hen,hun]},A,A) :-
    human(Lemma,_).
pronoun_(Lemma,dat,sg,nonper,{[het]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,die,sg,nonper,{[ze,deze]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,_,pl,nonper,{[deze,ze]},A,A) :-
    \+ human(Lemma,_).

lex_replace(Cat0,Cat,Ds0,Ds) :-
    ds_lex_replace(Ds0,Cat0,Cat,Ds).

ds_lex_replace([],_Cat0,_Cat,[]).
ds_lex_replace([H|T],Cat0,Cat,[NH|NT]) :-
    lex_replace_t(H,Cat0,Cat,NH),
    ds_lex_replace(T,Cat0,Cat,NT).

lex_replace_t(tree(r(Rel,X),Ds0),Cat0,Cat,tree(r(Rel,Y),Ds)):-
    c_replace(X,Cat0,Cat,Y),
    ds_lex_replace(Ds0,Cat0,Cat,Ds).

c_replace(X,X,Y,Y) :- !.
c_replace(X,_,_,X).
	       
%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).
