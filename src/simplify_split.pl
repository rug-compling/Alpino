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
    select_replace(Ds0,D,Xs,Ds),
    split_transformation(D,Xs).


split_transformation(r(Rel,p(MAIN)),Ds0,r(Rel,p(conj)),[CRD,N1,N2]) :-
    lists:member(MAIN,[smain,sv1]),
    get_su_and_index(Ds0,Ds1,tree(r(su,i(Ix,SuCat)),SuDs)),
    VC = tree(r(vc,p(conj)),VCDS0),
    Hd = tree(r(hd,_),_),
    lists:select(Hd,Ds1,Ds2),
    lists:select(VC,Ds2,PredMs),
    CRD = tree(r(crd,_),_),
    lists:select(CRD,VCDS0,[CNJ1,CNJ2]),
    CNJ1 = tree(r(cnj,CNJCAT1),CNJDS1),
    CNJ2 = tree(r(cnj,CNJCAT2),CNJDS2),
    CNJDS2 = [_|_],  % should at least contain subject, and not be "VP en omgekeerd"
    Su1 = tree(r(su,i(Ix,SuCat)),SuDs),
    Su2 = tree(r(su,i(XX,SuCat2)),SuDs2),
    generate_pronoun(SuCat,SuDs,SuCat2,SuDs2),
    hdrug_util:gen_sym(XX,adt_index),
    replace_index(CNJDS2,CNJDS22,Ix,XX),
    N1 = tree(r(cnj,p(MAIN)),[Su1,Hd,tree(r(vc,CNJCAT1),CNJDS1)|PredMs]),
    N2 = tree(r(cnj,p(MAIN)),[Su2,Hd,tree(r(vc,CNJCAT2),CNJDS22)|PredMs]).

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

%% don't touch it if it is a pronoun / single word already
generate_pronoun(adt_lex(np,B,_,pron,Atts),[],adt_lex(np,B,_,pron,Atts),[]).
generate_pronoun(adt_lex(np,B,_,det,Atts),[],adt_lex(np,B,_,det,Atts),[]).

%% a new pronoun
generate_pronoun(adt_lex(np,B,_,_,Atts0),[],adt_lex(np,Pronoun,_,_,Atts),[]) :-
    pronoun(B,_Dat,Atts0,Atts,Pronoun).

%% long np: find pronoun for its head
generate_pronoun(p(_),Ds0,adt_lex(np,Pronoun,_,_,Atts),[]) :-
    Hd = tree(r(hd,adt_lex(_,B,_,_,Atts0)),[]),
    Det = tree(r(det,adt_lex(_,DetL,_,_,_)),[]), 
    lists:member(Hd,Ds0),
    lists:member(Det,Ds0),
    !,
    pronoun(B,DetL,Atts0,Atts,Pronoun).

generate_pronoun(p(_),Ds0,adt_lex(np,Pronoun,_,_,Atts),[]) :-
    Hd = tree(r(hd,p(mwu(Lemma,_))),MWU),
    Det = tree(r(det,adt_lex(_,DetL,_,_,_)),[]), 
    lists:member(Hd,Ds0),
    lists:member(Det,Ds0),
    MWP = tree(r(mwp,adt_lex(np,_,_,_,Atts0)),[]),
    lists:select(MWP,MWU,_),
    !,
    pronoun(Lemma,DetL,Atts0,Atts,Pronoun).

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

generate_pronoun(p(conj),_,adt_lex(np,{[deze,die,ze]},_,_,[rnum=pl]),[]).

%% replace element by N elements
select_replace([H|T],H,List,Result) :-
    alpino_wappend:wappend(List,T,Result).
select_replace([H|T],D,List,[H|NT]) :-
    select_replace(T,D,List,NT).

split_transformation(tree(r(_,p(MAIN)),Ds0),[N1,N2]) :-
    lists:member(MAIN,[smain,sv1]),
    get_su_and_index(Ds0,Ds1,tree(r(su,i(Ix,SuCat)),SuDs)),
    VC = tree(r(vc,p(conj)),VCDS0),
    Hd = tree(r(hd,_),_),
    lists:select(Hd,Ds1,Ds2),
    lists:select(VC,Ds2,[]),
    CRD = tree(r(crd,_),_),
    lists:select(CRD,VCDS0,[CNJ1,CNJ2]),
    CNJ1 = tree(r(cnj,CNJCAT1),CNJDS1),
    CNJ2 = tree(r(cnj,CNJCAT2),CNJDS2),
    Su1 = tree(r(su,i(Ix,SuCat)),SuDs),
    Su2 = tree(r(su,i(XX,SuCat2)),SuDs2),
    generate_pronoun(SuCat,SuDs,SuCat2,SuDs2),
    hdrug_util:gen_sym(XX,adt_index),
    replace_index(CNJDS2,CNJDS22,Ix,XX),
    N1 = tree(r(top,p(top)),[tree(r('--',p(MAIN)),[Su1,Hd,tree(r(vc,CNJCAT1),CNJDS1)])]),
    N2 = tree(r(top,p(top)),[tree(r('--',p(MAIN)),[Su2,Hd,tree(r(vc,CNJCAT2),CNJDS22)])]).

split_transformation(tree(r(_,p(MAIN)),Ds0),[N1,N2,N3]) :-
    lists:member(MAIN,[smain,sv1]),
    get_su_and_index(Ds0,Ds1,tree(r(su,i(Ix,SuCat)),SuDs)),
    VC = tree(r(vc,p(conj)),VCDS0),
    Hd = tree(r(hd,_),_),
    lists:select(Hd,Ds1,Ds2),
    lists:select(VC,Ds2,[]),
    CRD = tree(r(crd,_),_),
    lists:select(CRD,VCDS0,[CNJ1,CNJ2,CNJ3]),
    CNJ1 = tree(r(cnj,CNJCAT1),CNJDS1),
    CNJ2 = tree(r(cnj,CNJCAT2),CNJDS2),
    CNJ3 = tree(r(cnj,CNJCAT3),CNJDS3),
    Su1 = tree(r(su,i(Ix,SuCat)),SuDs),
    Su2 = tree(r(su,i(XX,SuCat2)),SuDs2),
    Su3 = tree(r(su,i(XX2,SuCat2)),SuDs2),
    generate_pronoun(SuCat,SuDs,SuCat2,SuDs2),
    hdrug_util:gen_sym(XX,adt_index),
    replace_index(CNJDS2,CNJDS22,Ix,XX),
    hdrug_util:gen_sym(XX2,adt_index),
    replace_index(CNJDS3,CNJDS33,Ix,XX2),
    N1 = tree(r(top,p(top)),[tree(r('--',p(MAIN)),[Su1,Hd,tree(r(vc,CNJCAT1),CNJDS1)])]),
    N2 = tree(r(top,p(top)),[tree(r('--',p(MAIN)),[Su2,Hd,tree(r(vc,CNJCAT2),CNJDS22)])]),
    N3 = tree(r(top,p(top)),[tree(r('--',p(MAIN)),[Su3,Hd,tree(r(vc,CNJCAT3),CNJDS33)])]).

split_transformation(tree(r('--',p(conj)),Ds0),NewList) :-
    D1 = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    lists:select(D1,Ds0,ConjList),
    conj_list_split(ConjList,NewList).

split_transformation(tree(r('--',p(du)),Ds0),[TagFirst|NewList]) :-
    Tag = tree(r(tag,_),_),
    Nucl = tree(r(nucl,p(conj)),NuclDs0),
    lists:select(Tag,Ds0,[Nucl]),
    D1 = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    lists:select(D1,NuclDs0,ConjList),
    conj_list_split(ConjList,[First|NewList]),
    First = tree(r(top,p(top)),[tree(r('--',FirstCat),FirstDs)]),
    TagFirst = tree(r(top,p(top)),[tree(r('--',p(du)),[Tag,tree(r(nucl,FirstCat),FirstDs)])]).

%% ap: sterker nog, smain
%% advp: dan nog een korte opmerking over ..
%% smain , smain => smain. smain.
split_transformation(tree(r('--',p(du)),[D2,D3]),[X1,X2]) :-
    D2 = tree(r(dp,p(Cat1)),L1),
    \+ do_not_split_du(Cat1,L1),
    D3 = tree(r(dp,p(Cat2)),L2),
    X1 = tree(r(top,p(top)),[tree(r('--',p(Cat1)),L1)]),
    X2 = tree(r(top,p(top)),[tree(r('--',p(Cat2)),L2)]).

%% smain of smain ==> smain. Of smain.
%% must rule out "balansschikking" E-ANS ch 26.7
%% if there is a "negatief element" in the first clause
%% "we waren nog niet thuis of de telefoon ging"
%% 
split_transformation(tree(r('--',p(conj)),Ds0),[A1,A2]) :-
    D1 = tree(r(crd,adt_lex(_,of,_,_,_)),[]),
    D2 = tree(r(cnj,p(P1)),L1),
    D3 = tree(r(cnj,P2),L2),
    lists:select(D1,Ds0,[D2,D3]),
    \+ contains_negatief_element(L1),
    correct_conjunct(P1,P2,P,L2,L),
    A1 = tree(r(top,p(top)),[tree(r('--',p(P1)),L1)]),
    A2 = tree(r(top,p(top)),[tree(r('--',p(du)),[DLINK,BODY])]),
    DLINK = tree(r(dlink,adt_lex(_,of,_,_,[])),[]),
    BODY  = tree(r(nucl,P),L).

split_transformation(tree(r('--',p(conj)),Ds0),[A1,A2]) :-
    D1 = tree(r(crd,adt_lex(_,Maar,_,_,_)),[]),
    D2 = tree(r(cnj,p(P1)),L1),
    D3 = tree(r(cnj,P2),L2),
    lists:select(D1,Ds0,[D2,D3]),
    lists:member(Maar,[maar,dus,want]),
    correct_conjunct(P1,P2,P,L2,L),
    A1 = tree(r(top,p(top)),[tree(r('--',p(P1)),L1)]),
    A2 = tree(r(top,p(top)),[tree(r('--',p(du)),[DLINK,BODY])]),
    DLINK = tree(r(dlink,adt_lex(_,Maar,_,_,[])),[]),
    BODY  = tree(r(nucl,P),L).

%% SU [rel die/dat Rest] VP  ==> SU VP. SU Rest.
split_transformation(tree(r('--',p(smain)),Ds0),[X1,X2]) :-
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

%%% poor man's version of restricted relative clause
forbid_rel_split(adt_lex(_,HdLem,_,_,_)):-
    forbid_lemma_rel_split(HdLem).

forbid_lemma_rel_split(al).
forbid_lemma_rel_split(datgeen).
forbid_lemma_rel_split(degeen).
forbid_lemma_rel_split(diegene).
forbid_lemma_rel_split(één).
forbid_lemma_rel_split(enig).
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
forbid_lemma_rel_split(het).

diedat(Var,Lemma,Atts,DieDat) :-
    var(Var),
    !,
    guess_die_dat(Lemma,Atts,DieDat).
diedat(Val,_,_,DieDat) :-
    simple_diedat(Val,DieDat0),
    !,
    DieDat = DieDat0.
diedat(_,Lemma,Atts,DieDat) :-
    guess_die_dat(Lemma,Atts,DieDat).

simple_diedat(dit,dat).
simple_diedat(dat,dat).
simple_diedat(het,dat).
simple_diedat(de,die).
simple_diedat(die,die).
simple_diedat(deze,die).

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
    ;   atom_concat('De ',_,Lemma),
	Dat = die
    ;   atom_concat('Het ',_,Lemma),
	Dat = dat
    ).
guess_die_dat(_,_,_).

pronoun(HdLem,DieDat0,Atts0,Atts,Pron) :-
    diedat(DieDat0,HdLem,Atts0,DieDat),
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
pronoun_(Lemma,dit,sg,nonper,{[het]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,die,sg,nonper,{[deze,die,ze]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,het,sg,nonper,{[het]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,de,sg,nonper,{[deze,die,ze]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,Det,sg,nonper,{[deze,die,het,ze]},A,A) :-
    \+ lists:member(Det,[de,het,die,dat,dit]),
    \+ human(Lemma,_).
pronoun_(Lemma,_,pl,nonper,{[deze,die,ze]},A,A) :-
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

%% replace_index(CNJDS2,CNJDS22,Ix,XX),
replace_index([],[],_,_).
replace_index([H0|T0],[H|T],A,B) :-
    replace_index_tree(H0,H,A,B),
    replace_index(T0,T,A,B).

replace_index_tree(tree(r(Rel,Cat0),Ds0), tree(r(Rel,Cat),Ds),Ix0,Ix) :-
    replace_cat(Cat0,Cat,Ix0,Ix),
    replace_index(Ds0,Ds,Ix0,Ix).

replace_cat(p(Cat),p(Cat),_,_).
replace_cat(adt_lex(A,B,C,D,E),adt_lex(A,B,C,D,E),_,_).
replace_cat(i(X),i(Y),A,B) :-
    (   X == A
    ->  Y = B
    ;   X = Y
    ).
replace_cat(i(X,Cat),i(Y,Cat),A,B) :- % this should not happen?
    (   X == A
    ->  Y = B
    ;   X = Y
    ).


get_su_and_index(Ds0,Ds,tree(r(su,i(Ix,SuCat)),SuDs)) :-
    lists:select(tree(r(su,Cat),NDs),Ds0,Ds1),
    (   Cat = i(Ix,SuCat),
	NDs = SuDs,
	Ds1 = Ds
    ;   Cat = i(Ix),
	NDs = [],
	swap_index(Ds1,Ds,Ix,SuCat,SuDs), !
    ).

swap_index([H0|T],[H|T],Ix,Cat,Ds) :-
    swap_index_tree(H0,H,Ix,Cat,Ds).
swap_index([H|T0],[H|T],Ix,Cat,Ds) :-
    swap_index(T0,T,Ix,Cat,Ds).

swap_index_tree(tree(r(Rel,Cat0),Ds0), tree(r(Rel,Cat),Ds),Ix,C,D) :-
    swap_index_cat(Cat0,Cat,Ds0,Ds,Ix,C,D).
swap_index_tree(tree(r(Rel,Cat),Ds0), tree(r(Rel,Cat),Ds),Ix,C,D) :-
    swap_index(Ds0,Ds,Ix,C,D).

swap_index_cat(i(Ix0,Cat0),Cat,Ds0,Ds,Ix,C,D) :-
    Ix0 == Ix,
    Cat0 = C,
    Ds0 = D,
    Cat = i(Ix),
    Ds = [].

do_not_split_du(ap,_).
do_not_split_du(advp,_).
do_not_split_du(ppart,_).
do_not_split_du(mwu(_,_),L1) :-
    lists:member(tree(r(mwp,adt_lex(Cat,_,_,_,_)),[]),L1),
    do_not_split_du(Cat,[]).

contains_negatief_element(List) :-
    lists:member(tree(_,Ds),List),
    contains_negatief_element(Ds).
contains_negatief_element(List) :-
    lists:member(tree(r(_,adt_lex(_,L,_,_,_)),_),List),
    alpino_simplify_modifier:negatief_element(L).
contains_negatief_element(List) :-
    lists:member(tree(r(_,i(_,adt_lex(_,L,_,_,_))),_),List),
    alpino_simplify_modifier:negatief_element(L).


%     


conj_list_split([Conj|List],NewList) :-
    conj_split0(Conj,P1,NewList,NextList),
    conj_list_split(List,P1,NextList).

conj_list_split([],_,[]).
conj_list_split([Conj|List],P1,NewList) :-
    conj_split(Conj,P1,NewList,NextList),
    conj_list_split(List,P1,NextList).

%% de eerste, check cat
conj_split0(tree(r(cnj,p(P2)),L2), P2, [tree(r(top,p(top)),[tree(r('--',p(P2)),L2)])|List],List) :-
    lists:member(P2,[du,smain,sv1,whq]).
conj_split0(tree(r(crd,adt_lex(_,en,_,_,_)),[]),_,L,L).

conj_split(tree(r(cnj,P2),L2), P1, [tree(r(top,p(top)),[tree(r('--',P),L)])|List],List) :-
    correct_conjunct(P1, P2, P, L2, L).

conj_split(tree(r(crd,adt_lex(_,en,_,_,_)),[]),_,L,L).

correct_conjunct(P1, P2, P, L2, L) :-
    (   P1 = smain, P2 = p(ssub)
    ->  P = p(smain),
	lists:select(tree(r(hd,adt_lex(ssub,X1,X2,X3,X4)),[]),L2,L3),
	L = [tree(r(hd,adt_lex(smain,X1,X2,X3,X4)),[])|L3]
    ;   P2 = p(cp),
	lists:select(tree(r(mod,MOD),MODDS),L2,L3),
	P = p(du),
	DP1 = tree(r(dp,MOD),MODDS),
	DP2 = tree(r(dp,P2),L3),
	L = [DP1,DP2]
    ;   P = P2,
	L = L2
    ).
