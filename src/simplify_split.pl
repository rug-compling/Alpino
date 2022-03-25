:- module(alpino_simplify_split, [ apply_split_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%


apply_split_transformations(Tree0,Tree) :-
    simple_split_transformations(Tree0,Tree1),
    !,
    apply_split_transformations(Tree1,Tree).
apply_split_transformations(tree(Cat0,Ds0),Tree) :-
    split_transformation(Cat0,Ds0,Cat1,Ds1),
    alpino_cg:collapse_all(tree(Cat1,Ds1),Tree2,List),
				% if the relative is removed, we need to ensure
                                % its contents end up at the co-indexed node
    expand_all(Tree2,Tree3,List),  % if i(I) occurs in a split as first mention, it must become a pronoun

    !,
    apply_split_transformations(Tree3,Tree).
apply_split_transformations(Tree,Tree).


split_transformation(r(top,p(top)),Ds0,r(split,p(split)),Ds) :-
    select_replace(Ds0,D,Xs,Ds),
    split_transformation(D,Xs).
split_transformation(r(Rel,Node), Ds0,r(Rel,Node),Ds) :-
    split_transformations_list(Ds0,Ds).

split_transformations_list([tree(Node0,H0)|T],[tree(Node,H)|T]) :-
    split_transformation(Node0,H0,Node,H).
split_transformations_list([H|T0],[H|T]) :-
    split_transformations_list(T0,T).


simple_split_transformations(tree(Node0,Ds0),tree(Node,Ds)) :-
    simple_split_transformation(Node0,Ds0,Node,Ds).

simple_split_transformations(tree(Node,Ds0),tree(Node,Ds)) :-
    simple_split_transformations_list(Ds0,Ds).

simple_split_transformations_list([H0|Ds],[H|Ds]):-
    simple_split_transformations(H0,H).
simple_split_transformations_list([H|Ds0],[H|Ds]):-
    simple_split_transformations_list(Ds0,Ds).

%% het is zo/duidelijk dat X ==> X
simple_split_transformation(r(Rel,p(smain)),Ds0,r(Rel,p(SMAIN)),BODYDS) :-
    HD = tree(r(hd,adt_lex(smain,ben,_,verb,_)),[]),
    lists:select(HD,Ds0,Ds1),
    SUP = tree(r(sup,adt_lex(_,het,_,_,_)),[]),
    lists:select(SUP,Ds1,Ds2),
    PREDC = tree(r(predc,adt_lex(_,Zo,_,_,_)),[]),
    lists:select(PREDC,Ds2,[CP]),
    lemma_in(Zo,[duidelijk,over_duidelijk,zo]),
    CP = tree(r(su,p(cp)),CPDS0),
    DAT = tree(r(cmp,adt_lex(_,dat,_,_,_)),[]),
    lists:select(DAT,CPDS0,[BODY]),
    BODY = tree(r(body,p(SSUB_OR_CONJ)),BODYDS0),
    replace_hd_ssub_smain(SSUB_OR_CONJ,SMAIN,BODYDS0,BODYDS).

%% het klopt dat X => X
simple_split_transformation(r(Rel,p(smain)),Ds0,r(Rel,p(SMAIN)),BODYDS) :-
    HD = tree(r(hd,adt_lex(smain,klop,_,verb,_)),[]),
    lists:select(HD,Ds0,Ds1),
    SUP = tree(r(sup,adt_lex(_,het,_,_,_)),[]),
    lists:select(SUP,Ds1,[CP]),
    CP = tree(r(su,p(cp)),CPDS0),
    DAT = tree(r(cmp,adt_lex(_,dat,_,_,_)),[]),
    lists:select(DAT,CPDS0,[BODY]),
    BODY = tree(r(body,p(SSUB_OR_CONJ)),BODYDS0),
    replace_hd_ssub_smain(SSUB_OR_CONJ,SMAIN,BODYDS0,BODYDS).

%% iedereen weet dat X => X
simple_split_transformation(r(Rel,p(smain)),Ds0,r(Rel,p(SMAIN)),BODYDS) :-
    HD = tree(r(hd,adt_lex(smain,weet,_,verb,_)),[]),
    lists:select(HD,Ds0,Ds1),
    SU = tree(r(su,adt_lex(np,iedereen,_,_,_)),[]),
    lists:select(SU,Ds1,CPLIST),  
    CPLIST = [tree(r(vc,CPCAT),CPDS)],
    cp_to_smain(CPCAT,CPDS,SMAIN,BODYDS).

%% ik denk/vind dat X => X
simple_split_transformation(r(Rel,p(smain)),Ds0,r(Rel,p(SMAIN)),BODYDS) :-
    HD = tree(r(hd,adt_lex(smain,Vind,_,verb,Atts)),[]),
    lists:select(HD,Ds0,Ds1),
    lemma_in(Vind,[denk,meen,vind]),
    lists:member(tense=present,Atts),
    SU = tree(r(su,adt_lex(np,ik,_,_,_)),[]),
    lists:select(SU,Ds1,CPLIST),  
    CPLIST = [tree(r(vc,CPCAT),CPDS)],
    cp_to_smain(CPCAT,CPDS,SMAIN,BODYDS).

%% dat ik een boek koop en een plaat luister
%% dat ik een boek koop en dat PRON een plaat luister
simple_split_transformation(r(Rel,p(cp)),Ds0,Cat,D) :-
    \+ Rel = obcomp,
    CMP = tree(r(cmp,_),_),
    lists:select(CMP,Ds0,[BODY]),
    BODY = tree(r(body,p(conj)),CNJDS0),
    ssub_to_cp_conj(CNJDS0,CMP,CNJDS1),
    alpino_cg:collapse_all(tree(r(Rel,p(conj)),CNJDS1),tree(Cat,D0),List),
    expand_split_ds(D0,D1,List),
    rename_indices(D1,D).

%% 
%% Mijnheer de Voorzitter , ik zal het heel kort houden en de reglementaire spreektijd van één minuut niet overschrijden .
simple_split_transformation(r(Rel,p(MAIN)),Ds0,r(Rel,p(conj)),[CRD,N1,N2]) :-
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
    nonvar(Ds),
    nonvar(Cat),  % fail for cases in which antecedent "too far" above
    %  Zo : het gaat om wat de mensen denken dat de werkelijkheid is of wordt .
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
generate_pronoun(adt_lex(np,B,_,PronDet,Atts),[],adt_lex(np,B,_,PronDet,Atts),[]) :-
    \+ \+ lists:member(PronDet,[pron,det]).

%% a new pronoun
generate_pronoun(adt_lex(np,B,_,_,Atts0),[],adt_lex(np,Pronoun,_,PronDet,Atts),[]) :-
    pronoun(B,_Dat,Atts0,Atts,Pronoun),
    when(nonvar(PronDet),(PronDet=pron; PronDet=det)).

%% long np: find pronoun for its head
generate_pronoun(p(_),Ds0,adt_lex(np,Pronoun,_,PronDet,Atts),[]) :-
    Hd = tree(r(hd,adt_lex(_,B,_,_,Atts0)),[]),
    Det = tree(r(det,adt_lex(_,DetL,_,_,_)),[]), 
    lists:member(Hd,Ds0),
    lists:member(Det,Ds0),
    !,
    when(nonvar(PronDet),(PronDet=pron; PronDet=det)),
    pronoun(B,DetL,Atts0,Atts,Pronoun).

generate_pronoun(p(_),Ds0,adt_lex(np,Pronoun,_,PronDet,Atts),[]) :-
    Hd = tree(r(hd,p(mwu(Lemma,_))),MWU),
    Det = tree(r(det,adt_lex(_,DetL,_,_,_)),[]), 
    lists:member(Hd,Ds0),
    lists:member(Det,Ds0),
    MWP = tree(r(mwp,adt_lex(np,_,_,_,Atts0)),[]),
    lists:select(MWP,MWU,_),
    !,
    when(nonvar(PronDet),(PronDet=pron; PronDet=det)),
    pronoun(Lemma,DetL,Atts0,Atts,Pronoun).

%% long np: find pronoun for its head
generate_pronoun(p(_),Ds0,Pronoun,Ds) :-
    Hd = tree(r(hd,HdCat),[]),
    lists:member(Hd,Ds0),
    generate_pronoun(HdCat,[],Pronoun,Ds).

%% long name: find pronoun
generate_pronoun(p(mwu(Lemma,_)),Ds0,adt_lex(np,Pronoun,Pronoun,PronDet,Atts),[]) :-
    Hd = tree(r(mwp,adt_lex(np,_,_,_,Atts0)),[]),  % attributes are identical for all mwp's, right?
    lists:member(Hd,Ds0),
    when(nonvar(PronDet),(PronDet=pron; PronDet=det)),
    pronoun(Lemma,_,Atts0,Atts,Pronoun).

generate_pronoun(p(conj),_,adt_lex(np,{[deze,die]},_,_,[rnum=pl]),[]).

%% replace element by N elements
select_replace([H|T],H,List,Result) :-
    alpino_wappend:wappend(List,T,Result).
select_replace([H|T],D,List,[H|NT]) :-
    select_replace(T,D,List,NT).

split_transformation(tree(r(_,p(MAIN)),Ds0),[N1|NS]) :-
    lists:member(MAIN,[smain,sv1]),
    get_su_and_index(Ds0,Ds1,tree(r(su,i(Ix,SuCat)),SuDs)),
    VC = tree(r(vc,p(conj)),VCDS0),
    Hd = tree(r(hd,_),_),
    lists:select(Hd,Ds1,Ds2),
    lists:select(VC,Ds2,[]),
    CRD = tree(r(crd,adt_lex(_,EN,_,_,_)),_),
    lists:select(CRD,VCDS0,[CNJ1|CNJS]),
    !, % the first crd
    CNJ1 = tree(r(cnj,CNJCAT1),CNJDS1),
    Su1 = tree(r(su,i(Ix,SuCat)),SuDs),
    generate_pronoun(SuCat,SuDs,SuCat2,SuDs2),
    N1 = tree(r(top,p(top)),[tree(r('--',p(MAIN)),[Su1,Hd,tree(r(vc,CNJCAT1),CNJDS1)])]),
    (   EN == en
    ->  cnj_ds_vc(CNJS,NS,SuCat2,SuDs2,MAIN,Ix,Hd)
    ;   lists:member(EN,[maar,dus,want]),
	cnj_ds_vc_dlink(CNJS,NS,SuCat2,SuDs2,MAIN,Ix,Hd,EN)
    ).

split_transformation(tree(r('--',p(conj)),Ds0),NewList) :-
    D1 = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    lists:select(D1,Ds0,ConjList),
    conj_list_split(ConjList,NewList).

split_transformation(tree(r('--',p(du)),Ds0),[TagFirst|NewList]) :-
    Nucl = tree(r(OtherRel,p(conj)),NuclDs0),
    lists:select(Tag,Ds0,[Nucl]),
    short_du_part(Tag,OtherRel),
    D1 = tree(r(crd,adt_lex(_,en,_,_,_)),[]),
    lists:select(D1,NuclDs0,ConjList),
    conj_list_split(ConjList,[First|NewList]),
    First = tree(r(top,p(top)),[tree(r('--',FirstCat),FirstDs)]),
    TagFirst = tree(r(top,p(top)),[tree(r('--',p(du)),[Tag,tree(r(OtherRel,FirstCat),FirstDs)])]).

%% ap: sterker nog, smain
%% advp: dan nog een korte opmerking over ..
%% smain , smain => smain. smain.
split_transformation(tree(r('--',p(du)),[D2,D3]),[X1,X2]) :-
    D2 = tree(r(dp,p(Cat1)),L1),
    \+ do_not_split_du(Cat1,L1),
    D3 = tree(r(dp,p(Cat2)),L2),
    X1 = tree(r(top,p(top)),[tree(r('--',p(Cat1)),L1)]),
    X2 = tree(r(top,p(top)),[tree(r('--',p(Cat2)),L2)]).

%% neem me niet kwalijk , ik moet weg
%% neem me niet kwalijk. Ik moet weg.
split_transformation(tree(r('--',p(du)),[D2,D3]),[X1,X2]) :-
    D2 = tree(r(tag,p(sv1)),L1),
    imparative_hd(D2),
    \+ alpino_simplify_modifier:me_dunkt_ds(L1),
    D3 = tree(r(nucl,p(smain)),L2),
    X1 = tree(r(top,p(top)),[tree(r('--',p(sv1)),L1)]),
    X2 = tree(r(top,p(top)),[tree(r('--',p(smain)),L2)]).

%% meneer , neem me niet kwalijk , ik moet weg
%% => meneer , neem me niet kwalijk. Ik  moet weg.
split_transformation(tree(r('--',p(du)),Ds0),[X1,X2]) :-
    Tag =  tree(r(tag,_),_),
    Nucl = tree(r(nucl,p(du)),Ds),
    lists:select(Tag,Ds0,[Nucl]),
    D2 = tree(r(tag,p(sv1)),L1),
    lists:select(D2,Ds,[D3]),
    imparative_hd(D2),
    \+ alpino_simplify_modifier:me_dunkt_ds(L1),
    D3 = tree(r(nucl,p(smain)),L2),
    X1 = tree(r(top,p(top)),[tree(r('--',p(du)),[Tag,tree(r(nucl,p(sv1)),L1)])]),
    X2 = tree(r(top,p(top)),[tree(r('--',p(smain)),L2)]).

%% er was een probleem : ik ging weg
split_transformation(tree(r('--',p(du)),[D2,D3]),[X1,X2]) :-
    D2 = tree(r(nucl,p(Smain)),L1),
    D3 = tree(r(sat,SMAIN),L2),
    lists:member(Smain,[smain,du]),
    smain(SMAIN,L2),
    X1 = tree(r(top,p(top)),[tree(r('--',p(Smain)),L1)]),
    X2 = tree(r(top,p(top)),[tree(r('--',SMAIN),L2)]).

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

%% smain maar smain -> smain. Maar smain.
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

%% tag, smain maar smain -> tag, smain. Maar smain.
split_transformation(tree(r('--',p(du)),Ds0),[A1,A2]) :-
    Nucl = tree(r(OtherRel,p(conj)),NuclDs0),
    lists:select(Tag,Ds0,[Nucl]),
    short_du_part(Tag,OtherRel),
    D1 = tree(r(crd,adt_lex(_,Maar,_,_,_)),[]),
    D2 = tree(r(cnj,p(P1)),L1),
    D3 = tree(r(cnj,P2),L2),
    lists:select(D1,NuclDs0,[D2,D3]),
    lists:member(Maar,[maar,dus,want]),
    correct_conjunct(P1,P2,P,L2,L),
    A1 = tree(r(top,p(top)),[tree(r('--',p(du)),[Tag,tree(r(OtherRel,p(P1)),L1)])]),
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
    Rel = tree(r(mod,p(rel)), RelDs),
    lists:select(Rel,SuDs1,SuDs2),
    \+ forbid_rel_split(HdCat,SuDs2),
    Rhd = tree(r(rhd,i(Ix,adt_lex(_,Die,Die,_,RelAtts))),[]),
    Body= tree(r(body,p(ssub)),BodyDs0),
    lists:select(Rhd,RelDs,RelDs1),
    lists:member(Die,[die,dat]),
    lists:select(Body,RelDs1,[]),
    generate_rel_pronoun(HdCat,HdDs,Die,RelAtts,Atts,Pronoun),!,
    lex_replace(i(Ix),i(Ix,adt_lex(_,Pronoun,Pronoun,PronDet,Atts)),BodyDs0,BodyDs1),
    when(nonvar(PronDet),(PronDet=pron; PronDet=det)),
    BHd0 = tree(r(hd,adt_lex(ssub,B,C,D,E)),[]),
    BHd  = tree(r(hd,adt_lex(smain,B,C,D,E)),[]),
    replace(BHd0,BHd,BodyDs1,BodyDs),
    new_su(SuDs2,SuCat,SuDs),
    X1 = tree(r(top,p(top)),[tree(r('--',p(smain)),[Su|Ds])]),
    X2 = tree(r(top,p(top)),[tree(r('--',p(smain)),BodyDs)]).

new_su([D1,D2|Ds],p(np),[D1,D2|Ds]).
new_su([tree(r(hd,HdCat),HdDs)],HdCat,HdDs).

%%% poor man's version of restricted relative clause
forbid_rel_split(adt_lex(_,HdLem,_,_,_),_):-
    forbid_lemma_rel_split(HdLem).
forbid_rel_split(adt_lex(_,_,_,num,_),_).

%% er is geen mens die dat wil =/= er is geen mens. Hij wil dat.
forbid_rel_split(_,Ds):-
    contains_negatief_element(Ds).

forbid_lemma_rel_split(al).
forbid_lemma_rel_split(alles).
forbid_lemma_rel_split(daar).
forbid_lemma_rel_split(dat).
forbid_lemma_rel_split(datgeen).
forbid_lemma_rel_split(degeen).
forbid_lemma_rel_split(diegene).
forbid_lemma_rel_split(één).
forbid_lemma_rel_split(enig).
forbid_lemma_rel_split(er).
forbid_lemma_rel_split(het).
forbid_lemma_rel_split(ieder).
forbid_lemma_rel_split(iedereen).
forbid_lemma_rel_split(iets).
forbid_lemma_rel_split(niets).
forbid_lemma_rel_split(weinig).

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
    ;   atom(Lemma),
	atom_concat('De ',_,Lemma),
	Dat = die
    ;   atom(Lemma),
	atom_concat('Het ',_,Lemma),
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
    atom(L),
    atom_concat(Lem,'_DIM',L),
    pronoun_(Lem,DieDat,A,nonper,B,Atts0,Atts).
pronoun_(L,DieDat,A,nonper,B,Atts0,Atts) :-
    atom(L),
    atom_concat(_,Right,L),
    atom_concat('_',Lem,Right),
    pronoun_(Lem,DieDat,A,nonper,B,Atts0,Atts).

pronoun_(Lemma,_,sg,_,{[hij,hem]},A,A) :-
    human(Lemma,m).
pronoun_(Lemma,_,sg,_,{[zij,ze,haar]},A,A) :-
    human(Lemma,f).
pronoun_(Lemma,_,sg,_,{[hij,zij,ze,hem,haar]},A,A) :-
    human(Lemma,'').
pronoun_(Lemma,_,sg,_,{[zij,ze,hen,hun]},A0,A) :-
    human(Lemma,p),
    replace(rnum=sg,rnum=pl,A0,A).
pronoun_(_Lemma,_,sg,per,{[hij,zij,ze,hem,haar]},A,A).
pronoun_(Lemma,_,pl,_,{[zij,ze,hen,hun]},A,A) :-
    human(Lemma,_).
pronoun_(Lemma,dat,sg,nonper,het,A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,dit,sg,nonper,het,A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,die,sg,nonper,{[deze,die]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,het,sg,nonper,het,A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,de,sg,nonper,{[deze,die]},A,A) :-
    \+ human(Lemma,_).
pronoun_(Lemma,Det,sg,nonper,{[deze,die,het]},A,A) :-
    \+ lists:member(Det,[de,het,die,dat,dit]),
    \+ human(Lemma,_).
pronoun_(Lemma,_,pl,nonper,{[deze,die]},A,A) :-
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

do_not_split_du(whrel,_).  % hoe sneller ... hoe beter
do_not_split_du(detp,_).
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
    lists:member(P2,[du,smain,sv1,whq,conj]).
conj_split0(tree(r(crd,adt_lex(_,en,_,_,_)),[]),_,L,L).

conj_split(tree(r(cnj,P2),L2), P1, [tree(r(top,p(top)),[tree(r('--',P),L)])|List],List) :-
    correct_conjunct(P1, P2, P, L2, L).

conj_split(tree(r(crd,adt_lex(_,en,_,_,_)),[]),_,L,L).



correct_conjunct(Cat, p(ssub), P, L2, L) :-
    lists:member(Cat,[smain,sv1]),
    !,
    P = p(Cat),
    lists:select(tree(r(hd,adt_lex(ssub,X1,X2,X3,X4)),[]),L2,L3),
    L = [tree(r(hd,adt_lex(Cat,X1,X2,X3,X4)),[])|L3].
correct_conjunct(_,P2,P,L2,L):-
    correct_conjunct(P2,P,L2,L),
    !.
correct_conjunct(_,P,P,L,L) :-
    \+ do_not_split_conj(P,L).

correct_conjunct(p(cp),p(du),L2,[DP1,DP2]) :-
    lists:select(tree(r(mod,MOD),MODDS),L2,L3),
    DP1 = tree(r(dp,MOD),MODDS),
    DP2 = tree(r(dp,p(cp)),L3).
correct_conjunct(p(pp),p(du),L2,[DP1,DP2]) :-
    lists:select(tree(r(mod,MOD),MODDS),L2,L3),
    DP1 = tree(r(dp,MOD),MODDS),
    DP2 = tree(r(dp,p(pp)),L3).
correct_conjunct(p(Smain),p(Cat),L2,L) :-
    lists:member(Smain,[smain,whq]),
    replace(tree(r(hd,adt_lex(Smain,Prep,Prep2,Pos,Atts)),[]),
	    tree(r(hd,adt_lex(Cat,Prep,Prep2,Pos,[])),[]),
	    L2,L),
    lists:member(stype=whquestion,Atts),
    correct_wh_cat(Cat,Pos).
correct_conjunct(adt_lex(Smain,Prep,Prep2,Pos,Atts),adt_lex(Cat,Prep,Prep2,Pos,[]),[],[]) :-
    lists:member(Smain,[smain,whq]),
    lists:member(stype=whquestion,Atts),
    correct_wh_cat(Cat,Pos).

imparative_hd(tree(r(tag,p(sv1)),Ds)) :-
    Hd = tree(r(hd,adt_lex(_,_,_,_,Atts)),[]),
    lists:member(Hd,Ds),
    lists:member(stype=imparative,Atts).

smain(p(smain),_).
smain(p(whq),_).
smain(p(conj),List) :-
    lists:member(tree(r(cnj,Cat),List2),List),
    smain(Cat,List2).

short_du_part(tree(r(tag,_),_),nucl).
short_du_part(tree(r(dlink,_),_),nucl).
short_du_part(tree(r(dp,p(Cat)),Ds),dp) :-
    do_not_split_du(Cat,Ds).

correct_wh_cat(pp,pp).
correct_wh_cat(pp,prep).
correct_wh_cat(np,noun).
correct_wh_cat(np,pron).
correct_wh_cat(ap,adj).
correct_wh_cat(advp,adv).

cnj_ds_vc([],[],_,_,_,_,_).
cnj_ds_vc([CRD|CNJS],NS,SuCat2,SuDs2,CAT,Ix,Hd) :-
    CRD = tree(r(crd,adt_lex(_,EN,_,_,_)),[]),
    lists:member(EN,[en,maar,dus,want]),
    cnj_ds_vc_dlink(CNJS,NS,SuCat2,SuDs2,CAT,Ix,Hd,EN).
cnj_ds_vc([CNJ2|CNJS],[N2|NS],SuCat2,SuDs2,CAT,Ix,Hd) :-
    CNJ2 = tree(r(cnj,CNJCAT2),CNJDS2),
    Su2 = tree(r(su,i(XX,SuCat2)),SuDs2),
    hdrug_util:gen_sym(XX,adt_index),
    replace_index(CNJDS2,CNJDS22,Ix,XX),
    N2 = tree(r(top,p(top)),[tree(r('--',p(CAT)),[Su2,Hd,tree(r(vc,CNJCAT2),CNJDS22)])]),
    cnj_ds_vc(CNJS,NS,SuCat2,SuDs2,CAT,Ix,Hd).

cnj_ds_vc_dlink([],[],_,_,_,_,_,_).  %how can this happen?
cnj_ds_vc_dlink([CNJ2|CNJS],[N2|NS],SuCat2,SuDs2,CAT,Ix,Hd,Maar) :-
    CNJ2 = tree(r(cnj,CNJCAT2),CNJDS2),
    Su2 = tree(r(su,i(XX,SuCat2)),SuDs2),
    hdrug_util:gen_sym(XX,adt_index),
    replace_index(CNJDS2,CNJDS22,Ix,XX),
    N2 = tree(r(top,p(top)),[tree(r('--',p(du)),[tree(r(dlink,adt_lex(_,Maar,_,_,[])),[]),
						 tree(r(nucl,p(CAT)),[Su2,Hd,tree(r(vc,CNJCAT2),CNJDS22)])])]),
    cnj_ds_vc(CNJS,NS,SuCat2,SuDs2,CAT,Ix,Hd).


do_not_split_conj(adt_lex(_,andersom,_,_,_),[]).
do_not_split_conj(p(ap),[tree(r(hd,adt_lex(_,uit,_,_,_)),[]),
			 tree(r(mod,adt_lex(_,daarmee,_,_,_)),[])]).

/*
add_pronouns_for_su_in_conj([],[],_,_,_).
add_pronouns_for_su_in_conj([H0|T0],[H|T],Ix,Cat,Ds) :-
    add_pronouns_for_su_in_conj1(H0,H,Ix,Cat,Ds),
    add_pronouns_for_su_in_conj(T0,T,Ix,Cat,Ds).

add_pronouns_for_su_in_conj1(tree(r(crd,CRD),CRDDS),tree(r(crd,CRD),CRDDS),_,_,_).
add_pronouns_for_su_in_conj1(tree(r(cnj,Cat),CNJDS0),tree(r(cnj,Cat),CNJDS),IX,SuCat,SuDs) :-
    Su0 = tree(r(su,i(IXS)),[]),
    Su = tree(r(su,i(XX,SuCat2)),SuDs2),
    replace(Su0,Su,CNJDS0,CNJDS1),
    IX == IXS,
    generate_pronoun(SuCat,SuDs,SuCat2,SuDs2),
    hdrug_util:gen_sym(XX,adt_index),
    replace_index(CNJDS1,CNJDS,IX,XX).
*/

replace_hd_ssub_smain(ssub,smain,BODYDS0,BODYDS) :-
    A0 = tree(r(hd,adt_lex(_,ARR1,ARR2,ARR3,ARR4)),[]),
    A1 = tree(r(hd,adt_lex(smain,ARR1,ARR2,ARR3,[stype=declarative|ARR4])),[]),
    replace(A0,A1,BODYDS0,BODYDS).

replace_hd_ssub_smain(conj,conj,BODYDS0,BODYDS) :-
    replace_hd_ssub_smain_ds(BODYDS0,BODYDS).

replace_hd_ssub_smain_ds([],[]).
replace_hd_ssub_smain_ds([H0|T0],[H|T]) :-
    replace_hd_ssub_smain_d(H0,H),
    replace_hd_ssub_smain_ds(T0,T).

replace_hd_ssub_smain_d(tree(r(crd,CRD),CDS),tree(r(crd,CRD),CDS)).
replace_hd_ssub_smain_d(tree(r(cnj,p(ssub)),DS0), tree(r(cnj,p(smain)),DS)) :-
    replace_hd_ssub_smain(ssub,_,DS0,DS).



lemma_in({Lemma},List):-
    !,
    lists:member(L,Lemma),
    lists:member(L,List).
lemma_in(Lemma,List) :-
    lists:member(Lemma,List).

lemma({Lemma},Lem) :-
    !,
    lists:member(Lem,Lemma).
lemma(L,L).%

cp_to_smain(p(cp),CPDS0,CAT,BODYDS) :-
    DAT = tree(r(cmp,adt_lex(_,dat,_,_,_)),[]),
    lists:select(DAT,CPDS0,[BODY]),
    BODY = tree(r(body,p(SSUBORCONJ)),BODYDS0),
    replace_hd_ssub_smain(SSUBORCONJ,CAT,BODYDS0,BODYDS).

cp_to_smain(p(conj),CPDS0,conj,BODYDS) :-
    cp_to_smain_conj(CPDS0,BODYDS).

cp_to_smain_conj([],[]).
cp_to_smain_conj([Tree0|Trees0],[Tree|Trees]):-
    cp_so_smain_conj1(Tree0,Tree),
    cp_to_smain_conj(Trees0,Trees).

cp_so_smain_conj1(tree(r(crd,CRDCAT),CRDDS),tree(r(crd,CRDCAT),CRDDS)).
cp_so_smain_conj1(tree(r(cnj,p(cp)),CPDS0),tree(r(cnj,p(SMAIN)),CPDS)):-
    cp_to_smain(p(cp),CPDS0,SMAIN,CPDS).

ssub_to_cp_conj([],_,[]).
ssub_to_cp_conj([H0|T0],CMP,[H|T]) :-
    ssub_to_cp_conj1(H0,CMP,H),
    ssub_to_cp_conj(T0,CMP,T).

ssub_to_cp_conj1(tree(r(crd,CRD),DS),_,tree(r(crd,CRD),DS)).
ssub_to_cp_conj1(tree(r(cnj,p(ssub)),DS),CMP,tree(r(cnj,p(cp)),[CMP,tree(r(body,p(ssub)),DS)])).

rename_indices([],[]).
rename_indices([tree(r(Rel,Cat0),Ds0)|T0],[tree(r(Rel,Cat),Ds)|T]) :-
    rename_indices(Cat0,Cat,Ds0,Ds),
    rename_indices(T0,T).

rename_indices(Cat0,Cat,Ds0,Ds) :-
    rename_indices_cat(Cat0,Cat,Map),
    rename_indices_ds(Ds0,Ds,Map).

rename_indices_cat(p(Cat),p(Cat),_).
rename_indices_cat(adt_lex(A,B,C,D,E),adt_lex(A,B,C,D,E),_).
rename_indices_cat(i(X),i(Y),Map) :-
    lists:memberchk(X=Y,Map).
rename_indices_cat(i(X,Cat),i(Y,Cat),Map) :-
    hdrug_util:gen_sym(Y,adt_index),
    lists:memberchk(X=Y,Map).

rename_indices_ds([],[],_).
rename_indices_ds([tree(r(Rel,Cat0),Ds0)|T0],[tree(r(Rel,Cat),Ds)|T],Map) :-
    rename_indices_cat(Cat0,Cat,Map),
    rename_indices_ds(Ds0,Ds,Map),
    rename_indices_ds(T0,T,Map).




