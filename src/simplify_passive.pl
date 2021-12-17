:- module(alpino_simplify_passive, [ apply_passive_transformations/2 ]).

apply_passive_transformations(Tree0,Tree) :-
    dont_paraphrase(Tree0), !,
    Tree0 = Tree.

apply_passive_transformations(Tree0,Tree) :-
    apply_unraise_transformations(Tree0,Tree1),
    passive_transformations(Tree1,Tree2,[]),
    apply_raise_transformations(Tree2,Tree).

passive_transformations(tree(Cat0,Ds0),tree(Cat,Ds),C) :-
    passive_transformation(Cat0,Ds0,Cat1,Ds1,C),
    !,
    passive_transformations(tree(Cat1,Ds1),tree(Cat,Ds),C).
passive_transformations(tree(Cat,Ds0),tree(Cat,Ds),C) :-
    Cat = r(Rel,_),
    raiser_head(Ds0,Raiser),
    passive_transformations_list(Ds0,Ds,[Raiser/Rel|C]).

passive_transformations_list([],[],_).
passive_transformations_list([H0|T0],[H|T],C) :-
    passive_transformations(H0,H,C),
    passive_transformations_list(T0,T,C).

apply_unraise_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    unraise_transformation(Cat0,Ds0,Cat1,Ds1),
    !,
    apply_unraise_transformations(tree(Cat1,Ds1),tree(Cat,Ds)).
apply_unraise_transformations(tree(Cat,Ds0),tree(Cat,Ds)) :-
    apply_unraise_transformations_list(Ds0,Ds).

apply_unraise_transformations_list([],[]).
apply_unraise_transformations_list([H0|T0],[H|T]) :-
    apply_unraise_transformations(H0,H),
    apply_unraise_transformations_list(T0,T).

apply_raise_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    apply_raise_transformations_list(Ds0,Ds1),
    (   raise_transformation(Cat0,Ds1,Cat1,Ds2)
    ->  Cat1=Cat, Ds2=Ds
    ;   Cat0=Cat, Ds1=Ds
    ).

apply_raise_transformations_list([],[]).
apply_raise_transformations_list([H0|T0],[H|T]) :-
    apply_raise_transformations(H0,H),
    apply_raise_transformations_list(T0,T).


%% inf-vp
unraise_transformation(r(Rel,Cat),Ds0,
		       r(Rel,Cat),Ds) :-
    Hd = tree(r(hd,adt_lex(_,L,_M,_V,_F)),[]),
    Su = tree(r(su,i(Var,_)),_),
    VCCAT = p(inf),
    Vc0 = tree(r(vc,VCCAT),VCDS0),
    lists:select(Hd,Ds0,Ds1),
    lists:select(Su,Ds1,[Vc0]),
    raiser(L),
    lists:select(tree(r(su,i(Var)),[]),VCDS0,VCDS),
    Vc = tree(r(vc,VCCAT),[Su|VCDS]),
    Ds = [Hd,Vc].

%% te-vp
unraise_transformation(r(Rel,Cat),Ds0,
		       r(Rel,Cat),Ds) :-
    Hd = tree(r(hd,adt_lex(_,L,_M,_V,_F)),[]),
    Su = tree(r(su,i(Var,_)),_),
    VCCAT = p(ti),
    Vc0 = tree(r(vc,VCCAT),VCDS0),
    Comp = tree(r(cmp,_),_),
    Body0 = tree(r(body,BODYCAT),BODYDS0),
    Body  = tree(r(body,BODYCAT),[Su|BODYDS]),
    lists:select(Hd,Ds0,Ds1),
    lists:select(Su,Ds1,[Vc0]),
    lists:select(Comp,VCDS0,[Body0]),    
    raiser(L),
    lists:select(tree(r(su,i(Var)),[]),BODYDS0,BODYDS),
    Vc = tree(r(vc,VCCAT),[Comp,Body]),
    Ds = [Hd,Vc].


%% to do:
%% raising in er-pp
%% men_i deed er alles aan om PRO_i op tijd te komen

%% inf-vp
raise_transformation(r(Rel,Cat),Ds0,
		     r(Rel,Cat),Ds) :-
    Hd = tree(r(hd,adt_lex(_,L,_M,_V,_F)),[]),
    lists:select(Hd,Ds0,[Vc0]),
    raiser(L),
    VCCAT = p(inf),
    Vc0 = tree(r(vc,VCCAT),VCDS0),
    Su0 = tree(r(su,SuSu),SuDs),
    lists:select(Su0,VCDS0,VCDS),
    (   SuSu = i(IX,SuCat)
    ;   SuSu = SuCat,
	hdrug_util:gen_sym(IX,adt_index)
    ),
    Su  = tree(r(su,i(IX,SuCat)),SuDs),
    Vc = tree(r(vc,VCCAT),[tree(r(su,i(IX)),[])|VCDS]),
    Ds = [Hd,Su,Vc].

%% te-vp
raise_transformation(r(Rel,Cat),Ds0,
		     r(Rel,Cat),Ds) :-
    Hd = tree(r(hd,adt_lex(_,L,_M,_V,_F)),[]),
    lists:select(Hd,Ds0,[Vc0]),
    raiser(L),
    VCCAT = p(ti),
    Vc0 = tree(r(vc,VCCAT),VCDS0),
    Comp = tree(r(cmp,_),_),
    Body0 = tree(r(body,BODYCAT),BODYDS0),
    Body = tree(r(body,BODYCAT),[tree(r(su,i(IX)),[])|BODYDS]),
    lists:select(Comp,VCDS0,[Body0]),
    
    Su0 = tree(r(su,SuSu),SuDs),
    lists:select(Su0,BODYDS0,BODYDS),
    (   SuSu = i(IX,SuCat)
    ;   SuSu = SuCat,
	hdrug_util:gen_sym(IX,adt_index)
    ),
    Su  = tree(r(su,i(IX,SuCat)),SuDs),
    Vc = tree(r(vc,VCCAT),[Comp,Body]),
    Ds = [Hd,Su,Vc].

passive_transformation(r(Rel,VAR),A,
		       r(Rel2,i(X,Cat2)),C,Context) :-
    nonvar(VAR),
    VAR = i(X,Cat),
    passive_transformation(r(Rel,Cat),A,
			   r(Rel2,Cat2),C,Context).


passive_transformation(Cat0,Ds0,Cat,Ds,Context) :-
    passive_transformation(Cat0,Ds0,Cat1,Ds1,Context,Door),
    add_su_control(Door,Cat1,Ds1,Cat,Ds).

%% passive with "worden"
%% ik werd door Piet geslagen ==> Piet sloeg mij
passive_transformation(r(Rel,Cat),Ds0,
		       r(Rel,Cat),Ds, Context,Door ) :-

    Su = tree(r(su,SUCAT1),SUDS1),
    Hd = tree(r(hd,adt_lex(LexCat,word,word,verb,Feats)),[]),
    Vc = tree(r(vc,p(ppart)),VcDs0),

    lists:select(Su,Ds0,Ds1),  \+ men(Su),  
    context_allows_passive(Rel,Cat,Context,Su),
    lists:select(Hd,Ds1,Ds2),  
    lists:select(Vc,Ds2,DsRest), 

    SuI = tree(r(obj1,SUCAT2),SUDS2),
    lists:select(SuI,VcDs0,VcDs1),

    sucat(SUCAT1,SUDS1,SUCAT2,SUDS2,SUCAT3,SUDS3),
    
    EmbHd = tree(r(hd,adt_lex(_,L,M,V,_)),[]),
    NewHd = tree(r(hd,adt_lex(LexCat,L,M,V,Feats)),[]),

    lists:select(EmbHd,VcDs1,VcDs2),

    select_doorpp(VcDs2,VcDs3,Obj1Cat,Obj1Ds,Door),

    verb_allows_passive(L,word,person,Door),

    remove_er(VcDs3,VcDs4),

    NewSu = tree(r(su,Obj1Cat),Obj1Ds),

    Ds = [NewSu,NewHd,tree(r(obj1,SUCAT3),SUDS3)|REST],

    lists:append(DsRest,VcDs4,REST).


%% impersonal passive with "worden"
%% toen werd door sommigen geapplaudiseerd ==> sommigen applaudiseerden toen
%% TODO (?): check that there is a frame for the resulting pred-arg struct, e.g.,
%% if there are no further complements, check that there is an intransitive
%% frame for the verb
passive_transformation(r(Rel,Cat),Ds0,
		       r(Rel,Cat),Ds,  Context, Door ) :-

    context_allows_passive(Rel,Cat,Context,none),

    Su = tree(r(su,_),_),
    Hd = tree(r(hd,adt_lex(LexCat,word,word,verb,Feats)),[]),
    Vc = tree(r(vc,p(ppart)),VcDs0),

    \+ lists:select(Su,Ds0,_),  
    lists:select(Hd,Ds0,Ds1),  
    lists:select(Vc,Ds1,DsRest), 

    EmbHd = tree(r(hd,adt_lex(_,L,M,V,_)),[]),
    NewHd = tree(r(hd,adt_lex(LexCat,L,M,V,Feats)),[]),

    lists:select(EmbHd,VcDs0,VcDs2),

    select_doorpp(VcDs2,VcDs3,Obj1Cat,Obj1Ds,Door),

    verb_allows_passive(L,word,not_person,Door),

    NewSu = tree(r(su,Obj1Cat),Obj1Ds),

    Ds = [NewSu,NewHd|REST],

    remove_er(VcDs3,VcDs4),
    
    lists:append(DsRest,VcDs4,REST).

%% passive with "zijn"
%% ik ben geslagen door Piet => Piet heeft mij geslagen
passive_transformation(r(Rel,Cat),Ds0,
		       r(Rel,Cat),Ds,  Context, Door ) :-


    Su = tree(r(su,SUCAT1),SUDS1),
    Hd = tree(r(hd,adt_lex(_,ben,ben,verb,Feats)),[]),
    Vc = tree(r(vc,p(ppart)),VcDs0),

    lists:select(Su,Ds0,Ds1),  \+ men(Su), 
    context_allows_passive(Rel,Cat,Context,Su),
    lists:select(Hd,Ds1,Ds2),  
    lists:select(Vc,Ds2,DsRest), 

    SuI = tree(r(obj1,SUCAT2),SUDS2),
    lists:select(SuI,VcDs0,VcDs1),

    EmbHd = tree(r(hd,adt_lex(_,L,_,_,_)),[]),
    lists:member(EmbHd,VcDs1),
    
    sucat(SUCAT1,SUDS1,SUCAT2,SUDS2,SUCAT3,SUDS3),

    select_doorpp(VcDs1,VcDs2,Obj1Cat,Obj1Ds,Door),

    remove_er(VcDs2,VcDs3),

    verb_allows_passive(L,ben,person,Door),

    Ds = [NewSu,NewHd,NewVc|DsRest],

    NewSu = tree(r(su,i(XX,Obj1Cat)),Obj1Ds),
    NewHd = tree(r(hd,adt_lex(_,PERFECT,PERFECT,verb,Feats)),[]),
    NewVc = tree(r(vc,p(ppart)),NewVcDs),

    NewVcDs = [tree(r(su,i(XX)),[]),
	       tree(r(obj1,SUCAT3),SUDS3)|VcDs3],
    hdrug_util:gen_sym(XX,adt_index),
    PERFECT = {[heb,ben]}.

%% impersonal passive with "zijn"
%% toen is door sommigen geprotesteerd ==> Sommigen hebben toen geprotesteerd
passive_transformation(r(Rel,Cat),Ds0,
		       r(Rel,Cat),Ds,  Context, Door ) :-

    context_allows_passive(Rel,Cat,Context,none),

    Su = tree(r(su,_),_),
    Hd = tree(r(hd,adt_lex(_,ben,ben,verb,Feats)),[]),
    Vc = tree(r(vc,p(ppart)),VcDs0),

    \+ lists:select(Su,Ds0,_),  
    lists:select(Hd,Ds0,Ds2),  
    lists:select(Vc,Ds2,DsRest), 

    select_doorpp(VcDs0,VcDs2,Obj1Cat,Obj1Ds,Door),

    EmbHd = tree(r(hd,adt_lex(_,L,_,_,_)),[]),
    lists:member(EmbHd,VcDs2),

    verb_allows_passive(L,ben,not_person,Door),
    
    Ds = [NewSu,NewHd,NewVc|DsRest],

    NewSu = tree(r(su,i(XX,Obj1Cat)),Obj1Ds),
    NewHd = tree(r(hd,adt_lex(_,PERFECT,PERFECT,verb,Feats)),[]),
    NewVc = tree(r(vc,p(ppart)),NewVcDs),

    remove_er(VcDs2,VcDs3),
    
    NewVcDs = [tree(r(su,i(XX)),[])|VcDs3],
    hdrug_util:gen_sym(XX,adt_index),
    PERFECT = {[heb,ben]}.

add_su_control(not_door,Node,Ds0,Node,Ds) :-
    Hd = tree(r(hd,adt_lex(_,Proberen,_,verb,_)),[]),
    lists:member(Hd,Ds0),
    lists:member(Proberen,[wens,wensen,besluit,besluiten,
		     probeer,proberen,begin,beginnen,hoef,hoeven]),
    Su0 = tree(r(su,SuNode),SuDs),  
    Su  = tree(r(su,NewSuNode),SuDs),
    replace(Su0,Su,Ds0,Ds1),

    su_node(SuNode,NewSuNode,NewSu),
    

    lists:member(VC0,Ds1),

    vp_argument(VC0,VC,NewSu),
    
    replace(VC0,VC,Ds1,Ds),
    !.
add_su_control(_,N,D,N,D).

%% do not allow vp's:
%% ik wil worden geslagen door Piet =/= Piet wil mij slaan
empty(tree(r(_,i(_)),[])).

%% if men is subject, it cannot be unpassived, because men
%% cannot be used as direct object
men(tree(r(_,adt_lex(_,men,_,_,_)),[])).
men(tree(r(_,i(_,adt_lex(_,men,_,_,_))),[])).

sucat(i(I),[],i(I),[],i(I),[]).
sucat(i(I),[],i(I,Cat),Ds,i(I,Cat),Ds).
sucat(i(I,Cat),Ds,i(I),[],i(I,Cat),Ds).       

select_doorpp(VcDs2,VcDs3,Obj1Cat,Obj1Ds,Door) :-
    lists:select(DoorPP,VcDs2,VcDs3),
    doorpp(DoorPP,Obj1Cat,Obj1Ds),
    !,
    Door = door.
select_doorpp(Ds,Ds,adt_lex(np,men,men,pron,[]),[],not_door).

doorpp(DoorPP,Obj1Cat,Obj1Ds) :-
    DoorPP = tree(r(mod,p(pp)),PPDS),
    Door = tree(r(hd,adt_lex(pp,door,door,prep,[])),_),
    Obj  = tree(r(obj1,Obj1Cat0),Obj1Ds0),
    lists:select(Door,PPDS,PPDS1),
    lists:select(Obj,PPDS1,Rest),
    transform_er(Obj1Ds0,Obj1Ds1,Obj1Cat0,Obj1Cat1),
    add_ds(Rest,Obj1Cat1,Obj1Ds1,Obj1Cat,Obj1Ds).

doorpp(tree(r(mod,adt_lex(pp,Hierdoor,Hierdoor,pp,[])),[]),adt_lex(Cat,Dit,Dit,Pos,Atts),[]) :-
    hierdoor(Hierdoor,Hier),
    er(adv,Pos,_,Cat,Hier,Dit,Hier,Dit,[],Atts).

%% door Piet, door Marie en door Kees => Piet, Marie en Kees
doorpp(tree(r(mod,p(conj)),ConjDs),p(conj),ObjDs) :-
    select_doorpps(ConjDs,ObjDs).

select_doorpps([],[]).
select_doorpps([D|Ds],[C|Cs]) :-
    select_doorpp(D,C),
    select_doorpps(Ds,Cs).

select_doorpp(tree(r(crd,Crd),CrdDs), tree(r(crd,Crd),CrdDs)).
select_doorpp(tree(r(cnj,p(pp)),PPDS), tree(r(cnj,Obj1Cat),Obj1Ds)) :-
    Door = tree(r(hd,adt_lex(pp,door,door,prep,[])),_),
    Obj  = tree(r(obj1,Obj1Cat0),Obj1Ds0),
    lists:select(Door,PPDS,PPDS1),
    lists:select(Obj,PPDS1,Rest),
    transform_er(Obj1Ds0,Obj1Ds1,Obj1Cat0,Obj1Cat1),
    add_ds(Rest,Obj1Cat1,Obj1Ds1,Obj1Cat,Obj1Ds).

add_ds([],Cat,Ds,Cat,Ds).
add_ds([H|T],Cat0,Ds0,Cat,Ds) :-
    add_m_ds(Cat0,Ds0,[H|T],Cat,Ds).

add_m_ds(p(Cat),Ds0,[H|T],p(Cat),Ds) :-
    lists:append([H|T],Ds0,Ds).
add_m_ds(adt_lex(Cat,A,B,C,D),[],[H|T],p(Cat),[tree(r(hd,adt_lex(Cat,A,B,C,D)),[]),H|T]).

hierdoor(hierdoor,hier).
hierdoor(daardoor,daar).
hierdoor(erdoor,er).
 

transform_er([H|T],[Hd2|List],p(advp),p(ObjCat)) :-
    Hd  = tree(r(hd,adt_lex(K0,L0,M0,Pos0,N0)),[]),
    lists:select(Hd,[H|T],List1),
    er(Pos0,Pos,K0,K,L0,L,M0,M,N0,N),
    !,
    Hd2  = tree(r(hd,adt_lex(K, L, M, Pos ,N )),[]),
    List = List1,
    ObjCat = K.
transform_er([H|T],[H|T],Cat,Cat).
transform_er([],[],adt_lex(K0,L0,M0,Pos0,N0),adt_lex(K,L,M,Pos,N)):-
    er(Pos0,Pos,K0,K,L0,L,M0,M,N0,N),!.
transform_er([],[],Lex,Lex).

%%% TODO: these can be head of a word group
%%% ik word [nog overal] door uitgedaagd

er(adv,det,advp,np,er,het,er,het,[],[rnum=sg]).
er(adv,det,advp,np,daar,dat,daar,dat,[],[rnum=sg]).
er(adv,det,advp,np,hier,dit,hier,dit,[],[rnum=sg]).
er(adv,noun,advp,np,overal,alles,overal,alles,[],[rnum=sg]).
er(adv,noun,advp,np,nergens,niets,nergens,niets,[],[rnum=sg]).
er(adv,noun,advp,np,ergens,iets,ergens,iets,[],[rnum=sg]).

remove_er(List0,List) :-
    Er = tree(r(mod,adt_lex(_,er,er,adv,_)),[]),
    lists:select(Er,List0,List).
remove_er(List,List).

%% Maria wil worden gekust door Noa =/= Noa wil Maria kussen
context_allows_passive(vc,p(inf),[noraiser/_|_],Su) :- \+ empty(Su), Su \= none.
%% Frankrijk verzoekt op de hoogte te worden gesteld =/= Frankrijk verzoekt men stelt op de hoogte
context_allows_passive(body,p(inf),[_/vc,noraiser/_|_],Su) :- \+ empty(Su), Su \= none.
%% Maria zou worden gekust door Noa = Noa zou Maria kussen
context_allows_passive(vc,p(inf),[raiser/_|_],_).
%% Maria bleek te worden gekust door Noa = Noa bleek Maria te kussen
context_allows_passive(body,p(inf),[_/vc,raiser/_|_],_).
context_allows_passive(_,p(smain),_,_).
context_allows_passive(_,p(ssub),_,_).
context_allows_passive(_,p(sv1),_,_).

%% verb_allows_passive(Verb,word/ben,person/not_person,door/not_door)

verb_allows_passive(V,Word,Person,Door) :-
    verb_disallows_passive(V,Word,Person,Door),
    !,
    fail.
verb_allows_passive(_,_,_,_).

verb_disallows_passive(bedek,ben,_,not_door).
verb_disallows_passive(beken,_,_,_).
verb_disallows_passive(ben,_,_,_).                 % zoals jarenlang het geval is geweest
verb_disallows_passive(bevriend,_,_,_).
verb_disallows_passive(breek_aan,ben,_,not_door).  % het moment is aangebroken =/= men breekt het moment aan
verb_disallows_passive(geboren,_,_,_).
verb_disallows_passive(interesseer,ben,_,not_door).
verb_disallows_passive(hecht,ben,_,not_door).
verb_disallows_passive(open,ben,_,not_door).
verb_disallows_passive(ontwikkel,ben,_,not_door).
verb_disallows_passive(overtuig,ben,_,not_door).
verb_disallows_passive(rechtvaardig,ben,_,not_door).
verb_disallows_passive(relateer,ben,_,not_door).
verb_disallows_passive(sluit,ben,_,not_door).
verb_disallows_passive(vestig,ben,_,not_door).
verb_disallows_passive(verbind,ben,_,not_door).
verb_disallows_passive(voorzien,ben,_,not_door).

%% dun bevolkt
%% beperkt?
%% haar ogen zijn gesloten


raiser(begin).
raiser(behoef).
raiser(behoor).
raiser(blijk).
raiser(dien).
raiser(dreig).
raiser(ga).
raiser(hoef).
raiser(hoor).
raiser(kan).
raiser(lijk).
raiser(mag).
raiser(moet).
raiser(placht).
raiser(schijn).
raiser(zal).

%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).


vp_argument(tree(r(vc,p(ti)),CmpInf0),
	    tree(r(vc,p(ti)),[tree(r(cmp,Cmp),[]),
			      tree(r(body,InfCat),[NewSu|VCDs])
			     ]),
	    NewSu) :-
    lists:select(tree(r(cmp,Cmp),[]),CmpInf0,[tree(r(body,InfCat0),VCDs0)]),
    Cmp = adt_lex(_,te,_,_,_),
    NotSu = tree(r(su,_),_),
    \+ lists:member(NotSu,VCDs0),
    add_hd_if_lex(VCDs0,InfCat0,VCDs,InfCat).


vp_argument(tree(r(vc,p(oti)),CmpInf0),
	    tree(r(vc,p(oti)),[tree(r(cmp,Cmp1),[]),
			       tree(r(body,p(ti)),[tree(r(cmp,Cmp2),[]),
						   tree(r(body,InfCat),[NewSu|VCDs])
						  ]
				   )
			      ]),
	    NewSu) :-
    lists:select(tree(r(cmp,Cmp1),[]),CmpInf0,[tree(r(body,p(ti)),Ds0)]),
    lists:select(tree(r(cmp,Cmp2),[]),Ds0,[tree(r(body,InfCat0),VCDs0)]),
    Cmp1 = adt_lex(_,om,_,_,_),
    Cmp2 = adt_lex(_,te,_,_,_),
    NotSu = tree(r(su,_),_),
    \+ lists:member(NotSu,VCDs0),
    add_hd_if_lex(VCDs0,InfCat0,VCDs,InfCat).

add_hd_if_lex([],adt_lex(Cat,A,B,C,D),[tree(r(hd,adt_lex(Cat,A,B,C,D)),[])],p(Cat)).
add_hd_if_lex([H|T],Cat,[H|T],Cat).

su_node(SuNode,NewSuNode,NewSu) :-
    (   \+ SuNode = i(_),
	\+ SuNode = i(_,_),
	NewSuNode = i(Index,SuNode),
	hdrug_util:gen_sym(Index,adt_index)
    ;   SuNode = i(Index),
	NewSuNode = i(Index)
    ;   SuNode = i(Index,SuNodeRest),
	NewSuNode = i(Index,SuNodeRest)
    ),
    NewSu = tree(r(su,i(Index)),[]).

dont_paraphrase(Tree) :-
    tree_member(Sub,Tree),
    dont_paraphrase_sub(Sub).

dont_paraphrase_sub(tree(r(_,adt_lex(_,_,_,_,Atts)),[])) :-
    lists:member(stype=topic_drop,Atts).
dont_paraphrase_sub(tree(r(_,i(_,adt_lex(_,_,_,_,Atts))),[])) :-
    lists:member(stype=topic_drop,Atts).

tree_member(Sub,Sub).
tree_member(Sub,tree(_,Ds)) :-
    lists:member(D,Ds),
    tree_member(Sub,D).

raiser_head(Ds0,Raiser) :-
    Hd = tree(r(hd,adt_lex(_,L,_M,_V,_F)),[]),
    lists:select(Hd,Ds0,Ds1),
    raiser(L),
    Vc0 = tree(r(vc,p(Inf)),_),
    lists:select(Vc0,Ds1,_),
    lists:member(Inf,[inf,ti,oti]),
    !,
    Raiser = raiser.
raiser_head(_,no_raiser).
