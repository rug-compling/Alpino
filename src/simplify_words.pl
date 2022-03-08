:- module(alpino_simplify_words, [ words_transformation/5 ]).

words_transformation(r(Rel,p(Cat0)),Ds0,r(Rel,p(Cat)),Ds,_) :-
    Hd0 = tree(r(HD,adt_lex(Cat0,Old,Old,D0,E0)),[]),
    Hd  = tree(r(HD,adt_lex(Cat,New,New,D,E)),[]),
    head_rel(HD),
    replace(Hd0,Hd,Ds0,Ds1),
    simplify(Old,New,Cat0,Cat,D0,D,E0,E,Ds1),
    adapt_det(Ds1,Ds).

%% constructief en (of, maar) positief => positief en (of, maar) positief => positief
words_transformation(r(Rel,p(conj)),Ds0,r(Rel,Cat),D,_) :-
    CRD = tree(r(crd,_),_),
    lists:select(CRD,Ds0,[tree(r(cnj,Cat),D),tree(r(cnj,Cat),D)]).

words_transformation(r('--',p(pp)),Ds0,r('--',p(du)),[Niet,PP],_) :-
    Niet0 = tree(r(mod,adt_lex(A,niet,B,C,D)),[]),
    Niet  = tree(r(dp,adt_lex(A,niet,B,C,D)),[]),
    lists:select(Niet0,Ds0,Ds1),
    PP = tree(r(dp,p(pp)),Ds1).

words_transformation(r(Rel,p(Cat)),Ds0,
		     r(Rel,p(Cat)),[Hd,VC|Ds],Up) :-
    Hd0 = tree(r(hd,adt_lex(Cat0,Old,Old,Pos,Atts)),[]),
    lists:select(Hd0,Ds0,Ds1),
    lists:member(Old,[heb,ben]),
    Hd  = tree(r(hd,adt_lex(Cat0,{[heb,ben]},{[heb,ben]},Pos,Atts)),[]),
    VC0 = tree(r(vc,p(ppart)),VC0Ds),
    lists:select(VC0,Ds1,Ds),
    subjects(Ds,VC0Ds),
    VC  = tree(r(vc,p(ppart)),_),
    hdrug_util:hdrug_flag(simplify_modifier,Mod),
    hdrug_util:hdrug_flag(simplify_words,Words),
    hdrug_util:hdrug_flag(simplify_split,Split),
    alpino_user_transformation:apply_further_adt_transformations(VC0,VC,Mod,Words,Split,Up),
    VC0 \== VC.

words_transformation(r(Rel,p(mwu(_,_))),Ds,
		     r(Rel,adt_lex(Cat,Lem,Lem,Pos,Atts)),[],_):-
    mwu(Ds,Words),
    simplify_mwu(Words,Lem,Pos,Cat,Atts).

words_transformation(r(Rel,adt_lex(_,Lem,Lem,_,_)),[],
		     r(Rel,Cat),Ds,_):-
    lemma_to_tree(Lem,Cat,Ds).

words_transformation(r(Rel,adt_lex(Cat0,Old,Old,D0,E0)),[],
		     r(Rel,adt_lex(Cat,New,New,D,E)),[],_) :-
    \+ Rel = mwp,
    \+ Rel = svp,
    simplify(Old,New,Cat0,Cat,D0,D,E0,E,[]).

words_transformation(r(Rel,Cat),Ds0,r(Rel,Cat),Ds,_) :-
    pattern_rule(Left,Right),
    match_left_pattern(Left,Ds0,Ds1),
    add_right_pattern(Right,Ds,Ds1).

%% X wordt geacht VP => X moet VP
words_transformation(r(Rel,Cat),Ds0,r(Rel,Cat),[SU,NEWHD,tree(r(vc,p(inf)),INFDS)],_) :-
    SU = tree(r(su,i(I,_)),_),
    HD = tree(r(hd,adt_lex(_,word,_,_,Atts)),[]),
    VC = tree(r(vc,p(ppart)),VCDS0),
    lists:select(SU,Ds0,Ds1),
    lists:select(HD,Ds1,Ds2),
    lists:select(VC,Ds2,[]),
    OBJ = tree(r(obj1,i(I)),[]),
    lists:select(OBJ,VCDS0,VCDS1),
    ACHT = tree(r(hd,adt_lex(_,acht,_,_,_)),[]),
    lists:select(ACHT,VCDS1,VCDS2),
    VC2 = tree(r(vc,p(ti)),TIDS),
    lists:select(VC2,VCDS2,VCDS),
    INF = tree(r(body,p(inf)),INFDS0),
    lists:select(INF,TIDS,_),
    lists:append(VCDS,INFDS0,INFDS),
    NEWHD = tree(r(hd,adt_lex(_,moet,_,verb,Atts)),[]).

%% opgemerkt zij dat X => X
words_transformation(r(Rel,p(smain)),Ds0,r(Rel,p(smain)),BODYDS,_) :-
    HD = tree(r(hd,adt_lex(smain,ben,_,verb,Atts)),[]),
    lists:select(HD,Ds0,[VC]),
    lists:member(tense=subjunctive,Atts),
    lists:member(stype=declarative,Atts),
    VC = tree(r(vc,p(ppart)),VCDS0),
    MERK = tree(r(hd,adt_lex(ppart,merk_op,_,_,_)),[]),
    lists:select(MERK,VCDS0,[CP]),
    CP = tree(r(vc,p(cp)),CPDS0),
    DAT = tree(r(cmp,adt_lex(_,dat,_,_,_)),[]),
    lists:select(DAT,CPDS0,[BODY]),
    BODY = tree(r(body,p(ssub)),BODYDS0),
    A0 = tree(r(hd,adt_lex(_,ARR1,ARR2,ARR3,ARR4)),[]),
    A1 = tree(r(hd,adt_lex(smain,ARR1,ARR2,ARR3,ARR4)),[]),
    replace(A0,A1,BODYDS0,BODYDS).

%% er zij op/aan gewezen/herinnerd dat X => X  
words_transformation(r(Rel,p(smain)),Ds0,r(Rel,p(smain)),BODYDS,_) :-
    HD = tree(r(hd,adt_lex(smain,ben,_,verb,Atts)),[]),
    lists:select(HD,Ds0,[VC]),
    lists:member(tense=subjunctive,Atts),
    lists:member(stype=declarative,Atts),
    VC = tree(r(vc,p(ppart)),VCDS0),
    MERK = tree(r(hd,adt_lex(ppart,Wijs,_,_,_)),[]),
    lists:select(MERK,VCDS0,[PP]),
    (   OPL = op, Wijs = wijs
    ;   OPL = aan, Wijs = herinner
    ),
    PP = tree(r(pc,p(pp)),PPDS0),
    (   OP = tree(r(hd,adt_lex(_,OPL,_,_,_)),[]),
	lists:select(OP,PPDS0,PPDS1),
	POP = tree(r(pobj1,_),_),
	lists:select(POP,PPDS1,[CP])
    ;   EROP = tree(r(hd,adt_lex(_,EROPL,_,_,_)),[]),
	lists:select(EROP,PPDS0,[CP]),
	erop(EROPL,OPL)
    ),
    CP = tree(r(vc,p(cp)),CPDS0),
    DAT = tree(r(cmp,adt_lex(_,dat,_,_,_)),[]),
    lists:select(DAT,CPDS0,[BODY]),
    BODY = tree(r(body,p(ssub)),BODYDS0),
    A0 = tree(r(hd,adt_lex(_,ARR1,ARR2,ARR3,ARR4)),[]),
    A1 = tree(r(hd,adt_lex(smain,ARR1,ARR2,ARR3,ARR4)),[]),
    replace(A0,A1,BODYDS0,BODYDS).

subjects(List0,List) :-
    S1 = tree(r(su,L1),_),
    lists:member(S1,List0),
    S = tree(r(su,L),_),
    lists:member(S,List),
    id(L1,L).

id(i(N),i(N)).
id(i(N,_),i(N)).
id(i(N),i(N,_)).
    

head_rel(hd).
head_rel(cmp).

mwu([],[]).
mwu([tree(r(mwp,adt_lex(_,W,_,_,_)),[])|Trees],[W|Ws]) :-
    mwu(Trees,Ws).

%%% uiterst ADJ => heel ADJ
pattern_rule([mod=l(uiterst,adj,_),
	      hd=l(ADJ,adj,Atts)
	     ],
	     [mod=l(heel,adv,[]),
	      hd=l(ADJ,adj,Atts)
	     ]).


%%% het verheugt X dat .. => X is blij dat ..
pattern_rule([sup=l(het,_,_),
	      hd=l(verheug,verb,Atts),
	      obj2=OBJ,
	      su=dt(cp,CP)
	     ],
	     [su=OBJ,
	      hd=l(ben,verb,Atts),
	      predc=dt(ap,[hd=l(blij,adj,[aform=base]),
			   vc=dt(cp,CP)
			  ])
	     ]).

%% ik ben van oordeel => ik vind
pattern_rule([hd=l(ben,Pos,Atts),
	      svp=mwu([van,oordeel])
	     ],
	     [hd=l(vind,Pos,Atts)
	     ]).

%% ten goede komen aan NP => NP helpen
pattern_rule([hd=l(kom,Pos,Atts),
	      svp=mwu([ten,goede]),
	      obj2=dt(pp,[hd=l(aan,_,_),
			  obj1=OBJ])
	     ],
	     [hd=l(help,Pos,Atts),
	      obj1=OBJ
	     ]).

%% NP ten goede komen => helpen
pattern_rule([hd=l(kom,Pos,Atts),
	      svp=mwu([ten,goede]),
	      obj2=OBJ
	     ],
	     [hd=l(help,Pos,Atts),
	      obj1=OBJ
	     ]) :-
    np(OBJ).

%% in het vooruitzicht stellen => aankondigen
pattern_rule([hd=l(stel,Pos,Atts),
	      ld=dt(pp,[hd=l(in,_,_),
			obj1=dt(np,[hd=l(vooruitzicht,_,_),
				    det=l(het,_,_)
				   ])])
	     ],
	     [hd=l(kondig_aan,Pos,Atts)
	     ]
	    ).

%% op het punt staan te => gaan
%% todo: VC should not itself be headed by "ga"
%% ik sta op het punt naar buiten te gaan
%% ==> ik ga naar buiten gaan
pattern_rule([hd=l(sta,Pos,Atts),
	      vc=dt(ti,[cmp=l(te,_,_),
			body=VC]),
	      svp=mwu([op,het,punt])
	     ],
	     [hd=l(ga,Pos,Atts),
	      vc=VC
	     ]).

%% op het punt staan om te => gaan
pattern_rule([hd=l(sta,Pos,Atts),
	      vc=dt(oti,[cmp=l(om,_,_),
			 body=dt(ti,[cmp=l(te,_,_),
				     body=VC])]),
	      svp=mwu([op,het,punt])
	     ],
	     [hd=l(ga,Pos,Atts),
	      vc=VC
	     ]).

match_left_pattern([],Ds,Ds).
match_left_pattern([Pat|Pats],Ds0,Ds) :-
    match_left_pattern_tree(Pat,Ds0,Ds1),
    match_left_pattern(Pats,Ds1,Ds).

match_left_pattern_tree(Rel=Node,Ds0,Ds) :-
    lists:select(tree(r(Rel,NodeCat),NodeDs),Ds0,Ds),
    match_left_pattern_node(Node,NodeCat,NodeDs).

match_left_pattern_node(Var,Cat,Ds):-
    var(Var),
    !,
    Var = Cat/Ds.
match_left_pattern_node(Cat/Ds,p(Cat),Ds).
match_left_pattern_node(dt(Cat,Pattern),p(Cat),Ds) :-
    match_left_pattern(Pattern,Ds,[]).
    
match_left_pattern_node(l(Lem,Pos,Atts0),adt_lex(_,Lem,_,Pos,Atts),[]):-
    match_atts(Atts0,Atts).
match_left_pattern_node(mwu(List),p(mwu(_,_)),Ds) :-
    mwu(Ds,List).

match_atts(Var,Var2) :-
    var(Var), !,
    Var=Var2.
match_atts([],_).
match_atts([Att|Atts],List) :-
    lists:member(Att,List),
    match_atts(Atts,List).

add_right_pattern([],Ds,Ds).
add_right_pattern([H|T],Ds0,Ds) :-
    add_right_pattern_tree(H,Ds0,Ds1),
    add_right_pattern(T,Ds1,Ds).

add_right_pattern_tree(Rel=Node,[tree(r(Rel,NewNode),NewDs)|Ds],Ds) :-
    add_right_pattern_node(Node,NewNode,NewDs).

add_right_pattern_node(Cat/Ds,Cat,Ds).
add_right_pattern_node(dt(Cat,Ds0),p(Cat),Ds) :-
    add_right_pattern(Ds0,Ds,[]).
add_right_pattern_node(l(Lem,Pos,Atts),adt_lex(_,Lem,_,Pos,Atts),[]).

%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).

simplify_mwu([a,posterio],achteraf,adv,advp,[]).
simplify_mwu([a,posteriori],achteraf,adv,advp,[]).
simplify_mwu([a,priori],vooraf,adv,advp,[]).
simplify_mwu([ad,hoc],{[direct,tijdelijk]},adj,ap,[]).
simplify_mwu([aan,de,hand,van],{[met,door]},prep,pp,[]).
simplify_mwu([acquis,communautaire],gemeenschap_recht,noun,np,[]).
simplify_mwu([als,gevolg,van],door,prep,pp,[]).
simplify_mwu([te,allen,tijde],altijd,adv,advp,[]).
simplify_mwu([te,zijner,tijd],laat,adj,ap,[aform=compar]).
simplify_mwu([ten,behoeve,van],voor,prep,pp,[]).
simplify_mwu([ten,dele],gedeeltelijk,adj,ap,[]).
simplify_mwu([ten,gevolge,van],door,prep,pp,[]).
simplify_mwu([ten,tijde,van],tijdens,prep,pp,[]).
simplify_mwu([ten,volle],helemaal,adv,advp,[]).

lemma_to_tree(abusievelijk, p(pp),
	      [tree(r(hd,adt_lex(pp,per,per,prep,[])),[]),
	       tree(r(obj1,adt_lex(np,ongeluk,ongeluk,noun,[])),[])]).

lemma_to_tree(Lem,Cat,Tree) :-
    lemma_to_stree(Lem,Cat,STree),
    stree_ds(STree,Tree).

stree(Lem,adt_lex(_,Lem,Lem,_,[]),[]) :-
    atomic(Lem),
    !.

stree(STree,Cat,Ds):-
    STree =.. [Cat|StreeDs],
    stree_ds(StreeDs,Ds).

stree_ds([],[]).
stree_ds([Rel=Stree|StreeDs],[tree(r(Rel,Cat),N)|Ds]):-
    stree(Stree,Cat,N),
    stree_ds(StreeDs,Ds).

lemma_to_stree(anderzijds,p(pp),[hd=aan,obj1=np(det=de,mod=ander,hd=kant)]).

/* fun, but almost never useful

%% NAVO-interventie --> interventie
simplify(Compound,Atom,Cat,Cat,noun,noun,E,E) :-
    atom(Compound),
    alpino_util:split_atom(Compound,"_",[Left,Atom]),
    \+ Atom = 'DIM',
    \+ (   hdrug_util:concat_all([Left,Atom],LeftAtom,''),
	   alpino_lex:lexicon(_,_,[LeftAtom],[],normal)
       ),
    \+ (   hdrug_util:concat_all([Left,Atom],LeftAtom,'-'),
	   alpino_lex:lexicon(_,_,[LeftAtom],[],normal)
       ).
*/

simplify(Ik,{List},np,np,pron,pron,Atts,Atts,[]) :-
    eq_pronoun(List,Atts),
    lists:member(Ik,List).

simplify(Ik,{List},np,np,pron,pron,Atts,Atts,[]) :-
    eq_pronoun(Ik,List,Atts).


simplify(aandachtig,goed,Cat,Cat,D,D,E,E,_).
simplify(aangaande,over,Cat,Cat,D,D,E,E,_).
simplify(aangezien,omdat,Cat,Cat,D,D,E,E,_).
simplify(aanmerkelijk,groot,Cat,Cat,D,D,E,E,_).
simplify(aanstonds,zo,Cat,Cat,D,D,E,E,_).
simplify(aanvang,begin,Cat,Cat,D,D,E,E,_).
%%simplify(aanvankelijk,{[eerst,eerder]},Cat,Cat,D,D,E,E,_).   %TODO para Aanvankelijk een overeenkomst voor drie jaar en vervolgens om de twee jaar verlengde overeenkomsten .
simplify(aanzienlijk,groot,Cat,Cat,D,D,E,E,_).
simplify(abuis,fout,Cat,Cat,D,D,E,E,_).
simplify(acceptatie,goedkeuring,Cat,Cat,D,D,E,E,_).
simplify(accomodatie,{[gebouw,locatie]},Cat,Cat,D,D,E,E,_).
simplify(accordeer,keur_goed,Cat,Cat,D,D,E,E,_).
simplify(acht,{[vind,acht]},Cat,Cat,verb,verb,E,E,_).
simplify(actualiseer,{[pas_aan,moderniseer,werk_bij]},Cat,Cat,D,D,E,E,_).
simplify(acuut,direct,Cat,Cat,D,D,E,E,_).
simplify(additioneel,voeg_toe,ap,ppart,D,D,E,E,_).
simplify(adequaat,juist,Cat,Cat,D,D,E,E,_).
simplify(adhesie,steun,Cat,Cat,D,D,E,E,_).
simplify(adstrueer,leg_uit,Cat,Cat,D,D,E,E,_).
simplify(affirmatief,bevestig,ap,ppres,D,D,E,E,_).
simplify(ageer,treed_op,Cat,Cat,D,D,E,E,_).
simplify(aldaar,daar,Cat,Cat,D,D,E,E,_).
simplify(aldus,zo,advp,advp,D,D,E,E,_).
simplify(aldus,volgens,pp,pp,D,D,E,E,_).
simplify(alloceer,wijs_toe,Cat,Cat,D,D,E,E,_).
simplify(alom,overal,Cat,Cat,D,D,E,E,_).
simplify(alternatief,ander,ap,ap,D,D,_,[aform=compar],_).
simplify(alvorens,voordat,Cat,Cat,D,D,E,E,_).
simplify(ambivalent,dubbel,Cat,Cat,D,D,E,E,_).
simplify(amendement,wijziging,Cat,Cat,D,D,E,E,_).
simplify(amotie,sloop,Cat,Cat,D,D,E,E,_).
simplify(amoveer,sloop,Cat,Cat,D,D,E,E,_).
simplify(ampel,uitvoerig,Cat,Cat,D,D,E,E,_).
simplify(andermaal,weer,Cat,Cat,D,D,E,E,_).
simplify(anderszins,anders,Cat,Cat,D,D,E,E,_).
simplify(annonce,aankondiging,Cat,Cat,D,D,E,E,_).
simplify(apocrief,twijfelachtig,Cat,Cat,D,D,E,E,_).
simplify(archaïsch,ouderwets,Cat,Cat,D,D,E,E,_).
simplify(authentiek,echt,Cat,Cat,D,D,E,E,_).
simplify(autonoom,zelfstandig,Cat,Cat,D,D,E,E,_).
simplify(behels,houd_in,Cat,Cat,D,D,E,E,_).
simplify(behoedzaam,voorzichtig,Cat,Cat,D,D,E,E,_).
simplify(bemachtig,krijg,Cat,Cat,D,D,E,E,_).
simplify(bemiddeling_procedure,bemiddeling,Cat,Cat,D,D,E,E,_).
simplify(bijgevolg,daarom,advp,pp,adv,prep,_,[],_).
simplify(blijkens,volgens,Cat,Cat,D,D,E,E,_).
simplify(bonafide,betrouwbaar,Cat,Cat,D,D,E,E,_).	% oh brother where art thou
simplify(catastrofe,ramp,Cat,Cat,D,D,E,E,_).
simplify(cineast,filmmaker,Cat,Cat,D,D,E,E,_).
simplify(coherentie,samenhang,Cat,Cat,D,D,E,E,_).
simplify(communiceer,praat,Cat,Cat,D,D,E,E,_).
simplify(compliceren,moeilijk,ppart,ap,adj,adj,E,E,_).
simplify(concretiseer,verduidelijk,Cat,Cat,D,D,E,E,_).
simplify(concretisering,verduidelijking,Cat,Cat,D,D,E,E,_).
simplify(confirmeer,bevestig,Cat,Cat,D,D,E,E,_).
simplify(conform,volgens,Cat,Cat,D,D,E,E,_).
simplify(constructief,positief,Cat,Cat,D,D,E,E,_).
simplify(consulteer,raadpleeg,Cat,Cat,D,D,E,E,_).
simplify(continueer,zet_voort,Cat,Cat,D,D,E,E,_).
simplify(continuïteit,voortgang,Cat,Cat,D,D,E,E,_).
simplify(continu,steeds,Cat,Cat,D,D,E,E,_).
simplify(controversieel,omstreden,Cat,Cat,D,D,E,E,_).
simplify(convenant,overeenkomst,Cat,Cat,D,D,E,E,_).
simplify(coördinatie,afstemming,Cat,Cat,D,D,E,E,_).
simplify(cultus,verering,Cat,Cat,D,D,E,E,_).
simplify(daadwerkelijk,echt,Cat,Cat,D,D,E,E,_).
simplify(daar,omdat,Cat,Cat,comp,comp,E,E,_).
simplify(decadent,smakeloos,Cat,Cat,D,D,E,E,_).
simplify(declameer,draag_voor,Cat,Cat,D,D,E,E,_).
simplify(deel_mede,zeg,Cat,Cat,D,D,E,E,_).
simplify(delicaat,gevoelig,Cat,Cat,D,D,E,E,_).
simplify(derhalve,dus,Cat,Cat,D,D,E,E,_).
simplify(derving,verlies,Cat,Cat,D,D,E,E,_).
simplify(destijds,toen,Cat,Cat,D,D,E,E,_).
simplify(diffuus,onduidelijk,Cat,Cat,D,D,E,E,_).
simplify(discrepantie,verschil,Cat,Cat,D,D,E,E,_).
simplify(discutabel,twijfelachtig,Cat,Cat,D,D,E,E,_).
simplify(dissertatie,proefschrift,Cat,Cat,D,D,E,E,_).
simplify(dogmatisch,star,Cat,Cat,D,D,E,E,_).
simplify(dominant,overheersen,ap,VAR,D,D,E,E,_) :- % ppres or ap, depending on dominant/dominanter
    when(nonvar(VAR),( VAR=ap; VAR=ppres )).
simplify(donatie,schenking,Cat,Cat,D,D,E,E,_).
simplify(doneer,schenk,Cat,Cat,D,D,E,E,_).
simplify(doorgaans,meestal,Cat,Cat,D,D,E,E,_).
simplify(dotatie,schenking,Cat,Cat,D,D,E,E,_).
simplify(draconisch,streng,Cat,Cat,D,D,E,E,_).
simplify(dubieus,twijfelachtig,Cat,Cat,D,D,E,E,_).
simplify(dupeer,benadeel,Cat,Cat,D,D,E,E,_).
simplify(echter,maar,du,du,D,D,E,E,_).
simplify(eenvoudig,simpel,Cat,Cat,D,D,E,E,_).
simplify(eminent,munt_uit,Cat,Cat,D,D,E,E,_).
simplify(enerverend,spannend,Cat,Cat,D,D,E,E,_).
simplify(enigma,raadsel,Cat,Cat,D,D,E,E,_).
simplify(epiloog,slotwoord,Cat,Cat,D,D,E,E,_).
simplify(essentieel,belangrijk,Cat,Cat,D,D,E,E,_).
simplify(etisch,moreel,Cat,Cat,D,D,E,E,_).
simplify(evident,duidelijk,Cat,Cat,D,D,E,E,_).
simplify(excessief,overdreven,Cat,Cat,D,D,E,E,_).
simplify(exercitie,oefening,Cat,Cat,D,D,E,E,_).
simplify(exhaustief,compleet,Cat,Cat,D,D,E,E,_).
simplify(exorbitant,overdreven,Cat,Cat,D,D,E,E,_).
simplify(expertise,kennis,Cat,Cat,D,D,E,E,_).
simplify(explicatie,uitleg,Cat,Cat,D,D,E,E,_).
simplify(exploitatie,gebruik,Cat,Cat,D,D,E,E,_).
simplify(exporteer,voer_uit,Cat,Cat,D,D,E,E,_).
simplify(fair,eerlijk,Cat,Cat,D,D,E,E,_).
simplify(falsificatie,vervalsing,Cat,Cat,D,D,E,E,_).
simplify(fiatteer,keur_goed,Cat,Cat,D,D,E,E,_).
simplify(filantroop,weldoener,Cat,Cat,D,D,E,E,_).
simplify(florissant,gunstig,Cat,Cat,D,D,E,E,_).
simplify(fluctueer,schommel,Cat,Cat,D,D,E,E,_).
simplify(frictie,wrijving,Cat,Cat,D,D,E,E,_).
simplify(furieus,boos,Cat,Cat,D,D,E,E,_).
simplify(futiliteit,kleinigheid,Cat,Cat,D,D,E,E,_).
simplify(gaarne,graag,advp,ap,adv,adj,E,E,_).
simplify(geenszins,niet,Cat,Cat,D,D,E,E,_).
simplify(gedurende,tijdens,Cat,Cat,D,D,E,E,_).
simplify(geëquipeerd,toegerust,Cat,Cat,D,D,E,E,_).
simplify(gemeenzaam,alledaags,Cat,Cat,D,D,E,E,_).
simplify(gepikeerd,boos,Cat,Cat,D,D,E,E,_).
simplify(gering,klein,Cat,Cat,D,D,E,E,_).
simplify(geschil,ruzie,Cat,Cat,D,D,E,E,_).
simplify(governance,bestuur,Cat,Cat,D,D,E,E,_).
simplify(gracieus,elegant,Cat,Cat,D,D,E,E,_).
simplify(hectiek,drukte,Cat,Cat,D,D,E,E,_).
simplify(hectisch,druk,Cat,Cat,D,D,E,E,_).
simplify(heden,nu,advp,advp,D,D,E,E,_).
simplify(hermetisch,helemaal,Cat,Cat,D,D,E,E,_).
simplify(heterogeen,mengen,ap,ppart,D,D,E,E,_).
simplify(hiërarchie,rangorde,Cat,Cat,D,D,E,E,_).
simplify(hypothese,aanname,Cat,Cat,D,D,E,E,_).
simplify(immer,altijd,Cat,Cat,D,D,E,E,_).
simplify(implementeer,voer_in,Cat,Cat,D,D,E,E,_).
simplify(impliceer,beteken,Cat,Cat,D,D,E,E,_).
simplify(implicatie,gevolg,Cat,Cat,D,D,E,E,_).
simplify(importantie,belang,Cat,Cat,D,D,E,E,_).
simplify(importeer,voer_in,Cat,Cat,D,D,E,E,_).
simplify(incidenteel,eenmalig,Cat,Cat,D,D,E,E,_).
simplify(incrimineer,beschuldig,Cat,Cat,D,D,E,E,_).
simplify(indicatie,aanwijzing,Cat,Cat,D,D,E,E,_).
simplify(initieel,één,ap,ap,adj,adj,E,E,_).
simplify(initieer,begin,Cat,Cat,D,D,E,E,_).
simplify(insinueer,suggereer,Cat,Cat,D,D,E,E,_).
simplify(integraal,volledig,Cat,Cat,D,D,E,E,_).
simplify(intensief,grondig,Cat,Cat,D,D,E,E,_).
simplify(intensiveer,versterk,Cat,Cat,D,D,E,E,_).
simplify(intensivering,verdieping,Cat,Cat,D,D,E,E,_).
simplify(introvert,verlegen,Cat,Cat,D,D,E,E,_).
simplify(inzake,over,Cat,Cat,D,D,E,E,_).
simplify(laveloos,dronken,Cat,Cat,D,D,E,E,_).
simplify(legitiem,wettig,Cat,Cat,D,D,E,E,_).
simplify(linguïstiek,taalkunde,Cat,Cat,D,D,E,E,_).
simplify(linguïstisch,taalkundig,Cat,Cat,D,D,E,E,_).
simplify(linguïst,taalkundige,Cat,Cat,D,D,E,E,_).
simplify(louter,alleen,Cat,Cat,D,D,E,E,_).
simplify(ludiek,speels,Cat,Cat,D,D,E,E,_).
simplify(marginaal,klein,Cat,Cat,D,D,E,E,_).
simplify(minutieus,precies,Cat,Cat,D,D,E,E,_).
simplify(mitigeer,zwak_af,Cat,Cat,D,D,E,E,_).
simplify(moedwillig,expres,Cat,Cat,D,D,E,E,_).
simplify(momenteel,nu,Cat,Cat,D,D,E,E,_).
simplify(mutatie,wijziging,Cat,Cat,D,D,E,E,_).
simplify(nadien,daarna,Cat,Cat,D,D,E,E,_).
simplify(nauwgezet,precies,Cat,Cat,D,D,E,E,_).
simplify(nietemin,toch,Cat,Cat,D,D,E,E,_).
simplify(nimmer,nooit,Cat,Cat,D,D,E,E,_).
simplify(nochtans,toch,Cat,Cat,D,D,E,E,_).
%%%simplify(noodzakelijk,nodig,Cat,Cat,D,D,E,E,_). subject_sbar(_nohet)  mismatch
%%%simplify(noop,dwing,Cat,Cat,D,D,E,E,_).  TODO obj2 <-> obj1 mismatch
simplify(notificatie,melding,Cat,Cat,D,D,E,E,_).
simplify(omineus,onheilspellend,Cat,Cat,D,D,E,E,_).
simplify(omtrent,over,Cat,Cat,D,D,E,E,_).
simplify(onberispelijk,keurig,Cat,Cat,D,D,E,E,_).
simplify(onjuist,fout,Cat,Cat,D,D,E,E,_).
simplify(ontbeer,mis,Cat,Cat,D,D,E,E,_).
simplify(ontdaan,overstuur,Cat,Cat,D,D,E,E,_).
simplify(onverwijld,onmiddellijk,Cat,Cat,D,D,E,E,_).
simplify(onvolkomenheid,fout,Cat,Cat,D,D,E,E,_).
simplify(opgetogen,blij,Cat,Cat,D,D,E,E,_).
simplify(opteer,kies,Cat,Cat,D,D,E,E,_).
simplify(optimaliseer,verbeter,Cat,Cat,D,D,E,E,_).
simplify(overeenkomstig,volgens,Cat,Cat,D,D,E,E,_).
simplify(paraaf,handtekening,Cat,Cat,D,D,E,E,_).
simplify(participatie,deelname,Cat,Cat,D,D,E,E,_).
simplify(partieel,gedeeltelijk,Cat,Cat,D,D,E,E,_).
simplify(pendule,klok,Cat,Cat,D,D,E,E,_).
simplify(percipieer,neem_waar,Cat,Cat,D,D,E,E,_).
simplify(perspectief,zicht,Cat,Cat,D,D,E,E,_) :-
    lists:member(rnum=sg,E).
simplify(pertinent,echt,Cat,Cat,D,D,E,E,_).
simplify(plausibel,redelijk,Cat,Cat,D,D,E,E,_).
simplify(poog,probeer,Cat,Cat,D,D,E,E,_).
simplify(potentieel,mogelijk,Cat,Cat,adj,adj,E,E,_).
simplify(potentieel,mogelijkheid,Cat,Cat,noun,noun,E,E,_).
simplify(prerogatief,voorrecht,Cat,Cat,D,D,E,E,_).
simplify(prioritair,belangrijk,Cat,Cat,D,D,E,E,_).
simplify(procedure,werkwijze,Cat,Cat,D,D,E,E,_).
simplify(prolongatie,verlenging,Cat,Cat,D,D,E,E,_).
simplify(prominent,belangrijk,Cat,Cat,D,D,E,E,_).
simplify(quarantaine,afzondering,Cat,Cat,D,D,E,E,_).
simplify(rationeel,verstandig,Cat,Cat,D,D,E,E,_).
simplify(recapituleer,vat_samen,Cat,Cat,D,D,E,E,_).
simplify(reduceer,verminder,Cat,Cat,D,D,E,E,_).
simplify(reductie,beperking,Cat,Cat,D,D,E,E,_).
simplify(reeds,al,Cat,Cat,D,D,E,E,_).
simplify(refereer,verwijs,Cat,Cat,D,D,E,E,_).
simplify(referentie,verwijzing,Cat,Cat,D,D,E,E,_).
simplify(relevant,belangrijk,Cat,Cat,D,D,E,E,_).
simplify(reprimande,waarschuwing,Cat,Cat,D,D,E,E,_).
simplify(respons,antwoord,Cat,Cat,D,D,E,E,_).
simplify(ressentiment,wrok,Cat,Cat,D,D,E,E,_).
simplify(restrictie,beperking,Cat,Cat,D,D,E,E,_).
simplify(resumé,samenvatting,Cat,Cat,D,D,E,E,_).
simplify(resumeer,vat_samen,Cat,Cat,D,D,E,E,_).
simplify(retourneer,stuur_terug,Cat,Cat,D,D,E,E,_).
simplify(ridicuul,belachelijk,Cat,Cat,D,D,E,E,_).
simplify(robuust,stevig,Cat,Cat,D,D,E,E,_).
simplify(sanctie,maatregel,Cat,Cat,D,D,E,E,_).
simplify(secularisatie,ontkerkelijking,Cat,Cat,D,D,E,E,_).
simplify(secuur,precies,Cat,Cat,D,D,E,E,_).
simplify(sedert,sinds,Cat,Cat,D,D,E,E,_).
simplify(sereen,vredig,Cat,Cat,D,D,E,E,_).
simplify(sommeer,beveel,Cat,Cat,D,D,E,E,_).
simplify(speculaar,gok,Cat,Cat,D,D,E,E,_).
simplify(sporadisch,soms,Cat,Cat,D,D,E,E,_).
simplify(staatshoofd,koning,Cat,Cat,D,D,E,E,_).
simplify(stagnatie,stilstand,Cat,Cat,D,D,E,E,_).
simplify(stakeholder,betrekken,Cat,Cat,D,D,E,[personalized=true,aform=base|E],_).
simplify(start_op,begin,Cat,Cat,D,D,E,E,_).
simplify(stoïcijns,onverstoorbaar,Cat,Cat,D,D,E,E,_).
simplify(strategie,plan,Cat,Cat,D,D,E,E,_).
simplify(stringent,streng,Cat,Cat,D,D,E,E,_).
simplify(stuur_aan,{[leid,stuur]},Cat,Cat,D,D,E,E,_).
simplify(substantieel,flink,Cat,Cat,D,D,E,E,_).
simplify(substituut,vervanging,Cat,Cat,D,D,E,E,_).
simplify(summier,kort,Cat,Cat,D,D,E,E,_).
simplify(target,doel,Cat,Cat,D,D,E,E,_).
simplify(taskforce,werk_groep,Cat,Cat,D,D,E,E,_).
simplify(teneinde,om,cp,oti,D,D,E,E,_).
simplify(tenuitvoerlegging,uitvoering,Cat,Cat,D,D,E,E,_).
simplify(terstond,onmiddellijk,Cat,Cat,D,D,E,E,_).
simplify(tevens,ook,Cat,Cat,D,D,E,E,_).
simplify(transparantie,duidelijkheid,Cat,Cat,D,D,E,E,_).
simplify(tref_aan,vind,Cat,Cat,D,D,E,E,_).
simplify(triviaal,gewoon,Cat,Cat,D,D,E,E,_).
simplify(uitfasering,stop,Cat,Cat,D,D,E,E,_).
simplify(universeel,algemeen,Cat,Cat,D,D,E,E,_).
simplify('up-to-date',actueel,Cat,Cat,D,D,E,E,_).
simplify(urgent,dringend,Cat,Cat,D,D,E,E,_).
simplify(urgentie,haast,Cat,Cat,D,D,E,E,_).
simplify(vacant,vrij,Cat,Cat,D,D,E,E,_).
simplify(verifieer,controleer,Cat,Cat,D,D,E,E,_).
simplify(verklaar,zeg,Cat,Cat,D,D,E,E,Ds) :-
    lists:member(tree(r(vc,_),_),Ds).
simplify(volgaarne,graag,Cat,Cat,D,D,E,E,_).
simplify(voorts,ook,Cat,Cat,D,D,E,E,_).
% simplify(vorder,eis,Cat,Cat,D,D,E,E,_).  alleen als er obj1 is? 
simplify(wederrechtelijk,onwettig,Cat,Cat,D,D,E,E,_).
simplify(weifel,aarzel,Cat,Cat,D,D,E,E,_).
simplify(wend_aan,gebruik,Cat,Cat,D,D,E,E,_).
simplify(wetgeving_resolutie,resolutie,Cat,Cat,D,D,E,E,_).

adapt_det(List0,List):-
    Det0 = tree(r(det,adt_lex(Cat0,Old,Old,Pos,Atts)),[]),
    Det  = tree(r(det,adt_lex(Cat0,New,New,Pos,Atts)),[]),
    replace(Det0,Det,List0,List1),
    simplify_det(Old,New),
    !,
    List = List1.
adapt_det(List,List).

simplify_det(dat,{[die,dat]}).
simplify_det(de,{[de,het]}).
simplify_det(deze,{[deze,dit]}).
simplify_det(die,{[die,dat]}).
simplify_det(dit,{[deze,dit]}).
simplify_det(het,{[de,het]}).
simplify_det(hetzelfde,{[dezelfde,hetzelfde]}).
simplify_det(dezelfde,{[dezelfde,hetzelfde]}).
simplify_det(diezelfde,{[datzelfde,diezelfde]}).
simplify_det(datzelfde,{[datzelfde,diezelfde]}).
simplify_det(ditzelfde,{[ditzelfde,diezelfde]}).


np(X) :-
    np0(X).
np(dt(conj,[cnj=NP|_])):-
    np0(NP).

np0(dt(np,_)).
np0(l(_,noun,_)).

eq_pronoun([ik,me,mij],_).
eq_pronoun([je,jij,jou],_).
eq_pronoun([hij,hem],_).
eq_pronoun([zij,haar],Atts) :-
    lists:member(rnum=sg,Atts).
eq_pronoun([wij,we,ons],_).
eq_pronoun([zij,ze,hun],Atts) :-
    lists:member(rnum=pl,Atts).

eq_pronoun([ikzelf,mezelf,mijzelf],_).
eq_pronoun([jezelf,jijzelf,jouzelf],_).
eq_pronoun([hijzelf,hemzelf],_).
eq_pronoun([zijzelf,haarzelf],Atts) :-
    lists:member(rnum=sg,Atts).
eq_pronoun([wijzelf,onszelf],_).
eq_pronoun([zijzelf,hunzelf],Atts) :-
    lists:member(rnum=pl,Atts).

eq_pronoun(ze,[zij,haar],Atts) :-
    lists:member(rnum=sg,Atts).
eq_pronoun(hen,[zij,ze,hun],Atts) :-
    lists:member(rnum=pl,Atts).

eq_pronoun(henzelf,[zijzelf,hunzelf],Atts) :-
    lists:member(rnum=pl,Atts).

erop(EROP,OP) :-
    atom_concat(er,OP,EROP).
erop(EROP,OP) :-
    atom_concat(daar,OP,EROP).
erop(EROP,OP) :-
    atom_concat(hier,OP,EROP).
erop(EROP,OP) :-
    atom_concat(waar,OP,EROP).

