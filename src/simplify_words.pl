:- module(alpino_simplify_words, [ apply_words_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%

apply_words_transformations(tree(Cat0,Ds0),tree(Cat,Ds)) :-
    words_transformation(Cat0,Ds0,Cat1,Ds1),
    !,
    apply_words_transformations(tree(Cat1,Ds1),tree(Cat,Ds)).
apply_words_transformations(tree(Cat,Ds0),tree(Cat,Ds)) :-
    apply_words_transformations_list(Ds0,Ds).

apply_words_transformations_list([],[]).
apply_words_transformations_list([H0|T0],[H|T]) :-
    apply_words_transformations(H0,H),
    apply_words_transformations_list(T0,T).

words_transformation(r(Rel,VAR),A,r(Rel2,i(X,Cat2)),C) :-
    nonvar(VAR),
    VAR = i(X,Cat),
    words_transformation(r(Rel,Cat),A,r(Rel2,Cat2),C).

words_transformation(r(Rel,p(Cat0)),Ds0,r(Rel,p(Cat)),[Hd|Ds]) :-
    Hd0 = tree(r(HD,adt_lex(Cat0,Old,Old,D0,E0)),[]),
    Hd  = tree(r(HD,adt_lex(Cat,New,New,D,E)),[]),
    head_rel(HD),
    lists:select(Hd0,Ds0,Ds1),
    simplify(Old,New,Cat0,Cat,D0,D,E0,E),
    adapt_det(Ds1,Ds).

words_transformation(r(Rel,p(Cat)),Ds0,
		     r(Rel,p(Cat)),[Hd,VC|Ds]) :-
    Hd0 = tree(r(hd,adt_lex(Cat0,Old,Old,Pos,Atts)),[]),
    lists:select(Hd0,Ds0,Ds1),
    lists:member(Old,[heb,ben]),
    Hd  = tree(r(hd,adt_lex(Cat0,{[heb,ben]},{[heb,ben]},Pos,Atts)),[]),
    VC0 = tree(r(vc,p(ppart)),VC0Ds),
    lists:select(VC0,Ds1,Ds),
    subjects(Ds,VC0Ds),
    VC  = tree(r(vc,p(ppart)),_),
    apply_words_transformations(VC0,VC),
    VC0 \== VC.

words_transformation(r(Rel,p(mwu(_,_))),Ds,
		     r(Rel,adt_lex(Cat,Lem,Lem,Pos,Atts)),[]):-
    mwu(Ds,Words),
    simplify_mwu(Words,Lem,Pos,Cat,Atts).

words_transformation(r(Rel,adt_lex(_,Lem,Lem,_,_)),[],
		     r(Rel,Cat),Ds):-
    lemma_to_tree(Lem,Cat,Ds).

words_transformation(r(Rel,adt_lex(Cat0,Old,Old,D0,E0)),[],
		     r(Rel,adt_lex(Cat,New,New,D,E)),[]) :-
    \+ Rel = mwp,
    simplify(Old,New,Cat0,Cat,D0,D,E0,E).

words_transformation(r(Rel,Cat),Ds0,r(Rel,Cat),Ds) :-
    pattern_rule(Left,Right),
    match_left_pattern(Left,Ds0,Ds1),
    add_right_pattern(Right,Ds1,Ds).

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

%%% TODO:
%%% het is zo dat X => X


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

%% ten goede komen => helpen
pattern_rule([hd=l(kom,Pos,Atts),
	      svp=mwu([ten,goede]),
	      obj2=OBJ
	     ],
	     [hd=l(help,Pos,Atts),
	      obj1=OBJ
	     ]).

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

match_atts(Var,_) :-
    var(Var), !.
match_atts([],_).
match_atts([Att|Atts],List) :-
    lists:member(Att,List),
    match_atts(Atts,List).

add_right_pattern([],Ds,Ds).
add_right_pattern([H|T],Ds0,Ds) :-
    add_right_pattern_tree(H,Ds0,Ds1),
    add_right_pattern(T,Ds1,Ds).

add_right_pattern_tree(Rel=Node,Ds,[tree(r(Rel,NewNode),NewDs)|Ds]) :-
    add_right_pattern_node(Node,NewNode,NewDs).

add_right_pattern_node(Cat/Ds,Cat,Ds).
add_right_pattern_node(dt(Cat,Ds0),p(Cat),Ds) :-
    add_right_pattern(Ds0,[],Ds).
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

simplify(aandachtig,goed,Cat,Cat,D,D,E,E).
simplify(aangaande,over,Cat,Cat,D,D,E,E).
simplify(aangezien,omdat,Cat,Cat,D,D,E,E).
simplify(aanmerkelijk,groot,Cat,Cat,D,D,E,E).
simplify(aanstonds,zo,Cat,Cat,D,D,E,E).
simplify(aanvang,begin,Cat,Cat,D,D,E,E).
%%simplify(aanvankelijk,{[eerst,eerder]},Cat,Cat,D,D,E,E).   %TODO para Aanvankelijk een overeenkomst voor drie jaar en vervolgens om de twee jaar verlengde overeenkomsten .
simplify(aanzienlijk,groot,Cat,Cat,D,D,E,E).
simplify(abuis,fout,Cat,Cat,D,D,E,E).
simplify(acceptatie,goedkeuring,Cat,Cat,D,D,E,E).
simplify(accomodatie,{[gebouw,locatie]},Cat,Cat,D,D,E,E).
simplify(accordeer,keur_goed,Cat,Cat,D,D,E,E).
simplify(acht,vind,Cat,Cat,verb,verb,E,E).
simplify(actualiseer,{[pas_aan,moderniseer,werk_bij]},Cat,Cat,D,D,E,E).
simplify(acuut,direct,Cat,Cat,D,D,E,E).
simplify(additioneel,voeg_toe,ap,ppart,D,D,E,E).
simplify(adequaat,juist,Cat,Cat,D,D,E,E).
simplify(adhesie,steun,Cat,Cat,D,D,E,E).
simplify(adstrueer,leg_uit,Cat,Cat,D,D,E,E).
simplify(affirmatief,bevestig,ap,ppres,D,D,E,E).
simplify(ageer,treed_op,Cat,Cat,D,D,E,E).
simplify(aldaar,daar,Cat,Cat,D,D,E,E).
simplify(aldus,{[zo,volgens]},Cat,Cat,D,D,E,E).
simplify(alloceer,wijs_toe,Cat,Cat,D,D,E,E).
simplify(alom,overal,Cat,Cat,D,D,E,E).
simplify(alvorens,voordat,Cat,Cat,D,D,E,E).
simplify(ambivalent,dubbel,Cat,Cat,D,D,E,E).
simplify(amendement,wijziging,Cat,Cat,D,D,E,E).
simplify(amotie,sloop,Cat,Cat,D,D,E,E).
simplify(amoveer,sloop,Cat,Cat,D,D,E,E).
simplify(ampel,uitvoerig,Cat,Cat,D,D,E,E).
simplify(andermaal,weer,Cat,Cat,D,D,E,E).
simplify(anderszins,anders,Cat,Cat,D,D,E,E).
simplify(annonce,aankondiging,Cat,Cat,D,D,E,E).
simplify(apocrief,twijfelachtig,Cat,Cat,D,D,E,E).
simplify(archaïsch,ouderwets,Cat,Cat,D,D,E,E).
simplify(authentiek,echt,Cat,Cat,D,D,E,E).
simplify(autonoom,zelfstandig,Cat,Cat,D,D,E,E).
simplify(behels,houd_in,Cat,Cat,D,D,E,E).
simplify(behoedzaam,voorzichtig,Cat,Cat,D,D,E,E).
simplify(bemachtig,krijg,Cat,Cat,D,D,E,E).
simplify(bemiddeling_procedure,procedure,Cat,Cat,D,D,E,E).
simplify(bijgevolg,daarom,advp,pp,adv,prep,_,[]).
simplify(blijkens,volgens,Cat,Cat,D,D,E,E).
simplify(bonafide,betrouwbaar,Cat,Cat,D,D,E,E).	% oh brother where art thou
simplify(catastrofe,ramp,Cat,Cat,D,D,E,E).
simplify(cineast,filmmaker,Cat,Cat,D,D,E,E).
simplify(coherentie,samenhang,Cat,Cat,D,D,E,E).
simplify(communiceer,praat,Cat,Cat,D,D,E,E).
simplify(compliceren,moeilijk,ppart,ap,adj,adj,E,E).
simplify(confirmeer,bevestig,Cat,Cat,D,D,E,E).
simplify(conform,volgens,Cat,Cat,D,D,E,E).
simplify(constructief,bruikbaar,Cat,Cat,D,D,E,E).
simplify(consulteer,raadpleeg,Cat,Cat,D,D,E,E).
simplify(continueer,zet_voort,Cat,Cat,D,D,E,E).
simplify(continuïteit,voortgang,Cat,Cat,D,D,E,E).
simplify(continu,steeds,Cat,Cat,D,D,E,E).
simplify(controversieel,omstreden,Cat,Cat,D,D,E,E).
simplify(convenant,overeenkomst,Cat,Cat,D,D,E,E).
simplify(coördinatie,afstemming,Cat,Cat,D,D,E,E).
simplify(cruciaal,belangrijk,Cat,Cat,D,D,E,E).
simplify(cultus,verering,Cat,Cat,D,D,E,E).
simplify(daadwerkelijk,echt,Cat,Cat,D,D,E,E).
simplify(daar,omdat,Cat,Cat,comp,comp,E,E).
simplify(decadent,smakeloos,Cat,Cat,D,D,E,E).
simplify(declameer,draag_voor,Cat,Cat,D,D,E,E).
simplify(deel_mede,zeg,Cat,Cat,D,D,E,E).
simplify(delicaat,gevoelig,Cat,Cat,D,D,E,E).
simplify(derhalve,dus,Cat,Cat,D,D,E,E).
simplify(derving,verlies,Cat,Cat,D,D,E,E).
simplify(destijds,toen,Cat,Cat,D,D,E,E).
simplify(diffuus,onduidelijk,Cat,Cat,D,D,E,E).
simplify(discrepantie,verschil,Cat,Cat,D,D,E,E).
simplify(discutabel,twijfelachtig,Cat,Cat,D,D,E,E).
simplify(dissertatie,proefschrift,Cat,Cat,D,D,E,E).
simplify(dogmatisch,star,Cat,Cat,D,D,E,E).
simplify(dominant,overheers,Cat,Cat,D,D,E,E).
simplify(donatie,schenking,Cat,Cat,D,D,E,E).
simplify(doneer,schenk,Cat,Cat,D,D,E,E).
simplify(doorgaans,meestal,Cat,Cat,D,D,E,E).
simplify(dotatie,schenking,Cat,Cat,D,D,E,E).
simplify(draconisch,streng,Cat,Cat,D,D,E,E).
simplify(dubieus,twijfelachtig,Cat,Cat,D,D,E,E).
simplify(dupeer,benadeel,Cat,Cat,D,D,E,E).
simplify(echter,maar,du,du,D,D,E,E).
simplify(eenvoudig,simpel,Cat,Cat,D,D,E,E).
simplify(eminent,munt_uit,Cat,Cat,D,D,E,E).
simplify(enerverend,spannend,Cat,Cat,D,D,E,E).
simplify(enigma,raadsel,Cat,Cat,D,D,E,E).
simplify(epiloog,slotwoord,Cat,Cat,D,D,E,E).
simplify(etisch,moreel,Cat,Cat,D,D,E,E).
simplify(evident,duidelijk,Cat,Cat,D,D,E,E).
simplify(excessief,overdreven,Cat,Cat,D,D,E,E).
simplify(exercitie,oefening,Cat,Cat,D,D,E,E).
simplify(exhaustief,compleet,Cat,Cat,D,D,E,E).
simplify(exorbitant,overdreven,Cat,Cat,D,D,E,E).
simplify(expertise,kennis,Cat,Cat,D,D,E,E).
simplify(explicatie,uitleg,Cat,Cat,D,D,E,E).
simplify(exploitatie,gebruik,Cat,Cat,D,D,E,E).
simplify(exporteer,voer_uit,Cat,Cat,D,D,E,E).
simplify(fair,eerlijk,Cat,Cat,D,D,E,E).
simplify(falsificatie,vervalsing,Cat,Cat,D,D,E,E).
simplify(fiatteer,keur_goed,Cat,Cat,D,D,E,E).
simplify(filantroop,weldoener,Cat,Cat,D,D,E,E).
simplify(florissant,gunstig,Cat,Cat,D,D,E,E).
simplify(fluctueer,schommel,Cat,Cat,D,D,E,E).
simplify(frictie,wrijving,Cat,Cat,D,D,E,E).
simplify(furieus,boos,Cat,Cat,D,D,E,E).
simplify(futiliteit,kleinigheid,Cat,Cat,D,D,E,E).
simplify(gaarne,graag,Cat,Cat,D,D,E,E).
simplify(geenszins,niet,Cat,Cat,D,D,E,E).
simplify(gedurende,tijdens,Cat,Cat,D,D,E,E).
simplify(geëquipeerd,toegerust,Cat,Cat,D,D,E,E).
simplify(gemeenzaam,alledaags,Cat,Cat,D,D,E,E).
simplify(gepikeerd,boos,Cat,Cat,D,D,E,E).
simplify(gering,klein,Cat,Cat,D,D,E,E).
simplify(geschil,ruzie,Cat,Cat,D,D,E,E).
simplify(governance,bestuur,Cat,Cat,D,D,E,E).
simplify(gracieus,elegant,Cat,Cat,D,D,E,E).
simplify(hectiek,drukte,Cat,Cat,D,D,E,E).
simplify(hectisch,druk,Cat,Cat,D,D,E,E).
simplify(heden,nu,advp,advp,D,D,E,E).
simplify(hermetisch,helemaal,Cat,Cat,D,D,E,E).
simplify(heterogeen,mengen,ap,ppart,D,D,E,E).
simplify(hiërarchie,rangorde,Cat,Cat,D,D,E,E).
simplify(hypothese,aanname,Cat,Cat,D,D,E,E).
simplify(immer,altijd,Cat,Cat,D,D,E,E).
simplify(implementeer,voer_in,Cat,Cat,D,D,E,E).
simplify(impliceer,beteken,Cat,Cat,D,D,E,E).
simplify(implicatie,gevolg,Cat,Cat,D,D,E,E).
simplify(importantie,belang,Cat,Cat,D,D,E,E).
simplify(importeer,voer_in,Cat,Cat,D,D,E,E).
simplify(incidenteel,eenmalig,Cat,Cat,D,D,E,E).
simplify(incrimineer,beschuldig,Cat,Cat,D,D,E,E).
simplify(indicatie,aanwijzing,Cat,Cat,D,D,E,E).
simplify(initieel,één,ap,ap,adj,adj,E,E).
simplify(initieer,begin,Cat,Cat,D,D,E,E).
simplify(insinueer,suggereer,Cat,Cat,D,D,E,E).
simplify(integraal,volledig,Cat,Cat,D,D,E,E).
simplify(intensief,grondig,Cat,Cat,D,D,E,E).
simplify(intensiveer,versterk,Cat,Cat,D,D,E,E).
simplify(intensivering,verdieping,Cat,Cat,D,D,E,E).
simplify(introvert,verlegen,Cat,Cat,D,D,E,E).
simplify(inzake,over,Cat,Cat,D,D,E,E).
simplify(laveloos,dronken,Cat,Cat,D,D,E,E).
simplify(legitiem,wettig,Cat,Cat,D,D,E,E).
simplify(linguïstiek,taalkunde,Cat,Cat,D,D,E,E).
simplify(linguïstisch,taalkundig,Cat,Cat,D,D,E,E).
simplify(linguïst,taalkundige,Cat,Cat,D,D,E,E).
simplify(louter,alleen,Cat,Cat,D,D,E,E).
simplify(ludiek,speels,Cat,Cat,D,D,E,E).
simplify(marginaal,klein,Cat,Cat,D,D,E,E).
simplify(minutieus,precies,Cat,Cat,D,D,E,E).
simplify(mitigeer,zwak_af,Cat,Cat,D,D,E,E).
simplify(moedwillig,expres,Cat,Cat,D,D,E,E).
simplify(momenteel,nu,Cat,Cat,D,D,E,E).
simplify(mutatie,wijziging,Cat,Cat,D,D,E,E).
simplify(nadien,daarna,Cat,Cat,D,D,E,E).
simplify(nauwgezet,precies,Cat,Cat,D,D,E,E).
simplify(nietemin,toch,Cat,Cat,D,D,E,E).
simplify(nimmer,nooit,Cat,Cat,D,D,E,E).
simplify(nochtans,toch,Cat,Cat,D,D,E,E).
simplify(noodzakelijk,nodig,Cat,Cat,D,D,E,E).
simplify(noop,dwing,Cat,Cat,D,D,E,E).
simplify(notificatie,melding,Cat,Cat,D,D,E,E).
simplify(omineus,onheilspellend,Cat,Cat,D,D,E,E).
simplify(omtrent,over,Cat,Cat,D,D,E,E).
simplify(onberispelijk,keurig,Cat,Cat,D,D,E,E).
simplify(onjuist,fout,Cat,Cat,D,D,E,E).
simplify(ontbeer,mis,Cat,Cat,D,D,E,E).
simplify(ontdaan,overstuur,Cat,Cat,D,D,E,E).
simplify(onverwijld,onmiddellijk,Cat,Cat,D,D,E,E).
simplify(onvolkomenheid,fout,Cat,Cat,D,D,E,E).
simplify(opgetogen,blij,Cat,Cat,D,D,E,E).
simplify(opteer,kies,Cat,Cat,D,D,E,E).
simplify(optimaliseer,verbeter,Cat,Cat,D,D,E,E).
simplify(overeenkomstig,volgens,Cat,Cat,D,D,E,E).
simplify(paraaf,handtekening,Cat,Cat,D,D,E,E).
simplify(participatie,deelname,Cat,Cat,D,D,E,E).
simplify(partieel,gedeeltelijk,Cat,Cat,D,D,E,E).
simplify(pendule,klok,Cat,Cat,D,D,E,E).
simplify(percipieer,neem_waar,Cat,Cat,D,D,E,E).
simplify(perspectief,zicht,Cat,Cat,D,D,E,E).
simplify(pertinent,echt,Cat,Cat,D,D,E,E).
simplify(plausibel,redelijk,Cat,Cat,D,D,E,E).
simplify(poog,probeer,Cat,Cat,D,D,E,E).
simplify(potentieel,mogelijk,Cat,Cat,adj,adj,E,E).
simplify(potentieel,mogelijkheid,Cat,Cat,noun,noun,E,E).
simplify(prerogatief,voorrecht,Cat,Cat,D,D,E,E).
simplify(prioritair,belangrijk,Cat,Cat,D,D,E,E).
simplify(prioriteit,{[voorrang,kern_punt]},Cat,Cat,D,D,E,E).
simplify(procedure,werkwijze,Cat,Cat,D,D,E,E).
simplify(prolongatie,verlenging,Cat,Cat,D,D,E,E).
simplify(prominent,belangrijk,Cat,Cat,D,D,E,E).
simplify(quarantaine,afzondering,Cat,Cat,D,D,E,E).
simplify(rationeel,verstandig,Cat,Cat,D,D,E,E).
simplify(recapituleer,vat_samen,Cat,Cat,D,D,E,E).
simplify(reduceer,verminder,Cat,Cat,D,D,E,E).
simplify(reductie,beperking,Cat,Cat,D,D,E,E).
simplify(reeds,al,Cat,Cat,D,D,E,E).
simplify(refereer,verwijs,Cat,Cat,D,D,E,E).
simplify(referentie,verwijzing,Cat,Cat,D,D,E,E).
simplify(relevant,belangrijk,Cat,Cat,D,D,E,E).
simplify(reprimande,waarschuwing,Cat,Cat,D,D,E,E).
simplify(respons,antwoord,Cat,Cat,D,D,E,E).
simplify(ressentiment,wrok,Cat,Cat,D,D,E,E).
simplify(restrictie,beperking,Cat,Cat,D,D,E,E).
simplify(resumé,samenvatting,Cat,Cat,D,D,E,E).
simplify(resumeer,vat_samen,Cat,Cat,D,D,E,E).
simplify(retourneer,stuur_terug,Cat,Cat,D,D,E,E).
simplify(ridicuul,belachelijk,Cat,Cat,D,D,E,E).
simplify(robuust,stevig,Cat,Cat,D,D,E,E).
simplify(sanctie,maatregel,Cat,Cat,D,D,E,E).
simplify(secularisatie,ontkerkelijking,Cat,Cat,D,D,E,E).
simplify(secuur,precies,Cat,Cat,D,D,E,E).
simplify(sedert,sinds,Cat,Cat,D,D,E,E).
simplify(sereen,vredig,Cat,Cat,D,D,E,E).
simplify(sommeer,beveel,Cat,Cat,D,D,E,E).
simplify(speculaar,gok,Cat,Cat,D,D,E,E).
simplify(sporadisch,soms,Cat,Cat,D,D,E,E).
simplify(staatshoofd,koning,Cat,Cat,D,D,E,E).
simplify(stagnatie,stilstand,Cat,Cat,D,D,E,E).
simplify(stakeholder,betrekken,Cat,Cat,D,D,E,[personalized=true,aform=base|E]).
simplify(start_op,begin,Cat,Cat,D,D,E,E).
simplify(stoïcijns,onverstoorbaar,Cat,Cat,D,D,E,E).
simplify(strategie,plan,Cat,Cat,D,D,E,E).
simplify(stringent,streng,Cat,Cat,D,D,E,E).
simplify(stuur_aan,{[leid,stuur]},Cat,Cat,D,D,E,E).
simplify(substantieel,flink,Cat,Cat,D,D,E,E).
simplify(substituut,vervanging,Cat,Cat,D,D,E,E).
simplify(summier,kort,Cat,Cat,D,D,E,E).
simplify(target,doel,Cat,Cat,D,D,E,E).
simplify(taskforce,werk_groep,Cat,Cat,D,D,E,E).
simplify(teneinde,om,cp,oti,D,D,E,E).
simplify(tenuitvoerlegging,uitvoering,Cat,Cat,D,D,E,E).
simplify(terstond,onmiddellijk,Cat,Cat,D,D,E,E).
simplify(tevens,ook,Cat,Cat,D,D,E,E).
simplify(transparantie,duidelijkheid,Cat,Cat,D,D,E,E).
simplify(tref_aan,vind,Cat,Cat,D,D,E,E).
simplify(triviaal,gewoon,Cat,Cat,D,D,E,E).
simplify(uiterst,heel,ap,advp,adj,adv,_,[]).
simplify(uitfasering,stop,Cat,Cat,D,D,E,E).
simplify(universeel,algemeen,Cat,Cat,D,D,E,E).
simplify('up-to-date',actueel,Cat,Cat,D,D,E,E).
simplify(urgent,dringend,Cat,Cat,D,D,E,E).
simplify(urgentie,haast,Cat,Cat,D,D,E,E).
simplify(vacant,vrij,Cat,Cat,D,D,E,E).
simplify(verifieer,controleer,Cat,Cat,D,D,E,E).
simplify(verklaar,zeg,Cat,Cat,D,D,E,E).
simplify(volgaarne,graag,Cat,Cat,D,D,E,E).
simplify(voorts,ook,Cat,Cat,D,D,E,E).
% simplify(vorder,eis,Cat,Cat,D,D,E,E).  alleen als er obj1 is? 
simplify(wederrechtelijk,onwettig,Cat,Cat,D,D,E,E).
simplify(weifel,aarzel,Cat,Cat,D,D,E,E).
simplify(wend_aan,gebruik,Cat,Cat,D,D,E,E).
simplify(wetgeving_resolutie,resolutie,Cat,Cat,D,D,E,E).

adapt_det(List0,List):-
    Det0 = tree(r(det,adt_lex(Cat0,Old,Old,Pos,Atts)),[]),
    Det  = tree(r(det,adt_lex(Cat0,New,New,Pos,Atts)),[]),
    lists:select(Det0,List0,List1),
    simplify_det(Old,New),
    !,
    List = [Det|List1].

simplify_det(dat,{[die,dat]}).
simplify_det(de,{[de,het]}).
simplify_det(deze,{[deze,dit]}).
simplify_det(die,{[die,dat]}).
simplify_det(dit,{[deze,dit]}).
simplify_det(het,{[de,het]}).

