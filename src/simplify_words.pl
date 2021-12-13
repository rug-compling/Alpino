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
    Hd0 = tree(r(HD,adt_lex(Cat0,Old,Old,D,E0)),[]),
    Hd  = tree(r(HD,adt_lex(Cat,New,New,D,E)),[]),
    head_rel(HD),
    lists:select(Hd0,Ds0,Ds1),
    simplify(Old,New,Cat0,Cat,D,E0,E),
    adapt_det(Ds1,Ds).

words_transformation(r(Rel,p(mwu(_,_))),Ds,
		     r(Rel,adt_lex(Cat,Lem,Lem,Pos,Atts)),[]):-
    mwu(Ds,Words),
    simplify_mwu(Words,Lem,Pos,Cat,Atts).

words_transformation(r(Rel,adt_lex(_,Lem,Lem,_,_)),[],
		     r(Rel,Cat),Ds):-
    lemma_to_tree(Lem,Cat,Ds).

words_transformation(r(Rel,adt_lex(Cat0,Old,Old,D,E0)),[],
		     r(Rel,adt_lex(Cat,New,New,D,E)),[]) :-
    \+ Rel = mwp,
    simplify(Old,New,Cat0,Cat,D,E0,E).

words_transformation(r(Rel,Cat),Ds0,r(Rel,Cat),Ds) :-
    pattern_rule(Left,Right),
    match_left_pattern(Left,Ds0,Ds1),
    add_right_pattern(Right,Ds1,Ds).

head_rel(hd).
head_rel(cmp).

mwu([],[]).
mwu([tree(r(mwp,adt_lex(_,W,_,_,_)),[])|Trees],[W|Ws]) :-
    mwu(Trees,Ws).

%%%

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
match_left_pattern_node(dt(Cat,Pattern),p(Cat),Ds) :-
    match_left_pattern(Pattern,Ds,[]).
    
match_left_pattern_node(l(Lem,Pos,Atts),adt_lex(_,Lem,_,Pos,Atts),[]).
match_left_pattern_node(mwu(List),p(mwu(_,_)),Ds) :-
    mwu(Ds,List).

add_right_pattern([],Ds,Ds).
add_right_pattern([H|T],Ds0,Ds) :-
    add_right_pattern_tree(H,Ds0,Ds1),
    add_right_pattern(T,Ds1,Ds).

add_right_pattern_tree(Rel=Node,Ds,[tree(r(Rel,NewNode),NewDs)|Ds]) :-
    add_right_pattern_node(Node,NewNode,NewDs).

add_right_pattern_node(Cat/Ds,Cat,Ds).
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

simplify(aandachtig,goed,Cat,Cat,_,E,E).
simplify(aangaande,over,Cat,Cat,_,E,E).
simplify(aangezien,omdat,Cat,Cat,_,E,E).
simplify(aanmerkelijk,groot,Cat,Cat,_,E,E).
simplify(aanstonds,zo,Cat,Cat,_,E,E).
simplify(aanvang,begin,Cat,Cat,_,E,E).
%%simplify(aanvankelijk,{[eerst,eerder]},Cat,Cat,_,E,E).   %TODO para Aanvankelijk een overeenkomst voor drie jaar en vervolgens om de twee jaar verlengde overeenkomsten .
simplify(aanzienlijk,groot,Cat,Cat,_,E,E).
simplify(abuis,fout,Cat,Cat,_,E,E).
simplify(acceptatie,goedkeuring,Cat,Cat,_,E,E).
simplify(accomodatie,{[gebouw,locatie]},Cat,Cat,_,E,E).
simplify(accordeer,keur_goed,Cat,Cat,_,E,E).
simplify(acht,vind,Cat,Cat,_,E,E).
simplify(actualiseer,{[pas_aan,moderniseer,werk_bij]},Cat,Cat,_,E,E).
simplify(acuut,direct,Cat,Cat,_,E,E).
simplify(additioneel,voeg_toe,ap,ppart,_,E,E).
simplify(adequaat,juist,Cat,Cat,_,E,E).
simplify(adhesie,steun,Cat,Cat,_,E,E).
simplify(adstrueer,leg_uit,Cat,Cat,_,E,E).
simplify(affirmatief,bevestig,ap,ppres,_,E,E).
simplify(ageer,treed_op,Cat,Cat,_,E,E).
simplify(aldaar,daar,Cat,Cat,_,E,E).
simplify(aldus,{[zo,volgens]},Cat,Cat,_,E,E).
simplify(alloceer,wijs_toe,Cat,Cat,_,E,E).
simplify(alom,overal,Cat,Cat,_,E,E).
simplify(alvorens,voordat,Cat,Cat,_,E,E).
simplify(ambivalent,dubbel,Cat,Cat,_,E,E).
simplify(amendement,wijziging,Cat,Cat,_,E,E).
simplify(amotie,sloop,Cat,Cat,_,E,E).
simplify(amoveer,sloop,Cat,Cat,_,E,E).
simplify(ampel,uitvoerig,Cat,Cat,_,E,E).
simplify(andermaal,weer,Cat,Cat,_,E,E).
simplify(anderszins,anders,Cat,Cat,_,E,E).
simplify(annonce,aankondiging,Cat,Cat,_,E,E).
simplify(apocrief,twijfelachtig,Cat,Cat,_,E,E).
simplify(archaïsch,ouderwets,Cat,Cat,_,E,E).
simplify(authentiek,echt,Cat,Cat,_,E,E).
simplify(autonoom,zelfstandig,Cat,Cat,_,E,E).
simplify(behels,houd_in,Cat,Cat,_,E,E).
simplify(behoedzaam,voorzichtig,Cat,Cat,_,E,E).
simplify(bemachtig,krijg,Cat,Cat,_,E,E).
simplify(bemiddeling_procedure,procedure,Cat,Cat,_,E,E).
simplify(bijgevolg,daarom,Cat,Cat,_,E,E).
simplify(blijkens,volgens,Cat,Cat,_,E,E).
simplify(bonafide,betrouwbaar,Cat,Cat,_,E,E).	% oh brother where art thou
simplify(catastrofe,ramp,Cat,Cat,_,E,E).
simplify(cineast,filmmaker,Cat,Cat,_,E,E).
simplify(coherentie,samenhang,Cat,Cat,_,E,E).
simplify(communiceer,praat,Cat,Cat,_,E,E).
simplify(compliceren,moeilijk,ppart,ap,adj,E,E).
simplify(confirmeer,bevestig,Cat,Cat,_,E,E).
simplify(conform,volgens,Cat,Cat,_,E,E).
simplify(constructief,bruikbaar,Cat,Cat,_,E,E).
simplify(consulteer,raadpleeg,Cat,Cat,_,E,E).
simplify(continueer,zet_voort,Cat,Cat,_,E,E).
simplify(continuïteit,voortgang,Cat,Cat,_,E,E).
simplify(continu,steeds,Cat,Cat,_,E,E).
simplify(controversieel,omstreden,Cat,Cat,_,E,E).
simplify(convenant,overeenkomst,Cat,Cat,_,E,E).
simplify(coördinatie,afstemming,Cat,Cat,_,E,E).
simplify(cruciaal,belangrijk,Cat,Cat,_,E,E).
simplify(cultus,verering,Cat,Cat,_,E,E).
simplify(daadwerkelijk,echt,Cat,Cat,_,E,E).
simplify(daar,omdat,Cat,Cat,comp,E,E).
simplify(decadent,smakeloos,Cat,Cat,_,E,E).
simplify(declameer,draag_voor,Cat,Cat,_,E,E).
simplify(deel_mede,zeg,Cat,Cat,_,E,E).
simplify(delicaat,gevoelig,Cat,Cat,_,E,E).
simplify(derving,verlies,Cat,Cat,_,E,E).
simplify(diffuus,onduidelijk,Cat,Cat,_,E,E).
simplify(discrepantie,verschil,Cat,Cat,_,E,E).
simplify(discutabel,twijfelachtig,Cat,Cat,_,E,E).
simplify(dissertatie,proefschrift,Cat,Cat,_,E,E).
simplify(dogmatisch,star,Cat,Cat,_,E,E).
simplify(dominant,overheers,Cat,Cat,_,E,E).
simplify(donatie,schenking,Cat,Cat,_,E,E).
simplify(doneer,schenk,Cat,Cat,_,E,E).
simplify(doorgaans,meestal,Cat,Cat,_,E,E).
simplify(dotatie,schenking,Cat,Cat,_,E,E).
simplify(draconisch,streng,Cat,Cat,_,E,E).
simplify(dubieus,twijfelachtig,Cat,Cat,_,E,E).
simplify(dupeer,benadeel,Cat,Cat,_,E,E).
simplify(echter,maar,du,du,_,E,E).
simplify(eenvoudig,simpel,Cat,Cat,_,E,E).
simplify(eminent,munt_uit,Cat,Cat,_,E,E).
simplify(enerverend,spannend,Cat,Cat,_,E,E).
simplify(enigma,raadsel,Cat,Cat,_,E,E).
simplify(epiloog,slotwoord,Cat,Cat,_,E,E).
simplify(etisch,moreel,Cat,Cat,_,E,E).
simplify(evident,duidelijk,Cat,Cat,_,E,E).
simplify(excessief,overdreven,Cat,Cat,_,E,E).
simplify(exercitie,oefening,Cat,Cat,_,E,E).
simplify(exhaustief,compleet,Cat,Cat,_,E,E).
simplify(exorbitant,overdreven,Cat,Cat,_,E,E).
simplify(expertise,kennis,Cat,Cat,_,E,E).
simplify(explicatie,uitleg,Cat,Cat,_,E,E).
simplify(exploitatie,gebruik,Cat,Cat,_,E,E).
simplify(exporteer,voer_uit,Cat,Cat,_,E,E).
simplify(fair,eerlijk,Cat,Cat,_,E,E).
simplify(falsificatie,vervalsing,Cat,Cat,_,E,E).
simplify(fiatteer,keur_goed,Cat,Cat,_,E,E).
simplify(filantroop,weldoener,Cat,Cat,_,E,E).
simplify(florissant,gunstig,Cat,Cat,_,E,E).
simplify(fluctueer,schommel,Cat,Cat,_,E,E).
simplify(frictie,wrijving,Cat,Cat,_,E,E).
simplify(furieus,boos,Cat,Cat,_,E,E).
simplify(futiliteit,kleinigheid,Cat,Cat,_,E,E).
simplify(gaarne,graag,Cat,Cat,_,E,E).
simplify(geenszins,niet,Cat,Cat,_,E,E).
simplify(gedurende,tijdens,Cat,Cat,_,E,E).
simplify(geëquipeerd,toegerust,Cat,Cat,_,E,E).
simplify(gemeenzaam,alledaags,Cat,Cat,_,E,E).
simplify(gepikeerd,boos,Cat,Cat,_,E,E).
simplify(gering,klein,Cat,Cat,_,E,E).
simplify(geschil,ruzie,Cat,Cat,_,E,E).
simplify(governance,bestuur,Cat,Cat,_,E,E).
simplify(gracieus,elegant,Cat,Cat,_,E,E).
simplify(hectiek,drukte,Cat,Cat,_,E,E).
simplify(hectisch,druk,Cat,Cat,_,E,E).
simplify(heden,nu,advp,advp,_,E,E).
simplify(hermetisch,helemaal,Cat,Cat,_,E,E).
simplify(heterogeen,mengen,ap,ppart,_,E,E).
simplify(hiërarchie,rangorde,Cat,Cat,_,E,E).
simplify(hypothese,aanname,Cat,Cat,_,E,E).
simplify(immer,altijd,Cat,Cat,_,E,E).
simplify(implementeer,voer_in,Cat,Cat,_,E,E).
simplify(impliceer,beteken,Cat,Cat,_,E,E).
simplify(implicatie,gevolg,Cat,Cat,_,E,E).
simplify(importantie,belang,Cat,Cat,_,E,E).
simplify(importeer,voer_in,Cat,Cat,_,E,E).
simplify(incidenteel,eenmalig,Cat,Cat,_,E,E).
simplify(incrimineer,beschuldig,Cat,Cat,_,E,E).
simplify(indicatie,aanwijzing,Cat,Cat,_,E,E).
simplify(initieel,één,ap,ap,adj,E,E).
simplify(initieer,begin,Cat,Cat,_,E,E).
simplify(insinueer,suggereer,Cat,Cat,_,E,E).
simplify(integraal,volledig,Cat,Cat,_,E,E).
simplify(intensief,grondig,Cat,Cat,_,E,E).
simplify(intensiveer,versterk,Cat,Cat,_,E,E).
simplify(intensivering,verdieping,Cat,Cat,_,E,E).
simplify(introvert,verlegen,Cat,Cat,_,E,E).
simplify(inzake,over,Cat,Cat,_,E,E).
simplify(laveloos,dronken,Cat,Cat,_,E,E).
simplify(legitiem,wettig,Cat,Cat,_,E,E).
simplify(linguïstiek,taalkunde,Cat,Cat,_,E,E).
simplify(linguïstisch,taalkundig,Cat,Cat,_,E,E).
simplify(linguïst,taalkundige,Cat,Cat,_,E,E).
simplify(louter,alleen,Cat,Cat,_,E,E).
simplify(ludiek,speels,Cat,Cat,_,E,E).
simplify(marginaal,klein,Cat,Cat,_,E,E).
simplify(minutieus,precies,Cat,Cat,_,E,E).
simplify(mitigeer,zwak_af,Cat,Cat,_,E,E).
simplify(moedwillig,expres,Cat,Cat,_,E,E).
simplify(momenteel,nu,Cat,Cat,_,E,E).
simplify(mutatie,wijziging,Cat,Cat,_,E,E).
simplify(nadien,daarna,Cat,Cat,_,E,E).
simplify(nietemin,toch,Cat,Cat,_,E,E).
simplify(nimmer,nooit,Cat,Cat,_,E,E).
simplify(nochtans,toch,Cat,Cat,_,E,E).
simplify(noodzakelijk,nodig,Cat,Cat,_,E,E).
simplify(noop,dwing,Cat,Cat,_,E,E).
simplify(notificatie,melding,Cat,Cat,_,E,E).
simplify(omineus,onheilspellend,Cat,Cat,_,E,E).
simplify(omtrent,over,Cat,Cat,_,E,E).
simplify(onberispelijk,keurig,Cat,Cat,_,E,E).
simplify(onjuist,fout,Cat,Cat,_,E,E).
simplify(ontbeer,mis,Cat,Cat,_,E,E).
simplify(ontdaan,overstuur,Cat,Cat,_,E,E).
simplify(onverwijld,onmiddellijk,Cat,Cat,_,E,E).
simplify(onvolkomenheid,fout,Cat,Cat,_,E,E).
simplify(opgetogen,blij,Cat,Cat,_,E,E).
simplify(opteer,kies,Cat,Cat,_,E,E).
simplify(optimaliseer,verbeter,Cat,Cat,_,E,E).
simplify(overeenkomstig,volgens,Cat,Cat,_,E,E).
simplify(paraaf,handtekening,Cat,Cat,_,E,E).
simplify(participatie,deelname,Cat,Cat,_,E,E).
simplify(partieel,gedeeltelijk,Cat,Cat,_,E,E).
simplify(pendule,klok,Cat,Cat,_,E,E).
simplify(percipieer,neem_waar,Cat,Cat,_,E,E).
simplify(perspectief,zicht,Cat,Cat,_,E,E).
simplify(pertinent,echt,Cat,Cat,_,E,E).
simplify(plausibel,redelijk,Cat,Cat,_,E,E).
simplify(poog,probeer,Cat,Cat,_,E,E).
simplify(potentieel,mogelijk,Cat,Cat,adj,E,E).
simplify(potentieel,mogelijkheid,Cat,Cat,noun,E,E).
simplify(prerogatief,voorrecht,Cat,Cat,_,E,E).
simplify(prioritair,belangrijk,Cat,Cat,_,E,E).
simplify(prioriteit,{[voorrang,kern_punt]},Cat,Cat,_,E,E).
simplify(procedure,werkwijze,Cat,Cat,_,E,E).
simplify(prolongatie,verlenging,Cat,Cat,_,E,E).
simplify(prominent,belangrijk,Cat,Cat,_,E,E).
simplify(quarantaine,afzondering,Cat,Cat,_,E,E).
simplify(rationeel,verstandig,Cat,Cat,_,E,E).
simplify(recapituleer,vat_samen,Cat,Cat,_,E,E).
simplify(reduceer,verminder,Cat,Cat,_,E,E).
simplify(reductie,beperking,Cat,Cat,_,E,E).
simplify(refereer,verwijs,Cat,Cat,_,E,E).
simplify(referentie,verwijzing,Cat,Cat,_,E,E).
simplify(relevant,belangrijk,Cat,Cat,_,E,E).
simplify(reprimande,waarschuwing,Cat,Cat,_,E,E).
simplify(respons,antwoord,Cat,Cat,_,E,E).
simplify(ressentiment,wrok,Cat,Cat,_,E,E).
simplify(restrictie,beperking,Cat,Cat,_,E,E).
simplify(resumé,samenvatting,Cat,Cat,_,E,E).
simplify(resumeer,vat_samen,Cat,Cat,_,E,E).
simplify(retourneer,stuur_terug,Cat,Cat,_,E,E).
simplify(ridicuul,belachelijk,Cat,Cat,_,E,E).
simplify(robuust,stevig,Cat,Cat,_,E,E).
simplify(sanctie,maatregel,Cat,Cat,_,E,E).
simplify(secularisatie,ontkerkelijking,Cat,Cat,_,E,E).
simplify(secuur,precies,Cat,Cat,_,E,E).
simplify(sedert,sinds,Cat,Cat,_,E,E).
simplify(sereen,vredig,Cat,Cat,_,E,E).
simplify(sommeer,beveel,Cat,Cat,_,E,E).
simplify(speculaar,gok,Cat,Cat,_,E,E).
simplify(sporadisch,soms,Cat,Cat,_,E,E).
simplify(staatshoofd,koning,Cat,Cat,_,E,E).
simplify(stagnatie,stilstand,Cat,Cat,_,E,E).
simplify(stakeholder,betrekken,Cat,Cat,_,E,[personalized=true,aform=base|E]).
simplify(start_op,begin,Cat,Cat,_,E,E).
simplify(stoïcijns,onverstoorbaar,Cat,Cat,_,E,E).
simplify(strategie,plan,Cat,Cat,_,E,E).
simplify(stringent,streng,Cat,Cat,_,E,E).
simplify(stuur_aan,{[leid,stuur]},Cat,Cat,_,E,E).
simplify(substantieel,flink,Cat,Cat,_,E,E).
simplify(substituut,vervanging,Cat,Cat,_,E,E).
simplify(summier,kort,Cat,Cat,_,E,E).
simplify(target,doel,Cat,Cat,_,E,E).
simplify(taskforce,werk_groep,Cat,Cat,_,E,E).
simplify(teneinde,om,cp,oti,_,E,E).
simplify(tenuitvoerlegging,uitvoering,Cat,Cat,_,E,E).
simplify(terstond,onmiddellijk,Cat,Cat,_,E,E).
simplify(tevens,ook,Cat,Cat,_,E,E).
simplify(transparantie,duidelijkheid,Cat,Cat,_,E,E).
simplify(tref_aan,vind,Cat,Cat,_,E,E).
simplify(triviaal,gewoon,Cat,Cat,_,E,E).
simplify(uiterst,heel,Cat,Cat,_,E,E).
simplify(uitfasering,stop,Cat,Cat,_,E,E).
simplify(universeel,algemeen,Cat,Cat,_,E,E).
simplify('up-to-date',actueel,Cat,Cat,_,E,E).
simplify(urgent,dringend,Cat,Cat,_,E,E).
simplify(urgentie,haast,Cat,Cat,_,E,E).
simplify(vacant,vrij,Cat,Cat,_,E,E).
simplify(verifieer,controleer,Cat,Cat,_,E,E).
simplify(verklaar,zeg,Cat,Cat,_,E,E).
simplify(volgaarne,graag,Cat,Cat,_,E,E).
simplify(voorts,ook,Cat,Cat,_,E,E).
simplify(vorder,eis,Cat,Cat,_,E,E).
simplify(wederrechtelijk,onwettig,Cat,Cat,_,E,E).
simplify(weifel,aarzel,Cat,Cat,_,E,E).
simplify(wend_aan,gebruik,Cat,Cat,_,E,E).
simplify(wetgeving_resolutie,resolutie,Cat,Cat,_,E,E).

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

