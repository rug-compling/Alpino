:- module(alpino_user_transformation, [ apply_adt_transformations/2 ]).

%% --------------------------------------------------------------------------------------------- %%

apply_adt_transformations(Tree0,Tree) :-
    dont_paraphrase(Tree0), !,
    Tree0 = Tree.

apply_adt_transformations(Tree0,Tree) :-
    apply_words_transformations(Tree0,Tree).

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
    Hd0 = tree(r(HD,adt_lex(Cat0,Old,Old,D,E)),[]),
    Hd  = tree(r(HD,adt_lex(Cat,New,New,D,E)),[]),
    head_rel(HD),
    lists:select(Hd0,Ds0,Ds),
    simplify(Old,New,Cat0,Cat,D).

words_transformation(r(Rel,p(mwu(_,_))),Ds,
		     r(Rel,adt_lex(Cat,Lem,Lem,Pos,[])),[]):-
    mwu(Ds,Words),
    simplify_mwu(Words,Lem,Pos,Cat).

words_transformation(r(Rel,adt_lex(Cat0,Old,Old,D,E)),[],
		     r(Rel,adt_lex(Cat,New,New,D,E)),[]) :-
    \+ Rel = mwp,
    simplify(Old,New,Cat0,Cat,D).

head_rel(hd).
head_rel(cmp).

mwu([],[]).
mwu([tree(r(mwp,adt_lex(_,W,_,_,_)),[])|Trees],[W|Ws]) :-
    mwu(Trees,Ws).

%% must replace something...
replace(El0,El,[El0|Tail],[El|Tail]).
replace(El0,El,[X|Tail0],[X|Tail]):-
    replace(El0,El,Tail0,Tail).

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

simplify_mwu([acquis,communautaire],gemeenschap_recht,noun,np).
simplify_mwu([ten,volle],helemaal,adv,advp).

%% det
simplify(de,{[de,het]},Cat,Cat,_).
simplify(het,{[de,het]},Cat,Cat,_).
simplify(dit,{[deze,dit]},Cat,Cat,_).
simplify(deze,{[deze,dit]},Cat,Cat,_).
simplify(die,{[die,dat]},Cat,Cat,_).
simplify(dat,{[die,dat]},Cat,Cat,_).

%% compounds
simplify(bemiddeling_procedure,procedure,Cat,Cat,_).
simplify(wetgeving_resolutie,resolutie,Cat,Cat,_).

%% adj, noun
simplify(aanstonds,zo,Cat,Cat,_).
simplify(aanzienlijk,groot,Cat,Cat,_).
simplify(abuis,fout,Cat,Cat,_).
simplify(additioneel,voeg_toe,ap,ppart,_).
simplify(adhesie,steun,Cat,Cat,_).
simplify(ampel,uitvoerig,Cat,Cat,_).
simplify(apocrief,twijfelachtig,Cat,Cat,_).
simplify(archaïsch,ouderwets,Cat,Cat,_).
simplify(authentiek,echt,Cat,Cat,_).
simplify(autonoom,zelfstandig,Cat,Cat,_).
simplify(behoedzaam,voorzichtig,Cat,Cat,_).
simplify(bijgevolg,daarom,Cat,Cat,_).
simplify(bonafide,betrouwbaar,Cat,Cat,_).	% oh brother where art thou
simplify(catastrofe,ramp,Cat,Cat,_).
simplify(cineast,filmmaker,Cat,Cat,_).
simplify(coherentie,samenhang,Cat,Cat,_).
simplify(compliceren,moeilijk,ppart,ap,adj).
simplify(constructief,bruikbaar,Cat,Cat,_).
simplify(continu,steeds,Cat,Cat,_).
simplify(continuïteit,voortgang,Cat,Cat,_).
simplify(controversieel,omstreden,Cat,Cat,_).
simplify(convenant,overeenkomst,Cat,Cat,_).
simplify(coördinatie,afstemming,Cat,Cat,_).
simplify(cruciaal,belangrijk,Cat,Cat,_).
simplify(cultus,verering,Cat,Cat,_).
simplify(daadwerkelijk,echt,Cat,Cat,_).
simplify(decadent,smakeloos,Cat,Cat,_).
simplify(delicaat,gevoelig,Cat,Cat,_).
simplify(derving,verlies,Cat,Cat,_).
simplify(discrepantie,verschil,Cat,Cat,_).
simplify(discutabel,twijfelachtig,Cat,Cat,_).
simplify(dissertatie,proefschrift,Cat,Cat,_).
simplify(diffuus,onduidelijk,Cat,Cat,_).
simplify(dogmatisch,star,Cat,Cat,_).
simplify(dominant,overheers,Cat,Cat,_).
simplify(donatie,schenking,Cat,Cat,_).
simplify(doorgaans,meestal,Cat,Cat,_).
simplify(dotatie,schenking,Cat,Cat,_).
simplify(draconisch,streng,Cat,Cat,_).
simplify(dubieus,twijfelachtig,Cat,Cat,_).
simplify(eenvoudig,simpel,Cat,Cat,_).
simplify(eminent,munt_uit,Cat,Cat,_).
simplify(enerverend,spannend,Cat,Cat,_).
simplify(enigma,raadsel,Cat,Cat,_).
simplify(epiloog,slotwoord,Cat,Cat,_).
simplify(etisch,moreel,Cat,Cat,_).
simplify(evident,duidelijk,Cat,Cat,_).
simplify(excessief,overdreven,Cat,Cat,_).
simplify(exercitie,oefening,Cat,Cat,_).
simplify(exhaustief,compleet,Cat,Cat,_).
simplify(exorbitant,overdreven,Cat,Cat,_).
simplify(expertise,kennis,Cat,Cat,_).
simplify(explicatie,uitleg,Cat,Cat,_).
simplify(fair,eerlijk,Cat,Cat,_).
simplify(falsificatie,vervalsing,Cat,Cat,_).
simplify(filantroop,weldoener,Cat,Cat,_).
simplify(florissant,gunstig,Cat,Cat,_).
simplify(frictie,wrijving,Cat,Cat,_).
simplify(furieus,boos,Cat,Cat,_).
simplify(futiliteit,kleinigheid,Cat,Cat,_).
simplify(geenszins,niet,Cat,Cat,_).
simplify(gepikeerd,boos,Cat,Cat,_).
simplify(gering,klein,Cat,Cat,_).
simplify(governance,bestuur,Cat,Cat,_).
simplify(gracieus,elegant,Cat,Cat,_).
simplify(hectiek,drukte,Cat,Cat,_).
simplify(hectisch,druk,Cat,Cat,_).
simplify(hermetisch,helemaal,Cat,Cat,_).
simplify(heterogeen,mengen,ap,ppart,_).
simplify(hiërarchie,rangorde,Cat,Cat,_).
simplify(hypothese,aanname,Cat,Cat,_).
simplify(immer,altijd,Cat,Cat,_).
simplify(implicatie,gevolg,Cat,Cat,_).
simplify(importantie,belang,Cat,Cat,_).
simplify(initieel,één,ap,ap,adj).
simplify(incidenteel,eenmalig,Cat,Cat,_).
simplify(indicatie,aanwijzing,Cat,Cat,_).
simplify(integraal,volledig,Cat,Cat,_).
simplify(intensief,grondig,Cat,Cat,_).
simplify(intensivering,verdieping,Cat,Cat,_).
simplify(introvert,verlegen,Cat,Cat,_).
simplify(laveloos,dronken,Cat,Cat,_).
simplify(legitiem,wettig,Cat,Cat,_).
simplify(linguïstiek,taalkunde,Cat,Cat,_).
simplify(linguïst,taalkundige,Cat,Cat,_).
simplify(linguïstisch,taalkundig,Cat,Cat,_).
simplify(louter,alleen,Cat,Cat,_).
simplify(ludiek,speels,Cat,Cat,_).
simplify(marginaal,klein,Cat,Cat,_).
simplify(minutieus,precies,Cat,Cat,_).
simplify(moedwillig,expres,Cat,Cat,_).
simplify(momenteel,nu,Cat,Cat,_).
simplify(notificatie,melding,Cat,Cat,_).
simplify(omineus,onheilspellend,Cat,Cat,_).
simplify(onberispelijk,keurig,Cat,Cat,_).
simplify(ontdaan,overstuur,Cat,Cat,_).
simplify(opgetogen,blij,Cat,Cat,_).
simplify(participatie,deelname,Cat,Cat,_).
simplify(pendule,klok,Cat,Cat,_).
simplify(perspectief,zicht,Cat,Cat,_).
simplify(pertinent,echt,Cat,Cat,_).
simplify(potentieel,mogelijk,Cat,Cat,adj).
simplify(potentieel,mogelijkheid,Cat,Cat,noun).
simplify(plausibel,redelijk,Cat,Cat,_).
simplify(prioritair,belangrijk,Cat,Cat,_).
simplify(prioriteit,kern_punt,Cat,Cat,_).
simplify(prolongatie,verlenging,Cat,Cat,_).
simplify(quarantaine,afzondering,Cat,Cat,_).
simplify(reductie,beperking,Cat,Cat,_).
simplify(relevant,belangrijk,Cat,Cat,_).
simplify(reprimande,waarschuwing,Cat,Cat,_).
simplify(ressentiment,wrok,Cat,Cat,_).
simplify(ridicuul,belachelijk,Cat,Cat,_).
simplify(robuust,stevig,Cat,Cat,_).
simplify(secularisatie,ontkerkelijking,Cat,Cat,_).
simplify(secuur,precies,Cat,Cat,_).
simplify(sereen,vredig,Cat,Cat,_).
simplify(sporadisch,soms,Cat,Cat,_).
simplify(staatshoofd,koning,Cat,Cat,_).
simplify(stagnatie,stilstand,Cat,Cat,_).
simplify(stoïcijns,onverstoorbaar,Cat,Cat,_).
simplify(substantieel,flink,Cat,Cat,_).
simplify(substituut,vervanging,Cat,Cat,_).
simplify(tenuitvoerlegging,uitvoering,Cat,Cat,_).
simplify(tevens,ook,Cat,Cat,_).
simplify(transparantie,duidelijkheid,Cat,Cat,_).
simplify(triviaal,gewoon,Cat,Cat,_).
simplify(uiterst,heel,Cat,Cat,_).
simplify(uitfasering,stop,Cat,Cat,_).
simplify('up-to-date',actueel,Cat,Cat,_).
simplify(voorts,ook,Cat,Cat,_).
simplify(wederrechtelijk,onwettig,Cat,Cat,_).

%% verb
simplify(actualiseer,werk_bij,Cat,Cat,_).
simplify(adstrueer,leg_uit,Cat,Cat,_).
simplify(ageer,treed_op,Cat,Cat,_).
simplify(behels,houd_in,Cat,Cat,_).
simplify(bemachtig,krijg,Cat,Cat,_).
simplify(communiceer,praat,Cat,Cat,_).
simplify(confirmeer,bevestig,Cat,Cat,_).
simplify(consulteer,raadpleeg,Cat,Cat,_).
simplify(continueer,zet_voort,Cat,Cat,_).
simplify(declameer,draag_voor,Cat,Cat,_).
simplify(deel_mede,zeg,Cat,Cat,_).
simplify(doneer,schenk,Cat,Cat,_).
simplify(dupeer,benadeel,Cat,Cat,_).
simplify(exporteer,voer_uit,Cat,Cat,_).
simplify(implementeer,voer_in,Cat,Cat,_).
simplify(importeer,voer_in,Cat,Cat,_).
simplify(incrimineer,beschuldig,Cat,Cat,_).
simplify(insinueer,suggereer,Cat,Cat,_).
simplify(intensiveer,versterk,Cat,Cat,_).
simplify(mitigeer,zwak_af,Cat,Cat,_).
simplify(ontbeer,mis,Cat,Cat,_).
simplify(optimaliseer,verbeter,Cat,Cat,_).
simplify(percipieer,neem_waar,Cat,Cat,_).
simplify(recapituleer,vat_samen,Cat,Cat,_).
simplify(reduceer,verminder,Cat,Cat,_).
simplify(sommeer,beveel,Cat,Cat,_).
simplify(speculaar,gok,Cat,Cat,_).
simplify(verifieer,controleer,Cat,Cat,_).
simplify(verklaar,zeg,Cat,Cat,_).
simplify(weifel,aarzel,Cat,Cat,_).




%% prep, comp
simplify(blijkens,volgens,Cat,Cat,_).
simplify(conform,volgens,Cat,Cat,_).
simplify(daar,omdat,Cat,Cat,comp).
simplify(inzake,over,Cat,Cat,_).
simplify(teneinde,om,cp,oti,_).

