:- module(alpino_classify_named_entity, [ classify_named_entity/6,
					  sents2c45/0
					]).

:- expects_dialect(sicstus).

:- use_module(library(lists)).
:- use_module(features).
:- use_module(alpino('src/utils')).
:- use_module(alpino('src/latin1')).

classify_named_entity(Name0,LC2,LC1,RC1,RC2,Cat) :-
    replace_comma(Name0,Name),
    select_features(Name,LC2,LC1,RC1,RC2,Features),
    hdrug_util:debug_message(3,"~w :",[Features]),
    evaluate(Features,Cat),
    hdrug_util:debug_message(3,"~w~n",[Cat]).

evaluate(Fields,Cat) :-
    findall(Score-Cat,evaluate(Fields,Score,Cat),List0),
    keysort(List0,List),
    select_best(List,Cat).

evaluate(Fields,Score,Cat) :-
    alpino_named_entity_features:category(Cat,Score0),
    evaluate(Fields,Cat,Score0,Score,0),
    hdrug_util:debug_message(3,"~w ~w~n",[Cat,Score]).

evaluate([],_Cat,Score,Score,_).
evaluate([H|T],Cat,Score0,Score,Counter) :-
    (   alpino_named_entity_features:feature(Counter,H,Cat,Score1),
        hdrug_util:debug_message(4,"~w ~w ~w ~w~n",[Counter,H,Cat,Score1])
    
    ->  Score2 is Score0+Score1
    ;   Score2 is Score0
    ),
    Counter1 is Counter + 1,
    evaluate(T,Cat,Score2,Score,Counter1).


select_features(Name,LC20,LC10,_RC1,_RC2,Features) :-
    split_atom(Name,Words0),
    append(Words0,['','','',''],Words1),
    replace_comma_list([LC20,LC10|Words1],[LC2,LC1|Words]),
    findall(Int-Val,feature(Int,Val,Words,Name,LC2,LC1), Features0),
    keysort(Features0,Features1),
    (   vls(Features1,Features)
    ->  true
    ;   format(user_error,"classify_named_entity: select_features fails~n",[]),
	format(user_error,"input was: ~w ~w ~w~n",[Name,LC20,LC10]),
	format(user_error,"result was: ~w~n",[Features1]),
	fail
    ).
	

vls([0-A,1-B,2-C,3-D,4-E,5-F,6-G,7-H,8-I,9-J,
     10-K,11-L,12-M,13-N,14-O,15-P,16-Q,17-R,18-S,19-T,
     20-U,21-V,22-W,23-X,24-Y,25-Z,26-A0,27-B0,28-C0,29-D0,
     30-E0,31-F0,32-G0,33-H0,34-I0,35-J0,36-K0,37-L0,38-M0,39-N0],
    [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,
     A0,B0,C0,D0,E0,F0,G0,H0,I0,J0,K0,L0,M0,N0]
   ).
     
split_atom(Atom,List) :-
    atom_codes(Atom,Codes),
    split_string(Codes," ",List0),
    atoms(List0,List).

atoms([],[]).
atoms([Hstr|Tstr],[H|T]) :-
    atom_codes(H,Hstr),
    atoms(Tstr,T).

feature(0,Val,_Words,_Name,_LC2,LC1) :-
    (   alpino_unknowns:decap(LC1,Val)
    ->  true
    ;   LC1=Val
    ).

feature(31,Val,_Words,_Name,LC2,_LC1) :-
    (   alpino_unknowns:decap(LC2,Val)
    ->  true
    ;   LC2=Val
    ).

feature(1,Val,Words,_Name,_,_) :-
    (   matches_year(Words)
    ->  Val=year
    ;   matches_num(Words)
    ->  Val=num
    ;   Val=''
    ).

feature(2,Val,Words,Name,_,_) :-
    (   dash_xY(Words)
    ->  Val= 'x-Y'
    ;   dash_Xy(Words)
    ->  Val= 'X-y'
    ;   sub_atom(Name,_,1,_,'-')
    ->  Val= 'X-Y'
    ;   Val=''
    ).

feature(3,Val,_Words,Name,_,_) :-
    (   sub_atom(Name,_,1,_,'\'')
    ->  Val= dash  % weird name!
    ;   Val= ''
    ).

feature(4,Val,_Words,Name,_,_) :-
    (   sub_atom(Name,_,1,_,'&')
    ->  Val= ampersand  % weird name!
    ;   Val= ''
    ).

feature(5,Val,Words,_Name,_,_) :-
    (   member(en,Words)
    ->  Val = en
    ;   Val = ''
    ).

feature(6,Val,Words,_Name,_,_) :-
    (   member(voor,Words)
    ->  Val = voor
    ;   Val = ''
    ).

feature(7,Val,_Words,Name,_,_) :-
    (   initiaal(Name)
    ->  Val= initiaal
    ;   Val= ''
    ).

feature(8,Val,_Words,Name,_,_) :-
    sub_atom(Name,_,1,0,Val).

feature(9,Val,_Words,Name,_,_) :-
    atom_length(Name,NameLen),
    (  NameLen < 2
    -> Len = NameLen
    ;  Len = 2
    ),
    sub_atom(Name,_,Len,0,Val).

feature(10,Val,_Words,Name,_,_) :-
    atom_length(Name,NameLen),
    (  NameLen < 5
    -> Len = NameLen
    ;  Len = 5
    ),
    sub_atom(Name,_,Len,0,Val).

feature(11,Val,_Words,Name,_,_) :-
    atom_length(Name,NameLen),
    (  NameLen < 4
    -> Len = NameLen
    ;  Len = 4
    ),
    sub_atom(Name,0,Len,_,Val).

feature(12,Val,_Words,Val,_,_).

feature(13,W,[W|_],_,_,_).
feature(14,W,[_,W|_],_,_,_).
feature(15,W,[_,_,W|_],_,_,_).
feature(16,W,[_,_,_,W|_],_,_,_).
feature(17,W,[_,_,_,_,W|_],_,_,_).

feature(18,Val,[W|_],_,_,_) :-
    caseval(W,Val).
feature(19,Val,[_,W|_],_,_,_) :-
    caseval(W,Val).
feature(20,Val,[_,_,W|_],_,_,_) :-
    caseval(W,Val).
feature(21,Val,[_,_,_,W|_],_,_,_) :-
    caseval(W,Val).
feature(22,Val,[_,_,_,_,W|_],_,_,_) :-
    caseval(W,Val).

feature(23,Val,[A,B,C,D,E|_],_,_,_) :-
    caseval(A,Aval),
    caseval(B,Bval),
    caseval(C,Cval),
    caseval(D,Dval),
    caseval(E,Eval),
    hdrug_util:concat_all([Aval,Bval,Cval,Dval,Eval],Val).

feature(24,Val,_Words,Name,_,_) :-
    (   matches_org(Name)
    ->  Val= org
    ;   Val= ''
    ).

feature(25,Val,_Words,Name,_,_) :-
    (   matches_loc(Name)
    ->  Val= loc
    ;   Val= ''
    ).

feature(26,Val,_Words,Name,_,_) :-
    (   matches_misc(Name)
    ->  Val= misc
    ;   Val= ''
    ).

feature(27,Val,_Words,_Name,_,LC) :-
    (   matches_per_lc(LC)
    ->  Val= per
    ;   Val= ''
    ).

feature(28,Val,_Words,_Name,_,LC) :-
    (   matches_org_lc(LC)
    ->  Val= org
    ;   Val= ''
    ).

feature(29,Val,_Words,_Name,_,LC) :-
    (   matches_loc_lc(LC)
    ->  Val= loc
    ;   Val= ''
    ).

feature(30,Val,_Words,_Name,_,LC) :-
    (   matches_misc_lc(LC)
    ->  Val= misc
    ;   Val= ''
    ).

feature(32,Val,_,Name,_,_) :-
    (   atom_codes(Name,[_|Codes]),
        all_consonants(Codes)
    ->  Val= novowels
    ;   Val= ''
    ).

feature(33,Val,_,Name,_,_) :-
    (   atom_length(Name,L),
        L > 1,
        alpino_unknowns:decap(Name,Name1),
        atom_codes(Name1,Codes),
        all_consonants(Codes)
    ->  Val= novowels
    ;   Val= ''
    ).

feature(34,Val,_,Name,_,_) :-
    atom_length(Name,L),
    (   L < 7
    ->  Val = short
    ;   L < 15
    ->  Val = medium
    ;   L < 25
    ->  Val = long
    ;   Val = verylong
    ).

feature(35,Val,_Words,Name,_,_) :-
    (   full_matches_org(Name)
    ->  Val= org
    ;   Val= ''
    ).

feature(36,Val,_Words,Name,_,_) :-
    (   full_matches_loc(Name)
    ->  Val= loc
    ;   Val= ''
    ).

feature(37,Val,_Words,Name,_,_) :-
    (   full_matches_misc(Name)
    ->  Val= misc
    ;   Val= ''
    ).

feature(38,Val,[Name|_],_,_,_) :-
    (   name_title(Name)
    ->  Val = title
    ;   Val = ''
    ).

feature(39,Val,Words,_,_,_) :-
    (   member(Word,Words),
	alpino_unknowns:decap(Word,LWord),
	alpino_unknowns:foreign_word(LWord)
    ->  Val = foreign
    ;   Val = ''
    ).


/*
this does not work at all
feature(40,Val,Words1,_,_,_) :-
    append(Words,['','','',''],Words1),
    findall(L,in_dict(Words,L),Ls0),
    sort(Ls0,Ls),
    hdrug_util:concat_all(Ls,Val,'_').

%% known as PER in dictionary
in_dict(Words,'PER') :-
    alpino_lex:lexicon_(proper_name(_,'PER'),_,Words,[],_,[]).

in_dict(Words,'ORG') :-
    alpino_lex:lexicon_(proper_name(_,'ORG'),_,Words,[],_,[]).

in_dict(Words,'LOC') :-
    alpino_lex:lexicon_(proper_name(_,'LOC'),_,Words,[],_,[]).

in_dict(Words,'MISC') :-
    alpino_lex:lexicon_(proper_name(_),_,Words,[],_,[]).

in_dict(Words,'MISC') :-
    alpino_lex:lexicon_(name_determiner(pron,'PER'),_,Words,[],_,[]).

in_dict([Word|Words],'MISC') :-
    alpino_lex:xl(Word,_,_,Words,[]).
*/

all_consonants([]).
all_consonants([H|T]) :-
    atom_codes(Cons,[H]),
    cns(Cons),
    all_consonants(T).

cns(b). cns(c). cns(d). cns(f). cns(g). cns(h). cns(j). cns(k).
cns(l). cns(m). cns(n). cns(p). cns(q). cns(r). cns(s). cns(t).
cns(v). cns(w). cns(x). cns(z). 

caseval('','') :- !.
caseval(Word,Code) :-
    (   alpino_unknowns:only_capitals(Word,_)
    ->  Code=u
    ;   only_lower(Word)
    ->  Code=l
    ;   cap_low(Word)
    ->  Code=b
    ;   Code=m
    ).

mac('De').
mac('Del').
mac('Mc').
mac('Le').
mac('Mac').

cap_low(Word) :-
    atom_concat(W1,W2,Word),
    up_low(W1),
    \+ mac(W1),
    up_low(W2).

cap_low(Word) :-
    atom_concat(W1,W2s,Word),
    up_low(W1),
    \+ mac(W1),
    atom_concat(W2,W3,W2s),
    up_low(W2),
    up_low(W3).

up_low(Atom) :-
    atom_codes(Atom,[C,C1|Codes]),
    isupper(C),
    all_lower([C1|Codes]).

all_lower([]).
all_lower([C|Cs]):-
    islower(C),
    all_lower(Cs).

only_lower(Atom) :-
    alpino_unknowns:decap(Atom,Atom).

initiaal(Name) :-
    sub_atom(Name,_,4,_,Sub),
    atom_codes(Sub,[A,46,32,D]),
    isupper(A),
    isupper(D).

dash_xY([Word|_]) :-
    atom_codes(Word,[F|Codes]),
    islower(F),
    append(_Begin,[D,E|_End],Codes),
    [D] = "-",
    isupper(E).

dash_Xy([Word|_]) :-
    atom_codes(Word,[F|Codes]),
    isupper(F),
    append(_Begin,[D,E|_End],Codes),
    [D] = "-",
    islower(E).

matches_year(List) :-
    member(Word,List),
    atom_codes(Word,Codes),
    append(_Begin,End,Codes),
    append(_,[A,B,C,D],End),
    isdigit(A),
    isdigit(B),
    isdigit(C),
    isdigit(D).

matches_num(List) :-
    member(Word,List),
    atom_codes(Word,Codes),
    member(A,Codes),
    isdigit(A).

matches_org(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    org(Org),
    sub_atom(Name,_,_,_,Org).

full_matches_org(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    org(Org),
    atom_concat(_,Org,Name).

org(academie).
org(academy).
org(administratie).
org(agency).
org(agentschap).
org(air).
org(airways).
org(alliantie).
org(associatie).
org(association).
org(automatisering).
org(aviation).
org(bank).
org(bedrijf).
org(bescherming).
org(bescherming).
org(beweging).
org(bond).
org(brigade).
org(bureau).
org(bureau).
org(business).
org('b.v.').
org('b.v').
org(bv).
org(capital).
org(cassatie).
org(center).
org(centrale).
org(centrum).
org(club).
org(college).
org(comit).
org(commissie).
org(commission).
org(companies).
org(company).
org(corporation).
org(corps).
org(council).
org(dagblad).
org(dienst).
org(dispuut).
org(divisi).
org(ensemble).
org(enterprise).
org(federatie).
org(federation).
org(fonds).
org(forum).
org(foundation).
org(front).
org(fund).
org(gasthuis).
org(gelegenheden).
org(gemeenschap).
org(genetics).
org(genootschap).
org(gezelschap).
org(groep).
org(group).
org(gymnasium).
org(hof).
org(holding).
org(hotels).
org(inc).
org(industrie).
org(institute).
org(instituut).
org(instrument).
org(internationa).
org(invest).
org(investment).
org(koninklijk).
org(korps).
org(kwartet).
org(leefbaar).
org(leger).
org(lloyd).
org(ltd).
org(lyceum).
org(maatschappij).
org(magazine).
org(media).
org(netwerk).
org(network).
org(news).
org(newspaper).
org('n.v.').
org('n.v').
org(nv).
org(omroep).
org(onderwijs).
org(onderzoek).
org(ontwikkel).
org(ontwikkeling).
org(orchest).
org(orchestra).
org(orde).
org(orgaan).
org(organisatie).
org(organisation).
org(orkest).
org(orkest).
org(overleg).
org(parlement).
org(partido).
org(partij).
org(party).
org(pharma).
org(philharm).
org(platform).
org(product).
org(quartet).
org(raad).
org(radio).
org(railway).
org(records).
org(redactie).
org(royale).
org(rundfunk).
org(school).
org(securities).
org(service).
org(shops).
org(society).
org(software).
org(spaarbank).
org(stichting).
org(systems).
org(team).
org(technologies).
org(technology).
org(telecom).
org(televisie).
org(toezicht).
org(trade).
org(trio).
org('uitg.').
org(uitgever).
org(unie).
org(unie).
org(universiteit).
org(university).
org(unlimited).
org(venture).
org(vereniging).
org(werken).
org(wetenschap).
org(zaken).

matches_loc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    loc(Org),
    sub_atom(Name,_,_,_,Org).

full_matches_loc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    loc(Org),
    atom_concat(_,Org,Name).

loc(airport).
loc(avenue).
loc(basiliek).
loc(berg).
loc(berg).
loc(center).
loc('col ').
loc(county).
loc(dreef).
loc(eiland).
loc(galerie).
loc(galery).
loc(gallerie).
loc(gallery).
loc(garden).
loc(hill).
loc(hospitaal).
loc(hotel).
loc(huis).
loc(kaai).
loc(kanaal).
loc(kapelle).
loc(kathedraal).
loc(kerk).
loc(kerke).
loc(laan).
loc(land).
loc(lei).
loc(luchthaven).
loc(maison).
loc(markt).
loc(mountain).
loc(museum).
loc(oceaan).
loc(paleis).
loc(pavillon).
loc(piazza).
loc(pic).
loc(plaza).
loc(plein).
loc(regio).
loc(reservaat).
loc(restaurant).
loc(rivier).
loc(route).
loc('rue ').
loc(singel).
loc(square).
loc(stadion).
loc(stadium).
loc(steeg).
loc(straat).
loc(territory).
loc(theater).
loc(theatre).
loc(village).
loc(ville).
loc(vliegveld).
loc(weg).
loc(zee).
loc(ziekenhuis).

matches_misc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    misc(Org),
    sub_atom(Name,_,_,_,Org).

full_matches_misc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    misc(Org),
    atom_concat(_,Org,Name).

misc(accoord).
misc(accoorden).
misc(akkoord).
misc(akkoorden).
misc(bachelor).
misc(championship).
misc(column).
misc(commentaar).
misc(conference).
misc(conferentie).
misc(disease).
misc(disorder).
misc(festival).
misc(pact).
misc(prijs).
misc(prijzen).
misc(show).
misc(toernooi).
misc(tournament).
misc(verdrag).
misc(verordening).
misc(wet).

matches_per_lc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    per_lc(Org),
    sub_atom(Name,_,_,0,Org).

per_lc('\'er').
per_lc(aanvoerder).
per_lc(acteur).
per_lc(activist).
per_lc(actrice).
per_lc(advocaat).
per_lc(advocate).
per_lc(amerikaan).
per_lc(architect).
per_lc(argentijn).
per_lc(arts).
per_lc(australiër).
per_lc(auteur).
per_lc(baas).
per_lc(beeldhouwer).
per_lc(belg).
per_lc(bestuurder).
per_lc(bevelhebber).
per_lc(bisschop).
per_lc(bouwvakker).
per_lc(braziliaan).
per_lc(broer).
per_lc(burgemeester).
per_lc(chef).
per_lc(cineast).
per_lc(coach).
per_lc(collega).
per_lc(commandant).
per_lc(commissaris).
per_lc(componist).
per_lc(couturier).
per_lc(coördinator).
per_lc(dictator).
per_lc(directeur).
per_lc(docent).
per_lc(dochter).
per_lc(doelman).
per_lc(dokter).
per_lc(dominee).
per_lc(duitser).
per_lc(echtpaar).
per_lc(engelsman).
per_lc(filosoof).
per_lc(fotograaf).
per_lc(fotograaf).
per_lc(fransman).
per_lc(fries).
per_lc(galeriehouder).
per_lc(gebroeders).
per_lc(gedeputeerde).
per_lc(generaal).
per_lc(geneticus).
per_lc(gezel).
per_lc(gouverneur).
per_lc(handelaar).
per_lc(historicus).
per_lc(historicus).
per_lc(hongaar).
per_lc(invaller).
per_lc(italiaan).
per_lc(jarige).
per_lc(kamerlid).
per_lc(kampioen).
per_lc(kanselier).
per_lc(keizer).
per_lc(koning).
per_lc(koningin).
per_lc(krijgsheer).
per_lc(kundige).
per_lc(kunstenaar).
per_lc(landgenoot).
per_lc(leider).
per_lc(leraar).
per_lc(lerares).
per_lc(lid).
per_lc(magnaat).
per_lc(manager).
per_lc(meester).
per_lc(meneer).
per_lc(mevrouw).
per_lc(minister).
per_lc(missionaris).
per_lc(moeder).
per_lc(nederlander).
per_lc(onderhandelaar).
per_lc(oom).
per_lc(oplichter).
per_lc(overste).
per_lc(paus).
per_lc(percussionist).
per_lc(pianist).
per_lc(premier).
per_lc(presentator).
per_lc(president).
per_lc(prins).
per_lc(prinses).
per_lc(producent).
per_lc(professor).
per_lc(psychiater).
per_lc(rechter).
per_lc(redacteur).
per_lc(regiseusse).
per_lc(regisseur).
per_lc(rivaal).
per_lc(saxofonist).
per_lc(schepen).
per_lc(schilder).
per_lc(schrijver).
per_lc(secretaris).
per_lc(senator).
per_lc(speler).
per_lc(tante).
per_lc(theatermaker).
per_lc(therapeut).
per_lc(topman).
per_lc(trainer).
per_lc(vader).
per_lc(verdediger).
per_lc(verloofde).
per_lc(verslaggeefster).
per_lc(verslaggever).
per_lc(voetballer).
per_lc(voorganger).
per_lc(voorzitter).
per_lc(vriend).
per_lc(vrouw).
per_lc(weduwe).
per_lc(wethouder).
per_lc(woordvoerder).
per_lc(woordvoerster).
per_lc(zakenman).
per_lc(zanger).
per_lc(zangeres).
per_lc(zoon).
per_lc(zus).
per_lc(zweed).

matches_org_lc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    org_lc(Org),
    sub_atom(Name,_,_,0,Org).

org_lc(aandeel).
org_lc(agency).
org_lc(agentschap).
org_lc(automatiseerder).
org_lc(bedrijf).
org_lc(beurs).
org_lc(blad).
org_lc(bond).
org_lc(bouwer).
org_lc(brouwer).
org_lc(bureau).
org_lc(centrale).
org_lc(club).
org_lc(coalitiepartner).
org_lc(commissie).
org_lc(concern).
org_lc(coörperatie).
org_lc(departement).
org_lc(dienst).
org_lc(exploitant).
org_lc(fabriek).
org_lc(fabrikant).
org_lc(federatie).
org_lc(fonds).
org_lc(gigant).
org_lc(groep).
org_lc(holding).
org_lc(institute).
org_lc(instituut).
org_lc(journal).
org_lc(keten).
org_lc(krant).
org_lc(maatschappij).
org_lc(marktleider).
org_lc(merk).
org_lc(netwerk).
org_lc(network).
org_lc(omroep).
org_lc(organisatie).
org_lc(orkest).
org_lc(partij).
org_lc(producent).
org_lc(projectontwikkelaar).
org_lc(stichting).
org_lc(team).
org_lc(tijdschrift).
org_lc(uitgever).
org_lc(uitgeverij).
org_lc(vakbond).
org_lc(vennootschap).
org_lc(vereniging).
org_lc(verzekeraar).
org_lc(waterschap).
org_lc(zender).

matches_loc_lc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    loc_lc(Org),
    sub_atom(Name,_,_,0,Org).

loc_lc(arrondissement).
loc_lc(bestemming).
loc_lc(bisdom).
loc_lc(bos).
loc_lc(buurland).
loc_lc(café).
loc_lc(camping).
loc_lc(deelgemeente).
loc_lc(deelstaat).
loc_lc(district).
loc_lc(dorp).
loc_lc(dorpje).
loc_lc(eiland).
loc_lc(galerie).
loc_lc(galery).
loc_lc(gallerie).
loc_lc(gallery).
loc_lc(gebied).
loc_lc(gemeente).
loc_lc(graafschap).
loc_lc(hartje).
loc_lc(kanaal).
loc_lc(kuuroord).
loc_lc(land).
loc_lc(luchthaven).
loc_lc(paviljoen).
loc_lc(plaats).
loc_lc(plein).
loc_lc(provincie).
loc_lc(regio).
loc_lc(republiek).
loc_lc(restaurant).
loc_lc(rivier).
loc_lc(staat).
loc_lc(stad).
loc_lc(stadje).
loc_lc(steden).
loc_lc(staten).
loc_lc(streek).
loc_lc(tehuis).
loc_lc(terrein).
loc_lc(theater).
loc_lc(theatre).
loc_lc(vliegbasis).
loc_lc(vliegveld).
loc_lc(voormalige).
loc_lc(wijk).
loc_lc(woonplaats).
loc_lc(woud).

matches_misc_lc(Name0) :-
    alpino_unknowns:decap(Name0,Name),
    misc_lc(Org),
    sub_atom(Name,_,_,0,Org).

misc_lc(agreement).
misc_lc(album).
misc_lc(begrip).
misc_lc(bijlage).
misc_lc(blad).
misc_lc(boek).
misc_lc(cd).
misc_lc(cyclus).
misc_lc(dans).
misc_lc(documentaire).
misc_lc(drama).
misc_lc(elpee).
misc_lc(festival).
misc_lc(film).
misc_lc(handvest).
misc_lc(komedie).
misc_lc(manifestatie).
misc_lc(operatie).
misc_lc(productie).
misc_lc(programma).
misc_lc(project).
misc_lc(reeks).
misc_lc(registratie).
misc_lc(reportage).
misc_lc(roman).
misc_lc(serie).
misc_lc(symfonie).
misc_lc(tentoonstelling).
misc_lc(term).
misc_lc(these).
misc_lc(tijdschrift).
misc_lc(toneelstuk).
misc_lc(vakblad).
misc_lc(voorstelling).
misc_lc(werk).
misc_lc(wet).
misc_lc(woord).

:- public sents2c45/0.
sents2c45 :-
    alpino_lex:lex_initialize,
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  !
    ;   split_string(Chars,"|",[CCat,CName,CLC2,CLC1,CRC1,CRC2]),
        atom_codes(Cat,CCat),
        atom_codes(Name0,CName),
        atom_codes(LC2,CLC2),
        atom_codes(LC1,CLC1),
        atom_codes(RC1,CRC1),
        atom_codes(RC2,CRC2),
	replace_comma(Name0,Name),
        select_features(Name,LC2,LC1,RC1,RC2,Features),
        formatc45(Features,Cat),
        fail
    ).


formatc45([],Cat) :-
    format("~w~n",[Cat]).
formatc45([H|T],Cat) :-
    format("~w,",[H]),
    formatc45(T,Cat).


replace_comma(',','COMMA') :-
    !.
replace_comma(Atom0,Atom) :-
    (   sub_atom(Atom0,_,1,_,',')
    ->  atom_codes(Atom0,Codes0),
	replace_comma_str(Codes0,Codes),
	atom_codes(Atom,Codes)
    ;   Atom0=Atom
    ).

replace_comma_str([],[]).
replace_comma_str([C0|T0],[C|T]) :-
    (   C0 == 44
    ->  C = 46
    ;   C0 = C
    ),
    replace_comma_str(T0,T).


replace_comma_list([],[]).
replace_comma_list([H0|T0],[H|T]) :-
    replace_comma(H0,H),
    replace_comma_list(T0,T).

%% increase this value, to allow multiple results
:- hdrug_util:initialize_flag(classify_names_threshold,0).

%% select_best(List,Cat).
select_best(List0,Cat) :-
    hdrug_util:hdrug_flag(classify_names_threshold,Th),
    reverse(List0,[Score-Cat0|List1]),
    select_best(List1,Score,Cat0,Cat,Th).

select_best(_,_,Cat,Cat,_).
select_best([Score1-Cat1|Tail],Score,_,Cat,Th) :-
    Score1 > Score - Th,
    select_best(Tail,Score,Cat1,Cat,Th).

name_title('dr.').
name_title('dhr.').
name_title('prof.').
name_title('prof.dr').
name_title('drs.').
name_title('ds.').
name_title('ing.').
name_title('ir.').
name_title('jhr.').
name_title('mej.').
name_title('mw.').
name_title('mevr.').
name_title('mgr.').
name_title('mr.').
name_title('mrs.').
name_title('wed.').   % weduwe

name_title('Dr.').
name_title('Dhr.').
name_title('Prof.').
name_title('Prof.dr').
name_title('Drs.').
name_title('Ds.').
name_title('Ing.').
name_title('Ir.').
name_title('Jhr.').
name_title('Mej.').
name_title('Mw.').
name_title('Mevr.').
name_title('Mgr.').
name_title('Mr.').
name_title('Mrs.').
name_title('Wed.').   % weduwe
