%% mwu_postag(lemma,surf,tags,newlemma)
mwu_postag('lang niet','lang niet',['ADJ(vrij,basis,zonder)','BW()'],[lang,niet]).
mwu_postag('lang niet','niet lang',['BW()','ADJ(vrij,basis,zonder)'],[niet,lang]).
mwu_postag('lang niet','niet langer',['BW()','ADJ(vrij,comp,zonder)'],[niet,lang]).
mwu_postag(voorzover,'voor zover',
	   ['VZ(init)','BW()'],
	   [voor,zover]).
mwu_postag('mogelijk zoveel',_,
	   ['TW(hoofd,vrij)','ADJ(vrij,basis,zonder)'],
	   [zoveel,mogelijk]).
mwu_postag('aantal een',_,
	   ['LID(onbep,stan,agr)','N(soort,ev,basis,onz,stan)'],
	   [een,aantal]).
mwu_postag('hard om','om het hardst',
	   ['VZ(init)','LID(bep,stan,evon)','ADJ(vrij,sup,zonder)'],
	   [om,het,hard]).


%% da 's
mwu_postag(v_root(ben,zijn),'da \'s',
	   ['VNW(aanw,pron,stan,vol,3o,ev)','WW(pv,tgw,ev)'],
	   [dat,v_root(ben,zijn)]).
mwu_postag('als het ware','als het ware',
	   ['VG(onder)','VNW(pers,pron,stan,red,3,ev,onz)','WW(pv,conj,ev)'],
	   [als,het,v_root(ben,zijn)]).

%% confusing, try both stem and surf
mwu_postag(_,Surf,Tags,Stem) :-
    mwu_postag(Surf,Tags,Stem).
mwu_postag(Stem0,_,Tags,Stem) :-
    mwu_postag(Stem0,Tags,Stem).
%% for vreemd only surf, because stem must be surf if vreemd
mwu_postag(_,Surf,Vreemd,Words) :-
    vreemd(Surf),
    alpino_util:split_atom(Surf," ",Words),
    vreemd(Words,Vreemd).
mwu_postag(Stem,Surf,Vreemd,Words) :-
    vreemd(Stem),
    alpino_util:split_atom(Surf," ",Words),
    vreemd(Words,Vreemd).

mwu_postag('me eigen',           ['VNW(pr,pron,obl,red,1,ev)','ADJ(nom,basis,zonder,zonder-n)'],[me,eigen]).
mwu_postag('mijn eigen',         ['VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[mijn,eigen]).
mwu_postag('m\'n eigen',         ['VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[mijn,eigen]).
mwu_postag('je eigen',           ['VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[je,eigen]).
mwu_postag('d\'r eigen',         ['VNW(pers,pron,obl,red,3v,getal,fem)','ADJ(nom,basis,zonder,zonder-n)'],[haar,eigen]).
mwu_postag('\'r eigen',          ['VNW(pers,pron,obl,red,3v,getal,fem)','ADJ(nom,basis,zonder,zonder-n)'],[haar,eigen]).
mwu_postag('haar eigen',         ['VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[haar,eigen]).
mwu_postag('zijn eigen',         ['VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[zijn,eigen]).
mwu_postag('z\'n eigen',         ['VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[zijn,eigen]).
mwu_postag('ons eigen',          ['VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)','ADJ(nom,basis,zonder,zonder-n)'],[ons,eigen]).
mwu_postag('jullie eigen',       ['VNW(bez,det,stan,nadr,2v,mv,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[jullie,eigen]).
mwu_postag('u eigen',            ['VNW(pers,pron,nomin,vol,2b,getal)','ADJ(nom,basis,zonder,zonder-n)'],[u,eigen]).
mwu_postag('uw eigen',           ['VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[u,eigen]).
mwu_postag('hun eigen',          ['VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)','ADJ(nom,basis,zonder,zonder-n)'],[hun,eigen]).

mwu_postag('100 meter vrije slag',['TW(hoofd,prenom,stan)','N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['100',meter,vrij,slag]).
mwu_postag('10 jaar Urbanus Live',['TW(hoofd,prenom,stan)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)','ADJ(vrij,basis,zonder)'],['10',jaar,'Urbanus',live]).
mwu_postag('17 jarige',['TW(hoofd,vrij)','ADJ(prenom,basis,met-e,stan)'],['17',jarig]).
mwu_postag('200 meter vrije slag',['TW(hoofd,prenom,stan)','N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['200',meter,vrij,slag]).
mwu_postag('2e Wereldoorlog',['SPEC(deeleigen)','SPEC(deeleigen)'],['2e','Wereld_oorlog']).
mwu_postag('4 x 100 meter vrije slag',['TW(hoofd,prenom,stan)','SPEC(symb)','TW(hoofd,prenom,stan)','N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['4',x,'100',meter,vrij,slag]).
mwu_postag('50 meter vrije slag',['TW(hoofd,prenom,stan)','N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['50',meter,vrij,slag]).
mwu_postag('aan banden',['VZ(init)','N(soort,mv,basis)'],[aan,band]).
mwu_postag('aan de bal',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[aan,de,bal]).
mwu_postag('aan de beurt',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[aan,de,beurt]).
mwu_postag('aan de gang',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[aan,de,gang]).
mwu_postag('aan de hand',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[aan,de,hand]).
mwu_postag('aan de hand van',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[aan,de,hand,van]).
mwu_postag('aan de vooravond van',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[aan,de,vooravond,van]).
mwu_postag('aan het',['VZ(init)','LID(bep,stan,evon)'],[aan,het]).
mwu_postag('aan het adres van',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[aan,het,adres,van]).
mwu_postag('aan het hoofd',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[aan,het,hoofd]).
mwu_postag('aan den lijve',['VZ(init)','LID(bep,dat,evmo)','N(soort,ev,basis,dat)'],[aan,de,lijf]).
mwu_postag('aan t',['VZ(init)','LID(bep,stan,evon)'],[aan,het]).
mwu_postag('aan toe',['VZ(fin)','VZ(fin)'],[aan,toe]).
mwu_postag('ach ja',['TSW()','TSW()'],[ach,ja]).
mwu_postag('achter de rug',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[achter,de,rug]).
mwu_postag('acht-uur Journaal',['SPEC(deeleigen)','N(eigen,ev,basis,onz,stan)'],[acht_uur,'Journaal']).
mwu_postag('ACHT uur Journaal',['TW(hoofd,vrij)','N(soort,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)'],[acht,uur,journaal]).
mwu_postag('Actieprogramma Duurzame Ontwikkeling',['N(soort,ev,basis,onz,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[actie_programma,duurzaam,ontwikkeling]).
mwu_postag('ad valorem\'-tarieven',['SPEC(vreemd)','N(soort,mv,basis)'],[ad,'advalorem-tarief']).
mwu_postag('af aan',['VZ(fin)','VZ(fin)'],[af,aan]).
mwu_postag('af en toe',['VZ(fin)','VG(neven)','VZ(fin)'],[af,en,toe]).
mwu_postag('Agenda 2000',['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)'],[agenda,'2000']).
mwu_postag('Agenda 2010',['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)'],[agenda,'2010']).
mwu_postag('Alcohol en de wet',['N(soort,ev,basis,zijd,stan)','VG(neven)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[alcohol,en,de,wet]).
mwu_postag('Alcohol Infolijn',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[alcohol,'Infolijn']).
mwu_postag('Algemeen Klassement',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[algemeen,klassement]).
mwu_postag('Algemeen Maatschappelijk Werk',['ADJ(prenom,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[algemeen,maatschappelijk,werk]).
mwu_postag('Algemene Kinderbijslagwet',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[algemeen,'Kinderbijslagwet']).
mwu_postag('Algemene Maatregel van Bestuur',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,onz,stan)'],[algemeen,maatregel,van,bestuur]).
mwu_postag('Algemene Ouderdomswet',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[algemeen,ouderdomswet]).
mwu_postag('Algemene Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[algemeen,zaak]).
mwu_postag('alleen dan',['BW()','BW()'],[alleen,dan]).
mwu_postag('alleen maar',['BW()','BW()'],[alleen,maar]).
mwu_postag('alleen niet',['BW()','BW()'],[niet,alleen]).
mwu_postag('alles behalve',['VNW(onbep,pron,stan,vol,3o,ev)','VG(onder)'],[alles,behalve]).
mwu_postag('al bij al',['BW()','VZ(init)','BW()'],[al,bij,al]).
mwu_postag('al dan niet',['BW()','BW()','BW()'],[al,dan,niet]).
mwu_postag('al met al',['BW()','VZ(init)','BW()'],[al,met,al]).
mwu_postag('al naargelang',['BW()','VG(onder)'],[al,naargelang]).
mwu_postag('al of niet',['BW()','VG(neven)','BW()'],[al,of,niet]).
mwu_postag('Al Qaeda-kopstuk',['SPEC(deeleigen)','N(soort,ev,basis,onz,stan)'],['Al','Qaeda_kopstuk']).
mwu_postag('Al Qaida-man',['SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['Al','Qaida_man']).
mwu_postag('als gevolg van',['VG(onder)','N(soort,ev,basis,onz,stan)','VZ(init)'],[als,gevolg,van]).
mwu_postag('als het ware',['VG(onder)','VNW(pers,pron,stan,red,3,ev,onz)','WW(pv,conj,ev)'],[als,het,zijn]).
mwu_postag('al te',['BW()','BW()'],[al,te]).
mwu_postag('Amerikaanse Burgeroorlog',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Amerikaans',burger_oorlog]).
mwu_postag('Amsterdamse School',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Amsterdams',school]).
mwu_postag('anabole steroïden',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[anabool,steroïde]).
mwu_postag('Andere Tijden',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[ander,tijd]).
mwu_postag('Anders GAan LEVen',['BW()','WW(inf,vrij,zonder)','WW(inf,vrij,zonder)'],[anders,gaan,leven]).
mwu_postag('a raison de',['SPEC(vreemd)','SPEC(vreemd)','SPEC(vreemd)'],[a,raison,de]).
mwu_postag('à raison de',['SPEC(vreemd)','SPEC(vreemd)','SPEC(vreemd)'],[à,raison,de]).
mwu_postag('a raison van',['SPEC(vreemd)','SPEC(vreemd)','VZ(init)'],[a,raison,van]).
mwu_postag('à raison van',['SPEC(vreemd)','SPEC(vreemd)','VZ(init)'],[à,raison,van]).
mwu_postag('art deco-stijl',['SPEC(vreemd)','N(soort,ev,basis,zijd,stan)'],[art,deco_stijl]).
mwu_postag('Artificiële Intelligentie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[artificieel,intelligentie]).
mwu_postag('Asterix en Obelix-strips',['N(eigen,ev,basis,zijd,stan)','VG(neven)','N(soort,mv,basis)'],['Asterix',en,'Obelix_strip']).
mwu_postag('As van het Kwaad',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[as,van,het,kwaad]).
mwu_postag('as van het Kwaad',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[as,van,het,kwaad]).
mwu_postag('As van het kwaad',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[as,van,het,kwaad]).
mwu_postag('As van het kwaad\'-toespraak',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[as,van,het,kwaad_toespraak]).
mwu_postag('avondje uit',['N(soort,ev,dim,onz,stan)','VZ(fin)'],[avond,uit]).
mwu_postag('Baldakijn van Bernini',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,zijd,stan)'],[baldakijn,van,'Bernini']).
mwu_postag('Basistakenpakket Jeugdgezondheidszorg',['N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)'],[basis_taak_pakket,jeugd_gezondheid_zorg]).
mwu_postag('Basistakenpakket JGZ',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[basis_taak_pakket,'JGZ']).
mwu_postag('Begeleid Individueel Studeren',['WW(vd,vrij,zonder)','ADJ(vrij,basis,zonder)','WW(inf,vrij,zonder)'],[begeleiden,individueel,studeren]).
mwu_postag('Belgische OS',['ADJ(prenom,basis,met-e,stan)','N(eigen,ev,basis,zijd,stan)'],['Belgisch','OS']).
mwu_postag('Belgisch Kampioenschap',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Belgisch',kampioenschap]).
mwu_postag('Belgisch-Nederlandse Conferentie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Belgisch-Nederlands',conferentie]).
mwu_postag('Belgisch Parlement',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Belgisch',parlement]).
mwu_postag('Belgisch Staatsblad',['ADJ(prenom,basis,zonder)','N(eigen,ev,basis,onz,stan)'],['Belgisch','Staatsblad']).
mwu_postag('Beloofde Land',['WW(vd,prenom,met-e)','N(soort,ev,basis,onz,stan)'],[beloven,land]).
mwu_postag('beslist niet',['ADJ(vrij,basis,zonder)','BW()'],[beslist,niet]).
mwu_postag('betaald voetbal',['WW(vd,prenom,zonder)','N(soort,ev,basis,onz,stan)'],[betalen,voetbal]).
mwu_postag('Beter Wonen',['ADJ(vrij,comp,zonder)','WW(inf,vrij,zonder)'],[goed,wonen]).
mwu_postag('Bezette Gebieden',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[bezet,gebied]).
mwu_postag('bij deze',['VZ(init)','VNW(aanw,det,stan,nom,met-e,zonder-n)'],[bij,deze]).
mwu_postag('bij dezen',['VZ(init)','VNW(aanw,det,dat,nom,met-e,zonder-n)'],[bij,deze]).
mwu_postag('bij het rechte eind',['VZ(init)','LID(bep,stan,evon)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[bij,het,recht,eind]).
mwu_postag('bij hoog en bij laag',['VZ(init)','ADJ(vrij,basis,zonder)','VG(neven)','VZ(init)','ADJ(vrij,basis,zonder)'],[bij,hoog,en,bij,laag]).
mwu_postag('bij lange',['VZ(init)','ADJ(prenom,basis,met-e,stan)'],[bij,lang]).
mwu_postag('bij lange na',['VZ(init)','ADJ(nom,basis,met-e,zonder-n,stan)','VZ(fin)'],[bij,lang,na]).
mwu_postag('bij machte',['VZ(init)','N(soort,ev,basis,dat)'],[bij,macht]).
mwu_postag('bij monde van',['VZ(init)','N(soort,ev,basis,dat)','VZ(init)'],[bij,mond,van]).
mwu_postag('bij tijd en wijle',['VZ(init)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[bij,tijd,en,wijle]).
mwu_postag('bij verre',['VZ(init)','BW()'],[bij,verre]).
mwu_postag('bij voorbaat',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[bij,voorbaat]).
mwu_postag('bij voorbeeld',['VZ(init)','N(soort,ev,basis,onz,stan)'],[bij,voorbeeld]).
mwu_postag('bij wijze van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[bij,wijze,van]).
mwu_postag('Binnenlandse Zaken en Koninkrijksrelaties',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,mv,basis)'],[binnenlands,zaak,en,koninkrijksrelatie]).
mwu_postag('Binnenlandse Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['binnenlands','zaak']).
mwu_postag('Binnenlandse zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['binnenlands','zaak']).
mwu_postag('binnenlandse zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['binnenlands','zaak']).
mwu_postag('binnenste buiten',['ADJ(nom,basis,met-e,zonder-n,stan)','VZ(fin)'],[binnenste,buiten]).
mwu_postag('Brussel-Hoofdstedelijk Gewest',['SPEC(deeleigen)','SPEC(deeleigen)'],['Brussel_Hoofdstedelijk','Gewest']).
mwu_postag('Brusselse Hoofdstedelijke Gewest',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],['Brussels',hoofdstedelijk,gewest]).
mwu_postag('Brusselse Hoofdstedelijke Regering',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Brussels',hoofdstedelijk,regering]).
mwu_postag('Brussels Hoofstedelijk Gewest',['ADJ(prenom,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Brussels',hoofdstedelijk,gewest]).
mwu_postag('Brussels Onthaalbureau Nieuwkomers',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],['Brussels',onthaalbureau,nieuwkomer]).
mwu_postag('Brussels Regeerakkoord',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Brussels',regeer_akkoord]).
mwu_postag('Brussels Zakboekje',['ADJ(prenom,basis,zonder)','N(soort,ev,dim,onz,stan)'],['Brussels',zakboek]).
mwu_postag('Bruto Nationaal Product',['ADJ(vrij,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[bruto,nationaal,product]).
mwu_postag('buiten kijf',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[buiten,kijf]).
mwu_postag('Buitenlands beleid',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[buitenlands,beleid]).
mwu_postag('Buitenlands Beleid',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[buitenlands,beleid]).
mwu_postag('Buitenlandse Handel',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[buitenlands,handel]).
mwu_postag('Buitenlandse hulp aan ontwikkelingslanden',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)'],[buitenlands,hulp,aan,ontwikkelingsland]).
mwu_postag('Buitenlandse Markten',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[buitenlands,markt]).
mwu_postag('Buitenlandse Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['buitenlands','zaak']).
mwu_postag('Buitenlandse zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['buitenlands','zaak']).
mwu_postag('buitenlandse zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['buitenlands','zaak']).
mwu_postag('business managers',['N(soort,ev,basis,zijd,stan)','N(soort,mv,basis)'],[business,manager]).
mwu_postag('Business Partner',['SPEC(vreemd)','N(soort,ev,basis,zijd,stan)'],['Business',partner]).
mwu_postag('Camp David-akkoorden',['SPEC(deeleigen)','N(soort,mv,basis)'],['Camp','David_akkoord']).
mwu_postag('CAO - onderhandelingen',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,mv,basis)'],['CAO',-,onderhandeling]).
mwu_postag('Caribisch Gebied',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Caribisch',gebied]).
mwu_postag('Casino van Knokke',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[casino,van,'Knokke']).
mwu_postag('Centraal Informatie Punt',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[centraal,informatie,punt]).
mwu_postag('Centraal Schriftelijk Eindexamen',['ADJ(prenom,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[centraal,schriftelijk,eind_examen]).
mwu_postag('Centraal Testamenten Register',['ADJ(prenom,basis,zonder)','N(soort,mv,basis)','N(soort,ev,basis,onz,stan)'],[centraal,testament,register]).
mwu_postag('Centra voor Werk en Inkomen',['N(soort,mv,basis)','VZ(init)','N(soort,ev,basis,onz,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[centrum,voor,werk,en,inkomen]).
mwu_postag('Chronologisch woordenboek',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[chronologisch,woord_boek]).
mwu_postag('Civiele Bescherming',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[civiel,bescherming]).
mwu_postag('Code Geel',['N(soort,ev,basis,zijd,stan)','ADJ(vrij,basis,zonder)'],[code,geel]).
mwu_postag('Code Oranje',['N(soort,ev,basis,zijd,stan)','ADJ(vrij,basis,zonder)'],[code,oranje]).
mwu_postag('cognitieve neurowetenschappen',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[cognitief,neurowetenschap]).
mwu_postag('Concilie van Clermont',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[concilie,van,'Clermont']).
mwu_postag('Concordaat van Worms',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[concordaat,van,'Worms']).
mwu_postag('Congres van Wenen',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[congres,van,'Wenen']).
mwu_postag('Consensus van Washington',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[consensus,van,'Washington']).
mwu_postag('Conservatieve Revolutie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[conservatief,revolutie]).
mwu_postag('Constitutioneel Verdrag',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Constitutioneel',verdrag]).
mwu_postag('consumer concerns',['SPEC(vreemd)','N(soort,mv,basis)'],[consumer,concern]).
mwu_postag('Convenant Duurzaam Bouwen',['N(soort,ev,basis,onz,stan)','ADJ(vrij,basis,zonder)','WW(inf,vrij,zonder)'],[convenant,duurzaam,bouwen]).
mwu_postag('Conventie van Geneve',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[conventie,van,'Geneve']).
mwu_postag('Conventie van Genève',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[conventie,van,'Genève']).
mwu_postag('Conventie van Oslo',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[conventie,van,'Oslo']).
mwu_postag('Crittenden-Johnson Resolutie',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],['Crittenden_Johnson',resolutie]).
mwu_postag('Culturele Hoofdstad',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[cultureel,hoofdstad]).
mwu_postag('Cultuurcentrum Mechelen',['SPEC(deeleigen)','SPEC(deeleigen)'],['Cultuur_centrum','Mechelen']).
mwu_postag('curriculum vitae\'s',['N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[curriculum,vitae]).
mwu_postag('Dag der Geliefden',['N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(soort,mv,basis)'],[dag,de,geliefde]).
mwu_postag('Dagelijks Brood',['ADJ(vrij,basis,zonder)','N(soort,ev,basis,onz,stan)'],[dagelijks,'Brood']).
mwu_postag('dag in dag uit',['N(soort,ev,basis,zijd,stan)','VZ(fin)','N(soort,ev,basis,zijd,stan)','VZ(fin)'],[dag,in,dag,uit]).
mwu_postag('dag in , dag uit',['N(soort,ev,basis,zijd,stan)','VZ(fin)','LET()','N(soort,ev,basis,zijd,stan)','VZ(fin)'],[dag,in,',',dag,uit]).
mwu_postag('Dag van de Geliefden',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[dag,van,de,geliefde]).
mwu_postag('Dag van de Student',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[dag,van,de,student]).
mwu_postag('Dag voor Internet-veiligheid',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[dag,voor,internet_veiligheid]).
mwu_postag('dagje uit',['N(soort,ev,dim,onz,stan)','VZ(fin)'],[dag,uit]).
mwu_postag('dank je wel',['WW(pv,tgw,ev)','VNW(pr,pron,obl,red,2v,getal)','BW()'],[danken,je,wel]).
mwu_postag(dankzij,['N(soort,ev,basis,zijd,stan)','WW(pv,conj,ev)'],[dank,zijn]).
mwu_postag('dan ook',['BW()','BW()'],[dan,ook]).
mwu_postag('dat soort',['VNW(aanw,det,stan,prenom,zonder,evon)','N(soort,ev,basis,onz,stan)'],[dat,soort]).
mwu_postag('dan wel',['BW()','BW()'],[dan,wel]).
mwu_postag('dat wel',['VNW(aanw,pron,stan,vol,3o,ev)','BW()'],[dat,wel]).
mwu_postag('dat wil zeggen',['VNW(aanw,pron,stan,vol,3o,ev)','WW(pv,tgw,ev)','WW(inf,vrij,zonder)'],[dat,willen,zeggen]).
mwu_postag('Davis Cup-team',['SPEC(deeleigen)','N(eigen,ev,basis,onz,stan)'],['Davis','Cup_team']).
mwu_postag('degelijk wel',['BW()','ADJ(vrij,basis,zonder)'],[wel,degelijk]).
mwu_postag('De Dag',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,dag]).
mwu_postag('de eerste de beste',['LID(bep,stan,rest)','TW(rang,prenom,stan)','LID(bep,stan,rest)','ADJ(prenom,sup,met-e,stan)'],[de,één,de,goed]).
mwu_postag('de ene na de andere',['LID(bep,stan,rest)','VNW(onbep,det,stan,prenom,met-e,evz)','VZ(init)','LID(bep,stan,rest)','ADJ(nom,basis,met-e,zonder-n,stan)'],[de,een,na,de,ander]).
mwu_postag('Deposito- en Consignatiekas',['SPEC(afgebr)','VG(neven)','N(eigen,ev,basis,zijd,stan)'],['Deposito-',en,'Consignatiekas']).
mwu_postag('Derde Republiek',['TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[drie,republiek]).
mwu_postag('Derde Wereldlanden',['TW(rang,prenom,stan)','N(soort,mv,basis)'],[drie,'Wereldland']).
mwu_postag('de Rechtvaardige rechters',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[de,rechtvaardig,rechter]).
mwu_postag('De Rechtvaardige Rechters',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[de,rechtvaardig,rechter]).
mwu_postag('De Regenboogprinses',['LID(bep,stan,rest)','N(eigen,ev,basis,zijd,stan)'],[de,'Regenboogprinses']).
mwu_postag('De reinigende Weg',['LID(bep,stan,rest)','WW(od,prenom,met-e)','BW()'],[de,reinigen,weg]).
mwu_postag('De ringelingschat',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,ringelingschat]).
mwu_postag('der Nederlanden',['LID(bep,gen,rest3)','N(eigen,mv,basis)'],[de,'Nederland']).
mwu_postag('de ronde',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,ronde]).
mwu_postag('De Rode Ridder',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,rood,ridder]).
mwu_postag('De roze rimpel',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,roze,rimpel]).
mwu_postag('De Russische muziek',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,'Russisch',muziek]).
mwu_postag('des avonds',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,avond]).
mwu_postag('des te',['LID(bep,gen,evmo)','VZ(init)'],[de,te]).
mwu_postag('De schat van Beersel',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[de,schat,van,'Beersel']).
mwu_postag('De Scheepvaart',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,scheepvaart]).
mwu_postag('De Schreeuw',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,schreeuw]).
mwu_postag('De Schriftgeleerden',['LID(bep,stan,rest)','N(soort,mv,basis)'],[de,schriftgeleerde]).
mwu_postag('De Seefhoek',['LID(bep,stan,rest)','N(eigen,ev,basis,zijd,stan)'],[de,'Seefhoek']).
mwu_postag('de Sint-Michiels en Sint-Goedele katheraal',['LID(bep,stan,rest)','N(eigen,ev,basis,zijd,stan)','VG(neven)','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[de,'Sint-Michiels',en,'Sint-Goedele',kathedraal]).
mwu_postag('de Spaanse Nederlanden',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(eigen,mv,basis)'],[de,'Spaans','Nederland']).
mwu_postag('de Stedendwinger',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,stedendwinger]).
mwu_postag('De Steen',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,'Steen']).
mwu_postag('De Stem van de Arabieren',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(eigen,mv,basis)'],[de,stem,van,de,'Arabier']).
mwu_postag('De stervende Christus',['LID(bep,stan,rest)','WW(od,prenom,met-e)','N(eigen,ev,basis,zijd,stan)'],[de,sterven,'Christus']).
mwu_postag('De Stomme van Portici',['LID(bep,stan,rest)','ADJ(nom,basis,met-e,zonder-n,stan)','VZ(init)','N(eigen,ev,basis,zijd,stan)'],[de,'Stomme',van,'Portici']).
mwu_postag('de stoutmoedige diefte',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,stoutmoedig,diefte]).
mwu_postag('De strijkster',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,strijkster]).
mwu_postag('De student',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,student]).
mwu_postag('De Student',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,student]).
mwu_postag('de stuipen op het lijf',['LID(bep,stan,rest)','N(soort,mv,basis)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[de,stuip,op,het,lijf]).
mwu_postag('De Tweeling',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,tweeling]).
mwu_postag('De Val',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[de,val]).
mwu_postag('De Val van de Goden',['LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(eigen,mv,basis)'],[de,val,van,de,'God']).
mwu_postag('Deventer Boekenmarkt',['N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)'],['Deventer',boekenmarkt]).
mwu_postag('De Veste-kerk',['SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['De','Veste_kerk']).
mwu_postag('De Vier Winden',['LID(bep,stan,rest)','TW(hoofd,prenom,stan)','N(soort,mv,basis)'],[de,vier,wind]).
mwu_postag('De Vlaamse Vlagge',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,'Vlaams','Vlagge']).
mwu_postag('de Witte Waan',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,wit,waan]).
mwu_postag('De Witte Waan',['LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[de,wit,waan]).
mwu_postag('De Zevende Dag',['LID(bep,stan,rest)','TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[de,zeven,dag]).
mwu_postag('De Zevende Hemel',['LID(bep,stan,rest)','TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[de,zeven,hemel]).
mwu_postag('dichtst bij',['ADJ(vrij,sup,zonder)','VZ(init)'],[dicht,bij]).
mwu_postag('dichter bij',['ADJ(vrij,comp,zonder)','VZ(init)'],[dicht,bij]).
mwu_postag('dicht bij',['ADJ(vrij,basis,zonder)','VZ(init)'],[dicht,bij]).
mwu_postag('die van',['VZ(init)','VNW(aanw,det,stan,prenom,zonder,rest)'],[van,die]).
mwu_postag('die paar',['VNW(aanw,det,stan,prenom,zonder,rest)','N(soort,ev,basis,onz,stan)'],[die,paar]).
mwu_postag('Dienst van Schrift en Tafel',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[dienst,van,schrift,en,tafel]).
mwu_postag('Dietsche Militie-Zwarte Brigade',['ADJ(prenom,basis,met-e,stan)','SPEC(deeleigen)','SPEC(deeleigen)'],['Dietsch','Militie-Zwarte','Brigade']).
mwu_postag('Digitaal Actieplan Vlaanderen',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[digitaal,actieplan,'Vlaanderen']).
mwu_postag('Digitale Bibliotheek voor de Nederlandse Letteren',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[digitaal,bibliotheek,voor,de,'Nederlands',letteren]).
mwu_postag('dit keer',['VNW(aanw,det,stan,prenom,zonder,evon)','N(soort,ev,basis,onz,stan)'],[dit,keer]).
mwu_postag('dit soort',['VNW(aanw,det,stan,prenom,zonder,evon)','N(soort,ev,basis,onz,stan)'],[dit,soort]).
mwu_postag('DJ Tiësto',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[dj,'Tiësto']).
mwu_postag('Doha Conferentie',['N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)'],['Doha',conferentie]).
mwu_postag('Doha Ministeriële Conferentie',['N(eigen,ev,basis,onz,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Doha',ministerieel,conferentie]).
mwu_postag('door de knieen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[door,de,knie]).
mwu_postag('door de knieën',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[door,de,knie]).
mwu_postag('door en door',['VZ(fin)','VG(neven)','VZ(fin)'],[door,en,door]).
mwu_postag('door het stof',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[door,het,stof]).
mwu_postag('door middel van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[door,middel,van]).
mwu_postag('door toedoen van',['VZ(init)','WW(inf,nom,zonder,zonder-n)','VZ(init)'],[door,toe_doen,van]).
mwu_postag('Douane 2000"',['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)'],[douane,'2000']). % "
mwu_postag('Douane 2002',['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)'],[douane,'2002']).
mwu_postag('DRANK - EN HORECAWET',['N(soort,ev,basis,zijd,stan)','LET()','VG(neven)','N(soort,ev,basis,zijd,stan)'],[drank,-,en,horeca_wet]).
mwu_postag('Drank- en Horecawet',['SPEC(afgebr)','VG(neven)','N(soort,ev,basis,zijd,stan)'],['Drank-',en,horeca_wet]).
mwu_postag('dr. Henri Van der Hoeven-kliniek',['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)'],['dr.','Henri','Van',der,'Hoeven_kliniek']).
mwu_postag('Drugs Infolijn',['N(soort,mv,basis)','N(eigen,ev,basis,zijd,stan)'],[drug,'Infolijn']).
mwu_postag('Duitse Rijk',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],['Duits',rijk]).
mwu_postag('Duurzame Ontwikkeling',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[duurzaam,ontwikkeling]).
mwu_postag('Economie , Wetenschap en Innovatie ( EWI )',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)','LET()','N(eigen,ev,basis,genus,stan)','LET()'],[economie,',',wetenschap,en,innovatie,'(','EWI',')']).
mwu_postag('Economische Partnerschap Akkoorden',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[economisch,partnerschap,akkoord]).
mwu_postag('Economische Partnerschapovereenkomsten',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[economisch,partnerschapovereenkomst]).
mwu_postag('Economische Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[economisch,zaak]).
mwu_postag('Economische Zaken en Werkgelegenheid',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[economisch,zaak,en,werkgelegenheid]).
mwu_postag('Economisch Partnerschap Akkoorden',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[economisch,partnerschap,akkoord]).
mwu_postag('een en ander',['TW(hoofd,nom,zonder-n,basis)','VG(neven)','ADJ(nom,basis,zonder,zonder-n)'],[één,en,ander]).
mwu_postag('één en ander',['TW(hoofd,nom,zonder-n,basis)','VG(neven)','ADJ(nom,basis,zonder,zonder-n)'],[één,en,ander]).
mwu_postag('een handje',['LID(onbep,stan,agr)','N(soort,ev,dim,onz,stan)'],[een,hand]).
mwu_postag('een ieder',['LID(onbep,stan,agr)','VNW(onbep,det,stan,vrij,zonder)'],[een,ieder]).
mwu_postag('een kleine',['LID(onbep,stan,agr)','ADJ(prenom,basis,met-e,stan)'],[een,klein]).
mwu_postag('een loopje',['LID(onbep,stan,agr)','N(soort,ev,dim,onz,stan)'],[een,loop]).
mwu_postag('Eenmalige Bevrijdende Aangifte',['ADJ(prenom,basis,met-e,stan)','WW(od,prenom,met-e)','N(soort,ev,basis,zijd,stan)'],[eenmalig,bevrijden,aangifte]).
mwu_postag('een na',['TW(hoofd,vrij)','VZ(fin)'],[één,na]).
mwu_postag('een of andere',['TW(hoofd,prenom,stan)','VG(neven)','ADJ(prenom,basis,met-e,stan)'],[één,of,ander]).
mwu_postag('één of andere',['TW(hoofd,prenom,stan)','VG(neven)','ADJ(prenom,basis,met-e,stan)'],[één,of,ander]).
mwu_postag('een of ander',['TW(hoofd,prenom,stan)','VG(neven)','ADJ(prenom,basis,zonder)'],[één,of,ander]).
mwu_postag('een oogje',['LID(onbep,stan,agr)','N(soort,ev,dim,onz,stan)'],[een,oog]).
mwu_postag('een paar',['LID(onbep,stan,agr)','N(soort,ev,basis,onz,stan)'],[een,paar]).
mwu_postag('een slordige',['LID(onbep,stan,agr)','ADJ(prenom,basis,met-e,stan)'],[een,slordig]).
mwu_postag('een soort',['LID(onbep,stan,agr)','N(soort,ev,basis,genus,stan)'],[een,soort]).
mwu_postag('één voor één',['TW(hoofd,prenom,stan)','VZ(init)','TW(hoofd,prenom,stan)'],[één,voor,één]).
mwu_postag('een weinig',['LID(onbep,stan,agr)','VNW(onbep,grad,stan,vrij,zonder,basis)'],[een,weinig]).
mwu_postag('een zulk',['LID(onbep,stan,agr)','VNW(aanw,det,stan,vrij,zonder)'],[zulk,een]).
mwu_postag('eens en vooral',['BW()','VG(neven)','BW()'],[eens,en,vooral]).
mwu_postag('eens niet',['BW()','BW()'],[niet,eens]).
mwu_postag('Eerste en Tweede Kamer',['TW(rang,prenom,stan)','VG(neven)','TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[één,en,twee,kamer]).
mwu_postag('Eerste Kamerlid',['TW(rang,prenom,stan)','N(soort,ev,basis,onz,stan)'],[één,kamer_lid]).
mwu_postag('Eerste Kerstavond',['TW(rang,prenom,stan)','N(eigen,ev,basis,zijd,stan)'],[één,'Kerstavond']).
mwu_postag('eerste klas',['TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[één,klas]).
mwu_postag('Eerste Slag',['TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[één,slag]).
mwu_postag('Eerste Wereldoorlogde',['TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[één,wereldoorlog]).
mwu_postag('Eigen Verklaring',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],[eigen,verklaring]).
mwu_postag('El Greco-figuur',['SPEC(deeleigen)','N(soort,ev,basis,onz,stan)'],['El','Greco_figuur']).
mwu_postag('Emancipatie Proclamatie',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[emancipatie,proclamatie]).
mwu_postag('een weinig',['LID(onbep,stan,agr)','VNW(onbep,grad,stan,vrij,zonder,basis)'],[een,weinig]).
mwu_postag('en dergelijke',['VG(neven)','ADJ(nom,basis,met-e,zonder-n,stan)'],[en,dergelijk]).
mwu_postag('en jong oud',['ADJ(vrij,basis,zonder)','VG(neven)','ADJ(vrij,basis,zonder)'],[jong,en,oud]).
mwu_postag('Energie Onderzoekscentrum Nederland',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[energie,onderzoekscentrum,'Nederland']).
mwu_postag('Energie Premie Regeling',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[energie,premie,regeling]).
mwu_postag('Energie Prestatie Advies',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[energie,prestatie,advies]).
mwu_postag('Energie Prestatie Coëfficiënt',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[energie,prestatie,coëfficiënt]).
mwu_postag('Energie Prestatie Norm',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[energie,prestatie,norm]).
mwu_postag('en ga zo maar door',['VG(neven)','WW(pv,tgw,ev)','BW()','BW()','VZ(fin)'],[en,gaan,zo,maar,door]).
mwu_postag('en of',['VG(neven)','VG(neven)'],[en,of]).
mwu_postag('en / of',['VG(neven)','LET()','VG(neven)'],[en,'/',of]).
mwu_postag('en wat dies meer zij',['VG(neven)','VNW(vb,pron,stan,vol,3o,ev)','VNW(aanw,pron,gen,vol,3o,ev)','VNW(onbep,grad,stan,vrij,zonder,comp)','WW(pv,conj,ev)'],[en,wat,die,veel,zijn]).
mwu_postag('en zo meer',['VG(neven)','BW()','VNW(onbep,grad,stan,vrij,zonder,comp)'],[en,zo,veel]).
mwu_postag('Epilepsie Infolijn',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[epilepsie,'Infolijn']).
mwu_postag('Epilepsie Zelfzorgboek',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[epilepsie,zelfzorgboek]).
mwu_postag('Europa Cup',['SPEC(deeleigen)','SPEC(deeleigen)'],['Europa','Cup']).
mwu_postag('Europees Stelsel der Rekeningen',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','LID(bep,gen,rest3)','N(soort,mv,basis)'],['Europees',stelsel,de,rekening]).
mwu_postag('Europees Veiligheids en Defensiebeleid',['ADJ(prenom,basis,zonder)','SPEC(afgebr)','VG(neven)','N(soort,ev,basis,onz,stan)'],['Europees','Veiligheids',en,defensiebeleid]).
mwu_postag('Europees Veiligheids- en Defensiebeleid',['ADJ(prenom,basis,zonder)','SPEC(afgebr)','VG(neven)','N(soort,ev,basis,onz,stan)'],['Europees','Veiligheids-',en,defensiebeleid]).
mwu_postag('Europe Publicaties',['SPEC(vreemd)','N(soort,mv,basis)'],['Europe',publicatie]).
mwu_postag('Europese Commissaris voor Ontwikkelingssamenwerking',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],['Europees',commissaris,voor,ontwikkelingssamenwerking]).
mwu_postag('Europese Commissie-voorzitter',['SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)'],['Europese','Commissie_voorzitter']).
mwu_postag('Europese Handvest voor Regionale en Minderheidstalen',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','VG(neven)','N(soort,mv,basis)'],['Europees',handvest,voor,regionaal,en,minderheidstaal]).
mwu_postag('Europese Strategie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Europees',strategie]).
mwu_postag('Europese Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['Europees',zaak]).
mwu_postag('Exclusieve Belgische Economische Zone',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[exclusief,'Belgisch',economisch,zone]).
mwu_postag('Exclusieve Economische Zone',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[exclusief,economisch,zone]).
mwu_postag('Externe Communicatie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[extern,communicatie]).
mwu_postag('Externe Hulp',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[extern,hulp]).
mwu_postag('Extern Optreden',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[extern,op_treden]).
mwu_postag('ex-Vlaams Blok',['ADJ(prenom,basis,zonder)','N(eigen,ev,basis,onz,stan)'],[ex_Vlaams,'Blok']).
mwu_postag('ezeltje prik',['N(soort,ev,dim,onz,stan)','N(soort,ev,basis,zijd,stan)'],[ezel,prik]).
mwu_postag('Farao Feros',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[farao,'Feros']).
mwu_postag('Farm Frites-ploeg',['SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['Farm','Frites_ploeg']).
mwu_postag('Federaal Plan',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[federaal,plan]).
mwu_postag('Federaal Plan voor Duurzame Ontwikkeling',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[federaal,plan,voor,duurzaam,ontwikkeling]).
mwu_postag('Federale Ministerraad',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[federaal,ministerraad]).
mwu_postag('Federale Overheidsdienst ( FOD ) Volksgezondheid , Veiligheid van de Voedselketen en Leefmilieu',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','LET()','N(eigen,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[federaal,overheidsdienst,'(','FOD',')',volksgezondheid,',',veiligheid,van,de,voedselketen,en,leefmilieu]).
mwu_postag('Federale Wetenschapsbeleid',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[federaal,wetenschapsbeleid]).
mwu_postag('Festival van Vlaanderen',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[festival,van,'Vlaanderen']).
mwu_postag('Festival VSB Poezieprijs',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[festival,'VSB',poezieprijs]).
mwu_postag('FIFA Wereldvoetballer van het Jaar',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],['FIFA',wereldvoetballer,van,het,jaar]).
mwu_postag('Film in Vlaanderen',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[film,in,'Vlaanderen']).
mwu_postag('Firma Vynckier',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[firma,'Vynckier']).
mwu_postag('FOD Economie',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],['FOD',economie]).
mwu_postag('FOD Justitie',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],['FOD',justitie]).
mwu_postag('FOD Sociale zekerheid',['N(eigen,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['FOD',sociaal,zekerheid]).
mwu_postag('FOD Sociale Zekerheid',['N(eigen,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['FOD',sociaal,zekerheid]).
mwu_postag('FOD Volksgezondheid',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],['FOD',volksgezondheid]).
mwu_postag('Foetaal Alcoholsyndroom',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[foetaal,alcoholsyndroom]).
mwu_postag('Fonds voor Arbeidsongevallen',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,mv,basis)'],[fonds,voor,arbeidsongeval]).
mwu_postag('Fonds voor de Podiumkunsten',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[fonds,voor,de,podiumkunst]).
mwu_postag('frank en vrij',['ADJ(vrij,basis,zonder)','VG(neven)','ADJ(vrij,basis,zonder)'],[frank,en,vrij]).
mwu_postag('Frans-Belgische Gebarentaal',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Frans-Belgisch',gebarentaal]).
mwu_postag('Franse Westhoek',['ADJ(prenom,basis,met-e,stan)','N(eigen,ev,basis,zijd,stan)'],['Frans','Westhoek']).
mwu_postag('Gebroeders van Eyck',['N(soort,mv,basis)','SPEC(deeleigen)','SPEC(deeleigen)'],[gebroeders,van,'Eyck']).
mwu_postag('geen doekjes',['VNW(onbep,det,stan,prenom,zonder,agr)','N(soort,mv,dim)'],[geen,doek]).
mwu_postag('geen een',['VNW(onbep,det,stan,prenom,zonder,agr)','TW(hoofd,nom,zonder-n,basis)'],[geen,één]).
mwu_postag('geen van allen',['VNW(onbep,det,stan,prenom,zonder,agr)','VZ(init)','VNW(onbep,det,stan,nom,met-e,mv-n)'],[geen,van,al]).
mwu_postag('geen van beiden',['VNW(onbep,det,stan,prenom,zonder,agr)','VZ(init)','VNW(onbep,grad,stan,nom,met-e,mv-n,basis)'],[geen,van,beide]).
mwu_postag('geheime dienst',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[geheim,dienst]).
mwu_postag('geleidelijk aan',['ADJ(vrij,basis,zonder)','VZ(fin)'],[geleidelijk,aan]).
mwu_postag('Gelijke Kansen',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[gelijk,kans]).
mwu_postag('gelukkig maar',['ADJ(vrij,basis,zonder)','BW()'],[gelukkig,maar]).
mwu_postag('Gemeenschappelijke Marktordening',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[gemeenschappelijk,marktordening]).
mwu_postag('Gemeenschappelijk Landbouwbeleid',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[gemeenschappelijk,landbouw_beleid]).
mwu_postag('Gemeenschappelijk Leidraad',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],[gemeenschappelijk,leidraad]).
mwu_postag('Gemeenschapsminister voor Cultuur',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[gemeenschapsminister,voor,cultuur]).
mwu_postag('Gemeentelijk Cultuurbeleid - een handleiding',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','LET()','LID(onbep,stan,agr)','N(soort,ev,basis,zijd,stan)'],[gemeentelijk,cultuurbeleid,-,een,handleiding]).
mwu_postag('Gemeentelijke Basisadministratie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[gemeentelijk,basisadministratie]).
mwu_postag('Gemeente Sluis',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)'],[gemeente,'Sluis']).
mwu_postag('Generaal Bernadotte',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[generaal,'Bernadotte']).
mwu_postag('Generaal Chassé',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[generaal,'Chassé']).
mwu_postag('Genie Compagnie',['N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)'],[genie,compagnie]).
mwu_postag('Gerechtelijk Wetboek',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[gerechtelijk,wetboek]).
mwu_postag('Gewestelijk Ontwikkelingsplan',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[gewestelijk,ontwikkelingsplan]).
mwu_postag('God de Vader',['N(eigen,ev,basis,zijd,stan)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],['God',de,vader]).
mwu_postag('God de Zoon',['N(eigen,ev,basis,zijd,stan)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],['God',de,zoon]).
mwu_postag('God wil het',['N(eigen,ev,basis,zijd,stan)','WW(pv,tgw,ev)','VNW(pers,pron,stan,red,3,ev,onz)'],['God',willen,het]).
mwu_postag('God zij dank',['N(eigen,ev,basis,zijd,stan)','WW(pv,conj,ev)','N(soort,ev,basis,zijd,stan)'],['God',zijn,dank]).
mwu_postag('goeden avond',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[goed,avond]).
mwu_postag('Goeden avond',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[goed,avond]).
mwu_postag('Goede Tijden , Slechte Tijden',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','LET()','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[goed,tijd,',',slecht,tijd]).
mwu_postag('Goede Vrijdag-akkoord',['SPEC(deeleigen)','N(soort,ev,basis,onz,stan)'],['Goede','Vrijdag_akkoord']).
mwu_postag('Gold Experience-materiaal',['SPEC(deeleigen)','N(soort,ev,basis,onz,stan)'],['Gold','Experience_materiaal']).
mwu_postag('Gouden Bal',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],[gouden,bal]).
mwu_postag('Gouden Oogst',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],['Gouden',oogst]).
mwu_postag('Gouden Roos 2000',['ADJ(prenom,basis,zonder)','SPEC(deeleigen)','TW(hoofd,vrij)'],[gouden,'Roos','2000']).
mwu_postag('Graaf de Ferraris-gebouw',['SPEC(deeleigen)','SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['Graaf',de,ferraris_gebouw]).
mwu_postag('Graafschap Vroenhof',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[graafschap,'Vroenhof']).
mwu_postag('Graaf van Vlaanderen',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[graaf,van,'Vlaanderen']).
mwu_postag('Grand Slam-titels',['SPEC(deeleigen)','N(soort,mv,basis)'],['Grand','Slam_titel']).
mwu_postag('Grand Slam-toernooi',['SPEC(deeleigen)','N(soort,ev,basis,onz,stan)'],['Grand','Slam_toernooi']).
mwu_postag('Grensoverschrijdende Onderwijssamenwerking',['WW(od,prenom,met-e)','N(soort,ev,basis,zijd,stan)'],[grens_overschrijden,onderwijssamenwerking]).
mwu_postag('Groene Boulevard',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[groen,'Boulevard']).
mwu_postag('Groene Fietsers',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[groen,fietser]).
mwu_postag('Groene Lijn',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[groen,lijn]).
mwu_postag('Grondwettelijk Verdrag',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[grondwettelijk,verdrag]).
mwu_postag('Grondwettelijk verdrag',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[gronwettelijk,verdrag]).
mwu_postag('Grondwet voor Europa - Presentatie voor de burger',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)','LET()','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[grondwet,voor,'Europa',-,presentatie,voor,de,burger]).
mwu_postag('groot-ayatollah as-Sistani',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[groot_ayatollah,'as-Sistani']).
mwu_postag('Groot Beeldwoordenboek Nederlands - Engels - Frans',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)','LET()','N(eigen,ev,basis,onz,stan)','LET()','N(eigen,ev,basis,onz,stan)'],[groot,beeldwoordenboek,'Nederlands',-,'Engels',-,'Frans']).
mwu_postag('Groot Beschrijf',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[groot,beschrijf]).
mwu_postag('Grootkruis in de Kroonorde',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[grootkruis,in,de,kroonorde]).
mwu_postag('Grootofficier in de Leopoldsorde',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(eigen,ev,basis,zijd,stan)'],[groot_officier,in,de,'Leopoldsorde']).
mwu_postag('Grote Depressie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[groot,depressie]).
mwu_postag('Grote Firewall',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[groot,firewall]).
mwu_postag('Grote Keurvorst',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[groot,keurvorst]).
mwu_postag('Grote Prijs van België',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[groot,prijs,van,'België']).
mwu_postag('Grote Prijs van België F1',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)'],[groot,prijs,van,'België','F1']).
mwu_postag('Grote Prijs van Frankrijk',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[groot,prijs,van,'Frankrijk']).
mwu_postag('Grote Schisma',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[groot,schisma]).
mwu_postag('Grote Spel',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[groot,spel]).
mwu_postag('Grote Steden Beleid',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','N(soort,ev,basis,onz,stan)'],[groot,stad,beleid]).
mwu_postag('Grotesteden- en Integratiebeleid',['SPEC(afgebr)','VG(neven)','N(soort,ev,basis,onz,stan)'],['Grotesteden-',en,integratiebeleid]).
mwu_postag('Grote Vlakten',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[groot,vlakte]).
mwu_postag('Grote Volksverhuizing',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[groot,volksverhuizing]).
mwu_postag('Grotten van Lascaux',['N(soort,mv,basis)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[grot,van,'Lascaux']).
mwu_postag('Guido Gezelle-comité',['SPEC(deeleigen)','N(soort,ev,basis,onz,stan)'],['Guido','Gezelle_comité']).
mwu_postag('Gulden Ontsporing',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],[gulden,ontsporing]).
mwu_postag('Gymnasium Nr. 6',['N(soort,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)','TW(hoofd,vrij)'],[gymnasium,nummer,'6']).
mwu_postag('Haagse Conferentie voor Internationaal Privaatrecht',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Haags',conferentie,voor,internationaal,privaatrecht]).
mwu_postag('haantje de voorste',['N(soort,ev,dim,onz,stan)','LID(bep,stan,rest)','ADJ(nom,basis,met-e,zonder-n,stan)'],[haan,de,voorste]).
mwu_postag('Habsburgse Nederlanden',['ADJ(prenom,basis,met-e,stan)','N(eigen,mv,basis)'],['Habsburgs','Nederland']).
mwu_postag('Hal 4',['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)'],[hal,'4']).
mwu_postag('hals over kop',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[hals,over,kop]).
mwu_postag('Handboek Cultuurbeleid',['N(soort,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)'],[handboek,cultuurbeleid]).
mwu_postag('Handel en Ontwikkeling',['N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[handel,en,ontwikkeling]).
mwu_postag('hand- tegen handgevechten',['SPEC(afgebr)','VZ(init)','N(soort,mv,basis)'],['hand-',tegen,hand_tegen_hand_gevecht]).
mwu_postag('hard drugs',['SPEC(vreemd)','N(soort,mv,basis)'],[hard,drug]).
mwu_postag('Hare Koninklijke Hoogheid',['VNW(bez,det,stan,vol,3v,ev,prenom,met-e,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[haar,koninklijk,hoogheid]).
mwu_postag('Hare Koninklijke en Keizerlijke Hoogheid',['VNW(bez,det,stan,vol,3v,ev,prenom,met-e,rest)','ADJ(prenom,basis,met-e,stan)','VG(neven)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[haar,koninklijk,en,keizerlijk,hoogheid]).
mwu_postag('hart- en vaatziekten',['SPEC(afgebr)','VG(neven)','N(soort,mv,basis)'],['hart-',en,vaat_ziekte]).
mwu_postag('heden ten dage',['N(soort,ev,basis,onz,stan)','VZ(versm)','N(soort,ev,basis,dat)'],[heden,te,dag]).
mwu_postag('heen en weer',['VZ(fin)','VG(neven)','BW()'],[heen,en,weer]).
mwu_postag('Heilige Land',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[heilig,land]).
mwu_postag('Heilige Pilaar',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[heilig,pilaar]).
mwu_postag('Heilige Roomse Keizerrijk',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[heilig,'Rooms',keizerrijk]).
mwu_postag('Held van de Sovjet-Unie',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(eigen,ev,basis,zijd,stan)'],[held,van,de,'Sovjet-Unie']).
mwu_postag('Hell\'s Angels-club',['SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)'],['Hell\'s','Angels_club']).
mwu_postag('Help Ons Overwinteren in Pakistan',['WW(pv,tgw,ev)','VNW(pr,pron,obl,vol,1,mv)','WW(inf,vrij,zonder)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[helpen,ons,overwinteren,in,'Pakistan']).
mwu_postag('her en der',['BW()','VG(neven)','BW()'],[her,en,der]).
mwu_postag('Herrezen Christus',['WW(vd,prenom,zonder)','N(eigen,ev,basis,zijd,stan)'],[herrijzen,'Christus']).
mwu_postag('Hertogdom Brabant',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[hertogdom,'Brabant']).
mwu_postag('Hertogdom Limburg',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[hertogdom,'Limburg']).
mwu_postag('Hertogdom Warschau',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[hertogdom,'Warschau']).
mwu_postag('Hertogin Anna-Amalia-bibliotheek',['SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)'],['Hertogin','Anna-Amalia_bibliotheek']).
mwu_postag('Hertogin van Brabant',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[hertogin,van,'Brabant']).
mwu_postag('het beste',['LID(bep,stan,evon)','ADJ(nom,sup,met-e,zonder-n,stan)'],[het,goed]).
mwu_postag('het best',['LID(bep,stan,evon)','ADJ(vrij,sup,zonder)'],[het,goed]).
mwu_postag('het dichtst bij',['LID(bep,stan,evon)','ADJ(nom,sup,zonder,zonder-n)','VZ(init)'],[het,dicht,bij]).
mwu_postag('het eerste het beste',['LID(bep,stan,evon)','TW(rang,prenom,stan)','LID(bep,stan,evon)','ADJ(prenom,sup,met-e,stan)'],[het,één,het,goed]).
mwu_postag('het van het',['LID(bep,stan,evon)','VZ(init)','LID(bep,stan,evon)'],[het,van,het]).
mwu_postag('bijzonder het in',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[in,het,bijzonder]).
mwu_postag('echt het in',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[in,het,echt]).
mwu_postag('echter op grond van',['VZ(init)','N(soort,ev,basis,zijd,stan)','BW()','VZ(init)'],[op,grond,echter,van]).
mwu_postag('groot het in',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[in,het,groot]).
mwu_postag('het hof',['LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[het,hof]).
mwu_postag('het in klein',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[in,het,klein]).
mwu_postag('het in kort',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[in,het,kort]).
mwu_postag('het in lang',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[in,het,lang]).
mwu_postag('het laatste woord',['LID(bep,stan,evon)','ADJ(prenom,sup,met-e,stan)','N(soort,ev,basis,onz,stan)'],[het,laat,woord]).
mwu_postag('het Lam Gods',['LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,gen)'],[het,lam,'God']).
mwu_postag('Het Lam Gods',['LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,gen)'],[het,lam,'God']).
mwu_postag('Het Louvre',['LID(bep,stan,evon)','N(eigen,ev,basis,onz,stan)'],[het,'Louvre']).
mwu_postag('hij / zij',['VNW(pers,pron,nomin,vol,3,ev,masc)','LET()','VNW(pers,pron,nomin,vol,3v,ev,fem)'],[hij,'/',zij]).
mwu_postag('Hoge Commissaris voor de Mensenrechten',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[hoog,commissaris,voor,de,mensenrecht]).
mwu_postag('Hogere Handelsschool Erasmus',['ADJ(prenom,comp,met-e,stan)','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[hoog,handelsschool,'Erasmus']).
mwu_postag('Hoge Snelheidslijn',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[hoog,snelheidslijn]).
mwu_postag('Hoge Vergadering',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[hoog,vergadering]).
mwu_postag('Hoge Vertegenwoordiger',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[hoog,vertegenwoordiger]).
mwu_postag('Hollands Glorie',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],['Hollands',glorie]).
mwu_postag('honderd uit',['TW(hoofd,vrij)','VZ(fin)'],[honderd,uit]).
mwu_postag('Honderdjarige Oorlog',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[honderjarig,oorlog]).
mwu_postag('Hoofdstedelijk Parlement',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[hoofdstedelijk,parlement]).
mwu_postag('hoog tijd',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],[hoog,tijd]).
mwu_postag('Huis Bernadotte',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[huis,'Bernadotte']).
mwu_postag('Huis Bourbon-Huis Napoléon',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[huis,'Bourbon','Napoléon']).
mwu_postag('Huis der Ottonen',['N(soort,ev,basis,onz,stan)','LID(bep,gen,rest3)','N(eigen,mv,basis)'],[huis,de,'Ottoon']).
mwu_postag('Huis Habsburg',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[huis,'Habsburg']).
mwu_postag('Huis Hohenstaufen',['N(soort,ev,basis,onz,stan)','N(eigen,mv,basis)'],[huis,'Hohenstaufer']).
mwu_postag('Huis Hohenzollern',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[huis,'Hohenzollern']).
mwu_postag('Huis Luxemburg',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[huis,'Luxemburg']).
mwu_postag('Huis van Bewaring',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[huis,van,bewaring]).
mwu_postag('Huis van het Nederlands',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,evon)','N(eigen,ev,basis,onz,stan)'],[huis,van,het,'Nederlands']).
mwu_postag('Huis Wettin',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[huis,'Wettin']).
mwu_postag('Huis Windsor',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[huis,'Windsor']).
mwu_postag('Hulpkas voor Werkloosheidsuitkeringen',['N(eigen,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)'],['Hulpkas',voor,werkloosheidsuitkering]).
mwu_postag('Humalog Mix-producten',['SPEC(deeleigen)','N(soort,mv,basis)'],['Humalog','Mix_product']).
mwu_postag('Humane Wetenschappen',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[humaan,wetenschap]).
mwu_postag('IB-Groep Infolijn',['N(eigen,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],['IB-groep','Infolijn']).
mwu_postag('ieder voor zich',['VNW(onbep,det,stan,vrij,zonder)','VZ(init)','VNW(refl,pron,obl,red,3,getal)'],[ieder,voor,zich]).
mwu_postag('Immateriële Erfgoed van de Mensheid',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[immaterieel,erfgoed,van,de,mensheid]).
mwu_postag('import / export-inspectie',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,zijd,stan)'],[import,/,export_inspectie]).
mwu_postag('in acht',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,acht]).
mwu_postag('in allerijl',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,allerijl]).
mwu_postag('in antwoord op',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[in,antwoord,op]).
mwu_postag('in combinatie met',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,combinatie,met]).
mwu_postag('in contact',['VZ(init)','N(soort,ev,basis,onz,stan)'],[in,contact]).
mwu_postag('in de aanloop naar',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,de,aanloop,naar]).
mwu_postag('in de boeien',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[in,de,boei]).
mwu_postag('in de eerste plaats',['VZ(init)','LID(bep,stan,rest)','TW(rang,prenom,stan)','N(soort,ev,basis,zijd,stan)'],[in,de,één,plaats]).
mwu_postag('in de fout',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,fout]).
mwu_postag('in de gaten',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[in,de,gat]).
mwu_postag('in de lijn der verwachting',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(soort,ev,basis,zijd,stan)'],[in,de,lijn,de,verwachting]).
mwu_postag('in de lijn der verwachtingen',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(soort,mv,basis)'],[in,de,lijn,de,verwachting]).
mwu_postag('in de loop van',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,de,loop,van]).
mwu_postag('in de maak',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,maak]).
mwu_postag('in de plaats',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,plaats]).
mwu_postag('in de prijzen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[in,de,prijs]).
mwu_postag('in de rede',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,rede]).
mwu_postag('in de schoenen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[in,de,schoen]).
mwu_postag('in de sterren geschreven',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)','WW(vd,vrij,zonder)'],[in,de,ster,schrijven]).
mwu_postag('in de weer',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,weer]).
mwu_postag('in de weg',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,weg]).
mwu_postag('in de war',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[in,de,war]).
mwu_postag('in de zeilen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[in,de,zeil]).
mwu_postag('in deze',['VZ(init)','VNW(aanw,det,stan,nom,met-e,zonder-n)'],[in,deze]).
mwu_postag('in dezen',['VZ(init)','VNW(aanw,det,dat,nom,met-e,zonder-n)'],[in,deze]).
mwu_postag('Indonesische Volkscongres',['ADJ(prenom,basis,met-e,stan)','N(eigen,ev,basis,onz,stan)'],['Indonesisch','Volkscongres']).
mwu_postag('in duigen',['VZ(init)','N(soort,mv,basis)'],[in,duig]).
mwu_postag('Industriële Revolutie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[industrieel,revolutie]).
mwu_postag('in elkaar',['VZ(init)','VNW(recip,pron,obl,vol,persoon,mv)'],[in,elkaar]).
mwu_postag('in feite',['VZ(init)','N(soort,ev,basis,dat)'],[in,feit]).
mwu_postag('in functie van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,functie,van]).
mwu_postag('in gebreke',['VZ(init)','N(soort,ev,basis,dat)'],[in,gebrek]).
mwu_postag('in gebruik',['VZ(init)','N(soort,ev,basis,onz,stan)'],[in,gebruik]).
mwu_postag('in geval van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[in,geval,van]).
mwu_postag('in goeie doen',['VZ(init)','ADJ(prenom,basis,met-e,stan)','WW(inf,nom,zonder,zonder-n)'],[in,goed,doen]).
mwu_postag('in godsnaam',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,god_naam]).
mwu_postag('in Godsnaam',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,'God_naam']).
mwu_postag('in groten getale',['VZ(init)','ADJ(prenom,basis,met-e,bijz)','N(soort,ev,basis,dat)'],[in,groot,getal]).
mwu_postag('in het geheel',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,geheel]).
mwu_postag('in het geval',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,geval]).
mwu_postag('in het geweer',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,geweer]).
mwu_postag('in het kader van',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[in,het,kader,van]).
mwu_postag('in het licht van',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[in,het,licht,van]).
mwu_postag('in het onzekere',['VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,met-e,zonder-n,stan)'],[in,het,onzeker]).
mwu_postag('in het oog',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,oog]).
mwu_postag('in het spel',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,spel]).
mwu_postag('in het teken',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,teken]).
mwu_postag('in het wilde weg',['VZ(init)','LID(bep,stan,evon)','ADJ(prenom,basis,met-e,stan)','BW()'],[in,het,wild,weg]).
mwu_postag('in het zadel',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[in,het,zadel]).
mwu_postag('in kaart',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,kaart]).
mwu_postag('in lang',['VZ(init)','ADJ(vrij,basis,zonder)'],[in,lang]).
mwu_postag('in levende lijve',['VZ(init)','WW(od,prenom,met-e)','N(soort,ev,basis,dat)'],[in,leven,lijf]).
mwu_postag('in lichterlaaie',['VZ(init)','BW()'],[in,lichterlaaie]).
mwu_postag('in naam van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,naam,van]).
mwu_postag('in navologing van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,navolging,van]).
mwu_postag('in omloop',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,omloop]).
mwu_postag('in orde',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,orde]).
mwu_postag('in overleg met',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[in,overleg,met]).
mwu_postag('in opdracht van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,opdracht,van]).
mwu_postag('in plaats van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,plaats,van]).
mwu_postag('in ruil voor',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,ruil,voor]).
mwu_postag('in samenwerking met',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,samenwerking,met]).
mwu_postag('in staat',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,staat]).
mwu_postag('in strijd',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,strijd]).
mwu_postag('in weerwil van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,weerwil,van]).
mwu_postag('in zijn werk',['VZ(init)','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','N(soort,ev,basis,onz,stan)'],[in,zijn,werk]).
mwu_postag('Integraal Dossier JGZ',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[integraal,dossier,'JGZ']).
mwu_postag('Intercultureel Onderwijs',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[intercultureel,onderwijs]).
mwu_postag('Intergouvernementele Conferentie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[intergouvernementeel,conferentie]).
mwu_postag('in tegenstelling tot',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,tegenstelling,tot]).
mwu_postag('in termen van',['VZ(init)','N(soort,mv,basis)','VZ(init)'],[in,term,van]).
mwu_postag('Internationaal Filmfestival Rotterdam',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[internationaal,filmfestival,'Rotterdam']).
mwu_postag('Internationaal Middeleeuws theaterfestival',['ADJ(prenom,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[internationaal,middeleeuws,theater_festival]).
mwu_postag('Internationaal Stelsel van Eenheden',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,mv,basis)'],[internationaal,stelsel,van,eenheid]).
mwu_postag('Internationaal Theaterschool Festival',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[internationaal,theaterschool,festival]).
mwu_postag('Internationaal Verdrag',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[internationaal,verdrag]).
mwu_postag('Internationale Betrekkingen',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[internationaal,betrekking]).
mwu_postag('Internationale Gerechtshof',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[internationaal,gerechtshof]).
mwu_postag('Internationale Goudstandaard',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[internationaal,goudstandaard]).
mwu_postag('Internationale Schakelklas',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[internationaal,schakelklas]).
mwu_postag('Investeringsbudget Stedelijke Vernieuwing',['N(soort,ev,basis,onz,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[investeringsbudget,stedelijk,vernieuwing]).
mwu_postag('in verband met',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[in,verband,met]).
mwu_postag('in verbinding',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[in,verbinding]).
mwu_postag('in vergelijking met',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[in,vergelijking,met]).
mwu_postag('in wezen',['VZ(init)','N(soort,ev,basis,onz,stan)'],[in,wezen]).
mwu_postag('in zak en as',['VZ(init)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[in,zak,en,as]).
mwu_postag('Islamitische Jihad',['ADJ(prenom,basis,met-e,stan)','N(eigen,ev,basis,zijd,stan)'],[islamitisch,'Jihad']).
mwu_postag('Islamitische Verzetsbeweging',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[islamitisch,verzet_beweging]).
mwu_postag('Jacques Brel-liederen',['SPEC(deeleigen)','N(soort,mv,basis)'],['Jacques','Brel_lied']).
mwu_postag('Jacques Brel-vertolkers',['SPEC(deeleigen)','N(soort,mv,basis)'],['Jacques','Brel_vertolker']).
mwu_postag('ja hoor',['TSW()','TSW()'],[ja,hoor]).
mwu_postag('jawel hoor',['TSW()','TSW()'],[jawel,hoor]).
mwu_postag('Jehovah\'s Getuige',['N(eigen,ev,basis,gen)','N(soort,ev,basis,zijd,stan)'],['Jehovah',getuige]).
mwu_postag('Jehovah\'s Getuigen',['N(eigen,ev,basis,gen)','N(soort,mv,basis)'],['Jehovah',getuige]).
mwu_postag('Jellinek Preventie Amsterdam',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)'],['Jellinek',preventie,'Amsterdam']).
mwu_postag('je reinste',['VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)','ADJ(prenom,sup,met-e,stan)'],[je,rein]).
mwu_postag('Jeugd en Sport',['N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[jeugd,en,sport]).
mwu_postag('Jeugd en Stad',['N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[jeugd,en,stad]).
mwu_postag('jihad - strijders',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,mv,basis)'],[jihad,-,strijder]).
mwu_postag('Jurgen van den Goorbergh-tribune',['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['Jurgen',van,den,'Goorbergh_tribune']).
mwu_postag('Juridische Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[juridisch,zaak]).
mwu_postag('Kaders van België',['N(soort,mv,basis)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[kader,van,'België']).
mwu_postag('Kalmthoutse School',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Kalmthouts',school]).
mwu_postag('Kamers van Koophandel',['N(soort,mv,basis)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[kamer,van,koophandel]).
mwu_postag('Kamer van Gedeputeerden',['N(soort,ev,basis,zijd,stan)','VZ(init)','WW(vd,nom,met-e,mv-n)'],[kamer,van,deputeren]).
mwu_postag('Kamer van Inbeschuldigingstelling',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[kamer,van,inbeschuldigingstelling]).
mwu_postag('Kamer van Koophandel en Fabrieken',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,mv,basis)'],[kamer,van,koophandel,en,fabriek]).
mwu_postag('Kamer van Volksvertegenwoordigers',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)'],[kamer,van,volksvertegenwoordiger]).
mwu_postag('Kanselarij Algemene Diensten',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[kanselarij,algemeen,dienst]).
mwu_postag('Kanselarij & Voorlichting',['N(soort,ev,basis,zijd,stan)','SPEC(symb)','N(soort,ev,basis,zijd,stan)'],[kanselarij,&,voorlichting]).
mwu_postag('Kasteel van Stuyvenberg',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,zijd,stan)'],[kasteel,van,'Stuyvenberg']).
mwu_postag('keer op keer',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[keer,op,keer]).
mwu_postag('Kerkelijke Staat',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[kerkelijk,staat]).
mwu_postag('Keuringsdienst van Waren',['N(soort,ev,basis,zijd,stan)','VZ(init)','WW(pv,verl,mv)'],[keuringsdienst,van,zijn]).
mwu_postag('kijk- en luistergeld',['SPEC(afgebr)','VG(neven)','N(soort,ev,basis,onz,stan)'],['kijk-',en,luister_geld]).
mwu_postag('Klassieke Oudheid',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[klassiek,oudheid]).
mwu_postag('kommer en kwel',['N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[kommer,en,kwel]).
mwu_postag('koning der Belgen',['N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(eigen,mv,basis)'],[koning,de,'Belg']).
mwu_postag('Koning der Belgen',['N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(eigen,mv,basis)'],[koning,de,'Belg']).
mwu_postag('Koning der Nederlanden',['N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(eigen,mv,basis)'],[koning,de,'Nederland']).
mwu_postag('Koningin der Belgen',['N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(eigen,mv,basis)'],[koningin,de,'Belg']).
mwu_postag('Koningin van België',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[koningin,van,'België']).
mwu_postag('Koninklijk Atheneum Voskenslaan',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[koninklijk,atheneum,'Voskenslaan']).
mwu_postag('Koninklijk Besluit',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[koninklijk,besluit]).
mwu_postag('Koninklijk Commissaris voor het Migrantenbeleid',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[koninklijk,commissaris,voor,het,migrantenbeleid]).
mwu_postag('Koninklijke Crypte',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[koninklijk,crypte]).
mwu_postag('Koninklijke Musea voor Kunst en Geschiedenis',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VZ(init)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[koninklijk,museum,voor,kunst,en,geschiedenis]).
mwu_postag('Koninklijke Musea voor Schone Kunsten',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[koninklijk,museum,voor,schoon,kunst]).
mwu_postag('Koninklijke Musea voor Schone Kunsten van België',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[koninklijk,museum,voor,schoon,kunst,van,'België']).
mwu_postag('Koninklijk Huis',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[koninklijk,huis]).
mwu_postag('Koninkrijk Bourgondië',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[koninkrijk,'Bourgondië']).
mwu_postag('Koninkrijk Holland',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[koninkrijk,'Holland']).
mwu_postag('Koninkrijk Sicilië',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[koninkrijk,'Sicilië']).
mwu_postag('Koninkrijk Westfalen',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[koninkrijk,'Westfalen']).
mwu_postag('kopje onder',['N(soort,ev,dim,onz,stan)','VZ(fin)'],[kop,onder]).
mwu_postag('kort geding',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[kort,geding]).
mwu_postag('korte metten',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[kort,metten]).
mwu_postag('koste wat het kostte',['WW(pv,conj,ev)','VNW(vb,pron,stan,vol,3o,ev)','VNW(pers,pron,stan,red,3,ev,onz)','WW(pv,verl,ev)'],[kosten,wat,het,kosten]).
mwu_postag('koste wat het kost',['WW(pv,conj,ev)','VNW(vb,pron,stan,vol,3o,ev)','VNW(pers,pron,stan,red,3,ev,onz)','WW(pv,tgw,ev)'],[kosten,wat,het,kosten]).
mwu_postag('koste wat koste',['WW(pv,conj,ev)','VNW(vb,pron,stan,vol,3o,ev)','WW(pv,conj,ev)'],[kosten,wat,kosten]).
mwu_postag('koste wat \'t kost',['WW(pv,conj,ev)','VNW(onbep,pron,stan,vol,3o,ev)','LID(bep,stan,evon)','WW(pv,tgw,ev)'],[kosten,wat,het,kosten]).
mwu_postag('kost wat kost',['WW(pv,tgw,ev)','VNW(vb,pron,stan,vol,3o,ev)','WW(pv,tgw,ev)'],[kosten,wat,kosten]).
mwu_postag('Kroonprinses van België',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[kroonprinses,van,'België']).
mwu_postag('Kroonprins van België',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[kroon_prins,van,'België']).
mwu_postag('KWR 2000',['N(soort,ev,basis,zijd,stan)','TW(hoofd,vrij)'],['KWR','2000']).
mwu_postag('laat staan',['WW(pv,tgw,ev)','WW(inf,vrij,zonder)'],[laten,staan]).
mwu_postag('Lam Gods , dat wegneemt de zonden der wereld',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,gen)','LET()','VNW(aanw,pron,stan,vol,3o,ev)','WW(pv,tgw,met-t)','LID(bep,stan,rest)','N(soort,mv,basis)','LID(bep,gen,rest3)','N(soort,ev,basis,zijd,stan)'],[lam,'God',',',dat,wegnemen,de,zonde,de,wereld]).
mwu_postag('Lam Gods',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,gen)'],[lam,'God']).
mwu_postag('Landbouw , Natuurbeheer en Visserij',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,onz,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[landbouw,',',natuurbeheer,en,visserij]).
mwu_postag('Land van Dendermonde',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[land,van,'Dendermonde']).
mwu_postag('Land van Ny',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[land,van,'Ny']).
mwu_postag('Landelijke Alcohol Infolijn',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[landelijk,alcohol,'Infolijn']).
mwu_postag('let wel',['WW(pv,tgw,ev)','BW()'],[letten,wel]).
mwu_postag('Limburgs Schutterstijdschrift',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Limburgs',schutterstijdschrift]).
mwu_postag('lucht- en ruimtevaarttechniek',['SPEC(afgebr)','VG(neven)','N(soort,ev,basis,zijd,stan)'],['lucht-',en,ruimtevaart_techniek]).
mwu_postag('Luchthaven Zaventem',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)'],[luchthaven,'Zaventem']).
mwu_postag('maar dan',['VG(neven)','BW()'],[maar,dan]).
mwu_postag('maar liefst',['BW()','BW()'],[maar,liefst]).
mwu_postag('makkelijk praten',['ADJ(vrij,basis,zonder)','WW(inf,vrij,zonder)'],[makkelijk,praten]).
mwu_postag('Man bijt hond',['N(eigen,ev,basis,onz,stan)','WW(pv,tgw,ev)','N(soort,ev,basis,zijd,stan)'],['Man',bijten,hond]).
mwu_postag('Man Bijt Hond',['N(soort,ev,basis,zijd,stan)','WW(pv,tgw,ev)','N(soort,ev,basis,zijd,stan)'],[man,bijten,hond]).
mwu_postag('Máxima der Nederlanden',['N(eigen,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(eigen,mv,basis)'],['Máxima',de,'Nederland']).
mwu_postag('Medische Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[medisch,zaak]).
mwu_postag('meer dan',['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[veel,dan]).
mwu_postag('Meer dan',['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[veel,dan]).
mwu_postag('MEER dan',['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[veel,dan]).
mwu_postag('méér dan',['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[veel,dan]).
mwu_postag('meer en meer',['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(neven)','VNW(onbep,grad,stan,vrij,zonder,comp)'],[veel,en,veel]).
mwu_postag('meer niet',['VNW(onbep,grad,stan,vrij,zonder,comp)','BW()'],[veel,niet]).
mwu_postag('Mei Requiem',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[mei,requiem]).
mwu_postag('Meld misdaad anoniem',['WW(pv,tgw,ev)','N(soort,ev,basis,zijd,stan)','ADJ(vrij,basis,zonder)'],[melden,misdaad,anoniem]).
mwu_postag('met andere woorden',['VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[met,ander,woord]).
mwu_postag('met behulp van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[met,behulp,van]).
mwu_postag('meten is weten',['WW(inf,vrij,zonder)','WW(pv,tgw,ev)','WW(inf,vrij,zonder)'],[meten,zijn,weten]).
mwu_postag('met betrekking tot',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[met,betrekking,tot]).
mwu_postag('met het oog op',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[met,het,oog,op]).
mwu_postag('Met het Oog op Morgen',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)','BW()'],[met,het,oog,op,morgen]).
mwu_postag('met inachtneming van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[met,inachtneming,van]).
mwu_postag('met ingang van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[met,ingang,van]).
mwu_postag('met medewerking van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[met,medewerking,van]).
mwu_postag('met name',['VZ(init)','N(soort,ev,basis,dat)'],[met,naam]).
mwu_postag('met steun van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[met,steun,van]).
mwu_postag('met uitzondering van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[met,uitzondering,van]).
mwu_postag('met verve',['VZ(init)','N(soort,ev,basis,dat)'],[met,verve]).
mwu_postag('met voeten',['VZ(init)','N(soort,mv,basis)'],[met,voet]).
mwu_postag('Middellandse Zee-gebied',['SPEC(deeleigen)','N(eigen,ev,basis,onz,stan)'],['Middellandse','Zee_gebied']).
mwu_postag('mijns inziens',['VNW(bez,det,gen,vol,1,ev,prenom,zonder,evmo)','N(soort,ev,basis,gen)'],[mijn,in_zien]).
mwu_postag('Milieu Actie Plan',['N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[milieu,actie,plan]).
mwu_postag('Milieu Marin',['N(soort,ev,basis,onz,stan)','SPEC(vreemd)'],[milieu,'Marin']).
mwu_postag('Militanten Orde',['N(soort,mv,basis)','N(soort,ev,basis,zijd,stan)'],[militant,orde]).
mwu_postag('Millennium Doelstellingen',['N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[millennium,doelstelling]).
mwu_postag('Millennium Ontwikkelingsdoelen',['N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[millennium,ontwikkelingsdoel]).
mwu_postag('millennium ontwikkelingsdoelstellingen',['N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[millennium,ontwikkelingsdoelstelling]).
mwu_postag('Millennium ontwikkelingsdoelstellingen',['N(soort,ev,basis,onz,stan)','N(soort,mv,basis)'],[millennium,ontwikkelingsdoelstelling]).
mwu_postag('minder dan',['VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[weinig,dan]).
mwu_postag('Ministerie EZ',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,genus,stan)'],[ministerie,'EZ']).
mwu_postag('Ministeriële Conferentie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[ministerieel,conferentie]).
mwu_postag('Ministerie van Binnenlandse Zaken',['N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[ministerie,van,binnenlands,zaak]).
mwu_postag('Ministerie van Buitenlandse Zaken',['N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[ministerie,van,buitenlands,zaak]).
mwu_postag('Ministerie van Buitenlandse Zaken / Ontwikkelingssamenwerking',['N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','LET()','N(soort,ev,basis,zijd,stan)'],[ministerie,van,buitenlands,zaak,/,ontwikkelingssamenwerking]).
mwu_postag('Ministerie van Defensie',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[ministerie,van,defensie]).
mwu_postag('Ministerie van Economische Zaken',['N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[ministerie,van,economisch,zaak]).
mwu_postag('Ministerie van EZ',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,genus,stan)'],[ministerie,van,'EZ']).
mwu_postag('Ministerie van Justitie',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[ministerie,van,justitie]).
mwu_postag('Ministerie van Onderwijs en Wetenschappen',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,onz,stan)','VG(neven)','N(soort,mv,basis)'],[ministerie,van,onderwijs,en,wetenschap]).
mwu_postag('Ministerie van Onderwijs',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,onz,stan)'],[ministerie,van,onderwijs]).
mwu_postag('Ministerie van Sociale Zaken en Werkgelegenheid',['N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[ministerie,van,sociaal,zaak,en,werkgelegenheid]).
mwu_postag('Ministerie van Volksgezondheid , Welzijn en Sport',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,onz,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[ministerie,van,volksgezondheid,',',welzijn,en,sport]).
mwu_postag('Ministerie van VROM',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,genus,stan)'],[ministerie,van,'VROM']).
mwu_postag('Ministerie van Wapenproductie',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[ministerie,van,wapenproductie]).
mwu_postag('min of meer',['BW()','VG(neven)','VNW(onbep,grad,stan,vrij,zonder,comp)'],[min,of,veel]).
mwu_postag('moederziel alleen',['N(soort,ev,basis,zijd,stan)','BW()'],[moederziel,alleen]).
mwu_postag('mogelijk zo',['BW()','ADJ(vrij,basis,zonder)'],[zo,mogelijk]).
mwu_postag('min mogelijk zo',['BW()','BW()','ADJ(vrij,basis,zonder)'],[zo,min,mogelijk]).
mwu_postag('mond- en klauwzeercrisis',['SPEC(afgebr)','VG(neven)','N(soort,ev,basis,zijd,stan)'],['mond-',en,klauwzeer_crisis]).
mwu_postag('Muziekhuis van Pantalone',['N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[muziekhuis,van,'Pantalone']).
mwu_postag('na afloop van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[na,afloop,van]).
mwu_postag('na verloop van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[na,verloop,van]).
mwu_postag('naar aanleiding van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[naar,aanleiding,van]).
mwu_postag('naar buiten',['VZ(init)','VZ(fin)'],[naar,buiten]).
mwu_postag('naar gelang van',['VZ(init)','VG(onder)','VZ(init)'],[naar,naargelang,van]).
mwu_postag('naar schatting',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[naar,schatting]).
mwu_postag('naar verluidt',['VZ(init)','WW(pv,tgw,met-t)'],[naar,verluiden]).
mwu_postag('naar voren',['VZ(init)','BW()'],[naar,voren]).
mwu_postag('Nationaal Akkoord Wonen',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','WW(inf,vrij,zonder)'],[nationaal,akkoord,wonen]).
mwu_postag('Nationaal Herbarium',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[nationaal,herbarium]).
mwu_postag('Nationaal Isolatie Programma',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[nationaal,isolatie,programma]).
mwu_postag('Nationaal Monument Slavernijverleden',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)'],[nationaal,monument,slavernijverleden]).
mwu_postag('Nationaal Schengen Informatie Systeem',['ADJ(prenom,basis,zonder)','N(eigen,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],[nationaal,'Schengen',informatie,systeem]).
mwu_postag('Nationaal-Socialistische Arbeiderspartij',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[nationaal_socialistisch,arbeiderspartij]).
mwu_postag('Nationaal Waterkanaal',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[nationaal,waterkanaal]).
mwu_postag('Nationale Assemblee',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[nationaal,assemblee]).
mwu_postag('Nationale Centrale der Liberale Vakbonden van België',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','LID(bep,gen,rest3)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[nationaal,centraal,de,liberaal,vakbond,van,'België']).
mwu_postag('Nationale Conventie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[nationaal,conventie]).
mwu_postag('Nationale Delcrederedienst',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[nationaal,'Delcrederedienst']).
mwu_postag('Nationale Indicatieve Programma\'s',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[nationaal,indicatief,programma]).
mwu_postag('Nationale Pact',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[nationaal,pact]).
mwu_postag('Nationale Strategie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[nationaal,strategie]).
mwu_postag('Nationale Voorleesdagen',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[nationaal,voorleesdag]).
mwu_postag('nature van',['VZ(init)','N(soort,ev,basis,dat)'],[van,natuur]).
mwu_postag('nederland 3',['N(eigen,ev,basis,onz,stan)','TW(hoofd,vrij)'],['Nederland','3']).
mwu_postag('Nederlandse Investeringsbank voor Ontwikkelingslanden',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)'],['Nederlands',investeringsbank,voor,ontwikkelingsland]).
mwu_postag('Nederlandse Journalistenvakbond',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Nederlands',journalistenvakbond]).
mwu_postag('Nederlandse Rode Lijst Korstmossen',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','N(soort,mv,basis)'],['Nederlands',rood,lijst,korstmos]).
mwu_postag('Nederlandse Staat',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Nederlands',staat]).
mwu_postag('Nederlandse Taal- en Letterkunde',['ADJ(prenom,basis,met-e,stan)','SPEC(afgebr)','VG(neven)','N(soort,ev,basis,zijd,stan)'],['Nederlands','Taal-',en,letterkunde]).
mwu_postag('Nederlands Fonds voor de Film',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],['Nederlands',fonds,voor,de,film]).
mwu_postag('nee hoor',['TSW()','TSW()'],[nee,hoor]).
mwu_postag('nek aan nek',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[nek,aan,nek]).
mwu_postag('New Yorkse',['SPEC(deeleigen)','ADJ(prenom,basis,met-e,stan)'],['New','Yorks']).
mwu_postag('niemand minder dan',['VNW(onbep,pron,stan,vol,3p,ev)','VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[niemand,weinig,dan]).
mwu_postag('niet alleen',['BW()','BW()'],[niet,alleen]).
mwu_postag('niet alleen meer',['BW()','BW()','VNW(onbep,grad,stan,vrij,zonder,comp)'],[niet,alleen,veel]).
mwu_postag('niet het minst',['BW()','LID(bep,stan,evon)','VNW(onbep,grad,stan,nom,zonder,zonder-n,sup)'],[niet,het,weinig]).
mwu_postag('niet meer dan',['BW()','VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[niet,veel,dan]).
mwu_postag('niet minder dan',['BW()','VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[niet,weinig,dan]).
mwu_postag('niet waar',['BW()','ADJ(vrij,basis,zonder)'],[niet,waar]).
mwu_postag('niet zonder',['BW()','VZ(init)'],[niet,zonder]).
mwu_postag('niet zozeer',['BW()','BW()'],[niet,zozeer]).
mwu_postag('niets minder dan',['VNW(onbep,pron,stan,vol,3o,ev)','VNW(onbep,grad,stan,vrij,zonder,comp)','VG(onder)'],[niets,weinig,dan]).
mwu_postag('Nieuwe Orde',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[nieuw,orde]).
mwu_postag('Nieuws in Het Kort',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[nieuws,in,het,kort]).
mwu_postag('nieuws in het kort',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,evon)','ADJ(nom,basis,zonder,zonder-n)'],[nieuws,in,het,kort]).
mwu_postag('noem maar op',['WW(pv,tgw,ev)','BW()','VZ(fin)'],[noemen,maar,op]).
mwu_postag('nog eens',['BW()','BW()'],[nog,eens]).
mwu_postag('nog geen',['BW()','VNW(onbep,det,stan,prenom,zonder,agr)'],[nog,geen]).
mwu_postag('nog liever',['BW()','BW()'],[nog,liever]).
mwu_postag('nooit ofte nimmer',['BW()','VG(neven)','BW()'],[nooit,ofte,nimmer]).
mwu_postag('Noord-Amerikaanse Vrijhandelsovereenkomst',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Noord-Amerikaans',vrijhandelsovereenkomst]).
mwu_postag('Noordelijke Commando',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[noordelijk,commando]).
mwu_postag('Noord Frankrijk',['ADJ(vrij,basis,zonder)','N(eigen,ev,basis,onz,stan)'],[noord,'Frankrijk']).
mwu_postag('Noord Nederlandse interfaculteit der Kunsten',['SPEC(deeleigen)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','LID(bep,gen,rest3)','N(soort,mv,basis)'],['Noord','Nederlands',interfaculteit,de,kunst]).
mwu_postag('Noord-West Engeland',['ADJ(prenom,basis,zonder)','N(eigen,ev,basis,onz,stan)'],['noord-west','Engeland']).
mwu_postag('Noord-West Europa',['ADJ(prenom,basis,zonder)','N(eigen,ev,basis,onz,stan)'],[noord_west,'Europa']).
mwu_postag('Noord-Zuid-koepel 11.11.11',['N(soort,ev,basis,zijd,stan)','SPEC(symb)'],[noord_zuid_koepel,'11.11.11']).
mwu_postag('Norton 500 CC',['N(eigen,ev,basis,zijd,stan)','TW(hoofd,prenom,stan)','SPEC(symb)'],[norton,'500','CC']).
mwu_postag('NOS Actueel',['N(eigen,ev,basis,zijd,stan)','ADJ(vrij,basis,zonder)'],['NOS',actueel]).
mwu_postag('NOS Journaal',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,onz,stan)'],['NOS',journaal]).
mwu_postag('NOS Televisie',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],['NOS',televisie]).
mwu_postag('nou ja',['BW()','TSW()'],[nou,ja]).
mwu_postag('nou eenmaal',['BW()','BW()'],[nou,eenmaal]).
mwu_postag('nu eenmaal',['BW()','BW()'],[nu,eenmaal]).
mwu_postag('nu eens',['BW()','BW()'],[nu,eens]).
mwu_postag('nummer 1 hits',['N(soort,ev,basis,onz,stan)','TW(hoofd,vrij)','N(soort,mv,basis)'],[nummer,'1',hit]).
mwu_postag('o ja',['TSW()','TSW()'],[o,ja]).
mwu_postag('o nee',['TSW()','TSW()'],[o,nee]).
mwu_postag('o zo',['TSW()','BW()'],[o,zo]).
mwu_postag('och arme',['TSW()','ADJ(prenom,basis,met-e,stan)'],[och,arm]).
mwu_postag('of dat',['VG(onder)','VG(onder)'],[of,dat]).
mwu_postag('om het even',['VZ(init)','LID(bep,stan,evon)','BW()'],[om,het,even]).
mwu_postag('om niet',['VZ(init)','BW()'],[om,niet]).
mwu_postag('omwille van',['BW()','VZ(init)'],[omwille,van]).
mwu_postag('Omwille van',['BW()','VZ(init)'],[omwille,van]).
mwu_postag('Onafhankelijke Schatkistakte',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[onafhankelijk,schatkist_akte]).
mwu_postag('Onbevlekte Ontvangenis',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[onbevlekt,ontvangenis]).
mwu_postag('onder aanvoering van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[onder,aanvoering,van]).
mwu_postag('onder anderen',['VZ(init)','ADJ(nom,basis,met-e,mv-n)'],[onder,ander]).
mwu_postag('onder andere',['VZ(init)','ADJ(nom,basis,met-e,zonder-n,stan)'],[onder,ander]).
mwu_postag('Onder andere',['VZ(init)','ADJ(nom,basis,met-e,zonder-n,stan)'],[onder,ander]).
mwu_postag('Onder anderen',['VZ(init)','ADJ(nom,basis,met-e,mv-n)'],[onder,ander]).
mwu_postag('onder auspicien van',['VZ(init)','N(soort,mv,basis)','VZ(init)'],[onder,auspiciën,van]).
mwu_postag('onder auspiciën van',['VZ(init)','N(soort,mv,basis)','VZ(init)'],[onder,auspiciën,van]).
mwu_postag('onder bewindstelling',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[onder,bewind_stelling]).
mwu_postag('onder de indruk',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[onder,de,indruk]).
mwu_postag('onder ede',['VZ(init)','N(soort,ev,basis,dat)'],[onder,eed]).
mwu_postag('onder handen',['VZ(init)','N(soort,mv,basis)'],[onder,hand]).
mwu_postag('onder invloed van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[onder,invloed,van]).
mwu_postag('onder leiding van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[onder,leiding,van]).
mwu_postag('onder meer',['VZ(init)','VNW(onbep,grad,stan,vrij,zonder,comp)'],[onder,veel]).
mwu_postag('onder ogen',['VZ(init)','N(soort,mv,basis)'],[onder,oog]).
mwu_postag('onder stoelen of banken',['VZ(init)','N(soort,mv,basis)','VG(neven)','N(soort,mv,basis)'],[onder,stoel,of,bank]).
mwu_postag('onder voorzitterschap van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[onder,voorzitterschap,van]).
mwu_postag('Onderwijs , Cultuur en Wetenschappen',['N(soort,ev,basis,onz,stan)','LET()','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,mv,basis)'],[onderwijs,',',cultuur,en,wetenschap]).
mwu_postag('Onderwijs Cultuur en Wetenschappen',['N(soort,ev,basis,onz,stan)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,mv,basis)'],[onderwijs,cultuur,en,wetenschap]).
mwu_postag('ons inziens',['VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)','N(soort,ev,basis,gen)'],[ons,in_zien]).
mwu_postag('oog in oog',['N(soort,ev,basis,onz,stan)','VZ(init)','N(soort,ev,basis,onz,stan)'],[oog,in,oog]).
mwu_postag('ook maar',['BW()','BW()'],[ook,maar]).
mwu_postag('op andere gedachten',['VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[op,ander,gedachte]).
mwu_postag('op basis van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,basis,van]).
mwu_postag('op de hoogte',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[op,de,hoogte]).
mwu_postag('op de hielen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[op,de,hiel]).
mwu_postag('op de lange baan',['VZ(init)','LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[op,de,lang,baan]).
mwu_postag('Op de duur',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[op,de,duur]).
mwu_postag('Op den duur',['VZ(init)','LID(bep,dat,evmo)','N(soort,ev,basis,zijd,stan)'],[op,de,duur]).
mwu_postag('op de duur',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[op,de,duur]).
mwu_postag('op den duur',['VZ(init)','LID(bep,dat,evmo)','N(soort,ev,basis,zijd,stan)'],[op,de,duur]).
mwu_postag('op de proppen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[op,de,prop]).
mwu_postag('op dreef',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,dreef]). 
mwu_postag('op en neer',['VZ(fin)','VG(neven)','BW()'],[op,en,neer]).
mwu_postag('op en top',['VZ(fin)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[op,en,top]).
mwu_postag('op gang',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,gang]).
mwu_postag('op het gebied van',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[op,het,gebied,van]).
mwu_postag('op het gevaar af',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(fin)'],[op,het,gevaar,af]).
mwu_postag('op het spoor',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[op,het,spoor]).
mwu_postag('op hol',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,hol]).
mwu_postag('Openbaar Vervoer',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[openbaar,vervoer]).
mwu_postag('Openbare Bibliotheken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[openbaar,bibliotheek]).
mwu_postag('Operatie Barbarossa',['N(soort,ev,basis,zijd,stan)','SPEC(vreemd)'],[operatie,'Barbarossa']).
mwu_postag('Operatie Market Garden',['N(soort,ev,basis,zijd,stan)','SPEC(deeleigen)','SPEC(deeleigen)'],[operatie,'Market','Garden']).
mwu_postag('op grond van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,grond,van]).
mwu_postag('op haar smalst',['VZ(init)','VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)','ADJ(vrij,comp,zonder)'],[op,haar,smal]).
mwu_postag('op handen',['VZ(init)','N(soort,mv,basis)'],[op,hand]).
mwu_postag('op het laatst',['VZ(init)','LID(bep,stan,evon)','ADJ(vrij,sup,zonder)'],[op,het,laat]).
mwu_postag('op het verkeerde been',['VZ(init)','LID(bep,stan,evon)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],[op,het,verkeerd,been]).
mwu_postag('op heterdaad',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,heterdaad]).
mwu_postag('op initiatief van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[op,initiatief,van]).
mwu_postag('op jacht',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,jacht]).
mwu_postag('op komst',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,komst]). 
mwu_postag('op kosten van',['VZ(init)','N(soort,mv,basis)','VZ(init)'],[op,kost,van]).
mwu_postag('op last van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,last,van]).
mwu_postag('op los',['VZ(fin)','ADJ(vrij,basis,zonder)'],[op,los]).
mwu_postag('op lossen schroeven',['VZ(init)','WW(inf,vrij,zonder)','N(soort,mv,basis)'],[op,lossen,schroef]).
mwu_postag('op losse schroeven',['VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[op,los,schroef]).
mwu_postag('op naam van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,naam,van]).
mwu_postag('op non actief',['VZ(init)','SPEC(vreemd)','ADJ(vrij,basis,zonder)'],[op,non,actief]).
mwu_postag('op poten',['VZ(init)','N(soort,mv,basis)'],[op,poot]).
mwu_postag('op scherp',['VZ(init)','ADJ(vrij,basis,zonder)'],[op,scherp]).
mwu_postag('op slag',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,slag]).
mwu_postag('op til',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,til]).
mwu_postag('op uitnodiging van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,uitnodiging,van]).
mwu_postag('op verdenking van',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,verdenking,van]).
mwu_postag('op verzoek van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[op,verzoek,van]).
mwu_postag('Op weg',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,weg]).
mwu_postag('op weg',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[op,weg]).
mwu_postag('Op weg naar',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,weg,naar]).
mwu_postag('op weg naar',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[op,weg,naar]).
mwu_postag('op zichzelfstaande',['VZ(init)','ADJ(prenom,basis,met-e,stan)'],[op,op_zichzelf_staand]).
mwu_postag('op zoek',['VZ(init)','ADJ(vrij,basis,zonder)'],[op,zoek]).
mwu_postag('oud en nieuw',['ADJ(nom,basis,zonder,zonder-n)','VG(neven)','ADJ(nom,basis,zonder,zonder-n)'],[oud,en,nieuw]).
mwu_postag('orde op zaken',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)'],[orde,op,zaak]).
mwu_postag('over de',['VZ(init)','LID(bep,stan,rest)'],[over,de]).
mwu_postag('over de schreef',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[over,de,schreef]).
mwu_postag('over en weer',['VZ(fin)','VG(neven)','VZ(fin)'],[over,en,weer]).
mwu_postag('Palestijnse Gebieden',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['Palestijns',gebied]).
mwu_postag('pas op de plaats',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[pas,op,de,plaats]).
mwu_postag('Platform JGZ',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[platform,'JGZ']).
mwu_postag('Postbus 51-Infolijn',['SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)'],['Postbus','51-Infolijn']).
mwu_postag('Prins Alexander',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prins,'Alexander']).
mwu_postag('Prins Aymeric',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prins,'Aymeric']).
mwu_postag('Prinsbisdom Luik',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[prinsbisdom,'Luik']).
mwu_postag('Prinsdom Luik',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[prinsdom,'Luik']).
mwu_postag('Prinses Claire',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prinses,'Claire']).
mwu_postag('Prinses Laetitia Maria',['N(soort,ev,basis,zijd,stan)','SPEC(deeleigen)','SPEC(deeleigen)'],[prinses,'Laetitia','Maria']).
mwu_postag('Prinses Louise',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prinses,'Louise']).
mwu_postag('Prinses Luisa Maria',['N(soort,ev,basis,zijd,stan)','SPEC(deeleigen)','SPEC(deeleigen)'],[prinses,'Luisa','Maria']).
mwu_postag('Prinses Margriet-kanaal',['SPEC(deeleigen)','SPEC(deeleigen)'],['Prinses','Margrietkanaal']).
mwu_postag('Prinses Maria-Esmeralda',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prinses,'Maria-Esmeralda']).
mwu_postag('Prinses Maria Laura',['N(soort,ev,basis,zijd,stan)','SPEC(deeleigen)','N(eigen,ev,basis,zijd,stan)'],[prinses,'Maria','Laura']).
mwu_postag('Prinses Marie-Christine',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prinses,'Marie-Christine']).
mwu_postag('Prinses van België',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[prinses,van,'België']).
mwu_postag('Prinses van Bohemen en Hongarije',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)','VG(neven)','N(eigen,ev,basis,onz,stan)'],[prinses,van,'Bohemen',en,'Hongarije']).
mwu_postag('Prinses van Réthy',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[prinses,van,'Réthy']).
mwu_postag('Prins Filip',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prins,'Filip']).
mwu_postag('Prins Joachim',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prins,'Joachim']).
mwu_postag('Prins Laurent',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[prins,'Laurent']).
mwu_postag('Prins Nicolas',['N(soort,ev,basis,zijd,stan)','SPEC(deeleigen)'],[prins,'Nicolas']).
mwu_postag('Prins van België',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[prins,van,'België']).
mwu_postag('Prins van Luik',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[prins,van,'Luik']).
mwu_postag('Prins van Réthy',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[prins,van,'Réthy']).
mwu_postag('Prins Willem-Alexanderhof',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)'],[prins,'Willem-Alexanderhof']).
mwu_postag('Provinciaalse Zeeuwse Courant',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Provinciaals','Zeeuws',courant]).
mwu_postag('Rad van Fortuin',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,onz,stan)'],[rad,van,fortuin]).
mwu_postag('Regio Brussel',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)'],[regio,'Brussel']).
mwu_postag('rock en roll',['SPEC(vreemd)','VG(neven)','SPEC(vreemd)'],[rock,en,roll]).
mwu_postag('Rode Brigades',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[rood,brigade]).
mwu_postag('Rode Duivels',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[rood,duivel]).
mwu_postag('rond de',['VZ(init)','LID(bep,stan,rest)'],[rond,de]).
mwu_postag('Rooms katholieke',['ADJ(vrij,basis,zonder)','ADJ(prenom,basis,met-e,stan)'],['Rooms',katholiek]).
mwu_postag('Rotterdam Centraal',['N(eigen,ev,basis,onz,stan)','ADJ(vrij,basis,zonder)'],['Rotterdam',centraal]).
mwu_postag('Ruimtelijk Structuurplan Vlaanderen',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[ruimtelijk,structuur_plan,'Vlaanderen']).
mwu_postag('Ruimtelijk Structuurplan Vlaanderen',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[ruimtelijk,structuurplan,'Vlaanderen']).
mwu_postag('Russische Revolutie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Russisch',revolutie]).
mwu_postag('rustig aan',['ADJ(vrij,basis,zonder)','VZ(fin)'],[rustig,aan]).
mwu_postag('\'s anderdaags',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,anderdaags]).
mwu_postag('\'s anderendaags',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,anderendaags]).
mwu_postag('\'s avonds',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,avond]).
mwu_postag('\'s Avonds',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,avond]).
mwu_postag('Slag bij Chattanooga',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[slag,bij,'Chattanooga']).
mwu_postag('Slag bij Gettysburg',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[slag,bij,'Gettysburg']).
mwu_postag('Slag bij Shiloh',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,zijd,stan)'],[slag,bij,'Shiloh']).
mwu_postag('Slag in de Wildernis',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[slag,in,de,wildernis]).
mwu_postag('Slag op het Lechveld',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[slag,op,het,'Lechveld']).
mwu_postag('\'s land',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,land]).
mwu_postag('\'s Land',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,land]).
mwu_postag('\'s middags',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,middag]).
mwu_postag('\'s morgens',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,morgen]).
mwu_postag('\'s nachts',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,nacht]).
mwu_postag('s nachts',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,nacht]).
mwu_postag('\'s Nachts',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,nacht]).
mwu_postag('\'s ochtends',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,ochtend]).
mwu_postag('\'s rijk',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,rijk]).
mwu_postag('\'s rijk',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,rijk]).
mwu_postag('\'s koning',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,koning]).
mwu_postag('\'s koning',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,koning]).
mwu_postag('\'s keizer',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,keizer]).
mwu_postag('\'s keizer',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,keizer]).
mwu_postag('\'s lezer',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,lezer]).
mwu_postag('\'s lezer',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,lezer]).
mwu_postag('\'s liefde',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,liefde]).
mwu_postag('\'s liefde',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,liefde]).
mwu_postag('\'s man',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,man]).
mwu_postag('\'s man',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,man]).
mwu_postag('\'s mens',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,mens]).
mwu_postag('\'s mens',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,mens]).
mwu_postag('\'s wereld',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,wereld]).
mwu_postag('\'s wereld',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,wereld]).
mwu_postag('\'s woensdagavonds',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,woensdag_avond]).
mwu_postag('\'s zaterdagavonds',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,zaterdag_avond]).
mwu_postag('\'s zondags',['LID(bep,gen,evmo)','N(eigen,ev,basis,gen)'],[de,zondag]).
mwu_postag('sinds dat',['VG(onder)','VG(onder)'],[sinds,dat]).
mwu_postag('Sociaal-Economisch Rapport',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['sociaal-economisch',rapport]).
mwu_postag('Sociaal-Economisch Rapport Vlaanderen 2007',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)','TW(hoofd,vrij)'],['sociaal-economisch',rapport,'Vlaanderen','2007']).
mwu_postag('Sociale Economie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[sociaal,economie]).
mwu_postag('Sociale Huisvestingsmaatschappijen',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[sociale,huisvestingsmaatschappij]).
mwu_postag('Sociale Hygiëne',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[sociaal,hygiene]).
mwu_postag('Sociale Integratie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[sociaal,integratie]).
mwu_postag('Sociale Voorzorg',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[sociaal,voorzorg]).
mwu_postag('Sociale Zaken',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[sociaal,zaak]).
mwu_postag('Sociale Zaken en Volksgezondheid',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[sociaal,zaak,en,volksgezondheid]).
mwu_postag('Sociale Zaken en Werkgelegenheid',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[sociaal,zaak,en,werkgelegenheid]).
mwu_postag('Sociale Zekerheid',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[sociaal,zekerheid]).
mwu_postag('SP.a - Spirit',['N(eigen,ev,basis,zijd,stan)','LET()','N(eigen,ev,basis,genus,stan)'],['sp.a',-,spirit]).
mwu_postag('Sportvrouw van het jaar',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[sportvrouw,van,het,jaar]).
mwu_postag('Sportvrouw van het Jaar',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[sportvrouw,van,het,jaar]).
mwu_postag('Sri Lankaanse',['SPEC(deeleigen)','ADJ(prenom,basis,met-e,stan)'],['Sri','Lankaans']).
mwu_postag('Sri Lankese',['SPEC(deeleigen)','ADJ(prenom,basis,met-e,stan)'],['Sri','Lankees']).
mwu_postag('Stadion Galgenwaard',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,zijd,stan)'],[stadion,'Galgenwaard']).
mwu_postag('stap voor stap',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[stap,voor,stap]).
mwu_postag('stapje voor stapje',['N(soort,ev,dim,onz,stan)','VZ(init)','N(soort,ev,dim,onz,stan)'],[stap,voor,stap]).
mwu_postag('steen des aanstoots',['N(soort,ev,basis,zijd,stan)','LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[steen,de,aanstoot]).
mwu_postag('stukje bij beetje',['N(soort,ev,dim,onz,stan)','VZ(init)','N(soort,ev,dim,onz,stan)'],[stuk,bij,beetje]).
mwu_postag('Suske en Wiske',['N(eigen,ev,basis,zijd,stan)','VG(neven)','N(eigen,ev,basis,zijd,stan)'],['Suske',en,'Wiske']).
mwu_postag('Suske en Wiske-avonturen',['N(eigen,ev,basis,zijd,stan)','VG(neven)','N(soort,mv,basis)'],['Suske',en,'Wiske_avontuur']).
mwu_postag('Suske en Wiske-strip',['N(eigen,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],['Suske',en,'Wiske_strip']).
mwu_postag('Suske en Wiskeverhalen',['N(eigen,ev,basis,zijd,stan)','VG(neven)','N(soort,mv,basis)'],['Suske',en,'Wiskeverhaal']).
mwu_postag('\'s wereld',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,wereld]).
mwu_postag('\'s werelds',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,wereld]).
mwu_postag('\'s winters',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,winter]).
mwu_postag('\'s zaterdags',['LID(bep,gen,evmo)','N(eigen,ev,basis,gen)'],[de,zaterdag]).
mwu_postag('\'s maandags',['LID(bep,gen,evmo)','N(eigen,ev,basis,gen)'],[de,maandag]).
mwu_postag('\'s woensdags',['LID(bep,gen,evmo)','N(eigen,ev,basis,gen)'],[de,woensdag]).
mwu_postag('\'s zondags',['LID(bep,gen,evmo)','N(eigen,ev,basis,gen)'],[de,zondag]).
mwu_postag('\'s zomers',['LID(bep,gen,evmo)','N(soort,ev,basis,gen)'],[de,zomer]).
mwu_postag('\'s anderendaags',['LID(bep,gen,evmo)','BW()'],[de,anderendaags]).
mwu_postag('Tante Sidonia',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[tante,'Sidonia']).
mwu_postag('te allen tijde',['VZ(init)','VNW(onbep,det,dat,prenom,met-e,evmo)','N(soort,ev,basis,dat)'],[te,al,tijd]).
mwu_postag('te berde',['VZ(init)','N(soort,ev,basis,dat)'],[te,berde]).
mwu_postag('te boven',['VZ(init)','VZ(fin)'],[te,boven]).
mwu_postag('te buiten',['VZ(init)','VZ(fin)'],[te,buiten]).
mwu_postag('te gast',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[te,gast]).
mwu_postag('te gelegenertijd',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[te,gelegenertijd]).
mwu_postag('te goed',['VZ(init)','ADJ(vrij,basis,zonder)'],[te,goed]).
mwu_postag('te gortig',['BW()','ADJ(vrij,basis,zonder)'],[te,gortig]).
mwu_postag('te gronde',['VZ(init)','N(soort,ev,basis,dat)'],[te,grond]).
mwu_postag('te huis',['VZ(init)','N(soort,ev,basis,onz,stan)'],[te,huis]).
mwu_postag('te huur',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[te,huur]).
mwu_postag('te koop',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[te,koop]).
mwu_postag('te kort',['VZ(init)','ADJ(vrij,basis,zonder)'],[te,kort]).
mwu_postag('te leen',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[te,leen]).
mwu_postag('te meer',['BW()','VNW(onbep,grad,stan,vrij,zonder,comp)'],[te,veel]).
mwu_postag('te midden van',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,midden,van]).
mwu_postag('te na',['BW()','VZ(fin)'],[te,na]).
mwu_postag('te pakken',['VZ(init)','WW(inf,vrij,zonder)'],[te,pakken]).
mwu_postag('te pas',['VZ(init)','BW()'],[te,pas]).
mwu_postag('te onpas',['VZ(init)','BW()'],[te,onpas]).
mwu_postag('te over',['VZ(init)','VZ(fin)'],[te,over]).
mwu_postag('te snel af',['VZ(fin)','ADJ(vrij,basis,zonder)','VZ(fin)'],[te,snel,af]).
mwu_postag('te voorschijn',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[te,voorschijn]).
mwu_postag('te water',['VZ(init)','N(soort,ev,basis,onz,stan)'],[te,water]).
mwu_postag('te weten',['VZ(init)','WW(inf,vrij,zonder)'],[te,weten]).
mwu_postag('te zamen',['VZ(init)','BW()'],[te,zamen]).
mwu_postag('te zijner tijd',['VZ(init)','VNW(bez,det,dat,vol,3,ev,prenom,met-e,evf)','N(soort,ev,basis,zijd,stan)'],[te,zijn,tijd]).
mwu_postag('tegen de',['VZ(init)','LID(bep,stan,rest)'],[tegen,de]).
mwu_postag('ten aan zien van',['VZ(versm)','N(soort,ev,basis,onz,stan)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,aan,zien,van]).
mwu_postag('ten aanzien van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,aanzien,van]).
mwu_postag('ten aanzien',['VZ(versm)','N(soort,ev,basis,onz,stan)'],[te,aanzien]).
mwu_postag('ten achter',['VZ(versm)','VZ(fin)'],[te,achter]).
mwu_postag('ten bate van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,baat,van]).
mwu_postag('ten behoeve van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,behoeve,van]).
mwu_postag('ten belope van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,beloop,van]).
mwu_postag('ten deel',['VZ(versm)','N(soort,ev,basis,onz,stan)'],[te,deel]).
mwu_postag('ten dele',['VZ(versm)','N(soort,ev,basis,dat)'],[te,deel]).
mwu_postag('ten derde',['VZ(versm)','TW(rang,nom,zonder-n)'],[te,drie]).
mwu_postag('ten detrimente van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,detriment,van]).
mwu_postag('ten dienste',['VZ(versm)','N(soort,ev,basis,dat)'],[te,dienst]).
mwu_postag('ten diepste',['VZ(versm)','ADJ(nom,sup,met-e,zonder-n,stan)'],[te,diep]).
mwu_postag('ten doel',['VZ(versm)','N(soort,ev,basis,onz,stan)'],[te,doel]).
mwu_postag('ten einde',['VZ(versm)','N(soort,ev,basis,onz,stan)'],[te,einde]).
mwu_postag('ten enenmale',['VZ(versm)','N(soort,ev,basis,dat)'],[te,enenmale]).
mwu_postag('ten faveure van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,faveur,van]).
mwu_postag('ten gepasten tijde',['VZ(versm)','ADJ(prenom,basis,met-e,bijz)','N(soort,ev,basis,dat)'],[te,gepast,tijd]).
mwu_postag('ten gehore',['VZ(versm)','N(soort,ev,basis,dat)'],[te,gehoor]).
mwu_postag('ten gevolge van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,gevolg,van]).
mwu_postag('ten goede',['VZ(versm)','ADJ(nom,basis,met-e,zonder-n,bijz)'],[te,goed]).
mwu_postag('ten gronde',['VZ(versm)','N(soort,ev,basis,dat)'],[te,grond]).
mwu_postag('ten grondslag',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,grondslag]).
mwu_postag('ten gunste van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,gunst,van]).
mwu_postag('ten het westen van',['VZ(versm)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,het,westen,van]).
mwu_postag('ten hoogste',['VZ(versm)','ADJ(nom,sup,met-e,zonder-n,stan)'],[te,hoog]).
mwu_postag('ten koste van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,kost,van]).
mwu_postag('ten koste',['VZ(versm)','N(soort,ev,basis,dat)'],[te,kost]).
mwu_postag('ten lastelegging',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,lastelegging]).
mwu_postag('ten laste van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,last,van]).
mwu_postag('ten laste',['VZ(versm)','N(soort,ev,basis,dat)'],[te,last]).
mwu_postag('laste ten',['VZ(versm)','N(soort,ev,basis,dat)'],[te,last]).
mwu_postag('ten minste',['VZ(versm)','VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)'],[te,weinig]).
mwu_postag('ten name van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,naam,van]).
mwu_postag('ten noorden van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,noorden,van]).
mwu_postag('ten noordoosten van',['VZ(versm)','N(eigen,ev,basis,onz,stan)','VZ(init)'],[te,noordoosten,van]).
mwu_postag('ten noordwesten van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,noordwesten,van]).
mwu_postag('ten noord-oosten van',['VZ(versm)','N(eigen,ev,basis,onz,stan)','VZ(init)'],[te,noordoosten,van]).
mwu_postag('ten noord-westen van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,noordwesten,van]).
mwu_postag('ten onder',['VZ(versm)','VZ(fin)'],[te,onder]).
mwu_postag('ten ondergaan',['VZ(versm)','WW(inf,vrij,zonder)'],[te,onder_gaan]).
mwu_postag('ten ongerieve van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,ongerief,van]).
mwu_postag('ten onrechte',['VZ(versm)','N(soort,ev,basis,dat)'],[te,onrecht]).
mwu_postag('ten oosten van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,oosten,van]).
mwu_postag('ten opzichte van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,opzicht,van]).
mwu_postag('ten overstaan van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,overstaan,van]).
mwu_postag('ten overvloede',['VZ(versm)','N(soort,ev,basis,dat)'],[te,overvloed]).
mwu_postag('ten prooi',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,prooi]).
mwu_postag('ten slotte',['VZ(versm)','N(soort,ev,basis,dat)'],[te,slot]).
mwu_postag('ten spijt',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,spijt]).
mwu_postag('ten spoedigste',['VZ(versm)','ADJ(nom,sup,met-e,zonder-n,stan)'],[te,spoedig]).
mwu_postag('ten stelligste',['VZ(versm)','ADJ(nom,sup,met-e,zonder-n,stan)'],[te,stellig]).
mwu_postag('ten strijde',['VZ(versm)','N(soort,ev,basis,dat)'],[te,strijd]).
mwu_postag('ten tijde van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,tijd,van]).
mwu_postag('ten tonele',['VZ(versm)','N(soort,ev,basis,dat)'],[te,toneel]).
mwu_postag('ten uitvoer',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,uitvoer]).
mwu_postag('ten val',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,val]).
mwu_postag('ten velde',['VZ(versm)','N(soort,ev,basis,dat)'],[te,veld]).
mwu_postag('ten volle',['VZ(versm)','ADJ(nom,basis,met-e,zonder-n,bijz)'],[te,vol]).
mwu_postag('ten voordele van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,voordeel,van]).
mwu_postag('ten vroegste',['VZ(versm)','ADJ(nom,sup,met-e,zonder-n,stan)'],[te,vroeg]).
mwu_postag('ten westen van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,westen,van]).
mwu_postag('ten zeerste',['VZ(versm)','BW()'],[te,zeerste]).
mwu_postag('ten zuiden van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,zuiden,van]).
mwu_postag('ten zuidoosten van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,zuidoosten,van]).
mwu_postag('ten zuidwesten van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,zuidwesten,van]).
mwu_postag('ten zuid-oosten van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,zuidoosten,van]).
mwu_postag('ten zuid=westen van',['VZ(versm)','N(soort,ev,basis,onz,stan)','VZ(init)'],[te,zuidwesten,van]).
mwu_postag('te rade',['VZ(init)','N(soort,ev,basis,dat)'],[te,raad]).
mwu_postag('ter attentie van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,attentie,van]).
mwu_postag('ter behartiging van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,behartiging,van]).
mwu_postag('ter beschikking',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,beschikking]).
mwu_postag('ter bevordering van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,bevordering,van]).
mwu_postag('ter discussie',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,discussie]).
mwu_postag('ter dood',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,dood]).
mwu_postag('ter elfde ure',['VZ(versm)','TW(rang,prenom,bijz)','N(soort,ev,basis,dat)'],[te,elf,uur]).
mwu_postag('ter elfder ure',['VZ(versm)','TW(rang,prenom,bijz)','N(soort,ev,basis,dat)'],[te,elf,uur]).
mwu_postag('ter ere van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,eer,van]).
mwu_postag('ter gelegenheid van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,gelegenheid,van]).
mwu_postag('ter grootte van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,grootte,van]).
mwu_postag('ter hand',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,hand]).
mwu_postag('ter harte',['VZ(versm)','N(soort,ev,basis,dat)'],[te,hart]).
mwu_postag('ter herinnering aan',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,herinnering,aan]).
mwu_postag('ter hoogte van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,hoogte,van]).
mwu_postag('ter kennis',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,kennis]).
mwu_postag('ter nagedachtenis aan',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,nagedachtenis,aan]).
mwu_postag('Ter nagedachtenis aan',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,nagedachtenis,aan]).
mwu_postag('ter navolging',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,navolging]).
mwu_postag('ter plaatse',['VZ(versm)','N(soort,ev,basis,dat)'],[te,plaats]).
mwu_postag('plekke ter',['VZ(versm)','N(soort,ev,basis,dat)'],[te,plek]).
mwu_postag('ter sprake',['VZ(versm)','N(soort,ev,basis,dat)'],[te,spraak]).
mwu_postag('ter sprake',['VZ(versm)','N(soort,ev,basis,dat)'],[te,spraak]).
mwu_postag('ter uitvoering van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,uitvoering,van]).
mwu_postag('ter uitvoering',['VZ(versm)','N(soort,ev,basis,zijd,stan)'],[te,uitvoering]).
mwu_postag('ter vervanging van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,vervanging,van]).
mwu_postag('ter voorbereiding op',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,voorbereiding,op]).
mwu_postag('ter waarde van',['VZ(versm)','N(soort,ev,basis,zijd,stan)','VZ(init)'],[te,waarde,van]).
mwu_postag('ter weerszijden van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,weerszijde,van]).
mwu_postag('ter wille van',['VZ(versm)','N(soort,ev,basis,dat)','VZ(init)'],[te,wil,van]).
mwu_postag('terzake',['VZ(versm)','N(soort,ev,basis,dat)'],[te,zaak]).
mwu_postag(terzake,['VZ(versm)','N(soort,ev,basis,dat)'],[te,zaak]).
mwu_postag('ter ziele',['VZ(versm)','N(soort,ev,basis,dat)'],[te,ziel]).
mwu_postag('\'t koudst',['LID(bep,stan,evon)','ADJ(vrij,sup,zonder)'],[het,koud]).
mwu_postag('t / m',['SPEC(afk)','LET()','SPEC(afk)'],[tot,/,met]).
mwu_postag('The Artist Formerly Known As Prince',['SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)','SPEC(deeleigen)'],
	   ['The','Artist','Formerly','Known','As','Prince']).
mwu_postag('tot daar aan toe',['VZ(init)','VNW(aanw,adv-pron,obl,vol,3o,getal)','VZ(fin)','VZ(fin)'],[tot,daar,aan,toe]).
mwu_postag('tot dusver',['VZ(init)','BW()'],[tot,dusver]).
mwu_postag('tot dusverre',['VZ(init)','BW()'],[tot,dusverre]).
mwu_postag('tot en met',['VZ(init)','VG(neven)','VZ(init)'],[tot,en,met]).
mwu_postag('tot staan',['VZ(init)','WW(inf,vrij,zonder)'],[tot,staan]).
mwu_postag('tot vervelens toe',['VZ(init)','BW()','VZ(fin)'],[tot,vervelens,toe]).
mwu_postag('tot voor kort',['VZ(init)','VZ(init)','ADJ(vrij,basis,zonder)'],[tot,voor,kort]).
mwu_postag('tot ziens',['VZ(init)','TSW()'],[tot,ziens]).
mwu_postag('tot zinken',['VZ(init)','WW(inf,vrij,zonder)'],[tot,zinken]).
mwu_postag('Tweede Kamerfractie',['SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['Tweede','Kamer_fractie']).
mwu_postag('Tweede Kamerlid',['TW(rang,prenom,stan)','N(soort,ev,basis,onz,stan)'],[twee,kamer_lid]).
mwu_postag('Tweede Kamerverkiezingen',['TW(rang,prenom,stan)','N(soort,mv,basis)'],[twee,'Kamer_verkiezing']).
mwu_postag('Tweede Kamervoorzitter',['SPEC(deeleigen)','N(soort,ev,basis,zijd,stan)'],['Tweede','Kamer_voorzitter']).
mwu_postag('twee derde',['TW(hoofd,prenom,stan)','TW(rang,nom,zonder-n)'],[twee,drie]).
mwu_postag('Twee Vandaag',['TW(hoofd,vrij)','BW()'],[twee,vandaag]).
mwu_postag('uit de doeken',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[uit,de,doek]).
mwu_postag('uit den treure',['VZ(init)','LID(bep,dat,evmo)','N(soort,ev,basis,dat)'],[uit,de,treur]).
mwu_postag('uit de voeten',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[uit,de,voet]).
mwu_postag('uit de zeilen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[uit,de,zeil]).
mwu_postag('uit dien hoofde',['VZ(init)','VNW(aanw,det,dat,prenom,met-e,evmo)','N(soort,ev,basis,dat)'],[uit,die,hoofd]).
mwu_postag('uit het oogpunt van',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[uit,het,oogpunt,van]).
mwu_postag('uit het oogpunt van',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)','VZ(init)'],[uit,het,oogpunt,van]).
mwu_postag('uit hoofde van',['VZ(init)','N(soort,ev,basis,dat)','VZ(init)'],[uit,hoofd,van]).
mwu_postag('uit hoofde daarvan',['VZ(init)','N(soort,ev,basis,dat)','BW()'],[uit,hoofd,daarvan]).
mwu_postag('Universitaire Compagnie',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[universitair,compagnie]).
mwu_postag('Universitaire Pers Leuven',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,onz,stan)'],[universitair,pers,'Leuven']).
mwu_postag('Universitair Ziekenhuis Antwerpen',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[universitair,ziekenhuis,'Antwerpen']).
mwu_postag('Universitair Ziekenhuis Gent',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[universitair,ziekenhuis,'Gent']).
mwu_postag('Universitair Ziekenhuis van Leuven',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universitair,ziekenhuis,van,'Leuven']).
mwu_postag('Universiteit van Californië',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universiteit,van,'Californië']).
mwu_postag('Universiteit van Gent',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universiteit,van,'Gent']).
mwu_postag('Universiteit van Iowa',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universiteit,van,'Iowa']).
mwu_postag('Universiteit van Luik',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universiteit,van,'Luik']).
mwu_postag('Universiteit van Uppsala',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universiteit,van,'Uppsala']).
mwu_postag('Universiteit van Utrecht',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[universiteit,van,'Utrecht']).
mwu_postag('Van Dale Groot beeldwoordenboek',['SPEC(deeleigen)','SPEC(deeleigen)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Van','Dale',groot,beeld_woordenboek]).
mwu_postag('Van Dale Groot Beeldwoordenboek',['SPEC(deeleigen)','SPEC(deeleigen)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Van','Dale',groot,beeld_woordenboek]).
mwu_postag('Van Dale Groot leenwoordenboek',['SPEC(deeleigen)','SPEC(deeleigen)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Van','Dale',groot,leen_woordenboek]).
mwu_postag('Van Dale Groot Leenwoordenboek',['SPEC(deeleigen)','SPEC(deeleigen)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Van','Dale',groot,leen_woordenboek]).
mwu_postag('valsheid in geschrifte',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,dat)'],[valsheid,in,geschrift]).
mwu_postag('vandaag de dag',['BW()','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[vandaag,de,dag]).
mwu_postag('van alles',['VZ(init)','VNW(onbep,pron,stan,vol,3o,ev)'],[van,alles]).
mwu_postag('van dien aard',['VZ(init)','VNW(aanw,det,dat,prenom,met-e,evmo)','N(soort,ev,basis,zijd,stan)'],[van,die,aard]).
mwu_postag('van dien',['VZ(init)','VNW(aanw,det,dat,prenom,met-e,evmo)'],[van,die]).
mwu_postag('van een leien dakje',['VZ(init)','LID(onbep,stan,agr)','ADJ(prenom,basis,zonder)','N(soort,ev,dim,onz,stan)'],[van,een,leien,dak]).
mwu_postag('van gedachten',['VZ(init)','N(soort,mv,basis)'],[van,gedachte]).
mwu_postag('van harte',['VZ(init)','N(soort,ev,basis,dat)'],[van,hart]).
mwu_postag('van heb ik jou daar',['VZ(init)','WW(pv,tgw,ev)','VNW(pers,pron,nomin,vol,1,ev)','VNW(pers,pron,obl,vol,2v,ev)','VNW(aanw,adv-pron,obl,vol,3o,getal)'],[van,hebben,ik,jou,daar]).
mwu_postag('van heinde en verre',['VZ(init)','BW()','VG(neven)','BW()'],[van,heinde,en,verre]).
mwu_postag('van jewelste',['VZ(init)','BW()'],[van,jewelste]).
mwu_postag('van je welste',['VZ(init)','VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)','BW()'],[van,je,welste]).		
mwu_postag('van kracht',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[van,kracht]).
mwu_postag('van pas',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[van,pas]).
mwu_postag('van te voren',['VZ(init)','VZ(init)','BW()'],[van,te,voren]).
mwu_postag('van tevoren',['VZ(init)','BW()'],[van,tevoren]).
mwu_postag('van toepassing',['VZ(init)','N(soort,ev,basis,zijd,stan)'],[van,toepassing]).
mwu_postag('van tussenuit',['VZ(fin)','VZ(fin)'],[van,tussenuit]).
mwu_postag('van kwaad naar erger',['VZ(init)','N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(vrij,comp,zonder)'],[van,kwaad,naar,erg]).
mwu_postag('van week tot week',['VZ(init)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[van,week,tot,week]).
mwu_postag('van zins',['VZ(init)','N(soort,ev,basis,gen)'],[van,zin]).
mwu_postag('van zodra',['VZ(init)','VG(onder)'],[van,zodra]).
mwu_postag('Vendex / KBB-directie',['N(eigen,ev,basis,genus,stan)','LET()','N(soort,ev,basis,zijd,stan)'],['Vendex',/,'KBB_directie']).
mwu_postag('Verenigde Provinciën',['WW(vd,prenom,met-e)','N(soort,mv,basis)'],[verenigen,provincie]).
mwu_postag('Verkeer en Waterstat',['N(soort,ev,basis,onz,stan)','VG(neven)','SPEC(vreemd)'],[verkeer,en,'Waterstat']).
mwu_postag('Verklaring van Laken',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[verklaring,van,'Laken']).
mwu_postag('Verklaring van Londen',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[verklaring,van,'Londen']).
mwu_postag('Verklaring van München',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[verklaring,van,'München']).
mwu_postag('verre van',['BW()','VZ(init)'],[verre,van]).
mwu_postag('Vindplaats Trb',['N(soort,ev,basis,zijd,stan)','SPEC(symb)'],[vind_plaats,'Trb']).
mwu_postag('Vlaams Audiovisueel Fonds',['ADJ(prenom,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',audiovisueel,fonds]).
mwu_postag('Vlaams Blok-meeting',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,zijd,stan)'],['Vlaams','Blok_meeting']).
mwu_postag('Vlaamse Executieve',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams',executieve]).
mwu_postag('Vlaamse Infolijn',['ADJ(prenom,basis,met-e,stan)','N(eigen,ev,basis,zijd,stan)'],['Vlaams','Infolijn']).
mwu_postag('Vlaamse KMO',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams','KMO']).
mwu_postag('Vlaamse MiNa-Raad',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams','MiNa-raad']).
mwu_postag('Vlaamse Openbare Instellingen',['ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['Vlaams',openbaar,instelling]).
mwu_postag('Vlaamse Overheid',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams',overheid]).
mwu_postag('Vlaamse Parlement',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,onz,stan)'],['Vlaams',parlement]).
mwu_postag('Vlaamse Primitieven',['ADJ(prenom,basis,met-e,stan)','ADJ(nom,basis,met-e,mv-n)'],['Vlaams','Primitieven']).
mwu_postag('Vlaamse Rand',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams',rand]).
mwu_postag('Vlaamse Socialisten',['ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],['Vlaams',socialist]).
mwu_postag('Vlaamse Wetenschapsweek',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams',wetenschapsweek]).
mwu_postag('Vlaams Fonds',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',fonds]).
mwu_postag('Vlaams Fonds voor Sociale Integratie van Personen met een Handicap',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)','VZ(init)','LID(onbep,stan,agr)','N(soort,ev,basis,zijd,stan)'],['Vlaams',fonds,voor,sociaal,integratie,van,persoon,met,een,handicap]).
mwu_postag('Vlaams Gemeentefonds',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',gemeentefonds]).
mwu_postag('Vlaams Kartel',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',kartel]).
mwu_postag('Vlaams Klimaatbeleid',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',klimaatbeleid]).
mwu_postag('Vlaams Klimaatbeleidsplan',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',klimaatbeleidsplan]).
mwu_postag('Vlaams overleg Duurzame ontwikkeling',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams',overleg,duurzaam,ontwikkeling]).
mwu_postag('Vlaams Secretariaat van het Katholiek Onderwijs',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,evon)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',secretariaat,van,het,katholiek,onderwijs]).
mwu_postag('Vlaams structureel overleg voor Duurzame Ontwikkeling',['ADJ(vrij,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Vlaams',structureel,overleg,voor,duurzaam,ontwikkeling]).
mwu_postag('Vlaams Werkgelegenheidsakkoord',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Vlaams',werkgelegenheidsakkoord]).
mwu_postag('Vlaanderens Mooiste',['N(eigen,ev,basis,gen)','ADJ(nom,sup,met-e,zonder-n,stan)'],['Vlaanderen',mooi]).
mwu_postag('Vliegveld Valkenburg',['N(soort,ev,basis,onz,stan)','N(eigen,ev,basis,onz,stan)'],[vliegveld,'Valkenburg']).
mwu_postag('volgens zeggen',['VZ(init)','WW(inf,nom,zonder,zonder-n)'],[volgens,zeggen]).
mwu_postag('Volksfront voor de Bevrijding van Angola',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[volksfront,voor,de,bevrijding,van,'Angola']).
mwu_postag('Volksfront voor de Bevrijding van Palestina',['N(soort,ev,basis,onz,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[volksfront,voor,de,bevrijding,van,'Palestina']).
mwu_postag('Volksgezondheid en Milieuhygiëne',['N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[volksgezondheid,en,milieuhygiëne]).
mwu_postag('Volksgezondheid , Welzijn en Sport',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,onz,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[volksgezondheid,',',welzijn,en,sport]).
mwu_postag('Volkshuisvesting Ruimtelijke ordening en Milieubeheer',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[volkshuisvesting,ruimtelijk,ordening,en,milieu_beheer]).
mwu_postag('Volkshuisvesting Ruimtelijke Ordening en Milieubeheer',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[volkshuisvesting,ruimtelijk,ordening,en,milieubeheer]).
mwu_postag('Volkshuisvesting , Ruimtelijke Ordening en Milieubeheer',['N(soort,ev,basis,zijd,stan)','LET()','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[volkshuisvesting,',',ruimtelijk,ordening,en,milieubeheer]).
mwu_postag('Volksunie Jongeren',['N(eigen,ev,basis,zijd,stan)','ADJ(nom,comp,met-e,mv-n)'],['Volksunie',jong]).
mwu_postag('Voorbereidend Middelbaar Beroepsonderwijs',['WW(od,prenom,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[voorbereiden,middelbaar,beroepsonderwijs]).
mwu_postag('voor de hand',['VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[voor,de,hand]).
mwu_postag('voor de ogen',['VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[voor,de,oog]).
mwu_postag('voor gezien',['VZ(init)','WW(vd,vrij,zonder)'],[voor,zien]).
mwu_postag('voor het eerst',['VZ(init)','LID(bep,stan,evon)','TW(rang,nom,zonder-n)'],[voor,het,één]).
mwu_postag('voor het geval',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,basis,onz,stan)'],[voor,het,geval]).
mwu_postag('voor het éérst',['VZ(init)','LID(bep,stan,evon)','TW(rang,nom,zonder-n)'],[voor,het,één]).
mwu_postag('voor het laatst',['VZ(init)','LID(bep,stan,evon)','ADJ(vrij,sup,zonder)'],[voor,het,laat]).
mwu_postag('voor het lapje',['VZ(init)','LID(bep,stan,evon)','N(soort,ev,dim,onz,stan)'],[voor,het,lap]).
mwu_postag('voor het zeggen',['VZ(init)','LID(bep,stan,evon)','WW(inf,nom,zonder,zonder-n)'],[voor,het,zeggen]).
mwu_postag('voor in de plaats',['VZ(fin)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[voor,in,de,plaats]).
mwu_postag('voor lief',['VZ(init)','ADJ(vrij,basis,zonder)'],[voor,lief]).
mwu_postag('voor ogen',['VZ(init)','N(soort,mv,basis)'],[voor,oog]).
mwu_postag('voor en tegen',['N(soort,mv,basis)','VG(neven)','N(soort,mv,basis)'],[voor,en,tegen]).
mwu_postag('voor zover',['VZ(init)','BW()'],[voor,zover]).
mwu_postag('Voor zover',['VZ(init)','BW()'],[voor,zover]).
mwu_postag('Vrede van Aken',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Aken']).
mwu_postag('Vrede van Brest-Litovsk',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Brest-Litovsk']).
mwu_postag('Vrede van Karlowitz',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Karlowitz']).
mwu_postag('Vrede van Lausanne',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Lausanne']).
mwu_postag('Vrede van Lunéville',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Lunéville']).
mwu_postag('Vrede van Sevres',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Sevres']).
mwu_postag('Vrede van Sèvres',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Sèvres']).
mwu_postag('Vrede van Tilsit',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Tilsit']).
mwu_postag('Vrede van Venetië',['N(soort,ev,basis,zijd,stan)','VZ(init)','N(eigen,ev,basis,onz,stan)'],[vrede,van,'Venetië']).
mwu_postag('Vrouwe Justitia',['N(soort,ev,basis,zijd,stan)','N(eigen,ev,basis,zijd,stan)'],[vrouwe,'Justitia']).
mwu_postag('VSB Poezieprijs',['N(eigen,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],['VSB',poezieprijs]).
mwu_postag('Waalse en Brusselse Hoofdstedelijk Gewest',['ADJ(prenom,basis,met-e,stan)','VG(neven)','ADJ(prenom,basis,met-e,stan)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Waals',en,'Brussels',hoofdstedelijk,gewest]).
mwu_postag('Waalse en Brussels Hoofdstedelijk Gewest',['ADJ(prenom,basis,met-e,stan)','VG(neven)','ADJ(prenom,basis,zonder)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Waals',en,'Brussels',hoofdstedelijk,gewest]).
mwu_postag('Waalse milieuraad voor duurzame ontwikkeling',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],['Waals',milieu_raad,voor,duurzaam,ontwikkeling]).
mwu_postag('Waals Parlement',['ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],['Waals',parlement]).
mwu_postag('wat af',['VNW(onbep,pron,stan,vol,3o,ev)','VZ(fin)'],[wat,af]).
mwu_postag('wat betreft',['VNW(vb,pron,stan,vol,3o,ev)','WW(pv,tgw,met-t)'],[wat,betreffen]).
mwu_postag('wat een',['VNW(excl,pron,stan,vol,3,getal)','LID(onbep,stan,agr)'],[wat,een]).
mwu_postag('wat voor',['VNW(vb,pron,stan,vol,3o,ev)','VZ(init)'],[wat,voor]).
mwu_postag('wat voor een',['VNW(vb,pron,stan,vol,3o,ev)','VZ(init)','LID(onbep,stan,agr)'],[wat,voor,een]).
mwu_postag('wat voor één',['VNW(vb,pron,stan,vol,3o,ev)','VZ(init)','TW(hoofd,prenom,stan)'],[wat,voor,één]).
mwu_postag('wel en wee',['N(soort,ev,basis,onz,stan)','VG(neven)','N(soort,ev,basis,onz,stan)'],[wel,en,wee]).
mwu_postag('wel of geen',['BW()','VG(neven)','VNW(onbep,det,stan,prenom,zonder,agr)'],[wel,of,geen]).
mwu_postag('wel te verstaan',['BW()','VZ(init)','WW(inf,vrij,zonder)'],[wel,te,verstaan]).
mwu_postag(['Welzijn , Volksgezondheid en Cultuur'],
	   ['N(soort,ev,basis,onz,stan)',
	    'LET()',
	    'N(soort,ev,basis,zijd,stan)',
	    'VG(neven)',
	    'N(soort,ev,basis,zijd,stan)'],
	   [welzijn,',',volksgezondheid,en,cultuur]).
mwu_postag('Wereld Sociaal Forum',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,zonder)','N(soort,ev,basis,onz,stan)'],[wereld,sociaal,forum]).
mwu_postag('werkgevers- en werknemersorganisaties',['SPEC(afgebr)','VG(neven)','N(soort,mv,basis)'],['werkgevers-',en,werknemersorganisatie]).
mwu_postag('werkgroep Duurzame Ontwikkeling',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[werk_groep,duurzaam,ontwikkeling]).
mwu_postag('Wet Bescherming Persoonsgegevens',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,mv,basis)'],[wet,bescherming,persoongegeven]).
mwu_postag('Wet bescherming persoonsgegevens',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,mv,basis)'],[wet,bescherming,persoonsgegeven]).
mwu_postag('Wet Collectieve Preventie Volksgezondheid',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[wet,collectief,preventie,volksgezondheid]).
mwu_postag('Wetenschap in de kijker',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,ev,basis,zijd,stan)'],[wetenschap,in,de,kijker]).
mwu_postag('Wetenschap maakt knap',['N(soort,ev,basis,zijd,stan)','WW(pv,tgw,met-t)','ADJ(vrij,basis,zonder)'],[wetenschap,maken,knap]).
mwu_postag('Wetenschap , Technologie en Innovatie',['N(soort,ev,basis,zijd,stan)','LET()','N(soort,ev,basis,zijd,stan)','VG(neven)','N(soort,ev,basis,zijd,stan)'],[wetenschap,',',technologie,en,innovatie]).
mwu_postag('Wet Geneeskundige Behandelingsovereenkomst',['N(soort,ev,basis,zijd,stan)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[wet,geneeskundig,behandelingsovereenkomst]).
mwu_postag('Wet op de Geneeskundige Behandelingsovereenkomst',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[wet,op,de,geneeskundig,behandelingsovereenkomst]).
mwu_postag('Wet op de kansspelen',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','N(soort,mv,basis)'],[wet,op,de,kansspel]).
mwu_postag('Wet op de Medische Keuringen',['N(soort,ev,basis,zijd,stan)','VZ(init)','LID(bep,stan,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,mv,basis)'],[wet,op,de,medisch,keuring]).
mwu_postag('Wet Reïntegratie van Arbeidsgehandicapten',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','VZ(init)','N(soort,mv,basis)'],[wet,reïntegratie,van,abeidsgehandicapte]).
mwu_postag('Wet Rietkerk-uitkering',['SPEC(deeleigen)','SPEC(deeleigen)'],['Wet','Rietkerk_uitkering']).
mwu_postag('Wettelijke Aansprakelijkheid',['ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[wettelijk,aansprakelijkheid]).
mwu_postag('Wetten van De Morgan',['N(soort,mv,basis)','VZ(init)','SPEC(deeleigen)','SPEC(deeleigen)'],[wet,van,'De','Morgan']).
mwu_postag('Wet Verbetering Poortwachter',['N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)','N(soort,ev,basis,zijd,stan)'],[wet,verbetering,poortwachter]).
mwu_postag('wie o wie',['VNW(vb,pron,stan,vol,3p,getal)','TSW()','VNW(vb,pron,stan,vol,3p,getal)'],[wie,o,wie]).
mwu_postag('Wijs op Reis',['ADJ(vrij,basis,zonder)','VZ(init)','N(soort,ev,basis,zijd,stan)'],[wijs,op,reis]).
mwu_postag('win win',['WW(pv,tgw,ev)','WW(pv,tgw,ev)'],[winnen,winnen]).
mwu_postag('zegge en schrijve',['WW(pv,conj,ev)','VG(neven)','WW(pv,conj,ev)'],[zeggen,en,schrijven]).
mwu_postag('zeg maar',['WW(pv,tgw,ev)','BW()'],[zeggen,maar]).
mwu_postag('zich zelf',['VNW(refl,pron,obl,red,3,getal)','BW()'],[zich,zelf]).
mwu_postag('zij het',['WW(pv,conj,ev)','VNW(pers,pron,stan,red,3,ev,onz)'],[zijn,het]).
mwu_postag('Zijne Koninklijke Hoogheid',['VNW(bez,det,stan,vol,3m,ev,prenom,met-e,rest)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[zijn,koninklijk,hoogheid]).
mwu_postag('Zijne Koninklijke en Keizerlijke Hoogheid',['VNW(bez,det,stan,vol,3m,ev,prenom,met-e,rest)','ADJ(prenom,basis,met-e,stan)','VG(neven)','ADJ(prenom,basis,met-e,stan)','N(soort,ev,basis,zijd,stan)'],[zijn,koninklijk,en,keizerlijk,hoogheid]).
mwu_postag('zijns inziens',['VNW(bez,det,gen,vol,3,ev,prenom,zonder,evmo)','N(soort,ev,basis,gen)'],[zijn,in_zien]).
mwu_postag('zo af en toe',['BW()','VG(fin)','VG(neven)','VG(fin)'],[zo,af,en,toe]).
mwu_postag('zo gauw',['BW()','ADJ(vrij,basis,zonder)'],[zo,gauw]).
mwu_postag('zo ja',['BW()','TSW()'],[zo,ja]).
mwu_postag('zo meteen',['BW()','BW()'],[zo,meteen]).
mwu_postag('zo niet',['BW()','BW()'],[zo,niet]).
mwu_postag('zo nu en dan',['BW()','BW()','VG(neven)','BW()'],[zo,nu,en,dan]).
mwu_postag('zo ook',['BW()','BW()'],[zo,ook]).
mwu_postag('zo te zien',['BW()','VZ(init)','WW(inf,vrij,zonder)'],[zo,te,zien]).
mwu_postag('zonder meer',['VZ(init)','VNW(onbep,grad,stan,vrij,zonder,comp)'],[zonder,veel]).
mwu_postag('zulk een',['VNW(aanw,det,stan,vrij,zonder)','LID(onbep,stan,agr)'],[zulk,een]).
mwu_postag('ZYPREXA VELOTAB-tabletten',['SPEC(deeleigen)','N(soort,mv,basis)'],['ZYPREXA','VELOTAB_tablet']).

vreemd([],[]).
vreemd([W|T],[Tag|L]) :-
    (   punct(W)
    ->  Tag = 'LET()'
    ;   Tag = 'SPEC(vreemd)'
    ),
    vreemd(T,L).


vreemd('2001 : A Space Odyssey').
vreemd('2004 est').
vreemd('action man').
vreemd('action painting').
vreemd('ad absurdum').
vreemd('ad hoc').
vreemd('ad rem').
vreemd('ad valorem').
vreemd('a fortiori').
vreemd('Agent Site').
vreemd('air marshall').
vreemd('air marshalls').
vreemd('air marshals').
vreemd('à la').
vreemd('à la minute').
vreemd('All Stars').
vreemd('Alphabet St.').
vreemd('alter ego').
vreemd('amber box').
vreemd('American football').
vreemd('American Journal of Epidemiology').
vreemd('ancien régime').
vreemd('ancien régimete').
vreemd('An Inconvenient Truth').
vreemd('Annals of the Rheumatic Diseases').
vreemd('annus mirabilis').
vreemd('an sich').
vreemd('Applications on Demand').
vreemd('a priori').
vreemd('Archives of General Psychiatry').
vreemd('Army of Northern Virginia').
vreemd('Army of the Potomac').
vreemd('art deco').
vreemd('art déco').
vreemd('art de vivre').
vreemd('art nouveau').
vreemd('au fond').
vreemd('au sérieux').
vreemd('Autorité centrale communautaire').
vreemd('bad guy').
vreemd('Ballad of Hollis Brown').
vreemd('Battle of Britain').
vreemd('Battle of Gettysburg').
vreemd('beau monde').
vreemd('Behavioral Ecology and Sociobiology').
vreemd('Behavioral Research and Therapy').
vreemd('Behavioural Ecology and Sociobiology').
vreemd('best practice').
vreemd('best practices').
vreemd('BETA COMPLEET').
vreemd('bien commun').
vreemd('Big Brother').
vreemd('big buissiness').
vreemd('Big en Little Round Tops').
vreemd('billard russe').
vreemd('binge drinken').
vreemd('Biogas for Better Life').
vreemd('Black Album').
vreemd('Black Venus').
vreemd('Blowin\' in the Wind').
vreemd('blue box').
vreemd('blue chip rate').
vreemd('box tank').
vreemd('Budget of Paradoxes').
vreemd('business angel').
vreemd('business class').
vreemd('business lines').
vreemd('Business Partner').
vreemd('Business Partners').
vreemd('captain of industry').
vreemd('captains of industry').
vreemd('carbon sink').
vreemd('carte blanche').
vreemd('case mix').
vreemd('case study').
vreemd('cash & carry').
vreemd('catch phrases').
vreemd('ça va').
vreemd('cavalry runs').
vreemd('char de bataille').
vreemd('char léger').
vreemd('char lourd').
vreemd('checks and balances').
vreemd('chief executive officer').
vreemd('civil order').
vreemd('civil society').
vreemd('clicks & bricks').
vreemd('Closer than Ever').
vreemd('code civil').
vreemd('collegia musica').
vreemd('coming man').
vreemd('commander in chief').
vreemd('community workers').
vreemd('con amore').
vreemd('Concertación Democrática').
vreemd('concours hippique').
vreemd('condition humaine').
vreemd('connected home').
vreemd('consilium abeundi').
vreemd('consumer concern').
vreemd('consumer concerns').
vreemd('contradictio in terminis').
vreemd('cordon sanitaire').
vreemd('corporate governance').
vreemd('corporate services').
vreemd('corps diplomatique').
vreemd('country club').
vreemd('Coupe du monde').
vreemd('courtesy pass').
vreemd('Creativity , Humor and Imagery in Language').
vreemd('credentials evaluation').
vreemd('credit points').
vreemd('crème de menthe').
vreemd('cross country').
vreemd('cross over').
vreemd('Crystal Ball').
vreemd('curriculum vitae').
vreemd('custom made').
vreemd('Dance Valley').
vreemd('Deep Space Nine').
vreemd('de facto').
vreemd('dementia praecox').
vreemd('Democratic Voice of Birma').
vreemd('den Um').
vreemd('Den Um').
vreemd('Deutschsprachigen Gemeinschaft').
vreemd('development box').
vreemd('diabetes mellitus').
vreemd('Direction générale de l\'aide à la jeunesse').
vreemd('director general').
vreemd('Dirty Mind').
vreemd('Doha Development Agenda').
vreemd('Don\'t Look Back').
vreemd('double digit').
vreemd('Drang nach Osten').
vreemd('drotrecogin alfa').
vreemd('Drotrecogin alfa').
vreemd('dual use').
vreemd('du chef').
vreemd('early warning').
vreemd('economy class').
vreemd('email address').
vreemd('empire of evil').
vreemd('End of the Road').
vreemd('en masse').
vreemd('En masse').
vreemd('en passant').
vreemd('et cetera').
vreemd('Europ Aid').
vreemd('Europe Publicaties').
vreemd('Everything But Arms').
vreemd('evidence based').
vreemd('execeteur testementair').
vreemd('fair trade').
vreemd('fait divers').
vreemd('fast forward').
vreemd('fata morgana').
vreemd('fata morgana\'s').
vreemd('Financial Times').
vreemd('fine fleur').
vreemd('fine printing').
vreemd('First Corps').
vreemd('foramen obturatorium').
vreemd('For You').
vreemd('founding fathers').
vreemd('from France').
vreemd('front office').
vreemd('full colour').
vreemd('full time').
vreemd('fund management').
vreemd('gay parade').
vreemd('Gegen die Wand').
vreemd('Gender-related development index').
vreemd('general manager').
vreemd('girl power').
vreemd('girls only').
vreemd('Global Economic Prospects and the Developing Countries 2002').
vreemd('go between').
vreemd('good governance').
vreemd('Good Luck').
vreemd('government requests').
vreemd('Graffiti Bridge').
vreemd('grand batterie').
vreemd('grand café').
vreemd('grand chef').
vreemd('grande vedette').
vreemd('Grand Gala').
vreemd('Grand Jury').
vreemd('Grand Prix').
vreemd('Grand Prix du Disque').
vreemd('grand slam').
vreemd('Grand Slam').
vreemd('grand strategy').
vreemd('green box').
vreemd('grosso modo').
vreemd('Guinness Book of Records').
vreemd('Guinness Book of World Records').
vreemd('gyrus frontalis inferior').
vreemd('gyrus frontalis medius').
vreemd('gyrus fusiformis').
vreemd('gyrus supramarginalis').
vreemd('hands free').
vreemd('happy end').
vreemd('happy few').
vreemd('hard drugs').
vreemd('haute couture').
vreemd('haute couturier').
vreemd('haute cuisine').
vreemd('haute finance').
vreemd('hazard ratio').
vreemd('high pressure liquid chromatograph').
vreemd('Highway 61 Revisited').
vreemd('Hormones and Behavior').
vreemd('host friends').
vreemd('human capital management').
vreemd('Human development index').
vreemd('Human Development Report').
vreemd('human media interaction').
vreemd('Human poverty index').
vreemd('human resource management').
vreemd('human resources').
vreemd('human resources management').
vreemd('human rights monitors').
vreemd('IBM Human Capital Management').
vreemd('IBM Technology Collaboration Solutions').
vreemd('ICT Executives').
vreemd('Il neige sur Liège').
vreemd('im groszen ganzen').
vreemd('in casu').
vreemd('in extremis').
vreemd('information retrieval').
vreemd('in memoriam').
vreemd('In memoriam').
vreemd('in pectore').
vreemd('in petto').
vreemd('in residence').
vreemd('in spe').
vreemd('in vitro').
vreemd('institutional trading').
vreemd('Integrated Framework for Trade-related Technical Assistance').
vreemd('intensive care').
vreemd('International Student Barometer').
vreemd('Internet Service Providers').
vreemd('In the Valley of Elah').
vreemd('intra muros').
vreemd('island hopping').
vreemd('It\'s the economy , stupid').
vreemd('Jean de Bruges').
vreemd('jeu de boules').
vreemd('Joint Integrated Technical Assistance Programme').
vreemd('joint venture').
vreemd('Journal of Biological Chemistry').
vreemd('Journal of Clinical Endocrinology and Metabolism').
vreemd('Journal of Experimental Biology').
vreemd('Journal of Hepatology').
vreemd('Journal of Molecular Biology').
vreemd('Journal of Neuroendocrinology').
vreemd('Journal of the Acoustical Society of America').
vreemd('Journal of the Acoustic Society of America').
vreemd('Kingdom of Heaven').
vreemd('knock out').
vreemd('know how').
vreemd('La bastille').
vreemd('la Douce France').
vreemd('La Mort de Tintagiles').
vreemd('La Vita È Bella').
vreemd('lead donor').
vreemd('leader of tomorrow').
vreemd('lead nation').
vreemd('Le Chant du Rossignol').
vreemd('Les Arts Décoratiefs').
vreemd('Les bonbons').
vreemd('Les bourgeois').
vreemd('Les Echos').
vreemd('Les flamandes').
vreemd('les zoreilles').
vreemd('lettres de foire').
vreemd('level playing field').
vreemd('Like A Rolling Stone').
vreemd('limited edition').
vreemd('linea recta').
vreemd('Line Out').
vreemd('lingua franca').
vreemd('Little Round Top').
vreemd('Lord Brougham').
vreemd('Lord of War').
vreemd('love peace en happiness').
vreemd('low bat').
vreemd('Mai 40').
vreemd('mala fide').
vreemd('managing director').
vreemd('Manufacture Belge de Lampes Electriques').
vreemd('Market Access for Developing Country Exports - selected issues').
vreemd('martial arts').
vreemd('Masterly Inactivity').
vreemd('Mein Kampf').
vreemd('minimal art').
vreemd('Ministère de la Communauté française').
vreemd('Ministerium der Deutschsprachigen Gemeinschaft').
vreemd('missing link').
vreemd('missing links').
vreemd('MISSING PARA').
vreemd('mission statement').
vreemd('model validation').
vreemd('modus operandi').
vreemd('Mons Pietatis').
vreemd('monstre sacré').
vreemd('More from People').
vreemd('mother fucker').
vreemd('Moulin Rouge').
vreemd('multiple sclerose').
vreemd('Ne me quitte pas').
vreemd('New Materials Venturing Day').
vreemd('New Scientist').
vreemd('New York Times').
vreemd('noblesse oblige').
vreemd('non compos mentis').
vreemd('non-state actors').
vreemd('North by Northwest').
vreemd('North Country Blues').
vreemd('North Sea Survey').
vreemd('nota bene').
vreemd('not allowed').
vreemd('not amused').
vreemd('not out').
vreemd('not-state actors').
vreemd('Nouvelle Vague').
vreemd('nucleus accumbens').
vreemd('Ode to Napoleon').
vreemd('Official Development Assistance').
vreemd('off shoring').
vreemd('On Demand business').
vreemd('On Demand Business').
vreemd('One Nite Alone').
vreemd('One Nite Alone Live').
vreemd('one-off measures').
vreemd('on line').
vreemd('on the spot').
vreemd('open source').
vreemd('over the top').
vreemd('Pals Battalions').
vreemd('pars pro toto').
vreemd('pay by view').
vreemd('peace clause').
vreemd('period rooms').
vreemd('perpetua mobiles').
vreemd('per se').
vreemd('petite histoire').
vreemd('Physiology and Behavior').
vreemd('Pickett\'s Charge').
vreemd('Plum Run').
vreemd('pop up').
vreemd('pop ups').
vreemd('practical joke').
vreemd('practical jokes').
vreemd('premium product').
vreemd('price bands').
vreemd('Prière païenne').
vreemd('prima ballerina').
vreemd('prime rate').
vreemd('prime time').
vreemd('Prime time').
vreemd('prime times').
vreemd('private banker').
vreemd('private bankers').
vreemd('Private Bankers').
vreemd('Private Banking').
vreemd('Proceedings of the National Academy of Sciences').
vreemd('promotor fidei').
vreemd('pro-poor growth').
vreemd('prunus anneniaca').
vreemd('Pulp Fiction').
vreemd('Purple Rain').
vreemd('Purple Rain Tour').
vreemd('quality time').
vreemd('Quand maman reviendra').
vreemd('Quand on n\'a que l\'amour').
vreemd('Quatre Bras').
vreemd('Queen of Mean').
vreemd('raison d\'être').
vreemd('Rat der Volksbeauftragten').
vreemd('Rave Un2 the Joy Fantastic').
vreemd('rector magnificus').
vreemd('Rector Magnificus').
vreemd('reductio ad absurdum').
vreemd('Regional Surveys of the World').
vreemd('remote control').
vreemd('remote controls').
vreemd('retail banking').
vreemd('right policy mix').
vreemd('rock and roll').
vreemd('rule of law').
vreemd('running joke').
vreemd('running mate').
vreemd('salle d\'applications').
vreemd('science fiction').
vreemd('Science fiction').
vreemd('Science Fiction').
vreemd('Seasons in the sun').
vreemd('second opinion').
vreemd('Secret Story').
vreemd('Security code').
vreemd('sede vacante').
vreemd('Senior Business').
vreemd('sense of urgency').
vreemd('sensu stricto').
vreemd('sentinel node').
vreemd('serotonin-norepinephrine reuptake inhibitor').
vreemd('Service Oriented Architecture').
vreemd('shared services').
vreemd('shopping center').
vreemd('Sign " O " The Times').
vreemd('signum bello').
vreemd('silver cushions').
vreemd('sine regno').
vreemd('SKETCH ARTIST').
vreemd('slow motion').
vreemd('slow starter').
vreemd('smoking gun').
vreemd('sneak preview').
vreemd('soft drugs').
vreemd('soft skills').
vreemd('space shuttle').
vreemd('standard setters').
vreemd('standard takers').
vreemd('Star Trek').
vreemd('Star Trek : The Motion Picture').
vreemd('State of the Union').
vreemd('status epilepticus').
vreemd('status quo').
vreemd('status quo ante').
vreemd('Stranger in my house').
vreemd('strategic policy unit').
vreemd('stricto sensu').
vreemd('student union').
vreemd('student unions').
vreemd('success stories').
vreemd('Sunday Telegraph').
vreemd('surat jalan').
vreemd('surround sound').
vreemd('survival syndrome').
vreemd('Sustainable Trade and Innovation Centre').
vreemd('Symbol Album').
vreemd('tabula rasa').
vreemd('tailor made').
vreemd('task force').
vreemd('Task Force').
vreemd('Technology Upgrade').
vreemd('Tequila Effect').
vreemd('thank you').
vreemd('The Amazing Adventures of Kavalier and Clay').
vreemd('the Army of Northern Virginia').
vreemd('the Army of the Potomac').
vreemd('The Army of the Potomac').
vreemd('The Astrophysical Journal').
vreemd('The Atlantic Monthly').
vreemd('The Basement Tapes').
vreemd('The Battle of Arnhem').
vreemd('The Bootleg Series 5').
vreemd('The Boston Globe').
vreemd('The Cross').
vreemd('The Daily Express').
vreemd('The Differential and Integral Calculus').
vreemd('The Economist').
vreemd('The end of the world').
vreemd('The Evening Standard').
vreemd('The Game').
vreemd('The Gold Experience').
vreemd('The Gold Experience Tour').
vreemd('the Guardian').
vreemd('The Guardian').
vreemd('The Independant').
vreemd('The Independent').
vreemd('The Jungle Book').
vreemd('The Language Blueprint').
vreemd('The Lonesome Death of Hattie Caroll').
vreemd('The lovers').
vreemd('The Most Beautiful Girl In The World').
vreemd('The News of the World').
vreemd('The New York Daily News').
vreemd('The New Yorker').
vreemd('The New York Times').
vreemd('The Next Generation Films').
vreemd('The Observer').
vreemd('The Original Series').
vreemd('The Original Series Films').
vreemd('The passion of the Christ').
vreemd('The return of the king').
vreemd('The Sun').
vreemd('The Sunday Times').
vreemd('The trials of Oscar Wilde').
vreemd('The Truth').
vreemd('The Wall Street Journal').
vreemd('The Washington Post').
vreemd('The Wild Bunch').
vreemd('time trip').
vreemd('top secret').
vreemd('total loss').
vreemd('tour de force').
vreemd('tour d\'horizon').
vreemd('tour of duty').
vreemd('traveller cheque').
vreemd('traveller cheques').
vreemd('Trigonometry and Double Algebra').
vreemd('troy ounce').
vreemd('try out').
vreemd('ultimi habitatores mundi').
vreemd('under attack').
vreemd('Under The Cherry Moon').
vreemd('unfinished business').
vreemd('urbi et orbi').
vreemd('Urbi et Orbi').
vreemd('user interface').
vreemd('varlet de chambre').
vreemd('venture capitalist').
vreemd('vice versa').
vreemd('Vice versa').
vreemd('view contact').
vreemd('Villa des Roses').
vreemd('viola d\'amore').
vreemd('vota bronca').
vreemd('war for talent').
vreemd('warming up').
vreemd('wealth management').
vreemd('Web Gallery of Art').
vreemd('wet lease').
vreemd('wishful thinking').
vreemd('Women\'s Ordination Worldwide').
vreemd('Woodstock of Dance').
vreemd('World Economic Forum').
vreemd('World Port Tournament').
vreemd('World Social Forum').
vreemd('World Summit on Sustainable Development').
vreemd('You love it or you hate it').
vreemd('zero sum game').
