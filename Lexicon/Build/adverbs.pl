:- expects_dialect(sicstus).

m(Stem, adverb, PN) :-
    adverb(PN),
    (	atom(PN)
    ->	PN = Stem
    ;	hdrug_util:concat_all(PN,Stem,' ')
    ).

m('onder ander',adverb, [onder,andere]).
m('te veel',    adverb, [te,meer]). 

m('zijn in_zien',adverb,[zijns,inziens]).
m('zijn ondanks',adverb,[zijns,ondanks]).
m('hun in_zien',adverb,[huns,inziens]).
m('hun in_zien',adverb,[huns,ondanks]).
m('mijn in_zien',adverb,[mijns,inziens]).
m('mijn in_zien',adverb,[mijns,ondanks]).
m('ons in_zien',adverb,[ons,inziens]).
m('ons in_zien',adverb,[onzes,inziens]).

adverb([geen,bal]).
adverb([geen,barst]).
adverb([geen,biet]).
adverb([geen,donder]).
adverb([geen,ene,fuck]).
adverb([geen,fuck]).
adverb([geen,fluit]).
adverb([geen,haar]).
adverb([geen,hout]).
adverb([geen,klap]).
adverb([geen,lor]).
adverb([geen,ene,moer]).
adverb([geen,moer]).
adverb([geen,reet]).
adverb([geen,ene,reet]).
adverb([geen,ruk]).
adverb([geen,ene,ruk]).
adverb([geen,sikkepit]).
adverb([geen,snars]).
adverb([geen,sodeflikker]).
adverb([geen,spat]).
adverb([geen,steek]).
adverb([geen,zak]).
adverb([geen,zier]).

adverb([bij,elkaar]). % pre-nump

adverb([a,fortiori]).
adverb([a,posterio]).
adverb([a,posteriori]).
adverb([a,priori]).
adverb([à,la,minute]).
adverb([aan,den,lijve]).	% ondervinden/ervaren/meemaken
adverb(achtereen).
adverb(achtereenvolgens).
adverb(achterlangs).		% ?
adverb(achterover).             % ?
adverb([ad,fundum]).
adverb([ad,interim]).  % hij werd ad interim aangesteld
adverb(al).
adverb([al,bij,al]).
adverb([al,dan,niet]).
adverb([al,met,al]).
adverb([al,of,niet]).
adverb(aldus).
adverb(algauw).
adverb([all,out]).  % cricket ?
adverb([all,over,the,world]).
adverb(allang).
adverb(allengs).
adverb(allereerst).
adverb(allerliefst).  % ze lachte allerliefst =/= het allerliefste
adverb(allerminst).
adverb(allerwege).
adverb(allerwegen).
adverb(allesbehalve).
adverb(alleszins).
adverb(allicht).
adverb(almaar).
adverb(alras).
adverb([als,het,ware]).
adverb(alsdan).
adverb(alsjeblieft).
adverb(alsmaar).
adverb(alsnog).
adverb(alstublieft).
adverb(althans).
adverb(alvast).
adverb(alweer).
adverb(alzo).
adverb(amper).
adverb([an,sich]).
adverb(andermaal).
adverb(anderszins).
adverb(anderzijds).
adverb([at,random]).
adverb([au,bain,marie]).
adverb([au,'bain-marie']).
adverb([au,fond]).
adverb(azo).   % vlaams
%% adverb(begin).
%adverb(beetje).
adverb(beroepshalve).
adverb(bovenmate).
adverb([bij,de,vleet]).
adverb([bij,hoog,en,bij,laag]).
adverb([bij,lange]).
adverb([bij,lange,na]).
adverb([bij,tijd,en,wijle]).
adverb([bij,voorbaat]).
adverb(bijvoorbaat).
adverb(bijgeval). % ouderwets
adverb(bijgevolg).
adverb(bijkans).
adverb(bijtijds).
adverb([bij,tijds]).
adverb([bij,verre]).
adverb(bijwijlen).
adverb([bij,wijze,van]).
adverb([binnen,no,time]).
adverb(binnenslands).
adverb(binnensmonds).
adverb(bis).
adverb(botweg).
adverb(bovenal).
%% adverb(bovenuit). % ?R only? todo: van bovenuit (NP)
adverb(breeduit).
adverb(circa).
adverb([con,amore]).
adverb(contra).
adverb([contre,coeur]).
adverb([coûte,que,coûte]).
adverb('cross-over').
adverb([cum,laude]).
adverb(daarenboven).
adverb(daarentegen).
adverb(daareven).
adverb(daarzo).
adverb(deels).
adverb(dermate).
adverb(desalniettemin).
adverb(desgevraagd).
adverb(desnoods).
adverb(desondanks).
adverb(dienovereenkomstig).
adverb(dientengevolge).
adverb([dien,tengevolge]).
adverb(dolgraag).
adverb(domweg).
adverb([donders,goed]).
adverb(doodleuk).
adverb([door,en,door]).
adverb(dra).
adverb(duizendmaal).
adverb(dusdanig).
adverb(dusver).  
adverb(dusverre).
adverb([een,twee,drie]).
adverb('een-twee-drie').
adverb([een,weinig]).
adverb([wel,'\'s']).
adverb([eens,en,vooral]).
adverb([een,en,andermaal]).
adverb([één,en,andermaal]).
adverb([één,op,één]).
adverb(eensklaps).
adverb(eenvoudigweg).
adverb(eerdaags).
adverb(eerlijkheidshalve).
adverb(eerstdaags).  %VL
adverb(eerlang).
adverb(eerst).
adverb([om,ter,eerst]).
adverb([ei,zo,na]).
adverb([eigener,beweging]).
adverb([en,passant]).
adverb([en,plein,public]).
adverb([en,plein,public]).
adverb([en,plein,publique]).
adverb([en,taille,directe]).
adverb(enerzijds).
adverb(enigermate).
adverb(enigzins).
adverb(enigszins).
adverb(ergo).
adverb(eveneens).
adverb(evengoed).
adverb(evenmin).
adverb(evenzeer).
adverb(evenzo).
adverb([ex,aequo]).
adverb([ex,ante]).
adverb([ex,cathedra]).
adverb([ex,dividend]).
adverb([ex,nihilo]).
adverb([ex,officio]).
adverb([face,to,face]).
adverb([fast,forward]).
adverb([for,the,time,being]).
adverb(fluks).
adverb(gaandeweg).
adverb(gaarne).
adverb(geeneens).
adverb(geenszins).
adverb(gelijkelijk).
adverb([gelukkig,maar]).
adverb(gemakshalve).
adverb(gewapenderhand).
adverb(gewoonweg).
adverb([god,zij,dank]).
adverb(['God',zij,dank]).
adverb(goddank).
adverb(goddomme).
adverb(godverdomme).
adverb(gvd).
adverb(godbetert).
adverb([god,betert]).
adverb([god,betere,het]).
adverb([god,betere,'\'t']).
adverb(['God',betert]).
adverb(['God',betere,het]).
adverb(['God',betere,'\'t']).
adverb(godverdorie).
adverb(goeddeels).
adverb(grofweg).
adverb([grosso,modo]).
adverb(grotendeels).
adverb(haast).
adverb([half,om,half]).
adverb(halfkoers).
adverb(halsoverkop).
adverb([hals,over,kop]).
adverb([hap,snap]).
adverb(hapsnap).
adverb(hardop).
adverb(harerzijds).
adverb(hartstikke).
adverb(heden).
adverb([heen,en,weer]).
adverb('heen-en-weer').
adverb(helemaal).
adverb(heletijd).
adverb([de,heletijd]).
adverb(hieromtrent).
adverb(hierzo).
adverb(hoegenaamd).
adverb(honderdmaal).
adverb(hoogstens).
adverb(hooguit).
adverb([hors,concours]).
adverb('huis-aan-huis').
adverb(hunnerzijds).
adverb(idealiter).
adverb(idem).
adverb([idem,dito]).
adverb(iets).
adverb(ietsje).
adverb(ietske).  % vlaams
adverb(ietsjes).
adverb(ietwat).
adverb([im,groszen,ganzen]).
adverb(immer).
adverb([in,den,treure]).
adverb([in,feite]).
adverb([in,grote,getale]).
adverb([in,groten,getale]).
adverb([in,het,geheel]).
adverb([in,het,minst]).
adverb([in,no,time]).
adverb([in,tijds]).
adverb([in,wezen]).
adverb(inderhaast).
adverb(ineens).
adverb(inmiddels).
adverb(insgelijks).
adverb(integendeel).
adverb(inzonderheid).
adverb(inzover).
adverb(inzoverre).
adverb([keer,op,keer,op,keer]).
adverb(kortweg).
adverb([koste,wat,het,kost]).
adverb([koste,wat,kost]).
adverb([kostte,wat,het,kost]).
adverb([kostte,wat,kost]).
adverb([koste,wat,koste]).
adverb([kostte,wat,koste]).
adverb([kostte,wat,kostte]).
adverb([kost,wat,kost]).
adverb(krek).
adverb(kriskras).
adverb(laatstelijk).
adverb(languit).
adverb(langzaamaan).
adverb([last,but,not,least]).
adverb([linea,recta]).
adverb(linksom).
adverb(logischerwijs).
adverb(logischerwijze).
adverb(luide).
adverb(luidop).
adverb(maar).
adverb([maar,liefst]).
adverb(maarliefst).
adverb([maar,wat,aan]).  % we rommelen (maar) wat aan; aan is really particle
adverb(mede).
adverb(meermaals).
adverb(meermalen).
adverb(meestentijds).
adverb(menigmaal).
adverb(merendeels).
adverb([met,andere,woorden]).
adverb([met,name]).
adverb([met,mondjesmaat]).
adverb([met,verve]).
adverb(meteen).
adverb(metterdaad).
adverb(mettertijd).
adverb(middelerwijl).
adverb(mijnerzijds).
% adverb(minder).
adverb([min,of,meer]).
adverb(minstens).
adverb(mitsdien).
adverb(mitsgaders).
adverb(mogelijks). % Vlaams
adverb(mondjesmaat).
adverb(mordicus).   % + (op) tegen
adverb([naar,schatting]).
adverb(nagenoeg).
adverb(nauwelijks).
adverb(neer). % loc dir pred?
adverb(netzomin).
adverb(niet).
adverb([niet,in,het,minst]).
%adverb(niets).  % niets te maken hebben met..
adverb(nihil).
adverb(nimmer).
adverb(node).
adverb(nog).
adverb([nog,eens]).   % nog eens twintig mensen
adverb(nogal).
adverb(nogmaals).
adverb(normaliter).
adverb([nota,bene]).
adverb(notabene).
adverb(nou). % tmp?
adverb([nou,eenmaal]).
adverb([nu,eenmaal]).
adverb([om,en,bij]).		%VL "ongeveer"
adverb([om,en,bij,de]).		%VL "ongeveer"
adverb([om,en,om]).
adverb([om,en,rond]).	        %VL "ongeveer"
adverb([om,en,rond,de]).        %VL "ongeveer"
adverb(omstreeks).	      % pre-num "ongeveer" pre-year pre-amount
adverb([onder,anderen]).
adverb(onderuit).  % loc? dir? pred?
adverb(ondermeer).
adverb([onder,meer]).
adverb(ongaarne).
adverb([ons,ondanks]).
adverb([onverrichter,zake]).
adverb(onverrichterzake).
adverb(onwijs).
adverb(onzerzijds).
adverb([op,en,af]).
adverb([op,en,top]).
adverb([op,slag]).
adverb(opnieuw).
adverb(opzich).
adverb([over,en,weer]).
adverb('over-en-weer').
adverb(overall).  % Overall staat hij nu derde ...
adverb(overlaatst).
adverb(overnieuw).
adverb(overweg).
adverb([pak,hem,beet]).
adverb([pak,'\'m',beet]).
adverb(pakweg).
adverb(pal).
adverb(pardoes).
adverb(pas).
adverb([per,se]).
adverb(perse).
adverb(perslotte).
adverb(plompverloren).
adverb(plusminus).
adverb([pro,rata]).
adverb([quod,non]).
adverb(rakelings).
adverb(rechtens).
adverb(rechtsom).
adverb([rechttoe,rechtaan]).
adverb([recht,toe,recht,aan]).
adverb([recht,toe,',',recht,aan]).
adverb(reeds).
adverb(ronduit).
adverb(ruwweg).
adverb([sans,gêne]).
adverb(schier).
adverb([schots,en,scheef]).
adverb(schuins).
adverb(sebiet).
adverb(sec).
adverb(seffens).   % vlaams
adverb(sic).
adverb(simpelweg).
adverb([sine,die]). % Adjournment sine die means "without assigning a day for a further meeting or hearing"
adverb(slechts).
adverb(solo).
adverb(sowieso).
adverb(spelenderwijs).
adverb(spoorslags).
adverb([stante,pede]).
adverb(steeds).
adverb(stilaan).
adverb(stillekes).
adverb(stillekesaan).
adverb(stormenderhand).
adverb(straal).  % hij liep ons straal voorbij 
adverb([strijk,en,zet]).
adverb([stukje,bij,beetje]).
adverb(subiet).
adverb([summa,cum,laude]).
adverb([te,elfder,ure]).
adverb([te,elfer,ure]).
adverb([te,enen,male]).
adverb([te,hooi,en,te,gras]).
adverb([te,vuur,en,te,zwaard]).
adverb(temeer).	   %  requires sbar:  'dit is temeer een probleem, omdat ..'
adverb([te,alle,prijze]).
adverb([te,alle,prijzen]).
adverb([te,allen,prijze]).
adverb([te,allen,prijzen]).
adverb([ten,alle,prijze]).
adverb([ten,alle,prijzen]).
adverb([ten,allen,prijze]).
adverb([ten,allen,prijzen]).
adverb([te,alle,koste]).
adverb([te,alle,kosten]).
adverb([te,allen,koste]).
adverb([te,allen,kosten]).
adverb([ten,alle,koste]).
adverb([ten,alle,kosten]).
adverb([ten,allen,koste]).
adverb([ten,allen,kosten]).
adverb([te,allen,tijde]).
adverb([ten,alle,tijde]).
adverb([ten,allen,tijde]).
adverb([ten,alle,tijden]).
adverb([ten,allen,tijden]).
adverb([te,alle,tijde]).
adverb([te,allen,tijde]).
adverb([te,alle,tijden]).
adverb([te,allen,tijden]).
adverb([ten,dele]).
adverb([ten,derde,male]).
adverb([ten,derden,male]).
adverb([ten,eigen,bate]).
adverb([ten,enenmale]).
adverb([ten,ene,male]).
adverb([ten,enen,male]).
adverb([ten,gronde]).
adverb([ten,halve]).
adverb([ten,hele]).
adverb([ten,hoogste]).
adverb([ten,overvloede]).
adverb([ten,principale]).
adverb([ten,spoedigste]).
adverb([ten,stelligste]).
adverb([ten,tweede,male]).
adverb([ten,tweeden,male]).
adverb([ten,volle]).
adverb([ten,voeten,uit]).
adverb([ten,zeerste]).
adverb(tengevolge).
adverb([ter,elfde,ure]).
adverb([ter,elfder,ure]).
adverb(terdege).
adverb(ternauwernood).
adverb(terstond).
adverb(terwijl).
adverb(terzelfdertijd).
adverb(terzijde).
adverb(tevens).
adverb(tevoren).
adverb([te,voren]).
adverb(tezeer).
adverb(tezelfdertijd).
adverb(thans).
adverb(toch).
adverb(toevalligerwijs).
adverb(toevalligerwijze).
adverb([tot,in,den,treure]).
adverb([uit,den,treure]).
adverb(uitentreure).
adverb(uitentreuren).
adverb([uit,dien,hoofde]).
adverb([uit,en,te,na]).
adverb(uitermate).
adverb([ultra,vires]).
adverb(uwerzijds).
adverb([va,banque]).
adverb([van,ganser,harte]).
adverb([van,harte]).
adverb(vandaar).
adverb(vanouds).
adverb(vanzelf).
adverb([van,zelf]).
adverb(vanzelve).
adverb([van,zelve]).
adverb(veeleer).
adverb(veiligheidshalve).
adverb(verre).  % dit heeft verre de voerkeur
adverb(verreweg).
adverb(veruit).
adverb(vervolgens).
adverb([via,via]).
adverb([via,',',via]).
adverb('via-via').
adverb([vice,versa]).
adverb(vj).  %  vorig jaar; bij economische berichten
adverb(vlnr).
adverb('v.l.n.r.').
adverb(voetstoots).
adverb(volgaarne).
adverb(volop).
adverb(voluit).
adverb([voor,het,geval,dat]).
adverb(vooral).
adverb(vooralsnog).
adverb(voorbeurs).
adverb(vooreerst).
adverb(voorgoed).
adverb(voornamelijk).
adverb(voorshands).
adverb(voort).			% brabants, vlaams
adverb(voortijds).
adverb(voorts).
adverb(vrijwel).
adverb(wat).
adverb(weder).
adverb(wederom).
adverb(weer).
adverb(weeral).
adverb(weerom).
adverb(weldegelijk).
adverb(weldra).
adverb(weleens).
adverb(welgemoed).
adverb(welgeteld).  % omdat dit verslag welgeteld uit 3 kantjes bestaat
adverb(welhaast).
% adverb(welnu).  tag only...
adverb([wie,weet]).
adverb([wijd,en,zijd]).
adverb(wijselijk).
adverb(willens).
adverb([willens,en,wetens]).
adverb([willens,nillens]).
adverb(wonderwel).
adverb(zeer).
adverb(zeg).
adverb([zeg,maar]).         
adverb([zeg,maar,gerust]).  
adverb(zelfs).
adverb(zienderogen).
adverb([zonder,meer]).
adverb(zondermeer).
adverb([zij,het]). % er is al een zij het bescheiden begin gemaakt...
adverb(zijnerzijds).
adverb(zo).
adverb(['zo\'n',beetje]).
adverb(zoal).
adverb(zodus).
adverb(zoetjesaan).
adverb([zoetjes,aan]).
adverb(zogauw).
adverb([zo,goed,als]).
adverb([zogoed,als]).
adverb(zomaar).
adverb(zomin).   % only with 'net'
adverb(zonodig).
adverb(zoverre).
adverb(zowat).
adverb(zowel).  % vlaams
adverb(zozeer).
adverb(zozo).
adverb(zus).
adverb('zwart-wit').
