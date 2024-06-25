:- expects_dialect(sicstus).

:- discontiguous
    with_dt/3,
    m/3.

mem_eq(W0,W) :-
    (	var(W0)
    ->	fail
    ;   W0 = [_|_]
    ->	lists:member(W,W0)
    ;   W0=W
    ).

stem_from_surf(Surf,Stem) :-
    (   atom(Surf)
    ->  Surf = Stem
    ;   hdrug_util:concat_all(Surf,Stem,' ')
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% WITH_DT %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: create stems for other with_dt as well, such as those
%% defined in lex_more.pl!!!
m('NA',with_dt(Cat,Dt),Surf) :-
    with_dt(Surf,Cat,Dt).
m('NA',with_dt(Cat,Dt),Surf) :-
    extra:with_dt(Surf,Cat,Dt).
/* stem will be generated in decl.pl
    %% format(user_error,"~w~n",[with_dt(Surf,Cat,Dt)]),
    roots_from_dt(Dt,Roots0,[]),
    sort_not_unique(Roots0,Roots1),
    %% Daniel: Any chance of having duplicates? 
    %% GJ: Would it matter?
    %% GJ: YES. If a sentence contains "was" twice, then lookup
    %% is attempted of "was" once, which is slow (and perhaps could
    %% even succeed...
    hdrug_util:concat_all(Roots1,Stem,' ').
*/

with_dt([begrijp,me,niet,verkeerd],
	tag,
	dt(sv1,[hd=l(v_root(begrijp,begrijpen),verb(hebben,sg1,transitive),0,1),
		obj1=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,1,2),
		mod=dt(ap,[mod=l(niet,adverb,advp,2,3),
			   hd=l(verkeerd,adjective(no_e(adv)),3,4)])
	       ])
       ).

with_dt([ik,zou,zeggen],
	tag,
	dt(smain,[hd=l(v_root(zal,zullen),verb(hebben,past(sg),aux(inf)),1,2),
		  su=ix(SU,l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)),
		  vc=dt(inf,[hd=l(v_root(zeg,zeggen),verb(hebben,inf,tr_sbar),2,3),
			     su=ix(SU)				  
			     ])
		 ])).

with_dt([ik,moet,zeggen],
	tag,
	dt(smain,[hd=l(v_root(moet,moeten),verb(hebben,past(sg),modifier(aux(inf))),1,2),
		  su=ix(SU,l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)),
		  vc=dt(inf,[hd=l(v_root(zeg,zeggen),verb(hebben,inf,tr_sbar),2,3),
			     su=ix(SU)				  
			     ])
		 ])).

with_dt([ik,bedoel],
	tag,
	dt(smain,[hd=l(v_root(bedoel,bedoelen),verb(hebben,sg1,sbar),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)])
       ).

with_dt([ik,zei],
	tag,
	dt(smain,[hd=l(v_root(zeg,zeggen),verb(hebben,past(sg),tr_sbar),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)])
       ).

with_dt([ik,dacht],
	tag,
	dt(smain,[hd=l(v_root(denk,denken),verb(hebben,past(sg),sbar),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)])
       ).

with_dt([ik,denk],
	tag,
	dt(smain,[hd=l(v_root(denk,denken),verb(hebben,sg1,sbar),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)])
       ).

with_dt([ik,geef,toe],
	tag,
	dt(smain,[hd=l(v_root(geef_toe,toe_geven),verb(hebben,sg1,part_sbar(toe)),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		  svp=l(toe,particle(toe),pp,2,3)])
       ).

with_dt([je,denkt],
	tag,
	dt(smain,[hd=l(v_root(denk,denken),verb(hebben,sg3,sbar),1,2),
		  su=l(je,pronoun(nwh,je,sg,de,both,def,wkpro),np,0,1)])
       ).

with_dt([ik,weet],
	tag,
	dt(smain,[hd=l(v_root(weet,weten),verb(hebben,sg1,sbar),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1)])
       ).

with_dt([ik,weet,het],
	tag,
	dt(smain,[hd=l(v_root(weet,weten),verb(hebben,sg1,sbar),1,2),
		  su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		  obj1=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),np,2,3)])
       ).

with_dt([me,dunkt],
	verb(hebben,sg3,ninv(sbar_subj_no_het_tpart,sbar_subj_no_het_tpart)),
	dt(smain,[hd=l(v_root(dunk,dunken),verb(hebben,sg3,sbar_subj_so_np_no_het),1,2),
		  su=orig(su),
		  obj2=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,0,1)])).

with_dt([me,dunkt],
	verb(hebben,sg3,ninv(incorporated_subj_topic(intransitive),
			     incorporated_subj_topic(intransitive))),
	dt(smain,[hd=l(v_root(dunk,dunken),verb(hebben,sg3,fixed([no_subj,dat],passive)),1,2),
		  obj2=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,0,1)])).

with_dt([me,dunkt],
	verb(hebben,sg3,ninv(incorporated_subj_topic(transitive),
			     incorporated_subj_topic(transitive))),
	dt(smain,[hd=l(v_root(dunk,dunken),verb(hebben,sg3,fixed([no_subj,nt(acc),yt(dat)],passive)),1,2),
		  obj1=orig(obj1),
		  obj2=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,0,1)])).

%% subjunctive ...
with_dt([men,zie],
	verb(hebben,sg3,ninv(incorporated_subj_topic(transitive),
			     incorporated_subj_topic(transitive))),
	dt(smain,[hd=l(v_root(zie,zien),verb(hebben,subjunctive,transitive),1,2),
		  obj1=orig(obj1),
		  su=l(men,pronoun(nwh,thi,sg,de,nom,def),np,0,1)])).

%% Nominals...

with_dt([god,de,vader],
	np,
	dt(np,[hd=l(god,noun(de,count,sg),0,1),
	       app=dt(np,[hd=l(vader,noun(de,count,sg),2,3),
			  det=l(de,determiner(de),detp,1,2)])])).

with_dt([moeder,aarde],
	np,
	dt(np,[hd=l(moeder,noun(de,count,sg),0,1),
	       app=l(aarde,noun(de,count,sg),np,1,2)])).

with_dt([moeder,de,vrouw],
	np,
	dt(np,[hd=l(moeder,noun(de,count,sg),0,1),
	       app=dt(np,[hd=l(vrouw,noun(de,count,sg),2,3),
			  det=l(de,determiner(de),detp,1,2)])])).

with_dt([moeder,natuur],
	np,
	dt(np,[hd=l(moeder,noun(de,count,sg),0,1),
	       app=l(natuur,noun(de,count,sg),np,1,2)])).

with_dt([moedertje,natuur],
	np,
	dt(np,[hd=l(moeder_DIM,noun(het,count,sg),0,1),
	       app=l(natuur,noun(de,count,sg),np,1,2)])).


with_dt([zijns,gelijken],
	np,
	dt(np,[hd=l(gelijk,nominalized_adjective,1,2),
	       det=l(zijn,determiner(pron),detp,0,1)])).

with_dt([voorbedachte,rade],
	np,
	dt(np,[hd=l(raad,noun(de,count,sg),1,2),
	       mod=l(voorbedacht,adjective(e),ap,0,1)])).

with_dt([voorbedachten,rade],
	np,
	dt(np,[hd=l(raad,noun(de,count,sg),1,2),
	       mod=l(voorbedacht,adjective(e),ap,0,1)])).

with_dt([hare,majesteit],
	np,
	dt(np,[hd=l(majesteit,noun(de,count,sg),1,2),
	       det=l(haar,determiner(pron),detp,0,1)])).

with_dt([zijne,majesteit],
	np,
	dt(np,[hd=l(majesteit,noun(de,count,sg),1,2),
	       det=l(zijn,determiner(pron),detp,0,1)])).

with_dt([hare,majesteit,de,koningin],
	np,
	dt(np,[hd=l(majesteit,noun(de,count,sg),1,2),
	       det=l(haar,determiner(pron),detp,0,1),
	       app=dt(np,[hd=l(koningin,noun(de,count,sg),3,4),
			  det=l(de,determiner(de),detp,2,3)])])).

with_dt(['Hare','Majesteit',de,'Koningin'],
	np,
	dt(np,[hd=l(majesteit,noun(de,count,sg),1,2),
	       det=l(haar,determiner(pron),detp,0,1),
	       app=dt(np,[hd=l(koningin,noun(de,count,sg),3,4),
			  det=l(de,determiner(de),detp,2,3)])])).

with_dt([politiek,leiders],
	noun(de,count,pl),
	dt(np,[hd=l(leider,noun(de,count,pl),1,2),
	       mod=l(politiek,adjective(no_e(adv)),ap,0,1)
	      ])).

with_dt([artistiek,leiders],
	noun(de,count,pl),
	dt(np,[hd=l(leider,noun(de,count,pl),1,2),
	       mod=l(artistiek,adjective(no_e(adv)),ap,0,1)
	      ])).

with_dt([ons,mam],
	np,
	dt(np,[hd=l(mam,noun(de,count,sg),1,2),
	       det=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1)
	      ])).

with_dt([ons,pap],
	np,
	dt(np,[hd=l(pap,noun(de,count,sg),1,2),
	       det=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1)
	      ])).

with_dt([ons,ma],
	np,
	dt(np,[hd=l(ma,noun(de,count,sg),1,2),
	       det=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1)
	      ])).

with_dt([ons,pa],
	np,
	dt(np,[hd=l(pa,noun(de,count,sg),1,2),
	       det=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1)
	      ])).

with_dt([ons,vader],
	np,
	dt(np,[hd=l(vader,noun(de,count,sg),1,2),
	       det=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1)
	      ])).

with_dt([ons,moeder],
	np,
	dt(np,[hd=l(moeder,noun(de,count,sg),1,2),
	       det=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1)
	      ])).

with_dt([arme,ik],
	np,
	dt(np,[hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2),
	       mod=l(arm,adjective(e),ap,0,1)])).

with_dt([arme,jij],
	np,
	dt(np,[hd=l(jij,pronoun(nwh,je,sg,de,nom,def),1,2),
	       mod=l(arm,adjective(e),ap,0,1)])).

with_dt([alles,samen],
	adverb,
	dt(np,[hd=l(alles,noun(het,mass,sg),0,1),
	       mod=l(samen,adjective(postn_pred(padv)),ap,1,2)
	      ])).

with_dt([alles,tesamen],
	adverb,
	dt(np,[hd=l(alles,noun(het,mass,sg),0,1),
	       mod=l(tesamen,adjective(postn_pred(padv)),ap,1,2)
	      ])).

with_dt([alles,tezamen],
	adverb,
	dt(np,[hd=l(alles,noun(het,mass,sg),0,1),
	       mod=l(samen,adjective(postn_pred(padv)),ap,1,2)
	      ])).

with_dt([alles,samen],
	modal_adverb(noun_prep),
	dt(np,[hd=l(alles,noun(het,mass,sg),0,1),
	       mod=l(samen,adjective(postn_pred(padv)),ap,1,2)
	      ])).

with_dt([alles,tesamen],
	modal_adverb(noun_prep),
	dt(np,[hd=l(alles,noun(het,mass,sg),0,1),
	       mod=l(tesamen,adjective(postn_pred(padv)),ap,1,2)
	      ])).

with_dt([alles,tezamen],
	modal_adverb(noun_prep),
	dt(np,[hd=l(alles,noun(het,mass,sg),0,1),
	       mod=l(tezamen,adjective(postn_pred(padv)),ap,1,2)
	      ])).

m('te bewijzene',noun(both,count,sg),[te,bewijzene]).

%% must be stem 'zorg' otherwise not selected by 'zorgen maken'...
m(zorg,with_dt(
	np,
	dt(np,[hd=l(zorg,noun(de,count,pl),1,2),
	       det=l('zo\'n',determiner(een),detp,0,1)])),
	       ['zo\'n',zorgen]).

with_dt([nummer,één,positie],
	noun(de,count,sg),
	dt(np,[hd=l(positie,noun(de,count,sg),2,3),
	       mod=dt(np,[hd=l(nummer,noun(both,count,sg),0,1),
			  app=l(één,number(hoofd(sg_num)),detp,1,2)])])).

with_dt([de,heb],
	np,
	dt(np,[hd=l(heb,noun(de,count,sg),1,2),
	       det=l(de,determiner(de),detp,0,1)])).

with_dt([het,nu],
	np,
	dt(np,[hd=l(nu,tmp_adverb,1,2),
	       det=l(het,determiner(het),detp,0,1)])).


with_dt([meer,van,dat],  % VL
        np,
        dt(np,[hd=l(veel,adjective(meer),0,1),
               obj1=dt(pp,
                       [hd=l(van,preposition(van,[af,uit,vandaan,[af,aan]]),1,2),
                        obj1=l(dat,determiner(het,nwh,nmod,pro,nparg),np,2,3)])
              ])).

with_dt([de,buiten],  %VL
        np,
        dt(np,[det=l(de,determiner(de),detp,0,1),
               hd=l(buiten,noun(sg,count,de),1,2)])).


with_dt([lieve,allemaal],
	np,
	dt(np,[hd=l(allemaal,pronoun(nwh,thi,pl,de,both,indef),1,2),
	       mod=l(lief,adjective(e),ap,0,1)])).

with_dt([beste,allemaal],
	np,
	dt(np,[hd=l(allemaal,pronoun(nwh,thi,pl,de,both,indef),1,2),
	       mod=l(best,adjective(e),ap,0,1)])).

with_dt([beste,allen],
	np,
	dt(np,[hd=l(alle,pronoun(nwh,thi,pl,de,both,indef),1,2),
	       mod=l(best,adjective(e),ap,0,1)])).

with_dt([beste,iedereen],
	np,
	dt(np,[hd=l(iedereen,pronoun(nwh,thi,sg,de,both,def,strpro),1,2),
	       mod=l(best,adjective(e),ap,0,1)])).

m(aflevering,      post_n_n, aflevering).
m(artikel,         post_n_n, artikel).
m(deel,            post_n_n, deel).
m(lid,             post_n_n, lid).
m(nummer,          post_n_n, nummer).
m(nummers,         post_n_n, nummers).
m(nummero,         post_n_n, nummero).
m(nummero,         post_n_n, numero).
m(nummertje,       post_n_n, nummertje).
m('#',             post_n_n, '#'). % Symfonie # 6 , op.79 , voor jongenskooren orkest , 1963 ;
m(opus,            post_n_n, opus).
m(type,            post_n_n, type).
m(versie,          post_n_n, versie).

%%%%%%%%%%%%%%%%%%%%%
%%%% PUNCTUATION %%%%
%%%%%%%%%%%%%%%%%%%%%

m(Stem,punct(Sub),PN) :-
    punct(PN,Sub),
    stem_from_surf(PN,Stem).

punct('"',aanhaal_both).  % "
punct('\'',aanhaal_both).
punct('\'\'',aanhaal_both).
punct('\'\'\'',aanhaal_both).
punct('\`',aanhaal_links).
punct('``',aanhaal_links).
punct('‘',aanhaal_links).  % enkel rechts
punct('&#8220;',aanhaal_links).  % dubbele aanhalingstekens links
punct('“',aanhaal_links).   % dubbele aanhalingstekens links
punct(',,',aanhaal_links).
punct('‚',aanhaal_links). % 
punct('„',aanhaal_links).
punct('“',aanhaal_links).
punct('„',aanhaal_links).
punct('',aanhaal_links).
punct('',aanhaal_links).
punct('&#8221;',aanhaal_rechts). % idem rechts
punct('”',aanhaal_rechts). % idem rechts
punct('’',aanhaal_rechts).
punct('”',aanhaal_rechts).
punct('“',aanhaal_rechts).
punct(:,dubb_punt).
punct('(',haak_open).
punct(')',haak_sluit).
punct('{',haak_open).
punct('}',haak_sluit).
punct('[',haak_open).
punct(']',haak_sluit).
%punct('«',haak_open).
punct('«',aanhaal_links).
%punct('»',haak_sluit).
punct('»',aanhaal_rechts).
punct('',aanhaal_rechts).
punct('',aanhaal_rechts).

punct('<',haak_open).
punct('>',haak_sluit).
punct('‹',haak_open).
punct('›',haak_sluit).
punct('&gt;',haak_sluit).
punct('&lt;',haak_open).
punct(['&',gt,';'],haak_sluit).
punct(['&',lt,';'],haak_open).

punct(..,hellip).
punct(...,hellip).
punct(....,hellip).
punct(.....,hellip).
punct(......,hellip).
punct(.......,hellip).
punct(........,hellip).
punct(.........,hellip).
punct(..........,hellip).
punct(...........,hellip).
punct(............,hellip).
punct(.............,hellip).
punct('…',hellip).
punct('...!',hellip).
punct('...?',hellip).
punct('...!!',hellip).
punct('...??',hellip).

%% smileys

/*
%% for now
punct(Word,hellip) :-
    smiley(Word).

punct(Words,hellip) :-
    (   punct(WordA,vraag)
    ;   punct(WordA,uitroep)
    ),
    (   atom(WordA)
    ->  WordAs = [WordA]
    ;   WordAs = WordA
    ),
    smiley(WordB),
    (   atom(WordB)
    ->  WordBs = [WordB]
    ;   WordBs = WordB
    ),
    lists:append(WordAs,WordBs,Words).
*/

punct(=,is_gelijk).
punct('=>',is_gelijk).
punct('<=',is_gelijk).
punct('<=>',is_gelijk).
punct('==>',is_gelijk).
punct('<==',is_gelijk).
punct('<==>',is_gelijk).
punct('===>',is_gelijk).
punct('<===',is_gelijk).
punct('<===>',is_gelijk).
punct('->',is_gelijk).
punct('<-',is_gelijk).
punct('<->',is_gelijk).
punct('-->',is_gelijk).
punct('<--',is_gelijk).
punct('<-->',is_gelijk).
punct('--->',is_gelijk).
punct('<---',is_gelijk).
punct('<--->',is_gelijk).
punct('→',is_gelijk).

punct(',',komma).

punct('#',symbol).    % whatever
punct('o',symbol).    % whatever
punct('·',symbol).    % whatever
punct('*',symbol).    % whatever
punct('**',symbol).    % whatever
punct('***',symbol).    % whatever

punct(-,ligg_streep).
punct('',ligg_streep).
punct('&#8211;',ligg_streep).   % dash
punct('–',ligg_streep).   % dash
punct('&#8212;',ligg_streep).   % mdash
punct('—',ligg_streep).   % mdash
punct('&#8211;',ligg_streep).   % ndash
punct('‰',ligg_streep).   % ndash
punct(ndash,ligg_streep).  % nlcow
punct('­',ligg_streep).  % 173
punct('--',ligg_streep).
punct(['-','-'],ligg_streep).  % trouw2004
punct('---',ligg_streep).
punct('----',ligg_streep).
punct('-----',ligg_streep).
punct('- -',ligg_streep).
punct('_',ligg_streep).  % ad2000...
punct('__',ligg_streep).  % ad2001...
punct('~',ligg_streep).   % twitter
punct('–',ligg_streep).
punct('.',punt).
punct('',punt).
punct(;,punt_komma).
punct('/',schuin_streep).
punct('//',schuin_streep).
punct('///',schuin_streep).
punct('////',schuin_streep).
punct('/////',schuin_streep).
punct('//////',schuin_streep).
punct('\\',schuin_streep).
punct('\\\\',schuin_streep).
punct('\\\\\\',schuin_streep).
punct('\\\\\\\\',schuin_streep).
punct('\\\\\\\\\\',schuin_streep).
punct('!',uitroep).
punct('!!',uitroep).
punct('!!!',uitroep).
punct('!!!!',uitroep).
punct('!!!!!',uitroep).
punct('!!!!!!',uitroep).
punct('!!!!!!!',uitroep).
punct('!!!!!!!!',uitroep).
punct('?',vraag).
punct(['.','?'],vraag).
punct(['?','.'],vraag).
punct(['.','?','?'],vraag).
punct(['.','?','?','?'],vraag).
punct(['.','?','?','?','?'],vraag).
punct('??',vraag).
punct('???',vraag).
punct('????',vraag).
punct('?????',vraag).
punct('??????',vraag).
punct('???????',vraag).
punct('????????',vraag).
punct('?.',vraag).
punct('?..',vraag).
punct('?...',vraag).
punct('?!',vraag).
punct('?!?',vraag).
punct('!!?',vraag).
punct('?!?!',vraag).
punct('!?!',vraag).
punct('!?',vraag).
punct('!?!?',vraag).
punct('?!',uitroep).
punct('?!?',uitroep).
punct('!!?',uitroep).
punct('!?!',uitroep).
punct('?!?!',uitroep).
punct('!?',uitroep).
punct('!?!?',uitroep).
punct(['!','?'],vraag).
punct(['!','?','!'],vraag).
punct(['!','?','!','?'],vraag).
punct(['!','?','!','?','!'],vraag).
punct(['!','?','!','?','!','?'],vraag).
punct(['?','!'],vraag).
punct(['?','!','?'],vraag).
punct(['?','!','?','!'],vraag).
punct(['?','!','?','!','?'],vraag).
punct(['?','!','?','!','?','!'],vraag).
punct(x,maal).
punct('×',maal).
punct(+,plus).
punct(&,ampersand).
punct('|',staand_streep).
punct('||',staand_streep).
punct('|||',staand_streep).
punct('||||',staand_streep).
punct('|||||',staand_streep).

%% tokenizer doesn't know about these yet
smiley([';','-',')']).
smiley([';','-',')',')']).
smiley([';',')']).
smiley([';',')',')']).
smiley([';','-P']).
smiley([';','P']).
smiley([';','p']).
smiley([';','O']).
smiley([';','o']).
smiley([';','D']).
smiley([';','$']).
smiley([';','s']).

smiley(';-)').
smiley(';-))').
smiley(';)').
smiley(';))').
smiley(';-P').
smiley(';P').
smiley(';p').
smiley(';O').
smiley(';o').
smiley(';D').
smiley(';$').
smiley(';s').

smiley([':','-',')']).
smiley([':','-',')',')']).
smiley([':',')']).
smiley([':',')',')']).
smiley([':','-','(']).
smiley([':','-','(','(']).
smiley([':','(']).
smiley([':','(',')']).
smiley([':','-P']).
smiley([':','P']).
smiley([':','p']).
smiley([':','O']).
smiley([':','o']).
smiley([':','D']).
smiley([':','$']).
smiley([':','s']).

smiley(':-)').
smiley(':-(').
smiley(':-))').
smiley(':-((').
smiley(':)').
smiley(':(').
smiley(':))').
smiley(':((').
smiley(':-P').
smiley(':P').
smiley(':p').
smiley(':O').
smiley(':o').
smiley(':D').
smiley(':$').
smiley(':s').


smiley(qq).
smiley(qqq).
smiley(qqqq).
smiley(qqqqq).
smiley(xD).
smiley(xd).
smiley('XD').
smiley('♥').
smiley('♡').
smiley('☹').
smiley('☺').
smiley('=D').
smiley('=O').
smiley('=P').
smiley('=S').
smiley('=d').
smiley('=o').
smiley('=p').
smiley('=s').
smiley('-D').
smiley('-O').
smiley('-P').
smiley('-S').
smiley('-d').
smiley('-o').
smiley('-p').
smiley('-s').

smiley('-.-').
smiley('-_-').
smiley('-__-').

smiley(':cheer:').
smiley(':confused:').
smiley(':cool:').
smiley(':evil:').
smiley(':grin:').
smiley(':hug:').
smiley(':lol:').
smiley(':lolabove:').
smiley(':mrgreen:').
smiley(':oops:').
smiley(':roll:').
smiley(':rolleyes:').
smiley(':shock:').
smiley(':sweatdrop:').
smiley(':twisted:').
smiley(':wink:').

%%%WR-P-E-A, NLCOW
smiley([':',cheer,':']).
smiley([':',confused,':']).
smiley([':',cool,':']).
smiley([':',evil,':']).
smiley([':',grin,':']).
smiley([':',hug,':']).
smiley([':',lol,':']).
smiley([':',lolabove,':']).
smiley([':',mrgreen,':']).
smiley([':',oops,':']).
smiley([':',roll,':']).
smiley([':',rolleyes,':']).
smiley([':',shock,':']).
smiley([':',sweatdrop,':']).
smiley([':',twisted,':']).
smiley([':',wink,':']).

smiley(xx).
smiley(xxx).
smiley(xxxx).

smiley(':+').
smiley([':','+']).

%%%%%%%%%%%%%
%%%% TAG %%%%
%%%%%%%%%%%%%

:- discontiguous tag/1.

m(Stem,tag,PN) :-
    tag(PN),
    stem_from_surf(PN,Stem).

m(Stem,tag,Word) :-
    smiley(Word),
    stem_from_surf(Word,Stem).

m(v_root(zeg,zeggen),tag,zeg).
m(v_root(kijk,kijken),tag,kijk).
m(v_root(kom,komen),tag,kom).

m(goedenavond,  tag,goedeavond).
m(goedenavond,  tag,goedenavond).
m(goedendag,    tag,goededag).
m(goedendag,    tag,goedendag).
m(goedenmiddag, tag,goedemiddag).
m(goedenmiddag, tag,goedenmiddag).
m(goedenmorgen, tag,goedemorgen).
m(goedenmorgen, tag,goedenmorgen).
m(goedennacht,  tag,goedenacht).
m(goedennacht,  tag,goedennacht).

m(goedenavond,  tag,goeieavond).
m(goedenavond,  tag,goeienavond).
m(goedendag,    tag,goeiedag).
m(goedendag,    tag,goeiendag).
m(goedenmiddag, tag,goeiemiddag).
m(goedenmiddag, tag,goeienmiddag).
m(goedenmorgen, tag,goeiemorgen).
m(goedenmorgen, tag,goeienmorgen).
m(goedennacht,  tag,goeienacht).
m(goedennacht,  tag,goeiennacht).


tag('√').
tag('♪').  % in subtitles, to indicate that the rest of sentence is sung

tag(aah).
tag(aaah).
tag(aaaah).
tag(aaaaah).
tag(ach).
tag([ach,gut]).
tag([ach,ja]).
tag(achja).
tag([ach,kom]).
tag([ach,meneer]).
tag([ach,mevrouw]).
tag([ach,mijnheer]).
tag([ach,nee]).
tag([ach,wat]).
tag(adieu).
tag(afijn).
tag(ah).
tag(ahh).
tag(aha).
tag(ahah).
tag(ahaha).
tag(ahahha).
tag(ahahah).
tag(ahahaha).
tag(ahahahah).
tag(ahahahaha).
tag(ahahahahah).
tag(ahahahahaha).
tag(ahw).
tag(ai).
tag(aii).
tag(alaaf).
tag(ale).        % Vlaams
tag(alhoewel).
tag([all,right]).
tag([all,the,best]).
tag(allee).			% vlaams
tag(allez). 
tag(allo).
tag(amaai).                     % vlaams
tag(amai).                      % vlaams
tag(amen).
tag(anyway).
tag(apropos).
tag(au).
tag([au,revoir]).
tag(auw).
tag(awel).			% vlaams
tag(bah).
tag(basta).
tag(bingo).
tag(bla).
tag(boem).
tag(boh).                       % vlaams?
tag(bon).                       % vlaams
tag([bon,appétit]).
tag(bonjour).
tag(btw).                       % by the way
tag(bullshit).
tag(bwa).
tag(bwah).
tag(bye).
tag([bye,bye]).
tag([ça,va]).
tag([ca,va]).
tag(['c\'est',ça]).
tag(ciao).
tag([da,da]).
tag(daag).
tag(daaag).
tag(daaaag).
tag(daaaaag).
tag([daar,niet,van]).
tag(dag).
tag([dag,dag]).  % Jaap Moddemijer
tag(damn).
tag(dankje).
tag(dankjewel).
tag(dankuwel).
tag([dank,je,wel]).
tag([dank,u,wel]).
tag([dat,wel]).
tag(dju).
tag(doeg).
tag(doei).
tag(donders).    % Rob Koeling
tag(duh).
tag(duhh).
tag(edoch).
tag(ee).
%tag(eeh).
tag(eey).
tag(eih).
%tag(eh).
%tag(ehh).
%tag(ehm).
tag([en,ach]).
tag(enfin).
tag([en,of]).
tag([en,voilà]).
tag([et,voilà]).
tag([en,voila]).
tag([et,voila]).
tag(voila).
%tag(euh).
tag(ey).
tag(fack).
tag(fff).
tag(floep).
tag(foei).
tag(fock).
tag(fuck).
tag(focking).
tag(fucking).
tag([focking,hell]).
tag([fucking,hell]).
tag([fuck,you]).
tag([fock,you]).
tag([what,the,fock]).
tag([what,the,fuck]).
tag(gadver).
tag(gadverdakkie).
tag(gadverdamme).
tag(gatver).
tag(gatverdamme).
tag(gedver).
tag([geen,sprake,van]).
tag(getver).
tag(ghehe).
tag(godjezus).
tag(godsamme).
tag(godver).
tag(god).
tag([god,allemachtig]).
tag([god,o,god]).
tag([god,oh,god]).
tag([god,',',o,god]).
tag([god,',',oh,god]).
tag([god,o,'God']).
tag([god,oh,'God']).
tag([god,',',o,'God']).
tag([god,',',oh,'God']).
tag(godsallemachtig).
tag([goeden,avond]).
tag([goeden,dag]).
tag([goeden,middag]).
tag([goeden,morgen]).
tag([goeden,nacht]).
tag(goedzo).
tag(goh).
tag(gosh).
tag(gow).
tag(gr).
tag(greets).
tag(greetz).
tag(grtjs).
tag(grts).
tag(groetjess).
tag(groetjesss).
tag(grr).
tag(grrr).
tag(gut).
tag(ha).
tag(hah).
tag(haha).
tag(hahaa).
tag(hahaah).
tag(hahha).
tag(hahhaha).
tag(hahah).
tag(hahaha).
tag(hahahaa).
tag(hahahha).
tag(hahahah).
tag(hahahaha).
tag(hahahahha).
tag(hahahhahha).
tag(hahhahhahha).
tag(hahhahahha).
tag(hahhahaha).
tag(hahahhaha).
tag(hahahhahha).
tag(hahahahah).
tag(hahahahaha).
tag(hahahahahah).
tag(hahahahahaha).
tag(hahahahahahah).
tag([ha,ha]).
tag([ha,ha,ha]).
tag([ha,ha,ha,ha]).
tag([ha,ha,ha,ha,ha]).
tag([ha,ha,ha,ha,ha,ha]).
tag([ha,',',ha]).
tag([ha,',',ha,',',ha]).
tag([ha,',',ha,',',ha,',',ha]).
tag([ha,',',ha,',',ha,',',ha,',',ha]).
tag([ha,',',ha,',',ha,',',ha,',',ha,',',ha]).
tag(hai).
tag(hallo).
tag(hea).
tag(hell).
tag(hello).
tag(help).
tag(hé).
tag([hè,hè]).
tag(hèhè).
tag([hè,',',hè]).
tag(hè).
tag(he).
tag(hee).
tag(heey).
tag(heh).
tag(hej).
tag(hela).
tag(hey).
tag(hi).
tag(hihi).
tag(hihih).
tag(hihihi).
tag(hihihihi).
tag(hihihihihi).
tag(hierzo).
%tag(hm).
%tag(hmm).
%tag(hmmm).
tag(ho).
tag(hoho).
tag(hohoho).
tag(hola).
tag([hoe,de,fuck]).
tag(hoera).
tag(hoewel).
tag(hoezee).
tag(hopla).
tag(hoppekee).
tag(houdoe). % brabants
tag(how).			% vlaams
tag([ho,ho]).
tag([ho,',',ho]).
tag([ho,maar]).
tag(hoi).
tag(hoor).
%tag(huh).
tag(hup).
tag(iel). % twitter voor als iets engs gebeurt?
tag(ja).
tag(jaah).
tag(jahoor).
tag([ja,hoor]).
tag([ja,ja]).
tag(jaja).
tag([ja,',',ja]).
tag([ja,toch]).
tag(jah).
tag(jamaar).
tag(jawel).
tag([o,jawel]).
tag([en,jawel]).
tag([jawel,hoor]).
tag([o,jawel,hoor]).
tag([en,jawel,hoor]).
tag(jazeker).
tag(jee).
tag(jeej).
tag(jeetje).
tag(jemig).
tag([jemig,de,pemig]).
tag(jep).
tag(jeuj).
tag(jezus).
tag(jo).
tag(joch).
tag(joe).
tag(joehoe).
tag(joepie).
tag(joh).
tag([ach,joh]).
tag([hee,joh]).
tag([nee,joh]).
tag([welnee,joh]).
tag([ja,joh]).
tag(kak). % draadstaal!
tag(kanker).
tag(kk).
tag(kkr).
tag(klopt).
tag(koest).
tag(komaan).
tag([kop,op]).
tag(kuss).
tag(kusss).
tag(kut).
tag(kutzooi).
tag(lalala).
tag(laters).
tag([let,wel]).
tag([lieve,help]).
tag([lieve,hemel]).
tag([maar,ach]).
tag(maargoed).
tag([maar,goed]).
tag(maarja).
tag([maar,ja]).
tag([maar,toch]).
tag(maja).
tag(mang).
tag(merci).
tag([mijn,god]).
tag([mijn,'God']).
tag('Miljaarde').
tag(mispoes).
tag([mis,poes]).
tag(mja).
%tag(mm).
%tag(mmm).
%tag(mmmm).
tag(moh).
tag(mvg).
tag(mwa).
tag(mwah).
tag(nah).
tag(naja).
tag(nee).
tag(neej).
tag(neeje).
tag(neen).
tag(neenee).
tag([nee,hoor]).
tag([nee,maar]).
tag([nee,nee]).
tag([nee,',',nee]).
tag([nee,toch]).
tag(nietwaar).
tag([niet,waar]).
tag([niets,van]).
tag([niks,van]).
tag(nja).			% vlaams
tag(nope).
tag([nou,en,of]).
tag([nou,en]).
tag([nou,èn]).
tag([nou,én]).
tag([nou,ja]).
tag(nouja).
tag([nou,nee]).
tag([nu,goed]).
tag([nu,ja]).
tag(o).
tag([o,boy]).
tag([o,god]).
tag([o,'God']).
tag([o,hemel]).
tag([o,ja]).
tag([o,jee]).
tag([o,jé]).
tag([o,my,god]).
tag([o,',',my,god]).
tag([o,mijn,god]).
tag([o,',',mijn,god]).
tag([o,mijn,'God']).
tag([o,',',mijn,'God']).
tag([o,nee]).
tag([o,wee]).
tag(och).
tag(ocharme).
tag([och,arme]).
tag(oef).
tag(oeh).
tag(oei).
tag(oelala).
tag(oeps).
tag(ofschoon).
tag(oh).
tag(ohh).
tag([oh,boy]).
tag([oh,god]).
tag([oh,'God']).
tag([oh,god,',',oh,god]).
tag([oh,'God',oh,'God']).
tag([oh,god,oh,god]).
tag([oh,'God',',',oh,'God']).
tag([oh,hemel]).
tag([oh,ja]).
tag(ohja).
tag([oh,jee]).
tag([oh,jé]).
tag([oh,my,god]).
tag([oh,',',my,god]).
tag([oh,mijn,god]).
tag([oh,',',mijn,god]).
tag([oh,mijn,'God']).
tag([oh,',',mijn,'God']).
tag([oh,nee]).
tag([oh,wee]).
tag(oi).
tag(oja).
tag(owja).
tag(ok).
tag('o.k.').
tag(okaay).
tag(okay).
tag(oke).
tag(okeoke).
tag(oké).
tag(okee).
tag(okeee).
tag(oo).
tag(ooh).
tag(oooh).
tag(ooooh).
tag(oops).
tag(ooow).
tag(oow).
tag(ow).
tag(owkee).
tag(parblue).  % buurman van Ollie
tag(pardon).
tag(pats).
tag(pf).
tag(pff).
tag(pfff).
tag(pffff).
tag(pfffff).
tag(ppf).
tag(ppff).
tag(ppfff).
tag(pppf).
tag(pppff).
tag(pppfff).
tag(please).
tag(pls).
tag(poe).
tag(poeh).
tag(phoe).
tag(potverdomme).
tag(potverdorie).
tag(primo).
tag(proficiat).
tag([punt,uit]).
tag(rara).
tag(right).
tag(rip).
tag(salukes).
tag(salut).
tag(saluut).
tag(saluutjes).
tag(secundo).
tag(seh).
tag(sh).
tag(shit).
tag(sjonge).
tag(slaaplekker).
tag(snapje).
tag(snif).
tag(snotter).
tag([so,what]).
tag(soit).
tag(sorry).
tag(sst).
tag(ssst).
tag(sssst).
tag(tering).
tag(tiens).
tag(tsja).
tag(tja).
tag(tjah).
tag(tjee).
tag(tjeetje).
tag(jonge).
tag(tjonge).
tag(tjongejonge).
tag([jonge,jonge]).
tag([jonge,jonge,jonge]).
tag([tjonge,jonge]).
tag([tjonge,jonge,jonge]).
tag([tjonge,tjonge]).
tag([tjonge,tjonge,tjonge]).
tag(tsjonge).
tag([tsjonge,jonge]).
tag([tsjonge,jonge,jonge]).
tag(tsjonge).
tag([tsjonge,tsjonge]).
tag([tsjonge,tsjonge,tsjonge]).
tag(sjonge).
tag([sjonge,jonge]).
tag([sjonge,jonge,jonge]).
tag([sjonge,sjonge]).
tag([sjonge,sjonge,sjonge]).
tag(thanks).
tag(thanx).
tag(thnx).
tag(thx).
tag(toe).
tag([toe,maar]).
tag([toe,nou]).
tag([tot,gauw]).
tag([tot,laters]).
tag([tot,snel]).
tag([tot,ziens]).
tag(true).
tag(truste).
tag(trusten).
%tag(uhum).
tag(verdomme).
tag(verdorie).
tag([verre,van]). % VL
tag([verre,van,dat]).
tag(verrek).
tag(voilà).
tag([vooruit,dan,maar]).
tag(wablief).
tag(wablieft).
tag(wah).
tag([wat,drommel]).
tag([wat,jij]).
tag([wat,nou]).
tag(wauw).
tag(waw).
tag([wee,je,gebeente]).
tag(weetje).
tag(weh).
tag(welaan).
tag(whaha).
tag(whahah).
tag(whahaha).
tag(whahahah).
tag(whaw).

with_dt([welaan,dan],
	tag,
	dt(du,[dp=l(welaan,tag,advp,0,1),
	       dp=l(dan,tmp_adverb,advp,1,2)])).

tag(wee).
tag(weh).
tag([wel,godverdomme]).
tag(welnee).
tag(welnu).
tag(welteruste).
tag(welterusten).
tag(weltruste).
tag(weltrusten).
tag([wel,te,ruste]).
tag([wel,te,rusten]).
tag(woh).
tag(wauw).
tag(waauw).
tag(waaauw).
tag(waaaauw).
tag(waaaaauw).
tag(waaaaaauw).
tag(waaaaaaauw).
tag(waaaaaaaauw).
tag(wooooooooow).
tag(woooooooow).
tag(wooooooow).
tag(woooooow).
tag(wooooow).
tag(woooow).
tag(wooow).
tag(woow).
tag(wow).
tag(yea).
tag(yeah).
tag(yeahh).
tag(yeahhh).
tag(yep).
tag(yes).
tag(yess).
tag(yo).
tag(zeh).
tag(zenne).                     % vlaams
tag(ziezo).
tag(zuh).			% vlaams
tag(zulle).
tag(zun).			% vlaams
tag(zunne).			% vlaams

tag(nvdr).
tag('nvdr.').

m(ps,tag,ps).
m(ps,tag,'p.s.').
m(ps,tag,'p.s').
m(ps,tag,'PS').
m(ps,tag,'P.S.').


tag(red).
tag('red.').

%%%twitter
%%%zie ook lex_more.pl

tag(bb).     % bye bye
tag(cont).   % wordt vervolgd
tag(dagga).  % doei
tag(lol).    % laughing out loud
tag('LOL').  % laughing out loud
tag(mpp).    % mapangpang (volgens Harm het geslachtsdeel van je moeder)
tag(omg).    % oh my god
tag(omfg).   % oh my fucking god
tag(omygod).
tag(qq).     % ik stop met twitteren
tag(smh).    % shaking my head
tag(wtf).    % what the fuck
tag(xxx).    % kusjes
tag(xxxx).   % kusjes

tag(allemachtig).
tag(asjeblief).
tag(alsjeblief).
tag(alsjeblieft).
tag(alstublieft).
tag(echt).
tag(godzijdank).
tag(nou).
tag(nu).
tag(vooruit).
tag(wel).


tag(althans).   % according to CGN annotations
tag(bovendien).
tag(eindelijk).
tag(gelukkig).
tag(goed).
tag(graag).
tag(heus).
tag(hier).
tag(immers).
tag(inderdaad).
tag(jammer).
tag(kortom).
tag(natuurlijk).
tag(nice).
tag(nogmaals).
tag(overigens).
tag(tenminste).
tag([ten,minste]).  % officieel is schrijfwijze afhankelijk van spelling, dus ander lemma
tag(toch).
tag(trouwens).
tag(uiteraard).
tag(warempel).
tag(weliswaar).
tag([wel,te,verstaan]).
tag(welteverstaan).
tag(zeker).
tag([zeer,zeker]).
tag([zeg,maar]).
tag(zelfs).
tag(zo).

tag(godbetert).
tag(gvd).
tag([god,betert]).
tag([god,betere,het]).
tag([god,betere,'\'t']).
tag(['God',betert]).
tag(['God',betere,het]).
tag(['God',betere,'\'t']).
tag(godverdomme).
tag(godverdorie).
tag(godverdegodver).
tag(goddomme).

tag([echt,waar]).
tag([eerlijk,waar]).
tag([heus,waar]).
tag([niet,waar]).
tag([wel,waar]).

tag(babe).
tag(baby).
tag(bitch).
tag(bloos).
tag([bloos,bloos]).
tag([*,bloos,*]).
tag([*,bloos,bloos,*]).
tag([bloos,',',bloos]).
tag(bby).
tag(bravo).
tag(bro).
tag(captain).
tag(deugniet).
tag(domkop).
tag(domoor).
tag(dude).
tag(eikel).
tag(faka).
tag(gossie).
tag(honneponnetje).
tag(honey).
tag(hoppa).
tag(hufter).
tag(jong).
tag(jongeman).
tag(jonguh).
tag(jonguhs).
tag(jow).
tag(juf).
tag(juffrouw).
tag(kerel).
tag(kindje).
tag(klootzak).
tag(lezer).
tag(liefje).
tag(lieffie).
tag(liefste).
tag(lieveling).
tag(lieverd).
tag(lul).
tag(ma).
tag(madam).
tag(makker).
tag(mam).
tag(mama).
tag(mamma).
tag(man).
tag(mattie).
tag(meester).
tag(meid).
tag(mens).
tag(mensen).
tag(moeder).
tag(monseigneur).
tag(oma).
tag(opa).
tag(pa).
tag(pap).
tag(papa).
tag(pappa).
tag(pastoor).
tag(schaam).
tag([schaam,schaam]).
tag([*,schaam,*]).
tag([*,schaam,schaam,*]).
tag([schaam,',',schaam]).
tag(schat).
tag(schatje).
tag(sir).
tag(stouterd).
tag(vriend).
tag(wifey).
tag(zak).

tag(stel).

%%% Riddle, hesitations

tag(ah).
tag(ehm).
tag(hm).
tag(hmm).
tag(hmmm).
tag(huh).
tag(mmmm).
tag(uh).

%%%%%%%%%%%%%%%%%%%%%%%%%

with_dt(['Dames',en,'Heren','Goedenavond'],
	max,
	dt(du,[tag=dt(conj,[cnj=l(dame,noun(de,count,pl),np,0,1),
			    crd=l(en,conj(en),vg,1,2),
			    cnj=l(heer,noun(de,count,pl),np,2,3)]),
	       nucl=l(goedenavond,tag,advp,3,4)
	      ])).

with_dt([ja,of,nee],
        tag,
        dt(conj,[cnj=l(ja,tag,advp,0,1),
                 crd=l(of,conj(of),vg,1,2),
                 cnj=l(nee,tag,advp,2,3)
                ])).

with_dt([ja,ofte,nee],
        tag,
        dt(conj,[cnj=l(ja,tag,advp,0,1),
                 crd=l(of,conj(of),vg,1,2),
                 cnj=l(nee,tag,advp,2,3)
                ])).

with_dt([weet,je],
	tag,
	dt(sv1,[hd=l(v_root(weet,weten),verb(hebben,sg,tr_sbar),0,1),
		su=l(je,pronoun(nwh,je,sg,de,both,def,wkpro),np,1,2)])).

with_dt([niet,te,geloven],
	adjective(pred(nonadv)),
	dt(ti,[cmp=l(te,complementizer(te),cp,1,2),
               body=dt(inf,[hd=l(v_root(geloof,geloven),verb(hebben,inf,transitive),2,3),
                            mod=l(niet,adverb,advp,0,1)
                           ])])).

with_dt([niet,te,geloven],
	adjective(pred(nonadv),subject_sbar),
	dt(ti,[cmp=l(te,complementizer(te),cp,1,2),
               body=dt(inf,[hd=l(v_root(geloof,geloven),verb(hebben,inf,transitive),2,3),
                            mod=l(niet,adverb,advp,0,1)
                           ])])).

with_dt([in,vuur,en,vlam],
        adjective(pred(padv)),
        dt(pp,[hd=l(in,preposition(in,[]),0,1),
               obj1=dt(conj,[cnj=l(vuur,noun(het,count,sg),np,1,2),
                             crd=l(en,conj(en),vg,2,3),
                             cnj=l(vlam,noun(de,count,sg),np,3,4)
                             ])])).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% ADVERBS %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

het(het).
het('\'t').

een2(Een) :-
    een1(Een).
een2('\'n').

een1(een).
een1(één).

een(een).
een('\'n').

with_dt([dat,Het,Een,aard,heeft],
	sentence_adverb,
	dt(cp,
	   [cmp=l(dat,complementizer(dat),cp,0,1),
	    body=dt(ssub,
		    [hd=l(v_root(heb,hebben),verb(hebben,sg_heeft,transitive_ndev),4,5),
		     su=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),np,1,2),
		     obj1=dt(np,
			     [det=l(een,determiner(een),detp,2,3),
			      hd=l(aard,noun(de,mass,sg),3,4)
			     ])
		    ])])) :- het(Het), een(Een).

with_dt([dat,Het,Een,aard,had],
	sentence_adverb,
	dt(cp,
	   [cmp=l(dat,complementizer(dat),cp,0,1),
	    body=dt(ssub,
		    [hd=l(v_root(heb,hebben),verb(hebben,past(sg),transitive_ndev),4,5),
		     su=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),np,1,2),
		     obj1=dt(np,
			     [det=l(een,determiner(een),detp,2,3),
			      hd=l(aard,noun(de,mass,sg),3,4)
			     ])
		    ])])) :- het(Het), een(Een).

with_dt([dat,Het,giet],
	sentence_adverb,
	dt(cp,
	   [cmp=l(dat,complementizer(dat),cp,0,1),
	    body=dt(ssub,
		    [su=l(het,het_noun,np,1,2),
		     hd=l(v_root(giet,gieten),verb(hebben,sg,het_subj),2,3)])])) :-
    het(Het).

with_dt([dat,Het,kraakt],
	sentence_adverb,
	dt(cp,
	   [cmp=l(dat,complementizer(dat),cp,0,1),
	    body=dt(ssub,
		    [su=l(het,het_noun,np,1,2),
		     hd=l(v_root(kraak,kraken),verb(hebben,sg3,intransitive),2,3)])])) :-
    het(Het).

with_dt([dat,Het,Een,lieve,lust,is],
	sentence_adverb,
	dt(cp,
	   [cmp=l(dat,complementizer(dat),cp,0,1),
	    body=dt(ssub,
		    [su=l(het,het_noun,np,1,2),
		     hd=l(v_root(ben,zijn),verb(zijn,sg_heeft,copula),5,6),
		     predc=dt(np,
			      [det=l(een,determiner(een),detp,2,3),
			       mod=l(lief,adjective(e),ap,3,4),
			       hd=l(lust,noun(de,count,sg),4,5)])
		    ])])) :-
    het(Het),
    een(Een).

with_dt([dat,Het,Een,lieve,lust,was],
	sentence_adverb,
	dt(cp,
	   [cmp=l(dat,complementizer(dat),cp,0,1),
	    body=dt(ssub,
		    [su=l(het,het_noun,np,1,2),
		     hd=l(v_root(ben,zijn),verb(zijn,past(sg),copula),5,6),
		     predc=dt(np,
			      [det=l(een,determiner(een),detp,2,3),
			       mod=l(lief,adjective(e),ap,3,4),
			       hd=l(lust,noun(de,count,sg),4,5)])
		    ])])) :- het(Het),een(Een).

with_dt([beloofd,is,beloofd],
	sentence_adverb,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg3,copula),1,2),
		  su=l(beloofd,adjective(ge_no_e(adv)),ap,0,1),
		  predc=l(beloofd,adjective(ge_no_e(adv)),ap,2,3)])).

with_dt([eerlijk,is,eerlijk],
	sentence_adverb,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg3,copula),1,2),
		  su=l(eerlijk,adjective(no_e(adv)),ap,0,1),
		  predc=l(eerlijk,adjective(no_e(adv)),ap,2,3)])).

with_dt([op,is,op],
	max,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg3,copula),1,2),
		  su=l(op,adjective(pred(nonadv)),ap,0,1),
		  predc=l(op,adjective(pred(nonadv)),ap,2,3)])).

with_dt([op,'=',op],
	max,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg3,copula),1,2),
		  su=l(op,adjective(pred(nonadv)),ap,0,1),
		  predc=l(op,adjective(pred(nonadv)),ap,2,3)])).

with_dt([naar,verwachting],
	adverb,
	dt(pp,[hd=l(naar,preposition(naar,[]),0,1),
	       obj1=l(verwachting,noun(de,count,sg),np,1,2)])
       ).

with_dt([niet,te,vergeten],
	adverb,
	dt(ti,[cmp=l(te,complementizer(te),cp,1,2),
	       body=dt(inf,[mod=l(niet,adverb,advp,0,1),
			    hd=l(v_root(vergeet,vergeten),verb(hebben/zijn,inf,transitive),2,3)])
	      ])).

with_dt([Zo,Mogelijk],
	adverb,
	dt(advp,[hd=l(Zo,adverb,0,1),
		 obcomp=l(Mogelijk,adjective(no_e(adv)),ap,1,2)])
       ) :-
    zo_mogelijk_adverb(Zo,Mogelijk).

with_dt([zo,Nodig],
	adverb,
	dt(cp,[cmp=l(zo,complementizer(a),cp,0,1),
	       body=l(Nodig,adjective(no_e(adv)),ap,1,2)])) :-
    zo_nodig(Nodig).

zo_nodig(nodig).
zo_nodig(mogelijk).

% zo_mogelijk_adverb(zo,mogelijk).
zo_mogelijk_adverb(zogauw,mogelijk).
zo_mogelijk_adverb(zolang,mogelijk).
zo_mogelijk_adverb(zomin,mogelijk).
zo_mogelijk_adverb(zoveel,mogelijk).

with_dt([niet,in,de,laatste,plaats],
	modal_adverb,
	dt(pp,[hd=l(in,preposition(in,[]),1,2),
	       obj1=dt(np,[det=l(de,determiner(de),detp,2,3),
			   mod=l(laatst,adjective(ste),ap,3,4),
			   hd=l(plaats,noun(de,count,sg),4,5)
			  ]),
	       mod=l(niet,adverb,advp,0,1)])
       ).

with_dt([niet,in,de,laatste,plaats],
	adverb,
	dt(pp,[hd=l(in,preposition(in,[]),1,2),
	       obj1=dt(np,[det=l(de,determiner(de),detp,2,3),
			   mod=l(laatst,adjective(ste),ap,3,4),
			   hd=l(plaats,noun(de,count,sg),4,5)
			  ]),
	       mod=l(niet,adverb,advp,0,1)])
       ).

with_dt([al,te,zeer],
	adverb,
	dt(advp,[hd=l(zeer,adverb,advp,2,3),
		 mod=dt(advp,[mod=l(al,adverb,advp,0,1),
			      hd=l(te,intensifier,advp,1,2)])
		])).

with_dt([te,zeer],
	adverb,
	dt(advp,[hd=l(zeer,adverb,advp,1,2),
		 mod=l(te,intensifier,advp,0,1)
		])).

%%%%%%%%%%%%%%%%%%%%%
%%%% DETERMINERS %%%%
%%%%%%%%%%%%%%%%%%%%%

%% determiner:
%% ik heb DET N
%% never preceded by determiner
%% sometimes stand-alone (like a pronoun, but can be modified: hoe weinig)

%% ik heb er NUM

%% number:
%% with quantitative 'er':                         ik heb er drie
%% cooccurs with determiner in NP: DET NUM N       de drie meisjes
%% stand-alone with determiner:                    ik zag de drie
%% stand-alone in partitives: ik heb NUM van NP    ik zag drie van de meisjes
%% can occur both before or after adjective in NP: de drie aardige meisjes/
%%                                                 de aardige drie meisjes
%% can be stacked???
%% can be modified by certain adverbs:             bijna honderd meisjes
%% can form 'predm' phrases with 'alle':           de meisjes gaan alle zes
%%  
%% adj_number:
%% like adjecties, 
%% only allow (DET) NUM N, but we don't want 'mod' relation,
%% in addition there can be more specific agreement restrictions

m(beide,    adj_number(pl_num), beide).    % de beide
m(beide,    adj_number(both),   beider).   % hun beider levenslot
m(enkel,    adj_number(enkel),  enkel).   % een enkel, geen enkel
m(enkel,    adj_number(enkele), enkele).
m(ettelijk, adj_number(pl_num), ettelijke).
%% according to ANS, this should also include "verscheidene, verschillende"

m('meer als',         intensifier, [meer,als]).
m('meer dan',         intensifier, [meer,dan]).
m('minder als',       intensifier, [minder,als]).
m('minder dan',       intensifier, [minder,dan]).
m('niet meer als',    intensifier, [niet,meer,als]).
m('niet meer dan',    intensifier, [niet,meer,dan]).
m('niet minder als',  intensifier, [niet,minder,als]).
m('niet minder dan',  intensifier, [niet,minder,dan]).

m('meer als',         pre_num_adv(both), [meer,als]).
m('meer dan',         pre_num_adv(both), [meer,dan]).
m('minder als',       pre_num_adv(both), [minder,als]).
m('minder dan',       pre_num_adv(both), [minder,dan]).
m('niet meer als',    pre_num_adv(both), [niet,meer,als]).
m('niet meer dan',    pre_num_adv(both), [niet,meer,dan]).
m('niet minder als',  pre_num_adv(both), [niet,minder,als]).
m('niet minder dan',  pre_num_adv(both), [niet,minder,dan]).

%% pre_det_quant
%% quantifiers that may precede determiner in NP
%% + tal van?
m(al,          pre_det_quant(al),       al).
m(allebei,     pre_det_quant(allebei),  allebei).
m(alletwee,    pre_det_quant(allebei),  alletwee).
m(alledrie,    pre_det_quant(allebei),  alledrie).
m(allevier,    pre_det_quant(allebei),  allevier).
m(allevijf,    pre_det_quant(allebei),  allevijf).

with_dt([niet,al],
	pre_det_quant(al),
	dt(detp,[hd=l(al,pre_det_quant(al),1,2),
		 mod=l(niet,adverb,advp,0,1)])).

with_dt([lang,niet,al],
	pre_det_quant(al),
	dt(detp,[hd=l(al,pre_det_quant(al),2,3),
		 mod=dt(advp,[hd=l(niet,adverb,1,2),
			      mod=l(lang,adjective(no_e(tmpadv)),ap,0,1)])])).

with_dt([Zoveel,Mogelijk],
	determiner(wat,nwh,nmod,pro,yparg),
	dt(detp,[hd=l(Zoveel,number(hoofd(both)),0,1),
		 obcomp=l(Mogelijk,adjective(no_e(adv)),ap,1,2)])
       ) :-
    zo_mogelijk_determiner(Zoveel,Mogelijk).

zo_mogelijk_determiner(zomin,mogelijk).
zo_mogelijk_determiner(zoveel,mogelijk).

with_dt([Zo,Veel,Mogelijk],
	determiner(wat,nwh,nmod,pro,yparg),
	dt(detp,[hd=l(Veel,adjective(no_e(odet_adv)),1,2),
		 mod=dt(advp,[hd=l(Zo,adverb,0,1),
			      obcomp=l(Mogelijk,adjective(no_e(adv)),ap,2,3)])
		])
       ) :-
    zo_veel_mogelijk_determiner(Zo,Veel,Mogelijk).

zo_veel_mogelijk_determiner(zo,min,mogelijk).

%%  Xtal (X any number) in lex_more
with_dt([A,B],
	determiner(Agr,nwh,nmod,pro,yparg),
	dt(np,[det=l(A,determiner(een),detp,0,1),
	       hd=l(B,noun(both,both,sg),1,2)])) :-
    een_aantal_detp(A,B,Agr).

with_dt([A,B],
	determiner(Agr,nwh,nmod,pro,yparg),
	dt(np,[det=l(A,Pos,detp,0,1),
	       hd=l(B,noun(both,both,sg),1,2)])) :-
    een_aantal_detp_pos(A,B,Agr,Pos).

with_dt([A,B,C],
	determiner(Agr,nwh,nmod,pro,yparg),
	dt(np,[det=l(A,determiner(een),detp,0,1),
	       mod=l(B,adjective(e),ap,1,2),
	       hd=l(C,noun(both,both,sg),2,3)])) :-
    een_aantal_detp(A,B,C,Agr).

with_dt([A,B,C],
	determiner(Agr,nwh,nmod,pro,yparg),
	dt(np,[det=l(A,determiner(een),detp,0,1),
	       mod=l(B,adjective(no_e(adv)),ap,1,2),
	       hd=l(C,noun(both,both,sg),2,3)])) :-
    een_groot_aantal_detp(A,B,C,Agr).

een_groot_aantal_detp(Een,beperkt,aantal,pl_num) :-
    een(Een).
een_groot_aantal_detp(Een,enorm,aantal,pl_num) :-
    een(Een).
een_groot_aantal_detp(Een,flink,aantal,pl_num) :-
    een(Een).
een_groot_aantal_detp(Een,groot,aantal,pl_num) :-
    een(Een).
een_groot_aantal_detp(Een,klein,aantal,pl_num) :-
    een(Een).
een_groot_aantal_detp(Een,respectabel,aantal,pl_num) :-
    een(Een).

%% een groot aantal gaat niet --> compositioneel
%% een groot aantal gaan niet --> speciaal met deze versie
%% een groot aantal kinderen gaat niet --> gewoon
%% een groot aantal kinderen gaan niet --> speciaal

%% TODO
%% "paar" is meas_mod_noun(het,count,meas)
%% so we get the plural reading compositional too!


een_aantal_detp(Een,aantal,pl_num) :- een(Een).
een_aantal_detp(Een,boel,wat) :- een(Een).
een_aantal_detp(Een,handvol,pl_num) :- een(Een).
een_aantal_detp(Een,heleboel,wat) :- een(Een).
een_aantal_detp(Een,hoop,wat) :- een(Een).
een_aantal_detp(Een,miljoen,pl_num) :- een2(Een).
een_aantal_detp(Een,paar,pl_num) :- een(Een).
een_aantal_detp(Een,pak,pl_num) :- een(Een).  % VL
een_aantal_detp(Een,reeks,pl_num) :- een(Een).
een_aantal_detp(Een,soort,pron) :- een(Een).
een_aantal_detp(Een,stel,pl_num) :- een(Een).
een_aantal_detp(Een,stelletje,pl_num) :- een(Een).

een_aantal_detp_pos(die,paar,pl_num,determiner(de,nwh,nmod,pro,nparg)).  % wat maakt die paar minuten nou uit.
een_aantal_detp_pos(elke,paar,pl_num,determiner(elke,nwh,mod)).
een_aantal_detp_pos(iedere,paar,pl_num,determiner(elke,nwh,mod)).
een_aantal_detp_pos(dat,soort,pron,determiner(het,nwh,nmod,pro,nparg)).
een_aantal_detp_pos(dit,soort,pron,determiner(het,nwh,nmod,pro,nparg)).
een_aantal_detp_pos(zulk,soort,pron,determiner(zulk)).

een_aantal_detp_pos(dat,type,pron,determiner(het,nwh,nmod,pro,nparg)).   % dit type klysma's hebben ...
een_aantal_detp_pos(dit,type,pron,determiner(het,nwh,nmod,pro,nparg)).

een_aantal_detp(Een,hele,hoop,wat) :- een(Een).

with_dt([Een,paar,Honderd],
	determiner(pl_num,nwh,nmod,pro,yparg),
	dt(np,[det=dt(np,[det=l(Een,Pos,detp,0,1),
			  hd=l(paar,meas_mod_noun(het,count,meas),1,2)]),
	       hd=l(Honderd,number(hoofd(pl_num)),2,3)
	      ])) :-
    een_paar_een(Een,Pos),
    een_paar_honderd(Honderd).

%% TODO almost any determiner with appropriate agr can come here!
een_paar_een(de,determiner(de)).
een_paar_een(deze,determiner(de,nwh,nmod,pro,nparg)).
een_paar_een(die,determiner(de,nwh,nmod,pro,nparg)).
een_paar_een(Een,determiner(een)) :-
    een(Een).

een_paar_honderd(honderd).
een_paar_honderd(duizend).
een_paar_honderd(honderdduizend).
een_paar_honderd(miljoen).
een_paar_honderd(biljoen).
een_paar_honderd(miljard).

m(een,               pre_num_adv(pl_indef),     Een)            :- een(Een).
m('een kleine',      pre_num_adv(pl_indef),     [Een,kleine])   :- een(Een).
m('een slordige',    pre_num_adv(pl_indef),     [Een,slordige]) :- een(Een).
m('iets van',        pre_num_adv(pl),           [iets,van]).
m('nog geen',        pre_num_adv(pl),           [nog,geen]).
m('over de',         pre_num_adv(pl),           [over,de]).
m('rond de',         pre_num_adv(pl_indef),     [rond,de]).
m('om en nabij de',  pre_num_adv(pl_indef),     [om,en,nabij,de]).
m('tegen de',        pre_num_adv(pl_indef),     [tegen,de]).
m('zo\'n',           pre_num_adv(pl_indef),     'zo\'n').
m('zo\'n kleine',    pre_num_adv(pl_indef),     ['zo\'n',kleine]).
m('zo\'n slordige',  pre_num_adv(pl_indef),     ['zo\'n',slordige]).

%% TODO: add meer/minder dan/als here too?

%% 3rd argument mod
%% indicates that adverbs can modify the determiner
%% heel/erg/nogal veel boeken
%%

m('de eerste de beste',    determiner(de,nwh,nmod,pro,yparg),
  [de,eerste,de,beste]).
m('de eerste de beste',    determiner(de,nwh,nmod,pro,yparg),
  [de,eerste,',',de,beste]).
m('het eerste het beste',  determiner(het,nwh,nmod,pro,yparg),
  [het,eerste,het,beste]).
m('het eerste het beste',  determiner(het,nwh,nmod,pro,yparg),
  [het,eerste,',',het,beste]).

m('de ene na de andere',determiner(de,nwh,nmod,pro,yparg), [de,ene,na,de,andere]).
m('de ene na de andere',determiner(de,nwh,nmod,pro,yparg), [de,een,na,de,andere]).

m(al,          determiner(alle,nwh,mod,pro,nparg), alle).
m(al,          determiner(der),                aller).
m(al,          determiner(pron),               aller). % op aller lippen (ouderwets)
m(allerlei,      determiner(wat,nwh,mod,pro,yparg),                allerlei).
m(andermans,     determiner(pron),               andermans).
m(dat,           determiner(het,nwh,nmod,pro,nparg),      dat).
m(datzelfde,     determiner(het,nwh,mod,pro,yparg),  datzelfde).
m(de,            determiner(de),                 de).
%m(de,            determiner(den),                den).
m(de,            determiner(der),                der).
m(de,            determiner(des),                des).
m('des duivels', determiner(pron),               [des,duivels]).
m('d\'rlui',     determiner(pron),               'd\'rlui').
m(deze,          determiner(de,nwh,nmod,pro,nparg),       deze).
m(dezelve,       determiner(de,nwh,nmod,pro,yparg),       dezelve).
%m(deze,          determiner(both,nwh,nmod,pro,yparg),     dees).   % vlaams
m(dezelfde,      determiner(de,nwh,mod,pro,yparg),   dezelfde).
m(deze,          determiner(der),                dezer).
m(deze,          determiner(pron),               dezer).
m(deze,          determiner(pron),               dezes).
m(die,           determiner(de,nwh,nmod,pro,nparg),       die).
m(die,           determiner(der),                dier).
%m(die,           determiner(de,nwh,nmod,pro,nparg),       dieje).  % vlaams
%m(die,           determiner(de,nwh,nmod,pro,nparg),       diejen). % vlaams
%m(die,           determiner(de,nwh,nmod,pro,nparg),       diene).  % vlaams
%m(die,           determiner(de,nwh,nmod,pro,nparg),       dien).   % vlaams
m(diens,         determiner(pron),               diens).
m(diezelfde,     determiner(de,nwh,mod,pro,yparg),   diezelfde).
m(dit,           determiner(het,nwh,nmod,pro,nparg),      dit).
m(ditzelfde,     determiner(het,nwh,mod,pro,yparg),  ditzelfde).
%m(een,           determiner(ener),               eener).
m(een,           determiner(ener),               ener).
m(een,           determiner(een),                Een) :- een(Een).
%m(een,           determiner(een),                nen).    % vlaams
%m(een,           determiner(een),                enen).   % vlaams
%m(een,           determiner(een),                ne).     % vlaams
%m(een,           determiner(een),                ene).
%m(één,           determiner(een),                enen).
m('een weinig',   determiner(wat),                 [een,weinig]).
m(eenieder,      determiner(pron),eenieders).
m(eenzelfde,     determiner(een,nwh,mod,pro,yparg),  eenzelfde).
m(elk,           determiner(het,nwh,mod,pro,yparg),      elk).
m(elkaar,        determiner(pron),               elkaars).
m(elkander,      determiner(pron),               elkanders).
m(elk,           determiner(elke,nwh,mod),           elke).
m(enig,          determiner(welk),               enig).
m(enig,          determiner(welke),              enige).
%% m(enig,          determiner(welke),              enigste).
m(evenveel,      determiner(wat,nwh,mod,pro,yparg),  evenveel).
m(evenzoveel,    determiner(wat,nwh,mod,pro,yparg),  evenzoveel).
m(geen,          determiner(geen,nwh,mod,pro,yparg,nwkpro,geen), geen).
m(geeneen,       determiner(geen,nwh,mod,pro,yparg,nwkpro,geen), [geen,Een]) :-
    een1(Een).
m(geeneen,       determiner(geen,nwh,mod,pro,yparg,nwkpro,geen), geeneen).
m(geeneen,       determiner(geen,nwh,mod,pro,yparg,nwkpro,geen), geenéén).
m(gene,          determiner(de,nwh,nmod,pro,yparg),       gene).
				% deze of gene;
				% aan gene zijde
m(generlei,      determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),   generlei).
m(genoeg,        determiner(wat,nwh,mod,pro,yparg),  genoeg).
m(haar,          determiner(pron),               haar).
m(haar,          determiner(der),harer).
m(haar,          determiner(der),haars).
%m(haar,          determiner(pron),               heur).
m(heleboel,      determiner(wat,nwh,mod,pro,nparg),heleboel).
m(het,           determiner(het,nwh,nmod,pro,nparg,wkpro),het).
m(hetzelfde,     determiner(het,nwh,mod,pro,yparg),hetzelfde).
m(hun,           determiner(pron),hun).
m(hun,           determiner(der),hunner).
m(hun,           determiner(der),huns).
%m(hun,           determiner(pron),ulder).
%m(hun,           determiner(pron),zullie).
m(ieder,         determiner(het,nwh,mod),ieder).
m(ieder,         determiner(pron),ieders).
m(ieder,         determiner(elke,nwh,mod),iedere).
m(iemand,        determiner(pron),iemands).
m(je,            determiner(pron),je).
m('de reinste',  determiner(both),[de,reinste]).
m('je reinste',  determiner(both),[je,reinste]).
m(jou,           determiner(pron),jouw).
m(jullie,        determiner(pron),jullie).
m(meerdere,      determiner(pl_num,nwh,nmod,pro,yparg),meerdere).
m(menig,         determiner(een),menig).
m(menig,         determiner(sg_num),menige).
m(mijn,          determiner(pron),mijn).
%m(mijn,          determiner(pron),mijne).  % mijne heren, ,mijne..
m(mijn,          determiner(der),mijner).
m(mijn,          determiner(der),mijns).
%m(mijn,          determiner(pron),mijner).
m(niemand,       determiner(pron),niemands).
m(nul,           determiner(geen,nwh,mod,pro,yparg,nwkpro,geen), nul). % dat zou nul rendement betekenen
                                                                       % van nul of generlei waarde (?)
m(ons,           determiner(het),ons).
m(ons,           determiner(onze),onze).
m(ons,           determiner(der),onzer).
m(ons,           determiner(der),onzes).
%m(ons,           determiner(pron),onzer).
m('ons al',   determiner(pron),[ons,aller]).
m(onvoldoende,   determiner(wat,nwh,mod,pro,yparg),onvoldoende).
m(sommig,        determiner(pl_num,nwh,nmod,pro,yparg),sommige).
%m(tal,           determiner(wat),tal).  % ja dat zeggen mensen echt
%m('tal van',     determiner(wat),[tal,van]).
m(tal,           determiner(wat,nwh,mod,pro,yparg),tal).
%% tal medewerkers raakten gewond
%% tal van medewerkers raakten gewond
%% tal van zijn medewerkers raakten gewond
m(u,             determiner(pron),uw).
m('u al',    determiner(pron),[uw,aller]).
%m(u,            determiner(pron),uwe).    % vlaams
%m(u,            determiner(pron),uwen).   % vlaams
%m(u,            determiner(pron),uwer).
m(u,             determiner(der),uwer).
m(u,             determiner(der),uws).  % erg ouderwets
m(veel,          determiner(pron),veler).
m(voldoende,     determiner(wat,nwh,mod,pro,yparg),voldoende).
m(volop,         determiner(wat),volop).
m(wat,           determiner(wat,nwh,mod,pro,nparg,ntopicpro),wat). %  "wkpro"
                              % *wat vond ik het (without question reading)
                              % vond je het wat?
                              % was/wordt het wat?
m('wel en geen', determiner(geen),[wel,en,geen]).
m('wel of geen', determiner(geen),[wel,of,geen]).
m(zijn,          determiner(pron),zijn).
m(zijn,          determiner(pron),'z\'n').
m('zijn/haar',   determiner(pron),'zijn/haar').
m(zijn,          determiner(der),zijner).
%m(zijn,          determiner(pron),zijner).
m('zo\'n',       determiner(een),[zo,een]).
m('zo\'n',       determiner(een),['zo\'n',een]). % VL
m('zo\'n',       determiner(een),'zo\'n').
m('zo\'n',       determiner(pl),'zo\'n'). % VL
m('een zo\'n',   determiner(een),[een,'zo\'n']).
m('een zo\'n',   determiner(een),[één,'zo\'n']).
m(zulk,          determiner(zulk),zulk).
m('zulk een',          determiner(een),[zulk,Een]) :- een(Een).
m(zulk,          determiner(zulke,nwh,nmod,pro,yparg),zulke).

m(paar,          determiner(pl_num), paar ).   % paar jaar geleden heeft hij een analyse gemaakt

%% wat een mensen wonen hier. Is this wh_determiner? Topicalization/relativ.
%% appears to be obligatory, so perhaps syntactically it is...
m(Stem,      determiner(pron,wh),[wat,Een] ) :- een(Een), stem_from_surf([wat,Een],Stem).    
m(Stem,      determiner(pron,wh),[welk,Een]) :- een(Een), stem_from_surf([welk,Een],Stem).    
m('wat voor',determiner(pron,wh),[wat,voor]).
m('wat voor',determiner(pron,wh),watvoor).
m(Stem,      determiner(pron,wh),[wat,voor,Een]) :- een(Een), stem_from_surf([wat,voor,Een],Stem).
m(Stem,      determiner(pron,wh),[watvoor,Een]) :- een(Een), stem_from_surf([wat,voor,Een],Stem).

%% iedere twee weken
%% elke twee jaar
m(elk,          tmp_determiner,  elke).
m(ieder,        tmp_determiner,  iedere).

with_dt([niet,een],
	determiner(een),
	dt(detp,[mod=l(niet,adverb,advp,0,1),
		 hd=l(één,number(hoofd(sg_num)),1,2)])).

with_dt([niet,een],
	pronoun(nwh,thi,sg,both,both,indef,strpro),
	dt(detp,[mod=l(niet,adverb,advp,0,1),
		 hd=l(één,number(hoofd(sg_num)),1,2)])).

with_dt([niet,eentje],
	pronoun(nwh,thi,sg,both,both,indef,strpro),
	dt(detp,[mod=l(niet,adverb,advp,0,1),
		 hd=l(één,pronoun(nwh,thi,sg,de,both,indef),1,2)])).

m('\'s dichter',determiner(pron), ['\'s',dichters]).
m('\'s koning', determiner(pron), ['\'s',konings]).
m('\'s koning', determiner(pron), ['\'s',keizers]).
m('\'s land',   determiner(pron), ['\'s',lands]).
m('\'s lezer',  determiner(pron), ['\'s',lezers]).
m('\'s liefde', determiner(pron), ['\'s',liefdes]).
m('\'s man',    determiner(pron), ['\'s',mans]).
m('\'s mens',   determiner(pron), ['\'s',mensen]).
m('\'s rijk',   determiner(pron), ['\'s',rijks]).
m('\'s vijand', determiner(pron), ['\'s',vijands]).
m('\'s wereld', determiner(pron), ['\'s',werelds]).

m(god,          determiner(pron), gods).
m(god,          determiner(pron), 'Gods').
m(grootmoeder,  determiner(pron), grootmoeders).
m(grootvader,   determiner(pron), grootvaders).
m(stief_moeder, determiner(pron), stiefmoeders).
m(stief_vader,  determiner(pron), stiefvaders).
m(land,         determiner(pron), slands).
m('\'s land',   determiner(pron), ['\'',slands]).
m(mam,          determiner(pron), mams).
m(meester,      determiner(pron), meesters).
m(moeder,       determiner(pron), moeders).
m(noord,        determiner(pron), noords).
m(oost,         determiner(pron), oosts).
m(overvloed,    determiner(pron), overvloeds).
m(tante,        determiner(pron), 'tante\'s').
m(vader,        determiner(pron), vaders).
m(west,         determiner(pron), wests).
m(wit,          determiner(pron), wits).
m(zuid,         determiner(pron), zuids).
m(zwart,        determiner(pron), zwarts).

m(oma,          determiner(pron), 'oma\'s').
m(opa,          determiner(pron), 'opa\'s').
m(ma,           determiner(pron), 'ma\'s').
m(pa,           determiner(pron), 'pa\'s').
m(mama,         determiner(pron), 'mama\'s').
m(papa,         determiner(pron), 'papa\'s').
m(mama,         determiner(pron), 'mamma\'s').
m(papa,         determiner(pron), 'pappa\'s').

m('Nederland',  name_determiner(pron,'LOC'),'Neerlands').
m('\'s Nederland',  name_determiner(pron,'LOC'),['\'s','Neerlands']).
m('Prince',     name_determiner(pron,'PER'),'Prince\'').
m('van Gogh',   name_determiner(pron,'PER'),[van,'Goghs']).
m('Bin Laden',  name_determiner(pron,'PER'),['Bin','Ladens']).

m('Rood',      name_determiner(pron,'PER'),['Roods']).

%% also with other possessives of course...
with_dt([Mijn,Vaders],
	determiner(pron),
	dt(detp,[hd=l(Vader,determiner(pron),1,2),
		 det=l(MijnL,determiner(pron),detp,0,1)
		])) :-
    mijn_vaders_mijn(Mijn,MijnL),
    mijn_vaders_vaders(Vader,Vaders).

mijn_vaders_mijn(je,je).
mijn_vaders_mijn(mijn,mijn).
mijn_vaders_mijn('m\'n',mijn).
mijn_vaders_mijn(zijn,zijn).
mijn_vaders_mijn('z\'n',zijn).
mijn_vaders_mijn(haar,haar).
mijn_vaders_mijn(haars,haar).  %ouderwets
mijn_vaders_mijn('d\'r',haar).
mijn_vaders_mijn(hun,hun).
mijn_vaders_mijn(jouw,jou).
mijn_vaders_mijn(jullie,jullie).
mijn_vaders_mijn(uw,u).
mijn_vaders_mijn(des,de).

mijn_vaders_vaders(broer,broers).
mijn_vaders_vaders(broeder,broeders).
mijn_vaders_vaders(moeder,moeders).
mijn_vaders_vaders(grootmoeder,grootmoeders).
mijn_vaders_vaders(tante,tantes).
mijn_vaders_vaders(vader,vaders).
mijn_vaders_vaders(grootvader,grootvaders).
mijn_vaders_vaders(stief_vader,stiefvaders).
mijn_vaders_vaders(stief_moeder,stiefmoeders).
mijn_vaders_vaders(vriend,vriends).
mijn_vaders_vaders(vriendin,vriendins).
mijn_vaders_vaders(zuster,zusters).
mijn_vaders_vaders(opa,'opa\'s').
mijn_vaders_vaders(oma,'oma\'s').
mijn_vaders_vaders(man,mans).
mijn_vaders_vaders(minnaar,minnaars).

m(welk,          determiner(welke,rwh,nmod,pro,yparg),   welke).
%% m(welk,          determiner(welke,rwh,nmod,pro,yparg),   [de,welke]). % VL; now parse_only
m(welk,          determiner(welk, rwh,nmod,pro,yparg),   welk).

with_dt([ik,weet,niet,welk],
	determiner(welk,nwh,nmod,pro,yparg),
	dt(smain,[ hd=l(v_root(weet,weten),verb(hebben,sg,tr_sbar),1,2),
		   su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		   mod=l(niet,adverb,advp,2,3),
		   vc=l(welk,determiner(welk,rwh,nmod,pro,yparg),detp,3,4)])).

with_dt([ik,weet,niet,welke],
	determiner(welke,nwh,nmod,pro,yparg),
	dt(smain,[ hd=l(v_root(weet,weten),verb(hebben,sg,tr_sbar),1,2),
		   su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		   mod=l(niet,adverb,advp,2,3),
		   vc=l(welk,determiner(welke,rwh,nmod,pro,yparg),detp,3,4)])).

with_dt([laten,we,zeggen],
	adverb,
	dt(sv1, [hd=l(v_root(laat,laten),verb(hebben,pl,inverted_aux(inf)),0,1),
		 su=ix(SU,l(we,pronoun(nwh,fir,pl,de,nom,def,wkpro),np,1,2)),
		 vc=dt(inf,[hd=l(v_root(zeg,zeggen),verb(hebben,inf,tr_sbar),2,3),
			    su=ix(SU)])])).

with_dt([wat,je,noemt],
        adverb,
        dt(whrel,
           [rhd=ix(RHD,
                   l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),np,0,1)),
            body=dt(ssub,
                    [obj1=ix(RHD),
                     su=l(je,pronoun(nwh,je,sg,de,both,def,wkpro),np,1,2),
                     hd=l(v_root(noem,noemen),verb(hebben,sg3,pred_np),2,3)])
           ])).

with_dt([wat,je,noemt],
        modal_adverb(adv_noun_prep),
        dt(whrel,
           [rhd=ix(RHD,
                   l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),np,0,1)),
            body=dt(ssub,
                    [obj1=ix(RHD),
                     su=l(je,pronoun(nwh,je,sg,de,both,def,wkpro),np,1,2),
                     hd=l(v_root(noem,noemen),verb(hebben,sg3,pred_np),2,3)])
           ])).

with_dt([welk,Een],
	determiner(pron,wh),
	dt(detp,[  hd=l(een,determiner(een),1,2),
		   obj1=l(welk,determiner(welk,rwh,nmod,pro,yparg),detp,0,1)
		])
       ) :- een(Een).

with_dt([van,dat],
	determiner(zulk),
	dt(detp,[ hd=l(dat,determiner(het,nwh,nmod,pro,nparg),1,2),
		  mod=l(van,preposition(van,[]),pp,0,1)
		])
       ).

with_dt([van,deze],
	determiner(zulke),
	dt(detp,[ hd=l(deze,determiner(de,nwh,nmod,pro,yparg),1,2),
		  mod=l(van,preposition(van,[]),pp,0,1)
		])
       ).

with_dt([van,die],
	determiner(zulke),
	dt(detp,[ hd=l(die,determiner(de,nwh,nmod,pro,nparg),1,2),
		  mod=l(van,preposition(van,[]),pp,0,1)
		])
       ).

with_dt([van,dit],
	determiner(zulk),
	dt(detp,[ hd=l(dit,determiner(het,nwh,nmod,pro,nparg),1,2),
		  mod=l(van,preposition(van,[]),pp,0,1)
		])
       ).

with_dt([van,zulk],
	determiner(zulk),
	dt(detp,[ hd=l(zulk,determiner(zulk),1,2),
		  mod=l(van,preposition(van,[]),pp,0,1)
		])
       ).

with_dt([van,zulke],
	determiner(zulke),
	dt(detp,[ hd=l(zulk,determiner(zulke,nwh,nmod,pro,yparg),1,2),
		  mod=l(van,preposition(van,[]),pp,0,1)
		])
       ).

with_dt([één,en,dezelfde],
	determiner(de,nwh,mod,pro,yparg),
	dt(conj,[cnj=l(één,number(hoofd(sg_num)),detp,0,1),
		 crd=l(en,conj(en),conj,1,2),
		 cnj=l(dezelfde,determiner(de,nwh,mod,pro,yparg),detp,2,3)
		])).

with_dt([één,of,meerdere],
	determiner(pl_num,nwh,nmod,pro,yparg),
	dt(conj,[cnj=l(één,number(hoofd(sg_num)),detp,0,1),
		 crd=l(of,conj(of),conj,1,2),
		 cnj=l(meerdere,determiner(pl_num,nwh,nmod,pro,yparg),detp,2,3)
		])).

%m(welk,          determiner(pron,rwh), welks). parse_only_lex
m(wiens,         determiner(pron,rwh), wiens).
m(wier,          determiner(pron,rwh), wier).

m(datzelfde,     comp_determiner(het,als),datzelfde).
m(dezelfde,      comp_determiner(de,als  ),dezelfde).
m(diezelfde,     comp_determiner(de,als ),diezelfde).
m(ditzelfde,     comp_determiner(het,als),ditzelfde).
m(eenzelfde,     comp_determiner(de,als ),eenzelfde).
m(hetzelfde,     comp_determiner(het,als),hetzelfde).

m(genoeg,        comp_determiner(wat,om    ),genoeg).
m(onvoldoende,   comp_determiner(wat,om),onvoldoende).
m(voldoende,     comp_determiner(wat,om ),voldoende).
m('zo\'n',       comp_determiner(een,als  ),'zo\'n').
m('zo\'n',       comp_determiner(een,dat  ),'zo\'n').
m('zo\'n',       comp_determiner(een,dat  ),[Een,'zo\'n']) :- een(Een).

m(zulk,          comp_determiner(zulk,dat),  zulk).
m(zulk,          comp_determiner(zulke,dat), zulke).
m('zulk een',          comp_determiner(een,dat),   [zulk,een]).

with_dt([des,te,meer],
	determiner(wat),
	dt(detp,[mod=l('des te',intensifier,advp,0,2),
		 hd=l(veel,determiner(wat),2,3)])).

with_dt([des,te,meer],
	adverb,
	dt(detp,[mod=l('des te',intensifier,advp,0,2),
		 hd=l(veel,determiner(wat),2,3)])).

m(destemeer,determiner(wat),destemeer).
m(destemeer,adverb,destemeer).

m(destemeer,determiner(wat),[des,temeer]).
m(destemeer,adverb,[des,temeer]).

m(destemeer,determiner(wat),[deste,meer]).
m(destemeer,adverb,[deste,meer]).

m(haar,          gen_determiner(sg),haar).
m(hun,           gen_determiner(pl),hun).
m(zijn,          gen_determiner(sg),zijn).
m(zijn,          gen_determiner(sg),'z\'n').
m('\'s',         gen_determiner(sg),'\'s').
 
%%%%%%%%%%%%%%%%%%%
%%%% PARTICLES %%%%
%%%%%%%%%%%%%%%%%%%

:- discontiguous
    particle/1.

m(Stem, particle(PN), PN) :-
    particle(PN),
    stem_from_surf(PN,Stem).

m('ten onder', particle('ten onder'),[ten,onder]).

particle(aaneen).
particle(aan).
particle([aan,toe]).  % tot X aan toe
particle(achterna).
particle(achterop).
particle(achterover).
particle(achteruit).
particle(achter).
particle(actie).
particle(adem).
particle(af).
particle([af,aan]).
particle(apart).
particle(auto).
particle(beet).
particle(bekend).
particle(belang).
particle(bezig).
particle(bijeen).
particle(bij). 
particle(binnen).
particle(blij).
particle(blijk).
particle(bloot).
particle(bol).
particle(bot).
particle(boven).
particle(buit).
particle(buiten).
particle(daar).
particle(dank).
particle(deel).

with_dt([geen,deel],
        particle(deel),
        dt(np,[hd=l(deel,particle(deel),1,2),
               det=l(geen,determiner(geen),detp,0,1)])).
        
with_dt([geen,stand],
        particle(stand),
        dt(np,[hd=l(stand,particle(stand),1,2),
               det=l(geen,determiner(geen),detp,0,1)])).
        
particle(dicht).
particle(dienst).
particle(dood).
particle(door).
particle(droog).
particle(druk).
particle(duidelijk).  % omdat ik jullie zou duidelijk willen maken...
particle(dwars).
particle(eigen).
particle([en,al]).  % met NP en al
particle(eruit).
particle(feest).
particle(fijn).
particle(flauw).
particle(fout).
particle(gade).
particle(gebruik).  % alleen non-separable, anders obj1  %% NO: is required for generation
particle(geheim).
particle(gelijk).
particle(geluk).
particle(gereed).
particle(gerust).
particle(gevangen).
particle(gewaar).
particle(glad).
particle(goed).
particle(groot).
particle(halt).
particle(hard).
particle(heen).
particle(hoog).
particle(huis).
particle(in).
particle(ineen).
particle(instand).
particle([in,de,plaats]).
particle(kaal).
particle(kaart).
particle(kapot).
particle(kennis).
particle(klaar).
particle(klein).
particle(klem).
particle(kort).
particle(kwalijk).
particle(kwijt).
particle(lam).
particle(langs).
particle(lastig).
particle(leeg).
particle(lek).
particle(les).
particle(lief).
particle(linksaf).
particle(los).
particle(mede).
particle(mee).
particle(meester).
particle(mis).
particle(na).
particle(naast).
particle(nat).
particle(neer).
particle(nodig).
particle(oorlog).
particle(omhoog).
particle(om).
particle(omver).
particle(onder).
particle(onderdoor).  % hij kan er aan onderdoorgaan
particle(ondergedoken).
particle(onderuit).
particle(opeen).
particle(open).
particle(op).
particle(opgesloten).
particle(opzij).
particle(over).
particle(overeen).
particle(overhoop).
particle(paard).
particle(plaats).
particle(plat).
particle(post).
particle(prijs).
particle(proef).
particle(puin).
particle(raak).
particle(recht).
particle(rechtsaf).
particle(rond).
particle(samen).
particle(scheep).
particle(school).
particle(schoon).
particle(schuil).
particle(schuldig).
particle(stand).
particle(stil).
particle(stop).
particle(storm).
particle(stuk).
particle(tegemoet).
particle(tegenover).
particle(tegen).
particle(tekeer).
particle(tekort).
particle(teleur).
particle(teloor).
particle(tengoede).
particle(teniet).
particle(tenonder).
particle(tentoon).
particle(terecht).
particle(terneer).
particle(terug).
particle(tevreden).
particle(teweeg).
particle(tewerk).
particle(thee).
particle(thuis).
particle(tijd).
particle(toe).
particle(totstand).
particle(toneel).
particle(tussen).
particle(uiteen).
particle(uit).
particle(vanaf).
particle(vandaan).
particle(vandoor).
particle(vaneen).
particle(vast).
particle(veilig).
particle(verder).
particle(verrot).
particle(vet).
particle(vlam).
particle(vlot).
particle(vol).
particle(vooraf).
particle(voorbij).
particle(voorlangs).
particle(voorop).
particle(voor).
particle(voort).
particle(vooruit).
particle(vorm).
particle(vreemd).
particle(vrij).
particle(vrijaf).
particle(vuil).
particle(waar).
particle(wacht).
particle(wakker).
particle(warm).
particle(wederop).  % VL
particle(weer).
particle(weg).
particle(wel).
particle(wijs).
particle(wit).
particle(zaken).		% zakendoen
particle(zoek).
particle(ziek).
particle(zorg).			% zorgdragen voor
particle(zwart).

particle([wat,af]).
particle([heel,wat,af]).

%%%%%%%%%%%%%%%%%%%%%%
%%%% PREPOSITIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%

% - can it occur as an NP modifier?
% - can it be the ld or pc complement of a verb?
% - can it occur as a VP modifier to the right of the verb cluster?

m(Stem, preposition(PN,Sub),PN) :-
    preposition(PN,Sub),
    stem_from_surf(PN,Stem).

:- discontiguous
    preposition/1,
    preposition/2.

preposition(Prep,[]) :-
    preposition(Prep).

preposition(aangaande).
preposition(aangezien). % VL
preposition(ad).
preposition(afgezien).
preposition(aldus).
m(dixit,preposition(aldus,[]),dixit). % also for dip
preposition(alias).
preposition(anno).
preposition(à).
m(betreffen,preposition(betreffende,[]),betreffende).
preposition(bezijden).
%preposition(bijgenaamd).
preposition(blijkens).
preposition(conform).
preposition(contra).
preposition(dankzij).
preposition([de,dato]).
preposition(exclusief).
preposition(gaande).
preposition(gaandeweg).
preposition(gedurende).
preposition(gegeven).
%preposition(genaamd).
%preposition(getiteld).
preposition(getuige).
preposition(gezien).
preposition(halfweg).
preposition(halverwege).
preposition(hangende).
preposition(ingevolge).
preposition(indachtig).
preposition(inzake).
preposition(jegens).
preposition(kortweg).   % doelman Joaquim Silva , kortweg Quim , ..
preposition(krachtens).
preposition(lopende).
preposition(luidens).  % juridisch? luidens die en die wet
preposition(middels).
preposition(middenin).
preposition(middenop).
preposition(minus).
preposition(naargelang).
preposition([al,naargelang]).
preposition([al,gelang]).
preposition([al,naar,gelang]).
preposition([al,naargelang,van]).
preposition([al,naar,gelang,van]).
preposition([naar,gelang]).
preposition([naar,gelang,van]).
preposition([naargelang,van]).
preposition(namens).
preposition(niettegenstaande).
preposition(omstreeks).
preposition(omtrent).
preposition(ondanks).
preposition(ongeacht).
preposition(overeenkomstig).
preposition('o.w.').  % onder wie...
preposition(plus).
preposition('+').
preposition(pro).
preposition(qua).
preposition(richting).		% we gaan richting Amsterdam
preposition(rond,[heen]).
preposition(rondom,[heen]).
preposition(sedert).
preposition(sinds).
preposition(sub).
preposition(tijdens).
preposition(wijlens).
preposition([tot,en,met]).
preposition([t,/,m]).
preposition(vanachter).
%preposition(vanaf).
preposition(vanonder).
preposition(vanop).		% vlaams?
preposition(vanover).		% vlaams
preposition(vanuit).
preposition(vanwege).
preposition([vanwege,van]).
preposition([des,weegs]).
preposition(vergelijk).
preposition(via).
preposition(wegens).
preposition(volgens).
m(zijn,preposition(zijnde,[]),zijnde).

preposition('d.d.').            % tja...
preposition('na/op').           % tja...
preposition([na,'(',op,')']).   % tja...
preposition('feat.').           % tja...


preposition('i.c.m.').

m(barstens_vol, preposition(vol,[]), barstensvol).
m(boorde_vol,   preposition(vol,[]), boordevol).
m(bom_vol,      preposition(vol,[]), bomvol).
m(ei_vol,       preposition(vol,[]), eivol).
m(half_vol,     preposition(vol,[]), halfvol).
m(mud_vol,      preposition(vol,[]), mudvol).
m(over_vol,     preposition(vol,[]), overvol).
m(prop_vol,     preposition(vol,[]), propvol).
m(stamp_vol,    preposition(vol,[]), stampvol).
m(tjok_vol,     preposition(vol,[]), tjokvol).
preposition(vol).

m(dichtbij,   preposition(dichtbij,[]), dichtbij).
m(dichtbij,   preposition(dichtbij,[]), dichterbij).
m(dichtbij,   preposition(dichtbij,[]), [dicht,bij]).
m(dichtbij,   preposition(dichtbij,[]), [dichter,bij]).
m(dichtbij,   preposition(dichtbij,[]), [het,dichtst,bij]).
m(dichtbij,   preposition(dichtbij,[]), [het,dichtste,bij]).
m(dichtbij,   preposition(dichtbij,[]), [het,dichtstbij]).
m(dichtbij,   preposition(dichtbij,[]), [het,dichtstebij]).

preposition(aan,[vooraf]).
preposition(achteraan).
preposition(achterin).
preposition(achterop).
preposition(achter,[aan,door,langs,om,uit,vandaan]).
preposition(beneden).
preposition(benoorden).
preposition(bezuiden).
preposition(bij,[vandaan]).
preposition(binnen).
preposition(binnenin).
preposition(boven,[uit]).
preposition(bovenaan).
preposition(bovenin).
preposition(bovenop).
preposition(buiten,[om]).
preposition(door,[heen]).
preposition(doorheen).   % Vlaams
preposition(in).
preposition(langs,[heen]).  % langs elkaar heen..
preposition(langsheen).  % Vlaams
preposition(met,[mee,[en,al]]).
preposition(naar,[toe]).
preposition(naast).
preposition(nabij).
preposition(na).
preposition(om,[heen]).
preposition(omheen). % Vlaams
preposition(onder,[door,vandaan]).
preposition(onderaan).
preposition(onderdoor). % Vlaams
preposition(onderin).
preposition(onderop).
preposition(onderuit).
preposition(op,[af,na]).	% op een haar na; op twee dagen na
preposition(over,[heen]).
preposition(per).  % per een bepaalde periode; per die datum; etc.
preposition(staande).
preposition(tegenover).
preposition(tegen,[aan,in,op]).
preposition(tegenaan). % VL
preposition(tot,[toe,[aan,toe]]).
preposition(tussen,[door,in,vandaan]).
preposition(uit,[vandaan]).
preposition(van,[af,uit,vandaan,[af,aan]]).
preposition(vanaf,[[af,aan]]).   % VL vanaf het begin af aan
preposition(vanonder,[vandaan]).   % alsof hij vanonder de douche vandaan komt
preposition(vanop).  % vlaams
preposition(vlakbij).
preposition(vooraan).  % vooraan de kop 
preposition(voor,[aan,door,uit,[in,de,plaats]]).
preposition(voorbij).
preposition(voorin).
preposition(voorop).  % een bull bar voorop de auto
preposition(zonder).

preposition([à,la]).
preposition([te,oordelen,naar]).

preposition([wat,betreft]).

preposition(hartje).  % hij woont hartje centrum

%mwu heads not supported in with_dt
with_dt([op,grond,echter,van],
	preposition([op,grond,van],[]),
	dt(pp,[hd=l('op grond van',preposition([op,grond,van],[]),pp,[0,1,3],[1,2,4]),
	       obj1=orig(obj1),
	       mod=l(echter,sentence_adverb,advp,2,3)
	      ])).

with_dt([op,grond,dus,van],
	preposition([op,grond,van],[]),
	dt(pp,[hd=l('op grond van',preposition([op,grond,van],[]),pp,[0,1,3],[1,2,4]),
	       obj1=orig(obj1),
	       mod=l(dus,sentence_adverb,advp,2,3)
	      ])).

m(Stem,preposition(Words,[]),Words) :-
    collocational_preposition(Words),
    stem_from_surf(Words,Stem).

m('onder leiding van',preposition([onder,leiding,van],[]),['o.l.v.',van]).

collocational_preposition([à,raison,de]).
collocational_preposition([à,raison,van]).
collocational_preposition([als,gevolg,van]).
collocational_preposition([en,marge,van]).
collocational_preposition([gepaard,aan]).
collocational_preposition([in,geval,van]).
collocational_preposition([ingeval,van]).
collocational_preposition([niet,te,verwarren,met]).
collocational_preposition([om,der,wille,van]).
collocational_preposition([omwille,van]).
collocational_preposition([onder,invloed,van]).
collocational_preposition([onder,auspicien,van]).

%% collocational_preposition([op,weg,naar]).
%% no: omdat ik op weg ben naar ...
collocational_preposition([temidden,van]).
collocational_preposition([ten,bate,van]).
collocational_preposition([ten,dienste,van]).
collocational_preposition([ten,faveure,van]).
collocational_preposition([ten,huize,van]).
collocational_preposition([ten,gehore,van]).
collocational_preposition([ten,laste,van]).
collocational_preposition([ten,ongerieve,van]).
collocational_preposition([ter,attentie,van]).
collocational_preposition([ter,grootte,van]).
collocational_preposition([ter,vervanging,van]).
collocational_preposition([ter,weerszijden,van]).
collocational_preposition([ter,wille,van]).
collocational_preposition([terwille,van]).
collocational_preposition([terzake,van]).
collocational_preposition([uit,Het,oogpunt,van]) :- het(Het).

%% van-PP is obligatory...
%% * hij woont ten noorden
collocational_preposition([ten,noorden,van]).
collocational_preposition([ten,noordoosten,van]).
collocational_preposition([ten,noordwesten,van]).
collocational_preposition([ten,oosten,van]).
collocational_preposition([ten,westen,van]).
collocational_preposition([ten,zuiden,van]).
collocational_preposition([ten,zuidoosten,van]).
collocational_preposition([ten,zuidwesten,van]).
collocational_preposition([te,noorden,van]).
collocational_preposition([te,noordoosten,van]).
collocational_preposition([te,noordwesten,van]).
collocational_preposition([te,oosten,van]).
collocational_preposition([te,westen,van]).
collocational_preposition([te,zuiden,van]).
collocational_preposition([te,zuidoosten,van]).
collocational_preposition([te,zuidwesten,van]).

collocational_preposition([aan,de,hand,van]).            
%%% collocational_preposition([aan,de,orde,van]).
collocational_preposition([aan,de,vooravond,van]).
collocational_preposition([aan,Het,adres,van]) :- het(Het).
%%%  collocational_preposition([aan,het,begin,van]).     % compositional?
collocational_preposition([bij,de,gratie,van]).
collocational_preposition([bij,gebrek,aan]).
collocational_preposition([bij,monde,van]).              
collocational_preposition([bij,wijze,van]).
%%% collocational_preposition([door,gebrek,aan]).        % compositional?
collocational_preposition([door,middel,van]).
collocational_preposition([doormiddel,van]).
collocational_preposition([in,antwoord,op]).
collocational_preposition([in,combinatie,met]).
collocational_preposition([in,de,aanloop,naar]).
%%% collocational_preposition([in,de,buurt,van]).        % compositional?
collocational_preposition([in,de,hitte,van]).
collocational_preposition([in,de,loop,van]).
collocational_preposition([in,het,loop,van]).            % Vlaams?
collocational_preposition([in,de,persoon,van]).
collocational_preposition([in,een,vlaag,van]).
collocational_preposition([in,functie,van]).
%%% collocational_preposition([in,het,begin,van]).       % compositional?
collocational_preposition([in,Het,kader,van]) :- het(Het).
collocational_preposition([in,Het,licht,van]) :- het(Het).
collocational_preposition([in,Het,voetspoor,van]) :- het(Het).
collocational_preposition([in,navolging,van]).
collocational_preposition([in,opdracht,van]).
collocational_preposition([in,overleg,met]).
collocational_preposition([in,plaats,van]).              
collocational_preposition([inplaats,van]).              
collocational_preposition([in,reactie,op]).
collocational_preposition([in,ruil,tegen]).
collocational_preposition([in,ruil,voor]).
collocational_preposition([in,samenwerking,met]).
collocational_preposition([in,tegenstelling,met]).
collocational_preposition([in,tegenstelling,tot]).
collocational_preposition([in,termen,van]).
collocational_preposition([in,verband,met]).             
collocational_preposition([in,vergelijking,met]).
collocational_preposition([in,vergelijking,tot]).
collocational_preposition([in,weerwil,van]).
collocational_preposition([met,behulp,van]).
collocational_preposition([met,betrekking,tot]).
collocational_preposition([met,dank,aan]).
collocational_preposition([met,Het,oog,op]) :- het(Het).
collocational_preposition([met,inachtneming,van]).
collocational_preposition([met,ingang,van]).
collocational_preposition([met,medewerking,van]).
collocational_preposition([met,steun,van]).
collocational_preposition([met,uitzondering,van]).        
collocational_preposition([na,afloop,van]).
collocational_preposition([na,verloop,van]).
collocational_preposition([naar,aanleiding,van]).
collocational_preposition([onder,aanvoering,van]).
collocational_preposition([onder,behandeling,van]).
collocational_preposition([onder,Het,genot,van]) :- het(Het).
collocational_preposition([onder,Het,mom,van]) :- het(Het).
collocational_preposition([onder,leiding,van]).
collocational_preposition([onder,vermelding,van]).
collocational_preposition([onder,voorzitterschap,van]).
collocational_preposition([op,advies,van]).
collocational_preposition([op,basis,van]).
collocational_preposition([op,grond,van]).               
collocational_preposition([op,grondslag,van]).
collocational_preposition([op,Het,gebied,van]) :- het(Het).
collocational_preposition([op,Het,tijdstip,van]) :- het(Het).
%%% collocational_preposition([op,het,voetspoor,van]).       % ???? in ?
collocational_preposition([op,initiatief,van]).
%%% collocational_preposition([op,jacht,naar]).
collocational_preposition([op,kosten,van]).
collocational_preposition([op,last,van]).
collocational_preposition([op,naam,van]).
collocational_preposition([op,straffe,van]).
collocational_preposition([op,uitnodiging,van]).
collocational_preposition([op,verdenking,van]).
collocational_preposition([op,verzoek,van]).
collocational_preposition([op,voet,van]).
collocational_preposition([op,vraag,van]). % VL
collocational_preposition([te,midden]).  % VL
collocational_preposition([te,midden,van]).
collocational_preposition([ten,aanzien,van]).
collocational_preposition([ten,behoeve,van]).
collocational_preposition([ten,belope,van]).   % addition GvN VL?
collocational_preposition([ten,detrimente,van]).  % addition GvN
collocational_preposition([ten,gevolge,van]).           
collocational_preposition([tengevolge,van]).
collocational_preposition([ten,gunste,van]).
collocational_preposition([ten,koste,van]).             
collocational_preposition([ten,nadele,van]).
collocational_preposition([ten,name,van]).              
collocational_preposition([ten,nutte,van]).
collocational_preposition([ten,opzichte,van]).          
collocational_preposition([ten,op,zichte,van]).
collocational_preposition([ten,overstaan,van]).
collocational_preposition([ten,tijde,van]).
collocational_preposition([ten,voordele,van]).
collocational_preposition([ter,bevordering,van]).
collocational_preposition([ter,ere,van]).
collocational_preposition([ter,gelegenheid,van]).       
collocational_preposition([ter,herinnering,aan]).
collocational_preposition([ter,hoogte,van]).
collocational_preposition([ter,nagedachtenis,aan]).
collocational_preposition([ter,voorbereiding,op]).
collocational_preposition([ter,voorbereiding,voor]).
collocational_preposition([ter,waarde,van]).
%%% collocational_preposition([tot,de,kern,van]).
%% removed: only occurs with 'ramp' --> adverb
%% collocational_preposition([tot,overmaat,van]).
collocational_preposition([uit,angst,voor]).
collocational_preposition([uit,hoofde,van]).
collocational_preposition([uit,protest,tegen]).
collocational_preposition([uit,respect,voor]).
collocational_preposition([uit,vrees,voor]).
collocational_preposition([voor,rekening,van]).

%% added by Jack Hoeksema

collocational_preposition([buiten,medeweten,van]).
collocational_preposition([met,medeweten,van]).
collocational_preposition([in,naam,van]).
%collocational_preposition([in,overeenstemming,met]).
%collocational_preposition([in,de,richting,van]).
%collocational_preposition([in,strijd,met]).
%collocational_preposition([met,behoud,van]).
%collocational_preposition([met,toestemming,van]).
collocational_preposition([onder,verwijzing,naar]).
collocational_preposition([op,vertoon,van]).
collocational_preposition([tegen,betaling,van]).
collocational_preposition([uit,kracht,van]).
collocational_preposition([van,de,kant,van]).
collocational_preposition([door,toedoen,van]).


with_dt([zonder,blikken,of,blozen],
	pp(zonder),
	dt(pp,[hd=l(zonder,preposition(zonder,[]),0,1),
	       obj1=dt(conj,[crd=l(of,conj(of),vg,2,3),
			     cnj=l(v_root(blik,blikken),v_noun(intransitive),inf,1,2),
			     cnj=l(v_root(bloos,blozen),v_noun(intransitive),inf,3,4)
			    ])])).

with_dt([met,vallen,en,opstaan],
	pp(met),
	dt(pp,[hd=l(met,preposition(met,[]),0,1),
	       obj1=dt(conj,[crd=l(en,conj(en),vg,2,3),
			     cnj=l(v_root(val,vallen),v_noun(intransitive),inf,1,2),
			     cnj=l(v_root(sta_op,op_staan),v_noun(intransitive),inf,3,4)
			    ])])).

with_dt([met,hangen,en,wurgen],
	pp(met),
	dt(pp,[hd=l(met,preposition(met,[]),0,1),
	       obj1=dt(conj,[crd=l(en,conj(en),vg,2,3),
			     cnj=l(v_root(hang,hangen),v_noun(intransitive),inf,1,2),
			     cnj=l(v_root(wurg,wurgen),v_noun(intransitive),inf,3,4)
			    ])])).

with_dt([P,Adv],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=l(Adv,adjective(no_e(adv)),ap,1,2)
	      ])) :-
    p_adj_pp(P,Adv).

p_adj_pp(op,oneindig). % blik
p_adj_pp(naar,af).     % terug

with_dt([aan,beter],
	pp(aan),
	dt(pp,[hd=l(aan,preposition(aan,[]),0,1),
	       obj1=l(goed,adjective(er(adv)),ap,1,2)
	      ])).

with_dt([P,Adv],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=l(Adv,adverb,advp,1,2)
	      ])) :-
    p_adv_pp(P,Adv).

p_adv_pp(aan,vroeger).
p_adv_pp(met,toen).
p_adv_pp(met,vroeger).
p_adv_pp(over,vroeger).
p_adv_pp(van,allebei).
p_adv_pp(van,alledag).

p_adv_pp(langs,ginder).

p_adv_pp(op,vandaag). % VL

p_adv_pp(tot,zo).

p_adv_pp(anno,nu).
p_adv_pp(per,direct).
p_adv_pp(per,onmiddellijk).
p_adv_pp(van,oudsher).

with_dt([in,tegenstelling,tot,vroeger],
	pp([in,tegenstelling,tot]),
	dt(pp,[hd=l('in tegenstelling tot',
		    preposition([in,tegenstelling,tot],[]),0,3),
	       obj1=l(vroeg,adjective(er(tmpadv)),ap,3,4)
	      ])).

%% only if it functions as a preposition argument
%% ik rekende op vanmiddag
%% *ik slaap op vanmiddag
%% todo: perhaps this is general feature of prep's?
m(aan,   preposition(aan,[],pc_adv),aan).
m(met,   preposition(met,[],pc_adv),met).
m(naar,  preposition(naar,[],pc_adv),naar).
m(op,    preposition(op,[],pc_adv),op).
m(over,  preposition(over,[],pc_adv),over).
m(tegen, preposition(tegen,[],pc_adv),tegen).
m(tot,   preposition(tot,[],pc_adv),tot).  % het zit me tot hier
m(van,   preposition(van,[],pc_adv),van).
m(voor,  preposition(voor,[],pc_adv),voor).
m(zonder,preposition(zonder,[],pc_adv),zonder).

m(met,   preposition(met,[],pc_vp),met).

m('van heinde en verre',pp(van), [van,heinde,en,verre]).
m('van heinde en verre',pp(van), [van,heinde,en,ver]).

m(terplekke,pp(ter),terplekke).

with_dt([P,Paard],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=l(Paard,noun(both,both,sg),np,1,2)])) :-
    p_n_pp(P,Paard).

p_n_pp(af,fabriek).
p_n_pp(bij,af).   % omdat we terug zijn bij af
p_n_pp(bij,name).
p_n_pp(in,punt).  % VL: in punt spelen (voetbal)
p_n_pp(in,ruste).
p_n_pp(onder,ede).
p_n_pp(te,bed).
p_n_pp(te,bedde).
p_n_pp(te,biecht).
p_n_pp(te,communie).
p_n_pp(te,fiets).
p_n_pp(te,grabbel).
p_n_pp(te,koets).
p_n_pp(te,kooi).
p_n_pp(te,land).
p_n_pp(te,lande).
p_n_pp(te,lijf).
p_n_pp(ten,lande).
p_n_pp(te,linkerzijde).
p_n_pp(te,onzent).
p_n_pp(ten,onzent).
p_n_pp(te,paard).
p_n_pp(te,pronk).
p_n_pp(te,rade).
p_n_pp(te,rechterzijde).
p_n_pp(te,ruste).
p_n_pp(ter,stede).
p_n_pp(te,velde).
p_n_pp(te,voet).
p_n_pp(te,water).
p_n_pp(te,wereld).
p_n_pp(ten,laste).  % personen ten laste
p_n_pp(ten,strijde).
p_n_pp(ter,plekke).
p_n_pp(van,nature).
p_n_pp(van,weggeweest).

p_n_pp(sinds,jongsaf).
p_n_pp(sinds,kindsaf).
p_n_pp(sinds,kleinsaf).
p_n_pp(van,jongsaf).
p_n_pp(van,kindsaf).
p_n_pp(van,kleinsaf).

%% ??
p_n_pp(van,gemeentewege).
p_n_pp(van,overheidswege).
p_n_pp(van,rechtswege).
p_n_pp(van,regeringswege).
p_n_pp(van,rijkswege).
p_n_pp(van,staatswege).

p_n_pp(zonder,erg).

with_dt([P,Det,N],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=dt(np,[det=l(Det,determiner(den),detp,1,2),
			   hd=l(N,noun(both,both,sg),2,3)])
	      ])) :-
    p_det_n_pp(P,Det,N).

p_det_n_pp(om,den,brode).
p_det_n_pp(in,den,beginne).
p_det_n_pp(in,den,lande).
p_det_n_pp(in,den,vreemde).
p_det_n_pp(te,allen,prijze).
p_det_n_pp(ten,allen,kante).
p_det_n_pp(ten,allen,prijze).
p_det_n_pp(te,dien,aanzien).
p_det_n_pp(te,dien,einde).
p_det_n_pp(te,dien,opzichte).
p_det_n_pp(te,dien,tijde).
%p_det_n_pp(te,harer,eer).
%p_det_n_pp(te,mijner,eer).
%p_det_n_pp(te,zijner,eer).
%p_det_n_pp(te,harer,ere).
%p_det_n_pp(te,mijner,ere).
%p_det_n_pp(te,zijner,ere).

p_det_n_pp(te,zijnen,aanzien).
p_det_n_pp(te,zijnen,gunste).
p_det_n_pp(te,zijnen,huize).
p_det_n_pp(te,zijnen,laste).
p_det_n_pp(te,zijnen,nadele).

p_det_n_pp(te,mijnen,aanzien).
p_det_n_pp(te,mijnen,gunste).
p_det_n_pp(te,mijnen,huize).
p_det_n_pp(te,mijnen,laste).
p_det_n_pp(te,mijnen,nadele).

p_det_n_pp(te,haren,aanzien).
p_det_n_pp(te,haren,gunste).
p_det_n_pp(te,haren,huize).
p_det_n_pp(te,haren,laste).
p_det_n_pp(te,haren,nadele).

p_det_n_pp(te,uwen,aanzien).
p_det_n_pp(te,uwen,gunste).
p_det_n_pp(te,uwen,huize).
p_det_n_pp(te,uwen,laste).
p_det_n_pp(te,uwen,nadele).

p_det_n_pp(te,dien,aanzien).
p_det_n_pp(te,dien,dage).
p_det_n_pp(te,dien,einde).
p_det_n_pp(te,dien,opzichte).
p_det_n_pp(te,dien,tijde).

%p_det_n_pp(te,uwer,beschikking).
%p_det_n_pp(te,uwer,informatie).
%p_det_n_pp(te,harer,nagedachtenis).
%p_det_n_pp(te,mijner,nagedachtenis).
%p_det_n_pp(te,zijner,nagedachtenis).
%p_det_n_pp(ter,zijner,eer).
%p_det_n_pp(ter,zijner,ere).
%p_det_n_pp(ter,zijner,glorie).
%p_det_n_pp(ter,zijner,nagedachtenis).
%p_det_n_pp(ter,zijner,promotie).

with_dt([P,Adv,Adj,N],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=dt(np,[mod=dt(ap,[mod=l(Adv,adverb,advp,1,2),
				      hd=l(Adj,adjective(e),2,3)]),
			   hd=l(N,noun(both,both,both),3,4)])
	      ])) :-
    p_adv_adj_n_pp(P,Adv,Adj,N).

p_adv_adj_n_pp(van,heel,goeden,huize).
p_adv_adj_n_pp(in,heel,goeden,doen).
p_adv_adj_n_pp(in,heel,vroeger,tijden).

with_dt([P,Adj,N],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=dt(np,[mod=l(Adj,adjective(e),ap,1,2),
			   hd=l(N,noun(both,both,both),2,3)])
	      ])) :-
    p_adj_n_pp(P,Adj,N).

p_adj_n_pp(in,goede,doen).
p_adj_n_pp(in,goeden,doen).
p_adj_n_pp(in,koelen,bloede).
p_adj_n_pp(in,levende,lijve).
p_adj_n_pp(in,levenden,lijve).
p_adj_n_pp(van,goeden,huize).  % todo: van goeden Arabische huize...
p_adj_n_pp(van,goeden,huizen).  % todo: van goeden Arabische huize...
p_adj_n_pp(in,vroeger,tijden).
p_adj_n_pp(zonder,veel,erg).

with_dt([P,Adv,Part],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=l(Adv,adverb,advp,1,2),
	       hdf=l(Part,particle(Part),part,2,3)
	      ])) :-
    p_adv_part_pp(P,Adv,Part).

p_adv_part_pp(tot,nog,toe).
p_adv_part_pp(van,vooraf,aan). % spelling mistake for "van voor af aan"?

with_dt([P,Adv,Part1,Part2],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=l(Adv,adverb,advp,1,2),
	       hdf=l(Part,particle(Part),part,2,4)
	      ])) :-
    p_adv_part_part_pp(P,Adv,Part1,Part2),
    hdrug_util:concat_all([Part1,Part2],Part,' ').

p_adv_part_part_pp(van,dan,af,aan).  % VL?
p_adv_part_part_pp(van,nu,af,aan).
p_adv_part_part_pp(van,toen,af,aan).
p_adv_part_part_pp(van,voor,af,aan).
p_adv_part_part_pp(van,voren,af,aan).

m(Stem, pp, PN) :-
    pp(PN),
    stem_from_surf(PN,Stem).

m(Stem, pp(Prep), PN) :-
    pp(PN,Prep),
    stem_from_surf(PN,Stem).


:- discontiguous
    pp/1,
    pp/2.

pp(achterelkaar).
pp([à,la,carte]).
pp([ad,limina]).
pp(benedendeks,beneden).
pp(benedenstrooms,beneden).
pp(binnenshuis,binnen).
pp(binnenskamers,binnen).
pp([bij,deze]).
pp([bij,dezen]).
pp(bovendeks,boven).
pp(bovengronds,boven).
pp(bovenstrooms,boven).
pp('b.d.').
pp(buitenshuis,buiten).
pp(buitenslands,buiten).
pp(bijelkaar,bij).
pp(dienaangaande).
pp([door,dik,en,dun]).
pp([en,marge]).
pp([en,profil]).
pp([extra,muros]).
pp('i.h.a.').
pp([in,deze]).
pp([in,dezen]).
pp([in,optima,forma]).
pp([in,residence]).
pp([in,wildeweg]).
pp([in,wilde,weg]).
pp([in,Het,wildeweg]) :- het(Het).
pp([in,Het,wilde,weg]) :- het(Het).
pp(ineen,in).
pp([langs,achter]).   % Vlaams
pp([langs,achteren]). % Vlaams
pp([met,heen,en,terug]). % VL
pp(nabeurs).
pp(nadien).
pp([op,heterdaad]).
pp([op,love]). % tennis
pp(perongeluk).  % spelling mistake, really
pp('p.p.'). 
pp(rondom,rond).
pp(sindskort,sinds).
pp(sindsdien,sinds).
pp([sui,generis]).
pp([te,been]). % Vlaams
pp(tevoren).
pp([te,voren]).
pp([om,des,keizers,baard],om).
pp(onderdeks,onder).
pp(overzee,over).
pp([te,dezen]).  pp([ten,deze]). pp([te,deze]).
pp([te,gepasten,tijde]).
pp([ten,gepasten,tijde]).
pp([te,kust,en,te,keur]).
pp([te,voorschijn],te).
pp([te,wier,laste]).
pp([ter,leering,ende,vermaak]).
pp([ter,lering,ende,vermaak]).
pp([ter,leering,ende,vermaeck]).
pp([ter,lering,ende,vermaeck]).
pp([ter,leringhe,ende,vermaeck]).
pp([tot,leering,ende,vermaak]).
pp([tot,lering,ende,vermaak]).
pp([tot,leering,ende,vermaeck]).
pp([tot,lering,ende,vermaeck]).
pp([tot,leringhe,ende,vermaeck]).
pp('tot-nu-toe').
pp([tot,voor,kort]).
pp(uitelkaar,uit).
pp(vantevoren).

pp([te,Onzen,Noun]):-
    onzen(Onzen),
    noun3(Noun).

onzen(onzen).
onzen(mijnen).
onzen(haren).
onzen(hunnen).
onzen(uwen).

noun3(behoeve).
noun3(opzichte).
noun3(gunste).
noun3(gerieve).
noun3(koste).
noun3(tijde).
noun3(bate).
noun3(aanzien).
noun3(voordele).
noun3(nadele).
noun3(laste).
noun3(huize).
noun3(genoegen).
noun3(huize).
noun3(eer).
noun3(gevalle).

%% todo: if required, these should be with_dt
%% 
pp([van,dag,tot,dag]).
pp([van,jaar,tot,jaar]).
pp([van,week,tot,week]).
pp([van,uur,tot,uur]).
pp([van,groot,tot,klein]).
pp([van,hoog,tot,laag]).
pp([van,horen,zeggen],van).
pp([van,hot,naar,haar]).
pp([van,hot,naar,her]).
pp([van,jewelste]).  % only with nouns, but can be extraposed
pp([van,je,welste]).  % only with nouns, but can be extraposed
pp([van,heb,ik,jou,daar]).  % id.
pp([van,kus,mijn,gat]).     % id; VL
pp([van,kus,'m\'n',gat]).     % id; VL
pp([van,punt,naar,punt]).
pp(vanoudsher).
%% pp(vooraf,voor).
pp(voordien).

pp(elders,elders).

pp(vanachter,van).
pp(vanbinnen,van).   % diep vanbinnen
pp(vanboven,van).
pp(vanbuiten,van).
pp(vanonder,van).
pp(vanvoor,van).   % vlaams


%% optional complements:
pp(zonder,zonder).  % zitten zonder; kunnen zonder; patat zonder
                    % todo: cannot go right of verb?

%% de argument pro en contra
pp(pro).
pp(contra).

%% wikipedia:
%% in de eerste eeuw v.Chr.
pp('v.Chr').
pp('v.Chr.').
pp('v.C.').
pp('n.Chr').
pp('n.Chr.').
pp('n.C.').


%% others:
%% patat met; dan moet het toch maar met;

with_dt([P,N,Part],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,[]),0,1),
	       obj1=l(N,noun(both,both,both),np,1,2),
	       hdf=l(Part,particle(Part),part,2,3)
	      ])) :-
    p_n_part_pp(P,N,Part).

%% todo: van heel kleins af aan
%%       van zeer jongs af (aan)

p_n_part_pp(van,jongs,af).
p_n_part_pp(van,kinds,af).
p_n_part_pp(van,kleins,af).
p_n_part_pp(van,kindsbeen,af).

p_n_part_pp(van,jongs,aan).

p_n_part_pp(van,jongsaf,aan).
p_n_part_pp(van,kindsaf,aan).
p_n_part_pp(van,kleinsaf,aan).

p_n_part_pp(vanaf,jongs,af).
p_n_part_pp(vanaf,kinds,af).
p_n_part_pp(vanaf,kleins,af).
p_n_part_pp(vanaf,kindsbeen,af).

p_n_part_pp(vanaf,jongs,aan).

p_n_part_pp(vanaf,jongsaf,aan).
p_n_part_pp(vanaf,kindsaf,aan).
p_n_part_pp(vanaf,kleinsaf,aan).

p_n_part_pp(sinds,jongs,af).
p_n_part_pp(sinds,kinds,af).
p_n_part_pp(sinds,kleins,af).
p_n_part_pp(sinds,kindsbeen,af).

p_n_part_pp(sinds,jongsaf,aan).
p_n_part_pp(sinds,kindsaf,aan).
p_n_part_pp(sinds,kleinsaf,aan).

with_dt([P,N,Part1,Part2],
	pp(P),
	dt(pp,[hd=l(P,preposition(P,AllPart),0,1),
	       obj1=l(N,noun(both,both,both),np,1,2),
	       hdf=l(Part,particle(Part),part,2,4)
	      ])) :-
    p_n_part_part_pp(P,N,Part1,Part2),
    preposition(P,AllPart),
    hdrug_util:concat_all([Part1,Part2],Part,' ').

p_n_part_part_pp(van,jongs,af,aan).
p_n_part_part_pp(van,kinds,af,aan).
p_n_part_part_pp(van,kleins,af,aan).
p_n_part_part_pp(van,kindsbeen,af,aan).

p_n_part_part_pp(van,jongs,aan,af).

p_n_part_part_pp(vanaf,jongs,af,aan).
p_n_part_part_pp(vanaf,kinds,af,aan).
p_n_part_part_pp(vanaf,kleins,af,aan).
p_n_part_part_pp(vanaf,kindsbeen,af,aan).

p_n_part_part_pp(vanaf,jongs,aan,af).

p_n_part_part_pp(sinds,jongs,af,aan).
p_n_part_part_pp(sinds,kinds,af,aan).
p_n_part_part_pp(sinds,kleins,af,aan).
p_n_part_part_pp(sinds,kindsbeen,af,aan).

%% TODO: this is productive...
with_dt([in,Het,Adj],
	pp(in),
	dt(pp,[hd=l(in,preposition(in,[]),0,1),
	       obj1=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,1,2),
			   hd=l(Adj,adjective(no_e(adv)),2,3)])
	      ])) :-
    in_het_adj_pp(Adj),
    het(Het).

in_het_adj_pp(bijzonder).
in_het_adj_pp(breed).		% ouderwets
in_het_adj_pp(echt).
in_het_adj_pp(groot).
in_het_adj_pp(klein).
in_het_adj_pp(kort).
in_het_adj_pp(lang).

in_het_adj_modal_adverb(bijzonder).

with_dt([in,Het,Adj],
	modal_adverb,
	dt(pp,[hd=l(in,preposition(in,[]),0,1),
	       obj1=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,1,2),
			   hd=l(Adj,adjective(no_e(adv)),2,3)])
	      ])) :-
    in_het_adj_modal_adverb(Adj),
    het(Het).

%% heel in het bijzonder mijn ouders wil ik welkom heten
with_dt([heel,in,Het,Adj],
	modal_adverb,
	dt(pp,[mod=l(heel,intensifier,advp,0,1),
               hd=l(in,preposition(in,[]),1,2),
	       obj1=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,2,3),
			   hd=l(Adj,adjective(no_e(adv)),3,4)])
	      ])) :-
    in_het_adj_modal_adverb(Adj),
    het(Het).

%% TODO: this is productive...
with_dt([om,Het,Adj],
	  pp(om),
	  dt(pp,[hd=l(om,preposition(om,[]),0,1),
		 obj1=l(Stem,adjective(het_st(adv)),ap,1,3)
		])) :-
    om_het_adj_pp(Adj,Stem),
    het(Het).

om_het_adj_pp(hardst,hard).

with_dt([tot,Het,laatst],
	pp(tot),
	dt(pp,[hd=l(tot,preposition(tot,[]),0,1),
	       obj1=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,1,2),
			   hd=l(laatst,adjective(st(adv)),2,3)])
	      ])) :- het(Het).

with_dt([tot,Het,laatst,toe],
	pp(tot),
	dt(pp,[hd=l(tot,preposition(tot,[]),0,1),
	       hdf=l(toe,particle(toe),part,3,4),
	       obj1=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,1,2),
			   hd=l(laatst,adjective(st(adv)),2,3)])
	      ])) :- het(Het).

with_dt([zonder,dat],
	pp(zonder),
	dt(pp,[hd=l(zonder,preposition(zonder,[]),0,1),
	       obj1=l(dat,determiner(het,nwh,nmod,pro,nparg),np,1,2)
	      ])).

with_dt([buiten,dat],
	pp(buiten),
	dt(pp,[hd=l(buiten,preposition(buiten,[]),0,1),
	       obj1=l(dat,determiner(het,nwh,nmod,pro,nparg),np,1,2)
	      ])).

with_dt([ondanks,dat],
	pp(ondanks),
	dt(pp,[hd=l(ondanks,preposition(ondanks,[]),0,1),
	       obj1=l(dat,determiner(het,nwh,nmod,pro,nparg),np,1,2)
	      ])).

with_dt([niet,zonder,reden],
	pp,
	dt(pp,[mod=l(niet,adverb,advp,0,1),
	       hd=l(zonder,preposition(zonder,[]),1,2),
	       obj1=l(reden,noun(de,count,sg),np,2,3)])).

with_dt([niet,zonder],
	preposition(zonder,[]),
	dt(pp,[mod=l(niet,adverb,advp,0,1),
	       hd=l(zonder,preposition(zonder,[]),1,2),
	       obj1=orig(obj1)
	      ])).

m(eer,              preposition(eer,[],sbar),              eer).
m(buiten,           preposition(buiten,[],sbar),           buiten).
m(naast,            preposition(naast,[],sbar),            naast).
m(niettegenstaande, preposition(niettegenstaande,[],sbar), niettegenstaande).
m(ondanks,          preposition(ondanks,[],sbar),          ondanks).
m(vanaf,            preposition(vanaf,[],sbar),            vanaf).
m(zonder,           preposition(zonder,[],sbar),           zonder).

%% van toen ik drie jaar oud was
m(van,              preposition(van,[],mod_sbar),       van).
%% voor als ik jarig ben
m(voor,             preposition(voor,[],mod_sbar),      voor).



m(eender,           preposition(eender,[],of_sbar),        eender).
m(ongeacht,         preposition(ongeacht,[],of_sbar),      ongeacht).
m(onverschillig,    preposition(onverschillig,[],of_sbar), onverschillig).
m(willekeurig,      preposition(willekeurig,[],of_sbar),   willekeurig).

%% ik wil iets weten over hoe dit moet
m(over,             preposition(over,[],redrel),           over).

m(achter,           preposition(achter,[aan,door,langs,uit],pp), achter).
m(achteraan,        preposition(achteraan,[],pp),                achteraan).
m(beneden,          preposition(beneden,[],pp),                  beneden).
m(bezuiden,         preposition(bezuiden,[],pp),                 bezuiden).
m(boven,            preposition(boven,[],pp),                    boven).
m(naar,             preposition(naar,[],pp),                     naar).
m(onder,            preposition(onder,[],pp),                    onder).
m(sinds,            preposition(sinds,[],pp),                    sinds).
m(tot,              preposition(tot,[toe,[aan,toe]],pp),         tot).
m(van,              preposition(van,[af,uit,vandaan],pp),        van).
m(voor,             preposition(voor,[],pp),                     voor).
m(vanaf,            preposition(vanaf,[],pp),                    vanaf).
%% sinds halverwege de oorlog
%% vanaf voor de oorlog ..

m(naar,             preposition(naar,[toe],loc_adv),             naar).
m(tot,              preposition(tot,[toe],loc_adv),              tot).
m(tussen,           preposition(tussen,[],loc_adv),              tussen).
m(van,              preposition(van,[af,uit,vandaan],loc_adv),   van).
m(vanaf,            preposition(vanaf,[],loc_adv),               vanaf).
m(vanuit,           preposition(vanuit,[],loc_adv),              vanuit).

m(binnen,           preposition(binnen,[],tmp_adv),          binnen).
				% binnen nu en vijf jaar:
m('in de loop van', preposition([in,de,loop,van],[],tmp_adv), [in,de,loop,van]).
m('in het loop van',preposition([in,het,loop,van],[],tmp_adv),[in,het,loop,van]).
m('met ingang van', preposition([met,ingang,van],[],tmp_adv), [met,ingang,van]).
m(na,               preposition(na,[],tmp_adv),              na).
m(naar,             preposition(naar,[],tmp_adv),            naar).
                                % heimwee naar vroeger; terug naar vroeger
m(per,              preposition(per,[],tmp_adv),             per).
m(sedert,           preposition(sedert,[],tmp_adv),          sedert).
m(sinds,            preposition(sinds,[],tmp_adv),           sinds).
m(tegen,            preposition(tegen,[],tmp_adv),           tegen).
m(tot,              preposition(tot,[toe],tmp_adv),          tot).
m('tot en met',     preposition([tot,en,met],[],tmp_adv),    [tot,en,met]).
m('tot en met',     preposition([tot,en,met],[],tmp_adv),    [t,/,m]).
m('tot en met',     preposition([tot,en,met],[],tmp_adv),    tem). %VL
m(tussen,           preposition(tussen,[],tmp_adv),          tussen).
				% tussen nu en vijf jaar
m(van,              preposition(van,[af],tmp_adv),           van).
m(vanaf,            preposition(vanaf,[],tmp_adv),           vanaf).
m(voor,             preposition(voor,[],tmp_adv),            voor).

%% daar wil ik heen (ld variant of naar), cf:
%% daar hunker ik naar
m(achteraan,        preposition(achter,[],extracted_np),achteraan).
m(achterdoor,       preposition(achter,[],extracted_np),achterdoor).
m(achterheen,       preposition(achter,[],extracted_np),achterheen).
m(achterlangs,      preposition(achter,[],extracted_np),achterlangs).
m(achteruit,        preposition(achter,[],extracted_np),achteruit).
m(af,               preposition(van,[],extracted_np),af).
m(af,               preposition(af,[],extracted_np),af).
m(bovenuit,         preposition(boven,[],extracted_np),bovenuit).
m(doorheen,         preposition(door,[],extracted_np),doorheen).
m(heen,             preposition(naar,[],extracted_np),heen).
m(mee,              preposition(met,[mee],extracted_np),mee).
m(naartoe,          preposition(naar,[],extracted_np),naartoe).
m(naar,             preposition(naartoe,[],extracted_np),naartoe).
m(omheen,           preposition(om,[],extracted_np),omheen).
m(omheen,           preposition(omheen,[],extracted_np),omheen).
m(omtrent,          preposition(omtrent,[],extracted_np),omtrent).  % VL?
m(onderdoor,        preposition(onder,[],extracted_np),onderdoor).
m(overheen,         preposition(over,[],extracted_np),overheen).
m(overheen,         preposition(overheen,[],extracted_np),overheen).
m(tegenaan,         preposition(tegen,[],extracted_np),tegenaan).
m(tegenaan,         preposition(tegenaan,[],extracted_np),tegenaan).
m(tegenin,          preposition(tegen,[],extracted_np),tegenin).
m(tegenin,          preposition(tegenin,[],extracted_np),tegenin).
m(tegenop,          preposition(tegen,[],extracted_np),tegenop).
m(tegenop,          preposition(tegenop,[],extracted_np),tegenop).
m(tegenop,          preposition(tegenop,[],extracted_np),[tegen,op]).
m(tussendoor,       preposition(tussen,[],extracted_np),tussendoor).
m(tussenin,         preposition(tussen,[],extracted_np),tussenin).
m(tussenuit,        preposition(tussenuit,[],extracted_np),tussenuit).
m(vantussen,        preposition(vantussen,[],extracted_np),vantussen).  % VL
m('van tussen',     preposition([van,tussen],[],extracted_np),[van,tussen]).  % VL
m('van tussenuit',  preposition(tussenuit,[],extracted_np),[van,tussenuit]).  % VL
m(toe,              preposition(tot,[],extracted_np),toe).
m(vanaf,            preposition(vanaf,[],extracted_np),[van,af]).
%% dat zijn bossen om hakhuit vandaan te halen
m(vandaan,          preposition(vandaan,[],extracted_np),vandaan).
m(vandoor,          preposition(vandoor,[],extracted_np),vandoor).
m(vanlangs,         preposition(vanlangs,[],extracted_np),vanlangs).
m(vanuit,           preposition(van,[],extracted_np),vanuit).
m('voor in de plaats',preposition(voor,[],extracted_np),[voor,in,de,plaats]).

m('op los',         preposition([op,los],[],extracted_np),[op,los]).

m('aan\'t',         preposition(aan,[],nodet),  'aan\'t' ).
m(te,               preposition(te, [],nodet),  te       ).
m(te,               preposition(ten,[],nodet),  ten      ).
m(te,               preposition(ter,[],nodet),  ter      ).
m('van de',         preposition(van,[],nodet),  'v/d'    ).
m('van de',         preposition(van,[],nodet),  vd       ).

m(voor,             preposition(voor,[],voor_pred),voor).   
% voor onmogelijk houden/verslijten

m(met,              preposition(met,[],absolute),met).
m(zonder,           preposition(zonder,[],absolute),zonder).

%% van twee weken oud
m(van,              preposition(van,[],me_adj),     van).

%% VL: wegens niet open; wegens overzekerd; wegens te oud
m(vanwege,          preposition(vanwege,[],adj),    vanwege).
m(wegens,           preposition(wegens,[],adj),     wegens).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% COMPLEMENTIZERS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

m(alsmede,          complementizer(root),alsmede).
m('dat wil zeggen', complementizer(root),[dat,wil,zeggen]).
m(doch,             complementizer(root),doch).
m(dus,              complementizer(root),dus).
m(echter,           complementizer(root),echter).
m(en,               complementizer(root),en).
m(immers,           complementizer(root),immers).  % only root (+punctuation)
m(maar,             complementizer(root),maar).
m(noch,             complementizer(root),noch).
m(nochtans,         complementizer(root),nochtans).  % VL?
m(of,               complementizer(root),of).
m(ofwel,            complementizer(root),ofwel).
m(oftewel,          complementizer(root),oftewel).
m(plus,             complementizer(root),plus). % plus dat ik vind dat je moet komen
m(want,             complementizer(root),want).
m('met andere woorden', complementizer(root), [met,andere,woorden]).
m('i.e.',           complementizer(root),'i.e.').

m(dat,              complementizer(dat), dat).

m(of,               complementizer(of),  of).

m(om,               complementizer(om),  om).

m(te,               complementizer(te),  te).

m(alsof,            complementizer(alsof), alsof).

m(van,              complementizer(van), van).

m('aan het',        complementizer(aan_het),[aan,het]).
m('aan het',        complementizer(aan_het),[aan,'\'t']).
m('aan het',        complementizer(aan_het),'aan\'t').

m(uit,              complementizer(uit),[uit]).

m(op,               complementizer(op),[op]).

m(dan,              comparative(dan), dan).
m(als,              comparative(als), als).

m(aleer,            complementizer(inf),aleer).	          % not in CGN
m(alvorens,         complementizer(inf),alvorens).        % cmp in CGN
m(door,             complementizer(inf),door).            % prep
m('in plaats van',  complementizer(inf),[in,plaats,van]). % prep
m('in plaats van',  complementizer(inf),[inplaats,van]).  % prep
m(met,              complementizer(inf),met).             % VL "Met ouder te worden voel ik me milder worden"
m(na,               complementizer(inf),na).              % prep
m(teneinde,         complementizer(inf),teneinde).        % cmp
m('ten einde',      complementizer(inf),[ten,einde]).        % cmp
m(vooraleer,        complementizer(inf),vooraleer).       % not in CGN
m(zonder,           complementizer(inf),zonder).          % prep

m('alleen dan',     complementizer(vp),[alleen,dan]).
m(behalve,          complementizer(vp),behalve).
m(echter,           complementizer(vp),echter).  % echter zonder gevaarlijk te
m('en wel',         complementizer(vp),[en,wel]).
m(evenwel,          complementizer(vp),evenwel).
m('laat staan',     complementizer(vp),[laat,staan]).
m('maar dan',       complementizer(vp),[maar,dan]).
m(namelijk,         complementizer(vp),namelijk).
m('zij het',        complementizer(vp),[zij,het]).
m(zogenaamd,        complementizer(vp),zogenaamd).

m(aangezien,        complementizer,aangezien).
m(aleer,            complementizer,aleer).
m(alhoewel,         complementizer,alhoewel).
m(alvorens,         complementizer,alvorens).
m(daar,             complementizer,daar).
m(doordat,          complementizer,doordat).
m('du moment',      complementizer,[du,moment]).
m('du moment',      complementizer,[du,moment,dat]).
m(eenmaal,          complementizer, eenmaal).    % Vlaams
m(eens,             complementizer, eens).       % Vlaams
m('eens dat',       complementizer, [eens,dat]). % Vlaams
m(eer,              complementizer,eer).
m(gezien,           complementizer,gezien).      % Vlaams
m(hoewel,           complementizer,hoewel).
m(hoezeer,          complementizer,hoezeer). % Hoezeer ik 't ook betreur, we..
m(indien,           complementizer,indien).
m(ingeval,          complementizer,ingeval).
m(ingeval,          complementizer,[in,geval]).
m(ingeval,          complementizer,[in,Het,geval]) :- het(Het).
m('in plaats dat',  complementizer,[in,plaats,dat]).
m('voor het geval', complementizer,[voor,Het,geval]) :- het(Het).
m('met dat',        complementizer,[met,dat]). % "net toen"
m('met dien verstande dat',
                    complementizer,[met,dien,verstande,dat]).
m(mits,             complementizer,mits).
%% m(naar,             complementizer,naar).
m(naargelang,       complementizer,[naar,gelang]).
m(naargelang,       complementizer,naargelang).
m('al naargelang',  complementizer,[al,naar,gelang]).
m('al naargelang',  complementizer,[al,naargelang]).
m('al gelang',      complementizer,[al,gelang]).
m(naarmate,         complementizer,naarmate).
m(nadat,            complementizer,nadat).
m(niettegenstaande, complementizer,niettegenstaande).
m(nou,              complementizer,nou).  % nou je het zegt
m(nu,               complementizer,nu).
m(ofschoon,         complementizer,ofschoon).
m(omdat,            complementizer,omdat).
m(ondanks,          complementizer,ondanks).  % regionaal?
				              % Ondanks ik Pino ontzettend waardeer .
                                              % Ondanks ik in België woon was het ook voor mij niet simpel om ...
m(opdat,            complementizer,opdat).
m(sedert,           complementizer,sedert).
m(sinds,            complementizer,sinds).
m(sinds,            complementizer,sindsdat).
m('sinds dat',      complementizer,[sinds,dat]).
m('tegen dat',      complementizer,[tegen,dat]).
m(telkens,          complementizer,telkens). % Vlaams
m(temeer,           complementizer,temeer). % VL temeer we op cruciale momenten zelf in de fout gingen .
m('te veel',        complementizer,[te,meer]). % VL temeer we op cruciale momenten zelf in de fout gingen .
m(tenzij,           complementizer,tenzij).
m(terwijl,          complementizer,terwijl).
m(toen,             complementizer,toen).
m(tot,              complementizer,tot).
m(totdat,           complementizer,totdat).
m(vermits,          complementizer,vermits).
m('van zodra',      complementizer,[van,zodra]).
m(voor,             complementizer,voor).
m(vooraleer,        complementizer,vooraleer).
m(voordat,          complementizer,voordat).
m(wanneer,          complementizer,wanneer).  % perhaps only as red.rel?
m('ware het niet dat',complementizer,[ware,Het,niet,dat]) :- het(Het).
m(zo,               complementizer,zo).
m(zodat,            complementizer,zodat).
m(zodra,            complementizer,zodra).
m('zo gauw',           complementizer,zogauw).
m('zo gauw',           complementizer,[zo,gauw]).
m(zolang,           complementizer,zolang).
m(zolang,           complementizer,[zo,lang]).

m(als,              complementizer(start), als).
m(zoals,            complementizer(start), zoals).

m(als,              complementizer(als),  als).
m('als en alleen als',              complementizer(als),  [als,en,alleen,als]).
m('als en slechts als',              complementizer(als),  [als,en,slechts,als]).
m('dan en slechts dan',              complementizer(als),  [dan,en,slechts,dan]).
m('dan en slechts dan als',              complementizer(als),  [dan,en,slechts,dan,als]).

m(evenals,          complementizer(zoals),evenals).
m(gelijk,           complementizer(zoals),gelijk).
m(inzover,          complementizer(zoals),inzover).
m(inzoverre,        complementizer(zoals),inzoverre).
m(inzoverre,        complementizer(zoals),[in,zoverre]).
m(voorzover,        complementizer(zoals),voorzover).
m(zoals,            complementizer(zoals),zoals).
m(zover,            complementizer(zoals),zover).
m('zover als',      complementizer(zoals),[zover,als]).
m('voor zover als', complementizer(zoals),[voor,zover,als]).
m(zover,            complementizer(zoals),[zo,ver]).
m(zowaar,           complementizer(zoals),zowaar).

m(alhoewel,         complementizer(a),alhoewel).
m('alleen dan',     complementizer(a),[alleen,dan]).
m(alsook,           complementizer(a),alsook).
m(behalve,          complementizer(a),behalve).
m('dat wil zeggen', complementizer(a),[dat,wil,zeggen]).
m(hoewel,           complementizer(a),hoewel).
m('in plaats van',  complementizer(a),[inplaats,van]).
m('in plaats van',  complementizer(a),[in,plaats,van]).
m(indien,           complementizer(a),indien).
m('laat staan',     complementizer(a),[laat,staan]).
m('maar dan',       complementizer(a),[maar,dan]).
m(mits,             complementizer(a),mits).
m(ofschoon,         complementizer(a),ofschoon).
m('ten opzichte van',complementizer(a),[ten,opzichte,van]).
m(tenzij,           complementizer(a),tenzij).
m(waar,             complementizer(a),waar).  % waar mogelijk
m(wanneer,          complementizer(a),wanneer).
m(zodra,            complementizer(a),zodra). % VL?
m('zij het',        complementizer(a),[zij,het]).
m('cfr',            complementizer(a),'cf.').
m('cfr.',           complementizer(a),'cfr.').
m('d.i.',           complementizer(a),'d.i.').
m('i.e.',           complementizer(a),'i.e.').

m(alhoewel,         complementizer(pp),alhoewel).
m('alleen dan',     complementizer(pp),[alleen,dan]).
m(alsook,           complementizer(pp),alsook).
m(behalve,          complementizer(pp),behalve).
m(behoudens,        complementizer(pp),behoudens).
m(benevens,         complementizer(pp),benevens).
m('dat wil zeggen', complementizer(pp),[dat,wil,zeggen]).
m(echter,           complementizer(pp),echter). % echter zonder resultaat
m('en wel',         complementizer(pp),[en,wel]).
m(evenwel,          complementizer(pp),evenwel).
m(hoewel,           complementizer(pp),hoewel).
m('in casu',        complementizer(pp),[in,casu]).
m(indien,           complementizer(pp),indien).
m('in plaats van',  complementizer(pp),[inplaats,van]).
m('in plaats van',  complementizer(pp),[in,plaats,van]).
m('in tegenstelling tot',
                    complementizer(pp),[in,tegenstelling,tot]).
m('laat staan',     complementizer(pp),[laat,staan]).
m('maar dan',       complementizer(pp),[maar,dan]).
m(mits,             complementizer(pp),mits).
m(namelijk,         complementizer(pp),namelijk).
m(ofschoon,         complementizer(pp),ofschoon).
m(tenzij,           complementizer(pp),tenzij).
m('te weten',       complementizer(pp),[te,weten]).
m(waar,             complementizer(pp),waar).
m(wanneer,          complementizer(pp),wanneer).
m('zij het',        complementizer(pp),[zij,het]).
m('zo niet',           complementizer(pp),[zo,niet]). % zo niet in Groningen
m('zo niet',           complementizer(pp),zoniet).    % zoniet in Groningen
m('zo ook',         complementizer(pp),[zo,ook]).
m(zogenaamd,        complementizer(pp),zogenaamd).
m('cf.',            complementizer(pp),'cf.').
m('cfr.',           complementizer(pp),'cfr.').
m('d.i.',           complementizer(pp),'d.i.').
m('i.e.',           complementizer(pp),'i.e.').

/*
with_dt([behalve,misschien],
	complementizer(pp),
	dt(cp,[mod=l(misschien,sentence_adverb,advp,1,2),
	       cmp=l(behalve,complementizer(pp),cp,0,1),
	       body=orig(body)])).

with_dt([behalve,dan],
	complementizer(pp),
	dt(cp,[cmp=l(behalve,complementizer(pp),cp,0,1),
	       mod=l(dan,adverb,advp,1,2),
               body=orig(body)])).

with_dt([behalve,dus],
	complementizer(pp),
	dt(cp,[cmp=l(behalve,complementizer(pp),cp,0,1),
	       mod=l(dus,sentence_adverb,advp,1,2),
               body=orig(body)])).

with_dt([behalve,natuurlijk],
	complementizer(pp),
	dt(cp,[cmp=l(behalve,complementizer(pp),cp,0,1),
	       mod=l(natuurlijk,adjective(no_e(sentadv)),ap,1,2),
               body=orig(body)])).
*/
m(alsook,           complementizer(np),alsook).
m(behalve,          complementizer(np),behalve).
m(behoudens,        complementizer(np),behoudens).
m(benevens,         complementizer(np),benevens).
m('dat wil zeggen', complementizer(np),[dat,wil,zeggen]).
m('en wel',         complementizer(np),[en,wel]).
m('in casu',        complementizer(np),[in,casu]).
m(inclusief,        complementizer(np),inclusief).
m('laat staan',     complementizer(np),[laat,staan]).
m(mits,             complementizer(np),mits).  % VLAAMS
m(namelijk,         complementizer(np),namelijk).
m('te weten',       complementizer(np),[te,weten]).
m('zo niet',        complementizer(np),[zo,niet]). % zo niet de Nederlanders
m('zo niet',        complementizer(np),zoniet). % zo niet de Nederlanders
m('zo ook',         complementizer(np),[zo,ook]).  
m('zij het',        complementizer(np),[zij,het]).
m('cfr',            complementizer(np),'cf.').
m('cfr.',           complementizer(np),'cfr.').
m('d.i.',           complementizer(np),'d.i.').
m('i.e.',           complementizer(np),'i.e.').

/*
with_dt([behalve,misschien],
	complementizer(np),
	dt(cp,[mod=l(misschien,sentence_adverb,advp,1,2),
	       cmp=l(behalve,complementizer(np),cp,0,1),
	       body=orig(body)])).

with_dt([behalve,dan],
	complementizer(np),
	dt(cp,[cmp=l(behalve,complementizer(np),cp,0,1),
	       mod=l(dan,adverb,advp,1,2),
               body=orig(body)])).

with_dt([behalve,natuurlijk],
	complementizer(np),
	dt(cp,[cmp=l(behalve,complementizer(pp),cp,0,1),
	       mod=l(natuurlijk,adjective(no_e(sentadv)),ap,1,2),
               body=orig(body)])).
*/

m('alleen dan',     complementizer(adv),[alleen,dan]).
m(alsook,           complementizer(adv),alsook).
m(behalve,          complementizer(adv),behalve).
m('dat wil zeggen', complementizer(adv),[dat,wil,zeggen]).
m('en wel',         complementizer(adv),[en,wel]).
m('laat staan',     complementizer(adv),[laat,staan]).
m('maar dan',       complementizer(adv),[maar,dan]).
m('te weten',       complementizer(adv),[te,weten]).
m('zo niet',        complementizer(adv),[zo,niet]). % zo niet vandaag
m('zo niet',        complementizer(adv),zoniet). % zo niet vandaag
m('zo ook',         complementizer(adv),[zo,ook]).  % zo niet vandaag
m('cfr',            complementizer(adv),'cf.').
m('cfr.',           complementizer(adv),'cfr.').
m('d.i.',           complementizer(adv),'d.i.').
m('i.e.',           complementizer(adv),'i.e.').

/*
with_dt([behalve,misschien],
	complementizer(sbar),
	dt(cp,[mod=l(misschien,sentence_adverb,advp,1,2),
	       cmp=l(behalve,complementizer(np),cp,0,1),
	       body=orig(body)])).

with_dt([behalve,dan],
	complementizer(sbar),
	dt(cp,[cmp=l(behalve,complementizer(np),cp,0,1),
	       mod=l(dan,adverb,advp,1,2),
               body=orig(body)])).

with_dt([behalve,natuurlijk],
	complementizer(sbar),
	dt(cp,[cmp=l(behalve,complementizer(pp),cp,0,1),
	       mod=l(natuurlijk,adjective(no_e(sentadv)),ap,1,2),
               body=orig(body)])).
*/

%% naar wij verwachten
%% consumes sbar on slash...
m(naar,             complementizer(naar),naar).  

m(al,               complementizer(al),al).  % al is de leugen ...
m(of,               complementizer(al),of).  % of zal ik je 'ns in elkaar rammen

m('alleen dan',     complementizer(sbar),[alleen,dan]).
m(behalve,          complementizer(sbar),behalve).
m(behoudens,        complementizer(sbar),behoudens).  % behoudens indien
m('dat wil zeggen', complementizer(sbar),[dat,wil,zeggen]).
m('en wel',         complementizer(sbar),[en,wel]).
m('in plaats van',  complementizer(sbar),[inplaats,van]).
m('in plaats van',  complementizer(sbar),[in,plaats,van]).
m('laat staan',     complementizer(sbar),[laat,staan]).
m('maar dan',       complementizer(sbar),[maar,dan]).
m(naast,            complementizer(sbar),naast).  % "behalve" naast dat ...
m(namelijk,         complementizer(sbar),namelijk).
m(tenzij,           complementizer(sbar),tenzij).  % tenzij als schrijfster
m('zij het',        complementizer(sbar),[zij,het]).

with_dt([liever,vandaag,dan,morgen],
	sentence_adverb,
	dt(du,[dp=l(vandaag,tmp_adverb,advp,1,2),
	       dp=dt(ap,[hd=l(lief,adjective(er(adv)),0,1),
			 obcomp=dt(cp,[cmp=l(dan,comparative(dan),comparative,2,3),
				       body=l(morgen,tmp_adverb,advp,3,4)])])])).

%%%%%%%%%%%%%%%%%%%%%%
%%%% CONJUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%

m(Stem,conj(Form),Surf) :-
    conj(Stem,Surf,Form).

m(Stem,conj(Stem),Surf) :-
    conj(Stem,Surf).

:- discontiguous
    conj/2,
    conj/3.

conj('&',              '&').
conj(à,                à).
conj(alsmede,          alsmede,   maar).
conj(alsook,           alsook,    maar).
conj(alswel,           [als,wel], maar).
conj(alswel,           alswel,    maar).
conj(annex,            annex).
conj('c.q.',           cq).
conj('c.q.',           'cq.').
conj('c.q.',           'c.q.').
conj('dan wel',        [dan,wel], maar).
conj(danwel,           danwel,    maar).
conj(doch,             doch).
conj(dus,              dus).
conj(edoch,            edoch).
conj(en,               en).
conj('en/of',          'en/of').
conj('en/of',          [en,/,of]).
conj('en/of',          'e/o').
conj('en/of',          [en,'(',of,')']).
conj(hetzij,           hetzij).
conj('laat staan',     [laat,staan]).
conj(maal,             maal).
conj(maar,             maar).
conj(min,              min).
conj(noch,             noch).
conj(of,               of).
conj(om,               om).  % dat is lood om oud ijzer
conj(ofwel,            ofwel).
conj(oftewel,          oftewel).
conj(onderscheidenlijk,onderscheidenlijk).
conj(respectievelijk,  respectievelijk).
conj(tegen,            tegen).
conj(tegenover,        tegenover).
conj(tot,              tot).
conj('tot en met',     [tot,en,met]).
conj('tot en met',     [t,/,m]).
conj(versus,           versus).
conj(want,             want).
conj('met andere woorden',[met,andere,woorden]).
conj('zo niet',             [zo,niet]).
conj('zo niet',             zoniet).
conj('zowel als',        [zowel,als]).

m(evenveel,              left_conj(als),evenveel).
m(zowel,                 left_conj(als),zowel).
m(als,                   right_conj(als),als).

m('niet zozeer',         left_conj(maar),[niet,zozeer]).
m('niet eens zozeer',    left_conj(maar),[niet,eens,zozeer]).
m('niet alleen',         left_conj(maar),[niet,alleen]).
m(niet,                  left_conj(maar),niet).
m('niet eens',           left_conj(maar),[niet,eens]).

m('nu eens',             left_conj(nu_eens_dan),[nu,eens]).
m('dan weer',            right_conj(nu_eens_dan),[dan,weer]).

m(eens,             left_conj(eens_altijd),  eens).
m(altijd,           right_conj(eens_altijd), altijd).

%% eerder voor dan na de voorstelling
%% eerder ondanks dan dankzij ...
m(eerder,           left_conj(eerder_dan),eerder).
m(dan,              right_conj(eerder_dan),  dan).

m(zowel,            left_conj(en),zowel).

m(achtereenvolgens, left_conj(en),achtereenvolgens).
m(behalve,          left_conj(en),behalve).
m(noch,             left_conj(noch),noch).
m(of,               left_conj(of),of).
m(ofwel,            left_conj(ofwel),ofwel).
m(en,               left_conj(en),en).
m(hetzij,           left_conj(hetzij),hetzij).
m(hetzij,           left_conj(of),hetzij).
m(respectievelijk,  left_conj(en),respectievelijk).

m(Stem, etc, Surf):-
    etc(Surf),
    stem_from_surf(Surf,Stem).

etc([cum,suis]).  % X cum suis --> kan als meervoud worden gebruikt
etc(etcetera).
etc(enzoverder).
etc(enzovoort).
etc(enzovoorts).
etc(enzo).
etc([en,zo,voort]).
etc([en,zo,voorts]).
etc([en,dergelijke]).
etc([en,dies,meer]).
etc([en,wat,dies,meer,zij]).
etc([en,wat,niet,al]).
etc('e.a').
etc('e.a.').
etc('e.d').
etc('e.d.').
etc('e.v').
etc('e.v.').
etc('e.v.a').
etc('e.v.a.').
etc([en,noem,maar,op]).
etc([et,cetera]).
etc([noem,maar,op]).
etc([en,ga,zo,maar,door]).
etc('o.i.d').
etc('o.i.d.').
etc(ofzo).
etc(ofzoiets).
etc([en,'that\'s',it]).

with_dt([en,niet,anders],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=dt(ap,[mod=l(niet,adverb,advp,1,2),
			    hd=l(ander,adjective(anders),2,3)])])).

with_dt([en,niet,andersom],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=dt(ap,[mod=l(niet,adverb,advp,1,2),
			    hd=l(andersom,adjective(pred(adv)),2,3)])])).

with_dt([en,andersom],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(andersom,adjective(pred(adv)),ap,1,2)])).

with_dt([en,daarmee,uit],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=dt(du,[dp=l(daarmee,er_adverb(met),pp,1,2),
			    dp=l(uit,adjective(pred(adv)),ap,2,3)])
		])).

with_dt([en,zo],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(zo,adverb,advp,1,2)])).

with_dt([en,dan],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(dan,tmp_adverb,advp,1,2)])).

with_dt([en,klaar],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(klaar,adjective(no_e(adv)),ap,1,2)])).

with_dt([en,omgekeerd],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(omgekeerd,adverb,advp,1,2)])).

with_dt([en,terecht],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(terecht,adjective(no_e(adv)),ap,1,2)])).

with_dt([en,vice,versa],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l('vice versa',adverb,advp,1,3)])).

with_dt([en,zo],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(zo,adverb,advp,1,2)])).

with_dt([en,verder],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l(ver,dir_adverb,advp,1,2)])).

with_dt([of,andersom],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(andersom,adjective(pred(adv)),advp,1,2)])).

with_dt([of,vice,versa],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l('vice versa',adverb,advp,1,3)])).

with_dt([of,allebei],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(allebei,predm_adverb,advp,1,2)])).

with_dt([of,anderszins],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(anderszins,adverb,advp,1,2)])).

with_dt([of,daaromtrent],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(daaromtrent,er_adverb(omtrent),advp,1,2)])).

with_dt([of,niet],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(niet,adverb,advp,1,2)])).

with_dt([of,toch,niet],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=dt(advp,[hd=l(niet,adverb,advp,2,3),
			      mod=l(toch,adverb,advp,1,2)
			     ])])).

with_dt([of,omgekeerd],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(omgekeerd,adverb,advp,1,2)])).

with_dt([of,wel],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(wel,adverb,advp,1,2)])).

with_dt([of,zo],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(zo,adverb,advp,1,2)])).


with_dt([of,zoiets],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(zoiets,pronoun(nwh,thi,sg,het,both,indef),np,1,2)])).

with_dt([of,zo,iets],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l('zo iets',pronoun(nwh,thi,sg,het,both,indef),np,1,3)])).

with_dt([of,iets,dergelijks],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=dt(np,[hd=l(iets,iets_noun,1,2),
			    mod=l(dergelijks,post_adjective(no_e),ap,2,3)
			   ])])).

with_dt([of,iets,soortgelijks],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=dt(np,[hd=l(iets,iets_noun,1,2),
			    mod=l(soortgelijks,post_adjective(no_e),ap,2,3)
			   ])])).

with_dt([en,niet,'zo\'n',beetje,ook],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=l('niet zo\'n beetje ook',adverb,advp,1,5)])).

with_dt([of,hoger],
	complex_etc,
	dt(conj,[crd=l(of,conj(of),vg,0,1),
		 cnj=l(hoog,adjective(er(adv)),ap,1,2)])).

with_dt([en,met,succes],
	complex_etc,
	dt(conj,[crd=l(en,conj(en),vg,0,1),
		 cnj=dt(pp,[hd=l(met,preposition(met,[mee,[en,al]]),pp,1,2),
			    obj1=l(succes,noun(het,count,sg),np,2,3)
			    ]
		       )])).

%%%%%%%%%%%%%%%%%%%%%
%%%% ADJECTIVES %%%%%
%%%%%%%%%%%%%%%%%%%%%

%% todo: can occur right of verb-cluster
with_dt([met,Hun,Tweeen],
	adjective(pred(padv)),
	dt(pp,[hd=l(met,preposition(met,[]),0,1),
	       obj1=dt(np,[det=l(Hun,determiner(pron),detp,1,2),
			   hd=l(Lemma,Postag,2,3)])])) :-

    zijn(Hun),
    wij_allebei(Tweeen,Lemma,Postag,_Cat).

zijn(ons).
zijn(jullie).
zijn(hun).
zijn(zijn).
zijn('z\'n').

m('het één',     adjective(het_st(adv)),   [Het,eerst]) :- het(Het).
m('het één',     adjective(het_st(oadv)), [Het,eerste]) :- het(Het).

%%%% TODO: netzomin/evenmin/evenveel XP als XP ... (conj?)
m(evenmin,          als_adjective(both(adv)),evenmin).
m(evenveel,         als_adjective(no_e(odet_adv)),evenveel).
m(evenzeer,         als_adjective(both(adv)),evenzeer).
m(netzomin,         als_adjective(both(adv)),netzomin).
m(netzomin,         als_adjective(both(adv)),[net,zomin]).
m(soortgelijk,      als_adjective(no_e(nonadv)),soortgelijk).
m(soortgelijk,      als_adjective(e),soortgelijke).
m(zelfde,           als_adjective(e),zelfde).
m(zomin,            als_adjective(both(adv)),zomin).
m(zoveel,           als_adjective(no_e(odet_adv)),zoveel).

m(even,             comp_adverb(als),even).
m(zo,               comp_adverb(als),zo).
% also with of: "geen mens is zo dom of hij weet waar Rome ligt"

m(veeleer,          comp_adverb(dan),veeleer).

%% ik slaap zoveel ik kan
%% wij eten zoveel we lusten obj1/mod??
%% todo: similar constructs with "zo ADJ"
%%       ik kom zo vaak ik wil
%%       ik schrijf zo lelijk ik maar wil
%% it looks like the "als" is optional...
%% in such cases, the obcomp can not be extraposed?
%%       ik kus zoveel ik wil
%%       ik kus zoveel meisjes vandaag ik wil
%% ik sla zo hard ik zelf wil
%% zo snel ik kon
%%
%% TODO: extraposition
%% we mogen eten zoveel we willen
%%
%% TODO: zoveel consumes embedded arg
%% we mogen eten zoveel we lusten[trans]

m(zoveel,           e_als_adjective(no_e(odet_adv)),zoveel).

m(zo,               comp_adverb(e_als),zo).


%% np_adjective: np to e_deps;
%% always no_e and nonadv
%% ordinarily, subcat of adjectives are always optional. But these
%% complements are really obligatory!
%% * ik ben beu
m(beschoren,        np_adjective,beschoren    ).
m(beu,              np_adjective,beu          ).
m(kots_beu,         np_adjective,kotsbeu          ).
m(eens,             np_adjective,eens         ).
m(wennen,           np_adjective,gewend       ).
m(gewoon,           np_adjective,gewoon       ).
m(oneens,           np_adjective,oneens       ).
m(spuug_zat,        np_adjective,spuugzat     ).
m(troef,            np_adjective,troef        ). % Het was armoe troef
m(zat,              np_adjective,zat          ).
m(erkentelijk,      np_adjective,erkentelijk  ).
m(kwijt,            np_adjective,kwijt        ).
m(machtig,          np_adjective,machtig      ). % ik ben het Frans helaas niet machtig
m(meester,          np_adjective,meester      ).
m(moe,              np_adjective,moe          ).
m(rijk,             np_adjective,rijk         ). % De stad is 5 kerken rijk
m(schuldig,         np_adjective,schuldig     ).
m('te slim af',     np_adjective,[te,slim,af] ).
m('te snel af',     np_adjective,[te,snel,af] ).
m('te vlug af',     np_adjective,[te,vlug,af] ).
m(trouw,            np_adjective,trouw        ).
m('van zins',       np_adjective,[van,zins]   ).
m(verplicht,        np_adjective,verplicht    ).
m(verschuldigd,     np_adjective,verschuldigd ).
m(vooruit,          np_adjective,vooruit      ). % hij is zijn tijd vooruit
m(waard,            np_adjective,waard        ).

% Gronings?
m(nodig,            np_adjective, nodig).

m(beu,              clause_np_adjective, beu).
m(kots_beu,         clause_np_adjective, kotsbeu).
m(eens,             clause_np_adjective, eens).
m(eens,             clause_np_adjective(pp(met)), eens). % we zijn het met u eens dat ...
m(wennen,           clause_np_adjective, gewend).
m(gewoon,           clause_np_adjective, gewoon).
m(moe,              clause_np_adjective, moe).           % VL
m(oneens,           clause_np_adjective, oneens).
m(spuug_zat,        clause_np_adjective, spuugzat).
m(waard,            clause_np_adjective, waard).
m(zat,              clause_np_adjective, zat).

% m(waard,            pred_np_me_adjective(nonadv),waard).
m(waard,            subject_sbar_pred_np_adjective,waard).
m(waard,            subject_vp_pred_np_adjective,waard).

m(eens,             adjective(no_e(padv),object_sbar),    eens).  % ik ben met je eens dat ...

m(eens,             np_adjective(pp(met)),                eens).
m(eens,             het_np_adjective(pp(over)),           eens).
m(eens,             het_np_adjective(er_pp_sbar(met)),    eens).
m(eens,             het_np_adjective(er_pp_vp(met)),      eens).
m(eens,             het_np_adjective(er_pp_sbar(over)),   eens).
m(eens,             het_np_adjective(er_pp_vp(over)),     eens).

m(oneens,           np_adjective(pp(met)),                oneens).
m(oneens,           het_np_adjective(pp(over)),           oneens).
m(oneens,           het_np_adjective(er_pp_sbar(met)),    oneens).
m(oneens,           het_np_adjective(er_pp_vp(met)),      oneens).
m(oneens,           het_np_adjective(er_pp_sbar(over)),   oneens).
m(oneens,           het_np_adjective(er_pp_vp(over)),     oneens).

%% VL: het afgebold zijn: er mee opgehouden
m(afgebold,         het_np_adjective,                     afgebold).

m(kwijt,            np_adjective(pp(aan)), kwijt).
				% we zijn er een fortuin aan kwijt

m(erkentelijk,      np_adjective(pp(voor)),   erkentelijk).
m(erkentelijk,      np_adjective(er_pp_sbar(voor)),   erkentelijk).
m(schuldig,         np_adjective(so_np),      schuldig).
m(schuldig,         np_adjective(so_pp(aan)), schuldig).
m(verschuldigd,     np_adjective(so_np),      verschuldigd).
m(verschuldigd,     np_adjective(so_pp(aan)), verschuldigd).
m(verplicht,        np_adjective(so_np),      verplicht).
m(verplicht,        np_adjective(so_pp(aan)), verplicht).

m(Stem,np_me_adjective(e),Surf) :-
    np_me_adjective(Stem,Surf,_Adv).
m(Stem,np_me_adjective(no_e(Adv)),Stem) :-
    np_me_adjective(Stem,_Surf,Adv).
m(Stem,np_me_adjective(no_e(Adv)),Stem) :-
    np_me_adjective(Stem,Adv).

m(achter,           pred_np_me_adjective(nonadv),achter).
m(achtereen,        pred_np_me_adjective(adv),achtereen).
m(achterop,         pred_np_me_adjective(dir_locadv),achterop).
m(bergaf,           pred_np_me_adjective(padv),bergaf).
m(bergop,           pred_np_me_adjective(padv),bergop).
m(geleden,          pred_np_me_adjective(tmpadv),geleden).
m(hogerop,          pred_np_me_adjective(dir_locadv),hogerop).
m(terug,            pred_np_me_adjective(tmpadv),terug).
m(verderop,         pred_np_me_adjective(dir_locadv),verderop).
m(verwijderen,      pred_np_me_adjective(adv),verwijderd).
m(voor,             pred_np_me_adjective(nonadv),voor).

m(geleden,          subject_sbar_pred_np_me_adjective,geleden).
m(terug,            subject_sbar_pred_np_me_adjective,terug).


m(door,             np_me_adjective(both(oadv)), door). % het hele jaar door

m(duren, np_me_adjective(e),          durende).
m(duren, np_me_adjective(no_e(adv)),  durende).

np_me_adjective(breed,brede,adv       ).
np_me_adjective(diep,diepe,locadv        ).
np_me_adjective(dik,dikke,adv         ). % een 300 blz dik boek

%% in het twee graden koude water
np_me_adjective(koud,koude,adv).
np_me_adjective(warm,warme,adv).


%% np_me_adjective(duur,dure,adv         ).  only false hits
np_me_adjective(groot,grote,adv       ).   % een 2 ha groot ..
np_me_adjective(hoog,hoge,adv         ).
np_me_adjective(jong,jonge,adv        ).
np_me_adjective(lang,lange,adv        ).
np_me_adjective(metend,metende,adv    ).
np_me_adjective(oud,oude,adv          ).
np_me_adjective(sterk,sterke,adv      ).
				% de 3 man sterke ..
np_me_adjective(ver,verre,locadv      ). 
np_me_adjective(wegend,wegende,adv    ).
np_me_adjective(zwaar,zware,adv       ).

np_me_adjective(extra,adv).

np_me_adjective(buitengaats,locadv    ).
np_me_adjective(achterwaarts,diradv   ).
np_me_adjective(bergafwaarts,diradv   ).
np_me_adjective(bergopwaarts,diradv   ).
np_me_adjective(binnenwaarts,diradv   ).
np_me_adjective(heuvelafwaarts,diradv ).
np_me_adjective(heuvelopwaarts,diradv ).
np_me_adjective(landinwaarts,diradv   ).
np_me_adjective(noordwaarts,diradv    ).
np_me_adjective(oostwaarts,diradv     ).
np_me_adjective(stroomafwaarts,diradv ).
np_me_adjective(stroomopwaarts,diradv ).
np_me_adjective(westwaarts,diradv     ).
np_me_adjective(zeewaarts,diradv      ).
np_me_adjective(zijwaarts,diradv      ).
np_me_adjective(zuidwaarts,diradv     ).

np_me_adjective(zeker, adv            ).  % de 100% zekere overwinning

%% bridge: hij is/gaat drie down
%%         we spelen hem drie down
np_me_adjective(down,nonadv).

%% hij woont twee kilometer ver van het water
m(ver,np_me_adjective(pp(van),no_e(locadv)),ver).

with_dt([voor,niets],
	adjective(pred(adv)),
	dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
	       obj1=l(niets,noun(het,mass,sg),np,1,2)
	      ])).

m(dermate,          sbar_adjective(both(oadv)), dermate).
m(dusdanig,         sbar_adjective(no_e(adv)),  dusdanig).
m(dusdanig,         sbar_adjective(e),          dusdanige).
m('in dier voege',  sbar_adjective(both(oadv)), [in,dier,voege]).
m('van dien aard',  sbar_adjective(pred(nonadv)),[van,dien,aard]).
m(zodanig,          sbar_adjective(no_e(adv)),  zodanig).
m(zodanig,          sbar_adjective(e),          zodanige).
m(zoveel,           sbar_adjective(no_e(odet_adv)),  zoveel).
m(zoveel,           sbar_adjective(e),          zovele).
m(zozeer,           sbar_adjective(both(adv)),  zozeer).

m(dergelijk,        sbar_adjective(e),          dergelijke).
m(dergelijk,        sbar_adjective(no_e(adv)),  dergelijk).

m('in die mate',    sbar_pred_adjective(adv),[in,die,mate]).
m('bij zoverre',    sbar_pred_adjective(adv),[bij,zoverre]).
m(inzoverre,        sbar_pred_adjective(adv),inzoverre).
m(inzoverre,        sbar_pred_adjective(adv),[in,zoverre]).
m('van dien aard',  sbar_pred_adjective(adv),[van,dien,aard]).
m(zo,               sbar_pred_adjective(adv),zo).
m(zolang,           sbar_pred_adjective(adv),zolang).
m(zover,            sbar_pred_adjective(adv),zover).
m(zover,            sbar_pred_adjective(adv),[zo,ver]).

m(zover,            vp_pred_adjective(nonadv),zover).
m(zover,            vp_pred_adjective(nonadv),[zo,ver]).

%% VL het is daarom dat ik kom
m(daarom,           adjective(no_e(nonadv),subject_sbar), daarom ).

nattr(both(_)).
nattr(no_e(_)).

%% REALLY ELLIPSIS
with_dt(ZozeerZelfs,
	sbar_adjective(Adv),
	dt(ap,[hd=l(ZozeerStem,sbar_pred_adjective(Adv),0,Len),
	       obcomp=orig(obcomp),
	       mod=l(zelfs,adverb,advp,Len,Len2)])) :-
    m(ZozeerStem,sbar_adjective(Adv),Zozeer),
    (   Zozeer = [H|T]
    ->  length([H|T],Len),
	lists:append(Zozeer,[zelfs],ZozeerZelfs)
    ;   Len=1,
	ZozeerZelfs = [Zozeer,zelfs]
    ),
    Len2 is Len+1.

with_dt(ZozeerZelfs,
	sbar_pred_adjective(Adv),
	dt(ap,[hd=l(ZozeerStem,sbar_pred_adjective(Adv),0,Len),
	       obcomp=orig(obcomp),
	       mod=l(zelfs,adverb,advp,Len,Len2)])) :-
    m(ZozeerStem,sbar_pred_adjective(Adv),Zozeer),
    (   Zozeer = [H|T]
    ->  length([H|T],Len),
	lists:append(Zozeer,[zelfs],ZozeerZelfs)
    ;   Len=1,
	ZozeerZelfs = [Zozeer,zelfs]
    ),
    Len2 is Len+1.

with_dt([blij,verrast],
	Tag,
	dt(ap,[mod=l(blij,adjective(no_e(padv)),ap,0,1),
	       vc=orig(vc),
               pc=orig(pc),
	       hd=l(verrast,Tag,1,2)])) :-
    verrast_tag(Tag).

verrast_tag(adjective(no_e(adv))).
verrast_tag(adjective(no_e(adv),object_sbar)).
verrast_tag(adjective(no_e(adv),er_pp_sbar(door))).
verrast_tag(adjective(no_e(adv),er_pp_sbar(over))).
verrast_tag(adjective(no_e(adv),pp(door))).
verrast_tag(adjective(no_e(adv),pp(met))).
verrast_tag(adjective(no_e(adv),pp(over))).

with_dt([nieuwe,stijl],
	postnp_adverb,
	dt(np,[mod=l(nieuw,adjective(e),ap,0,1),
	       hd=l(stijl,noun(de,count,sg),1,2)])).

with_dt([oude,stijl],
	postnp_adverb,
	dt(np,[mod=l(oud,adjective(e),ap,0,1),
	       hd=l(stijl,noun(de,count,sg),1,2)])).

with_dt([vrije,stijl],
	postnp_adverb,
	dt(np,[mod=l(vrij,adjective(e),ap,0,1),
	       hd=l(stijl,noun(de,count,sg),1,2)])).

with_dt([Eerste,Klas],
	postnp_adverb,
	dt(np,[det=l(Eerste,number(rang),detp,0,1),
	       hd=l(Klas,noun(de,count,sg),1,2)])) :-
    eerste_klas_eerste(Eerste),
    eerste_klas_klas(Klas).

eerste_klas_eerste(eerste).
eerste_klas_eerste(tweede).
eerste_klas_eerste(derde).
eerste_klas_eerste(vierde).

eerste_klas_eerste('1e').
eerste_klas_eerste('2e').
eerste_klas_eerste('3e').
eerste_klas_eerste('4e').

eerste_klas_klas(klas).
eerste_klas_klas(klasse).

m('eerste-klas',postnp_adverb,'eerste-klas').
m('tweede-klas',postnp_adverb,'tweede-klas').
m('derde-klas', postnp_adverb,'derde-klas' ).
m('vierde-klas',postnp_adverb,'vierde-klas').

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ADVERBS %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%

m(zo,               zo_mogelijk_zo,zo).
m(mogelijk,         zo_mogelijk_mogelijk(no_e),mogelijk).
m(mogelijk,         zo_mogelijk_mogelijk(e),mogelijke).

m(allerlei,         postn_adverb,allerlei). % Vlaams
m(allerhande,       postn_adverb,allerhande). 
m(apart,            postn_adverb,apart).  % verhaal, vak, paar, geval, klasse
m('avant la lettre',postn_adverb,[avant,la,lettre]).
m(brut,             postn_adverb,brut).   % Kaapse Pracht brut etc.
m(bruto,            postn_adverb,bruto).
m('cum suis',       postn_adverb,[cum,suis]).
m(indoor,           postn_adverb,indoor).
m(jongstleden,      postn_adverb,jongstleden).
m(junior,           postn_adverb,junior).
m(laatstleden,      postn_adverb,laatstleden).
m(light,            postn_adverb,light).  % cola light
m('om niet',        postn_adverb,[om,niet]).
m(netto,            postn_adverb,netto).
m('pur sang',       postn_adverb,[pur,sang]).
m(senior,           postn_adverb,senior).
m(tegelijk,         postn_adverb,tegelijk).
m(tegen,            postn_adverb,tegen).   % europarl: u hebt een spreker tegen gehoord
m('ten principale', postn_adverb,[ten,principale]). % de commissie ten principale?
m(terzake,          postn_adverb,terzake). % voorstellen terzake zijn welkom
m(zaliger,          postn_adverb,zaliger).

m(senior,           postn_adverb,senioren). % het WK atletiek junioren
m(junior,           postn_adverb,junioren).
m(dame,             postn_adverb,dames).
m(estafette,        postn_adverb,estafette).
m(vrouw,            postn_adverb,vrouwen).
m(heer,             postn_adverb,heren).
m(man,              postn_adverb,mannen).
m(militair,         postn_adverb,militairen).

%% wereldkampioenschap schaatsen lange baan
m(afstand,          postn_adverb,afstanden).
m(allround,         postn_adverb,allround).
m(allround,         postn_adverb,'all-round').
m(allround,         postn_adverb,[all,round]).
m(halfzwaargewicht, postn_adverb,halfzwaargewicht).
m(lichtgewicht,     postn_adverb,lichtgewicht).
m(outdoor,          postn_adverb,outdoor).
with_dt([lange,afstand],
        postn_adverb,
        dt(np,[mod=l(lang,adjective(e),ap,0,1),
               hd=l(afstand,noun(de,count,sg),1,2)])).
with_dt([lange,baan],
        postn_adverb,
        dt(np,[mod=l(lang,adjective(e),ap,0,1),
               hd=l(baan,noun(de,count,sg),1,2)])).
m(sprint,           postn_adverb,sprint).
m(weltergewicht,    postn_adverb,weltergewicht).
m(zwaargewicht,     postn_adverb,zwaargewicht).

with_dt([alleen,al],
	postp_adverb,
	dt(advp,[hd=l(alleen,adverb,advp,0,1),
		 mod=l(al,adverb,advp,1,2)
		])).

m(junior,           postpn_adverb,junior).
m(senior,           postpn_adverb,senior).

%% in principe althans
%% volgens Piet althans
m(al,               postp_adverb,al). %% In januari al, voor zijn geboorte al,
m(althans,          postp_adverb,althans).
m(bijvoorbeeld,     postp_adverb,bijvoorbeeld).
m(daarentegen,      postp_adverb,daarentegen).
m(derhalve,         postp_adverb,derhalve).
m(echter,           postp_adverb,echter).
m(dus,              postp_adverb,dus).
m(evenwel,          postp_adverb,evenwel).
m(immers,           postp_adverb,immers).
m(intussen,         postp_adverb,intussen).
m(nog,              postp_adverb,nog). % cdb/2535 in december nog
m(nu,               postp_adverb,nu).
m(ook,              postp_adverb,ook).
m(pas,              postp_adverb,pas). % in december pas
m(tenslotte,        postp_adverb,tenslotte).
m('ten slotte',     postp_adverb,[ten,slotte]).
m('wel te verstaan',postp_adverb,welteverstaan).
m('wel te verstaan',postp_adverb,[wel,te,verstaan]).
m(weliswaar,        postp_adverb,weliswaar).

%% these appear only in topic (?), or within comma's/brackets etc.
%% these take also PP and ADVP
%% they are modifiers since the category remains intact (?)
%% op Piet althans wacht ik nooit
%% TODO: pronouns like "niemand" can do this too
%% niemand echter/overigens die iets deed
m(althans,          postnp_adverb,althans).
m(bijvoorbeeld,     postnp_adverb,bijvoorbeeld).
m(daarentegen,      postnp_adverb,daarentegen).
m(derhalve,         postnp_adverb,derhalve).
m(dus,              postnp_adverb,dus).
m(echter,           postnp_adverb,echter).
m(evenwel,          postnp_adverb,evenwel).
m(immers,           postnp_adverb,immers).
m(intussen,         postnp_adverb,intussen).
m(namelijk,         postnp_adverb,namelijk).
m(nog,              postnp_adverb,nog).  % drie jaar nog
m(nu,               postnp_adverb,nu).
m(ondertussen,      postnp_adverb,ondertussen).
m(onderwijl,        postnp_adverb,onderwijl).
m(ook,              postnp_adverb,ook).
m(overigens,        postnp_adverb,overigens).
m(pas,              postnp_adverb,pas).  % twee weken pas
m(slechts,          postnp_adverb,slechts).
m(tenslotte,        postnp_adverb,tenslotte).
m('ten slotte',     postnp_adverb,[ten,slotte]).
m(toch,             postnp_adverb,toch).
m('tot en met',     postnp_adverb,[tot,en,met]).
m(trouwens,         postnp_adverb,[trouwens]).
m('wel te verstaan',postnp_adverb,[wel,te,verstaan]).
m(weliwaar,         postnp_adverb,weliswaar).

%% ?? voor een ogenblik maar, want ...
%% eentje maar...
m(maar,             postnp_adverb,maar).

m(alleen,           postnp_adverb,alleen).
m('an sich',        postnp_adverb,[an,sich]).
m('over en weer',   postnp_adverb,[over,en,weer]).
m(persoonlijk,      postnp_adverb,persoonlijk). % Ik persoonlijk
%m(samen,            postnp_adverb,samen).
m(zelf,             postnp_adverb,zelf).
m(zelve,            postnp_adverb,zelve).
m(haarzelf,         postnp_adverb,haarzelve).
m(hemzelf,          postnp_adverb,hemzelve).

m(opnieuw,          postnp_adverb,opnieuw).  % elke dag opnieuw
m(weer,             postnp_adverb,weer).     % elke dag weer

with_dt([alleen,al],
	postnp_adverb,
	dt(advp,[hd=l(alleen,adverb,advp,0,1),
		 mod=l(al,adverb,advp,1,2)
		])).

m(al,               postnp_adverb,al).	%% tweehonderd mensen al hebben ..
m(reeds,            postnp_adverb,reeds).
m(inmiddels,        postnp_adverb,inmiddels).

m(anderzijds,       postnp_adverb,anderzijds).
m(enerzijds,        postnp_adverb,enerzijds).
m(harerzijds,       postnp_adverb,harerzijds).
m(hunnerzijds,      postnp_adverb,hunnerzijds).
m(mijnerzijds,      postnp_adverb,mijnerzijds).
m(onzerzijds,       postnp_adverb,onzerzijds).
m(uwerzijds,        postnp_adverb,uwerzijds).
m(zijnerzijds,      postnp_adverb,zijnerzijds).

m(genoeg,           postnp_adverb,genoeg). % todo: + om-vp
m(temeer,           postnp_adverb,temeer).  % reden temeer
m('te veel',        postnp_adverb,[te,meer]).  % reden temeer
m('te over',        postnp_adverb,[te,over]). % todo: + om-vp
m('te over',        postnp_adverb,[ten,over]). % todo: + om-vp

m(ongeveer,         postnp_adverb,ongeveer).
m(zowat,            postnp_adverb,zowat).

m('k.k.',           postnp_adverb,'k.k.').

m('en suite',       postnp_adverb,[en,suite]).  % kamer e.d.
m('en suites',      postnp_adverb,[en,suites]). % kamers e.d.

m('ad interim',     postnp_adverb,[ad,interim]).
m('in spe',         postnp_adverb,[in,spe]).

m('at large',       postnp_adverb,[at,large]).
m('par excellence', postnp_adverb,[par,excellence]).
m('optima forma',   postnp_adverb,[optima,forma]).
m('tout court',     postnp_adverb,[tout,court]).

m(speciaal,         postnp_adverb,  speciaal).  % de babi pangang speciaal

m('extra vierge',   postnp_adverb,[extra,vierge]).

m('not out',        postnp_adverb,[not,out]).   % cricket?

m('dan ook',        post_wh_adverb,[dan,ook]).
m('ook maar',       post_wh_adverb,[ook,maar]).
m(ook,              post_wh_adverb,ook).

m(eender,           pre_wh_adverb,eender).
m(eenders,          pre_wh_adverb,eenders). % VL
m(gelijk,           pre_wh_adverb,gelijk).
m(ongeacht,         pre_wh_adverb,ongeacht).
m(onverschillig,    pre_wh_adverb,onverschillig).
m(willekeurig,      pre_wh_adverb,willekeurig).
m('om het even',    pre_wh_adverb,[om,Het,even]) :- het(Het).

%%%% TODO: AP/ADVP ADJ/ADV
%% adverbs left of XP:
%% noun comp prep adv verb
%% for np, sbar, pp, advp, om-vp
%%
%% perhaps this should be replaced with a system of more semantically
%% oriented features - but those features are hard to construct

%% modal_adverb without arg: combines with each of those

%% ook al in het zwart
%% ook al een ex-Byrd
%% ook al omdat ..
with_dt([Ook,Al],
	modal_adverb,
	dt(advp,[hd=l(Ook,adverb,0,1),
		 mod=l(Al,adverb,advp,1,2)])) :-
    modified_modal_hd_mod(Ook,Al).

with_dt([Ook,Al],
	modal_adverb(noun_prep),
	dt(advp,[hd=l(Ook,adverb,0,1),
		 mod=l(Al,adverb,advp,1,2)])) :-
    modified_modal_hd_mod_noun_prep(Ook,Al).

with_dt([Vooral,Ook,Weer],
	modal_adverb,
	dt(advp,[hd=l(Vooral,adverb,0,1),
		 mod=l(Ook,adverb,advp,1,2),
                 mod=l(Weer,adverb,advp,2,3)])) :-
    modified_modal_hd_mod_mod(Vooral,Ook,Weer).

with_dt([temeer,niet],
	modal_adverb(comp),
	dt(advp,[hd=l(temeer,adverb,0,1),
		 mod=l(niet,adverb,advp,1,2)])).

with_dt([te,meer,niet],
	modal_adverb(comp),
	dt(advp,[hd=l('te veel',adverb,0,2),
		 mod=l(niet,adverb,advp,2,3)])).

with_dt([Ook,Al],
	modal_adverb,
	dt(Cat,[mod=l(Ook,adverb,advp,0,1),
		 hd=l(Al,Pos,1,2)])) :-
    modified_modal_mod_hd(Ook,Al,Cat,Pos).

modified_modal_hd_mod(ook,al).
modified_modal_hd_mod(zo,ongeveer).
modified_modal_hd_mod(vooral,niet).
modified_modal_hd_mod(vooral,ook).

modified_modal_hd_mod_noun_prep(misschien,wel).

%% maybe have a rule modal_adv --> mod:modal_adv hd:modal_adv
modified_modal_mod_hd(bijna,uitsluitend,ap,adjective(no_e(adv))).
modified_modal_mod_hd(haast,uitsluitend,ap,adjective(no_e(adv))).
modified_modal_mod_hd(vrijwel,uitsluitend,ap,adjective(no_e(adv))).
modified_modal_mod_hd(alleen,maar,advp,adverb).

modified_modal_hd_mod_mod(vooral,ook,weer).

with_dt([alleen,al],
	modal_adverb,
	dt(advp,[hd=l(alleen,adverb,advp,0,1),
		 mod=l(al,adverb,advp,1,2)
		])).

m(Stem, modal_adverb, Surf) :-
    modal_adverb(Surf),
    stem_from_surf(Surf,Stem).

modal_adverb(al).
modal_adverb(alleen).  
modal_adverb([al,dan,niet]).
modal_adverb([al,of,niet]).
modal_adverb(allesbehalve).
modal_adverb(althans).
modal_adverb(anderzijds).
modal_adverb(bijvoorbeeld).
modal_adverb(eerst). % meaning "pas"
modal_adverb(enerzijds).
modal_adverb(enkel).
modal_adverb(eveneens).
modal_adverb(godbetert).
modal_adverb([god,betert]).
modal_adverb([god,betere,het]).
modal_adverb([god,betere,'\'t']).
modal_adverb(['God',betert]).
modal_adverb(['God',betere,het]).
modal_adverb(['God',betere,'\'t']).
modal_adverb(inzonderheid).
modal_adverb(juist).
modal_adverb(louter).
modal_adverb(nauwelijks).
modal_adverb(net).
modal_adverb(nog).
modal_adverb([nota,bene]).
modal_adverb(ook).
modal_adverb(pas).
modal_adverb(precies).
modal_adverb(reeds).
modal_adverb(royaal).
modal_adverb(ruim).
modal_adverb(slechts).
modal_adverb(speciaal).
modal_adverb(tevens).
modal_adverb(uitgerekend).
modal_adverb(uitsluitend).
modal_adverb(vooral).
modal_adverb(voornamelijk).
modal_adverb(weer).  % ??
modal_adverb(zeker).
modal_adverb(zelfs).
modal_adverb([ja,zelfs]).
modal_adverb(zowel). % Vlaams?

m('met name', modal_adverb,[met,name]).
m('onder ander', modal_adverb, [onder,andere]).
m('onder ander', modal_adverb, [onder,anderen]).
m('onder meer',  modal_adverb, [onder,meer]).
m(ondermeer,  modal_adverb, ondermeer).

%% prep   bijna in alle gevallen
%% noun   bijna iedereen
%% adv    bijna altijd
m(alvast,           modal_adverb(adv_noun_prep),alvast).  % Vl?
m(bijlange,         modal_adverb(adv_noun_prep),bijlange).  % Vl?
m(bijna,            modal_adverb(adv_noun_prep),bijna).
m(dik,              modal_adverb(adv_noun_prep),dik).
m(gemiddeld,        modal_adverb(adv_noun_prep),gemiddeld).
m(nagenoeg,         modal_adverb(adv_noun_prep),nagenoeg).
m(haast,            modal_adverb(adv_noun_prep),haast).
m(ongeveer,         modal_adverb(adv_noun_prep),ongeveer).

m(heel,             modal_adverb(adv_noun_prep),heel).
%% heel de wereld; heel Nederland
%% heel in de verte; heel in het algemeen/kort/bijzonder
%% heel ergens anders; heel dikwijls; heel af en toe; binnenkort

m(praktisch,        modal_adverb(adv_noun_prep),praktisch).
m(stevig,           modal_adverb(adv_noun_prep),stevig).
m(vrijwel,          modal_adverb(adv_noun_prep),vrijwel).
m(werkelijk,        modal_adverb(adv_noun_prep),werkelijk).
m('zeg maar',       modal_adverb(adv_noun_prep),[zeg,maar]).
m(zowat,            modal_adverb(adv_noun_prep),zowat).

%% zomaar ineens
%% zomaar iemand; zomaar een dag (has to be indefinite?)
m(zomaar,           modal_adverb(adv_noun_prep),zomaar).

%% nog niet de helft van ..
%% nog niet in de helft van de gevallen ..
with_dt([nog,niet],
        modal_adverb(noun_prep),
        dt(advp,[mod=l(nog,adverb,advp,0,1),
                 hd=l(niet,adverb,1,2)])
       ).

%% hier niet ver vandaan
%% 
with_dt([niet,ver],
        modal_adverb(prep),
        dt(advp,[mod=l(niet,adverb,advp,0,1),
                 hd=l(ver,modal_adverb(noun_prep),1,2)
		])).

%% nog altijd volgens X
with_dt([nog,altijd],
        modal_adverb(prep),
        dt(advp,[mod=l(nog,modal_adverb,advp,0,1),
                 hd=l(altijd,sentence_adverb,1,2)
		])).

with_dt([in,totaal],
	modal_adverb(noun_prep),
	dt(pp,[hd=l(in,preposition(in,[]),pp,0,1),
	       obj1=l(totaal,noun(het,count,sg),np,1,2)
	      ])).

with_dt([in,totaal],
	adverb,
	dt(pp,[hd=l(in,preposition(in,[]),pp,0,1),
	       obj1=l(totaal,noun(het,count,sg),np,1,2)
	      ])).

with_dt([in,ieder,geval],
	modal_adverb(noun_prep),
	dt(pp,[hd=l(in,preposition(in,[]),pp,0,1),
	       obj1=dt(np,[det=l(ieder,determiner(het,nwh,mod),detp,1,2),
			   hd=l(geval,noun(both,count,sg),np,2,3)])
	      ])).

m(amper,            modal_adverb(noun_prep),amper).
m(circa,            modal_adverb(noun_prep),circa).
m(geheel,           modal_adverb(noun_prep),geheel). % met geheel uw hart, geheel in de stijl van ..
m(grofweg,          modal_adverb(noun_prep),grofweg).
m(hooguit,          modal_adverb(noun_prep),hooguit).
m(hoogstens,        modal_adverb(noun_prep),hoogstens).
m(krap,             modal_adverb(noun_prep),krap).
m(liefst,           modal_adverb(noun_prep),liefst).
% maar de helft van de aanwezigen had een kaartje
% maar in twaalf procent van de gevallen ...
m(maar,             modal_adverb(noun_prep),maar).    % also adv? "maar ternauwernood"
m('maar liefst',    modal_adverb(noun_prep),[maar,liefst]).
m('maar liefst',    modal_adverb(noun_prep),maarliefst).
m(maximaal,         modal_adverb(noun_prep),maximaal).
m(minimaal,         modal_adverb(noun_prep),minimaal).
m(minstens,         modal_adverb(noun_prep),minstens).
m(misschien,        modal_adverb(noun_prep),misschien).
m('om en bij',      modal_adverb(noun_prep),[om,en,bij]). % VL
m('om en nabij',    modal_adverb(noun_prep),[om,en,bij]). % VL
m('om en bij de',   modal_adverb(noun_prep),[om,en,bij,de]). % VL
m('om en nabij de', modal_adverb(noun_prep),[om,en,bij,de]). % VL
m('om en rond',     modal_adverb(noun_prep),[om,en,rond]). % VL
m('om en rond de',  modal_adverb(noun_prep),[om,en,rond,de]). % VL
%m(ongeveer,         modal_adverb(noun_prep),ongeveer). % also adv now
m('±',              modal_adverb(noun_prep),'±').
m('+ / -',          modal_adverb(noun_prep),['+','/','-']).
m('pak hem beet',   modal_adverb(noun_prep),[pak,hem,beet]).
m('pak hem beet',   modal_adverb(noun_prep),[pak,'\'m',beet]).
m(pakweg,           modal_adverb(noun_prep),pakweg).
m(plusminus,        modal_adverb(noun_prep),plusminus).
				% ruim de helft van ...
m(ruwweg,           modal_adverb(noun_prep),ruwweg).
m('ten hoogste',    modal_adverb(noun_prep),[ten,hoogste]).
m(tenminste,        modal_adverb(noun_prep),tenminste).
m('ten minste',        modal_adverb(noun_prep),[ten,minste]).
m(uiterlijk,        modal_adverb(noun_prep),uiterlijk).
m(ver,              modal_adverb(noun_prep),ver).
m(verreweg,         modal_adverb(noun_prep),verreweg).  % with superlative
				% verreweg in de meeste gevallen
m(veruit,           modal_adverb(noun_prep),veruit).
m(wellicht,         modal_adverb(noun_prep),wellicht).
m(waarschijnlijk,   modal_adverb(noun_prep),waarschijnlijk).
m('zo\'n beetje',   modal_adverb(noun_prep),['zo\'n',beetje]).

m(kort,             modal_adverb(adv_comp_prep),kort).
m(lang,             modal_adverb(adv_comp_prep),lang).
%% helemaal als een verrassing kwam het niet
m(helemaal,         modal_adverb(adv_comp_prep), helemaal).

m(zo,               modal_adverb(adv_prep), zo).

m(alweer,           modal_adverb(noun),alweer).
m('een beetje',     modal_adverb(noun),[een,beetje]).
				% een beetje een aansteller is hij wel
				% hij profileert zich als een beetje een linkse projectontwikkelaar
m(echt,             modal_adverb(noun),echt). 
m('every inch',     modal_adverb(noun),[every,inch]).
m(gans,             modal_adverb(noun),gans). % gans het land
%m('naar schatting', modal_adverb(noun),[naar,schatting]). % bedragen
%m('nog eens',       modal_adverb(noun),[nog,eens]). % een dozijn etc
m('nog geen',       modal_adverb(noun),[nog,geen]). % nog geen grammetje
m('ook maar',       modal_adverb(noun),[ook,maar]).
m(opnieuw,          modal_adverb(noun),opnieuw). 
m(rond,             modal_adverb(noun),rond). % rond de 100 N's
m(samen,            modal_adverb(noun),samen).  % van samen 1700 euro
m(wederom,          modal_adverb(noun),wederom).
m(wijlen,           modal_adverb(noun),wijlen).
m('zo goed als',    modal_adverb(noun),[zo,goed,als]).
m('zo goed als',    modal_adverb(noun),[zogoed,als]).

m('zegge en schrijve', modal_adverb(noun_prep),[zegge,en,schrijve]).
m('zegge en schrijve', modal_adverb(noun_prep),[zegge,en,schrijven]).
m('zegge en schrijve', modal_adverb(noun_prep),[zeggen,en,schrijve]).
m('zegge en schrijve', modal_adverb(noun_prep),[zeggen,en,schrijven]).

%% even voor/na het einde; even voordat ..
m(even,             modal_adverb(comp_prep),even).
m(direct,           modal_adverb(comp_prep),direct).
m(meteen,           modal_adverb(comp_prep),meteen). % meteen na(dat)..
m(onmiddellijk,     modal_adverb(comp_prep),onmiddellijk).
m(terstond,         modal_adverb(comp_prep),terstond).
m(vlak,             modal_adverb(comp_prep),vlak). % vlak voor(dat)..

with_dt([nog,Eens],
	modal_adverb(noun),
	dt(advp,[mod=l(nog,adverb,advp,0,1),
		 hd=l(eens,adverb,advp,1,2)
		])) :-
    eens(Eens).

eens(eens).
eens('\'ns').

with_dt([naar,schatting],
	 modal_adverb(noun),
	 dt(pp,[hd=l(naar,preposition(naar,[]),pp,0,1),
		obj1=l(schatting,noun(de,count,sg),np,1,2)
	       ])).

with_dt([naar,schatting],
	 adverb,
	 dt(pp,[hd=l(naar,preposition(naar,[]),pp,0,1),
		obj1=l(schatting,noun(de,count,sg),np,1,2)
	       ])).

with_dt([Ook,Al],
	modal_adverb(comp_prep),
	dt(Cat,[mod=l(Ook,adverb,advp,0,1),
		 hd=l(Al,Pos,1,2)])) :-
    modified_modal_mod_hd_comp_prep(Ook,Al,Cat,Pos).

with_dt([Ook,Al],
	modal_adverb(prep),
	dt(Cat,[mod=l(Ook,adverb,advp,0,1),
		 hd=l(Al,Pos,1,2)])) :-
    modified_modal_mod_hd_prep(Ook,Al,Cat,Pos).

modified_modal_mod_hd_prep(bijzonder,dicht,ap,adjective(no_e(adv))).
modified_modal_mod_hd_prep(heel,dicht,ap,adjective(no_e(adv))).

modified_modal_mod_hd_comp_prep(vrijwel,direct,ap,adjective(no_e(adv))).
modified_modal_mod_hd_comp_prep(vrijwel,meteen,advp,adverb).
modified_modal_mod_hd_comp_prep(vrijwel,onmiddellijk,ap,adjective(no_e(adv))).
modified_modal_mod_hd_comp_prep(bijna,direct,ap,adjective(no_e(adv))).
modified_modal_mod_hd_comp_prep(bijna,meteen,advp,adverb).
modified_modal_mod_hd_comp_prep(bijna,onmiddellijk,ap,adjective(no_e(adv))).
modified_modal_mod_hd_comp_prep(haast,direct,ap,adjective(no_e(adv))).
modified_modal_mod_hd_comp_prep(haast,meteen,advp,adverb).
modified_modal_mod_hd_comp_prep(haast,onmiddellijk,ap,adjective(no_e(adv))).
modified_modal_mod_hd_comp_prep(niet,lang,ap,adjective(no_e(adv))).


m(altijd,           modal_adverb(comp), altijd).  % wanneer als 
m(steeds,           modal_adverb(comp), steeds).  % wanneer als indien
m(telkens,          modal_adverb(comp), telkens). % wanneer als indien
m(temeer,           modal_adverb(comp), temeer).  % temeer daar/omdat
m('te veel',        modal_adverb(comp), [te,meer]).  % temeer daar/omdat


%% todo: dichter bij elkaar [dan ...]!
%% nog dieper in blessuretijd
m(desnoods,         modal_adverb(prep),desnoods).
m(centraal,         modal_adverb(prep),centraal). % centraal in het midden; op het veld
m(dicht,            modal_adverb(prep),dicht).    % dicht tegen zich aan ; dicht bij de macht?
m(diep,             modal_adverb(prep),diep).     % diep in blessuretijd
m(dwars,            modal_adverb(prep),dwars).    % dwars door
m(elders,           modal_adverb(prep),elders).   % kerken elders in Nederland
m(hoog,             modal_adverb(prep),hoog).     % hoog boven de wolken
m(laag,             modal_adverb(prep),laag). % hoog boven de wolken
m(lijnrecht,        modal_adverb(prep),lijnrecht).  % lijnrecht tegenover ..
m(midden,           modal_adverb(prep),midden).   % midden in
m(pal,              modal_adverb(prep),pal).      % pal naast/achter/..
m(recht,            modal_adverb(prep),recht).    % recht tegenover ..
m(recht,            modal_adverb(prep),recht).    % lijnrecht tegenover ..
m(redelijk,         modal_adverb(prep),redelijk). % redelijk op tijd
m(schuin,           modal_adverb(prep),schuin).   % schuin naar voren
m(ster,             modal_adverb(prep),sterk).    % sterk in de minderheid
m(volkomen,         modal_adverb(prep),volkomen). % with pp (predicative?) % 'volkomen tegen mijn wil' / in de war
m(vroeg,            modal_adverb(prep),vroeg).    % vroeg in de middag
m(zwaar,            modal_adverb(prep),zwaar).    % zwaar onder druk staan
				

%% mede dankzij hem; mede omdat; mede om ..
m(deels,            modal_adverb(comp_prep_verb), deels).
m(mede,             modal_adverb(comp_prep_verb), mede).


m(hoe,              hoe_adv,hoe).
m('des te',         hoe_adv,[des,te]).
m('zo te',          hoe_adv,[zo,te]).   % Vlaams? Zo te verder je in het boek komt , zo te meer verhalen erbij komen .

%% adverb +Wh
m(hoe,              wh_adjective,hoe).

m(hoelaat,          wh_adjective,hoelaat).

%%m('des te',         wh_adjective,[des,te]).

m(hoelang,          wh_me_adjective,hoelang).

m(hoeveel,          wh_adjective(odet_adv),hoeveel).
% m(hoeveel,          wh_adjective(odet_adv),[de,hoeveel]).  % ? "om de hoeveel"?

m(hoeveel,          pronoun(ywh,thi,pl,de,both,indef), hoevelen).

m(hoeveelste,       wh_number(rang), hoeveelste).

%% met z'n hoevelen zijn jullie?
with_dt([met,Hun,hoevelen],
	wh_adjective(padv),
	dt(pp,[hd=l(met,preposition(met,[]),0,1),
	       obj1=dt(np,[det=l(Hun,determiner(pron),detp,1,2),
			   hd=l(hoeveel,noun(both,both,pl),2,3)])])) :-
    zijn(Hun).


m(hoever,            wh_adverb,hoever).
m(hoeverre,          wh_adverb,hoeverre).
m(hoezeer,           wh_adverb,hoezeer).
m(hoezo,             wh_adverb,hoezo).

m('hoe de fock',     wh_adverb,[hoe,de,fock]).
m('hoe de fuck',     wh_adverb,[hoe,de,fuck]).
m('hoe de fuk',      wh_adverb,[hoe,de,fuk]).
m('hoe de hell',     wh_adverb,[hoe,de,hell]).
m('hoe the fock',    wh_adverb,[hoe,the,fock]).
m('hoe the fuck',    wh_adverb,[hoe,the,fuck]).
m('hoe the fuk',     wh_adverb,[hoe,the,fuk]).
m('hoe the hell',    wh_adverb,[hoe,the,hell]).

m('hoe dat zo',      wh_adverb,[hoe,dat,zo]).

%%  ignore solely lexical ambiguity
%%  m(vanwaar,          wh_adverb,vanwaar).  % meaning: why?
%%  m(waarom,           wh_adverb,waarom).

with_dt([vanwaar,dan],
        wh_adverb,
        dt(advp,[hd=l(vanwaar,wh_adverb,0,1),
                 mod=l(dan,tmp_adverb,advp,1,2)
                ])).

with_dt([vanwaar,dan,toch],
        wh_adverb,
        dt(advp,[hd=l(vanwaar,wh_adverb,0,1),
                 mod=l(dan,tmp_adverb,advp,1,2),
                 mod=l(toch,adverb,advp,2,3)
                ])).

with_dt([vanwaar,dan,toch,altijd],
        wh_adverb,
        dt(advp,[hd=l(vanwaar,wh_adverb,0,1),
                 mod=l(dan,tmp_adverb,advp,1,2),
                 mod=l(toch,adverb,advp,2,3),
                 mod=l(altijd,sentence_adverb,advp,3,4)
                ])).

with_dt([vanwaar,toch],
        wh_adverb,
        dt(advp,[hd=l(vanwaar,wh_adverb,0,1),
                 mod=l(toch,adverb,advp,1,2)
                ])).

with_dt([vanwaar,toch,altijd],
        wh_adverb,
        dt(advp,[hd=l(vanwaar,wh_adverb,0,1),
                 mod=l(toch,adverb,advp,1,2),
                 mod=l(altijd,sentence_adverb,advp,2,3)
                ])).

with_dt([waarom,dan],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(dan,tmp_adverb,advp,1,2)
	      ])).

with_dt([waarom,dan,toch],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(dan,tmp_adverb,advp,1,2),
	       mod=l(toch,adverb,advp,2,3)
	      ])).

with_dt([waarom,eigenlijk,niet],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(eigenlijk,adjective(no_e(adv)),ap,1,2),
	       mod=l(niet,adverb,advp,2,3)
	      ])).

with_dt([waarom,dan,toch,altijd],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(dan,tmp_adverb,advp,1,2),
	       mod=l(toch,adverb,advp,2,3),
	       mod=l(altijd,sentence_adverb,advp,3,4)
	      ])).

with_dt([waarom,niet],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(niet,adverb,advp,1,2)
	      ])).

with_dt([waarom,toch],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(toch,adverb,advp,1,2)
	      ])).

with_dt([waarom,toch,altijd],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(toch,adverb,advp,1,2),
	       mod=l(altijd,sentence_adverb,advp,2,3)
	      ])).

with_dt([waarom,nu,weer],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(nu,tmp_adverb,advp,1,2),
	       mod=l(weer,adverb,advp,2,3)
	      ])).

with_dt([waarom,steeds,weer],
        waar_adverb(om),
        dt(pp,[hd=l(waarom,waar_adverb(om),0,1),
	       mod=l(steeds,adverb,advp,1,2),
	       mod=l(weer,adverb,advp,2,3)
	      ])).

m('wat nou',        wh_adverb,[wat,nou]).  % wat nou vormcrisis?

with_dt([ik,weet,niet,hoeveel],
	adjective(no_e(odet_adv)),
	dt(smain,[ hd=l(v_root(weet,weten),verb(hebben,sg,tr_sbar),1,2),
		   su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		   mod=l(niet,adverb,advp,2,3),
		   vc=l(hoeveel,wh_adjective(odet_adv),ap,3,4)])).

with_dt([ik,weet,niet,hoelang],
	adjective(no_e(tmpadv)),
	dt(smain,[ hd=l(v_root(weet,weten),verb(hebben,sg,tr_sbar),1,2),
		   su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		   mod=l(niet,adverb,advp,2,3),
		   vc=l(hoelang,wh_adjective(wh_me_adjective),ap,3,4)])).

with_dt([ik,weet,niet,waar],
	loc_adverb,
	dt(smain,[ hd=l(v_root(weet,weten),verb(hebben,sg,tr_sbar),1,2),
		   su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
		   mod=l(niet,adverb,advp,2,3),
		   vc=l(waar,wh_loc_adverb,advp,3,4)])).

with_dt([voor,hoelang],
	wh_adverb,
	dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
	       obj1=l(hoelang,wh_adverb,advp,1,2)])).

with_dt([tot,hoelaat],
	wh_adverb,
	dt(pp,[hd=l(tot,preposition(tot,[]),0,1),
	       obj1=l(hoelaat,wh_adjective,advp,1,2)])).

with_dt([tot,hoever],
	wh_adverb,
	dt(pp,[hd=l(tot,preposition(tot,[]),0,1),
	       obj1=l(hoever,wh_adverb,advp,1,2)])).

with_dt([voor,hoever],
	wh_adverb,
	dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
	       obj1=l(hoever,wh_adverb,advp,1,2)])).

with_dt([in,hoeverre],
	wh_adverb,
	dt(pp,[hd=l(in,preposition(in,[]),0,1),
	       obj1=l(hoeverre,wh_adverb,advp,1,2)])).

with_dt([in,hoever],
	wh_adverb,
	dt(pp,[hd=l(in,preposition(in,[]),0,1),
	       obj1=l(hoever,wh_adverb,advp,1,2)])).

%%    wanneer / waar --> wh_tmp_adverb / wh_loc_adverb 

%% adverb +R and +Wh
m(Stem, waar_adverb(Prep), Surf ):-
    waar_adverb(Surf,Prep),
    stem_from_surf(Surf,Stem).

m(Stem,waar_adverb(Prep), Words) :-
    collocational_preposition(Prep),
    replace_last_adverbial(Prep,Words,waar),
    stem_from_surf(Words,Stem).

m(Stem, er_adverb(Prep), Surf ):-
    er_adverb(Surf,Prep),
    stem_from_surf(Surf,Stem).

m(Stem,er_adverb(Prep),Words) :-
    collocational_preposition(Prep),
    (	replace_last_adverbial(Prep,Words,daar)
    ;   replace_last_adverbial(Prep,Words,er)
    ;   replace_last_adverbial(Prep,Words,hier)
    ),
    stem_from_surf(Words,Stem).

%% todo: ten koste van wie/wat

replace_last_adverbial(L0,L,Wh) :-
    lists:append(Ws,[Prep],L0),
    lists:append(Ws,[Adv],L),
    replace_adverbial(Prep,Adv,Wh).

replace_adverbial(Prep0,Adv,Wh) :-
    replace_prep(Prep0,Prep),
    atom_concat(Wh,Prep,Adv).

replace_prep(Prep0,Prep) :-
    (	Prep0 == tot
    ->	Prep = toe
    ;	Prep0 == met
    ->	Prep = mee
    ;	Prep0 = Prep
    ).

:- discontiguous
    er_adverb/2,
    waar_adverb/2.

%% specials:
er_adverb(vandaaraf,vanaf).
er_adverb([van,daaraf],vanaf).
er_adverb(vandaaruit,vanuit).
er_adverb([van,daaruit],vanuit).

er_adverb(vanhieraf,vanaf).
er_adverb([van,hieraf],vanaf).
er_adverb(vanhieruit,vanuit).
er_adverb([van,hieruit],vanuit).

waar_adverb(vanwaaraf,vanaf).
waar_adverb([van,waaraf],vanaf).
waar_adverb(vanwaaruit,vanuit).
waar_adverb([van,waaruit],vanuit).

waar_adverb(vanwaar,van).

er_adverb(derwaarts,naar).
er_adverb(daarheen,naar).
er_adverb(erheen,naar).
er_adverb(hierheen,naar).
waar_adverb(waarheen,naar).
waar_adverb(werwaarts,naar).

er_adverb(langsdaar,langs).
er_adverb(langshier,langs).
waar_adverb(langswaar,langs).

er_adverb(daarmede,met).
er_adverb(daarmee,met).
er_adverb(ermee,met).
er_adverb(ermede,met).  % weg ermede!
er_adverb(hiermede,met).
er_adverb(hiermee,met).
waar_adverb(waarmede,met).
waar_adverb(waarmee,met).

er_adverb(daartoe,tot).
er_adverb(ertoe,tot).
er_adverb(hiertoe,tot).
waar_adverb(waartoe,tot).

er_adverb(daarvandaan,van).
er_adverb([daar,vandaan],van).
er_adverb(ervandaan,van).
er_adverb([er,vandaan],van).
er_adverb(hiervandaan,van).
er_adverb([hier,vandaan],van).
waar_adverb(waarvandaan,van).
waar_adverb([waar,vandaan],van).

er_adverb(ervandoor,vandoor).
er_adverb(ervanonder,vanonder).
er_adverb(eronderuit,onderuit).

er_adverb(eraf,van).

er_adverb([daarvoor,in,de,plaats],voor).
er_adverb([ervoor,in,de,plaats],voor).
er_adverb([hiervoor,in,de,plaats],voor).
waar_adverb([waarvoor,in,de,plaats],voor).

er_adverb([van,dien],none).
er_adverb(vandien,none).
er_adverb([overal,vandaan],van).

er(er).
er(daar).
er(hier).

er_adverb(Form,Prep) :-
    er(Er),
    er_adverb_prep(Prep),
    atom_concat(Er,Prep,Form).

er_adverb([Er,Prep,Post],Prep) :-
    er(Er),
    er_adverb_prep(Prep,Post).
er_adverb([ErPrep,Post],Prep) :-
    er(Er),
    er_adverb_prep(Prep,Post),
    atom_concat(Er,Prep,ErPrep).
er_adverb([Er,PrepPost],Prep) :-
    er(Er),
    er_adverb_prep(Prep,Post),
    atom_concat(Prep,Post,PrepPost).
er_adverb(ErPrepPost,Prep) :-
    er(Er),
    er_adverb_prep(Prep,Post),
    atom_concat(Prep,Post,PrepPost),
    atom_concat(Er,PrepPost,ErPrepPost).

er_adverb([Er,Prep,Post],PrepForm) :-
    er(Er),
    er_adverb_prep(Prep,Post,PrepForm).
er_adverb([ErPrep,Post],PrepForm) :-
    er(Er),
    er_adverb_prep(Prep,Post,PrepForm),
    atom_concat(Er,Prep,ErPrep).
er_adverb([Er,PrepPost],PrepForm) :-
    er(Er),
    er_adverb_prep(Prep,Post,PrepForm),
    atom_concat(Prep,Post,PrepPost).
er_adverb(ErPrepPost,PrepForm) :-
    er(Er),
    er_adverb_prep(Prep,Post,PrepForm),
    atom_concat(Prep,Post,PrepPost),
    atom_concat(Er,PrepPost,ErPrepPost).


er_adverb_prep(aan).
er_adverb_prep(achter).
er_adverb_prep(bij).
er_adverb_prep(binnen).
er_adverb_prep(boven).
er_adverb_prep(buiten).
er_adverb_prep(door).
er_adverb_prep(in).
er_adverb_prep(langs).
er_adverb_prep(na).
er_adverb_prep(naar).
er_adverb_prep(naast).
er_adverb_prep(om).
er_adverb_prep(omtrent).
er_adverb_prep(onder).
er_adverb_prep(op).
er_adverb_prep(over).
er_adverb_prep(rond). % VL
er_adverb_prep(tegen).
er_adverb_prep(tussen).
er_adverb_prep(uit).
er_adverb_prep(van).
er_adverb_prep(vanaf).
er_adverb_prep(vanuit).
er_adverb_prep(voor).
er_adverb_prep(zonder).

er_adverb_prep(achter,aan).
er_adverb_prep(boven,uit).
er_adverb_prep(door,heen).
er_adverb_prep(naar,toe).
er_adverb_prep(om,heen).
er_adverb_prep(over,heen).
er_adverb_prep(tegen,aan).
er_adverb_prep(tegen,in).
er_adverb_prep(tegen,op).

er_adverb_prep(boven,op,bovenop).
er_adverb_prep(midden,op,middenop).
er_adverb_prep(om,heen,omheen).
er_adverb_prep(op,uit,uit_op).
er_adverb_prep(tegen,in,tegenin).
er_adverb_prep(tegen,over,tegenover).
er_adverb_prep(tussen,door,tussendoor).
er_adverb_prep(tussen,in,tussenin).
er_adverb_prep(tussen,uit,tussenuit).

waar_adverb([ten,wiens,Noun],te) :-
    noun3(Noun).

waar_adverb(Form,Prep) :-
    er_adverb_prep(Prep),
    atom_concat(waar,Prep,Form).

waar_adverb([waar,Prep,Post],Prep) :-
    er_adverb_prep(Prep,Post).
waar_adverb([ErPrep,Post],Prep) :-
    er_adverb_prep(Prep,Post),
    atom_concat(waar,Prep,ErPrep).
waar_adverb([waar,PrepPost],Prep) :-
    er_adverb_prep(Prep,Post),
    atom_concat(Prep,Post,PrepPost).
waar_adverb(ErPrepPost,Prep) :-
    er_adverb_prep(Prep,Post),
    atom_concat(Prep,Post,PrepPost),
    atom_concat(waar,PrepPost,ErPrepPost).

waar_adverb([waar,Prep,Post],PrepForm) :-
    er_adverb_prep(Prep,Post,PrepForm).
waar_adverb([ErPrep,Post],PrepForm) :-
    er_adverb_prep(Prep,Post,PrepForm),
    atom_concat(waar,Prep,ErPrep).
waar_adverb([waar,PrepPost],PrepForm) :-
    er_adverb_prep(Prep,Post,PrepForm),
    atom_concat(Prep,Post,PrepPost).
waar_adverb(ErPrepPost,PrepForm) :-
    er_adverb_prep(Prep,Post,PrepForm),
    atom_concat(Prep,Post,PrepPost),
    atom_concat(waar,PrepPost,ErPrepPost).

with_dt([Hieraan,vooraf],
	er_adverb(aan),
	dt(pp,[hd=l(Hieraan,er_adverb(aan),0,1),
	       hdf=l(vooraf,particle(vooraf),part,1,2),
	       obj1=orig(obj1)])) :-
    er_adverb(Hieraan,aan).

with_dt([Hieraan,vooraf],
	waar_adverb(aan),
	dt(pp,[hd=l(Hieraan,waar_adverb(aan),0,1),
	       hdf=l(vooraf,particle(vooraf),part,1,2),
	       obj1=orig(obj1)])) :-
    waar_adverb(Hieraan,aan).

m(Stem, sentence_adverb, Sub) :-
    sentence_adverb(Sub),
    stem_from_surf(Sub,Stem).

:- discontiguous
    sentence_adverb/1.

sentence_adverb([aan,Een,stuk,door]) :- een1(Een).
sentence_adverb([ad,absurdum]).
sentence_adverb([ad,infinitum]).
sentence_adverb([af,en,toe]).
sentence_adverb([af,een,toe]).
sentence_adverb([af,te,toe]).  %VL
sentence_adverb([af,ten,toe]).  %VL
sentence_adverb([af,toe]).
sentence_adverb(aldoor).
sentence_adverb([alles,bijeen]). 
sentence_adverb([alles,bijelkaar]). 
sentence_adverb([alles,bij,elkaar]).
sentence_adverb([alles,bijmekaar]). 
sentence_adverb([alles,bij,mekaar]).
sentence_adverb([alles,met,elkaar]).
sentence_adverb([alles,met,mekaar]).
sentence_adverb(altijd).
sentence_adverb(bijna). 
sentence_adverb(bijvoorbeeld). 
sentence_adverb(bovendien).
sentence_adverb([cum,grano,salis]).
sentence_adverb(derhalve). 
sentence_adverb(ditmaal). 
sentence_adverb(doorgaans). 
sentence_adverb(dus). 
sentence_adverb(echter).
sentence_adverb(efkens).
sentence_adverb(eventjes).
sentence_adverb(evenveel). % iedereen moet evenveel slapen
sentence_adverb(evenwel).
sentence_adverb([excusez,le,mot]).
sentence_adverb(godverdomme).
sentence_adverb(godzijdank).
sentence_adverb(hedentendage). 
sentence_adverb([heden,ten,dage]). 
sentence_adverb(helaas).
sentence_adverb(hetzelfde).  % reageren, zich gedragen, zich opstellen
sentence_adverb([honderd,uit]).
sentence_adverb(immers).
sentence_adverb([in,allerijl]). 
sentence_adverb([in,aller,ijl]).
sentence_adverb([in,arren,moede]).
sentence_adverb([in,arrenmoede]).
sentence_adverb([in,casu]).
sentence_adverb([in,der,minne]).
sentence_adverb([in,dier,voege]).
sentence_adverb([in,extremis]).
sentence_adverb([in,godsnaam]). 
sentence_adverb([in,'Godsnaam']). 
sentence_adverb([in,hemelsnaam]).
sentence_adverb([in,'\'s',hemelsnaam]).
sentence_adverb([in,het,eerst]). % ouderwets?
sentence_adverb([in,lang]).  % ik heb hem in lang niet gezien
sentence_adverb([in,the,end]).
sentence_adverb([in,vredesnaam]).
sentence_adverb(inderdaad). 
sentence_adverb(intussen). 
sentence_adverb(kortom). 
sentence_adverb(laatst).
sentence_adverb(langzamerhand).
sentence_adverb([liefst,van,al]).
sentence_adverb([meerderheid,tegen,minderheid]).
sentence_adverb(meestal). 
sentence_adverb(merkwaardigerwijs). 
sentence_adverb(merkwaardigerwijze). 
sentence_adverb(misschien).  
sentence_adverb(mogelijkerwijs).
sentence_adverb(mogelijkerwijze).
sentence_adverb([mutatis,mutandis]).
sentence_adverb(naderhand). 
sentence_adverb(namelijk). 
sentence_adverb(niettemin). 
sentence_adverb(nochtans). 
sentence_adverb(noodzakelijkerwijs). 
sentence_adverb(noodzakelijkerwijze).
sentence_adverb([om,mijnentwille]).
sentence_adverb(omtrent).	% ouderwets
sentence_adverb(onderhand). 
sentence_adverb(ondertussen). 
sentence_adverb(onderwijl). 
sentence_adverb(ongetwijfeld). 
sentence_adverb(ongeveer).
sentence_adverb('±').
sentence_adverb(ook). 
sentence_adverb([op,de,duur]).
sentence_adverb([op,de,koop,toe]).
sentence_adverb([op,den,duur]).
sentence_adverb([op,tijd,en,stond]).  % VL soms
sentence_adverb(opeens).
sentence_adverb(opzij).
sentence_adverb([out,of,court]).
sentence_adverb([out,of,the,blue]).
sentence_adverb(overigens).
sentence_adverb(potverdomme).
sentence_adverb(potverdorie).
sentence_adverb(redelijkerwijs). 
sentence_adverb(redelijkerwijze). 
sentence_adverb(['\'s',anderdaags]).
sentence_adverb(['\'s',anderendaags]). 
sentence_adverb(['\'s',ander,daags]).
sentence_adverb(['\'s',anderen,daags]). 
sentence_adverb(anderdaags).  
sentence_adverb(anderendaags).
sentence_adverb([des,ander,daags]).
sentence_adverb([des,anderen,daags]).
sentence_adverb(sedert). 
sentence_adverb(sedertdien).
sentence_adverb(sluiks).
sentence_adverb(soms). % +extr
sentence_adverb([te,gelegenertijd]).
sentence_adverb(tezijnertijd).
sentence_adverb([te,zijner,tijd]). 
sentence_adverb([te,zijnder,tijd]). 
sentence_adverb([ter,zijner,tijd]). 
sentence_adverb([ter,zijnder,tijd]). 
sentence_adverb([tegen,heug,en,meug]). 
sentence_adverb(telkenjare). 
sentence_adverb(telkenmale). 
sentence_adverb(telkens). 
sentence_adverb([te,lange,leste]). 
sentence_adverb([te,langen,leste]).
sentence_adverb([te,onpas]).
sentence_adverb([te,pas]).
sentence_adverb([ten,diepste]).
sentence_adverb([ten,lange,leste]). 
sentence_adverb([ten,langen,leste]). 
sentence_adverb(tenminste). 
sentence_adverb([ten,minste]). 
sentence_adverb(tenslotte).
sentence_adverb([ten,slotte]). 
sentence_adverb(toendertijd).
sentence_adverb([tot,dusver]). 
sentence_adverb([tot,dusverre]). 
sentence_adverb([tot,overmaat,van,ramp]). 
sentence_adverb([tot,vervelends,toe]).  % NB: tot vervelens toe in tst_head
sentence_adverb([tot,gekmakens,toe]).
sentence_adverb([tot,gekwordens,toe]).
sentence_adverb([tot,ziekmakens,toe]).
sentence_adverb([tot,ziekwordens,toe]).
sentence_adverb([tot,vervelends,aan,toe]). % NB: tot vervelens toe in tst_head
sentence_adverb([tot,gekmakens,aan,toe]).
sentence_adverb([tot,gekwordens,aan,toe]).
sentence_adverb([tot,ziekmakens,aan,toe]).
sentence_adverb([tot,ziekwordens,aan,toe]).
sentence_adverb(totnogtoe).  % vlaams?
sentence_adverb(totnutoe).   % 300x in Twnc...
sentence_adverb(trouwens).
sentence_adverb([uit,arren,moede]).
sentence_adverb([uit,arrenmoede]).
sentence_adverb(uiteraard).
sentence_adverb([van,langs,om]).  % VL
sentence_adverb([van,meetaf,aan]).
sentence_adverb([van,tijd,tot,tijd]).
sentence_adverb([van,lieverlee]).
sentence_adverb([van,lieverlede]).
sentence_adverb(veelal). 
sentence_adverb(verdomme).
sentence_adverb(verdorie).
sentence_adverb([volgens,zeggen]). 
sentence_adverb(voortaan). 
sentence_adverb(voorwaar). 
sentence_adverb(voorzeker). 
sentence_adverb(vort).  % zuidelijk
sentence_adverb(warempel).
sentence_adverb(weliswaar). 
sentence_adverb(wellicht). 
sentence_adverb([zo,af,en,toe]). 
sentence_adverb([zo,een,twee,drie]). 
sentence_adverb([zo,één,twee,drie]). 
sentence_adverb([zo,nu,en,dan]). 
sentence_adverb([zo,te,horen]). 
sentence_adverb([zo,te,lezen]). 
sentence_adverb([zo,te,ruiken]). 
sentence_adverb([zo,te,voelen]). 
sentence_adverb([zo,te,zien]). 
sentence_adverb(zodoende).
sentence_adverb(zopas).
sentence_adverb(zostraks). 
sentence_adverb(zowaar).
sentence_adverb([zo,waar]).
sentence_adverb(überhaupt). 

sentence_adverb([jaar,na,jaar]).
sentence_adverb([jaar,op,jaar]).

sentence_adverb([maand,na,maand]).

sentence_adverb([uur,na,uur]).

%% grep '^4|\([a-z]*\) in \1 uit|' twnc.ngram
sentence_adverb('avond-in-avond-uit').
sentence_adverb([avond,in,avond,uit]).
sentence_adverb([avond,in,',',avond,uit]).
sentence_adverb(['avond-in','avond-uit']).
sentence_adverb(['avond-in',',','avond-uit']).
sentence_adverb('dag-in-dag-uit').
sentence_adverb([dag,in,dag,uit]).
sentence_adverb([dag,in,',',dag,uit]).
sentence_adverb(['dag-in','dag-uit']).
sentence_adverb(['dag-in',',','dag-uit']).
sentence_adverb('jaar-in-jaar-uit').
sentence_adverb([jaar,in,jaar,uit]).
sentence_adverb([jaar,in,',',jaar,uit]).
sentence_adverb(['jaar-in','jaar-uit']).
sentence_adverb(['jaar-in',',','jaar-uit']).
sentence_adverb('maand-in-maand-uit').
sentence_adverb([maand,in,maand,uit]).
sentence_adverb([maand,in,',',maand,uit]).
sentence_adverb(['maand-in','maand-uit']).
sentence_adverb(['maand-in',',','maand-uit']).
sentence_adverb('week-in-week-uit').
sentence_adverb([week,in,week,uit]).
sentence_adverb([week,in,',',week,uit]).
sentence_adverb(['week-in','week-uit']).
sentence_adverb(['week-in',',','week-uit']).

%% loc-adverb
%% can be ld complement of ld-verb
%% can be complement of ld-preposition (tot,van,naar)

m(Stem, loc_adverb, Surf) :-
    loc_adverb(Surf),
    stem_from_surf(Surf,Stem).

%% pp?
%% can be used postn  "de mensen boven"
loc_adverb(achter).
loc_adverb(achteraan).
loc_adverb(achteraf).
loc_adverb(achterin).
loc_adverb(achterom).
loc_adverb(achterop).
loc_adverb(achterover).
loc_adverb(achteruit).
loc_adverb(aldaar).
loc_adverb(alhier).
loc_adverb(backstage). loc_adverb('back-stage'). loc_adverb([back,stage]).
loc_adverb(beneden).
loc_adverb(bezijden).
loc_adverb(binnen).
loc_adverb(binnenboord).
loc_adverb(binnendoor).
loc_adverb(binnenin).
loc_adverb(binnenuit).
loc_adverb(boven).
loc_adverb(bovenaan).
loc_adverb(bovenaf).
loc_adverb(bovenin). 
loc_adverb(bovenop). 
loc_adverb(bovenuit).
loc_adverb(breed).     % hij speelde breed; een balletje breed
loc_adverb(buitenboord).
loc_adverb(buiten).
loc_adverb(buitenaf).
loc_adverb(buitenom).
loc_adverb(buitengaats).
loc_adverb(daarginds).
loc_adverb(daarginder).
loc_adverb([down,under]).
loc_adverb(ginder).
loc_adverb(halfweg).
loc_adverb(halverwege).
loc_adverb(linksboven).
loc_adverb(linksonder).
loc_adverb(midden).
loc_adverb(middenin).
loc_adverb(middenop).
loc_adverb(midscheeps).
loc_adverb(onder).
loc_adverb(onderaan).
loc_adverb(onderaf).
loc_adverb(onderin).
loc_adverb(onderop).
loc_adverb(ondergronds).
loc_adverb(opzij).
loc_adverb([op,zij]).
loc_adverb(rechtdoor).
loc_adverb(rechtuit).
loc_adverb(rechtsboven).
loc_adverb(rechtsonder).
loc_adverb(retour).
loc_adverb([te,bestemder,plaatse]).
loc_adverb([te,bestemder,plekke]).
loc_adverb(tegenover).
loc_adverb([ten,achteren]).
loc_adverb([ter,perse]).
loc_adverb([ter,plaatse]).  % not pp: 'ter plaatse waar ...'; cf also postnp_adverb
loc_adverb(terplaatse).
loc_adverb(thuis).
loc_adverb([te,huis]).
loc_adverb(tussenbeide).
loc_adverb(tussendoor).
loc_adverb(tussenin).    % 
loc_adverb(terug).
loc_adverb(vooraan).
loc_adverb(voorin).
loc_adverb(voorop).
loc_adverb(voorover).
loc_adverb(vooruit).

%% arg of p
loc_adverb(achteren).  % only pp-compl
loc_adverb(onderen).   % only pp compl
loc_adverb(voren).     % only pp compl

loc_adverb(alom).

%% dir_adverb?
loc_adverb(aaneen).
loc_adverb(dooreen).
loc_adverb([heen,en,weer]).
loc_adverb('heen-en-weer').
loc_adverb([op,en,neer]).
loc_adverb('op-en-neer').
loc_adverb(opeen).
loc_adverb([over,en,weer]).
loc_adverb('over-en-weer').
loc_adverb(uiteen).
loc_adverb(vaneen).

loc_adverb([her,en,der]).
loc_adverb(langszij).

loc_adverb(mee).   % hij ging toen mee de boerderij in
                   % hij zou toen mee naar huis gaan
                   % hij zou toen mee op de motor gaan (?)
loc_adverb(scheep).

loc_adverb(overboord).  % er is een man overboord

loc_adverb([haars,weegs]).
loc_adverb([huns,weegs]).
loc_adverb([jouws,weegs]).
loc_adverb([mijns,weegs]).
loc_adverb([ons,weegs]).
loc_adverb([uws,weegs]).
loc_adverb([zijns,weegs]).

% twitter
loc_adverb(osso).

m(alwaar, rwh_loc_adverb,   alwaar ).

m(ergens, post_loc_adv_adv, ergens ).

m(Stem, dir_adverb, Surf) :-
    dir_adverb(Surf),
    stem_from_surf(Surf,Stem).

m(ver,dir_adverb,verder).

dir_adverb(achterdoor).
dir_adverb(achterlangs).
dir_adverb(achterover).
dir_adverb(achteruit).
dir_adverb(bergaf).
dir_adverb(bergop).
dir_adverb(binnendoor).
dir_adverb(buitenom).
dir_adverb([heen,en,weer]).
dir_adverb('heen-en-weer').
dir_adverb(herwaarts).
dir_adverb(heuvelaf).
dir_adverb(heuvelop).
dir_adverb([kopje,onder]).
dir_adverb(linksaf).
dir_adverb(linksom).
dir_adverb(omhoog).
dir_adverb(ophoog).  %VL
dir_adverb(omlaag).
dir_adverb(omver).
dir_adverb(onderuit).
dir_adverb([op,en,af]).
dir_adverb(opzij).
dir_adverb([op,zij]).
dir_adverb([over,en,weer]).
dir_adverb('over-en-weer').
dir_adverb(overboord).
dir_adverb(overeind).
dir_adverb(overkop).  % VLAAMS
dir_adverb(rechtdoor).
dir_adverb(rechtsaf).
dir_adverb(rechtsom).
dir_adverb(rechtsomkeert).
dir_adverb(rechtuit).
dir_adverb(retour).
dir_adverb(terneer).
dir_adverb(terug).
dir_adverb(tevoorschijn).
dir_adverb(vaneen).
dir_adverb(voorlangs).
dir_adverb(voorover).
dir_adverb(vooruit).

with_dt([heen,en,terug],
        dir_adverb,
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(heen,particle(heen),advp,0,1),
                 cnj=l(terug,dir_adverb,advp,2,3)])
       ).

with_dt([voor,altijd],
	pp(voor),
	dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
	       obj1=l(altijd,tmp_adverb,advp,1,2)
	      ])).

with_dt([voor,eeuwig],
	pp(voor),
	dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
	       obj1=l(eeuwig,adjective(no_e(adv)),ap,1,2)
	      ])).

with_dt([voor,onderweg],
	pp(voor),
	dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
	       obj1=l(onderweg,adjective(pred(adv)),ap,1,2)
	      ])).

with_dt([de,klok,rond],
	sentence_adverb,
	dt(pp,[hd=l(rond,preposition(rond,[heen]),2,3),
	       obj1=dt(np,[hd=l(klok,noun(de,count,sg),1,2),
			   det=l(de,determiner(de),detp,0,1)])])).

with_dt([dezer,dagen],
	tmp_adverb,
	dt(advp,[hd=l(dag,tmp_noun(de,count,pl),1,2),
		det=l(deze,determiner(der),dp,0,1)])).

m(Stem,tmp_adverb,Surf) :-
    tmp_adverb(Surf),
    stem_from_surf(Surf,Stem).

%m(Stem,tmp_adverb,Surf) :-
%    tmp_adverb(Surf,Stem).

%m(Stem,wk_tmp_adverb,Surf) :-
%    wk_tmp_adverb(Surf,Stem).
% 's es 'ns
% --> lex.pl as parse_only_variant

tmp_adverb([vandaag,aan,de,dag]). % http://www.onzetaal.nl/taaladvies/advies/vandaag-de-dag-vandaag-aan-de-dag
tmp_adverb([vandaag,de,dag]).

tmp_adverb(aanstonds).
tmp_adverb(binnenkort).
tmp_adverb(daags).
tmp_adverb(daarjuist).
tmp_adverb(daarna).
tmp_adverb(daarnet).
tmp_adverb(daarstraks).
tmp_adverb(dan).
tmp_adverb([dan,en,slechts,dan]).
tmp_adverb(dato).
tmp_adverb(destijds).
tmp_adverb(dinsdags).
tmp_adverb(donderdags).
tmp_adverb(eens).
tmp_adverb(eergisteren).
tmp_adverb(eergisteravond).
tmp_adverb(eergistermiddag).
tmp_adverb(eergisternamiddag).
tmp_adverb(eergistermorgen).
tmp_adverb(eergisternacht).
tmp_adverb(eergisterochtend).
tmp_adverb(eergisterenochtend).
tmp_adverb(eergisterenavond).
tmp_adverb(eergisterenmiddag).
tmp_adverb(eergisterennamiddag).
tmp_adverb(eergisterenmorgen).
tmp_adverb(eergisterennacht).
tmp_adverb(eertijds).
tmp_adverb(gisteravond).
tmp_adverb(gistermiddag).
tmp_adverb(gisternamiddag).
tmp_adverb(gistermorgen).
tmp_adverb(gisternacht).
tmp_adverb(gisterochtend).
tmp_adverb(gisteren).
tmp_adverb(gisterenavond).
tmp_adverb(gisterenmiddag).
tmp_adverb(gisterennamiddag).
tmp_adverb(gisterenmorgen).
tmp_adverb(gisterennacht).
tmp_adverb(gisterenochtend).
tmp_adverb(hedenavond).
tmp_adverb(indertijd). 
tmp_adverb(kortgeleden).	% tot kortgeleden
tmp_adverb(maandags).
tmp_adverb(morgen).
tmp_adverb(morgenavond).
tmp_adverb(morgenmiddag).
tmp_adverb(morgennamiddag).
tmp_adverb(morgenmorgen).
tmp_adverb(morgennacht).
tmp_adverb(morgenochtend).
tmp_adverb(morgenvroeg).
tmp_adverb(net).
tmp_adverb(nooit).
tmp_adverb([never,nooit]).
tmp_adverb([nooit,en,te,nimmer]).
tmp_adverb([nooit,of,te,nimmer]).
tmp_adverb([nooit,ofte,nimmer]).
tmp_adverb(nu).
tmp_adverb(onlangs).
tmp_adverb(ooit).
tmp_adverb([op,Het,allerlaatst]) :- het(Het).
tmp_adverb([op,Het,laatst]) :- het(Het).
tmp_adverb([op,Het,lest]) :- het(Het).
tmp_adverb(overdag). 
tmp_adverb(overmorgen).
tmp_adverb(seffens).
tmp_adverb(strakjes).
tmp_adverb(straks).
tmp_adverb(subiet).
tmp_adverb([te,eniger,tijd]).
tmp_adverb(toen).
tmp_adverb(toenmaals).
tmp_adverb(toentertijd).
tmp_adverb(tussendoor).
tmp_adverb(vanavond).
tmp_adverb(vandaag).
tmp_adverb(vanmiddag).
tmp_adverb(vanmorgen).
tmp_adverb(vannacht).
tmp_adverb(vanochtend).
tmp_adverb('v.j.').  % vorig jaar; beursberichten
tmp_adverb(vooraf).
tmp_adverb(voorheen).
tmp_adverb(vrijdags).
tmp_adverb(weleer).
tmp_adverb(woensdags).
tmp_adverb(zaterdags).
tmp_adverb(zoëven). 
tmp_adverb(zojuist).
tmp_adverb(zolang).
tmp_adverb(zondags).
tmp_adverb(zometeen).
tmp_adverb([zo,meteen]). 
tmp_adverb(zonet).
tmp_adverb(zopas).

tmp_adverb(['\'s',morgens]).
tmp_adverb(['\'s',middags]).
tmp_adverb(['\'s',namiddags]).
tmp_adverb(['\'s',avonds]).
tmp_adverb(['\'s',nachts]).
tmp_adverb(['\'s',ochtends]).
tmp_adverb(['\'s',zondags]).
tmp_adverb(['\'s',maandags]).
tmp_adverb(['\'s',woensdags]).
tmp_adverb(['\'s',zaterdags]).
tmp_adverb(['\'s',winters]).
tmp_adverb(['\'s',zomers]).

tmp_adverb([des,morgens]).
tmp_adverb([des,middags]).
tmp_adverb([des,namiddags]).
tmp_adverb([des,avonds]).
tmp_adverb([des,nachts]).
tmp_adverb([des,ochtends]).
tmp_adverb([des,zondags]).
tmp_adverb([des,maandags]).
tmp_adverb([des,woensdags]).
tmp_adverb([des,zaterdags]).
tmp_adverb([des,winters]).
tmp_adverb([des,zomers]).

tmp_adverb(['\'s',zondagavonds]).
tmp_adverb(['\'s',zondagsavonds]).
tmp_adverb(['\'s',maandagavonds]).
tmp_adverb(['\'s',maandagsavonds]).
tmp_adverb(['\'s',woensdagavonds]).
tmp_adverb(['\'s',woensdagsavonds]).
tmp_adverb(['\'s',zaterdagavonds]).
tmp_adverb(['\'s',zaterdagsavonds]).

tmp_adverb(['\'s',zondagnachts]).
tmp_adverb(['\'s',zondagsnachts]).
tmp_adverb(['\'s',maandagnachts]).
tmp_adverb(['\'s',maandagsnachts]).
tmp_adverb(['\'s',woensdagnachts]).
tmp_adverb(['\'s',woensdagsnachts]).
tmp_adverb(['\'s',zaterdagnachts]).
tmp_adverb(['\'s',zaterdagsnachts]).

tmp_adverb(['\'s',zondagochtends]).
tmp_adverb(['\'s',zondagsochtends]).
tmp_adverb(['\'s',maandagochtends]).
tmp_adverb(['\'s',maandagsochtends]).
tmp_adverb(['\'s',woensdagochtends]).
tmp_adverb(['\'s',woensdagsochtends]).
tmp_adverb(['\'s',zaterdagochtends]).
tmp_adverb(['\'s',zaterdagsochtends]).

tmp_adverb(['\'s',zondagmorgens]).
tmp_adverb(['\'s',zondagsmorgens]).
tmp_adverb(['\'s',maandagmorgens]).
tmp_adverb(['\'s',maandagsmorgens]).
tmp_adverb(['\'s',woensdagmorgens]).
tmp_adverb(['\'s',woensdagsmorgens]).
tmp_adverb(['\'s',zaterdagmorgens]).
tmp_adverb(['\'s',zaterdagsmorgens]).

tmp_adverb(['\'s',zondagmiddags]).
tmp_adverb(['\'s',zondagsmiddags]).
tmp_adverb(['\'s',maandagmiddags]).
tmp_adverb(['\'s',maandagsmiddags]).
tmp_adverb(['\'s',woensdagmiddags]).
tmp_adverb(['\'s',woensdagsmiddags]).
tmp_adverb(['\'s',zaterdagmiddags]).
tmp_adverb(['\'s',zaterdagsmiddags]).

with_dt([Later,Vandaag],
	tmp_adverb,
	dt(ap,[hd=l(Later,adjective(er(tmpadv)),ap,0,1),
	       mod=l(Vandaag,tmp_adverb,advp,1,2)
	      ])) :-
    later(Later),
    vandaag(Vandaag).

later(later).
later(eerder).

vandaag(gisteren).
vandaag(gisteravond).
vandaag(gistermiddag).
vandaag(gistermorgen).
vandaag(gisternacht).
vandaag(gisterochtend).

vandaag(gisterenavond).
vandaag(gisterenmiddag).
vandaag(gisterenmorgen).
vandaag(gisterennacht).
vandaag(gisterenochtend).

vandaag(vandaag).
vandaag(vanmiddag).
vandaag(vanavond).
vandaag(vanmorgen).
vandaag(vannacht).
vandaag(vanochtend).

%% a real wh-adverb
%% a real tmp-adverb
%% hij vroeg sinds wanneer de bar open was
m(wanneer,          wh_tmp_adverb, wanneer).

m(donders,          intensifier,donders).
m(fucking,          intensifier,fucking).
m(heel,             intensifier,heel).
m(heel,             intensifier(e),hele).     % TODO: selects for -e
m(hoogst,           intensifier,hoogst).
m('des te',         intensifier,[des,te]).    % TODO: selects for -er(e)
m(kanker,           intensifier,kanker).
m(kaulo,            intensifier,kaulo).
m(kaulo,            intensifier,koulo).
m(mega,             intensifier,mega).
m(mpp,              intensifier,mpp). % mapangpang (volgens Harm het geslachtsdeel van je moeder)
m('o zo',           intensifier,[o,zo]).
m('o zo',           intensifier,[oh,zo]).
m(omin,             intensifier,omin).
m(oming,            intensifier,oming).
m(te,               intensifier,te).
m('veel te',        intensifier,veelste).
m('verre van',      intensifier,[verre,van]).
m(verrekte,         intensifier,verrekte).

m(te,               vp_om_intensifier,te).

m(te,               me_intensifier,te).
m(zo,               me_intensifier,zo).  % drie keer zo groot

with_dt([eens,zo],
        intensifier,
        dt(advp,[hd=l(zo,intensifier,1,2),
                 me=l(eens,adverb,advp,0,1)
                ])).

m(zo,               als_me_intensifier,zo).  % drie keer zo hard als ik

m(te,               vp_om_me_intensifier,te).

with_dt([al,te],
	intensifier,
	dt(advp,
	   [mod=l(al,adverb,advp,0,1),
	    hd=l(te,intensifier,1,2)])).

with_dt([maar,al,te],
	intensifier,
	dt(advp,
	   [mod=l(maar,adverb,advp,0,1),
	    mod=l(al,adverb,advp,1,2),
	    hd=l(te,intensifier,2,3)])).

m(onvoldoende,      vp_om_adverb,onvoldoende). % hij was voldoende aardig om..
m(voldoende,        vp_om_adverb,voldoende).   % hij was voldoende aardig om..
m(genoeg,           vp_om_adverb,genoeg).

m(zo,               vp_adverb,zo).
				% hij was zo aardig (om) weg te gaan

m(aldus,            dip_sbar_adverb,aldus).
m(zo,               dip_sbar_adverb,zo).  % we gaan , zo constateert hij , ..
                                           % .., aldus sprak zara..
                                           % zo saturates sbar slash, if
                                           % there is a DIP

m(althans,          postadv_adverb,althans).
m(bijvoorbeeld,     postadv_adverb,bijvoorbeeld).
m(daarentegen,      postadv_adverb,daarentegen).
m(derhalve,         postadv_adverb,derhalve).
m(echter,           postadv_adverb,echter).
m(dus,              postadv_adverb,dus).
m(eerst,            postadv_adverb,eerst).   % dan eerst kan Pakistan een normaal land worden
m(evenwel,          postadv_adverb,evenwel).
m(immers,           postadv_adverb,immers).
m(intussen,         postadv_adverb,intussen).
m(nog,              postadv_adverb,nog). % cdb/2535 in december nog
m(nu,               postadv_adverb,nu).
m(pas,              postadv_adverb,pas). % in december pas
m(tenslotte,        postadv_adverb,tenslotte).
m('ten slotte',     postadv_adverb,[ten,slotte]).
m(toch,             postadv_adverb,toch).
m(trouwens,         postadv_adverb,trouwens).
m('wel te verstaan',postadv_adverb,welteverstaan).
m('wel te verstaan',postadv_adverb,[wel,te,verstaan]).
m(weliswaar,        postadv_adverb,weliswaar).


m(al,               postadv_adverb,al).      % meteen al
m(alweer,           postadv_adverb,alweer).  % twee jaar geleden alweer
m(ineens,           postadv_adverb,ineens).  % toen ineens
m(laat,             postadv_adverb,laat).    % gisteravond laat
m(maar,             postadv_adverb,maar).    % eventjes maar
m(veel,             postadv_adverb,meer).    % niet meer ; nooit meer
m(ook,              postadv_adverb,ook).
m(ongeveer,         postadv_adverb,ongeveer).
m(opeens,           postadv_adverb,opeens).  % dan opeens; nu opeens
m(precies,          postadv_adverb,precies).
m(reeds,            postadv_adverb,reeds).
m(vroeg,            postadv_adverb,vroeg).   % morgenochtend vroeg
m(weer,             postadv_adverb,weer).    % ook nu weer ; dan weer
m(zowat,            postadv_adverb,zowat).   % een uur geleden zowat kwam hij voorbij

m(genoeg,           postadj_adverb,genoeg).  % gek genoeg; wonderlijk genoeg
m(zat,              postadj_adverb,zat).     % makkelijk zat

m(genoeg,           om_postadj_adverb,genoeg). % gek genoeg om

m(althans,          postadj_adverb,althans).
m(bijvoorbeeld,     postadj_adverb,bijvoorbeeld).
m(daarentegen,      postadj_adverb,daarentegen).
m('dan ook',        postadj_adverb,[dan,ook]).
m(derhalve,         postadj_adverb,derhalve).
m(dus,              postadj_adverb,dus).        % logisch dus , dat
m(echter,           postadj_adverb,echter).
m(eigenlijk,        postadj_adverb,eigenlijk).  % vreemd eigenlijk , dat
m(evenwel,          postadj_adverb,evenwel).
m(immers,           postadj_adverb,immers).
m(intussen,         postadj_adverb,intussen).
m(nog,              postadj_adverb,nog). % wat bleekjes nog ...
m(nu,               postadj_adverb,nu).
m(ook,              postadj_adverb,ook).
m(tenslotte,        postadj_adverb,tenslotte).
m('ten slotte',     postadj_adverb,[ten,slotte]).
m(toch,             postadj_adverb,toch).
m('wel te verstaan',postadj_adverb,welteverstaan).
m('wel te verstaan',postadj_adverb,[wel,te,verstaan]).
m(weliswaar,        postadj_adverb,weliswaar).


%% jammer genoeg is adverbial, but jammer is not..

with_dt([jammer,alleen],
	adjective(no_e(nonadv),subject_sbar),
	dt(ap,[hd=l(jammer,adjective(no_e(nonadv),subject_sbar),0,1),
	       mod=l(alleen,adverb,advp,1,2)])).

with_dt([jammer,genoeg],
	adverb,
	dt(advp,[hd=l(jammer,adjective(no_e(nonadv)),0,1),
		 mod=l(genoeg,adverb,advp,1,2)])).

with_dt([niet,alleen],
	modal_adverb,
	dt(advp,[hd=l(alleen,adverb,advp,1,2),
		 mod=l(niet,adverb,advp,0,1)
		])).

%% for
%% niet alleen Piet woont daar maar ook zijn zus
with_dt([niet,alleen],
	adverb,
	dt(advp,[mod=l(alleen,adverb,advp,1,2),
		 hd=l(niet,adverb,advp,0,1)
		])).

with_dt([M,H],adverb,dt(advp,[mod=l(M,Tag,Cat,0,1),
			      hd=l(H,adverb,1,2)])) :-
    mod_hd_adverb(M,H,Tag,Cat).

with_dt([M,H],adverb,dt(advp,[mod=l(L,Tag,Cat,0,1),
			      hd=l(H,adverb,1,2)])) :-
    mod_hd_adverb(M,L,H,Tag,Cat).

with_dt([wellicht,ten,overvloede],  
        adverb,dt(advp,[mod=l(wellicht,sentence_adverb,advp,0,1),
                        hd=l('ten overvloede',adverb,1,3)])).

with_dt([H,M], adverb,dt(advp,[mod=l(M1,adverb,advp,1,2),
			       hd=l(H1,adverb,0,1)])) :-
    hd_mod_adverb(H,M,H1,M1).

with_dt([H,M], adverb,dt(advp,[mod=l(M1,Pos,Cat,1,2),
			       hd=l(H1,adverb,0,1)])) :-
    hd_mod_adverb(H,M,H1,M1,Pos,Cat).

with_dt([MOD,niet,altijd],
	adverb,
	dt(advp,[mod=dt(advp,[mod=l(MOD,Tag,Cat,0,1),
			      hd=l(niet,adverb,1,2)
			     ]),
		 hd=l(altijd,adverb,2,3)])) :-
    mod_hd_adverb(MOD,niet,Tag,Cat).

with_dt([MOD,niet,overal],
	adverb,
	dt(advp,[mod=dt(advp,[mod=l(MOD,Tag,Cat,0,1),
			      hd=l(niet,adverb,1,2)
			     ]),
		 hd=l(overal,loc_adverb,2,3)])) :-
    mod_hd_adverb(MOD,niet,Tag,Cat).

mod_hd_adverb(nogal,    dikwijls).
mod_hd_adverb(nogal,    vaak).

mod_hd_adverb(altijd,   weer).

mod_hd_adverb(steeds,   weer).
mod_hd_adverb(steeds,   opnieuw).

mod_hd_adverb(telkens,  weer).
mod_hd_adverb(telkens,  opnieuw).

mod_hd_adverb(niet,     meteen).
mod_hd_adverb(niet,     vaak).
mod_hd_adverb(niet,     zelden).
mod_hd_adverb(niet,     zozeer).  % CGN_R5/r2nl_03/fn0137__96.xml
mod_hd_adverb(niet,     dikwijls).

mod_hd_adverb(nooit,    voorheen).

mod_hd_adverb(allicht,  daarom).
mod_hd_adverb(misschien,daarom).
mod_hd_adverb(wellicht, daarom).

% mod_hd_adverb(helemaal, nooit).

mod_hd_adverb(zeer,     binnenkort).
mod_hd_adverb(zeer,     onlangs).
mod_hd_adverb(zeer,     vaak).
mod_hd_adverb(zeer,     wel).
mod_hd_adverb(zeer,     dikwijls).

mod_hd_adverb(te,       dikwijls).

%% in het geheel niets 
%%                     '(helemaal) niets te maken hebben met'
% ???

% in het geheel niet
mod_hd_adverb(hoegenaamd,     niet).
mod_hd_adverb(heus,      niet).
mod_hd_adverb(toch,      niet).
mod_hd_adverb(vast,      niet).
mod_hd_adverb(wellicht,  niet).

mod_hd_adverb(niet,     altijd).
mod_hd_adverb(niet,     graag).
mod_hd_adverb(niet,     helemaal).
mod_hd_adverb(niet,     overal).

mod_hd_adverb(Absoluut,       Niet, adverb,                     advp) :-
    mod_hd_adverb(Absoluut,   Niet).

mod_hd_adverb(natuurlijk,     niet, adjective(no_e(sentadv)),   ap).
mod_hd_adverb(absoluut,       niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(bepaald,        niet, adjective(ge_no_e(adv)),    ap).
mod_hd_adverb(beslist,        niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(echt,           niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(lang,           niet, adjective(no_e(tmpadv)),    ap).
mod_hd_adverb(mooi,           niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(volstrekt,      niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(waarachtig,     niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(waarschijnlijk, niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(werkelijk,      niet, adjective(no_e(adv)),       ap).
mod_hd_adverb(zeker,          niet, adjective(no_e(adv)),       ap).

mod_hd_adverb(liever,lief,    niet, adjective(er(adv)),         ap).


%% hd < mod
% hd_mod_adverb(alleen,   maar, alleen, maar).  % can be built with rules
% hd_mod_adverb(even,     maar, even,   maar).  % ,,

hd_mod_adverb(niet,     eens, niet,   eens).
hd_mod_adverb(misschien,zelfs, misschien,zelfs).

%% wie is hoofd hier?
hd_mod_adverb(dan,      ineens, dan, ineens).

hd_mod_adverb(eens,     temeer, eens, temeer).

% hd_mod_adverb(werkelijk,overal,werkelijk,overal). % wrong

hd_mod_adverb(niet,     langer,    niet,lang,adjective(er(tmpadv)),          ap).
hd_mod_adverb(niet,     bepaald,   niet,bepaald,adjective(ge_no_e(adv)),        ap).
hd_mod_adverb(niet,     echt,      niet,echt,adjective(no_e(adv)),           ap).
hd_mod_adverb(wel,      degelijk,  wel,degelijk,adjective(no_e(adv)),           ap).

with_dt([eens,te,meer],
	adverb,
	dt(advp,[mod=l('te veel',adverb,advp,1,3),
		 hd=l(eens,adverb,0,1)])).

%%% predm_adverbs

with_dt([Haast,Allemaal],
	predm_adverb,
	dt(advp,[hd=l(Allemaal,adverb,1,2),
		 mod=l(Haast,predm_adverb,advp,0,1)])) :-
    haast_allemaal(Haast,Allemaal).

haast_allemaal(bijna,allemaal).
haast_allemaal(haast,allemaal).
haast_allemaal(niet,allemaal).
haast_allemaal(vrijwel,allemaal).
haast_allemaal(zowat,allemaal).

haast_allemaal(ook,zelf).

with_dt([lang,niet,allemaal],
	predm_adverb,
	dt(np,[mod=dt(advp,[mod=l(lang,adverb,advp,0,1),
			    hd=l(niet,adverb,1,2)]),
	       hd=l(allemaal,pronoun(nwh,thi,pl,de,both,indef),np,2,3)
	      ])).

with_dt([lang,niet,altijd],
	sentence_adverb,
	dt(advp,[mod=dt(advp,[mod=l(lang,adverb,advp,0,1),
			      hd=l(niet,adverb,1,2)]),
		 hd=l(altijd,sentence_adverb,advp,2,3)
		])).

m(Stem,predm_adverb,Surf) :-
    predm_adverb(Surf),
    stem_from_surf(Surf,Stem).

m(al,               predm_adverb,  alle).
m(al,               predm_adverb,  allen).
m(beide,            predm_adverb,  beiden).

m('geen van alle',  predm_adverb,  [geen,van,alle]).
m('geen van allen', predm_adverb,  [geen,van,allen]).
m('geen van beide', predm_adverb,  [geen,van,beide]).
m('geen van beide', predm_adverb,  [geen,van,beiden]).

% floating quantifiers and similar:
predm_adverb(allebei).
predm_adverb(alledrie).
predm_adverb(allemaal).
predm_adverb(alletwee).
predm_adverb(allevier).
predm_adverb(allevijf).
predm_adverb(beide).
predm_adverb([Een,voor,Een2]) :- een1(Een), een1(Een2).
predm_adverb([Een,na,Een2]) :- een1(Een), een1(Een2).  % VL
predm_adverb(elk).
predm_adverb([elk,voor,zich]).
predm_adverb([en,bloc]).
predm_adverb([en,masse]).
predm_adverb(gedrieën).
predm_adverb([geen,van,drieën]).
predm_adverb([geen,van,tweeën]).
predm_adverb([geen,van,vieren]).
predm_adverb(gedrieën).
predm_adverb(getweeën).
predm_adverb(gevieren).
predm_adverb(gevijven).
predm_adverb(gezessen).
predm_adverb(ieder).
predm_adverb([ieder,voor,zich]).
predm_adverb([qualitate,qua]).
predm_adverb([twee,aan,twee]).
predm_adverb([zij,aan,zij]).
predm_adverb(zelf).

%% NP zelf;
%% ons allen
%% dat allemaal
%% een laadvermogen van twee ton elk

% jullie               allebei
% zij                  beiden
% ons                  allemaal
% hun
% hen

with_dt([Wij,WORD],
        TagH,
	dt(np,[hd=l(Wij,TagH,0,1),
	       mod=l(PREDM,Tag,Cat,1,2)])) :-
    wij_allebei_wij(Wij,TagH),
    wij_allebei(WORD,PREDM,Tag,Cat).

wij_allebei_wij(wij,pronoun(nwh,fir,pl,de,nom,def)).
wij_allebei_wij(gij,pronoun(nwh,u,sg,de,nom,def)).
wij_allebei_wij(jullie,pronoun(nwh,je,pl,de,both,def)).
wij_allebei_wij(ons,pronoun(nwh,fir,pl,de,dat_acc,def)).
wij_allebei_wij(hun,pronoun(nwh,thi,pl,de,dat_acc,def)).
wij_allebei_wij(hen,pronoun(nwh,thi,pl,de,dat_acc,def)).
wij_allebei_wij(zij,pronoun(nwh,thi,both,de,nom,def)).
wij_allebei_wij(u,pronoun(nwh,u,sg,de,both,def)).

wij_allebei(beiden,   beide,     predm_adverb,       advp).
wij_allebei(allen,    al,        predm_adverb,       advp).
wij_allebei(allemaal, allemaal,  predm_adverb,       advp).
wij_allebei(allebei,  allebei,   predm_adverb,       advp).
wij_allebei(beidjes,  beide_DIM, noun(het,count,pl), np).

%% VL
wij_allebei(twee,     twee,      noun(de,count,sg),  np).
%% 
wij_allebei(tweeën,   twee,      noun(de,count,pl),  np).
wij_allebei(drieën,   drie,      noun(de,count,pl),  np).
wij_allebei(tweeen,   twee,      noun(de,count,pl),  np).
wij_allebei(drieen,   drie,      noun(de,count,pl),  np).
wij_allebei(vieren,   vier,      noun(de,count,pl),  np).
wij_allebei(vijven,   vijf,      noun(de,count,pl),  np).
wij_allebei(zessen,   zes,       noun(de,count,pl),  np).
wij_allebei(zevenen,  zeven,     noun(de,count,pl),  np).
wij_allebei(achten,   acht,      noun(de,count,pl),  np).
wij_allebei(negenen,  negen,     noun(de,count,pl),  np).
wij_allebei(tienen,   tien,      noun(de,count,pl),  np).
wij_allebei(tweetjes, twee_DIM,  noun(het,count,pl), np).
wij_allebei(drietjes, drie_DIM,  noun(het,count,pl), np).
wij_allebei(viertjes, vier_DIM,  noun(het,count,pl), np).
wij_allebei(vijfjes,  vijf_DIM,  noun(het,count,pl), np).
wij_allebei(zesjes,   zes_DIM,   noun(het,count,pl), np).

wij_allebei(elven,       elf,        noun(de,count,pl),  np).
wij_allebei(twaalven,    twaalf,     noun(de,count,pl),  np).
wij_allebei(dertienen,   dertien,    noun(de,count,pl),  np).
wij_allebei(viertienen,  veertien,   noun(de,count,pl),  np).
wij_allebei(vijftienen,  vijftien,   noun(de,count,pl),  np).
wij_allebei(zestienen,   zestien,    noun(de,count,pl),  np).
wij_allebei(zeventienen, zeventien,  noun(de,count,pl),  np).
wij_allebei(achttienen,  achttien,   noun(de,count,pl),  np).
wij_allebei(negentienen, negentien,  noun(de,count,pl),  np).
wij_allebei(twintigen,   twintig,    noun(de,count,pl),  np).


m(eenmaal,eenmaal_adverb,eenmaal).
m(eens,   eenmaal_adverb,eens).  % Vlaams

% ze gingen alle zes naar huis
% todo: geen van tweeen/drieen
m(al,               num_predm_adverb, alle).

%% vandaar SBAR dp/dp
m(alleen,             vandaar_adverb,alleen).
m(allereerst,         vandaar_adverb,allereerst).
m(allicht,            vandaar_adverb,allicht).
m(bijvoorbeeld,       vandaar_adverb,bijvoorbeeld).
m(daarmee,            vandaar_adverb,daarmee). % Vlaams
m(daarom,             vandaar_adverb,daarom).  % Vlaams
m('gelukkig maar',    vandaar_adverb,[gelukkig,maar]).
m(goddank,            vandaar_adverb,goddank).
m(helaas,             vandaar_adverb,helaas).
m(hopelijk,           vandaar_adverb,hopelijk).
m(misschien,          vandaar_adverb,misschien).
m(mogelijkerwijs,     vandaar_adverb,mogelijkerwijs).
m(mogelijkerwijze,    vandaar_adverb,mogelijkerwijze).
m(natuurlijk,         vandaar_adverb,natuurlijk).
m(niet,               vandaar_adverb,niet).
m(top,                vandaar_adverb,top).
m(vandaar,            vandaar_adverb,vandaar).
m(wellicht,           vandaar_adverb,wellicht).
m(zodoende,           vandaar_adverb,zodoende).
m('en dan te bedenken',
                      vandaar_adverb,[en,dan,te,bedenken]).

m(zo,                 zo_van_adverb,zo).

with_dt([vandaar,Ook],
	vandaar_adverb,
	dt(advp,[hd=l(vandaar,vandaar_adverb,0,1),
		 mod=l(Ook,adverb,advp,1,2)])) :-
    vandaar_ook(Ook).

with_dt([vandaar,dan,ook],
	vandaar_adverb,
	dt(advp,[hd=l(vandaar,vandaar_adverb,0,1),
		 mod=l(dan,adverb,advp,1,2),
                 mod=l(ook,adverb,advp,2,3)])).

with_dt([vandaar,Ook],
	adverb,
	dt(advp,[hd=l(vandaar,adverb,0,1),
		 mod=l(Ook,adverb,advp,1,2)])) :-
    vandaar_ook(Ook).

with_dt([misschien,daarom],
	vandaar_adverb,
	dt(advp,[hd=l(daarom,vandaar_adverb,1,2),
		 mod=l(misschien,sentence_adverb,advp,0,1)])).

with_dt([wellicht,daarom],
	vandaar_adverb,
	dt(advp,[hd=l(daarom,vandaar_adverb,1,2),
		 mod=l(wellicht,sentence_adverb,advp,0,1)])).

with_dt([allicht,daarom],
	vandaar_adverb,
	dt(advp,[hd=l(daarom,vandaar_adverb,1,2),
		 mod=l(allicht,adverb,advp,0,1)])).

%% Vlaams
with_dt([daarom,ook],
	vandaar_adverb,
	dt(advp,[hd=l(daarom,vandaar_adverb,0,1),
		 mod=l(ook,adverb,advp,1,2)])).

with_dt([daarom,ook],
	adverb,
	dt(advp,[hd=l(daarom,adverb,0,1),
		 mod=l(ook,adverb,advp,1,2)])).

vandaar_ook(allicht).  % VL
vandaar_ook(dus).
vandaar_ook(misschien).
vandaar_ook(natuurlijk).
vandaar_ook(ook).
vandaar_ook(waarschijnlijk).
vandaar_ook(wellicht).

%%%%%%%%%%%%%%%%%%
%%%% PRONOUNS %%%%
%%%%%%%%%%%%%%%%%%

m(allebei,      pronoun(nwh,thi,pl,de,both,indef),allebei).
m(alletwee,     pronoun(nwh,thi,pl,de,both,indef),alletwee).
m(alledrie,     pronoun(nwh,thi,pl,de,both,indef),alledrie).
m(allevier,     pronoun(nwh,thi,pl,de,both,indef),allevier).
m(allevijf,     pronoun(nwh,thi,pl,de,both,indef),allevijf).
m(allemaal,     pronoun(nwh,thi,pl,de,both,indef),allemaal).
m(al,           pronoun(nwh,thi,pl,de,both,indef),allen).
m(beide,        pronoun(nwh,thi,pl,de,both,indef),beide).
m(beide,        pronoun(nwh,thi,pl,de,both,indef),beiden).
m(beide,        pronoun(nwh,thi,pl,de,both,indef),[die,beiden]).
m(datgeen,      pronoun(nwh,thi,sg,het,both,def,strpro),datgeen).
m(datgeen,      pronoun(nwh,thi,sg,het,both,def,strpro),datgene).
m(degeen,       pronoun(nwh,thi,sg,de,both,def,strpro),degeen).
m(degeen,       pronoun(nwh,thi,sg,de,both,def,strpro),degene).
m(degeen,       pronoun(nwh,thi,pl,de,both,def,strpro),degenen).
m(deze,         pronoun(nwh,thi,both,both,gen,def),dezer).
m(deze,         pronoun(nwh,thi,both,both,gen,def),dezes).
m(deze,         pronoun(nwh,thi,pl,de,both,def,strpro),dezen).
m(diegene,      pronoun(nwh,thi,sg,de,both,def,strpro),diegene).
m(diegene,      pronoun(nwh,thi,pl,de,both,def,strpro),diegenen).
m(één,          pronoun(nwh,thi,sg,de,both,indef,strpro),een).  %% both => de
m(één,          pronoun(nwh,thi,sg,de,both,indef,strpro),één).  %% both => de
m(eenieder,     pronoun(nwh,thi,sg,de,both,def,strpro),[Een,ieder]) :- een(Een).
m(eenieder,     pronoun(nwh,thi,sg,de,both,def,strpro),eenieder).
m(één,          pronoun(nwh,thi,sg,de,both,indef,strpro),eentje).
m(elkaar,       pronoun(nwh,thi,pl,de,dat_acc,def),elkaar).
m(elkander,     pronoun(nwh,thi,pl,de,dat_acc,def),elkander).
m(elkeen,       pronoun(nwh,thi,sg,de,both,def,strpro),elkeen).
m(ge,           pronoun(nwh,u,sg,de,both,def),ge).
m(gene,         pronoun(nwh,thi,pl,de,both,def,strpro),genen).
m(gij,          pronoun(nwh,u,sg,de,nom,def),gij).
m(haar,         pronoun(nwh,thi,sg,de,dat_acc,def),haar).
m(haar,         pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),'\'r').
m(haarzelf,     pronoun(nwh,thi,sg,de,dat_acc,def),haarzelf).
m(haarzelf,     pronoun(nwh,thi,sg,de,dat_acc,def),haarzelve).
m(haar,         pronoun(nwh,thi,both,both,gen,def),harer).
m(hem,          pronoun(nwh,thi,sg,de,dat_acc,def),hem).
m(hem,          pronoun(nwh,thi,sg,de,dat_acc,def,wkpro),'\'m').
m(hemzelf,      pronoun(nwh,thi,sg,de,dat_acc,def),hemzelf).
m(hemzelf,      pronoun(nwh,thi,sg,de,dat_acc,def),hemzelve).
m(hen,          pronoun(nwh,thi,pl,de,dat_acc,def),hen).
m(henzelf,      pronoun(nwh,thi,pl,de,dat_acc,def),henzelf).
m(hetgeen,      pronoun(ywh,thi,sg,het,both,def),hetgeen).
m(hetgeen,      pronoun(nwh,thi,sg,het,both,def,strpro),hetgene).
m(hetwelk,      pronoun(ywh,thi,sg,het,both,def),hetwelk).
m(hij,          pronoun(nwh,thi,sg,de,nom,def),hij).
m('hij_zij',    pronoun(nwh,thi,sg,de,nom,def),'hij/zij').
m('hij_zij',    pronoun(nwh,thi,sg,de,nom,def),[hij,'/',zij]).
m(hijzelf,      pronoun(nwh,thi,sg,de,nom,def),hijzelf).
m(hijzelf,      pronoun(nwh,thi,sg,de,nom,def),hijzelve).
m(hun,          pronoun(nwh,thi,pl,de,dat_acc,def),hun).
m(hunzelf,      pronoun(nwh,thi,pl,de,dat_acc,def),hunzelf).
m(hunzelf,      pronoun(nwh,thi,pl,de,dat_acc,def),hunzelve).
m(hun,          pronoun(nwh,thi,pl,both,gen,def),hunner).
m(iedereen,     pronoun(nwh,thi,sg,de,both,def,strpro),iedereen).
m(ieder,        pronoun(nwh,thi,sg,de,both,def),ieder).
m(ik,           pronoun(nwh,fir,sg,de,nom,def),ik).
m(ikzelf,       pronoun(nwh,fir,sg,de,nom,def),ikzelf).
m(ikzelf,       pronoun(nwh,fir,sg,de,nom,def),ikzelve).
m(je,           pronoun(nwh,je,sg,de,both,def,wkpro),je).
m(je,           pronoun(nwh,inv,sg,de,both,def,wkpro),je_POSTV). % from lex.pl
m(jezelf,       pronoun(nwh,je,both,de,dat_acc,def),jezelf).
m(jezelf,       pronoun(nwh,je,both,de,dat_acc,def),jezelve).
m(jij,          pronoun(nwh,je,sg,de,nom,def),jij).
m(jijzelf,      pronoun(nwh,je,sg,de,nom,def),jijzelf).
m(jijzelf,      pronoun(nwh,je,sg,de,nom,def),jijzelve).
m(jou,          pronoun(nwh,je,sg,de,dat_acc,def),jou).
m(jullie,       pronoun(nwh,je,pl,de,both,def),jullie).
m(men,          pronoun(nwh,thi,sg,de,nom,def),men).
m(me,           pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),me).
m(mekaar,       pronoun(nwh,thi,pl,de,dat_acc,def),mekaar).
m(mekander,     pronoun(nwh,thi,pl,de,dat_acc,def),mekander).
m(menigeen,     pronoun(nwh,thi,sg,de,both,def),menigeen).
m(mezelf,       pronoun(nwh,fir,sg,de,dat_acc,def),mezelf).
m(mezelf,       pronoun(nwh,fir,sg,de,dat_acc,def),mezelve).
m(mij,          pronoun(nwh,fir,sg,de,dat_acc,def),mij).
m(mijn,         pronoun(nwh,fir,sg,both,gen,def),mijner).
m(mijzelf,      pronoun(nwh,fir,both,de,dat_acc,def),mijzelf).  % why both and not sg???
m(mijzelf,      pronoun(nwh,fir,both,de,dat_acc,def),mijzelve). % why both and not sg???
m(niemand,      pronoun(nwh,thi,sg,de,both,indef,strpro),niemand).
m(ons,          pronoun(nwh,fir,pl,de,dat_acc,def),ons).
m(onszelf,      pronoun(nwh,fir,pl,de,dat_acc,def),onszelf).
m(onszelf,      pronoun(nwh,fir,pl,de,dat_acc,def),onszelve).
m(ons,          pronoun(nwh,fir,pl,both,gen,def),onzer).
m(u,            pronoun(nwh,u,sg,de,both,def),u).
m(u,            pronoun(nwh,u,both,both,gen,def),uwer).
m(uzelf,        pronoun(nwh,u,sg,de,both,def),uzelf).
m(uzelf,        pronoun(nwh,u,sg,de,both,def),uzelve).
m('van alles',  pronoun(nwh,thi,sg,het,both,indef,strpro),[van,alles]).
m('van alles',  pronoun(nwh,thi,sg,het,both,indef,strpro),vanalles).
m(wat,          pronoun(ywh,thi,sg,het,both,indef,nparg),wat).
m(we,           pronoun(nwh,fir,pl,de,nom,def,wkpro),we).
m('what the fuck', pronoun(ywh,thi,sg,het,both,def),    [what,the,fuck]).
m('what the fock', pronoun(ywh,thi,sg,het,both,def),    [what,the,fock]).
m('who the fuck',  pronoun(ywh,thi,both,de,both,indef), [who,the,fuck]).
m('who the fock',  pronoun(ywh,thi,both,de,both,indef), [who,the,fock]).
m('wie de fuck',   pronoun(ywh,thi,both,de,both,indef), [wie,de,fuck]).
m('who the hell',  pronoun(ywh,thi,both,de,both,indef), [who,the,hell]).
m(wie,          pronoun(ywh,thi,both,de,both,indef),wie).
m('wie o wie',          pronoun(ywh,thi,both,de,both,indef),[wie,o,wie]).
m('wie o wie',          pronoun(ywh,thi,both,de,both,indef),[wie,',',o,wie]).
m('wie o wie',          pronoun(ywh,thi,both,de,both,indef),[wie,oh,wie]).
m('wie o wie',          pronoun(ywh,thi,both,de,both,indef),[wie,',',oh,wie]).
m('wie de hell',  pronoun(ywh,thi,both,de,both,indef), [wie,de,hell]).
m('wie the hell', pronoun(ywh,thi,both,de,both,indef), [wie,thee,hell]).
m(wie,          pronoun(ywh,thi,both,de,dat_acc,def),wien). 
m(wij,          pronoun(nwh,fir,pl,de,nom,def),wij).
m(wijzelf,      pronoun(nwh,fir,pl,de,nom,def),wijzelf).
m(wijzelf,      pronoun(nwh,fir,pl,de,nom,def),wijzelve).

%% enkelvoud, acc? Ja, in zuidelijk NL https://taaladvies.net/taal/advies/vraag/358/
m(ze,           pronoun(nwh,thi,both,de,both,def,wkpro),ze). 
m(zich,         pronoun(nwh,thi,both,de,dat_acc,def,wkpro),zich).
m(zichzelf,     pronoun(nwh,thi,both,de,dat_acc,def),zichzelf).
m(zichzelf,     pronoun(nwh,thi,both,de,dat_acc,def),zichzelve).
m(zij,          pronoun(nwh,thi,both,de,nom,def),zij).
m(zijn,         pronoun(nwh,thi,sg,both,gen,def),zijner).
m(zijn,         pronoun(nwh,thi,sg,both,gen,def),zijns).
m(zijzelf,      pronoun(nwh,thi,both,de,nom,def),zijzelf).
m(zijzelf,      pronoun(nwh,thi,both,de,nom,def),zijzelve).
m(zoiets,       pronoun(nwh,thi,sg,het,both,indef,strpro),zoiets).
m(zoiets,       pronoun(nwh,thi,sg,het,both,indef,strpro),[zo,iets]).
m(zulk,         pronoun(nwh,thi,sg,het,both,indef),zulks).
m('een en ander',pronoun(nwh,thi,sg,het,both,indef),[een,en,ander]).

m('de een na de ander',     
                pronoun(nwh,thi,sg,de,both,def,strpro),[de,een,na,de,ander]).

m('de een na de ander',     
                predm_adverb,[de,een,na,de,ander]).

m('wat voor één',
  pronoun(ywh,thi,sg,both,both,indef),[wat,voor,Een]) :- een1(Een).


with_dt([zijns,vaders],
  pronoun(nwh,thi,both,both,gen,def),
  dt(np,[det=l(zijn,pronoun(nwh,thi,sg,both,gen,def),detp,0,1),
	 hd =l(vader,determiner(pron),np,1,2)
	])).

%% het huis des schrijvers: ok (although plural for schrijvers)
%% des schrijvers huis: use this one
with_dt([des,schrijvers],
  determiner(pron),
  dt(np,[det=l(de,determiner(des),detp,0,1),
	 hd =l(schrijver,determiner(pron),np,1,2)
	])).

%% de oude wetten des staats
with_dt([des,staats],
  pronoun(nwh,thi,both,both,gen,def),
  dt(np,[det=l(de,determiner(des),detp,0,1),
	 hd =l(staat,determiner(pron),np,1,2)
	])).

%% dat is niet des staats
%% dat zijn des staats
with_dt([des,staats],
  determiner(pron),
  dt(np,[det=l(de,determiner(des),detp,0,1),
	 hd =l(staat,determiner(pron),np,1,2)
	])).

with_dt([de,beiden],
	pronoun(nwh,thi,pl,both,both,def),
	dt(np,[det=l(de,determiner(de),detp,0,1),
	       hd=l(beide,pronoun(nwh,thi,pl,de,both,indef),1,2)
	      ])).

with_dt([zelden,of,nooit],
	max,  %% normal rules can build conjoined modifier
	dt(conj,[cnj=l(zelden,adjective(both(osentadv)),ap,0,1),
		 crd=l(of,conj(of),vg,1,2),
		 cnj=l(nooit,tmp_adverb,advp,2,3)
		])).

with_dt([graag,of,niet],
	max,  %% normal rules can build conjoined modifier
	dt(conj,[cnj=l(graag,adjective(both(osentadv)),ap,0,1),
		 crd=l(of,conj(of),vg,1,2),
		 cnj=l(niet,adverb,advp,2,3)
		])).

% with_dt([wie,wel,en,wie,niet],
% 	pronoun(ywh,thi,both,de,both,indef),
% 	dt(conj,[cnj=dt(np,[hd=l(wie,pronoun(ywh,thi,both,de,both,indef),0,1),
%                             mod=l(wel,adverb,advp,1,2)]),
%                  crd=l(en,conj(en),conj,2,3),
%                  cnj=dt(np,[hd=l(wie,pronoun(ywh,thi,both,de,both,indef),3,4),
%                             mod=l(niet,adverb,advp,4,5)])])).

with_dt([wie,en,wat],
	pronoun(ywh,thi,both,both,both,indef),
	dt(conj,[cnj=l(wie,pronoun(ywh,thi,both,de,both,indef),np,0,1),
                 crd=l(en,conj(en),vg,1,2),
                 cnj=l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),np,2,3)
                ])).

with_dt([wie,of,wat],
	pronoun(ywh,thi,both,both,both,indef),
	dt(conj,[cnj=l(wie,pronoun(ywh,thi,both,de,both,indef),np,0,1),
                 crd=l(of,conj(en),vg,1,2),
                 cnj=l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),np,2,3)
                ])).

with_dt([wien,en,wat],
	pronoun(ywh,thi,both,both,both,indef),
	dt(conj,[cnj=l(wie,pronoun(ywh,thi,both,de,dat_acc,indef),np,0,1),
                 crd=l(en,conj(en),vg,1,2),
                 cnj=l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),np,2,3)
                ])).

with_dt([wien,of,wat],
	pronoun(ywh,thi,both,both,both,indef),
	dt(conj,[cnj=l(wie,pronoun(ywh,thi,both,de,dat_acc,indef),np,0,1),
                 crd=l(of,conj(en),vg,1,2),
                 cnj=l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),np,2,3)
                ])).

with_dt([je,en,jij],
	noun(both,mass,sg),
	dt(conj,[cnj=l(je,pronoun(nwh,je,sg,de,both,def,wkpro),np,0,1),
                 crd=l(en,conj(en),vg,1,2),
                 cnj=l(jij,pronoun(nwh,je,sg,de,nom,def),np,2,3)
                ])).

with_dt([jij,en,jou],
	noun(both,mass,sg),
	dt(conj,[cnj=l(jij,pronoun(nwh,je,sg,de,nom,def),np,0,1),
                 crd=l(en,conj(en),vg,1,2),
                 cnj=l(jou,pronoun(nwh,je,sg,de,dat_acc,def),np,2,3)
                ])).

with_dt([de,ik],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[det=l(de,determiner(de),detp,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([de,'Ik'],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[det=l(de,determiner(de),detp,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([andere,ik],
	noun(de,count,sg),
	dt(np,[mod=l(ander,adjective(e),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([betere,ik],
	noun(de,count,sg),
	dt(np,[mod=l(goed,adjective(ere),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([echte,ik],
	noun(de,count,sg),
	dt(np,[mod=l(echt,adjective(e),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([ware,ik],
	noun(de,count,sg),
	dt(np,[mod=l(waar,adjective(e),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([diepste,ik],
	noun(de,count,sg),
	dt(np,[mod=l(diep,adjective(ste),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([oude,ik],
	noun(de,count,sg),
	dt(np,[mod=l(oud,adjective(e),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([nieuwe,ik],
	noun(de,count,sg),
	dt(np,[mod=l(nieuw,adjective(e),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([lyrische,ik],
	noun(both,count,sg),
	dt(np,[mod=l(lyrisch,adjective(e),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([een,ik],
	pronoun(nwh,thi,sg,de,both,indef,strpro),
	dt(np,[det=l(een,determiner(een),detp,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])).

with_dt([de,hij],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[det=l(de,determiner(de),detp,0,1),
	       hd=l(hij,pronoun(nwh,thi,sg,de,nom,def),1,2)
	      ])).

with_dt([een,hij],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[det=l(een,determiner(een),detp,0,1),
	       hd=l(hij,pronoun(nwh,thi,sg,de,nom,def),1,2)
	      ])).

with_dt([eigen,ik],
	noun(het,count,sg),
	dt(np,[mod=l(eigen,adjective(no_e(nonadv)),ap,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)])).

with_dt([de,een],
	pronoun(nwh,thi,sg,both,both,def),
	dt(np,[det=l(de,determiner(de),detp,0,1),
	       hd=l(een,pronoun(nwh,thi,sg,both,both,indef),1,2)
	      ])).

with_dt([Het,een],
	pronoun(nwh,thi,sg,both,both,def),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(een,pronoun(nwh,thi,sg,both,both,indef),1,2)
	      ])) :- het(Het).

with_dt([Het,ik],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])) :- het(Het).

with_dt([Het,'Ik'],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])) :- het(Het).

with_dt([Het,zelf],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(zelf,predm_adverb,1,2)
	      ])) :- het(Het).

with_dt([Het,'Zelf'],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(zelf,predm_adverb,1,2)
	      ])) :- het(Het).

with_dt([Het,al],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(al,noun(het,count,sg),1,2)
	      ])) :- het(Het).

with_dt([Het,'Al'],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
	       hd=l(al,noun(het,count,sg),1,2)
	      ])) :- het(Het).

with_dt([Een,ik],
	pronoun(nwh,thi,sg,het,both,indef,strpro),
	dt(np,[det=l(een,determiner(een),detp,0,1),
	       hd=l(ik,pronoun(nwh,fir,sg,de,nom,def),1,2)
	      ])) :-
    een(Een).

with_dt([Een,maar],
	pronoun(nwh,thi,sg,het,both,indef,strpro),
	dt(np,[det=l(een,determiner(een),detp,0,1),
	       hd=l(maar,conj(maar),1,2)
	      ])) :-
    een(Een).

with_dt([Een,mits],
	pronoun(nwh,thi,sg,het,both,indef,strpro),
	dt(np,[det=l(een,determiner(een),detp,0,1),
	       hd=l(mits,complementizer,1,2)
	      ])) :-
    een(Een).



with_dt([Jong,en,Oud],
	pronoun(nwh,thi,both,de,both,indef,strpro),
	dt(conj,[crd=l(en,conj(en),vg,1,2),
		 cnj=l(Jong,adjective(no_e(adv)),ap,0,1),
		 cnj=l(Oud,adjective(no_e(adv)),ap,2,3)])) :-
    jong_en_oud(Jong,Oud).

with_dt([zowel,Jong,als,Oud],
	pronoun(nwh,thi,both,de,both,indef,strpro),
	dt(conj,[crd=l(zowel,left_conj(als),vg,0,1),
		 cnj=l(Jong,adjective(no_e(adv)),ap,1,2),
		 crd=l(als,right_conj(als),vg,2,3),
		 cnj=l(Oud,adjective(no_e(adv)),ap,3,4)])) :-
    jong_en_oud(Jong,Oud).

jong_en_oud(J,O) :-
    jong_en_oud1(J,O).
jong_en_oud(J,O) :-
    jong_en_oud1(O,J).

jong_en_oud1(echt,onecht).
jong_en_oud1(groot,klein).
jong_en_oud1(hard,zacht).
jong_en_oud1(hoog,laag).
jong_en_oud1(jong,oud).
jong_en_oud1(oud,nieuw).
jong_en_oud1(rijp,groen).

with_dt([des,Doods],
	determiner(pron),
	dt(np,[ hd=l(Dood,noun(both,both,both),1,2),
		det=l(de,determiner(des),detp,0,1)
	      ])) :-
    genitive_noun(Doods),
    atom_concat(Dood,s,Doods).

with_dt([des,Doods],
	pronoun(nwh,thi,sg,both,gen,def),
	dt(np,[ hd=l(Dood,noun(both,both,both),1,2),
		det=l(de,determiner(des),detp,0,1)
	      ])) :-
    genitive_noun(Doods),
    atom_concat(Dood,s,Doods).

genitive_noun(aanschijns).
genitive_noun(dichters).
genitive_noun(doods).
genitive_noun(heelals).
genitive_noun(hemels).
genitive_noun(huizes).
genitive_noun(keizers).
genitive_noun(konings).
genitive_noun(kwaads).
genitive_noun(lands).
genitive_noun(levens).
genitive_noun(onheils).
genitive_noun(onderscheids).
genitive_noun(oordeels).
genitive_noun(overvloeds).
genitive_noun(persoons).
genitive_noun(rijks).
genitive_noun(tijds).
genitive_noun(vaderlands).
genitive_noun(velds).
genitive_noun(vijands).
genitive_noun(volks).
genitive_noun(woords).

%% het lam [gG]ods
m(god,            pronoun(nwh,thi,sg,de,gen,def),gods).
m('God',            pronoun(nwh,thi,sg,de,gen,def),'Gods').

%% 28 miljard mark 's jaars
m('\'s jaar',            pronoun(nwh,thi,sg,de,gen,def),['\'s',jaars]).

with_dt([de,één],
	pronoun(nwh,thi,sg,de,both,def),
	dt(np,[hd=l(één,pronoun(nwh,thi,sg,de,both,indef),1,2),
	       det=l(de,determiner(de),detp,0,1)
	      ])).

with_dt([Het,één],
	pronoun(nwh,thi,sg,het,both,def),
	dt(np,[hd=l(één,pronoun(nwh,thi,sg,de,both,indef),1,2),
	       det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1)
	      ])) :- het(Het).

with_dt([helemaal,niemand],
	pronoun(nwh,thi,sg,de,both,indef,strpro),
	dt(np,[mod=l(helemaal,adverb,advp,0,1),
	       hd=l(niemand,pronoun(nwh,thi,sg,de,both,indef,strpro),1,2)
	      ])).

with_dt([absoluut,niemand],
	pronoun(nwh,thi,sg,de,both,indef,strpro),
	dt(np,[mod=l(absoluut,adverb,advp,0,1),
	       hd=l(niemand,pronoun(nwh,thi,sg,de,both,indef,strpro),1,2)
	      ])).

with_dt([absoluut,iedereen],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[mod=l(absoluut,adverb,advp,0,1),
	       hd=l(niemand,pronoun(nwh,thi,sg,de,both,def,strpro),1,2)
	      ])).

with_dt([zo,iemand],
	pronoun(nwh,thi,sg,de,both,indef,strpro),
	dt(np,[mod=l(zo,adverb,advp,0,1),
	       hd=l(iemand,noun(de,count,sg),1,2)
	      ])).


with_dt([helemaal,niets],
	pronoun(nwh,thi,sg,het,both,indef,strpro),
	dt(np,[mod=l(helemaal,adverb,advp,0,1),
	       hd=l(niets,noun(het,mass,sg),1,2)
	      ])).

with_dt([helemaal,niks],
	pronoun(nwh,thi,sg,het,both,indef,strpro),
	dt(np,[mod=l(helemaal,adverb,advp,0,1),
	       hd=l(niks,noun(het,mass,sg),1,2)
	      ])).

with_dt([zo,eentje],
	pronoun(nwh,thi,sg,de,both,indef,strpro),
	dt(np,[mod=l(zo,adverb,advp,0,1),
	       hd=l(één,pronoun(nwh,thi,sg,de,both,indef,strpro),1,2)
	      ])).

with_dt([niet,iedereen],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[mod=l(niet,adverb,advp,0,1),
	       hd=l(iedereen,pronoun(nwh,thi,sg,de,both,def,strpro),1,2)
	      ])).

with_dt([lang,niet,iedereen],
	pronoun(nwh,thi,sg,de,both,def,strpro),
	dt(np,[mod=dt(advp,[mod=l(lang,adverb,advp,0,1),
			    hd=l(niet,adverb,1,2)]),
	       hd=l(iedereen,pronoun(nwh,thi,sg,de,both,def,strpro),2,3)
	      ])).

with_dt([lang,niet,allemaal],
	pronoun(nwh,thi,pl,de,both,indef),
	dt(np,[mod=dt(advp,[mod=l(lang,adverb,advp,0,1),
			    hd=l(niet,adverb,1,2)]),
	       hd=l(allemaal,pronoun(nwh,thi,pl,de,both,indef),2,3)
	      ])).

with_dt([niet,alles],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[mod=l(niet,adverb,advp,0,1),
	       hd=l(alles,noun(het,mass,sg),1,2)
	      ])).

with_dt([lang,niet,alles],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(np,[mod=dt(advp,[mod=l(lang,adverb,advp,0,1),
			    hd=l(niet,adverb,1,2)]),
	       hd=l(alles,noun(het,mass,sg),2,3)
	      ])).

with_dt([Het,hoe,en,wat],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(conj,[crd=l(en,conj(en),vg,2,3),
		 cnj=dt(np,[det=ix(DET,l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1)),
			    hd=l(hoe,wh_adverb,1,2)]),
		 cnj=dt(np,[det=ix(DET),
			    hd=l(wat,pronoun(ywh,thi,sg,het,both,indef,nparg),3,4)])])) :- het(Het).

with_dt([Het,hier,en,nu],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(conj,[crd=l(en,conj(en),vg,2,3),
		 cnj=dt(np,[det=ix(DET,l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1)),
			    hd=l(hier,er_loc_adverb,1,2)]),
		 cnj=dt(np,[det=ix(DET),
			    hd=l(nu,tmp_adverb,3,4)])])) :- het(Het).

%% VL
with_dt([Het,hier,en,Het2,nu],
	pronoun(nwh,thi,sg,het,both,def,strpro),
	dt(conj,[crd=l(en,conj(en),vg,2,3),
		 cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
			    hd=l(hier,er_loc_adverb,1,2)]),
		 cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,3,4),
			    hd=l(nu,tmp_adverb,4,5)])])) :-
    het(Het),
    het(Het2).

with_dt([hard,en,soft,drugs],
	noun(het,mass,sg),
	dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(hard,noun(het,mass,sg),np,0,1),
                 cnj=l('soft drugs',noun(het,mass,sg),np,2,4)])).

with_dt([soft,en,hard,drugs],
	noun(het,mass,sg),
	dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(soft,noun(het,mass,sg),np,0,1),
                 cnj=l('hard drugs',noun(het,mass,sg),np,2,4)])).

with_dt([binnen,en,buitenland],
	noun(het,mass,sg),
	dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(binnenland,noun(het,count,sg),np,0,1),
                 cnj=l(buitenland,noun(het,mass,sg),np,2,3)])).

with_dt([eigen,en,andermans],
	adjective(both(nonadv)),
	dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(eigen,adjective(both(nonadv)),ap,0,1),
                 cnj=l(andermans,determiner(pron),detp,2,3)])).

with_dt([eigen,of,andermans],
	adjective(both(nonadv)),
	dt(conj,[crd=l(of,conj(en),vg,1,2),
                 cnj=l(eigen,adjective(both(nonadv)),ap,0,1),
                 cnj=l(andermans,determiner(pron),detp,2,3)])).

with_dt([eigen,en,elkaars],
	adjective(both(nonadv)),
	dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(eigen,adjective(both(nonadv)),ap,0,1),
                 cnj=l(elkaar,determiner(pron),detp,2,3)])).

with_dt([eigen,of,elkaars],
	adjective(both(nonadv)),
	dt(conj,[crd=l(of,conj(en),vg,1,2),
                 cnj=l(eigen,adjective(both(nonadv)),ap,0,1),
                 cnj=l(elkaar,determiner(pron),detp,2,3)])).

with_dt([deze,en,aanverwante],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(aanverwant,adjective(e),ap,2,3)])
       ).

with_dt([deze,en,bovengenoemde],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(bovengenoemd,adjective(e),ap,2,3)])
       ).

with_dt([deze,en,volgende],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(volg,adjective(ende(padv)),ap,2,3)])
       ).

with_dt([deze,en,vorige],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(vorig,adjective(e),ap,2,3)])
       ).

with_dt([deze,en,soortgelijke],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(soortgelijk,adjective(e),ap,2,3)])
       ).

with_dt([dit,en,volgend],
        adjective(no_e(nonadv)),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(dit,determiner(het,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(volg,adjective(end(padv)),ap,2,3)])
       ).

with_dt([dit,en,vorig],
        adjective(no_e(nonadv)),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(dit,determiner(het,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(vorig,adjective(no_e(nonadv)),ap,2,3)])
       ).

with_dt([vorig,en,dit],
        adjective(no_e(nonadv)),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(vorig,adjective(no_e(nonadv)),ap,0,1),
                 cnj=l(dit,determiner(het,nwh,nmod,pro,nparg),detp,2,3)])
       ).

with_dt([deze,en,andere],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=l(ander,adjective(ere),ap,2,3)])
       ).

with_dt([deze,en,nog,andere],
        adjective(e),
        dt(conj,[crd=l(en,conj(en),vg,1,2),
                 cnj=l(deze,determiner(de,nwh,nmod,pro,nparg),detp,0,1),
                 cnj=dt(ap,[mod=l(nog,adverb,advp,2,3),
			    hd=l(ander,adjective(ere),ap,3,4)
			   ])])).

with_dt([helemaal,ongelijk],
	noun(het,mass,sg),
	dt(np,[mod=l(helemaal,adverb,advp,0,1),
	       hd=l(ongelijk,noun(het,mass,sg),1,2)
	      ])).

with_dt([nauwelijks,of,geen],
        determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),
        dt(conj,[crd=l(of,conj(of),vg,1,2),
                 cnj=l(geen,determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),detp,2,3),
                 cnj=l(nauwelijks,adverb,advp,0,1)
                ])).

with_dt([Een,of,meerdere],
	determiner(pl_num,nwh,nmod,pro,yparg),
	dt(conj,[cnj=l(één,number(hoofd(sg_num)),detp,0,1),
		 crd=l(of,conj(of),vg,1,2),
		 cnj=l(meerdere,determiner(pl_num,nwh,nmod,pro,yparg),detp,2,3)
		])) :-
    een(Een).

with_dt([Een,of,meer],
	determiner(pl_num,nwh,nmod,pro,yparg),
	dt(conj,[cnj=l(één,number(hoofd(sg_num)),detp,0,1),
		 crd=l(of,conj(of),vg,1,2),
		 cnj=l(veel,adjective(meer),ap,2,3)
		])) :-
    een(Een).

with_dt([dit,en,het,V,J],
	tmp_np,
	dt(conj,[cnj=dt(np,[det=l(dit,determiner(het,nwh,nmod,pro,nparg),detp,0,1),
			    hd=ix(JAAR,l(J,tmp_noun(het,count,meas),np,4,5))
			   ]),
		 crd=l(en,conj(en),vg,1,2),
		 cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,2,3),
			    mod=l(Vstem,adjective(ende(padv)),ap,3,4),
			    hd=ix(JAAR)
			    ])
		])) :-
    jaar(J),
    volgende(V,Vstem).

jaar(decennium).
jaar(jaar).
jaar(millenium).
jaar(seizoen).

volgende(komende,kom).
volgende(volgende,volg).

%% TODO: deze en de volgende pagina/...


%% prescriptive:
%% m(wat,          rel_pronoun(wat,het,dat)).
%% descriptive:
%% alles wat je ziet, in de Indesit
%% het mooiste wat ik ken is ..
%% ?het huis wat we gezien hadden

m(dat,          rel_pronoun(het,   no_obl), dat).
m(die,          rel_pronoun(de,    no_obl), die).
m(hetgeen,      rel_pronoun(both,  no_obl), hetgeen).
m(hetwelk,      rel_pronoun(both,  no_obl), hetwelk).
m(wat,          rel_pronoun(het,   both),   wat).
m(welk,         rel_pronoun(both,     obl), welke). % een vereniging, voor welke nog hoop bestaat
m(welk,         rel_pronoun(both,     obl), [de,welke]). %VL
m(wie,          rel_pronoun(both,     obl), wie). % het moedertje voor wie dat belangrijk is
m(wie,          rel_pronoun(both,  no_nom), wien).

m('hetgeen dat',rel_pronoun(het,   both),   [hetgeen,dat]). % dat is hetgeen dat ze zoeken

m(je,           reflexive(je,both),je).
m(jullie,       reflexive(je,pl),jullie).
m(me,           reflexive(fir,sg),me).
m(mij,          reflexive(fir,sg),mij).
m(ons,          reflexive(fir,pl),ons).
m(u,            reflexive(u,sg),u).
m(zich,         reflexive(u_thi,both),zich).

%% gesproken taal, regionaal (?)

m('me eigen',           pronoun(nwh,fir,both,de,dat_acc,def),[me,eigen]).
m('mijn eigen',         pronoun(nwh,fir,sg,de,dat_acc,def),  [mijn,eigen]).
m('m\'n eigen',         pronoun(nwh,fir,sg,de,dat_acc,def),  ['m\'n',eigen]).
m('je eigen',           pronoun(nwh,je,sg,de,dat_acc,def),   [je,eigen]).
m('d\'r eigen',         pronoun(nwh,thi,sg,de,dat_acc,def),  ['d\'r',eigen]).
m('\'r eigen',          pronoun(nwh,thi,sg,de,dat_acc,def),  ['\'r',eigen]).
m('haar eigen',         pronoun(nwh,thi,sg,de,dat_acc,def),  [haar,eigen]).
m('zijn eigen',         pronoun(nwh,thi,sg,de,dat_acc,def),  [zijn,eigen]).
m('z\'n eigen',         pronoun(nwh,thi,sg,de,dat_acc,def),  ['z\'n',eigen]).
m('ons eigen',          pronoun(nwh,fir,pl,de,dat_acc,def),  [ons,eigen]).
m('jullie eigen',       pronoun(nwh,je,pl,de,dat_acc,def),   [jullie,eigen]).
m('u eigen',            pronoun(nwh,u,sg,de,both,def),       [u,eigen]).
m('uw eigen',           pronoun(nwh,u,sg,de,both,def),       [uw,eigen]).
m('hun eigen',          pronoun(nwh,thi,pl,de,dat_acc,def),  [hun,eigen]).

m('me eigen',           reflexive(fir,sg),                   [me,eigen]).
m('mijn eigen',         reflexive(fir,sg),                   [mijn,eigen]).
m('m\'n eigen',         reflexive(fir,sg),                   ['m\'n',eigen]).
m('je eigen',           reflexive(je,both),                  [je,eigen]).
m('d\'r eigen',         reflexive(thi,sg),                   ['d\'r',eigen]).
m('\'r eigen',          reflexive(thi,sg),                   ['\'r',eigen]).
m('haar eigen',         reflexive(thi,sg),                   [haar,eigen]).
m('zijn eigen',         reflexive(thi,sg),                   [zijn,eigen]).
m('z\'n eigen',         reflexive(thi,sg),                   ['z\'n',eigen]).
m('ons eigen',          reflexive(fir,pl),                   [ons,eigen]).
m('jullie eigen',       reflexive(je,pl),                    [jullie,eigen]).
m('u eigen',            reflexive(u,sg),                     [u,eigen]).
m('uw eigen',           reflexive(u,sg),                     [uw,eigen]).
m('hun eigen',          reflexive(thi,pl),                   [hun,eigen]).

m(jezelf,       reflexive(je,both),jezelf).
m(mezelf,       reflexive(fir,sg),mezelf).
m(mijzelf,      reflexive(fir,sg),mijzelf).
m(onszelf,      reflexive(fir,pl),onszelf).
m(uzelf,        reflexive(u,sg),uzelf).
m(zichzelf,     reflexive(u_thi,both),zichzelf).

%% Vlaams
m(ulder,        reflexive(thi,pl),ulder).

%%%%%%%%%%%%%%%
%%%% NOUNS %%%%
%%%%%%%%%%%%%%%

:- discontiguous
    nominalized_adjective/2,
    nominalized_adjective/3,
    end_nominalized_adjective/2,
    end_nominalized_adjective/3,
    ge_nominalized_adjective/2,
    ge_nominalized_adjective/3.

m(Stem,nominalized_adjective,Surf) :-
    nominalized_adjective(Stem,Surf).
m(Stem,nominalized_adjective,Surf) :-
    nominalized_adjective(Stem0,Surf0,Compounds),
    add_compounds:add_compounds(Compounds,Stem0,Surf0,Stem,Surf).
m(Stem,end_nominalized_adjective,Surf) :-
    end_nominalized_adjective(Stem,Surf).
m(Stem,end_nominalized_adjective,Surf) :-
    end_nominalized_adjective(Stem0,Surf0,Compounds),
    add_compounds:add_compounds(Compounds,Stem0,Surf0,Stem,Surf).
m(Stem,nominalized_adjective_sg,Surf) :-
    nominalized_adjective_both(Stem,Surf,_).
m(Stem,ge_nominalized_adjective,Surf) :-
    nominalized_adjective_both(Stem,_,Surf).
m(Stem,ge_nominalized_adjective,Surf) :-
    ge_nominalized_adjective(Stem,Surf).
m(Stem,ge_nominalized_adjective,Surf) :-
    ge_nominalized_adjective(Stem0,Surf0,Compounds),
    add_compounds:add_compounds(Compounds,Stem0,Surf0,Stem,Surf).

m(spreken,     end_nominalized_adjective(transitive),             sprekenden).
m(veroordelen, ge_nominalized_adjective(fixed([[ter,dood]])),    veroordeelden).

%% always participles
nominalized_adjective_both(aan_wijzen,aangewezene,aangewezenen).
nominalized_adjective_both(betrekken,betrokkene,betrokkenen).
nominalized_adjective_both(bezeten,bezetene,bezetenen).
nominalized_adjective_both(bieden,gebodene,gebodenen).
nominalized_adjective_both(geboren,geborene,geborenen).
nominalized_adjective_both(vallen,gevallene,gevallenen).
nominalized_adjective_both(treffen,getroffene,getroffenen).
nominalized_adjective_both(overwegen,overwogene,overwogenen).
nominalized_adjective_both(over_blijven,overgeblevene,overgeblevenen).
nominalized_adjective_both(pasgeboren,pasgeborene,pasgeborenen).
nominalized_adjective_both(uit_verkiezen,uitverkorene,uitverkorenen).
nominalized_adjective_both(vrij_laten,vrijgelatene,vrijgelatenen).
nominalized_adjective_both(werkwillig,werkwillige,werkwilligen).



%% een ander dan ik had allang...
m(ander,nominalized_compar_adjective_sg,ander).

m(ander,nominalized_compar_adjective,anderen).
m(oud,  nominalized_compar_adjective,ouderen).
m(klein,nominalized_compar_adjective,kleineren).
m(rijk, nominalized_compar_adjective,rijkeren).
m(zwak, nominalized_compar_adjective,zwakkeren).

m(achter,nominalized_super_adjective,achtersten).
m(beroemd,nominalized_super_adjective,beroemdsten).
m(laat,  nominalized_super_adjective,laatsten).
m(lief,  nominalized_super_adjective,liefsten).

%% plural only, because singular is an adjective, and hence can be
%% treated in syntax:
%% we only need to list words that are otherwise in the lexicon too,
%% eg as a past tense verb. Other cases are treated in unknowns.pl
end_nominalized_adjective(alleen_staan,alleenstaanden).
end_nominalized_adjective(anders_denken,andersdenkenden).
end_nominalized_adjective(belang_hebben,belanghebbenden).
end_nominalized_adjective(belang_stellen,belangstellenden).
end_nominalized_adjective(hulp_zoeken,hulpzoekenden).
end_nominalized_adjective(leiding_geven,leidinggevenden).
end_nominalized_adjective(slecht_horen,slechthorenden).
end_nominalized_adjective(slecht_zien,slechtzienden).
end_nominalized_adjective(spreken,sprekenden).
end_nominalized_adjective(werk_zoeken,werkzoekenden).
end_nominalized_adjective(woning_zoeken,woningzoekenden).

ge_nominalized_adjective(afleiden,afgeleiden).
ge_nominalized_adjective(af_studeren,afgestudeerden).
ge_nominalized_adjective(arresteren,gearresteerden).
ge_nominalized_adjective(bedelen,bedeelden).
ge_nominalized_adjective(bedreigen,bedreigden).
ge_nominalized_adjective(bedroeven,bedroefden).
ge_nominalized_adjective(begunstigen,begunstigden).
ge_nominalized_adjective(behandelen,behandelden).
ge_nominalized_adjective(bekeren,bekeerden).
ge_nominalized_adjective(beklagen,beklaagden).
ge_nominalized_adjective(bekronen,bekroonden).
ge_nominalized_adjective(belasteren,belasterden).
ge_nominalized_adjective(beledigen,beledigden).
ge_nominalized_adjective(belegeren,belegerden).
ge_nominalized_adjective(beminnen,beminden).
ge_nominalized_adjective(benadelen,benadeelden).
ge_nominalized_adjective(benoemen,benoemden).
ge_nominalized_adjective(beperken,beperkten).
ge_nominalized_adjective(beschuldigen,beschuldigden).
ge_nominalized_adjective(beschuldigen,beschuldigden).
ge_nominalized_adjective(besmetten,besmetten).
ge_nominalized_adjective(bestraffen,bestraften).
ge_nominalized_adjective(betalen,betaalden).
ge_nominalized_adjective(betichten,betichten).
ge_nominalized_adjective(betrappen,betrapten).
ge_nominalized_adjective(betrekken,betrokkenen).
ge_nominalized_adjective(bevoegen,bevoegden).
ge_nominalized_adjective(bevoordelen,bevoordeelden).
ge_nominalized_adjective(bevragen,bevraagden).
ge_nominalized_adjective(bezielen,bezielden).
ge_nominalized_adjective(bezoldigd,bezoldigden).
ge_nominalized_adjective(bezwaren,bezwaarden).
ge_nominalized_adjective(delegeren,gedelegeerden).
ge_nominalized_adjective(deporteren,gedeporteerden).
ge_nominalized_adjective(drug_verslaven,drugsverslaafden).
ge_nominalized_adjective(duperen,gedupeerden).
ge_nominalized_adjective(gedogen,gedoogden).
ge_nominalized_adjective(gijzelen,gegijzelden).
ge_nominalized_adjective(hoog_op_leiden,hoogopgeleiden).
ge_nominalized_adjective(huwen,gehuwden).
ge_nominalized_adjective(interesseren,geïnteresseerden).
ge_nominalized_adjective(in_wijden,ingewijden).
ge_nominalized_adjective(kies_rechtigen,kiesgerechtigden).
ge_nominalized_adjective(kwetsten,gekwetsten).
ge_nominalized_adjective(laag_op_leid,laagopgeleiden).
ge_nominalized_adjective(laag_scholen,laaggeschoolden).
ge_nominalized_adjective(machtigen,gemachtigden).
ge_nominalized_adjective(mis_maken,mismaakten).
ge_nominalized_adjective(mis_vormen,misvormden).
ge_nominalized_adjective(nomineren,genomineerden).
ge_nominalized_adjective(onderdrukken,onderdrukten).
ge_nominalized_adjective(ondervragen,ondervraagden).
ge_nominalized_adjective(onderzoeken,onderzochten).
ge_nominalized_adjective(onteigenen,onteigenden).
ge_nominalized_adjective(onterven,onterfden).
ge_nominalized_adjective(ontsnappen,ontsnapten).
ge_nominalized_adjective(ontvoeren,ontvoerden).
ge_nominalized_adjective(ontwikkelen,ontwikkelden).
ge_nominalized_adjective(ontwortelen,ontwortelden).
ge_nominalized_adjective(op_leiden,opgeleiden).
ge_nominalized_adjective(overtuigen,overtuigden).
ge_nominalized_adjective(pensioneren,gepensioneerden).
ge_nominalized_adjective(plaatsen,geplaatsten).
ge_nominalized_adjective(slagen,geslaagden).
ge_nominalized_adjective(stellen,gestelden).
ge_nominalized_adjective(tekenen,getekenden).
ge_nominalized_adjective(trouwen,getrouwden).
ge_nominalized_adjective(veinzen,geveinsden).
ge_nominalized_adjective(verblinden,verblinden).
ge_nominalized_adjective(verdoemen,verdoemden).
ge_nominalized_adjective(verdrukken,verdrukten).
ge_nominalized_adjective(verhoren,verhoorden).
ge_nominalized_adjective(verkiezen,verkozenen).
ge_nominalized_adjective(verkommeren,verkommerden).
ge_nominalized_adjective(verloederen,verloederden).
ge_nominalized_adjective(verlossen,verlosten).
ge_nominalized_adjective(verminken,verminkten).
ge_nominalized_adjective(vermoeien,vermoeiden).
ge_nominalized_adjective(vermoorden,vermoorden).
ge_nominalized_adjective(verongelukken,verongelukten).
ge_nominalized_adjective(verontrusten,verontrusten).
ge_nominalized_adjective(verontwaardigen,verontwaardigden).
ge_nominalized_adjective(veroordelen,veroordeelden).
ge_nominalized_adjective(verpauperen,verpauperden).
ge_nominalized_adjective(verplegen,verpleegden).
ge_nominalized_adjective(verslaven,verslaafden,
			 [alcohol,
			  drug,
			  drugs,
			  game,
			  gok,
			  nicotine,
			  sex]).
ge_nominalized_adjective(vertrappen,vertrapten).
ge_nominalized_adjective(vertrouwen,vertrouwden).
ge_nominalized_adjective(vervloeken,vervloekten).
ge_nominalized_adjective(vervolgen,vervolgden).
ge_nominalized_adjective(verzekeren,verzekerden).
ge_nominalized_adjective(verzuren,verzuurden).
ge_nominalized_adjective(vorderen,gevorderden).
ge_nominalized_adjective(ziekenfonds_verzekeren,ziekenfondsverzekerden).

nominalized_adjective(aanstaand,aanstaanden).
nominalized_adjective(aanwezig,aanwezigen).
nominalized_adjective(achterlijk,achterlijken).
nominalized_adjective(afvallig,afvalligen).
nominalized_adjective(afwezig,afwezigen).
nominalized_adjective(arbeidsongeschikt,arbeidsongeschikten).
nominalized_adjective(arm,armen).
nominalized_adjective(bejaard,bejaarden).
nominalized_adjective(bekend,bekenden).
nominalized_adjective(belasting_plichtig,belastingplichtigen).
nominalized_adjective('BTW_belasting_plichtig','BTW-belastingplichtigen').
nominalized_adjective('niet_belasting_plichtig','niet-belastingplichtigen').
nominalized_adjective(beroemd,beroemden).
nominalized_adjective(beschaafd,beschaafden).
nominalized_adjective(bevoorrecht,bevoorrechten).
nominalized_adjective(blank,blanken).
nominalized_adjective(blind,blinden).
nominalized_adjective(blond,blonden).
nominalized_adjective(boos,bozen).
nominalized_adjective(bruin,bruinen).
nominalized_adjective(dakloos,daklozen).
nominalized_adjective(deskundig,deskundigen). % in order to be able to analyze "onafhankelijk deskundigen"
nominalized_adjective(dezelfde,dezelfden).
nominalized_adjective(diezelfde,diezelfden).
nominalized_adjective(dienstplichtig,dienstplichtigen).
nominalized_adjective(dik,dikken).
nominalized_adjective(dood,doden).
nominalized_adjective(doof,doven).
nominalized_adjective(doopsgezind,doopsgezinden).
nominalized_adjective(doortrapt,doortrapten).
nominalized_adjective(driejarig,driejarigen).
nominalized_adjective(droog,drogen).
nominalized_adjective('Duits_talig','Duitstaligen').
nominalized_adjective(dun,dunnen).
nominalized_adjective(edel,edelen).
nominalized_adjective(eerwaard,eerwaarden).
nominalized_adjective(ene,enen).
nominalized_adjective('Engel_stalig','Engelstaligen').
nominalized_adjective(enig,enigen).
nominalized_adjective(enkel,enkelen).
nominalized_adjective(extern,externen).
nominalized_adjective('Frans_talig','Franstaligen').
nominalized_adjective(geallieerd,geallieerden).
nominalized_adjective(gehandicapt,gehandicapten).
nominalized_adjective(geleerd,geleerden).
nominalized_adjective(geliefd,geliefden).
nominalized_adjective(gelijk,gelijken).
nominalized_adjective(gelijkgestemd,gelijkgestemden).
nominalized_adjective(gelovig,gelovigen).
nominalized_adjective(gelukkig,gelukkigen).
nominalized_adjective(genodigd,genodigden).
nominalized_adjective(gereformeerd,gereformeerden).
nominalized_adjective(gestoord,gestoorden).
nominalized_adjective(getrouw,getrouwen).
nominalized_adjective(geweldig,geweldigen).
nominalized_adjective(gewetensbezwaard,gewetensbezwaarden).
nominalized_adjective(gewond,gewonden).
nominalized_adjective(gezond,gezonden).
nominalized_adjective(goed,besten).
nominalized_adjective(goed,beteren).
nominalized_adjective(goed,goeden).
nominalized_adjective(groen,groenen).
nominalized_adjective(groot,groten).
nominalized_adjective(grotesk,grotesken).
nominalized_adjective(heilig,heiligen).
nominalized_adjective(herkeurd,herkeurden).
nominalized_adjective(hervormd,hervormden).
nominalized_adjective(hoog,hogen).
nominalized_adjective(invalide,invaliden).
nominalized_adjective(ingezeten,ingezetenen).
nominalized_adjective(intern,internen).
nominalized_adjective(jarig,jarigen).
nominalized_adjective(kansarm,kansarmen).
nominalized_adjective(klassiek,klassieken).
nominalized_adjective(klein,kleinen).
nominalized_adjective(krankzinnig,krankzinnigen).
nominalized_adjective(kreupel,kreupelen).
nominalized_adjective(krom,krommen).
nominalized_adjective(kwaad,kwaden).   % waar de goeden onder moeten lijden
nominalized_adjective(langharigen,langharigen).
nominalized_adjective(licht_gewond,lichtgewonden).
nominalized_adjective(los,lossen).
nominalized_adjective(machtig,machtigen).
nominalized_adjective(meest,meesten).
nominalized_adjective(minderjarig,minderjarigen).
nominalized_adjective(minder,minderen).
nominalized_adjective(mobiel,mobielen).  % de opwaarts mobielen
nominalized_adjective(moedig,moedigen).
nominalized_adjective(naast,naasten).
nominalized_adjective('Nederlands_talig','Nederlandstaligen').
nominalized_adjective(nieuwsgierig,nieuwsgierigen).
nominalized_adjective(onafhankelijk,onafhankelijken).
nominalized_adjective(onbekend,onbekenden).
nominalized_adjective(ongelovig,ongelovigen).
nominalized_adjective(ongelukkig,ongelukkigen).
nominalized_adjective(ontevreden,ontevredenen).
nominalized_adjective(ontheemd,ontheemden).
nominalized_adjective(onwetend,onwetenden).
nominalized_adjective(onzalig,onzaligen).
nominalized_adjective(orthodox,orthodoxen).
nominalized_adjective(oud,ouden).
nominalized_adjective(overig,overigen).
nominalized_adjective(partijloos,partijlozen).
nominalized_adjective(prominent,prominenten).
nominalized_adjective(rampzalig,rampzaligen).
nominalized_adjective(rap,rappen).
nominalized_adjective(rechthebbend,rechthebbenden).
nominalized_adjective(rechtvaardig,rechtvaardigen).
nominalized_adjective(religieus,religieuzen).
nominalized_adjective(rijk,rijken).
nominalized_adjective(rood,roden).
nominalized_adjective(rood,rooien).
nominalized_adjective(scheel,schelen).
nominalized_adjective(schoon,schonen).
nominalized_adjective(schuldig,schuldigen).
nominalized_adjective(slecht,slechten).
nominalized_adjective(sommig,sommigen).
nominalized_adjective('Spaans_talig','Spaanstaligen').
nominalized_adjective(staatkundig,staatkundigen).
nominalized_adjective('Staats','Staatsen').
nominalized_adjective(stemgerechtigd,stemgerechtigden).
nominalized_adjective(sterk,sterken).
nominalized_adjective(stil,stillen).
nominalized_adjective(stom,stommen).
nominalized_adjective(thuisloos,thuislozen).
nominalized_adjective(uiterst,uitersten).
nominalized_adjective(valide,validen,[minder]). 
nominalized_adjective(veel,velen).
nominalized_adjective(verantwoordelijk,verantwoordelijken).
nominalized_adjective(verkeerd,verkeerden).
nominalized_adjective(vermist,vermisten).
nominalized_adjective(vreemd,vreemden).
nominalized_adjective(vrijgesteld,vrijgestelden).
nominalized_adjective(vrij,vrijen).
nominalized_adjective(vroom,vromen).
nominalized_adjective(waanzinnig,waanzinnigen).
nominalized_adjective(weinig,weinigen).
nominalized_adjective(welgesteld,welgestelden).
nominalized_adjective(werkeloos,werkelozen).
nominalized_adjective(werkloos,werklozen).
nominalized_adjective(wijs,wijzen).
nominalized_adjective(wild,wilden).
nominalized_adjective(wit,witten).
nominalized_adjective(zalig,zaligen).
nominalized_adjective(ziek,zieken).
nominalized_adjective(zoveel,zovelen).
nominalized_adjective(zwaar_gewond,zwaargewonden).
nominalized_adjective(zwak,zwakken).
nominalized_adjective(zwakzinnig,zwakzinnigen).
nominalized_adjective(zwart,zwarten).

m(Stem,iets_noun,Surf) :-
    iets_noun(Surf),
    stem_from_surf(Surf,Stem).

m(veel,iets_noun,meer).

m(Stem,wh_iets_noun,Surf) :-
    wh_iets_noun(Surf),
    stem_from_surf(Surf,Stem).

%% zie ANS, blz 412
%% + voldoende, een (hele) hoop/boel, ... minder
%% + split wat voor: wat wil je voor geks doen?
%% + hoeveel
%% requires spec. type of determiner?
%% ook:
%% Er is een hoop plezier en spannends te beleven in de techniek .
%% Bonnie ziet zichzelf niet als iemand speciaals .
%% Of kinderen door het nieuwe systeem werkelijk minder verderfelijks zullen zien , blijft de vraag .
%% Hij zegt er ' primaire eerlijkheid ' in aan te treffen , een standpunt dat inderdaad waars bevat .
%% Hier vinden we leuks als : " A. ALBERTS stopte na de toekenning van de P.C. Hooftprijs met schrijven .

with_dt([al,het],
	iets_noun,
	dt(detp,[mod=l(al,pre_det_quant(al),detp,0,1),
		 hd=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),1,2)
		 ])).

with_dt([al,dit],
	iets_noun,
	dt(detp,[mod=l(al,pre_det_quant(al),detp,0,1),
		 hd=l(dit,determiner(het,nwh,nmod,pro,nparg),1,2)
		 ])).

with_dt([al,dat],
	iets_noun,
	dt(detp,[mod=l(al,pre_det_quant(al),detp,0,1),
		 hd=l(dat,determiner(het,nwh,nmod,pro,nparg),1,2)
		 ])).

with_dt([al,degenen],
        pronoun(nwh,thi,pl,de,both,def,strpro),
        dt(np,[det=l(al,pre_det_quant(al),detp,0,1),
               hd=l(degeen,pronoun(nwh,thi,pl,de,both,def,strpro),1,2)
              ])).

with_dt([al,wie],
        pronoun(ywh,thi,both,de,both,indef),
        dt(np,[det=l(al,pre_det_quant(al),detp,0,1),
               hd=l(wie,pronoun(ywh,thi,both,de,both,indef),1,2)
              ])).

%% heel wat = nogal wat, maar "nogal" is adverb, and "heel" is intensifier
%% heel wat genodigden trokken de beurs
with_dt([heel,wat],
        determiner(wat),
        dt(detp,[hd=l(wat,determiner(wat,nwh,mod,pro,nparg,ntopicpro),1,2),
                 mod=l(heel,intensifier,advp,0,1)])).

%% heel wat van de genodigden trok(ken) de beurs...
%% er speelt zich heel wat af dat beter verborgen blijft
with_dt([heel,wat],
        pronoun(nwh,thi,sg,het,both,indef,strpro),
        dt(np,[hd=l(wat,determiner(wat,nwh,mod,pro,nparg,ntopicpro),1,2),
                 mod=l(heel,intensifier,advp,0,1)])).



iets_noun(allerlei).
iets_noun(ander).
iets_noun(genoeg).
iets_noun(iets).
iets_noun(niets).
iets_noun(niks).
iets_noun(teveel).
iets_noun(veel).
iets_noun(wat).
iets_noun(weinig).
iets_noun(zoiets).
iets_noun([zo,iets]).
iets_noun(zoveel).

with_dt([te,veel],
        iets_noun,
        dt(np,[hd=l(veel,adjective(no_e(odet_adv)),1,2),
               mod=l(te,intensifier,advp,0,1)])).

with_dt([niet,veel],
        iets_noun,
        dt(np,[hd=l(veel,adjective(no_e(odet_adv)),1,2),
               mod=l(niet,adverb,advp,0,1)])).

wh_iets_noun(hoeveel).
wh_iets_noun(watvoor).
wh_iets_noun([wat,voor]).

%% ik wil ergens anders wonen
%% ik wil nergens anders wonen dan in Aduard
m(ergens,           iets_adverb, ergens).
m(nergens,          iets_adverb,nergens).
m(waar,             wh_iets_adverb,waar).

%% niemand anders dan Kok moet lijsttrekker worden
m(iemand,           iets_anders_noun,iemand).
m(niemand,          iets_anders_noun,niemand).
m(wie,              wh_iets_anders_noun,wie).

%% (n)iemand anders; (n)ergens anders
m(ander,           post_adjective_anders(er), anders).

m(daar,     er_loc_adverb,     daar    ).
m(hier,     er_loc_adverb,     hier    ).
m(ergens,   er_loc_adverb,     ergens  ).
m(nergens,  er_loc_adverb,     nergens ).
m(overal,   er_loc_adverb,     overal  ).

m(er,       er_vp_adverb,      er).

m(waar,             er_wh_loc_adverb,          waar            ).
m('waar de fock',   er_wh_loc_adverb,          [waar,de,fock]  ).
m('waar de fuck',   er_wh_loc_adverb,          [waar,de,fuck]  ).
m('waar de fuk',    er_wh_loc_adverb,          [waar,de,fuk]   ).
m('waar de hell',   er_wh_loc_adverb,          [waar,de,hell]  ).
m('waar the fock',  er_wh_loc_adverb,          [waar,the,fock] ).
m('waar the fuck',  er_wh_loc_adverb,          [waar,the,fuck] ).
m('waar the fuk',   er_wh_loc_adverb,          [waar,the,fuk]  ).
m('waar the hell',  er_wh_loc_adverb,          [waar,the,hell] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m(het,              het_noun, het).
m(dat,              cleft_het_noun, dat).
m(dit,              cleft_het_noun,dit).

m(aanvang,          tmp_app_noun,aanvang).
m(begin,            tmp_app_noun,begin).
m(eind,             tmp_app_noun,eind).
m(halfweg,          tmp_app_noun,halfweg).
m(midden,           tmp_app_noun,midden).
m(eerder,           tmp_app_noun,eerder).
m(half,             tmp_app_noun,half).
m(later,            tmp_app_noun,later).       % later die dag
m(medio,            tmp_app_noun,medio).
m(ultimo,           tmp_app_noun,ultimo).

%% voor het eerst deze week
%% voor het laatst dit seizoen
m('voor het eerst', tmp_app_noun,[voor,Het,eerst]) :- het(Het).
m('voor het aller_eerst', tmp_app_noun,[voor,Het,allereerst]) :- het(Het).
m('voor het eerst', tmp_app_noun,[voor,eerst]). % VL
m('voor het laatst',tmp_app_noun,[voor,Het,laatst]) :- het(Het).
m('voor het aller_laatst',tmp_app_noun,[voor,Het,allerlaatst]) :- het(Het).

%% niets dan ellende
m(niets,comp_noun(het,mass,sg,dan),niets).
m(niets,comp_noun(het,mass,sg,dan),niks).

% TODO: extraposition
% er treedt niemand minder op dan Jasparina de Jong
% omdat dit niets minder was dan een drama
%with_dt([niemand,minder],
%	comp_noun(de,mass,sg,dan),
%	dt(np,[hd=l(niemand,pronoun(nwh,thi,sg,de,both,indef,strpro),0,1),
%	       mod=dt(ap,[obcomp=orig(obcomp),
%			  hd=l(min,adjective(er(adv)),ap,1,2)])])).


m('niets minder dan', modal_adverb(noun),[niets,minder,dan]).
m('niemand minder dan', modal_adverb(noun),[niemand,minder,dan]).
m('niets minder als', modal_adverb(noun),[niets,minder,als]).
m('niemand minder als', modal_adverb(noun),[niemand,minder,als]).

%% op zich 't idee vind ik wel leuk
with_dt([op,zich],
	modal_adverb(noun),
	dt(pp,[hd=l(op,preposition(op,[af,na]),pp,0,1),
	       obj1=l(zich,pronoun(nwh,thi,both,de,dat_acc,def,wkpro),np,1,2)
	      ])).

%% 'dit keer' should be impossible, since 'keer' is 'de'-word!
with_dt([dit,keer],
	tmp_np,
	dt(np,[det=l(dit,determiner(het,nwh,nmod,pro,nparg),detp,0,1),
	       hd=l(keer,tmp_noun(de,count,meas),1,2)
	      ])).

with_dt([dit,maal],
	tmp_np,
	dt(np,[det=l(dit,determiner(het,nwh,nmod,pro,nparg),detp,0,1),
	       hd=l(maal,tmp_noun(de,count,meas),1,2)
	      ])).

%% VL, kort voor 'een dezer dagen'
with_dt([een,dezer],
	tmp_np,
	dt(np,[hd=l(één,pronoun(nwh,thi,sg,de,both,indef,strpro),np,0,1),
	       mod=l(deze,determiner(der),detp,1,2)
	      ])).

%% geloof in eigen kunnen
%% not produced because n->adj n forbids deverbal n, to reduce spur.amb. in
%% 'ziek zijn' etc.
with_dt([eigen,kunnen],
	noun(het,mass,sg),
	dt(np,[mod=l(eigen,adjective(no_e(nonadv)),ap,0,1),
	       hd=l(kunnen,v_noun(intransitive),1,2)
	      ])).

with_dt([eigen,zeggen],
	noun(het,mass,sg),
	dt(np,[mod=l(eigen,adjective(no_e(nonadv)),ap,0,1),
	       hd=l(zeggen,v_noun(intransitive),1,2)
	      ])).

with_dt([slapen,gaan],
	noun(het,mass,sg),
	dt(np,[vc=l(v_root(slaap,slapen),v_noun(intransitive),inf,0,1),
	       hd=l(ga,v_noun(aux(inf)),1,2)])).


m(M,post_p(W),W) :-
    postposition(W),
    stem_from_surf(W,M).

postposition(achterna).
postposition(af).
postposition(binnen).
postposition(door).
postposition(in).
postposition(langs).
postposition(om).
postposition(op).
postposition(over).
postposition(rond).
postposition(tegemoet).
postposition(uit).
postposition(voorbij).

%% zo niet , dan ga ik naar Utrecht

m('indien neen', sbar, [indien,neen]).
m('indien niet', sbar, [indien,niet]).
m('indien ja',   sbar,[indien,ja]).

m('zo ja',   sbar,[zo,ja]).
m('zo nee',  sbar,[zo,nee]).
m('zo neen', sbar,[zo,neen]).
m('zo niet', sbar,[zo,niet]).
m('zo niet', sbar,zoniet).

m('op=op',max,'op=op').
m('pecunia non olet',max,[pecunia,non,olet]).

with_dt([geen,denken,aan],
	max,
	dt(np,[det=l(geen,determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),detp,0,1),
	       hd=l(v_root(denk,denken),v_noun(pc_pp(aan)),1,2),
	       pc=l(aan,preposition(aan,[]),pp,2,3)])).

with_dt([geen,denken,aan],
	vandaar_adverb,
	dt(np,[det=l(geen,determiner(geen,nwh,mod,pro,yparg,nwkpro,geen),detp,0,1),
	       hd=l(v_root(denk,denken),v_noun(pc_pp(aan)),1,2),
	       pc=l(aan,preposition(aan,[]),pp,2,3)])).

with_dt([beter,ten,halve,gekeerd,dan,ten,hele,gedwaald],
	max,
	dt(du,[dp=dt(ap,[hd=l(goed,adjective(er(adv)),0,1),
			 obcomp=dt(cp,
				   [cmp=l(dan,comparative(dan),cp,4,5),
				    body=dt(ppart,[hd=l(v_root(dwaal,dwalen),verb(hebben,psp,intransitive),7,8),
						   mod=l('ten hele',adverb,advp,5,7)])])
			]),
	       dp=dt(ppart,[hd=l(v_root(keer,keren),verb(zijn,psp,intransitive),3,4),
			    mod=l('ten halve',adverb,advp,1,3)])])).

with_dt([beter,ten,halve,gekeerd,',',dan,ten,hele,gedwaald],
	max,
	dt(du,[dp=dt(ap,[hd=l(goed,adjective(er(adv)),0,1),
			 obcomp=dt(cp,
				   [cmp=l(dan,comparative(dan),cp,5,6),
				    body=dt(ppart,[hd=l(v_root(dwaal,dwalen),verb(hebben,psp,intransitive),8,9),
						   mod=l('ten hele',adverb,advp,6,8)])])
			]),
	       dp=dt(ppart,[hd=l(v_root(keer,keren),verb(zijn,psp,intransitive),3,4),
			    mod=l('ten halve',adverb,advp,1,3)])])).


with_dt([onbekend,maakt,onbemind],
	max,
	dt(smain,[hd=l(v_root(maak,maken),verb(hebben,sg3,ap_copula),1,2),
		  su=l(onbekend,adjective(no_e(nonadv)),ap,0,1),
		  predc=l(onbemind,adjective(no_e(padv)),ap,2,3)
		  ])).

with_dt([bekend,maakt,bemind],
	max,
	dt(smain,[hd=l(v_root(maak,maken),verb(hebben,sg3,ap_copula),1,2),
		  su=l(bekend,adjective(no_e(nonadv)),ap,0,1),
		  predc=l(bemind,adjective(ge_no_e(padv)),ap,2,3)
		  ])).

with_dt([liefde,maakt,blind],
	max,
	dt(smain,[hd=l(v_root(maak,maken),verb(hebben,sg3,ap_copula),1,2),
		  su=l(liefde,noun(de,count,sg),np,0,1),
		  predc=l(blind,adjective(no_e(padv)),ap,2,3)
		 ])).

with_dt([jong,geleerd,is,oud,gedaan],
	max,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg_heeft,copula),2,3),
		  su=dt(ppart,[hd=l(leer,verb(hebben,psp,transitive),1,2),
			       predm=l(jong,adjective(no_e(padv)),ap,0,1)]),
		  predc=dt(ppart,[hd=l(v_root(doe,doen),verb(hebben,psp,transitive),4,5),
				  predm=l(oud,adjective(no_e(padv)),ap,3,4)])])).


with_dt([niet,geschoten,is,altijd,mis],
         max,
         dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg_heeft,copula),2,3),
                   su=dt(ppart,[hd=l(v_root(schiet,schieten),verb(hebben,psp,intransitive),1,2),
			        mod=l(niet,adverb,advp,0,1)]),
                   mod=l(altijd,sentence_adverb,advp,3,4),
                   predc=l(mis,adjective(pred(nonadv)),ap,4,5)])).

with_dt([u,vraagt,wij,draaien],
	max,
	dt(du,[dp=dt(smain,[hd=l(v_root(vraag,vragen),verb(hebben,sg3,intransitive),1,2),
			    su=l(u,pronoun(nwh,u,sg,de,both,def),np,0,1)]),
	       dp=dt(smain,[hd=l(v_root(draai,draaien),verb(hebben,pl,intransitive),3,4),
			    su=l(wij,pronoun(nwh,fir,pl,de,nom,def),np,2,3)])
	      ]
	  )).

with_dt([u,vraagt,',',wij,draaien],
	max,
	dt(du,[dp=dt(smain,[hd=l(v_root(vraag,vragen),verb(hebben,sg3,intransitive),1,2),
			    su=l(u,pronoun(nwh,u,sg,de,both,def),np,0,1)]),
	       dp=dt(smain,[hd=l(v_root(draai,draaien),verb(hebben,pl,intransitive),4,5),
			    su=l(wij,pronoun(nwh,fir,pl,de,nom,def),np,3,4)])
	      ]
	  )).

m(Stem,max,PN) :-
    max(PN),
    stem_from_surf(PN,Stem).

max([daarmee,basta]).
max([mij,niet,gezien]).
max([niet,om,Het,een,of,ander]) :- het(Het).
max([never,change,a,winning,team]).
max(['don\'t',change,a,winning,team]).
max(['what\'s',in,a,name]).
max([niets,mis,mee]).
max([niks,mis,mee]).
max([niets,op,tegen]).
max([niks,op,tegen]).
max([opgeruimd,staat,netjes]).

max([noblesse,oblige]).

with_dt([gebeurd,is,gebeurd],
	max,
	dt(smain,[hd=   l(v_root(ben,zijn),verb(zijn,sg1,copula),1,2),
		  su=   l(gebeur,verb(unacc,psp,intransitive),ppart,0,1),
		  predc=l(gebeur,verb(unacc,psp,intransitive),ppart,2,3)])).

with_dt([Vol,is,Vol],
	max,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg1,copula),1,2),
		  su=l(Vol,adjective(no_e(adv)),ap,0,1),
		  predc=l(Vol,adjective(no_e(adv)),ap,2,3)])) :-
    vol_adjective(Vol).

vol_adjective(binnen).
vol_adjective(vol).
vol_adjective(recht).
vol_adjective(krom).

with_dt([alleen,is,maar,alleen],
	max,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg1,copula),1,2),
		  su=l(alleen,adjective(pred(both)),ap,0,1),
                  mod=l(maar,adverb,advp,2,3),
		  predc=l(alleen,adjective(pred(both)),ap,3,4)])).

%% HACK; is productive :-(
with_dt([Leuk,is,anders],
	max,
	dt(smain,[hd=l(v_root(ben,zijn),verb(zijn,sg1,copula),1,2),
		  su=l(Leuk,adjective(no_e(adv)),ap,0,1),
		  predc=l(ander,adjective(anders),ap,2,3)])) :-
    is_anders(Leuk).

is_anders(leuk).
is_anders(makkelijk).
is_anders(mooi).
is_anders(plezant).
is_anders(prettig).

with_dt([moeilijk,gaat,ook],
        max,
        dt(smain,[hd=l(v_root(ga,gaan),verb(zijn,sg3,intransitive),1,2),
                  su=l(moeilijk,adjective(no_e(adv)),ap,0,1),
                  mod=l(ook,sentence_adverb,advp,2,3)
                 ])).

with_dt([dit,terzijde],
	max,
	dt(du,[dp=l(dit,determiner(het,nwh,nmod,pro,nparg),np,0,1),
	       dp=l(terzijde,adverb,advp,1,2)])).

with_dt([eerlijk,duurt,Het,langst],
	max,
	dt(smain,[hd=l(v_root(duur,duren),verb(hebben,sg3,adv_meas),1,2),
		  me=l(lang,adjective(het_st(tmpadv)),ap,2,4),
		  su=l(eerlijk,adjective(no_e(adv)),ap,0,1)])) :- het(Het).

%% ik ga denk maar
m(v_root(denk,denken),denk_ik,denk).

with_dt([me,dunkt],
	denk_ik,
	dt(sv1,[hd=l(v_root(dunk,dunken),verb(hebben,sg3,dip_sbar_subj_so_np_no_het),1,2),
		su=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,0,1)])).

with_dt([me,dunkt],
	denk_ik_dip,
	dt(sv1,[hd=l(v_root(dunk,dunken),verb(hebben,sg3,dip_sbar_subj_so_np_no_het),1,2),
		su=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,0,1)])).

with_dt([geloof,me],
	denk_ik,
	dt(sv1,[hd=l(v_root(geloof,geloven),verb(hebben,sg1,tr_sbar),0,1),
		su=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,1,2)])).

with_dt([denk,ik],
	denk_ik,
	dt(sv1,[hd=l(v_root(denk,denken),verb(hebben,sg1,tr_sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)])).

with_dt([hoop,ik],
	denk_ik,
	dt(sv1,[hd=l(v_root(hoop,hopen),verb(hebben,sg1,tr_sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)])).

with_dt([vermoed,ik],
	denk_ik,
	dt(sv1,[hd=l(v_root(vermoed,vermoeden),verb(hebben,sg1,tr_sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)])).

with_dt([vrees,ik],
	denk_ik,
	dt(sv1,[hd=l(v_root(vrees,vrezen),verb(hebben,sg1,sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)])).

with_dt([vind,ik],
	denk_ik,
	dt(sv1,[hd=l(v_root(vind,vinden),verb(hebben,sg1,tr_sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)])).

with_dt([neem,ik,aan],
	denk_ik,
	dt(sv1,[hd=l(v_root(neem_aan,aannemen),verb(hebben,sg1,tr_sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2),
		svp=l(aan,particle(aan),part,2,3)])).

with_dt([dunkt,me],
	denk_ik,
	dt(sv1,[hd=l(v_root(dunk,dunken),verb(hebben,sg1,dip_sbar_subj_so_np_no_het),0,1),
		su=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,1,2)])).

with_dt([dunkt,mij],
	denk_ik,
	dt(sv1,[hd=l(v_root(dunk,dunken),verb(hebben,sg1,dip_sbar_subj_so_np_no_het),0,1),
		su=l(mij,pronoun(nwh,fir,sg,de,dat_acc,def),np,1,2)])).

with_dt([geloof,ik],
	denk_ik,
	dt(sv1,[hd=l(v_root(geloof,geloven),verb(hebben,sg1,sbar),0,1),
		su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)])).

with_dt([lijkt,me],
	denk_ik,
	dt(sv1,[hd=l(v_root(lijk,lijken),verb(hebben,sg3,dip_sbar_subj_so_np_opt_het),0,1),
		 obj2=l(me,pronoun(nwh,fir,sg,de,dat_acc,def,wkpro),np,1,2)
		])).

with_dt([lijkt,mij],
	denk_ik,
	dt(sv1,[hd=l(v_root(lijk,lijken),verb(hebben,sg3,dip_sbar_subj_so_np_opt_het),0,1),
		 obj2=l(mij,pronoun(nwh,fir,sg,de,dat_acc,def),np,1,2)
		])).

with_dt([zou,ik,zeggen],
	denk_ik,
	dt(sv1,[hd=l(v_root(zal,zullen),verb(hebben,past(sg),aux(inf)),0,1),
		su=ix(SU,l(ik,pronoun(nwh,fir,sg,de,nom,def),np,1,2)),
		vc=dt(inf,[hd=l(v_root(zeg,zeggen),verb(hebben,inf,tr_sbar),2,3),
			   su=ix(SU)				  
			  ])
	       ])).

with_dt([onder,ons,gezegd,en,gezwegen],
	adverb,
	dt(conj,
	   [crd=l(en,conj(en),vg,3,4),
	    cnj=dt(ppart,
		   [mod=ix(O,dt(pp,[hd=l(onder,preposition(onder,[door,vandaan]),0,1),
				    obj1=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,1,2)])),
		    hd=l(v_root(zeg,zeggen),verb(hebben,psp,intransitive),2,3)]),
	    cnj=dt(ppart,
		   [mod=ix(O),
		    hd=l(v_root(zwijg,zwijgen),verb(hebben,psp,intransitive),4,5)])
	   ])).

with_dt([samen,uit,samen,thuis],
	max,
	dt(du,[dp=dt(du,[dp=l(samen,adjective(pred(padv)),ap,0,1),
			 dp=l(uit,adjective(pred(adv)),ap,1,2)]),
	       dp=dt(du,[dp=l(samen,adjective(pred(padv)),ap,2,3),
			 dp=l(thuis,loc_adverb,advp,3,4)])])).

with_dt([samen,uit,',',samen,thuis],
	max,
	dt(du,[dp=dt(du,[dp=l(samen,adjective(pred(padv)),ap,0,1),
			 dp=l(uit,adjective(pred(adv)),ap,1,2)]),
	       dp=dt(du,[dp=l(samen,adjective(pred(padv)),ap,3,4),
			 dp=l(thuis,loc_adverb,advp,4,5)])])).

with_dt([Een,man,Een,man,Een,woord,Een,woord],
	max,
	dt(du,[dp=dt(du,[dp=dt(np,[det=l(een,determiner(een),detp,0,1),
				   hd=l(man,meas_mod_noun(de,count,meas),1,2)]),
			 dp=dt(np,[det=l(een,determiner(een),detp,2,3),
				   hd=l(man,meas_mod_noun(de,count,meas),3,4)])]),
	       dp=dt(du,[dp=dt(np,[det=l(een,determiner(een),detp,4,5),
				   hd=l(woord,noun(het,count,sg),5,6)]),
			 dp=dt(np,[det=l(een,determiner(een),detp,6,7),
				   hd=l(woord,noun(het,count,sg),7,8)])])])) :-
    een(Een).

with_dt([Een,man,Een,man,',',Een,woord,Een,woord],
	max,
	dt(du,[dp=dt(du,[dp=dt(np,[det=l(een,determiner(een),detp,0,1),
				   hd=l(man,meas_mod_noun(de,count,meas),1,2)]),
			 dp=dt(np,[det=l(een,determiner(een),detp,2,3),
				   hd=l(man,meas_mod_noun(de,count,meas),3,4)])]),
	       dp=dt(du,[dp=dt(np,[det=l(een,determiner(een),detp,5,6),
				   hd=l(woord,noun(het,count,sg),6,7)]),
			 dp=dt(np,[det=l(een,determiner(een),detp,7,8),
				   hd=l(woord,noun(het,count,sg),8,9)])])])) :-
    een(Een).

with_dt([ere,wie,ere,toekomt],
	max,
	dt(du,[dp=l(ere,noun(de,mass,sg),np,0,1),
	       dp=dt(whrel,[rhd=ix(O,l(wie,pronoun(ywh,thi,both,de,both,indef),np,1,2)),
			    body=dt(ssub,[su=ix(O),
					  obj2=l(ere,noun(de,mass,sg),np,2,3),
					  hd=l(v_root(kom_toe,toe_komen),verb(zijn,sg3,ninv(so_np,part_so_np(toe))),3,4)])])])
       ).

with_dt([ere,wie,ere,toe,komt],
	max,
	dt(du,[dp=l(ere,noun(de,mass,sg),np,0,1),
	       dp=dt(whrel,[rhd=ix(O,l(wie,pronoun(ywh,thi,both,de,both,indef),np,1,2)),
			    body=dt(ssub,[su=ix(O),
					  obj2=l(ere,noun(de,mass,sg),np,2,3),
                                          svp=l(toe,particle(toe),part,3,4),
					  hd=l(v_root(kom_toe,toe_komen),verb(zijn,sg3,part_so_np(toe)),4,5)])])])
       ).


with_dt([eerst,zien,',',dan,geloven],
	max,
	dt(du,[dp=dt(du,[dp=l(eerst,adverb,advp,0,1),
			 dp=l(v_root(zie,zien),verb(hebben,inf,transitive),inf,1,2)]),
	       dp=dt(du,[dp=l(dan,adverb,cp,3,4),
			 dp=l(v_root(geloof,geloven),verb(hebben,inf,transitive),inf,4,5)])])).

with_dt([mij,best],
	max,
	dt(du,[dp=l(mij,pronoun(nwh,fir,sg,de,dat_acc,def),np,0,1),
	       dp=l(goed,adjective(st(adv)),ap,1,2)])).


with_dt([gelijke,monniken,',',gelijke,kappen],
        max,
        dt(du,[dp=dt(np,[hd=l(monnik,noun(de,count,pl),np,1,2),
                         mod=l(gelijk,adjective(e),ap,0,1)]),
               dp=dt(np,[hd=l(kap,noun(de,count,pl),np,4,5),
                         mod=l(gelijk,adjective(e),ap,3,4)])])
               ).

m('c\'est la vie',max,['c\'est',la,vie]).

%% TODO
%% zo gezegd, zo gedaan

m('petje af',noun(het,mass,sg),[petje,af]). % als "applaus"

with_dt([voor,wat,',',hoort,wat],
        max,
        dt(du,
           [dp=dt(pp,[hd=l(voor,preposition(voor,[]),0,1),
                      obj1=l(wat,pronoun(ywh,thi,sg,het,
                                         both,indef,nparg),np,1,2)]),
            dp=dt(sv1,[hd=l(v_root(hoor,horen),verb(hebben,sg3,transitive),3,4),
                       obj1=l(wat,pronoun(ywh,thi,sg,het,
                                          both,indef,nparg),np,4,5)])])).

with_dt([niet,dus],
        max,
        dt(du,[dp=l(niet,adverb,advp,0,1),
               dp=l(dus,sentence_adverb,advp,1,2)
	      ])).

with_dt([mooi,niet,dus],
        max,
        dt(du,[dp=l(mooi,adjective(no_e(adv)),ap,0,1),
               dp=l(niet,adverb,advp,1,2),
               dp=l(dus,sentence_adverb,advp,2,3)])).

m('Jan en alleman',np,['Jan',en,alleman]).
m('Jan en alleman',np,[jan,en,alleman]).

m('punt na punt',np,[punt,na,punt]).
m('punt op punt',np,[punt,op,punt]).

m('©',np,'©').

%% voor ieder/elk
m('wat wils',np,[wat,wils]).

with_dt([ons,kent,ons],
        np,
        dt(np,[hd=l(v_root(ken,kennen),verb(hebben,sg3,transitive),1,2),
               su=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,0,1),
               obj1=l(ons,pronoun(nwh,fir,pl,de,dat_acc,def),np,2,3)])).

with_dt([geloof,Het,of,niet],
        max,
        dt(conj,[cnj=dt(sv1,[hd=l(v_root(geloof,geloven),verb(hebben,sg1,transitive),0,1),
                             obj1=l(het,determiner(het,nwh,nmod,pro,
                                                   nparg,wkpro),detp,1,2)]),
                 crd=l(of,conj(of),vg,2,3),
                 cnj=l(niet,adverb,advp,3,4)
                ])) :- het(Het).

m('for president',      max, [for,president]).
m('take it or leave it',max, [take,it,or,leave,it]).
m('the show must go on',max, [the,show,must,go,on]).
m('the show must go on',max, [de,show,must,go,on]).
m('op = op',            max, [op,'=',op]).

with_dt([ik,zeg,doen],
	max,
	dt(du,[tag=dt(smain,[su=l(ik,pronoun(nwh,fir,sg,de,nom,def),np,0,1),
			     hd=l(v_root(zeg,zeggen),verb(hebben,sg1,tr_sbar),smain,1,2)
			    ]),
	       nucl=l(v_root(doe,doen),verb(hebben,inf(no_e),intransitive),inf,2,3)
	      ])
       ).

%% middelbaar
with_dt([het,lager,en,het,secundair,onderwijs],
	np,
	dt(conj,[cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
			    mod=l(laag,adjective(er(adv)),ap,1,2),
			    hd=ix(HD,l(onderwijs,noun(het,mass,sg),np,5,6))
			   ]),
		 crd=l(en,conj(en),vg,2,3),
		 cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,3,4),
			    mod=l(secundair,adjective(no_e(adv)),ap,4,5),
			    hd=ix(HD)
			   ])
		])).
with_dt([het,lager,en,het,middelbaar,onderwijs],
	np,
	dt(conj,[cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,0,1),
			    mod=l(laag,adjective(er(adv)),ap,1,2),
			    hd=ix(HD,l(onderwijs,noun(het,mass,sg),np,5,6))
			   ]),
		 crd=l(en,conj(en),vg,2,3),
		 cnj=dt(np,[det=l(het,determiner(het,nwh,nmod,pro,nparg,wkpro),detp,3,4),
			    mod=l(middelbaar,adjective(no_e(adv)),ap,4,5),
			    hd=ix(HD)
			   ])
		])).

with_dt([als,volgt],
	adjective(pred(adv)),
	dt(cp,[cmp=l(als,complementizer(als),cp,0,1),
	       body=l(v_root(volg,volgen),verb(hebben/zijn,sg3,intransitive),ssub,1,2)
	      ])).



m('SP.A',proper_name(sg,'ORG'),'sp.a').
m('SP.A',proper_name(sg,'ORG'),'Sp.a').

m(Stem, proper_name(sg,'MISC'), PN) :-
    term(PN),
    stem_from_surf(PN,Stem).

m(Stem, proper_name(both,'MISC'), PN) :-
    term(PN,_),
    stem_from_surf(PN,Stem).

m(Stem, proper_name(both,'MISC'), PL) :-
    term(PN,PL),
    stem_from_surf(PN,Stem).

m(Stem, proper_name(both,'MISC'), PL) :-
    term_pl(PL),
    stem_from_surf(PL,Stem).

%% geanonimizeerde teksten waar [ eiser ] of [ gedaagde ] in voorkomen
m(Stem,proper_name(sg,'PER'), ['[',Anom,']']) :-
    anom_per(Anom),
    stem_from_surf(['[',Anom,']'],Stem).

m(Stem,proper_name(sg,'ORG'), ['[',Anom,']']) :-
    anom_org(Anom),
    stem_from_surf(['[',Anom,']'],Stem).

m(Stem,proper_name(sg,'LOC'), ['[',Anom,']']) :-
    anom_loc(Anom),
    stem_from_surf(['[',Anom,']'],Stem).

anom_loc(woonplaats).

anom_per(appellant).
anom_per(belanghebbende).
anom_per(eiser).
anom_per(eiseres).
anom_per(gedaagde).
anom_per(vergunninghouder).
anom_per(verzoekster).
anom_per(wederpartij).

anom_org(bedrijf).

:- discontiguous
    term/1,
    term/2.

term([accident,de,parcours],     [accidents,de,parcours]).
term([accumulate]).
term([air,support]).
term([alive,and,kicking]).
term([all,in,the,game]).
term([all,time,favorite],        [all,time,favorites]).
term([all,time,favourite],       [all,time,favourites]).
term([all,time,hero]).
term([all,time,high],            [all,time,highs]).
term([all,time,low],             [all,time,lows]).
term([all,time,record]).
term([alma,mater]).
term(['American',football]).
term([amicus,curiae],            [amici,curiae]).
term([annus,horribilis]).
term(appelation).
term([appelation,controlée]).
term([appelation,'d\'origine',controlée]).
term([avviso,di,garanzia]).
term([back,to,basics]).
term(balsamico).
term([basso,continuo]).
term([beau,monde]).
term([beat,'\'m',up]).
term([beat,'\'em',up]).
term([beat,'\'',m,up]).
term([beat,'\'',em,up]).
term([best,practice],[best,practices]).
term([bible,belt]).
term(['Bible',belt]).
term([big,business]).
term([big,deal]).
term([big,government]).
term([big,thing]).
term([billing,contact]).
term([black,box]).
term([black,spot]).
term([blind,date]).
term([bloody,shame]).
term([body,mass,index]).
term([born,to,be,wild]).
term([brain,drain]).
term([breaking,news]).
term(breedband).
term([bull,bar]).
term([business,as,usual]).
term([business,class]).
term([business,to,business]).
term([business,manager],[business,managers]).
term([buy]).
term([buy,out]).
term([carpe,diem]).
term([carte,blanche]).
term([carte,de,visite],[cartes,de,visite]).
term([cause,célèbre]).
term(['census-designated',place],['census-designated',places]).
term([ceremonie,protocollaire]).
term([chambre,de,réflexion]).
term([chambre,'d\'hôtes']).
term([champignons,de,'Paris']).
term([chapelle,ardente]).
term([checks,and,balances]).
term([chill,out]).
term([circumstantial,evidence]).
term([cliff,hanger]).
term([close,air,support]).
term([codex,ethicus]).
term([cold,cases]).
term([collateral,damage]).
term([comeback,kid]).
term([coming,out]).
term([commedia,del,arte]).
term([common,sense]).
term([conditio,sine,qua,non]).
term([condition,humaine]).
term([contradictio,in,terminis]).
term([cordon,sanitaire]).
term([cordon,sanitair]).
term([core,business]).
term([corporate,'&',finance]).
term([corpus,juris]).
term([couleur,locale]).
term([cuvée,de,prestige]).
term([damage,control]).
term([danse,macabre]).
term([das,war,einmal]).
term([debt,service,ratio]).
term(['debt-service',ratio]).
term([des,poedels,kern]).
term([deus,ex,machina]).
term(['director\'s',cut]).
term([dolce,far,niente]).
term([drag,and,drop]).
term([drag,'&',drop]).
term([dramatis,personae]).
term([drive,through,penalty],[drive,through,penalties]).
term([droit,de,regard]).
term([droit,de,suite]).
term([easy,listening]).
term([economy,class]).
term([emerging,market],[emerging,markets]).
term([eminance,grise]).
term([eminence,grise]).
term([enfant,terrible]).
term([equality,of,arms]).
term([esprit,de,corps]).
term([every,inch,a,gentleman]).
term([fact,of,life]).
term([fait,accompli]).
term([fair,play]).
term([fair,trade]).
term([fair,use]).
term([fellow,traveller],[fellow,travellers]).
term([fellow,traveler],[fellow,travelers]).
term([fin,de,siècle]).
term([fine,fleur]).
term([finest,hour]).
term([finishing,touch]).
term([flat,fee]).
term([fleur,de,lis]).
term([free,publicity]).
term([frequent,flyer],[frequent,flyers]).
term([friendly,fire]).
term([front,running]).
term([full,colour]).
term([gallery,play]).
term([game,set,and,match]).
term([game,',',set,and,match]).
term([gefundenes,'Fressen']).
term(['gentleman\'s',agreement]).
term([glitter,and,glamour]).
term([global,village]).
term([golden,parachute],[golden,parachutes]).
term([golden,score]).
term([good,governance]).
term([ground,zero]).
term([haasje,over]).   % otherwise we get the parse "[[haasje] [over spelen]]"
term([hall,of,fame]).
term([hard,court]).
term(hardcourt).
term([haute,couture]).
term([haute,cuisine]).
term([haute,finance]).
term(heartbeating).
term([hell,of,a,job]).
term([himmelhoch,jauchzend]).
term([hit,and,run]).
term([hold]).
term([hot,news]).
term([hot,stuff]).
term([human,interest]).
term([incompatibilité,'d\'humeur']).
term([incrowd]).
term([inner,circle]).
term([insta,love]).
term([insuline,lispro]).
term([jalousie,de,métier]).
term([jeune,homme]).
term([jeune,premier]).
term([jus,sanguinis]).
term([jus,soli]).
term([kick,and,rush]).
term([kiss,'&',ride]).
term([kiss,and,ride]).
term([kiss,en,ride]).
term(['l\'art',pour,'l\'art']).
term([la,condition,humaine]).
term([laisser,faire]).
term([last,in,first,out]).
term([last,in,',',first,out]).
term([last,in,last,out]).
term([last,in,',',last,out]).
term([law,and,order]).
term([le,condition,humaine]).
term([learning,curve]).
term([lees,meer]).  % klik op lees meer
term([lees,verder]).  % klik op lees meer
term(['let\'s',make,things,better]).
term(['Let\'s',make,things,better]).
term([leveraged,buy,out]).
term([lex,specialis]).
term([liber,amicorum]).
term([licence,to,kill]).
term([limbo,de,muje]). % traditioneel reinigingsritueel
term([low,profile]).
term([man,of,the,match]).
term([management,buy,out]).
term([management,by,speech]).
term([mea,culpa]).
term([memento,mori]).
term([ménage,à,trois]).
term([meet,and,greet],[meet,and,greets]).
term([method,acting]).
term([middle,class]).
term([middle,of,the,road]).
term(['mid-term','review']).
term([mission,impossible]).
term([monologue,intérieur]).
term([money,time]).
term([narrow,escape]).
term([ne,bis,in,idem]).
term([nec,plus,ultra]).
term([new,age]).
term([next,big,thing]).
term([no,cure,no,pay]).
term([no,cure,',',no,pay]).
term([no,guts,no,glory]).
term([no,guts,',',no,glory]).
term([no,nonsense]).
term('no-nonsense').
term([noblesse,oblige]).
term([nom,de,guerre]).
term([nouvelle,cuisine]).
term([nouvelle,vague]).
term([numerus,clausus]).
term([oboe,da,caccia]).
term([onderzoek,'&',ontwikkeling]).
term([off,shore]).
term('off-shore').
term([old,'boys-network']).
term([oom,agent]).
term([on,hold]).
term([one,man,one,vote]).
term([one,of,the,boys]).
term([one,size,fits,all]).
term([open,end],'open-end').
term([opting,out]).
term([oral,history]).
term([out,of,competition]).
term('out-of-competition').
term([own,goal],[own,goals]).
term([pais,en,vree]).
term([peis,en,vree]).               % VL
term([park,and,ride]).
term([pars,pro,toto]).
term([part,of,the,deal]).
term([pas,de,deux]).
term([pay,per,view]).
term(pdf).
term([perfect,fit]).
term([petite,histoire]).
term([the,place,to,be]).
term([piece,of,cake]).
term([place,to,be]).
term([plea,bargain]).
term([plug,and,play]).
term([plug,en,play]).
term([plug,'&',play]).
term([pluralis,majestatis]).
term([poésie,pure]).
term([point,of,no,return]).
term([policy,mix]).
term(postscript).
term([primus,inter,pares]).
term([private,banker],[private,bankers]).
term([quality,time]).
term([quick,fix]).
term([quid,pro,quo]).  % Clarice!
term([raison,'d\'état']).
term([raison,'d\'être']).
term([raison,'d\'','être']).
term([raison,'d\'être']).
term([rank,and,file]).
term([real,audio]).
term([reculer,pour,mieux,sauter]).
term([reduce]).
term([remedial,teaching]).
term([res,publica]).
term([rigor,mortis]).
term([rite,de,passage],[rites,de,passage]).
term([rule,of,law]).
term([rules,of,engagement]).
term([running,gag]).
term([sabbatical,year]).
term(sabbatical).
term([safe,haven],[safe,havens]).
term([safety,play]). % bridge
term([salary,cap]).
term([savoir,faire]).
term('savoir-faire').
term([savoir,vivre]).
term('savoir-vivre').
term(['self-fulfilling',prophecy]).
term([selffulfilling,prophecy]).
term(['self-fulfilling',prophecy]).
term([selffullfilling,prophecy]).
term(['self-fulfilling',prophecy]).
term(['self-fullfilling',prophecy]).
term([sentimental,journey]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,and,drugs,and,rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,and,rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,and,rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,and,rock,'\'n',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,and,rock,'\'n',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,and,rock,'\'n\'',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,and,rock,'\'n\'',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,en,drugs,en,rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,en,rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,en,rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,en,rock,'\'n',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,en,rock,'\'n',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,en,rock,'\'n\'',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,en,rock,'\'n\'',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,en,drugs,'&',rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,'&',rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,'&',rock,'&',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,'&',rock,'\'n',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,'&',rock,'\'n',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,',',drugs,'&',rock,'\'n\'',roll]).
term([sex,and,drugs,and,rock,and,roll]).
term([sex,'&',drugs,'&',rock,'\'n\'',roll]).
term([shoot,'\'m',up]).
term([shoot,'\'em',up]).
term([shoot,'\'',m,up]).
term([shoot,'\'',em,up]).
term([sine,qua,non]).
term([slip,of,the,pen]).
term([slip,of,the,tongue]).
term([small,cap],[small,caps]).
term(småort). % dorp in Zweden
term([soul,jazz]).
term([soft,skill],[soft,skills]).
term([so,far,so,good]).
term([so,far,',',so,good]).
term([special,issue]).
term([splendid,isoalation]).
term([state,of,the,art]).
term([status,epilepticus]).
term([street,credibility]).
term([struggle,for,life]).
term([sudden,death]).
term([sur,place]).
term([survival,of,the,fittest]).
term([swinging,sixties]).
term([tableau,de,la,troupe]).
term([take,it,or,leave,it]).
term([talent,management]).
term([tempo,doeloe]).
term([terra,incognita]).
term([the,name,of,the,game]).
term([the,next,big,thing]).
term([the,real,thing]).
term([the,show,must,go,on]).
term([time,slot],[time,slots]).
term([tongue,in,cheek]).
term([total,loss]).
term([trade,off]).
term(['trade-off']).
term([tour,de,chante]).
term([tour,de,force]).
term([tour,of,duty]).
term([tout,le,monde]).
term([tunnel,vision]).
term([turn,around]).
term([ultimum,remedium]).  % final solution?
term([up,or,out]).   % carriere maken of bedrijf verlaten
term([upper,class]).
term([usual,suspects]).
term([uti,possidetis,juris]).
term([u,vraagt,wij,draaien]).
term([u,vraagt,',',wij,draaien]).
term([vol,au,vent]).
term([vox,populi]).
term([war,on,drugs]).
term([war,on,terror]).
term([welles,nietes]).
term([welles,'-',nietes]).
term([welles,'/',nietes]).
term(['welles-nietes']).
term(['welles/nietes']).
term([wet,lease]).
term([white,trash]).
term([winner,takes,all]).
term([winning,goal],       [winning,goals]).
term([wishful,thinking]).
term([work,in,progress]).
term([working,class]).
term([working,class,hero]).
term([worst,case,scenario],[worst,case,'scenario\'s']).
term([worst,case,scenario],[worst,'case-scenario\'s']).
term([worst,case,scenario],[worst,'case-scenario']).
term(['writer\'s',block]).
term([zero,sum,game]).
term([zero,tolerance]).
term([zum,'Tode',betrübt]).

term_pl([bad,guys]).
term_pl([the,bad,guys]).
term_pl(['do\'s',and,'don\'ts']).
term_pl([family,values]).
term_pl([good,guys]).
term_pl([the,good,guys]).
term_pl([good,guys,and,bad,guys]).
term_pl([the,good,guys,and,the,bad,guys]).
term_pl([hard,feelings]).
term_pl([happy,few]).
term_pl([heat,shock,proteins]).
term_pl([no,hard,feelings]).
term_pl([social,media]).
term_pl([sans]).
term_pl([sans,atout]).
term_pl(['Sans']).
term_pl(['Sans',atout]).
term_pl(['Sans','Atout']).

%% mwu inf_only verbs
inf_only_sport([inline,skaten]).
inf_only_sport(['in-line',skaten]).

inf_only_cat(v_noun(intransitive)).
inf_only_cat(verb(hebben,inf,intransitive)).
inf_only_cat(verb(hebben,pl,intransitive)).

m(Stem,Cat,Surf) :-
    inf_only_cat(Cat),
    inf_only_sport(Surf),
    stem_from_surf(Surf,Stem).

%% all others done in unknowns.pl
%% but this one is requires for spell correction "de voor en nadelen"
m('heen-',within_word_conjunct,'heen-').
m('voor-',within_word_conjunct,'voor-').

m(datti,complementizer(datti),datti).
m(datti,complementizer(datti),'dat-i').
m(datti,complementizer(datti),'dat-ie').
m(datti,complementizer(datti),dattie).
m(datti,complementizer(datti),datie).

%%% these are the most frequent productive with_dt phrases.
%%% they are included here for the sole purpose of generation
%%% these productive with_dt cases otherwise cannot be generated
:- use_module(extra).

