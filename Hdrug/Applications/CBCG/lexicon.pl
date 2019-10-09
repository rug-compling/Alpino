%%%%%%%%%%%%%%%%%%%%%%% lexicon proper %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proper_name(jan,jan).
proper_name(marie,marie).
proper_name(wie,'??').
proper_name(groningen,groningen).
proper_name(water,water).

pronoun(ik,sg1,nom,pro(sg,1)).
pronoun(mij,sg1,acc,pro(sg,1)).
pronoun(me,sg1,acc,pro(sg,1)).
pronoun(jij,sg(2),nom,pro(sg,2)).
pronoun(jou,sg(2),acc,pro(sg,2)).
pronoun(je,sg(2),acc,pro(sg,2)).
pronoun(hij,sg(3),nom,pro(sg,3,m)).
pronoun(hem,sg(3),acc,pro(sg,3,m)).
pronoun(zij,sg(3),nom,pro(sg,3,f)).
pronoun(ze,sg(3),nom,pro(sg,3,f)).
pronoun(haar,sg(3),acc,pro(sg,3,f)).
pronoun(wij,pl(1),nom,pro(pl,1)).
pronoun(we,pl(1),nom,pro(pl,1)).
pronoun(ons,pl(1),acc,pro(pl,1)).

noun([man,mannen],de,man).
noun([broer,broers],de,broer).
noun([vrouw,vrouwen],de,vrouw).
noun([zuster,zusters],de,zuster).
noun([meisje,meisjes],het,meisje).
noun([jongen,jongens],de,jongen).

noun([boek,boeken],het,boek).

adj([aardig,aardige],aardig).
adj([slim,slimme],slim).

det(de,_,de,_,unique).
det(het,sg(3),het,def,unique).
det(een,sg(3),_,indef,exist).
det(alle,pl(3),_,_,all).
det([welk,welke],which).

verb([slaap,slaapt,slapen,geslapen],iv,heb,slapen).
verb([val,valt,vallen,gevallen],iv,zijn,vallen).
verb([zing,zingt,zingen,gezongen],iv,zijn,zingen).     
  
verb([kus,kust,kussen,gekust],tv,heb,kussen).
verb([ken,kent,kennen,gekend],tv,heb,kennen).
verb([drink,drinkt,drinken,gedronken],tv,heb,drinken).
verb([lees,leest,lezen,gelezen],tv,heb,lezen).

verb([geef,geeft,geven,gegeven],dtv,heb,geven).

verb([houd,houdt,houden,gehouden],tvp(van),heb,houden_van).
verb([bel,belt,bellen,gebeld],tv_pref(op),heb,op_bellen).

verb([wil,wil,willen,willen],modal,heb,willen).		% ipp
verb([moet,moet,moeten,moeten],modal,heb,moeten).	

verb([schijn,schijnt,schijnen,xxxxx],s_raising,xxx,schijnen).

verb([probeer,probeert,proberen,proberen],modal_te,heb,proberen).

verb([zie,ziet,zien,zien],perc_verb,heb,zien).
verb([hoor,hoort,horen,gehoord],perc_verb,heb,horen).


verb([verklaar,verklaart,verklaren,verklaard],extra_verb(no_om),heb,verklaren).
verb([zeg,zegt,zeggen,gezegd],extra_verb(no_om),heb,zeggen).

verb([vraag,vraagt,vragen,gevraagd],trans_extra_verb(_),heb,vragen).

verb([probeer,probeert,proberen,geprobeerd],p_extra_verb,heb,proberen).
verb([verzuim,verzuimt,verzuimen,verzuimd],p_extra_verb,heb,verzuimen).

verb([heb,hebt,heeft,hebben,xxxxx],perf_aux(heb),xxx,perf).
verb([ben,bent,is,zijn,xxxxx],perf_aux(zijn),xxx,perf).

verb([beweer,beweert,beweren,beweerd],s_compl_verb,heb,beweren).
verb([denk,denkt,denken,gedacht],s_compl_verb,heb,denken).

preposition(van,van).
preposition(uit,uit).
preposition(aan,aan).

prefix(op).

rel_pronoun(dat,sg(3),het).		
rel_pronoun(die,sg(3),de).
rel_pronoun(die,pl(3),_).

rel_pronoun(wie,sg(3),de).
rel_pronoun(wie,pl(3),_).

rel_det(wiens).

adjunct(nooit,nooit).
adjunct(altijd,altijd).
adjunct(mogelijk,mogelijk).
