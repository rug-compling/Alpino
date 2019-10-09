stem_ens(boek,  boeken).
stem_ens(meisje,meisjes).
stem_ens(paard, paarden).
stem_ens(kind,  kinderen).
stem_ens(krant, kranten).
stem_ens(jongen,jongens).
stem_ens(man,   mannen).
stem_ens(vrouw, vrouwen).

stem_e(aardig,aardige).
stem_e(lief,lieve).
stem_e(vervelend,vervelende).

stem_t(beweer,beweert).
stem_t(schep,schept).
stem_t(reken,rekent).
stem_t(heb,heeft).
stem_t(wil,wil).
stem_t(kan,kan).
stem_t(zie,ziet).
stem_t(slaap,slaapt).
stem_t(lees,leest).
stem_t(probeer,probeert).
stem_t(geef,geeft).
stem_t(schijn,schijnt).
stem_t(overreed,overreedt).
stem_t(beloof,belooft).
stem_t(zeg,zegt).
stem_t(bel,belt).
stem_t(loop,loopt).
stem_t(kus,kust).
stem_t(sla,slaat).
stem_t(vind,vindt).
stem_t(meen,meent).

ge_stem_d(vind,gevonden).
ge_stem_d(beweer,beweerd).
ge_stem_d(schep,geschept).
ge_stem_d(reken,gerekend).
%% ge_stem_d(heb,gehad).
ge_stem_d(wil,gewild).
ge_stem_d(kan,gekund).
ge_stem_d(zie,gezien).
ge_stem_d(slaap,geslapen).
ge_stem_d(lees,gelezen).
ge_stem_d(probeer,geprobeerd).
ge_stem_d(geef,gegeven).
%% ge_stem_d(schijn,geschenen).
ge_stem_d(overreed,overreed).
ge_stem_d(beloof,beloofd).
ge_stem_d(zeg,gezegd).
ge_stem_d(bel,gebeld).
ge_stem_d(loop,gelopen).
ge_stem_d(kus,gekust).
ge_stem_d(sla,geslagen).
ge_stem_d(meen,gemeend).

stem_en(vind,vinden).
stem_en(beweer,beweren).
stem_en(schep,scheppen).
stem_en(reken,rekenen).
stem_en(heb,hebben).
stem_en(wil,willen).
stem_en(kan,kunnen).
stem_en(zie,zien).
stem_en(slaap,slapen).
stem_en(lees,lezen).
stem_en(probeer,proberen).
stem_en(geef,geven).
stem_en(schijn,schijnen).
stem_en(overreed,overreden).
stem_en(beloof,beloven).
stem_en(zeg,zeggen).
stem_en(bel,bellen).
stem_en(loop,lopen).
stem_en(kus,kussen).
stem_en(sla,slaan).
stem_en(meen,menen).

inflection(Stem,Word,Sign) :-  
	Sign:cat:vform => fin,
	Sign:fargs:h:sem:index => sg,
	stem_t(Stem,Word).

inflection(Stem,Word,Sign) :-
	Sign:cat:vform => inf,
	stem_en(Stem,Word).

inflection(Stem,Word,Sign) :-
	Sign:cat:vform => fin,
	Sign:fargs:h:sem:index => pl,
	stem_en(Stem,Word).

inflection(Stem,[te,Word],Sign) :-
	Sign:cat:vform => te,
	stem_en(Stem,Word).

inflection(Stem,Word,Sign) :-
	Sign:cat:vform => pas,
	ge_stem_d(Stem,Word).

