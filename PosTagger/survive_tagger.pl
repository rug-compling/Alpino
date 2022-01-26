:- module(alpino_survive_tagger, [ survive_tagger/2 ]).

:- expects_dialect(sicstus).

%% meant to keep tags for pos-tags or words that should not be removed
%% e.g. because recently changed/added in grammar.
survive_tagger([],[]).
survive_tagger([T|Ts],Rs) :-
    survive_tagger(T,Rs,Rs0),
    survive_tagger(Ts,Rs0).

survive_tagger(Ref,Rs0,Rs) :-
    instance(Ref,(Tag:-true)),
    (	survives(Tag)
    ->	Rs0=Rs,
	Tag = tag(_,_,_,_,W,_,_,T),
	hdrug_util:debug_message(4,"tag ~w of word ~w survives by rule~n",[W,T])
    ;	Rs0=[Ref|Rs]
    ).

survives(tag(_,_,_,_,W,_,_,_T)) :-
    surviving_root(W).
survives(tag(_,_,_,_,_,W,_,_T)) :-
    surviving_word(W).
survives(tag(_,_,_,_,_W,_,_,T)) :-
    surviving_tag(T).
survives(tag(_,_,_,_,W,_,_,T)) :-
    surviving_root_tag(W,T).
survives(tag(_,_,_,_,_,W,_,T)) :-
    surviving_word_tag(W,T).

survives(tag(_,_,_,R1,Root1,Word1,_,Tag1)) :-
    survives_trigram(Root1,Word1,Tag1,Root2,Word2,Tag2,Root3,Word3,Tag3),
    alpino_lexical_analysis:tag(_,_,R1,R2,Root2,Word2,_,Tag2),
    alpino_lexical_analysis:tag(_,_,R2,_, Root3,Word3,_,Tag3).
survives(tag(_,_,R1,R2,Root2,Word2,_,Tag2)) :-
    survives_trigram(Root1,Word1,Tag1,Root2,Word2,Tag2,Root3,Word3,Tag3),
    alpino_lexical_analysis:tag(_,_,_,R1,Root1,Word1,_,Tag1),
    alpino_lexical_analysis:tag(_,_,R2,_, Root3,Word3,_,Tag3).
survives(tag(_,_,R2,_,Root3,Word3,_,Tag3)) :-
    survives_trigram(Root1,Word1,Tag1,Root2,Word2,Tag2,Root3,Word3,Tag3),
    alpino_lexical_analysis:tag(_,_,_,R1,Root1,Word1,_,Tag1),
    alpino_lexical_analysis:tag(_,_,R1,R2,Root2,Word2,_,Tag2).

survives(tag(_,_,_,R1,Root1,Word1,_,Tag1)) :-
    survives_bigram(Root1,Word1,Tag1,Root2,Word2,Tag2),
    alpino_lexical_analysis:tag(_,_,R1,_,Root2,Word2,_,Tag2).

survives(tag(_,_,R1,_,Root2,Word2,_,Tag2)) :-
    survives_bigram(Root1,Word1,Tag1,Root2,Word2,Tag2),
    alpino_lexical_analysis:tag(_,_,_,R1,Root1,Word1,_,Tag1).

%% because "naar voren" is also fixed_part, it is often filtered out
survives_bigram(naar,_,preposition(naar,[toe],loc_adv),voren,_,loc_adverb).
%% wat later
survives_bigram(wat,_,adverb,laat,later,_).
%% blijft behouden
survives_bigram(v_root(blijf,blijven),_,verb(unacc,_,copula),behouden,_,adjective(ge_both(adv))).
%% voor bekeken
survives_bigram(voor,_,preposition(voor,[],voor_pred),bekeken,_,adjective(ge_both(adv))).
%% korte termijn is mwu too
survives_bigram(kort,korte,adjective(e),termijn,termijn,tmp_noun(de,count,sg)).
%% lange termijn is mwu too
survives_bigram(lang,lange,adjective(e),termijn,termijn,tmp_noun(de,count,sg)).
%% temp
survives_bigram(op,_,preposition(op,_),bezoek,_,tmp_noun(het,count,sg)).
survives_bigram(op,_,preposition(op,_),bezoek,_,tmp_noun(het,count,sg,pred_pp(op))).
%% temp
survives_bigram(het,_,determiner(het,nwh,nmod,pro,nparg,wkpro),laatste,_,number(rang)).

%% because "dames en heren" is also tag
survives_trigram(dame,_,noun(de,count,pl),en,_,conj(en),heer,_,noun(de,count,pl)).

%% because "zou ik zeggen" is also denk_ik
survives_trigram(_,zou,verb(hebben,past(sg),aux(inf)),_,ik,pronoun(nwh,fir,sg,de,nom,def),_,zeggen,verb(hebben,inf,transitive)).

%% because alles of niets is also adjective
survives_trigram(alles,_,noun(het,mass,sg),of,_,conj(of),niets,_,meas_mod_noun(het,mass,sg)).

surviving_word(_):-
    fail.

surviving_root('high tech').      % temp
surviving_root('naar schatting').
surviving_root('om het even').
surviving_root('heel wat').       % temp
surviving_root(maand).            % temp
surviving_root(tal).

surviving_root(_):-
    fail.

surviving_word_tag(zorgen,noun(de,count,pl)).

%% temp
surviving_word_tag(eten,noun(het,mass,sg)).

%% often wrong in novels
surviving_word_tag('Beiden',predm_adverb).
surviving_word_tag('Wat',adverb).

%% temp
surviving_word_tag(bezoek,tmp_noun(het,count,sg)).
surviving_word_tag(bezoek,tmp_noun(het,count,sg,pred_pp(op))).
surviving_word_tag(voorbij,adjective(both(adv))).

%surviving_word_tag(_,_):-
%    fail.

surviving_root_tag(_,_):-
    fail.

%% De ref wees terecht naar de stip en de strafschop werd omgezet
surviving_root_tag(terecht,adjective(_)).
surviving_root_tag(terecht,adjective(_,_)).

surviving_root_tag(aan,particle(aan)) :-
    alpino_lexical_analysis:tag(_,_,_,_,v_root(kondig_aan,aan_kondigen),_,_,_).

surviving_tag(_):-
    fail.

surviving_tag(reflexive(_,_)).

%% If there is no suitable verb (adjective), they will be removed anyway.
surviving_tag(fixed_part(_)).
surviving_tag(particle(_)).

surviving_tag(preposition(_,_,me_adj)).

%% temporarily
surviving_tag(particle('ten onder')).

%% finite verbs survive since if they appear in V2, their context is not
%% locally available
surviving_tag(verb(_,Fin,Sc)):-
    finite(Fin),
    alpino_tr_tag:t_subcat(Sc,aux(_)).

finite(sg1).
finite(sg3).
finite(sg).
finite(pl).
finite(sg_hebt).
finite(sg_heeft).
finite(sg_bent).
finite(sg_is).
finite(modal_not_u).
finite(modal_inv).
finite(past(_)).
finite(subjunctive).

