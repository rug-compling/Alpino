%% Simple HPSG grammar

%% type signature

top([sign,syn,num,head,parse]).
type(parse,[],[sign,prob]).
type(sign,[word,phrase],[id,syn]).
type(word,[],[morph]).
type(phrase,[],[dtrs]).
type(syn,[],[head,spr,comps]).
type(head,[nhead,mhead],[]).
type(nhead,[v,d,n],[num]).
type(mhead,[p],[mod]).
at(v).
at(d).
at(n).
at(p).
type(num,[sg,pl],[]).
at(sg).
at(pl).

extensional(parse).

list_type(h,t).

:- type_compiler.

%% rules

rule_fs(Id,M,Dtrs) :-
    M => phrase,
    M:id ==> Id,
    M:dtrs ==> Dtrs.

head_feature_p(M,H) :-
    M:syn:head <=> H:syn:head.

rule(M,[D,H]) :-
    rule_fs(head_spr,M,[D,H]),
    head_feature_p(M,H),
    H:syn:comps => [],
    H:syn:spr:h <=> D:syn,
    H:syn:spr:t <=> M:syn:spr,
    H:syn:comps <=> M:syn:comps.

rule(M,[H,D]) :-
    rule_fs(head_comp,M,[H,D]),
    head_feature_p(M,H),
    H:syn:comps:h <=> D:syn,
    H:syn:comps:t <=> M:syn:comps,
    H:syn:spr <=> M:syn:spr.

rule(M,[H,D]) :-
    rule_fs(head_mod,M,[H,D]),
    head_feature_p(M,H),
    D:syn:head:mod <=> H:syn,
    M:syn:spr <=> H:syn:spr,
    M:syn:comps <=> H:syn:comps.

%% lexicon

sat_syn(F,Cat) :-
    F:head => Cat,
    F:spr => [],
    F:comps => [].

word_fs(X,F) :-
    F => word,
    F:morph ==> X.

spr_word(F,Cat) :-
    F:syn:spr <=> [Spr],
    sat_syn(Spr,Cat),
    Spr:head:num <=> F:syn:head:num.
    
det_fs(X,F) :-
    word_fs(X,F),
    F:syn:head => d,
    F:syn:comps => [],
    F:syn:spr => [].

noun_fs(X,F) :-
    word_fs(X,F),
    F:syn:head => n,
    spr_word(F,d),
    F:syn:comps => [].

verb_fs(X,F) :-
    word_fs(X,F),
    F:syn:head => v,
    spr_word(F,n).

iverb_fs(X,F) :-
    verb_fs(X,F),
    F:syn:comps => [].

tverb_fs(X,F) :-
    verb_fs(X,F),
    F:syn:comps <=> [Comp],
    sat_syn(Comp,n).

dverb_fs(X,F) :-
    verb_fs(X,F),
    F:syn:comps <=> [Comp1,Comp2],
    sat_syn(Comp1,n),
    sat_syn(Comp2,p).

prep_fs(X,F) :-
    word_fs(X,F),
    F:syn:head => p,
    F:syn:head:mod:spr:h => syn,
    F:syn:head:mod:comps => [],
    F:syn:comps <=> [Comp],
    sat_syn(Comp,n),
    F:syn:spr => [].

det(the,_).
det(a,sg).

noun(dog,sg).
noun(dogs,pl).
noun(cat,sg).
noun(cats,pl).
noun(park,sg).
noun(parks,pl).
noun(book,sg).
noun(books,pl).
noun(table,sg).
noun(tables,pl).
noun(woman,sg).
noun(women,pl).
noun(man,sg).
noun(men,pl).
noun(person,sg).
noun(people,pl).
noun(telescope,sg).
noun(telescopes,pl).
noun(fountain,sg).
noun(fountains,pl).

iverb(barks,sg).
iverb(barked,_).
iverb(bark,pl).

tverb(chases,sg).
tverb(chased,_).
tverb(chase,pl).
tverb(sees,sg).
tverb(saw,_).
tverb(see,pl).

dverb(puts,sg).
dverb(put,_).
dverb(put,pl).

prep(in).
prep(on).
prep(with).
prep(to).

number(Num,F) :-
    (
      ground(Num)
    ->
      F:syn:head:num => Num
    ;
      true
    ).

lexicon(W,F) :-
    det(W,Num),
    det_fs(W,F),
    number(Num,F).

lexicon(W,F) :-
    noun(W,Num),
    noun_fs(W,F),
    number(Num,F).

lexicon(W,F) :-
    iverb(W,Num),
    iverb_fs(W,F),
    number(Num,F).

lexicon(W,F) :-
    tverb(W,Num),
    tverb_fs(W,F),
    number(Num,F).

lexicon(W,F) :-
    dverb(W,Num),
    dverb_fs(W,F),
    number(Num,F).

lexicon(W,F) :-
    prep(W),
    prep_fs(W,F).

:- dynamic user:clexicon/2.

compile_lex :-
    (
      lexicon(W,F),
      hdrug_util:gen_sym(Id,W),
      F:id ==> Id,
      assert(clexicon(W,F)),
      fail
    ;
      true
    ).


:- retractall(clexicon(_,_)).
:- compile_lex.

%% features

:- compile(templates).

get_dtrs(F,Ds) :-
  ( 
      F => phrase
  ->
	F:dtrs <=> Ds
   ;
	Ds = []
).

get_type(F,T) :-
    find_type(F,[T|_]),!.

headof(F,H) :-
    F => word,
    F:morph <=> H.

headof(F,H) :-
    F => phrase,
    F:dtrs:h <=> D,
    F:syn:head <=> D:syn:head,
    !,
    headof(D,H).

headof(F,H) :-
    F => phrase,
    F:dtrs:t:h <=> D,
    F:syn:head <=> D:syn:head,
    !,
    headof(D,H).

%%
%% Treebank stuff
%%

:- ensure_loaded(suite).
:- ensure_loaded(treebank).

sentence(N,Sent) :-
    sentence(N,test,Sent).

