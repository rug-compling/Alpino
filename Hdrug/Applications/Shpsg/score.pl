:- module(score,[]).

feature(_,F,id(Id)) :-
    F:id <=> Id.

feature(maxent,F,dep(mod,HeadCat,ModCat)) :-
    F => phrase,
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr2:syn:head:mod <=> Dtr1:syn,
    Dtr1:syn:head <=> Head,
    find_type(Head,[HeadCat|_]),
    Dtr2:syn:head <=> Mod,
    find_type(Mod,[ModCat|_]).

feature(maxent,F,dep(spr,HeadCat,DepCat)) :-
    F => phrase,
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr2:syn:spr:h <=> Dtr1:syn,
    Dtr2:syn:head <=> Head,
    find_type(Head,[HeadCat|_]),
    Dtr1:syn:head <=> Dep,
    find_type(Dep,[DepCat|_]).

feature(maxent,F,dep(comp,HeadCat,DepCat)) :-
    F => phrase,
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr1:syn:comps:h <=> Dtr2:syn,
    Dtr1:syn:head <=> Head,
    find_type(Head,[HeadCat|_]),
    Dtr2:syn:head <=> Dep,
    find_type(Dep,[DepCat|_]).

feature(maxent,F,ldep(mod,H1,H2)) :-
    F => phrase,
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr2:syn:head:mod <=> Dtr1:syn,
    headof(Dtr1,H1),
    headof(Dtr2,H2).

feature(maxent,F,ldep(spr,H1,H2)) :-
    F => phrase,
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr2:syn:spr:h <=> Dtr1:syn,
    headof(Dtr1,H1),
    headof(Dtr2,H2).

feature(maxent,F,ldep(comp,H1,H2)) :-
    F => phrase,
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr1:syn:comps:h <=> Dtr2:syn,
    headof(Dtr1,H1),
    headof(Dtr2,H2).

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
