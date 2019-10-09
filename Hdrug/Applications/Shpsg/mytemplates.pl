%%
%% Feature templates for maxent models

:- dynamic user:template/2.

template(F,id(Id)) :-
    F:id <=> Id.

template(F,ids(Id,IdL,IdR)) :-
    F => phrase,
    F:id <=> Id, 
    F:dtrs <=> [ Dtr1, Dtr2 ],
    Dtr1:id <=> IdL,
    Dtr2:id <=> IdR.

%template(F,dep(mod,HeadCat,ModCat)) :-
%    F => phrase,
%    F:dtrs <=> [ Dtr1, Dtr2 ],
%    Dtr2:syn:head:mod <=> Dtr1:syn,
%    Dtr1:syn:head <=> Head,
%    get_type(Head,HeadCat),
%    Dtr2:syn:head <=> Mod,
%    get_type(Mod,ModCat).

%template(F,dep(spr,HeadCat,DepCat)) :-
%    F => phrase,
%    F:dtrs <=> [ Dtr1, Dtr2 ],
%    Dtr2:syn:spr:h <=> Dtr1:syn,
%    Dtr2:syn:head <=> Head,
%    get_type(Head,HeadCat),
%    Dtr1:syn:head <=> Dep,
%    get_type(Dep,DepCat).

%template(F,dep(comp,HeadCat,DepCat)) :-
%    F => phrase,
%    F:dtrs <=> [ Dtr1, Dtr2 ],
%    Dtr1:syn:comps:h <=> Dtr2:syn,
%    Dtr1:syn:head <=> Head,
%    get_type(Head,HeadCat),
%    Dtr2:syn:head <=> Dep,
%    get_type(Dep,DepCat).

%template(F,ldep(mod,H1,H2)) :-
%    F => phrase,
%    F:dtrs <=> [ Dtr1, Dtr2 ],
%    Dtr2:syn:head:mod <=> Dtr1:syn,
%    headof(Dtr1,H1),
%    headof(Dtr2,H2).

%template(F,ldep(spr,H1,H2)) :-
%    F => phrase,
%    F:dtrs <=> [ Dtr1, Dtr2 ],
%    Dtr2:syn:spr:h <=> Dtr1:syn,
%    headof(Dtr1,H1),
%    headof(Dtr2,H2).

%template(F,ldep(comp,H1,H2)) :-
%    F => phrase,
%    F:dtrs <=> [ Dtr1, Dtr2 ],
%    Dtr1:syn:comps:h <=> Dtr2:syn,
%    headof(Dtr1,H1),
%    headof(Dtr2,H2).

