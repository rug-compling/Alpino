% Trees

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(syn,parse(S,_),S).

graphic_label(syn,Node,Cat) :-
    node_label(Node,Cat).
graphic_label(syn,lex(Word),Word).

graphic_daughter(syn,No,Node,D):-
    Node => phrase,
    Node:dtrs ==> Ds,
    lists:nth(No,Ds,D).
graphic_daughter(syn,1,Node,lex(Word)):-
    Node => word,
    Node:morph ==> Word.

graphic_path(matrix(syn),parse(S,_),S).

graphic_label(matrix(syn),Node,Cat) :-
    Node:syn <=> Cat.
graphic_label(matrix(syn),lex(Word),Word).

graphic_daughter(matrix(syn),No,Node,D):-
    Node => phrase,
    Node:dtrs ==> Ds,
    lists:nth(No,Ds,D).
graphic_daughter(matrix(syn),1,Node,lex(Word)):-
    Node => word,
    Node:morph ==> Word.

graphic_path(prob,parse(_,P),P).

graphic_label(prob,Term,Term).

%%node_label(Node,Cat) :-
%%    Node:syn:head <=> Head,
%%    find_type(Head,HType),
%%    Node:syn:spr <=> Spr,
%%    find_type(Spr,SType),
%%    Node:syn:comps <=> Comps,
%%    find_type(Comps,CType),
%%    name(HType,SType,CType,Cat).

node_label(Node,Cat) :-
    Node:syn:head <=> Head,
    find_type(Head,[Cat]).

%%
%% Display features and weights
%%

show_model :-
  ( user:prob(Feat,maxent,Weight),
    format("~q ~q~n", [Feat,Weight]),
    fail
;
    true
).
