:- module(alpino_optrel, [ repair_opt/2, remove_opt/2 ]).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remove nodes that are optional %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Optional nodes are nodes that should be removed before generation.
%% Removal is performed in two steps:
%%
%% 1. The nodes marked optional are removed, as well as resulting
%%    non-lexical nodes with no daughters.
%% 2. We attempt to repair any irregularities in the tree caused
%%    by node removals.
%%
%% Todo:
%%
%% - Coindexing: where an indexed node is removed, the referencing
%%   node should also be removed.
%%

remove_opt(tree(Rel,Ds0),tree(Rel,Ds)) :-
    select_all(Ds0,tree(r(opt(_),_),_),Ds1),
    remove_opt_ds(Ds1,Ds).
remove_opt_ds(L0,L) :-
    findall(T,(lists:member(T0,L0),remove_opt(T0,T),non_empty_node(T)),L).

non_empty_node(tree(r(_Rel,p(_Cat)),Ds)) :-
    Ds \= [].
non_empty_node(tree(r(_Rel,adt_lex(_,_,_,_)),[])).
non_empty_node(tree(r(_Rel,i(_I)),[])).
non_empty_node(tree(r(_Rel,i(_I,_Lex)),_Ds)).

repair_opt(tree(Rel0,Ds0),tree(Rel,Ds)) :-
    opt_transform(tree(Rel0,Ds0),tree(Rel,Ds1)),
    repair_opt_ds(Ds1,Ds).

repair_opt_ds([],[]).
repair_opt_ds([H|T],[NewH|NewT]) :-
    repair_opt(H,NewH),
    repair_opt_ds(T,NewT).

%%    opt_transform(tree(Rel0,Ds2),tree(Rel,Ds)).

%% Conjunctions with only one conjunct.
opt_transform(tree(r(R,p(conj)),Ds0),tree(r(R,C),Ds)) :-
    length(Ds0,1),
    !,
    Ds0 = [tree(r(_,C),Ds)].
%% "in/1 de/1 periode/1 na/0 de/0 verkiezingen/0"
opt_transform(tree(r(R,p(pp)),[tree(r(obj1,p(np)),[tree(r(mod,p(pp)),PPDs)])]),
	      tree(r(R,p(pp)),PPDs)) :-
    !.
%% If an NP has no head, but does have a noun modifier, promote it
%% to a head.
opt_transform(tree(r(R,p(np)),Ds0),
	      tree(r(R,p(np)),[tree(r(hd,adt_lex(Root,Sense,_,_)),[])|Rest])) :-
    \+ lists:memberchk(tree(r(hd,_),_),Ds0),
    lists:select(tree(r(_,adt_lex(Root,Sense,noun,_Attrs)),[]),Ds0,Rest),
    !.
opt_transform(tree(r(R,p(np)),Ds0),
	      tree(r(R,p(np)),[tree(r(hd,adt_lex(Root,Sense,_,_)),[])|Rest])) :-
    \+ lists:memberchk(tree(r(hd,_),_),Ds0),
    lists:select(tree(r(_,adt_lex(Root,Sense,name,_Attrs)),[]),Ds0,Rest),
    !.
%% If we have an NP with only a remaining PP modifier, promote its obj1
%% to the head of the NP.
opt_transform(tree(r(R,p(np)),[tree(r(mod,p(pp)),PPDs)]),
	      tree(r(R,p(np)),NewDs)) :-
    member(tree(r(obj1,p(np)),NewDs),PPDs),
    !.
opt_transform(tree(r(R,i(I,p(np))),[tree(r(mod,p(pp)),PPDs)]),
	      tree(r(R,i(I,p(np))),NewDs)) :-
    member(tree(r(obj1,p(np)),NewDs),PPDs),
    !.
opt_transform(tree(r(R,p(np)),[tree(r(mod,p(pp)),PPDs)]),
	      tree(r(R,adt_lex(Root,Sense,Pos,Attrs)),[])) :-
    member(tree(r(obj1,adt_lex(Root,Sense,Pos,Attrs)),[]),PPDs),
    !.
opt_transform(T,T).

% Remove all elements of a list that unify with a term. Variables
% in that term will not be bound.
select_all([],_,[]).
select_all([H|T],E,NewT) :-
    copy_term(E,ECopy),
    H = ECopy,
    select_all(T,E,NewT).
select_all([H|T],E,[H|NewT]) :-
    H \= E,
    select_all(T,E,NewT).
