:- module(packing, [ divide_synsem/4,
	             apply_semantics/4
                   ]).

:- use_module(database).


% divide_synsem(+M,+Ds,-M1,-Ds1)
% a copy of the rule where `semantics' is ignored
% expects predicate user:ignore_semantics/2

divide_synsem(M,Ds,SynM,SynDs) :-
	dsds([M|Ds],[SynM|SynDs]).

dsds([],[]).
dsds([H|T],[Hsyn|Tsyn]):-
	user:ignore_semantics(H,Hsyn),
	dsds(T,Tsyn).


% apply_semantics(Cat,P0,P,Module)
% builds results on the basis of packed representation,
% using the rules WITH the semantics.
% It is assumed that in Module the predicate tt represents
% the packed forest, such that tt(P0,P,Name,Daughters), where
% P0, P start/end positions, Name is rulename, and Daughters is
% a list of t(P0,P,Cat) triples

apply_semantics(Mother,P0,P,Module):-
	clean_up_database(packing:a_item(_,_,_,_,_)),
	clean_up_database(packing:p_item(_,_,_)),
	initialize(P0,P,Mother,Module),
	left_right(P0,P,Module),
	p_item(P0,P,Mother).

initialize(P0,P,Mother,Module):-
	( store([t(P0,P,Mother)],P0,P0,'$$$',Module)
        ; true
        ).

left_right(P,P,_):-!.
left_right(P0,P,Module):-
	P1 is P0 + 1,
	( user:lex(P0,P1,Small,_Name),
	  store([],P0,P1,t(P0,P1,Small),Module)
        ; left_right(P1,P,Module)
        ).

store([],P0,P,t(P0,P,Mother),Module):-
	assertz_most_general(packing:p_item(P0,P,Mother),no),
	closure(p_item(P0,P,Mother),Module).
store([H|T],P0,P,Mother,Module):-
	assertz_most_general(packing:a_item(P0,P,H,T,Mother),no),
	closure(a_item(P0,P,H,T,Mother),Module).

match([],[]).
match([t(_,_,H)|T],[H|T2]):-
	match(T,T2).

% prediction
closure(a_item(_P0,P1,t(P1,P,H),_T,_Mother),Module):-
	Module:tt(P1,P,Name,Ds),
	Module:pack_rule(Name,H,Ds2),
	match(Ds,Ds2),
	store(Ds,P1,P1,t(P1,P,H),Module),
	fail.

% completion
closure(a_item(P0,P,t(P,P,H),T,Mother),Module):-
	p_item(P,P,H),
	store(T,P0,P,Mother,Module),
	fail.

% completion
closure(p_item(P1,P,H),Module):-
	a_item(P0,P1,t(P1,P,H),T,Mother),
	store(T,P0,P,Mother,Module),
	fail.


