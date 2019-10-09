:- module(right_ch,[]).

:- use_module( lists, library(lists), [ append/3, reverse/2 ]).

:- use_module( database ).

clean :-
	clean_up_database(right_ch:inactive(_,_,_)),
	clean_up_database(right_ch:tt(_,_,_,_)).

parse(o(Node,String,_)) :-
	hdrug_util:hdrug_flag(ticks,_,0),
	bu_lr(String,0,Max),
	write('recover parse trees'), nl,
	packing:apply_semantics(Node,0,Max,right_ch).

bu_lr([],I,I).

bu_lr([_|T],I0,I):-
	lexical_lookup(I0,Agenda),
	process(Agenda),
	I1 is I0 + 1,
	bu_lr(T,I1,I).

lexical_lookup(I0,Agenda) :-
	findall(Edge,lookup(I0,Edge),Agenda).

lookup(I0,Edge):-
	user:ign_lex(I0,I,M,Name),
	store(M,I0,I,tree(Name,[]),Edge).

% process(+ListOfEdges)
% process a list of edges
process([]).
process([Edge|OldAgenda]):-
	process_one(Edge,SubAgenda),         %% process each one in turn
	append(SubAgenda,OldAgenda,Agenda),     %% depth first!
	process(Agenda).                     %% process the rest

% process_one(+Edge,-Agenda)
% depending on the form of Edge, builds all new edges from Edge 
% by prediction or completion
process_one(inactive(Cat,P0,P1),Agenda):-
	findall(Edge,completer(Cat,P0,P1,Edge),Agenda).

% completer(+Cat,+P1,+P,-Edge)
% selects active edge whose rightmost symbol matches Cat
completer(Cat,P1,P,Edge):-
	user:ign_r_rule(Cat,M,Lefties,MR),
	select_lefties(Lefties,P0,P1,[t(P1,P,Cat)],His),
	store(M,P0,P,tree(MR,His),Edge).   % store in database

select_lefties([],P,P,His,His).
select_lefties([H|T],P0,P,HisIn,HisOut):-
	inactive_or_gap(P,P1,H),
	select_lefties(T,P0,P1,[t(P1,P,H)|HisIn],HisOut).

inactive_or_gap(P,P1,H):-
	inactive(P,P1,H).
inactive_or_gap(P,P,H) :-
	user:ign_gap(H,Gap),
	assertz_most_general(right_ch:tt(P,P,Gap,[])).

% store(+M,+P0,+P,+Tree,-Edge)
% stores an edge, and depending on form gives different representation
% of the same edge back. If the edge already exists, then failure.
store(_,_,_,_,_):-
	hdrug_util:hdrug_flag(ticks,V),
	V2 is V+1,
	hdrug_util:hdrug_flag(ticks,_,V2),
	fail.
store(M,P0,P,tree(Rule,His),inactive(M,P0,P)):-
	assertz_most_general(right_ch:tt(P0,P,Rule,His),_),
	assertz_most_general(right_ch:inactive(P,P0,M),no).

list :-
	listing(inactive/3),
	listing(tt/4).

count :-
	hdrug_util:count_edges(right_ch:inactive(_,_,_),B),
	write(B),write(' inactive edges'),nl,
	hdrug_util:count_edges(right_ch:tt(_,_,_,_),D),
	write(D),write(' tt edges'),nl,
	hdrug_util:hdrug_flag(ticks,E),
	write(E),write(' ticks'),nl.

count(B) :-
	hdrug_util:count_edges(right_ch:inactive(_,_,_),B).

pack_rule(Name,Mother,[H|T]) :-
	user:r_rule(R,Mother,L,Name),
	reverse([R|L],[H|T]).
pack_rule(Name,Mother,[]):-
	user:gap(Mother,Name),
	user:ign_gap(Mother,Name).





