:- module(left_ch,[]).

:- use_module(lists, library(lists), [ append/3 ]).

:- use_module(database).

parse(o(Node,String,_)) :-
	hdrug_util:hdrug_flag(ticks,_,0),
	bu_lr(String,0,Max),
	write('recover parse trees'), nl,
	packing:apply_semantics(Node,0,Max,left_ch).

clean :-
	clean_up_database(left_ch:inactive(_,_,_)),
	clean_up_database(left_ch:active(_,_,_,_,_)),
	clean_up_database(left_ch:tt(_,_,_,_)).

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
	store([],I0,I,M,tree(Name,[]),Edge).

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
process_one(inactive(P0,P1,Cat),Agenda):-
	findall(Edge,completer(Cat,P0,P1,Edge),Agenda).

process_one(active(_,_,_,_,_),[]).

% completer(+Cat,+P1,+P,-Edge)
% selects active edge whose leftmost symbol matches Cat
completer(Cat,P1,P,Edge):-
	select_active(M,Cat,Rest,P0,P1,tree(MR,His2)),   % select active edge
	append(His2,[t(P1,P,Cat)],His3),
	store(Rest,P0,P,M,tree(MR,His3),Edge).   % store in database

select_active(M,Cat,Rest,P0,P1,His):-
	active(P1,P0,M,[Cat|Rest],His).

select_active(M,Cat,Rest,P,P,tree(Name,[])):-
	user:ign_l_rule(Cat,M,Rest,Name).

% store(+Edge,-Edge)
% stores an edge, and depending on form gives different representation
% of the same edge back. If the edge already exists, then failure.
store(_,_,_,_,_,_):-
	hdrug_util:hdrug_flag(ticks,V),
	V2 is V+1,
	hdrug_util:hdrug_flag(ticks,_,V2),
	fail.
store([],P0,P,M,tree(Rule,His),inactive(P0,P,M)):-
	assertz_most_general(left_ch:tt(P0,P,Rule,His),_),
	assertz_most_general(left_ch:inactive(P0,P,M),no).

store([H|T],P0,P,M,tree(Name,His),Edge):-
	user:ign_gap(H,Gap),
	assertz_most_general(left_ch:tt(P,P,Gap,[])),
	append(His,[t(P,P,H)],NewHis),
	store(T,P0,P,M,tree(Name,NewHis),Edge).

store([H|T],P0,P,M,His,active(P0,P,M,[H|T],His)):-
	assertz_most_general(left_ch:active(P,P0,M,[H|T],His),no).

% compilation is not necc:

list :-
	listing(inactive/3),
	listing(active/5),
	listing(tt/4).

count(No) :-
	hdrug_util:count_edges(left_ch:inactive(_,_,_),B),
	hdrug_util:count_edges(left_ch:active(_,_,_,_,_),C),
	No is B + C.


count :-
	hdrug_util:count_edges(left_ch:inactive(_,_,_),B),
	write(B),write(' inactive edges'),nl,
	hdrug_util:count_edges(left_ch:active(_,_,_,_,_),C),
	write(C),write(' active edges'),nl,
	hdrug_util:count_edges(left_ch:tt(_,_,_,_),D),
	write(D),write(' tt edges'),nl,
	hdrug_util:hdrug_flag(ticks,E),
	write(E),write(' ticks'),nl.

pack_rule(Name,Mother,[]):-
	user:ign_gap(Mother,Name),
	user:    gap(Mother,Name).
pack_rule(Name,Mother,[H|T]) :-
	user:l_rule(H,Mother,T,Name).


