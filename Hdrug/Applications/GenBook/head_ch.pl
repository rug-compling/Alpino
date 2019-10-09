:- module(head_ch,[]).

:- use_module( lists, library(lists), [ append/3, reverse/2 ]).

:- use_module( database ).


clean :-
	clean_up_database(head_ch:inactive(_,_,_)),
	clean_up_database(head_ch:active(_,_,_,_,_)),
	clean_up_database(head_ch:tt(_,_,_,_)).

parse(o(Node,String,_)) :-
	hdrug_util:hdrug_flag(ticks,_,0),
	bu_lr(String,0,Max),
	write('recover parse trees'), nl,
	packing:apply_semantics(Node,0,Max,head_ch).

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
	store([],M,I0,I,tree(Name,[]),Edge).

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
process_one(active(_,_,_,_,_),[]).

% completer(+Cat,+P1,+P,-Edge)
% selects rule whose head symbol matches Cat
completer(Cat,P1,P,Edge):-
	(
	user:ign_h2_rule(Cat,M,Lefties,MR),
	select_lefties(Lefties,P0,P1,[t(P1,P,Cat)],His),
	store([],M,P0,P,tree(MR,His),Edge)
	;
	user:ign_h_rule(Cat,M,Lefties,Rest,MR),
	select_lefties(Lefties,P0,P1,[t(P1,P,Cat)],His),
	store(Rest,M,P0,P,tree(MR,His),Edge)   % store in database
        ;
% or active edge whose leftmost daughter (right of the head) matches Cat
	active(P1,P0,M,[Cat|Rest],tree(MR,His)),
	append(His,[t(P1,P,Cat)],His1),
	store(Rest,M,P0,P,tree(MR,His1),Edge)
        ).

select_lefties([],P,P,His,His).
select_lefties([H|T],P0,P,HisIn,HisOut):-
	inactive_or_gap(P,P1,H),
	select_lefties(T,P0,P1,[t(P1,P,H)|HisIn],HisOut).

inactive_or_gap(P1,P,H):-
	inactive(P1,P,H).
inactive_or_gap(P,P,H):-
	user:ign_gap(H,Gap),
	assertz_most_general(head_ch:tt(P,P,Gap,[])).

% store(+Edge,-Edge)
% stores an edge, and depending on form gives different representation
% of the same edge back. If the edge already exists, then failure.
store(_,_,_,_,_,_):-
	hdrug_util:hdrug_flag(ticks,V),
	V2 is V+1,
	hdrug_util:hdrug_flag(ticks,_,V2),
	fail.
store([],M,P0,P,tree(Rule,His),inactive(M,P0,P)):-
	assertz_most_general(head_ch:tt(P0,P,Rule,His),_),
	assertz_most_general(head_ch:inactive(P,P0,M),no).

store([H|T],M,P0,P,tree(Rule,His),Edge):-
	user:ign_gap(H,_),
	append(His,[t(P,P,H)],NewHis),
	store(T,M,P0,P,tree(Rule,NewHis),Edge).

store([H|T],M,P0,P,His,active(M,[H|T],P0,P,His)):-
	assertz_most_general(head_ch:active(P,P0,M,[H|T],His),no).

list :-
	listing(inactive/3),
	listing(active/5),
	listing(tt/4).

count(No) :-
	hdrug_util:count_edges(head_ch:inactive(_,_,_),B),
	hdrug_util:count_edges(head_ch:active(_,_,_,_,_),C),
	No is B + C.

count :-
	hdrug_util:count_edges(head_ch:inactive(_,_,_),B),
	write(B),write(' inactive edges'),nl,
	hdrug_util:count_edges(head_ch:active(_,_,_,_,_),C),
	write(C),write(' active edges'),nl,
	hdrug_util:count_edges(head_ch:tt(_,_,_,_),D),
	write(D),write(' tt edges'),nl,
	hdrug_util:hdrug_flag(ticks,V),
	write(V),write(' ticks'), nl.

pack_rule(Name,Mother,[]):-
	user:ign_gap(Mother,Name),
	user:    gap(Mother,Name).
pack_rule(Name,Mother,[F|T]) :-
	user:h_rule(H,Mother,L,R,Name),
	reverse(L,RevL),
	append(RevL,[H|R],[F|T]).
pack_rule(Name,Mother,[F|T]):-
	user:h2_rule(F,Mother,T,Name).


