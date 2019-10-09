:- module(right_chart,[]).

% inactive chart parser.
%
% Does not check for more/less general items on the
% chart.

clean :-
	retractall(inactive(_,_,_)).

parse(o(Node,String,_)) :-
	length(String,Max),
	bu_lr(0,Max),
	an_inactive(Max,0,Node).

bu_lr(I,I) :- !.

bu_lr(I0,I):-
	( lookup(I0,Edge),
	  process_one(Edge),
	  fail
        ; true ),
	I1 is I0 + 1,
	bu_lr(I1,I).

lookup(I0,inactive(I0,I,M)):-
	user:lex(I0,I,M,_Name).

process_one(Edge0) :-
	completer(Edge0,Edge),
	process_one(Edge).

% completer(+Cat,+P1,+P,-Edge)
% selects active edge whose rightmost symbol matches Cat
completer(inactive(P1,P,Cat),inactive(P0,P,M)):-
	user:r_rule(Cat,M,Lefties,_MR),
	select_lefties(Lefties,P0,P1),
	assert_if(P,P0,M).

select_lefties([],P,P).
select_lefties([H|T],P0,P):-
	an_inactive(P,P1,H),
	select_lefties(T,P0,P1).

an_inactive(P,P0,H) :-
	inactive(P,P0,H).
an_inactive(P,P0,H) :-
	user:lex(P0,P,H,_).

list :-
	listing(inactive/3).

count :-
	count(B),
	write(B),write(' inactive edges'),nl.

count(B) :-
	hdrug_util:count_edges(right_chart:inactive(_,_,_),B).

assert_if(P,P0,Cat) :-
	assertz(inactive(P,P0,Cat)).


