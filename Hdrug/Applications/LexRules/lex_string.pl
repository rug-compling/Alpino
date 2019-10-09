:- module( lex_string, [ lex_string/1
		       ]).

:- use_module(lists, library(lists), all).

lex_string(Str) :-
	retractall(user:lex(_,_,_,_)),
	retractall(user:ign_lex(_,_,_,_)),
	assert_lex(Str,0),
	check_covered(Str).

assert_lex([],_P).
assert_lex([H|T],P0):-
	P2 is P0 + 1,
	( user:lexicon([H|T0],Small,N),
	  append(T0,_,T),
	  length([H|T0],L),
	  P1 is P0 + L,
	  assertz(user:lex(P0,P1,Small,N)),
	  fail
        ; user:ign_lexicon([H|T0],Small,N),
	  append(T0,_,T),
	  length([H|T0],L),
	  P1 is P0 + L,
	  assertz(user:ign_lex(P0,P1,Small,N)),
	  fail
        ; assert_lex(T,P2)
        ).

check_covered(Str) :-
	check_covered(Str,0,true,Call),
	Call.

check_covered([],_,T,T).
check_covered([H|T],P0,T0,Tr) :-
	P is P0 + 1,
	check_covered0(H,P0,P,T0,T1),
	check_covered(T,P,T1,Tr).

check_covered0(Word,P0,P,T0,T) :-
	(  call_residue(user:lex(Q0,Q,_,_),_),
	   Q0 =< P0, P =< Q
        -> T0 = T
        ;  format("ERROR: ~w is unknown~n",[Word]),
	   T = raise_exception(restart)
        ).

