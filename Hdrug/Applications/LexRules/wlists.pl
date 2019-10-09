:- module(wlists,
	  [ 
            wappend/3,
            wreverse/2,
            wreverse/4

	  ]).

%%%%%%%%%%%
% wappend %
%%%%%%%%%%%

wappend(L0,L1,L) :-
	( L0 == L -> L1 = []
        ; L1 == L -> L0 = []
        ; wappend0(L0,L1,L)
        ).

:- block wappend0(-,-,-).
wappend0(L0,L1,L) :-
	( L1 == [] -> L0 = L
        ; wappend1(L0,L1,L)
        ).

:- block wappend1(-,?,-).
wappend1([],X,X).
wappend1([H|T],X,[H|T2]):-
	wappend(T,X,T2).

%%%%%%%%%%%%
% wreverse %
%%%%%%%%%%%%

% really reversible reverse:
wreverse(L,RevL) :-
	wreverse(L,[],RevL,RevL).

:- block wreverse(-,?,?,-).
wreverse([],L,L,[]).
wreverse([H|T],Hulp,RevL,[_|NT]):-
	wreverse(T,[H|Hulp],RevL,NT).


