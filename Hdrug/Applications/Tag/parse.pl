%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% auxiliary predicates for parsers %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module( lists, library(lists), [ member/2 ]).

adjoin(Node,Foot,Top):-
	user_bottom(Foot,B),
	user_bottom(Node,B),
	user_top(Top,T),
	user_top(Node,T),
	user_syn(Node,Syn),
	user_syn(Foot,Syn),
	user_syn(Top,Syn).

unify_node(Cat) :-
	user_bottom(Cat,F),
	user_top(Cat,F).

% note that check_lexs is non-deterministic. Could this lead to
% spurious ambiguities? If it were not nd, then problems in case
% of multiple occurrences of the same word.
%
% the argument Index of lex(Word/Index) is used to remember which position
% the anchor of an auxiliary was supposed to be consuming (this index is
% instantiated by check_lexs)
%
% check_lexs is replaced by check_lex in most parsers
check_lexs([],_,_,_,_,U,U).
check_lexs([_/L|T],A,B,C,D,Old,New):-
        % check it occurs in a possible position
	% to the left or to the right of the current foot within
	% extreme positions,
	( A=< L, L < B
        ; C=< L, L < D
        ), % third check it is not `reserved' by previous aux.
	\+ member(L,Old),
	check_lexs(T,A,B,C,D,[L|Old],New).

check_lex(P0,P,L0,L,R0,R,Used) :-
	\+ member(P0,Used),
	( L0 =< P0, P =< L
        ; R0 =< P0, P =< R
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% first phase of parsing: select initial trees %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hook(parse,Mode,o(_,Words,_),_) :-
	clean,
	first_phase(Words),
	(  member(Mode,[pack,pack_d,pack_tree])
        -> clean_ign,
	   first_phase_ign
        ;  true
	).


assertz_lex([],P,P).
assertz_lex([H|T],P0,P) :-
	P1 is P0 + 1,
	( assertz(lex(H,P0,P1)),
	  fail
        ; assertz_lex(T,P1,P)
        ).

clean :-
	retractall( lex(_,_,_)),
	retractall(init(_,_,_,_,_,_,_)),
	retractall( aux(_,_,_,_,_,_,_)).

clean_ign :-
	retractall(ign_init(_,_,_,_,_,_,_)),
	retractall( ign_aux(_,_,_,_,_,_,_)).

first_phase(String) :-
	assertz_lex(String,0,_Max),
	assertz_compiled_init,
	assertz_compiled_aux.

first_phase_ign :-
	assertz_compiled_ign_init,
	assertz_compiled_ign_aux.

assertz_compiled_init :-
	( user_syn(Cat,S),
	  user_syn(Small,S),
	  user_top(Cat,Top),
	  user_top(Small,Top),
	  address(Cat,Address),
	  lex(W,P0,P),
	  init_rule(Small,W,ToParse,Name),
	  assertz(init(Cat,Address,W,ToParse,Name,P0,P)),
	  fail
        ; true ).

assertz_compiled_ign_init :-
	( user_syn(Cat,S),
	  user_syn(Small,S),
	  user_top(Cat,Top),
	  user_top(Small,Top),
	  address(Cat,Add),
	  lex(W,P0,P),
	  ign_init_rule(Small,W,ToParse,Name),
	  assertz(ign_init(Cat,Add,W,ToParse,Name,P0,P)),
	  fail
        ; true ).

assertz_compiled_aux :-
	( adjoin(Small,Foot,Top),
	  unify_node(Foot),
	  address(Small,Add),
	  lex(W,Wi,P),
	  aux_rule(Foot,Top,ToParse,[W/Wi|Ws],Name),
	  others_ok(Ws,P),
	  assertz(aux(Small,Add,W,ToParse,Name,Wi,P)),
	  fail
        ; true ).

assertz_compiled_ign_aux :-
	( user:adjoin(Small,Foot,Top),
	  unify_node(Foot),
	  address(Small,Address),
	  lex(W,Wi,P),
	  ign_aux_rule(Foot,Top,ToParse,[W/Wi|Ws],Name),
	  others_ok(Ws,P),
          assertz(ign_aux(Small,Address,W,ToParse,Name,Wi,P)),
	  fail
        ; true ).

others_ok([],_).
others_ok([H/P1|T],P0):-
	lex(H,P1,_),
	P1 >= P0,
	others_ok(T,P1).


