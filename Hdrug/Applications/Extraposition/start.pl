:- op(1001,xfy,(...)).
:- op(1200,xfx,(==>)).

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(lists,library(lists),all).

terminal(T,[T|S],S,X,X) :-
	gap(X).
terminal(T,S,S,x(_C,terminal,T,X), X).

gap(x(gap,_T,_S,_X)).
gap([]).

virtual(NT,x(_C,nonterminal,NT,X), X).

:- multifile term_expansion/2.

term_expansion((L ==> R), (P:-Q)) :-
	expandlhs(L,S0,S,H0,H,P),
	expandrhs(R,S0,S,H0,H,Q).

expandlhs(T,S0,S,H0,H1,Q):-
	flatten(T,[P|L],[]),
	front(L,H1,H),
	tag(P,S0,S,H0,H,Q).

flatten((X...Y),L0,L):-
	!,
	flatten(X,L0,[gap|L1]),
	flatten(Y,L1,L).
flatten((X,Y),L0,L):-
	!,
	flatten(X,L0,[nogap|L1]),
	flatten(Y,L1,L).
flatten(X,[X|L],L).

front([],H,H).
front([K,X|L],H0,H):-
	case(X,K,H1,H),
	front(L,H0,H1).

case([T|Ts],K,H0,x(K,terminal,T,H)):-
	!,
	unwind(Ts,H0,H).
case(Nt,K,H,x(K,nonterminal,Nt,H)) :-
	virtual_rule(Nt).

virtual_rule(Nt):-
	functor(Nt,F,N),
	functor(Y,F,N),
	tag(Y,S,S,Hx,Hy,P),
	( clause(P,virtual(_,_,_),_),
	  !
        ; asserta((P :- virtual(Y,Hx,Hy)))
        ).

expandrhs((X1,X2),S0,S,H0,H,Y):-
	!,
	expandrhs(X1,S0,S1,H0,H1,Y1),
	expandrhs(X2,S1,S,H1,H,Y2),
	and(Y1,Y2,Y).
expandrhs((X1;X2),S0,S,H0,H,(Y1;Y2)):-
	!,
	expandor(X1,S0,S,H0,H,Y1),
	expandor(X2,S0,S,H0,H,Y2).
expandrhs({X},S,S,H,H,X) :-
	!.
expandrhs(L,S0,S,H0,H,G):-
	is_list(L),
	!,
	expandlist(L,S0,S,H0,H,G).
expandrhs(X,S0,S,H0,H,Y):-
	tag(X,S0,S,H0,H,Y).

expandor(X,S0,S,H0,H,Y):-
	expandrhs(X,S0a,S,H0a,H,Ya),
	( S\==S0a, !, S0=S0a, Yb=Ya; and(S0=S0a,Ya,Yb)),
	( H\==H0a, !, H0=H0a,Y=Yb; and(H0=H0a,Yb,Y)).

expandlist([],S,S,H,H,true).
expandlist([X],S0,S,H0,H,terminal(X,S0,S,H0,H)):-
	!.
expandlist([X|L],S0,S,H0,H,terminal(X,S0,S1,H0,H1),Y):-
	expandlist(L,S1,S,H1,H,Y).

tag(P,A1,A2,A3,A4,Q):-
	P =.. [F|Args0],
	append(Args0,[A1,A2,A3,A4],Args),
	Q =.. [F|Args].

and(true,P,P) :- !.
and(P,true,P) :- !.
and(P,Q,(P,Q)).

unwind([],H,H).
unwind([T|Ts],H0,x(nogap,terminal,T,H)):-
	unwind(Ts,H0,H).



:- ensure_loaded(td).

compile_grammar :- 
	compile_grammar_file(dutch).

reconsult_grammar :-
	reconsult_grammar_file(dutch).

compile_grammar_file(File) :-
	compile(File).

reconsult_grammar_file(File) :-
	reconsult(File).

:- initialize_flag(top_features,main).
:- initialize_flag(parser,td).
% :- initialize_flag(generator,td).

:- version('F. Pereira''s Extraposition Grammars').

:- compile_grammar.

:- initialize_flag(parser(td),on).
:- initialize_flag(generator(td),on).

