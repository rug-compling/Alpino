:- module(clp_assert,
	  [
	  ]).


:- if(current_prolog_flag(dialect,swi)).

:- use_module(library(debug)).
:- use_module(library(prolog_stack)).

% :- debug(clp_assert).

/** <module> Assert clauses with constraints

*/

:- meta_predicate
	system:clp_asserta(:),
	system:clp_assertz(:),
	system:clp_asserta(:, -),
	system:clp_assertz(:, -),
	system:noclp_asserta(:),
	system:noclp_assertz(:),
	system:noclp_asserta(:, -),
	system:noclp_assertz(:, -).

system:clp_asserta(M:Clause) :-
	(   term_attvars(Clause, [])
	->  asserta(M:Clause)
	;   lift_constraints(Clause, M, NewClause),
	    asserta(M:NewClause)
	).

system:clp_assertz(M:Clause) :-
	(   term_attvars(Clause, [])
	->  assertz(M:Clause)
	;   lift_constraints(Clause, M, NewClause),
	    assertz(M:NewClause)
	).

system:clp_asserta(M:Clause, Ref) :-
	(   term_attvars(Clause, [])
	->  asserta(M:Clause, Ref)
	;   lift_constraints(Clause, M, NewClause),
	    asserta(M:NewClause, Ref)
	).

system:clp_assertz(M:Clause, Ref) :-
	(   term_attvars(Clause, [])
	->  assertz(M:Clause, Ref)
	;   lift_constraints(Clause, M, NewClause),
	    assertz(M:NewClause, Ref)
	).

lift_constraints(Cl0, _M, Cl) :-
	copy_term(Cl0, Cl1, Cs),
	(   Cs == []
	->  Cl = Cl0
	;   (   debugging(clp_assert)
	    ->	debug(clp_assert, 'Expanding ~p', [Cl1+Cs]),
		backtrace(5)
	    ;	true
	    ),
	    list_comma(Cs, InsertClp),
	    head_body(Cl1, InsertClp, Cl)
	).

head_body(Term,InsertClp,Clause) :-
    (   Term = (Head :- Body)
    ->  Clause = (Head :- InsertClp,Body)
    ;   Clause = (Term :- InsertClp)
    ).

list_comma([],true).
list_comma([H|T],Result) :-
    list_comma(T,H,Result).

list_comma([],H,H).
list_comma([H|T],Result0,(Result0,Result1)) :-
    list_comma(T,H,Result1).

system:noclp_asserta(M:Clause) :-
    asserta(M:Clause).
system:noclp_assertz(M:Clause) :-
    assertz(M:Clause).
system:noclp_asserta(M:Clause,Ref) :-
    asserta(M:Clause,Ref).
system:noclp_assertz(M:Clause,Ref) :-
    assertz(M:Clause,Ref).

%%	Trap all assert-calls

system:goal_expansion(assert(G),          clp_assertz(G)).
system:goal_expansion(asserta(G),         clp_asserta(G)).
system:goal_expansion(assertz(G),         clp_assertz(G)).
system:goal_expansion(assert(G,R),        clp_assertz(G,R)).
system:goal_expansion(asserta(G,R),       clp_asserta(G,R)).
system:goal_expansion(assertz(G,R),       clp_assertz(G,R)).

system:goal_expansion(noclp_assert(G),    noclp_assertz(G)).
system:goal_expansion(noclp_asserta(G),   noclp_asserta(G)).
system:goal_expansion(noclp_assertz(G),   noclp_assertz(G)).
system:goal_expansion(noclp_assert(G,R),  noclp_assertz(G,R)).
system:goal_expansion(noclp_asserta(G,R), noclp_asserta(G,R)).
system:goal_expansion(noclp_assertz(G,R), noclp_assertz(G,R)).

:- else.

:- multifile
    user:goal_expansion/3.

user:goal_expansion(noclp_assert(G),    Module, Module:assertz(G)).
user:goal_expansion(noclp_asserta(G),   Module, Module:asserta(G)).
user:goal_expansion(noclp_assertz(G),   Module, Module:assertz(G)).
user:goal_expansion(noclp_assert(G,R),  Module, Module:assertz(G,R)).
user:goal_expansion(noclp_asserta(G,R), Module, Module:asserta(G,R)).
user:goal_expansion(noclp_assertz(G,R), Module, Module:assertz(G,R)).

:- endif.
