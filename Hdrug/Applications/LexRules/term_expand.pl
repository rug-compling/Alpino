%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% term_expansion %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. handles constraints (by placing them back in body if necc.)
% 2. handles disjunction as it should be
% 3. allows series of term-expansions
% 4. does not make difference for unit/non-unit clauses
% 5. body is a list, rather than a conjunction

% the user defines a term_expansion as follows
%
% 
%
% adda_expansion(F)
% add_expansion(F)
% addz_expansion(F)
% del_expansion(F)
% where F(Head0,Body0,Head,Body) is defined

:- use_module( lists, library(lists), [ delete/3, append/3, member/2]).

dcg_expansion(H0,B0,H,B) :-
	dcg_expansion(H0,HB),
        head_body(HB,H,B1),
	append(B1,B0,B).

% pre 3.7
dcg_expansion(A,B):-
    prolog:current_predicate(dcg_expansion,Body),
    dcg_expansion(Body,A,B).

dcg_expansion(dcg_expansion(_,_),A,B):-
    prolog:dcg_expansion(A,B).

dcg_expansion(dcg_expansion(_,_,_,_),A,B):-
    prolog:dcg_expansion(A,[],B,[]).


adda_expansion(F):-
	hdrug_flag(expansion_list,Old),
	(  member(F,Old)
	-> format(user_error,"~w already in expansion list~n",[F])
	;  hdrug_flag(expansion_list,Old,[F|Old])
	).

add_expansion(F):-
	adda_expansion(F).
	
addz_expansion(F):-
	hdrug_flag(expansion_list,Old),
	(  member(F,Old)
	-> format(user_error,"~w already in expansion list~n",[F])
	;  append(Old,[F],New),
	   hdrug_flag(expansion_list,_,New)
	).

del_expansion(F):-
	hdrug_flag(expansion_list,Old),
	delete(Old,F,New),   % succeeds if it was not there..
	hdrug_flag(expansion_list,_,New).

head_body((?- X0),query,X) :-
	prolog_conjunction(X0,X).
head_body((:- X0),fail,X) :-
	prolog_conjunction(X0,X).
head_body((H:-Body0),H,Body) :-
	!,
	prolog_conjunction(Body0,Body). 
head_body(H,H,[]).

% term_expansion does findall(call_residue(series of expansion))

:- (  current_predicate(term_expansion,term_expansion(_,_))
   -> abolish(term_expansion/2)
   ;  true
   ).

te(( ?- _Q),_) :- !,fail.
te(( :- _Q),_) :- !,fail.
te(end_of_file,_) :- !,fail.

te(Clause0,Clausesx) :-
	( hdrug_flag(trace_exp,on) -> trace ; true ),
	head_body(Clause0,Head,Body),
	findall(Clause,constraints_expand(Head,Body,Clause),Clauses),
	( hdrug_flag(write_exp,on) -> portrays(Clauses) ; true ),
	!,
	single_or_set(Clauses,Clausesx).
te(C,C).  % robustness

single_or_set([H],H) :-!.
single_or_set(L,L).

portrays([]).
portrays([H|T]) :-
	portray_clause(H),
	portrays(T).

constraints_expand(Head0,Body0,Clause) :-
	call_residue(expander(Head0,Body0,Head,Body1),Body2),
	rewrite_body(Body2,[],Body3,[]),
	append(Body3,Body1,Body4),
	build_clause(Head,Body4,Clause).

build_clause(query,Body0,(?-Body)) :-
	!,
	prolog_conjunction(Body,Body0).
build_clause(fail,Body0,(:-Body)) :-
	!,
	prolog_conjunction(Body,Body0).
build_clause(Head,true,Head) :- !.
build_clause(Head,Body0,(Head:-Body)) :-
	prolog_conjunction(Body,Body0).

expander(Head0,Body0,Head,Body) :-
	hdrug_flag(expansion_list,Old),
	expander(Old,Head0,Body0,Head,Body).

expander([],H,B,H,B).
expander([F|T],H0,B0,H,B) :-
	expand_one(F,H0,B0,H1,B1),
	expander(T,H1,B1,H,B).

expand_one(F,H0,B0,H,B) :-
	if( expand_it(F,H0,B0,H,B),
            true,
            ( H0=H, B0=B )
	  ).

expand_it(F,H0,B0,H,B) :-
	is_mod(F,Module,Functor),
	Call =.. [Functor,H0,B0,H,B],
	call(Module:Call).

is_mod(F,user,F) :-
	atomic(F).
is_mod(Mod:F,Mod,F).

rewrite_body([],_,C,C).
rewrite_body([_-H|T],Other0,C0,C):-
    (	eq_member(H,Other0)
    ->  Other0=Other,
	C1=C0
    ;	C0 = [H|C1],
	Other=[H|Other0]
    ),
    rewrite_body(T,Other,C1,C).

eq_member(X,[Y|T]):-
    (	X==Y
    ->  true
    ;   eq_member(X,T)
    ).

:- initialize_flag(expansion_list,[dcg_expansion]).

:- assertz((term_expansion(A,B) :- te(A,B))).
