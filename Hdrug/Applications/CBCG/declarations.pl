top(s,tree(X,_,_)) :-
	X <=> s,
	X <=> vsecond,
	X:gap ==> empty.

top(q,tree(X,_,_)) :-
	X <=> s,
	X <=> vfirst,
	X:gap ==> empty.

% SEMANTICS/2
% semantics(Node,Sem)
% defines the semantics Sem of node Node. Is used: 
% 
% 1. in order to determine the semantic-head of a rule 
%    (for head-driven generation).
% 2. to instantiate the semantics of a node, at the
%    start of generation. 
% 3. to print the result of parsing.

semantics(tree(Sign,_,_),Sem) :-
	Sign:sem <=> Sem.


user_clause(rule(A,B,C,D),Body) :-
	call_residue(rule(A,B,C,D),Cons),
	rewrite_body(Cons,[],Body,[]).

user_clause(Head,Body) :-
%	clause(lex(A,B),Body0),
	call_residue(lex(A,B),Cons),
	rewrite_body(Cons,[],Body,[]),
	(   atomic(A) 
        ->  Head =.. [A,B]
        ;   A = [te,X],
	    concat('te_',X,F),
	    Head =.. [F,B]
	).

/*
result_hook(parse,_,o(Obj,_,_),_) :-
	(  hdrug_flag(demo,on)
	-> show(tree(syn),tk,[value(Obj)])
	;  true
	), 
	if_gui(tcl("update")).
*/
end_hook(parse,_,_,_) :-
    show_object_no(1,tree(syn),clig).

show_object_default2(No) :-
    show_object_no(No,tree(syn),clig).



:- initialize_flag(parser(parser),on).
:- initialize_flag(parser(shift_reduce),on).

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
