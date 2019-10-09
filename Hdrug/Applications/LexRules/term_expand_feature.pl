%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% C %%%%%%%%%%% partial evaluation facility %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% this allows `functional' use of feature expressions in head and 
% body of clauses. To put this to work, define
% term_expansion(Clause0,Clause) :- 
%          eval_funct(Clause0,Clause).
%
%
% For example, given an appropriately compiled type system consisting of
%
% define_type(top,[[list,..]],[],_,true).
% define_type(list,[[elist,nelist]],[],_,true).
% define_type(elist,[],[],_,true).
% define_type(nelist,[],[f,r],_,true).
%
% the following definition 
%
% append(Elist,List,List) :-
%    Elist <=> `elist.
% append(HT,X,HT2) :-
%    HT:f <=> HT2:f,
%    HT:r <=> T,
%    HT2:r <=> T2,
%    append(T,X,T2)
%
% can now be abbreviated as:
%
% append(`elist,List,List).
% append(HT,X,HT2) :-
%    HT:f <=> HT2:f,
%    append(HT:r,X,HT2:r).
%
%
% so anywhere in a definition where a Path occurs, this Path
% is evaluated.
% X:Path
% `Type
% @Macro
% {X} evaluates to X (escape)
%  =Term
% Path & Path
% 
%
%
% ONLY MAKES SENSE FOR PURE PROLOG DEFINITIONS
% and not for definitions using disjunction, etc.
%
%
:- (  user:current_predicate(del_expansion,_)
   -> user:del_expansion(hdrug_feature:eval_head_body)
   ;  true
   ).

eval_head_body(Head0,Body0,Head,Body) :-
	hdrug_flag(eval_feature,on),
	evalt(Head0,Head),
	eval_body(Body0,Body).

evall([],[]).
evall([H0|T0],[H|T]):-
	evalt(H0,H),
	evall(T0,T).

evalt(Var0,Var) :-
	var(Var0),!,
	Var=Var0.
evalt(Term0,Term) :-
	functor(Term0,Fun,Ar),
	evalt1(Fun,Ar,Term0,Term).

evalt1(Fun,Ar,Term0,Term) :-
	a_eval_op(Fun/Ar,Type),!,
	eval_op(Type,Term0,Term).
evalt1(Fun,Ar,Term0,Term) :-
	Term0 =.. [Fun|L0],
	length(L,Ar),
	Term  =.. [Fun|L],
	evall(L0,L).

eval_body(Body0,Body) :-
	eval_body_l(Body0,Body).
eval_body({X},X).

eval_body_l(V0,V) :-
	var(V0),!,
	V0=V.
eval_body_l([],[]).
eval_body_l([H|T],Out) :-
	eval_goal(H,T2,Out),
	eval_body_l(T,T2).

eval_goal((A->B;C),T,L) :- 
	!,
	L=[(A->B;C)|T].
eval_goal((A;B),T0,T) :-
	!,
	(  eval_goal(A,T0,T)
	;  eval_goal(B,T0,T)
	).

eval_goal({H},T,[H|T]) :-
	!.
eval_goal(Call0,T,T) :-
	a_call(Call0,Call),!,
	call(Call).
eval_goal(Term,T,[NewTerm|T]) :-
	evalt(Term,NewTerm).

a_call(C,C) :-
	eval_a_call(C,no).
a_call(C0,C) :-
	eval_a_call(C0,yes),
	evalt(C0,C).

a_call(C,user:C) :-
	usercall(eval_a_call(C,no)).
a_call(C0,user:C) :-
	usercall(eval_a_call(C0,yes)),
	evalt(C0,C).

eval_a_call( _ ==> _ , no).
eval_a_call( _ <=> _ , no).
eval_a_call( _  => _ , no).
eval_a_call(unify_except(_,_,_), yes).       % for third argument should be no
eval_a_call(unify_except_l(_,_,_), yes).     %               ,,
eval_a_call(overwrite(_,_,_,_), yes).

%%eval_a_call(Term,Yn) :-
%%	user:eval_a_call(Term,Yn).

a_eval_op('{}'/1,escape). % escape
a_eval_op(':'/2,eval).  % `real' path
a_eval_op('`'/1,eval).  % type
a_eval_op('@'/1,eval).  % macro
a_eval_op('&'/2,eval).  % conjunction of paths
a_eval_op('='/1,eval).  % untyped prolog term
a_eval_op('<=>'/2,eq). % equation
a_eval_op( '=>'/2,eq). % ,,
a_eval_op('==>'/2,eq). % ,,
a_eval_op(F/Ar,eval):-
	functor(Term,F,Ar),
	usercall(user_defined_eval(Term)).

eval_op(escape,{T0},T) :-      % escape
	T0=T.
eval_op(eval,Term0,Term) :-       % so Term0 is a `path'
	Term0 <=> Term.        % hence evaluates to Term
eval_op(eq,A <=> B,Term) :-
	A <=> B <=> Term.
eval_op(eq,A => B, Term) :-
	A => B,
	lvar(A,X),
	X <=> Term.
eval_op(eq,A ==> B, Term) :-
	A ==> B,
	lvar(A,X),
	X <=> Term.

lvar(Var0,Var) :-
	var(Var0),
	!,
	Var0=Var.
lvar(Var0:_,Var) :-
	!,
	Var0=Var.
lvar(Term,Term).


:- (  user:current_predicate(add_expansion,_)
   -> user:add_expansion(hdrug_feature:eval_head_body)
   ;  true
   ).


