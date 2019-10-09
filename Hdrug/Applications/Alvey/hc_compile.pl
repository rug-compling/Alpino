:- module(hc_compile,[ hc_compile_grammar/0 ]).

compile_grammar(File) :-
	user:ensure_grammar_compiled_for_parser(hc9,File).

dump_grammar:-
	(  hc_in:current_predicate(_,A),
	   hc_in:clause(A,B,_),
	   my_portray_clause(A,B),
%%	   portray_clause((hc_in:A:-hc_in:B)),
	   fail
	;  true
	).

my_portray_clause(Head,true) :-
	!,
	portray_clause(hc_in:Head).
my_portray_clause(Head,Body) :-
	portray_clause((hc_in:Head :- hc_in:Body)).

clean_grammar :-
	(  hc_in:current_predicate(_,X),
	   functor(X,F,A),
	   abolish(hc_in:(F/A)),
	   fail
	;  true
	).

compile_sem_filter :-
	(  grammar:sem_filter(Vars,Cat),
	   replace_each(Vars,Cat,Syn,SemCons,SemCat),
	   assertz(hc_in:separate(Cat,Syn,SemCons,SemCat)),
	   fail
	;  true
	).

compile_rules :-
	(  grammar:grammar_rule(Id,M,Ds),
	   try_separate(M,SynM,_,_),
	   compile_hcr(Ds,M,Id,SynM,Rule1,Rule2),
	   assertz(hc_in:Rule1),
	   assertz(hc_in:Rule2),
	   fail
	;  true
	).

compile_grammar :-
	clean_grammar,
	hc_in:assertz(unknown_predicate_handler(_,fail)),
	format(user_error,"compiling sem_filter..~n",[]),
	compile_sem_filter,
	format(user_error,"compiling sem_filter done~n",[]),
	format(user_error,"compiling rules..~n",[]),
	compile_rules,
	format(user_error,"compiling rules done~n",[]),
	format(user_error,"compiling head-corner table..~n",[]),
	head_corner_table,
	format(user_error,"compiling head-corner table done~n",[]),
	format(user_error,"indexing head-corner table..~n",[]),
	s_index_table3,
	format(user_error,"indexing head-corner table done~n",[]),
	format(user_error,"indexing lexical head-corner table..~n",[]),
	s_index_table4,
	format(user_error,"indexing lexical head-corner table done~n",[]),
	format(user_error,"indexing gap head-corner table..~n",[]),
	s_index_table5,
	format(user_error,"indexing gap head-corner table done~n",[]),
	retractall(hc_in:db(_,_,_,_,_,_)),
	hdrug_util:report_count_edges_pred(hc_in:headed_rule/5),
	hdrug_util:report_count_edges_pred(hc_in:gap/2),
	hdrug_util:report_count_edges_pred(hc_in:head_corner_table/6),
	hdrug_util:report_count_edges_pred(hc_in:head_corner_table_lex/6),
	hdrug_util:report_count_edges_pred(hc_in:head_corner_table_gap/6).

% compile_hcr(+Ds,+M,+Id,+M2,+Ds2,-Rule)
% no daughters: gap
compile_hcr([],M,Name,SynM,gap(SynM,Name),gap_i(Name,M)) :-
	(  grammar:head(Name,_)
	-> format(user_error,"Head declaration for gap ~w ignored~n",[Name])
	;  true
	).

compile_hcr([D0|Ds],M,Id,SynM,headed_rule(H,SynM,Lds,Rds,Id),
	                       headed_rule_i(Id,H,M,Lds,Rds)):-
	(  grammar:head(Id,HeadIndex), integer(HeadIndex), HeadIndex > 0,
           length([D0|Ds],L), HeadIndex =< L
	-> true
	;  HeadIndex=1, 
	   format(user_error,
            "Warning: no proper head declaration; using first daughter (rule ~w)~n",
	    [Id])
	),
        find_head(1,HeadIndex,[D0|Ds],H,[],Lds,Rds).

find_head(I,I,[H|Ds],H,L,L,Ds).
find_head(I0,I,[H|T],NH,L0,L,R) :-
	I1 is I0+1,
	find_head(I1,I,T,NH,[H|L0],L,R).

a_hc(x(Head,P0,P),x(Mother,Pm0,Pm)) :-
	hc_in:headed_rule(Head,Mother,Ls,Rs,_),
	(  Ls=[]
	-> P0=Pm0
	;  true
	),
	(  Rs=[]
	-> P = Pm
	;  true
	).
s_index_table3 :-
	%% for each pair of functor, find the generalization of all
        %% info in hfc. With indexing, now.
	findall(Functor,functor3(Functor),Functors0),
	sort(Functors0,Functors),
	findall(p(A,B,C,D),(  member(A/B,Functors),
			      member(C/D,Functors)
			   ), Pairs),
	generalize_pairs3(Pairs),
	link_clauses.

s_index_table4 :-
	%% for each pair of functor, find the generalization of all
        %% info in hfc. With indexing, now.
	findall(Functor,functor3(Functor),Functors0),
	sort(Functors0,Functors),
	findall(p(A,B,C,D),(  member(A/B,Functors),
			      member(C/D,Functors)
			   ), Pairs),
	generalize_pairs4(Pairs),
	link_clauses4.

s_index_table5 :-
	%% for each pair of functor, find the generalization of all
        %% info in hfc. With indexing, now.
	findall(Functor,functor3(Functor),Functors0),
	sort(Functors0,Functors),
	findall(p(A,B,C,D),(  member(A/B,Functors),
			      member(C/D,Functors)
			   ), Pairs),
	generalize_pairs5(Pairs),
	link_clauses5.


link_clauses :-
	findall(F/A,goal_fa(F,A),Pairs0),
	sort(Pairs0,Pairs),
	link_clauses(Pairs).

link_clauses([]).
link_clauses([F/A|Tail]) :-
	hfc_functor(F,A,Term,Arg4,Arg5,Arg6,Arg1,Arg2,Arg3),
	(  \+ hc_in:Term
	-> true
	;  functor(Arg1,F,A),
	   hc_in:assertz(
	      (head_corner_table(Arg1,Arg2,Arg3,Arg4,Arg5,Arg6):-
	        Term))
	),
	link_clauses(Tail).

link_clauses4 :-
	findall(F/A,goal_fa(F,A),Pairs0),
	sort(Pairs0,Pairs),
	link_clauses4(Pairs).

link_clauses4([]).
link_clauses4([F/A|Tail]) :-
	hfc_functor4(F,A,Term,Arg4,Arg5,Arg6,Arg1,Arg2,Arg3),
	(  \+ hc_in:Term
	-> true
	;  functor(Arg1,F,A),
	   hc_in:assertz(
	      (head_corner_table_lex(Arg1,Arg2,Arg3,Arg4,Arg5,Arg6):-
	        Term))
	),
	link_clauses4(Tail).


link_clauses5 :-
	findall(F/A,goal_fa(F,A),Pairs0),
	sort(Pairs0,Pairs),
	link_clauses5(Pairs).

link_clauses5([]).
link_clauses5([F/A|Tail]) :-
	hfc_functor5(F,A,Term,Arg4,Arg5,Arg6,Arg1,Arg2,Arg3),
	(  \+ hc_in:Term
	-> true
	;  functor(Arg1,F,A),
	   hc_in:assertz(
	      (head_corner_table_gap(Arg1,Arg2,Arg3,Arg4,Arg5,Arg6):-
	           Term))
	),
	link_clauses5(Tail).


hfc_functor(F,A,Term,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6):-
	number_codes(A,X),
	atom_codes(B,X),
	hdrug_util:concat_all([hfc_,F,B],Fun),
	functor(Term,Fun,6),
	arg(1,Term,Arg1),
	arg(2,Term,Arg2),
	arg(3,Term,Arg3),
	arg(4,Term,Arg4),
	arg(5,Term,Arg5),
	arg(6,Term,Arg6).

hfc_functor4(F,A,Term,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6):-
	number_codes(A,X),
	atom_codes(B,X),
	hdrug_util:concat_all([lex_hfc_,F,B],Fun),
	functor(Term,Fun,6),
	arg(1,Term,Arg1),
	arg(2,Term,Arg2),
	arg(3,Term,Arg3),
	arg(4,Term,Arg4),
	arg(5,Term,Arg5),
	arg(6,Term,Arg6).

hfc_functor5(F,A,Term,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6):-
	number_codes(A,X),
	atom_codes(B,X),
	hdrug_util:concat_all([gap_hfc_,F,B],Fun),
	functor(Term,Fun,6),
	arg(1,Term,Arg1),
	arg(2,Term,Arg2),
	arg(3,Term,Arg3),
	arg(4,Term,Arg4),
	arg(5,Term,Arg5),
	arg(6,Term,Arg6).


functor3(F/A) :-
	goal_fa(F,A).

%pair3(p(F,A,F2,A2)) :-
%	goal_fa(F,A),
%	goal_fa(F2,A2).

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(lists,library(lists),all).
goal_fa(F,A) :-
	(   grammar:top_category(Term)
	;   hc_in:headed_rule(H,M,Ls,Rs,_),
	    (  member(Term,Ls)
	    ;  member(Term,Rs)
	    ;  Term=H
	    ;  Term=M
	    )
	),
	functor(Term,F,A).

generalize_pairs3([]).
generalize_pairs3([P|T]) :-
	generalize_pair3(P),
	generalize_pairs3(T).

an_hfc(Term0,P,Q,Term0,P,Q).
an_hfc(Term,P,Q,Term0,P0,Q0) :-
%	hc_in:hfc(x(Term0,P0,Q0),x(Term,P,Q)).
	hc_in:db(Term0,P0,Q0,Term,P,Q).

generalize_pair3(p(F,A,F2,A2)) :-
	functor(Term0,F,A),
	functor(Term,F2,A2),
	findall(head_corner_table(Term0,P0,Q0,Term,P,Q),an_hfc(Term0,P0,Q0,Term,P,Q),List0),
	(  generalize_term(List0,GeneralTerm)
	-> GeneralTerm=head_corner_table(X1,X2,X3,X4,X5,X6),
	   hfc_functor(F,A,NewTerm,X4,X5,X6,X1,X2,X3),
	   assertz(hc_in:NewTerm)
	;  true
	).

generalize_term([H|T],Term) :-
	generalize_term0(T,H,Term).

generalize_term0([],Term,Term).
generalize_term0([H|T],Term0,Term) :-
	terms:term_subsumer(H,Term0,Term1),
	generalize_term0(T,Term1,Term).

generalize_pairs4([]).
generalize_pairs4([P|T]) :-
	generalize_pair4(P),
	generalize_pairs4(T).

an_hfc4(Term0,P,Q,Term,R,S) :-
	an_hfc(Term0,P,Q,Term,R,S),
	\+ \+ ( lex_in:syn_lex(_,_,Term,Ids),
                lists:member(Id,Ids),
                lex_in:total_lex(Id,Term,_,_)
	      ).
%	\+ \+ lex_in:lex(_,Term,Term,_,_).

generalize_pair4(p(F,A,F2,A2)) :-
	functor(Term0,F,A),
	functor(Term,F2,A2),
	findall(head_corner_table(Term0,P0,Q0,Term,P,Q),an_hfc4(Term0,P0,Q0,Term,P,Q),List0),
	(  generalize_term(List0,GeneralTerm)
	-> GeneralTerm=head_corner_table(X1,X2,X3,X4,X5,X6),
	   hfc_functor4(F,A,NewTerm,X4,X5,X6,X1,X2,X3),
	   assertz(hc_in:NewTerm)
	;  true
	).


generalize_pairs5([]).
generalize_pairs5([P|T]) :-
	generalize_pair5(P),
	generalize_pairs5(T).

an_hfc5(Term0,P,Q,Term,R,S) :-
	an_hfc(Term0,P,Q,Term,R,S),
	\+ \+ hc_in:gap(Term,_).

generalize_pair5(p(F,A,F2,A2)) :-
	functor(Term0,F,A),
	functor(Term,F2,A2),
	findall(head_corner_table(Term0,P0,Q0,Term,P,Q),an_hfc5(Term0,P0,Q0,Term,P,Q),List0),
	(  generalize_term(List0,GeneralTerm)
	-> GeneralTerm=head_corner_table(X1,X2,X3,X4,X5,X6),
	   hfc_functor5(F,A,NewTerm,X4,X5,X6,X1,X2,X3),
	   assertz(hc_in:NewTerm)
	;  true
	).



try_separate(Cat,Syn,SemCons,Sem) :-
	hc_in:separate(Cat,Syn,SemCons,Sem).
try_separate(Cat,Syn,SemCons,Sem) :-
	\+ hc_in:separate(Cat,Syn,SemCons,Sem),
	Cat=Syn,
	SemCons=[],
	Cat=Sem.

replace_each([],C,C,[],_).
replace_each([H|T],C0,C,[PlaceHolder=H|Cons],SemCat) :-
	replace(H,PlaceHolder,C0,C1,no,YesNo,SemCat),
	(  YesNo = no
	-> format("Error in sem_filter/2: could not replace ~q in ~q~n",[H,C0]),
	   fail
	;  replace_each(T,C1,C,Cons,SemCat)
	).

%% replace(TermOld,TermNew,In,Out)
replace(T0,T,I0,I,_B,yes,Sem) :-
	T0 == I0,
	!,
	Sem=T0,
	T = I.
replace(_T0,_T,I0,I,B,B,_) :-
	var(I0),
	!,
	I0=I.
replace(T0,T,I0,I,B0,B,SC) :-
	functor(I0,F,A),
	functor(I,F,A),
	functor(SC,F,A),
	replace(A,T0,T,I0,I,B0,B,SC).

replace(0,_,_,_,_,B,B,_) :-
	!.
replace(Arg,T0,T,I0,I,B0,B,SC) :-
	arg(Arg,I0,A0),
	arg(Arg,I ,A ),
	arg(Arg,SC,Asc),
	replace(T0,T,A0,A,B0,B1,Asc),
	Arg2 is Arg-1,
	replace(Arg2,T0,T,I0,I,B1,B,SC).

head_corner_table :-
	findall(H/M,a_hc(H,M),Pairs0),
	add_db(Pairs0,Pairs,[]),
	process_tr(Pairs).

process_tr([]).
process_tr([Ref|T]) :-
%	length(T,Len), format(user_error,"length: ~w~n",[Len]),
	findall(A/B,new_pair(Ref,A/B),Pairs0),
%	length(Pairs0,Len1), format(user_error,"length: ~w~n",[Len1]),
	add_db(Pairs0,Pairs1,T),
	process_tr(Pairs1).

new_pair(Ref,x(A,B,C)/x(G,H,I)) :-
	hc_in:clause(db(A,B,C,D,E,F),_,Ref),
	hc_in:db(D,E,F,G,H,I).
	

add_db([],P,P).
add_db([H/M|Tail],P0,P) :-
	add_db0(H,M,P0,P1),
	add_db(Tail,P1,P).

add_db0(x(H,R0,R),x(M,Q0,Q),P0,P) :-
	functor(H,Hf,Ha),
	functor(Hd,Hf,Ha),
	functor(M,Mf,Ma),
	functor(Md,Mf,Ma),
	(  clause(hc_in:db(Hd,S0,S,Md,T0,T),_,Ref)  % pair exists
	-> (  terms:subsumes_chk(x(Hd,S0,S)/x(Md,T0,T),x(H,R0,R)/x(M,Q0,Q))
	   -> P0=P
	   ;  erase(Ref),
	      terms:term_subsumer(x(H,R0,R)/x(M,Q0,Q),x(Hd,S0,S)/x(Md,T0,T),
				  x(H2,V0,V)/x(M2,W0,W)),
	      assertz(hc_in:db(H2,V0,V,M2,W0,W),RefX),
	      P0 = [RefX|P]
	   )
	;  assertz(hc_in:db(H,R0,R,M,Q0,Q),RefX),        % new pair
	   P0 = [RefX|P]
	).







