:- module(lc_compile,[ lc_compile_grammar/0 ]).

compile_grammar(File) :-
	user:ensure_grammar_compiled_for_parser(lc,File).

dump_grammar:-
	(  lc_in:current_predicate(_,A),
	   lc_in:clause(A,B,_),
	   my_portray_clause(A,B),
%%	   portray_clause((lc_in:A:-lc_in:B)),
	   fail
	;  true
	).

my_portray_clause(Head,true) :-
	!,
	portray_clause(lc_in:Head).
my_portray_clause(Head,Body) :-
	portray_clause((lc_in:Head :- lc_in:Body)).


clean_grammar :-
	(  lc_in:current_predicate(_,X),
	   functor(X,F,A),
	   abolish(lc_in:(F/A)),
	   fail
	;  true
	).

compile_sem_filter :-
	(  grammar:sem_filter(Vars,Cat),
	   replace_each(Vars,Cat,Syn,SemCons,SemCat),
	   assertz(lc_in:separate(Cat,Syn,SemCons,SemCat)),
	   fail
	;  true
	).

compile_rules :-
	(  grammar:grammar_rule(Id,M,Ds),
	   try_separate(M,SynM,_,_),
	   compile_lcr(Ds,M,Id,SynM,Rule1,Rule2),
	   assertz(lc_in:Rule1),
	   assertz(lc_in:Rule2),
	   fail
	;  true
	).

compile_grammar :-
	clean_grammar,
	lc_in:assertz(unknown_predicate_handler(_,fail)),
	format(user_error,"compiling sem_filter..~n",[]),
	compile_sem_filter,
	format(user_error,"compiling sem_filter done~n",[]),
	format(user_error,"compiling rules..~n",[]),
	compile_rules,
	format(user_error,"compiling rules done~n",[]),
	format(user_error,"compiling left-corner table..~n",[]),
	left_corner_table,
	format(user_error,"compiling left-corner table done~n",[]),
	format(user_error,"indexing left-corner table..~n",[]),
	s_index_table(all),
	format(user_error,"indexing left-corner table done~n",[]),
	format(user_error,"indexing left-corner_lex table..~n",[]),
	s_index_table(lex),
	format(user_error,"indexing left-corner_lex table done~n",[]),
	format(user_error,"indexing left-corner_gap table..~n",[]),
	s_index_table(gap),
	format(user_error,"indexing left-corner_gap table done~n",[]),
	hdrug_util:report_count_edges_pred(lc_in:lc_rule/4),
	hdrug_util:report_count_edges_pred(lc_in:gap/2),
	hdrug_util:report_count_edges_pred(lc_in:left_corner_table/2),
	hdrug_util:report_count_edges_pred(lc_in:left_corner_table_lex/2),
	hdrug_util:report_count_edges_pred(lc_in:left_corner_table_gap/2),
	retractall(lc_in:db(_,_)).

% compile_lcr(+Ds,+M,+Id,+M2,+Ds2,-Rule)
% no daughters: gap
compile_lcr([],M,Name,SynM,gap(SynM,Name),gap_i(Name,M)).

compile_lcr([D0|Ds],M,Id,SynM,lc_rule(D0,SynM,Ds,Id),
	                      lc_rule_i(Id,D0,M,Ds)).


a_lc(Head,Mother) :-
	lc_in:lc_rule(Head,Mother,_,_).

s_index_table(Mode) :-
	%% for each pair of functor, find the generalization of all
        %% info in lc. With indexing, now.
	findall(Functor,functor3(Functor),Functors0),
	sort(Functors0,Functors),
	findall(p(A,B,C,D),(  member(A/B,Functors),
			      member(C/D,Functors)
			   ), Pairs),
	generalize_pairs(Pairs,Mode),
	link_clauses(Mode).

link_clauses(Mode) :-
	findall(F/A,goal_fa(F,A),Pairs0),
	sort(Pairs0,Pairs),
	link_clauses(Pairs,Mode).

link_clauses([],_).
link_clauses([F/A|Tail],Mode) :-
	lc_functor(Mode,F,A,Term,Arg2,Arg1),
	functor(Arg1,F,A),
	(  \+ lc_in:Term -> true
	;  Mode=all ->
	       lc_in:assertz(
			    (left_corner_table(Arg1,Arg2):-
			    Term))
	;  Mode=lex ->
	       lc_in:assertz(
			    (left_corner_table_lex(Arg1,Arg2):-
			    Term))
	;  Mode=gap ->
	       lc_in:assertz(
			    (left_corner_table_gap(Arg1,Arg2):-
			    Term))
	),
	link_clauses(Tail,Mode).


lc_functor(all,F,A,Term,Arg1,Arg2):-
	number_codes(A,X),
	atom_codes(B,X),
	hdrug_util:concat_all([lc_,F,B],Fun),
	functor(Term,Fun,2),
	arg(1,Term,Arg1),
	arg(2,Term,Arg2).
lc_functor(lex,F,A,Term,Arg1,Arg2):-
	number_codes(A,X),
	atom_codes(B,X),
	hdrug_util:concat_all([lc_lex_,F,B],Fun),
	functor(Term,Fun,2),
	arg(1,Term,Arg1),
	arg(2,Term,Arg2).
lc_functor(gap,F,A,Term,Arg1,Arg2):-
	number_codes(A,X),
	atom_codes(B,X),
	hdrug_util:concat_all([lc_gap_,F,B],Fun),
	functor(Term,Fun,2),
	arg(1,Term,Arg1),
	arg(2,Term,Arg2).

functor3(F/A) :-
	goal_fa(F,A).

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(lists,library(lists),all).
goal_fa(F,A) :-
	(   grammar:top_category(Term)
	;   lc_in:lc_rule(H,M,Rs,_),
	    (  member(Term,Rs)
	    ;  Term=H
	    ;  Term=M
	    )
	),
	functor(Term,F,A).

generalize_pairs([],_mode).
generalize_pairs([P|T],M) :-
	generalize_pair(P,M),
	generalize_pairs(T,M).

an_lc(all,Term0,Term0).
an_lc(all,Term,Term0) :-
	lc_in:db(Term0,Term).

an_lc(gap,Term0,Term) :-
	an_lc(all,Term0,Term),
	\+ \+ lc_in:gap(Term,_).

an_lc(lex,Term0,Term) :-
	an_lc(all,Term0,Term),
	\+ \+ ( lex_in:syn_lex(_,_,Term,Ids),
                lists:member(Id,Ids),
                lex_in:total_lex(Id,Term,_,_)
	      ).
%%	\+ \+ lex_in:lex(_,Term,Term,_,_).

generalize_pair(p(F,A,F2,A2),Mode) :-
	functor(Term0,F,A),
	functor(Term,F2,A2),
	findall(left_corner_table(Term0,Term),an_lc(Mode,Term0,Term),List0),
	(  generalize_term(List0,GeneralTerm)
	-> GeneralTerm=left_corner_table(X1,X2),
	   lc_functor(Mode,F,A,NewTerm,X2,X1),
	   assertz(lc_in:NewTerm)
	;  true
	).

generalize_term([H|T],Term) :-
	generalize_term0(T,H,Term).

generalize_term0([],Term,Term).
generalize_term0([H|T],Term0,Term) :-
	terms:term_subsumer(H,Term0,Term1),
	generalize_term0(T,Term1,Term).

try_separate(Cat,Syn,SemCons,Sem) :-
	lc_in:separate(Cat,Syn,SemCons,Sem).
try_separate(Cat,Syn,SemCons,Sem) :-
	\+ lc_in:separate(Cat,Syn,SemCons,Sem),
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
replace(T0,T,I0,I,_B,yes,T0) :-
	T0 == I0,
	!,
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

left_corner_table :-
	findall(H/M,a_lc(H,M),Pairs0),
	add_db(Pairs0,Pairs,[]),
	process_tr(Pairs).

process_tr([]).
process_tr([Ref|T]) :-
	findall(A/B,new_pair(Ref,A/B),Pairs0),
	add_db(Pairs0,Pairs1,T),
	process_tr(Pairs1).

new_pair(Ref,A/C) :-
	lc_in:clause(db(A,B),_,Ref),
	lc_in:db(B,C).


add_db([],P,P).
add_db([H/M|Tail],P0,P) :-
	add_db0(H,M,P0,P1),
	add_db(Tail,P1,P).

add_db0(H,M,P0,P) :-
	functor(H,Hf,Ha),
	functor(Hd,Hf,Ha),
	functor(M,Mf,Ma),
	functor(Md,Mf,Ma),
	(  lc_in:clause(db(Hd,Md),_,Ref)  % pair exists
	-> (  terms:subsumes_chk(Hd/Md,H/M)
	   -> P0=P
	   ;  erase(Ref),
	      terms:term_subsumer(H/M,Hd/Md,H2/M2),
	      lc_in:assertz(db(H2,M2),RefX),
	      P0 = [RefX|P]
	   )
	;  lc_in:assertz(db(H,M),RefX),        % new pair
	   P0 = [RefX|P]
	).









