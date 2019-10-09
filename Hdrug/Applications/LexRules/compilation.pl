% compilation of a grammar
%
% this is all a bit messy, because some grammar definitions
% affect the way other grammar definitions are compiled...
% furthermore, because partial evaluation of `macro'-like
% definitions are used (this implies that some clauses are
% invalid once e.g. the file templates is recompiled).
%
% order: types < decl < templates < inflection < rules < approx
%
% note that compile(File) is different from compile_grammar_file(File)
% because in the latter case certain definitions of term_expansion
% from library(feature) are incorporated.

%:- use_module( library(hdrug_feature) ).

:- use_module( wlists,               [ wappend/3,
				       wreverse/2           ]).

:- del_expansion(exp).
:- del_expansion(my_clause).

exp(l(Word0,Cat),Body0,NHead,[]) :-
	prolog_conjunction(Body,Body0),
	call(Body),
	( atomic(Word0) ->
	  Word = Word0,
          Words = [Word0]
        ; Word0 = Words,
          concat_all(Words,Word,' ')
        ),
	gen_sym(Name,Word),
	tab(1),write(Name),ttyflush,
	lor2(Words,Name,Cat,NHead).

%%%%%% lor(Word0,Cat,Body,lexicon_clause(lexicon(Cat),Body)).

lor2(Words,Name,Cat,lexicon(Words,Cat,Name)).
lor2(Words,Name,Cat,ign_lexicon(Words,CatNoSem,Name)) :-
	ignore_semantics(Cat,CatNoSem).



exp(h_rule(H,M,Ls,Rs,Name),Body0,h_rule(H,M,Ls,Rs,Name),[]) :-
	!,
	prolog_conjunction(Body,Body0),
	call(Body),
	write(Name),nl,ttyflush.

% a copy for debug/inspection etc.
my_clause(Head0,Body0,Head,[]) :-
	hdrug_flag(my_clause,on(Head0,Body0,Head)).

% the original clause is given in any case as well:
my_clause(H,B,H,B).

:- addz_expansion(my_clause).
:- addz_expansion(exp).


compile_grammar_files([]).
compile_grammar_files([H|T]) :-
	compile_grammar_file(H),
	compile_grammar_files(T).

reconsult_grammar_files([]).
reconsult_grammar_files([H|T]) :-
	reconsult_grammar_file(H),
	reconsult_grammar_files(T).

compile_grammar_file(File) :-
	hdrug_flag(eval_feature,_,on),    % uses term_expansion from
	use_module(File),              % library(feature)
	hdrug_flag(eval_feature,_,off).

reconsult_grammar_file(File) :-
	hdrug_flag(eval_feature,_,on),
	reconsult(File),
	hdrug_flag(eval_feature,_,off).

compile_grammar :-
	use_module(types),
	prolog_flag(character_escapes,OldChar,off),
	compile_grammar_files([decl,templates,inflection,lexicon,rules,approx]),
	%% compiling the lexicon is extremely slow...
	%% and we dont gain anything, because lexical entries are
	%% asserted before parsing starts, anyway.
	prolog_flag(character_escapes,_,OldChar),
	report_count_edges(h_rule(_,_,_,_,_)),
	report_count_edges(lexicon(_,_,_)).

reconsult_grammar :-
	reconsult(types),
	prolog_flag(character_escapes,OldChar,off),
	reconsult_grammar_files([decl,templates,inflection,lexicon,rules,approx]),
	prolog_flag(character_escapes,_,OldChar),
	report_count_edges(h_rule(_,_,_,_,_)),
	report_count_edges(lexicon(_,_,_)).

%%%% PRODUCED BY TERM_EXPANSION ON THE BASIS OF THE GRAMMAR:
%%%% lexicon(Word,Cat,Name)
%%%% ign_lexicon(Word,Cat,Name)
%%%% h_rule(H,M,LDs,Rds,Name)

% DEFINED IN TERMS OF THESE:
%%%% ign_h_rule
%%%%     r_rule
%%%% ign_r_rule
%%%%     l_rule
%%%% ign_l_rule

% lexicon, ign_lexicon, rule, ign_rule already exist.
% everything is in module user, so no recompilation if parser
% is reconsulted.

ign_h_rule(H,M,L,R,Name) :-
	h_rule(H0,M0,L0,R0,Name),
	ignore_semantics(H0,H),
	ignore_semantics(M0,M),
	ignore_semantics_list(L0,L),
	ignore_semantics_list(R0,R).

:- block ignore_semantics_list(-,-).
ignore_semantics_list([],[]).
ignore_semantics_list([H0|T0],[H|T]):-
	ignore_semantics(H0,H),
	ignore_semantics_list(T0,T).	

r_rule(Rm,M,Left,Name) :-
	h_rule(H,M,Ls,Rs,Name),
	wreverse(Rs,RRs),
	wappend(RRs,[H|Ls],[Rm|Left]).

ign_r_rule(Rm,M,Left,Name) :-
	ign_h_rule(H,M,Ls,Rs,Name),
	wreverse(Rs,RRs),
	wappend(RRs,[H|Ls],[Rm|Left]).

l_rule(L,M,Left,Name) :-
	h_rule(H,M,Ls,Rs,Name),
	wreverse(Ls,RLs),
	wappend(RLs,[H|Rs],[L|Left]).

ign_l_rule(L,M,Left,Name) :-
	ign_h_rule(H,M,Ls,Rs,Name),
	wreverse(Ls,RLs),
	wappend(RLs,[H|Rs],[L|Left]).

	
