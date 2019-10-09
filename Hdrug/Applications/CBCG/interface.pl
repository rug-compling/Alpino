%%%%%%%%%%%%%%%%%%%%%%%%% io etc. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(lists,library(lists),all).

%%%% check_and_display_result/1 is called by the parser	  

%check_and_display_result(t(Sign,Ds)) :-
%	startsymbol(Sign),
%	display_tree(t(Sign,Ds),0),
%	display_semantics(Sign),
%	ttyflush.

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(matrix(syn),X,X).

graphic_label(matrix(syn),lex(W),W).
graphic_label(matrix(syn),tree(L,_,_),L).

graphic_daughter(matrix(syn),No,tree(_,_,Ds),D) :-
    lists:nth(No,Ds,D).

%% graphic_daughter(matrix(syn),1,tree(lex(Word),_,[]),lex(Word)).

graphic_path(syn,Obj,Obj).

graphic_label(syn,tree(Sign,_,[_|_]),Label) :-
    cat_symbol(Sign,Label).
graphic_label(syn,tree(lex(W),_,[]),W).

graphic_daughter(syn,No,tree(_,_,[H|T]),D) :-
    lists:nth(No,[H|T],D).

show_node(_,tree(Sign0,[_|_],_)) :-
    shorten_label(Sign0,Sign),
    show(fs,clig,[value(Sign)]).

graphic_path(sem,Obj,Obj).
graphic_label(sem,tree(Sign,_,[_|_]),Label) :-
    semantics(tree(Sign,_,_),Label).
graphic_label(sem,tree(W,_,[]),W).

graphic_daughter(sem,No,tree(_,_,[H|T]),D) :-
    lists:nth(No,[H|T],D).

cat_symbol(Sign,Sym) :-
	( Sign:gap ==> empty -> cat_symbol0(Sign,Sym)
	; cat_symbol0(Sign,Sym0),
	  Sign:gap <=> Gap,
	  cat_symbol0(Gap,GapSym),
	  Sym = Sym0-GapSym
	).
	
cat_symbol0(Sign,Cat) :-
	Sign:cat <=> Var,
	var(Var),!,
	Var = Cat.
cat_symbol0(Sign,Cat) :-
	( Sign:cat ==> n
        ; Sign:cat ==> np
	; Sign:cat ==> pref
	; Sign:cat ==> pp
	; Sign:cat ==> adj
	),
	Sign:cat <=> Cat.
cat_symbol0(Sign,s) :-
	Sign:cat ==> sent.
cat_symbol0(Sign,Symbol) :-
	( Sign <=> Val/Arg -> Symbol = ValSym/ArgSym
	; Sign <=> Arg\Val,
	  Symbol = ArgSym\ValSym
	),
	cat_symbol0(Val,ValSym),
	cat_symbol0(Arg,ArgSym).



%%%%%%%%%%%%%%%% grammar compilation (for efficiency only) %%%%%%%%%%%%%%%%
:- dynamic lex/2, rule0/4.

compile_lexicon :-
	retractall(lex(_,_)),
	( lexic(Word,Sign), assert(lex(Word,Sign)), 
	  write(Word), write(', '), ttyflush, fail
	; true
	).

compile_rules :-
	retractall(rule0(_,_,_,_)),
	( rule(Name,Lhs,Rhs,Head), assert(rule0(Name,Lhs,Rhs,Head)), fail
	; true
	).

shorten_label(Sign0,Sign) :-
	Sign0:dir <=> Sign:dir,
	Sign0:mor <=> Sign:mor,
	Sign0:rel <=> Rel0,
	Sign:rel <=> Rel,
	shorten_label_rel(Rel0,Rel),
	Sign0:gap <=> Gap0,
	Sign:gap <=> Gap,
	shorten_label_gap(Gap0,Gap),
	Sign0:val <=> Val0,
	Sign:val <=> Val,
	shorten_label_val(Val0,Val),
	Sign0:arg <=> Arg0,
	Sign:arg <=> Arg,
	shorten_label_val(Arg0,Arg),
	Sign0:cat <=> Cat0,
	Sign:cat <=> Cat,
	shorten_label_cat(Cat0,Cat),
	Sign0:sem <=> Sem0,
	Sign:sem <=> Sem,
	shorten_sem(Sem0,Sem).

:- initialize_flag(shorten,nosem).

shorten_sem(S0,S) :-
	(  hdrug_flag(shorten,nosem)
	-> true
	;  S0 = S
	).

shorten_label_rel(empty,_) :- !.
shorten_label_rel(X,X).


shorten_label_gap(empty,_) :- !.
shorten_label_gap(X0,X) :-
	shorten_label(X0,X).

shorten_label_cat(nil,_) :- !.
shorten_label_cat(X,X).

shorten_label_val(nil,_) :- !.
shorten_label_val(X0,X) :-
	shorten_label(X0,X).

shorten_label_l([],[]).
shorten_label_l([H0|T0],[H|T]) :-
	shorten_label(H0,H),
	shorten_label_l(T0,T).


% this is all stolen from lexical rules application
stem_exists :-
	(  a_stem(_,_)
        -> tcl("set stem_exists 1"), 	
	   send_stems
        ;  tcl("set stem_exists 0")
        ).


send_stems :-
	format(user_error,"Updating lexical entries...",[]),
	tcl("set stems(max) 0"),
	findall(Name,a_stem(Name,_),Names0),
	sort(Names0,Names),
	update_array(Names,stems),
	format(user_error,"Done.~n",[]).

all_stems(Name,Bag) :-
	findall(clause(stem(Name,FS),[]),a_stem(Name,FS),Bag).

a_stem(Name,FS) :-
	call_residue(lex(Name,FS0),_),   % for now..
	shorten_label(FS0,FS),
	atomic(Name).
a_stem(Name,FS) :-
	call_residue(lex([te,X],FS0),_),  % id.
	shorten_label(FS0,FS),
	concat('te ',X,Name).

show_stem_prolog_text(Stem) :-
	all_stems(Stem,Values),
	show(term(print),user,Values).

show_stem_prolog_matrix(Stem) :-
	all_stems(Stem,Values),
	show(fs([]),user,Values).

show_stem_latex_text(Stem) :-
	all_stems(Stem,Values),
	show(term(print),latex,Values).

show_stem_latex_matrix(Stem) :-
	all_stems(Stem,Values),
	show(fs([]),latex,Values).

show_stem_clig_matrix(Stem) :-
	all_stems(Stem,Values),
	show(fs([]),clig,Values).

show_stem_tk_matrix(Stem) :-
    all_stems(Stem,Values),
    show(fs([]),tk,Values).

show_stem_tk_text(Stem) :-
	all_stems(Stem,Values),
	show(term(print),tk,Values).


%%% rules
rule_exists :-
	(  a_rule(_,_)
        -> tcl("set rule_exists 1"), 	
	   send_rules
        ;  tcl("set rule_exists 0")
        ).


send_rules :-
	format(user_error,"Updating rules...",[]),
	tcl("set rules(max) 0"),
	findall(Name,a_rule(Name,_),Names0),
	sort(Names0,Names),
	update_array(Names,rules),
	write('Done.'),nl.

all_rules(Name,Bag) :-
	findall(clause(rule(Name,FS),[]),a_rule(Name,FS),Bag).

a_rule(Name,tree(M,_,L)) :-
	rule(Name,M0,L0,_),
	shorten_label(M0,M),
	shorten_label_l(L0,L).

show_rule_prolog_text(Rule) :-
    all_rules(Rule,Values),
    show(term(print),user,Values).

show_rule_prolog_matrix(Rule) :-
    all_rules(Rule,Values),
    show(fs([]),user,Values).

show_rule_latex_text(Rule) :-
    all_rules(Rule,Values),
    show(term(print),latex,Values).

show_rule_latex_matrix(Rule) :-
    all_rules(Rule,Values),
    show(fs([]),latex,Values).

show_rule_clig_matrix(Rule) :-
    all_rules(Rule,Values),
    show(fs([]),clig,Values).

show_rule_tk_matrix(Rule) :-
    all_rules(Rule,Values),
    show(fs([]),tk,Values).

show_rule_tk_text(Rule) :-
    all_rules(Rule,Values),
    show(term(print),tk,Values).


