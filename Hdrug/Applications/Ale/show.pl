:- use_module(change).

show_object_default2(No) :-
    show_it(clig,tree,No).
show_object_default3(No) :-
    show_it(clig,object,No).

% show_it(Output,Type,Thing)
% this predicate is called from Tcl to show some things
% Output is one of ale, tk, latex
% Type is one of rules, clauses, lexs, macros, lex_rules, types, object, tree
% Thing is the name of a specific object
show_it(ale,Type,Thing) :-
    (	show_ale(Type,Thing)
    ;	true
    ),
    nl,
    write('| ?- '),
    ttyflush.			% succeeds after all have been shown..

show_it(tk,Type,Thing) :-
    show_tk(Type,Thing).

show_it(latex,Type,Thing) :-
    show_latex(Type,Thing).

show_it(clig,Type,Thing) :-
    show_clig(Type,Thing).

% show_ale(Type,Thing)
% uses the standard Ale way of showing things
show_ale(rules,X) :-
    rule(X).

show_ale(clauses,X0) :-
    atom_term(X0,X),
    show_clause(X).

show_ale(lexs,X) :-
    lex(X).

show_ale(macros,X) :-
    macro(X).

show_ale(lex_rules,X) :-
    lex_rule(X).

show_ale(types,X) :-
    show_type(X).

show_ale(object,X) :-
    object(X,o(ale(A,B,C,_),_,_)),
    pp_fs(A-B,C).

% this is not part of standard Ale
show_ale(tree,X) :-
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(derivation),user,[value(D)]).

show_ale(signature,Type) :-
    show(tree(sig),user,[value(Type)]).

% tk_ale(Thing,Iqs)
% Thing is one of value(Term) clause(Head) clause(Head,Body)
% Iqs are the inequalities
% General predicate to view things thru Tk
tk_ale(Thing,Iqs) :-
    ale_to_hdrug(Thing,Iqs,Result), % change.pl
    show(fs,tk,[changed(Result)]).

% clig_ale(Thing,Iqs)
% Thing is one of fs(Term) clause(Head) clause(Head,Body)
% Iqs are the inequalities
% General predicate to view things thru Tk
clig_ale(Thing,Iqs) :-
    ale_to_hdrug(Thing,Iqs,Result), % change.pl
    show(fs,clig,[changed(Result)]).

latex_ale(Thing,Iqs) :-
    ale_to_hdrug(Thing,Iqs,Result), % change.pl
    show(fs,latex,[changed(Result)]).

%% show_latex(Type,Thing)
%% shows all Types with name Thing thru Latex.

show_latex(tree,X) :-
    !,
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(derivation),latex,[value(D)]).

show_latex(matrix_tree,X) :-
    !,
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(user(derivation)),latex,[value(D)]).

show_latex(signature,Type) :-
    !,
    show(tree(sig),latex,[value(Type)]).

show_latex(Type,Thing) :-
    findall(FS/Iqs,obtain(Type,Thing,FS,Iqs),Things0),
    change_things(Things0,Things),
    show(fs,latex,Things).

change_things([],[]).
change_things([F/I|T0],[changed(Term)|T]) :-
    ale_to_hdrug(F,I,Term),
    change_things(T0,T).

% show_tk(Type,Thing)
show_tk(tree,X) :-
    !,
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(derivation),tk,[value(D)]).

show_tk(matrix_tree,X) :-
    !,
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(user(derivation)),tk,[value(D)]). 

show_tk(signature,Type) :-
    !,
    show(tree(sig),tk,[value(Type)]).

show_tk(Type,Thing) :-
    (	obtain(Type,Thing,Fs,Iqs),
	tk_ale(Fs,Iqs),
	fail
    ;	true
    ).

% show_clig(Type,Thing)
show_clig(tree,X) :-
    !,
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(derivation),clig,[value(D)]).

show_clig(matrix_tree,X) :-
    !,
    object(X,o(ale(_,_,_,D),_,_)),
    show(tree(user(derivation)),clig,[value(D)]). 

show_clig(signature,Type) :-
    !,
    show(tree(sig),clig,[value(Type)]).

show_clig(Type,Thing) :-
    (	obtain(Type,Thing,Fs,Iqs),
	clig_ale(Fs,Iqs),
	fail
    ;	true
    ).


%%% obtain/4 does all the real work, mostly taking the ordinary
%%% ale view_thing definition, and instead of writing something
%%% building an appropriate datastructure.
obtain(object,No,fs(X-Y),Ins) :-
    object(No,o(ale(X,Y,Ins,_),_,_)).

obtain(lexs,Word,fs(Tag-SVs),IqsOut) :-
    lex(Word,Tag,SVs,IqsIn),
    extensionalise(Tag,SVs,IqsIn),
    check_inequal(IqsIn,IqsOut).

obtain(macros,VarName,clause(macro(MacroSat,Tag-bot)),IqsMid3) :-
    (	VarName macro Desc   ),
    add_to(Desc,Tag,bot,[],IqsMid),
    mg_sat_goal(VarName,MacroSat,IqsMid,IqsMid2),
    MacroSat =.. [_|MacroArgs],
    deref_list([Tag-bot|MacroArgs],ArgsOut),
    extensionalise_list(ArgsOut,IqsMid2),
    check_inequal(IqsMid2,IqsMid3),
    duplicates_list(ArgsOut,IqsMid3,[],_,[],_,0,_).

obtain(rules,Rule,clause(Head,Body),IqsMid3) :-
    (	Rule rule Moth ===> DtrsDesc),
    term_atom(Rule,RuleAtom),
    add_to(Moth,TagMoth,bot,[],IqsMid),
    satisfy_dtrs(DtrsDesc,DtrCats,[],Dtrs,IqsMid,IqsMid2),
    deref_list([TagMoth-bot|DtrCats],CatsOut),
    extensionalise_list(CatsOut,IqsMid2),
    check_inequal(IqsMid2,IqsMid3),
    duplicates_list(CatsOut,IqsMid3,[],_,[],_,0,_),
    rule_to_clause(RuleAtom,TagMoth-bot,Dtrs,Head,Body).

obtain(lex_rules,RuleName,clause(lex_rule(Tag1-bot,Tag2-bot),Body),IqsMid4) :-
    (	(RuleName lex_rule Desc1 **> Desc2 if Cond morphs Morphs)
    ;	(RuleName lex_rule Desc1 **> Desc2 morphs Morphs),
	Cond = none
    ),
    add_to(Desc1,Tag1,bot,[],IqsMid),
    add_to(Desc2,Tag2,bot,IqsMid,IqsMid2),
    mg_sat_goal(Cond,Goal,IqsMid2,IqsMid3),
    Goal =.. [_Rel|Args],
    deref_list([Tag1-bot,Tag2-bot|Args],ArgsOut),
    extensionalise_list(ArgsOut,IqsMid3),
    check_inequal(IqsMid3,IqsMid4),
    !,
    make_lr_body(Morphs,Body,Body1),
    make_lr_body2(Goal,Body1).

obtain(empty,empty,fs(Tag-SVs),IqsOut) :-
    empty_cat(_,-1,Tag,SVs,IqsIn,_dtrs,_rulename),
    extensionalise(Tag,SVs,IqsIn),
    check_inequal(IqsIn,IqsOut).

obtain(types,Type,clause(type(Type),
			 [  subtypes(SubTypes),
			    supertypes(SuperTypes),
			    immediate_constraints(Cons),
			    most_general_unifier(Ref-SVs)
			 ]), IqsOut ) :-
    immed_subtypes(Type,SubTypes),
  ( setof(T,T2^(sub_type(T,Type),
                T \== Type,
                \+ (sub_type(T2,Type),
                    T2 \== Type, T2 \== T,
                    sub_type(T,T2))),SuperTypes),
    !
  ; SuperTypes = []
  ),
  (\+ current_predicate(cons,(_ cons _)),
   !, Cons=none
  ;Type cons Cons goal _,!
  ;Type cons Cons,!
  ;Cons = none),
  ( var(Cons) -> Cons = none ; true ),
  add_to(Type,Tag,bot,[],IqsIn),
  deref(Tag,bot,Ref,SVs),
  extensionalise(Ref,SVs,IqsIn),
  check_inequal(IqsIn,IqsOut).

obtain(clauses,Spec0,Clause,IqsMid3) :-
    atom_term(Spec0,Spec),
    (	Spec = Name/Arity
    ->  true
    ;	Spec = Name
    ),
    (	Head if Body  ),
    Head =.. [Name|Args],
    length(Args,Arity),
    satisfy_dtrs_goal(Head,Cats,CatsRest,HeadGoal,[],IqsMid),
    satisfy_dtrs_goal(Body,CatsRest,[],BodyGoal0,IqsMid,IqsMid2),
    deref_list(Cats,CatsOut),
    extensionalise_list(CatsOut,IqsMid2),
    check_inequal(IqsMid2,IqsMid3),
    duplicates_list(CatsOut,IqsMid3,[],_,[],_,0,_),
    hdrug_util:prolog_conjunction(BodyGoal0,BodyGoal),
    (	BodyGoal=[] 
    ->	Clause = clause(HeadGoal)
    ;	Clause = clause(HeadGoal,BodyGoal)
    ).

% this defines the action upon clicking on a relation name
% in the canvas (e.g. if clauses are being shown there).
show_relation(F/A) :-
    show_tk(clauses,F/A).

% a few aux predicates
make_lr_body((A becomes B,Rest),[becomes(A,B)|Tail0],Tail) :-
    !,
    make_lr_body(Rest,Tail0,Tail).

make_lr_body((A becomes B),[becomes(A,B)|T],T).

%% ??? suspicious
make_lr_body2(true,[]):-!.
make_lr_body2(none,[]):-!.
make_lr_body2(G,[G]).

rule_to_clause(Name,Mother,Dtrs,rule(Name,Mother),Body) :-
    dtrs_to_body(Dtrs,Body).

dtrs_to_body([],[]).
dtrs_to_body([cats>C|T0],[cats(C)|T]) :-
    dtrs_to_body(T0,T).

dtrs_to_body([sem_head>C|T0],[sem_head(C)|T]) :-
    dtrs_to_body(T0,T).

dtrs_to_body([goal>C|T0],[goal(C)|T]) :-
    dtrs_to_body(T0,T).

dtrs_to_body([sem_goal>C|T0],[sem_goal(C)|T]) :-
    dtrs_to_body(T0,T).

dtrs_to_body([cat>C|T0],[cat(C)|T]) :-
    dtrs_to_body(T0,T).

goals_to_body((X,Y),[X|T0],T) :-
    !,
    goals_to_body(Y,T0,T).
goals_to_body(X,[X|T],T).

%% defines ways to view `things' as trees.
:- use_module(lists,library(lists), [nth/3]).

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4,
	     show_node/2,
	     show_node2/2,
	     show_node3/2,
	     tk_tree_show_node_help/2.

% the signature as a tree
graphic_path(signature,T,T).

graphic_label(signature,Type,Type).

graphic_daughter(signature,No,Type,Sub) :-
    immed_subtypes(Type,Subs),
    lists:nth(No,Subs,Sub).

%% the signature as a graph
%% as per special request by
%% Alexandr.Rosen@ff.cuni.cz (Alexandr Rosen)
graphic_path(sig,Type,Tree):-
    hdrug_show:tree_def_to_tree(signature,Type,Tree0),
    replace_equals_with_keys(Tree0,Tree1,[],_),
    remove_var_labels(Tree1,Tree,1,_).

graphic_label(sig,tree(L0,_,_),L) :-
    sig_label(L0,L).

sig_label(t(T),T).
sig_label(l(I),Atom):-
    charsio:format_to_chars('<~w>',[I],Chars),
    atom_codes(Atom,Chars).
sig_label(X,X).

graphic_daughter(sig,No,tree(_,_,Tree),D) :-
    lists:nth(No,Tree,D).


replace_equals_with_keys(tree(L,used=Label,Ds),tree(l(Label),_,[]),His,His):-
    member(tree(L,used=Label,Ds),His),
    !.
replace_equals_with_keys(tree(L,G,Ds0),tree(t(L),G,Ds),His0,His):-
    replace_equals_with_keys_ds(Ds0,Ds,[tree(L,G,Ds0)|His0],His).

replace_equals_with_keys_ds([],[]) --> [].
replace_equals_with_keys_ds([H0|T0],[H|T]) -->
    replace_equals_with_keys(H0,H),
    replace_equals_with_keys_ds(T0,T).

remove_var_labels(tree(l(Label),_,[]),tree(l(Label),_,[]),I,I).
remove_var_labels(tree(t(Type),V,Ds0),Tree,Label0,Label):-
    (	var(V)
    ->  Tree=tree(t(Type),_,Ds),
	Label0=Label1
    ;   V=(used=Label0),
	Tree=tree(l(Label0),_,[tree(t(Type),_,Ds)]),
	Label1 is Label0+1
    ),
    remove_var_labels_ds(Ds0,Ds,Label1,Label).

remove_var_labels_ds([],[],L,L).
remove_var_labels_ds([H0|T0],[H|T],L0,L):-
    remove_var_labels(H0,H,L0,L1),
    remove_var_labels_ds(T0,T,L1,L).


% derivation trees
graphic_path(derivation,ale(_,_,_,EdgeNo),EdgeNo).
graphic_path(derivation,EdgeNo,EdgeNo).

graphic_label(derivation,EdgeNo,Label) :-
    ale_edge(EdgeNo,_,End,_,_,_,_,Label0),
    (	Label0 = lexicon(Label)   %%% only for Ale 3.0
    ->	true
    ;   Label0 == lexicon         %%% only for Ale 3.1
    ->  parsing(Words),
	lists:nth(End,Words,Label)
    ;	Label0=Label
    ).

graphic_daughter(derivation,No,EdgeNo,Daughter) :-
    ale_edge(EdgeNo,_,_,_,_,_,Daughters,_),
    lists:nth(No,Daughters,Daughter).

% this ignores inequalities...
graphic_path(user(derivation),EdgeNo,EdgeNo).

graphic_label(user(derivation),EdgeNo,Tag-SVs) :-
    ale_edge(EdgeNo,_,_,Tag,SVs,_Iqs,_,_).

graphic_daughter(user(derivation),No,EdgeNo,Daughter) :-
    ale_edge(EdgeNo,_,_,_,_,_,Daughters,_),
    lists:nth(No,Daughters,Daughter).

% clicking with leftmost button gives information of that type
show_node(sig,tree(t(Type),_,_)) :-
    show_clig(types,Type).

% clicking with middle mouse button gives local tree
show_node2(sig,tree(t(Type),_,_)) :-
    show(tree(sig),clig,[value(Type)]).

% clicking with third button goes up one level in the tree
show_node3(sig,tree(t(Type),_,_)) :-
    imm_sub_type(Super,Type),
    show(tree(sig),clig,[value(Super)]).

% left mouse button prints corresponding featurestructure
show_node(derivation,No) :-
    ale_edge(No,_,_,Tag,SVs,Iqs,_,_),
    clig_ale(fs(Tag-SVs),Iqs).

% middle mouse button prints the corresponding rule
show_node2(derivation,No) :-
    ale_edge(No,_,_,_,_,_,_,Label),
    (	Label = lexicon(W)
    ->	show_clig(lexs,W)
    ;	show_clig(rules,Label)
    ).
% third mouse button prints edge number (default)

tk_tree_show_node_help(sig,'<1> info <2> down <3> up').
tk_tree_show_node_help(derivation,'<1> avm <2> rule <3> edge #'). 

clig_tree_show_node_help(sig,'<1> info <2> down <3> up').
clig_tree_show_node_help(derivation,'<1> avm <2> rule <3> edge #'). 

show_edge(ale,M,N) :-
    (	nl, write('COMPLETED CATEGORIES SPANNING: '),
	write_out(M,N),
	nl, ale_edge(I,M,N,Tag,SVs,Iqs,Dtrs,RuleName),
	nl, edge_act(I,M,N,Tag,SVs,Iqs,Dtrs,RuleName), 
	fail
    ;	true
    ),
    nl, write('| ?- '), ttyflush.

show_edge(tk,L,R) :-
    (	ale_edge(_I,L,R,Tag,SVs,Iqs,_Dtrs,_RuleName),
	tk_ale(fs(Tag-SVs),Iqs),
	fail
    ;	true
    ).

show_edge(clig,L,R) :-
    (	ale_edge(_I,L,R,Tag,SVs,Iqs,_Dtrs,_RuleName),
	clig_ale(fs(Tag-SVs),Iqs),
	fail
    ;	true
    ).

show_edge(latex,L,R) :-
    findall(changed(Term),
	    (	ale_edge(_,L,R,Tag,SVs,Iqs,_,_),
		ale_to_hdrug(fs(Tag-SVs),Iqs,Term)
	    ), Terms),
    show(fs,latex,Terms).

