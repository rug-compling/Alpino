:- expects_dialect(sicstus).

:- use_module(hdrug(hdrug_util)).

:- public lexicon_hierarchy/0, lexicon_hierarchy_dot/0,
    grammar_hierarchy/0, grammar_hierarchy_dot/0.

lexicon_hierarchy :-
    set_flag(hierarchy,lexicon),
    hdrug_call_tree:call_tree_bu_flat.

lexicon_hierarchy_dot :-
    set_flag(hierarchy,lexicon),
    hdrug_call_tree:call_tree_bu_dot.

grammar_hierarchy :-
    set_flag(hierarchy,grammar),
    hdrug_call_tree:call_tree_bu_flat.

grammar_hierarchy_dot :-
    set_flag(hierarchy,grammar),
    hdrug_call_tree:call_tree_bu_dot.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% tree-like pretty printing of lexicon set-up %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% to view the lexicon as if it were a real inheritance
%% hierarchy. Interface to hdrug_call_tree library.

:- initialize_flag(hierarchy,grammar).

%% for display of lexical hierarchy
%call_leaf(a_lex(Tag,_),Tag).

call_leaf(Leaf,Id) :-
    hdrug_flag(hierarchy,Flag),
    call_leaf(Flag,Leaf,Id).

call_leaf(grammar,grammar_rule(Id,_,_),Label) :-
    alpino_lc_in:grammar_rule(Id,_,_),
    charsio:format_to_chars("rule: ~w",[Id],Chars),
    atom_codes(Label,Chars).

%% top of hierarchy

call_default(Fun) :-
    hdrug_flag(hierarchy,Flag),
    call_default(Flag,Fun).

call_default(lexicon,sign/2).
call_default(grammar,structure/0).

call_clause(A,Body) :-
    hdrug_flag(hierarchy,Flag),
    call_clause(Flag,A,Body).

call_clause(grammar,A,Body) :-
    hook(alpino_grammar:user_clause(A,Body)).
	
call_clause(lexicon,A,Body) :-
    hook(alpino_lex_types:user_clause(A,Body)).

%% this always ignores additional predicates used in a definition of a type
call_build_lab(F,_,F).

call_ignore_clause('=>'/2).
call_ignore_clause('=?>'/2).
call_ignore_clause('==>'/2).
call_ignore_clause('<=>'/2).
call_ignore_clause('=?>'/2).
call_ignore_clause('==?>'/2).
call_ignore_clause('<?=?>'/2).
call_ignore_clause(if_defined/2).
call_ignore_clause(if_defined/3).
call_ignore_clause(unify_except/3).
call_ignore_clause(overwrite/4).
call_ignore_clause(a_lex/2).
call_ignore_clause(a_lex/3).
call_ignore_clause(a_lex/4).

:- public missing_rules/0.

missing_rules :-
    set_flag(hierarchy,grammar),
    hdrug_call_tree:get_tree(structure/0,Tree),
    all_rules(Tree,Rules,[]),
    Rule = grammar_rule(Id,_,_),
    (   call_clause(Rule,_),
        \+ member(Id,Rules),
        format(user_error,"~w not in hierarchy~n",[Id]),
        fail
    ;   true
    ).

all_rules(tree(Name,_,Ds)) -->
    all_rules_ds(Ds,Name).

all_rules_ds([],Name) --> [Name].
all_rules_ds([H|T],_) -->
    all_rules(H),
    all_rules_list(T).

all_rules_list([]) --> [].
all_rules_list([H|T]) -->
    all_rules(H),
    all_rules_list(T).
