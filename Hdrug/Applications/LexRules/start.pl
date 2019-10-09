:- use_module( term_expand ).
:- use_module( hdrug_feature:term_expand_feature ).

:- use_module( compilation ).
:- use_module( [ rcp3,rcp3_d,rcp3_dtrs,approx_parser,hc,
		 lc,hc_d,hc_dtrs,right_chart,shift_reduce,bug_dd
	       ]).

:- initialize_flag(top_features,main).

:- initialize_flag(parser,rcp3_dtrs).
:- initialize_flag(generator,bug_dd).

:- initialize_flag(parser(approx_parser),on).
:- initialize_flag(parser(hc),off).
:- initialize_flag(parser(hc_d),on).
:- initialize_flag(parser(hc_dtrs),on).
:- initialize_flag(parser(rcp3),off).
:- initialize_flag(parser(rcp3_d),on).
:- initialize_flag(parser(rcp3_dtrs),on).
:- initialize_flag(parser(right_chart),on).
:- initialize_flag(parser(shift_reduce),on).
:- initialize_flag(parser(lc),on).


:- initialize_flag(generator(bug),off).
:- initialize_flag(generator(bug_dd),on).

% decl.pl
% declarations that use type information

:- use_module( wlists, [ wappend/3 ] ).
:- use_module( lex_string).

:- compile_grammar.

:- version('Grammar based on the paper:
The Scope of Adjuncts and the Processing of Lexical Rules').

gram_startup_hook_begin :-
	update_unary_preds.

gram_startup_hook_end :-
	tcl('

button .t.version -text {Recursive Lexical Rules} -command {
    tk_dialog .d "About the Grammar" "This grammar is written by G. van Noord (with help from \
        Gosse Bouma). It is a \
        head-driven unification grammar for Dutch in which\
        powerful recursive lexical rules are used. The\
        approach is desribed in the paper `The Scope of Adjuncts and the\
        Processing of Lexical Rules\', co-authored with Gosse\
        Bouma, which appears in the Proceedings of Coling\
        1994, Kyoto. A more elaborated paper entitled `Dutch Verb\
        Clustering without Verb Clusters is in preparation." "" 0 ok
}

pack .t.version -side right


.menu.show.m add cascade -label "Lexical Hierarchy"\
	-menu .menu.show.m.lh

.menu.show.m add cascade -label "Part of Hierarchy"\
	-menu .menu.show.m.lhp

menu .menu.show.m.lh

.menu.show.m.lh add command -label "Tk" -command {
    prolog call_tree_bu_tk
}
.menu.show.m.lh add command -label "LaTex" -command {
    prolog call_tree_bu_latex
}
.menu.show.m.lh add command -label "Clig" -command {
    prolog call_tree_bu_clig
}
.menu.show.m.lh add command -label "Prolog" -command {
    prolog call_tree_bu
}

menu .menu.show.m.lhp

.menu.show.m.lhp add command -label "Tk" -command {
    prolog "ct(tk, \
               [hdrug_qbox_array .w unary_preds {Lexical Type} {Lexical Type}])"
}
.menu.show.m.lhp add command -label "LaTex" -command {
    prolog "ct(latex, \
               [hdrug_qbox_array .w unary_preds {Lexical Type} {Lexical Type}])"
}
.menu.show.m.lhp add command -label "Clig" -command {
    prolog "ct(clig, \
               [hdrug_qbox_array .w unary_preds {Lexical Type} {Lexical Type}])"
}
.menu.show.m.lhp add command -label "Prolog" -command {
    prolog "ct(prolog, \
               [hdrug_qbox_array .w unary_preds {Lexical Type} {Lexical Type}])"
}



.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "This grammar is written by \
        G. van Noord (with help from \
        Gosse Bouma). It is a \
        head-driven unification grammar for Dutch in which\
        powerful recursive lexical rules are used. The\
        approach is desribed in the paper `The Scope of Adjuncts and the\
        Processing of Lexical Rules\', co-authored with Gosse\
        Bouma, which appears in the Proceedings of Coling\
        1994, Kyoto. A more elaborated paper is entitled `Dutch Verb\
        Clustering without Verb Clusters." "" 0 ok
}

prolog send_lexs
prolog send_rules
prolog send_tops

if {$lexs(max) > 0} then {
        .menu.show.m add cascade -label "Lexical Entry" -menu .menu.show.m.lex
}

if {$rules(max) > 0} then {
        .menu.show.m add cascade -label "Grammar Rule" -menu .menu.show.m.rule
} 

if {$tops(max) > 0} then {
	.menu.show.m add cascade -label "Top Categories" -menu .menu.show.m.top
}

menu .menu.show.m.lex
.menu.show.m.lex add cascade -label "Tk"  -menu .menu.show.m.lex.tk
.menu.show.m.lex add cascade -label "Prolog" -menu .menu.show.m.lex.prolog
.menu.show.m.lex add cascade -label "LaTeX"  -menu .menu.show.m.lex.latex
.menu.show.m.lex add cascade -label "Clig"  -menu .menu.show.m.lex.clig

proc show_lex {how where} {
    global lexs
    set lex [hdrug_qbox_array .w lexs Lex "Lex to show:" ]
    if {$lex == 0} {return 0}
    prolog "show_lex($lex,$how,$where)"
}

menu .menu.show.m.lex.tk
.menu.show.m.lex.tk add command -label "Text" \
    -command {show_lex term(print) tk}
.menu.show.m.lex.tk add command -label "Matrix" \
    -command {show_lex fs tk}

menu .menu.show.m.lex.latex
.menu.show.m.lex.latex add command -label "Text" \
    -command {show_lex term(print) latex}
.menu.show.m.lex.latex add command -label "Matrix" \
    -command {show_lex fs latex}

menu .menu.show.m.lex.clig
.menu.show.m.lex.clig add command -label "Matrix" \
    -command {show_lex fs clig}

menu .menu.show.m.lex.prolog
.menu.show.m.lex.prolog add command -label "Text" \
    -command {show_lex term(print) user}
.menu.show.m.lex.prolog add command -label "Matrix" \
    -command {show_lex fs user}

menu .menu.show.m.rule
.menu.show.m.rule add cascade -label "Tk"  -menu .menu.show.m.rule.tk
.menu.show.m.rule add cascade -label "Prolog" -menu .menu.show.m.rule.prolog
.menu.show.m.rule add cascade -label "LaTeX"  -menu .menu.show.m.rule.latex
.menu.show.m.rule add cascade -label "Clig"  -menu .menu.show.m.rule.clig

proc show_rule {how where} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule(\'$rule\',$how,$where)"
}

menu .menu.show.m.rule.tk
.menu.show.m.rule.tk add command -label "Text" \
    -command {show_rule term(print) tk}
.menu.show.m.rule.tk add command -label "Matrix" \
    -command {show_rule fs tk}

menu .menu.show.m.rule.latex
.menu.show.m.rule.latex add command -label "Text" \
    -command {show_rule term(print) latex}
.menu.show.m.rule.latex add command -label "Matrix" \
    -command {show_rule fs latex}

menu .menu.show.m.rule.clig
.menu.show.m.rule.clig add command -label "Matrix" \
    -command {show_rule fs clig}

menu .menu.show.m.rule.prolog
.menu.show.m.rule.prolog add command -label "Text" \
    -command {show_rule term(print) user}
.menu.show.m.rule.prolog add command -label "Matrix" \
    -command {show_rule fs user}


menu .menu.show.m.top
.menu.show.m.top add cascade -label "Tk"     -menu .menu.show.m.top.tk
.menu.show.m.top add cascade -label "Prolog" -menu .menu.show.m.top.prolog
.menu.show.m.top add cascade -label "LaTeX"  -menu .menu.show.m.top.latex
.menu.show.m.top add cascade -label "Clig"  -menu .menu.show.m.top.clig

menu .menu.show.m.top.tk
.menu.show.m.top.tk add command -label "Text" \
    -command {show_top term(print) tk}
.menu.show.m.top.tk add command -label "Matrix" \
    -command {show_top fs tk}

menu .menu.show.m.top.latex
.menu.show.m.top.latex add command -label "Text" \
    -command {show_top term(print) latex}
.menu.show.m.top.latex add command -label "Matrix" \
    -command {show_top fs latex}

menu .menu.show.m.top.clig
.menu.show.m.top.clig add command -label "Matrix" \
    -command {show_top fs clig}

menu .menu.show.m.top.prolog
.menu.show.m.top.prolog add command -label "Text" \
    -command {show_top term(print) user}
.menu.show.m.top.prolog add command -label "Matrix" \
    -command {show_top fs user}

proc show_top {how where} {
    global tops
    set top [hdrug_qbox_array .w tops Top "Top to show:" ]
    if {$top == 0} {return 0}
    prolog "show_top(\'$top\',$how,$where)"
}

wm iconname . "LexRules"


	   '),
	menu_flag(add_mod,[on,off]),
	menu_flag(push_to_slash,[on,off]),
	menu_flag(push_to_extra,[on,off]),
	menu_flag(shorten,[short_sem,sc_cat,short,cat,off]).


:- use_module(suite).

