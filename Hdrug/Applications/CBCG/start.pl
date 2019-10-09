:- op(500,xfy,\).
:- op(600, xfx, <=> ).

:- use_module(types).

:- prolog_flag(redefine_warnings,Old,off),
   use_module(ubg_utils),     % defines unification macros's
   use_module(declarations),  % needed by TkHdrug
   prolog_flag(redefine_warnings,_,Old).

:- use_module(lexicon).       %
:- use_module(parser).        %
:- use_module(shift_reduce).  %
:- use_module(interface).     % 



compile_grammar :-
	compile_grammar_file(cug).

reconsult_grammar :-
	reconsult_grammar_file(cug).

compile_grammar_file(File) :-
	use_module(File),
	compile_lexicon,
	compile_rules.

reconsult_grammar_file(File) :-
	reconsult(File),
	compile_lexicon,
	compile_rules.

:- compile_grammar.


:- initialize_flag(top_features,s).

:- initialize_flag(parser,parser).

:- version('Grammar by Gosse Bouma, cf. Bouma and van Noord, 
"Constraint-based Categorial Grammar" ACL 1994').


gram_startup_hook_end :-
    tcl('
button .t.version -text {Constraint-based Categorial Grammar} -command {
    tk_dialog .d "About the Grammar" "
        This grammar is a contraint-based categorial grammar\
        written by Gosse Bouma. The techniques that are used\
        are discussed in a paper co-authored by Gertjan van Noord\
        entitled `Constraint-based Categorial Grammar\'. This\
        paper appears in the 1994 ACL Proceedings\
        " @$hdrug_library/bitmaps/hdrug_gosse.bm 0 ok
}
pack .t.version -side right

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "
	This grammar is a contraint-based categorial grammar\
        written by Gosse Bouma. The techniques that are used\
	are discussed in a paper co-authored by Gertjan van Noord\
        entitled `Constraint-based Categorial Grammar\'. This\
	paper appears in the 1994 ACL Proceedings\
	" @$hdrug_library/bitmaps/hdrug_gosse.bm 0 ok
}
# lexical entries
prolog stem_exists

if $stem_exists then {
        .menu.show.m add cascade -label "View Lexical Entry" -menu .menu.show.m.stem
}

menu .menu.show.m.stem
.menu.show.m.stem add cascade -label "Tk"  -menu .menu.show.m.stem.tk
.menu.show.m.stem add cascade -label "Prolog" -menu .menu.show.m.stem.prolog
.menu.show.m.stem add cascade -label "LaTeX"  -menu .menu.show.m.stem.latex
.menu.show.m.stem add cascade -label "Clig"  -menu .menu.show.m.stem.clig



menu .menu.show.m.stem.tk
.menu.show.m.stem.tk add command -label "Text" \
      -command show_stem_tk_text

if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.stem.tk add command -label "Matrix" \
          -command show_stem_tk_matrix
}

menu .menu.show.m.stem.latex
.menu.show.m.stem.latex add command -label "Text" \
      -command show_stem_latex_text

if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.stem.latex add command -label "Matrix" \
          -command show_stem_latex_matrix
}

menu .menu.show.m.stem.clig
if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.stem.clig add command -label "Matrix" \
          -command show_stem_clig_matrix
}

menu .menu.show.m.stem.prolog
.menu.show.m.stem.prolog add command -label "Text" \
      -command show_stem_prolog_text
if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.stem.prolog add command -label "Matrix" \
          -command show_stem_prolog_matrix
}

proc show_stem_prolog_text {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_prolog_text($stem)"
}

### matrix
proc show_stem_prolog_matrix {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show:" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_prolog_matrix($stem)"
}

## latex
### text
proc show_stem_latex_text {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show:" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_latex_text($stem)"
}

### matrix
proc show_stem_latex_matrix {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show:" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_latex_matrix($stem)"
}

proc show_stem_clig_matrix {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show:" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_clig_matrix($stem)"
}

## tk
### text
proc show_stem_tk_text {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show:" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_tk_text($stem)"
}

### matrix
proc show_stem_tk_matrix {} {
    global stems
    set stem [hdrug_qbox_array .w stems Stem "Stem to show:" ]
    if {$stem == 0} {return 0}
    prolog "show_stem_tk_matrix($stem)"
}


prolog rule_exists

if $rule_exists then {
        .menu.show.m add cascade -label "View Rule" -menu .menu.show.m.rule
}

menu .menu.show.m.rule
.menu.show.m.rule add cascade -label "Tk"  -menu .menu.show.m.rule.tk
.menu.show.m.rule add cascade -label "Prolog" -menu .menu.show.m.rule.prolog
.menu.show.m.rule add cascade -label "LaTeX"  -menu .menu.show.m.rule.latex
.menu.show.m.rule add cascade -label "Clig"  -menu .menu.show.m.rule.clig



menu .menu.show.m.rule.tk
.menu.show.m.rule.tk add command -label "Text" \
      -command show_rule_tk_text

if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.rule.tk add command -label "Matrix" \
          -command show_rule_tk_matrix
}

menu .menu.show.m.rule.latex
.menu.show.m.rule.latex add command -label "Text" \
      -command show_rule_latex_text

if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.rule.latex add command -label "Matrix" \
          -command show_rule_latex_matrix
}

menu .menu.show.m.rule.clig
if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.rule.clig add command -label "Matrix" \
          -command show_rule_clig_matrix
}

menu .menu.show.m.rule.prolog
.menu.show.m.rule.prolog add command -label "Text" \
      -command show_rule_prolog_text
if [prolog hook(hdrug_feature:define_type(_,_,_,_,_))] then {
    .menu.show.m.rule.prolog add command -label "Matrix" \
          -command show_rule_prolog_matrix
}

proc show_rule_prolog_text {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_prolog_text($rule)"
}

### matrix
proc show_rule_prolog_matrix {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_prolog_matrix($rule)"
}

## latex
### text
proc show_rule_latex_text {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_latex_text($rule)"
}

### matrix
proc show_rule_latex_matrix {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_latex_matrix($rule)"
}
### matrix
proc show_rule_clig_matrix {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_clig_matrix($rule)"
}

## tk
### text
proc show_rule_tk_text {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_tk_text($rule)"
}

### matrix
proc show_rule_tk_matrix {} {
    global rules
    set rule [hdrug_qbox_array .w rules Rule "Rule to show:" ]
    if {$rule == 0} {return 0}
    prolog "show_rule_tk_matrix($rule)"
}


wm iconname . "CBCG"


'),
    menu_flag(shorten,[nosem,off]).

:- use_module(suite).

