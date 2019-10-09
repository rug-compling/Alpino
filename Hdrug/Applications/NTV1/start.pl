% parsers and generators
:- initialize_flag(parser(parser),on).
:- initialize_flag(parser,parser).
:- initialize_flag(top_features,s).
:- initialize_flag(grammar,opgave1).
:- set_flag(complex_parse_widget,on).
:- set_flag(cmdint,on).

:- if(current_prolog_flag(dialect,swi)).

:- style_check(-atom).

:- endif.

% top_features
top(s,_Term).

semantics(X,X).

show_object_default2(No) :-
    show_object_no(No,tree(syn),clig).

show_object_default3(No) :-
    show_object_no(No,tree(syn),clig).

% grammar compilation
compile_grammar :-
    hdrug_flag(grammar,File),
    use_module(opgave1:File).

reconsult_grammar :-
    hdrug_flag(grammar,File),
    reconsult(opgave1:File).

compile_grammar_file(File) :-
    set_flag(grammar,File),
    compile_grammar.

reconsult_grammar_file(File) :-
    set_flag(grammar,File),
    reconsult_grammar.

:- version('NTV1 opgave 1').

% Trees
:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(syn,S,S).

graphic_label(syn,b(Label,_),Label).
graphic_label(syn,w(Label),Label).

graphic_daughter(syn,I,b(_,Ds),D) :-
    lists:nth(I,Ds,D).

% Extending GUI
gram_startup_hook_end :-
    tcl('
button .t.version -text {Simple DCG} -command {
 tk_dialog .d "About the Grammar" "College NTV1 opdracht 1" "" 0 ok
}
pack .t.version -side right
',[]),
    tcl('
.menu.help.m add command -label "About the grammar" -underline 0 -command {
    tk_dialog .d "About the Grammar" "College NTV1 opdracht 1" "" 0 ok
}',[]),
    tcl('
wm iconname . "NTV1"
add_parse_widget
       ',[]).

result_hook(parse,_,_,_) :-
    show_object_no(1,tree(syn),clig).

sentence(Key,Sent) :-
    try_hook(opgave1:sentence(Key,Sent)).

parser:parse(o(notree,Str,_)) :-
    opgave1:current_predicate(s/2),
    opgave1:s(Str,[]).
	   
parser:parse(o(Tree,Str,_)) :-
    opgave1:current_predicate(s/3),
    opgave1:s(Tree,Str,[]).
	   
hdrug_initialization :-
    compile_grammar.
