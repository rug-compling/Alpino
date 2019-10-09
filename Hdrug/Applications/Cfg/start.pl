% parsers and generators
:- initialize_flag(parser(parser),on).
:- initialize_flag(generator(generator),on).

:- initialize_flag(parser,parser).
:- initialize_flag(generator,generator).

% top_features
top(s,node(s,_)).

:- initialize_flag(top_features,s).

% grammar compilation
compile_grammar :-
	compile(cfg).

reconsult_grammar :-
	reconsult(cfg).

compile_grammar_file(File) :-
	compile(File).

reconsult_grammar_file(File) :-
	reconsult(File).

:- version('Simple DCG').

semantics(node(_,Sem),Sem).

% Trees
:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(syn,node(_,S),S).

graphic_label(syn,Term,Label) :-
    functor(Term,Label,_).

graphic_daughter(syn,1,Term,D) :-
    arg(1,Term,D).

graphic_daughter(syn,2,Term,D) :-
    arg(2,Term,D).

% Extending GUI
gram_startup_hook_end :-
    tcl('
button .t.version -text {Simple CFG} -command {
 tk_dialog .d "About the Grammar" "
        This trivial Context Free Grammar is meant\
        to illustrate the minimun of interface that is\
        needed to use Hdrug." "" 0 ok
}
pack .t.version -side right

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "
	This trivial Context Free Grammar is meant\
        to illustrate the minimun of interface that is\
        needed to use Hdrug." "" 0 ok
}

wm iconname . "Cfg"

       ').

/*
result_hook(parse,_,o(Obj,_,_),_) :-
        show(tree(syn),tk,[value(Obj)]).                           
*/

end_hook(parse,_,_,_) :-
    show_object_no(1,tree(syn),clig).

show_object_default2(No) :-
    show_object_no(No,tree(syn),clig).

:- op(1200,xfx,'--->').

rule(Node,Nodes) :-
	(  Node0 ---> Nodes0  ),
	\+ functor(Nodes0,'.',2),   % lexical entry.
	conv_nodes(Nodes0,Nodes,Vars),
	conv_mother(Node0,Node,Vars).

lexicon(Word,node(Cat,Word)) :-
	(  Cat ---> [Word] ).

conv_mother(Atom,node(Atom,Tree),Vars) :-
	atom(Atom),
	Tree =.. [Atom|Vars].

conv_nodes((Node0,Nodes0),[node(Node0,Var)|Nodes],[Var|Vars]) :-
        conv_nodes(Nodes0,Nodes,Vars).
conv_nodes(Node0,[node(Node0,Var)],[Var]) :-
	atom(Node0).

:- use_module(cfg).
:- use_module(suite).
:- use_module(parser).
:- use_module(generator).

