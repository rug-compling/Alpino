%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (c) 1992 - 1994 Gertjan van Noord RUG %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% declare operators %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(300,fx,*).        % substitution node
:- op(300,fx,=).        % foot node
%            +          % head node (should precede foot node marker
                        % and subs node marker)
%            :          % seperates mother node from list of daughters
:- op(900,fx,#).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% declarations needed by Hdrug: %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

semantics(tree(S,_,_),Sem):-
	user_sem(S,Sem).

top(Name,tree(Cat,_,_)):-
	user_top_category(Name,Cat).

:- ensure_loaded([ pretty,
	           compile,
		   parse
	         ]).

compile_parsers :-
    ensure_loaded([ mm,mm_d,mm_tree,pack,pack_d,
		    pack_tree, bt,bt_d, bt_tree ] ).


:- ensure_loaded(gram).
% grammar first, because that allows partial evaluation for
% some of the predicates in the parser..

:- compile_parsers.

:- initialize_flag(parser(bt),off).
:- initialize_flag(parser(bt_tree),on).
:- initialize_flag(parser(mm),off).
:- initialize_flag(parser(mm_d),on).
:- initialize_flag(parser(mm_tree),on).
:- initialize_flag(parser(pack),off).
:- initialize_flag(parser(bt_d),on).
:- initialize_flag(parser(pack_d),on).
:- initialize_flag(parser(pack_tree),on).


:- initialize_flag(parser,bt_tree).

:- version('Tree Adjoining Grammar v1').

gram_startup_hook_end :-
    tcl('
button .t.version -text {Tree Adjoining Grammars} -command {
    tk_dialog .d "About the Grammar" "
	This simple headed TAG is written by G. van Noord\
	in order to illustrate the family of head-corner \
	parsers for Tags (included) as described in a \
	paper entitled Head-corner parsing for Tags which\
	appeared in Computational Intelligence." "" 0 ok
}
pack .t.version -side right

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "
	This simple headed TAG is written by G. van Noord\
	in order to illustrate the family of head-corner \
	parsers for Tags (included) as described in a \
	paper entitled `Head-corner parsing for Tags\' which\
	appeared in Computational Intelligence." "" 0 ok
}

wm iconname . "Tag"
       ').

:- ensure_loaded(suite).


%result_hook(parse,Module,o(Obj,_,_),_) :-
%	(  hdrug_flag(demo,on)
%	-> parser_tree(Module,TreeType),
%           tk_tree(TreeType,Obj)
%	;  true
%	).

end_hook(parse,Module,_,_) :-
    parser_tree(Module,TreeType),
    show_object_no(1,tree(TreeType),clig).

show_object_default2(No) :-
    hdrug_flag(parser,Parser),
    parser_tree(Parser,TreeType),
    show_object_no(No,tree(TreeType),clig).


parser_tree(mm_tree,syn).
parser_tree(pack_tree,syn).
parser_tree(bt_tree,syn).
parser_tree(mm_d,dt).
parser_tree(pack_d,dt).
parser_tree(bt_d,dt).

