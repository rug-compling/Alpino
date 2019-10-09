:- module(hdrug_clig,
	  [ %clig_eval/1,
	    %clig_eval/2,
	    %clig_eval/3,
	    clig_fs/1,
	    clig_fs_list/1,
	    clig_tree/2,
	    clig_tree/1,
	    clig_tree_on_widget/3
	  ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- multifile
    user:help_info/4.

:- use_module(hdrug_util).
:- use_module(library(charsio)).
:- use_module(library(lists),  [ member/2, append/3 ]).

:- public
    help_info/4.
:- discontiguous
    help_info/4.

help_info(class,pred,"Predicates",
"This section lists the predicates exported by the hdrug_clig
library.").  


user:help_info(module,hdrug_clig,"Interface to CLiG",
"This module provides an interface to Karsten Konrad's CLiG system for
visualization of feature-structures and trees.").  

help_info(pred,clig_fs,
"clig_fs(Fs)",
"displays a feature structure in CLiG. Assumes that
hdrug(hdrug_feature) is loaded and that feature declarations have been
compiled. Example: 

        X:cat => np, clig_fs(value(X)).
").

help_info(pred,clig_fs_list,
"clig_fs_list(List)",
"displays each feature structure in List in CLiG. Assumes that
hdrug(hdrug_feature) is loaded and that feature declarations have been
compiled. Example: 

        X:cat => np, Y:cat => vp, clig_fs_list([X,Y]).
").

help_info(pred,clig_tree,
"clig_tree(Format,Term)",
"displays a feature structure in CLiG. Format is a tree-format; Term
is an arbitrary term. The hook-predicates graphic_path, graphic_label
and graphic_daughter are used to obtain the tree structure for
Term."). 

:- initialize_flag(clig_tree_hspace,5).   % clig default 16
:- initialize_flag(clig_tree_vspace,20).  % clig default 42
:- initialize_flag(clig_fontsize,10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prolog terms as feature structures %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% First: prolog terms / clauses / listing where the terms that
%% occur might represent feature-structures.
%%
%% cf. library(feature) & library p_feature
%%
%% note that the file library(p_feature) defines similar predicates
%% for txt output

%% Nov 23, 2000: reverse list, since tk is used as a stack.
clig_fs_list(List0) :-
    lists:reverse(List0,List),
    clig_fs_list_(List).

clig_fs_list_([]).
clig_fs_list_([H|T]) :-
    clig_fs(H),
    clig_fs_list_(T).

clig_fs(Thing0) :-
    hdrug_gui:select_widget(fs,C),
    hdrug_show:change_thing_fs(Thing0,Thing),
    format_to_chars("{leftStack ",[],Chars0,Chars1),
    write_it_fs_clig(Thing,Chars1,Chars2),
    format_to_chars("} ",[],Chars2,[]),
    hdrug_flag(clig_fontsize,Font),
    hdrug_gui:tcl('
canvas ~a
pack ~a
set clig_globals(fontsize) ~w
set size [execobj ~s 10 10 ~a {"cmg"}]
~a configure -width [expr [lindex $size 0]+10]
~a configure -height [expr [lindex $size 1]+10]
		 ',[C,C,Font,Chars0,C,C,C]),
    hdrug_gui:reposition_widget(fs).

write_it_fs_clig(value(FS,C)) -->
    format_to_chars("{seq ",[]),
    pp(FS),
    clig0_if,
    write_pretty_constraint_cligs(C),
    format_to_chars("} ",[]).

write_it_fs_clig(value(FS)) -->
    pp(FS).

write_it_fs_clig(clause(H)) -->
    format_to_chars("{seq ",[]),
    write_pretty_constraint_clig(H),
    format_to_chars("{plain-text .}} ",[]).

write_it_fs_clig(clause(H,B)) -->
    format_to_chars("{leftstack {seq ",[]),
    write_pretty_constraint_clig(H), 
    clig0_if,
    format_to_chars("} ",[]),
    write_pretty_constraint_cligs(B),
    format_to_chars("} ",[]).

write_pretty_constraint_cligs([H|T]) -->
    format_to_chars("{seq {hspace 50} ",[]),
    write_pretty_constraint_clig(H),
    write_pretty_constraint_cligs0(T).

write_pretty_constraint_cligs0([]) -->
    format_to_chars("{plain-text .}} ",[]).
write_pretty_constraint_cligs0([H|T]) -->
    format_to_chars("{plain-text ,}} {seq {hspace 50} ",[]),
    write_pretty_constraint_clig(H),
    write_pretty_constraint_cligs0(T).

write_pretty_constraint_clig(true) --> [].   % ??
write_pretty_constraint_clig(H) -->
    {  H =.. [F,ArgL,ArgR],
       current_op(_,xfy,F),
       !
    },
    format_to_chars("{seq ",[]),
    pp(ArgL),
    format_to_chars(" {plain-text  ~w  } ",[F]),
    pp(ArgR),
    format_to_chars("} ",[]).

write_pretty_constraint_clig(H) -->
    {  H =.. [F|Args],
       length(Args,Arity)
    },
    format_to_chars("{seq ",[]),
    write_relation(F,Arity),
    write_begin_functor_clig(Args),
    write_pretty_argument_cligs0(Args),
    write_end_functor_clig(Args),
    format_to_chars("} ",[]).

write_begin_functor_clig([]) --> [].
write_begin_functor_clig([_|_])-->
    format_to_chars("{plain-text (} ",[]).

write_end_functor_clig([]) --> [].
write_end_functor_clig([_|_]) -->
    format_to_chars("{plain-text )} ",[]).

write_pretty_argument_cligs0([]) --> [].
write_pretty_argument_cligs0([H|T])-->
    pp(H),
    write_pretty_argument_cligs(T).

write_pretty_argument_cligs([]) --> [].
write_pretty_argument_cligs([H|T])-->
    format_to_chars("{plain-text ,} ",[]),
    pp(H),
    write_pretty_argument_cligs(T).

pp(_Var/n=FS) -->
    !,
    ppl_clig(FS,no).

pp(Var/_='R') -->
    !,
    clig0_var(Var).

pp(Var/y=[]) -->
    !,
    clig0_var(Var).

pp(Var/y='$VAR'(_)) -->
    !,
    clig0_var(Var).

pp(Var/y=FS) -->
    !,
    ppl_clig(FS,yes(Var)).

pp(lex(W)) -->
    !,
    format_to_chars("{plain-text {~w}} ",[W]).

% ??
pp(H) -->
    pp(_/n=H).

ppl_clig([a(_Att,Thing)|Rest],Tab) -->
    { do_not_print(Thing) },
    !,
    ppl_clig(Rest,Tab).

ppl_clig([a(type,['.']),a(_,Head),a(_,Tail)],no) -->
    !,
    ppl_list(Head,Tail).

ppl_clig([a(type,['.']),a(_,Head),a(_,Tail)],yes(Var)) -->
    !,
    format_to_chars("{seq ",[]),
    clig0_var(Var),
    ppl_list(Head,Tail),
    format_to_chars("} ",[]).

ppl_clig([a(type,[[]])],_) -->
    !,
    format_to_chars("{angle {vspace 10}} ",[]).

ppl_clig([a(type,Types)|T],no) -->
    {  all_empty(T)  },
    !,
    {  hdrug_feature:write_as_conj(Types,PTypes)  },
    write_type_clig(PTypes).

ppl_clig([a(type,Types)|T],yes(Var)) -->
    {  all_empty(T) },
    !,
    format_to_chars("{Seq ",[]),
    clig0_var(Var),
    {  hdrug_feature:write_as_conj(Types,PTypes)  },
    write_type_clig(PTypes),
    format_to_chars(" } ",[]).

ppl_clig([a(type,Types)|T],no) -->
    !,
    format_to_chars("{fs ",[]),
    {  hdrug_feature:write_as_conj(Types,PTypes)  },
    write_type_clig(PTypes),
    ppl_clig(T,_Tab),
    format_to_chars("} ",[]).

ppl_clig([a(type,Types)|T],yes(Var)) -->
    !,
    format_to_chars("{seq ",[]),
    clig0_var(Var),
    format_to_chars("{fs ",[]),
    {  hdrug_feature:write_as_conj(Types,PTypes)  },
    write_type_clig(PTypes),
    ppl_clig(T,_Tab),
    format_to_chars("}}  ",[]).

ppl_clig([a('BOOLEAN',_Type,Val)|T],no) -->
    {  all_empty(T)  },
    !,
    {  hdrug_feature:give_boolean_type(Val,Exp)  },
    write_type_clig(Exp).

ppl_clig([a('BOOLEAN',_Type,Val)|T],yes(Var)) -->
    {  all_empty(T)  },
    !,
    format_to_chars("{seq ",[]),
    clig0_var(Var),
    {  hdrug_feature:give_boolean_type(Val,Exp)  },
    write_type_clig(Exp),
    format_to_chars("} ",[]).

ppl_clig([a('BOOLEAN',_Type,Val)|T],no) -->
    {  hdrug_feature:give_boolean_type(Val,Exp)  },
    write_type_clig(Exp),
    ppl_clig(T,_Tab).


ppl_clig([a('BOOLEAN',_Type,Val)|T],yes(Var)) -->
    {  hdrug_feature:give_boolean_type(Val,Exp)  },
    format_to_chars("{seq ",[]),
    clig0_var(Var),
    write_type_clig(Exp),
    format_to_chars("} ",[]),
    ppl_clig(T,_Tab).

ppl_clig([a('UNTYPED',_Att,Val)|T],no) -->
    !,
    clig0_term0(Val),
    ppl_clig(T,_Tab).

ppl_clig([a('UNTYPED',_Att,Val)|T],yes(Var))-->
    format_to_chars("{seq ",[]),
    clig0_var(Var),
    clig0_term0(Val),
    format_to_chars("} ",[]),
    ppl_clig(T,_Tab).

ppl_clig([a(Att,FS)|T],no) -->
    !,
    format_to_chars("{feature ",[]),
    write_attribute(Att),
    pp(FS),
    format_to_chars("} ",[]),
    ppl_clig(T,_Tab).

ppl_clig([a(Att,FS)|T],yes(Var)) -->
    !,
    format_to_chars("{seq ",[]),
    clig0_var(Var),
    format_to_chars("{fs ",[]),
    write_attribute(Att),
    pp(FS),
    format_to_chars("}} ",[]),
    ppl_clig(T,_Tab).

ppl_clig([],_) --> !.

ppl_clig('$VAR'(_),_) -->
    !,
    format_to_chars("{plain-text _} ",[]).

%% so we can have arbitrary terms here. I don't think the change
%% predicates support this, though.
ppl_clig(Thing,_) -->
    write_pretty_constraint_clig(Thing).

clig0_term0(Term) -->
    {  prolog_flag(toplevel_print_options,List) },
    format_to_chars("{plain-text {~@}} ",[write_term(Term,List)]).

ppl_list(Head,Tail) -->
    format_to_chars("{angle {Seq ",[]),
    ppx(Head),
    ppl_list(Tail),
    format_to_chars("}} ",[]).

ppl_list(V/y='R') -->
    !,
    format_to_chars("{plain-text |} ",[]),
    ppx(V/y='R').

ppl_list(V/YN='$VAR'(_)) -->
    !,
    format_to_chars("{plain-text |} ",[]),
    ppx(V/YN='$VAR'(_)).

ppl_list(_Var/_YN=[a(type,[[]])]) -->
    !.

ppl_list(_Var/n=[a(type,['.']),a(_,Head),a(_,Tail)]) -->
    !,
    format_to_chars("{plain-text ,} ",[]),
    ppx(Head),
    ppl_list(Tail).

ppl_list(Var/y=[a(type,['.']),a(_,Head),a(_,Tail)]) -->
    !,
    format_to_chars("{plain-text |} ",[]),
    clig0_var(Var),
    ppl_list(Head,Tail).

%% sometimes the tail of a list is not a list ...
%% added Thu Aug 22 09:45:58 METDST 2002
ppl_list(Whatever) -->
    !,
    format_to_chars("{plain-text |} ",[]),
    pp(Whatever).

ppx(ListEl) -->
    pp(ListEl),!.

ppx(_) -->
    format_to_chars("{plain-text _} ",[]).

write_attribute(A) -->
    format_to_chars("{~w} ",[A]).

%write_relation(A) -->
%    format_to_chars("{plain-text {~w}} ",[A]).

write_relation(A,Len) -->
    { a_user_clause(A/Len,_,_), ! },
    format_to_chars("{active {plain-text {~w}} {<1> {prolog {hdrug:show_relation(~q/~q,clig)}}}} ",[A,A,Len]).

write_relation(A,_) -->
    format_to_chars("{plain-text {~w}} ",[A]).

write_type_clig(A) -->
    format_to_chars("{bold-text {~w}} ",[A]).

all_empty([]).
all_empty([a(_,H)|T]):-
    do_not_print(H),
    all_empty(T).
                         
do_not_print(_Var/n='$VAR'(_)). 

clig0_var(N) -->
    format_to_chars("{boxed 1 {plain-text {~w}}} ",['$VAR'(N)]).

clig0_if -->
    format_to_chars("{plain-text {:-}} ",[]).

%%%%%%%%%%%%%%%%%%%%%% TREES %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                       %
% the following predicates must be defined elsewhere:   %
% graphic_path(Name,FS,FS2)                             %
% graphic_daughter(Name,Pos,FS,Daught)                  %
% graphic_label(Name,FS,Label)                          %
%                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clig_tree_on_widget(Name,Tree0,Widget):-
    hdrug_show:change_thing_tree_with_orig(Tree0,Name,Tree),
    pp_tree(Tree,Name,Chars,[]),
    clig_tree0(Chars,Widget),
    hdrug_tk:help_info(Name,Widget).

clig_tree(Name,FS):-
    hdrug_gui:select_widget(tree(Name),Widget),
    hdrug_show:change_thing_tree_with_orig(FS,Name,Tree),
    pp_tree(Tree,Name,Chars,[]),
    clig_tree0(Chars,Widget),
    hdrug_tk:help_info(Name,Widget),
    hdrug_gui:reposition_widget(tree(Name)).

clig_tree(Tree) :-
    hdrug_gui:select_widget(tree(Name),Widget),
    pp_tree(Tree,'',Chars,[]),
    clig_tree0(Chars,Widget),
    hdrug_gui:reposition_widget(tree(Name)).

clig_tree0(Chars,C) :-
    hdrug_flag(clig_tree_hspace,Hs),
    hdrug_flag(clig_tree_vspace,Vs),
    hdrug_flag(clig_fontsize,Font),
    hdrug_gui:tcl('
canvas ~a 
pack ~a
set clig_globals(tree_hspace) ~w
set clig_globals(tree_vspace) ~w
set clig_globals(fontsize) ~w
set size [execobj ~s 10 10 ~a {"cmg"}]
~a configure -width [expr [lindex $size 0]+10]
~a configure -height [expr [lindex $size 1]+10]
		 ',[C,C,Hs,Vs,Font,Chars,C,C,C]).
    
pp_tree(tree(Node,_,Ds),TreeFormat) -->
    pp_tree(Ds,Node,TreeFormat).

pp_tree([],Node,TreeFormat) -->
    pp_node(Node,TreeFormat).

pp_tree([Hds|Tds],Node,TreeFormat) -->
    format_to_chars("{tree ",[]),
    pp_node(Node,TreeFormat),
    pp_ds([Hds|Tds],TreeFormat),
    format_to_chars("} ",[]).

pp_ds([],_) --> [].
pp_ds([H|T],Format) -->
    pp_tree(H,Format),
    pp_ds(T,Format).

pp_node(Node,Format) -->
    {  Format = matrix(_) },!,
    pp(Node).

pp_node(Node,Format,C0,C) :-
    Format = user(Type),
    !,
    hdrug:clig_tree_user_node(Node,C0,C,Type).

:- initialize_flag(clig_tree_active_nodes,on).

pp_node(Node,Format) -->
    { hdrug_flag(clig_tree_active_nodes,Val)},
    pp_node(Val,Node,Format).

pp_node(off,Node/_Orig,_F) -->
    format_to_chars("{plain-text {~w}}  ",[Node]).

pp_node(on,Node/Orig,F) -->
    { gen_sym(OrigKey,hdrug_clig),
      bb_put(hdrug_clig:OrigKey,Orig)
    },
    format_to_chars("{active {plain-text {~w}} {<1> {prolog {hdrug_clig:xshow_node(~w,~q)}}} {<2> {prolog {hdrug_clig:xshow_node2(~w,~q)}}} {<3> {prolog {hdrug_clig:xshow_node3(~w,~q)}}}} ",
		    [Node,F,OrigKey,F,OrigKey,F,OrigKey]).

:- public xshow_node/2, xshow_node2/2, xshow_node3/2.

xshow_node(A,Key) :-
    bb_get(hdrug_clig:Key,Node),
    hdrug:show_node(A,Node,clig).
xshow_node2(A,Key) :-
    bb_get(hdrug_clig:Key,Node),
    hdrug:show_node2(A,Node,clig).
xshow_node3(A,Key) :-
    bb_get(hdrug_clig:Key,Node),
    hdrug:show_node3(A,Node,clig).
