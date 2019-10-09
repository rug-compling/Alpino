:- module(hdrug_call_tree,
	  [ call_tree_bu/0,
	    call_tree_bu/1,
	    call_tree_bu_tk/0,
	    call_tree_bu_tk/1,
	    call_tree_bu_dot/0,
	    call_tree_bu_dot/1,
	    call_tree_bu_clig/0,
	    call_tree_bu_clig/1,
	    call_tree_bu_flat/0,
	    call_tree_bu_flat/1,
	    call_tree_bu_latex/0,
	    call_tree_bu_latex/1
	  ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- multifile user:help_info/4.

:- use_module(library(lists)).
:- use_module(hdrug_util).

:- public
    help_info/4.
:- discontiguous
    help_info/4.

help_info(class,hook,"Hook Predicates",
"This section lists the hook predicates for the hdrug_call_tree
library."). 

help_info(class,pred,"Predicates",
"This section lists the predicates exported by the hdrug_call_tree
library."). 

user:help_info(module,hdrug_call_tree,"Displaying Lexical Hierarchies",
"This library is intended to be used to display lexical hierarchies in
tree format. The relevant predicates all take a unary predicate
Pred. The predicates then pretty print in a tree format the hierarchy
related to the predicate Fuctor/1 as follows. Pred dominates all
predicates that call Pred in their body. 

If the optional Functor argument is absent, then the
call_default/1 hook predicate is used to obtain Functor.  

        transitive(X) :- verb(X).  
        
        verb(X) :- lex(X).
        
        noun(X) :- lex(X).
        
gives the tree: lex(verb(transitive),noun)

Leaves of the tree can be defined by the user (e.g. to stop the tree
at interesting point, and to give interesting info in the label, use
the hook predicate call_leaf(Call,Label). And yes, don't forget
the obvious: it is assumed that the predicates are not recursive,
although the current implementation does not rely on it."). 

help_info(pred,call_tree_bu,"
hdrug_call_tree:call_tree_bu[_tk/_clig/_latex][(Functor)]",
"pretty prints in a tree format the hierarchy related to the predicate
Fuctor/1. If the optional Functor argument is absent, then the
call_default/1 hook predicate is used to obtain Functor. The _tk
_clig and _latex suffices indicate that a different output medium
should be chosen (instead of the console). "). 

help_info(hook,call_default,"call_default(Functor)","Indicates
that Functor is the default predicate for the various call_tree
predicates."). 

help_info(hook,call_clause,"call_clause(Head,Body)","").
help_info(hook,call_leaf,"call_leaf(Leaf)","").
help_info(hook,call_build_lab,"call_build_lab(F,Fs,L)","").
help_info(hook,call_ignore_clause,"call_ignore_clause(F/A)","").



call_tree_bu :-
    hdrug:call_default(F),
    call_tree_bu(F).

call_tree_bu_tk :-
    hdrug:call_default(F),
    call_tree_bu_tk(F).

call_tree_bu_dot :-
    hdrug:call_default(F),
    call_tree_bu_dot(F).

call_tree_bu_latex :-
    hdrug:call_default(F),
    call_tree_bu_latex(F).

call_tree_bu_clig :-
    hdrug:call_default(F),
    call_tree_bu_clig(F).

call_tree_bu_flat :-
    hdrug:call_default(F),
    call_tree_bu_flat(F).

call_tree_bu(G) :-
    get_tree(G,Tree),
    hdrug_txt:pretty_graphic(t,Tree).

get_tree(G,Tree) :-
    (   atom(G)
    ->  A=1, F=G
    ;   G = F/A
    ),
    get_edges([F/A],[],Edges),
    edges_to_tree(F/A,Edges,[],_,Tree).

call_tree_bu_tk(G) :-
    get_tree(G,Tree),
    hdrug_gui:tk_tree(t,Tree).

call_tree_bu_dot(G) :-
    get_tree(G,Tree),
    hdrug_dot:dot_tree(t,Tree).

call_tree_bu_latex(G) :-
    get_tree(G,Tree),
    hdrug_latex:latex_tree(t,Tree).

call_tree_bu_clig(G) :-
    get_tree(G,Tree),
    hdrug_clig:clig_tree(t,Tree).

call_tree_bu_flat(G) :-
    get_tree(G,Tree),
    flat(Tree,'').

get_edges([],_,[]).
get_edges([F/A|Tail],Done0,Edges) :-
    findall(Node,get_edge(F/A,Node),Ds0),
    sort(Ds0,Ds1),
    add_nodes(Ds1,Done0,Done,Ds),
    add_edges(Ds1,F/A,Edges,Edges0),
    append(Ds,Tail,Agenda),
    get_edges(Agenda,Done,Edges0).

get_edge(F/A,D) :-
    functor(Goal,F,A),
    hdrug:call_clause(Head,Body),
    member(Goal,Body),
    (   hdrug:call_leaf(Head,Id)
    ->  D = id(Id)
    ;   functor(Head,NF,NA),
	\+ hdrug:call_ignore_clause(NF/NA),
	D = NF/NA
    ).

add_nodes([],Done,Done,[]).
add_nodes([Node|Nodes],Done0,Done,T) :-
    add_node(Node,Done0,Done1,T,T1),
    add_nodes(Nodes,Done1,Done,T1).

add_node(id(_),D,D,T,T).
add_node(F/A,Done0,Done,T0,T) :-
    (   member(F/A,Done0)
    ->  T0 = T,
	Done0 = Done
    ;   T0 = [F/A|T],
	Done = [F/A|Done0]
    ).

add_edges([],_,Edges,Edges).
add_edges([D|Ds],M,[edge(M,D)|Edges0],Edges):-
    add_edges(Ds,M,Edges0,Edges).

edges_to_tree(id(Id),_,Done,Done,tree(Id,x,[])).
edges_to_tree(F/A,_,Done,Done,tree(Label,x,[])):-
    member(F/A,Done),
    !,
    hdrug:call_build_lab(F/A,[],Label0),
    charsio:format_to_chars("~w ****(see above)****",[Label0],Chars),
    atom_codes(Label,Chars).
edges_to_tree(F/A,Edges,Done0,Done,tree(Label,x,TreeDs)):-
    hdrug:call_build_lab(F/A,[],Label),
    findall(D,member(edge(F/A,D),Edges),Ds),
    ds_to_trees(Ds,Edges,[F/A|Done0],Done,TreeDs).

ds_to_trees([],_,Done,Done,[]).
ds_to_trees([D|Ds],Edges,Done0,Done,[Tree|Trees]):-
    edges_to_tree(D,Edges,Done0,Done1,Tree),
    ds_to_trees(Ds,Edges,Done1,Done,Trees).

flat(tree(Label,_,Ds),Tab) :-
    format("~a~w",[Tab,Label]),
    flat_ds(Ds,Tab).

flat_ds([],_) :-
    nl.
flat_ds([H|T],Tab0) :-
    atom_concat(Tab0,'  ',Tab),
    nl,
    flat_ds1([H|T],Tab).

flat_ds1([],_).
flat_ds1([H|T],Tab) :-
    flat(H,Tab),
    flat_ds1(T,Tab).
    
