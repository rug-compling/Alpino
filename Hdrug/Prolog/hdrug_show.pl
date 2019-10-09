:- module(hdrug_show,
	  [ show/3,
	    show_predicate/3,
	    show_object_no/3
	  %% change_thing_fs/2   %  util for vis. libraries
	  %% change_thing_term/2 %  util for vis. libraries
	  %% change_thing_tree/3 %  util for vis. libraries
	  %%                  _with_orig/3 %%
	  %% shorten_label/2     %  util for vis. libraries
	  ]). 


:- expects_dialect(sicstus).

:- multifile user:help_pred/3.

:- use_module(library(lists)).
:- use_module(hdrug_util    ).

:- public change_thing_fs/2, change_thing_tree/3,
          change_thing_tree_with_orig/3.

%% eval_paths(Obj,Paths,NewObjWithPaths)
%% makes version of Obj in which only Paths are instantiated
%% if there is more than one path, then we keep all the information
%% leading to the specified parts. If there is only one path then
%% we `extract' the information...
%%
%%
%% 2nd argument will be a list of:
%% value/1
%% clause/2
%% 
eval_paths_list([],[],_Paths).
eval_paths_list([H0|T0],[H|T],Paths) :-
    eval_paths_o(H0,H,Paths),
    eval_paths_list(T0,T,Paths).

eval_paths_o(object(_Name,o(FS0,_S,_W)),value(FS),Paths) :-
    eval_paths(Paths,FS0,FS).
eval_paths_o(value(FS0),value(FS),Paths) :-
    eval_paths(Paths,FS0,FS).
eval_paths_o(clause(A,B),clause(A,B),_).
eval_paths_o(changed(X),changed(X),_).

eval_paths([],V,V).
eval_paths([H],V0,V) :-
    eval_single(H,V0,V).
eval_paths([H,I|T],V0,V) :-
    eval_paths0([H,I|T],V0,V).

eval_paths0([],_,_).
eval_paths0([H|T],Obj,Val):-
    eval_p(Obj,H,Val),
    eval_paths0(T,Obj,Val).

eval_p(Obj,path(F),Val):-
    eval_f(F,Obj,Val).

eval_single(path(F),V0,V) :-
    eval_single(F,V0,V).

eval_single([],Obj,Obj).
eval_single([0|T],tree(Obj,_,_),Val) :-
    eval_single(T,Obj,Val).
eval_single([I|T],tree(_,_,Ds0),D) :-
    integer(I),
    nth(I,Ds0,D0),
    eval_single(T,D0,D).
eval_single([F|T],Obj,Val):-
    hook(hdrug_feature:'<=>'(Obj:F,Obj1)),
    eval_single(T,Obj1,Val).

eval_f([],Obj,Obj).
eval_f([0|T],tree(Obj,_,_),tree(Val,_,_)) :-
    !,
    eval_f(T,Obj,Val).
eval_f([I|T],tree(_,_,Ds0),tree(_,_,Ds)) :-
    integer(I),
    !,
    length(Ds0,L),
    length(Ds,L),
    nth(I,D0,Ds0),
    nth(I,D,Ds),
    eval_f(T,D0,D).
eval_f([F|T],Obj,Val):-
    hook(hdrug_feature:'<=>'(Obj:F,Obj1)),
    hook(hdrug_feature:'<=>'(Val:F,Val1)),
    !,
    eval_f(T,Obj1,Val1).

eval_f([F|T],tree(Obj,_,_),tree(Val,_,_)) :-
    eval_f([F|T],Obj,Val).

user:help_pred(show_object_no,"show_object_no(+No,+Style,+Output)",
"Displays the object numbered No using the Style and Output. These latter two arguments are of the type accepted by the first and second argument of the generic show/3 predicate.").

user:help_pred(show,"show(+Style,+Medium,+Things)",
"Generic interface to the Hdrug visualization tools. Style is one of:

words (only defined for object/2 things; displays the phonological representation of an object, i.e. Phon in object(Ident,o(Cat,Phon,Sem))

sem (only defined for object/2 things; displays the semantic representation of an object, i.e. Sem in object(Ident,o(Cat,Phon,Sem))

fs(+Path) (extracts the feature structure at path Path; and displays the result as a feature structure in matrix notation. In such a Path the prefix might consist of integers to refer to daughters in a tree/3 tree structure; 0 is the root node of a local tree.)

fs (feature structure in matrix notation)

term(print) output as a Prolog term, using print where appropriate (in order that any application-specific portray/1 hook predicates will be applicable)

term(write) same as term(print), but not using print.

tree(Format) displays as a tree using Format as the relevant tree-format. Such a tree-format is defined by clauses for the hook predicates graphic_path, graphic_daughter and graphic_label.

Medium is one of:

user (normal text to SICStus Prolog standard output).

tk (on a canvas of the graphical user interface).

latex (latex code is input to latex and either xdvi or dvips followed by ghostview). 

clig (using the CLiG system).

dot 

and Things is a list where each element is one of:

object(Ident,o(Cat,Words,Sem))

value(Term)

clause(Head,Body), Body a _list_ of goals.
").

eval_sems([],[]).
eval_sems([H0|T0],[H|T]):-
    eval_sem(H0,H),
    eval_sems(T0,T).

eval_sem(object(_,o(_,_,Sem)),value(Ext)) :-
    hdrug:extern_sem(Ext,Sem).

eval_words([],[]).
eval_words([H0|T0],[H|T]):-
    eval_word(H0,H),
    eval_words(T0,T).

eval_word(object(_,o(_,Phon,_)),value(Ext)) :-
    hdrug:extern_phon(Ext,Phon).

%% terms and fs:
%% passes on to each of the _fs and _term predicates:
%% list of value/1 and clause/2 terms.
%% trees:
%% list of terms to *_tree_list predicates
%% single term to *_tree predicate

%% only for objects
show(sem,Medium,Things0):-
    eval_sems(Things0,Things),
    show(term(print),Medium,Things).

%% only for objects
show(words,Medium,Things0):-
    eval_words(Things0,Things),
    show(term(print),Medium,Things).

show(fs(Path),Medium,Things0) :-
    eval_paths_list(Things0,Things,Path),
    sh_fs(Medium,Things).

show(fs,Medium,Things0) :-
    eval_paths_list(Things0,Things,[]), % unpack object/2
    sh_fs(Medium,Things).

%% term user
show(term(I), user, Things0) :-
    eval_paths_list(Things0,Things,[]), % unpack object/2
    sh_term_list(Things,I).

%% term latex
show(term(_),latex,Things0) :-
    eval_paths_list(Things0,Things,[]),
    hdrug_latex:latex_term_list(Things).

%% term tk
show(term(_),tk,Things0) :-
    eval_paths_list(Things0,Things,[]),
    hdrug_gui:tk_term_list(Things).

%% term clig
show(term(_),clig,_) :-
    format(user_error,"CLiG output of Prolog terms not supported~n",[]).

%% Trees. Only for values; ignores constraints.
show(tree(Mode),Out,Things0) :-
    eval_paths_list(Things0,Things1,[]),
    extract_values(Things1,Things),
    show_tree(Out,Mode,Things).

%% matrix notation of feature structures

%% fs latex
sh_fs(latex,Things) :-
    hdrug_latex:latex_fs_list(Things).

%% fs tk
sh_fs(tk,Things) :-
    hdrug_gui:tk_fs_list(Things).

%% fs clig
sh_fs(clig,Things) :-
    hdrug_clig:clig_fs_list(Things).

%% fs user
sh_fs(user,Things) :-
    hdrug_txt:txt_fs_list(Things).

extract_values([],[]).
extract_values([H0|T0],[H|T]) :-
    extract_value(H0,H),
    extract_values(T0,T).

extract_value(value(H),H).
extract_value(value(H,_),H).

show_tree(latex,Mode,Things):-
    hdrug_latex:latex_tree_list(Mode,Things).

show_tree(tk,Mode,Things) :-
    show_tree_tk_list(Things,Mode).

show_tree(clig,Mode,Things):-
    show_tree_clig_list(Things,Mode).

show_tree(dot,Mode,Things):-
    show_tree_dot_list(Things,Mode).

show_tree(user,Mode,Things):-
    show_tree_user_list(Things,Mode).

%% Nov 23, 2000: reverse list, since tk is used as a stack.
show_tree_clig_list(List0,Mode) :-
    lists:reverse(List0,List),
    show_tree_clig_list_(List,Mode). 

show_tree_clig_list_([],_).
show_tree_clig_list_([H|T],Mode):-
    hdrug_clig:clig_tree(Mode,H),
    show_tree_clig_list_(T,Mode).

%% Nov 23, 2000: reverse list, since tk is used as a stack.
show_tree_tk_list(List0,Mode) :-
    lists:reverse(List0,List),
    show_tree_tk_list_(List,Mode). 

show_tree_tk_list_([],_).
show_tree_tk_list_([H|T],Mode):-
    hdrug_gui:tk_tree(Mode,H),
    show_tree_tk_list_(T,Mode).

show_tree_user_list([],_).
show_tree_user_list([H|T],Mode):-
    hdrug_txt:pretty_graphic(Mode,H),
    show_tree_user_list(T,Mode).

show_tree_dot_list([],_).
show_tree_dot_list([H|T],Mode):-
    hdrug_dot:dot_tree(Mode,H),
    show_tree_dot_list(T,Mode).

%% Predicates
show_predicate(F/A,Filter,Output) :-
    !,
    all_defs(F,A,Clauses),
    show(Filter,Output,Clauses).
show_predicate(Atom,Filter,Output) :-
    atom_term(Atom,F/A),
    all_defs(F,A,Clauses),
    show(Filter,Output,Clauses).

%% notify also for show object from command line???
show_object_no(No,Filter,Output) :-
    (	Output == tk
    ->	hdrug_gui:notify_active_obj(No)
    ;	Output == clig
    ->	hdrug_gui:notify_active_obj(No)
    ;   true
    ),
    hdrug:object(No,Object),
    show(Filter,Output,[object(No,Object)]).

all_defs(F,A,Clauses) :-
    findall(Clause,a_def(F,A,Clause),Clauses).

a_def(F,A,clause(Term,Body)) :-
    a_user_clause(F/A,Term,Body).

%% change_thing_fs(In,Out)
%% In: changed(In0)
%%     In0
%% In0: clause(H,B), B a list
%%      value(H)
%% Out: clause(H)
%%      clause(H,B), B a list
%%      value(H)
%%      value(H,B), C a list
change_thing_fs(changed(Thing0),Thing) :-
    !,
    Thing0=Thing.
change_thing_fs(Thing0,Thing) :-
    copy_term(Thing0,Thing1,Cons0),
    rewrite_body(Cons0,[],Cons,[]),
    add_cons(Thing1,Cons,Thing2),
                                % worry: we loose identities
                                %      x(a(X,[]),b(X,[])) may be shortened in non-identical:
                                %      x(a(X,_1),b(X,_2)) ... :-(
    shorten_it(Thing2,Thing3),
    numbervars(Thing3,0,No),
    hdrug_feature:change_it_fs(Thing3,Thing,No).

change_thing_term(changed(Thing0),Thing) :-
    !,
    Thing0=Thing.
change_thing_term(Thing0,Thing) :-
    copy_term(Thing0,Thing1,Cons0),
    rewrite_body(Cons0,[],Cons,[]),
    add_cons(Thing1,Cons,Thing).

rewrite_body([],_,C,C).
rewrite_body([H|T],Other0,C0,C):-
    (	eq_member(H,Other0)
    ->  Other0=Other,
	C1=C0
    ;	C0 = [H|C1],
	Other=[H|Other0]
    ),
    rewrite_body(T,Other,C1,C).

eq_member(X,[Y|T]):-
    (	X==Y
    ->  true
    ;   eq_member(X,T)
    ).

add_cons(clause(H,B0),C,Clause) :-
    append(B0,C,B),
    add_cons_clause(B,H,Clause).
add_cons(value(H),Cons,Thing):-
    add_cons_value(Cons,H,Thing).

add_cons_value([],H,value(H)).
add_cons_value([H|T],Term,value(Term,[H|T])).

add_cons_clause([],Head,clause(Head)).
add_cons_clause([H|T],Head,clause(Head,[H|T])).

shorten_it(clause(H0,B0),clause(H,B)) :-
    shorten_goal(H0,H),
    shorten_goals(B0,B).
shorten_it(value(X0),value(X)) :-
    shorten_label(X0,X).
shorten_it(clause(H0),clause(H)):-
    shorten_goal(H0,H).
shorten_it(value(X0,Y0),value(X,Y)) :-
    shorten_label(X0,X),
    shorten_goals(Y0,Y).

shorten_goal(M:G0,M:G) :-
    !,
    shorten_goal(G0,G).

shorten_goal(when(C,G0),when(C,G)) :-
    !,
    shorten_goal(G0,G).

shorten_goal(G0,G) :-
    G0 =.. [F|T0],
    shorten_labels(T0,T),
    G =.. [F|T].

shorten_goals([],[]).
shorten_goals([H0|T0],[H|T]) :-
    shorten_goal(H0,H),
    shorten_goals(T0,T).

shorten_labels([],[]).
shorten_labels([H0|T0],[H|T]) :-
    shorten_label(H0,H),
    shorten_labels(T0,T).

shorten_label(V0,V) :-
    var(V0),
    !,
    V0=V.
shorten_label(V0,V) :-
    hdrug:shorten_label(V0,V).

%%%%%%%%%%%%%%%
%% should go to hdrug_txt

sh_term_list([],_).
sh_term_list([H|T],I):-
    sh_term(H,I),
    sh_term_list(T,I).

sh_term(Thing0,I) :-
    change_thing_term(Thing0,Thing),
    sh_term0(Thing,I).

sh_term0(value(FS),I):-
    sh_term_value(I,FS).
sh_term0(value(FS,Cons),I):-
    sh_term_value(I,FS),
    portray_clause((:-Cons)).
sh_term0(clause(H),_):-
    portray_clause(H).
sh_term0(clause(H,B),_):-
    portray_clause((H:-B)).

sh_term_value(print,FS) :-
    print(FS),nl.
sh_term_value(write,FS) :-
    write(FS),nl.

change_thing_tree(FS,Name,Tree):-
    hdrug:graphic_path(Name,FS,FS2),
    tree_def_to_tree(Name,FS2,Tree0), % builds datastructure
    !,				% cut to protect from bugs in graphic_* defs
    (	Name = matrix(_)
    ->	shorten_tree(Tree0,Tree1),
	!,			% cut in protect from bugs in shorten_* defs
	prettyvars(Tree1,0,No),
	hdrug_feature:change(Tree1,Tree2,[],_,No,_),
	change_back(Tree2,Tree)
    ;	Name = user(_)
    ->	hdrug:change_tree(Tree0,Tree)
    ;	Tree0=Tree
    ).

%% tk wants to have original label, for nice actions on mouse clicks.
change_thing_tree_with_orig(FS,Name,Tree):-
    hdrug:graphic_path(Name,FS,FS2),
    (	Name = matrix(_)
    ->	tree_def_to_tree(Name,FS2,Tree0),
	shorten_tree(Tree0,Tree1),
	prettyvars(Tree1,0,No),
	hdrug_feature:change(Tree1,Tree2,[],_,No,_),
	change_back(Tree2,Tree)
    ;	Name = user(_)
    ->	tree_def_to_tree(Name,FS2,Tree0),
	hdrug:change_tree(Tree0,Tree)
    ;	tree_def_to_tree_with_orig(Name,FS2,Tree)
    ),
    !.				% cut in protect from bugs in user defs
    

shorten_tree(tree(L0,M,Ds0),tree(L,M,Ds)) :-
    shorten_label(L0,L),
    shorten_ds(Ds0,Ds).

shorten_ds([],[]).
shorten_ds([H0|T0],[H|T]) :-
    shorten_tree(H0,H),
    shorten_ds(T0,T).

change_back(_/_=[_,a(0,Node)|Ds0],tree(Node,_,Ds)) :-
    change_back_ds(Ds0,Ds).
change_back(_/_=[a(type,_),a(_Label,Node),_,a(_,Ds0)],tree(Node,_,Ds)) :-
    change_back_ds(Ds0,Ds).
change_back(a(_,_/_=[_,a(0,Node)|Ds0]),tree(Node,_,Ds)) :-
    change_back_ds(Ds0,Ds).

change_back_ds([],[]).
change_back_ds([H0|T0],[H|T]) :-
    change_back(H0,H),
    change_back_ds(T0,T).
change_back_ds(_/_=[a(type,[[]])],[]).
change_back_ds(_/_=[a(type,[.]),a(_,H0),a(_,T0)],[H|T]) :-
    change_back(H0,H),
    change_back_ds(T0,T).

%%% trees..
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                       %
%  the following predicates must be defined elsewhere:  %
% graphic_path(Name,FS,FS2)                             %
% graphic_daughter(Name,Pos,FS,Daught)                  %
% graphic_label(Name,FS,Label)                          %
%                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
As an example, consider the following:

% picks out the part of a term that should be shown as a tree
graphic_path(test,Tree,Tree,_).

% picks out a daughter; Pos must be instantiated..
% enumerator
graphic_daughter(test,Pos,Tree,Arg):-
    member(Pos,[1,2,3,4,5,6,7,8,9,10]),
    arg(Pos,Tree,Arg).

% returns label of a daughter to be printed
graphic_label(test,Tree,Label):-
    functor(Tree,Label,_).

% now the call:

?- show(tree(test),user,s(np(det,n),vp(np,v))).

will then print:



     s
   _____   
  |     |  
  np   vp  
  ___  ___ 
 |  |  | | 
det n np v 
*/

f_graphic_label(Name,FS,Label):-
    (	nonvar(FS)
    ->	hdrug:graphic_label(Name,FS,Label)
    ;	Label = '_'
    ).

tree_def_to_tree(Name,FS,tree(Label,_,Ds)):-
    f_graphic_label(Name,FS,Label),
    graphic_daughters(Name,1,FS,Daughters),
    (	Daughters = [] 
    ->	Ds = []
    ;	def_tree_list(Daughters,Name,Ds)
    ),
    !.   %% catch multiple solutions in graphic_daughter def's

def_tree_list([],_,[]).
def_tree_list([H|T],Name,[H2|T2]):-
    tree_def_to_tree(Name,H,H2),
    def_tree_list(T,Name,T2).

tree_def_to_tree_with_orig(Name,FS,tree(Label/FS,_,Ds)):-
    f_graphic_label(Name,FS,Label),
    graphic_daughters(Name,1,FS,Daughters),
    (	Daughters = [] 
    ->	Ds = []
    ;	def_tree_list_with_orig(Daughters,Name,Ds)
    ),
    !.%% catch multiple solutions in graphic_daughter def's

def_tree_list_with_orig([],_,[]).
def_tree_list_with_orig([H|T],Name,[H2|T2]):-
    tree_def_to_tree_with_orig(Name,H,H2),
    def_tree_list_with_orig(T,Name,T2).

graphic_daughters(Name,Pos,FS,[]):-
        nothigherone(Name,Pos,FS),
        !.

graphic_daughters(Name,Pos,FS,[Daughter|Daughters]):-
        hdrug:graphic_daughter(Name,Pos,FS,Daughter),
        !,
        Pos2 is Pos + 1,
        graphic_daughters(Name,Pos2,FS,Daughters).

graphic_daughters(Name,Pos,FS,Ds):-
        Pos2 is Pos + 1,
        graphic_daughters(Name,Pos2,FS,Ds).

nothigherone(_Name,_Pos,Var):-
	var(Var),!.
nothigherone(Name,Pos,FS):-
        hdrug:graphic_daughter(Name,Pos2,FS,_),
        Pos2 >= Pos,
        !,
        fail.
nothigherone(_,_,_).

:- multifile user:help_info/4.
:- public user:help_info/4.

user:help_info(module,hdrug_show,"Visualization","

The libraries contain predicates to visualize trees,
feature-structures and Prolog terms (including Prolog clauses). A
number of different output media are available: LaTeX, Tcl/Tk, CLiG, DOT,
and ordinary text output. The visualization tools are all available by
means of a single generic predicate show/3.

Viewing Prolog Terms representing Feature Structures

Note that a couple of predicates are available to view Prolog terms as
feature structures. Again, the predicate show/3 is available as an
interface to this functionality. For example, you might
try the conjunction: 

        Y:cat:agr <=> Y:cat:subj:cat:agr, show(fs,tk,[value(Y)]).

Instead of *tk*, any of the identifiers *latex*, *user*, *clig*, *dot* can be
used to direct the output to a different medium. For instance, the
query

        Y:cat:agr <=> Y:cat:subj:cat:agr, show(fs,latex,[value(Y)]).


But if you insist on ordinary output, try:

        show(fs,user,[value(X)]).

This produces:

         {sign}
         |cat {verb}
         |    |agr <A>
         |    |subj {sign}
         |    |     |cat {cat}
         |    |     |    |agr <A>.

Not only can you view feature structures this way, but also clauses;
cf. show/3 below.
 
Tree Formats

The libraries contain extensive possibilities to produce output in the
form of trees. Only a few declarations are needed to define what
things you want to see in the tree. In effect, such declarations
define a `tree format'.

In Hdrug, there can be any number of tree formats. These tree formats
are named by a ground identifier. A tree format consists of three
parts: the *path* definition indicates what part of the object you
want to view as a tree; the *label* definition indicates how you want
to print the node of a tree; and the *daughter* definition indicates
what you consider the daughters of a node.

Because we want to be able to have multiple tree formats around, we
must declare the corresponding predicates `multifile', as otherwise
existing tree formats would be erased.

For example, the following predicates define a tree-format called `s'
(this example is taken from the `Dcg' application).

        :- multifile graphic_path/3.
        graphic_path(s,node(_,S),S).
        
        :- multifile graphic_label/3.
        graphic_label(s,Term,Label) :-
            functor(Term,Label,_).
        
        :- multifile graphic_daughter/4.
        graphic_daughter(s,1,Term,D) :-
            arg(1,Term,D).
        
        graphic_daughter(s,2,Term,D) :-
            arg(2,Term,D).

The first predicate defines that we want to take the semantics part
of a node as the term that we want to view as a tree. The second
predicate defines that for a given tree Term we want to print its
functor as the node label. Finally the third predicate defines that
for a given tree Term the first daughter is to be the first
argument of the term, and the second daughter is to be the second
argument. 

As another example of a tree format definition, consider the
constraint-based Categorial Grammar application.
application. Here we find:

        :- multifile graphic_path/3.
        graphic_path(syn,Obj,Obj).
        
        :- multifile graphic_label/3.
        graphic_label(syn,tree(Sign,_,[_|_]),Label) :-
            cat_symbol(Sign,Label).
        
        graphic_label(syn,tree(W,_,[]),W).
        
        :- multifile graphic_daughter/4.
        graphic_daughter(syn,No,tree(_,_,[H|T]),D) :-
            nth(No,[H|T],D).

Here, objects generally are of the form tree(Node,_,ListOfDs).
Therefore, the path part of the tree format definition simply unifies
the object and the tree part. The label part of the tree format
definition distinguishes two cases. If there are no more daughters,
then the node is a terminal, and this terminal is simply taken to be
the node label. In the other case the node label is defined by a
seperate predicate `cat_symbol'. This predicate changes the internal
representation into some more readable format. Finally, the daughter
part of the tree format definition uses the Sicstus library predicate
`nth'.  The effect of the definition is that the first daughter is the
first element of the daughter list, etc.

Tk Output

The library defines the predicate show/3 as a generic
interface to the visualization tools. If a tree is to be displayed on
the Tcl/Tk Canvas widget, then we can use this predicate by taking the 
desired tree format as the first argument, the atom tk as the
second argument, and a list of objects we want to be displayed as the
third and final argument.  For instance:

        ?- findall(object(A,B), object(A,B), Objects),
           show(syn,tk,Objects).

If the tree is output thru the Tk/Tcl canvas, then the nodes of the
trees are buttons. For each tree format we can define what action
should be undertaken if a button is pressed. This is defined by the
predicate show_node/2. The first argument is the identifier of the
tree format, the second argument is the current node (note: this is
not the label as defined by graphic_label, but the term on the basis
of which graphic_label is defined).

The following definition, from the Constraint-based Categorial Grammar
application, prints the node as a feature structure in a separate Tk
window.

        show_node(syn,tree(Sign,[_|_],_)) :-
                show(fs,tk,[value(Sign)]).

If this predicate is not defined then the label will simply be written
out as a Prolog term to standard output.

Similarly, the predicates show_node2/2 and show_node3/2 can be used
to define an action for pressing the second and third mouse-button
respectively. Generally these predicates should be defined multifile. 


LaTeX output

The predicate show/3 is also used to produce LaTeX output of
trees. A variant of the previous example produces LaTeX:

        ?- findall(object(A,B), object(A,B), Objects),
           show(syn,latex,Objects).

This ensures that a LaTeX file is created and the appropriate shell
commands are called to get ghostview to display the tree. The first
argument is the name of a tree-format.

CLiG Output

A further possibility concerns is to use the CLiG system for
displaying output. In that case the example becomes;

        ?- findall(object(A,B), object(A,B), Objects),
           show(syn,latex,Objects).

Dot Output

For trees, you can also use the DOT graph visualization programme.

ASCII Art Output

Ordinary text (to standard output) is available as well; in that case
the identifier *user* is used:

        ?- findall(object(A,B), object(A,B), Objects),
           show(syn,user,Objects).

Trees of feature structures

Trees in which each of the nodes is a feature-structure are supported
for Tk output and LaTeX output. Nodes are interpreted as a
description of a feature-structure if the tree format identifier
matches matrix(_). 

User defined action for a given node can be obtained using a tree
format which matches user(_). In such a case you are responsible for
displaying a given node by defining the predicate tk_tree_user_node/2
where the first argument is the label of the current node, and the
second argument is a Tcl/Tk frame identifier already packed as part of
the tree, which can be further worked upon.

Visualization of clauses

The third argument of the predicate show
can be a clause. An is example is
http://www.let.rug.nl/~vannoord/Hdrug/Manual/clause.png

Visualization of the type declarations

Refer to the predicates pretty_type/0, pretty_type/1.


").
