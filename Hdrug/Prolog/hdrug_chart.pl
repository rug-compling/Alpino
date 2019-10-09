:- module( hdrug_chart, [ pp_chart/3 ]).

:- expects_dialect(sicstus).

:- use_module(hdrug_gui).
:- use_module(hdrug_util).
:- use_module(library(arrays)).

:- multifile
    user:help_info/4.
:- public
    help_info/4.
:- discontiguous
    help_info/4.

help_info(class,hook,"Hook Predicates",
"This section lists the hook predicates for the hdrug_chart
library."). 

help_info(class,pred,"Predicates",
"This section lists the predicates exported by the hdrug_chart
library."). 

help_info(class,flag,"Global Variables",
"This section lists the global variables maintained by the
hdrug_chart library.").

user:help_info(module,hdrug_chart,"Displaying Charts",
"The module hdrug_chart is intended to be used to display chart-like
data-structures (on a Tcl/Tk canvas)."). 

help_info(pred,pp_chart,"pp_chart(Nodes,Edges,Bedges)",
"Pretty-printing routine (on the Tk widget) for chart-like
datastructures. Nodes is a list of integers, indicating the nodes of
the chart (the string positions). Edges is a list of edges. Each edge
is a term edge(P,Q,Cat,Ident) where P and Q are chart nodes, Cat is
some atom used as the label of the edge, and Ident is some atom used
to identify the edge. This identifier is passed on to the hook
predicates pp_chart_item and pp_chart_item_b which can be used to
define an action for clicking the label of a chart item. Bedges is
also a list of edges; these edges are placed on and below the nodes of
the chart (this can be used, for instance, to display the words of the
chart). Example: 

        ?- pp_chart([0,1,2],[edge(0,1,np,1),edge(1,2,vp,2),edge(0,2,s,3)],
     	                    [edge(0,1,jan,4), edge(1,2,slaapt,5)]).
	       ").

pp_chart(Nodes0,Edges0,BEdges0):-
    sort(Nodes0,Nodes),
    select_canvas(chart,Canvas),
    tcl('empty_canvas ~w',[Canvas]),
    help_info(Canvas),
    pp_chart_nodes(Nodes,Canvas),
    add_keys(Edges0,Edges1),
    sort(Edges1,Edges),
    add_keys(BEdges0,BEdges1),
    sort(BEdges1,BEdges),
    new_array(Bs0),
    new_array(Ts0),
    pp_chart_edges(Edges,t,Canvas,Ts0),
    pp_chart_edges(BEdges,b,Canvas,Bs0),
    ratio_y(Canvas,Ratio),
    tcl("~w yview moveto ~w",[Canvas,Ratio]),
    tcl("~w xview moveto 0",[Canvas]).

pp_chart_edges([],_,_,_).
pp_chart_edges([N|T],BT,C,A0) :-
    pp_chart_edge(BT,N,C,A0,A),
    pp_chart_edges(T,BT,C,A).

pp_chart_nodes([],_).
pp_chart_nodes([N|T],C) :-
    pp_chart_node(N,C),
    pp_chart_nodes(T,C).

pp_chart_node(N,C) :-
    hdrug_flag(chart_xdist,Xdist),
    chart_ystart(Ystart,C),
    X0 is (N*Xdist)-2,
    X1 is (N*Xdist)+2,
    X is N*Xdist,
    Y0 is Ystart+2, Y1 is Ystart-2,
    Yt is Ystart+10,
    tcl('~w create oval ~w ~w ~w ~w -fill black',[C,X0,Y0,X1,Y1]),
    tcl('~w create text ~w ~w -text {~w}',[C,X,Yt,N]).

add_keys([],[]).
add_keys([edge(P,Q,A,B)|T0],[D-edge(P,Q,A,B)|T]):-
    D is Q-P,
    add_keys(T0,T).

help_info(flag,chart_xdist,"chart_xdist",
"This flag determines the horizontal distance between nodes of the
chart."). 
help_info(flag,chart_ydist,"chart_ydist",
"This flag determines the distance between edges over the same
range."). 

:- initialize_flag(chart_xdist,130).
%:- initialize_flag(chart_ystart,8000).
:- initialize_flag(chart_ydist,70).

pp_chart_edge(t,_-edge(P,Q,Cat,Ident),C,A0,A):-
    new_max(P,Q,N,A0,A),
    hdrug_flag(chart_xdist,Xdist),
    chart_ystart(Ystart,C),
    hdrug_flag(chart_ydist,Ydist),
    X0 is P*Xdist,
    X is Q*Xdist,
    X1 is (X0+X)/2,
    Y1 is Ystart-(N*Ydist),
    Y2 is Ystart-(N*Ydist/2),
    tcl('~w create line ~w ~w ~w ~w ~w ~w -smooth 1',
	[C,X0,Ystart,X1,Y1,X,Ystart]),
    tcl('catch {destroy ~w.label~w}',[C,Ident]),
    tcl('button ~w.label~w -text {~w} -relief raised \
            -command "prolog hdrug:pp_chart_item(~w)"',
	[C,Ident,Cat,Ident]),
    tcl('bind ~w.label~w <Enter> {raise ~w.label~w}',[C,Ident,C,Ident]),
    tcl('bind ~w.label~w <2> "prolog hdrug:pp_chart_item2(~w)"',
	[C,Ident,Ident]),
    tcl('bind ~w.label~w <3> "prolog hdrug:pp_chart_item3(~w)"',
	[C,Ident,Ident]),
    tcl('~w create window ~w ~w -window ~w.label~w -anchor center',
	[C,X1,Y2,C,Ident]).

pp_chart_edge(b,_-edge(P,Q,Cat,Ident),C,A0,A):-
    new_max(P,Q,N0,A0,A1),
    (	P =:= Q-1,
	N0=:= 1
    ->  N=0, aset(P,A0,0.0001,A)
    ;	N0=N, A1=A
    ),
    hdrug_flag(chart_xdist,Xdist),
    chart_ystart(Ystart,C),
    hdrug_flag(chart_ydist,Ydist),
    X0 is P*Xdist,
    X is Q*Xdist,
    X1 is (X0+X)/2,
    Y1 is Ystart+(N*Ydist),
    Y2 is Ystart+(N*Ydist/2),
    tcl('~w create line ~w ~w ~w ~w ~w ~w -smooth 1',
	[C,X0,Ystart,X1,Y1,X,Ystart]),
    tcl('catch {destroy ~w.label~w}',[C,Ident]),
    tcl('button ~w.label~w -text {~w} -relief raised \
            -command "prolog hdrug:pp_chart_item_b(~w)"',
	[C,Ident,Cat,Ident]),
    tcl('bind ~w.label~w <2> "prolog hdrug:pp_chart_item_b2(~w)"',
	[C,Ident,Ident]),
    tcl('bind ~w.label~w <3> "prolog hdrug:pp_chart_item_b3(~w)"',
	[C,Ident,Ident]),
    tcl('bind ~w.label~w <Enter> {raise ~w.label~w}',[C,Ident,C,Ident]),
    tcl('~w create window ~w ~w -window ~w.label~w -anchor center',
	[C,X1,Y2,C,Ident]).

new_max(P,Q,Max,A0,A) :-
    get_max(P,Q,0,Cur,A0),
    Max is Cur+1,
    set_max(P,Q,Max,A0,A).

get_max(P,Q,Cur0,Cur,Arr) :-
    (	P=:=Q
    ->	Cur0=Cur
    ;   aref0(P,Arr,Cur1),   
	Cur2 is max(Cur0,Cur1),
	P1 is P+1,
	get_max(P1,Q,Cur2,Cur,Arr)
    ).

set_max(P,Q,Max,Arr0,Arr):-
    (	P=:=Q
    ->  Arr0=Arr
    ;	aset(P,Arr0,Max,Arr1),
	P1 is P+1,
	set_max(P1,Q,Max,Arr1,Arr)
    ).

aref0(Index,Array,Item0) :-
    (   aref(Index,Array,Item)
    ->  Item0=Item
    ;   Item0=0
    ).

help_info(Canvas) :-
    %% tell the user what the actions for pressing mouse buttons is
    (	hdrug:pp_chart_show_node_help(Message)
    ->	tcl("help_line ~a {~a}",[Canvas,Message])
    ;	true
    ).


help_info(hook,pp_chart_show_node_help,"pp_chart_show_node_help(Atom)",
"Short atom to be displayed in the help-line upon entering nodes of
the chart. This is typically used to indicate the corresponding
actions of mouse clicks on the nodes."). 

help_info(hook,pp_chart_item,"pp_chart_item[23](Ident)",
"Used by hdrug_chart. This predicate can be used to define an action
to be executed upon clicking the label of an edge of the chart. 
This variant is for edges above the horizontal axis. The
argument Ident refers to the fourth argument of the relevant edge that
was one of the elements in the list passed on as the second argument
of the pp_chart/3 predicate. The variants with a 2 or 3 suffix are
used to define an action for the second or third mouse button."). 

help_info(hook,pp_chart_item_b,"pp_chart_item_b[23](Ident)",
"Used by hdrug_chart. This predicate can be used to define an action
to be executed upon clicking the label of an edge of the chart. 
This variant is for edges below the horizontal axis. The
argument Ident refers to the fourth argument of the relevant edge that
was one of the elements in the list passed on as the third argument of
the pp_chart/3 predicate. The variants with a 2 or 3 suffix are used
to define an action for the second or third mouse button."). 
	       
chart_ystart(Y,Canvas) :-
%    tcl('expr [lindex [lindex [~w configure -scrollregion ] 4 ] 3 ] - [lindex [lindex [~w configure -scrollregion ] 4 ] 1 ]',[Canvas,Canvas],Y0),
    tcl('lindex [lindex [~w configure -scrollregion ] 4 ] 3',[Canvas],Y0),
    atom_codes(Y0,Y0chars), number_codes(Y0int,Y0chars),
    tcl('lindex [lindex [~w configure -scrollregion ] 4 ] 1',[Canvas],Y1),
    atom_codes(Y1,Y1chars), number_codes(Y1int,Y1chars),
    Y is Y1int + (Y0int-Y1int)/2.


ratio_y(Canvas,R) :-
    tcl('lindex [lindex [~w configure -scrollregion ] 4 ] 3',[Canvas],Y0),
    atom_codes(Y0,Y0chars), number_codes(Y0int,Y0chars),
    tcl('lindex [lindex [~w configure -scrollregion ] 4 ] 1',[Canvas],Y1),
    atom_codes(Y1,Y1chars), number_codes(Y1int,Y1chars),
    Y is (Y0int-Y1int)/2,
    tcl('lindex [~w configure -height] 4',[Canvas],Y2),
    format(user_error,"~w~n",[Y2]),
    atom_codes(Y2,Y2chars), number_codes(Y2int,Y2chars),
    R is (Y - Y2int)/(Y0int-Y1int),
    format(user_error,"~w~n",[R]).

