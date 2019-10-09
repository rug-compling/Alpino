pp_chart :-
    parsing(Sent),        %% hook..
    nodes_and_bedges(Sent,0,Nodes,Bedges),
    findall(edge(P,Q,Cat,Ident),ale_edge0(Ident,P,Q,Cat),Edges),
    pp_chart(Nodes,Edges,Bedges).

nodes_and_bedges([],N,[N],[]).
nodes_and_bedges([H|T],N0,[N0|Ns],[edge(N0,N,H,N0)|Es]):-
    N is N0+1,
    nodes_and_bedges(T,N,Ns,Es).

ale_edge0(Ident,P,Q,Cat):-
    ale_edge(Ident,P,Q,_,_,_,_,Cat),
    P \== Q.   %% these exist always anyway....

pp_chart_show_node_help('<1> tk <2> clig <3> latex').

pp_chart_item(Ident) :-
    ale_edge(Ident,_M,_N,Tag,SVs,Iqs,_Dtrs,_RuleName),
    tk_ale(fs(Tag-SVs),Iqs).
pp_chart_item2(Ident) :-
    ale_edge(Ident,_M,_N,Tag,SVs,Iqs,_Dtrs,_RuleName),
    clig_ale(fs(Tag-SVs),Iqs).
pp_chart_item3(Ident) :-
    ale_edge(Ident,_M,_N,Tag,SVs,Iqs,_Dtrs,_RuleName),
    latex_ale(fs(Tag-SVs),Iqs).

%%%% tree of edge items
:- multifile graphic_path/3,
    graphic_label/3,
    graphic_daughter/4,
    show_node/2,
    show_node2/2,
    show_node3/2,
    tk_tree_show_node_help/2.

graphic_path(chart,Tree0,Tree) :-
    (	var(Tree0)
    ->  true
    ;	create_chart_tree(Tree)
    ).

graphic_label(chart,tree(i(L),_,_),L):-!.
graphic_label(chart,tree(L,_,_),L).

graphic_daughter(chart,Nth1,tree(_,_,Ds),D) :-
    nth(Nth1,Ds,D).


create_chart_tree(tree(chart,_,Ds)) :-
    findall(P,ale_edge(_,P,_,_,_,_,_,_),Ds0),
    sort(Ds0,Ds1),
    create_chart_tree_ds(Ds1,Ds).

create_chart_tree_ds([],[]).
create_chart_tree_ds([No|Nos],[H|T]) :-
    create_chart_tree0(No,H),
    create_chart_tree_ds(Nos,T).

create_chart_tree0(No,tree(No,_,Ds)):-
    findall(Pos,ale_edge(_Ident,No,Pos,_,_,_,_,_),Ds0),
    sort(Ds0,Ds1),
    create_chart_tree_ds(Ds1,Ds,No).

create_chart_tree_ds([],[],_).
create_chart_tree_ds([Q|Qs],[H|T],P):-
    create_chart_tree0(P,Q,H),
    create_chart_tree_ds(Qs,T,P).

create_chart_tree0(P,Q,tree(P-Q,_,Ds)):-
    findall(Cat,ale_edge(_,P,Q,_,_,_,_,Cat),Ds0),
    sort(Ds0,Ds1),
    create_chart_tree_ds(Ds1,Ds,P,Q).

create_chart_tree_ds([],[],_,_).
create_chart_tree_ds([Cat|T0],[tree(Cat,_,Ds)|T],P,Q):-
    findall(tree(i(T),_,[]),ale_edge(T,P,Q,_,_,_,_,Cat),Ds0),
    sort(Ds0,Ds),
    create_chart_tree_ds(T0,T,P,Q).

show_node(chart,tree(i(Ident),_,_)):-
    pp_chart_item(tk,Ident).

show_node2(chart,tree(i(Ident),_,_)):-
    pp_chart_item(clig,Ident).

show_node3(chart,tree(i(Ident),_,_)):-
    pp_chart_item(latex,Ident).


tk_tree_show_node_help(chart,'<1> tk <2> clig <3> latex').
