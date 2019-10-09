:- module(bug,[]).

%%%%%%%%%%%%%%%
%% GENERATOR %%
%%%%%%%%%%%%%%%

generate(o(Obj,Str,_)):-
	bug(node(Obj,Str,[])).

bugs([]).
bugs([First|Rest]):-
	bug(First),
	bugs(Rest).

bug(Node):- 
	predict_headg(Node,SmallNode),
	connectg(SmallNode,Node).

connectg(X,X).
connectg(SmallNode,Node):-
  	user:cr(SmallNode,MidNode,OtherNodes,_Rule),
	check_g_link(MidNode,Node),
	bugs(OtherNodes),
	connectg(MidNode,Node).
connectg(node(SmallNode,_,_),Node):-
	user:head_gap(SmallNode,MidNode),
	check_g_link(node(MidNode,_,_),Node),
	connectg(node(MidNode,P,P),Node).

predict_headg(Node,SmallNode):-
	connection(Node,SmallNode),
	user:ncr(SmallNode,Ds,_Rule),
	check_g_link(SmallNode,Node),
	bugs(Ds).

connection(node(A,_,_),node(B,_,_)):-
	user:semantics(A,Sem),
	user:semantics(B,Sem).

check_g_link(node(A,_,_),node(B,_,_)):-
	\+ \+ user:g_link(A,B).








