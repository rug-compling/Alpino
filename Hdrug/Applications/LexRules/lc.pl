:- module(lc,[]).

parse(o(Obj,Str,_)):-
	length(Str,P),
	goal(Obj,0,P).

goals([],P,P).
goals([First|Rest],P0,P):-
	goal(First,P0,P1),
	goals(Rest,P1,P).

goal(Node,P0,P):- 
	user:lex(P0,P1,SmallNode,_Name),
	lc(SmallNode,Node,P1,P).

lc(X,X,P,P).
lc(SmallNode,Node,P0,P):-
  	user:l_rule(SmallNode,MidNode,OtherNodes,_Rule),
	goals(OtherNodes,P0,P1),
	lc(MidNode,Node,P1,P).







