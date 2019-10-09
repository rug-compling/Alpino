:- module(bug,[]).

%%%%%%%%%%%%%%%
%% GENERATOR %%
%%%%%%%%%%%%%%%

generate(o(Obj,Str,_)):-
	user:deriv_tree(Obj,Tree),
	bug(Obj,Str,[],Tree).

bug(Node,P0,P,Tree) :-
	user:semantics(Node,Sem),
	bug(Sem,Node,P0,P,Tree).

:- block bug(-,?,?,?,?).

bug(_Sem,Node,P0,P,Tree) :-
	predict_headg(Node,SmallNode,Q0,Q,SmallTree),
	connectg(SmallNode,Node,Q0,Q,P0,P,SmallTree,Tree).

connectg(X,X,P0,P,P0,P,Tree,Tree).
connectg(SmallNode,Node,P1,P2,Q0,Q,Small,Tree):-
  	user:h_rule(SmallNode,MidNode,Left,Right,Rule),
	bugl(Left,P0,P1,[Small|Rtrees],Trees),
	bugr(Right,P2,P,Rtrees),
	connectg(MidNode,Node,P0,P,Q0,Q,tree(Rule,_,Trees),Tree).

:- block bugr(-,?,?,?).

bugr([],P,P,[]).
bugr([H|T],P0,P,[Ht|Tt]) :-
	bug(H,P0,P1,Ht),
	bugr(T,P1,P,Tt).

:- block bugl(-,?,?,?,?).
bugl([],P,P,T,T).
bugl([H|T],P0,P,T0,Tt) :-
	bug(H,P1,P,Ht),
	bugl(T,P0,P1,[Ht|T0],Tt).

predict_headg(Node,SmallNode,P0,P,tree(Rule,_,[])):-
	user:semantics(Node,Sem),
	user:semantics(SmallNode,Sem),
	user:hfc(SmallNode,Node),
	user:lexicon(Words,SmallNode,Rule),
	append(Words,P,P0).




