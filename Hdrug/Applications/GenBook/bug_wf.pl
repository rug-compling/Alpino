:- module(bug_wf,[]).

:- use_module(wf).

%%%%%%%%%%%%%%%
%% GENERATOR %%
%%%%%%%%%%%%%%%

clean :-
	clean_up_wf.

count :-
	wf_count.

count(W) :-
	wf_count(W).

generate(o(Obj,Str,_)):-
	wf(bug_wf:bug(node(Obj,Str,[]))).

bugs([]).
bugs([First|Rest]):-
	wf(bug_wf:bug(First)),
	bugs(Rest).

bug(Node):- 
	predict_headg(Node,SmallNode),
	connectg(SmallNode,Node).

connectg(X,X).
connectg(SmallNode,Node):-
  	user:cr(SmallNode,MidNode,OtherNodes,_Rule),
	bug:check_g_link(MidNode,Node),
	bugs(OtherNodes),
	connectg(MidNode,Node).
connectg(node(SmallNode,_,_),Node):-
	user:head_gap(SmallNode,MidNode),
	bug:check_g_link(node(MidNode,_,_),Node),
	connectg(node(MidNode,P,P),Node).

predict_headg(Node,SmallNode):-
	bug:connection(Node,SmallNode),
	user:ncr(SmallNode,Ds,_Rule),
	bug:check_g_link(SmallNode,Node),
	bugs(Ds).




















