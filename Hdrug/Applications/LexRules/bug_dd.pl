:- module(bug_dd,[]).

:- use_module(lists, library(lists), [ append/3 ]).

%%%%%%%%%%%%%%%
%% GENERATOR %%
%%%%%%%%%%%%%%%

generate(o(Obj,Str,_)):-
	user:deriv_tree(Obj,Tree),  
	bug(Obj,Str,[],Tree0),
	convert_tree(Tree0,Tree).   % since we have the tree, we might as
                                    % well put it in a readable form for
                                    % the user to look at..

bug(Node,P0,P,h(Predict,Connect)) :-
	predict_headg(Node,SmallNode,Q0,Q,Predict),
	connectg(SmallNode,Node,Q0,Q,P0,P,Connect).

connectg(X,X,P0,P,P0,P,[]).
connectg(SmallNode,Node,P1,P2,Q0,Q,[rule(Name,Ltrees,Rtrees)|Tree]):-
  	user:h_rule(SmallNode,MidNode,Left,Right,Name),
	bugl(Left,P0,P1,Ltrees),
	bugr(Right,P2,P,Rtrees),
	connectg(MidNode,Node,P0,P,Q0,Q,Tree).

bugr([],P,P,[]).
bugr([H|T],P0,P,[Ht|Tt]) :-
	user:deriv_tree(H,Ht),
	bug(H,P0,P1,Ht),
	bugr(T,P1,P,Tt).

bugl([],P,P,[]).
bugl([H|T],P0,P,[Ht|Tt]) :-
	user:deriv_tree(H,Ht),
	bug(H,P1,P,Ht),
	bugl(T,P0,P1,Tt).

predict_headg(Node,SmallNode,Words,Tail,Rule):-
	user:semantics(Node,Sem),
	user:semantics(SmallNode,Sem),
	user:hfc(SmallNode,Node),
	(  var(Rule)                              % first time ..
        -> user:lexicon(Words0,SmallNode,Rule),   % take a lexical entry
	   user:get_args(SmallNode,Args),         % take its arguments
	   bugr(Args,_,_,_)                       % and generate these
        ;  user:lexicon(Words0,SmallNode,Rule)    % second time, just use lexical entry
        ),
	append(Words0,Tail,Words).

%% convert_tree(Tree0,Tree).
convert_tree(h(P,C),Tree) :-
	convert_tree(C,tree(P,_,[]),Tree).

convert_tree([],T,T).
convert_tree([rule(H,Ltrees0,Rtrees0)|T],Tree0,Tree) :-
	convert_trees_l(Ltrees0,[Tree0|Rtrees],Trees),   % append implicitly
	convert_trees(Rtrees0,Rtrees),
	convert_tree(T,tree(H,_,Trees),Tree).

% reverse implicitly
convert_trees_l([],L,L).
convert_trees_l([H|T],L0,L) :-
	convert_tree(H,Ht),
	convert_trees_l(T,[Ht|L0],L).

convert_trees([],[]).
convert_trees([H0|T0],[H|T]) :-
	convert_tree(H0,H),
	convert_trees(T0,T).

/*
Problems in generation.
-----------------------

In a flat rule you don't know how many daughters there are, unless you
know the subcat list of the head. However, the subcat list of a head
may be underspecified. For example in the case of partial vp
topicalization:

[[slapen] wil jan

the generator selects `wil' as the head-corner. In order to complete
the verb phrase it has to generate the arguments not selected by
`slapen' (or any other verb). It does not know yet the semantics of
these arguments, nor the NUMBER of the arguments. The first problem is
solved by delaying generation of phrases with uninstantiated
semantics. If we were to delay the generation of an unknown sequence
of categories, then we will not be able to get an instantiated
slash-value and hence we cannot properly generate the topic! This is so
because the foot feature principle can only be applied after the list
of arguments has been generated.

Note that this problem is not specific for semantic-head-driven
generation but also occurs in Shiebers chart-based bottom-up approach,
or in Wedekind's top-down approach.

In bug_c.pl the problem is `solved' by associating a size to each
semantic structure. In order to generate a semantic structure with
size s we can never (sub-) generate more than s categories.  This is
potentially problematic --- but no actual problems have been
encountered yet. Note that such a size function could be added to
any of the other strategies.

In bug_dd.pl the problem is solved by associating each lexical entry
with a list of arguments that it is going to select at some point --
either directly via the subcat list or indirectly via topicalisation or 
extraposition. This list is generated first. As the elements of the list
are (at least partially) reentrant with the elements on all kinds of
selection lists we only have to make sure that this list is properly
ordered.

Note that a history mechanism is used in order not to generate the
same thing twice (in fact, we do generate it twice, but the second
time is deterministically). 

Finally note that bug_dd is MUCH faster than bug_c.
*/



