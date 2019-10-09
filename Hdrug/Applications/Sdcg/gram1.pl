% rules
rule(r(r1,s,_), [r(_,np(Num),_), r(_,v(Num),_)]).
rule(r(r2,np(Num),_), [r(_,d(Num),_), r(_,n(Num),_)]).

% lexicon
word(l1,the,d(_)).
word(l2,a,d(sg)).
word(l3,dog,n(sg)).
word(l4,cat,n(sg)).
word(l5,dogs,n(pl)).
word(l6,cats,n(pl)).
word(l7,barks,v(sg)).
word(l8,barked,v(_)).
word(l9,bark,v(pl)).

lexicon(W,r(Id,Cat,[w(W)])) :-
    word(Id,W,Cat).

% features

feature(_,Id,r(Id,_,_)).

:- compile(suite).
:- compile(treebank1).
