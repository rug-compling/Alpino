% rules
rule(r(r1,s,_), [r(_,np(Num),_), r(_,vp(Num),_)]).
rule(r(r2,np(Num),_), [r(_,d(Num),_), r(_,n(Num),_)]).
rule(r(r3,vp(Num),_), [r(_,v(Num),_), r(_,np(_),_)]).
rule(r(r4,vp(Num),_), [r(_,vp(Num),_), r(_,pp,_)]).
rule(r(r5,np(Num),_), [r(_,np(Num),_), r(_,pp,_)]).
rule(r(r6,pp,_), [r(_,p,_), r(_,np(_),_)]).

% lexicon
word(l1,the,d(_)).
word(l2,a,d(sg)).
word(l3,dog,n(sg)).
word(l4,cat,n(sg)).
word(l5,dogs,n(pl)).
word(l6,cats,n(pl)).
word(l7,barks,vp(sg)).
word(l8,barked,vp(_)).
word(l9,bark,vp(pl)).
word(l10,chases,v(sg)).
word(l11,chased,v(_)).
word(l12,chase,v(pl)).
word(l13,park,n(sg)).
word(l14,in,p).

lexicon(W,r(Id,Cat,[w(W)])) :-
    word(Id,W,Cat).

:- compile(suite2).
:- compile(treebank2).
