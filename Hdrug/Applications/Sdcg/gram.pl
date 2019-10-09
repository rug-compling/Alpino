% rules
rule(r(r1,s,_), [r(_,nps,_), r(_,vs,_)]).
rule(r(r2,s,_), [r(_,npp,_), r(_,vp,_)]).
rule(r(r3,nps,_), [r(_,ds,_), r(_,ns,_)]).
rule(r(r4,npp,_), [r(_,dp,_), r(_,np,_)]).

% lexicon
word(l1,the,ds).
word(l2,a,ds).
word(l3,the,dp).
word(l4,dog,ns).
word(l5,cat,ns).
word(l6,dogs,np).
word(l7,cats,np).
word(l8,barks,vs).
word(l9,barked,vs).
word(l10,bark,vp).
word(l11,barked,vp).

lexicon(W,r(Id,Cat,[w(W)])) :-
    word(Id,W,Cat).


feature(scfg,Id,r(Id,_,_)).



:- compile(suite).
:- compile(treebank).
