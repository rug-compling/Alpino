top(main,s(_,[],[],[])).
semantics(_,_).
phonology(s(P,[],_,_),P).

s ==> as, bs, cs.

as ==> [].
as ... xb ==> [a], as.

bs ==> [].
bs ... xc ==> xb, [b], bs.

cs ==> [].
cs ==> xc, [c], cs.

sentence(a,[a,a,a,a,b,b,b,b,c,c,c,c]).
