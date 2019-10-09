top(main,s(_,[],[],[])).
semantics(_,_).
phonology(s(P,[],_,_),P).

s ==> as, bs, cs, ds, es.

as ==> [].
as ... xb ==> [a], as.

bs ==> [].
bs ... xc ==> xb, [b], bs.

cs ==> [].
cs ... xd ==> xc, [c], cs.

ds ==> [].
ds ... xe ==> xd, [d], ds.

es ==> [].
es  ==> xe, [e], es.


sentence(a,[a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d,e,e,e,e]).
