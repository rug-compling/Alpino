top(main,s(_,[],[],[])).
semantics(_,_).
phonology(s(Phon,[],_,_),Phon).

s ==> np, vp.

np ==> pn.
np ==> det, noun, rel.
np ==> det, noun, pp.
np ==> trace.

vp ==> verb, np.
vp ==> verb.

rel ==> [].
rel ==> open, rel_marker, s, close.

open ... close ==> [].

rel_marker ... trace ==> rel_pronoun.

pp ==> prep, np.

pn ==> [goofy].
pn ==> [mickey].
pn ==> [donald].

det ==> [].
det ==> [the].
det ==> [a].
det ==> [an].

noun ==> [mouse].
noun ==> [cat].
noun ==> [fish].
noun ==> [dog].

prep ==> [with].
prep ==> [without].

verb ==> [likes].
verb ==> [chased].
verb ==> [squeaks].

rel_pronoun ==> [that].
rel_pronoun ==> [who].
rel_pronoun ==> [].

sentence(a,[the,cat,that,chases,the,dog,likes,mickey]).
sentence(b,[the,cat,that,chases,the,dog,likes,a,fish,who,squeaks]).
