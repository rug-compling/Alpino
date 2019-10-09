top(main,s(_,[],[],[])).
semantics(_,_).
phonology(s(Phon,[],_,_),Phon).

s ==> np, vp.

np ==> pn.
np ==> det, noun, rel.
np ==> det, noun, pp.
np ==> det, ap, noun.
np ==> trace.

vp ==> [kisses], np.
vp ==> [sleeps].
vp ==> [likes], np.
vp ==> [chased], np.
vp ==> [squeaks].

vp ==> [is], ap.
vp ==> [is], np.

ap ==> [nice].

ap ==> a, vp_inf.

a ==> [eager].

a ... trace ==> [easy].

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
noun ==> [person].

prep ==> [with].
prep ==> [without].

vp_inf ==> [to],[please], np.
vp_inf ==> [to],[sleep].

rel_pronoun ==> [that].
rel_pronoun ==> [who].
rel_pronoun ==> [].

sentence(a,[the,cat,chased,the,dog]).
sentence(b,[the,cat,is,easy,to,please]).
sentence(c,[the,cat,is,eager,to,sleep]).
sentence(d,[the,cat,is,eager,to,please,the,dog,who,sleeps]).

