% rules
rule(node(s,s(NP,VP)), 
   [ node(np,NP),
     node(vp,VP)
   ]).
rule(node(vp,vp(V,NP)), 
   [ node(v,V),
     node(np,NP)
   ]).

% lexicon
lexicon(kisses,node(v,kisses)).
lexicon(kissed,node(v,kissed)).
lexicon(sleeps,node(vp,sleeps)).
lexicon(john,node(np,john)).
lexicon(mary,node(np,mary)).


