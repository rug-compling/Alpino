%%%%%%%%%%%%%%%%%%%%%%%% cf grammar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cf_rule(ra,np,[det,n]).
cf_rule(ra,n,[adj,n]).
cf_rule(la,n,[n,rel_s]).
cf_rule(la,n,[n,pp]).

cf_rule(ra,pp,[prep,np]).

cf_rule(rel,rel_s,[rel_pronoun,vsub]).
cf_rule(rel,rel_s,[rel_np,vsub]).
cf_rule(rel,rel_s,[rel_pp,vsub]).
cf_rule(ra,rel_np,[rel_det,n]).
cf_rule(ra,rel_np,[det,rel_n]).
cf_rule(la,rel_n,[n,rel_pp]).
cf_rule(ra,rel_pp,[prep,rel_np]).
cf_rule(ra,rel_pp,[prep,rel_pronoun]).




cf_rule(ra,vsub,[om,vsub]).
cf_rule(ra,vsub,[dat,vsub]).

cf_rule(la,vsub,[np,vsub]).
cf_rule(la,vsub,[adjunct,vsub]).
cf_rule(la,vsub,[pp,vsub]).

cf_rule(ra,vsub,[vc,vsub]).		% 1.
cf_rule(la,vsub,[prefix,vc]).
% cf_rule(ra,vsub,[v,vc]).		% 2. heeft [willen slapen]
%% regel 1. subsumeert regel 2, aangezien iedere v een vc is, en iedere
%% vc een vsub.
cf_rule(la,vsub,[v,vc]).		% gekust [wil hebben]

cf_rule(la,vc,[prefix,vc]).
cf_rule(ra,vc,[v,vc]).			% heeft [willen slapen]
cf_rule(la,vc,[v,vc]).			% gekust [wil hebben]


cf_rule(ra,v1,[v1,np]).
cf_rule(ra,v1,[v1,adjunct]).
cf_rule(ra,v1,[v1,pp]).
cf_rule(ra,v1,[v1,prefix]).
cf_rule(ra,v1,[v1,vsub]).		% iedere vc is ook vsub

cf_rule(que,que,[np,v1]).
cf_rule(que,que,[adjunct,v1]).
cf_rule(que,que,[vsub,v1]).
cf_rule(que,que,[pp,v1]).


%%%%%%%%%%%%%%  cf lexicon

cf_lex(Word,np) :- proper_name(Word,_).
cf_lex(Word,np) :- pronoun(Word,_,_,_). 

cf_lex(Word,rel_pronoun) :- rel_pronoun(Word,_,_). 

cf_lex(Word,n) :- noun([Word,_],_,_).
cf_lex(Word,n) :- noun([_,Word],_,_).

cf_lex(Word,adj) :- adj([Word,_],_).
cf_lex(Word,adj) :- adj([_,Word],_).

cf_lex(Word,det) :- det(Word,_,_,_,_).
cf_lex(Word,det) :- det([Word,_],_,_,_,_).
cf_lex(Word,det) :- det([_,Word],_,_,_,_).

cf_lex(Word,rel_det) :- rel_det(Word).



cf_lex(Word,prep) :- preposition(Word,_).

cf_lex(Word,prefix) :- prefix(Word).

cf_lex(Word,adjunct) :- adjunct(Word,_).

cf_lex(om,om).
cf_lex(dat,dat).

cf_lex(Word,v1) :- cf_verb(Word).
cf_lex(Word,vsub) :- cf_verb(Word).
cf_lex(Word,vc) :- cf_verb(Word).
cf_lex(Word,v) :- cf_verb(Word).

cf_verb(Word) :- verb([Word|_],_,_,_).
cf_verb(Word) :- verb([_,Word|_],_,_,_).
cf_verb(Word) :- verb([_,_,Word|_],_,_,_).
cf_verb(Word) :- verb([_,_,_,Word|_],_,_,_).
cf_verb(Word) :- verb([_,_,_,_,Word],_,_,_).
cf_verb([te,Word]) :- verb([_,_,Word,_],_,_,_).
cf_verb([te,Word]) :- verb([_,_,_,Word,_],_,_,_).


