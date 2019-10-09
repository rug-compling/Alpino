top([sign,part,sbar,vp,np,n,adj,adv,pp,sem,prime(_),rule,
     +,-,lexical,phrasal,mod]).

extensional(X) :-
	member(X,[part,sbar,n,adv,pp,prime(_),+,-,lexical,phrasal]).


list_type(h,t).

type(mod,[],[arg,val]).

type(sign,[],[sc,sem,cat,lex,inv,fargs,slash,extra,rel,
              f_slash,f_extra,f_rel,sep,dir,tree,args,mod]).

at(part).
at(sbar).

type(vp,[],[ipp,vform]).
type(np,[],[case]).
at(n).
type(adj,[att,pred],[]).
  at(att).
  at(pred).
at(adv).
at(pp).

type(sem,[ix,cx],[index]).
  at(ix).
  type(cx,[c0,cxx],[fun,restr]).
    type(cxx,[c1,cxxx],[arg1]).
      type(cxxx,[c2,c3],[arg2]).
        type(c3,[],[arg3]).
    at(c0). 
      at(c1). 
        at(c2). 

at(prime(_)). 

type(rule,[apply,topicalize,extrapose,cliticize,verbfront],[mt,ls,hd,rs]).
  at(apply). 
  at(topicalize). 
  at(extrapose). 
  at(cliticize). 
  at(verbfront).

at(+). 

at(-). 

type(lexical,[],[stem,word]).

at(phrasal).

catch_print_error(_,V,_):-
   write(V).

boolean_type(vform,[[fin,inf,te,pas,om,of,dat]]).
boolean_type(dir,[[left,right,extra,topic]]).
boolean_type(case,[[nom,obj,obl]]).
boolean_type(mor,[[mass,sg,pl],[nneut,neut],[def,indef],[fem,masc,nhuman]]).

intensional(mor).





:- type_compiler.

