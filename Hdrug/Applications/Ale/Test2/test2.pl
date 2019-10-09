sign sub [s,np,vp,nbar,vbar,v,n,p,pp,det] intro [phon:(a_ _),
                                                 prob:(a_ _),
                                                 cont:(a_ _)].
:- op(1175,xfx,my_rule).
(
Name-RProb rule
(Mother,prob:(a_ MProb), phon:(a_ MPhon))
===>
PDtrs
) :-
  (Name my_rule Mother ===> Dtrs),
  prob(Name,RProb),  % if this were in the hook itself, we could change the
                     %  probabilities of the rules on-line.
  build_pdtrs(Dtrs,PDtrs,MProb,RProb,Phons,Phons,MPhon). 
                         % add hooks to calculate MProb and append phonologies.

prob(s_np_vp,1).
prob(np_det_nbar,1).
prob(vp_vbar,0.5).
prob(vp_vbar_pp,0.5).
prob(vbar_v_np,1).
prob(nbar_n,0.9).
prob(nbar_n_pp,0.1).
prob(pp_p_np,1).

s_np_vp my_rule
(s,cont:(a_ SCont))
===>
cat> (np,cont:(a_ NPCont)),
cat> (vp,cont:(a_ lam(NPCont,SCont))).

vbar_v_np my_rule
(vbar, cont:(a_ VBarCont))
===>
cat> (v, cont:(a_ lam(NPCont,VBarCont))),
cat> (np, cont:(a_ NPCont)).

pp_p_np my_rule
(pp, cont:(a_ PPCont))
===>
cat> (p, cont:(a_ lam(NPCont,PPCont))),
cat> (np, cont:(a_ NPCont)).


np_det_nbar my_rule
(np, cont:(a_ NPCont))
===>
cat> (det, cont:(a_ lam(NProp,NPCont))),
cat> (nbar, cont:(a_ NProp)).

vp_vbar my_rule
(vp, cont:VBarCont)
===>
cat> (vbar, cont:VBarCont).

nbar_n my_rule
(nbar, cont:NCont)
===>
cat> (n, cont:NCont).

vp_vbar_pp my_rule
(vp, cont:(a_ VPCont))
===>
cat> (vbar, cont:(a_ VBarCont)),
cat> (pp, cont:(a_ lam(VBarCont,VPCont))),
goal> prolog((format("verb attachment fired!",[]),nl)).

nbar_n_pp my_rule
(nbar, cont:(a_ NBarCont))
===>
cat> (n, cont:(a_ NCont)),
cat> (pp, cont:(a_ lam(NCont,NBarCont))),
goal> prolog((format("noun attachment fired!",[]),nl)).


:-op(1150,xfx,my_lex).
(
Word ---> (Desc,prob:(a_ 1),phon:(a_ [Word]))
) :-
  Word my_lex Desc.

telescope my_lex (n, cont:(a_ lam(E,telescope(E)))).
boy my_lex (n, cont:(a_ lam(E,boy(E)))).
dog my_lex (n, cont:(a_ lam(E,dog(E)))).
sees my_lex (v, cont:(a_ lam(Int,lam(Ext,see(Int,Ext))))).
the my_lex (det, cont:(a_ lam(lam(E,Prop),the(E,Prop)))).
with my_lex (p, cont:(a_ lam(Pred,lam(lam(X,Prop),lam(X,with(Pred,Prop)))))).


% build_pdtrs(+OldDtrs,-NewDtrs,+MotherProbability,+ProbAccum,+Phonologies,
%             +PhonDifference,+MotherPhon):
% Automatically add hooks to rules for probability calculation and phonology 
%  combination

build_pdtrs((Dtr,Dtrs),(PDtr,PDtrs),MProb,Probs,Phons,PhonsMid,MPhon) :-
  !,build_pdtr(Dtr,PDtr,Prob,PhonsMid,PhonsRest),
  build_pdtrs(Dtrs,PDtrs,MProb,(Prob * Probs),Phons,PhonsRest,MPhon).
build_pdtrs(Dtr,(PDtr,
                 goal> prolog(MProb is Prob * Probs),
                 goal> prolog(conc_list(Phons,MPhon))),MProb,Probs,
            Phons,PhonsMid,MPhon) :-
  build_pdtr(Dtr,PDtr,Prob,PhonsMid,[]).

% conc_list(+,-):  concatenate list of lists

conc_list([],[]).
conc_list([L|Ls],Result) :-
  conc_list_act(L,Ls,Result).

conc_list_act([],Ls,Result) :-
  conc_list(Ls,Result).
conc_list_act([X|Xs],Ls,[X|Result]) :-
  conc_list_act(Xs,Ls,Result).

build_pdtr(cat> Desc,cat> (Desc,prob:(a_ Prob),phon:(a_ Phon)),
           Prob,[Phon|PhonsRest],PhonsRest).
build_pdtr(goal> Goal,goal> Goal,1,PhonsRest,PhonsRest).

