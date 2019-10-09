%%%%%%%%%%%%%%%%%%%%%%%%%%%%  The Grammar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%  headed grammar rules   %%%%%%%%%%%%%%%%%%%%%%
	
rule(ra, Val, [ Fun, Arg ], 1) :-		% rightward application
	Fun <=> Val/Arg,
	apply_principles(Val,Fun,Arg).
	
rule(la, Val, [ Arg, Fun ], 2) :-		% leftward application
	Fun <=> Arg\Val,
	apply_principles(Val,Fun,Arg).
	
rule(que, Que, [ Filler, Sent ], 2) :-		% topicalization, wh-questions
	Sent <=> s,
	Sent <=> vfirst,
	Sent:gap <=> Filler,		
	Que:gap  ==> empty,		% wh-clauses are extraction islands
	Que <=> s,
	Que <=> vsecond,
	Que:sem <=> Sent:sem,
	Filler:gap ==> empty,
	Filler:rel ==> empty.		% exclude rel. pronouns
	
rule(rel, RelS, [ RelPro, S ], 1) :-		% relative clause formation
	S <=> s,
	S <=> vfinal,
	S:gap <=> RelPro,
	RelS <=> n(_)\n(_),		
	RelS:gap ==> empty,		% relatives are extraction islands
	RelS:mor <=> RelPro:rel:rmor,	
	RelS:mor <=> RelS:arg:mor,
	RelS:sem ==> X^and(NSem,Ssem),
	RelS:arg:sem ==> X^NSem,
	S:sem <=> Ssem,
	RelPro:rel:index <=> X.
	
%%%%% grammar principles

apply_principles(Val,Fun,Arg) :-
	Fun:mor <=> Val:mor,
	Fun:sem <=> Val:sem,
	Val:phrase <=> Arg:rphrase,	
	Fun:phrase <=> Arg:phrase,	% * marie wil op verzuimen te bellen
	non_local(rel,Fun,Arg,Val),
	non_local(gap,Fun,Arg,Val).	% nb gap is not a list-valued feature
					% so, no multiple extractions
					
non_local(rel,Dghtr1,Dghtr2,Mthr) :-	% percolation of non-local,
	Dghtr1:rel <=> D1,		% `sign-valued', features
	Dghtr2:rel <=> D2,
	Mthr:rel <=> M,
	non_local(D1,D2,M).

non_local(gap,Dghtr1,Dghtr2,Mthr) :-    % percolation of non-local,
        Dghtr1:gap <=> D1,              % `sign-valued', features
        Dghtr2:gap <=> D2,
        Mthr:gap <=> M,
        non_local(D1,D2,M).

:- block non_local(-,-,?).		% delay evaluation of disjunction 

non_local(Dghtr1,Dghtr2,Mthr) :-		
	( nonvar(Dghtr1) -> 			 
	  ( Dghtr1 = empty -> Dghtr2 <=> Mthr
	  ; Dghtr2 = empty,   Dghtr1 <=> Mthr
	  )
	; Dghtr2 = empty -> Dghtr1 <=> Mthr
	; Dghtr1 = empty,   Dghtr2 <=> Mthr
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%% templates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% syntax

temp(basic(Cat),X) :-
	X:cat ==> Cat,			% distinguish basic and complex
	X:arg ==> nil,			% properly (for delayed stuff)
	X:val ==> nil.
	
temp(np(Case),X) :-
	X <=> basic(np),
	% X:rphrase <=> yes,
	X:mor:case ==> Case.
	
temp(pn,X) :-
	X <=> np(_),
	X:mor:agr <=> sg(3).
	
temp(pronoun(Agr,Case),X) :-
	X <=> np(Case),
	X:mor:agr <=> Agr.

temp(n(Det),X) :-
	X <=> basic(n),
	X:mor:det <=> Det.
		
temp(adj(Agr,Det,Def),X) :-
	X <=> n(_)/n(_),
	X:mor <=> X:arg:mor,
	X:mor:det <=> Det,
	X:mor:def <=> Def,
	X:mor:agr <=> Agr.
			
temp(det(Agr,Det,Def),X) :-
	X <=> np(_)/n(Det),
	X:mor:agr <=> Agr,
	X:arg:mor:agr <=> Agr,
	X:arg:mor:def <=> Def.
	
temp(s,X) :-
	X <=> basic(sent).
	
temp(iv,X) :-
	X <=> np(nom)\s,
	X <=> rphrasal_arg.
	
temp(tv,X) :-
	X <=> np(acc)\iv,
	X <=> rphrasal_arg.

temp(dtv1,X) :-				% marie het boek geven
	X <=> np(acc)\tv,
	X <=> rphrasal_arg.
temp(dtv2,X) :-				% het boek aan marie geven
	X <=> pp(aan)\tv,
	X <=> rphrasal_arg.
	
temp(tvp(PrepFrm),X) :-
	X <=> pp(PrepFrm)\iv,
	X <=> rphrasal_arg.
	
temp(tv_pref(PrefFrm),X) :-
	X <=> prefix(PrefFrm)\tv.

temp(modal,X) :-
	( X <=> iv/iv
	; X <=> iv\iv,		% modal inversion
	  X <=> finite,		% for finite modals only
	  X:arg <=> no_ipp
	),
	X:arg <=> infinitive.
	
temp(s_raising,X) :-
	X <=> iv/iv,
	X:arg <=> no_om.		% te_infinitive without om
	
temp(perc_verb,X) :-
	X <=> tv/iv,
	X:arg <=> infinitive.
	
temp(perf_aux(AuxFrm),X) :-
	( X <=> iv/iv
	; X <=> iv\iv,		% account for participle inversion
	  X:arg <=> no_ipp
	),
	X:arg <=> participle(AuxFrm).

temp(extra_verb(Om),X) :-
	X <=> iv/iv,
	X:arg <=> te_infinitive,
	X:arg <=> Om,
	X <=> rphrasal_arg.
	
temp(trans_extra_verb(Om),X) :-
	X <=> tv/iv,
	X:arg <=> te_infinitive,
	X:arg <=> Om,
	X <=> rphrasal_arg.
	
temp(s_compl_verb,X) :-
	X <=> iv/s,
	X:arg <=> subordinate,
	X <=> rphrasal_arg.
	
	 
temp(pp(PrepFrm),X) :-
	X <=> basic(pp),
	X:mor:form ==> PrepFrm.
		
temp(prep1(PrepFrm),X) :-
	X <=> pp(PrepFrm)/np(acc).
	
temp(prep2,X) :-
	X <=> (n(_)\n(_))/np(acc),
	X:mor <=> X:val:arg:mor.
			
temp(rel_pro(Agr,Det),X) :-
	X <=> np(_),
	X:mor:agr <=> Agr,
	X:rel:rmor:agr <=> Agr,
	X:rel:rmor:det <=> Det.
	
temp(rel_det,X) :-
	X <=> det(_Agr,_Det,def).
	
temp(prefix(Prefix),X) :-
	X <=> basic(pref),
	X <=> nonphrasal,
	X:mor:form <=> Prefix.

temp(adjunct,X) :-
	X <=> basic(adj).
	
%%%% semantics	(mixing fvt and prolog terms ...)

temp(pn_sem(Name),S) :-
	S:sem ==> (Name^Prop)^Prop.
		
temp(n_sem(Name),S) :-
	S:sem ==> X^Prop,
	Prop =.. [Name,X].
			
temp(adj_sem(Name),S) :-
	S:sem ==> Var^and(NSem,Asem),
	S:arg:sem ==> Var^NSem,
	Asem =.. [Name,Var].
		
temp(det_sem(Name),S) :-
	S:sem ==> (X^Scope)^Formula,
	S:arg:sem ==> X^Restriction,
	Formula =.. [Name,X,Restriction,Scope].
		
temp(iv_sem(Name),S) :-
	S:sem <=> Sem,
	S:arg:sem ==> (X^Pred)^Sem,
	Pred =.. [Name,X].
	
temp(tv_sem(Name),S) :-
	S:sem <=> Sem,
  	S:arg:sem ==> (Y^Pred)^Sem0,
	S:val:arg:sem ==> (X^Sem0)^Sem,
	Pred =.. [Name,X,Y].

temp(dtv1_sem(Name),S) :-
	S:sem <=> Sem,
  	S:arg:sem ==> (Z^Pred)^Sem0,
	S:val:arg:sem ==> (Y^Sem0)^Sem1,
	S:val:val:arg:sem ==> (X^Sem1)^Sem,
	Pred =.. [Name,X,Y,Z].
	
temp(dtv2_sem(Name),S) :-
	S:sem <=> Sem,
  	S:arg:sem ==> (Z^Pred)^Sem0,
	S:val:arg:sem ==> (Y^Sem0)^Sem1,
	S:val:val:arg:sem ==> (X^Sem1)^Sem,
	Pred =.. [Name,X,Z,Y].
	
temp(tv_pref_sem(Name),S) :-
	S:sem <=> Sem,
  	S:val:arg:sem ==> (Y^Pred)^Sem0,
	S:val:val:arg:sem ==> (X^Sem0)^Sem,
	Pred =.. [Name,X,Y].
	
temp(modal_sem(Name),S) :-
	S:sem <=> Sem,
	S:arg:arg:sem ==> (X^VPsem)^VPsem,
	S:arg:sem <=> ArgSem,
	Form =.. [Name,X,ArgSem],
	S:val:arg:sem ==> (X^Form)^Sem.
	
temp(s_raising_sem(Name),S) :-
	S:sem <=> Sem,
	Sem =.. [Name,Ssem],
	S:arg:arg:sem ==> (X^VPsem)^VPsem,
	S:arg:sem <=> ArgSem,
	S:val:arg:sem ==> (X^ArgSem)^Ssem.
	
temp(perc_verb_sem(Name),S) :-
	S:sem <=> SubjSem,
	S:arg:arg:sem ==> (X^VPsem)^VPsem,
	S:arg:sem <=> ArgSem,
	Sem =.. [Name,Y,ArgSem],
	S:val:arg:sem ==> (X^Sem)^ObjSem,		% object wide scope 
	S:val:val:arg:sem ==> (Y^ObjSem)^SubjSem.	% wrt perc verb ...

temp(trans_extra_verb_sem(Name),S) :-
	S:sem <=> SubjSem,
	S:arg:arg:sem ==> (X^VPsem)^VPsem,
	S:arg:sem <=> ArgSem,
	Sem =.. [Name,Y,X,ArgSem],
	S:val:arg:sem ==> (X^Sem)^ObjSem,		% object wide scope 
	S:val:val:arg:sem ==> (Y^ObjSem)^SubjSem.	% wrt perc verb ...

temp(s_compl_sem(Name),S) :-
	S:sem <=> SubjSem,
	S:arg:sem <=> ComplSem,
	Sem =.. [Name,X,ComplSem],
	S:val:arg:sem ==> (X^Sem)^SubjSem.
	
temp(prep1_sem,S) :-
	S:sem <=> S:arg:sem.
	
temp(prep2_sem(Name),S)  :-
	S:sem ==> X^and(Nsem,Formula),
	S:arg:sem ==> (Y^Scope)^Formula,
	S:val:arg:sem ==> X^Nsem,
	Scope =..[Name,Y,X].
	
temp(rel_pro_sem,S) :-
	S:sem ==> (Index^Prop)^Prop,
	S:rel:index <=> Index.	
		
temp(rel_det_sem,S) :-
	S:sem ==> (X^Scope)^unique(X,and(Restriction,poss(Index,X)),Scope),
	S:arg:sem ==> X^Restriction,	
	S:rel:index <=> Index.
	
temp(adjunct_sem(Name),S) :-
	S:sem ==> X^Form,
	Form =..[Name,X].
	
%%%%% verbal subcategorization types (combine syntax and semantics) %%%%%%%%

temp(subcat(iv,Sem,nil),Sign) :-
	Sign <=> iv,
	Sign <=> iv_sem(Sem).
temp(subcat(tv,Sem,nil),Sign) :-
	Sign <=> tv,
	Sign <=> tv_sem(Sem).
temp(subcat(dtv,Sem,nil),Sign) :-
	Sign <=> dtv1,
	Sign <=> dtv1_sem(Sem).
temp(subcat(dtv,Sem,nil),Sign) :-
	Sign <=> dtv2,
	Sign <=> dtv2_sem(Sem).
temp(subcat(tvp(PrpFrm),Sem,nil),Sign) :-
	Sign <=> tvp(PrpFrm),
	Sign <=> tv_sem(Sem).
temp(subcat(tv_pref(PrefFrm),Sem,nil),Sign) :-
	Sign <=> tv_pref(PrefFrm),
	Sign <=> tv_pref_sem(Sem).
temp(subcat(modal,Sem,vr),Sign) :-
	Sign <=> modal,
	Sign <=> modal_sem(Sem).
temp(subcat(modal_te,Sem,vr),Sign) :-
	Sign <=> s_raising,
	Sign <=> modal_sem(Sem).
temp(subcat(s_raising,Sem,vr),Sign) :-
	Sign <=> s_raising,
	Sign <=> s_raising_sem(Sem).
temp(subcat(perc_verb,Sem,vr),Sign) :-
	Sign <=> perc_verb,
	Sign <=> perc_verb_sem(Sem).
temp(subcat(perf_aux(AuxFrm),Sem,vr),Sign) :-
	Sign <=> perf_aux(AuxFrm),
	Sign <=> s_raising_sem(Sem).
temp(subcat(extra_verb(Om),Sem,nil),Sign) :-
	Sign <=> extra_verb(Om),
	Sign <=> modal_sem(Sem).
temp(subcat(trans_extra_verb(Om),Sem,nil),Sign) :-
	Sign <=> trans_extra_verb(Om),
	Sign <=> trans_extra_verb_sem(Sem).
temp(subcat(p_extra_verb,Sem,p_extra),Sign) :-
	Sign <=> extra_verb(no_om),
	Sign <=> modal_sem(Sem).	
temp(subcat(s_compl_verb,Sem,nil),Sign) :-
	Sign <=> s_compl_verb,
	Sign <=> s_compl_sem(Sem).
	
	
%%%% morphological stuff

temp(finite,X) :-
	X:mor:form ==> fin(_).
temp(vfirst,X) :-
	X:mor:form ==> fin(1).
temp(vsecond,X) :-
	X:mor:form ==> fin(2).
temp(vfinal,X) :-
	X:mor:form ==> fin(3).
temp(subordinate,X) :-
	X:mor:form ==> fin(sub).
	
temp(infinitive,X) :-				% needed for ipp effect 
	X:mor:form ==> nonfin(inf,no,_,_).
temp(participle(Frm),X) :-
	X:mor:form ==> nonfin(no,prt,Frm,_).
temp(inf_or_part(Frm),X) :-			% infinitival form of VR verb
	X:mor:form ==> nonfin(_,_,Frm,ipp).	% must unify with Prt as well
temp(no_ipp,X) :-				
	X:mor:form ==> nonfin(_,_,_,no_ipp).	
	
temp(te_infinitive,X) :-
	X:mor:form ==> te(_).
temp(om,X) :-
	X:mor:form ==> te(om).
temp(no_om,X) :-
	X:mor:form ==> te(no_om).

	
temp(verbal_morphology(fin(Agr)),X) :-
	X <=> finite,
	sv_agreement(Agr,X).		% ensure X is instantiated
temp(verbal_morphology(non_fin(Frm)),X) :-
	X <=> Frm.
	
temp(singular,X) :-
	X:mor:agr ==> sg(3).
temp(plural,X) :-
	X:mor:agr ==> pl(3).

temp(phrasal,X) :-
	X:phrase ==> yes.
temp(nonphrasal,X) :-
	X:phrase ==> no.
temp(rphrasal_arg,X) :-
	X:arg:rphrase ==> yes.


%%%%%%%%%%% recursive constraints and lexical rules %%%%%%%%%%%%%%%%%%%%%%
		
sv_agreement(Agr,X) :-
	X:arg <=> Arg,
	sv_agreement(Agr,X,Arg).
	
:- block sv_agreement(?,?,-).

sv_agreement(Agr,X,_) :-		% agreement as a recursive constraint
	X <=> np(nom)\s, !,		
	X:arg:mor:agr <=> Agr.
sv_agreement(Agr,X,_) :-
	X:val <=> Val,
	sv_agreement(Agr,Val).
	
	
verb_position(In,Out) :-		% do nothing 
	In <=> Out,
	apply_default(Out <=> vfinal).	% and if Out is finite, it is vfinal
verb_position(In,Out) :-		% apply verb_first lexical rule
	In <=> finite,			
	In:mor <=> Out:mor,		
	In:gap <=> Out:gap,
	In:sem <=> Out:sem,
	Out <=> vfirst,
	S <=> s,
	verb_first(In,S,Out,_).
	
verb_first(In,Sofar,Out,Dir) :-	
	Out:arg <=> Arg,
	verb_first(In,Sofar,Out,Dir,Arg).
	
:- block verb_first(?,?,?,?,-).

verb_first(In,Mid,Out,_,_) :-		% verb first as a lexical rule	 
	In <=> Mid,			% Out is the verb first version of In
	Out <=> s.
verb_first(In,Sofar,Out,left,_) :-
	Out <=> Val/Arg0,
	Sofar0 <=> Arg1\Sofar,
	unify_except(Arg0,Arg1,rphrase),	% hack, needed for
	verb_first(In,Sofar0,Val,_).		% belt jan marie[+rp] op[-p]
verb_first(In,Mid,Out,right,_) :-	% handle vfinal verbs selecting 
	In <=> In0/Arg0,		% arguments to their right
	Out <=> Out0/Arg1,
	unify_except(Arg0,Arg1,rphrase),
	verb_first(In0,Mid,Out0,right).
		
gap_intro_lr(In,Out):-			% gap introduction (aka push_to_slash)
	In <=> Out.			% as a (optional) lexical rule
gap_intro_lr(In,Out) :-			
	In:mor <=> Out:mor,
	In:sem <=> Out:sem,
	Out:gap <=> Gap,
	gap_intro(In,Out,Gap).		
	
gap_intro(In,Out,Gap) :-
	Out:arg <=> OutArg,
	In:arg <=> InArg,
	Gap:cat <=> _Any,		% used to trigger non_local/3
	gap_intro(In,Out,Gap,InArg,OutArg).
	
:- block gap_intro(?,?,?,-,-).		% block only if both in and out Arg
					% are unknown (to avoid deadlock if
gap_intro(In,Out,Gap,_,_) :-		% a verb is selected by a vraiser)	
	In:arg <=> Arg,			
	unify_except(Arg,Gap,phrase),	% remove phrase feature from gap
	In:val <=> Out.			% to get (partial) vp topicalization
gap_intro(In,Out,Gap,_,_) :-
	In:arg <=> Out:arg,
	In:dir <=> Out:dir,
	In:cat <=> Out:cat,
	In:val <=> InVal,
	Out:val <=> OutVal,
	gap_intro(InVal,OutVal,Gap).
	
verb_raise(In,Out,nil) :-		% not a v-raising verb
	In <=> Out.
verb_raise(In,Out,Type) :-
	In:mor <=> Out:mor,
	In:sem <=> Out:sem,
	In:gap <=> Out:gap,
	In:arg:mor <=> Out:arg:mor,
	( Type = vr, 		
	  Out:arg <=> nonphrasal
	; Type = p_extra, 	
	  Out:arg:rphrase <=> yes
	),
	( In:dir <=> left -> Out <=> rphrasal_arg	% inversion cases
	; true
	),
	division(In,Out).
	
division(In,Out) :-
	Out:val:arg <=> Arg,
	division(In,Out,Arg).
	
:- block division(?,?,-).

division(In,Out,_) :-
	In <=> Out.
division(In,Out,_) :-				% four versions in one
	Out:val:val <=> Mid:val,
	Out:arg:val <=> Mid:arg,
	Out:dir <=> Mid:dir,
	Out:arg:arg <=> Out:val:arg,
	Out:arg:dir <=> Out:val:dir,
	Out:cat ==> nil,
	Out:val:cat ==> nil,
	Out:arg:cat ==> nil,
	Out:arg:dir ==> Dir1,
	Out:dir <=> Dir2,
	Out:arg:arg <=> Arg,
	check_harmonic(Dir1,Dir2,Arg),
	Out:arg:sem <=> Out:arg:val:sem,	% do raising verbs properly
	division(In,Mid).
	
:- block check_harmonic(-,?,?), check_harmonic(?,-,?).

check_harmonic(Dir1,Dir2,Arg) :-		% harmonic division implies
	( Dir1 = Dir2 -> Arg <=> phrasal	% argument is a non VR verb
	; true					% (avoids sp. ambiguities)
	).

add_adjuncts_lr(In,Out) :-
	In:gap <=> Out:gap,
	In:mor <=> Out:mor,
	In:sem <=> InSem,
	Out:sem <=> OutSem,			
	add_adjuncts(In,Out,InSem,OutSem).
	
add_adjuncts(In,Out,InSem,OutSem) :-
	Out:arg <=> Arg,
	add_adjuncts(In,Out,InSem,OutSem,Arg).
	
:- block add_adjuncts(?,?,?,?,-).
	
add_adjuncts(In,Out,Sem,Sem,_) :-
	In <=> Out,
	In <=> s.
add_adjuncts(In,Out,InSem,OutSem,_) :-		
	Out <=> Adj\OutVal,			
	Adj <=> adjunct,			% adjuncts take scope over
	Out <=> rphrasal_arg,			% arguments, with the first 
	Adj:sem <=> InSem^ResultSem,		% adjunct taking widest scope
	add_adjuncts(In,OutVal,ResultSem,OutSem).	
add_adjuncts(In,Out,InSem,OutSem,_) :-
	Out:arg <=> In:arg,
	Out:dir <=> In:dir,
	Out:val <=> OutVal,
	In:val <=> InVal,
	add_adjuncts(InVal,OutVal,InSem,OutSem).
	
%%%%%%%%%%%%%%%%%%%%%%%%% lexicon interface %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% apply lexical defaults

lexic(Word,Sign)	:-
	lex0(Word,Sign),
	apply_default(Sign:gap ==> empty),
	apply_default(Sign:rel ==> empty).

%%%% word class specific interface:
%%%% handle morphology, combine syntax and semantics

lex0(Word,Sign) :-
	proper_name(Word,Sem),
	Sign <=> pn,
	Sign <=> pn_sem(Sem).
	
lex0(Word,Sign) :-
	pronoun(Word,Agr,Case,Sem),
	Sign <=> pronoun(Agr,Case),
	Sign <=> pn_sem(Sem).
	
lex0(Word,Sign) :-
	( noun([Word,_],Det,Sem),
	  Sign <=> singular
	; noun([_,Word],Det,Sem),
	  Sign <=> plural
	),
	Sign <=> n(Det),
	Sign <=> n_sem(Sem).
	
lex0(Word,Sign) :-
	( adj([Word,_],Sem),
	  Sign <=> adj(sg(3),het,indef)
	; adj([_,Word],Sem),
	  ( Sign <=> adj(pl(3),_,_)
	  ; Sign <=> adj(sg(3),de,_)
	  ; Sign <=> adj(sg(3),het,def)
	  )
	),
	Sign <=> adj_sem(Sem).
	
lex0(Word,Sign) :-
	( det(Word,Agr,Det,Def,Sem),
	  Sign <=> det(Agr,Det,Def)
	; det([Word,_],Sem),
	  Sign <=> det(sg(3),het,indef)
	; det([_,Word],Sem),
	  ( Sign <=> det(pl(3),_,_)
	  ; Sign <=> det(sg(3),de,_)
	  )
	),
	Sign <=> det_sem(Sem).
	
lex0(Word,Sign) :-
	verb0(Word,Syn,Sem,Morphology),
	Sign0 <=> subcat(Syn,Sem,VerbType),
	Sign0 <=> verbal_morphology(Morphology),
	verb_raise(Sign0,Sign1,VerbType),	
	add_adjuncts_lr(Sign1,Sign2),
	gap_intro_lr(Sign2,Sign3),
	verb_position(Sign3,Sign).
	
verb0(Word,Syn,Sem,fin(sg1)) :-
	verb([Word|_],Syn,_,Sem).
verb0(Word,Syn,Sem,fin(sg(_))) :-		% 2nd or 3rd person singular
	verb([_,Word,_,_],Syn,_,Sem).
verb0(Word,Syn,Sem,fin(sg(2))) :-		% 2nd person (hebt, bent)
	verb([_,Word,_,_,_],Syn,_,Sem).
verb0(Word,Syn,Sem,fin(sg(3))) :-		% 3nd person (heeft, is)
	verb([_,_,Word,_,_],Syn,_,Sem).
verb0(Word,Syn,Sem,fin(pl(_))) :- 
	( verb([_,_,Word,_],Syn,_,Sem)
	; verb([_,_,_,Word,_],Syn,_,Sem)
	).
verb0(Word,Syn,Sem,Morphology) :- 	
	( verb([_,_,Inf,Prt],Syn,PartFrm,Sem)	
	; verb([_,_,_,Inf,Prt],Syn,PartFrm,Sem)
	), 
	(  Inf = Prt 					% ipp verbs
	-> Word = Inf,
	   Morphology = non_fin(inf_or_part(PartFrm))	
	;  Word = Inf,					% others
	   Morphology = non_fin(infinitive)
	;  Word = Prt,
	   Morphology = non_fin(participle(PartFrm))
	).
	
verb0([te,Word],Syn,Sem,non_fin(te_infinitive)) :- 
	( verb([_,_,Word,_],Syn,_,Sem)
	; verb([_,_,_,Word,_],Syn,_,Sem)
	).

	
lex0(Word,Sign) :-
	preposition(Word,Sem),
	( Sign <=> prep1(Word),
	  Sign <=> prep1_sem
	; Sign <=> prep2,
	  Sign <=> prep2_sem(Sem)
	). 
	
lex0(Word,Sign) :-
	rel_pronoun(Word,Num,Det),
	Sign <=> rel_pro(Num,Det),
	Sign <=> rel_pro_sem.

lex0(Word,Sign) :-
	rel_det(Word),
	Sign <=> rel_det,
	Sign <=> rel_det_sem.

lex0(Word,Sign) :-
	prefix(Word),
	Sign <=> prefix(Word).

lex0(om,Sign) :-
	Sign <=> iv/iv,
	Sign:arg <=> no_om,
	Sign:val <=> om,
	Sign:arg:arg <=> Sign:val:arg,		% `subject raising' semantics
	Sign:arg:sem <=> Sign:sem.		
	
lex0(dat,Sign) :-
	Sign <=> s/s,
	Sign:arg <=> vfinal,
	Sign:val <=> subordinate,
	Sign:arg:sem <=> Sign:sem.		
	
lex0(Word,Sign) :-
	adjunct(Word,Sem),			
	Sign <=> adjunct,
	Sign <=> adjunct_sem(Sem).	
	
:- ensure_loaded(cf_grammar).
