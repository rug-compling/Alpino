% first define a compiler from ALE terms to the kind of terms
% that library(p_feature), library(latex), library(p_tk) know
% about
%
% 
% these terms look like
% X/Y=Z where
%
% Integer/YesNo = Term
% Integer is some index
% YesNo if yes then it is an index which some other substructure refers to
% Term is a term, or special symbol 'R' when it should be skipped because
% already printed...
% Term is a list of a(Att,Val) terms, where Val is a term as described.


% there are three cases: value(X), clause(X), clause(X,Y).
% inequalities are moved into body of clause/2

clig_tree_user_node(Node) -->
	hdrug_clig:pp(Node).

latex_tree_user_node(Node) :-
	hdrug_latex:write_it_fs_latex(value(Node)).

tk_tree_user_node(Node,Frame) :-
	hdrug_tk:pp_tk(Node,Frame,'').

change_tree(Tree0,Tree) :-
	ale_duplicates(Tree0,[]),
	change_tree(Tree0,Tree,[],_),
	hdrug_util:prettyvars(Tree).

change_tree(tree(L0,_,Ds0),tree(L,_,Ds),V0,V) :-
	change(L0,L,V0,V1),
	change_ds(Ds0,Ds,V1,V).

change_ds([],[],V,V).
change_ds([H0|T0],[H|T],V0,V) :-
	change_tree(H0,H,V0,V1),
	change_ds(T0,T,V1,V).

ale_to_hdrug(Term,Iqs,Result) :-
	ale_duplicates(Term,Iqs),
	ale_to_hdrug_term(Term,Iqs,Result),
	hdrug_util:prettyvars(Result).   

ale_to_hdrug_term(fs(X),[],value(Y)) :-
	change(X,Y,[],_).

ale_to_hdrug_term(fs(X),[H|T],clause(Y,Iqs)) :-
	change_pretty_constraints([H|T],Iqs,[],Vis1),
	change(X,Y,Vis1,_Vis).

ale_to_hdrug_term(clause(X),[],clause(Y)) :-
	change_pretty_constraint(X,Y,[],_).

ale_to_hdrug_term(clause(X),[H|T],clause(Y,Body)) :-
	change_pretty_constraints([X,H|T],[Y|Body],[],_).

ale_to_hdrug_term(clause(H,B),Iqs,clause(NH,NB)) :-
	lists:append(B,Iqs,NB0),
	change_pretty_constraints([H|NB0],[NH|NB],[],_).

% interface to ale's duplicates predicate for arbitrary terms.
ale_duplicates(Term,Iqs) :-
	ale_duplicates_term(Term,[],DupsMid,[],VisMid,0,NumMid),
	duplicates_iqs(Iqs,DupsMid,_,VisMid,_,NumMid,_).

ale_duplicates_term(Var,D0,D,V0,V,N0,N) :-
	var(Var),!,
	D0=D, V0=V, N0=N.
% if it has the _-_ shape, then it's an Ale feature structure.
ale_duplicates_term(X-Y,D0,D,V0,V,N0,N) :-
	!,
	duplicates_fs(X-Y,D0,D,V0,V,N0,N).

ale_duplicates_term(Term,D0,D,V0,V,N0,N) :-
	functor(Term,_F,A),
	ale_duplicates_term(1,A,Term,D0,D,V0,V,N0,N).

ale_duplicates_term(Arg,Arity,_,D0,D,V0,V,N0,N) :-
    Arg > Arity,
    !, D0=D, V0=V, N0=N.
ale_duplicates_term(Arg,Arity,Term,D0,D,V0,V,N0,N) :-
    arg(Arg,Term,NTerm),
    ale_duplicates_term(NTerm,D0,D1,V0,V1,N0,N1),
    Arg2 is Arg+1,
    ale_duplicates_term(Arg2,Arity,Term,D1,D,V1,V,N1,N).

% change/4 for feature structures
% change_pretty_constraints/4 for lists of terms

% case 1: a variable
change(Var,Result,V0,V) :-
	var(Var),!,
	Var=Result,
	V0=V.

% case 2: an ALE feature structure
% same structure as pp_fs from ale, but does not write output,
% instead builds up a term of the specified format above
change(A-B,Ix/YN=Rest,VisIn,VisOut) :-
	!,   % it is an ALE feature structure
  deref_pp(A-B,Ref,SVs),
  ( var(Ref), YN=n                  % print ref if shared (nonvar)
  ; nonvar(Ref), %% write('['), write(Ref), write(']'),
    Ref=Ix, YN=y,
    ( member_eq(Ref,VisIn), !
    ; true         %%write(' ')
    )
  ),
  ( member_eq(Ref,VisIn), !, VisOut = VisIn, Rest='R'
  ; SVs = a_(X),                               %
    !,(no_write_type_flag(a_(X))               %
       ,!,Rest=[]                          %
      ;%%write(a_ X),                         %
          Rest=[a(type,[ X])]
      ),                                      %
    VisOut = [Ref|VisIn]                      %
  ; SVs =.. [Type|Vs],              % print FS if not already visited
    approps(Type,FRs),
    ( no_write_type_flag(Type),
      !, change_pp_vs(FRs,Rest1,Vs,[Ref|VisIn],VisOut),
      Rest=Rest1
    ; %%write(Type),
      Rest = [a(type,[Type])|Rest1],
      change_pp_vs(FRs,Rest1,Vs,[Ref|VisIn],VisOut) 
    )
  ).


% case 3: a list
change([],_/n=[a(type,[[]])],V0,V) :- !, V0=V.
change([H|T],_/n=[a(type,[.]),a(h,H2),a(t,T2)],V0,V) :-
	!,
	change(H,H2,V0,V1),
	change(T,T2,V1,V).

% case 4: anything else..
change(Term0,_/n=Term,V0,V) :-
	change_pretty_constraint(Term0,Term,V0,V).



change_pp_vs([],[],[],Vis,Vis).
change_pp_vs([F:_|FRs],Result,[V|Vs],VisIn,VisOut):-
  ( no_write_feat_flag(F),
    VisMid = VisIn, !,
    Result=Rest
  ; %%nl, tab(Col),
    %%write_feature(F,LengthF), 
    %%NewCol is Col + LengthF +1,
    change(V,AttVal,VisIn,VisMid),
    Result = [a(F,AttVal)|Rest]
  ),
  change_pp_vs(FRs,Rest,Vs,VisMid,VisOut).

change_pretty_constraints(Var,X,V0,V) :-
	var(Var),!,
	Var=X, V0=V.
change_pretty_constraints([],[],V,V).
change_pretty_constraints([ineq(A,B,C,D,E)|T],Result,V0,V):-
    !,
    change_ineqs(ineq(A,B,C,D,E),Result,Result0,V0,V1),
    change_pretty_constraints(T,Result0,V1,V).

change_pretty_constraints([H|T],[NH|NT],V0,V) :-
	change_pretty_constraint(H,NH,V0,V1),
	change_pretty_constraints(T,NT,V1,V).

change_pretty_constraint(Var,X,V0,V) :-
	var(Var),!,
	Var=X, V0=V.
change_pretty_constraint(_Module:H,NH,V0,V) :-
	!,
	change_pretty_constraint(H,NH,V0,V).

change_pretty_constraint(when(_,H),NH,V0,V) :-
	!,
	change_pretty_constraint(H,NH,V0,V).


change_pretty_constraint(H,NH,V0,V) :-
	H =.. [F|Args],
	change_pretty_arguments(Args,NArgs,V0,V),
	NH =.. [F|NArgs].

change_pretty_arguments([],[],V,V).
change_pretty_arguments([H|T],[NH|NT],V0,V):-
	change(H,NH,V0,V1),
	change_pretty_arguments(T,NT,V1,V).

change_ineqs(done,R,R,V,V).
change_ineqs(ineq(A,B,C,D,E),[ineq(L,R)|Result],Result0,V0,V):-
    change(A-B,L,V0,V1),
    change(C-D,R,V1,V2),
    change_ineqs(E,Result,Result0,V2,V).
