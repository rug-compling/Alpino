:- module(database,[assertz_if_not/1, 
                    assertz_if_not/2,
                    assertz_most_general/1,
		    assertz_most_general/2,
		    assertz_most_general/3,
		    clean_up_database/1,
		    assertz_if_necc/1,
		    assertz_opt/1]).

% DATABASE.PL

% assertz_most_general(Pred)
% assertz_most_general(Pred,Bool)
%     assertz Pred if no more general version exists yet, and deletes all
%     more specific versions..
%     Bool = yes/no if a more general one did/did not exist
% assertz_if_not(Pred)
% assertz_if_not(Pred,Bool)
%     as before, but does NOT delete less general versions!

add_module(Pred0,M:Pred) :-
	( Pred0 = M:Pred -> true ; Pred0 = Pred, M=user ).

assertz_if_not(Pred):-
	add_module(Pred,Pred2),
	assertz_most_general(Pred2,_,nodelete).

assertz_if_not(Pred,Bool):-
	add_module(Pred,Pred2),
	assertz_most_general(Pred2,Bool,nodelete).

assertz_most_general(Pred):-
	add_module(Pred,Pred2),
	assertz_most_general(Pred2,_,delete).

assertz_most_general(Pred,Bool):-
	add_module(Pred,Pred2),
	assertz_most_general(Pred2,Bool,delete).

assertz_most_general(Module:Pred,Bool,Del):-
	copy_term(Pred,Copy),
	numbervars(Copy,0,_),
	assertz_most_general(Module,Pred,Copy,Bool,Del).

assertz_most_general(Module,_Pred,Copy,Bool,_Del):-
	Module:Copy,
	!,
	Bool = yes.  
assertz_most_general(Module,Pred,Copy,no,delete):-
	delete_more_specific(Module:Pred),
	assertz(Module:Pred,Ref),
	recordz(Pred,t(Module,Copy,Ref),_).   % assert a copy + reference to original
                                    % for later subsumption check in delete_
                                    % more specific!!

assertz_most_general(Module,Pred,_Copy,no,nodelete):-
	assertz(Module:Pred).

delete_more_specific(Module:Pred):-
	( recorded(Pred,t(Module,Pred,Ref),Ref2),  % find frozen copy & ref of more
                                         % specific one
	  erase(Ref),                    % erase it
	  erase(Ref2),                    % and erase its copy
	  fail
        ; true).

%% this is tricky..
clean_up_database(Pred) :-
	add_module(Pred,M:Pred2),
	( recorded(Pred2,t(M,_,_),Ref),
          erase(Ref),
	  fail
        ; true),
	retractall(M:Pred2).
%	functor(Pred2,F,A),
%	abolish(M:(F/A)).



%%%%%%%%%%%% assertz_if_necc %%%%%%%%%%%%%%%%%%%
% no complicated stuff, but plain unification:
% i.e. only sound for ground relations

assertz_if_necc(X):-
	add_module(X,Y),
	(  Y
        -> fail
        ;  assertz(Y)
        ).

assertz_opt(X):-
	add_module(X,Y),
	(  Y 
        -> true
        ;  assertz(Y)
        ).



