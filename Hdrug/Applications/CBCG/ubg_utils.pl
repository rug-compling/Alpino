%%%%%%%%%%%%%%%%%%%%% Feature Unification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fvt_unify(A,A).

%%%% fvt unification macros

user_defined_eval(_/_).
user_defined_eval(_\_).

user_defined_eval(Term) :-
	\+ \+ temp(Term,_).

user_eval(MacroName,FS) :-
	temp(MacroName,FS).

user_eval(A/B,Sign) :-
	Sign:cat ==> nil,
	Sign:val <=> A,
	Sign:dir ==> right,
	Sign:arg <=> B.

user_eval(B\A,Sign) :-
	Sign:cat ==> nil,
	Sign:val <=> A,
	Sign:dir ==> left,
	Sign:arg <=> B.





%%%%%%%%%%%%%%%%%%%%%%%%%% defaulty stuff %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% add hoc solution for this grammar only
/*
unify_except(sign(Cat,Val,Dir,Arg,Sem,Mor,_,RP,Rel,Gap),
	     sign(Cat,Val,Dir,Arg,Sem,Mor,_,RP,Rel,Gap),[phrase]).
unify_except(sign(Cat,Val,Dir,Arg,Sem,Mor,P,_,Rel,Gap),
	     sign(Cat,Val,Dir,Arg,Sem,Mor,P,_,Rel,Gap),[rphrase]).
*/
	     
	
%%% lexical defaults 

apply_default(Default) :-		
	( call(Default), !
	; true
	).

	
