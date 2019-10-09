:- version('Grammar for Generation, adapted from Shieber et al. 1990.').

semantics(ale(A,B,C,_),Sem) :-
	add_to(sem:Sem,Tag,bot,[],Iqs),
	append(C,Iqs,Niqs),
	ud(A-B,Tag-bot,Niqs,_).


% ALE does not have a top category, but I don't want
% to see the value `undefined', as this looks like something
% goes wrong.

top(bot,_).

:- initialize_flag(top_features,bot).
