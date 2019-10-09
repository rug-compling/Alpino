:- version('Hpsg Grammar (shipped with Ale). By Gerald Penn and Bob Carpenter').

semantics(ale(A,B,C,_),Sem) :-
	add_to(synsem:loc:cont:Sem,Tag,bot,[],Iqs),
	append(C,Iqs,Niqs),
	ud(A-B,Tag-bot,Niqs,_).


% ALE does not have a top category, but I don't want
% to see the value `undefined', as this looks like something
% goes wrong.

top(bot,_).

:- initialize_flag(top_features,bot).

%%%%%%%%%%%%% defaults to print / not print types and features %%%%%%%%%


%% :- no_write_feat(conx).

