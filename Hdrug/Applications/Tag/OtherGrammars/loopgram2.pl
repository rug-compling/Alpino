%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% TAG GRAMMAR %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DATA STRUCTURES %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module( lists, library(lists), [member/2]).

boolean_type(agr,[[fir,sec,thi],[sg,pl],[masc,fem,neut]]).

define_type(top, [[node,sign,syn]],[],_,true).
define_type(node,[],[b,t,syn,address,deriv],_,true).
define_type(sign,[],[sem],_,true).
define_type(syn, [[wh,sbar,pp,p,c,n,v,np,vp,d,s,a,adv,gen]],[agr],_,true).
define_type(Name,[],[],_,true):-
	member(Name,[wh,sbar,pp,p,c,n,v,np,vp,d,s,a,adv,gen]).

catch_print_error(sem,Val,_):-
	write(Val).

catch_print_error(address,Val,_):-
	write(Val).

catch_print_error(deriv,Val,_):-
	write(Val).

% compile types before they're used, otherwise expand-term doesn't
% work..

:- compile_boolean_types.

:- type_compiler.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% DECLARATIONS FOR PARSER %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_deriv(Cat,Deriv) :-
	Cat:deriv <=> Deriv.

address(Cat,Address) :-
	Cat:address <=> Address.

user_top_category(Name,Cat):-
	Cat:syn => Name.

ignore_semantics(Node,Ign) :-
	Node:syn <=> Ign:syn.

user_syn(X,Syn):-
	X:syn <=> Syn.

user_top(Cat,Top):-
	Cat:t <=> Top.

user_bottom(Cat,Bot):-
	Cat:b <=> Bot.

user_sem(Sign,Sem):-
	Sign:t:sem <=> Sem.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% PRETTY PRINT %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_show(syn,Cat,Res):-
	Cat:syn <=> SynT,
	find_type(SynT,[Res|_]).

user_show(sem,Cat,Res):-
	Cat:t:sem <=> Res.

:- user:usr_cmd([alias,s,show,-,g,syn]).
:- user:usr_cmd([alias,q,show,-,g,sem]).
:- user:usr_cmd([alias,sg,show,-,gm,syn]).
:- user:usr_cmd([alias,qg,show,-,gm,sem]).
:- user:usr_cmd([alias,w,w,-,g,syn]).
:- user:usr_cmd([alias,wg,w,-,gm,syn]).
:- user:usr_cmd([alias,r,c,'../tag','../tag_bt','../tag_bt_d']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% The elementary trees %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(i,X:t) :-
	X:syn => np.

aux(a,X:[D:t, + X1:[= X2]]) :-
	X:syn => np,
	D:syn => d,
	X1:syn => np,
	X2:syn => np.

aux(b,X:[D:t, + X1:[= X2]]) :-
	X:syn => np,
	D:syn => d,
	X1:syn => np,
	X2:syn => np.



%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% end of file %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

