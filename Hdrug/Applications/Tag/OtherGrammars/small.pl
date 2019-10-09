%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% TAG GRAMMAR %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DATA STRUCTURES %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_top_category(Name,f(Name,_,_)).

ignore_semantics(Node,Node).

user_syn(f(Syn,_,_),Syn).

user_top(f(_,Top,_),Top).

user_bottom(f(_,_,Bottom),Bottom).

user_sem(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% PRETTY PRINT %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_show(syn,Cat,Res) :-
	user_syn(Cat,Res).

:- user:usr_cmd([alias,s,show,-,g,syn]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% The elementary trees %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ring,f(np,_,_):[*f(d,_,_),+ f(n,_,_):ring ]).

init(a,f(d,_,_):a).

init(a,f(d,_,_):the).

aux(very, f(a,_,_):[f(adv,_,_):very,+ =f(a,_,_)]).

aux(pretty, f(n,_,_):[f(a,_,_):pretty,+ =f(n,_,_)]).

aux(little, f(n,_,_):[f(a,_,_):little,+ =f(n,_,_)]).


