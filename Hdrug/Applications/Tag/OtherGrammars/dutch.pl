%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% TAG GRAMMAR %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DATA STRUCTURES %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(lists, library(lists), [member/2]).

boolean_type(agr,[[fir,sec,thi],[sg,pl],[masc,fem,neut]]).

define_type(top, [[node,sign,syn]],[],_,true).
define_type(node,[],[b,t,syn],_,true).
define_type(sign,[],[sem],_,true).
define_type(syn, [[wh,sbar,pp,p,c,n,v,np,vp,d,s,s1,a,adv,gen]],[agr],_,true).
define_type(Name,[],[],_,true):-
	member(Name,[wh,sbar,pp,p,c,n,v,np,vp,d,s,s1,a,adv,gen]).

catch_print_error(sem,Val,_):-
	write(Val).

% compile types before they're used, otherwise expand-term doesn't
% work..

:- compile_boolean_types.

:- type_compiler.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% DECLARATIONS FOR PARSER %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% The elementary trees %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% no tree `families', every tree is defined

init(jongen,Np:[*D,+N:jongen]) :-
	Np:syn  => np,
	 D:syn  => d,
	 N:syn  => n,
	Np:b:sem <=> Npsem,
	 D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> jongen,
     Np:syn:agr => thi&sg&masc.

init(boek,Np:[*D,+N:boek]) :-
	Np:syn => np,
	 D:syn => d,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> boek,
	 Np:syn:agr => thi&sg&neut.

init(telescoop,Np:[*D,+N:telescoop]) :-
	Np:syn => np,
	 D:syn => d,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> telescoop,
	 Np:syn:agr => thi&sg&neut.

init(boeken,Np:[*D, +N:boeken]) :-
	 D:syn => d,
	Np:syn => np,
	 N:syn => n,
	D:t:sem ==> Nsem:Npsem,
	Np:b:sem <=> Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> boeken,
	 Np:syn:agr => thi&pl&neut.

init(boeken,Np:[N:boeken]) :-
	Np:syn => np,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	 N:t:sem <=> Npsem,
	 N:b:sem ==> boeken,
	 Np:syn:agr => thi&pl&neut.

init(periscoop,Np:[*D,+N:periscoop]) :-
	Np:syn => np,
	 D:syn => d,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> periscoop,
	 Np:syn:agr => thi&sg&neut.


init(vertrekt(a),S:[*Np,+Vp:[V:vertrekt]]):-
	Np:syn => np,
	S:syn => s,
	Vp:syn => vp,
	V:syn => v,
	S:b:sem <=> Vp:t:sem,
	Vp:b:sem <=> V:t:sem,
	V:b:sem ==> vertrekt:Subj,
	Np:t:sem <=> Subj.


init(zwemmen(a),S1:[S:[*Np,+Vp:[V0:[]]],+V:zwemmen]):-
	Np:syn => np,
	S:syn => s,
	S1:syn => s,
	Vp:syn => vp,
	V0:syn => v,
	V:syn => v.

aux(laten(a),S0:[+S1:[*Np,+Vp: [+ =S, V0:[]]],V:laten]) :-
	S0:syn => s,
	S1:syn => s,
	S :syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V0:syn => v,
	V :syn => v.

aux(zag(a),S0:[*Np,+Vp:[+ =S, V:zag]]):-
	S0:syn => s,
	S :syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V :syn => v.
	


init(dat,Sbar:[+C:dat,*S]):-
	Sbar:syn => sbar,
	C:syn => c,
	S:syn => s,
	Sbar:b:sem <=> S:t:sem.

init(e_dat,Sbar:[+ C:[],*S]):-
	Sbar:syn => sbar,
	C:syn => c,
	S:syn => s,
	Sbar:b:sem <=> S:t:sem.

init(de,D:de):-
	D:syn => d,
	D:b:sem ==> X:X.

init(een,D:een):-
	D:syn => d,
	D:b:sem ==> X:X.

init(marie,Np:marie):-
	Np:syn => np,
	Np:b:sem ==> marie.

init(jan,Np:jan):-
	Np:syn => np,
	Np:b:sem ==> jan.

init(piet,Np:piet):-
	Np:syn => np,
	Np:b:sem ==> piet.

init(wie,Wh:wie):-
	Wh:syn => wh,
	Wh:b:sem ==> wie.

init(wat,Wh:wat):-
	Wh:syn => wh,
	Wh:b:sem ==> wat.

aux(mooie,N1:[A:mooie,+ =N2]):-
	A:syn => a,
	A:b:sem ==> mooie,
	A:t:sem ==> F,
	N1:b:sem ==> F:Arg,
	N2:t:sem <=> Arg,
	N1:syn => n,
	N2:syn => n.

aux(kleine,N1:[A:kleine,+ =N2]):-
	A:syn => a,
	A:b:sem ==> kleine,
	A:t:sem ==> F,
	N1:b:sem ==> F:Arg,
	N2:t:sem <=> Arg,
	N1:syn => n,
	N2:syn => n.

aux(zeer,A1:[Adv:zeer,+ =A2]):-
	Adv:syn => adv,
	Adv:b:sem ==> zeer,
	Adv:t:sem ==> F,
	A1:b:sem ==> F:Arg,
	A2:t:sem <=> Arg,
	A1:syn => a,
	A2:syn => a.

aux(zonder(a),Np1:[+ =Np2,Pp:[+P:zonder,*Np3]]):-
	P:syn => p,
	Pp:syn => pp,
	Np3:syn => np,
	Np2:syn => np,
	Np1:syn => np,
	Np1:b:sem ==> Ppsem:Np2sem,
	Np2:t:sem ==> Np2sem,
	P:t:sem ==> Psem,
	Pp:b:sem ==> Psem:Np3sem,
	Pp:t:sem ==> Ppsem,
	Np3:t:sem ==> Np3sem,
	P:b:sem ==> zonder.

aux(zonder(b),Vp1:[+ =Vp2,Pp:[+ P:zonder,*Np]]):-
	P:syn => p,
	Pp:syn => pp,
	Np:syn => np,
	Vp2:syn => vp,
	Vp1:syn => vp,
	Vp1:b:sem ==> Psem:Npsem:Vp2sem,
	Vp2:t:sem ==> Vp2sem,
	P:t:sem ==> Psem,
	Np:t:sem ==> Npsem,
	P:b:sem ==> zonder.

aux(zonder(c),Vp1:[Pp:[+ P:zonder,*Np],+ =Vp2]):-
	P:syn => p,
	Pp:syn => pp,
	Np:syn => np,
	Vp2:syn => vp,
	Vp1:syn => vp,
	Vp1:b:sem ==> Psem:Npsem:Vp2sem,
	Vp2:t:sem ==> Vp2sem,
	P:t:sem ==> Psem,
	Np:t:sem ==> Npsem,
	P:b:sem ==> zonder.

aux(vandaag,Vp1:[Adv:vandaag,+ =Vp2]):-
	Adv:syn => adv,
	Adv:b:sem ==> vandaag,
	Adv:t:sem ==> F,
	Vp1:b:sem ==> F:Arg,
	Vp2:t:sem <=> Arg,
	Vp1:syn => vp,
	Vp2:syn => vp.

aux(zachtjes,Vp1:[Adv:zachtjes,+ =Vp2]):-
	Adv:syn => adv,
	Adv:b:sem ==> zachtjes,
	Adv:t:sem ==> F,
	Vp1:b:sem ==> F:Arg,
	Vp2:t:sem <=> Arg,
	Vp1:syn => vp,
	Vp2:syn => vp.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% end of file %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

