%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% TAG GRAMMAR %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DATA STRUCTURES %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(lists, library(lists), [ member/2 ]).

%:- use_module(library(hdrug_feature)).

boolean_type(agr,[[fir,sec,thi],[sg,pl],[masc,fem,neut]]).

top([node,sign,syn]).
type(node,[],[b,t,syn,address]).
type(sign,[],[sem]).
type(syn, [wh,sbar,pp,p,c,n,v,np,vp,d,s,a,adv,gen],[agr]).
type(Name,[],[]):-
	member(Name,[wh,sbar,pp,p,c,n,v,np,vp,d,s,a,adv,gen]).

extensional(Name) :-
       member(Name,[wh,sbar,pp,p,c,n,v,np,vp,d,s,a,adv,gen]).

catch_print_error(sem,Val,_):-
	write(Val).

catch_print_error(address,Val,_):-
	write(Val).

% compile types before they're used, otherwise expand-term doesn't
% work..

:- hdrug_feature:type_compiler.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% DECLARATIONS FOR PARSER %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

address(Cat,Address) :-
	Cat:address <=> Address.

user_top_category(Name,Cat):-
	member(Name,[wh,sbar,pp,p,c,n,v,np,vp,d,s,a,adv,gen]),
	Cat:syn => Name.

:- initialize_flag(top_features,s).

ignore_semantics(Node,Ign) :-
	Node:address <=> Ign:address,
	Node:syn <=> Ign:syn.

user_syn(X,Syn):-
	X:syn <=> Syn.

user_top(Cat,Top):-
	Cat:t <=> Top.

user_bottom(Cat,Bot):-
	Cat:b <=> Bot.

user_sem(Sign,Sem):-
	Sign:t:sem <=> PlaceHolder,
	PlaceHolder = Sem.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% PRETTY PRINT %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_show(syn,Cat,Res):-
	Cat:syn <=> SynT,
	find_type(SynT,[Res|_]).

user_show(semx,Cat,Res):-
	Cat:t:sem <=> Res.

user_show(matrix(x),C,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% The elementary trees %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% no tree `families', every tree is defined

init(boy,Np:[*D,+N:boy]) :-
	Np:syn  => np,
	 D:syn  => d,
	 N:syn  => n,
	Np:b:sem <=> Npsem,
	 D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> boy,
     Np:syn:agr => thi&sg&masc.

init(book,Np:[*D,+N:book]) :-
	Np:syn => np,
	 D:syn => d,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> book,
	 Np:syn:agr => thi&sg&neut.

init(telescope,Np:[*D,+N:telescope]) :-
	Np:syn => np,
	 D:syn => d,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> telescope,
	 Np:syn:agr => thi&sg&neut.

init(glasses,Np:[*D, +N:glasses]) :-
	 D:syn => d,
	Np:syn => np,
	 N:syn => n,
	D:t:sem ==> Nsem:Npsem,
	Np:b:sem <=> Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> glasses,
	 Np:syn:agr => thi&pl&neut.

init(glasses,Np:[N:glasses]) :-
	Np:syn => np,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	 N:t:sem <=> Npsem,
	 N:b:sem ==> glasses,
	 Np:syn:agr => thi&pl&neut.

init(periscope,Np:[*D,+N:periscope]) :-
	Np:syn => np,
	 D:syn => d,
	 N:syn => n,
	Np:b:sem <=> Npsem,
	D:t:sem ==> Nsem:Npsem,
	 N:t:sem <=> Nsem,
	 N:b:sem ==> periscope,
	 Np:syn:agr => thi&sg&neut.

init(leave_a,S:[*Np,+Vp:[V:left]]):-
	Np:syn => np,
	S:syn => s,
	Vp:syn => vp,
	V:syn => v,
	S:b:sem <=> Vp:t:sem,
	Vp:b:sem <=> V:t:sem,
	V:b:sem ==> leave:Subj,
	Np:t:sem <=> Subj.

init(count_a,S:[*Np,+Vp:[+V:counted,PP:[P:on,+ *Np2]]]):-
	Np:syn => np,
	Np2:syn => np,
	S:syn => s,
	Vp:syn => vp,
	V:syn => v,
	S:b:sem <=> Vp:t:sem,
	Vp:b:sem <=> V:t:sem,
	V:b:sem ==> count_on:Subj:Obj,
	Np:t:sem <=> Subj,
	Np2:t:sem <=> Obj,
	PP:syn => pp,
	P:syn => p.

init(see_a,S:[*Np,+Vp:[+V:saw,*Np2]]):-
	Np:syn => np,
	Np2:syn => np,
	S:syn => s,
	Vp:syn => vp,
	V:syn => v,
	S:b:sem <=> Vp:t:sem,
	Vp:b:sem <=> V:t:sem,
	V:b:sem ==> see:Subj:Obj,
	Np:t:sem <=> Subj,
	Np2:t:sem <=> Obj.

init(see_b,S:[*Wh,+Sbar:[C:did,+S2:[*Np,+Vp:[V:see]]]]):-
	S:syn => s,
	Wh:syn => wh,
	Sbar:syn => sbar,
	C:syn => c,
	S2:syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> see:Subj:Obj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Obj,
	Np:t:sem <=> Subj.

init(see_c,S:[*Wh,+Sbar:[C:that,+S2:[*Np,+Vp:[V:saw]]]]):-
	S:syn => s,
	Wh:syn => wh,
	Sbar:syn => sbar,
	C:syn => c,
	S2:syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> see:Subj:Obj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Obj,
	Np:t:sem <=> Subj.

init(see_d,S:[*Wh,+Sbar:[C:[],+S2:[*Np,+Vp:[V:saw]]]]):-
	S:syn => s,
	Wh:syn => wh,
	Sbar:syn => sbar,
	C:syn => c,
	S2:syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> see:Subj:Obj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Obj,
	Np:t:sem <=> Subj.

init(see_e,S:[*Wh,+Sbar:[C:[],+S2:[Vp:[+V:saw,*Np]]]]):-
	S:syn => s,
	C:syn => c,
	Wh:syn => wh,
	Sbar:syn => sbar,
	S2:syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> see:Subj:Obj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Subj,
	Np:t:sem <=> Obj.

init(see_f,S:[*Wh,+Sbar:[C:did,+S2:[Vp:[+V:see,*Np]]]]):-
	S:syn => s,
	Wh:syn => wh,
	Sbar:syn => sbar,
	C:syn => c,
	S2:syn => s,
	Np:syn => np,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> see:Subj:Obj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Subj,
	Np:t:sem <=> Obj.

init(leave_b,S:[*Wh,+Sbar:[C:[],+S2:[Vp:[V:left]]]]):-
	S:syn => s,
	C:syn => c,
	Wh:syn => wh,
	Sbar:syn => sbar,
	S2:syn => s,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> leave:Subj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Subj.

init(leave_c,S:[*Wh,+Sbar:[C:did,+S2:[Vp:[V:leave]]]]):-
	S:syn => s,
	Wh:syn => wh,
	Sbar:syn => sbar,
	C:syn => c,
	S2:syn => s,
	Vp:syn => vp,
	V:syn => v,
	V:b:sem ==> leave:Subj,
	V:t:sem <=> Vp:b:sem,
	Vp:t:sem <=> S2:b:sem,
	S2:t:sem <=> Sbar:b:sem,
	Sbar:t:sem <=> S:b:sem,
	Wh:t:sem <=> Subj.


aux(say_a,Sbar1:[C:did,+S:[*Np,+Vp1:[Vp2:[V:say],+ =Sbar2]]]):-
	Sbar1:syn => sbar,
	C:syn => c,
	S:syn => s,
	Np:syn => np,
	Vp1:syn => vp,
	Vp2:syn => vp,
	V:syn => v,
	Sbar2:syn => sbar,
	Sbar2:t:sem <=> Obj,
	Np:t:sem <=> Subj,
	V:b:sem ==> say:Subj:Obj,
	V:t:sem <=> Vp2:b:sem,
	Vp2:t:sem <=> Vp1:b:sem,
	Vp1:t:sem <=> S:b:sem,
	S:t:sem <=> Sbar1:b:sem.

aux(say_b,Sbar1:[C:that,+S:[*Np,+Vp1:[Vp2:[V:said],+ =Sbar2]]]):-
	Sbar1:syn => sbar,
	C:syn => c,
	S:syn => s,
	Np:syn => np,
	Vp1:syn => vp,
	Vp2:syn => vp,
	V:syn => v,
	Sbar2:syn => sbar,
	Sbar2:t:sem <=> Obj,
	Np:t:sem <=> Subj,
	V:b:sem ==> say:Subj:Obj,
	V:t:sem <=> Vp2:b:sem,
	Vp2:t:sem <=> Vp1:b:sem,
	Vp1:t:sem <=> S:b:sem,
	S:t:sem <=> Sbar1:b:sem.

aux(say_c,Sbar1:[C:[],+S:[*Np,+Vp1:[Vp2:[V:said],+ =Sbar2]]]):-
	C:syn => c,
	Sbar1:syn => sbar,
	S:syn => s,
	Np:syn => np,
	Vp1:syn => vp,
	Vp2:syn => vp,
	V:syn => v,
	Sbar2:syn => sbar,
	Sbar2:t:sem <=> Obj,
	Np:t:sem <=> Subj,
	V:b:sem ==> say:Subj:Obj,
	V:t:sem <=> Vp2:b:sem,
	Vp2:t:sem <=> Vp1:b:sem,
	Vp1:t:sem <=> S:b:sem,
	S:t:sem <=> Sbar1:b:sem.

init(that,Sbar:[+C:that,*S]):-
	Sbar:syn => sbar,
	C:syn => c,
	S:syn => s,
	Sbar:b:sem <=> S:t:sem.

/* not a lexicalized tree, hence illegal
init(e_that,Sbar:[+ C:[],*S]):-
	Sbar:syn => sbar,
	C:syn => c,
	S:syn => s,
	Sbar:b:sem <=> S:t:sem.
*/

init(the,D:the):-
	D:syn => d,
	D:b:sem ==> X:X.

init(a,D:a):-
	D:syn => d,
	D:b:sem ==> X:X.

init(john,Np:john):-
	Np:syn => np,
	Np:b:sem ==> john.

init(mary,Np:mary):-
	Np:syn => np,
	Np:b:sem ==> mary.

init(gen_s,D:[*Np,+Gen:s]):-
	D:syn => d,
	Np:syn => np,
	Gen:syn => gen,
	D:b:sem <=> Gen:t:sem,
	Np:t:sem <=> Arg,
	Gen:b:sem ==> Book:(Arg:Book).

init(who,Wh:who):-
	Wh:syn => wh,
	Wh:b:sem ==> who.

init(what,Wh:what):-
	Wh:syn => wh,
	Wh:b:sem ==> what.

aux(pretty,N1:[A:pretty,+ =N2]):-
	A:syn => a,
	A:b:sem ==> pretty,
	A:t:sem ==> F,
	N1:b:sem ==> F:Arg,
	N2:t:sem <=> Arg,
	N1:syn => n,
	N2:syn => n.

aux(little,N1:[A:little,+ =N2]):-
	A:syn => a,
	A:b:sem ==> little,
	A:t:sem ==> F,
	N1:b:sem ==> F:Arg,
	N2:t:sem <=> Arg,
	N1:syn => n,
	N2:syn => n.

aux(very,A1:[Adv:very,+ =A2]):-
	Adv:syn => adv,
	Adv:b:sem ==> very,
	Adv:t:sem ==> F,
	A1:b:sem ==> F:Arg,
	A2:t:sem <=> Arg,
	A1:syn => a,
	A2:syn => a.

aux(almost,A1:[Adv:almost,+ =A2]):-
	Adv:syn => adv,
	Adv:b:sem ==> almost,
	Adv:t:sem ==> F,
	A1:b:sem ==> F:Arg,
	A2:t:sem <=> Arg,
	A1:syn => adv,
	A2:syn => adv.

aux(with_a,Np1:[+ =Np2,Pp:[+P:with,*Np3]]):-
	P:syn => p,
	Pp:syn => pp,
	Np3:syn => np,
	Np2:syn => np,
	Np1:syn => np,
	Np1:b:sem ==> Psem:Np3sem:Np2sem,
	Np2:t:sem ==> Np2sem,
	P:t:sem ==> Psem,
	Np3:t:sem ==> Np3sem,
	P:b:sem ==> with.

aux(with_b,Vp1:[+ =Vp2,Pp:[+P:with,*Np]]):-
	P:syn => p,
	Pp:syn => pp,
	Np:syn => np,
	Vp2:syn => vp,
	Vp1:syn => vp,
	Vp1:b:sem ==> Psem:Npsem:Vp2sem,
	Vp2:t:sem ==> Vp2sem,
	P:t:sem ==> Psem,
	Np:t:sem ==> Npsem,
	P:b:sem ==> with.

% zo moet 't eigenlijk: opbouw semantiek. Niet goed bij andere preps..
aux(without_a,Np1:[+ =Np2,Pp:[+P:without,*Np3]]):-
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
	P:b:sem ==> without.

aux(without_b,Vp1:[+ =Vp2,Pp:[+ P:without,*Np]]):-
	P:syn => p,
	Pp:syn => pp,
	Np:syn => np,
	Vp2:syn => vp,
	Vp1:syn => vp,
	Vp1:b:sem ==> Psem:Npsem:Vp2sem,
	Vp2:t:sem ==> Vp2sem,
	P:t:sem ==> Psem,
	Np:t:sem ==> Npsem,
	P:b:sem ==> without.

aux(behind_a,Np1:[+ =Np2,Pp:[+ P:behind,*Np3]]):-
	P:syn => p,
	Pp:syn => pp,
	Np3:syn => np,
	Np2:syn => np,
	Np1:syn => np,
	Np1:b:sem ==> Psem:Np3sem:Np2sem,
	Np2:t:sem ==> Np2sem,
	P:t:sem ==> Psem,
	Np3:t:sem ==> Np3sem,
	P:b:sem ==> behind.

aux(behind_b,Vp1:[+ =Vp2,Pp:[+ P:behind,*Np]]):-
	P:syn => p,
	Pp:syn => pp,
	Np:syn => np,
	Vp2:syn => vp,
	Vp1:syn => vp,
	Vp1:b:sem ==> Psem:Npsem:Vp2sem,
	Vp2:t:sem ==> Vp2sem,
	P:t:sem ==> Psem,
	Np:t:sem ==> Npsem,
	P:b:sem ==> behind.

aux(today,Vp1:[+ =Vp2,Adv:today]):-
	Adv:syn => adv,
	Adv:b:sem ==> today,
	Adv:t:sem ==> F,
	Vp1:b:sem ==> F:Arg,
	Vp2:t:sem <=> Arg,
	Vp1:syn => vp,
	Vp2:syn => vp.

aux(silently,Vp1:[+ =Vp2,Adv:silently]):-
	Adv:syn => adv,
	Adv:b:sem ==> silently,
	Adv:t:sem ==> F,
	Vp1:b:sem ==> F:Arg,
	Vp2:t:sem <=> Arg,
	Vp1:syn => vp,
	Vp2:syn => vp.


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% end of file %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

