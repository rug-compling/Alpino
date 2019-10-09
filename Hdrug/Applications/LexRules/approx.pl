:- use_module( wlists, [ wappend/3 ] ).


% implode_cat(?Sign,Atom)
implode_cat(H,Cat) :-
	H:cat <=> Cat0,
	find_type(Cat0,[Cat|_]).

% explode_cat(+Atom,Sign).
explode_cat(Cat0,Cat) :-
	{ Cat:cat => Cat0 }.

cfg_approx_lex(I0,I,M,Name) :-
	call_residue(ign_lex(I0,I,M0,Name),_),
	implode_cat(M0,M).

cfg_approx_rule(X ,X ,Ls,Rs,apply) :-
	dif((Ls,Rs),([],[])),
	largs(Ls,X),
	rargs(Rs,X).

cfg_approx_rule(vp,vp,[T],[],topicalize) :-
	topical(T).

cfg_approx_rule(vp,vp,[],[E],extrapose) :-
	extrapos(E).

cfg_approx_rule(vp ,vp,[part],[],cliticize).
cfg_approx_rule(vp ,vp,[vp],[],cliticize).

cfg_approx_rule(vp ,vp,[],[Rs0|Rs],verbfront) :-
	wappend(R,L,[Rs0|Rs]),
	largs(L,vp),
	rargs(R,vp).

extrapos(sbar).
extrapos(vp).
extrapos(pp).

topical(sbar).
topical(vp).
topical(np).
topical(adj).
topical(pred).
topical(adv).
topical(pp).

:- block largs(-,?).
largs([],_).
largs([H|T],X) :-
	larg(X,H),
	largs(T,X).

:- block rargs(-,?).
rargs([],_).
rargs([H|T],X) :-
	rarg(X,H),
	rargs(T,X).

larg(vp,part ).
larg(vp,vp ).
larg(vp,np ).
larg(vp,adj ).
larg(vp,pred).
larg(vp,adv ).
larg(vp,pp ).
larg(n,adj ).
larg(n,att ).
larg(np,adj ).
larg(np,att ).
larg(adj,adv ).
larg(att,adv ).
larg(pred,adv ).
larg(adv,adv ).

rarg(sbar,vp ).
rarg(vp,sbar ).
rarg(vp,pp ).
rarg(vp,vp).
rarg(n,pp ).
rarg(n,vp ).
rarg(n,sbar ).
rarg(np,n ).
rarg(np,pp ).
rarg(np,vp ).
rarg(np,sbar ).
rarg(pp,np ).
rarg(adj,pp ).
rarg(adj,sbar ).
rarg(att,pp ).
rarg(att,sbar ).






























































