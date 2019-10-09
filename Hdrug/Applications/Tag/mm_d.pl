%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (c) 1992, 1993 Gertjan van Noord RUG %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% HEAD-CORNER PARSER for LTAGs %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% head-corner parser for headed, lexicalized and feature-based
% tree adjoining grammars

:- module(mm_d,[]).

:- use_module( memo ).

clean :-
	clean_up_memo.

count :-
	memo_count.

count(M) :-
	memo_count(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% the real stuff %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(o(tree(Cat,Deriv,[]),String,Sem)) :-
	user:user_sem(Cat,Sem),
	length(String,Max),
	memo(mm_d:parse(subs_head(Cat,[]),0,Max,0,Max,[],[Deriv])).

% parse(+CatExpr,?Begin,?End,+BeginExtreme,+EndExtreme)
% Catexpr is one of 
% lex_head(Word,ToParse)
% subs_head(Cat,ToParse) 
% e_head(Cat,ToParse)
parse(subs_head(Cat,ToParse),P0,P,E0,E,Ds0,Deriv):-
	user:init(Cat,Add,_Word,DownToParse,Name,Q0,Q),
	E0 =< Q0, Q =< E,
	hc_no_adjoin(DownToParse,_,Mid,Q0,Q,R0,R,E0,E,[],[],Dx),
	hc_no_adjoin(    ToParse,Mid,_,R0,R,P0,P,E0,E,[],[dtree(Name,Add,Dx)|Ds0],Deriv).

parse(lex_head(Word/Q0,ToP),P0,P,E0,E,Deriv0,Deriv):-
	user:lex(Word,Q0,Q),
	E0 =< Q0, Q =< E,
	hc_no_adjoin(ToP,_,_,Q0,Q,P0,P,E0,E,[],Deriv0,Deriv).

parse(e_head(Cat,ToP),P0,P,E0,E,Deriv0,Deriv):-
	hdrug_util:between(E0,E,Q),
	hc(ToP,Cat,_,Q,Q,P0,P,E0,E,[],Deriv0,Deriv).

hc(ToDo,Cat0,Cat,P0,P,Q0,Q,E0,E,U,D0,D) :-
	user:unify_node(Cat0),
	hc_no_adjoin(ToDo,Cat0,Cat,P0,P,Q0,Q,E0,E,U,D0,D).

% case 1: finished
hc_no_adjoin([],Cat,Cat,P0,P,P0,P,_,_,_,D,D).

% case 2: go one level up in your own tree
hc_no_adjoin([t(Mid,L,R)|T],_,Goal,QL,QR,P0,P,E0,E,U,D0,D) :-
	parse_l(L,Q0,QL,E0,D0,D1),
	parse_r(R,QR,Q,E,D1,D2),
	hc(T,Mid,Goal,Q0,Q,P0,P,E0,E,U,D2,D).

% case 3: adjunction takes place at the current node
hc(ToParse,Small,Goal,QL,QR,P0,P,E0,E,U,Ds,D) :-
	user:aux(Small,Add,_,OwnToParse,Name,R0,R),
	user:check_lex(R0,R,E0,QL,QR,E,U),
	hc_no_adjoin(OwnToParse,_Foot,Mid,QL,QR,QLL,QRR,E0,E,[R0|U],[],AD),
	hc_no_adjoin(ToParse,Mid,Goal,QLL,QRR,P0,P,E0,E,U,[dtree(Name,Add,AD)|Ds],D).

% parse_l(+RevDs,?Q0,+Q,+LeftExtreme)
parse_l([],Q,Q,_,D,D).
parse_l([H|T],Q0,Q,E0,D0,D):-
	memo(mm_d:parse(H,Q1,Q,E0,Q,D0,D1)),
	parse_l(T,Q0,Q1,E0,D1,D).

% parse_r(+Ds,+Q0,?Q,+RightExtreme)
parse_r([],Q,Q,_,D,D).
parse_r([H|T],Q0,Q,E,D0,D):-
	memo(mm_d:parse(H,Q0,Q1,Q0,E,D0,D1)),
	parse_r(T,Q1,Q,E,D1,D).

%%%%%%%%%%%%%%%
% end of file %
%%%%%%%%%%%%%%%
