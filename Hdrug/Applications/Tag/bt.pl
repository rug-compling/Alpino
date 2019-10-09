%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (c) 1992, 1993 Gertjan van Noord RUG %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% HEAD-CORNER PARSER for LTAGs %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% head-corner parser for headed, lexicalized and feature-based
% tree adjoining grammars

:- module(bt,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% the real stuff %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(o(tree(Cat,_,[]),String,Sem)) :-
	length(String,Max),
	user:user_sem(Cat,Sem),
	parse(subs_head(Cat,[]),0,Max,0,Max).

% parse(+CatExpr,?Begin,?End,+BeginExtreme,+EndExtreme)
% Catexpr is one of 
% lex_head(Word,ToParse)
% subs_head(Cat,ToParse) 
% e_head(Cat,ToParse)
parse(subs_head(Cat,ToParse),P0,P,E0,E):-
	user:init(Cat,_Add,_Word,DownToParse,_Nm,Q0,Q),
	E0 =< Q0, Q =< E,
	hc_no_adjoin(DownToParse,_,Mid,Q0,Q,R0,R,E0,E,[]),
	hc_no_adjoin(ToParse,Mid,_,R0,R,P0,P,E0,E,[]).

parse(lex_head(Word/Q0,ToP),P0,P,E0,E):-
	user:lex(Word,Q0,Q),
	E0 =< Q0, Q =< E,
	hc_no_adjoin(ToP,_,_,Q0,Q,P0,P,E0,E,[]).

parse(e_head(Cat,ToP),P0,P,E0,E):-
	hdrug_util:between(E0,E,Q),
	hc(ToP,Cat,_,Q,Q,P0,P,E0,E,[]).

hc(ToDo,Cat0,Cat,P0,P,Q0,Q,E0,E,U) :-
	user:unify_node(Cat0),
	hc_no_adjoin(ToDo,Cat0,Cat,P0,P,Q0,Q,E0,E,U).

% case 1: finished
hc_no_adjoin([],Cat,Cat,P0,P,P0,P,_,_,_).

% case 2: go one level up in your own tree
hc_no_adjoin([t(Mid,L,R)|T],_,Goal,QL,QR,P0,P,E0,E,U) :-
	parse_l(L,Q0,QL,E0),
	parse_r(R,QR,Q,E),
	hc(T,Mid,Goal,Q0,Q,P0,P,E0,E,U).

% case 3: adjunction takes place at the current node
hc(ToParse,Small,Goal,QL,QR,P0,P,E0,E,U) :-
	user:aux(Small,_Ad,_Anchor,OwnToParse,_Name,R0,R),
	user:check_lex(R0,R,E0,QL,QR,E,U),
	hc_no_adjoin(OwnToParse,_Foot,Mid,QL,QR,QLL,QRR,E0,E,[R0|U]),
	hc_no_adjoin(ToParse,Mid,Goal,QLL,QRR,P0,P,E0,E,U).

% parse_l(+RevDs,?Q0,+Q,+LeftExtreme)
parse_l([],Q,Q,_).
parse_l([H|T],Q0,Q,E0):-
	parse(H,Q1,Q,E0,Q),
	parse_l(T,Q0,Q1,E0).

% parse_r(+Ds,+Q0,?Q,+RightExtreme)
parse_r([],Q,Q,_).
parse_r([H|T],Q0,Q,E):-
	parse(H,Q0,Q1,Q0,E),
	parse_r(T,Q1,Q,E).

%%%%%%%%%%%%%%%
% end of file %
%%%%%%%%%%%%%%%
