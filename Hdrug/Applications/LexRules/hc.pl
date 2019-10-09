:- module(hc,[]).


parse(o(Cat,String,_)) :-
	length(String,L),
	parse_r(Cat,0,L,L).  % or parse_l(Cat,0,L,0)

% parse Cat starting from P0 to the right ending in P, such that P =< Ext
%
% parse_r(?,+,?,+)
parse_r(Cat,P0,P,Ext) :-
	predict(Cat,P0,Ext,Small,QL,QR),
	hc(Small,QL,QR,Cat,P0,P,P0,Ext).

% parse Cat starting from P to the left ending in P0, such that Ext0 =< P0
%
% parse_l(?,?,+,+)
parse_l(Cat,P0,P,Ext0) :-
	predict(Cat,Ext0,P,Small,QL,QR),
	hc(Small,QL,QR,Cat,P0,P,Ext0,P).

% predict the head-corner Small from QL-QR of Cat within boundaries E0-E
%
% predict(?,+,+,?,-,-)
predict(Cat,E0,E,Small,QL,QR) :-
	user:hfc(Small,Cat),
	E1 is E-1,     % lexical entry always extends one position to the right..
%	larger(E0,QL,E1),
	hdrug_util:between(E0,E1,QL),
	user:lex(QL,QR,Small,_Name),
	QR =< E.

% prove that Small from Q0-Q is a head-corner of Goal from P0-P such that P0-P lies
% within E0-E. 
%
% hc(+,+,+,?,?,?,+,+)
hc(X,Y,Z,X,Y,Z,_,_).
hc(Small,Q0,Q,Goal,P0,P,E0,E) :-
	user:h_rule(Small,Mid,Lefties,Righties,_Name),
	parsel_l(Lefties,QL,Q0,E0),
	parsel_r(Righties,Q,QR,E),
	hc(Mid,QL,QR,Goal,P0,P,E0,E).

%
%
% parsel_l(?,-,+,+)
parsel_l([],L,L,_).
parsel_l([H|T],L0,L,E0):-
	parse_l(H,L1,L,E0),
	parsel_l(T,L0,L1,E0).

%
%
% parsel_r(?,+,?,+)
parsel_r([],L,L,_).
parsel_r([H|T],L0,L,E):-
	parse_r(H,L0,L1,E),
	parsel_r(T,L1,L,E).







