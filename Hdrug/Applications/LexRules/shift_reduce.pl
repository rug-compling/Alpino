:- module(shift_reduce,[]).

clean.
count.
count(0).

parse(o(TopCat,String,_)):-
	user:deriv_tree(TopCat,Tree),
	length(String,Max),
	parse1(0,Max,[],TopCat/Tree).

parse1(P,P,[Final],Final).                    % finished
parse1(P0,P,S0,Final):-                       % shift
	user:lex(P0,P1,Cat,Name),
	parse1(P1,P,[Cat/tree(Name,_,[])|S0],Final).
parse1(P0,P,[Cat0/Tree0|S0],Final):-          % reduce
	user:r_rule(Cat0,Cat,Others,Name),
	find_daughters(Others,S0,S,[Tree0],Ds),
	parse1(P0,P,[Cat/tree(Name,_,Ds)|S],Final).

find_daughters([],S,S,Ds,Ds).
find_daughters([H|T],[H/Tree1|S0],S,Tree0,Tree) :-
	find_daughters(T,S0,S,[Tree1|Tree0],Tree).

