:- hdrug_flag(my_clause,_,on(H,B,rules_clause(H,B))).

lexical_entry(Cat) :-
	verb_stem(Cat0),
	inflection(Cat0,Cat1),
	add_adj(Cat1,Cat2),
	add_subj(Cat2,Cat3),
	push_slash(Cat3,Cat).

add_adj(S0,S) :-
	unify_except_l(S0,S,[sem,sc]),
	add_adj(S0:sc,S:sc,S0:sem,S:sem).

add_adj([],[],S,S).
add_adj([H|T0],[H|T],S0,S) :-
	add_adj(T0,T,S0,S).
add_adj(Sc0,[Mod|Sc],Mod:mod:arg,S) :-
	add_adj(Sc0,Sc,Mod:mod:val,S).

push_slash(S0,S) :-
	unify_except_l(S0,S,[sc,slash]),
	push_slash(S0:sc,S:sc,S0:slash,S:slash).

add_subj(S,S) :-
	S:cat:vform => ~fin.
add_subj(S0,S) :-
	S0:cat:vform => fin,
	unify_except(S0,S,sc),
	append(S0:sc,S0:subj,S:sc).

push_slash([],[],S,S).
push_slash([H0|T ],   T, [], [H]) :-
	unify_except(H0,H,lex).    %i.e. lexicality
                                           % waarom rel? Omdat normaal -rel objecten?
push_slash([H|T0],[H|T],S0,S) :-
	push_slash(T0,T,S0,S).


add_sub(S,S).
add_sub(S0,S) :-
	unify_except(S0,S,sc),
	add_el(S0:sc,S0:subj,S:sc).

:- block add_el(-,?,-).

add_el([],El,[El]).
add_el([H|T0],El,[H|T]) :-
	add_el(T0,El,T).

:- hdrug_flag(my_clause,_,off).
