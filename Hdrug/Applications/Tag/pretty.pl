%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% pretty printing of trees %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% datastructure for derived trees is as follows:
%% tree(Mother,Marker,ListOfDaughters)
%% where in the case of a terminal symbol Term
%% Mother = lex(Term) and
%% ListOfDaughters = []
%%
%% Marker is sometimes used to indicate whether
%% the node is a substitution node (`subs') or
%% a foot node (`foot'). If not it is variable.

%% The Marker position is also used to represent
%% the derivation tree of a parse. Cf. below.

:- use_module(lists, library(lists),[ nth/3, reverse/2, member/2, append/3 ]).

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(V,X,X) :-
	user_show(V,_,_).

graphic_label(V,tree(Cat,Kind,_),C) :-
	user_show(V,Cat,C0),
	add_kind(Kind,C0,C).
graphic_label(V,tree(lex(Cat0),_,_),Cat) :-
	del_index(Cat0,Cat),
	user_show(V,_,_).

graphic_daughter(V,P,tree(_,_,Ds),El):-
	user_show(V,_,_),
	nth(P,Ds,El).

del_index(W0/_I,W):-
	!,
	W0 = W.
del_index(W,W).

add_kind(Var,C0,C) :-
	var(Var),!,C0=C.
add_kind(subs,C,*C).
add_kind(foot,C,=C).
add_kind(_,C,C).


%% datastructure for derived trees is
%% dtree(Name,Address,ListOfDaughters

graphic_path(dt,tree(_,D,_),D).

graphic_label(dt,dtree(Name,Address,_),Label) :-
	(  var(Address)   % root 
        -> Name = Label
        ;  Label = Address:Name
        ).

graphic_daughter(dt,P,dtree(_,_,Ds),El) :-
	nth(P,Ds,El).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% show elementary trees %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile hdrug_command/3.
hdrug_command(l,show:sh(Type,Output,Things),L0) :-
        show_command(Type,Output,L0,Words),
	sel_words(Words,Things).

:- multifile hdrug_command_help/3.
hdrug_command_help(l,"l <Type> <Output> Words",
"displays elementary trees in various forms, cf. s command").

sel_words(Ws,Ts) :-
	findall(T,(member(W,Ws),sel_word(W,T)),Ts).

sel_word(H,object(Name,o(Obj,_,_))) :-
	ele(H,Name,Obj).

% key is either a terminal symbol or the rule name
ele(Word,initial(Name),Tree) :-
	ini(Word,Tree,Name).

ele(Word,auxiliary(Name),Tree) :-
	au(Word,Tree,Name).

ele(Name,initial(Name),Tree) :-
	ini(Name,Tree).

ele(Name,auxiliary(Name),Tree) :-
	au(Name,Tree).

ini(Word,Tree,Name) :-
	init_rule(_,Word,List,Name),
	to_tree(List,tree(lex(Word),none,[]),Tree).

ini(Name,Tree) :-
	init_rule(_,Word,List,Name),
	to_tree(List,tree(lex(Word),none,[]),Tree).

au(Word,Tree,Name) :-
	aux_rule(Foot,_,List,Words,Name),
	member(Word/_,Words),
	to_tree(List,tree(Foot,foot,[]),Tree).

au(Name,Tree) :-
	aux_rule(Foot,_,List,_,Name),
	to_tree(List,tree(Foot,foot,[]),Tree).

to_tree([],T,T).
to_tree([t(Node,L,R)|Rest],T0,T) :-
	to_tree_ls(L,[T0|Rds],Ds),
	to_tree_rs(R,Rds),
	to_tree(Rest,tree(Node,none,Ds),T).

to_tree_rs([],[]).
to_tree_rs([H|T],[H0|T0]):-
	to_tree(H,H0),
	to_tree_rs(T,T0).

to_tree_ls([],I,I).
to_tree_ls([H|T],I0,I):-
	to_tree(H,H0),
	to_tree_ls(T,[H0|I0],I).

to_tree(lex_head(Word,Chain),Tree) :-
	to_tree(Chain,tree(lex(Word),none,[]),Tree).

to_tree(subs_head(Subs,Chain),Tree) :-
	to_tree(Chain,tree(Subs,subs,[]),Tree).

to_tree(e_head(Node,Chain),Tree) :-
	to_tree(Chain,tree(Node,none,[]),Tree).

show_node(syn,tree(L,_,_)) :-
	show(fs,clig,[value(L)]).
