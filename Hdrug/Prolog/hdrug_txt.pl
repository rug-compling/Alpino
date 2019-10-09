:- module(hdrug_txt, [ txt_fs/1,
		       txt_fs_list/1,
		       pretty_graphic/2,
		       pretty_graphic/3,
		       pretty_graphic_tree/1,
		       pretty_graphic_tree/2
		     ]).

:- expects_dialect(sicstus).

:- use_module(hdrug_util).
:- use_module(library(lists)).

txt_fs_list([]).
txt_fs_list([H|T]):-
    txt_fs(H),
    txt_fs_list(T).

txt_fs(Thing) :-
    print_txt_fs(Thing).

print_txt_fs(Thing0) :-
    hdrug_show:change_thing_fs(Thing0,Thing),
    write_it_fs(Thing).

write_it_fs(value(FS,C)) :-
    pp(FS,[]),
    write_pretty_constraints(C),
    nl.

write_it_fs(value(FS)) :-
    pp(FS,[]),
    write('.'),nl.

write_it_fs(clause(H)) :-
    write_pretty_constraint(H),
    write('.'),nl.

write_it_fs(clause(H,B)) :-
    write_pretty_constraint(H), 
    write_pretty_constraints(B),
    nl.

write_pretty_constraints([]) :-
    write('.'),nl.
write_pretty_constraints([H|T]) :-
    write(':-'),nl,
    write_pretty_constraint(H),
    write_pretty_constraints0(T).

write_pretty_constraints0([]) :- 
    write('.').
write_pretty_constraints0([H|T]) :-
    write(','),nl,
    write_pretty_constraint(H),
    write_pretty_constraints0(T).

write_pretty_constraint(H) :-
    H =.. [F|Args],
    name(F,Chars), length(Chars,Len), 
    write(F),
    write_begin_functor(Args),
    write_pretty_arguments0(Args,[tab(Len)]),
    nl, write_list([tab(Len)]),
    write_end_functor(Args).

write_begin_functor([]).
write_begin_functor([_|_]):-
	write('(').

write_end_functor([]).
write_end_functor([_|_]) :-
	write(')').

write_pretty_arguments0([H|T],Tab):-
	nl, 
	write_list(Tab),
	pp(H,[tab(2)|Tab]),
	write_pretty_arguments(T,Tab).

write_pretty_arguments([],_Tab).
write_pretty_arguments([H|T],Tab):-
	write(','),
	nl,
	write_list(Tab),
	pp(H,[tab(2)|Tab]),
	write_pretty_arguments(T,Tab).

pp(_Var/n=FS,Tab):-
	!,
        ppl(FS,Tab).

pp(Var/_='R',_Tab):-
        !,
        write(' <'),
        write_var(Var),
        write('>').  

pp(Var/y=[],_Tab):-
        !,
        write(' <'),
        write_var(Var),
        write('>').

pp(Var/y='$VAR'(_),_Tab):-
	!,
        write(' <'),
        write_var(Var),
        write('>').

pp(Var/y=FS,Tab):-
        write(' <'),
        write_var(Var),
        write('>'), % what happened here?
	ppl(FS,Tab).

pp(lex(_:Lex),_Tab):-
	!,
	write(' "'),
	write(Lex),
	write('"').

pp(lex(Lex),_Tab):-
	!,
	write(' "'),
	write(Lex),
	write('"').

do_not_print(_Var/n='$VAR'(_)).

ppl([a(_Att,Thing)|Rest],Tab):-
	do_not_print(Thing),!,
	ppl(Rest,Tab).

ppl([a(type,Types)|T],Tab):-
	!,
	write(' {'),
	hdrug_feature:write_as_conj(Types,PTypes),
	write(PTypes),
	write('}'),
	ppl2(T,Tab).

ppl([a('BOOLEAN',_Type,Val)|T],Tab):-
	hdrug_feature:give_boolean_type(Val,Exp),
	write(' {'),write(Exp),write('}'),
	ppl2(T,Tab).

ppl([a('UNTYPED',Att,Val)|T],Tab):-
        append(Tab,[' |'],Tab2),
	write(' {U}'),nl,
	write_list(Tab2), 
	hdrug:catch_print_error(Att,Val,Tab2),!,
	ppl2(T,Tab).

ppl([a('UNTYPED',_,_Val)|T],Tab):-
	!,      % should have been catched by catch_print_error
	write(' (error....)'),
	ppl2(T,Tab).

ppl([a(Att,FS)|T],Tab):-
        !,
        write(Att),
        name(Att,AttStr),
        length(AttStr,Length),
        append(Tab,[' |',tab(Length)],Tab2),
        pp(FS,Tab2),
        ppl2(T,Tab).


ppl([],_).

ppl('$VAR'(No),_) :- write('$VAR'(No)).  % changed gj 21/7/93 5/11/93


ppl2([a(_Att,Thing)|Rest],Tab):-
	do_not_print(Thing),!,
	ppl2(Rest,Tab).

ppl2([a(Att,FS)|T],Tab):-
        !,
	nl,write_list(Tab),write(' |'),
        write(Att),
        name(Att,AttStr),
        length(AttStr,Length),
        append(Tab,[' |',tab(Length)],Tab2),
        pp(FS,Tab2),
        ppl2(T,Tab).

ppl2([],_).

write_var(No):-
	write('$VAR'(No)).

write_list([]).
write_list([H|T]):-
	write_term(H),
	write_list(T).

% write_term(Exp)
% write an expression, which is either of a special form
% or otherwise simply written using `write'
% specials include:
%   variables
%   tab(I)
%   nl
%   msg(L)
%   A-B
%   msg2(L)
%
% write_term2(Exp) is similar, but writes an extra space after each
% written term

write_term(X):-
	special_write(X),
	!.

write_term(X):-
	print(X).

special_write(tab(X)):-
	tab(X),
	!.

special_write(nl) :-
	!,
	nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%  pretty printer of graphic trees  %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pretty graphic(+Name,+Term)
pretty_graphic(Name,FS):-
	pretty_graphic(Name,FS,[]).

% pretty graphic(+Name,+Term,?TabExpr)
% TabExpr is a list consisting of tab(I) statements and atoms to be 
% printed with write_list/1
% Name is a name of a graphic mode to allow for different ways to print
% a given structure
% Term is the term to be shown
pretty_graphic(Name,FS,Tab):-
    hdrug_show:change_thing_tree(FS,Name,Tree0),
    tree(Tree0,Tree,_,_),%% build tree
    scalp(Tree,Tab).%% print tree

pretty_graphic_tree(Tree) :-
    pretty_graphic_tree(Tree,[]).

pretty_graphic_tree(Tree0,Tab) :-
    tree(Tree0,Tree,_,_),
    scalp(Tree,Tab).

%%  tree looks like tree(Label,Daughters,Width1,Width2)
%%    where Label is label(Label) or empty
%%          Daughters is list of Daughters (or [])
%%          Width1 is number of labels
%%          Width2 is number of spaces that is needed
%%             (does not take Tab into account...

tree(tree(Label0,_,Ds0),tree(label(Label),Ds,W1,W2),W1,W2):-
    term_atom(Label0,Label),
    atom_length(Label,Length),
    (	Ds0 == []
    ->	W1=1,Ds2=[]
    ;   tree_list(Ds0,Ds2,0,W1,0,Length_d)
    ),
    max_tree(Ds2,Ds,Length,Length_d,W2).

tree_list([],[],W1,W1,W2,W2).
tree_list([H|T],[H2|T2],W10,W1,W20,W2):-
    tree(H,H2,W1H,W2H),
    W11 is W10 + W1H,
    W21 is W20 + W2H,
    tree_list(T,T2,W11,W1,W21,W2).

%% change to 1 if you hate touching node labels
%% leave at 0 if you prefer compact trees
:- initialize_flag(add_node_width,1).


% max_tree(Ds,NewDs,LabelWidth,DaughtersWidth,NewLabelWidth)
% we add extra space around the daughters if it turns out that
% the current label is wider than the space needed for the daughters.
max_tree([],[],L,L,W3):-
    hdrug_flag(add_node_width,Add),
    W2 is L+Add,
    (  oneven(W2)
    -> W3=W2
    ;  W3 is W2+1
    ).
max_tree([Hd|Tds],New_ds,Length,Length_d,W2):-
    (	Length>=Length_d
    ->	W3 is Length + 1,
        (  oneven(W3)
        -> W2 = W3
        ;  W2 is W3+1
        ),
	Extra is W2 - Length_d,
	divi(Extra,E1,E2),
	append([tree(empty,[],nobar,E1)|[Hd|Tds]],
	       [tree(empty,[],nobar,E2)],New_ds)
    ;	New_ds = [Hd|Tds],
	W2 = Length_d
    ).

scalp(tree(label(Id),Br,W1,W2),StTab):-
    atom_length(Id,Idl),
    Width is W2-Idl,
    divi(Width,Tab,_Rest),
%    Tab is (W2-Idl) // 2,
    tab(Tab),
    write(Id),nl,
    print_hor_bars(StTab,[tree(label(Id),Br,W1,W2)]),
    scalplist(Br,StTab).

scalplist(Ds,Tab):- 
    (	empty(Ds)
    ->	true
    ;	print_vert_bars(Ds),
	nl,
	write_list(Tab),
	print_ids(Ds),
	nl,
	print_hor_bars(Tab,Ds),
	scalp_all(Ds,NewDs),
	scalplist(NewDs,Tab)
    ).

print_vert_bars([]).
print_vert_bars([tree(empty,[],_,Br)|Tail]):-
    tab(Br),
    print_vert_bars(Tail).

print_vert_bars([tree(label(_),_Ds,_,Br)|Tail]):-
%    Br2 is (Br - 1) // 2,
    Br1 is Br-1,
    divi(Br1,Br2,Br3),
    tab(Br2),
    write('|'),
    (   empty(Tail)
    ->  true
    ;
         tab(Br3),
%        tab(Br2),
%        tab2(even(Br),1),
        print_vert_bars(Tail)
    ).

scalp_all([],[]).
scalp_all([tree(_Label,[],_W1,W2)|Tail],[tree(empty,[],bar,W2)|Res]):-
    scalp_all(Tail,Res).
scalp_all([tree(_,[Hds|Tds],_,_)|Tail],Res):-
    scalp_all(Tail,Res1),
    append([Hds|Tds],Res1,Res).

print_ids([]).
print_ids([tree(empty,_,_,Br)|Tail]):-
    tab(Br),
    print_ids(Tail).

print_ids([tree(label(Id),_,_,Br)|Tail]):-
    atom_length(Id,L),
    Br3 is Br - L,
    divi(Br3,BrL,BrR),
%    Br2 is Br3 // 2,
    tab(BrL),
    write(Id),
    tab(BrR),
%    tab(Br2),
%    tab2(oneven(Br3),1),
    print_ids(Tail).

empty_hb([]).
empty_hb([tree(_,[],_,_)|T]):-
	empty_hb(T).

no_hor_bars([]).
no_hor_bars([tree(_,[],_,_)|T]):-
	no_hor_bars(T).
no_hor_bars([tree(_,[H|C],_,_)|T]):-
	atmostone([H|C],in),
	no_hor_bars(T).

atmostone([],_).
atmostone([tree(empty,[],_,_)|T],I):-
	!,
	atmostone(T,I).
atmostone([_R|T],in):-
	atmostone(T,out).

print_hor_bars(_,L):-
        empty_hb(L),!.

print_hor_bars(Tab,L):-
	no_hor_bars(L),!,
	write_list(Tab).

print_hor_bars(Tab,L):-
	write_list(Tab),
	print_hor_bars0(L),
	nl,
	write_list(Tab).

print_hor_bars0([]).
print_hor_bars0([tree(_,[],_,Br)|T]):-
        tab(Br),
        print_hor_bars0(T).

print_hor_bars0([tree(_,[H0|Ht],_,_)|T]):-
        print_hor_bars1([H0|Ht]),
        print_hor_bars0(T).

print_hor_bars1([]). %% not used?
print_hor_bars1([tree(_,[],nobar,Br)|T]):-
	!,
        tab(Br),
        print_hor_bars1(T).

print_hor_bars1([tree(_,_I,_,Br)|T]):-
        (   empty(T)
        ->  Br1 is Br-1,
            divi(Br1,Br2,Br3),
            tab(Br2), write('|'), 
            print_hor_bars2(T,Br3)
        ;   divi(Br,Br2,Br3),
            tab(Br2), 
            print_hor_bars2(T,Br3)
        ).

print_hor_bars2([],Br) :- 
	tab(Br).

print_hor_bars2([tree(empty,[],nobar,Br)|T],BrIn):-
        !,
        tab(Br),
        print_hor_bars2(T,BrIn).

print_hor_bars2([tree(_,_I,_,Br)|T],BrIn):-
        empty(T),!,
        wrnum('_',BrIn),
        divi(Br,Br2,Br3),
        wrnum('_',Br2),
        tab(Br3),
	print_hor_bars2(T,0).

print_hor_bars2([tree(_,_I,_,Br)|T],BrIn):-
        wrnum('_',BrIn),
        print_hor_bars2(T,Br).

empty([]).
empty([tree(empty,_,_,_)|Tail]):-
        empty(Tail).
        
wrnum(_,Num):-
	Num < 1,
        !.

wrnum(A,Num):-
        write(A),
        Num2 is Num - 1,
        wrnum(A,Num2).

even(X) :- 
        0 is mod(X,2).

oneven(X) :-
    1 is mod(X,2).

divi(Tot,A,A):-
        even(Tot),
        !,
        A is Tot // 2.

divi(Tot,A,B):-
    A is Tot // 2,
    B is A + 1.

