% some syntactic sugar

:- op(403,fx,'\').      % not right
:- op(403,fx,'/').      % not left
:- op(403,fx,'\\').     % left
:- op(403,fx,'//').     % right
:- op(402,xfy,'#').     % Cat # SubcatList
:- op(200,fx,^).        % category

:- op(800,xfy,'=/=').   % dif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% data structures %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

X =/= Y :-
	{ hdrug_feature:eval_path(X,Xval),
	  dif(Xval,Yval),
	  hdrug_feature:eval_path(Y,Yval) 
        }.

eval_a_call(_ =/= _, no).   % call it upon partial evaluation, but
                            % do not touch the terms..

user_eval({Cat#Sc},C) :-
	{ C:sc <=> Sc,
          C <=> Cat }.

user_eval({\Cat},C) :-
	{ C:dir => ~right,
	  C <=> Cat }.

user_eval({/Cat},C) :-
	{ C:dir => ~left,
	  C <=> Cat }.

user_eval({\\Cat},C) :-
	{ C:dir => left,
	  C <=> Cat }.

user_eval({//Cat},C) :-
	{ C:dir => right,
	  C <=> Cat }.

user_eval({[H|T]},C) :-
	{ C:h <=> H,
	  C:t <=> T }.

user_eval({^Cat},C) :-
	{ C:cat => Cat }.

user_defined_eval({/_}).
user_defined_eval({\_}).
user_defined_eval({//_}).
user_defined_eval({\\_}).
user_defined_eval({_#_}).
user_defined_eval([_|_]).
user_defined_eval({^_}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HFC/2 for head-driven processing %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hfc(Cat0,Cat) :-
	Cat0:inv   <=> Cat:inv,
	Cat0:cat   <=> Cat:cat,
	Cat0:sem   <=> Cat:sem,
	Cat0:fargs  <=> Cat:fargs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IGNORE_SEMANTICS/2 for packing %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ignore_semantics(Node,NodeWithoutSem).

ignore_semantics(Sa,Sb) :-
	unify_except(Sa,Sb,sem).

%%%%%%%%%%%%%%%%%%%
%%% SEMANTICS/2 %%%
%%%%%%%%%%%%%%%%%%%

% semantics(Node,Sem)
% defines the semantics Sem of node Node. Is used: 
% 
% 1. in order to determine the semantic-head of a rule 
%    (for head-driven generation).
% 2. to instantiate the semantics of a node, at the
%    start of generation. 
% 3. to print the result of parsing.

semantics(Sign,Sign:sem).

%%%%%%%%%%%
%% TOP/2 %%
%%%%%%%%%%%

% top(Name,Node)
% Name is atom
% Node is node
% if flag(top_features,Name), then top(Name,Node) is called before
% parsing or generation of Node.

top(complete, _ # []).

top(s,S # []) :-
	S:cat:vform => fin,
	S:inv => - ,
	S:f_slash => [],
	S:slash => [],
	S:rel => [],
	S:extra => [].

top(sbar,S & ^vp # []) :-
	S:cat:vform => dat,
	S:rel => [],
	S:slash => [],
	S:extra => [].

top(main,S # []) :-
	S:rel => [],
	S:slash => [],
	S:extra => [],
	S:inv => +,
	S:f_slash <=> [_],
	S:cat:vform => fin.

top(main_np,S) :-
	top(main,S),
	S:f_slash <=> [^np].

top(top,Nslash # []) :-
	Nslash:slash => [],
	Nslash:rel => [],
	Nslash:extra => [].

top(inv,Inv # []) :-
	Inv:rel => [],
	Inv:slash => [],
	Inv:extra => [],
	Inv:inv => + .

top(tp,Cat) :-
	Cat:slash => [],
	Cat:extra => [].

top(np,Cat) :-
	Cat:cat => np,
	Cat:sc => [],
	Cat:slash => [],
	Cat:extra => [].

top(vp,Cat) :-
	Cat:cat => vp,
	Cat:sc => [],
	Cat:slash <=> [_Subj],
	Cat:extra => [],
	Cat:inv => + .

%%%%%%%%%%%%%%%%%%%%%
%%%% deriv_tree %%%%%
%%%%%%%%%%%%%%%%%%%%%

deriv_tree(X,Tree) :-
	X:tree ==> Tree.

%%%%%%%%%%%%%%%%
% PRETTY PRINT %
%%%%%%%%%%%%%%%%

show_object_default2(No) :-
    hdrug_flag(parser,P),
    parser_to_type(P,Type),
    show_object_no(No,tree(Type),clig).

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

:- use_module( lists, library(lists), all ).

graphic_path(q,Rule,Sem) :-
	copy_term(Rule:sem,Sem),
	prettyvars(Sem).

graphic_daughter(q,1,Sem,Sem:arg1).
graphic_daughter(q,2,Sem,Sem:arg2).
graphic_daughter(q,3,Sem,Sem:arg3).
graphic_daughter(q,P,Sem,Mod) :-
	Sem => c0,
	nth(P,Sem:restr,Mod).

graphic_daughter(q,P,Sem,Mod) :-
	Sem => c1,
	nth(P0,Sem:restr,Mod),
	P is P0 + 1.

graphic_daughter(q,P,Sem,Mod) :-
	Sem => c2,
	nth(P0,Sem:restr,Mod),
	P is P0 + 2.

graphic_daughter(q,P,Sem,Mod) :-
	Sem => c3,
	nth(P0,Sem:restr,Mod),
	P is P0 + 3.

graphic_label(q,Sem,Label) :-
	find_index(Sem:index,Label0),
	find_label(Sem,Label1),
	cola(Label0,Label1,Label).

find_index('$VAR'('_'),nolabel) :- 
	!.
find_index('$VAR'(L),'$VAR'(L)).
find_index(Term,Label) :-
	functor(Term,_,A),
	arg(A,Term,ReentPos),
	find_index(ReentPos,Label).

find_label(`ix,nolabel).
find_label(S,Label) :-
	S:fun => prime(Label).

cola(nolabel,nolabel,'').
cola(nolabel,L,L).
cola(L,nolabel,L).
cola(L0,L1,i(L0,L1)).

graphic_path(d,X,Tree) :-
	X:tree <=> Tree.

graphic_daughter(d,No,tree(_,_,Ds),D) :-
	nth(No,Ds,D).

graphic_daughter(d,1,tree(L,_,[]),w(Word)) :-
	L:lex:word <=> Word.

graphic_label(d,w(Word),Word).
graphic_label(d,tree(L0,_,_),L) :-
        pr(L0,L).

graphic_path(matrix(e),X,Tree) :-
	X:tree <=> Tree.

graphic_daughter(matrix(e),No,tree(_,_,Ds),D) :-
	nth(No,Ds,D).

graphic_daughter(matrix(e),1,tree(L,_,[]),w(Word)) :-
	L:lex:word <=> Word.

graphic_label(matrix(e),w(Word),Word).
graphic_label(matrix(e),tree(L,_,_),L).

graphic_path(dt,X,Tree) :-
	X:tree <=> Tree.

graphic_daughter(dt,No,tree(_,_,Ds),D) :-
	nth(No,Ds,D).

graphic_label(dt,tree(L,_,_),L).

graphic_path(fargs,X0,X) :-
	copy_term(X0,X),
	prettyvars(X).

graphic_label(fargs,Sign,Sem) :-
	Sign:sem <=> Sem0,
	extern_sem(Sem,Sem0).

graphic_daughter(fargs,No,Sign,D):-
	nonvar(Sign:fargs),
	nth(No,Sign:fargs,D).

% show_node/2 is used when you click on a node of a tree
show_node(q,Term) :-
    show(fs,clig,[value(Term)]).
show_node(d,tree(L0,_,_)) :-
    shorten_label(L0,L),
    show(fs,clig,[value(L)]).

%%%%%%%%%%%%%%%%%%
%% EXTERN_SEM/2 %%
%%%%%%%%%%%%%%%%%%

extern_sem(Term,Fs) :-
	nonvar(Fs),!,        % from fs to sem
	copy_term(Fs,Fs0),   % unwise to affect fs, we dont know where
	                     % this predicate will be useful
	prettyvars(Fs0),
	extern_sem0(Term,Fs0).

extern_sem(Term0,Fs) :-     
	% var(Fs),           % from sem to fs
	un_prettyvars(Term0,Term),
	extern_sem0(Term,Fs).


extern_sem0(Index,Sem) :-
	var(Index),
	var(Sem),!,
	{ Sem => ix,
	  Sem:index <=> Index }.

extern_sem0( i(Index,Short),Sem) :-
	var(Sem),!,
	{ Sem:index <=> Index },
	extern_sem00(Short,Sem).

extern_sem0(Index,Sem) :-
	var(Index),
	nonvar(Sem),
	{ Sem => ix,
	  Sem:index <=> Ind },
	find_index(Ind,Ind1),
	( Ind1 == nolabel -> true ; Ind1 = Index ), !.

extern_sem0(i(Index,Short),Sem) :-
	nonvar(Sem),
	{ Sem:index <=> Ind },
	find_index(Ind,Index),
	\+ Index == nolabel,
	!,
	extern_sem00(Short,Sem).

extern_sem0(Term,Sem) :-
	extern_sem00(Term,Sem).

extern_sem00(Term,Sem) :-
	extract_restr(Term0,Term,Sem:restr),
	extern_sem1(Term0,Sem).

extern_sem1(Term0,Sem) :-
	var(Term0),
	!,
	Sem:fun <=> FunT,
	find_type(FunT,[prime(Fun)|_]),
	find_args(Sem,Args),
	Term0 =.. [Fun|Args].

extern_sem1(Term0,Sem) :-
	Term0 =.. [Fun|Args],
	Sem:fun => prime(Fun),
	find_args(Sem,Args).

find_args(Sem,[]) :-
	Sem => c0.
find_args(Sem,[A1]) :-
	Sem:arg1 <=> A1a,
	extern_sem0(A1,A1a),
	Sem => c1.
find_args(Sem,[A1,A2]) :-
	Sem => c2,
	Sem:arg1 <=> A1a,
	extern_sem0(A1,A1a),
	Sem:arg2 <=> A2a,
	extern_sem0(A2,A2a).
find_args(Sem,[A1,A2,A3]) :-
	Sem => c3,
	Sem:arg1 <=> A1a,
	extern_sem0(A1,A1a),
	Sem:arg2 <=> A2a,
	extern_sem0(A2,A2a),
	Sem:arg3 <=> A3a,
	extern_sem0(A3,A3a).

extract_restr(A,T,[Hr|Tr]):-
	wappend(Tail0,[A],T),
	extern_sem_l(Tail0,[Hr|Tr]).
extract_restr(A,A,[]).

:- block extern_sem_l(-,-).
extern_sem_l([],[]).
extern_sem_l([H|T],[Sem|SemT]) :-
	extern_sem0(H,Sem),
	extern_sem_l(T,SemT).

%%%%%%%%%%%%%%%%%
%%%%% flags %%%%%
%%%%%%%%%%%%%%%%%

gflags :-
	wr_flag(add_mod),
	wr_flag(push_to_slash),
	wr_flag(push_to_extra).

flags_on :-
	tk_flag(add_mod,_,on),
	tk_flag(push_to_slash,_,on),
	tk_flag(push_to_extra,_,on).

flags_off :-
	tk_flag(add_mod,_,off),
	tk_flag(push_to_slash,_,off),
	tk_flag(push_to_extra,_,off).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PORTRAY of signs, etc. %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%gm_show(A,B) :-
%%	pr(A,B).
:- multifile portray/1.
portray(Sign):-
    pr(Sign).

pr(Sign):-
	pr(Sign,Term),
	write(Term).

pr(Sign # Args,Term) :-
	find_type(Sign:cat,[C0|_]),
	combine_pt(Sign:dir,C0,Term0),
	combine_sl(Sign:slash,Term0,Term1),
	combine_ex(Sign:extra,Term1,Term2),
	combine_sc(Args,Term2,Term).

combine_sc(Var,T0,T) :-
	var(Var),!,T0=T.
combine_sc([],T,T).
combine_sc([H|T],X0,{ X0 # Slashed }) :-
	pr_list([H|T],Slashed).

combine_sl(Var,T0,T) :-
	var(Var),!,T0=T.
combine_sl([],T,T).
combine_sl([H|T],X0,X0+Slashed) :-
	pr_list([H|T],Slashed).

combine_ex(Var,T0,T) :-
	var(Var),!,T0=T.
combine_ex([],T,T).
combine_ex([H|T],X0,X0-Extra) :-
	pr_list([H|T],Extra).

combine_pt(`topic,C0,C0).   % default no mark
combine_pt(`extra,C0,C0).
combine_pt(`left,C0,{\C0}).
combine_pt(`right,C0,{/C0}).

pr_list(Var,V):-
	var(Var),!,
	Var = V.

pr_list([],[]).
pr_list([H|T],[NH|NT]):-
	pr(H,NH),
	pr_list(T,NT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% GET_ARGS for the generator bug_dd.pl %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_args(Node,Args) :-
	general(Node:args,Args0),
	restrictors(Node:sem,Args1),
	wappend(Args0,Args1,Args).

restrictors(`ix,[]).
restrictors(S,R) :-
	restr(S:restr,R).

restr([],[]).
restr([H:sem|T0],[H|T]) :-
	restr(T0,T).

:- block general(-,-).
general([],[]).
general([H0|T0],[H|T]) :-
	unify_except_l(H0,H,{[lex, rel]}),   % because that's what
	                                     % is changed by lexical rules
	general(T0,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% tree-like pretty printing of lexicon set-up %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% to view the lexicon as if it were a real inheritance
%% hierarchy.


call_default(lexical).

call_leaf(a_word(C),L) :-
	C:lex:stem <=> L.

call_leaf(verb_stem(C),L) :-
	C:lex:stem <=> L.

call_leaf(a_noun(C),L) :-
	C:lex:stem <=> L.

call_leaf(r(Rule),L) :-
	find_type(Rule,[L|_]).

call_clause(A,B) :-
	user_clause(A,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% pretty printing of trees            %%%%
%%%% whose labels are feature structures %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shorten_label([H|T],Cat) :-
	shorten_l([H|T],Cat).

shorten_label(FS,Cat) :-
	FS => sign,
	hdrug_flag(shorten,Flag),
	shorten_label(Flag,FS,Cat).

shorten_label(sc_cat,FS,Cat) :-
	FS:cat <=> Cat:cat,
	shorten_l(FS:sc,Cat:sc).

shorten_label(Att,FS,Part) :-
	{  hdrug_feature:e(Att,_,_),
	   FS:Att <=> Part:Att
	}.

shorten_label(short,FS,Cat) :-
	simple_features(Simple),
	unify_simple(Simple,FS,Cat),
	list_features(List),
	unify_list(List,FS,Cat),
	change_features(Change),
	unify_change(Change,FS,Cat),
	sign_features(Sign),
	unify_sign(Sign,FS,Cat). 

%% shorten_label(almost_off,FS,Cat) :-
	

shorten_label(short_sem,FS,Cat) :-
	simple_features(Simple),
	unify_simple(Simple,FS,Cat),
	list_features(List),
	unify_list(List,FS,Cat),
	change_features(Change),
	unify_change(Change,FS,Cat),
	sign_features(Sign),
	unify_sign(Sign,FS,Cat),
	unify_sem(FS,Cat).

shorten_label(off,X,X).

simple_features([]).
list_features([sc,slash,rel,f_slash,f_extra,f_rel]).
change_features([inv,dir,cat]).
sign_features([]).

unify_sem(FS,Cat) :-
	FS:sem <=> Sem0,
	Cat:sem <=> Sem1,
	extern_sem(Sem1,Sem0).

unify_change_att(cat,Type,FType) :-
	find_type(Type,[FType0|_]),
	(FType0 == top -> true ; FType0 = FType ). 
unify_change_att(lex,_ => lexical, +).
unify_change_att(lex,_ => phrasal, -).

unify_change_att(inv,` -,_).
unify_change_att(inv,` +,` +).
unify_change_att(dir,T,_) :-
	T => topic.
unify_change_att(dir,L,left) :-
	L => left.
unify_change_att(dir,R,right) :-
	R => right.
unify_change_att(dir,E,_) :-
	E => extra.

unify_change([],_,_).
unify_change([Att|T],F0,F) :-
	{ F0:Att <=> Val0,
          F:Att <=> Val },
	unify_change_att(Att,Val0,Val),
	unify_change(T,F0,F).


unify_simple([],_,_).
unify_simple([Att|T],F0,F) :-
	{ F0:Att <=> F:Att },
	unify_simple(T,F0,F).

unify_list([],_,_).
unify_list([Att|T],F0,F) :-
	{ F0:Att <=> A0,
	  F:Att <=> A },
        shorten_l(A0,A),
	unify_list(T,F0,F).

unify_sign([],_,_).
unify_sign([Att|T],F0,F) :-
	{ F0:Att <=> A0,
	  F:Att <=> A,
          hdrug_show:shorten_label(A0,A) },
	unify_sign(T,F0,F).

shorten_l(Var,V) :-
	var(Var),!,Var=V.
shorten_l([],_).      % if empty - dont mention it
shorten_l([H0|T0],[H|T]) :-
	{ hdrug_show:shorten_label(H0,H),
	  shorten_l1(T0,T)
	}.

shorten_l1(Var,V) :-
	var(Var),!,Var=V.
shorten_l1([],[]).
shorten_l1([H0|T0],[H|T]) :-
	{ hdrug_show:shorten_label(H0,H),
	  shorten_l1(T0,T)
	}.


:- initialize_flag(shorten,short_sem).
%%% :- initialize_flag(nodeskip,300).
:- initialize_flag(generator,bug_dd).
:- initialize_flag(parser,rcp3).


user_clause(H,B) :-
	templates_clause(H,B).

user_clause(H,B) :-
	rules_clause(H,B).

%%% TK interface

ct(tk,F) :-
	call_tree_bu_tk(F).

ct(latex,F) :-
	call_tree_bu_latex(F).

ct(clig,F) :-
	call_tree_bu_clig(F).

ct(prolog,F) :-
	call_tree_bu(F).


update_unary_preds :-
    {
     format(user_error,"Updating lexical predicates...",[]),
     tcl("set unary_preds(max) 0"),
     findall(Fun,(user_clause(H,_), functor(H,Fun,1)), Preds0),
     sort(Preds0,Preds),
     update_array(Preds,unary_preds),
     format(user_error,"Done.~n",[])
    }.

%%%% procedures for viewing lexical entries, rules and top categories
send_lexs :-
    {
     tcl("set lexs(max) 0",[]),
     format(user_error,"Updating lexical entries...",[]),
     (	 setof(Name,a_lexname(Name),Names)
     ->	 update_array(Names,lexs,0)
     ;	 true
     ),
     format(user_error,"Done.~n",[])
    }.

send_rules :-
    { tcl("set rules(max) 0",[]),
      format(user_error,"Updating rules...",[]),
      (	  setof(Name,a_rulename(Name),Names)
      ->  update_array(Names,rules,0)
      ;	  true
      ),
      format(user_error,"Done.~n",[])
    }.


send_tops :-
    { tcl("set tops(max) 0",[]),
      format(user_error,"Updating topcategories...",[]),
      (	  setof(Name,a_topname(Name),Names)
      ->  update_array(Names,tops,0)
      ;	  true
      ),
      format(user_error,"Done.~n",[])
    }.

a_lexname(Name) :-
	FS:lex:stem ==> Name,
	a_word(FS).

a_topname(Name) :-
	top(Name,_).

a_rulename(Name) :-
	r(X),
	find_type(X,[Name|_]).

a_rule(Name,clause(rule(Name,Mother,Lefties,Head,Righties),[])) :-
	{ Cat => Name },
	Cat:mt <=> Mother,
	Cat:hd <=> Head,
	Cat:ls <=> Lefties,
	Cat:rs <=> Righties,
	r(Cat).

a_top(Name,clause(top(Name,Cat),[])) :-
	top(Name,Cat).

a_lex(Name,clause(lex(Name,Cat),[])) :-
	Cat:lex:stem ==> Name,
	a_word(Cat).

show_lex(Lex,Type,Output) :-
	findall(Clause,a_lex(Lex,Clause),Clauses),
	sh(Type,Output,Clauses).

show_rule(Rule,Type,Output) :-
	findall(Clause,a_rule(Rule,Clause),Clauses),
	sh(Type,Output,Clauses).

show_top(Top,Type,Output) :-
	findall(Clause,a_top(Top,Clause),Clauses),
	sh(Type,Output,Clauses).

start_hook(parse,_,o(_Obj,Str,_),_) :-
	lex_string(Str).

/*
result_hook(parse,PM,o(Obj,_,_),_) :-
	(  hdrug_flag(demo,on)
	-> parser_to_type(PM,Type),
	   show(tree(Type),clig,[value(Obj)])
	;  true
	), 
	if_gui(tcl("update")).

result_hook(generate,_,o(Obj,_,_),_) :-
	(  hdrug_flag(demo,on)
	-> show(tree(dt),clig,[value(Obj)])
	;  true
	),
	if_gui(tcl("update")).
*/

end_hook(parse,PM,_,_):-
    parser_to_type(PM,Type),
    show_object_no(1,tree(Type),clig).

parser_to_type(rcp3_dtrs,d).
parser_to_type(shift_reduce,dt).
parser_to_type(rcp3_d,dt).

parser_to_type(hc_dtrs,d).
parser_to_type(hc_d,dt).

parser_to_type(approx,q).
parser_to_type(right_chart,q).

show_relation(F/A) :-
	show_predicate(F/A,fs,tk).

