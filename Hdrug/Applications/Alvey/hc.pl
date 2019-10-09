%           -*-Mode: prolog;-*-

:- module(hc,[]).

% goal-weakening must only leave functor symbol!
% heads cannot be gapped! (this is checked).

parse(o(Result,Phon,_)) :-
	\+ unsound,
	P0 = 1,
	length(Phon,P1), P is P1+1,
	user:result_cat(Result,Cat),
	user:result_tree(Result,Tree),
	reset_program_space,
	parse(l,Cat,P0,P,P0,P,Ref),
	parse_2nd(Ref,Cat,P0,P,Tree).

% use this to check if there are gaps that can be head of a rule. If so,
% you have to use a more expensive version of the parser...
unsound :-
	hc_in:gap(Gap,Name),
	hc_in:headed_rule(Gap,_,_,_,Name2),
	hc_in:headed_rule_i(Name2,Gap,_,_,_),
	hc_in:gap_i(Name,Gap),
	format(user_error,
	       "Warning: head-corner parser unsound: ~w as head of ~w~n",
	       [Gap/Name,Name2]).

all_unsound :-
	findall(_,unsound,_).

done(l,F,Ar,P0,E) :-
	Call =.. [F,P0,Right,Ar],
	(  table_goal_l:Call,
	   E =< Right
	-> true
	;  Right=E,
	   assertz(table_goal_l:Call),
	   fail
	).

done(r,F,Ar,E0,P) :-
	Call =.. [F,P,Left,Ar],
	(  table_goal_r:Call,
	   Left =< E0
	-> true
	;  Left=E0,
	   assertz(table_goal_r:Call),
	   fail
	).

parse(_,Gap,P,P,_,_,gap(Name)) :-
	hc_in:gap(Gap,Name).

parse(Eq,Cat,P0,P,E0,E,Ref) :-
	E0 \== E,
	functor(Cat,Fun,Ar),
	functor(Cat0, Fun,Ar),
	Cat0=..[Fun|Args],
	MemoItem =.. [Fun,PP0,PP|Args],
	(  Eq==l -> PP0=E0
	;  Eq==r -> PP=E   
	),
        (   done(Eq,Fun,Ar,E0,E)
	->  true             
	;   ( predict(Cat0,PP0,PP,E0,E,Small,QL,QR,His0), 
	      head_corner(Small,QL,QR,Cat0,PP0,PP,E0,E,His),
	      add_item(MemoItem,His0,His),
	      fail
	    ; true
	    )
	),
	Cat=Cat0,
	P=PP,
	P0=PP0,
	table_item:clause(MemoItem,_,Ref),
	E0 =< P0, 
	 P =< E.  

predict(Cat0,PP0,PP,E0,E,Small,QL,QR,lex(Ids,QL,QR)) :-
	gen_left(QL,E0,E),
	hc_in:head_corner_table_lex(Cat0,PP0,PP,Small,QL,QR),
	lex:lex(QL,QR,Small,Ids),
	QR =< E.  % only makes sense for multi-word units

gen_left(E0,E0,_).
gen_left(Left,E0,E) :-
	E1 is E0+1,
	E1 < E,
	gen_left(Left,E1,E).

head_corner(X,Y,Z,X,Y,Z,_,_,[]).
head_corner(Small,Q0,Q,Goal,P0,P,E0,E,[rule(Name,Lhis,Rhis)|His]) :-
	hc_in:headed_rule(Small,Mid,RevLefties,Righties,Name),
	hc_in:head_corner_table(Goal,P0,P,Mid,QL,QR),
	parse_left_ds(RevLefties,QL,Q0,E0,E,Lhis),
	parse_right_ds(Righties,Q,QR,E0,E,Rhis),
	head_corner(Mid,QL,QR,Goal,P0,P,E0,E,His).

parse_left_ds([],L,L,_,_,[]).
parse_left_ds([H|T],L0,L,E0,E,[Ref|His]):-
	parse(r,H,L1,L,E0,L,Ref),
	parse_left_ds(T,L0,L1,E0,E,His).

parse_right_ds([],L,L,_,_,[]).
parse_right_ds([H|T],L0,L,E0,E,[Ref|His]):-
	parse(l,H,L0,L1,L0,E,Ref),
	parse_right_ds(T,L1,L,E0,E,His).

clean :-
    (	table_goal_l:current_predicate(_,X), 
	table_goal_l:predicate_property(X,dynamic),
	table_goal_l:retractall(X),
	fail
    ;	table_goal_r:current_predicate(_,X), 
	table_goal_r:predicate_property(X,dynamic),
	table_goal_r:retractall(X),
	fail
    ;	table_item:current_predicate(_,X),
	table_item:predicate_property(X,dynamic),
	table_item:retractall(X),
	fail
    ;	true
    ).

count(Z) :-
	program_space(Z).

count :-
	(  hdrug_util:hdrug_flag(debug,1)
	-> report_program_space,
	   hdrug_util:count_edges(
                   ( table_goal_l:current_predicate(_,X),
		     table_goal_l:X;
		     table_goal_r:current_predicate(_,X),
		     table_goal_r:X),Goals),
	   format(user_error,"~w goals~n",[Goals]),
	   hdrug_util:count_edges(
                   ( table_item:current_predicate(_,Y),
		     functor(Y,F,A),
		     F/A \== 'MEMO_HIS'/4,
		     F/A \== reconstructed/2,
		     F/A \== reconstructed/6,
		     table_item:Y
		   ),Items	  ),
	   format(user_error,"~w items~n",[Items]),
	   hdrug_util:report_count_edges(
                   table_item:'MEMO_HIS'(_,_,_,_)),
	   hdrug_util:report_count_edges(
		   table_item:reconstructed(_,_)),
	   hdrug_util:report_count_edges(
                   table_item:reconstructed(_,_,_,_,_,_))
	;  hdrug_util:hdrug_flag(debug,2)
	-> report_program_space,
	   (  format(user_error,"goals: ~n",[]),
	      table_goal_l:current_predicate(_,Z),
	      hdrug_util:report_count_edges(table_goal_l:Z),
	      table_goal_r:current_predicate(_,Z),
	      hdrug_util:report_count_edges(table_goal_r:Z),
	      fail
	   ;  format(user_error,"~nitems: ~n",[]),
	      table_item:current_predicate(_,Z),
              hdrug_util:report_count_edges(table_item:Z),
	      fail
	   ;  true
	   )
	;  true
	).


compile_grammar(_File) :-
	hc_compile:compile_grammar.

parse_2nd(gap(Name),Gap,Q,Q,tree(Name,_,[])) :-
	hc_in:gap_i(Name,Gap).

parse_2nd('$ref'(Refa,Refb),Cat,P0,P,Tree) :-
	(  table_item:reconstructed(Refa,Refb)
	-> true
	;  table_item:assertz(reconstructed(Refa,Refb)),
	   (  table_item:'MEMO_HIS'(Refa,Refb,Seed,His),
	      predict_2nd(Seed,Small,QL,QR,Tree0),
	      head_corner_2nd(His,Small,Cat0,QL,QR,Q0,Q,Tree0,Tree),
	      table_item:assertz(reconstructed(Refa,Refb,Q0,Q,Cat0,Tree)),
	      fail
	   ;  true
	   )
	),
	table_item:reconstructed(Refa,Refb,P0,P,Cat,Tree).

predict_2nd(lex(Ids,P0,P),Small,P0,P,tree(Id,_,[])) :-
%	LexRef = '$ref'(Refa,Refb),
%	lex:total(Refa,Refb,P0,P,Small,Name).
	lists:member(Id,Ids),
	lex_in:total_lex(Id,Small,_,_).

head_corner_2nd([],C,C,P0,P,P0,P,Tree,Tree).
head_corner_2nd([rule(Name,Lhis,Rhis)|His],Small,Goal,P1,P2,Q0,Q,Tree0,Tree) :-
	hc_in:headed_rule_i(Name,Small,Mid,Lds,Rds),
	parse_lds_2nd(Lhis,Lds,P0,P1,[Tree0|Rtree],Tree1),
	parse_rds_2nd(Rhis,Rds,P2,P,Rtree),
	head_corner_2nd(His,Mid,Goal,P0,P,Q0,Q,tree(Name,_,Tree1),Tree).

parse_lds_2nd([],[],P,P,Tr,Tr).
parse_lds_2nd([H|T],[Hd|Td],P0,P,Tr0,Tr) :-
	parse_2nd(H,Hd,P1,P,Tr1),
	parse_lds_2nd(T,Td,P0,P1,[Tr1|Tr0],Tr).

parse_rds_2nd([],[],P,P,[]).
parse_rds_2nd([H|T],[Hd|Td],P0,P,[Tr0|Tr]) :-
	parse_2nd(H,Hd,P0,P1,Tr0),
	parse_rds_2nd(T,Td,P1,P,Tr).

dump_grammar:-
	hc_compile:dump_grammar.

a_more_general(Module:Item,Ref):-
	copy_term(Item,Copy),
	Module:clause(Copy,_,Ref),
	Module:clause(General,_,Ref),
	terms:subsumes_chk(General,Item).

add_item(Item,His0,His) :-
	add_item(table_item:Item,IRef),
	IRef='$ref'(IRefA,IRefB),
	(  table_item:'MEMO_HIS'(IRefA,IRefB,His0,His)
	-> true
	;  table_item:assertz('MEMO_HIS'(IRefA,IRefB,His0,His))
	).

add_item(Module:Item,Ref) :-
	(  a_more_general(Module:Item,Ref)
	-> true
	;  Module:assertz(Item,Ref)
	).


reset_program_space :-
	statistics(program,[X,_]),
	hdrug_util:hdrug_flag(program_space,_,X).

program_space(Used) :-
	statistics(program,[X,_]),
	hdrug_util:hdrug_flag(program_space,Old),
	Used is X-Old.

report_program_space :-
	program_space(Used),
	format(user_error,"used ~w bytes program space~n",[Used]).

