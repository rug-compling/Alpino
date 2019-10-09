:- module(lc,[]).

parse(o(Result,Phon,_)) :-
	P0 = 1,
	length(Phon,P1), P is P1+1,
	assertz(table_item:max(P)),
	user:result_cat(Result,Cat),
	user:result_tree(Result,Tree),
	reset_program_space,
	parse(Cat,P0,P,Ref),
	parse_2nd(Ref,Cat,P0,P,Tree).

parse(Cat,P,P,gap(Name)) :-
	lc_in:gap(Cat,Name).
parse(Cat,P0,P,Ref) :-
	\+ table_item:max(P0),
	functor(Cat,Fun,Ar),
	functor(Cat0, Fun,Ar),
	Cat0=..[F|Args],
	MemoCall =.. [F,P0,Ar],
	MemoItem =.. [F,P0,PP|Args],
        (   table_goal:MemoCall  
	->  true             
	;   ( predict(Cat0,P0,Small,P1,His0), 
	      left_corner(Small,Cat0,P1,PP,His),
	      add_item(MemoItem,His0,His,table_item),
	      fail
	    ; assertz(table_goal:MemoCall)
	    ) 
	),
	Cat=Cat0,
	P=PP,
	table_item:clause(MemoItem,_,Ref).

add_item(Item,His0,His,Module) :-
	add_item(Module:Item,IRef),
	IRef='$ref'(IRefA,IRefB),
	(  Module:'MEMO_HIS'(IRefA,IRefB,His0,His)
	-> true
	;  Module:assertz('MEMO_HIS'(IRefA,IRefB,His0,His))
	).

predict(Cat0,P0,Small,P,lex(Ids,P0,P)) :-
	lc_in:left_corner_table_lex(Cat0,Small),
	lex:lex(P0,P,Small,Ids).

left_corner(X,X,Y,Y,[]).
left_corner(Small,Goal,Q0,Q,[rule(Name,Rhis)|His]) :-
	lc_in:lc_rule(Small,Mid,Righties,Name),
	lc_in:left_corner_table(Goal,Mid),
	parse_right_ds(Righties,Q0,Q1,Rhis),
	left_corner(Mid,Goal,Q1,Q,His).

parse_right_ds([],L,L,[]).
parse_right_ds([H|T],L0,L,[Ref|His]):-
	parse(H,L0,L1,Ref),
	parse_right_ds(T,L1,L,His).

clean :-
    (	table_goal:current_predicate(_,X), 
	table_goal:predicate_property(X,dynamic),
	table_goal:retractall(X),
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
	   hdrug_util:count_edges((table_goal:current_predicate(_,X),table_goal:X),Goals),
	   format(user_error,"~w goals~n",[Goals]),
	   hdrug_util:count_edges((table_item:current_predicate(_,Y),
				    functor(Y,F,A),
				    F/A \== 'MEMO_HIS'/4,
				    F/A \== reconstructed/2,
				    F/A \== reconstructed/6,
				    table_item:Y),Items),
	   format(user_error,"~w items~n",[Items]),
	   hdrug_util:report_count_edges(table_item:'MEMO_HIS'(_,_,_,_)),
	   hdrug_util:report_count_edges(table_item:reconstructed(_,_)),
	   hdrug_util:report_count_edges(table_item:reconstructed(_,_,_,_,_,_))
	;  hdrug_util:hdrug_flag(debug,2)
	-> report_program_space,
	   (  format(user_error,"goals: ~n",[]),
	      table_goal:current_predicate(_,Z),
	      hdrug_util:report_count_edges(table_goal:Z),
	      fail
	   ;  format(user_error,"~nitems: ~n",[]),
	      table_item:current_predicate(_,Z),
              hdrug_util:report_count_edges(table_item:Z),
	      fail
	   ;  true
	   )
	;  true
	).

compile_grammar(_):-
	lc_compile:compile_grammar.

dump_grammar:-
	lc_compile:dump_grammar.

parse_2nd(gap(Name),Cat,P,P,tree(Name,_,[])) :-
	lc_in:gap_i(Name,Cat).

parse_2nd('$ref'(Refa,Refb),Cat,P0,P,Tree) :-
	(  table_item:reconstructed(Refa,Refb)
	-> true
	;  table_item:assertz(reconstructed(Refa,Refb)),
	   (  table_item:'MEMO_HIS'(Refa,Refb,Seed,His),
	      predict_2nd(Seed,Small,P0,P1,Tree0),
	      left_corner_2nd(His,Small,Cat0,P1,P,Tree0,Tree),
%%            `expensive' version: (no spurious ambiguities)
%%	      add_item(table_item:reconstructed(Refa,Refb,P0,P,Cat0),_),
%%            `cheap' version: (spurious ambiguities)
	      table_item:assertz(reconstructed(Refa,Refb,P0,P,Cat0,Tree)),
	      fail
	   ;  true
	   )
	),
	table_item:reconstructed(Refa,Refb,P0,P,Cat,Tree).

% second_phase variant of prediction
predict_2nd(lex(Ids,P0,P),Small,P0,P,tree(Id,_,[])) :-
%%	LexRef = '$ref'(Refa,Refb),
%%	lex:total(Refa,Refb,P0,P,Small,Id).
	lists:member(Id,Ids),
	lex_in:total_lex(Id,Small,_,_).

left_corner_2nd([],C,C,P,P,Tr,Tr).
left_corner_2nd([rule(Name,Rhis)|His],Small,Goal,Q0,Q,Tr0,Tr) :-
	lc_in:lc_rule_i(Name,Small,Mid,Rds),
	parse_rds_2nd(Rhis,Rds,Q0,Q1,Tr1),
	left_corner_2nd(His,Mid,Goal,Q1,Q,tree(Name,_,[Tr0|Tr1]),Tr).


parse_rds_2nd([],[],P,P,[]).
parse_rds_2nd([H|T],[Hd|Td],P0,P,[Tr0|Tr]) :-
	parse_2nd(H,Hd,P0,P1,Tr0),
	parse_rds_2nd(T,Td,P1,P,Tr).

a_more_general(Module:Item) :-
	a_more_general(Module:Item,_).

a_more_general(Module:Item,Ref):-
	copy_term(Item,Copy),
	Module:clause(Copy,_,Ref),
	Module:clause(General,_,Ref),
	terms:subsumes_chk(General,Item).

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

