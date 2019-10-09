%% left-corner parser with goal-weakening and memorization. This is an
%% improved (at least for our purposes) version of the parser described
%% in: van Noord, An Efficient Implementation of the Head-Corner Parser,
%% Computational Linguistics, 23(3), 425-456, 1997. The improvements are
%% related to:
%% -- treatment of constraints (for improved functionality)
%% -- compilation (for improved efficiency)
%% -- parse selection and beam search
%%
%% The parser proceeds in two phases. The first phase ignores parse trees
%% and semantics. Full parses are only constructed in the second phase,
%% perhaps interleaved with parse selection.

%% In the first phase, the parser finds all solutions
%% for every *weakened* goal, identified by the dynamic database reference
%% of the dynamic unit clause in module alpino_table_goal.
%%
%% We maintain a list of solutions (per weakened goal) in
%% alpino_table_goal:'RESULTS'(Refa,Refb,List). These solutions are unique (*)
%% (packing!) and are associated each with a list of histories, indicating
%% how the solutions were constructed. These histories are asserted
%% separately in 'MEMO_HIS'(Refa,Refb,N,His) indicating how the Nth solution
%% for the Refa/Refb goal can be constructed. So in the case of packing
%% there are several clauses for a given Refa,Refb,N triple. The assumption
%% here is that semantic constraints in the second phase will distinguish
%% the resulting readings (if not: spurious ambiguity). (**)
%%
%% the solution that is actually used by the remainder of the parse
%% is identified by i(Refa,Refb,N) where N is the position in List. In fact,
%% a history is a left-corner spine with the lexical left-corner, and
%% all dominating rule names. Each non-left-corner daughter is refered to
%% with such an i/3 triple.
%%
%% (*) since solutions can contain constraints, it is actually not
%% guaranteed that they are unique: the effect of the two syntactically
%% different constraints might turn out to be equivalent
%%
%% (**) it might be the case that the same history produces the same
%% result multiple times. These cases are not detected, and will cause
%% spurious ambiguities. This should not happen as long as rule names
%% are unique, and each lexical entry reference produces unique results.
%% But it can happen with constraints (with multiple solutions). Suppose a
%% single history produces two different solutions (both of them compatible
%% with the context), for the same goal. In the
%% first phase, this will give rise to two histories, in the second
%% phase each of them produces the same two results again. Typical
%% example: NP V NP were both NP's are compatible with both subject and
%% object role, e.g. "de man kust de vrouw"
%% really, each derivation should have a different derivation tree - but
%% this is not the case due to constraints (their derivational history
%% is typically not represented - but perhaps it should).
%% Currently, during the second phase of parsing such cases *are* detected,
%% cf the definitions in utils.pl of n_best.

:- module(alpino_lc,[]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(hdrug(hdrug_util)).
:- use_module(utils).

:- dynamic
    alpino_table_item:top_/3,
    alpino_table_goal:'RESULTS'/3,
    alpino_table_item:'MEMO_HIS'/4,
    alpino_table_item:done_2nd/4.

:- thread_local
    alpino_table_item:top_/3,
    alpino_table_goal:'RESULTS'/3,
    alpino_table_item:'MEMO_HIS'/4,
    alpino_table_item:done_2nd/4.

:- public parse/1.

parse(o(Result,Wg,_)) :-
    clean,
    hdrug_flag(use_guides,OnOff),
    (   OnOff == on
    ->  debug_message(1,"Using guided parsing...~n",[])
    ;   true
    ),
    hdrug_flag(robustness,Robust),
    statistics(runtime,[Time0,_]),
    hdrug_flag(parse_candidates_beam,Beam),
    hdrug_flag(require_brackets_balanced,Brackets0),
    adapt_brackets_flag(Brackets0,Brackets),
    init_check_crd,
    parse_upto_robust(Robust,Wg,Beam,OnOff,Brackets),
    bb_put(hdrug_gen_sym:parse_values,0),
    bb_put(hdrug_gen_sym:parse_values2,0),
    statistics(runtime,[Time1,_]),
    Msecs is Time1-Time0,
    debug_message(1,"Parsed: ~w msec~n",[Msecs]),
    report_final_parse(Wg,Result,Time1).

adapt_brackets_flag(Old,New) :-
    (   bracket
    ->  Old = New
    ;   New = none
    ).

bracket :-
    alpino_lexical_analysis:open_bracket(_,_,_).
bracket :-
    alpino_lexical_analysis:close_bracket(_,_,_).


report_final_parse(Wg,Result,_) :-
    alpino_robust:robust(Wg,Result).

report_final_parse(_,_,Time1) :-
    statistics(runtime,[Time2,_]),
    Msecs2 is Time2-Time1,
    debug_message(1,"Found best path: ~w msec~n",[Msecs2]),
    fail.

parse_upto_robust(OnOff,Wg,Beam,Guides,Brackets) :-
    length(Wg,P),
    parse_upto_robust_(OnOff,0,P,Beam,Guides,Brackets).

parse_upto_robust_(off,P0,P,Beam,Guides,Brackets) :-
    parse_upto_robust_p(P0,P,Beam,Guides,Brackets).
parse_upto_robust_(on,P0,P,Beam,Guides,Brackets) :-
    parse_upto_robust_p_anywhere(P0,P,Beam,Guides,Brackets).
parse_upto_robust_(undefined,P0,P,Beam,Guides,Brackets) :- % just to be sure
    parse_upto_robust_(if_required,P0,P,Beam,Guides,Brackets).
parse_upto_robust_(if_required,P0,P,Beam,Guides,Brackets) :-
    parse_upto_robust_p(P0,P,Beam,Guides,Brackets),
    (	alpino_table_item:top_(P0,P,_)
    ->	true
    ;	debug_message(1,
	 "No full parse; attempting to allow top categories anywhere..~n",[]),
	parse_upto_robust_p_anywhere(P0,P,Beam,Guides,Brackets)
    ).

parse_upto_robust_p(P0,P,Beam,Guides,Brackets) :-
    (	alpino_lc_in:parse_top_(P0,P,Ref,Beam,Guides,Brackets),        % HERE
	assertz(alpino_table_item:top_(P0,P,top(Ref))),
	fail
    ;	true
    ).

parse_upto_robust_p_anywhere(P0,P,Beam,Guides,Brackets) :-
    %% instantiate P0, otherwise parse_candidates_beam will only find
    %% parses with low P0
    (   between(P0,P,P1), P1 < P,
        alpino_lc_in:parse_robust_top_(P1,P2,Ref,Beam,Guides,Brackets), % HERE
	assertz(alpino_table_item:top_(P1,P2,robust_top(Ref))),
	fail
    ;	true
    ).

%% used in body of compiled clauses in alpino_lc_in:
:- public parse/10.

:- if(current_predicate(variant_sha1/2)).

parse(Cat,P0,P,i(Index,Key,No,SkipCount),PP,Cat0,McCopy,BmC,Guides,Brackets):-
    variant_sha1(McCopy,Key),
    Index = Key,
    (	alpino_table_goal:'RESULTS'(Index,Key,Items)
    ->	true
    ;	(   findall_atmost(BmC,
	            p(PP,P0,Cat0)-h(His0,His),
		    alpino_lc_in:parse0_(Cat0,P0,PP,His0,His,BmC,Guides,Brackets),
		    Results),
	    add_results(Results,Index,Key,Items),
	    fail		% apparantly, Cat0 is changed by findall?
	;   alpino_table_goal:'RESULTS'(Index,Key,Items)
	)
    ),
    nth(No,Items,f(p(P,P0,Cat),Call)-SkipCount),
    call(Call).

:- else.

parse(Cat,P0,P,i(Index,Ref,No,SkipCount),PP,Cat0,McCopy,BmC,Guides,Brackets):-
    (	goal_exists(McCopy,Index,Ref,Items)
    ->	true
    ;	assertz(alpino_table_goal:McCopy,Ref),
	term_hash(Ref,Index),
	(   findall_atmost(BmC,
	            p(PP,P0,Cat0)-h(His0,His),
		    alpino_lc_in:parse0_(Cat0,P0,PP,His0,His,BmC,Guides,Brackets),
		    Results),
	    add_results(Results,Index,Ref,Items),
	    fail		% apparantly, Cat0 is changed by findall?
	;   alpino_table_goal:'RESULTS'(Index,Ref,Items)
	)
    ),
    nth(No,Items,f(p(P,P0,Cat),Call)-SkipCount),
    call(Call).

goal_exists(Item,Index,Ref,Items) :-
    copy_term(Item,Copy),
    alpino_table_goal:clause(Copy,true,Ref),
    alpino_table_goal:clause(General,true,Ref),
    subsumes_chk(General,Item),
    term_hash(Ref,Index),
    alpino_table_goal:'RESULTS'(Index,Ref,Items).

:- endif.

%% Pairs is a list of p(Cat,P0,P)-HisList
%% this list is packed such that equivalent entries are combined in a
%% single p(Cat,P0,P,Calls) entry with multiple entries in HisList
%% We then assert history entries, and the list of categories
%%
%% Due to constraints, sometimes there are results in Pairs that
%% are not equivalent, but are built with the same history. This gives
%% rise to spurious ambiguity later, so these should be removed. Somehow??
add_results(Pairs0,Refa,Refb,Items) :-
    freeze_const(Pairs0,Pairs1),
    check_equivalence(Pairs1,Pairs2),    % this is where the CPU-time goes
    assert_history_items(Pairs2,Items,1,Refa,Refb),
    alpino_table_goal:assertz('RESULTS'(Refa,Refb,Items)).

freeze_const([],[]).
freeze_const([Item0-His|T0],[f(Item,Cons)-His|T]) :-
    copy_term(Item0,Item,Cons0),
    list_to_conj(Cons0,Cons),
    freeze_const(T0,T).

list_to_conj(L0,Conj) :-
    sort(L0,L1),
    list_to_conj0(L1,Conj).

list_to_conj0([],true).
list_to_conj0([H|T],Conj) :-
    list_to_conj0(T,H,Conj).

list_to_conj0([],C,C).
list_to_conj0([H|T],F,(F,C)):-
    list_to_conj0(T,H,C).

assert_history_items([],[],_,_,_).
assert_history_items([P-HisList0|T0],[P-SkipCount|T],N,Refa,Refb):-
    remove_skips_his(HisList0,HisList,SkipCount),
    alpino_table_item:noclp_assertz('MEMO_HIS'(Refa,Refb,N,HisList)),
    N1 is N+1,
    assert_history_items(T0,T,N1,Refa,Refb).

remove_skips_his(List0,List,SkipCount) :-
    count_skips_his(List0,List1),
    keysort(List1,List2),
    delete_skips_his(List2,List3,SkipCount),
    sort(List3,List).        % this prevents some spurious ambiguities
                             % due to interacting constraints (?)

count_skips_his([],[]).
count_skips_his([Item|Items0],[Count-Item|Items]) :-
    count_skip_his(Item,0,Count),
    count_skips_his(Items0,Items).

count_skip_his(h(Seed,Rules),C0,C) :-
    count_skip_his_seed(Seed,C0,C1),
    count_skip_his_seed_list(Rules,C1,C).

count_skip_his_seed(bracket(_,_,_,_,Seed),C0,C) :-
    count_skip_his_seed(Seed,C0,C).
count_skip_his_seed(nm(Seed,His),C0,C) :-
    count_skip_his_seed(Seed,C0,C1),
    count_skip_his_seed_list(His,C1,C).
count_skip_his_seed(gap(_),C,C).
count_skip_his_seed(call(_),C,C).
count_skip_his_seed(i(_,_,_,Count),C0,C) :-
    C is C0 + Count.
count_skip_his_seed(rule(_Rule,List),C0,C) :-
    count_skip_his_seed_list(List,C0,C).
count_skip_his_seed(lex([Count-_|_List]),C0,C) :-
    C is C0 + Count.
count_skip_his_seed(bracket(_,_),C,C).

count_skip_his_seed_list([],C,C).
count_skip_his_seed_list(bracket(_,_),C,C).
count_skip_his_seed_list([H|T],C0,C):-
    count_skip_his_seed(H,C0,C1),
    count_skip_his_seed_list(T,C1,C).

delete_skips_his([],[],0).
delete_skips_his([Count-Item|Items0],[Item|Items],Count) :-
    delete_skips_his1(Items0,Count,Items).

delete_skips_his1([],_,[]).
delete_skips_his1([Count1-Item|Items0],Count,Items) :-
    (   Count1 > Count
    ->  Items = []
    ;   Items = [Item|Items1],
        delete_skips_his1(Items0,Count,Items1)
    ).

%% merges histories of equivalent items
check_equivalence([],[]).
check_equivalence([H|T],L) :-
    check_equivalence(T,H,L).

check_equivalence([],ITEM-His,[ITEM-[His]]).
check_equivalence([B|T],A,L) :-
    add_frozen_copy([A,B|T],L1),
    keysort(L1,L2),
    check_equivalence_frozen(L2,L).

add_frozen_copy([],[]).
add_frozen_copy([ITEM-His|T0],[FROZEN-g(ITEM,His)|T]) :-
    copy_term(ITEM,FROZEN),
    numbervars(FROZEN,0,_),
    add_frozen_copy(T0,T).

check_equivalence_frozen([],[]).
check_equivalence_frozen([FROZEN-g(ITEM,His0)|T0],[ITEM-His|T]) :-
    check_equivalence_frozen(T0,FROZEN,[His0],His,T).

check_equivalence_frozen([],_,His,His,[]).
check_equivalence_frozen([PREV-g(_ITEM,His0)|T0],PREV,His1,His,T) :-
    !,
    check_equivalence_frozen(T0,PREV,[His0|His1],His,T).
check_equivalence_frozen([NEXT-g(ITEM,His0)|T0],_PREV,H,H,[ITEM-His|T]) :-
    check_equivalence_frozen(T0,NEXT,[His0],His,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% UNPACKING  %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public max_category_first/2, max_category_second/7.

max_category_first(P0,P) :-
    findall(P0-P,alpino_table_item:top_(P0,P,_),List0),
    sort(List0,List),
    member(P0-P,List).

max_category_second(P0,P,Cat,Words,Frames,Tree,Score) :-
    %% ensure P is instantiated, to be able to do fair disambiguation beam
    %% search ....
    %% P is not instantiated in case integrate_robustness_disambiguation=on,
    %% i.e., if 2nd phase is incorporated in robustness search
    findall(P,alpino_table_item:top_(P0,P,_),Ps0),
    sort(Ps0,Ps),
    member(P,Ps),
    hdrug_flag(unpack_bestfirst,Unpack),
    hdrug_flag(disambiguation_beam,Bm),
    hdrug_flag(disambiguation_candidates_beam,BmC),
    debug_message(3,"unpacking ~w-~w~n",[P0,P]),
    parse_2nd_variant(Unpack,Cat,P0,P,Words,Frames,Tree,Bm,BmC,Score).

parse_2nd_variant(on,Cat,P0,P,Words,Frames,Tree,Bm,BmC,Score) :-
    if(unpack_best(Cat,P0,P,Words,Frames,Tree,Bm,BmC,Score),
       true,
       inc_unpack_best(Cat,P0,P,Words,Frames,Tree,Bm,BmC,Score)
      ).
parse_2nd_variant(off,Cat,P0,P,Words,Frames,Tree,_Bm,_BmC,_Score) :-
    hdrug_flag(no_dt_unpack_all,NoDt),
    unpack_all(NoDt,Cat,P0,P,Words,Frames,Tree).

:- initialize_flag(no_dt_unpack_all,off).

inc_unpack_best(Cat,P0,P,Words,Frames,Tree,Bm,BmC,Score) :-
    (  Bm =\= 0
    ;  BmC =\= 0
    ),
    Bm3 is Bm*2,
    BmC3 is BmC*2,
    (   Bm3 > 100
    ->  Bm2 = 0
    ;   Bm2 = Bm3
    ),
    (   BmC3 > 10000
    ->  BmC2 = 0
    ;   BmC2 = BmC3
    ),
    format(user_error,"second phase failed. Trying with ~w ~w~n",[Bm2,BmC2]),
    retractall(alpino_table_item:done_2nd(_,_,_,_)),
    parse_2nd_variant(on,Cat,P0,P,Words,Frames,Tree,Bm2,BmC2,Score).

unpack_all(off,Cat,P0,P,Words,Frames,Tree) :-
    alpino_table_item:top_(P0,P,Ref),
    parse_2nd_all(Ref,Cat,P0,P,Words,[],Frames,[],Tree).

unpack_all(on,Cat,P0,P,Words,Frames,Tree) :-
    alpino_table_item:top_(P0,P,Ref),
    format(user_error,"Warning: remember to re-compile the grammar!~n",[]),
    format(user_error,"Warning: remember to set debug level > 1!~n",[]),
    parse_2nd_all_nodt(Ref,Cat,P0,P,Words,[],Frames,[],Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% UNPACKING: best-first  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpack_best(Cat,P0,P,Words,Frames,Tree,Bm,BmC,Score) :-
    memo_parse_2nd_top(P0,P,Bm,BmC,List),
    member(p(Cat,P0,P,Words,[],Frames,Tree,Score),List).

memo_parse_2nd_top(Q0,Q,Bm,BmC,List) :-
    alpino_table_item:top_(Q0,Q,Ref),
    best_parse_2nd(Ref,List,Bm,BmC).

memo_parse_2nd(nm(Seed,His),Cat,Q0,Q,W0,W,Fr,Tr,Bm,BmC) :-
    predict_2nd(Seed,Small,Q0,Q1,W0,W1,Fr,Fr1,Tr0),
    left_corner_2nd(His,Small,Cat,Q1,Q,W1,W,Fr1,Tr0,Tr,Bm,BmC).

memo_parse_2nd(top(Ref),Cat,Q0,Q,W0,W,Fr,Tr,Bm,BmC) :-
    alpino_lc_in:top_category_(Cat),
    memo_parse_2nd(Ref,Cat,Q0,Q,W0,W,Fr,Tr,Bm,BmC).

memo_parse_2nd(robust_top(Ref),Cat,Q0,Q,W0,W,Fr,Tr,Bm,BmC) :-
    alpino_lc_in:robust_top_category_(Cat),
    memo_parse_2nd(Ref,Cat,Q0,Q,W0,W,Fr,Tr,Bm,BmC).

memo_parse_2nd(call(Abstract),call(Goal),Q,Q,W,W,[],notree(Abstract),_,_) :-
    abstract_call2(Goal,Abstract),
    call(Goal).

memo_parse_2nd(i(A,B,C,_),Cat,P0,P,W0,W,Fr,Tree,Bm,BmC):-
    (	alpino_table_item:done_2nd(A,B,C,List)
    ->	true
    ;	(   best_parse_2nd(i(A,B,C,_),List,Bm,BmC),
	    assertz(alpino_table_item:done_2nd(A,B,C,List)),
	    fail  % get rid of constraints (for swi only?)
	;   alpino_table_item:done_2nd(A,B,C,List)
	)
    ),
    member(p(Cat,P0,P,W0,W,Fr,Tree,_),List).

best_parse_2nd(Ref,List,Bm,BmC):-
    n_best(Bm,Pen,atmost(BmC,
			 eval_parse_2nd(Ref,Cat,Q0,Q,W0,W,Fr,Tree,Pen,Bm,BmC)
			),
	   p(Cat,Q0,Q,W0,W,Fr,Tree,Pen),List
	  ).

eval_parse_2nd(top(Ref),Cat,Q0,Q,W0,W,Fr,Tr,Pen,Bm,BmC) :-
    alpino_lc_in:top_category_(Cat),
    eval_parse_2nd(Ref,Cat,Q0,Q,W0,W,Fr,Tr,Pen,Bm,BmC).

eval_parse_2nd(robust_top(Ref),Cat,Q0,Q,W0,W,Fr,Tr,Pen,Bm,BmC) :-
    alpino_lc_in:robust_top_category_(Cat),
    eval_parse_2nd(Ref,Cat,Q0,Q,W0,W,Fr,Tr,Pen,Bm,BmC).

eval_parse_2nd(i(A,B,C,_),Cat,Q0,Q,W0,W,Fr,Tr,Pen,Bm,BmC) :-
    alpino_table_item:'MEMO_HIS'(A,B,C,List),
    member(h(Seed,His),List),
    predict_2nd(Seed,Small,Q0,Q1,W0,W1,Fr,Fr1,Tr0),
    left_corner_2nd(His,Small,Cat,Q1,Q,W1,W,Fr1,Tr0,Tr,Bm,BmC),
    penalties(Cat,Tr,Fr,Pen).

predict_2nd(bracket(P0,P1,P2,P,His),Small,P0,P,Wds0,Wds,Frs0,Frs,Tree) :-
    predict_2nd(His,Small,P1,P2,Wds0,Wds,Frs0,Frs,Tree).

predict_2nd(lex(List),Small,P0,P,[W|Wds],Wds,[Frame|Frs],Frs,
	    tree(Small,W,lex(LexRef),_)) :-
    alpino_lexical_analysis:get_lref_list(List,W,LexRef,P0,P,Small,Frame).

predict_2nd(gap(Name),Small,Q,Q,Ws,Ws,Frs,Frs,tree(Small,Name2,[],_)) :-
    alpino_lc_in:gap_i_(Name,Name2,Small).

predict_2nd(call(Abstract),call(CallGoal),Q,Q,Ws,Ws,Frs,Frs,notree(Abstract)) :-
    abstract_call2(CallGoal,Abstract),
    call(CallGoal).

left_corner_2nd([],C,C,P,P,W,W,[],Tr,Tr,_,_).
left_corner_2nd([rule(Name,Rhis)|His],Small,Goal,Q0,Q,W0,W,Frs,
		Tr0,Tr,Bm,BmC):-
    alpino_lc_in:lc_rule_i_(Name,Small,Mid,Rds,Name2),
    parse_rds_2nd(Rhis,Rds,Q0,Q1,W0,W1,Frs,Frs1,Trs,Bm,BmC),
    trees(Tr0,Trs,TrDs),
    left_corner_2nd(His,Mid,Goal,Q1,Q,W1,W,
		    Frs1,tree(Mid,Name2,TrDs,_),Tr,Bm,BmC).

parse_rds_2nd([],[],P,P,W,W,Frs,Frs,[],_,_).
parse_rds_2nd(bracket(P0,P),[],P0,P,W,W,Frs,Frs,[],_,_).
parse_rds_2nd([H|T],[Hd|Td],P0,P,W0,W,Frs0,Frs,Tr,B,BmC) :-
    memo_parse_2nd(H,Hd,P0,P1,W0,W1,FrsX,Tr0,B,BmC),
    trees(Tr0,Tr1,Tr),
    lists:append(FrsX,Frs1,Frs0),
    parse_rds_2nd(T,Td,P1,P,W1,W,Frs1,Frs,Tr1,B,BmC).

:- initialize_flag(keep_notree,off).

trees(notree(Call),Tr0,Tr) :-
    hdrug_flag(keep_notree,Keep),
    trees(Keep,Call,Tr0,Tr).
trees(tree(A,B,C,D),TrT,[tree(A,B,C,D)|TrT]).

trees(off,_,Tr,Tr).
trees(on,Call,TrT,[tree(Call,call(F),[],_)|TrT]) :-
    functor(Call,F,_A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% UNPACKING: all %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_2nd_all(nm(Seed,His),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    predict_2nd(Seed,Small,Q0,Q1,Wds0,Wds1,Frs0,Frs1,Tr0),
    left_corner_2nd_all(His,Small,Cat,Q1,Q,Wds1,Wds,Frs1,Frs,Tr0,Tr).

parse_2nd_all(top(Ref),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    alpino_lc_in:top_category_(Cat),
    parse_2nd_all(Ref,Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr).

parse_2nd_all(robust_top(Ref),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    alpino_lc_in:robust_top_category_(Cat),
    parse_2nd_all(Ref,Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr).

parse_2nd_all(call(Abstract),call(Goal),Q,Q,W,W,Fr,Fr,notree(Abstract)) :-
    abstract_call2(Goal,Abstract),
    call(Goal).

parse_2nd_all(i(A,B,C,_),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    alpino_table_item:'MEMO_HIS'(A,B,C,List),
    member(h(Seed,His),List),
    predict_2nd(Seed,Small,Q0,Q1,Wds0,Wds1,Frs0,Frs1,Tr0),
    left_corner_2nd_all(His,Small,Cat,Q1,Q,Wds1,Wds,Frs1,Frs,Tr0,Tr).

left_corner_2nd_all([],C,C,P,P,W,W,Frs,Frs,Tr,Tr).
left_corner_2nd_all([rule(Name,Rhis)|His],Small,Goal,Q0,Q,W0,W,Frs0,Frs,
		    Tr0,Tr):-
    alpino_lc_in:lc_rule_i_(Name,Small,Mid,Rds,Name2),
    parse_rds_2nd_all(Rhis,Rds,Q0,Q1,W0,W1,Frs0,Frs1,Trs),
    trees(Tr0,Trs,TrDs),
    left_corner_2nd_all(His,Mid,Goal,Q1,Q,W1,W,
			Frs1,Frs,tree(Mid,Name2,TrDs,_),Tr).

parse_rds_2nd_all([],[],P,P,W,W,Frs,Frs,[]).
parse_rds_2nd_all(bracket(P0,P),[],P0,P,W,W,Frs,Frs,[]).
parse_rds_2nd_all([H|T],[Hd|Td],P0,P,W0,W,Frs0,Frs,Tr) :-
    parse_2nd_all(H,Hd,P0,P1,W0,W1,Frs0,Frs1,Tr0),
    trees(Tr0,Tr1,Tr),
    parse_rds_2nd_all(T,Td,P1,P,W1,W,Frs1,Frs,Tr1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% UNPACKING: all - DT %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% For debugging purposes only, to see where dt
%% is filtering derivations; therefore no memo-ing either.

parse_2nd_all_nodt(nm(Seed,His),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    predict_2nd_nodt(Seed,Small,Q0,Q1,Wds0,Wds1,Frs0,Frs1,Tr0),
    left_corner_2nd_all_nodt(His,Small,Cat,Q1,Q,Wds1,Wds,Frs1,Frs,Tr0,Tr).

parse_2nd_all_nodt(top(Ref),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    alpino_lc_in:top_category_(Cat),
    parse_2nd_all_nodt(Ref,Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr).

parse_2nd_all_nodt(call(Abstract),call(Goal),Q,Q,W,W,Fr,Fr,notree(Abstract)) :-
    abstract_call2(Goal,Abstract),
    call(Goal).

parse_2nd_all_nodt(robust_top(Ref),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    alpino_lc_in:robust_top_category_(Cat),
    parse_2nd_all_nodt(Ref,Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr).

parse_2nd_all_nodt(i(A,B,C,_),Cat,Q0,Q,Wds0,Wds,Frs0,Frs,Tr) :-
    alpino_table_item:'MEMO_HIS'(A,B,C,List),
    member(h(Seed,His),List),
    predict_2nd_nodt(Seed,Small,Q0,Q1,Wds0,Wds1,Frs0,Frs1,Tr0),
    left_corner_2nd_all_nodt(His,Small,Cat,Q1,Q,Wds1,Wds,Frs1,Frs,Tr0,Tr).

left_corner_2nd_all_nodt([],C,C,P,P,W,W,Frs,Frs,Tr,Tr).
left_corner_2nd_all_nodt([rule(Name,Rhis)|His],Small,Goal,Q0,Q,W0,W,Frs0,Frs,
		    Tr0,Tr):-
    alpino_lc_in:lc_rule_(Small,Mid,Rds,Name,_),
    parse_rds_2nd_all_nodt(Rhis,Rds,Q0,Q1,W0,W1,Frs0,Frs1,Trs),
    trees(Tr0,Trs,TrDs),
    left_corner_2nd_all_nodt(His,Mid,Goal,Q1,Q,W1,W,
			Frs1,Frs,tree(Mid,Name,TrDs,_),Tr).

parse_rds_2nd_all_nodt([],[],P,P,W,W,Frs,Frs,[]).
parse_rds_2nd_all_nodt(bracket(P0,P),[],P0,P,W,W,Frs,Frs,[]).
parse_rds_2nd_all_nodt([H|T],[Hd|Td],P0,P,W0,W,Frs0,Frs,Tr) :-
    parse_2nd_all_nodt(H,Hd,P0,P1,W0,W1,Frs0,Frs1,Tr0),
    trees(Tr0,Tr1,Tr),
    parse_rds_2nd_all_nodt(T,Td,P1,P,W1,W,Frs1,Frs,Tr1).

predict_2nd_nodt(bracket(P0,P1,P2,P,His),Small,P0,P,W0,W1,Fr0,Fr1,Tr0) :-
    predict_2nd_nodt(His,Small,P1,P2,W0,W1,Fr0,Fr1,Tr0).

predict_2nd_nodt(lex(List),Small,P0,P,[W|Wds],Wds,[Frame|Frs],Frs,
	    tree(Small,W,lex(LexRef),_)) :-
    member(_-m(W,LexRef),List),
    alpino_lexical_analysis:get_lref(LexRef,P0,P,Small0,Frame),
    alpino_data:separate(Small0,Small). % remove sem

predict_2nd_nodt(gap(Name),Small,Q,Q,Ws,Ws,Frs,Frs,tree(Small,Name,[],_)) :-
    alpino_lc_in:gap_(Small,Name,_).

predict_2nd_nodt(call(Abstract),call(CallGoal),Q,Q,Ws,Ws,Frs,Frs,notree(Abstract)) :-
    abstract_call2(CallGoal,Abstract),
    call(CallGoal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% clean and count %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public clean/0, count/0, count/1.

clean :-
    retractall_items(alpino_parse_values),
    bb_put(hdrug_gen_sym:parse_values,0),
    bb_put(hdrug_gen_sym:parse_values2,0),
    retractall_items(alpino_table_goal),
    retractall_items(alpino_table_item).

count(C):-
    count_history_items(C).

count_history_items(C) :-
    catch(findall(_,alpino_table_item:'MEMO_HIS'(_,_,_,_),L),
	  resource_error(_,memory),
	  L=[]
	 ),
    length(L,C).

/*
count_goals(C) :-
    on_exception(
		 resource_error(_,memory),
		 findall(_,alpino_table_goal:'RESULTS'(_,_,_),L),
		 L=[]
		),
    length(L,C).
*/

count :-
    debug_call(1,count_).

count_ :-
    count_history_items(C),
    format(user_error,"~w history items~n",[C]).

:- public count_goals/0.
count_goals :-
    (   findall(Pred,clause_predicate_spec(alpino_table_goal,Pred),List0),
        sort(List0,List),
        member(Pred,List),
        report_count_edges_pred(alpino_table_goal:Pred),
        fail
    ;   true
    ).

:- public analyse_items/0, report_items/2.

analyse_items :-
    (   alpino_table_goal:'RESULTS'(_,Ref,List),
        instance(Ref,(Head:-_)),
        length(List,Len),
        Len > 0,
        Head =.. [Fun,Pos|_],
        format(user_error,"~w solutions, more details with alpino_lc:report_items(~w,~w,~q).~n",[Len,Fun,Pos,Ref]),
        fail
    ;   true
    ).

report_items(Fun,Pos) :-
    report_items(Fun,Pos,_).

report_items(Fun,Pos,Ref) :-
    (   alpino_table_goal:'RESULTS'(_,Ref,List),
	instance(Ref,(Head:-_)),
	Head =.. [Fun,Pos|_],
	List=[_|_], 
	member(El,List),
	hdrug_util:prettyvars(El),
	write(El),
	nl,
	fail
    ;   true
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% compilation %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% not checked for threads. In practice irrelevant, because this part of
%% the code is only used during grammar compilation.

:- public compile_grammar/0.

:- dynamic
    cons_clause/2.

%% only those that are *not* dumped into grammar_lc.pl
:- dynamic
    alpino_lc_in:lc_rule_/5,
    alpino_lc_in:gap_/3,
    alpino_lc_in:gap_i_/3,
    alpino_lc_in:parse_/7.

compile_grammar :-
    format(user_error,"Compiling grammar...~n",[]),
    remember_rule_ids(Ids,GIds),
    abolish_items(alpino_lc_in),
    retractall(cons_clause(_,_)),
    assertz(alpino_lc_in:unknown_predicate_handler(_,fail)),
    compile_rules,
    check_compiled_rules(Ids),
    check_compiled_rules_g(GIds),
%    check_rules_succeed,
    check_rules_unique,
    debug_call(1,tab(user_error,3)),
    report_count_edges_pred(alpino_lc_in:grammar_rule/3),
    compile_rules_g,
    debug_call(1,tab(user_error,3)),
    report_count_edges_pred(alpino_lc_in:grammar_rule_g/3),
    format(user_error,"   compiling parse goals..~n",[]),
    goal_lc_fas(Cats),
    goal_fas(Goals),
    parse_clauses(Goals),
    format(user_error,"   compiling left-corner table..~n",[]),
    compile_weaken_links(Cats),
    left_corner_table,
    format(user_error,"   generalizing left-corner table..~n",[]),
    generalize_left_corner_table(Cats,Goals),
    format(user_error,"   compiling predict clauses..~n",[]),
    predict_clauses(Goals),
    format(user_error,"   compiling left-corner clauses..~n",[]),
    c_left_corner(Goals),
    c_left_corner_rules(Goals),    
    format(user_error,"   compiling top..~n",[]),
    compile_top_category,
    compile_extra_predicates,
    format(user_error,"   cleanup..~n",[]),
    hdrug_flag(no_dt_unpack_all,V),
    hdrug_flag(debug,D),
    (	V == on
    ->	true
    ;	(   D < 2
	->  retractall(alpino_lc_in:lc_rule_(_,_,_,_,_)),
	    retractall(alpino_lc_in:gap_(_,_,_))
	;   true
	)
    ),
    (	D < 2
    ->	retractall(alpino_util_tmp:pair(_,_)),
	retractall(alpino_util_tmp:bpair(_,_)),
	retractall(alpino_lc_in:parse_(_,_,_,_,_,_,_)),
	retractall(alpino_lc_in:generalized_pair(_,_)),
	retractall(alpino_lc_in:weaken_link_cat(_,_))
    ;	true
    ),
    format(user_error,"Compiled grammar for parser lc~n",[]).

remember_rule_ids(Ids,GIds) :-
    findall(Id,try_hook(alpino_lc_in:grammar_rule(Id,_,_)),Ids),
    findall(GId,try_hook(alpino_lc_in:grammar_rule_g(GId,_,_)),GIds).

goal_lc_fas(Goals):-
    findall(F/A,goal_lc_fa(F,A),Goals0),
    sort(Goals0,Goals).

goal_fas(Goals):-
    findall(F/A,goal_fa(F,A),Goals0),
    sort(Goals0,Goals).

check_compiled_rules(Ids) :-
    (   member(Id,Ids),
        \+ alpino_grammar:grammar_rule(Id,_,_),
        format(user_error,"warning: previous rule ~w not created!~n",[Id]),
        fail
    ;   true
    ).

check_compiled_rules_g(Ids) :-
    (   member(Id,Ids),
        \+ alpino_grammar:grammar_rule(Id,_,_),
        format(user_error,"warning: previous rule ~w not created!~n",[Id]),
        fail
    ;   true
    ).

% check_rules_succeed :-
%     (   clause(alpino_grammar:grammar_rule(Id,_,_),_Body,_Ref),
%         \+ alpino_grammar:grammar_rule(Id,_,_),
%         format(user_error,"warning: compilation of rule ~w failed~n",[Id]),
%         fail
%     ;   true
%     ).

check_rules_unique :-
    findall(Id,alpino_grammar:grammar_rule(Id,_,_),Ids0),
    sort(Ids0,Ids),
    check_rules(Ids).

check_rules([]).
check_rules([H|T]) :-
    check_rule(H),
    check_rules(T).

check_rule(H) :-
    findall(H,alpino_grammar:grammar_rule(H,_,_),List),
    length(List,Len),
    (    Len =:= 1
    ->   true
    ;    format(user_error,"error: rule ~w succeeds ~w times~n",[H,Len])
    ).

compile_extra_predicates :-
    (	hook(alpino_grammar:compile_predicate(F/A)),
	alpino_lc_in:abolish(F/A),
	functor(Goal,F,A),
	(   alpino_grammar:call(Goal),
	    assertz(alpino_lc_in:Goal),
	    debug_message(2,"~w~n",[alpino_lc_in:F/A]),
	    fail
	;   report_count_edges_pred(alpino_lc_in:F/A)
	),
	fail
    ;	true
    ).

goal_fa(F,A) :-
    goal_cat(Cat),
    functor(Cat,F,A).

goal_cat(Cat):-
    alpino_grammar:top_category(Cat).

goal_cat(Cat):-
    alpino_grammar:robust_top_category(Cat).

goal_cat(Cat) :-
    alpino_lc_in:lc_rule_(_H,_M,Rs,_,_),
    member(Cat,Rs).

goal_lc_fa(F,A) :-
    goal_lc_cat(Cat),
    functor(Cat,F,A).

goal_lc_cat(Cat) :-
    goal_cat(Cat).

goal_lc_cat(M) :-
    alpino_lc_in:lc_rule_(_H,M,_Rs,_,_).    

goal_lc_cat(H) :-
    alpino_lc_in:lc_rule_(H,_M,_Rs,_,_).    

compile_top_category :-
    (	alpino_grammar:top_category(Cat),
	assertz(alpino_lc_in:top_category_(Cat)),
	Cat =.. [F|Args],
	Item =.. [F,_,_,P0,P|Args],
	assertz(alpino_lc_in:top_category_(Cat,P0,P,Item)),
	cons_clause(parse_(Cat,P0,P,Ref,Beam,Guides,Brackets),Body),
	assertz((alpino_lc_in:parse_top_(P0,P,Ref,Beam,Guides,Brackets) :- Body)),
	fail
    ;	alpino_grammar:robust_top_category(Cat),
	assertz(alpino_lc_in:robust_top_category_(Cat)),
	Cat =.. [F|Args],
	Item =.. [F,_,_,P0,P|Args],
	assertz(alpino_lc_in:robust_top_category_(Cat,P0,P,Item)),
	cons_clause(parse_(Cat,P0,P,Ref,Beam,Guides,Brackets),Body),
	assertz((alpino_lc_in:parse_robust_top_(P0,P,Ref,Beam,Guides,Brackets) :- Body)),
	fail
    ;   true
    ).


compile_rules :-
    set_flag(parse_or_generate,parse),
    (	grammar__grammar_rule(Id,TermId,M,Ds),
	assertz(alpino_lc_in:grammar_rule(TermId,M,Ds)),   %% for debug/visualization

	debug_message(2,"Compiling rule ~w~n",[TermId]),

	alpino_data:separate(M,SynM),
	separate_list(Ds,SynDs),
	compile_lcr(Ds,M,Id,TermId,SynM,SynDs,Rule1,Rule2),
	assertz(alpino_lc_in:Rule1),
	assertz(alpino_lc_in:Rule2),
	fail
    ;   true
    ).

compile_rules_g :-
    set_flag(parse_or_generate,generate),
    (   grammar__grammar_rule(_Id,TermId,M,Ds),
	assertz(alpino_lc_in:grammar_rule_g(TermId,M,Ds)),   %% for debug/visualization
	fail
    ;   true
    ).

grammar__grammar_rule(Id,TryId,M,Ds) :-
    findall(IdT,alpino_grammar:grammar_rule(IdT,_,_),ListIds0),
    sort(ListIds0,ListIds),
    member(TryId,ListIds),
%    frequent_rule(TryId),
    timegr(alpino_grammar:grammar_rule(TryId,M,Ds),TryId),
    term_atom(TryId,Id).

timegr(Module:Goal,Id) :-
    hdrug_flag(debug,Debug),
    (   Debug > 0
    ->  statistics(runtime,[Tstart,_]),
	call(Module:Goal),
	statistics(runtime,[Tend,_]),
	Time is Tend-Tstart,
	(   Time > 20
	->  format(user_error,"rule ~w ~w msec~n",[Id,Time])
	;   true
	)
    ;   call(Module:Goal)
    ).

separate_list([],[]).
separate_list([H0|T0],[H|T]) :-
    alpino_data:separate(H0,H),
    separate_list(T0,T).

% compile_lcr(+Ds,+M,+Id,+M2,+Ds2,-Rule1,-Rule2)
% no daughters: gap
compile_lcr([],M,Name,Name2,SynM,[],gap_(SynM,Name,Name2),gap_i_(Name,Name2,M)).
compile_lcr([D0|Ds],M,Id,Id2,SynM,[SynD0|SynDs],
                              lc_rule_(SynD0,SynM,SynDs,Id,Id2),
	                      lc_rule_i_(Id,D0,M,Ds,Id2)).

left_corner_table :-
    findall(Head-Mother,head_mother_rule_no_constraints(Head,Mother),Pairs),
    transitive(Pairs).

head_mother_rule_no_constraints(Head,Mother) :-
    alpino_lc_in:lc_rule_(H,M,_,_,_),
    copy_term(H/M,Head/Mother,_).


generalize_left_corner_table(Cats,Functors) :-
    %% for each pair of functor, find the generalization of all
    %% info in lc. With indexing, now.
    (	member(A/B,Functors),
	member(C/D,Cats),
	generalize_pair(A,B,C,D),
	fail
    ;   true
    ).

generalize_pair(F,A,F2,A2) :-
    functor(Term0,F,A),
    functor(Term,F2,A2),
    generalize_pair(Term0,Term),
    \+ \+ (   prettyvars(link(Term0,Term)),
	      debug_message(2,"link(~w,~w).~n",[Term0,Term])
	  ),
    link_functor(Term,Term0,NewTerm),
    assertz(alpino_lc_in:NewTerm).

generalize_pair(Term0,Term) :-
    findall(left_corner_table(Term0,Term),an_lc(Term0,Term),List0),
    generalize_term(List0,GeneralTerm),
    GeneralTerm=left_corner_table(Term0,Term),
    assertz(alpino_lc_in:generalized_pair(Term0,Term)).

%% lc(X,Y)  waarbij indexing op Y (altijd nonvar)
%% e.g. lc(f(A,B,C),g(D,E,F,G,H)) wordt lc_g(f(A,B,C),D,E,F,G,H)
link_functor(Arg1,Arg2,Term):-
    Arg2 =.. [F|Arg2s],
    atom_concat(link_,F,Fun),
    Term =.. [Fun,Arg1|Arg2s].

an_lc(Term0,Term0).
an_lc(Term,Term0) :-
    alpino_util_tmp:pair(Term0,Term).

generalize_term([H|T],Term) :-
    generalize_term0(T,H,Term).

generalize_term0([],Term,Term).
generalize_term0([H|T],Term0,Term) :-
    term_subsumer(H,Term0,Term1),
    generalize_term0(T,Term1,Term).

:- if(current_prolog_flag(dialect,swi)).

predict_clauses([]) :-
    alpino_lc_in:assertz((

	predict_(A,B,C,D,E,F,_G) :-
	    alpino_lexical_analysis:syn_lex_analysis(A,B,C,D,E,F)

			 )).

predict_clauses([F/A|Tail]) :-
    functor(Arg1,F,A),
    link_functor(Arg2,Arg1,Term),
    Refs0=[_-m(_,ref(_TagClass,_Tag,_,_,_,_,_,_,_,_LexNr,Tag2))|_],
    (	\+ alpino_lc_in:Term
    ->	true
    ;   alpino_lc_in:noclp_assertz((

	  create_lex(P0,P,Arg2,Refs0,Guides,
		         syn_lex_analysis(Arg1,P0,Arg2,P,lex(Refs0),lex(Tag2))) :-
		Term,
		alpino_lc:check_predict(Guides,F,lex(Tag2))
			     
				   )),
	(   alpino_lc_in:gap_(Arg2,Name,Name2),
	    alpino_lc_in:generalized_pair(Arg1,Arg2),
	    alpino_lc_in:noclp_assertz(( 

                predict_(Arg1,P,Arg2,P,gap(Name),gap(Name2),Guides):-
		     alpino_lc:check_predict(Guides,F,gap(Name2))
	  
				       )),
	    fail
	;   true
	),
	(   Arg2 = call(CallGoal),
            alpino_lc_in:generalized_pair(Arg1,Arg2),
            abstract_call(CallGoal,Abstract),
            functor(Abstract,CallF,_),
	    alpino_lc_in:assertz((

                    predict_(Arg1,P,Arg2,P,call(Abstract),CallF,Guides):-
                          alpino_lc:check_predict(Guides,F,CallF),
                          alpino_lc:CallGoal

				 )),
	    fail
	;   true
	)
    ),
    predict_clauses(Tail).

:- else.

predict_clauses([]).
predict_clauses([F/A|Tail]) :-
    functor(Arg1,F,A),
    link_functor(Arg2,Arg1,Term),
    Refs0=[_-m(_,ref(_TagClass,_Tag,_,_,_,_,_,_,_,_LexNr,Tag2))|_],
    (	\+ alpino_lc_in:Term
    ->	true
    ;   alpino_lc_in:assertz((

             predict_(Arg1,P0,Arg2,P,lex(Refs0),lex(Tag2),Guides):-
	         alpino_lexical_analysis:syn_lex_analysis(P0,P,Arg2,Refs0),
	         Term,
	         alpino_lc:check_predict(Guides,F,lex(Tag2))
			     
			     )),
	(   alpino_lc_in:gap_(Arg2,Name,Name2),
	    alpino_lc_in:generalized_pair(Arg1,Arg2),
	    alpino_lc_in:assertz((

               predict_(Arg1,P,Arg2,P,gap(Name),gap(Name2),Guides):-
		   alpino_lc:check_predict(Guides,F,gap(Name2))

				 )),
	    fail
	;   true
	),
	(   Arg2 = call(CallGoal),
            alpino_lc_in:generalized_pair(Arg1,Arg2),
            abstract_call(CallGoal,Abstract),
	    functor(Abstract,CallF,_),
	    alpino_lc_in:assertz((

                    predict_(Arg1,P,Arg2,P,call(Abstract),CallF,Guides):-
                          alpino_lc:check_predict(Guides,F,CallF),
                          alpino_lc:CallGoal

				 )),
	    fail
	;   true
				  
	)
    ),
    predict_clauses(Tail).

:- endif.


parse_clauses([]).
parse_clauses([F/A|Goals]) :-
    parse_clause(F,A),
    parse_clauses(Goals).

parse_clause(F,A) :-
    functor(Cat,F,A),
    (   Cat=call(CallGoal)
    ->  assert_w( parse_(call(CallGoal),P,P,call(Abstract),_,_,_),
                  ( alpino_lc:abstract_call(CallGoal,Abstract),
                    call(alpino_lc:CallGoal)
                  ))
    ;	\+ no_memo(Cat)
    ->	functor(Cat0,F,A),
	Cat0=..[F|Args],
	MemoCall=..[F,P0|Args],
	MemoCall2=..[F,P0],  %% contents completely weakened away...
	A2 is A+2, functor(Copy,F,A2), %% arg(A2,Copy,Cs),
	Copy =.. [F|List], append(PreList,[Cs],List),
	Copy1 =.. [F|PreList],
	(   alpino_grammar:weaken(Cat,Cat0)
	->  assert_w(parse_(Cat,P0,P,Ref,Beam,Guides,Brackets),
			 (   hdrug_util:copy_term(MemoCall,Copy1,Cs),
			     alpino_lc:parse(Cat,P0,P,Ref,_,Cat0,Copy,Beam,Guides,Brackets)
			 ))
	;   assert_w(parse_(Cat,P0,P,Ref,Beam,Guides,Brackets),
	                  alpino_lc:parse(Cat,P0,P,Ref,_,Cat0,MemoCall2,Beam,Guides,Brackets)
			)
	),

	functor(Goal,F,A),
	lc_functor(Goal,Small,X2,X,H1,BrStack,[Name2],Beam,Guides,Brackets,LC),
	alpino_lc_in:assertz((
	   parse0_(Goal,X0,X,bracket(X0,X1,X20,X2,H0),H1,Beam,Guides,Brackets):-
	       alpino_lc:open_brackets(Brackets,X0,X1,BrStack0),
	       predict_(Goal,X1,Small,X20,H0,Name2,Guides),
	       alpino_lc:close_brackets(Brackets,X20,X2,Small,BrStack0,BrStack,_),
	       LC
		      ))
    ;   lc_functor(Cat,Small,P2,P,H,BrStack,[Name2],Beam,Guides,Brackets,LC),
	assert_w(parse_(Cat,P0,P,nm(bracket(P0,P1,P20,P2,H0),H),Beam,Guides,Brackets),
		 (   alpino_lc:open_brackets(Brackets,P0,P1,BrStack0),
		     predict_(Cat,P1,Small,P20,H0,Name2,Guides),
		     alpino_lc:close_brackets(Brackets,P20,P2,Small,BrStack0,BrStack,_),
		     LC
		 ))
    ).


assert_w(Head,Body) :-
    alpino_lc_in:assertz((Head :- Body)),
    assertz(cons_clause(Head,Body)).


:- public open_brackets/4, close_brackets/7.

open_brackets(Flag,P0,P,List) :-
    (   Flag == none
    ->  P0 = P,
        List = []
    ;   open_brackets_(P0,P,List,[])
    ).

open_brackets_(P0,P,Brs,T) :-
    alpino_lexical_analysis:open_bracket(P0,P1,H),
    open_brackets_(P1,P,Brs,[H|T]).
open_brackets_(P,P,Brs,Brs).

lc_functor(Goal,Small,X1,X,H1,BrStack,Path,Beam,Guides,Brackets,LC) :-
    Goal =.. [F|Gs],
    atom_concat(lc_,F,F2),
    LC =.. [F2,Small,X1,X,H1,BrStack,Path,Beam,Guides,Brackets|Gs].

c_left_corner_rules(Goals) :-
    (	member(F/A,Goals),
	functor(Goal,F,A),
	alpino_lc_in:generalized_pair(Goal,Mid),
	alpino_lc_in:lc_rule_(Small,Mid,Righties,Name,Name2),
	lc_functor(Goal,Small,Q0,Q,[rule(Name,Rhis)|His],Brs0,PREV,
                   Beam,Guides,Brackets,LC1),
	lc_functor(Goal,Mid,Q1,Q,His,Brs,[Name2|PREV],
                   Beam,Guides,Brackets,LC3),
	body(Righties,Q0,Q1,Rhis,Brs0,Brs,Mid,Body,LC3,[],Beam,Guides,Brackets),
	assert_lc_clause(LC1,Body,alpino_lc:check_connect(Guides,F,[Name2|PREV])),
	fail
    ;   true
    ).

%% ensure that Goal is called before any constraints
assert_lc_clause(Head0,Body0,Goal0) :-
    copy_term((Head0,Body0,Goal0),(Head,Body,Goal),Cs),
    add_constraints(Cs,Body,CsBody),
    alpino_lc_in:noclp_assertz((Head :- (Goal,CsBody))).

body([],Q0,Q,His,Brs0,Brs,G,Body,LC,Cs,_Beam,_Guides,Brackets) :-
    functor(G,Fun,_),
    add_constraints([alpino_lc:close_brackets_f(Brackets,Q0,Q,Fun,Brs0,Brs,His)|Cs],LC,Body).
body([C|Cs],Q0,Q,[H|Hs],Brs0,Brs,G,(Body1,Body),LC,Cons,Beam,Guides,Brackets):-
    cons_clause(parse_(C,Q0,Q1,H,Beam,Guides,Brackets),Body1),
    body(Cs,Q1,Q,Hs,Brs0,Brs,G,Body,LC,Cons,Beam,Guides,Brackets).

add_constraints([],B,B).
add_constraints([H|T],LC,(H,Rest)):-
    add_constraints(T,LC,Rest).

c_left_corner(Goals) :-
    (	member(F/A,Goals),
	functor(G,F,A),
	lc_functor(G,G,P,P,[],[],PREV,_Beam,Guides,_Brackets,LC),
	alpino_lc_in:noclp_assertz(
		(LC:- alpino_lc:check_connect(Guides,F,[finish|PREV]))),
	fail
    ;   true
    ).


:- public close_brackets_f/7.
close_brackets_f(none,Q,Q,_,Brs,Brs,[]).

close_brackets_f(on,Q0,Q,Fun,[Fun|Brs],Brs,bracket(Q0,Q)) :-
    alpino_lexical_analysis:close_bracket(Q0,Q,Fun).
close_brackets_f(on,Q,Q,_,Brs,Brs,[]).

%% cuts: reduce "spurious" ambiguities. We don't care *which* bracket
%% we match, as long as we match one.
close_brackets_f(off,Q0,Q,Fun,[Fun|Brs],Brs,bracket(Q0,Q)) :-
    alpino_lexical_analysis:close_bracket(Q0,Q,Fun). %  ,
close_brackets_f(off,Q,Q,Fun,[Fun|Brs],Brs,bracket(Q,Q)). % :-
close_brackets_f(off,Q0,Q,Fun,Brs,Brs,bracket(Q0,Q)) :-
    alpino_lexical_analysis:close_bracket(Q0,Q,Fun).
close_brackets_f(off,Q,Q,_,Brs,Brs,[]).


close_brackets(none,Q,Q,_,Brs,Brs,[]).

close_brackets(on,Q0,Q,Term,[Fun|Brs],Brs,bracket(Q0,Q)) :-
    alpino_lexical_analysis:close_bracket(Q0,Q,Fun),
    functor(Term,Fun,_).
close_brackets(on,Q,Q,_,Brs,Brs,[]).

%% cuts: reduce "spurious" ambiguities. We don't care *which* bracket
%% we match, as long as we match one.
close_brackets(off,Q0,Q,Term,[Fun|Brs],Brs,bracket(Q0,Q)) :-
    alpino_lexical_analysis:close_bracket(Q0,Q,Fun),
    functor(Term,Fun,_).
close_brackets(off,Q,Q,Term,[Fun|Brs],Brs,bracket(Q,Q)) :-
    functor(Term,Fun,_).
close_brackets(off,Q0,Q,Term,Brs,Brs,bracket(Q0,Q)) :-
    alpino_lexical_analysis:close_bracket(Q0,Q,Fun),
    functor(Term,Fun,_).
close_brackets(off,Q,Q,_,Brs,Brs,[]).

display_link_pair_for_cat(Cat) :-
    functor(Cat,F,A),
    functor(Cat1,F,A),
    (	alpino_lc_in:generalized_pair(Cat1,M)
    ;	alpino_lc_in:generalized_pair(M,Cat1)
    ),
    alpino_data:separate(Cat1,Cat),
    prettyvars(r(Cat,M)).

relevant_features_for_type(Type,Atts) :-
    findall(Att,relevant_feature_for_type(Type,Att),Atts0),
    sort(Atts0,Atts).

relevant_feature_for_type(Type,Att) :-
    hdrug_feature:has_type(Type,Cat,_),
    display_link_pair_for_cat(Cat),
    hdrug_feature:e(Att,Cat,Val),
    Val \== '$VAR'('_').

no_memo(Cat) :-
    alpino_grammar:no_memo(Cat).
no_memo(Cat) :-
    \+ alpino_lc_in:lc_rule_(_,Cat,_,_,_).

:- public suggest_link_weakenings/0.

suggest_link_weakenings :-
    (   alpino_lc_in:lc_rule_(_,_,_,_,_)
    ->  true
    ;   hdrug_flag(debug,Old,2),
	user:load_grammar,
	hdrug_flag(debug,_,Old)
    ),
    (	goal_fas(Goals),
	member(X/_,Goals),
	relevant_features_for_type(X,Atts),
	format_link_clause(X,Atts),
	fail
    ;	true
    ).

format_link_clause(X,Atts) :-
    format("weaken_link_cat(A,B) :-~n",[]),
    format("    A => ~w,~n",[X]),
    format("    B => ~w",[X]),
    format_link_clause_(Atts).

format_link_clause_([]) :-
    format(".~n~n",[]).
format_link_clause_([Att|Atts]) :-
    format(",~n    A:~w <=> B:~w",[Att,Att]),
    format_link_clause_(Atts).

compile_weaken_links(Goals) :-
    (	member(Fun/Ar,Goals),
	functor(A,Fun,Ar),
	functor(B,Fun,Ar),
	try_hook(alpino_grammar:weaken_link_cat(A,B),A=B),
        alpino_data:separate(A,Asyn),
	alpino_data:separate(B,Bsyn),
	noclp_assertz(alpino_lc_in:weaken_link_cat(Asyn,Bsyn)),
	fail
    ;	true
    ).


:- public check_predict/3, check_connect/3.
check_predict(off,F,PREV) :-
    \+ fail_predict(F,PREV).
check_predict(undefined,F,PREV) :-
    \+ fail_predict(F,PREV).
check_predict(on,F,PREV) :-
    alpino_guides:check_predict(F,PREV).

check_connect(off,F,[R|PREV]) :-
    \+ fail_connect(F,[R|PREV]),
    \+ fail_connect_rule(R).
check_connect(undefined,F,[R|PREV]) :-
    \+ fail_connect(F,[R|PREV]),
    \+ fail_connect_rule(R).
check_connect(on,F,[R|PREV]) :-
    alpino_guides:check_connect(F,[R|PREV]),
    \+ fail_connect_rule(R).

%% these are always ruled out
%% by "inspection" of the grammar

fail_predict(vc,lex(punct(komma))).
fail_predict(p,lex(verb(_,_,_))).
fail_predict(adv,lex(verb(_,_,_))).
fail_predict(a,lex(verb(_,PSP,_))) :-
    PSP \== psp.
fail_predict(n,lex(verb(_,PSP,_))) :-
    PSP \== psp.
fail_predict(pred,lex(verb(_,PSP,_))) :-
    PSP \== psp.
fail_predict(np,lex(verb(_,PSP,_))) :-
    PSP \== psp.
fail_predict(vc_noun,lex(verb(_,PSP,_))) :-
    PSP \== psp.
fail_predict(num,lex(verb(_,_,_))).
fail_predict(pp,lex(verb(_,_,_))).
fail_predict(rel,lex(verb(_,_,_))).
fail_predict(det,lex(verb(_,_,_))).
fail_predict(sbar,lex(verb(_,_,_))).
fail_predict(v_noun,lex(verb(_,_,_))).
fail_predict(app_n_app,lex(verb(_,_,_))).
fail_predict(redrel,lex(verb(_,_,_))).
fail_predict(part,lex(verb(_,_,_))).

fail_connect(vc,[start_coord(vb,_),vb_v,lex(verb(_,_,_))]).
fail_connect(vc,[vb_v,vgap,get_val]).
fail_connect(vb,[start_coord(vb,en),vb_v,te,lex(complementizer(te))]).
%% next one is wrong for "Omdat ik hem opgebeld wil en moet hebben"
fail_connect(vc,[start_coord(v,_),lex(verb(_,_,_))]).

:- initialize_thread_flag(lc_crd_exists,off).
:- initialize_thread_flag(lc_crd_r_exists,off).
:- initialize_thread_flag(lc_crd_z_exists,off).

init_check_crd :-
    (   (   alpino_lexical_analysis:search_tag_tag(conj(_),_)
        ;   alpino_lexical_analysis:search_tag_tag(etc,_)
        ;   alpino_lexical_analysis:search_tag_tag(with_dt(complex_etc,_),_)
        )
    ->  set_thread_flag(lc_crd_exists,on)
    ;   set_thread_flag(lc_crd_exists,off)
    ),
    (   (   alpino_lexical_analysis:search_tag_tag(right_conj(_),_)
	;   alpino_lexical_analysis:search_tag_tag(conj(_),_)
	)
    ->  set_thread_flag(lc_crd_r_exists,on)
    ;   set_thread_flag(lc_crd_r_exists,off)
    ),
    (   (   alpino_lexical_analysis:search_tag_tag(etc,_)
        ;   alpino_lexical_analysis:search_tag_tag(with_dt(complex_etc,_),_)
        )
    ->  set_thread_flag(lc_crd_z_exists,on)
    ;   set_thread_flag(lc_crd_z_exists,off)
    ).

fail_connect_rule(ID) :-
    \+ frequent_rule(ID).

fail_connect_rule(start_coord(CAT,en)) :-
    coord_requires_conj(CAT),
    thread_flag(lc_crd_exists,off).

fail_connect_rule(start_coord(CAT,en)) :-
    coord_requires_conj_or_etc(CAT),
    thread_flag(lc_crd_exists,off),
    thread_flag(lc_crd_z_exists,off).

fail_connect_rule(start_coord(_,root,en)) :-
    thread_flag(lc_crd_exists,off),
    thread_flag(lc_crd_z_exists,off).

fail_connect_rule(start_coord(CAT,zowel_als)) :-
    coord_requires_conj(CAT),
    thread_flag(lc_crd_r_exists,off).

fail_connect_rule(start_coord(CAT,zowel_als)) :-
    coord_requires_conj_or_etc(CAT),
    thread_flag(lc_crd_r_exists,off).

fail_connect_rule(zowel_swap(_)) :-
    thread_flag(lc_crd_r_exists,off).
fail_connect_rule(zowel_swap(_,_)) :-
    thread_flag(lc_crd_r_exists,off).
fail_connect_rule(within_word_conjunction(zowel_als_np)) :-
    thread_flag(lc_crd_r_exists,off).
fail_connect_rule(within_word_conjunction(zowel_als_pp)) :-
    thread_flag(lc_crd_r_exists,off).

fail_connect_rule(start_coord(_,root,en)) :-
    thread_flag(lc_crd_exists,off).
fail_connect_rule(van_en) :-
    thread_flag(lc_crd_exists,off).
fail_connect_rule(xp_en_root) :-
    thread_flag(lc_crd_exists,off).
fail_connect_rule(within_word_conjunction(en_np)) :-
    thread_flag(lc_crd_exists,off).



coord_requires_conj(within_word_conjunction(Name)) :-
    coord_requires_conj(Name).
coord_requires_conj(quest_short).
coord_requires_conj(root_imp).
coord_requires_conj(root_imparative).
coord_requires_conj(root_modifier).
coord_requires_conj(topic_drop_root).
coord_requires_conj(v).
coord_requires_conj(vb).
coord_requires_conj(vc).
coord_requires_conj(vpx).
coord_requires_conj(sv1).
coord_requires_conj(imp).
coord_requires_conj(prep).
coord_requires_conj(part).
coord_requires_conj(adv).
coord_requires_conj(a).
coord_requires_conj(pn).
coord_requires_conj(pp).
coord_requires_conj(rel).
coord_requires_conj(det).
coord_requires_conj(vproj).
coord_requires_conj(comp).
coord_requires_conj(v_noun).
coord_requires_conj(vc_noun).
coord_requires_conj(enumeration).

coord_requires_conj_or_etc(vp).
coord_requires_conj_or_etc(modifier).
coord_requires_conj_or_etc(root).
coord_requires_conj_or_etc(n).
coord_requires_conj_or_etc(sbar).

% pred, np

:- dynamic
    alpino_parse_values:val/3.
:- thread_local
    alpino_parse_values:val/3.

:- public get_val/2, put_val/2, abstract_call/2.

:- block get_val(-,?).
get_val(Key,Val) :-
    alpino_parse_values:val(Key,Val,Cons),
    call(Cons).

%%KEY not instantiated -> 1st phase, otherwise 2nd
put_val(Key,Val0) :-
    var(Key),
    !,
    copy_term(Val0,Val,Cons0),
    list_to_conj(Cons0,Cons),
    gen_sym(Key,parse_values),
    noclp_assertz(alpino_parse_values:val(Key,Val,Cons)).

%% second phase: simply use the desired key!
%% allow several values for the same key, because of addition of semantics
put_val(Key/N,Val0) :-
    copy_term(Val0,Val,Cons0),
    list_to_conj(Cons0,Cons),
    (   alpino_parse_values:val(Key/N,Val1,Cons1),
        variant(Val1/Cons1,Val/Cons)
    ->  true
    ;   gen_sym(N,parse_values2),
        noclp_assertz(alpino_parse_values:val(Key/N,Val,Cons))
    ).

abstract_call(get_val(Key,_),get_val(Key)).
abstract_call(put_val(Key,_),put_val(Key)).

abstract_call2(get_val(Key/_,_),get_val(Key)).
abstract_call2(put_val(Key/_,_),put_val(Key)).

%% I have added
%%                    VSLASH:vframe <=> V:vframe,
%%
%% in the unify_vslash predicate in rules.gram:
%% don't generalize values for different frames,
%% because in 2nd phase these are not yet available perhaps
%% ex.: 'zij doet iets aan X' where we have both
%% so_pp_np and np_pc_pp(aan)
%%
%% this problem appears to be related to memo of the second phase.
%% It might then be that you memo a predicate in a global context,
%% where later the global context is changed in such a way taht more
%% solutions would be possible.
%% So, memo should consider if contexts had changed or not...
%%
%% Alternative: work breadth-first as suggested by TALN paper. So then
%% first treat all sub-trees with call(put_val) before you try any
%% call(get_val)....

frequent_rule(finish). % only required if called from fail_connect_rule/1
frequent_rule(Id) :-
    hdrug_flag(parse_candidates_beam,Val),
    (   Val == 50
    ->			%i.e. we are running after time-out
	frequent_rule(Id,_N)
    ;   true
    ).



frequent_rule(robust,12706).
frequent_rule(optpunct(e),12461).
frequent_rule(top_start,12142).
frequent_rule(top_start_xp,12089).
frequent_rule(vpx_vproj,11754).
frequent_rule(vc_vb,11699).
frequent_rule(vproj_vc,11690).
frequent_rule(optpunct(ne),11675).
frequent_rule(vgap,11443).
frequent_rule(v2_vp_vproj,11442).
frequent_rule(imp,11335).
frequent_rule(vb_v,10778).
frequent_rule(max_xp(root),10626).
frequent_rule(np_det_n,10291).
frequent_rule(vp_arg_v(np),9463).
frequent_rule(np_n,9024).
frequent_rule(pp_p_arg(np),8947).
frequent_rule(vp_mod_v,6631).
frequent_rule(n_pn,6115).
frequent_rule(mod2,5542).
frequent_rule(n_adj_n,5517).
frequent_rule(non_wh_topicalization(np),5478).
frequent_rule(np_pron_weak,5291).
frequent_rule(n_n_pps,5258).
frequent_rule(v_v_v,5101).
frequent_rule(vp_vpx,4083).
frequent_rule(mod1,3989).
frequent_rule(mod1a,3293).
frequent_rule(adv_a,3106).
frequent_rule(vp_arg_v(pred),2878).
frequent_rule(non_wh_topicalization(modifier),2624).
frequent_rule(vp_arg_v(pp),2352).
frequent_rule(pred_np,2088).
frequent_rule(sbar(vp),2011).
frequent_rule(pred_a,1996).
frequent_rule(pron_det,1589).
frequent_rule(vp_v_mod,1526).
frequent_rule(te,1469).
frequent_rule(a_adv_a,1468).
frequent_rule(n_num_n,1460).
frequent_rule(rel_arg(np),1433).
frequent_rule(n_n_pnapp,1414).
frequent_rule(start_coord(np,en),1382).
frequent_rule(end_coord(np,conj),1278).
frequent_rule(n_n_rel,1262).
frequent_rule(vb_part_v,1148).
frequent_rule(max_xp(np),1082).
frequent_rule(wh_topicalization(np),1065).
frequent_rule(mod5,968).
frequent_rule(vp_v_komma_arg(sbar),948).
frequent_rule(wh_topicalization(pred),908).
frequent_rule(vp_v_komma_arg(pp),810).
frequent_rule(adv_tmp_np,754).
frequent_rule(det_adj,646).
frequent_rule(wh_topicalization(modifier),592).
frequent_rule(xp_modal_xp(np),523).
frequent_rule(end_coord(root,conj),521).
frequent_rule(start_coord(root,en),518).
frequent_rule(vp_v_komma_mod,505).
frequent_rule(vp_v_m_extra,499).
frequent_rule(v_v_pspgap,488).
frequent_rule(v_psp_v,488).
frequent_rule(sbar(np),475).
frequent_rule(vpx_vc_noun,457).
frequent_rule(nominalization,457).
frequent_rule(deverbal_vc_v,457).
frequent_rule(n_n_napp,446).
frequent_rule(n_n_modnp(komma),423).
frequent_rule(vp_v_komma_arg(vp),418).
frequent_rule(er_np,416).
frequent_rule(pp_p,398).
frequent_rule(max_xp(rootbar),391).
frequent_rule(sbar(want),389).
frequent_rule(vp_arg_v(fixed_pred),387).
frequent_rule(np_pron,365).
frequent_rule(n_num,364).
frequent_rule(n_adj,345).
frequent_rule(a_pp_a,323).
frequent_rule(mod_om_vpx,322).
frequent_rule(n_n_modroot(haak),321).
frequent_rule(wh_sbar_arg(np),294).
frequent_rule(end_coord(n,conj),292).
frequent_rule(q_pn,279).
frequent_rule(vp_om_vpx,278).
frequent_rule(rel_mod(pp),274).
frequent_rule(mid_coord(np),269).
frequent_rule(dp_dp_root,267).
frequent_rule(pred_als_cp,264).
frequent_rule(a_a_pp,259).
frequent_rule(xp_modal_xp(prep),258).
frequent_rule(start_coord(n,en),255).
frequent_rule(end_coord(sv1,conj),252).
frequent_rule(a_me_comp_a,248).
frequent_rule(sbar_red_rel,244).
frequent_rule(start_coord(sv1,en),236).
frequent_rule(wh_sbar_mod(pp),227).
frequent_rule(max_xp(sbar),226).
frequent_rule(xp_modal_xp(adv),218).
frequent_rule(vp_np_v_pp,205).
frequent_rule(max_xp(pp),196).
frequent_rule(wh_topicalization(pp),195).
frequent_rule(pron_pron_pps,193).
frequent_rule(n_adj_n_marked,191).
frequent_rule(dip_sv1,190).
frequent_rule(q_n,189).
frequent_rule(end_coord(modifier,conj),188).
frequent_rule(vp_v_inf,179).
frequent_rule(end_coord(a,conj),172).
frequent_rule(max_xp(vp),165).
frequent_rule(top_start_tag,164).
frequent_rule(start_coord(a,en),162).
frequent_rule(start_mod_np,157).
frequent_rule(vp_v_extra_pp,155).
frequent_rule(vp_v_sbar,153).
frequent_rule(n_n_mod(komma),153).
frequent_rule(wh_topicalization(adv),151).
frequent_rule(non_wh_topicalization(pp),150).
frequent_rule(vp_v_extra,149).
frequent_rule(start_coord(modifier,en),145).
frequent_rule(a_int_adv_a,145).
frequent_rule(non_wh_topicalization(pred),143).
frequent_rule(comparative(np),142).
frequent_rule(nucl_sat(sbar),141).
frequent_rule(n_adj_pl,140).
frequent_rule(n_n_mod_adv(komma),139).
frequent_rule(nucl_sat(np),133).
frequent_rule(end_coord(np,noconj),133).
frequent_rule(pred_van_belang,131).
frequent_rule(pp_p_arg(n),129).
frequent_rule(imp_imp,129).
frequent_rule(topic_drop,126).
frequent_rule(top_tag_start_xp,126).
frequent_rule(mod5rr,126).
frequent_rule(etopic,126).
frequent_rule(xp_en_root,125).
frequent_rule(np_np_tmp,124).
frequent_rule(max_xp(sv1),119).
frequent_rule(start_coord(vproj,en),117).
frequent_rule(max_xp(pred),117).
frequent_rule(end_coord(vproj,conj),117).
frequent_rule(max_xp(imp),116).
frequent_rule(n_n_num_app,111).
frequent_rule(vp_predm_adj_v,110).
frequent_rule(start_coord(pred,en),109).
frequent_rule(pp_p_arg(adv),105).
frequent_rule(vp_predm_v,102).
frequent_rule(n_n_napp_app,101).
frequent_rule(start_coord(vp,en),100).
frequent_rule(n_pn_n,99).
frequent_rule(end_coord(vp,conj),98).
/*
frequent_rule(vp_predm_pp_v,97).
frequent_rule(start_max_dip,96).
frequent_rule(n_n_mod_a,96).
frequent_rule(end_coord(pred,conj),96).
frequent_rule(adv_meas_np,95).
frequent_rule(vp_c_mod_c_v,93).
frequent_rule(n_n_cp,93).
frequent_rule(a_pp_comp_a,93).
frequent_rule(adv_meer,91).
frequent_rule(quoted_top_start,90).
frequent_rule(np_np_adv,88).
frequent_rule(n_n_sbar,87).
frequent_rule(np_adjn,85).
frequent_rule(n_comma_adj_n,82).
frequent_rule(deverbal_vp_mod_v,81).
frequent_rule(start_coord(pp,en),80).
frequent_rule(xp_modal_xp(sbar),79).
frequent_rule(np_red_rel,79).
frequent_rule(nucl_sat(redrel),76).
frequent_rule(end_coord(pp,conj),76).
frequent_rule(a_np_comp_a,76).
frequent_rule(a_a_pp_comp,75).
frequent_rule(rel_arg(pp),73).
frequent_rule(vp_arg_v(adv),71).
frequent_rule(start_dq1_max_dip,71).
frequent_rule(a_detadv_a,70).
frequent_rule(n_n_vp,69).
frequent_rule(n_n_modroot(min),68).
frequent_rule(num_num_adv_num,67).
frequent_rule(np_np,67).
frequent_rule(adv_adv_pps,67).
frequent_rule(max_xp(advp),65).
frequent_rule(a_a_compp,64).
frequent_rule(deverbal_vp_arg_v(np),61).
frequent_rule(n_n_gennp,56).
frequent_rule(max_xp(om_rel),56).
frequent_rule(dip_scope_root,56).
frequent_rule(non_wh_topicalization(sbar),55).
frequent_rule(vp_v_komma_arg(pred_als),54).
frequent_rule(start_coord(vpx,en),54).
frequent_rule(end_coord(vpx,conj),54).
frequent_rule(modifier_p(1),52).
frequent_rule(root_en_dp,51).
frequent_rule(dip_root,51).
frequent_rule(dip_pp,50).
frequent_rule(p_npmod_p,48).
frequent_rule(n_rang,48).
frequent_rule(non_wh_topicalization_dip,47).
frequent_rule(adv_red_rel,47).
frequent_rule(start_start_dubb_punt,46).
frequent_rule(sbar(a),45).
frequent_rule(np_det_n_q,45).
frequent_rule(n_n_modified_np,45).
frequent_rule(dp_dp_root_sv1,45).
frequent_rule(modifier_p(2),44).
frequent_rule(start_coord(root_modifier,en),43).
frequent_rule(pp_p_arg_part(np),42).
frequent_rule(start_start_ligg_streep,41).
frequent_rule(sat_nucl(sbar),41).
frequent_rule(pp_pp_dp,41).
frequent_rule(np_np_year,41).
frequent_rule(start_dq2_max_dip,40).
frequent_rule(num_adv_num,40).
frequent_rule(end_coord(np,conj,mod),40).
frequent_rule(vp_vp_colon,39).
frequent_rule(start_coord(within_word_conjunction(n),en),39).
frequent_rule(start_coord(sbar,en),39).
frequent_rule(rel_pp_np,39).
frequent_rule(np_adv_dp,39).
frequent_rule(end_coord(num,conj),39).
frequent_rule(vc_part_vc,38).
frequent_rule(start_start_punt_komma,38).
frequent_rule(start_coord(num,en),38).
frequent_rule(sbar(al),38).
frequent_rule(sbar(pp),37).
frequent_rule(vp_pred_v_pp,36).
frequent_rule(end_coord(sbar,conj),36).
frequent_rule(wh_sbar_arg(pred),34).
frequent_rule(start_coord(pn,en),33).
frequent_rule(n_n_mod_a(komma),33).
frequent_rule(end_coord(pn,conj),33).
frequent_rule(adj_te_v,32).
frequent_rule(predm_ap_topicalization,30).
frequent_rule(n_n_tmpnapp,30).
frequent_rule(pp_p_arg(pp),29).
frequent_rule(nucl_sat_brackets,29).
frequent_rule(n_n_loc_adv,29).
frequent_rule(det_adv_det,29).
frequent_rule(comparative(vp),29).
frequent_rule(start_coord(vc,en),28).
frequent_rule(start_coord(rel,en),28).
frequent_rule(end_coord(rel,conj),28).
frequent_rule(pron_pron_rel,27).
frequent_rule(top_adv_tag_start_xp,26).
frequent_rule(q_mod,26).
frequent_rule(top_start_tag_xp,25).
frequent_rule(sat_nucl_nocomma(sbar),25).
frequent_rule(q_a,25).
frequent_rule(n_n_modnp(dubb_punt),25).
frequent_rule(n_n_mod_vp,25).
frequent_rule(mid_coord(n),25).
frequent_rule(a_pred_a,25).
frequent_rule(vp_v_extra_rel,24).
frequent_rule(sat_nucl(np),24).
frequent_rule(n_n_adv,24).
frequent_rule(adv_mod_np,24).
frequent_rule(adj_genoeg,24).
frequent_rule(top_quoted_start,23).
frequent_rule(n_n_adj,23).
frequent_rule(comparative(pp),23).
frequent_rule(vp_np_v_en_np,22).
frequent_rule(end_coord(vc,conj),22).
frequent_rule(vpx_v_mod,21).
frequent_rule(rel_om_vpx,21).
frequent_rule(non_wh_topicalization(vc),21).
frequent_rule(start_coord(np,zowel_als),20).
frequent_rule(start_coord(adv,en),20).
frequent_rule(det_quant_det,20).
frequent_rule(a_a_sinf,20).
frequent_rule(q_vproj,19).
frequent_rule(n_n_modroot(comma),19).
frequent_rule(end_coord(adv,conj),19).
frequent_rule(a_a_sbar,19).
frequent_rule(pp_p_arg(sbar),18).
frequent_rule(n_measn_n,18).
frequent_rule(xp_modal_xp(pp),17).
frequent_rule(vp_v_predm_pp,17).
frequent_rule(pp_p_arg(r),17).
frequent_rule(aan_het_v,17).
frequent_rule(zo_adj_mogelijk,16).
frequent_rule(start_coord(root_imp,en),16).
frequent_rule(start_coord(modifier,zowel_als),16).
frequent_rule(end_coord(enz),16).
frequent_rule(comparative(adv),16).
frequent_rule(vp_dip_vp1,15).
frequent_rule(start_dq1_max_dip_no_comma,15).
frequent_rule(n_score_n,15).
frequent_rule(n_n_comparative,15).
frequent_rule(end_coord(pred,noconj),15).
frequent_rule(dip_scope_root_q,15).
frequent_rule(a_advp_dp,15).
frequent_rule(vp_vp_dip,14).
frequent_rule(vp_v_komma_predm,14).
frequent_rule(pp_p_arg_part(adv),14).
frequent_rule(mid_coord(sv1),14).
frequent_rule(end_coord(modifier,conj,mod),14).
frequent_rule(deverbal_vp_arg_v(pred),14).
frequent_rule(a_int_me_adv_a,14).
frequent_rule(a_a_modcp,14).
frequent_rule(vp_v_m_extra_vp,13).
frequent_rule(start_coord(within_word_conjunction(np),en),13).
frequent_rule(start_coord(topic_drop_root,en),13).
frequent_rule(rel_arg(pred),13).
frequent_rule(q_pred,13).
frequent_rule(deverbal_vp_v_arg(pp),13).
frequent_rule(a_a_er_pp_comp,13).
frequent_rule(xp_en_ques,12).
frequent_rule(wh_sbar_arg(pp),12).
frequent_rule(ques_dan,12).
frequent_rule(pron_pron_gennp,12).
frequent_rule(mid_coord(np,conj),12).
frequent_rule(mid_coord(a),12).
frequent_rule(loc_pp_dp,12).
frequent_rule(end_coord(pp,conj,mod),12).
frequent_rule(abs_p_predc,12).
frequent_rule(start_coord(v,en),11).
frequent_rule(start_coord(prep,en),11).
frequent_rule(ques_comma_dan,11).
frequent_rule(mod7,11).
frequent_rule(mid_coord(root,conj),11).
frequent_rule(mid_coord(pred),11).
frequent_rule(end_coord(v,conj),11).
frequent_rule(deverbal_vp_arg_v(pp),11).
frequent_rule(comparative(vpx),11).
frequent_rule(a_a_np_comp,11).
frequent_rule(vp_pred_v_en_pred,10).
frequent_rule(sv1_dip_sv1,10).
frequent_rule(rel_arg(adv),10).
frequent_rule(q_v,10).
frequent_rule(end_coord(pred,conj,mod),10).
frequent_rule(sv1_dip_sv1_q,9).
frequent_rule(short_wh_sbar(pp),9).
frequent_rule(sat_nucl(redrel),9).
frequent_rule(np_wh_np_adv,9).
frequent_rule(np_ap,9).
frequent_rule(mid_coord(root),9).
frequent_rule(end_coord(root,conj,mod),9).
frequent_rule(end_coord(prep,conj),9).
frequent_rule(dit_omdat,9).
frequent_rule(vp_dip_vp,8).
frequent_rule(vp_c_whq_c_v,8).
frequent_rule(pron_pron_mod_a,8).
frequent_rule(predm_pp_topicalization,8).
frequent_rule(pn_n_modpn(komma),8).
frequent_rule(n_bracketed_adj_n,8).
frequent_rule(mid_coord(pn),8).
frequent_rule(mid_coord(modifier),8).
frequent_rule(max_xp(adj),8).
frequent_rule(adv_adv_dp,8).
frequent_rule(a_cp_a,8).
frequent_rule(a_a_bracketed_mod,8).
frequent_rule(wh_topicalization(modifier,vp),7).
frequent_rule(vpx_v_komma_mod,7).
frequent_rule(vp_vp_dip_q,7).
frequent_rule(vp_v_predm_ap,7).
frequent_rule(start_dq2_max_dip_no_comma,7).
frequent_rule(start_coord(within_word_conjunction(a),en),7).
frequent_rule(short_wh_sbar(np),7).
frequent_rule(short_wh_sbar(advp),7).
frequent_rule(q_vp,7).
frequent_rule(pron_pron_cp,7).
frequent_rule(num_num_haak,7).
frequent_rule(non_wh_topicalization(adv),7).
frequent_rule(n_n_napp_app_start2,7).
frequent_rule(mid_coord(vp),7).
frequent_rule(end_coord(complex_enz),7).
frequent_rule(adv_wh_adv_adv,7).
frequent_rule(a_fixed_a,7).
frequent_rule(sat_nucl(pp),6).
frequent_rule(pp_pp_adv,6).
frequent_rule(pp_p_arg(dubb),6).
frequent_rule(pp_adv_dp,6).
frequent_rule(np_n_napp_app,6).
frequent_rule(mid_coord(vproj),6).
frequent_rule(dip_scope_vp,6).
frequent_rule(adv_mod_sbar_dp,6).
frequent_rule(abs_p_mod_np,6).
frequent_rule(a_predm_a,6).
frequent_rule(a_a_modcp_pred,6).
frequent_rule(wh_sbar_arg(adv),5).
frequent_rule(vp_v_komma_arg(np_heavy),5).
frequent_rule(top_imp_tag_start_xp,5).
frequent_rule(start_coord(vb,en),5).
frequent_rule(start_coord(pp,zowel_als),5).
frequent_rule(start_coord(det,en),5).
frequent_rule(sbar(adv),5).
frequent_rule(q_sbar_punct,5).
frequent_rule(predm_topicalization,5).
frequent_rule(pred_pp_vol,5).
frequent_rule(np_dip,5).
frequent_rule(non_wh_topicalization_dip_mod,5).
frequent_rule(n_n_n_pnapp,5).
frequent_rule(modal_inv,5).
frequent_rule(end_coord(vc_noun,conj),5).
frequent_rule(end_coord(vc,conj,mod),5).
frequent_rule(end_coord(det,conj),5).
frequent_rule(comparative(sbar),5).
frequent_rule(adv_pp_dp,5).
frequent_rule(abs_p_predc_inv,5).
frequent_rule(abs_p_np_mod,5).
frequent_rule(xp_modal_xp(vp_modifier),4).
frequent_rule(vp_np_v_sbar,4).
frequent_rule(start_tag_dip_max,4).
frequent_rule(start_mod_comma_np,4).
frequent_rule(start_coord(vc_noun,en),4).
frequent_rule(start_coord(a,zowel_als),4).
frequent_rule(pred_sbar_dp,4).
frequent_rule(np_adv_wh_np,4).
frequent_rule(n_n_predm_modified_np,4).
frequent_rule(n_comma_adj_comma_n,4).
frequent_rule(n_bracketed_mod_n,4).
frequent_rule(mid_coord(modifier,conj),4).
frequent_rule(end_coord(vb,conj),4).
frequent_rule(end_coord(imp,conj),4).
frequent_rule(deverbal_vp_predm_adj_v,4).
frequent_rule(bracketed_top_start,4).
frequent_rule(adv_root_dp,4).
frequent_rule(a_hoe_a,4).
frequent_rule(within_word_conjunction(en_np),3).
frequent_rule(vp_v_komma_arg(rel),3).
frequent_rule(vp_pp_v_en_pp,3).
frequent_rule(vp_np_v_ap,3).
frequent_rule(vp_dip_vp_no_comma,3).
frequent_rule(vp_dip_vp_ligg2,3).
frequent_rule(vast_staat,3).
frequent_rule(van_en,3).
frequent_rule(top_start_tag_root_imp,3).
frequent_rule(top_start_root_dip,3).
frequent_rule(top_bracketed_start,3).
frequent_rule(start_coord(pred,zowel_als),3).
frequent_rule(start_coord(imp,en),3).
frequent_rule(sbar_npmod_sbar,3).
frequent_rule(sbar_np_red_rel,3).
frequent_rule(sbar(vpx),3).
frequent_rule(sbar(sbar),3).
frequent_rule(rel_pp_np_dp,3).
frequent_rule(pron_pron_napp,3).
frequent_rule(pron_pron_modroot(haak),3).
frequent_rule(predm_adv_np_dp,3).
frequent_rule(pred_zoals_cp,3).
frequent_rule(non_wh_topicalization_dip_pred,3).
frequent_rule(n_dashed_adj_n,3).
frequent_rule(mid_coord(vpx),3).
frequent_rule(mid_coord(adv),3).
frequent_rule(max_xp(vpx),3).
frequent_rule(max_wh_adv_np,3).
frequent_rule(end_coord(prep,conj,mod),3).
frequent_rule(ecomp,3).
frequent_rule(daarom_immers,3).
frequent_rule(adv_np_dp,3).
frequent_rule(adv_dat_sbar_dp,3).
frequent_rule(a_predm_dev_a,3).
frequent_rule(a_ligg_mod_a,3).
frequent_rule(a_bracketed_mod_a,3).
frequent_rule(vpnoun_v_m_extra,2).
frequent_rule(vp_vp_dip_q_nocolon,2).
frequent_rule(vp_v_extra_pred,2).
frequent_rule(vp_predm_adj_v_comma,2).
frequent_rule(vp_dip_vp2,2).
frequent_rule(vp_dashed_predm_adj_v,2).
frequent_rule(v2_vp_dubb_vproj,2).
frequent_rule(sv1_dip_sv1_ligg,2).
frequent_rule(start_max_dip_max_q2,2).
frequent_rule(start_coord(root_imparative,en),2).
frequent_rule(start_coord(dipq,root,en),2).
frequent_rule(short_wh_sbar(np_mod),2).
frequent_rule(short_hoe_short_hoe,2).
frequent_rule(sbar(van),2).
frequent_rule(sbar(dubb),2).
frequent_rule(sat_nucl(sbar_ques),2).
frequent_rule(rel_arg(meas_mod_np),2).
frequent_rule(ques_ques,2).
frequent_rule(q_sbar,2).
frequent_rule(pron_pron_loc_adv,2).
frequent_rule(pred_pp_voor,2).
frequent_rule(pred_adv_sbar_dp,2).
frequent_rule(pp_p_part,2).
frequent_rule(np_zoon_n,2).
frequent_rule(np_imp,2).
frequent_rule(non_wh_topicalization(vp),2).
frequent_rule(n_n_pntag,2).
frequent_rule(n_n_pnappgen,2).
frequent_rule(n_n_num_number_app,2).
frequent_rule(n_n_napp_app_start1,2).
frequent_rule(n_comma_adj_n_marked,2).
frequent_rule(mid_coord(sv1,conj),2).
frequent_rule(mid_coord(rel),2).
frequent_rule(mid_coord(pp),2).
frequent_rule(end_coord(sbar,conj,mod),2).
frequent_rule(end_coord(en_hellip),2).
frequent_rule(eenmaal_absolute,2).
frequent_rule(dit_vanwege,2).
frequent_rule(dit_om,2).
frequent_rule(deverbal_vp_v_arg(sbar),2).
frequent_rule(deverbal_v_v_v,2).
frequent_rule(det_np_det,2).
frequent_rule(bracketed_end_coord(np,conj,mod),2).
frequent_rule(ap_wh_ap_adv,2).
frequent_rule(adv_adv_sbar,2).
frequent_rule(adv_adv_rel,2).
frequent_rule(adv_adv_adj,2).
frequent_rule(abs_p_mod_predc,2).
frequent_rule(a_pp_dp,2).
frequent_rule(a_a_sinf_tough,2).
frequent_rule(zowel_swap(within_word_conjunction(np)),1).
frequent_rule(wh_sbar_arg_inf(np),1).
frequent_rule(vpnoun_v_extra,1).
frequent_rule(vp_v_predm_adv,1).
frequent_rule(vp_v_bracketed_m_extra,1).
frequent_rule(vp_predm_pp_v_comma,1).
frequent_rule(vp_hellip_arg_v(np),1).
frequent_rule(vp_c_tag_c_v,1).
frequent_rule(vp_bracketed_predm_adj_v,1).
frequent_rule(top_start_anp,1).
frequent_rule(start_tag_dip_max_q2,1).
frequent_rule(start_start_dubb_punt_q,1).
frequent_rule(start_max_dip_no_comma,1).
frequent_rule(start_coord(within_word_conjunction(vc_noun),en),1).
frequent_rule(start_coord(within_word_conjunction(num),en),1).
frequent_rule(start_coord(within_word_conjunction(modifier),zowel_als),1).
frequent_rule(start_coord(prep,zowel_als),1).
frequent_rule(start_coord(part,en),1).
frequent_rule(start_coord(num,zowel_als),1).
frequent_rule(start_coord(dipq2,root,en),1).
frequent_rule(start_coord(dip,root,en),1).
frequent_rule(sbar_want_tag,1).
frequent_rule(sbar_want_dip,1).
frequent_rule(sbar_punct,1).
frequent_rule(sbar(quoted_a),1).
frequent_rule(sat_tag_nucl(np),1).
frequent_rule(ques_imp,1).
frequent_rule(q_vb,1).
frequent_rule(pron_pron_modroot(min),1).
frequent_rule(predm_adv_sbar_dp,1).
frequent_rule(preda_adv_np_dp,1).
frequent_rule(pred_vp_dp,1).
frequent_rule(pred_inf_rel,1).
frequent_rule(pp_p_mod_arg(np),1).
frequent_rule(pp_p_arg(vp),1).
frequent_rule(pp_npmod_pp,1).
frequent_rule(pp_np_p,1).
frequent_rule(pp_aldus_mod_arg(np),1).
frequent_rule(num_cp_num,1).
frequent_rule(num_bracketed_adv_num,1).
frequent_rule(np_tmp_det_num_n,1).
frequent_rule(n_year_n,1).
frequent_rule(n_n_npapp,1).
frequent_rule(n_dashed_mod_n,1).
frequent_rule(mid_coord(within_word_conjunction(np),conj),1).
frequent_rule(mid_coord(vpx,conj),1).
frequent_rule(mid_coord(vproj,conj),1).
frequent_rule(mid_coord(vp,conj),1).
frequent_rule(mid_coord(vc),1).
frequent_rule(mid_coord(sbar),1).
frequent_rule(mid_coord(num,conj),1).
frequent_rule(mid_coord(num),1).
frequent_rule(mid_coord(n,conj),1).
frequent_rule(mid_coord(a,conj),1).
frequent_rule(max_xp(post_pp),1).
frequent_rule(max_xp(dip_sv1),1).
frequent_rule(hoe_short_hoe,1).
frequent_rule(end_coord(vb,conj,mod),1).
frequent_rule(end_coord(sbar,conj,c_mod),1).
frequent_rule(end_coord(root,conj,c_mod),1).
frequent_rule(end_coord(pred,conj,c_mod),1).
frequent_rule(end_coord(part,conj),1).
frequent_rule(end_coord(num,noconj),1).
frequent_rule(end_coord(np,conj,c_mod),1).
frequent_rule(end_coord(modifier,conj,c_mod),1).
frequent_rule(end_coord(imp,conj,mod),1).
frequent_rule(end_coord(adv,conj,mod),1).
frequent_rule(dit_allemaal_vanwege,1).
frequent_rule(dip_scope_sbar,1).
frequent_rule(deverbal_vp_arg_v(fixed_pred),1).
frequent_rule(deverbal_vp_arg_v(adv),1).
frequent_rule(deverbal_v_inv_v_v,1).
frequent_rule(comparative(a),1).
frequent_rule(bracketed_end_coord(vc,conj,mod),1).
frequent_rule(bracketed_end_coord(np,conj),1).
frequent_rule(bracketed_end_coord(modifier,conj),1).
frequent_rule(bracketed_end_coord(a,conj),1).
frequent_rule(adv_rel_vp_dp,1).
frequent_rule(adj_vp_dp,1).
frequent_rule(abs_p_predc_vp_inv,1).
frequent_rule(abs_p_predc_sbar_inv,1).
frequent_rule(a_tag_pp_dp,1).
*/