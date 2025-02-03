:- module( alpino_util, [  retractall_items/1,
			   abolish_items/1,
			   count_items/1,
			   
			   restrict_list/3,
			   restrict/3,
			   
			   transitive/1,
			   
			   findall_atmost/4,
			   atmost/2,
			   n_best/3,
			   n_best/4,
			   n_best/5,
			   
			   stddev/2,
			   average/2,
			   
			   time/1,
			   time/2,
			   inform_undefined_module/1,
			   inform_undefined_module/2,
			   
			   overrule_module/1,
			   overrule_module/2,
			   
			   penalties/4,
			   
			   codes_to_words_or_tokenize/2,
			   codes_to_words/2,

			   format_word_list/1,
			   
			   format_gen_suite/2,
			   
			   format_counted_features/1,

			   tr/4,
			   
			   sort_not_uniq/2,
			   
			   write_list/2,

			   split_string/3,

			   split_atom/3,
			   replace_sub_atom/4,

			   thread_flag/2, thread_flag/3, initialize_thread_flag/2, set_thread_flag/2
			
			]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(lists)).
:- use_module(library(charsio)).

:- dynamic
    alpino_util_tmp:bpair/2,
    alpino_util_tmp:pair/2.

:- thread_local
    alpino_util_tmp:bpair/2,
    alpino_util_tmp:pair/2.

:- meta_predicate findall_atmost(?,?,:,?),
                  atmost(?,:),
		  n_best(?,?,:),
		  n_best(?,?,:,?),
		  n_best(?,?,:,?,?).

:- meta_predicate overrule_module(:).

:- use_module(library(terms), [ subsumes_chk/2 ]).
:- use_module(library(lists), [ append/3 ]).
:- use_module(hdrug(hdrug_util)).

retractall_items(Module) :-
    (	Module:current_predicate(_,X),
	Module:predicate_property(X,dynamic),
	Module:retractall(X),
	fail
    ;	true
    ).

count_items(Module) :-
    (
	findall(F/A,(Module:current_predicate(_,X),
		     Module:predicate_property(X,dynamic),
		     functor(X,F,A)
		    ),Xs0),
	sort(Xs0,Xs),
	member(F/A,Xs),
	functor(X,F,A),
	findall(_,clause(Module:X,_,_),Ls),
	length(Ls,L), L > 0,
	format(user_error,"~w: ~w~n",[F/A,L]),
	fail
    ;	true
    ).


abolish_items(Module) :-
    (	Module:current_predicate(_,X),
	can_be_abolished(Module,X),
	functor(X,F,A),
	Module:abolish(F/A),
	fail
    ;	true
    ).

can_be_abolished(Module,X) :-
    Module:predicate_property(X,compiled),
    !.
can_be_abolished(Module,X) :-
    Module:predicate_property(X,dynamic),
    !.
can_be_abolished(Module,X) :-
    Module:predicate_property(X,interpreted),
    \+ Module:predicate_property(X,built_in),
    !.

%%%%%%%%%%%%%%%%%%%%%%%
%%%%% RESTRICTION %%%%%
%%%%%%%%%%%%%%%%%%%%%%%

restrict_pair(Depth,A0,B0,A,B) :-
    weaken_link_cat(A0,A1),
    weaken_link_cat(B0,B1),
    restrict(Depth,A1-B1,A-B).

weaken_link_cat(A0,A1) :-
    (   alpino_lc_in:weaken_link_cat(A0,A1)
    ->  true
    ;   A0 = A1,
        functor(A0,Fun,Arity),
        format(user_error,"warning: No weaken_link_cat for ~w/~w~n",[Fun,Arity] )
    ).

%% restrict each el in List to Depth
%% if Depth<0 then no restriction
%% if Depth=0: variable remains
restrict_list(L0,L1,Depth) :-
    (	var(L0)
    ->	L0=L1
    ;   restrict_list_(L0,L1,Depth)
    ).

restrict_list_([],[],_).
restrict_list_([H0|T0],[H1|T1],Depth) :-
    restrict(Depth,H0,H1),
    restrict_list(T0,T1,Depth).

restrict(Depth,Term,Restr) :-
    (	Depth < 1
    ->	true
    ;	var(Term)
    ->	Term=Restr
    ;	atomic(Term)
    ->	Term=Restr
    ;	functor(Term,F,Arity),
	functor(Restr,F,Arity),
	Depth1 is Depth-1,
	restrict_args(Arity,Depth1,Term,Restr)
    ).

restrict_args(Ar,Depth,Term,Restr) :-
    (	Ar < 1
    ->	true
    ;	arg(Ar,Term,ArgT),
	arg(Ar,Restr,ArgR),
	restrict(Depth,ArgT,ArgR),
	Ar1 is Ar-1,
	restrict_args(Ar1,Depth,Term,Restr)
    ).

transitive(Pairs) :-
    transitive(Pairs,4).

%% transitive(+Pairs,-NewPairs,+R)
%% Pairs and NewPairs: list of A-B pairs
%% R: integer indicating restriction level
transitive(Pairs0,R) :-
    retractall(alpino_util_tmp:pair(_,_)),
    retractall(alpino_util_tmp:bpair(_,_)),
    add_basic_pairs(Pairs0,R,Pairs1),
    transitive_n(Pairs1,R).

%% add_basic_pairs(+Pairs,+R,?UniqueRestrictedPairs,?UniquePairs)
add_basic_pairs([],_,[]).
add_basic_pairs([Pair|Pairs],R,URPairs):-
    add_basic_pair(Pair,R,URPairs,URPairs0),
    add_basic_pairs(Pairs,R,URPairs0).

add_basic_pair(A-B,R,UR,UR0) :-
    (	add_new_item(bpair(A,B))
    ->	restrict_pair(R,A,B,Ar,Br),
	(   add_new_item(pair(Ar,Br))
	->  UR = [Ar-Br|UR0]
	;   UR=UR0
	)
    ;	UR=UR0
    ).

transitive_n([],_).
transitive_n([Pair|Pairs],R):-
    debug_call(2,count_items(alpino_util_tmp)),
    findall(NPair,new_pair1([Pair|Pairs],R,NPair),Pairs1),
    transitive_n(Pairs1,R).

new_pair1(Pairs,R,New) :-
    member(Pair,Pairs),
    new_pair(Pair,R,New).

new_pair(A-B,R,Ar-Cr):-
    alpino_util_tmp:bpair(B,C),
    restrict_pair(R,A,C,Ar,Cr),
    add_new_item(pair(Ar,Cr)).

a_more_general(Item):-
    numbervars(Item,0,_),
    alpino_util_tmp:Item.

a_more_specific(Item,Ref):-
    copy_term(Item,Copy),
    alpino_util_tmp:clause(Copy,_,Ref),
    alpino_util_tmp:clause(Specific,_,Ref),
    subsumes_chk(Copy,Specific).

add_new_item(Item) :-
    \+	a_more_general(Item),
    (   a_more_specific(Item,SpecRef),
	erase(SpecRef),
	fail
    ;   true
    ),
    alpino_util_tmp:noclp_assertz(Item).

%% n_best(N,Score,Goal)
%% n_best(N,Score,Goal,Var)
%% returns each of the best solutions Var to goal upon backtracking,
%% starting with the best, and gradually generating the inferior
%% solutions. Each time 'Goal' succeeds, Score is supposed
%% to be instantiated. Score will be used to order the various results.
%% Variants are removed (if N>0).

%% this definition is supposed to be better than just collecting all
%% solutions, and then sorting, and producing the best N of the result.
%% In that naive solution, *all* solutions are kept in memory. In the
%% current solution there are at any point at most N solutions in
%% memory, which is much better if N is small.

n_best(N,Score,Goal) :-
    n_best(N,Score,Goal,Goal,List),
    member(Goal,List).

n_best(N,Score,Goal,Var) :-
    n_best(N,Score,Goal,Var,List),
    member(Var,List).

:- if(current_predicate(nb_setarg/3)).

%% contributed by Jan Wielemakers
n_best(N,Score,Goal,Var,List) :-
    (	N < 1
    ->	findall(Score-Var,Goal,KvList0),
	keysort(KvList0,KvList),
	remove_keys(KvList,List)
    ;	nb_best(N,Score,Goal,Var,List)
    ).

nb_best(N,Score,Goal,Var,Term) :-
    functor(Best,best,N),
    Scores = score(0, -),		% #Sols in bag, Highest score in bag
    (	call(Goal),
	update(Scores, Best, N, Score, Var),
	fail
    ;	Best =.. [_|List],
	arg(1,Scores,Count),
	length(Answers,Count),
	append(Answers,_,List),
	keysort(Answers,Sorted),
	remove_keys(Sorted,Term)
    ).

update(Scores,Best,Max,Score,Var) :-
    (   arg(1, Scores, 0)
    ->  nb_setarg(1,Scores,1),
	nb_setarg(2,Scores,Score),
	nb_setarg(1,Best,Score-Var)
    ;   arg(1,Scores,N),
	N < Max
    ->  (   arg(_,Best,Score-Val2),
	    variant(Val2,Var)
	->  true			% There is already a variant
	;   N2 is N + 1,
	    nb_setarg(1,Scores,N2),
	    arg(2,Scores,Worst),
	    (   Score @> Worst
	    ->  nb_setarg(2,Scores,Score)
	    ;   true
	    ),
	    nb_setarg(N2,Best,Score-Var)
	)
    ;   arg(2,Scores,Worst),
	Score @< Worst
    ->  (   arg(_,Best,Score-Val2),
	    variant(Val2,Var)
	->  true			% There is already a variant
	;   arg(I,Best,Worst-_)
	->  nb_setarg(I,Best,Score-Var),
	    worst_score(Best,Max,NewWorst),
	    (	arg(2,Scores,NewWorst)
	    ->	true
	    ;	nb_setarg(2,Scores,NewWorst)
	    )
	;   print([sc=Score,scs=Scores,h=Worst]), nl,
	    forall(arg(_,Best,Sc-_),(print(Sc),nl)),
	    assertion(false)
	)
    ;   true
    ).

worst_score(Best,Max,Worst) :-
	arg(1,Best,W0-_),
	worst_score(2,Max,Best,W0,Worst).

worst_score(I,Max,Best,W0,Worst) :-
	I =< Max, !,
	arg(I,Best,W1-_),
	I2 is I + 1,
	(   W0 @> W1
	->  worst_score(I2,Max,Best,W0,Worst)
	;   worst_score(I2,Max,Best,W1,Worst)
	).
worst_score(_,_,_,W,W).

:- else.

n_best(N,Score,Goal,Var,List) :-
    (	N < 1
    ->	findall(Score-Var,Goal,KvList0),
	keysort(KvList0,KvList),
	remove_keys(KvList,List)
    ;	gensym(Ref),
	bb_put(utils_n_best:Ref,[]),
	call_cleanup(n_best(N,Score,Goal,Var,Ref,List),
		     (	 bb_delete(utils_n_best:Ref,_),
			 freesym(Ref)
		     )
		    )
    ).


n_best(N,Score,Goal,Var,Ref,List) :-
    (	call(Goal),
	bb_get(utils_n_best:Ref,List0),
	add_n_best(List0,List,0,N,Score,Var,Goal),  % fails if not good enough
	bb_put(utils_n_best:Ref,List),
	fail
    ;	bb_get(utils_n_best:Ref,KvList),
	remove_keys(KvList,List)
    ).

add_n_best([],[Score-Goal],C,N,Score,Goal,_):-
    C < N.
add_n_best([Sc0-G0|Gs0],List,C0,C,Score,Goal,Msg):-
    C1 is C0 + 1,
    (	                        % it's the best now
	Score @< Sc0
    ->	List=[Score-Goal|List0],
	leave_n_best([Sc0-G0|Gs0],List0,C1,C,Score-Goal)
    ;	                        % it's not the best
	C1 < C,			% there must be space
        \+ terms:variant(Score-Goal,Sc0-G0),
        List = [Sc0-G0|List0],
	add_n_best(Gs0,List0,C1,C,Score,Goal,Msg)
    ).

leave_n_best([],[],_,_,_).
leave_n_best([H|T0],List,C0,C,Added) :-
    (	C0 < C
    ->  (   terms:variant(Added,H)
        ->  List = T0
        ;   List = [H|List1],
            C1 is C0 + 1,
            leave_n_best(T0,List1,C1,C,Added)
        )
    ;	List=[]
    ).

:- endif.

remove_keys([],[]).
remove_keys([_-V|Kvs],[V|Vs]) :-
    remove_keys(Kvs,Vs).

findall_atmost(Max,V,Goal,Vs) :-
    Max < 1, !,
    findall(V, Goal, Vs).
findall_atmost(Max,V,Goal,Vs) :-
    findall(V,atmost(Max,Goal),Vs).

%% sicstus has create_mutable etc, but the resulting
%% code is *much* slower with mutables than the old
%% version below. So, having the predicate is not
%% enough reason to use the first version, it is
%% per prolog a matter of experimenting.


atmost(0,Goal) :-
    !,
    call(Goal).
atmost(1,Goal) :-
    !,
    once(Goal).
atmost(Max,Goal) :-
    atmostX(Max,Goal).

:- if(current_prolog_flag(dialect,swi)).

atmostX(Max,Goal) :-
    State = count(0),
    Goal,
    arg(1, State, C0),
    C1 is C0+1,
    (   C1 == Max
    ->  !,
	debug_message(1,"atmost: found all ~w solutions~n",[Max,Goal])
    ;   nb_setarg(1, State, C1)
    ).

:- else.

atmostX(Max,Goal) :-
    gensym(Ref),
    bb_put(utils_sol:Ref,1),
    call_cleanup(atmost0(Max,Ref,Goal),
		 (   bb_delete(utils_sol:Ref,_),
		     freesym(Ref)
		 )
		).

atmost0(Max,Ref,Goal):-
    call(Goal),
    bb_get(utils_sol:Ref,N),
    (	N  =:= Max
    ->  !,
	debug_message(1,"atmost: found all ~w solutions~n",[Max])
    ;   N1 is N+1,
	bb_put(utils_sol:Ref,N1)
    ).

:- endif.

/*
:- use_module(library(terms), [ term_variables/2 ]).
term_constraints(Term,Constraints) :-
    term_variables(Term,Vars),
    vars_constraints(Vars,Constraints).

vars_constraints([],true).
vars_constraints([H|T],Cons) :-
    frozen(H,Cons1),
    (	Cons1==true
    ->	vars_constraints(T,Cons)
    ;   vars_constraints1(T,Cons1,Cons)
    ).

vars_constraints1([],C,C).
vars_constraints1([H|T],Cons0,Cons) :-
    frozen(H,Cons1),
    (	Cons1==true
    ->	vars_constraints1(T,Cons0,Cons)
    ;   vars_constraints1(T,(Cons0,Cons1),Cons)
    ).
*/


/*
%% binary heaps with maximum size implemented as Prolog terms...
%% new_heap(+Max,-Heap)
%% "better" : @<
%% fails if no more space for a new (bad scoring) value
%% NOT USED currently

new_heap(N,heap(Heap,1,N)) :-
    functor(Heap,h,N).

%% add_heap(+Key,?Val,+Heap0,-Heap)
add_heap(Key,Val,heap(H0,C0,Max),heap(H,C,Max)) :-
    (   C0 > Max
    ->  C0 = C,
        add_heap_last(H0,H,Key,Val,Max)
    ;   C is C0 + 1,
        arg(C0,H0,Key-Val),
        add_heap_bubble(H0,H,C0)
    ).

add_heap_last(H0,H,Key,Val,C) :-
    arg(C,H0,Key0-_Val0),
    (   Key @< Key0
    ->  copy_heap1(H0,H,C),
        arg(C,H,Key-Val)
    ;   fail % fails if not good enough; special property!!
        % H0=H
    ).

copy_heap1(H0,H,C) :-
    functor(H0,Fun,Ar),
    functor(H, Fun,Ar),
    copy_heap(H0,H,1,C,Ar).

copy_heap2(H0,H,C0,C) :-
    functor(H0,Fun,Ar),
    functor(H, Fun,Ar),
    copy_heap(H0,H,1,C0,C,Ar).

copy_heap(H0,H,P0,C0,C,P) :-
    (   P0 < C0
    ->  arg(P0,H0,Val),
        arg(P0,H, Val),
        P1 is P0+1,
        copy_heap(H0,H,P1,C0,C,P)
    ;   P1 is P0+1,
        copy_heap(H0,H,P1,C,P)
    ).

copy_heap(H0,H,P0,C,P) :-
    (   P0 < C
    ->  arg(P0,H0,Val),
        arg(P0,H, Val),
        P1 is P0+1,
        copy_heap(H0,H,P1,C,P)
    ;   P0 < P
    ->  P1 is P0+1,
        copy_heap(H0,H,P1,P)
    ;   true
    ).

copy_heap(H0,H,P0,P) :-
    (   P0 =< P
    ->  arg(P0,H0,Arg),
        arg(P0,H, Arg),
        P1 is P0 + 1,
        copy_heap(H0,H,P1,P)
    ;   true
    ).

add_heap_bubble(H0,H,C) :-
    arg(C,H0,Key-_Val),
    Mother is C//2,
    (   Mother < 1
    ->  H0 = H
    ;   arg(Mother,H0,Key1-_Val1),
        (   Key @< Key1
        ->  swap_heap(H0,H1,Mother,C),
            add_heap_bubble(H1,H,Mother)
        ;   H0=H
        )
    ).

swap_heap(H0,H,C0,C) :-
    copy_heap2(H0,H,C0,C),
    arg(C0,H0,Val0),
    arg(C, H, Val0),
    arg(C, H0,Val),
    arg(C0,H, Val).

%% for now
heap_to_list(heap(Heap,Cur,_Max),List) :-
    Heap =.. [_|Args],
    Len is Cur-1,
    length(SubList,Len),
    append(SubList,_,Args),
    keysort(SubList,List).

*/

/*
nth_solution(Max,Goal) :-
    gensym(Ref),
    bb_put(utils_sol:Ref,1),
    call_cleanup(nth_solution0(Max,Ref,Goal),
                 (   bb_delete(utils_sol:Ref,_),
                     freesym(Ref)
                 )
                ).

nth_solution0(Max,Ref,Goal):-
    call(Goal),
    bb_get(utils_sol:Ref,N),
    (	N  =:= Max
    ->  !
    ;   N1 is N+1,
	bb_put(utils_sol:Ref,N1),
        fail
    ).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Math utility predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

stddev(L,StdDev) :-
    average(L,Avg),
    dev_square(L,DS,Avg),
    sum_list(DS,DSSum),
    length(DS,Len),
    StdDev is sqrt((DSSum / Len)).

average(L,Avg) :-
    sum_list(L,Sum),
    length(L,Len),
    Avg is Sum / Len.

dev_square([],[],_).
dev_square([H|T],[DS|DST],Avg) :-
    Dev is H - Avg,
    DS is Dev ** 2,
    dev_square(T,DST,Avg).

inform_undefined_module(Module) :-
    prolog_flag(system_type,Type),
    inform_undefined_module(Type,Module,Module).

inform_undefined_module(Module1,Module2) :-
    prolog_flag(system_type,Type),
    inform_undefined_module(Type,Module1,Module2).

inform_undefined_module(runtime,_,_).
inform_undefined_module(development,Flag,Module) :-
    (   current_module(Module,File)
    ->  debug_message(1,
        "~w=undefined: nothing loaded, but ~w from ~w still available~n",
                                 [Flag,Module,File])
    ;   debug_message(1,
        "~w=undefined: nothing loaded, no ~w available~n",
                                 [Flag,Module])
    ).

:- meta_predicate time(?,:), time(:).
time(Goal) :-
    hdrug_flag(debug,Debug),
    time(Debug,Goal).

time(Debug,Module:Goal) :-
    (   Debug > 0
    ->  statistics(runtime,[Tstart,_]),
	call(Module:Goal),
	statistics(runtime,[Tend,_]),
	Time is Tend-Tstart,
	(   (  Time > 90 ; Debug > 2 )
	->  functor(Goal,Fun,Ar),
	    format(user_error,"~w msec ~w~n",[Time,Fun/Ar])
	;   true
	)
    ;   call(Module:Goal)
    ).

:- if(current_prolog_flag(dialect,swi)).

overrule_module(File) :-
    load_files(File,[redefine_module(true)]).

overrule_module(File,Imports) :-
    load_files(File,[redefine_module(true),imports(Imports)]).

:- else.

overrule_module(File) :-
    prolog_flag(redefine_warnings,OldState,off),
    call_cleanup(use_module(File),
		 prolog_flag(redefine_warnings,_,OldState)).

overrule_module(File,Imports) :-
    prolog_flag(redefine_warnings,OldState,off),
    call_cleanup(use_module(File,Imports),
		 prolog_flag(redefine_warnings,_,OldState)).

:- endif.

penalties(Cat,Tree,Frames,Score) :-
    hdrug_flag(disambiguation,OnOff),
    (	OnOff == off
    ->	no_score(Score)
    ;	(   alpino_penalties:assign_score(Cat,Tree,Frames,Score)
        ->  true
        ;   format(user_error,"error: alpino_penalties:assign_score/4 fails!~n",[]),
            fail
        )
    ).

no_score(p(0.0,[])).

%% default (for historical reasons): assume input is tokenized
:- initialize_flag(assume_input_is_tokenized,on).

codes_to_words_or_tokenize(Codes,Words) :-
    hdrug_flag(assume_input_is_tokenized,Flag),
    codes_to_words_or_tokenize(Flag,Codes,Words).

codes_to_words_or_tokenize(on,Codes,Words) :-
    codes_to_words(Codes,Words).
codes_to_words_or_tokenize(off,Codes,Words) :-
    alpino_tokenize:tokenize(Codes,Words).

codes_to_words(Codes,Words) :-
    split_string(Codes,[32],Words0),
    codes_words_to_words(Words0,Words,[]).

codes_to_words(Codes,Words,WordsT) :-
    split_string(Codes,[32],Words0),
    codes_words_to_words(Words0,Words,WordsT).

codes_words_to_words([],W,W).
codes_words_to_words([H|T],W0,W) :-
    codes_word_to_words(H,W0,W1),
    codes_words_to_words(T,W1,W).

codes_word_to_words([],W,W). % skip empty words (particularly at endofline)
codes_word_to_words([H|T],[Word|Ws],Ws) :-
    atom_codes(Word,[H|T]).

:- public format_word_list/1.

%% UTIL format_word_list(List)
%% write each element of List to standard output, separated by spaces
%% eg: format_word_list([a,b,c,d])
%% a b c d
format_word_list([]).
format_word_list([H|T]) :-
    format("~w",[H]),
    format_word_list1(T).

format_word_list1([]).
format_word_list1([H|T]) :-
    format(" ~w",[H]),
    format_word_list1(T).

format_gen_suite(Result,Id) :-
    alpino_adt:result_to_adt(Result,Adt),
    un_prettyvars(Adt,AdtCopy),       %% prettyvars/numbervars was applied 
    prettyvars(AdtCopy),              %% previously, giving pretty now-singletons
    format("~q.~n",[lf(Id,AdtCopy)]).

format_counted_features(His) :-
    count_features(His,Features),
    write_feature_list(Features).

write_feature_list([]) :-
    nl.
write_feature_list([H|T]) :-
    write_feature(H),
    write_feature_list0(T).

write_feature_list0([]) :-
    nl.
write_feature_list0([H|T]) :-
    write('|'),
    write_feature(H),
    write_feature_list0(T).

write_feature(Count-Feat) :-
    with_output_to_chars(format("~q",[Feat]),FeatCodes),
    atom_codes(FeatAtom,FeatCodes),
    alpino_format_syntax:escape_b(FeatAtom,FeatString),
    format("~w@~s",[Count,FeatString]).

count_features(List0,List) :-
    add_var(List0,List1),
    keysort(List1,List2),
    count_vals(List2,List).

count_vals([],[]).
count_vals([H-Count|T],List) :-
    count_vals(T,H,Count,List).

count_vals([],H,N,[N-H]).
count_vals([H-Count|T],P,N0,List) :-
    (	H == P
    ->	N1 is N0+Count,
	count_vals(T,H,N1,List)
    ;	List=[N0-P|List1],
	count_vals(T,H,Count,List1)
    ).      

add_var([],[]).
add_var([H0|T],[H|T1]) :-
    add_var_el(H0,H),
    add_var(T,T1).

add_var_el(Feature-Count,New) :-
    !,
    New = (Feature-Count).
add_var_el(Feature,Feature-1).

sort_not_uniq(L0,L) :-
    add_var(L0,L1),
    keysort(L1,L2),
    add_var(L,L2).

write_list([],_).
write_list([H|T],Str) :-
    write(Str,H),
    write_list0(T,Str).

write_list0([],_).
write_list0([H|T],Str):-
    tab(Str,1),
    write(Str,H), 
    write_list0(T,Str).

%% split_atom(+Atom,+Sep,-List)
%% Atom is an atom
%% Sep is a list of character codes of length 1
%% List is a list of atoms.
split_atom(Atom,Sep,List) :-
    atom_codes(Atom,Codes),
    split_string(Codes,Sep,List0),
    list_atoms(List0,List).

list_atoms([],[]).
list_atoms([Codes|T0],[Atom|T]) :-
    atom_codes(Atom,Codes),
    list_atoms(T0,T).

%% replace_sub_atom(Atom0,Atom,Sub0,Sub)
%% replace sub_atom Sub0 by Sub in Atom0 yielding Atom
%% e.g.
%% replace_sub_atom(eigenljik,eigenlijk,ji,ij).

replace_sub_atom(Word1, Word, Sub0, Sub) :-
    atom(Word1),
    atom(Sub0),
    atom(Sub),
    atom_length(Sub0,SubL),
    sub_atom(Word1,Before,SubL,_After,Sub0),
    sub_atom(Word1,0,Before,_,Prefix),
    NBefore is Before + SubL,
    sub_atom(Word1,NBefore,_,0,Suffix),
    hdrug_util:concat_all([Prefix,Sub,Suffix],Word).


%% split_string(+Chars,+Sep,-List)
%% Chars is a list of character codes
%% Sep is a list of character codes of length 1
%% List is a list of lists of character codes.
%%
%% inspired by Perl's split function, but separator must be single
%% character (of course, it is easy to extend this, but I didn't).
%%
%% example:
%%| ?- split("john kisses mary"," ",L).
%% L = [[106,111,104,110],[107,105,115,115,101,115],[109,97,114,121]] ?

split_string(Chars,Sep,List) :-
    split(Sep,List0,Chars,[]),
    List=List0. % output after cut, generic for whole dcg

split(Sep,List) -->
    field(Sep,H),
    split1(H,Sep,List).

split1([],Sep,List) -->
    e_split1(Sep,List).
split1([H|T],Sep,[[H|T]|List]) -->
    split2(Sep,List).

e_split1(Sep,[[],Next|List]) -->
    sep(Sep),
    !,
    field(Sep,Next),
    split2(Sep,List).
e_split1(_Sep,[]) --> [].

split2(Sep,[Next|Result]) -->
    sep(Sep),
    !,
    field(Sep,Next),
    split2(Sep,Result).
split2(_Sep,[]) --> [].

sep([Char]) -->
    [Char].

%% read all chars until | or end of line
field(Sep,[H|T]) -->
    [H],
    {  [H] =\= Sep },
    !,
    field(Sep,T).
field(_,[]) --> [].

%% flags which are specific per thread
%% this distinction is only relevant for SWI

:- if(current_prolog_flag(dialect,swi)).

initialize_thread_flag(Key,Val) :-
    create_prolog_flag(Key,Val,[type(term)]).

thread_flag(Key,Val) :-
    prolog_flag(Key,Val).

thread_flag(Key,Old,New) :-
    prolog_flag(Key,Old),
    set_prolog_flag(Key,New).

set_thread_flag(Key,New) :-
    set_prolog_flag(Key,New).

:- else.

initialize_thread_flag(Key,Val) :-
    initialize_flag(Key,Val).

thread_flag(Key,Val) :-
    hdrug_flag(Key,Val).

thread_flag(Key,Old,New) :-
    hdrug_flag(Key,Old,New).

set_thread_flag(Key,New) :-
    set_flag(Key,New).

:- endif.


%%% replace char Ca by char Cb everywhere in Atom0, resulting in Atom
tr(Ca,Cb,Atom0,Atom) :-
    atom_codes(Ca,[CA]),
    atom_codes(Cb,[CB]),
    atom_codes(Atom0,String0),
    tr0(String0,String,CA,CB),
    atom_codes(Atom,String).

tr0([],[],_,_).
tr0([H0|T0],[H|T],CA,CB) :-
    (  H0 =:= CA
    -> H = CB
    ;  H = H0
    ),
    tr0(T0,T,CA,CB).
