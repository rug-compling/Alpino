%%           -*-Mode: prolog;-*-

:- module( alpino_robust, [ robust/2 ] ).

:- expects_dialect(sicstus).

:- use_module(hdrug(hdrug_util)).
:- use_module(utils).
:- use_module(library(lists)).

%% segfault:
%%:- use_module(hdrug_util,hdrug_util,all).

%% find a sequence of edges (categories and transitions) from left
%% to right thru the sentence. 
%%  - Minimize the number of skips.             (smallest nr of skips)
%%  - Minimize the number of chunks,            (smallest nr of chunks)
%%  - Minimize penalties (assigned by penalties/3).
%% 
%% This version implements the DAG-SHORTEST-PATH algorithm, page
%% 536ff. of Cormen, Leiserson and Rivest, Introduction to Algorithms,
%% 1990, MIT press. This algorithm finds shortest paths for directed acyclic 
%% graphs. Scores are allowed to be negative. 
%%
%% The implementation slightly generalizes this algorithm to be able to
%% obtain the N best solutions (N can be set): a table is maintained 
%% (a m_array) which keeps track of the N best paths to each state, rather
%% than simply the best path.

%% NOTE that there is also `local' disambiguation taking place
%% in the second phase of the parser. Cf. the flag disambiguation_beam.

robust(Input,Object) :-
    hdrug_flag(number_analyses,Number0),
    (	integer(Number0),
	Number0 > 0
    ->	Number0 = Number
    ;	Number = 0
    ),
    hdrug_flag(robustness,Rob),
    hdrug_flag(disambiguation,Sort),
    hdrug_flag(robustness_allows_skips,Skips),
    hdrug_flag(unpack_bestfirst,Unpack),
    retractall(history(_,_,_,_,_,_,_,_,_)),
    robust(Input,Object,Number,Rob,Skips,Unpack,Sort).

:- dynamic history/9.

robust(Input,Object,Number,Rob,Skips,Unpack,Sort):-
    dag_shortest_path_algorithm(Rob,Skips,Objects,Input),
    select_object(Sort,Objects,Object,Number,Input,Unpack).

dag_shortest_path_algorithm(Rob0,Skips,Objects,Input) :-
    input_final(Input,F),
    is_required(Rob0,Rob,F),
    dag_shortest_path_algorithm_if(Rob,Skips,Objects,Input,F).

dag_shortest_path_algorithm_if(on,Skips,Objects,Input,F) :-
    initialize_item(ITEM,Input),
    relax(ITEM,_Done0,Done1),
    dag_shortest_path(0,F,Done1,Done,Skips,Input),
    u_array_get(F,Objects0,[],Done),
    debug_message(1,"recover robust histories ....~n",[]),
    hdrug_flag(disambiguation_beam,Beam),
    findall_atmost(Beam,Object,recover_histories(Objects0,Object,F),Objects),
    length(Objects,Len),
    debug_message(1,"recover robust histories done (~w results)~n",[Len]).

dag_shortest_path_algorithm_if(off,_,Objects,_,F) :-
    findall(Object,spanning_object(F,Object),Objects).

spanning_object(P,
		Total-item(score(Total,S1,C1),[Item])) :-
    max_category_first_or_allowed_skips(0,P,i(P,S1,C1,Item)),
    score(S1,C1,Total).

dag_shortest_path(V0,V,Done0,Done,Skips,Input) :-
    (	V0=:=V
    ->	Done0=Done
    ;	debug_message(3,"robust ~w/~w~n",[V0,V]),
	next_states(V0,V,Done0,Input,New,Skips),
	length(New,Length),
	debug_message(3,"robust ~w/~w ~w news~n",[V0,V,Length]),
	debug_message(4,"robust ~w ~n",[New]),
	relax_all(New,Done0,Done1),
	V1 is V0+1,
    	dag_shortest_path(V1,V,Done1,Done,Skips,Input)
    ).

%%%%%%%%%%%%%%%%
%% relaxation %%
%%%%%%%%%%%%%%%%

relax_all([],Done,Done).
relax_all([H|T],Done0,Done) :-
    relax(H,Done0,Done1),
    relax_all(T,Done1,Done).

%% Nr: maximum number of items per state
%% Best: if on-> only keep track of results that are maximally ok
%%         off-> keep track of Nr best results, however bad
%%         Int-> keep track of those results that are within Int of best
relax(Total-f(State,Item),Done0,Done) :-
    u_array_put(State,List0,[],[ListH|ListT],Done0,Done),
    insert_if_worth_best(List0,[ListH|ListT],Total,Item).

%% insert_if_worth(ListOfItems,NewListOfItems,NewItem,N0,N) 
%% add NewItem to the ListOfItems, and then take the Nr-length
%% prefix of the resulting list.
insert_if_worth_best([],[El-Item],El,Item).
insert_if_worth_best([Sc0-Item0|T0],[Sc-Item|T],Sc1,Item1) :-
    compare(Op,Sc0,Sc1),
    insert_if_worth0_best(Op,Sc0,Item0,Sc1,Item1,Sc,Item,T0,T).

insert_if_worth0_best(<,Sc,Item,_,_,Sc,Item,T,T).
insert_if_worth0_best(=,Sc,Item,Sc1,Item1,Sc,Item,T0,T) :-
    insert_if_worth_best(T0,T,Sc1,Item1).
insert_if_worth0_best(>,_,_,Sc,Item,Sc,Item,_,[]).

%%%%%%%%%%%%%%%%%%
%% finalization %%
%%%%%%%%%%%%%%%%%%

select_object(on,List,Obj,Number,Input,Unpack) :-
    n_best(Number,
	   Score,
	   select_object(List,Score,Obj,on,Number,Input,Unpack),
	   Obj
	  ).

select_object(off,List,Obj,Number,Input,Unpack) :-
    (   Number == 0
    ->  select_object(List,_,Obj,off,Number,Input,Unpack)
    ;   Number == 1
    ->  once(select_object(List,_,Obj,off,Number,Input,Unpack))
    ;   atmost(Number,
	       select_object(List,_,Obj,off,Number,Input,Unpack)
	      )
    ).

select_object(List,Sc,Obj,Dis,Number,Input,Unpack) :-
    member(_-item(_,Cats),List),
    final_object(Cats,Sc,Obj,Dis,Number,Input,Unpack).

final_object(Cats0,Pen,Obj,Dis,Number,Input,Unpack) :-
    reverse(Cats0,Cats),
    alpino_lexical_analysis:remove_brackets(Input,InputUnb),
    alpino_data:result_term(Pen,InputUnb,Cat,tree(robust,robust,Ds,CachePen),Frames,Obj),
    extract(Cats,Ds,Fs,_Words,Frames,Dis,Number,Unpack),
    alpino_data:robust_list_to_cat(Fs,Cat),
    penalties(Cat,tree(robust,robust,Ds,CachePen),Frames,Pen).

extract([],[],[],[],[],_,_,_).
extract([H|T],Ds,Fs,Words,Frs,Dis,Number,Unpack):-
    extract_one_indep(Dis,Unpack,Number,H,Ds,Ds1,Fs,Fs1,Words,Words1,Frs0),
    lists:append(Frs0,Frs1,Frs),
    extract(T,Ds1,Fs1,Words1,Frs1,Dis,Number,Unpack).

extract_one_indep(off,_,Number,H,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs) :-
    (   Number == 0
    ->  extract_one(H,_,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs)
    ;   Number == 1
    ->  once(extract_one(H,_,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs))
    ;   atmost(Number,
	       extract_one(H,_,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs)
	      )
    ).
extract_one_indep(on,Unpack,Number,H,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs) :-
    extract_one_indep_disamb(Unpack,Number,H,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs).

extract_one_indep_disamb(on,Number,H,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs) :-
    (   Number == 0
    ->  extract_one_with_score(H,_,i(Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs))
    ;   Number == 1
    ->  once(extract_one_with_score(H,_,i(Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs)))
    ;   atmost(Number,
	       extract_one_with_score(H,_,i(Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs))
	      )
    ).

extract_one_indep_disamb(off,Number,H,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs) :-
    n_best(Number,
	   Score,
	   extract_one_with_score(H,Score,
				  i(Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs)),
	   i(Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs)
	  ).

extract_one_with_score(H,Score,i(Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs)):-
    extract_one(H,Score,Ds0,Ds,Fs0,Fs,Ws0,Ws,Frs),
    (   var(Score)
    ->  Ds0 = [D|_],
	Fs0 = [F|_],
	penalties(F,D,Frs,Score)
    ;   true
    ).

extract_one(skip(W,P0,P),0,[Tree|Ds],Ds,Fs,Fs,[W|L],L,[Frame]) :-
    create_skip_tree(P0,P,W,Tree,Frame).
extract_one(ignore_skip(_,_),0,Ds,Ds,Fs,Fs,L,L,[]).
extract_one(allowed_skips(P0,P,W),0,[Tree|Ds],Ds,Fs,Fs,[W|L],L,[Frame]) :-
    create_skip_tree(P0,P,W,Tree,Frame).
extract_one(first(P0,P),Pen,[Tree|Ds],Ds,[Cat|Fs],Fs,Ws,Ws1,Frs):-
    alpino_lc:max_category_second(P0,P,Cat,Ws0,Frs,Tree,Pen),
    append(Ws0,Ws1,Ws).

create_skip_tree(P0,P,W,tree(skip,robust_skips(W),lex(W),_),
		 frame(P0,P,R0,R,W,robust_skip,W,robust_skip)) :-
    %%% hack
    (   alpino_lexical_analysis:tag(P0,P,R0,R,_,_,_,_)
    ->  true
    ;   alpino_lexical_analysis:rpos(P0,R0),
	Pd is P-P0,
	R is R0+Pd
    ;   P0=R0, P=R	  % wrong, but has worked for years until 2020
    ).

%%%%%%%%%%%%%%%%%%%%
%% initialization %%
%%%%%%%%%%%%%%%%%%%%

initialize_item(0-f(0,score(0,0,0)),_).

%%%%%%%%%%%%%%%%
%% next_state %%
%%%%%%%%%%%%%%%%

%% how to move in the search space.
%% a. by skipping a word
%% b. by using category as found by parser

next_states(St0,St,Done,Input,Items,Skips):-
    u_array_get(St0,Items0,[],Done),
    next_skips(Skips,St0,Input,Hs,Hs0),
    next_cats(St0,St,Hs0),
    combine_items(Items0,Hs,Items1,St0),
    sort(Items1,Items).

next_skips(on,St0,Input,Ts0,Ts):-
    input_word(St0,Input,Candidates),
    next_skips_candidates(Candidates,St0,Ts0,Ts).
next_skips(off,_,_,Hs,Hs).

next_skips_candidates([],_,Ts,Ts).
next_skips_candidates([Q-Word|Qs],Q0,Ts0,Ts) :-
    next_skips_candidate(Q0,Q,Word,Ts0,Ts1),
    next_skips_candidates(Qs,Q0,Ts1,Ts).

next_skips_candidate(Q0,Q,Word,Ts0,Ts) :-
    (   alpino_lexical_analysis:skips(Q0,Q,Word)
    ->  Ts0=Ts
    ;   Ts0=[i(Q,1,1,skip(Word,Q0,Q))|Ts]
    ).

%% [x1...xi] [,] [xj...xn]
%% should not be worse than
%% [x1...xi,xj..xm] [xm..xn]
allow_skip(',').

next_cats(St0,_,Hs) :-
    findall(Item,max_category_first_or_allowed_skips(St0,_,Item),Hs).

max_category_first_or_allowed_skips(St0,St,i(St,0,1,first(St0,St))) :-
    alpino_lc:max_category_first(St0,St).
max_category_first_or_allowed_skips(St0,St,i(St,0,SKIP,allowed_skips(St0,St,Word))) :-
    alpino_lexical_analysis:skips(St0,St,Word),
    (   allow_skip(Word)
    ->  SKIP=0
    ;   SKIP=1
    ).
max_category_first_or_allowed_skips(St0,St,i(St,0,0,ignore_skip(St0,St))) :-
    alpino_lexical_analysis:open_bracket(St0,St,_).
max_category_first_or_allowed_skips(St0,St,i(St,0,0,ignore_skip(St0,St))) :-
    alpino_lexical_analysis:close_bracket(St0,St,_).

is_required(on,on,_).
is_required(off,off,_).
is_required(undefined,Rob,P) :-
    is_required(if_required,Rob,P).
is_required(if_required,Rob,P) :-
    (	max_category_first_or_allowed_skips(0,P,_)
    ->	Rob=off
    ;	Rob=on
    ).

combine_items([],_,[],_).
combine_items([_-Item0|Items0],Hs,NItems,St0) :-
    combine_items0(Hs,Item0,NItems,NItems0,St0),
    combine_items(Items0,Hs,NItems0,St0).

combine_items0([],_,N,N,_).
combine_items0([H|T],Item0,Ns0,Ns,St0) :-
    combine_item(H,Item0,Ns0,Ns1,St0),
    combine_items0(T,Item0,Ns1,Ns,St0).

/*
combine_item(i(St,S1,C1,Cat), item(score(_,S0,C0), Cs),
	     [Total-f(St,item(score(Total,S,C),[Cat|Cs]))|T],T):-
    S is S0+S1,
    C is C0+C1,
    score(S,C,Total).
*/

combine_item(i(St,S1,C1,Cat), score(Total0,S0,C0),
	     [Total-f(St,score(Total,S,C))|T],T,St0):-
    S is S0+S1,
    C is C0+C1,
    score(S,C,Total),
    add_history(St,Total,S,C,Cat,St0,Total0,S0,C0).

add_history(St,Total,S,C,Cat,St0,Total0,S0,C0) :-
    (   history(St,Total,S,C,Cat,St0,Total0,S0,C0)
    ->  true
    ;   assertz(history(St,Total,S,C,Cat,St0,Total0,S0,C0))
    ).

%% score(+Skips,+Cats,?TotalScore).
%% totalscore must be a term such that standard ordering of terms
%% corresponds to ordering of totalscores.
score(Skip,Step,Score) :-
    Score is Skip + Step.

input_final(Input,Final) :-
    length(Input,Final).

input_word(P,Input,Qs) :-
    findall(Q1-Word1,input_word_option(P,Q1,Word1,Input),Qs0),
    sort(Qs0,Qs).

%% known skips
input_word_option(P,Q,Word,Input) :-
    alpino_lexical_analysis:user_skips(List),
    member(mwu(P,Q),List),
    length(Prefix,P),
    append(Prefix,Suffix,Input),
    L is Q-P,
    length(WordList,L),
    append(WordList,_,Suffix),
    concat_all(WordList,Word,' ').

%% prefer mwu over individual words
input_word_option(P,Q,Word,_Input) :-
    alpino_lexical_analysis:tag(P,Q,_,_,_,Word,His,_),
    \+ His = skip(_,_,_,_).

%% if all else fails
input_word_option(P,Q,Word,Input) :-
    nth0(P,Input,Word),
    Q is P+1.

u_array_get(Index0,Val,Default,Thing):-
    Index is Index0+1,
    u_array_get0(Index,Val0,Thing),
    (	var(Val0)
    ->	Val=Default
    ;   Val=Val0
    ).

%% 15 + 16
u_array_get0(Index,Val,' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE)):-
    (	Index < 16
    ->	arg(Index,' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),Val)
    ;   IndK is (Index /\ 15)+16,
	IndJ is Index >> 4,
	arg(IndK,' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),Son),
	u_array_get0(IndJ,Val,Son)
    ).

u_array_put(Index0,OldVal,Default,Val,Tree,NewTree):-
    Index is Index0+1,
    u_array_put0(Index,Val,Tree,NewTree,OldVal0),
    (   var(OldVal0)
    ->  OldVal=Default
    ;   OldVal=OldVal0
    ).

%% 15 + 16
u_array_put0(Index,Val,' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),NewTree,OldVal):-
    (	Index < 16
    ->	replace_arg(Index,' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),OldVal,NewTree,Val)
    ;   IndK is (Index /\ 15)+16,
	IndJ is Index >> 4,
	replace_arg(IndK,' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),Son,NewTree,NewSon),
	u_array_put0(IndJ,Val,Son,NewSon,OldVal)
    ).

replace_arg(1,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  A,
  ' nk '(XX,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(2,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  B,
  ' nk '(A,XX,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(3,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  C,
  ' nk '(A,B,XX,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(4,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  D,
  ' nk '(A,B,C,XX,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(5,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  E,
  ' nk '(A,B,C,D,XX,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(6,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  F,
  ' nk '(A,B,C,D,E,XX,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(7,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  G,
  ' nk '(A,B,C,D,E,F,XX,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(8,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  H,
  ' nk '(A,B,C,D,E,F,G,XX,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(9,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  I,
  ' nk '(A,B,C,D,E,F,G,H,XX,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(10,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  J,
  ' nk '(A,B,C,D,E,F,G,H,I,XX,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(11,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  K,
  ' nk '(A,B,C,D,E,F,G,H,I,J,XX,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(12,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  L,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,XX,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(13,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  M,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,XX,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(14,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  N,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,XX,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(15,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  O,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,XX,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(16,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  P,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,XX,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(17,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  Q,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,XX,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(18,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  R,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,XX,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(19,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  S,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,XX,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(20,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  T,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,XX,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(21,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  U,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,XX,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(22,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  V,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,XX,W,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(23,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  W,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,XX,X,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(24,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  X,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,XX,Y,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(25,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  Y,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,XX,Z,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(26,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  Z,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,XX,AA,BB,CC,DD,EE),
  XX).	    
replace_arg(27,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  AA,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,XX,BB,CC,DD,EE),
  XX).	    
replace_arg(28,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  BB,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,XX,CC,DD,EE),
  XX).	    
replace_arg(29,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  CC,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,XX,DD,EE),
  XX).	    
replace_arg(30,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  DD,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,XX,EE),
  XX).	    
replace_arg(31,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE),
  EE,
  ' nk '(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,XX),
  XX).	    

:- multifile user:portray/1.

user:portray(' nk '(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)) :-
    write('<n-k-tree>').

recover_histories(List,Object,St) :-
    member(Object0,List),
    recover_history(Object0,Object,St).

recover_history(T-score(T,S,C),T-item(score(T,S,C),His), St0):-
    debug_message(3,"recovering ~w ~w~n",[score(T,S,C),St0]),
    rec_history(St0,T,S,C,His),
    debug_message(3,"recovered ~w ~w~n",[score(T,S,C),St0]).

rec_history(0,_T,_S,_C,His) :-
    !,
    His = [].
rec_history(St,T,S,C,[Cat|His]) :-
    history(St,T,S,C,Cat,St1,T1,S1,C1),
    rec_history(St1,T1,S1,C1,His).

    