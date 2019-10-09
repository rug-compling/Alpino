:- module(alpino_wappend, [ wappend/3,
			    wselect/3,
			    wmember/2,
			    wwselect/3,
			    wappend_atmost1/3,
			    wwmember/2,
			    nomember/2,
			    unify_inout/1,
			    wsublist/2,
			    wcheck_els/2,
			    wcheck_els_svp/2,
			    skip_subject_list/2,
			    add_at_end_list/3,
			    add_at_end/3
			  ]).

:- expects_dialect(sicstus).

:- use_module(hdrug(hdrug_util)).
:- use_module(library(lists)).

wappend_atmost1(L0,L1,L) :-
    (   L0 == L
    ->	L1 = []
    ;	L1 == L
    ->	L0 = []
    ;	when((nonvar(L0);nonvar(L1);nonvar(L)),
	     wappend_atmost10(L0,L1,L)
	    )
    ).

%% first argument known --> deterministic
%% second argument known --> deterministic
%% third argument known --> if empty list then deterministic
% :- block wappend_atmost10(-,-,-).
wappend_atmost10(L0,L1,L) :-
    (	nonvar(L0)
    ->  wappend_atmost1__1(L0,L1,L)
    ;	nonvar(L1)
    ->	wappend_atmost1__1(L1,L0,L)
    ;	L == []
    ->	L0 = [], L1 = []
    ;	wappend_atmost3(L0,L1,L)
    ).

wappend_atmost1__1([],L,L).
wappend_atmost1__1([H|T],[],[H|T]).

wappend_atmost3([],[H],[H]).
wappend_atmost3([H],[],[H]).


%%%% WAPPEND. Now maintains a counter, in order to prevent non-termination.
%%%% Non-termination occurs (very seldomly) in a variety of circumstances.
%%%% Example 1:
%%%% wappend(X,[a],[b|X])
%%%%
%%%% Example 2:
%%%% wappend([A|C],B,D), wappend(D,B,C).

:- public wappend/3, wselect/3, wmember/2, wwselect/3, wappend_atmost1/3,
          wwmember/2, nomember/2, unify_inout/1.

wappend(A,B,C) :-
    hdrug_flag(wappend_max,Max),
    wappend(A,B,C,Max).

wappend(L0,L1,L,N) :-
    (   L0 == L
    ->	L1 = []
    ;	L1 == L
    ->	L0 = []
    ;   when((nonvar(L0);nonvar(L1);nonvar(L)),
	     wappend0(L0,L1,L,N)
	    )
    ).

:- initialize_flag(wappend_max,100).

%:- block wappend0(-,-,-,?).
wappend0(L0,L1,L,N) :-
    (	L1 == []
    ->	L0 = L
    ;   L0 == L
    ->	L1 = []
    ;	L1 == L
    ->	L0 = []
    ;	L0==[]
    ->	L1=L
    ;   N1 is N-1,
	(   N1 > 0
	->  when((nonvar(L0);nonvar(L)),
		 wappend1(L0,L1,L,N1)
		)
	;   debug_message(2,
				     "warning: wappend: > wappend_max!~n",[]),
	    fail
	)
    ).

% :- block wappend1(-,?,-,?).
wappend1([],X,X,_).
wappend1([H|T],X,[H|T2],N):-
    wappend(T,X,T2,N).

wmember(Element, [Head|Tail]) :-
	when(nonvar(Tail),
	     wmember(Tail, Head, Element)
	    ).

%:- block wmember(-,?,?).
wmember(_, Element, Element).
wmember([Head|Tail], _, Element) :-
	when(nonvar(Tail),
	     wmember(Tail, Head, Element)
	    ).

wwmember(Element, [Head|Tail]) :-
	when((nonvar(Tail),nonvar(Element)),
	     wwmember(Tail, Head, Element)
	    ).

% :- block wwmember(-,?,?), wwmember(?,?,-).
wwmember(_, Element, Element).
wwmember([Head|Tail], _, Element) :-
	when((nonvar(Tail),nonvar(Element)),
	     wwmember(Tail, Head, Element)
	    ).

% :- block wselect(-,?,?,?).
wselect(Element, [Head|Tail],Rest) :-
	when(nonvar(Tail),wselect(Tail, Head, Element,Rest)).

wselect(T, Element, Element,T).
wselect([Head|Tail], First, Element,[First|Rest]) :-
	when(nonvar(Tail),wselect(Tail, Head, Element,Rest)).

wwselect(Element,List,Rest) :-
    when(nonvar(List),wwselect_(Element,List,Rest)).

% :- block wwselect(?,-,?).
wwselect_(Element, [Head|Tail],Rest) :-
    when(nonvar(Tail),
	 wwselect(Tail, Head, Element,Rest)
	).

% :- block wwselect(-,?,?,?).
wwselect(T, Element, Element,T).
wwselect([Head|Tail], First, Element,[First|Rest]) :-
	when(nonvar(Tail),
	     wwselect(Tail, Head, Element,Rest)
	    ).

nomember(_W,[]).
nomember(W,[X|Tail]) :-
    dif(W,X),
    nomember(W,Tail).

%:- block nomember_iflist(?,-).
%nomember_iflist(_,[]).
%nomember_iflist(El,[H|T]) :-
%    dif(El,H),
%    nomember_iflist(El,T).

:- public add_at_end/3, add_at_end_list/3.
add_at_end(A,B,C) :-
    hdrug_flag(wappend_max,Max),
    when(nonvar(B),
         add_at_end(A,B,C,Max)
	).

%:- block add_at_end(?,-,?,?).
add_at_end(El,List,[El|List],_) :-
    skip_subject_list(List,El).
add_at_end(El,[H|T],[H|List],N) :-
    N1 is N-1, N1 > 0,
    when(nonvar(T),add_at_end(El,T,List,N1)).

add_at_end_list(A,B,C,D) :-
    when((nonvar(A),nonvar(B)),
	 add_at_end_list_(A,B,C,D)
	).

%:- block add_at_end_list(-,?,?,?),
%         add_at_end_list(?,-,?,?).
add_at_end_list_(Add,List0,List,N) :-
    (   List0 == []
    ->  Add = List
    ;   when(nonvar(Add),add_at_end_list1(Add,List0,List,N))
    ).

%:- block add_at_end_list1(-,?,?,?).
add_at_end_list1([],List,List,_).
add_at_end_list1([H|T],List0,List,N) :-
    N1 is N-1, N1 > 0, 
    when(nonvar(List0),add_at_end(H,List0,List1,N1)),
    when((nonvar(T),nonvar(List1)),
	 add_at_end_list(T,List1,List,N1)).

add_at_end_list(Add,List0,List) :-
    hdrug_flag(wappend_max,Max),
    add_at_end_list(Add,List0,List,Max).

% :- block skip_subject_list(-,?).

skip_subject_list(L,El) :-
    when(nonvar(L),skip_subject_list_(L,El)).

skip_subject_list_([],_).
skip_subject_list_([H|T],El) :-
    alpino_data:precedes_subject_cat(H,El),
    when(nonvar(T),skip_subject_list_(T,El)).

%% every member of Guess must be from Given,
%% and vv

wcheck_els(Guess,Given) :-
    (   Given == []
    ->  Guess = []
    ;   Given = [H|T],
	(   T == []
	->  Guess = [H]
	;   when(nonvar(Guess),wcheck_els0(Guess,Given))
	)
    ).

%:- block wcheck_els0(-,?).
wcheck_els0([],[]).
wcheck_els0([H|T],List) :-
    select(H,List,List1),
    wcheck_els(T,List1).

% wcheck_els_svp(Own,Input)
%% every member of Own must be either from Input or be a particle
%% atmost 1 can be particle
%:- block wcheck_els_svp(-,?).

wcheck_els_svp(L0,L) :-
    when(nonvar(L0), wcheck_els_svp_(L0,L)).

wcheck_els_svp_([],[]).
wcheck_els_svp_([H|T],List) :-
    alpino_data:dt(H,_,Frame,_,_),
    when(nonvar(Frame),wcheck_els_svp(T,Frame,H,List)).

% :- block wcheck_els_svp(?,-,?,?).
wcheck_els_svp(T,Frame,H,List) :-
    (   Frame = particle(_),
	wcheck_els(T,List)
    ;   select(H,List,List1),
        when(nonvar(T),wcheck_els_svp_(T,List1))
    ).

%:- block unify_inout(-).

unify_inout(L) :-
    when(nonvar(L), unify_inout_(L)).

unify_inout_([]).
unify_inout_([H|T]) :-
    when(nonvar(H),alpino_data:inout(H)),
    unify_inout(T).

% :- block wsublist(-,?).
wsublist(List0,List) :-
    when(nonvar(List0),
	 wsublist_(List0,List)).

wsublist_([],_).
wsublist_([H|T],List) :-
    wwselect(H,List,List1),
    when(nonvar(T),wsublist_(T,List1)).

wsubseq(List0,List) :-
    when(nonvar(List0),
	 wsubseq_(List0,List)).

wsubseq_([],_).
wsubseq_([H|T],List) :-
    when(nonvar(List),
	 wwsubseq_(H,List,List1)),
    when(nonvar(T),wsubseq_(T,List1)).

wwsubseq_(H,[H|List],List).
wwsubseq_(H,[_|List0],List) :-
    when(nonvar(List0),
	 wwsubseq_(H,List0,List)).
