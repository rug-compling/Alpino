:- module(memo, [ memo/1, 
                  clean_up_memo/0,
		  memo_count/0,
		  memo_count/1,
		  memo_list/0
                ]).

:- meta_predicate memo(:).


memo(Pred) :-
	(  copy_term(Pred,PredNV),
	   numbervars(PredNV,0,_),  
	   'MEMO'(PredNV)
        -> true  %'MEMO_ITEM'(Pred,_)
        ; ( Pred,
  	    copy_term(Pred,Copy),
	    numbervars(Copy,0,_),
	    (  'MEMO_ITEM'(Copy,_)
            -> fail
            ;  retractall('MEMO_ITEM'(_,Pred)),
	       assertz('MEMO_ITEM'(Pred,Copy))
            ),
            fail
          ; assertz('MEMO'(Pred))
            % 'MEMO_ITEM'(Pred,_)
          )
        ),
	'MEMO_ITEM'(Pred,_).


%%%% PROBLEM: cyclic unifications

%%% 
% example:
% program: x(A,B).
%
% goal: x(f(X),X).  => table:   x(f(X),X)
%
% next goal: x(Y,Y).  => new table:  x(Y,Y).
%
% collect result for second goal:  CRASH.......

% ONE solution is to weaken goals before they are input to memo, i.e.:
%
% weaken(Goal0,Goal),
% memo(Goal),
% Goal0=Goal
%
% where weaken produces a less informative copy of the original goal
% (e.g. by replacing all variables by fresh variables..) Note that this
% often is more efficient anyway..


/* THE ORIGINAL VERSION IS EASIER TO UNDERSTAND:
memo(Pred) :-
	finished_memo(Pred),
	!,
	select_item(Pred).
memo(Pred) :-
	Pred,
	add_item(Pred),
	fail.
memo(Pred) :-
	assertz('MEMO'(Pred)),
	select_item(Pred).

add_item(Pred):-
	copy_term(Pred,Copy),
	numbervars(Copy,0,_),
	add_item(Pred,Copy).

select_item(Pred):-
	'MEMO_ITEM'(Pred,_).

% case 1: a more general item already exists
add_item(_Pred,Copy):-
	'MEMO_ITEM'(Copy,_),!,fail.	

% case 2: assert item, remove all more specific items
add_item(Pred,Copy):-
	remove_items(Pred),
	assertz('MEMO_ITEM'(Pred,Copy)).

remove_items(Pred) :-
	retractall('MEMO_ITEM'(_,Pred)).

finished_memo(Pred) :-
	copy_term(Pred,PredNV),
	numbervars(PredNV,0,_),  % a more general goal has already been tried
	'MEMO'(PredNV).

*/

% Auxiliaries:
clean_up_memo :-
	retractall('MEMO'(_)),
	retractall('MEMO_ITEM'(_,_)).

memo_count(MI) :-
	hdrug_util:count_edges(memo:'MEMO_ITEM'(_,_), MI).

memo_count :-
	hdrug_util:count_edges(memo:'MEMO'(_), M),
	hdrug_util:count_edges(memo:'MEMO_ITEM'(_,_), MI),
	write(M),write(' MEMO edges'),nl,
	write(MI),write(' MEMO_ITEM edges'),nl.

memo_list :-
	listing('MEMO'),
	listing('MEMO_ITEM').






