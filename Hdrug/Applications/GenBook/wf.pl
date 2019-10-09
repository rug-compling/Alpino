% wf.pl
% to memo-ize a relation, with indexing on goal
%
% note that no check exists to see whether a result has already
% been found. 
% This is OK (?), but if the original program is not
% minimal, then the same spurious ambiguities will be delivered
% here.
% This is not OK, for applications with structure sharing (where
% you explicitly make use of the fact that some computations will
% eventually collapse). Use library(memo) in that case. 

:- module(wf,[ wf/1,
               clean_up_wf/0,
	       wf_count/0,
	       wf_count/1,
	       wf_list/0
             ]).

:- meta_predicate wf(:).

wf(Goal):-
	copy_term(Goal,Frozen),
	numbervars(Frozen,0,_),
	(  'WF'(Frozen)
	-> true
        ;  ( copy_term(Goal,Result),
	     retractall('WF_ITEM'(_,Goal,_)),
	     call(Result),
	     assertz('WF_ITEM'(Goal,Frozen,Result)),
	     fail
           ; assertz('WF'(Goal))
           )
        ),
        'WF_ITEM'(Frozen,_,Goal).

clean_up_wf :-
	retractall('WF'(_)),
	retractall('WF_ITEM'(_,_,_)).

wf_count(WI):-
	hdrug_util:count_edges( wf:'WF_ITEM'(_,_,_), WI).

wf_count :-
	hdrug_util:count_edges(wf: 'WF'(_), W),
	hdrug_util:count_edges(wf: 'WF_ITEM'(_,_,_), WI),
	write(W), write(' WF edges'),nl,
	write(WI),write(' WF_ITEM edges'),nl.

wf_list :-
	listing('WF'),
	listing('WF_ITEM').

/* EASY and equivalent (I hope) version:
wf(Goal):-
	copy_term(Goal,Frozen),
	numbervars(Frozen,0,_),
	wf(Goal,Frozen).

wf(Goal,Frozen) :-
	wf_finished(Frozen),
	!,
	result(Frozen,Goal).
wf(Goal,Frozen) :-
	copy_term(Goal,Result),
	retract_results_of_more_specific_goal(Goal),
	call(Result),
	assertz('WF_ITEM'(Goal,Frozen,Result)),
	fail.
wf(Goal,Frozen) :-
	assert_wf_finished(Goal),
	result(Frozen,Goal).

%% new: only pick up results from this, or more general, goal. 
result(Frozen,Goal) :-
	'WF_ITEM'(Frozen,_,Goal).

% Each result knows from
% which goal it has been built; if a more general goal occurs later
% then results are to be retracted again.

wf_finished(Frozen) :-
	'WF'(Frozen).

assert_wf_finished(Pred) :-
	assertz('WF'(Pred)).

retract_results_of_more_specific_goal(Goal):-  %Goal is already numbervarred!
	retractall('WF_ITEM'(_,Goal,_)).
*/



