%%%%%%%%%%%%%%%%%%%%%%%%%% Parser %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% simple shift reduce parser
%%%% no empty categories, binary rules only
%%%% grammar file must define check_and_display_result/1.

:- module(shift_reduce,[]).

parse(o(Result,Sentence,_)) :-
	\+ \+ check_words(Sentence),
	parse(Sentence,[],Result0),
	conv(Result0,Result).
	
parse([],[t(Sign,Ds)],t(Sign,Ds)).
parse(Words,Stack,Tree) :-
	reduce(Stack,NewStack),
	parse(Words,NewStack,Tree).
parse(Words,Stack,Tree) :-
	shift(Words,Words0,Sign),
	parse(Words0,[Sign|Stack],Tree).

shift([Word|Words],Words,t(Sign,w(Word))) :-
	user:lex(Word,Sign).
shift([Word1,Word2|Words],Words,t(Sign,w([Word1,Word2]))) :-
	user:lex([Word1,Word2],Sign).
	
reduce( [t(RDghtr,RDtree),t(LDghtr,LDtree)|Stack],
	[t(M,[t(LDghtr,LDtree),t(RDghtr,RDtree)])|Stack]) :-
	user:rule0(_Name,M,[LDghtr, RDghtr],_Head).

%%%% morphological_prefix/1 called by check_wrd in parser

morphological_prefix(te).


check_words([]).
check_words([Word|Words]) :-
	( user:lex(Word,_) -> true
	; morphological_prefix(Word) -> true
	; nl, write('unkown: '), write(Word)
	), 
	check_words(Words).

conv(t(A,B0),tree(A,_,B)) :-
	conv_ds(B0,B).
conv_ds(w(Word),[tree(lex(Word),_,[])]).

conv_ds([],[]).
conv_ds([H0|T0],[H|T]) :-
	conv(H0,H),
	conv_ds(T0,T).


