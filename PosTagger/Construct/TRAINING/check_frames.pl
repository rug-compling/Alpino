:- reconsult('allframes.pl').

frame_with_lemma(L) :-
    findall(Lemma,frame(_,verb(_,_,_),Lemma),Lemmas0),
    sort(Lemmas0,Lemmas),
    lists:member(L,Lemmas).

check_frames :-
    frame_with_lemma(Lemma),
    findall(Sc,find_unused_frame(Lemma,Sc),List0),
    sort(List0,List),
    (   List = []
    ->  true
    ;   member(S,List),
	format("unused frame ~w for ~w~n",[S,Lemma])
    ),
    fail.


find_unused_frame(Lemma,Sc) :-
    (   Lemma=NewWord
    ;   alpino_lex:inv_lex(Lemma,NewWord)
    ),
    alpino_lex:lexicon(verb(_,_,Sc),v_root(Lemma,_),[NewWord],[],normal,[]),
    \+ frame(_,verb(_,_,Sc),Lemma),
    prettyvars(Sc).

