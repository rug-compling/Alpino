:- dynamic np/5, v/6, pp/5, v/7.

top(main,main(_,_,[],[],[])).
semantics(main(Sem,_,_,_,_),Sem).
phonology(main(_,Phon,[],_,_),Phon).


main(LF) ==> front, s(LF).

front ... np(LF) ... v(A,B) ==> np(LF), v(A,B).
front ... pp(LF) ... v(A,B) ==> pp(LF), v(A,B).
front ... np(LF) ... v(A,B,C) ==> np(LF), v(A,B,C).
front ... pp(LF) ... v(A,B,C) ==> pp(LF), v(A,B,C).

np(john) ==> [john].
np(mary) ==> [mary].
np(LF) ==> det, n(N), pp_mod(N,LF).

pp_mod(N,N) ==> [].
pp_mod(N,PP/N) ==> pp(PP).

pp(LF) ==> p(Np,LF), np(Np).

s(LF) ==> np(Sub), vp(Sub,LF).

vp(Sub,LF) ==> np(Ob), v(Sub,Ob,LF).
vp(Sub,LF) ==> v(Sub,LF).

v(Jan,slaapt(Jan)) ==> [slaapt].
v(Jan,Marie,kust(Jan,Marie)) ==> [kust].

p(Np,met(Np)) ==> [met].

det ==> [de].
det ==> [het].
det ==> [een].
det ==> [].

n(boek) ==> [boek].
n(vrouw) ==> [vrouw].
n(man) ==> [man].


sentence(a,[john,slaapt]).
sentence(b,[john,kust,een,boek]).
sentence(c,[john,kust,een,boek,met,het,boek]).

:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(sem,Cat,Sem) :-
    semantics(Cat,Sem).

graphic_label(sem,Term,L) :-
    functor(Term,L,_).

graphic_daughter(sem,1,Term,Arg) :-
    arg(1,Term,Arg).
graphic_daughter(sem,2,Term,Arg) :-
    arg(2,Term,Arg).
graphic_daughter(sem,3,Term,Arg) :-
    arg(3,Term,Arg).

end_hook(parse,_,_,_) :-
    show_object_no(1,tree(sem),clig).

show_object_default2(No) :-
    show_object_no(No,tree(sem),clig).

