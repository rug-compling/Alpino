:- prolog_flag(single_var_warnings,_,off).


:- use_module(load).

:- initialize_flag(parser(parser),on).
:- initialize_flag(parser,parser).

:- version('Chat-80 by Pereira and Warren').

semantics(r(_,_,_,Ans),Ans).

parser:parse(o(r(E,S,S1,Answer),String,_)) :-
    sentence(E,String,[],[],[]),
    logic(E,S),  !,
    qplan(S,S1), !,
    charsio:with_output_to_chars(user:answer(S1),Chars),!, 
    atom_codes(Answer,Chars).

:- use_module(suite).


:- multifile graphic_path/3,
             graphic_label/3,
	     graphic_daughter/4.

graphic_path(syn, r(E,_,_,_), E).
graphic_path(sem, r(_,E,_,_), E).

graphic_label(_,'$VAR'(X),L) :-
    !,
    L = '$VAR'(X).

graphic_label(_,Cat,Label) :-
    functor(Cat,Label,_).

graphic_daughter(_,_,'$VAR'(_),_) :-
    !,
    fail.
graphic_daughter(_,N,Term,D) :-
    functor(Term,_,Ar),
    hdrug_util:between(1,Ar,N),
    arg(N,Term,D).


gram_startup_hook_end :-
    tcl('
button .t.version -text {Chat-80} -command {
 tk_dialog .d "About the Grammar" "Chat-80 by Pereira and Warren" "" 0 ok
}
pack .t.version -side right

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "Chat-80 by Pereira and Warren" "" 0 ok
}

wm iconname . "Chat-80"

       ').

result_hook(parse,_,o(Obj,_,_),_) :-
    hdrug_flag(demo,OnOff),
    (	OnOff == on
    ->	show(tree(syn),clig,[value(Obj)]),
	semantics(Obj,Sem),
	tcl('tk_dialog .d "Answer" {~a} "" 0 ok',[Sem])
    ;   true
    ).

extern_phon([],[]).
extern_phon([H0|T0],[H|T]) :-
    extern_phon1(H0,H),
    extern_phon(T0,T).

extern_phon1(AtomExt,Atom) :-
    (	var(AtomExt)
    ->	AtomExt=Atom
    ;   perhaps_nb(AtomExt,Atom)
    ).

perhaps_nb(Number,Atom) :-
    (	on_exception(_,(atom_codes(Number,Chars),number_codes(Numb,Chars)),fail)
    ->	Atom=nb(Numb)
    ;   number(Number)
    ->	Atom=nb(Number)
    ;   Number=Atom
    ).

