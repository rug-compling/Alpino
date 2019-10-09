:- module(restrict, [ restrict/3 ]).

% RESTRICT(+Term,+Depth,-RestrTerm)
restrict(Term,Depth,Restr):-
	integer(Depth),
	restrict0(Depth,Term,Restr).

restrict0(0,_,_) :- !.
restrict0(Depth,Term,Restr) :-
    (  var(Term)
    -> Term=Restr
    ;  functor(Term,F,Arity),
       functor(Restr,F,Arity),
       Depth1 is Depth-1,
       restrict_args(Arity,Depth1,Term,Restr)
    ).

restrict_args(0,_,_,_):- !.
restrict_args(Ar,Depth,Term,Restr) :-
    arg(Ar,Term,ArgT),
    arg(Ar,Restr,ArgR),
    restrict0(Depth,ArgT,ArgR),
    Ar1 is Ar-1,
    restrict_args(Ar1,Depth,Term,Restr).
