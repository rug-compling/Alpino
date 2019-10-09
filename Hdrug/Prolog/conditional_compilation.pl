/*

% taken from plunit.pl in the SWI-Prolog distribution

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006-2008, University of Amsterdam

    This file is covered by the `The Artistic License', also in use by
    Perl.  See http://www.perl.com/pub/a/language/misc/Artistic.html
*/

:- multifile
    user:term_expansion/2.

:- discontiguous
    user:term_expansion/2.

:- dynamic
    include_code/1.

:- op(1150,fx,thread_local).

including :-
    include_code(X), !,
    X == true.
including.

if_expansion((:- if(G)), []) :-
    (   including
    ->  (   catch(G, E, (print_message(error, E), fail))
	->  asserta(include_code(true))
	;   asserta(include_code(false))
	)
    ;   asserta(include_code(else_false))
    ).
if_expansion((:- else), []) :-
    (   retract(include_code(X))
    ->  (   X == true
	->  X2 = false
	;   X == false
	->  X2 = true
	;   X2 = X
	),
	asserta(include_code(X2))
    ;   throw_error(context_error(no_if),_)
    ).

if_expansion((:- endif), []) :-
    retract(include_code(_)), !.

if_expansion(_Rule, []) :-
    \+ including.

user:term_expansion((:- expects_dialect(sicstus)),[]).
user:term_expansion((:- thread_local(_)),[]).
user:term_expansion(In, Out) :-
    if_expansion(In, Out).

