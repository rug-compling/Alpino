treat(X) :-
    (	X == end_of_file
    ->	true
    ;   catch(charsio:read_from_chars(X,Term),
	      syntax_error(_,_,_,_,_),
	      format(user_error,"syntax error in ~s~n",[X])
	     ),
	(   nonvar(Term)
	->  (   Term == end_of_file % e.g. comments!
	    ->  format("~s~n",[X])
	    ;	format("~q.~n",[Term])
	    )
	;   true
	),	    
	echo
    ).

:- use_module(library(charsio)).

echo :-
    read_line(X),
    treat(X).

:- echo.
