treat(X) :-
    (	X == end_of_file
    ->	true
    ;	echo_and_reverse,
	format("~q.~n",[X])
    ).

echo_and_reverse :-
    read(X),
    treat(X).

:- echo_and_reverse.
