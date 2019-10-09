to_sguides(Flag) :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  !
    ;	(   catch(chars2pl(Chars,Flag),P,print_message(error,P))
	->  true
	;   format(user_error,"error: ~s~n",[Chars]),
	    fail
	),
	fail
    ).

chars2pl(Chars,Flag) :-
    alpino_util:split_string(Chars,[124],Fields0),
    terms_list(Fields0,Fields),
    (   evaluate(Flag,Fields),
	fail
    ;   true
    ).

terms_list([],[]).
terms_list([H0|T0],[Term|T]) :-
    lists:append(H0,[32,46],Codes),
    charsio:read_from_chars(Codes,Term),
    terms_list(T0,T).

evaluate(connect,List) :-
    Term =.. [g|List],
    numbervars(Term,0,_),
    terms:term_hash(Term,Index),
    format("~q.~n",[check_connect(Index)]).

evaluate(predict,[Predict,F]) :-
    numbervars(Predict,0,_),
    terms:term_hash(g(F,Predict),Index),
    format("~q.~n",[check_predict(Index)]).

evaluate(prefix,[Goal|List0]) :-
    (   uptodate(List0,List)
    ->  true
    ;   format(user_error,"not uptodate: ~w~n",[List0]),
        List0=List
    ),
    lists:reverse(List,Term),
    numbervars(g(Goal,Term),0,_),
    terms:term_hash(g(Goal,Term),Index),
    format("~q.~n",[check_connect(Index)]).

evaluate(tpl_prefix,[Goal|List0]) :-
    (   uptodate(List0,List)
    ->  true
    ;   format(user_error,"not uptodate: ~w~n",[List0]),
        List0=List
    ),
    lists:reverse(List,Term),
    numbervars(g(Goal,Term),0,_),
%    terms:term_hash(g(Goal,Term),-1,67108863,Index),
    terms:term_hash(g(Goal,Term),Index),
    format("~q|~q~n",[Index,g(Goal,Term)]).

evaluate(uptodate,[Goal|List0]) :-
    (   uptodate(List0,List)
    ->  true
    ;   format(user_error,"not uptodate: ~w~n",[List0]),
        List0=List
    ),
    format("~q",Goal),
    format_l(List).

format_l([]) :-
    format("~n",[]).
format_l([H|T]) :-
    format("|~q",H),
    format_l(T).

uptodate([],[]).
uptodate([H0|T],[H|T]) :-
    uptodate_(H0,H).

uptodate_(lex(Full0),lex(Full)) :-
    alpino_guides:tr_tag(Full0,Full).
uptodate_(gap(X),gap(X)).
uptodate_(get_val,get_val).
uptodate_(put_val,put_val).
