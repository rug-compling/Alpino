:- module(alpino_tokenize, [ tokenize/2 ]).


foreign_resource(tokenize, [ tok ]).

foreign(tok, c, tok( +term, +integer, [-term])).

:- load_foreign_resource(tokenize).

tokenize(String,ListOfWords) :-
    length(String,Len),
    tok(String,Len,TokString),
    split_string(TokString," ",ListOfStrings),
    atom_codes_list(ListOfStrings,ListOfWords).

atom_codes_list([],[]).
atom_codes_list([String|Strings],[Word|Words]) :-
    atom_codes(Word,String),
    atom_codes_list(Strings,Words).


:- use_module(library(lists), [ append/3] ).

%% split_string(+Chars,+Sep,-List)
%% Chars is a list of character codes
%% Sep is a list of character codes of length 1
%% List is a list of lists of character codes.
%%
%% inspired by Perl's split function, but separator must be single
%% character (of course, it is easy to extend this, but I didn't).
%%
%% example:
%%| ?- split("john kisses mary"," ",L).
%% L = [[106,111,104,110],[107,105,115,115,101,115],[109,97,114,121]] ?

split_string(Chars,Sep,List) :-
    split(Sep,List0,Chars,[]),
    List=List0. % output after cut, generic for whole dcg

split(Sep,List) -->
    field(Sep,H),
    split1(H,Sep,List).

split1([],Sep,List) -->
    e_split1(Sep,List).
split1([H|T],Sep,[[H|T]|List]) -->
    split2(Sep,List).

e_split1(Sep,[[],Next|List]) -->
    sep(Sep),
    !,
    field(Sep,Next),
    split2(Sep,List).
e_split1(_Sep,[]) --> [].

split2(Sep,[Next|Result]) -->
    sep(Sep),
    !,
    field(Sep,Next),
    split2(Sep,Result).
split2(_Sep,[]) --> [].

sep([Char]) -->
    [Char].

%% read all chars until | or end of line
field(Sep,[H|T]) -->
    [H],
    {  [H] =\= Sep },
    !,
    field(Sep,T).
field(_,[]) --> [].

