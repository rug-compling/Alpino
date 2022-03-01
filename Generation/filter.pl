:- expects_dialect(sicstus).

:- use_module(library(terms)).

bigram(M,Ds):-
    create_index(bigram(M,Ds),Index),
    bool_vector_member_bigram(Index).

local(M,Ds):-
    create_index(local(M,Ds),Index),
    bool_vector_member_local(Index).

create_index(Term,Index) :-
    term_hash(Term,Index).

foreign_resource(filter, [ bool_vector_member_bigram, bool_vector_member_local ]).

foreign(bool_vector_member_bigram,c,bool_vector_member_bigram(+integer,[-integer])).
foreign(bool_vector_member_local,c,bool_vector_member_local(+integer,[-integer])).

bool_vector_member_bigram(Int) :-
    bool_vector_member_bigram(Int,1).

bool_vector_member_local(Int) :-
    bool_vector_member_local(Int,1).

:- load_foreign_resource(filter).
