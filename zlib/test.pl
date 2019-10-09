:- expects_dialect(sicstus).
:- use_module([library(terms),zlib]).

:- multifile user:term_expansion/2.

:- op(1200,xfx,::-).

user:term_expansion((unit_test(Id) ::- Body),[(unit_test(Id) :- Body)]) :-
    assert(unit_test_id(Id)).

test :-
    findall(X,unit_test_id(X),Tests),
    run_tests(Tests).

run_tests([]).
run_tests([Test|Tail]) :-
    (  unit_test(Test)
    -> format("Test '~s': succes~n", [Test])
    ;  format("Test '~s': failed!~n", [Test])
    ),
    run_tests(Tail),
    halt.

unit_test(byte_array) ::-
    atom_codes(abbaabba,U),
    length(U,ULen),
    zlib_compress(U,C),
    zlib_uncompress(C,ULen,U).

unit_test(simple_term) ::-
    T = a(abcdef),
    zlib_compress_term(T,C,TLen),
    zlib_uncompress_term(C,TLen,T2),
    subsumes_chk(T,T2),
    subsumes_chk(T2,T).

unit_test(complex_term) ::-
    T = a(X),
    when(nonvar(X),write(X)),
    zlib_compress_term(T,C,TLen),
    zlib_uncompress_term(C,TLen,T2),
    variant(T, T2).
