:- use_module('/net/aps/64/lib/fsa/sic/fsa_library').

:- use_module('/net/aps/64/src/fsa6/Examples/Spell/uniform').

:- fsa_global_set(symbol_separator,32).

output_all([],_,Nr,Nr).
output_all([StringOut|T],Key,Nr0,Nr) :-
    fsa_interpreter:list_of_symbols_to_string(StringOut,Result,in),
    format("~s#~w|~s~n",[Key,Nr0,Result]),
    Nr1 is Nr0 + 1,
    output_all(T,Key,Nr1,Nr).

main :-
    prolog_flag(argv,Files),
    samples(Files).

samples([]).
samples([H|T]) :-
    sample(H),
    samples(T).

sample(File) :-
    (   atom_concat(Key,'.m',File)
    ->  true
    ;   File = Key
    ),
    fsa_read_file(File,Fa),
    fsa_uniform(Fa,FaU),
    findall(StringOut,
	    fsa_sample(FaU,1000,StringOut), 
	    List0
	   ),
    
    sort(List0,List),
    output_all(List,Key,1,_Nr).

fsa_sample(Fa,Max,Symbols) :-
    fsa_data:fsa_symbols_decl(Fa,r(Module,_)),
    fsa_data:index_transitions(Fa,Table),
    user:bb_put(fsa_interpreter:nrsol,0),
    repeat,
    fsa_interpreter:fsa_sample(Fa,Table,List,recognizer),
    fsa_interpreter:random_symbols_from_preds(List,Symbols,Module),
    fsa_interpreter:inc(nrsol),
    user:bb_get(fsa_interpreter:nrsol,Sols),
    (   Sols >= Max
    ->  !  % repeat
    ;   true
    ).

:- main, halt.
