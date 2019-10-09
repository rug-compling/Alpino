%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Derivation treebanks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(derivbank, [ derivbank_read_handle/1,
		       derivbank_write_handle/1,
		       derivbank_open_read/2,
		       derivbank_close_read/1,
		       derivbank_read_tree/4,
		       derivbank_entry/3,
		       derivbank_open_write/2,
		       derivbank_close_write/1,
		       derivbank_write_tree/4
		     ]).

:- use_module(['../zlib/zlib']).

derivbank_write_handle(Handle) :-
    hdrug_util:hdrug_flag(derivbank_write_handle,H),
    (   H == undefined
    ->  hdrug_util:hdrug_flag(derivbank,DerivBank0),
	(  DerivBank0 == undefined
	-> DerivBank = derivbank
	;  DerivBank = DerivBank0
	),
	derivbank_open_write(DerivBank,Handle),
	hdrug_util:set_flag(derivbank_write_handle,Handle)
    ;   Handle = H
    ).

derivbank_read_handle(Handle) :-
    hdrug_util:hdrug_flag(derivbank_read_handle,H),
    (   H == undefined
    ->  hdrug_util:hdrug_flag(derivbank,DerivBank0),
	(  DerivBank0 == undefined
	-> DerivBank = derivbank
	;  DerivBank = DerivBank0
	),
	derivbank_open_read(DerivBank,Handle),
	hdrug_util:set_flag(derivbank_read_handle,Handle)
    ;   Handle = H
    ).


%% Reader predicates
derivbank_open_read(Filename,Handle) :-
    open_indexed_corpus_reader(Filename,Handle).

derivbank_close_read(Handle) :-
    close_indexed_corpus_reader(Handle).

derivbank_read_tree(Handle,Key,N,Tree) :-
    charsio:format_to_chars("G#~w#~w",[Key,N],Chars),
    atom_codes(Name,Chars),
    indexed_corpus_read(Handle,Name,Data),
    bytes_to_n(4,Len,0,ZTree,Data),
    zlib_uncompress_term(ZTree,Len,Tree).

derivbank_entry(Handle,Key,N) :-
    indexed_corpus_entries(Handle,Entries),
    lists:member(Id,Entries),
    atom_codes(Id,IdChars),
    alpino_util:split_string(IdChars,"#",[_,KeyChars,NChars]),
    atom_codes(Key,KeyChars),
    atom_codes(N,NChars),
    hdrug_util:concat_all(['G',Key,N],Id,'#').

%% Writer predicates
derivbank_open_write(Filename,Handle) :-
    open_indexed_corpus_writer(Filename,Handle).

derivbank_close_write(Handle) :-
    close_indexed_corpus_writer(Handle).

derivbank_write_tree(Handle,Key,N,Tree) :-
    zlib_compress_term(Tree,ZTree0,Len),
    n_to_bytes(4,Len,ZTree,ZTree0),
    charsio:format_to_chars("G#~w#~w",[Key,N],Chars),
    atom_codes(Name,Chars),
    indexed_corpus_write(Handle,Name,ZTree).

n_to_bytes(0,_,List,List) :- !.
n_to_bytes(Count,Num,List,List0) :-
    Byte is Num /\ 16'ff,
    NewNum is Num >> 8,
    NewCount is Count - 1,
    n_to_bytes(NewCount,NewNum,List,[Byte|List0]).

bytes_to_n(0,Num,Num,List,List) :- !.
bytes_to_n(Count,Num,Num0,List,[Head|Tail]) :-
    NewNum is (Num0 << 8) \/ Head,
    NewCount is Count - 1,
    bytes_to_n(NewCount,Num,NewNum,List,Tail).

%% Prolog <-> C++ binding
foreign_resource(derivbank,[ openIndexedCorpusReader,
			     closeIndexedCorpusReader,
			     indexedCorpusEntries,
			     indexedCorpusRead,
			     openIndexedCorpusWriter,
			     closeIndexedCorpusWriter,
			     indexedCorpusWrite ]).

%% Reader functions
foreign(openIndexedCorpusReader,c,open_indexed_corpus_reader(+string,[-integer])).
foreign(closeIndexedCorpusReader,c,close_indexed_corpus_reader(+integer)).
foreign(indexedCorpusRead,c,indexed_corpus_read(+integer,+string,[-term])).
foreign(indexedCorpusEntries,c,indexed_corpus_entries(+integer,[-term])).

%% Writer functions
foreign(openIndexedCorpusWriter,c,open_indexed_corpus_writer(+string,[-integer])).
foreign(closeIndexedCorpusWriter,c,close_indexed_corpus_writer(+integer)).
foreign(indexedCorpusWrite,c,indexed_corpus_write(+integer,+string,+term)).

:- load_foreign_resource(derivbank).
