:- module(corpusreader, [ file_to_codes/2 ]).

:- expects_dialect(sicstus).

foreign_resource(corpusreader, [ file_to_codes_f,
				 get_data_f,
				% pathname_before_f,
				% pathname_after_f,
				 init(init_func),
				 deinit(deinit_func) ]).

:- public get_data/2, file_to_codes/2.

foreign(file_to_codes_f,c,file_to_codes_f(+string,[-term])).
foreign(get_data_f,c,get_data_f(+string,[-term])).

%foreign(pathname_before_f,c,pathname_before_f(+string,[-string])).
%foreign(pathname_after_f,c,pathname_after_f(+string,[-string])).

:- load_foreign_resource(corpusreader).

file_to_codes(File0,Codes) :-
    absolute_file_name(File0,File),
    file_to_codes_f(File,Codes).

get_data(File0,Term) :-
    absolute_file_name(File0,File),
    get_data_f(File,Term).

%pathname_before(FileIn0,File) :-
%    absolute_file_name(FileIn0,FileIn),
%    pathname_before_f(FileIn,File).
%
%pathname_after(FileIn0,File) :-
%    absolute_file_name(FileIn0,FileIn),
%    pathname_after_f(FileIn,File).


/*
test1(Out):-
        get_data('/storage/geertk/compact-corpora/cdb/1.xml', Codes),
        atom_codes(Out, Codes).

test2(Out):-
        get_data('deze file bestaat niet', Codes),
        atom_codes(Out, Codes).

test3(Out):-
        file_to_codes('/storage/geertk/Alpino/Treebank/cdb/1.xml', Codes),
        atom_codes(Out, Codes).

test4(Out):-                    % deze faalt bij file_to_codes...
        file_to_codes('/storage/geertk/compact-corpora/cdb/1.xml', Codes),
        atom_codes(Out, Codes).

test5(Out):-
        file_to_codes('deze file bestaat niet', Codes),
        atom_codes(Out, Codes).


test6(Out):-
        pathname_before('/storage/geertk/compact-corpora/cdb/1.xml', Out).

test7(Out):-
        pathname_after('/storage/geertk/compact-corpora/cdb/1.xml', Out).


% non-existing paths
test8(Out):-pathname_before('ditbestaatniet/1.xml', Out).
test9(Out):-pathname_after('ditbestaatniet/1.xml', Out).


% non-existing members
test10(Out):-
        pathname_before('/storage/geertk/compact-corpora/cdb/XXXX.xml', Out).

test11(Out):-
        pathname_after('/storage/geertk/compact-corpora/cdb/XXXX.xml', Out).


% .index e.d. in padnaam
test12(Out):-
        pathname_before('/storage/geertk/compact-corpora/cdb.index/1.xml', Out).


*/