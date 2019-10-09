marginal(dep35(Arg,ArgPos,Rel,HeadPos,Head),
         tdep35,
	 ldep35(Arg,ArgPos),
	 rdep35(Rel,HeadPos,Head)
	).

marginal(hdpp(Head,HeadPos,Rel,Prep,Noun,NounPos),
	 thdpp,
	 lhdpp(Noun,NounPos,hd/obj1,prep,Prep),
	 rhdpp(Rel,HeadPos,Head)
	).

marginal(appos_person(TYPE,Word),
	 tappos,
	 lappos(TYPE),
	 rappos(Word)
	).

score_corpus_feature(Feature,Val) :-
    corpus_frequency_lookup2(Feature,Pair),
    marginal(Feature,Total,Left,Right),
    corpus_frequency_lookup1(Left,L),
    corpus_frequency_lookup1(Total,T),
    corpus_frequency_lookup1(Right,R),
    association_score(L,R,Pair,T,Val),
    Val > 0.

%%% normalized PMI, as suggested by Gerlof Bouma
%%% Everything is multiplied by 10000 so that
%%% we can work with integers.
association_score(L,R,Pair,T,Val) :-
    PMI is log(T*Pair/(L * R)),
    NORM is -log(Pair/T),
    Val is integer(10000*PMI/NORM).

corpus_frequency_lookup1(TripleTerm,Score) :-
    initialize_corpus_frequency1(DictNo),
    feature_term_to_codes(TripleTerm,Triple),
    pro_fadd:associate_word_integer(Triple,DictNo,Score),
    Score > 0.

corpus_frequency_lookup2(TripleTerm,Score) :-
    initialize_corpus_frequency2(DictNo),
    feature_term_to_codes(TripleTerm,Triple),
    pro_fadd:associate_word_integer(Triple,DictNo,Score),
    Score > 0.

initialize_corpus_frequency1(No) :-
    hdrug_flag(initialized_corpus_frequency1,Init),
    initialize_corpus_frequency1(Init,No).

initialize_corpus_frequency1(undefined,No) :-
    !,
    File = 'temp1.fsa',
    pro_fadd:init_morph(File,0,0,0,0,No),
    debug_message(1,"initialized corpus_frequency1 ~w (~w)~n",[File,No]),
    set_flag(initialized_corpus_frequency1,initialized(No)).
initialize_corpus_frequency1(initialized(No),No).

initialize_corpus_frequency2(No) :-
    hdrug_flag(initialized_corpus_frequency2,Init),
    initialize_corpus_frequency2(Init,No).

initialize_corpus_frequency2(undefined,No) :-
    !,
    File = 'temp2.fsa',
    pro_fadd:init_morph(File,0,0,0,0,No),
    debug_message(1,"initialized corpus_frequency2 ~w (~w)~n",[File,No]),
    set_flag(initialized_corpus_frequency2,initialized(No)).
initialize_corpus_frequency2(initialized(No),No).

feature_term_to_codes(Feature,Codes) :-
    format_to_chars('~q',[Feature],Codes).

go:-
    repeat,
    catch(read(X),
	  syntax_error(_,_,_,_,_),
	  (  format(user_error,"ignoring a line~n",[]), fail )
	 ),
    (   X == end_of_file
    ->  true
    ;   score_corpus_feature(X,Val),
        format("~q\t~q\t~w~n",[X,X,Val]),
        fail
    ), !.


chars_to_term(Chars) :-
    split_string(Chars,"|",[_F1,F2,F3]),
    lists:append(F2," .",FeatureChars),
    charsio:read_from_chars(FeatureChars,Feature),
    marginal(Feature,M1,M2,M3),
%    format("~s~n",[Chars]),
    format("1|~q|~s~n",[M1,F3]),
    format("1|~q|~s~n",[M2,F3]),
    format("1|~q|~s~n",[M3,F3]).



marginals :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	(   catch(chars_to_term(Chars),
		  syntax_error(A,B,C,D,E),
		  (  format(user_error,"ignoring line ~s#~w~n~w~n",[Chars,Chars,syntax_error(A,B,C,D,E)]), fail )
		 )
	->  true
	;   %% format(user_error,"error: ~s~n",[Chars]),
	    fail
	),
	fail
    ), !.
