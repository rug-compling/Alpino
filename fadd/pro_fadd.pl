:- module(pro_fadd, [ init_accent/3,
		      init_dict/3,
		      init_guess/6,
		      init_morph/6,
		      init_tuple/2,
		      accent_word/3,
		      prefix_word/3,
		      morph_word/3,
		      morph_word/4,
		      associate_word_integer/3,
		      guess_word/3,
		      number_word/3,
		      word_tuple_grams/3,
		      word_tuple_fpgrams/3,
%%%		      close_accent/2,
%%%		      close_dict/2,
%%%		      close_tuple/2,
		      fadd_get_errno/1
		    ]). 

:- expects_dialect(sicstus).

:- if(current_predicate(with_mutex/2)).

:- else.

:- meta_predicate with_mutex(?,:).
with_mutex(_,Call) :-
    once(Call).

:- endif.

foreign_resource(pro_fadd, [init_accent, init_dict, init_morph, init_guess,
			    pro_init_tuple,
			    pro_accent_word, pro_prefix_word, pro_morph_word,
			    pro_guess_word,
			    associate_word_integer,
			    number_word, pro_word_tuple_grams, 
                                         pro_word_tuple_fpgrams,
%%%			    close_accent,close_dict, close_tuple,
			    fadd_get_errno
			   ]).

foreign(init_accent, c, init_accent0(+string, +integer, [-integer])).

foreign(init_dict, c, init_dict0(+string, +integer, [-integer])).

foreign(init_morph, c,
	init_morph0(+string, +integer, +integer, +integer, +integer,
		   [-integer])).

foreign(init_guess, c,
	init_guess0(+string, +integer, +integer, +integer, +integer,
		   [-integer])).

foreign(pro_init_tuple, c, init_tuple0(+term, [-integer])).

foreign(pro_accent_word, c, accent_word0(+string, +integer, [-term])).

foreign(pro_prefix_word, c, prefix_word0(+string, +integer, [-term])).

foreign(pro_morph_word, c, morph_word0(+string, +integer, [-term])).

foreign(pro_guess_word, c, guess_word0(+string, +integer, [-term])).

foreign(number_word, c, number_word0(+string, +integer, [-integer])).

foreign(pro_word_tuple_grams, c, word_tuple_grams0(+term, +integer, [-term])).

foreign(pro_word_tuple_fpgrams, c, word_tuple_fpgrams0(+term, +integer, [-term])).

%%%foreign(close_accent, c, close_accent0(+integer, [-integer])).
%%%
%%%foreign(close_dict, c, close_dict0(+integer, [-integer])).
%%%
%%%foreign(close_tuple, c, close_tuple0(+integer, [-integer])).

foreign(fadd_get_errno, c, fadd_get_errno([-integer])).

foreign(associate_word_integer,c,
	associate_word_integer0(+chars,+integer,[-integer])).

%%%close_dict(C,O) :-
%%%    with_mutex(fadd,close_dict0(C,O)).
%%%
%%%close_accent(C,O) :-
%%%    with_mutex(fadd,close_accent0(C,O)).
%%%
%%%close_tuple(C,O) :-
%%%    with_mutex(fadd,close_tuple0(C,O)).

%% like memo, in order to ensure that a dictionary is
%% initialized only once - even in the presence of threads.
:- dynamic initialized/1.

init_accent(File0,Type,Dict) :-
    absolute_file_name(File0,File),
    with_mutex(fadd,init_accent_(File,Type,Dict)).

init_accent_(File,Type,Dict) :-
    (   initialized(init_accent(File,Type,Dict1))
    ->  Dict=Dict1
    ;   init_accent0(File,Type,Dict),
	check_error(Dict,init_accent(File,Type,Dict)),
	assertz(initialized(init_accent(File,Type,Dict)))
    ).

init_dict(File0,Type,Dict) :-
    absolute_file_name(File0,File),
    with_mutex(fadd,init_dict_(File,Type,Dict)).

init_dict_(File,Type,Dict) :-
    (   initialized(init_dict(File,Type,Dict1))
    ->  Dict=Dict1
    ;   init_dict0(File,Type,Dict),
	check_error(Dict,init_dict(File,Type,Dict)),
	assertz(initialized(init_dict(File,Type,Dict)))
    ).

init_morph(File0,T1,T2,T3,T4,Dict) :-
    absolute_file_name(File0,File),
    with_mutex(fadd,init_morph_(File,T1,T2,T3,T4,Dict)).

init_morph_(File,T1,T2,T3,T4,Dict) :-
    (   initialized(init_morph(File,T1,T2,T3,T4,Dict1))
    ->  Dict=Dict1
    ;   init_morph0(File,T1,T2,T3,T4,Dict),
	check_error(Dict,init_morph(File,T1,T2,T3,T4,Dict)),
	assertz(initialized(init_morph(File,T1,T2,T3,T4,Dict)))
    ).

init_guess(File0,T1,T2,T3,T4,Dict) :-
    absolute_file_name(File0,File),
    with_mutex(fadd,init_guess_(File,T1,T2,T3,T4,Dict)).

init_guess_(File,T1,T2,T3,T4,Dict) :-
    (   initialized(init_guess(File,T1,T2,T3,T4,Dict1))
    ->  Dict=Dict1
    ;   init_guess0(File,T1,T2,T3,T4,Dict),
	check_error(Dict,init_guess(File,T1,T2,T3,T4,Dict)),
	assertz(initialized(init_guess(File,T1,T2,T3,T4,Dict)))
    ).

init_tuple(FileList0,Dict) :-
    absolute_file_name_list(FileList0,FileList),
    with_mutex(fadd,init_tuple_(FileList,Dict)).

init_tuple_(FileList,Dict) :-
    (   initialized(init_tuple(FileList,Dict1))
    ->  Dict=Dict1
    ;   init_tuple0(FileList,Dict),
	check_error(Dict,init_tuple(FileList,Dict)),
	assertz(initialized(init_tuple(FileList,Dict)))
    ).    

absolute_file_name_list([],[]).
absolute_file_name_list([H0|T0],[H|T]) :-
    absolute_file_name(H0,H),
    absolute_file_name_list(T0,T).

check_error(Dict,Goal) :-
    (	Dict == -1
    ->	fadd_get_errno(Err),
	raise_exception(fadd_error("~w (error no: ~w)~n",[Goal,Err]))
    ;	true
    ).

accent_word(String,Dict,Result) :-
    with_mutex(fadd,accent_word0(String,Dict,Result)).

prefix_word(Atom,DictNo,Prefix) :-
    with_mutex(fadd,prefix_word0(Atom,DictNo,[Prefix])).

:- load_foreign_resource(pro_fadd).

:- if(current_prolog_flag(language,sicstus)).

:- multifile user:portray_message/2.
user:portray_message(error,fadd_error(Term,Args)):-
    !,
    format(user_error,"** error in fadd library: ",[]),
    format(user_error,Term,Args).

:- else.

:- multifile message_hook/3.
user:message_hook(fadd_error(Term,Args),error,_):-
    !,
    format(user_error,"** error in fadd library: ",[]),
    format(user_error,Term,Args).

:- endif.


:- use_module(library(lists),   [ member/2,
				  reverse/2]).
morph_word(Word,Dict,Stem,Tag) :-
    morph_word(Word,Dict,List),
    select_pair(List,Stem,Tag).

%% hack for now
morph_word(Word0,Dict,Result) :-
    sub_atom(Word0,_,1,_,'+'),
    !,
    atom_codes(Word0,WordCodes0),
    replace_plus(WordCodes0,WordCodes),
    atom_codes(Word,WordCodes),
    with_mutex(fadd,morph_word0(Word,Dict,Result0)),
    replace_inv_plus(Result0,Result).
morph_word(Word,Dict,Result) :-
    with_mutex(fadd,morph_word0(Word,Dict,Result)).

replace_inv_plus([],[]).
replace_inv_plus([Val0|Vals0],[Val|Vals]):-
    replace_inv_plus1(Val0,Val),
    replace_inv_plus(Vals0,Vals).

replace_inv_plus1(Atom0,Atom):-
    sub_atom(Atom0,_,5,_,'@@@@@'),
    !,
    atom_codes(Atom0,WordCodes0),
    replace_inv_plus2(WordCodes0,WordCodes),
    atom_codes(Atom,WordCodes).
replace_inv_plus1(Atom,Atom).

replace_plus([],[]).
replace_plus([No|Nos],Result) :-
    (   No == 43
    ->  Result = [64,64,64,64,64|T]
    ;   Result = [No|T]
    ),
    replace_plus(Nos,T).

replace_inv_plus2([],[]).
replace_inv_plus2([64,64,64,64,64|Nos],Result) :-
    !,
    Result = [43|T],
    replace_inv_plus2(Nos,T).
replace_inv_plus2([No|Nos],[No|T]) :-
    replace_inv_plus2(Nos,T).

select_pair([Left,Right|_],Left,Right).
select_pair([_,_|Tail],Stem,Tag) :-
    select_pair(Tail,Stem,Tag).

guess_word(A,B,C) :-
    with_mutex(fadd,guess_word0(A,B,C)).

number_word(A,B,C) :-
    with_mutex(fadd,number_word0(A,B,C)).

word_tuple_grams(A,B,C) :-
    with_mutex(fadd,word_tuple_grams0(A,B,C)).

word_tuple_fpgrams(A,B,C) :-
    with_mutex(fadd,word_tuple_fpgrams0(A,B,C)).

associate_word_integer(A,B,C) :-
    with_mutex(fadd,associate_word_integer0(A,B,C)).

