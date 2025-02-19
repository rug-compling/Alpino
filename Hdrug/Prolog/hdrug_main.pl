%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 							  %
% written by Gertjan van Noord				  %
% (C) 1988 - 2002	                                  %
% 							  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- (   current_prolog_flag(language, sicstus)
   ->  compile(conditional_compilation)
   ;   true
   ).

:- expects_dialect(sicstus).

:- use_module(library(system)).

:- multifile
    user:file_search_path/2.

:- public
    user:file_search_path/2.

user:file_search_path(alpino, Alpino) :-
    environ('ALPINO_HOME', Alpino).
user:file_search_path(hdrug, Hdrug) :-
    user:file_search_path(alpino,Alpino),
    atom_concat(Alpino,'/Hdrug/Prolog',Hdrug).

:- if(current_prolog_flag(dialect,swi)).
user:file_search_path(library, alpino(library)).
:- endif.

:- thread_local
    hdrug:object/2,
    hdrug:dyn_sentence/2,
    hdrug:dyn_sentence/3,
    hdrug:table_entry/6,
    hdrug:dyn_lf/2,
    hdrug_dyn_lf/3.

%:- if(current_prolog_flag(dialect,swi)).
:- use_module(clp_assert).
%:- endif.

:- multifile
    help_pred/3,
    help_info/4.

:- discontiguous
    help_pred/3,
    help_info/4.

:- use_module(hdrug_util).
:- use_module(help).
:- use_module(library(lists)).
:- use_module(library(timeout)).
:- use_module(library(system)).
:- use_module(library(charsio)).

:- prolog_load_context(module,MODULE),
   set_flag(gui_calling_module,MODULE).

%% ensure libraries are loaded (for applications), but don't import
%% any predicates. For stand-alone runtime systems it is crucial that we have
%% libraries in, since on target platform the libraries are not available,
%% perhaps.

:- use_module(library(terms),  []).
:- use_module(library(ugraphs),[]).
:- use_module(library(ordsets),[]).
:- use_module(library(assoc),  []).
:- use_module(library(arrays), []).
:- use_module(library(random), []).
:- use_module(hdrug_cmdint).
:- use_module(hdrug_gui).
:- use_module(hdrug_show).
:- use_module(hdrug_feature).
:- use_module(hdrug_txt).
:- use_module(hdrug_clig).
:- use_module(hdrug_dot).
:- use_module(hdrug_latex).
:- use_module(hdrug_tk).
:- use_module(hdrug_call_tree).
:- use_module(hdrug_chart).
:- use_module(hdrug_stats).

:- meta_predicate translate_result_hook_loop(:,?,?,?,?,?,?,?).
:- meta_predicate translate_timelimit(:,?,?).
:- meta_predicate translate_memlimit(:,?).
:- meta_predicate translate_all(?,:).

:- public hdrug_library/1, tex_library/1, blt_lib/2.
hdrug_library(Dir) :-
    hdrug_flag(hdrug_library,Dir).

tex_library(Dir) :-
    hdrug_flag(tex_library,Dir).

blt_lib(Blt,Dir) :-
    hdrug_flag(blt,Blt),
    hdrug_flag(blt_library,Dir).

:- multifile
    unknown_predicate_handler/3.

unknown_predicate_handler(Goal,Module,Goal2) :-
    (   current_predicate(unknown_predicate_handler,
			  Module:unknown_predicate_handler(_,_)),
        Module:unknown_predicate_handler(Goal,Goal2)
    ->  true
    ;   fail
    ).

%% set to off for saved states.
:- initialize_flag(debug,0).
:- initialize_flag(cmdint,off).

:- initialize_flag(object_saving,semi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simple PARSE and GENERATE  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public generate/1, generate_atom/1, generate_obj_no/1,
          generator_comparisons/0, generator_comparisons/1,
	  generator_comparisons_int/2, generator_comparison/1,
	  generate_compare/1, generate_compare_atom/1, generate_compare/2,
          parse/1, parser_comparisons/0, parser_comparisons/1,
	  parser_comparisons_int/2, parser_comparison/1,
	  parse_compare/1, parse_compare/2,
	  hdrug_main/0, runtime_entry/1,
	  try_reconsult_test_suite/1,
	  try_compile_test_suite/1,
	  available/0, sentences/0, lfs/0,
	  reset_table/0, reset_table/1.

% generate(+ExternSem)
generate(X):-
    try_hook(extern_sem(X,Sem),X=Sem),
    lf_length(Sem,Length),
    maximum_per_word(Length,Max),
    translate(generate,o(_Node,_Phon,Sem),_,X,Max,_).

generate_atom(Atom) :-
    atom_term(Atom,Lf),
    generate(Lf).

help_pred(generate,"generate(Sem)","
generates from the semantic representation Sem. Sem is first filtered
through the hook predicate extern_sem.").

% parse(+ListOfWords)
%parse(X):-
%    try_hook(extern_phon(X,Phon),X=Phon),
%    translate(parse,o(_Node,Phon,_Sem),_,X,_,_).

parse(X):-
    try_hook(extern_phon(X,Phon),X=Phon),
    check_length(Phon),
    sentence_length(Phon,Length),
    maximum_per_word(Length,Max),
    translate(parse,o(_Node,Phon,_Sem),_,X,Max,_).

help_pred(parse,"parse(Phon)","
parses from the phonological representation Phon; typically Phon is a
list of atoms, refer to the extern_phon hook predicate for more
complex possibilities.").

% generate_obj(+Oterm)
generate_obj_no(No) :-
    object(No,Object),
    generate_obj(Object).

help_pred(generate_obj_no,"generate_obj_no(Integer)","
generated from the semantic representation of object Integer. Only the
semantic representation of that object is passed to the generator.").

generate_obj(o(_,_,Sem)):-	% only take Sem into account
    try_hook(extern_sem(X,Sem),X=Sem),
    translate(generate,o(_Node,_Phon,Sem),_,X,0,_).

% wr_module(PG,Module)
% note: parse =/= parser
%       generate =/= generator
wr_module(parse,Module) :-
    hdrug_flag(parser,Module).
wr_module(generate,Module) :-
    hdrug_flag(generator,Module).

:- initialize_flag(parse_or_generate,parse).

% translate(+ParseGenerate,?OTerm,?TotalTime,Extern,?Max,?Status)
translate(ParGen,Obj,Total,X,Max,Status):-
    set_flag(parse_or_generate,ParGen),
    wr_module(ParGen,Mod),
    (	Mod == undefined
    ->	raise_exception(hdrug_error('flag ~w not defined!~n',[ParGen]))
    ;	true
    ),
    start_hook(ParGen,Mod,Obj,Start,CNo,UserOpt,X),
    debug_message(2,"          ******** ~w: ~w~n",[ParGen,Mod]),
    translate_timelimit(try_hook(start_hook0(ParGen,Mod,Obj,UserOpt)),
			Max,
			Status0
		       ),
    (   Status0 == success
    ->  functor(Call,ParGen,1),
	arg(1,Call,Obj),
	catch(translate_result_hook_loop(Mod:Call,Max,Status,
					 ParGen,Obj,Start,CNo,UserOpt),
	      enough,
	      true
	     )
    ;   Status = start_hook(Status0),
	set_flag(hdrug_status,Status)
    ),
    end_hook(ParGen,Mod,Obj,Start,Total,UserOpt),
    debug_message(2,"          ******** done: ~w ~n",[Mod]).

translate_result_hook_loop(Mod:Call,Max,Status,ParGen,Obj,Start,CNo,UserOpt):-
    set_flag(hdrug_status,failure),
    (	translate_timelimit(Mod:Call,Max,Status),
	(   Status == success
	->  result_hook(ParGen,Mod,Obj,Start,CNo,UserOpt)
	;   true
	),
	set_flag(hdrug_status,Status),
	fail
    ;	hdrug_flag(hdrug_status,Status)
    ).

%%% NEW: time-out "doens't work" once it has found a single solution.
%%% therefore we check here ourselves...
%%% this is more complicated than I thought, since it that case
%%% Call is already instantiated. Therefore copy_term
translate_timelimit(Call,Max,Status) :-
    copy_term(Call,Call2),
    current_output(OutputStream),
    (	Max =:= 0
    ->	translate_memlimit(Call,Status)
    ;   statistics(runtime,[Start,_]),
        time_out(
          translate_memlimit(Call2,StatusMem),
          Max,
          StatusTimeOut0),
        statistics(runtime,[End,_]),
        Used is End-Start,
        (   Used > Max
        ->  StatusTimeOut = time_out,
            ! %% don't backtrack after time-out!
        ;   StatusTimeOut = StatusTimeOut0
        )
    ),
    (	StatusTimeOut == time_out
    ->	format(user_error,"timed out after ~w msec~n",[Max]),
	(    hook(after_timeout_options(Call))
	->   call_cleanup(translate_timelimit(Call,Max,Status),try_hook(undo_timeout_options(Call)))
	;    Status = time_out
	)
    ;   Status = StatusMem,
	Call=Call2
    ),
    current_output(OutputStreamChanged),  % bug in charsio/timeout
    (   OutputStream == OutputStreamChanged
    ->  true
    ;   set_output(OutputStream),
	close(OutputStreamChanged)
    ).

:- if(current_prolog_flag(language,sicstus)).

translate_memlimit(Call,Status) :-
    catch(translate_all(Call),
	  resource_error(_,memory),
	  Status = out_of_memory
	 ),
    (   Status == out_of_memory
    ->  format(user_error,"out of memory~n",[])
    ;	Status = success
    ).

:- else.

translate_memlimit(Call,Status) :-
    catch(translate_all(Call),
	  error(resource_error(stack),_),
	  Status = out_of_memory
	 ),
    (   Status == out_of_memory
    ->  format(user_error,"out of memory~n",[])
    ;	Status = success
    ).

:- endif.

translate_all(Call):-
    call(Call).

% called before parser/generator starts
start_hook(ParGen,Mod,Obj,Start,CNo,UserOpt,X) :-
    resetsym,  % re-set gensym to 0, because for a large
               % corpus, we got after 410083 sentences a gensym that
               % was too large to be used for bb_ predicates (2**25!)
    init_object(ParGen,Obj),
    instantiate_top(Obj),
    hdrug_flag(object_saving,Semi),
    (	Semi==semi
    ->	hdrug_flag(current_no,No,1),
	CNo = 1,
	retract_objects(No)
    ;	find_current_no(CNo)
    ),
    debug_call(1,(
                   format(user_error,"~w: ",[ParGen]),
                   short_before(ParGen,X)
		 )),
    try_hook(Mod:clean),
    maybe_trimcore,
    hdrug_flag(found_solutions,_,0),
    try_hook(start_hook(ParGen,Mod,Obj,UserOpt)),
    statistics(runtime,[Start,_]).

% be careful, the time needed for result_hook will be part
% of the parse times. Therefore, for reliable statistics run
% with hdrug_flag(object_saving,off), flag(demo,off).
% called for each parser/generator result
result_hook(ParGen,Mod,Obj,Start,CNo,UserOpt) :-
    hdrug_flag(found_solutions,NoSol0),
    NoSol is NoSol0+1,
    hdrug_flag(found_solutions,_,NoSol),
    fin_object(ParGen,Obj),
    (   terms:cyclic_term(Obj)
    ->  format(user_error,"warning: cyclic object solution #~w~n",[NoSol])
    ;   true
    ),
    hdrug_flag(demo,Demo),
    (	Demo==on
    ->	statistics(runtime,[Now,_]),
	Total is (Now - Start),
	debug_message(2,"cputime passed ~w msec~n",[Total]),
	short_after(ParGen,Obj)
    ;	true
    ),
    create_new_object(_No,Obj,CNo),
    try_hook(result_hook(ParGen,Mod,Obj,UserOpt)).

% called after parser/generator fails (i.e., after all solutions are found)
end_hook(ParGen,Mod,o(Node,Sent,Sem),Start,Total,UserOpt) :-
    try_hook(end_hook0(ParGen,Mod,o(Node,Sent,Sem),UserOpt)),
    statistics(runtime,[End,_]),
    Total is (End - Start),
    sentence_length(Sent,Len),
    debug_message(1,"cputime total ~w msec ~w words~n",[Total,Len]),
    hdrug_flag(found_solutions,F),
    debug_message(1,"Found ~w solution(s)~n",[F]),
    try_hook(Mod:count),
    try_hook(end_hook(ParGen,Mod,o(Node,Sent,Sem),UserOpt)).

init_object(parse,o(Node,Phon,_Sem)):-
    try_hook(phonology(Node,Phon)).

init_object(generate,o(Node,_Phon,Sem)):-
    try_hook(semantics(Node,Sem)).

fin_object(generate,o(Node,Phon,_Sem)):-
    try_hook(phonology(Node,Phon)).

fin_object(parse,o(Node,_Phon,Sem)):-
    try_hook(semantics(Node,Sem)).

help_pred(available,"available","
Lists all available parsers and generators, and their associated
activity status. During parser comparison and generator comparison,
only those parsers and generators are compared which are currenly
active. ").

available :-
    format(user_error,"Active parsers:~n",[]),
    (	hdrug_flag(parser(P)), hdrug_flag(parser(P),on),
	tab(user_error,5),write(user_error,P),nl(user_error),
	fail
    ;	true
    ),
    format(user_error,"Active generators:~n",[]),
    (	hdrug_flag(generator(P)), hdrug_flag(generator(P),on),
	tab(user_error,5),write(user_error,P), nl(user_error),
	fail
    ;	true
    ),
    format(user_error,"Inactive parsers:~n",[]),
    (	hdrug_flag(parser(P)), hdrug_flag(parser(P),off),
	tab(user_error,5),write(user_error,P), nl(user_error),
	fail
    ;	true
    ),
    format(user_error,"Inactive generators:~n",[]),
    (	hdrug_flag(generator(P),off),
	tab(user_error,5),write(user_error,P),nl(user_error),
	fail
    ;	true
    ).

% SHORT/2
% short(ParseGenerate,Obj)

short_before(parse,ExtPhon):-
    try_hook(display_extern_phon(ExtPhon),format(user_error,"~p~n",[ExtPhon])),
    format(user_error,"~N",[]).

short_before(generate,ExtSem):-
    try_hook(display_extern_sem(ExtSem),format(user_error,"~p~n",[ExtSem])),
    format(user_error,"~N",[]).

short_after(generate,o(_,Phon,_)):-
    try_hook(extern_phon(ExtPhon,Phon),ExtPhon=Phon),
    try_hook(display_extern_phon(ExtPhon),format(user_error,"~p~n",[ExtPhon])),
    format(user_error,"~N",[]).

short_after(parse,o(_,_,Sem)):-
    try_hook(extern_sem(ExtSem,Sem),ExtSem=Sem),
    try_hook(display_extern_sem(ExtSem),format(user_error,"~p~n",[ExtSem])),
    format(user_error,"~N",[]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%                    %%
%% CREATE NEW OBJECT  %%
%%                    %%
%%%%%%%%%%%%%%%%%%%%%%%%

:- initialize_flag(object_exists_check,on).

create_new_object(No,Val,MinNo):-
    hdrug_flag(current_ref,Ref),
    set_flag(current_ref_of_object,Ref),
    hdrug_flag(object_saving,OS),
    create_new_object(OS,No,Val,MinNo).

create_new_object(off,_,_,_).
create_new_object(on,No,Obj_val,MinNo) :-
    hdrug_flag(object_exists_check,Exists),
    (	Exists==on
    ->	object_does_not_exist(Obj_val,MinNo)
    ;	true
    ),
    new_no(No),
    assert_object(No,Obj_val).
create_new_object(semi,No,Obj_val,MinNo) :-
    create_new_object(on,No,Obj_val,MinNo).

:- dynamic hdrug:object/2.
object_does_not_exist(Obj,MinNo):-
    find_current_no(Cur),
    between(MinNo,Cur,No),
    copy_term(Obj,Copy),
    hdrug:clause(object(No,Copy),_,Ref),
    hdrug:clause(object(No,Variant),_,Ref),
    terms:variant(Obj,Variant),
    !,
    debug_message(1,"(Found object identical to: ~w)~n",[No]).
    % removed 'fail' here, otherwise other (app-spec) counters might get
    % confused

object_does_not_exist(_,_).

new_no(No) :-
    find_current_no(No),
    No2 is No + 1,
    hdrug_flag(current_no,_,No2).

find_current_no(No):-
    hdrug_flag(current_no,N),
    (   integer(N)
    ->  No = N
    ;   No is 1
    ).

retract_objects(No) :-
    tk_retract_objects(No),
    hdrug:retractall(object(_,_)).

:- initialize_flag(max_number_of_objects,500).

assert_object(No,Val):-
    hdrug_flag(max_number_of_objects,N),
    (   integer(N), N > 0, No > N
    ->  (   No is N+1
	->  debug_message(1,"more objects than flag(max_number_of_objects)!~n",[])
	;   adapted_create_new_object_message(No)
	)
    ;   tk_assert_object(No),
	assertz(hdrug:object(No,Val))
    ).

adapted_create_new_object_message(No) :-
    (   (  No < 10
        ;  No < 100,
           0 is mod(No,10)
        ;  No < 1000,
           0 is mod(No,100)
        ;  No < 10000,
           0 is mod(No,1000)
        ;  0 is mod(No,10000)
        )
    ->  hdrug_util:debug_message(1,"alpino: found analysis: ~w~n",[No])
    ;   true
    ).

help_pred(object,"object(No,Object)","
Results of parsing and generation are normally added to the
database. This predicate can be used to fetch such an object. The
first argument is an integer used as the key of the object, the second
argument is a triple o(Cat,Phon,Sem).").

object(No,Val):-
    hdrug:object(No,Val).

%correct_id(I):-
%    find_current_no(I2),
%    I1 is I2 - 1,
%    between(1,I1,I).

:- initialize_flag(current_no,1).

%%%%%%%%%%%%%%%%%%%%
%%% TOP CATEGORY %%%
%%%%%%%%%%%%%%%%%%%%

instantiate_top(o(Cat,_,_)):-
    hdrug_flag(top_features,Name),
    (	Name == undefined
    ->	true
    ;	top(Name,Cat)
    ),
    debug_call(1,wr_flag(top_features)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parsing sets of sentences with sets of parsers           %%
%% generating sets of logical forms with sets of generators %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_pred(reset_table,"reset_table / reset_table(ParGen)","
Without an argument, removes all results of parser comparison and
generator comparison runs. With an argument, only remove information
concerning that particular parser or generator. ").

reset_table :-
    retractall(hdrug:table_entry(_,_,_,_,_,_)).

reset_table(Parser) :-
    retractall(hdrug:table_entry(_,_,_,_,Parser,_)).

help_pred(parser_comparisons,"parser_comparisons /
parser_comparisons(Keys)",
"Without arguments, compares active parsers on all sentences in test
suite. With an argument, Keys is a list of keys which relate to the
first argument of the sentence hook predicate. The active parsers will
be compared on sentences with a matching key.").

/*
%% list of items, print table afterwards
parser_comparisons([]) :-
    print_table_total.
parser_comparisons([H|T]):-
    parser_comparison(H),
    parser_comparisons(T).
*/

%% failure-driven doesn't seem to fill up memory ..???
parser_comparisons(List0) :-
    select_relevant_keys(List0,List),
    (	lists:member(Key,List),
        parser_comparison(Key),
	fail
    ;	print_table_total
    ).

select_relevant_keys(List0,List) :-
    hdrug_flag(first_key_no,First),
    hdrug_flag(last_key_no,Last),
    select_first(List0,First,List1),
    select_last(List1,Last,List).

select_first(List0,First,List) :-
    (   First == undefined
    ->  List0 = List
    ;   lists:append(_,[First|Tail],List0)
    ->  List = [First|Tail]
    ;   List0 = List
    ).

select_last(List0,Last,List) :-
    (   Last == undefined
    ->  List0 = List
    ;   lists:append(Prefix,[Last|_],List0)
    ->  lists:append(Prefix,[Last],List)
    ;   List0 = List
    ).

%% all items
parser_comparisons :-
    findall(Key,a_sentence(Key,_,_),Keys),
    parser_comparisons(Keys).

%% interval; do each key in range Min-Max
parser_comparisons_int(Min,Max) :-
%    findall(Key,( a_sentence(Key,_,_),
%                  between(Min,Max,Key)
%                ),Keys),
%% interval: parse from Min-th to Max-th sentence
    findall(Key,a_sentence(Key,_,_),Keys),
    select_keys(Keys,Min,Max,0,RelKeys),
    parser_comparisons(RelKeys).

select_keys([],_,_,_,[]).
select_keys([K|T0],Min,Max,C,RelKeys0) :-
    (  Min =< C,
         C =< Max
    -> RelKeys0 = [K|RelKeys]
    ;  RelKeys0 = RelKeys
    ),
    C1 is C+1,
    select_keys(T0,Min,Max,C1,RelKeys).

help_pred(generator_comparisons,
"generator_comparisons / generator_comparisons(Keys)","
Without arguments, compares active generators for all logical forms of
test suite. With an argument, Keys is a list of keys which relate to
the first argument of the lf hook predicate. Only the logical forms
with a matching key are compared.").

%% for generators too
generator_comparisons :-
    findall(Key,a_lf(Key,_,_),Keys),
    generator_comparisons(Keys).

%% list of items, print table afterwards
generator_comparisons([]) :-
    print_table_total.
generator_comparisons([H|T]):-
    generator_comparison(H),
    generator_comparisons(T).

%% interval; do each key in range Min-Max
generator_comparisons_int(Min,Max) :-
    findall(Key,between(Min,Max,Key),Keys),
    generator_comparisons(Keys).



% if Ref uninstantiated, then use all sentences upon backtracking
parser_comparison(Ref0) :-
    a_sentence(Ref0,Ref,Max,Sentence),

    hook(allow_sentence_key_for_parser(Ref),true),

    debug_message(1,"~n% ######## Running parser(s) on ~w~n",[Ref]),
    parse_compare_st(Ref,Max,Sentence),
    hdrug_flag(print_table_total,OnOff),
    (	OnOff==on
    ->	print_table_total
    ;	true
    ),
    debug_message(1,"% ####### Done parser(s) on ~w~n",[Ref]).

%% sentences are given as
%% sentence(Key,MaxMilliSec,Sentence) or
%% sentence(Key,Sentence)
%% in the latter case the maximum is computed by number of words
%% times maximum_per_word
%% dyn_sentences are sentences asserted (previous parse goals or
%% previous generation results)

%% new: max_sentence_length is a flag, whose value is either 0 (every
%% sentence is accepted) or an integer I, indicating that each sentence
%% with more words than I should be ignored

a_sentence(Ref,Max,Sentence) :-
    a_sentence(Ref,_Ref,Max,Sentence).

a_sentence(Ref,Ref,Max,Sentence):-
    hook(sentence(Ref,Max,Sentence)),
    check_length(Sentence).
a_sentence(Ref,Ref,Max,Sentence) :-
    dyn_sentence(Ref,Max,Sentence),
    check_length(Sentence).
a_sentence(Ref0,Ref,Max,Sentence):-
    a_sentence_with_map(Ref0,Ref,Sentence),
    sentence_length(Sentence,Length),
    maximum_per_word(Length,Max),
    check_length(Sentence).

a__sentence(Ref,Sentence):-
    hook(sentence(Ref,Sentence)),
    check_length(Sentence).
a__sentence(Ref,Sentence) :-
    dyn_sentence(Ref,Sentence),
    check_length(Sentence).

a_sentence(I,Sent) :-
    a_sentence_with_map(I,_,Sent).

a_sentence_with_map(I0,I,Sent) :-
    if(
       if(a__sentence(I0,Sent),
	  I=I0,
	  map_key_sentence(I0,I,Sent)
	 ),
       true,
       (  integer(I0),
	  findall(K-S,a__sentence(K,S),List),
	  lists:nth(I0,List,I-Sent)
       )
    ).

% map_key_sentence(+I,?Iatom,?Sent)
map_key_sentence(I,Iatom,Sent) :-
    (   a__sentence(I,Sent)
    ->  I = Iatom
    ;   integer(I)
    ->  number_codes_silent(I,Icodes),
	atom_codes(Iatom,Icodes),
	a__sentence(Iatom,Sent)
    ;   atom(I)
    ->  atom_codes(I,Icodes),
	number_codes_silent(Iatom,Icodes),
	a__sentence(Iatom,Sent)
    ).

number_codes_silent(Number,Codes) :-
    catch(number_codes(Number,Codes),_,fail).

%% nb:
%% number_codes(Var,"0002").
%% Var = 2 ? ;
%% no
%% | ?- number_codes(Var,"0002").
%% Var = 2 ?
%% yes
%%
%%

check_length(Sentence) :-
    hdrug_flag(max_sentence_length,MaxVal),
    (	integer(MaxVal),
	MaxVal > 0
    ->	check_max_length(Sentence,MaxVal)
    ;	true
    ),
    hdrug_flag(min_sentence_length,MinVal),
    (	integer(MinVal),
	MinVal > 0
    ->	check_min_length(Sentence,MinVal)
    ;	true
    ).

check_max_length(Sentence,Val) :-
    sentence_length(Sentence,Length),
    Length =< Val.

check_min_length(Sentence,Val) :-
    sentence_length(Sentence,Length),
    Length >= Val.

a_lf(Ref,Max,Lf):-
    hook(lf(Ref,Max,Lf)).
a_lf(Ref,Max,Lf) :-
    dyn_lf(Ref,Max,Lf).
a_lf(Ref,Max,Lf):-
    a_lf(Ref,Lf),
    lf_length(Lf,Length),
    maximum_per_word(Length,Max).

a_lf(Ref,Lf):-
    hook(lf(Ref,Lf)).
a_lf(Ref,Lf) :-
    dyn_lf(Ref,Lf).

help_pred(sentences,"sentences","lists all sentences in test-suite").
sentences :-
    (	a_sentence(_,Key,B,C),
	write(sentence(Key,B,C)),nl,
	fail
    ;	true
    ).

help_pred(lfs,"lfs","lists all logical forms in test-suite").
lfs :-
    (	a_lf(A,B,C),
	write(lf(A,B,C)),nl,
	fail
    ;	true
    ).

% this e.g. gives:
%  3 words   12700
%  8         29200
% 16         86800
% 25        197500
maximum_per_word(L,Max) :-
    try_hook(user_max(L,Max),
	     (	 hdrug_flag(user_max,Max),
		 integer(Max)
	     ->	 true
	     ;	 Max is 10000 + (L * L * 300)
	     )
	    ).

% fails if a parse with same length or longer for this parser timed out
:- initialize_flag(useful_try_check,on).

useful_try(Parser,Ref,_):-
    table_entry(Ref,_,_,_,Parser,_),
    !,
    hdrug_flag(useful_try_check,OnOff),
    (   OnOff == off
    ->  retract(hdrug:table_entry(Ref,_,_,_,Parser,_))
    ;   format(user_error,"already done ~w for ~w~n",[Ref,Parser]),
	fail
    ).
useful_try(Parser,Ref,Length):-
    hdrug_flag(useful_try_check,OnOff),
    useful_try(OnOff,Parser,Ref,Length).

useful_try(on,Parser,Ref,Length) :-
    table_entry(Name,Length2,time_out,_,Parser,_),
    Length >= Length2,
    !,
    format(user_error,"not doing ~w for ~w because of time-out of ~w~n",
	   [Ref,Parser,Name]),
    fail.
useful_try(on,Parser,Ref,Length) :-
    table_entry(Name,Length2,out_of_memory,_,Parser,_),
    Length >= Length2,
    !,
    format(user_error,"not doing ~w for ~w because of out-of-memory of ~w~n",
	   [Ref,Parser,Name]),
    fail.
useful_try(_,_,_,_).

:- initialize_flag(sent_nr,1).

:- dynamic hdrug:dyn_sentence/2, hdrug:dyn_sentence/3.

help_pred(parse_compare,"parse_compare(Sentence)/parse_compare(Max,Sentence)","
Compares active parsers on Sentence. In the binary format, Max is an
integer indicating the maximum amount of msec.").
parse_compare(Sentence):-
    hdrug_flag(sent_nr,Sym),
    Sym2 is Sym+1,
    hdrug_flag(sent_nr,_,Sym2),
    assertz(hdrug:dyn_sentence('$'(Sym),Sentence)),
    parser_comparison('$'(Sym)).

parse_compare(Max,Sentence):-
    hdrug_flag(sent_nr,Sym),
    Sym2 is Sym+1,
    hdrug_flag(sent_nr,_,Sym2),
    assertz(hdrug:dyn_sentence('$'(Sym),Max,Sentence)),
    parser_comparison('$'(Sym)).

:- initialize_flag(compare_object_saving,off).

:- dynamic hdrug:table_entry/6.
parse_compare_st(Ref,Max,Sentence):-
    hdrug_flag(demo,OldDemo,off),
    hdrug_flag(compare_object_saving,NewOs),
    hdrug_flag(object_saving,OldOs,NewOs),
    call_cleanup(parse_compare_st0(Ref,Max,Sentence),
		 (   set_flag(demo,OldDemo),
		     set_flag(object_saving,OldOs)
		 )).

%% todo: check that at least one parser is on, use current parser
%% otherwise; idem for generate_comp..
parse_compare_st0(Ref,Max,Sentence):-
    try_hook(extern_phon(Sentence,Phon),Sentence=Phon),
    sentence_length(Phon,Len),
    (	hdrug_flag(parser(Parser),on),
	hdrug_flag(parser,OldParser,Parser),
	call_cleanup(parse_compare_st1(Ref,Max,Sentence,Phon,Len,Parser),
		     set_flag(parser,OldParser)
		    ),
	fail
    ;	true
    ).

parse_compare_st1(Ref,Max,Sentence,Phon,Len,Parser):-
    useful_try(Parser,Ref,Len),
    set_flag(current_ref,Ref),
    catch(translate(parse,o(_Node,Phon,_Sem),Total,Sentence,Max,Succ),
	  hdrug_error(Msg,Args),
	  (   Total=0,
	      Succ=error,
	      print_message(error,hdrug_error(Msg,Args))
	  )
	 ),
    try_hook(Parser:count(NoEdges),NoEdges=0),
    try_hook(Parser:clean),
    (	Succ==time_out
    ->  NoAnalyses=time_out
    ;   Succ==out_of_memory
    ->  NoAnalyses=out_of_memory
    ;   Succ==error
    ->  NoAnalyses=error
    ;	hdrug_flag(found_solutions,NoAnalyses)
    ),
    maybe_trimcore,  % make sure the space emptied by Parser:clean is available
               % otherwise we cannot assert the next item...
    format(user_error,"T#~w|~w|~w|~w|~w|~w~n",[Ref,Len,NoAnalyses,Total,Parser,NoEdges]),
    assertz(hdrug:table_entry(Ref,Len,NoAnalyses,Total,Parser,NoEdges)).

%% sentence_length(Sentence,Len)
sentence_length(Ws, Len) :-
    try_hook(exceptional_sentence_length(Ws,Len),
	     (   nonvar(Ws)
	     ->  length(Ws,Len)
	     ;   Len=unknown
	     )
	    ).

lf_length(Lf, Len) :-
    try_hook(exceptional_lf_length(Lf,Len),
	     (	 format_to_chars('~w',[Lf],Chars),
		 length(Chars,Len)
	     )
	    ).

%%%%%%%%%%%%%%
% generation %
%%%%%%%%%%%%%%

:- initialize_flag(lf_nr,1).
:- dynamic hdrug:dyn_lf/2, hdrug:dyn_lf/3.

help_pred(generate_compare,
"generate_compare(Lf)/generate_compare(Max,Lf)",
"Compares active generators on Lf. In the binary format, Max is an
integer indicating the maximum amount of msec.").
generate_compare(Lf):-
    hdrug_flag(lf_nr,Sym),
    Sym2 is Sym+1,
    hdrug_flag(lf_nr,_,Sym2),
    assertz(hdrug:dyn_lf('$'(Sym),Lf)),
    generator_comparison('$'(Sym)).

generate_compare_atom(Atom):-
    atom_term(Atom,Term),
    generate_compare(Term).

generate_compare(Max,Lf):-
    hdrug_flag(lf_nr,Sym),
    Sym2 is Sym+1,
    hdrug_flag(lf_nr,_,Sym2),
    assertz(hdrug:dyn_lf('$'(Sym),Max,Lf)),
    generator_comparison('$'(Sym)).


% if Ref uninstantiated, then use all sentences upon backtracking
generator_comparison(Ref) :-
    a_lf(Ref,Max,Lf),
    debug_message(1,"~n% ######## Comparing generators on ~w~n",[Ref]),
    generate_compare_st(Ref,Max,Lf),
    hdrug_flag(print_table_total,OnOff),
    (	OnOff==on
    ->	print_table_total
    ;	true
    ),
    debug_message(1,"% ####### Compared generators on ~w~n",[Ref]).

generate_compare_st(Ref,Max,Lf):-
    hdrug_flag(compare_object_saving,NewOs),
    hdrug_flag(object_saving,OldOs,NewOs),
    call_cleanup(generate_compare_st0(Ref,Max,Lf),
		 (   set_flag(object_saving,OldOs)
		 )).

generate_compare_st0(Ref,Max,Lf0):-
    try_hook(extern_sem(Lf0,Lf),Lf0=Lf),
    lf_length(Lf,Len),
    (	hdrug_flag(generator(Generator),on),
	hdrug_flag(generator,OldGenerator,Generator),
	call_cleanup(generate_compare_st1(Ref,Max,Lf,Len,Generator),
		     set_flag(generator,OldGenerator)
		    ),
	fail
    ;	true
    ).

generate_compare_st1(Ref,Max,Lf,Len,Generator):-
    useful_try(Generator,Ref,Len),
    try_hook(extern_sem(X,Lf),X=Lf),
    set_flag(current_ref,Ref),
    catch(translate(generate,o(_Node,_,Lf),Total,X,Max,Succ),
	  hdrug_error(Msg,Args),
	  (   Total=0,
	      Succ=error,
	      print_message(error,hdrug_error(Msg,Args))
	  )
	 ),
    try_hook(Generator:count(NoEdges),NoEdges=0),
    try_hook(Generator:clean),
    (	Succ==time_out
    ->  NoAnalyses=time_out
    ;	Succ==out_of_memory
    ->  NoAnalyses=out_of_memory
    ;   Succ==error
    ->  NoAnalyses=error
    ;	hdrug_flag(found_solutions,NoAnalyses)
    ),
    maybe_trimcore,
    format(user_error,"T#~w|~w|~w|~w|~w|~w~n",[Ref,Len,NoAnalyses,Total,Generator,NoEdges]),
    assertz(hdrug:table_entry(Ref,Len,NoAnalyses,Total,Generator,NoEdges)).

%%%%%%%%%%%%%%%%%%%%%%%%
%%%% MAIN + OPTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%

hdrug_main :-
    catch(hdrug_main0,P,print_message(error,P)).

hdrug_main0 :-
    options,
    debug_message(3,"finished option processing~n",[]),
    start_interface_or_batch.

start_interface_or_batch :-
    hdrug_flag(batch_command,Val),
    system_type(DR),
    (	Val==undefined
    ->	Interactive=on
    ;	Interactive=off(Val)
    ),
    start_interface_or_batch(DR,Interactive).

start_interface_or_batch(development,Interactive) :-
    start_interface_or_batch(Interactive).
start_interface_or_batch(runtime,Interactive) :-
    bb_put(interactive,Interactive).  % communicate to runtime_entry/1

start_interface_or_batch(on) :-
    initialize_flag(demo,on),
    initialize_flag(debug,1),
    debug_message(3,"starting hdrug_initialization~n",[]),
    try_hook(hdrug_initialization), % must be after flags
    debug_message(3,"finished hdrug_initialization~n",[]),
    debug_message(3,"starting gui~n",[]),
    start_x,
    debug_message(3,"finished gui~n",[]),
    start_cmdint.
start_interface_or_batch(off(Call)) :-
    batch_information,
    initialize_flag(demo,off),
    initialize_flag(debug,0),
    debug_message(3,"starting hdrug_initialization~n",[]),
    try_hook(hdrug_initialization), % must be after flags
    debug_message(3,"finished hdrug_initialization~n",[]),
    debug_message(3,"starting ~w~n",[Call]),
    (	call(Call),
	fail
    ;	true
    ),
    debug_message(3,"finished call~n",[]),
    halt.

batch_information :-
    host_name(Host),
    pid(Pid),
    datime(Date),
    format(user_error,"hdrug: process ~w on host ~w (~w)~n",[Pid,Host,Date]).

runtime_entry(start) :-
    bb_get(interactive,Val),
    start_interface_or_batch(Val),
    hdrug_runtime_cmd_interpreter.

hdrug_runtime_write_answer([]) :-
    format(user_error,"~nyes~n",[]).
hdrug_runtime_write_answer([H|T]) :-
    format(user_error,"~n~w",[H]),
    hdrug_runtime_write_answer0(T).

hdrug_runtime_write_answer0([]) :-
    format(user_error," ? ",[]),
    hdrug_runtime_more.
hdrug_runtime_write_answer0([H|T]) :-
    format(user_error,",~n~w",[H]),
    hdrug_runtime_write_answer0(T).

hdrug_runtime_cmd_interpreter :-
    format(user_error,"Welcome to HDRUG; runtime command interpreter~n",[]),
    catch( hdrug_runtime_cmd_interpreter0,
	   Error,
	   (   print_message(error,Error),
	       hdrug_runtime_cmd_interpreter
	   )
	 ).

hdrug_runtime_cmd_interpreter0 :-
    %%	prompt(_,'o| ?- '),       % is written to user_error; but we want user
    format("o| ?- ",[]),
    ttyflush,
    read_term(Query,[variable_names(X)]),
    (	Query == halt
    ->	true
    ;   Query == end_of_file
    ->	true
    ;	(  call(Query),
	    hdrug_runtime_write_answer(X)
	;   format(user_error,"~nno~n",[])
	),
	hdrug_runtime_cmd_interpreter0
    ).

hdrug_runtime_more :-
    read_line(C),
    (	C = [59|_]
    ->	format(user_error,"~n",[]),fail
    ;	format(user_error,"~nyes~n",[])
    ).

%% parse options
%% (determine whether we want a develop system, or standalone (unix filter))
%% process options to the Ovis command
options :-
    prolog_flag(argv,Options),
    parse_options(Options).

usage :-
    format(user_error,"\
Usage: hdrug [Options]
Options are:~n",[]),
    help(option).

:- multifile
    usage_option/3.

usage_option(flag,"-flag Att Val",
"Sets global variable Att to Val; Val is read as an atom. Consider
using the Flag=Val option if you want to assign arbitrary Prolog terms
to Att.").
usage_option(iflag,"-iflag Att Val",
"Sets global variable Att to Val; Val is read as an
integer. Equivalent to Flag=Val where Val is an integer.").
usage_option(pflag,"-pflag Att Val",
"Sets prolog_flag(Att) to Val; Val is read as an atom. This is an
interface to the SICStus Prolog built-in predicate prolog_flag/3.").
usage_option(flag,"Flag=Val",
"Sets global variable Att to Val; Val is read as a Prolog
term. Consider using the opton -flag Flag Val if you want to assign an
atom to Att.").
usage_option(cmd,"-cmd Goal","evaluates Prolog Goal; Goal is parsed as
Prolog term. Example:

        hdrug -notk -cmd 'listing(library_directory)' -quit").
usage_option(tk,"-tk",
"Indicates that the graphical user interface should be started when
hdrug starts. Equivalent to tcltk=on. This is the default.").
usage_option(notk,"-notk",
"Indicates that the graphical user interface should not be started
when hdrug starts. Equivalent to tcltk=off. The default is to start
the graphical user interface.").
usage_option(dir,"-dir Dir",
"This options ensures that Dir is added to the list of library
directories.").
usage_option(help,"-help",
"This display usage information and terminates. ").
usage_option(l,"-l File",
"Loads the file File (containing Prolog), using the goal
use_module(File).").
usage_option(parser,"-parser Parser on/off",
"This option indicates that the parser Parser is set to on
(off). Parsers which are on will take part in parser comparison
runs.").
usage_option(generator,"-generator Generator on/off",
"This option indicates that the generator Generator is set to on
(off). Generators which are on will take part in generator comparison
runs.").
usage_option(quit,"-quit","Terminates Hdrug. Useful in combination
with the -cmd Goal option.").
usage_option(suite,"-suite Suite","Same as -flag suite Suite.").

%% parsing of options. Options are treated from left to right.

parse_options([]).
parse_options([P0h|P0t]):-
    (	option([P0h|P0t],P1)
    ->	parse_options(P1)
    ;   format(user_error,
	       "~n*** error *** ~nin command-line arguments at: ~w~n",
	       [P0h]),
	parse_options(P0t)
    ).

%% hookable
:- multifile
    option/3.
:- discontiguous
    option/3.

option -->
    prefix_option(FlagIsVal),
    {  parse_flag(FlagIsVal,Flag,Val),
       !,
       set_flag(Flag,Val),
       debug_call(1,wr_flag(Flag))
    }.

option -->
    [FlagIsVal],
    {  parse_flag(FlagIsVal,Flag,Val),
       !,
       set_flag(Flag,Val),
       debug_call(1,wr_flag(Flag))
    }.

option -->
    prefix_option(Prefix),
    option(Prefix).

prefix_option(Prefix) -->
    [Pref],
    {  concat('-',Prefix,Pref)  }.

no_dash([W|Ws]) -->
    [W],
    {  \+ concat('-',_,W)  },
    no_dash(Ws).
no_dash([]) --> [].


option(script) --> [].
option(state) --> [].
option(alt_state) --> [_].
option(altstate) --> [_].

option(flag) -->
    no_dash([Att,Value]),
    {  hdrug_flag(Att,_,Value),
       debug_call(1,wr_flag(Att))}.

option(iflag) -->
    no_dash([Att,Value0]),
    {  atom_codes(Value0,String),
       number_codes(Value,String),
       hdrug_flag(Att,_,Value),
       debug_call(1,wr_flag(Att))
    }.

option(pflag) -->
    no_dash([Att,Value]),
    {  prolog_flag(Att,_,Value)  }.

option(sversion) -->
    [Version],
    {  version(Version) }.

option(v) -->
    { version_command }.

option(version) -->
    { version_command }.

option(suite) -->
    no_dash([Suite]),
    {  hdrug_flag(suite,_,Suite),
       debug_call(1,wr_flag(suite))}.

onoff(on) --> [on].
onoff(off) --> [off].

option(lib) -->
    [Dir],
    {  assertz(library_directory(Dir))  }.

option(dir) -->
    [Dir],
    {  assertz(library_directory(Dir))  }.

option(cmd) -->
    [CommandAtom],
    {  atom_term(CommandAtom,Term),
       call(Term)
    }.

option(notk) -->
    {  set_flag(tcltk,off)  }.

option(tk) -->
    {  set_flag(tcltk,on)   }.

option(help) -->
    {  usage, halt          }.

option(l) -->
    [File],
    { use_module(File) }.

option(parser) -->
    [Parser],
    onoff(OnOff),
    {  set_flag(parser(Parser),OnOff)  }.

option(generator) -->
    [Generator],
    onoff(OnOff),
    {  set_flag(generator(Generator),OnOff)  }.

option(quit) -->
    { halt }.


:- multifile
    help_flag/2.
:- discontiguous
    help_flag/2.

help_flag(application_name,
"Used by the graphical user interface. Determines which application
default file is loaded, and the title of the widget.").

help_flag(batch_command,
"This flag can be used to run a command *after* `hdrug' is initialized
and *after* the application is initialized by
hdrug_initialization/0. The value of the flag is called as a goal and
all solutions are found using a failure-driven loop, after which the
program terminates.").

help_flag(clig_tree_active_nodes,
"Boolean flag which determines whether nodes of clig_trees should be
clickable. This is nice, but for large trees slow.").

help_flag(blt_graph_lines,
"on/off. Should we connect dots in a blt_graph widget? Default:
off.").

help_flag(debug,
"0/1/2. Determines the number and detail of continuation
messages. Default: 0 (minimum)").

help_flag(demo,
"on/off. If demo=on then the system provides somewhat more
information. A short representation of the semantic form of a parse
will be shown. Note that during test-suite runs this value is off.").

help_info(flag,generator_generator,"generator(Generator)",
"on/off. determines whether Generator is currently active or not,
i.e. whether it will take part in generator comparison runs or
not. ").

help_flag(nodeskip,
"Integer. This flag determines for LaTeX-based tree output the value
of nodeskip that is passed on to pstree. If you don't like the tree
that is produced then you might try to increase or decrease this
value. If the tree is ugly because nodes are too far apart, decrease
this value; if the tree is a mess, increase it.").

help_flag(object_exists_check,
"on/off. If parse and generation results are saved as objects (flag
object_saving) then the system normally checks whether an equivalent
object has already been constructed on the basis of the same parse /
generation request. This flag determines wether such a check should be
made").

help_flag(object_saving,
"This value determines whether parse/generation results should be kept
as objects for later inspection. If the value is off, no objects are
asserted. If it is semi then for each new parse request older objects
are removed. If it is on ojbjects are never removed.").

help_flag(compare_object_saving,
"This value determines whether parse/generation results should be kept
as objects for later inspection, during parser and generator comparison
runs. If the value is off, no objects are
asserted. If it is semi then for each new parse request older objects
are removed. If it is on ojbjects are never removed.").

help_flag(parser,
"Atom. Determines which parser is currently the parser to use for
parse commands.").

help_info(flag,parser_parser,"parser(Parser)",
"on/off. Determines whether Parser is currently active or not,
i.e. whether it will take part in parser comparison runs or not. ").

help_flag(complex_parse_widget,"on/off. Determines whether the parse
widget should contain a listbox with all the sentences from the suite to
select from. Default: off.").


help_flag(add_help_menu,"on/off. determines whether on-line help info
must be available through the graphical user interface. Should be
switched off if you run Hdrug on the display of a different machine
and the connection with that machine is slow (like over a
phone-line).").

help_flag(print_table_total,
"on/off. determines whether during a parse comparison totals should be
displayed after each sentence.").

help_flag(start_results_within_bound,
"Integer. Determines for the results_within_bound command
(hdrug_stats) at which number of millisecond reporting should
start. Default: 100.").

help_flag(end_results_within_bound,
"Integer. Determines for the results_within_bound command
(hdrug_stats) at which number of millisecond reporting should
end. Default: 5000.").

help_flag(incr_results_within_bound,
"Integer. Determines for the results_within_bound command
(hdrug_stats) with which number of millisecond increment percentages
should be shown. Default: 100.").

help_flag(clig_tree_hspace,
"Integer. Determines the horizontal width between nodes in CLIG
trees.").
help_flag(clig_tree_vspace,
"Integer. Determines the vertical width between nodes in CLIG
trees.").


help_flag(tcltk,
"on/off. Determines whether the graphical user interface
starts. Default: on").

help_flag(tkconsol,
"on/off. Determines whether or not the tkconsol feature
(cf. library(tkconsol) should be used. Default: off.").

help_flag(top_features,
"Atom. Determines top category used by the parsers. If the value is
`vp', then the predicate top(vp,Cat) determines the top category
(start symbol)").

help_flag(useful_try_check,
"on/off. This flag determines during a test-suite run whether a
sentence should be parsed even if a shorter sentence has already been
timed-out for the current parser. If the value is on, then the
sentence is skipped for the current parser.").

help_flag(dot_program,
"Name of the program that will be used to display dot output.").

help_flag(cmdint,
"on/off. This flag determines whether the command-interpreter should
be switched on upon startup. Default: off.~n").

help_flag(update_array_max,
"Integer. Indicates how many items are passed on in update_array/2
(this predicate is used to inform the gui about the available
predicates, types, lexical entries, test sentences, etc.). The default
is 1000. ").

help_flag(hdrug_status,
"This flag is not meant to be set by an application, but is set by
Hdrug to communicate the status of the latest parse/generation attempt.
The flag has three possible values: success, out_of_memory, time_out.
Every parse/generation starts out with te value 'success'. The latter
two values are set in the case of a time out exception and a resource
error exception.").

:- multifile
    help_hook/3.
:- discontiguous
    help_hook/3.

help_hook(help_hook,"help_hook(PredSymbol,UsageString,ExplanationString)","
This predicate can be defined to provide help on a hook predicate with
predicate symbol PredSymbol. The UsageString is a list of character
codes which shortly shows the usage of the predicate. The help_hook
predicate which is defined for the help_hook predicate itself has as
its UsageString \"help_hook(PredSymbol, UsageString,
ExplanationString)\". The ExplanationString is a list of
charactercodes containing further explanation.").

help_hook(parse,"ParserModule:parse(o(Cat,Str,Sem))",
"If ParserModule is the current parser, then this predicate is called
to do the actual parsing. At the time of calling, the argument of the
parse/1 predicate is a term o(Obj,Str,Sem) where Cat is a term in
which the top-category  is already instantiated. Furthermore, part of
the term may have been instantiated to some representation of the
input string (if the hook predicate phonology/2 was defined to do
so). The input string is also available in the second argument of the
o/3 term. The third argument is not used for parsing.").

help_hook(generate,"GeneratorModule:generate(o(Cat,Str,Sem))",
"If GeneratorModule is the current generator, then this predicate is
called to do the actual generation. At the time of calling, the
argument of the generate/1 predicate is a term o(Obj,Str,Sem) where
Cat is a term in which the top-category  is already
instantiated. Furthermore, part of the term may have been instantiated
to some representation of the input semantics (if the hook predicate
{semantics/2} was defined to do so). The input semantics is also
available in the third argument of the o/3 term. The second argument
is not used for generation.").

help_hook(count,"Module:count",
"This optional predicate is thought of as a predicate that might
display some statistical information e.g. on the number of chart edges
built. The predicate Module:count is called in module Parser after
parsing has been completed for parser Parser or it is called in module
Generator after generation has been completed for generator
Generator. Note that library(hdrug_util) contains predicates to count
the number of clauses for a given predicate.").

help_hook(count,"Module:count(Integer)",
"This optional predicate is thought of as a predicate that binds
Integer representing some statistical information e.g. on the number
of chart edges built. The predicate Module:count(Integer) is called in
module Parser after parsing has been completed for parser Parser or it
is called in module Generator after generation has been completed for
generator Generator. Note that library(hdrug_util) contains
predicates to count the number of clauses for a given predicate. The
resulting Integers are kept in the table_entry/6 records which are
used to summarize parser and generator comparison runs").

help_hook(clean,"Module:clean","This optional predicate is thought of
as a predicate that might remove e.g. chart edges added dynamically to
the database once parsing has been completed. The predicate
Module:clean is called in module Parser after parsing has been
completed for parser Parser or it is called in module Generator after
generation has been completed for generator Generator. ").

help_hook(start_hook,"start_hook(parse/generate,Module,o(A,B,C),Term)",
"This predicate is a hook that is called before the parser starts. Its
first argument is either the atom parse or the atom generate; the
second argument is the current parser or generator (hence the name of
the module); the third argument is an object. The fourth argument can
be anything. It wis provided to pass on arbitrary information to the
result_hook and end_hook hook predicates. For example, the predicate
could pass on information concerning the current memory usage of
Sicstus. This information could then be used by end_hook to compute
the amount of memory that the parser has consumed. The time required
by the start_hook predicate is NOT considered to be part of parsing
time; cf start_hook0/4 for a similar hook predicate of which timing IS
considered part of parsing time").

help_hook(start_hook0,"start_hook0(parse/generate,Module,o(A,B,C),Term)",
"This predicate is a hook that is called before the parser starts. Its
first argument is either the atom parse or the atom generate; the
second argument is the current parser or generator (hence the name of
the module); the third argument is an object. The fourth argument can
be anything. It is provided to pass on arbitrary information to the
result_hook and end_hook hook predicates. For example, the predicate
could pass on information concerning the current memory usage of
Sicstus. This information could then be used by end_hook to compute
the amount of memory that the parser has consumed. The time required
by the start_hook0 predicate IS considered to be part of parsing time;
cf start_hook/4 for a similar hook predicate of which timing is NOT
considered part of parsing time").

help_hook(result_hook,"result_hook(parse/generate,Module,o(A,B,C),Term)",
"This predicate is a hook that is called for each time the parser or
generator succeeds. Its first argument is either the atom parse or the
atom generate; the second argument is the current parser or generator
(hence the name of the module); the third argument is an object. The
fourth argument can be anything. It is provided to pass on arbitrary
information from the start_hook hook predicate. Warning: the time
taken by result_hook will always be considered as part of the time
required for parsing. Consider using the demo flag to ensure that
expensive result_hooks are not fired for parsing comparison runs.").

help_hook(end_hook,"end_hook(parse/generate,Module,o(A,B,C),Term)",
"This predicate is a hook that is called if the parser / generator can
not wfind any results anymore. Its first argument is either the atom
parse or the atom generate; the second argument is the current parser
or generator (hence the name of the module); the third argument is an
object. The fourth argument can be anything. It is provided to pass on
arbitrary information from the start_hook hook predicate. Note that at
the moment of calling this predicate the object will typically NOT be
instantiated. The time required by end_hook is NOT considered to be
part of parsing time; see end_hook0.").

help_hook(end_hook0,"end_hook0(parse/generate,Module,o(A,B,C),Term)",
"This predicate is a hook that is called if the parser / generator can
not find any results anymore. Its first argument is either the atom
parse or the atom generate; the second argument is the current parser
or generator (hence the name of the module); the third argument is an
object. The fourth argument can be anything. It is provided to pass on
arbitrary information from the start_hook hook predicate. Note that at
the moment of calling this predicate the object will typically NOT be
instantiated. The time required by end_hook0 IS considered to be part
of parsing time; see end_hook0.").

help_hook(top,"top(Name,Cat)",
"Usually a grammar comes with a notion of a `start symbol' or `top
category'. In Hdrug there can be any number of different top
categories, of which one is the currently used top category. These top
categories are Prolog terms. Each one of them is associated with an
atomic identifier for reference purposes. Each top category is defined
by a clause for the predicate top/2, where the first argument is the
atomic identifier and the second argument is the top-category
term. The latter term will be unified with the first argument of the
o/3 terms passed on to parsers and generators.

        top(s,node(s,_)).
        top(np,node(np,_)).

The flag `top_features' is used to indicate what the current choice of
top-category is. Usually an application defines a default value for
this flag. The identifier relates to the first argument of a top/2
definition.").
help_hook(semantics,"semantics(Cat,Sem)",
"The predicate semantics/2 defines which part of an object contains
the semantics (if any). For example, if in an application categories
are generally of the form node(Syn,Sem), then the following definition
of semantics/2 is used:

        semantics(node(_,Sem),Sem).

The predicate is mainly used for generation. ").

help_hook(phonology,"phonology(Cat,Phon)",
"This predicate is useful for `sign-based' grammars in which the
string to be parsed is considered a part of the category. This
predicate is called before parsing so that in such cases the current
string Phon can be unified with some part of the object. ").

help_hook(extern_sem,"extern_sem(Extern,Intern)",
"This predicate can be defined in order to distinguish internal and
external semantic representations. This predicate is used in two ways:
if a semantic representation is read in, and if a semantic
representation is written out. The first argument is the external
representation, the second argument the internal one. The default
definition is extern_sem(X,X). A typical usage of this predicate could
be a situation in which an external format such as kisses(john,mary)
is to be translated into a feature structure format such as [
pred=kisses, arg1=john, arg2=mary]. NB, the external format is read in
as a single Prolog term.").

help_hook(extern_phon,"extern_phon(Extern,Intern)",
"This predicate can be defined in order to distinguish internal and
external phonological representations. This predicate is used in two
ways: if a phonological representation is read in, and if a
phonological representation is written out. The first argument is the
external representation, the second argument the internal one. The
default definition is extern_phon(X,X). NB, the external format is
read in as a list of Prolog terms. ").

help_hook(sentence,"sentence(Key,Sentence),
sentence(Key,Max,Sentence)",
"Applications can define a number of test sentences by defining
clauses for this predicate. For ease of reference, Key is some atomic
identifier (typically an integer). Sentence is typically a list of
atoms. The parser comparison predicates refer to this atomic
identifier. Example sentences are also listed in the listbox available
through the parse menu-button. Max can be an integer indicating the
maximum amount of milliseconds allowed for this sentence in parser
comparison runs.").

help_hook(lf,"lf(Key,LF), lf(Key,Max,Lf)",
"Applications can define a number of test logical forms by defining
clauses for this predicate. For ease of reference, Key is some atomic
identifier (typically an integer). LF is a term (external format of a
logical form). The generator comparison predicates refer to this
atomic identifier. Example logical forms are also listed in the
listbox available through the generate menu-button. Max can be an
integer indicating the maximum amount of milliseconds allowed for this
lf in generator comparison runs.").

help_hook(user_max,"user_max(Length,Max)",
"This predicate is used to define an upper time limit, possibly based
on the length of the test sentence (the first argument), for parsing
that sentence in a test-suite run. By default, Hdrug behaves as if
this predicate is defined as follows: user_max(L,Max) :- Max is 10000
+ (L * L * 300). If you don't want a time out at all, then define this
predicate as user_max(_,0).").

help_hook(gram_startup_hook_begin,"gram_startup_hook_begin",
"This predicate is meant to be used to extend the graphical user
interface. It is called right before Hdrug's own graphical user
interface definitions are loaded (i.e., right before hdrug.tcl is
sourced).").

help_hook(gram_startup_hook_end,"gram_startup_hook_end",
"This predicate is meant to be used to extend the graphical user
interface. It is called right after Hdrug's own graphical user
interface definitions are loaded (i.e., right after hdrug.tcl is
sourced). A typical use is to add application specific menu-buttons,
etc. ").

help_hook(user_clause,"user_clause(Head,Body)",
"If you want to use Hdrug's built-in facilities to view Prolog
clauses, then it is neccessary that these clauses are accessible via
the predicate user_clause/2. The arguments of this predicate are the
head and the body of the clause respectively. Note that the body of
the clause should be provided as a list of goals, rather than a
conjunction. The reason that Hdrug does not use the built-in clause/3
predicate, is that this predicate is only available for dynamic
clauses.  ").

help_hook(graphic_path,"graphic_path(Format,Obj,Term)",
"One of the three hook predicates which together define tree
formats. The others are graphic_label/3 and graphic_daughter/4. The
Hdrug libraries contain extensive possibilities to produce output in
the form of trees. Only a few declarations are needed to define what
things you want to see in the tree. In effect, such declarations
define a `tree format'. In Hdrug, there can be any number of tree
formats. These tree formats are named by a ground identifier. A tree
format consists of three parts: the path definition indicates what
part of the object you want to view as a tree; the label definition
indicates how you want to print the node of a tree; and the daughter
definition indicates what you consider the daughters of a node. The
graphic_path definition is the first part. For instance if the parser
creates an object of the form node(Syn,Sem,DerivTree) where DerivTree
is a derivation tree, then we can define a tree format `dt' where the
graphic_path definition extracts the third argument of this term:
graphic_path(dt,node(_,_,Tree),Tree).").

help_hook(graphic_label,"graphic_label(Format,Node,Label)",
"One of the three hook predicates which together define tree
formats. The others are graphic_path/3 and graphic_daughter/4. The
Hdrug libraries contain extensive possibilities to produce output in
the form of trees. Only a few declarations are needed to define what
things you want to see in the tree. In effect, such declarations
define a `tree format'. In Hdrug, there can be any number of tree
formats. These tree formats are named by a ground identifier. A tree
format consists of three parts: the path definition indicates what
part of the object you want to view as a tree; the label definition
indicates how you want to print the node of a tree; and the daughter
definition indicates what you consider the daughters of a node. The
graphic_label definition is the second part. For instance, if subtrees
are of the form tree(Node,Ds), where Node are terms representing
syntactic objects such as np(Agr,Case) and vp(Agr,Subcat,Sem) then a
tree format could be defined which only displays the functor symbol:
graphic_label(syn,tree(Term,_),Label) :- functor(Term,Label,_).").

help_hook(graphic_daughter,"graphic_daughter(Format,No,Term,Daughter)",
"One of the three hook predicates which together define tree
formats. The others are graphic_label/3 and graphic_daughter/4. The
Hdrug libraries contain extensive possibilities to produce output in
the form of trees. Only a few declarations are needed to define what
things you want to see in the tree. In effect, such declarations
define a `tree format'. In Hdrug, there can be any number of tree
formats. These tree formats are named by a ground identifier. A tree
format consists of three parts: the path definition indicates what
part of the object you want to view as a tree; the label definition
indicates how you want to print the node of a tree; and the daughter
definition indicates what you consider the daughters of a node. The
graphic_daughter definition is the third part. For instance if
subtrees are of the form tree(Label,Daughters), where Daughters is a
list of daughters, then you could simply define:
graphic_daughter(syn,No,tree(_,Ds),D):- lists:nth(No,Ds,D).").

help_hook(show_node,"show_node(Format,Node)",
"If trees are displayed on the canvas widget, then it is possible to
define an action for clicking the left-most mouse button on the node
of the tree. This action is defined by this predicate. Format is the
identifier of a tree format, and Node is the full sub-tree (that was
used as input to the graphic_label definition).").

help_hook(show_node2,"show_node2(Format,Node)",
"If trees are displayed on the canvas widget, then it is possible to
define an action for clicking the middle mouse button on the node of
the tree. This action is defined by this predicate. Format is the
identifier of a tree format, and Node is the full sub-tree (that was
used as input to the graphic_label definition).").

help_hook(show_node3,"show_node3(Format,Node)",
"If trees are displayed on the canvas widget, then it is possible to
define an action for clicking the rightmost mouse button on the node
of the tree. This action is defined by this predicate. Format is the
identifier of a tree format, and Node is the full sub-tree (that was
used as input to the graphic_label definition).").

help_hook(tk_tree_user_node,"tk_tree_user_node(Label,Frame)",
"If a tree-format is defined which matches user(_), then if a tree is
to be displayed on the Canvas widget this predicate is responsible for
creating the actual nodes of the tree. Label is the current label, and
Frame is the identifier of a Tcl/Tk frame which should be further used
for this label. The frame is already packed.").

help_hook(clig_tree_user_node,"clig_tree_user_node(Label)",
"If a tree-format is defined which matches user(_), then if a tree is
to be displayed using Clig output, then this predicate is responsible
for creating the actual nodes of the tree. Label is the current
label.").

help_hook(dot_tree_user_node,"dot_tree_user_node(Label)",
"If a tree-format is defined which matches user(_), then if a tree is
to be displayed using DOT output, then this predicate is responsible
for creating the actual label of the nodes of the tree. Label is the current
label.").

help_hook(latex_tree_user_node,"latex_tree_user_node(Label)",
"If a tree-format is defined which matches user(_), then if a tree is
to be displayed using LaTeX output, then this predicate is responsible
for creating the actual nodes of the tree. Label is the current
label.").

help_hook(shorten_label,"shorten_label(Label0,Label)",
"This predicate can be defined for feature-structure display of tree
nodes; its intended use is to reduce the information of a given
node.").

help_hook(call_build_lab,"call_build_lab(F,Fs,L)",
"for library(hdrug_call_tree)").

help_hook(call_ignore_clause,"call_build_lab(Functor/Arity)",
"for library(hdrug_call_tree)").

help_hook(exceptional_sentence_length,
"exceptional_sentence_length(Phon,Length)",
"For (internal) phonological representations this predicate can be
defined to return the length of the representation. If the predicate
is not defined, then the representation is assumed to be a list, and
the length is assumed to be the number of elements of the list. The
length of phonological representations is used by the display of the
results of parser comparison runs.").

help_hook(exceptional_lf_length,
"exceptional_lf_length(Sem,Length)",
"For (internal) semantic representations this predicate can be defined
to return the length of the representation. If the predicate is not
defined, then the representation is assumed to be a term, and the
length is assumed to be the number of characters required to print the
term. The length of semantic representations is used by the display of
the results of generator comparison runs.").

help_hook(hdrug_initialization,"hdrug_initialization",
"If hdrug is started, then three things happen. First, hdrug treats
its command line options. After that, the predicate
hdrug_initialization is called. Finally, the graphical user interface
is started (if hdrug_flag(tcltk) is on). This predicate can thus be used to
define application-specific initialization.").

help_hook(hdrug_command,"hdrug_command(Name,Goal,Args)",
"This predicate can be used to define further commands for the command
interpreter. Name is the first word of the command, Goal is the
resulting Prolog goal, and Args is a possibly empty list of arguments
to the command.").

help_hook(hdrug_command_help,
"hdrug_command_help(Name,UsageString,ExplanationString)",
"This predicate can be used to provide help information on commands
for the command interpreter. Name is the first word of the command,
The second argument displays usage information in a short form (list
of character codes); the third argument is a list of character codes
containing an explanation of the command.").

help_hook(help_flag,"help_flag(Flag,Help)",
"This predicate can be used to provide help information on global
variable Flag. Help is a list of character codes containing the help
info.").

help_hook(option,"option(Option,ArgvIn,ArgvOut)",
"This predicate can be used to define application-specific
command-line options to the hdrug command. Option is the option minus
the minus sign; moreover Option relates to the first argument of a
corresponding usage_option/3 definition. The second and third argument
is a difference list of the list of options in case the option takes
further arguments. ").

help_hook(usage_option,
"usage_option(Option,UsageString,ExplanationString)",
"This predicate is defined to provide help information on the Option
startup option (cf. option/3). The UsageString is a list of character
codes presenting short usage information; ExplanationString is a list
of character codes containing the explanation of the option. ").

help_hook(tk_tree_show_node_help,
"tk_tree_show_node_help(TreeFormat,Atom)",
"If a tree according to TreeFormat is displayed on the canvas, then
this predicate can be defined in order that below the widget a short
message appears indicating what actions are bound to clicking on the
tree nodes. Atom is the message.").

help_hook(show_relation,"show_relation(F/A[,Medium])",
"you can define the relation show_relation/1 to define an action for
pressing the first mouse-button on a relation name, when viewing
predicate definitions in the Tk Canvas. The argument is a
Functor/Arity pair. For example,

        show_relation(F/A) :-
                show_predicate(F/A,fs,tk).

	show_relation(F/A,Medium) :-
	        show_predicate(F/A,fs,Medium).

will show the predicate definition.").

help_hook(display_extern_sem,"display_extern_sem(+ExtSem)",
"Predicate to print a given external format of semantics.").

help_hook(display_extern_phon,"display_extern_phon(+ExtPhon)",
"Predicate to print a given external format of phonology.").

help_hook(compile_test_suite,"compile_test_suite(+File)",
"Predicate to compile the test suite in file File.").

help_hook(reconsult_test_suite,"reconsult_test_suite(+File)",
"Predicate to reconsult the test suite in file File.").

help_hook(show_object_default2,"show_object_default2(+Int)",
"Predicate which is called if the user presses mouse button <2> on the
object button number Int. A typical definition could be, for instance:

	 show_object_default2(No):-
	     show_object_no(No,tree(syn),clig).").

help_hook(show_object_default3,"show_object_default3(+Int)",
"Predicate which is called if the user presses mouse button <2> on the
object button number Int.").



help_info(flag,Key,Short,Long) :-
    help_flag(Key,Long),
    atomic(Key),
    atom_codes(Key,Short).
help_info(option,Key,Short,Long):-
    usage_option(Key,Short,Long).
help_info(hook,Key,Short,Long) :-
    help_hook(Key,Short,Long).
help_info(pred,Key,Short,Long) :-
    help_pred(Key,Short,Long).
help_info(command,Key,Short,Long):-
    hdrug_cmdint:hdrug_command_help(Key,Short,Long).


try_reconsult_test_suite(File) :-
    try_hook(reconsult_test_suite(File),
	     reconsult(File)
	    ).
try_compile_test_suite(File) :-
    try_hook(compile_test_suite(File),
	     compile(File)
	    ).

help_info(module,user,"HDRUG: A Development Environment for Logic Grammars",
"
Hdrug is an environment to develop grammars, parsers and generators
for natural languages.  The system provides a number of visualisation
tools, including visualisation of feature structures, syntax trees,
type hierarchies, lexical hierarchies, feature structure trees,
definite clause definitions, grammar rules, lexical entries, chart
datastructures and graphs of statistical information e.g. concerning
cputime requirements of different parsers.  Visualisation can be
requested for various output formats, including ASCII text format, TK
Canvas widget, LaTeX output, DOT output, and CLiG output.

Extendibility and flexibility have been major concerns in the design
of Hdrug.  The Hdrug system provides a small core system with a large
library of auxiliary relations which can be included upon demand.
Hdrug extends a given NLP system with a command interpreter, a
graphical user interface and a number of visualisation tools.
Applications using Hdrug typically add new features on top of the
functionality provided by Hdrug.  The system is easily extendible
because of the use of the Tcl/Tk scripting language, and the
availability of a large set of libraries.  Flexibility is obtained by
a large number of global flags which can be altered easily to change
aspects of the system.  Furthermore, a number of hook predicates can
be defined to adapt the system to the needs of a particular
application.

The flexibility is illustrated by the fact that Hdrug has been used
both for the development of grammars and parsers for practical systems
but also as a tool to experiment
with new theoretical notions and alternative processing strategies.
Furthermore, Hdrug has been used extensively
both for batch processing of large text corpora, and also for
demonstrating particular applications for audiences of non-experts.

Hdrug is implemented in SICStus Prolog version 3, exploiting the
built-in Tcl/Tk library. The Hdrug sources are available free of
charge under the Gnu Public Licence copyright restrictions.

1.1 Interface

Hdrug provides three ways of interacting with the underlying NLP
system:
        * Using an extendible command interpreter.
        * Using Prolog queries.
        * Using an extendible graphical user interface (based on Tcl/Tk).

The first two approaches are mutually exclusive: if the command
interpreter is listening, then you cannot give ordinary Prolog
commands and vice versa. In contrast, the graphical user interface
(with mouse-driven menu's and buttons) can always be used. This
feature is very important and sets Hdrug apart from competing systems.
It implies that we can use at the same time the full power of the
Prolog prompt (including tracing) and the graphical user interface.
Using the command interpreter (with a history and alias mechanism) can
be useful for experienced users, as it might be somewhat faster than
using the mouse (but note that many menu options can be selected using
accelerators). Furthermore, it is useful for situations in which the
graphical user interface is not available (e.g. in the absence of an X
workstation).  The availability of a command-line interface in
combination with mouse-driven menu's and buttons illustrates the
*flexible* nature of the interface.

An important and interesting property of both the command interpreter
and the graphical user interface is *extendibility*. It is very
easy to add further commands (and associated actions) to the command
interpreter (using straightforward DCG syntax).  The graphical user
interface can be extended by writing Tcl/Tk scripts, possibly in
combination with some Prolog code. A number of examples will be given
in the remainder of this paper.

Finally note that it is also possible to run Hdrug without the
graphical user interface present (simply give the *notk* option
at startup). This is sometimes useful if no X workstation is available
(e.g. if you connect to the system over a slow serial line), but also
for batch processing.  At any point you can start or stop the
graphical user interface by issuing a simple command.

1.2 Visualisation

Hdrug supports the visualisation of a large collection of
data-structures into a number of different formats.

These formats include (at the moment not all datastructures
are supported for all formats. For example, plots of two dimensional
data is only available for Tk):

        * ASCII art
        * Tk Canvas
        * LaTeX
        * CLiG
	* DOT

The data-structures for which visualisation is provided
are:

* Trees. Various tree definitions can exist in parallel. For example,
the system supports the printing of syntax trees, derivation trees,
type hierarchy trees, lexical hierarchies etc. Actions can be
defined which are executed upon clicking on a node of a tree. New
tree definitions can be added to the system by simple declarations.

* Feature structures. Clicking on attributes of a feature-structure
implode or explode the value of that attribute.  Such feature
structures can be the feature structures associated with grammar
rules, lexical entries, macro definitions and parse results.

* Trees with feature structure nodes. Again, new tree definitions
can be declared. An example is
http://www.let.rug.nl/~vannoord/Hdrug/Manual/dt.png

* Graph (plots of two variable data), e.g. to display the
(average) cputime or memory requirements of different parsers.

* Tables.

* Prolog clauses.

* Definite clauses with feature structure arguments. This can be
used e.g. to visualise macro definitions, lexical entries, and
grammar rules (possibly with associated constraints).

1.3 Parser and Generator Management

Hdrug provides an interface for the definition of parsers and
generators.  Hdrug manages the results of a parse or generation
request. You can inspect these results later.  Multiple parsers and
generators can co-exist. You can compare some of these parsers with
respect to speed and memory usage on a single example sentence, or on
sets of pre-defined example sentences. Furthermore, actions can be
defined which are executed right before parsing (generation) starts,
or right after the construction of each parse result (generation
result), or right after parsing is completed.

1.4 Useful Libraries

Most of the visualisation tools are available through libraries as
well. In addition, the Hdrug library contains mechanisms to translate
Prolog terms into feature structures and vice versa (on the basis of a
number of declarations). Furthermore, a library is provided for the
creation of `Mellish' Prolog terms on the basis of boolean expressions
over finite domains. The reverse translation is provided too. Such
terms can be used as values of feature structures to implement a
limited form of disjunction and negation by unification.

A number of smaller utilities is provided in the library as well, such
as the management of global variables, and an extendible on-line help
system.
	  ").

help_info(class,flag,"Global Variables",
"

Hdrug manages a number of `global' variables.  A flag consists of a
ground key (the `global variable') and a value (arbitrary Prolog
term).  Flags can be set by means of command-line options,
command-interpreter commands, the Options menu, and directly by Prolog
predicates.

Global variables are passed on to Tcl/Tk. For this purpose there
exists a Tcl/Tk array variable, called `flag'. If the graphical user
interface is running, then this Tcl/Tk variable is automatically
updated when the Prolog flag is altered.
The predicate tk_flag/1 can be used to explicitly send the value of a
flag to Tcl/Tk.

").

help_info(class,option,"Command-line Options",
"When Hdrug is started, it first interprets the command-line
options. Command-line options are interpreted from left to right. The
following section lists the command-line options which are
standard. Each application possibly extends this list: this is defined
below.

Note that command-line options are interpreted *before*
application-specific initialization is performed. This is to allow
command-line options to have an effect on this initialization. Refer
to the hook predicate *hdrug_initialization* for application-specific
initialization.

Hdrug applications can extend the list of possible startup options by
adding definitions to the multifile predicate option/3. Short usage
information for such options can be defined with further definitions
for the multifile predicate usage_option/3. An an example, the
following definitions ensure that an option -rc File will reconsult
the file File:

        :- multifile option/3, usage_option/3.

        option(rc) --> [File], { reconsult(File) }.
        usage_option(rc,\"rc File\",\"File is reconsulted.\").



").

help_info(class,command,"Command Interpreter","

In principle there are three ways to interact with Hdrug:

       * SICStus Prolog Top-level
       * Command Interpreter
       * Graphical User Interface

The first two items are mutually exclusive: if the command interpreter
is listening, then you cannot give ordinary SICStus Prolog commands
and vice versa. The graphical user interface can be used in
combination with both the SICStus Prolog Top-level or the command
interpreter.

Prolog queries are given as ordinary Sicstus commands. This way of
interacting with Hdrug can be useful for low level debugging etc.
Using the command interpreter can be useful for experienced users, as
it might be somewhat faster than using the graphical user interface.


The command-interpreter features a history and an alias mechanism. It
includes a facility to escape to Unix, and is easily extendible by an
application.

The command interpreter is started by the Prolog predicate r/0 The
command interpreter command *p* halts the command interpreter (but
Hdrug continues).

Commands for the command interpreter always constitute one line of
user input. Such a line of input is tokenized into a number of *words
using spaces and tabs as separation symbols. The first *word* is taken
as the command; optional further words are taken as arguments to the
command. Each command will define certain restrictions on the number
and type of arguments it accepts.

Each word is treated as a Prolog atomic (either atom or integer, using
name/2).  In order to pass a non-atomic Prolog term as an argument to
a command, you need to enclose the word in the meta-characters {
and }. For example, the flag command can be used to set a global
variable. For example:

        16 |: flag foo bar

sets the flag *foo* to the value *bar*. As an other example,

        17 |: flag foo bar(1,2,3)

sets the flag *foo* to the Prolog atom 'bar(1,2,3)'. Finally,

        17 |: flag foo {bar(1,2,3)}

sets the flag *foo* to the complex Prolog term bar(1,2,3),
i.e. a term with functor bar/3 and arguments 1, 2 and 3.

Also note that in case the {,} meta-characters are used, then the
variables occurring in words take scope over the full
command-line. For instance, the parse command normally takes a
sequences of words:

        18 |: parse John kisses Mary

In other to apply the parser on a sequence of three variables, where
the first and third variable are identical, you give the command:

        19 |: parse {A} {B} {A}

The special meaning of the {,} meta-characters can be switched off
by putting a backslash in front of them. For example:

        53 |: tcl set jan \\{ piet klaas \\}
        =>  piet klaas

        54 |: tcl puts jan
        jan

The following meta-devices apply: All occurences of $word are
replaced by the definition of the alias *word*.  The alias command
itself can be used to define aliases:

        19 |: alias hallo ! cat hallo
        20 |: $hallo

so command number 20 will have the same effect as typing

        33 |: ! cat hallo

and if this command had been typed as command number 33 then
typing

        35 |: $33

gives also the same result.

Moreover, if no alias has been defined, then it will apply the last
command that started with the name of the alias:

        66 |: parse john kisses mary
        67 |: $parse

Both commands will do the same task.


It is possible to add commands to the command interpreter. The idea is
that you can define further clauses for the multifile predicate
hdrug_command/3.  The first argument is the first word of the command.
The second argument will be the resulting Prolog goal, whereas the
third argument is a list of the remaining words of the command (the
arguments to the command).

        :- multifile hdrug_command/3.

        hdrug_command(plus,(X is A+B, format('~w~n',[X])),[A,B]).

Relevant help information for such a command should be defined using
the multifile predicate hdrug_command_help/3. The first argument of
this predicate should be the same as the first argument of
hdrug_command. The second and third arguments are strings (list of
character codes). They indicate respectively usage information, and a
short explanation.

        :- multifile hdrug_command_help/3.
        hdrug_command_help(plus,\"plus A B\",\"Prints the sum of A and B\").


For example, consider the case where we want the
command *rx* to restart the Tcl/Tk interface. Furthermore, an
optional argument of `on' or `off' indicates whether the TkConsol feature
should be used. This could be defined as follows:

        hdrug_command(rx,restart_x,L):-
            rxarg(L).

        rxarg([on])  :- set_flag(tkconsol,on).
        rxarg([off]) :- set_flag(tkconsol,off).
        rxarg([]).

        hdrug_command_help(rx,\"rx [on,off]\",
             \"(re)starts graphical user interface with/without TkConsol\").

").


help_info(class,hook,"Interfacing Hdrug","

For an application to work with the Hdrug system, there are a number
of predicates you have to supply. Furthermore, you can extend the Hdrug
system with application-specific options. Finally, you can always
overwrite existing Hdrug definitions. In this chapter I discuss
the various possibilities.

Parsers and Generators

In Hdrug you can define any number of parsers and generators. A parser
and generator is identified by an atomic identifier. A parser is
declared by the following directive:

        :- hdrug_flag(parser(Identifier),on/off).

Similarly, a generator is declared by:

        :- hdrug_flag(generator(Identifier),on/off).

This defines a parser of generator and moreover tells Hdrug whether
this parser is active (on) or not (off).  Only if a parser is active,
it will be used in parser-comparison runs.  Not only should the application
define which parsers and generators exist, but usually it will also
define the `current' parser and generator.  This is achieved by
initializing the *parser* and *generator* flag.

        :- initialize_flag(parser,Identifier).
        :- initialize_flag(generator,Identifier).

Summarizing, there exist a number of parsers. A subset of those
parsers are active. One of the parsers is the current parser.

If a parser (generator) is defined, then there should be a module with
the same name which provides the following predicates. Note that only
the first one of these predicates, parse/1 or generate/1, is
obligatory. The others are not.

* parse/1;generate/1. This predicate is the predicate that does the
actual parsing (generation).  At the time of calling, the argument of
the parse/1 (generate/1) predicate is a term o(Obj,Str,Sem) where Obj
is a term in which the top-category is already instantiated.
Furthermore, part of the term might be instantiated to some
representation of the input string (in case of parsing if the
predicate phonology/2 is defined) or some representation of the input
logical form (in case of generation if the predicate semantics/2 is
defined). But note that the string and logical form are also available
(if instantiated) in the second and third argument of the o/3 term.

* count/0.This optional predicate is thought of as a predicate that
might produce some statistical information e.g. on the number of chart
edges built. Note that library(hdrug_util) contains predicates to
count the number of clauses for a given predicate.

* count/1. Similarly, but this time the argument should get bound to
some integer. The argument of this predicate determines the final
argument of the table_entry/6 predicate in test runs.

* clean/0. If the parser adds items, chart edges etc. to the database,
then this predicate defines the way to remove these again.


Top categories

Usually a grammar comes with a notion of a `start symbol' or `top
category'.  In Hdrug there can be any number of different top
categories. These top categories are Prolog terms. Each one of them is
associated with an atomic identifier for reference purposes. Each top
category is defined by a clause for the predicate top/2,
where the first argument is the atomic identifier and the second
argument is the top-category term. For example:

        top(s,node(s,_)).
        top(np,node(np,_)).

The flag `top_features' is used to indicate what the current choice
of top-category is. Usually an application defines a default value for
this flag by the directive:

        :- initialize_flag(top_features,Identifier).

The identifier relates to the first argument of a top/2 definition.

Strings and Semantics

The predicate semantics/2 defines which part of an object
contains the semantics (if any). For example, in an application
categories are generally of the form node(Syn,Sem). Therefore,
the following definition of semantics/2 is used:

        semantics(node(_,Sem),Sem).

The predicate is mainly used for generation. By default, the predicate
is defined as semantics(_,_).

In a similar way, the predicate phonology(Node,Phon) can be
defined. This is only useful for `sign-based' grammars in which the
string to be parsed is considered a part of the category. The default
definition is phonology(_,_).

The predicate extern_sem/2 can be used to define a mapping between
`internal' and `external' formats of the semantic representation. This
predicate is used in two ways: if a semantic representation is read
in, and if a semantic representation is written out. The first
argument is the external representation, the second argument the
internal one. The default definition is extern_sem(X,X).

Grammar compilation

Currently, the grammar menu contains four distinct options to
recompile (parts of) the grammar. It is assumed that if an application
is started, the grammars are already compiled. These options will thus
be chosen if the grammar has to be recompiled (e.g. because part of
the grammar has been changed).

The following four predicates have to be provided by the application.
If these predicates do not fulfill your needs, you can always extend
the grammar menu (cf. below), or even overwrite it (as in the Ale
application).

* compile_grammar/0 should recompile the whole grammar.

* reconsult_grammar/0 should recompile the whole grammar. If files
are to be loaded, then `reconsult' is used rather than `compile'. This
allows easier debugging.

* compile_grammar_file/1 should recompile the grammar file that is
its argument.

* reconsult_grammar_file/1 idem, but uses reconsult

Test-suites

A test suite consists of a number of Prolog clauses for the predicate
sentence/2, where the first argument is a unique identifier of that
sentence, and the second argument is a list of atoms; and clauses for
the predicate lf/2.  For example:

        sentence(a,[john,kisses,mary]).
        sentence(b,[john,will,kiss,mary]).
        lf(1,fut(kiss(john,mary))).
        lf(2,past(kiss(mary,john))).

The test suite might also contain a definition of the predicate
user_max/2.  This predicate is used to define an upper time limit,
possibly based on the length of the test sentence (the first
argument), for parsing that sentence in a test-suite run.  By default,
Hdrug behaves as if this predicate is defined as follows:

        user_max(L,Max) :-
             Max is 10000 + (L * L * 300).

Statistical information for each parse is preserved by the dynamic
predicate table_entry/6.
The arguments of this predicate
indicate:

* an atom (the unique identifier of the sentence)

* an integer (the length of the sentence)

* an integer (the number of parses of the sentence, i.e. the
degree of ambiguity)

* an atom (the name of the parser)

* an integer (the amount of milliseconds it took to parse the
sentence. In case of time-out the atom `time_out').

* a term (often used to indicate the number of chart-edges built).
It is determined by the count/1.

Extending the Graphical User Interface

It is easy to extend the Graphical User Interface for a specific
application. There are two predicates that you can define. The first
predicate, gram_startup_hook_begin/0 is called before loading of
hdrug.tcl, whereas the predicate gram_startup_hook_end/0 is called at
the end of the loading of this file.

Viewing Prolog Clauses

If you want to use Hdrug's built-in facilities to view Prolog clauses,
then it is neccessary that these clauses are accessible via the
predicate user_clause/2.  The arguments of this predicate are the head
and the body of the clause respectively. The reason that Hdrug does
not use the built-in clause/3 predicate, is that this predicate is
only available for dynamic clauses. ").


help_info(class,pred,"List of Predicates","This chapter lists the
important predicates used in Hdrug.").

help_info(class,application,"Hdrug Applications",
"This chapter shortly lists a number of example applications which are
part of the Hdrug distribution.").

help_info(application,ale,"Ale",

"The Attribute-Logic Engine by Bob Carpenter and Gerald Penn is a
freeware logic programming and grammar parsing and generation system.
The following description is quote from the Ale Homepage
http://www.sfs.nphil.uni-tuebingen.de/~gpenn/ale.html:

  [Ale] integrates phrase structure parsing, semantic-head-driven
  generation and constraint logic programming with typed feature
  structures as terms. This generalizes both the feature structures of
  PATR-II and the terms of Prolog II to allow type inheritance and
  appropriateness specifications for features and values. Arbitrary
  constraints may be attached to types, and types may be declared as
  having extensional structural identity conditions. Grammars may also
  interleave unification steps with logic program goal calls (as can
  be done in DCGs), thus allowing parsing to be interleaved with other
  system components. ALE was developed with an eye toward Head-Driven
  Phrase Structure Grammar (HPSG), but it can also execute PATR-II
  grammars, definite clause grammars (DCGs), Prolog, Prolog-II, and
  LOGIN programs, etc. With suitable coding, it can also execute
  several aspects of Lexical-Functional Grammar (LFG).

").

help_info(application,alvey,"Alvey NL Tools",

"Definite-clause version of the grammar of the Alvey NL Tools, and a
fragment of the lexicon. Thanks to John Carroll for making the grammar
and test-set available. The accompanying README file states:

NOTICE: these files were supplied by John Carroll,
johnca@cogs.susx.ac.uk, and are derived from a version of the ALVEY
Natural Language Tools. Current information about these ALVEY NL Tools
is available at http://www.cl.cam.ac.uk/Research/NL/anlt.html

").

help_info(application,cfg,"CFG","

Tiny context-free grammar. Illustration what you need to do minimally
to adapt a grammar / parser to Hdrug.
").

help_info(application,cbcg,"Constraint-based Categorial Grammar",

"Constraint-based Categorial Grammar for Dutch written
by G.Bouma, slightly adopted by G. van Noord for Hdrug.").

help_info(application,dcg,"Definite Clause Grammar",

"Tiny DCG. Illustration what you need to do minimally to adapt a
grammar / parser to Hdrug.
").

help_info(application,chat,"Chat-80",

"The classic Chat-80 system by Fernando Pereira and David Warren").

help_info(application,tag,"Tree Adjoining Grammar",

"Small Tree Adjoining Grammar with nine (related) head-corner parsing
algorithms for *headed* Lexicalized and Feature-based TAG's.

").

help_info(application,genbook,"Semantic-head-driven Generation and
Head-corner Parsing",

"DCG for Dutch, originally used as illustration for
semantic-head-driven generation.
Furthermore, some of the parsers were used for the timings of
the paper co-authored with G. Bouma on the potential efficiency of
head-driven parsing.").

help_info(application,extraposition,"Extraposition Grammar",

"Extraposition grammars as described in Pereira's CL paper.").

help_info(application,lexrules,"Delayed Evaluation of Lexical Rules",

"HPSG grammar for Dutch using delayed evaluation techniques
to implement recursive lexical rules.").

help_info(application,sdcg,"Stochastic Definite Clause Grammar",

"Experimental Stochastic Definite Clause Grammar by Robert Malouf.").

help_info(application,shpsg,"Stochastic Head-driven Phrase Structure Grammar",

"Experimental Stochastic Head-driven Phrase Structure Grammar by
Robert Malouf. This material was used by Rob's ESSLLI course (with
Miles Osborne) in Helsinki 2001.").



help_info(class,gui,"Graphical User Interface",

"
The Hdrug widget
(http://www.let.rug.nl/~vannoord/Hdrug/Manual/ale1.png)
contains the following sub-widgets (from top-to-bottom).

* MenuBar. The MenuBar contains a number of menubuttons. Each of
these menu's is described below.

* ObjectBar. The ObjectBar is initially invisible. Once `objects' are
produced by a parser or generator these objects will be placed on this
bar.

* Two scrollable canvases. Each canvas is initially left blank but is
used for graphical output such as parse-trees, etc. You can scroll the
canvas using the scrollbars, but also using the middle mouse
button. The relative size of the canvases can be adapted by dragging
the border with the left mouse button, or with the file-enlarge left
canvas resp. enlarge right canvas commands.

* TkConsol. If the global variable tkconsol is on, then the
TkConsol widget treats standard input, standard output and error
output.

* ButtonBar. The ButtonBar contains three buttons that can be used to
change the value of the top category, the parser and the
generator. The current values of these are also listed next to the
corresponding buttons. The checkbox `new canvas' can be switched on in
order for the next graphical output to be directed to a seperate
window. Finally the rightmost button is a button that is added by manu
applications, and which functions as an `About' button.
").

help_info(gui,menubar,"The MenuBar",
"
* The File Menu. The file menu contains buttons of which the meaning is
rather straightforward. The first items can be used to (re)compile
grammar files. The detailed meaning is dependent on the application.

The `compile prolog file' button lets you choose a Prolog file
which will be compiled. The `reconsult prolog file' button
behaves similarly but reconsults the file. The `source tk/tcl file'
button can be used to source a Tcl/Tk file. The `halt' button
halts the application (really!).

The `enlarge left canvas' and `enlarge right canvas' buttons
adapt the relative size of the two canvases.

It is also possible to restart the application. This implies that the
graphical environment is restarted, but the application files are not
reconsulted. This is useful if you are adding/debugging parts of the
graphical interface. Use `restart x' to restart.  `quit x -
keep prolog alive' allows you to stop the interface, but continue
the Prolog session.

* The Debug Menu. The debug menu contains a few buttons that are
straightforward interfaces to the corresponding Prolog predicates. It
contains the options

        * nodebug
        * debug
        * remove spypoints
        * spy predicate
        * unspy predicate
        * statistics

* The Options Menu. The options menu provides an interface to the
global variables.

* The Parse Menu. The parse menu is a more interesting menu, although it
consists of only a single button.  Pressing this button will present a
dialog widget asking you for a sentence. You can either type in a new
sentence, or select one of the available test sentences. The available
sentences are the sentences that were previously parsed during the
current session, or that were listed in the test-suite, or that were
the result of generation in the current session. The sentence you type
or select will be parsed using the current parser, and the current top
category. Some statistical output will be presented on standard
output. For each parse result a numbered object is created. Each
object is visible as a button on the ObjectBar, allowing you to
inspect each object.  The maximum number of objects that is built is
limited by the max_objects flag.

The option `compare parsers' allows you to parse a single test
sentence for each of the active parsers.

Use the option `parser selection' to activate or de-active a
particular parser.

* The Generate Menu

generate lf. This option is the inverse of the parse option. Now you
are prompted for a logical form, which is subsequently input to the
current generator. Again, you can either specify a logical form `by
hand' or select a pre-existing logical form. Note that there is (yet)
no concept of a test-suite for logical forms.

generate object. This option takes the logical form from the object
that you select, and generates from this logical form. Note that other
information ofthis object is not taken into account in the generation
process.

generator selection. Use this option to activate or de-activate a
particular generator.

compare generators on lf. Generate from a given logical form for each
of the active generators.

compare generators on object. Generate from the logical form of a
given object for each of the active generators.


* The Test-suite Menu

The test-suite menu contains a number of options in order to test the
application for a (pre-defined) set of sentences. This set of
sentences is defined in a file called {suite.pl} in the application
directory, and consists of a number of Prolog clauses for the
predicate sentence/2, where the first argument is a unique identifier
of that sentence, and the second argument is a list of atoms.  Note
that there is (yet) no concept of a test-suite for logical forms.

The following options are provided.

run test-suite. If this option is chosen then all sentences are parsed
in turn, for each of the parsers that are `active'}. Use the `parser
selection' option to select the parsers you want to include/exclude
for the run.

run test-suite and view. This option behaves similar to the previous
option, but in addition statistical information comparing the
different parsers is shown in a separate TK widget. The statistical
information is updated after a sentence has been parsed by all active
parsers.

reload test-suite. Choosing this option reloads the test-suite.
Note that it does *not* destroy existing test-results.

view test-results. This option contains a number of sub-options that
allow you to view the test-results in various ways.

individual tk. Presents a graph in which the length of the
sentence is plotted against the parse time, for each of the different
parsers and sentences.

totals per #words tk. Similarly, but now averages per sentence length
are used.

totals per #words latex. This produces an Xdvi window containing a
table of the parse results, again averaged over sentence-length. Note
that all :atex files are placed in a temporary directory, given by the
environment variable $TMPDIR. If this variable is not set, the
directory /tmp is used.  The predicate latex:files/5 can be used to
get the actual file names that are used.

totals per #readings latex. This produces a table of the parse
results, averaged over the number of readings.

individual prolog. This simply gives a Prolog listing of the
table_entry/6 predicate.

totals per #words prolog. Prolog output of the average cputimes per
parser per #words. This is given as a list of terms
t(Length,Time,Parser) with the obvious interpretation.

destroy test results. Removes the test-results, i.e. retracts all
clauses of the table_entry/6 predicate.


* The View Menu. The `view menu' contains several sub-menus indicating
the type of things you can view. Typically, you can view `objects'
(the result of parsing and generation) `predicates' (Prolog
predicates) and `types' (if your application uses
library(hdrug_feature). For each of the sub-menus you can choose
between different output widgets: Tk, CLiG, Prolog, DOT and LaTeX.
Finally, for each combination of `thing' and `output widget' you can
choose between a number of different output filters. The choice of
filter determines whether the output is printed as a tree, a feature
structure or the internal Prolog encoding.

Note that not all view options will produce results. Sometimes these
options are only defined for particular inputs. For example, in the
Tree Adjoining Grammar application there are parsers that build
derivation trees, and there are parsers that build derived trees.
Hdrug is not always able to tell in advance whether e.g. the
Tree(dt) filter (for derivation trees) is defined for a particular
object.

* The Help Menu. The help menu contains an interface to the various
on-line help resources.  The About button produces a rather ugly
picture of the author of Hdrug. The Version button produces some
information concerning the version of SICStus, Hdrug and application.
Finally some applications add an extra `About the Grammar' option that
mentions the authors of the application.
").

help_info(gui,objectbar,"The ObjectBar",

"* ObjectBar. The results of parsing and generation are asserted in the
database as `objects'. The existence of objects is indicated in the
ObjectBar. For example, if we have parsed the sentence `jan kust
marie' in an application, then the ObjectBar contains two buttons,
labeled `1' and `2', indicating the first and second parse
result. Pressing the button gives rise to a submenu containing three
options. The first option allows inspection of the object. The
possibilities are essentially those described above under the `view
menu'. The second and third option allow the generation from the
logical form representation of the object (if the application in fact
defines a generator), either for the current generator, or for all
active generators.
").

help_info(gui,buttonbar,"The ButtonBar",

"* ButtonBar. The ButtonBar contains maximally three buttons that can be
used to change the value of the top category, the parser and the
generator. The current values of these are also listed next to the
corresponding buttons. Note that there is only a `generator' button if
there are generators defined; similarly the `parser' button might not
exist in some applications.

").

:- if(current_prolog_flag(language,sicstus)).
:- multifile portray_message/2.
portray_message(error,hdrug_error(Term,Args)):-
    !,
    format(user_error,"** HDRUG error: ",[]),
    format(user_error,Term,Args).
:- else.

:- multifile message_hook/3.
message_hook(hdrug_error(Term,Args),error,_) :-
    !,
    format(user_error,"** HDRUG error: ",[]),
    format(user_error,Term,Args).

:- endif.


:- public list_sentences/0, list_sentences/1.

list_sentences :-
    findall(Key,a_sentence(Key,_,_),Keys),
    list_sentences(Keys).

list_sentences([]).
list_sentences([H|T]) :-
    list_sentence(H),
    list_sentences(T).

list_sentence(Key0) :-
    a_sentence_with_map(Key0,Key,Sentence),
    format(user_error,"~w~n",[Key]),
    try_hook(display_extern_phon(Sentence),
	     format(user_error,"~w~n",[Sentence])),
    hdrug_util:set_flag(current_ref,Key).

%%
maybe_trimcore :-
    hdrug_flag(allow_trimcore,Val),
    (   Val == on
    ->  trimcore,
	format(user_error,"trimcore/0 done~n",[])
    ;   true
    ).

:- public version_command/0.
version_command :-
    system_type(DR),
    version_command(DR).

version_command(development) :-
    version,
    try_hook(application_version).
version_command(runtime) :-
    (   hdrug_flag(version,L),
	format("~w~n",[L]),
	fail
    ;   true
    ),
    try_hook(application_version).
   

:- multifile
    graphic_path/3,
    graphic_label/3,
    graphic_daughter/4.
:- discontiguous
    graphic_path/3,
    graphic_label/3,
    graphic_daughter/4.

%%% for hdrug_feature
graphic_path(type,T,T).

graphic_label(type,T,Label):-
    hdrug_feature:define_type(T,_,[H|Tail],_,_),
    hdrug_util:concat_all([T,':',H|Tail],Label,' ').
graphic_label(type,T,T):-
    hdrug_feature:define_type(T,_,[],_,_).
graphic_label(type,[_H|_T],and).

graphic_daughter(type,I,T,Val):-
    hdrug_feature:define_type(T,[V],_,_,_),
    !,
    lists:nth(I,V,Val).
graphic_daughter(type,I,T,V):-
    hdrug_feature:define_type(T,Subs,_,_,_),
    lists:nth(I,Subs,V).
graphic_daughter(type,I,[H|T],V):-
    lists:nth(I,[H|T],V).

%%% for hdrug_call_tree
graphic_path(t,X,X).
graphic_daughter(t,No,tree(_,_,Ds),D) :-
    nth(No,Ds,D).
graphic_label(t,tree(L,_,_),L).

%%% LINKING PREDICATES for HOOKS -- HOOK FORWARDING
%%% this techniques is used to allow the current file to be loaded into
%%% modules that we currently don't know about, rather than (just) user.
%%% so, if the current file is loaded into user module, we expect that
%%% the hooks are defined in user, but if the current file is loaded into
%%% e.g. the alpino module, then the hooks are defined in module alpino.

%%% this also implies that any hdrug builtin definitions for these hooks
%%% should also be placed in the same module as current file.

%%% most hooks are called internally in module hdrug. The predicates here
%%% forward these hooks to the module the application is running in.

%%% this one is special because hdrug_cmdint itself defines many
%%% hdrug_command/3 definitions already.

hdrug:hdrug_user_command(A,B,C) :-
    hook(hdrug_command(A,B,C)).
hdrug:hdrug_user_command_help(A,B,C) :-
    hook(hdrug_command_help(A,B,C)).

hdrug:change_tree(T0,T) :-
    try_hook(change_tree(T0,T),T0=T).

%% hdrug_call_tree

hdrug:call_clause(Head,Body) :-
    hook(call_clause(Head,Body)).
hdrug:call_leaf(A,B) :-
    hook(call_leaf(A,B)).
hdrug:call_default(Leaf) :-
    hook(call_default(Leaf)).
hdrug:call_build_lab(A,B,C):-
    hook(call_build_lab(A,B,C),C=A+B).
hdrug:call_ignore_clause(A) :-
    hook(call_ignore_clause(A)).

%% hdrug_chart

:- public
    hdrug:pp_chart_item/1,
    hdrug:pp_chart_item2/1,
    hdrug:pp_chart_item3/1,
    hdrug:pp_chart_item_b/1,
    hdrug:pp_chart_item_b2/1,
    hdrug:pp_chart_item_b3/1.

hdrug:pp_chart_show_node_help(Msg) :-
    hook(pp_chart_show_node_help(Msg)).
hdrug:pp_chart_item(X) :-
    hook(pp_chart_item(X)).
hdrug:pp_chart_item2(X) :-
    hook(pp_chart_item2(X)).
hdrug:pp_chart_item3(X) :-
    hook(pp_chart_item3(X)).
hdrug:pp_chart_item_b(X) :-
    hook(pp_chart_item_b(X)).
hdrug:pp_chart_item_b2(X) :-
    hook(pp_chart_item_b2(X)).
hdrug:pp_chart_item_b3(X) :-
    hook(pp_chart_item_b3(X)).

%% hdrug_tk

hdrug:tk_portray(Term,Widget) :-
    hook(tk_portray(Term,Widget)).


%% hdrug_dot
hdrug:dot_tree_user_node(Node,C1,C2) :-
    try_hook(dot_tree_user_node(Node,C1,C2),
	     C1=[Node|C2]
	    ).

%% hdrug_latex

hdrug:latex_tree_user_node(L) :-
    try_hook(latex_tree_user_node(L),
	     hdrug_latex:tree_label(L)
	    ).

%% hdrug_clig

hdrug:clig_tree_user_node(Node,C0,C,Type):-
    try_hook(clig_tree_user_node(Node,C0,C),
	     hdrug_clig:pp_node(Node,Type,C0,C)
	    ).

:- public
    hdrug:show_node/2,
    hdrug:show_node2/2,
    hdrug:show_node3/2,
    hdrug:show_node/3,
    hdrug:show_node2/3,
    hdrug:show_node3/3.

hdrug:show_node(Name,Term) :-
    hook(show_node(Name,Term,tk)).

hdrug:show_node2(Name,Term) :-
    hook(show_node2(Name,Term,tk)).

hdrug:show_node3(Name,Term) :-
    hook(show_node3(Name,Term,tk)).


%% hdrug_tk

hdrug:tk_tree_user_node(Label,Wnode,Name) :-
    try_hook(tk_tree_user_node(Label,Wnode),
	     hdrug_tk:tk_tree_rest_node(Label/Label,Wnode,Name)).

hdrug:tk_tree_show_node_help(Name,Message):-
    hook(tk_tree_show_node_help(Name,Message)).

hdrug:show_node(Name,Term,Medium) :-
    hook(show_node(Name,Term,Medium)).

hdrug:show_node2(Name,Term,Medium) :-
    hook(show_node2(Name,Term,Medium)).

hdrug:show_node3(Name,Term,Medium) :-
    hook(show_node3(Name,Term,Medium)).

%% hdrug_gui

hdrug:gram_startup_hook_begin :-
    try_hook(gram_startup_hook_begin).

hdrug:gram_startup_hook_end :-
    try_hook(gram_startup_hook_end).

hdrug:use_canvas(Mode,Val) :-
    try_hook(use_canvas(Mode,Val),
	     Val=system).

%% useful predicates in this module
%% and hooks that are called from various places.

hdrug:graphic_path(Name,Term0,Term) :-
    hook(graphic_path(Name,Term0,Term)).

hdrug:graphic_daughter(Name,Pos,Term,Daughter) :-
    hook(graphic_daughter(Name,Pos,Term,Daughter)).

hdrug:graphic_label(Name,Term,Label) :-
    hook(graphic_label(Name,Term,Label)).

hdrug:a_sentence(A,B) :-
    a_sentence(A,B).

hdrug:a_sentence(A,B,C) :-
    a_sentence(A,B,C).

hdrug:a_lf(A,B) :-
    a_lf(A,B).

:- public
    hdrug:display_extern_phon/1,
    hdrug:display_extern_sem/1.

hdrug:display_extern_phon(E) :-
    hook(display_extern_phon(E)).

hdrug:display_extern_sem(E) :-
    hook(display_extern_sem(E)).

hdrug:extern_phon(E,F) :-
    try_hook(extern_phon(E,F),E=F).

hdrug:extern_sem(E,F) :-
    try_hook(extern_sem(E,F),E=F).

:- public
    hdrug:check_length/1.

hdrug:check_length(E) :-
    check_length(E).

hdrug:shorten_label(A,B) :-
    try_hook(shorten_label(A,B), A=B).

hdrug:parser_comparison(N) :-
    parser_comparison(N).

hdrug:find_current_no(N) :-
    find_current_no(N).

hdrug:catch_print_error(A,B,C) :-
    hook(catch_print_error(A,B,C)).

hdrug:available:-
    available.

hdrug:top(A,B) :-
    hook(top(A,B)).


:- public
    hdrug:show_relation/1,
    hdrug:show_relation/2.

hdrug:show_relation(A) :-
    try_hook(show_relation(A,tk)).

hdrug:show_relation(A,B) :-
    try_hook(show_relation(A,B)).

%% hdrug.tcl
:- public
    hdrug:show_object_default2/1,
    hdrug:show_object_default3/1,
    hdrug:create_object_hook/2.
    
hdrug:show_object_default2(No) :-
    try_hook(show_object_default2(No)).

hdrug:show_object_default3(No) :-
    try_hook(show_object_default3(No)).

hdrug:create_object_hook(A,B) :-
    try_hook(create_object_hook(A,B)).

%%%%%%%%%%%%%%%% end of hook forwarding %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% hdrug_util
%% backward compatibility, but only if predicates do not yet exist

:- if((\+current_predicate(flag/1),
      \+current_predicate(flag/2),
      \+current_predicate(flag/3)
      )).

flag(X) :- hdrug_flag(X).
flag(X,Y) :- hdrug_flag(X,Y).
flag(X,Y,Z) :- hdrug_flag(X,Y,Z).

:- endif.

%% dynamic predicates in hdrug module

table_entry(A,B,C,D,E,F) :-
    hdrug:table_entry(A,B,C,D,E,F).

dyn_sentence(A,B) :-
    hdrug:dyn_sentence(A,B).

dyn_sentence(A,B,C) :-
    hdrug:dyn_sentence(A,B,C).

dyn_lf(A,B) :-
    hdrug:dyn_lf(A,B).

dyn_lf(A,B,C) :-
    hdrug:dyn_lf(A,B,C).


:- public try_to_set_current_ref/1.
try_to_set_current_ref(List) :-
    (	hdrug_flag(current_ref,Ref),
	a_sentence(Ref,List)
    ->	if_gui(listbox_see_sentence(Ref)),
	if_gui(tcl('set pp_nr {~w}',[Ref]))
    %   confusing if ref is given explicitly,
    %   so we do this only if it is the only ref
    %   which has this sentence
    ;	findall(Ref,a_sentence(Ref,List),[Ref])
    ->	set_flag(current_ref,Ref),
	if_gui(tcl('set pp_nr {~w}',[Ref])),
	if_gui(listbox_see_sentence(Ref))
    ;	true
    ).

listbox_see_sentence(Ref) :-
    nth_sentence(Ref,N),
    tcl('if [winfo exists .pp.frame.list] then { .pp.frame.list see ~w}',[N]).

nth_sentence(Ref,N) :-
    findall(K,a_sentence(K,_),Ks),
    nth(N,Ks,Ref).

