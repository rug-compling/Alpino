:- module(alpino_treebank, [ xml_save_object/1,
			     xml_save_object/2,
			     xml_save_object_adt/2,
			     xml_save_and_edit_object/1,
			     xml_save_and_edit_object/2,
			     xml_save/5,
			     treebank/0,
			     treebank/1,
			     dtedit/0,
			     dtedit/1,
			     xml_filename/1,
			     xml_filename/2,
			     adt_xml_filename/2,
			     treebank_directory/1,
			     file_to_xml_terms/2,
			     xml_from_treebank_server/2,
			     alpino_ds_file_to_xml_terms/2,
			     xml_file_to_dt/2,
			     xml_file_to_dt/4,
			     xml_file_to_adt/2,
			     xml_save_adt/2,
			     xml_file_to_xml_file/2,
			     treebank_mwu_roots_list/1,
			     treebank_dep_features_dt/1,
			     treebank_dep_features_stdin/0,
%			     preceding_treebank_file/2,
%			     following_treebank_file/2,
			     converse_xml_files/1,
			     canonical_xml_overwrite_files/1,
			     canonical_xml_file/1,
			     empty_xml_files/1,
 			     compare_deprels/7,
			     evaluate_result/3,
			     format_word_list_xml/1,
			     format_word_list_xml/3,
			     tree_comparison/0,
			     tree_comparison/1,
			     tree_comparison_find_best/0,
			     tree_comparison_all/0,
			     score_string_of_result/2,
			     best_score_new_parse/0,
			     best_score_new_parse/1,
			     best_score_end_parse/0,
			     best_score_end_parse/1,
			     best_score_end_parse_sub/1,
			     best_score_result_parse/3,
			     format_dep_features/2,
			     format_score_with_penalties/1,
			     format_penalties/1,
			     tree_comparison_pair/2
			   ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(utils).
:- use_module(hdrug(hdrug_util)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(system)).

:- public
    treebank/0,
    treebank/1,
    dtedit/0,
    dtedit/1,
    xml_file_to_xml_file/2,
    converse_xml_files/1,
    canonical_xml_file/1,
    canonical_xml_overwrite_files/1,
    empty_xml_files/1,
    treebank_triples/0,
    treebank_triples_list/1,
    treebank_dep_features_stdin/0,
    treebank_dep_features_list/1,
    treebank_dep_features_with_file_list/1,
    treebank_dep_features_dt/1,
    treebank_full_triples_list/1,
    treebank_lassy_triples_list/1,
    treebank_pl_triples_list/1,
    show_treebank/3.

string_to_words(String,Words) :-
    split_string(String," ",Strings),
    atom_codes_list(Strings,Words).

atom_codes_list([],[]).
atom_codes_list([H|T],[NH|NT]) :-
    atom_codes(NH,H),
    atom_codes_list(T,NT).

treebank_triples_list([]).
treebank_triples_list([H|T]) :-
    treebank_triples(H),
    treebank_triples_list(T).

treebank_dep_features_list([]).
treebank_dep_features_list([H|T]) :-
    treebank_dep_features(H),!,
    treebank_dep_features_list(T).

treebank_dep_features_stdin :-
    repeat,
    read_line(Codes),
    (   Codes == end_of_file
    ->  !
    ;   atom_codes(File,Codes),
	(   on_exception(Exc,
			 treebank_dep_features(File),
			 (  format(user_error,"exception: ~w~n",[Exc])
			 ,  fail
			 )
			)
	->  true
	;   format(user_error,"treebank_dep_features failed on ~w~n",[File])
	),
	fail
    ).


treebank_dep_features_with_file_list([]).
treebank_dep_features_with_file_list([H|T]) :-
    treebank_dep_features_with_file(H),!,
    treebank_dep_features_with_file_list(T).

treebank_mwu_roots_list([]).
treebank_mwu_roots_list([H|T]) :-
    treebank_mwu_roots(H),
    treebank_mwu_roots_list(T).

treebank_mwu_roots(File) :-
    xml_file_to_dt(File,DT0),
    alpino_dt:undo_mwp(DT0,DT),
    alpino_dt:dt_to_relations(DT,Rels),
    rels_to_roots(Rels,Roots),
    format_roots(Roots).

format_roots(Roots) :-
    roots_to_string(Roots,String),
    format("~s\n",[String]).

roots_to_string([],[]).
roots_to_string([_-H|T],S) :-
    charsio:format_to_chars("~q",[H],S1),
    escape_space(S1,S2),
    lists:append(S2,ST,S),
    roots_to_string2(T,ST).

roots_to_string2([],[]).
roots_to_string2([_-H|T],S) :-
    charsio:format_to_chars("~q",[H],S1),
    escape_space(S1,S2),
    lists:append([32|S2],ST,S),
    roots_to_string2(T,ST).

escape_space([],[]).
escape_space([H|T],Result) :-
    (   H =:= 32
    ->  Result = [95,95,95|ResT]
    ;   Result = [H|ResT]
    ),
    escape_space(T,ResT).

rels_to_roots(Rels,Roots) :-
    findall(Root,rels_to_root(Rels,Root),Roots0),
    sort(Roots0,Roots).  % remove duplicates and sort position

rels_to_root(Rels,Root) :-
    member(Rel,Rels),
    rel_to_root(Rel,Root).

rel_to_root(deprel(_:Root/[Pos,_],_,_),Pos-Root).
rel_to_root(deprel(_,_,_:Root/[Pos,_]),Pos-Root).

treebank_full_triples_list([]).
treebank_full_triples_list([H|T]) :-
    treebank_full_triples(H),
    treebank_full_triples_list(T).

treebank_lassy_triples_list([]).
treebank_lassy_triples_list([H|T]) :-
    treebank_lassy_triples(H),
    treebank_lassy_triples_list(T).

treebank_triples :-
    hdrug_flag(current_ref,Item0),
    current_ref_treebank(Item0,Item),
    treebank_directory(GoldDir),
    format_to_chars("~w/~w.xml",[GoldDir,Item],CharsFile),
    atom_codes(CorrectFile,CharsFile),
    treebank_triples(CorrectFile).

treebank_triples(File) :-
    xml_file_to_dt(File,DT),
    alpino_dt:dt_to_relations(DT,Rels),
    alpino_dt:format_relations(Rels,File).

treebank_dep_features(File) :-
    (   treebank_dep_features_(File)
    ->  true
    ;   format(user_error,"treebank_dep_features failed on ~w~n",[File])
    ).

treebank_dep_features_with_file(File) :-
    (   treebank_dep_features_with_file_(File)
    ->  true
    ;   format(user_error,"treebank_dep_features failed on ~w~n",[File])
    ).

treebank_dep_features_(File) :-
    xml_file_to_dt(File,DT0),
    treebank_dep_features_dt(DT0).

treebank_dep_features_dt(DT0):-
    alpino_dt:undo_mwp(DT0,DT),
    alpino_dt:dt_to_relations_with_somewhat_simplified_postags(DT,Rels),
    alpino_penalties:triples_to_features_which_have_freq_feature(Rels,Features),
    format_features(Features).
%    alpino_penalties:marginals(Features,Marginals),
%    format_features(Marginals).

treebank_dep_features_with_file_(File) :-
    xml_file_to_dt(File,DT0),
    alpino_dt:undo_mwp(DT0,DT),
    alpino_dt:dt_to_relations_with_somewhat_simplified_postags(DT,Rels),
    alpino_penalties:triples_to_features_which_have_freq_feature(Rels,Features),
    format_features_with_file(Features,File).
%    alpino_penalties:marginals(Features,Marginals),
%    format_features_with_file(Marginals,File).

format_features([]).
format_features([H|T]) :-
    format_feature(H),
    format_features(T).

format_feature(Feature) :-
    format("~q~n",[Feature]).

format_features_with_file([],_).
format_features_with_file([H|T],File) :-
    format_feature_with_file(H,File),
    format_features_with_file(T,File).

format_feature_with_file(Feature,File) :-
    format("~q\t~w~n",[Feature,File]).

treebank_full_triples(File) :-
    xml_file_to_dt(File,DT),
    alpino_dt:dt_to_relations_with_full_postags(DT,Rels),
    alpino_dt:format_relations(Rels,File).

treebank_lassy_triples(File) :-
    xml_file_to_dt(File,DT),
    alpino_dt:dt_to_relations_with_full_postags(DT,Rels),!,
    lassy_relations(Rels,Rels1),
    alpino_dt:format_relations(Rels1,File).

lassy_relations([],[]).
lassy_relations([H0|T0],[H|T]) :-
    (    lassy_relation(H0,H)
    ->   true
    ;    format(user_error,"lassy_relation failed on ~n~w~n",[H0]),
	 fail
    ),
    lassy_relations(T0,T).

lassy_relation(deprel(Dep,Rel,Head),deprel(DepLem,DepPos,Rel,HeadPos,HeadLem)) :-
    lassy_lem_pos(Dep,DepLem,DepPos),
    lassy_lem_pos(Head,HeadLem,HeadPos).

lassy_lem_pos(none:none/[0,0],none,none).
lassy_lem_pos(top:top/top,top,top).
lassy_lem_pos(read_from_treebank(_,Lem,Pos):_/_,Lem,Pos).

treebank_pl_triples_list([]).
treebank_pl_triples_list([H|T]) :-
    treebank_pl_triples(H),
    treebank_pl_triples_list(T).

treebank_pl_triples(File) :-
    xml_file_to_dt(File,DT),
    alpino_dt:dt_to_relations_with_full_postags(DT,Rels),
    format("~q.~n",[triples(File,Rels)]).

show_treebank(Type,Output,File) :-
    xml_file_to_dt(File,DT),
    hdrug_show:show(Type,Output,[value(already_canonical_dt(DT))]).

%% start dtview with the `correct' treebank cgn deptree
treebank :-
    hdrug_flag(current_ref,Ref),
    treebank(Ref).

treebank(Key) :-
    treebank_directory(Dir),
    format_to_chars("~w ~w/~w.xml",[dtview,Dir,Key],CharsCmd),
    atom_codes(Cmd,CharsCmd),
    exec(Cmd,[std,std,std],_).

treebank_directory(Dir) :-
    hdrug_flag(treebank,Dir0),
    (	Dir0==undefined
    ->  hdrug_flag(suite,Suite0),
	absolute_file_name(Suite0,Suite1),
	subtract_pl(Suite1,Suite),
	(   atom_concat(Path,SuitesFile,Suite),
	    atom_concat('/Suites/',File,SuitesFile),
	    atom_concat(Path,'/Treebank/',PathTreebank),
	    atom_concat(PathTreebank,File,Dir)
	->  true
        ;   atom_concat('Suites/',File,Suite),
	    atom_concat('Treebank/',File,Dir)
	),
	set_flag(treebank,Dir)
    ;	Dir0=Dir
    ).

subtract_pl(Suite0,Suite) :-
    (   atom_concat(Suite,'.pl',Suite0)
    ->  true
    ;   Suite0=Suite
    ).

dtedit :-
    hdrug_flag(current_ref,Ref),
    dtedit(Ref).

dtedit(Key) :-
    dtedit(dttred,Key).

%% tred without special intermediate script:
dtedit(dttred,Key) :-
    !,
    treebank_directory(Dir),
    format_to_chars("~w ~w/~w.xml",['Tred',Dir,Key],CharsCmd),
    atom_codes(Cmd,CharsCmd),
    exec(Cmd,[std,std,std],_).

set_current_ref_to_object_ref :-
    hdrug_flag(current_ref_of_object,Value),
    set_current_ref_to_object_ref(Value).

set_current_ref_to_object_ref(undefined) :-
    !.
set_current_ref_to_object_ref(Value) :-
    set_flag(current_ref,Value).


:- public
    xml_save_and_edit_object/1,
    xml_save_and_edit_object/2,
    xml_save_object/1,
    xml_save_object/2.

xml_save_object(Obj) :-
    set_current_ref_to_object_ref,
    xml_filename(File),
    (	file_exists(File)
    ->	(   user_confirmation(
			"File ~w exists. Do you want to overwrite? ",[File])
	->  xml_save_object(Obj,File)
	;   true
	)
    ;	xml_save_object(Obj,File)
    ).

xml_save_object(Obj,File) :-
    hdrug:object(Obj,o(Result,String0,_)),
    alpino_lexical_analysis:remove_brackets(String0,String1),
    remove_phantoms(String1,String2), % sometimes ok, sometimes not
    %% necc for xml_save of "mensen [ @phantom , ] kom naar buiten"
    xml_save(Result,String2,[],File,normal).

remove_phantoms(String0,String) :-
    get_phantoms(Ps),
    remove_phantoms(String0,String,Ps,0).

remove_phantoms([],[],_,_).
remove_phantoms([W|Ws],Result,Ps,P0) :-
    (   member(P0,Ps)
    ->  Result=Rest
    ;   Result=[W|Rest]
    ),
    P1 is P0+1,
    remove_phantoms(Ws,Rest,Ps,P1).

get_phantoms(Ps) :-
    try_hook(alpino_lexical_analysis:user_skips(S),S=[]),
    findall(P,member(phantom_skip(P),S),Ps).

xml_save_and_edit_object(Obj) :-
    xml_save_object(Obj),
    dtedit.

xml_save_and_edit_object(Obj,File) :-
    xml_save_object(Obj,File),
    dtedit.

xml_filename(File,Identifier0) :-
    current_ref_treebank(Identifier0,Identifier),
    treebank_directory(Dir),
    format_to_chars("~w/~w.xml",[Dir,Identifier],Codes),
    atom_codes(File,Codes).

adt_xml_filename(File,Identifier0) :-
    current_ref_treebank(Identifier0,Identifier),
    treebank_directory(Dir),
    format_to_chars("~w/~w-adt.xml",[Dir,Identifier],Codes),
    atom_codes(File,Codes).

sentence_with_key(Base) :-
    alpino_suite:sentence(Base,_),!.
sentence_with_key(Atom) :-
    atom_codes(Atom,Codes),
    number_codes_silent(Number,Codes),
    alpino_suite:sentence(Number,_).

%number_codes_silent(Number,Codes) :-
%    prolog:'$number_elems'(Number, Codes, character_code, true).

number_codes_silent(Number,Codes) :-
    catch(number_codes(Number,Codes),_,fail).

xml_filename(File) :-
    hdrug_flag(current_ref,Key),
    (   Key == undefined
    ->  raise_exception(hdrug_error(
               "key not defined - don't know how to get XML filename~n",[]))
    ;   true
    ),
    xml_filename(File,Key).

xml_save(Result,String,Comments,File,Flag):-
    xml_save(Result,String,Comments,[],File,Flag).

xml_save(Result,String,Comments,Meta,File,Flag) :-
    (   % remove_phantoms(String0,String),
	xml_save__(Result,String,Comments,Meta,File,Flag)
    ->  true
    ;   format(user_error,"ERROR: something went wrong in saving the XML in ~w!~n",[File])
    ).

xml_save__(already_dt(Result),String,Comments,Meta,File,Flag) :-
    !,
    deptree_xml(already_dt(Result),String,Comments,Meta,Flag,[],[],Chars,[]),
    stream_or_open_file(File,Stream),
    format(Stream,"~s",[Chars]),
    close(Stream),
    debug_message(1,"xml saved in ~w~n",[File]).
xml_save__(Result,String,Comments,Meta,File,Flag) :-
    alpino_format_syntax:result_to_frames(Result,Frames,_),
    alpino_format_syntax:frames_to_postags(Frames,Result,SysTags),
    frames_to_his(Frames,HisList,[]),
    deptree_xml(Result,String,Comments,Meta,Flag,SysTags,HisList,Chars,[]),
    stream_or_open_file(File,Stream),
    format(Stream,"~s",[Chars]),
    close(Stream),
    debug_message(1,"xml saved in ~w~n",[File]).

frames_to_his([]) --> [].
frames_to_his([Frame|Frames]) -->
    frame_to_his(Frame),
    frames_to_his(Frames).

frame_to_his(_Context-frame(_P0,_P,Q0,Q,_Stem,_Frame,_Surf,His) ) -->
    [his(Q0,Q,His)].



stream_or_open_file(stream(Stream0),Stream) :-
    !,
    Stream0=Stream.
stream_or_open_file(File,Stream) :-
    open(File,write,Stream).

add_xml_entities([],[]).
add_xml_entities([H|T],Chars) :-
    add_xml_entity(H,T,Chars,Chars0),
    add_xml_entities(T,Chars0).

add_xml_entity(C,T,Chars,Chars0) :-
    (	xml_entity(C,Enc,T)
    ->	append(Enc,Chars0,Chars)
    ;	Chars=[C|Chars0]
    ).

xml_entity(0'&,_,Chars) :-
    xml_entity_prefix(Chars,_,_),
    !,
    fail.
xml_entity(C,Enc,_) :-
    xml_entity(C,Enc).

% <
xml_entity(0'","&quot;").   % "
xml_entity(0'&,"&amp;").
xml_entity(0'<,"&lt;").
xml_entity(0'>,"&gt;").
xml_entity(0'',"&apos;").

xml_entity_prefix([38,113,117,111,116,59|T],T,0'").  % "
xml_entity_prefix([38,35,51,52,59|T],       T,0'").  % "
xml_entity_prefix([38,97,109,112,59|T],     T,0'&).
xml_entity_prefix([38,35,51,56,59|T],       T,0'&).
xml_entity_prefix([38,108,116,59|T],        T,0'<).
xml_entity_prefix([38,35,54,48,59|T],       T,0'<).
xml_entity_prefix([38,103,116,59|T],        T,0'>).
xml_entity_prefix([38,35,54,50,59|T],       T,0'>).
xml_entity_prefix([38,97,112,111,115,59|T], T,0'').
xml_entity_prefix([38,35,51,57,59|T],       T,0'').

format_word_list_xml(List) :-
    with_output_to_chars(format_word_list(List),Chars0),
    add_xml_entities(Chars0,Chars),
    format("~s",[Chars]).

format_word_list_xml(List) -->
    { with_output_to_chars(format_word_list(List),Chars0),
      add_xml_entities(Chars0,Chars)
    },
    format_to_chars("~s",[Chars]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CHECK_STRINGS IN TREEBANK %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public check_suite_and_treebank_are_consistent/0.

check_suite_and_treebank_are_consistent :-
    check_suite_and_treebank_are_consistent(_).

%% assumes the .sent files have been created
check_suite_and_treebank_are_consistent(Item) :-
    hdrug_flag(suite,Suite),
    format(user_error,"checking strings in treebank for suite ~w~n",[Suite]),
    (	alpino_suite:sentence(Item,Sent0),
        hdrug:extern_phon(Sent0,Sent1),
	with_output_to_chars(format_word_list(Sent1),Sent),
	(   xml_filename(File,Item),
            file_exists(File),
            xml_file_to_dt(File,DT,Words,_)
        ->  (	Words \== Sent
            ->	format(user_error,
                   "severe warning:~n~w contains wrong sentence!~n~s~n~s~n",
		       [File,Words,Sent])
            ;	check_valid_strings(DT)
            ->  true
            ;   format(user_error,
                   "severe warning:~n~w contains wrong leafs!~n~s~n~s~n",
		       [File,Words,Sent])
            )
        ;  format(user_error,"no file for sentence ~w ~s~n",[Item,Sent])
        ),
	fail
    ;   treebank_directory(Directory),
        catch(directory_files(Directory,Files),_,Files=[]),
        (   member(File,Files),
            (   atom_concat(Base,'.xml',File)
            ->  (   sentence_with_key(Base)
                ->  true
                ;   format(user_error,".xml file without sentence: ~w.xml~n",[Base])
                )
            ;   true
            ),
            fail
        ;   true
        )
    ).

check_valid_strings(DT) :-
    dt_to_words(DT,Words0,[]),
    keysort(Words0,Words),
    check_valid_words(Words,0).

check_valid_words([],_).
check_valid_words([P-Q|T],P) :-
    Q is P+1,
    check_valid_words(T,Q).

dt_to_words(tree(r(_,Node),_,Ds)) -->
    dt_node_to_words(Node),
    dt_ds_to_words(Ds).

dt_node_to_words(i(_)) --> [].
dt_node_to_words(i(_,Node)) -->
    dt_node_to_words(Node).
dt_node_to_words(p(_)) --> [].
dt_node_to_words(l(_,_,_/[P0,P])) -->
    [P0-P].

dt_ds_to_words([]) --> [].
dt_ds_to_words([H|T]) -->
    dt_to_words(H),
    dt_ds_to_words(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% DT --> CLEAN XML %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public deptree_xml/5.  % used in Joost?

deptree_xml(Cat,String,Comments) -->
    deptree_xml(Cat,String,Comments,[]).

deptree_xml(Cat,String,Comments,Meta) -->
    deptree_xml(Cat,String,Comments,Meta,normal,[],[]).

deptree_xml(Cat,String,Comments,Meta,Flag,Tags,HisList) -->
    { alpino_dt:result_to_dt(Cat,Flag,Tree0),
      deptree_xml_tree(Tree0,Tree,String,0,_,Tags,HisList)
    },
    deptree_xml_start,
    deptree_xml_meta(Meta),
    deptree_xml_score(Cat),
    deptree_xml_tree_format(Tree,2),
    deptree_xml_sentence(String),
    deptree_xml_comment_list(Comments),
    deptree_xml_end.


%% version 1.3: added mwu_root attribute for mwu nodes / removed again, not used
%% version 1.4: metadata
%% version 1.5: sentid
%% currently, sentid only in output
%% version 1.6: parser tag for number of categories and skips; attribute 'his' for words
%% currently, parser tag and his attribute only for output
%% version 1.7: dscsense dscmanual attributes
:- initialize_flag(alpino_ds_version,'1.6').
:- initialize_flag(xml_format_header,on).

deptree_xml_start -->
    {  hdrug_flag(alpino_ds_version,Version) },
    {  hdrug_flag(xml_format_header,Header)  },
    deptree_xml_start(Header,Version).

deptree_xml_start(off,Version) -->
    format_to_chars('<alpino_ds version="~w">~n',[Version]).
deptree_xml_start(on,Version) -->
    format_to_chars('<?xml version="1.0" encoding="UTF-8"?>~n<alpino_ds version="~w">~n',[Version]).

deptree_xml_end -->
    format_to_chars('</alpino_ds>~n',[]).

deptree_xml_comment_list([]) --> [].
deptree_xml_comment_list([H|T]) -->
    format_to_chars('  <comments>~n',[]),
    deptree_xml_comment_list_([H|T]),
    format_to_chars('  </comments>~n',[]).

deptree_xml_comment_list_([]) --> [].
deptree_xml_comment_list_([H|T]) -->
    deptree_xml_comment(H),
    deptree_xml_comment_list_(T).

deptree_xml_comment([]) --> [].
deptree_xml_comment([H|T]) -->
    { add_xml_entities([H|T],CommentString)
    },
    format_to_chars('    <comment>~s</comment>~n',[CommentString]).

deptree_xml_meta([]) --> [].
deptree_xml_meta([H|T]) -->
    format_to_chars('  <metadata>~n',[]),
    deptree_meta_list([H|T]),
    format_to_chars('  </metadata>~n',[]).

deptree_meta_list([]) --> [].
deptree_meta_list([H|T]) -->
    deptree_meta_el(H),
    deptree_meta_list(T).

deptree_meta_el(meta(Attributes)) -->
    format_to_chars('    <meta',[]),
    deptree_meta_atts(Attributes),
    format_to_chars('/>~n',[]).

deptree_meta_atts([]) --> [].
deptree_meta_atts([H|T]) -->
    deptree_meta_att(H),
    deptree_meta_atts(T).

deptree_meta_att(Att=String) -->
    format_to_chars(' ~w="~s"',[Att,String]).

deptree_xml_score(Cat) -->
    {
     hdrug_flag(xml_format_frame,on),
     alpino_data:result_term(_,_,_,tree(_,_,Trees,_),_,_,Cat), !,
     user:application_version(Build),
     system:datime(datime(Year,Month0,Day0,Hour0,Min0,_)),
     add_z(Month0,Month),
     add_z(Day0,Day),
     add_z(Hour0,Hour),
     add_z(Min0,Min),
     count_cats_and_skips(Trees,0,Cats,0,Skips)
    },
    format_to_chars('  <parser build="~s" date="~w-~s-~sT~s:~s" cats="~w" skips="~w" />~n',
		    [Build,Year,Month,Day,Hour,Min,Cats,Skips]).
deptree_xml_score(_Cat) --> [].

add_z(Num,Str) :-
    (   Num < 10
    ->  format_to_chars("0~w",Num,Str)
    ;   format_to_chars("~w",Num,Str)
    ).

deptree_xml_sentence(Sentence) -->
    format_to_chars('  <sentence',[]),
    format_sentid,
    format_to_chars('>',[]),
    format_word_list_xml(Sentence),
    format_to_chars('</sentence>~n',[]).

format_sentid -->
    {  hdrug_flag(current_ref,Ref),
       Ref \== undefined,
       hdrug_flag(xml_format_frame,on),
       format_to_chars("~w",[Ref],WordChars0),
       add_xml_entities(WordChars0,WordChars),
       !
    },
    format_to_chars(' sentid="~s"',[WordChars]).
format_sentid --> [].

deptree_xml_tree(tree(Label,Ds0),tree(Atts,_,Ds),String,No0,No,Tags,HisList) :-
    deptree_xml_label(Label,_,String,No0,Atts0,Tags,HisList),
    keysort(Atts0,Atts1),
    check_atts(Atts1,Atts),
    No1 is No0+1,
    deptree_xml_ds(Ds0,Ds,String,No1,No,Tags,HisList).

deptree_xml_tree(tree(Label,Key,Ds0),tree(Atts,_,Ds),String,No0,No,Tags,HisList) :-
    deptree_xml_label(Label,Key,String,No0,Atts0,Tags,HisList),
    keysort(Atts0,Atts1),
    check_atts(Atts1,Atts),
    No1 is No0+1,
    deptree_xml_ds(Ds0,Ds,String,No1,No,Tags,HisList).

deptree_xml_label(r(Rel,Label),Key,String,NodeId,[id-NodeId,rel-Rel|Atts],Tags,HisList) :-
    deptree_xml_label_rest(Label,String,Key,Atts,Tags,HisList).

deptree_xml_key(P1-List,[begin-P0,end-P],P0,P) :-
    nonvar(P1),
    !,
    List=[P0|_],
    P is P1+1.
deptree_xml_key(_,[],-1,-1). % ADT

deptree_xml_label_rest(i(I,Rest),String,Key,[index-I|Atts],Tags,HisList) :-
    deptree_xml_label_rest(Rest,String,Key,Atts,Tags,HisList).

deptree_xml_label_rest(i(I),_,Key,[index-I|Atts],_Tags,_His) :-
    deptree_xml_key(Key,Atts,_P0,_P).

deptree_xml_label_rest(p(C0),_,Key,[cat-C|Atts1],_Tags,HisList) :-
    extract_category_features(C0,C,Atts1,Atts2),
    deptree_xml_key(Key,Atts,P0,P),
    (   C == mwu,
	lists:member(his(P0,P,His),HisList)
    ->  encode_his(His,Atts2,Atts)
    ;   Atts2 = Atts
    ).

deptree_xml_label_rest(l(FrameTerm,Cat,Root0/[P0,P]),String,_,
                       [begin-P0,end-P,lemma-Lemma,root-Root,word-Word|Atts1],Tags,HisList) :-
    get_root(P0,P,String,Word),
    (   lists:member(cgn_postag(P0,P,Lemma0,Tag),Tags)
    ->  true
    ;   Tag = 'NA()'
    ),
    get_lemma(FrameTerm,Lemma0,Lemma,Word),
    get_root(Root0,Root),    
    hdrug_flag(xml_format_frame,On),
    (   On == on,
	lists:member(his(P0,P,His),HisList)
    ->  encode_his(His,Atts1,Atts)
    ;   Atts1 = Atts
    ),
    deptree_xml_label_rest_frame(On,Root,FrameTerm,Cat,Atts,P0,Word,Tag).

% ADT
deptree_xml_label_rest(adt_lex(Cat,Root,Sense,PosTag,LexAtts),_,_,
                       [root-Root,sense-Sense,pos-PosTag,cat-Cat|Atts],_,_) :-
    adt_lex_atts(LexAtts,Atts).

extract_category_features(mwu(_Root,_Sense),mwu,Atts0,Atts) :-
    !,
    Atts0=Atts.
extract_category_features(Cat,Cat,Atts,Atts).

adt_lex_atts([],[]).
adt_lex_atts([A=V|T],[A-V|NewT]) :-
    adt_lex_atts(T,NewT).

get_lemma(read_from_treebank(_,_,Lemma0,_),_,Lemma,_) :-
    !, Lemma0=Lemma.
get_lemma(read_from_treebank(_,Lemma0,_),_,Lemma,_) :-
    !, Lemma0=Lemma.
get_lemma(_,Lemma0,Lemma,Root) :-
    get_lemma_or_word(Lemma0,Lemma,Root).

get_lemma_or_word(Var,Lemma,Word) :-
    var(Var), !,
    debug_message(0,"error: variable lemma~n",[]),
    Lemma = Word.
get_lemma_or_word(Pos/Atom,Lemma,_Word) :-
    atom(Atom),
    integer(Pos),!,
    atom_codes(Atom,Codes),
    alpino_util:split_string(Codes," ",SubList),
    length(SubList,Len),
    (   Pos =:= 1,
	Len =:= 1
    ->  Lemma = Pos/Atom,
	debug_message(2,"warning: no matching lemma ~w~n",[Pos/Atom])
    ;   lists:nth(Pos,SubList,LemmaCodes)
    ->  atom_codes(Lemma,LemmaCodes)
    ;   Lemma=Pos/Atom,
	debug_message(2,"warning: no matching lemma ~w~n",[Pos/Atom])
    ).	
get_lemma_or_word(v_root(_,Lemma0),Lemma,Word) :-
    !,
    get_lemma_or_word(Lemma0,Lemma,Word).
get_lemma_or_word(Lemma0,Lemma,_) :-
    atom(Lemma0),
    atom_concat(Lemma1,'_DIM',Lemma0),
    !,
    Lemma1=Lemma.
get_lemma_or_word(L,L,_).

get_root(v_root(Root0,_),Root) :-
    !,
    Root0=Root.
get_root(L,L).

deptree_xml_label_rest_frame(on,Root,FrameTerm0,Cat,[pos-Pos|Atts],P0,_,Tag) :-
    add_folia(FrameTerm0,FrameTerm,P0),
    alpino_postags:postag_of_frame(FrameTerm,Frame,Pos,Attributes,Tag),
    deptree_xml_label_frame(Attributes,Atts1),
    add_lcat(Cat,Atts1,Atts2),
    add_frame(Frame,Root,Atts2,Atts).

deptree_xml_label_rest_frame(off,_Root,FrameTerm0,_,[pos-Pos|Atts],P0,_,Tag) :-
    add_folia(FrameTerm0,FrameTerm,P0),
    alpino_postags:postag_of_frame(FrameTerm,_,Pos,Attributes,Tag),
    deptree_xml_label_frame(Attributes,Atts).


add_folia(Atts:Term0,Atts:Term,P0) :- !,
    add_folia(Term0,Term,P0).

add_folia(Term0,Term,P0) :-
    (   alpino_lexical_analysis:user_skips(Skips),
	lists:member(folia(P0,_,Lemma,Tag),Skips)
    ->  Term = read_from_treebank(_,Term0,Lemma,Tag)
    ;   Term0=Term
    ).

add_lcat(Cat,Atts1,Atts) :-
    (   Cat == 'UNKNOWN'
    ->  Atts1=Atts
    ;   var(Cat)
    ->  Atts1=Atts
    ;   Cat=='_'
    ->  Atts1=Atts
    ;   Cat='$VAR'(_)
    ->  Atts1=Atts
    ;   Atts=[lcat-Cat|Atts1]
    ).

add_frame(Frame,Root,Atts1,Atts) :-
    (   Frame == none
    ->  Atts1=Atts
    ;   (   frame2sense(Root,Frame,Sense)
	->  Atts=[frame-Frame,sense-Sense|Atts1]
	;   format(user_error,"warning: frame2sense fails: ~w ~w~n",[Root,Frame]),
	    Atts1=Atts
	)
    ).

deptree_xml_label_frame([],[]).
deptree_xml_label_frame([Att=Val|T],[Att-Val|Atts]) :-
    deptree_xml_label_frame(T,Atts).

deptree_xml_ds([],[],_String,No,No,_,_).
deptree_xml_ds([H|T],[NH|NT],String,No0,No,Tags,His) :-
    deptree_xml_tree(H,NH,String,No0,No1,Tags,His),
    deptree_xml_ds(T,NT,String,No1,No,Tags,His).

get_root(P0,P,Sentence,Word) :-
    (   get_root0(P0,P,Sentence,Word0)
    ->  Word0=Word
    ;   format(user_error,"ERROR: mismatch in string and string positions!~n",
	       []),
	format(user_error,"ERROR: cannot find ~w-~w in ~w~n",[P0,P,Sentence]),
	fail
    ).

get_root0(P0,P,Sentence,Word) :-
    P1 is P-P0,
    length(Prefix,P0),
    length(Middle,P1),
    append(Prefix,Suffix,Sentence),
    append(Middle,_,Suffix),
    concat_all(Middle,Word,' ').


deptree_xml_tree_format(tree(Atts,_,Ds),Tab) -->
    format_to_chars('~*c<node',[Tab,32]),
    deptree_xml_atts_format(Atts),
    deptree_xml_close_label(Ds),
    deptree_xml_ds_format(Ds,Tab).

check_atts([],[]).
check_atts([A0-V0|Tail],[A0-V0|Tail1]) :-
    check_atts(Tail,A0,V0,Tail1).

%% duplicates are not supposed to occur, but if something
%% is wrong in postags.pl, we'll make sure the result only
%% has a single value per attribute
check_atts([],_,_,[]).
check_atts([A1-V1|Tail],A,V,Rest) :-
    (   A1 == A
    ->  format(user_error,"error: duplicate attributes: ~w ~w~n",[A=V,A=V1]),
	Rest=Rest1
    ;   Rest=[A1-V1|Rest1]
    ),
    check_atts(Tail,A1,V1,Rest1).

deptree_xml_close_label([]) -->
    format_to_chars('/>~n',[]).
deptree_xml_close_label([_|_]) -->
    format_to_chars('>~n',[]).

deptree_xml_ds_format([],_Tab) --> [].
deptree_xml_ds_format([H|T],Tab) -->
    { Tab2 is Tab+2
    },
    deptree_xml_tree_format(H,Tab2),
    deptree_xml_ds_format_(T,Tab).

deptree_xml_ds_format_([],Tab) -->
    format_to_chars('~*c</node>~n',[Tab,32]).
deptree_xml_ds_format_([H|T],Tab) -->
    { Tab2 is Tab+2
    },
    deptree_xml_tree_format(H,Tab2),
    deptree_xml_ds_format_(T,Tab).

deptree_xml_atts_format([]) --> [].
deptree_xml_atts_format([Att-Val|T]) -->
    deptree_xml_att_format(Att,Val),
    deptree_xml_atts_format(T).

deptree_xml_att_format(word,Word) -->
    !,
    {  format_to_chars("~w",[Word],WordChars0),
       add_xml_entities(WordChars0,WordChars)
    },
    format_to_chars(' word="~s"',[WordChars]).

deptree_xml_att_format(lemma,Word) -->
    !,
    {  format_to_chars("~w",[Word],WordChars0),
       add_xml_entities(WordChars0,WordChars)
    },
    format_to_chars(' lemma="~s"',[WordChars]).

deptree_xml_att_format(root,Root) -->
    !,
    {  format_to_chars("~w",[Root],RootChars0),
       add_xml_entities(RootChars0,RootChars)
    },
    format_to_chars(' root="~s"',[RootChars]).

%deptree_xml_att_format(mwu_root,Root) -->
%    !,
%    {  format_to_chars("~w",[Root],RootChars0),
%       add_xml_entities(RootChars0,RootChars)
%    },
%    format_to_chars(' mwu_root="~s"',[RootChars]).

deptree_xml_att_format(sense,Sense) -->
    !,
    {  format_to_chars("~w",[Sense],SenseChars0),
       add_xml_entities(SenseChars0,SenseChars)
    },
    format_to_chars(' sense="~s"',[SenseChars]).

%deptree_xml_att_format(mwu_sense,Sense) -->
%    !,
%    {  format_to_chars("~w",[Sense],SenseChars0),
%       add_xml_entities(SenseChars0,SenseChars)
%    },
%    format_to_chars(' mwu_sense="~s"',[SenseChars]).

deptree_xml_att_format(frame,Frame) -->
    !,
    format_to_chars(' frame="~q"',[Frame]).

deptree_xml_att_format(Att,Val) -->
    format_to_chars(' ~w="~w"',[Att,Val]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% GET XML FILE FROM SOMEWHERE.... %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_file_to_dt(File,DT) :-
    xml_file_to_dt(File,DT,_,_).

xml_file_to_dt(File,DT,Sentence,Comments) :-
    alpino_ds_file_to_xml_terms(File,XML),
    xml_term_to_dt(XML,DT,Sentence,Comments,_).

xml_file_to_dt(File,DT,Sentence,Comments,Meta) :-
    alpino_ds_file_to_xml_terms(File,XML),
    xml_term_to_dt(XML,DT,Sentence,Comments,Meta).

%% clib/dtget/treebank_server/file
%% how to get xml/treebank file?

alpino_ds_file_to_xml_terms(FN,Terms) :-
    hdrug_flag(get_treebank_file,Val),
    alpino_ds_file_to_xml_terms(Val,FN,Terms).

%% not available on hpc (never carefully checked why)
:- initialize_flag(get_treebank_file,clib).
:- use_module('../TreebankTools/IndexedCorpus/prolog/corpusreader').

:- initialize_flag(get_treebank_file,file).

alpino_ds_file_to_xml_terms(clib,FN,Terms):-
    (   try_get_data(FN,String)
    ->  true
    ;   format(user_error,"warning: cannot read file ~w~n",[FN]),
	fail
    ),
    alpino_ds_string_to_xml_terms(String,Terms).

alpino_ds_file_to_xml_terms(treebank_server,FN,Terms) :-
    xml_from_treebank_server(FN,Terms).

alpino_ds_file_to_xml_terms(file,FN,Terms) :-
    file_to_xml_terms(FN,Terms).

alpino_ds_file_to_xml_terms(dtget,FN,Terms) :-
    format_to_chars("dtget ~a",[FN],Codes),
    atom_codes(Command,Codes),
    popen(Command,read,Stream),
    getchars(Stream,String,[]),
    close(Stream),
    alpino_ds_string_to_xml_terms(String,Terms).

%% pre dtget;
%% also used for other xml files, e.g. in thistle2dt for thistle files
%% and in IMIX (pre dtget), make retrieval robust for QA/IR tasks....GB 2006-01-24
file_to_xml_terms(FN,Terms) :-
    (   try_file_to_codes(FN,String)
    ->  true
    ;   format(user_error,"warning: cannot read file ~w~n",[FN]),
	fail
    ),
    alpino_ds_string_to_xml_terms(String,Terms).

try_file_to_codes(FN,String) :-
    catch(corpusreader:file_to_codes(FN,String),_,fail),
    !.
try_file_to_codes(FN,String) :-
    alpino_treebank:prolog_file_to_codes(FN,String),
    !.
try_file_to_codes(FN,_) :-
    format(user_error,"warning: cannot read file ~w~n",[FN]),
    fail.

try_get_data(FN,String) :-
    catch(corpusreader:get_data(FN,String),_,fail),
    !.
try_get_data(FN,String) :-
    alpino_treebank:prolog_file_to_codes(FN,String),
    !.
try_get_data(FN,_) :-
    format(user_error,"warning: cannot read file ~w~n",[FN]),
    fail.


%% CLIENT for TREEBANK SERVER
%% this part by Joerg Tiedemann, originally for Joost
%% based on SICStus-Prolog Single-Threaded Client Example, check
%% http://dlp.cs.vu.nl/~ctv/dlpinfo/srcs/tcp/sicsock.pl.html
:- initialize_flag(treebank_server_host,'roskva.let.rug.nl').
:- initialize_flag(treebank_server_port,44444).

:- use_module(library(sockets)).

:- initialize_flag(initialized_persistent_treebank_server,off).

initialize_persistent_treebank_server(Stream) :-
    hdrug_flag(initialized_persistent_treebank_server,Val),
    (   Val == off
    ->  hdrug_flag(treebank_server_port,Port),
	hdrug_flag(treebank_server_host,Host),
	socket('AF_INET', Socket),
	(   connect_to_socket(5,Socket,Host,Port,Stream)
	->  true
	;   format(user_error,'xml_from_socket/2: cannot open socket!~n',[]),
	    fail
	),
	set_flag(initialized_persistent_treebank_server,
			    on(Stream)
			   )
    ;   Val = on(Stream)
    ).

xml_from_treebank_server(Id,XML) :-
    xml_from_treebank_server(5,Id,XML).

xml_from_treebank_server(Tries,Id,XML) :-
    (   Tries =< 0
    ->  format(user_error,"xml_from_treebank: server closes connection on us consistently. Giving up.~n",[]),
	fail
    ;   initialize_persistent_treebank_server(Stream),
	format(Stream, '~w~n', [Id]),
	flush_output(Stream),
	read_line(Stream,CodeList),
	(   CodeList == end_of_file
	->  format(user_error,"xml_from_treebank: warning socket appears to be closed. Retrying...~n",[]),
	    set_flag(initialized_persistent_treebank_server,off),
	    Tries2 is Tries-1,
	    xml_from_treebank_server(Tries2,Id,XML)
	;   number_codes(Number,CodeList),
	    (   Number =:= 0
	    ->  format(user_error,"xml_from_treebank: warning no result for ~w~n",[Id]),
		fail
	    ;   true
	    ),
	    getchars(Stream,String,[],Number),
	    alpino_ds_string_to_xml_terms(String,XML)
	)
    ).

% /users1/vannoord/z/Alpino/Treebank/Machine/clef/AD19940513/AD19940513-0143-902-1.xml
connect_to_socket(I,Socket,Host,Port,Stream) :-
    (   I =< 0
    ->  format(user_error,'connect_to_socket/2: cannot open socket!~n',[]),!,
	fail
    ;   N is I - 1,
	catch(socket_connect(Socket, 'AF_INET'(Host,Port), Stream),
	      PAT,
	      (
		format(user_error,"error: ~w~n",[PAT]),
		sleep(1),
		connect_to_socket(N,Socket,Host,Port,Stream)
	      )
	     )
    ).

%%%% STRING to XML term
:- use_module(pillow_xml2terms, [ xml2terms/2 ]).

alpino_ds_string_to_xml_terms(String,Terms) :-
    pillow_xml2terms:xml2terms(String,Terms0),
    remove_white_space(Terms0,Terms1),
    translate_entities(Terms1,Terms).

%%% GETCHARS: read all contents from buffer
%%% still used for reading all output of a command (dtget interface)
getchars(Stream,In,Out) :-
    get_code(Stream,Char),
    (   Char = -1
    ->	In = Out
    ;	In = [Char|Tail],
	getchars(Stream,Tail,Out)
    ).

%%% GETCHARS: read N characters from buffer
%%% still used for reading output of treebank server
%% getchars(Stream,In,Out,Nr)
%% reads Nr characters from Stream.
%% todo: error checking
getchars(Stream,In,Out,Number) :-
    (   Number =:= 0
    ->  In = Out
    ;   get_code(Stream,Char),
	(   Char = -1
	->  In = Out
	;   In = [Char|Tail],
	    Number1 is Number-1,
	    getchars(Stream,Tail,Out,Number1)
	)
    ).

%% in thistle and xml files, all only-whitespace elements can be ignored
remove_white_space([],[]).
remove_white_space([Chars|Terms],Terms1) :-
    white_space(Chars),
    !,
    remove_white_space(Terms,Terms1).
remove_white_space([env(Type,Atts,Content)|Terms],
		   [env(Type,Atts,Content1)|Terms1]) :-
    remove_white_space(Content,Content1),
    !,
    remove_white_space(Terms,Terms1).
remove_white_space([Term|Terms],[Term|Terms1]) :-
    remove_white_space(Terms,Terms1).

translate_entities([],[]).
translate_entities([H|T],[NH|NT]) :-
    translate_entities_el(H,NH),
    translate_entities(T,NT).

translate_entities_el([],[]) :- !.
translate_entities_el(xmldecl(A),xmldecl(A)) :- !.
translate_entities_el(env(Type,Atts0,Content0),env(Type,Atts,Content)) :-
    !,
    translate_entities_atts(Atts0,Atts),
    translate_entities(Content0,Content).
translate_entities_el(elem(Node,Atts0),elem(Node,Atts)) :-
    !,
    translate_entities_atts(Atts0,Atts).
translate_entities_el([0'&|T0],[H|T2]) :-
    xml_entity_prefix([0'&|T0],T,H),
    !,
    translate_entities_el(T,T2).
translate_entities_el([H|T],[H|T2]) :-
    !,
    translate_entities_el(T,T2).
translate_entities_el(X,X).

translate_entities_atts([],[]).
translate_entities_atts([Att=String0|T],[Att=String|NT]) :-
    translate_entities_el(String0,String),
    translate_entities_atts(T,NT).

white_space([]).
white_space([H|T]) :-
    white_space1(H),
    white_space(T).

white_space1(32).  % space
white_space1(13).  % dos newline
white_space1(10).  % newline
white_space1(9).   % tab

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% XML_TO_DT %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% todo: check version??
xml_term_to_dt(Term,DT,Sentence,Comments,Meta) :-
    (  member(env(top,_,Elems),Term)       % old
    ;  member(env(alpino_ds,_,Elems),Term) % new
    ), !,
    xml_term_to_dt_sentence(Elems,Sentence),
    xml_term_to_dt_comments(Elems,Comments),
    xml_term_to_dt_list(Elems,DT,dt),
    xml_term_to_dt_meta(Elems,Meta).

xml_term_to_dt(Term,DT,_,_,[]) :-
    member(env(alpino_adt,_,Elems),Term),
    !,
    xml_term_to_dt_list(Elems,DT,adt).

xml_term_to_dt_sentence(Elems,Sentence) :-
    member(env(sentence,Attributes,[Sentence]),Elems),
    sentid(Attributes).

xml_term_to_dt_comments(Elems,Comments) :-
    (   member(env(comments,Ignore,List),Elems)
    ->  ignore_message(Ignore,attributes,comments),
	xml_term_to_dt_comment_list(List,Comments)
    ;   Comments=[]
    ).

xml_term_to_dt_meta(Elems,Comments) :-
    (   member(env(metadata,Ignore,List),Elems)
    ->  ignore_message(Ignore,attributes,metadata),
	xml_term_to_dt_meta_list(List,Comments)
    ;   Comments=[]
    ).

sentid(Attributes) :-
    member(sentid=String, Attributes), !,
    name(Atom,String),  % will be number if possibl, otherwise atom
    set_flag(current_ref,Atom).
sentid([]).

ignore_message([],_,_).
ignore_message([_|_],Type,Env) :-
    debug_message(0,"warning: ignoring ~w of ~w~n",[Type,Env]).

xml_term_to_dt_comment_list([],[]).
xml_term_to_dt_comment_list([env(comment,Ignore,[String])|T0],[String|T]) :-
    ignore_message(Ignore,attributes,comment),
    xml_term_to_dt_comment_list(T0,T).

xml_term_to_dt_meta_list([],[]).
xml_term_to_dt_meta_list([elem(meta,Attributes)|T0],[meta(Attributes)|T]) :-
    xml_term_to_dt_meta_list(T0,T).

xml_term_to_dt_list(List,DT,Type) :-
    member(env(node,Atts,Sub),List),
    !,
    xml_term_to_dt_sub(env(node,Atts,Sub),DT,Type).
xml_term_to_dt_list(List,DT,Type) :-
    member(elem(node,Atts),List),
    !,
    xml_term_to_dt_sub(elem(node,Atts),DT,Type).

xml_term_to_dt_ds([],[],_).
xml_term_to_dt_ds([H0|T0],[H|T],Type) :-
    xml_term_to_dt_sub(H0,H,Type),
    !,
    xml_term_to_dt_ds(T0,T,Type).
xml_term_to_dt_ds([__H0|T0],T,Type) :- % ignore unknown elements, eg. ud
    xml_term_to_dt_ds(T0,T,Type).

xml_term_to_dt_sub(elem(node,Atts),tree(Label,_,[]),Type) :-
    xml_term_to_dt_atts(Atts,Label,leaf,Type).
xml_term_to_dt_sub(env(node,Atts,Sub),tree(Label,_,Ds),Type) :-
    (   (  member(env(node,_),Sub)
	;  member(env(node,_,_),Sub)
	;  member(elem(node,_),Sub)
	)
    ->  xml_term_to_dt_atts(Atts,Label,non_leaf,Type),
	xml_term_to_dt_ds(Sub,Ds,Type)
    ;   xml_term_to_dt_atts(Atts,Label,leaf,Type),
	Ds = []
    ).

xml_term_to_dt_atts(Atts0,r(Rel,Label),Leaf,Type) :-
    remove_empty_atts(Atts0,Atts1),
    (   select(rel=RelCodes,Atts1,Atts2)
    ->  true
    ;   debug_message(0,"error in ~w: attribute rel is missing~n",[Type]),
	fail
    ),
    atom_codes(Rel0,RelCodes),
    Rel = Rel0,
    Atts = Atts2,
    xml_term_to_dt_atts_rest(Leaf,Atts,Label,Type).

remove_empty_atts([],[]).
remove_empty_atts([Rel=H|T],Result):-
    remove_empty_att(H,Rel,Result,Result0),
    remove_empty_atts(T,Result0).

remove_empty_att([],_,Result,Result).
remove_empty_att([H|T],Att,[Att=[H|T]|Result],Result).

%% hard: how to distinguish at this point if a node has further info or
%% not; this is a hack!
xml_term_to_dt_atts_rest(leaf,Atts0,Label,_Type) :-
    \+ select(pos=_,Atts0,_),
    \+ select(cat=_,Atts0,_),
    \+ select(root=_,Atts0,_),
    \+ select(word=_,Atts0,_),
    \+ select(sense=_,Atts0,_),
    select(index=IndexCodes,Atts0,_),
    !,
    xml_index(Index,IndexCodes),
    Label=i(Index).

%% special case for many mistakes in D-Coi/Leuven trees
xml_term_to_dt_atts_rest(leaf,Atts0,Label,dt) :-
    \+ select(pos=_,Atts0,_),
    \+ select(cat=_,Atts0,_),
    !,
    format(user_error,"warning: leaf node without attribute pos~n",[]),
    xml_term_to_dt_atts_rest(leaf,[pos="--"|Atts0],Label,dt).

xml_term_to_dt_atts_rest(Leaf,Atts0,Label,Type) :-
    select(index=IndexCodes,Atts0,Atts1),
    !,
    xml_index(Index,IndexCodes),
    Label=i(Index,Rest),
    xml_term_to_dt_atts_rest(Leaf,Atts1,Rest,Type).

% ADTs
xml_term_to_dt_atts_rest(leaf,Atts0,adt_lex(Cat,Root,Sense,PosTag,LexAtts),adt) :-
    !,
    (   select(sense=SenseCodes,Atts0,Atts1)   % root or sense is obligatory
    ->  atom_codes(Sense,SenseCodes)
    ;   Atts0=Atts1
    ),
    (   select(root=RootCodes,Atts1,Atts2)
    ->  atom_codes(Root,RootCodes)
    ;   Atts1 = Atts2
    ),

    (   (  var(Root) ; var(Sense) )
    ->  Root = Sense
    ;   true
    ),

    (   var(Root)
    ->  debug_message(0,"Neither root nor sense attribute specified in ADT: ~w~n",[Atts0]),
	fail
    ;   true
    ),
    
    (   select(pos=PosTagCodes,Atts2,Atts3)
    ->  atom_codes(PosTag,PosTagCodes)
    ;   Atts2 = Atts3
    ),
    (   select(cat=CatCodes,Atts3,Atts4)
    ->  atom_codes(Cat,CatCodes)
    ;   Atts3=Atts4
    ),
    (   select(id=_,Atts4,Atts5)  % ignore this attribute
    ->  true
    ;   Atts4 = Atts5
    ),
    xml_atts_adt_lex(Atts5,LexAtts).

%% TODO: do something with remaining attributes!
xml_term_to_dt_atts_rest(leaf,Atts0,l(Pos,Cat,Root/[P0,P]),dt) :-
    xml_term_to_dt_atts_rest_frame(Atts0,Atts1,Pos),
    !,
    (   select(root=RootCodes,Atts1,Atts2)
    ->  atom_codes(Root,RootCodes)
    ;   Atts1 = Atts2,
	Root = 'NA'
    ),    
    select(begin=P0Codes,Atts2,Atts3),
    number_codes(P0,P0Codes),
    select(end=PCodes,Atts3,Atts4),
    number_codes(P,PCodes),
    (   select(lcat=CatCodes,Atts4,_Atts5)
    ->  atom_codes(Cat,CatCodes)
    ;   Cat = _
    ).

%% the "empty" dependency structure has cat and still is leaf
%% also, perhaps we want cat in the future for leaves
xml_term_to_dt_atts_rest(_,Atts0,Label,_Type) :-
    select(cat=CatCodes,Atts0,_Atts),
    !,
    atom_codes(Cat,CatCodes),
    Label=p(Cat).

%% be robust against missing cat
xml_term_to_dt_atts_rest(non_leaf,_Atts0,p(_),_Type).

xml_atts_adt_lex([],[]).
xml_atts_adt_lex([A=VCodes|T],[A=V|NewT]) :-
    atom_codes(V,VCodes),
    xml_atts_adt_lex(T,NewT).

:- use_module(library(charsio), [ read_from_chars/2 ]).
%% use frame if it is available (more informative), otherwise pos
%% (eg for manually annotated data)
xml_term_to_dt_atts_rest_frame(Atts0,Atts,Pos) :-
    (   select(frame=FrameCodes,Atts0,Atts1),
        select(lemma=LemmaC,Atts1,Atts2),
        select(postag=PosTagC,Atts2,Atts3),
	select(pos=PosCodes,Atts3,Atts),
        append(FrameCodes,[32,46],TerminatedFrameCodes),
	read_from_chars(TerminatedFrameCodes,Frame),
	parse_pos(Pos0,PosCodes),
        atom_codes(Lemma,LemmaC),
        atom_codes(PosTag,PosTagC),
	Pos = read_from_treebank(Pos0,Frame,Lemma,PosTag)
    ;   select(frame=PosCodes,Atts0,Atts),
        append(PosCodes,[32,46],TerminatedPosCodes),
	read_from_chars(TerminatedPosCodes,Pos)
    ;   select(lemma=LemmaC,Atts0,Atts1),
        select(postag=PosTagC,Atts1,Atts2),
	select(pos=PosCodes,Atts2,Atts),
	parse_pos(Pos0,PosCodes),
        atom_codes(Lemma,LemmaC),
        atom_codes(PosTag,PosTagC),
	Pos = read_from_treebank(Pos0,Lemma,PosTag)
    ;   select(pos=PosCodes,Atts0,Atts),
	parse_pos(Pos0,PosCodes),
	Pos = read_from_treebank(Pos0)
    ).

xml_index(Index,IndexCodes) :-
    number_codes_silent(Index,IndexCodes),
    !.
xml_index(Index,IndexCodes) :-
    atom_codes(Index,IndexCodes).  % even if mostly numbers, not always: CGN

%% historical pos-tags...
parse_pos(Pos,Codes) :-
     (   parse_named_pos(Pos0,Codes)
     ->  Pos=Pos0
     ;   atom_codes(Pos,Codes)
     ).

parse_named_pos(name('ORG'), "name(ORG)").
parse_named_pos(name('MISC'),"name(MISC)").
parse_named_pos(name('LOC'), "name(LOC)").
parse_named_pos(name('PER'), "name(PER)").
parse_named_pos(noun('AMOUNT'), "noun(AMOUNT)").
parse_named_pos(noun('TEMP'), "noun(TEMP)").


pretty_alignment(TriplesA,TriplesB,Alignment) :-
    lemmas(TriplesA,LemmasA),
    lemmas(TriplesB,LemmasB),
    pretty_alignment_(Alignment,LemmasA,LemmasB).

lemmas(Triples,Lemmas) :-
    findall(N-L,a_lemma(Triples,N-L),Lemmas0),
    sort(Lemmas0,Lemmas).

a_lemma(Triples,N-L) :-
    lists:member(deprel(L0/_/N0,_,L1/_/N1),Triples),
    (  L0/N0 = L/N ; L1/N1 = L/N ).

pretty_alignment_([],_,_).
pretty_alignment_([m(A0,B0)|T],A,B) :-
    lists:member(A0-LemmaA,A),
    lists:member(B0-LemmaB,B),
    (   _-LemmaA = _-LemmaB
    ->  true
    ;   format("Lemma mismatch  : ~w/~w <=> ~w/~w~n",[LemmaA,A0,LemmaB,B0])
    ),
    pretty_alignment_(T,A,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% COMPARE_DEPRELS %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_deprels(CorrectRels0,SystemRels0,CorSen,SysSen,Overlap,Correct,System) :-
    %% first, generate best alignment with gmt
    (   CorSen == SysSen  % frequent special case...
    ->  ignore_roots(SystemRels0,SystemRels)
    ;   alpino_util:time(2,alpino_geneval:gtm_best_alignment(CorSen,SysSen,Alignment)),
	debug_message(2,"alignment: ~w~n",[Alignment]),
	pretty_alignment(CorrectRels0,SystemRels0,Alignment),
	adapt_triples_with_alignment(SystemRels0,SystemRels,Alignment)
    ),
    ignore_roots(CorrectRels0,CorrectRels),
    overlap(CorrectRels,SystemRels,0,Overlap,MissingCorrect,SystemMistakes),
    (	System == Overlap,
	Correct == Overlap
    ->	true
    ;   analyze_mistakes(MissingCorrect,SystemMistakes)
    ).

overlap([deprel(HW,DepRel,DW)|Rels1],Rels2,
	S,S2,MC,MS) :-
    (	select(deprel(HW,DepRel,DW),Rels2,Rels2a)
    ->	S1 is S + 1,
	overlap(Rels1,Rels2a,S1,S2,MC,MS)
    ;	MC = [deprel(HW,DepRel,DW)|MC1],
	overlap(Rels1,Rels2,S,S2,MC1,MS)
    ).
overlap([],SystemMistakes,S,S,[],SystemMistakes).

analyze_mistakes(MissingCorrect,SystemMistakes) :-
    select(deprel(HW,CorrDepRel,DW),MissingCorrect,MC1),
    select(deprel(HW,SysDepRel,DW),SystemMistakes,SM1),
    !,
    format("Wrong dependency: ~p ~p ~p ~p~n",
	   [HW,DW,CorrDepRel,SysDepRel]),
    analyze_mistakes(MC1,SM1).
analyze_mistakes(MissingCorrect,SystemMistakes) :-
    format_deprels('Missing relation',MissingCorrect),
    format_deprels('System mistake  ',SystemMistakes).

format_deprels(_,[]).
format_deprels(Prefix,[deprel(HW,DepRel,DW)|Rels]) :-
    format("~p: ~p ~p ~p ~n",[Prefix,HW,DW,DepRel]),
    format_deprels(Prefix,Rels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% COMPARE RESULT WITH TREEBANK %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public tree_comparison/0,
          tree_comparison/1,
	  tree_comparison_find_best/0,
	  tree_comparison_all/0.


%% display how good object 1 is, compared to the treebank?
tree_comparison :-
    tree_comparison(1,_).

%% display how good object 1 is, compared to the treebank?
tree_comparison(Obj):-
    tree_comparison(Obj,_).

%% Score is the evaluation score of object Obj
tree_comparison(Obj,Score) :-
    hdrug:object(Obj,o(Result,_,_)),
    evaluate_result(Result,Score,Obj).

%% the parser has found N objects for a given sentence
%% which object is closest/least close to the treebank?
tree_comparison_find_best :-
    hdrug_flag(stop_if_optimal,OldFlag,off),  % we also report worst parse
    best_score_new_parse,
    (   hdrug:object(Obj,o(Result,_,_)),
	best_score_result_parse(Result,_,Obj),
	fail
    ;   true
    ),
    best_score_end_parse,
    hdrug_flag(stop_if_optimal,_,OldFlag).

%% for each object, display its evaluation score
tree_comparison_all :-
    (	hdrug:object(Object,o(Result,_,_)),
	format(user_error,"object ~w:~n",[Object]),
	evaluate_result(Result,_Score,Object),
	fail
    ;	true
    ).

best_score_new_parse(Sub) :-
    (    Sub == adt
    ->   hdrug_flag(current_ref,Key),
	 retractall(alpino_gen_suite:lf(Key,_))
    ;    true
    ),
    best_score_new_parse.

best_score_new_parse :-
    set_flag(best_score_object_no,undefined),
    hdrug_flag(current_ref,Item0),
    current_ref_treebank(Item0,Item),
    bb_put(best_score_object,none),
    bb_put(best_score_message,"no message
"),
    (	get_correct_triples(CorRels,Cor,CorSentence)
    ->	set_flag(current_ref_triples, triples(Item,CorRels,Cor,CorSentence)),
	set_flag(best_score_current,val(-1.0,0,Cor,0,0)),
	set_flag(worst_score_current,val(101.0,0,Cor,0,0))
    ;	set_flag(current_ref_triples,undefined),
	set_flag(best_score_current,undefined),
	set_flag(worst_score_current,undefined)
    ),
    statistics(runtime,[Now,_]),
    set_flag(start_time,Now).

best_score_end_parse :-
    best_score_end_parse(_).

best_score_end_parse(No) :-
    statistics(runtime,[EndTime,_]),
    hdrug_flag(start_time,StartTime),
    Runtime is EndTime - StartTime,
    hdrug_flag(best_score_current,val(ScoreB0,Ov,Corr,Sys,No)),
    hdrug_flag(worst_score_current,val(ScoreW0,Ov1,Corr1,Sys1,No1)),
    (	ScoreW0 > 100   % i.e. worst was never updated (no solution found)
    ->	ScoreW = 0
    ;	ScoreW = ScoreW0
    ),
    (   ScoreB0 < 0     % i.e. best was never updated (no solution found)
    ->  ScoreB = 0
    ;   ScoreB = ScoreB0
    ),
    hdrug_flag(current_ref,Item),
    hdrug_flag(found_solutions,Sols),
    format(user_error,
    "BEST SCORE: ~2f ov: ~w corr: ~w sys: ~w item ~w solution ~w of ~w ~w msec~n",
	   [ScoreB,Ov,Corr,Sys,Item,No,Sols,Runtime]),
    format(user_error,
   "WORST SCORE: ~2f ov: ~w corr: ~w sys: ~w item ~w solution ~w of ~w ~w msec~n",
	   [ScoreW,Ov1,Corr1,Sys1,Item,No1,Sols,Runtime]),
    bb_get(best_score_message,Message),
    format(user_error,"~s",[Message]).

best_score_result_parse(Result,_String,No) :-
    hdrug_flag(current_ref,Item0),
    current_ref_treebank(Item0,Item),
    hdrug_flag(current_ref_triples,triples(Item,CorRels,Cor,CorSentence)),
    get_system_triples(Result,SysRels,Sys,SysSentence,No),
    with_output_to_chars(
	alpino_treebank:compare_deprels(CorRels,SysRels,CorSentence,SysSentence,Ov,Cor,Sys),
				 EvalMessage),
    ca_score(Ov,Cor,Sys,CaScore),
    debug_message(2,"ca-score ~w: ~w~n",[Item/No,CaScore]),
    (	No =:= 1
    ->	format(user_error,
	   "FIRST SCORE: ~2f ov: ~w corr: ~w sys: ~w (item ~w)~n",
	   [CaScore,Ov,Cor,Sys,Item])
    ;	true
    ),
    hdrug_flag(best_score_current,val(CaScore0,_Ov0,_Cor0,_Sys0,_)),
    (	CaScore > CaScore0
    ->	set_flag(best_score_current,val(CaScore,Ov,Cor,Sys,No)),
	set_flag(best_score_object_no,No),
	bb_put(best_score_object,Result),
	bb_put(best_score_message,EvalMessage),
	debug_message(2,
	       "IMPROVED SCORE: ~2f item ~w; solution ~w~n",
	       [CaScore,Item,No]),
	debug_message(2,"~s",[EvalMessage])
    ;	true
    ),
    hdrug_flag(worst_score_current,val(WCaScore0,_,_,_,_)),
    (	CaScore < WCaScore0
    ->	set_flag(worst_score_current,val(CaScore,Ov,Cor,Sys,No))
    ;	true
    ),
    (	CaScore =:= 100
    ->	hdrug_flag(stop_if_optimal,Flag),
	(   Flag == on
	->  debug_message(2,
			"*** No further search (stop_if_optimal=on)~n",[]),
	    raise_exception(enough)
	;   true
	)
    ;	true
    ).

best_score_end_parse_sub(Sub) :-
    bb_get(best_score_object,Result),
    hdrug_flag(current_ref,Key),
    (	Result == none
    ->	true
    ;	format_of_result(Sub,Result,Key)
    ).

format_of_result(dump_xml,Result,_Key) :-
    alpino_data:result_term(_,String,_,_,_,_,Result),
    xml_save(Result,String,[],stream(user_output),normal).
format_of_result(xml,Result,Key) :-
    alpino_data:result_term(_,String,_,_,_,_,Result),
    format_to_chars("~w.xml",[Key],Codes),
    atom_codes(File,Codes),
    xml_save(Result,String,[],File,normal).
format_of_result(triples,Result,Key) :-
    alpino_dt:format_triples_without_postags_of_result(Result,Key).
format_of_result(frames,Result,Key) :-
    alpino_format_syntax:format_frames_of_result(Result,Key).
format_of_result(triples_with_frames,Result,Key) :-
    alpino_dt:format_triples_with_frames_of_result(Result,Key).
format_of_result(syntax,Result,Key) :-
    alpino_format_syntax:format_syntax_of_result(Result,Key).
format_of_result(deriv,Result,Key) :-
    alpino_format_syntax:format_deriv_of_result(Result,Key).
format_of_result(train_penalties,Result,_) :-
    format_score_with_penalties(Result).
format_of_result(gen_suite,Result,Key) :-
    format_gen_suite(Result,Key).
format_of_result(new_left_corners,Result,Key) :-
    alpino_format_syntax:format_new_left_corners_of_result(Result,Key).
format_of_result(dep_features,Result,Key) :-
    set_flag(dep_with_pos,on),
    format_dep_features(Result,Key),
    set_flag(dep_with_pos,off).
format_of_result(adt,Result,Key):-
    alpino_adt:result_to_adt(Result,Adt),
    assertz(alpino_gen_suite:lf(Key,Adt)).
format_of_result(adt_xml,Result,Key) :-
    adt_xml_filename(File,Key),
    xml_save_adt(Result,File).

 %% for Simon
format_dep_features(Result,Identifier) :-
    alpino_dt:result_to_dt(Result,DT),
    alpino_dt:dt_to_relations_with_somewhat_simplified_postags(DT,Rels),
    alpino_penalties:triples_to_features_which_have_freq_feature(Rels,Features),
    format_features_with_file(Features,Identifier).

%% this is ok *per sentence*, but not overall!
ca_score(Overlap,Correct,System,Score) :-
    (	Correct =:= 0
    ->	(   System =:= 0
	->  Score = 100.0
	;   Score = 0.0
	)
    ;	Score is 100* (1.0 - (max(Correct,System)-Overlap)/max(Correct,System))
    ).

score_string_of_result(Result,ScoreString) :-
    hdrug_flag(current_ref,Item0),
    current_ref_treebank(Item0,Item),
    hdrug_flag(current_ref_triples,triples(Item,CorRels,Cor,CorSentence)),
    get_system_triples(Result,SysRels,Sys,SysSentence,_),
    with_output_to_chars(
	alpino_treebank:compare_deprels(CorRels,SysRels,CorSentence,SysSentence,Ov,Cor,Sys),
			 _),
    format_to_chars("~w|~w|~w",[Ov,Cor,Sys],ScoreString).

%% construct data to be used in maxent learning
format_score_with_penalties(Result) :-
    hdrug_flag(current_ref,Item0),
    current_ref_treebank(Item0,Item),
    alpino_data:result_term(_,Sentence,Cat,Tree,Frames,Result),
    alpino_penalties:count_maxent_features(Cat,Tree,Frames,His,on),
    score_string_of_result(Result,ScoreString),
    compare_cgn_string_of_result(Item,Result,CgnString),
    length(Sentence,Length),
    format("MAXENT#~q#~w#~s~s#",[Item,Length,ScoreString,CgnString]),
    format_counted_features(His).
%    alpino_format_syntax:result_to_bracketed_string_palm(Result,PalmString,[]),
%    format("PALM#~q#~s\n",[Item,PalmString]).

compare_cgn_string_of_result(Item,Result,CgnString) :-
    xml_filename(File,Item),
    alpino_format_syntax:result_to_frames(Result,Frames,_),
    alpino_format_syntax:frames_to_postags(Frames,Result,SysTags),
    hdrug_flag(current_input_sentence,Words0),
    user:ignore_brackets(Words0,Words),
    user:collect_treebank_cgn(File,GoldTags),
    length(GoldTags,Len),
    user:compare_cgn(GoldTags,SysTags,0,Words,0,Correct,Item),
    format_to_chars("|~w|~w",[Correct,Len],CgnString).

format_penalties(Result) :-
    alpino_data:result_term(_,_,Cat,Tree,Frames,Result),
    alpino_penalties:count_maxent_features(Cat,Tree,Frames,His,on),
    hdrug_flag(current_ref,Item),
    writeq(Item), write('#'),
    write('NA'), write('#'),
    write('NA'), write('|'),
    write('NA'), write('|'),
    write('NA'), write('#'),
    format_counted_features(His).

current_ref_treebank(Item0,Item) :-
    atom(Item0),
    alpino_unknowns:atom_split(Item0,'#',Item,_),
    !.
current_ref_treebank(Item,Item).

get_correct_triples(CorrectRels,Correct,Sentence) :-
    hdrug_flag(current_ref,Item0),
    current_ref_treebank(Item0,Item),
    treebank_directory(GoldDir),
    format_to_chars("~w/~w.xml",[GoldDir,Item],CharsFile),
    atom_codes(CorrectFile,CharsFile),
    file2deprels(CorrectFile,CorrectRels,Correct,Sentence).

file2deprels(File,Relations,Size,Sentence) :-
    xml_file_to_dt(File,DT,Sentence0,_),
    string_to_words(Sentence0,Sentence),
    alpino_dt:dt_to_relations_with_lemma(DT,Relations0),
    positions_to_words(Relations0,Relations,Sentence),
    length(Relations,Size),!.
file2deprels(File,[],0,_) :-
    raise_exception(hdrug_error("get_correct_triples failed on: ~a~n",[File])).

positions_to_words([],[],_).
positions_to_words([deprel(Root/PosList,Rel,Root2/PosList2)|T0],
		   [deprel(Root/Word/Pos,Rel,Root2/Word2/Pos2)|T],Sentence) :-
    pos_to_word(PosList,Word,Pos,Sentence),
    pos_to_word(PosList2,Word2,Pos2,Sentence),
    positions_to_words(T0,T,Sentence).

pos_to_word([0,0],W,P,_) :-
    !,
    W = none, P = none.
pos_to_word(top,top,top,_).
pos_to_word([Pos,_],Word,Pos,Sentence) :-
    lists:nth0(Pos,Sentence,Word).

get_system_triples(Result,Relations,Size,Sentence,No) :-
    hdrug_flag(current_ref,Ref),
    (   alpino_dt:result_to_triples_without_postags_with_mwu(Result,Relations0),
	alpino_data:result(Result,_List,Sentence0),
	remove_phantoms(Sentence0,Sentence),
	positions_to_words(Relations0,Relations,Sentence)
    ->  true
    ;   raise_exception(hdrug_error("get_system_triples failed on: ~w (object ~w)~n",[Ref,No])),
	Relations=[]
    ),
    length(Relations,Size).

%% Result has a concept accuracy of Ca
evaluate_result(Result,Ca,ObjectNo) :-
    hdrug_flag(current_ref,Item),
    get_correct_triples(CorRels,Cor,CorSentence),
    get_system_triples(Result,SysRels,Sys,SysSentence,ObjectNo),
    compare_deprels(CorRels,SysRels,CorSentence,SysSentence,Ov,Cor,Sys),
    ca_score(Ov,Cor,Sys,Ca),
    format("item ~w: Dep Rel's: ~d/~d/~d CA: ~2f% ~w~n",[Item,Ov,Cor,Sys,Ca,ObjectNo]).

xml_file_to_xml_file(File0,File) :-
    xml_file_to_dt(File0,DT,Sentence0,Comments),
    string_to_words(Sentence0,Sentence),
    xml_save(already_dt(DT),Sentence,Comments,File,normal).

converse_xml_files([]).
converse_xml_files([File|Files]) :-
    converse_xml_file(File),
    converse_xml_files(Files).

converse_xml_file(File) :-
    xml_file_to_dt(File,DT0,Sentence0,Comments),
    string_to_words(Sentence0,Sentence),
    (   alpino_dt:apply_dt_transformations(DT0,DT,Sentence,Change)
    ->  true
    ;   format(user_error,"ERROR in conversion of ~w~n",[File]),
	fail
    ),
    (   Change == no
    ->  debug_message(1,"not converted file ~w~n",[File])
    ;   xml_save(already_dt(DT),Sentence,Comments,File,normal),
	debug_message(0,"converted file ~w~n",[File])
    ).

canonical_xml_overwrite_files([]).
canonical_xml_overwrite_files([File|Files]) :-
    (   canonical_xml_overwrite_file(File)
    ->  debug_message(1,"made file ~w canonical~n",[File])
    ;   format(user_error,"ERROR in conversion of ~w~n",[File])
    ),
    canonical_xml_overwrite_files(Files).

canonical_xml_overwrite_file(File) :-
    xml_file_to_dt(File,DT,Sentence0,Comments,Meta),
    string_to_words(Sentence0,Sentence),
    xml_save(already_dt(DT),Sentence,Comments,Meta,File,normal).

canonical_xml_file(File) :-
    xml_file_to_dt(File,DT,Sentence0,Comments,Meta),
    string_to_words(Sentence0,Sentence),
    xml_save(already_dt(DT),Sentence,Comments,Meta,stream(user_output),normal).

empty_xml_files([]).
empty_xml_files([File|Files]) :-
    (   empty_xml_file(File)
    ->  true
    ;   format(user_error,"ERROR in removing all structure from ~w~n",[File])
    ),
    empty_xml_files(Files).

empty_xml_file(File) :-
    format(user_error,"removing all structure from ~w~n",[File]),
    xml_file_to_dt(File,_DT,Sentence0,_Comments),
    string_to_words(Sentence0,Sentence),
    alpino_data:result_term(p(0.0,[]),Sentence,robust([]),
                     tree(robust,robust,[]),[],Result),
    xml_save(Result,Sentence,["ignore"],File,ignore).

%preceding_treebank_file(CurFile,PrevFile) :-
%    corpusreader:pathname_before(CurFile,PrevFile).
%
%following_treebank_file(CurFile,NextFile) :-
%    corpusreader:pathname_after(CurFile,NextFile).

%% if corpusreader:file_to_codes does not exist:
:- public prolog_file_to_codes/2.
prolog_file_to_codes(File,String):-
    open(File,read,Stream),
    getchars(Stream,String,[]),
    close(Stream).

%% add sense if frame is given
%% frame2sense(+Root,+Frame,-Sense)
frame2sense(Root,_:Frame,Sense) :-
    !,
    frame2sense(Root,Frame,Sense).
frame2sense(Root,Frame,Sense) :-
    f2s(Frame,Prefixes,Suffixes,_FixedEls0,_FixedEls),
    !,
    append(Prefixes,[Root|Suffixes],List0),
    all_atoms(List0,List),
    concat_all(List,Sense,'-').
frame2sense(_,read_from_treebank(_,Lemma0,_),Lemma) :-
    !,
    Lemma0=Lemma.
frame2sense(Root,_,Root).

f2s(verb(_,_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(adjective(_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(v_noun(Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(noun(_,_,_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(mod_noun(_,_,_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(meas_mod_noun(_,_,_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(amount_meas_mod_noun(_,_,_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).
f2s(tmp_noun(_,_,_,Frame),Prefix,Suf,Fix0,Fix) :-
    fr2s(Frame,Prefix,Suf,Fix0,Fix).

fr2s(Frame,Pre,Suf,Fix0,Fix) :-
    Frame =.. [Fun,_Part|Args],
    atom_concat(part_,Fun1,Fun),!,
    Frame1 =.. [Fun1|Args],
    fr2s(Frame1,Pre,Suf,Fix0,Fix).
fr2s(fixed(List0,_),Prefix,Suf,Fix0,Fix) :-
    reverse(List0,List),
    fix2s(List,Prefix,[],Suf,[],Fix0,Fix).
fr2s(fixed(List0),Prefix,Suf,Fix0,Fix) :-
    reverse(List0,List),
    fix2s(List,Prefix,[],Suf,[],Fix0,Fix).
fr2s(part(Part),[Part],[],Fix,Fix).
fr2s(part(Part,Frame),[Part|Ls],Rs,Fix0,Fix) :-
    fr2s(Frame,Ls,Rs,Fix0,Fix).
fr2s(pp(Prep),[],[Prep],Fix,Fix).
fr2s(pp_meas(Prep),[],[Prep],Fix,Fix).
fr2s(pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(er_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(amb_so_np_pass_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(np_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(aan_het,[],[aan_het],Fix,Fix).
fr2s(np_aan_het,[],[aan_het],Fix,Fix).
fr2s(op,[],[op],Fix,Fix).
fr2s(np_er_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(so_np_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(np_np_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(meas_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(meas_er_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(meas_er_pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(adv_meas_er_pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(adv_meas_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(adv_meas_er_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(refl,[zich],[],Fix,Fix).
fr2s(refl_np,[zich],[],Fix,Fix).
fr2s(refl_ld,[zich],[],Fix,Fix).
fr2s(refl_ld_adv,[zich],[],Fix,Fix).
fr2s(refl_ld_dir,[zich],[],Fix,Fix).
fr2s(refl_ld_pp,[zich],[],Fix,Fix).
fr2s(refl_ld_transitive,[zich],[],Fix,Fix).
fr2s(refl_sbar,[zich],[],Fix,Fix).
fr2s(refl_pp(Prep),[zich],[Prep],Fix,Fix).
fr2s(refl_er_pc_pp(Prep),[zich],[Prep],Fix,Fix).
fr2s(pred_refl,[zich],[],Fix,Fix).
fr2s(nonp_pred_refl,[zich],[],Fix,Fix).
fr2s(als_pred_refl,[zich],[],Fix,Fix).
fr2s(ap_pred_refl,[zich],[],Fix,Fix).
fr2s(np_pred_refl,[zich],[],Fix,Fix).
fr2s(refl_pc_pp(Prep),[zich],[Prep],Fix,Fix).
fr2s(refl_aan_pc_pp,[zich],[aan],Fix,Fix).
fr2s(pc_pp_refl(Prep),[Prep,zich],[],Fix,Fix).
fr2s(np_pc_pp_refl(Prep),[Prep,zich],[],Fix,Fix).
fr2s(refl_vp,[zich],[],Fix,Fix).
fr2s(pred_pc_pp(Prep),[],[Prep],Fix,Fix).
fr2s(het_subj,[het],[],Fix,Fix).
fr2s(er_pp_sbar(Prep),[],[Prep],Fix,Fix).
fr2s(er_pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(np_er_pp_sbar(Prep),[],[Prep],Fix,Fix).
fr2s(np_er_pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(obj_np_er_pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(refl_er_pp_vp(Prep),[zich],[Prep],Fix,Fix).
fr2s(so_np_er_pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(so_np_er_pp_sbar(Prep),[],[Prep],Fix,Fix).
fr2s(refl_er_pp_sbar(Prep),[zich],[Prep],Fix,Fix).
fr2s(ninv(L,_),ListL,ListR,Fix0,Fix) :-
    fr2s(L,ListL,ListR,Fix0,Fix).
fr2s(pp_pred_np(A,B),[A,B],[],Fix,Fix).
fr2s(pp_pred_np_sbar(A,B),[A,B],[],Fix,Fix).
fr2s(pp_pred_np_vp(A,B),[A,B],[],Fix,Fix).
fr2s(pp_copula(A,B),[A,B],[],Fix,Fix).
fr2s(pp_copula_sbar(A,B),[A,B],[],Fix,Fix).
fr2s(pp_copula_vp(A,B),[A,B],[],Fix,Fix).
fr2s(pp_refl_sbar(Prep),[zich],[Prep],Fix,Fix).
fr2s(refl_sbar,[zich],[],Fix,Fix).
fr2s(sbar_subj_refl_no_het,[zich],[],Fix,Fix).
fr2s(pp_dip_sbar(Prep),[],[Prep],Fix,Fix).
fr2s(pp_sbar(Prep),[],[Prep],Fix,Fix).
fr2s(pp_sbar_obj(Prep),[],[Prep],Fix,Fix).
fr2s(pp_sbar_subj(Prep),[],[Prep],Fix,Fix).
fr2s(pp_sbar_subj_no_het(Prep),[],[Prep],Fix,Fix).
fr2s(pp_sbar_subj_opt_het(Prep),[],[Prep],Fix,Fix).
fr2s(pp_vp(Prep),[],[Prep],Fix,Fix).
fr2s(pp_vp_obj(Prep),[],[Prep],Fix,Fix).
fr2s(pp_vp_subj(Prep),[],[Prep],Fix,Fix).
fr2s(pp_vp_subj_no_het(Prep),[],[Prep],Fix,Fix).
fr2s(het_subj,[het],[],Fix,Fix).
fr2s(er_er,[er],[],Fix,Fix).
fr2s(obj_er_er,[er],[],Fix,Fix).

%% nouns:
%% to distinguish "het woord hoogste"
%% from "het hoogste woord"
%% this used to add MEAS to the sense

fr2s(measure,[],[],Fix,Fix).

fix2s([],L,L,R,R,Fix,Fix).
fix2s([H|T],L0,L,R0,R,Fix0,Fix) :-
    fixel(H,L0,L1,R0,R1,Fix0,Fix1),
    fix2s(T,L1,L,R1,R,Fix1,Fix).

fixel({List},L0,L,R0,R,Fix0,Fix) :-
    !,
    fix2s(List,L0,L,R0,R,Fix0,Fix).
fixel([H|T],L0,L,R,R,[[H|T]|Fix],Fix) :-
    !,
    append([H|T],L,L0).
fixel(op_een_v,[op_een_v|L],L,R,R,Fix,Fix) :-
    !.
fixel(acc(nominalization(W)),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(acc(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(nominalization(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(inv_acc(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(subj(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(ap_pred(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(np_pred(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(inv_np_pred(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(pc(W),L,L,[W|R],R,Fix,Fix) :-
    !.
fixel(er_pp(W),L,L,[W|R],R,Fix,Fix) :-
    !.
fixel(svp_er_pp(W),L,L,[W|R],R,Fix,Fix) :-
    !.
fixel(svp_er,[er|L],L,R,R,Fix,Fix) :-
    !.
fixel(er_pp(W,_),L,L,[W|R],R,Fix,Fix) :-
    !.
fixel(pp_refl(W),[W,zich|L],L,R,R,Fix,Fix) :-
    !.
fixel(pp_pred(A,B),[A,B|L],L,R,R,Fix,Fix) :-
    !.
fixel(compar,['ADJ-er'|L],L,R,R,Fix,Fix) :-
    !.
fixel(dat_pp(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(inv_dat_pp(W),[W|L],L,R,R,Fix,Fix) :-
    !.
fixel(het_obj1,[het|L],L,R,R,Fix,Fix) :-
    !.
fixel(het_svp,[het|L],L,R,R,Fix,Fix) :-
    !.
fixel(het_pobj1,[het|L],L,R,R,Fix,Fix) :-
    !.
fixel(refl,[zich|L],L,R,R,Fix,Fix) :-
    !.
fixel(vc(V,_,_),L,L,[V|R],R,Fix,Fix) :-
    !.
fixel(svp_pp(A,B),[A,B|L],L,R,R,Fix,Fix) :-
    !.
fixel(i(Abb,_),L0,L,R0,R,Fix0,Fix) :-
    !,
    fixel(Abb,L0,L,R0,R,Fix0,Fix).
fixel(yt(Abb),L0,L,R0,R,Fix0,Fix) :-
    !,
    fixel(Abb,L0,L,R0,R,Fix0,Fix).
fixel(nt(Abb),L0,L,R0,R,Fix0,Fix) :-
    !,
    fixel(Abb,L0,L,R0,R,Fix0,Fix).
fixel(_H,L,L,R,R,Fix,Fix).


all_atoms([],[]).
all_atoms([H0|T0],[H|T]) :-
    (   atom(H0)
    ->  H0 = H
    ;   H0 = [_|_],
        concat_all(H0,H,'-')
    ),
    all_atoms(T0,T).

%%%%%%%%%%%%%%%
% ADTs in XML %
%%%%%%%%%%%%%%%

:- public xml_save_object_adt/2.

xml_save_object_adt(Obj,File) :-
    hdrug:object(Obj,o(Result,_,_)),
    xml_save_adt(Result,File).

xml_save_adt(Result,File) :-
    deptree_xml_adt(Result,Chars,[]),
    alpino_treebank:stream_or_open_file(File,Stream),
    format(Stream,"~s",[Chars]),
    close(Stream),
    debug_message(1,"ADT xml saved in ~w~n",[File]).

deptree_xml_adt(Cat) -->
    { alpino_adt:result_to_adt(Cat,Tree0),
      deptree_xml_tree(Tree0,Tree,_,0,_,[],[]) },
    deptree_xml_start_adt,
    deptree_xml_tree_format(Tree,2),
    deptree_xml_end_adt.

deptree_xml_start_adt -->
    {  hdrug_flag(alpino_ds_version,Version) },
    format_to_chars('<?xml version="1.0" encoding="UTF-8"?>~n<alpino_adt version="~w">~n',[Version]).

deptree_xml_end_adt -->
    format_to_chars('</alpino_adt>~n',[]).

xml_file_to_adt(File,Adt) :-
    xml_file_to_dt(File,Adt0),
    tree2tree3(Adt0,Adt).

tree2tree3(tree(Node,_,Ds0),tree(Node,Ds)) :-
    tree2tree3_ds(Ds0,Ds).

tree2tree3_ds([],[]).
tree2tree3_ds([H0|Ds0],[H|Ds]) :-
    tree2tree3(H0,H),
    tree2tree3_ds(Ds0,Ds).

%% adapt_triples_with_alignment(SystemRels0,SystemRels,Alignment) 

adapt_triples_with_alignment([],[],_).
adapt_triples_with_alignment([H0|T0],[H|T],Al) :-
    adapt_triple_with_alignment(H0,H,Al),
    adapt_triples_with_alignment(T0,T,Al).

adapt_triple_with_alignment(deprel(_/Hw/Hp,Rel,_/Dw/Dp), deprel(Hw/Hp2,Rel,Dw/Dp2), Al) :-
    adapt_pos_with_alignment(Hp,Hp2,Al),
    adapt_pos_with_alignment(Dp,Dp2,Al).

adapt_pos_with_alignment(Pos0,Pos,_) :-
    Pos0 == top,
    !,
    Pos0=Pos.
adapt_pos_with_alignment(Pos0,Pos,Al) :-
    (   lists:member(m(Pos1,Pos0),Al)
    ->  Pos = Pos1
    ;   Pos = o(Pos0)
    ).
    
ignore_roots([],[]).
ignore_roots([H0|T0],[H|T]) :-
    ignore_roots_(H0,H),
    ignore_roots(T0,T).

ignore_roots_(deprel(_/Hw/Hp,Rel,_/Dw/Dp), deprel(Hw/Hp,Rel,Dw/Dp) ).

tree_comparison_pair(File1,File2) :-
    file2deprels(File1,Rels10,X1,Sent10),
    file2deprels(File2,Rels20,X2,Sent20),
    normalize_sent(Sent10,Sent1),
    normalize_sent(Sent20,Sent2),
    normalize_rels(Rels10,Rels1),
    normalize_rels(Rels20,Rels2),
    compare_deprels(Rels1,Rels2,Sent1,Sent2,Ov,X1,X2),
    ca_score(Ov,X1,X2,Ca),
    format("Dep Rel's: ~d/~d/~d CA: ~2f%~n",[Ov,X1,X2,Ca]).

normalize_sent([],[]).
normalize_sent([H0|T0],[H|T]) :-
    normalize(H0,H),
    normalize_sent(T0,T).

normalize(H0,H) :-
    atom(H0),
    atom_codes(H0,Codes),
    alpino_unknowns:decap_chars(Codes,Codes1),
    alpino_latin1:deaccent_chars(Codes1,Codes2),
    atom_codes(H,Codes2).

normalize_rels([],[]).
normalize_rels([H0|T0],[H|T]) :-
    normalize_rel(H0,H),
    normalize_rels(T0,T).

normalize_rel(deprel(A/B/C,Rel,D/E/F),deprel(A/B1/C,Rel,D/E1/F)) :-
    normalize(B,B1),
    normalize(E,E1).



count_cats_and_skips([],Cats,Cats,Skips,Skips).
count_cats_and_skips([tree(skip,robust_skips(W),_,_)|T],Cats0,Cats,Skips0,Skips) :-
    !,
    count_skips(W,Skips0,Skips1),
    count_cats_and_skips(T,Cats0,Cats,Skips1,Skips).
count_cats_and_skips([tree(_,_,_,_)|T],Cats0,Cats,Skips0,Skips) :-
    Cats1 is Cats0+1,
    count_cats_and_skips(T,Cats1,Cats,Skips0,Skips).

count_skips(W,P0,P) :-
    (    alpino_lexical_analysis:tag(_,_,_,_,_,W,_,punct(_))
    ->   P is P0
    ;    P is P0 + 1
    ).

encode_his(HisTerm,[his-Fun|Atts0],Atts) :-
    HisTerm =.. [Fun|HisArgs],
    encode_his_rest(HisArgs,his_,1,Atts0,Atts).

encode_his_rest([],_,_,Atts,Atts).
encode_his_rest([HisTerm|HisRest],Path,N0,[Att-Fun|Atts0],Atts) :-
    attribute(Path,N0,Att,PathNext),
    (   number(HisTerm)
    ->  HisTerm = Fun,
	Atts0=Atts1
    ;   HisTerm =.. [Fun0|HisArgs],
	N1 is N0 + 1,
	atom_codes(Fun0,Fun0Codes),
	add_xml_entities(Fun0Codes,FunCodes),
	atom_codes(Fun,FunCodes),
	encode_his_rest(HisArgs,PathNext,1,Atts0,Atts1)
    ),
    encode_his_rest(HisRest,Path,N1,Atts1,Atts).

attribute(Path,N0,Att,PathNext) :-
    format_to_chars("~w~w",[Path,N0],AttChars),
    atom_codes(Att,AttChars),
    format_to_chars("~w~w_",[Path,N0],PathNextChars),
    atom_codes(PathNext,PathNextChars).

%Atts1 = [his-His|Atts]

