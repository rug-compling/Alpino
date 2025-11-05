:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

%% start.pl
%% loads all files; defines various hdrug hooks; sets global hdrug variables

:- multifile
    hdrug_command/3,
    hdrug_command_help/3.
:- discontiguous
    hdrug_command/3,
    hdrug_command_help/3.

:- public
    alpino_table_item:unknown_predicate_handler/2,
    alpino_table_goal:unknown_predicate_handler/2.

alpino_table_item:unknown_predicate_handler(_,fail).
alpino_table_goal:unknown_predicate_handler(_,fail).

%% initialize hdrug flags
:- set_flag(parser(alpino_lc),on).
:- set_flag(generator(alpino_cg),on).
:- set_flag(top_features,grammar).
:- set_flag(useful_try_check,off).
:- set_flag(object_exists_check,off).
:- set_flag(cmdint,on).
:- set_flag(parser,alpino_lc).
:- set_flag(generator,alpino_cg).
:- set_flag(useful_try_check,off).
:- set_flag(max_number_of_objects,5000).
:- initialize_flag(geneval,on).

%% :- initialize_flag(application_type,news).

%% FADD DATA FILES
:- initialize_flag(names_dict,
		   alpino('Names/seeds.fsa')).

:- initialize_flag(gen_dict,
		   alpino('Lexicon/dict')).

:- initialize_flag(corpus_frequency,
		   alpino('Grammar/corpus_frequency_features.fsa')).

:- initialize_flag(pos_tagger_dir,
		   alpino('PosTagger/MODELS')).

:- initialize_flag(ngram_model_dir,
		   alpino('Generation/fluency')).

%% standard files - it is not assumed you want to load
%% alternatives for these.
:- initialize_flag(grammar,          alpino('Grammar/grammar')).
:- initialize_flag(genrules,         alpino('Grammar/grammar_genrules')).
:- initialize_flag(lexicon,          alpino('Grammar/lex')).
:- initialize_flag(lex,              alpino('Lexicon/lex')).
:- initialize_flag(lex_conv,         alpino('Grammar/lex_conv.gram')).
:- initialize_flag(types,            alpino('Grammar/types')).
:- initialize_flag(data_definitions, alpino('Grammar/data')).

%% Prolog model files
:- initialize_flag(penalties,        alpino('Grammar/penalties')).

:- initialize_flag(syntax_features,  penalty_weights).
                      % historically, relative to directory of penalties...

:- initialize_flag(suite,            alpino('Suites/g_suite')).

:- initialize_flag(fluency_feature_weights,
                   alpino('Generation/fluency/maxent_feature_weights')).

:- initialize_flag(use_fluency_model,on).
:- initialize_flag(fluency_beam,0).
:- initialize_flag(generate_bc_check,on).
:- initialize_flag(generation_suite_parts,1).
:- initialize_flag(generation_suite_part,1).

:- initialize_flag(user_max,0).
:- set_flag(hdrug_report_space,total).
:- initialize_flag(display_quality,on).

:- initialize_flag(disambiguation_beam,0).
:- initialize_flag(disambiguation_candidates_beam,0).
:- initialize_flag(use_guides,off).
:- initialize_flag(parse_candidates_beam,0).
:- initialize_flag(unpack_bestfirst,on).
:- initialize_flag(number_analyses,10).
:- initialize_flag(robustness,if_required).
:- initialize_flag(disambiguation,on).
:- initialize_flag(robust_parts_are_independent,on).
:- initialize_flag(robustness_allows_skips,on).
:- initialize_flag(display_main_parts,on).
:- initialize_flag(require_brackets_balanced,on).

:- initialize_flag(compare_cgn,off).

%% load additional files

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(timeout)).
:- use_module(library(system)).

:-  use_module( [ lc,
		  robust,
		  pretty,
		  tree_formats,
		  dt,
		  hierarchy,
		  treebank,
		  postags,
		  cgn_postags,
		  utils,
		  format_syntax, % tags, syntax, deriv
		  compile_grammar,
		  wappend,
		  lexical_analysis,
		  select,
		  guides,
		  '../fadd/pro_fadd',
		  '../PosTagger/alpino_pos',
		  '../Tokenization/tokenize.pl',
		  '../Generation/adt',
		  '../Generation/geneval',
		  '../Generation/cg'
		]).

%% hdrug_hook
:- public
    compile_grammar/0,
    compile_grammar_file/1,
    reconsult_grammar/0,
    reconsult_grammar_file/1,
    interpret_grammar/0.

%% used in lc.pl
interpret_grammar :-
    set_flag(load_grammar_mode,interpret),
    load_types,
    load_grammar.

compile_grammar :-
    load_types,
    load_grammar.

compile_grammar_file(File):-
    set_flag(grammar,File),
    load_types,
    load_grammar.

reconsult_grammar :-
    load_types,
    load_grammar.

reconsult_grammar_file(File):-
    load_types,
    set_flag(grammar,File),
    load_grammar.

:- public
    compile_test_suite/1,
    reconsult_test_suite/1.

%% hdrug_hook
compile_test_suite(File) :-
    set_flag(suite,File),
    compile(File).

%% hdrug_hook
reconsult_test_suite(File) :-
    set_flag(suite,File),
    reconsult(File).

%% used in suites
%% done in a very complicated way so that it still works if you create .po files for suites
:- multifile user:term_expansion/2.
user:term_expansion(expand_sentences,List) :-
    expand_sentences_orig(List,List1),
    expand_sentences_sort(List1,List2),
    meta_pred(List2,[]).

expand_sentences_orig(List0,List) :-
    findall(orig_sent(Key,Sent),alpino_suite:my_sentence(Key,Sent),List0,List).

expand_sentences_sort(Total0,Total) :-
    findall(L-sentence(Key,Sent),(alpino_suite:my_sentence(Key,Sent),
				  length(Sent,L)
				 ), List0),
    keysort(List0,List1),
    findall(sort_sent(Key,Sent),lists:member(_-sentence(Key,Sent),List1),Total0,Total).

meta_pred([ ( sentence(Key,Sent) :-
	           hdrug_util:hdrug_flag(suite_shortest_first,YesNo),
		(   YesNo == on
                ->  sort_sent(Key,Sent)
		;   orig_sent(Key,Sent)
		)
	    )|Tail], Tail).


:- public load_suite/0.
load_suite :-
    hdrug_flag(suite,File),
    (	File==undefined
    ->  inform_undefined_module(suite,alpino_suite)
    ;   File==n
    ->  inform_undefined_module(suite,alpino_suite)
    ;   set_flag(print_suite,""),
	overrule_module(File)
    ),

    if_gui(update_sents),
    if_gui(tcl(add_parse_widget)),
    if_gui(tcl(extend_parse_widget)),
    if_gui(update_buttons).

lf(Id,LF) :-
    hdrug_flag(generation_suite_parts,Parts),
    hdrug_flag(generation_suite_part,Part),
    lf_(Parts,Part,Id,LF).

lf_(1,_Part,Id,Lf) :-
    !,
    hook(alpino_gen_suite:lf(Id,Lf)).
lf_(Parts,Part,Id,Lf) :-
    findall(lf(A,B),hook(alpino_gen_suite:lf(A,B)),LFs),
    extract_part(LFs,PartLFs,1,Part,Parts),
    member(lf(Id,Lf),PartLFs).

extract_part([],[],_,_,_).
extract_part([H|T],[H|NewT],Part,Part,Parts) :-
    !,
    (   Part == Parts
    ->  NewCount = 1
    ;   NewCount is Part + 1
    ),
    extract_part(T,NewT,NewCount,Part,Parts).
extract_part([_|T],NewT,Count,Part,Parts) :-
    (   Count == Parts
    ->  NewCount = 1
    ;   NewCount is Count + 1
    ),
    extract_part(T,NewT,NewCount,Part,Parts).
 

load_types :-
    hdrug_flag(types,File),
    (   File==undefined
    ->  inform_undefined_module(types,alpino_types)
    ;   File==n
    ->  inform_undefined_module(types,alpino_types)
    ;   use_module(File)
    ),
    load_data_definitions.

load_data_definitions :-
    hdrug_flag(data_definitions,File),
    (   File==undefined
    ->  inform_undefined_module(data,alpino_data)
    ;   File==n
    ->  inform_undefined_module(data,alpino_data)
    ;   compile(File)  % perhaps types.pl has changed...
    ).

hdrug_command(load,interpret_grammar,[g]).
hdrug_command(load,(set_suite(Suite),
		    set_flag(treebank,undefined),
		    load_suite),[s,Suite]).
hdrug_command(load,load_suite,[s]).
hdrug_command(load,(load_types,
		    load_lexicon),[l]).
hdrug_command(load,load_penalties,[p]).

hdrug_command(su,
	      (  set_flag(treebank,undefined),
		 set_suite(SUITE)
	      ),[SUITE]).
hdrug_command(suite,
	      (  set_flag(treebank,undefined),
		 set_suite(SUITE)
	      ),[SUITE]).

hdrug_command(l,list_sentences([Key]),[]) :-
    hdrug_flag(current_ref,Key).
hdrug_command(list,list_sentences([Key]),[]) :-
    hdrug_flag(current_ref,Key).

hdrug_command_help(load,"load g","to load grammar").
hdrug_command_help(load,"load s","to load default suite").
hdrug_command_help(load,"load s File","to load suite $(ALPINO_HOME)/Suites/File").
hdrug_command_help(load,"load l","to load lexicon").
hdrug_command_help(load,"load p","to load penalties").

%% hdrug_hook
hdrug_initialization :-
    load_parametric_data.
    
:- public load_penalties/0.
load_penalties :-
    hdrug_flag(penalties,File),
    (	File==undefined
    ->	inform_undefined_module(penalties,alpino_penalties)
    ;	File==n
    ->	inform_undefined_module(penalties,alpino_penalties)
    ;   overrule_module(File)
    ),
    hdrug_flag(syntax_features,File2),
    (	File2==undefined
    ->	inform_undefined_module(syntax_features,alpino_disambiguation_weights)
    ;	File2==n
    ->	inform_undefined_module(syntax_features,alpino_disambiguation_weights)
    ;   atom_concat('Grammar/',File2,File3),
	FileSpec = alpino(File3),  % to make the x-ref silence
	overrule_module(alpino_penalties:FileSpec)
    ).

%% hdrug_hook
top(grammar,_).

%% hdrug_hook
:- multifile
    option/3.
:- discontiguous
    option/3.

option(parse) -->
    !,
    no_dash(Words),
    !,
    {     set_flag(batch_command,parse_single_or_loop(Words))
    }.

option(generate) -->
    {  set_flag(batch_command,generate_adt_xmls) }.

option(save_adt) -->
    {  set_flag(batch_command,save_adt_xmls) }.

option(test1) -->
    { test1_options }.

option(testN) -->
    { testN_options }.

option(u) -->
    { undefined_options }.

option(veryfast) -->
    { veryfast_options }.

option(disamb) -->
    { disamb_options }.

option(very_fast) -->
    { veryfast_options }.

option(fast) -->
    { fast_options }.

option(slow) -->
    { slow_options }.

option(s) -->
    [SUITE],
    { set_suite(SUITE) }.

set_suite(SUITE) :-
    atom_concat('Suites/',SUITE,Psuite0),
    absolute_file_name(alpino(Psuite0),Psuite),
    set_flag(suite,Psuite),
    load_suite.

%% always load types + data_definitions
undefined_options :-
    set_flag(grammar,undefined),
    set_flag(suite,undefined),
    set_flag(lexicon,undefined),
    set_flag(penalties,undefined),
    set_flag(syntax_features,undefined),
    set_flag(ngram_model_dir,undefined),
    set_flag(pos_tagger,off).

test1_options :-
    set_flag(after_timeout_options,on),
    set_flag(end_hook,best_score),
    set_flag(compare_cgn,on),
    veryfast_options.

:- public no_test1_options/0.
no_test1_options :-
    set_flag(after_timeout_options,off),
    set_flag(end_hook,undefined),
    set_flag(compare_cgn,off),
    veryfast_options.    

veryfast_options :-
    fast_options,
    set_flag(use_guides,on),
    set_flag(parse_candidates_beam,1000),
    set_flag(parse_mode,veryfast).

disamb_options :-
    veryfast_options,
    set_flag(number_analyses,4),
    set_flag(list_all_maxent_features,on).

annotate_options :-
    veryfast_options,
    set_flag(pos_tagger,off),
    set_flag(use_guides,off).

fast_options :-
    set_flag(after_timeout_options,on),
    set_flag(disambiguation,on),
    set_flag(disambiguation_beam,4),
    set_flag(disambiguation_candidates_beam,0),
    set_flag(parse_candidates_beam,3000),
    set_flag(fluency_beam,4),
    set_flag(number_analyses,1),
    set_flag(unpack_bestfirst,on),
    set_flag(pos_tagger,on),   
    set_flag(use_guides,off),  
    set_flag(pos_tagger_n,-1),
    set_flag(pos_tagger_m,special),
    set_flag(parse_mode,fast).

slow_options :-
    set_flag(disambiguation,on),
    set_flag(disambiguation_beam,0),
    set_flag(disambiguation_candidates_beam,0),
    set_flag(parse_candidates_beam,0),
    set_flag(fluency_beam,0),
    set_flag(number_analyses,0),
    set_flag(unpack_bestfirst,on),
    set_flag(use_guides,off),
    set_flag(pos_tagger,off),
    set_flag(parse_mode,slow).

%% NB: veryfast is now the default!
:- veryfast_options.

:- public no_heur_options/0, annotate_options/0.
no_heur_options:-
    set_flag(pos_tagger,off),
    set_flag(use_guides,off).

testN_options :-
    set_flag(compare_cgn,off),
    set_flag(after_timeout_options,testN),
    set_flag(end_hook,best_score),
    set_flag(disambiguation,off),
    set_flag(disambiguation_beam,0),
    set_flag(disambiguation_candidates_beam,0),
    set_flag(parse_candidates_beam,0),
    set_flag(number_analyses,0),
    set_flag(unpack_bestfirst,off),
    set_flag(pos_tagger,off),
    set_flag(display_main_parts,off),
    set_flag(display_quality,off).

hdrug_command(ann,annotate_options,[]).
hdrug_command(annot,annotate_options,[]).
hdrug_command(test1,test1_options,[]).
hdrug_command(testN,testN_options,[]).
hdrug_command(veryfast,veryfast_options,[]).
hdrug_command(vf,veryfast_options,[]).
hdrug_command(very_fast,veryfast_options,[]).
hdrug_command(disamb,disamb_options,[]).
hdrug_command(fast,fast_options,[]).
hdrug_command(slow,slow_options,[]).
hdrug_command(nh,no_heur_options,[]).
hdrug_command(no_heur,no_heur_options,[]).
hdrug_command(noheur,no_heur_options,[]).

:- public parse_single_or_loop/1.  % option
parse_single_or_loop([]) :-
    parse_loop.
parse_single_or_loop([H|T]) :-
    initialize_flag(current_ref,0),
    alpino_parse_tokens(0,[H|T]).

update_line_number(Int) :-
    hdrug_flag(current_line_no,Int0),
    Int is Int0+1,
    set_flag(current_line_no,Int).

%% ignore all lines until:
:- initialize_flag(first_line_no,0).  
:- initialize_flag(parse_unannotated_only,off).

hdrug_command('**',parse_loop,[]).

parse_loop :-
    hdrug_flag(first_line_no,First),
    initialize_flag(current_line_no,0),
    hdrug_flag(parse_unannotated_only,Only),
    repeat,

    %%% reset, because apparantly this can get lost if timed outs occur????

    set_output(user_output),
    
    create_parse_prompt(Prompt),	% incorporate number in prompt
    prompt(_,Prompt),           % and redefine prompt
    
    read_line(Line0),
    (	Line0 == []
    ->	fail           % empty lines are ignored
    ;   Line0 = [37|_]
    ->  fail           % lines starting with % are treated as comments
    ;	Line0 == end_of_file
    ->	!              % end of file -> halt
    ;	Line0 == [16]
    ->	hdrug_runtime_cmd_interpreter,              % ^P -> escape to Prolog
	fail
    ;   update_line_number(Int),
	Int >= First,
        (   Line0 = [92,37|Line1]
        ->  Line= [37|Line1]
        ;   Line0 = Line
        ),
	(   append(Prefix,[124|ParseLine],Line)
	->  (   Prefix = [_|_],
		atom_codes(Ref,Prefix),
		set_flag(current_ref,Ref),
		parse_this_line(Only,Ref,Int,ParseLine),
		fail
	    ;   Prefix = [],
		set_flag(current_ref,Int),
		parse_this_line(Only,Int,Int,ParseLine)
	    )
	;   set_flag(current_ref,Int),
	    parse_this_line(Only,Int,Int,Line),
	    fail
	)
    ).

parse_this_line(Only,Ref,Int,Line) :-
    catch(parse_this_line_(Only,Ref,Int,Line),P,print_message(error,P)).

parse_this_line_(off,Ref,Int,Line) :-
    debug_message(2,"**** considering ~w (line number ~w)~n",
			     [Ref,Int]),
    debug_message(1,"**** parsing ~w (line number ~w)~n",[Ref,Int]),
    alpino_parse_line(Ref,Line),
    debug_message(1,"**** parsed ~w (line number ~w)~n",[Ref,Int]).

parse_this_line_(on,Ref,Int,Line) :-
    construct_identifier(Ref,1,Identifier),
    xml_filename(File,Identifier),
    (   file_exists(File)
    ->  true
    ;   parse_this_line_(off,Ref,Int,Line)
    ).

%% only parse a line if the corresponding xml file 
%% in the directory .. does not exist; it must be
%% the case that the current directory is called
%% 'uncorrected'
parse_this_line_(uncorrected,Ref,Int,Line) :-
    construct_identifier(Ref,1,Identifier),
    treebank_directory(Dir0),
    atom_concat(Dir,'/uncorrected',Dir0),
    format_to_chars("~w/~w.xml",[Dir,Identifier],Codes),
    atom_codes(File,Codes),
    (   file_exists(File)
    ->  true
    ;   parse_this_line_(off,Ref,Int,Line)
    ).

allow_sentence_key_for_parser(Key) :-
    hdrug_flag(parse_unannotated_only,Only),
    allow_sentence_key_for_parser(Only,Key).

allow_sentence_key_for_parser(off,_).
allow_sentence_key_for_parser(on,Ref) :-
    hdrug_flag(end_hook,best_score(xml)),
    !,
    format_to_chars("~w.xml",[Ref],Codes),
    atom_codes(File,Codes),
    \+ file_exists(File).
allow_sentence_key_for_parser(fast_best(Dir),Ref) :-
    format_to_chars("~w/~w.xml",[Dir,Ref],Codes),
    atom_codes(File,Codes),
    \+ file_exists(File),
    allow_sentence_key_for_parser(on,Ref).
allow_sentence_key_for_parser(on,Ref) :-
    construct_identifier(Ref,1,Identifier),
    xml_filename(File,Identifier),
    \+ file_exists(File).
allow_sentence_key_for_parser(uncorrected,Ref) :-
    construct_identifier(Ref,1,Identifier),
    treebank_directory(Dir0),
    atom_concat(Dir,'/uncorrected',Dir0),
    format_to_chars("~w/~w.xml",[Dir,Identifier],Codes),
    atom_codes(File,Codes),
    \+ file_exists(File).

alpino_parse_line(Ref,Line) :-
    alpino_parse_line(Ref,Line,_).

alpino_parse_line(Ref,Line,Words) :-
    codes_to_words_or_tokenize(Line,Words),
    check_length(Words),
    alpino_parse_tokens(Ref,Words).

alpino_parse_tokens(Ref,Tokens) :-
    set_flag(current_ref,Ref),
    parse(Tokens).

option(additional_lexicon) -->
    [File],
    { set_flag(additional_lexicon, File) }.

%% hdrug_hook
:- multifile usage_option/3.
usage_option(parse,"-parse W1 .. Wn~n",
"This command will parse the sentence W1 .. Wn. Without any words, the system will read lines from standard input, and parse each line.").
usage_option(d,"-d","development: load_grammar_mode=interpret").

%% hdrug_hook
%% adds some stuff to GUI
gram_startup_hook_end :-
    send_rules,
    send_frames,
    send_tops,
    hdrug_flag(tcl_dir,Dir0),
    (   Dir0 == undefined
    ->  absolute_file_name(alpino(src),Dir)
    ;   Dir = Dir0
    ),
    tcl('source ~w/alpino.tcl',[Dir]),
    hdrug_flag(annotating,Annotating),
    (    Annotating == on
    ->   tcl('
    button .t.ann_next -text {Annotate Next} -command {
		prolog $module:next_unannotated }
    button .t.ann_short -text {Annotate Shortest} -command {
		prolog $module:next_unannotated_shortest }
    button .t.ann_ignore -text {Ignore Current} -command {
		prolog $module:ignore_current_ref }

    pack .t.ann_next .t.ann_short .t.ann_ignore -side right
	    ',[])
    ;    true
    ),
    tcl('
    label .t.wit -text {} -width 5
    add_parse_widget
    extend_parse_widget

    button .t.next -text {Next} -command {prolog $module:next}
    button .t.current -text {Repeat} -command {prolog $module:current}
    button .t.prev -text {Prev} -command {prolog $module:prev}
    button .t.eval -text {Evaluate} -command {prolog $module:tree_comparison}
    button .t.bank -text {Treebank} -command {prolog $module:treebank}
    button .t.select -text {Select} -command {prolog $module:select_parse}
    button .t.selbest -text {Best} -command {prolog $module:tree_comparison_find_best}
    button .t.comp -text {Compare} -command {prolog $module:analyse_differences}
    pack .t.wit .t.next .t.current .t.prev .t.eval .t.bank .t.select .t.selbest .t.comp -side right',
       []),
    tcl('catch "destroy .t.top .t.topt .t.parsert .t.parser .t.generatort .t.generator "
         label .t.suite  -text "Parser Suite:"
         label .t.suitef -textvariable flag(suite) 
         pack .t.suite .t.suitef -side left',[]),
    tcl(' help_line .t.next {Parse the next sentence}
          help_line .t.comp {Compare all objects}
          help_line .t.current {Parse current sentence again}
          help_line .t.prev {Prev previous sentence}
          help_line .t.eval {Evaluate Results in comparison with treebank}
          help_line .t.bank {Display dependency structure in Treebank}
          help_line .t.ann_next {Annotate the next unannotated sentence of this suite}
          help_line .t.ann_short {Annotate the shortest unannotated sentence of this suite}
          help_line .t.ann_ignore {This sentence should not be annotated; a dummy xml file is saved}
          help_line .t.select {Select best parse using Parse Selection Tool}
          help_line .t.selbest {Find parse with best score} ',[]),
    select_widget(tree(_),Canvas),
    tcl('many_photo ~w 5 [ image create photo -file ~w/mattenklopper.gif ]',[Canvas,Dir]),
    select_widget(fs,Canvas2),
    tcl('many_photo ~w 5 [ image create photo -file ~w/mattenklopper.gif ]',[Canvas2,Dir]),

    menu_flag(stop_if_optimal,[on,off]),
    menu_flag(load_grammar_mode,[interpret,compiled]),
    menu_flag(robustness,[on,if_required,off]),
    menu_flag(disambiguation_beam,[0,1,2,3,4,5,6,10,25,100]),
    menu_flag(disambiguation_candidates_beam,[0,100,200,500,1000,2000,3000,5000]),
    menu_flag(parse_candidates_beam,[0,100,200,500,1000,2000,3000,5000]),
    menu_flag(number_analyses,[0,1,2,3,4,5,10,40,100,1000]),
    menu_flag(list_all_maxent_features,[on,off]),
    menu_flag(use_guides,[on,off]),
    menu_flag(keep_decided,[on,off]),
    menu_flag(filter_lexical_analysis,[on,off]),
    menu_flag(interactive_lexical_analysis,[on,off]),
    menu_flag(pos_tagger,[on,off]),
    menu_flag(pos_tagger_n,[-1,100,200,300,400,500]),
    menu_flag(pos_tagger_m,[special,1,2,3,4,5]),
    menu_flag(unpack_bestfirst,[on,off]),
    menu_flag(no_dt_unpack_all,[on,off]),
    menu_flag(disambiguation,[on,off]),
    menu_flag(robust_parts_are_independent,[on,off]),
    menu_flag(xml_format_frame,[on,off]),
    menu_flag(xml_format_failed_parse,[on,off]),

    try_hook(alpino_startup_hook_end),
    update_buttons,
    tcl('bind . <Control-n> "prolog $module:next"',[]),
    tcl('bind . <Control-b> "prolog $module:prev"',[]),
    tcl('bind . <Control-p> "prolog $module:prev"',[]),
    tcl('bind . <Control-r> "prolog $module:current"',[]),
    tcl('bind . <Control-c> "prolog $module:analyse_differences"',[]),
    tcl('bind . <Control-t> "prolog $module:show_treebank_current_ref"',[]).
    
:- set_flag(type_default(user),tree(dts)).
:- set_flag(type_default(clig),tree(user(dt))).

:- initialize_flag(xml_format_failed_parse,off).
:- initialize_flag(xml_format_frame,on).

update_buttons :-
    (   a_sentence(_,_)
    ->  tcl('.t.next configure -state normal'),
	tcl('.t.current configure -state normal'),
	tcl('.t.prev configure -state normal')
    ;   tcl('.t.next configure -state disabled'),
	tcl('.t.current configure -state disabled'),
	tcl('.t.prev configure -state disabled')
    ),
    (   object(_,_)
    ->  tcl('.t.eval configure -state normal'),
	tcl('.t.bank configure -state normal'),
	tcl('.t.select configure -state normal'),
	tcl('.t.selbest configure -state normal'),
	tcl('.t.comp configure -state normal'),
	tcl('.menu.generate.m entryconfigure "Generate object" -state normal'),
	tcl('.menu.generate.m entryconfigure "Compare Generators on object" -state normal')
    ;   tcl('.t.eval configure -state disabled'),
	tcl('.t.bank configure -state disabled'),
	tcl('.t.select configure -state disabled'),
	tcl('.t.selbest configure -state disabled'),
	tcl('.t.comp configure -state disabled'),
	tcl('.menu.generate.m entryconfigure "Generate object" -state disabled'),
	tcl('.menu.generate.m entryconfigure "Compare Generators on object" -state disabled')
    ),
    (   a_lf(_,_,_)
    ->  tcl('.menu.generate.m entryconfigure "Generate Lf" -state normal'),
	tcl('.menu.generate.m entryconfigure "Compare Generators on lf" -state normal')
    ;   tcl('.menu.generate.m entryconfigure "Generate Lf" -state disabled'),
	tcl('.menu.generate.m entryconfigure "Compare Generators on lf" -state disabled')
    ),
    (   alpino_cg:inactive_edge(_,_,_)
    ->  tcl('.menu.generate.m entryconfigure "Inactive edges" -state normal')
    ;   tcl('.menu.generate.m entryconfigure "Inactive edges" -state disabled')
    ).

    
%% hdrug_hook
:- multifile help_flag/2.

help_flag(list_all_maxent_features,
"Boolean flag which determines if the disambiguation features of a derivation are stored as part of the derivation. For efficiency reasons, the value of this flag is often off, but in order to inspect multiple derivations for a given input you may want to switch this flag on.").

help_flag(stop_if_optimal,
"Boolean flag which determines if *all* parses are supposed to be returned with option -testN, or whether it suffices to stop once a parse has been found with a 100% accuracy.").

help_flag(xml_format_frame,
"Boolean flag which determines if the full postag frame of the word should be included in xml output. ").

help_flag(xml_format_failed_parse,
"Boolean flag which determines if an XML-file should be constructed also if the parser failed (eg. due to time out, or out of memory). ").

help_flag(pos_tagger,
"Boolean flag to determine whether to use a POS-tagger to filter the result of lexical lookup. The POS-tagger is based on unigram, bigram and trigram frequencies. This filter can be made more or less strict using the pos_tagger_n flag.").

help_flag(pos_tagger_n,
"This flag takes a numerical value which determines how much filtering should be done by the POS-tagger which filters the result of lexical lookup (if pos_tagger=on). The POS-tagger is based on unigram, bigram and trigram frequencies. For each position in the string the tagger compares the combined forward and backward probability of a tag with the best score. If the score of a tag is greater than the best_score + the value of this flag, then the tag is removed. Thus, a lower value indicates that more filtering is done.").

help_flag(filter_lexical_analysis,
"Boolean flag which indicates whether during lexical analyses rules are used to attempt to reduce the number of readings for each given words. For instance, a verb with a subcategorization pattern which requires a certain particle or preposition is ignored if that particle or preposition does not occur in the string. For efficiency this flag is required to be on; however, during debugging you might set it off if you think these lexical filter rules are inconsistent with the current version of the grammar and lexicon.  ").

help_flag(interactive_lexical_analysis,
"Boolean flag which indicates whether during lexical analyses the graphical user interface will display all valid tags in order that you can then filter any of these interactively.").

help_flag(disambiguation_beam,
"Integer which determines during unpacking the how many best analyses are kept for each `maximal projection'. A larger value will imply slower and more accurate processing. The value 0 is special: in that case the system performs a full search (hence maximal accuracy and minimal speed). The value of this flag is ignored in case unpack_bestfirst=off.").

help_flag(parse_candidates_beam,
"Integer which determines during parsing (first phase) how many parses are produced for any given goal.  A value of 0 means that all parses are produced. If the value is N>0, then only the first N parses are computed.").

help_flag(disambiguation_candidates_beam,
"Integer which determines during unpacking how many parses are produced for any given maximal projection. From these, only the best are kept for further processing (using the disambiguation_beam flag). This flag can be used to limit the number of parses that are computed in the first place. A value of 0 means that all parses are produced. If the value is N>0, then only the first N parses are computed. The value of this flag is ignored in case unpack_bestfirst=off.").

help_flag(unpack_bestfirst,
"If this flag is on, then the parser attempts to unpack parse trees from the packed parse forest using a best-first search. This search takes into account various preferences defined in penalties (cf flag penalties), and may use the flags disambiguation_beam and disambiguation_candidates_beam in order to make this search more efficient. If the flag is off, on the other hand, then no attempt is made to search for the best tree, penalties are ignored, and the system simply generates parse trees in arbitrary order until the number_analyses flag determines there are enough. Note that at this point, the resulting trees will still be ordered according to the preferences in penalties (if any). Clearly, the flags 'disambiguation_beam' and 'disambiguation_candidates_beam' are ignored if this flag if off.").

help_flag(disambiguation,
"Boolean flag to determine whether the system should sort the set of results based on the scores assigned by the various preference rules (penalties). If the flag is on, then more plausible analyses are generated before less plausible ones. If the flag is off, then no preferences are obtained, and consequenlty there is no reason to sort the set of results. ").

help_flag(robust_parts_are_independent,
"Boolean flag to determine whether - for disambiguation - the parts of a sequence of analyses (considered because of robustness) are independent. In such cases, the quality of a part is not influenced by the surrounding parts. This is more efficient, since this implies that we never need to generate more than flag(number_analyses) analyses for each part. ").

help_flag(robustness,
"Flag to determine whether the system should attempt to construct a best-fitting partial analysis of the input if the input cannot be analysed as a single instance of the top category. The flag now has three values: on, off, if_required. If robustness=on, then the parser finds all maximal projections in the input, and the robustness component finds the best sequence. If robustness=off, then the system only attempts to find a single analysis from the beginning to the end. If robustness=if_required then the system first attempts to find a single analysis; if that fails it does full robustness search after all. ").

help_flag(number_analyses,
"This flag determines the number of analyses that is passed on by the robustness / disambiguation component. If the value is 0, then the system simply finds all solutions.").

help_flag(use_guides,
"This flag switches on an optimization of the parser which might result in a drop in accuracy.").


%% hdrug_hook
start_hook(parse,_,o(_,Sentence,_),Sentence):-
    hdrug_flag(current_ref,Key),
    hdrug_flag(end_hook,Th),
    set_flag(last_one_timeout,off),
    (	Th == best_score
    ->	best_score_new_parse
    ;	Th == best_score_check_tags
    ->	best_score_new_parse
    ;	Th == train_penalties
    ->	best_score_new_parse
    ;	Th = best_score(Sub)
    ->	best_score_new_parse(Sub)
    ;	true
    ),
    ensure_grammar_compiled,
    check_flags,
    try_hook(alpino_start_hook(Key,Sentence)),
    (   Th == xml
    ->  check_unannotated(Key)
    ;   true
    ),
    if_gui(tcl(update)).

start_hook(generate,_,_,_) :-
    ensure_grammar_compiled,  % !
    check_flags,
    if_gui(tcl(update)).

check_unannotated(Key) :-
    allow_sentence_key_for_parser(Key),
    !.
check_unannotated(_) :-
    raise_exception(alpino_error('This sentence is already annotated')).

check_flags :-
    hdrug_flag(end_hook,EndHook),
    (   EndHook == left_corners
    ->  set_flag(keep_notree,on)
    ;   EndHook == new_left_corners
    ->  set_flag(keep_notree,on)
    ;   true
    ),
    hdrug_flag(number_analyses,Total),
    hdrug_flag(disambiguation,Dis),
    hdrug_flag(disambiguation_beam,Beam),
    hdrug_flag(disambiguation_candidates_beam,CBeam),
    (	Beam>0,
	Dis == on,
	Beam<Total
    ->	debug_message(1,
   "Warning: disambiguation_beam (~w) < number_analyses (~w)~n",
		      [Beam,Total]),
	set_flag(disambiguation_beam,Total)
    ;   CBeam>0,
	Dis == on,
	CBeam<Total
    ->	debug_message(1,
   "Warning: disambiguation_candidates_beam (~w) < number_analyses (~w)~n",
		      [CBeam,Total]),
	set_flag(disambiguation_candidates_beam,Total)
    ;	CBeam>0,
	Dis == on,
	CBeam < Beam
    ->	debug_message(1,
   "Warning: disambiguation_candidates_beam (~w) < disambiguation_beam (~w)~n",
		      [CBeam,Beam]),
	set_flag(disambiguation_candidates_beam,Beam)
    ;	true
    ),
    hdrug_flag(end_hook,End),
    hdrug_flag(list_all_maxent_features,List),
    (   List == off
    ->  (   (  End == train_fluency
	    ;  End == train_penalties
	    ;  End == format_penalties
	    )
	->  debug_message(1,
		"Warning: setting list_all_maxent_features=on for end_hook=~w~n",
			  [End]),
	    set_flag(list_all_maxent_features,on)
	;   true
	)
    ;   true
    ).

%% hdrug_hook
start_hook0(parse,_,o(_,Wg,_),_):-
    lexical_analysis(Wg).

construct_identifier(Key,No,Identifier) :-
    hdrug_flag(number_analyses,Number),
    (   No =:= 1, Number =:= 1
    ->  Key = Identifier
    ;   Key-No = Identifier
    ).

result_hook(generate,_,o(Result,String,_),_) :-
    hdrug_flag(current_ref,Key),
    hdrug_flag(found_solutions,No),
    concat_all(String,Sent,' '),
    alpino_data:result_term(p(Score,_),_,Cat,Tree,_,Result),
    hdrug_flag(end_hook,Th),
    (   Th == train_fluency
    ->  alpino_fluency_maxent:count_maxent_features(Cat,Tree,His),
	a_sentence(Key,RefString),
	decap_list(String,String1),
	decap_list(RefString,RefString1),
	alpino_geneval:gtm(RefString1,String1,GTM),
	format("G#~w#~w#~w#",[Key,No,GTM]),
	format_counted_features(His)
%    ;   Th == derivbank
%    ->  format(user_error,"Storing result for G#~w#~w...~n",[Key,No]),
%	derivbank_write_handle(Handle),
%	prepare_derivbank_tree(Result,ResultClean),
%	derivbank_write_tree(Handle,Key,No,ResultClean)
    ;   Th == features,
	No == 1
    ->  alpino_fluency_maxent:count_maxent_features(Cat,Tree,His),
	format_counted_features(His)
    ;   Th == print_generated_sentence
    ->  format("~w~n",[Sent])
    ;   true
    ),
    hdrug_flag(geneval,On),
    geneval(On,String,Key,No,off),
    debug_message(1,"G#~w|~w|~w|~w~n",[Key,No,Sent,Score]),
    flush_output.

%% hdrug_hook
result_hook(parse,_,o(Result,String,_),Flag) :-
    alpino_data:result_term(Score,_,_,Tree,_,DT,Result),
    result_to_dt(Result,DT),
    hdrug_flag(end_hook,Th),
    hdrug_flag(current_ref,Key),
    hdrug_flag(found_solutions,No),
    hdrug_flag(display_main_parts,DMP),
    hdrug_flag(display_quality,Q),
    hdrug_flag(compare_cgn,CGN),
    thread_flag(input,StringInput),
    construct_identifier(Key,No,Identifier),
    ignore_brackets(String,StringNoBrackets),
    (   Th==xml
    ->  display_quality(Q,Tree,StringInput,Score,Identifier,QualityString),
	xml_filename(File,Identifier),
        xml_save(Result,StringNoBrackets,[QualityString],File,normal)
    ;   Th==xml_without_comments
    ->  display_quality(Q,Tree,StringInput,Score,Identifier,_QualityString),
	xml_filename(File,Identifier),
        xml_save(Result,StringNoBrackets,[],File,normal)
    ;   (  Th==xml_dump ; Th==dump_xml )
    ->  display_quality(Q,Tree,StringInput,Score,Identifier,QualityString),
        xml_save(Result,StringNoBrackets,[QualityString],stream(user_output),normal)
%    ;   Th=ldplayer
%    ->  format_thistle_script_of_result(Result,String,[])
    ;	Th==triples
    ->	format_triples_without_postags_of_result(Result,Identifier)
    ;   Th==gerlof
    ->	hook(result_to_gerlof(Result,Key,No))   % external
    ;	Th==syntax
    ->	format_syntax_of_result(Result,Identifier)
    ;	Th==some_syntax
    ->	format_some_syntax_of_result(Result,Identifier,StringNoBrackets)
    ;	Th==triples_with_postags
    ->	format_triples_with_postags_of_result(Result,Identifier)
    ;	Th==triples_with_full_postags
    ->	format_triples_with_full_postags_of_result(Result,Identifier)
    ;	Th==dependencies
    ->	format_full_triples_with_full_postags_of_result(Result,
                                                        Identifier,StringNoBrackets)
    ;	Th==triples_with_frames
    ->	format_triples_with_postags_of_result(Result,Identifier)
    ;   Th==deriv
    ->  format_deriv_of_result(Result,Identifier)
    ;   Th==nderiv
    ->  format_nderiv_of_result(Result,Identifier)
    ;	Th==frames
    ->	format_frames_of_result(Result,Identifier)
    ;	Th==postags
    ->	format_postags_of_result(Result,String,Identifier)
    ;	Th==pts
    ->	format_pts_of_result(Result,String,Identifier)
    ;	Th==best_score
    ->	best_score_result_parse(Result,StringNoBrackets,No)
    ;	Th==best_score_check_tags
    ->	best_score_result_parse(Result,StringNoBrackets,No)
    ;	Th=best_score(_)
    ->	best_score_result_parse(Result,StringNoBrackets,No)
    ;	Th==train_penalties
    ->	format_score_with_penalties(Result)
    ;	Th==format_penalties
    ->	format_penalties(Result)
    ;   Th==left_corners
    ->  format_left_corners_of_result(Result,Identifier)
    ;   Th==new_left_corners
    ->  format_new_left_corners_of_result(Result,Identifier)
    ;   Th==gen_suite
    ->  format_gen_suite(Result,Identifier)
    ;   Th==adt_prolog
    ->  format_gen_suite(Result,Identifier)
    ;   Th==adt_xml
    ->  adt_xml_filename(File,Identifier),
	xml_save_adt(Result,File)
    ;   Th==compare_cgn
    ->  xml_filename(File,Key),
	compare_treebank_cgn_result(File,Result,Key)
    ;   Th==dep_features
    ->  alpino_treebank:format_dep_features(Result,Identifier)
    ;   Th==demo
    ->  show(tree(dt),user,[value(Result)])
    ;   true
    ),

    (   CGN == on
    ->  xml_filename(File,Key),
	compare_treebank_cgn_result(File,Result,Key)
    ;   true
    ),

    (	DMP == on
    ->	display_main_parts(user_error,Tree),
	nl(user_error)
    ;	true
    ),
    (   Q == on
    ->  (   var(QualityString)  % perhaps already done this, above
	->  display_quality(on,Tree,StringInput,Score,Identifier,QualityString)
	;   true
	),
	format(user_error,"~s~n",[QualityString])
    ;   true
    ),
    try_hook(alpino_result_hook(Key,No,Result,Flag)),
    flush_output.

display_quality(off,_,_,_,_,"").
display_quality(on,tree(robust,robust,List,_),String,p(Real,_),Key,Codes) :-
    concat_all(String,StringAtom,' '),
    alpino_format_syntax:escape_b(StringAtom,StringString),
    count_cats_and_skips(List,0,RobustLength),
    with_output_to_chars(
         format("Q#~w|~s|1|~w|~w",
		[Key,StringString,RobustLength,Real]),Codes).

%% like length, but ignore certain skips
count_cats_and_skips([],P,P).
count_cats_and_skips([tree(skip,robust_skips(W),_,_)|T],P0,P) :-
    !,
    count_skips(W,P0,P1),
    count_cats_and_skips(T,P1,P).
count_cats_and_skips([tree(_,_,_,_)|T],P0,P) :-
    P1 is P0+1,
    count_cats_and_skips(T,P1,P).

count_skips(W,P0,P) :-
    (    alpino_lexical_analysis:tag(_,_,_,_,_,W,_,punct(_))
    ->   P is P0
    ;    P is P0 + 1
    ).

%% hdrug_hook
end_hook(parse,_,_,String) :-
    hdrug_flag(demo,Demo),
    hdrug_flag(end_hook,Th),
    hdrug_flag(found_solutions,NumberOfSolutions),
    hdrug_flag(display_quality,Q),
    hdrug_flag(current_ref,Key),
    hdrug_flag(hdrug_status,Status0),
    hdrug_flag(last_one_timeout,LastOneTimeOut),
    ignore_brackets(String,StringNoBrackets),
    (   LastOneTimeOut == on
    ->  Status = time_out/Status0
    ;   Status = Status0
    ),

    (	Th==best_score
    ->	best_score_end_parse(No)
    ;	Th==best_score_check_tags
     
    ->	hdrug_flag(best_score_current,val(Score,_,_,_,_)),
	(   hdrug_flag(pos_tagger,on)
	->  (   Score < 100
	    ->  format(user_error,"TAGGER ~2f score with tagger, checking tags...~n",[Score]),
		check_tags(Score)
	    ;   format(user_error,"TAGGER ~2f score with tagger, no need to check~n",[100.0])
	    )
	;   true
	)
    ;	Th=best_score(Sub)
    ->	best_score_end_parse(No),
	best_score_end_parse_sub(Sub)
    ;   Th==adt
    ->  object(1,o(Result,_,_)),
	alpino_adt:result_to_adt(Result,Adt),
	retractall(alpino_gen_suite:lf(Key,_)),
	assertz(alpino_gen_suite:lf(Key,Adt))
    ),
    (   Demo==on,
	Th==best_score
    ->  object(No,o(Result,_,_)),
	show_defaults(Format,Output),
	if_gui(show(Format,Output,[value(Result)])),
	notify_active_obj(No)	       
    ;	Demo==on,
	object(1,o(Result,_,_))
    ->	show_defaults(Format,Output),
        if_gui((show(Format,Output,[value(Result)]),
		notify_active_obj(1))
%	       show(tree(dt),user,[value(Result)])
	      )
    ;	true
    ),
    (   Q==on,
	NumberOfSolutions < 1
    ->  concat_all(StringNoBrackets,StringAtom,' '),
	alpino_format_syntax:escape_b(StringAtom,StringString),
	format(user_error,"Q#~w|~s|~w|-1|-1~n",[Key,StringString,Status])
    ;   true
    ),
    (   NumberOfSolutions < 1,
        Th == xml,
        hdrug_flag(xml_format_failed_parse,on)
    ->  format_to_chars("~w",[Status],StatusChars),
        ignore_current_ref(Key,StringNoBrackets,[StatusChars])
    ;   true
    ),
    (   NumberOfSolutions < 1,
        ( Th == xml_dump ; Th == dump_xml )
    ->  format_to_chars("~w",[Status],StatusChars),
        ignore_current_ref_dump(Key,StringNoBrackets,[StatusChars])
    ;   true
    ),
    try_hook(alpino_end_hook(Key,StringNoBrackets,Status,NumberOfSolutions)),
    % used by Gosse a.o.
    if_gui(update_buttons),
    flush_output.    

%% hdrug_hook
end_hook(generate,_A,_B,_C) :-
    hdrug_flag(current_ref,Key),
    hdrug_flag(demo,Demo),
    hdrug_flag(hdrug_status,Status),
    hdrug_flag(geneval,Geneval),
    hdrug_flag(found_solutions,NumberOfSolutions),
    hdrug_flag(end_hook,Th),
    (	Demo==on,
	object(1,o(Result,_,_))
    ->	if_gui((show(tree(syn),clig,[value(Result)]),
		notify_active_obj(1))
%	       show(tree(syn),user,[value(Result)])
	      )
    ;	true
    ),
    (   Geneval == on,
        Status \== success,
        NumberOfSolutions < 1   % maybe time-out after generating N sols
    ->  format(user_error,
               "K#~w|~w|~3f|rougeN|~3f|rougeL|~3f|rougeS|~3f|rougeSu~n",
               [Key,0,0,0,0,0])
    ;   true
    ),
    (   NumberOfSolutions < 1,
	Th == print_generated_sentence
    ->  format("~n",[])    % always produce a line of output for each input
    ;   true
    ),
    if_gui(update_buttons),
    flush_output.

%% hdrug_hook
%% goal: replace numbers by atoms

extern_phon(L0,L) :-
    nonvar(L0),
    L0 = [],
    !,
    format(user_error,"sentence: ",[]),
    ttyflush,    
    read_line(Codes),
    alpino_util:codes_to_words(Codes,Words),
    (   Words == []
    ->  raise_exception(alpino_error('Cowardly refusing to parse an empty sentence'))
    ;   extern_phon(Words,L)
    ).
extern_phon(L0,L) :-
    nonvar(L0),
    replace_numbers_by_atoms(L0,L1), % also removes spaces
    adapt_input(L1,L2),
    set_thread_flag(input,L2),
    alpino_lexical_analysis:extract_skip_positions(L2,L,[],0,UserSkips),
    retractall(alpino_lexical_analysis:user_skips(_)),
    assertz(alpino_lexical_analysis:user_skips(UserSkips)).

adapt_input(['Volgens',het,'United','States','Census','Bureau',beslaat,de,plaats,een,oppervlakte,van],
	    ['Volgens',het,'United','States','Census','Bureau',beslaat,de,plaats,een,oppervlakte,van,'[','@phantom',tonnen,']']) :-
    !.
adapt_input(X,X).

replace_numbers_by_atoms([],[]).
replace_numbers_by_atoms([H0|T0],Result) :-
    (   number(H0)
    ->  number_codes(H0,Codes),
	atom_codes(H,Codes),
	Result=[H|T]
    ;   H0 == ''
    ->  Result = T
    ;   Result = [H0|T]
    ),
    replace_numbers_by_atoms(T0,T).

%% hdrug_hook
display_extern_phon(List0) :-
    replace_numbers_by_atoms(List0,List),
    write_list(List,user_error),nl(user_error),
    if_gui(add_string_to_pp(List)).
%%    too confusing, and actually wrong, if key is given explicitly
%%    try_to_set_current_ref(List).

add_string_to_pp(List) :-
    concat_all(List,Atom,' '),
    tcl('set pp_answer {~a} ; update',[Atom]),
    tcl('set last_cmd_no [expr $last_cmd_no+1]
	 set current_cmd_no $last_cmd_no
	 set pp_answer_history($last_cmd_no) $pp_answer',[]).

%% hdrug_hook
semantics(Result,Sem) :-
    nonvar(Result),
    alpino_adt:result_to_adt(Result,Sem).

phonology(Result,Phon) :-
    nonvar(Result),
    alpino_data:result(Result,_,Phon).

%% hdrug_hook
%% has this any purpose?
display_extern_sem(_).

display_main_parts(Stream,tree(_,_,Ds,_)):-
    display_tree_ds(Ds,Format,[]),
    format_tree_ds(Format,0,Stream).

display_tree_ds([]) --> [].
display_tree_ds([H|T]) -->
    ['['],
    [W], { leaves(H,W,[]) },
    [']'],
    display_tree_ds(T).


leaves_his(skip(_,SkipsL,SkipsR,_),W) -->
    !,
    leaves_his_(SkipsL),
    [W],
    leaves_his_(SkipsR).
leaves_his(_,W) --> [W].

leaves_his_(L,P0,P):-
    append(L,P,P0).

leaves_ds([]) --> [].
leaves_ds(lex(ref(_,_,_,W,_,_,_,_,His,_,_))) -->
    !,
    leaves_his(His,W).
leaves_ds(lex(W)) -->
    [W].
leaves_ds([H|T]) -->
    leaves(H),
    leaves_ds(T).

leaves(notree(Ws),Ds0,Ds) :-
    append(Ws,Ds,Ds0).
leaves(tree(_,_,Ds,_)) -->
    leaves_ds(Ds).

format_tree_ds([],_,_).
format_tree_ds([H|T],Space0,S):-
    format_tree_d(H,Space0,Space,S),
    format_tree_ds(T,Space,S).

format_tree_d(H,Space0,Space,S):-
    (	atomic(H)
    ->  format(S,"~a",[H]), Space=0
    ;   H=[_|_]
    ->	tab(S,Space0), write_list(H,S), Space=1
    ;   tab(S,Space0), write(S,H), Space=1
    ).

:- if(current_prolog_flag(language,sicstus)).
:- multifile portray_message/2.
portray_message(warning,no_match(abolish(_))).
:- endif.

:- public flags/0.  % ?
flags :-
    (	hdrug_flag(A,V),
	format(user_output,"%% ~w=~w~n",[A,V]),
	fail
    ;   true
    ).

:- public parse_with_tags/1.
parse_with_tags(Sent) :-
    hdrug_flag(interactive_lexical_analysis,Prev,current),
    call_cleanup(parse(Sent),set_flag(interactive_lexical_analysis,Prev)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PARSE next/previous/current Ref %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public sen/1, sen/2, sens/1, sens/0.


hdrug_command(sens,sens,[]).
hdrug_command(sen,sens([H|T]),[H|T]).
hdrug_command(n,sens([H|T]),[H|T]).

sens :-
    findall(K,a_sentence_with_map(_,K,_),Ks),
    sens(Ks).

sen(I0,I) :-
    integer(I0),
    integer(I),
    findall(Key,between(I0,I,Key),Keys),
    sens(Keys).

sens([]).
sens([Key|Keys]) :-
    sen(Key),
    sens(Keys).

sen(Key0) :-
    a_sentence_with_map(Key0,Key,Sent),
    if_gui(tcl('set pp_nr {~w}',[Key])),
    set_flag(current_ref,Key),
    parse(Sent).

hdrug_command_help(sens,"sens","to parse all test sentences").

hdrug_command_help(sen,"sen Key","to parse test sentence with key Key").

hdrug_command(prev,prev,[]).
hdrug_command(back,prev,[]).
hdrug_command(b,prev,[]).
hdrug_command(next,next,[]).
hdrug_command(n,next,[]).
hdrug_command(repeat,current,[]).
hdrug_command(r,current,[]).
hdrug_command(sen,current,[]).

hdrug_command(gen,generate(LF),[ID]) :-
    a_lf(ID,LF),
    set_flag(current_ref,ID).

hdrug_command(gen,generate(LF),[ID]) :-
    number(ID),
    number_codes(ID,IDCodes),
    atom_codes(ID2,IDCodes),
    a_lf(ID2,LF),
    set_flag(current_ref,ID2).

hdrug_command(gen,generate(LF),[]) :-
    hdrug_flag(current_ref,ID),
    a_lf(ID,LF).


:- public gen/1.
gen(ID) :-
    a_lf(ID,LF),
    set_flag(current_ref,ID),
    generate(LF).

gen(ID) :-
    number(ID),
    number_codes(ID,IDCodes),
    atom_codes(ID2,IDCodes),
    a_lf(ID2,LF),
    set_flag(current_ref,ID2),
    generate(LF).


hdrug_command_help(gen,"gen Key","to generate test lf with key Key").

hdrug_command(gnext,gnext,[]).

:- public next/0, prev/0, current/0, gnext/0.

next :-
    hdrug_flag(current_ref,Key0),
    find_next_ref(Key0,Key,Sent),
    hdrug_gui:to_tk_sentence(Sent,TkSent),
    set_flag(current_ref,Key),
    if_gui(tcl('set pp_answer {~w}; set pp_nr {~w}',[TkSent,Key])),
    a_sentence(Key,Sent),
    parse(Sent).

gnext :-
    hdrug_flag(current_ref,Key0),
    find_gnext_ref(Key0,Key,Sent),
    set_flag(current_ref,Key),
    a_lf(Key,Sent),
    generate(Sent).

find_prev_ref(Key0,Key,Sentence) :-
    findall(K-S,a_sentence_with_map(_,K,S),Ks),
    (	Key0 == undefined
    ->	last(Ks,Key-Sentence)
    ;	find_prev(Ks,Key0,Key,Sentence)
    ).

find_prev([Hk-Hs|T],Key0,Key,Sentence) :-
    (	Hk==Key0
    ->	last(T,Key-Sentence)
    ;	find_prev(T,Key0,Key,Sentence,Hk-Hs)
    ).

find_prev([K1-S1|T],Key0,K,S,Next) :-
    (	K1==Key0
    ->	K-S = Next
    ;	find_prev(T,Key0,K,S,K1-S1)
    ).

find_next_ref(Key0,Key,Sentence) :-
    findall(K-S,a_sentence_with_map(_,K,S),Ks),
    (	Key0 == undefined
    ->	Ks = [Key-Sentence|_]
    ;	find_next(Ks,Key0,Key,Sentence)
    ).

find_current_ref(Key0,Key,Sentence) :-
    findall(K-S,a_sentence_with_map(_,K,S),Ks),
    (	Key0 == undefined
    ->	Ks=[Key-Sentence|_]
    ;	Key0=Key,
	a_sentence(Key,Sentence)
    ).

find_gnext_ref(Key0,Key,Sentence) :-
    findall(K-S,a_lf(K,S),Ks),
    (	Key0 == undefined
    ->	Ks = [Key-Sentence|_]
    ;	find_next(Ks,Key0,Key,Sentence)
    ).

find_next([H|T],K0,K,Sent) :-
    find_next([H|T],K0,K,Sent,H).

find_next([K0-_|T],K0,K,Sent,H) :-
    !,
    take_next(T,H,K,Sent).
find_next([_|T],K0,K,Sent,H) :-
    find_next(T,K0,K,Sent,H).

take_next([],K-S,K,S).
take_next([K-S|_],_,K,S).
    
prev :-
    hdrug_flag(current_ref,Key0),
    find_prev_ref(Key0,Key,Sent),
    hdrug_gui:to_tk_sentence(Sent,TkSent),
    set_flag(current_ref,Key),
    if_gui(tcl('set pp_answer {~w}; set pp_nr {~w}',[TkSent,Key])),
    a_sentence(Key,Sent),
    parse(Sent).

current :-
    hdrug_flag(current_ref,Key0),
    find_current_ref(Key0,Key,Sent),
    set_flag(current_ref,Key),
    hdrug_gui:to_tk_sentence(Sent,TkSent),
    if_gui(tcl('set pp_answer {~w}; set pp_nr {~w}',[TkSent,Key])),
    parse(Sent).

:- public next_unannotated/0, next_unannotated_sentence_key/0,
          next_unannotated_shortest/0.				% tcltk
next_unannotated :-
    next_unannotated_key(_,Sent),
    !,
    parse(Sent).

next_unannotated_key(Key,Sent) :-
    if(next_unannotated_key0(Key,Sent),
       true,
       next_unannotated_key_finished
      ).

next_unannotated_key0(Key,Sent) :-
    treebank_directory(Dir),
    a_sentence(Key,Sent),
    is_unannotated_key(Key,Dir),
    check_identical_sentence(Key,Sent,Dir).

next_unannotated_key_finished :-
    if_gui(gui_dialog("No more unannotated sentences in this suite!!",[]),
	   format(user_error,"No more unannotated sentences in this suite!!~n",[])
	  ),
    fail.

next_unannotated_shortest :-
    findall(Len-l(Key,Sent),( next_unannotated_key(Key,Sent),
			      length(Sent,Len)
			    ), List0),
    keysort(List0,[_-l(Key,Sent)|_]),
    parse(Sent).

check_identical_sentence(Key,Sent,Dir) :-
    a_sentence(Key2,Sent),
    Key \= Key2,
    is_annotated(Key2),
    !,
    format(user_error,"cp ~w.xml ~w.xml~n",[Key2,Key]),
    format_to_chars("cp ~w/~w.xml ~w/~w.xml",[Dir,Key2,Dir,Key],Chars),
    atom_codes(Cmd,Chars),
    system(Cmd),
    fail.
check_identical_sentence(_,_,_).

%% memo the keys that *do* have an annotation. This assumes we're only dynamically
%% adding trees, but not removing them.
:- dynamic
    is_annotated/1.

is_unannotated_key(Key,_) :-
    is_annotated(Key),
    !,
    fail.

is_unannotated_key(Key,Dir) :-
    format_to_chars("~w/~w.xml",[Dir,Key],Chars),
    atom_codes(File,Chars),
    (   file_exists(File)
    ->  assertz(is_annotated(Key)),
        fail
    ;   true
    ).

next_unannotated_sentence_key :-
    next_unannotated_key(Key,Sent),
    hdrug_gui:to_tk_sentence(Sent,TkSent),
    set_flag(current_ref,Key),
    tcl("set pp_answer {~w}; set pp_nr {~w}",[TkSent,Key]).

:- public put_sentence_key/1, put_sentence_key_and_parse/1. % tcltk
put_sentence_key(Key) :-
    set_flag(current_ref,Key),
    a_sentence(Key,Sent),
    hdrug_gui:to_tk_sentence(Sent,TkSent),
    tcl("set pp_answer {~w}",[TkSent]).

put_sentence_key_and_parse(Key) :-
    a_sentence(Key,Sent),
    hdrug_gui:to_tk_sentence(Sent,TkSent),
    tcl("set pp_answer {~w}",[TkSent]),
    parse(Sent).

:- public create_object_hook/2.

create_object_hook(Menu,No) :-
    tcl("~a add cascade -label Xml -menu ~a.xml",[Menu,Menu]),
    tcl("menu ~a.xml",[Menu]),
    tcl("~a.xml add command -label Save -command {\
             prolog $module:xml_save_object(~w) }",[Menu,No]),
    tcl("~a.xml add command -label SaveAs -command {\
             set filename [tk_getSaveFile];\
             if {\"$filename\" != \"\"} { \
                prolog $module:xml_save_object(~w,'$filename')\
                }\
             }",[Menu,No]),
    tcl("~a.xml add command -label Dump -command {\
             prolog $module:xml_save_object(~w,stream(user_output)) }",[Menu,No]),
    tcl("~a.xml add command -label SaveAndEdit -command {\
             prolog $module:xml_save_and_edit_object(~w) }",[Menu,No]),
    tcl("~a.xml add command -label SaveAsAndEdit -command {\
             set filename [tk_getSaveFile];\
             if {\"$filename\" != \"\"} { \
                prolog $module:xml_save_and_edit_object(~w,'$filename')\
                }\
             }",[Menu,No]),
    tcl("~a.xml add command -label ViewTreebank -command {\
             prolog $module:treebank }",[Menu]),
    tcl("~a.xml add command -label EditTreebank -command {\
             prolog $module:dtedit }",[Menu]),
    tcl("~a add cascade -label {Xml (ADT)} -menu ~a.xmladt",[Menu,Menu]),
    tcl("menu ~a.xmladt",[Menu]),
    tcl("~a.xmladt add command -label Dump -command {\
       prolog $module:xml_save_object_adt(~w,stream(user_output)) }",[Menu,No]),
    %%tcl("~a.xmladt add command -label Save -command {\
    %%   prolog $module:xml_save_object_adt(~w) }",[Menu,No]),
    tcl("~a.xmladt add command -label SaveAs -command {\
       set filename [tk_getSaveFile];\
       if {\"$filename\" != \"\"} { \
       prolog $module:xml_save_object_adt(~w,'$filename')\
       }\
       }",[Menu,No]),
    tcl("~a add command -label DependencyTriples -command {\
             prolog $module:format_triples_of_obj(~w)}",[Menu,No]),
    tcl("~a add command -label Frames -command {\
             prolog $module:format_frames_of_obj(~w)}",[Menu,No]),
    tcl("~a add command -label MaxentFeatures -command {\
             prolog $module:display_penalties_of_obj(~w)}",[Menu,No]),
    tcl("~a add command -label SyntaxTree -command {\
             prolog $module:format_syntax_of_obj(~w)}",[Menu,No]),
    tcl("~a add command -label DerivationTree -command {\
             prolog $module:format_deriv_of_obj(~w)}",[Menu,No]),
    tcl("~a add command -label NewDerivationTree -command {\
             prolog $module:format_nderiv_of_obj(~w)}",[Menu,No]),
    tcl("~a add command -label Evaluate -command {\
             prolog $module:tree_comparison(~w)}",[Menu,No]).

%% Hdrug hook
catch_print_error(_,V,_):-
       write(V).

:- public ignore_current_ref/0.

%% save "empty" xml of current sentence 
ignore_current_ref :-
    create_empty_object(o(Result,String,_)),
    xml_filename(File),
    (	system:file_exists(File)
    ->	(   user_confirmation(
			"File ~w exists. Do you want to overwrite? ",[File])
	->  alpino_treebank:xml_save(Result,String,["ignore"],File,ignore)
	;   true
	)
    ;	alpino_treebank:xml_save(Result,String,["ignore"],File,ignore)
    ).

create_empty_object(o(Result,String,_)) :-
    hdrug_flag(current_ref,Key),
    alpino_data:result_term(p(0.0,[]),String,robust([]),tree(robust,robust,[],_),Frames,Result),
    a_sentence(Key,String),
    default_frames(String,Frames).

ignore_current_ref(Key,String,Comments) :-
    xml_filename(File,Key),
    alpino_data:result_term(p(0.0,[]),String,robust([]),tree(robust,robust,[],_),Frames,Result),
    default_frames(String,Frames),
    (	system:file_exists(File)
    ->	(   user_confirmation(
			"File ~w exists. Do you want to overwrite? ",[File])
	->  alpino_treebank:xml_save(Result,String,Comments,File,ignore)
	;   true
	)
    ;	alpino_treebank:xml_save(Result,String,Comments,File,ignore)
    ).

ignore_current_ref_dump(_Key,String,Comments) :-
    default_frames(String,Frames),
    alpino_data:result_term(p(0.0,[]),String,robust([]),tree(robust,robust,[],_),Frames,Result),
    alpino_treebank:xml_save(Result,String,Comments,stream(user_output),ignore).

hdrug_command(dt,compile_grammar_dt,[]).
hdrug_command(no_dt,compile_grammar_no_dt,[]).
hdrug_command(nodt,compile_grammar_no_dt,[]).

hdrug_command(suggest_link_weakenings,alpino_lc:suggest_link_weakenings,[]).

hdrug_command(nl,list_next,[]).
hdrug_command(bl,list_prev,[]).
hdrug_command(pl,list_prev,[]).

:- public compile_grammar_no_dt/0, compile_grammar_dt/0.

compile_grammar_no_dt :-
    set_flag(no_dt_unpack_all,on),
    set_flag(unpack_bestfirst,off),
    set_flag(debug,2),
    (   interpret_grammar,
        fail
    ;   true
    ).

compile_grammar_dt :-
    set_flag(unpack_bestfirst,on),
    set_flag(no_dt_unpack_all,off),
    set_flag(debug,1),
    (   interpret_grammar,
        fail
    ;   true
    ).

:- public list_next/0, list_prev/0.

list_next :-
    hdrug_flag(current_ref,Key0),
    find_next_ref(Key0,Key,_Sent),
    set_flag(current_ref,Key),
    list_sentences([Key]).

list_prev :-
    hdrug_flag(current_ref,Key0),
    find_prev_ref(Key0,Key,_Sent),
    set_flag(current_ref,Key),
    list_sentences([Key]).

:- public named_sentences/0.
named_sentences :-
    (   a_sentence(Key,_,Sent),
        format("~w|",[Key]),
        write_list(Sent,user_output),nl(user_output),
        fail
    ;   true
    ).

hdrug_command(gt,generate_from_treebank(FileXml),[File]) :-
    set_flag(current_ref,File),
    treebank_directory(Dir),
    format_to_chars('~w/~w.xml',[Dir,File],Chars),
    atom_codes(FileXml,Chars).

hdrug_command(gt,generate_from_treebank(FileXml),[]) :-
    hdrug_flag(current_ref,File),
    treebank_directory(Dir),
    format_to_chars('~w/~w.xml',[Dir,File],Chars),
    atom_codes(FileXml,Chars).

hdrug_command(ga,generate_from_xml_adt(File),[File]).
hdrug_command(ga,generate_from_xml_adt(File),[]) :-
    hdrug_flag(current_ref,File),
    File \= undefined.

:- public generate_from_treebank/1.
generate_from_treebank(File) :-
    alpino_treebank:xml_file_to_dt(File,DT),
    alpino_adt:dt_to_adt(DT,ADT),
    generate(ADT).

:- public dt2adt_xml/2.
dt2adt_xml(File0,File) :-
    alpino_treebank:xml_file_to_dt(File0,DT),
    xml_save_adt(already_dt(DT),File).

option(dt2adt_xml) -->
    [File0],
    [File1],
    { set_flag(batch_command,dt2adt_xml(File0,File1)) }.


:- public generate_from_xml_adt/1.
generate_from_xml_adt(File) :-
    set_flag(current_ref,File),
    alpino_treebank:xml_file_to_adt(File,ADT),
    generate(ADT).

decap_list([],[]).
decap_list([H|T],[NewH|NewT]) :-
    alpino_unknowns:decap(H,NewH),
    decap_list(T,NewT).

geneval(off,_String,_Key,_No,_Decap).
geneval(on,String0,Key,No,Decap) :-
    (   a_sentence(Key,RefString0)
    ->  (   Decap == on
	->  decap_list(String0,String),
	    decap_list(RefString0,RefString)
	;   String = String0,
	    RefString = RefString0
	),
	alpino_geneval:gtm(RefString,String,GTM),
	alpino_geneval:rouge_n(RefString,String,2,RougeN2),
        alpino_geneval:rouge_l(RefString,String,RougeL),
        alpino_geneval:rouge_s(RefString,String,RougeS),
        alpino_geneval:rouge_su(RefString,String,RougeSu),
        format(user_error,"K#~w|~w|~3f|GTM|~3f|rougeN|~3f|rougeL|~3f|rougeS|~3f|rougeSu~n",
               [Key,No,GTM,RougeN2,RougeL,RougeS,RougeSu])
    ;   true
    ).

:- public list_data_predicates/0.

%% to be used in hooks.pl
list_data_predicates :-
    (   alpino_data:current_predicate(_,X),
        format("alpino_data:~q.~n",[X]),
        fail
    ;   true
    ).

load_standard_data :-
    load_types,
    load_grammar,
    load_lexicon.

alpino_compile_grammar:notify_hdrug_grammar :-
    if_gui((send_rules,
	    send_frames,
	    send_tops
	   )).

load_parametric_data :-
    debug_message(3,"loading suite..~n",[]),
    load_suite,
    debug_message(3,"loading suite done~n",[]),
    debug_message(3,"loading penalties..~n",[]),
    load_penalties,
    debug_message(3,"loading penalties done~n",[]),
    debug_message(3,"loading fluency..~n",[]),
    alpino_cg:load_fluency,
    debug_message(3,"loading fluency done~n",[]).

:- load_standard_data.

:- load_parametric_data.

ignore_brackets(S0,S):-
    ignore_brackets_first(S0,S1),
    alpino_lexical_analysis:user_skips(List),
    ignore_phantoms(List,S1,S).

ignore_phantoms([],S,S).
ignore_phantoms([H|T],S0,S) :-
    ignore_phantoms(S0,S,0,[H|T]).

ignore_phantoms([],[],_,_).
ignore_phantoms([H|T0],Rest,P0,Skips) :-
    (   lists:member(phantom_skip(P0),Skips)
    ->  Rest = Result
    ;   Rest = [H|Result]
    ),
    P1 is P0 + 1,
    ignore_phantoms(T0,Result,P1,Skips).

ignore_brackets_first([],[]).
ignore_brackets_first([H|T0],T) :-
    ignore_bracket(H,T0,T).

ignore_bracket(Atom,T0,[Atom|T]) :-
    atom(Atom),
    ignore_brackets_first(T0,T).
ignore_bracket(bracket(open),T0,T) :-
    ignore_bracket_type(T0,T).
ignore_bracket(bracket(close),T0,T) :-
    ignore_brackets_first(T0,T).

ignore_bracket_type([H|T0],T) :-
    atom(H),
    atom_codes(H,[C|_String]),
    C =:= 64,
    !,
    ignore_brackets_first(T0,T).
ignore_bracket_type(T0,T) :-
    ignore_brackets_first(T0,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saving to derivation treebanks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prepare_derivbank_tree(Result0,Result) :-
%     alpino_data:result_term(_Sc,Sentence,Cat,Tree0,_Frames,Result0),
%     clean_tree_cache(Tree0,Tree),
%     alpino_data:result_term(_,Sentence,Cat,Tree,_,Result).

% %% Clean cached fluency ranking/disambiguation features.
% clean_tree_cache(tree(A,B,Ds,_),tree(A,B,NewDs,_)) :-
%     clean_tree_cache_ds(Ds,NewDs).

% clean_tree_cache_ds([],[]).
% clean_tree_cache_ds(lex(Ref),lex(Ref)).
% clean_tree_cache_ds([H|T],[NewH|NewT]) :-
%     clean_tree_cache(H,NewH),
%     clean_tree_cache_ds(T,NewT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extraction from derivation treebanks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- public extract_fluency_features/0.
% extract_fluency_features :-
%     derivbank_read_handle(H),
%     derivbank_entry(H,Key,N),
%     extract_fluency_features_aux(H,Key,N),
%     fail.
% extract_fluency_features.

% extract_fluency_features_aux(H,Key,N) :-
%     derivbank_read_tree(H,Key,N,Result),
%     alpino_data:result_term(_,Sentence,Cat,Tree,_,Result),
%     alpino_fluency_maxent:count_maxent_features(Cat,Tree,His),
%     a_sentence(Key,RefString),
%     decap_list(Sentence,String1),
%     decap_list(RefString,RefString1),
%     alpino_geneval:gtm(RefString1,String1,Score), %% for now...
%     format("~w#~w#~w#",[Key,N,Score]),
%     format_counted_features(His),
%     !.

% :- public extract_realizations/0.
% extract_realizations :-
%     derivbank_read_handle(H),
%     derivbank_entry(H,Key,N),
%     extract_realizations_aux(H,Key,N),
%     fail.
% extract_realizations.

% extract_realizations_aux(H,Key,N) :-
%     derivbank_read_tree(H,Key,N,Result),
%     alpino_data:result_term(_,String,_,_,_,Result),
%     concat_all(String,Sentence,' '),
%     a_sentence(Key,RefString),
%     decap_list(String,String1),
%     decap_list(RefString,RefString1),
%     alpino_geneval:gtm(RefString1,String1,Score),
%     format("G#~w|~w|~w|~w~n",[Key,N,Score,Sentence]),
%     !.

%% options and commands from treebank.pl

option(check_suites,Files,[]) :-
    set_flag(batch_command,alpino_treebank:check_suites(Files)).

option(check_suite) -->
    {
    set_flag(grammar,undefined),
    set_flag(lexicon,undefined),
    set_flag(penalties,undefined),
    set_flag(pos_tagger,off),
    set_flag(batch_command,
               alpino_treebank:check_suite_and_treebank_are_consistent)
    }.

option(treebank_triples,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_triples_list(Files)).

option(treebank_dep_features,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_dep_features_list(Files)).

option(treebank_dep_features_with_file,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_dep_features_with_file_list(Files)).

option(treebank_full_triples,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_full_triples_list(Files)).

option(treebank_lassy_triples,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_lassy_triples_list(Files)).

option(treebank_pl_triples,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_pl_triples_list(Files)).

option(treebank_mwu_roots,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:treebank_mwu_roots_list(Files)).

option(xml2xml,[File0,File],[]) :-
    set_flag(batch_command,
	     alpino_treebank:xml_file_to_xml_file(File0,File)).

:- initialize_flag(user_transformation,alpino('src/user_transformation')).

option(xml_converse,Files,[]) :-
    hdrug_flag(user_transformation,FILE),
    use_module(FILE),
    set_flag(batch_command,
	     alpino_treebank:converse_xml_files(Files)).

option(xml_canonical_overwrite,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:canonical_xml_overwrite_files(Files)).

option(xml_canonical,[File],[]) :-
    set_flag(batch_command,
	     alpino_treebank:canonical_xml_file(File)).

option(empty_xml,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:empty_xml_files(Files)).

option(xml_empty,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:empty_xml_files(Files)).

option(converse_xml,Files,[]) :-
    hdrug_flag(user_transformation,FILE),
    use_module(FILE),
    set_flag(batch_command,
	     alpino_treebank:converse_xml_files(Files)).

option(canonical_xml,Files,[]) :-
    set_flag(batch_command,
	     alpino_treebank:canonical_xml_overwrite_files(Files)).


hdrug_command(save,xml_save_object(Obj),[Obj]).
hdrug_command(save,xml_save_object(1),[]).

hdrug_command(dump,xml_save_object(Obj,stream(user_output)),[Obj]).
hdrug_command(dump,xml_save_object(1,stream(user_output)),[]).
hdrug_command(dump,xml_save_object(Obj,File),[Obj,File]).

hdrug_command(edit,xml_save_and_edit_object(Obj),[Obj]).
hdrug_command(edit,xml_save_and_edit_object(1),[]).
hdrug_command(edit,xml_save_and_edit_object(Obj,File),[Obj,File]).

hdrug_command(dtview,treebank(Key),[Key]).
hdrug_command(dtview,treebank,[]).

hdrug_command(dtedit,dtedit(Key),[Key]).
hdrug_command(dtedit,dtedit,[]).

hdrug_command(dttred,dtedit(Key),[Key]).
hdrug_command(dttred,dtedit,[]).

hdrug_command(tred,dtedit(Key),[Key]) :-
    set_flag(current_ref,Key).
hdrug_command(tred,dtedit,[]).

hdrug_command('Tred',dtedit(Key),[Key]) :-
    set_flag(current_ref,Key).
hdrug_command('Tred',dtedit,[]).

hdrug_command('T',dtedit(Key),[Key]) :-
    set_flag(current_ref,Key).
hdrug_command('T',dtedit,[]).

%hdrug_command(dtrestore,dtrestore(Key),[Key]).
%hdrug_command(dtrestore,dtrestore,[]).

hdrug_command(check_suite,alpino_treebank:check_suite_and_treebank_are_consistent,[]).

hdrug_command(treebank_triples,alpino_treebank:treebank_triples_list([F|Files]),
		   [F|Files]).

hdrug_command(treebank_triples,alpino_treebank:treebank_triples,[]).

hdrug_command(treebank_dep_features,alpino_treebank:treebank_dep_features_list(Files),
		   Files).

hdrug_command(dep_features,treebank_dep_features_obj(Obj),[Obj]).
hdrug_command(dep_features,treebank_dep_features_obj(1),[]).

hdrug_command(treebank_full_triples,
		   alpino_treebank:treebank_full_triples_list(Files),Files).

hdrug_command(treebank,alpino_treebank:show_treebank(Type,Output,FileXml),L0) :-
    hdrug_cmdint:show_command(Type,Output,L0,[File]),
    treebank_directory(Dir),
    format_to_chars('~w/~w.xml',[Dir,File],Chars),
    atom_codes(FileXml,Chars).

hdrug_command(treebank,alpino_treebank:show_treebank(Type,Output,FileXml),L0) :-
    hdrug_cmdint:show_command(Type,Output,L0,[]),
    xml_filename(FileXml).

hdrug_command(t,alpino_treebank:show_treebank(Format,Output,FileXml),
		   [File]) :-
    set_flag(current_ref,File),
    treebank_directory(Dir),
    format_to_chars('~w/~w.xml',[Dir,File],Chars),
    atom_codes(FileXml,Chars),
    show_defaults(Format,Output).

hdrug_command(dtget,alpino_treebank:show_treebank(Format,Output,File),
	      [File]) :-
    show_defaults(Format,Output).

hdrug_command(t,alpino_treebank:show_treebank(Format,Output,FileXml),[]) :-
    xml_filename(FileXml),
    show_defaults(Format,Output).

hdrug_command(t_cgn,show_treebank_cgn(FileXml),
		   [File]) :-
    set_flag(current_ref,File),
    treebank_directory(Dir),
    format_to_chars('~w/~w.xml',[Dir,File],Chars),
    atom_codes(FileXml,Chars).

hdrug_command(t_cgn,show_treebank_cgn(FileXml),
		   []) :-
    xml_filename(FileXml).

hdrug_command(tg,show_xml_adt(Key),[Key]).
hdrug_command(tg,show_xml_adt(File),[]) :-
    hdrug_flag(current_ref,File),
    File \= undefined.
hdrug_command(tgb,show_xml_adt_bare(Key),[Key]).
hdrug_command(tgb,show_xml_adt_bare(File),[]) :-
    hdrug_flag(current_ref,File),
    File \= undefined.



:- public compare_treebank_cgn/0, compare_treebank_cgn/2, reset_cgn_numbers/0, show_treebank_cgn/1.
compare_treebank_cgn :-
    xml_filename(File),
    compare_treebank_cgn(File,1).

hdrug_command(cc,compare_treebank_cgn(File,Obj),[Obj]) :-
    xml_filename(File).
hdrug_command(cc,compare_treebank_cgn(File,1),[]) :-
    xml_filename(File).

hdrug_command(compare_cgn,compare_treebank_cgn(File,Obj),[Obj]) :-
    xml_filename(File).
hdrug_command(compare_cgn,compare_treebank_cgn(File,1),[]) :-
    xml_filename(File).

hdrug_command(ncc,( next, compare_treebank_cgn ), []).

:- public show_treebank_current_ref/0.
show_treebank_current_ref :-
    xml_filename(FileXml),
    show_defaults(Format,Output),
    alpino_treebank:show_treebank(Format,Output,FileXml).

hdrug_command(tree_comparison,tree_comparison(Obj),[Obj]).
hdrug_command(tree_comparison,tree_comparison,[]).
hdrug_command(tree_comparison_find_best,tree_comparison_find_best,[]).
hdrug_command(eval,tree_comparison(I),[I]).
hdrug_command(eval,tree_comparison,[]).
hdrug_command(eval_all,tree_comparison_all,[]).
hdrug_command(best,tree_comparison_find_best,[]).

hdrug_command(dump_adt,xml_save_object_adt(Obj,stream(user_output)),[Obj]).
hdrug_command(dump_adt,xml_save_object_adt(1,stream(user_output)),[]).
hdrug_command(dump_adt,xml_save_object_adt(Obj,File),[Obj,File]).

%% options and commands from Generation/cg.pl


%% Hdrug commands
hdrug_command(active_edges,active_edges,[]).
hdrug_command_help(active_edges,"active_edges",
	"list all generated active edges").

hdrug_command(edges,inactive_edges,[]).
hdrug_command(inactive_edges,inactive_edges,[]).
hdrug_command_help(inactive_edges,"inactive_edges",
	"list all generated inactive edges").

hdrug_command(top_edges,top_edges,[]).
hdrug_command_help(top_edges,"top_edges",
	"list all generated inactive edges with top category").

hdrug_command(analyse_edges,alpino_cg:analyse_edges,[]).
hdrug_command_help(analyse_edges,"analyse_edges",
	"count how often inactive edges (generation) are used").

%% this allows stuff like:
%% edge deriv 2099
%% and
%% edge 2099
hdrug_command(edge,alpino_cg:show_edge(E,Type,Output),L0) :-
    hdrug_cmdint:show_command(Type0,Output,L0,[E]),
    (  Type0 == default -> Type=tree(deriv) ; Type = Type0 ).

hdrug_command(cat_edge,alpino_cg:show_cat_edge(Cat,Type,Output),L0) :-
    hdrug_cmdint:show_command(Type0,Output,L0,[Cat]),
    (  Type0 == default -> Type=tree(deriv) ; Type = Type0 ).

hdrug_command(top_edge,alpino_cg:show_top_edge(Type,Output),L0) :-
    hdrug_cmdint:show_command(Type0,Output,L0,[]),
    (  Type0 == default -> Type=tree(deriv) ; Type = Type0 ).

hdrug_command(gen_fs,alpino_cg:show_gen_fs(Type,Output),L0) :-
    hdrug_cmdint:show_command(Type,Output,L0,[]).

hdrug_command(gen_lex,alpino_cg:show_gen_lex(Word,Type,Output),L0) :-
    hdrug_cmdint:show_command(Type,Output,L0,[Word]).

hdrug_command(gen_init,alpino_cg:gen_init(Ref),[Ref]).
hdrug_command(gen_init,alpino_cg:gen_init(Ref),[]) :-
    hdrug_flag(current_ref,Ref).

hdrug_command(glex,alpino_cg:glex(N),[N]).

hdrug_command(gtags,alpino_cg:gtags,[]).

%% options and commands from lexical_analysis.pl

hdrug_command(skip_word,alpino_lexical_analysis:cmd_skip_word(W),W).
hdrug_command(clear_skip_table,
	      retractall(alpino_lexical_analysis:add_skip_word(_,_)),[]).

hdrug_command(add_lex,
	      alpino_lexical_analysis:add_lex(Words),
	      Words).
hdrug_command(add_tag,
	      alpino_lexical_analysis:add_tag(Words),
	      Words).
hdrug_command(add_sc,
	      alpino_lexical_analysis:add_sc(Words),
	      Words).
hdrug_command(clear_add_lex_table,
	      (   retractall(alpino_lexical_analysis:add_lex_table(_,_,_,_,_)),
		  retractall(alpino_lexical_analysis:add_tag_table(_,_,_,_,_))
	        % retractall(alpino_lexical_analysis:add_universal_table(_,_,_,_,_,_))
	      ), []).
hdrug_command(tags,alpino_lexical_analysis:tags,[]).
hdrug_command(tr_tags,alpino_lexical_analysis:tr_tags,[]).

hdrug_command(unknown,alpino_lexical_analysis:only_unknowns(Word),[Word]).

hdrug_command(la,do_one(lex,Key),[Key]) :-
    a_sentence(Key,_,_).
hdrug_command(la,do_one(lex,Key),[]) :-
    hdrug_flag(current_ref,Key).
hdrug_command(la,do_one(lex,undef,Words),Words).

%% options and commands from dt.pl

hdrug_command(triples,format_triples_of_obj(Obj),[Obj]).
hdrug_command(triples,format_triples_of_obj(1),[]).

%% options and commands from format_syntax.pl

hdrug_command(syntax,format_syntax_of_obj(Obj),[Obj]).
hdrug_command(syntax,format_syntax_of_obj(1),[]).

hdrug_command(cfg,format_cfg_of_obj(Obj),[Obj]).
hdrug_command(cfg,format_cfg_of_obj(1),[]).

hdrug_command(some_syntax,format_some_syntax_of_obj(Obj),[Obj]).
hdrug_command(some_syntax,format_some_syntax_of_obj(1),[]).

hdrug_command(deriv,format_deriv_of_obj(Obj),[Obj]).
hdrug_command(deriv,format_deriv_of_obj(1),[]).

hdrug_command(nderiv,format_nderiv_of_obj(Obj),[Obj]).
hdrug_command(nderiv,format_nderiv_of_obj(1),[]).

hdrug_command(lc,format_left_corners_of_obj(Obj),[Obj]).
hdrug_command(lc,format_left_corners_of_obj(1),[]).
hdrug_command(left_corners,format_left_corners_of_obj(Obj),[Obj]).
hdrug_command(left_corners,format_left_corners_of_obj(1),[]).

hdrug_command(nlc,format_new_left_corners_of_obj(Obj),[Obj]).
hdrug_command(nlc,format_new_left_corners_of_obj(1),[]).

hdrug_command(frames,format_frames_of_obj(Obj),[Obj]).
hdrug_command(frames,format_frames_of_obj(1),[]).

hdrug_command(postag,format_postags_of_obj(Obj),[Obj]).
hdrug_command(postag,format_postags_of_obj(1),[]).
hdrug_command(postags,format_postags_of_obj(Obj),[Obj]).
hdrug_command(postags,format_postags_of_obj(1),[]).
hdrug_command(cgn,format_postags_of_obj(Obj),[Obj]).
hdrug_command(cgn,format_postags_of_obj(1),[]).

%% options and commands from adt.pl

hdrug_command(adt,alpino_adt:show_adt(Objs,Type,Output),Objs0) :-
    hdrug_cmdint:show_command(Type0,Output,Objs0,Objs),
    (  Type0 == default
    -> Type = tree(adt)
    ;  Type0 = Type
    ).

:- public treebank_dep_features_obj/1.
treebank_dep_features_obj(Obj) :-
    object(Obj,o(Result,_,_)),
    result_to_dt(Result,DT),
    treebank_dep_features_dt(DT).

%%%%%%%%%%%%%%%%%%
%% Parse server %%
%%%%%%%%%%%%%%%%%%

:- use_module('../unix/unix').
:- use_module(library(sockets)).

:- initialize_flag(server_port,42424).
:- initialize_flag(server_kind,parse).
:- initialize_flag(server_timeout,30).

:- if(current_predicate(is_stream/1)).

:- else.

%% current_stream raises an error for a closed stream for SWI
%% SICStus has no is_stream

is_stream(Stream) :-
    current_stream(_,_,Stream).

:- endif.

:- public alpino_server/0.

alpino_server :-
    \+ current_predicate(sockets:socket_select/5),
    \+ current_predicate(sicstus_sockets:socket_select/5),
    !,
    debug_message(0,
	"alpino_server/0 is not supported on this platform: socket_select/5 from library(sockets) not available",[]).

alpino_server :-
    hdrug_flag(server_port,Port),
    hdrug_flag(server_kind,Kind),
    hdrug_flag(server_pidfile,PidFile),
    hdrug_flag(server_timeout,Timeout),
    write_pid_file(PidFile),
    alpino_server(Port,Kind,Timeout).

alpino_server(Port,Kind,Timeout) :-
    socket('AF_INET',Socket),
    socket_bind(Socket,'AF_INET'(Hostname,Port)),
    socket_listen(Socket,10),
    debug_message(1,"Listening on port ~a ~d~n",[Hostname,Port]),
    server_accept(Socket,Kind,Timeout).

write_pid_file(undefined) :-
    !.
write_pid_file(PidFile) :-
    open(PidFile,write,Stream),
    alpino_unix:pid(Pid),
    format(Stream,"~d",[Pid]),
    close(Stream).

server_accept(Socket,Kind,Timeout) :-
    reap_zombies,
    socket_accept(Socket,Peer,Stream),
    set_stream_encoding(Stream,utf8),
    fork(Pid),
    handle_connection(Pid,Kind,Socket,Peer,Stream,Timeout).

handle_connection(child,Kind,_Socket,Peer,Stream,Timeout) :-
    !,
    catch(process_and_save(Kind,Stream,Peer,Timeout),
	  Error,
	  print_error_stream(Stream,Error)),
    (   %% current_stream(_,_,Stream)
	is_stream(Stream)
    ->  close(Stream)
    ;   true
    ),
    halt.

handle_connection(Pid,Kind,Socket,Peer,Stream,Timeout) :-
    close(Stream),
    debug_message(1,"Child with pid ~d is handling the connection with ~s~n",
		  [Pid,Peer]),
    server_accept(Socket,Kind,Timeout).

read_line_timeout(Stream,Line,Timeout) :-
    socket_select([],[],Timeout:0,[Stream],Streams),
    (   Streams = [Stream]
    ->  read_line(Stream,Line)
    ;   print_error_stream(Stream,'timeout'),
	close(Stream),
	halt
    ).

process_and_save(parse,Stream,Peer,Timeout) :-
    read_line_timeout(Stream,Line,Timeout),
    alpino_parse_line(Peer,Line),
    object(1,o(Result,String,_)),
    ignore_brackets(String,StringNoBrackets),
    xml_save(Result,StringNoBrackets,[],stream(Stream),normal),
    !.

process_and_save(_Kind,Stream,_Peer,_Timeout) :-
    print_error_stream(Stream,'parsing failed').

print_error_stream(Stream,Error) :-
    format(Stream,"Error: ~w~n",[Error]).

:- if(current_prolog_flag(dialect,swi)).

set_stream_encoding(Stream,Encoding) :-
    stream_pair(Stream,ReadStream,WriteStream),
    set_stream(WriteStream,encoding(Encoding)),
    set_stream(ReadStream,encoding(Encoding)).

:- else.

set_stream_encoding(_Stream,_Encoding).

:- endif.


%% Predicates that mirror Hdrug equivalents, but do actual processing in
%% a child process.

:- public generator_comparisons_fork/0.
generator_comparisons_fork :-
    initialize_dictionaries_generation,
    findall(Key,a_lf(Key,_,_),Keys),
    generator_comparisons_fork(Keys).

generator_comparisons_fork([]).
generator_comparisons_fork([H|T]) :-
    fork(Pid),
    (   Pid == child
    ->  generator_comparison(H),
	halt
    ;   wait_for_pid(Pid,Status),
	print_child_status(Status,Pid)
    ),
    generator_comparisons_fork(T).

wait_for_pid(Pid,Status) :-
    alpino_unix:wait(Pid0,Status0),
    (   Pid0 == Pid
    ->  Status = Status0
    ;   wait_for_pid(Pid,Status)
    ).

print_child_status(exited(0),_) :-
    !.
print_child_status(Status,Pid) :-
    debug_message(1,"Child with PID ~d ended with: ~q~n",[Pid,Status]).


%% collect stuff here that we want to do only once, before e.g. different
%% threads or different forks are executed and dictionaries will be present
%% more than once...
:- public initialize_dictionaries/0.

initialize_dictionaries :-
    initialize_dictionaries_parsing,
    initialize_dictionaries_generation.

initialize_dictionaries_parsing :-
    alpino_lex:lex_initialize,
    alpino_lex:initialize_names_dict(_),
    alpino_postagger:pos_filter_initialize,
    alpino_penalties:initialize_corpus_frequency(_).

initialize_dictionaries_generation :-
    alpino_lex:lex_initialize,
    alpino_lex:initialize_names_dict(_),    
    alpino_ngram_lm:fluency_model_initialize.

option(init_dict) -->
    { initialize_dictionaries }.

option(init_dict_p) -->
    { initialize_dictionaries_parsing }.

option(init_dict_g) -->
    { initialize_dictionaries_generation }.

:- initialize_flag(generate_failsafe,off).

%% TODO: take threads into account here
%% TODO: this is a mess

after_timeout_options_(testN):-
    alpino_lc:clean,
    hdrug_flag(pos_tagger,off),
    set_flag(pos_tagger,on),
    set_flag(last_one_timeout,on),
    best_score_new_parse.

after_timeout_options_(off) :-
    fail.

after_timeout_options_(on) :-
    alpino_lc:clean,
    hdrug_flag(parse_candidates_beam,Beam),
    (  Beam =:= 0 ; Beam > 100 ),
    set_flag(save_parse_candidates_beam,Beam),
    set_flag(parse_candidates_beam,100),
    hdrug_flag(disambiguation_beam,Beam2),
    (  Beam2 =:= 0 ; Beam2 > 2 ),
    set_flag(save_disambiguation_beam,Beam2),
    set_flag(disambiguation_beam,2),
    set_flag(last_one_timeout,on).

after_timeout_options(alpino_lc:parse(_)) :-
    alpino_lc:clean,
    hdrug_flag(current_input_sentence,Sentence0),
%%    hdrug_flag(current_ref,Key),
%%    ignore_brackets(Sentence0,Sentence),
%%    concat_all(Sentence,StringAtom,' '),
%%    format(user_error,"timeout|~w|~w~n",[Key,StringAtom]),
    hdrug_flag(after_timeout_options,Val),
    after_timeout_options_(Val),
    hdrug:retractall(object(_,_)),
    lexical_analysis(Sentence0).

after_timeout_options(alpino_cg:generate(_)) :-
    hdrug_flag(after_timeout_options,on),
    hdrug_flag(generate_failsafe,off),
    hdrug_flag(filter_local_trees,on),
    set_flag(filter_local_trees,bigram),
    set_flag(after_timeout_options,on2).

after_timeout_options(alpino_cg:generate(_)) :-
    hdrug_flag(after_timeout_options,on2),
    set_flag(generate_failsafe,on),
    set_flag(after_timeout_options,on3).

after_timeout_options(alpino_cg:generate(_)) :-
    hdrug_flag(after_timeout_options,on3),
    set_flag(generate_failsafe,last),
    set_flag(after_timeout_options,on4).

undo_timeout_options(alpino_cg:generate(_)) :-
    set_flag(filter_local_trees,on),
    set_flag(generate_failsafe,off),
    set_flag(after_timeout_options,on),
    !.

undo_timeout_options(alpino_lc:parse(_)) :-
    hdrug_flag(after_timeout_options,testN),
    set_flag(pos_tagger,off),
    !.

undo_timeout_options(alpino_lc:parse(_)) :-
    hdrug_flag(after_timeout_options,on),
    hdrug_flag(save_parse_candidates_beam,Beam),
    set_flag(parse_candidates_beam,Beam),
    hdrug_flag(save_disambiguation_beam,Beam2),
    set_flag(disambiguation_beam,Beam2),
    !.
undo_timeout_options(_).

show_treebank_cgn(File) :-
    alpino_treebank:xml_file_to_dt(File,DT,Sent,_),
    alpino_treebank:string_to_words(Sent,Words),
    collect_cgn_tags_of_tree(DT,Tags0,[]),
    sort(Tags0,Tags),
    alpino_format_syntax:format_cgn_postags(Tags,Words).

collect_treebank_cgn(File,Tags) :-
    alpino_treebank:xml_file_to_dt(File,DT,_Words,_),
    collect_cgn_tags_of_tree(DT,Tags0,[]),
    sort(Tags0,Tags).

collect_cgn_tags_of_tree(tree(Label,_Key,Ds)) -->
    collect_cgn_tags_of_label(Label),
    collect_cgn_tags_of_ds(Ds).

collect_cgn_tags_of_ds([]) --> [].
collect_cgn_tags_of_ds([H|T]) -->
    collect_cgn_tags_of_tree(H),
    collect_cgn_tags_of_ds(T).

collect_cgn_tags_of_label(r(_,Label)) -->
    collect_cgn_tags_of_label_rest(Label).

collect_cgn_tags_of_label_rest(i(_)) --> [].
collect_cgn_tags_of_label_rest(i(_,Rest)) -->
    collect_cgn_tags_of_label_rest(Rest).
collect_cgn_tags_of_label_rest(p(_)) --> [].
collect_cgn_tags_of_label_rest(l(read_from_treebank(_,Lemma,CGN),_Cat,_Root/[P0,P])) -->
    [cgn_postag(P0,P,Lemma,CGN)].
collect_cgn_tags_of_label_rest(l(read_from_treebank(_,_,Lemma,CGN),_Cat,_Root/[P0,P])) -->
    [cgn_postag(P0,P,Lemma,CGN)].


compare_treebank_cgn_result(File,Result,Identifier) :-
    alpino_format_syntax:result_to_frames(Result,_,_,Frames),
    alpino_format_syntax:frames_to_postags(Frames,Result,SysTags),
    compare_treebank_cgn_continue(File,SysTags,Identifier),
    !.
compare_treebank_cgn_result(File,_,_) :-
    format(user_error,"error: compare_treebank_cgn_result/3 failed (~w)~n",[File]),
    fail.

compare_treebank_cgn(File,Obj) :-
    hdrug_flag(current_ref,Key),
    alpino_format_syntax:collect_postags_of_obj(Obj,SysTags,_Words),
    compare_treebank_cgn_continue(File,SysTags,Key/Obj),
    !.
compare_treebank_cgn(File,_) :-
    format(user_error,"error: compare_treebank_cgn/2 failed (~w)~n",[File]),
    fail.

compare_treebank_cgn_continue(File,SysTags,Ident) :-
    hdrug_flag(current_input_sentence,Words0),
    ignore_brackets(Words0,Words),
    collect_treebank_cgn(File,GoldTags),
    format(user_error,"~w~t~40+~w~t~40+~w~t~40+ T    ~w~n",[word,treebank,parser,key]),
    length(GoldTags,Len),
    compare_cgn(GoldTags,SysTags,0,Words,0,Correct,0,CorrectL,Ident),
    format(user_error,"postag ~w / ~w correct~n",[Correct, Len]),
    format(user_error," lemma ~w / ~w correct~n",[CorrectL,Len]),
    add_cgn_numbers(Correct,CorrectL,Len).

add_cgn_numbers(C1,D1,L1) :-
    hdrug_flag(cgn_numbers,Val),
    (   Val=c(C0,D0,L0)
    ->  true
    ;   C0=0, D0=0, L0=0
    ),
    C is C0 + C1,
    D is D0 + D1,
    L is L0 + L1,
    P is 100*C/L,
    R is 100*D/L,
    format(user_error,"#PT#~w / ~w correct (~2f%)~n",[C,L,P]),
    format(user_error,"#LM#~w / ~w correct (~2f%)~n",[D,L,R]),
    set_flag(cgn_numbers,c(C,D,L)).

reset_cgn_numbers :-
    set_flag(cgn_numbers,c(0,0,0)).

:- initialize_flag(print_suite,"").

compare_cgn_quiet(A,B,C,D,E,F,G,H,I) :-
    compare_cgn(A,B,C,D,E,F,G,H,I,_,_).

compare_cgn(A,B,C,D,E,F,G,H,I) :-
    compare_cgn(A,B,C,D,E,F,G,H,I,Messages,[]),
    format_messages(Messages).

format_messages([]).
format_messages([String-Vars|L]):-
    format(user_error,String,Vars),
    format_messages(L).


%% sorted, so in order
%% complete
compare_cgn([],[],_,[],C,C,L,L,_,Msg,Msg).
compare_cgn([cgn_postag(P0,P,LemmaA,H)|T],
	    [cgn_postag(P0,P,LemmaB0,H2)|T2],P0,[W|Words],C0,C,L0,L,Ident,Msg0,Msg) :-
    hdrug_flag(print_suite,SuiteStr),
    alpino_treebank:get_lemma_or_word(LemmaB0,LemmaB,W),
    (   H == H2
    ->  C1 is C0 + 1,
	Msg0 = Msg1
    ;   Msg0 = ["~w~t~40+~w~t~40+~w~t~40+ #PT# ~w ~s~n"-[W,H,H2,Ident,SuiteStr]|Msg1],
	C1 = C0
    ),
    (   LemmaA == LemmaB
    ->  L1 is L0 + 1,
	Msg1 = Msg2
    ;   Msg1 = ["~w~t~40+~w~t~40+~w~t~40+ #LM# ~w ~s~n"-[W,LemmaA,LemmaB,Ident,SuiteStr]|Msg2],
	L1 = L0
    ),
    compare_cgn(T,T2,P,Words,C1,C,L1,L,Ident,Msg2,Msg).

/*
:- public lex_without_context_options/0.
lex_without_context_options :-
    lex_without_context_options(_,_,_,_,_).

lex_without_context_options(Old,OldP,OldC,OldF,OldE) :-
    hdrug_flag(debug,Old,0),
    hdrug_flag(pos_tagger,OldP,off),
    hdrug_flag(check_word_form,OldC,off),
    hdrug_flag(filter_lexical_analysis,OldF,off),
    hdrug_flag(expand_subcat,OldE,off).

lex_reset_options(Old,OldP,OldC,OldF,OldE) :-
    set_flag(pos_tagger,OldP),
    set_flag(debug,Old),
    set_flag(check_word_form,OldC),
    set_flag(filter_lexical_analysis,OldF),
    set_flag(expand_subcat,OldE).


:- public lu_form/0.
lu_form :-
    reconsult(lu_form),
    lex_without_context_options(Old,OldP,OldC,OldF,OldE),
    call_cleanup(lu_form_,
		 lex_reset_options(Old,OldP,OldC,OldF,OldE)
		).

lu_form_ :-
    (   lu_form(Id,Form,Pos),
	map_pos(Pos,Pos2),
	atom_codes(Form,Codes),
	alpino_util:codes_to_words(Codes,Words),
	findall(Root/Pos2,
	    (   alpino_lexical_analysis:lexical_analysis_one(Words,Tag,Root), 
		\+ ignore_tag_cornetto_ukw(Pos2,Tag)
	    ),
	    L),
	(   L == []
	->  format(user_error,"%% ~w ~w ~w not found~n",[Words,Pos2,Id])
	;   sort(L,L2),
	    member(Root/Pos2,L2),
	    format("~w\t~w\t~w\t~w\n",[Id,Form,Pos,Root])
	),
	fail
    ;   true
    ).

map_pos('NOUN',n).
map_pos('ADJ',a).
map_pos('VERB',v).

	

%%% file:/net/corpora/Cornetto2.0/UKB-resources/lexicon-file/cdb2.0-nld_synset_domain_graph.lex
%%%
%%% nestelen  d_v-4459-v d_v-4458-v
%%% wegvallen  d_v-8896-v d_v-8326-v
%%% ad_hoc  n_a-501633-a

%%% per regel: woord (met _) gevolgd door aantal synset-identifiers
%%%

:- public c/0, t/0.

c :-
    lex_without_context_options(Old,OldP,OldC,OldF,OldE),
    call_cleanup(cornetto_ukw('/net/corpora/Cornetto2.0/UKB-resources/lexicon-file/cdb2.0-nld_synset_domain_graph.lex'),
		 lex_reset_options(Old,OldP,OldC,OldF,OldE)
		).

t :-
    lex_without_context_options(Old,OldP,OldC,OldF,OldE),
    call_cleanup(cornetto_ukw('t.lex'),
		 lex_reset_options(Old,OldP,OldC,OldF,OldE)
		).

cornetto_ukw(File) :-
    see(File),
    call_cleanup(cornetto_ukw, seen).

cornetto_ukw :-
    (   repeat,
	read_line(Codes),
	(   Codes == end_of_file
	->  !
	;   cornetto_ukw_line(Codes),
	    fail
	)
    ;   true
    ).

cornetto_ukw_line(X) :-
    alpino_util:codes_to_words(X,[Word|Ids0]),
    guess_cornetto_pos(Ids0,Pos,Ids,Word), %% generates up to 4 solutions for each POS
    tr('_',' ',Word,Word2),
    atom_codes(Word2,Codes2),
    codes_to_words(Codes2,Words2),
    findall(Root/Tag/Pos,
	    (   alpino_lexical_analysis:lexical_analysis_one(Words2,Tag,Root), 
		\+ ignore_tag_cornetto_ukw(Pos,Tag)
	    ),
	    L),
    (   L == []
    ->  format("%% ~w ~w ~w not found~n",[Word,Pos,Ids])
    ;   member(Root/Tag/Pos,L),
	prettyvars(Tag),
	member(Id,Ids),
	format("~q.~n",[root_cornetto_id(Root,Tag,Pos,Id,Word)])
    ).
    

ignore_tag_cornetto_ukw(_,verb(_,_,Sc)) :-
    functor(Sc,Fun,_),
    atom_concat(part_,_,Fun).
ignore_tag_cornetto_ukw(_,v_noun(Sc)) :-
    functor(Sc,Fun,_),
    atom_concat(part_,_,Fun).
ignore_tag_cornetto_ukw(_,adjective(_,part(_))).
ignore_tag_cornetto_ukw(_,complementizer(_)).
ignore_tag_cornetto_ukw(_,particle(_)).
ignore_tag_cornetto_ukw(_,conj(_)).
ignore_tag_cornetto_ukw(_,preposition(_)).
ignore_tag_cornetto_ukw(_,preposition(_,_)).


ignore_tag_cornetto_ukw(v,V) :-
    (   (   V = verb(_,inf,_)
	;   V = verb(_,inf(no_e),_)
	)
    ->  fail
    ;   true
    ).
ignore_tag_cornetto_ukw(a,verb(_,_,_)).
ignore_tag_cornetto_ukw(a,v_noun(_)).
ignore_tag_cornetto_ukw(a,noun(_,_,_)).
ignore_tag_cornetto_ukw(a,noun(_,_,_,_)).
ignore_tag_cornetto_ukw(a,name_determiner(_)).
ignore_tag_cornetto_ukw(a,name_determiner(_,_)).  % Noors = het Noors, niet genitive of Noor

ignore_tag_cornetto_ukw(n,verb(_,_,_)).
ignore_tag_cornetto_ukw(n,v_noun(_)).
ignore_tag_cornetto_ukw(n,adjective(_)).
ignore_tag_cornetto_ukw(n,adjective(_,_)).
ignore_tag_cornetto_ukw(n,adverb).
ignore_tag_cornetto_ukw(n,loc_adverb).
ignore_tag_cornetto_ukw(n,dir_adverb).
ignore_tag_cornetto_ukw(n,post_adjective(_)).    % * iets Noors
ignore_tag_cornetto_ukw(n,name_determiner(_)).
ignore_tag_cornetto_ukw(n,name_determiner(_,_)).  % Noors = het Noors, niet genitive of Noor

guess_cornetto_pos(Ids0,Pos,[Id|Ids],Word) :-
    parse_ids(Ids0,Ids1,Word),
    lists:member(Pos,[n,a,v,none]),
    findall(Id,member(Pos/Id,Ids1),[Id|Ids]).

parse_ids([],[],_).
parse_ids([H|T],[Pos/NH|NT],Word) :-
    sub_atom(H,_,1,1,'-'),
    sub_atom(H,_,1,0,Pos),
    sub_atom(H,0,_,2,NH),
    lists:member(Pos,[n,a,v]),
    !,
    parse_ids(T,NT,Word).
parse_ids([H|T],[none/H|NT],Word) :-
    format("%%%% ~w no pos: ~w~n",[Word,H]),
    parse_ids(T,NT,Word).
*/

%%% user_hook in dt.pl
%%% after parsing, create output attribute-values on the basis of Dt
dt_extract_attributes(Dt,Attributes,Rel) :-
    (   Rel == se
    ->  Attributes = []
    ;   findall(Att,dt_extract_attribute(Dt,Att),Attributes)
    ).

dt_extract_attribute(Dt,AttVal) :-
    dt_extract_rnum(Dt,AttVal).

dt_extract_attribute(Dt,stype=ynquestion) :-
    \+ alpino_data:not_ynquestion(Dt).
dt_extract_attribute(Dt,stype=whquestion) :-
    \+ alpino_data:not_whquestion(Dt).
dt_extract_attribute(Dt,stype=imparative) :-
    \+ alpino_data:not_imparative(Dt).
dt_extract_attribute(Dt,stype=topic_drop) :-
    \+ alpino_data:not_topic_drop(Dt).
dt_extract_attribute(Dt,stype=declarative) :-
    \+ alpino_data:not_declarative(Dt).

dt_extract_attribute(Dt,dropped_prs=fir) :-
    \+ alpino_data:not_topic_drop(Dt),
    \+ alpino_data:not_dropped_fir(Dt).

dt_extract_attribute(Dt,dropped_prs=thi) :-
    \+ alpino_data:not_topic_drop(Dt),
    \+ alpino_data:not_dropped_thi(Dt).

dt_extract_attribute(Dt,dropped_agr=sg) :-
    \+ alpino_data:not_topic_drop(Dt),
    \+ alpino_data:not_dropped_sg(Dt).

dt_extract_attribute(Dt,dropped_agr=pl) :-
    \+ alpino_data:not_topic_drop(Dt),
    \+ alpino_data:not_dropped_pl(Dt).

dt_extract_attribute(Dt,v_per=Val) :-
    alpino_data:dt_prs(Dt,Val0),
    hdrug_feature:give_boolean_type(Val0,Val),
    Val \== per.

%% should succeeed only once
dt_extract_rnum(Dt,rnum=pl) :-
    alpino_data:dt(Dt,_,Frame,_,_),
    lists:member(Frame,[noun(_,_,pl),noun(_,_,pl,_)]),
    !.

%% if it cannot be singular, it must be plural
dt_extract_rnum(Dt,rnum=pl) :-
    alpino_data:dt_num(Dt,Num), 
    \+ alpino_data:sg(Num),
    !.
%% if it cannot be plurar, it must be singular
dt_extract_rnum(Dt,rnum=sg) :-
    alpino_data:dt_num(Dt,Num), 
    \+ alpino_data:pl(Num),
    !.
%% exceptions to the default below: for these
%% lemma's, plural is much more probable
dt_extract_rnum(Dt,rnum=pl) :-
    alpino_data:dt_num(Dt,Num),
    nonvar(Num),
    \+ \+ alpino_data:pl(Num),
    \+ \+ alpino_data:sg(Num),
    alpino_data:dt(Dt,Hword,_,_,_),
    alpino_data:label(Hword,_,Lemma,_,_,_),
    lists:member(Lemma,[ze,miljoen,miljard]),  % more to come?
    !.    
%% if it can be singular, it will be singular
%% i.e., default is singular
%% unless it is marked "both" in its proper name tag (? but via unknowns most are both...)
dt_extract_rnum(Dt,rnum=sg) :-
    alpino_data:dt(Dt,_,Frame,_,_),
    \+ Frame = proper_name(both,_),
    \+ Frame = proper_name(both),
    alpino_data:dt_num(Dt,Num),
    nonvar(Num),
    \+ \+ alpino_data:sg(Num).

%%% user_hook in adt.pl
%%% before generation, instantiate Dt on the basis of attribute-values of Adt
dt_apply_attributes(Dt,Attributes) :-
    (   lists:member(rnum=Val1,Attributes)
    ->  dt_apply_rnum(Val1,Dt)
    ;   true
    ),
    (   lists:member(stype=Val2,Attributes)
    ->  dt_apply_stype(Val2,Dt)
    ;   true 
    ),
    (   lists:member(dropped_agr=Val3,Attributes)
    ->  dt_apply_dropped_agr(Val3,Dt)
    ;   true
    ),
    (   lists:member(dropped_prs=Val4,Attributes)
    ->  dt_apply_dropped_prs(Val4,Dt)
    ;   true
    ).

dt_apply_dropped_agr(sg,Dt) :-
    alpino_data:dropped_sg(Dt).
dt_apply_dropped_agr(pl,Dt) :-
    alpino_data:dropped_pl(Dt).

dt_apply_dropped_prs(fir,Dt) :-
    alpino_data:dropped_fir(Dt).
dt_apply_dropped_prs(thi,Dt) :-
    alpino_data:dropped_thi(Dt).

dt_apply_rnum(sg,Dt) :-
    alpino_data:dt_num(Dt,Num),
    alpino_data:sg(Num).
dt_apply_rnum(pl,Dt) :-
    alpino_data:dt_num(Dt,Num),
    alpino_data:pl(Num).

dt_apply_stype(ynquestion,Dt) :-
    alpino_data:ynquestion(Dt).
dt_apply_stype(whquestion,Dt) :-
    alpino_data:whquestion(Dt).
dt_apply_stype(declarative,Dt) :-
    alpino_data:declarative(Dt).
dt_apply_stype(imparative,Dt) :-
    alpino_data:imparative(Dt).
dt_apply_stype(topic_drop,Dt) :-
    alpino_data:topic_drop(Dt).

option(compare_xml_files) -->
    {  set_flag(batch_command,alpino_treebank:tree_comparison_pairs) }.
				% actual files read from stdin

hdrug_command(compare_xml_pair,tree_comparison_pair(File1,File2),[File1,File2]).
option(compare_xml_pair) -->
    [File1],
    [File2],
    {  set_flag(batch_command,tree_comparison_pair(File1,File2)) }.

getchars(Stream,In,Out) :-
    get_code(Stream,Char),
    (   Char = -1
    ->	In = Out
    ;	In = [Char|Tail],
	getchars(Stream,Tail,Out)
    ).

:- initialize_flag(alpino_version, alpino(version)).
application_version :-
    application_version(String),
    format(user_error,"~s~n",[String]).

application_version(String) :-
    hdrug_flag(alpino_version,File0),
    (   File0 == undefined
    ->  String = "No version information available"
    ;   absolute_file_name(File0,File),
	open(File,read,Stream),
	getchars(Stream,String0,[]),
	remove_nl(String0,String),
	close(Stream)
    ).

remove_nl([],[]).
remove_nl([C|Cs0],Cs) :-
    remove_nlc(C,Cs,Cs1),
    remove_nl(Cs0,Cs1).

remove_nlc(C,Cs,Cs1) :-
    (   C =:= 10
    ->  Cs = Cs1
    ;   Cs = [C|Cs1]
    ).

/*
:- public generate_adt_xmls/0.

generate_adt_xmls :-
    
    initialize_flag(current_line_no,0),

    repeat,

    %%% reset, because apparantly this can get lost if timed outs occur????
    set_output(user_output),
    
    read_line(Chars),
    (   Chars == []
    ->  fail			% ignore empty line
    ;   Chars == end_of_file
    ->  !
    ;   update_line_number(Int),

	debug_message(1,"**** generating line number ~w~n",[Int]),

	(    catch(file_property('.alpino',type(directory)),_,fail)
	->   format_to_chars('.alpino/~w.xml',[Int],FileChars),
	     atom_codes(File,FileChars),
	     tell(File),
	     format("~s~n",[Chars]),
	     told
	;    true
	),

	(   alpino_treebank:alpino_ds_string_to_xml_terms(Chars,Terms),
	    alpino_treebank:xml_term_to_dt(Terms,DT,_,_,_),
	    alpino_treebank:tree2tree3(DT,ADT)
	->  generate(ADT)
	;   format(user_error,"error: Cannot parse input as ADT XML~n",[]),
	    generate([]) % this way, an empty line will be printed, to ensure we remain in sync
	),
	debug_message(1,"**** generated line number ~w~n",[Int]),
	fail
    ).

xml_dump_adt(Result) :-
    alpino_treebank:deptree_xml_adt(Result,Chars,[]),
    lists:substitute(10,Chars,32,Chars2),
    format("~s~n",[Chars2]).

%% constructs separate XML files from typical Treex output
:- public save_adt_xmls/0.

save_adt_xmls :-
    set_flag(counter,0),
    repeat,

    hdrug_flag(counter,Int),
    Int1 is Int+1,
    set_flag(counter,Int1),
    format_to_chars('~w.xml',[Int1],File0),
    atom_codes(File,File0),

    read_line(Chars),
    (   Chars == []
    ->  fail			% ignore empty line
    ;   Chars == end_of_file
    ->  !
    ;   tell(File),
	format("~s~n",[Chars]),
	told,
	fail
    ).

:- public show_xml_adt/1, show_xml_adt_bare/1.
show_xml_adt(File) :-
    if_gui(Output=clig,Output=user),
    set_flag(current_ref,File),
    alpino_treebank:xml_file_to_adt(File,ADT0),
    alpino_cg:combine_mwu(ADT0,ADT1),
    alpino_cg:transform_adt(ADT1,ADT),
    show(tree(adt),Output,[value(ADT)]).

show_xml_adt_bare(File) :-
    if_gui(Output=clig,Output=user),
    set_flag(current_ref,File),
    alpino_treebank:xml_file_to_adt(File,ADT0),
    alpino_cg:combine_mwu(ADT0,ADT),
    show(tree(adt),Output,[value(ADT)]).

:- public roundtrip/0, roundtrip/1, nroundtrip/0, proundtrip/0.

hdrug_command(roundtrip,roundtrip(Ref),[Ref]).
hdrug_command(trip,     roundtrip(Ref),[Ref]).
hdrug_command(pg,       roundtrip(Ref),[Ref]).
hdrug_command(roundtrip,roundtrip(Ref),[]) :-
    hdrug_flag(current_ref,Ref).
hdrug_command(trip,     roundtrip(Ref),[]) :-
    hdrug_flag(current_ref,Ref).
hdrug_command(pg,       roundtrip(Ref),[]) :-
    hdrug_flag(current_ref,Ref).

hdrug_command(proundtrip,proundtrip,[]).
hdrug_command(ptrip,proundtrip,[]).
hdrug_command(ppg,proundtrip,[]).
hdrug_command(nroundtrip,nroundtrip,[]).
hdrug_command(ntrip,nroundtrip,[]).
hdrug_command(npg,nroundtrip,[]).

nroundtrip :-
    hdrug_flag(current_ref,Key0),
    find_next_ref(Key0,Key,_),
    roundtrip(Key).

proundtrip :-
    hdrug_flag(current_ref,Key0),
    find_prev_ref(Key0,Key,_),
    roundtrip(Key).


roundtrip(Ref) :-
    veryfast_options,
    set_flag(order_canonical_dt,off),
    set_flag(robust_attr,frag),  % dt.pl; so the various dp parts of fragments are not connected with puncts
    a_sentence(Ref,_,_),
    set_flag(current_ref,Ref),
    set_flag(end_hook,best_score(adt)),
    sen(Ref),
    veryfast_options,
    set_flag(geneval,on),
    set_flag(print_table_total,on),
    set_flag(compare_object_saving,on),
    generator_comparison(Ref).

roundtrip :-
    a_sentence(Ref,_,_),
    on_exception(Exc,
		 roundtrip(Ref),
		 format(user_error,"exception: ~w~n",[Exc])
		),
    fail.
roundtrip :-
    print_table_total.
*/

:- public train_generation/0.
train_generation :-
    a_sentence(Ref,_,_),
    on_exception(Exc,
		 train_generation(Ref),
		 format(user_error,"exception: ~w~n",[Exc])
		),
    fail.
train_generation.

train_generation(Ref) :-
    veryfast_options,
    set_flag(robustness,if_required),
    set_flag(order_canonical_dt,off),
    set_flag(list_all_maxent_features,off),
    a_sentence(Ref,_,_),
    set_flag(current_ref,Ref),
    set_flag(end_hook,best_score(adt)),
    parser_comparison(Ref),
    slow_options,
    set_flag(list_all_maxent_features,on),
    set_flag(number_analyses,1000),
    set_flag(fluency_candidates_beam,1000),
    set_flag(end_hook,train_fluency),
    set_flag(robustness,off),
    generator_comparison(Ref).

:- initialize_flag(check_tags,off).

:- public check_tags/0.

hdrug_command(check_tags,check_tags,[]).

check_tags :-
    hdrug_flag(pos_tagger,on),
    call_cleanup(check_tags_do,check_tags_redo).

check_tags(PrevScore) :-
    hdrug_flag(pos_tagger,on),
    call_cleanup(check_tags_do(PrevScore),check_tags_redo),
    fail.

check_tags_do :-    
    hdrug_flag(current_input_sentence,Sent),
    findall(_-frame(_,_,P0,P,_,Tag,Word,_),alpino_lexical_analysis:tag(_,_,P0,P,_,Word,_,Tag),Tags),
    set_flag(pos_tagger,off),
    parse(Sent),
    (    hdrug:object(1,o(Result,_,_))
    ->   result_to_frames(Result,Frames,_),
	 find_missing_frames(Frames,Tags,Sent)
    ;    true
    ).

check_tags_do(Score) :-    
    hdrug_flag(current_input_sentence,Sent),
    findall(_-frame(_,_,P0,P,_,Tag,Word,_),alpino_lexical_analysis:tag(_,_,P0,P,_,Word,_,Tag),Tags),
    set_flag(pos_tagger,off),
    parse(Sent),
    alpino_treebank:tree_comparison(1,NewScore),
    (    NewScore > Score
    ->   format(user_error,"TAGGER Without tagger, score improves from ~2f to ~2f~n",[Score,NewScore]),
	 hdrug:object(1,o(Result,_,_)),
         result_to_frames(Result,Frames,_),
	 find_missing_frames(Frames,Tags,Sent)
    ;    format(user_error,"TAGGER Without tagger, score does not improve (from ~2f to ~2f)~n",[Score,NewScore])
    ).

check_tags_redo :-
    set_flag(pos_tagger,on).

find_missing_frames([],_,_).
find_missing_frames([H|T],Tags,Sent):-
    find_missing_frame(H,Tags,Sent),
    find_missing_frames(T,Tags,Sent).

find_missing_frame(Frame,List,_Sent) :-
    Frame = _-frame(_P0,_P,_,_,_,Tag,Word,_),
    (   member(Frame,List)
    ->  true
    ;   format(user_error,"TAGGER missing frame: ~w ~w~n",[Word,Tag])
    ).


:- initialize_flag(copy_input_if_no_transformation,off).

create_parse_prompt(Prompt):-
    hdrug_cmdint:dev_run(prolog:'$breaklevel'(BreakLevel,0),BreakLevel=0),
    hdrug_flag(current_line_no,N),
    (   BreakLevel =:= 0
    ->  charsio:format_to_chars('~w parse: ',[N],Chars)
    ;   charsio:format_to_chars('{~w} ~w parse: ',[BreakLevel,N],Chars)
    ),
    name(Prompt,Chars).

:- public has_rule/1.
hdrug_command(hasrule,has_rule(Rule),[Rule]).
hdrug_command(has_rule,has_rule(Rule),[Rule]).

%% prints object id of each object that has been built
%% using rule Rule
has_rule(Rule) :-
    (   object(No,o(Result,_,_)),
	alpino_data:result_term(_,_,_,Tree,_,Result),
	tree_has_rule(Tree,Rule),
	format("~w~n",[No]),
	fail
    ;   true
    ).

tree_has_rule(tree(_,Rule,_,_),Rule).
tree_has_rule(tree(_,_,Ds,_),Rule) :-
    lists:member(D,Ds),
    tree_has_rule(D,Rule).

show_defaults(Format,Output) :-
    hdrug_flag(type_default(user),UserType),
    hdrug_flag(type_default(clig),CligType),
    if_gui(( Format=CligType,
	     Output=clig
	   ),
	   ( Format=UserType,
	       Output=user
	   )).
