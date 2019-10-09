:- ensure_loaded(ops).

:- ensure_loaded(compilation). 

:- ensure_loaded([ hc_ch, hc_mp,   hc_mmp,
	           mj,    hc,      lc_ch,
		   left_ch,        head_ch,
		   right_ch
		 ]).

:- ensure_loaded([ bug_wf, bug]).

:- ensure_loaded(d), compile_rules.


:- hdrug_util:initialize_flag(parser,hc_mp).
:- hdrug_util:initialize_flag(top_features,main).
:- hdrug_util:initialize_flag(generator,bug_wf).

:- hdrug_util:initialize_flag(generator(bug),on).
:- hdrug_util:initialize_flag(generator(bug_wf),on).

:- hdrug_util:initialize_flag(parser(hc),off).
:- hdrug_util:initialize_flag(parser(hc_ch),on).
:- hdrug_util:initialize_flag(parser(mj),on).
:- hdrug_util:initialize_flag(parser(hc_mmp),on).
:- hdrug_util:initialize_flag(parser(hc_mp),on).
:- hdrug_util:initialize_flag(parser(head_ch),on).
:- hdrug_util:initialize_flag(parser(left_ch),on).
:- hdrug_util:initialize_flag(parser(right_ch),on).
:- hdrug_util:initialize_flag(parser(lc_ch),on).
:- hdrug_util:initialize_flag(parser(hc_xoldt),off).

:- hdrug_util:set_flag(application_name,'SH-GEN').  

:- version('Grammar based on the paper: 
An overview of Head-driven Generation').

gram_startup_hook_end :-
    tcl('
button .t.version -text {Semantic Head-driven Generation} -command {
    tk_dialog .d "About the Grammar" "
        This simple Definite Clause Grammar for Dutch\
        was written by Gertjan van Noord in order to \
        illustrate the semantic-head-driven generation\
        algorithm. More specifically this grammar was \
        used to obtain the performance data of the\
        semantic-head-driven generation algorithm as\
        described in the paper `An overview of head-driven\
        bottom-up generation\'. This paper is published in\
        a collection entitled `Current Research in Natural\
        Language Generation\' edited by Robert Dale, Chris\
        Mellish and Michael Zock, Academic Press 1990." "" 0 ok
}
pack .t.version -side right

.menu.help.m add command -label "About the grammar" -underline 0\
               -command {
    tk_dialog .d "About the Grammar" "
	This simple Definite Clause Grammar for Dutch\
	was written by Gertjan van Noord in order to \
	illustrate the semantic-head-driven generation\
	algorithm. More specifically this grammar was \
	used to obtain the performance data of the\
	semantic-head-driven generation algorithm as\
	described in the paper `An overview of head-driven\
	bottom-up generation\'. This paper is published in\
	a collection entitled `Current Research in Natural\
	Language Generation\' edited by Robert Dale, Chris\
	Mellish and Michael Zock, Academic Press 1990." "" 0 ok
}

set HDRUG sh-gen



       ').

    
:- ensure_loaded(suite).

