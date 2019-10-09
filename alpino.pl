:- module(alpino, [ alpino_parse/2,	% +Sentence, -Result
		    alpino_parse/3,	% +Sentence, -Result, +Options
		    alpino_result/3,	% +What, +Result, -Value
		    alpino_gui/0
		  ]).

:- (   current_prolog_flag(language, sicstus)
   ->  compile('Hdrug/Prolog/conditional_compilation')
   ;   true
   ).

:- if(current_prolog_flag(dialect, swi)).

:- expects_dialect(sicstus).

:- initialization(set_prolog_stack(global, limit(2*1024**3))).
:- initialization(set_prolog_stack(trail,  limit(1*1024**3))).

%:- set_prolog_flag(qcompile, auto).

:- endif.

:- use_module(library(system)).

:- multifile
    user:file_search_path/2.

user:file_search_path(alpino, Alpino) :-
    environ('ALPINO_HOME', Alpino).

user:file_search_path(hdrug, Hdrug) :-
    user:file_search_path(alpino,Alpino),
    atom_concat(Alpino,'/Hdrug/Prolog',Hdrug).

:- if(current_prolog_flag(dialect, swi)).

user:file_search_path(library, alpino(library)).

:- endif.

:- use_module(hdrug(hdrug_main)).

set_dir(Name, Alias) :-
       absolute_file_name(Alias, Dir,
                          [ file_type(directory),
                            access(read)
                          ]),
       set_flag(Name, Dir).

:- set_dir(hdrug_library,alpino('Hdrug/Tcl')).
:- set_dir(tex_library,alpino('Hdrug/Tex')).

:- set_flag(suite,n).
:- set_flag(generation_suite,n).

:- use_module(alpino('src/start')).

:- veryfast_options.
:- set_flag(display_quality,off).
:- set_flag(debug,0).
:- set_flag(display_main_parts,off).
:- set_flag(tk_main_loop_in_thread,off).

%% alpino_parse(+Zin,-Result)
%% alpino_parse(+Zin,-Result,+Options)
%%
%% alpino_gui
%%
%% options are:
%% Flag=Value for any of the Alpino and Hdrug flags
%% veryfast
%% fast
%% slow
%%
%% Options are reset after the predicate finishes.

:- public alpino_parse/2, alpino_parse/3.
alpino_parse(Zin,Result,Options) :-
    set_flags_remember_old(Options,ResetOptions),
    call_cleanup(alpino_parse(Zin,Result),
		 set_flags_remember_old(ResetOptions,_)
		).

alpino_parse(Zin,Result) :-
    parse(Zin),
    object(_,Result).

set_flags_remember_old([],[]).
set_flags_remember_old([H|T],[R|RT]) :-
    set_flag_remember_old(H,R),
    set_flags_remember_old(T,RT).

set_flag_remember_old(Att=Val,Att=Old) :-
    hdrug_flag(Att,Old,Val).
set_flag_remember_old(very_fast,veryfast) :-
    veryfast_options.
set_flag_remember_old(veryfast,veryfast) :-
    veryfast_options.
set_flag_remember_old(fast,veryfast) :-
    fast_options.
set_flag_remember_old(slow,veryfast) :-
    slow_options.

%%	alpino_result(?What, +Result, -Value)
%
%	Extract  high-level  descriptions  from  the   raw  result  from
%	alpino_parse/2.  What is one of:
%
%	    * dt
%	    * triples
%	    Value is a list deprel(Obj1, Rel, Obj2)
%	    * tree(Tree)
%	    Extract a tree.  Available trees are:
%	        * adt
%	        * deriv
%	        * deriv0
%	        * deriv1
%	        * dt
%	        * dtp
%	        * dts
%	        * syn
%	        * matrix(syn)
%	        * user(dt)

alpino_result(dt,o(Cat,_,_),DT) :-
    alpino_dt:result_to_dt(Cat,DT).
alpino_result(triples,o(Cat,_,_),Triples) :-
    alpino_dt:result_to_dt(Cat,DT),
    alpino_dt:dt_to_relations(DT,Triples).
alpino_result(tree(What),Object,Trees) :-
    hdrug_show:eval_paths_list([object(1, Object)], Things1, []),
    hdrug_show:extract_values(Things1, Things),
    trees(Things, What, Trees).

trees([], _, []).
trees([Thing|Things], What, [Tree|Trees]) :-
    hdrug_show:change_thing_tree(Thing, What, Tree),
    trees(Things,What,Trees).


:- public alpino_gui/0.

alpino_gui :-
    %% a gui implies interaction, therefore:
    set_flag(debug,1),
    set_flag(demo,on),

    really_start_x.


%%
%% recompile swi for improved speed:
%% set ALPINO_HOME to the directory of Alpino
%% cd pl-devel/src
%% make OPTIMISE=prof PROFILE_SRC=$ALPINO_HOME/alpino.pl\
%%                    PROFILE_GOAL=alpino:profile_goal
%%
%% this gives some errors about missing libraries, but that
%% does not seem to matter
:- public profile_goal/0.

profile_goal :-
    set_flag(suite,alpino('Suites/cdb')),
    set_flag(debug,1),
    load_suite,
    sen(1,25).
