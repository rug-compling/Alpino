:- module( hdrug_gui,
	   [ start_x/0, really_restart_x/0,
	     really_start_x/0,  restart_x/0,  halt_x/0,
	     update_types/0, update_preds/0, update_sents/0, update_lfs/0,
	     update_array/2, update_array/3, 
	     tk_retract_objects/1, tk_assert_object/1,
	     tk_fs/1, tk_fs_list/1, tk_term/1, tk_term_list/1, tk_tree/2,
	     tcl_eval/1, tcl_eval/2, tcl_eval/3, tcl/1, tcl/2, tcl/3,
	     tk_atom/2,
	     menu_flag/1, menu_flag/2,
	     notify_active_obj/1,
	     select_widget/2,
	     reposition_widget/1,
	     create_button_for_object/1,
	     create_button_for_object/2,
	     select_canvas/2,  %% obsolete, use select_widget/2 instead
	     canvas_frame/2,
	     tkconsol_larger/0,
	     tkconsol_larger/1,
	     tkconsol_smaller/0,
	     tkconsol_smaller/1,
	     gui_user_confirmation/2,
	     gui_dialog/2
	   ]).

%% Hdrug Graphical User Interface

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- multifile
    user:help_pred/3.
:- public
    user:help_pred/3.
:- discontiguous
    user:help_pred/3.

:- multifile
    user:help_hook/3.
:- public
    user:help_hook/3.

:- meta_predicate findall_atmost(?,?,:,?).

:- use_module( library(lists)).
:- use_module( hdrug_util ).
:- use_module( library(charsio)).
:- use_module( library(tcltk)).

%% starting and halting tk interface
% x starts up Tk environment, unless the option `notk' is present.

:- initialize_flag(tcltk,on).
:- initialize_flag(complex_parse_widget,on).

:- public
    help_pred/3.
:- discontiguous
    help_pred/3.

help_pred(really_start_x,"really_start_x",
"Starts the graphical user interface, even if the tcltk flag is
switched off."). 
really_start_x :-
    set_flag(tcltk,on),
    start_x.

:-  public really_restart_x/0.
really_restart_x :-
    set_flag(tcltk,on),
    restart_x.

help_pred(restart_x,"restart_x","Restarts the graphical user interface.").
restart_x :-
%%%    use_module(library(tcltk),[]),
    halt_x,
    start_x.

help_pred(halt_x,"halt_x",
"Halts the graphical user interface; Hdrug continues.").
%halt_x :-
%    if_gui((reset_tkconsol,
%	    tcl('destroy .'),
%	    tcl_interp(I),
%	    tcltk:tcl_delete(I),
%	    set_flag(gui,not_started),
%            set_flag(output_default,user)
%	   )).

halt_x :-
    reset_tkconsol,
    set_flag(gui,not_started),
    set_flag(output_default,user),
    (   tcl_interp(I)
    ->  tcltk:tcl_delete(I),
	set_flag(tcl_interp,undefined)
    ;   true
    ),
    (   current_prolog_flag(dialect,swi),
	thread_self(L),
	L \== main
    ->  thread_exit(exit_requested_by_user_from_gui)
    ;   true
    ).

load_blt :-
    hdrug_flag(blt,BLT),
    hdrug_flag(blt_library,LIB),
    load_blt(BLT,LIB).

load_blt(noblt,_).
load_blt(undefined,_).
load_blt(off,_).
load_blt(blt,BltLib) :-
    (	BltLib \== ''
    ->	catch((	 
		 tcl('load ~a',[BltLib]),
		 format(user_error,"BLT extension loaded~n",[])
	      ),
	      tcl_error(_,ErrorString),
	      (	 format(user_error,"Could not load BLT: ~s~n",
			[ErrorString]),
		  format(user_error,"Maybe old Tcl/Tk version?~n",[])
	      ))
    ;	format(user_error,"BLT library not specified, ",[]),
	format(user_error,"BLT extension not loaded ~n",[]),
	format(user_error,
	       "(but maybe it's already loaded statically)~n",[])
    ). 

:- if(current_prolog_flag(dialect,swi)).

%%% this one returns result (both status and instantiated prolog_variables)
define_prolog_q :-
    tcl("
  proc prolog_q {agoal} {
     global term_socket
     global tcl_eval_mode
     if {$tcl_eval_mode} {
         puts  $term_socket goal			  
         puts  $term_socket execute_boolean_result($agoal).
         flush $term_socket
     } else {
         prolog_event execute_boolean_result($agoal)
     }
     gets  $term_socket term 
     if {$term == \"true\"} { return 1 }
     return 0
  }

  proc prolog_var {agoal var} {
     global term_socket
     global tcl_eval_mode
     if {$tcl_eval_mode} {
	 puts $term_socket goal
         puts $term_socket execute_var_result($agoal,$var).
         flush $term_socket
     } else {
         prolog_event execute_var_result($agoal,$var)
     }
     gets $term_socket term
     return $term			      
  }

			
  proc prolog_all {agoal var} {
     global term_socket
     global tcl_eval_mode
     if {$tcl_eval_mode} {
         puts $term_socket goal
         puts $term_socket execute_all_result($agoal,$var).
         flush $term_socket
     } else {
         prolog_event execute_all_result($agoal,$var)
     }
     gets $term_socket term
     return $term			      
  }

			
").

:- else.

define_prolog_q :-
    tcl("
proc prolog_q {goal} {
    prolog $goal
}

proc prolog_var {call var} {
    global prolog_variables
    prolog $call
    return $prolog_variables($var)
}

proc prolog_all {call var} {
    global xxx_prolog_all
    set xxx_prolog_all {}
    prolog hdrug_gui:send_all('f($var,$call)',xxx_prolog_all)
    return $xxx_prolog_all
}


").

:- endif.


user:help_pred(start_x,"start_x",
"Attempts to start the graphical user interface, but will not start it
if hdrug_flag(tcltk) is switched off"). 
start_x :-
    hdrug_flag(tcltk,TcltkFlag),
    (	TcltkFlag == on
    ->	%%% use_module(library(tcltk),[]),
	tk_init,
	load_blt,
	%% enables hdrug library
	hdrug_flag(hdrug_library,Lib),
	tcl("set auto_path [linsert $auto_path 0 ~a]",[Lib]),
	tcl("set hdrug_library ~a",[Lib]),
	tcl("set autopath [linsert $auto_path 0 ~a/Clig]",[Lib]),
	tcl("set env(CLIG_DIR) ~a/Clig",[Lib]),
	tcl("source ~a/Clig/cliglib.tcl",[Lib]),
	tcl("set background_color wheat3"),
	tcl("set highlight_color DarkSeaGreen1"),

	define_prolog_q,
	
	set_flag(gui,started),

	%% get information about current grammar
	%% It would be too slow to ask for this dynamically.
	update_flags,
	update_types,
	update_preds,
	update_sents,
	update_lfs,
	hdrug_flag(application_name,Val),
	(   ( Val==undefined
	    ;	\+ atom(Val)
	    )
	->  AppName='Hdrug'
	;   AppName=Val
	),
	tcl('set HDRUG ~a',[AppName]),
	hdrug:gram_startup_hook_begin,
	tcl('source $hdrug_library/hdrug.tcl'),
	format(user_error,"Updating objects...",[]),
	create_objects,
	format(user_error,"Done.~n",[]),
	format(user_error,"Updating help items...",[]),
	tcl_interp(Interp),
	hdrug_flag(add_help_menu,On),
	(   On==off
	->  true
	;   hdrug_help:help_add_to_menu('.menu.help.m',Interp)
	),
	format(user_error,"Done.~n",[]),
	start_tkconsol,
	hdrug:gram_startup_hook_end,
	format(user_error,"Updating menu items...",[]),
        set_flag(output_default,clig),
	add_hdrug_menu_flags,
	add_menu_flags,
	format(user_error,"Done.~n",[]),
	tk_main_loop_if_no_top_level_events
    ;	true
    ).

%% Windows systems do not support top_level events (i.e. allowing
%% tcltk to interpret events while SICStus is waiting for input).

:- if(current_prolog_flag(language,sicstus)).

tk_main_loop_if_no_top_level_events :-
    prolog_flag(host_type,Host),
    (   sub_atom(Host,_,_,_,win32)
    ->  add_toplevel,
	format(user_error,"~nInitializing gui finished, welcome to Hdrug!~n",
	       []),
        tcltk:tk_main_loop
    ;   true
    ).

:- endif.

:- if(current_prolog_flag(dialect,swi)).

:- initialize_flag(tk_main_loop_in_thread,on).
%% todo: for SWI add toplevel in separate thread?
tk_main_loop_if_no_top_level_events :-
    at_halt(halt_x),
    hdrug_flag(tk_main_loop_in_thread,Threading),
    format(user_error,"~nInitializing gui finished, welcome to Hdrug!~n",
	   []),
    tcl_interp(Interp),
    (   Threading == on
    ->  thread_create(tcltk:tk_main_loop(Interp),_,[alias=hdrug_gui])
    ;   add_toplevel,
	tcltk:tk_main_loop(Interp)
    ).

:- endif.

add_toplevel :-
    tcl("\
button .t.prolog -text {Prolog Command} -command {
    prolog $module:hdrug_runtime_cmd_interpreter }
pack .t.prolog -side right",[]).   

update_flags :-
    (	hdrug_flag(X),
	tk_flag(X),
	fail
    ;   true
    ).

update_types :-
    format(user_error,"Updating types...",[]),
    tcl("set types(max) 0"),
    findall(Type,hook(hdrug_feature:define_type(Type,_,_,_,_)),All0),
    sort(All0,All),
    update_array(All,types),
    format(user_error,"Done.~n",[]).

update_preds :-
    format(user_error,"Updating preds...",[]),
    tcl("set preds(max) 0"),
    findall(Spec, user_clause_functor(Spec), All0),
    sort(All0,All),
    update_array(All,preds),
    format(user_error,"Done.~n",[]).

user_clause_functor(F/A):-
    a_user_clause(F/A,_,_).

user:help_pred(update_array,"update_array(+List,+ArrayName)",
"a Tcl array named ArrayName is constructed where the values in List
are to be the values in the array, i.e. ArrayName(1), ArrayName(2),
etc.; the special value ArrayName(max) is set to the last index of the
array (counting starts at 0). The flag update_array_max can be used to
pass to Tcl only the first N items. If that value is 0 then all items
are passed on (default=1000)."). 

:- initialize_flag(update_array_max,1000).

update_array(Values0,Var) :-
    hdrug_flag(update_array_max,Max),
    atmost_prefix(Max,Values0,Values),
    update_array(Values,Var,0).

%update_array(Values,Var,Int)
update_array([],Var,Int) :-
    tcl("set ~w(max) ~w",[Var,Int]).
update_array([H0|T],Var,Int) :-
    term_atom(H0,H),
    tcl("set ~w(~w) {~w}",[Var,Int,H]),
    Int2 is Int+1,
    update_array(T,Var,Int2).

update_sents :-
    hdrug_flag(complex_parse_widget,OnOff),
    update_sents(OnOff).

update_sents(off).
update_sents(on) :-
    hdrug_flag(update_array_max,Max),
    findall_atmost(Max,S,a_sentence(S),Ss0),
    (   Ss0 = [_|_]
    ->  to_tk_sentences(Ss0,Ss),
	format(user_error,"Updating sentences...",[]),
	tcl("set sents(max) 0"),
	update_array(Ss,sents),
	format(user_error,"Done.~n",[])
    ;   true
    ).

update_lfs :-
    format(user_error,"Updating logical forms...",[]),
    tcl("set lfs(max) 0"),
    hdrug_flag(update_array_max,Max),
    findall_atmost(Max,S,a_lf(S),Ss0),
    to_tk_lfs(Ss0,Ss),
    update_array(Ss,lfs),
    format(user_error,"Done.~n",[]).

a_lf(Lf) :-
    hdrug:a_lf(_,Lf).

to_tk_lfs([],[]).
to_tk_lfs([Lf0|Lfs0],[Lf|Lfs]) :-
    hdrug:extern_sem(Lf0,Lf),
    to_tk_lfs(Lfs0,Lfs).

a_sentence(S) :-
    hdrug:a_sentence(_,S).

%% problem: sentences which contain {}
%% if we escape these, we must also un-escape them later
%% for now: simply skip those sentences
to_tk_sentences([],[]).
to_tk_sentences([H0|T0],[H|T]) :-
    to_tk_sentence(H0,H),
    to_tk_sentences(T0,T).

to_tk_sentence(S0,S) :-
    create_list_of_atoms(S0,Chars,[]),
    atom_codes(S1,Chars),
    (   sub_atom(S1,_,_,_,'{')
    ->  S = 'SKIPPED'
    ;   sub_atom(S1,_,_,_,'}')
    ->  S = 'SKIPPED'
    ;   S1 = S
    ).


create_list_of_atoms([],C,C).
create_list_of_atoms([H|T],C0,C) :-
    create_list_of_atoms_(T,H,C0,C).

create_list_of_atoms_([],H,C0,C) :-
    tk_atom(H,H1),
    charsio:format_to_chars("~w",[H1],C0,C).
create_list_of_atoms_([H|T],F,C0,C) :-
    charsio:format_to_chars("~w ",[F],C0,C1),
    create_list_of_atoms_(T,H,C1,C).


%% predicates called from Prolog
%%
%% i.e. these should succeed if tk interface is not started.

%% these predicates are used in go.pl
%% tk_retract_objects/1
%% tk_assert_object/1
%% tk_send_a_sentence/1
%% tk_send_a_lf/1

tk_retract_objects(_) :-
    if_gui(tk_retract_objects0).

tk_retract_objects0 :-
    tcl("hdrug_delete_objects",[]).

tk_assert_object(No) :-
    if_gui(tk_assert_object0(No),
           create_new_object_message(No)
	   ).

create_new_object_message(No) :-
    (   (  No < 10
        ;  No < 100,
           0 is mod(No,10)
        ;  No < 1000,
           0 is mod(No,100)
        ;  No < 10000,
           0 is mod(No,1000)
        ;  0 is mod(No,10000)
        )
    ->  hdrug_util:debug_message(1,"alpino: created object: ~w~n",[No])
    ;   true
    ).

tk_assert_object0(No) :-
    create_button_for_object(No).

create_objects :-
    hdrug:find_current_no(NextNo),
    MaxNo is NextNo-1,
    hdrug_flag(gui_max_objects,Max),
    (   Max < MaxNo
    ->  MyMax = Max
    ;   MyMax = MaxNo
    ),
    (   hdrug_util:between(1,MyMax,No),
	create_button_for_object(No,Max),
	fail
    ;   true
    ),
    tcl("set flag(number_of_objects) ~w",[MaxNo]).

:- initialize_flag(gui_max_objects,30).

create_button_for_object(No) :-
    hdrug_flag(gui_max_objects,Max),
    create_button_for_object(No,Max).
    
create_button_for_object(No,Max) :-
    tcl("set flag(number_of_objects) ~w",[No]),
    (	No > Max
    ->	true
    ;	No =:= Max
    ->	tcl("catch {destroy .bb.o.obj~w}
	     button .bb.o.obj~w -padx 0.5 -pady 0.5 -text {>} -command {
	              prolog hdrug_gui:create_more_buttons(~w) }
             pack .bb.o.obj~w -side left",[Max,Max,Max,Max]),
	tcl('set add_object 0
catch { destroy .bb.text .bb.label .bb.omax .bb.omaxv }
label .bb.label -text {add button for object:}
label .bb.omax -text {objects}
label .bb.omaxv -textvariable flag(number_of_objects) -width 8
entry .bb.text -textvariable add_object -width 10
pack .bb.omax .bb.omaxv .bb.text .bb.label -side right
bind .bb.text <Return> {if [prolog hdrug:object($add_object,_)] { hdrug_object $add_object} }',[])
    ;   create_button_for_any_object(No)
    ),
    tcl(update).

create_button_for_any_object(No) :-
    tcl("hdrug_object ~w",[No]).

:- public create_more_buttons/1, create_less_buttons/1.
create_more_buttons(OldMax) :-
    hdrug_flag(gui_max_objects,Inc),
    NewMax is OldMax+Inc,
    tcl("hdrug_delete_objects",[]),
    tcl("button .bb.o.obj1  -padx 0.5 -pady 0.5 -text {<} -command {
                    prolog hdrug_gui:create_less_buttons(~w) }",[OldMax]),
    tcl("pack .bb.o.obj1 -side left",[]),
    (	between(OldMax,NewMax,N),
	hdrug:object(N,_),
	create_button_for_object(N,NewMax),
	fail
    ;	true
    ).


create_less_buttons(OldMax) :-
    hdrug_flag(gui_max_objects,Inc),
    NewMax is OldMax-Inc,
    tcl("hdrug_delete_objects",[]),
    (	NewMax > 0
    ->	tcl("button .bb.o.obj1  -padx 0.5 -pady 0.5 -text {<} -command { 
                        prolog hdrug_gui:create_less_buttons(~w) }",[NewMax]),
	tcl("pack .bb.o.obj1 -side left",[])
    ;	true
    ),
    (	between(NewMax,OldMax,N),
	hdrug:object(N,_),
	create_button_for_object(N,OldMax),
	fail
    ;	true
    ).


%% treatment of canvases
%% there are two `standard' canvases, and an infinite number of new
%% toplevel canvases.
%% 
%% the name of the standard canvases is available thru the value of
%% the hdrug_flag(canvas) and hdrug_flag(canvas2)
%%
%% the current canvas will be the value of canvas passed on to the
%% different libraries. In the libraries I don't want to see those
%% flags predicates anymore (at best a local one ..)
%%
%% for each type of output, one of the standard canvases is used as
%% the default. The boolean value of hdrug_flag(use_default_canvas) indicates
%% whether the default canvas or a new one should be used.

create_top_canvas(Canvas) :-
    create_top_canvas(Canvas,_).

create_top_canvas(Canvas,W) :-
    gen_sym(W,'.cs'),
    tcl("create_top_canvas ~a",[W],Canvas).

user:help_pred(tk_fs,"tk_fs(+Term)",
"Term is displayed as a feature-structure on the canvas widget of the
graphical user interface"). 
user:help_pred(tk_fs_list,"tk_fs(List)",
"Each Term in List is displayed as a feature-structure on the canvas
widget of the graphical user interface"). 
tk_fs(Thing) :-
    if_gui( (   select_widget(fs,Frame),
		hdrug_tk:tk_fs(Thing,Frame),
		reposition_widget(fs)
	    )
	  ).

%% Nov 23, 2000: reverse list, since tk is used as a stack.
tk_fs_list(List0) :-
    lists:reverse(List0,List),
    tk_fs_list_(List).

tk_fs_list_([]).
tk_fs_list_([H|T]) :-
    tk_fs(H),
    tk_fs_list_(T).

%%% this is really a mess: we are having a text-widget here, but
%%% our scrollbars only send messages to the canvas. Solution:
%%% make a very large text widget .... However, now it is not so clear
%%% how to automatically `watch' the insertion cursor.
%%%
%%% other possible solution: don't use text widget at all, but use tk_fs
%%%    currently tk_fs is not very good at terms yet...
user:help_pred(tk_term,"tk_term(?Term)","Term is displayed on the canvas of the graphical user interface").
tk_term(Thing) :-
    if_gui( (   select_widget(text,Text),
		tcl("text ~a -wrap char -width 3000 -height 10 -bd 0",[Text]),
		hdrug_tk:tk_term(Thing,Text),
		reposition_widget(text)
	    )
	  ).  %% -height 1500 

tk_term_list(Thing) :-
    if_gui( (   select_widget(text,Text),
		tcl("text ~a -wrap char -width 3000 -height 10 -bd 0",[Text]),
		hdrug_tk:tk_term_list(Thing,Text),
		reposition_widget(text)
	   )
	 ).

help_pred(tk_tree,"tk_tree(+TreeFormat,?Term)",
"Term is displayed as a tree according to the TreeFormat declarations
on the canvas of the graphical user interface. The TreeFormat should
be specified by means of clauses for the hook predicates graphic_path,
graphic_label, graphic_daughter."). 
tk_tree(Mode,FS) :-
    if_gui(  (   select_widget(tree(Mode),Frame),
		 hdrug_tk:tk_tree(Mode,FS,Frame),
		 reposition_widget(tree(Mode))
	     )
	  ).

canvas_frame(Canvas,Frame) :-
    hdrug_flag(grr,N0),
    N is N0+1,
    set_flag(grr,N),
    charsio:format_to_chars('~w.f.f~w.f',[Canvas,N0],FrameChars),
    atom_codes(Frame,FrameChars),
    tcl('winfo exists ~w.f',[Canvas],Return),
    (	Return=='0'
    ->	tcl('
	   empty_canvas ~a
	   frame ~a.f
	   pack ~a.f -side left -anchor n -expand 1 -fill x
	   ~a create window 20 5 -window ~a.f -anchor nw
           ~a configure -width [lindex [.cv.canvas configure -width] 4]
           ~a configure -height [lindex [.cv.canvas configure -height] 4]
	   ',[Canvas,Canvas,Canvas,Canvas,Canvas,Canvas,Canvas])
    ;   true
    ),
    %% TODO: throw away oldest sub-frame after N frames have been added
    tcl('
       frame ~w.f.f~w
       frame ~w.f.g~w -height 40
       pack ~w.f.g~w ~w.f.f~w -side bottom -anchor nw -expand 0
       catch {destroy ~w}
       ',
       [Canvas,N0,Canvas,N0,Canvas,N0,Canvas,N0,Frame]).
    
select_widget(Type,Widget) :-
    select_canvas(Type,Canvas),
    canvas_frame(Canvas,Widget).

reposition_widget(Type) :-
    tcl("global use_canvas ; set use_canvas",[],Return),
    (	Return == '1'
    ->	true
    ;	select_standard_canvas(Type,Canvas),
	tcl('~w yview moveto 0',[Canvas]),
	tcl('~w xview moveto 0',[Canvas])
    ).

select_canvas(Type,Canvas) :-
    tcl("global use_canvas ; set use_canvas",[],Return),
    (	Return == '1'
    ->	create_top_canvas(Canvas)
    ;	select_standard_canvas(Type,Canvas)
    ).

user:help_hook(use_canvas,"use_canvas(+Mode,LeftRightTop)",
"Mode is a term indicating the type of data-structure to be displayed. It
is one of tree(TreeMode), fs, text, chart, stat. The predicate should 
instantiate the second argument as one of the atoms left, right or top 
(for a new widget).").

select_standard_canvas(Mode,Canvas) :-
    hdrug:use_canvas(Mode,Val),
    select_standard_canvas0(Val,Mode,Canvas).

:- hdrug_util:initialize_flag(canvas,'.cv.canvas').
:- hdrug_util:initialize_flag(canvas2,'.cw.canvas').

select_standard_canvas0(top,_,Canvas) :-
    create_top_canvas(Canvas).
select_standard_canvas0(left,_,Canvas) :-
    hdrug_flag(canvas,Canvas).
select_standard_canvas0(right,_,Canvas) :-
    hdrug_flag(canvas2,Canvas).
select_standard_canvas0(system,tree(_Mode),Canvas):-
    hdrug_flag(canvas,Canvas).
select_standard_canvas0(system,fs,Canvas):-
    hdrug_flag(canvas2,Canvas).
select_standard_canvas0(system,text,Canvas) :-
    hdrug_flag(canvas2,Canvas).
select_standard_canvas0(system,stat,Canvas) :-
    create_top_canvas(Canvas).
select_standard_canvas0(system,chart,Canvas) :-
    create_top_canvas(Canvas).

:- public a_treedef/1.  % from hdrug.tcl
a_treedef(TreeDef) :-
    findall(G,hdrug:graphic_path(G,_,_),Gs0),
    sort(Gs0,Gs),
    member(TreeDef,Gs),
    TreeDef \== type,    %% hdrug(p_type), not for objects
    TreeDef \== t.       %% hdrug(call_tree), not for objects

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% utilities %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%

user:help_pred(tcl_eval,"tcl_eval(+Cmd[,-Return])",
"Abbrevation for the tcltk library predicate tcl_eval/3. The current
TclTk interpreter, accessible through the tcl_interp flag, is added as
the first argument."). 
tcl_eval(Cmd) :-
	tcl_interp(I),
	tcl_eval(I,Cmd,_).
tcl_eval(Cmd,Return) :-
	tcl_interp(I),
	tcl_eval(I,Cmd,Return).
%tcl_eval(I,Cmd,Return) :-
%    tcltk:tcl_eval(I,Cmd,Return).

user:help_pred(tcl,"tcl(+Expr[,+Subs[,-ReturnAtom]])",
"Expr is a string as accepted as the second argument of format/3; the
optional Subs is equivalent to the third argument of format/3. After
evaluating the meta-charcters in Expr, the string is sent as a tcl
command using the current tcl interpreter (flag tcl_interp). The
return string is turned into an atom and available in the optional
third argument."). 
tcl(Expr) :-
    tcl(Expr,[],_).

tcl(Expr,Subs) :-
    tcl(Expr,Subs,_).

tcl(Expr,Subs,Return) :-
    tcl_interp(Interp),
    tcl_eval(Interp,format(Expr,Subs),Return0),
    atom_codes(Return,Return0).

%% we only support a single gui
tk_new(Options) :-
    (   tcl_interp(I)
    ->	tcltk:tcl_delete(I)
    ;	true
    ),
    tcltk:tk_new(Options,Int),
    set_flag(tcl_interp,Int).

%% tk_atom(Term,Atom)
%% converts between Prolog term and atom with appropriate
%% symbols escaped for Tcl/Tk 

% don't trust this!
tk_atom(A,B) :-
	integer(A),!,
	number_codes(A,Astr),
	atom_codes(B,Astr).
tk_atom(A0,A) :-
	term_atom(A0,A1),
	name(A1,Chars1),
	tk_escape_l(Chars1,Chars),!,
	name(A,Chars).

tk_escape_l([],[]).
tk_escape_l([H|T],Out):-
	tk_escape_c(H,Rest,Out),
	tk_escape_l(T,Rest).

% 92: \

% 59: ;
% 32: <SPACE>
% 34: "
% 91: [
% 93: ]
% 123 {
% 125 }
% 36: $
tk_escape_c(34,R,[92,34|R]).
tk_escape_c(59,R,[92,59|R]).
tk_escape_c(91,[93|R],[92,91,92,32,92,93|R]).
tk_escape_c(91,R,[92,91|R]).
tk_escape_c(92,R,[92,92|R]).
tk_escape_c(93,R,[92,93|R]).
tk_escape_c(32,R,[92,32|R]).
tk_escape_c(123,R,[92,123|R]).
tk_escape_c(125,R,[92,125|R]).
tk_escape_c(36,R,[92,36|R]).

% catch all:
tk_escape_c(C,R,[C|R]).

%escape_double_quote([],[]).
%escape_double_quote([H|T],Rest) :-
%    escape_dq(H,Rest,Rest1),
%    escape_double_quote(T,Rest1).
%
%escape_dq(C,Rest,Rest1) :-
%    (	C =:= 34
%    ->  Rest=[92,34|Rest1]
%    ;	C =:= 123
%    ->	Rest=[92,123|Rest1]
%    ;	C =:= 125
%    ->	Rest=[92,125|Rest1]
%    ;	C =:= 36
%    ->	Rest=[92,36|Rest1]
%    ;	Rest=[C|Rest1]
%    ).

%%%%%%%%%%%%%%%%%%%%%%
%% Global variables %%
%%%%%%%%%%%%%%%%%%%%%%

% sets the flag Flag, and passes the flag to Tcl/Tk
% flag is supposedly ground
tk_flag(Flag) :-
    hdrug_flag(Flag,Val),
    if_gui(tcl("set flag(~q) {~q}",[Flag,Val])). % then send it to tk

% menu_flag(Flag,Values)
% adds a radiobutton for Flag under the Options menu, with Values
% an instantiated list of possible values
:- use_module(library(lists)).

menu_flag(Flag) :-
    menu_flag(Flag,[]).

menu_flag(Flag,Values) :-
    retractall(hdrug_gui_menu:menu_flag(Flag,_)),
    assertz(hdrug_gui_menu:menu_flag(Flag,Values)).

add_menu_flag(Flag,Values0) :-
	hdrug_flag(Flag,Value),
	(   member(Value,Values0)
	->  Values0=Values
	;   Values=[Value|Values0]
	),
	tcl("set flag(~w) ~w",[Flag,Value]),
	menu_flag('.menu.options.m',Flag,Values).

:- discontiguous
    hdrug_menu_flag/1,
    hdrug_menu_flag/2.

hdrug_menu_flag( top_features,        [ undefined | Tail ] ) :-
    findall(Top, hdrug:top(Top,_), Tail).
hdrug_menu_flag( parser,              List ) :-
    findall(P,   hdrug_util:hdrug_flag(parser(P)), List).
hdrug_menu_flag( generator,              List ) :-
    findall(P,   hdrug_util:hdrug_flag(generator(P)), List).
hdrug_menu_flag( useful_try_check,    [ on, off ]).
hdrug_menu_flag( nodeskip,            [ 0,1,2,3,4,5,6,7,8,9,10]).
hdrug_menu_flag( demo,                [ on, off ]).
hdrug_menu_flag( object_saving,       [ on, off, semi ]).
hdrug_menu_flag( object_exists_check, [ on, off ]).
hdrug_menu_flag( max_objects,         [ 1,10,40,100,1000000]).
hdrug_menu_flag( gui_max_objects,     [ 30,40,50,60 ]).
hdrug_menu_flag( application_name ).
hdrug_menu_flag( blt_graph_lines,     [ on, off]).
hdrug_menu_flag( debug,               [ 0, 1, 2]).
hdrug_menu_flag( clig_tree_hspace,    [3,5,10,20,30,40,50 ]).
hdrug_menu_flag( clig_tree_vspace,    [3,5,10,20,30,40,50 ]).
hdrug_menu_flag( clig_tree_active_nodes, [ on,off]).
hdrug_menu_flag( print_table_total,   [ on, off]).
hdrug_menu_flag( start_results_within_bound ).
hdrug_menu_flag( end_results_within_bound ).
hdrug_menu_flag( incr_results_within_bound ).
hdrug_menu_flag( complex_parse_widget, [on, off]).
hdrug_menu_flag( tcltk ).
hdrug_menu_flag( tkconsol,            [ on, off] ).
hdrug_menu_flag( dot_program,         [ ghostview, gv, xv, dotty, user ]).

hdrug_menu_flag(Flag,[]) :-
    hdrug_menu_flag(Flag).

:- dynamic hdrug_gui_menu:menu_flag/2.

add_menu_flags :-
    findall(Flag-Values,hook(hdrug_gui_menu:menu_flag(Flag,Values)),List0),
    sort(List0,List),
    (   lists:member(Flag-Values,List),
	add_menu_flag(Flag,Values),
	fail
    ;   true
    ).

add_hdrug_menu_flags :-
    findall(Flag-Values,hdrug_menu_flag(Flag,Values),List0),
    sort(List0,List),
    (   lists:member(Flag-Values,List),
	add_hdrug_menu_flag(Flag,Values),
	fail
    ;   true
    ).

add_hdrug_menu_flag(Flag,Values0) :-
	hdrug_flag(Flag,Value),
	(   member(Value,Values0)
	->  Values0=Values
	;   Values=[Value|Values0]
	),
	tcl("set flag(~w) ~w",[Flag,Value]),
	menu_flag('.menu.hdrug.m',Flag,Values).

menu_flag(Menu,Flag,Values) :-
	tcl('~w add cascade -label "~w" -menu ~w.~w',
	    [Menu,Flag,Menu,Flag]),
	tcl('menu ~w.~w',[Menu,Flag],SubMenu),
	menu_flag_values(Values,Flag,SubMenu).

menu_flag_values([],Flag,Menu) :-
	tcl('~w add command -label Help -command "prolog hdrug_gui:tk_help_flag(~w)"',
	    [Menu,Flag]).
menu_flag_values([V0|V],Flag,Menu) :-
	tcl('~w add radiobutton -label ~w -value ~w -variable flag(~w) \
	      -command "prolog hdrug_util:hdrug_flag(~w,_,~w) "',
	      [Menu,V0,V0,Flag,Flag,V0]),
	menu_flag_values(V,Flag,Menu).

tk_init :-
	tk_new([top_level_events]).

:- public tk_help_flag/1.
tk_help_flag(Flag) :-
    (	user:help_flag(Flag,Text)
    ->  tcl("tk_dialog .dialog {Help on ~w} {~s} {} 0 OK",[Flag,Text])
    ;	tcl("tk_dialog .dialog {Help} {No help available for ~w} {} 0 OK",
	    [Flag])
    ).

:- if(current_prolog_flag(language,sicstus)).
:- public send_all/2.
send_all(Term,TclVar):-
    atom_term(Term,f(Var,Call)),
    send_all(Var,Call,TclVar).

%% used by hdrug.tcl
send_all(Var,Call,TclVar):-
    findall(Var,hook(user:Call),Vars0),
    wrap_write(Vars0,Vars),
    tcl_eval([set,TclVar,br(Vars)]).

wrap_write([],[]).
wrap_write([H|T0],[write(H)|T]) :-
    wrap_write(T0,T).
:- endif.

start_tkconsol :-
    hdrug_flag(tkconsol,Val),
    start_tkconsol(Val).

:- if(current_predicate(tk_terminal/5)).
start_tkconsol(off).
start_tkconsol(undefined).
start_tkconsol(on) :-
    tcl('frame .cons
         text .cons.console -width 80 -height 10 -yscrollcommand ".cons.sb set"
         bind Text <Control-u> {
             tkTextSetCursor %W {insert linestart}
         if {[%W compare insert == {insert lineend}]} {
             %W delete insert
         } else {
             %W delete insert {insert lineend}
         }
	 }
         scrollbar .cons.sb -command ".cons.console yview"
         pack .cons.sb -side left -fill both
         pack .cons.console -fill both -expand 1  -side left
         pack .cons -fill both  -after .t
',[]),
    tcl_interp(Interp),
    (	catch(tcltk:tk_terminal(Interp,".cons.console",InS,OutS,ErrS),
	      Error,
	      (	 print_message(error,Error),
		  fail
	      )
	     )
    ->	prolog_flag(user_input, InOld, InS),
	prolog_flag(user_output, OutOld, OutS),
	prolog_flag(user_error, ErrOld, ErrS),
	set_flag(tkconsol_streams,old(InOld,OutOld,ErrOld)),
	set_input(InS),
	set_output(OutS),
	format(user_error,"TkConsol started (experimental)~n",[]),
	tcl('.menu.file.m add separator'),
	tcl('.menu.file.m add command -label "Enlarge Console (1 line)" \
	   -command { .cons.console configure -height \
             [expr [lindex [.cons.console configure -height] 4]+1]
	   }',[]),
	tcl('.menu.file.m add command -label "Enlarge Console (5 lines)" \
	   -command { .cons.console configure -height \
             [expr [lindex [.cons.console configure -height] 4]+5]
	   }',[]),
	tcl('.menu.file.m add command -label "Shrink Console (1 line)" \
	   -command { .cons.console configure -height \
             [expr [lindex [.cons.console configure -height] 4]-1]
	   }',[]),
	tcl('.menu.file.m add command -label "Shrink Console (5 lines)" \
	   -command { .cons.console configure -height \
             [expr [lindex [.cons.console configure -height] 4]-5]
	   }',[])
        
    ;   tcl('destroy .cons'),
	format(user_error,"ERROR: Cannot start tkconsol~n",[])
    ).

:- else.
start_tkconsol(_).
:- endif.

tkconsol_larger :-
    tkconsol_larger(1).

tkconsol_larger(I) :-
    integer(I),
    tcl('.cons.console configure -height \
             [expr [lindex [.cons.console configure -height] 4]+~w]',[I]).

tkconsol_smaller :-
    tkconsol_smaller(1).

tkconsol_smaller(I) :-
    integer(I),
    tcl('.cons.console configure -height \
             [expr [lindex [.cons.console configure -height] 4]-~w]',[I]).

reset_tkconsol :-
    hdrug_flag(tkconsol_streams,Old,undefined),
    reset_tkconsol(Old).

reset_tkconsol(undefined).
reset_tkconsol(old(In,Out,Err)):-
    prolog_flag(user_input,_,In),
    prolog_flag(user_output,_,Out),
    prolog_flag(user_error,_,Err),
    set_input(In),
    set_output(Out).


%% TODO: do something special for those numbers which not currently have
%% an obj button, e.g., because > 39.
notify_active_obj(No) :-
    if_gui(notify_active_obj0(No)).

notify_active_obj0(No) :-
    hdrug_flag(active_obj,No0,No),
    (	No0 == undefined
    ->  true
    ;   tcl('catch {.bb.o.obj~w configure -background [option get .bb.o.obj~w background Background]}',[No0,No0])
    ),
    tcl('if {![winfo exists .bb.o.obj~w]} { hdrug_object ~w } ',[No,No]),
    tcl('catch {.bb.o.obj~w configure -background [option get .bb.o.obj~w highlightBackground HighlightBackground]}',[No,No]).

%tk_help_message(Class,Key) :-
%    gen_sym(W,'.dt'),
%    user:help_info(Class,Key,Usage,Expl),
%    tcl("display_text ~w {Help on ~w ~w} {~s} {~s}",
%	[W,Class,Key,Expl,Usage]).

:- public spy_atom/1, nospy_atom/1.
spy_atom(Atom) :-
    atom_term(Atom,Term),
    user:spy(Term).

nospy_atom(Atom) :-
    atom_term(Atom,Term),
    user:nospy(Term).

tcl_interp(Val):-
    hdrug_flag(tcl_interp,Val),
    (	Val==undefined
    ->  fail
    ;   true
    ).


atmost_prefix(Max,Values0,Values) :-
    (	Max =:= 0
    ->	Values0=Values
    ;   length(Values0,Len),
	(   Len > Max
	->  length(Values,Max),
	    lists:append(Values,_,Values0)
	;   Values0=Values
	)
    ).

findall_atmost(Max,V,Goal,Vs) :-
    Max < 1, !,
    findall(V, Goal, Vs).
findall_atmost(Max,V,Goal,Vs) :-
    findall(V,atmost(Max,Goal),Vs).

%% sicstus has create_mutable etc, but the resulting
%% code is *much* slower with mutables than the old
%% version below. So, having the predicate is not
%% enough reason to use the first version, it is
%% per prolog a matter of experimenting.
:- if(current_prolog_flag(dialect,swi)).

atmost(Max,Goal) :-
    State = count(0),
    Goal,
    arg(1, State, C0),
    C1 is C0+1,
    (   C1 == Max
    ->  !
    ;   nb_setarg(1, State, C1)
    ).

:- else.

atmost(Max,Goal) :-
    hdrug_util:gensym(Ref),
    bb_put(utils_sol:Ref,1),
    call_cleanup(atmost0(Max,Ref,Goal),
		 (   bb_delete(utils_sol:Ref,_),
		     hdrug_util:freesym(Ref)
		 )
		).

atmost0(Max,Ref,Goal):-
    call(Goal),
    bb_get(utils_sol:Ref,N),
    (	N  =:= Max
    ->  !,
	hdrug_util:debug_message(1,"atmost: found all ~w solutions~n",[Max])
    ;   N1 is N+1,
	bb_put(utils_sol:Ref,N1)
    ).

:- endif.

%more_or_stop(Ref,Vs,Number):-
%    bb_get(sol:Ref,N),
%    (	N < Number
%    ->	N1 is N+1,
%	bb_put(sol:Ref,N1),
%	fail
%    ;   findall(V,retract(sol(Ref,V)),Vs)
%    ).

%display_sentence_key(Sent) :-
%    hdrug:a_sentence(Key,Sent),
%    format(user_error,"key: ~w~n",[Key]).

gui_user_confirmation(Msg,Args) :-
    charsio:format_to_chars(Msg,Args,Chars),
    tcl("tk_dialog .dialog {Confirmation} {~s} {} 0 NO YES",[Chars],Return),
    Return='1'.

gui_dialog(Msg,Args) :-
    charsio:format_to_chars(Msg,Args,Chars),
    tcl("tk_dialog .dialog {Alpino Message} {~s} {} 0 OK",[Chars]).

