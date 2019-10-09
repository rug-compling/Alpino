%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                         %
% written by Gertjan van Noord                            %
% (C) 1995  all rights reserved                           %
%                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(hdrug_tk,
	  [ tk_fs/2,
	    %% pp_tk/3,  used by Ale
	    tk_term/2,
	    tk_term_list/2,
	    tk_subterm/2,
	    tk_tree/3
	  ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(lists)).
:- use_module(hdrug_util).
:- use_module(library(charsio), [format_to_chars/3]).

% print a new set of structures
tk_fs(Thing0,Frame) :-
    hdrug_show:change_thing_fs(Thing0,Thing),
    hdrug_gui:tcl("frame ~a",[Frame]),
    hdrug_gui:tcl("pack ~a -anchor w -pady 10 -side bottom",[Frame]),
    write_it_fs_tk(Thing,Frame).

write_it_fs_tk(value(FS,C),Frame) :-
    hdrug_gui:tcl("frame ~w.top",[Frame],Top),
    hdrug_gui:tcl("frame ~w.bot",[Frame],Bot),
    hdrug_gui:tcl("pack ~w ~w -anchor w",[Top,Bot]),
    hdrug_gui:tcl("frame ~w.fs",[Top],Fs),
    hdrug_gui:tcl("label ~w.if -text {:-}",[Top],If),
    hdrug_gui:tcl("pack ~w ~w -side left",[Fs,If]),
    pp_tk(FS,Fs,''),
    write_pretty_constraint_tks(C,Bot,0).

write_it_fs_tk(value(FS),Frame) :-
	pp_tk(FS,Frame,'').

write_it_fs_tk(clause(H),Frame) :-
	hdrug_gui:tcl("frame ~w.cons",[Frame],Cons),
	hdrug_gui:tcl("label ~w.dot -text {.}",[Frame],Dot),
	hdrug_gui:tcl("pack ~w ~w -side left",[Cons,Dot]),
	write_pretty_constraint_tk(H,Cons).

write_it_fs_tk(clause(H,B),Frame) :-
	hdrug_gui:tcl("frame ~w.top",[Frame],Top),
	hdrug_gui:tcl("frame ~w.bot",[Frame],Bot),
	hdrug_gui:tcl("pack ~w ~w -anchor w",[Top,Bot]),
	hdrug_gui:tcl("frame ~w.fs",[Top],Fs),
	hdrug_gui:tcl("label ~w.if -text {:-}",[Top],If),
	hdrug_gui:tcl("pack ~w ~w -side left",[Fs,If]),
	write_pretty_constraint_tk(H,Fs), 
	write_pretty_constraint_tks(B,Bot,0).

write_pretty_constraint_tks([],_,_).
write_pretty_constraint_tks([H|T],Frame,No) :-
	(  T = [] -> End = ('.') ; End = (',') ),
	No2 is No+1,
	hdrug_gui:tcl("frame ~w~w",[Frame,No],Cons0),
	hdrug_gui:tcl("pack ~w -anchor w -pady 2",[Cons0]),
	hdrug_gui:tcl("frame ~w.tab -width 40",[Cons0],Tab),
	hdrug_gui:tcl("frame ~w.cons",[Cons0],Cons),
	hdrug_gui:tcl("label ~w.comma -text {~w}",[Cons0,End],Comma),
	hdrug_gui:tcl("pack ~w ~w ~w -side left",[Tab,Cons,Comma]),
	write_pretty_constraint_tk(H,Cons),
	write_pretty_constraint_tks(T,Frame,No2).

write_pretty_constraint_tk(true,_).   % ??
write_pretty_constraint_tk(H,Frame) :-
	H =.. [F|Args],
	write_relation(Args,F,Frame).

write_relation([],F,Frame) :-
    hdrug_gui:tcl("label ~w.functor -text {~w}",[Frame,F],Label),
    hdrug_gui:tcl("pack ~w",[Label]),
    hdrug_gui:tcl('bind ~a <Enter> "~a config -bg white"',
		  [Label,Label]),
    hdrug_gui:tcl('bind ~a <Leave> "~a config -bg [lindex [~a configure -bg] 4]"',
		  [Label,Label,Label]),
    hdrug_gui:tcl('bind ~a <1> "prolog hdrug:show_relation(''~w''/0)"',[Label,F]),
    hdrug_gui:tcl('help_line ~a {left button for definition}',[Label]).

write_relation([ArgL,ArgR],F,Frame) :-
    current_op(_,xfy,F),
    !,
    hdrug_gui:tcl("label ~w.lfunctor -text {~w}",[Frame,F],LLabel),
    hdrug_gui:tcl("frame ~w.argl",[Frame],ArgsL),
    hdrug_gui:tcl("frame ~w.argr",[Frame],ArgsR),
    hdrug_gui:tcl("pack ~w ~w ~w -side left",[ArgsL,LLabel,ArgsR]),
    write_pretty_argument_tks0([ArgL],ArgsL),
    write_pretty_argument_tks0([ArgR],ArgsR).

write_relation([H|T],F,Frame) :-
    length([H|T],Arity),
    a_user_clause(F/Arity,_,_),!,
    hdrug_gui:tcl("label ~w.lfunctor -text {~w(}",[Frame,F],LLabel),
    hdrug_gui:tcl('bind ~a <Enter> "~a config -bg white"',
		  [LLabel,LLabel]),
    hdrug_gui:tcl('bind ~a <Leave> "~a config -bg [lindex [~a configure -bg] 4]"',
		  [LLabel,LLabel,LLabel]),
    hdrug_gui:tcl('bind ~a <1> "prolog hdrug:show_relation(''~w''/~w)"',[LLabel,F,Arity]),
    % hdrug_gui:tcl('help_line ~a {left button for definition}',[LLabel]),
    hdrug_gui:tcl("label ~w.rfunctor -text {)}",[Frame],RLabel),
    hdrug_gui:tcl("frame ~w.args",[Frame],Args),
    hdrug_gui:tcl("pack ~w ~w ~w -side left",[LLabel,Args,RLabel]),
    write_pretty_argument_tks0([H|T],Args).

write_relation([H|T],F,Frame) :-
    hdrug_gui:tcl("label ~w.lfunctor -text {~w(}",[Frame,F],LLabel),
    hdrug_gui:tcl("label ~w.rfunctor -text {)}",[Frame],RLabel),
    hdrug_gui:tcl("frame ~w.args",[Frame],Args),
    hdrug_gui:tcl("pack ~w ~w ~w -side left",[LLabel,Args,RLabel]),
    write_pretty_argument_tks0([H|T],Args).

write_pretty_argument_tks0([H|T],Frame):-
	hdrug_gui:tcl("frame ~w.arg1",[Frame],Frame0),
	hdrug_gui:tcl("pack ~w -side left",[Frame0]),
	pp_tk(H,Frame0,''),
	write_pretty_argument_tks(T,Frame,2).

write_pretty_argument_tks([],_,_).
write_pretty_argument_tks([H|T],Frame,No):-
	No2 is No + 1,
	hdrug_gui:tcl("label ~w.comma~w -text {,}",[Frame,No],Comma),
	hdrug_gui:tcl("frame ~w.arg~w",[Frame,No],Arg),
	hdrug_gui:tcl("pack ~w ~w -side left",[Comma,Arg]),
	pp_tk(H,Arg,''),
	write_pretty_argument_tks(T,Frame,No2).

pp_tk(_/n='$VAR'(_),W,Aff) :-
	!,
	hdrug_gui:tcl("label ~w.label~w -text {_}",[W,Aff],L),
	hdrug_gui:tcl("pack ~w",[L]).

pp_tk(_Var/n=FS,W,Aff):-
	!,
        ppl_tk(FS,W,Aff).

pp_tk(Var/_='R',W,Aff):-
        !,
        write_var_tk(Var,W,Aff).

pp_tk(Var/y=[],W,Aff):-
        !,
        write_var_tk(Var,W,Aff).

pp_tk(Var/y='$VAR'(_),W,Aff):-
	!,
        write_var_tk(Var,W,Aff).

pp_tk('$VAR'(Var),W,Aff):-
	!,
        write_var_tk(Var,W,Aff).

pp_tk(Var/y=FS,W,Aff):-
	!,
        write_var_tk(Var,W,Aff),
	ppl_tk(FS,W,Aff).

do_not_print(_Var/n='$VAR'(_)).

ppl_tk([a(_Att,Thing)|Rest],W,Aff):-
	do_not_print(Thing),!,
	ppl_tk(Rest,W,Aff).

ppl_tk([a(type,Types)|T],W,Aff):-
	all_empty(T),
	!,
	hdrug_feature:write_as_conj(Types,PTypes),
	write_constant_tk(PTypes,W,Aff).

ppl_tk([a('BOOLEAN',_Type,Val)|T],W,Aff):-
	all_empty(T),
	!,
	hdrug_feature:give_boolean_type(Val,Exp),
	write_constant_tk(Exp,W,Aff).

:- use_module(library(charsio)).

ppl_tk([a('UNTYPED',Att,Val0)|T],W,Aff):-
	all_empty(T),!,

	(   charsio:with_output_to_chars(hdrug:catch_print_error(Att,Val0,_),Chars)
	->  atom_codes(Val,Chars)
	;   Val0=Val
	),
/*  why did we want this complication???
	(   charsio:with_output_to_chars(
		hdrug:catch_print_error(Att,Val0,_),Chars)
	->  atom_codes(Val,Chars),
	    length(Chars,Width0), Width is Width0+1
	;  Val0=Val,
	    charsio:format_to_chars("~w",[Val0],Chars),
	    length(Chars,Width)
	),
	hdrug_gui:tcl("text ~a.v_a_ll~w -relief flat -bd 0  -height 1 -width ~w",
	    [W,Aff,Width],W1),
	hdrug_gui:tcl("pack ~a.v_a_ll~w -side left",[W,Aff]),
	tk_subterm(Val,W1),
	hdrug_gui:tcl("~a.v_a_ll~w  configure -state disabled",[W,Aff]).
*/	write_constant_tk(Val,W,Aff).

ppl_tk([a(type,['.']),a(_,Head),a(_,Tail)],W,Aff) :-
	!,   %%% 
	ppl_list(Head,Tail,W,Aff).

ppl_tk([a(type,Types)|T],W,Aff):-
	!,
	hdrug_feature:write_as_conj(Types,PTypes),
	write_type_tk(PTypes,W,W2,Aff),
	ppl_tk2(T,W2,Aff).

ppl_tk([a('BOOLEAN',_Type,Val)|T],W,Aff):-
	!,
	hdrug_feature:give_boolean_type(Val,Exp),
	write_type_tk(Exp,W,W2,Aff),
	ppl_tk2(T,W2,Aff).

% how can this happen?
ppl_tk([a('UNTYPED',_Att,Val)|T],W,Aff):-
	!,
	write_type_tk(Val,W,W2,Aff),
	ppl_tk2(T,W2,Aff).

%which case was this?? ppl2_tk is not defined 
%ppl_tk([a(Att,FS)|T],W,Aff):-
%        !,
%        write_att_tk(Att,W,NewW,Aff),
%        pp_tk(FS,NewW,''),
%        ppl2_tk(T,W,Aff).

ppl_tk([],_,_) :-
	!.

% this one for tk_tree only..
ppl_tk(Atomic,W,Aff) :-
	write_var_tk(Atomic,W,Aff),!.

% this one for ale
ppl_tk(Term,W,Aff) :-
	Term=..[F|Args],
	tk_write_term(Args,F,W,Aff).

% for ale only...
tk_write_term([],F,Frame,Aff) :-
	hdrug_gui:tcl("button ~w.functor~w -text {~w} -relief flat",[Frame,Aff,F],Label),
	hdrug_gui:tcl("pack ~w -side left",[Label]).


tk_write_term([H|T],F,Frame,Aff) :-
	hdrug_gui:tcl("button ~w.lfunctor~w -text  {~w(} -relief flat",[Frame,Aff,F],LLabel),
	hdrug_gui:tcl("label ~w.rfunctor~w -text {)}",[Frame,Aff],RLabel),
	hdrug_gui:tcl("frame ~w.args~w",[Frame,Aff],Args),
	hdrug_gui:tcl("pack ~w ~w ~w -side left",[LLabel,Args,RLabel]),
	tk_write_term_argument_tks0([H|T],Args).

tk_write_term_argument_tks0([H|T],Frame):-
	hdrug_gui:tcl("frame ~w.arg1",[Frame],Frame0),
	hdrug_gui:tcl("pack ~w -side left",[Frame0]),
	pp_tk(H,Frame0,''),
	tk_write_term_argument_tks(T,Frame,2).

tk_write_term_argument_tks([],_,_).
tk_write_term_argument_tks([H|T],Frame,No):-
	No2 is No + 1,
	hdrug_gui:tcl("label ~w.comma~w -text {,}",[Frame,No],Comma),
	hdrug_gui:tcl("frame ~w.arg~w",[Frame,No],Arg),
	hdrug_gui:tcl("pack ~w ~w -side left",[Comma,Arg]),
	pp_tk(H,Arg,''),
	tk_write_term_argument_tks(T,Frame,No2).
% end ale only


	

%% ??
%% ppl_tk('$VAR'(No),W) :- write('$VAR'(No)).  % changed gj 21/7/93 5/11/93


ppl_tk2([a(_Att,Thing)|Rest],W,Aff):-
	do_not_print(Thing),!,
	ppl_tk2(Rest,W,Aff).

ppl_tk2([a(Att,FS)|T],W,Aff):-
        !,
        write_att_tk(Att,W,W2,Aff),
        pp_tk(FS,W2,''),
        ppl_tk2(T,W,Aff).

ppl_tk2([],_W,_).


ppl_list(Head,Tail,W,Aff) :-
	write_start_list_tk(W,W2,Aff),
	ppx(Head,W2,1),
	ppl_list0(Tail,W2,2),
	write_end_list_tk(W2).

ppl_list0(V/y='R',W,No) :-
	!,
	write_bar_list_tk(W),
	ppx(V/y='R',W,No).

ppl_list0(V/YN='$VAR'(_),W,No) :-
	!,
	write_bar_list_tk(W),
	ppx(V/YN='$VAR'(_),W,No).
	
ppl_list0(_Var/_YN=[a(type,[[]])],_,_) :-
	!.

% sometimes there is a reference of a tail of a list. In that case
% we should not use this rule..
ppl_list0(_Var/n=[a(type,['.']),a(_,Head),a(_,Tail)],W,No0) :-
	!,
	write_comma_list_tk(W,No0),
	ppx(Head,W,No0),
	No is No0 + 1,
	ppl_list0(Tail,W,No).

ppl_list0(V/y=Whatever,W,No) :-
%	!,
	write_bar_list_tk(W),
	ppx(V/y=Whatever,W,No).

ppx(ListEl,W,No) :-
	\+ ListEl = (_/n='$VAR'(_)),
	term_atom(No,AtomNo),
%%	concat_all([W0,'.',No],W),
	pp_tk(ListEl,W,AtomNo).

ppx(_/n='$VAR'(_),W,No) :-
	term_atom(No,AtomNo),
	write_underscore_tk(W,AtomNo).


write_att_tk(Att0,W,NewW,Aff) :-
	hdrug_gui:tk_atom(Att0,Att),
	concat_all([W,'.',Att,Aff],NewW),
        hdrug_gui:tcl("frame ~a",[NewW]),
	hdrug_gui:tcl("pack ~a -anchor w",[NewW]),
	hdrug_gui:tcl("label ~a.attl -text {~a:}",[NewW,Att]),
	hdrug_gui:tcl("pack ~a.attl -side left",[NewW]),
	hdrug_gui:tcl('bind ~a.attl <1> "catch {pack forget ~a.v_a_l}
                                         ~a.attl config -bg white"',[NewW,NewW,NewW]),
	hdrug_gui:tcl('bind ~a.attl <3> "catch {pack ~a.v_a_l}
                                         ~a.attl config -bg [lindex [~a configure -bg] 4]"',
		     [NewW,NewW,NewW,NewW]),
	hdrug_gui:tcl('help_line ~a.attl {Left to implode, right to explode}',[NewW]).

write_end_list_tk(W) :-
	hdrug_gui:tcl("label ~a.rr -text { ]}",[W]),
	hdrug_gui:tcl("pack ~a.rr -side left",[W]).


write_bar_list_tk(W) :-
	hdrug_gui:tcl("label ~a.bb -text { | }",[W]),
	hdrug_gui:tcl("pack ~a.bb -side left",[W]).

write_comma_list_tk(W,No) :-
	hdrug_gui:tcl("label ~a.cc~w -text { , }",[W,No]),
	hdrug_gui:tcl("pack ~a.cc~w -side left",[W,No]).

write_underscore_tk(W,No) :-
	hdrug_gui:tcl("label ~a.uu~w -text { _ }",[W,No]),
	hdrug_gui:tcl("pack ~a.uu~w -side left",[W,No]).

write_start_list_tk(W,W2,Aff) :-
	concat_all([W,'.v_a_l',Aff],W2),
	hdrug_gui:tcl("frame ~a -relief flat",[W2]),
	hdrug_gui:tcl("pack ~a -side left",[W2]),
	hdrug_gui:tcl("label ~a.ll -text {[ }",[W2]),
	hdrug_gui:tcl("pack ~a.ll -side left",[W2]).

write_type_tk(Type0,W,W2,Aff) :-
	hdrug_gui:tk_atom(Type0,Type),
	concat_all([W,'.v_a_l',Aff],W2),
	hdrug_gui:tcl("frame ~a -relief ridge -bd 4",[W2]),
	hdrug_gui:tcl("pack ~a -side left",[W2]),
	hdrug_gui:tcl("label ~a.typel -text ~a",[W2,Type]),
	hdrug_gui:tcl("pack ~a.typel -anchor w",[W2]).

write_var_tk(No,W,Aff) :-
	integer(No),
	hdrug_gui:tk_atom('$VAR'(No),Atom),    % works!!
        hdrug_gui:tcl("label ~a.varl~w -text ~a",[W,Aff,Atom]),
	hdrug_gui:tcl("pack ~a.varl~w -side left -fill y",[W,Aff]).

write_var_tk(No,W,Aff) :-
	var(No),
        hdrug_gui:tcl("label ~a.varl~w -text ~a",[W,Aff,'_']),
	hdrug_gui:tcl("pack ~a.varl~w -side left -fill y",[W,Aff]).


write_constant_tk(Val,W,Aff) :-
	hdrug_gui:tk_atom(Val,Atom),
	hdrug_gui:tcl("label ~a.v_a_ll~w -text ~w",[W,Aff,Atom]),
	hdrug_gui:tcl("pack ~a.v_a_ll~w -side left",[W,Aff]).


all_empty([]).
all_empty([a(_,H)|T]):-
	do_not_print(H),
	all_empty(T).

%% march 1996: a hook predicate has been added to include special
%% symbols. Example: lambda. If you define:
%% undocumented feature.
/*
tk_portray(lambda(A,B),Widget) :-
	special_font(Font),
	tk_subterm('$special'(Font,154),Widget),
	tk_subterm(A,Widget),
	tk_subterm('.',Widget),
	tk_subterm(B,Widget).

special_font('-adobe-symbol-medium-r-normal--18-180-75-75-p-107-adobe-fontspecific'). 
*/
% and then try the command:
% ?- tk_term(lambda(A,slaapt(A))). 
% you'll get something nice!
% tk_portray is like portray. The $special functor combines a font with
% the octal (if I remember correctly) number of the character.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% tk output for ordinary prolog terms %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_term_list(Thing,Widget):-
    tk_term(list(Thing),Widget).

tk_term(Thing,Widget) :-
    hdrug_gui:tcl("~a configure -state normal -wrap char",[Widget]),
    print_it(Thing,Widget),
    hdrug_gui:tcl("~a configure -state disabled",[Widget]),
    hdrug_gui:tcl("pack ~a",[Widget]).

tk_subterm(Term,Widget) :-
	write_goal(Term,1199,0,_,Widget).

print_it_list([],_Widget).
print_it_list([H|T],Widget) :-
	print_it(H,Widget),
	print_it_list(T,Widget).

print_it(list(List),Widget) :-
    print_it_list(List,Widget).

print_it(Thing0,Widget) :-
    hdrug_show:change_thing_term(Thing0,Thing),
    print_it0(Thing,Widget).

print_it0(value(T,Cons),Widget) :-
    print_it0(clause(v(T),Cons),Widget).

print_it0(value(T),Widget):-
    prettyvars(T),
    tk_begin_line(Widget),
    write_goal(T, 1199, 0, Co,Widget),
    write_fullstop(Co,Widget),
    tk_end_line(Widget).

print_it0(clause(H),Widget) :-
    prettyvars(H),
    tk_begin_line(Widget),
    portray_clause1(H, Co,Widget),
    write_fullstop(Co,Widget),
    tk_end_line(Widget).

print_it0(clause(H,B),Widget) :-
    prettyvars((H:-B)),
    tk_begin_line(Widget),
    portray_clause1((H:-B), Co,Widget),
    write_fullstop(Co,Widget),
    tk_end_line(Widget).


%%% changed the following so as to generate tk code...
%%% buggy.
%%% 

%% ADAPTED FROM:
%   File   : WRITE.PL
%   Author : Richard A. O'Keefe
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

% Priority 999 is o.k. if printed e.g. as elements of a list. /MC

%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the contkt demands it.
%   Contkt = 2'000 for alpha
%   Contkt = 2'001 for quote
%   Contkt = 2'010 for other
%   Contkt = 2'100 for punct

maybe_paren(P, Prio, _Lpar, '(', _, 2'100,Widget) :-
	P > Prio, !,
	write_tk('(',Widget).


maybe_paren(_, _, Lpar, Lpar, C, C,_).

maybe_paren(P, Prio, _, 2'100,Widget) :-
	P > Prio, !,
	write_tk(')',Widget).
maybe_paren(_, _, C, C,_).

%   maybe_space(LeftContkt, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(Ci, Co,Widget) :-
	(   Ci\/Co<2'100, Ci#Co<2'010 -> tk_tab(1,Widget)  %%put(0' )
	;   true
	).

/*
sticky_contkts(alpha, alpha).
sticky_contkts(quote, quote).
sticky_contkts(other, other).
sticky_contkts(alpha, quote).
sticky_contkts(quote, alpha).
*/

%   write_out(Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co)
%   writes out a Term in given SynStyle, LexStyle
%   at nesting depth Depth
%   in a contkt of priority Priority (that is, expressions with
%   greater priority must be parenthesized), 
%   and prefix operators =< PrePrio must be parenthesized,
%   where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

write_out(Term, _, _, _, _, _, _, Ci, 2'000,Widget) :-
	var(Term), !,
	maybe_space(Ci, 2'000, Widget),
	tk_variable(Term,Widget).
write_out('$VAR'(N), SynStyle, LexStyle, _, _, Depth, _, Ci, Co,Widget) :- !,
	Depth1 is Depth+1,
	write_VAR(N, SynStyle, LexStyle, Depth1, Ci, Co,Widget).
write_out(_, print(Limit), _, _, _, Depth, _, Ci, 2'010,Widget) :-
	Depth >= Limit, !,
	maybe_space(Ci, 2'010,Widget),
	tk_dots(Widget).
write_out('$special'(Font,Text),_,_,_,_,_,_,_,2'000,Widget) :-
	!,
	special_font(Font,Text,Widget).
write_out(Term,_,_,_,_,_,_,_,2'000,Widget) :-
	hdrug:tk_portray(Term,Widget),!.
write_out(Atom, _, LexStyle, _, PrePrio, _, _Lpar, _, 2'100,Widget) :-
	atom(Atom),
	current_prefixop(Atom, P, _),
	P =< PrePrio, !,
	write_tk('(',Widget),
	write_atom(LexStyle, Atom, 2'100, _,Widget),
	write_tk(')',Widget).
write_out(Atom, _, LexStyle, _, _, _, _, Ci, Co,Widget) :-
	atom(Atom), !,
	write_atom(LexStyle, Atom, Ci, Co,Widget).
write_out(N, _, _, _, _, _, _, Ci, 2'000,Widget) :-
	number(N), !,
	(   N < 0 -> maybe_space(Ci, 2'010,Widget)
	;   maybe_space(Ci, 2'000,Widget)
	),
	% hdrug_gui:tcl("~a insert end ~w",[Widget,N]).
	tcl_insert(Widget,N).

write_out(Term, noop, LexStyle, _, _, Depth, _, Ci, 2'100,Widget) :-
	functor(Term, Atom, Arity), !,
	write_atom(LexStyle, Atom, Ci, _,Widget),
	Depth1 is Depth+1,
	write_args(0, Arity, Term, noop, LexStyle, Depth1,Widget).
write_out({Term}, SynStyle, LexStyle, _, _, Depth, _, _, 2'100,Widget) :- !,
	write_tk('\\{',Widget),
	Depth1 is Depth+1,
	write_out(Term, SynStyle, LexStyle, 1200, 0, Depth1, '(', 2'100, _,Widget),
	write_tk('\\}',Widget).
write_out([Head|Tail], SynStyle, LexStyle, _, _, Depth, _, _, 2'100,Widget) :- !,
	write_tk('\\[',Widget),
%	put(0'[),
	Depth1 is Depth+1,
	write_out(Head, SynStyle, LexStyle, 999, 0, Depth1, '(', 2'100, _,Widget),
	write_tail(Tail, SynStyle, LexStyle, Depth1,Widget).
write_out((A,B), SynStyle, LexStyle, Prio, _, Depth, Lpar, Ci, Co,Widget) :- !,
	%  This clause stops writeq quoting commas.
	Depth1 is Depth+1,
	maybe_paren(1000, Prio, Lpar, Lpar1, Ci, C1,Widget),
	write_out(A, SynStyle, LexStyle, 999, 0, Depth1, Lpar1, C1, _,Widget),
	write_tk(',',Widget),
	write_out(B, SynStyle, LexStyle, 1000, 1000, Depth1, '(', 2'100, C2,Widget),
	maybe_paren(1000, Prio, C2, Co,Widget).
write_out(Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co,Widget) :-
	functor(Term, F, N),
	Depth1 is Depth+1,
	write_out(N, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth1, Lpar, Ci, Co,Widget).

write_out(1, F, Term, SynStyle, LexStyle, Prio, _, Depth, Lpar, Ci, Co,Widget) :-
	current_postfixop(F, P, O), !,
	(current_infixop(F, _, _, _) -> O1=1200; O1=O),
	maybe_paren(O1, Prio, Lpar, Lpar1, Ci, C1,Widget),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, 1200, Depth, Lpar1, C1, C2,Widget),
	write_atom(LexStyle, F, C2, C3,Widget),
	maybe_paren(O1, Prio, C3, Co,Widget).
write_out(1, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co,Widget) :-
	F \== -,
        current_prefixop(F, O, P), !,
	(PrePrio=1200 -> O1 is P+1; O1=O),	% for "fy X yf" etc. cases
	maybe_paren(O1, Prio, Lpar, _, Ci, C1,Widget),
	write_atom(LexStyle, F, C1, C2,Widget),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, P, Depth, ' (', C2, C3,Widget),
	maybe_paren(O1, Prio, C3, Co,Widget).
write_out(2, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co,Widget) :-
        current_infixop(F, P, O, Q), !,
	(PrePrio=1200 -> O1 is Q+1; O1=O),	% for "U xfy X yf" etc. cases
	maybe_paren(O1, Prio, Lpar, Lpar1, Ci, C1,Widget),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, 1200, Depth, Lpar1, C1, C2,Widget),
	write_atom(LexStyle, F, C2, C3,Widget),
	arg(2, Term, B),
	write_out(B, SynStyle, LexStyle, Q, Q, Depth, '(', C3, C4,Widget),
	maybe_paren(O1, Prio, C4, Co,Widget).
write_out(N, F, Term, SynStyle, LexStyle, _, _, Depth, _, Ci, 2'100,Widget) :-
	write_atom(LexStyle, F, Ci, _,Widget),
	write_args(0, N, Term, SynStyle, LexStyle, Depth,Widget).

write_VAR(N, SynStyle, _, _, Ci, 2'000,Widget) :-
	integer(N), N >= 0,
	SynStyle \== noop, !,
	maybe_space(Ci, 2'000,Widget),
	tk_var(N,Widget).

write_VAR('_', SynStyle, _, _, Ci, Co,Widget) :-
    % '_' was String
/*	nonvar(String),
	(   atom_codes(Atom, String) 
	;   Atom = String
	),
	atom(Atom),
*/
	SynStyle \== noop, !,
	write_atom(noquote,'_',Ci,Co,Widget).

write_VAR(X, SynStyle, LexStyle, Depth, Ci, 2'100,Widget) :-
	write_atom(LexStyle, '$VAR', Ci, _,Widget),
	write_args(0, 1, '$VAR'(X), SynStyle, LexStyle, Depth,Widget).

write_atom(noquote, Atom, Ci, Co,Widget) :-
%	prolog:'$atom_mode'(Atom, Co), % no longer exists...
        Co = 2'000,
	maybe_space(Ci, Co,Widget),
	write_tk_atom(Atom,Widget).
write_atom(quote, Atom, Ci, Co,Widget) :-
%	prolog:'$atom_mode'(Atom, Co),  % no longer exists...
        Co = 2'000,
	maybe_space(Ci, Co,Widget),
	write_tk_atom(Atom,Widget).

%   write_args(DoneSoFar, Arity, Term, SynStyle, LexStyle, Depth)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in SynStyle, LexStyle, given that DoneSoFar have already been written.

write_args(N, N, _, _, _, _,Widget) :- !,
	write_tk(')',Widget).
%	put(0')).
write_args(I, _, _, print(Limit), _, Depth,Widget) :-
	Depth >= Limit, !,
	write_args(I, Depth,Widget),
	tk_dots(Widget),
	write_tk(')',Widget).
write_args(I, N, Term, SynStyle, LexStyle, Depth,Widget) :-
	write_args(I, Depth,Widget),
	J is I+1,
	arg(J, Term, A),
	write_out(A, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _,Widget),
	write_args(J, N, Term, SynStyle, LexStyle, Depth,Widget).

write_args(0, _,Widget) :- !, write_tk('(',Widget).
write_args(_, 0,Widget) :- !, write_tk(', ',Widget).
write_args(_, _,Widget) :- write_tk(',',Widget).



%   write_tail(Tail, SynStyle, LexStyle, Depth)
%   writes the tail of a list of a given SynStyle, LexStyle, Depth.

write_tail(Var, _, _, _,Widget) :-			%  |var]
	var(Var), !,
	write_tk('|',Widget),
	%%hdrug_gui:tcl("~a insert end ~w",[Widget,Var]),
	tcl_insert(Widget,Var),
	write_tk('\\]',Widget).
write_tail([], _, _, _,Widget) :- !,			%  ]
	write_tk('\\]',Widget).
write_tail(_, print(Limit), _, Depth,Widget) :-
	Depth >= Limit, !,
	write_tk('|',Widget),
	tk_dots(Widget),
	write_tk('\\]',Widget).
write_tail([Head|Tail], SynStyle, LexStyle, Depth,Widget) :- !, %  ,Head tail
	write_tk(',',Widget),
	write_out(Head, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _,Widget),
	Depth1 is Depth+1,
	write_tail(Tail, SynStyle, LexStyle, Depth1,Widget).
write_tail(Other, SynStyle, LexStyle, Depth,Widget) :-	%  |junk]
	write_tk('|',Widget),
	write_out(Other, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _,Widget),
	write_tk('\\]',Widget).

portray_clause1(:-(Command), Co,Widget) :-
	functor(Command, Key, 1),
	current_op(_, fx, Key), !,
	arg(1, Command, Body),
	'list clauses'(Body, :-(Key), 8, Co,Widget).
portray_clause1((Pred:-true), Co,Widget) :- !,	
	 write_goal(Pred, 1199, 0, Co,Widget).
portray_clause1((Pred:-Body), Co,Widget) :- !,	
	write_goal(Pred, 1199, 1200, _,Widget),
	'list clauses'(Body, 0, 8, Co,Widget).
portray_clause1((Pred-->Body), Co,Widget) :- !,
	write_goal(Pred, 1199, 1200, _,Widget),
	'list clauses'(Body, 2, 8, Co,Widget).
portray_clause1(Pred, Co,Widget) :-
	write_goal(Pred, 1199, 0, Co,Widget).


write_goal(M:Goal, Prio, PrePrio, C,Widget) :- !,
	write_out(M:Goal, op, quote, Prio, PrePrio, -2, '(', 2'100, C,Widget).
write_goal(Goal, Prio, PrePrio, C,Widget) :-
	write_out(Goal, op, quote, Prio, PrePrio, -1, '(', 2'100, C,Widget).

write_fullstop(Ci,Widget) :-
	maybe_space(Ci, 2'010,Widget),
	write_tk('.',Widget),
	tk_nl(Widget).


'list clauses'((A,B), L, D, Co,Widget) :- !,
	'list clauses'(A, L, D, _,Widget),
	'list clauses'(B, 1, D, Co,Widget).
'list clauses'((A;B), L, D, 2'100,Widget) :- !,
	'list magic'(L, D,Widget),
	'list disj'(A, 3, D,Widget),
	'list disj'(B, D,Widget).
'list clauses'((A->B), L, D, 2'100,Widget) :- !,
	'list magic'(L, D,Widget),
	E is D+4,
	'list clauses'(A, 3, E, _,Widget),
	'list clauses'(B, 5, E, _,Widget),
	tk_nl(Widget), tk_tab(D,Widget),
	write_tk(')',Widget).
'list clauses'(!, 0, _, 2'100,Widget) :- !,
	write_tk(' :- !',Widget).
'list clauses'(!, 1, _, 2'100,Widget) :- !,
	write_tk(', !',Widget).
'list clauses'(!, 2, _, 2'100,Widget) :- !,
	write_tk(' --> !',Widget).
'list clauses'(Goal, L, D, Co,Widget) :- !,
	'list magic'(L, D,Widget),
	write_goal(Goal, 999, 0, Co,Widget).


'list magic'(0, D,Widget) :-
	tk_if(Widget),
	tk_nl(Widget), tk_tab(D,Widget).
'list magic'(1, D,Widget) :-
	write_tk(',',Widget),
	tk_nl(Widget), tk_tab(D,Widget).
'list magic'(2, D,Widget) :-
	write_tk(' -->',Widget),
	tk_nl(Widget), tk_tab(D,Widget).
'list magic'(3, _,Widget) :-
	write_tk('(   ',Widget).
'list magic'(4, _,Widget) :-
	write_tk('\\;   ',Widget).
'list magic'(5, D,Widget) :-
	write_tk(' ->',Widget),
	%% prolog:'$display'(' ->'),
	tk_nl(Widget), tk_tab(D,Widget).
'list magic'(:-(Key), D,Widget) :-
	tk_if(Widget),
	%% hdrug_gui:tcl("~a insert end ~w",[Widget,Key]),
	tcl_insert(Widget,Key),
	tk_nl(Widget), tk_tab(D,Widget).

'list disj'((A;B), D,Widget) :- !,
	'list disj'(A, 4, D,Widget),
	'list disj'(B, D,Widget).
'list disj'(Conj, D,Widget) :-
	'list disj'(Conj, 4, D,Widget),
	write_tk(')',Widget).

'list disj'((A->B), L, D,Widget) :- !,
	E is D+4,
	'list clauses'(A, L, E, _,Widget),
	'list clauses'(B, 5, E, _,Widget),
	tk_nl(Widget), tk_tab(D,Widget).
'list disj'(A, L, D,Widget) :-
	E is D+4,
	'list clauses'(A, L, E, _,Widget),
	tk_nl(Widget), tk_tab(D,Widget).

write_tk_atom(Atom0,Widget) :-
	hdrug_gui:tk_atom(Atom0,Atom),
	write_tk(Atom,Widget).

write_tk(Atom,Widget) :-
	%%hdrug_gui:tcl('~a insert end "~a"',[Widget,Atom]).
	tcl_insert(Widget,Atom).
	

tk_integer(Int,Widget) :-
	%%hdrug_gui:tcl('~a insert end "~d"',[Widget,Int]).
	tcl_insert(Widget,Int).

tk_nl(Widget) :-
	%% hdrug_gui:tcl('~a insert end "\n"',[Widget]).
	tcl_insert(Widget,'\\n').

special_font(Font,Integer,Widget) :-
	integer(Integer),
	gen_sym(TagName,tag),
	hdrug_gui:tcl('~a insert end \\~w {~a}',[Widget,Integer,TagName]),
%	hdrug_gui:tcl('~a tag add ~a insert "insert + 1 chars"',
%	    [Widget,TagName]),
	hdrug_gui:tcl('~a tag configure ~a -font ~a',[Widget,TagName,Font]).

%	hdrug_gui:tcl('~a tag add default insert end',[Widget,TagName]),
%	hdrug_gui:tcl('~a tag configure default -font fixed',[Widget]).


%%% these are all supposed to be in mathematical mode !
tk_tab(0,_Widget) :-
	!.
tk_tab(N0,Widget) :-
	write_tk(' ',Widget),
	N is N0-1,
	tk_tab(N,Widget).

tk_end_line(_).

tk_begin_line(Widget) :-
	tk_nl(Widget).

tk_variable(Term,Widget) :-
	%% hdrug_gui:tcl('~a insert end "~w"',[Widget,Term]).
	tcl_insert(Widget,Term).

tk_dots(Widget) :-
	write_tk('...',Widget).

tk_var(N,Widget) :-
	Letter is mod(N,26) + 0'A,
%	write('\mbox{'),put(Letter),write('}'),
	name(Atom,[Letter]),
	write_tk_atom(Atom,Widget),
	(   N>=26 ->
	    Rest is N//26, 
	    % write('_{'),write(Rest),write('}')
	    tk_integer(Rest,Widget)
	;   true
	).

tk_if(Widget) :-
	write_tk_atom(' :-',Widget).


tcl_insert(Widget,Text) :-
	hdrug_gui:tcl('~a index end',[Widget],I),
	hdrug_gui:tcl('~a insert end "~w"',[Widget,Text]),
	hdrug_gui:tcl('foreach tag [~a tag names] {~a tag remove $tag ~w end}',[Widget,Widget,I]).

:- initialize_flag(grr,0).

tk_tree(Name,FS,Frame):-
    hdrug_show:change_thing_tree_with_orig(FS,Name,Tree),
    tk_tree_nodes(Tree,Frame,Name,Frame),
    help_info(Name,Frame),
    hdrug_gui:tcl(update),
    tk_tree_lines_f(Frame).

tk_tree_nodes(Tree,W,Name,Top):-
    functor(Name,M,A),
    (	M/A == matrix/1
    ->  tk_tree_nodes_matrix(Tree,W,Name,Top)
    ;   M/A == user/1
    ->  tk_tree_nodes_user(Tree,W,Name,Top)
    ;   tk_tree_nodes_normal(Tree,W,Name,Top)
    ).

tk_tree_nodes_matrix(tree(Label,_,Ds),W,Name,Top):-
    hdrug_gui:tcl("\
       frame ~w
       pack ~w -side left -anchor n -expand 1 -fill x
       frame ~w.node
       ",[W,W,W],Wnode),
    hdrug_gui:tcl("pack ~w.node",[W]),
    pp_tk(Label,Wnode,''),
    hdrug_gui:tcl("bind ~w <Leave> {+ prolog {hdrug_tk:tk_tree_lines_f('~w')}}",[Wnode,Top]),
    tk_tree_nodes_list(Ds,W,Name,Top).

tk_tree_nodes_user(tree(Label,_,Ds),W,Name,Top):-
    hdrug_gui:tcl("\
       frame ~w
       pack ~w -side left -anchor n -expand 1 -fill x
       frame ~w.node
       ",[W,W,W],Wnode),
    hdrug_gui:tcl("pack ~w.node",[W]),
    hdrug:tk_tree_user_node(Label,Wnode,Name),
    tk_tree_nodes_list(Ds,W,Name,Top).

tk_tree_rest_node(Label/LabelOrig,W,Name):-
    hdrug_gui:tcl("\
       button ~w.node -text {~w} -command {prolog {hdrug_tk:internal_show_node(~w,~q)}}
       bind ~w.node <2> {prolog {hdrug_tk:internal_show_node2(~w,~q)}}
       bind ~w.node <3> {prolog {hdrug_tk:internal_show_node3(~w,~q)}}
       pack ~w.node
       ",[W,Label,Name,LabelOrig,W,Name,LabelOrig,W,Name,LabelOrig,W]).

tk_tree_nodes_normal(tree(Label/LabelOrig,_,Ds),W,Name,Top):-
    hdrug_gui:tcl("\
       frame ~w
       pack ~w -side left -anchor n -expand 1 -fill x
       button ~w.node -text {~w} -command {prolog {hdrug_tk:internal_show_node(~w,~q)}}
       bind ~w.node <2> {prolog {hdrug_tk:internal_show_node2(~w,~q)}}
       bind ~w.node <3> {prolog {hdrug_tk:internal_show_node3(~w,~q)}}
       pack ~w.node
       ",[W,W,W,Label,Name,LabelOrig,W,Name,LabelOrig,W,Name,LabelOrig,W]),
    tk_tree_nodes_list(Ds,W,Name,Top).

tk_tree_nodes_list([],_,_,_).
tk_tree_nodes_list([H|T],W,Name,Top) :-
    hdrug_gui:tcl("\
       canvas ~w.lines -height [option get . vert_dist Vert_dist] -width 0
       pack ~w.lines -fill x
       frame ~w.ds
       pack ~w.ds -expand 1 -fill x
       ",[W,W,W,W]),
    tk_tree_nodes_list0([H|T],W,1,Name,Top).

tk_tree_nodes_list0([],_,_,_,_).
tk_tree_nodes_list0([H|T],W,N0,Name,Top):-
    charsio:format_to_chars("~w.ds.f~w",[W,N0],Chars),
    name(Window,Chars),
    tk_tree_nodes(H,Window,Name,Top),
    N is N0+1,
    tk_tree_nodes_list0(T,W,N,Name,Top).
   
tk_tree_lines_f(Widget):-
    hdrug_gui:tcl('winfo exists ~w.lines',[Widget],Return),
    (	Return=='0'
    ->  true
    ;   hdrug_gui:tcl("~w.lines delete all",[Widget])
    ),
    tk_tree_lines_f(Widget,1).

tk_tree_lines_f(Widget,N) :-
    hdrug_gui:tcl('set w ~w.ds.f~w',[Widget,N],Next),
    hdrug_gui:tcl('winfo exists ~w.ds.f~w',[Widget,N],Return),
    (	Return=='0'
    ->  true
    ;   hdrug_gui:tcl("~w.lines create line [expr [winfo width ~w.lines]/2] 0 \
       [expr [winfo x ~w]+[winfo x ~w.node]+[winfo width ~w.node]/2] \
       [option get . vert_dist Vert_dist]",[Widget,Widget,Next,Next,Next]),
	N1 is N+1,
        tk_tree_lines_f(Next),
	tk_tree_lines_f(Widget,N1)
    ).

:- public internal_show_node/2, internal_show_node2/2, internal_show_node3/2.
internal_show_node(Name,Term) :-
    hdrug:show_node(Name,Term,tk),!.
internal_show_node(_Name,Term) :-
    write(Term),nl.

internal_show_node2(Name,Term) :-
    hdrug:show_node2(Name,Term,tk),!.
internal_show_node2(_Name,Term) :-
    write(Term),nl.

internal_show_node3(Name,Term) :-
    hdrug:show_node3(Name,Term,tk),!.
internal_show_node3(_Name,Term) :-
    write(Term),nl.

help_info(Name,Canvas) :-
    %% tell the user what the actions for pressing mouse buttons is
    (	hdrug:tk_tree_show_node_help(Name,Message)
    ->	hdrug_gui:tcl("help_line ~a {~a}",[Canvas,Message])
    ;	true
    ).

