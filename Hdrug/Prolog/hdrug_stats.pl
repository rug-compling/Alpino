%%%%%%%% THIS FILE IS A REAL MESS!

:- module(hdrug_stats,
	  [ print_table/0,
	    print_table_add/0,
	    add_info/1,
	    add_info_space/1,
	    print_table_total/0,
	    results_within_bound/0,
	    results_within_bound/1,
	    results_within_bound/2,
	    results_within_bound/3,
	    analyse_nr_readings/0,
	    analyse_nr_words/0,

	    go_and_table/0,
	    go_and_table/1,
	    go_and_table_amb/0,
	    go_and_table_amb/1,
	    p_tt/0,
	    p_tt/1,
	    p_tt_amb/0,
	    p_tt_amb/1,

	    go_and_table_tk/0,
	    go_and_table_tk_add/0,
	    sts/0, 
	    sts/1,
	    sts_add/0,
	    sts_add/1,
	    sts_space/0,
	    sts_space/1,
	    sts_space_add/0,
	    sts_space_add/1,
	    blt_within_bound/0
	  ]).


:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(lists)).
:- use_module(hdrug_util).
:- use_module(library(charsio)).
:- use_module(library(system)).

:- initialize_flag(start_results_within_bound, 100).
:- initialize_flag(  end_results_within_bound,5000).
:- initialize_flag( incr_results_within_bound, 100).

:- discontiguous
    results_within_bound/1,
    results_within_bound/2.

noblt_error :-
    raise_exception(hdrug_error('not supported (requires BLT library)~n',[])).

bound_list(Parser) :-
    hdrug_flag(start_results_within_bound,Start),
    hdrug_flag(end_results_within_bound,End),
    hdrug_flag(incr_results_within_bound,Incr),
    results_within_bound(Parser,Start,Incr,End).

results_within_bound(Parser,Start,Incr,Max) :-
    gen_incr_list(Incr,Start,Max,List),
    results_within_bound(List,Parser).

gen_incr_list(_Incr,V,Max,L) :-
    Max < V,
    !,
    L=[].
gen_incr_list(Incr,V,Max,[V|L]) :-
    V2 is V+Incr,
    gen_incr_list(Incr,V2,Max,L).

results_within_bound :-
    hdrug_flag(parser,Parser),
    bound_list(Parser).

results_within_bound(List) :-
    hdrug_flag(parser,Parser),
    results_within_bound(List,Parser).

results_within_bound(List,Parser) :-
    findall(T,hdrug:table_entry(_,_,_,T,Parser,_),Times0),
    length(Times0,Length),
    findall(T,good_table_entry(_,_,_,T,Parser,_), Times),
    partition(List,Times,TimesList),
    format("parser: ~w~n",[Parser]),
    print_results_within_bound(List,TimesList,0,Length).

partition([],Times,[Times]).   % remaining beyond last mentioned number
partition([Max|Rest],Times0,[MaxTimes|RestTimes]) :-
    partition0(Times0,Max,MaxTimes,Times1),
    partition(Rest,Times1,RestTimes).

partition0([],_,[],[]).
partition0([H0|T0],Max,MaxList,RestList) :-
    (	Max > H0
    ->	MaxList = [H0|MaxList2],
	RestList2 = RestList
    ;	MaxList2 = MaxList,
	RestList = [H0|RestList2]
    ),
    partition0(T0,Max,MaxList2,RestList2).

print_results_within_bound([],[El],_,Length) :-
    length(El,Length0),
    Perc is (Length0/Length)*100,
    format("~2f% (~w/~w) results take longer~n",[Perc,Length0,Length]).

print_results_within_bound([Bound|Bounds],[Set|Sets],Before,Length) :-
    length(Set,Length1),
    Length0 is Length1 + Before,
    Perc is (Length0/Length)*100,
    format("~2f% (~w/~w) results take less than ~w msec~n",
	   [Perc,Length0,Length,Bound]),
    print_results_within_bound(Bounds,Sets,Length0,Length).


print_table :-
    format("%% ~t~11+~t~w~t~10+~t~w~7+~t~w~7+~t~w~8+~t~w~t~15+~t~w~9+~n",
	   ['Name','Length','#Objs','Time','Parser','Space']),
    (	hdrug:table_entry(T1,T2,T3,T4,T5,T6),
	format("table_entry(",[]),
	format(
	 "~t~11+~t~w,~t~10+~t~w,~7+~t~w,~7+~t~w,~8+~t~w,~t~15+~t~w~8+  ).~n",
	 [T1,T2,T3,T4,T5,T6]),
	fail
    ;	true
    ),
    print_table_total.

print_table_add :-
    format("% t(Length,Msec/Input,Parser).~n",[]),
    (	add_info(El),
	format("~q.~n",[El]),
	fail
    ;	true
    ).

print_table_total :-
    hdrug_flag(hdrug_report_space,Val),
    (   Val == max
    ->  MaxSpace='MaxSpace'
    ;	MaxSpace='TotalSpace'
    ),
    format("%  ~t~w~16|~t~w~10+~t~w~t~16+~t~w~10+~t~w~10+~t~w~11+~t~w~10+~n",
	   ['Time','#Objs','Parser/Gen','Time/Input',
	    'Time/Unit',MaxSpace,'ProcessSize']),
    format("%  ~t~w~16|~t~w~10+~t~w~t~16+~t~w~10+~t~w~10+~t~w~11+~t~w~10+~n",
	   ['(msec)','','','(msec)','(msec)','','(megabytes)']),
    statistics(memory,[Process0|_]),
    Process is Process0//1000000,
    findall(T,total_info(T),Table0),
    sort(Table0,Table),
    (	member(total(F1,F2,F3,F4,F5,F6),Table),
	format(
	  "%XX~t~w~16|~t~w~10+~t~w~t~16+~t~w~10+~t~w~10+~t~w~11+~t~w~10+~n",
	  [F1,F2,F3,F4,F5,F6,Process]),
	fail
    ;	true
    ).


total_info(total(TotalTime,TotalObjs,Parser,AverageSent,AverageWord,
		 MaxSpace)) :-
    (	hdrug_flag(parser(Parser))	% only if info available at all:
    ;   hdrug_flag(generator(Parser))
    ),
    \+ \+ hdrug:table_entry(_,_,_,_,Parser,_),  
    findall(Time,hdrug:table_entry(_,_,_,Time,Parser,_),Times),
    sum_list(Times,TotalTime),
    findall(Obj,good_table_entry(_,_,Obj,_,Parser,_), Objs),
    sum_list(Objs,TotalObjs),
    length(Times,Len),
    AverageSent is TotalTime//Len,
    findall(Leng,hdrug:table_entry(_,Leng,_,_,Parser,_),Lengs),
    sum_list(Lengs,TotalWords),
    AverageWord is TotalTime//TotalWords,
    findall(Space,hdrug:table_entry(_,_,_,_,Parser,Space),Spaces),
    hdrug_flag(hdrug_report_space,Val),
    (   Val == max
    ->  max(Spaces,0,MaxSpace)
    ;	sum_list(Spaces,MaxSpace)
    ).

max([],L,L).
max([H|T],L0,L) :-
    L1 is max(H,L0),
    max(T,L1,L).

add_info(t(Length,Av,Parser)):-
    (hdrug_flag(parser(Parser));hdrug_flag(generator(Parser))),
    bagof(Time,Nm^Am^Ed^(hdrug:table_entry(Nm,Length,Am,Time,Parser,Ed),
			 \+Am=space_out),Times),
    sum_list(Times,Sum),
    length(Times,Len),
    Av is Sum/Len.

add_info_space(t(Length,Av,Parser)):-
    (hdrug_flag(parser(Parser));hdrug_flag(generator(Parser))),
    bagof(Space,Nm^Am^Time^(hdrug:table_entry(Nm,Length,Am,Time,Parser,Space),
			    \+Am=space_out),Spaces),
    sum_list(Spaces,Sum),
    length(Spaces,Len),
    Av is Sum/Len.

analyse_nr_readings :-
    hdrug_flag(parser,Parser),
    setof(Rea,hdrug:Space^Words^Id^Time^
              table_entry(Id,Words,Rea,Time,Parser,Space),
	  Reas),
    analyse_nr_readings(Reas,Parser).

analyse_nr_readings([],_).
analyse_nr_readings([H|T],P) :-
    count_edges(hdrug:table_entry(_,_,H,_,P,_),Count),
    format(user_error,"~w examples with ~w readings~n",[Count,H]),
    analyse_nr_readings(T,P).


analyse_nr_words :-
    findall(Sen,hdrug:a_sentence(_,_,Sen),Sents),
    length(Sents,Len),
    findall(_,(lists:member(S,Sents),lists:member(_,S)),Ws),
    length(Ws,WsLen),
    format(user_error,"~w sentences, ~w words~n",[Len,WsLen]),
    findall(L-_,(hdrug:a_sentence(_,_,S), length(S,L)),Vals0),
    keysort(Vals0,Vals1),
    count_nr_words(Vals1,Vals),
    report_nr_words(Vals).

count_nr_words([],[]).
count_nr_words([Key-_|Keys],[Key-Nr|Res]) :-
    count_nr_words(Keys,Key,1,Nr,Res).

count_nr_words([],_,Nr,Nr,[]).
count_nr_words([Key1-_|Keys],Key,Nr0,Nr,Res) :-
    (	Key1 =:= Key
    ->	Nr1 is Nr0+1,
	count_nr_words(Keys,Key,Nr1,Nr,Res)
    ;	Nr0=Nr,
	Res=[Key1-Nr1|Res1],
	count_nr_words(Keys,Key1,1,Nr1,Res1)
    ).

report_nr_words([]).
report_nr_words([L-V|T]) :-
    format("~w ~w~n",[L,V]),
    report_nr_words(T).

						 
    /*
    analyse_nr_words([0-0,1-1,2-2,3-3,4-4,5-5,6-6,7-7,8-8,9-9,10-10,
		      11-11,12-12,13-13,14-14,15-15,16-16,17-17,18-18,19-19,20-20,
		      21-25,26-30,31-40,41-50,51-75,76-100,101-10000]).

analyse_nr_words([]).
analyse_nr_words([H0-H1|T]) :-
    analyse_nr_words0(H0,H1),
    analyse_nr_words(T).

analyse_nr_words0(H,H) :-
    !,
    count_edges((hdrug:a_sentence(_,_,Sen),
		 length(Sen,H)
		),Count),
    format(user_error,"~w examples with ~w words~n",[Count,H]).
		       
analyse_nr_words0(H0,H1) :-
    count_edges((hdrug:a_sentence(_,_,Sen),
		 length(Sen,H),
		 H0 =< H,
		 H =< H1
		),Count),
    format(user_error,"~w examples with ~w words~n",[Count,H0-H1]).
*/


%  this produces TeX output on the basis of statistical parse information. 

%%% TIME_OUT values in table_entry makes this a real mess...

go_and_table :-
	go_and_table(_).

go_and_table(Parser) :-
	( findall(Nr,hdrug:a_sentence(Nr,_X,_Y),Nrs),
	  member(N,Nrs),
	  hdrug:parser_comparison(N),
	  p_tt(Parser),
	  fail
        ; true 
        ).

p_tt(Parser):-
	files(TexFile,DviFile),
	print_table_tex(TexFile,Parser),
	latex_and_xdvi(TexFile,DviFile).

p_tt :-
%	member_flag(parser_mode,P),
	hdrug_flag(parser(P),on),
	!,
	p_tt(P).

print_table_tex(File,Parser) :-
	telling(Old),
	tell(File),
	print_table_tex_core(length,Parser),!,
	told,
	tell(Old).

print_table_tex_core(LengthAmb,Parser) :-
	start_docu,
	setof(P,a_parser_mode(P),Parsers),
	setof(L,a_la(LengthAmb,L),Length),
	select(Parser,Parsers,Parsers2),
	begin_table([Parser|Parsers2]),
	do_table(Length,[Parser|Parsers2],LengthAmb),
	end_table,
	end_docu.

a_la(length,L):-
	hdrug:table_entry(_,L,_,_,_,_).

a_la(amb,L):-
	hdrug:table_entry(_,_,L,_,_,_),
	integer(L).

go_and_table_amb :-
	go_and_table_amb(_).

go_and_table_amb(Parser) :-
	( findall(Nr,hdrug:a_sentence(Nr,_X,_Y),Nrs),
	  member(N,Nrs),
	  hdrug:parser_comparison(N),
	  p_tt_amb(Parser),
	  fail
        ; true 
        ).

p_tt_amb(Parser):-
	files(TexFile,DviFile),
	print_table_tex_amb(TexFile,Parser),
	latex_and_xdvi(TexFile,DviFile).

p_tt_amb :-
%	member_flag(parser_mode,P),
	hdrug_flag(parser(P),on),
	!,
	p_tt_amb(P).

print_table_tex_amb(File,Parser) :-
	telling(Old),
	tell(File),
	print_table_tex_core(amb,Parser),!,
	told,
	tell(Old).

a_parser_mode(P):-
	hdrug:table_entry(_,_,_,_,P,_).

print_parsers(P):-
	p_par(P),
	write('\\\\ \\hline '),nl.

p_par([]).
p_par([H|T]):-
	escape_chars(H,H1),
	write(' & '),
	write(H1),
	p_par(T).

do_table([],_,_).
do_table([N1|T],Ps,LA):-
	write(N1),
	write(' & '),
	give_number_of_readings(LA,N1,Ps),
	write_e_first(Ps,N1,LA),
	write('\\\\'),nl,
	do_table(T,Ps,LA).


give_number_of_readings(length,Length,[P|_]):-
	compute_readings(Length,P,Total),
	write(Total).
give_number_of_readings(amb,Amb,[P|_]):-
	compute_lengths(Amb,P,Total),
	write(Total).

write_e_first([Ph|Pt],L,LA):-
	write_first(Ph,L,Factor,LA),
	write_e(Pt,L,Factor,LA,Ph).

write_e([],_,_,_,_).
write_e([H|T],N,F,LA,Ph):-
	write(' & '),
	write_e1(N,H,F,LA,Ph),
	write_e(T,N,F,LA,Ph).

write_first(Parser,Length,Factor,LA):-
	write(' & '),
	bagof(Time,one(LA,Length,Parser,Time),Times),!,
	write(100),
	length(Times,Number),
	sum_list(Times,AllTime),
	NewTime is AllTime/Number,
	Factor is 100/NewTime,
	Extra is NewTime//1,   %afronding
	write('='), write(Extra).
write_first(_,_,1,_).

write_e1(Key,Parser,Factor,ChooseKey,NormParser):-
	bagof(Time,one(ChooseKey,Key,Parser,Time),Times),
	(  one_time_out(ChooseKey,Key,Parser)
        -> recompute_factor(Key,Parser,Factor2,ChooseKey,NormParser)
        ;  Factor2 = Factor
        ),
	length(Times,Number),
	sum_list(Times,AllTime),
	NewTime is (Factor2*(AllTime/Number))//1,
	write(NewTime).
write_e1(_,_,_,_,_).

one_time_out(length,Length,Parser):-
    ( hdrug:table_entry(_,Length,time_out,_,Parser,_)
    ; hdrug:table_entry(_,Length,out_of_memory,_,Parser,_)
    ).

recompute_factor(Key,Parser,Factor,ChooseKey,NormParser):-
	% find those Times (Amb) such that they are not timeout for Parser
	setof(Time,rec_one(ChooseKey,Key,NormParser,Parser,Time),Times),!,
	length(Times,Number),
	sum_list(Times,AllTime),
	NewTime is AllTime/Number,
	Factor is 100/NewTime.

rec_one(length,Length,Parser,Other,Time):-
	good_table_entry(Name,Length,_,Time,Parser,_),
	good_table_entry(Name,Length,_,_,Other,_).

rec_one(amb,Length,Parser,Other,Time):-
	hdrug:table_entry(Name,_,Length,Time,Parser,_),
	good_table_entry(Name,_,_,_,Other,_).

one(length,Length,Parser,Time):-
	good_table_entry(_,Length,_,Time,Parser,_).

one(amb,Length,Parser,Time):-
	hdrug:table_entry(_,_,Length,Time,Parser,_).

end_table :-
	write('\\hline\\end{tabular}\\end{center}'),nl.

begin_table(Parsers) :-
	write('\\begin{center}'),nl,
	write('\\begin{tabular}'),
	write('{||r|r|'),
	once_r(Parsers),
	write('|}\\hline '),nl,
	write(' n & \\# '),
	print_parsers(Parsers).

escape_chars(A,A2):-
	term_atom(A,A1),
	name(A1,Chars),
	escape_l(Chars,Chars2),
	name(A2,Chars2).

escape_l([],[]).
escape_l([H|T],Out):-
	escape_c(H,Rest,Out),
	escape_l(T,Rest).

% 38: &
% 95: _
% 36: $

% 92: \
escape_c(36,R,[92,36|R]) :- !.
escape_c(38,R,[92,38|R]) :- !.
escape_c(95,R,[92,95|R]) :- !.

% catch all:
escape_c(C,R,[C|R]).



once_r([]).
once_r([_|T]):-
	write('r|'),
	once_r(T).

compute_readings(L,P,Total) :-
    bagof(Amb,T^Name^Edges^good_table_entry(Name,L,Amb,T,P,Edges),Ambs),!,
    count_edges(hdrug:table_entry(_,L,_,_,P,_),L2),
    sum_list(Ambs,Total1),
    Total is Total1//L2.

compute_readings(_,_,'').

compute_lengths(Amb,Parser,Total):-
	bagof(Length,Amb^Time^Name^Edges^(hdrug:table_entry(Name,Length,Amb,Time,Parser,Edges)),Ambs),!,
	count_edges(hdrug:table_entry(_,_,Amb,_,Parser,_),L2),
	sum_list(Ambs,Total1),
	Total is Total1//L2.

compute_lengths(_,_,'').



dir(TmpDir) :-
	environ('TMPDIR',TmpDir),!.
dir('/tmp').

files(Tex,Dvi) :-
	dir(Tmp),
	concat(Tmp,'/hdrugtableXXXXXX',Base0),
	mktemp(Base0,Base),
	concat(Base,'.tex',Tex),
	concat(Base,'.dvi',Dvi).

latex_and_xdvi(Tex,Dvi) :-
	dir(Dir),
	format_to_chars('xterm -iconic -e sh -c "(cd ~a ; latex ~a ; xdvi -geometry 800x480+100+800 -paper a1 ~a)" &',[Dir,Tex,Dvi],Chars),
	name(Cmd,Chars),
	shell(Cmd).

% the structures are embedded in a document, hence we set up the
% document:
% LaTeX2E
start_docu :-
	format("
\\documentclass{article}
\\usepackage{a4wide}
\\begin{document}
\\thispagestyle{empty}
\\small


",[]).

% and finish the document:
end_docu :-
        nl,write('\\end{document}'),nl.

go_and_table_tk :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_go_and_table_tk
    ;	noblt_error
    ).

go_and_table_tk_add :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_go_and_table_tk_add
    ;   noblt_error
    ).

sts  :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts 
    ;   noblt_error
    ).

sts(A) :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts(A)
    ;   noblt_error
    ).

sts_add :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts_add
    ;   noblt_error
    ).

sts_add(X) :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts_add(X)
    ;   noblt_error
    ).

sts_space :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts_space
    ;   noblt_error
    ).

sts_space(X) :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts_space(X)
    ;   noblt_error
    ).

sts_space_add :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts_space_add
    ;   noblt_error
    ).

sts_space_add(X) :-
    hdrug_flag(blt,BLT),
    (	BLT==blt
    ->  blt_sts_space_add(X)
    ;   noblt_error
    ).

blt_go_and_table_tk :-
	tk_graph_reset('.time'),
	tk_graph_reset('.space'),
	( findall(Nr,hdrug:a_sentence(Nr,_X,_Y),Nrs),
	  member(N,Nrs),
	  hdrug:parser_comparison(N),
	  findall(P,hdrug_flag(parser(P),on),Parsers),
	  blt_sts(Parsers,'.time'),
	  blt_sts_space(Parsers,'.space'),
	  hdrug_gui:tcl(update),
	  fail
        ; true 
        ).

blt_go_and_table_tk_add :-
	tk_graph_reset('.time'),
	tk_graph_reset('.space'),
	( findall(Nr,hdrug:a_sentence(Nr,_X,_Y),Nrs),
	  member(N,Nrs),
	  hdrug:parser_comparison(N),
	  findall(P,hdrug_flag(parser(P),on),Parsers),
	  blt_sts_add(Parsers,'.time'),
	  blt_sts_space_add(Parsers,'.space'),
	  hdrug_gui:tcl(update),
	  fail
        ; true 
        ).


tk_it_continue(List,Options,Widget) :-
	tk_graph_continue(List,
	          ['configure -title Results',
		   'xaxis configure -title "Input Length"',
		   'yaxis configure -title "Time in Millisecs."' |
		  Options
		  ],Widget).

blt_sts :-
	findall(P,hdrug_flag(parser(P),on),Parsers),
	blt_sts(Parsers).

blt_sts(Parsers):-
	findall(t(P,L,time),(member(P,Parsers),
                        get_table(P,L)),List),
	tk_it_continue(List,['yaxis configure -title "Time."'],'.time').

blt_sts(Parsers,Widget):-
	findall(t(P,L,time),(member(P,Parsers),
                        get_table(P,L)),List),
	tk_it_continue(List,['yaxis configure -title "Time."'],Widget).

blt_sts_add :-
	findall(P,hdrug_flag(parser(P),on),Parsers),
	blt_sts_add(Parsers).

blt_sts_add(Parsers):-
	findall(t(P,L,avtime),(member(P,Parsers),
                        get_table_add(P,L)),List),
	tk_it_continue(List,
		       ['yaxis configure -title "Average Time."'],'.time').

blt_sts_add(Parsers,W):-
	findall(t(P,L,avtime),(member(P,Parsers),
                        get_table_add(P,L)),List),
	tk_it_continue(List,
		       ['yaxis configure -title "Average Time."'],W).

blt_sts_space :-
	findall(P,hdrug_flag(parser(P),on),Parsers),
	blt_sts_space(Parsers).

blt_sts_space(Parsers):-
	findall(t(P,L,space),(member(P,Parsers),
                        get_table_space(P,L)),List),
	tk_it_continue(List,['yaxis configure -title "Space."'],'.space').

blt_sts_space(Parsers,W):-
	findall(t(P,L,space),(member(P,Parsers),
                        get_table_space(P,L)),List),
	tk_it_continue(List,['yaxis configure -title "Space."'],W).

blt_sts_space_add :-
	findall(P,hdrug_flag(parser(P),on),Parsers),
	blt_sts_space_add(Parsers).

blt_sts_space_add(Parsers):-
	findall(t(P,L,avspace),(member(P,Parsers),
                        get_table_space_add(P,L)),List),
	tk_it_continue(List,
		       ['yaxis configure -title "Average Space."'],'.space').

blt_sts_space_add(Parsers,W):-
	findall(t(P,L,avspace),(member(P,Parsers),
                        get_table_space_add(P,L)),List),
	tk_it_continue(List,
		       ['yaxis configure -title "Average Space."'],W).

get_table(Parser,List):-
	setof(Len/Time,
              Name^Amb^Edges^(hdrug:table_entry(Name,Len,Amb,Time,Parser,Edges),
                              number(Len), 
			      number(Time)),
              List).

get_table_space(Parser,List):-
	setof(Len/Edges,
              Name^Amb^Time^(hdrug:table_entry(Name,Len,Amb,Time,Parser,Edges),
                              number(Len), 
			      number(Edges)),
              List).

get_table_add(Parser,List):-
	setof(X/Y,table_element_add(Parser,X/Y),List).

table_element_add(Parser,X/Y):-
	setof(T,add_info(T),Table),
	member(t(X,Y,Parser),Table).

get_table_space_add(Parser,List):-
	setof(X/Y,table_element_space_add(Parser,X/Y),List).

table_element_space_add(Parser,X/Y):-
	setof(T,add_info_space(T),Table),
	member(t(X,Y,Parser),Table).

blt_within_bound :-
    hdrug_flag(blt,BLT),
    (	BLT == blt
    ->	hdrug_flag(blt_graph_lines,Old,on),
	findall(t(P,L,gb),( hdrug_flag(parser(P),on),
			    bound_list(P,L)
			  ), List),
	tk_graph(List,[
		       'configure -title "Percentage of inputs treated within cputime"',
		       'xaxis configure -title "Cputime (msec)"',
		       'yaxis configure -title "Number of inputs (%)"'
		       ],'.gb'),
	hdrug_flag(blt_graph_lines,_,Old)
    ;	noblt_error
    ).

bound_list(Parser,List) :-
	hdrug_flag(start_results_within_bound,Start),
	hdrug_flag(end_results_within_bound,End),
	hdrug_flag(incr_results_within_bound,Incr),
	results_within_bound(Parser,Start,Incr,End,List).

results_within_bound(Parser,Start,Incr,Max,List) :-
	gen_incr_list(Incr,Start,Max,IList),
	results_within_bound(IList,Parser,List).

results_within_bound(List) :-
	hdrug_flag(parser,Parser),
	bound_list(Parser,List).
%%	results_within_bound(Parser,List).
%%	results_within_bound([100,250,500,750,1000,1500,2000,5000], Parser, List).

results_within_bound(List,Out) :-
	hdrug_flag(parser,Parser),
	results_within_bound(List,Parser,Out).

results_within_bound(List,Parser,Out) :-
	findall(T,hdrug:table_entry(_,_,_,T,Parser,_),Times0),
	length(Times0,Length),
	findall(T,good_table_entry(_,_,_,T,Parser,_), Times),
	partition(List,Times,TimesList),
	get_results_within_bound(List,TimesList,0,Length,Out).

get_results_within_bound([],[_El],_,_Length,[]).

get_results_within_bound([Bound|Bounds],[Set|Sets],Before,Length,[Bound/Perc|Tail]) :-
	Length =\= 0,
	length(Set,Length1),
	Length0 is Length1 + Before,
	Perc is (Length0/Length)*100,
	get_results_within_bound(Bounds,Sets,Length0,Length,Tail).





%% :- initialize_flag(graph_widget,'.graph').
:- initialize_flag(blt_graph_lines,off).
:- initialize_flag(hdrug_report_space,max).

tk_graph_reset(W):-
	hdrug_gui:tcl('if [winfo exists ~w] "destroy ~w"',[W,W]).


tk_graph(List,Options,W) :-
	hdrug_gui:tcl('
    set w ~w
    set geom =600x450
    catch {set geom [winfo geometry $w]}
    catch {destroy $w}
    toplevel $w -class Dialog
    wm minsize $w 0 0
    wm title $w Graph
    wm iconname $w Graph
    wm iconbitmap $w @$hdrug_library/bitmaps/hdrug.xbm
    wm geometry $w $geom
    frame $w.b
    pack $w.b -side bottom -fill x
    button $w.b.ok -text OK -command "destroy $w ; set graph_started($w) 0"
    pack $w.b.ok -side right
    button $w.b.logon -text "Logscale On" -command \
	"$w.g yaxis configure -logscale 1"
    button $w.b.logoff -text "Logscale Off" -command \
	"$w.g yaxis configure -logscale 0"
#    button $w.b.ps -text Postscript -command "$w.g postscript graph.ps"
#    pack $w.b.logon $w.b.logoff $w.b.ps -side left
	   pack $w.b.logon $w.b.logoff  -side left
    blt::graph $w.g 
    pack $w.g',[W]),
    concat(W,'.g',SubWidget),
	hdrug_gui:tcl("~w configure -width 1200 -height 900",[SubWidget]),
	tk_graph_options(Options,SubWidget),
	tk_graph0(List,SubWidget,1).

%tk_graph_continue(List) :-
%	tk_graph_continue(List,[]).
%
%tk_graph_continue(List,Options) :-
%	hdrug_flag(graph_widget,W),
%	tk_graph_continue(List,Options,W).

tk_graph_continue(List,Options,W) :-
    hdrug_gui:tcl("winfo exists ~w",[W],Exists),
    (	Exists == '1'
    ->	hdrug_gui:tcl('\
	   if [winfo exists ~w.g] {
	      foreach name [~w.g element names] {
		 ~w.g element delete $name
	      }
           } else { error "Cannot continue graph: ~w.g does not exist"}
            ',[W,W,W,W]),
        concat(W,'.g',SubWidget),
	tk_graph0(List,SubWidget,1)
    ;	tk_graph(List,Options,W)
    ).


tk_graph_options([],_).
tk_graph_options([H|T],W) :-
	hdrug_gui:tcl("~a ~w",[W,H]),
	tk_graph_options(T,W).

tk_graph0([],_,_).
tk_graph0([H|T],W,I) :-
	tk_graph1(H,W,I),
	I2 is I + 1,
	tk_graph0(T,W,I2).

%tk_graph00([],_).
%tk_graph00([H|T],W) :-
%	tk_graph01(H,W),
%	tk_graph00(T,W).

tk_graph1(t(Name,List,Extra),W,I) :-
	show_ith(I,Affix0,Affix1),
	hdrug_gui:tcl("catch {blt::vector destroy vecx~w~w}",[Extra,Name]),
	hdrug_gui:tcl("catch {blt::vector destroy vecy~w~w}",[Extra,Name]),
	hdrug_gui:tcl("blt::vector create vecx~w~w",[Extra,Name]),
	hdrug_gui:tcl("blt::vector create vecy~w~w",[Extra,Name]),
	hdrug_gui:tcl("~a element create ~w ~w ~w -xdata vecx~w~w -ydata vecy~w~w",
	    [W,Name,Affix0,Affix1,Extra,Name,Extra,Name]),
	add_pairs(List,W,Name,Extra).

%tk_graph01(t(Name,List,Extra),W) :-
%	add_pairs(List,W,Name,Extra).

add_pairs([],_,_,_).
add_pairs([X/Y|Tail],W,Name,Extra) :-
    hdrug_gui:tcl("set vecx~w~w(++end) ~w",[Extra,Name,X]),
    hdrug_gui:tcl("set vecy~w~w(++end) ~w",[Extra,Name,Y]),
%%	hdrug_gui:tcl("~a element append ~w { ~w ~w }",[W,Name,X,Y]),
	add_pairs(Tail,W,Name,Extra).

%concat_pairs([],A,A).
%concat_pairs([X0/Y0|T],A0,A) :-
%	term_atom(X0,X),
%	term_atom(Y0,Y),
%	concat_all([A0,X,Y],A1,' '),
%	concat_pairs(T,A1,A).

show_ith(N,Atom0,Atom1) :-
	show_ith(N,Atom0),
	hdrug_flag(blt_graph_lines,OnOff),
	show_line(OnOff,Atom1).

show_line(on, '-linewidth 1').  % -activelinewidth 1').
show_line(off,'-linewidth 0').  % -activelinewidth 0').


show_ith(1,' -symbol square -color blue -outline blue -pixels 4').
show_ith(2,' -symbol circle -color red -outline red -pixels 4').
show_ith(3,' -symbol scross -color green4 -outline green4 -pixels 4').
show_ith(4,' -symbol diamond -color black -outline black  -pixels 4').
show_ith(5,' -symbol plus -color brown -outline brown  -pixels 4').
show_ith(6,' -symbol cross -color blue -outline blue -pixels 4').
show_ith(7,' -symbol splus -color purple -outline purple  -pixels 4').
show_ith(8,' -symbol circle -color orange -outline orange -pixels 4').
show_ith(9,' -symbol square -color grey -outline grey  -pixels 4').
show_ith(10,' -symbol circle -color bisque3 -outline bisque3  -pixels 4').
show_ith(11,' -symbol diamond -color PeachPuff4 -outline PeachPuff4  -pixels 4').
show_ith(12,' -symbol plus -color RoyalBlue4 -outline RoyalBlue4  -pixels 4').
show_ith(13,' -symbol cross -color SlateBlue1 -outline SlateBlue1  -pixels 4').
show_ith(14,' -symbol splus -color PaleTurquoise3 -outline PaleTurquoise3  -pixels 4').
show_ith(15,' -symbol scross -color SpringGreen4 -outline SpringGreen4  -pixels 4').
show_ith(16,' -symbol square -color khaki4 -outline khaki4  -pixels 4').
show_ith(17,' -symbol circle -color gold3 -outline gold3  -pixels 4').
show_ith(18,' -symbol diamond -color IndianRed4 -outline IndianRed4  -pixels 4').
show_ith(19,' -symbol plus -color salmon2 -outline salmon2  -pixels 4').
show_ith(20,' -symbol cross -color DeepPink1 -outline DeepPink1  -pixels 4').
show_ith(21,' -symbol splus -color thistle4 -outline thistle4  -pixels 4').

show_ith(N,' -symbol scross ') :-
	N > 21.

%% a table entry which was not timed out or out of memory
good_table_entry(A,B,C,D,E,F) :-
    hdrug:table_entry(A,B,C,D,E,F),
    integer(C).
