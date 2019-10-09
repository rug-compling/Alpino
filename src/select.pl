:- module(alpino_select, [ select_parse/0 ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(charsio)).
:- use_module(library(ordsets)).
:- use_module(library(lists)).
:- use_module(hdrug(hdrug_util)).
:- use_module(hdrug(hdrug_gui)).

:- dynamic
    select_state/3,
    weights/1,
    parses/1.

:- thread_local
    select_state/3,
    weights/1,
    parses/1.

:- initialize_flag(keep_decided,off).
:- multifile user:help_flag/2.

user:help_flag(keep_decided,
"cf. select.pl. Boolean flag which determines whether discriminators are still displayed after a choice has been made.").

%% Get parse results as dependency trees

get_tree(parse(Id,Tree),String) :-
    hdrug:object(Id,o(Cat,String,_)),
    alpino_dt:result_to_dt(Cat,Tree).

parse_ids([],[]).
parse_ids([parse(Id,_)|Ps],[Id|Qs]) :-
    parse_ids(Ps,Qs).

parse_sets([],[]).
parse_sets([parse(_,Q)|Ps],[Q|Qs]) :-
    parse_sets(Ps,Qs).

%% Convert a list of trees to a list of dependency sets

trees_to_sets([],[],_).
trees_to_sets([parse(Id,T)|Ts],[parse(Id,S)|Ss],String) :-
    tree_to_set(T,S,String),
    trees_to_sets(Ts,Ss,String).

tree_to_set(Tree,Set,String) :-
    tree_to_set(Tree,List,[],_,[],_,String),
    merge_yields(List, Merged),
    list_to_ord_set(Merged, Set).

tree_to_set(tree(r(Rel,p(Cat)),_,Dtrs),[dep(NewPath,Cat,Yield),brak(Yield)|Ds],
	    Path,Yield,InInds,OutInds,String) :-
    append(Path,[Rel],NewPath),
    do_daughters(NewPath,Dtrs,[],Yield,Ds,InInds,OutInds,String).

tree_to_set(tree(r(Rel,i(N,Dtr)),_,Dtrs),Result,Path,Yield,
	    InInds,[i(N,ref(NewPath,Yield))|OutInds],String) :-
    append(Path,[Rel],NewPath),
    tree_to_set(tree(r(Rel,Dtr),_,Dtrs),Result,Path,Yield,
		InInds,OutInds,String).

tree_to_set(tree(r(Rel,i(N)),_,[]),[dep(NewPath,ref(Cat),Yield)],Path,
	    Yield,Inds,Inds,_) :-
    member(i(N,ref(Cat,Yield)),Inds),
    append(Path,[Rel],NewPath).

tree_to_set(tree(r(Rel,l(Cat,_,Word/[A,_])),_,_),[dep(NewPath,Cat,[A-[Word]]),
						morph(Word,Morph,A)],
	    Path,[A-[Word]],Inds,Inds,String) :-
    append(Path,[Rel], NewPath),
    nth0(A,String,Morph).

do_daughters(_,[],InYield,OutYield,[],Index,Index,_) :-
    keysort(InYield,OutYield).

do_daughters(Path,[D1|D1s],InYield,OutYield,Result,InInds,OutInds,String) :-
    tree_to_set(D1,D2,Path,NewYield,InInds,Index1,String),
    append(NewYield, InYield, Z),
    do_daughters(Path,D1s,Z,OutYield,D2s,Index1,OutInds,String),
    append(D2, D2s, Result).

merge_yields([],[]).

merge_yields([dep(Path,Cat,[Start-Z|Yield])|Xs],
	     [dep(Path,NewCat,yield(Merged,Start))|Ys]):-
%    merge_cat_yield(Cat,NewCat),
    Cat = NewCat,
    merge_yield([Start-Z|Yield], Merged),
    merge_yields(Xs, Ys).

merge_yields([morph(Word,Morph,Start)|Xs],[morph(Word,Morph,Start)|Ys]) :-
    merge_yields(Xs, Ys).

merge_yields([dep(X,Y,[])|Xs],[dep(X,Y,yield([],0))|Ys]) :-
    merge_yields(Xs, Ys).

merge_yields([brak([Start-Z|Yield])|Xs],
	     [brak(Merged,Start)|Ys]):-
    merge_yield([Start-Z|Yield], Merged),
    merge_yields(Xs, Ys).

merge_yield(Y0,Y) :-
    sort(Y0,Y1),
    merge_yield_(Y1,Y).

merge_yield_([],[]).
merge_yield_([_-[Word]|Xs],[Word|Ys]) :-
    merge_yield_(Xs, Ys).

%% Given a list of parses, find a set of maximal discriminators

make_discriminators(Parses,Set,Weights) :-
    parse_sets(Parses,Sets),
    ord_intersection(Sets,Common),
    ord_union(Sets,All),
    ord_subtract(All,Common,Diff),
    annotate_diffs(Parses,Diff,X),
    minimize_diffs(X,Z),
    dediff(Z,W,Weights),
    list_to_ord_set(W,Set).


dediff([],[],[]).
dediff([diff(_,W,X)|Xs],[X|Ys],[diff(W,X)|Zs]) :-
    dediff(Xs,Ys,Zs).

%% Annotate properties with set of parses that have that property

annotate_diffs(_,[],[]).
annotate_diffs(Sets,[X|Xs],[diff(Z,W,X)|Ys]) :-
    select_sets(X,Sets,[],Yes,[],No),
    min_list(Yes,MinYes),
    min_list(No,MinNo),
%    write(X),nl,
    functor(X,Type,_),
    (   Type == morph
    ->
	%% Always put lexical discriminators first
	W is 0
    ;
	%% Rank other discriminators by their power
	W is MinYes + MinNo
    ),
    list_to_ord_set(Yes,Z),
    annotate_diffs(Sets,Xs,Ys).

select_sets(_,[],Yes,Yes,No,No).
select_sets(Diff,[parse(Id,P)|Ps],InYes,OutYes,InNo,OutNo) :-
    (   my_ord_member(Diff,P)
    ->	NewYes=[Id|InYes],
	NewNo=InNo
    ;
	NewYes=InYes,
	NewNo=[Id|InNo]
    ),
    select_sets(Diff,Ps,NewYes,OutYes,NewNo,OutNo).

minimize_diffs(Diffs,Mindiffs) :-
    minimize_diffs(Diffs,Diffs,[],X),
    list_to_ord_set(X,Mindiffs).

minimize_diffs([],_,X,X).
minimize_diffs([X|Xs],Diffs,In,Out) :-
    (   superior_diff(X,Diffs)
    ->  T=In
    ;   T=[X|In]
    ),
    minimize_diffs(Xs,Diffs,T,Out).

superior_diff(_,[]) :-
    fail.
superior_diff(diff(S,_,dep(Path1,_,_)),[diff(T,_,dep(Path2,_,_))|_]) :-
    ord_seteq(S,T),
    length(Path1,Len1),
    length(Path2,Len2),
    compare(>,Len1,Len2),
    !.
superior_diff(diff(S,_,brak(Yield1,_)),[diff(T,_,brak(Yield2,_))|_]) :-
    ord_seteq(S,T),
    length(Yield1,Len1),
    length(Yield2,Len2),
    compare(>,Len1,Len2),
    !.
superior_diff(diff(S,_,S1),[diff(_,_,_)|Ts]) :-
    superior_diff(diff(S,_,S1),Ts).

%% Print sets of dependencies to the console

% debugging only
:- public print_results/0, print_diff/0.

print_results :-
    bagof(Tree, get_tree(Tree,String), Trees),
    trees_to_sets(Trees,Parses,String),
    print_results(Parses).

print_results([]).
print_results([parse(Id,S)|Ss]) :-
    write(Id), nl,
    print_result(S),
    nl,
    print_results(Ss).

print_result([]).
print_result([X|Xs]) :-
    write(X), nl, print_result(Xs).

print_diff :-
    bagof(Tree, get_tree(Tree,String), Trees),
    trees_to_sets(Trees,Parses,String),
    parse_sets(Parses,Sets),
    ord_intersection(Sets,Common),
    ord_union(Sets,All),
    ord_subtract(All,Common,Diff),
    annotate_diffs(Parses,Diff,X),
    minimize_diffs(X,Y),
    print_result(Y).

%% User interface stuff

% main entry point.
:- public select_parse/0.  %tcltk

select_parse :-
    init_select(C),
    display_diffs(C).

init_select(C) :-
    bagof(Tree, get_tree(Tree,String), Trees),
    trees_to_sets(Trees,Parses,String),
    make_discriminators(Parses,Disc,Weights),
    retractall(select_state(_,_,_)),
    retractall(weights(_)),
    retractall(parses(_)),
    assertz(select_state(Disc,[],[])),
    assertz(weights(Weights)),
    assertz(parses(Parses)),
    select_widget(fs,C),
    tcl('canvas ~a ; pack ~a',[C,C]),
    tcl('help_line ~a {<1> Menu; <2> Yes; <3> No}',[C]),
    hdrug_flag(clig_fontsize,Font),
    tcl('set clig_globals(fontsize) ~d',[Font]),
    reposition_widget(fs).

update_state(Unk,Yes,No) :-
    adjust_discriminants(Unk,Yes,No,OutUnk,OutYes,OutNo),
    asserta(select_state(OutUnk,OutYes,OutNo)).

display_diffs(C) :-
    select_state(Unk,Yes,No),
    parses_to_clig(Yes,No,FParses),
    diffs_to_clig(Unk,Yes,No,FDiffs,C),
    %% format_to_chars('{cornerbox {seq ~s} {leftStack ~s}}',
    format_to_chars('{leftstack {seq ~s} {vspace 6} {leftStack ~s}}',
		    [FParses,FDiffs],Obj),
    tcl('~a delete all',[C]),
    tcl('set size [execobj ~s 10 10 ~a {"cmg"}]',[Obj,C]),
    tcl('~a configure -width [expr [lindex $size 0]+10]',[C]),
    tcl('~a configure -height [expr [lindex $size 1]+10]',[C]).

%% Output a list of still possible parses

parses_to_clig(Yes,No,Out) :-
    get_still_valid(Yes,No,Valid),
    parse_ids(Valid,Ids),
    format_to_chars('{plain-text ~w}',[Ids],Out).

%% Output a set of discriminants

diffs_to_clig(Unk,Yes,No,Text,Cv) :-
    mark_status(Unk,?,A),
    mark_status(Yes,+,B),
    mark_status(No,-,C),
    append(A,B,T),
    append(T,C,All),
    sort_diffs(All,Diffs),
    diffs_to_clig(Diffs,Text,[],Cv).
	
mark_status([],_,[]).
mark_status([X|Xs],Status,[status(X,Status)|Ys]) :-
    mark_status(Xs,Status,Ys).

sort_diffs(In,Out) :-
    weights(Weights),
    add_weights(In,Weights,T0),
    keysort(T0,T1),
    remove_weights(T1,Out).

add_weights([],_,[]).
add_weights([X|Xs],Weights,[W-X|Ys]) :-
    X=status(Diff,_),
    member(diff(W,Diff),Weights),
    add_weights(Xs,Weights,Ys).

remove_weights([],[]).
remove_weights([_-X|Xs],[X|Ys]) :-
    remove_weights(Xs,Ys).

diffs_to_clig([],S,S,_C).
diffs_to_clig([D|Ds],S0,S,C) :-
    (   format_diff(D,Text)
    ->	D=status(Diff,_),
	format_to_chars( 
			 '{active ~s {popup <1> {{command -label "Yes" -command {prolog {alpino_select:click(yes,~k,~k)}}} {command -label "No" -command {prolog {alpino_select:click(no,~k,~k)}}}}} {<2> {prolog {alpino_select:click(yes,~k,~k)}}} {<3> {prolog {alpino_select:click(no,~k,~k)}}}} ',
			 [Text,Diff,C,Diff,C,Diff,C,Diff,C],S0,S1),
	diffs_to_clig(Ds,S1,S,C)
    ;	diffs_to_clig(Ds,S0,S,C)
    ).
	
format_diff(status(dep(Path,Cat,yield(Yield,Pos)),Status),Text) :-
    diff_color(Status, Color1, Color2),
    format_path(Path,FPath),
    (   % Cat = ref([dep(NPath,_,_)|_])
	Cat = ref(NPath)
    ->  format_path(NPath,FNPath),
	format_to_chars('{seq {color-area ~a {color ~a {plain-text { ~a }}}} {plain-text { ~s  =  ~s}}}',
			[Color1,Color2,Status,FPath,FNPath],Text)
    ;   format_to_chars('{seq {color-area ~a {color ~a {plain-text { ~a }}}} {plain-text { ~s  =  ~p: ~p / ~d}}}',
			[Color1,Color2,Status,FPath,Cat,Yield,Pos],Text)
    ).

format_diff(status(brak(Yield,Start),Status),Text) :-
    diff_color(Status, Color1, Color2),
    format_to_chars('{seq {color-area ~a {color ~a {plain-text { ~a }}}} {plain-text { ~p / ~d}}}',
    [Color1,Color2,Status,Yield,Start],Text).

format_diff(status(morph(Word,Morph,Start),Status),Text) :-
    diff_color(Status, Color1, Color2),
    format_to_chars('{seq {color-area ~a {color ~a {plain-text { ~a }}}} {plain-text { "~p" = ~p / ~d}}}',
    [Color1,Color2,Status,Morph,Word,Start],Text).

diff_color(?, red, white) :- !.
diff_color(_, green, black) :-
    hdrug_flag(keep_decided,on).

format_path([P|Ps],S0) :-
    format_to_chars("~a",[P],S0,S),
    format_path(Ps,S,[]).

format_path([],S,S).
format_path([P|Ps],S0,S) :-
    format_to_chars(".~a",[P],S0,S1),
    format_path(Ps,S1,S).

%% Update info based on user's input

:- public click/3.  % tcltk

click(yes,Diff,C) :-
    select_state(Unk,Yes,No),
    my_ord_member(Diff,Unk),
    ord_del_element(Unk,Diff,NewUnk),
    ord_add_element(Yes,Diff,NewYes),
    update_state(NewUnk,NewYes,No),
    display_diffs(C).

click(no,Diff,C) :-
    select_state(Unk,Yes,No),
    my_ord_member(Diff,Unk),
    ord_del_element(Unk,Diff,NewUnk),
    ord_add_element(No,Diff,NewNo),
    update_state(NewUnk,Yes,NewNo),
    display_diffs(C).

%% Collect parses which could still be valid

get_still_valid(Yes,No,Out) :-
    parses(Parses),
    get_still_valid(Parses,Yes,No,Out).

get_still_valid([],_,_,[]).
get_still_valid([parse(Id,Set)|Ps],Yes,No,Out) :-
    (   still_valid(Set,Yes,No)
    ->  Out=[parse(Id,Set)|Outs]
    ;   Out=Outs
    ),
    get_still_valid(Ps,Yes,No,Outs).

still_valid(Parses,Yes,No) :-
    still_valid_yes(Parses,Yes),
    still_valid_no(Parses,No).

%% R1: bad discriminant -> bad parse

still_valid_no([],_).
still_valid_no([S|Ss],Diffs) :-
    \+my_ord_member(S,Diffs),
    still_valid_no(Ss,Diffs).

%% R2: missing good discriminant -> bad parse

still_valid_yes(_,[]).
still_valid_yes(Set,[D|Ds]) :-
    my_ord_member(D,Set), 
    still_valid_yes(Set,Ds).

%% Adjust discriminants to reflect valid parses

adjust_discriminants(InUnk,InYes,InNo,ResUnk,ResYes,ResNo) :-
    get_still_valid(InYes,InNo,Valid),
    parse_sets(Valid,ValidSets),
    adjust_no(ValidSets,InUnk,InNo,TempUnk,OutNo),
    adjust_yes(ValidSets,TempUnk,InYes,OutUnk,OutYes),
    (   ord_seteq(InUnk,OutUnk)
    ->	ResUnk=OutUnk, ResYes=OutYes, ResNo=OutNo 
    ;	adjust_discriminants(OutUnk,OutYes,OutNo,ResUnk,ResYes,ResNo)
    ).

%% R3: only bad parses -> bad discriminant

adjust_no(Sets,InUnk,InNo,OutUnk,OutNo) :-
    ord_union(Sets,All),
    ord_subtract(InUnk,All,MustBeBad),
    ord_subtract(InUnk,MustBeBad,OutUnk),
    ord_union(InNo,MustBeBad,OutNo).

%% R4: all good parses -> good discriminant

adjust_yes(Sets,InUnk,InYes,OutUnk,OutYes) :-
    ord_intersection(Sets,All),
    ord_intersection(InUnk,All,MustBeGood),
    ord_subtract(InUnk,MustBeGood,OutUnk),
    ord_union(InYes,MustBeGood,OutYes).

%% equivalent to ord_member in the Sicstus 3.12.11 library ordsets.pl
%% but SWI does not want to use this:
%%
%% %       Some Prolog implementations also provide  ord_member/2, with the
%% %       same semantics as ord_memberchk/2.  We   believe  that  having a
%% %       semidet ord_member/2 is unacceptably inconsistent with the *_chk
%% %       convention.  Portable  code  should    use   ord_memberchk/2  or
%% %       member/2.
%% %
%% %       @author Richard O'Keefe

my_ord_member(X, [H|T]) :-
    compare(C, X, H),
    my_ord_member(C, X, T).

my_ord_member(=, _X, _T).
my_ord_member(>, X, [H|T]) :-
    compare(C, X, H),
    my_ord_member(C, X, T).
