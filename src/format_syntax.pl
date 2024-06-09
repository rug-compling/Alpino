:- module(alpino_format_syntax, [ format_syntax_of_obj/1,
				  format_syntax_of_result/2,
				  format_some_syntax_of_result/3,
				  format_deriv_of_obj/1,
				  format_deriv_of_result/2,
				  format_nderiv_of_obj/1,
				  format_nderiv_of_result/2,
				  format_nderiv_of_result_unknowns/2,
				  format_palm_of_obj/1,
				  format_palm_of_result/2,
				  format_palm_score_of_obj/1,
				  format_palm_score_of_result/2,
				  format_left_corners_of_obj/1,
				  format_new_left_corners_of_obj/1,
				  format_left_corners_of_result/2,
				  format_new_left_corners_of_result/2,
				  format_frames_of_obj/1,
				  result_to_frames/3,
				  format_frames_of_result/1,
				  format_frames_of_result/2,
				  format_postags_of_result/3,
				  format_pts_of_result/3,
				  format_postags_of_obj/1,
				  collect_postags_of_obj/3
				]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(utils).
:- use_module(hdrug(hdrug_util)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% BRACKETED STRING %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public format_syntax_of_obj/1.

format_syntax_of_result(Result,Key) :-
    result_to_bracketed_string(Result,String,[]),
    format("~w|~s~n",[Key,String]).

format_syntax_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,_,_)),
    format_syntax_of_result(Cat,Key).

result_to_bracketed_string(Result,S0,S):-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    tree_to_bracketed_string(Tree,S0,S).

tree_to_bracketed_string(tree(Node,_,Ds,_),S0,S) :-
    Node == robust,
    !,
    tree_to_bracketed_string_ds(Ds,S0,S).

tree_to_bracketed_string(tree(Node,_,Ds,_),S0,S) :-
    functor(Node,Cat,_),
    format_to_chars(" [ @~w",[Cat],S0,S1),
    tree_to_bracketed_string_ds(Ds,S1,S2),
    format_to_chars(" ]",[],S2,S).

tree_to_bracketed_string_ds(lex(ref(_,_,_,Surf,_,_,_,_,_,_,_)),S0,S) :-
    format_to_chars(" ~w",[Surf],S0,S).
tree_to_bracketed_string_ds([],S,S).
tree_to_bracketed_string_ds([H|T],S0,S) :-
    tree_to_bracketed_string(H,S0,S1),
    tree_to_bracketed_string_ds(T,S1,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% some syntax %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% only format *certain* brackets;
%% also allows to show non-matching brackets. Therefore, closing brackets
%% are named too.

show_bracket(open,np).
show_bracket(close,np).

:- public format_some_syntax_of_obj/1.

format_some_syntax_of_result(Result,Key,Sentence) :-
    result_to_some_bracketed_string(Result,String,Sentence),
    format("sentence(~q,~q).~n",[Key,String]).

format_some_syntax_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,Sentence,_)),
    format_some_syntax_of_result(Cat,Key,Sentence).

result_to_some_bracketed_string(Result,S,Sent):-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    tree_to_some_bracketed_string(Tree,S,[],Sent).

tree_to_some_bracketed_string(tree(Node,_,Ds,_),S0,S,Sent) :-
    Node == robust,
    !,
    tree_to_some_bracketed_string_ds(Ds,S0,S,Sent).

tree_to_some_bracketed_string(tree(Node,_,Ds,_),S0,S,Sent) :-
    functor(Node,Cat,_),
    (   show_bracket(open,Cat)
    ->  atom_concat('@',Cat,Label),
	S0=['[',Label|S1]
    ;   S0=S1
    ),
    tree_to_some_bracketed_string_ds(Ds,S1,S2,Sent),
    (   show_bracket(close,Cat)
    ->  atom_concat('@',Cat,Label),
	S2=[']',Label|S]
    ;   S2=S
    ).

get_sub(P0,P,Sent,S0,S) :-
    length(Prefix,P0),
    ItLength is P-P0,
    length(It,ItLength),
    append(Prefix,Rest,Sent),
    append(It,_,Rest),
    append(It,S,S0).

tree_to_some_bracketed_string_ds(lex(ref(_,_,_,_,P0,P,_,_,_,_,_)),S0,S,Sent):-
    get_sub(P0,P,Sent,S0,S).
tree_to_some_bracketed_string_ds([],S,S,_).
tree_to_some_bracketed_string_ds([H|T],S0,S,Sent) :-
    tree_to_some_bracketed_string(H,S0,S1,Sent),
    tree_to_some_bracketed_string_ds(T,S1,S,Sent).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% with deriv %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public format_nderiv_of_obj/1, format_deriv_of_obj/1, format_palm_of_obj/1, format_palm_score_of_obj/1, format_nderiv_of_obj_unknowns/1.

format_deriv_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,_,_)),
    format_deriv_of_result(Cat,Key).

format_deriv_of_result(Result,Key) :-
    result_to_bracketed_string_deriv(Result,String,[]),
    format("~w|~s~n",[Key,String]).

format_nderiv_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,_,_)),
    format_nderiv_of_result(Cat,Key).

format_nderiv_of_obj_unknowns(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,_,_)),
    format_nderiv_of_result_unknowns(Cat,Key).

format_nderiv_of_result(Result,Key) :-
    result_to_deriv_tree(Result,Tree),
    format("~q.~n",[deriv(Key,Tree)]),
    format_local_trees(Tree,Key).

format_nderiv_of_result_unknowns(Result,Key) :-
    result_to_deriv_tree(Result,Tree),
    format_local_trees_unknowns(Tree,Key).

format_palm_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,_,_)),
    format_palm_of_result(Cat,Key).

format_palm_of_result(Result,Key) :-
    result_to_bracketed_string_palm(Result,String,[]),
    format("~w|~s~n",[Key,String]).

format_palm_score_of_obj(N) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(N,o(Cat,_,_)),
    format_palm_score_of_result(Cat,Key).

format_palm_score_of_result(Result,Key) :-
    result_to_bracketed_string_palm(Result,String,[]),
    alpino_treebank:score_string_of_result(Result,ScoreString),
    format("~w|~s|~s~n",[Key,ScoreString,String]).

result_to_bracketed_string_deriv(Result,S0,S):-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    tree_to_bracketed_string_deriv(Tree,S0,S).

tree_to_bracketed_string_deriv(tree(Node,_,Ds,_),S0,S) :-
    Node == robust,
    !,
    tree_to_bracketed_string_ds_deriv(Ds,S0,S).

tree_to_bracketed_string_deriv(
          tree(Node,_,lex(ref(_,Tag,_,Surf,_,_,_,_,_,_,_)),_),S0,S) :-
    !,
    functor(Node,Cat,_),
    display_quoted(Tag,QTag),
    display_quoted(Cat,QCat),
    display_quoted(Surf,QSurf),
    format_to_chars("[('~s','~s'),'~s']",[QTag,QCat,QSurf],S0,S).

tree_to_bracketed_string_deriv(tree(Node,Id,Ds,_),S0,S) :-
    functor(Node,Cat,_),
    display_quoted(Id,QId),
    display_quoted(Cat,QCat),
    format_to_chars("[('~s','~s'),[",[QId,QCat],S0,S1),
    tree_to_bracketed_string_ds_deriv(Ds,S1,S2),
    format_to_chars("]]",[],S2,S).

tree_to_bracketed_string_ds_deriv([],S,S).
tree_to_bracketed_string_ds_deriv([H|T],S0,S) :-
    tree_to_bracketed_string_deriv(H,S0,S1),
    tree_to_bracketed_string_ds_deriv2(T,S1,S).

tree_to_bracketed_string_ds_deriv2([],S,S).
tree_to_bracketed_string_ds_deriv2([H|T],S0,S) :-
    format_to_chars(",",[],S0,S1),
    tree_to_bracketed_string_deriv(H,S1,S2),
    tree_to_bracketed_string_ds_deriv2(T,S2,S).



result_to_bracketed_string_palm(Result,S0,S):-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    tree_to_bracketed_string_palm(Tree,S0,S),
    !.
result_to_bracketed_string_palm(_,_,_) :-
    raise_exception(hdrug_error("result_to_bracketed_string_palm failed~n",[])).

tree_to_bracketed_string_palm(
          tree(_,_,lex(ref(_,_,_,Surf,_,_,_,_,_,_,_)),_),S0,S) :-
    !,
    format_to_chars("~w",[Surf],S0,S).
tree_to_bracketed_string_palm(
	  tree(skip,_,lex(W),_), S0, S) :-
    !,
    format_to_chars("[skip ~w]",[W],S0,S).
tree_to_bracketed_string_palm(tree(_,Id,Ds,_),S0,S) :-
    format_to_chars("[~w ",[Id],S0,S1),
    tree_to_bracketed_string_ds_palm(Ds,S1,S2),
    format_to_chars(" ~w]",[Id],S2,S).

tree_to_bracketed_string_ds_palm([],S,S).
tree_to_bracketed_string_ds_palm([H|T],S0,S) :-
    tree_to_bracketed_string_palm(H,S0,S1),
    tree_to_bracketed_string_ds_palm2(T,S1,S).

tree_to_bracketed_string_ds_palm2([],S,S).
tree_to_bracketed_string_ds_palm2([H|T],S0,S) :-
    format_to_chars(" ",[],S0,S1),
    tree_to_bracketed_string_palm(H,S1,S2),
    tree_to_bracketed_string_ds_palm2(T,S2,S).



display_quoted(Term,String) :-
    format_to_chars("~q",[Term],String0),
    escape_q(String0,String).

escape_q([],[]).
escape_q([H|T],String) :-
    (	H == 39
    ->	String = [92,39|StringT]
    ;	H == 92
    ->	String = [92,92|StringT]
    ;   String = [H|StringT]
    ),
    escape_q(T,StringT).

escape_b(Term,String) :-
    format_to_chars("~w",[Term],String0),
    escape_bar(String0,String).

escape_bar([],[]).
escape_bar([H|T],String) :-
    (   H == 124
    ->  String = [95,95|String1]
    ;   String = [H|String1]
    ),
    escape_bar(T,String1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% left corners %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public
    format_left_corners_of_obj/1,
    format_new_left_corners_of_obj/1.

format_left_corners_of_obj(Obj) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(Obj,o(Cat,_,_)),
    format_left_corners_of_result(Cat,Key).

format_left_corners_of_result(Result,Key) :-
    result_to_deriv_tree(Result,Tree),
    findall(LC,tree_to_left_corners(Tree,LC),LCS),
    format_left_corners(LCS,Key).

format_new_left_corners_of_obj(Obj) :-
    hdrug_flag(current_ref,Key),
    hdrug:object(Obj,o(Cat,_,_)),
    format_new_left_corners_of_result(Cat,Key).

format_new_left_corners_of_result(Result,Key) :-
    result_to_deriv_tree(Result,Tree),
    tree_to_new_left_corners(Tree,LCS),
    format_left_corners(LCS,Key).

format_left_corners([],_).
format_left_corners([H|T],Key) :-
    format("LEFTCORNER#",[]),
    format_left_corner(H,Key),
    format_left_corners(T,Key).

format_left_corner(lc(Goal,LC,His),Key) :-
    format("~w|~q",[Key,Goal]),
    format_left_corner_his([LC|His]).

format_left_corner_his([]) :-
    format("~n",[]).
format_left_corner_his([H|T]) :-
    format("|~q",[H]),
    format_left_corner_his(T).

tree_to_new_left_corners(Tree,LCS) :-
    findall(LC,tree_to_left_corners(Tree,LC),LCS0),
    filter_new_left_corners(LCS0,LCS).

tree_to_left_corners(tree(robust,_,DS),LC) :-
    !,
    member(Tree,DS),
    tree_to_left_corners(Tree,LC).

tree_to_left_corners(tree(Rule,TopCat,Sub),lc(GoalCat,Start,Path)) :-
    find_goal_or_self(tree(Rule,TopCat,Sub),tree(Goal,GoalCat,GoalDs)),
    find_left_corner([tree(Goal,GoalCat,GoalDs)],Path0),
    reverse(Path0,[Start|Path]).

find_goal_or_self(Tree,Tree).
find_goal_or_self(Tree0,Tree) :-
    subtree(Tree0,tree(_,_,[_|Ds])),
    member(Tree,Ds).

subtree(ST,ST).
subtree(tree(_,_,List),Tree) :-
    member(Tree0,List),
    subtree(Tree0,Tree).

find_left_corner([],[]).
find_left_corner([tree(Name,_,Ds)|_],[Name|Rules]) :-
    find_left_corner(Ds,Rules).

result_to_deriv_tree(Result,DerivTree) :-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    nonvar(Tree),  % if timed out, sometimes no tree available
    tree_to_deriv_tree(Tree,DerivTree).

tree_to_deriv_tree(tree(Term,R,Ds0,_),tree(Name,Fun,Ds)) :-
    functor(Term,Fun,_Ar),
    (   Ds0 = lex(Ref)
    ->  lex_rule_name(Term,Ref,Name),
	Ds=[]
    ;   Ds0 = [],
        R = call(R1)
    ->  Name = R1,
        Ds=[]
    ;   Ds0 = []
    ->  Name=gap(R),
	Ds=[]
    ;   Name=R,
	tree_to_deriv_tree_ds(Ds0,Ds)
    ).

tree_to_deriv_tree_ds([],[]).
tree_to_deriv_tree_ds([H0|T0],[H|T]) :-
    tree_to_deriv_tree(H0,H),
    tree_to_deriv_tree_ds(T0,T).

lex_rule_name(_,ref(_ClassTag,Tag,_,_,_,_,_,_,_,_,_),lex(Tag)).
lex_rule_name(skip,_,skip).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% RESULT to POSTAGS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% cgn/dcoi/lassy postags, not Alpino postags (these are called
%% frames here)

:- public format_postags_of_obj/1.

collect_postags_of_obj(N,PosTags,Words) :-
    hdrug:object(N,o(Result,Words0,_)),
    result_to_frames(Result,_,_,Frames),
    frames_to_postags(Frames,Result,PosTags),
    alpino_lexical_analysis:remove_brackets(Words0,Words1),
    alpino_treebank:remove_phantoms(Words1,Words).

format_postags_of_obj(N) :-
    collect_postags_of_obj(N,PosTags,Words),
    format_cgn_postags(PosTags,Words).

%Y    ignore_phantoms_postags(PosTags0,PosTags),
frames_to_postags(Frames,Result,PosTags) :-
    frames_to_postags_(Frames,Result,PosTags0,[]),
    sort(PosTags0,PosTags1),
    add_missing_postags(PosTags1,PosTags2,Result),
    alpino_dt:get_phantoms(PosList),
    ignore_phantoms_postags(PosList,PosTags2,PosTags).

% frames_to_postags(Frames,Result,Words,PosTags) :-
%     frames_to_postags_(Frames,Result,Words,PosTags0,[]),
%     sort(PosTags0,PosTags1),
%     add_missing_postags(PosTags1,PosTags,Result).

add_missing_postags(PosTags0,PosTags,Result) :-
    alpino_data:result_term(_,Words0,_,_,_,Result),
    alpino_lexical_analysis:replace_alt(Words0,0,Words),
    add_missing_postags_(PosTags0,Words,0,PosTags).

add_missing_postags_([],Str,N,PosTags) :-
    add_final_postags(Str,N,PosTags).
%add_missing_postags_(Postags,Str,N0,Result) :-
%    (   alpino_lexical_analysis:open_bracket(N0,N,_)
%    ;   alpino_lexical_analysis:close_bracket(N0,N,_)
%    ), !,
%    add_missing_postags_(Postags,Str,N,Result).

add_missing_postags_([cgn_postag(P0,P1,Lemma,Tag)|Tags0],[W|Words],N0,PosTags) :-
    N1 is N0 + 1,
    (   P0 = N0
    ->  PosTags=[cgn_postag(P0,P1,Lemma,Tag)|PosTags1],
	add_missing_postags_(Tags0,Words,N1,PosTags1)
    ;   P0 > N0,
	add_postag(W,TagN,LemmaN),
	PosTags=[cgn_postag(N0,N1,LemmaN,TagN)|PosTags1],
	add_missing_postags_([cgn_postag(P0,P1,Lemma,Tag)|Tags0],Words,N1,PosTags1)
    ).

add_final_postags([],_,[]).
add_final_postags([H|T],N,[cgn_postag(N,N1,Lemma,Tag)|Tags]) :-
    add_postag(H,Tag,Lemma),
    N1 is N + 1,
    add_final_postags(T,N1,Tags).

%% in case of timeout, this really should depend on the best frame sequence
%% this is quick hack
add_postag(H,Tag,Lemma) :-
    (   alpino_cgn_postags:lassy(H,Lemma,Tag)
    ->  true
    ;   %% e.g. in case of timeout
	alpino_lexical_analysis:tag(_,_,Q0,Q,Stem,H,His,Frame),
	alpino_cgn_postags:cgn_postag(Frame,Stem,H,Q0,Q,n,His,[cgn_postag(_,_,Lemma,Tag)],[])
    ->  true	
    ;   format(user_error,"warning: no default postag for ~w~n",[H]),
	Tag = 'NA()',
	Lemma = H
    ).

% frames_to_postags_([],_,[]) --> [].
% frames_to_postags_([Frame|Frames],Result,[Word|Words]) -->
%     frame_to_postag(Frame,Result,Word),
%     frames_to_postags_(Frames,Result,Words).

frames_to_postags_([],_) --> [].
frames_to_postags_([Frame|Frames],Result) -->
    frame_to_postag(Frame,Result,_),
    frames_to_postags_(Frames,Result).

frame_to_postag(_Context-frame(_P0,_P,Q0,Q,Stem,Frame,Surf,His),Result,Surf ) -->
    alpino_cgn_postags:cgn_postag(Frame,Stem,Surf,Q0,Q,Result,His).

format_postags_of_result(Result,Words0,Key) :-
    result_to_frames(Result,_,_,Frames),
    frames_to_postags(Frames,Result,PosTags),
    alpino_lexical_analysis:remove_brackets(Words0,Words1),
    alpino_treebank:remove_phantoms(Words1,Words),
    format_cgn_postags(PosTags,Words,Key).

format_pts_of_result(Result,Words0,Key) :-
    result_to_frames(Result,_,_,Frames),
    frames_to_postags(Frames,Result,PosTags),
    postags_to_pts(PosTags,Pts),
    alpino_lexical_analysis:remove_brackets(Words0,Words1),
    alpino_treebank:remove_phantoms(Words1,Words),
    format_cgn_postags(Pts,Words,Key).

postags_to_pts([],[]).
postags_to_pts([cgn_postag(P0,P,Lemma,Tag0)|T0],[cgn_postag(P0,P,Lemma,Tag)|T]) :-
    alpino_postags:lassy_postag_atts(Tag0,List,[]),
    lists:member(pt=Tag,List),
    postags_to_pts(T0,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% RESULT to FRAMES %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public format_frames_of_obj/1.

format_frames_of_obj(N) :-
    hdrug:object(N,o(Result,_,_)),
    format_frames_of_result(Result).

format_frames_of_result(Result) :-
    result_to_frames(Result,Frames,_),
    result_to_skips_and_cats(Result,Skips,Cats),
    format_frames(Frames,none,Skips,Cats).

format_frames_of_result(Result,Key) :-
    result_to_frames(Result,Frames,_),
    result_to_skips_and_cats(Result,Skips,Cats),
    format_frames(Frames,Key,Skips,Cats).

result_to_frames(Result,Fs,Fs1):-
    result_to_frames(Result,Fs,Fs1,_).

result_to_frames(Result,Fs,Fs1,Fs2) :-
    alpino_data:result_term(_,_,_,_,Fs0,Result),
    expand_skips_frames(Fs0,Fs1),
    add_context_frames(Fs1,Fs2,Result),
    alpino_dt:get_phantoms(PosList),
    ignore_phantoms(PosList,Fs2,Fs).

%% complication: renumber in case of phantoms

ignore_phantoms_postags([],F,F).
ignore_phantoms_postags([H|T],F0,F):-
    ignore_phantoms_postags(F0,F,[H|T],0).

ignore_phantoms_postags([],[],[],_).
ignore_phantoms_postags([cgn_postag(P0,_,_,_)|Frames0],Frames,PosList,C0):-
    lists:select(P0,PosList,PosList1),
    !,
    ignore_phantoms_postags(Frames0,Frames,PosList1,C0 + 1).
ignore_phantoms_postags([cgn_postag(P0,P,A,B)|Frames0],[cgn_postag(Q0,Q,A,B)|Frames],PosList,C):-
    Q0 is P0 - C,
    Q is P - C,
    ignore_phantoms_postags(Frames0,Frames,PosList,C).

ignore_phantoms([],P,P).
ignore_phantoms([P0|Ps0],F0,F) :-
    ignore_phantoms_(F0,F1,P0),
    add_one(Ps0,Ps),
    ignore_phantoms(Ps,F1,F).

add_one([],[]).
add_one([P0|P0s],[P|Ps]) :-
    P is P0 - 1,
    add_one(P0s,Ps).

ignore_phantoms_([],[],_).
ignore_phantoms_([FH0|FT0],Frames,Pos) :-
    ignore_phantom(FH0,FT0,Frames,Pos).

ignore_phantom(_-frame(_,_,Q0,_,_,_,_,_),FT,Frames,Pos) :-
    Pos =:= Q0, !,
    renumber_frames(FT,Frames).
ignore_phantom(A0-frame(P0,P,Q0,Q,A3,A4,A5,A6),FT,[Frame|Frames],Pos) :-
    Pos > Q0,
    Pos < Q, !,
    Q1 is Q -1,
    P1 is P -1,
    Frame = A0-frame(P0,P1,Q0,Q1,A3,A4,A5,A6),
    renumber_frames(FT,Frames).
ignore_phantom(Frame,FT,[Frame|Frames],Pos) :-
    ignore_phantoms_(FT,Frames,Pos).

renumber_frames([],[]).
renumber_frames([Context-frame(P0,P,Q0,Q,Stem,Frame,Surf,His)|FT0],
		[Context-frame(R0,R,S0,S,Stem,Frame,Surf,His)|FT]) :-
    R0 is P0-1, R is P-1, S0 is Q0-1, S is Q-1,
    renumber_frames(FT0,FT).

format_frames([],_,_,_).
format_frames([FRAME|Fs],Key,Skips,Cats) :-
    format_frame(FRAME,Key,Skips,Cats),
    format_frames(Fs,Key,Skips,Cats).

format_frame(Context-frame(P0,P,_,_,Stem0,Frame,Surf0,His),Key,Skips,Cats) :-
    get_stem(Stem0,Stem1),
    escape_b(Surf0,Surf),
    escape_b(Stem1,Stem),
    prettyvars(Frame),
    format(user_error,
           "FRAME#~s|~q|~w|~w|~w|~w|~w|~s|~w|~w~n",
	   [Surf,Frame,Key,P0,P,His,Context,Stem,Skips,Cats]).

expand_skips_frames([],[]).
expand_skips_frames([H|T],Fs) :-
    expand_skips_frame(H,Fs,Fs0),
    expand_skips_frames(T,Fs0).

expand_skips_frame(frame(P0,P,Q0,Q,Stem,Frame,Surf,skip(_,L,R,His)),Fs0,Fs) :-
    !,
    expand_skips_(L,P0,P1,Q0,Q1,Fs0,Fs1),
    Tag = frame(P1,P2,Q1,Q2,Stem,Frame,Surf,His),
    Fs1=[Tag|Fs2],
    length(R,Rlen),P2 is P-Rlen, Q2 is Q-Rlen,
    expand_skips_(R,P2,P,Q2,Q,Fs2,Fs).

expand_skips_frame(F,[F|Fs],Fs).

expand_skips_([],P,P,Q,Q,Fs,Fs).
expand_skips_([Token|Skips],P0,P,Q0,Q,
		  [frame(P0,P1,Q0,Q1,Token,skip,Token,skip)|Fs0],Fs):-
    P1 is P0+1,
    Q1 is Q0+1,
    expand_skips_(Skips,P1,P,Q1,Q,Fs0,Fs).

result_to_skips_and_cats(Result,Skips,Cats) :-
    alpino_data:result_term(_,_,_,tree(robust,robust,Ds,_),_,Result),
    result_to_skips_and_cats(Ds,0,Skips,0,Cats),
    !.
result_to_skips_and_cats(_,0,0).

result_to_skips_and_cats([],S,S,C,C).
result_to_skips_and_cats([tree(_,R,_,_)|T],S0,S,C0,C) :-
    (   nonvar(R),
	R = robust_skips(_)
    ->  S1 is S0+1,
        C1 is C0
    ;   S1 is S0,
	C1 is C0+1
    ),
    result_to_skips_and_cats(T,S1,S,C1,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% ADD CONTEXT FRAMES %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_context_frames(Frs0,Frs,Result) :-
    alpino_data:result_term(_,_,_,Tree,_,Result),
    find_context_frame_tree(Tree,pre,_,CFrames,[]),
    add_context_frames_(Frs0,Frs,CFrames).

add_context_frames_([],[],_).
add_context_frames_([  frame(P0,P,Q0,Q,X,Frame,Surf,His)|T],
		   [C-frame(P0,P,Q0,Q,X,Frame,Surf,His)|T2],Cs) :-
    (   member(f(P0,P,C),Cs)
    ->  true
    ;   member(f(Z0,Z,C),Cs),
	Z0 =< P0,
	 P =< Z
    ->  true
    ;   C=pre
    ),
    add_context_frames_(T,T2,Cs).

find_context_frame_tree_each(lex(_),_,Cs,Cs).
find_context_frame_tree_each([],_,Cs,Cs).
find_context_frame_tree_each([H|T],C0,Cs0,Cs) :-
    find_context_frame_tree(H,C0,_,Cs0,Cs1),
    find_context_frame_tree_each(T,C0,Cs1,Cs).

find_context_frame_tree(tree(Node,_,Ds,_),C0,_C,Cs0,Cs) :-
    Node == robust,
    !,
    find_context_frame_tree_each(Ds,C0,Cs0,Cs).
find_context_frame_tree(tree(Node,_,_,_),_,_,Cs,Cs) :-
    Node == skip,
    !.

find_context_frame_tree(tree(Node,_,Ds,_),C0,C,Cs0,Cs) :-
    context_type_of_node(Node,Cx),
    find_context_frame_tree(Cx,Ds,C0,C,Cs0,Cs).

find_context_frame_tree(embed,Ds,C,C,Cs0,Cs) :-
    find_context_frame_tree_ds(Ds,pre,_,Cs0,Cs).
find_context_frame_tree(swap,Ds,_,post,Cs0,Cs):-
    find_context_frame_tree_ds(Ds,post,_C,Cs0,Cs).
find_context_frame_tree(cont,Ds,C0,C,Cs0,Cs) :-
    find_context_frame_tree_ds(Ds,C0,C,Cs0,Cs).

find_context_frame_tree_ds(lex(ref(_,_,_,_,P0,P,_,_,_,_,_)),
			   C,C,[f(P0,P,C)|Cs],Cs).
find_context_frame_tree_ds([],C,C,Cs,Cs).
find_context_frame_tree_ds([H|T],C0,C,Cs0,Cs) :-
    find_context_frame_tree(H,C0,C1,Cs0,Cs1),
    find_context_frame_tree_ds(T,C1,C,Cs1,Cs).

context_type_of_node(Node,Type) :-
    (   alpino_data:context_embed_node(Node)
    ->  Type = embed
    ;   alpino_data:context_swap_node(Node)
    ->  Type = swap
    ;   Type = cont
    ).

filter_new_left_corners([],[]).
filter_new_left_corners([H|T],L) :-
    filter_new_left_corner(H,L,L1),
    filter_new_left_corners(T,L1).

filter_new_left_corner(H,L,L1):-
    (   not_new_left_corner(H)
    ->  L=L1
    ;   L = [H|L1]
    ).

not_new_left_corner(lc(skip,skip,[])).
not_new_left_corner(lc(Goal,First,Rest)) :-
    rewrite_first(First,First2),
    lists:append(Rest,[finish],Rest2),
    not_new_left_corner(Rest2,Goal,First2).

rewrite_first(put_val,put_val).
rewrite_first(get_val,get_val).
rewrite_first(gap(X),gap(X)).
rewrite_first(lex(Cat),lex(Cat2)) :-
    alpino_guides:tr_tag(Cat,Cat2).

not_new_left_corner([H|T],Goal,First) :-
    lists:reverse([First,H|T],List),
    alpino_guides:check_connect(Goal,List).

format_cgn_postags([],[]).
format_cgn_postags([cgn_postag(P0,P,Lemma0,Tag)|T],[W|Words]) :-
    alpino_treebank:get_lemma_or_word(Lemma0,Lemma,W),
    format("~w~t~25+~w~t~5+~w~t~5+~w~t~50+~w\n",[W,P0,P,Tag,Lemma]),
    format_cgn_postags(T,Words).

format_cgn_postags([],[],_).
format_cgn_postags([cgn_postag(P0,P,Lemma0,Tag)|T],[W|Words],Key) :-
    alpino_treebank:get_lemma_or_word(Lemma0,Lemma,W),
    format("~w|~w|~w|~w|~w|~w\n",[Key,W,P0,P,Tag,Lemma]),
    format_cgn_postags(T,Words,Key).

get_stem(v_root(_A,B),C) :-
    !,
    B = C.
get_stem(A,A).

format_local_trees(Result,Key):-
    local_trees(Result,Trees),
    format_local_list(Trees,Key).

format_local_trees_unknowns(Result,Key):-
    local_trees(Result,Trees0),
    ignore_existing(Trees0,Trees),
    format_local_list(Trees,Key),
    (   print_unknown_bigrams(Result),
        fail
    ;   true
    ).

format_local_list([],_).
format_local_list([M-Ds|T],Key) :-
    format("~q.~n",[local(M,Ds)]),
    format_local_list(T,Key).

local_trees(tree(M,_,Ds0),[M-Ds|Trees]) :-
    local_trees_ds(Ds0,Ds,Trees,[]).

local_trees_ds([],[],Ts,Ts).
local_trees_ds([H0|T0],[H|T],Ts0,Ts):-
    local_trees(H0,H,Ts0,Ts1),
    local_trees_ds(T0,T,Ts1,Ts).

local_trees(tree(Node,_,Ds),Result,Ts0,Ts):-
    local_trees_i(Ds,Node,Result,Ts0,Ts).

local_trees_i([],gap(Gap),gap(Gap),Ts,Ts).

local_trees_i([],lex(Tag),lex(Tag),Ts,Ts).
local_trees_i([D0|S0],Rule,Rule,[Rule-Ds|Ts0],Ts) :-
    local_trees_ds([D0|S0],Ds,Ts0,Ts).

		
ignore_existing([],[]).
ignore_existing([H|T],Trees):-
    ignore_ex(H,Trees,Trees1),
    ignore_existing(T,Trees1).

ignore_ex(M-Ds0,L0,L) :-
    alpino_cg:adapt(Ds0,Ds),
    (   alpino_cg:local(M,Ds)
    ->  L0=L
    ;   L0=[M-Ds|L]
    ).

print_unknown_bigrams(tree(_,_,Ds)) :-
    print_unknown_bigrams_ds(Ds).
print_unknown_bigrams(tree(M,_,Ds0)) :-
    l_trees_ds(Ds0,Ds),
    \+ alpino_cg:bigram(M,Ds),
    format("~q.~n",[bigram(M,Ds)]).

print_unknown_bigrams_ds(Ds) :-
    lists:member(tree(M,_,[D|E]),Ds),
    print_unknown_bigrams(tree(M,_,[D|E])).

l_trees_ds([],[]).
l_trees_ds([H0|T0],[H|T]):-
    l_trees(H0,H),
    l_trees_ds(T0,T).

l_trees(tree(Node,_,Ds),Result):-
    l_trees_i(Ds,Node,Result).

l_trees_i([],gap(Gap),gap(Gap)).

l_trees_i([],lex(Tag),lex(Class)):-
    alpino_tr_tag:tr_tag(Tag,Class).
l_trees_i([D0|S0],Rule,tree(Rule,Ds)):-
    l_trees_ds1([D0|S0],Ds).

l_trees_ds1([],[]).
l_trees_ds1([H0|T0],[H|T]):-
    l_trees1(H0,H),
    l_trees_ds1(T0,T).

l_trees1(tree(Node,_,Ds),Result):-
    l_trees_i1(Ds,Node,Result).

l_trees_i1([],gap(Gap),gap(Gap)).

l_trees_i1([],lex(Tag),lex(Class)):-
    alpino_tr_tag:tr_tag(Tag,Class).
l_trees_i1([_|_],Rule,Rule).

