:- module(alpino_dt, [ result_to_dt/2, result_to_dt/3,
		       result_to_dt_simple/2, % from penalties, not for export
		       format_triples_of_obj/1,
		       format_triples_of_result/1,
		       dt_to_relations/2,
		       dt_to_relations_without_positions/2,
		       dt_to_relations_without_postags/2,
		       dt_to_relations_with_full_postags/2,
		       dt_to_relations_with_somewhat_simplified_postags/2,
		       dt_to_relations_with_attributes/3,
		       apply_dt_transformations/4,
		       result_to_triples/2,
		       result_to_triples_without_postags/2,
		       result_to_triples_with_full_postags/2,
		       format_triples_without_postags_of_result/2,
		       format_triples_gosse_of_result/2,
		       format_triples_harmen_of_result/2,
		       format_triples_with_postags_of_result/2,
		       format_triples_with_full_postags_of_result/2,
		       format_full_triples_with_full_postags_of_result/3,
		       format_triples_with_frames_of_result/2]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(hdrug(hdrug_util)).
:- use_module(utils).

% :- initialize_flag(check_dt_variable,off).

:- initialize_flag(triples_undo_mwu,on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% dt manipulation %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% utils for dt and user(dt)

%% read back from .xml file:
result_to_dt(Cat,Result) :-
    result_to_dt(Cat,normal,Result).

result_to_dt(Var,_,Result) :-
    var(Var),
    !,
    Result=none.
result_to_dt(already_dt(Tree),_,Result) :-
    !,
    dt_canonical_dt(Tree,Result).
result_to_dt(already_canonical_dt(Tree),_,Result) :-
    !,
    Result = Tree.
result_to_dt(Result0,Flag,SortedTree) :-
    (   result_to_dt__(Result0,Flag,SortedTree)
    ->  true
    ;   format(user_error,"ERROR: result_to_dt/3 failed!~n",[]),
	fail
    ).

result_to_dt__(Result0,Flag,SortedTree) :-
    alpino_data:result_cdt(Result0,SortedTree),
    (   nonvar(SortedTree)
    ->  true
    ;   copy_term(Result0,Result),
	alpino_data:result(Result,List,Tokens0),
	alpino_treebank:remove_phantoms(Tokens0,Tokens1),
	alpino_format_syntax:result_to_frames(Result,Frames,_),
	(   nonvar(List)
	->  get_phantoms(Positions),
	    graphic_path_dt_list(List,Tree),
	    rewrite_labels(Tree,TreeWithLabels0),
	    renumber_positions(TreeWithLabels0,TreeWithLabels1,Positions),
	    (   nonvar(Tokens1)
	    ->  add_nodes_for_missing_tokens(TreeWithLabels1,TreeWithLabels2,Flag,Tokens1,Frames),
		add_nodes_for_mwu(TreeWithLabels2,TreeWithLabels,Tokens1)
	    ;   TreeWithLabels1=TreeWithLabels
	    ),
	    dt_canonical_dt(TreeWithLabels,SortedTree),
	    check_variable_cat(SortedTree)
	;   SortedTree = none
	)
    ).

% result_to_dt_simple(Result0,DtTree) :-
%     hdrug_flag(check_dt_variable,Prev,off),
%     call_cleanup(result_to_dt_simple_(Result0,DtTree),
%                  set_flag(check_dt_variable,Prev)
%                 ).

result_to_dt_simple(Result0,DtTree) :-
    copy_term(Result0,Result),
    alpino_data:result(Result,List,_),
    (   nonvar(List)
    ->  graphic_path_dt_list_simple(List,Tree),
	rewrite_labels(Tree,DtTree)
    ;   DtTree = none
    ).

:- initialize_flag(order_canonical_dt,on).

dt_canonical_dt(Tree0,Tree) :-
    hdrug_flag(order_canonical_dt,Bool),
    dt_canonical_dt(Bool,Tree0,Tree).

dt_canonical_dt(on,Tree0,Tree) :-
    dt_canonical_dt_sort_ds(Tree0,Tree1),
    dt_canonical_dt_indices(Tree1,Tree),
    prettyvars(Tree).
dt_canonical_dt(off,Tree0,Tree) :-
    order_somewhat(Tree0,Tree1),
    dt_canonical_dt_indices(Tree1,Tree),
    prettyvars(Tree).

rewrite_labels(tree(Rel/L,i(I,R),Ds0),tree(r(Rel,Label),_,Ds)) :-
    rest_label(R,I,L,Label),
    rewrite_labels_ds(Ds0,Ds).

rewrite_labels_ds([],[]).
rewrite_labels_ds([H0|T0],[H|T]) :-
    rewrite_labels(H0,H),
    rewrite_labels_ds(T0,T).

rest_label(R,I,L,Label) :-
    (	nonvar(R)
    ->	Label=i(I,Node),
	rest_label1(L,Node)
    ;	rest_label_(L,Label)
    ).

rest_label_(L,Label) :-
    var(L),
    !,
    rest_label1(L,Label).
rest_label_(i(L1),Label) :-
    !,
    Label=i(L1).
rest_label_(L,Label) :-
    rest_label1(L,Label).


rest_label1(L,Node) :-
    (	nonvar(L),
	L=(postag(Pos,Cat):Wrd)
    ->	Node=l(Pos,Cat,Wrd)
    ;   Node=p(L)
    ).

%% complicated
%% - because it's old code
%% - because it should be steadfast (called from penalties from unfinished
%%   parse results)
%% - called from robust with either a single parse or a list of parses
graphic_path_dt_list(Ds,Tree) :-
    graphic_path_dt_list_(Ds,NDs,[],0,_),
    nds_to_tree(NDs,Tree).

nds_to_tree([],tree(top/du,_,[])).  % the empty dt
nds_to_tree([H|T],Tree) :-
    nds_to_tree(T,H,Tree).

nds_to_tree([],Tree0,Tree) :-
    converse_dependency_structure(Tree0,Tree).
nds_to_tree([H|T],F,Tree) :-
    list_to_ds([F,H|T],Ds),
    converse_dependency_structure(tree(top/du,_,Ds),Tree).

list_to_ds([],[]).
list_to_ds([tree(top/Node,Ix,Ds)|T],
	   [tree(ATT/Node,Ix,Ds)|NT]) :-
    hdrug_flag(robust_attr,ATT),
    list_to_ds(T,NT).

:- initialize_flag(robust_attr,dp).

graphic_path_dt_list_([],Ts,Ts,Nr,Nr).
graphic_path_dt_list_([Dtr|T],Ts0,Ts,Nr0,Nr) :-
    alpino_data:dt(Dtr,DT),
    dt_to_d(top/DT,Nr0,Nr1,Ts0,Ts1),
    graphic_path_dt_list_(T,Ts1,Ts,Nr1,Nr).

list_valued_daughters([],L,L).
list_valued_daughters([H/HList|T],L0,L) :-
    (	nonvar(HList)
    ->	list_daughters(HList,H,L0,L1)
    ;	L0=L1
    ),
    list_valued_daughters(T,L1,L).

list_daughters([],_,L,L).
list_daughters([H|T],Rel,L0,L):-
    (	var(H)
    ->	L0 = L1
    ;	L0 = [Rel/H|L1]
    ),
    (	nonvar(T)
    ->	list_daughters(T,Rel,L1,L)
    ;	L1=L
    ).

daughters(Dom,List) :-
    alpino_data:dt_daughters(Dom,SingleValued,ListValued),
    single_valued_daughters(SingleValued,List,List1),
    list_valued_daughters(ListValued,List1,[]).

single_valued_daughters([],Ds,Ds).
single_valued_daughters([Att/Arg|Atts],List0,List) :-
    (   nonvar(Arg)
    ->	List0 = [Att/Arg|List1]
    ;   List0 = List1
    ),
    single_valued_daughters(Atts,List1,List).


dt_to_d(Rel/Node,N0,N,Trees0,Trees):-
    (   var(Node)
    ->  Trees0=Trees,
	N0=N,
	debug_message(1,"warning: variable value for ~w~n",[Rel])
    ;   Node == []
    ->  Trees0=Trees,
	N0=N 
    ;   Trees0=[TREE|Trees],
	%%% there was something wrong with hwrd/fwrd in with_dt
	%%% train_generation('WS-U-E-A-0000000040.p.4.s.1').
	%%% solved by instantiating hwrd to be [] for some categories in lex_types.gram l 7787
	%%% also h_suite/866 gave multiple solutions - solved by initializing dt in van_en rule
	alpino_data:dt_with_hwrd(Node,Label,Pos,Cat,Ix),
	(   var(Label)
	->  debug_message(3,"warning: variable hwrd in ~w~n",[Rel/Node])
	;   alpino_data:hwrd_with_pos(Label,Begin)
	),
	(   var(Ix)
	->  Ix=i(N0,_), N1 is N0+1,
	    TREE=tree(Rel/Cat,Ix,Ds),
	    daughters(Node,List),
	    dt_list_to_ds(List,Ds0,N1,N2),
	    (   nonvar(Begin), Label \== []
	    ->  alpino_data:lix(Node,LIX),
		label_dt_to_d(LIX,Label,Pos,Cat,Node,Dtr,N2,N,Rel),
		Ds=[Dtr|Ds0]
	    ;   Ds=Ds0,
		N2=N
	    )
	;	N0 = N,
	    Ix=i(Prev,y),
	    TREE=tree(Rel/i(Prev),_,[])
	)
    ).

label_dt_to_d(Ix,Label,Pos,Cat,Node,Dtr,N0,N,Rel) :-
    alpino_data:label(Label,Root,Lemma,_His,Q0,Q),
    (   Root == Lemma
    ->  Root = Word
    ;   var(Lemma)
    ->  Root = Word
    ;   Lemma= Lemma1,
	Root = Root1,
	Word = v_root(Root1,Lemma1)
    ),

    (   var(Q0)
    ->  debug_message(5,"warning: variable positions~n",[])
        %%% this message always fires for generation if veryfast is used... why?
    ;   integer(Q0),
	Q is Q0+1
    ->  P0=Q0, P=Q
    ;	P0=Q0, P=Q
    ),

    try_hook(user:dt_extract_attributes(Node,Attributes,Rel),Attributes=[]),
    
    (   var(Ix)
    ->  Ix=i(N0,_), N is N0+1,
        Dtr = tree(hd/(postag(Attributes:Pos,Cat):(Word/[P0,P])),Ix,[])
    ;   N0=N,
        Ix=i(Prev,y),
        Dtr=tree(hd/i(Prev),_,[])
    ).

dt_list_to_ds([],[],N,N).
dt_list_to_ds([H0|T0],Out,N0,N) :-
    dt_to_d(H0,N0,N1,Out,Out1),
    dt_list_to_ds(T0,Out1,N1,N).

graphic_path_dt_list_simple(Ds,Tree) :-
    graphic_path_dt_list_(Ds,NDs,[],0,_),
    nds_to_tree_simple(NDs,Tree).

nds_to_tree_simple([],tree(top/du,_,[])).  % the empty dt
nds_to_tree_simple([H|T],Tree) :-
    nds_to_tree_simple(T,H,Tree).

nds_to_tree_simple([],Tree,Tree).
nds_to_tree_simple([H|T],F,tree(top/du,_,Ds)) :-
    list_to_ds([F,H|T],Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% CONVERSE_DEPENDENCY_STRUCTURE %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialize_flag(converse_dependency,on).

converse_dependency_structure(Tree0,Tree) :-
    hdrug_flag(converse_dependency,OnOff),
    if(converse_dependency_structure0(OnOff,Tree0,Tree),
       true,
       (   format(user_error,"ERROR: converse_dependency_structure/2 failed!~n",[]),
	   fail
       )
      ).

converse_dependency_structure0(off,Tree,Tree).
converse_dependency_structure0(on,Tree0,Tree) :-
    converse_conj(Tree0,Tree1),
    (   Tree0 == Tree1
    ->  true
    ;   debug_message(2,"warning: non-reversible transformation applied (coord)~n",[])
    ),
    converse_phantom(Tree1,Tree2),
    converse_unary(Tree2,Tree).

converse_unary1(tree(Rel/_Cat,Ix,[tree(_/P,_,[])]),Tree) :-
    !,
    Tree=tree(Rel/P,Ix,[]).
converse_unary1(Tree,Tree).
%converse_unary1(tree(Rel/_,_Ix,[tree(crd/_,_,[tree(hd/Cat0,Ix0,Ds0)])]),Tree) :-
%    !,
%    converse_unary(tree(Rel/Cat0,Ix0,Ds0),Tree).


converse_unary(tree(Node,Ix,Ds0),Tree) :-
    converse_unary_ds(Ds0,Ds),
    converse_unary1(tree(Node,Ix,Ds),Tree).

converse_unary_ds([],[]).
converse_unary_ds([tree(_Rel/Cat,_,[])|T0],T) :-
    atomic(Cat),!,
    converse_unary_ds(T0,T).
converse_unary_ds([H0|T0],[H|T]) :-
    converse_unary(H0,H),
    converse_unary_ds(T0,T).

%%% for
%%% "ik wil een boek kopen en een plaat draaien"
%%% where conj at smain is lowered into conj of vp
%%% NON-REVERSIBLE!!
converse_conj_ds([],[]).
converse_conj_ds([H0|T0],[H|T]) :-
    converse_conj(H0,H),
    converse_conj_ds(T0,T).

converse_conj(tree(Rel/conj,Index,Ds0),Tree) :-
    cnj_crd_ds(Ds0,Cnjs,[],Crds,[],Cat),
    Cnjs = [_,_|_],
    coindexed_but_one(Cnjs,OrigDs,RealDs0),
    replace_rel_with_cnj(RealDs0,RealDs,VC),
    lists:append(RealDs,Crds,ConjDs),
    ConjD = tree(VC/conj,_,ConjDs),
    Ds = [ConjD | OrigDs],
    !,
    converse_conj(tree(Rel/Cat,Index,Ds),Tree).
converse_conj(tree(Rel/Node,IX,Ds0),tree(Rel/Node,IX,Ds)):-
    converse_conj_ds(Ds0,Ds).

cnj_crd_ds([],Cnjs,Cnjs,Crds,Crds,_).
cnj_crd_ds([H|T],Cnjs0,Cnjs,Crds0,Crds,Cat) :-
    cnj_crd_d(H,Cnjs0,Cnjs1,Crds0,Crds1,Cat),
    cnj_crd_ds(T,Cnjs1,Cnjs,Crds1,Crds,Cat).

cnj_crd_d(tree(Rel/Cat,Ix,Ds),Cnjs0,Cnjs,Crds0,Crds,Cat2) :-
    (   Rel == cnj
    ->  Cnjs0 = [tree(Rel/Cat,Ix,Ds)|Cnjs],
	Crds0 = Crds,
	Cat=Cat2
    ;   Rel == crd,
	Cnjs0 = Cnjs,
	Crds0 = [tree(Rel/Cat,Ix,Ds)|Crds]
    ).

%% OrigDs: the antencedents of the coindexed daughters
%% RealDs: the non-coindexed daughter of each conj
coindexed_but_one(Cnjs,Ds2,[Real|RealDs]) :-
    select(tree(_,_,Ds),Cnjs,Cnjs1),  % select the "antecedent"
    select(Real,Ds,Ds2),              % select a real d from the antecedent
    all_ds_coi(Ds2,Cnjs1,Cnjs2),      % remove all coindexed ones
    each_real(Cnjs2,Real,RealDs).     % remaining ones should be all reals


%% each D in Ds should have a coindexed alternative in each of Cnjs
all_ds_coi([],C,C).
all_ds_coi([H|T],C0,C) :-
    get_index_rel(H,IX,REL),
    all_d_coi(C0,IX,REL,C1),
    all_ds_coi(T,C1,C).

get_index_rel(tree(REL/_,i(IX,_),_),IX,REL) :-
    nonvar(IX).
get_index_rel(tree(REL/i(IX),_,[]),IX,REL) :-
    nonvar(IX).

all_d_coi([],_,_,[]).
all_d_coi([tree(A,B,Ds0)|T0],IX,REL,[tree(A,B,Ds)|T]) :-
    lists:select(tree(REL/i(IX),_,[]),Ds0,Ds), 
    all_d_coi(T0,IX,REL,T).

%% every remaining D in DS should be unary tree with same relation as REAL
each_real([],_,[]).
each_real([tree(_,_,[tree(A,B,C)])|T],tree(A,D,E),[tree(A,B,C)|T2]) :-
    each_real(T,tree(A,D,E),T2).

replace_rel_with_cnj([],[],_).
replace_rel_with_cnj([tree(REL/CAT,Ix,Ds)|T0],[tree(cnj/CAT,Ix,Ds)|T],REL) :-
    replace_rel_with_cnj(T0,T,REL).

%%%%%%%%%%%%%%%%%%%%%
%% DT_TO_RELATIONS %%
%%%%%%%%%%%%%%%%%%%%%

dt_to_relations(tree(r(_,L),_,Ds),Head,Rs,Re0,Re) :-
    dt_to_relations(L,Ds,Head,Rs,[],Re0,Re).

dt_to_relations(i(Index),[],Head,Rs,Rs,Re0,Re) :-
    member_or_add(Re0,i(Index,Head),Re).

dt_to_relations(i(Index,Label),Ds,Head,Rs0,Rs,Re0,Re) :-
    dt_to_relations(Label,Ds,Head,Rs0,Rs,Re0,Re1),
    member_or_add(Re1,i(Index,Head),Re).

dt_to_relations(l(PosTerm,_,Word0/Pos),[],PosTerm:(Word/Pos),Rs,Rs,Re,Re) :-
    root_of_pair(Word0,Word).

dt_to_relations(p(_Cat),Ds,Head,Rs0,Rs,Re0,Re) :-
    dt_to_relations_p(Ds,Head,Rs0,Rs,Re0,Re).

dt_to_relations_p([DsH|DsT],Head,Rs0,Rs,Re0,Re) :-
    select_head(HeadRel,Head,[DsH|DsT],Ds2,Rs0,Rs1,Re0,Re1),
    ds_to_relations(Ds2,HeadRel,Head,Rs1,Rs,Re1,Re).
dt_to_relations_p([],none,Rs,Rs,Re,Re).  % only for empty dt?

ds_to_relations([],_,_,Rs,Rs,Re,Re).
ds_to_relations([tree(r(Rel,Label),_,Ds)|Rest],HeadRel,Head,
		[deprel(Head,HeadRel/Rel,Ehd)|Rs0],Rs,Re0,Re) :-
    dt_to_relations(Label,Ds,Ehd,Rs0,Rs1,Re0,Re1),
    ds_to_relations(Rest,HeadRel,Head,Rs1,Rs,Re1,Re).

%% normal case, do not select -- daughter
select_head(HeadRel,Head,Ds0,Ds,Rs0,Rs,Re0,Re) :-
    head_rel(HeadRel), 
    select(tree(r(HeadRel,Label),_,More),Ds0,Ds),
    !,
    dt_to_relations(Label,More,Head,Rs0,Rs,Re0,Re).

%% catch all
select_head(Rel,Head,Ds0,Ds,Rs0,Rs,Re0,Re) :-
    select(tree(r(Rel,Cat),_,More),Ds0,Ds),
    \+ Rel = '--',
    !,
    dt_to_relations(Cat,More,Head,Rs0,Rs,Re0,Re).

%% top level case, select non-lexical -- daughter
%% since we now have mwu skipped nodes, we will take the
%% largest non-lexical -- daughter
select_head('--',Head,Ds,Ds,Rs0,Rs,Re0,Re) :-
    dt_to_relations(l(none,none,none/[0,0]),[],Head,Rs0,Rs,Re0,Re).


head_rel(hd).
head_rel(cmp).
head_rel(crd).
head_rel(dlink).
head_rel(rhd).
head_rel(whd).
head_rel(Rel) :-
    dif(Rel,'--').

root_of_pair(v_root(Root0,_Lemma),Root) :-
    !,
    Root0=Root.
root_of_pair(Root,Root).

member_or_add([],El,[El]).
member_or_add([H|T],El,[H|NT]) :-
    (	H=El
    ->	NT=T
    ;	member_or_add(T,El,NT)
    ).

member_or_add(El,List) :-
    member(El,List), !.

%%%%%%%%%%%%%%%%%%%%%
%% DT_TO_RELATIONS %%
%%%%%%%%%%%%%%%%%%%%%

dt_to_relations_lemma(tree(r(_,L),_,Ds),Head,Rs,Re0,Re) :-
    dt_to_relations_lemma(L,Ds,Head,Rs,[],Re0,Re).

dt_to_relations_lemma(i(Index),[],Head,Rs,Rs,Re0,Re) :-
    member_or_add(Re0,i(Index,Head),Re).

dt_to_relations_lemma(i(Index,Label),Ds,Head,Rs0,Rs,Re0,Re) :-
    dt_to_relations_lemma(Label,Ds,Head,Rs0,Rs,Re0,Re1),
    member_or_add(Re1,i(Index,Head),Re).

dt_to_relations_lemma(l(_PosTerm,_,Word0/Pos),[],Word/Pos,Rs,Rs,Re,Re) :-
    lemma_of_pair(Word0,Word).

dt_to_relations_lemma(p(_Cat),Ds,Head,Rs0,Rs,Re0,Re) :-
    dt_to_relations_lemma_p(Ds,Head,Rs0,Rs,Re0,Re).

dt_to_relations_lemma_p([DsH|DsT],Head,Rs0,Rs,Re0,Re) :-
    select_head_lemma(HeadRel,Head,[DsH|DsT],Ds2,Rs0,Rs1,Re0,Re1),
    ds_to_relations_lemma(Ds2,HeadRel,Head,Rs1,Rs,Re1,Re).
dt_to_relations_lemma_p([],none,Rs,Rs,Re,Re).  % only for empty dt?

ds_to_relations_lemma([],_,_,Rs,Rs,Re,Re).
ds_to_relations_lemma([tree(r(Rel,Label),_,Ds)|Rest],HeadRel,Head,
		[deprel(Head,HeadRel/Rel,Ehd)|Rs0],Rs,Re0,Re) :-
    dt_to_relations_lemma(Label,Ds,Ehd,Rs0,Rs1,Re0,Re1),
    ds_to_relations_lemma(Rest,HeadRel,Head,Rs1,Rs,Re1,Re).

select_head_lemma(HeadRel,Head,Ds0,Ds,Rs0,Rs,Re0,Re) :-
    head_rel(HeadRel),
    select(tree(r(HeadRel,Label),_,More),Ds0,Ds),
    !,
    dt_to_relations_lemma(Label,More,Head,Rs0,Rs,Re0,Re).
select_head_lemma(Rel,Head,Ds0,Ds,Rs0,Rs,Re0,Re) :-
    select(tree(r(Rel,Cat),_,More),Ds0,Ds),
    \+ Rel = '--',
    !,
    dt_to_relations_lemma(Cat,More,Head,Rs0,Rs,Re0,Re).

%% top level case, select non-lexical -- daughter
%% since we now have mwu skipped nodes, we will take the
%% largest non-lexical -- daughter
select_head_lemma('--',Head,Ds,Ds,Rs0,Rs,Re0,Re) :-
    dt_to_relations_lemma(l(none,none,none/[0,0]),[],Head,Rs0,Rs,Re0,Re).


lemma_of_pair(v_root(_Root,Lemma0),Lemma) :-
    !,
    Lemma0=Lemma.
lemma_of_pair(Lemma,Lemma).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% RESULT to TRIPLES %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public format_triples_of_obj/1.

format_triples_of_obj(N) :-
    hdrug:object(N,o(Cat,_,_)),
    format_triples_of_result(Cat).

format_triples_of_result(Result) :-
    result_to_triples(Result,Triples),
    format_relations(Triples).

result_to_triples(Result,Relations) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations(DT,Relations).

result_to_triples_with_full_postags(Result,Relations) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_with_full_postags(DT,Relations).

result_to_triples_without_postags(Result,Relations) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_without_postags(DT,Relations).

result_to_triples_without_postags_with_mwu(Result,Relations) :-
    result_to_dt(Result,DT),
    dt_to_relations_lemma(DT,Relations).

format_triples_without_postags_of_result(Result,Key) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_without_postags(DT,Relations),
    format_relations(Relations,Key).

format_triples_gosse_of_result(Result,Key) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_without_postags(DT,Relations0),
    sort(Relations0,Relations),
    format_relations_gosse(Relations,Key).

format_triples_harmen_of_result(Result,Key) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_harmen(DT,Relations0),
    sort(Relations0,Relations),
    format_relations_gosse(Relations,Key).

format_triples_with_frames_of_result(Result,Key) :-
    format_triples_with_postags_of_result(Result,Key).

format_triples_with_postags_of_result(Result,Key) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_harmen(DT,Relations0),
    sort(Relations0,Relations),
    format_relations(Relations,Key).

format_triples_with_full_postags_of_result(Result,Key) :-
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_with_full_postags(DT,Relations0),
    sort(Relations0,Relations),
    format_relations_with_full_postags(Relations,Key).

format_full_triples_with_full_postags_of_result(Result,Key,String0) :-
    alpino_lexical_analysis:remove_brackets(String0,String1),
    alpino_treebank:remove_phantoms(String1,String),
    result_to_dt(Result,DT0),
    hdrug_flag(triples_undo_mwu,On),
    maybe_undo_mwp(On,DT0,DT),
    dt_to_relations_with_full_postags(DT,Relations0),
    sort(Relations0,Relations),
    format_full_relations_with_full_postags(Relations,Key,String).

format_relations_gosse(Triples,Key) :-
    format("triples(~q,~q).~n",[Key,Triples]).

dt_to_relations(DT,Rels) :-
    dt_to_relations_start(DT,Rels0),
    relations_simplify_frames(Rels0,Rels).

dt_to_relations_with_full_postags(DT,Rels) :-
    dt_to_relations_start(DT,Rels).

dt_to_relations_without_positions(DT,Rels) :-
    dt_to_relations_start(DT,Rels0),
    relations_simplify_frames(Rels0,Rels1),
    relations_ignore_string_pos(Rels1,Rels).

dt_to_relations_without_postags(DT,Rels) :-
    dt_to_relations_start(DT,Rels0),
    relations_ignore_pos(Rels0,Rels).

dt_to_relations_with_lemma(DT,Rels) :-
    dt_to_relations_start(DT,Rels0),
    relations_with_lemma(Rels0,Rels).

dt_to_relations_harmen(DT,Rels) :-
    dt_to_relations_start(DT,Rels0),
    relations_simplify_frames(Rels0,Rels1),
    relations_harmen(Rels1,Rels).

dt_to_relations_start(DT,RELS) :-
    dt_to_relations(DT,_,RELS,[],_).
    
dt_to_relations_lemma(DT,RELS) :-
    dt_to_relations_lemma(DT,_,RELS,[],_).

relations_simplify_frames([],[]).
relations_simplify_frames([H0|T0],[H|T]) :-
    relations_simplify_frames_(H0,H),
    relations_simplify_frames(T0,T).

relations_simplify_frames_(deprel(Pa0:Ha,R,Pb0:Hb),
			   deprel(Pa :Ha,R,Pb :Hb)) :-
    relations_simplify_frame(Pa0,Pa),
    relations_simplify_frame(Pb0,Pb).

relations_simplify_frame(PosTerm,Pos) :-
    try_hook(alpino_postags:postag_of_frame(PosTerm,Pos,_),
			(   nonvar(PosTerm)
			->  functor(PosTerm,Pos,_)
			;   Pos=none
			)
		       ).

relations_harmen([],[]).
relations_harmen([H0|T0],[H|T]) :-
    relations_harmen_(H0,H),
    relations_harmen(T0,T).

relations_harmen_(deprel(Pos:H,Rel,Pos2:H2),deprel(H,Pos,Rel,H2,Pos2)).

relations_ignore_string_pos([],[]).
relations_ignore_string_pos([H0|T0],[H|T]) :-
    relations_ignore_string_pos_(H0,H),
    relations_ignore_string_pos(T0,T).

% relations_ignore_string_pos_(deprel(P:(H/_),R,P2:(H2/_)),deprel(P:H,R,P2:H2)).
relations_ignore_string_pos_(deprel(A0,Rel,B0),
			     deprel(A,Rel,B)) :-
    relations_ignore_string_pos__(A0,A),
    relations_ignore_string_pos__(B0,B).

relations_ignore_string_pos__(P:(H/_),P:H).
%% relations_ignore_string_pos__(none,none).  % for "old" xml files



relations_ignore_pos([],[]).
relations_ignore_pos([H0|T0],[H|T]) :-
    relations_ignore_pos_(H0,H),
    relations_ignore_pos(T0,T).

relations_ignore_pos_(deprel(_:H,Rel,_:H2),deprel(H,Rel,H2)).

relations_with_lemma([],[]).
relations_with_lemma([H0|T0],[H|T]) :-
    relations_with_lemma_(H0,H),
    relations_with_lemma(T0,T).

relations_with_lemma_(deprel(Tag:WordPos,Rel,Tag2:WordPos2),
		      deprel(H/Pos,      Rel,H2/Pos2)
		     ) :-
    postag_word(Tag, WordPos, H, Pos),
    postag_word(Tag2,WordPos2,H2,Pos2).

postag_word(read_from_treebank(_,H,_),_/Pos,H,Pos).
postag_word(read_from_treebank(_,_,H,_),_/Pos,H,Pos).
postag_word(_,Root/Pos,Lemma,Pos) :-
    (   Root = v_root(_,Lemma)
    ->  true
    ;   Root = Lemma
    ).


format_relations([]).
format_relations([H|T]) :-
    format_relation(H),
    format_relations(T).

format_relation(deprel(Word,Rel,Word2)) :-
    check_var(Word), check_var(Rel), check_var(Word2),
    format("~w|~w|~w~n",[Word,Rel,Word2]).

format_relation(deprel(Word,Pos,Rel,Word2,Pos2)) :-
    check_var(Word), check_var(Pos),
    check_var(Rel), check_var(Word2), check_var(Pos2),
    format("~w|~w|~w|~w|~w~n",[Word,Pos,Rel,Word2,Pos2]).

format_relation(val(Head,Val0)) :-
    check_var(Head),
    keysort(Val0,Val),
    chuck_keys(Val,Rels),
    format("+~q|~q~n",[Head,Rels]).

format_relation(word(Word)) :-
    check_var(Word),
    format("$~q~n",[Word]).

chuck_keys([],[]).
chuck_keys([_-X|Xs],[X|Ys]) :-
    check_var(X), 
    chuck_keys(Xs,Ys).	

format_relations([],_).
format_relations([H|T],Key) :-
    format_relation(H,Key),
    format_relations(T,Key).

format_relations_with_full_postags([],_).
format_relations_with_full_postags([H|T],Key) :-
    format_relation_with_full_postags(H,Key),
    format_relations_with_full_postags(T,Key).

format_relation_with_full_postags(deprel(POSTAG1:Word1/POS1,
                                         Rel,
                                         POSTAG2:Word2/POS2),Key) :-
    check_var(Word1),
    check_var(Rel),
    check_var(Word2),
    format("~w|~w|~w|~w|~w|~w~n",[Word1/POS1,POSTAG1,
                                  Rel,
                                  Word2/POS2,POSTAG2,Key]).

format_full_relations_with_full_postags([],_,_).
format_full_relations_with_full_postags([H|T],Key,String) :-
    format_full_relation_with_full_postags(H,Key,String),
    format_full_relations_with_full_postags(T,Key,String).

format_full_relation_with_full_postags(deprel(POSTAG1:Root1/POS1,
                                              Rel,
                                              POSTAG2:Root2/POS2),
                                       Key,String) :-
    check_var(Root1),
    (   POS1 == top
    ->  P0 = 0, P = 0, Word1 = top
    ;   POS1=[P0,P],
	alpino_treebank:get_root(P0,P,String,Word1)
    ),
    relations_simplify_frame(POSTAG1,SIMPLE1),
    somewhat_simplify_frame(POSTAG1,SOME1),
    check_var(Rel),
    check_var(Root2),
    (   POS2 == top
    ->  Q0 = 0, Q = 0, Word2 = top
    ;   POS2=[Q0,Q],
	alpino_treebank:get_root(Q0,Q,String,Word2)
    ),
    relations_simplify_frame(POSTAG2,SIMPLE2),
    somewhat_simplify_frame(POSTAG2,SOME2),
    format("~w|~w|~w|~w|~w|~w|~w|~w|~w|~w|~w|~w|~w|~w|~w|~w~n",
           [Root1,Word1,P0,P,SIMPLE1,SOME1,POSTAG1,
            Rel,
            Root2,Word2,Q0,Q,SIMPLE2,SOME2,POSTAG2,
            Key]).

format_relation(deprel(Word,Rel,Word2),Key) :-
    check_var(Word), check_var(Rel), check_var(Word2),
    format(user_output,"~w|~w|~w|~w~n",[Word,Rel,Word2,Key]).

format_relation(deprel(Word,Pos,Rel,Word2,Pos2),Key) :-
    check_var(Word), check_var(Pos),
    check_var(Rel), check_var(Word2), check_var(Pos2),
    format("~w|~w|~w|~w|~w|~w~n",[Word,Pos,Rel,Word2,Pos2,Key]).

check_var(W) :-
    (	nonvar(W)
    ->	true
    ;	W = 'ERROR'
    ).

%% 1. replace index with P0-P of corresponding antecedent
%% 2. add key to each daughter
%%    a. lexical daughter: p(P0,P)
%%    b. index node:       i(Key) where Key is key of antecedent
%%    c. complex node:     sorted list of keys of daughter
dt_canonical_dt_sort_ds(Tree0,Tree) :-
    dt_canonical_add_keys(Tree0,_-Tree,_,[],_).

dt_canonical_add_keys(    tree(r(Rel,Node),_,Ds0),
		      Key-tree(r(Rel,Node),Key,Ds),Key,Re0,Re) :-
    dt_canonical_add_keys_ds(Ds0,Ds,Node,Key,Re0,Re).

dt_canonical_add_keys_ds([],[],Node,Key,Re0,Re) :-
    dt_canonical_add_keys_leaf(Node,Key,Re0,Re).
dt_canonical_add_keys_ds([H0|T0],[H|T],Node,Key,Re0,Re) :-
    dt_canonical_add_keys_complex(Node,Key,Re0,Re1),
    dt_canonical_add_keys_ds1([H0|T0],[H1|T1],Key0,Re1,Re),
    when(ground(Key0),  % wait until all indices are resolved...
	 (  dt_canonical_construct_key(Key0,Key),
	    sort([H1|T1],[H2|T2]),
	    vals([H2|T2],[H|T])
	 )
	).

:- public dt_canonical_construct_key/2, vals/2.

dt_canonical_construct_key(Keys0,Last-Key) :-
    append_all_vals(Keys0,Key0),
    sort(Key0,Key),
    last(Key,Last).

append_all_vals([],[]).
append_all_vals([_-H|T],List) :-
    append_all_vals(T,H,List).

append_all_vals([],L,L).
append_all_vals([_-H|T],L0,L) :-
    append(L0,H,L1),
    append_all_vals(T,L1,L).

dt_canonical_add_keys_ds1([],[],[],Re,Re).
dt_canonical_add_keys_ds1([H0|T0],[H|T],[Key0|Keys],Re0,Re) :-
    dt_canonical_add_keys(H0,H,Key0,Re0,Re1),
    dt_canonical_add_keys_ds1(T0,T,Keys,Re1,Re).

vals([],[]).
vals([_-H|T1],[H|T]) :-
    vals(T1,T).

dt_canonical_add_keys_complex(p(_),_,R,R).
dt_canonical_add_keys_complex(i(Index,_),Key,Re0,Re) :-
    member_or_add(Re0,Index=Key,Re).

dt_canonical_add_keys_leaf(p(_),[]-[],R,R).  % only for empty dt?
dt_canonical_add_keys_leaf(l(_,_,_/[P0,_P]),P0-[P0],Re,Re).
dt_canonical_add_keys_leaf(i(Index),Key,Re0,Re) :-
    member_or_add(Re0,Index=Key,Re).
dt_canonical_add_keys_leaf(i(Index,l(_,_,_/[P0,_P])),P0-[P0],Re0,Re) :-
    member_or_add(Re0,Index=P0-[P0],Re).

%%%% "Om te leven met ALS is al zwaar genoeg , maar zoals Steve ermee leeft ... zo voor het oog van de hele wereld , dan komt daar heel veel opoffering bij kijken ... en dat is opoffering door je familie en moet je je prioriteiten goed hebben .

dt_canonical_dt_indices(Tree0,Tree) :-
    dt_canonical_dt_indices_collect(Tree0,Info), % collect info for indexed nodes
    renumber_indices(Tree0,Tree1,Info,[],Assigned,[],Used,0,_),
    check_unique(Tree1,Tree,Assigned,Used).

%%%% only required for transformed trees???
check_unique(Tree0,Tree,Assigned,Used) :-
    vals(Assigned,Ants0),
    list_to_ord_set(Ants0,Ants),
    list_to_ord_set(Used,Anas),
    ord_subtract(Ants,Anas,Super),
    remove_superfl(Super,Tree0,Tree).

remove_superfl([],Tree,Tree).
remove_superfl([H|T],tree(Label0,Key,Ds0),tree(Label,Key,Ds)) :-
    remove_superfl_label_r(Label0,Label,[H|T]),
    remove_superfl_ds(Ds0,Ds,[H|T]).

remove_superfl_ds([],[],_).
remove_superfl_ds([H0|T0],[H|T],List) :-
    remove_superfl(List,H0,H),
    remove_superfl_ds(T0,T,List).

remove_superfl_label_r(r(Rel,Label0),r(Rel,Label),List) :-
    remove_superfl_label(Label0,Label,List).

remove_superfl_label(l(A,B,C),l(A,B,C),_).
remove_superfl_label(p(A),p(A),_).
remove_superfl_label(i(N),i(N),_).
remove_superfl_label(i(N,Stuff),New,List) :-
    (   member(N,List)
    ->  New=Stuff
    ;   New=i(N,Stuff)
    ).

renumber_indices(tree(Label0,Key,Ds0),tree(Label,Key,Ds),Info,Vs0,Vs,Us0,Us,N0,N) :-
    renumber_label_r(Label0,Label,Ds0,Ds,Info,Vs0,Vs,Us0,Us,N0,N).

renumber_daughters([],[],_,Vs,Vs,Us,Us,N,N).
renumber_daughters([H0|T0],[H|T],Info,Vs0,Vs,Us0,Us,N0,N) :-
    renumber_indices(H0,H,Info,Vs0,Vs1,Us0,Us1,N0,N1),
    renumber_daughters(T0,T,Info,Vs1,Vs,Us1,Us,N1,N).

renumber_label_r(r(Rel,Label0),r(Rel,Label),Ds0,Ds,Info,Vs0,Vs,Us0,Us,N0,N) :-
    renumber_label(Label0,Label,Ds0,Ds,Info,Vs0,Vs,Us0,Us,N0,N).

renumber_label(l(A,B,C),l(A,B,C),[],[],_Info,Vs,Vs,Us,Us,N,N).
renumber_label(p(A),p(A),Ds0,Ds,Info,Vs0,Vs,Us0,Us,N0,N) :-
    renumber_daughters(Ds0,Ds,Info,Vs0,Vs,Us0,Us,N0,N).
renumber_label(i(Old),NewNode,[],Ds,Info,Vs0,Vs,Us0,Us,N0,N) :-
    (   memberchk(Old-New,Vs0) % we are indeed not the first mention of this node,
    ->  Ds = [],
	Vs0 = Vs,
	Us = [New|Us0],
	N0 = N,
	NewNode = i(New)
    ;                         % we are now the first time to mention this node
	member(Old=s(Stuff,Ds0),Info),
	N1 is N0 + 1,
	Vs1 = [Old-N1|Vs0],
	NewNode = i(N1,Stuff),
	renumber_daughters(Ds0,Ds,Info,Vs1,Vs,Us0,Us,N1,N)
    ).

renumber_label(i(Old,Stuff),NewNode,Ds0,Ds,Info,Vs0,Vs,Us0,Us,N0,N) :-
    (   memberchk(Old-New,Vs0)	% we are now a second mention!
    ->  Ds = [],
	Vs0 = Vs,
	Us = [New|Us0],
	N0 = N,
	NewNode= i(New)
    ;                         % we are now the first time to mention this node
	N1 is N0 + 1,
	Vs1 = [Old-N1|Vs0],
	NewNode = i(N1,Stuff),
	renumber_daughters(Ds0,Ds,Info,Vs1,Vs,Us0,Us,N1,N)
    ).

dt_canonical_dt_indices_collect(tree(r(_,Label),_,Ds),V) :-
    dt_canonical_dt_indices_label(Label,Ds,V).
dt_canonical_dt_indices_collect(tree(r(_,Label),Ds),V) :-
    dt_canonical_dt_indices_label(Label,Ds,V).

dt_canonical_dt_indices_label(l(_,_,_),[],_).
dt_canonical_dt_indices_label(adt_lex(_,_,_,_,_),[],_).
dt_canonical_dt_indices_label(p(_),Ds,V) :-
    dt_canonical_dt_indices_ds(Ds,V).
dt_canonical_dt_indices_label(i(_),[],_V).

dt_canonical_dt_indices_label(i(Old,Stuff),Ds,V) :-
    member_or_add(Old=s(Stuff,Ds),V),
    dt_canonical_dt_indices_ds(Ds,V).
	
dt_canonical_dt_indices_ds([],_V).
dt_canonical_dt_indices_ds([H0|T0],V) :-
    dt_canonical_dt_indices_collect(H0,V),
    dt_canonical_dt_indices_ds(T0,V).

apply_dt_transformations(DT0,DT,String,Change) :-
    transform_dt(1,DT0,DT1,String),
    (   DT0 == DT1
    ->  DT0 = DT,
	Change =no
    ;   Change =yes,
	add_nodes_for_missing_tokens(DT1,DT2,normal,String,_),
	dt_canonical_dt(DT2,DT)
    ).

%% arg 1: Status is 0 or 1 to indicate whether transformations
%% are attempted to the output of a transformation.
transform_dt(1,tree(A0,B0,C0),tree(A,B,C),String) :-
    transformation(A0,B0,C0,A1,B1,C1,String,Status),
    !,
    transform_dt(Status,tree(A1,B1,C1),tree(A,B,C),String).
transform_dt(_,tree(A,B,C0),tree(A,B,C),String) :-
    transform_dt_list(C0,C,String).

transform_dt_list([],[],_String).
transform_dt_list([H0|T0],[H|T],String) :-
    transform_dt(1,H0,H,String),
    transform_dt_list(T0,T,String).

transformation(A0,B0,C0,A,B,C,String,Status) :-
    hook(alpino_user_transformation:user_transformation(A0,B0,C0,A,B,C,String,Status)),
    f(A0,B0,C0) \== f(A,B,C).

:- initialize_flag(add_nodes_for_mwu,on).

add_nodes_for_mwu(DT0,DT,Tokens) :-
    hdrug_flag(add_nodes_for_mwu,Bool),
    add_nodes_for_mwu(Bool,DT0,DT1,Tokens),
    flatten_punctuation(DT1,DT).

add_nodes_for_mwu(off,DT,DT,_).
add_nodes_for_mwu(on,DT0,DT,Tokens) :-
    (   add_nodes_for_mwu_(DT0,DT,Tokens)
    ->  true
    ;   format(user_error,
	       "ERROR in alpino_dt:add_nodes_for_mwu!~n",[]),
	DT0 = DT
    ).

add_nodes_for_mwu_(tree(Label0,Adm0,Ds0),tree(Label,Adm,Ds),Tokens) :-
    add_nodes_for_mwu_(Label0,Adm0,Ds0,Label,Adm,Ds,Tokens).

add_nodes_for_mwu_(r(Rel,Label0),Adm0,Ds0,r(Rel,Label),Adm,Ds,Tokens) :-
    add_nodes_for_mwu_(Label0,Adm0,Ds0,Label,Adm,Ds,Tokens).
add_nodes_for_mwu_(i(Index),Adm,[],i(Index),Adm,[],_Tokens).
add_nodes_for_mwu_(i(Index,Label0),Adm0,Ds0,i(Index,Label),Adm,Ds,Tokens) :-
    add_nodes_for_mwu_(Label0,Adm0,Ds0,Label,Adm,Ds,Tokens).
add_nodes_for_mwu_(p(Cat),Adm,Ds0,p(Cat),Adm,Ds,Tokens) :-
    add_nodes_for_mwu_list(Ds0,Ds,Tokens).
add_nodes_for_mwu_(l(Tag,Cat,Word/[P0,P]),Adm,[],l(Tag,Cat,Word/[P0,P]),Adm,[],_Tokens) :-
    integer(P0), integer(P),
    P is P0 + 1,
    !.
add_nodes_for_mwu_(l(Tag,Cat,Stam/[P0,P]),Adm,[],p(mwu(Stam,Sense)),Adm,NewDs,Tokens) :-
    integer(P0), integer(P),
    !,
    Length is P-P0,
    length(NewDs,Length),
    parse_stam(Stam,Stams),
    length(Stams,Length2),
    (   Length=:=Length2
    ->  mwu_daughters_stam(NewDs,Tag,Cat,P0,P,Stams)
    ;   mwu_daughters(NewDs,Tag,Cat,P0,P,Tokens)
    ),
    root_of_pair(Stam,Root),
    alpino_treebank:frame2sense(Root,Tag,Sense).
add_nodes_for_mwu_(l(Tag,Cat,Stam/[P0list,Plist]),Adm,[],p(mwu(Stam,Sense)),Adm,NewDs,Tokens) :-
    length(P0list,Length),
    length(NewDs,Length),
    parse_stam(Stam,Stams),
    length(Stams,Length2),
    (   Length=:=Length2
    ->  mwu_daughters_stam_list(NewDs,Tag,Cat,P0list,Plist,Stams)
    ;   mwu_daughters_list(NewDs,Tag,Cat,P0list,Plist,Tokens)
    ),
    root_of_pair(Stam,Root),
    alpino_treebank:frame2sense(Root,Tag,Sense).   %%%%% NB this fails if Stam is a list containing v_root(..) ....!!!!

mwu_daughters_stam([],_,_,P,P,[]).
mwu_daughters_stam([tree(r(mwp,l(Pos,Cat,Stem/[P0,P1])),_,[])|T],Pos,Cat,
                   P0,P,[Stem|Stems]) :-
    P1 is P0+1,
    mwu_daughters_stam(T,Pos,Cat,P1,P,Stems).

mwu_daughters([],_,_Cat,P,P,_Tokens).
mwu_daughters([tree(r(mwp,l(Pos,Cat,Root/[P0,P1])),_,[])|T],Pos,Cat,
              P0,P,Tokens) :-
    P1 is P0+1,
    nth(P1,Tokens,Root),
    mwu_daughters(T,Pos,Cat,P1,P,Tokens).

mwu_daughters_stam_list([],_,_,[],[],[]).
mwu_daughters_stam_list([tree(r(mwp,l(Pos,Cat,Stem/[P0,P1])),_,[])|T],Pos,Cat,
                   [P0|P0list],[P1|Plist],[Stem|Stems]) :-
    mwu_daughters_stam_list(T,Pos,Cat,P0list,Plist,Stems).

mwu_daughters_list([],_,_Cat,[],[],_Tokens).
mwu_daughters_list([tree(r(mwp,l(Pos,Cat,Root/[P0,P1])),_,[])|T],Pos,Cat,
              [P0|P0list],[P1|Plist],Tokens) :-
    nth(P1,Tokens,Root),
    mwu_daughters_list(T,Pos,Cat,P0list,Plist,Tokens).

add_nodes_for_mwu_list([],[],_Tokens).
add_nodes_for_mwu_list([H0|T0],[H|T],Tokens) :-
    add_nodes_for_mwu_(H0,H,Tokens),
    add_nodes_for_mwu_list(T0,T,Tokens).

add_nodes_for_missing_tokens(DT0,DT,Flag,Tokens,Frames) :-
    (   add_nodes_for_missing_tokens_(DT0,DT,Flag,Tokens,Frames)
    ->  true
    ;   format(user_error,
	       "ERROR in alpino_dt:add_nodes_for_missing_tokens!~n",[]),
	DT0 = DT
    ).

%% special case for empty dt, eg. for a single punctuation token

add_nodes_for_missing_tokens_(tree(r(top,p(_)),Ix,[]),
			      tree(r(top,p(top)),Ix,Ds),Flag,Tokens,Frames) :-
    !,
    select_all_positions(tree(r(top,p(_)),Ix,[]),Pos,_),
    find_missing_positions(Tokens,Pos,0,Ds,[],Flag,Frames).

add_nodes_for_missing_tokens_(DT0,DT,Flag,Tokens,Frames) :-
    DT0=tree(r(top,p(top)),Index,Ds0),
    member(tree(r('--',_),_,_),Ds0), !,  % already robust format
    DT=tree(r(top,p(top)),Index,Ds),
    select_all_positions(DT0,Pos,_),
    find_missing_positions(Tokens,Pos,0,Ds,Ds0,Flag,Frames).

add_nodes_for_missing_tokens_(DT0,DT,Flag,Tokens,Frames) :-
    DT0=tree(r(top,Node),Index,Ds0),
    DT=tree(r(top,p(top)),Index,[tree(r('--',Node),Index,Ds0)|Ds]),
    select_all_positions(DT0,Pos,_),
    find_missing_positions(Tokens,Pos,0,Ds,[],Flag,Frames).

combine_stam([],[],[]).
combine_stam([Root|Roots],[Lemma|Lemmas],[v_root(Root,Lemma)|Rest]) :-
    combine_stam(Roots,Lemmas,Rest).

parse_stam(v_root(Root,Stam),List) :-
    !,
    atom_codes(Root,RootCodes),
    split_string(RootCodes," ",ListCodes),
    list_codes(ListCodes,List1),
    atom_codes(Stam,StamCodes),
    split_string(StamCodes," ",ListCodes2),
    list_codes(ListCodes2,List2),
    (   combine_stam(List1,List2,List)
    ->  true
    ;   List1 = List
    ).

parse_stam(Stam,List) :-
    atom_codes(Stam,StamCodes),
    split_string(StamCodes," ",ListCodes),
    list_codes(ListCodes,List).

list_codes([],[]).
list_codes([H0|T0],[H|T]) :-
    atom_codes(H,H0),
    list_codes(T0,T).

find_missing_positions([],[],_,Ns,Ns,_,_).
find_missing_positions([H|T],Ps0,P,Ns0,Ns,Flag,Frames) :-
    (   P1 is P +1,
	Ps0 = [P|Ps1],
	T=T2
    ->  Ns0=Ns1
    ;   Ps1=Ps0,
	(   nonvar(Frames),
	    member(_-frame(_,_,P,P1,Hstem0,Tag0,_,_),Frames),
	    \+ Tag0 = punct(_),
	    \+ (   Tag0 = robust_skip,
		   alpino_lexical_analysis:tag(P,P1,_,_,_,_,_,punct(_))
	       )
	->  true
	;   P1 is P + 1
	),
	skip_positions(P,P1,T,T2),
	guess_tag(Tag0,Tag,Hstem0,H,Hstem,P,P1),
        Ns0=[tree(r('--',l(Tag,Cat,Hstem/[P,P1])),_,[])|Ns1],
        missing_node_message(Flag,Tag0,Cat,H)
    ),
    find_missing_positions(T2,Ps1,P1,Ns1,Ns,Flag,Frames).

guess_tag(Tag0,Tag,Hstem0,H,Hstem,P,P1) :-
    (   var(Tag0)
    ->  guess_tag(Tag,H,Hstem,P,P1)
    ;   Tag0 == skip
    ->  guess_tag(Tag,H,Hstem,P,P1)
    ;   Tag0 == robust_skip
    ->  guess_tag(Tag,H,Hstem,P,P1)
    ;   Tag0=Tag,
	Hstem0=Hstem
    ).

%% in case of time-out, this should really compute the best frame sequence
guess_tag(Tag,H,Hstem,_P,_) :-
    (   alpino_lexical_analysis:tag(_,_,_,_,Hstem,H,_,Tag0)
    ->  (    Tag0 = with_dt(Tag,_)
	->   true
	;    Tag0 = Tag
	)
    ;   alpino_lex:lex_initialize,
	hook(alpino_lex:lexicon___(H,Tag0,Hstem,[],[],normal))
    ->  (    Tag0 = with_dt(Tag,_)
	->   true
	;    Tag0 = Tag
	)
    ;   Hstem=H,
	Tag='--'
    ).
    
skip_positions(P0,P,List0,List) :-
    Dif is P-P0,
    skip_positions(Dif,List0,List).

skip_positions(I,List0,List) :-
    (   I-1 < 1
    ->  List0=List
    ;   List0=[_|List1],
	I2 is I-1,
	skip_positions(I2,List1,List)
    ).

missing_node_message(ignore,_,'--',_).
missing_node_message(normal,Tag0,Cat,W) :-
    (   Tag0 == skip
    ->  Cat = '--'
    ;   Tag0 == robust_skip
    ->  Cat = '--'
    ;   missing_node_message(Tag0,Cat,W)
    ).

%% part of derivation, but not a dt?
%% should only be possible for punctuation, give
%% warning for other tags
missing_node_message(punct(_),punct,_) :-
    !.
missing_node_message(Tag,'--',H) :-
    debug_message(1,"warning: missing dt node for ~w (~w)~n",[H,Tag]).

select_all_positions(DT,Pos,File) :-
    findall(P,select_a_position(DT,P),Pos0),
    length(Pos0,L0),
    sort(Pos0,Pos),
    length(Pos,L),
    (   L0 =\= L
    ->  find_duplicates(Pos0,Duplicates),
	format(user_error,"ERROR: the positions overlap: ~w ~w~n",[Duplicates,File])
    ;   true
    ).

find_duplicates(List0,Dups) :-
    add_vals(List0,List1),
    keysort(List1,List2),
    keys(List2,List),
    find_duplicates_(List,Dups).

keys([],[]).
keys([H-_|T],[H|NT]) :-
    keys(T,NT).

add_vals([],[]).
add_vals([H|NT],[H-_|T]) :-
    add_vals(NT,T).

find_duplicates_([],[]).
find_duplicates_([H|T],Dups) :-
    find_duplicates_(T,H,Dups).

find_duplicates_([],_,[]).
find_duplicates_([H|T],N,Dups) :-
    (   H == N
    ->  Dups = [N|Dups1]
    ;   Dups = Dups1
    ),
    find_duplicates_(T,H,Dups1).

%select_a_position(_,Pos) :-
%    get_phantoms(PosList),
%    member(Pos,PosList).

select_a_position(tree(Label,_,Ds),Pos) :-
    select_a_position_(Ds,Label,Pos).

select_a_position__(tree(Label,_,Ds),Pos) :-
    select_a_position_(Ds,Label,Pos).

select_a_position_([],r(_,Label),Pos) :-
    select_a_position_label(Label,Pos).
select_a_position_([H|T],_,Pos) :-
    member(Tree,[H|T]),
    select_a_position__(Tree,Pos).

select_a_position_label(i(_,Rest),Pos) :-
    select_a_position_label(Rest,Pos).
select_a_position_label(l(_,_,_/[P0,P]),Pos) :-
    select_a_position_pos(P0,P,Pos).

select_a_position_pos(P0,_,P0) :-
    integer(P0).
select_a_position_pos(P0,P,Pos) :-
    integer(P0),
    integer(P),
    P1 is P0 + 1,
    P1 < P,
    select_a_position_pos(P1,P,Pos).
select_a_position_pos([H|T],_,P0) :-
    member(P0,[H|T]).

dt_to_relations_with_attributes(DT,Triples,Atts) :-
    dt_to_relations_with_full_postags(DT,Triples0),
    relations_with_attributes(Triples0,Triples,Atts).

relations_with_attributes([],[],_).
relations_with_attributes([H0|T0],[H|T],Atts) :-
    relation_with_attributes(H0,H,Atts),
    relations_with_attributes(T0,T,Atts).

relation_with_attributes(deprel(Pa0:Ha,R,  Pb0:Hb),
                         deprel(Pa: Ha,Rel,Pb :Hb),Atts) :-
    is_verbal_complement(Pa0,R), !,
    somewhat_simplify_frame(Pa0,Pa1,Atts),
    somewhat_simplify_frame(Pb0,Pb1,Atts),
    incorporate_cluster(deprel(Pa1:Ha,R,Pb1:Hb),
                        deprel(Pa:Ha, Rel,Pb: Hb)).

relation_with_attributes(Deprel0,Deprel,_) :-
    somewhat_simplify_postags_of_relation(Deprel0,Deprel).

%incorporate_cluster(deprel(verb(Frame):Root/[P0,P],hd/Rel,N),
%                    deprel(verb(clust):Root/[P0,P],hd/No, N)) :-
%    frames_clusters:cluster(Root,Rel,Frame,No), !.
incorporate_cluster(Rel,Rel).

is_verbal_complement(Frame,Rel) :-
    functor(Frame,verb,3),
    Rel = hd/Comp,
    member(Comp,[su,obj1,obj2]).

dt_to_relations_with_somewhat_simplified_postags(DT,Triples) :-
    dt_to_relations_with_full_postags(DT,Triples0),
    somewhat_simplify_postags_of_relations(Triples0,Triples).

somewhat_simplify_postags_of_relations([],[]).
somewhat_simplify_postags_of_relations([H0|T0],[H|T]) :-
    somewhat_simplify_postags_of_relation(H0,H),
    somewhat_simplify_postags_of_relations(T0,T).

somewhat_simplify_postags_of_relation(deprel(Pa0:Ha,R,Pb0:Hb),
				      deprel(Pa: Ha,R,Pb :Hb)) :-
    somewhat_simplify_frame(Pa0,Pa,[numtype,special,wh,refl,neclass]),
    somewhat_simplify_frame(Pb0,Pb,[numtype,special,wh,refl,neclass]).

somewhat_simplify_frame(with_dt(A,_B),Frame) :-
    !,
    somewhat_simplify_frame(A,Frame).
somewhat_simplify_frame(Frame0,Frame) :-
    somewhat_simplify_frame(Frame0,Frame,[numtype,sc,infl,special,wh,refl,neclass]).

somewhat_simplify_frame(Frame0,Frame,RelFeats) :-
    alpino_postags:postag_of_frame(Frame0,PosTag,List0),
    findall(F,lexical_feature(List0,F,RelFeats),List),
    Frame =.. [PosTag|List].

lexical_feature(List,Val,RelFeats) :-
    member(Att,RelFeats),
    member(Att=Val0,List),
    lexical_feature_val(Att,Val0,Val).
 
lexical_feature_val(sc,Term,Sc) :-
    !,
    lexical_feature_val_(Term,Sc).
lexical_feature_val(_,Val,Val).

lexical_feature_val_(ninv(_,Sc0),Sc) :-
    !,
    Sc0=Sc.
lexical_feature_val_(Sc,Sc).

converse_phantom(Tree0,Tree) :-
    get_phantoms(Ps),
    converse_phantom_ps(Ps,Tree0,Tree).

converse_phantom(Tree0,P,Tree) :-
    converse_phantom(Tree0,Tree1,P,Is,[]),
    !,
    remove_removed_is(Tree1,Tree,Is).
%% apparantly, the word is not used in the dep. structure (e.g. skipped)
converse_phantom(Tree,_,Tree).

converse_phantom_ps([],Tree,Tree).
converse_phantom_ps([P|Ps],Tree0,Tree) :-
    converse_phantom(Tree0,P,Tree1),
    converse_phantom_ps(Ps,Tree1,Tree).

converse_phantom(tree(Rel/Node,IX,Ds0),tree(Rel/Node,IX,Ds),P,Is0,Is):-
    converse_phantom_ds(Ds0,Ds,P,Is0,Is).

remove_removed_is(tree(Node,Ix,Ds0), tree(Node,Ix,Ds), Is) :-
    remove_removed_is_ds(Ds0,Ds,Is).

remove_removed_is_ds(Ds0,Ds,Is) :-
    lists:select(D,Ds0,Ds1),
    D = tree(_,NONVAR,_),
    nonvar(NONVAR),
    NONVAR = i(I),
    nonvar(I),
    lists:member(I,Is),
    !,
    remove_removed_is_ds(Ds1,Ds,Is).
remove_removed_is_ds(Ds0,Ds,Is) :-
    remove_removed_is_ds_ds(Ds0,Ds,Is).

remove_removed_is_ds_ds([],[],_).
remove_removed_is_ds_ds([H0|T0],[H|T],Is) :-
    remove_removed_is(H0,H,Is),
    remove_removed_is_ds_ds(T0,T,Is).

converse_phantom_ds(Ds0,Ds,P0,[I|Is0],Is) :-
    select(D,Ds0,Ds1),
    starts_with(D,P0,I),
    !,
    Is0 = Is,
    Ds1 = Ds.
converse_phantom_ds(Ds0,Ds,P0,Is0,Is) :-
    select(D,Ds0,Ds1),
    is_part(D,D1,P0),
    !,
    Ds = [D1|Ds1],
    Is0 = Is.
converse_phantom_ds([H0|T],[H|T],P,Is0,Is) :-
    converse_phantom(H0,H,P,Is0,Is).
converse_phantom_ds([H|T0],[H|T],P,Is0,Is) :-
    converse_phantom_ds(T0,T,P,Is0,Is).

starts_with(tree(_Rel/(_Pos:_Word/[Pos,_]),i(Ix,_),[]),Pos,Ix).
starts_with(tree(_,_,[Daughter]),Pos,Ix) :-
    starts_with(Daughter,Pos,Ix).

is_part(tree(Rel/(Tag:Word/[Pos0,Pos]),Ix,[]),Tree,Pos1) :-
    Pos0 < Pos1,
    Pos1 < Pos,
    !,
    Pos2 is Pos - 1,
    Tree = tree(Rel/(Tag:Word/[Pos0,Pos2]),Ix,[]).

get_phantoms(Ps) :-
    try_hook(alpino_lexical_analysis:user_skips(S),S=[]),
    findall(P,member_phantom_skip(P,S),Ps0),
    sort(Ps0,Ps).

member_phantom_skip(Q,S) :-
    member(phantom_skip(P),S),
    alpino_lexical_analysis:rpos(P,Q).

renumber_positions(tree(Node0,Ix,Ds0),tree(Node,Ix,Ds),Ps) :-
    renumber_positions_node(Node0,Node,Ps),
    renumber_positions_ds(Ds0,Ds,Ps).

renumber_positions_ds([],[],_).
renumber_positions_ds([H0|T0],[H|T],Ps) :-
    renumber_positions(H0,H,Ps),
    renumber_positions_ds(T0,T,Ps).

renumber_positions_node(r(Rel,L0),r(Rel,L),Ps) :-
    renumber_positions_node(L0,L,Ps).
renumber_positions_node(i(Rel,L0),i(Rel,L),Ps) :-
    renumber_positions_node(L0,L,Ps).
renumber_positions_node(l(PosTerm,Cat,Word0),l(PosTerm,Cat,Word),Ps) :-
    renumber_positions_node(Word0,Word,Ps).
renumber_positions_node(i(Index),i(Index),_Ps).
renumber_positions_node(p(Index),p(Index),_Ps).
renumber_positions_node(Word/[P0,P],Word/[Q0,Q],Ps) :-
    (   nonvar(P0), nonvar(P)
    ->  renumber_positions_pos(P0,P,Q0,Q,Ps)
    ;   format(user_error,"ERROR: variables in renumber position~n",[]),
	fail
    ).


renumber_positions_pos_list([],[],[],[],_).
renumber_positions_pos_list([H0|T0],[H1|T1],[H2|T2],[H|T],Ps) :-
    renumber_positions_pos(H0,H1,H2,H,Ps),
    renumber_positions_pos_list(T0,T1,T2,T,Ps).

renumber_positions_pos([H0|T0],[H1|T1],Q0,Q,Ps) :-
    renumber_positions_pos_list([H0|T0],[H1|T1],Q0,Q,Ps).

renumber_positions_pos(P0,P,Q0,Q,Ps):-
    integer(P0), integer(P),
    count_smaller_ps(Ps,P0,0,Count),
    Q0 is P0 - Count,
    Q  is P - Count.

count_smaller_ps([],_,C,C).
count_smaller_ps([H|T],P,C0,C) :-
    (   H < P
    ->  C1 is C0 + 1,
        count_smaller_ps(T,P,C1,C)
    ;   C0 = C
    ).

check_variable_cat(tree(r(_,Node),_,Ds)) :-
    check_variable_cat_node(Node),
    check_variable_cat_ds(Ds).

check_variable_cat_ds([]).
check_variable_cat_ds([H|T]):-
    check_variable_cat(H),
    check_variable_cat_ds(T).

check_variable_cat_node(l(A,Cat,B)) :-
    (   Cat = '$VAR'('_')
    ->  debug_message(1,"warning: variable cat in dt (~w ~w)~n",[A,B])
    ;   true
    ).
check_variable_cat_node(p(Cat)) :-
    (   Cat = '$VAR'('_')
    ->  debug_message(1,"warning: variable cat in dt~n",[])
    ;   true
    ).
check_variable_cat_node(i(_)).
check_variable_cat_node(i(_,Node)) :-
    check_variable_cat_node(Node).

% check_dt_variable(Rel) :-
%     hdrug_flag(check_dt_variable,Var),
%     (   Var == off
%     ->  true
%     ;   Rel == su
%     ->  true
%     ;   Rel == sup
%     ->  true
%     ;   debug_message(1,"variable in dt for attribute ~w~n",[Rel])
%     ).

maybe_undo_mwp(off,DT,DT).
maybe_undo_mwp(on,DT0,DT) :-
    undo_mwp(DT0,DT).

undo_mwp(tree(Label0,Adm0,Ds0), tree(Label,Adm,Ds)) :-
    undo_mwp(Label0,Adm0,Ds0,Label,Adm,Ds).

undo_mwp(r(Rel,Label0),Adm0,Ds0,r(Rel,Label),Adm,Ds) :-
    undo_mwp(Label0,Adm0,Ds0,Label,Adm,Ds).
undo_mwp(i(Index),Adm,[],i(Index),Adm,[]).
undo_mwp(i(Index,Label0),Adm0,Ds0,i(Index,Label),Adm,Ds) :-
    undo_mwp(Label0,Adm0,Ds0,Label,Adm,Ds).
undo_mwp(p(Cat),Adm0,Ds0,Label,Adm,Ds) :-
    undo_mwp_cat(Cat,Adm0,Ds0,Label,Adm,Ds),!.
undo_mwp(l(Tag,Cat,Word),Adm,[],l(Tag,Cat,Word),Adm,[]).

%% try mwu, but use ordinary rule as fall-back option for cases
%% of non-contiguous mwu...
%% new style mwu:
undo_mwp_cat(mwu(Root,_),Adm,Ds0,l(Tag,Cat,Root/[P0,P]),Adm,[]) :-
    extract_mwu(Ds0,P0,P,Tag,Cat,_Words).
%% old style mwu:
undo_mwp_cat(mwu,Adm,Ds0,l(Tag,Cat,Root/[P0,P]),Adm,[]) :-
    extract_mwu(Ds0,P0,P,Tag,Cat,Words),
    concat_all(Words,Root,' ').
undo_mwp_cat(Cat,Adm,Ds0,p(Cat),Adm,Ds) :-
    undo_mwp_ds(Ds0,Ds).

undo_mwp_ds([],[]).
undo_mwp_ds([H0|T0],[H|T]) :-
    undo_mwp(H0,H),
    undo_mwp_ds(T0,T).

extract_mwu([tree(r(mwp,l(Pos,Cat,Word/[P0,P1])),_,[])|T],P0,P,Pos,Cat,[Word|Words]) :-
    extract_mwu_(T,P1,P,Words).

extract_mwu_([],P,P,[]).
extract_mwu_([tree(r(mwp,l(_,_,Word/[P0,P1])),_,[])|T],P0,P,[Word|Words]) :-
    extract_mwu_(T,P1,P,Words).


flatten_punctuation(tree(r(top,p(top)),Ix,Ds0),
		    tree(r(top,p(top)),Ix,Ds)):-
    flatten_punctuation_ds(Ds0,Ds).

flatten_punctuation_ds([],[]).
flatten_punctuation_ds([H|T],Result) :-
    flatten_punct(H,Result,Result1),
    flatten_punctuation_ds(T,Result1).

flatten_punct(tree(r('--',p(mwu(P1,_))),_,Ds0),Ds,Tail) :-
    flatten_punct_cand(P1),
    !,
    replace_rel(Ds0,Ds,Tail).
flatten_punct(Tree,[Tree|Result],Result).

flatten_punct_cand('?').
flatten_punct_cand('!').
flatten_punct_cand('! ?').
flatten_punct_cand('? !').

replace_rel([],Tail,Tail).
replace_rel([tree(r(mwp,Cat),Ix,[])|T0],
	    [tree(r('--',Cat),Ix,[])|T1],Result) :-
    replace_rel(T0,T1,Result).


order_somewhat(tree(r(Rel,Cat),I,Ds0), tree(r(Rel,Cat),I,Ds)):-
    order_somewhat(Cat,Ds0,Ds1),
    order_ds(Ds1,Ds).

order_somewhat(p(du),Ds0,Ds):-
    !,
    order_real_ds(Ds0,Ds).
order_somewhat(p(conj),Ds0,Ds):-
    !,
    order_real_ds(Ds0,Ds).
order_somewhat(_,Ds,Ds).

order_ds([],[]).
order_ds([H0|T0],[H|T]):-
    order_somewhat(H0,H),
    order_ds(T0,T).

order_real_ds(Ds0,Ds):-
    simple_pos(Ds0,Ds1),
    keysort(Ds1,Ds2),
    vals(Ds2,Ds).

simple_pos([],[]).
simple_pos([H|T0],[Key-H|T]):-
    simple_pos1(H,Key), !,
    simple_pos(T0,T).

simple_pos1(tree(r(_,Cat),_,Ds),Pos):-
    simple_pos1_cat(Cat,Ds,Pos).

simple_pos1_cat(l(_,_,_/[Pos,_]),_,Pos).
simple_pos1_cat(i(_,Cat),Ds,Pos):-
    simple_pos1_cat(Cat,Ds,Pos).
simple_pos1_cat(p(_),Ds,Pos):-
    lists:member(D,Ds),
    simple_pos1(D,Pos).
