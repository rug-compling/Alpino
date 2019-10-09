:- module(alpino_geneval, [rouge_n/4,
			   rouge_l/3,
			   rouge_l/4,
			   rouge_s/3,
			   rouge_s/4,
			   rouge_s/5,
			   rouge_su/3,
			   rouge_su/4,
			   gtm/3,
			   gtm_best_alignment/3,
			   gtm_mr/3
			  ]).

:- use_module(library(lists)).

% Evaluation measures based on ROUGE. Note that we use a limited version
% of the ROUGE measures: there is only one reference (gold standard) string.
% ROUGE is described in the following paper:
%
% ROUGE: A Package for Automatic Evaluation of Summaries, Chin-Yew Lin, 2004

fscore(P,R,Beta,Fscore) :-
    (   (  P is 0 ; P is 0.0 )
    ->  Fscore = 0.0
    ;   (  Beta is 0; Beta is 0.0 )
    ->  P = Fscore
    ;   BetaSquare is Beta ** 2,
	Fscore is ((1 + BetaSquare) * P * R) / (BetaSquare * P + R)
    ).

%%%%%%%%%%%
% ROUGE-N %
%%%%%%%%%%%

rouge_n(Ref,Sent,N,Score) :-
    ngrams(Ref,RefNgrams,N),
    ngrams(Sent,SentNgrams,N),
    list_intersect(RefNgrams,SentNgrams,Is),
    length(RefNgrams,RefLen),
    length(Is,IsLen),
    Score is IsLen / RefLen.

ngrams([H|T],[L|Ngrams],N) :-
    length(L,N),
    prefix(L,[H|T]),
    !,
    ngrams(T,Ngrams,N).
ngrams(_,[],_).

list_intersect(L1,L2,Is) :-
    list_intersect(L1,L2,Is,[]).

list_intersect([],_,Is,Is).
list_intersect([H|T],L,Is,Is0) :-
    lists:select(H,L,L1),
    !,
    list_intersect(T,L1,Is,[H|Is0]).
list_intersect([_|T],L,Is,Is0) :-
    list_intersect(T,L,Is,Is0).

%%%%%%%%%%%
% ROUGE-L %
%%%%%%%%%%%

rouge_l(Ref,Sent,FScore) :-
    rouge_l(Ref,Sent,1,FScore).
rouge_l(Ref,Sent,Beta,FScore) :-
    lcs(Ref,Sent,Lcs),
    length(Ref,RefLen),
    length(Sent,SentLen),
    length(Lcs,LcsLen),
    R is LcsLen / RefLen,
    P is LcsLen / SentLen,
    fscore(P,R,Beta,FScore).    

% Longest common substring with memoization. For a description of the
% algorithm see:
%
% http://en.wikipedia.org/wiki/Longest_common_subsequence#LCS_function_defined
%
% Note: I have also found an implementation on the Wikipedia Prolog page,
% it seems to be the same except for some minor details.

:- dynamic lcs_memo/3.

:- thread_local lcs_memo/3.

lcs(Seq1,Seq2,Lcs) :-
    lcs_aux(Seq1,Seq2,Lcs),
    retractall(lcs_memo(_,_,_)).

lcs_aux([],_,[]) :- !.
lcs_aux(_,[],[]) :- !.
lcs_aux([H|T1],[H|T2],[H|Lcs]) :-
    !,
    lcs_aux_m(T1,T2,Lcs).
lcs_aux([H1|T1],[H2|T2],Lcs) :-
    lcs_aux_m([H1|T1],T2,Lcs1),
    lcs_aux_m(T1,[H2|T2],Lcs2),
    length(Lcs1,Len1),
    length(Lcs2,Len2),
    (   Len1 > Len2
    ->  Lcs = Lcs1
    ;   Lcs = Lcs2
    ).

lcs_aux_m(Seq1,Seq2,Lcs) :-
    (   lcs_memo(Seq1,Seq2,Lcs)
    ->  true
    ;   lcs_aux(Seq1,Seq2,Lcs),
	assertz(lcs_memo(Seq1,Seq2,Lcs))
    ).

%%%%%%%%%%%
% ROUGE-S %
%%%%%%%%%%%

% ROUGE-S normally uses skip-bigrams, but while we are at it,
% let's make it general, and allow any N.
rouge_s(Ref,Sent,FScore) :-
    rouge_s(Ref,Sent,2,1,FScore).

rouge_s(Ref,Sent,N,FScore) :-
    rouge_s(Ref,Sent,N,1,FScore).

rouge_s(Ref,Sent,N,Beta,FScore) :-
    (   (length(Ref,1) ; length(Sent,1))
    ->  FScore = 0.0
    ;   rouge_s_(Ref,Sent,N,Beta,FScore)
    ).

rouge_s_(Ref,Sent,N,Beta,FScore) :-
    skip_ngrams(Ref,N,RefSkips),
    skip_ngrams(Sent,N,SentSkips),
    list_intersect(RefSkips,SentSkips,Is),
    length(Is,IsLen),
    length(RefSkips,RSLen),
    length(SentSkips,SSLen),
    P is IsLen / SSLen,
    R is IsLen / RSLen,
    fscore(P,R,Beta,FScore).

skip_ngrams(Seq,N,SkipNgrams) :-
    length(L,N),
    findall(L,sublist(L,Seq),SkipNgrams).

%%%%%%%%%%%%
% ROUGE-SU %
%%%%%%%%%%%%

% ROUGE-SU is a variant of ROUGE-S with skip-bigrams that also
% takes unigrams into account. We can cheaply implement this by
% prefixing both sequences by a marker. Since this marker will
% create a skip-bigram with all other words within the sequence,
% we are also counting unigram overlap.
rouge_su(Ref,Sent,FScore) :-
    rouge_su(Ref,Sent,1,FScore).

rouge_su(Ref,Sent,Beta,FScore) :-
    rouge_s(['#'|Ref],['#'|Sent],2,Beta,FScore).

/*
%               /n\      n!
% Combinations: | | = --------
%               \k/   k!(n-k)!
%
% Currently unused.
combination(N,K,_) :-
    K > N,
    !,
    fail.
combination(N,K,Result) :-
    combination_aux(N,K,Result0,1),
    Result is round(Result0).

combination_aux(_,0,N,N) :- !.
combination_aux(N,K,Result,Result0) :-
    Result1 is Result0 * (N / K),
    NNew is N - 1,
    KNew is K - 1,
    combination_aux(NNew,KNew,Result,Result1).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% General Text Matching (GTM) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% See: Melamed et al., 2003
%%
%% Note: while the authors discuss maximum matchings, their implementation
%% does not search the maximum matchings. Instead, they find the runs, order
%% them by length, and then select runs that do not conflict. For now we
%% follow authors, since in practice this seems ok if the run lengths are
%% squared. But if a smaller exponent is used, the implementation may not
%% calculate the maximum score. Given enough time it may be interesting
%% to search the maximum matches (e.g. through the Hopcroft-Karp algorithm).
%% Interesting literature:
%%
%% Algorithms for Enumerating All Perfect, Maximum and Maximal Matchings
%% in Bipartite Graphs, Takeaki Uno, 1997

%% GTM for multiple refs.
gtm_mr(Refs,Cand,Fscore) :-
    %% Step 2: find maximum matchings with a barrier.
    gtm_ref_runs(Refs,Cand,Runs0,[]),
    id_pairs(Runs0,Runs1),
    keysort(Runs1,Runs2),
    remove_keys(Runs2,Runs),
    length(Cand,CandLen),
    
    %% Step 3: normalize runs of maximum matchings with repsect
    %% to the lengths of the input texts.
    map(Refs,length,RefLengths),
    average(RefLengths,RefAvg),
    TruncRefAvg is RefAvg // 1,
    min(TruncRefAvg,CandLen,MinLen),
    remove_runs(MinLen,Runs,TruncRuns),

    %% Calaculate GTM score.
    sum_squares(TruncRuns,0,Sum),
    Score is sqrt(Sum),
    Prec is Score / CandLen,
    Rec is Score / TruncRefAvg,
    fscore(Prec,Rec,1.0,Fscore).

%% Remove hits, so that we keep only N hits. If the input list was
%% sorted, there is a preference for removing short runs.
remove_runs(0,Runs,Runs) :-
    !.
remove_runs(L,[H|Runs],TruncRuns) :-
    min(H,L,Min),
    H1 is H - Min,
    L1 is L - Min,
    (   H1 == 0
    ->  remove_runs(L1,Runs,TruncRuns)
    ;   remove_runs(L1,[H1|Runs],TruncRuns)
    ).

%% Hit runs for a list of references, and a candidate.
gtm_ref_runs([],_,RunsSizes,RunsSizes).
gtm_ref_runs([Ref|T],Cand,RunsSizes,RunsSizes0) :-
    gtm_runs(Ref,Cand,Runs),
    map(Runs,length,RunsSizes,RunsSizes1),
    gtm_ref_runs(T,Cand,RunsSizes1,RunsSizes0).

%% Runs for a reference and a candidate.
gtm_runs(Ref,Cand,Runs) :-
    number_items(Ref,RefNum,0),
    number_items(Cand,CandNum,0),
    findall(A,aligned_items(RefNum,CandNum,A),Aligned),
    runs(Aligned,Runs0),
    keysort(Runs0,Runs1),
    reverse(Runs1,Runs2),
    remove_keys(Runs2,Runs3),
    remove_conflicting_runs(Runs3,Runs).

%% GTM with a single reference.
gtm(Ref,Cand,Fscore) :-
    gtm_runs(Ref,Cand,Runs),
    map(Runs,length,RunSizes),
    sum_squares(RunSizes,0,Sum),
    Score is sqrt(Sum),
    length(Ref,RefLen),
    length(Cand,CandLen),
    Prec is Score / CandLen,
    Rec is Score / RefLen,
    fscore(Prec,Rec,1.0,Fscore).

gtm_best_alignment(Ref,Cand,Alignment) :-
    gtm_runs(Ref,Cand,Runs),
    flatten(Runs,Alignment,[]).

flatten([],Al,Al).
flatten([H|T],Al0,Al) :-
    lists:append(H,Al1,Al0),
    flatten(T,Al1,Al).

number_items([],[],_).
number_items([H|T],[i(Count,H)|NewT],Count) :-
    NewCount is Count + 1,
    number_items(T,NewT,NewCount).

aligned_items(Items1,Items2,m(I,J)) :-
    member(i(I,Item),Items1),
    member(i(J,Item),Items2).

runs(Matches,Runs) :-
    findall(Run,run(Matches,Run),Runs).

run([H|T],Run) :-
    run(T,[H],Run).

run([_|T],Run) :-
    run(T,Run).
    
run(Matches,[m(I,J)|Run0],Run) :-
    X is I + 1,
    Y is J + 1,
    memberchk(m(X,Y),Matches),
    run(Matches,[m(X,Y),m(I,J)|Run0],Run).
run(_,Run0,L-Run) :-
    reverse(Run0,Run),
    length(Run,L).


%%% this implements the "greedy approximation" as suggested
%%% in Melamed 2003.
remove_conflicting_runs(Runs0,Runs) :-
    remove_conflicting_aux(Runs0,[],Runs,[],[]).

remove_conflicting_aux([],Runs,Runs,_,_).
remove_conflicting_aux([H|T],Runs0,Runs,RowCov,ColCov) :-
    (  run_no_conflicts(H,RowCov,ColCov)
    -> Runs1 = [H|Runs0],
       add_coverage(H,RowCov,NewRowCov,ColCov,NewColCov)
    ;  Runs1 = Runs0,
       NewRowCov = RowCov,
       NewColCov = ColCov
    ),
    remove_conflicting_aux(T,Runs1,Runs,NewRowCov,NewColCov).

run_no_conflicts([],_,_).
run_no_conflicts([m(I,J)|T],RowCov,ColCov) :-
    \+ memberchk(I,RowCov),
    \+ memberchk(J,ColCov),
    run_no_conflicts(T,RowCov,ColCov).

add_coverage([],RowCov,RowCov,ColCov,ColCov).
add_coverage([m(I,J)|T],RowCov0,RowCov,ColCov0,ColCov) :-
    add_coverage(T,[I|RowCov0],RowCov,[J|ColCov0],ColCov).
    
sum_squares([],Sum,Sum).
sum_squares([H|T],Sum0,Sum) :-
    NewSum is Sum0 + (H ** 2),
    sum_squares(T,NewSum,Sum).

%% Pairs with identical elements (for keysorting).
id_pairs([],[]).
id_pairs([H|T],[H-H|NewT]) :-
    id_pairs(T,NewT).

%% Remove keys (first element of a pair).
remove_keys([],[]).
remove_keys([_-V|Tail],[V|NewTail]) :-
    remove_keys(Tail,NewTail).

average(L,Avg) :-
    sum_list(L,Sum),
    length(L,Len),
    Avg is Sum / Len.

min(X,Y,X) :-
    X =< Y.
min(X,Y,Y) :-
    Y < X.

map([],_,[]).
map([H|T],Pred,[NewH|NewT]) :-
    Goal =.. [Pred,H,NewH],
    call(Goal),
    map(T,Pred,NewT).

map([],_,Map,Map).
map([H|T],Pred,[NewH|Map],Map0) :-
    Goal =.. [Pred,H,NewH],
    call(Goal),
    map(T,Pred,Map,Map0).
