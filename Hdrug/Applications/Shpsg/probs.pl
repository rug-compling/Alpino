%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module(lists,   library(lists),   all).
:- use_module(ordsets, library(ordsets), all).
:- use_module(system,  library(system),  all).
:- use_module(charsio, library(charsio), all).

:- dynamic user:prob/3.
:- multifile user:prob/3.

lhs(Id,Lhs) :-
    user:rule(r(Id,Cat,_), _),
    functor(Cat,Lhs,_).
lhs(Id,Lhs) :-
    user:lexicon(_,r(Id,Cat,_)),
    functor(Cat,Lhs,_).

%% Compute the probability of a feature structure

fs_prob(Model,Cat,Ids,P) :-
    feats(maxent,Cat,Ids),
    ( probs(Model,Ids,Ps)
    ->
	combine(Model,Ps,P)
    ;
	P is 1
    ).

combine(scfg,List,Result) :-
    combine(scfg,List,1,Result).
combine(scfg,[],P,P).
combine(scfg,[L|Ls],P0,P) :-
    P1 is P0 * L,
    combine(Ls,P1,P).

combine(maxent,List,Result) :-
    sum_list(List,Result).

%% Look up probabilities in database

probs(_,[],[]).
probs(Model,[Id|Ids],[P|Ps]) :-
    (
	user:prob(Id,Model,P), !
    ;
	P = 0
    ),
	probs(Model,Ids,Ps).

%% Collect the names of all the properties active for a feature structure

feats(Model,Cat,Ids) :-
    feats(Model,Cat,Ids,IdsRest),
    IdsRest = [].

feats(Model,Cat,Ids,IdsLast) :-
    findall(F,user:template(Cat,F),Ids,IdsRest),
    get_dtrs(Cat,Dtrs),
    feats0(Model,Dtrs,IdsRest,IdsLast).

feats0(_,[],X,X).
feats0(Model,[R|Rs],Ids,IdsT) :-
    feats0(Model,Rs,Ids0,IdsT),
    feats(Model,R,Ids,Ids0).

%% Train SCFG from treebank

train :-
    retractall(user:prob),
    hdrug_util:hdrug_flag(model,Model),
    train(Model),
    format("Trained ~q.~n",[Model]).

accum(X,V,[(X,V1)|Xs],[(X,V2)|Xs]) :-
    V2 is V1 + V.
accum(X,V,[],[(X,V)]).
accum(X,V,[(X1,V1)|Xs],[(X1,V1)|Ys]) :-
    X \= X1,
    accum(X,V,Xs,Ys).

count_rules(Rules,Cats) :-
    findall(treebank(F,T),user:treebank(F,T),Ts),
    count_tree_rules(Ts,[],Rules,[],Cats).

count_tree_rules([],Rules,Rules,Cats,Cats).
count_tree_rules([treebank(Freq,T)|Ts],R0,R,C0,C) :-
    feats(T,scfg,Ids),
    count_feats(Ids,Freq,R0,R1),
    count_lhs(Ids,Freq,C0,C1),
    count_tree_rules(Ts,R1,R,C1,C).

count_feats([],_,R,R).
count_feats([Id|Ids],Freq,R0,R) :-
    accum(Id,Freq,R0,R1),
    count_feats(Ids,Freq,R1,R).

count_lhs([],_,C,C).
count_lhs([Id|Ids],Freq,C0,C) :-
    lhs(Id,Lhs),
    accum(Lhs,Freq,C0,C1),
    count_lhs(Ids,Freq,C1,C).

train(scfg) :-
    count_rules(Rules,Cats),
    train_scfg(Rules,Cats).
train_scfg([],_).
train_scfg([(Id,F)|Rs],Cats) :-
    lhs(Id,Lhs),
    member((Lhs,Z),Cats),
    P is F / Z,
    assert(user:prob(Id,scfg,P)),
    train_scfg(Rs,Cats).

%% Train random field from treebank

tree_feats(Set) :-
  user:sentence(Sent,train,_),
  user:treebank(Sent,_,T),
  feats(maxent,T,F),
  list_to_ord_set(F,Set).

find_features(Feats,NTrees,NFeats) :-
    format("Collecting properties~n",[]),
    findall(S,tree_feats(S),Ss),
    length(Ss,NTrees),
    ord_union(Ss,Set),
    number_feats(Set,0,Feats),
    length(Feats,NFeats),
    format("Found ~d properties~n",[NFeats]).

number_feats([],_,[]).
number_feats([F|Fs],N0,[(F,N0)|Ps]) :-
    N is N0 + 1,
    number_feats(Fs,N,Ps).

tmp(File) :-
    hdrug_latex:dir(Tmp),
    hdrug_util:concat(Tmp,'/iisXXXXXX',Base0),
    mktemp(Base0,File).

run_iis(Events,Params) :-
    tmp(Temp),
    hdrug_util:hdrug_flag(iis_iterations,Iters),
    format_to_chars('./do_iis ~w ~w ~w ~d',[Events,Params,Temp,Iters],Chars),
    name(Cmd,Chars),
    shell(Cmd).

iis(Feats,_NTrees,NFeats,Params) :-
    format("Writing properties~n",[]),
    tmp(Events),
    tmp(Params),
    open(Events,write,EStream),
    format(EStream,"&header~nnfeats=~d~n/~n",[NFeats]),
    events(EStream,Feats),
    close(EStream),
    run_iis(Events,Params).

events(EStream,Feats) :-
    ( user:sentence(Sent,train,_),
      findall((Score,Tree),user:treebank(Sent,Score,Tree),Trees),
      length(Trees,Count),
      format(EStream,"~d~n",[Count]),
      tree_to_events(EStream,Feats,Trees),
      fail
    ;
      true
    ).

tree_to_events(_,_,[]).
tree_to_events(Stream,Feats,[(Score,Tree)|Ts]) :-
  feats(maxent,Tree,Ids),
  count_feats(Ids,1,[],Fs),
  length(Fs,N),
  format(Stream,"~d ~d ",[Score,N]),
  write_feats(Stream,Feats,Fs),
  format(Stream,"~n",[]),
  tree_to_events(Stream,Feats,Ts).


write_feats(_,_,[]).
write_feats(EStream,Feats,[(F,C)|Fs]) :-
    member((F,N),Feats),
    format(EStream,"~d ~d ",[N,C]),
    write_feats(EStream,Feats,Fs).

get_params(Params,Feats) :-
    open(Params,read,PStream),
    read_params(PStream,Feats),
    close(PStream),
    delete_file(Params,[ignore]).

read_params(PStream,_) :-
    at_end_of_stream(PStream),
    !,
    true.
read_params(PStream,Feats) :-
    read(PStream,weight(C,W)),
    member((F,C),Feats),
    assert(user:prob(F,maxent,W)),
    read_params(PStream,Feats).

train(maxent) :-
    find_features(Feats,NTrees,NFeats),
    iis(Feats,NTrees,NFeats,Params),
    get_params(Params,Feats).

%% Evaluate models

best_trees(Trees,BestTrees) :-
    get_scores(Trees,Scores),
    lists:max_list(Scores,BestScore),
    get_trees(Trees,BestScore,BestTrees).

get_scores([],[]).
get_scores([treebank(_,S,_)|Ts],[S|Ss]) :-
    get_scores(Ts,Ss).

get_trees([],_,[]).
get_trees([treebank(_,Score,T)|Ss],Score,[T|Ts]) :-
    !,
    get_trees(Ss,Score,Ts).
get_trees([_|Ss],Score,Ts):-
    get_trees(Ss,Score,Ts).

check_match(N,Parse) :-
    best_score(N,BestScore),
    treebank(N,BestScore,BestParse),
    variant(Parse,BestParse).

test :-
    findall((Sent,String),sentence(Sent,test,String),Sents),
    eval_sents(Sents,0,Match),
    lists:length(Sents,N),
    Score is Match / N, 	
    format("Result = ~q~n",[Score]).

eval_sents([],M,M).
eval_sents([S|Ss],M0,M) :-
  eval_sent(S,M0,M1),
  eval_sents(Ss,M1,M).

eval_sent((Sent,String),M0,M) :-
  findall(treebank(Sent,Score,Tree),treebank(Sent,Score,Tree),Treebank),
  best_trees(Treebank,TreebankBest),
  score_trees(Treebank,System),
  best_trees(System,SystemBest),
  overlap(TreebankBest,SystemBest,0,Overlap),	
  lists:length(SystemBest,L),	
  Score is Overlap / L * 100.0,
  format("Sentence ~q : ~q : ~q~n",[Sent,Score,String]),
  M is M0 + Score.

overlap([],_,Ov,Ov).
overlap([T|Ts],Trees,Ov0,Ov) :-
  member(T,Trees),
  !,
  Ov1 is Ov0 + 1,
  overlap(Ts,Trees,Ov1,Ov).
overlap([_|Ts],Trees,Ov0,Ov) :-
  overlap(Ts,Trees,Ov0,Ov).

score_trees([],[]).
score_trees([treebank(N,_,T)|Ts],[treebank(N,S,T)|Ss]) :-
    fs_prob(maxent,T,_,S),
    score_trees(Ts,Ss).


	
