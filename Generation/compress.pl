:- module(alpino_compress, [ best_compression_parse/2 ]).

%%
%% Method 1
%%
%% Remove optional words from the depencency structure and generate.
%%
%% TODO
%%

%%
%% Method 2
%%
%% Paraphrase the compression.
%%
%% TODO

%%
%% Method 3
%%
%% Generate from the dependency structure of the compression that
%% has the highest correspondency (CA-score) when comparing to the
%% uncompressed sentence.
%%

best_compression_parse(Sentence,Compression) :-
    best_parse(Sentence,SentResult),
    result_sentence_cat(SentResult,Cat),
    best_compression(Compression,SentResult,BestResult),
    alpino_adt:result_to_adt(BestResult,Adt0),
    restore_sentence_cat(Adt0,Adt,Cat),
    hdrug_util:set_flag(number_analyses,1),
    user:generate(Adt).

best_parse(Sentence,Result) :-
    user:veryfast_options,
    user:alpino_parse_tokens(sentence,Sentence),
    user:object(1,o(Result,_,_)).

best_compression(Compression,CorrectResult,Result) :-
    hdrug_util:set_flag(number_analyses,3),
    user:alpino_parse_tokens(compression,Compression),
    alpino_treebank:get_system_triples(CorrectResult,CorRels,Cor,_,_),
    findall(Ca-Result,score_compressions(CorRels,Cor,Ca,Result),Compressions),
    keysort(Compressions,CompressionsSorted),
    lists:reverse(CompressionsSorted,[BestCa-Result|_]),
    format("best CA: ~f~n",[BestCa]).

score_compressions(CorRels,Cor,Ca,Result) :-
    user:object(_,o(Result,_,_)),
    alpino_treebank:get_system_triples(Result,SysRels,Sys,_,_),
    compr_compare_deprels(CorRels,SysRels,Ov,Cor,Sys),
    alpino_treebank:ca_score(Ov,Cor,Sys,Ca),
    format("Dep Rel's: ~d/~d/~d CA: ~2f~n",[Ov,Cor,Sys,Ca]).

compr_compare_deprels(CorrectRels,SystemRels,Overlap,Correct,System) :-
    compr_overlap(CorrectRels,SystemRels,0,Overlap,MissingCorrect,SystemMistakes),
    (   System == Overlap,
        Correct == Overlap
    ->  true
    ;   alpino_treebank:analyze_mistakes(MissingCorrect,SystemMistakes)
    ).

compr_overlap([deprel(HW/[HPos0,HPos],DepRel,DW/[DPos0,DPos])|Rels1],Rels2,
		S,S2,MC,MS) :-
    (   lists:select(deprel(HW/[_,_],DepRel,DW/[_,_]),
		     Rels2,Rels2a)
    ->  S1 is S + 1,
	compr_overlap(Rels1,Rels2a,S1,S2,MC,MS)
    ;   MC = [deprel(HW/[HPos0,HPos],DepRel,DW/[DPos0,DPos])|MC1],
	compr_overlap(Rels1,Rels2,S,S2,MC1,MS)
    ).
compr_overlap([],SystemMistakes,S,S,[],SystemMistakes).

result_sentence_cat(Result,Cat) :-
    alpino_data:result(Result,[TopCat],_),
    alpino_data:dt(TopCat,DT),
    alpino_data:dt(DT,_,_,Cat,_).

restore_sentence_cat(tree(r(top,p(top)),[tree(r(--,p(_)),Ds1)]),
		     tree(r(top,p(top)),[tree(r(--,p(Cat)),Ds1)]),
		     Cat).