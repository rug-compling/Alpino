:- module(mj,[]).      % no exported predicates, the important ones are
                       % called with explicit Module: prefix, in order to
                       % be able to have multiple parse predicates. The
                       % ones that ARE used from outside, are:
        %% parse/1:    parse(o(Term,String,Semantics)) 
        %%             this is dictated by Hdrug. In the current directory
        %%             we furthermore assume that Term in fact will be 
        %%             a tree(Cat,Score,Ds) where Cat already incorporates
        %%             the top_category. Score and Ds are not instantiated
        %%             but they will be instantiated by this predicate. 
        %%             The parse/1 should generate all solutions upon
        %%             backtracking.

        %% compile_grammar/2:   compile_grammar(FileRoot,File)
        %%                      this is specific to current directory. This
        %%                      defines parser-specific compilation.

        %% clean/0:    to remove garbage (dynamic predicates from last run)

:- use_module( lists, library(lists), all ).

:- dynamic item/3, completer_index/4.

parse(o(Cat,String,_)) :-
	length(String,N),
	process_string(String,0,N),
	res(Cat). % represents results found by check_accept/5


store_item(Begin, End, RuleId, LHS, MemNr, RHS_suffix) :-
   assert(item(Begin, LHS, RHS_suffix), Ref),
   assert(completer_index(End, RuleId, MemNr, Ref)).


process_string([],M,M).
process_string([_Word|_],P0,P) :-
	P1 is P0 + 1,
%%      parser_lex(RuleId, LHS, Word), 
	user:lex(P0,P1,LHS,RuleId),
      process_completed(P0, P1, RuleId, LHS,P),
      fail.
process_string([_|Tail],P0,P) :-
      P1 is P0 + 1,
      process_string(Tail,P1,P).

% If a complete constituent is found, then either it is 
% the first member of the next rule to try; or a next member of
% a rule of which some members were already found; or a complete
% parse has been found for the complete input.

process_completed(Begin, End, Id, LHS, N) :-
   initiate_entries(Begin, End, Id, LHS, N),
   extend_entries(Begin, End, Id, LHS, N),
   check_accept(Begin, End, LHS, N).

initiate_entries(Begin, End, _Id, Mem, N) :-
%      copy_term(Mem, MemCopy),
%      first_member_index(Id, Members),
%      member(RuleId, Members),

%      parser_rule(RuleId, LHS, [Mem | RHS_suf]),
	user:l_rule(Mem,LHS,RHS_suf,RuleId),

      ( RHS_suf = [],
        process_completed(Begin, End, RuleId, LHS, N)
      ;
        RHS_suf = [_|_],
        store_item(Begin, End, RuleId, LHS, 2, RHS_suf)
      ),
      fail
   ;
      true.

extend_entries(Begin, End, _Id, Mem, N) :-
%%      copy_term(Mem, MemCopy),
%      nonfirst_member_index(Id, Members),
%      member(RuleId - MemNr, Members),
      completer_index(Begin, RuleId, MemNr, Ref),
      clause(item(ItBegin, LHS, [Mem | RHS_suf]),
             true, Ref),
      ( RHS_suf = [],
        process_completed(ItBegin, End, RuleId, LHS,N)
      ;
        RHS_suf = [_|_],
        NewMemNr is MemNr + 1,
        store_item(ItBegin, End, RuleId, LHS, NewMemNr, RHS_suf)
      ),
      fail
   ;
      true.
   
clean :-
   retractall(item(_,_,_)),          % cf. parserdriver
   retractall(completer_index(_,_,_,_)), % cf. parserdriver
   retractall(res(_)). % represents results found by check_accept/5


check_accept(0,N,Term,N) :-
	!,
     assertz(res(Term)).
check_accept(_,_,_,_).


