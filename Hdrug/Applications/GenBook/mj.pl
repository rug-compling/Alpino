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

%% GvN: if module is already known, only import predicates, but don't
%% check if a newer version exists. This is crucial for runtime systems
%% in which the library is not even around anymore!
:- use_module( lists, library(lists), all ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Driver to parsers.

% Appended to this file should be 
% 1) a list of unit clauses derived from the grammar by the parser generator.
% 2) a specification of a word graph.

% For 1) we assume the following kind of information:
%
%    parser_root(Root).
%
% (is actually the same as root/1 in the source grammar)
%  
%    parser_lex(Id, LHS, Terminal).
%
% (is actually the same as lex/3 in the source grammar)
%
%    parser_rule(Id, LHS, RHS).
%
% (very similar to grammar_rule/3 but some transformations were
% performed by the parser generator)
%
%    first_member_index(Id, Members).
%
% (Members is a list of id's RuleId in which the first member may be unifiable
% with the LHS of the rule with id Id.)
%
%    nonfirst_member_index(Id, Members).
%
% (Members is a list of pairs RuleId-MemNr specifying members which may be 
% unifiable with the LHS of the rule with id Id.)

% For 2) we assume the following kind of information:
%
%    arrow(Begin, End, Label, Score, Id).
%
% (Begin and End are states in the word graph. Label is a terminal of
% the grammar. Score is some value indicating the probability of the arrow.
% Id is a unique identification of the arrow.
% We assume that the arrows are ordered so that arrow A1 of which Begin
% is the same as End of some arrow A2 occurs after A2.)
%
%    initial(Begin).
%
% (indicates the/a initial state in the word graph; not necessarily unique)
%
%    final(End).
%
% (indicates the/a final state in the word graph; not necessarily unique)
%
% We assume Begin is not End, so no empty input.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The parser is purely bottom-up, left-to-right; starting from lexical 
% information.
% We drag along the list of Id's of the arrows of paths though
% the word graph.
% Items are identified with some score which is some indication of
% the probability of the subparse.
% Each active item is stored in a table (not INactive ones!).
% There is no subsumption check of items in the table.

% The table is set up in such a way that finding active items from
% inactive ones is very easy.
% The actual active items are of the form
% 
%    item(Begin, LHS, RHS_suffix, Score, Arrows)
%
% indicating that the beginning of the item is at Begin; the untreated part
% of the RHS is RHS_suffix. The item is identified with a key Ref.
%
% Further, the table contains entries
%
%    completer_index(End, RuleId, MemNr, Ref)
%
% which points to an item with key Ref if that item conceptually has the end 
% at End, the used rule id is RuleId, and the number of the next member
% to be found (first element of RHS_suffix) is MemNr.
%
% For e.g. Earley's algorithm as it occurs in the literature, 
% we would have (instead of item/5 and completer/4)
% things like [Begin, End, LHS -> RHS_prefix dot RHS_suffix]
% where length(RHS_prefix) +1 = MemNr, and Rule_Id is the id of rule
% LHS -> RHS_prefix RHS_suffix.

:- dynamic item/3, completer_index/4.

parse(o(Cat,String,_)) :-
	length(String,N),
	process_string(String,0,N),
	res(Cat). % represents results found by check_accept/5


store_item(Begin, End, RuleId, LHS, MemNr, RHS_suffix) :-
   assert(item(Begin, LHS, RHS_suffix), Ref),
   assert(completer_index(End, RuleId, MemNr, Ref)).


process_string([],M,M).
process_string([Word|_],P0,P) :-
	P1 is P0 + 1,
      parser_lex(RuleId, LHS, Word), 
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

initiate_entries(Begin, End, Id, Mem, N) :-
%      copy_term(Mem, MemCopy),
      first_member_index(Id, Members),
      member(RuleId, Members),
      parser_rule(RuleId, LHS, [Mem | RHS_suf]),
      ( RHS_suf = [],
        process_completed(Begin, End, RuleId, LHS, N)
      ;
        RHS_suf \== [],
        store_item(Begin, End, RuleId, LHS, 2, RHS_suf)
      ),
      fail
   ;
      true.

extend_entries(Begin, End, Id, Mem, N) :-
%%      copy_term(Mem, MemCopy),
      nonfirst_member_index(Id, Members),
      member(RuleId - MemNr, Members),
      completer_index(Begin, RuleId, MemNr, Ref),
      clause(item(ItBegin, LHS, [Mem | RHS_suf]),
             true, Ref),
      ( RHS_suf = [],
        process_completed(ItBegin, End, RuleId, LHS,N)
      ;
        RHS_suf \== [],
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
     assertz(res(Term)),
     !.
check_accept(_,_,_,_).


/* Parser generated by the OVIS tools, */
/* University of Groningen, 1-6-95 */

parser_root(x(main/_44500,_44502-_44502,nil,_44497)).

parser_lex(jan0, x(np(sg)/john,_44779-_44779,nil,np(john)), jan).
parser_lex(marie0, x(np(sg)/mary,_44779-_44779,nil,np(mary)), marie).
parser_lex(jane0, x(np(sg)/jane,_44779-_44779,nil,np(jane)), jane).
parser_lex(tarzan0, x(np(sg)/tarzan,_44779-_44779,nil,np(tarzan)), tarzan).
parser_lex(man0, x(n(sg)/sg(man),_44781-_44781,nil,n(man)), man).
parser_lex(mannen0, x(n(pl)/pl(man),_44781-_44781,nil,n(mannen)), mannen).
parser_lex(vrouw0, x(n(sg)/sg(woman),_44781-_44781,nil,n(vrouw)), vrouw).
parser_lex(vrouwen0, x(n(pl)/pl(woman),_44781-_44781,nil,n(vrouwen)), vrouwen).
parser_lex(puree0, x(n(sg)/sg(mashed_potatoes),_44781-_44781,nil,n(puree)), puree).
parser_lex(de0, x(det(_44775,_44778)/_44775,_44780-_44780,nil,det(de)), de).
parser_lex(naar0, x(p(_44777)/naar(_44777),_44781-_44781,nil,p(naar)), naar).
parser_lex(met0, x(p(_44777)/met(_44777),_44781-_44781,nil,p(met)), met).
parser_lex(in0, x(p(_44777)/in(_44777),_44781-_44781,nil,p(in)), in).
parser_lex(van0, x(p(_44777)/van(_44777),_44781-_44781,nil,p(van)), van).
parser_lex(aan0, x(p(_44777)/aan(_44777),_44781-_44781,nil,p(aan)), aan).
parser_lex(op0, x(part/op,_44777-_44777,nil,part(op)), op).
parser_lex(dat0, x(comp(fin,_44778)/dat(_44778),_44782-_44782,nil,comp(dat)), dat).
parser_lex(omdat0, x(comp(fin,_44778)/because(_44778),_44782-_44782,nil,comp(omdat)), omdat).
parser_lex(om0, x(comp(infin,_44775)/_44775,_44780-_44780,nil,comp(om)), om).
parser_lex(vandaag0, x(adv/vandaag,_44777-_44777,nil,adv(vandaag)), vandaag).
parser_lex(slaapt0, x(v0(main,fin,np(sg)/_44783,_44787-_44787)/sleep(_44783),_44792-_44792,nil,v0(slaapt)), slaapt).
parser_lex(slapen0, x(v0(main,fin,np(pl)/_44783,_44787-_44787)/sleep(_44783),_44792-_44792,nil,v0(slapen)), slapen).
parser_lex(slapen1, x(v0(main,infin,np(_44785)/_44783,_44787-_44787)/sleep(_44783),_44792-_44792,nil,v0(slapen)), slapen).
parser_lex(geslapen0, x(v0(main,part,np(_44785)/_44783,_44787-_44787)/sleep(_44783),_44792-_44792,nil,v0(geslapen)), geslapen).
parser_lex(vangt0, x(v0(main,fin,np(sg)/_44783,[part/op,np(_44800)/_44798|_44788]-_44788)/catch(_44783,_44798),_44805-_44805,nil,v0(vangt)), vangt).
parser_lex(vangen0, x(v0(main,fin,np(pl)/_44783,[part/op,np(_44800)/_44798|_44788]-_44788)/catch(_44783,_44798),_44805-_44805,nil,v0(vangen)), vangen).
parser_lex(vangen1, x(v0(main,infin,np(_44785)/_44783,[part/op,np(_44800)/_44798|_44788]-_44788)/catch(_44783,_44798),_44805-_44805,nil,v0(vangen)), vangen).
parser_lex(gevangen0, x(v0(main,part,np(_44785)/_44783,[part/op,np(_44800)/_44798|_44788]-_44788)/catch(_44783,_44798),_44805-_44805,nil,v0(gevangen)), gevangen).
parser_lex(zit0, x(v0(main,fin,np(sg)/_44783,[pp/in(sg(mashed_potatoes))|_44788]-_44788)/in_trouble(_44783),_44801-_44801,nil,v0(zit)), zit).
parser_lex(zitten0, x(v0(main,fin,np(pl)/_44783,[pp/in(sg(mashed_potatoes))|_44788]-_44788)/in_trouble(_44783),_44801-_44801,nil,v0(zitten)), zitten).
parser_lex(zitten1, x(v0(main,infin,np(_44785)/_44783,[pp/in(sg(mashed_potatoes))|_44788]-_44788)/in_trouble(_44783),_44801-_44801,nil,v0(zitten)), zitten).
parser_lex(gezeten0, x(v0(main,part,np(_44785)/_44783,[pp/in(sg(mashed_potatoes))|_44788]-_44788)/in_trouble(_44783),_44801-_44801,nil,v0(gezeten)), gezeten).
parser_lex(kust0, x(v0(main,fin,np(sg)/_44783,[np(_44795)/_44793|_44788]-_44788)/kiss(_44783,_44793),_44800-_44800,nil,v0(kust)), kust).
parser_lex(kussen0, x(v0(main,fin,np(pl)/_44783,[np(_44795)/_44793|_44788]-_44788)/kiss(_44783,_44793),_44800-_44800,nil,v0(kussen)), kussen).
parser_lex(kussen1, x(v0(main,infin,np(_44785)/_44783,[np(_44795)/_44793|_44788]-_44788)/kiss(_44783,_44793),_44800-_44800,nil,v0(kussen)), kussen).
parser_lex(gekust0, x(v0(main,part,np(_44785)/_44783,[np(_44795)/_44793|_44788]-_44788)/kiss(_44783,_44793),_44800-_44800,nil,v0(gekust)), gekust).
parser_lex(zegt0, x(v0(main,fin,np(sg)/_44783,[sbar(fin)/dat(_44797)|_44788]-_44788)/say(_44783,_44797),_44802-_44802,nil,v0(zegt)), zegt).
parser_lex(zeggen0, x(v0(main,fin,np(pl)/_44783,[sbar(fin)/dat(_44797)|_44788]-_44788)/say(_44783,_44797),_44802-_44802,nil,v0(zeggen)), zeggen).
parser_lex(zeggen1, x(v0(main,infin,np(_44785)/_44783,[sbar(fin)/dat(_44797)|_44788]-_44788)/say(_44783,_44797),_44802-_44802,nil,v0(zeggen)), zeggen).
parser_lex(gezegd0, x(v0(main,part,np(_44785)/_44783,[sbar(fin)/dat(_44797)|_44788]-_44788)/say(_44783,_44797),_44802-_44802,nil,v0(gezegd)), gezegd).
parser_lex(dwingt0, x(v0(main,fin,np(sg)/_44783,[sbar(infin,np(sg)/_44799)/_44793,np(_44808)/_44799|_44788]-_44788)/force(_44783,_44799,_44793),_44814-_44814,nil,v0(dwingt)), dwingt).
parser_lex(dwingen0, x(v0(main,fin,np(pl)/_44783,[sbar(infin,np(pl)/_44799)/_44793,np(_44808)/_44799|_44788]-_44788)/force(_44783,_44799,_44793),_44814-_44814,nil,v0(dwingen)), dwingen).
parser_lex(dwingen1, x(v0(main,infin,np(_44785)/_44783,[sbar(infin,np(_44785)/_44799)/_44793,np(_44808)/_44799|_44788]-_44788)/force(_44783,_44799,_44793),_44814-_44814,nil,v0(dwingen)), dwingen).
parser_lex(gedwongen0, x(v0(main,part,np(_44785)/_44783,[sbar(infin,np(_44785)/_44799)/_44793,np(_44808)/_44799|_44788]-_44788)/force(_44783,_44799,_44793),_44814-_44814,nil,v0(gedwongen)), gedwongen).
parser_lex(probeert0, x(v0(main,fin,np(sg)/_44783,[sbar(infin,np(sg)/_44783)/_44793|_44788]-_44788)/try(_44783,_44793),_44806-_44806,nil,v0(probeert)), probeert).
parser_lex(proberen0, x(v0(main,fin,np(pl)/_44783,[sbar(infin,np(pl)/_44783)/_44793|_44788]-_44788)/try(_44783,_44793),_44806-_44806,nil,v0(proberen)), proberen).
parser_lex(proberen1, x(v0(main,infin,np(_44785)/_44783,[sbar(infin,np(_44785)/_44783)/_44793|_44788]-_44788)/try(_44783,_44793),_44806-_44806,nil,v0(proberen)), proberen).
parser_lex(geprobeerd0, x(v0(main,part,np(_44785)/_44783,[sbar(infin,np(_44785)/_44783)/_44793|_44788]-_44788)/try(_44783,_44793),_44806-_44806,nil,v0(geprobeerd)), geprobeerd).
parser_lex(geeft0, x(v0(main,fin,np(sg)/_44783,[np(_44795)/_44793,np(_44802)/_44800|_44788]-_44788)/give(_44783,_44793,_44800),_44808-_44808,nil,v0(geeft)), geeft).
parser_lex(geven0, x(v0(main,fin,np(pl)/_44783,[np(_44795)/_44793,np(_44802)/_44800|_44788]-_44788)/give(_44783,_44793,_44800),_44808-_44808,nil,v0(geven)), geven).
parser_lex(geven1, x(v0(main,infin,np(_44785)/_44783,[np(_44795)/_44793,np(_44802)/_44800|_44788]-_44788)/give(_44783,_44793,_44800),_44808-_44808,nil,v0(geven)), geven).
parser_lex(gegeven0, x(v0(main,part,np(_44785)/_44783,[np(_44795)/_44793,np(_44802)/_44800|_44788]-_44788)/give(_44783,_44793,_44800),_44808-_44808,nil,v0(gegeven)), gegeven).
parser_lex(geeft1, x(v0(main,fin,np(sg)/_44783,[pp/aan(_44795),np(_44802)/_44800|_44788]-_44788)/give(_44783,_44800,_44795),_44808-_44808,nil,v0(geeft)), geeft).
parser_lex(geven2, x(v0(main,fin,np(pl)/_44783,[pp/aan(_44795),np(_44802)/_44800|_44788]-_44788)/give(_44783,_44800,_44795),_44808-_44808,nil,v0(geven)), geven).
parser_lex(geven3, x(v0(main,infin,np(_44785)/_44783,[pp/aan(_44795),np(_44802)/_44800|_44788]-_44788)/give(_44783,_44800,_44795),_44808-_44808,nil,v0(geven)), geven).
parser_lex(gegeven1, x(v0(main,part,np(_44785)/_44783,[pp/aan(_44795),np(_44802)/_44800|_44788]-_44788)/give(_44783,_44800,_44795),_44808-_44808,nil,v0(gegeven)), gegeven).
parser_lex(heeft0, x(v0(aux,fin,np(sg)/_44783,v(part,np(sg)/_44783,_44788)/_44791+_44788)/perf(_44791),_44804-_44804,nil,aux(heeft)), heeft).
parser_lex(hebben0, x(v0(aux,fin,np(pl)/_44783,v(part,np(pl)/_44783,_44788)/_44791+_44788)/perf(_44791),_44804-_44804,nil,aux(hebben)), hebben).
parser_lex(hebben1, x(v0(aux,infin,np(_44785)/_44783,v(part,np(_44785)/_44783,_44788)/_44791+_44788)/perf(_44791),_44804-_44804,nil,aux(hebben)), hebben).
parser_lex(gehad0, x(v0(aux,part,np(_44785)/_44783,v(part,np(_44785)/_44783,_44788)/_44791+_44788)/perf(_44791),_44804-_44804,nil,aux(gehad)), gehad).
parser_lex(zal0, x(v0(aux,fin,np(sg)/_44783,v(infin,np(sg)/_44783,_44788)/_44791+_44788)/fut(_44791),_44804-_44804,nil,aux(zal)), zal).
parser_lex(zullen0, x(v0(aux,fin,np(pl)/_44783,v(infin,np(pl)/_44783,_44788)/_44791+_44788)/fut(_44791),_44804-_44804,nil,aux(zullen)), zullen).
parser_lex(zullen1, x(v0(aux,infin,np(_44785)/_44783,v(infin,np(_44785)/_44783,_44788)/_44791+_44788)/fut(_44791),_44804-_44804,nil,aux(zullen)), zullen).
parser_lex(te0, x(v0(aux,te,np(_44785)/_44783,v(infin,np(_44785)/_44783,_44788)/_44775+_44788)/_44775,_44802-_44802,nil,aux(te)), te).
parser_lex(probeert1, x(v0(aux,fin,np(sg)/_44783,v(te,_44797/_44783,_44788)/_44791+_44788)/try(_44783,_44791),_44803-_44803,nil,aux(probeert)), probeert).
parser_lex(proberen2, x(v0(aux,fin,np(pl)/_44783,v(te,_44797/_44783,_44788)/_44791+_44788)/try(_44783,_44791),_44803-_44803,nil,aux(proberen)), proberen).
parser_lex(proberen3, x(v0(aux,infin,np(_44785)/_44783,v(te,_44797/_44783,_44788)/_44791+_44788)/try(_44783,_44791),_44803-_44803,nil,aux(proberen)), proberen).
parser_lex(proberen4, x(v0(aux,part,np(_44785)/_44783,v(te,_44797/_44783,_44788)/_44791+_44788)/try(_44783,_44791),_44803-_44803,nil,aux(proberen)), proberen).
parser_lex(ziet0, x(v0(aux,fin,np(sg)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/see(_44783,_44791),_44808-_44808,nil,aux(ziet)), ziet).
parser_lex(zien0, x(v0(aux,fin,np(pl)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/see(_44783,_44791),_44808-_44808,nil,aux(zien)), zien).
parser_lex(zien1, x(v0(aux,infin,np(_44785)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/see(_44783,_44791),_44808-_44808,nil,aux(zien)), zien).
parser_lex(zien2, x(v0(aux,part,np(_44785)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/see(_44783,_44791),_44808-_44808,nil,aux(zien)), zien).
parser_lex(helpt0, x(v0(aux,fin,np(sg)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/help(_44783,_44791),_44808-_44808,nil,aux(helpt)), helpt).
parser_lex(helpen0, x(v0(aux,fin,np(pl)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/help(_44783,_44791),_44808-_44808,nil,aux(helpen)), helpen).
parser_lex(helpen1, x(v0(aux,infin,np(_44785)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/help(_44783,_44791),_44808-_44808,nil,aux(helpen)), helpen).
parser_lex(helpen2, x(v0(aux,part,np(_44785)/_44783,v(infin,_44794,_44797-[_44794|_44800])/_44791+(_44797-_44800))/help(_44783,_44791),_44808-_44808,nil,aux(helpen)), helpen).

parser_rule(x0:[+,+,-]:1, x(main/_44934,_44931-_44931,nil,main(_44926,_44927,s(topic_gap,vp(v(vgap))))),
   [x(np(_44907)/_44910,_44905-_44905,nil,_44926),x(v0(main,fin,np(_44907)/_44910,[]-[])/_44934,_44879-_44879,nil,_44927)]).

parser_rule(x0:[+,+,+]:1, x(main/_44947,_44944-_44944,nil,main(_44939,_44940,_44941)),
   [x(np(_44927)/_44930,_44925-_44925,nil,_44939),x(v0(_44909,fin,_44911,_44912)/_44915,_44907-_44907,nil,_44940),x(s(fin)/_44947,np(_44927)/_44930-nil,v0(_44909,fin,_44911,_44912)/_44915,_44941)]).

parser_rule(x1:[+,+,+]:1, x(main/_44943,_44940-_44940,nil,main(_44935,_44936,_44937)),
   [x(pp/_44926,_44923-_44923,nil,_44935),x(v0(_44907,fin,_44909,_44910)/_44913,_44905-_44905,nil,_44936),x(s(fin)/_44943,pp/_44926-nil,v0(_44907,fin,_44909,_44910)/_44913,_44937)]).

parser_rule(x2:[+,+,+]:1, x(main/_44947,_44944-_44944,nil,main(_44939,_44940,_44941)),
   [x(sbar(_44927)/_44930,_44925-_44925,nil,_44939),x(v0(_44909,fin,_44911,_44912)/_44915,_44907-_44907,nil,_44940),x(s(fin)/_44947,sbar(_44927)/_44930-nil,v0(_44909,fin,_44911,_44912)/_44915,_44941)]).

parser_rule(x3:[+,+,+]:1, x(main/_44949,_44946-_44946,nil,main(_44941,_44942,_44943)),
   [x(sbar(_44928,_44929)/_44932,_44926-_44926,nil,_44941),x(v0(_44910,fin,_44912,_44913)/_44916,_44908-_44908,nil,_44942),x(s(fin)/_44949,sbar(_44928,_44929)/_44932-nil,v0(_44910,fin,_44912,_44913)/_44916,_44943)]).

parser_rule(x4:[+,+,+]:1, x(main/_44943,_44940-_44940,nil,main(_44935,_44936,_44937)),
   [x(adv/_44926,_44923-_44923,nil,_44935),x(v0(_44907,fin,_44909,_44910)/_44913,_44905-_44905,nil,_44936),x(s(fin)/_44943,adv/_44926-nil,v0(_44907,fin,_44909,_44910)/_44913,_44937)]).

parser_rule(x5:[+,+]:1, x(sbar(fin)/_44918,_44913-_44913,nil,sbar(_44909,_44910)),
   [x(comp(fin,_44897)/_44918,_44894-_44894,nil,_44909),x(s(fin)/_44897,_44879-_44879,nil,_44910)]).

parser_rule(x6:[-,+]:1, x(sbar(infin,_44902)/_44905,_44899-_44899,nil,sbar(comp,_44896)),
   [x(vp(te,_44902,[])/_44905,_44879-_44879,nil,_44896)]).

parser_rule(x6:[+,+]:1, x(sbar(infin,_44918)/_44921,_44915-_44915,nil,sbar(_44911,_44912)),
   [x(comp(infin,_44899)/_44921,_44896-_44896,nil,_44911),x(vp(te,_44918,[])/_44899,_44879-_44879,nil,_44912)]).

parser_rule(x7:[-,-]:1, x(s(fin)/_44912,np(_44901)/_44904-nil,v0(main,fin,np(_44901)/_44904,[]-[])/_44912,s(topic_gap,vp(v(vgap)))),
   []).

parser_rule(x7:[-,+]:1, x(s(_44911)/_44914,np(_44903)/_44906-_44909,_44918,s(topic_gap,_44901)),
   [x(vp(_44911,np(_44903)/_44906,[])/_44914,nil-_44909,_44918,_44901)]).

parser_rule(x7:[+,-]:1, x(s(fin)/_44922,_44916-_44917,v0(main,fin,np(_44903)/_44906,[]-[])/_44922,s(_44897,vp(v(vgap)))),
   [x(np(_44903)/_44906,_44916-_44917,nil,_44897)]).

parser_rule(x7:[+,-]:2, x(s(fin)/_44937,_44931-nil,v0(main,fin,np(_44918)/_44921,[np(_44908)/_44911]-[])/_44937,s(_44905,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44918)/_44921,_44931-np(_44908)/_44911,nil,_44905)]).

parser_rule(x7:[+,-]:3, x(s(fin)/_44933,_44927-nil,v0(main,fin,np(_44914)/_44917,[adv/_44907]-[])/_44933,s(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44914)/_44917,_44927-adv/_44907,nil,_44903)]).

parser_rule(x7:[+,-]:4, x(s(fin)/_44933,_44927-nil,v0(main,fin,np(_44914)/_44917,[pp/_44907]-[])/_44933,s(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44914)/_44917,_44927-pp/_44907,nil,_44903)]).

parser_rule(x7:[+,-]:5, x(s(fin)/_44937,_44931-nil,v0(main,fin,np(_44918)/_44921,[sbar(_44908)/_44911]-[])/_44937,s(_44905,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44918)/_44921,_44931-sbar(_44908)/_44911,nil,_44905)]).

parser_rule(x7:[+,-]:6, x(s(fin)/_44939,_44933-nil,v0(main,fin,np(_44920)/_44923,[sbar(_44909,_44910)/_44913]-[])/_44939,s(_44906,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44920)/_44923,_44933-sbar(_44909,_44910)/_44913,nil,_44906)]).

parser_rule(x7:[+,-]:7, x(s(fin)/_44933,_44927-nil,v0(main,fin,np(_44914)/_44917,[pp/_44907]-[])/_44933,s(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44914)/_44917,_44927-pp/_44907,nil,_44903)]).

parser_rule(x7:[+,-]:8, x(s(fin)/[_44926,_44924],_44922-nil,v0(main,fin,np(_44909)/_44912,[]-[])/_44924,s(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44909)/_44912,_44922-pp/_44926,nil,_44903)]).

parser_rule(x7:[+,-]:9, x(s(fin)/[_44926,_44924],_44922-nil,v0(main,fin,np(_44909)/_44912,[]-[])/_44924,s(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44909)/_44912,_44922-adv/_44926,nil,_44903)]).

parser_rule(x7:[+,-]:10, x(s(fin)/[_44926,_44924],_44922-nil,v0(main,fin,np(_44909)/_44912,[]-[])/_44924,s(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44909)/_44912,_44922-pp/_44926,nil,_44903)]).

parser_rule(x7:[+,-]:11, x(s(fin)/[_44928,_44926],_44924-nil,v0(main,fin,np(_44911)/_44914,[]-[])/_44926,s(_44905,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44911)/_44914,_44924-sbar(_44878)/_44928,nil,_44905)]).

parser_rule(x7:[+,-]:12, x(s(fin)/[_44929,_44927],_44925-nil,v0(main,fin,np(_44912)/_44915,[]-[])/_44927,s(_44906,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44912)/_44915,_44925-sbar(_44878,_44879)/_44929,nil,_44906)]).

parser_rule(x7:[+,+]:1, x(s(_44921)/_44924,_44918-_44919,_44928,s(_44915,_44916)),
   [x(np(_44903)/_44906,_44918-_44901,nil,_44915),x(vp(_44921,np(_44903)/_44906,[])/_44924,_44901-_44919,_44928,_44916)]).

parser_rule(x8:[-,-]:1, x(vp(fin,_44912,_44913)/_44916,np(_44903)/_44906-nil,v0(main,fin,_44912,[np(_44903)/_44906|_44913]-[])/_44916,vp(topic_gap,vp(v(vgap)))),
   []).

parser_rule(x8:[-,+]:1, x(vp(_44913,_44914,_44915)/_44918,np(_44905)/_44908-_44911,_44922,vp(topic_gap,_44903)),
   [x(vp(_44913,_44914,[np(_44905)/_44908|_44915])/_44918,nil-_44911,_44922,_44903)]).

parser_rule(x8:[+,-]:1, x(vp(fin,_44922,_44923)/_44926,_44918-_44919,v0(main,fin,_44922,[np(_44900)/_44903|_44923]-[])/_44926,vp(_44897,vp(v(vgap)))),
   [x(np(_44900)/_44903,_44918-_44919,nil,_44897)]).

parser_rule(x8:[+,-]:2, x(vp(fin,_44937,_44938)/_44941,_44933-nil,v0(main,fin,_44937,[np(_44915)/_44918,np(_44908)/_44911|_44938]-[])/_44941,vp(_44905,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44908)/_44911,_44933-np(_44915)/_44918,nil,_44905)]).

parser_rule(x8:[+,-]:3, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[adv/_44914,np(_44906)/_44909|_44934]-[])/_44937,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44906)/_44909,_44929-adv/_44914,nil,_44903)]).

parser_rule(x8:[+,-]:4, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[pp/_44914,np(_44906)/_44909|_44934]-[])/_44937,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44906)/_44909,_44929-pp/_44914,nil,_44903)]).

parser_rule(x8:[+,-]:5, x(vp(fin,_44937,_44938)/_44941,_44933-nil,v0(main,fin,_44937,[sbar(_44915)/_44918,np(_44908)/_44911|_44938]-[])/_44941,vp(_44905,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44908)/_44911,_44933-sbar(_44915)/_44918,nil,_44905)]).

parser_rule(x8:[+,-]:6, x(vp(fin,_44939,_44940)/_44943,_44935-nil,v0(main,fin,_44939,[sbar(_44916,_44917)/_44920,np(_44909)/_44912|_44940]-[])/_44943,vp(_44906,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44909)/_44912,_44935-sbar(_44916,_44917)/_44920,nil,_44906)]).

parser_rule(x8:[+,-]:7, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[pp/_44914,np(_44906)/_44909|_44934]-[])/_44937,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44906)/_44909,_44929-pp/_44914,nil,_44903)]).

parser_rule(x8:[+,-]:8, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[np(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44906)/_44909,_44924-pp/_44928,nil,_44903)]).

parser_rule(x8:[+,-]:9, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[np(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(np(_44906)/_44909,_44924-adv/_44928,nil,_44903)]).

parser_rule(x8:[+,-]:10, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[np(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44906)/_44909,_44924-pp/_44928,nil,_44903)]).

parser_rule(x8:[+,-]:11, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[np(_44908)/_44911|_44935]-[])/_44928,vp(_44905,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44908)/_44911,_44926-sbar(_44878)/_44930,nil,_44905)]).

parser_rule(x8:[+,-]:12, x(vp(fin,_44935,_44936)/[_44931,_44929],_44927-nil,v0(main,fin,_44935,[np(_44909)/_44912|_44936]-[])/_44929,vp(_44906,vp(vp(v(vgap)),topic_gap))),
   [x(np(_44909)/_44912,_44927-sbar(_44878,_44879)/_44931,nil,_44906)]).

parser_rule(x8:[+,+]:1, x(vp(_44923,_44924,_44925)/_44928,_44920-_44921,_44932,vp(_44917,_44918)),
   [x(np(_44905)/_44908,_44920-_44903,nil,_44917),x(vp(_44923,_44924,[np(_44905)/_44908|_44925])/_44928,_44903-_44921,_44932,_44918)]).

parser_rule(x9:[+,-]:1, x(vp(fin,_44918,_44919)/_44922,_44914-_44915,v0(main,fin,_44918,[part/_44899|_44919]-[])/_44922,vp(_44895,vp(v(vgap)))),
   [x(part/_44899,_44914-_44915,nil,_44895)]).

parser_rule(x9:[+,-]:2, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[np(_44911)/_44914,part/_44907|_44934]-[])/_44937,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(part/_44907,_44929-np(_44911)/_44914,nil,_44903)]).

parser_rule(x9:[+,-]:3, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[adv/_44910,part/_44905|_44930]-[])/_44933,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(part/_44905,_44925-adv/_44910,nil,_44901)]).

parser_rule(x9:[+,-]:4, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,part/_44905|_44930]-[])/_44933,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(part/_44905,_44925-pp/_44910,nil,_44901)]).

parser_rule(x9:[+,-]:5, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[sbar(_44911)/_44914,part/_44907|_44934]-[])/_44937,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(part/_44907,_44929-sbar(_44911)/_44914,nil,_44903)]).

parser_rule(x9:[+,-]:6, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[sbar(_44912,_44913)/_44916,part/_44908|_44936]-[])/_44939,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(part/_44908,_44931-sbar(_44912,_44913)/_44916,nil,_44904)]).

parser_rule(x9:[+,-]:7, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,part/_44905|_44930]-[])/_44933,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(part/_44905,_44925-pp/_44910,nil,_44901)]).

parser_rule(x9:[+,-]:8, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[part/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(part/_44905,_44920-pp/_44924,nil,_44901)]).

parser_rule(x9:[+,-]:9, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[part/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(part/_44905,_44920-adv/_44924,nil,_44901)]).

parser_rule(x9:[+,-]:10, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[part/_44905|_44929]-[])/_44922,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(part/_44905,_44920-pp/_44924,nil,_44901)]).

parser_rule(x9:[+,-]:11, x(vp(fin,_44930,_44931)/[_44926,_44924],_44922-nil,v0(main,fin,_44930,[part/_44907|_44931]-[])/_44924,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(part/_44907,_44922-sbar(_44878)/_44926,nil,_44903)]).

parser_rule(x9:[+,-]:12, x(vp(fin,_44931,_44932)/[_44927,_44925],_44923-nil,v0(main,fin,_44931,[part/_44908|_44932]-[])/_44925,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(part/_44908,_44923-sbar(_44878,_44879)/_44927,nil,_44904)]).

parser_rule(x9:[+,+]:1, x(vp(_44919,_44920,_44921)/_44924,_44916-_44917,_44928,vp(_44913,_44914)),
   [x(part/_44904,_44916-_44901,nil,_44913),x(vp(_44919,_44920,[part/_44904|_44921])/_44924,_44901-_44917,_44928,_44914)]).

parser_rule(x10:[-,-]:1, x(vp(fin,_44908,_44909)/_44912,adv/_44902-nil,v0(main,fin,_44908,[adv/_44902|_44909]-[])/_44912,vp(topic_gap,vp(v(vgap)))),
   []).

parser_rule(x10:[-,+]:1, x(vp(_44909,_44910,_44911)/_44914,adv/_44904-_44907,_44918,vp(topic_gap,_44901)),
   [x(vp(_44909,_44910,[adv/_44904|_44911])/_44914,nil-_44907,_44918,_44901)]).

parser_rule(x10:[+,-]:1, x(vp(fin,_44918,_44919)/_44922,_44914-_44915,v0(main,fin,_44918,[adv/_44899|_44919]-[])/_44922,vp(_44895,vp(v(vgap)))),
   [x(adv/_44899,_44914-_44915,nil,_44895)]).

parser_rule(x10:[+,-]:2, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[np(_44911)/_44914,adv/_44907|_44934]-[])/_44937,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44907,_44929-np(_44911)/_44914,nil,_44903)]).

parser_rule(x10:[+,-]:3, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[adv/_44910,adv/_44905|_44930]-[])/_44933,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44905,_44925-adv/_44910,nil,_44901)]).

parser_rule(x10:[+,-]:4, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,adv/_44905|_44930]-[])/_44933,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44905,_44925-pp/_44910,nil,_44901)]).

parser_rule(x10:[+,-]:5, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[sbar(_44911)/_44914,adv/_44907|_44934]-[])/_44937,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44907,_44929-sbar(_44911)/_44914,nil,_44903)]).

parser_rule(x10:[+,-]:6, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[sbar(_44912,_44913)/_44916,adv/_44908|_44936]-[])/_44939,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44908,_44931-sbar(_44912,_44913)/_44916,nil,_44904)]).

parser_rule(x10:[+,-]:7, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,adv/_44905|_44930]-[])/_44933,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44905,_44925-pp/_44910,nil,_44901)]).

parser_rule(x10:[+,-]:8, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[adv/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44905,_44920-pp/_44924,nil,_44901)]).

parser_rule(x10:[+,-]:9, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[adv/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44905,_44920-adv/_44924,nil,_44901)]).

parser_rule(x10:[+,-]:10, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[adv/_44905|_44929]-[])/_44922,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44905,_44920-pp/_44924,nil,_44901)]).

parser_rule(x10:[+,-]:11, x(vp(fin,_44930,_44931)/[_44926,_44924],_44922-nil,v0(main,fin,_44930,[adv/_44907|_44931]-[])/_44924,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44907,_44922-sbar(_44878)/_44926,nil,_44903)]).

parser_rule(x10:[+,-]:12, x(vp(fin,_44931,_44932)/[_44927,_44925],_44923-nil,v0(main,fin,_44931,[adv/_44908|_44932]-[])/_44925,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44908,_44923-sbar(_44878,_44879)/_44927,nil,_44904)]).

parser_rule(x10:[+,+]:1, x(vp(_44919,_44920,_44921)/_44924,_44916-_44917,_44928,vp(_44913,_44914)),
   [x(adv/_44904,_44916-_44901,nil,_44913),x(vp(_44919,_44920,[adv/_44904|_44921])/_44924,_44901-_44917,_44928,_44914)]).

parser_rule(x11:[-,-]:1, x(vp(fin,_44908,_44909)/_44912,pp/_44902-nil,v0(main,fin,_44908,[pp/_44902|_44909]-[])/_44912,vp(topic_gap,vp(v(vgap)))),
   []).

parser_rule(x11:[-,+]:1, x(vp(_44909,_44910,_44911)/_44914,pp/_44904-_44907,_44918,vp(topic_gap,_44901)),
   [x(vp(_44909,_44910,[pp/_44904|_44911])/_44914,nil-_44907,_44918,_44901)]).

parser_rule(x11:[+,-]:1, x(vp(fin,_44918,_44919)/_44922,_44914-_44915,v0(main,fin,_44918,[pp/_44899|_44919]-[])/_44922,vp(_44895,vp(v(vgap)))),
   [x(pp/_44899,_44914-_44915,nil,_44895)]).

parser_rule(x11:[+,-]:2, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[np(_44911)/_44914,pp/_44907|_44934]-[])/_44937,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44907,_44929-np(_44911)/_44914,nil,_44903)]).

parser_rule(x11:[+,-]:3, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[adv/_44910,pp/_44905|_44930]-[])/_44933,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44905,_44925-adv/_44910,nil,_44901)]).

parser_rule(x11:[+,-]:4, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,pp/_44905|_44930]-[])/_44933,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44905,_44925-pp/_44910,nil,_44901)]).

parser_rule(x11:[+,-]:5, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[sbar(_44911)/_44914,pp/_44907|_44934]-[])/_44937,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44907,_44929-sbar(_44911)/_44914,nil,_44903)]).

parser_rule(x11:[+,-]:6, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[sbar(_44912,_44913)/_44916,pp/_44908|_44936]-[])/_44939,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44908,_44931-sbar(_44912,_44913)/_44916,nil,_44904)]).

parser_rule(x11:[+,-]:7, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,pp/_44905|_44930]-[])/_44933,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44905,_44925-pp/_44910,nil,_44901)]).

parser_rule(x11:[+,-]:8, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44905,_44920-pp/_44924,nil,_44901)]).

parser_rule(x11:[+,-]:9, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44905,_44920-adv/_44924,nil,_44901)]).

parser_rule(x11:[+,-]:10, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44905,_44920-pp/_44924,nil,_44901)]).

parser_rule(x11:[+,-]:11, x(vp(fin,_44930,_44931)/[_44926,_44924],_44922-nil,v0(main,fin,_44930,[pp/_44907|_44931]-[])/_44924,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44907,_44922-sbar(_44878)/_44926,nil,_44903)]).

parser_rule(x11:[+,-]:12, x(vp(fin,_44931,_44932)/[_44927,_44925],_44923-nil,v0(main,fin,_44931,[pp/_44908|_44932]-[])/_44925,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44908,_44923-sbar(_44878,_44879)/_44927,nil,_44904)]).

parser_rule(x11:[+,+]:1, x(vp(_44919,_44920,_44921)/_44924,_44916-_44917,_44928,vp(_44913,_44914)),
   [x(pp/_44904,_44916-_44901,nil,_44913),x(vp(_44919,_44920,[pp/_44904|_44921])/_44924,_44901-_44917,_44928,_44914)]).

parser_rule(x12:[-,-]:1, x(vp(fin,_44912,_44913)/_44916,sbar(_44903)/_44906-nil,v0(main,fin,_44912,[sbar(_44903)/_44906|_44913]-[])/_44916,vp(vp(v(vgap)),topic_gap)),
   []).

parser_rule(x12:[-,+]:1, x(vp(fin,_44922,_44923)/_44926,_44918-_44919,v0(main,fin,_44922,[sbar(_44900)/_44903|_44923]-[])/_44926,vp(vp(v(vgap)),_44898)),
   [x(sbar(_44900)/_44903,_44918-_44919,nil,_44898)]).

parser_rule(x12:[-,+]:2, x(vp(fin,_44937,_44938)/_44941,_44933-nil,v0(main,fin,_44937,[np(_44915)/_44918,sbar(_44908)/_44911|_44938]-[])/_44941,vp(vp(topic_gap,vp(v(vgap))),_44906)),
   [x(sbar(_44908)/_44911,_44933-np(_44915)/_44918,nil,_44906)]).

parser_rule(x12:[-,+]:3, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[adv/_44914,sbar(_44906)/_44909|_44934]-[])/_44937,vp(vp(topic_gap,vp(v(vgap))),_44904)),
   [x(sbar(_44906)/_44909,_44929-adv/_44914,nil,_44904)]).

parser_rule(x12:[-,+]:4, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[pp/_44914,sbar(_44906)/_44909|_44934]-[])/_44937,vp(vp(topic_gap,vp(v(vgap))),_44904)),
   [x(sbar(_44906)/_44909,_44929-pp/_44914,nil,_44904)]).

parser_rule(x12:[-,+]:5, x(vp(fin,_44937,_44938)/_44941,_44933-nil,v0(main,fin,_44937,[sbar(_44915)/_44918,sbar(_44908)/_44911|_44938]-[])/_44941,vp(vp(vp(v(vgap)),topic_gap),_44906)),
   [x(sbar(_44908)/_44911,_44933-sbar(_44915)/_44918,nil,_44906)]).

parser_rule(x12:[-,+]:6, x(vp(fin,_44939,_44940)/_44943,_44935-nil,v0(main,fin,_44939,[sbar(_44916,_44917)/_44920,sbar(_44909)/_44912|_44940]-[])/_44943,vp(vp(vp(v(vgap)),topic_gap),_44907)),
   [x(sbar(_44909)/_44912,_44935-sbar(_44916,_44917)/_44920,nil,_44907)]).

parser_rule(x12:[-,+]:7, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[pp/_44914,sbar(_44906)/_44909|_44934]-[])/_44937,vp(vp(vp(v(vgap)),topic_gap),_44904)),
   [x(sbar(_44906)/_44909,_44929-pp/_44914,nil,_44904)]).

parser_rule(x12:[-,+]:8, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[sbar(_44906)/_44909|_44933]-[])/_44926,vp(vp(topic_gap,vp(v(vgap))),_44904)),
   [x(sbar(_44906)/_44909,_44924-pp/_44928,nil,_44904)]).

parser_rule(x12:[-,+]:9, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[sbar(_44906)/_44909|_44933]-[])/_44926,vp(vp(topic_gap,vp(v(vgap))),_44904)),
   [x(sbar(_44906)/_44909,_44924-adv/_44928,nil,_44904)]).

parser_rule(x12:[-,+]:10, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[sbar(_44906)/_44909|_44933]-[])/_44926,vp(vp(vp(v(vgap)),topic_gap),_44904)),
   [x(sbar(_44906)/_44909,_44924-pp/_44928,nil,_44904)]).

parser_rule(x12:[-,+]:11, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[sbar(_44908)/_44911|_44935]-[])/_44928,vp(vp(vp(v(vgap)),topic_gap),_44906)),
   [x(sbar(_44908)/_44911,_44926-sbar(_44878)/_44930,nil,_44906)]).

parser_rule(x12:[-,+]:12, x(vp(fin,_44935,_44936)/[_44931,_44929],_44927-nil,v0(main,fin,_44935,[sbar(_44909)/_44912|_44936]-[])/_44929,vp(vp(vp(v(vgap)),topic_gap),_44907)),
   [x(sbar(_44909)/_44912,_44927-sbar(_44878,_44879)/_44931,nil,_44907)]).

parser_rule(x12:[+,-]:1, x(vp(_44913,_44914,_44915)/_44918,sbar(_44905)/_44908-_44911,_44922,vp(_44902,topic_gap)),
   [x(vp(_44913,_44914,[sbar(_44905)/_44908|_44915])/_44918,nil-_44911,_44922,_44902)]).

parser_rule(x12:[+,+]:1, x(vp(_44923,_44924,_44925)/_44928,_44920-_44921,_44932,vp(_44917,_44918)),
   [x(vp(_44923,_44924,[sbar(_44896)/_44899|_44925])/_44928,_44893-_44921,_44932,_44917),x(sbar(_44896)/_44899,_44920-_44893,nil,_44918)]).

parser_rule(x13:[-,-]:1, x(vp(fin,_44914,_44915)/_44918,sbar(_44904,_44905)/_44908-nil,v0(main,fin,_44914,[sbar(_44904,_44905)/_44908|_44915]-[])/_44918,vp(vp(v(vgap)),topic_gap)),
   []).

parser_rule(x13:[-,+]:1, x(vp(fin,_44924,_44925)/_44928,_44920-_44921,v0(main,fin,_44924,[sbar(_44901,_44902)/_44905|_44925]-[])/_44928,vp(vp(v(vgap)),_44899)),
   [x(sbar(_44901,_44902)/_44905,_44920-_44921,nil,_44899)]).

parser_rule(x13:[-,+]:2, x(vp(fin,_44939,_44940)/_44943,_44935-nil,v0(main,fin,_44939,[np(_44917)/_44920,sbar(_44909,_44910)/_44913|_44940]-[])/_44943,vp(vp(topic_gap,vp(v(vgap))),_44907)),
   [x(sbar(_44909,_44910)/_44913,_44935-np(_44917)/_44920,nil,_44907)]).

parser_rule(x13:[-,+]:3, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[adv/_44916,sbar(_44907,_44908)/_44911|_44936]-[])/_44939,vp(vp(topic_gap,vp(v(vgap))),_44905)),
   [x(sbar(_44907,_44908)/_44911,_44931-adv/_44916,nil,_44905)]).

parser_rule(x13:[-,+]:4, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[pp/_44916,sbar(_44907,_44908)/_44911|_44936]-[])/_44939,vp(vp(topic_gap,vp(v(vgap))),_44905)),
   [x(sbar(_44907,_44908)/_44911,_44931-pp/_44916,nil,_44905)]).

parser_rule(x13:[-,+]:5, x(vp(fin,_44939,_44940)/_44943,_44935-nil,v0(main,fin,_44939,[sbar(_44917)/_44920,sbar(_44909,_44910)/_44913|_44940]-[])/_44943,vp(vp(vp(v(vgap)),topic_gap),_44907)),
   [x(sbar(_44909,_44910)/_44913,_44935-sbar(_44917)/_44920,nil,_44907)]).

parser_rule(x13:[-,+]:6, x(vp(fin,_44941,_44942)/_44945,_44937-nil,v0(main,fin,_44941,[sbar(_44918,_44919)/_44922,sbar(_44910,_44911)/_44914|_44942]-[])/_44945,vp(vp(vp(v(vgap)),topic_gap),_44908)),
   [x(sbar(_44910,_44911)/_44914,_44937-sbar(_44918,_44919)/_44922,nil,_44908)]).

parser_rule(x13:[-,+]:7, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[pp/_44916,sbar(_44907,_44908)/_44911|_44936]-[])/_44939,vp(vp(vp(v(vgap)),topic_gap),_44905)),
   [x(sbar(_44907,_44908)/_44911,_44931-pp/_44916,nil,_44905)]).

parser_rule(x13:[-,+]:8, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[sbar(_44907,_44908)/_44911|_44935]-[])/_44928,vp(vp(topic_gap,vp(v(vgap))),_44905)),
   [x(sbar(_44907,_44908)/_44911,_44926-pp/_44930,nil,_44905)]).

parser_rule(x13:[-,+]:9, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[sbar(_44907,_44908)/_44911|_44935]-[])/_44928,vp(vp(topic_gap,vp(v(vgap))),_44905)),
   [x(sbar(_44907,_44908)/_44911,_44926-adv/_44930,nil,_44905)]).

parser_rule(x13:[-,+]:10, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[sbar(_44907,_44908)/_44911|_44935]-[])/_44928,vp(vp(vp(v(vgap)),topic_gap),_44905)),
   [x(sbar(_44907,_44908)/_44911,_44926-pp/_44930,nil,_44905)]).

parser_rule(x13:[-,+]:11, x(vp(fin,_44936,_44937)/[_44932,_44930],_44928-nil,v0(main,fin,_44936,[sbar(_44909,_44910)/_44913|_44937]-[])/_44930,vp(vp(vp(v(vgap)),topic_gap),_44907)),
   [x(sbar(_44909,_44910)/_44913,_44928-sbar(_44878)/_44932,nil,_44907)]).

parser_rule(x13:[-,+]:12, x(vp(fin,_44937,_44938)/[_44933,_44931],_44929-nil,v0(main,fin,_44937,[sbar(_44910,_44911)/_44914|_44938]-[])/_44931,vp(vp(vp(v(vgap)),topic_gap),_44908)),
   [x(sbar(_44910,_44911)/_44914,_44929-sbar(_44878,_44879)/_44933,nil,_44908)]).

parser_rule(x13:[+,-]:1, x(vp(_44915,_44916,_44917)/_44920,sbar(_44906,_44907)/_44910-_44913,_44924,vp(_44903,topic_gap)),
   [x(vp(_44915,_44916,[sbar(_44906,_44907)/_44910|_44917])/_44920,nil-_44913,_44924,_44903)]).

parser_rule(x13:[+,+]:1, x(vp(_44925,_44926,_44927)/_44930,_44922-_44923,_44934,vp(_44919,_44920)),
   [x(vp(_44925,_44926,[sbar(_44897,_44898)/_44901|_44927])/_44930,_44894-_44923,_44934,_44919),x(sbar(_44897,_44898)/_44901,_44922-_44894,nil,_44920)]).

parser_rule(x14:[-,-]:1, x(vp(fin,_44908,_44909)/_44912,pp/_44902-nil,v0(main,fin,_44908,[pp/_44902|_44909]-[])/_44912,vp(vp(v(vgap)),topic_gap)),
   []).

parser_rule(x14:[-,+]:1, x(vp(fin,_44918,_44919)/_44922,_44914-_44915,v0(main,fin,_44918,[pp/_44899|_44919]-[])/_44922,vp(vp(v(vgap)),_44896)),
   [x(pp/_44899,_44914-_44915,nil,_44896)]).

parser_rule(x14:[-,+]:2, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[np(_44911)/_44914,pp/_44907|_44934]-[])/_44937,vp(vp(topic_gap,vp(v(vgap))),_44904)),
   [x(pp/_44907,_44929-np(_44911)/_44914,nil,_44904)]).

parser_rule(x14:[-,+]:3, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[adv/_44910,pp/_44905|_44930]-[])/_44933,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(pp/_44905,_44925-adv/_44910,nil,_44902)]).

parser_rule(x14:[-,+]:4, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,pp/_44905|_44930]-[])/_44933,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(pp/_44905,_44925-pp/_44910,nil,_44902)]).

parser_rule(x14:[-,+]:5, x(vp(fin,_44933,_44934)/_44937,_44929-nil,v0(main,fin,_44933,[sbar(_44911)/_44914,pp/_44907|_44934]-[])/_44937,vp(vp(vp(v(vgap)),topic_gap),_44904)),
   [x(pp/_44907,_44929-sbar(_44911)/_44914,nil,_44904)]).

parser_rule(x14:[-,+]:6, x(vp(fin,_44935,_44936)/_44939,_44931-nil,v0(main,fin,_44935,[sbar(_44912,_44913)/_44916,pp/_44908|_44936]-[])/_44939,vp(vp(vp(v(vgap)),topic_gap),_44905)),
   [x(pp/_44908,_44931-sbar(_44912,_44913)/_44916,nil,_44905)]).

parser_rule(x14:[-,+]:7, x(vp(fin,_44929,_44930)/_44933,_44925-nil,v0(main,fin,_44929,[pp/_44910,pp/_44905|_44930]-[])/_44933,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(pp/_44905,_44925-pp/_44910,nil,_44902)]).

parser_rule(x14:[-,+]:8, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(pp/_44905,_44920-pp/_44924,nil,_44902)]).

parser_rule(x14:[-,+]:9, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(pp/_44905,_44920-adv/_44924,nil,_44902)]).

parser_rule(x14:[-,+]:10, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(pp/_44905,_44920-pp/_44924,nil,_44902)]).

parser_rule(x14:[-,+]:11, x(vp(fin,_44930,_44931)/[_44926,_44924],_44922-nil,v0(main,fin,_44930,[pp/_44907|_44931]-[])/_44924,vp(vp(vp(v(vgap)),topic_gap),_44904)),
   [x(pp/_44907,_44922-sbar(_44878)/_44926,nil,_44904)]).

parser_rule(x14:[-,+]:12, x(vp(fin,_44931,_44932)/[_44927,_44925],_44923-nil,v0(main,fin,_44931,[pp/_44908|_44932]-[])/_44925,vp(vp(vp(v(vgap)),topic_gap),_44905)),
   [x(pp/_44908,_44923-sbar(_44878,_44879)/_44927,nil,_44905)]).

parser_rule(x14:[+,-]:1, x(vp(_44909,_44910,_44911)/_44914,pp/_44904-_44907,_44918,vp(_44900,topic_gap)),
   [x(vp(_44909,_44910,[pp/_44904|_44911])/_44914,nil-_44907,_44918,_44900)]).

parser_rule(x14:[+,+]:1, x(vp(_44919,_44920,_44921)/_44924,_44916-_44917,_44928,vp(_44913,_44914)),
   [x(vp(_44919,_44920,[pp/_44895|_44921])/_44924,_44891-_44917,_44928,_44913),x(pp/_44895,_44916-_44891,nil,_44914)]).

parser_rule(x15:[-,-]:1, x(vp(fin,_44907,_44908)/[_44903,_44901],pp/_44903-nil,v0(main,fin,_44907,_44908-[])/_44901,vp(topic_gap,vp(v(vgap)))),
   []).

parser_rule(x15:[-,+]:1, x(vp(_44908,_44909,_44910)/[_44905,_44903],pp/_44905-_44902,_44917,vp(topic_gap,_44896)),
   [x(vp(_44908,_44909,_44910)/_44903,nil-_44902,_44917,_44896)]).

parser_rule(x15:[+,-]:1, x(vp(fin,_44917,_44918)/[_44913,_44911],_44909-_44910,v0(main,fin,_44917,_44918-[])/_44911,vp(_44895,vp(v(vgap)))),
   [x(pp/_44913,_44909-_44910,nil,_44895)]).

parser_rule(x15:[+,-]:2, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[np(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44928,_44924-np(_44906)/_44909,nil,_44903)]).

parser_rule(x15:[+,-]:3, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[adv/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44924,_44920-adv/_44905,nil,_44901)]).

parser_rule(x15:[+,-]:4, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44924,_44920-pp/_44905,nil,_44901)]).

parser_rule(x15:[+,-]:5, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[sbar(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44928,_44924-sbar(_44906)/_44909,nil,_44903)]).

parser_rule(x15:[+,-]:6, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[sbar(_44907,_44908)/_44911|_44935]-[])/_44928,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44930,_44926-sbar(_44907,_44908)/_44911,nil,_44904)]).

parser_rule(x15:[+,-]:7, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44924,_44920-pp/_44905,nil,_44901)]).

parser_rule(x15:[+,-]:8, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],_44915-nil,v0(main,fin,_44927,_44928-[])/_44917,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44923,_44915-pp/_44919,nil,_44901)]).

parser_rule(x15:[+,-]:9, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],_44915-nil,v0(main,fin,_44927,_44928-[])/_44917,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(pp/_44923,_44915-adv/_44919,nil,_44901)]).

parser_rule(x15:[+,-]:10, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],_44915-nil,v0(main,fin,_44927,_44928-[])/_44917,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44923,_44915-pp/_44919,nil,_44901)]).

parser_rule(x15:[+,-]:11, x(vp(fin,_44929,_44930)/[_44925,[_44921,_44919]],_44917-nil,v0(main,fin,_44929,_44930-[])/_44919,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44925,_44917-sbar(_44878)/_44921,nil,_44903)]).

parser_rule(x15:[+,-]:12, x(vp(fin,_44930,_44931)/[_44926,[_44922,_44920]],_44918-nil,v0(main,fin,_44930,_44931-[])/_44920,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(pp/_44926,_44918-sbar(_44878,_44879)/_44922,nil,_44904)]).

parser_rule(x15:[+,+]:1, x(vp(_44918,_44919,_44920)/[_44915,_44913],_44911-_44912,_44927,vp(_44908,_44909)),
   [x(pp/_44915,_44911-_44896,nil,_44908),x(vp(_44918,_44919,_44920)/_44913,_44896-_44912,_44927,_44909)]).

parser_rule(x16:[-,-]:1, x(vp(fin,_44907,_44908)/[_44903,_44901],adv/_44903-nil,v0(main,fin,_44907,_44908-[])/_44901,vp(topic_gap,vp(v(vgap)))),
   []).

parser_rule(x16:[-,+]:1, x(vp(_44908,_44909,_44910)/[_44905,_44903],adv/_44905-_44902,_44917,vp(topic_gap,_44896)),
   [x(vp(_44908,_44909,_44910)/_44903,nil-_44902,_44917,_44896)]).

parser_rule(x16:[+,-]:1, x(vp(fin,_44917,_44918)/[_44913,_44911],_44909-_44910,v0(main,fin,_44917,_44918-[])/_44911,vp(_44895,vp(v(vgap)))),
   [x(adv/_44913,_44909-_44910,nil,_44895)]).

parser_rule(x16:[+,-]:2, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[np(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44928,_44924-np(_44906)/_44909,nil,_44903)]).

parser_rule(x16:[+,-]:3, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[adv/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44924,_44920-adv/_44905,nil,_44901)]).

parser_rule(x16:[+,-]:4, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44924,_44920-pp/_44905,nil,_44901)]).

parser_rule(x16:[+,-]:5, x(vp(fin,_44932,_44933)/[_44928,_44926],_44924-nil,v0(main,fin,_44932,[sbar(_44906)/_44909|_44933]-[])/_44926,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44928,_44924-sbar(_44906)/_44909,nil,_44903)]).

parser_rule(x16:[+,-]:6, x(vp(fin,_44934,_44935)/[_44930,_44928],_44926-nil,v0(main,fin,_44934,[sbar(_44907,_44908)/_44911|_44935]-[])/_44928,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44930,_44926-sbar(_44907,_44908)/_44911,nil,_44904)]).

parser_rule(x16:[+,-]:7, x(vp(fin,_44928,_44929)/[_44924,_44922],_44920-nil,v0(main,fin,_44928,[pp/_44905|_44929]-[])/_44922,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44924,_44920-pp/_44905,nil,_44901)]).

parser_rule(x16:[+,-]:8, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],_44915-nil,v0(main,fin,_44927,_44928-[])/_44917,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44923,_44915-pp/_44919,nil,_44901)]).

parser_rule(x16:[+,-]:9, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],_44915-nil,v0(main,fin,_44927,_44928-[])/_44917,vp(_44901,vp(topic_gap,vp(v(vgap))))),
   [x(adv/_44923,_44915-adv/_44919,nil,_44901)]).

parser_rule(x16:[+,-]:10, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],_44915-nil,v0(main,fin,_44927,_44928-[])/_44917,vp(_44901,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44923,_44915-pp/_44919,nil,_44901)]).

parser_rule(x16:[+,-]:11, x(vp(fin,_44929,_44930)/[_44925,[_44921,_44919]],_44917-nil,v0(main,fin,_44929,_44930-[])/_44919,vp(_44903,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44925,_44917-sbar(_44878)/_44921,nil,_44903)]).

parser_rule(x16:[+,-]:12, x(vp(fin,_44930,_44931)/[_44926,[_44922,_44920]],_44918-nil,v0(main,fin,_44930,_44931-[])/_44920,vp(_44904,vp(vp(v(vgap)),topic_gap))),
   [x(adv/_44926,_44918-sbar(_44878,_44879)/_44922,nil,_44904)]).

parser_rule(x16:[+,+]:1, x(vp(_44918,_44919,_44920)/[_44915,_44913],_44911-_44912,_44927,vp(_44908,_44909)),
   [x(adv/_44915,_44911-_44896,nil,_44908),x(vp(_44918,_44919,_44920)/_44913,_44896-_44912,_44927,_44909)]).

parser_rule(x17:[-,-]:1, x(vp(fin,_44907,_44908)/[_44903,_44901],pp/_44903-nil,v0(main,fin,_44907,_44908-[])/_44901,vp(vp(v(vgap)),topic_gap)),
   []).

parser_rule(x17:[-,+]:1, x(vp(fin,_44917,_44918)/[_44913,_44911],_44909-_44910,v0(main,fin,_44917,_44918-[])/_44911,vp(vp(v(vgap)),_44896)),
   [x(pp/_44913,_44909-_44910,nil,_44896)]).

parser_rule(x17:[-,+]:2, x(vp(fin,_44932,_44933)/[_44928,_44926],np(_44919)/_44922-_44925,v0(main,fin,_44932,[np(_44919)/_44922|_44933]-[])/_44926,vp(vp(topic_gap,vp(v(vgap))),_44899)),
   [x(pp/_44928,nil-_44925,nil,_44899)]).

parser_rule(x17:[-,+]:3, x(vp(fin,_44928,_44929)/[_44924,_44922],adv/_44918-_44921,v0(main,fin,_44928,[adv/_44918|_44929]-[])/_44922,vp(vp(topic_gap,vp(v(vgap))),_44899)),
   [x(pp/_44924,nil-_44921,nil,_44899)]).

parser_rule(x17:[-,+]:4, x(vp(fin,_44928,_44929)/[_44924,_44922],pp/_44918-_44921,v0(main,fin,_44928,[pp/_44918|_44929]-[])/_44922,vp(vp(topic_gap,vp(v(vgap))),_44899)),
   [x(pp/_44924,nil-_44921,nil,_44899)]).

parser_rule(x17:[-,+]:5, x(vp(fin,_44932,_44933)/[_44928,_44926],sbar(_44919)/_44922-_44925,v0(main,fin,_44932,[sbar(_44919)/_44922|_44933]-[])/_44926,vp(vp(vp(v(vgap)),topic_gap),_44899)),
   [x(pp/_44928,nil-_44925,nil,_44899)]).

parser_rule(x17:[-,+]:6, x(vp(fin,_44934,_44935)/[_44930,_44928],sbar(_44920,_44921)/_44924-_44927,v0(main,fin,_44934,[sbar(_44920,_44921)/_44924|_44935]-[])/_44928,vp(vp(vp(v(vgap)),topic_gap),_44899)),
   [x(pp/_44930,nil-_44927,nil,_44899)]).

parser_rule(x17:[-,+]:7, x(vp(fin,_44928,_44929)/[_44924,_44922],pp/_44918-_44921,v0(main,fin,_44928,[pp/_44918|_44929]-[])/_44922,vp(vp(vp(v(vgap)),topic_gap),_44899)),
   [x(pp/_44924,nil-_44921,nil,_44899)]).

parser_rule(x17:[-,+]:8, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],pp/_44919-_44916,v0(main,fin,_44927,_44928-[])/_44917,vp(vp(topic_gap,vp(v(vgap))),_44899)),
   [x(pp/_44923,nil-_44916,nil,_44899)]).

parser_rule(x17:[-,+]:9, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],adv/_44919-_44916,v0(main,fin,_44927,_44928-[])/_44917,vp(vp(topic_gap,vp(v(vgap))),_44899)),
   [x(pp/_44923,nil-_44916,nil,_44899)]).

parser_rule(x17:[-,+]:10, x(vp(fin,_44927,_44928)/[_44923,[_44919,_44917]],pp/_44919-_44916,v0(main,fin,_44927,_44928-[])/_44917,vp(vp(vp(v(vgap)),topic_gap),_44899)),
   [x(pp/_44923,nil-_44916,nil,_44899)]).

parser_rule(x17:[-,+]:11, x(vp(fin,_44929,_44930)/[_44925,[_44921,_44919]],sbar(_44912)/_44921-_44918,v0(main,fin,_44929,_44930-[])/_44919,vp(vp(vp(v(vgap)),topic_gap),_44899)),
   [x(pp/_44925,nil-_44918,nil,_44899)]).

parser_rule(x17:[-,+]:12, x(vp(fin,_44930,_44931)/[_44926,[_44922,_44920]],sbar(_44912,_44913)/_44922-_44919,v0(main,fin,_44930,_44931-[])/_44920,vp(vp(vp(v(vgap)),topic_gap),_44899)),
   [x(pp/_44926,nil-_44919,nil,_44899)]).

parser_rule(x17:[+,-]:1, x(vp(_44908,_44909,_44910)/[_44905,_44903],_44901-nil,_44917,vp(_44898,topic_gap)),
   [x(vp(_44908,_44909,_44910)/_44903,_44901-pp/_44905,_44917,_44898)]).

parser_rule(x17:[+,+]:1, x(vp(_44918,_44919,_44920)/[_44915,_44913],_44911-_44912,_44927,vp(_44908,_44909)),
   [x(vp(_44918,_44919,_44920)/_44913,_44911-_44892,_44927,_44908),x(pp/_44915,_44892-_44912,nil,_44909)]).

parser_rule(x18:[-,-]:1, x(vp(fin,_44909,_44910)/[_44905,_44903],sbar(_44896)/_44905-nil,v0(main,fin,_44909,_44910-[])/_44903,vp(vp(v(vgap)),topic_gap)),
   []).

parser_rule(x18:[-,+]:1, x(vp(fin,_44919,_44920)/[_44915,_44913],_44911-_44912,v0(main,fin,_44919,_44920-[])/_44913,vp(vp(v(vgap)),_44898)),
   [x(sbar(_44881)/_44915,_44911-_44912,nil,_44898)]).

parser_rule(x18:[-,+]:2, x(vp(fin,_44934,_44935)/[_44930,_44928],np(_44921)/_44924-_44927,v0(main,fin,_44934,[np(_44921)/_44924|_44935]-[])/_44928,vp(vp(topic_gap,vp(v(vgap))),_44901)),
   [x(sbar(_44881)/_44930,nil-_44927,nil,_44901)]).

parser_rule(x18:[-,+]:3, x(vp(fin,_44930,_44931)/[_44926,_44924],adv/_44920-_44923,v0(main,fin,_44930,[adv/_44920|_44931]-[])/_44924,vp(vp(topic_gap,vp(v(vgap))),_44901)),
   [x(sbar(_44881)/_44926,nil-_44923,nil,_44901)]).

parser_rule(x18:[-,+]:4, x(vp(fin,_44930,_44931)/[_44926,_44924],pp/_44920-_44923,v0(main,fin,_44930,[pp/_44920|_44931]-[])/_44924,vp(vp(topic_gap,vp(v(vgap))),_44901)),
   [x(sbar(_44881)/_44926,nil-_44923,nil,_44901)]).

parser_rule(x18:[-,+]:5, x(vp(fin,_44934,_44935)/[_44930,_44928],sbar(_44921)/_44924-_44927,v0(main,fin,_44934,[sbar(_44921)/_44924|_44935]-[])/_44928,vp(vp(vp(v(vgap)),topic_gap),_44901)),
   [x(sbar(_44881)/_44930,nil-_44927,nil,_44901)]).

parser_rule(x18:[-,+]:6, x(vp(fin,_44936,_44937)/[_44932,_44930],sbar(_44922,_44923)/_44926-_44929,v0(main,fin,_44936,[sbar(_44922,_44923)/_44926|_44937]-[])/_44930,vp(vp(vp(v(vgap)),topic_gap),_44901)),
   [x(sbar(_44881)/_44932,nil-_44929,nil,_44901)]).

parser_rule(x18:[-,+]:7, x(vp(fin,_44930,_44931)/[_44926,_44924],pp/_44920-_44923,v0(main,fin,_44930,[pp/_44920|_44931]-[])/_44924,vp(vp(vp(v(vgap)),topic_gap),_44901)),
   [x(sbar(_44881)/_44926,nil-_44923,nil,_44901)]).

parser_rule(x18:[-,+]:8, x(vp(fin,_44929,_44930)/[_44925,[_44921,_44919]],pp/_44921-_44918,v0(main,fin,_44929,_44930-[])/_44919,vp(vp(topic_gap,vp(v(vgap))),_44901)),
   [x(sbar(_44881)/_44925,nil-_44918,nil,_44901)]).

parser_rule(x18:[-,+]:9, x(vp(fin,_44929,_44930)/[_44925,[_44921,_44919]],adv/_44921-_44918,v0(main,fin,_44929,_44930-[])/_44919,vp(vp(topic_gap,vp(v(vgap))),_44901)),
   [x(sbar(_44881)/_44925,nil-_44918,nil,_44901)]).

parser_rule(x18:[-,+]:10, x(vp(fin,_44929,_44930)/[_44925,[_44921,_44919]],pp/_44921-_44918,v0(main,fin,_44929,_44930-[])/_44919,vp(vp(vp(v(vgap)),topic_gap),_44901)),
   [x(sbar(_44881)/_44925,nil-_44918,nil,_44901)]).

parser_rule(x18:[-,+]:11, x(vp(fin,_44931,_44932)/[_44927,[_44923,_44921]],sbar(_44914)/_44923-_44920,v0(main,fin,_44931,_44932-[])/_44921,vp(vp(vp(v(vgap)),topic_gap),_44901)),
   [x(sbar(_44881)/_44927,nil-_44920,nil,_44901)]).

parser_rule(x18:[-,+]:12, x(vp(fin,_44932,_44933)/[_44928,[_44924,_44922]],sbar(_44914,_44915)/_44924-_44921,v0(main,fin,_44932,_44933-[])/_44922,vp(vp(vp(v(vgap)),topic_gap),_44901)),
   [x(sbar(_44881)/_44928,nil-_44921,nil,_44901)]).

parser_rule(x18:[+,-]:1, x(vp(_44910,_44911,_44912)/[_44907,_44905],_44903-nil,_44919,vp(_44900,topic_gap)),
   [x(vp(_44910,_44911,_44912)/_44905,_44903-sbar(_44878)/_44907,_44919,_44900)]).

parser_rule(x18:[+,+]:1, x(vp(_44920,_44921,_44922)/[_44917,_44915],_44913-_44914,_44929,vp(_44910,_44911)),
   [x(vp(_44920,_44921,_44922)/_44915,_44913-_44894,_44929,_44910),x(sbar(_44881)/_44917,_44894-_44914,nil,_44911)]).

parser_rule(x19:[-,-]:1, x(vp(fin,_44910,_44911)/[_44906,_44904],sbar(_44896,_44897)/_44906-nil,v0(main,fin,_44910,_44911-[])/_44904,vp(vp(v(vgap)),topic_gap)),
   []).

parser_rule(x19:[-,+]:1, x(vp(fin,_44920,_44921)/[_44916,_44914],_44912-_44913,v0(main,fin,_44920,_44921-[])/_44914,vp(vp(v(vgap)),_44899)),
   [x(sbar(_44881,_44882)/_44916,_44912-_44913,nil,_44899)]).

parser_rule(x19:[-,+]:2, x(vp(fin,_44935,_44936)/[_44931,_44929],np(_44922)/_44925-_44928,v0(main,fin,_44935,[np(_44922)/_44925|_44936]-[])/_44929,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(sbar(_44881,_44882)/_44931,nil-_44928,nil,_44902)]).

parser_rule(x19:[-,+]:3, x(vp(fin,_44931,_44932)/[_44927,_44925],adv/_44921-_44924,v0(main,fin,_44931,[adv/_44921|_44932]-[])/_44925,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(sbar(_44881,_44882)/_44927,nil-_44924,nil,_44902)]).

parser_rule(x19:[-,+]:4, x(vp(fin,_44931,_44932)/[_44927,_44925],pp/_44921-_44924,v0(main,fin,_44931,[pp/_44921|_44932]-[])/_44925,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(sbar(_44881,_44882)/_44927,nil-_44924,nil,_44902)]).

parser_rule(x19:[-,+]:5, x(vp(fin,_44935,_44936)/[_44931,_44929],sbar(_44922)/_44925-_44928,v0(main,fin,_44935,[sbar(_44922)/_44925|_44936]-[])/_44929,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(sbar(_44881,_44882)/_44931,nil-_44928,nil,_44902)]).

parser_rule(x19:[-,+]:6, x(vp(fin,_44937,_44938)/[_44933,_44931],sbar(_44923,_44924)/_44927-_44930,v0(main,fin,_44937,[sbar(_44923,_44924)/_44927|_44938]-[])/_44931,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(sbar(_44881,_44882)/_44933,nil-_44930,nil,_44902)]).

parser_rule(x19:[-,+]:7, x(vp(fin,_44931,_44932)/[_44927,_44925],pp/_44921-_44924,v0(main,fin,_44931,[pp/_44921|_44932]-[])/_44925,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(sbar(_44881,_44882)/_44927,nil-_44924,nil,_44902)]).

parser_rule(x19:[-,+]:8, x(vp(fin,_44930,_44931)/[_44926,[_44922,_44920]],pp/_44922-_44919,v0(main,fin,_44930,_44931-[])/_44920,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(sbar(_44881,_44882)/_44926,nil-_44919,nil,_44902)]).

parser_rule(x19:[-,+]:9, x(vp(fin,_44930,_44931)/[_44926,[_44922,_44920]],adv/_44922-_44919,v0(main,fin,_44930,_44931-[])/_44920,vp(vp(topic_gap,vp(v(vgap))),_44902)),
   [x(sbar(_44881,_44882)/_44926,nil-_44919,nil,_44902)]).

parser_rule(x19:[-,+]:10, x(vp(fin,_44930,_44931)/[_44926,[_44922,_44920]],pp/_44922-_44919,v0(main,fin,_44930,_44931-[])/_44920,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(sbar(_44881,_44882)/_44926,nil-_44919,nil,_44902)]).

parser_rule(x19:[-,+]:11, x(vp(fin,_44932,_44933)/[_44928,[_44924,_44922]],sbar(_44915)/_44924-_44921,v0(main,fin,_44932,_44933-[])/_44922,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(sbar(_44881,_44882)/_44928,nil-_44921,nil,_44902)]).

parser_rule(x19:[-,+]:12, x(vp(fin,_44933,_44934)/[_44929,[_44925,_44923]],sbar(_44915,_44916)/_44925-_44922,v0(main,fin,_44933,_44934-[])/_44923,vp(vp(vp(v(vgap)),topic_gap),_44902)),
   [x(sbar(_44881,_44882)/_44929,nil-_44922,nil,_44902)]).

parser_rule(x19:[+,-]:1, x(vp(_44911,_44912,_44913)/[_44908,_44906],_44904-nil,_44920,vp(_44901,topic_gap)),
   [x(vp(_44911,_44912,_44913)/_44906,_44904-sbar(_44878,_44879)/_44908,_44920,_44901)]).

parser_rule(x19:[+,+]:1, x(vp(_44921,_44922,_44923)/[_44918,_44916],_44914-_44915,_44930,vp(_44911,_44912)),
   [x(vp(_44921,_44922,_44923)/_44916,_44914-_44895,_44930,_44911),x(sbar(_44881,_44882)/_44918,_44895-_44915,nil,_44912)]).

parser_rule(x20:[-]:1, x(vp(fin,_44897,_44898)/_44901,_44894-_44894,v0(main,fin,_44897,_44898-[])/_44901,vp(v(vgap))),
   []).

parser_rule(x20:[+]:1, x(vp(_44903,_44904,_44905)/_44908,_44900-_44901,_44912,vp(_44898)),
   [x(v(_44903,_44904,_44905-[])/_44908,_44900-_44901,_44912,_44898)]).

parser_rule(x21:[-]:1, x(v(fin,_44892,_44893)/_44896,_44889-_44889,v0(main,fin,_44892,_44893)/_44896,v(vgap)),
   []).

parser_rule(x21:[+]:1, x(v(_44901,_44902,_44903)/_44906,_44898-_44899,_44910,v(_44896)),
   [x(v0(main,_44901,_44902,_44903)/_44906,_44898-_44899,_44910,_44896)]).

parser_rule(x22:[-,+]:1, x(v(fin,_44920,_44921)/_44924,_44916-_44917,v0(aux,fin,_44920,v(_44898,_44899,_44900)/_44903+_44921)/_44924,v(vgap,_44896)),
   [x(v(_44898,_44899,_44900)/_44903,_44916-_44917,nil,_44896)]).

parser_rule(x22:[+,+]:1, x(v(_44929,_44930,_44931)/_44934,_44926-_44927,_44938,v(_44923,_44924)),
   [x(v0(aux,_44929,_44930,v(_44898,_44899,_44900)/_44903+_44931)/_44934,_44926-_44896,_44938,_44923),x(v(_44898,_44899,_44900)/_44903,_44896-_44927,nil,_44924)]).

parser_rule(x23:[]:1, x(v0(_44889,fin,_44891,_44892)/_44895,_44887-_44887,v0(_44889,fin,_44891,_44892)/_44895,vgap),
   []).

parser_rule(x24:[-,+]:1, x(np(pl)/_44904,_44899-_44899,nil,np(det(e),_44896)),
   [x(n(pl)/_44904,nil-nil,nil,_44896)]).

parser_rule(x24:[+,+]:1, x(np(_44915)/_44918,_44913-_44913,nil,np(_44909,_44910)),
   [x(det(_44896,_44915)/_44918,nil-nil,nil,_44909),x(n(_44915)/_44896,nil-nil,nil,_44910)]).

parser_rule(x25:[+,-]:1, x(n(_44906)/[_44903,_44901],_44899-nil,nil,n(_44896,topic_gap)),
   [x(n(_44906)/_44901,_44899-pp/_44903,nil,_44896)]).

parser_rule(x25:[+,+]:1, x(n(_44916)/[_44913,_44911],_44909-_44910,nil,n(_44906,_44907)),
   [x(n(_44916)/_44911,_44909-_44892,nil,_44906),x(pp/_44913,_44892-_44910,nil,_44907)]).

parser_rule(x26:[+,+]:1, x(pp/_44915,_44912-_44912,nil,pp(_44908,_44909)),
   [x(p(_44896)/_44915,_44894-_44894,nil,_44908),x(np(_44881)/_44896,_44879-_44879,nil,_44909)]).

parser_rule(x27:[]:1, x(np(_44886)/_44889,np(_44886)/_44889-nil,nil,topic_gap),
   []).

parser_rule(x28:[]:1, x(pp/_44885,pp/_44885-nil,nil,topic_gap),
   []).

parser_rule(x29:[]:1, x(sbar(_44886)/_44889,sbar(_44886)/_44889-nil,nil,topic_gap),
   []).

parser_rule(x30:[]:1, x(sbar(_44887,_44888)/_44891,sbar(_44887,_44888)/_44891-nil,nil,topic_gap),
   []).

parser_rule(x31:[]:1, x(adv/_44885,adv/_44885-nil,nil,topic_gap),
   []).

parser_rule(x32:[]:1, x(det(_44887,pl)/_44887,_44881-_44881,nil,det(e)),
   []).

parser_rule(x33:[]:1, x(comp(infin,_44885)/_44885,_44879-_44879,nil,comp),
   []).

first_member_index(jan0,
    [x0:[+,+,-]:1,x0:[+,+,+]:1,x7:[+,-]:1,x7:[+,-]:2,x7:[+,-]:3,x7:[+,-]:4,x7:[+,-]:5,x7:[+,-]:6,x7:[+,-]:7,x7:[+,-]:8,x7:[+,-]:9,x7:[+,-]:10,x7:[+,-]:11,x7:[+,-]:12,x7:[+,+]:1,x8:[+,-]:1,x8:[+,-]:2,x8:[+,-]:3,x8:[+,-]:4,x8:[+,-]:5,x8:[+,-]:6,x8:[+,-]:7,x8:[+,-]:8,x8:[+,-]:9,x8:[+,-]:10,x8:[+,-]:11,x8:[+,-]:12,x8:[+,+]:1]).
nonfirst_member_index(jan0,
    [(x26:[+,+]:1)-2]).

first_member_index(marie0,
    [x0:[+,+,-]:1,x0:[+,+,+]:1,x7:[+,-]:1,x7:[+,-]:2,x7:[+,-]:3,x7:[+,-]:4,x7:[+,-]:5,x7:[+,-]:6,x7:[+,-]:7,x7:[+,-]:8,x7:[+,-]:9,x7:[+,-]:10,x7:[+,-]:11,x7:[+,-]:12,x7:[+,+]:1,x8:[+,-]:1,x8:[+,-]:2,x8:[+,-]:3,x8:[+,-]:4,x8:[+,-]:5,x8:[+,-]:6,x8:[+,-]:7,x8:[+,-]:8,x8:[+,-]:9,x8:[+,-]:10,x8:[+,-]:11,x8:[+,-]:12,x8:[+,+]:1]).
nonfirst_member_index(marie0,
    [(x26:[+,+]:1)-2]).

first_member_index(jane0,
    [x0:[+,+,-]:1,x0:[+,+,+]:1,x7:[+,-]:1,x7:[+,-]:2,x7:[+,-]:3,x7:[+,-]:4,x7:[+,-]:5,x7:[+,-]:6,x7:[+,-]:7,x7:[+,-]:8,x7:[+,-]:9,x7:[+,-]:10,x7:[+,-]:11,x7:[+,-]:12,x7:[+,+]:1,x8:[+,-]:1,x8:[+,-]:2,x8:[+,-]:3,x8:[+,-]:4,x8:[+,-]:5,x8:[+,-]:6,x8:[+,-]:7,x8:[+,-]:8,x8:[+,-]:9,x8:[+,-]:10,x8:[+,-]:11,x8:[+,-]:12,x8:[+,+]:1]).
nonfirst_member_index(jane0,
    [(x26:[+,+]:1)-2]).

first_member_index(tarzan0,
    [x0:[+,+,-]:1,x0:[+,+,+]:1,x7:[+,-]:1,x7:[+,-]:2,x7:[+,-]:3,x7:[+,-]:4,x7:[+,-]:5,x7:[+,-]:6,x7:[+,-]:7,x7:[+,-]:8,x7:[+,-]:9,x7:[+,-]:10,x7:[+,-]:11,x7:[+,-]:12,x7:[+,+]:1,x8:[+,-]:1,x8:[+,-]:2,x8:[+,-]:3,x8:[+,-]:4,x8:[+,-]:5,x8:[+,-]:6,x8:[+,-]:7,x8:[+,-]:8,x8:[+,-]:9,x8:[+,-]:10,x8:[+,-]:11,x8:[+,-]:12,x8:[+,+]:1]).
nonfirst_member_index(tarzan0,
    [(x26:[+,+]:1)-2]).

first_member_index(man0,
    [x25:[+,-]:1,x25:[+,+]:1]).
nonfirst_member_index(man0,
    [(x24:[+,+]:1)-2]).

first_member_index(mannen0,
    [x24:[-,+]:1,x25:[+,-]:1,x25:[+,+]:1]).
nonfirst_member_index(mannen0,
    [(x24:[+,+]:1)-2]).

first_member_index(vrouw0,
    [x25:[+,-]:1,x25:[+,+]:1]).
nonfirst_member_index(vrouw0,
    [(x24:[+,+]:1)-2]).

first_member_index(vrouwen0,
    [x24:[-,+]:1,x25:[+,-]:1,x25:[+,+]:1]).
nonfirst_member_index(vrouwen0,
    [(x24:[+,+]:1)-2]).

first_member_index(puree0,
    [x25:[+,-]:1,x25:[+,+]:1]).
nonfirst_member_index(puree0,
    [(x24:[+,+]:1)-2]).

first_member_index(de0,
    [x24:[+,+]:1]).
nonfirst_member_index(de0,
    []).

first_member_index(naar0,
    [x26:[+,+]:1]).
nonfirst_member_index(naar0,
    []).

first_member_index(met0,
    [x26:[+,+]:1]).
nonfirst_member_index(met0,
    []).

first_member_index(in0,
    [x26:[+,+]:1]).
nonfirst_member_index(in0,
    []).

first_member_index(van0,
    [x26:[+,+]:1]).
nonfirst_member_index(van0,
    []).

first_member_index(aan0,
    [x26:[+,+]:1]).
nonfirst_member_index(aan0,
    []).

first_member_index(op0,
    [x9:[+,-]:1,x9:[+,-]:2,x9:[+,-]:3,x9:[+,-]:4,x9:[+,-]:5,x9:[+,-]:6,x9:[+,-]:7,x9:[+,-]:8,x9:[+,-]:9,x9:[+,-]:10,x9:[+,-]:11,x9:[+,-]:12,x9:[+,+]:1]).
nonfirst_member_index(op0,
    []).

first_member_index(dat0,
    [x5:[+,+]:1]).
nonfirst_member_index(dat0,
    []).

first_member_index(omdat0,
    [x5:[+,+]:1]).
nonfirst_member_index(omdat0,
    []).

first_member_index(om0,
    [x6:[+,+]:1]).
nonfirst_member_index(om0,
    []).

first_member_index(vandaag0,
    [x4:[+,+,+]:1,x10:[+,-]:1,x10:[+,-]:2,x10:[+,-]:3,x10:[+,-]:4,x10:[+,-]:5,x10:[+,-]:6,x10:[+,-]:7,x10:[+,-]:8,x10:[+,-]:9,x10:[+,-]:10,x10:[+,-]:11,x10:[+,-]:12,x10:[+,+]:1,x16:[+,-]:1,x16:[+,-]:2,x16:[+,-]:3,x16:[+,-]:4,x16:[+,-]:5,x16:[+,-]:6,x16:[+,-]:7,x16:[+,-]:8,x16:[+,-]:9,x16:[+,-]:10,x16:[+,-]:11,x16:[+,-]:12,x16:[+,+]:1]).
nonfirst_member_index(vandaag0,
    []).

first_member_index(slaapt0,
    [x21:[+]:1]).
nonfirst_member_index(slaapt0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(slapen0,
    [x21:[+]:1]).
nonfirst_member_index(slapen0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(slapen1,
    [x21:[+]:1]).
nonfirst_member_index(slapen1,
    []).

first_member_index(geslapen0,
    [x21:[+]:1]).
nonfirst_member_index(geslapen0,
    []).

first_member_index(vangt0,
    [x21:[+]:1]).
nonfirst_member_index(vangt0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(vangen0,
    [x21:[+]:1]).
nonfirst_member_index(vangen0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(vangen1,
    [x21:[+]:1]).
nonfirst_member_index(vangen1,
    []).

first_member_index(gevangen0,
    [x21:[+]:1]).
nonfirst_member_index(gevangen0,
    []).

first_member_index(zit0,
    [x21:[+]:1]).
nonfirst_member_index(zit0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zitten0,
    [x21:[+]:1]).
nonfirst_member_index(zitten0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zitten1,
    [x21:[+]:1]).
nonfirst_member_index(zitten1,
    []).

first_member_index(gezeten0,
    [x21:[+]:1]).
nonfirst_member_index(gezeten0,
    []).

first_member_index(kust0,
    [x21:[+]:1]).
nonfirst_member_index(kust0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(kussen0,
    [x21:[+]:1]).
nonfirst_member_index(kussen0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(kussen1,
    [x21:[+]:1]).
nonfirst_member_index(kussen1,
    []).

first_member_index(gekust0,
    [x21:[+]:1]).
nonfirst_member_index(gekust0,
    []).

first_member_index(zegt0,
    [x21:[+]:1]).
nonfirst_member_index(zegt0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zeggen0,
    [x21:[+]:1]).
nonfirst_member_index(zeggen0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zeggen1,
    [x21:[+]:1]).
nonfirst_member_index(zeggen1,
    []).

first_member_index(gezegd0,
    [x21:[+]:1]).
nonfirst_member_index(gezegd0,
    []).

first_member_index(dwingt0,
    [x21:[+]:1]).
nonfirst_member_index(dwingt0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(dwingen0,
    [x21:[+]:1]).
nonfirst_member_index(dwingen0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(dwingen1,
    [x21:[+]:1]).
nonfirst_member_index(dwingen1,
    []).

first_member_index(gedwongen0,
    [x21:[+]:1]).
nonfirst_member_index(gedwongen0,
    []).

first_member_index(probeert0,
    [x21:[+]:1]).
nonfirst_member_index(probeert0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(proberen0,
    [x21:[+]:1]).
nonfirst_member_index(proberen0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(proberen1,
    [x21:[+]:1]).
nonfirst_member_index(proberen1,
    []).

first_member_index(geprobeerd0,
    [x21:[+]:1]).
nonfirst_member_index(geprobeerd0,
    []).

first_member_index(geeft0,
    [x21:[+]:1]).
nonfirst_member_index(geeft0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(geven0,
    [x21:[+]:1]).
nonfirst_member_index(geven0,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(geven1,
    [x21:[+]:1]).
nonfirst_member_index(geven1,
    []).

first_member_index(gegeven0,
    [x21:[+]:1]).
nonfirst_member_index(gegeven0,
    []).

first_member_index(geeft1,
    [x21:[+]:1]).
nonfirst_member_index(geeft1,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(geven2,
    [x21:[+]:1]).
nonfirst_member_index(geven2,
    [(x0:[+,+,-]:1)-2,(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(geven3,
    [x21:[+]:1]).
nonfirst_member_index(geven3,
    []).

first_member_index(gegeven1,
    [x21:[+]:1]).
nonfirst_member_index(gegeven1,
    []).

first_member_index(heeft0,
    [x22:[+,+]:1]).
nonfirst_member_index(heeft0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(hebben0,
    [x22:[+,+]:1]).
nonfirst_member_index(hebben0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(hebben1,
    [x22:[+,+]:1]).
nonfirst_member_index(hebben1,
    []).

first_member_index(gehad0,
    [x22:[+,+]:1]).
nonfirst_member_index(gehad0,
    []).

first_member_index(zal0,
    [x22:[+,+]:1]).
nonfirst_member_index(zal0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zullen0,
    [x22:[+,+]:1]).
nonfirst_member_index(zullen0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zullen1,
    [x22:[+,+]:1]).
nonfirst_member_index(zullen1,
    []).

first_member_index(te0,
    [x22:[+,+]:1]).
nonfirst_member_index(te0,
    []).

first_member_index(probeert1,
    [x22:[+,+]:1]).
nonfirst_member_index(probeert1,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(proberen2,
    [x22:[+,+]:1]).
nonfirst_member_index(proberen2,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(proberen3,
    [x22:[+,+]:1]).
nonfirst_member_index(proberen3,
    []).

first_member_index(proberen4,
    [x22:[+,+]:1]).
nonfirst_member_index(proberen4,
    []).

first_member_index(ziet0,
    [x22:[+,+]:1]).
nonfirst_member_index(ziet0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zien0,
    [x22:[+,+]:1]).
nonfirst_member_index(zien0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(zien1,
    [x22:[+,+]:1]).
nonfirst_member_index(zien1,
    []).

first_member_index(zien2,
    [x22:[+,+]:1]).
nonfirst_member_index(zien2,
    []).

first_member_index(helpt0,
    [x22:[+,+]:1]).
nonfirst_member_index(helpt0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(helpen0,
    [x22:[+,+]:1]).
nonfirst_member_index(helpen0,
    [(x0:[+,+,+]:1)-2,(x1:[+,+,+]:1)-2,(x2:[+,+,+]:1)-2,(x3:[+,+,+]:1)-2,(x4:[+,+,+]:1)-2]).

first_member_index(helpen1,
    [x22:[+,+]:1]).
nonfirst_member_index(helpen1,
    []).

first_member_index(helpen2,
    [x22:[+,+]:1]).
nonfirst_member_index(helpen2,
    []).

first_member_index(x0:[+,+,-]:1,
    []).
nonfirst_member_index(x0:[+,+,-]:1,
    []).

first_member_index(x0:[+,+,+]:1,
    []).
nonfirst_member_index(x0:[+,+,+]:1,
    []).

first_member_index(x1:[+,+,+]:1,
    []).
nonfirst_member_index(x1:[+,+,+]:1,
    []).

first_member_index(x2:[+,+,+]:1,
    []).
nonfirst_member_index(x2:[+,+,+]:1,
    []).

first_member_index(x3:[+,+,+]:1,
    []).
nonfirst_member_index(x3:[+,+,+]:1,
    []).

first_member_index(x4:[+,+,+]:1,
    []).
nonfirst_member_index(x4:[+,+,+]:1,
    []).

first_member_index(x5:[+,+]:1,
    [x2:[+,+,+]:1,x12:[-,+]:1,x12:[-,+]:2,x12:[-,+]:3,x12:[-,+]:4,x12:[-,+]:5,x12:[-,+]:6,x12:[-,+]:7,x12:[-,+]:8,x12:[-,+]:9,x12:[-,+]:10,x12:[-,+]:11,x12:[-,+]:12,x18:[-,+]:1,x18:[-,+]:2,x18:[-,+]:3,x18:[-,+]:4,x18:[-,+]:5,x18:[-,+]:6,x18:[-,+]:7,x18:[-,+]:8,x18:[-,+]:9,x18:[-,+]:10,x18:[-,+]:11,x18:[-,+]:12]).
nonfirst_member_index(x5:[+,+]:1,
    [(x12:[+,+]:1)-2,(x18:[+,+]:1)-2]).

first_member_index(x6:[-,+]:1,
    [x3:[+,+,+]:1,x13:[-,+]:1,x13:[-,+]:2,x13:[-,+]:3,x13:[-,+]:4,x13:[-,+]:5,x13:[-,+]:6,x13:[-,+]:7,x13:[-,+]:8,x13:[-,+]:9,x13:[-,+]:10,x13:[-,+]:11,x13:[-,+]:12,x19:[-,+]:1,x19:[-,+]:2,x19:[-,+]:3,x19:[-,+]:4,x19:[-,+]:5,x19:[-,+]:6,x19:[-,+]:7,x19:[-,+]:8,x19:[-,+]:9,x19:[-,+]:10,x19:[-,+]:11,x19:[-,+]:12]).
nonfirst_member_index(x6:[-,+]:1,
    [(x13:[+,+]:1)-2,(x19:[+,+]:1)-2]).

first_member_index(x6:[+,+]:1,
    [x3:[+,+,+]:1,x13:[-,+]:1,x13:[-,+]:2,x13:[-,+]:3,x13:[-,+]:4,x13:[-,+]:5,x13:[-,+]:6,x13:[-,+]:7,x13:[-,+]:8,x13:[-,+]:9,x13:[-,+]:10,x13:[-,+]:11,x13:[-,+]:12,x19:[-,+]:1,x19:[-,+]:2,x19:[-,+]:3,x19:[-,+]:4,x19:[-,+]:5,x19:[-,+]:6,x19:[-,+]:7,x19:[-,+]:8,x19:[-,+]:9,x19:[-,+]:10,x19:[-,+]:11,x19:[-,+]:12]).
nonfirst_member_index(x6:[+,+]:1,
    [(x13:[+,+]:1)-2,(x19:[+,+]:1)-2]).

first_member_index(x7:[-,-]:1,
    []).
nonfirst_member_index(x7:[-,-]:1,
    []).

first_member_index(x7:[-,+]:1,
    []).
nonfirst_member_index(x7:[-,+]:1,
    [(x0:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:1,
    []).
nonfirst_member_index(x7:[+,-]:1,
    []).

first_member_index(x7:[+,-]:2,
    []).
nonfirst_member_index(x7:[+,-]:2,
    [(x0:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:3,
    []).
nonfirst_member_index(x7:[+,-]:3,
    [(x4:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:4,
    []).
nonfirst_member_index(x7:[+,-]:4,
    [(x1:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:5,
    []).
nonfirst_member_index(x7:[+,-]:5,
    [(x2:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:6,
    []).
nonfirst_member_index(x7:[+,-]:6,
    [(x3:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:7,
    []).
nonfirst_member_index(x7:[+,-]:7,
    [(x1:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:8,
    []).
nonfirst_member_index(x7:[+,-]:8,
    [(x1:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:9,
    []).
nonfirst_member_index(x7:[+,-]:9,
    [(x4:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:10,
    []).
nonfirst_member_index(x7:[+,-]:10,
    [(x1:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:11,
    []).
nonfirst_member_index(x7:[+,-]:11,
    [(x2:[+,+,+]:1)-3]).

first_member_index(x7:[+,-]:12,
    []).
nonfirst_member_index(x7:[+,-]:12,
    [(x3:[+,+,+]:1)-3]).

first_member_index(x7:[+,+]:1,
    []).
nonfirst_member_index(x7:[+,+]:1,
    [(x0:[+,+,+]:1)-3,(x4:[+,+,+]:1)-3,(x1:[+,+,+]:1)-3,(x2:[+,+,+]:1)-3,(x3:[+,+,+]:1)-3,(x5:[+,+]:1)-2]).

first_member_index(x8:[-,-]:1,
    []).
nonfirst_member_index(x8:[-,-]:1,
    []).

first_member_index(x8:[-,+]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,-]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x8:[+,-]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x8:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x8:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x9:[+,-]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,-]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x9:[+,-]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x9:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x9:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x10:[-,-]:1,
    []).
nonfirst_member_index(x10:[-,-]:1,
    []).

first_member_index(x10:[-,+]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,-]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x10:[+,-]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x10:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x10:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x11:[-,-]:1,
    []).
nonfirst_member_index(x11:[-,-]:1,
    []).

first_member_index(x11:[-,+]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,-]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x11:[+,-]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x11:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x11:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x12:[-,-]:1,
    []).
nonfirst_member_index(x12:[-,-]:1,
    []).

first_member_index(x12:[-,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[-,+]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[-,+]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[+,-]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x12:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x12:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x12:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x13:[-,-]:1,
    []).
nonfirst_member_index(x13:[-,-]:1,
    []).

first_member_index(x13:[-,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[-,+]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[-,+]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[+,-]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x13:[+,+]:1,
    [x6:[-,+]:1,x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x13:[+,+]:1,
    [(x6:[+,+]:1)-2,(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,-]:1,
    []).
nonfirst_member_index(x14:[-,-]:1,
    []).

first_member_index(x14:[-,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[-,+]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[-,+]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[+,-]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x14:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x14:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x14:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x15:[-,-]:1,
    []).
nonfirst_member_index(x15:[-,-]:1,
    []).

first_member_index(x15:[-,+]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,-]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x15:[+,-]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x15:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x15:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x16:[-,-]:1,
    []).
nonfirst_member_index(x16:[-,-]:1,
    []).

first_member_index(x16:[-,+]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,-]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x16:[+,-]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x16:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x16:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x17:[-,-]:1,
    []).
nonfirst_member_index(x17:[-,-]:1,
    []).

first_member_index(x17:[-,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[-,+]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[-,+]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[+,-]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x17:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x17:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x17:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x18:[-,-]:1,
    []).
nonfirst_member_index(x18:[-,-]:1,
    []).

first_member_index(x18:[-,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[-,+]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[-,+]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[+,-]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x18:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x18:[+,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x18:[+,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x19:[-,-]:1,
    []).
nonfirst_member_index(x19:[-,-]:1,
    []).

first_member_index(x19:[-,+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:2,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:2,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:3,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:3,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:4,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:4,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:5,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:5,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:6,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:6,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:7,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:7,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:8,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:8,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:9,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:9,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:10,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:10,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:11,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:11,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[-,+]:12,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[-,+]:12,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[+,-]:1,
    [x12:[+,+]:1,x13:[+,+]:1,x14:[+,+]:1,x17:[+,+]:1,x18:[+,+]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[+,-]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x19:[+,+]:1,
    [x6:[-,+]:1,x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1]).
nonfirst_member_index(x19:[+,+]:1,
    [(x6:[+,+]:1)-2,(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2]).

first_member_index(x20:[-]:1,
    []).
nonfirst_member_index(x20:[-]:1,
    []).

first_member_index(x20:[+]:1,
    [x7:[-,+]:1,x8:[-,+]:1,x10:[-,+]:1,x11:[-,+]:1,x12:[+,-]:1,x12:[+,+]:1,x13:[+,-]:1,x13:[+,+]:1,x14:[+,-]:1,x14:[+,+]:1,x15:[-,+]:1,x16:[-,+]:1,x17:[+,-]:1,x17:[+,+]:1,x18:[+,-]:1,x18:[+,+]:1,x19:[+,-]:1,x19:[+,+]:1,x6:[-,+]:1]).
nonfirst_member_index(x20:[+]:1,
    [(x7:[+,+]:1)-2,(x8:[+,+]:1)-2,(x9:[+,+]:1)-2,(x10:[+,+]:1)-2,(x11:[+,+]:1)-2,(x15:[+,+]:1)-2,(x16:[+,+]:1)-2,(x6:[+,+]:1)-2]).

first_member_index(x21:[-]:1,
    []).
nonfirst_member_index(x21:[-]:1,
    []).

first_member_index(x21:[+]:1,
    [x20:[+]:1,x22:[-,+]:1]).
nonfirst_member_index(x21:[+]:1,
    [(x22:[+,+]:1)-2]).

first_member_index(x22:[-,+]:1,
    [x20:[+]:1]).
nonfirst_member_index(x22:[-,+]:1,
    []).

first_member_index(x22:[+,+]:1,
    [x20:[+]:1,x22:[-,+]:1]).
nonfirst_member_index(x22:[+,+]:1,
    [(x22:[+,+]:1)-2]).

first_member_index(x23:[]:1,
    []).
nonfirst_member_index(x23:[]:1,
    []).

first_member_index(x24:[-,+]:1,
    [x0:[+,+,-]:1,x0:[+,+,+]:1,x7:[+,-]:1,x7:[+,-]:2,x7:[+,-]:3,x7:[+,-]:4,x7:[+,-]:5,x7:[+,-]:6,x7:[+,-]:7,x7:[+,-]:8,x7:[+,-]:9,x7:[+,-]:10,x7:[+,-]:11,x7:[+,-]:12,x7:[+,+]:1,x8:[+,-]:1,x8:[+,-]:2,x8:[+,-]:3,x8:[+,-]:4,x8:[+,-]:5,x8:[+,-]:6,x8:[+,-]:7,x8:[+,-]:8,x8:[+,-]:9,x8:[+,-]:10,x8:[+,-]:11,x8:[+,-]:12,x8:[+,+]:1]).
nonfirst_member_index(x24:[-,+]:1,
    [(x26:[+,+]:1)-2]).

first_member_index(x24:[+,+]:1,
    [x0:[+,+,-]:1,x0:[+,+,+]:1,x7:[+,-]:1,x7:[+,-]:2,x7:[+,-]:3,x7:[+,-]:4,x7:[+,-]:5,x7:[+,-]:6,x7:[+,-]:7,x7:[+,-]:8,x7:[+,-]:9,x7:[+,-]:10,x7:[+,-]:11,x7:[+,-]:12,x7:[+,+]:1,x8:[+,-]:1,x8:[+,-]:2,x8:[+,-]:3,x8:[+,-]:4,x8:[+,-]:5,x8:[+,-]:6,x8:[+,-]:7,x8:[+,-]:8,x8:[+,-]:9,x8:[+,-]:10,x8:[+,-]:11,x8:[+,-]:12,x8:[+,+]:1]).
nonfirst_member_index(x24:[+,+]:1,
    [(x26:[+,+]:1)-2]).

first_member_index(x25:[+,-]:1,
    [x25:[+,+]:1]).
nonfirst_member_index(x25:[+,-]:1,
    []).

first_member_index(x25:[+,+]:1,
    [x25:[+,-]:1,x25:[+,+]:1,x24:[-,+]:1]).
nonfirst_member_index(x25:[+,+]:1,
    [(x24:[+,+]:1)-2]).

first_member_index(x26:[+,+]:1,
    [x1:[+,+,+]:1,x11:[+,-]:1,x11:[+,-]:2,x11:[+,-]:3,x11:[+,-]:4,x11:[+,-]:5,x11:[+,-]:6,x11:[+,-]:7,x11:[+,-]:8,x11:[+,-]:9,x11:[+,-]:10,x11:[+,-]:11,x11:[+,-]:12,x11:[+,+]:1,x14:[-,+]:1,x14:[-,+]:2,x14:[-,+]:3,x14:[-,+]:4,x14:[-,+]:5,x14:[-,+]:6,x14:[-,+]:7,x14:[-,+]:8,x14:[-,+]:9,x14:[-,+]:10,x14:[-,+]:11,x14:[-,+]:12,x15:[+,-]:1,x15:[+,-]:2,x15:[+,-]:3,x15:[+,-]:4,x15:[+,-]:5,x15:[+,-]:6,x15:[+,-]:7,x15:[+,-]:8,x15:[+,-]:9,x15:[+,-]:10,x15:[+,-]:11,x15:[+,-]:12,x15:[+,+]:1,x17:[-,+]:1,x17:[-,+]:2,x17:[-,+]:3,x17:[-,+]:4,x17:[-,+]:5,x17:[-,+]:6,x17:[-,+]:7,x17:[-,+]:8,x17:[-,+]:9,x17:[-,+]:10,x17:[-,+]:11,x17:[-,+]:12]).
nonfirst_member_index(x26:[+,+]:1,
    [(x14:[+,+]:1)-2,(x17:[+,+]:1)-2,(x25:[+,+]:1)-2]).

first_member_index(x27:[]:1,
    []).
nonfirst_member_index(x27:[]:1,
    []).

first_member_index(x28:[]:1,
    []).
nonfirst_member_index(x28:[]:1,
    []).

first_member_index(x29:[]:1,
    []).
nonfirst_member_index(x29:[]:1,
    []).

first_member_index(x30:[]:1,
    []).
nonfirst_member_index(x30:[]:1,
    []).

first_member_index(x31:[]:1,
    []).
nonfirst_member_index(x31:[]:1,
    []).

first_member_index(x32:[]:1,
    []).
nonfirst_member_index(x32:[]:1,
    []).

first_member_index(x33:[]:1,
    []).
nonfirst_member_index(x33:[]:1,
    []).

