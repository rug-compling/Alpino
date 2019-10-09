:- use_module( wlists, [ wappend/3 ] ).

:- hdrug_flag(my_clause,_,on(H,B,rules_clause(H,B))).

%%%%%%%%%%%%%
% the rules %
%%%%%%%%%%%%%

% rule A: Xbar -> SomeLefties X SomeRs
% Xbar subcategorizes for the complements of X that are not being
% found in this rule. Note that this does NOT give rise to
% spurious ambiguities because we simply require that the head
% is LEXICAL (and also subcategorizes for at least an element)
%
% the way the sc feature is propagated is similar to the foot feature
% principle of slash, rel and extra. The difference is that the order
% of the sc list is of more importance: left in sc list implies
% closer to the head. extra, rel, and slash are handled as if they represent
% sets.
%
% the rule is not binary because of:
% - verb clusters: now we can simply require +lex of verbal argument
%                  that would otherwise not be possible (this is problematic
%                  for categorial accounts)
% - partial vp topicalization where the topicalized constituent is not a
% `subcluster' - if these are well-formed ???
%   what if these are NOT well-formed???
%
% the rule does not select all arguments, because of:
% - partial vp topicalisation (??not very good in Dutch)
% - third construction
%
% this does NOT lead to Pollards spurious ambiguities because:
% head is required to be +LEXICAL!


r(Rule) :-
	rule(Rule),
	Rule:mt:lex => phrasal,
	Rule:hd:lex => lexical,
	Rule:mt:f_slash => [],
	Rule:mt:f_extra => [],
	Rule:mt:f_rel => [],
	Rule => apply,
	Rule:hd:inv => -,    
	% if inverted should use verb-front rule instead
	ScFound =/= [],
	wappend(ScFound,Rule:mt:sc,Rule:hd:sc),     
        % the ones that are found should precede on subcat  
        % the ones that are left behind 
        % should we require that the first not-selected
        % argument should be `seperable'? * vinden wil jan marie aardig
	args_dir(ScFound,Rule:ls,Rule:rs),          
        % get left & right daughters from subcat list resp.
	allows_sep(Rule:mt:sc).                     
        % partial vp's only if trailing arguments allow it.


% no reason to allow partial inverted vp's.
% rule A-inverted vp -> v(inv) Lefties Rs
r(Rule) :-
	rule(Rule),
	Rule:mt:lex => phrasal,
	Rule:hd:lex => lexical,
	Rule:mt:f_slash => [],
	Rule:mt:f_extra => [],
	Rule:mt:f_rel => [],
	Rule => verbfront,
	Rule:hd:inv => +,
	Rule:hd:cat:vform => fin,
	Rule:mt:sc => [],
	Rule:ls => [],
	Rule:rs =/= [],
	front(Rule:hd:sc,Rule:rs).

% rule B: main --> Topic, Vp
% this rule only aplies on saturated vp's containing a slash
r(Rule) :-
	rule(Rule),
	Rule:mt:lex => phrasal,
	Rule => topicalize,
	Rule:hd:sc => [],
	Rule:mt:sc => [],
	Rule:ls <=> [@topicalization_cat],
	Rule:rs => [],
	Rule:mt:f_slash <=> [Rule:ls:h],
	Rule:mt:slash => [],
	Rule:mt:f_extra => [],
	Rule:mt:extra => [],
	Rule:mt:f_rel <=> Rule:ls:h:rel,
	Rule:mt:rel => [],
	Rule:hd:cat:vform => fin.

% rule C: vp --> vp[extra:Extras] Extra {member(Extra,Extras)}
r(Rule) :-
	rule(Rule),
	Rule:mt:lex => phrasal,
	Rule => extrapose,
	Rule:hd:sc => [],         % applies after selection of subcat 
	Rule:mt:sc => [],
	Rule:ls => [],
	Rule:rs <=> [@extraposition_cat],
	Rule:mt:f_slash => [],
	Rule:mt:f_extra <=> Rule:rs,
	Rule:mt:f_rel => [],
        Rule:hd:cat => vp.


% rule D: v --> particle v
% Verb-projection Raising. In Dutch we only allow particles and
% participiums here:
% dat jan heeft OP willen bellen
% dat jan heeft willen OP bellen
%
% dat Jan wil GESLAPEN hebben
% dat Jan GESLAPEN wil hebben
r(Rule) :-
	rule(Rule),
	Rule:hd:lex => lexical,
	Rule:mt:f_slash => [],
	Rule:mt:f_extra => [],
	Rule:mt:f_rel => [],
	Rule:mt:lex => lexical,
	Rule => cliticize,
	Rule:ls <=> [@clitic_cat],
	Rule:rs => [],
        Rule:hd:cat:vform => ~fin,
	wdel_leftmost(Rule:ls:h,Rule:hd:sc,Rule:mt:sc),
        Rule:hd:cat => vp.                           % of course

h_rule(Rule:hd,Rule:mt,Rule:ls,Rule:rs,Name) :-
	r(Rule),           % specific
	find_type(Rule,[Name|_]).

:- hdrug_flag(my_clause,_,off).
