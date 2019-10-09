:- module(alpino_genrules, [ headed_grammar_rule/4, grammar_rule_unpack/4 ]).

:- use_module(hdrug(hdrug_util)).

:- multifile user:term_expansion/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar rule modification %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% We will generate head first rules by picking the next grammar rule,
%% and moving the head daughter as the first argument of the headed_grammar_rule
%% term. This will allow us to invocate rules quickly.	
generate_headfirst_rule(Rule) :-
    alpino_lc_in:grammar_rule_g(Id,Mother,Daughters),
    generate_headfirst_rules_aux(Id,Mother,Daughters,Rule).

generate_headfirst_rule(gap(ID,Cat)) :-
    alpino_lc_in:grammar_rule_g(ID,Cat,[]).

%generate_headfirst_rule(headed_grammar_rule(Punct,optpunct(ne),Mother,[Punct])) :-
%    alpino_lc_in:grammar_rule(optpunct(ne),Mother,[Punct]).

%% imp and imp_imp form a special case, since the semantic head uses get_val, but we
%% need to do a put_val first. So, we will make the leftmost daughter the head.
generate_headfirst_rules_aux(vast_staat,Mother,[Head|Daughters],Rule) :-
    !,
    Rule = headed_grammar_rule(Head,vast_staat,Mother,[Head|Daughters]).

generate_headfirst_rules_aux(imp,Mother,[Head|Daughters],Rule) :-
    !,
    Rule = headed_grammar_rule(Head,imp,Mother,[Head|Daughters]).

generate_headfirst_rules_aux(imp_imp,Mother,[Head|Daughters],Rule) :-
    !,
    Rule = headed_grammar_rule(Head,imp_imp,Mother,[Head|Daughters]).

%% First attempt to move the head in the 'proper' manner.
generate_headfirst_rules_aux(Id,Mother,Daughters,Rule) :-
    move_head(Mother,Head,Daughters),
    !,
    Rule = headed_grammar_rule(Head,Id,Mother,Daughters).

%% Last resort.
generate_headfirst_rules_aux(Id,Mother,[Daughter|Rest],Rule) :-
    Rule = headed_grammar_rule(Daughter,Id,Mother,[Daughter|Rest]).

%% Move the leftmost daughter that has the same semantics as the mother.
move_head(Mother,Head,[Daughter|_]) :-
    sem_match(Mother,Daughter),
    !,
    Head = Daughter.
move_head(Mother,Head,[_|Rest]) :-
    move_head(Mother,Head,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modified grammar rules for unpacking %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpack_rule(grammar_rule_unpack(Id,Mother,Daughters,Projections)) :-
    alpino_lc_in:grammar_rule_g(Id,Mother,Daughters0),
    remove_call(Daughters0,Daughters),
    daughter_proj(Daughters,Mother,Projections).

remove_call(Daughters0,Daughters) :-
    lists:select(call(_),Daughters0,Daughters),
    !.
remove_call(Daughters,Daughters).

daughter_proj([],_,[]).
daughter_proj([H|T],Mother,[Proj|NewT]) :-
    (   sem_match(Mother,H)
    ->  Proj = -
    ;   Proj = +
    ),
    daughter_proj(T,Mother,NewT).

%%%%%%%%%%%%%
% Semantics %
%%%%%%%%%%%%%

%% Check whether two feature structures have the same semantics 
sem_match(Mother,Daughter) :-
    alpino_data:dt(Mother,MDT),
    alpino_data:dt(Daughter,DDT),
    MDT == DDT.

call_with_constraints(Body1,Goal1,Rule) :-
    call(Body1),
    copy_term(Body1/Goal1,_/Goal,ConsList),
    cons_body(ConsList,Goal,Rule).

cons_body([],Head,Head).
cons_body([Goal|T],Head,(Head:-Goals)):-
    cons_body1(T,Goal,Goals).

cons_body1([],Goal,Goal).
cons_body1([Goal|T],PrevGoal,(PrevGoal,Goals)) :-
    cons_body1(T,Goal,Goals).

user:term_expansion(expand_genrules,GoalList) :-
    findall(Rule,call_with_constraints(generate_headfirst_rule(Goal),
				       Goal,Rule),GoalList).

user:term_expansion(expand_unpack_rules,GoalList) :-
    findall(Rule,call_with_constraints(unpack_rule(Goal),
				       Goal,Rule),GoalList).


expand_genrules.
expand_unpack_rules.
