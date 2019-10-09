%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% constraints used by rules and lexicon %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module( wlists, [ wappend/3 ] ).

:- hdrug_flag(my_clause,_,on(H,B,templates_clause(H,B))).

:- initialize_flag(add_mod,on).
:- initialize_flag(push_to_extra,off).
:- initialize_flag(push_to_slash,on).

%%%%%%%%%%%%%%%%%%%%%%%
% argument categories %
%%%%%%%%%%%%%%%%%%%%%%%

arg(Arg) :-
	Arg:sc => [],        % arguments are saturated
	Arg:rel => [].

particle_arg(Part) :-
	arg(Part),
	Part:lex => lexical,
	Part:slash => [],
	Part:extra => [],
 	Part:dir => left,
	Part:cat => part,
	Part:sem => c0,
	Part:sem:restr => [].

np_arg(Cat) :-
	arg(Cat),
	Cat:cat => np,
	Cat:dir => ~extra,   % np's never extrapose
        Cat:slash => [].     % np's are islands

pp_arg(Cat) :-
	arg(Cat),
	Cat:cat => pp,
	Cat:slash => [].     % pp's are islands

adv_arg(Cat) :- 
	arg(Cat), 
	Cat:cat => adv, 
	Cat:dir => ~extra, % too strict -- some adverbs can: 
                           % * jan heeft gelopen hard 
                           % jan heeft gelopen vandaag 
	Cat:extra => [],   % island for extra % ??
	                   % difficult to check as adverbs don't take args?  
	Cat:slash => [].   % island for slash

adj_arg(Cat) :-
	arg(Cat),
	Cat:cat => adj,    % not an island for slash: 
                           % voor vrouwen is jan erg aardig
                           % van muizen is jan bang
	                   % not an island for extra:
                           % dat jan bang is voor muizen
	Cat:dir => ~extra. % *dat jan is aardig

pred_arg(Pred) :-
	adj_arg(Pred),
	Pred:cat => pred.

att_arg(Att) :-
	adj_arg(Att),
	Att:cat => att.

% not an island for slash: 
% waarom denk je dat jan zegt dat piet getrouwd is?
% is dependent on the `bridge-verb' zegt
%
% island for slash:
%
% * .. dat [ dat jan ziek t_i is] erg vervelend is [_i van ellende]
%      dat [ dat jan ziek t_i is [_i van ellende ] ] erg vervelend is
%      dat [ dat jan ziek van ellende is ] erg vervelend is

sbar_arg(Cat) :-
	arg(Cat),
	Cat:cat => sbar.

% dat_arg and om_arg are vp's
dat_arg( Cat ) :-
	arg(Cat),
	Cat:cat:vform => dat,
	Cat:dir => (extra ; topic),      % a bit too strict: forces topicalisation 
                                        % or extraposition
	Cat:extra => [].                % island for extra

om_arg( Cat ) :-
	arg(Cat),
	Cat:cat:vform => om,
	Cat:dir => (extra ; topic),      % again: forces topicalisation or extraposition
	Cat:extra => [].                % island for extra

iv_arg(Cat) :-
	arg(Cat),
	Cat:cat => vp,
	Cat:f_slash => [].              % don't take the result of the topic rule as a
                                        % possible argument..

% ordinary i.e. non-third construction obligatory extraposed vp: 
extraposed_vp(Vp) :-
        iv_arg(Vp), 
	Vp:cat:vform => ( om ; te ),
	Vp:extra => [],                 % to disallow sp. amb??
%%	Vp:dir => (extra;topic).
	Vp:dir => (right;topic).        % experiment

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% special categories in rules %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extraposition_cat(C):-
	C:extra => [],
	C:dir => extra.

topicalization_cat(Slash) :-
	Slash:slash => [],
	Slash:extra => [].

% what is called `clitic' is only used for seperable prefixes
% and participles.
clitic_cat(Clitic) :-
	Clitic:dir => left,
	Clitic:sep => -,      % ?????
	Clitic:cat => part,   % enough?
	Clitic:lex => lexical,
	Clitic:sc => [],
        Clitic:rel => [],     % toch?
	Clitic:slash => [],
	Clitic:extra => [].

clitic_cat(Prt) :-
	Prt:dir => left,
	Prt:lex => lexical,
	Prt:cat:vform => pas.

%%%%%%%%%%%%
% controle %
%%%%%%%%%%%%

% controle: _index_ of controller and embedded subject are identical,
%           the subject role of embedded vp is not realized, but an index.
controle(Obj,VpArg) :-
	Obj:sem:index <=> VpArg:fargs:h:sem:index,
	VpArg:fargs:h:sem => ix.

% raising: controller and embedded subject are _identical_
raising(Obj,VpArg) :-
	VpArg:fargs:h <=> Obj.

%%%%%%%%%%%%%%%%%%%%%%
% relative arguments %
%%%%%%%%%%%%%%%%%%%%%%

% a relative phrase such as `die Jan kust'
% N is the `head-noun' selecting this relative argument. Agreement
% of index is necc. (via f_rel feature):
%
% het boek dat/*die jan kust
%  de  man die/*dat jan kust

relative_arg(Cat,N) :-
	Cat:dir => ~left & ~topic,
	Cat:inv => -,
	Cat:cat:vform => fin,
	Cat:f_rel <=> [Rel:sem:index],       % must have found a rel!
	N:sem:index <=> Rel:sem:index.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% relational constraints %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% strategy: constraints are fired as soon as they can be
% fired without the risk of non-termination. This implies
% that constraints might be fired too soon --- but never
% too late. Experimenting with other strategies leads to
% `residues': constraints that never get fired at all.

%%%%%%%%%%%
% add_mod %
%%%%%%%%%%%

% constraint adds an indefinite number of modifiers to a
% lexical entry. Semantics is built in naive way. Responsibility
% should be passed to the modifier instead! E.g. by a `mod' feature
% with `in' and `out' value. Cf. Kasper.

add_mod(S0,S1,S) :-
	hdrug_flag(add_mod,Val),     % set to off for debugging the grammar 
                               % without modifiers
	add_modf(Val,S0,S1,S).

add_modf(off,S,S,_).

% add_mod(LexSign,ResSign,_)
add_modf(on,Sign0,Sign,S) :-
	unify_except_l(Sign0,Sign,{ [sc,sem:restr,fargs] }),   
		% if we do not unify sem, then horrible
                % for generation, of course
	add_mod(Sign0:sc,Sign:sc,Sign0:sem,Sign:sem,S,NewFargs),
	wappend(Sign0:fargs,NewFargs,Sign:fargs).

add_mod(Sc0,Sc,Sem0,Sem,Sign,NewFargs) :-
	add_mod(Sc0,Sc,Sem0,Sem,Sign,Sem:restr,NewFargs).

add_mod(Sc0,Sc,Sem0,Sem,Sign,R,NewFargs) :-
	when((nonvar(Sc);(nonvar(Sc0),nonvar(R))),
	     add_mod0(Sc0,Sc,Sem0,Sem,Sign,R,NewFargs)).

add_mod0([],[],Sem,Sem,_,_,[]).
add_mod0([H|T],[H|T2],Sem0,Sem,Sign,R,NewFargs) :-
	add_mod(T,T2,Sem0,Sem,Sign,R,NewFargs).
add_mod0(T2,[Mod|T],Sem0,Sem,Sign,[_|R],[Mod|NewFargs]):-
	allows_sep(T2),
	a_mod(Sign,Mod,Sem0,Sem1),
	add_mod(T2,T,Sem1,Sem,Sign,R,NewFargs).

% for now: all modifiers of Kasper's restrictor type.
% restrictor list should be a set - but it is a list 
% now representing order in which mod's are put in, and
% hence left-right order if before head, and right-left
% if after head.

% for now adjuncts are added to fargs (but where?)

a_mod(Head,Mod,Sem0,Sem) :-
	unify_except(Sem0,Sem,restr),
	Sem:restr <=> [Mod:sem|Sem0:restr],
	a_mod(Head,Mod).

% a_mod(?modified, ?modifier)
a_mod(^vp, \ @pp_arg).         % rightward is extraposition
a_mod(^vp, \ @adv_arg).
a_mod(^vp, @sbar_arg).
a_mod(^adj,\ @adv_arg).
a_mod(^n,  / @pp_arg). 
a_mod(N & ^n,  @relative_arg(N)). 
a_mod(N & ^n,  \\ Att & @att_arg) :-
	N:sem:index <=> Att:fargs:h:sem:index.

%% the subcat list is ordered from close to the verb to 
%% further from the verb. Some arguments are so `close' to
%% the verb that they do not allow modifiers between themselves
%% and their heads. Eg:
%%   jan marie aardig *vandaag vindt
%%   jan marie gekust *vandaag heeft
%% For this reason there is a feature
%% `sep', if - -> no modifiers

:- block allows_sep(-).
allows_sep([]).
allows_sep([H|_]) :-
	H:sep => + .

%%%%%%%%%%%%%%%
% add_subject %
%%%%%%%%%%%%%%%

% A subject is added by a lexical rule that applies to
% finite verbs only. 
% vp[-fin]/... -> vp[-fin]/....
% vp -> np\s

%%% subjects sometimes allow extraposition:
%%% het feit blijft staan dat ...
%%% ?de auto verdween die we niet hadden horen aankomen

%%% but also:
%%% * de auto verdween van de brandweer
%%%  een auto verdween van de brandweer
%%% dat er iedere dag een auto verdween van de brandweer
%%%
%%% it is allowed at the moment only if the subject is not the
%%% topic

add_subject(Sign,Sign) :-
	Sign:cat:vform => ~fin.

add_subject(Sign0,Sign) :-
	Sign0:cat:vform => fin,                % for finite verbs
	Sign:fargs:h <=> \ @np_arg,               % a nominate np to the left   
	Sign:fargs:h:cat:case => nom,
        unify_except(Sign0,Sign,sc),
	wappend(Sign0:sc,[Sign:fargs:h],Sign:sc). % as the last element on the subcat list

%%%%%%%%%%%%%%%%%
% push_to_slash %
%%%%%%%%%%%%%%%%%

push_to_slash(S0,S) :-
	hdrug_flag(push_to_slash,Val),               % set to off for debugging
                                               % subordinate clauses
	push_to_slashf(Val,S0,S).

push_to_slashf(off,S,S).

push_to_slashf(on,Sign0,Sign) :-
	unify_except_l(Sign0,Sign,[sc,slash]),
	push_to_slash(Sign0:sc,Sign:sc,Sign0:slash,Sign:slash).

:- block push_to_slash(-,-,?,?).

push_to_slash([],[],S,S).
push_to_slash([H0|T ],   T, [], [H]) :-
	unify_except_l(H0,H,[lex,rel]),    %i.e. lexicality
                                           % why rel? Because objects normally -rel??
	topicalization_cat(H).
push_to_slash([H|T0],[H|T],S0,S) :-
	push_to_slash(T0,T,S0,S).

%%%%%%%%%%%%%%%%%
% push_to_extra %
%%%%%%%%%%%%%%%%%

%% It is unclear whether extraposed categories allow extraposition
%% out of their phrase:
%%
%% dat Jan [[droomt van een boek] van Vestdijk] where
%% van Vestdijk 2x extraposed modifier of `een boek'
%% Not allowed right now

push_to_extra(S0,S) :-
	hdrug_flag(push_to_extra,Val),       % set to off for a grammar without extraposition
	push_to_extraf(Val,S0,S).

push_to_extraf(off,S,S) :-
	S:extra => [].

push_to_extraf(on,Sign0,Sign) :-
	unify_except_l(Sign0,Sign,[sc,extra]),
	push_to_extra(Sign0:sc,Sign:sc,Sign0:extra,Sign:extra).

push_to_extra(Sc0,Sc,Ex0,Ex) :-
	when((nonvar(Sc0);(nonvar(Sc),nonvar(Ex))),push_to_extra0(Sc0,Sc,Ex0,Ex)).

push_to_extra0([],[],E,E).
push_to_extra0([H|T0],T,E0,[H|E]) :-
	extraposition_cat(H),
	push_to_extra(T0,T,E0,E).
push_to_extra0([H|T0],[H|T],S0,S) :-
	push_to_extra(T0,T,S0,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% percolation of foot features %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% foot feature principle and head-feature principle are
% very straightforward!

:- block collect_ffp(-,?,?,?).

collect_ffp([],[],[],[]).
collect_ffp([H|T],Sl,Ex,Rl) :-
	wappend(H:slash,Sl0,Sl),
	wappend(H:extra,Ex0,Ex),
	wappend(H:rel,Rl0,Rl),
	collect_ffp(T,Sl0,Ex0,Rl0).

:- block complement(-,?,?,-).
complement([],L,L,_).
complement([H|T],L0,L,[_|XR]) :-
	wdel(H,L0,L1),
	complement(T,L1,L,XR).

% e.g. complement([a,b],[d,a,c,b,e],[d,c,e])
complement(V,L0,L) :-
	complement(V,L0,L,L0).

% e.g. wdel(x,[a,x,b,x],[a,b,x]) en ook
%      wdel(x,[a,x,b,x],[a,x,b])
:- block wdel(?,-,-).
wdel(El,[El|T],T).
wdel(El,[H|T0],[H|T]):-
	wdel(El,T0,T).

ffp(R) :-
	wappend(R:ls,[R:hd|R:rs],Ds),
	collect_ffp(Ds,Sl0,Ex0,Rl0),
	complement(R:mt:f_slash,Sl0,R:mt:slash),
	complement(R:mt:f_extra,Ex0,R:mt:extra),
	complement(R:mt:f_rel,Rl0,R:mt:rel).

hfp(R) :-
	R:hd:inv   <=> R:mt:inv,
	R:hd:cat   <=> R:mt:cat,
	R:hd:sem   <=> R:mt:sem,
	R:hd:fargs <=> R:mt:fargs.


rule(R) :-
	ffp(R),
	hfp(R).

%%%%%%%%%%%%
% args_dir %
%%%%%%%%%%%%

% used in rules.pl
% args_dir(Sc,Lefties,Righties)
% where Lefties reversed order as occuring in subcat

args_dir(Sc,Ls,Rs) :-
	when((nonvar(Sc);(nonvar(Ls),nonvar(Rs))),extract_args(Sc,Ls,Rs)).

extract_args([],[],[]).
extract_args([\\H|T],[H|Ls],Rs) :-
	args_dir(T,Ls,Rs).
extract_args([//H|T],Ls,[H|Rs]) :-
	args_dir(T,Ls,Rs).

:- block wdel_leftmost(?,-,-).
wdel_leftmost(El,[El|T],T).
wdel_leftmost(El,[H|T0],[H|T]):-
        H:dir => right,
	wdel_leftmost(El,T0,T).

%%%%%%%%%%%%%%%%%
% verb fronting %
%%%%%%%%%%%%%%%%%

front(Sc0,Sc) :-
	rev_args_dir(Sc0,R,Sc,R,Sc).

:- block rev_args_dir(-,?,?,?,-).
rev_args_dir([],Ls,Ls,[],_).
rev_args_dir([\\H|T],Ls0,Ls,Rs,[_|RT]):-
	rev_args_dir(T,[H|Ls0],Ls,Rs,RT).
rev_args_dir([//H|T],Ls0,Ls,[H|Rs],[_|RT]) :-
	rev_args_dir(T,Ls0,Ls,Rs,RT).


%%%%%%%%%%%%%%%
%%% LEXICON %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top of lexical hierarchy %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some function words are lexical1 but not lexical, etc.

lexical2(S) :-
	S:f_slash => [],
	S:f_extra => [],
	S:f_rel => [],
	S:slash => [],
	S:extra => [].

lexical1(S) :-
	lexical2(S),
	S:rel => [].

lexical(S) :-
	lexical1(S),
	S:lex:stem <=> Stem,
	S:sem:fun => prime(Stem),
	S:sem:restr => [].

%%%%%%%%%%%%%%%
% determiners %
%%%%%%%%%%%%%%%

% determiners are analysed as the head of the noun-phrase. Note that there
% is a lexical rule n -> np to allow for determiner-less np's.

det(S) :-
	lexical(S),
	S:sem => c1,
	S:sc:h:sem <=> S:sem:arg1,
	S:sem:index <=> S:sc:h:sem:index,  % for agreement
	S <=> ^np # [// ^n # []],
	S:lex:stem <=> S:lex:word,
	S:fargs => [],
	S:sc <=> S:args.

a_word(S) :-
	S:lex:stem ==> het,
	det(S),
	S:sc:h:sem:index => sg & neut & def.

a_word(S) :-
	S:lex:stem ==> de,
	det(S),
	S:sc:h:sem:index => sg & nneut & def.

a_word(S) :-
	S:lex:stem ==> de,
	det(S),
	S:sc:h:sem:index => pl & def.

a_word(S) :-
	S:lex:stem ==> een,
	det(S),
	S:sc:h:sem:index => sg & indef.

%%%%%%%%%%%%%%%%
% prepositions %
%%%%%%%%%%%%%%%%

prep(S) :-
        S <=> ^pp # [// NP], 
	np_arg(NP0),
	unify_except(NP0,NP,rel),    % not island for rel  & de man met wie jan slaapt
	S:sc:h:cat:case => obl,
	S:sem => c1,
	lexical(S),
	S:sc:h:sem <=> S:sem:arg1,
	S:lex:stem <=> S:lex:word,
	S:fargs <=> S:args,
	S:sc <=> S:args.

a_word(S) :-
	S:lex:stem ==> met,
	prep(S).

a_word(S):-
	S:lex:stem ==> naar,
	prep(S).

a_word(S):-
	S:lex:stem ==> voor,
	prep(S).

a_word(S):-
	S:lex:stem ==> onder,
	prep(S).

a_word(S):-
	S:lex:stem ==> achter,
	prep(S).

a_word(S):-
	S:lex:stem ==> zonder,
	prep(S).

a_word(S):-
	S:lex:stem ==> boven,
	prep(S).

a_word(S):-
	S:lex:stem ==> in,
	prep(S).

a_word(S):-
	S:lex:stem ==> op,
	prep(S).

a_word(S):-
	S:lex:stem ==> bij,
	prep(S).

a_word(S):-
	S:lex:stem ==> van,
	prep(S).

%%%%%%%%%%%%%%%%%%%
% complementizers %
%%%%%%%%%%%%%%%%%%%

%% there are two kinds of objects traditionally analysed as
%% complementizers: - markers (a la HPSG) that inherit semantics
%% and `change' vform feature, these typically can be optional, 
%% these have vp as their category, optionality is analysed by
%% subcategorizing for disjunctive vform feature.
%%                  - `real' complementizers that have semantic
%% content. These can function as modifiers. They have a distinct
%% syntactic category (sbar).
%%
%% The following minimal pairs motivate the distinction:
%% a.   jan zegt dat piet slaapt
%% b. * jan zegt omdat piet slaapt
%%
%% a.   jan slaapt omdat piet slaapt
%% b. * jan slaapt dat piet slaapt
%%
%% a.   jan slaapt om uit te rusten
%% b. * jan slaapt uit te rusten
%%
%% a.   jan probeert om uit te rusten
%% b.   jan probeert uit te rusten
%%
%% A similar distinction could be useful for prepositions, although
%% all prepositions can function as meaningful elements, it seems.

marker(S) :-
	lexical1(S),
	S <=> ^vp # [// @iv_arg ],
	S:lex:stem <=> S:lex:word,
	S:sc <=> S:args,
	S:sc:h:fargs <=> S:fargs,
	S:sem <=> S:sc:h:sem.

a_word(S) :-
	S:lex:stem ==> dat,
	marker(S),
	S:sc:h:inv => -,
	S:cat:vform => dat,
	S:sc:h:cat:vform => fin.

a_word(S) :-
	S:lex:stem ==> om,
	marker(S),
	S:cat:vform => om,
	S:sc:h:cat:vform => te.

a_word(S) :-
	S:lex:stem ==> of,
	marker(S),
	S:cat:vform => of,
	S:sc:h:inv => -,
	S:sc:h:cat:vform => fin.

comp(S) :-
	lexical(S),
	S:lex:stem <=> S:lex:word,
	S:sc <=> S:args,
	S:args <=> S:fargs,
	S <=> ^sbar # [// @iv_arg],
	S:sem => c1,
	S:sem:arg1 <=> S:sc:h:sem,
	S:lex:stem <=> Stem,
	S:sem:fun => prime(Stem),
	S:sem:restr => [].

a_word(S) :-
	S:lex:stem ==> om,
	comp(S),
%	S:fargs:h <=> S:sc:h:fargs:h,  % for control
	S:sc:h:cat:vform => te,
	S:sc:h:fargs:h:sem => ix.   % `pronominal' subject of infinite vp

a_word(S) :-
	S:lex:stem ==> omdat,
	comp(S),
	S:sc:h:inv => -,
	S:sc:h:cat:vform => fin.

%%%%%%%%%
% nouns %
%%%%%%%%%

noun(S) :-
	lexical(S),
	S:cat => n,
	S:sc => [],
	S:sem => c0,
	S:args => [],
	S:fargs => [].

a_noun(S) :-
	S:lex:stem ==> boek,
	noun(S),
	S:sem:index => nhuman & neut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> kind,
	noun(S),
	S:sem:index => ~nhuman & neut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> meisje,
	noun(S),
	S:sem:index => fem & neut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> paard,
	noun(S),
	S:sem:index => nhuman & neut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> krant,
	noun(S),
	S:sem:index => nhuman & nneut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> jongen,
	noun(S),
	S:sem:index => masc & nneut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> man,
	noun(S),
	S:sem:index => masc & nneut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> vrouw,
	noun(S),
	S:sem:index => fem & nneut & (sg;pl).

a_noun(S) :-
	S:lex:stem ==> wijn,
	noun(S),
	S:sem:index => nhuman & nneut & mass.

a_noun(S) :-
	S:lex:stem ==> melk,
	noun(S),
	S:sem:index => nhuman & nneut & mass.

a_noun(S) :-
	S:lex:stem ==> water,
	noun(S),
	S:sem:index => nhuman & neut & mass.

a_noun(S) :-
	S:lex:stem ==> eten,
	noun(S),
	S:sem:index => nhuman & neut & mass.

sg_or_pl_noun(S) :-
	S:sem:index => (sg;mass),
	S:lex:stem <=> S:lex:word.

sg_or_pl_noun(S) :-
	S:sem:index => pl,
	stem_ens(S:lex:stem,S:lex:word).

n_or_np(N,N).
n_or_np(N,NP) :-
	%% * man slaapt
        %% * kind slaapt
        %% mannen slapen
        %% kinderen slapen
        %% ijs is lekker
        %% wijn is lekker
	N:sem:index => (mass ; pl),
	N:cat => n,
	NP:cat => np,
	unify_except(N,NP,cat).

ln(S0,S) :-
	add_mod(S0,S1,S0),
	push_to_extra(S1,S2),
	n_or_np(S2,S),
	sg_or_pl_noun(S),
	S0:args <=> S:args.

a_word(S) :-
	S:lex:stem <=> S0:lex:stem,
	a_noun(S0),
	ln(S0,S).

%%%%%%%%%%%%%%
% adjectives %
%%%%%%%%%%%%%%

att_or_pred(S) :-
	S:cat => att,
	S:lex:word <=> S:lex:stem,
	S:fargs:h:sem:index => indef & sg & neut.

att_or_pred(S) :-
	S:cat => att,
	stem_e(S:lex:stem,S:lex:word),
	S:fargs:h:sem:index => ~(indef & sg & neut).

att_or_pred(S) :-
	S:cat => pred,
	S:lex:word <=> S:lex:stem.

adjective(S) :-
	lexical(S0),
	S0:sem => c0,
	S0:sc => [],
	add_mod(S0,S,S),
	S:cat => adj,
	att_or_pred(S),
	S:fargs <=> S:sc,
	S:sc <=> S:args.

a_word(S) :-
	adjective(S),
	S:lex:stem ==> aardig.

a_word(S) :-
	adjective(S),
	S:lex:stem ==> lief.

a_word(S) :-
	adjective(S),
	S:lex:stem ==> vervelend.

%%%%%%%%%%%%%%
% possesives %
%%%%%%%%%%%%%%

poss(S) :-
	lexical1(S),
	S:lex:stem <=> S:lex:word,
	S <=> ^np # [\\ NP & @np_arg, //N & ^n # [] ],
	unify_except(N:sem,S:sem,restr),
	S:sem:restr:t <=> N:sem:restr,
	S:sem:restr:h <=> NP:sem,
	S:sc <=> S:args,
	S:args <=> S:fargs.

a_word(S) :-
	poss(S),
	S:sc:h:sem:index => masc & sg,
	S:lex:stem ==> zijn.

a_word(S) :-
	poss(S),
	S:sc:h:sem:index => fem & sg,
	S:lex:stem ==> haar.

a_word(S) :-
	poss(S),
	S:sc:h:sem:index => pl & ~nhuman,
	S:lex:stem ==> hun.

%%%%%%%%%%%%%
% relatives %
%%%%%%%%%%%%%

relative(S) :-
	lexical2(S),
	S:lex:word <=> S:lex:stem,
	S:rel <=> [_index],
	S:sc <=> S:args,
	S:args <=> S:fargs.

a_word(S) :-
	relative(S),
	unify_except(S:sem,N:sem,restr),
	S:sem:restr:t <=> N:sem:restr,
	S:sem:restr:h <=> Sem,
	Sem => ix,
	Sem:index <=> S:rel:h,
	S:rel:h => ~nhuman,
	S:lex:stem ==> wiens,
	S <=> ^np # [// N & ^n # []].

sat_relative(S) :-
	relative(S),
	S:sem => ix,
	S:sem:index <=> S:rel:h,
	S:sc => [],
	S:args => [],
	S:fargs => [].

np_relative(S) :-
	sat_relative(S),
	S:cat => np.

adv_relative(S) :-
	sat_relative(S),
	S:cat => adv.

a_word(S) :-
	adv_relative(S),
	S:lex:stem ==> waar.

a_word(S) :-
	np_relative(S),
	S:cat:case => (nom ; obj),    % * de man met die
	S:lex:stem ==> die,
	S:sem:index => (sg & nneut ; pl ).

a_word(S) :-
	np_relative(S),
	S:cat:case => (nom ; obj),    % * het boek met dat
	S:lex:stem ==> dat,
	S:sem:index => sg & neut.

a_word(S) :-
	np_relative(S),
	S:rel:h => ~nhuman,   % anders `waar' + R-inversion
	S:cat:case => obl,
	S:lex:stem ==> wie.

%%%%%%%%%%%%%%
%% pronouns %%
%%%%%%%%%%%%%%

pronoun(S) :-
	S:sem => ix,
	S <=> ^np # [],
	lexical1(S),
	S:lex:word <=> S:lex:stem,
	S:sc <=> S:args,
	S:fargs <=> S:sc.

a_word(S) :-
	pronoun(S),
	S:lex:stem ==> hij,
	S:cat:case => nom,
	S:sem:index => masc & sg.

a_word(S) :-
	pronoun(S),
	S:lex:stem ==> hem,
	S:cat:case => ~nom,
	S:sem:index => masc & sg.

a_word(S) :-
	pronoun(S),
	S:lex:stem ==> zij,
	S:cat:case => nom,
	S:sem:index => ( fem & sg ; ~nhuman & pl ).

a_word(S) :-
	pronoun(S),
	S:lex:stem ==> ze,
	S:cat:case => nom,
	S:sem:index => fem & sg.

a_word(S) :-
	pronoun(S),
	S:lex:stem ==> haar,
	S:cat:case => ~nom,
	S:sem:index => fem & sg.

a_word(S) :-
	pronoun(S),
	S:lex:stem ==> hun,
	S:cat:case => ~nom,
	S:sem:index => ~nhuman & pl.

%%%%%%%%%%%%%%%%
% proper nouns %
%%%%%%%%%%%%%%%%

proper_noun(S) :-
	S:sem => c0,
	S <=> ^np # [],
	lexical(S),
	S:lex:word <=> S:lex:stem,
	S:sc <=> S:args,
	S:sc <=> S:fargs.

a_word(S) :-
	S:sem:index => masc,
	proper_noun(S),
	S:lex:stem ==> jan,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => fem,
	proper_noun(S),
	S:lex:stem ==> marie,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => masc,
	proper_noun(S),
	S:lex:stem ==> bob,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => masc,
	proper_noun(S),
	S:lex:stem ==> arie,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => fem,
	proper_noun(S),
	S:lex:stem ==> jane,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => masc,
	proper_noun(S),
	S:lex:stem ==> tarzan,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => masc,
	proper_noun(S),
	S:lex:stem ==> piet,
	S:sem:index => sg.

a_word(S) :-
	S:sem:index => masc,
	proper_noun(S),
	S:lex:stem ==> klaas,
	S:sem:index => sg.

%%%%%%%%%%%%%
% particles %
%%%%%%%%%%%%%

particle(S) :-
	lexical(S),
	S:sem => c0,
	S:cat => part,
	S:sc => [],
	S:fargs => [],
	S:lex:stem <=> S:lex:word,
	S:sc <=> S:args.

a_word(S) :-
	particle(S),
	S:lex:stem ==> op.

a_word(S) :-
	particle(S),
	S:lex:stem ==> uit.

a_word(S) :-
	particle(S),
	S:lex:stem ==> af.

a_word(S) :-
	particle(S),
	S:lex:stem ==> over.

a_word(S) :-
	particle(S),
	S:lex:stem ==> mee.

%%%%%%%%%%%%%%
% adverbials %
%%%%%%%%%%%%%%

adverb(S) :-
	lexical(S),
	S:sem => c0,
	S:cat => adv,
	S:sc => [],
	S:fargs => [],
	S:lex:word <=> S:lex:stem,
	S:sc <=> S:args.

a_word(S) :-
	adverb(S),
	S:lex:stem ==> vandaag.

a_word(S) :-
	adverb(S),
	S:lex:stem ==> erg.

%%%%%%%%%
% verbs %
%%%%%%%%%

verb(S) :-
	lexical(S),
	S:cat => vp,
	S:cat:ipp => - .

intransitive(S) :-
	verb(S),
	S:sc => [],
	S:fargs <=> [Subj],
	S:sem => c1,
	S:sem:arg1 <=> Subj:sem.

transitive(S) :-
	verb(S),
	S:sc <=> [Obj],
	S:fargs <=> [Subj,Obj],
	S:sem => c2,
	S:sem:arg1 <=> Subj:sem,
	S:sem:arg2 <=> Obj:sem.

bi_transitive(S):-
	verb(S),
	S:sc <=> [IObj,Obj],
	S:fargs <=> [Subj,Obj,IObj],
	S:sem => c3,
	S:sem:arg1 <=> Subj:sem,
	S:sem:arg2 <=> Obj:sem,
	S:sem:arg3 <=> IObj:sem.

/*
predicative(S) :-
	verb(S),
	S:sc <=> [\ Pred,\ Obj , \ Subj],
	np_arg(Subj),
	Obj:cat:case => obj,
	S:sem => c2,
	S:sem:arg1 <=> S:fargs:h:sem,
	S:sem:arg2 => c2,
	S:sem:arg2:fun => prime(apply),
	S:sem:arg2:arg1 <=> Pred:sem,
	S:sem:arg2:arg2 <=> Obj:sem,
	S:fargs <=> [Subj,Obj,Pred],
	S:sem:arg2:restr => [],
	Pred:sep => - .    %% dat ik jan aardig *morgen vindt
*/


np_transitive(S) :-
	transitive(S),
	S:sc:h:cat:case => obj,
	S:sc:h <=> \ @np_arg.

pp_transitive(S) :-
	transitive(S),
	S:sc:h <=> \ @pp_arg.

verb_raising_verb(S) :-
	verb(S0),
	overwrite(S0,S,{cat:ipp},+),
	S:sc <=> [ / Arg | _], 
	S:sc:h:dir => ~extra,
	Arg:sep => -,
	Arg:cat => vp,
	Arg:lex => lexical,
	Arg:slash => [],   % do it yourself
	Arg:extra => [].   % d i y

subj_raising_verb(S) :-
	verb_raising_verb(S),	
	S:sc:h:sc <=> S:sc:t.

subj_raising_raising_verb(S) :-
	subj_raising_verb(S),
	S:sc:h <=> Arg,
	S:fargs <=> [Subj,Arg],
	S:sem => c1,
	S:sem:arg1 <=> Arg:sem,
	raising(Subj,Arg).

subj_control_raising_verb(S) :-
	subj_raising_verb(S),
	S:sc:h <=> Arg,
	S:fargs <=> [Subj,Arg],
	S:sem => c2,
	S:sem:arg1 <=> Subj:sem,
	S:sem:arg2 <=> Arg:sem,
	controle(Subj,Arg).
/*
verb_stem(S) :-
	predicative(S),
	S:lex:stem ==> vind,
	S:sc:h <=> @pred_arg.
*/
verb_stem(S) :- 
	S:lex:stem ==> slaap,
	intransitive(S).

verb_stem(S) :- 
	S:lex:stem ==> loop,
	intransitive(S).

verb_stem(S) :- 
	S:lex:stem ==> lees,
	np_transitive(S).

verb_stem(S) :- 
	S:lex:stem ==> kus,
	np_transitive(S).

verb_stem(S) :- 
	S:lex:stem ==> sla,
	np_transitive(S).

verb_stem(S) :-
	S:lex:stem ==> reken,
	pp_transitive(S),
	S:sc:h:sem:fun => prime(op).

verb_stem(S) :-
	verb(S),
	S:sc <=> [@particle_arg,\ @np_arg],
	S:sem => c2,
	S:sem:arg1 <=> S:fargs:h:sem,
	S:sem:arg2 <=> S:sc:t:h:sem,
	S:lex:stem ==> bel,
	S:sc:h:sem:fun => prime(op).

verb_stem(S) :-
	verb(S),
	S:sc <=> [@particle_arg],
	S:fargs <=> [_],
	S:sem => c1,
	S:sem:arg1 <=> S:fargs:h:sem,
	S:lex:stem ==> schep,
	S:sc:h:sem:fun => prime(op).

verb_stem(S # [\ @np_arg,\ @np_arg]):-
	S:sc:h:cat:case => obj,
	S:sc:t:h:cat:case => obj,
	S:lex:stem ==> geef,
	bi_transitive(S).

verb_stem(S) :-
	S:lex:stem ==> zeg,
	transitive(S),
	dat_arg(S:sc:h).

%% extraposition_verbs
% object controle, vp argument is extraposed

intrans_extraposition_verb(S) :-
	transitive(S),
	S:sc:h <=> Vp,
	controle(S:fargs:h,Vp),
	extraposed_vp(Vp).

verb_stem(S) :-
	S:lex:stem ==> beweer,
	S:sc:h:cat:vform => te,
	intrans_extraposition_verb(S).

trans_extraposition_verb(S) :-
	bi_transitive(S),
	S:sc:h <=> \ @np_arg,
	S:sc:h:cat:case => obj,
	S:sc:t:h <=> Vp,
	controle(S:sc:h,Vp),
	extraposed_vp(Vp).

verb_stem(S) :-
	S:lex:stem ==> overreed,
	trans_extraposition_verb(S).

verb_stem(S) :-
	S:lex:stem ==> beloof,
	trans_extraposition_verb(S0),
% should remove default object-control: - this is difficult here because of transitivity
% of unification - therefore we have to remove two of the three paths
	unify_except_l(S0,S,{ [ sc:h:sem:index, sem:arg3:index ] }),  
	controle(S:fargs:h,S:sc:t:h).

%%%%%%%%%%%%%%%%%%
% verb - raising %
%%%%%%%%%%%%%%%%%%

verb_stem(S) :-
	S:lex:stem ==> schijn,
	subj_raising_raising_verb(S),
	S:sc:h:cat:vform => te.

% heb kan zowel rechts als pre verb participia vinden:
% .. dat jan heeft geslapen
% .. dat jan geslapen heeft
% de rest volgt!
% .. dat jan geslapen zou moeten kunnen willen hebben 


%% forget about dir hence left or right
verb_stem(S) :-
	S:lex:stem ==> heb,              % gekust heeft
	subj_raising_raising_verb(S0),   % ~left ~extra
	S:sc:h:cat:ipp => -,
	S:sc:h:cat:vform => pas,
	unify_except(S0,S,{ sc:h:dir }),
	S:sc:h:dir => ~extra & ~left.       % no extraposition allowed

verb_stem(S) :-
	S:lex:stem ==> heb,              % gekust heeft
	subj_raising_raising_verb(S0),   % ~left ~extra
	S:sc:h:cat:ipp => -,
	S:sc:h:cat:vform => pas,
	unify_except(S0,S,{ sc:h:dir }),
	S:sc:h:dir => left.


verb_stem(S) :-
	S:lex:stem ==> heb,              % heeft willen kussen
	subj_raising_raising_verb(S),
	S:sc:h:cat:ipp => +,
	S:sc:h:dir => right,
	S:sc:h:cat:vform => inf.

% if the verb cluster has size 2, then a modal verb may FOLLOW its
% argument verb:
% dat jan het boek lezen wil
% dat jan de kinderen op bellen wil
% this is allowed only if 1. the modal does not inherit a clitic
%                         2. the argument is verb-cluster-ish

modal_verb(S) :-
	S:sc:h:cat:vform => inf,
	subj_control_raising_verb(S).

modal_verb(S) :-
	S:sc:h:cat:vform => inf,
	subj_control_raising_verb(S0),
	overwrite(S0,S,{sc:h:dir},left),
	S:sc:h:cat:ipp => -,  % and not complex  * dat jan hebben geslapen wil
	S:cat:vform => fin,   % finite           * dat jan slapen heeft willen
        S:inv => - .          % not inverted to reduce spurious ambiguities
%%	no_particle_on_sc(S:sc:h).

verb_stem(S) :-
	S:lex:stem ==> wil,
	modal_verb(S).

verb_stem(S) :-
	S:lex:stem ==> kan,
	modal_verb(S).

% v-raising - probeer
verb_stem(S) :-
	S:lex:stem ==> probeer,
	S:sc:h:cat:vform => te,
	subj_control_raising_verb(S).

% extraposition - probeer
verb_stem(S) :-
	S:lex:stem ==> probeer,
	intrans_extraposition_verb(S),
	S:sc:h:cat:vform => om.   % the `te' variant is partial extrap.

%% difference with extraposed variant:
%%%% te ipv om
%%%% inheritance of arguments

%% difference with raised variant:
%%%% argument need not be lexical
%%%% seperation of first argument is allowed
%%%% ipp -

third_construction(S) :-
	intrans_extraposition_verb(S0),
	unify_except_l(S0,S,{[sc:h:cat:vform,sc:t,sc:h:sc]}),
	S:sc:t <=> S:sc:h:sc,
	S:sc:h:cat:vform => te.    

% you can also specify (te;om) for the third-construction to get
% the `ordinary' extraposition too. However, note that in that
% case you can also inherit the argument of `om': 
%            s
%           /   \     \
%      probeert om te slapen
% Sigh..
% this is because `om' is a verb and not a ``marker'' in the HPSG sense.

% if `om' then no 3rd construction (Den Besten & Rutten 89), but
% that follows since `om' only combines with saturated vp.

% should be equivalent:
%third_construction(S) :-
%	subj_control_raising_verb(S0),
%	S:sc:h:cat:vform => te,       % hence not `om'
%	unify_except(S0,S,{[sc:h:lex,ipp]}),
%	S:ipp => - .
%

% 3rd construction - probeer

verb_stem(S) :- 
	S:lex:stem ==> probeer,
	third_construction(S).

verb_stem(S) :- 
	S:lex:stem ==> meen,
	third_construction(S).

aci(S) :-
	verb_raising_verb(S),
	S:cat:ipp => +,
	S:sc:h <=> Vp # Args,
	wappend(Args,[\ Obj & @np_arg],S:sc:t),

	S:fargs <=> [Subj,Obj,Vp],
	Obj:cat:case => obj,
	Vp:cat:vform => inf,
	raising(Obj,Vp),  % waarom raising en geen controle? Omdat
                          % het object geen semantische rol vervult bij `zien'
	                  %    en ook: dat jan het ziet regenen
	S:sem => c2,
	S:sem:arg1 <=> Subj:sem,
	S:sem:arg2 <=> Vp:sem.

% ACI
verb_stem(S) :-
	S:lex:stem ==> zie,
	aci(S).

% LEXICAL CONSTRAINTS FOR VERBS
% volgorde is nogal willekeurig. Belangrijk is dat:
% add_subject < push_to_slash (subjecten kunnen geextraheerd)
%                hoewel: verschil tweede persoon enkelvoud subject!!
%                suggereert dat add_subject dat zelf kan..
% add_mod < push_to_slash (want moeten getopicaliseerd kunnen worden)
%         < push_to_extra (want moeten geextraponeerd kunnen worden)
%         < add_subject (want subject altijd voor modifiers)  

a_word(S) :-
	verb_stem(S0),
	add_mod(S0,S1,S),     % add modifiers
	add_subject(S1,S2),   % add subject
	S2:sc <=> S2:args,    % from now on only changes - no addition
	push_to_slash(S2,S3), % move one subcat element to slash
	push_to_extra(S3,S),  % move a number of subcat elements to extraposition
	inflection(S:lex:stem,S:lex:word,S).

:- hdrug_flag(my_clause,_,off).
