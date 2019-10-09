%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% lexicon %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% defines: the predicate l(Word,Rule)
%
% uses:     definitions from templates.pl
%           notation from library(feature) and decl.pl using the
%                    declarations in types.pl


l(Word,S) :-
	a_word(S),
	S:lex:word <=> Word.

