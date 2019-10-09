%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% some default definitions for user-provided predicates %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PHONOLOGY/2
% phonology(-Node,+Phon)
% phonology(+Node,?Phon
% defines the phonology Phon of node Node
%
% is only neccesary if you do generation, or if your
% grammar is sign-based, and you want the string attribute
% of the top-goal to be instantiated. Note that phonology
% is called _before_ parser starts as well, hence don't
% define it as `yield'
%
% is used to 
% 1. print result of generation
% 2. instantiate phon attribute during parsing (in case of
%    sign-based grammar)
%
%
% phonology(_,_).

% SEMANTICS/2
% semantics(Node,Sem)
% defines the semantics Sem of node Node. Is used: 
% 
% 1. in order to determine the semantic-head of a rule 
%    (for head-driven generation).
% 2. to instantiate the semantics of a node, at the
%    start of generation. 
% 3. to print the result of parsing.

% semantics(_,_).


% TOP/2
% top(Name,Node)
% Name is atom
% Node is node
% if hdrug_flag(top_features,Name), then top(Name,Node) is called before
% parsing or generation of Node.
%
%
% no default is provided here, as the flag value of top_features will
% be `undefined' by default

%% for typing:
% => library(feature), library(mellish), library(p_feature)
%
% catch_print_error(Att,Val,Tab)
% define_type(Type,ConjOfDisjOfSubTypes,ApprAtts,Var,ConstraintOnVar)
% boolean_type(Type,Product)

% RULE/3
% a grammar consists of a definition of the predicate
% rule(Mother,Daughters,Name)
%   Mother is a term
%   Daughters is a list of terms
%   Name is a term
%
% Responsibility of building of parse-tree and
% building of string is entirely in the hands of
% grammar-writer. Note furthermore that Words is a list
% of index:word pair, such that index is the position of
% the word in the string. Also note that the string, built
% by the grammar, should be such a index-word pair list,
% although the grammar does not have to worry about the
% actual values of the indices.
%
% You can use the ---> notation, and let the compiler work out
% an appropriate name. Furthermore, using ---> will give the
% possibility for some partial evaluation by putting constraints
% after :-
% Mother ---> Daughter is term-expanded into rule/3 format.

% EXTRA_CMD/3
% to add commands to the command interpreter
% e.g. to add the command 'xx Arg' which calls the
% predicate foo(foo,Arg)
% extra_cmd(foo(foo,Arg)) -->
%	[foo],
%       [Arg].
%

% GRAMMAR_WRITER_HELP/2
%
% grammar_writer_help(CmdName,MsgList)
% defines a help message for cmd CmdName (for example
% for a command which has been added with the extra_cmd
% predicate). Typing ?CmdName will force write_list(MsgList).
%

% RESTRICTION/2
%
% defines the restriction of a category
% cf. Shieber ACL 1985
% 
% restriction(Term,SubsumingTerm)

% :- hdrug_flag(restriction_level,_,0).

% restriction(Term,Restr):-
%	hdrug_flag(restriction_level,I),
%	restrict(Term,I,Restr).

% HFC/6
%
% CHECK_HFC/6
%
% defines head-feature percolation. Hfc is a unification, check_hfc
% is a check (checks for two categories whether the first is a possible
% head-corner of the second.
%% hfc(_,_,_,_,_,_).

check_hfc(_,_,_,_,_,_).

% IGNORE_SEMANTICS/2
% ignore_semantics(Node,NodeWithoutSem).

% ignore_semantics(Sem,Sem).

% GM_SHOW/2
% gm_show(Cat,Label)
% defines how to pretty-print a category for the graphical debuggers of
% parsers

% gm_show(Cat,Cat).

% SYN_HEAD/2
%
% syn_head(Mother,Daughter)
% succeeds if Daughter is the syntactic head of Mother. Fails otherwise.
% no default is provided



