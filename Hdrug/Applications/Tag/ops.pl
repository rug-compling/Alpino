%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% declare operators %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(300,fx,*).        % substitution node
:- op(300,fx,=).        % foot node
%            +          % head node (should precede foot node marker and subs node marker)
%            :          % seperates mother node from list of daughters
:- op(900,fx,#).


