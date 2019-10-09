%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% TAG GRAMMAR %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_top_category(C,C).

ignore_semantics(Node,Node).

user_syn(n(_),n).
user_syn(x(_),x).

user_top(_,_).

user_bottom(_,_).

user_sem(_Sign,_).

user_show(syn,X,X).

address(n(N),N).
address(x(N),N).

:- user:usr_cmd([alias,s,show,-,g,syn]).

init('I',n(_):a).


aux('A',n(_):[x(_):a, +n(_):[+ =n(_)] ]).


