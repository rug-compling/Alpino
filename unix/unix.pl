:- module(alpino_unix,[fork/1,
		       %% wait/2,  cannot be exported, because in module system
		       reap_zombies/0]).

:- if(current_prolog_flag(dialect,swi)).

:- use_module(library(unix)).

:- expects_dialect(sicstus).

foreign_resource(unix,[pl_reap_zombies]).

pid(Pid) :-
  current_prolog_flag(pid,Pid).

:- else.

:- use_module(library(system)).

foreign_resource(unix,[pl_fork,pl_reap_zombies,pl_wait]).
foreign(pl_fork,c,fork([-term])).
foreign(pl_wait,c,wait(-integer,[-term])).

:- endif.

foreign(pl_reap_zombies,c,reap_zombies).

:- load_foreign_resource(unix).
