:- (   current_prolog_flag(language, sicstus)
   ->  compile('Hdrug/Prolog/conditional_compilation')
   ;   true
   ).

:- use_module(library(system)).

:- multifile
    user:file_search_path/2.

user:file_search_path(alpino, Alpino) :-
    environ('ALPINO_HOME', Alpino).

user:file_search_path(hdrug, Hdrug) :-
    user:file_search_path(alpino,Alpino),
    atom_concat(Alpino,'/Hdrug/Prolog',Hdrug).


