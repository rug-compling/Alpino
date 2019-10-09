:- expects_dialect(sicstus).

:- [hdrug_main].

:- initialization(set_prolog_stack(global, limit(2*1024**3))).
:- initialization(set_prolog_stack(trail,  limit(1*1024**3))).

set_dir(Name, Alias) :-
       absolute_file_name(Alias, Dir,
                          [ file_type(directory),
                            access(read)
                          ]),
       set_flag(Name, Dir).

:- set_dir(hdrug_library,alpino('Hdrug/Tcl')).
:- set_dir(tex_library,alpino('Hdrug/Tex')).


