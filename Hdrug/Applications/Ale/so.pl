
:- set_prolog_flag(discontiguous_warnings,off).

:- use_module('/home/vannoord/z/Hdrug/Prolog/hdrug_main').     

:- set_flag(dir,'/home/vannoord/z/Hdrug/Prolog').

:- set_flag(hdrug_library,'/home/vannoord/z/Hdrug/Tcl').

:- set_flag(tex_library,'/home/vannoord/z/Hdrug/Tex').

:- set_flag(blt,noblt).

:- set_flag(blt_library,none).

:- use_module('/home/vannoord/z/Hdrug/Applications/Ale/start').

:- prolog_flag(compiling,_,debugcode).

:- hdrug_main.




