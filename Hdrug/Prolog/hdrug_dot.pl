:- module(hdrug_dot,
	  [ dot_tree/2
	  ]).

:- expects_dialect(sicstus).

%% otherwise, swi writes hd/mod as 'hd/ (mod)' etc
:- op(0,yfx,mod).

:- use_module(library(charsio)).
:- use_module(library(system)).
:- use_module(hdrug_util).

:- initialize_flag(dot_program,dotty).

dir(TmpDir) :-
    environ('TMPDIR',TmpDir),!.
dir('/tmp').

tmp_file(File) :-
    dir(TmpDir),
    atom_concat(TmpDir,'/hdrug_dotXXXXXX',Base0),
    mktemp(Base0,Base),
    atom_concat(Base,'.dot',File).

dot_tree(Name,FS) :-
    hdrug_show:change_thing_tree(FS,Name,Tree),
    pp_tree_start(Tree,Name,Chars,[]),
    dot_tree_command(Chars).

dot_tree_command(Chars) :-
    hdrug_flag(dot_program,P),
    dot_tree_command(P,Chars).

dot_tree_command(ghostview,Chars) :-
    !,
    tmp_file(File),
    tell(File),
    call_cleanup(format("~s~n",[Chars]),
		 told
		),
    format_to_chars('dot -Tps ~w | ghostview - &',[File],CmdChars),
    atom_codes(Cmd,CmdChars),
    shell(Cmd).
dot_tree_command(gv,Chars) :-
    !,
    tmp_file(File),
    tell(File),
    call_cleanup(format("~s~n",[Chars]),
		 told
		),
    format_to_chars('dot -Tps ~w | gv - &',[File],CmdChars),
    atom_codes(Cmd,CmdChars),
    shell(Cmd).
dot_tree_command(xv,Chars) :-
    !,
    tmp_file(File),
    tell(File),
    call_cleanup(format("~s~n",[Chars]),
		 told
		),
    format_to_chars('dot -Tgif ~w | xv - &',[File],CmdChars),
    atom_codes(Cmd,CmdChars),
    shell(Cmd).
dot_tree_command(dotty,Chars) :-
    !,
    tmp_file(File),
    tell(File),
    call_cleanup(format("~s~n",[Chars]),
		 told
		),
    format_to_chars('dotty ~w',[File],CmdChars),
    atom_codes(Cmd,CmdChars),
    shell(Cmd).
dot_tree_command(user,Chars) :-
    !,
    format("~s~n",[Chars]).
dot_tree_command(F,_) :-
    format(user_error,"dot_program=~w~n",[F]),
    format(user_error,"error: value ~w not supported!~n",[F]),
    fail.

pp_tree_start(tree(Node,_,Ds),Name) -->
    dot_header,
    pp_node(Node,Name,1),
    pp_tree_ds(Ds,Name,1,2,_),
    dot_footer.

dot_header -->
    format_to_chars('digraph "Hdrug" {~nranksep=0.25;rankdir=LR~n',[]),
    format_to_chars('node [height=0.2,width=0.2,shape=plaintext];~n',[]),
    format_to_chars('edge [dir=none];~n',[]).


dot_footer -->
    format_to_chars('}',[]).

pp_node(Node,Name,N,C0,C) :-
    Name = user(_),
    !,
    format_to_chars('~w [shape=record,label="',[N],C0,C1),  %" 
    hdrug:dot_tree_user_node(Node,C1,C2),
    format_to_chars('"];~n',[],C2,C). %"
pp_node(Node,_Name,N) -->
    format_to_chars('~w [label="~w"];~n',[N,Node]).


pp_tree_ds([],_,_,N,N) --> [].
pp_tree_ds([tree(Node,_More,NodeDs)|Nodes],Name,Id,N0,N) -->
    { N1 is N0 + 1 },
    pp_node(Node,Name,N1),
    format_to_chars('~w -> ~w [];~n',[Id,N1]),
    pp_tree_ds(NodeDs,Name,N1,N1,N2),
    pp_tree_ds(Nodes,Name,Id,N2,N).

