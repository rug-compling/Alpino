:- module(tcltk,
	  [ tcl_eval/3,
	    tcl_delete/1,
	    tk_main_loop/1,
	    tk_new/2
	  ]).

%%----------------------------------------------------------------------

%% adapted by Gertjan van Noord from:
%% The Tcl/Tk interface of CIAO prolog
%%
%% Montse Iglesias Urraca
%% http://www.clip.dia.fi.upm.es/
%% The CLIP Group
%% Facultad de Inform@'{a}tica
%% Universidad Polit@'{e}cnica de Madrid
%%

:- use_module(library(socket)).

bind_socket(Port, Length, Socket) :-
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, Length).

socket_accept(Socket, StreamPair) :-
    tcp_accept(Socket, Client, _Peer),
    tcp_open_socket(Client, Read, Write),
    stream_pair(StreamPair, Read, Write).

% :- style_check(-atom).

%%------------------------------------------------------------------------
%
%:- pred new_interp(-TclInterpreter,+Options) :: tclInterpreter * atom
%
%        # "Creates two sockets, the term socket and the event socket,
%           and opens a pipe to process @em{wish} in a new shell
%           invoked with the @var{Options}.".
%
%%------------------------------------------------------------------------

new_interp('$wish$'(Strm,TermStream,EventStream),Options) :-
    % gethostname(Host),
    Host = localhost,
    bind_socket(Port,1,Socket),
    atom_concat('wish',Options,V),
    open_command(V,Strm),
    format(Strm,'set prolog_host ~w~n',[Host]), flush_output(Strm),
    format(Strm,'set prolog_port ~w~n',[Port]), flush_output(Strm),
    bind_socket(EPort,1,ESocket),
    format(Strm,'set event_port ~w~n',[EPort]), flush_output(Strm),
    send_initial_code(Strm),
    socket_accept(Socket,TermStream),
    socket_accept(ESocket,EventStream).

open_command(Command, Stream) :-
    shell(Shell, Command, ArgsList),
    process:process_create(Shell, ArgsList,
                  [ stdin(pipe(Stream)),
                    detached(true)
                  ]).

shell(path('cmd.exe'), Command, ['/c', Command]) :-
    current_prolog_flag(windows, true), !.
shell('/bin/sh', Command, ['-c', Command]).



%%------------------------------------------------------------------------
%:- pred delete(+TclInterpreter) :: tclInterpreter
%
%        # "Terminates the @em{wish} process and closes the pipe, term
%          socket and event socket. Deletes the interpreter
%          @var{TclInterpreter} from the system".
%%------------------------------------------------------------------------

delete('$wish$'(Strm,TermStrm,EventStrm)) :-
    catch(( format(Strm,'uplevel 0 exit~n',[]), flush_output(Strm)),_,true),
    catch(delete_deleted('$wish$'(Strm,TermStrm,EventStrm)),_,true).

delete_deleted('$wish$'(Strm,TermStrm,EventStrm)) :-
    (  is_stream(TermStrm)  -> close(TermStrm)  ; true ),
    (  is_stream(Strm)      -> close(Strm)      ; true ),
    (  is_stream(EventStrm) -> close(EventStrm) ; true ).

send_initial_code(Strm) :-
    core(Atom),
    format(Strm,'~a~n',[Atom]),
    flush_output(Strm).

%%------------------------------------------------------------------------
%:- pred tcltk_raw_code(+String,+TclInterpreter) :: string * tclInterpreter
%
%        # "Sends the tcltk code items of the @var{Stream} to the
%          @var{TclInterpreter}".
%%------------------------------------------------------------------------

tcltk_raw_code(Atom,'$wish$'(Strm,_,_)) :-
    format(Strm,'~a~n',[Atom]),
    flush_output(Strm).

%%------------------------------------------------------------------------
%:- pred tcltk(+Code,+TclInterpreter) :: tclCommand * tclInterpreter
%
%        # "Sends the @var{Code} converted to string to the @var{TclInterpreter}".
%%------------------------------------------------------------------------

tcltk(format(String,Args),'$wish$'(Strm,_,_)) :-
    !,
    format(Strm,String,Args),
    nl(Strm),
    flush_output(Strm).

%%------------------------------------------------------------------------
%:- pred send_term(+String,+TclInterpreter) :: string * tclInterpreter
%
%        # "Sends the goal executed to the
%          @var{TclInterpreter}. @var{String} has the predicate with
%          unified variables".
%%------------------------------------------------------------------------

send_term(Term,'$wish$'(_,Stream,_)) :-
    write_term(Stream,Term,[]),
    nl(Stream),
    flush_output(Stream).

%%------------------------------------------------------------------------
%:- pred receive_result(-Result,+TclInterpreter) :: string * tclInterpreter
%
%        # "Receives the @var{Result} of the last @em{TclCommand} into
%           the @var{TclInterpreter}. If the @em{TclCommand} is not
%           correct the @em{wish} process is terminated and a message
%           appears showing the error".
%%------------------------------------------------------------------------
receive_result(Result,I) :-
    receive_term(Term,I),
    get_result(Term,Result).

get_result(end_of_file,end_of_file).
get_result(goal(Goal),goal(Goal)).
get_result(tcl_result(Result),Result).
get_result(tcl_eval_finished,tcl_eval_finished).
get_result(tcl_error(Term),_):-
    format(user_error,'tcl_error: ~s~n',[Term]),
    fail.

receive_term(Term,'$wish$'(_,Stream,_)) :-
    read_line_to_codes(Stream,Text),
    (   Text==end_of_file
    ->  Term=end_of_file
    ;   once(analyse_tcl_output(Term,Stream,Text,[]))
    ).
%    format(user_error,"received: ~w~n",[Term]).

analyse_tcl_output(tcl_error(Term),_) -->
    "tcl_error",
    " ",
    codes(Term).

analyse_tcl_output(tcl_eval_finished,_) -->
    "tcl_eval_finished".

analyse_tcl_output(tcl_result(Result),_) -->
    "tcl_result",
    " ",
    codes(Result).

analyse_tcl_output(goal(Goal),Stream) -->
    "goal",
    more_codes(Stream,GoalCodes),
    {  term_to_atom(Goal,GoalCodes) }.

%% empty line !?!?
analyse_tcl_output(Term,Stream) -->
    more_codes(Stream,GoalCodes),
    {  analyse_tcl_output(Term,Stream,GoalCodes,[]) }.

codes(Codes,Codes,[]).

more_codes(Stream,Codes) -->
    { read_line_to_codes(Stream,Codes),
      Codes \== end_of_file }.

%%------------------------------------------------------------------------
%% INITIAL CODE
%%------------------------------------------------------------------------

core('

set event_socket [socket $prolog_host $event_port]
set term_socket [socket $prolog_host $prolog_port]

set tcl_eval_mode 0
set prolog_variables(X) 1
set terms [list]

proc prolog_unset_tcl_eval_mode {} {
  global tcl_eval_mode
  set tcl_eval_mode 0
}

proc prolog_set_tcl_eval_mode {} {
  global tcl_eval_mode
  set tcl_eval_mode 1
}

proc prolog {agoal} {
  global term_socket
  global tcl_eval_mode
  if {$tcl_eval_mode} {
      puts  $term_socket goal
      puts  $term_socket execute_no_result($agoal)
      flush $term_socket
  } else {
      prolog_event execute_no_result($agoal)
  }
}

proc prolog_event {term} {
    global event_socket
    puts $event_socket goal
    puts $event_socket $term
    flush $event_socket
}

proc prolog_cmd {command} {
   global term_socket
#   puts "command: $command"
   set error [catch {set result [uplevel $command]} var]
   if {$error} {
#       puts "error: $var"
       puts  $term_socket "tcl_error $var"
       flush $term_socket
       return $error
   } else {
#       puts "result: $result"
       puts  $term_socket "tcl_result $result"
       flush $term_socket
       puts  $term_socket tcl_eval_finished
       flush $term_socket
       return $error
   }
}


').


%%------------------------------------------------------------------------
%:- pred tcl_delete(+TclInterpreter) :: tclInterpreter # "Given a
%   handle to a Tcl interpreter in variable @var{TclInterpreter}, it
%   deletes the interpreter from the system.".
%%------------------------------------------------------------------------

tcl_delete(I) :-
    delete(I).

%%------------------------------------------------------------------------
%:- pred tcl_eval(+TclInterpreter,+Command,-Result)
%        :: tclInterpreter * tclCommand * string
%
%        # "Evaluates the commands given in @var{Command} in the Tcl
%          interpreter @var{TclInterpreter}. The result will be stored
%          as a string in @var{Result}. If there is an error in
%          @em{Command} an exception is raised. The error messages will
%          be @em{Tcl Exception:} if the error is in the syntax of the
%          Tcl/Tk code or @em{Prolog Exception:}, if the error is in
%          the prolog term.".
%%------------------------------------------------------------------------

tcl_eval_result(Result, X) :-
    (   Result==end_of_file
    ->  delete_deleted(X)
    ;   Result=goal(Goal)
    ->  tcl_eval_event(Goal, X)
    ;   true
    ).

tcl_eval_event(execute(Goal), X) :-
    (   catch(user:Goal,Error,tcl_loop_exit(X,Error))
    ->  send_term(Goal,X)
    ;   send_term(fail,X)
    ).

tcl_eval_event(execute_no_result(Goal), X) :-
    (   catch(user:Goal,Error,tcl_loop_exit(X,Error))
    ->  true
    ;   true
    ).

tcl_eval_event(execute_var_result(Goal,Var), X) :-
    (   catch(user:Goal,Error,tcl_loop_exit(X,Error))
    ->  send_term(Var,X)
    ;   send_term('{}',X)
    ).

tcl_eval_event(execute_all_result(Goal,Var), X) :-
    (   catch(findall(Var,user:Goal,List),Error,tcl_loop_exit(X,Error))
    ->  vars_to_codes(List,Codes,[]),
	atom_codes(Atom,Codes),
	send_term(Atom,X)
    ;   send_term('{}',X)
    ).

tcl_eval_event(execute_boolean_result(Goal), X) :-
    (   catch(user:Goal,Error,tcl_loop_exit(X,Error))
    ->  send_term(true,X)
    ;   send_term(fail,X)
    ).

vars_to_codes([]) -->
    [].
vars_to_codes([H|T]) -->
    format_to_chars("~w ",[H]),
    vars_to_codes(T).

tcl_eval(I, Command, Result) :-
    tcltk_raw_code('prolog_set_tcl_eval_mode', I),
    tcltk_raw_code('prolog_cmd {',I),
    tcltk(Command,I),
    tcltk_raw_code('}',I),
    tcl_eval_aux(Command, I, Result, []),
    tcltk_raw_code('prolog_unset_tcl_eval_mode', I).

tcl_eval_aux(Command, I, Result, AccResult) :-
    (   receive_result(Result1,I)
    ->  tcl_eval_result(Result1, I),
	(   not_waiting_for_more(Result1)
	->  AccResult = Result
	;   tcl_eval_aux(Command, I, Result, Result1)
	)
    ;   true
    ).

not_waiting_for_more(tcl_eval_finished).
not_waiting_for_more(end_of_file).

tk_new(Options, Interp):-
    tk_options(Options,_,Appname,_,Display,_,File),
    !,
    tk_new(Interp,Appname,Display,File).


tk_options(Option,_,_,_,_,_,_):-
    var(Option),
    !,
    fail.
tk_options([],App,App,Disp,Disp,File,File).
tk_options([Option|Options],App0,App,Disp0,Disp,File0,File):-
    nonvar(Option),
    tk_option(Option,App0,App1,Disp0,Disp1,File0,File1),
    tk_options(Options,App1,App,Disp1,Disp,File1,File).

tk_option(file(File),App0,App,Disp0,Disp,_,Filename):-
    !,
    App=App0,
    Disp=Disp0,
    Filename = File.
tk_option(name(Name),_,App,Disp0,Disp,File0,File):-
    !,
    App=Name,
    Disp=Disp0,
    File=File0.
tk_option(display(Display),App0,App,_,Disp,File0,File):-
    !,
    App=App0,
    Disp=Display,
    File=File0.
tk_option(top_level_events,App,App,Disp,Disp,File,File) :-
    !.

tk_option(Option,App,App,Disp,Disp,File,File):-
    format(user_error,'warning tk_new ignores option ~w~n',[Option]).

% Need to put in the second condition of the if case no display
tk_new(Interp,Appname,Display,File):-
    (   nonvar(Appname)
    ->  atom_concat(' -name ',Appname,Str1)
    ;   atom_concat(' ',' ',Str1)
    ),
    (   nonvar(Display)
    ->  atom_concat(' -display ',Display,Str2)
    ;   atom_concat(' ',' ',Str2)
    ),
    (   nonvar(File)
    ->  atom_concat(' ',File,Str3)
    ;   atom_concat(' ',' ',Str3)
    ),
    atom_concat(Str1,Str2,Str4),
    atom_concat(Str4,Str3,Options),
    new_interp(Interp,Options).

tcl_loop_exit(_X,E):-
    format(user_error,'Prolog exception: ~w~n',[E]),
    fail.

%%------------------------------------------------------------------------
%:- pred tk_main_loop(+TclInterpreter)
%
%        :: tclInterpreter
%
%        # "Passes control to Tk until all windows are gone.".
%%------------------------------------------------------------------------

tk_main_loop(X) :-
    thread_self(Id),
    format(user_error,'tk_main_loop in thread ~w~n',[Id]),
    tk_main_loop_(X).

tk_main_loop_(X):-
    tk_next_event(X,Event),
    (   Event == end_of_file  % wish process must have died!
    ->  hdrug_gui:halt_x
    ;   Event=goal(Goal),
	tcl_eval_event(Goal,X),
	tk_main_loop_(X)
    ).

tk_next_event('$wish$'(_,_,Stream),Event) :-
    catch(read_line_to_codes(Stream,Text),_,Text=end_of_file),
    (   Text == end_of_file
    ->  Event = end_of_file
    ;   once(analyse_tcl_output(Event,Stream,Text,[]))
    ).

