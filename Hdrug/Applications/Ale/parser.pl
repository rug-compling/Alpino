:- module(parser,[]).

parse(o(ale(Tag,SVs,Iqs,EdgeNo),Str,_)) :-
	rec_modified(Str,Tag,SVs,Iqs,EdgeNo).

:- dynamic user:parsing/1.

rec_modified(Ws,Tag,SVs,IqsOut,Index):-
  user:clear,
  user:assert(parsing(Ws)),
  user:asserta(num(0)),
  user:reverse_count(Ws,[],WsRev,0,Length),
  user:CLength is Length - 1,
  user:functor(Chart,chart,CLength),
  user:build(WsRev,Length,Chart),
  user:retract(to_rebuild(Index)),
  user:clause(edge(Index,0,Length,Tag,SVs,IqsIn,_,_),true),
  user:extensionalise(Tag,SVs,IqsIn),
  user:check_inequal(IqsIn,IqsOut).

