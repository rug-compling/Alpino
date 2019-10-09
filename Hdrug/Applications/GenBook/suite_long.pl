:- prolog_flag(redefine_warnings,_,off).

maximum_per_word(L,Max) :-
	Max is 60000 + (L * L * 1000).

:- prolog_flag(redefine_warnings,_,on).

% :- flag(useful_try_check,_,off).

% 3
sentence(30,[vandaag,slaapt,jane]).
sentence(31,[jane,kust,tarzan]).
sentence(32,[jane,heeft,geslapen]).
sentence(33,[jane,kust,mannen]).
sentence(34,[mannen,kussen,jane]).
sentence(35,[jane,slaapt,vandaag]).
sentence(36,[mannen,slapen,vandaag]).
sentence(37,[mannen,kussen,vrouwen]).
sentence(38,[de,man,slaapt]).
sentence(39,[de,mannen,slapen]).
% 6
sentence(60,[tarzan,zegt,dat,jane,vandaag,slaapt]).
sentence(61,[de,mannen,proberen,vandaag,te,slapen]).
sentence(62,[de,mannen,proberen,om,te,slapen]).
sentence(63,[de,mannen,hebben,geprobeerd,te,slapen]).
sentence(64,[tarzan,heeft,vandaag,jane,zien,slapen]).
sentence(65,[mannen,kussen,tarzan,met,de,vrouwen]).
sentence(66,[jane,slaapt,vandaag,met,de,mannen]).
sentence(67,[de,mannen,zien,tarzan,jane,kussen]).
sentence(68,[mannen,proberen,jane,vandaag,te,kussen]).
sentence(69,[mannen,slapen,met,vrouwen,met,jane]).
% 9
%%sentence(90,[dat,de,mannen,tarzan,kussen,heeft,de,vrouw,gezegd]).
sentence(91,[vandaag,heeft,tarzan,geslapen,met,mannen,met,de,vrouwen]).
sentence(92,[jane,slaapt,omdat,tarzan,jane,heeft,proberen,te,kussen]).
sentence(93,[jane,heeft,geprobeerd,tarzan,de,vrouwen,te,zien,kussen]).
sentence(94,[omdat,jane,slaapt,heeft,tarzan,geprobeerd,vandaag,te,slapen]).
sentence(95,[de,vrouwen,proberen,vandaag,met,de,mannen,te,slapen]).
sentence(96,[jane,kust,tarzan,met,mannen,met,mannen,met,mannen]).
sentence(97,[om,te,slapen,heeft,jane,geprobeerd,tarzan,te,kussen]).
sentence(98,[jane,heeft,tarzan,mannen,zien,kussen,met,de,vrouw]).
sentence(99,[omdat,tarzan,slaapt,heeft,jane,geprobeerd,tarzan,te,kussen]).
%12
sentence(120,[jane,heeft,vrouwen,met,tarzan,in,de,puree,zien,zitten,met,jane]).
sentence(121,[om,tarzan,te,dwingen,te,zeggen,dat,tarzan,slaapt,heeft,jane,geslapen]).
sentence(122,[jane,slaapt,met,de,mannen,met,de,mannen,met,mannen,met,jane]).
sentence(123,[dat,jane,vandaag,heeft,geslapen,heeft,de,man,vandaag,met,tarzan,gezegd]).
sentence(124,[om,tarzan,te,kussen,om,jane,te,kussen,slaapt,jane,met,tarzan]).
sentence(125,[om,met,mannen,in,de,puree,te,zitten,heeft,jane,vandaag,geprobeerd]).
sentence(126,[jane,heeft,vandaag,geprobeerd,om,met,mannen,te,slapen,met,de,vrouwen]).
sentence(127,[de,mannen,hebben,vandaag,met,vrouwen,gezegd,dat,jane,met,vrouwen,slaapt]).
sentence(128,[in,de,puree,hebben,de,vrouwen,de,mannen,met,tarzan,zien,zitten]).
sentence(129,[jane,heeft,met,de,mannen,de,vrouwen,gedwongen,tarzan,te,zien,slapen]).
%15
sentence(150,[om,vrouwen,de,mannen,tarzan,te,zien,helpen,kussen,heeft,de,vrouw,jane,vandaag,gedwongen]).
sentence(151,[tarzan,heeft,de,mannen,gedwongen,om,vandaag,met,de,vrouwen,de,vrouwen,te,zien,slapen]).
sentence(152,[dat,tarzan,de,vrouwen,heeft,gedwongen,te,slapen,heeft,tarzan,met,vrouwen,met,mannen,gezegd]).
sentence(153,[de,man,probeert,vandaag,met,mannen,vandaag,de,mannen,met,de,mannen,te,zien,slapen]).
sentence(154,[omdat,tarzan,de,vrouwen,kust,heeft,jane,de,vrouwen,gedwongen,te,proberen,om,te,slapen]).
sentence(155,[om,vandaag,met,de,mannen,vrouwen,in,de,puree,te,zien,zitten,heeft,tarzan,geprobeerd]).
sentence(156,[dat,jane,probeert,om,te,slapen,heeft,tarzan,vandaag,gezegd,met,de,mannen,met,jane]).
sentence(157,[vandaag,hebben,de,mannen,met,de,mannen,met,de,mannen,geprobeerd,om,tarzan,te,kussen]).
sentence(158,[met,jane,heeft,tarzan,vandaag,de,vrouwen,proberen,te,dwingen,om,tarzan,te,zien,slapen]).
sentence(159,[mannen,proberen,te,zeggen,dat,vrouwen,hebben,gezegd,dat,jane,vandaag,met,tarzan,heeft,geslapen]).
%18
sentence(180,[dat,jane,tarzan,kust,heeft,de,vrouw,vandaag,gezegd,met,de,mannen,met,de,vrouwen,met,de,mannen]).
sentence(181,[om,vandaag,te,slapen,heeft,jane,vandaag,met,de,mannen,geprobeerd,om,tarzan,vandaag,jane,te,zien,kussen]).
sentence(182,[vandaag,hebben,de,mannen,de,vrouwen,gedwongen,om,te,zeggen,dat,tarzan,probeert,om,met,jane,te,slapen]).
% sentence(183,[de,mannen,met,de,vrouwen,met,de,vrouwen,hebben,geprobeerd,om,te,proberen,om,jane,te,zien,slapen]).
sentence(184,[tarzan,heeft,gezegd,dat,tarzan,slaapt,met,de,mannen,met,de,vrouwen,om,de,vrouwen,vandaag,te,kussen]).
sentence(185,[vandaag,heeft,de,vrouw,met,tarzan,gezegd,dat,de,vrouwen,proberen,om,te,proberen,te,slapen,met,jane]).
sentence(186,[omdat,jane,de,vrouwen,in,de,puree,heeft,zien,zitten,probeert,tarzan,de,mannen,te,dwingen,te,slapen]).
sentence(187,[jane,heeft,mannen,vrouwen,tarzan,zien,helpen,kussen,om,te,proberen,de,vrouwen,met,jane,te,zien,slapen]).
sentence(188,[vandaag,heeft,tarzan,geprobeerd,om,te,zeggen,dat,jane,tarzan,vandaag,ziet,slapen,omdat,tarzan,jane,ziet,slapen]).
sentence(189,[tarzan,heeft,vandaag,geprobeerd,om,de,vrouwen,te,dwingen,te,zeggen,dat,tarzan,met,de,vrouwen,heeft,geslapen]).


sentence(210,[jane,heeft,tarzan,gedwongen,om,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt]).
sentence(211,[vandaag,hebben,de,vrouwen,de,mannen,de,vrouwen,zien,kussen,omdat,jane,heeft,geprobeerd,om,te,zeggen,dat,de,mannen,slapen]).
sentence(212,[vandaag,hebben,de,mannen,met,de,vrouwen,de,vrouwen,gedwongen,om,te,zeggen,dat,tarzan,probeert,om,met,jane,te,slapen]).
sentence(213,[de,mannen,met,de,vrouwen,met,de,vrouwen,hebben,geprobeerd,om,te,proberen,om,jane,te,zien,slapen,met,de,vrouw]).
sentence(214,[tarzan,heeft,gezegd,dat,tarzan,slaapt,met,de,mannen,met,de,vrouwen,om,de,vrouwen,vandaag,met,de,mannen,te,kussen]).
sentence(215,[vandaag,heeft,de,vrouw,met,tarzan,gezegd,dat,de,vrouwen,proberen,om,te,proberen,met,de,man,te,slapen,met,jane]).
sentence(216,[omdat,jane,de,vrouwen,met,de,mannen,in,de,puree,heeft,zien,zitten,probeert,tarzan,de,mannen,te,dwingen,te,slapen]).
sentence(217,[tarzan,heeft,de,mannen,de,vrouwen,zien,kussen,omdat,jane,tarzan,heeft,zien,slapen,omdat,tarzan,jane,heeft,gekust,met,tarzan]).
sentence(218,[de,mannen,met,de,vrouwen,met,de,vrouwen,hebben,vandaag,geprobeerd,omdat,tarzan,slaapt,de,mannen,te,dwingen,te,hebben,geslapen]).
sentence(219,[om,de,vrouwen,de,mannen,tarzan,te,zien,helpen,kussen,heeft,de,vrouw,jane,gedwongen,met,de,vrouwen,met,de,mannen]).

sentence(240,[de,man,met,mannen,heeft,tarzan,gedwongen,om,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt]).
sentence(241,[vandaag,hebben,de,vrouwen,met,de,vrouwen,de,mannen,de,vrouwen,zien,kussen,omdat,jane,heeft,geprobeerd,om,te,zeggen,dat,de,mannen,slapen]). %fail
sentence(242,[vandaag,hebben,de,mannen,met,de,vrouwen,de,vrouwen,gedwongen,om,met,de,mannen,te,zeggen,dat,tarzan,probeert,om,met,jane,te,slapen]).
sentence(243,[de,mannen,met,de,vrouwen,met,de,vrouwen,met,de,vrouwen,hebben,geprobeerd,om,te,proberen,om,jane,te,zien,slapen,met,de,vrouw]).
sentence(244,[de,man,met,vrouwen,heeft,gezegd,dat,tarzan,slaapt,met,de,mannen,met,de,vrouwen,om,de,vrouwen,vandaag,met,de,mannen,te,kussen]).
sentence(245,[vandaag,heeft,de,vrouw,met,tarzan,gezegd,dat,de,vrouwen,proberen,om,te,proberen,vandaag,met,jane,met,de,man,te,slapen,met,jane]).
sentence(246,[de,mannen,met,jane,heeft,jane,vandaag,gedwongen,om,te,zeggen,dat,tarzan,de,vrouwen,met,de,mannen,heeft,zien,slapen,met,de,vrouwen]).
sentence(247,[omdat,tarzan,de,vrouwen,de,mannen,heeft,zien,kussen,om,de,vrouwen,in,de,puree,te,zien,zitten,heeft,tarzan,vandaag,met,jane,geslapen]).
sentence(248,[de,mannen,hebben,vandaag,geprobeerd,om,de,vrouwen,met,de,mannen,vandaag,de,vrouw,te,zien,kussen,omdat,jane,vandaag,de,mannen,ziet,slapen]).
sentence(249,[om,vrouwen,met,de,mannen,in,de,puree,te,zien,zitten,heeft,tarzan,vandaag,geprobeerd,om,de,vrouwen,in,de,puree,te,zien,zitten]).

sentence(270,[tarzan,heeft,vandaag,gezegd,dat,de,mannen,met,de,vrouwen,met,de,mannen,hebben,geprobeerd,om,te,proberen,om,de,vrouwen,te,zien,slapen,met,de,vrouwen]).
sentence(271,[jane,heeft,vandaag,de,mannen,gedwongen,de,vrouwen,met,de,mannen,in,de,puree,te,zien,zitten,omdat,tarzan,heeft,geprobeerd,met,de,vrouwen,mannen,te,kussen]).
sentence(272,[de,mannen,met,de,vrouwen,met,de,mannen,hebben,vandaag,gezegd,dat,de,mannen,hebben,geprobeerd,om,de,vrouwen,te,kussen,omdat,jane,tarzan,heeft,zien,slapen]).
sentence(273,[de,mannen,heeft,jane,vandaag,vandaag,vandaag,gedwongen,om,te,zeggen,dat,tarzan,vandaag,met,de,mannen,de,vrouwen,met,de,mannen,heeft,zien,slapen,met,jane]).
sentence(274,[omdat,tarzan,heeft,gezegd,dat,de,mannen,met,de,vrouwen,vandaag,met,de,vrouwen,hebben,geprobeerd,om,de,mannen,te,kussen,hebben,de,mannen,vrouwen,zien,slapen]).
sentence(275,[de,man,met,mannen,heeft,tarzan,met,de,vrouwen,gedwongen,om,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt]).
sentence(276,[omdat,jane,de,vrouwen,met,de,mannen,in,de,puree,heeft,zien,zitten,met,de,vrouwen,probeert,tarzan,de,mannen,te,dwingen,met,de,vrouwen,te,slapen]).
sentence(277,[de,man,met,tarzan,heeft,vandaag,met,vrouwen,met,de,mannen,gezegd,dat,de,vrouwen,met,de,mannen,vandaag,hebben,geprobeerd,de,vrouwen,met,mannen,te,kussen]).
sentence(278,[vandaag,heeft,de,man,met,de,vrouwen,met,de,mannen,geprobeerd,om,te,proberen,tarzan,te,dwingen,te,zeggen,dat,de,mannen,vandaag,met,jane,hebben,geslapen]).
sentence(279,[tarzan,heeft,vandaag,gezegd,dat,de,vrouwen,de,mannen,mannen,in,de,puree,hebben,zien,helpen,zitten,omdat,jane,tarzan,heeft,proberen,te,kussen,met,de,vrouwen]).

sentence(300,[vandaag,heeft,de,man,met,de,vrouwen,met,de,mannen,geprobeerd,om,met,de,mannen,te,proberen,tarzan,te,dwingen,te,zeggen,dat,de,mannen,vandaag,met,jane,hebben,geslapen]).
sentence(301,[dat,de,mannen,met,de,vrouwen,de,mannen,hebben,zien,proberen,te,slapen,heeft,jane,vandaag,met,tarzan,met,vrouwen,proberen,te,zeggen,omdat,jane,vandaag,de,mannen,vandaag,kust]).
sentence(302,[om,met,de,mannen,de,vrouwen,te,zien,slapen,hebben,mannen,met,vrouwen,vandaag,de,mannen,gedwongen,om,te,zeggen,dat,de,vrouwen,vandaag,met,de,mannen,vandaag,hebben,geslapen]).
sentence(303,[vandaag,heeft,jane,gezegd,dat,de,mannen,de,vrouwen,hebben,proberen,te,dwingen,om,te,zeggen,dat,jane,vandaag,vandaag,de,mannen,met,de,vrouwen,met,de,mannen,heeft,gekust]).
sentence(304,[tarzan,heeft,de,mannen,gedwongen,om,te,zeggen,dat,jane,met,de,vrouwen,met,de,mannen,vandaag,de,vrouwen,heeft,zien,slapen,omdat,tarzan,vandaag,de,vrouwen,met,tarzan,kust]).
sentence(305,[tarzan,kust,jane,omdat,jane,zegt,dat,jane,heeft,gezegd,dat,jane,heeft,proberen,te,zeggen,dat,jane,heeft,gezegd,dat,jane,heeft,gezegd,dat,tarzan,zegt,dat,jane,slaapt]).
sentence(306,[vandaag,hebben,de,mannen,met,de,vrouwen,de,vrouwen,met,de,mannen,gedwongen,om,te,proberen,te,zeggen,dat,jane,de,vrouwen,de,mannen,heeft,zien,helpen,slapen,met,tarzan]).
sentence(307,[tarzan,heeft,vandaag,geprobeerd,om,de,mannen,met,vrouwen,met,vrouwen,te,dwingen,te,zeggen,dat,jane,de,mannen,heeft,gedwongen,om,de,vrouwen,de,mannen,te,zien,helpen,slapen]).
sentence(308,[tarzan,heeft,vandaag,met,de,vrouwen,gezegd,dat,de,vrouwen,de,mannen,mannen,in,de,puree,hebben,zien,helpen,zitten,omdat,jane,tarzan,heeft,proberen,te,kussen,met,de,vrouwen]).
sentence(309,[de,vrouw,met,mannen,heeft,tarzan,gedwongen,om,de,vrouwen,met,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt,om,te,slapen]).






