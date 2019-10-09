user_max(L,Max) :-
	Max is 60000 + (L * L * 1000).

% 3
sentence(30,[vandaag,slaapt,jane]).
sentence(39,[de,mannen,slapen]).
% 6
sentence(60,[tarzan,zegt,dat,jane,vandaag,slaapt]).
sentence(69,[mannen,slapen,met,vrouwen,met,jane]).
% 9
sentence(91,[vandaag,heeft,tarzan,geslapen,met,mannen,met,de,vrouwen]).
sentence(99,[omdat,tarzan,slaapt,heeft,jane,geprobeerd,tarzan,te,kussen]).
%12
sentence(120,[jane,heeft,vrouwen,met,tarzan,in,de,puree,zien,zitten,met,jane]).
sentence(129,[jane,heeft,met,de,mannen,de,vrouwen,gedwongen,tarzan,te,zien,slapen]).
%15
sentence(150,[om,vrouwen,de,mannen,tarzan,te,zien,helpen,kussen,heeft,de,vrouw,jane,vandaag,gedwongen]).
sentence(159,[mannen,proberen,te,zeggen,dat,vrouwen,hebben,gezegd,dat,jane,vandaag,met,tarzan,heeft,geslapen]).
%18
sentence(180,[dat,jane,tarzan,kust,heeft,de,vrouw,vandaag,gezegd,met,de,mannen,met,de,vrouwen,met,de,mannen]).
sentence(189,[tarzan,heeft,vandaag,geprobeerd,om,de,vrouwen,te,dwingen,te,zeggen,dat,tarzan,met,de,vrouwen,heeft,geslapen]).

sentence(210,[jane,heeft,tarzan,gedwongen,om,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt]).
sentence(219,[om,de,vrouwen,de,mannen,tarzan,te,zien,helpen,kussen,heeft,de,vrouw,jane,gedwongen,met,de,vrouwen,met,de,mannen]).
/*
sentence(240,[de,man,met,mannen,heeft,tarzan,gedwongen,om,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt]).
sentence(249,[om,vrouwen,met,de,mannen,in,de,puree,te,zien,zitten,heeft,tarzan,vandaag,geprobeerd,om,de,vrouwen,in,de,puree,te,zien,zitten]).

sentence(270,[tarzan,heeft,vandaag,gezegd,dat,de,mannen,met,de,vrouwen,met,de,mannen,hebben,geprobeerd,om,te,proberen,om,de,vrouwen,te,zien,slapen,met,de,vrouwen]).
sentence(279,[tarzan,heeft,vandaag,gezegd,dat,de,vrouwen,de,mannen,mannen,in,de,puree,hebben,zien,helpen,zitten,omdat,jane,tarzan,heeft,proberen,te,kussen,met,de,vrouwen]).

sentence(300,[vandaag,heeft,de,man,met,de,vrouwen,met,de,mannen,geprobeerd,om,met,de,mannen,te,proberen,tarzan,te,dwingen,te,zeggen,dat,de,mannen,vandaag,met,jane,hebben,geslapen]).
sentence(309,[de,vrouw,met,mannen,heeft,tarzan,gedwongen,om,de,vrouwen,met,de,vrouwen,te,kussen,omdat,tarzan,jane,heeft,proberen,te,dwingen,te,zeggen,dat,tarzan,slaapt,om,te,slapen]).
*/





lf(1,say(tarzan,[vandaag,sleep(jane)])).
lf(2,[met(pl(man)),perf(force(jane,pl(woman),see(pl(woman),sleep(tarzan))))]).
lf(3,[because(sleep(tarzan)),perf(try(jane,kiss(jane,tarzan)))]).
lf(4,sleep(pl(man))).
lf(5,try(pl(man),say(pl(man),perf(say(pl(woman),[vandaag,[met(tarzan),perf(sleep(jane))]]))))).
