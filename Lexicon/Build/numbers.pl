:- expects_dialect(sicstus).

m(Stem, number(Sub), PN) :-
    number(PN,Sub),
    (	atom(PN)
    ->	PN = Stem
    ;	hdrug_util:concat_all(PN,Stem,' ')
    ).

number('¼',hoofd(both)).    % 1/4
number('½',hoofd(both)).    % 1/2
number('¾',hoofd(both)).    % 3/4

number('één-honderdste', hoofd(both)).

%% most are in lex_more.pl (really this should
%% have been the other way around)
number(allereerste,rang).
number(eerste,rang).
number(derde,rang).

number(miljoenste,rang).
number(miljardste,rang).
number(triljoenste,rang).

number(zoveelste,rang).

number(allerlaatste,rang).      % we staan allerlaatste
number(laatste,rang).		% we staan laatste
number(voorlaatste,rang).	% we staan voorlaatste

number(tig, hoofd(both)).

number(zoveel,hoofd(both)).	% in/om de zoveel tijd/maanden/weken
number([zo,veel],hoofd(both)).

number(anderhalf,hoofd(both)).
number(anderhalve,hoofd(both)).

number(één,hoofd(sg_num)).

%% de letter l wordt ook vaak voor 1 gebruikt
number(l,hoofd(sg_num)).

number(N,hoofd(pl_num)) :-
    nm(N).

/* these are adjectives:

tweeënhalve week later
*tweeënhalve weken later

nm(tweeënhalf).
nm(tweeëneenhalf).
nm(tweeënhalve).
nm(tweeëneenhalve).
nm(drieënhalf).
nm(drieëneenhalf).
nm(drieënhalve).
nm(drieëneenhalve).
nm(vierenhalve).
nm(vierenhalf).
nm(viereneenhalve).
nm(viereneenhalf).
nm(vijfenhalve).
nm(vijfenhalf).
nm(vijfeneenhalve).
nm(vijfeneenhalf).
nm(zesenhalve).
nm(zesenhalf).
nm(zeseneenhalve).
nm(zeseneenhalf).
nm(zevenenhalve).
nm(zevenenhalf).
nm(zeveneneenhalve).
nm(zeveneneenhalf).
nm(achtenhalve).
nm(achtenhalf).
nm(achteneenhalve).
nm(achteneenhalf).
nm(negenenhalve).
nm(negenenhalf).
nm(negeneneenhalve).
nm(negeneneenhalf).
*/

nm(tweetjes).

nm(nul).

nm([nul,komma,nul]).

