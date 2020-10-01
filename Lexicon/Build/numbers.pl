:- expects_dialect(sicstus).

m(Stem, number(Sub), PN) :-
    number(PN,Sub),
    (	atom(PN)
    ->	PN = Stem
    ;	hdrug_util:concat_all(PN,Stem,' ')
    ).

m(Stem, number(Sub), PN) :-
    number(PN, Stem, Sub).

number(allereerste,aller_één,rang).
number(eerste,één,rang).
number(derde,drie,rang).

number(miljoenste,miljoen,rang).
number(miljardste,miljard,rang).
number(triljoenste,triljoen,rang).

number(zoveelste,zoveel,rang).

number(allerlaatste,aller_laatst,rang).      % we staan allerlaatste
number(laatste,laatst,rang).		% we staan laatste
number(voorlaatste,voor_laatst,rang).	% we staan voorlaatste

number(anderhalve,anderhalf,hoofd(both)).

%% de letter l wordt ook vaak voor 1 gebruikt
number(l,'1',hoofd(sg_num)).

number('¼',hoofd(both)).    % 1/4
number('½',hoofd(both)).    % 1/2
number('¾',hoofd(both)).    % 3/4

number('één-honderdste', hoofd(both)).

number(tig, hoofd(both)).

number(zoveel,hoofd(both)).	% in/om de zoveel tijd/maanden/weken
number([zo,veel],hoofd(both)).

number(anderhalf,hoofd(both)).
number(één,hoofd(sg_num)).

number(N,hoofd(pl_num)) :-
    nm(N).

nm(tweetjes).

nm(nul).

nm([nul,komma,nul]).

