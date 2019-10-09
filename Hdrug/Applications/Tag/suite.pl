%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% sample sentences for statistics %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% user_max(Length,MaxMilli)
user_max(L,Max) :-
	Max is 10000 + (L * L * 1000).

sentence(a1,[john,left,today]).
sentence(a2,[who,did,john,say,mary,saw,today]).
sentence(a3,[who,did,john,say,mary,saw,with,the,telescope,behind,the,
              periscope,without,the,glasses]).
sentence(a4,[who,did,john,say,mary,saw,with,the,telescope,behind,the,
              periscope,without,the,glasses,with,a,telescope]).
sentence(a5,[who,did,john,say,mary,saw,with,the,telescope,behind,the,
              periscope,without,the,glasses,with,a,telescope,without,
	      the,periscope]).
sentence(a6,[who,did,john,say,mary,saw,with,the,telescope,behind,the,
              periscope,without,the,glasses,with,a,telescope,without,
	      the,periscope,behind,the,boy,s,periscope]).
sentence(b,[who,did,john,see,with,the,telescope,without,the,periscope,
              behind,the,glasses]).
sentence(c,[who,did,john,say,the,boy,said,mary,saw,today,without,the,boy,s,
              very,little,pretty,periscope,s,glasses,behind,mary,s,periscope]).
/* sentence(d,[who,did,john,say,the,boy,said,mary,saw,today,without,the,boy,s,
              very,little,pretty,periscope,s,glasses,behind,mary,s,periscope,
              with,the,boy,s,very,little,pretty,periscope,s,glasses,behind,mary,
              s,periscope]).
*/
