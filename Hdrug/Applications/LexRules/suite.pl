user_max(_,1000000).

% intransitives
sentence(a,[marie,slaapt]).

% transitives
sentence(b,[jan,heeft,geslapen]).

% modification
sentence(c,[jan,kust,vandaag,marie]).

% verb-raising
sentence(d,[vandaag,wil,jan,marie,kussen]).

% verb-raising
%sentence(e,[jan,heeft,marie,willen,kussen]).

% vp-topicalisation
sentence(f,[marie,gekust,heeft,jan,vandaag]).

% 
sentence(g,[jan,probeert,vandaag,op,te,scheppen]).

% partial vp-topicalisation
sentence(h,[te,kussen,proberen,de,mannen,de,vrouwen]).

% aci
% sentence(i,[jan,heeft,de,vrouwen,zien,proberen,te,slapen]).
sentence(i,[jan,heeft,vrouwen,zien,slapen]).

% relatives
sentence(j,[de,man,die,slaapt,slaapt]).

% extraposed modifiers of a noun-phrase over the verb
sentence(k,[jan,probeert,de,vrouw,te,kussen,die,slaapt]).

% pied piping
sentence(l,[de,man,met,wiens,boek,jan,slaapt,kust,de,vrouwen]).
