%% General predicates for dealing with character encodings.  Right now we
%% handle all of latin1.  When the system is extended to deal with mixed
%% Burmese/Cherokee texts, this will require modification.
%%
%% as of today, we use latin9. I don't think any changes are required in
%% this file.
%%
%% now we use UTF-8. Still, no changes required, although perhaps there are
%% upper characters in Cherokee that we haven't captured.

:- module(alpino_latin1, [isupper/1,
			  islower/1,
			  isdigit/1,
			  isalpha/1,
			  isaccented/1,
			  vowel/1,
			  cons/1,
			  accent_chars/2,
			  deaccent_chars/2, deaccent_chars/4, deaccent/2,
			  toupper/2, 
			  tolower/2]).

:- expects_dialect(sicstus).

isupper(65).  isupper(66). isupper(67). isupper(68). isupper(69). isupper(70).
isupper(71).  isupper(72). isupper(73). isupper(74). isupper(75). isupper(76).
isupper(77).  isupper(78). isupper(79). isupper(80). isupper(81). isupper(82).
isupper(83).  isupper(84). isupper(85). isupper(86). isupper(87). isupper(88).
isupper(89).  isupper(90). isupper(192). isupper(193). isupper(194). 
isupper(195). isupper(196). isupper(197). isupper(198). isupper(199). 
isupper(200). isupper(201). isupper(202). isupper(203). isupper(204). 
isupper(205). isupper(206). isupper(207). isupper(208). isupper(209).
isupper(210). isupper(211). isupper(212). isupper(213). isupper(214).
isupper(216). isupper(217). isupper(218). isupper(219). isupper(220).
isupper(221). isupper(222).

%% extensions to latin-1
%% for the time being, only for isupper.
%%% https://en.wikipedia.org/wiki/List_of_Unicode_characters
isupper(256).
isupper(258).
isupper(260).
isupper(262).
isupper(264).
isupper(266).
isupper(268).
isupper(270).
isupper(272).
isupper(274).
isupper(276).
isupper(278).
isupper(280).
isupper(282).
isupper(284).
isupper(286).
isupper(288).
isupper(290).
isupper(292).
isupper(294).
isupper(296).
isupper(298).
isupper(300).
isupper(302).
isupper(304).
isupper(306).
isupper(308).
isupper(310).
isupper(313).
isupper(315).
isupper(317).
isupper(319).
isupper(321).
isupper(323).
isupper(325).
isupper(327).
isupper(330).
isupper(332).
isupper(334).
isupper(336).
isupper(338).
isupper(339).
isupper(340).
isupper(342).
isupper(344).
isupper(346).
isupper(348).
isupper(350).
isupper(352).
isupper(353).
isupper(354).
isupper(356).
isupper(358).
isupper(360).
isupper(362).
isupper(364).
isupper(366).
isupper(368).
isupper(370).
isupper(372).
isupper(374).
isupper(376).
isupper(377).
isupper(379).
isupper(381).
isupper(385).
isupper(386).
isupper(388).
isupper(390).
isupper(391).
isupper(393).
isupper(394).
isupper(395).
isupper(398).
isupper(399).
isupper(400).
isupper(401).
isupper(403).
isupper(404).
isupper(406).
isupper(407).
isupper(408).
isupper(412).
isupper(413).
isupper(415).
isupper(416).
isupper(418).
isupper(420).
isupper(423).
isupper(425).
isupper(428).
isupper(430).
isupper(431).
isupper(433).
isupper(434).
isupper(435).
isupper(437).
isupper(439).
isupper(440).
isupper(444).
isupper(452).
isupper(453).
isupper(455).
isupper(456).
isupper(458).
isupper(459).
isupper(461).
isupper(463).
isupper(465).
isupper(467).
isupper(469).
isupper(471).
isupper(473).
isupper(475).
isupper(478).
isupper(480).
isupper(482).
isupper(484).
isupper(486).
isupper(488).
isupper(490).
isupper(492).
isupper(494).
isupper(497).
isupper(498).
isupper(500).
isupper(502).
isupper(503).
isupper(504).
isupper(506).
isupper(508).
isupper(510).
isupper(512).
isupper(514).
isupper(516).
isupper(518).
isupper(520).
isupper(522).
isupper(524).
isupper(526).
isupper(528).
isupper(530).
isupper(532).
isupper(534).
isupper(536).
isupper(538).
isupper(540).
isupper(542).
isupper(544).
isupper(546).
isupper(548).
isupper(550).
isupper(552).
isupper(554).
isupper(556).
isupper(558).
isupper(560).
isupper(562).
isupper(570).
isupper(571).
isupper(573).
isupper(574).
isupper(577).
isupper(579).
isupper(580).
isupper(581).
isupper(582).
isupper(584).
isupper(586).
isupper(588).
isupper(590).

islower(97). islower(98). islower(99). islower(100). islower(101).
islower(102). islower(103). islower(104). islower(105). islower(106).
islower(107). islower(108). islower(109). islower(110). islower(111).
islower(112). islower(113). islower(114). islower(115). islower(116).
islower(117). islower(118). islower(119). islower(120). islower(121).
islower(122). islower(223). islower(224). islower(225). islower(226).
islower(227). islower(228). islower(229). islower(230). islower(231).
islower(232). islower(233). islower(234). islower(235). islower(236).
islower(237). islower(238). islower(239). islower(240). islower(241).
islower(242). islower(243). islower(244). islower(245). islower(246).
islower(248). islower(249). islower(250). islower(251). islower(252).
islower(253). islower(254). islower(255).

isdigit(48). isdigit(49). isdigit(50). isdigit(51). isdigit(52).
isdigit(53). isdigit(54). isdigit(55). isdigit(56). isdigit(57).

isalpha(65). isalpha(66). isalpha(67). isalpha(68). isalpha(69).
isalpha(70). isalpha(71). isalpha(72). isalpha(73). isalpha(74).
isalpha(75). isalpha(76). isalpha(77). isalpha(78). isalpha(79).
isalpha(80). isalpha(81). isalpha(82). isalpha(83).
isalpha(84). isalpha(85). isalpha(86). isalpha(87). isalpha(88).
isalpha(89). isalpha(90). isalpha(97). isalpha(98). isalpha(99).
isalpha(100). isalpha(101). isalpha(102). isalpha(103). isalpha(104).
isalpha(105). isalpha(106). isalpha(107). isalpha(108). isalpha(109).
isalpha(110). isalpha(111). isalpha(112). isalpha(113). isalpha(114).
isalpha(115). isalpha(116). isalpha(117). isalpha(118). isalpha(119).
isalpha(120). isalpha(121). isalpha(122). isalpha(192). isalpha(193).
isalpha(194). isalpha(195). isalpha(196). isalpha(197). isalpha(198).
isalpha(199). isalpha(200). isalpha(201). isalpha(202). isalpha(203).
isalpha(204). isalpha(205). isalpha(206). isalpha(207). isalpha(208).
isalpha(209). isalpha(210). isalpha(211). isalpha(212). isalpha(213).
isalpha(214). isalpha(216). isalpha(217). isalpha(218). isalpha(219).
isalpha(220). isalpha(221). isalpha(222). isalpha(223). isalpha(224).
isalpha(225). isalpha(226). isalpha(227). isalpha(228). isalpha(229).
isalpha(230). isalpha(231). isalpha(232). isalpha(233). isalpha(234).
isalpha(235). isalpha(236). isalpha(237). isalpha(238). isalpha(239).
isalpha(240). isalpha(241). isalpha(242). isalpha(243). isalpha(244).
isalpha(245). isalpha(246). isalpha(248). isalpha(249). isalpha(250).
isalpha(251). isalpha(252). isalpha(253). isalpha(254). isalpha(255).

isaccented(192). isaccented(193). isaccented(194). isaccented(195).
isaccented(196). isaccented(197). isaccented(199). isaccented(200).
isaccented(201). isaccented(202). isaccented(203). isaccented(204).
isaccented(205). isaccented(206). isaccented(207). isaccented(209).
isaccented(210). isaccented(211). isaccented(212). isaccented(213).
isaccented(214). isaccented(216). isaccented(217). isaccented(218).
isaccented(219). isaccented(220). isaccented(221). isaccented(224).
isaccented(225). isaccented(226). isaccented(227). isaccented(228).
isaccented(229). isaccented(231). isaccented(232). isaccented(233).
isaccented(234). isaccented(235). isaccented(236). isaccented(237).
isaccented(238). isaccented(239). isaccented(241). isaccented(242).
isaccented(243). isaccented(244). isaccented(245). isaccented(246).
isaccented(248). isaccented(249). isaccented(250). isaccented(251).
isaccented(252). isaccented(253). isaccented(255). 

toupper(I,J) :-
    (   toupper_(I,K)
    ->  J=K
    ;   J=I
    ).

toupper_(97,65). toupper_(98,66). toupper_(99,67).
toupper_(100,68). toupper_(101,69). toupper_(102,70). toupper_(103,71).
toupper_(104,72). toupper_(105,73). toupper_(106,74). toupper_(107,75).
toupper_(108,76). toupper_(109,77). toupper_(110,78). toupper_(111,79).
toupper_(112,80). toupper_(113,81). toupper_(114,82). toupper_(115,83).
toupper_(116,84). toupper_(117,85). toupper_(118,86). toupper_(119,87).
toupper_(120,88). toupper_(121,89). toupper_(122,90).

toupper_(224,192). toupper_(225,193). toupper_(226,194). toupper_(227,195).
toupper_(228,196). toupper_(229,197). toupper_(230,198). toupper_(231,199).
toupper_(232,200). toupper_(233,201). toupper_(234,202). toupper_(235,203).
toupper_(236,204). toupper_(237,205). toupper_(238,206). toupper_(239,207).
toupper_(240,208). toupper_(241,209). toupper_(242,210). toupper_(243,211).
toupper_(244,212). toupper_(245,213). toupper_(246,214). 

tolower(I,J) :-
    (   tolower_(I,K)
    ->  J=K
    ;   J=I
    ).

tolower_(65,97).   tolower_(66,98).   tolower_(67,99).   tolower_(68,100).
tolower_(69,101).  tolower_(70,102).  tolower_(71,103).  tolower_(72,104).
tolower_(73,105).  tolower_(74,106).  tolower_(75,107).  tolower_(76,108).
tolower_(77,109).  tolower_(78,110).  tolower_(79,111).  tolower_(80,112).
tolower_(81,113).  tolower_(82,114).  tolower_(83,115).  tolower_(84,116).
tolower_(85,117).  tolower_(86,118).  tolower_(87,119).  tolower_(88,120).
tolower_(89,121).  tolower_(90,122).

tolower_(192,224).
tolower_(193,225). tolower_(194,226). tolower_(195,227). tolower_(196,228).
tolower_(197,229). tolower_(198,230). tolower_(199,231). tolower_(200,232).
tolower_(201,233). tolower_(202,234). tolower_(203,235). tolower_(204,236).
tolower_(205,237). tolower_(206,238). tolower_(207,239). tolower_(208,240).
tolower_(209,241). tolower_(210,242). tolower_(211,243). tolower_(212,244).
tolower_(213,245). tolower_(214,246). tolower_(215,215). tolower_(216,248).
tolower_(217,249). tolower_(218,250). tolower_(219,251). tolower_(220,252).
tolower_(221,253). tolower_(222,254).


accent_chars([],[]).
accent_chars([In0|Ins],[Out0|Outs]) :-
    deaccent(Out0,In0,_,_),
    isaccented(Out0),
    accent_chars(Ins,Outs).
accent_chars([X|Ins],[X|Outs]) :-
    integer(X),
    accent_chars(Ins,Outs).

deaccent_chars([],[]).
deaccent_chars([In0|Ins],[Out0|Outs]) :-
    deaccent(In0,Out0,_,_),
    deaccent_chars(Ins,Outs).

deaccent_chars([],[],F,F).
deaccent_chars([In0|Ins],[Out0|Outs],F0,F) :-
    deaccent(In0,Out0,F0,F1),
    deaccent_chars(Ins,Outs,F1,F).

deaccent(C0,C,_,1) :-
    deaccent(C0,C).

deaccent(X,X,Flag,Flag) :-
     \+ isaccented(X).

deaccent(192, 65).		% À -> A
deaccent(193, 65).		% Á -> A
deaccent(194, 65).		% Â -> A
deaccent(195, 65).		% Ã -> A
deaccent(196, 65).		% Ä -> A
deaccent(197, 65).		% Å -> A

deaccent(199, 67).		% Ç -> C

deaccent(200, 69).		% È -> E
deaccent(201, 69).		% É -> E
deaccent(202, 69).		% Ê -> E
deaccent(203, 69).		% Ë -> E

deaccent(204, 73).		% Ì -> I
deaccent(205, 73).		% Í -> I
deaccent(206, 73).		% Î -> I
deaccent(207, 73).		% Ï -> I

deaccent(209, 78).		% Ñ -> N

deaccent(210, 79).		% Ò -> O
deaccent(211, 79).		% Ó -> O
deaccent(212, 79).		% Ô -> O
deaccent(213, 79).		% Õ -> O
deaccent(214, 79).		% Ö -> O

deaccent(216, 79).		% Ø -> O

deaccent(217, 85).		% Ù -> U
deaccent(218, 85).		% Ú -> U
deaccent(219, 85).		% Û -> U
deaccent(220, 85).		% Ü -> U

deaccent(221, 89).		% Ý -> Y

deaccent(224, 97).		% à -> a
deaccent(225, 97).		% á -> a
deaccent(226, 97).		% â -> a 
deaccent(227, 97).		% ã -> a
deaccent(228, 97).		% ä -> a
deaccent(229, 97).		% å -> a

deaccent(231, 99).		% ç -> c

deaccent(232, 101).		% è -> e
deaccent(233, 101).		% é -> e
deaccent(234, 101).		% ê -> e
deaccent(235, 101).		% ë -> e

deaccent(236, 105).		% ì -> i
deaccent(237, 105).		% í -> i
deaccent(238, 105).		% î -> i
deaccent(239, 105).		% ï -> i

deaccent(241, 110).		% ñ -> n

deaccent(242, 111).		% ò -> o
deaccent(243, 111).		% ó -> o
deaccent(244, 111).		% ô -> o
deaccent(245, 111).		% õ -> o
deaccent(246, 111).		% ö -> o

deaccent(248, 111).		% ø -> o

deaccent(249, 117).		% ù -> u
deaccent(250, 117).		% ú -> u
deaccent(251, 117).		% û -> u
deaccent(252, 117).		% ü -> u

deaccent(253, 121).		% ý -> y
deaccent(255, 121).		% ÿ -> y

vowel(97).
vowel(101).
vowel(105).
vowel(111).
vowel(117).

cons(98).
cons(99).
cons(100).
cons(102).
cons(103).
cons(104).
cons(106).
cons(107).
cons(108).
cons(109).
cons(110).
cons(112).
cons(113).
cons(114).
cons(115).
cons(116).
cons(118).
cons(119).
cons(120).
cons(122).