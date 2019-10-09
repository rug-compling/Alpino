dutch.qa :  clef2qa.xsl
	xsltproc clef2qa.xsl DISEQuA_v1.0.xml > dutch.qa

questions.named_sents : dutch.qa
	grep QUESTION dutch.qa |cut -f2,4 -d'|' | ../tok > questions.named_sents

answers.pl : dutch.qa
	grep '^ *answer' dutch.qa |sed 's/^ *//' |sed 's/"/\\"/g' |sed 's/\\"/"/' |sed 's/\\")/")/' > answers.pl

