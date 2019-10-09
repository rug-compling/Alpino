all: paragraphs
	zcat ../../clef_part*sents.gz |\
        grep -F -f paragraphs | ../../split_in_parts -o clefrel 
