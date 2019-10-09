ALL=$(shell ls *.TXT)
TOK=$(ALPINO_HOME)/Tokenization

%.sents: %.TXT
	recode dos < $< | ./txt2sents | $(TOK)/tokenize.sh |\
        sed -e "s/\([0-9]\)\(°\)/\1 \2/g" |\
        tr -d '|' \
             > $@

all: $(ALL:%.TXT=%.sents)

#            -e "s/o\|al/ocial/g"\
#            -e "s/o\|olog/ociolog/g"\
#            -e "s/prin\|pi/principi/g"\
#            -e "s/eman\|p/emancip/g"\
#            -e "s/Con\|lie/Concilie/g"\
#            -e "s/pre\|es/precies/g"



%.ingang: %.TXT
	recode dos < $< | ./ingang > $@

index_files=../../Treebank/Machine/index_files_spectrum

ingangen0: $(ALL:%.TXT=%.ingang) 
	cat *.ingang > ingangen0

ingangen: ingangen0 $(index_files)
	./ingang2 < $(index_files) > ingangen

# mgbuild spectrumingangen
# /users3/vannoord/bin/mg_fast_comp_dict -f spectrumingangen/spectrumingangen
