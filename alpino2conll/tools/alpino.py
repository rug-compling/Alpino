#!/usr/bin/python

'''
Script to call Alpino and get pos tags
'''

__author__ = 'Barbara <b.plank@rug.nl>'

import string
import sys
import os.path
import subprocess
# adjust path
#DIR = "/storage3/data/bplank/sw/conversion/alpino2conll/tools/"

DIR=os.environ["ALPINO2CONLL_HOME"]+"/tools/"
#DIR=os.environ["ALPINO2CONLL_HOME"]

def alpino(sentence, filename):
	"""
	parse sentence with alpino, return triples
	"""

	i = 0
	for w in sentence:
		if sentence[i].find("|") != -1:
			print sentence[i]
			sentence[i] = "[ @postag punct(staand_streep) streep ]"
			#sentence[i] = sentence[i].replace("|","\|",1)
			print sentence[i]
		i+=1

	outf = open(filename,"w")
	print sentence
	sent = open("sent","w")
	sent.write("| ") # empty key identifier
	sent.write(string.join(sentence))
	sent.close()

	fin = subprocess.Popen(["cat","sent"],stdout=subprocess.PIPE)

#	executable = os.environ["ALPINO_HOME"] +"/src/Alpino"
	executable = os.environ["ALPINO_HOME"] +"/bin/Alpino"
# 	args = [executable, "-notk", "-fast", "end_hook=triples_with_full_postags","user_max=14400000", "-parse"]   # this was used for LASSY conversion
 	args = [executable, "-notk", "-veryfast", "end_hook=triples_with_full_postags","user_max=14400000", "-parse"]
	alp = subprocess.Popen(args,stdin=fin.stdout,stdout=subprocess.PIPE)
	output = alp.communicate()[0]

	print "OUT:"
	print output

	output = triples2tab(output)

	for item in output:
		outf.write(item)
		outf.write(" ")
	outf.close()
	# convert triples
	p = subprocess.Popen([DIR+"alppos2conllfeat.pl",filename],stdout=subprocess.PIPE)
	cfeat = p.stdout.read()
	print cfeat
	
	
	return cfeat


def triples2tab(output):
	lines = output.split("\n")
	word_pos_list = []
	for line in lines:
		if len(line.split("|")) > 1:
			word_pos_list.append(line.split("|")[0:2]) #first word and pos
			word_pos_list.append(line.split("|")[3:]) #second word and pos
			print "**",line

	# get position of words
	t=0
	for w in word_pos_list:
		split = w[0].split("/[")
		##print "** ",split
		row = []
		row.append(split[1].split(",")[0]) # word index
		row.append(split[0].replace(" ","_")) #word
		row.append(w[1].replace(" ","_")) #pos
		#row.append(w[1]) #pos
		word_pos_list[t] = string.join(row,"@")
		t+=1


	wl = list(set(word_pos_list)) # get uniq words

	# split in index and word@pos to sort
	t=0
	for w in wl:
		row = []
		split = w.split("@")
		row.append(int(split[0]))
		row.append(string.join(split[1:],"@"))
		wl[t] = tuple(row)
		t+=1
		
	#print wl
	#wl = wl.sort(numeric_compare) # sort
	wl.sort() # sort
	#print "sorted: ", wl
	#wl=sorted(wl.iteritems(),lambda x,y: 1 if x[1] >y[1] else -1)
	# remove word index
	output = []
	for w in wl:
		output.append(w[1])
	print output
	return output


### to test
#tokens=['dit','in','verband','met','de','gemiddeld','langere','levensduur','van','de','vrouw']
#tokens = ['Een', 'jong', ',', 'zorgzaam', 'moedertje', ',', 'voor', 'wie', 'de', 'kinderen', 'het', 'belangrijkste', 'zijn', '.']
#alpino(tokens, "blue")
#tokens=['[ @mwu Burgemeester ]', '[ @mwu waren ]', '[ @mwu ondere andere ]', '[ @mwu Franz Rens ]', '[ @mwu ( ]', '[ @mwu lib ]', '[ @mwu . ]', '[ @mwu ) ]', '[ @mwu ( ]', '[ @mwu 1894-98 ]', '[ @mwu ) ]', '[ @mwu , ]', '[ @mwu Robert Rens ]', '[ @mwu ( ]', '[ @mwu lib ]', '[ @mwu . ]', '[ @mwu ) ]', '[ @mwu ( ]', '[ @mwu 1953-1958 ]', '[ @mwu ) ]', '[ @mwu , ]', '[ @mwu Agnes Allebosch-De Munter ]', '[ @mwu ( ]', '[ @mwu 1977-1994 ]', '[ @mwu ) ]', '[ @mwu , ]', '[ @mwu Freddy De Chou ]', '[ @mwu ( ]', '[ @mwu Socialistische Partij Anders ]', '[ @mwu | ]', '[ @mwu SP ]', '[ @mwu ) ]', '[ @mwu ) ]', '[ @mwu ) ]', '[ @mwu ( ]', '[ @mwu 1995-2000 ]', '[ @mwu ) ]', '[ @mwu en ]', '[ @mwu Guido De Padt ]', '[ @mwu ( ]', '[ @mwu VLD ]', '[ @mwu ) ]', '[ @mwu ( ]', '[ @mwu 2001- ]', '[ @mwu ) ]']
#alpino(tokens,"testit")
