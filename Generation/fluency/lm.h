#ifndef ALPINO_LM_H
#define ALPINO_LM_H

#include <string>
#include <vector>

#include "../../fadd/fadd.h"

// Start/end markers
#define START_MARKER "<START>"
#define END_MARKER "<END>"

static bool const debug = false;

void deinitLM();
void deleteListOfWords(list_of_words *words);
double fluency(std::vector<std::string> const &words, int unigramDict,
	       int bigramDict, int trigramDict);
double unigram_fl(std::vector<std::string> const &words, int unigramDict);
void initLM(list_of_words *wordUnigramDictList,
	    list_of_words *wordBigramDictList,
	    list_of_words *wordTrigramDictList,
	    list_of_words *tagUnigramDictList,
	    list_of_words *tagBigramDictList,
	    list_of_words *tagTrigramDictList);
void initLib();

#endif // ALPINO_LM_H
