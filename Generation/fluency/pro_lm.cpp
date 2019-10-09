/*
 * n-gram language model
 *
 * This file implements trigram language model, to be used as a
 * Sicstus Prolog module. Smoothing is done by the means of linear
 * interpolation. The uni/bi/trigrams were already multiplied by
 * their weights by the training program. So, here we simply add
 * the weighted uni/bi/trigram probabilities.
 */

#include <algorithm>
#include <exception>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <vector>

#include <sicstus/sicstus.h>

#include "../../fadd/fadd.h"
#include "lm.h"

using namespace std;

// Yuck
extern int wordUnigramDict;
extern int wordBigramDict;
extern int wordTrigramDict;

extern int tagUnigramDict;
extern int tagBigramDict;
extern int tagTrigramDict;

// Hdrug flag
char const * const INITIALIZED_FLAG = "lm_initialized";
char const * const UNINITIALIZED_VALUE = "no";

list_of_words *pro_get_word_list(SP_term_ref word_list);

void raise_exception(char const *message)
{
  SP_term_ref m = SP_new_term_ref();
  SP_put_string(m, message);
  SP_raise_exception(m);
}

vector<string> listTermToVector(SP_term_ref pl_words)
{
  vector<string> list;

  SP_term_ref tmp = SP_new_term_ref();
  SP_term_ref head = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();

  SP_put_term(tmp, pl_words);
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS)
  {
    char *headName;
    if (SP_get_string(head, &headName) == 0)
      throw runtime_error("listTermToVector: string conversion failed!");
    // Fixme: probably not OK when some non-latin1 locale is used.
    headName = SP_to_os(headName, 0);
    list.push_back(string(headName));

    SP_put_term(tmp, tail);
  }

  return list;
}

// From fadd
list_of_words *
pro_get_word_list(SP_term_ref word_list)
{
  SP_term_ref   tmp, head, tail;
  char          *word;
  char          *nword;
  list_of_words *low, *llow, *lstart;
  head = SP_new_term_ref();
  tail = SP_new_term_ref();
  tmp = SP_new_term_ref();
  tmp = word_list;
  lstart = low = llow = NULL;
  word = NULL;
  nword = NULL;
  while (SP_get_list(tmp,head,tail)==SP_SUCCESS) {
    if (SP_get_string(head, &nword) == 0) {
      cerr << "No strings attached\n";
    }
    word = SP_to_os(nword,0);

    /*fprintf(stderr, "%s\n", word);*/
    if ((low = (list_of_words *)SP_malloc(sizeof(list_of_words))) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if ((low->word = nstrdup(word)) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    low->next = NULL;
    if (llow) {
      llow->next = low;
      llow = low;
    }
    else {
      lstart = llow = low;
    }
    tmp = tail;
  }

  return lstart;
}

#ifdef __cplusplus
extern "C" {
#endif

void lm_init_hook(int)
{
  if (debug)
    cerr << "pro_lm: lm_init_hook()" << endl;

  initLib();
}

void lm_deinit_hook(int)
{
  if (debug)
    cerr << "pro_lm: closing tuple dictionaries" << endl;

  deinitLM();

  if (debug)
    cerr << "pro_lm: changing state to uninitialized" << endl;

  SP_term_ref flagT = SP_new_term_ref();
  SP_put_string(flagT, INITIALIZED_FLAG);
  SP_term_ref valT = SP_new_term_ref();
  SP_put_string(valT, UNINITIALIZED_VALUE);
  
  SP_query(SP_predicate("set_flag",2,"hdrug_util"), flagT, valT);
}

void pro_init_lm(SP_term_ref wordUnigramDictList,
		 SP_term_ref wordBigramDictList,
		 SP_term_ref wordTrigramDictList,
		 SP_term_ref tagUnigramDictList,
		 SP_term_ref tagBigramDictList,
		 SP_term_ref tagTrigramDictList)
{
  if (debug)
    cerr << "pro_lm: loading n-gram tuple dictionaries" << endl;

  list_of_words *wordUnigramLOW = pro_get_word_list(wordUnigramDictList);
  list_of_words *wordBigramLOW = pro_get_word_list(wordBigramDictList);
  list_of_words *wordTrigramLOW = pro_get_word_list(wordTrigramDictList);

  list_of_words *tagUnigramLOW = pro_get_word_list(tagUnigramDictList);
  list_of_words *tagBigramLOW = pro_get_word_list(tagBigramDictList);
  list_of_words *tagTrigramLOW = pro_get_word_list(tagTrigramDictList);

  initLM(wordUnigramLOW, wordBigramLOW, wordTrigramLOW, tagUnigramLOW,
	 tagBigramLOW, tagTrigramLOW);
}

double unigram_fluency(SP_term_ref pl_words)
{
  double p;

  try {
    vector<string> words = listTermToVector(pl_words);
    p = unigram_fl(words, wordUnigramDict);
  } catch (exception &e) {
    raise_exception(e.what());
    return -1.0;
  }

  return p;
}


double phrase_fluency(SP_term_ref pl_words)
{
  double p;

  try {
    vector<string> words = listTermToVector(pl_words);
    p = fluency(words, wordUnigramDict, wordBigramDict, wordTrigramDict);
  } catch (exception &e) {
    raise_exception(e.what());
    return -1.0;
  }

  return p;
}

double sentence_fluency(SP_term_ref pl_words)
{
  double p;

  try {
    vector<string> wordsNoMarkers = listTermToVector(pl_words);
    vector<string> words(2, START_MARKER);
    copy(wordsNoMarkers.begin(), wordsNoMarkers.end(), back_inserter(words));
    words.push_back(END_MARKER);

    p = fluency(words, wordUnigramDict, wordBigramDict, wordTrigramDict);
  } catch (exception &e) {
    raise_exception(e.what());
    return -1.0;
  }

  return p;
}

double phrase_tag_prob(SP_term_ref pl_tags)
{
  double p;

  try {
    vector<string> tags = listTermToVector(pl_tags);
    p = fluency(tags, tagUnigramDict, tagBigramDict, tagTrigramDict);
  } catch (exception &e) {
    raise_exception(e.what());
    return -1.0;
  }

  return p;
}

double sentence_tag_prob(SP_term_ref pl_tags)
{
  double p;

  try {
    vector<string> tagsNoMarkers = listTermToVector(pl_tags);
    vector<string> tags(2, START_MARKER);
    copy(tagsNoMarkers.begin(), tagsNoMarkers.end(), back_inserter(tags));
    tags.push_back(END_MARKER);

    p = fluency(tags, tagUnigramDict, tagBigramDict, tagTrigramDict);
  } catch (exception &e) {
    raise_exception(e.what());
    return -1.0;
  }

  return p;
}

#ifdef __cplusplus
} // extern "C"
#endif
