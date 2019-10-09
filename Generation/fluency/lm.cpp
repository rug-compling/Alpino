#include <algorithm>
#include <cmath>
#include <functional>
#include <iostream>
#include <vector>

#include "../../fadd/fadd.h"

#include "lm.h"

#include "logadd.hh"

using namespace std;

// Unknown words
char const * const UNKNOWN_WORD = "<unknown_word>";

int libkey = -1;

int wordUnigramDict = -1;
int wordBigramDict = -1;
int wordTrigramDict = -1;

int tagUnigramDict = -1;
int tagBigramDict = -1;
int tagTrigramDict = -1;

list_of_words *createListOfWords(vector<string>::const_iterator i1,
				 vector<string>::const_iterator i2)
{

  list_of_words *head = 0;
  list_of_words *prev = 0;

  for (vector<string>::const_iterator iter = i1; iter != i2; ++iter)
  {
    list_of_words *cur = new list_of_words;

    cur->word = iter->c_str();

    if (head == 0)
    {
      head = cur;
      prev = cur;
    }
    else
      prev->next = cur;

    prev = cur;
  }

  prev->next = 0;

  return head;
}

void deleteListOfWords(list_of_words *words)
{
  while (words != 0)
  {
    list_of_words *tmp = words;
    words = words->next;
    delete tmp;
  }
}

void deinitLM()
{
  if (wordUnigramDict != -1)
    close_tuple(wordUnigramDict);
  if (wordBigramDict != -1)
    close_tuple(wordBigramDict);
  if (wordTrigramDict != -1)
    close_tuple(wordTrigramDict);

  if (tagUnigramDict != -1)
    close_tuple(tagUnigramDict);
  if (tagBigramDict != -1)
    close_tuple(tagBigramDict);
  if (tagTrigramDict != -1)
    close_tuple(tagTrigramDict);

  if (libkey != -1)
    fadd_close_lib(libkey);
}

void deleteListOfNumbers(list_of_numbers *list)
{
  while (list != 0)
  {
    list_of_numbers *tmp = list;
    list = list->next;
    delete tmp;
  }
}

void initLM(list_of_words *wordUnigramDictList,
	    list_of_words *wordBigramDictList,
	    list_of_words *wordTrigramDictList,
	    list_of_words *tagUnigramDictList,
	    list_of_words *tagBigramDictList,
	    list_of_words *tagTrigramDictList)
{
  wordUnigramDict = init_tuple(wordUnigramDictList);
  if (debug)
    cerr << "Initialized 1-gram tuple_dict: " << wordUnigramDict << endl;
  wordBigramDict = init_tuple(wordBigramDictList);
  if (debug)
    cerr << "Initialized 2-gram tuple_dict: " << wordBigramDict << endl;
  wordTrigramDict = init_tuple(wordTrigramDictList);  
  if (debug)
    cerr << "Initialized 3-gram tuple_dict: " << wordTrigramDict << endl;

  tagUnigramDict = init_tuple(tagUnigramDictList);
  if (debug)
    cerr << "Initialized tag 1-gram tuple_dict: " << tagUnigramDict << endl;
  tagBigramDict = init_tuple(tagBigramDictList);
  if (debug)
    cerr << "Initialized tag 2-gram tuple_dict: " << tagBigramDict << endl;
  tagTrigramDict = init_tuple(tagTrigramDictList);  
  if (debug)
    cerr << "Initialized tag 3-gram tuple_dict: " << tagTrigramDict << endl;
}

void initLib()
{
  libkey = fadd_init_lib(64);
  if (libkey < 0)
    cerr << "error: fadd_init_lib: " << libkey << endl;
}

long int wordsProb(vector<string>::const_iterator i1,
		 vector<string>::const_iterator i2,
		 int dict)
{
  if (debug)
    cout << "Called wordsProb with dictionary: " << dict << endl;

  list_of_words *low = createListOfWords(i1, i2);
  list_of_numbers *ps = word_tuple_grams(low, dict);

  double p;
  if (ps == NULL && distance(i1, i2) == 1) {
    // In the unigram case, we can still try to look for
    // '<unknown_word>'.
    deleteListOfNumbers(ps);
    low->word = UNKNOWN_WORD;
    ps = word_tuple_grams(low, dict);
  }

  p = (ps == NULL ? 1000 : ps->word);

  // if (p == 0) {
  //   cerr << "zero-valued ngram in tuple file... " << distance(i1,i2) << endl;
  //   list_of_words *tmp = low;
  //   while (tmp != 0){
  //     cerr << tmp->word << " ";
  //     tmp = tmp->next;
  //   }
  //   cerr << endl;
  //   p = 1000;  // something is wrong
  // }
  deleteListOfWords(low);
  deleteListOfNumbers(ps);

  return p;
}

// double logAdd(double x, double y)
// {
//   if (x > y + 70)
//     return y;

//   if (x + 70 < y)
//     return x;

//   double minVal = min(x,y);
//   double maxVal = max(x,y);

//   return minVal + log(exp(x - maxVal) + exp(y - maxVal));
// }

// corrected by GvN
// cf https://en.wikipedia.org/wiki/Log_probability
// double logAdd(double x, double y, double z)
// {

//   double minVal = min(x,min(y,z));

//   return minVal - log(exp(minVal-x) + exp(minVal-y) + exp(minVal-z));

// }


double unigram_fl(vector<string> const &words, int unigramDict)
{
  double p = 0.0;

  for (vector<string>::const_iterator iter = words.begin();
       iter != words.end(); ++iter)
  {
    long int p1 = wordsProb(iter, iter + 1, unigramDict);
    p += p1;
  }

  return p;
}


double fluency(vector<string> const &words, int unigramDict,
	       int bigramDict, int trigramDict)
{
  double p = 0.0;

  for (vector<string>::const_iterator iter = words.begin() + 2;
       iter != words.end(); ++iter)
  {
    long int p3 = wordsProb(iter - 2, iter + 1, trigramDict);
    long int p2 = wordsProb(iter - 1, iter + 1, bigramDict);
    long int p1 = wordsProb(iter, iter + 1, unigramDict);

    double p0 = logAdd(p1,p2,p3);

    p += p0;

    // cerr << (iter-2) -> c_str() << " " << (iter-1) -> c_str() << " " << (iter-0) -> c_str() << " " << p3 << endl;
    // cerr <<                               (iter-1) -> c_str() << " " << (iter-0) -> c_str() << " " << p2 << endl;
    // cerr <<                                                             (iter-0) -> c_str() << " " << p1 << endl;
    // cerr << "logadd: " << p0 << endl;

  }


  return p;
}
