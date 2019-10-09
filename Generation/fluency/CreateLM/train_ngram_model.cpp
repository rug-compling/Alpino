#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

#include <tr1/memory>
#include <tr1/unordered_map>

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "train_ngram_model.h"

using namespace std;
using namespace std::tr1;

typedef unordered_map<vector<string>, size_t, VectorHash<string> > NgramFreqs;
typedef unordered_map<vector<string>, size_t, VectorHash<string> > NgramProbs;

shared_ptr<NgramFreqs> countNgrams(istream &in)
{
  shared_ptr<NgramFreqs> ngramFreqs(new NgramFreqs);

  string line;
  while (getline(in, line)) {
    // Split on whitespace.
    istringstream lineStream(line);
    vector<string> tokens(2, string("<START>"));
    copy(istream_iterator<string>(lineStream), istream_iterator<string>(),
	 std::back_inserter(tokens));
    tokens.push_back("<END>");

    // Convert the sentence initial word to lowercase.
    boost::to_lower(tokens[2]);

    vector<string> trigram;
    for (vector<string>::const_iterator iter = tokens.begin();
	 iter != tokens.end(); ++iter) {
      // Get the current trigram.
      trigram.push_back(*iter);
      if (trigram.size() > 3)
	trigram.erase(trigram.begin(), trigram.begin() + 1);

      // Extract uni/bi/trigram where w3 is always the final n-gram word.
      for (vector<string>::iterator trigramIter = trigram.begin();
	   trigramIter != trigram.end(); ++trigramIter) {
	vector<string> ngram(trigramIter, trigram.end());
	++(*ngramFreqs)[ngram];
      }
    }
  }

  return ngramFreqs;
}

size_t calcCorpusSize(NgramFreqs const &ngramFreqs)
{
  size_t corpusSize = 0;

  for (NgramFreqs::const_iterator iter = ngramFreqs.begin();
       iter != ngramFreqs.end(); ++iter)
    if (iter->first.size() == 1)
      corpusSize += iter->second;

  return corpusSize;
}

size_t nTypes(NgramFreqs const &ngramFreqs)
{
  size_t n = 0;

  for (NgramFreqs::const_iterator iter = ngramFreqs.begin();
       iter != ngramFreqs.end(); ++iter)
    if (iter->first.size() == 1)
      ++n;

  return n;
}

Lambdas calcLambdas(NgramFreqs const &ngramFreqs, size_t corpusSize)
{
  size_t l1f = 0;
  size_t l2f = 0;
  size_t l3f = 0;

  for (NgramFreqs::const_iterator iter = ngramFreqs.begin();
       iter != ngramFreqs.end(); ++iter) {
    if (iter->first.size() != 3)
      continue;

    // p(w1,w2,w3) = f(w1,w2,w3) / f(w1,w2)
    vector<string> w1w2(iter->first.begin(), iter->first.begin() + 1);
    double trigramP = iter->second / (ngramFreqs.find(w1w2)->second + 1.0);

    // p(w2,w3) = f(w2,w3) / f(w2)
    vector<string> w2w3(iter->first.begin() + 1, iter->first.end());
    vector<string> w2(iter->first.begin() + 1, iter->first.begin() + 2);
    double bigramP = ngramFreqs.find(w2w3)->second /
      (ngramFreqs.find(w2)->second + 1.0);

    // p(w3) = f(w3) / N
    vector<string> w3(iter->first.begin() + 2, iter->first.end());
    double unigramP = ngramFreqs.find(w3)->second / (corpusSize + 1.0);

    if (unigramP > bigramP && unigramP > trigramP)
      l1f += iter->second;
    else if (bigramP > unigramP && bigramP > trigramP)
      l2f += iter->second;
    else
      l3f += iter->second;
  }

  double trigrams = static_cast<double>(l1f + l2f + l3f);
  double l1 = l1f / trigrams;
  double l2 = l2f / trigrams;
  double l3 = l3f / trigrams;

  return Lambdas(l1, l2, l3);
}

double smoothUnigram(size_t freq, size_t corpusSize, size_t types)
{
  return (freq + 0.5) / (corpusSize + types);
}

int probToLogProbInt(double prob)
{
  return abs(log(prob));
}

shared_ptr<NgramProbs> ngramFreqsToProbs(NgramFreqs const &ngramFreqs,
					 size_t corpusSize, size_t types,
					 Lambdas const &lambdas,
					 size_t threshold)
{
  shared_ptr<NgramProbs> ngramProbs(new NgramProbs);

  for (NgramFreqs::const_iterator iter = ngramFreqs.begin();
       iter != ngramFreqs.end(); ++iter) {
    // Skip low-frequency ngrams.
    if (iter->second < threshold)
      continue;

    if (iter->first.size() == 1)
      (*ngramProbs)[iter->first] =
	probToLogProbInt(smoothUnigram(iter->second, corpusSize, types) * lambdas.l1);
    else {
      vector<string> firstNgram;
      copy(iter->first.begin(), iter->first.end() - 1,
      	   back_inserter(firstNgram));
      double lambda = iter->first.size() == 2 ? lambdas.l2 : lambdas.l3;
      (*ngramProbs)[iter->first] = probToLogProbInt(lambda *
        (iter->second / static_cast<double>(ngramFreqs.find(firstNgram)->second)));
    }
  }

  vector<string> unknown;
  unknown.push_back("<unknown_word>");
  (*ngramProbs)[unknown] = probToLogProbInt(smoothUnigram(0, corpusSize, types) * lambdas.l1);

  return ngramProbs;
}

void printNgrams(ostream &out, size_t n, NgramProbs const &ngramProbs)
{
  for (NgramProbs::const_iterator iter = ngramProbs.begin();
       iter != ngramProbs.end(); ++iter) {
    if (iter->first.size() != n)
      continue;

    copy(iter->first.begin(), iter->first.end(),
	 ostream_iterator<string>(out, "/|\\"));
    out << iter->second << endl;
  }
}

int main(int argc, char *argv[])
{

  if (argc != 6) {
    cerr << "Usage: " << argv[0]
	 << " corpus threshold unigrams bigrams trigrams" << endl;
    return 1;
  }

  ifstream corpusStream(argv[1]);

  size_t threshold = parseString<size_t>(argv[2]);
  
  if (corpusStream.bad()) {
    cerr << "Could not open corpus!" << endl;
    return 1;
  }

  ofstream unigramStream(argv[3]);
  ofstream bigramStream(argv[4]);
  ofstream trigramStream(argv[5]);

  if (unigramStream.bad() || bigramStream.bad() || trigramStream.bad()) {
    cerr << "Could not open output files for writing!" << endl;
    return 1;
  }

  cerr << "Counting ngrams..." << endl;
  shared_ptr<NgramFreqs> ngramFreqs(countNgrams(corpusStream));

  cerr << "Determining corpus size..." << endl;
  size_t corpusSize = calcCorpusSize(*ngramFreqs);
  size_t types = nTypes(*ngramFreqs);

  cerr << "Calculating lambdas..." << endl; 
  Lambdas lambdas(calcLambdas(*ngramFreqs, corpusSize));

  cerr << "Calculating ngram probabilities..." << endl; 
  shared_ptr<NgramProbs> ngramProbs = ngramFreqsToProbs(
    *ngramFreqs, corpusSize, types, lambdas, threshold);

  cerr << "Writing output..." << endl;
  printNgrams(unigramStream, 1, *ngramProbs);
  printNgrams(bigramStream, 2, *ngramProbs);
  printNgrams(trigramStream, 3, *ngramProbs);
}
