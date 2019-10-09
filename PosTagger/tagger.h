//-----------------------------------------------------------------------------
// tagger.h -- header file for POS-tag filter; implementation in tagger.cc
//-----------------------------------------------------------------------------

#ifndef _TAGGER_H_
#define _TAGGER_H_

//-----------------------------------------------------------------------------
// includes
//-----------------------------------------------------------------------------

// for guesser
#include <cstdio>
#include <cstdlib>
#include <cctype>     // isupper()
#include <sstream>     // ostringstream

// for tagger
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <list>
#include <utility>
#include <math.h>
#include <fadd.h>

using namespace std;

//-----------------------------------------------------------------------------
// Windows compatibility 
//-----------------------------------------------------------------------------

#ifdef _MSC_VER
#define popen _popen
#define pclose _pclose
#endif

//-----------------------------------------------------------------------------
// constants
//-----------------------------------------------------------------------------

const string DATADIR                = "./";

const string WORDTAGTUPLEFILE       = "wordTag.tpl";
const string TAGWORDTUPLEFILE       = "tagWord.tpl";
const string CONTEXTTAGTUPLEFILE    = "context3.tpl";
const string BIGRAMTUPLEFILE        = "tag2.tpl";
const string TRIGRAMTUPLEFILE       = "tag3.tpl";
const string FOURGRAMTUPLEFILE      = "tag4.tpl";
const string PREFIXBIGRAMTUPLEFILE  = "prefix2.tpl";
const string PREFIXTRIGRAMTUPLEFILE = "prefix3.tpl";
const string WORDDICTFILE           = "words.fsa";
const string TAGDICTFILE            = "tags.fsa";
const string CONTEXTDICTFILE        = "context.fsa";
const string USEDCONTEXTFILE        = "usedContext";
const string WORDTAGLEXICONDICTFILE = "wordTagLex.fsa";

const string HEURISTICSFILE         = "heuristics";

const int    USE_EXTERNAL_FSA_GUESS = 0; // alternative is functionality in fadd library
const string FSAGUESS               = "fsa_guess -g -d ";
const string FSAGUESS_FAILURE       = "*not found*";

const string FSAGUESS_DICT_ALL      = "sufLexAll.fsa";
const string FSAGUESS_DICT_CAPS     = "sufLexCaps.fsa";
const string FSAGUESS_DICT_NONCAPS  = "sufLexNonCaps.fsa";

const string DUMMY_WORD             = "<DUMMY_WORD>";
const string DUMMY_CONTEXT          = "<DUMMY_CONTEXT>";
const string DUMMY_TAG              = "<DUMMY_TAG>"; // used for unknown words in baseline method
const string SENTENCE_START         = "xxx_sentence_start";
const string SENTENCE_END           = "xxx_sentence_end";
const string UNKNOWN_WORD           = "<UNKNOWN_WORD>";

const int    DIRECTION_FORWARD      = 0;
const int    DIRECTION_BACKWARD     = 1;
const int    ALPHA                  = 0;
const int    BETA                   = 1;
const int    DEFAULT_TAG_ID         = 0;
const int    CONTEXT_STATESIZE      = 1;
const int    INTERPOLATION_LIMIT    = 2; // smallest n-gram in interpolation

// note: here, the extra context feature is also counted, thus using a
// value of 2 for INTERPOLATION_LIMIT means we are also including the
// tag unigram in the interpolation (but since a context feature is
// added, the unigram is internally a "bigram"); see function double
// ngramProb()

const int    TAGSET_WOTAN           = 0;
const int    TAGSET_ALPINO          = 1;

const double WEIRD_PROB_TOLERANCE   = -1.0e-10;

const long   MAX_NGRAM_VALUE        = 1350;
const long   MAX_WORD_TAG_VALUE     = 1350;
const long   MAX_TAG_WORD_VALUE     = 1350;

//-----------------------------------------------------------------------------
// typedefs
//-----------------------------------------------------------------------------

// forward references
class TAGNODE;
class STATENODE;

// typedef for trellis structure
typedef vector<vector<TAGNODE> > vvTAGNODE;

//-----------------------------------------------------------------------------
// forward declarations of global functions
//-----------------------------------------------------------------------------

// ---------------------
// "interface" functions
// ---------------------

// initializing the tagger/filter
void tagger_init(int    p_model,
		 int    p_threshold,
		 int    p_percentage,
		 double    p_min_prob,
		 int    p_debug,
		 int    p_baseline,
		 int    p_standAlone,
		 double p_diversity,
		 string p_startTag,
		 string p_endTag,
		 string p_dataDir                = DATADIR, 
		 string p_wordTagTupleFile       = WORDTAGTUPLEFILE, 
		 string p_tagWordTupleFile       = TAGWORDTUPLEFILE, 
		 string p_contextTagTupleFile    = CONTEXTTAGTUPLEFILE,
		 string p_bigramTupleFile        = BIGRAMTUPLEFILE, 
		 string p_trigramTupleFile       = TRIGRAMTUPLEFILE, 
		 string p_fourgramTupleFile      = FOURGRAMTUPLEFILE, 
		 string p_prefixBigramTupleFile  = PREFIXBIGRAMTUPLEFILE,
		 string p_prefixTrigramTupleFile = PREFIXTRIGRAMTUPLEFILE,
		 string p_wordDictFile           = WORDDICTFILE, 
		 string p_tagDictFile            = TAGDICTFILE,
		 string p_contextDictFile        = CONTEXTDICTFILE,
		 string p_usedContextFile        = USEDCONTEXTFILE,
                 string p_wordTagLexiconDictFile = WORDTAGLEXICONDICTFILE);

// doing the filtering for a single sentence
int tagger_filter(vector<string> const &words,
		  vvTAGNODE const &tags,
		  int tagCount,
		  int *remainingTags,
                  double *remainingTagScores,
		  int debug=0);

// doing the filtering for a single sentence, storing evaluation information too
void tagger_filter_evaluation(vector<string> &words,
			      vvTAGNODE &tags,
			      int tagCount,
			      vector<pair<string,pair<int,int> > > corrTags,
			      vector<double> &info,
			      int debug=0);

// doing the filtering for a single sentence, stand-alone only
void tagger_filter_ask(vector<string> &words,vvTAGNODE &tags,int debug);

// closing down the tagger/filter
void tagger_close();

int legal_transition(string t1, string t2);

// -------------------------
// functions used internally
// -------------------------

// loading used context labels
void tagger_loadContextLabels(string usedContextFileName);

// loading all fadd structures
void tagger_loadfadd(string wordTagTupleFile,
		     string tagWordTupleFile,
		     string contextTagTupleFile,
		     string bigramTupleFile,
		     string trigramTupleFile,
		     string fourgramTupleFile,
		     string prefixBigramTupleFile,
		     string prefixTrigramTupleFile,
		     string wordDictFile,
		     string tagDictFile,
		     string contextDictFile,
		     string wordTagLexiconDictFile);

// loading specific fadd structures
int loadProbsBigram(const char *tupleFile, 
		    const char *dictFile0, 
		    const char *dictFile1);

int loadProbsTrigram(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1, 
		     const char *dictFile2);

int loadProbsFourgram(const char *tupleFile, 
		      const char *dictFile0, 
		      const char *dictFile1, 
		      const char *dictFile2, 
		      const char *dictFile3);

int loadProbsContextTag(const char *tupleFile, 
			const char *dictFile0, 
			const char *dictFile1,
			const char *dictFile2);

int loadProbsWordTag(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1);

int loadProbsTagWord(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1);

int loadPrefixBigram(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1);

int loadPrefixTrigram(const char *tupleFile, 
		      const char *dictFile0, 
		      const char *dictFile1,
		      const char *dictFile2);

// adding log values
double addLogProbabilities(double value1, double value2);

// computing transition probablities
double transitionProbability(vector<string> tagNGram, 
			     vector<string> tagWords);

// closing fadd structures
void closeDataFiles();

// retrieving probabilities from fadd structures
double wordTagProb(list_of_words *wordList);
double tagWordProb(list_of_words *wordList);
double ngramProb(list_of_words *tags, double totalLambda, int n, int interpolationLimit, int useContextFadd);

// dealing with fadd linked lists
list_of_words *createWordList(vector<string> const &wordVector);
void destroyWordList(list_of_words *list);
void destroyNumberList(list_of_numbers *list);

// assigning possible tags to words
int  tagger_getPossibleTags(const vector<string> &words, vvTAGNODE &tags);
int  tagger_expandMultiWordTag(int wordCount,string baseTag,unsigned int sentencePos,vvTAGNODE &tags,const vector<string> &words);
void tagger_getRequiredWords(string &tagWords,vector<string> &requiredWords);
int  tagger_requiredWordsAvailable(vector<string> &reqWords, const vector<string> &words, vector<string>::const_iterator checkPos);

// lexical analysis
void guesser_initData(const string dataDir);
void guesser_closeData();
void guesser_guessTags(string &word, vector<string> &tags, const string dataDir);
void guesser_getTagsFromLexicon(const string &word, vector<string> &tags);
void guesser_getTagsFromSuffixDictionary(string &word, vector<string> &tags);
void guesser_getTagsFromSuffixDictionaryOldVersion(string &word, vector<string> &tags,const string p_dataDir);

// other functions
void chomp(string &line);
string addSlashes(string word, char character);

//-----------------------------------------------------------------------------
// TRANSITION class
//-----------------------------------------------------------------------------

// In order to be able to store for individual transitions their probability,
// this transition class is used. Each state (statenode) will have a vector
// containing as many objects of this type as there are transitions to other
// states. Each transition object will hold the target state of the transition 
// and the associated transition probability.

class TRANSITION
{
 public:

  TRANSITION(list<STATENODE>::iterator targetState,double transitionProb);

  ~TRANSITION();

  list<STATENODE>::iterator getTargetState()
    {return targetState;}

  double getTransitionProbability()
    {return transitionProbability;}

 private:

  list<STATENODE>::iterator
    targetState;

  double
    transitionProbability;
};

//-----------------------------------------------------------------------------
// STATENODE class
//-----------------------------------------------------------------------------

// The statenode objects are the actual states in the trellis, where transitions
// from one state to the next are represented by transition objects. Each state
// holds the tag history for the states it precedes, as well as the tag that
// is associated with the current position in the trellis, and a vector for
// storing both the forward and backward probability up to that state.

class STATENODE
{
 public:

  STATENODE(vector<string> const &history, vector<string> const &contextHistory,
	    vector<string> const &observedWords, string const &tag,
	    string const &context);

  ~STATENODE();

  STATENODE(const STATENODE& obj) 
    {*this = obj;}
  
  double getProbability(int direction) 
    {return probability[direction];}
  
  vector<TRANSITION> getTransitions()
    {return transitions;}
  
  vector<string> getHistory() 
    {return history;}

  vector<string> getContextHistory() 
    {return contextHistory;}
  
  vector<string> getObservedWords()  
    {return observedWords;}

  string getTagName()
    {return tag;}

  string getContextName()
    {return context;}

  void mark_r_reachable()
    { r_reach=1;}

  int r_reachable() 
    { return(r_reach);}
  
  double getForwardBackwardValue();
  void   setProbability(double p_probability, int direction);	 
  void   addToComputation(list<STATENODE>::iterator sourceState,
			double transitionProb,int direction);
  double createTransition(list<STATENODE>::iterator targetState,
			  int transitionType);
  void   print();
  void   printHistory();
  
 private:
  
  vector<string>
    history,
    observedWords,
    contextHistory;

  string
    tag,
    context;
 
  vector<int>
    probabilityDefined;
  
  vector<double>
    probability;

  int r_reach;
  
  double
    forwardBackwardProbability,
    normalizedProbability;
  
  vector<TRANSITION>
    transitions;
};

//-----------------------------------------------------------------------------
// TAGNODE class
//-----------------------------------------------------------------------------

// The statenodes are maintained in lists which are again part of tagnode
// objects. Each tagnode represents a different tag. The probabilities
// computed for the statenodes are combined to get the probabilities for
// tags. The tagnode constructor accepts a tag ID which is used by Alpino.

class TAGNODE
{
 public:
  
  TAGNODE(string const &tagName, vector<string> const &allWords,
	  int wordPos, int tagSpan, int tagID);

  ~TAGNODE();

  TAGNODE(const TAGNODE& obj) 
    {*this = obj;}
  
  int operator<(TAGNODE const &other) const
    {return(forwardBackwardProbability < other.forwardBackwardProbability);}
  
  int betterDifferenceThan(TAGNODE const &other)
    {return(differenceWithBest < other.differenceWithBest);}
  
  int getWordPos()
    {return wordPos;}
  
  int getTagSpan() 
    {return tagSpan;}
  
  int getTagID()
    {return tagID;}
  
  string getTagName() 
    {return tagName;}

  void setTagName(string newName)
    {tagName = newName;}

  vector<string> getObservedWords()  
    {return observedWords;}

  void setContextName(string newName)
    {contextName = newName;}

  string getContextName()
    {return contextName;}
  
  double getDifferenceWithBest()
    {return differenceWithBest;}
  
  list<STATENODE> *getAssociatedStates()
    {return &associatedStates;}
  
  void setForwardBackwardProbability(double value)
    {forwardBackwardProbability = value;}
  
  void setNormalizedProbability(double value)
    {normalizedProbability = value;}
  
  double getForwardBackwardProbability()
    {return(forwardBackwardProbability);}
  
  double getNormalizedProbability()
    {return(normalizedProbability);}
  
  void markAsDeleted()
    {deleted = 1;}
  
  int markedAsDeleted()
    {return(deleted || !l_reach);}
  
  void markAsBest()
    {best = 1;}
  
  int markedAsBest()
    {return(best);}

  void computeDifferenceWithBest(double best)
    {differenceWithBest = forwardBackwardProbability - best;}
  
  list<STATENODE>::iterator addNewState(list<STATENODE>::iterator sourceState);
  void addDummyState(vector<string> const &history,
		     vector<string> const &contextHistory);
  void setAllStatesTo(double value, int direction);
  void setAllStatesReach();
  void computeForwardBackwardValue();
  void print();

  void mark_l_reachable()
    {l_reach=1;}
  
  int l_reachable()
    {return l_reach;}

 private:
  
  string 
    tagName,
    contextName;
  
  int    
    tagID,
    wordPos,
    tagSpan,
    deleted,
    l_reach,
    best;
  
  double
    forwardBackwardProbability,
    normalizedProbability,
    differenceWithBest;
  
  vector<string> 
    observedWords;
  
  list<STATENODE>
    associatedStates;     
};

class TAGNODE_PTR_CMP
{
 public: 
  int operator() (TAGNODE *p1, TAGNODE *p2) 
    {return((*p1).betterDifferenceThan(*p2));}
};

//-----------------------------------------------------------------------------
// TRELLIS class
//-----------------------------------------------------------------------------

// The TRELLIS class holds the actual "viterbi trellis", including the
// sequence of elements which are our observations ("words"). There are
// functions for computing the forward/backward probability. The forward
// computation includes the creation of states. Then there are some
// functions for filtering out tags on the basis of different criteria.

class TRELLIS 
{
 public:
  
  TRELLIS(vector<string> const &words, vvTAGNODE const &trellis,
	  string startTag, string endTag);

  ~TRELLIS();

  int getTagCount()  
    {return tagCount;}

  int getWordCount() 
    {return wordCount;}
  
  void computeForwardProbabilities(); 
  void computeBackwardProbabilities();
  void computeForwardBackwardValues();
  void computeBaselineValues();
  void rankTags();  
  void keepNBestTags(int n);
  void keepPercentageOfTags(int p);
  void keepGoodTags(double margin);  
  void keepMinProb(double margin);  
  void normalizeScore();
  int  collectGoodTags(int *remainingTags,
                       double *remainingTagScores,
                       int debug);
  int  compareWithCorrect(vector<pair<string,pair<int,int> > > corrTags, int debug);
  void countWordsAndTags(int onlyNonDeleted);
  void expandTags();
  void reduceTags();
  void print();
  void printTagging();

 private:
  
  vector<string> 
    words;
  
  vvTAGNODE 
    trellis;

  int 
    tagCount,
    wordCount;
};

#endif /* _TAGGER_H_ */

//-----------------------------------------------------------------------------

