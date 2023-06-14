//-----------------------------------------------------------------------------
// tagger.cc -- implementation of tagger.h
//-----------------------------------------------------------------------------

#include "tagger.h"

//-----------------------------------------------------------------------------
// GLOBAL VARIABLES
//-----------------------------------------------------------------------------

// counting proportion of unknown words
int
  g_knownWords             = 0,
  g_unknownWords           = 0;

// fadd structures identifiers
long int 
  fourgramFaddNumber       = -1,
  trigramFaddNumber        = -1,
  bigramFaddNumber         = -1,
  contextFaddNumber        = -1,
  prefixBigramFaddNumber   = -1,
  prefixTrigramFaddNumber  = -1,
  wordTagFaddNumber        = -1,
  tagWordFaddNumber        = -1,
  wordFaddNumber           = -1,
  wordTagLexiconFaddNumber = -1;

#ifdef __cplusplus
extern "C" {
#endif
int 
  g_model                  = 2,
  g_threshold              = 0,
  g_percentage             = 50,
  g_debug                  = 0,
  g_lexicalAnalysis        = 0,
  g_baseline               = 0;
double g_min_prob          = 2;
#ifdef __cplusplus
}
#endif
		
double
  g_diversity              = 10;

string
  g_dataDir                = "";

string 
  g_startTag               = "",
  g_endTag                 = "";

vector<string>
  g_usedContextLabels;

vector<string>
  g_heuristicNamesSingleTags,    // tags used for single-tag names
  g_heuristicNamesMultiTags;     // tags used for multi-tag names

int
  g_usedTagSet;                  // for selecting notation format in expanding multi-word tags

//-----------------------------------------------------------------------------
// GLOBAL FUNCTIONS
//-----------------------------------------------------------------------------

// - "interface functions" can be called by the user
// - "functions used internally" are called by interface functions

//-----------------------------------------------------------------------------
// interface functions
//-----------------------------------------------------------------------------

void tagger_init(int    p_model,
		 int    p_threshold,
		 int    p_percentage,
		 double    p_min_prob,
		 int    p_debug,
		 int    p_baseline,
                 int    p_lexicalAnalysis,
		 double p_diversity,
		 string p_startTag,
		 string p_endTag,
		 string p_dataDir, 
		 string p_wordTagTupleFile, 
		 string p_tagWordTupleFile, 
		 string p_contextTagTupleFile,
		 string p_bigramTupleFile, 
		 string p_trigramTupleFile, 
		 string p_fourgramTupleFile, 
		 string p_prefixBigramTupleFile,
		 string p_prefixTrigramTupleFile,
		 string p_wordDictFile, 
		 string p_tagDictFile,
		 string p_contextDictFile,
		 string p_usedContextFile,
		 string p_wordTagLexiconDictFile)
  // This sets parameters that will be used in constructing the trellis, and it
  // loads the fadd structures into memory. Location of the data files can be
  // provided, otherwise default values will be used (see above).
{
  g_model           = p_model;
  g_threshold       = p_threshold;
  g_min_prob        = p_min_prob;
  g_percentage      = p_percentage;
  g_debug           = p_debug;
  g_lexicalAnalysis = p_lexicalAnalysis;
  g_diversity       = p_diversity;
  g_startTag        = p_startTag;
  g_endTag          = p_endTag;
  g_dataDir         = p_dataDir;
  g_baseline        = p_baseline;

  // baseline method does not need tag n-gram probabilities; implemented by setting g_model to 0
  if(g_baseline)
    g_model = 0;
  	
  // initialize fadd library
  // nb the key is ignored at the moment, and the memory is never returned.
  int fadd_key=fadd_init_lib(16);
  if(fadd_key < 0)
    cerr << "TAGGER ERROR: fadd_init_lib returns error code " << fadd_key << endl;

  // load fadd data structures
  tagger_loadfadd(p_dataDir+p_wordTagTupleFile,
		  p_dataDir+p_tagWordTupleFile,
		  p_dataDir+p_contextTagTupleFile,
		  p_dataDir+p_bigramTupleFile,
 		  p_dataDir+p_trigramTupleFile,
		  p_dataDir+p_fourgramTupleFile,
		  p_dataDir+p_prefixBigramTupleFile,
		  p_dataDir+p_prefixTrigramTupleFile,
		  p_dataDir+p_wordDictFile,
		  p_dataDir+p_tagDictFile,
		  p_dataDir+p_contextDictFile,
		  p_dataDir+p_wordTagLexiconDictFile);

  // load context labels used in training data; not necessary for baseline method
  if(!g_baseline)
    tagger_loadContextLabels(p_dataDir+p_usedContextFile);

  // initialize guesser data if needed
  if(g_lexicalAnalysis)
    guesser_initData(p_dataDir);
}

int tagger_filter(vector<string> const &words,
		  vvTAGNODE const &tags,
		  int tagCount,
		  int *remainingTags,
                  double *remainingTagScores,
		  int debug)
  // This can be called to filter the tags of a single sentence. One argument
  // will be an array of integers that represent the tags remaining after 
  // filtering. The sentence is a vector of strings, the tags are in a vector
  // of vectors of TAGNODES.
  // Note: the remainingTags array should be deleted by the caller of the 
  // function when not needed any longer.
{
  if(g_baseline){
    cerr << "TAGGER ERROR: baseline method only available in tagger evaluation" << endl;
    tagger_close();
    exit(1);
  }

  TRELLIS
    trellis(words,tags,g_startTag,g_endTag);

  
  trellis.computeForwardProbabilities();
  trellis.computeBackwardProbabilities();
  trellis.computeForwardBackwardValues();

  trellis.reduceTags();
  
  trellis.rankTags();

  trellis.normalizeScore();

  if(g_percentage>=0){
    trellis.keepPercentageOfTags(g_percentage);
  }else{
    if(g_threshold>=0){
      trellis.keepGoodTags(g_threshold);
    }else{
      trellis.keepMinProb(g_min_prob);
    }
  }

  if(debug>1) trellis.print();

  int remainingCount = trellis.collectGoodTags(remainingTags,remainingTagScores,debug);
  
  return(remainingCount);
}

void tagger_filter_evaluation(vector<string> &words,
			      vvTAGNODE &tags,
			      int tagCount,
			      vector<pair<string,pair<int,int> > > corrTags, 
			      vector<double> &info,
			      int debug)
  // This does the same as the tagger_filter function, except that it fills a
  // supplied vector with evaluation information (about ambiguity and accuracy).
{
  if(g_lexicalAnalysis)
    tagCount = tagger_getPossibleTags(words,tags);

  if(debug>2) cout << "Setting up trellis...";
  TRELLIS
    trellis(words,tags,g_startTag,g_endTag);
  if(debug>2) cout << "done." << endl;
  
  info[0] = trellis.getWordCount();
  info[1] = trellis.getTagCount();

  if(!g_baseline){
    if(debug>2) cout << "Doing forward/backward computations..." << endl;
    trellis.computeForwardProbabilities();
    trellis.computeBackwardProbabilities();
    trellis.computeForwardBackwardValues();
    trellis.reduceTags();
    if(debug>2) cout << "done." << endl;
  }else{
    if(debug>2) cout << "Doing baseline computations..." << endl;
    trellis.computeBaselineValues();
    if(debug>2) cout << "done." << endl;
  }

  if(debug>2) cout << "Ranking tags...";
  trellis.rankTags();
  trellis.normalizeScore();
  if(debug>2) cout << "done." << endl;

  if(debug>2) cout << "Removing bad tags...";
  if(!g_baseline){
    if(g_percentage>=0){
      trellis.keepPercentageOfTags(g_percentage);
    }else{if(g_threshold>=0){
	trellis.keepGoodTags(g_threshold);
      } else {
	trellis.keepMinProb(g_min_prob);
      }
    }
  }else{
    trellis.keepNBestTags(1);
  }
  if(debug>2) cout << "done." << endl;

  trellis.countWordsAndTags(1);
  info[2] = trellis.getWordCount();
  info[3] = trellis.getTagCount();
  
  if(debug>2) cout << "Comparing remaining tags with correct tagging...";
  info[4] = trellis.compareWithCorrect(corrTags,debug);
  if(debug>2) cout << "done." << endl;

  if(debug>2)  trellis.print();
}

void tagger_filter_ask(vector<string> &words,vvTAGNODE &tags,int debug)
  // This does the same as the tagger_filter_evaluation function, except that
  // it does not deal with evaluation information; this is for stand-alone usage
{
  if(g_baseline){
    cerr << "TAGGER ERROR: baseline method only available in tagger evaluation" << endl;
    tagger_close();
    exit(1);
  }

  //  int tagCount;
  //  tagCount = 
  tagger_getPossibleTags(words,tags);

  if(debug>2) cout << "Setting up trellis...";
  TRELLIS
    trellis(words,tags,g_startTag,g_endTag);
  if(debug>2) cout << "done." << endl;
  
  if(debug>2) cout << "Doing forward/backward computations..." << endl;
  trellis.computeForwardProbabilities();
  trellis.computeBackwardProbabilities();
  trellis.computeForwardBackwardValues();
  if(debug>2) cout << "done." << endl;
  
  if(debug>2) cout << "Removing context information from tags...";
  trellis.reduceTags();
  if(debug>2) cout << "done." << endl;
  
  if(debug>2) cout << "Ranking tags...";
  trellis.rankTags();
  if(debug>2) cout << "done." << endl;

  if(debug>2) cout << "Removing bad tags...";
  if(g_percentage>=0){
    trellis.keepPercentageOfTags(g_percentage);
  }else{
    if(g_threshold>=0){
      trellis.keepGoodTags(g_threshold);
    } else {
      trellis.keepMinProb(g_min_prob);
    }
  }
  if(debug>2) cout << "done." << endl;

  trellis.printTagging();

  if(debug>2) trellis.print();
}

void tagger_close()
  // This should be called when the tagger is no longer needed and it will
  // remove the fadd structures from memory.
{
  // close fadd data files
  closeDataFiles();
  
  // close fadd library; not working at the moment?
  // fadd_close_lib();
}

//-----------------------------------------------------------------------------
// functions used internally
//-----------------------------------------------------------------------------

void tagger_loadContextLabels(string usedContextFileName)
  // load the context labels used in the trainingdata; these will be used
  // later in creating copies of states with all possible context labels
{
  ifstream USED_CONTEXT;
  USED_CONTEXT.open(usedContextFileName.c_str());
  if(!USED_CONTEXT){
    cerr << "TAGGER ERROR: cannot load used context labels file" << endl;
    tagger_close();
    exit(1);
  }
  string label;
  while(!USED_CONTEXT.eof()){
    getline(USED_CONTEXT,label);
    if (label.length()>0) {  // ignore empty string at end of file
      // cerr << "READING context " << label << endl;
      g_usedContextLabels.push_back(label);
    }
  }
  USED_CONTEXT.close();
}

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
		     string wordTagLexiconDictFile)
  // load fadd data structures used by the tagger (data used by the
  // lexical analysis part (guesser.cc) is loaded separately)
{

  if(!loadProbsWordTag(wordTagTupleFile.c_str(),
		       wordDictFile.c_str(),
		       tagDictFile.c_str())) {
    cerr << "TAGGER ERROR: cannot load word-tag tuple" << endl;
    tagger_close();
    exit(1);
  }

  if(g_baseline){
    if(!loadProbsTagWord(tagWordTupleFile.c_str(),
			 tagDictFile.c_str(),
			 wordDictFile.c_str())) {
      cerr << "TAGGER ERROR: cannot load tag-word tuple" << endl;
      tagger_close();
      exit(1);
    }
  }

  if(!g_baseline){
    if(!loadProbsContextTag(contextTagTupleFile.c_str(),
			    contextDictFile.c_str(),
			    tagDictFile.c_str(),
			    contextDictFile.c_str())) {
      cerr << "TAGGER ERROR: cannot load context tuple" << endl;
      tagger_close();
      exit(1);
    }
  }

  // "g_model" is the n-gram model we want to use; 2 for bigram etc.
  // Since we are now adding an extra position for context, we use
  // sequences of length n+1 when using an n-gram POS-tag model.

  int n = g_model+1;

  // note: we use a value for g_model of 0 to indicate the use of the 
  // baseline method (in which case not a single tag n-gram model is
  // needed); this means n will be 1, and no unnecessary models will 
  // be loaded below

  if(n >= 2){
    if(!(loadProbsBigram(bigramTupleFile.c_str(),
			 contextDictFile.c_str(),
			 tagDictFile.c_str()))) {
      cerr << "TAGGER ERROR: cannot load bigram tuple" << endl;
      tagger_close();
      exit(1);
    }
  }else{
    if(!g_baseline){
      cerr << "TAGGER ERROR: g_model = " << g_model << "; should be 1 or 2 or 3" << endl;
      tagger_close();
      exit(1);
    }
  }

  if(n >= 3){
    if(!(loadProbsTrigram(trigramTupleFile.c_str(),
			  tagDictFile.c_str(),
			  contextDictFile.c_str(),
			  tagDictFile.c_str()))) {
      cerr << "TAGGER ERROR: cannot load trigram tuple" << endl;
      tagger_close();
      exit(1);
    }

    if(!(loadPrefixBigram(prefixBigramTupleFile.c_str(),
			  tagDictFile.c_str(),
			  contextDictFile.c_str()))) {
      cerr << "TAGGER ERROR: cannot load prefix bigram tuple" << endl;
      tagger_close();
      exit(1);
    }
  }

  if(n >= 4){
    if(!(loadProbsFourgram(fourgramTupleFile.c_str(),
			   tagDictFile.c_str(),
			   tagDictFile.c_str(),
			   contextDictFile.c_str(),
			   tagDictFile.c_str()))) {
      cerr << "TAGGER ERROR: cannot load fourgram tuple" << endl;
      tagger_close();
      exit(1);
    }

    if(!(loadPrefixTrigram(prefixTrigramTupleFile.c_str(),
			  tagDictFile.c_str(),
			  tagDictFile.c_str(),
			  contextDictFile.c_str()))) {
      cerr << "TAGGER ERROR: cannot load prefix trigram tuple" << endl;
      tagger_close();
      exit(1);
    }
  }
}

double addLogProbabilities(double value1, double value2)
  // add two (-100) log probabilities: corresponds to adding 
  // two normal probabilities
{
  double 
    a = value1/double(100),
    b = value2/double(100), 
    c;

  if(a<b){
    c = a-log(1+exp(a-b));
  }else{
    c = b-log(1+exp(b-a));
  }
  
  return(100*c);
}

double transitionProbability(vector<string> const &tagSequence, 
			     vector<string> const &contextSequence,
			     vector<string> const &words,
                             int transitionType)
  // compute the probability of the transition defined by the
  // supplied tag n-gram, context n-gram and observed word(s)
{
  double
    wordProb,    // observation probability; Wj given Tj
    tagProb,     // tag n-gram probability;  Tj given Ti... and Ci
    contextProb; // context probability;     Cj given Tj    and Ci
  
  string
    destinationTag = tagSequence.back(),
    observedWord   = words.front();

  for(vector<string>::const_iterator vi=words.begin()+1; vi!=words.end();
      ++vi){
    observedWord += (" " + *vi);
  }
  
  // create list of words and tag
  list_of_words *wordTagList = new list_of_words;
  
  wordTagList->word       = observedWord.c_str();
  wordTagList->next       = new list_of_words;
  wordTagList->next->word = destinationTag.c_str();
  wordTagList->next->next = NULL;
  
  // get observation probability
  wordProb = wordTagProb(wordTagList);
  
  if(g_debug>2){
    cout << "word-tag (1): " << wordTagList->word << " ";
    cout << wordTagList->next->word << " " << wordProb << endl;
  }

  destroyWordList(wordTagList);
  
  // create list of tags and context
  vector<string> tagsPlusContext = tagSequence;
  
  tagsPlusContext.pop_back();
  for(vector<string>::const_iterator s=contextSequence.begin();
      s!=contextSequence.end()-1 ; ++s){
    tagsPlusContext.push_back(*s);
  }
  tagsPlusContext.push_back(tagSequence.back());

  list_of_words *tagsPlusContextList = createWordList(tagsPlusContext);
  

  if(transitionType == 2)  // only possible mwu transition
    tagProb=0;
  else 
    // get tag n-gram probability
    if(tagsPlusContext.size() < 5){
      tagProb = ngramProb(tagsPlusContextList,1,tagsPlusContext.size(),INTERPOLATION_LIMIT,0) ;
    }else{
      cerr << "TAGGER ERROR: supplied n-gram is of unsupported size " << tagsPlusContext.size() << endl;
      exit(1);
    }
  
  if(g_debug>2){
    cout << "tag-" << tagsPlusContext.size() << "gram: ";
    list_of_words
      *temp = tagsPlusContextList;
    while(temp != NULL){
      cout << temp->word << " ";
      temp = temp->next;
    }
    cout << tagProb  << endl;
  }

  destroyWordList(tagsPlusContextList);
 
  // create list of context and tags
  vector<string> context = contextSequence;
  context.pop_back();         
  context.push_back(destinationTag);
  context.push_back(contextSequence.back());
  
  list_of_words *contextList = createWordList(context);
  
  // get context n-gram probability
  if(context.size() == 3){
    contextProb = ngramProb(contextList,1,3,3,1);
  }else{
    cerr << "TAGGER ERROR: supplied context n-gram is of unsupported size " << context.size() << endl;
    exit(1);
  }

  if(g_debug>2){
    cout << "context-" << context.size() << "gram: " ;
    for(vector<string>::iterator vi=context.begin() ; vi!=context.end() ; vi++){
      cout << (*vi) << " " ;
    }
    cout << contextProb << endl;
  }

  destroyWordList(contextList);
  
  // combine the three values
  return(wordProb + tagProb + contextProb);
}

double wordTagProb(list_of_words *wordTagList)
  // retrieve probability of word given tag
{	
  list_of_numbers *probability = word_tuple_grams(wordTagList,wordTagFaddNumber);
  
  double value = (probability == NULL ? MAX_WORD_TAG_VALUE : probability->word);
  
  destroyNumberList(probability);

  if(value > MAX_WORD_TAG_VALUE)
    return(MAX_WORD_TAG_VALUE);

  return(value);
}

double tagWordProb(list_of_words *tagWordList)
  // retrieve probability of tag given word
{	
  list_of_numbers *probability = word_tuple_grams(tagWordList,tagWordFaddNumber);
  
  double value = (probability == NULL ? MAX_TAG_WORD_VALUE : probability->word);
  
  destroyNumberList(probability);

  if(value > MAX_TAG_WORD_VALUE)
    return(MAX_TAG_WORD_VALUE);

  return(value);
}

double ngramProb(list_of_words *tags, double totalLambda, int n, int interpolationLimit, int useContextFadd) 
  // compute probability of n gram (through interpolation with 
  // n-1 gram if n is actually above the interpolation limit)
  // weight probability with totalLambda (or use totalLambda as
  // total to be divided when doing further interpolation)
{
  /////////////////////////////////////////////////////////////
  // NOTE: perhaps value should be weighted by lambda even if 
  //       MAX_NGRAM_VALUE is used?
  /////////////////////////////////////////////////////////////
  
  double
    ngramValue,ngramLambda,
    lowerOrderValue,lowerOrderLambda;

  long int
    prefixFaddNumber = 0,
    ngramFaddNumber = 0;

  // get the correct fadd number
  if(useContextFadd){
    ngramFaddNumber = contextFaddNumber;
  }else{
    switch(n){
    case 2:
      ngramFaddNumber  = bigramFaddNumber;
      break;
    case 3:
      ngramFaddNumber  = trigramFaddNumber;
      prefixFaddNumber = prefixBigramFaddNumber;
      break;
    case 4:
      ngramFaddNumber  = fourgramFaddNumber;
      prefixFaddNumber = prefixTrigramFaddNumber;
      break;
    }
  }

  if(n > interpolationLimit){ 

    //////////////////////
    // do interpolation //
    //////////////////////
    
    // get prefix
    vector <string> prefixVector;
    list_of_words *current = tags;
    for(int i=0 ; i<n-1 ; ++i){
      prefixVector.push_back(current->word);
      current = current->next;
    }

    // create list of words with same contents
    list_of_words *prefixList = createWordList(prefixVector);
  
    // create list of numbers of data associated with prefix
    list_of_numbers *prefixData = word_tuple_grams(prefixList,prefixFaddNumber);
    
    // compute lambda
    if(prefixData != NULL){  

      int
	prefixCount     = prefixData->word,
	prefixDiversity = prefixData->next->word;

      ngramLambda = prefixCount/(prefixCount+g_diversity*prefixDiversity);


    }else{
      if(g_debug>4) {
	cout << "prefixData NULL! ";
	for(int i=0 ; i<n-1 ; ++i){
	  cout << prefixVector[i] << " ";
	}
	cout << endl;
      }
      
      // prefix not found: ngram lambda will be set to zero
      // return MAX_NGRAM_VALUE;
      ngramLambda = 0;
    }

    ngramLambda *= totalLambda;


    destroyWordList(prefixList);
    destroyNumberList(prefixData);
        
    // compute lambda remaining for lower n-gram
    lowerOrderLambda = totalLambda - ngramLambda;
      
    // compute probability of lower n-gram
    lowerOrderValue = ngramProb(tags->next,lowerOrderLambda,n-1,interpolationLimit,useContextFadd);

    // compute probability of n-gram
    if(ngramLambda > 0){
      
      list_of_numbers *ngramData = word_tuple_grams(tags,ngramFaddNumber);
      
      if(ngramData == NULL){
	// ngram not found; not considered in interpolation
	ngramValue  = lowerOrderValue;
      }else{
	// ngram found; combine with lower ngram value
	ngramLambda = -100*log(ngramLambda);
	ngramValue  = addLogProbabilities(ngramData->word+ngramLambda,lowerOrderValue);
      }
      destroyNumberList(ngramData);
    }else{
      // ngramLambda is 0; ngram value not considered in interpolation
      ngramValue = lowerOrderValue;
    }

  }else{

    //////////////////////
    // no interpolation //
    //////////////////////
    
    // use totalLambda for lambda
    ngramLambda = totalLambda;

    list_of_numbers *ngramData = word_tuple_grams(tags,ngramFaddNumber);
    
    if(ngramData == NULL){
//       if(useContextFadd) {
//         list_of_words *cw=tags;
//         if (cw -> word == cw -> next -> next -> word)
//           ngramValue = 0;
//         else
//           ngramValue = 1350;
//       } else 
      // ngram not found; use default bad value
      // ngramLambda = -100*log(ngramLambda);
        ngramValue  = MAX_NGRAM_VALUE; // + ngramLambda;
    }else{
      ngramLambda = -100*log(ngramLambda);
      ngramValue  = ngramData->word + ngramLambda;	
    }
    destroyNumberList(ngramData);
  }

  if (ngramValue > MAX_NGRAM_VALUE) { ngramValue = MAX_NGRAM_VALUE; }
  if (ngramValue < 0 ) { ngramValue = 0; }

  return(ngramValue);
}

list_of_words *createWordList(vector<string> const &words)
  // create list_of_words from all elements in vector 'words'
{
  list_of_words 
    *first   = new list_of_words,
    *current = first;
  
  current->next = NULL;
  for(vector<string>::const_iterator vi=words.begin(); vi!=words.end();
      vi++){
    current->word = (*vi).c_str();
    if(vi!=words.end()-1){
      current->next = new list_of_words;
      current       = current->next;
    }else{
      current->next = NULL;
    }
  }
  return(first);
}

void destroyWordList(list_of_words *list)
  // remove such a linked list from memory
{
  list_of_words 
    *current = list,
    *next;
  
  while(current != NULL){
    next = current->next;
    delete(current);
    current = next;
  }
}

void destroyNumberList(list_of_numbers *list)
  // remove such a linked list from memory
{
  list_of_numbers 
    *current = list,
    *next;
  
  while(current != NULL){
    next = current->next;
    delete(current);
    current = next;
  }
}

int loadProbsFourgram(const char *tupleFile, 
		      const char *dictFile0, 
		      const char *dictFile1, 
		      const char *dictFile2,
		      const char *dictFile3)
  // initialize fourgram data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words,
    *dict2 = new list_of_words,
    *dict3 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  dict2->word = dictFile2;
  dict3->word = dictFile3;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = dict2;
  dict2->next = dict3;
  dict3->next = NULL;
  
  fourgramFaddNumber = init_tuple(tuple);
  
  destroyWordList(tuple);
  
  return(fourgramFaddNumber != -1);
}

int loadProbsTrigram(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1, 
		     const char *dictFile2)
  // initialize trigram data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words,
    *dict2 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  dict2->word = dictFile2;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = dict2;
  dict2->next = NULL;
  
  trigramFaddNumber = init_tuple(tuple);
  
  destroyWordList(tuple);
  
  return(trigramFaddNumber != -1);
}

int loadProbsBigram(const char *tupleFile, 
		    const char *dictFile0, 
		    const char *dictFile1)
  // initialize bigram data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = NULL;
  
  bigramFaddNumber = init_tuple(tuple);
  
  destroyWordList(tuple);
  
  return(bigramFaddNumber != -1);
}

int loadProbsWordTag(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1)
  // initialize word+tag data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = NULL;
  
  wordTagFaddNumber = init_tuple(tuple);
  if(wordTagFaddNumber)
    wordFaddNumber = init_dict(dictFile0,FADD_HASH);
  
  destroyWordList(tuple);	
  
  return(wordTagFaddNumber != -1);
}

int loadProbsTagWord(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1)
  // initialize tag+word data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = NULL;
  
  tagWordFaddNumber = init_tuple(tuple);

  destroyWordList(tuple);	
  
  return(tagWordFaddNumber != -1);
}

int loadProbsContextTag(const char *tupleFile, 
			const char *dictFile0, 
			const char *dictFile1,
			const char *dictFile2)
  // initialize context data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words,
    *dict2 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  dict2->word = dictFile2;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = dict2;
  dict2->next = NULL;
  
  contextFaddNumber = init_tuple(tuple);
  
  destroyWordList(tuple);
  
  return(contextFaddNumber != -1);
}

int loadPrefixBigram(const char *tupleFile, 
		     const char *dictFile0, 
		     const char *dictFile1)
  // initialize prefix bigram data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = NULL;
  
  prefixBigramFaddNumber = init_tuple(tuple);
  
  destroyWordList(tuple);
  
  return(prefixBigramFaddNumber != -1);
}

int loadPrefixTrigram(const char *tupleFile, 
		      const char *dictFile0, 
		      const char *dictFile1,
		      const char *dictFile2)
  // initialize prefix tigram data
{
  list_of_words 
    *tuple = new list_of_words,
    *dict0 = new list_of_words,
    *dict1 = new list_of_words,
    *dict2 = new list_of_words;
  
  tuple->word = tupleFile;
  dict0->word = dictFile0;
  dict1->word = dictFile1;
  dict2->word = dictFile2;
  
  tuple->next = dict0;
  dict0->next = dict1;
  dict1->next = dict2;
  dict2->next = NULL;
  
  prefixTrigramFaddNumber = init_tuple(tuple);
  
  destroyWordList(tuple);
  
  return(prefixTrigramFaddNumber != -1);
}

void closeDataFiles()
  // close all tuples
{
  int n = g_model+1;

  if(n >= 2)
    close_tuple(bigramFaddNumber);
  if(n >= 3){
    close_tuple(trigramFaddNumber);
    close_tuple(prefixBigramFaddNumber);
  }
  if(n >= 4){
    close_tuple(fourgramFaddNumber);
    close_tuple(prefixTrigramFaddNumber);
  }

  if(!g_baseline)
    close_tuple(contextFaddNumber);

  close_tuple(wordTagFaddNumber);

  if(g_lexicalAnalysis)
    guesser_closeData();
}

int tagger_expandMultiWordTag(int p_wordCount,string p_baseTag,unsigned int p_sentencePos, vvTAGNODE &p_tags,
			      const vector<string> &p_words)
  // given a base tag, number of associated words and position of first word, add numbered multi-word
  // tags to the list of possible tags for the relevant positions in the sentence (vvtagnode vector)
{
  unsigned int 
    newPos         = p_sentencePos;
  
  int tagCount = 0;

  for(int counter=1 ; counter<p_wordCount+1 ; ++counter){
  
    stringstream tempStream(stringstream::out);

    // select notation format for multi-word tag
    if(g_usedTagSet == TAGSET_WOTAN){
      tempStream << counter << "-" << p_wordCount << "-" << p_baseTag;
    }else if(g_usedTagSet == TAGSET_ALPINO){
      tempStream << counter << "-" << p_baseTag;
    }
    
    string numberedTag(tempStream.str());
    
    // only add if not yet added (note: newPos+1 to get at tag position (skip dummy))
    unsigned int exists = 0;
    for(vector<TAGNODE>::iterator tn=p_tags[newPos+1].begin() ; tn!=p_tags[newPos+1].end() ; ++tn){
      if(tn->getTagName() == numberedTag){
	exists = 1;
      }
    }
    
    if(!exists){
      TAGNODE tagNode(numberedTag,p_words,newPos,1,DEFAULT_TAG_ID);    
      p_tags[newPos+1].push_back(tagNode);
      tagCount++;
    }  
  
    ++newPos;
  }

  return(tagCount);
}

void tagger_getRequiredWords(string &p_tagWords,vector<string> &p_requiredWords)
  // extract required words from a multi-word tag in compact notation ("tag^words")
{
  string::size_type wordSeparatorPos = p_tagWords.find(',');
  while(wordSeparatorPos != string::npos){
    string requiredWord(p_tagWords,0,wordSeparatorPos);
    p_requiredWords.push_back(requiredWord);
    p_tagWords.erase(0,wordSeparatorPos+1);
    wordSeparatorPos = p_tagWords.find(',');
  }
  p_requiredWords.push_back(p_tagWords);
}

int tagger_requiredWordsAvailable(vector<string> &p_reqWords, const vector<string> &p_words, 
				  vector<string>::const_iterator p_checkPos)
  // check if words required by a multi-word tag are indeed available in the sentence
  // at the right locations; also returns false if not enough positions in sentence
{
  for(vector<string>::iterator s=p_reqWords.begin() ; s!=p_reqWords.end() ; ++s){
    if(p_checkPos != p_words.end()){
      if(*p_checkPos != *s){
	return(0);
      }
      ++p_checkPos;
    }else{
      return(0);
    }
  }
  return(1);
}

int tagger_getPossibleTags(const vector<string> &words, vvTAGNODE &tags)
  // collect possible tags for words, return total number of tags
{
  int
    sentencePos    = 0,
    tagCount       = 0,
    heuristicPos   = 0;

  for(vector<string>::const_iterator w=words.begin() ; w!=words.end() ; w++){

    string word = *w;

    vector<string> tagsForWord;

    // 1) use lexicon

    guesser_getTagsFromLexicon(word,tagsForWord);
   
    if(tagsForWord.size()==0){
      g_unknownWords++;
    }else{
      g_knownWords++;
    }

    if(g_baseline){
      if(tagsForWord.size()==0){
	// tagsForWord.push_back(DUMMY_TAG);

	guesser_getTagsFromLexicon(UNKNOWN_WORD,tagsForWord);

	if(tagsForWord.size()==0){
	  cerr << "TAGGER WARNING: baseline method could not find [" << UNKNOWN_WORD << "] in lexicon" << endl;
	}else if(tagsForWord.size()>1){
	  cerr << "TAGGER WARNING: multiple entries for [" << UNKNOWN_WORD << "] in lexicon" << endl;
	}

      }
    }
 
    // 2) apply heuristic for recognizing word at beginning of sentence
    
    if(sentencePos==0){
      if(tagsForWord.size()==0){
	if(isupper(word[0])){
	  string lowerCaseWord(word);
	  lowerCaseWord[0] = tolower(lowerCaseWord[0]);
	  guesser_getTagsFromLexicon(lowerCaseWord,tagsForWord);
	}
      }
    }
    
    // 3) apply heuristics for tagging names
    
    //########################################################################################
    // heuristic for tagging unknown capitalized words as names, not at beginning of sentence
    //########################################################################################
    
    if(sentencePos>0){
      if(tagsForWord.size()==0){
	// word is unknown
	if(isupper(word[0])){
	  // word is capitalized
	  for(vector<string>::iterator t=g_heuristicNamesSingleTags.begin() ; t!=g_heuristicNamesSingleTags.end() ; ++t){
	    TAGNODE tagNode(*t,words,sentencePos,1,DEFAULT_TAG_ID);	
	    tags[sentencePos+1].push_back(tagNode);
	    tagCount++;
	  }
	}
      }
    }
    
    //########################################################################################
    // heuristic for tagging series of capitalized words as multi-word names, not at beginning
    //########################################################################################

    if(!g_baseline){
      if(sentencePos>0){
	if(sentencePos >= heuristicPos){
	  int nameCount = 0;
	  vector<string>::const_iterator checkW = w;
	  // count adjacent capitalized words
	  while((checkW != words.end()) && isupper((*checkW)[0])){
	    ++checkW;
	    ++nameCount;
	  }
	  // if more than one, add multi-word tags
	  if(nameCount>1){
	    string nameTag;
	    for(vector<string>::iterator s=g_heuristicNamesMultiTags.begin() ; s!=g_heuristicNamesMultiTags.end() ; ++s){
	      tagCount += tagger_expandMultiWordTag(nameCount,*s,sentencePos,tags,words);
	    }
	    heuristicPos = sentencePos+nameCount;
	  }
	} 
      }
    }
    
    // 4) copy tags, expand unexpanded multi-word tags only when useful
    
    for(vector<string>::iterator t=tagsForWord.begin() ; t!=tagsForWord.end() ; ++t){
      
      string::size_type separatorPos = (*t).rfind('^');
      
      if(separatorPos != string::npos){
	
	// deal with multi-word tag
	string
	  baseTag(*t,0,separatorPos),
	  tagWords(*t,separatorPos+1,(*t).size()-(separatorPos+1));
	
	vector<string> requiredWords;
	requiredWords.push_back(word);
	tagger_getRequiredWords(tagWords,requiredWords);
	
	if(tagger_requiredWordsAvailable(requiredWords,words,w))
	  tagCount += tagger_expandMultiWordTag(requiredWords.size(),baseTag,sentencePos,tags,words);
	
      }else{
	
	// deal with normal tag	
	TAGNODE tagNode(*t,words,sentencePos,1,DEFAULT_TAG_ID);	
	tags[sentencePos+1].push_back(tagNode);
	tagCount++;
      }
    }
    
    // 5) if resulting list of tags is still empty, use suffix dictionary
    
    if(tags[sentencePos+1].size()==0){
      tagsForWord.clear();

      if(USE_EXTERNAL_FSA_GUESS){
	guesser_getTagsFromSuffixDictionaryOldVersion(word,tagsForWord,g_dataDir);
      }else{
	guesser_getTagsFromSuffixDictionary(word,tagsForWord);
      }

      if(tagsForWord.size()!=0){
	// copy tags (note: suffix automaton does not contain multi-word tags)
	for(vector<string>::iterator t=tagsForWord.begin() ; t!=tagsForWord.end() ; ++t){
	  TAGNODE tagNode(*t,words,sentencePos,1,DEFAULT_TAG_ID);	
	  tags[sentencePos+1].push_back(tagNode);
	  tagCount++;
	}
      }
    }
    
    // 6) if still no tags, repeat suffix search with suffixes of word (...)
    
    if(tags[sentencePos+1].size()==0){
      tagsForWord.clear();
      
      int 
	wordLength   = word.size(),
	suffixLength = wordLength-1;
      
      string suffix;
      
      while((tagsForWord.size()==0) && (suffixLength>=0)){
	suffix = word.substr(wordLength-suffixLength,suffixLength);
	
	if(USE_EXTERNAL_FSA_GUESS){
	  guesser_getTagsFromSuffixDictionaryOldVersion(suffix,tagsForWord,g_dataDir);
	}else{
	  guesser_getTagsFromSuffixDictionary(suffix,tagsForWord);
	}

	--suffixLength;
      }
      
      if(tagsForWord.size()!=0){
	// copy tags (note: suffix automaton does not contain multi-word tags)
	for(vector<string>::iterator t=tagsForWord.begin() ; t!=tagsForWord.end() ; ++t){
	  TAGNODE tagNode(*t,words,sentencePos,1,DEFAULT_TAG_ID);	
	  tags[sentencePos+1].push_back(tagNode);
	  tagCount++;
	}
	// why did step 5 not find this? show warning
	cerr << "TAGGER WARNING: word [" << word << "] " << "received tags on basis of suffix [" << suffix << "]" << endl;
      }
    }
      
    // 7) if *still* no tags: this is currently treated as an error
    
    if(tags[sentencePos+1].size()==0){
      cerr << "TAGGER ERROR: not a single tag assigned to word [" << word << "]" << endl;
      tagger_close();
      exit(1);
    }

    ++sentencePos;
  }
  
  if(g_debug>2){
    sentencePos = 0;
    cout << endl << "LEXICAL ANALYSIS:" << endl << endl;
    for(vvTAGNODE::iterator v=tags.begin() ; v!=tags.end() ; ++v){
      if((v==tags.begin()) || (v==tags.end()-1)){
	cout << "word: [" << DUMMY_WORD         << "]" << endl << endl;
      }else{
	cout << "word: [" << words[sentencePos] << "]" << endl << endl;
	++sentencePos;
      }
      cout << "tags:" << endl;
      for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
	cout << t->getTagName() << endl;
      }
      cout << endl;
    }
  }

  return(tagCount);
}

//-----------------------------------------------------------------------------
// TRELLIS member functions
//-----------------------------------------------------------------------------

TRELLIS::TRELLIS(vector<string> const &words, 
		 vvTAGNODE const &p_trellis,
		 string startTag, 
		 string endTag)
  :
  words(words),           
  trellis(p_trellis) // NOTE: includes start and end dummy positions already
{
  countWordsAndTags(0);

  if(!g_baseline){ 
    // increase number of tags by adding context information
    expandTags();
  }
 
  // note: it is important that this is done *before* adding dummy tagnodes
  // (below), since otherwise we would end up with dummy tagnodes that have
  // non-dummy context labels assigned to them
  
  // add dummy tagnodes at begin and end of trellis
  
  if(startTag == "")
    startTag = SENTENCE_START;
  if(endTag == "")
    endTag   = SENTENCE_END;
  
  TAGNODE
    dummyTagNodeStart(startTag,words,-1,1,DEFAULT_TAG_ID),
    dummyTagNodeEnd(endTag,words,-1,1,DEFAULT_TAG_ID);

  dummyTagNodeStart.mark_l_reachable();
  
  dummyTagNodeStart.setContextName(DUMMY_CONTEXT);
  dummyTagNodeEnd.setContextName(DUMMY_CONTEXT);
  
  int historySize;
  
  if(g_model == 0){
    historySize = 1;
  }else{
    historySize = g_model-1;
  }
  
  vector<string> 
    stateInternalHistoryStart(historySize,startTag),
    stateInternalContextHistory(CONTEXT_STATESIZE,DUMMY_CONTEXT);
  
  // add statenode to first dummy tagnode
  dummyTagNodeStart.addDummyState(stateInternalHistoryStart,stateInternalContextHistory);
  
  // add dummy tagnodes to first and last position of trellis
  trellis.begin()->push_back(dummyTagNodeStart);
  (trellis.end()-1)->push_back(dummyTagNodeEnd);
}

TRELLIS::~TRELLIS()
{}

void TRELLIS::computeForwardProbabilities()
  // forward computation; includes the adding of new states
{
  // assign initial Alpha (0) to all starting states
  for(vector<TAGNODE>::iterator vi=trellis.begin()->begin() ; vi!=trellis.begin()->end() ; ++vi){
    vi->setAllStatesTo(0,DIRECTION_FORWARD);
  }
  
  // for every vector v in the trellis except the last in-sentence one
  for(vvTAGNODE::iterator v=trellis.begin() ; v!=trellis.end()-1 ; ++v){

    // for every TAGNODE t in v
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      
      int tagSpan = t->getTagSpan();
      if (t->l_reachable()) {
      
	list<STATENODE> *associatedStates = t->getAssociatedStates();
	
	// for every STATENODE s in t
	for(list<STATENODE>::iterator s=associatedStates->begin() ; s!=associatedStates->end() ; ++s){
	  
	  // get iterator to vector at position v+tagSpan
	  vvTAGNODE::iterator v2(v+tagSpan);
	  
	  // for every TAGNODE t2 in vector v2
	  for(vector<TAGNODE>::iterator t2=v2->begin() ; t2!=v2->end() ; ++t2){
	    
	    int legal = legal_transition(s->getTagName(),t2->getTagName());

	    if (legal) {
	      
	      // tagnode can be reached
	      t2->mark_l_reachable();
	      
	      // create new state, history based on s, add to tagNode t2
	      list<STATENODE>::iterator newState = t2->addNewState(s);
	 
	      // create transition
	      double transitionProb = s->createTransition(newState,legal); 
	      
	      // add transition probability to forward probability
	      newState->addToComputation(s,transitionProb,DIRECTION_FORWARD);
	    } else {
	      if (g_debug>2) {
		cout << "filtered " << s->getTagName() << " --> " << 
		  t2->getTagName() << endl;
	      }
	    }
	  }
	}
      }
    }
  }
}

void TRELLIS::computeBackwardProbabilities()
  // backward computation; uses trellis constructed during forward computation
{
  // assign initial Beta (0) to all final states
  for(vector<TAGNODE>::iterator vi=(trellis.rbegin())->begin() ; vi!=(trellis.rbegin())->end() ; ++vi){
    vi->setAllStatesTo(0,DIRECTION_BACKWARD);
    vi->setAllStatesReach();
  }
  
  // visiting in reversed order all vector v in trellis
  for(vvTAGNODE::reverse_iterator v=trellis.rbegin()+1 ; v!=trellis.rend() ; ++v){
 
    // for every TAGNODE t in v
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      if(t->l_reachable()) {

	// for every STATENODE s in t
	
	for(list<STATENODE>::iterator s=t->getAssociatedStates()->begin() ; s!=t->getAssociatedStates()->end() ; ++s){

	  vector<TRANSITION> transitions = s->getTransitions();
	  
	  // for every state s2 that is reachable from s
	  for(vector<TRANSITION>::iterator tr=transitions.begin() ; tr!=transitions.end() ; ++tr){
	    
	    if (tr->getTargetState()->r_reachable()) {
	      s->mark_r_reachable();
	    	    
	    // add transition probability to backward probability
	    s->addToComputation(tr->getTargetState(),tr->getTransitionProbability(),DIRECTION_BACKWARD);
	    }
	  }
	} 
      }
    }
  }
}


void TRELLIS::computeForwardBackwardValues()
  // combine the results of the two computations
{
  for(vvTAGNODE::iterator vi=trellis.begin() ; vi!=trellis.end() ; ++vi){
    for(vector<TAGNODE>::iterator t=vi->begin() ; t!=vi->end() ; ++t){
      t->computeForwardBackwardValue();
    }		
  }
}

void TRELLIS::computeBaselineValues()
  // visit all tags and assign just word-tag probabilities as part of the baseline method
{
  // the baseline method selects for each word the tag that occurs most often 
  // with that word; this value is assigned to the forward-backward field of the
  // tagnode as the tags will later be ranked on the value stored in that field

  // for every vector v in the trellis except the last in-sentence one
  for(vvTAGNODE::iterator v=trellis.begin() ; v!=trellis.end()-1 ; ++v){

    // for every TAGNODE t in v
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      
      int tagSpan = t->getTagSpan();
      
      list<STATENODE> *associatedStates = t->getAssociatedStates();
      
      // for every STATENODE s in t
      for(list<STATENODE>::iterator s=associatedStates->begin() ; s!=associatedStates->end() ; ++s){

	// get iterator to vector at position v+tagSpan
	vvTAGNODE::iterator v2(v+tagSpan);

	// for every TAGNODE t2 in vector v2
	for(vector<TAGNODE>::iterator t2=v2->begin() ; t2!=v2->end() ; ++t2){
	  
	  // tagnode can be reached
	  t2->mark_l_reachable();
	  
	  // compute baseline value and store as forward-backward value

	  // get word and tag
	  vector<string>
	    observedWords = t2->getObservedWords();
	  
	  string
	    tag  = t2->getTagName(),
	    word = *(observedWords.begin());
	  
	  for(vector<string>::iterator vi=observedWords.begin()+1 ; vi!=observedWords.end() ; ++vi){
	    word += (" " + *vi);
	  }
	  
	  // create list of tag and word
	  list_of_words *tagWordList = new list_of_words;
	  
	  tagWordList->word       = tag.c_str();
	  tagWordList->next       = new list_of_words;
	  tagWordList->next->word = word.c_str();
	  tagWordList->next->next = NULL;
	  
	  // get observation probability and store as forward-backward value
	  t2->setForwardBackwardProbability(tagWordProb(tagWordList));
	  
	  destroyWordList(tagWordList);
	}
      }
    }
  }
}

void TRELLIS::rankTags()
  // put tags in order of increasing forward/backward value
{
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    sort(vi->begin(),vi->end());
  }
}

void TRELLIS::keepNBestTags(int n)
  // keep only the n best tags, remove rest; call this function after rankTags;
  // tags (tagNodes) that are to be removed are actually marked as deleted
{
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    int number = 1;
    for(vector<TAGNODE>::iterator t=vi->begin() ; t!=vi->end() ; ++t){
      if(!(t->markedAsDeleted())){
	if(number++>n){
	  t->markAsDeleted();
	}
      }
    }
  }
}

void TRELLIS::keepPercentageOfTags(int p)
  // keep the best p percent of all tags; call this function after rankTags
{
  int 
    tagCount = 0,
    targetCount;
  
  // count total number of tags
  for(vvTAGNODE::iterator v=trellis.begin()+1 ; v!=trellis.end()-1 ; ++v){
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      if(!(t->markedAsDeleted())){
	tagCount++;
      }
    }
  }
    
  // compute number of tags to keep
  targetCount = p * tagCount / 100;
  
  // for all tags, compute their difference to the best
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    
    if(vi->size() > 0){ 
      
      // get first non-deleted tag (also best tag)
      vector<TAGNODE>::iterator t=vi->begin();
      while((t!=vi->end()) && (t->markedAsDeleted())) t++;
     
      if(t!=vi->end()){
	t->markAsBest();
	double best = t->getForwardBackwardProbability();
	
	// now compute difference with best tag for current and following tags
	for( ; t!=vi->end() ; ++t){
	  // only necessary if not deleted
	  if(!(t->markedAsDeleted())){
	    t->computeDifferenceWithBest(best);
	  }
	}
      }
    }
  }

  vector<TAGNODE*> allTags;

  // collect all pointers to tags in one vector "allTags"
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    for(vector<TAGNODE>::iterator t=vi->begin() ; t!=vi->end() ; ++t){
      // add only non-deleted tags
      if(!(t->markedAsDeleted())){
	allTags.push_back((TAGNODE *&) t);
      }
    }
  }
  
  // sort this vector of iterators on their differenceWithBest values
  sort(allTags.begin(),allTags.end(),TAGNODE_PTR_CMP());
  
  int seen = 0;
  
  //double currentValue, lastValue = 0;
  
  // now mark bad tags; remove tag if: seen at least targetCount 
  // AND this is not the best tag for a given position

  for(vector<TAGNODE*>::iterator t=allTags.begin() ; t!=allTags.end() ; t++){
    //currentValue = (*t)->getDifferenceWithBest();
    if((seen >= targetCount) && (!((*t)->markedAsBest()))){ 
      // tags with same value are both retained when using following check
      //if(currentValue > lastValue)
      (*t)->markAsDeleted();
    }
    //lastValue = currentValue;
    seen++;
  }
}
	
void TRELLIS::keepMinProb(double margin)
// keep only tags that are good enough; call this function after rankTags
// tags are deleted if their normalized log prob is > margin 
{
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    // vector of tags could be empty
    if(vi->size()){
      for(vector<TAGNODE>::iterator t=vi->begin() ; t!=vi->end() ; ++t){
	// cerr << "check if " << t -> getNormalizedProbability() << " > " << margin << endl;
	if(t->getNormalizedProbability() > margin){
	  t->markAsDeleted();
	}
      }
    }
  }
}

void TRELLIS::keepGoodTags(double margin)
  // keep only tags within margin of best; call this function after rankTags
{
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    // vector of tags could be empty
    if(vi->size()){
      // get first non-deleted tag (also best tag)
      vector<TAGNODE>::iterator t=vi->begin();
      while((t!=vi->end()) && (t->markedAsDeleted())) t++;
      double bestPlusMargin = t->getForwardBackwardProbability() + margin;
      for(t = t+1 ; t!=vi->end() ; ++t){
	if(!(t->markedAsDeleted())){
	  if(t->getForwardBackwardProbability() > bestPlusMargin){
	    t->markAsDeleted();
	  }
	}
      }
    }
  }
}

void TRELLIS::normalizeScore()
// compute normalized log-probability per tag
// ignore deleted tags, these are deleted because of subsumption, not because of filtering!
{
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    // vector of tags could be empty
    if(vi->size()){
      // get first non-deleted tag (also best tag)
      vector<TAGNODE>::iterator t=vi->begin();
      while((t!=vi->end()) && (t->markedAsDeleted())) t++;
      double Normalizer = t->getForwardBackwardProbability();
      for(t = t+1 ; t!=vi->end() ; ++t){
	if(!(t->markedAsDeleted())){
	  Normalizer = addLogProbabilities(Normalizer,t->getForwardBackwardProbability());
	}
      }
      for(t=vi->begin(); t!=vi->end() ; ++t){
	t -> setNormalizedProbability( (t->getForwardBackwardProbability() - Normalizer)/100 );
      }
      t=vi->begin();

    }
  }
}

int TRELLIS::collectGoodTags(int *remainingTags,
                             double *remainingTagScores,
                             int debug)
  // collect id numbers of tags not marked as deleted
{
  int tagCount = 0;

  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    for(vector<TAGNODE>::iterator t=vi->begin() ; t!=vi->end() ; ++t){
      if(!(t->markedAsDeleted())){
	remainingTags[tagCount] = t->getTagID();
        remainingTagScores[tagCount] = t->getNormalizedProbability();
        tagCount++;
      }
    }
  }
  return(tagCount);
}

void TRELLIS::printTagging()
  // print words and tags in nice looking columns with | inbetween
{
  vector<string>::size_type maxLength = 0;
  vector<string>::iterator w;
  for(w=words.begin() ; w!=words.end() ; ++w){
    if(w->size()>maxLength) maxLength=w->size();
  }
  ++maxLength;
  w=words.begin();
  for(vvTAGNODE::iterator vi=trellis.begin()+1 ; vi!=trellis.end()-1 ; ++vi){
    int size = w->size();
    string tab(maxLength-size,' ');
      cout << *w << tab;
    for(vector<TAGNODE>::iterator t=vi->begin() ; t!=vi->end() ; ++t){
      if(!(t->markedAsDeleted())){
	cout << "| " << t->getTagName();
      }
    }
    cout << endl;
    ++w;
  }
}

int TRELLIS::compareWithCorrect(vector<pair<string,pair<int,int> > > corrTags,int debug)
  // return difference between our tagging and 'correct' tagging
{
  int 
    difference = 0,
    position   = 1; // since 0 is dummy position
  
  if(debug>1) cout << "size of trellis: [" << trellis.size() << "]" << endl;
  
  string
    first  = "NONE",
    second = "NONE";

  for(vector<pair<string,pair<int,int> > >::iterator vi=corrTags.begin() ; vi!=corrTags.end() ; ++vi){
    
    string tagName = vi->first;

    pair<int,int> tagPositions = vi->second;
    
    int 
      tagposBegin = tagPositions.first,
      tagposEnd   = tagPositions.second,
      available   = 0;
    
//     if(debug>1) cout << "correct tag is: [" << tagName << "] " << tagposBegin << "-" << tagposEnd << endl;

    string wronglyAssignedTag;
    
    if (debug>1) cout << "output#" << words[tagposBegin];
    for(vector<TAGNODE>::iterator t=trellis[tagposBegin+1].begin() ; t!=trellis[tagposBegin+1].end() ; ++t){
      if(!(t->markedAsDeleted())){
// 	if(debug>1){
// 	  cout << "  remaining   : [" << t->getTagName() << "] " << tagposBegin;
// 	  cout << "-" << tagposBegin + t->getTagSpan();
// 	}

        if(debug>1){
          cout << "|" << t -> getTagName();
        }

	if(t->getTagName() == tagName && t->getTagSpan() == tagposEnd-tagposBegin){
// 	  if(debug>1) cout << " CORRECT";
	  available = 1;
	}else{
	  wronglyAssignedTag = t->getTagName();
	}
      }
    }

	if(debug>1) cout << endl;
    
    if(!available){
      if(debug){
	cout << "MISSING TAG: " << words[tagposBegin] << " " << tagName << " " 
             << tagposBegin << "-" << tagposEnd << endl;
	cout << "MISTAKE: word= " << words[tagposBegin] << " assigned= " 
	     << wronglyAssignedTag << " correct= " << tagName << endl;
      }
      difference++;
    }

    // begin temporary output
    // print word|assignedTag|correctTag in order to do sign test on different models
    //string assignedTag;
    //if(available) 
    //  assignedTag = tagName;
    //else
    //  assignedTag = wronglyAssignedTag;
    //cout << words[tagposBegin] << "|" << assignedTag << "|" << tagName << endl;
    // end temporary output

    first  = second;
    second = tagName;
    
    position++; // moving to position to which next correct tag applies
  }
  
  if(debug>1) cout << endl;
  
  return(difference);
}

void TRELLIS::countWordsAndTags(int onlyNonDeleted)
  // count number of tags in trellis [not marked as deleted], also count words
{
  wordCount = 0;
  tagCount  = 0;
  
  list<pair<int,int> > seenWords;
  
  for(vvTAGNODE::iterator v=trellis.begin()+1 ; v!=trellis.end()-1 ; ++v){
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      if((onlyNonDeleted && !(t->markedAsDeleted())) || (!onlyNonDeleted)){
	
	// a tag is always a unique tag
	tagCount++;
	
	// decide if this word should be added
	pair<int,int> correspondingWord(t->getWordPos(),t->getTagSpan());
	
	int countedAlready = 0;
	
	for(list<pair<int,int> >::iterator li=seenWords.begin() ; li!=seenWords.end() ; li++){
	  if(*li == correspondingWord){
	    countedAlready = 1;
	  }
	}

	if(!countedAlready){
	  seenWords.push_back(correspondingWord);
	  wordCount++;
	}
	
      }
    }
  }
}

void TRELLIS::expandTags()
  // replace each tagnode with as many copies as there are different context
  // labels being used, assigning the different labels to the new tagnodes
{
  vvTAGNODE newTrellis(words.size()+2);
   
  // for every position in trellis
  for(vvTAGNODE::iterator v=trellis.begin()+1 ; v!=trellis.end()-1 ; ++v){
    // for every tag at that position
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      // for all context labels being used
      for(vector<string>::iterator c=g_usedContextLabels.begin() ; c!=g_usedContextLabels.end() ; ++c){
	// create a copy of the original tagnode
	TAGNODE newTagNode(*t);
	// set new context name
	newTagNode.setContextName(*c);
	// add new tag to new trellis
	newTrellis[newTagNode.getWordPos()+1].push_back(newTagNode);
      }
    }
  }
  // replace old trellis with new one
  trellis = newTrellis;
}

void TRELLIS::reduceTags()
  // remove double occurrences of tag at same position, summing their probabilities
  // and storing the result in the first tag encountered, which is not removed
  // double occurrences occur due to introduction of context label; here we
  // are removing effect of context again...
{  
  // for every position in trellis
  for(vvTAGNODE::iterator v=trellis.begin()+1 ; v!=trellis.end()-1 ; ++v){
    // remove all except first in case of multiple occurrences of same tag
    for(vector<TAGNODE>::iterator t1=v->begin() ; t1!=v->end() ; ++t1){
      if(!(t1->markedAsDeleted())){
	for(vector<TAGNODE>::iterator t2=t1+1 ; t2!=v->end() ; ++t2){
	  if(t2->getTagName() == t1->getTagName()){
	    
	    double
	      prob1 = t1->getForwardBackwardProbability(),
	      prob2 = t2->getForwardBackwardProbability(),
	      sum   = addLogProbabilities(prob1,prob2);
	    
	    t1->setForwardBackwardProbability(sum);
	    t1->setContextName(t1->getContextName()+ " + " + t2->getContextName());
	    t2->markAsDeleted();
	    t2->setContextName(t2->getContextName()+" (SUBSUMED)");
	  }
	}
      }
    }   
  }
}

void TRELLIS::print()
  // show trellis; for debugging
{
  cout << endl << "TRELLIS STRUCTURE:" << endl << endl;
  
  for(vvTAGNODE::iterator v=trellis.begin() ; v!=trellis.end() ; ++v){
    for(vector<TAGNODE>::iterator t=v->begin() ; t!=v->end() ; ++t){
      t->print();
      cout << endl;
    }
  }
}

//-----------------------------------------------------------------------------
// TAGNODE member functions
//-----------------------------------------------------------------------------

TAGNODE::TAGNODE(string const &tagName, vector<string> const &sentence,
		 int wordPos, int tagSpan, int tagID)
  :
  tagName(tagName),
  tagID(tagID),
  wordPos(wordPos),
  tagSpan(tagSpan),
  deleted(0), 
  l_reach(0),
  best(0),
  forwardBackwardProbability(0)
{
  // store observedWords for this tag in this tagNode
  for(int i=wordPos ; i<wordPos+tagSpan ; ++i){
    if(i > -1){
      observedWords.push_back(sentence[i]);
    }else{
      observedWords.push_back(DUMMY_WORD);
    }
  }
}

TAGNODE::~TAGNODE()
{}

void TAGNODE::setAllStatesTo(double value, int direction)
  // initialize all states' forward or backward probabilities to value
{
  for(list<STATENODE>::iterator li=associatedStates.begin() ; li!=associatedStates.end() ; ++li){
    li->setProbability(value,direction);
  }
}

void TAGNODE::setAllStatesReach()
{
  for(list<STATENODE>::iterator li=associatedStates.begin() ; li!=associatedStates.end() ; ++li){
    li->mark_r_reachable();
  }
}

list<STATENODE>::iterator TAGNODE::addNewState(list<STATENODE>::iterator sourceState)
  // add a state to the current tagNode, using source state for history data
{
  // tag history:
  vector<string> newHistory(sourceState->getHistory());                 



  // add current tag to end
  newHistory.push_back(tagName);  
  // remove oldest tag
  newHistory.erase(newHistory.begin()); 


  // context history:
  vector<string> newContextHistory(sourceState->getContextHistory());                 
  // add current context to end
  newContextHistory.push_back(contextName);  
  // remove oldest tag
  newContextHistory.erase(newContextHistory.begin()); 

  for(list<STATENODE>::iterator li=associatedStates.begin() ; li!=associatedStates.end() ; ++li){
    // if state with same histories already exists
    if((newHistory == li->getHistory()) && (newContextHistory == li->getContextHistory())){
      // return iterator to existing state
      return(li);
    }
  }

  STATENODE state(newHistory,newContextHistory,observedWords,tagName,contextName);
  
  // otherwise, add new state and return iterator
  associatedStates.push_front(state);
  return(associatedStates.begin());
}

void TAGNODE::addDummyState(vector<string> const &history,
			    vector<string> const &contextHistory)
  // add a statenode without doing the computations of the addState function
{
  STATENODE newState(history,contextHistory,observedWords,tagName,contextName);
  associatedStates.push_back(newState);
}

void TAGNODE::computeForwardBackwardValue()
  // Compute and combine fw/bw values of all states belonging to this tagNode.
  // Since regular probabilities would have to be added (thus representing 
  // probability of reaching one of these states) the log probabilities have 
  // to be added using the function addLogProbabilities.
{

  int r_reachable=1;
  for(list<STATENODE>::iterator s=associatedStates.begin() ; s!=associatedStates.end() ; ++s){
    if(s == associatedStates.begin()){
      forwardBackwardProbability = s->getForwardBackwardValue();
    } else {
      forwardBackwardProbability = addLogProbabilities(forwardBackwardProbability, s->getForwardBackwardValue());
    }
    r_reachable=(r_reachable && s->r_reachable());
  }
  if (!r_reachable) {
    markAsDeleted();
  }
}

void TAGNODE::print()
{

  string observedWord = *observedWords.begin();
  for(vector<string>::iterator vi=observedWords.begin()+1 ; vi!=observedWords.end() ; ++vi){
    observedWord += (" " + *vi);
  }
  
  cout << observedWord << " " << tagName << " " << contextName << " " 
       << wordPos << "-"
       << wordPos+tagSpan << " " << forwardBackwardProbability << " (normalized: " << normalizedProbability << " " << exp(-(normalizedProbability))
       << ") " << (deleted ? "deleted" : "")
       << " " << (l_reach ? "" : "not-reachable")  << endl;
  
  for(list<STATENODE>::iterator li=associatedStates.begin() ; li!=associatedStates.end() ; ++li){
    li->print();
  }
}

//-----------------------------------------------------------------------------
// STATENODE member functions
//-----------------------------------------------------------------------------

STATENODE::STATENODE(vector<string> const &history,
		     vector<string> const &contextHistory,
		     vector<string> const &observedWords,
		     string const &tag, string const &context)
  :
  history(history),
  observedWords(observedWords),
  contextHistory(contextHistory),
  tag(tag),
  context(context),
  probabilityDefined(2),
  probability(2),
  r_reach(0)
{}

STATENODE::~STATENODE()
{}

void STATENODE::setProbability(double p_probability, int direction)
  // set either forward or backward probability of current state
{
  probability[direction]        = p_probability;
  probabilityDefined[direction] = 1;
}

double STATENODE::createTransition(list<STATENODE>::iterator targetState,
				   int transitionType)
  // create a transition object in the current state for transition from this
  // state to targetState; this includes computing the transition probability
{
  // compute transition probability:

  // get associated tag sequence
  vector<string> tagSequence = history;
  tagSequence.push_back(targetState->getTagName());

  // get associated context sequence
  vector<string> contextSequence = contextHistory;
  contextSequence.push_back(targetState->getContextName());

  // transitionType == 1: ordinary
  //                == 2: only possible transition, assign 0

  double transitionProb;

  transitionProb = transitionProbability(tagSequence,
					 contextSequence,
					 targetState->getObservedWords(),
					 transitionType);
  if(transitionProb < 0){
    if(transitionProb < WEIRD_PROB_TOLERANCE){
      cerr << "TAGGER ERROR: weird transitionProb: " << transitionProb << endl;
      print();
    }
    transitionProb = 0;
  }

  // add transition object to state s
  TRANSITION transition(targetState,transitionProb);
  transitions.push_back(transition);

  return(transitionProb);
}

void STATENODE::addToComputation(list<STATENODE>::iterator sourceState,double transitionProb,int direction)
  // add transition probability to forward (or backward) probability computed
  // so far and stored in sourceState; store result in current state
{
  double newProbability = sourceState->getProbability(direction) + transitionProb;
  
  if(probabilityDefined[direction]){
    // add probability to existing value
    probability[direction] = addLogProbabilities(probability[direction],newProbability);
  }else{
    // just use new probability
    probability[direction] = newProbability;
  }

  probabilityDefined[direction] = 1;
}

double STATENODE::getForwardBackwardValue()
  // compute fw/bw value as sum of fw and bw value 
  // (sum, since values are log probabilities)
{
  if(r_reachable()) {
    
    if(probabilityDefined[DIRECTION_FORWARD] && 
       probabilityDefined[DIRECTION_BACKWARD]) {
      return(probability[DIRECTION_FORWARD]+probability[DIRECTION_BACKWARD]);
    } else {
    // this should never happen; assuming probabilities are always set through
    // the TAGNODE function "setAllStatesTo()" before this function is called
      cerr << "TAGGER ERROR: FW or BW probability undefined in state:" << endl;
      print();
      tagger_close();
      exit(1);
    } 
  } else {
    return 1000000; 
  }
}

void STATENODE::print()
  // show forward and backward probabilities
{
  cout << " state:";
  for(vector<string>::iterator vi=history.begin() ; vi!=history.end() ; ++vi){
    cout << " " << *vi;
  }
  cout << " fw: " << probability[DIRECTION_FORWARD]
       << " bw: " << probability[DIRECTION_BACKWARD]  
       << " " << (r_reach ? "" : "not-ending") << endl;
}

void STATENODE::printHistory()
  // show history; for debugging
{
  for(vector<string>::iterator vi=history.begin() ; vi!=history.end() ; ++vi){
    cout << *vi << " ";
  }
}

//-----------------------------------------------------------------------------
// TRANSITION member functions
//-----------------------------------------------------------------------------

TRANSITION::TRANSITION(list<STATENODE>::iterator targetState,double transitionProb)
  :
  targetState(targetState),
  transitionProbability(transitionProb)
{}

TRANSITION::~TRANSITION()
{}

int legal_transition(string t1, string t2) 
{
  // return codes:
  // 0: not a legal transition
  // 1: legal non-mwu transition
  // 2: obliged mwu transition i/j-T --> i+1/j-T
  // NOTE: unexpected results are expected for tags which have the shape
  //  xxxx/xxx-xxxx but which are not mwu tags.

  int t1_begin=1;
  int t1_end=1;
  int t2_begin=1;
  int t2_end=1;
  string t1_tag(t1);
  string t2_tag(t2);

  string::size_type p,q;
  string tmp;

  p=t1.find("/",0);
  if (p != string::npos) {
    tmp=t1.substr(0,p);
    t1_begin=atoi(tmp.c_str());
    q=t1.find("-",p);
    if (q != string::npos) {
      tmp=t1.substr(p+1,q-p);
      t1_end=atoi(tmp.c_str());
      t1_tag=t1.substr(q+1,t1.length()-q);
    } else {
      t1_begin=1;
    }
  }

  p=t2.find("/",0);
  if (p != string::npos) {
    tmp=t2.substr(0,p);
    t2_begin=atoi(tmp.c_str());
    q=t2.find("-",p);
    if (q != string::npos) {
      tmp=t2.substr(p+1,q-p);
      t2_end=atoi(tmp.c_str());
      t2_tag=t2.substr(q+1,t2.length()-q);
    } else {
      t2_begin=1;
    }
  }

  int mwu_transition = ( t1_tag == t2_tag &&
			 t2_begin == t1_begin + 1 &&
			 t1_end == t2_end 
			 );


  if (t1_begin < t1_end || t2_begin > 1) {
    if(mwu_transition)
      return 2;
    else
      return 0;
  }

  return 1;
}

