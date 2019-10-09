//---------------------------------------------------------------------------------------------
// main.cc -- stand-alone tagger/filter evaluation code
//---------------------------------------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <getopt.h>

#include "tagger.h"

//---------------------------------------------------------------------------------------------
// globals
//---------------------------------------------------------------------------------------------

// counting proportion of unknown words
extern int
  g_unknownWords,
  g_knownWords;

// used in evaluation
int 
  g_wordsBefore   = 0,
  g_tagsBefore    = 0,
  g_wordsAfter    = 0,
  g_tagsAfter     = 0,
  g_difference    = 0,
  g_totalTagCount = 0;

// command line options
const char *optstring = "s:t:p:m:f:n:d:l:a:b:C:?";

//---------------------------------------------------------------------------------------------
// usage function
//---------------------------------------------------------------------------------------------

void usage()
{
  cout << endl;
  cout << "Usage: tagger [options]" << endl << endl;
  cout << "options:" << endl << endl; 
  cout << "-? --help                      show options" << endl << endl;
  cout << "-s --model           <integer> n-gram tag model: 1=unigram, 2=bigram, 3=trigram; default is 3" << endl; 
  cout << "-t --threshold       <integer> threshold value used in deciding which tags to remove; default is 0" << endl; 
  cout << "-p --percentage      <integer> percentage of tags (other than best) that should remain; default is 0" << endl; 
  cout << "-m --minprob         <float>   every tags with lower log-prob will be deleted" << endl; 
  cout << "-f --firstsent       <integer> start tagging at the f-th sentence; default is 1" << endl; 
  cout << "-n --nrsents         <integer> number of sentences to process; default is -1 (all)" << endl; 
  cout << "-C --diversity       <float>   diversity constant; default is 14" << endl; 
  cout << "-d --debug           <0|1|2>   show debug messages; default is 0" << endl << endl;
  cout << "-l --lexicalanalysis <0|1>     use lexical analysis component other than Alpino's; default is 0" << endl;
  cout << "-a --standalone      <0|1>     interactive stand-alone mode; default is 0" << endl;
  cout << "-b --baseline        <0|1>     use baseline method: most frequent tag for given word; default is 0" << endl;
  cout << "   --starttag        <string>  start tag; default is <xxx_sentence_start>" << endl; 
  cout << "   --endtag          <string>  end tag; default is <xxx_sentence_end>" << endl;
  cout << "   --fadddatadir     <string>  directory containing fadd data structures" << endl;  
  cout << "   --lexanalysisfile <string>  file containing lexical analysis results for testing" << endl;
  cout << "   --framesfile      <string>  file containing correct frames for testing" << endl;
  
  exit(1);
}

//---------------------------------------------------------------------------------------------
// other functions
//---------------------------------------------------------------------------------------------

void getWordsFromSentence(string sentence, vector<string> &words, int p_debug)
{      
  int
    end,
    length;
  
  unsigned pos = 0;

//   if(p_debug>2) cout << "Reading words in test sentence... ";
      
  while(pos < sentence.length()) {
    end = sentence.find(' ',pos);
    if(end == (int) string::npos){
      end = sentence.length();
    }
    length = end - pos; 
    words.push_back(string(sentence,pos,length));
    pos = end+1;
  }
  
  //  if(p_debug>2) cout << "done." << endl;
}

//---------------------------------------------------------------------------------------------
// main function
//---------------------------------------------------------------------------------------------

int main(int argc, char **argv) 
{
  int 
    model             = 3,  // 1=unigram, 2=bigram, 3=trigram model
    threshold         = 0,  // threshold "tau" used in filtering out bad tags
    percentage        = 0,  // percentage of remaining tags to aim at
    nrSents           = -1, // number of sentences to apply filter to in test (-1 = all)
    firstSent         = 1,  // first sentence to start testing
    debug             = 0,  // show debug messages
    lexicalAnalysis   = 0,  // use own lexicon/heuristics for guessing possible tags
    standAlone        = 1,  // get sentence from standard input, print tagging to output
    baseline          = 0;  // assign most frequent tag to given word
    
  double
    diversityC        = 3,  // diversity factor 'C' used in linear interpolation
    minprob           = 2.0;

  string 
    startTag          = "",
    endTag            = "",
    dataDirectory     = "./",
    faddDataDirectory,
    lexAnalysisFile   = dataDirectory+"la.testing",     // possible taggings
    framesFile        = dataDirectory+"frames.testing"; // correct taggings
  
  // getopt options array
  const option optionDef[] =
  {
    {"firstsent",1,0,'f'},
    {"statesize",1,0,'s'},
    {"threshold",1,0,'t'},
    {"percentage",1,0,'p'},
    {"minprob",1,0,'m'},
    {"nrsents",1,0,'n'},
    {"starttag",1,0,0},
    {"endtag",1,0,0},
    {"debug",1,0,'d'},
    {"standalone",1,0,'a'},
    {"baseline",1,0,'b'},
    {"lexicalAnalysis",1,0,'l'},
    {"fadddatadir",1,0,0},
    {"lexanalysisfile",1,0,0},
    {"framesfile",1,0,0},
    {"diversity",1,0,'C'},
    {"help",0,0,'?'},
    {0,0,0,0}
  };

  char *alpinohome;
  alpinohome = getenv("ALPINO_HOME");
  if (alpinohome!=NULL) 
    faddDataDirectory = string(alpinohome) + "/PosTagger/MODELS";
  else {
    faddDataDirectory = "./MODELS";
    cerr << "Warning: ALPINO_HOME not set." << endl;
  }
  
  // get options
  while(1){
    int optIndex = 0;
    int c = getopt_long(argc,argv,optstring,optionDef,&optIndex);
    if(c == -1){
      break;
    }

    string longName;

    switch (c)
      {
      case 0:
	// long-only options; use optIndex to get at long name
	longName = optionDef[optIndex].name;
	if(longName == "starttag"){
	  startTag = optarg;
	}else if(longName == "endtag"){
	  endTag = optarg;
	}else if(longName == "fadddatadir"){
	  faddDataDirectory = optarg;
	}else if(longName == "lexanalysisfile"){
	  lexAnalysisFile = optarg;
	}else if(longName == "framesfile"){
	  framesFile = optarg;
	}
	break;
      case 'f':
	firstSent = atoi(optarg);
	break;
      case 's':
	model = atoi(optarg);
	break;
      case 't':
	threshold = atoi(optarg);
	break;
      case 'p':
	percentage = atoi(optarg);
	break;
      case 'm':
	minprob = atof(optarg);
	break;
      case 'n':
	nrSents = atoi(optarg);
	break;
      case 'd':
	debug = atoi(optarg);
	break;
      case 'a':
	standAlone = atoi(optarg);
	break;
      case 'b':
	baseline = atoi(optarg);
	break;
      case 'l':
	lexicalAnalysis = atoi(optarg);
	break;
      case 'C':
	diversityC = atof(optarg);
	break;
      case '?':
	usage();
	break;
      }
  }

  // if standalone, always use own lexical analysis component 
  if(standAlone){
    lexicalAnalysis = 1;
  }
    
  // initialize tagger/filter
  tagger_init(model,threshold,percentage,minprob,debug,baseline,lexicalAnalysis,diversityC,startTag,endTag,faddDataDirectory+"/");

  ifstream 
    LEX_ANALYSIS,
    CORRECT_TAGGINGS;

  // lexical analysis file and correct taggings not needed in standalone mode
  if(!standAlone){
    // open lexical analysis file; this should be opened even if lexicon work
    // is done without Alpino's help, since in the current setup the la file also 
    // contains the sentences we want to tag
    LEX_ANALYSIS.open(lexAnalysisFile.c_str());
    if(!LEX_ANALYSIS){
      cout << "TAGGER ERROR (main.cc): error opening lexical analysis file " << lexAnalysisFile.c_str() << endl;
      tagger_close();
      exit(1); 
    } 
    // open file holding correct tagging
    CORRECT_TAGGINGS.open(framesFile.c_str());
    if(!CORRECT_TAGGINGS){
      cout << "TAGGER ERROR (main.cc): error opening file holding correct tagging " << framesFile.c_str() << endl;
      tagger_close();
      exit(1);
    }
  }


  // RUN FILTER ON TEST DATA

  
  if(standAlone){

    while(!cin.eof()){
      vector<string> words;
      string sentence;
      getline(cin,sentence);
      getWordsFromSentence(sentence,words,debug);
      vvTAGNODE tags(words.size()+2);
      tagger_filter_ask(words,tags,debug);
    }

  }else{

    string 
      sentenceLine = "",
      correctLine  = "";
    
    // get first sentence line
    getline(LEX_ANALYSIS,sentenceLine);
    // get first correct frame line
    getline(CORRECT_TAGGINGS,correctLine);
    
    int sentenceCount = 0;  
             
    cout << "Processing sentences... " << endl;
    if(debug>2) cout << endl;
    
    while((!(LEX_ANALYSIS.eof())) && (nrSents != 0)){
      
      sentenceCount++;
      nrSents--;
            

      // GET WORDS

      
      unsigned div = sentenceLine.find('|',0);
      
      string sentence(sentenceLine,div+1,sentenceLine.length()-(div+1));
      vector<string> words;
      getWordsFromSentence(sentence,words,debug);


      // GET LEXICAL ANALYSIS

      
      vvTAGNODE                 // create a trellis of TAGNODES that
	tags(words.size()+2);   // is as wide as the number of words + 2
                                // for dummy nodes at beginning and end
      
      string line;
      
//       if(debug>2) cout << "Reading lexical analysis of test sentence... ";
      
      int
	nextSentence = 0,
	tagCount     = 0,
	lastPos      = words.size(); // last tag should end here
      
      while((!nextSentence) && (getline(LEX_ANALYSIS,line))){
	
	string type(line,0,3);
	
	// if using own lexical analysis component, do not add anything 
	// to tags vector, but skip till next sentence is reached
	
	if(type == "TAG"){
	  
	  if(!lexicalAnalysis){
	    
	    int start, end, length;
	    
	    // get tag begin
	    start  = line.find('#')+1;
	    end    = line.find('|');
	    length = end - start;
	    string tagStartPos(line,start,length);
	    
	    // get tag end
	    start  = end+1;
	    end    = line.find('|',start);
	    length = end - start;
	    string tagEndPos(line,start,length);
	    
	    // only continue if end is not beyond end of vector
	    if(atoi(tagEndPos.c_str()) > lastPos){

	      cerr << "TAGGER WARNING (main.cc): moving past end of trellis in lexical analysis; tag not included" << endl;

	    }else{
	      
	      // skip word
	      end = line.find('|',end+1);
	      
	      // get tag name
	      start  = end+1;
	      length = line.size() - start;
	      string tagName(line,start,length);
	      
	      int tagSpan = atoi(tagEndPos.c_str()) - atoi(tagStartPos.c_str());
	      
	      TAGNODE tagNode(tagName,words,atoi(tagStartPos.c_str()),tagSpan,DEFAULT_TAG_ID);
	      
	      // assuming tagStartPos counting starts at 0
	      tags[atoi(tagStartPos.c_str())+1].push_back(tagNode);
	      
	      tagCount++;
	    }
	  }
	}else{
	  // type == "KEY", next sentence
	  sentenceLine = line;
	  nextSentence = 1;
	}
      }
      
//       if(debug>2) cout << "done." << endl;
      

      // GET *CORRECT* TAGGING

      
      vector<pair<string,pair<int,int> > > correctTagging;         
      
	vector<string>::size_type correctTaggingCount = 0;
	bool encounteredNextSentence = false;
      
      string previousKey = "";
      
      while((correctLine != "") && (!encounteredNextSentence)){
	
	unsigned
	  pos1   = correctLine.find('|',0),
	  pos2   = correctLine.find('|',pos1+1),
	  pos3   = correctLine.find('|',pos2+1),
	  pos4   = correctLine.find('|',pos3+1),
	  length = pos3-(pos2+1);
	
	string key(correctLine,pos2+1,length);
	
	if((key == previousKey) || (previousKey == "")){
	  
	  // word|tag|key|b|e
	  // pos 1   2   3 4
	  
	  string
	    tagName(correctLine,pos1+1,pos2-(pos1+1)),
	    startPos(correctLine,pos3+1,pos4-(pos3+1)),
	    endPos(correctLine,pos4+1,correctLine.size()-(pos4+1));
	  
	  pair<int,int> tagPositions(atoi(startPos.c_str()),atoi(endPos.c_str()));
	  
	  pair<string, pair<int, int> > tag(tagName,tagPositions);
	  
	  correctTagging.push_back(tag);
	  correctTaggingCount++;
	  
	  // read next line
	  getline(CORRECT_TAGGINGS,correctLine);
	}else{
	  encounteredNextSentence = true;
	}
	
	previousKey = key;
      }
      
      if(words.size() != correctTaggingCount){
	cerr << "TAGGER ERROR (main.cc): length of sentence " << sentenceCount 
             << " does not match length of correct tagging"   << endl;
	tagger_close();
	exit(1);
      }
      
      
      // APPLY POSFILTER
      
      
      if(firstSent==0){
	firstSent=1;
      }
      
      if(sentenceCount >= firstSent){
	
	if(debug) cout << "Sentence " << sentenceCount << " [" << sentence << "]" << endl;
      	
	vector<double> results(5);
	
	g_totalTagCount += correctTaggingCount;
	
	tagger_filter_evaluation(words,tags,tagCount,correctTagging,results,debug);
	
	g_wordsBefore += int(results[0]);
	g_tagsBefore  += int(results[1]);
	g_wordsAfter  += int(results[2]);
	g_tagsAfter   += int(results[3]);
	g_difference  += int(results[4]);	

	if(debug) cout << "stat|" << sentenceCount << "|" << results[0] << "|" << results[4] << "|" <<
		    100.00 - (100.0 * results[4])/results[0] << endl;

      }    
    }
    
    cout << "done." << endl;


    // SHOW RESULTS

    
    if(!baseline){
      cout << "> model used        : " << model      << endl;
    }else{
      cout << "> model used        : " << "baseline" << endl;
    }

    if(!baseline){
      // percentage of tags, or threshold
	cout << "> target % remaining: " << percentage << endl;
	cout << "> threshold         : " << threshold << endl;
	cout << "> minprob           : " << minprob << endl;
    }

    float
      ambiguityAfter  = (double)g_tagsAfter/(double)g_wordsAfter,
      ambiguityBefore = (double)g_tagsBefore/(double)g_wordsBefore;

    if(!baseline){
      cout << "> diversity factor  : " << diversityC << endl;
    }

    cout << "> sentence count    : " << sentenceCount-firstSent+1                            << endl
	 << "> tags before       : " << g_tagsBefore                                         << endl
	 << "> tags after        : " << g_tagsAfter                                          << endl
	 << "> ambiguity before  : " << ambiguityBefore                                      << endl
	 << "> ambiguity after   : " << ambiguityAfter                                       << endl
         << "> remaining ambig.% : " << (ambiguityAfter-1.0)/((ambiguityBefore-1.0)/100.0)   << endl
	 << "> word accuracy%    : " << 100*(1-(double)g_difference/(double)g_totalTagCount) << endl;

    if(lexicalAnalysis){
	 cout << "> unknown words%    : " << 100*(double)g_unknownWords/(double)(g_knownWords+g_unknownWords) << endl;
    }

    cout << "#S	" << model << "	" << percentage << "	" << threshold << "	" << minprob << "	"
         << sentenceCount-firstSent+1 << "	" << g_tagsBefore << "	" << g_tagsAfter << "	"
         << ambiguityBefore << "	" << ambiguityAfter << "	"
         <<  (ambiguityAfter-1.0)/((ambiguityBefore-1.0)/100.0) << "	"
         << 100*(1-(double)g_difference/(double)g_totalTagCount) << endl;
    
    // close files
    LEX_ANALYSIS.close();  
    CORRECT_TAGGINGS.close();    
  }
  
  // close tagger
  tagger_close();
  
  return(0);
}


