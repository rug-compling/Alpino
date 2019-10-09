
// guesser.cc -- lexical analysis part of tagger

//###################################################################################################################

#include "tagger.h"

//###################################################################################################################

long int
  g_wordTagLexiconFaddNumber   = -1,
  g_guessDictAllFaddNumber     = -1,
  g_guessDictNoncapsFaddNumber = -1,
  g_guessDictCapsFaddNumber    = -1;

int
  g_useLexicon                 = 1,  // assign tags found for this word in lexicon
  g_useLexiconForUnknown       = 0,  // using tags assigned to UNKNOWN in lexicon
  g_assignAllTags              = 0,  // assign *all* tags present in lexicon
  g_useSetOfLexicons           = 1;  // use different lexicons for subsets of words

int
  g_currentUnknown             = 0,
  g_treated                    = 0,
  g_unknown                    = 0,
  g_unclassified               = 0,
  g_tagCount                   = 0,
  g_correct                    = 0,
  g_unknownTagCount            = 0,
  g_unknownCorrect             = 0,
  g_unknownUnclassified        = 0;

vector<string>
  g_allTags;

extern vector <string>
  g_heuristicNamesSingleTags,
  g_heuristicNamesMultiTags;

extern int
  g_usedTagSet,
  g_debug;

//###################################################################################################################

void guesser_initData(const string p_dataDir)
{
  // load data for heuristics and proper handling of multi-word units

  g_usedTagSet = TAGSET_ALPINO; // default

  // file "heuristics" should be in data directory; may be (partly) empty; file format is as follows:

  // <tagset>
  // either WOTAN or ALPINO
  // <multi-word name tags>
  // tags; each ended by newline 
  // <single-word name tags>
  // tags; each ended by newline

  // example, wotan:

  // <tagset>
  // WOTAN
  // <single-word name tags>
  // N(eigen,ev,neut)
  // N(eigen,mv,neut)
  // N(eigen,ev,gen)
  // <multi-word name tags>
  // N(eigen,ev,neut)
  // N(eigen,mv,neut)
  // N(eigen,ev,gen)

  // example, alpino:

  // <tagset>
  // ALPINO
  // <single-word name tags>
  // proper_name(both)
  // proper_name(pl)
  // proper_name(sg)
  // <multi-word name tags>
  // proper_name(both)
  // proper_name(pl)
  // proper_name(sg)

  string heuristicsFile = p_dataDir + HEURISTICSFILE;
  ifstream HEURISTICS;
  HEURISTICS.open(heuristicsFile.c_str());
  if(!HEURISTICS){
    cerr << "GUESSER ERROR: could not open file holding heuristics info " << heuristicsFile.c_str() << endl;
    exit(1);
  }

  string line;
  int mode = 0;
  getline(HEURISTICS,line);
  while(!HEURISTICS.eof()){
    if(line=="<tagset>"){
      mode = 2;
    }else if(line=="<multi-word name tags>"){
      mode = 1;
    }else if(line=="<single-word name tags>"){
      mode = 0;
    }else{
      if(mode==0){
	g_heuristicNamesSingleTags.push_back(line);
      }else if(mode==1){
	g_heuristicNamesMultiTags.push_back(line);
      }else if(mode==2){
	if(line=="WOTAN"){
	  g_usedTagSet = TAGSET_WOTAN;
	}else if(line=="ALPINO"){
	  g_usedTagSet = TAGSET_ALPINO;
	}
      }
    }
    getline(HEURISTICS,line); 
  }
  HEURISTICS.close();

  // initialize fadd library
  int lib_key;

  if ((lib_key = fadd_init_lib(40)) == -1L){
    cerr << "GUESSER ERROR: could not initialize fadd library; fadd error code " << fadd_get_errno() << endl;
    exit(1);
  }

  // initialize guessing dictionaries
  g_guessDictAllFaddNumber     = init_guess((p_dataDir + FSAGUESS_DICT_ALL).c_str(),0,0,1,0);
  g_guessDictCapsFaddNumber    = init_guess((p_dataDir + FSAGUESS_DICT_CAPS).c_str(),0,0,1,0);
  g_guessDictNoncapsFaddNumber = init_guess((p_dataDir + FSAGUESS_DICT_NONCAPS).c_str(),0,0,1,0);

  if (g_guessDictAllFaddNumber == -1L){
    cerr << "GUESSER ERROR: could not intialize guessing dictionary; fadd error code " << fadd_get_errno() << endl;
    exit(1);
  }
  if (g_guessDictCapsFaddNumber == -1L){
    cerr << "GUESSER ERROR: could not intialize guessing dictionary; fadd error code " << fadd_get_errno() << endl;
    exit(1);
  }
  if (g_guessDictNoncapsFaddNumber == -1L){
    cerr << "GUESSER ERROR: could not intialize guessing dictionary; fadd error code " << fadd_get_errno() << endl;
    exit(1);
  }

  // note: check if values currently used for parameters (0,0,0,0) are correct

  // initialize lexicon
  if(g_useLexicon){
    string wordTagLexiconDictFile = p_dataDir + WORDTAGLEXICONDICTFILE;
    g_wordTagLexiconFaddNumber = init_morph(wordTagLexiconDictFile.c_str(),0,0,0,0);
    if(g_wordTagLexiconFaddNumber == -1){
      cerr << "GUESSER ERROR: could not load word->tag lexicon dictionary " << wordTagLexiconDictFile << endl;
      guesser_closeData();
      exit(1);
    }
  }

  if(g_assignAllTags){
    string allTagsFile = p_dataDir + "tags";
    ifstream ALL_TAGS;
    ALL_TAGS.open(allTagsFile.c_str());
    if(!ALL_TAGS){
      cerr << "GUESSER ERROR: could not open file holding all tags " << allTagsFile.c_str() << endl;
      guesser_closeData();
      exit(1);
    }
    string tag;
    getline(ALL_TAGS,tag);
    while(!ALL_TAGS.eof()){
      g_allTags.push_back(tag);
      getline(ALL_TAGS,tag); 
    }
    ALL_TAGS.close();
  }
}

void guesser_closeData()
{
  if(g_useLexicon){
    close_dict(g_wordTagLexiconFaddNumber);
  }
}

//###################################################################################################################

void guesser_getTagsFromLexicon(const string &word, vector<string> &tags)
{
  list_of_words *tagList;
  
  // get tags stored in lexicon for this word
  tagList = morph_word(word.c_str(),g_wordTagLexiconFaddNumber);

  if(tagList==NULL){
    g_unknown++;
    g_currentUnknown = 1;
  }else{
    g_currentUnknown = 0;
  }

  // use data for UNKNOWN_WORD in lexicon
  if((tagList==NULL) && g_useLexiconForUnknown){
    tagList = morph_word(UNKNOWN_WORD.c_str(),g_wordTagLexiconFaddNumber);
  }
  
  list_of_words *temp = tagList;
  
  // add all tags in tagList (if any) to this word's vector
  while(temp!=NULL){
    string wordPlusTag = temp->word;
    string::size_type div = wordPlusTag.find('+',0);
    string tagOnly(wordPlusTag,div+1,wordPlusTag.length()-(div+1));
    tags.push_back(tagOnly);
    temp = temp->next;
  }

  destroyWordList(tagList);
}

void guesser_getTagsFromSuffixDictionary(string &word, vector<string> &tags)
{
  list_of_words *tagList;

  // use appropriate dictionary
  long int guessDict;
  if(!g_useSetOfLexicons){
    guessDict = g_guessDictAllFaddNumber;
  }else if(isupper(word[0])){
    guessDict = g_guessDictCapsFaddNumber;
  }else{
    guessDict = g_guessDictNoncapsFaddNumber;
  }

  // get tags
  if((tagList = guess_word(word.c_str(), guessDict)) != NULL){
    list_of_words *temp = tagList;
    while(temp!=NULL){
      string wordPlusTag = temp->word;
      string::size_type div = wordPlusTag.find('+',0);
      string tagOnly(wordPlusTag,div+1,wordPlusTag.length()-(div+1));
      tags.push_back(tagOnly);
      temp = temp->next;
    }
  }

  destroyWordList(tagList);
}

void guesser_getTagsFromSuffixDictionaryOldVersion(string &word, vector<string> &tags,const string p_dataDir)
{
  string commandLine("");

  // create command for fsa_guess
  commandLine+="echo \"";
  commandLine+=addSlashes(word,'"');
  commandLine+="\" | ";
  commandLine+=FSAGUESS;
  commandLine+=p_dataDir;

  // use appropriate dictionary
  if(!g_useSetOfLexicons){
    commandLine+=FSAGUESS_DICT_ALL;
  }else if(isupper(word[0])){
    commandLine+=FSAGUESS_DICT_CAPS;
  }else{
    commandLine+=FSAGUESS_DICT_NONCAPS;
  }
  
  FILE *proc = popen(commandLine.c_str(), "r");
  
  if(proc == NULL){
    cerr << "error opening file stream" << endl;
    exit(1);
  }

  string line;

  int
    seenWord = 0,
    reading  = 0,
    c;

  // read fsa_guess output
  while((c = fgetc(proc)) && (c != EOF)){
    if(reading){
      line+=c;
    }else{
      if(seenWord){
	reading = 1;
      }else if(c == ':'){
	seenWord = 1;
      }
    }
  }

  pclose(proc);

  // remove newline
  chomp(line);

  // get tags, if any were returned
  if(line!=FSAGUESS_FAILURE){ 
    
    string::size_type
      beginPos   = 0,
      endPos,
      tagsLength = line.size(),
      length;

    // get tags from line
    while(beginPos<tagsLength){
      endPos = line.find(' ',beginPos);
      if(endPos==string::npos){
	endPos = tagsLength+2;
      }
      length = endPos - beginPos - 1;
      string tag(line,beginPos,length);
      tags.push_back(tag);
      beginPos = endPos+1;
    }
  }
}

//###################################################################################################################

void chomp(string &line)
{
  if(line[line.size()-1] == '\n'){
    line.erase(line.size()-1,1);
  }
}

string addSlashes(string word, char character)
{
  string::size_type pos = word.find(character);
  while(pos!=string::npos){
    word = word.insert(pos,"\\");
    pos  = word.find(character,pos+2);
  }
  return(word);
}

