#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cstdarg>
#include <cerrno>
#include <cmath>
#include <iostream>
#include <unordered_map>
#include <string>
#include <unistd.h>

using namespace std;

#define BUFSIZE 10000

char buffer[BUFSIZE + 1];

int arg_c;

char **arg_v;
char *programname;

long int
    opt_c = 0,
    opt_f = 0,
    opt_g = 0;

char
    *opt_d = (char *)".";

string SENTENCE_START("xxx_sentence_start");
string SENTENCE_END("xxx_sentence_end");
string DUMMY_CONTEXT("<DUMMY_CONTEXT>");
string DUMMY_WORD("<DUMMY_WORD>");
string UNKNOWN_WORD("<UNKNOWN_WORD>");

long int THRESHOLD;                // = opt_f, used for computing word-tag freq
//#$UNKNOWN_WORD_TAGPERCENTAGE = 0;  # percentage of most frequent tags to be used for unknown words

string firstTag(SENTENCE_START);
string secondTag(SENTENCE_START);
string oldContext(DUMMY_CONTEXT);

string previousKey("<NONE>");

unordered_map<string, long int>
    wordFreq,
    wordTagFreq,
    contextTrigramFreq,
    usedContext,
    tagFourgramFreq,
    tagTrigramFreq,
    tagBigramFreq,
    tagUnigramFreq,
    prefixTrigramFreq,
    prefixBigramFreq,
    prefixUnigramFreq,
    prefixTrigramDiv,
    prefixBigramDiv,
    lexicon;

string previousMultiTag("<NONE>");
long int previousMultiNumber = 0;
string multiTagFirstWord("");
string multiTagRestWords("");

void
    get_programname (char const *argv0),
    process_args (),
    errit (char const *format, ...),
    syntax ();
char
    *get_arg ();

void addToLexicon(string &word, string &tag, string &words)
{
    string entry;
    if (words.size() == 0)
	entry = word  + "|" +  tag;
    else
	entry = word  + "|" +  tag + "^" + words;
    lexicon[entry]++;
}

FILE *create(char *filename)
{
    FILE *fp;
    strcpy (buffer, opt_d);
    strcat (buffer, filename);
    fp = fopen(buffer, "w");
    if (! fp)
	errit ("Creating file \"%s\": %s", buffer, strerror (errno));
    return fp;
}

int main(int argc, char *argv[])
{
    FILE
	*fp;

    long int
	i,
	i1,
	i2,
	n,
	lineno;

    string nullstring("");
    string word, thirdTag, sentenceKey, newContext, baseTag;
    long int number;

    get_programname (argv [0]);
    arg_c = argc;
    arg_v = argv;
    process_args ();

    if (arg_c == 1 && !isatty (fileno (stdin)))
	fp = stdin;
    else if (arg_c == 2) {
	fp = fopen(arg_v [1], "r");
	if (! fp)
            errit ("Reading file \"%s\": %s", arg_v [1], strerror (errno));
    } else
        syntax ();

    lineno = 0;
    while (fgets (buffer, BUFSIZE, fp) != NULL) {

	i = strlen (buffer);
        while (i > 0 && isspace ((unsigned char) buffer [i - 1]))
            buffer [--i] = '\0';

	n = 0;
	i = -1;
	i1 = 0;
	i2 = 1;
	while (i2) {
	    i++;
	    if (buffer [i] == '|' || buffer[i] == '\0') {
		if (buffer[i] == '\0')
		    i2 = 0;
		else
		    buffer [i] = '\0';
		switch (n) {
		    // word|thirdTag|sentenceKey|undef|undef|undef|newContext
		    //  0      1          2        3     4     5      6
		    case 0:
		        word = buffer + i1;
		        break;
		    case 1:
		        thirdTag = buffer + i1;
		        break;
		    case 2:
		        sentenceKey = buffer + i1;
		        break;
		    case 6:
		        newContext = buffer + i1;
		        break;
		}
		n++;
		i1 = i + 1;
	    }
	}
	if (n != 7)
	    continue;

	lineno++;
	if  (lineno % 1000000 == 0)
	    cerr << "read " << lineno / 1000000 << " million lines" << endl;

	// als we een multitag zien en het getal is 1, dan wordt de tag
	// opgeslagen in previousMultiTag, en het getal 1 in
	// previousMultiNumber, en het woord in $multiTagFirstWord.  als het
	// getal hoger is dan 1 en volgt op previousMultiNumber en de tag
	// komt overeen met previousMultiTag, dan wordt het betreffende woord
	// toegevoegd aan multiTagRestWords.  als we een gewone tag zien en
	// previousMultiTag is niet <NONE>, dan worden de opgeslagen gegevens
	// verwerkt: multiTagFirstWord en previousMultiTag worden opgeslagen
	// in het lexicon, waarbij de tag wordt uitgebreid met de informatie
	// dat multiTagRestWords vereist zijn.

	// recognize multiword tag
	i = 0;
	while (thirdTag [i] && isdigit ((unsigned char) thirdTag [i]))
	    i++;
	if (thirdTag [i] && i > 0 && (thirdTag [i] == '-' || thirdTag [i] == '/')) {

		// reconstruct thirdTag

		number  = atoi(thirdTag.substr(0, i).c_str());
		baseTag = thirdTag.substr(i + 1);

		// if tag was converted from WOTAN format, multi-word tag will have an extra number
		i = 0;
		while (i < (long int) baseTag.size() && isdigit ((unsigned char) baseTag [i]))
		    i++;
		if (i > 0 && baseTag [i] == '-')
		    baseTag = baseTag.substr(i + 1);

		if (number == 1) {
		    if (previousMultiTag != "<NONE>") {
			addToLexicon(multiTagFirstWord, previousMultiTag, multiTagRestWords);
		    }
		    previousMultiTag    = baseTag;
		    previousMultiNumber = number;
		    multiTagFirstWord   = word;
		    multiTagRestWords = "";
		} else if ((number == previousMultiNumber+1) && (baseTag == previousMultiTag)) {
		    previousMultiNumber = number;
		    if (multiTagRestWords != "") {
			multiTagRestWords += ",";
		    }
		    multiTagRestWords += word;
		}
	} else {
	    if (previousMultiTag != "<NONE>") {
		addToLexicon(multiTagFirstWord, previousMultiTag, multiTagRestWords);
		previousMultiTag = "<NONE>";
	    }
	    addToLexicon(word, thirdTag, nullstring);
	}

	// not using context implemented by using dummy everywhere
	if (opt_c == 0) {
	    newContext = DUMMY_CONTEXT;
	}

	// if new sentence, then
	if (sentenceKey != previousKey) {
	    if (previousKey != "<NONE>"){
		// take care of ending of previous sentence (with sentenceEnd for third tag)
		string bigram   = oldContext + "|" + SENTENCE_END;
		string trigram  = secondTag + "|" + bigram;
		string fourgram = firstTag + "|" + trigram;

		string prefixUnigram = oldContext;
		string prefixBigram  = secondTag + "|" + prefixUnigram;
		string prefixTrigram = firstTag + "|" + prefixBigram;

		// diversity
		if (tagFourgramFreq.count(fourgram) == 0) {
		    prefixTrigramDiv[prefixTrigram]++;
		}
		if (tagTrigramFreq.count(trigram) == 0){
		    prefixBigramDiv[prefixBigram]++;
		}

		// store tag n-grams
		tagBigramFreq[bigram]++;
		tagTrigramFreq[trigram]++;
		tagFourgramFreq[fourgram]++;

		// store prefix n-grams
		prefixUnigramFreq[prefixUnigram]++;
		prefixBigramFreq[prefixBigram]++;
		prefixTrigramFreq[prefixTrigram]++;

		// store context n-gram
		string contextTrigram = oldContext + "|" + SENTENCE_END + "|" + DUMMY_CONTEXT;
		contextTrigramFreq[contextTrigram]++;

		// replace tags by default start-of-sentence tags
		firstTag    = SENTENCE_START;
		secondTag   = SENTENCE_START;
		oldContext  = DUMMY_CONTEXT;

	    }
	    previousKey = sentenceKey;
	}

	string bigram   = oldContext + "|" + thirdTag;
	string trigram  = secondTag + "|" + bigram;
	string fourgram = firstTag + "|" + trigram;

	string prefixUnigram = oldContext;
	string prefixBigram  = secondTag + "|" + prefixUnigram;
	string prefixTrigram = firstTag + "|" + prefixBigram;

	// diversity
	if (tagFourgramFreq.count(fourgram) == 0) {
	    prefixTrigramDiv[prefixTrigram]++;
	}
	if (tagTrigramFreq.count(trigram) == 0) {
	    prefixBigramDiv[prefixBigram]++;
	}

	// store tag n-grams
	tagUnigramFreq[thirdTag]++;
	tagBigramFreq[bigram]++;
	tagTrigramFreq[trigram]++;
	tagFourgramFreq[fourgram]++;

	// store prefix n-grams
	prefixUnigramFreq[prefixUnigram]++;
	prefixBigramFreq[prefixBigram]++;
	prefixTrigramFreq[prefixTrigram]++;

	// store context n-gram
	string contextTrigram = oldContext + "|" + thirdTag + "|" + newContext;
	contextTrigramFreq[contextTrigram]++;

	// keep track of encountered contexts
	usedContext[newContext] = 1;

	// store word-tag combination
	string wordTag = word + "|" + thirdTag;
	wordTagFreq[wordTag]++;
	wordFreq[word]++;

	// set previous to current
	firstTag    = secondTag;
	secondTag   = thirdTag;
	oldContext  = newContext;

    }
    cerr << "read all " << lineno << " lines" << endl;

    // ########################################################################
    // # take care of end of last sentence in file
    // ########################################################################

    string bigram   = oldContext + "|" + SENTENCE_END;
    string trigram  = secondTag + "|" + bigram;
    string fourgram = firstTag + "|" + trigram;

    string prefixUnigram = oldContext;
    string prefixBigram  = secondTag + "|" + prefixUnigram;
    string prefixTrigram = firstTag + "|" + prefixBigram;

    if (tagFourgramFreq.count(fourgram) == 0) {
	prefixTrigramDiv[prefixTrigram]++;
    }
    if (tagTrigramFreq.count(trigram) == 0) {
	prefixBigramDiv[prefixBigram]++;
    }

    tagBigramFreq[bigram]++;
    tagTrigramFreq[trigram]++;
    tagFourgramFreq[fourgram]++;

    prefixUnigramFreq[prefixUnigram]++;
    prefixBigramFreq[prefixBigram]++;
    prefixTrigramFreq[prefixTrigram]++;

    if(previousMultiTag != "<NONE>") {
	addToLexicon(multiTagFirstWord, previousMultiTag, multiTagRestWords);
    }

    // ########################################################################
    // # compute probabilities and diversity, print to appropriate files
    // ########################################################################

    // open files
    FILE *TAG_4_FREQ = create((char *) "/tag4");
    FILE *TAG_3_FREQ = create((char *)"/tag3");
    FILE *TAG_2_FREQ = create((char *)"/tag2");
    FILE *PRE_3_FDIV = create((char *)"/prefix3");
    FILE *PRE_2_FDIV = create((char *)"/prefix2");
    FILE *CONTEXT_3_FREQ = create((char *)"/context3");
    FILE *WORD_TAG_FREQ = create((char *)"/wordTag");
    FILE *TAG_WORD_FREQ = create((char *)"/tagWord");
    FILE *USED_CONTEXT = create((char *)"/usedContext");
    FILE *WORD_TAG_LEX = create((char *)"/wordTagLex");

    THRESHOLD = opt_f;

    // 4-gram tag context data
    for (unordered_map<string, long int>::iterator it = tagFourgramFreq.begin(); it != tagFourgramFreq.end(); it++) {
	string fourgram = (*it).first;
	size_t p = fourgram.rfind('|');
	string trigram = fourgram.substr(0, p);
	if ((*it).second > THRESHOLD) {
	    double probability = ((double) (*it).second) / (double) prefixTrigramFreq[trigram];
	    long int probabilityInt = (long int) (-100.0 * log(probability));
	    if (opt_g != 0 && probabilityInt < opt_g) {
		fprintf(TAG_4_FREQ, "%s|%li\n", fourgram.c_str(), probabilityInt);
	    }
	}
    }

    // 3-gram tag context data
    for (unordered_map<string, long int>::iterator it = tagTrigramFreq.begin(); it != tagTrigramFreq.end(); it++) {
	string trigram = (*it).first;
	size_t p = trigram.rfind('|');
	string bigram = trigram.substr(0, p);
	if ((*it).second > THRESHOLD) {
	    double probability =  ((double) (*it).second) / (double) prefixBigramFreq[bigram];
	    long int probabilityInt = (long int) (-100.0 * log(probability));
	    if (opt_g != 0 && probabilityInt < opt_g) {
		fprintf(TAG_3_FREQ, "%s|%li\n", trigram.c_str(), probabilityInt);
	    }
	}
    }

    //  2-gram tag context data
    for (unordered_map<string, long int>::iterator it = tagBigramFreq.begin(); it != tagBigramFreq.end(); it++) {
	string bigram = (*it).first;
	size_t p = bigram.rfind('|');
	string unigram = bigram.substr(0, p);
	if ((*it).second > THRESHOLD) {
	    double probability = ((double) (*it).second) / (double) prefixUnigramFreq[unigram];
	    long int probabilityInt = (long int) (-100.0 * log(probability));
	    if (opt_g != 0 && probabilityInt < opt_g) {
		fprintf(TAG_2_FREQ, "%s|%li\n", bigram.c_str(), probabilityInt);
	    }
	}
    }

    // 3-gram tag context prefix data
    for (unordered_map<string, long int>::iterator it = prefixTrigramFreq.begin(); it != prefixTrigramFreq.end(); it++) {
	if((*it).second > THRESHOLD) {
	    long int frequency = (*it).second;
	    long int diversity = prefixTrigramDiv[(*it).first];
	    fprintf(PRE_3_FDIV, "%s|%li|%li\n", (*it).first.c_str(), frequency, diversity);
	}
    }

    // 2-gram tag context prefix data
    for (unordered_map<string, long int>::iterator it = prefixBigramFreq.begin(); it != prefixBigramFreq.end(); it++) {
	if ((*it).second > THRESHOLD) {
	    long int frequency = (*it).second;
	    long int diversity = prefixBigramDiv[(*it).first];
	    fprintf(PRE_2_FDIV, "%s|%li|%li\n", (*it).first.c_str(), frequency, diversity);
	}
    }

    // 3-gram context tag data
    for (unordered_map<string, long int>::iterator it = contextTrigramFreq.begin(); it != contextTrigramFreq.end(); it++) {
	if ((*it).second > THRESHOLD) {
	    string trigram = (*it).first;
	    size_t p = trigram.rfind('|');
	    string bigram = trigram.substr(0, p);
	    double probability = ((double) ((*it).second)) / (double) tagBigramFreq[bigram];
	    long int probabilityInt = (long int)(-100.0 * log(probability));
	    if (opt_g != 0 && probabilityInt < opt_g) {
		fprintf(CONTEXT_3_FREQ, "%s|%li\n", trigram.c_str(), probabilityInt);
	    }
	}
    }

    // word-tags lexicon
    for (unordered_map<string, long int>::iterator it = lexicon.begin(); it != lexicon.end(); it++) {
	// foreach $pair (keys %lexicon){
	// lexicon (word is printed twice as this format is required by fadd morphology handling)
	if ((*it).second > THRESHOLD) {
	    string pair = (*it).first;
	    size_t p = pair.find('|');
	    string word = pair.substr(0, p);
	    string tag = pair.substr(p + 1);
	    fprintf(WORD_TAG_LEX, "%s\t%s\t%s\n", word.c_str(), word.c_str(), tag.c_str());
	}
    }

    // word-tag data, tag-word data
    unordered_map<string, long int> unknown;
    for (unordered_map<string, long int>::iterator it = wordTagFreq.begin(); it != wordTagFreq.end(); it++) {
	string pair = (*it).first;
	size_t p = pair.find('|');
	string word = pair.substr(0, p);
	string tag = pair.substr(p + 1);
	// word-tag data (probability of seeing word given tag)
	if (wordFreq[word] > THRESHOLD) {
	    long int probabilityInt = (long int)(-100.0 * log(((double) (*it).second) / (double) tagUnigramFreq[tag]));
	    if (opt_g != 0 && probabilityInt < opt_g) {
		fprintf(WORD_TAG_FREQ, "%s|%li\n", pair.c_str(), probabilityInt);
	    }
	}
	// tag-word data (probability of seeing tag given word; used for baseline)
	if ((*it).second > THRESHOLD) {
	    long int probabilityInt = (long int) (-100.0 * log(((double) (*it).second) / (double) wordFreq[word]));
	    if (opt_g != 0 && probabilityInt < opt_g) {
		fprintf(TAG_WORD_FREQ, "%s|%s|%lu\n", tag.c_str(), word.c_str(), probabilityInt);
	    }
	}
	// collect data for unknown word; collect tags for all words that occur only once
	//if($UNKNOWN_WORD_TAGPERCENTAGE>0){
	if (wordFreq[word] == 1) {
	    unknown[tag]++;
	}
	//}
    }
    // print tag that was most often assigned to words that occur only once
    long int tagvalue = 0;
    string tagkey = "";
    for (unordered_map<string, long int>::iterator it = unknown.begin(); it != unknown.end(); it++) {
	if ((*it).second > tagvalue) {
	    tagvalue = (*it).second;
	    tagkey = (*it).first;
	}
    }
    if (tagvalue > 0) {
	fprintf(WORD_TAG_LEX, "%s\t%s\t%s\n", UNKNOWN_WORD.c_str(), UNKNOWN_WORD.c_str(), tagkey.c_str());
    }
    fprintf(WORD_TAG_FREQ, "%s|%s|0\n", DUMMY_WORD.c_str(), SENTENCE_END.c_str());
    fprintf(WORD_TAG_FREQ, "%s|%s|0\n", DUMMY_WORD.c_str(), SENTENCE_START.c_str());

    // used context labels
    for (unordered_map<string, long int>::iterator it = usedContext.begin(); it != usedContext.end(); it++) {
	fprintf(USED_CONTEXT, "%s\n", (*it).first.c_str());
    }

    // close files
    fclose(TAG_4_FREQ);
    fclose(TAG_3_FREQ);
    fclose(TAG_2_FREQ);
    fclose(PRE_3_FDIV);
    fclose(PRE_2_FDIV);
    fclose(CONTEXT_3_FREQ);
    fclose(WORD_TAG_FREQ);
    fclose(TAG_WORD_FREQ);
    fclose(USED_CONTEXT);
    fclose(WORD_TAG_LEX);

    return 0;
}

void process_args ()
{
    while (arg_c > 1 && arg_v [1][0] == '-') {
        switch (arg_v [1][1]) {
            case 'c':
                opt_c = atoi (get_arg ());
                break;
            case 'd':
                opt_d = get_arg ();
                break;
            case 'f':
                opt_f = atoi (get_arg ());
                break;
            case 'g':
                opt_g = atoi (get_arg ());
                break;
            default:
                errit ("Illegal option '%s'", arg_v [1]);
        }
	arg_c--;
	arg_v++;
    }
}

char *get_arg ()
{
    if (arg_v [1][2])
        return arg_v [1] + 2;

    if (arg_c == 2)
        errit ("Missing argument for '%s'", arg_v [1]);

    arg_v++;
    arg_c--;
    return arg_v [1];
}

void errit (char const *format, ...)
{
    va_list
	list;

    fprintf (stderr, "\nError %s: ", programname);

    va_start (list, format);
    vfprintf (stderr, format, list);

    fprintf (stderr, "\n\n");

    exit (1);
}

void get_programname (char const *argv0)
{
    char const
        *p;
    p = strrchr (argv0, '/');
    if (p)
        programname = strdup (p + 1);
    else
        programname = strdup (argv0);
}

void syntax ()
{
    fprintf (
	stderr,
	"Usage: %s [args] [filename]\n"
	"\n"
	"args with default values are:\n"
	"\n"
	"-c 0 : use extra context (0 means using dummy everywhere)\n"
	"-d . : directory to write files to\n"
	"-f 0 : minimal frequency - tuple is otherwise ignored\n"
	"-g 0 : maximal -log score - tuple is otherwise ignored\n"
	"\n"
	"if filename is missing, read from stdin\n"
	"\n",
	programname
    );
    exit (1);
}
