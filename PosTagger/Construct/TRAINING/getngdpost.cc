#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cstdarg>
#include <cerrno>
#include <cmath>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <unistd.h>

using namespace std;

#define BUFSIZE 10000

char buffer[BUFSIZE + 1];
char filebase[BUFSIZE + 1];

int arg_c;

char **arg_v;
char *programname;
char statCmd[100];

long int
    opt_f = 0,
    opt_g = 0;

bool
    opt_s = false;

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

unordered_set<string>
    usedContext;

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

void stats()
{
    if (! opt_s)
	return;

    std::system(statCmd);
}

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

FILE *fileOpen(char const *ext)
{
    FILE
	*fp;

    strcpy(buffer, filebase);
    strcat(buffer, ext);
    fp = fopen(buffer, "r");
    if (! fp)
	errit ("Opening file %s: %s", buffer, strerror (errno));
    return fp;
}

bool getLine(FILE *fp)
{
    if (fgets (buffer, BUFSIZE, fp) == NULL)
	return false;

    size_t i = std::strlen (buffer);
    while (i > 0 && isspace ((unsigned char) buffer [i - 1]))
	buffer [--i] = '\0';

    return true;
}

int main(int argc, char *argv[])
{
    FILE
	*fp;

    string nullstring("");
    string word, thirdTag, sentenceKey, newContext, baseTag;

    get_programname (argv [0]);
    arg_c = argc;
    arg_v = argv;
    process_args ();

    if (arg_c < 2)
	syntax();

    std::sprintf(statCmd, "ps -o %%mem,etime %i", getpid());

    for (int i = 1; i < arg_c; i++) {
	size_t n = std::strlen(arg_v [i]);
	if (n < 4)
	    syntax();
	if (strcmp (arg_v [i] + n - 4, ".cTF"))
	    errit ("Invalid filename: %s", arg_v [i]);
	strcpy (filebase, arg_v [i]);
	filebase [n - 4] = '\0';
	
	fp = fileOpen(".tFF");
	while (getLine (fp)) {
	    char *p = strrchr(buffer, '\t');
	    p[0] = '\0';
	    tagFourgramFreq[p + 1] += atol(buffer);
	}
	fclose(fp);

	fp = fileOpen(".cTF");
	while (getLine (fp)) {
	    char *p = strrchr(buffer, '\t');
	    p[0] = '\0';
	    contextTrigramFreq[p + 1] += atol(buffer);
	}
	fclose(fp);

	fp = fileOpen(".wTF");
	while (getLine (fp)) {
	    char *p = strrchr(buffer, '\t');
	    p[0] = '\0';
	    wordTagFreq[p + 1] += atol(buffer);
	}
	fclose(fp);

	fp = fileOpen(".wF");
	while (getLine (fp)) {
	    char *p = strrchr(buffer, '\t');
	    p[0] = '\0';
	    wordFreq[p + 1] += atol(buffer);
	}
	fclose(fp);

	fp = fileOpen(".lex");
	while (getLine (fp)) {
	    char *p = strrchr(buffer, '\t');
	    p[0] = '\0';
	    lexicon[p + 1] += atol(buffer);
	}
	fclose(fp);

	fp = fileOpen(".uC");
	while (getLine (fp)) {
	    usedContext.insert(buffer);
	}
	fclose(fp);

	std::cerr << "Processed file " << filebase << std::endl;

	stats();
    }

    // ########################################################################

    for (unordered_map<string, long int>::iterator it = tagFourgramFreq.begin(); it != tagFourgramFreq.end(); it++) {
	string fourgram = (*it).first;
	long int count = (*it).second;

	size_t i = fourgram.find_first_of('|');
	string trigram = fourgram.substr(i+1, std::string::npos);

	// store tag n-grams
	tagTrigramFreq[trigram] += count;

        size_t li = fourgram.find_last_of('|');
	string prefixTrigram = fourgram.substr(0, li);

	// diversity
	prefixTrigramDiv[prefixTrigram]++;

	// store prefix n-grams
	prefixTrigramFreq[prefixTrigram] += count;
    }

    for (unordered_map<string, long int>::iterator it = tagTrigramFreq.begin(); it != tagTrigramFreq.end(); it++) {
	string trigram = (*it).first;
	long int count = (*it).second;

	size_t i = trigram.find_first_of('|');
	string bigram = trigram.substr(i+1, std::string::npos);

	size_t li = trigram.find_last_of('|');
	string prefixBigram = trigram.substr(0, li);

	// diversity
	prefixBigramDiv[prefixBigram]++;

	// store tag n-grams
	tagBigramFreq[bigram] += count;
    }

    for (unordered_map<string, long int>::iterator it = tagBigramFreq.begin(); it != tagBigramFreq.end(); it++) {
	string bigram = (*it).first;
	long int count = (*it).second;

	size_t i = bigram.find_first_of('|');
	string unigram = bigram.substr(i+1,std::string::npos);

	// store tag n-grams
	tagUnigramFreq[unigram] += count;
    }

    for (unordered_map<string, long int>::iterator it = prefixTrigramFreq.begin(); it != prefixTrigramFreq.end(); it++) {
	string prefixTrigram = (*it).first;
	long int count = (*it).second;

	size_t i = prefixTrigram.find_first_of('|');
	string prefixBigram = prefixTrigram.substr(i+1,std::string::npos);

	// store prefix n-grams
	prefixBigramFreq[prefixBigram] += count;
    }

    for (unordered_map<string, long int>::iterator it = prefixBigramFreq.begin(); it != prefixBigramFreq.end(); it++) {
	string prefixBigram = (*it).first;
	long int count = (*it).second;

	size_t i = prefixBigram.find_first_of('|');
	string prefixUnigram = prefixBigram.substr(i+1,std::string::npos);

	// store prefix n-grams
	prefixUnigramFreq[prefixUnigram] += count;
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
    for (unordered_set<string>::iterator it = usedContext.begin(); it != usedContext.end(); it++) {
	fprintf(USED_CONTEXT, "%s\n", (*it).c_str());
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

    stats();

    return 0;
}

void process_args ()
{
    while (arg_c > 1 && arg_v [1][0] == '-') {
        switch (arg_v [1][1]) {
            case 'd':
                opt_d = get_arg ();
                break;
            case 'f':
                opt_f = atoi (get_arg ());
                break;
            case 'g':
                opt_g = atoi (get_arg ());
                break;
	    case 's':
		opt_s = true;
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
	"Usage: %s [args] filebase.cTF ...\n"
	"\n"
	"args with default values are:\n"
	"\n"
	"-d . : directory to write files to\n"
	"-f 0 : minimal frequency - tuple is otherwise ignored\n"
	"-g 0 : maximal -log score - tuple is otherwise ignored\n"
	"-s   : print statistics: memory and time usage\n"
	"\n",
	programname
    );
    exit (1);
}
