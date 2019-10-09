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

long int
    opt_c = 0;

string SENTENCE_START("xxx_sentence_start");
string SENTENCE_END("xxx_sentence_end");
string DUMMY_CONTEXT("<DUMMY_CONTEXT>");
string DUMMY_WORD("<DUMMY_WORD>");
string UNKNOWN_WORD("<UNKNOWN_WORD>");

string firstTag(SENTENCE_START);
string secondTag(SENTENCE_START);
string oldContext(DUMMY_CONTEXT);

string previousKey("<NONE>");

unordered_map<string, long int>
    wordFreq,
    wordTagFreq,
    contextTrigramFreq,
    tagFourgramFreq,
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

void addToLexicon(string &word, string &tag, string &words)
{
    string entry;
    if (words.size() == 0)
	entry = word  + "|" +  tag;
    else
	entry = word  + "|" +  tag + "^" + words;
    lexicon[entry]++;
}

FILE *fileCreate(char const *ext)
{
    FILE *fp;
    strcpy (buffer, filebase);
    strcat (buffer, ext);
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

    filebase[0] = '\0';

    get_programname (argv [0]);
    arg_c = argc;
    arg_v = argv;
    process_args ();

    if (arg_c == 1 && !isatty (fileno (stdin)) && filebase[0] != '\0')
	fp = stdin;
    else if (arg_c == 2) {
	if (filebase[0] == '\0')
	    strcpy (filebase, arg_v [1]);
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
	string w0, w1, w2, w6;
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
		        w0 = buffer + i1;
		        break;
		    case 1:
		        w1 = buffer + i1;
		        break;
		    case 2:
		        w2 = buffer + i1;
		        break;
		    case 6:
		        w6 = buffer + i1;
		        break;
		}
		n++;
		i1 = i + 1;
	    }
	}
	if (n < 7) {
	    for (i--; i >= 0; i--)
		if (buffer [i] == '\0')
		    buffer [i] = '|';
	    cerr << "Parse failed for line " << lineno << ": " << buffer << endl;
	    continue;
	}
	word = w0;
	thirdTag = w1;
	sentenceKey = w2;
	newContext = w6;

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
		} else {
		    std::cerr << "Multiword tag error: " << word << " : " << thirdTag << std::endl;
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
		string fourgram = firstTag + "|" + secondTag + "|" + oldContext + "|" + SENTENCE_END;

		// store tag n-grams
		tagFourgramFreq[fourgram]++;

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

	string fourgram = firstTag + "|" + secondTag + "|" + oldContext + "|" + thirdTag;

	// store tag n-gram
	tagFourgramFreq[fourgram]++;

	// store context n-gram
	string contextTrigram = oldContext + "|" + thirdTag + "|" + newContext;
	contextTrigramFreq[contextTrigram]++;

	// keep track of encountered contexts
	usedContext.insert(newContext);

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

    string fourgram = firstTag + "|" + secondTag + "|" + oldContext + "|" + SENTENCE_END;

    tagFourgramFreq[fourgram]++;

    // store context n-gram
    string contextTrigram = oldContext + "|" + SENTENCE_END + "|" + DUMMY_CONTEXT;
    contextTrigramFreq[contextTrigram]++;

    if(previousMultiTag != "<NONE>") {
	addToLexicon(multiTagFirstWord, previousMultiTag, multiTagRestWords);
    }


    // ########################################################################

    fp = fileCreate(".tFF");
    for (unordered_map<string, long int>::iterator it = tagFourgramFreq.begin(); it != tagFourgramFreq.end(); it++)
	fprintf(fp, "%li\t%s\n", (*it).second, (*it).first.c_str()); 
    fclose(fp);

    fp = fileCreate(".cTF");
    for (unordered_map<string, long int>::iterator it = contextTrigramFreq.begin(); it != contextTrigramFreq.end(); it++)
	fprintf(fp, "%li\t%s\n", (*it).second, (*it).first.c_str()); 
    fclose(fp);

    fp = fileCreate(".wTF");
    for (unordered_map<string, long int>::iterator it = wordTagFreq.begin(); it != wordTagFreq.end(); it++)
	fprintf(fp, "%li\t%s\n", (*it).second, (*it).first.c_str()); 
    fclose(fp);

    fp = fileCreate(".wF");
    for (unordered_map<string, long int>::iterator it = wordFreq.begin(); it != wordFreq.end(); it++)
	fprintf(fp, "%li\t%s\n", (*it).second, (*it).first.c_str()); 
    fclose(fp);

    fp = fileCreate(".lex");
    for (unordered_map<string, long int>::iterator it = lexicon.begin(); it != lexicon.end(); it++)
	fprintf(fp, "%li\t%s\n", (*it).second, (*it).first.c_str()); 
    fclose(fp);

    fp = fileCreate(".uC");
    for (unordered_set<string>::iterator it = usedContext.begin(); it != usedContext.end(); it++)
	fprintf(fp, "%s\n", (*it).c_str()); 
    fclose(fp);

    // ########################################################################

    return 0;
}

void process_args ()
{
    while (arg_c > 1 && arg_v [1][0] == '-') {
        switch (arg_v [1][1]) {
	    case 'b':
	        strcpy (filebase, get_arg ());
		break;
            case 'c':
                opt_c = atoi (get_arg ());
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
	"-b output : filebase (optional unless reading from stdin)\n"
	"-c 0 : use extra context (0 means using dummy everywhere)\n"
	"\n"
	"if filename is missing, read from stdin\n"
	"\n",
	programname
    );
    exit (1);
}
