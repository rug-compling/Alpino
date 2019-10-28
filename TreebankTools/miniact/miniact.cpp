/*
 * Mini-act: create Alpino compact corpus files.
 * Send flames to: Daniel de Kok <me@danieldk.eu>
 */

#include <algorithm>
#include <cctype>
#include <stdlib.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <glob.h>
#include <libgen.h>
#include <unistd.h>
#include <sys/stat.h>

#include "util/fdstream.hh"
#include "util/stringutil.hh"

#include <IndexedCorpus/DzIstreamBuf.hh>
#include <IndexedCorpus/DzOstream.hh>
#include <IndexedCorpus/IndexedCorpusReader.hh>
#include <IndexedCorpus/IndexedCorpusWriter.hh>
#include <IndexedCorpus/util/InfInt/InfInt.h>
#include "ProgramOptions/ProgramOptions.hh"

using namespace std;

char const *TMP_TEMPLATE = "miniactXXXXXX";

struct IndexNamePair
{
	IndexNamePair(vector<string> const &newIndex, string const &newName) :
		index(newIndex), name(newName) {}
	vector<string> index;
	string name;
};

struct IndexNamePairCompare
{
	bool operator()(IndexNamePair const &p1, IndexNamePair const &p2)
	{
		vector<string> const &i1 = p1.index;
		vector<string> const &i2 = p2.index;
		
		for (size_t i = 0; i < i1.size() && i < i2.size(); ++i)
		{
			// Both digits? Sort on digits!
			if (isdigit(i1[i][0]) && isdigit(i2[i][0]))
			{
				InfInt d1 = miniact::parseString<InfInt>(i1[i]);
				InfInt d2 = miniact::parseString<InfInt>(i2[i]);
				
				if (d1 != d2)
					return d1 < d2;
			}
			else
				if (i1[i] != i2[i])
					return i1[i] < i2[i];
		}
		
		// The pairs are equal...
		return false;
	}
};

bool fExists(string const &name)
{
	struct stat s;
	return stat(name.c_str(), &s) == 0;
}

bool isDir(string const &name)
{
	struct stat s;
	int r = stat(name.c_str(), &s);
	
	if (r != 0)
		return false;
	
	return s.st_mode & S_IFDIR;
}

vector<string> nameIndex(string const &name)
{
	vector<string> index;
	
	if (name.size() == 0)
		return index;

	ostringstream buf;
	
	bool prevIsDigit = isdigit(name[0]);
	for (string::const_iterator iter = name.begin(); iter != name.end();
		++iter)
	{
		bool curIsDigit = isdigit(*iter);

		if (curIsDigit && prevIsDigit)
			buf << *iter;
		else
		{
			index.push_back(buf.str());
			buf.str("");
			buf << *iter;
		}
		
		prevIsDigit = curIsDigit;
	}
	
	// Leftover
	index.push_back(buf.str());
	
	return index;
}

void usage(string const &programName)
{
	cerr << "Usage: " << programName << " [OPTION] treebank_dir arguments" <<
		endl << endl <<
		"  -c\t\tCreate a compact archive" << endl <<
		"  -l\t\tList compact archive contents" << endl <<
		"  -s\t\tPrint to stdout (affects -x)" << endl <<
		"  -x\t\tExtract compact archive" << endl << endl;
}

bool isTreebankDir(std::string const &directory)
{
	glob_t g;

	string pattern = directory + "/*.xml";
	glob(pattern.c_str(), 0, NULL, &g);
	bool treebankDir = g.gl_pathc != 0;

	globfree(&g);

	return treebankDir;
}

string readFile(std::string const &filename)
{
	ifstream in(filename.c_str());
	if (!in)
		throw(string("Could not read file: ") + filename);

	ostringstream buf;
	
	istreambuf_iterator<char> it(in);
	istreambuf_iterator<char> end;
	std::copy(it, end, ostream_iterator<char>(buf));
	
	return buf.str();
}

void compactDir(std::string const &directory, indexedcorpus::IndexedCorpusWriter *corpusWriter)
{
	cout << "Processing '" << directory << "'..." << endl;
	glob_t g;
	string pattern = directory + "/*.xml";
	glob(pattern.c_str(), 0, NULL, &g);
	
	vector<IndexNamePair> xmlFiles;
	
	for (size_t i = 0; i < g.gl_pathc; ++i)
	{
		string name = basename(g.gl_pathv[i]);
		vector<string> index(nameIndex(name));
		xmlFiles.push_back(IndexNamePair(index, name));
	}
	
	sort(xmlFiles.begin(), xmlFiles.end(), IndexNamePairCompare());
	
	for (vector<IndexNamePair>::const_iterator iter = xmlFiles.begin();
		iter != xmlFiles.end(); ++iter)
	{
		string fn = directory + "/" + iter->name;
		string data = readFile(fn);

		if (data.empty() || data.find_first_not_of(" \n\r") == string::npos) {
			cerr << "Skipping empty file: " << fn << endl;
			continue;
		}

		corpusWriter->write(iter->name, data);
	}
	
	globfree(&g);
}

void compactDirs(vector<string> const &dirs)
{
	for (vector<string>::const_iterator iter = dirs.begin();
		iter != dirs.end(); ++ iter)
	{
		string dirName = *iter;
		
		// Remove trailing directory name slashes.
		while (dirName[dirName.size() - 1] == '/')
			dirName.erase(dirName.size() - 1);

		if (!isTreebankDir(dirName))
		{
			cerr << "'" << dirName <<
				"' does not look like a treebank directory!" << endl;
			continue;
		}
		

		string dataFilename = dirName + ".data.dz";
		std::shared_ptr<indexedcorpus::DzOstream> dataStream(
			new indexedcorpus::DzOstream(dataFilename.c_str()));
		if (!*dataStream)
			throw runtime_error("Could not open corpus data file for writing!");

		string indexFilename = dirName + ".index";
		std::shared_ptr<ofstream> indexStream(new ofstream(indexFilename.c_str()));
		if (!*indexStream)
			throw runtime_error("Could not open corpus index file for writing!");
		
		indexedcorpus::IndexedCorpusWriter corpusWriter(dataStream, indexStream);
		compactDir(dirName, &corpusWriter);		
	}
}

void extractCorpus(vector<string> const &args, bool useStdout)
{
	string corpusName = args[0];
	
	// Remove trailing directory name slashes.
	while (corpusName[corpusName.size() - 1] == '/')
		corpusName.erase(corpusName.size() - 1);

	// Get the last path component. Since basename can modify a string,
	// make a copy first.
	char *tmp = strdup(corpusName.c_str());
	string dirName(basename(tmp));
	free(tmp);

	string dataFilename = corpusName + ".data.dz";
	indexedcorpus::DzIstreamBuf dzStreamBuf(dataFilename.c_str());
	std::shared_ptr<istream> dataStream(new istream(&dzStreamBuf));
	if (!*dataStream)
		throw runtime_error("Could not open corpus data file for reading!");

	string indexFilename = corpusName + ".index";
	std::shared_ptr<ifstream> indexStream(new ifstream(indexFilename.c_str()));
	if (!*indexStream)
		throw runtime_error("Could not open corpus index file for reading!");

	indexedcorpus::IndexedCorpusReader corpusReader(dataStream, indexStream);
	
	// Create the target directory, if it does not exist already.
	if (!useStdout && !isDir(dirName))
	{
		if (fExists(dirName))
			throw runtime_error(string("Corpus '") + dirName +
				"' exists, but is not a directory!" );
		else
			mkdir(dirName.c_str(), 0755);
	}

	vector<string> entries;
	if (args.size() == 1) // All entries
		entries = corpusReader.entries();
	else // Specified entries
		copy(args.begin() + 1, args.end(), back_inserter(entries));
	
	for (vector<string>::const_iterator iter = entries.begin();
		iter != entries.end(); ++iter)
	{
		vector<unsigned char> data = corpusReader.read(*iter);
		
		if (useStdout)
			copy(data.begin(), data.end(), ostream_iterator<char>(cout, ""));
		else {
			string outFilename(dirName + "/" + *iter);
			ofstream out(outFilename.c_str());
			copy(data.begin(), data.end(), ostream_iterator<char>(out, ""));
		}
	}
}

void listCorpus(indexedcorpus::IndexedCorpusReader const &corpusReader)
{
	for (indexedcorpus::IndexPtrVec::const_iterator iter = corpusReader.indices().begin();
			iter != corpusReader.indices().end(); ++iter)
		cout << (*iter)->name << endl;
}

void listCorpora(vector<string> const &dirs)
{
	for (vector<string>::const_iterator iter = dirs.begin();
		iter != dirs.end(); ++ iter)
	{
		string dirName = *iter;
		
		// Remove trailing directory name slashes.
		while (dirName[dirName.size() - 1] == '/')
			dirName.erase(dirName.size() - 1);
			
		string dataFilename = dirName + ".data.dz";
		std::shared_ptr<ifstream> dataStream(new ifstream(dataFilename.c_str()));
		if (!*dataStream)
			throw runtime_error("Could not open corpus data file for reading!");

		string indexFilename = dirName + ".index";
		std::shared_ptr<ifstream> indexStream(new ifstream(indexFilename.c_str()));
		if (!*indexStream)
			throw runtime_error("Could not open corpus index file for reading!");
			
		indexedcorpus::IndexedCorpusReader corpusReader(dataStream, indexStream);
		listCorpus(corpusReader);
	}
	
}

int main(int argc, char const *argv[])
{
	miniact::ProgramOptions programOptions(argc, argv, "clsx");

	if (programOptions.arguments().size() == 0 ||
		!(programOptions.option('c') || programOptions.option('l') ||
		programOptions.option('x')))
	{
		usage(programOptions.programName());
		return 1;
	}
	
	if (programOptions.option('c'))
		compactDirs(programOptions.arguments());
	else if (programOptions.option('x'))
		extractCorpus(programOptions.arguments(), programOptions.option('s'));
	else
		listCorpora(programOptions.arguments());
}
