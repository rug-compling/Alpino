#include <algorithm>
#include <iterator>
#include <sstream>
#include <string>
#include <boost/shared_ptr.hpp>

#include <IndexedCorpus/ActCorpusReader.hh>
#include <IndexedCorpus/DzOstream.hh>

#include "test.h"

using namespace std;
using namespace indexedcorpus;

void test_dzostream_crash()
{
	string dataFilename("/tmp/doesnotexist/asdf.data.dz");
	boost::shared_ptr<indexedcorpus::DzOstream> dataStream(
	new indexedcorpus::DzOstream(dataFilename.c_str()));
}

string dataToString(vector<unsigned char> const &data)
{
	ostringstream oss;

	copy(data.begin(), data.end(), ostream_iterator<char>(oss, ""));
	
	return oss.str();
}

void testDzPathName(ActCorpusReader corpusReader, string const &suite)
{
	// Valid pathNames
	testEqual<string>(corpusReader.pathName(suite + "/1.xml", 1), suite + "/2.xml",
		"ActCorpusReader::pathName (positive offset)");
	testEqual<string>(corpusReader.pathName(suite + "/3.xml", -2), suite + "/1.xml",
		"ActCorpusReader::pathName (negative offset)");

	// Invalid path
	bool fail = false;
	try {
		corpusReader.pathName(suite + "/10.xml", -9);
	} catch (...) {
		fail = true;
	}
	testEqual<bool>(fail, true, "ActCorpusReader::pathName (unknown path)");
	
	// Bad offset
	fail = false;
	try {
		corpusReader.pathName(suite + "/1.xml", -1);
	} catch (...) {
		fail = true;
	}
	testEqual<bool>(fail, true, "ActCorpusReader::pathName (bad offset #1)");
	
	// Bad offset
	fail = false;
	try {
		corpusReader.pathName(suite + "/1.xml", 5);
	} catch (...) {
		fail = true;
	}
	testEqual<bool>(fail, true, "ActCorpusReader::pathName (bad offset #2)");
}

void testDzGetData(ActCorpusReader corpusReader, string const &suite)
{
	testEqual<string>(dataToString(corpusReader.getData(suite + "/1.xml")), "one\n",
		"ActCorpusReader::getData (get data)");
	
	// Unknown path
	bool fail = false;
	try {
		corpusReader.getData(suite + "/10.xml");
	} catch (...) {
		fail = true;
	}
	testEqual<bool>(fail, true, "ActCorpusReader::getData (unknown path)");
}

int main(int argc, char *argv[])
{
	ActCorpusReader reader;

	cout << "---" << endl << "-> check handling of unwritable file" << endl << endl;
	test_dzostream_crash();
	cout << "OK" << endl;

	// Pathname checks
	cout << "---" << endl << "-> pathName checks on 'test_suite'" << endl << endl;
	testDzPathName(reader, "test_suite");
	
	cout << "---" << endl << "-> pathName checks on 'test_suite2'" << endl << endl;
	testDzPathName(reader, "test_suite2");

	cout << "---" << endl << "-> pathName checks on 'test_suite.index'" << endl << endl;
	testDzPathName(reader, "test_suite.index");
	
	cout << "---" << endl << "-> pathName checks on 'test_suite.data.dz'" << endl << endl;
	testDzPathName(reader, "test_suite.data.dz");
	
	// Data checks
	cout << "---" << endl << "-> data checks on 'test_suite'" << endl << endl;
	testDzGetData(reader, "test_suite");
	
	cout << "---" << endl << "-> data checks on 'test_suite2'" << endl << endl;
	testDzGetData(reader, "test_suite2");

	cout << "---" << endl << "-> data checks on 'test_suite.index'" << endl << endl;
	testDzGetData(reader, "test_suite.index");
	
	cout << "---" << endl << "-> data checks on 'test_suite.data.dz'" << endl << endl;
	testDzGetData(reader, "test_suite.data.dz");
}
