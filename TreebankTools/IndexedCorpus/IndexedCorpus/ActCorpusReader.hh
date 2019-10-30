/*
 * Reader for Alpino Corpus Tool-style corpora
 *
 * Alpino uses compact corpora for treebanks, which can be read by combining
 * DzStreamBuf and IndexedCorpusReader instances. However, pathnames, rather
 * than arbitrary names are used. If paths lead to existing files, reading
 * a file directly is preferred over reading the compact corpus.
 *
 * This class implements such functionality.
 */


#ifndef ACT_CORPUS_READER
#define ACT_CORPUS_READER

#include <mutex>
#include <string>
#include <vector>

#include <boost/filesystem.hpp>

#include "IndexNamePair.hh"
#include "IndexedCorpusReader.hh"

namespace indexedcorpus {

class ActCorpusReader
{
public:
	ActCorpusReader() {}
	ActCorpusReader(ActCorpusReader const &other);
	ActCorpusReader &operator=(ActCorpusReader const &other);
	std::vector<std::string> entries(std::string const &path);
	std::vector<unsigned char> getData(std::string const &path);
	std::string pathName(std::string const &path, int offset);
private:
	void copy(ActCorpusReader const &other);
	bool dzCorpusExists(boost::filesystem::path const &name) const;
	std::vector<std::string> entriesCorpus(boost::filesystem::path const &name);
	std::vector<std::string> entriesDirectory(boost::filesystem::path const &name);
	std::string findEntry(std::vector<std::string> const &entries,
		std::string const &entry, int offset) const;
	std::string pathNameCorpus(boost::filesystem::path const &corpus,
		boost::filesystem::path const &filename, int offset);
	std::string pathNameDirectory(boost::filesystem::path const &directory,
		boost::filesystem::path const &filename, int offset);
	std::vector<unsigned char> readFromCorpus(
		boost::filesystem::path const &corpus,
		boost::filesystem::path const &file);
	std::string stripCorpusExt(std::string const &name) const;

	std::string d_lastDir;
	std::vector<IndexNamePair> d_lastDirEntries;
	std::string d_lastCorpusPath;
	IndexedCorpusReader d_lastCorpusReader;

	std::mutex d_readMutex;
};

inline ActCorpusReader::ActCorpusReader(ActCorpusReader const &other)
{
	copy(other);
}

}

#endif // ACT_CORPUS_READER

