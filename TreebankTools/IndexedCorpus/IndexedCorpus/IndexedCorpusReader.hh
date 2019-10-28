#ifndef CORPUSREADER_HH
#define CORPUSREADER_HH

#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <boost/config.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>

#if defined(BOOST_HAS_THREADS)
#include <boost/thread/thread.hpp>
#endif

namespace indexedcorpus
{

struct IndexItem
{
	IndexItem(std::string newName, size_t newOffset, size_t newSize)
		: name(newName), offset(newOffset), size(newSize) {}
	IndexItem() : name(""), offset(0), size(0) {}

	std::string name;
	size_t offset;
	size_t size;
};

typedef boost::shared_ptr<IndexItem> IndexItemPtr;

typedef std::map<std::string, IndexItemPtr> IndexMap;
typedef std::vector<IndexItemPtr> IndexPtrVec;

typedef boost::shared_ptr<std::istream> IstreamPtr;

class IndexedCorpusReader
{
public:
	IndexedCorpusReader() {}
	IndexedCorpusReader(IndexedCorpusReader const &other);
	IndexedCorpusReader(IstreamPtr dataStream, IstreamPtr indexStream);
	virtual ~IndexedCorpusReader();
	IndexedCorpusReader &operator=(IndexedCorpusReader const &other);
	std::vector<std::string> entries() const;
	IndexPtrVec const &indices() const;
	std::vector<unsigned char> read(std::string const &filename);
private:
	void copy(IndexedCorpusReader const &other);
	void destroy();
	
	IstreamPtr d_dataStream;
	IndexPtrVec d_indices;
	IndexMap d_namedIndices;

#if defined(BOOST_HAS_THREADS)
	boost::mutex d_readMutex;
#endif
};

inline IndexedCorpusReader::IndexedCorpusReader(IndexedCorpusReader const &other)
{
	copy(other);
}

inline IndexedCorpusReader::~IndexedCorpusReader()
{
	destroy();
}

inline IndexPtrVec const &IndexedCorpusReader::indices() const
{
	return d_indices;
}

}

#endif // CORPUSWRITER_HH
