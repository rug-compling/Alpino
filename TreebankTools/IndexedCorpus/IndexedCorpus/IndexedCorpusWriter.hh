#ifndef CORPUSWRITER_HH
#define CORPUSWRITER_HH

#include <iostream>
#include <memory>

#include <boost/config.hpp>

#if defined(BOOST_HAS_THREADS)
#include <boost/thread.hpp>
#endif

#include "NullStream.hh"

namespace indexedcorpus
{

typedef std::shared_ptr<std::ostream> ostreamPtr;

class IndexedCorpusWriter
{
public:
	IndexedCorpusWriter(ostreamPtr dataStream, ostreamPtr indexStream) :
		d_dataStream(dataStream), d_indexStream(indexStream), d_offset(0) {}
	IndexedCorpusWriter() :
		d_dataStream(new NullStream), d_indexStream(new NullStream), d_offset(0) {}
	IndexedCorpusWriter(IndexedCorpusWriter const &other);
	IndexedCorpusWriter &operator=(IndexedCorpusWriter const &other);
	void write(std::string const &name, std::string const &data);
	void write(std::string const &name, char const *buf, size_t len);
private:
	void copy(IndexedCorpusWriter const &other);
	ostreamPtr d_dataStream;
	ostreamPtr d_indexStream;
	size_t d_offset;

#if defined(BOOST_HAS_THREADS)
	boost::mutex d_writeMutex;
#endif
};

inline IndexedCorpusWriter::IndexedCorpusWriter(IndexedCorpusWriter const &other)
{
	copy(other);
}

inline IndexedCorpusWriter &IndexedCorpusWriter::operator=(IndexedCorpusWriter const &other)
{
	if (this != &other)
		copy(other);
	
	return *this;
}

}

#endif // CORPUSWRITER_HH
