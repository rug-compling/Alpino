#ifndef DZ_OSTREAMBUF_HH
#define DZ_OSTREAMBUF_HH

#include <cstdio>
#include <streambuf>
#include <string>
#include <vector>

#include <boost/smart_ptr/shared_ptr.hpp>

#include <zlib.h>

#include "DzChunk.hh"
#include "gzip.hh"

namespace indexedcorpus {

// Warning: streambufs are really to stateful for multithreading (see
// work done during destruction). Users of this classes should do locking.
class DzOstreamBuf : public std::streambuf {
public:
	DzOstreamBuf(char const *filename);
	virtual ~DzOstreamBuf();
protected:
	virtual int overflow(int c);
	virtual std::streamsize xsputn(char const *s, std::streamsize n);
private:
	DzOstreamBuf(DzOstreamBuf const &other);
	DzOstreamBuf &operator=(DzOstreamBuf const &other);
	void flushBuffer();
	void writeChunkInfo();
	void writeHeader();
	void writeTrailer();
	void writeZData();

	std::string d_tmpFilename;
	FILE *d_dzStream;
	FILE *d_zDataStream;
	z_stream d_zStream;
	std::vector<unsigned char> d_buffer;
	size_t d_size;
	uLong d_crc32;
	std::vector<DzChunk> d_chunks;
};

}

#endif //DZ_OSTREAMBUF_HH

