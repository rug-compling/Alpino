#ifndef DZ_STREAMBUF_HH
#define DZ_STREAMBUF_HH

#include <cstdio>
#include <iostream>
#include <streambuf>
#include <vector>

#include <stdint.h>

#include "DzChunk.hh"
#include "gzip.hh"

namespace indexedcorpus {

// Warning: streambufs are really to stateful for multithreading (think
// seeking, telling). Users of this classes should do locking, since they
// are more able to lock seek/read combinations.
class DzIstreamBuf : public std::streambuf
{
public:
	typedef std::streambuf::pos_type pos_type;
	typedef std::streambuf::off_type off_type;
	typedef std::ios::seekdir seekdir;
	typedef std::ios::openmode openmode;
	
	DzIstreamBuf(char const *filename);
	~DzIstreamBuf();
	pos_type seekoff(off_type off, seekdir dir, openmode);
	pos_type seekpos(pos_type off, openmode mode);
protected:
	int underflow();
	std::streamsize xsgetn(char *dest, std::streamsize n);
private:
	void readChunk(long n);
	void readHeader();
	void readExtra();
	void skipOptional();
	
	FILE *d_stream;
	std::vector<unsigned char> d_buffer;
	std::vector<unsigned char> d_header;
	size_t d_chunkLen;
	long d_dataOffset;
	long d_curChunk;
	std::vector<DzChunk> d_chunks;
};

}

#endif // DZ_STREAMBUF_HH

