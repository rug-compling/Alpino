#ifndef DZ_ISTREAM_HH
#define DZ_ISTREAM_HH

#include <iostream>
#include <memory>
#include <string>

#include "DzIstreamBuf.hh"

namespace indexedcorpus {

class DzIstream : public std::istream
{
public:
	DzIstream(char const *filename); // Let's stick to the standards... :/
	virtual ~DzIstream() {}
private:
	std::shared_ptr<DzIstreamBuf> d_streamBuf;
};

}

#endif // DZ_ISTREAM_HH
