#ifndef DZ_OSTREAM_HH
#define DZ_OSTREAM_HH

#include <iostream>
#include <memory>
#include <string>

#include "DzOstreamBuf.hh"

namespace indexedcorpus {

class DzOstream : public std::ostream
{
public:
	DzOstream(char const *filename); // Let's stick to the standards... :/
	virtual ~DzOstream() {}
private:
	std::shared_ptr<DzOstreamBuf> d_streamBuf;
};

}

#endif // DZ_ISTREAM_HH
