#ifndef DZ_ISTREAM_HH
#define DZ_ISTREAM_HH

#include <iostream>
#include <string>

#include <boost/smart_ptr/shared_ptr.hpp>

#include "DzIstreamBuf.hh"

namespace indexedcorpus {

class DzIstream : public std::istream
{
public:
	DzIstream(char const *filename); // Let's stick to the standards... :/
	virtual ~DzIstream() {}
private:
	boost::shared_ptr<DzIstreamBuf> d_streamBuf;
};

}

#endif // DZ_ISTREAM_HH
