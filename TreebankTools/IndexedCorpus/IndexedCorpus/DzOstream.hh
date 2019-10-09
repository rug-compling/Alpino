#ifndef DZ_OSTREAM_HH
#define DZ_OSTREAM_HH

#include <iostream>
#include <string>

#include <boost/smart_ptr/shared_ptr.hpp>

#include "DzOstreamBuf.hh"

namespace indexedcorpus {

class DzOstream : public std::ostream
{
public:
	DzOstream(char const *filename); // Let's stick to the standards... :/
	virtual ~DzOstream() {}
private:
	boost::shared_ptr<DzOstreamBuf> d_streamBuf;
};

}

#endif // DZ_ISTREAM_HH
