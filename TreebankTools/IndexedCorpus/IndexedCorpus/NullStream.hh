#ifndef NULLSTREAM_HH
#define NULLSTREAM_HH

#include <iostream>

namespace indexedcorpus {

struct NullStream : std::ostream
{
	NullStream() : std::ios(0), std::ostream(0) {}
};

}

#endif // NULLSTREAM_HH

