#ifndef DZ_CHUNK_HH
#define DZ_CHUNK_HH

#include <cstddef>

namespace indexedcorpus {

struct DzChunk
{
	DzChunk(size_t newOffset, size_t newSize) : offset(newOffset), size(newSize) {}
	size_t offset;
	size_t size;
};

}

#endif // DZ_CHUNK_HH
