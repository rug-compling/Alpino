#ifndef BUF_UTIL_HH
#define BUF_UTIL_HH

namespace indexedcorpus {

template <typename T>
void writeToBuf(unsigned char *buf, T n)
{
	for (size_t i = 0; i < sizeof(T); ++i)
		buf[i] = (n >> i * 8) & 0xff;
}

}

#endif // BUF_UTIL_HH

