/* Rewritten from scratch by Daniël de Kok <me@danieldk.eu>. Tables from
 * original base64 decoding functions, Copyright 1996, 2002 Rickard E.
 * Faith.
 */

#ifndef CORPUSWRITER_BASE64
#define CORPUSWRITER_BASE64

#include <stdexcept>
#include <string>

namespace {
  std::string b64_chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

#define XX 100

static unsigned char b64_index[256] = {
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,62, XX,XX,XX,63,
    52,53,54,55, 56,57,58,59, 60,61,XX,XX, XX,XX,XX,XX,
    XX, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,XX, XX,XX,XX,XX,
    XX,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
};

}

template <typename T>
std::string b64_encode(T val)
{
  // Find number of 6-bit chunks
  size_t chunks = 1;
  if (val != 0)
  {
    T val2 = val;
    while (val2 >>= 6) { ++chunks; }
  }

  std::string result(chunks, 0);
  for (size_t i = 0; i < chunks; ++i)
  {
    size_t shift = i * static_cast<size_t>(6);
    T mask = static_cast<T>(0x3f) << shift;
    result[chunks - i - 1] = b64_chars[(val & mask) >> shift];
  }

  return result;
}

template <typename T>
T b64_decode(std::string const &val)
{
  T result = 0;
  size_t offset = 0;

  for (std::string::const_reverse_iterator iter = val.rbegin();
    iter != val.rend(); ++iter)
  {

    T tmp = static_cast<T>(b64_index[static_cast<unsigned char>(*iter)]);
    if (tmp == XX)
      throw std::runtime_error(std::string("Illegal character in base64 value: ") + *iter);

    // If we lose bits in bit shifts, then the given type is not large
    // enough to store the encoded base64 value.
    if ((tmp << offset) >> offset != tmp )
      throw std::runtime_error(
        std::string("Type cannot store decoded base64 value:") + val);

    result |= tmp << offset;

    offset += static_cast<size_t>(6);
  }

  return result;
}

#endif // CORPUSWRITER_BASE64
