#ifndef GZIP_HH
#define GZIP_HH

#include <cstddef>

namespace indexedcorpus {

size_t const GZ_HEADER_SIZE = 10;

// Gzip header fields
size_t const GZ_HEADER_ID1 = 0;
size_t const GZ_HEADER_ID2 = 1;
size_t const GZ_HEADER_CM = 2;
size_t const GZ_HEADER_FLG = 3;
size_t const GZ_HEADER_MTIME = 4;
size_t const GZ_HEADER_XFL = 8;
size_t const GZ_HEADER_OS = 9;

// Gzip file magic
unsigned char const gzipId1 = 0x1f;
unsigned char const gzipId2 = 0x8b;

// Gzip compression method(s)
unsigned char const GZ_CM_DEFLATE = 8;

// Flags in GZ_HEADER_FLG
unsigned char const GZ_FLG_TEXT = 1;
unsigned char const GZ_FLG_HCRC = 1 << 1;
unsigned char const GZ_FLG_EXTRA = 1 << 2;
unsigned char const GZ_FLG_NAME = 1 << 3;
unsigned char const GZ_FLG_COMMENT = 1 << 4;

// GZ_HEADER_XFL values for deflate
unsigned char const GZ_XFL_MAX = 2;
unsigned char const GZ_XFL_FAST = 4;

// GZ_HEADER_OS values
unsigned char const GZ_OS_FAT = 0;
unsigned char const GZ_OS_AMIGA = 1;
unsigned char const GZ_OS_VMS = 2;
unsigned char const GZ_OS_UNIX = 3;
unsigned char const GZ_OS_VM_CMS = 4;
unsigned char const GZ_OS_TOS = 5;
unsigned char const GZ_OS_HPFS = 6;
unsigned char const GZ_OS_MAC = 7;
unsigned char const GZ_OS_ZSYSTEM = 8;
unsigned char const GZ_OS_CPM = 9;
unsigned char const GZ_OS_TOPS20 = 10;
unsigned char const GZ_OS_NTFS = 11;
unsigned char const GZ_OS_QDOS = 12;
unsigned char const GZ_OS_RISCOS = 13;
unsigned char const GZ_OS_UNKNOWN = 255;

size_t const GZ_TRAILER_SIZE = 8;

// Gzip trailer fields
size_t const GZ_TRAILER_CRC32 = 0;
size_t const GZ_TRAILER_ISIZE = 4;

}

#endif // GZIP_HH
