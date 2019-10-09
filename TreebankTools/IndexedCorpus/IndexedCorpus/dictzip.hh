#ifndef DICTZIP_HH
#define DICTZIP_HH

size_t const DZ_MAX_COMPRESSED_SIZE = 0xffffUL;
size_t const DZ_MAX_UNCOMPRESSED_SIZE = static_cast<size_t>(DZ_MAX_COMPRESSED_SIZE - 12) * 0.999;

// This seems bogus to me, but there are too many dictunzipping programs out there with this as
// a fixed buffer size, prefer it by default.
size_t const DZ_PREF_UNCOMPRESSED_SIZE = static_cast<size_t>((DZ_MAX_COMPRESSED_SIZE - 12) * 0.89);

#endif // DICZIP_HH

