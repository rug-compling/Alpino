#include "DzOstreamBuf.ih"

DzOstreamBuf::DzOstreamBuf(char const *filename) : d_size(0), d_crc32(crc32(0L, Z_NULL, 0))
{
	d_dzStream = fopen(filename, "w");
	
	if (d_dzStream == NULL) {
		d_zDataStream = NULL;
		setp(reinterpret_cast<char *>(&d_buffer[0]),
			reinterpret_cast<char *>(&d_buffer[0]));
		return;
	}

	string tmpTemplate = string(filename) + "-XXXXXX";	
	char *tmpFilename = strdup(tmpTemplate.c_str());
	int fd = mkstemp(tmpFilename);

	d_tmpFilename = tmpFilename;
	d_zDataStream = fdopen(fd, "w");
	free(tmpFilename);
	
	if (d_zDataStream == NULL)
		throw runtime_error(string("DzOstreamBuf::DzOstreamBuf: Could not open ") +
			d_tmpFilename + " for writing!");

	d_zStream.next_in = Z_NULL;
	d_zStream.avail_in = 0;
	d_zStream.next_out = Z_NULL;
	d_zStream.avail_out = 0;
	d_zStream.zalloc = Z_NULL;
	d_zStream.zfree = Z_NULL;
	
	if (deflateInit2(&d_zStream, Z_BEST_COMPRESSION, Z_DEFLATED, -15,
			Z_BEST_COMPRESSION, Z_DEFAULT_STRATEGY) != Z_OK)
		throw runtime_error(d_zStream.msg);

	d_buffer.resize(DZ_PREF_UNCOMPRESSED_SIZE);
	
	// Set up the buffer. However, we act as if it is one character smaller,
	// so that we can write a chunk when we get the last character.
	setp(reinterpret_cast<char *>(&d_buffer[0]),
		reinterpret_cast<char *>(&d_buffer[0] + DZ_PREF_UNCOMPRESSED_SIZE));
}

DzOstreamBuf::~DzOstreamBuf()
{
	if (d_zDataStream == NULL || d_dzStream == NULL)
		return;

	// Flush leftovers.
	try {
		flushBuffer();
	} catch (exception &e) {
		cerr << e.what() << endl;
	}

	vector<unsigned char> zBuf(DZ_PREF_UNCOMPRESSED_SIZE);
	d_zStream.next_in = &d_buffer[0];
	d_zStream.avail_in = 0;
	d_zStream.next_out = &zBuf[0];
	d_zStream.avail_out = DZ_PREF_UNCOMPRESSED_SIZE;
	if (deflate(&d_zStream, Z_FINISH) != Z_STREAM_END)
		cerr << d_zStream.msg << endl;
	size_t zSize = DZ_PREF_UNCOMPRESSED_SIZE - d_zStream.avail_out;
	fwrite(&zBuf[0], 1, zSize, d_zDataStream);
	
	switch (deflateEnd(&d_zStream)) {
	case Z_STREAM_ERROR:
		cerr << "DzOstreamBuf::flushBuffer: stream state inconsistent!" << endl;
	case Z_DATA_ERROR:
		cerr << "DzOstreamBuf::flushBuffer: stream freed prematurely!" << endl;
	}

	fclose(d_zDataStream);
	
	writeHeader();
	writeChunkInfo();
	writeZData();
	writeTrailer();
	
	fclose(d_dzStream);

	unlink(d_tmpFilename.c_str());
}

void DzOstreamBuf::flushBuffer()
{
	size_t size = pptr() - pbase();
	
	vector<unsigned char> zBuf(DZ_PREF_UNCOMPRESSED_SIZE);
	
	d_zStream.next_in = reinterpret_cast<unsigned char *>(pbase());
	d_zStream.avail_in = size;
	d_zStream.next_out = &zBuf[0];
	d_zStream.avail_out = DZ_PREF_UNCOMPRESSED_SIZE;
	
	if (deflate(&d_zStream, Z_FULL_FLUSH) != Z_OK)
		throw runtime_error(d_zStream.msg);		
	
	size_t zSize = DZ_PREF_UNCOMPRESSED_SIZE - d_zStream.avail_out;
	
	fwrite(&zBuf[0], 1, zSize, d_zDataStream);

	d_size += size;
	d_crc32 = crc32(d_crc32, reinterpret_cast<unsigned char *>(pbase()), size);
	d_chunks.push_back(DzChunk(0, zSize));

	pbump(-size);
}

int DzOstreamBuf::overflow(int c)
{
	flushBuffer();

	if (c != EOF)
	{
		*pptr() = c;
		pbump(1);
	}
	
	return c;
}

void DzOstreamBuf::writeChunkInfo()
{
	size_t xlen = 10 + (2 * d_chunks.size());
	fputc(xlen % 256, d_dzStream);
	fputc(xlen / 256, d_dzStream);
	
	fputc('R', d_dzStream);
	fputc('A', d_dzStream);
	
	// Length
	size_t len = 6 + (2 * d_chunks.size());
	fputc(len % 256, d_dzStream);
	fputc(len / 256, d_dzStream);
	
	// Version
	fputc(1, d_dzStream);
	fputc(0, d_dzStream);
	
	// Uncompressed chunk length
	fputc(DZ_PREF_UNCOMPRESSED_SIZE % 256, d_dzStream);
	fputc(DZ_PREF_UNCOMPRESSED_SIZE / 256, d_dzStream);
	
	// Chunk count
	fputc(d_chunks.size() % 256, d_dzStream);
	fputc(d_chunks.size() / 256, d_dzStream);
	
	// Put chunk information
	for (vector<DzChunk>::const_iterator iter = d_chunks.begin();
		iter != d_chunks.end(); ++iter)
	{
		fputc(iter->size % 256, d_dzStream);
		fputc(iter->size / 256, d_dzStream);
	}
}

void DzOstreamBuf::writeHeader()
{
	vector<unsigned char> header(GZ_HEADER_SIZE);

	// Get the current time. gzip only allows for 32-bit timestamps.
	struct timeval tv;
	if (gettimeofday(&tv, NULL) == -1 ||
			tv.tv_sec > numeric_limits<int32_t>::max())
		tv.tv_sec = 0;

	header[GZ_HEADER_ID1] = gzipId1;
	header[GZ_HEADER_ID2] = gzipId2;
	header[GZ_HEADER_CM] = GZ_CM_DEFLATE;
	header[GZ_HEADER_FLG] = GZ_FLG_EXTRA;
	writeToBuf<uint32_t>(&header[0] + GZ_HEADER_MTIME, tv.tv_sec);
	header[GZ_HEADER_XFL] = GZ_XFL_MAX;
	header[GZ_HEADER_OS] = GZ_OS_UNIX;
	
	fwrite(&header[0], 1, GZ_HEADER_SIZE, d_dzStream);
}

void DzOstreamBuf::writeTrailer()
{
	vector<unsigned char> trailer(GZ_TRAILER_SIZE);
	
	writeToBuf<uint32_t>(&trailer[0] + GZ_TRAILER_CRC32, d_crc32);
	writeToBuf<uint32_t>(&trailer[0] + GZ_TRAILER_ISIZE, d_size);
		
	fwrite(&trailer[0], 1, GZ_TRAILER_SIZE, d_dzStream);
}

void DzOstreamBuf::writeZData()
{
	FILE *chunkStream = fopen(d_tmpFilename.c_str(), "r");

	size_t const BUFSIZE = 0xffff;
	char buf[BUFSIZE];
	size_t n;
	
	while ((n = fread(buf, 1, BUFSIZE, chunkStream)))
		fwrite(buf, 1, n, d_dzStream);
	
	fclose(chunkStream);
}

streamsize DzOstreamBuf::xsputn(char const *s, streamsize n)
{
	int nWritten = 0;
	
	while (n) {
		int avail = epptr() - pptr();

		// If the buffer is full, flush it.
		if (avail == 0) {
			flushBuffer();
			avail = epptr() - pptr();
		}

		if (avail > n)
			avail = n;
		
		// Copy data, move pointer.
		memcpy(pptr(), s + nWritten, avail);
		pbump(avail);
		
		nWritten += avail;
		n -= avail;
	}
	
	return nWritten;
}


