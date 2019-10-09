#include "DzIstreamBuf.ih"

DzIstreamBuf::DzIstreamBuf(char const *filename)
{
	d_stream = fopen(filename, "r");
	
	if (d_stream == NULL)
	{
		d_curChunk = 0;

		unsigned char *buffer = &d_buffer[0];
		setg(reinterpret_cast<char *>(buffer),
			reinterpret_cast<char *>(buffer),
			reinterpret_cast<char *>(buffer));
		
		throw runtime_error("DzIstreamBuf::DzIstreamBuf Could not open input stream!");
	}
	else {
		readHeader();
		readExtra();
		skipOptional();
		d_dataOffset = ftell(d_stream);
		d_curChunk = -1;
	}
}

DzIstreamBuf::~DzIstreamBuf()
{
	if (d_stream != NULL)
		fclose(d_stream);
}

DzIstreamBuf::pos_type DzIstreamBuf::seekpos(pos_type off, openmode mode)
{
	return seekoff(off, std::ios::beg, mode);
}

void DzIstreamBuf::readChunk(long n)
{
	if (n == d_curChunk)
		return;

	DzChunk chunkN = d_chunks[n];

	unsigned char *zBuf = new unsigned char[chunkN.size];
	
	fseek(d_stream, d_dataOffset + chunkN.offset, SEEK_SET);
	size_t sizeRead = fread(zBuf, 1, chunkN.size, d_stream);
	if (sizeRead != chunkN.size) {
	  throw runtime_error("did not read enough data in readChunk!");
	}

	z_stream zStream;
	zStream.next_in = zBuf;
	zStream.avail_in = chunkN.size;
	zStream.next_out = &d_buffer[0];
	zStream.avail_out = d_chunkLen;
	zStream.zalloc = NULL;
	zStream.zfree = NULL;

	if (inflateInit2(&zStream, -15) != Z_OK)
	{
		delete[] zBuf;
		throw runtime_error(zStream.msg);
	}

	int r = inflate(&zStream, Z_PARTIAL_FLUSH);
	if (r != Z_OK && r != Z_STREAM_END)
	{
		delete[] zBuf;
		throw runtime_error(zStream.msg);
	}
	
	delete[] zBuf;

	if (inflateEnd(&zStream) != Z_OK)
		throw runtime_error(zStream.msg);

	unsigned char *buffer = &d_buffer[0];
	setg(reinterpret_cast<char *>(buffer),
		reinterpret_cast<char *>(buffer),
		reinterpret_cast<char *>(buffer) + zStream.total_out);
	
	d_curChunk = n;
}

void DzIstreamBuf::readExtra()
{
	int extraLen = fgetc(d_stream) + (fgetc(d_stream) * 256);
	
	long extraPos = ftell(d_stream);
	long nextField = extraPos + extraLen;

	while (ftell(d_stream) < nextField)
	{
		// Read extra field 'header'
		char si[2];
		size_t sizeRead = fread(si, 1, 2, d_stream);
		if (sizeRead != 2) {
		  throw runtime_error("did not read enough data in readExtra!");
		}


		int len = fgetc(d_stream) + (fgetc(d_stream) * 256);
		
		// Does this field part provide chunk information?
		if (si[0] == 'R' && si[1] == 'A')
		{
			int ver = fgetc(d_stream) + (fgetc(d_stream) * 256);
			if (ver != 1)
				throw("DzIstreamBuf::readExtra: unknown dictzip version!");
			
			d_chunkLen = fgetc(d_stream) + (fgetc(d_stream) * 256);
			size_t chunkCount = fgetc(d_stream) + (fgetc(d_stream) * 256);
			
			// We can set up a read buffer now...
			d_buffer.resize(d_chunkLen);
			unsigned char *buffer = &d_buffer[0];
			setg(reinterpret_cast<char *>(buffer),
				reinterpret_cast<char *>(buffer) + d_chunkLen,
				reinterpret_cast<char *>(buffer) + d_chunkLen);
			
			size_t chunkPos = 0;
			for (size_t i = 0; i < chunkCount; ++i)
			{
				size_t chunkLen = fgetc(d_stream) + (fgetc(d_stream) * 256);
				d_chunks.push_back(DzChunk(chunkPos, chunkLen));	
				chunkPos += chunkLen;
			}
		}
		else
			fseek(d_stream, len, SEEK_CUR);
	}
}

void DzIstreamBuf::readHeader()
{
	d_header.resize(GZ_HEADER_SIZE);
	unsigned char *header = &d_header[0];
	size_t sizeRead = fread(header, 1, GZ_HEADER_SIZE, d_stream);
	if (sizeRead != GZ_HEADER_SIZE) {
	  throw runtime_error("did not read enough data in readHeader!");
	}

	
	if (header[GZ_HEADER_ID1] != gzipId1 || header[GZ_HEADER_ID2] != gzipId2)
		throw runtime_error("DzIstreamBuf::readHeader: not a gzip file!");
	
	if (header[GZ_HEADER_CM] != GZ_CM_DEFLATE)
		throw runtime_error("DzIstreamBuf::readHeader: unknown compression method!");
	
	if (!(header[GZ_HEADER_FLG] & GZ_FLG_EXTRA))
		throw runtime_error("DzIstreamBuf::readHeader: no extra fields, cannot be a dictzip file!");
}

void DzIstreamBuf::skipOptional()
{
	unsigned char *header = &d_header[0];
	
	if (header[GZ_HEADER_FLG] & GZ_FLG_NAME)
		while (fgetc(d_stream) != 0) {}
		
	if (header[GZ_HEADER_FLG] & GZ_FLG_COMMENT)
		while (fgetc(d_stream) != 0) {}
	
	if (header[GZ_HEADER_FLG] & GZ_FLG_HCRC)
		fseek(d_stream, 2, SEEK_CUR);
}

int DzIstreamBuf::underflow()
{
	if (gptr() < egptr())
		return *gptr();

	if (d_curChunk + 1>= static_cast<int>(d_chunks.size()))
		return EOF;

	readChunk(d_curChunk + 1);
	
	return *gptr();
}

DzIstreamBuf::pos_type DzIstreamBuf::seekoff(off_type off, seekdir dir, openmode)
{
	pos_type targetPos;
	if (dir == std::ios::beg)
		targetPos = off;
	else if (dir == std::ios::cur)
	{
		pos_type curPos = (d_curChunk * d_chunkLen) + (gptr() - eback());
		targetPos = curPos + off;
	}
	else
		// XXX - We can only detmine the uncompressed file length by decompressing the
		// last chunk. Quite inefficient, haven't made my mind up whether we want to
		// support this.
		return EOF;

	int targetChunk = targetPos / d_chunkLen;
	int chunkPos = targetPos % d_chunkLen;
	
	if (targetPos < 0)
		return -1;
	
	readChunk(targetChunk);

	setg(eback(), eback() + chunkPos, egptr());
	
	return targetPos;
}


// From C++ annotations 8.1.0~pre1, chapter 23.
std::streamsize DzIstreamBuf::xsgetn(char *dest, streamsize n)
{
	int nread = 0;
	
	while (n)
	{
		if (!in_avail())
		{
			if (underflow() == EOF)
				break;
		}
		
		int avail = in_avail();
		
		if (avail > n)
			avail = n;
		
		memcpy(dest + nread, gptr(), avail);
		gbump(avail);
		
		nread += avail;
		n -= avail;
	}
	
	return nread;
}
