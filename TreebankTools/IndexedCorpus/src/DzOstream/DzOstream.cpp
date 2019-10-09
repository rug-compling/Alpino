#include "DzOstream.ih"

DzOstream::DzOstream(char const *filename) : std::ostream(0)
{
	d_streamBuf.reset(new DzOstreamBuf(filename));
	rdbuf(d_streamBuf.get());
}
