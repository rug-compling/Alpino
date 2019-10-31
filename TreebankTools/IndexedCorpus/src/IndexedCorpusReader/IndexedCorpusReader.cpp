#include "IndexedCorpusReader.ih"

IndexedCorpusReader::IndexedCorpusReader(IstreamPtr dataStream,
		IstreamPtr indexStream) :
	d_dataStream(dataStream)
{
	// Read indices
	string line;
	while(getline(*indexStream, line))
	{
		istringstream iss(line);
		
		string name;
		std::getline(iss, name, '\t');
		
		string offset64;
		std::getline(iss, offset64, '\t');
		size_t offset = b64_decode<size_t>(offset64);

		string size64;
		std::getline(iss, size64);
		size_t size = b64_decode<size_t>(size64);

		IndexItemPtr item(new IndexItem(name, offset, size));
		d_indices.push_back(item);
		d_namedIndices[name] = item;
	}	
}

IndexedCorpusReader &IndexedCorpusReader::operator=(IndexedCorpusReader const &other)
{
	if (this != &other)
	{
		destroy();
		copy(other);
	}
	
	return *this;
}

void IndexedCorpusReader::copy(IndexedCorpusReader const &other)
{
	d_dataStream = other.d_dataStream;
	d_namedIndices = other.d_namedIndices;
	d_indices = other.d_indices;
}

void IndexedCorpusReader::destroy()
{
	d_namedIndices.clear();
	d_indices.clear();	
	d_dataStream.reset();
}

std::vector<std::string> IndexedCorpusReader::entries() const
{
	vector<string> entries;
	
	for (vector<IndexItemPtr>::const_iterator iter = d_indices.begin();
			iter != d_indices.end(); ++iter)
		entries.push_back((*iter)->name);
	
	return entries;
}

vector<unsigned char> IndexedCorpusReader::read(string const &filename)
{
	map<string, IndexItemPtr>::const_iterator iter = d_namedIndices.find(filename);
	if (iter == d_namedIndices.end())
		throw runtime_error(string("Unknown entry: ") + filename);

	vector<unsigned char> data(iter->second->size);
	{
	 	lock_guard<mutex> lock(d_readMutex);
		
		// Read data. We rely on the fact that vector elements have to
		// be consecutive according to the 2003 standard update.
		d_dataStream->seekg(iter->second->offset, std::ios::beg);
		d_dataStream->read(reinterpret_cast<char *>(&data[0]), iter->second->size);
	}

	return data;
}
