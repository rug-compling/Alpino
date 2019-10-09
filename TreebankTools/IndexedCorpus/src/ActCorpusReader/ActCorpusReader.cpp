#include "ActCorpusReader.ih"

void ActCorpusReader::copy(ActCorpusReader const &other)
{
	d_lastDir = other.d_lastDir;
	d_lastDirEntries = other.d_lastDirEntries;
	d_lastCorpusPath = other.d_lastCorpusPath;
	d_lastCorpusReader = other.d_lastCorpusReader;
}

bool endsWith(string const &str, string const &end)
{
	size_t pos = str.rfind(end);
	
	if (pos == string::npos)
		return false;
	
	return (pos + end.size() == str.size());
}

string ActCorpusReader::stripCorpusExt(string const &name) const
{
	string noextName;
	if (endsWith(name, ACT_DATA_EXT))
		noextName = name.substr(0, name.size() - 8);
	else if (endsWith(name, ACT_INDEX_EXT))
		noextName = name.substr(0, name.size() - 6);
	else
		noextName = name;

	return noextName;
}

bool ActCorpusReader::dzCorpusExists(path const &name) const
{
	string noextName = stripCorpusExt(name.string());
	
	path dataPath(noextName + ACT_DATA_EXT);
	path indexPath(noextName + ACT_INDEX_EXT);

	return is_regular_file(dataPath) && is_regular_file(indexPath);
}

string ActCorpusReader::findEntry(vector<string> const &entries, string const &entry,
	int offset) const
{
	vector<string>::const_iterator iter = find(entries.begin(), entries.end(),
		entry);
	if (iter == entries.end())
		throw runtime_error("ActCorpusReader::findEntry: File name unknown!");
	
	iter += offset;
	
	if (iter < entries.begin() || iter >= entries.end())
		throw runtime_error("ActCorpusReader::findEntry: Offset points out of bounds!");
	
	return *iter;
}

vector<unsigned char> ActCorpusReader::getData(string const &pathName)
{
	path corpusPath(pathName);
	path filePart(corpusPath.filename());
	path dirPart(corpusPath.parent_path());

#if defined(BOOST_HAS_THREADS)
	boost::mutex::scoped_lock lock(d_readMutex);
#endif
	
	if (!dirPart.empty() && d_lastCorpusPath == dirPart.string())
		return d_lastCorpusReader.read(filePart.string());

	if (!corpusPath.has_parent_path())
		return readFile(pathName);
	
	vector<unsigned char> data;
	if (dzCorpusExists(dirPart))
		data = readFromCorpus(dirPart, filePart);
	else
		data = readFile(pathName);
	
	return data;
}

vector<string> ActCorpusReader::entries(string const &pathName)
{
	path corpusPath(pathName);

#if defined(BOOST_HAS_THREADS)
	boost::mutex::scoped_lock lock(d_readMutex);
#endif
	
	if (d_lastCorpusPath == corpusPath)
		return d_lastCorpusReader.entries();
	
	if (dzCorpusExists(corpusPath))
		return entriesCorpus(corpusPath);
	else
		return entriesDirectory(corpusPath);
}

vector<string> ActCorpusReader::entriesCorpus(path const &corpusPath)
{
	string noextName = stripCorpusExt(corpusPath.string());

	string dataFilename = noextName + ACT_DATA_EXT;
	path dataPath(dataFilename);
	if (!is_regular_file(dataPath))
		throw runtime_error(string("Data file is not a regular file: ") + dataFilename);
	
	boost::shared_ptr<istream> dataStream(new DzIstream(dataFilename.c_str()));
	if (!*dataStream)
		throw runtime_error(string("Could not open corpus data file for reading: ") + dataFilename);

	string indexFilename = noextName + ACT_INDEX_EXT;
	path indexPath(indexFilename);
	if (!is_regular_file(indexPath))
		throw runtime_error(string("Index file is not a regular file: ") + indexFilename);
	
	boost::shared_ptr<ifstream> indexStream(new ifstream(indexFilename.c_str()));
	if (!*indexStream)
		throw runtime_error(string("Could not open corpus index file for reading: ") + indexFilename);
	
	IndexedCorpusReader corpusReader(dataStream, indexStream);
	
	return corpusReader.entries();
}

vector<string> ActCorpusReader::entriesDirectory(path const &corpusPath)
{
	if (corpusPath.string() != d_lastDir)
	{
		d_lastDirEntries.clear();

		string pattern = corpusPath.string() + "/*.xml";

		glob_t g;
		if (glob(pattern.c_str(), GLOB_MARK, NULL, &g) != 0)
			throw runtime_error(string("Could not read directory entries from: ") + corpusPath.string());
	
		for (char **p = g.gl_pathv; p < g.gl_pathv + g.gl_pathc; ++p)
			d_lastDirEntries.push_back(IndexNamePair(*p));
		
		sort(d_lastDirEntries.begin(), d_lastDirEntries.end(), IndexNamePairCompare());

		globfree(&g);

		d_lastDir = corpusPath.string();
	}

	vector<string> entries;

	for (vector<IndexNamePair>::const_iterator iter = d_lastDirEntries.begin();
			iter != d_lastDirEntries.end(); ++iter)
		entries.push_back(iter->name);

	return entries;
}

string ActCorpusReader::pathName(string const &pathName, int offset)
{
	path corpusPath(pathName);
	path filePart(corpusPath.filename());
	path dirPart(corpusPath.parent_path());

#if defined(BOOST_HAS_THREADS)
	boost::mutex::scoped_lock lock(d_readMutex);
#endif
	
	if (!dirPart.empty() && d_lastCorpusPath == dirPart.string())
		return dirPart.string() + "/" +
			findEntry(d_lastCorpusReader.entries(), filePart.string(), offset);
	
	if (!corpusPath.has_parent_path())
		return pathNameDirectory(".", "path", offset);
	
	string newPath;
	if (dzCorpusExists(dirPart))
		newPath = pathNameCorpus(dirPart, filePart, offset);
	else
		newPath = pathNameDirectory(dirPart, filePart, offset);

	return newPath;
}

string ActCorpusReader::pathNameCorpus(path const &corpus,
	path const &filename, int offset)
{
	string noextName = stripCorpusExt(corpus.string());

	string dataFilename = noextName + ACT_DATA_EXT;
	path dataPath(dataFilename);
	if (!is_regular_file(dataPath))
		throw runtime_error(string("ActCorpusReader::pathNameCorpus: data file is not a regular file: ") + dataFilename);
	
	boost::shared_ptr<istream> dataStream(new DzIstream(dataFilename.c_str()));
	if (!*dataStream)
		throw runtime_error(string("Could not open corpus data file for reading: ") + dataFilename);

	string indexFilename = noextName + ACT_INDEX_EXT;
	path indexPath(indexFilename);
	if (!is_regular_file(indexPath))
		throw runtime_error(string("Index file is not a regular file: ") + indexFilename);
	
	boost::shared_ptr<ifstream> indexStream(new ifstream(indexFilename.c_str()));
	if (!*indexStream)
		throw runtime_error(string("Could not open corpus index file for reading: ") + indexFilename);
	
	IndexedCorpusReader corpusReader(dataStream, indexStream);
	
	vector<string> const &entries = corpusReader.entries();

	string found(findEntry(entries, filename.string(), offset));

	d_lastCorpusPath = corpus.string();
	d_lastCorpusReader = corpusReader;

	return corpus.string() + "/" + found;
}

string ActCorpusReader::pathNameDirectory(path const &directory,
	path const &filename, int offset)
{
	if (directory.string() != d_lastDir)
	{
		d_lastDirEntries.clear();

		string pattern = directory.string() + "/*.xml";

		glob_t g;
		if (glob(pattern.c_str(), GLOB_MARK, NULL, &g) != 0)
			throw runtime_error(string("Could not read directory entries from: ") + directory.string());
	
		for (char **p = g.gl_pathv; p < g.gl_pathv + g.gl_pathc; ++p)
			d_lastDirEntries.push_back(IndexNamePair(*p));
		
		sort(d_lastDirEntries.begin(), d_lastDirEntries.end(), IndexNamePairCompare());

		globfree(&g);

		d_lastDir = directory.string();
	}

	string path = directory.string() + "/" + filename.string();
	
	pair<vector<IndexNamePair>::const_iterator, vector<IndexNamePair>::const_iterator> found =
		equal_range(d_lastDirEntries.begin(), d_lastDirEntries.end(), path, IndexNamePairCompare());

	if (found.first == found.second)
		throw runtime_error(string("Unknown path!") + path);

	vector<IndexNamePair>::const_iterator iter = found.first;
	iter += offset;
	if (iter < d_lastDirEntries.begin() || iter >= d_lastDirEntries.end())
		throw runtime_error("Offset points out of bounds!");
	
	return iter->name;
}

vector<unsigned char> ActCorpusReader::readFromCorpus(path const &corpus,
	path const &file)
{
	string noextName = stripCorpusExt(corpus.string());

	string dataFilename = noextName + ACT_DATA_EXT;
	path dataPath(dataFilename);
	if (!is_regular_file(dataPath))
		throw runtime_error(string("Data file is not a regular file: ") + dataFilename);

	boost::shared_ptr<istream> dataStream(new DzIstream(dataFilename.c_str()));
	if (!*dataStream)
		throw runtime_error(string("Could not open corpus data file for reading: ") + dataFilename);

	string indexFilename = noextName + ACT_INDEX_EXT;
	path indexPath(indexFilename);
	if (!is_regular_file(indexPath))
		throw runtime_error(string("Index file is not a regular file: ") + indexFilename);

	boost::shared_ptr<ifstream> indexStream(new ifstream(indexFilename.c_str()));
	if (!*indexStream)
		throw runtime_error(string("Could not open corpus index file for reading: ") + indexFilename);
	
	IndexedCorpusReader corpusReader(dataStream, indexStream);
	
	d_lastCorpusPath = corpus.string();
	d_lastCorpusReader = corpusReader;
	
	return corpusReader.read(file.string());
}

