#include "IndexNamePair.ih"

IndexNamePair::IndexNamePair(string const &newName) : name(newName)
{
	if (name.size() == 0)
		return;

	ostringstream buf;
	
	bool prevIsDigit = isdigit(name[0]);
	for (string::const_iterator iter = name.begin(); iter != name.end();
		++iter)
	{
		bool curIsDigit = isdigit(*iter);

		if (curIsDigit && prevIsDigit)
			buf << *iter;
		else
		{
			index.push_back(buf.str());
			buf.str("");
			buf << *iter;
		}
		
		prevIsDigit = curIsDigit;
	}
	
	// Leftover
	index.push_back(buf.str());
}

bool IndexNamePairCompare::operator()(IndexNamePair const &p1, IndexNamePair const &p2)
{
	vector<string> const &i1 = p1.index;
	vector<string> const &i2 = p2.index;
	
	for (size_t i = 0; i < i1.size() && i < i2.size(); ++i)
	{
		// Both digits? Sort on digits!
		if (isdigit(i1[i][0]) && isdigit(i2[i][0]))
		{
			InfInt d1 = parseString<InfInt>(i1[i]);
			InfInt d2 = parseString<InfInt>(i2[i]);
			
			if (d1 != d2)
				return d1 < d2;
		}
		else
			if (i1[i] != i2[i])
				return i1[i] < i2[i];
	}
	
	// The pairs are equal...
	return false;
}
