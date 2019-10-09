#ifndef INDEX_NAME_PAIR_HH
#define INDEX_NAME_PAIR_HH

#include <cctype>
#include <string>
#include <vector>

namespace indexedcorpus {

struct IndexNamePair
{
	IndexNamePair(std::string const &newName);
	IndexNamePair(std::vector<std::string> const &newIndex, std::string const &newName) :
		index(newIndex), name(newName) {}
	std::vector<std::string> index;
	std::string name;
};

struct IndexNamePairCompare
{
	bool operator()(IndexNamePair const &p1, IndexNamePair const &p2);
};

}

#endif // INDEX_NAME_PAIR
