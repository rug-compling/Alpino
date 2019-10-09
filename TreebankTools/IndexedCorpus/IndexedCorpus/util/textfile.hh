#ifndef TEXT_FILE_HH
#define TEXT_FILE_HH

#include <string>
#include <vector>

namespace indexedcorpus {

std::vector<unsigned char> readFile(std::string const &filename);

}

#endif // TEXT_FILE_HH
