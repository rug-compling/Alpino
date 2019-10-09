#include <algorithm>
#include <climits>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <vector>

using namespace std;

struct VectorBuilder
{
  void operator()(size_t i);
  vector<char> data;
};

void VectorBuilder::operator()(size_t i)
{
  size_t index = i / CHAR_BIT;
  size_t bit = i % CHAR_BIT;

  if (data.size() < index + 1)
    data.resize(index + 1);

  data[index] |= (1 << bit);
}

int main(int argc, char *argv[])
{
  VectorBuilder builder;

  builder = for_each(istream_iterator<size_t>(cin), istream_iterator<size_t>(),
	   builder);

  if (!cin.eof())
    throw runtime_error("Could not correctly read data!");
  
  cout << "char guideVector[] = {";
  copy(builder.data.begin(), builder.data.end(),
       ostream_iterator<int>(cout,",\n"));
  cout << "};" << endl;
  cout << "int guideSize = " << builder.data.size() << ";" << endl << endl;
}
