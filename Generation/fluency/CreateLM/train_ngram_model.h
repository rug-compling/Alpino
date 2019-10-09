#include <functional>
#include <vector>
#include <stdexcept>

#include <tr1/functional>

struct Lambdas
{
  Lambdas(double l1New, double l2New, double l3New) : l1(l1New), l2(l2New),
    l3(l3New) {}

  double l1;
  double l2;
  double l3;
};

template <typename T>
struct VectorHash : public std::unary_function<std::vector<T>, size_t>
{
  std::size_t operator()(std::vector<T> const &vec) const
  {
    std::tr1::hash<T> tHash;
    size_t seed = 0;

    for (typename std::vector<T>::const_iterator iter = vec.begin();
	 iter != vec.end(); ++iter)
      seed ^= tHash(*iter) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
 
    return seed;
  }
};

template <typename T>
T parseString(std::string const &str)
{
  std::istringstream iss(str);
  T val;
  iss >> val;
  
  if (!iss)
    throw std::invalid_argument("Error parsing option argument: " + str);

  return val;
}
