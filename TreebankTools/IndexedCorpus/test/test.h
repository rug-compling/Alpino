#include <iostream>
#include <string>

template <typename T>
void testEqual(T v1, T v2, std::string const &message)
{
	std::cout << message << "... ";
	if (v1 == v2)
		std::cout << "OK" << std::endl;
	else
		std::cout << "Fail!" << std::endl;
}