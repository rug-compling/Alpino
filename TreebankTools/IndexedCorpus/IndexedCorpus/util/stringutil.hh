/*
 * Copyright (c) 2009 Daniël de Kok
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#ifndef STRINGUTIL_HH
#define STRINGUTIL_HH

#include <algorithm>
#include <iterator>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace indexedcorpus {

/**
 * Parse a string.
 * @The string to be parsed.
 */
template <typename T>
T parseString(std::string const &str)
{
	std::istringstream iss(str);
	T val;
	iss >> val;

	if (!iss)
		throw std::invalid_argument("Error parsing: " + 
str);

	return val;
}

/**
 * Split a string on whitespace.
 * @str The string to be split.
 */
inline std::vector<std::string> stringSplit(std::string str)
{
	std::istringstream iss(str);
	std::vector<std::string> strParts;
	copy(std::istream_iterator<std::string>(iss), std::istream_iterator<std::string>(),
		back_inserter(strParts));

	return strParts;
}

}

#endif // STRINGUTIL_HH

