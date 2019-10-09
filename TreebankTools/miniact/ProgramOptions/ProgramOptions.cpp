#include "ProgramOptions.ih"

ProgramOptions::ProgramOptions(int argc, char const *argv[], char const *optString)
	: d_programName(argv[0])
{
	opterr = 0;
	
	int opt;
	while ((opt = getopt(argc, const_cast<char * const *>(argv), optString)) != -1)
	{
		if (opt == '?' || opt == '|')
			throw runtime_error(string("Unknown option or missing argument: ") +
				static_cast<char>(optopt));
		
		d_options.insert(opt);
		
		if (optarg != 0)
			d_optionValues[opt] = optarg;
	}
	
	copy(argv + optind, argv + argc, back_inserter(d_arguments));
}

std::string const &ProgramOptions::optionValue(char option) const
{
	map<char, std::string>::const_iterator iter = d_optionValues.find(option);
	if (iter == d_optionValues.end())
		throw runtime_error("Tried to extract the value for an option without argument.");
	return iter->second;
}
