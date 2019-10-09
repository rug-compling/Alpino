#include <string>
#include <fstream.h>
#include <hash_map>

/////////////////////////////////////
// code for using hash tables
// FBBrokken C++ Annotations 4.4.2
/////////////////////////////////////

template <> class hash<string>
{
public:
	size_t operator()(string const &str) const
	{
		hash<char const *> h;
		return (h(str.c_str()));
	}
};	

/////////////
// GLOBALS
/////////////

hash_map<string,double,hash<string> >
//  tag4gram_hash,
//  tag3gram_hash,
  tag2gram_hash;

int pos_init_bigram(char *bigramFile)
{
  ifstream
    input_bigram(bigramFile,ios::in);

  if(!(input_bigram)) {
    cerr << "error opening file " << bigramFile << endl;
    exit(-1);
  }

  string line;
  int division;
  
  while(getline(input_bigram,line)) {
    division = line.rfind('|');
    string const
      bigram(line,0,division),
      prob(line,division+1,line.size()-(division+1)); 
    tag2gram_hash[bigram.c_str()] = atof(prob.c_str());
  }
  input_bigram.close();  
}

int main(int argc, char *argv[])
{	

  if (argc < 1) {
    cerr << "Usage: "<< argv[0] << " file" << endl;
    return(1);
  }

  char file[255];
  strcpy(file,argv[1]);
  pos_init_bigram(file);
  cerr << "loaded file " << file << endl;

  while(1)
    ;

}
