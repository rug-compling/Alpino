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

hash_map<string,int,hash<string> >
  tag3gram_hash;

int pos_init_trigram(char *trigramFile)
{
  ifstream
    input_trigram(trigramFile,ios::in);

  if(!(input_trigram)) {
    cerr << "error opening file " << trigramFile << endl;
    exit(-1);
  }

  string line;
  int division;
  
  while(getline(input_trigram,line)) {
    division = line.rfind('|');
    string const
      trigram(line,0,division),
      prob(line,division+1,line.size()-(division+1));  
    tag3gram_hash[trigram.c_str()] = atoi(prob.c_str());
  }
  input_trigram.close();  
}

int main(int argc, char *argv[])
{	

  if (argc < 1) {
    cerr << "Usage: "<< argv[0] << " file" << endl;
    return(1);
  }

  char file[255];
  strcpy(file,argv[1]);
  pos_init_trigram(file);
  cerr << "loaded file " << file << endl;

  while(1)
    ;

}
