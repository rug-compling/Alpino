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
  tag4gram_hash;

int pos_init_fourgram(char *fourgramFile)
{
  ifstream
    input_fourgram(fourgramFile,ios::in);

  if(!(input_fourgram)) {
    cerr << "error opening file " << fourgramFile << endl;
    exit(-1);
  }

  string line;
  int division;
  
  while(getline(input_fourgram,line)) {
    division = line.rfind('|');
    string const
      fourgram(line,0,division),
      prob(line,division+1,line.size()-(division+1));  
    tag4gram_hash[fourgram.c_str()] = atoi(prob.c_str());
  }
  input_fourgram.close();  
}

int main(int argc, char *argv[])
{	

  if (argc < 1) {
    cerr << "Usage: "<< argv[0] << " file" << endl;
    return(1);
  }

  char file[255];
  strcpy(file,argv[1]);
  pos_init_fourgram(file);
  cerr << "loaded file " << file << endl;

  while(1)
    ;

}
