
// createlex.cc

/*

This program creates a set of data files (lexicons) that are used in creating
suffix lexicon automata; automata that are used in deciding which tags could
be assigned to a word based on its suffix (this is done through fsa_guess).

input : complete lexicon
output: 3 lexicons, WITHOUT multi-word tags:

        - complete lexicon
	- lexicon of words starting with capital letter
	- lexicon of words not starting with capital letter
	
usage : createlex <input> <output_all> <output_cap> <output_noncap>

*/

#include <stdlib.h>
#include <stdio.h>     // popen() and pclose()
#include <ctype.h>     // isupper()
#include <sstream>     // ostringstream

#include <iostream>
#include <fstream>
#include <string>
#include <ctype.h>

using namespace std;


int main(int argc, char *argv[])
{  
  if(argc!=5){
    cerr << "usage: guess input_lexicon output_lexicon_all output_lexicon_cap output_lexicon_noncap" << endl;
    exit(1);
  }

  ifstream 
    INPUT;

  INPUT.open(argv[1]);

  if(!INPUT){
    cerr << "Error opening file " << argv[1] << endl;
    exit(1);
  }

  ofstream
    OUTPUT_ALL,
    OUTPUT_CAPS,
    OUTPUT_NONCAPS;

  OUTPUT_ALL.open(argv[2]);
  OUTPUT_CAPS.open(argv[3]);
  OUTPUT_NONCAPS.open(argv[4]);

  if(!OUTPUT_ALL){
    cerr << "Error creating file " << argv[2] << endl;
    INPUT.close();
    exit(1);
  }
  if(!OUTPUT_CAPS){
    cerr << "Error creating file " << argv[3] << endl;
    INPUT.close();
    exit(1);
  }

  if(!OUTPUT_NONCAPS){
    cerr << "Error creating file " << argv[4] << endl;
    OUTPUT_CAPS.close();
    INPUT.close();
    exit(1);
  }

  string
    line;
 
  getline(INPUT,line);

  while(!INPUT.eof()){
    // check for multi-word tag...
    if(line.find('^')==string::npos){
      OUTPUT_ALL << line << endl;
      if(isupper(line[0]))
	OUTPUT_CAPS << line << endl;
      else
	OUTPUT_NONCAPS << line << endl;
    }
    getline(INPUT,line);
  }

  INPUT.close(); 
  OUTPUT_ALL.close();
  OUTPUT_CAPS.close();
  OUTPUT_NONCAPS.close();
}
