#include <IndexedCorpus/ActCorpusReader.hh>
#include <libgen.h>

using namespace std;
using namespace indexedcorpus;


int main(int argc, char **argv)
{
  if (argc == 2) {

    int offset=1;
    if (string(basename(argv[0])) == string("dtprev")) {
      offset=-1;
    }

    ActCorpusReader reader;
    cout << reader.pathName(argv[1],offset) << endl;
  } else {
    cerr << "Usage: " << argv[0] << " PathName" << endl;
  }
}
