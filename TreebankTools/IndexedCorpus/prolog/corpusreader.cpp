#include <cstring>
#include <exception>
#include <vector>

#include <sicstus/sicstus.h>

#include <IndexedCorpus/ActCorpusReader.hh>
#include <IndexedCorpus/util/textfile.hh>

using namespace std;
using namespace indexedcorpus;

static ActCorpusReader *actCorpusReader = 0;

namespace {
string get_pathname(char const *path, int offset)
{
  string newPath;

  try {
    newPath = actCorpusReader->pathName(path, offset);
  } catch (...) {
    SP_fail();
  }

  return newPath;
}
}

extern "C" {

void init_func(int when)
{
  actCorpusReader = new ActCorpusReader;
}

void deinit_func(int when)
{
  if (actCorpusReader)
    delete actCorpusReader;
  actCorpusReader = 0;
}

/* Raise a Sicstus exception. */
void corpusreader_raise_exception(char const *message)
{
  SP_term_ref m = SP_new_term_ref();
  SP_put_string(m, message);
  SP_raise_exception(m);
}

SP_term_ref corpusreader_buf_to_list(unsigned char *buf, size_t len)
{
  // Zero-terminated.
  unsigned char *zbuf = new unsigned char[len + 1];
  memcpy(zbuf,buf,len);
  zbuf[len] = 0;

  // Convert to a list of characters.
  SP_term_ref bList = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();
  int r = SP_put_list_chars(bList, tail, reinterpret_cast<char *>(zbuf));

  delete[] zbuf;

  if (!r)
    corpusreader_raise_exception("Error in corpusreader_buf_to_list: could not convert buffer!");

  return bList;
}

SP_term_ref get_data_f(char const *path)
{
  vector<unsigned char> data;

  try {
    data = actCorpusReader->getData(path);
  } catch (...) {
    SP_fail();
    return SP_new_term_ref();
  }

  return corpusreader_buf_to_list(&data[0], data.size());
}

char const *pathname_before_f(char const *path)
{
  string pathName = get_pathname(path, -1);
  return strdup(pathName.c_str());
}


char const *pathname_after_f(char const *path)
{
  string pathName = get_pathname(path, 1);
  return strdup(pathName.c_str());
}

SP_term_ref file_to_codes_f(char const *filename)
{
  vector<unsigned char> data;
  try {
    data = readFile(filename);
  } catch (...) {
    SP_fail();
    return SP_new_term_ref();
  }

  return corpusreader_buf_to_list(&data[0], data.size());
}

}
