#include <cstdlib>
#include <fstream>
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/config.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>

#if defined(BOOST_HAS_THREADS)
#include <boost/thread.hpp>
#endif

#include <sicstus/sicstus.h>

#include "IndexedCorpus/IndexedCorpusWriter.hh"
#include "IndexedCorpus/IndexedCorpusReader.hh"

using namespace std;
using namespace indexedcorpus;

typedef int Handle;

Handle writeHandle = 0;
Handle readHandle = 0;

// Keep handles for open corpora.
std::map<Handle, IndexedCorpusWriter> writeHandles;
std::map<Handle, IndexedCorpusReader> readHandles;

// Mutexes for map access
#if defined(BOOST_HAS_THREADS)
boost::mutex writeHandlesMutex;
boost::mutex readHandlesMutex;
#endif

// Raise a Sicstus exception.
void derivbank_raise_exception(char const *message)
{
  SP_term_ref m = SP_new_term_ref();
  SP_put_string(m, message);
  SP_raise_exception(m);
}

extern "C" {

/* Get the length of a Prolog list. Returns 0 if the provided term is not
 * a list. */
int derivbank_list_length(SP_term_ref list, size_t *len)
{
  if (!SP_is_list(list))
      return 0;

  SP_term_ref head = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();
  SP_term_ref tmp = list;

  /* Chop off heads until the tail is empty */
  *len = 0;
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS) {
    ++(*len);
    tmp = tail;
  }

  return 1;
}

SP_term_ref derivbank_buf_to_list(unsigned char *buf, size_t len)
{
  SP_term_ref bList = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();
  int r = SP_put_list_n_bytes(bList, tail, len, buf);
  if (!r)
    derivbank_raise_exception("Error in derivbank_buf_to_list: could not convert buffer!");

  return bList;
}

int derivbank_list_to_buf(SP_term_ref data, unsigned char *cData, size_t n)
{
  size_t w;
  SP_term_ref tail = SP_new_term_ref();
  SP_get_list_n_bytes(data, tail, n, &w, cData);

  if (w != n) {
    return 0;
  }

  return 1;
}

SP_term_ref string_vector_to_list(vector<string> const &v)
{
  SP_term_ref tail = SP_new_term_ref();

  for (vector<string>::const_reverse_iterator iter = v.rbegin();
       iter != v.rend(); ++iter)
  {
    SP_term_ref term = SP_new_term_ref();
    SP_term_ref head = SP_new_term_ref();
    // XXX return
    SP_put_string(head, SP_from_os(iter->c_str(), 0));
    SP_cons_list(term, head, tail);
    tail = term;
  }

  return tail;
}

int openIndexedCorpusReader(char const *cBase)
{
  string base(cBase);

  string dataFilename(base + ".deriv");
  boost::shared_ptr<ifstream> dataStream(new ifstream(dataFilename.c_str()));
  if (!*dataStream) {
    derivbank_raise_exception("openIndexedCorpusReader: could not derivation treebank data file for reading!");
    return -1;
  }

  string indexFilename(base + ".index");
  boost::shared_ptr<ifstream> indexStream(new ifstream(indexFilename.c_str()));
  if (!*indexStream) {
    derivbank_raise_exception("openIndexedCorpusReader: could not derivation treebank index file for reading!");
    return -1;
  }

  Handle h;
  {
#if defined(BOOST_HAS_THREADS)
    boost::mutex::scoped_lock lock(readHandlesMutex);
#endif

    readHandles[readHandle] = IndexedCorpusReader(dataStream, indexStream);
    h = readHandle++;
  }

  return h;
}

int openIndexedCorpusWriter(char const *cBase)
{
  string base(cBase);

  string dataFilename(base + ".deriv");
  boost::shared_ptr<ofstream> dataStream(new ofstream(dataFilename.c_str()));
  if (!*dataStream) {
    derivbank_raise_exception("openIndexedCorpusReader: could not derivation treebank data file for writing!");
    return -1;
  }
  string indexFilename(base + ".index");
  boost::shared_ptr<ofstream> indexStream(new ofstream(indexFilename.c_str()));
  if (!*indexStream) {
    derivbank_raise_exception("openIndexedCorpusReader: could not derivation treebank index file for writing!");
    return -1;
  }

  Handle h;
  {
#if defined(BOOST_HAS_THREADS)
    boost::mutex::scoped_lock lock(writeHandlesMutex);
#endif

    writeHandles[writeHandle] = IndexedCorpusWriter(dataStream, indexStream);
    h = writeHandle++;
  }

  return h;
}

void closeIndexedCorpusReader(Handle handle)
{
  map<Handle, IndexedCorpusReader>::iterator iter = readHandles.find(handle);
  if (iter == readHandles.end())
    derivbank_raise_exception("closeIndexedCorpusReader: attempting to close unknown handle!");
  else {
#if defined(BOOST_HAS_THREADS)
    boost::mutex::scoped_lock lock(readHandlesMutex);
#endif
    readHandles.erase(handle);
  }
}

void closeIndexedCorpusWriter(Handle handle)
{
  map<Handle, IndexedCorpusWriter>::iterator iter = writeHandles.find(handle);
  if (iter == writeHandles.end())
    derivbank_raise_exception("closeIndexedCorpusWriter: attempting to close unknown handle!");
  else {
#if defined(BOOST_HAS_THREADS)
    boost::mutex::scoped_lock lock(writeHandlesMutex);
#endif
    writeHandles.erase(handle);
  }
}

void indexedCorpusWrite(Handle handle, char const *name, SP_term_ref data)
{
  size_t dataLen;
  int r = derivbank_list_length(data, &dataLen);
  if (!r) {
    derivbank_raise_exception("Error in indexedCorpusWrite: argument is not a valid list!");
    return;
  }

  /* Convert byte list to a buffer */
  unsigned char *cData = new unsigned char[dataLen];
  r = derivbank_list_to_buf(data, cData, dataLen);
  if (!r) {
    derivbank_raise_exception("Error in indexedCorpusWrite: could not convert list!");
    return;
  }

  map<Handle, IndexedCorpusWriter>::iterator iter = writeHandles.find(handle);
  if (iter == writeHandles.end())
    derivbank_raise_exception("indexedCorpusWrite: attempting to write to unknown handle!");
  else
    iter->second.write(name, reinterpret_cast<char const *>(cData), dataLen);

  delete cData;
}

SP_term_ref indexedCorpusRead(Handle handle, char const *name)
{
  map<Handle, IndexedCorpusReader>::iterator iter = readHandles.find(handle);
  if (iter == readHandles.end())
    derivbank_raise_exception("indexedCorpusRead: attempting to read from unknown handle!");

  vector<unsigned char> data;
  try {
    data = iter->second.read(name);
  } catch (exception &e) {
    derivbank_raise_exception(e.what());
    return SP_new_term_ref();
  }
  // 2003 update of the C++ standard guarantees consecutive memory.
  SP_term_ref dataList = derivbank_buf_to_list(&data[0], data.size());

  size_t len;
  derivbank_list_length(dataList, &len);

  return dataList;
}

SP_term_ref indexedCorpusEntries(Handle handle)
{
  SP_term_ref entryList = SP_new_term_ref();

  map<Handle, IndexedCorpusReader>::iterator iter = readHandles.find(handle);
  if (iter == readHandles.end()) {
    derivbank_raise_exception("indexedCorpusEntries: attempting to from unknown handle!");
    return entryList;
  }

  vector<string> entries(iter->second.entries());

  entryList = string_vector_to_list(entries);

  return entryList;
}

}
