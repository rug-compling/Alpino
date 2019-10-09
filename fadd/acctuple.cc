/***	acctuple.cc	***/

/*	Copyright (c) Jan Daciuk, for Alpino grammar, RuG, 2001	*/

#include	<iostream>
#include	<fstream>
#include	<string>
#include	<stdlib.h>
#include	<new.h>
#include	<ctype.h>
/* STL shit removed - I had no usable docs on it
#include	<set>
*/
#ifdef DMALLOC
#include	"dmalloc.h"
#endif


/* STL shit removed - I had no usable docs on it
using namespace std;
*/

int	HASH_SIZE = 2340047;

class tuple {
public:
  int		word_dicts;	// number of words in the tuple
  int		num_dicts;	// number of numbers in the tuple
  int		string_size;	// size of combined number of all words
  int		tuple_size;	// size of one tuple
  int		dict_size;	// size of the whole tuple dictionary
  int		orig_wdicts;	// original number of words in a tuple
  unsigned char	*sizes;		// sizes of each word number and number
  unsigned char *orig_sizes;	// original sizes
  const char	*signs;		// bit vector whether a column can be negative
  const char	*tuples;	// the tuples themselves
  int		*dicts;		// dictionary numbers for words in tuples
};

class tuple_node;

class transition {
protected:
  int		label;
  tuple_node	*target;
  int		tuple_nr;
public:
  transition(const int l, tuple_node *t);
  transition(void) { label = 0; target = NULL; tuple_nr = 0;}
  tuple_node *get_target(void) const { return target; }
  int get_label(void) const { return label; }
  const int get_tuple_nr(void) const { return tuple_nr; }
  void set_tuple_nr(const int n) { tuple_nr = n; }
};

class tuple_node {
protected:
  int		no_trans;	// number of transitions
  int		tuples_before;	// number of tuples before this one
  int		tuple_nr;	// address in a table
  transition	*outgoing;	// outgoing transitions
public:
  tuple_node(void) {
    no_trans = 0; tuples_before = 0; outgoing = NULL; tuple_nr = -1;
  }
  int get_no_trans(void) const { return no_trans; }
  transition *get_trans(void) const { return outgoing; }
  int get_tuples_before(void) const { return tuples_before; }
  int get_tuple_nr(void) const { return tuple_nr; }
  void set_tuple_nr(const int n) { tuple_nr = n; }
  tuple_node &add_trans(transition *t);
  int get_smallest(void) {
    int smallest = outgoing[0].get_label();
    for (int i = 1; i < no_trans; i++) {
      if (outgoing[i].get_label() < smallest)
	smallest = outgoing[i].get_label();
    }
    return smallest;
  }
  int get_biggest(void) {
    int biggest = outgoing[0].get_label();
    for (int i = 0; i < no_trans; i++) {
      if (outgoing[i].get_label() > biggest)
	biggest = outgoing[i].get_label();
    }
    return biggest;
  }
};

class reg_struct {
protected:
  int			level;
  const tuple_node	*node_ptr;
public:
  reg_struct(void) : node_ptr(NULL) {}
  reg_struct(const int l, const tuple_node *t) : level(l), node_ptr(t) {}
  tuple_node *get_node(void) const { return (tuple_node *)node_ptr; }
  int get_level(void) const { return level; }
  int operator <(const reg_struct &t) {
    if (level == t.level) {
      if (node_ptr->get_no_trans() == t.get_node()->get_no_trans()) {
	transition *an1 = node_ptr->get_trans();
	transition *an2 = t.get_node()->get_trans();
	for (int i = 0; i < node_ptr->get_no_trans(); i++) {
	  if (an1->get_label() == an2->get_label()) {
	    if (an1->get_target() != an2->get_target()) {
	      return an1->get_target() < an2->get_target() ? 1 : 0;
	    }
	  }
	  else {
	    return an1->get_label() < an2->get_label() ? 1 : 0;
	  }
	}
	return 0;
      }
      else {
	return node_ptr->get_no_trans() < t.get_node()->get_no_trans() ? 1 : 0;
      }
    }
    else {
      return level < t.level ? 1 : 0;
    }
  }
  int less(const reg_struct &t)  { return *this < t; }
  int operator ==(const reg_struct &t) {
    if (level == t.level) {
      return 1;
    }
    else if (node_ptr->get_no_trans() == t.get_node()->get_no_trans()) {
      return 1;
    }
    else {
      transition *an1 = node_ptr->get_trans();
      transition *an2 = t.get_node()->get_trans();
      for (int i = 0; i < node_ptr->get_no_trans(); i++) {
	if (an1->get_label() == an2->get_label()) {
	  if (an1->get_target() != an2->get_target()) {
	    return 0;
	  }
	}
	else {
	  return 0;
	}
      }
      return 0;
    }
  }
  int operator !=(const reg_struct &t);// { return !(*this == t); }
};//reg_struct

int
reg_struct::operator !=(const reg_struct &t)
{
  if (level == t.get_level()) {
    if (node_ptr->get_no_trans() == t.get_node()->get_no_trans()) {
      transition *an1 = node_ptr->get_trans();
      transition *an2 = t.get_node()->get_trans();
      for (int i = 0; i < node_ptr->get_no_trans(); i++, an1++, an2++) {
	if (an1->get_label() != an2->get_label() || an1->get_target()
	    != an2->get_target())
	  return 1;
      }
      return 0;
    }
  }
  return 1;
}//reg_struct::operator !=

class pf_item {
public:
  int		label;		// word number
  int		eb;		// entries before
  int		next;		// transition target

  pf_item(const int l, const int e, const int n) : label(l), eb(e), next(n) {}
  pf_item(void) : label(0), eb(-1), next(0) {}
};

class col_inf {
public:
  int		address;
  int		origin;
  int		size;
  int		item_size;
  int		hash_size;
  int		pointer_size;

  col_inf(void) : address(0), origin(1), size(0), item_size(0), hash_size(0),
		  pointer_size(0) {}
};

/* Name:	hash
 * Class:	None.
 * Purpose:	Computes a hash function on a state.
 * Parameters:	state		- (i) state to be hashed;
 *		level		- (i) column number;
 *		tt		- (i) structure describing tuple.
 * Returns:	Hash function value.
 * Remarks:	None.
 */
int
hash(const tuple_node *state, const int level, const tuple *tt);

class register_class {
protected:
  reg_struct	*states;
public:
  register_class(void) { states = NULL; }
  void set_size(const int s) {
    cerr << "Register size set to " << s << " entries of "
	 << sizeof(reg_struct) << " bytes (total " << s * sizeof(reg_struct)
	 << " bytes)" << endl;
    states = new reg_struct[HASH_SIZE];
    if (states == NULL) {
      cerr << "Not enough memory for the register" << endl;
      exit(7);
    }
    memset(states, 0, sizeof(reg_struct) * HASH_SIZE);
  }
  tuple_node *search_insert(const tuple_node *n, const int l, const tuple *t);
  int squeze_register(void);
  reg_struct *operator [](const int i) const { return states + i; }
} the_register;

/* Name:	search_insert
 * Class:	register_class
 * Purpose:	Searches the register for the record. If found, returns it.
 *		If not found, puts it into the register, and returns NULL.
 * Parameters:	n		- (i) the state to be searched for;
 *		l		- (i) column number;
 *		t		- (i) structure describing tuple.
 * Returns:	That state or NULL if not found.
 * Remarks:	None.
 */
tuple_node *
register_class::search_insert(const tuple_node *n, const int l, const tuple *t)
{
  int h = hash(n, l, t);
  reg_struct *rs = new reg_struct(l, n);
  while (states[h].get_node() && states[h] != *rs) {
    h++;
    if (h >= HASH_SIZE) {
      h = 0;
    }
  }
  if (states[h].get_node()) {
    delete rs;
    return (tuple_node *)(states[h].get_node());
  }
  else {
    states[h] = *rs;
    delete rs;
    return NULL;
  }
}//register_class::search_register

/* Name:	register_state
 * Class:	None.
 * Purpose:	Searches for a state in the register. If found, returns it,
 *		if not, puts it inot the register.
 * Parameters:	n		- (i) the state to be searched for
 *					or registered;
 *		l		- (i) tuple column;
 *		t		- (i) tuple structure.
 * Returns:	Pointer to the state if found, NULL if not found
 *		(and registered).
 * Remarks:	None.
 */
tuple_node *
register_state(const tuple_node *n, const int l, const tuple *t);
  

/* Name:	weq
 * Class:	None.
 * Purpose:	Compares two word numbers held in a tuple.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		l		- (i) first tuple number;
 *		r		- (i) second tuple number;
 *		level		- (i) which word in the tuple;
 * Returns:	TRUE if words are the same, FALSE otherwise.
 * Remarks:	None.
 */
int
weq(const tuple *t, const int l, const int r, const int level);

/* Name:	get_word
 * Class:	None.
 * Purpose:	Extracts word number.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		l		- (i) tuple number;
 *		level		- (i) which word in the tuple;
 * Returns:	Word number.
 * Remarks:	The size of the string is taken from the size array
 *		of the tuple structure.
 */
int
get_word(const tuple *t, const int l, const int level);

/* Name:	get_offset
 * Class:	None.
 * Purpose:	Calculate the offset of a word in a tuple.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		n		- (i) which word in the tuple.
 * Returns:	Offset of that word.
 * Remarks:	None.
 */
int
get_offset(const tuple *t, const int n);

/* Name:	bytes2int
 * Class:	None.
 * Purpose:	Convert an integer from a portable string to a number.
 * Parameters:	bytes		- (i) string;
 *		n		- (i) string length.
 * Returns:	The number.
 * Remarks:	None.
 */
inline int
bytes2int(const unsigned char *bytes, const int n);

/* Name:	put_node
 * Class:	None.
 * Purpose:	Puts all labels (word numbers) from outgoing transition
 *		of a state into appropriate sparse table.
 * Parameters:	root		- (i) state;
 *		t		- (i) tuple description;
 *		level		- (i) column number.
 * Returns:	Starting index.
 */
int
put_node(tuple_node *root, const tuple *t, const int level);

/* Name:	perf_column_word_size
 * Class:	None.
 * Purpose:	Find the length in bytes of a word number in a perfect hash
 *		combined columns (columns with the same word number lengths
 *		are combined to form one perfect hash combined column).
 * Parameters:	t		- (i) tuple description;
 *		k		- (i) combined column number.
 * Returns:	Size of words in the column (measured in bytes).
 * Remarks:	None.
 */
int
perf_column_word_size(const tuple *t, const int k);

/* Name:	num2bytes
 * Class:	None.
 * Purpose:	Converts a positive integer into a string of bytes, least
 *		significant byte first.
 * Parameters:	buf		- (0) buffer where the result is to be stored;
 *		m		- (i) the number to be converted;
 *		n		- (i) number of bytes to be used.
 * Returns:	buf.
 * Remarks:	The calling function should insure that there is enough space
 *		in the buffer.
 */
char *
num2bytes(char *buf, const int m, const int n);


/* Name:	filled
 * Class:	None.
 * Purpose:	Calculates the number of filled places in each column.
 * Parameters:	col_nr		- (i) column number.
 * Returns:	Number of filled positions.
 * Remarks:	None.
 */
int
filled(const int col_nr);


/* Name:	link_node
 * Class:	None.
 * Purpose:	Creates links among states in the automaton.
 * Parameters:	root		- (i/o) state to be linked;
 *		t		- (i) tuple description;
 *		level		- (i) column nr.
 * Returns:	Root number.
 * Remarks:	None.
 */
int
link_node(tuple_node *root, const tuple *t, const int level);

/* Name:	num2bytes
 * Class:	None.
 * Purpose:	Converts a positive integer into a string of bytes, least
 *		significant byte first.
 * Parameters:	buf		- (0) buffer where the result is to be stored;
 *		m		- (i) the number to be converted;
 *		n		- (i) number of bytes to be used.
 * Returns:	buf.
 * Remarks:	The calling function should insure that there is enough space
 *		in the buffer.
 */
char *
num2bytes(char *buf, const int m, const int n)
{
  int j = m;
  char *b = buf;
  for (int i = 0; i < n; i++) {
    *b++ = j & 0xff;
    j >>= 8;
  }
  return buf;
}//num2bytes

/* Name:	bytes2int
 * Class:	None.
 * Purpose:	Convert an integer from a portable string to a number.
 * Parameters:	bytes		- (i) string;
 *		n		- (i) string length.
 * Returns:	The number.
 * Remarks:	None.
 */
inline int
bytes2int(const unsigned char *bytes, const int n)
{
  register int r = 0;
  register int i;
  for (i = n - 1; i >= 0; --i) {
    r <<= 8; r |= bytes[i];
  }
  return r;
}


/* STL removed
set<reg_struct, reg_struct::less> the_register;
*/

int		*table_nr;	// map from column nr in tuple to combined
col_inf		*column_inf;	// info on each combined column
pf_item		**pf_table;	// perfect hash columns
int		**occ1;		// table indicating which states are occupied
int		*occ2;		// table indicating how many transitions
				// are occupied in each column
int		*column_length;	// items allocted for combined column
int		total_states = 0; // total number of states
int		total_trans = 0; // total number of transitions
int		biggest_gap = 0; // biggest gap in transition vector
int		first_gap = 0;
int		last_gap = 0;

/* Name:	open_tuple.
 * Purpose:	Opens a tuple dictionary.
 * Parameters:	filename	- (i) name of the file with the tuple
 *					dictionary.
 * Returns:	A structure describing the tuple or NULL if failed.
 * Remarks:	The structure of the tuple file is as follows:
 *		Signature
 *		012345
 *		\TUPL
 *
 *		Number of words in the tuple
 *		6
 *		n
 *
 *		Number of numbers (integers) in the tuple
 *		7
 *		m
 *
 *		Sizes of words and numbers in bytes (1B for 1 size)
 *		8 .. 8 + m + n
 *		s(1) .. s(n+m)
 *
 *		Tuples
 *		8 + m + n .. end
 *		(n+m)-tuples of integers of sizes s(1)..s(n+m)
 *
 *		In case of errors, fadd_get_error() provides error code.
 */
tuple *
open_tuple(const char *filename);

enum	{FALSE, TRUE};


/* Name:	nstrdup
 * Class:	None.
 * Purpose:	Create a copy of the given string in dynamically allocated
 *		memory.
 * Parameters:	word	- (i) string to copy.
 * Returns:	A pointer to the copy.
 * Remarks:	strdup that uses `new' instead of `malloc'.
 */
char *
nstrdup(const char *word)
{
  char *n = new char[strlen(word) + 1];
  return strcpy(n, word);
}//nstrdup

/* Name:	nnstrdup
 * Class:	None.
 * Purpose:	Create a copy of the first characters of the given string
 *		in dynamically allocated memory.
 * Parameters:	word	- (i) string to copy;
 *		n	- (i) number of characters to copy.
 * Returns:	A pointer to the copy.
 * Remarks:	Uses new, not malloc.
 */
char *
nnstrdup(const char *word, const int n)
{
  char *s = new char[n + 1];
  strncpy(s, word, n);
  s[n] = '\0';
  return s;
}//nnstrdup

/* Name:	memndup
 * Class:	None.
 * Purpose:	Creates a copy of a few bytes in dynamic memory.
 * Parameters:	word		- (i) a string of bytes.
 *		n		- (i) length of the string.
 * Returns:	A pointer to the copy.
 * Remarks:	Used instead of nnstrdup because of 0's in byte strings.
 */
char *
memndup(const char *word, const int n)
{
  char *s = new char[n];
  memcpy(s, word, n);
  return s;
}//memndup

/* Name:	grow_string
 * Class:	None.
 * Purpose:	Make a copy of a string with more space allocated.
 * Parameters:	s		- (i/o) the string / its copy;
 *		allocated	- (i/o) size of old/new string;
 *		alloc_step	- (i) size of old string - size of new string.
 * Returns:	New string.
 * Remarks:	None.
 */
char *
grow_string(char *&s, int &allocated, const int alloc_step)
{
  char *new_s;

  if ((new_s = new char[allocated + alloc_step])) {
    memcpy(new_s, s, (size_t)allocated);
    allocated += alloc_step;
    delete [] s;
    s = new_s;
  }
  return new_s;
}


/* Name:	reg_comp
 * Class:	None.
 * Purpose:	Compares two entries in the register (for sorting).
 * Parameters:	a1		- (i) first entry;
 *		a2		- (i) second entry.
 * Returns:	An integer < 0 if a1 < a2, == 0 if a1 == a2, > 0 if a1 > a2.
 * Remarks:	First levels are compared, than the distance between
 *		the biggest and the smallest label among children, then
 *		the number of children, then the smallest label.
 *		Smallest levels, biggest differences, largest numbers
 *		of children, and biggest smallest labels come first.
 */
int
reg_comp(const void *a1, const void *a2);


/* Name:	open_tuple.
 * Purpose:	Opens a tuple dictionary.
 * Parameters:	filename	- (i) name of the file with the tuple
 *					dictionary;
 *		out_ver		- (i) tuple version to be built.
 * Returns:	A structure describing the tuple or NULL if failed.
 * Remarks:	The structure of the tuple file is as follows:
 *		Signature
 *		012345
 *		\TUPL
 *
 *		Number of words in the tuple
 *		6
 *		n
 *
 *		Number of numbers (integers) in the tuple
 *		7
 *		m
 *
 *		Sizes of words and numbers in bytes (1B for 1 size)
 *		8 .. 8 + m + n
 *		s(1) .. s(n+m)
 *
 *		Tuples
 *		8 + m + n .. end
 *		(n+m)-tuples of integers of sizes s(1)..s(n+m)
 *
 *		In case of errors, fadd_get_error() provides error code.
 *
 *		out_ver determines handling of sizes.
 */
tuple *
open_tuple(const char *filename, const int out_ver)
{
  unsigned char	sig_arc[8];
  streampos	file_size;
#ifdef DEBUG
  cerr << "open tuple\n";
#endif
  if (filename == NULL)
    return NULL;

  ifstream dict(filename, ios::in | ios::nocreate | ios::ate);
  if (dict.bad()) {
    cerr << "Cannot open tuple file " << filename << "\n";
    return(NULL);
  }

  // see how many arcs are there
#ifdef LOOSING_RPM
  // There is a bug in libstdc++ distributed in rpms.
  // This is a workaround (thanks to Arnaud Adant <arnaud.adant@supelec.fr>
  // for pointing this out).
  if (!dict.seekg(0,ios::end)) {
    cerr << "Seek on dictionary file failed. File is "
         << filename << "\n";
    return NULL;
  }
  file_size = dict.tellg();
#else
  file_size = dict.tellg();
#endif
  if (!dict.seekg(0L)) {
    cerr << "Seek on dictionary file failed. File is "
         << filename << "\n";
    return NULL;
  }

  // read and verify signature
  if (!(dict.read((char *)&sig_arc, sizeof(sig_arc)))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    return(NULL);
  }
  if (strncmp((char *)sig_arc, "\\TUPL", (size_t)5)) {
    cerr << "Invalid dictionary file (bad magic number): " << filename
      << endl;
    return(NULL);
  }
  if (sig_arc[5] != 0) {
    cerr << "Invalid tuple version" << endl;
    return NULL;
  }

  // read sizes
  tuple *t;
  if ((t = new tuple) == NULL) {
    return NULL;
  }
  t->word_dicts = sig_arc[6];
  t->num_dicts = sig_arc[7];
  int total = t->word_dicts + t->num_dicts;
  if ((t->sizes = new unsigned char[total]) == NULL) {
    return NULL;
  }
  if (!(dict.read((char *)t->sizes, (size_t)total))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    return(NULL);
  }

  // read signs vector
#ifdef DEBUG
  cerr << "Allocating " << ((t->num_dicts + 7) / 8) << " bytes for sign vector"
       << endl;
#endif
  int sign_vec_size = (t->num_dicts + 7) / 8;
  if ((t->signs = new char[sign_vec_size]) == NULL) {
    return NULL;
  }
#ifdef DEBUG
  cerr << "Reading the sign vector" << endl;
#endif
  if (!dict.read((char *)(t->signs), (size_t)(sign_vec_size))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    return(NULL);
  }

  // read the tuples
#ifdef DEBUG
  cerr << "allocating and reading the tuple" << endl;
#endif
  t->string_size = 0;
  for (int j = 0; j < t->word_dicts; j++)
    t->string_size += t->sizes[j];
  int tuple_size = 0;
  for (int i = 0; i < total; i++)
    tuple_size += t->sizes[i];
  if (t->word_dicts == 1) {
    tuple_size = tuple_size - t->string_size;
  }
  t->tuple_size = tuple_size;
  if (out_ver == 1) {
    // WARNING: new, dangerous code
    t->orig_wdicts = t->word_dicts;
    t->orig_sizes = t->sizes;
    t->word_dicts = t->string_size;
    t->sizes = new unsigned char[t->string_size];
    for (int tt = 0; tt < t->string_size; tt++)
      t->sizes[tt] = 1;
    // end of new code
  }
  if ((t->tuples = new char[file_size - total - 8 - sign_vec_size]) == NULL) {
    return NULL;
  }
  if (!(dict.read((char *)t->tuples, file_size - total - 8 - sign_vec_size))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    return(NULL);
  }
  if ((t->dicts = new int[t->word_dicts]) == NULL) {
    return NULL;
  }
  t->dict_size = (file_size - total - 8 - sign_vec_size) / t->tuple_size;
  return t;
}//open_tuple

/* Name:	build_trie
 * Class:	None.
 * Purpose:	Builds a trie representing a tuple.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		level		- (i) word number in the tuple;
 *		first		- (i) starting index in the tuple part;
 *		last		- (i) last index in the tuple part.
 * Returns:	Root of a trie.
 * Remarks:	None.
 */
tuple_node *
build_trie(const tuple *t, const int level, const int first, const int last)
{
  int r = first;
  if (level >= t->word_dicts)
    return NULL;
  tuple_node *root = new tuple_node;
  for (int l = first; r <= last; l = r) {
    for (r = l; r <= last && weq(t, l, r, level); r++)
      ;
    root->add_trans(new transition(get_word(t, l, level),
				   build_trie(t, level + 1, l, r - 1)));
  }
  tuple_node *equivalent;
  if ((equivalent = register_state(root, level, t)) != NULL) {
    delete root;
    root = equivalent;
  }
  else {
    total_states++;
    total_trans += root->get_no_trans();
  }
  return root;
}//build_trie


/* Name:	weq
 * Class:	None.
 * Purpose:	Compares two word numbers held in a tuple.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		l		- (i) first tuple number;
 *		r		- (i) second tuple number;
 *		level		- (i) which word in the tuple;
 * Returns:	TRUE if words are the same, FALSE otherwise.
 * Remarks:	None.
 */
int
weq(const tuple *t, const int l, const int r, const int level)
{
  int offset = get_offset(t, level);
  return ((memcmp(t->tuples + l * t->tuple_size + offset,
		  t->tuples + r * t->tuple_size + offset,
		  (size_t)(t->sizes[level])) == 0) ?
	  TRUE : FALSE);
}//weq


/* Name:	get_word
 * Class:	None.
 * Purpose:	Extracts word number.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		l		- (i) tuple number;
 *		level		- (i) which word in the tuple;
 * Returns:	Word number
 * Remarks:	The size of the string is taken from the size array
 *		of the tuple structure.
 */
int
get_word(const tuple *t, const int l, const int level)
{
  return bytes2int((const unsigned char *)t->tuples + l * t->tuple_size +
		   get_offset(t, level),
		   (size_t)(t->sizes[level]));
}//get_word


/* Name:	get_offset
 * Class:	None.
 * Purpose:	Calculate the offset of a word in a tuple.
 * Parameters:	t		- (i) pointer to a tuple structure;
 *		n		- (i) which word in the tuple.
 * Returns:	Offset of that word.
 * Remarks:	None.
 */
int
get_offset(const tuple *t, const int n)
{
  int offset = 0;
  for (int i = 0; i < n; i++)
    offset += t->sizes[i];
  return offset;
}//get_offset

/* Name:	transition
 * Class:	transition
 * Purpose:	Initializes a transition.
 * Parameters:	l		- (i) label;
 *		t		- (i) target;
 *		tb		- (i) how many tuples are before this one;
 * Returns:	The transition.
 * Remarks:	Label pointer is copied.
 */
transition::transition(const int l, tuple_node *t) : label(l), target(t),
						       tuple_nr(0)
{
}//transition::transition

/* Name:	add_trans
 * Class:	tuple_node
 * Purpose:	Adds a transition to a state.
 * Parameters:	t		- (i) transition to be added.
 * Returns:	The state.
 * Remarks:	None.
 */
tuple_node &
tuple_node::add_trans(transition *t)
{
  transition *tvec;
  if ((tvec = new transition [no_trans + 1]) == NULL) {
    cerr << "Not enough memory for a new transition vector" << endl;
    exit(1);
  }
  if (outgoing) {
    memcpy(tvec, outgoing, no_trans * sizeof(transition));
    delete [] outgoing;
  }
  outgoing = tvec;
  t->set_tuple_nr(tuples_before);
  outgoing[no_trans] = *t;
  no_trans++;
  tuples_before += (t->get_target() ?
		    t->get_target()->get_tuples_before() : 1);
  return *this;
}//tuple_node::add_trans

/* Name:	register_state
 * Class:	None.
 * Purpose:	Searches for a state in the register. If found, returns it,
 *		if not, puts it inot the register.
 * Parameters:	n		- (i) the state to be searched for
 *					or registered;
 *		l		- (i) tuple column;
 *		t		- (i) structure describing tuple.
 * Returns:	Pointer to the state if found, NULL if not found
 *		(and registered).
 * Remarks:	None.
 */
tuple_node *
register_state(const tuple_node *n, const int l, const tuple *t)
{
  /* STL code not used
  reg_struct *t;
  if ((t = the_register.search(reg_struct(l, n))) != NULL) {
    return t->get_node();
  }
  else {
    the_register.insert(reg_struct(l, n));
    return NULL;
  }
  */
  return the_register.search_insert(n, l, t);
}//register_state


/* Name:	hash
 * Class:	None.
 * Purpose:	Computes a hash function on a state.
 * Parameters:	state		- (i) state to be hashed;
 *		level		- (i) column number;
 *		structure describing tuple.
 * Returns:	Hash function value.
 * Remarks:	None.
 */
int
hash(const tuple_node *state, const int level, const tuple *tt)
{
  int h = level * 17 + state->get_no_trans() * 23;
  transition *t = state->get_trans();
  for (int i = 0; i < state->get_no_trans(); i++) {
    h <<= 1;
    h += t->get_label() + (int)(t->get_target());
    if (h < 0) {
      h = - h;
    }
    t++;
  }
  h %= HASH_SIZE;
  return h;
}//hash

/* Name:	byte_size
 * Class:	None.
 * Purpose:	Calculates the size of an integer in bytes.
 * Parameters:	n		- (i) integer to be examined.
 * Returns:	The size in bytes.
 * Remarks:	None.
 */
int
byte_size(const int n)
{
  if (n == 0)
    return 1;
  else {
    int s = 1;
    for (int i = n; (i >>= 8) != 0; s++)
      ;
    return s;
  }
}//byte_size

/* Name:	write_perf_hash_sparse
 * Class:	None.
 * Purpose:	Writes the tuples as a sparse matrix representation
 *		with perfectly hashed strings of word numbers.
 * Parameters:	root		- (i) initial state of the automaton;
 *		t		- (i) tuple description;
 *		fn		- (i) file name for the new structure;
 *		out_ver		- (i) version of the new tuple.
 * Returns:	Completion code (0 means OK).
 * Remarks:	The structure of the tuple file in that version is as follows:
 *		Signature
 *		01234
 *		\TUPL
 *		Version number
 *		5
 *		v=1
 *		
 *
 *		Number of words in the tuple
 *		6
 *		n
 *
 *		Number of numbers (integers) in the tuple
 *		7
 *		m
 *
 *		Sizes of words and numbers in bytes (1B for 1 size)
 *		8 .. 8 + m + n - 1
 *		s(0) .. s(n+m-1)
 *
 *		Signs in the numerical part
 *		8 + m + n .. 8 + m + n + svs(=log2(m)+7)/8) - 1
 *
 *		Height of perfect hash automaton on word numbers
 *		8 + m + n + svs
 *		h
 *
 *		Size of addresses
 *		8 + m + n + svs + 1
 *		a
 *
 *		Address of number columns
 *		10 + m + n .. 9 + m + n + svs + a
 *		x
 *
 *		Table of data on perfect hash automaton columns
 *		10 + m + n + svs + a
 *		Each column has:
 *		address - aB
 *		origin - aB
 *		size - aB
 *		hash size - 1B
 *		pointer size - 1B
 *
 *		Perfect hash columns
 *		10 + m + n + a + h * (4 + 2a)
 *		data
 *
 *		Number columns
 *		x..end
 *		numerical data
 *
 *		End
 *
 *		Data in perfect hashing columns consists of:
 *		- label (1B for version 1, t->sizes[column_nr] for version 2)
 *		- perfect hashing number
 *		- pointer
 */
int
write_perf_hash_sparse(tuple_node *root, tuple *t, const char *fn,
		       const int out_ver)
{
  char		buf[16];
  // First collect information about size of labels in different columns.
  // If they are the same for consecutive columns, then they can be
  // stored in the same space.
  table_nr = new int[t->word_dicts];
  int curr_tab_nr = 0;
  for (int i = 0; i < t->word_dicts; i++) {
    if (i > 0
#ifndef MULTICOLUMN
	&& t->sizes[i-1] != t->sizes[i]
#endif
	)
      curr_tab_nr++;
    table_nr[i] = curr_tab_nr;
  }
  curr_tab_nr++;
  column_inf = new col_inf[curr_tab_nr];
  pf_table = new pf_item *[curr_tab_nr];
  occ1 = new int *[curr_tab_nr];
  occ2 = new int[curr_tab_nr];
  column_length = new int[curr_tab_nr];
  for (int pf = 0; pf < curr_tab_nr; pf++) {
    pf_table[pf] = new pf_item[t->dict_size];
    occ1[pf] = new int[t->dict_size];
    memset(occ1[pf], 0, t->dict_size * sizeof(int));
    occ2[pf] = 0;
    column_length[pf] = t->dict_size;
  }
  cerr << "Original: " << t->word_dicts << " words, " << t->num_dicts
       << " numbers in one tuple" << endl << "  " << t->dict_size
       << " tuples of " << t->tuple_size << " bytes." << endl;
  // put_node(root, t, 0);
  int reg_size = the_register.squeze_register();
  qsort(the_register[0], reg_size, sizeof(reg_struct), reg_comp);
  for (int pn = 0; pn < reg_size; pn++) {
    put_node(the_register[pn]->get_node(), t, the_register[pn]->get_level());
  }
  link_node(root, t, 0);
  int addr_size = byte_size(t->tuple_size * t->dict_size);
  int svs = 0;
  while (((t->num_dicts + 7) >> (8 * svs)) > 0) {
    svs++;
  }
  if (out_ver == 1) {
    // Restore original number of words in a tuple
    t->word_dicts = t->orig_wdicts;
  }
  column_inf[0].address = 9	// sig_arc + sizeof(address)
    + t->word_dicts + t->num_dicts // sizes of words and numbers
    + svs			// sign vector
    + addr_size			// addr of numbers
    + curr_tab_nr *		// number of perfect hashing columns
    (2 + 3 * addr_size);	// hash and pointer size + addr, origin, size
  column_inf[curr_tab_nr - 1].pointer_size = 0;
  for (int k = 0; k < curr_tab_nr; k++) {
    //
    if (curr_tab_nr == 1 && k == 0) {
      column_inf[0].pointer_size =
	byte_size(column_inf[0].size - column_inf[0].origin);
    }
    int largest = 0;
    for (int l = 0; l < column_inf[k].size; l++) {
      if (pf_table[k][l].eb > largest) {
	largest = pf_table[k][l].eb;
      }
    }
    column_inf[k].hash_size = byte_size(largest);
    if (k > 0) {
      column_inf[k - 1].pointer_size =
	byte_size(column_inf[k].size - column_inf[k].origin);
      column_inf[k - 1].item_size = column_inf[k - 1].hash_size +
	column_inf[k - 1].pointer_size
	+ (out_ver == 1 ? 1 : t->sizes[k - 1]);
	   // perf_column_word_size(t, k - 1));
      column_inf[k].address = column_inf[k - 1].address
	+ column_inf[k - 1].size * column_inf[k - 1].item_size;
    }
  }//for k
  if (curr_tab_nr != 1) {
    column_inf[curr_tab_nr - 1].item_size =
      column_inf[curr_tab_nr - 1].hash_size +
      (out_ver == 1 ? 1 : t->sizes[curr_tab_nr - 1]);
    // perf_column_word_size(t, curr_tab_nr - 1));
  }
  int address_of_numbers = column_inf[curr_tab_nr - 1].address +
    column_inf[curr_tab_nr - 1].item_size * column_inf[curr_tab_nr - 1].size;
  int address_size = byte_size(address_of_numbers);
  if (address_size != addr_size) {
    for (int aa = 0; aa < curr_tab_nr; aa++)
      column_inf[aa].address += (address_size - addr_size);
  }
  cerr << "New: " << total_states << " states, " << total_trans
       << " transitions, " << curr_tab_nr << " new columns." << endl
       << "  Root has " << root->get_no_trans() << " transitions." << endl;
  for (int kk1 = 0; kk1 < curr_tab_nr; kk1++) {
    cerr << "Column " << kk1 << ": " << filled(kk1) << "/"
	 << column_inf[kk1].size << " items of " << column_inf[kk1].item_size
	 << " bytes." << endl
	 << "  filled up to " << occ2[kk1] << ", biggest gap is "
	 << biggest_gap << ", first is " << first_gap << ", last "
	 << last_gap << endl
	 << "  hash size is " << column_inf[kk1].hash_size
	 << ", pointer size is " << column_inf[kk1].pointer_size << endl;
  }
  ofstream outf(fn);
  if (outf.bad()) {
    cerr << "Cannot open file " << fn << " for output!" << endl;
    exit(2);
  }
  if (out_ver == 1) {
    // Restore original size values
    delete [] t->sizes;
    t->sizes = t->orig_sizes;
  }
  // Write signature
  if (!(outf.write("\\TUPL", 5))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Tuple version
  char c = out_ver;
  if (!(outf.write((char *)&c, 1))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Number of words in a tuple
  c = t->word_dicts;
  if (!(outf.write((char *)&c, 1))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Number of intergers in a tuple
  c = t->num_dicts;
  if (!(outf.write((char *)&c, 1))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Sizes of word numbers and numbers
  if (!(outf.write((char *)(t->sizes), t->word_dicts + t->num_dicts))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Sign vector
  if (!(outf.write((char *)t->signs, (size_t)(t->num_dicts)))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Size of addresses
  c = address_size;
  if (!(outf.write((char *)&c, 1))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Address of numbers
  if (!(outf.write(num2bytes(buf, address_of_numbers, address_size),
		   address_size))) {
    cerr << "Cannot write to " << fn << endl;
    exit(3);
  }
  // Write data on individual columns of the perfect hashing part
  for (int ii = 0; ii < curr_tab_nr; ii++) {
    if (!(outf.write(num2bytes(buf, column_inf[ii].address, address_size),
		     address_size))) {
      cerr << "Cannot write to " << fn << endl;
      exit(3);
    }
    // Column origin
    if (!(outf.write(num2bytes(buf, - column_inf[ii].origin, address_size),
		     address_size))) {
      cerr << "Cannot write to " << fn << endl;
      exit(3);
    }
    // Number of items in a column
    if (!(outf.write(num2bytes(buf, column_inf[ii].size, address_size),
		      address_size))) {
      cerr << "Cannot write to " << fn << endl;
      exit(3);
    }
//    // Column item size
//    c = column_inf[ii].item_size;
//    if (!(outf.write((char *)&c, 1))) {
//	cerr << "Cannot write to " << fn << endl;
//	exit(3);
//    }
    // Hash counter size
    c = column_inf[ii].hash_size;
    if (!(outf.write((char *)&c, 1))) {
      cerr << "Cannot write to " << fn << endl;
      exit(3);
    }
    // Pointer size
    c = column_inf[ii].pointer_size;
    if (!(outf.write((char *)&c, 1))) {
      cerr << "Cannot write to " << fn << endl;
      exit(3);
    }
  }//for ii
  // Write data from perfect hashing columns
  for (int jj = 0; jj < curr_tab_nr; jj++) {
    int hs = column_inf[jj].hash_size;
    int ps = column_inf[jj].pointer_size;
    int l_size = column_inf[jj].item_size - hs - ps;
    for (int kk = 0; kk < column_inf[jj].size; kk++) {
      if (!(outf.write(num2bytes(buf, pf_table[jj][kk].label, l_size),
		       l_size))) {
	cerr << "Cannot write to " << fn << endl;
	exit(3);
      }
      if (!(outf.write(num2bytes(buf, pf_table[jj][kk].eb, hs), hs))) {
	cerr << "Cannot write to " << fn << endl;
	exit(3);
      }
      if (!(outf.write(num2bytes(buf, pf_table[jj][kk].next, ps), ps))) {
	cerr << "Cannot write to " << fn << endl;
	exit(3);
      }
    }
  }//for jj
  // Write data from numerical columns
  for (int kk = 0; kk < t->dict_size; kk++) {
    if (!(outf.write(t->tuples + kk * t->tuple_size + t->string_size,
		     t->tuple_size - t->string_size))) {
      cerr << "Cannot write to " << fn << endl;
      exit(3);
    }
  }
  return 0;
}//write_perf_hash_sparse

/* Name:	filled
 * Class:	None.
 * Purpose:	Calculates the number of filled places in each column.
 * Parameters:	col_nr		- (i) column number.
 * Returns:	Number of filled positions.
 * Remarks:	None.
 */
int
filled(const int col_nr)
{
  int result = 0;
  int gap = 0;
  biggest_gap = 0;
  first_gap = 0;
  last_gap = 0;
  for (int i = 0; i < column_inf[col_nr].size; i++) {
    if (pf_table[col_nr][i].eb >= 0) {
      result++;
      if (gap > biggest_gap)
	biggest_gap = gap;
      if (first_gap == 0)
	first_gap = gap;
      last_gap = gap;
      gap = 0;
    }
    else
      gap++;
  }
  return result;
}//filled

/* Name:	perf_column_word_size
 * Class:	None.
 * Purpose:	Find the length in bytes of a word number in a perfect hash
 *		combined columns (columns with the same word number lengths
 *		are combined to form one perfect hash combined column).
 * Parameters:	t		- (i) tuple description;
 *		k		- (i) combined column number.
 * Returns:	Size of words in the column (measured in bytes).
 * Remarks:	None.
 */
int
perf_column_word_size(const tuple *t, const int k)
{
  int j = 0;
  for (int i = 0; i < t->word_dicts; i++) {
    if (i > 0 && t->sizes[i] != t->sizes[i - 1])
      j++;
    if (j == k) {
      return t->sizes[i];
    }
  }
  return 0;
}//perf_column_word_size

/* Name:	put_node
 * Class:	None.
 * Purpose:	Puts all labels (word numbers) from outgoing transition
 *		of a state into appropriate sparse table.
 * Parameters:	root		- (i) state;
 *		t		- (i) tuple description;
 *		level		- (i) column number.
 * Returns:	Starting index.
 */
int
put_node(tuple_node *root, const tuple *t, const int level)
{
  if (root == NULL)
    return -1;			// the return value is not used in that case
  if (root->get_tuple_nr() != -1)
    return root->get_tuple_nr();
  int not_found = 1;
  int tab_nr = table_nr[level];
  int *occupied1 = occ1[tab_nr];
  int start_index = (column_inf[tab_nr].origin == 1) ?
    - root->get_smallest()
    : column_inf[tab_nr].origin;
  if (start_index + root->get_biggest() < occ2[tab_nr]) {
    start_index = occ2[tab_nr] - root->get_biggest();
  }
  if (column_inf[tab_nr].origin == 1)
    column_inf[tab_nr].origin = start_index;
  if (root->get_smallest() + start_index < 0)
    start_index = - root->get_smallest();
  // First see where to put it
  for (; not_found; start_index++) {
    // Is there any other state at the same place?
    if (column_inf[tab_nr].origin != 1 &&
	occupied1[start_index - column_inf[tab_nr].origin])
      continue;
    not_found = 0;
    transition *an = root->get_trans();
    int increment;
    if ((increment = start_index - column_inf[tab_nr].origin +
	root->get_biggest() - column_length[tab_nr]) > 0) {
      // Not enough memory allocated for column_inf[table_nr] and occupied
      if (t->dict_size > increment)
	increment = t->dict_size;
      int *tmp1 = new int[column_length[tab_nr] + increment];
      memmove(tmp1, occupied1, sizeof(int) * column_length[tab_nr]);
      memset(tmp1 + column_length[tab_nr], 0, increment * sizeof(int));
      delete [] occupied1;
      occupied1 = occ1[tab_nr] = tmp1;
      pf_item *tmp2 = new pf_item[column_length[tab_nr] + increment];
      memmove(tmp2, pf_table[tab_nr],
	      sizeof(pf_item) * column_length[tab_nr]);
      delete [] pf_table[tab_nr];
      pf_table[tab_nr] = tmp2;
      column_length[tab_nr] += increment;
    }
    // See if there are transitions of other states at the same place
    for (int i = 0; i < root->get_no_trans(); i++, an++) {
      if (pf_table[tab_nr]
	  [an->get_label() + start_index].eb >= 0) {
	// eb has initially -1, so any non-negative value means
	// that the slot is already occupied
	not_found = 1;
	break;
      }
    }
    if (!not_found)
      break;			// to avoid increasing start_index
  }
  // Start index found, put the transitions in place
  // First book the space
  occupied1[start_index - column_inf[tab_nr].origin] = 1;
  transition *an1 = root->get_trans();
  for (int j = 0; j < root->get_no_trans(); j++, an1++) {
    int t_index = an1->get_label() + start_index;
    pf_table[tab_nr][t_index] =
      pf_item(an1->get_label(), an1->get_tuple_nr(), 0);
    if (t_index++ == occ2[tab_nr]) {
      while (pf_table[tab_nr][t_index].eb > 0) {
	t_index++;
      }
      occ2[tab_nr] = t_index;
    }
  }
  // Now put the transitions (descendats already cannot steal that space)
  /*
  an1 = root->get_trans();
  for (int k = 0; k < root->get_no_trans(); k++, an1++) {
    int t_index = an1->get_label() + start_index;
    pf_table[tab_nr][t_index].next = put_node(an1->get_target(), t, level + 1);
  }
  */
  // Keep track of the table size
  int ll = root->get_biggest() + start_index + 1;
  if (ll > column_inf[tab_nr].size)
    column_inf[tab_nr].size = ll;
  root->set_tuple_nr(start_index  - column_inf[tab_nr].origin);
  return start_index - column_inf[tab_nr].origin;
}//put_node

/* Name:	link_node
 * Class:	None.
 * Purpose:	Creates links among states in the automaton.
 * Parameters:	root		- (i/o) state to be linked;
 *		t		- (i) tuple description;
 *		level		- (i) column nr.
 * Returns:	Root number.
 * Remarks:	None.
 */
int
link_node(tuple_node *root, const tuple *t, const int level)
{
  if (level >= t->word_dicts - 1)
    return root->get_tuple_nr();
  transition *an1 = root->get_trans();
  for (int k = 0; k < root->get_no_trans(); k++, an1++) {
    int t_index = an1->get_label() + root->get_tuple_nr() +
      column_inf[level].origin;
    pf_table[level][t_index].next = link_node(an1->get_target(), t, level + 1);
  }
  return root->get_tuple_nr();
}//link_node
  

/* Name:	squeze_register
 * Class:	register_class
 * Purpose:	Moves all entries in the sparse matrix register
 *		so that they occupy contigous space at the beginning.
 * Parameters:	None.
 * Returns:	Number of entries (size of the register after squezing).
 * Remarks:	hash function no longer points to entries afetr squezing.
 */
int
register_class::squeze_register(void)
{
  int filled = 0;
  while (states[filled].get_node() != NULL)
    filled++;
  // Now we have the first gap
  int i = filled;
  while (i < HASH_SIZE) {
    while (i < HASH_SIZE && states[i].get_node() == NULL)
      i++;
    int end_gap = i;
    while (i < HASH_SIZE && states[i].get_node() != NULL)
      i++;
    if (i != end_gap) {
      // If there is something after the gap
      memmove(states + filled, states + end_gap,
	      sizeof(reg_struct) * (i - end_gap));
    }
    filled += i - end_gap;
  }
  return filled;
}//register_class::squeze_register

/* Name:	reg_comp
 * Class:	None.
 * Purpose:	Compares two entries in the register (for sorting).
 * Parameters:	a1		- (i) first entry;
 *		a2		- (i) second entry.
 * Returns:	An integer < 0 if a1 < a2, == 0 if a1 == a2, > 0 if a1 > a2.
 * Remarks:	First levels are compared, than the distance between
 *		the biggest and the smallest label among children, then
 *		the number of children, then the smallest label.
 *		Smallest levels, biggest differences, largest numbers
 *		of children, and biggest smallest labels come first.
 */
int
reg_comp(const void *a1, const void *a2)
{
  int c1, c2, c3;
  if ((c1 = ((reg_struct *)a1)->get_level() -
       ((reg_struct *)a2)->get_level()) == 0) {
    if ((c2 = ((reg_struct *)a2)->get_node()->get_biggest() -
	((reg_struct *)a2)->get_node()->get_smallest() -
	((reg_struct *)a1)->get_node()->get_biggest() +
	 ((reg_struct *)a1)->get_node()->get_smallest())) {
      if ((c3 = ((reg_struct *)a2)->get_node()->get_no_trans() -
	  ((reg_struct *)a1)->get_node()->get_no_trans()) == 0) {
	return ((reg_struct *)a2)->get_node()->get_smallest() -
	  ((reg_struct *)a1)->get_node()->get_smallest();
      }
      else {
	return c3;
      }
    }
    else {
      return c2;
    }
  }
  else {
    return c1;
  }
}//reg_comp

/* Name:	main
 * Class:	None.
 * Purpose:	Launches the program.
 * Parameters:	argc		- (i) number of arguments;
 *		argv		- (i) table of arguments.
 * Returns:	Return code.
 * Remarks:	Synopsis:
 *		acctuple version input_tuple output_tuple
 */
int
main(const int argc, const char *argv[])
{
  if (argc < 3) {
    cerr << "Sysnopsis" << endl << argv[0]
	 << " version input_tuple output_tuple" << endl;
    return(4);
  }
  else {
    int version;
    if ((version = atoi(argv[1])) != 1 && version != 2) {
      cerr << "Version must be either 1 or 2" << endl;
      exit(6);
    }
    tuple *t = open_tuple(argv[2], version);
    if (t == NULL)
      exit(6);
    if (t->word_dicts == 1) {
      cerr << "Unigram is already represented in the most effective way"
	   << endl;
      exit(5);
    }
    the_register.set_size(10 * t->dict_size * t->word_dicts / 7);
    tuple_node *tt = build_trie(t, 0, 0, t->dict_size - 1);
    return write_perf_hash_sparse(tt, t, argv[3], version);
  }
}//main

/***	EOF acctuple.cc	***/
