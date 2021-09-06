/***	fadd.cc	***/

/*	Copyright (c) Jan Daciuk, for Alpino grammar, RuG, 2001	*/

/* This file contains library functions for accessing finite state
 * automata and tuples.
 */

#include	<cstring>
#include	<iostream>
#include	<fstream>
#include	<string>
#include	<stdlib.h>
#include	<new>
#include	<ctype.h>
#include	<math.h>
#ifdef DMALLOC
#include	"dmalloc.h"
#endif
#include	"fsa.h"
#include	"fadd.h"

using namespace std;

const int MAX_VANITY_LEVEL = 5;	/* how many letters to go without suffix */
const int MAX_GUESSES = 999;	/* how many guesses for one word */


/* The following 3 structures are used by tuples */

struct col_inf {		// info on perfect hashing columns (ver 1)
  long int	col_addr;	// column address (relative to root)
  int		origin;		// beginning offset
  int		size;		// number of items
  int		item_size;	// size of an individual item
  int		hash_size;	// size of number info
  int		pointer_size;	// size of the pointer field
};

struct col_inf3 {		// info on tree columns for version 3
  long int	col_addr;	// column address (relative to root)
  long int	nitems;		// number of items
  int		pointer_size;	// size of a pointer field
  int		item_size;	// size of a hash key and a pointer
};

class fadd_tuple {
public:
  int		word_dicts;	// number of words in the tuple
  int		num_dicts;	// number of numbers in the tuple
  int		string_size;	// size of combined number of all words
  int		tuple_size;	// size of one tuple
  int		dict_size;	// size of the whole tuple dictionary
  int		version;	// tuple version
  long int	end_of_tree;	// where the tree ends (version 3)
  col_inf	*column_inf;	// info about perfect hashing columns
  col_inf3	*tree_col_inf;	// info about columns of a tree (version 3)
  const char	*numbers;	// number columns (version 1,3)
  unsigned char	*sizes;		// sizes of each word number and number
  const char	*signs;		// bit vector whether a column can be negative
  unsigned char	*mantissa;	// sizes of mantissas for each number (0=int)
  const char	*tuples;	// the tuples themselves
  int		*dicts;		// dictionary numbers for words in tuples
  bool		relative;	// true for version 4 and relative pointers
  				// if first column has no hash keys
};

class fsa;

class dict_inf {
public:
  union {
    fsa		*fsap;		// automaton
    fadd_tuple	*tuplep;		// tuple
  }		dictionary;	// dictionary pointer
  const char	*filename;	// name of the file containing dictionary
  int		type;		// type of dictionary
  int		user;		// no of current users
  int		attr;		// attributes (depends on type)
};

int		MAX_DICTS	= 0;
dict_inf	**all_dict; // all dictionaries
int		all_dict_no = 0;
const char 	**acc_tables;

/* There can be a number of calls to the same library from different parts
   of the same program. They can be totally independent,
   and they may be interleaved. Each independent call for initialization
   returns a library key that is used to close the library. The library
   keeps track of the number of dictionaries requested in each call.
   If there are no dictionaries currently requested, some memory
   can be released. */
int		max_lib_calls = 10;
int		*lib_dicts;	// number of dicts allocated in each init
int		lib_calls;	// initialization calls to library
int		active_dicts;	// number of dictionaries used in all calls

/*	Internal definitions and prototypes	*/

char *invert(char *word);

/* Name:	fadd_set_errno
 * Purpose:	Sets the error number.
 * Parameters:	err		- (i) error number.
 * Returns:	Nothing.
 * Remarks:	Global variable fadd_errno is modified.
 */
void
fadd_set_errno(const long int);

/* Name:	nstrdup
 * Class:	None.
 * Purpose:	Create a copy of the given string in dynamically allocated
 *		memory.
 * Parameters:	word	- (i) string to copy.
 * Returns:	A pointer to the copy.
 * Remarks:	strdup that uses `new' instead of `malloc'.
 */
char *
nstrdup(const char *word);

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
nnstrdup(const char *word, const int n);

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
grow_string(char **s, int *allocated, const int alloc_step);

/* Name:	create_accent_table
 * Purpose:	Creates a table where the value at a place index by a character
 *		is the same character but without diacritics if it had any.
 * Parameters:	charset_no	- (i) character set number.
 * Returns:	Accent table.
 * Remarks:	None.
 */
const char *
create_accent_table(const long int charset_no);

/* Name:	get_dict_by_name
 * Purpose:	Finds dictionary by the name of the file that contains it.
 * Parameters:	filename	- (i) name of the file.
 * Returns:	dictionary number or -1 if failed.
 * Remarks:	Uses global variables all_dicts and all_dict_no.
 *		Linear search is used.
 */
static long int
get_dict_by_name(const char *filename);

/* Name:	add_dict
 * Purpose:	Adds a dictionary to the list of used dictionaries.
 * Parameters:	dict		- (i) pointer to fsa object for the dictionary;
 *		filename	- (i) name of the file containing dictionary;
 *		dict_type	- (i) type of the dictionary.
 * Returns:	Dictionary number or -1 if failed.
 * Remarks:	None.
 */
static long int
add_dict(fsa *dict, const char *filename, const long int dict_type);

/* Name:	add_tuple
 * Purpose:	Adds a tuple to a list of opened dictionaries.
 * Parameters:	filename	- (i) name of the file containing dictionary.
 * Returns:	Dictionary number or -1 if failed.
 * Remarks:	None.
 */
long int
add_tuple(const char *filename);

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
fadd_tuple *
open_tuple(const char *filename);

/* Name:	interp
 * Class:	None.
 * Purpose:	Determines the most likely index of a string in a vector
 *		of strings.
 * Parameters:	str		- (i) the string to be found;
 *		l		- (i) lower index boud;
 *		r		- (i) upper index bound;
 *		t		- (i) information about the tuple
 *					to be searched.
 * Returns:	Most likely index number.
 * Remarks:	This function implements interpolated search for a string
 *		in a set of sorted tuples. The function is not general;
 *		it is implemented to work only on specific data.
 *
 *		The strings (representing tuples in a vector of tuples)
 *		at the lower and upper bound are compared. If the initial
 *		bytes are equal, they are discarded until a difference
 *		is found or the strings are empty (i.e. they are equal).
 *		Then four bytes (or less if there are not enough of them)
 *		of both strings are used to compute corresponding numbers.
 *		The numbers are subtracted to give an estimate
 *		of the difference between the lower and upper bound.
 *		A similar calculation (with the same number of discarded bytes)
 *		is done on the searched for string and the lower bound.
 *		A ratio of of the differences should be the same as the ratio
 *		between the differences of respective indexes.
 */
int
interp(const char *str, const int l, const int r, const fadd_tuple *t);


/* Name:	str2i
 * Class:	None.
 * Purpose:	Converts a string of bytes of given length into an integer.
 * Parameters:	str		- (i) the string;
 *		len		- (i) length of the string.
 * Returns:	The number corresponding to the string.
 * Remarks:	The string is treated as an integer number stored
 *		on a big-endian architecture, i.e. with the most significant
 *		byte first. The lngth should not exceed the size of an integer.
 */
long int
str2i(const char *str, const int len);

/* Name:	find_keys
 * Class:	None.
 * Purpose:	Finds an index of the numerical part of a tuple identified
 *		by the string part of it (the keys).
 * Parameters:	word_list	- (i) list of words;
 *		t		- (i) tuple description.
 * Returns:	The index of the numerical part of the tuple if words match
 *		the string part, -1L otherwise.
 * Remarks:	What is returned is an index, a tuple number, not an offset
 *		in bytes. This is done in order to save space.
 */
long int
find_keys(list_of_words *word_list, const fadd_tuple *t);

/* Name:	find_subkeys
 * Class:	None.
 * Purpose:	Finds an index of the last word (or rather its hash key)
 *		of a prefix (a series of initial items) of a tuple
 *		in a tuple tree.
 * Parameters:	word_list	- (i) list of initial words;
 *		t		- (i) tuple description;
 *		plen		- (o) length of the prefix.
 * Returns:	The index of the rest of the tuple in the tree if the initial
 *		part matches, -1L otherwise.
 * Remarks:	This works with tuple versions 3 and 4.
 *		As in those versions, tuples are stored in a tree,
 *		the prefix (a few initial words, or rather their hash keys)
 *		refers to a unique subtree. That subtree is returned.
 */
long int
find_subkeys(list_of_words *word_list, const fadd_tuple *t, int *plen);


/* Name:	find_hashed_keys
 * Class:	None.
 * Purpose:	Finds an index of the numerical part of a tuple identified
 *		by the string part of it (the keys).
 * Parameters:	word_list	- (i) list of hash keys of words;
 *		t		- (i) tuple description.
 * Returns:	The index of the numerical part of the tuple if words match
 *		the string part, -1L otherwise.
 * Remarks:	What is returned is an index, a tuple number, not an offset
 *		in bytes. This is done in order to save space.
 *		The numerical part of the tuple has the same index
 *		as the entry in the last word column.
 *		In contrast to find_keys(), this function uses hash keys
 *		of words instead of words themselves.
 */
long int
find_hashed_keys(list_of_numbers *word_list, const fadd_tuple *t);

/* Name:	get_fpnumbers
 * Class:	None.
 * Purpose:	Returns a list of floating-point numbers at a given index
 *		in a numerical part of tuples.
 * Parameters:	indx		- (i) tuple number;
 *		t		- (i) tuple description.
 * Returns:	A list of floating-point numbers.
 * Remarks:	Intergers are converted to floating-point.
 */
list_of_fpnumbers *
get_fpnumbers(const long int indx, const fadd_tuple *t);

/* Name:	get_fpsubtree
 * Class:	None.
 * Purpose:	Returns a subtree starting at the given column and index.
 * Parameters:	idx		- (i) index in the column;
 *		t		- (i) tuple description;
 *		col		- (i) tuple column where the subtree starts.
 * Returns:	The subtree.
 * Remarks:	None.
 */
tree_of_fpnumbers *
get_fpsubtree(const long int idx, const fadd_tuple *t, const int col);

/* Name:	get_fpsisters
 * Class:	None.
 * Purpose:	Returns a forest of subtrees (a list of subtrees) from
 *		the tuple tree.
 * Parameters:	t		- (i) tuple description;
 *		col		- (i) tuple column number;
 *		l		- (i) index of the first subtree;
 *		p		- (i) index of the last subtree.
 * Returns:	A list of subtrees.
 * Remarks:	The subtrees end at the word level, as words in a tuple
 *		identify uniquely the numerical part.
 *		For example, for tuples:
 *		a a b 1 2 3
 *		a a c 2 3 1
 *		a b a 2 1 3
 *		a b c 1 3 2
 *		b a c 3 2 1
 *
 *		the structure returned is:
 *		( (a (a (b (1 2 3))
 *		        (c (2 3 1)))
 *		     (b (a (2 1 3))
 *		        (c (1 3 2))))
 *		  (b (a (c (3 2 1))))).
 */
tree_of_fpnumbers *
get_fpsisters(const fadd_tuple *t, const int col, const long int l,
	      const long int p);


/* Name:	get_float
 * Class:	None.
 * Purpose:	Converts a string of bytes (an internal representation)
 *		to a floating-point number.
 * Parameters:	buf		- (i) string of bytes;
 *		s		- (i) string size;
 *		m		- (i) mantissa size.
 * Returns:	The floating-point number.
 * Remarks:	The internal representation (the string of bytes) has s bytes,
 *		of which m are taken by the mantissa.
 *		If m is 0, the number is an interger and it is stored as such.
 *		The mantissa is stored as an integer. The value of the mantissa
 *		is the value of the integer stored on m bytes,
 *		divided by 2 ^ (8 * (m - 1) + 7). The mantissa has a sign;
 *		it is stored in the most significant bit. The number is
 *		represented in 2's complement code. The sign vector is not
 *		used for that purpose. The value of the mantissa mv
 *		is either 0 or (0.5 <= |mv| < 1.0).
 *		The exponent is a signed integer number in 2's complement.
 *		If an overflow occurs, HUGE_VAL is returned.
 */
double
get_float(const char *buf, const int s, const int m);

/* Name:	get_int
 * Class:	None.
 * Purpose:	Converts a string of bytes being 2's complement representation
 *		of an integer to an integer.
 * Parameters:	buf		- (i) the string;
 *		s		- (i) number of bytes in the string.
 * Returns:	The integer.
 * Remarks:	Overflows are not checked.
 */
long int
get_int(const char *buf, const int s);

enum	{FALSE, TRUE};

const int	Max_word_len = 120;     /* max word length in input file */
const int	LIST_INIT_SIZE = 16;	/* initial list capacity */
const int	LIST_STEP_SIZE = 8;	/* list size increment */
const int	MAX_NOT_CYCLE = 1024;   /* max length of candidate (if
					 greater, treated as cycle) */
static long int fadd_errno = 0;		// error number

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
grow_string(char **s, int *allocated, const int alloc_step)
{
  char *new_s;

  if ((new_s = new char[*allocated + alloc_step])) {
    memcpy(new_s, *s, (size_t) *allocated);
    *allocated += alloc_step;
    delete [] *s;
    *s = new_s;
  }
  else {
    fadd_set_errno(FADD_MEM);
  }
  return new_s;
}


/* Defines a dictionary with its inherent properties */
struct dict_desc {
  fsa_arc_ptr	dict;		/* dictionary itself (fsa) */
  int		no_of_arcs;	/* number of arcs */
  char          filler;		/* filler ("empty") character */
  char		annot_sep;	/* separates words from annotations */
  char		gtl;		/* size of goto field */
  char		entryl;		/* size of entries field */
#ifdef WEIGHTED
  int		goto_offset;	/* offset of the goto field in arcs */
  int		weighted;	/* TRUE if arcs weighted */
#endif
};/*dict_desc*/




/* Class name:	fsa
 * Purpose:	Provide an environment for a process.
 */
class fsa {
protected:
  dict_desc		*dictionary;	/* dictionary */
  arc_pointer	 	current_dict;	/* current dictionary */
  list_of_words		*replacements;	/* list of words */
  int			state;		/* true if OK, false otherwise */
  char			*candidate;	/* current replacement */
  int			cand_alloc;	/* size of candidate string */
  int			word_length;	/* length of word being processed */
  int			no_repls;	/* number of result strings */
  char			*word_ff;	/* word being processed */
  char			*word_syntax;	/* characters that form words */
  char			FILLER;		/* character to be ignored */
  char			ANNOT_SEPARATOR;/* separates parts of entries */
  const char		*char_eq;       /* accent table */
#ifdef WEIGHTED
  int			weighted; 	/* whether the automaton is weighted */
#endif //WEIGHTED
  int	morph_infixes;		/* TRUE if -I flag used */
  int	morph_prefixes;		/* TRUE if -P flag used */
#ifdef POOR_MORPH
  int	only_categories;	/* TRUE if -A flag used */
#endif /*POOR_MORPH*/
  int	ignore_filler;		/* if TRUE filler characters will be ignored */

  dict_desc *read_fsa(const char *dict_file_name);
  int word_in_dictionary(const char *word, fsa_arc_ptr start);
  void set_dictionary(dict_desc *dict);
  int word_accents(const char *word, const int level,
			  fsa_arc_ptr start);
  int morph_next_char(const char *word, const int level, fsa_arc_ptr start);
  int morph_infix(const int level, fsa_arc_ptr start);
  int morph_prefix(const int delete_position, const int level,
		   fsa_arc_ptr start);
  int morph_stem(const int delete_length, const int delete_position,
		 const int level, fsa_arc_ptr start);
  int morph_rest(const int level, fsa_arc_ptr start);
  int guess(const char *word, fsa_arc_ptr start, const int vanity_level);
  int print_rest(fsa_arc_ptr start, const int level);
  int handle_arc(fsa_arc_ptr next_node, const int vanity_level);
  int handle_annot_arc(fsa_arc_ptr next_node, const int vanity_level);
  int guess_stem(fsa_arc_ptr start, const int start_char,
		 const int infix_length);
  int check_prefix(fsa_arc_ptr start, const int char_no);
  int check_infix(fsa_arc_ptr start, const int char_no);
  int find_number(const char *word, fsa_arc_ptr start, int word_no);
  int words_in_node(fsa_arc_ptr start);
  char *find_word(const int word_no, int n, fsa_arc_ptr start,
			const int level);
  char *prefix_next_char(const char *word, const int char_no,
			 fsa_arc_ptr start);
public:
  fsa(const char *dict_name);
  virtual ~fsa();
  operator int(void) const {return state; }
  list_of_words *accent_word(const char *word, const char *equiv);
  list_of_words *morph_word(const char *word);
  list_of_words *guess_word(char *word);
  int number_word(const char *word);
  char *word_number(const int key);
  char *find_prefix(const char *word);
  void set_morph_parms(const int prefixes, const int infixes,
		       const int cat_only, const int ignorefiller) {
    morph_prefixes = prefixes; morph_infixes = infixes;
#ifdef POOR_MORPH
    only_categories = cat_only;
#endif
    ignore_filler = ignorefiller;
  }
};/*fsa*/

int fsa_arc_ptr::gtl = 2;	// initialization: this must be defined somewhere
int fsa_arc_ptr::size = 4;	// the same
int fsa_arc_ptr::entryl = 0; // the same
int fsa_arc_ptr::aunit = 0; // the same
#ifdef WEIGHTED
int goto_offset = 1;
#endif

#if defined(STOPBIT) && defined(TAILS)
arc_pointer curr_dict_address;
#endif

/* Name:	fsa
 * Class:	fsa (constructor).
 * Purpose:	Open dictionary files and read automata from them.
 * Parameters:	dict_name	- (i) dictionary file name;
 * Returns:	Nothing.
 * Remarks:	At least one dictionary file must be read.
 */
fsa::fsa(const char *dict_name)
{
  candidate = new char[cand_alloc = Max_word_len];
  dictionary = read_fsa(dict_name);
  state = (dictionary != NULL);
}//fsa::fsa

fsa::~fsa(void)
{
  delete[] candidate;

  if (dictionary != 0) {
    delete[] dictionary->dict.arc;
#if defined(STOPBIT) && defined(SPARSE)
    delete dictionary->sparse_vect;
#endif
    delete dictionary;
  }
}

/* Name:	read_fsa
 * Class:	fsa
 * Purpose:	Reads an automaton from a specified file.
 * Parameters:	dict_file_name	- (i) dictionary file name.
 * Returns:	A pointer to dictionary description structure or NULL.
 * Remarks:	None.
 */
dict_desc *
fsa::read_fsa(const char *dict_file_name)
{
#ifdef STOPBIT
#ifdef NEXTBIT
#ifdef TAILS
  const int	version = 7;	// FLEXIBLE,STOPBIT,NEXTBIT,TAILS
#else //!TAILS
#ifdef WEIGHTED
  const int	version = 8;	// FLEXIBLE,STOPBIT,NEXTBIT,!TAILS,WEIGHTED
#else //!WEIGHTED
  const int	version = 5;	// FLEXIBLE,STOPBIT,NEXTBIT,!TAILS,!WEIGHTED
#endif //!WEIGHTED
#endif //!TAILS
#else //!NEXTBIT
#ifdef TAILS
  const int	version = 6;	// FLEXIBLE,STOPBIT,!NEXTBIT,TAILS
#else //!TAILS
  const int	version = 4;	// FLEXIBLE,STOPBIT,!NEXTBIT,!TAILS
#endif //!TAILS
#endif //!NEXTBIT
#else //!STOPBIT
#ifdef NEXTBIT
  const int     version = 2;    // FLEXIBLE,!STOPBIT,NEXTBIT
#else //!NEXTBIT
  const int     version = 1;    // FLEXIBLE,!STOPBIT,!NEXTBIT
#endif //!NEXTBIT
#endif //!STOPBIT
  streampos	file_size;
  int		no_of_arcs;
  fsa_arc_ptr	new_fsa;
  signature	sig_arc;	/* magic number at the beginning of fsa */
  dict_desc	*dd;
  int		arc_size;

  // open dictionary file
#ifdef OLD_COMPILER
  ifstream dict(dict_file_name, ios::in | ios::nocreate | ios::ate |
    ios::binary);
#else
  ifstream dict(dict_file_name, ios::in | ios::ate | ios::binary);
#endif
  if (dict.bad()) {
    cerr << "Cannot open dictionary file " << dict_file_name << "\n";
    fadd_set_errno(FADD_DFILE_OPEN);
    return(NULL);
  }

  // see how many arcs are there
#ifdef LOOSING_RPM
  // There is a bug in libstdc++ distributed in rpms.
  // This is a workaround (thanks to Arnaud Adant <arnaud.adant@supelec.fr>
  // for pointing this out).
  if (!dict.seekg(0,ios::end)) {
    cerr << "Seek on dictionary file failed. File is "
         << dict_file_name << "\n";
    fadd_set_errno(FADD_DFILE_SEEK);
    return NULL;
  }
  file_size = dict.tellg();
#else
  file_size = dict.tellg();
#endif
  if (!dict.seekg(0L)) {
    cerr << "Seek on dictionary file failed. File is "
         << dict_file_name << "\n";
    fadd_set_errno(FADD_DFILE_SEEK);
    return NULL;
  }

  // read and verify signature
  if (!(dict.read((char *)&sig_arc, sizeof(sig_arc)))) {
    cerr << "Cannot read dictionary file " << dict_file_name << "\n";
    fadd_set_errno(FADD_DFILE_READ);
    return(NULL);
  }
  if (strncmp(sig_arc.sig, "\\fsa", (size_t)4)) {
    cerr << "Invalid dictionary file (bad magic number): " << dict_file_name
      << endl;
    fadd_set_errno(FADD_DFILE_MAGIC);
    return(NULL);
  }
  if (sig_arc.ver != version && !(sig_arc.ver == 5 && version == 8)) {
    cerr << "Invalid dictionary version in file: " << dict_file_name << endl
	 << "Version number is " << int(sig_arc.ver)
	 << " which indicates dictionary was build:" << endl;
    fadd_set_errno(FADD_DFILE_VERSION);
    switch (sig_arc.ver) {
    case 0:
      cerr << "without FLEXIBLE, without LARGE_DICTIONARIES, "
	   << "without STOPBIT, without NEXTBIT" << endl;
      break;
    case '\x80':
      cerr << "without FLEXIBLE, with LARGE DICTIONARIES, "
	   << "without STOPBIT, without NEXTBIT" << endl;
      break;
    case 1:
      cerr << "with FLEXIBLE, without LARGE DICTIONARIES, "
	   << "without STOPBIT, without NEXTBIT" << endl;
      break;
    case 2:
      cerr << "with FLEXIBLE, without LARGE_DICTIONARIES, "
	   << "without STOPBIT, with NEXTBIT" << endl;
      break;
    case 4:
      cerr << "with FLEXIBLE, without LARGE_DICTIONARIES, "
	   << "with STOPBIT, without NEXTBIT, without TAILS" << endl;
      break;
    case 5:
      cerr << "with FLEXIBLE, without LARGE_DICTIONARIES, "
	   << "with STOPBIT, with NEXTBIT, without TAILS" << endl;
      break;
    case 6:
      cerr << "with FLEXIBLE,  without LARGE_DICTIONARIES, "
	   << "with STOPBIT, without NEXTBIT, with TAILS" << endl;
      break;
    case 7:
      cerr << "with FLEXIBLE,  without LARGE_DICTIONARIES, "
	   << "with STOPBIT, with NEXTBIT, with TAILS" << endl;
      break;
    default:
      cerr << "with yet unknown compile options (upgrade your software)"
	   << endl;
    }
    return NULL;
  }

#ifdef WEIGHTED
  goto_offset = 1;
  if (sig_arc.ver == 8)
    goto_offset++;
#endif
  FILLER = sig_arc.filler;
  ANNOT_SEPARATOR = sig_arc.annot_sep;
  new_fsa.gtl = sig_arc.gtl & 0x0f;
  new_fsa.size = new_fsa.gtl + goto_offset;
  new_fsa.entryl = (sig_arc.gtl >> 4) & 0x0f;
  new_fsa.aunit = (new_fsa.entryl ? 1 : new_fsa.size);

  // allocate memory and read the automaton
#ifdef NEXTBIT
  arc_size = 1;
  no_of_arcs = (long)file_size - sizeof(sig_arc);
  new_fsa = new char[no_of_arcs];
  if (new_fsa.arc == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
#else
  if (new_fsa.entryl) {
    no_of_arcs = (long)file_size - sizeof(sig_arc);
    new_fsa = new char[no_of_arcs];
    if (new_fsa.arc == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    arc_size = 1; // for use in reading later on to specify how much to read
  }
  else {
    arc_size = goto_offset + sig_arc.gtl;
    no_of_arcs = ((long)file_size - sizeof(sig_arc)) / arc_size;
    if ((long)arc_size * no_of_arcs !=
	((long)file_size - (long)sizeof(sig_arc)))
      no_of_arcs++;
    //    if ((new_fsa = new char[((long)file_size - sizeof(sig_arc))]) == NULL) {
    //   fadd_set_errno(FADD_MEM);
    new_fsa = new char[((long)file_size - sizeof(sig_arc))];
    if (new_fsa.arc == NULL) {
      return NULL;
    }
  }
#endif //!NEXTBIT
  if (!(dict.read((char *)(new_fsa.arc), ((long)file_size-sizeof(sig_arc))))) {
    cerr << "Cannot read dictionary file " << dict_file_name << "\n";
    fadd_set_errno(FADD_DFILE_READ);
    return(NULL);
  }

  // put the automaton on the list of dictionaries
  if ((dd = new dict_desc) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
  dd->filler = FILLER;
  dd->annot_sep = ANNOT_SEPARATOR;
  dd->gtl = new_fsa.gtl;
  dd->entryl = new_fsa.entryl;
#ifdef WEIGHTED
  dd->goto_offset = goto_offset;
#endif
  dd->dict = new_fsa;
  dd->no_of_arcs = no_of_arcs;
  fadd_set_errno(FADD_OK);
  return dd;
}//fsa::read_fsa


/* Name:	word_in_dictionary
 * Class:	fsa.
 * Purpose:	Find if a word is in a dictionary (automaton).
 * Parameters:	word	- (i) word to check;
 *		start	- (i) look at children of this node.
 * Returns:	TRUE if word found, FALSE otherwise.
 * Remarks:	None.
 */
int
fsa::word_in_dictionary(const char *word, fsa_arc_ptr start)
{
  bool found = false;
  do {
    found = false;
    fsa_arc_ptr next_node = start.set_next_node(current_dict);
    forallnodes(i) {
      if (*word == next_node.get_letter()) {
	if (word[1] == '\0' && next_node.is_final())
	  return TRUE;
	else {
	  word++;
	  start = next_node;
	  found = TRUE;
	  break;
	}
      }
    }
  } while (found);
  return FALSE;
}//fsa::word_in_dictionary

/* Name:	set_dictionary
 * Class:	fsa
 * Purpose:	Sets variables associated with the current dictionary
 * Parameters:	dict	- (i) current dictionary description.
 * Returns:	Nothing.
 * Remarks:	None.
 */
void
fsa::set_dictionary(dict_desc *dict)
{
  fsa_arc_ptr	dummy(NULL);

  current_dict = dict->dict.arc;
#if defined(STOPBIT) && defined(TAILS)
  set_curr_dict_address(current_dict);
#endif
  FILLER = dict->filler;
#ifdef WEIGHTED
  goto_offset =  dict->goto_offset;
  weighted = dict->weighted;
#endif //WEIGHTED
  dummy.gtl = dict->gtl;
  dummy.size = dummy.gtl + goto_offset;
  dummy.entryl = dict->entryl;
  dummy.aunit = dummy.entryl ? 1 : (goto_offset + dummy.gtl);
}//fsa::set_dictionary


/* Name:	accent_word
 * Class:	fsa
 * Purpose:	Restore accents of a word using the specified dictionary.
 * Parameters:	word	- (i) word to be checked;
 *		equiv		- (i/o) classes of equivalences for characters.
 * Returns:	The list of equivalent words.
 * Remarks:	Class variable `replacements' is set to the list of equivalent
 *		words.
 *		The table of equivalent characters contains 256 characters,
 *		and contains:
 *		for characters with diacritics:
 *			- corresponding character without diacritic;
 *		for characters without diacritics:
 *			- that character.
 *		Note: this can be used for other forms of equivalencies,
 *		not necessarily with diacritics.
 */
list_of_words *
fsa::accent_word(const char *word, const char *equiv)
{
  fsa_arc_ptr		*dummy = NULL;

  char_eq = equiv;
  if ((word_ff = nstrdup(word)) == NULL)
    return NULL;
  word_length = strlen(word_ff);
  set_dictionary(dictionary);
  if (word_accents(word, 0, dummy->first_node(current_dict)) == -1) {
    return NULL;
  }
  return replacements;
}//fsa::accent_word



/* Name:	word_accents
 * Class:	fsa
 * Purpose:	Find all words that have the same letters, but sometimes
 *		with diacritics.
 * Parameters:	word	- (i) word to look for;
 *		level	- (i) how many characters of the word have been
 *				considered so far;
 *		start	- (i) look at children of that node.
 * Returns:	Number of words on the list of words equivalent to the `word'.
 * Remarks:	Class variable `replacements' is set to the list of words
 *		equivalent to the `word'.
 *		The table of equivalent characters contains 256 characters,
 *		and contains:
 *		for characters with diacritics:
 *			- corresponding character without diacritic;
 *		for characters without diacritics:
 *			- that character.
 *		Note: this can be used for other forms of equivalencies,
 *		not necessarily with diacritics.
 */
int
fsa::word_accents(const char *word, const int level, fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  unsigned char	char_no;
  list_of_words	*lwp;

  if (level + 1 >= cand_alloc)
    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
      return -1;
  forallnodes(i) {
    char_no = (unsigned char)(next_node.get_letter());
    if (*word == char_eq[char_no]) {
      candidate[level] = next_node.get_letter();
      if (word[1] == '\0' && next_node.is_final()) {
	candidate[level + 1] = '\0';
	if ((lwp = new list_of_words) == NULL) {
	  fadd_set_errno(FADD_MEM);
	  return -1;
	}
	lwp->next = replacements;
	lwp->word = nstrdup(candidate);
	replacements = lwp;
	no_repls++;
      }
      else if (word_accents(word + 1, level + 1, next_node) == -1) {
	  return -1;
      }
    }
  }
  return no_repls++;
}//fsa::word_accents


/* Name:	morph_word
 * Class:	fsa
 * Purpose:	Perform morphological analysis of a word
 *		using all specified dictionaries.
 * Parameters:	word	- (i) word to be checked.
 * Returns:	List of different analyses of the word.
 * Remarks:	Class variable `replacements' is set to the list
 *		of possible analyses.
 */
list_of_words *
fsa::morph_word(const char *word)
{
  fsa_arc_ptr		*dummy = 0;
  no_repls = 0;

#ifdef DEBUG
  cerr << "Morphing " << word << endl;
#endif
  if ((word_ff = nstrdup(word)) == NULL)
    return NULL;
  word_length = strlen(word);
#if DEBUG
  if (dictionary == NULL) {
    cerr << "NULL dictionary pointer in fsa::morph_word()" << endl;
  }
#endif
  ANNOT_SEPARATOR = dictionary->annot_sep;
  set_dictionary(dictionary);
  replacements = NULL;
  if (morph_next_char(word, 0, dummy->first_node(current_dict)) == -1) {
    return NULL;
  }
#ifdef DEBUG
  list_of_words *v = replacements;
  while (v) {
    cerr << "(" << v->word << ")";
    v = v->next;
  }
  cerr << endl;
#endif
  delete [] word_ff;
  return replacements;
}//fsa::morph_word



/* Name:	morph_next_char
 * Class:	fsa
 * Purpose:	Consider the next node in morphological analysis.
 * Parameters:	word	- (i) word to look for;
 *		level	- (i) how many characters of the word have been
 *				considered so far;
 *		start	- (i) look at children of that node.
 * Returns:	Number of different analyses of the word.
 * Remarks:	Class variable `replacements' is set to the list
 *		of different analyses of the word.
 *		Entries in the dictionary have the following format:
 *		inflected_word+Kending+tags
 *		where:
 *		inflected_word is an inflected form of a word,
 *		K specifies how many characters from the end of inflected_word
 *			do not match those from the respective lexeme; that
 *			number is computed as K - 'A',
 *		ending is the ending of lexeme,
 *		tags is a set of categories (annotation) of the inflected form.
 *		Example:
 *		cried+Dy+Vpp means that lexeme is cry, and tags are Vpp
 *		('D' - 'A' = 3, "cried" - 3 letters at end = "cr")
 */
int
fsa::morph_next_char(const char *word, const int level,
		     fsa_arc_ptr start)
{
  bool found = false;
  int lev = level;
  do {
    found = false;
    fsa_arc_ptr next_node = start.set_next_node(current_dict);
    if (*word == '\0') {
      forallnodes(i) {
	if (next_node.get_letter() == ANNOT_SEPARATOR) {
#ifdef POOR_MORPH
	  if (only_categories) {
	    if (lev >= cand_alloc)
	      if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL) {
		return -1;
	      }
	    strcpy(candidate, word_ff);
	    candidate[lev] = ANNOT_SEPARATOR;
	    candidate[lev + 1] = '\0';
	    morph_rest(lev + 1, next_node);
	  }
	  else
#endif
	    if (morph_infixes) {
	      if (morph_infix(lev, next_node) == -1) {
		return -1;
	      }
	    }
	    else if (morph_prefixes) {
	      if (morph_prefix(0, lev, next_node) == -1) {
		return -1;
	      }
	    }
	    else if (morph_stem(0, 0, lev, next_node) == -1) {
	      return -1;
	    }
	  break;
	}
      }
    }
    else {
      forallnodes(j) {
	if (*word == next_node.get_letter()) {
	  if (lev >= cand_alloc)
	    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL) {
		return -1;
	    }

	  candidate[lev] = *word;
	  word++;
	  lev++;
	  start = next_node;
	  found = true;
	  break;
	}
      }
    }
  } while (found);
  return no_repls;
}//morph_fsa::morph_next_char


/* Name:	morph_infix
 * Class:	fsa
 * Purpose:	Establish whether the inflected form has an infix,
 *		and locate it.
 * Parameters:	level	- (i) how many characters there are
 *				in the inflected form;
 *		start	- (i) look at the children of that node.
 * Returns:	Number of different analysis available in the analysed part
 *		of the automaton.
 * Remarks:	Class variable `replacements' is set to the list
 *		of different analyses of the word.
 *		Entries in the dictionary have the following format:
 *		inflected_word+MLKending+tags
 *		where:
 *		inflected_word is an inflected form of a word,
 *		M specifies whether the word has an infix, and locates it:
 *			"A" means there is no infix, "B" - the infix begins
 *			at the second character of the form, "C" - at the 3rd,
 *			"D" - at the fourth, and so on.
 *		L specifies the length of a prefix (in case M="A") or infix.
 *			"A" means there is no prefix or infix, "B" - it is
 *			one character long, "C" - 2 characters, and so on.
 *		K specifies how many characters from the end of inflected_word
 *			do not match those from the respective lexeme; that
 *			number is computed as K - 'A',
 *		ending is the ending of lexeme,
 *		tags is a set of categories (annotation) of the inflected form.
 *		Example:
 *		to come.
 */
int
fsa::morph_infix(const int level, fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int	delete_position;

  if (level >= cand_alloc)
    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
      return -1;

  forallnodes(i) {
    if ((delete_position = (next_node.get_letter() - 'A')) >= 0 &&
	delete_position < word_length) {
      if (morph_prefix(delete_position, level, next_node) == -1) {
	return -1;
      }
    }
  }
  return no_repls;
}//fsa::morph_infix


/* Name:	morph_prefix
 * Class:	fsa
 * Purpose:	Establish how many characters from the beginning part
 *		of the inflected word must be deleted to form the base form.
 * Parameters:	delete_position	- (i) where the characters to be deleted are;
 *		level		- (i) how many characters there are
 *					in the inflected form;
 *		start	- (i) look at children of this node.
 * Returns:	The number of different morphological analyses in the analysed
 *		part of the automaton.
 * Remarks:	Class variable `replacements' is set to the list
 *		of different analyses of the word.
 */
int
fsa::morph_prefix(const int delete_position, const int level,
			fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int	delete_length;

  if (level >= cand_alloc)
    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
      return -1;

  forallnodes(i) {
    if ((delete_length = (next_node.get_letter() - 'A')) >= 0 &&
	delete_length < word_length) {
      morph_stem(delete_length, delete_position, level, next_node);
    }
  }
  return no_repls;
}//fsa::morph_prefix



/* Name:	morph_stem
 * Class:	fsa
 * Purpose:	Establish how many characters from the end of the inflected
 *		form must be deleted to form the lexeme (with possibly some
 *		new characters).
 * Parameters:	level	- (i) how many characters there are
 *				in the inflected form;
 *		start	- (i) look at children of that node.
 * Returns:	Number of different analysis available in the analysed part
 *		of the automaton.
 * Remarks:	Class variable `replacements' is set to the list
 *		of different analyses of the word.
 *		Entries in the dictionary have the following format:
 *		inflected_word+Kending+tags
 *		where:
 *		inflected_word is an inflected form of a word,
 *		K specifies how many characters from the end of inflected_word
 *			do not match those from the respective lexeme; that
 *			number is computed as K - 'A',
 *		ending is the ending of lexeme,
 *		tags is a set of categories (annotation) of the inflected form.
 *		Example:
 *		cried+Dy+Vpp means that lexeme is cry, and tags are Vpp
 *		('D' - 'A' = 3, "cried" - 3 letters at end = "cr")
 */
int
fsa::morph_stem(const int delete_length, const int delete_position,
		const int level, fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int reject_from_word;

  if (level + 1 >= cand_alloc)
    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
      return -1;
  candidate[level] = '\0';
#ifdef DEBUG
  cerr << "Before morph_stem(" << level << "): " << candidate << endl;
#endif
  forallnodes(i) {
    if ((reject_from_word = (next_node.get_letter() - 'A')) >= 0 &&
	reject_from_word <= word_length) {
      if (delete_length > 0) {
	if (delete_position) {
	  // infix
	  // copy the word without ending
	  strncpy(candidate, word_ff, word_length - reject_from_word);
	  // remove the infix
	  memmove(candidate + delete_position,
		  candidate + delete_position + delete_length,
		  word_length - delete_position - delete_length);
	}
	else {
	  // prefix
	  // copy the word without the prefix and without ending
	  strncpy(candidate, word_ff + delete_length,
		  word_length - reject_from_word - delete_length);
	}
      }
      else
	strncpy(candidate, word_ff, word_length - reject_from_word);
      candidate[level - reject_from_word - delete_length] = '\0';
#ifdef DEBUG
      cerr << "Base is: " << candidate << endl;
#endif
      if (next_node.is_final()) {
	list_of_words	*lwp;
	candidate[level + 1] = '\0';
	if ((lwp = new list_of_words) == NULL) {
	  fadd_set_errno(FADD_MEM);
	  return -1;
	}
#ifdef DEBUG
	cerr << "Analysis found: " << candidate << endl;
#endif
	lwp->next = replacements;
	lwp->word = nstrdup(candidate);
	replacements = lwp;
	no_repls++;
      }
      if (morph_rest(level - reject_from_word - delete_length, next_node)
	  == -1) {
	return -1;
      }
    }
#ifdef DEBUG
    else {
      cerr << "Reject is " << reject_from_word << ", and word length is "
	   << word_length << endl;
    }
#endif
  }
  return no_repls;
}//morph_fsa::morph_stem


/* Name:	morph_rest
 * Class:	morph_fsa
 * Purpose:	Append lexeme ending and inflected form tags at the end
 *		of the candidate.
 * Parameters:	level	- (i) how many characters there are so far
 *				in the candidate;
 *		start	- (i) look at children of this node.
 * Returns:	The number of different morphological analyses in the analysed
 *		part of the automaton.
 * Remarks:	Class variable `replacements' is set to the list
 *		of different analyses of the word.
 *		Entries in the dictionary have the following format:
 *		inflected_word+Kending+tags
 *		where:
 *		inflected_word is an inflected form of a word,
 *		K specifies how many characters from the end of inflected_word
 *			do not match those from the respective lexeme; that
 *			number is computed as K - 'A',
 *		ending is the ending of lexeme,
 *		tags is a set of categories (annotation) of the inflected form.
 *		Example:
 *		cried+Dy+Vpp means that lexeme is cry, and tags are Vpp
 *		('D' - 'A' = 3, "cried" - 3 letters at end = "cr")
 */
int
fsa::morph_rest(const int level, fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  list_of_words	*lwp;

  if (level + 1 >= cand_alloc)
    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
      return -1;

  if (start.get_goto() != 0) {
    forallnodes(i) {
      if (!ignore_filler || next_node.get_letter() != FILLER) { 
	candidate[level] = next_node.get_letter();
      }
      if (next_node.is_final()) {
	candidate[level + 1] = '\0';
	if ((lwp = new list_of_words) == NULL) {
	  fadd_set_errno(FADD_MEM);
	  return -1;
	}
#ifdef DEBUG
	cerr << "Analysis found: " << candidate << endl;
#endif
	lwp->next = replacements;
	lwp->word = nstrdup(candidate);
	replacements = lwp;
	no_repls++;
      }
      if (morph_rest(level + 1, next_node) == -1) {
	return -1;
      }
    }
  }
  return no_repls;
}//fsa::morph_rest

/* Name:	guess_word
 * Class:	fsa
 * Purpose:	Perform approximative analysis of a word
 *		using the current dictionary.
 * Parameters:	word	- (i) word to be checked.
 * Returns:	List of different analyses of the word.
 * Remarks:	Class variable `replacements' is set to the list
 *		of possible analyses.
 */
list_of_words *
fsa::guess_word(char *word)
{
  fsa_arc_ptr		*dummy = 0;

#ifdef DEBUG
  cerr << "Guessing " << word << endl;
  if (dictionary == NULL) {
    cerr << "NULL dictionary pointer in fsa::guess_word()" << endl;
  }
#endif
  ANNOT_SEPARATOR = dictionary->annot_sep;
  FILLER = dictionary->filler;
  *word = FILLER;
  if ((word_ff = nstrdup(word)) == NULL)
    return NULL;
  word_length = strlen(word);
  invert(word);
  set_dictionary(dictionary);
  replacements = NULL;
  no_repls = 0;
  if (guess(word, dummy->first_node(current_dict), 0) == -1) {
    return NULL;
  }
#ifdef DEBUG
  list_of_words *v = replacements;
  while (v) {
    cerr << "(" << v->word << ")";
    v = v->next;
  }
  cerr << endl;
#endif
  delete [] word_ff;
  return replacements;
}//fsa::guess_word


/* Name:	handle_arc
 * Class:	fsa
 * Purpose:	Decides what to do with an arc in guess when no matching
 *		letters found on arcs.
 * Parameters:	arc	- (i) arc to be processed.
 * Returns:	TRUE if arc has an annotation separator, FALSE otherwise.
 * Remarks:	None.
 */
inline int
fsa::handle_arc(fsa_arc_ptr next_node, const int vanity_level)
{
  if (next_node.get_letter() == ANNOT_SEPARATOR) {
    handle_annot_arc(next_node, vanity_level);
    return FALSE;
  }
  else {
    guess("", next_node, vanity_level + 1);
    return TRUE;
  }
}/*handle_arc*/

/* Name:	handle_annot_arc
 * Class:	fsa
 * Purpose:	Decides what to do with an arc in guess when no matching
 *		letters found on arcs.
 * Parameters:	arc	- (i) arc to be processed.
 * Returns:	TRUE if arc has an annotation separator, FALSE otherwise.
 * Remarks:	None.
 */
inline int
fsa::handle_annot_arc(fsa_arc_ptr next_node, const int vanity_level)
{
  if (next_node.get_letter() == ANNOT_SEPARATOR) {
    if (morph_prefixes || morph_infixes) //
      check_prefix(next_node, 0);
    else if (only_categories)
      guess("", next_node, vanity_level + 1);
    else
      guess_stem(next_node, 0, 0);
    return FALSE;
  }
  else {
    guess("", next_node, vanity_level + 1);
    return TRUE;
  }
}/*handle_annot_arc*/

/* Name:	guess
 * Class:	fsa
 * Purpose:	Find categories the word might belong to.
 * Parameters:	word		- (i) word to look for;
 *		start		- (i) look at children of that node;
 *		vanity_level	- (i) level of calls with null word.
 * Returns:	Number of words on the list of categories the word
 *		might belong to.
 * Remarks:	The word is inverted, with a filler in front
 */
int
fsa::guess(const char *word, fsa_arc_ptr start, const int vanity_level)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int		to_be_completed = TRUE;


  // See if we are searching too deep in vain
  if (vanity_level > MAX_VANITY_LEVEL)
    return no_repls;

  // Look at children
  forallnodes(i) {
    if (*word == next_node.get_letter()) {
      guess(word + 1, next_node, 0);
      to_be_completed = FALSE;
      break;
    }
  }

  // No appropriate children found - look for annotation separator
  if (to_be_completed || no_repls == 0) {
    next_node = start.set_next_node(current_dict);
    forallnodes(j) {
      if (next_node.get_letter() == ANNOT_SEPARATOR) {
	if (morph_prefixes || morph_infixes) {
	  check_prefix(next_node, 0);
	}
	else if (only_categories) {
	  print_rest(next_node, 0);
	}
	else {
	  guess_stem(next_node, 0, 0);
	}
	to_be_completed = FALSE;
	break;
      }
    }

    // No annotation mark found - take all annotations below this node
    if (to_be_completed) {
#ifdef WEIGHTED
      if (weighted) {
	next_node = start.set_next_node(current_dict);
	if (next_node.is_last()) {
	  // the node has only one child
	  to_be_completed = handle_annot_arc(next_node, vanity_level);
	}
	else {
	  fsa_arc_ptr prev_node = next_node;
	  ++next_node;
	  if (next_node.is_last()) {
	    // the node has two children
	    if (prev_node.get_weight() > next_node.get_weight()) {
	      // first node is heavier
	      handle_arc(prev_node, vanity_level);
	      // lighter node
	      handle_arc(next_node, vanity_level);
	    }
	    else {
	      // second node is heavier
	      handle_arc(next_node, vanity_level);
	      // lighter node
	      handle_arc(prev_node, vanity_level);
	    }
	  }
	  else {
	    // more than two nodes
	    // first put arcs in an additional table and sort them
	    int no_of_arcs = 2;
	    while (!(next_node.is_last())) {
	      ++next_node; no_of_arcs++;
	    }
	    int *arc_indx = new int[no_of_arcs];
	    int *weights = new int[no_of_arcs];
	    next_node = start.set_next_node(current_dict);
	    int kk = 0;
	    forallnodes(kk) {
	      weights[kk] = next_node.get_weight();
	      arc_indx[kk] = kk;
	      kk++;
	    }
	    // crude sorting, but should be enough here and avoids problems
	    // with scope
	    for (int ii = 0; ii < no_of_arcs - 1; ii++) {
	      int x = weights[arc_indx[ii]];
	      for (int jj = ii + 1; jj < no_of_arcs; jj++) {
		if (x < weights[arc_indx[jj]]) {
		  x = weights[arc_indx[jj]];
		  int xj = arc_indx[ii];
		  arc_indx[ii] = arc_indx[jj];
		  arc_indx[jj] = xj;
		}
	      }
	    }
	    // now process the arcs in the order specified by arc_indx
	    next_node = start.set_next_node(current_dict);
	    for (int kk = 0; kk < no_of_arcs; kk++) {
	      prev_node = next_node.arc + arc_indx[kk] * next_node.size;
	      handle_annot_arc(prev_node, vanity_level);
	    }
	    delete [] weights;
	    delete [] arc_indx;
	  }
	}
      }
      else {
#endif //!WEIGHTED
      next_node = start.set_next_node(current_dict);
      forallnodes(l) {
	// Now it is possible that there is an arc with annotation separator
	// at this node, and it should be taken into account!
	if (next_node.get_letter() == ANNOT_SEPARATOR) {
	  to_be_completed = FALSE;
	  handle_annot_arc(next_node, vanity_level);
	}
      }
      if (to_be_completed) {
	next_node = start.set_next_node(current_dict);
	forallnodes(k) {
	  guess("", next_node, vanity_level + 1);
	}
      }
#ifdef WEIGHTED
      }
#endif //!WEIGHTED
    }
  }
  return no_repls;
}//guess_fsa::guess


/* Name:	print_rest
 * Class:	guess
 * Purpose:	Prints all strings from the automaton that start at a specified
 *		node.
 * Parameters:	start_node	- (i) look at children of that node;
 *		level		- (i) how many nodes are from the first one
 *					to this one.
 * Returns:	Number of items found in the part of the automaton
 *		starting in start.
 * Remarks:	level specifies how many characters there are already
 *		in the candidate string.
 */
int
fsa::print_rest(fsa_arc_ptr start, const int level)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int already_found = no_repls;
  list_of_words	*lwp;

  if (level > MAX_NOT_CYCLE) {
    cerr << "Possible cycle detected. Exiting." << endl;
    exit(5);
  }

  if (no_repls <= MAX_GUESSES) {
    if (level + 1 >= cand_alloc)
      grow_string(&candidate, &cand_alloc, Max_word_len);
    forallnodes(i) {
      candidate[level] = next_node.get_letter();
      if (next_node.is_final()) {
	candidate[level + 1] = '\0';
	if ((lwp = new list_of_words) == NULL) {
	  fadd_set_errno(FADD_MEM);
	  return -1;
	}
#ifdef DEBUG
	cerr << "Analysis found: " << candidate << endl;
#endif
	lwp->next = replacements;
	lwp->word = nstrdup(candidate);
	replacements = lwp;
	no_repls++;
      }
      if (next_node.get_goto() != 0)
	print_rest(next_node, level + 1);
    }
  }
  return no_repls - already_found;
}//fsa::print_rest


/* Name:	find_number
 * Class:	fsa
 * Purpose:	Translates word into a corresponding number.
 * Parameters:	word		- (i) word to be found;
 *		start		- (i) parent of the node to examined
 *		word_no		- (i) number of words before the given word.
 * Returns:	The number assigned to the word or -1 if not found.
 * Remarks:	The number assigned to a word is its position in a (sorted)
 *		file used to build the automaton.
 *		When more than one automaton is used, the numbers concern
 *		individual automata. No information about a specific
 *		automaton is returned.
 *
 *		All nodes contain information about the number of different
 *		words (more precisely: word suffixes) contained in this node
 *		and all nodes below.
 */
int
fsa::find_number(const char *word, fsa_arc_ptr start, int word_no)
{
  bool found = false;
  do {
    found = false;
    fsa_arc_ptr next_node = start.set_next_node(current_dict);
    forallnodes(i) {
      if (*word == next_node.get_letter()) {
	if (word[1] == '\0' && next_node.is_final())
	  return word_no;
	else {
	  if (next_node.get_goto() == 0) {
	    // end of string in the automaton
	    return -1;
	  }
	  start = next_node;
	  word++;
	  word_no += next_node.is_final();
	  found = true;
	  break;
	}
      }
      else {
	if (next_node.is_final())
	  word_no++;
	word_no += words_in_node(next_node);
      }
    }
  } while (found);
  return -1;
}//fsa::find_number


/* Name:	words_in_node
 * Class:	fsa
 * Purpose:	Returns the number of different words (word suffixes)
 *		in the given node.
 * Parameters:	start		- (i) parent of the node to be examined.
 * Returns:	Number of different word suffixes in the given node.
 * Remarks:	None.
 */
int
fsa::words_in_node(fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);

  return (start.get_goto() ?
	  bytes2int((unsigned char *)next_node.arc - next_node.entryl,
		    next_node.entryl)
	  : 0);
}//fsa::words_in_node


/* Name:	find_word
 * Class:	fsa
 * Purpose:	Finds a word whose number in a dictionary is given as argument.
 * Parameters:	word_no		- (i) number of the word to be found;
 *		n		- (i) number of words analysed;
 *		start		- (i) parent arc;
 *		level		- (i) character number of a word,
 *					or the distance from root.
 * Returns:	The word, or NULL if not found.
 * Remarks:	Only the first dictionary is searched.
 *		If we draw a graph representing the automaton, with the root
 *		on the left, and leaves on the right sorted from top to bottom,
 *		then the number of words analysed means the number of words
 *		that are in branches top of the search path in the automaton.
 */
char *
fsa::find_word(const int word_no, int n, fsa_arc_ptr start,
		    const int level)
{
  int m;
  bool found = false;
  int lev = level;

  do {
    found = false;
    fsa_arc_ptr next_node = start.set_next_node(current_dict);
    if (lev + 1 >= cand_alloc)
      if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
	return NULL;
    forallnodes(i) {
      if (next_node.is_final()) {
	if (n == word_no) {
	  candidate[lev] = next_node.get_letter();
	  candidate[lev + 1] = '\0';
	  return candidate;
	}
	else
	  n++;
      }

      if ((m = n + words_in_node(next_node)) > word_no) {
	candidate[lev] = next_node.get_letter();
	start = next_node;
	lev++;
	found = true;
	break;
      }
      else
	n = m;
    }//for
  } while (found);

  return NULL;
}//fsa::find_word

/* Name:	number_word
 * Class:	fsa
 * Purpose:	Finds the word number.
 * Parameters:	word		- (i) word whose number is to be found.
 * Returns:	Word number or -1 if failed.
 * Remarks:	None.
 */
int
fsa::number_word(const char *word)
{
  fsa_arc_ptr	*dummy = 0;

  set_dictionary(dictionary);
  return find_number(word, dummy->first_node(current_dict), 0);
}//fsa::number_word


/* Name:	word_number
 * Class:	fsa
 * Purpose:	Finds a word associated with a given number.
 * Parameters:	num		- (i) number associated with the word.
 * Returns:	The word or NULL if not found.
 * Remarks:	The word is stored in the class variable candidate.
 */
char *
fsa::word_number(const int key)
{
  fsa_arc_ptr	*dummy = 0;

  set_dictionary(dictionary);
  return find_word(key, 0, dummy->first_node(current_dict), 0);
}//fsa::word_number


/* Name:	find_prefix
 * Class:	fsa
 * Purpose:	Finds a prefix of a verb.
 * Parameters:	word		- (i) verb for which a prefix is to be found.
 * Returns:	The prefix or NULL if not present.
 * Remarks:	Strings in the dictionary consist of words, separator,
 *		and a capital letter indicating how many letters are in the
 *		prefix (A - 0, B - 1, C - 2, and so on).
 */
char *
fsa::find_prefix(const char *word)
{
  fsa_arc_ptr		*dummy = 0;

  ANNOT_SEPARATOR = dictionary->annot_sep;
  set_dictionary(dictionary);
  return prefix_next_char(word, 0, dummy->first_node(current_dict));
}//fsa::find_prefix

/* Name:	prefix_next_char
 * Class:	fsa
 * Purpose:	Finds a prefix of a verb.
 * Parameters:	word		- (i) verb to be analysed;
 *		char_no		- (i) which character to analyse;
 *		start		- (i) at which node of the automaton to start.
 * Returns:	The prefix or NULL if not found.
 * Remarks:	Strings in the dictionary consist of words, separator,
 *		and a capital letter indicating how many letters are in the
 *		prefix (A - 0, B - 1, C - 2, and so on).
 */
char *
fsa::prefix_next_char(const char *word, const int char_no, fsa_arc_ptr start)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  char	l, c;

  if (char_no + 1 >= cand_alloc)
    if (grow_string(&candidate, &cand_alloc, Max_word_len) == NULL)
      return NULL;
  forallnodes(i) {
    if ((l = next_node.get_letter()) == ANNOT_SEPARATOR) {
      // The next character says how long the prefix is
      next_node = next_node.set_next_node(current_dict);
      if ((c = next_node.get_letter()) == 'A') {
	// There is no prefix
	return NULL;
      }
      else {
	if (c - 'A' > 0 && c - 'A' < char_no) {
	  candidate[c - 'A'] = '\0';
	  return nstrdup(candidate);
	}
	else {
	  cerr << "Error in prefix dictionary" << endl;
	  return NULL;
	}
      }
    }
    else if (l == *word) {
      candidate[char_no] = l;
      return prefix_next_char(word + 1, char_no + 1, next_node);
    }
  }
  return NULL;
}//fsa::prefix_next_char

/* Name:	init_accent
 * Purpose:	Initializes a dictionary for restoration of diacritics.
 * Parameters:	filename	- (i) name of the file containing
 *					the dictionary;
 *		charset_no	- (i) character set number.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	Currently, charset_no = 0 is ISO8859-1, and charset = 1
 *		is ISO8859-2.
 *		If init_accent has failed, the error code is available
 *		by fadd_get_errno().
 */
long int
init_accent(const char *filename, const long int charset_no)
{
  long int dict_no;

  if ((dict_no = init_dict(filename, FADD_ACCENT)) != -1L) {
    if ((acc_tables[dict_no] = create_accent_table(charset_no)) == NULL)
      return -1L;
    all_dict[dict_no]->attr = charset_no;
    return dict_no;
  }
  return -1L;
}//init_accent


/* Name:	init_given_accent
 * Purpose:	Initializes a dictionary for restoration of diacritics.
 *		The dictionary is given the name
 * Parameters:	dict_no		- (i) dictionary number;
 *		filename	- (i) name of the file containing
 *					the dictionary;
 *		charset_no	- (i) character set number.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	Currently, charset_no = 0 is ISO8859-1, and charset = 1
 *		is ISO8859-2.
 *		If init_given_accent has failed, the error code is available
 *		by fadd_get_errno().
 *
 *		This function is provided in order to restore a saved session
 *		in prolog.
 */
long int
init_given_accent(const int dict_no, const char *filename,
		  const long int charset_no)
{
  long int dn;

  if ((dn = init_given_dict(dict_no, filename, FADD_ACCENT)) == dict_no) {
    if ((acc_tables[dict_no] = create_accent_table(charset_no)) == NULL)
      return -1L;
    all_dict[dict_no]->attr = charset_no;
    return dict_no;
  }
  return -1L;
}//init_given_accent


/* Name:	create_accent_table
 * Purpose:	Creates a table where the value at a place index by a character
 *		is the same character but without diacritics if it had any.
 * Parameters:	charset_no	- (i) character set number.
 * Returns:	Accent table.
 * Remarks:	None.
 */
const char *
create_accent_table(const long int charset_no)
{
  char const *latin1d = "¿¡¬√ƒ≈«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›‡·‚„‰ÂÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝ˇ";
  char const *latin1n = "AAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiinoooooouuuuyy";
  char const *latin2d = "°£•¶©™´¨ÆØ±≥µ∂π∫ªºæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›ﬁ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝˛";
  char const *latin2n = "ALLSSSTZZZallssstzzzRAAAALCCCCEEEIIDDNNOOOORUUUUYTraaaalccceeeeiiddnnooooruuuuyt";

  char *at;
  if ((at = new char[256]) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
  for (int i = 0; i < 256; i++)
    at[i] = i;
  const char *ld = charset_no ? latin2d : latin1d;
  const char *ln = charset_no ? latin2n : latin1n;
  int l = strlen((const char *)ld);
  for (int j = 0; j < l; j++)
    at[static_cast<int>(ld[j])]= ln[j];
  return at;
}//create_accent_table


/* Name:	init_dict
 * Purpose:	Initializes a dictionary of strings.
 * Parameters:	filename	- (i) name of the file containing dictionary;
 *		dict_type	- (i) dictionary type.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If init_dict has failed, the error code is available
 *		by fadd_get_errno().
 *		This function is appropriate for opening dictionaries for
 *		prefix_word() (with FADD_PREFIX), and number_word
 *		(with FADD_HASH). It is called internally
 *		for other types of dictionaries.
 */
long int
init_dict(const char *filename, const long int dict_type)
{
  long int	dict_no;
  fsa_arc_ptr	dummy(NULL);

  if ((dict_no = get_dict_by_name(filename)) != -1L) {
    all_dict[dict_no]->user++;
    return dict_no;
  }
  fsa *dict;
  if ((dict = new fsa(filename)) == NULL) {
    fadd_set_errno(FADD_MEM);
    return -1L;
  }
  if (int(*dict) == 0) {
    return -1L;
  }
  if ((dict_no = add_dict(dict, filename, dict_type)) == -1L) {
    return -1L;
  }
  if (dict_type == FADD_HASH && dummy.entryl == 0) {
    fadd_set_errno(FADD_NOTHASH);
    close_dict(dict_no);
    return -1L;
  }

  return dict_no;
}//init_dict

/* Name:	get_dict_by_name
 * Purpose:	Finds dictionary by the name of the file that contains it.
 * Parameters:	filename	- (i) name of the file.
 * Returns:	dictionary number or -1 if failed.
 * Remarks:	Uses global variables all_dicts and all_dict_no.
 *		Linear search is used.
 */
static long int
get_dict_by_name(const char *filename)
{
  for (long int i=0; i < all_dict_no; i++) {
    if (all_dict[i] && strcmp(filename, all_dict[i]->filename) == 0) {
      return i;
    }
  }
  return -1L;
}//get_dict_by_name

/* Name:	add_dict
 * Purpose:	Adds a dictionary to the list of used dictionaries.
 * Parameters:	dict		- (i) pointer to fsa object for the dictionary;
 *		filename	- (i) name of the file containing dictionary;
 *		dict_type	- (i) type of the dictionary.
 * Returns:	Dictionary number or -1 if failed.
 * Remarks:	None.
 */
static long int
add_dict(fsa *dict, const char *filename, const long int dict_type)
{
  for (long int i = 0; i < all_dict_no; i++) {
    if (all_dict[i] == NULL) {
      if ((all_dict[i] = new dict_inf) == NULL) {
	fadd_set_errno(FADD_MEM);
	return -1L;
      }
      all_dict[i]->dictionary.fsap = dict;
      if ((all_dict[i]->filename = nstrdup(filename)) == NULL)
	return -1L;
      all_dict[i]->type = dict_type;
      all_dict[i]->user = 1;
      return i;
    }
  }
  if (all_dict_no < MAX_DICTS) {
    if ((all_dict[all_dict_no] = new dict_inf) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1L;
    }
    all_dict[all_dict_no]->dictionary.fsap = dict;
    if ((all_dict[all_dict_no]->filename = nstrdup(filename)) == NULL)
      return -1L;
    all_dict[all_dict_no]->type = dict_type;
    all_dict[all_dict_no]->user = 1;
    all_dict_no++;
    return (all_dict_no - 1);
  }
  else {
    delete dict;
    fadd_set_errno(FADD_TOOMANY_DICTS);
    return -1;
  }
}//add_dict
  

/* Name:	init_given_dict
 * Purpose:	Initializes a dictionary of strings and gives it a specified
 *		number.
 * Parameters:	dict_no		- (i) dictionary number;
 *		dict_type	- (i) dictionary type.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If set_given_dict has failed, the error code is available
 *		by fadd_get_errno().
 *		This function is appropriate for opening dictionaries for
 *		prefix_word() (with FADD_PREFIX), and number_word
 *		(with FADD_HASH). It is called internally
 *		for other types of dictionaries.
 *		If dict_no is already in use, the function returns -1,
 *		and fadd_get_errno() returns FADD_NTAKEN.
 *
 *		This function is provided in order to restore a saved session
 *		in prolog.
 */
long int
init_given_dict(const int dict_no, const char *filename,
	       const long int dict_type)
{
  if (dict_no >= MAX_DICTS) {
    fadd_set_errno(FADD_TOOMANY_DICTS);
#ifdef DEBUG
    cerr << "Dictionary number is " << dict_no << ", max is " << MAX_DICTS
	 << "\n";
#endif
    return -1;
  }
  if (all_dict[dict_no] == NULL) {
    if ((all_dict[dict_no] = new dict_inf) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1L;
    }
    fsa *dict;
    if ((dict = new fsa(filename)) == NULL) {
      all_dict[dict_no] = NULL;
      fadd_set_errno(FADD_MEM);
      return -1L;
    }
    if (int(*dict) == 0) {
      return -1L;
    }
    all_dict[dict_no]->dictionary.fsap = dict;
    if ((all_dict[dict_no]->filename = nstrdup(filename)) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1L;
    }
    all_dict[dict_no]->type = dict_type;
    all_dict[dict_no]->user = 1;
    if (dict_no >= all_dict_no) {
      all_dict_no = dict_no + 1;
    }
#ifdef DEBUG
  cerr << "Dictionary restored: " << dict_no << " from " << filename << "\n";
#endif
    return dict_no;
  }
  else {
    fadd_set_errno(FADD_NTAKEN);
    return -1L;
  }
  if (dict_no >= all_dict_no) {
    all_dict_no = dict_no + 1;
  }

  return dict_no;
}//init_given_dict
    

/* Name:	init_morph
 * Purpose:	Initializes a morphological dictionary.
 * Parameters:	filename	- (i) name of the file containing dictionary;
 *		prefixes	- (i) TRUE if prefixes are specially encoded
 *					and infixes are not, FALSE otherwise;
 *		infixes		- (i) TRUE if infixes and prefixes are
 *					specially encoded;
 *		cat_only	- (i) TRUE if no baseforms are present in the
 *					dictionary;
 *		ignore_filler	- (i) TRUE if filler characters are to be
 *					ignored.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If init_morph has failed, the error code is available
 *		by fadd_get_errno().
 */
long int
init_morph(const char *filename, const long int prefixes,
	   const long int infixes, const long int cat_only,
	   const long int ignore_filler)
{
  long int dict_no;

  if ((dict_no = init_dict(filename, FADD_MORPH)) != -1L) {
    fsa *f = all_dict[dict_no]->dictionary.fsap;
    f->set_morph_parms(prefixes, infixes, cat_only, ignore_filler);
    int a = prefixes ? 1 : 0;
    if (infixes) a |= 2;
    if (cat_only) a |= 4;
    if (ignore_filler) a |= 8;
    all_dict[dict_no]->attr = a;
    return dict_no;
  }
  return -1L;
}//init_morph
  

/* Name:	init_given_morph
 * Purpose:	Initializes a morphological dictionary and gives it
 *		a specified number.
 * Parameters:	dict_no		- (i) dictionary number;
 *		filename	- (i) name of the file containing dictionary;
 *		prefixes	- (i) TRUE if prefixes are specially encoded
 *					and infixes are not, FALSE otherwise;
 *		infixes		- (i) TRUE if infixes and prefixes are
 *					specially encoded;
 *		cat_only	- (i) TRUE if no baseforms are present in the
 *					dictionary;
 *		ignore_filler	- (i) TRUE if filler characters are to be
 *					ignored.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If init_given_morph has failed, the error code is available
 *		by fadd_get_errno().
 *		If dict_no is already in use, the function returns -1,
 *		and fadd_get_errno() returns FADD_NTAKEN.
 *
 *		This function is provided in order to restore a saved session
 *		in prolog.
 */
long int
init_given_morph(const int dict_no, const char *filename,
		 const long int prefixes, const long int infixes,
		 const long int cat_only, const long int ignore_filler)
{
  long int dn;

  if ((dn = init_given_dict(dict_no, filename, FADD_MORPH)) == dict_no) {
    fsa *f = all_dict[dict_no]->dictionary.fsap;
    f->set_morph_parms(prefixes, infixes, cat_only, ignore_filler);
    int a = prefixes ? 1 : 0;
    if (infixes) a |= 2;
    if (cat_only) a |= 4;
    if (ignore_filler) a |= 8;
    all_dict[dict_no]->attr = a;
    return dict_no;
  }
  return -1L;
}//init_given_morph

/* Name:	init_guess
 * Purpose:	Initializes a guessing dictionary.
 * Parameters:	filename	- (i) name of the file containing dictionary;
 *		prefixes	- (i) TRUE if prefixes are specially encoded
 *					and infixes are not, FALSE otherwise;
 *		infixes		- (i) TRUE if infixes and prefixes are
 *					specially encoded;
 *		cat_only	- (i) TRUE if no baseforms are present in the
 *					dictionary;
 *		ignore_filler	- (i) TRUE if filler characters are to be
 *					ignored.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If init_guess has failed, the error code is available
 *		by fadd_get_errno().
 */
long int
init_guess(const char *filename, const long int prefixes,
	   const long int infixes, const long int cat_only,
	   const long int ignore_filler)
{
  long int dict_no;

  if ((dict_no = init_dict(filename, FADD_GUESS)) != -1L) {
    fsa *f = all_dict[dict_no]->dictionary.fsap;
    f->set_morph_parms(prefixes, infixes, cat_only, ignore_filler);
    int a = prefixes ? 1 : 0;
    if (infixes) a |= 2;
    if (cat_only) a |= 4;
    if (ignore_filler) a |= 8;
    all_dict[dict_no]->attr = a;
    return dict_no;
  }
  return -1L;
}//init_guess


/* Name:	init_given_guess
 * Purpose:	Initializes a guessing dictionary and gives it
 *		a specified number.
 * Parameters:	dict_no		- (i) dictionary number;
 *		filename	- (i) name of the file containing dictionary;
 *		prefixes	- (i) TRUE if prefixes are specially encoded
 *					and infixes are not, FALSE otherwise;
 *		infixes		- (i) TRUE if infixes and prefixes are
 *					specially encoded;
 *		cat_only	- (i) TRUE if no baseforms are present in the
 *					dictionary;
 *		ignore_filler	- (i) TRUE if filler characters are to be
 *					ignored.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If init_given_guess has failed, the error code is available
 *		by fadd_get_errno().
 *		If dict_no is already in use, the function returns -1,
 *		and fadd_get_errno() returns FADD_NTAKEN.
 *
 *		This function is provided in order to restore a saved session
 *		in prolog.
 */
long int
init_given_guess(const int dict_no, const char *filename,
		 const long int prefixes, const long int infixes,
		 const long int cat_only, const long int ignore_filler)
{
  long int dn;

  if ((dn = init_given_dict(dict_no, filename, FADD_GUESS)) == dict_no) {
    fsa *f = all_dict[dict_no]->dictionary.fsap;
    f->set_morph_parms(prefixes, infixes, cat_only, ignore_filler);
    int a = prefixes ? 1 : 0;
    if (infixes) a |= 2;
    if (cat_only) a |= 4;
    if (ignore_filler) a |= 8;
    all_dict[dict_no]->attr = a;
    return dict_no;
  }
  return -1L;
}//init_given_guess

/* Name:	init_given_tuple
 * Class:	None.
 * Purpose:	Opens a tuple, while assigning to it the given dictionary
 *		number.
 * Parameters:	dict_no		- (i) dictionary number to be used;
 *		filename	- (i) name of the file containing tuple;
 *		hash_dict_no	- (i) number of perfect hashing dictionaries;
 *		hash_dicts	- (i) hashing dictionary numbers.
 * Returns:	Dictionary number (dict_no), or -1 in case of failure.
 * Remarks:	It is assumed that the dictionary numbers in hash_dicts
 *		correspond to already opened perfect hashing (FADD_HASH)
 *		dictionaries.
 */
long int
init_given_tuple(const int dict_no, const char *filename,
		 const int hash_dict_no, int *hash_dicts)
{
#ifdef DEBUG
  cerr << "INIT_TUPLE: Dictionary number is " << dict_no << "\n" <<
    "File name is " << filename << "\n";
#endif
  if (dict_no >= MAX_DICTS) {
    fadd_set_errno(FADD_TOOMANY_DICTS);
    return -1L;
  }
  if (dict_no >= all_dict_no) {
    all_dict_no = dict_no + 1;
  }
  if (all_dict[dict_no] == NULL) {
    fadd_tuple *t;
    if ((t = open_tuple(filename))) {
      if ((all_dict[dict_no] = new dict_inf) == NULL) {
	fadd_set_errno(FADD_MEM);
	return -1L;
      }
      all_dict[dict_no]->dictionary.tuplep = t;
      all_dict[dict_no]->type = FADD_TUPLE;
      all_dict[dict_no]->user = 1;
      all_dict[dict_no]->dictionary.tuplep->dicts = hash_dicts;
      if (all_dict[dict_no]->dictionary.tuplep->word_dicts != hash_dict_no) {
	fadd_set_errno(all_dict[dict_no]->dictionary.tuplep->word_dicts
		       < hash_dict_no
		       ? FADD_TOOMANY_DICTS : FADD_NOTENOUGH_DICTS);
#ifdef DEBUG
	cerr << "Tuple " << dict_no << " file " << all_dict[dict_no]->filename
	     << " says it uses "
	     << all_dict[dict_no]->dictionary.tuplep->word_dicts
	     << " dictionaries (automata), init_given_tuple() got "
	     << hash_dict_no << " as parameter\n";
	cerr << "Dictionaries in file are:\n";
	for (int ee = 0; ee < all_dict[dict_no]->dictionary.tuplep->word_dicts;
	     ee++) {
	  cerr << "[" << ee << "] (" << hash_dicts[ee] << ") ";
	  if (all_dict[ee] != NULL) {
	    cerr << all_dict[hash_dicts[ee]]->filename;
	  }
	  else {
	    cerr << "(empty)";
	  }
	  cerr << "\n";
	}
	cerr << "Dictionaries in parameters are:\n";
	for (int rr = 0; rr < hash_dict_no; rr++) {
	  cerr << "[" << rr << "] (" << hash_dicts[rr] << ") ";
	  if (all_dict[rr] != NULL) {
	    cerr << all_dict[hash_dicts[rr]]->filename;
	  }
	  else {
	    cerr << "(empty)";
	  }
	  cerr << "\n";
	}
#endif
	return -1L;
      }
    }
    else {
      return -1L;
    }
  }
  else {
    fadd_set_errno(FADD_NTAKEN);
    return -1L;
  }
  return 0L;
}//init_given_tuple

/* Name:	init_tuple
 * Purpose:	Initializes dictionary structures to be used
 *		by word_tuple_grams.
 * Parameters:	dict_list	- (i) list of names of dictionaries.
 * Returns:	Dictionary number or -1 if failed.
 * Remarks:	The first dictionary in the list must be
 *		the principle dictionary - the one not in form of an automaton.
 *		Remaining dictionaries contain words present in tuples.
 *		They should be given in the order of words in those tuples.
 *		The names of dictionaries should be given even
 *		if they appear more than once. In that case, only one
 *		occurence of that dictionary will be loaded.
 *		If init_tuple has failed, the error code is available
 *		by fadd_get_errno().
 */
long int
init_tuple(list_of_words *dict_list)
{
  long int dict_no;
  fadd_tuple *dd;

#ifdef DEBUG
  cerr << "Init tuple\n";
#endif
  if (dict_list) {
    if ((dict_no = get_dict_by_name(dict_list->word)) != -1) {
      all_dict[dict_no]->user++;
    }
    else {
      dict_no = add_tuple(dict_list->word);
    }
    if (dict_no != -1) {
      dd = all_dict[dict_no]->dictionary.tuplep;
      int other_dicts = dd->word_dicts;
      dict_list = dict_list->next;
      for (int i = 0; i < other_dicts; i++) {
	if (dict_list) {
	  int otd;
#ifdef DEBUG
	  cerr << "calling init_dict(" << dict_list->word << ")\n";
#endif
	  if ((otd = init_dict(dict_list->word, FADD_HASH)) != -1) {
	    dd->dicts[i] = otd;
	  }
	  else {
	    for (int j = 0; j < i; j++) {
	      close_dict(j);
	    }
	    return -1L;
	  }
	  dict_list = dict_list->next;
	}
	else {
	  fadd_set_errno(FADD_NOTENOUGH_DICTS);
	  return -1L;
	}
      }
    }
    return dict_no;
  }
  else {
    fadd_set_errno(FADD_EMPTY_DICTLIST);
    return -1L;
  }
}//init_tuple
	  
/* Name:	add_tuple
 * Purpose:	Adds a tuple to a list of opened dictionaries.
 * Parameters:	filename	- (i) name of the file containing dictionary.
 * Returns:	Dictionary number or -1 if failed.
 * Remarks:	None.
 */
long int
add_tuple(const char *filename)
{
  fadd_tuple *t;
#ifdef DEBUG
  cerr << "add tuple\n";
#endif
  if ((t = open_tuple(filename))) {
    return add_dict((fsa *)t, filename, FADD_TUPLE);
  }
  return -1L;
}

/* Name:	open_tuple.
 * Purpose:	Opens a tuple dictionary.
 * Parameters:	filename	- (i) name of the file with the tuple
 *					dictionary.
 * Returns:	A structure describing the tuple or NULL if failed.
 * Remarks:	The structure of the tuple file is as follows:
 *		Signature
 *		01234
 *		\TUPL
 *
 *		Version
 *		5
 *		v
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
 *		s(1) .. s(n+m)
 *
 *		Sign vector
 *		8 + m + n .. 8 + m + n + svs - 1
 *		sign(1) .. sign(m)
 *		Note: svs is a sign vector size, which is the minimal
 *		number of bytes to hold m bits.
 *
 *		/-- for version 0 --\
 *		|                   |
 *		v		    v
 *
 *		Tuples
 *		8 + m + n + svs .. end
 *		(n+m)-tuples of integers of sizes s(1)..s(n+m)
 *
 *		^                   ^
 *		|                   |
 *		\-------------------/
 *
 *		/-- for version 1 --\
 *		|                   |
 *		v                   v
 *
 *		Size of addresses
 *		8 + m + n + svs
 *		as
 *
 *		Address of number columns
 *		9 + m + n + svs
 *		an
 *
 *		Description of columns of the perfect hashing automaton
 *		on strings made of compressed representation of word numbers
 *		(double hashed words).
 *		10 + m + n + svs ..
 *
 *		For each column:
 *			Address of the column	- as bytes
 *			Origin of the column	- as bytes
 *			Column item size	- 1 byte
 *			Hash counter size	- 1 byte
 *			Pointer size		- 1 byte
 *
 *		Perfect hashing columns
 *		9 + m + n + svs + (size of all word numbers) * (2as + 3)
 *
 *		For each column:
 *			Label			- 1 byte
 *			Hash value		- as in the column description
 *			Pointer			- as in the column description
 *
 *		Number columns
 *		an .. end
 *		m-tuples of integers of sizes s(n+1)..s(n+m)
 *
 *		^                   ^
 *		|                   |
 *		\-------------------/
 *
 *              /-- for version 3 --\
 *		|		    |
 *		v		    v
 *
 *		Mantissa size vector (1B for each number, 0 means integer)
 *		8 + m + n + svs .. 8 + 2m + n + svs - 1
 *		ms(1) .. ms(m)
 *
 *		Size of addresses
 *		8 + 2m + n + svs
 *		as
 *
 *		Address of number columns
 *		9 + 2m + n + svs
 *		an
 *
 *		Descriptions of columns for words (keys)
 *		9 + 2m + n + as + svs .. 8 + 2m + n + + svs + as + n * (as + 1)
 *		For each column:
 *			Address			- as bytes
 *			Pointer size		- 1 byte
 *
 *		Word columns
 *		9 + 2m + n + svs + as + n * (as + 1) .. an - 1
 *		For each column:
 *			word hash key		- specified in sizes of words
 *			pointer			- specified in column descr
 *
 *		Number columns
 *		an .. end
 *
 *		Note
 *
 *
 *		^		    ^
 *		|		    |
 *		\-------------------/
 *
 *		*********************
 *
 *		Version 0 is a table, where each row is n packed hash keys
 *		for words, and m packed numbers.
 *
 *		Version 1 (no longer used) is a perfect hashing automaton
 *		for strings consisting of individual bytes of hash codes
 *		for words (double hashing) and a table for packed numbers.
 *
 *		Version 3 is a tree for hash keys for words, and a table
 *		for numbers. In that version, real (floating point) numbers
 *		are used. If the number is an integer, the mantissa size
 *		for it is 0. Otherwise, it is represented as a mantissa
 *		and an exponent. The mantissa is always positive (the sign
 *		is stored in the vector sign).
 *		The columns (levels of tree) for words for version 3 contain
 *		1 additional (bogus) item at the end. The word hash key
 *		for them is set to 0, but the pointer is used to determine
 *		the size of the last subtree.
 *
 *		In case of errors, fadd_get_error() provides error code.
 */
fadd_tuple *
open_tuple(const char *filename)
{
  unsigned char	sig_arc[8];
  streampos	file_size;
#ifdef DEBUG
  cerr << "open tuple\n";
#endif
  if (filename == NULL)
    return NULL;

#ifdef OLD_COMPILER
  ifstream dict(filename, ios::in | ios::nocreate | ios::ate | ios::binary);
#else
  ifstream dict(filename, ios::in | ios::ate | ios::binary);
#endif
  if (dict.bad()) {
    cerr << "Cannot open tuple file " << filename << "\n";
    fadd_set_errno(FADD_DFILE_OPEN);
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
    fadd_set_errno(FADD_DFILE_SEEK);
    return NULL;
  }
  file_size = dict.tellg();
#else
  file_size = dict.tellg();
#endif
  if (!dict.seekg(0L)) {
    cerr << "Seek on dictionary file failed. File is "
         << filename << "\n";
    fadd_set_errno(FADD_DFILE_SEEK);
    return NULL;
  }

  // read and verify signature
  if (!(dict.read((char *)&sig_arc, sizeof(sig_arc)))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    fadd_set_errno(FADD_DFILE_READ);
    return(NULL);
  }
  if (strncmp((char *)sig_arc, "\\TUPL", (size_t)5)) {
    cerr << "Invalid dictionary file (bad magic number): " << filename
      << endl;
    fadd_set_errno(FADD_DFILE_MAGIC);
    return(NULL);
  }

  // read sizes
  fadd_tuple *t;
  if ((t = new fadd_tuple) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
  t->version = sig_arc[5];
  t->word_dicts = sig_arc[6];
  t->num_dicts = sig_arc[7];
  int total = t->word_dicts + t->num_dicts;
  if ((t->sizes = new unsigned char[total]) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
  if (!(dict.read((char *)t->sizes, (size_t)total))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    fadd_set_errno(FADD_DFILE_READ);
    return(NULL);
  }

  // read signs vector
#ifdef DEBUG
  cerr << "Allocating " << ((t->num_dicts + 7) / 8) << " bytes for sign vector"
       << endl;
#endif
  int sign_vec_size = (t->num_dicts + 7) / 8;
  if ((t->signs = new char[sign_vec_size]) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
#ifdef DEBUG
  cerr << "Reading the sign vector" << endl;
#endif
  if (!dict.read((char *)(t->signs), (size_t)(sign_vec_size))) {
    cerr << "Cannot read dictionary file " << filename << "\n";
    fadd_set_errno(FADD_DFILE_READ);
    return(NULL);
  }

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

  if (t->version == 0) {
    // read the tuples
#ifdef DEBUG
    cerr << "allocating and reading the tuple" << endl;
#endif
    if ((t->tuples = new char[(long int)file_size - total - 8 - sign_vec_size])
	== NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if (!(dict.read((char *)t->tuples,
		    (long int)file_size - total - 8 - sign_vec_size))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
    t->dict_size = ((long int)file_size - total - 8 - sign_vec_size) /
      t->tuple_size;
  }

  else if (t->version == 3 || t->version == 4) {
    // Read mantissa size vector
    if ((t->mantissa = new unsigned char[t->num_dicts]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if (!dict.read((char *)(t->mantissa), (size_t)(t->num_dicts))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }

    // Read size of addresses
    char address_size;
    if (!(dict.read(&address_size, (size_t)1))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }

    // Read address of numbers
    long int num_addr;
    char *buf = new char[address_size];
    if (buf == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if (!(dict.read(buf, (size_t)address_size))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
    num_addr = bytes2int((unsigned char *)buf, address_size);

    long int nc_size = 0L;
    if (t->word_dicts != 1) {
      // Read column info on tree columns
      if ((t->tree_col_inf = new col_inf3[t->word_dicts]) == NULL) {
	fadd_set_errno(FADD_MEM);
	return NULL;
      }
      char *buf3 = new char[address_size];
      for (int ci3 = 0; ci3 < t->word_dicts; ci3++) {
	if (!(dict.read(buf3, (size_t)address_size))) {
	  cerr << "Cannot read dictionary file " << filename << "\n";
	  fadd_set_errno(FADD_DFILE_READ);
	  return(NULL);
	}
	t->tree_col_inf[ci3].col_addr = bytes2int((unsigned char *)buf3,
						  address_size);
	if (!(dict.read(buf3, (size_t)1))) {
	  cerr << "Cannot read dictionary file " << filename << "\n";
	  fadd_set_errno(FADD_DFILE_READ);
	  return(NULL);
	}
	t->tree_col_inf[ci3].pointer_size = buf3[0];
      }

      // Read word columns
      long int tree_size = num_addr;
      t->end_of_tree = tree_size;
      if ((t->tuples = new char[tree_size]) == NULL) {
	fadd_set_errno(FADD_MEM);
	return NULL;
      }
      if (!(dict.read((char *)t->tuples, (size_t)tree_size))) {
	cerr << "Cannot read dictionary file " << filename << "\n";
	fadd_set_errno(FADD_DFILE_READ);
	return(NULL);
      }
      delete [] buf3;

      // Calculate size of numerical columns
      nc_size = ((num_addr -
		  t->tree_col_inf[t->word_dicts-1].col_addr) /
		 (t->sizes[t->word_dicts-1] +
		  t->tree_col_inf[t->word_dicts-1].pointer_size)) *
	(t->tuple_size - t->string_size);
      --nc_size;		// take the fake word tuple off the count
      if (t->version == 4 && t->sizes[0] == 0) {
	// See if we still have relative pointers for the first column
	long int x = ((t->word_dicts > 2) ?
		      t->tree_col_inf[2].col_addr :
		      t->end_of_tree);
	x -= t->tree_col_inf[1].col_addr;
	x /= (t->sizes[1] + t->tree_col_inf[1].pointer_size);
	int y = 0;
	while (x > 0) {
	  x >>= 8;
	  y++;
	}
	t->relative = (y != t->tree_col_inf[0].pointer_size);
      }
    }
    else {
      // Calculate the size of numerical columns
      // t->tuple_size is the size of the numerical part if the number
      // of word columns is 1
      nc_size = num_addr * t->tuple_size;
    }

    delete [] buf;
    // Read the numerical part
    if ((t->numbers = new char[nc_size]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if (!(dict.read((char *)t->numbers, (size_t)nc_size))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
  }

  else {			// t->version == 1 || t->version == 2
    // Read size of addresses
    char address_size;
    if (!(dict.read(&address_size, (size_t)1))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
    // Read address of numbers
    long int num_addr;
    char *buf = new char[address_size];
    if (buf == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if (!(dict.read(buf, (size_t)address_size))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
    num_addr = bytes2int((unsigned char *)buf, address_size);
    // Allocate memory for column info
    int vc = (t->version == 1 ? t->string_size : t->word_dicts);
    t->column_inf = new col_inf[vc];
    if (t->column_inf == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    // Read column info
    for (int ci = 0; ci < vc; ci++) {
      if (!(dict.read(buf, (size_t)address_size))) {
	cerr << "Cannot read dictionary file " << filename << "\n";
	fadd_set_errno(FADD_DFILE_READ);
	return(NULL);
      }
      t->column_inf[ci].col_addr = bytes2int((unsigned char *)buf,
					     address_size);
      if (!(dict.read(buf, (size_t)address_size))) {
	cerr << "Cannot read dictionary file " << filename << "\n";
	fadd_set_errno(FADD_DFILE_READ);
	return(NULL);
      }
      t->column_inf[ci].origin = bytes2int((unsigned char *)buf, address_size);
      if (!(dict.read(buf, (size_t)address_size))) {
	cerr << "Cannot read dictionary file " << filename << "\n";
	fadd_set_errno(FADD_DFILE_READ);
	return(NULL);
      }
      t->column_inf[ci].size = bytes2int((unsigned char *)buf, address_size);
      char c;
      if (!(dict.read(&c, (size_t)1))) {
	cerr << "Cannot read dictionary file " << filename << "\n";
	fadd_set_errno(FADD_DFILE_READ);
	return(NULL);
      }
      t->column_inf[ci].hash_size = c;
      if (!(dict.read(&c, (size_t)1))) {
	cerr << "Cannot read dictionary file " << filename << "\n";
	fadd_set_errno(FADD_DFILE_READ);
	return(NULL);
      }
      t->column_inf[ci].pointer_size = c;
      t->column_inf[ci].item_size = (t->version == 1 ? 1 : t->sizes[ci])
	+ t->column_inf[ci].pointer_size + t->column_inf[ci].hash_size;
    }
    long int kupa = t->column_inf[0].col_addr;
    for (int column_nr = 0; column_nr < vc; column_nr++) {
            t->column_inf[column_nr].col_addr -= kupa;
    }
    // Allocate memory for perfect hashing automaton
    int ph_size;
    ph_size = num_addr - (9 + t->num_dicts + t->word_dicts +
			  sign_vec_size + address_size +
			  vc * (3 * address_size + 2));
    if ((t->tuples = new char[ph_size]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    // Read the automaton
    if (!(dict.read((char *)t->tuples, (size_t)ph_size))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
    // Allocate memory for numerical columns
    if ((t->numbers = new char[file_size - streampos(num_addr)]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    // Read numerical columns
    if (!(dict.read((char *)t->numbers, (size_t)(file_size - streampos(num_addr))))) {
      cerr << "Cannot read dictionary file " << filename << "\n";
      fadd_set_errno(FADD_DFILE_READ);
      return(NULL);
    }
    delete [] buf;
  }
  if ((t->dicts = new int[t->word_dicts]) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
  int is;
  if (t->version == 3 || t->version == 4) {
    for (int nit = 0; nit < t->word_dicts - 1; nit++) {
      is = t->sizes[nit] + t->tree_col_inf[nit].pointer_size;
      t->tree_col_inf[nit].nitems =
	(t->tree_col_inf[nit + 1].col_addr - t->tree_col_inf[nit].col_addr)
	/ is - 1;
      t->tree_col_inf[nit].item_size = is;
    }
    is = t->sizes[t->word_dicts - 1] +
      t->tree_col_inf[t->word_dicts - 1].pointer_size;
    t->tree_col_inf[t->word_dicts - 1].nitems =
      (t->end_of_tree - t->tree_col_inf[t->word_dicts - 1].col_addr) / is - 1;
    t->tree_col_inf[t->word_dicts - 1].item_size = is;
  }
  return t;
}//open_tuple


/* Name:	accent_word
 * Purpose:	Finds all words in the dictionary that when stripped
 *		of diacritics return the argument word.
 * Parameters:	word		- (i) word to be searched for;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	List of equivalent words from the dictionary.
 * Remarks:	The automaton contains only words.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
list_of_words *
accent_word(const char *word, const long int dict_no)
{
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_ACCENT) {
    return all_dict[dict_no]->dictionary.fsap->accent_word(word,
							  acc_tables[dict_no]);
  }
  if (dict_no < all_dict_no && all_dict[dict_no]) {
    fadd_set_errno(FADD_BAD_DICT_NO);
  }
  else {
    fadd_set_errno(FADD_TYPE);
  }
  return NULL;
}//accent_word
    

/* Name:	prefix_word
 * Purpose:	Finds a prefix of the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	Word's prefix if any.
 * Remarks:	The automaton contains strings consisting of a word,
 *		a separator, and a capital letter indicating the length
 *		of the prefix (A - 0, B - 1, C - 2, and so on).
 *		No prefix means an empty string is returned.
 *		It is the responsability of the caller to release memory
 *		allocated for the prefix.
 */
char *
prefix_word(const char *word, const long int dict_no)
{
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_PREFIX) {
    return all_dict[dict_no]->dictionary.fsap->find_prefix(word);
  }
  if (dict_no < all_dict_no && all_dict[dict_no]) {
    fadd_set_errno(FADD_BAD_DICT_NO);
  }
  else {
    fadd_set_errno(FADD_TYPE);
  }
  return NULL;
}//prefix_word
    

/* Name:	morph_word
 * Purpose:	Finds all morphological analyses of the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	List of morphological analyses for the word.
 * Remarks:	An analysis consists of the base form and categories.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
list_of_words *
morph_word(const char *word, const long int dict_no)
{
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_MORPH) {
    return all_dict[dict_no]->dictionary.fsap->morph_word(word);
  }
  if (dict_no < all_dict_no && all_dict[dict_no]) {
    fadd_set_errno(FADD_BAD_DICT_NO);
  }
  else {
    fadd_set_errno(FADD_TYPE);
  }
  return NULL;
}//morph_word

/* Name:	invert
 * Class:	None.
 * Purpose:	Inverts a string.
 * Parameters:	word		- (i/o) the word to be inverted.
 * Returns:	The (inverted) word.
 * Remarks:	Inversion is done in situ. The previous contents is destroyed.
 */
char *
invert(char *word)
{
  int	c;
  int	l = strlen(word);
  char	*p1, *p2;

  for (p1 = word, p2 = word + l; p1 < p2;) {
    c = *p1;
    *p1++ = *--p2;
    *p2 = c;
  }
  return word;
}//invert

/* Name:	guess_word
 * Purpose:	Finds all approximative analyses of the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	List of analyses for the word.
 * Remarks:	What is an anylysis depends on the dictionary in use
 *		and on options specified when it was opened.
 *		There are 3 possibilities:
 *		1. Morphological features of a word (POS tags).
 *		2. Canonical form (or stem) of a word.
 *		3. Both, as one string, separated with annotation separator.
 *		If both the lexeme and the morphological features are
 *		returned, they are separated with the annotation separator
 *		character. That character is stored inside the automaton
 *		constituting the dictionary. It is put there using an option
 *		for fsa_build or fsa_ubuild.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
list_of_words *
guess_word(const char *word, const long int dict_no)
{
  char *new_word;
  // Allocate memory for the inversion of the word
  if ((new_word = new char[strlen(word) + 2]) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }
  // Place inverted word in new_word
  if (dict_no < all_dict_no) {
    if (all_dict[dict_no]) {
      if (all_dict[dict_no]->type == FADD_GUESS) {
	list_of_words *result;
	strcpy(new_word + 1, word);
	result = all_dict[dict_no]->dictionary.fsap->guess_word(new_word);
	delete [] new_word;
	return result;
      }
    }
  }
  delete [] new_word;
  if (dict_no < all_dict_no && all_dict[dict_no]) {
    fadd_set_errno(FADD_TYPE);
  }
  else {
    fadd_set_errno(FADD_BAD_DICT_NO);
  }
  return NULL;
}//guess_word

/* Name:	guess_stem
 * Class:	fsa
 * Purpose:	Computes the stem of the lexeme of the inflected word.
 * Parameters:	start		- (i) look at children of that node;
 *		start_char	- (i) length of the prefix, or length
 *					of the infix and length of all
 *					characters that precede the infix;
 *		infix_length	- (i) length of the infix (if found);
 * Returns:	Number of different hypothetical morphological analyses
 *		of the word in the part of the automaton reachable from start.
 * Remarks:	The character in the nodes at this stage is a coded number.
 *		It encodes the number of characters that should be deleted
 *		from the end of the word before a new ending is appended.
 *		The number is computed as the code of the character minus 65
 *		(the code of the letter 'A').
 *
 *		I have changed the condition for allowing a new guess
 *		so that at least one character from the original word
 *		must be kept. This is to limit the number of possible
 *		choices, but it prevents the function from recognizing
 *		correctly some irregular words.
 */
int
fsa::guess_stem(fsa_arc_ptr start, const int start_char,
		const int infix_length)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int reject_from_word;
  int already_found = no_repls;

  if (word_length + 1 >= cand_alloc)
    grow_string(&candidate, &cand_alloc, Max_word_len);
  forallnodes(i) {
    if ((reject_from_word = (next_node.get_letter() - 'A')) >= 0 &&
	reject_from_word + start_char < word_length) {
      strncpy(candidate, word_ff + reject_from_word, word_length - start_char);
      if (infix_length) {
	// copy what precedes infix
	strncpy(candidate + word_length - reject_from_word - start_char,
		word_ff + word_length - start_char + infix_length,
		start_char - infix_length);
	candidate[word_length - reject_from_word - infix_length] = '\0';
      }
      else
	candidate[word_length - reject_from_word - start_char] = '\0';
      invert(candidate);
      print_rest(next_node, word_length - reject_from_word -
		 (infix_length ? infix_length : start_char));
    }
  }
  return no_repls - already_found;
}//fsa::guess_stem

/* Name:	check_prefix
 * Class:	fsa
 * Purpose:	Check if the word contains a prefix.
 * Parameters:	start		- (i) look at children of that node;
 *		char_no		- (i) index of the character checked.
 * Returns:	The number of different morphological analyses found
 *		in the part of the automaton reachable from start.
 * Remarks:	The word in word_ff is inverted.
 *
 *		A suffix (a sequence of character at the end) of the word
 *		was recognized, and the next character in the automaton
 *		is the annotation separator. What can be next is:
 *		1) another annotation separator, meaning that there are
 *			no prefixes to be recognized;
 *		2) other letters beginning prefixes;
 *		3) both of the above.
 *
 *		All prefixes must be recognized in full. The filler character
 *		serves as the end marker for the prefix.
 */
int
fsa::check_prefix(fsa_arc_ptr start, const int char_no)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int prefixes_found = 0;
  int already_found = no_repls;

  forallnodes(i) {
    if (char_no >= word_length)
      break;
    if (next_node.get_letter() == word_ff[word_length - char_no - 1]) {
      prefixes_found = check_prefix(next_node, char_no + 1);
      break;
    }
  }

  if (prefixes_found == 0) {
    // We got here, because no character on arcs leaving this node
    // can be found in prefix
    next_node = start.set_next_node(current_dict);
    forallnodes(j) {
      if (next_node.get_letter() == ANNOT_SEPARATOR) {
	// this is either an annotation separator at the end of a prefix
	// or an annotation separator instead of a prefix
	if (morph_infixes)
	  prefixes_found += check_infix(next_node, char_no);
	else if (only_categories)
	  prefixes_found += print_rest(next_node, 0);
	else
	  prefixes_found += guess_stem(next_node, char_no, 0);
      }
    }
  }
  return prefixes_found - already_found;
}//fsa::check_prefix

/* Name:	check_infix
 * Class:	fsa
 * Purpose:	Check if the word contains an infix.
 * Parameters:	start		- (i) look at children of that node;
 *		char_no		- (i) index of the character checked.
 * Returns:	The number of different morphological analyses found
 *		in the part of the automaton reahcable from start.
 * Remarks:	The word contains prefix if the next character is `A'.
 *		Otherwise the word has an infix, and its length can be
 *		calculated as the character code - the character code of `A'.
 */
int
fsa::check_infix(fsa_arc_ptr start, const int char_no)
{
  fsa_arc_ptr next_node = start.set_next_node(current_dict);
  int infix_length;
  int infixes_found = 0;

  forallnodes(i) {
    if ((infix_length = (next_node.get_letter() - 'A')) >= 0 &&
	infix_length < word_length - char_no){
      infixes_found += guess_stem(next_node, char_no, infix_length);
    }
  }
  return infixes_found;
}//fsa::check_infix

/* Name:	number_word
 * Purpose:	Finds a number associated with the argument word
 *		in the dictionary (automaton).
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	The number assigned to the word in the dictionary.
 * Remarks:	None.
 */
long int
number_word(const char * word, const long int dict_no)
{
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_HASH) {
    fsa *f = all_dict[dict_no]->dictionary.fsap;
    long int r = f->number_word(word);
    return r;
  }
  if (dict_no < all_dict_no && all_dict[dict_no]) {
    fadd_set_errno(FADD_BAD_DICT_NO);
  }
  else {
    fadd_set_errno(FADD_TYPE);
  }
  return -1L;
}//number_word


/* Name:	word_number
 * Purpose:	Finds a word associated in the dictionary with the argument
 *		number.
 * Parameters:	key		- (i) word number (hash key);
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	The word or NULL if not found.
 * Remarks:	The word is copied into dynamically allocated memory.
 *		It is the responsability of the caller to release the memory.
 */
char *
word_number(const int key, int dict_no)
{
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_HASH) {
    fsa *f = all_dict[dict_no]->dictionary.fsap;
    char *r = f->word_number(key);
    return (r ? nstrdup(r) : r);
  }
  if (dict_no < all_dict_no && all_dict[dict_no]) {
    fadd_set_errno(FADD_BAD_DICT_NO);
  }
  else {
    fadd_set_errno(FADD_TYPE);
  }
  return NULL;
}

/* Name:	word_tuple_grams
 * Purpose:	Finds a list of numbers associated with a tuple of words.
 * Parameters:	word_list	- (i) list of words in a tuple;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of numbers associated with the tuple.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
list_of_numbers *
word_tuple_grams(list_of_words* word_list, const long int dict_no)
{
  int			c;
  long int		n = 0L;
  list_of_numbers	*nl = NULL, *nl1 = NULL;

  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_TUPLE) {
    fadd_tuple *t = all_dict[dict_no]->dictionary.tuplep;
    char *str;
    if ((str = new char[t->string_size + 1]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    char *s = str;
    if (t->version == 0) {
      for (int i = 0; i < t->word_dicts; i++) {
	if (word_list == NULL) {
	  cerr << "word list too short" << endl;
	  fadd_set_errno(FADD_WL_TOOSHORT);
	  return NULL;
	}
	n = number_word(word_list->word, t->dicts[i]);
	if (n < 0) {
#ifdef DEBUG
	  cerr << "word not found" << endl;
#endif
	  fadd_set_errno(FADD_WORD_NOTFOUND);
	  delete [] str;
	  return NULL;
	}
	int nnn = n;		// save the word number for future generations
	for (int j = 0; j < t->sizes[i]; j++) {
	  *s++ = (n & 0xFF);
	  n >>= 8;
	}
	n = nnn;		// restore word number
	word_list = word_list->next;
      }// for i
      if (t->word_dicts == 1) {
	// The word number is not stored in the tuple, it is an index
	const char *ss = t->tuples + n * t->tuple_size;
	for (int k = 0; k < t->num_dicts; k++) {
	  list_of_numbers *p;
	  if ((p = new list_of_numbers) == NULL) {
	    fadd_set_errno(FADD_MEM);
	    delete [] str;
	    return NULL;
	  }
	  p->word = bytes2int((unsigned char *)ss,
			      t->sizes[k + t->word_dicts]);
	  ss += t->sizes[k + t->word_dicts];
	  if ((t->signs[k >> 3] & (1 << (k & 3))) && (*(ss - 1) & 0x80) != 0) {
	    for (unsigned int ii = t->sizes[k + t->word_dicts];
		 ii < sizeof(long int); ii++) {
	      p->word |= (0xFF << ii);
	    }
	  }
	  p->next = NULL;
	  if (nl) {
	    nl1->next = p;
	  }
	  else {
	    nl = p;
	  }
	  nl1 = p;
#ifdef DEBUG
	  cerr << "Retrieved " << p->word << endl;
#endif
	}//for k
	delete [] str;
	return nl;
      }// if t->word_dicts == 1

      // Now str containes a string that identifies all words in the tuple
      // Look for that string in tuples
      int l = 0;
      int r = t->dict_size - 1;
      nl = nl1 = NULL;
      while (l <= r) {
#ifdef INTERPOLATED
	int m = interp(str, l, r, t);
#else
	int m = (l + r) / 2;
#endif
	if ((c = memcmp((void *)str, (void *)(t->tuples + m * t->tuple_size),
			(size_t)t->string_size)) == 0) {
	  // The right tuple found, produce a list of numbers
	  const char *ss = t->tuples + m * t->tuple_size
	    + t->string_size;
	  for (int k = 0; k < t->num_dicts; k++) {
	    list_of_numbers *p;
	    if ((p = new list_of_numbers) == NULL) {
	      fadd_set_errno(FADD_MEM);
	      delete [] str;
	      return NULL;
	    }
	    p->word = bytes2int((unsigned char *)ss,
				t->sizes[k + t->word_dicts]);
	    ss += t->sizes[k + t->word_dicts];
	    if ((t->signs[k >> 3] & (1 << (k & 3))) && (*(ss - 1) & 0x80) != 0) {
	      for (unsigned int ii = t->sizes[k + t->word_dicts];
		   ii < sizeof(long int); ii++) {
		p->word |= (0xFF << ii);
	      }
	    }
	    p->next = NULL;
	    if (nl) {
	      nl1->next = p;
	    }
	    else {
	      nl = p;
	    }
	    nl1 = p;
#ifdef DEBUG
	    cerr << "Retrieved " << p->word << endl;
#endif
	  }
	  delete [] str;	// it was allocated earlier
	  return nl;
	}//if c
	else if (c > 0)
	  l = m + 1;
	else
	  r = m - 1;
      }//while l <= r
      delete [] str;		// it was allocated earlier
      return NULL;
    }//if version == 0

    if (t->version == 1 || t->version == 2) {	// strings
      int hv = 0;
      int base = 0;
      int column = 0;
      int c_offset = 0;
      for (int i = 0; i < t->word_dicts; i++) {
	if (word_list == NULL) {
	  cerr << "word list too short" << endl;
	  fadd_set_errno(FADD_WL_TOOSHORT);
	  delete [] str;
	  return NULL;
	}
	n = number_word(word_list->word, t->dicts[i]);
	word_list = word_list->next;
	if (n < 0) {
#ifdef DEBUG
	  cerr << "word not found" << endl;
#endif
	  fadd_set_errno(FADD_WORD_NOTFOUND);
	  delete [] str;
	  return NULL;
	}
	if (t->version == 1) {
	  for (int bin = 0; bin < t->sizes[i]; bin++) {
	    c_offset = (n & 0xFF);
	    n >>= 8;
	    if (base + c_offset - t->column_inf[column].origin < 0) {
	      delete [] str;
	      return NULL;
	    }
	    const char *t_ptr = t->tuples + t->column_inf[column].col_addr +
	      (c_offset + (base - t->column_inf[column].origin)) *
	      t->column_inf[column].item_size;
	    if (c_offset != t_ptr[0]) {
	      delete [] str;
	      return NULL;
	    }
	    hv += bytes2int((unsigned char *)t_ptr + 1,
			    t->column_inf[column].hash_size);
	    base = bytes2int((unsigned char *)t_ptr + 1 +
			     t->column_inf[column].hash_size,
			     t->column_inf[column].pointer_size);
	    column++;
	  }
	}//if t->version == 1
	else {			// t->version == 2
	  if (base + n - t->column_inf[i].origin < 0) {
	    delete [] str;
	    return NULL;
	  }
	  const char *t_ptr = t->tuples + t->column_inf[i].col_addr +
	    (n + (base - t->column_inf[i].origin)) *
	    t->column_inf[i].item_size;
	  if (bytes2int((unsigned char *)t_ptr, t->sizes[i]) != n) {
	    delete [] str;
	    return NULL;
	  }
	  hv += bytes2int((unsigned char *)t_ptr + t->sizes[i],
			  t->column_inf[i].hash_size);
	  base = bytes2int((unsigned char *)t_ptr + t->sizes[i] +
			   t->column_inf[i].hash_size,
			   t->column_inf[i].pointer_size);
	}//t->version == 2
      }// for i (for each word number in word tuple)
      // Now hv contains tuple number for numeric columns
      const char *nn = t->numbers + hv * (t->tuple_size - t->string_size);
      for (int kk = 0; kk < t->num_dicts; kk++) {
	list_of_numbers *pp;
	if ((pp = new list_of_numbers) == NULL) {
	  fadd_set_errno(FADD_MEM);
	  delete [] str;
	  return NULL;
	}
	pp->word = bytes2int((unsigned char *)nn,
			     t->sizes[kk + t->word_dicts]);
	nn += t->sizes[kk + t->word_dicts];
	if ((t->signs[kk >> 3] & (1 << (kk & 3))) && (*(nn - 1) & 0x80) != 0) {
	  for (unsigned int ii = t->sizes[kk + t->word_dicts];
	       ii < sizeof(long int); ii++) {
	    pp->word |= (0xFF << ii);
	  }
	}
	pp->next = NULL;
	if (nl) {
	  nl1->next = pp;
	}
	else {
	  nl = pp;
	}
	nl1 = pp;
#ifdef DEBUG
	cerr << "Retrieved " << pp->word << endl;
#endif
      }
      delete [] str;
      return nl;
    }//if t->version == 1|2
    delete [] str;
    return NULL;
  }
  fadd_set_errno(dict_no < all_dict_no ? FADD_TOOMANY_DICTS : FADD_TYPE);
  return NULL;
}//word_tuple_grams


/* Name:	word_tuple_fpgrams
 * Class:	None.
 * Purpose:	Finds a list of floating-point numbers associated
 *		with a tuple of words.
 * Parameters:	word_list	- (i) list of words in the tuple;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of floating-point numbers associated with the tuple,
 *		or NULL if not found.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 *		Only tuples version 3 may contain floating-point numbers.
 *		Integers are converted to floating point.
 */
list_of_fpnumbers *
word_tuple_fpgrams(list_of_words *word_list, const long int dict_no)
{
  fadd_set_errno(FADD_OK);
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_TUPLE) {
    fadd_tuple *t = all_dict[dict_no]->dictionary.tuplep;
    if (t->version != 3 && t->version != 4) {
      fadd_set_errno(FADD_TYPE);
      return NULL;
    }
    long int result_index;
    if ((result_index = find_keys(word_list, t)) == -1L) {
      return NULL;
    }
    return get_fpnumbers(result_index, t);
  }
  fadd_set_errno(dict_no < all_dict_no ? FADD_TOOMANY_DICTS : FADD_TYPE);
  return NULL;
}//word_tuple_fpgrams

/* Name:	prefix_fpgrams
 * Class:	None.
 * Purpose:	Finds tuples that begin with the given words.
 * Parameters:	word_list	- (i) list of initial words in the tuple;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A tuple subtree reachable with the prefix.
 * Remarks:	In a tuple, the word part is a unique identifier.
 *		There can only be one tuple with it.
 *		This does not hold, when we take only some initial words
 *		from the word part. Because there may be many tuples
 *		that begin with the given prefix, the function returns
 *		a forest. This helps keeping memory requirements down.
 *		This function works with tuples version 3 and 4.
 */
tree_of_fpnumbers *
prefix_fpgrams(list_of_words *word_list, const long int dict_no)
{
  fadd_set_errno(FADD_OK);
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_TUPLE) {
    fadd_tuple *t = all_dict[dict_no]->dictionary.tuplep;
    if (t->version != 3 && t->version != 4) {
      fadd_set_errno(FADD_TYPE);
      return NULL;
    }
    long int result_index;
    int pref_len;		// length of the prefix
    if ((result_index = find_subkeys(word_list, t, &pref_len)) == -1L) {
      return NULL;
    }
    return get_fpsubtree(result_index, t, pref_len);
  }
  fadd_set_errno(dict_no < all_dict_no ? FADD_TOOMANY_DICTS : FADD_TYPE);
  return NULL;
}//prefix_fpgrams

/* Name:	find_keys
 * Class:	None.
 * Purpose:	Finds an index of the numerical part of a tuple identified
 *		by the string part of it (the keys).
 * Parameters:	word_list	- (i) list of words;
 *		t		- (i) tuple description.
 * Returns:	The index of the numerical part of the tuple if words match
 *		the string part, -1L otherwise.
 * Remarks:	What is returned is an index, a tuple number, not an offset
 *		in bytes. This is done in order to save space.
 *		The numerical part of the tuple has the same index
 *		as the entry in the last word column.
 */
long int
find_keys(list_of_words *word_list, const fadd_tuple *t)
{
  if (t->word_dicts == 1) {
    return number_word(word_list->word, t->dicts[0]);
  }
  list_of_words *wl = word_list;
  long l = 0;
  long col_offset = t->tree_col_inf[0].col_addr;
//  long r = ((((t->word_dicts - 1 == 0) ?
//		t->end_of_tree :
//		t->tree_col_inf[1].col_addr)
//	       - col_offset) / (t->sizes[0] + t->tree_col_inf[0].pointer_size));
  long r = t->tree_col_inf[0].nitems;
  --r;
  for (int i = 0; i < t->word_dicts; i++) {
    col_offset = t->tree_col_inf[i].col_addr;
    int ws = t->sizes[i];	// key size for ith column
    int es = ws + t->tree_col_inf[i].pointer_size; // entry size for ith col
    long n = number_word(wl->word, t->dicts[i]); // ith key
    if (n < 0) {
      fadd_set_errno(FADD_WORD_NOTFOUND);
      return -1L;
    }
    long n_read = 0;
    long m = 0;
    if (i == 0 && t->sizes[0] == 0) {
      // The hash key in the first column is not written;
      // it is an index to a vector of pointers
      // Go there directly without searching
      l = bytes2int((unsigned char *)t->tuples + col_offset + n * es, es);
      r = bytes2int((unsigned char *)t->tuples + col_offset + (n + 1) * es,
		    es);
      if (t->version == 4 && t->relative) {
	l += n;
	r += n; // used to be (n + 1); we take last before the next
      }
      if (l > r) {		// used to be ===
	return -1L;
      }
    }
    else {
      // Search for the hash key using binary search
      while (l <= r) {
	m = (l + r) / 2;
	n_read = bytes2int((unsigned char *)t->tuples + col_offset + m * es,
			   ws);
	if (n_read == n)
	  break;
	if (n_read < n) {
	  l = m + 1;
	}
	else {
	  r = m - 1;
	}
      }
      if (n_read != n) {
	return -1L;
      }
      if (i < t->word_dicts - 1) {
	l = bytes2int((unsigned char *)t->tuples + col_offset + m * es + ws,
		      t->tree_col_inf[i].pointer_size);
	if (t->version == 4) {
	  l += m;
	}
	r = bytes2int((unsigned char *)t->tuples + col_offset + (m + 1) * es
		      + ws, t->tree_col_inf[i].pointer_size);
	if (t->version == 4) {
	  r += m + 1;
	}
      }
      else {
	// There are no pointers in the last word column
	// Numerical part index is the index of the last word hash key
	// in the tuple
	l  = m;
      }
    }
    if (wl) {
      wl = wl->next;
    }
    else {
      fadd_set_errno(FADD_WL_TOOSHORT);
      return -1L;
    }
  }
  return l;
}//find_keys

/* Name:	find_subkeys
 * Class:	None.
 * Purpose:	Finds an index of the last word (or rather its hash key)
 *		of a prefix (a series of initial items) of a tuple
 *		in a tuple tree.
 * Parameters:	word_list	- (i) list of initial words;
 *		t		- (i) tuple description;
 *		plen		- (o) length of the prefix.
 * Returns:	The index of the rest of the tuple in the tree if the initial
 *		part matches, -1L otherwise.
 * Remarks:	This works with tuple versions 3 and 4.
 *		As in those versions, tuples are stored in a tree,
 *		the prefix (a few initial words, or rather their hash keys)
 *		refers to a unique subtree. That subtree is returned.
 */
long int
find_subkeys(list_of_words *word_list, const fadd_tuple *t, int *plen)
{
  if (t->word_dicts == 1) {
    // The same as normal grams - just one word column
    return number_word(word_list->word, t->dicts[0]);
  }
  list_of_words *wl = word_list;
  long l = 0;
  long col_offset = t->tree_col_inf[0].col_addr;
  long r = t->tree_col_inf[0].nitems - 1;
  for (int i = 0; i < t->word_dicts && wl; i++) {
    col_offset = t->tree_col_inf[i].col_addr;
    int ws = t->sizes[i];	// key size for ith column
    int es = ws + t->tree_col_inf[i].pointer_size; // entry size for ith col
    long n = number_word(wl->word, t->dicts[i]); // ith key
    if (n < 0) {
      fadd_set_errno(FADD_WORD_NOTFOUND);
      return -1L;
    }
    long n_read = 0;
    long m = 0;
    if (i == 0 && t->sizes[0] == 0) {
      // The hash key in the first column is not written;
      // it is an index to a vector of pointers
      l = bytes2int((unsigned char *)t->tuples + col_offset + n * es, es);
      r = bytes2int((unsigned char *)t->tuples + col_offset + (n + 1) * es,
		    es);
      if (t->version == 4 && t->relative) {
	l += n;
	r += n;
      }
      if (l > r) {
	return -1L;
      }
    }
    else {
      while (l <= r) {
	m = (l + r) / 2;
	n_read = bytes2int((unsigned char *)t->tuples + col_offset + m * es,
			   ws);
	if (n_read == n)
	  break;
	if (n_read < n) {
	  l = m + 1;
	}
	else {
	  r = m - 1;
	}
      }
      if (n_read != n) {
	return -1L;
      }
      if (i < t->word_dicts - 1) {
	l = bytes2int((unsigned char *)t->tuples + col_offset + m * es + ws,
		      t->tree_col_inf[i].pointer_size);
	if (t->version == 4) {
	  l += m;
	}
	r = bytes2int((unsigned char *)t->tuples + col_offset + (m + 1) * es
		      + ws, t->tree_col_inf[i].pointer_size);
	if (t->version == 4) {
	  r += m + 1;
	}
      }
      else {
	l = m;
      }
    }
    wl = wl->next;
    *plen = i;
  }
  return l;
}//find_subkeys

/* Name:	hashkey_tuple_fpgrams
 * Class:	None.
 * Purpose:	Finds a list of floating-point numbers associated
 *		with a tuple of hash keys of words.
 * Parameters:	word_list	- (i) list of hash keys of words in the tuple;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of floating-point numbers associated with the tuple,
 *		or NULL if not found.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 *		Only tuples version 3 may contain floating-point numbers.
 *		Integers are converted to floating point.
 *		This function is identical to word_tuple_fpgrams, except
 *		for the fact that it uses hash keys of words instead of
 *		the words themselves.
 */
list_of_fpnumbers *
hashkey_tuple_fpgrams(list_of_numbers *word_list, const long int dict_no)
{
  fadd_set_errno(FADD_OK);
  if (dict_no < all_dict_no && all_dict[dict_no] &&
      all_dict[dict_no]->type == FADD_TUPLE) {
    fadd_tuple *t = all_dict[dict_no]->dictionary.tuplep;
    if (t->version != 3 && t->version != 4) {
      fadd_set_errno(FADD_TYPE);
      return NULL;
    }
    long int result_index;
    if ((result_index = find_hashed_keys(word_list, t)) == -1L) {
      return NULL;
    }
    return get_fpnumbers(result_index, t);
  }
  fadd_set_errno(dict_no < all_dict_no ? FADD_TOOMANY_DICTS : FADD_TYPE);
  return NULL;
}//hashkey_tuple_fpgrams

/* Name:	find_hashed_keys
 * Class:	None.
 * Purpose:	Finds an index of the numerical part of a tuple identified
 *		by the string part of it (the keys).
 * Parameters:	word_list	- (i) list of hash keys of words;
 *		t		- (i) tuple description.
 * Returns:	The index of the numerical part of the tuple if words match
 *		the string part, -1L otherwise.
 * Remarks:	What is returned is an index, a tuple number, not an offset
 *		in bytes. This is done in order to save space.
 *		The numerical part of the tuple has the same index
 *		as the entry in the last word column.
 *		In contrast to find_keys(), this function uses hash keys
 *		of words instead of words themselves.
 */
long int
find_hashed_keys(list_of_numbers *word_list, const fadd_tuple *t)
{
  if (t->word_dicts == 1) {
    return word_list->word;
  }
  list_of_numbers *wl = word_list;
  long l = 0;
  long col_offset = t->tree_col_inf[0].col_addr;
  long r = t->tree_col_inf[0].nitems - 1;
  for (int i = 0; i < t->word_dicts; i++) {
    col_offset = t->tree_col_inf[i].col_addr;
    int ws = t->sizes[i];	// key size for ith column
    int es = ws + t->tree_col_inf[i].pointer_size; // entry size for ith col
    long n = wl->word;		// ith key
    if (n < 0) {
      fadd_set_errno(FADD_WORD_NOTFOUND);
      return -1L;
    }
    long n_read = 0;
    long m = 0;
    if (i == 0 && t->sizes[0] == 0) {
      // The hash key in the first column is not written;
      // it is an index to a vector of pointers
      l = bytes2int((unsigned char *)t->tuples + col_offset + n * es, es);
      r = bytes2int((unsigned char *)t->tuples + col_offset + (n + 1) * es,
		    es);
      if (t->version == 4 && t->relative) {
	l += n;
	r += n;
      }
      if (l > r) {
	return -1L;
      }
    }
    else {
      while (l <= r) {
	m = (l + r) / 2;
	n_read = bytes2int((unsigned char *)t->tuples + col_offset + m * es,
			   ws);
	if (n_read == n)
	  break;
	if (n_read < n) {
	  l = m + 1;
	}
	else {
	  r = m - 1;
	}
      }
      if (n_read != n) {
	return -1L;
      }
      if (i < t->word_dicts - 1) {
	l = bytes2int((unsigned char *)t->tuples + col_offset + m * es + ws,
		      t->tree_col_inf[i].pointer_size);
	if (t->version == 4) {
	  l += m;
	}
	r = bytes2int((unsigned char *)t->tuples + col_offset + (m + 1) * es
		      + ws, t->tree_col_inf[i].pointer_size);
	if (t->version == 4) {
	  r += m + 1;
	}
      }
      else {
	l = m;
      }
    }
    if (wl) {
      wl = wl->next;
    }
    else {
      fadd_set_errno(FADD_WL_TOOSHORT);
      return -1L;
    }
  }
  return l;
}//find_hashed_keys

/* Name:	get_fpnumbers
 * Class:	None.
 * Purpose:	Returns a list of floating-point numbers at a given index
 *		in a numerical part of tuples.
 * Parameters:	indx		- (i) tuple number;
 *		t		- (i) tuple description.
 * Returns:	A list of floating-point numbers.
 * Remarks:	Intergers are converted to floating-point.
 */
list_of_fpnumbers *
get_fpnumbers(const long int indx, const fadd_tuple *t)
{
  const char *buf = t->numbers +
    indx * (t->tuple_size -
	    ((t->word_dicts == 1) ? 0 : t->string_size));
  list_of_fpnumbers *nl = NULL;
  list_of_fpnumbers *nl1 = NULL;
  for (int i = 0; i < t->num_dicts; i++) {
    list_of_fpnumbers *pp;
    if ((pp = new list_of_fpnumbers) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    pp->next = NULL;
    long int ww = 0;
    if (t->mantissa[i] == 0) {
      // Integer
      ww = bytes2int((unsigned char *)buf, t->sizes[t->word_dicts + i]);
      // The purpose of this arcane construction is to pad leading bytes with 1
      // so that the whole is a negative integer
      if ((t->signs[i >> 3] & (1 << (i & 3))) && (*buf & 0x80) != 0) {
	for (unsigned int ii = t->sizes[i + t->word_dicts];
	     ii < sizeof(long int); ii++) {
	  ww |= (0xFF << ii);
	}
      }
      pp->n_type = intn;
      pp->wordp.iword = new long int;
      *(pp->wordp.iword) = ww;
    }
    else {
      // Floating-point
      pp->n_type = realn;
      pp->wordp.rword = new double;
      *(pp->wordp.rword) = get_float(buf, t->sizes[t->word_dicts + i],
				     t->mantissa[i]);
    }
    if (nl) {
      nl1->next = pp;
      nl1 = pp;
    }
    else {
      nl = nl1 = pp;
    }
    buf += t->sizes[i + t->word_dicts];
  }
  return nl;
}//get_fpnumbers

/* Name:	get_float
 * Class:	None.
 * Purpose:	Converts a string of bytes (an internal representation)
 *		to a floating-point number.
 * Parameters:	buf		- (i) string of bytes;
 *		s		- (i) string size;
 *		m		- (i) mantissa size.
 * Returns:	The floating-point number.
 * Remarks:	The internal representation (the string of bytes) has s bytes,
 *		of which m are taken by the mantissa.
 *		If m is 0, the number is an interger and it is stored as such.
 *		The mantissa is stored as an integer. The value of the mantissa
 *		is the value of the integer stored on m bytes,
 *		divided by 2 ^ (8 * (m - 1) + 7). The mantissa has a sign;
 *		it is stored in the most significant bit. The number is
 *		represented in 2's complement code. The sign vector is not
 *		used for that purpose. The value of the mantissa mv
 *		is either 0 or (0.5 <= |mv| < 1.0).
 *		The exponent is a signed integer number in 2's complement.
 *		If an overflow occurs, HUGE_VAL is returned.
 */
double
get_float(const char *buf, const int s, const int m)
{
  if (m == 0) {
    return get_int(buf, s);
  }
//  double mv = get_int(buf, m) / 128.0;
//  for (int j = m - 1; j > 0; --j) {
//    mv /= 256.0;
//  }
  double mv = 0.0;
  const unsigned char *ubuf = (unsigned char *)buf;
  for (int j = 0; j < m - 1; j++) {
    mv += ubuf[j];
    mv /= 256.0;
  }
  mv += (ubuf[m - 1] & 0x7f);
  mv /= 128.0;
  if ((ubuf[m - 1] & 0x80) != 0) {
    mv = -mv;
  }
  int e = (int) get_int(buf + m, s - m);
  return ldexp(mv, e);
}//get_float

/* Name:	get_int
 * Class:	None.
 * Purpose:	Converts a string of bytes being 2's complement representation
 *		of an integer to an integer.
 * Parameters:	buf		- (i) the string;
 *		s		- (i) number of bytes in the string.
 * Returns:	The integer.
 * Remarks:	Overflows are not checked.
 */
long int
get_int(const char *buf, const int s)
{
  long int sum = 0;
  const unsigned char *b = (const unsigned char *)buf;
  if (*buf & 0x80) {
    // Negative number
    sum = (~*b & 0xff);
    b++;
    for (int j = 1; j < s; j++) {
      sum <<= 8;
      sum += (~*b & 0xff);
      b++;
    }
    sum++;
    sum = -sum;
  }
  else {
    for (int i = 0; i < s; i++) {
      sum <<= 8;
      sum += *b;
      b++;
    }
  }
  return sum;
}//get_int


/* Name:	get_fpsubtree
 * Class:	None.
 * Purpose:	Returns a subtree starting at the given column and index.
 * Parameters:	idx		- (i) index in the column;
 *		t		- (i) tuple description;
 *		col		- (i) tuple column where the subtree starts.
 * Returns:	The subtree.
 * Remarks:	None.
 */
tree_of_fpnumbers *
get_fpsubtree(const long int idx, const fadd_tuple *t, const int col)
{
  long col_offset = t->tree_col_inf[col].col_addr;
  int ws = t->sizes[col];	// key size for ith column
  int ps = t->tree_col_inf[col].pointer_size;
  int es = ws + ps; // entry size for ith col
  long int l = bytes2int((unsigned char *)t->tuples + col_offset +
			 idx * es + ws, ps);
  long int r = bytes2int((unsigned char *)t->tuples + col_offset +
			 idx * es + es + ws, ps);
  if (t->version == 4 && t->relative) {
    l += idx;
    r += idx + 1;
  }
  return get_fpsisters(t, col + 1, l, r);
}//get_fpsubtree

/* Name:	get_fpsisters
 * Class:	None.
 * Purpose:	Returns a forest of subtrees (a list of subtrees) from
 *		the tuple tree.
 * Parameters:	t		- (i) tuple description;
 *		col		- (i) tuple column number;
 *		l		- (i) index of the first subtree;
 *		p		- (i) index of the last subtree.
 * Returns:	A list of subtrees.
 * Remarks:	The subtrees end at the word level, as words in a tuple
 *		identify uniquely the numerical part.
 *		For example, for tuples:
 *		a a b 1 2 3
 *		a a c 2 3 1
 *		a b a 2 1 3
 *		a b c 1 3 2
 *		b a c 3 2 1
 *
 *		the structure returned is:
 *		( (a (a (b (1 2 3))
 *		        (c (2 3 1)))
 *		     (b (a (2 1 3))
 *		        (c (1 3 2))))
 *		  (b (a (c (3 2 1))))).
 */
tree_of_fpnumbers *
get_fpsisters(const fadd_tuple *t, const int col, const long int l,
	      const long int p)
{
  long col_offset = t->tree_col_inf[col].col_addr;
  int ws = t->sizes[col];	// key size for ith column
  int ps = t->tree_col_inf[col].pointer_size;
  int es = ws + ps; // entry size for ith col
  tree_of_fpnumbers *sister_list = NULL;
  tree_of_fpnumbers *last_sister = NULL;
  long int next_pointer = bytes2int((unsigned char *)t->tuples
				    + col_offset + l * es + ws, ps);
  for (int i = l; i <= p; i++) {
    tree_of_fpnumbers *current_sister = new tree_of_fpnumbers;
    current_sister->word.iword = bytes2int((unsigned char *)t->tuples
					   + col_offset + i * es, ws);
    long int l1 = next_pointer;
    next_pointer = bytes2int((unsigned char *)t->tuples
			     + col_offset + (i + 1) * es + ws, ps);
    if (t->version == 4 && t->relative) {
      next_pointer += i + 1;
    }
    if (col == t->word_dicts - 1) {
      // Numerical part
      current_sister->down.downn = get_fpnumbers(l1, t);
    }
    else {
      // Still words (or rather hash keys)
      current_sister->down.downw = get_fpsisters(t, col + 1, l1, next_pointer);
    }
    if (sister_list == NULL) {
      sister_list = current_sister;
    }
    if (last_sister) {
      last_sister->next = current_sister;
    }
    last_sister = current_sister;
  }
  last_sister->next = NULL;
  return sister_list;
}//get_fpsisters
    

#ifdef INTERPOLATED
/* Name:	interp
 * Class:	None.
 * Purpose:	Determines the most likely index of a string in a vector
 *		of strings.
 * Parameters:	str		- (i) the string to be found;
 *		l		- (i) lower index boud;
 *		r		- (i) upper index bound;
 *		t		- (i) information about the tuple
 *					to be searched.
 * Returns:	Most likely index number.
 * Remarks:	This function implements interpolated search for a string
 *		in a set of sorted tuples. The function is not general;
 *		it is implemented to work only on specific data.
 *
 *		The strings (representing tuples in a vector of tuples)
 *		at the lower and upper bound are compared. If the initial
 *		bytes are equal, they are discarded until a difference
 *		is found or the strings are empty (i.e. they are equal).
 *		Then four bytes (or less if there are not enough of them)
 *		of both strings are used to compute corresponding numbers.
 *		The numbers are subtracted to give an estimate
 *		of the difference between the lower and upper bound.
 *		A similar calculation (with the same number of discarded bytes)
 *		is done on the searched for string and the lower bound.
 *		A ratio of of the differences should be the same as the ratio
 *		between the differences of respective indexes.
 */
int
interp(const char *str, const int l, const int r, const fadd_tuple *t)
{
  int offset = 0;
  const char *p1 = t->tuples + l * t->tuple_size;
  const char *p2 = t->tuples + r * t->tuple_size;
  for (; offset < t->string_size; offset++, p1++, p2++) {
    if (*p1 != *p2) {
      break;
    }
  }
  if (offset == t->string_size)
    return ((l + r) / 2);
  int min_len = (t->string_size - offset < 4) ? t->string_size - offset > 4
    : 4;
  unsigned long lv = str2i(t->tuples + l * t->tuple_size, min_len);
  unsigned long denom = str2i(t->tuples + r * t->tuple_size, min_len) - lv;
  if (denom == 0L) {
    return ((l + r) / 2);
  }
  return (str2i(str, min_len) - lv)/denom * (r - l) + l;
}//interp

/* Name:	str2i
 * Class:	None.
 * Purpose:	Converts a string of bytes of given length into an integer.
 * Parameters:	str		- (i) the string;
 *		len		- (i) length of the string.
 * Returns:	The number corresponding to the string.
 * Remarks:	The string is treated as an integer number stored
 *		on a big-endian architecture, i.e. with the most significant
 *		byte first. The lngth should not exceed the size of an integer.
 */
long int
str2i(const char *str, const int len)
{
  long int result = 0L;
  unsigned char *p = (unsigned char *)str;
  for (int l = len; l > 0; --l)
    result = (result << 8) + *p++;
  return result;
}//str2i
#endif
	


/* Name:	close_accent
 * Purpose:	Removes the dictionary from memory.
 * Parameters:	dict_no		- (i) dictionary number.
 * Returns:	0 if succesfull, -1 if failed.
 * Remarks:	Information about diacritics is removed as well if present.
 *		The dictionary should have been opened with init_accent().
 */
long int
close_accent(const long int dict_no)
{
  if (all_dict[dict_no] && all_dict[dict_no]->user == 1)
    delete [] acc_tables[dict_no];
  return close_dict(dict_no);
}//close_accent

/* Name:	close_dict
 * Purpose:	Removes the dictionary from memory.
 * Parameters:	dict_no		- (i) dictionary number.
 * Returns:	0 if successful, -1 if failed.
 * Remarks:	The dictionary should have been opened with init_dict().
 */
long int
close_dict(const long int dict_no)
{
  if (all_dict[dict_no]) {
    if (all_dict[dict_no]->user == 1) {
      delete all_dict[dict_no]->dictionary.fsap;
      delete[] all_dict[dict_no]->filename;
      delete all_dict[dict_no];
      all_dict[dict_no] = NULL;
      return 0L;
    }
    all_dict[dict_no]->user--;
    return 0L;
  }
  return -1L;
}//close_dict


/* Name:	close_tuple
 * Purpose:	Closes a tuple dictionary.
 * Parameters:	dict_no		- (i) dictionary number.
 * Returns:	0 if successful, -1 if failed.
 * Remarks:	Associated dictionaries are closed as well.
 */
long int
close_tuple(const long int dict_no)
{
  if (all_dict[dict_no]) {
    if (all_dict[dict_no]->user == 1) {
      // First close associated dictionaries
      fadd_tuple *t = all_dict[dict_no]->dictionary.tuplep;
      for (int i = 0; i < t->word_dicts; i++) {
	if (close_dict(t->dicts[i]) == -1L) {
	  return -1L;
	}
      }
      delete [] t->sizes;
      delete [] t->tuples;
      delete [] t->dicts;
      delete t;
      delete all_dict[dict_no];
      all_dict[dict_no] = NULL;
      return 0L;
    }
    all_dict[dict_no]->user--;
    return 0L;
  }
  return -1L;
}//close_tuple

/* Name:	fadd_get_errno
 * Purpose:	Finds error number for the last unsuccessful operation.
 * Parameters:	None.
 * Returns:	Error number.
 * Remarks:	Uses global variable fadd_errno.
 */
long int
fadd_get_errno(void)
{
  return fadd_errno;
}

/* Name:	fadd_set_errno
 * Purpose:	Sets error number for the last unsuccessful operation.
 * Parameters:	err_no		- (i) error number.
 * Returns:	Nothing.
 * Remarks:	Changes global variable fadd_errno.
 */
void
fadd_set_errno(const long int err_no)
{
  fadd_errno = err_no;
}

long int
get_all_dict_no(void) 
{
  return all_dict_no;
}

/* Name:	i2str
 * Class:	None.
 * Purpose:	Converts an integer to a sequence of bytes of given length,
 *		least significant byte first.
 * Parameters:	to		- (i/o) where to put the string;
 *		value		- (i) integer value to be converted to string;
 *		size		- (i) length of the string.
 * Returns:	Pointer to the first byte after the string.
 * Remarks:	None.
 */
char *
i2str(char *to, const int value, const int size)
{
  int v = value;
  for (int i = 0; i < size; i++) {
    *to++ = v & 0xff;
    v >>= 8;
  }
  return to;
}//i2str

namespace byte_enc {
  const char *mask 	= "\x7f\x3f\x1f\x0f\x07\x03\x01\xff";
  const char *cmask	= "\x80\xc0\xe0\xf0\xf8\xfc\xfe\xff";
  const char *shit_mask	= "\xff\x7f\x3f\x1f\x0f\x07\x03\x01";
}

/* Name:	encode_byte_string
 * Class:	None.
 * Purpose:	Encodes a string of bytes so that it does not contain any
 *		zeros except for a zero that ends the string, and that
 *		was not part of the original contents.
 * Parameters:	str		- (i/o) string to encode / after encoding;
 *		len		- (i) initial string length.
 * Returns:	The encoded string.
 * Remarks:	Each byte of the encoded string contains 7 bits of the original
 *		byte and 1 on the most significant bit positions. Bits
 *		are shifted to avoid MSB positions. A zero byte is appended
 *		after that transformation. Example:
 *		0x6ed5 is encoded as 0x41ddd500:
 *		 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *		|0 1 1 0 1 1 1 0|1 1 0 1 0 1 0 1|	<- 0x6ed5 0x41ddd500
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+			|
 *									v
 *		 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 ... 2 1 0
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+...+-+-+-+
 *		|1 0 0 0 0 0 0 1|1 1 0 1 1 1 0 1|1 1 0 1 0 1 0 1|0 0     0 0 0|
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+...+-+-+-+
 *
 *		Given l = strlen(str), the length of the encoded string is
 *		l + (l + 6) div 7 + 1
 *
 *		There must be enough space in str for the encoded string.
 */
char *
encode_byte_string(char *str, const int len)
{
  using namespace byte_enc;
  char		carry = 0;
  int 		offset = (len + 7) / 7;

  int nlen = len + offset;
  for (int k = len; k <= nlen; k++)
    str[k] = 0;
  for (int i = len - 1; i >= 0; --i) {
    int j = i % 7;
    carry = (((str[i] & cmask[j]) >> (7 - j)) & shit_mask[7 - j]);
    if (j == 6) {
      --offset;
    }
    str[i + offset - 1] = (((str[i] & mask[j]) << j) | 0x80);
    if (j == 6)
      str[i + offset] = (carry | 0x80);
    else
      str[i + offset] |= (carry | 0x80);
  }
  return str;
}//encode_byte_string

/* Name:	decode_byte_string
 * Purpose:	Decodes a string of bytes that was coded in such way
 *		that it did not contain zeros except for an additional one
 *		ending the string.
 * Parameters:	str		- (i/o) string to decode / decoded string.
 * Returns:	Pointer to the decoded string.
 * Remarks:	Each byte of the encoded string contains 7 bits of the original
 *		byte and 1 on the most significant bit positions. Bits
 *		are shifted to avoid MSB positions. A zero byte is appended
 *		after that transformation. Example:
 *		0x6ed5 is encoded as 0x41ddd500:
 *		 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *		|0 1 1 0 1 1 1 0|1 1 0 1 0 1 0 1|	<- 0x6ed5 0x41ddd500
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+			|
 *									v
 *		 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 ... 2 1 0
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+...+-+-+-+
 *		|1 0 0 0 0 0 0 1|1 1 0 1 1 1 0 1|1 1 0 1 0 1 0 1|0 0     0 0 0|
 *		+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+...+-+-+-+
 *
 *		Given l = strlen(str), the length of the encoded string is
 *		l + (l + 6) div 7 + 1
 */
char *
decode_byte_string(char *str)
{
  using namespace byte_enc;

  int offset = 0;
  for (int counter = 0; str[offset]; str++) {
    *str &= 0x7f;
    *str = ((((str[offset] & 0x7f) >> counter) & shit_mask[counter]) |
	    ((str[offset + 1] & mask[6 - counter]) << (7 - counter)));
    counter++;
    if (counter == 7) {
      counter = 0;
      offset++;
    }
  }
  return str;
}//decode_byte_string

/* Name:	save_lib_state
 * Class:	None.
 * Purpose:	Saves information about opened dictionaries in a string.
 * Parameters:	None.
 * Returns:	A string allocated in dynamic memory containing information
 *		about the state of the library.
 * Remarks:	The structure of the string is as follows:
 *				+-+
 *		0		| |	- magic number (D)
 *				+-+
 *		1		| |	- int size in file (i)
 *				+-+
 *		2		| |	- string pointer size (p)
 *				+-++
 *		3		|  |	- number of dictionaries (d)
 *				+--+
 *		3+i		|  |	- pointer to the second dict desc
 *				+--+
 *				:  :
 *				+--+
 *		3+(d-2)*p	|  |	- pointer to the last dict desc
 *				+--+
 *		3+(d-1)*p	|  |	- pointer to tuple descs
 *				+-+++-+...+-+
 *		3+d*p		| | |       |	- 1st dict desc
 *				+-+-+-+...+-+
 *				| | |       |	- 2nd dict desc
 *				+-+-+-+...+-+
 *				: : :       :
 *				+-+-+-+...+-+
 *				| | |       |	- last dict desc
 *				+-+++-+...+-+
 *		[3+(d-1)*p]	|  |		- number of tuples
 *				+--+
 *		[3+(d-1)*p]+i	|  |		- number of fsa for 1st tuple
 *				+--+...+--+
 *		[3+(d-1)*p]+2i	|  |   |  |	- dict nums for 1st tuple
 *				+--+...+--+
 *				|  |		- number of fsa for 2nd tuple
 *				+--+...+--+
 *				|  |   |  |	- dict nums for 2nd tuple
 *				+--+...+--+
 *				:  :
 *				+--+
 *				|  |		- number of fsa for last tuple
 *				+--+...+--+
 *				|  |   |  |	- dict nums for last tuple
 *				+--+...+--+
 *
 *		The structure of a dictionary description is as follows:
 *				+-+
 *		0		| |		- dict type (FADD_HASH etc.)
 *				+-+
 *		1		| |		- dict attribute
 *				+-++
 *		2		|  |		- dictionary number
 *				+-+++...+-+
 *		2+i		|         |	- dictionary file name
 *				+-+-+...+-+
 *
 *		Dictionary attribute depends on the dictionary type.
 *			For FADD_HASH, it is empty.
 *			For FADD_ACCENT, it is character set number.
 *			For FADD_MORPH, it is 1 byte bit vector:
 *			 7 6 5 4 3 2 1 0
 *			+-+-+-+-+-+-+-+-+
 *			| | | | | | | | |
 *			+-+-+-+-+-+-+-+-+
 *			         | | | |
 *				  \ \ \ \______	prefixes
 *			           \ \ \_______	infixes
 *			            \ \________	category only
 *			             \_________ ignore filler
 *		Dictionary file name is a C (0 terminated) string.
 *
 *		No dictionaries are closed by this function,
 *		no memory is released.
 *
 *		The string is encoded so that it contains no zeros,
 *		and a zero is appended to its end making it a C string.
 */
char *
save_lib_state(void)
{
  // Find some sizes
  int active_dicts = 0;
  int names_length = 0;
  int tups = 0;
  int tup_dicts = 0;
  for (int i = 0; i < all_dict_no; i++) {
    if (all_dict[i] != NULL) {
      active_dicts++;
      names_length += (strlen(all_dict[i]->filename) + 1);
      if (all_dict[i]->type == FADD_TUPLE) {
	tups++;
	tup_dicts += all_dict[i]->dictionary.tuplep->word_dicts;
      }//if FSA_TUPLE
    }//if
  }//for i
  int d = active_dicts;
  int int_size;
  for (int_size = 0; d > 0; int_size++) {
    d >>= 8;
  }
  if (int_size == 0) {
    // No dictionaries
    int_size = 1;
  }
  int str_ptr_size = 1;
  int str_length;
  do {
    str_length = 3 +  3 * int_size + names_length +
      active_dicts * (2 + int_size + str_ptr_size) +
      int_size * (tups + tup_dicts + 1);
    if ((str_length >> (8 * str_ptr_size)) > 0) {
      str_ptr_size++;
    }
    else {
      break;
    }
  } while (true);

  // Calculate the length after encoding
  int encoded_length = str_length + (str_length + 6) / 7 + 1;
  // Allocate memory for the string
  char *new_str;
  if ((new_str = new char[encoded_length]) == NULL) {
    fadd_set_errno(FADD_MEM);
    return NULL;
  }

  // Fill the magic number
  new_str[0] = 'D';

  // Fill the int size
  new_str[1] = int_size;

  // Fill the pointer size
  new_str[2] = str_ptr_size;

  // Fill the number of dictionaries
  i2str(new_str + 3, active_dicts, int_size);

#ifdef DEBUG
  cerr << "Saving " << active_dicts << " dicts, int_size = " << int_size
       << ", str_ptr_size = " << str_ptr_size << ", string length is "
       << str_length << "\n";
#endif

  // Fill pointer to dictionaries (except the first one, but including
  // a pointer after the last entry), and the dictionaries themselves
  int dd_beg = 3 + int_size + active_dicts * str_ptr_size;
#ifdef DEBUG
  cerr << "Dictionary entries begin at " << dd_beg << "\n";
#endif
  int k = -1;
  for (int j = 0; j < all_dict_no; j++) {
    if (all_dict[j] != NULL) {
      new_str[dd_beg] = all_dict[j]->type;
      new_str[dd_beg + 1] = all_dict[j]->attr;
      i2str(new_str + dd_beg + 2, j, int_size);
      strcpy(new_str + dd_beg + 2 + int_size, all_dict[j]->filename);
      if (k >= 0) {
	i2str(new_str + 3 + int_size + k * str_ptr_size, dd_beg, str_ptr_size);
      }
      k++;
      dd_beg += 3 + int_size + strlen(all_dict[j]->filename);
    }
  }
  if (active_dicts > 0) {
    i2str(new_str + 3 + int_size + k * str_ptr_size, dd_beg, str_ptr_size);
  }

#ifdef DEBUG
  cerr << "Written dict entries, first byte is "
       << hex << (int(*new_str) & 0xff) << dec << "\n";

  cerr << "Tuples begin at " << dd_beg << ", there are "
       << tups << " tuples\n";
#endif

  // Wow, it's so much done already, so we move to tuples
  i2str(new_str + dd_beg, tups, int_size);// number of tuples
  dd_beg += int_size;
  for (int l = 0; l < all_dict_no; l++) {
    if (all_dict[l] != NULL && all_dict[l]->type == FADD_TUPLE) {
      int dn = all_dict[l]->dictionary.tuplep->word_dicts;
#ifdef DEBUG
      cerr << "Tuple #" << l << "(" << all_dict[l]->filename << ")"
	   << " has " << dn << " dictionaries @" << dd_beg << ":\n";
#endif
      // Write tuple number
      i2str(new_str + dd_beg, l, int_size);
      dd_beg += int_size;
      // Write number of dictionaries for the tuple
      i2str(new_str + dd_beg, dn, int_size);
      dd_beg += int_size;
      for (int d = 0; d < dn; d++) {
#ifdef DEBUG
	cerr << "[" << d << "] #" << all_dict[l]->dictionary.tuplep->dicts[d]
	     << " "
	     << all_dict[all_dict[l]->dictionary.tuplep->dicts[d]]->filename
	     << "\n";
#endif
	// Write dictionary number of a d-th dictionary of the tuple
	i2str(new_str + dd_beg, all_dict[l]->dictionary.tuplep->dicts[d],
	      int_size);
	dd_beg += int_size;
      }
    }
  }
  encode_byte_string(new_str, str_length);
  return new_str;
}//save_lib_state


/* Name:	restore_lib_state
 * Class:	None.
 * Purpose:	Opens the dictionaries that were open during the call
 *		to save_lib_state().
 * Parameters:	saved		- (i) saved state description.
 * Returns:	0 if OK, error code if not.
 * Remarks:	There should be no open dictionaries at this stage.
 *		Any opened dictionaries may result in error in assignment
 *		of the requested number and the consequent failure of the call.
 *
 *		The structure of the string is as follows:
 *				+-+
 *		0		| |	- magic number (D)
 *				+-+
 *		1		| |	- int size in file (i)
 *				+-+
 *		2		| |	- string pointer size (p)
 *				+-++
 *		3		|  |	- number of dictionaries (d)
 *				+--+
 *		3+i		|  |	- pointer to the second dict desc
 *				+--+
 *				:  :
 *				+--+
 *		3+(d-2)*p	|  |	- pointer to the last dict desc
 *				+--+
 *		3+(d-1)*p	|  |	- pointer to tuple descs
 *				+-+++-+...+-+
 *		3+d*p		| | |       |	- 1st dict desc
 *				+-+-+-+...+-+
 *				| | |       |	- 2nd dict desc
 *				+-+-+-+...+-+
 *				: : :       :
 *				+-+-+-+...+-+
 *				| | |       |	- last dict desc
 *				+-+++-+...+-+
 *		[3+(d-1)*p]	|  |		- number of tuples
 *				+--+
 *		[3+(d-1)*p]+i	|  |		- number of fsa for 1st tuple
 *				+--+...+--+
 *		[3+(d-1)*p]+2i	|  |   |  |	- dict nums for 1st tuple
 *				+--+...+--+
 *				|  |		- number of fsa for 2nd tuple
 *				+--+...+--+
 *				|  |   |  |	- dict nums for 2nd tuple
 *				+--+...+--+
 *				:  :
 *				+--+
 *				|  |		- number of fsa for last tuple
 *				+--+...+--+
 *				|  |   |  |	- dict nums for last tuple
 *				+--+...+--+
 *
 *		The structure of a dictionary description is as follows:
 *				+-+
 *		0		| |		- dict type (FADD_HASH etc.)
 *				+-+
 *		1		| |		- dict attribute
 *				+-++
 *		2		|  |		- dictionary number
 *				+-+++...+-+
 *		2+i		|         |	- dictionary file name
 *				+-+-+...+-+
 *
 *		Dictionary attribute depends on the dictionary type.
 *			For FADD_HASH, it is empty.
 *			For FADD_ACCENT, it is character set number.
 *			For FADD_MORPH, it is 1 byte bit vector:
 *			 7 6 5 4 3 2 1 0
 *			+-+-+-+-+-+-+-+-+
 *			| | | | | | | | |
 *			+-+-+-+-+-+-+-+-+
 *			         | | | |
 *				  \ \ \ \______	prefixes
 *			           \ \ \_______	infixes
 *			            \ \________	category only
 *			             \_________ ignore filler
 *		Dictionary file name is a C (0 terminated) string.
 *
 *		The string is encoded so that it contains no zeros,
 *		and a zero is appended to its end making it a C string.
 */
int
restore_lib_state(char *saved)
{
  // Decode the string
  decode_byte_string(saved);

  // Is it the correct string?
  if (*saved != 'D') {
    fadd_set_errno(FADD_SSTR_MAGIC);
    return FADD_SSTR_MAGIC;
  }

  // First open everything but tuples
  int int_size = saved[1];
  int str_ptr_size = saved[2];
  int n = bytes2int((const unsigned char *)(saved + 3), int_size);
  if (n == 0) {
    return 0;
  }
  const char *pointers = saved + 3 + int_size;
  for (int i = 0; i < n; i++) {
    const char *pp = saved + 
      ((i == 0 ? 3 + int_size + n * str_ptr_size :
       bytes2int((const unsigned char *)(pointers + (i - 1) * str_ptr_size),
		str_ptr_size)));
    if (*pp != FADD_TUPLE) {
      switch (*pp) {

      case FADD_ACCENT:
	if (init_given_accent(bytes2int((const unsigned char *)pp + 2,
					int_size),
			      pp + 2 + int_size, pp[1]) == -1) {
	  return fadd_get_errno();
	}
	else
	  break;

      case FADD_MORPH:
	{
	  long int prefixes = ((pp[1] & 1) != 0);
	  long int infixes = ((pp[1] & 2) != 0);
	  long int cat_only = ((pp[1] & 4) != 0);
	  long int ignore_filler = ((pp[1] & 8) != 0);
	  if (init_given_morph(bytes2int((const unsigned char *)pp + 2,
					 int_size),
			       pp + 2 + int_size,
			       prefixes, infixes, cat_only,
			       ignore_filler) == -1L) {
	    return fadd_get_errno();
	  }
	  else
	    break;
	}

      case FADD_GUESS:
	{
	  long int prefixes = ((pp[1] & 1) != 0);
	  long int infixes = ((pp[1] & 2) != 0);
	  long int cat_only = ((pp[1] & 4) != 0);
	  long int ignore_filler = ((pp[1] & 8) != 0);
	  if (init_given_guess(bytes2int((const unsigned char *)pp + 2,
					 int_size),
			       pp + 2 + int_size,
			       prefixes, infixes, cat_only,
			       ignore_filler) == -1L) {
	    return fadd_get_errno();
	  }
	  else
	    break;
	}

      case FADD_PREFIX:
      case FADD_HASH:
	if (init_given_dict(bytes2int((const unsigned char *)pp + 2,
				      int_size),
			    pp + 2 + int_size, *pp) == -1) {
	  return fadd_get_errno();
	}
	else
	  break;

      default:
	cerr << "Oh, holy shit! This is not a dictionary type (0x"
	     << hex << int(*pp) << dec << ")!\n";
	cerr << "This is at byte " << (pp - saved) << " of the string.\n";
	cerr << "String contents is:\n";
	for (char *zz = saved; zz != pp + 10; zz++) {
	  cerr << "[" << dec << (zz - saved) << "] = " << hex
	       << (int(*zz) & 0xff) << dec << "\n";
	}
	fadd_set_errno(FADD_TYPE);
	return FADD_TYPE;
      }//switch
    }//if not tuple
  }//for i

  // Now deal with tuples
  const char *tp = saved + bytes2int((const unsigned char *)(pointers +
		       (n - 1) * str_ptr_size), str_ptr_size) + int_size;
  // Set number of tuples
  int nt = bytes2int((const unsigned char *)(tp - int_size), int_size);
#ifdef DEBUG
  cerr << "There are "
       << nt
       << " tuples\n";
  cerr << "Tuples begin @ " << (tp - saved) << ":\n";
  for (int hh = -5; hh < 20; hh++) {
    cerr << "tp[" << hh << "] " << hex << (int(*(tp + hh)) & 0xff)
	 << dec << "\n";
  }
#endif
  for (int j = 0; j < nt; j++) {
    int tn = bytes2int((const unsigned char *)tp, int_size); // tuple number
    tp += int_size;
    const char *qq = saved +
      ((j == 0) ? 3 + int_size + n * str_ptr_size :
       bytes2int((const unsigned char *)(pointers + (tn - 1) * str_ptr_size),
		 str_ptr_size));
#ifdef DEBUG
    cerr << "Tuple #" << j << " at " << (tp - saved) << "\n";
#endif
    int ts = bytes2int((const unsigned char *)tp, int_size);
    int *td;
    if ((td = new int[ts]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return FADD_MEM;
    }
    tp += int_size;
    for (int k = 0; k < ts; k++) {
      td[k] = bytes2int((const unsigned char *)tp, int_size);
      tp += int_size;
    }
    if (init_given_tuple(tn,	// tuple number
			 qq + 2 + int_size, // tuple file name
			 ts,	// number of dicts in the tuple
			 td) == -1) { // dicts numbers (a vector)
      return fadd_get_errno();
    }
  }//for j
  return 0;
}//restore_lib_state

/* Name:	fadd_init_lib
 * Class:	None.
 * Purpose:	Initializes the library (allocates memory for dictionary
 *		bookkeeping).
 * Parameters:	n		- (i) max number of dictionaries to be used.
 * Returns:	Non-negative library key, or -1 if failed.
 * Remarks:	The number of dictionaries includes dictionaries used by
 *		tuples.
 *		The function can be used to reallocate memory
 *		for new dictionaries. The library can be used independently
 *		in various parts of a program that can initialize and close
 *		the library independently of each other. The number of calls
 *		to fadd_init_lib should be matched with the number of calls
 *		to fadd_close_lib, and the library keys should match as well.
 */
int
fadd_init_lib(const int n)
{
  if (MAX_DICTS > 0) {
    // This is not the first call to the library,
    // add space for more dictionaries
    dict_inf **nall_dict;
    if ((nall_dict = new dict_inf*[MAX_DICTS + n]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1;
    }
    for (int i = 0; i < MAX_DICTS; i++) {
      nall_dict[i] = all_dict[i];
    }
    delete [] all_dict;
    all_dict = nall_dict;
    nall_dict = NULL;
    const char **nacc_tables;
    if ((nacc_tables = new const char *[MAX_DICTS + n]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1;
    }
    for (int j = 0; j < MAX_DICTS; j++) {
      nacc_tables[j] = acc_tables[j];
    }
    delete [] acc_tables;
    acc_tables = nacc_tables;
    nacc_tables = NULL;
    MAX_DICTS += n;
    if (lib_calls >= max_lib_calls) {
      int *nlib_dicts;
      if ((nlib_dicts = new int[max_lib_calls * 2]) == NULL) {
	fadd_set_errno(FADD_MEM);
	return -1;
      }
      delete [] lib_dicts;
      lib_dicts = nlib_dicts;
      nlib_dicts = NULL;
    }
    lib_dicts[lib_calls++] = n;
    active_dicts += n;
    return (lib_calls - 1);
  }
  else {
    if ((all_dict = new dict_inf*[n]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1;
    }
    if ((acc_tables = new const char *[n]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1;
    }
    if ((lib_dicts = new int[max_lib_calls]) == NULL) {
      fadd_set_errno(FADD_MEM);
      return -1;
    }
    lib_calls = 1;
    lib_dicts[0] = 0;
    MAX_DICTS = n;
    all_dict_no = 0;
    active_dicts = n;
    return 0;
  }
}//fadd_init_lib

/* Name:	fadd_close_lib
 * Class:	None.
 * Purpose:	Releases memory hold by the library.
 * Parameters:	lib_key		- (i) library key.
 * Returns:	0 if OK, error code otherwise.
 * Remarks:	The library can be used independantly in various parts
 *		of a program. The library is closed if all users close it.
 *		if the library gets closed, dictionaries get closed
 *		if found open.
 *		Users have separate library keys. They are returned by
 *		fadd_init_lib, and they should be given as arguments
 *		to fadd_close_lib.
 */
int
fadd_close_lib(const int lib_key)
{
  if (lib_dicts[lib_key] <= 0) {
    fadd_set_errno(FADD_LIBKEY);
    return -1;
  }
  active_dicts -= lib_dicts[lib_key];
  lib_dicts[lib_key] = -1;
  if (active_dicts == 0) {
    for (int i = 0; i < MAX_DICTS; i++) {
      if (all_dict[i] != NULL) {
	if (all_dict[i]->type == FADD_TUPLE) {
	  if (close_tuple(i) == -1L) {
	    return fadd_get_errno();
	  }
	}
      }
    }
    for (int j = 0; j < MAX_DICTS; j++) {
      if (all_dict[j] != NULL) {
	if (all_dict[j]->type != FADD_TUPLE) {
	  switch (all_dict[j]->type) {

	  case FADD_ACCENT:
	    if (close_accent(j) == -1L) {
	      return fadd_get_errno();
	    }
	    break;

	  case FADD_MORPH:
	  case FADD_PREFIX:
	  case FADD_HASH:
	  case FADD_GUESS:
	    if (close_dict(j) == -1L) {
	      return fadd_get_errno();
	    }
	    break;

	  default:
	    fadd_set_errno(FADD_TYPE);
	    return(FADD_TYPE);
	  }
	}
      }
    }
    delete [] all_dict; all_dict = NULL;
    delete [] acc_tables; acc_tables = NULL;
    MAX_DICTS = 0;
    all_dict_no = 0;
  }
  return 0;
}//fadd_close_lib

void free_lon_c(list_of_numbers *lon)
{
  return free_low(lon);
}

void free_low_c(list_of_words *low)
{
  return free_low(low);
}
  
/***	EOF fadd.cc	***/
