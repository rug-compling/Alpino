/***	fadd.h	***/

/*	Copyright (c) Jan Daciuk, for Alpino grammar, RuG, 2001-2003	*/

/*	This file defines an interface to a library of functions
 *	that use finite-state automata for various purposes.
 */

#ifndef FADD_H
#define FADD_H

/* error messages */
#define	FADD_OK			0	/* OK */
#define FADD_DFILE_MAGIC	1	/* invalid magic number in dict file */
#define FADD_DFILE_OPEN		2	/* cannot open file */
#define	FADD_DFILE_READ		3	/* cannot read dict file */
#define	FADD_DFILE_SEEK		4	/* cannot seek in file */
#define FADD_DFILE_VERSION	5	/* invalid dictionary version */
#define FADD_EMPTY_DICTLIST	6	/* no dictionary specified */
#define FADD_NOTENOUGH_DICTS	7	/* too few dictionaries specified */
#define	FADD_TOOMANY_DICTS	8	/* too many dictionaries specified */
#define	FADD_WL_TOOSHORT	9	/* too few words given */
#define FADD_WORD_NOTFOUND	10	/* word not found */
#define	FADD_NOTHASH		11	/* dictionary without number info */
#define	FADD_MEM		12	/* not enough memory */
#define	FADD_PROLOG		13	/* prolog convertion routines failed */
#define FADD_NTAKEN		14	/* dictionary number already used */
#define FADD_SSTR_MAGIC		15	/* bad magic number in saved string */
#define FADD_TYPE		16	/* bad dictionary type */
#define FADD_LIBKEY		17	/* invalid library key */
#define FADD_BAD_DICT_NO	18	/* invalid dictionary number */

/* dictionary types */
#define	FADD_ACCENT		0
#define	FADD_MORPH		1
#define FADD_PREFIX		2
#define FADD_HASH		3
#define FADD_TUPLE		4
#define	FADD_GUESS		5

typedef enum numtype { intn, realn } numtype;

typedef struct list_of_words {
  const char		*word;
  struct list_of_words	*next;
} list_of_words;

typedef struct list_of_numbers {
  long int			word;
  struct list_of_numbers	*next;
} list_of_numbers;

typedef struct list_of_fpnumbers {
  numtype			n_type;	/* real or integer */
  union {
    double 			*rword;	/* pointer to a real value */
    long int			*iword;	/* pointer to an integer value */
  }				wordp; /* pointer to a value */
  struct list_of_fpnumbers	*next; /* next on the list */
} list_of_fpnumbers;

typedef struct tree_of_fpnumbers {
  numtype			n_type;	/* real or int or list */
  union {
    double			rword;	/* real value */
    long int			iword;	/* integer or hash (word) value */
    list_of_fpnumbers		*fplist;/* numerical part */
  }				word; /* a value */
  struct tree_of_fpnumbers	*next; /* next value in the same column */
  union {
    struct tree_of_fpnumbers	*downw; /* next column in the tuple */
    struct list_of_fpnumbers	*downn;	/* numerical columns */
  } down;
} tree_of_fpnumbers;


#ifdef __cplusplus
extern "C" {
#endif

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
		  const long int charset_no);

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
init_accent(const char *filename, const long int charset_no);


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
	       const long int dict_type);


/* Name:	init_dict
 * Purpose:	Initializes a dictionary of strings.
 * Parameters:	filename	- (i) name of the file containing dictionary;
 *		dict_type	- (i) dictionary type.
 * Returns:	Dictionary number, or -1 if failed.
 * Remarks:	If init_prefix has failed, the error code is available
 *		by fadd_get_errno().
 *		This function is appropriate for opening dictionaries for
 *		prefix_word() (with FADD_PREFIX), and number_word
 *		(with FADD_NUMBER). It is called internally
 *		for other types of dictionaries.
 */
long int
init_dict(const char *filename, const long int dict_type);

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
		 const long int cat_only, const long int ignore_filler);


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
	   const long int ignore_filler);


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
	   const long int ignore_filler);


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
		 const long int cat_only, const long int ignore_filler);

/* Name:	init_given_tuple
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
		 const int hash_dict_no, const int *hash_dicts);

/* Name:	init_tuple
 * Purpose:	Initializes dictionary structures to be used
 *		by word_tuple_grams.
 * Parameters:	dict_list	- (i) list of dictionaries.
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
init_tuple(list_of_words *dict_list);


long int
get_all_dict_no(void);

/* Name:	accent_word
 * Purpose:	Finds all words in the dictionary that when stripped
 *		of diacritics return the argument word.
 * Parameters:	word		- (i) word to be searched for;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	list of equivalent words from the dictionary.
 * Remarks:	The automaton contains only words.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
list_of_words *
accent_word(const char *word, const long int dict_no);

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
prefix_word(const char *word, const long int dict_no);

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
morph_word(const char *word, const long int dict_no);


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
guess_word(const char *word, const long int dict_no);

/* Name:	number_word
 * Purpose:	Finds a number associated with the argument word
 *		in the dictionary (automaton).
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	The number assigned to the word in the dictionary.
 * Remarks:	None.
 */
long int
number_word(const char *word, const long int dict_no);

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
word_number(const int key, int dict_no);

/* Name:	word_tuple_grams
 * Purpose:	Finds a list of numbers associated with a tuple of words.
 * Parameters:	word_list	- (i) prolog-compatible list of words;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of numbers associated with the tuple.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
list_of_numbers *
word_tuple_grams(list_of_words *word_list, const long int dict_no);

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
word_tuple_fpgrams(list_of_words *word_list, const long int dict_no);

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
prefix_fpgrams(list_of_words *word_list, const long int dict_no);


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
hashkey_tuple_fpgrams(list_of_numbers *word_list, const long int dict_no);

/* Name:	close_accent
 * Purpose:	Removes the dictionary from memory.
 * Parameters:	dict_no		- (i) dictionary number.
 * Returns:	0 if succesfull, -1 if failed.
 * Remarks:	Information about diacritics is removed as well if present.
 *		The dictionary should have been opened with init_accent().
 */
long int
close_accent(const long int dict_no);

/* For closing morphological dictionaries, you should use close_dict() */
#define	close_morph(X)		close_dict(X)

/* Name:	close_dict
 * Purpose:	Removes the dictionary from memory.
 * Parameters:	dict_no		- (i) dictionary number.
 * Returns:	0 if successful, -1 if failed.
 * Remarks:	The dictionary should have been opened with init_dict().
 */
long int
close_dict(const long int dict_no);

/* Name:	close_tuple
 * Purpose:	Closes a tuple dictionary.
 * Parameters:	dict_no		- (i) dictionary number.
 * Returns:	0 if successful, -1 if failed.
 * Remarks:	Associated dictionaries are closed as well.
 */
long int
close_tuple(const long int dict_no);

/* Name:	fadd_get_errno
 * Purpose:	Finds error number for the last unsuccessful operation.
 * Parameters:	None.
 * Returns:	Error number.
 * Remarks:	None.
 */
long int
fadd_get_errno(void);

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

/* Name:	fadd_set_errno
 * Purpose:	Sets error number for the last unsuccessful operation.
 * Parameters:	err_no		- (i) error number.
 * Returns:	Nothing.
 * Remarks:	Changes global variable fadd_errno.
 */
void
fadd_set_errno(const long int err_no);

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
save_lib_state(void);


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
restore_lib_state(char *saved);

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
fadd_init_lib(const int n);

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
fadd_close_lib(const int lib_key);

void
free_lon_c(list_of_numbers *lon);

void
free_low_c(list_of_words *low);

#ifdef __cplusplus
}
#endif

#endif // FADD_H

/***	EOF fadd.h	***/
