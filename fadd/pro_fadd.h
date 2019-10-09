/***	pro_fadd.h	***/
#include <cstdio>

extern "C" {

/* 	Copyright (C) Jan Daciuk, for Alpino grammar, RuG, 2001	*/

/* Name:	pro_init_tuple
 * Purpose:	Initializes dictionary structures to be used
 *		by word_tuple_grams.
 * Parameters:	dict_list	- (i) prolog list of dictionaries.
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
pro_init_tuple(SP_term_ref dict_list);


/* Name:	pro_accent_word
 * Purpose:	Finds all words in the dictionary that when stripped
 *		of diacritics return the argument word.
 * Parameters:	word		- (i) word to be searched for;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	list of equivalent words from the dictionary.
 * Remarks:	The automaton contains only words.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
SP_term_ref
pro_accent_word(const char *word, const long int dict_no);


/* Name:	pro_morph_word
 * Purpose:	Finds all morphological analyses of the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	List of morphological analyses for the word.
 * Remarks:	An analysis consists of the base form and categories.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
SP_term_ref
pro_morph_word(const char *word, const long int dict_no);

/* Name:	pro_guess_word
 * Purpose:	Finds all guesses for the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	List of guesses for the word.
 * Remarks:	An analysis consists of either the base form, the categories,
 *		or both. It depends on the options used when opening
 *		the guessing dictionary, and on the contents of the dictionary.
 *		It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
SP_term_ref
pro_guess_word(const char *word, const long int dict_no);

/* Name:	pro_word_number
 * Purpose:	Finds a word associated with a number (hash key).
 * Parameters:	key		- (i) word number (hash key);
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	The word associated with the number or NULL if not found.
 * Remarks:	The only difference between pro_word_number and word_number
 *		is that the first converts NULL to a pointer to an empty
 *		string.
 */
SP_term_ref
pro_word_number(const int key, const long int dict_no);


/* Name:	pro_word_tuple_grams
 * Purpose:	Finds a list of numbers associated with a tuple of words.
 * Parameters:	word_list	- (i) prolog-compatible list of words;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of numbers associated with the tuple.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
SP_term_ref
pro_word_tuple_grams(SP_term_ref word_list, const long int dict_no);

/* Name:	pro_word_tuple_fpgrams
 * Purpose:	Finds a list of floating-point numbers associated
 *		with a tuple of words.
 * Parameters:	word_list	- (i) prolog-compatible list of words;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of floating-point numbers associated  with the tuple.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
SP_term_ref
pro_word_tuple_fpgrams(SP_term_ref word_list, const long int dict_no);

/* Name:	pro_prefix_word
 * Purpose:	Finds a prefix of the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	Word's prefix if any, or a pointer to an empty string.
 * Remarks:	The automaton contains strings consisting of a word,
 *		a separator, and a number indicating the length of the prefix.
 *		No prefix means an empty string is returned.
 *		It is the responsability of the caller to release memory
 *		allocated for the prefix.
 *
 *		The only difference between pro_prefix_word and prefix_word
 *		is that the first converts NULL to a pointer to an empty
 *		string.
 */
SP_term_ref
pro_prefix_word(const char *word, const long int dict_no);

void 
fadd_init_hook(int when);

}

/***	EOF pro_fadd.h	***/
