/***	pro_fadd.c	***/

/*	Copyright (c) Jan Daciuk, for Alpino grammar, RuG, 2001	*/

#include	<sicstus/sicstus.h>
#include	<stdlib.h>
#include        <string.h>
#include	"fadd.h"
#include	"pro_fadd.h"

#include <boost/config.hpp>
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
#include <boost/thread.hpp>
boost::mutex fadd_mutex;
#endif

extern "C" {

/* local prototypes */
list_of_words *pro_get_word_list(SP_term_ref word_list);
SP_term_ref pro_make_word_list(list_of_words *low);
SP_term_ref pro_make_word_pair_list(list_of_words *low);
SP_term_ref pro_make_number_list(list_of_numbers *low);
SP_term_ref pro_make_fpnumber_list(list_of_fpnumbers *low);


/* Name:	put_mb_atom
 * Purpose:	Put a string in native locale as an atom.
 */

static int
put_mb_atom(SP_term_ref t, const char *s, size_t len)
{
#ifdef __SWI_PROLOG__
  PL_put_variable(t);
  return PL_unify_chars(t, PL_ATOM|REP_MB, len, s);
#else
  if ( len == (size_t)-1 || strlen(s) == len )
  { return SP_put_string(t, s);
  } else {
    char *tmp = reinterpret_cast<char *>(malloc(len+1));
    int rc;
    strncpy(tmp, s, len);
    tmp[len] = 0;
    rc = SP_put_string(t, tmp);
    free(tmp);
    return rc;
  }
#endif
}


/* Name:        SP_free_low
 * Purpose:     Free a list of words that is allocated through SP_malloc
 * Parameters:  low - list of words
 * Remark:      Maybe lists of words should always be allocated on the
 *              normal heap, rather than the Sicstus heap.
 */
void SP_free_low(list_of_words *low)
{
  while (low != NULL) {
    list_of_words *prev_low = low;
    low = low->next;
    SP_free(prev_low);
  }
}

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
pro_init_tuple(SP_term_ref dict_list)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif
  list_of_words *words = pro_get_word_list(dict_list);
  SP_term_ref h = init_tuple(words);
  SP_free_low(words);
  return h;
}/*pro_init_tuple*/


/* Name:	pro_get_word_list
 * Purpose:	Converts a prolog list to list_of_words.
 * Parameters:	word_list	- (i) prolog list to be converted.
 * Returns:	C word list.
 * Remarks:	None.
 */
list_of_words *
pro_get_word_list(SP_term_ref word_list)
{
  SP_term_ref	tmp, head, tail;
  char *word;
  char *nword;
  list_of_words	*low, *llow, *lstart;
  head = SP_new_term_ref();
  tail = SP_new_term_ref();
  tmp = SP_new_term_ref();
  tmp = word_list;
  lstart = low = llow = NULL;
  word = NULL;
  nword = NULL;
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS) {
    if (SP_get_string(head, &nword) == 0) {
      fprintf(stderr, "No strings attached\n");
    }
    word = nword;

    /*fprintf(stderr, "%s\n", word);*/
    if ((low = (list_of_words *)SP_malloc(sizeof(list_of_words))) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    if ((low->word = nstrdup(word)) == NULL) {
      fadd_set_errno(FADD_MEM);
      return NULL;
    }
    low->next = NULL;
    if (llow) {
      llow->next = low;
      llow = low;
    }
    else {
      lstart = llow = low;
    }
    tmp = tail;
  }
  /*for (low = lstart; low; low = low->next) {
    fprintf(stderr, "(%s)", low->word);
  }
  fprintf(stderr, "\n");*/
  return lstart;
}/*pro_get_word_list*/


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
pro_accent_word(const char *word, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  return pro_make_word_list(accent_word(word, dict_no));
}/*pro_accent_word*/


/* Name:	pro_make_word_list
 * Purpose:	Translate list_of_words list into prolog list.
 * Parameters:	low		- (i) C list.
 * Returns:	Equivalent prolog list.
 * Remarks:	The C list is discarded.
 */
SP_term_ref
pro_make_word_list(list_of_words *low)
{
  SP_term_ref	w, l;
  list_of_words	*llow;
  const char *wd;
  l = SP_new_term_ref();
#ifdef DEBUG
  fprintf(stderr, "Making prolog word list of: \n");
  if (low == NULL) {
    fprintf(stderr, "Empty list\n");
  }
  for (llow = low; llow; llow = llow->next) {
    fprintf(stderr, "(%s)\n", llow->word);
  }
#endif
  if (low) {
#ifdef DEBUG
    fprintf(stderr, "List element: %s\n", low->word);
#endif
    w = SP_new_term_ref();
    wd = low->word;

    if (put_mb_atom(w, wd, -1) == 0) {
      fprintf(stderr, "Word %s could not be converted to prolog string\n",
	      low->word);
      fadd_set_errno(FADD_PROLOG);
    }
    llow = low;
    low = low->next;
    delete[] wd;
    delete llow;
    if (SP_cons_list(l, w , pro_make_word_list(low)) == 0) {
      l = SP_new_term_ref();
    }
    return l;
  }
  return l;
}/*pro_make_word_list*/


/* Name:	pro_make_word_pair_list
 * Purpose:	Translate list_of_words list into prolog list.
		The list is a list of <pre-+>, <post-+> atoms
 * Parameters:	low		- (i) C list.
 * Returns:	Equivalent prolog list.
 * Remarks:	The C list is discarded.
 */
SP_term_ref
pro_make_word_pair_list(list_of_words *low)
{
  SP_term_ref	w, l, m, wt;
  list_of_words	*llow;
  const char *wd;
  l = SP_new_term_ref();

  if (low) {
    wd = low->word;

    int k=strlen(wd)-1;
    while(wd[k] != '+') {
      k--;
    }

    wt = SP_new_term_ref();
    if (put_mb_atom(wt,wd+k+1,-1) == 0) {
      fprintf(stderr, "Word %s could not be converted to prolog string\n",
	      wd+k);
      fadd_set_errno(FADD_PROLOG);
    }


    w = SP_new_term_ref();
    if (put_mb_atom(w, wd, k) == 0) {
      fprintf(stderr, "Word %s could not be converted to prolog string\n",
	      low->word);
      fadd_set_errno(FADD_PROLOG);
    }
    llow = low;
    low = low->next;
    delete[] wd;
    delete llow;
    m = SP_new_term_ref();
    SP_cons_list(m, wt , pro_make_word_pair_list(low));
    SP_cons_list(l, w  , m );
    return l;
  }
  return l;
}/*pro_make_word_list*/


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
pro_morph_word(const char *word, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  return pro_make_word_pair_list(morph_word(word, dict_no));
}/*pro_morph_word*/


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
pro_guess_word(const char *word, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  return pro_make_word_list(guess_word(word, dict_no));
}/*pro_guess_word*/


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
pro_word_number(const int key, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  char *t;
  SP_term_ref l,h;
  l = SP_new_term_ref();
  h = SP_new_term_ref();
  if ((t = word_number(key, dict_no)) == NULL) {
    return l;
  }
  else {
    put_mb_atom(h, t, -1);
    free(t);
    SP_cons_list(l, h, SP_new_term_ref());
    return l;
  }
}/*pro_word_number*/

/* Name:	pro_word_tuple_grams
 * Purpose:	Finds a list of numbers associated with a tuple of words.
 * Parameters:	word_list	- (i) prolog-compatible list of words;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	A list of numbers associated with the tuple.
 * Remarks:	It is the responsability of the caller to release memory
 *		allocated for the resulting list.
 */
SP_term_ref
pro_word_tuple_grams(SP_term_ref word_list, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  list_of_words *words = pro_get_word_list(word_list);
  SP_term_ref numList = pro_make_number_list(word_tuple_grams(words, dict_no));
  SP_free_low(words);
  return numList;
}/*pro_word_tuple_grams*/

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
pro_word_tuple_fpgrams(SP_term_ref word_list, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  list_of_words *words = pro_get_word_list(word_list);
  SP_term_ref numList = pro_make_fpnumber_list(word_tuple_fpgrams(words,
    dict_no));
  SP_free_low(words);
  return numList;
}/*pro_word_tuple_fpgrams*/

/* Name:	pro_make_number_list
 * Purpose:	Convert a C list_of_numbers to a prolog list.
 * Parameters:	number_list	- (i) C list_of_numbers.
 * Returns:	Equivalent prolog list.
 * Remarks:	The C list is discarded.
 */
SP_term_ref
pro_make_number_list(list_of_numbers *low)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  SP_term_ref list = SP_new_term_ref();
  SP_term_ref sublist = SP_new_term_ref();

  /* Make sure we keep a reference to the 'outermost' list */
  SP_put_variable(list);
  SP_put_variable(sublist);
  SP_unify(list, sublist);

  list_of_numbers *cur_low = low;
  while (cur_low) {
    /* Create a list. */
    SP_term_ref tmp = SP_new_term_ref();
    SP_put_list(tmp);
    SP_unify(tmp, sublist);

    /* Get list head and tail. */
    SP_term_ref head = SP_new_term_ref();
    SP_term_ref tail = SP_new_term_ref();
    SP_get_list(sublist, head, tail);

    /* Store current number in the list head. */
    SP_put_integer(tmp, cur_low->word);
    SP_unify(tmp, head);

    /* Next sublist is the tail. */
    SP_put_variable(sublist);
    SP_unify(sublist, tail);

    /* Traverse linked list */
    cur_low = cur_low->next;
  }

  SP_unify(sublist, SP_new_term_ref());

  free_lon_c(low);

  return list;
}/*pro_make_number_list*/

/* Name:	pro_make_fpnumber_list
 * Purpose:	Convert a C list_of_fpnumbers to a prolog list.
 * Parameters:	number_list	- (i) C list_of_fpnumbers.
 * Returns:	Equivalent prolog list.
 * Remarks:	The C list is discarded.
 *		Analogous to pro_make_number_list, but for floating-point.
 */
SP_term_ref
pro_make_fpnumber_list(list_of_fpnumbers *low)
{
  SP_term_ref		w, l;
  list_of_fpnumbers	*llow;
  l = SP_new_term_ref();
  if (low) {
    w = SP_new_term_ref();
    if (low->n_type == intn) {
      SP_put_integer(w, *(low->wordp.iword));
    }
    else {
      SP_put_float(w, *(low->wordp.rword));
    }
    llow = low;
    low = low->next;
    if(llow->n_type == intn) {
      delete llow->wordp.iword;
    }
    else {
      delete llow->wordp.rword;
    }
    delete llow;
    if (SP_cons_list(l, w , pro_make_fpnumber_list(low)) == 0) {
      return l;
    }
    else {
      return l;
    }
  }
  return l;
}/*pro_make_fpnumber_list*/

/* Name:	pro_prefix_word
 * Purpose:	Finds a prefix of the argument word.
 * Parameters:	word		- (i) word to be examined;
 *		dict_no		- (i) dictionary number to be used.
 * Returns:	List with single element: Word's prefix; or empty list.
 * Remarks:	The automaton contains strings consisting of a word,
 *		a separator, and a number indicating the length of the prefix.
 *		No prefix means an empty string is returned.
 *
 *		The only difference between pro_prefix_word and prefix_word
 *		is the conversion of prefix to single element list - or the
 *		empty list in case there is no prefix.
 */
SP_term_ref
pro_prefix_word(const char *word, const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  char *t;
  SP_term_ref l,h;
  l = SP_new_term_ref();
  h = SP_new_term_ref();
  if ((t = prefix_word(word, dict_no)) == NULL) {
    return l;
  }
  else {
    put_mb_atom(h,t,-1);
    free(t);
    SP_cons_list(l,h,SP_new_term_ref());
    return l;
  }
}/*pro_prefix_word*/


long int
associate_word_integer(char *word,const long int dict_no)
{
#if defined(BOOST_HAS_THREADS) && defined(__SWI_PROLOG__)
  boost::mutex::scoped_lock lock(fadd_mutex);
#endif

  list_of_words *low;
  const char *wd;
  long int i=0;

  low = morph_word(word, dict_no);

  if(low) {
    wd = low->word;
    int k=strlen(wd)-1;
    while(wd[k] != '+' && k>0) {
      k--;
    }
    /*    i=atoi(wd+k+1); */
    i = strtol(wd+k+1,NULL,0);
    delete[] wd;
    delete low;
  } 
    return i;
  }

}

/***	EOF pro_fadd	***/
