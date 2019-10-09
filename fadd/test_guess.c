/***	test_guess.c	***/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	"fadd.h"

/* Name:	main
 * Purpose:	Launches the program.
 * Parameters:	argc		- (i) number of parameters of the program;
 *		argv		- (i) the parameters.
 * Returns:	Diagnostic code:
 *		0	- OK;
 *		0-18	- error, see fadd.h;
 *		100	- invalid parameters.
 * Globals:	None.
 * Remarks:	None.
 */
int
main(const int argc, const char *argv[])
{
  long int dict_no;		/* dictionary number */
  FILE *fd;			/* input */
  list_of_words *low;		/* results of guessing */
  list_of_words *llow;
  char *s;			/* input word */
  char *t;			/* end-of-line character position */
  char buf[1024];		/* input buffer */
  int lib_key;			/* library key */

  /* Check parameters */
  if (argc < 5) {
    fprintf(stderr, "Synopsis:\n\n");
    fprintf(stderr, "%s dictionary prefixes infixes cat_only ignore_filler%s",
	    argv[0], " [input_file]\n");
    fprintf(stderr, "\nwhere:\ndictionary is a guessing automaton (FSA),\n");
    fprintf(stderr, "prefixes is 1 if the FSA has prefixes, 0 otherwise,\n");
    fprintf(stderr, "infixes is 1 if the FSA has infixes, 0 otherwise,\n");
    fprintf(stderr, "cat_only is 1 if the dictionary has only categories,");
    fprintf(stderr, " 0 otherwise\n");
    fprintf(stderr, "ignore_filler is 1 if filler is to be ignores, ");
    fprintf(stderr, " 0 otherwise\n");
    return 100;
  }

  /* Initialize the library */
  if ((lib_key = fadd_init_lib(40)) == -1L) {
    fprintf(stderr, "%s: could not initialize the library, code %ld\n",
	    argv[0], fadd_get_errno());
    return 100;
  }

  /* Initialize guessing dictionary */
  if ((dict_no = init_guess(argv[1], atoi(argv[2]), atoi(argv[3]),
			    atoi(argv[4]), atoi(argv[5]))) == -1L) {
    fprintf(stderr, "%s: could not intialize guessing dictionary, code %ld\n",
	    argv[0], fadd_get_errno());
    return dict_no;
  }

  /* Initialize input stream */
  fd = stdin;
  if (argc == 7) {
    /* input file specified */
    fprintf(stderr, "7 parameters specified\n");
    if ((fd = fopen(argv[6], "r")) == NULL) {
      fprintf(stderr, "%s: could not open input file %s\n", argv[0], argv[6]);
      return 100;
    }
  }

  /* Test guessing */
  while ((s = fgets(buf, 1023, fd))) {
    if ((t = strchr(s, '\n'))) {
      *t = '\0';
    }
    printf("%s -", s);
    if ((low = guess_word(s, dict_no)) == NULL) {
      printf(" no guesses found\n");
    }
    else {
      for (llow = low; low; free(llow)) {
	printf(" %s", low->word);
	llow = low;
	low = low->next;
      }
      printf("\n");
    }
  }

  /* Close the library */
  fadd_close_lib(lib_key);

  /* Close input stream */
  if (argc == 6) {
    fclose(fd);
  }
  return 0;
} /* main */

/***	EOF test_guess.c	***/
