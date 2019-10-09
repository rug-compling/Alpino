/***	test_fadd.c	***/

/*	Simple program testing fadd library.
 *	Synopsis:
 *
 *	test_fadd a sep words tuple d1 d2...
 *
 *	where:
 *	tuple		- name of the file containing a tuple,
 *	d1 d2...	- names of files holding dictionaries/fsa for tuple
 *				(in reverse order),
 *	a		- data file - source for tuples.
 *	sep		- separator in the tuple source file
 *	words		- number of words in a tuple
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	"fadd.h"

const int VER = 3;		/* tuple version to test */

int main(const int argc, const char *argv[]);

/* Name:	main
 * Purpose:	Launches the program.
 * Parameters:	argc		- (i) number of program arguments,
 *		argv		- (i) argument values.
 * Returns:	Error code.
 * Remarks:	No checking for errors.
 */
int
main(const int argc, const char *argv[])
{
  list_of_words	*dicts;
  list_of_words	*low;
  list_of_words	*llow;
  list_of_numbers	*lon, *llon;
  list_of_fpnumbers	*lofp, *llofp;
  const char	*tuple_name, *input_name;
  char		*s, *t;
  int		i;
  FILE		*fd;
  long int	dn;
  const char	*sep;
  int		no_words;
  int		word_no;
  int		l;
  char		buf[1024];
  char		*cs;
  int		source_line;
  int		notOK;
  int		lib_key;
  const char *errors[] = {"OK", "invalid magic number in dict file",
			  "cannot open file", "cannot read dict file",
			  "cannot seek in file", "invalid dictionary version",
			  "no dictionary specified",
			  "too few dictionaries specified",
			  "too many dictionaries specified",
			  "too few words given", "word not found",
			  "dictionary without number info",
			  "not enough memory",
			  "prolog convertion routines failed"};

  if (argc < 4) {
    fprintf(stderr, "Usage: %s a sep words tuple d1 d2...\n", argv[0]);
    fprintf(stderr, "  where a is the source tuple file,\n");
    fprintf(stderr, "  sep is separator in in the source tuple file,\n");
    fprintf(stderr, "  words is the number of words in a tuple,\n");
    fprintf(stderr, "  tuple is the binary tuple file,\n");
    fprintf(stderr, "  d1,d2,... are perfect hashing automata for columns.\n");
    fprintf(stderr, "If there are fewer automata than columns...\n");
    exit(1);
  }
  notOK = 0;
  input_name = argv[1];
  sep = argv[2];
  no_words = atoi(argv[3]);
  tuple_name = argv[4];
  llow = dicts = NULL;
  for (i = 4; i < 30; i++) {
    if (argv[i] && strcmp(argv[i], "") != 0) {
      low = (list_of_words *)malloc(sizeof(list_of_words));
      low->next = NULL;
      low->word = argv[i];
      if (llow) {
	llow->next = low;
	llow = low;
      }
      else {
	dicts = low;
	llow = low;
      }
    }
    else {
      break;
    }
  }
  if ((lib_key = fadd_init_lib(50)) == -1) {
    fprintf(stderr, "Initialization of fadd library failed!\n");
    exit(100);
  }
  if ((dn = init_tuple(dicts)) != -1) {
    fd = fopen(input_name, "r");
    source_line = 1;
    while ((s = fgets(buf, 1024, fd))) {
      cs = strdup(s);
      if ((t = strchr(s, '\n'))) {
	*t = '\0';
      }
      llow = dicts = (list_of_words *)malloc(sizeof(list_of_words));
      t = strtok(s, sep);
      llow->word = t;
      /*printf("First word = %s\n", t);*/
      llow->next = NULL;
      word_no = 0;
      while (++word_no < no_words && (t = strtok(NULL, sep))) {
	low = (list_of_words *)malloc(sizeof(list_of_words));
	low->next = NULL;
	low->word = t;
	/*printf("Next_word: %s\n", t);*/
	llow->next = low;
	llow = low;
      }
      if (VER == 3 || VER == 4) {
	lofp = word_tuple_fpgrams(dicts, dn);
	if (lofp == NULL) {
	  fprintf(stderr, "Tuple not found, source[%d] is:\n", source_line);
	  fprintf(stderr, "%s\n", cs);
	  notOK = 1;
	}
	fprintf(stderr, "Source[%d] is:\n", source_line);
	fprintf(stderr, "%s\nResulting numbers:", cs);
	while (lofp) {
	  if (lofp->n_type == intn) {
	    fprintf(stderr, " %ld", *(lofp->wordp.iword));
	  }
	  else {
	    fprintf(stderr, " %g", *(lofp->wordp.rword));
	  }
	  llofp = lofp;
	  lofp = lofp->next;
	  if (llofp->n_type == intn) {
	    free(llofp->wordp.iword);
	  }
	  else {
	    free(llofp->wordp.rword);
	  }
	  free(llofp);
	}
	fprintf(stderr, "\n");
      }
      else {
	lon = word_tuple_grams(dicts, dn);
	if (lon == NULL) {
	  fprintf(stderr, "Tuple not found, source[%d] is:\n", source_line);
	  fprintf(stderr, "%s\n", cs);
	  notOK = 1;
	}
	while (lon) {
	  if (lon->word != (l = atoi(strtok(NULL, sep)))) {
	    fprintf(stderr, "%ld != %d for tuple %d:\n", lon->word, l,
		    source_line);
	    fprintf(stderr, "%s\n", cs);
	    notOK = 1;
	  }
	  /*printf("%ld\n", lon->word);*/
	  llon = lon;
	  lon = lon->next;
	  free(llon);
	}
      }
      while (dicts) {
	low = dicts;
	dicts = dicts->next;
	free(low);
      }
      free(cs);
      source_line++;
    }
  }
  else {
    printf("Error no %ld: %s\n", fadd_get_errno(), errors[fadd_get_errno()]);
  }
  fadd_close_lib(lib_key);
  if (notOK) {
    fprintf(stderr, "There were errors\n");
    return 1;
  }
  fprintf(stderr, "Test OK\n");
  return 0;
}
      
      
/***	EOF test_fadd.c	***/
