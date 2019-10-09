#include <stdio.h>
#include <locale.h>
#include <errno.h>
#include <string.h>
#include <wchar.h>
#include <stdlib.h>

int new_t_accepts(wchar_t *in,wchar_t **out,int *max) ;

int main(int argc, char *argv[]) {
  wchar_t *in;
  wchar_t *out;
  int in_max;
  int out_max;
  int offset;
  int return_value;
  wchar_t *ptr;

  in_max = 1000;
  out_max = 1000;
  in = (wchar_t *) malloc ((in_max + 1) * sizeof(wchar_t));
  out = (wchar_t *) malloc ((out_max + 1) * sizeof(wchar_t));
  if (in == NULL || out == NULL) {
    fprintf(stderr,"error: out of memory\n");
    return 2;
  }

  setlocale(LC_ALL,"");

  for(;;) {

    /* read a complete line */
    errno = 0;
    offset = 0;
    for(;;) {

      /* grow input buffer if necessary */
      if (offset > in_max - 2) {
	in_max *= 2;
	in = (wchar_t *) realloc (in, (in_max + 1)  * sizeof(wchar_t));
	if (in == NULL) {
	  fprintf(stderr,"error: out of memory\n");
	  return 2;
	}
      }

      /* read (part of) sentence */
      ptr = fgetws(in+offset,in_max-offset,stdin);

      if (ptr == NULL)
	  break;    /* EOF or error encountered: DONE reading line */

      /* check for newline */
      wchar_t *p = wcschr(in, '\n');
      if (p) {
	*p = '\0';  /* chomp */
	break;      /* newline encountered: DONE reading line */
      }

      offset = wcslen(in);
    } /* end of: read a complete line */

    /* if there is data, process it */
    if (ptr != NULL || offset > 0) {
      return_value = new_t_accepts(in,&out,&out_max);
      switch(return_value) {
      case 0:
	fprintf(stderr,"no\n");
	break;
      case 1:
	printf("%ls\n",out);
	break;
      case 2:
	fprintf(stderr,"error: out of memory\n");
	break;
      }
    }

    /* finish if no more data */
    if (ptr == NULL)
	break;
  }
  if (errno) {
    fprintf(stderr,"%s error: %s\n",argv[0],strerror(errno));
  }
	                                /* end-of-file */

  /* to satisfy valgrind */
  free(in);
  free(out);

  return 0;
}
	  

