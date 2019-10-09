#include "corpustools.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

char *buffer = NULL;
wchar_t *in = NULL;
wchar_t *out = NULL;
size_t bufsize = 0;
TOK_ERROR_ tok_error;

int t_accepts (wchar_t *in, wchar_t *out, int max);

char const *tokenize (char const *src)
{
    size_t
	minlen;

    minlen = strlen (src) * 2 + 1;
    if (bufsize < minlen) {
	bufsize = minlen;
	buffer = (char *) realloc (buffer, bufsize * sizeof (char));
	in = (wchar_t *) realloc (in, bufsize * sizeof (wchar_t));
	out = (wchar_t *) realloc (out, bufsize * sizeof (wchar_t));
	if (!buffer || !in || !out) {
	    tok_error = TOK_MEM;
	    return NULL;
	}
    }

    if (mbstowcs (in, src, bufsize) < 0) {
	tok_error = TOK_ENC;
	return NULL;
    }	    

    if (t_accepts (in, out, bufsize) != 1) {
	tok_error = TOK_FAIL;
	return NULL;
    }

    if (wcstombs (buffer, out, bufsize) < 0) {
	tok_error = TOK_ENC;
	return NULL;
    }

    return buffer;
}

int _test (char const *src)
{
    char
	cbuf [4];
    wchar_t
	wcbuf [4];
    int
	errors;

    errors = 0;

    /* Test supplied argument: assuming function was called with euro sign */
    if (src [0] != '\342' ||
	src [1] != '\202' ||
	src [2] != '\254' ||
	src [3] != '\0') {
	printf ("Encoding in Python failed\n");
	errors++;
    }


    /* Test decoding of UF-8 */
    cbuf [0] = '\342';
    cbuf [1] = '\202';
    cbuf [2] = '\254';
    cbuf [3] = '\0';
    mbstowcs (wcbuf, cbuf, 4);
    if (wcbuf [0] != 8364) {
	printf ("Decoding in C failed\n");
	errors++;
    }

    printf ("Should be identical: %lc == %s\n", wcbuf[0], cbuf);

    /* Test encoding of UF-8 */
    cbuf [0] = cbuf [1] = cbuf [2] = '\0';
    wcbuf [0] = 8364;
    wcbuf [1] = L'\0';
    wcstombs (cbuf, wcbuf, 4);
    if (cbuf [0] != '\342' ||
	cbuf [1] != '\202' ||
	cbuf [2] != '\254' ||
	cbuf [3] != '\0') {
	printf ("Encoding in C failed\n");
	errors++;
    }

    /* return number of errrors */
    return errors;
}
