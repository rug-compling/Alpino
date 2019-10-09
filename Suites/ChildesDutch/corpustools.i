%define DOCSTRING
"Tools for working with corpora

Importing this module sets the locale LC_CTYPE to en_US.utf8.

The function tokenize() depends on this setting. If you change the locale
after importing this module, you should undo that change before calling
tokenize().

Tokenize library by Gertjan van Noord
C/Python interface by Peter Kleiweg, using SWIG
"
%enddef
%module(docstring=DOCSTRING) corpustools

%header %{
#include "corpustools.h"
extern TOK_ERROR_ tok_error;
char const *tokenize (char const *s);
int _test (char const *src);
%}

#ifdef PYTHON2
%define TOKSTRING
"Return copy of string with spaces inserted between words and punctuation.

tokenize(string) -> string

The current locale LC_CTYPE should be UTF-8 (set by importing the module). 
Input string must be encoded in UTF-8.
Output string will be encoded in UTF-8.
"
%enddef
#else
%define TOKSTRING
"Return copy of string with spaces inserted between words and punctuation.

tokenize(string) -> string

The current locale LC_CTYPE should be UTF-8 (set by importing the module).
"
%enddef
#endif
%feature("autodoc", TOKSTRING) tokenize;

%exception tokenize {
  $action
  if (!result) {
    if (tok_error == TOK_MEM)
      PyErr_SetString (PyExc_MemoryError, "out of memory");
    else if (tok_error == TOK_ENC)
      PyErr_SetString (PyExc_RuntimeError, "encode/decode failure, wrong locale?");
    else
      PyErr_SetString (PyExc_RuntimeError, "tokenize() failure");
    return NULL;
  }
}

char const *tokenize (char const *s);

int _test (char const *src);

%pythoncode %{
import locale, sys
locale.setlocale(locale.LC_CTYPE, ('en_US', 'utf8'))

def test():
    '''Run tests to see if encoding/decoding of UTF-8 is working.'''
    if sys.version_info[0] < 3:
        i = _test('\xe2\x82\xac')
    else:
        i = _test('\N{EURO SIGN}')
    if i:
        print('There were', i, 'errors. Wrong locale used?', locale.getlocale(locale.LC_CTYPE))
    else:
        print('No errors')
%}
