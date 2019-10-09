#include <stdio.h>

/*
reads input text, and replaces a single newline by a space.
Multiple newlines are replaced by a single newline.

This is assumed to be useful as the first step for
sentence splitting and tokenization.

cat Text | ./lines | ./tokenize.sh
*/

int main() {
  int c1,c2;
  int lastn=0;
  int nextspace=0;

  while ( ( c2 = getchar() ) != EOF) {
    if (c2 == '\n') {
      if (c1 == '\n') {
	if (!lastn) {
	  putchar('\n');
	  lastn = 1;
	  nextspace=0;
	} 
      } else {
	nextspace=1;
	lastn = 0;
      }
    } else {
      if (nextspace) {
	putchar(' ');
	nextspace=0;
      }
      putchar(c2);
      lastn=0;
    }
    c1=c2;
  }
  return 0;
}
