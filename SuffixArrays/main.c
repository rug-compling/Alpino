/* (c) Gertjan van Noord                                                */
/*                                                                      */
/* This program is free software; you can redistribute it and/or modify */
/* it under the terms of the GNU General Public License as published by */
/* the Free Software Foundation; either version 2 of the License, or    */
/* (at your option) any later version.                                  */
/*                                                                      */
/* This program is distributed in the hope that it will be useful,      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        */
/* GNU General Public License for more details.                         */
/*                                                                      */
/* You should have received a copy of the GNU General Public License    */
/* along with this program; if not, write to the Free Software          */
/* Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.            */
/*                                                                      */
/* cf. the file COPYING                                                 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "fadd.h"

#define LINELENGTH 16384
#define QSORT 1
#define SSORT 2

#define CODES 1
#define WORDS 2

#define corpus_code(i) (i+1<words ? corpus[i]-1 : -1)
#define corpus_word(i) word_number(corpus[i]-1,dict);

int dict=-1;
int *corpus;  /* must be visible for suffix_compare */
char *fsafile = "words.fsa";
char *sep=" ";
char *csep="\t";

int suffix_compare(const void *a,const void *b) {
  int p=*(int *)a;
  int q=*(int *)b;
  while (corpus[p] == corpus[q]) {
    p++; q++;
  }
  return corpus[p] < corpus[q] ? -1 : 1;
}

int ssort(int a[], int s[]);

int identical(int *a, int *b, int n) {
  for(n=n-1;n>-1;n--) {
    if(a[n] != b[n]) return 0;
  }
  return 1;
}

void printit(int j, int ngram, int output, int words, int counter, 
	     int print_n_ngram) {
  int k=0;
  char *word;
  if (print_n_ngram) {
    fprintf(stdout,"%d%s",ngram,csep);
  }
  if (output==CODES) {
    fprintf(stdout,"%d",corpus_code(j));
    while(++k < ngram) 
      fprintf(stdout,"%s%d",sep,corpus_code(j+k));
  }

  if (output==WORDS) {
    if (j+1<words) {
      word=corpus_word(j);
      fprintf(stdout,"%s",word);
      free(word);
    } else {
      fprintf(stdout,"EOF");
    }

    while(++k < ngram) {
      if (j+k+1<words) {
	word=corpus_word(j+k);
	fprintf(stdout,"%s%s",sep,word);
	free(word);
      } else {
	fprintf(stdout,"%sEOF",sep);
      }
    }
  }
  if (counter)
    fprintf(stdout,"%s%d",csep,counter);
  fprintf(stdout,"\n");

}

void usage(char *progname) {
  fprintf(stderr,
"%s [Options] [File]\n"
"\n"
"where Options can be:\n"
"\n"
"-v      Prints some continuation messages to standard error.\n"
"-s      Uses the suffix sort implementation by McIlroy; should be\n"
"        better in cases where your input contains long duplicated strings.\n"
"        This is the default.\n"
"-q      Uses the qsort sort implementation. Is faster for 'easy' input.\n"
"        Is robust against cases where some words in the dictionary are not\n"
"        present in the corpus\n"
"-d DICT Use DICT as the perfect hash dictionary file (default: words.fsa).\n"
"-F STR  Use STR as the separator string between words in the output \n"
"        (default: the space character).\n"
"-G STR  Use STR as the separator string between in the output in front of\n"
"        the count (default: tab). Only makes sense in case of -n.\n"
"-c      Output perfect hash codes, (rather than words).\n"
"-w      Output words (rather than perfect hash codes), this is the default.\n"
"-n INT  The suffix array is printed to standard output. For every suffix,\n"
"        the first INT words (or codes) are printed. If INT=0, then no\n"
"        output is produced. Default: INT=0.\n"
"-N      each Ngram (if printed) is preceded by its length\n"
"-e      If this option is specified, then all intermediate length suffixes\n"
"        are printed too. \n"
"-u      If this option is set, then duplicate lines representing suffixes\n"
"        are only printed once; the number of duplicates is printed at the\n"
"        end of the line. So, in combination with the -n suffix you can use this\n"
"        program to count all ngrams for a particular value of n.\n"
"-f INT  In combination with -u, this option specifies that only those lines\n"
"        should be printed with a count of more than INT.\n"
"-b      Dump the suffix array in binary format.\n"
"-h      This message.\n"
"\n"
"The program reads text from File or standard input. Each line is\n"
"treated as a single word, and that word is mapped to its perfect hash\n"
"code (as defined by the dictionary specified by -d). The suffix array\n"
"is computed. The suffix array is then printed to standard output,\n"
"where for every suffix we print the first INT words (or codes) on a\n"
"single line. Alternatively, the suffix array is printed in binary\n"
"format completely.\n"
"\n"
"Example:\n"
"\n"
"%% tr -s '\\012\\011 ' '\\012' < TEXT | sort -u | fsa_build -N -o words.fsa\n"
"\n"
"followed by one of:\n"
"\n"
"%% tr -s '\\012\\011 ' '\\012' < TEXT | ./suffix_array -n 8\n"
"%% tr -s '\\012\\011 ' '\\012' < TEXT | ./suffix_array -n 3 -u | sort -nr -k 4\n"
"%% tr -s '\\012\\011 ' '\\012' < TEXT | ./suffix_array -b > TEXT.suf\n"
"\n"
"etc, ...\n",progname);
  exit(1);
}

int main( int argc, char *argv[] ) {
  char term[LINELENGTH];   /* for the next term from the input line */
  int *suffix_array;
  int size;
  int c,i,j,k;
  int verbose=0;
  int sort=SSORT;
  int ngram=0;
  int f_ngram=1;
  int print_n_ngram=0;
  int each_ngram=0;
  int found_frequent_ngram=1;  /* only attempt to report ngrams of size N,
                                  if there are frequent examples of N-1
			       */
  int freq=0;
  int binary=0;
  int output=WORDS;
  int words=0;  /* number of words seen */
  int uniq=0;   /* only print uniq lines + count */

  int *tmp;

  char *progname=argv[0];
  FILE *stream;    /* where to read characters from */

  while((c=getopt(argc,argv,"vsq?cewhd:F:G:n:ubf:N")) != -1) {
    switch(c) {
    case 'v': verbose=1;          break;
    case 's': sort=SSORT;         break;
    case 'q': sort=QSORT;         break;
    case 'c': output=CODES;       break;
    case 'w': output=WORDS;       break;
    case 'h': usage(progname);    break;
    case 'd': fsafile=optarg;     break;
    case 'F': sep=optarg;         break;
    case 'G': csep=optarg;        break;
    case 'f': freq=atoi(optarg);  break;
    case 'n': ngram=atoi(optarg); break;
    case 'N': print_n_ngram=1;    break;
    case 'e': each_ngram=1;       break;
    case 'u': uniq=1;             break;
    case 'b': binary=1;           break;
    case '?':
      if (optopt == '?')
	usage(progname);
      else 
	exit(1);
      break;
    default:
      break;

    }
  }

  argv += optind;
  argc -= optind;
  
  if (!argc)
    stream=stdin;
  else
    if (!(stream = fopen(argv[0],"r")) ) {
      fprintf(stderr,"Cannot open %s\n",argv[0]);
      exit(1);
    }

  /* initializes fadd fsa */
  fadd_init_lib(2);
  dict = init_dict(fsafile,FADD_HASH);
  if (dict < 0) {
    fprintf(stderr,"Cannot open dictionary: %s\n",fsafile);
    exit(1);
  }

  /* allocate memory for corpus; we dynamically extend memory later */
  corpus = (int *)malloc(1048576*sizeof(int));
  size=1048576;

  

  /* process each word in the file */
  while(fgets(term,LINELENGTH,stream)) {
    int key;
    term[strlen(term)-1] = '\0'; /* chop */
    if (term[0]=='\0') continue; /* ignore empty lines */
    /* NB: we start numbering keys at 1, since we use 0 as a special
     *  end of file marker (which is sorted before any other word).
     */
    key=number_word(term,dict);
    if (key < 0) {
      fprintf(stderr,
        "Warning: word %d \"%s\" not in dictionary; ignored\n",
	      words,term);
      continue;
    }

    if (words > size-1) {
      size=size+1048576;
      if(verbose) fprintf(stderr,"Extending array to %lu kbytes\n",
			  (size*sizeof(int))/1024);
      tmp = (int *)realloc(corpus,size*sizeof(int));
      if (tmp == NULL) {
	free(corpus);
	fprintf(stderr,"Out of memory (tried to get %lu kbytes)\n",
		(size*sizeof(int))/1024);
	exit(1);
      }
      corpus = tmp;
    }
    corpus[words++]=key+1;
  }

  fclose( stream );

  if (words > size-1) {
    size=size+1048576;
    if(verbose) fprintf(stderr,"Extending array to %lu kbytes\n",
			(size*sizeof(int))/1024);
    tmp = (int *)realloc(corpus,size*sizeof(int));
    if (tmp == NULL) {
      free(corpus);
      fprintf(stderr,"Out of memory (tried to get %lu kbytes)\n",
	      (size*sizeof(int))/1024);
      exit(1);
    }
    corpus=tmp;
  }
  corpus[words++]=0;
  suffix_array=(int *)malloc(words*sizeof(int));  
  if (suffix_array == NULL) {
    fprintf(stderr,"Out of memory trying to allocate the suffix array.\n");
    fprintf(stderr,"At this point, the program needs at least some %lu Mb.\n",(2*size*sizeof(int))/(1024*1024));
    exit(1);
  }
  if(sort==QSORT) {
    if(verbose) fprintf(stderr,"Start qsorting...");
    for(i=0;i<words;i++) suffix_array[i]=i;
    qsort(suffix_array,words,sizeof(int),suffix_compare);
  }
  if(sort==SSORT) {
    int result;
    if(verbose) fprintf(stderr,"Start ssorting...");
    for(i=0;i<words;i++) suffix_array[i]=corpus[i];
    result=ssort(suffix_array,NULL);

    if (result==1) {
      fprintf(stderr,"ssort: out of memory\n");
      exit(1);
    }
    if (result==2) {
      fprintf(stderr,"ssort: bad data\n");
      exit(1);
    }
  }
  if(verbose) fprintf(stderr," done.\n");


  /* print result
   *
   * print suffix array if -n specified and >0; for each suffix
   * we print the first n words; possibly with count
   */
  if (binary) {
    fwrite(suffix_array,words,sizeof(int),stdout);
  }

  if (!each_ngram) {
    f_ngram=ngram;
  }  

  while (!binary && ngram > 0 && f_ngram <= ngram && found_frequent_ngram) {
    int prev[f_ngram];
    int counter=1;

    if (uniq) {
      found_frequent_ngram=0;
      for(i=0;i<f_ngram;i++) prev[i]=-2;
    }
    for(i=0;i<words;i++) {
      j=suffix_array[i];
      if(uniq) {
	if (identical(prev,&corpus[j],f_ngram)) {
	  counter++;
	} else {

	  if (i>0 && counter>freq) {
	    printit(suffix_array[i-1],f_ngram,output,words,counter,
		    print_n_ngram);
	    found_frequent_ngram=1; 
	  }

	  counter=1;
	  for(k=0;k<f_ngram;k++) prev[k]=corpus[j+k];
	}
      } else {
	printit(j,f_ngram,output,words,0,print_n_ngram);
      }
    }
 

    if (uniq) {
      if (i>0 && counter>freq) {
	printit(suffix_array[i-1],f_ngram,output,words,counter,print_n_ngram);
	found_frequent_ngram=1;
      }
    }
    f_ngram++;
  }
  exit(0);
}

