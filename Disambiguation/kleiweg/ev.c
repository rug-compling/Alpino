/*

Gelijke uitkomst als perl script bij compilatie zonder optimalisatie
Andere uitkomst bij compilatie met -O2

*/


/*
 * File: ev.c
 *
 * (c) Peter Kleiweg
 *     Thu Dec 15 01:10:28 2011
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#define my_VERSION "0.01"

#define __NO_MATH_INLINES

#ifdef __WIN32__
#  define my_PATH_SEP '\\'
#else
#  define my_PATH_SEP '/'
#endif

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef enum { FALSE = 0, TRUE = 1} BOOL;

typedef struct {
    char *s;
    double f;
} WEIGHT;



#define MAX(A, B) ((A) > (B) ? (A) : (B))
#define MIN(A, B) ((A) < (B) ? (A) : (B))


#define BUFSIZE 100000

BOOL
    opt_d = FALSE,
    opt_r = FALSE,
    opt_x = FALSE;

double
    opt_u = 0.0;

char
    *opt_w = "data.weights";

WEIGHT
    *weights = NULL;
int
    n_weights = 0,
    max_weights = 0;


int
    arg_c;

char
    prevkey [BUFSIZE + 1],
    buffer [BUFSIZE + 1],
    **arg_v,
    *programname,
    *no_mem_buffer,
    out_of_memory [] = "Out of memory";

long int
    lineno = 0;

FILE
    *fp;

void
    get_programname (char const *argv0),
    process_args (void),
    errit (char const *format, ...),
    syntax (void),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size);
char
    *get_arg (void),
    *s_strdup (char const *s);

BOOL
    GetLine (FILE *);

int cmpsort (void const *p1, void const *p2)
{
    return strcmp (((WEIGHT *)p1)->s, ((WEIGHT *)p2)->s);
}

void read_weights ()
{
    int
	i,
	n;
    double
	f;

    FILE
	*fp;

    if (opt_x)
	return;

    lineno = 0;
    fp = fopen(opt_w, "r");
    if (! fp)
	errit ("Opening file \"%s\": %s", opt_w, strerror (errno));
    while (GetLine (fp)) {
	if (sscanf (buffer, "%*[^|]%n|%lf", &n, &f) < 1)
	    errit ("Error reading file \"%s\", line %li", opt_w, lineno);
	buffer [n] = '\0';
	for (i = 0; i < n_weights; i++)
	    if (! strcmp (weights [i].s, buffer))
		break;
	if (i == n_weights) {
	    if (n_weights == max_weights) {
		max_weights += 100;
		weights = (WEIGHT *) s_realloc (weights, max_weights * sizeof (WEIGHT));
	    }
	    weights [n_weights].s = s_strdup (buffer);
	    weights [n_weights].f = f;
	    n_weights++;
	}

    }
    fclose (fp);

    qsort (weights, n_weights, sizeof (WEIGHT), cmpsort);

}

double score (long int pen, long int n)
{
    if (n)
        return 100.0 * (1.0 - ((double) pen) / n);
    else
        return 100.0;
}

long int pen (long int ov, int corr, long int sys)
{
  return MAX(corr, sys) - ov;
}

int cmpsearch (void const *key, void const *p)
{
    return strcmp ((char *) key, ((WEIGHT *)p)->s);
}

double weight (char *s)
{
    double
	f,
	ww,
	w = 0.0;
    WEIGHT
	*p;

    register char *bp;

    bp = s;
    while (*bp) {
	f = atof (bp);
	while (*bp && (*bp) != '@')
	    bp++;
	if (*bp == '\0') {
	    fprintf (stderr, "Parse failed for line %li: %s\n", lineno, buffer);
	    exit(1);
	}
	s = bp + 1;
	while (*bp && (*bp) != '|')
	    bp++;
	if (*bp) {
	    *bp = '\0';
	    bp++;
	}

	ww = opt_u;
	if (n_weights) {
	    p = (WEIGHT *) bsearch(s, weights, n_weights, sizeof (WEIGHT), cmpsearch);
	    if (p)
		ww = p->f;
	}
	w += f * ww;
    }

    return w;

}


int main (int argc, char *argv [])
{
    long int
	b_correct = 0,
	b_n = 0,
	b_overlap = 0,
	b_pen = 0,
	b_system = 0,
	c_correct = 0,
	c_n = 0,
	c_overlap = 0,
	c_pen = 0,
	c_system = 0,
	exact = 0,
	f_correct = 0,
	f_n = 0,
	f_overlap = 0,
	f_pen = 0,
	f_system = 0,
	i_correct = 0,
	i_overlap = 0,
	i_system = 0,
	lb_correct = 0,
	lb_n = 0,
	lb_overlap = 0,
	lb_pen = 0,
	lb_system = 0,
	lc_correct = 0,
	lc_n = 0,
	lc_overlap = 0,
	lc_pen = 0,
	lc_system = 0,
	this_n = 0,
	this_pen = 0;

    double
	b_av = 0.0,
	b_average = 0.0,
	b_fscore = 0.0,
	b_precision = 0.0,
	b_recall = 0.0,
	b_score = 0.0,
	c_av = 0.0,
	c_average = 0.0,
	c_fscore = 0.0,
	c_precision = 0.0,
	c_recall = 0.0,
	c_score = 0.0,
	f_av = 0.0,
	f_average = 0.0,
	f_fscore = 0.0,
	f_precision = 0.0,
	f_recall = 0.0,
	f_score = 0.0,
	kappa = 0.0,
	kappa_av = 0.0,
	lb_score = 0.0,
	lc_score = 0.0,
	lc_weight = 0.0,
	this_score = 0.0,
	this_weight = 0.0;


    long int
	counter = 0;

    register char *bp;

    no_mem_buffer = (char *) malloc (1024);

    get_programname (argv [0]);

    arg_c = argc;
    arg_v = argv;
    process_args ();

    switch (arg_c) {
        case 1:
            if (isatty (fileno (stdin)))
                syntax ();
            fp = stdin;
            break;
        case 2:
            fp = fopen (arg_v [1], "r");
            if (! fp)
                errit ("Opening file \"%s\": %s", arg_v [1], strerror (errno));
            break;
        default:
            syntax ();
    }

    read_weights ();

    prevkey[0] = '\0';
    lineno = 0;
    while (GetLine (fp)) {
	if (buffer [0] == '#' || buffer [0] == '%')
	    continue;

	/* filename */
	bp = buffer;
	while (*bp && (*bp) != '#')
	    bp++;
	if (*bp == '\0') {
	    fprintf (stderr, "Parse failed for line %li: %s\n", lineno, buffer);
	    continue;
	}
	*bp = '\0';
	bp++;

	/* skip item */
	while (*bp && (*bp) != '#')
	    bp++;
	if (*bp == '\0') {
	    fprintf (stderr, "Parse failed for line %li: %s\n", lineno, buffer);
	    continue;
	}
	bp++;

	/* 3 items */
	i_overlap = atoi(bp);
	while (*bp && (*bp) != '|')
	    bp++;
	if (*bp == '\0') {
	    fprintf (stderr, "Parse failed for line %li: %s\n", lineno, buffer);
	    continue;
	}
	bp++;
	i_correct = atoi(bp);
	while (*bp && (*bp) != '|')
	    bp++;
	if (*bp == '\0') {
	    fprintf (stderr, "Parse failed for line %li: %s\n", lineno, buffer);
	    continue;
	}
	bp++;
	i_system = atoi(bp);
	while (*bp && (*bp) != '#')
	    bp++;
	if (*bp == '\0') {
	    fprintf (stderr, "Parse failed for line %li: %s\n", lineno, buffer);
	    continue;
	}
	bp++;

	this_weight = weight (bp);  /* double */
	this_pen = pen (i_overlap, i_correct, i_system);  /* int */
	this_n = MAX (i_correct, i_system);  /* int */
	this_score = score (this_pen, this_n);  /* double */

	if (! strcmp (buffer, prevkey)) {
	    /* another parse for same sentenc */

	    if (this_weight > lc_weight) {
		lc_weight = this_weight;
		lc_pen = this_pen;
		lc_n = this_n;
		lc_score = this_score;
		lc_overlap = i_overlap;
		lc_correct = i_correct;
		lc_system = i_system;
	    }
	    if (this_score > lb_score) {
		lb_score = this_score;
		lb_pen = this_pen;
		lb_n = this_n;
		lb_overlap = i_overlap;
		lb_correct = i_correct;
		lb_system = i_system;
	    }

	} else {
	    /* new sentence */

	    f_pen += this_pen;
	    f_n += this_n;
	    f_av += this_score;
	    f_overlap += i_overlap;
	    f_correct += i_correct;
	    f_system += i_system;

	    if (prevkey[0]) {

		c_pen += lc_pen;
		c_n += lc_n;
		c_av += score (lc_pen, lc_n);

		b_pen += lb_pen;
		b_n += lb_n;
		b_av += score (lb_pen, lb_n);

		if (score (lc_pen, lc_n) >= score (lb_pen, lb_n)) {
		    exact++;
		}

		b_overlap += lb_overlap;
		b_correct += lb_correct;
		b_system += lb_system;

		c_overlap += lc_overlap;
		c_correct += lc_correct;
		c_system += lc_system;

	    }

	    if (opt_d && prevkey[0]) {
		c_score = score (c_pen, c_n);
		fprintf (stderr,
			 "\t%s test-score\t%.2f\t(%.2f) (exact: %.2f)\n",
			 prevkey, lc_score, c_score, ((double) exact) / counter);
	    }
	    if (opt_r && prevkey[0]) {
		printf ("%s\t%li\t%li\n",
			prevkey, lc_pen, lc_n);
	    }

	    lc_weight = this_weight;
	    lc_n = this_n;
	    lc_pen = this_pen;
	    lc_score = this_score;

	    lb_n = this_n;
	    lb_pen = this_pen;
	    lb_score = this_score;

	    lc_overlap = i_overlap;
	    lc_correct = i_correct;
	    lc_system = i_system;
	    lb_overlap = i_overlap;
	    lb_correct = i_correct;
	    lb_system = i_system;

	    counter++;

	    strcpy (prevkey, buffer);

	}

    }

    c_pen += lc_pen;
    c_n += lc_n;
    c_av += score (lc_pen, lc_n);

    b_pen += lb_pen;
    b_n += lb_n;
    b_av += score (lb_pen, lb_n);

    /*
    if (c_pen >= b_pen)
	exact++;
    */
    if (score (lc_pen, lc_n) >= score (lb_pen, lb_n)) {
	exact++;
    }

    b_overlap += lb_overlap;
    b_correct += lb_correct;
    b_system += lb_system;

    c_overlap += lc_overlap;
    c_correct += lc_correct;
    c_system += lc_system;

    if (opt_d && prevkey[0]) {
	c_score = score (c_pen, c_n);
	lc_score = score (lc_pen, lc_n);
	fprintf (stderr, "\t%s test-score\t%.2f\t(%.2f) (exact: %.2f)\n",
		 prevkey, lc_score, c_score, ((double) exact) / counter);
    }

    if (opt_r)
	printf ("%s\t%li\t%li\n", buffer, lc_pen, lc_n);

    f_score = score (f_pen, f_n);
    b_score = score (b_pen, b_n);
    c_score = score (c_pen, c_n);
    if (b_score != f_score) {
	kappa = 100.0 * (c_score - f_score) / (b_score - f_score);
    } else {
	kappa = atof("NaN");
    }
    c_average = c_av / counter;
    b_average = b_av / counter;
    f_average = f_av / counter;
    if (b_average != f_average) {
	kappa_av = 100.0 * (c_average - f_average) / (b_average - f_average);
    } else {
	kappa_av = atof("NaN");
    }

    if (!opt_r) {
	printf ("\n");
	printf ("exact %.2f\n", ((double) exact) / counter);
	printf ("first-score %.2f %.2f\n", f_score, f_average);
	printf ("best-score  %.2f %.2f\n", b_score, b_average);
	printf ("test-score  %.2f %.2f\n", c_score, c_average);
	printf ("phi-score   %.2f %.2f\n", kappa, kappa_av);
	printf ("first-p/m:  %li     %li\n", f_pen, f_n);
	printf ("best-p/m:   %li     %li\n", b_pen, b_n);
	printf ("test-p/m:   %li     %li\n", c_pen, c_n);
	printf ("first-av:   %f     %li\n", f_av, counter);
	printf ("best-av:    %f     %li\n", b_av, counter);
	printf ("test-av:    %f     %li\n", c_av, counter);

	printf ("first-overlap     %li\n", f_overlap);
	printf ("first-correct     %li\n", f_correct);
	printf ("first-system      %li\n", f_system);
	printf ("best-overlap     %li\n", b_overlap);
	printf ("best-correct     %li\n", b_correct);
	printf ("best-system      %li\n", b_system);
	printf ("test-overlap     %li\n", c_overlap);
	printf ("test-correct     %li\n", c_correct);
	printf ("test-system      %li\n", c_system);

	f_precision = 100.0 * ((double) f_overlap) / f_system;
	f_recall = 100.0 * ((double) f_overlap) / f_correct;
	f_fscore = ((double)(2.0 * f_precision * f_recall)) / (f_precision + f_recall);
	printf ("first-precision  %.2f\n", f_precision);
	printf ("first-recall     %.2f\n", f_recall);
	printf ("first-fscore     %.2f\n", f_fscore);

	b_precision = 100.0 * ((double) b_overlap) / b_system;
	b_recall = 100.0 * ((double) b_overlap) / b_correct;
	b_fscore = ((double) (2 * b_precision * b_recall)) / (b_precision + b_recall);
	printf ("best-precision   %.2f\n", b_precision);
	printf ("best-recall      %.2f\n", b_recall);
	printf ("best-fscore      %.2f\n", b_fscore);

	c_precision = 100.0 * ((double) c_overlap) / c_system;
	c_recall = 100.0 * ((double) c_overlap) / c_correct;
	c_fscore = ((double) (2.0 * c_precision * c_recall)) / (c_precision + c_recall);
	printf ("test-precision   %.2f\n", c_precision);
	printf ("test-recall      %.2f\n", c_recall);
	printf ("test-fscore      %.2f\n", c_fscore);
    }

    return 0;
}

void process_args ()
{
    while (arg_c > 1 && arg_v [1][0] == '-') {
        switch (arg_v [1][1]) {
            case 'd':
		opt_d = TRUE;
                break;
            case 'r':
		opt_r = TRUE;
                break;
            case 'x':
		opt_x = TRUE;
                break;
            case 'u':
                opt_u = atof (get_arg ());
                break;
            case 'w':
                opt_w = s_strdup (get_arg ());
                break;
            default:
                errit ("Illegal option '%s'", arg_v [1]);
        }
	arg_c--;
	arg_v++;
    }
}

BOOL GetLine (FILE *fp)
{
    int
        i;

    for (;;) {
        if (fgets (buffer, BUFSIZE, fp) == NULL)
	    return FALSE;
        lineno++;
        i = strlen (buffer);
        while (i && isspace ((unsigned char) buffer [i - 1]))
            buffer [--i] = '\0';
	if (i)
	    return TRUE;
    }
}


char *get_arg ()
{
    if (arg_v [1][2])
        return arg_v [1] + 2;

    if (arg_c == 2)
        errit ("Missing argument for '%s'", arg_v [1]);

    arg_v++;
    arg_c--;
    return arg_v [1];
}

void errit (char const *format, ...)
{
    va_list
	list;

    fprintf (stderr, "\nError %s: ", programname);

    va_start (list, format);
    vfprintf (stderr, format, list);

    fprintf (stderr, "\n\n");

    exit (1);
}

void get_programname (char const *argv0)
{
#ifdef __MSDOS__
    char
        name [MAXFILE];
    fnsplit (argv0, NULL, NULL, name, NULL);
    programname = strdup (name);
#else
    char
        *p;
    p = strrchr (argv0, my_PATH_SEP);
    if (p)
        programname = strdup (p + 1);
    else
        programname = strdup (argv0);
#endif
}

void *s_malloc (size_t size)
{
    void
	*p;

    p = malloc (size);
    if (! p) {
        free (no_mem_buffer);
	errit (out_of_memory);
    }
    return p;
}

void *s_realloc (void *block, size_t size)
{
    void
	*p;

    p = realloc (block, size);
    if (! p) {
        free (no_mem_buffer);
	errit (out_of_memory);
    }
    return p;
}

char *s_strdup (char const *s)
{
    char
	*s1;

    if (s) {
	s1 = (char *) s_malloc (strlen (s) + 1);
	strcpy (s1, s);
    } else {
	s1 = (char *) s_malloc (1);
	s1 [0] = '\0';
    }
    return s1;
}

void syntax ()
{
    fprintf (
	stderr,
	"\n"
	"Version " my_VERSION "\n"
	"\n"
	"Usage: %s [options] [datafile]\n"
	"\n"
	"Options:\n"
	"  -d          : display continuation messages to standard output\n"
	"  -r          : if set, only the raw number of overlap/correct/system triples per sentence is reported\n"
	"  -u float    : weight of unknown features\n"
	"  -w filename : file containing features and their weights\n"
	"  -x          : don't read weights from file\n"
	"\n",
	programname
    );
    exit (1);
}
