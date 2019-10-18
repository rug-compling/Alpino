/* This file is generated automatically by FSA Utilities.
 * (c) 1993 - 2000 Gertjan van Noord RUG
 * More information from http://www.let.rug.nl/~vannoord/Fsa/
 * 
 * The representation of a finite-automaton is
 * similar to the technique explained on page 43 (table
 * 4.2) of Jan Daciuk's dissertation `Incremental
 * Construction of Finite-State Automata and Transducers
 * and their use in the Natural Language Processing'.
 * Politechnika Gdanska, 1998, except that instead of the number
 * of transitions we have a boolean flag indicating for each
 * line whether that line is the last transition for the current
 * state.
 */


#include <wchar.h>
#include <stdlib.h>



/*
 * Queue ADT
 *
 * Taken from http://purists.org/queue/
 * Institute of Applied Iconoclasm
 *
 * GvN: added explicit casts for malloc() to prevent warnings
 */

#define QDATUM wchar_t
	  
/*
 *  Queue item structure
 */
struct qentry
{
        struct qentry *next;
        QDATUM d;
};

/*
 *  Queue container structure
 *
 *  This structure encapsulates a linked list of qentry items.
 */
struct queue
{
        struct qentry *begin, **end;
};


/*
 *  qinit: Initialize queue.
 *
 *  Parameters:
 *
 *    q         Pointer to a queue, or NULL if the user wishes to leave 
 *              it to qinit to allocate the queue.
 *
 *  Return values:
 *
 *    non-NULL  Queue has been initialized.
 *
 *    NULL      Insufficient memory.
 */
struct queue *
qinit(struct queue *q)
{
        if (q || (q = (struct queue *) malloc(sizeof(struct queue))) != NULL) {
                q->begin = NULL;
                q->end = &q->begin;
        }
        return q;
}


/*
 *  qinsert: append an item to the queue.
 *
 *  Parameters:
 *
 *    q         Pointer to a queue. It is assumed the queue has been
 *              initialized by a call to qinit.
 *
 *    d         Item to be appended.
 *
 *  Return values:      
 *
 *    1         The item has been appended.
 *
 *    0         The item could not be appended. Either the queue 
 *              pointer provided was NULL, or the function was unable 
 *              to allocate the amount of memory needed for a new 
 *              queue item.     
 */
int
qinsert(struct queue *q, QDATUM d)
{
        if (!q || !(*q->end = (struct qentry *) malloc(sizeof(struct qentry)))) return 0;
        (*q->end)->d = d;
        (*q->end)->next = NULL;
        q->end = &((*q->end)->next);
        return 1;
}


/*
 *  qremove: remove an item from the queue.
 *
 *  Parameters:
 *
 *    q         Pointer to a queue.
 *
 *    d         Pointer to the QDATUM variable that will hold the datum
 *              corresponding to the queue item.
 *
 *  Return values:
 *
 *    non-NULL  An item has been removed. The variable that d points 
 *              to now contains the datum associated with the item 
 *              in question.
 *              
 *    NULL      No item could be removed. Either the queue pointer 
 *              provided was NULL, or the queue was empty. The memory 
 *              location that d points to has not been modified. 
 */
QDATUM *
qremove(struct queue *q, QDATUM *d)
{
        struct qentry *tmp;
                
        if (!q || !q->begin) return NULL;
        tmp = q->begin;
        if (!(q->begin = q->begin->next)) q->end = &q->begin;
        *d = tmp->d;
        free(tmp);
        return d;       
}


/*
 *  qpeek: access an item without removing it from the queue.
 *
 *  Parameters:
 *
 *    q         Pointer to a queue.
 *
 *    d         Pointer to the QDATUM variable that will hold the datum
 *              associated with the first item in the queue, i. e.,
 *              the item that would be removed had qremove been called
 *              instead of qpeek.
 *
 *  Return values:
 * 
 *    See qremove.
 */
QDATUM *
qpeek(struct queue *q, QDATUM *d)
{
        if (!q || !q->begin) return NULL;
        *d = q->begin->d;
        return d;
}



void replace_from_queue(struct queue *q,wchar_t *in)
{
  int i=0;
  wchar_t c;
  wchar_t d;
  c=in[i];
  while (c != '\0') {
    if (c == 2) {
      qremove(q,&d);
      in[i]=d;
    }
    i++;
    c=in[i];
  }

}



int unknown_symbol(wchar_t c) {
  return ( 1
        && c != (wchar_t)2
        && c != (wchar_t)32
        && c != (wchar_t)91
        && c != (wchar_t)92
        && c != (wchar_t)93
	   );
         
}

static const struct transition_struct {
  wchar_t symbol ;      /* current symbol                     */
  unsigned int last    :  1;      /* last line of current state?        */
  unsigned int final   :  1;      /* final state?                       */
  unsigned int next    : 22;      /* line in table to go to next        */
    wchar_t *out;                     /* output string                      */
    wchar_t *final_out;               /* output string if we terminate here */
  } trans[]= {
                     /* sink                               */
    {0,1,0,0,L"",L""},
                     /* start state                        */
    {0,0,1,2,L"",L""},
    {2,0,1,7,L"",L""},
    {32,0,1,2,L" ",L""},
    {91,0,1,17,L"",L"\\["},
    {92,0,1,12,L"\\",L""},
    {93,1,1,22,L"",L"\\]"},
    {2,0,1,7,L"",L""},
    {32,0,1,2,L" ",L""},
    {91,0,1,7,L"[",L""},
    {92,0,1,7,L"\\",L""},
    {93,1,1,7,L"]",L""},
    {2,0,1,7,L"",L""},
    {32,0,1,2,L" ",L""},
    {91,0,1,17,L"",L"\\["},
    {92,0,1,7,L"\\",L""},
    {93,1,1,22,L"",L"\\]"},
    {2,0,1,7,L"[",L""},
    {32,0,1,2,L"\\[ ",L""},
    {91,0,1,7,L"[[",L""},
    {92,0,1,7,L"[\\",L""},
    {93,1,1,7,L"[]",L""},
    {2,0,1,7,L"]",L""},
    {32,0,1,2,L"\\] ",L""},
    {91,0,1,7,L"][",L""},
    {92,0,1,7,L"]\\",L""},
    {93,1,1,7,L"]]",L""},
};

/* int t_accepts(wchar_t *in, wchar_t *out, int max)
   in is transduced to out. Note that memory for out should be allocated
   by the caller. At most max characters are writting into out. 
   The return values:

   0: no transduction possible
   1: transduction possible
   2: length of transduction would be > max
*/

int t_accepts(wchar_t *in,wchar_t *out,int max) {
  const wchar_t *ini_out=L"";
  register wchar_t c;
  register unsigned int i;
  register unsigned long int line=1;
  struct queue q;
  int outl;
  qinit(&q);
  i=0;
  c=in[i];
  out[0]='\0';
  outl=0;
  outl+=wcslen(ini_out);
  if (outl > max) return 2;
  wcscat(out,ini_out);
  while (c != '\0') {
    line=trans[line].next;
    if (trans[line].symbol == 1 && unknown_symbol(c) )
      ;
    else {
      if (trans[line].symbol == 2 && unknown_symbol(c) ) {
	qinsert(&q,c);
      }
      else {
        while (!trans[line].last && c > trans[line].symbol) {  
          line++;
        }
      if (c != trans[line].symbol)
        return 0;
      }
    }
    if (outl+wcslen(trans[line].out) > max) return 2;
    wcscat(&out[outl],trans[line].out);
    replace_from_queue(&q,&out[outl]);
    outl+=wcslen(trans[line].out);
    i++;
    c=in[i];
  }
  if (outl+wcslen(trans[line].final_out) > max) return 2;
  wcscat(&out[outl],trans[line].final_out);
  replace_from_queue(&q,&out[outl]);
  return trans[line].final;        /* ok if at final state */
}


int resize_buf(int n, int *max, wchar_t **buf) {
  while (n > *max)
    *max *= 2;
  *buf = (wchar_t *) realloc (*buf, (*max + 1)  * sizeof(wchar_t));
  return buf != NULL;
}

int new_t_accepts(wchar_t *in,wchar_t **out,int *max) {
  const wchar_t *ini_out=L"";

  register wchar_t c;
  register unsigned int i;
  register unsigned long int line=1;
  struct queue q;
  int outl;
  wchar_t *myout = *out;
  qinit(&q);
  i=0;
  c=in[i];
  myout[0]='\0';
  outl=wcslen(ini_out);
  if (outl >= *max && !resize_buf(outl,max,&myout)) { *out = myout; return 2; }
  wcscat(myout,ini_out);
  while (c != '\0') {
    line=trans[line].next;
    if (trans[line].symbol == 1 && unknown_symbol(c) )
      ;
    else {
      if (trans[line].symbol == 2 && unknown_symbol(c) ) {
	qinsert(&q,c);
      }
      else {
        while (!trans[line].last && c > trans[line].symbol) {
          line++;
        }
        if (c != trans[line].symbol) {
	  *out = myout;
	  return 0;
        }
      }
    }
    if (outl+wcslen(trans[line].out) >= *max && !resize_buf(outl+wcslen(trans[line].out),max,&myout)) { *out = myout; return 2; }
    wcscat(&myout[outl],trans[line].out);
    replace_from_queue(&q,&myout[outl]);
    outl+=wcslen(trans[line].out);
    i++;
    c=in[i];
  }
  if (outl+wcslen(trans[line].final_out) >= *max && !resize_buf(outl+wcslen(trans[line].final_out),max,&myout)) { *out = myout; return 2; }
  wcscat(&myout[outl],trans[line].final_out);
  replace_from_queue(&q,&myout[outl]);
  *out = myout;
  return trans[line].final;        /* ok if at final state */
}



#include <stdio.h>
#include <locale.h>
#include <errno.h>
#include <string.h>



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
	  

