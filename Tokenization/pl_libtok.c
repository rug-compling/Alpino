#include <stdio.h>
#include <sicstus/sicstus.h>
#include <wchar.h>
#include <assert.h>

int t_accepts(wchar_t *in, wchar_t *out, int max);

/* converts a Prolog list to a list of wide characters 
 */

int get_code_list_from_term(wchar_t *result,SP_term_ref word_list,int length)
{
  SP_term_ref	tmp, head, tail;
  long nint;
  int i=0;
  int rv;
  head = SP_new_term_ref();
  tail = SP_new_term_ref();
  tmp = SP_new_term_ref();
  tmp = word_list;
  while (SP_get_list(tmp,head,tail)) {
    rv = SP_get_integer(head, &nint);
    assert(rv == SP_SUCCESS);
    if(i>length-1) {
      fprintf(stderr,
	      "error: get_code_list_from_term line too long (max: %d)\n",length);
      result[0]='\0';
      return 0;
    }
    result[i]=(wchar_t)nint;
    i++;
    tmp = tail;
  }
  if(i>length) {
      fprintf(stderr,
	      "error: get_code_list_from_term line too long (max: %d)\n",length);
      result[0]='\0';
      return 0;
  }
  result[i]='\0';
}


SP_term_ref put_code_list_to_term(wchar_t *result) 
{
  SP_term_ref	hd, tl, l;
  int rv;
  int i=wcslen(result);

  l = SP_new_term_ref();
  tl = SP_new_term_ref();

  while (i>0) {
    hd = SP_new_term_ref();
    tl = l;
    rv = SP_put_integer(hd,(long) result[i-1]);
    assert(rv == SP_SUCCESS);
    rv = SP_cons_list(l,hd,tl);
    assert(rv == SP_SUCCESS);      
    i--;
  }
  return l;
}

int SP_exception(char *msg){
  SP_term_ref t = SP_new_term_ref();
  SP_put_string(t,msg);
  SP_raise_exception(t);

}

SP_term_ref tok(SP_term_ref in, int len) {
  int rv=2;
  int mult = 1;
  wchar_t *inw;
  wchar_t *outw;
  wchar_t *tmp;
  SP_term_ref r = SP_new_term_ref();


  /* convert input and allocate memory for it */
  tmp = (wchar_t *)SP_malloc((len+1)*sizeof(inw));
  if (tmp==NULL) {
    SP_exception("error: pl_libtok.c: out of memory");
  }
  inw=tmp;
  get_code_list_from_term(inw,in,len);



  /* how much memory do we need for output? Allocate until enough */
  while (rv==2) {
    mult=mult*2;
    tmp = (wchar_t *)SP_malloc(mult*(1+wcslen(inw))*sizeof(outw));
    
    if (tmp==NULL) {
      SP_exception("error: pl_libtoc.c: out of memory");
      return r;
    } else {
      outw=tmp;
      rv = t_accepts(inw,outw,mult*(1+wcslen(inw)));
      if (rv==0) {
	SP_free(inw);
	SP_free(outw);
	SP_exception("error: pl_libtok.c: tokenization fails!");
	return r;
      }
    }
  }



  /* convert output and free memory */
  r=put_code_list_to_term(outw);
  SP_free(inw);
  SP_free(outw);
  return(r);
}
