// prolog_pos.cc -- prolog OR alpino related functions in pos tag filter

#include "tagger.h"
#include "sicstus/sicstus.h"

#ifdef __cplusplus
extern "C" {
#endif

// initialize fadd datastructures
int alpino_pos_init(char *p_dataDir)
{
  tagger_init(3,   // must be 3, even if you want bigrams, because
	           // can be changed in alpino_pos_filter later
	      0,   // threshold
	      0,   // percentage
	      2,   // minProb
	      0,   // debug
              0,   // use baseline method; most frequent tag for each word
              0,   // use stand-alone lexical analysis instead of Alpino's
	      10,  // diversity
	      string("xxx_sentence_start"),
	      string("xxx_sentence_end"),
	      string(p_dataDir)
	      );
  return(1);
}

// apply filter to prolog terms
SP_term_ref alpino_pos_filter(SP_term_ref pl_words,
			      SP_term_ref pl_tags,
			      int p_threshold,
			      int p_percentage,
			      double p_min_prob,
			      int tc,
                              int p_debug,
			      int p_model)
{

  vector<string> words;

  SP_term_ref 
    tmp  = SP_new_term_ref(),
    head = SP_new_term_ref(),
    tail = SP_new_term_ref(),
    arg  = SP_new_term_ref();
  
  char *headname;
  long pos1,pos2;

  extern int g_threshold;
  extern int g_percentage;
  extern double g_min_prob;
  extern int g_debug;
  extern int g_model;

  g_threshold=p_threshold;
  g_percentage=p_percentage;
  g_min_prob=p_min_prob;
  g_debug=p_debug;
  g_model=p_model;

  if(p_debug>2)
    cerr << "Conversion from pl-words to vector-words..." << endl;

  tmp = pl_words;
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS) 
    {
      SP_get_string(head,&headname);
      words.push_back(string(headname));
      tmp = tail;
    }
  if(p_debug>2)
    cerr << "Conversion from pl-words to words done" << endl;

	
  // conversion pl_tags --> tags
  if(p_debug>2)
    cerr << "Conversion from pl-tags to tags..." << endl;

  vvTAGNODE                // create a trellis of TAGNODES that
    tags(words.size()+2);  // is as wide as the number of words + 2
                           // for dummy nodes at beginning and end


  int lastPos = words.size(); // last tag should end here

  int counter=0;
  tmp = pl_tags;
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS) 
    {
      SP_get_arg(1,head,arg);
      SP_get_integer(arg,&pos1);
      SP_get_arg(2,head,arg);
      SP_get_integer(arg,&pos2);
      SP_get_arg(3,head,arg);

#ifdef __SWI_PROLOG__

      PL_get_chars(arg,&headname,CVT_LIST|REP_MB);
      string str=string(headname);

#else

      SP_get_list_chars(arg,&headname);
      string str=string(headname);

#endif


      /* #define SP_get_list_chars(t,c)  */



      if(pos2 > lastPos)
	cerr << "Error in lexical analysis; moving past end of trellis: tag not included" << endl;
      else{
	TAGNODE tagNode(
			str,
			words,
			pos1,
			pos2-pos1,
			counter);
	tags[pos1+1].push_back(tagNode);
      }

      tmp = tail;
      counter++;
    }
  if(p_debug>2)
    cerr << "Conversion from pl-tags to tags done" << endl;

  if(p_debug>2)
    cerr << "Tagging..." << endl;

  // initialize results arrays
  int *filtered_tags = new int[tc];
  for(int i=0 ; i<tc ; i++)
    filtered_tags[i] = -1;

  double *filtered_tag_scores = new double[tc];
  for(int i=0 ; i<tc ; i++)
    filtered_tag_scores[i] = -1.0;

  int resultslength = tagger_filter(words,
                                    tags,tc,
                                    filtered_tags,
                                    filtered_tag_scores,p_debug);

  if(p_debug>2)
    cerr << "Tagging done" << endl;

  // conversion filtered_tags --> pl_filtered_tags
  if(p_debug>2)
    cerr << "Converting result to Prolog list..." << endl;

  SP_term_ref t = SP_new_term_ref();
  resultslength--;
  for( ; resultslength > -1 ; resultslength--)
    {
      SP_put_float(head,filtered_tag_scores[resultslength]);
      SP_cons_list(t,head,t);
      SP_put_integer(head,filtered_tags[resultslength]);
      SP_cons_list(t,head,t);
    }
  
  delete [] filtered_tags;
  delete [] filtered_tag_scores;

  if(p_debug>2)
    cerr << "Converting result to Prolog list done" << endl;

  return t;
}

#ifdef __cplusplus
}
#endif






