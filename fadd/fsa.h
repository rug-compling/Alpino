/***	fsa.h		***/

/*	Copyright (C) Jan Daciuk, 1996	*/

/* This structure describes a labelled arc in an automaton */

#ifndef		FSA_H
#define		FSA_H

#ifdef FLEXIBLE
inline int
bytes2int(const unsigned char *bytes, const int n)
{
  register int r = 0;
  register int i;
  for (i = n - 1; i >= 0; --i) {
    r <<= 8; r |= bytes[i];
  }
  return r;
}
#endif

/* This constant depends on the representation of the # of children per node.
 * It MUST BE a power of 2 minus 1.
 * If not compiled with STOPBIT, 127 is max, because there are 7 bits
 * for the number of children in that representation.
 * Otherwise it can perhaps be 255, because there is no counter of children,
 * but the labels must be different, and they have 8 bits. I have not tested
 * that.
 */
#ifdef STOPBIT
const int	MAX_ARCS_PER_NODE = 255;
#else
const int	MAX_ARCS_PER_NODE = 127;
#endif

#ifdef	FLEXIBLE
typedef		unsigned long		fas_pointer;
#else
#ifdef	LARGE_DICTIONARY
typedef		unsigned int		fas_pointer;
#else
typedef		unsigned short int	fas_pointer;
#endif
#endif

#ifndef FLEXIBLE
struct fsa_arc {
  fas_pointer	go_to;		/* Index in automaton's table of arcs.
				 * That place has several (see counter)
				 * arc elements that say where to go to
				 * on seeing a particular character.
				 *
				 * When compiled with FLEXIBLE, the size
				 * of this field depends on the size of
				 * the automaton. It can be from 2 to 4
				 * bytes.
				 */
  char		counter;	/* The MSB (most significant bit) tells
				 * whether the node reached by this arc
				 * is final, or not. If MSB is set, the
				 * node is final.
				 * (counter & 0x7F) says how many arcs
				 * lead from the node reached by this arc.
				 */
  char		letter;		/* This arc should be followed on encountering
				 * this letter.
				 */
};
#endif

#ifdef FLEXIBLE
typedef char		*mod_arc_ptr; /* modifiable arc pointer */
typedef	const char	*arc_pointer;
#else
typedef fsa_arc		*mod_arc_ptr; /* modifiable arc pointer */
typedef const fsa_arc	*arc_pointer;
#endif

#if defined (FLEXIBLE) && defined(STOPBIT) && defined(TAILS)
extern arc_pointer curr_dict_address;

inline arc_pointer get_curr_dict_address(void) {
  return curr_dict_address;
}

inline void set_curr_dict_address(arc_pointer p) {
  curr_dict_address = p;
}
#endif

/*

The structure of a node is as follows:

#ifdef FLEXIBLE
#ifdef STOPBIT
#ifdef NUMBERS
#ifdef NEXTBIT
#ifdef TAILS

FLEXIBLE, STOPBIT, NUMBERS, NEXTBIT, TAILS

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       +-+-+-+-+-+-+-+-+\
    0  | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                +--------------- rest of arcs at specified address (tail flag)
                | +------------- node pointed to is next
                | | +----------- the last arc of the node
                | | | +--------- the arc is final
                | | | |
           +-------------+
           |    | | | |  |
         __+__  | | | |  |
        /     \ | | | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     1 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     2 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     3 | | | | | | | | |     +----- target node address (in bytes)
       +-+-+-+-+-+-+-+-+     |      (not present except for the byte
       : : : : : : : : :     |       with flags if the node pointed to
       +-+-+-+-+-+-+-+-+     +       is next)
  gtl  | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /

gtl+1  +-+-+-+-+-+-+-+-+\
       | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
       | | | | | | | | |  |
       +-+-+-+-+-+-+-+-+  +-------- address of the rest of arcs
       : : : : : : : : :  |         (present only when the tail flag
       +-+-+-+-+-+-+-+-+  +          is set  on the arc)
       | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/
1+2*gtl

#else // !TAILS
#ifdef WEIGHTED

FLEXIBLE, STOPBIT, NUMBERS, NEXTBIT, !TAILS, WEIGHTED

WARNING: This format is only used when -W run-time option is specified
for fsa_build or fsa_ubuild! Otherwise it is like without WEIGHTED.

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       +-+-+-+-+-+-+-+-+\
    0  | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+X
    1  | | | | | | | | | +------ weight
       +-+-+-+-+-+-+-+-+/


                  +------------- node pointed to is next
                  | +----------- the last arc of the node
                  | | +--------- the arc is final
                  | | |
            +------------+
            |     | | |  |
         ___+___  | | |  |
        /       \ | | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     2 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     3 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     4 | | | | | | | | |     +----- target node address (in bytes)
       +-+-+-+-+-+-+-+-+     |      (not present except for the byte
       : : : : : : : : :     |       with flags if the node pointed to
       +-+-+-+-+-+-+-+-+     +       is next)
gtl+1  | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /
gtl+2

#else //!WEIGHTED

FLEXIBLE, STOPBIT, NUMBERS, NEXTBIT, !TAILS, !WEIGHTED

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       +-+-+-+-+-+-+-+-+\
    0  | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                  +------------- node pointed to is next
                  | +----------- the last arc of the node
                  | | +--------- the arc is final
                  | | |
            +------------+
            |     | | |  |
         ___+___  | | |  |
        /       \ | | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     1 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     2 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     3 | | | | | | | | |     +----- target node address (in bytes)
       +-+-+-+-+-+-+-+-+     |      (not present except for the byte
       : : : : : : : : :     |       with flags if the node pointed to
       +-+-+-+-+-+-+-+-+     +       is next)
  gtl  | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /
gtl+1

#endif // !WEIGHTED
#endif // !TAILS
#else // !NEXTBIT
#ifdef TAILS

FLEXIBLE, STOPBIT, NUMBERS, !NEXTBIT, TAILS

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       +-+-+-+-+-+-+-+-+\
    0  | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                  +------------- rest of arcs at specified address (tail flag)
                  | +----------- the last arc of the node
                  | | +--------- the arc is final
                  | | |
            +------------+
            |     | | |  |
         ___+___  | | |  |
        /       \ | | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     1 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     2 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     3 | | | | | | | | |     +----- target node address (in bytes)
       +-+-+-+-+-+-+-+-+     |
       : : : : : : : : :     |
       +-+-+-+-+-+-+-+-+     +
  gtl  | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /
gtl+1

gtl+1  +-+-+-+-+-+-+-+-+\
       | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
       | | | | | | | | |  |
       +-+-+-+-+-+-+-+-+  +-------- address of the rest of arcs
       : : : : : : : : :  |         (present only when the tail flag
       +-+-+-+-+-+-+-+-+  +          is set  on the arc)
       | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/
1+2*gtl

#else //!TAILS


FLEXIBLE, STOPBIT, NUMBERS, !NEXTBIT, !TAILS

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       +-+-+-+-+-+-+-+-+\
    0  | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                    +----------- the last arc of the node
                    | +--------- the arc is final
                    | |
             +-----------+
             |      | |  |
         ____+____  | |  |
        /         \ | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     1 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     2 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     3 | | | | | | | | |     +----- target node address (in bytes)
       +-+-+-+-+-+-+-+-+     |
       : : : : : : : : :     |
       +-+-+-+-+-+-+-+-+     +
  gtl  | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /
gtl+1

#endif //!TAILS
#endif //!NEXTBIT
#else //!NUMBERS
#ifdef NEXTBIT

FLEXIBLE, STOPBIT, !NUMBERS, NEXTBIT

A node is a vector of arcs

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                  +------------- node pointed to is next
                  | +----------- the last arc of the node
                  | | +--------- the arc is final
                  | | |
             +-----------+
             |    | | |  |
         ___+___  | | |  |
        /       \ | | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     1 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     2 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     3 | | | | | | | | |     +----- target node address (in bytes)
       +-+-+-+-+-+-+-+-+     |      (not present except for the byte
       : : : : : : : : :     |       with flags if the node pointed to
       +-+-+-+-+-+-+-+-+     +       is next)
   gtl | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /
 gtl+1

#else

FLEXIBLE, STOPBIT, !NUMBERS, !NEXTBIT

A node is a vector of arcs

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                    +----------- the last arc of the node
                    | +--------- the arc is final
                    | |
             +-----------+
             |      | |  |
         ____+____  | |  |
        /         \ | |  |
       MSB           LSB |
    	7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ |
     1 | | | | | | | | | \ \
       +-+-+-+-+-+-+-+-+  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     2 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     |
     3 | | | | | | | | |     +----- target node number
       +-+-+-+-+-+-+-+-+     |
       : : : : : : : : :     |
       +-+-+-+-+-+-+-+-+     +
   gtl | | | | | | | | |    /  MSB
       +-+-+-+-+-+-+-+-+   /
 gtl+1

#endif //!NEXTBIT
#endif //!NUMBERS
#else //!STOPBIT
#ifdef NUMBERS
#ifdef NEXTBIT

FLEXIBLE, !STOPBIT, NUMBERS, NEXTBIT

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       MSB           LSB
       7 6 5 4 3 2 1 0
       +-+-+-+-+-+-+-+-+
     0 | | | | | | | | |
       +-+-+-+-+-+-+-+-+
        | \___________/
        |       |
        |       +--------------- number of children
        +----------------------- the arc is final
       +-+-+-+-+-+-+-+-+\
     1 | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                      +------------ the node pointed to is next
                      |
               +---------+
               |      |  |
         ______+____  |  |
        /           \ |  |
       MSB           LSB |
        7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ \ \
     2 | | | | | | | | |  \ \  LSB
       +-+-+-+-+-+-+-+-+     +
     3 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     +----- target node address (in bytes)
       : : : : : : : : :     |      (not present except for the byte
       +-+-+-+-+-+-+-+-+     +       with "next" flag if the node
  gtl  | | | | | | | | |    /  MSB   pointed to is the next)
       +-+-+-+-+-+-+-+-+   /
 gtl+1

#else //!NEXTBIT

FLEXIBLE, !STOPBIT, NUMBERS, !NEXTBIT

First, cardinality of the right language

 Byte
       +-+-+-+-+-+-+-+-+\
     0 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     1 | | | | | | | | |  |      number of strings recognized
       +-+-+-+-+-+-+-+-+  +----- by the automaton starting
       : : : : : : : : :  |      from this node.
       +-+-+-+-+-+-+-+-+  +
 ctl-1 | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/

Then, a vector of arcs

       MSB           LSB
       7 6 5 4 3 2 1 0
       +-+-+-+-+-+-+-+-+
     0 | | | | | | | | |
       +-+-+-+-+-+-+-+-+
        | \___________/
        |       |
        |       +--------------- number of children
        +----------------------- final
       +-+-+-+-+-+-+-+-+\
     1 | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

       +-+-+-+-+-+-+-+-+\
     2 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     3 | | | | | | | | |  |
       +-+-+-+-+-+-+-+-+  +----- target node address (in bytes)
       : : : : : : : : :  |
       +-+-+-+-+-+-+-+-+  +
  gtl  | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/
 gtl+1

#endif //!NEXTBIT
#else //!NUMBERS
#ifdef NEXTBIT

FLEXIBLE, !STOPBIT, !NUMBERS, NEXTBIT

A node is a vector of arcs

 Byte
       MSB           LSB
    	7 6 5 4 3 2 1 0
       +-+-+-+-+-+-+-+-+
     0 | | | | | | | | |
       +-+-+-+-+-+-+-+-+
    	| \___________/
    	|       |
    	|       +--------------- number of children
    	+----------------------- final
       +-+-+-+-+-+-+-+-+\
     1 | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

                      +------------ the node pointed to is the next
                      |
              +----------+
              |       |  |
         _____+_____  |  |
        /           \ |  |
       MSB           LSB |
        7 6 5 4 3 2 1 0  |
       +-+-+-+-+-+-+-+-+ \ \
     2 | | | | | | | | |    \  LSB
       +-+-+-+-+-+-+-+-+     +
     3 | | | | | | | | |     |
       +-+-+-+-+-+-+-+-+     +----- target node address (in bytes)
       : : : : : : : : :     |      (not present except for the byte
       +-+-+-+-+-+-+-+-+     +       with "next" flag if the node
 1+gtl | | | | | | | | |    /  MSB   pointed to is next)
       +-+-+-+-+-+-+-+-+   /
    
#else

FLEXIBLE, !STOPBIT, !NUMBERS, !NEXTBIT

A node is a vector of arcs

 Byte
       MSB           LSB
    	7 6 5 4 3 2 1 0
       +-+-+-+-+-+-+-+-+
     0 | | | | | | | | |
       +-+-+-+-+-+-+-+-+
    	| \___________/
    	|       |
    	|       +--------------- number of children
    	+----------------------- final
       +-+-+-+-+-+-+-+-+\
     1 | | | | | | | | | +------ label
       +-+-+-+-+-+-+-+-+/

       +-+-+-+-+-+-+-+-+\
     2 | | | | | | | | | \  LSB
       +-+-+-+-+-+-+-+-+  +
     3 | | | | | | | | |  |
       +-+-+-+-+-+-+-+-+  +----- target node number
       : : : : : : : : :  |
       +-+-+-+-+-+-+-+-+  +
 1+gtl | | | | | | | | | /  MSB
       +-+-+-+-+-+-+-+-+/
    
#endif //!NEXTBIT
#endif //!NUMBERS
#endif //!STOPBIT
#else //!FLEXIBLE
#ifdef BIG_DICTIONARIES
!FLEXIBLE, BIG_DICTIONARIES
#else
!FLEXIBLE, !BIG_DICTIONARIES
#endif //!BIG_DICTIONARIES
#endif //!FLEXIBLE

*/

#ifdef WEIGHTED
  extern int goto_offset;
#else //!WEIGHTED
#ifdef STOPBIT
  const int goto_offset = 1;
#else //!STOPBIT
  const int goto_offset = 2;
#endif //!STOPBIT
#endif //!WEIGHTED

/* Class name:	fsa_arc_ptr
 * Purpose:	Provide a structure for an arc (a transition)
 *		of a finite-state automaton. This representation
 *		is used by application programs.
 * Remarks:	None.
 */
class fsa_arc_ptr {
public:
  arc_pointer	arc;		/* the arc itself */
#ifdef FLEXIBLE
  static int	gtl;		/* length of go_to field */
  static int	size;		/* size of the arc */
#ifdef NUMBERS
  static int	entryl;		/* size of number of entries field */
  static int	aunit;		/* how many bytes arc number represents */
#endif
#endif

  fsa_arc_ptr(void) { arc = NULL; } /* constructor */
  fsa_arc_ptr(const arc_pointer a) { arc = a; } /* constructor */
  fsa_arc_ptr & operator=(arc_pointer a) { arc = a; return *this;}

#ifdef STOPBIT
#ifdef FLEXIBLE
  int is_last(void) const { /* returns TRUE if the arc is the last in node */
    return ((arc[goto_offset] & 2) == 2);
  }

  int tail_present(void) const { /* returns TRUE if the next arc is
				    at a location pointed to by additional
				    pointer following the arc */
#ifdef NEXTBIT
    return ((arc[goto_offset] & 8) == 8);
#else
    return ((arc[goto_offset] & 4) == 4);
#endif
  }
#endif

#else

  int children(void) const {		/* return number of children */
#ifdef FLEXIBLE
    return ((*arc) & 0x7f);
#else
    return (arc->counter & 0x7f);
#endif
  }
#endif

  int is_final(void) const {	/* return TRUE iff the arc is final */
#ifdef FLEXIBLE
#ifdef STOPBIT
    return (arc[goto_offset] & 1);
#else
    return (((*arc) & 0x80) != 0);
#endif
#else
    return ((arc->counter & 0x80) != 0);
#endif
  }

  char get_letter(void) const {	/* return arc label */
#ifdef FLEXIBLE
#ifdef STOPBIT
    return (*arc);
#else
    return (arc[1]);
#endif
#else
    return arc->letter;
#endif
  }

  fas_pointer get_goto(void) const { /* get number of the target node */
#ifdef FLEXIBLE
#ifdef STOPBIT
#ifdef NEXTBIT
#ifdef TAILS
    /* FLEXIBLE, STOPBIT, NEXTBIT, TAILS */
    return bytes2int((const unsigned char *)arc + goto_offset, gtl) >> 4;
#else
    /* FLEXIBLE, STOPBIT, NEXTBIT, !TAILS */
    return bytes2int((const unsigned char *)arc + goto_offset, gtl) >> 3;
#endif
#else // !NEXTBIT
#ifdef TAILS
    /* FLEXIBLE, STOPBIT, !NEXTBIT, TAILS */
    return bytes2int((const unsigned char *)arc + goto_offset, gtl) >> 3;
#else
    /* FLEXIBLE, STOPBIT, !NEXTBIT, !TAILS */
    return bytes2int((const unsigned char *)arc + goto_offset, gtl) >> 2;
#endif // !TAILS
#endif // !NEXTBIT
#else // !STOPBIT
#ifdef NEXTBIT
    return bytes2int((const unsigned char *)arc + goto_offset, gtl) >> 1;
#else
    return bytes2int((const unsigned char *)arc + goto_offset, gtl);
#endif
#endif
#else
    return arc->go_to;
#endif
  }

  arc_pointer set_next_node(const arc_pointer curr_dict) { /* get address
							      of  target
							      node */
#ifdef NEXTBIT
#ifdef FLEXIBLE
#ifdef STOPBIT
#ifdef TAILS
    /* NEXTBIT,FLEXIBLE,STOPBIT,TAILS */
    /* if nextbit set, then it is the next address, but if the tail bit
       is also set, then we must skip the tail pointer */
  return (arc[goto_offset] & 4) ?
    ((arc[goto_offset] & 8) ? arc + goto_offset + 1 + gtl
     : arc + goto_offset + 1)
#else
    /* NEXTBIT,FLEXIBLE,STOPBIT */
  return (arc[goto_offset] & 4) ? arc + goto_offset + 1
#endif
#else
    /* NEXTBIT,FLEXIBLE,!STOPBIT */
  return (arc[goto_offset] & 1) ? arc + goto_offset + 1
#endif
    /* NEXTBIT,FLEXIBLE */
#ifdef NUMBERS
    /* NEXTBIT,FLEXIBLE,NUMBERS */
    + entryl
#endif
    /* NEXTBIT,FLEXIBLE */
     : curr_dict + 
#ifdef NUMBERS
    /* NEXTBIT,FLEXIBLE,NUMBERS */
    entryl +
#endif
    /* NEXTBIT,FLEXIBLE */
    get_goto();
#else
  /* NEXTBIT,!FLEXIBLE */
#endif //!FLEXIBLE
  /* NEXTBIT */
#else //!NEXTBIT
  /* !NEXTBIT */
  return (curr_dict +
#ifdef FLEXIBLE
#if defined(STOPBIT) && defined(TAILS)
#else
#ifdef NUMBERS
	  /* !NEXTBIT,FLEXIBLE,NUMBERS,!(STOPBIT,TAILS) */
	    entryl + aunit *
#else
	  /* !NEXTBIT,FLEXIBLE,!NUMBERS,!(STOPBIT,TAILS) */
	    size *
#endif //!NUMBERS
#endif //!STOPBIT&TAILS
#endif //FLEXIBLE
	  /* !NEXTBIT */
	    get_goto());
#endif
}


  fsa_arc_ptr & operator++(void) { /* get next arc */
#ifdef FLEXIBLE
#ifdef STOPBIT
#ifdef TAILS
#ifdef NEXTBIT
    if (arc[goto_offset] & 8) {
      /* The rest of arcs (the tail) is in another node, whose address
	 is after this node */
      if (arc[goto_offset] & 4) {
	/* This node points to another node that is the next one
	   in the automaton */
	arc = get_curr_dict_address() +
	  (bytes2int((const unsigned char *)arc + goto_offset + 1, gtl)>>4);
      }
      else {
	/* This arc has a normal goto field length */
	arc = get_curr_dict_address() +
	  (bytes2int((const unsigned char *)arc + goto_offset + gtl, gtl)>>4);
      }
    }
    else {
      /* There is no tail pointer */
      arc += size;
    }
#else // !NEXTBIT
    if (arc[goto_offset] & 4) {
      /* The rest of arcs (the tail) is in another node, whose address
	 is after this node */
      arc = get_curr_dict_address() +
	(bytes2int((const unsigned char *)arc + goto_offset + gtl, gtl)>>3);
    }
    else {
      arc += size;
    }
#endif // !NEXTBIT
#else // !TAILS
    arc += size;
#endif // !TAILS
#else
    arc += size;
#endif
#else
    arc++;
#endif
    return *this;
  }

  arc_pointer first_node(arc_pointer curr_dict) { /* get address of first arc*/
#ifdef FLEXIBLE
    return (curr_dict
#ifdef NUMBERS
	    + entryl * 2
#endif
	    + size);
#else
    return (curr_dict + 1);
#endif
  }

#if defined(WEIGHTED) && defined(FLEXIBLE) && defined(STOPBIT)
  int get_weight(void) { return arc[goto_offset - 1]; }
#endif
};/* class fsa_arc_ptr */



#ifdef STOPBIT
#ifdef FLEXIBLE
inline int
fsa_set_last(mod_arc_ptr arc, const int whether_last) {	/* set last arc */
  return (whether_last ? (arc[goto_offset] |= 2) : (arc[goto_offset] &= 0xfd));
}
#endif

#else

inline int
fsa_set_children(mod_arc_ptr arc, const int kids) { /* set  num of kids */
#ifdef FLEXIBLE
  return (*arc = (((*arc) & 0x80) | kids));
#else
  return arc->counter = ((arc->counter & 0x80) | kids);
#endif
}
#endif

inline void
fsa_set_final(mod_arc_ptr arc, const int f) { /* set final attribute */
#ifdef FLEXIBLE
#ifdef STOPBIT
  if (f) arc[goto_offset] |= 1; else arc[goto_offset] &= 0xfe;
#else
  if (f) (*arc) |= 0x80; else (*arc) &= 0x7f;
#endif
#else
  if (f) arc->counter |= 0x80; else arc->counter &= 0x7f;
#endif
}


#ifdef FLEXIBLE
#ifdef NEXTBIT
inline void
fsa_set_next(mod_arc_ptr arc, const int f) {/* set next flag */
#ifdef STOPBIT
  if (f) arc[goto_offset] |= 4; else arc[goto_offset] &= 0xfb;
#else
  if (f) arc[goto_offset] |= 1; else arc[goto_offset] &= 0xfe;
#endif
}
#endif
#endif

#if defined(FLEXIBLE) && defined(STOPBIT) && defined(TAILS)
inline void
fsa_set_tail(mod_arc_ptr arc, const int t) {/* set tail flag */
#ifdef NEXTBIT
  if (t) arc[goto_offset] |= 8; else arc[goto_offset] &= 0xf7;
#else
  if (t) arc[goto_offset] |= 4; else arc[goto_offset] &= 0xfb;
#endif
}
#endif

#ifdef STOPBIT
#ifdef TAILS
#define	forallnodes(i) \
          for (int i##nlast = 1;\
		 i##nlast;\
		 (i##nlast = !(next_node.is_last()) ||\
		  next_node.tail_present()), ++next_node)
#else
#define	forallnodes(i) \
          for (int i##nlast = 1;\
		 i##nlast;\
		 i##nlast = !(next_node.is_last()), ++next_node)
#endif
#else
#define forallnodes(i) \
          int i##kids = start.children(); \
          for (int i = 0; i < i##kids; i++, ++next_node)
#endif



struct signature {		/* dictionary file signature */
  char          sig[4];         /* automaton identifier (magic number) */
  char          ver;            /* automaton type number */
  char          filler;         /* char used as filler */
  char          annot_sep;      /* char that separates annotations from lex */
  char          gtl;            /* length of go_to field */
};/*struct signature */

template <typename T>
void free_low(T *low)
{
  while (low != 0) {
    T *prev_low = low;
    low = low->next;
    delete prev_low;
  }
}

#endif
/***	EOF fsa.h	***/
