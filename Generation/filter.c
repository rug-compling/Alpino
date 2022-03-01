#include <limits.h>

extern char bigramGuideVector[];
extern char localGuideVector[];
extern int bigramGuideSize;
extern int localGuideSize;

int bool_vector_member_bigram(int i)
{
  int index = i / CHAR_BIT;
  int bit = i % CHAR_BIT;

  if (index + 1> bigramGuideSize)
    return 0;

  return (bigramGuideVector[index] & (1 << bit)) != 0;
}

int bool_vector_member_local(int i)
{
  int index = i / CHAR_BIT;
  int bit = i % CHAR_BIT;

  if (index + 1> localGuideSize)
    return 0;

  return (localGuideVector[index] & (1 << bit)) != 0;
}
