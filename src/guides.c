#include <limits.h>

extern char guideVector[];
extern int guideSize;

int bool_vector_member(int i)
{
  int index = i / CHAR_BIT;
  int bit = i % CHAR_BIT;

  if (index + 1> guideSize)
    return 0;

  return (guideVector[index] & (1 << bit)) != 0;
}
