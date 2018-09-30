#include <stdio.h>

enum {a, b};
int g(void)
{
  if(sizeof(enum{b,a}) != sizeof(int))
    return a;
  return b;
}

int main()
{
  printf("%d\n", g());
  return 0;
}
