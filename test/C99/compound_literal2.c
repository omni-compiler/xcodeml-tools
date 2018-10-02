#include <stdio.h>
#include <stdlib.h>

enum {a, b};
int g(void)
{
  if(sizeof(enum{b,a}) != sizeof(int))
    return a;
  return b;
}

int main()
{
  if(g() == 1)
    printf("SUCCESS\n");
  else{
    printf("ERROR\n");
    exit(1);
  }
  return 0;
}
