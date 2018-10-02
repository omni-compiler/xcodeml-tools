#include <stdio.h>
#include <stdlib.h>

static inline void swap(int *n, int *m)
{
  int tmp = *n;
  *n = *m;
  *m = tmp;
}

int main()
{
  int a = 1, b = 2;
  swap(&a, &b);
  if(a == 2 && b == 1)
    printf("SUCCESS\n");
  else{
    printf("ERROR\n");
    exit(1);
  }
  return 0;
}
