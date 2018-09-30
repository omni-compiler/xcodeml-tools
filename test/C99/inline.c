#include <stdio.h>

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
  printf("a = %d b = %d\n", a, b);

  return 0;
}
