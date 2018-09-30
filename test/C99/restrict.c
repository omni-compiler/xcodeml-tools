#include <stdio.h>

void swap(int * restrict n, int * restrict m)
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
