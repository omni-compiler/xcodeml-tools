/*
 * 前定義識別名 __func__ 使用例 (c)1999 seclan
 * ver1.00 1999/05/16 最初のバージョン
 */
#include <stdio.h>
#define dbg(...) \
  (printf("%s %u @%s:", __FILE__, __LINE__, __func__), \
   printf(" "__VA_ARGS__))

void foo(int i)
{
  dbg("i=%d\n", i);
}

int main()
{
  foo(2);
  printf("SUCCESS\n");
  return 0;
}
