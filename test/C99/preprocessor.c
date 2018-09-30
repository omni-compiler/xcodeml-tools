/*
 * cpp: 空マクロサンプル (c)1999 seclan
 * ver1.00 1999/04/02 最初のバージョン
 */
#include <stdio.h>
#define catval(val,suf) val ## suf
int main()
{
  long l = catval(3,L);
  //3L に展開。
  
  int  i = catval(3,);
  //OK。3 に展開が保証される。

  printf("%ld %d\n", l, i);
  
  return 0;
}
