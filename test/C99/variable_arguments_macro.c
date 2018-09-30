/*
 * cpp: 新しい C 言語の CPP 可変個数引数マクロの例
 *      (c)1999 seclan
 * ver1.00 1999/04/05 最初のバージョン
 */
#include <stdio.h>
#define dbg(fmt, ...) \
  printf("debug:" fmt, __VA_ARGS__)
#define adbg(...) \
  printf("debug:" __VA_ARGS__)

int main()
{
  int x = 2, y = 4;
  dbg("ok value=%d", 1);
  
  //通常の printf のように使える！
  dbg("[%u,%u]", x, y);
  
  //多くてもOK
  adbg("%u", x);

  //ok
  adbg();
  //もちろん省略もできる

  return 0;
}
