//
// 引数一覧表示プログラム  (c)1999 seclan
// ver1.00 1999/03/15 最初のバージョン
//
#include <stdio.h>
int main(int argc, char *argv[])
{
  int i;
  for(i = 0; i < argc; i++){
    printf("%d: %s\n", 
	   i,            // 添え字。今何番目を
                         //表示しているか
	   argv[i]       // 引数の中身
	   );
  }
  return 0;      // 常に正常終了
}
// ここはコメント領域 
// /* ここの最後の */ もコメント */
