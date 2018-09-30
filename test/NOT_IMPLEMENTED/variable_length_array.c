#include <stdio.h>

void test(int n)
{
  // int の要素数 n を持つ配列 arrayN_t の型定義 
  // 実行時 n = 1 なら typedef int arrayN_t[1]; と同じ
  // 実行時 n = 10 なら typedef int arrayN_t[10];
  typedef int arrayN_t[n];

  n++; // n を一つ増やす。

  // arrayN_t 型の変数 A を宣言。
  // この時点での arrayN_t の配列要素数は typedef した時点(*)でのもの。
  // n++ した後の値ではない。
  // (*)の時点で n = 1 なら int A[1]; 
  // (*)の時点で n = 10 なら int A[10]; 
  arrayN_t A;

  // int 型の要素数 n を持つ配列変数 B を宣言。
  // n++ した後の値。したがって、
  // (*)の時点で n = 1 なら int B[2]; 
  // (*)の時点で n = 10 なら int B[11]; 
  int B[n];

  printf("%d %d\n", sizeof(A), sizeof(B));
}

int main()
{
  test(1);
  test(10);
}
