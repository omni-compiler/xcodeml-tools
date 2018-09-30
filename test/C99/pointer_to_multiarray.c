void aa(int n, int m)
{
  int ar1[n][8][m];
  int (*par1)[3][n-4];
  int (*par2)[n][n+1];
  
  //  par1 = ar1; // (1)エラー: 8 != 3
  par2 = ar1; // (2)OK: ただし n == 8, m == n + 1 の時以外は未定義の挙動
}
