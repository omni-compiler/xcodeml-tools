#include <stdio.h>

size_t getN(int n)
{
  unsigned char buf[n];
  return sizeof(buf);
}

int main()
{
  size_t sz[3];
  sz[0] = getN(1);    //sz == 1
  sz[1] = getN(256);  //sz == 256
  sz[2] = getN(1024); //sz == 1024

  printf("%d %d %d\n", sz[0], sz[1], sz[2]);

  return 0;
}
