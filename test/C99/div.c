#include <stdio.h>
#include <stdlib.h>

int main()
{
  if(-1/2 != 0){
    printf("Error\n");
    exit(1);
  }
  else
    printf("Success\n");

  return 0;
}
