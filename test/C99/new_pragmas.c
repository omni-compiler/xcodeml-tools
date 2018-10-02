#include <stdio.h>
int main()
{
#pragma STDC FP_CONTRACT ON
  {
  }

#pragma STDC FENV_ACCESS ON
 {
 }
 
#pragma STDC CX_LIMITED_RANGE ON
 {
 }

 printf("SUCCESS\n");
 return 0;
}
