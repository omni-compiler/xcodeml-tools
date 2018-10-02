#include <stdio.h>

int main()
{
  printf("%ld\n",__STDC_VERSION__);
  printf("%ld\n",__STDC_ISO_10646__);
  printf("%d\n",__STDC_IEC_559__);
  printf("%d\n",__STDC_IEC_559_COMPLEX__);

  printf("SUCCESS\n");
  return 0;
}
