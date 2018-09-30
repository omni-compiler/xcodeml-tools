#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int attr;
  int num;
  int items[];
} info_t;

int main()
{
  int num = 10;
  info_t *info = malloc(sizeof(info_t) + sizeof(int) * (num));
  info->num = num;
  
  return 0;
}
