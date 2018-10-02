#include <stdio.h>
typedef struct { int x, y; } point_t;
void PutPixel(const point_t *p){}
void func(int a[]){}

int main()
{
  PutPixel(&(point_t){640, 480});
  PutPixel(&(point_t){.x=640, .y=480});
  int pos_x = 640, pos_y = 480;
  PutPixel(&(point_t){pos_x, pos_y});

  func((int[ ]){1, 2, 3, 4});

  printf("SUCCESS\n");
  return 0;
}
