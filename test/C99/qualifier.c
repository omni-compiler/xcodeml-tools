#include <stdio.h>
void func1(int p1[static 10]){}
void func2(int p1[restrict], int p2[restrict]){}
void func3(int p1[const]){}
void func4(int p1[volatile]){}

int main()
{
  int p1[10], p2[10];
  func1(p1);
  func2(p1, p2);
  func3(p1);
  func4(p1);

  printf("SUCCESS\n");
  return 0;
}

