//void func1(int m, int n, int data[m][n]);
//void func1(int m, int n, int data[m][n]){}

// OK
//void func2(int m, int n, int data[*][*]);
//void func2(int m, int n, int data[m][n]){} 

// OK
//void func3(int m, int n, int data[ ][*]);
//void func3(int m, int n, int data[m][n]){}

//void func4(int m, int n, int data[ ][n]);
//void func4(int m, int n, int data[m][n]){}

void func5(int data[*][*], int m, int n);
void func5(data, n, m)
  int n, m;
  int data[m][n];
{}

