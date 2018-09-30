#define ERRMES_FILEREAD  20
#define ERRMES_FILEWRITE 21
const char *errmessage[] = {
[0]                = "no error",
[ERRMES_FILEREAD]  = "file read error",
[ERRMES_FILEWRITE] = "file write error",
};

struct largestruct {
  int size;
  int a, b;
  int cx, cy;
} sls = {
  sizeof(struct largestruct),
  .cx = 640,
  .cy = 480,
};

int main(){ return 0;}
