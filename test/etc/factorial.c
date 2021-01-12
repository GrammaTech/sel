#include <stdio.h>
#include <stdlib.h>

// 2 -> 2
// 3 -> 6
// 4 -> 24
int main(int argc, char **argv){
  int i = atoi(argv[1]);
  long f = 1;

  while (i) {
    f *= i--;
  }

  printf("%ld\n", f);

  return 0;
}
