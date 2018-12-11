#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  double a;
  double b;
  double c;
  double r1;
  double r2;

  a = atoi(argv[1]);
  b = atoi(argv[2]);

  if (a == 0) { printf("%g\n", b); }
  else        { }
  {
    while (b != 0) {
      if (a > b) {
        a = a - b;
      } else {
        b = b - a;
      }
    }
    printf("%g\n", a);
  }

  return 0;
}
