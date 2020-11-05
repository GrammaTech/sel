#include <stdio.h>
#define TRUE 1
#define FALSE 0

void f(int a) {
  printf("%d\n", a);
}

int main (int argc, char *argv[]) {
  int a = 0;

  a -= 1;

  for (int i = 0; i < a; i++, a ++) {
    continue;
  }

  while (FALSE) {
    break;
  }

  if (1 == 1) {
    break;
  } else if (0) {
    a = 0;
  }


  switch (1) {
    1:
      break;
#   ifdef TEST
    2:
      break;
#   endif
  }

  return 0;
}
