#include <stdio.h>
#include <stdarg.h>

int main(int argc, char **argv) {
  int a = 2;
  int b = 4, c = 5, d = 6;
  if (argc > 10) {
    printf("%d", a * b * c / d * argc);
  } else {
    printf("%d", a / b * c / d * argc);
  }
  return 0;
}
