//
// file: asm-test.c
//
// Simple program to test asm-super-mutant component.
// Determines if passed integer is even or odd.
//

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

extern int is_even(int);

//
// Really slow way to determine if integer is even or odd
//
int is_even(int n) {
    // take absolute value of n
    if (n < 0)
        n = -n;
    while (n > 0)
    {
        if (n == 1)
            return 0;
        n -= 2;
    }
    return 1;
}

int main(int argc, char** argv){
  if(argc != 2) {
    fputs("Usage: asm-test [int]\n", stderr);
    return 1;
  }
  int num = atoi(argv[1]);
  if (is_even(num))
      printf("even\n");
  else
      printf("odd\n");
  
  return 0;
}
