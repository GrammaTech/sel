#include <stdio.h>

int collatz(int m) {
    int k = 0;
    while (m != 1) {
        if (m % 2 == 0) {
            m /= 2;
        } else {
            m = 3*m + 1;
        }
        ++k;
    }
    printf("%d\n", k);
    return k;
}
