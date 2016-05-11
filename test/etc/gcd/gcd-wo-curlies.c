#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    double a,b,c;
    double r1, r2;
    a = atoi(argv[1]);
    b = atoi(argv[2]);

    if (a == 0) printf("%g\n", b);
    while (b != 0)
        if (a > b) a = a - b;
        else       b = b - a;

    printf("%g\n", a);

    return 0;
}
