/* Contrived test for condition synthesis in if-to-while mutation. */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
    int x = atoi(argv[1]);

    int quotient = 0;
    /* BUGS: should be a while loop. And should be >= 3. */
    if (x >= 2) {
        quotient++;
        x -= 3;
    }
    int remainder = x;

    printf("quotient: %d\nremainder: %d\n", quotient, remainder);

    return 0;
}
