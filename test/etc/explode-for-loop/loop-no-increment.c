#include <stdlib.h>

int main(int argc, char** argv) {
    int i, r = 1;

    for (i = 1; i <= atoi(argv[1]);) {
        i++;
        r*=(i-1);
    }

    return r;
}
