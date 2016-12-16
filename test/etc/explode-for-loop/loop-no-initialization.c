#include <stdlib.h>

int main(int argc, char** argv) {
    int i = 1, r = 1;

    for (; i <= atoi(argv[1]); i++) {
        r*=i;
    }

    return r;
}
