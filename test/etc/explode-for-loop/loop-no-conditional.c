#include <stdlib.h>

int main(int argc, char** argv) {
    int i, r = 1;

    for (i = 1; ;i++) {
        if (i > atoi(argv[1]))
            break;
        r*=i;
    }

    return r;
}
