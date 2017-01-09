#include <stdio.h>

int main(int argc, char** argv) {
    int i;
    int j;

    for (i = 0; i < argc; i++)
        for (j = 0; j < argc; j++)
            printf("%d\n", i+j);

    return 0;
}
