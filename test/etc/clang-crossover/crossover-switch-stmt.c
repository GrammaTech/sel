#include <stdio.h>

int main(int argc, char** argv) {
    printf("%d\n", argc);

    switch(argc) {
    case 1:
        printf("%d\n", argc + argc);
    case 2:
        printf("%d\n", argc * argc);
    default:
        printf("%d\n", argc * argc + argc);
    }

    return 0;
}
