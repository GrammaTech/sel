#include <stdio.h>

int main(int argc, char** argv) {
    argc += ((argc*4) / rand());
    return argc;
}
