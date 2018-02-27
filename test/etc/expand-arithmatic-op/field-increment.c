#include <stdio.h>

struct test {
    int x;
};

int main(int argc, char** argv) {
    struct test t;
    t.x++;
    return 0;
}
