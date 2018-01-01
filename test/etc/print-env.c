#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
    if( argc == 2){
        puts(getenv(argv[1]));
        return 0;
    } else {
        puts("Call with a single argument.");
    }
    return 1;
}
