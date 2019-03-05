#include <stdio.h>
#include <stdlib.h>

#define FUNCTION_LIKE_DEBUG(a,b) \
    if(a){ printf("DEBUG: %s\n", b); };

int main (int argc, char **argv)
{
    if(argc < 3){
        FUNCTION_LIKE_DEBUG(1, "insufficient arguments");
        return 2;
    }
    int x = atoi(argv[1]);
#ifdef DEBUG
    FUNCTION_LIKE_DEBUG(1,"read x");
#endif
    int y = atoi(argv[2]);
#ifdef DEBUG
    FUNCTION_LIKE_DEBUG(1,"read y");
#endif
    FUNCTION_LIKE_DEBUG((x < y),"x is less than y");
    if(x < y)
        printf("%d\n", (y - x));
    else
        printf("%d\n", (y + x));
    return 0;
}
