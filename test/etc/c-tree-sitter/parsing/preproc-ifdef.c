#ifdef test
#endif

int b = 0;

union {
    unsigned char *a;
#ifndef THING
    char *b;
#endif
} c;
