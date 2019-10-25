#define a(b) __builtin_##b
char *a(__strcpy_chk)(char *, const char *, typeof(sizeof(int)));
