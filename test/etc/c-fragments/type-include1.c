#include "type-include1.h"

int x;
struct s z;
foo_t p;
void f() { p = &x; }
void g() { p = &z.a; }

uint32_t u;
