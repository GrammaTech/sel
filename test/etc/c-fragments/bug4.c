#define c(a, b) (a) || b || a
#define d(a, b) c(a, b)
e() { f(d(f, e)); }
