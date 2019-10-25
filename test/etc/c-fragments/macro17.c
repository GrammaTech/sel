#define S(x,y) { int t; t = x; x = y; y = t; }

int a;
int b;
void f() {
  S(a,b)
}
