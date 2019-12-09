#define FOO(x,y) int x, y

FOO(a,b);

void f() {
  FOO(c,d);
}
