int g();
void h(int);
int k(int,...);

int f() {
  h(0);
  h(g());
  k(1,2);
  return g();
}
