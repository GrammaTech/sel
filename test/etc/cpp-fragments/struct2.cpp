struct a {
  a(int);
};
struct b {
  b(a);
};
void c() { b(0); }
