extern void g();

void f1(int i1) {
  do g(); while (--i1 > 0);
}

void f2(int i2) {
  do { g(); } while (--i2 > 0);
}
