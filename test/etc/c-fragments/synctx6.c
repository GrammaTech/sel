extern void g();

void f1(int i) {
  while (--i > 0) g();
}

void f2(int i) {
  while (--i > 0) { g(); }
}
