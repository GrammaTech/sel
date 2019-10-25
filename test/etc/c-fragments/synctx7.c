extern void g1();
extern void g2();

void f1(int a) {
  if (a) g1(); else g2();
}

void f2(int a) {
  if (a) { g1(); } else g2();
}

void f3(int a) {
  if (a) { g1(); }
}

void f4(int a) {
  if (a) g1(); else { g2(); }
}

void f5(int a) {
  if (a) ; else { g2(); }
}

void f6(int a) {
  if (a) { } else { g2(); }
}
