extern void f1();
extern void f2(int);

void g() {
  try { f1(); }
  catch (int e) {
    f2(e);
  }
}
