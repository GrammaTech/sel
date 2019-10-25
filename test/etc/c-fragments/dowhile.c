extern void g();

void f(int i) {
  do {
    g();
  } while ((--i) > 0);
}
