void extern g();

void f1(int n) {
  int i;
  for (i=0; i<n; i++) g();
}

void f2(int n) {
  int i;
  for (i=0; i<n; i++) { g(); };
}
