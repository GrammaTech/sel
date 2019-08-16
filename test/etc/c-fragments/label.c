int f() {
  goto L1;
 L1:
  return 0;
}

int g() {
  goto L2;
 L2:;
  return 0;
}
