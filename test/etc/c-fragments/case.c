int f(int i) {
  switch (i) {
  case 1: return 16;
  case 2: case 3:
    return 27;
  default:;
  }
  return 0;
}
