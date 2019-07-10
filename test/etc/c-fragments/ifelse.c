int f(int i) {
  if (i == 1) return 10;
  else if (i == 2) {
    return 20;
  } else if (i > 3)
    if (i == 4) return 20;
    else return 30;
  else return 40;
}
