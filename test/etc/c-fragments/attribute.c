switch (n) {
case 1:
case 2:
  g();
  [[fallthrough]];
case 3: // no warning on fallthrough
  h();
}
