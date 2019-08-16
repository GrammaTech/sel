class a {};
void operator<<(a, char *) {
  a b;
  b << "";
}
