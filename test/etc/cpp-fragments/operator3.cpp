struct a {
  operator bool();
};
void b() {
  if (a())
    ;
}
