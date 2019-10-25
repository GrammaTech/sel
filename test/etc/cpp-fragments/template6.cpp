struct a {
  void b();
};
template <void (a::*)()> void c();
void d() { c<&a::b>; }
