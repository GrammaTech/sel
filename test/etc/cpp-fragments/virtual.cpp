struct Foo {
  virtual void f();
  virtual void g();
};

void Foo::f() {}
void Foo::g() {}

template <void (Foo::*)()>
void h() {}

void x() {
  h<&Foo::f>();
  h<&Foo::g>();
}
