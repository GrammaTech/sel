struct B {
  virtual void f();
};

struct D : B {
  void f(int); // D::f hides B::f (wrong parameter list)
};

struct D2 : D {
  void f(); // D2::f overrides B::f (doesn't matter that it's not visible)
};
