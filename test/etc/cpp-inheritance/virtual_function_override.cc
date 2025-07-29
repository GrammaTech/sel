struct Base {
  virtual void f() {}
};

struct Derived : Base {
  void f() override // 'override' is optional
  {}
};
