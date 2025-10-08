struct Base {
  virtual void f() {}
  virtual void g() {}
};

struct Derived : Base {
  void f() override, g() override;
};

void Derived::f() {}

void Derived::g() {}

int main() {}
