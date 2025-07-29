struct Base {
  virtual int g();
  virtual ~Base() {}
};

struct A : Base {
  // OK: declares three member virtual functions, two of them pure
  virtual int f() = 0;
  virtual int g() override = 0;
  virtual int h();
};
