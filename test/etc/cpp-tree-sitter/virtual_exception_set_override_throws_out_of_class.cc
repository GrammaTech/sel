#include <iostream>
#include <stdexcept>

class Foo {
public:
  virtual void action() { std::cout << "OK" << std::endl; }
};

class Bar : public Foo {
public:
  virtual void action() override;
};

void Bar::action() { throw std::runtime_error("Error!"); }

int main(int argc, char *argv[]) {
  Foo *obj;
  Bar bar{};
  Foo foo{};
  if (argc == 1) {
    obj = &bar;
  } else {
    obj = &foo;
  }
  obj->action();
}
