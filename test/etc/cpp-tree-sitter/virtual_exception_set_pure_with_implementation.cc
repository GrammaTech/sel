#include <iostream>
#include <stdexcept>

class Foo {
public:
  virtual void action() = 0;
};

void Foo::action() { std::cout << "OK" << std::endl; }

class Bar : public Foo {
public:
  virtual void action() override { throw std::runtime_error("Error!"); }
};

int main(int argc, char *argv[]) {
  Foo *obj;
  Bar bar{};
  obj = &bar;
  obj->action();
}
