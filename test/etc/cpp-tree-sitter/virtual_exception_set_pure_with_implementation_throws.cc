#include <iostream>
#include <stdexcept>

class Foo {
public:
  virtual void action() = 0;
};

void Foo::action() { throw std::runtime_error("Error!"); }

class Bar : public Foo {
public:
  virtual void action() override { std::cout << "OK" << std::endl; }
};

int main(int argc, char *argv[]) {
  Foo *obj;
  Bar bar{};
  obj = &bar;
  obj->action();
}
