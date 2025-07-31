#include <iostream>

struct Foo {
  virtual void defined_virtual() {
    std::cout << "Foo method called" << std::endl;
  }
};

struct Bar : Foo {};

struct Baz : Bar {
  virtual void defined_virtual() {
    Bar::defined_virtual();
    std::cout << "Baz method called" << std::endl;
  }
};

int main() {
  Baz baz{};
  baz.defined_virtual();
  return 0;
}
