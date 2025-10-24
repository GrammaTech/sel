#include <iostream>

class Destructible {
public:
  virtual ~Destructible();
};

Destructible::~Destructible() {
  std::cout << "Destroying base class" << std::endl;
}

class AlsoDestructible : Destructible {
public:
  ~AlsoDestructible() override = default;
};

int main() {
  AlsoDestructible a{};
  return 0;
}
