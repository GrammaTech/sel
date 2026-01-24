#include <iostream>

struct Summable {
  int x;
  int y;
  int sum();
};

int Summable::sum() { return x + y; }

int main() {
  Summable s = Summable{1, 2};
  std::cout << s.sum() << std::endl;
}
