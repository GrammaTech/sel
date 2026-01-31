#include <iostream>

struct Rectangle {
  Rectangle(const double &x, const double &y) : x(x), y(y) {}
  float x, y;
};

int main() {
  Rectangle r(12.5, 4.5);
  std::cout << r.x + r.y << std::endl;

  return 0;
}
