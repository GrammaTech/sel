#include <iostream>

// the following template function definition relies on the availability of methods `area` and `circ` for an `object`,
// but -- unlike in Rust -- the availability is not checked/enforced until instantiation time.
template <typename T>
void print_size(const T& object) {
  std::cout << "The area of the given geometric object is " << object.area() << "." << std::endl;
  std::cout << "The circumference of the given geometric object is " << object.circ() << "." << std::endl;
}

struct Rectangle {
  Rectangle(const double& x, const double& y): x(x), y(y) {}
  float x, y;
  float area() const { return x * y; }
  float circ() const { return 2 * (x + y); }
};

int main() {
  Rectangle r ( 12.5, 4.5 );
  print_size(r);

  return 0;
}
