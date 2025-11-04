namespace D {
int d1;
void f(char);
} // namespace D
using namespace D; // introduces D::d1, D::f, D::d2, D::f,
                   // E::e, and E::f into global namespace!

int d1; // OK: no conflict with D::d1 when declaring

namespace E {
int e;
void f(int);
} // namespace E

namespace D // namespace extension
{
int d2;
using namespace E; // transitive using-directive
void f(int);
} // namespace D

int main() {
  // d1++;    // error: ambiguous ::d1 or D::d1?
  ::d1++;  // OK
  D::d1++; // OK
  d2++;    // OK, d2 is D::d2

  e++; // OK: e is E::e due to transitive using

  // f(1);   // error: ambiguous: D::f(int) or E::f(int)?

  // NB cppreference claims this is OK, but GCC and Clang++ reject it.

  // f('a'); // OK: the only f(char) is D::f(char)
}
