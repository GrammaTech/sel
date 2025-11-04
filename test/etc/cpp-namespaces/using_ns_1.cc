namespace A {
int i;
}

namespace B {
int i;
int j;

namespace C {
namespace D {
using namespace A;
// Names from A are "injected" into D.
// Unqualified lookup within D considers these names to have the same
// scope as the global scope (e.g. for the purposes of name hiding).
// Qualified lookup referring to D (D::name for some name)
// will find the same name as unqualified lookup within D.

int j;
int k;
int a = i; // i is B::i, because A::i is hidden by B::i
// int b = ::i; // error: there is still no i in the global namespace
} // namespace D

using namespace D; // names from D and A are injected into C

int k = 89; // OK to declare name identical to one introduced by a using
// int l = k;  // ambiguous: C::k or D::k
int m = i; // ok: B::i hides A::i
int n = j; // ok: D::j hides B::j
} // namespace C
} // namespace B

int main() {
  // These are all equivalent definitions:
  int t0 = B::i;
  int t1 = B::C::a;
  int t2 = B::C::D::a;
}
