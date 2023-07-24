module a:impl_part;
import :interface_part;
import b;

A::A(int x, int y): x(x), y(y) {}

void A::print() {
    B::print(x, y);
}