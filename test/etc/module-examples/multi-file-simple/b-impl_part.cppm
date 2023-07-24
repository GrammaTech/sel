module;
#include <iostream>
module b:impl_part;
import :interface_part;

namespace B {

void print (int x, int y) {
    std::cout << x << " " << y << std::endl;
}

}