#include <iostream>

struct C {
  int m;
};

struct D : C {
  int n;
};

int main() {
  int x = 1;
  int y = 2;
  D d{x, y};
  std::cout << "M: " << d.m << ", N: " << d.n << "\n";
}
