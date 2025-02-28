#include <iostream>

template <typename T> T cast_int_1(int x) { return static_cast<T>(x); }
template <typename T> T cast_int_2(int x) { return static_cast<T>(x); }

int main() {
  std::cout << cast_int_1<float>(1) << std::endl;
  std::cout << cast_int_2<float>(2) << std::endl;
}
