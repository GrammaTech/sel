#include <iostream>

// A function that calls itself.
std::string num_name(unsigned int n) {
  if (n == 0) {
    throw std::runtime_error("Out of numbers!");
  } else {
    return num_name(n - 1);
  }
}

int fun2(unsigned int n);

// Two functions that call each other.
int fun1(unsigned int n) {
  if (n == 0) {
    throw std::runtime_error("Out of numbers!");
  } else {
    return fun2(n - 1);
  }
}

int fun2(unsigned int n) {
  if (n == 0) {
    throw std::runtime_error("Out of numbers!");
  } else {
    return fun1(n - 1);
  }
}

int main(int argc, char *argv[]) {
  std::cout << num_name(argc) << std::endl;
  return 0;
}
