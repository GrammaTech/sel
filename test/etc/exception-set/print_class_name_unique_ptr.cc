#include <memory>
#include <stdio.h>

class MyClass {
public:
  void print_class_name() { printf("MyClass\n"); }
};

int main() {
  printf("This program defines: \n");
  std::unique_ptr<MyClass> myclass;
  myclass->print_class_name();
  return 0;
}
