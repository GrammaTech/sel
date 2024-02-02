#include <memory>
#include <stdio.h>

class MyClass {
public:
  void print_class_name() { printf("MyClass\n"); }
};

int main() {
  printf("This program defines: \n");
  MyClass myclass = new MyClass();
  myclass.print_class_name();
  delete myclass;
  return 0;
}
