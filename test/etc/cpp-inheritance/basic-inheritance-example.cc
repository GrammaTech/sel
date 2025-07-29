struct Base {
  int a, b, c;
};

// every object of type Derived includes Base as a subobject
struct Derived : Base {
  int b;
};

// every object of type Derived2 includes Derived and Base as subobjects
struct Derived2 : Derived {
  int c;
};

int main() {
  Derived2 derived2 = Derived2{0, 1, 2};
  derived2.a;
  derived2.b;
  derived2.c;
}
