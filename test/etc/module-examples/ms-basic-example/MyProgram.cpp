// MyProgram.cpp
#include <iostream>

import Example;
// import std.core;

using namespace std;

int main() {
  cout << "The result of f() is " << Example_NS::f() << endl; // 42
  // int i = Example_NS::f_internal(); // C2039
  // int j = ANSWER; //C2065
}
