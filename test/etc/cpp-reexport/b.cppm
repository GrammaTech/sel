module;
#include <iostream>
export module b;
namespace b {
export void say_hello() { std::cout << "Hello" << std::endl; }
void say_goodbye() { std::cout << "Goodbye" << std::endl; }
} // namespace b
