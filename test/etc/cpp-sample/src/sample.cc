#include "include/sample.h"

class Sample
{
public:
    void run() { std::cout << "This is sample program with generic makefile.\n"; }
};

int main (int argc, char* argv[]) {
    (new Sample())->run();
}
