#include <string>

int main(int argc, char* argv[])
{
    std::string x = argv[1];
    if (x == "test")
        return 1;
    return 0;
}
