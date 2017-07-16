class Scopes {
    int member;

    void method(int x);
};

void Scopes::method(int x)
{
    int y;
    if (x > 0) {
        int z;
        z = y + 1;
        y = 0;
    }
}
