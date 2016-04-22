
int main(void)
{
    int a;
    int b;
    int c;
    b = 1;
    for (b = 2; b < 3; ++b)
    {
        int d;
        c = 4;
        d = 5;
        while (d > 0) {
            --d;
        }
        c = 6;
    }
    b = 7;
    if (b == 7) {
        int e;
        e = 8;
        b = 9;
        c = 10;
    }
    a = 11;
    b = 12;
    c = 13;
    return a + b + c;
}
