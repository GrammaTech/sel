int global;

void foo(int a) {
    int b;
    int c;
    b = 0;
    c = a + b + global;
    {
        char d;
        d = 'x';
    }

    if (1)
        return;
    int e;
}

void bar() {
    foo(0);
    bar();
}
