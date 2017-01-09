int fib(int n) {
    int x = 0;
    int y = 1;
    while (n > 0) {
        int t = x;
        x = x + y;
        y = t;
    }
    return x;
}
