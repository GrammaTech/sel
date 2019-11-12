// Example of template functions

template<typename _T> _T foo(_T x);

template<>bool foo<bool>(bool b) { return !b; }

int f1(int x) { return foo(x); }
char f2(char y) { return foo(y); }
