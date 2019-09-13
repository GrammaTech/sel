int x;
int y;
void f() {
   { int t; t = x; x = y; y = t; }
}
