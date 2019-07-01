#define FOO(x) x
#define BAR(y) FOO((y)+2)

int f(int z) {
  BAR(z);
}
