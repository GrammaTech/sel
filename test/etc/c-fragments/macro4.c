#define FOO(x) BAR((x)+1)
#define BAR(y) (y)+2

int f(int z) {
  BAR(z);
}
