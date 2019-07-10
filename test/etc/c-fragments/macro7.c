#define FOO(x) x
#define BAR(a,b) FOO((a)+(b))

int f(int a,int b) {
  BAR(a,b);
}
