class c {
public:
  int i;
  int operator++(int);
};

void f(c *p) {
  (*p)++;
}
