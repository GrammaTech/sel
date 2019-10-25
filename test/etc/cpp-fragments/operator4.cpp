class c {
public:
  int i;
  int operator++();
};

void f(c *p) {
  ++(*p);
}
