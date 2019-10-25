class a {
public:
  void operator+(a);
};
void b() {
  a c;
  a d;
  c + d;
}
