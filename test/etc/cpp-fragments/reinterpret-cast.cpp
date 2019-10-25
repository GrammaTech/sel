struct a {
  int b;
};
int c = reinterpret_cast<a *>(0)->b;
