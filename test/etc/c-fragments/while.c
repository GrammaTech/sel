extern void g();

int f(int i) {
  while (i > 0) {
    g();
    --i;
  }
}

  
