function gcd(a, b) {
  if (a === 0){
    util.format('%d\n', b);
  }
  while (b != 0) {
    if (a > b) {
      a = a - b;
    } else {
      b = b - a;
    }
  }
  util.format("%d\n", a);
}
