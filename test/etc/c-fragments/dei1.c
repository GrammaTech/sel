/* designated init exprs */

typedef struct point {
  double x;
  double y;
} point;

void f() {
  point pts[2] = { [0 ... 1].y = 1.0, [1].x = 2.0, [0].x = 3.0 };
}
