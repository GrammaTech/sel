#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <complex.h>

enum traffic { RED, YELLOW, GREEN };

typedef union {
  int foo;
  char bar;
} test_union;

typedef struct point {
  double x;
  double y;
} point;

int add3(int x) {
  // PredefinedExpr (__func__)
  printf("Running function: %s\n", __func__);
  return x + 3;
}

int sub3(int x) {
  return x - 3;
}

int variadic_sum(int num_args, ...) {
  int next = 0;
  // VAArgExprs
  va_list nums;
  int sum;
  sum = 0;
  va_start(nums, num_args);
  while((next = va_arg(nums, int)) && num_args > 0) {
    sum += next;
    num_args--;
  }
  va_end(nums);
  return sum;
}

int main(int argc, char** argv) {
  enum traffic light = RED;
  test_union run, sun;
  test_union* tun;
  // CompoundLiteralExpr
  run.foo = (int) {5};
  sun.bar = 'a';

  // GenericSelectionExpr
  tun->foo = _Generic(tun->foo, int: 12, char: 'q');
  double complex z = 4.0 + 8.0 * I;
  int total = variadic_sum(4, 8, 1, 4, 2);
  printf("total: %d\n", total);

  // StmtExpr (GNU expression extension)
  int stmt_expr = ({ int x = 4; x; });
  printf("stmt_expr: %d\n", stmt_expr);

  // DesignatedInitExpr
  point pts[2] = { [0 ... 1].y = 1.0, [1].x = 2.0, [0].x = 3.0 };
  // InitListExpr
  point origin = { 0.0, 0.0 };
  point one_two = { .y=2.0, .x = 1.0 };

  printf("point 0: (%lf, %lf)\n", pts[1].x, pts[1].y);
  // OffsetOfExpr
  printf("offset of y in point: %ld\n", offsetof(point, y));

  // NullStmt
  ;

  do {
    run.foo++;
  } while(run.foo < 8);

  // switch
  switch(argc) {
    case 1:
        printf("%d\n", argc + argc);
        break;
    default:
        printf("%d\n", argc * argc + argc);
    }

  void const* l1_ptr = &&l1;
  int x = 0;
  int y = (int) x == 0 ? 1 : 0;
  // indirect goto
  goto *l1_ptr;
  x = 5;
l2:
  x += sizeof(int);
  printf("x: %d\n", x);
  goto l3;
l1:
  x = sub3(x);
  printf("x: %d\n", x);
  // goto
  goto l2;

l3:
// AttributedStmt
#pragma unroll(10)
  for(; x < 20; x++) {
    if(x > 14) {
      printf("%d > 14\n", x);
      continue;
    }
    else
      printf("%d <= 14\n", x);
    x = add3(x);
  }

  return 0;
}
