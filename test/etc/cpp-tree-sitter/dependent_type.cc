#include <vector>
template <typename T> void foo(const std::vector<T> &v) {
  typedef typename std::vector<T> tvec;
  tvec *p2;
}
