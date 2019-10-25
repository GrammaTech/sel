struct S { int a; };
constexpr S ss = { 0 };
static_assert(ss.a == 0, "");
