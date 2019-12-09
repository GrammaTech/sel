class S { public: int x; };
bool& operator<<(bool& o, const S &s) { return o; }
template<typename _Ty>
void r(const _Ty& v) { 0 << v; }
