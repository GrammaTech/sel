#define F(a,b) a#b

char* f() {
  return F("foo","bar");
}
