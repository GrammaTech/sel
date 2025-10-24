class HasVirtual {
  virtual void fn1();
  virtual void fn2();
};

using HasVirtualAlias1 = HasVirtual;
typedef HasVirtual HasVirtualAlias2;
