class A:
    pass

class A(B):
    pass

class C(B, metaclass=A):
    pass

@decorator
class D(B, metaclass=A):
    pass
