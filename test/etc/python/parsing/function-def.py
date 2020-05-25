import functools

def f():
    return 0

def g(a, /, b):
    return a

def h(a, /, b, *, c):
    return a

def i(a : "a", /, b: "b", *, c: "c"):
    return a

def j(a=0, /, b=1, *, c=2):
    return a

def k(a=0, /, b=1, *args, c=2):
    return a

def l(a=0, /, b=1, *args, c=2, **kwargs):
    return a

def m(a: "a"=0, /, b: "b"=1, *args, c:"c"=2, **kwargs) -> "annotation":
    return a

@wraps
@functools.lrucache
def n(a: "a"=0, /, b: "b"=1, *args, c:"c"=2, **kwargs) -> "annotation":
    return a
