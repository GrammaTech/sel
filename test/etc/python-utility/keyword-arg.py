def test(a, b=10, *, required, **args):
    pass

test(1, required=2, b=3, c=4, d=5)
