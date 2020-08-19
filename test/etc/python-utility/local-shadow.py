a = 10

def test():
    a = 20

    def test2():
        nonlocal a
        print(a)
