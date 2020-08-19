def test():
    a = 10

    def test2():
        nonlocal a
        return a

    return a + test2()
