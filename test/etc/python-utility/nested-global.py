a = 10

def test():
    a = 20

    def test2():
        global a
        return a

    return a + test2()
