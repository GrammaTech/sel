a = 10

def test():
    a = 20

    def test2():
        nonlocal a

        def test3(b):
            print(b)

        print(a)

    def test():
        a = 20

        def test2():
            nonlocal a

            def test3(b):
                print(b)

            print(a)

        pass


    pass
