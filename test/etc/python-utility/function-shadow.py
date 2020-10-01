# 1
def test():
    pass

def outer():
    # 2
    test()

    def inner():
        # 3
        test()

        test = 10
        return test + test + test

    # 4
    test()

    test = 100
    return test + inner()

# 5
test()

test = 100
test + test
