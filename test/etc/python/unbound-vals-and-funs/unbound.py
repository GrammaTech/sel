class Obj():
    def __init__(self):
        pass
    def function(self, x, y):
        return x * y

def f(x, y):
    return x * y

if __name__ == '__main__':
    i = 3
    j = 4
    obj = Obj()
    obj.function(i, j)
    f(i, j)
