def fibonacci(num):
    a = 1
    b = 0

    for _ in range(num):
        a, b = a + b, a

    return b

fibonacci(10)
