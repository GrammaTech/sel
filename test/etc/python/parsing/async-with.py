async with open("f") as f:
    f.read()

async with open("f") as f, open("g") as g:
    f.read()
    g.read()
