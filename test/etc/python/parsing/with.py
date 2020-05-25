with open("f") as f:
    f.read()

with open("f") as f, open("g") as g:
    f.read()
    g.read()
