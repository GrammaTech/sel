s = {1, 2, 3, 4}
t = {i**2 for i in s}
t = {i**2 for i in s if i % 2}
t = {i**2 if i%2 else i**3 for i in s}
t = {{j**2 for j in i} for i in s}
t = {i for i in s for j in i}
