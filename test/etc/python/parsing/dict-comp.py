d = {1:2, 3:4}
s = {k:v**2 for k,v in d}
s = {k:v**2 for k,v in d if v % 2}
s = {{k2:v2 for k2,v2 in v1} for k1,v1 in d}
s = {k2:v2 for k1,v1 in d for k2,v2 in v1}
