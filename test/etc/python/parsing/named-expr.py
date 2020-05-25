import re

s = 'hello world'
if m := re.match('hi', s):
    print(m)
