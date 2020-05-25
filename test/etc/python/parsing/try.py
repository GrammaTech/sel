try:
    a = 1/0
except Exception:
    pass

try:
    a = 1/0
except IOError as io:
    a = io
except Exception as e:
    a = e
else:
    a = 1
finally:
    a = 0
