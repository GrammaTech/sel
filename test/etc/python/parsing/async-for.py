async for i in range(0, 5):
    j = i

async for i in 'banana':
    j = i
else:
    j = 'hi'
