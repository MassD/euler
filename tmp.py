import time

def isPal(n):
  s = str(n)
  return s == s[::-1]

def pairsIter():
  num = 0
  while(True):
    for i in range(int(num/2), -1, -1):
      yield((i, num - i))
    num += 1
  
def findLargestMultiple(digits):
  x = y = 10 ** digits - 1
  for i, j in pairsIter():
    print(i)
    print(j)
    k = (x - i) * (y - j)
    if isPal(k):
      return k

start = time.time()
print(findLargestMultiple(4))
end = time.time()
print(end - start)
