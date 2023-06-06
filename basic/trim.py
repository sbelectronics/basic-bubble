import sys

b = sys.stdin.read()

while b[-1] == chr(0xff):
  b = b[:-1]

# make length a multiple of 16
while len(b)%16 != 0:
  b = b + chr(0xff)

# add one extra row for good measure
for i in range(0,16):
  b = b + chr(0xff)

sys.stdout.write(b)
