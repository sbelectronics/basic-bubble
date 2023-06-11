import sys

b = sys.stdin.read()

while b[-1] == chr(0xff):
  b = b[:-1]

# make length a multiple of 16
while len(b)%16 != 0:
  b = b + chr(0xff)

# pad it out to 32K
while len(b) < 32768:
    b = b + chr(0xff)

# write it twice, once for each bank
sys.stdout.write(b)
sys.stdout.write(b)
