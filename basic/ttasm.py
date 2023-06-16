import sys
for line in sys.stdin.readlines():
   line = line.strip()
   if line:
       print('\tDEFB\t"%s",CR,LF' % line)
   else:
       print('\tDEFB\tCR,LF')
print('\tDEFB\t0,0');
