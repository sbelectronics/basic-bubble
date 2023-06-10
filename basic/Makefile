#export PATH := /home/smbaker/projects/pi/zcc/bin:$(PATH)
export PATH := /home/smbaker/projects/pi/z88dk/bin:$(PATH)
export ZCCCFG=/home/smbaker/projects/pi/z88dk/lib/config

board_32k.rom: *.asm *.inc *.lst
	zcc +z80 --no-crt -v -m --list -Ca-f0xFF @nascom32k.lst -o board_32k
	z88dk-appmake +glue -b board_32k --ihex --pad --filler 0xFF --recsize 24 --clean
	python ./trim.py < board_32k__.bin > board_32k.rom
	python ./trimdup.py < board_32k__.bin > board_32k_bothbanks.rom
clean:
	rm -f *.hex board_32k *.bin *.rom
