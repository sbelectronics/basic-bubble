#export PATH := /home/smbaker/projects/pi/zcc/bin:$(PATH)
export PATH := /home/smbaker/projects/pi/z88dk/bin:$(PATH)
export ZCCCFG=/home/smbaker/projects/pi/z88dk/lib/config

all: board_32k_sio.rom board_32k_scc.rom board_32k_acia.rom

board_32k_sio.rom: *.asm *.inc *.lst bhelp.inc
	zcc +z80 --no-crt -v -m --list -Ca-f0xFF @nascom32k_sio.lst -o board_32k_sio
	z88dk-appmake +glue -b board_32k_sio --ihex --pad --filler 0xFF --recsize 24 --clean
	python ./trim.py < board_32k_sio__.bin > board_32k_sio.rom
	python ./trimdup.py < board_32k_sio__.bin > board_32k_sio_bothbanks.rom
	cp board_32k_sio_bothbanks.rom 00_burn_ya_now.rom

board_32k_scc.rom: *.asm *.inc *.lst bhelp.inc
	zcc +z80 --no-crt -v -m --list -Ca-f0xFF @nascom32k_scc.lst -o board_32k_scc
	z88dk-appmake +glue -b board_32k_scc --ihex --pad --filler 0xFF --recsize 24 --clean
	python ./trim.py < board_32k_scc__.bin > board_32k_scc.rom
	python ./trimdup.py < board_32k_scc__.bin > board_32k_scc_bothbanks.rom

board_32k_acia.rom: *.asm *.inc *.lst bhelp.inc
	zcc +z80 --no-crt -v -m --list -Ca-f0xFF @nascom32k_acia.lst -o board_32k_acia
	z88dk-appmake +glue -b board_32k_acia --ihex --pad --filler 0xFF --recsize 24 --clean
	python ./trim.py < board_32k_acia__.bin > board_32k_acia.rom
	python ./trimdup.py < board_32k_acia__.bin > board_32k_acia_bothbanks.rom

bhelp.inc: bhelp.txt
	echo "MBHELP:" > bhelp.inc
	python ./ttasm.py < bhelp.txt >> bhelp.inc

quadbasic.rom:
	dd if=/dev/zero ibs=1k count=64 | tr "\000" "\377" > quadbasic.rom
	dd if=/home/smbaker/projects/pi/rc2014/NASCOM_BASIC_4.7/rc2014_NascomBasic32k/board_32k.rom of=quadbasic.rom conv=notrunc
	dd if=board_32k_acia.rom bs=1024 seek=16 conv=notrunc of=quadbasic.rom conv=notrunc
	dd if=board_32k_sio.rom bs=1024 seek=32 conv=notrunc of=quadbasic.rom conv=notrunc
	dd if=board_32k_scc.rom bs=1024 seek=48 conv=notrunc of=quadbasic.rom conv=notrunc

clean:
	rm -f *.hex board_32k *.bin *.rom
