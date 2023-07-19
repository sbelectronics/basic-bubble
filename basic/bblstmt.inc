        ; Bubble Load and Save Routines. For simplicity sake, we save everything
        ; from PROGND to 0xFE00. We leave the last 256 bytes unsaved because that
        ; is where the stack is, and we don't want to clobber the stack while we're
        ; using the stack. By measurement, when BLOAD is called, after it does CLRPTR,
        ; the SP is at TOP_OF_MEM-54. So 256 bytes seems plenty safe.

        ; Each "Program" is ~ 32K in size. We can store 4 programs, and they're numbered
        ; 0-3.

        ; For example, "BSAVE 0", "BLOAD 0"

BLOAD:  CALL    GETINT          ; Get program number into A
        RET     NZ
BLOADA: LD      (BLDRUN),A      ; Save 'A', including the autorun bit
        AND     A,07FH          ; Strip off the autorun bit
        CP      A,4
        JP      NC,OVERR        ; Only programs 0-3 are allowed

        ;       SETUP FOR BUBBLE WRITE
        ; 	BARL <- LSB OF BUBBLE ADDRESS
        ; 	BARH <- MSB OF BUBBLE ADDRESS
        ; 	BC <- NUMBER OF BYTES TO TRANSFER
        ; 	DE <- STARTING RAM ADDRESS OF SOURCE

        RLCA                    ; multiply program number by 2
        LD      (BARH),A        ; store prgnum*2 to BARH = 16K offset into bubblespace
        LD      A,0
        LD      (BARL),A        ; bubble lower address is 0

        ;       We need to make sure that we don't read memory over the top of
        ;       the working stack. The stack gets pushed down if there is
        ;       string space above it. So, reset top of memory, reset stringspace
        ;       and then clear everything.

        LD      HL,0FFFFH       ; Assume top of memory is at 0xFFFF
        LD      DE,-50          ; 50 Bytes string space
        LD      (LSTRAM),HL     ; Save last available RAM
        ADD     HL,DE           ; Allocate string space
        LD      (STRSPC),HL     ; Save string space
        POP     DE              ; save return address in DE
        CALL    CLRPTR          ; this will wipe everything, including the stack
        PUSH    DE              ; put return address back on the stack

        LD      BC, 040H        ; Need to read in PROGND so we can CALCPGS. Might as well read 64 bytes.
        LD      DE, PROGND
        CALL    BREADC
        CP      A,040H          ; 40 is success
        JP      Z,BLDGD0
        CP      A,042H          ; 42 is success with parity error
        JP      Z,BLDGD0
        LD      HL,MBBAD
        CALL    PSHEXA
        JP      BLOAD2          ; We're outta here.

BLDGD0:
        ;CALL    PHEXSP          ; debugging that the stack is good

        CALL    CALCPS          ; calculate program and header size

        LD      DE, PROGND      ; progam header starts at prognd
        CALL    BREADC
        CP      A,040H          ; 40 is success
        JP      Z,BLDGD
        CP      A,042H          ; 42 is success with parity error
        JP      Z,BLDGD
        LD      HL,MBBAD
        CALL    PSHEXA          ; Bad mojo
        JP      BLOAD2

BLDGD:  LD      A,(BLDRUN)      ; Check autorun flag
        AND     A,080H
        JP      Z,BLOAD2        ; not set -- don't autorun

        LD      HL,TTYRUN       ; replace RINPUT with one that loads "RUN<null>"
        LD      (RINPUT+1),HL
        JP      SETPTR          ; Set pointers and return

BLOAD2: CALL    PRNTOK          ; Printing OK before SetPTR is what nascom basic does
        JP      SETPTR          ; Set pointers and return
        

BSAVE:  CALL    GETINT          ; Get program number into A
        RET     NZ
        CP      A,4
        JP      NC,OVERR        ; Only programs 0-3 are allowed
        PUSH    HL

        ;       SETUP FOR BUBBLE WRITE
        ; 	BARL <- LSB OF BUBBLE ADDRESS
        ; 	BARH <- MSB OF BUBBLE ADDRESS
        ; 	BC <- NUMBER OF BYTES TO TRANSFER
        ; 	DE <- STARTING RAM ADDRESS OF SOURCE
        RLCA                    ; multiply program number by 2
        LD      (BARH),A        ; store prgnum*2 to BARH = 16K offset into bubblespace
        LD      A,0
        LD      (BARL),A        ; bubble lower address is 0

        CALL    CALCPS          ; calculate program and header size

        LD      DE, PROGND      ; progam header starts at prognd
        CALL    BWRITEC
        CP      A,040H          ; 40 is success
        JP      Z,BSVGD
        CP      A,042H          ; 42 is success with parity error
        JP      Z,BSVGD
        LD      HL,MBBAD
        CALL    PSHEXA          ; Bad mojo
        JP      BSAVE2

BSVGD:
BSAVE2: POP     HL
        RET

        ; BAUTO <n>
        ;
        ; Set program to auto-load

BAUTO:  CALL    GETINT          ; Get program number into A
        RET     NZ
        CP      A,4
        JP      NC,OVERR        ; Only programs 0-3 are allowed
        LD      (BBLAUTO), A
        JMP     BWRSET          ; Save settings

        ; BNOAUTO
        ;
        ; Set no auto-load

BNOAUT: LD      A,0FFH
        LD      (BBLAUTO), A
        JMP     BWRSET          ; Save settings

        ; BINIT
        ;
        ; Initialize the bubble memory. It has been shown that it often fails the first
        ; time with a 31 (error, timing, FIFO) code. So do it twice. This is called
        ; automatically at startup, though may also be invoked at runtime with BINIT.

BINIT:  PUSH    HL
        CALL    BBLINIT
        CPI     040H
        JZ      BGOOD
        CALL    BBLINIT         ; Try once more
        CPI     040H
        JZ      BGOOD
        LD      HL,MBBAD        ; Point to message
        CALL    PSHEXA
        JMP     BINOUT
BGOOD:  
BINOUT: POP     HL
        JP      BRDSET          ; Jump to read settings
