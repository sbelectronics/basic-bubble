        ; LBAUD
        ;
        ; Set baud for port1

LBAUD:  LD      B,1             ; B=1, set baud rate for port1
        JP      BAUDS

BAUD:   LD      B,0             ; B=0, set baud rate for port0
BAUDS:  PUSH    BC
        CALL    GETNUM          ; Get address
        CALL    DEINT           ; Get integer -32768 to 32767 to DE
        POP     BC
        PUSH    HL
        LD      HL,BTABLE
        LD      C,0
BAUDL:  LD      A,(HL)
        INC     HL
        CP      E
        JP      NZ,BAUDN1
        LD      A,(HL)
        INC     HL
        CP      D
        JP      NZ,BAUDN2
        LD      A,B
        CMP     1
        JP      Z, BAUD1
        MOV     A,C
        LD      (BBLBAUD),A
        JP      BAUD2
BAUD1:
        MOV     A,C
        LD      (BBLBAUD1),A
BAUD2:
        CALL    BWRSET
        LD      HL,MBAUD
        CALL    PRS
        JP      BAUDO
BAUDN1:
        INC     HL
BAUDN2: LD      A,C
        INC     C
        CP      (BTABLEE-BTABLE)/2
        JP      NZ,BAUDL
        JP      IDERR
BAUDO:  POP     HL
        RET

BTABLE:  
        DEFW    300
        DEFW    1200
        DEFW    2400
        DEFW    4800
        DEFW    9600
        DEFW    19200
        DEFW    3840
        DEFW    5760
        DEFW    11520
BTABLEE:

        ; CONSOLE <n>
        ;
        ; Set console mask. Bit0=port0, Bit1=port1

CONSOL: CALL    GETNUM          ; Get address
        CALL    DEINT           ; Get integer -32768 to 32767 to DE
        LD      A,E
        LD      (CONPORT),A
        LD      (serPort),A
        CALL    BWRSET
        PUSH    HL
        LD      HL,MCONS
        CALL    PRS
        POP     HL
        RET

        ; PRINTER <n>
        ;
        ; Set printer mask. Bit0=port0, Bit1=port1

PRINTR: CALL    GETNUM          ; Get address
        CALL    DEINT           ; Get integer -32768 to 32767 to DE
        LD      A,E
        LD      (PRNPORT),A
        CALL    BWRSET
        PUSH    HL
        LD      HL,MPRN
        CALL    PRS
        POP     HL
        RET

