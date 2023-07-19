;--------------------------------------------------------
; TLOAD - load from paper tape on port1
;
; Waits for the letters 'T', 'A', 'P', 'E'
; Reads the length, LSB-first
; Emits the following on port1
;   32 characters of 0x16
;   The letters 'T', 'A', 'P', 'E'
;   The program length, MSB-first
;   Skips the checksum
;   Reads the program bytes

TLOAD:  PUSH    HL
        LD      A,(PRNPORT)     ; Port to port1
        LD      (serPort),A

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

TLHC:   RST     10H             ; Loop until we see 'TAPE'
        CMP     A,'T'
        JNZ     TLHC
        RST     10H
        CMP     A,'A'
        JNZ     TLHC
        RST     10H
        CMP     A,'P'
        JNZ     TLHC
        RST     10H
        CMP     A,'E'
        JNZ     TLHC

        RST     10H             ; Get length into BC
        LD      C,A
        RST     10H
        LD      B,A

        RST     10H             ; Skip placeholder for checksum
        RST     10H

        LD      HL,PROGND       ; Read the contents into PROGND
TLDLP:  RST     10H
        LD      (HL), A
        INC     HL
        DEC     BC
        LD      A,B
        OR      A,C
        JNZ     TLDLP

        LD      A,(CONPORT)
        LD      (serPort),A

        CALL    PRNTOK          ; Printing OK before SetPTR is what nascom basic does
        JP      SETPTR          ; Set pointers and return

;--------------------------------------------------------
; TSAVE - save to paper tape on port1
;
; Emits the following on port1
;   32 characters of 0x16
;   The letters 'T', 'A', 'P', 'E'
;   The program length, LSB-first
;   A placeholder for the checksum
;   The program bytes
        

TSAVE:  PUSH    HL
        LD      A,(PRNPORT)     ; Port to port1
        LD      (serPort),A

        LD      A,016H          ; Just like the H8, emit 32 020H to the tape
        LD      B,020H
TSHLP:  RST     08H
        DEC     B
        JNZ     TSHLP

        LD      A,'T'           ; This ought to be clear enough
        RST     08H
        LD      A,'A'
        RST     08H
        LD      A,'P'
        RST     08H
        LD      A,'E'
        RST     08H

        CALL    CALCPS          ; Get program length in BC
        LD      A,C             ; Output low byte of length
        RST     08H
        LD      A,B             ; Output high byte of length
        RST     08H     

        LD      DE,0            ; Calculate 16-bit checksum
        PUSH    BC
        LD      HL, PROGND      
TSCLP:  LD      A,(HL)
        ADD     A,E             ; E = E + A
        LD      E,A
        LD      A,0
        ADC     A,D             ; D = D + 0 + carry
        LD      D,A
        INC     HL
        DEC     BC
        LD      A,B
        OR      A,C
        JNZ     TSCLP
        POP     BC

        LD      A,E             ; Write the checksum
        RST     08H
        LD      A,D
        RST     08H

        LD      HL, PROGND      ; Write the data bytes
TSDLP:  LD      A,(HL)
        RST     08H
        INC     HL
        DEC     BC
        LD      A,B
        OR      A,C
        JNZ     TSDLP

        LD      A,(CONPORT)
        LD      (serPort),A

        POP     HL
        RET
