; Bubble Test, hand-ported from Craig Andrews source

DEFC	BTDBUF	= 0E000h	; DATA BUFFER -- PUT IT WAY OUT THERE.
DEFC	BTDRB	= 0E100h	; DATA BUFFER READBACK

BTEST:	; Erase any basic program that might be running
        LD      HL,0FFFFH       ; Assume top of memory is at 0xFFFF
        LD      DE,-50          ; 50 Bytes string space
        LD      (LSTRAM),HL     ; Save last available RAM
        ADD     HL,DE           ; Allocate string space
        LD      (STRSPC),HL     ; Save string space
        POP     DE              ; save return address in DE
        CALL    CLRPTR          ; this will wipe everything, including the stack
        PUSH    DE              ; put return address back on the stack

	; Now proceed with the test

	XRA	A		; CLEAR ACCUMULATOR
	STA	BARL		; SAVE BUBBLE STARTING PAGE NUMBER
	STA	BARH
	CALL	PRNTCRLF
	LXI	H,BUBTMSG	; MESSAGE TO USER
	CALL    PRSSAFE 

BUBTFIL:			; FILL BUFFER WITH TWO-BYTE PAGE NUMBER
	LHLD	BAR
	CALL	PHEXHL

	XCHG			; PAGE ADDRESS -> DE
	LXI	H,BTDBUF	; DATA BUFFER

BUBTFLP:
	MOV	M,D		; BIG ENDIAN FILL OF BUFFER WITH PAGE NUMBER
	INX	H
	MOV	M,E
	INX	H
	MOV	A,L		; FILL BYTE NUMBER
	CPI	64D		; LAST FILL LOCATION
	JNZ	BUBTFLP		; LOOP UNTIL DONE

	LXI	H,0064D		; NUMBER OF BYTES TO WRITE
	LXI	D,BTDBUF	; START OF FILL BUFFER DATA
	CALL	BBLWRIT		; WRITE TO BUBBLE

	LXI	H,0064D		; NUMBER OF BYTES TO READ
	LXI	D,BTDRB	; START OF OUTPUT BUFFER DATA
	CALL	BBLREAD		; READ FROM BUBBLE

BUBTSKP:


; NOW CHECK IF BUFFERS MATCH, SEND MSG WITH EACH ERROR

	LXI	H,BTDBUF	; ORIGINAL DATA LOCATION
	LXI	D,BTDRB	; READBACK DATA LOCATION

BUBTCHK:
	MOV	A,M		; GET ORIGINAL DATA
	XCHG			; HL=READBACK, DE=ORIGINAL
	CMP	M		; COMPARE ORIGINAL DATA WITH READBACK
	XCHG			; HL=ORIGINAL, DE=READBACK
	CNZ	BUBTFAI		; IF NOT A MATCH, DISPLAY ERROR
	INX	H
	INX	D
	MOV	A,L
	CPI	64D
	JNZ	BUBTCHK

	MVI	A,8		; BACKSPACE OVER PAGE NUMBER
	CALL    OUTC
	MVI	A,8
	CALL	OUTC
	MVI	A,8
	CALL	OUTC
	MVI	A,8
	CALL    OUTC
	LHLD	BAR		; GET BUBBLE PAGE
	INX	H		; INCREMENT
	SHLD	BAR		; SAVE PAGE NUMBER TO BUFFER

	MOV	A,H
	CPI	08H		; IS THE HIGH BYTE ONE PAST THE LAST BUBBLE PAGE (8) ?
	JNZ	BUBTFIL		;  NO, LOOP AGAIN
	MOV	A,L		; GET LOW BYTE
	CPI	00H		; IS THE LOW BYTE ONE PAST THE LAST BUBBLE PAGE?
	JNZ	BUBTFIL		;  NO, LOOP AGAIN
	JMP	BUBTOUT		;  YES, DONE SO RETURN

BUBTFAI:
	MOV	C,L		; BYTE NUMBER INTO C
	LXI	H,BTMFAB
	CALL	PRSSAFE

	MOV	A,C		; GET BYTE NUMBER
	CALL	PHEXA

	LXI	H,BTMFAW
	CALL	PRSSAFE

	MVI	H,20H		; RESTORE HL TO ORIGINAL BUFFER
	MOV	L,C		;
	MOV	A,M		; GET ORIGINAL DATA
	CALL	PHEXA
	LXI	H,BTMFAR
	CALL	PRSSAFE
	MVI	H,21H		; HL = READBACK
	MOV	L,C		; RESTORE HL TO READBACK BUFFER
	MOV	A,M
	CALL	PHEXA
	CALL	PRNTCRLF
	LXI	H,BUBTMSG	; OUTPUT PAGE MESSAGE AGAIN
	CALL	PRSSAFE
	LHLD	BAR		; GET PAGE NUMBER AGAIN
	CALL	PHEXHL		; DISPLAY FOR NEXT LOOP
	MVI	H,20H		; RESTORE BUFFER ADDRESSES
	MOV	L,C
	JMP	BUBTOUT

BUBTOUT:
	CALL	PRNTCRLF
	CALL	PRNTOK
	JP      SETPTR          ; Basic: Set pointers and return
	RET

PRSSAFE:			; PRS may alter BC 
	PUSH	BC
	CALL	PRS
	POP	BC
	RET

BUBTMSG:	DEFB	"TESTING PAGE --",0,0
BTMFAB:	DEFB	"  FAILED ON BYTE --",0,0
BTMFAW:	DEFB	"  WRITE VALUE --",0,0
BTMFAR:	DEFB	"  READ VALUE --",0,0