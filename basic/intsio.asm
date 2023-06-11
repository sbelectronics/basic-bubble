;==============================================================================
;
; The rework to support MS Basic HLOAD, RESET, MEEK, MOKE,
; and the Z80 instruction tuning are copyright (C) 2020-23 Phillip Stevens
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;
; ACIA 6850 interrupt driven serial I/O to run modified NASCOM Basic 4.7.
; Full input and output buffering with incoming data hardware handshaking.
; Handshake shows full before the buffer is totally filled to allow run-on
; from the sender. Transmit and receive are interrupt driven.
; 115200 baud, 8n2
;
; feilipu, August 2020
;
;==============================================================================
;
; The updates to the original BASIC within this file are copyright Grant Searle
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.wales/
;
;==============================================================================
;
; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted for the freeware Zilog Macro Assembler 2.10 to produce
; the original ROM code (checksum A934H). PA
;
;==============================================================================
;
; INCLUDES SECTION
;

INCLUDE "board.inc"

DEFC SIOA_D = $80
DEFC SIOA_C = $82
DEFC SIOB_D = $81
DEFC SIOB_C = $83
DEFC R5_RTS_HIGH = $E8
DEFC R5_RTS_LOW = $EA
DEFC R1_RX_ALL_NOTX = $18               ; RX Interrupt on all receive chart
DEFC R1_RX_ALL_TX = $1A                 ; RX Interrupt on all receive chars, and tx int

;==============================================================================
;
; CODE SECTION
;

;------------------------------------------------------------------------------
SECTION acia_interrupt              ; ORG $0070

.acia_int
        push af
        push hl

        sub     a                   ; XXX smbaker
        out     (SIOA_C),a          ; XXX smbaker    
        in      a, (SIOA_C)         ; XXX smbaker
        rrca                        ; check whether a byte has been received, via SER_RDRF
        jr NC,acia_tx_send          ; if not, go check for bytes to transmit

.acia_rx_get
        in a,(SIOA_D)               ; Get the received byte from the ACIA  XXX smbaker
        ld l,a                      ; Move Rx byte to l

        ld a,(serRxBufUsed)         ; Get the number of bytes in the Rx buffer
        cp SER_RX_BUFSIZE-1         ; check whether there is space in the buffer
        jr NC,acia_tx_check         ; buffer full, check if we can send something

        ld a,l                      ; get Rx byte from l
        ld hl,(serRxInPtr)          ; get the pointer to where we poke
        ld (hl),a                   ; write the Rx byte to the serRxInPtr address
        inc l                       ; move the Rx pointer low byte along, 0xFF rollover
        ld (serRxInPtr),hl          ; write where the next byte should be poked

        ld hl,serRxBufUsed
        inc (hl)                    ; atomically increment Rx buffer count

        ld a,(serRxBufUsed)         ; get the current Rx count
        cp SER_RX_FULLSIZE          ; compare the count with the preferred full size
        jp NZ,acia_tx_check         ; leave the RTS low, and check for Rx/Tx possibility

        ; set rts high
        LD       A, $05             ; XXX smbaker
        OUT      (SIOA_C),A         ; XXX smbaker
        LD       A,R5_RTS_HIGH      ; XXX smbaker - set RTS high
        OUT      (SIOA_C),A         ; XXX smbaker

        ; turn off tx interrupts       XXX smbaker defer this for now
        ;LD       A, $01
        ;OUT      (SIOA_C),A         ; XXX smbaker
        ;LD       A,R1_RX_ALL_NOTX   ; XXX smbaker - disable tx interrupt
        ;OUT      (SIOA_C),A         ; XXX smbaker

.acia_tx_check
        sub     a                   ; XXX smbaker
        out     (SIOA_C),a          ; XXX smbaker    
        in      a, (SIOA_C)         ; XXX smbaker
        rrca                        ; check whether a byte has been received, via SER_RDRF
        jr C,acia_rx_get            ; another byte received, go get it

.acia_tx_send
        rrca                        ; check whether a byte can be transmitted, via SER_TDRE
        jr NC,acia_txa_end          ; if not, we're done for now

        ld a,(serTxBufUsed)         ; get the number of bytes in the Tx buffer
        or a                        ; check whether it is zero
        jp Z,acia_tei_clear         ; if the count is zero, then disable the Tx Interrupt

        ld hl,(serTxOutPtr)         ; get the pointer to place where we pop the Tx byte
        ld a,(hl)                   ; get the Tx byte
        OUT (SIOA_D),A              ; Output the character   XXX smbaker

        inc l                       ; move the Tx pointer, just low byte, along
        ld a,SER_TX_BUFSIZE-1       ; load the buffer size, (n^2)-1
        and l                       ; range check
        or serTxBuf&0xFF            ; locate base
        ld l,a                      ; return the low byte to l
        ld (serTxOutPtr),hl         ; write where the next byte should be popped

        ld hl,serTxBufUsed
        dec (hl)                    ; atomically decrement current Tx count

        jr NZ,acia_txa_end          ; if we've more Tx bytes to send, we're done for now

.acia_tei_clear
        ; turn off tx interrupts        XXX smbaker defer this for now
        ;LD       A, $01
        ;OUT      (SIOA_C),A         ; XXX smbaker
        ;LD       A,R1_RX_ALL_NOTX   ; XXX smbaker - disable tx interrupt
        ;OUT      (SIOA_C),A         ; XXX smbaker

.acia_txa_end
        pop hl
        pop af

        ei
        reti

;------------------------------------------------------------------------------
; SECTION acia_rxa_chk              ; ORG $00D8
;
; .RXA_CHK                          ; insert directly into JumP table
;       ld a,(serRxBufUsed)
;       ret

;------------------------------------------------------------------------------
SECTION acia_rxa                    ; ORG $00D8

.RXA
        ld a,(serRxBufUsed)         ; get the number of bytes in the Rx buffer
        or a                        ; see if there are zero bytes available
        jr Z,RXA                    ; wait, if there are no bytes available

        cp SER_RX_EMPTYSIZE         ; compare the count with the preferred empty size
        jp NZ,rxa_get_byte          ; if the buffer is too full, don't change the RTS

        ; set rts low
        LD       A,$05              ; XXX SMBAKER
        OUT      (SIOA_C),A         ; XXX SMBAKER - set rts low 
        LD       A,R5_RTS_LOW       ; XXX SMBAKER
        OUT      (SIOA_C),A         ; XXX SMBAKER 

.rxa_get_byte
        push hl                     ; store HL so we don't clobber it

        ld hl,(serRxOutPtr)         ; get the pointer to place where we pop the Rx byte
        ld a,(hl)                   ; get the Rx byte
        inc l                       ; move the Rx pointer low byte along
        ld (serRxOutPtr),hl         ; write where the next byte should be popped

        ld hl,serRxBufUsed
        dec (hl)                    ; atomically decrement Rx count

        pop hl                      ; recover HL
        ret                         ; char ready in A

;------------------------------------------------------------------------------
SECTION acia_txa                    ; ORG $0100

.TXA                                ; output a character in A via ACIA
                PUSH     AF              ; Store character
conout1:        SUB      A
                OUT      (SIOA_C),A
                IN       A,(SIOA_C)
                RRCA
                BIT      1,A             ; Set Zero flag if still transmitting character
                JR       Z,conout1       ; Loop until flag signals ready
                POP      AF              ; Retrieve character
                OUT      (SIOA_D),A      ; Output the character
                RET

;------------------------------------------------------------------------------
SECTION init                        ; ORG $0148

PUBLIC  INIT

.MEM_ERR
        LD L,A                      ; preserve the error byte
        DEFB 01H                    ; skip "LD L,BEL"
.INIT
        LD L,BEL                    ; prepare a BEL, to indicate normal boot

        ; XXX smbaker - SIO initialization
        ; XXX smbaker - int32k did this with receiver disabled

        LD      A,$00            ; write 0
        OUT     (SIOA_C),A
        LD      A,$18            ; reset ext/status interrupts
        OUT     (SIOA_C),A

        LD      A,$04            ; write 4
        OUT     (SIOA_C),A
        LD      A,$C4            ; X64, no parity, 1 stop
        OUT     (SIOA_C),A

        LD      A,$01            ; write 1
        OUT     (SIOA_C),A
        LD      A,R1_RX_ALL_NOTX ; interrupt on all recv
        OUT     (SIOA_C),A

        LD      A,$03            ; write 3
        OUT     (SIOA_C),A
        LD      A,$E1            ; 8 bits, auto enable, rcv enab
        OUT     (SIOA_C),A

        LD      A,$05            ; write 5
        OUT     (SIOA_C),A
        LD      A,R5_RTS_LOW     ; dtr enable, 8 bits, tx enable, rts
        OUT     (SIOA_C),A


        LD A,L                      ; get byte to transmit
        OUT (SIOA_D),A              ; send it

        LD A,(basicStarted)         ; save BASIC STARTED flag

        LD HL,RAMSTART              ; do a short memory sanity check
        LD (HL),0FFH                ; set all bits
        LD DE,RAMSTART+1
        LD BC,WRKSPC-RAMSTART-1
        LDIR                        ; set all bytes
        DEC HL                      ; revert final increment
        DEC DE
        EX DE,HL                    ; swap load direction
        INC (HL)                    ; increment to 0, check all bits of all bytes were set
        JR NZ,MEM_ERR               ; if not output errored byte and do it again
        LD BC,WRKSPC-RAMSTART-1
        LDDR                        ; clear all bits of all bytes

        LD (basicStarted),A         ; restore BASIC STARTED flag

        LD A,(HL)
        OR A                        ; check if all bits of all bytes were cleared
        JR NZ,MEM_ERR               ; if not output errored byte and do it again

        LD SP,TEMPSTACK             ; set up a temporary stack

        LD HL,VECTOR_PROTO          ; establish Z80 RST Vector Table
        LD DE,VECTOR_BASE
        LD BC,VECTOR_SIZE
        LDIR

        LD HL,serRxBuf              ; initialise Rx Buffer
        LD (serRxInPtr),HL
        LD (serRxOutPtr),HL

        LD HL,serTxBuf              ; initialise Tx Buffer
        LD (serTxInPtr),HL
        LD (serTxOutPtr),HL

        XOR A                       ; zero the RXA & TXA Buffer Counts
        LD (serRxBufUsed),A
        LD (serTxBufUsed),A

        ; XXX SMBAKER - int32k enabled the receiver here

        IM 1                        ; interrupt mode 1
        EI                          ; enable interrupts

.START
        LD HL,SIGNON1               ; sign-on message
        CALL PRINT                  ; output string

;        XXX smbaker deleted warm-start option to make space

.COLDSTART
        LD A,'Y'                    ; set the BASIC STARTED flag
        LD (basicStarted),A
        JP $0240                    ; <<<< Start Basic COLD

.PRINT
        LD A,(HL)                   ; get character
        OR A                        ; is it $00 ?
        RET Z                       ; then RETurn on terminator
        CALL TXA                    ; output character in A
        INC HL                      ; next Character
        JP PRINT                    ; continue until $00

;==============================================================================
;
; STRINGS
;
SECTION init_strings                ; ORG $01F0

.SIGNON1
        DEFM    CR,LF
        DEFM    "SIO/2 Driver - smbaker",CR,LF
        DEFM    "RC2014 - MS Basic Loader",CR,LF
        DEFM    "z88dk - feilipu",CR,LF,0

;==============================================================================
;
; Z80 INTERRUPT VECTOR PROTOTYPE ASSIGNMENTS
;

EXTERN  NULL_NMI                            ; RETN
EXTERN  UFERR                               ; User Function undefined (RSTnn) error

PUBLIC  RST_00, RST_08, RST_10; RST_18
PUBLIC  RST_20, RST_28, RST_30

PUBLIC  INT_INT, INT_NMI

DEFC    RST_00      =       INIT            ; Initialise, should never get here
DEFC    RST_08      =       TXA             ; TX character, loop until space
DEFC    RST_10      =       RXA             ; RX character, loop until byte
;       RST_18      =       RXA_CHK         ; Check receive buffer status, return # bytes available
DEFC    RST_20      =       UFERR           ; User Function undefined (RST20)
DEFC    RST_28      =       UFERR           ; User Function undefined (RST28)
DEFC    RST_30      =       UFERR           ; User Function undefined (RST30)
DEFC    INT_INT     =       acia_int        ; ACIA interrupt
DEFC    INT_NMI     =       NULL_NMI        ; RETN

;==============================================================================
