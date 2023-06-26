INCLUDE "board.inc"

DEFC SCCA_D = $83
DEFC SCCA_C = $81
DEFC SCCB_D = $82
DEFC SCCB_C = $80
DEFC R5_RTS_HIGH = $E8
DEFC R5_RTS_LOW = $EA
DEFC R1_RX_ALL_NOTX = $10               ; RX Interrupt on all receive chart
DEFC R1_RX_ALL_TX = $12                 ; RX Interrupt on all receive chars, and tx int

;==============================================================================
;
; CODE SECTION
;

;------------------------------------------------------------------------------
SECTION acia_interrupt              ; ORG $0070

.scc_int
        push af
        push hl

        LD      A, $02              ; XXX smbaker - software intack cycle
        OUT     (SCCA_C),A
        IN      A, (SCCA_C)

        in      a, (SCCA_C)         ; XXX smbaker - skipped pointer reg here
        rrca                        ; check whether a byte has been received, via SER_RDRF
        jr NC,scc_tx_send          ; if not, go check for bytes to transmit

.scc_rx_get
        in a,(SCCA_D)               ; Get the received byte from the ACIA  XXX smbaker
        ld l,a                      ; Move Rx byte to l

        ld a,(serRxBufUsed)         ; Get the number of bytes in the Rx buffer
        cp SER_RX_BUFSIZE-1         ; check whether there is space in the buffer
        jr NC,scc_tx_check         ; buffer full, check if we can send something

        ld a,l                      ; get Rx byte from l
        ld hl,(serRxInPtr)          ; get the pointer to where we poke
        ld (hl),a                   ; write the Rx byte to the serRxInPtr address
        inc l                       ; move the Rx pointer low byte along, 0xFF rollover
        ld (serRxInPtr),hl          ; write where the next byte should be poked

        ld hl,serRxBufUsed
        inc (hl)                    ; atomically increment Rx buffer count

        ld a,(serRxBufUsed)         ; get the current Rx count
        cp SER_RX_FULLSIZE          ; compare the count with the preferred full size
        jp NZ,scc_tx_check         ; leave the RTS low, and check for Rx/Tx possibility

        ; set rts high
        LD       A, $05             ; XXX smbaker
        OUT      (SCCA_C),A         ; XXX smbaker
        LD       A,R5_RTS_HIGH      ; XXX smbaker - set RTS high
        OUT      (SCCA_C),A         ; XXX smbaker

        ; turn off tx interrupts       XXX smbaker defer this for now
        ;LD       A, $01
        ;OUT      (SCCA_C),A         ; XXX smbaker
        ;LD       A,R1_RX_ALL_NOTX   ; XXX smbaker - disable tx interrupt
        ;OUT      (SCCA_C),A         ; XXX smbaker

.scc_tx_check
        in      a, (SCCA_C)         ; XXX smbaker
        rrca                        ; check whether a byte has been received, via SER_RDRF
        jr C,scc_rx_get            ; another byte received, go get it

.scc_tx_send
        rrca                        ; check whether a byte can be transmitted, via SER_TDRE
        jr NC,scc_txa_end          ; if not, we're done for now

        ld a,(serTxBufUsed)         ; get the number of bytes in the Tx buffer
        or a                        ; check whether it is zero
        jp Z,scc_tei_clear         ; if the count is zero, then disable the Tx Interrupt

        ld hl,(serTxOutPtr)         ; get the pointer to place where we pop the Tx byte
        ld a,(hl)                   ; get the Tx byte
        OUT (SCCA_D),A              ; Output the character   XXX smbaker

        inc l                       ; move the Tx pointer, just low byte, along
        ld a,SER_TX_BUFSIZE-1       ; load the buffer size, (n^2)-1
        and l                       ; range check
        or serTxBuf&0xFF            ; locate base
        ld l,a                      ; return the low byte to l
        ld (serTxOutPtr),hl         ; write where the next byte should be popped

        ld hl,serTxBufUsed
        dec (hl)                    ; atomically decrement current Tx count

        jr NZ,scc_txa_end          ; if we've more Tx bytes to send, we're done for now

.scc_tei_clear
        ; turn off tx interrupts        XXX smbaker defer this for now
        ;LD       A, $01
        ;OUT      (SCCA_C),A         ; XXX smbaker
        ;LD       A,R1_RX_ALL_NOTX   ; XXX smbaker - disable tx interrupt
        ;OUT      (SCCA_C),A         ; XXX smbaker

.scc_txa_end
        LD      A,$38                ; XXX smbaker - reset IUS. skipped pointer reg here.
        OUT     (SCCA_C),A

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
        OUT      (SCCA_C),A         ; XXX SMBAKER - set rts low 
        LD       A,R5_RTS_LOW       ; XXX SMBAKER
        OUT      (SCCA_C),A         ; XXX SMBAKER 

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
        PUSH     AF                     ; Store character
conout1:        
        IN      A,(SCCA_C)
        BIT     2,A
        JR      Z,conout1               ; loop until tx flag is clear
        POP     AF
        OUT     (SCCA_D),A
        RET

;------------------------------------------------------------------------------
SECTION init                        ; ORG $0148

PUBLIC  INIT

.MEM_ERR
        LD D,A                      ; preserve the error byte
        DEFB 01H                    ; skip "LD L,BEL"
.INIT
        LD D,BEL                    ; prepare a BEL, to indicate normal boot

        IN      A,(SCCA_C)          ; make sure pointer is reset
        IN      A,(SCCA_C)

        LD      HL,SCCCMDS          ; Write all characters
        LD      B,SCCCMDE-SCCCMDS
        LD      C,SCCA_C
        OTIR

        LD A,D                      ; get byte to transmit
        OUT (SCCA_D),A              ; send it

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
;        XXX smbaker removed signon and relocated to 'ABOUT' command

;        XXX smbaker deleted warm-start option to make space

.COLDSTART
        LD A,'Y'                    ; set the BASIC STARTED flag
        LD (basicStarted),A
        JP $0240                    ; <<<< Start Basic COLD

.BAUD   PUSH    BC
        PUSH    HL
        LXI     HL,BAUDTABLE
        LD      B,0
        LD      C,A
        ADD     HL,BC
        LD      A,(HL)
        CMP     A,0FFH          ; FF means not supported
        JZ      BAUDOUT         ; Uh oh
        LD      A,12
        OUT     (SCCA_C),A
        LD      A,(HL)
        OUT     (SCCA_C),A
.BAUDOUT:
        POP     HL
        POP     BC
        RET

.SCCCMDS:
        DEFB    9, 0xc0         ; Reset
        DEFB    3, 0xc0         ; Receiver disable
        DEFB    5, 0xe2         ; Transmiter disable
        DEFB    4, 0x44         ; x16, 1stop-bit, non-parity
        DEFB    3, 0xe0         ; Receive  8bit/char 
        DEFB    5, 0xe2         ; Send 8bit/char dtr rts
        DEFB    9, 0x28         ; Software intack enable, Master int enable XXX smbaker
        DEFB    11, 0x50        ; BG use for receiver and transmiter
        DEFB    12, SCC_BRG_9600_73728MHz     ; see ../common/board.s
        DEFB    13, 00
        DEFB    14, SCC_CLK_CPU     ; see ../common/board.s
        DEFB    14, (SCC_CLK_CPU | 1)    ; BG enable
        DEFB    3, 0xe1         ; Receiver enable
        DEFB    5, 0xea         ; Transmiter enable
        DEFB    1, R1_RX_ALL_NOTX ; XXX smbaker - enable rx but not tx interrupt
.SCCCMDE:

.BAUDTABLE:
        DEFB    $FF                     ; 300 baud is not supported
        DEFB    SCC_BRG_1200_73728MHz
        DEFB    SCC_BRG_2400_73728MHz
        DEFB    SCC_BRG_4800_73728MHz
        DEFB    SCC_BRG_9600_73728MHz
        DEFB    SCC_BRG_19200_73728MHz
        DEFB    SCC_BRG_38400_73728MHz
        DEFB    SCC_BRG_57600_73728MHz
        DEFB    SCC_BRG_115200_73728MHz

DEFC SCC_BRG_1200_73728MHz = 190        ; 1200 baud using 7.3727Mhz crystal
DEFC SCC_BRG_2400_73728MHz = 94         ; 2400 baud using 7.3727Mhz crystal
DEFC SCC_BRG_4800_73728MHz = 46         ; 4800 baud using 7.3727Mhz crystal
DEFC SCC_BRG_9600_73728MHz = 22         ; 9600 baud using 7.3727Mhz crystal
DEFC SCC_BRG_19200_73728MHz = 10        ; 19200 baud using 7.3727Mhz crystal
DEFC SCC_BRG_38400_73728MHz = 4         ; 38400 baud using 7.3727Mhz crystal
DEFC SCC_BRG_57600_73728MHz = 2         ; 57600 baud using 7.3727Mhz crystal
DEFC SCC_BRG_115200_73728MHz = 0         ; 115200 baud using 7.3727Mhz crystal. Not sure if works.

DEFC SCC_CLK_CPU = 2
DEFC SCC_CLK_SEPARATE = 0

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
DEFC    RST_20      =       BAUD            ; Set Baud (RST20)
DEFC    RST_28      =       UFERR           ; User Function undefined (RST28)
DEFC    RST_30      =       UFERR           ; User Function undefined (RST30)
DEFC    INT_INT     =       scc_int        ; ACIA interrupt
DEFC    INT_NMI     =       NULL_NMI        ; RETN

;==============================================================================
