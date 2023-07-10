;==============================================================================
; Contents of this file are copyright Phillip Stevens
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; https://github.com/feilipu/
;
; https://feilipu.me/
;
;==============================================================================
;
; ACIA 68B50 interrupt driven serial I/O to run modified NASCOM Basic 4.7.
; Full input and output buffering with incoming data hardware handshaking.
; Handshake shows full before the buffer is totally filled to
; allow run-on from the sender. Transmit and receive are interrupt driven.
;
; https://github.com/feilipu/
; https://feilipu.me/
;
;==============================================================================
;
; HexLoadr option by @feilipu,
; derived from the work of @fbergama and @foxweb at RC2014
; https://github.com/RC2014Z80
;

;==============================================================================
;
; INCLUDES SECTION
;

INCLUDE "board_config.inc"

;==============================================================================
;
; DEFINES SECTION
;

DEFC    ROMSTART        =   $0000   ; Bottom of ROM

DEFC    RAMSTOP         =   $FFFF   ; Top of RAM

DEFC    SER_RX_BUFSIZE  =   $100    ; FIXED Rx buffer size, 256 Bytes, no range checking
DEFC    SER_RX_FULLSIZE =   SER_RX_BUFSIZE - $10
                                    ; Fullness of the Rx Buffer, when not_RTS is signalled
DEFC    SER_RX_EMPTYSIZE =  $08     ; Fullness of the Rx Buffer, when RTS is signalled

DEFC    SER_TX_BUFSIZE  =   $40     ; Size of the Tx Buffer, 2^n Bytes, n = 6 here

;==============================================================================
;
; Interrupt vectors (offsets) for Z80 RST, INT0, and NMI interrupts
;

DEFC    VECTOR_BASE         =   RAMSTART    ; RAM vector address for Z80 RST Table

; Squeezed between INT0 0x0038 and NMI 0x0066

DEFC    VECTOR_PROTO        =   $003C
DEFC    VECTOR_SIZE         =   $24

;   Prototype Interrupt Service Routines - complete in main program
;
;DEFC       RST_00          =    INIT        Reboot
;DEFC       RST_08          =    TXA         TX a character over ACIA
;DEFC       RST_10          =    RXA         RX a character over ACIA, block no bytes available
;DEFC       RST_18          =    RXA_CHK     Check ACIA status, return # bytes available
;DEFC       RST_20          =    UFERR       User Function undefined (RSTnn) error
;DEFC       RST_28          =    UFERR       User Function undefined (RSTnn) error
;DEFC       RST_30          =    UFERR       User Function undefined (RSTnn) error
;DEFC       INT_INT         =    NULL_INT
;DEFC       INT_NMI         =    NULL_NMI

;   Z80 Interrupt Service Routine Addresses - rewrite as needed

DEFC    RST_00_ADDR         =   VECTOR_BASE+$01
DEFC    RST_08_ADDR         =   VECTOR_BASE+$05
DEFC    RST_10_ADDR         =   VECTOR_BASE+$09
DEFC    RST_18_ADDR         =   VECTOR_BASE+$0D
DEFC    RST_20_ADDR         =   VECTOR_BASE+$11
DEFC    RST_28_ADDR         =   VECTOR_BASE+$15
DEFC    RST_30_ADDR         =   VECTOR_BASE+$19
DEFC    INT_INT0_ADDR       =   VECTOR_BASE+$1D
DEFC    INT_NMI_ADDR        =   VECTOR_BASE+$21

;==============================================================================
;
; Some definitions used with the RC2014 on-board peripherals:
;

; General TTY

DEFC    CTRLC           =    03H     ; Control "C"
DEFC    CTRLG           =    07H     ; Control "G"
DEFC    BEL             =    07H     ; Bell
DEFC    BKSP            =    08H     ; Back space
DEFC    LF              =    0AH     ; Line feed
DEFC    CS              =    0CH     ; Clear screen
DEFC    CR              =    0DH     ; Carriage return
DEFC    CTRLO           =    0FH     ; Control "O"
DEFC    CTRLQ           =    11H     ; Control "Q"
DEFC    CTRLR           =    12H     ; Control "R"
DEFC    CTRLS           =    13H     ; Control "S"
DEFC    CTRLU           =    15H     ; Control "U"
DEFC    ESC             =    1BH     ; Escape
DEFC    DEL             =    7FH     ; Delete

;==============================================================================
;
; VARIABLES
;

; Port0
DEFC    serRxInPtr      =     VECTOR_BASE+VECTOR_SIZE
DEFC    serRxOutPtr     =     serRxInPtr+2
DEFC    serTxInPtr      =     serRxOutPtr+2
DEFC    serTxOutPtr     =     serTxInPtr+2
DEFC    serRxBufUsed    =     serTxOutPtr+2
DEFC    serTxBufUsed    =     serRxBufUsed+1
DEFC    serControl      =     serTxBufUsed+1

; Port1
DEFC    ser1RxInPtr      =     serControl+1
DEFC    ser1RxOutPtr     =     ser1RxInPtr+2
DEFC    ser1TxInPtr      =     ser1RxOutPtr+2
DEFC    ser1TxOutPtr     =     ser1TxInPtr+2
DEFC    ser1RxBufUsed    =     ser1TxOutPtr+2
DEFC    ser1TxBufUsed    =     ser1RxBufUsed+1
DEFC    ser1Control      =     ser1TxBufUsed+1

; For two-port drivers, which port to use
DEFC    serPort          =     ser1Control+1

DEFC    basicStarted    =      serPort+1

; I/O Tx Buffer must start on 0xnnn0 because we increment low byte to roll-over
DEFC    BUFSTART_TX_IO  =     VECTOR_BASE-(VECTOR_BASE%$100) + $80

; ... for port1
DEFC    BUFSTART1_TX_IO  =     VECTOR_BASE-(VECTOR_BASE%$100) + $C0

; I/O Rx Buffer must start on 0xnn00 because we increment low byte to roll-over
DEFC    BUFSTART_RX_IO  =     VECTOR_BASE-(VECTOR_BASE%$100) + $100

; ... for port1
DEFC    BUFSTART1_RX_IO  =     VECTOR_BASE-(VECTOR_BASE%$100) + $200

DEFC    serTxBuf        =     BUFSTART_TX_IO
DEFC    serRxBuf        =     BUFSTART_RX_IO

DEFC    ser1TxBuf       =     BUFSTART1_TX_IO
DEFC    ser1RxBuf       =     BUFSTART1_RX_IO

;==============================================================================

