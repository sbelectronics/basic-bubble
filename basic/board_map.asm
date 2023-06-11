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

INCLUDE     "board.inc"

SECTION     vector_rst
ORG         $0000

SECTION     vector_table_prototype
ORG         VECTOR_PROTO

SECTION     vector_null_ret
ORG         VECTOR_PROTO+VECTOR_SIZE

SECTION     vector_nmi
ORG         $0066

SECTION     acia_interrupt
ORG         $0070

SECTION     acia_rxa
ORG         $00D8

SECTION     acia_txa
ORG         $0100

SECTION     init
ORG         $0148

SECTION     init_strings
ORG         $01F0

SECTION     bubble_lib
ORG         $2400

SECTION     bubble_vars
ORG         $8200

;==============================================================================
