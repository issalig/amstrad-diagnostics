

 IFDEF UpperROMBuild
UpperROMConfig: db 0				; Here we store the upper ROM we were launched from
 ENDIF

;; System info
ModelType: db 0
KeyboardLanguage: db 0
FDCPresent: db 0
VendorName: db 0
RefreshFrequency: db 0

Z80Type: db 0
Z80Flavor: db 0

;; Z80 IXF test
Z80IxfResults: ds 6


;; Upper RAM test
ValidBankCount: db 0
FailingBits: db 0
C3ConfigFailed: db 0
LastReadBank: db #FF, #FF

;; ROM test
ROMStringBuffer: ds 16

;; Soak test
SoakTestIndicator: ds 4				; Save 4 bytes
SoakTestCount: db 0

;; Sound test
MixerDataPtr: dw 0
SoundTestY: db 0

;; Keyboard test
KeyboardLayout: db 0				; 0: 6128, 1: 464
FramesESCPressed: db 0
FillKeyPattern: db 0
KeyboardLocationTable: dw 0
SpecialKeysTable: dw 0
KeyboardLabels: ds KeyboardLabelsTableSize	; Copy of the labels, patched up with correct language variations
;;KeyboardResult: ds

;; Keyboard
;; This buffer has one byte per keyboard line.
;; Each byte defines a single keyboard line, which defines
;; the state for up to 8 keys.
;;
;; A bit in a byte will be '1' if the corresponding key
;; is pressed, '0' if the key is not pressed.

KeyboardBufferSize equ 10
KeyboardMatrixBuffer: 	     defs KeyboardBufferSize
LastKeyboardMatrixBuffer:    defs KeyboardBufferSize
EdgeOnKeyboardMatrixBuffer:  defs KeyboardBufferSize
EdgeOffKeyboardMatrixBuffer: defs KeyboardBufferSize
PresseddMatrixBuffer: 	     defs KeyboardBufferSize
PressedRowsMask: db 0
PressedColsMask: dw 0

;; Disk test
;Drive:  db 0                     ; Current drive number (0 or 1)
;Track:  db 0                     ; Track number (0 for RPM measurement)
RpmInt: dw 0                     ; RPM integer value
TimeCount:   ds 2                     ; 16-bit timer counter storage
; TRCmd: db #09,#46,#00,#00,#00,#00,#00,#00,#2A,#FF ; PD765 FDC command: Read ID with MFM mode
; Result timing counter
ResTime:    DEFS    2
; Buffer for FDC result bytes
ResBuf:     DEFS    20

; division counter
clcd32c:  db 0


;; Print char
@TxtCoords:
@txt_y: defb 0
@txt_x: defb 0
@txt_pixels_y: defb 0
@txt_byte_x: defb 0
@txt_right: db 0			; 0 = draw left char at that byte, 1 = draw right char

@bk_color: db 0
@fg_color: db 0

 DISPLAY $
@scr_table: defs 200*2
 DISPLAY $
char_depack_buffer: defs 16
