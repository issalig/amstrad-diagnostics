
 MODULE KEYBOARDTEST


@TestKeyboard:
	call 	KeyboardSetUpScreen
.afterScreenDraw:
	call	SetKeyboardTables
	call 	PrintKeyboard
	ld 	hl, LastKeyboardMatrixBuffer
	call 	ClearKeyboardBuffer
	ld 	hl, PresseddMatrixBuffer
	call 	ClearKeyboardBuffer
	ld	a, 0
	ld	(FramesESCPressed), a

.keyboardLoop:
	call 	WaitForVsync
	call 	ReadFullKeyboard
	call 	UpdateKeyBuffers
	;FIX !!!! call PrintMatrixLabels
	call	CheckESCPressedLongEnough
	jr	nz, .continue

	;; Something was pressed long enough
	ld 	a, (KeyboardMatrixBuffer+8)
	bit 	4, a					; TAB
	jp	z, ExitTest

	;; Toggle the keyboard layout
	ld	a, (KeyboardLayout)
	inc	a
	cp	KEYBOARD_LAYOUT_MATRIX+1
	call	z, .clearA
	ld	(KeyboardLayout), a
	call	ClearESCBar
	call	ClearKeyboardArea
	jr	.afterScreenDraw

.continue:
	call ShowRolsColsMask			; Show pressed rows and columns

	; Print edge on keys in blue
	ld	a, PATTERN_BLUE
	ld	(FillKeyPattern), a
	ld 	h, pen2
	ld 	l, pen0
	call 	SetTxtColors	
	ld 	hl, EdgeOnKeyboardMatrixBuffer
	call 	PrintOnKeysFromBuffer

	; Print edge off keys in yellow
	ld	a, PATTERN_YELLOW
	ld	(FillKeyPattern), a
	ld 	h, pen1
	ld 	l, pen0
	call 	SetTxtColors	
	ld 	hl, EdgeOffKeyboardMatrixBuffer
	call 	PrintOnKeysFromBuffer

	jp .keyboardLoop

.clearA:
	ld	a, 0
	ret


ExitTest:
	call	CountPressedKeys
	cp	KEYBOARD_KEY_COUNT
	jr	nz, .keyTestFailed

	ld	a, TESTRESULT_PASSED
	ld	(TestResultTableKeyboard), a

.continueExit:
	call	CountPressedJoystickButtons
	cp	JOYSTICK_KEY_COUNT
	jr	nz, .joyTestFailed

	ld	a, TESTRESULT_PASSED
	ld	(TestResultTableJoystick), a

	ret

.keyTestFailed:
	ld	a, TESTRESULT_FAILED
	ld	(TestResultTableKeyboard), a
	jr	.continueExit

.joyTestFailed:
	ld	a, TESTRESULT_FAILED
	ld	(TestResultTableJoystick), a
	ret


;; OUT: A - number of different pressed keys
@CountPressedKeys:
	push	bc
	push	de
	ld	d, 0
	ld 	hl, PresseddMatrixBuffer
	ld 	c, KeyboardBufferSize
	ld	e, #FF			;; Mask for bits
.byteLoop:
	ld 	a, (hl)
	and	e			;; Mask out all bits from joystick
	ld	b, 8
.bitLoop:
	bit	0, a
	jr	z, .nextBit
	inc	d
.nextBit:
	srl	a
	djnz	.bitLoop

	inc	hl
	dec	c
	ld	a, c
	cp	1
	jr	nz, .notLastByte
	ld	e, %10000000		;; Set mask for last byte to ignore all joystick presses
.notLastByte:
	or	a
	jr	nz, .byteLoop

	ld	a, d
	pop	de
	pop	bc
	ret


;; OUT: A - number of different pressed joystick buttons
@CountPressedJoystickButtons:
	push	bc

	ld	d, 0
	ld 	hl, PresseddMatrixBuffer+9
	ld 	a, (hl)

	ld	b, 5
.bitLoop:
	bit	0, a
	jr	z, .nextBit
	inc	d
.nextBit:
	srl	a
	djnz	.bitLoop

	ld	a, d
	pop	bc
	ret

;; OUT: A - mask of all bits that were used in PresseddMatrixBuffer
@GetPressedBitsMask:
    push bc
    push hl
    
    ld hl, PresseddMatrixBuffer
    ld a, (hl)              ; Get first byte
    ld b, KeyboardBufferSize
    dec b                   ; Already got first byte
.byteLoop:
    inc hl
    or (hl)                ; Combine with next byte
    djnz .byteLoop         ; Decrement B and jump if not zero
    
    pop hl
    pop bc
    ret

;; This function returns a 8bit mask with the status of 8 bytes in a buffer
;; In the mask, each bit corresponds to one byte in the array
;; A bit is set (1) if the corresponding byte had any key pressed (non-zero)
;; For example: if bit 0 is set, row 0 had at least one key pressed == 0
;;              if bit 7 is set, row 7 had at least one key pressed  > 0
;; Input: HL - address of the buffer
;; Output: A - mask

@GetPressedColsMask:
	push hl              ; Save HL register
    push bc              ; Save BC register
    push de              ; Save DE register
    
    xor a                ; Clear A (our result mask)
    ld b, 8              ; Process 8 rows
    ld c, 1              ; Initialize mask to bit 0 (00000001)
    
.rowLoop:
    ld d, a              ; Save current result  	

    ld a, (hl)           ; Get byte for current row
    inc hl               ; Move to next row
    
    ; Check if any bits are set in this row
    or a                 ; Test if row has any keys pressed
    jr z, .noKeysPressed        ; Skip if no keys pressed 
    
    ; This row has keys pressed, set the corresponding bit
    ld a, d              ; Restore result
    or c                 ; Combine with current mask
    jr .shiftMask    ; Skip the restore below
    
.noKeysPressed:
    ld a, d              ; Restore result even when no keys pressed
    
.shiftMask:
    sla c                ; Shift mask left (multiply by 2)
    djnz .rowLoop        ; Repeat for all rows
    
    pop de               ; Restore DE
    pop bc               ; Restore BC
	pop hl               ; Restore HL
    ret                  ; Return with mask in A

; functions prints the values in keyboard matrix buffer
; Input: HL - buffer
; Modifies DE, BC
@PrintMatrixBuffer:
	ld 	b, KeyboardBufferSize
	ld 	c, 0
.loop:
 	;update 	(TxtCoords) +1 
	;ld de, (TxtCoords)
	;inc de
	;ld (TxtCoords), de

	ld 	a, (hl)
	push 	bc
	call 	PrintABin
	pop 	bc
	inc 	hl
	
	djnz 	.loop
	ret

; @Matrix2Cable
; Reorders bits from HL and C according to the keyboard cable mapping
; input HL 0,1,2,3,4,5,6,7,8,9 bits map to 18,17,16,15,14,13,12,11,10,1
; input C 0,1,2,3,4,5,6,7 bits map to 2,3,4,5,6,7,8,9
; input C bit 7 is also map to 0 (see schematics)
; IN: HL - row bits 0-9
; IN: C - column bits 0-7
; OUT: HL - cable reordered bits 0-15
; OUT: C - cable reordered bits 16-18
; @Matrix2Cable:
;     push de
;     push af
;     push bc      ; Save original C
    
;     ; Save original HL values for reference
;     ld d, h      ; D = original H
;     ld e, l      ; E = original L
    
;     ; Initialize result registers
;     ld hl, 0     ; Clear output HL (bits 0-15)
    
;     ; --- BIT 0 (from C bit 7) ---
;     bit 7, c     ; Test bit 7 of C
;     jr z, .skip0
;     set 0, l     ; Set bit 0 in result
; .skip0:
    
;     ; --- BIT 1 (from HL bit 9) ---
;     bit 1, d     ; Test bit 1 of D (bit 9 of original HL)
;     jr z, .skip1
;     set 1, l     ; Set bit 1 in result
; .skip1:
    
;     ; --- BIT 2 (from C bit 0) ---
;     bit 0, c
;     jr z, .skip2
;     set 2, l     ; Set bit 2 in result
; .skip2:
    
;     ; --- BIT 3 (from C bit 1) ---
;     bit 1, c
;     jr z, .skip3
;     set 3, l     ; Set bit 3 in result
; .skip3:
    
;     ; --- BIT 4 (from C bit 2) ---
;     bit 2, c
;     jr z, .skip4
;     set 4, l     ; Set bit 4 in result
; .skip4:
    
;     ; --- BIT 5 (from C bit 3) ---
;     bit 3, c
;     jr z, .skip5
;     set 5, l     ; Set bit 5 in result
; .skip5:
    
;     ; --- BIT 6 (from C bit 4) ---
;     bit 4, c
;     jr z, .skip6
;     set 6, l     ; Set bit 6 in result
; .skip6:
    
;     ; --- BIT 7 (from C bit 5) ---
;     bit 5, c
;     jr z, .skip7
;     set 7, l     ; Set bit 7 in result
; .skip7:
    
;     ; --- BIT 8 (from C bit 6) ---
;     bit 6, c
;     jr z, .skip8
;     set 0, h     ; Set bit 8 (bit 0 of H)
; .skip8:
    
;     ; --- BIT 9 (from C bit 7) ---
;     bit 7, c
;     jr z, .skip9
;     set 1, h     ; Set bit 9 (bit 1 of H)
; .skip9:
    
;     ; --- BIT 10 (from HL bit 8) ---
;     bit 0, d     ; Test bit 0 of D (bit 8 of original HL)
;     jr z, .skip10
;     set 2, h     ; Set bit 10 (bit 2 of H)
; .skip10:
    
;     ; --- BIT 11 (from HL bit 7) ---
;     bit 7, e     ; Test bit 7 of E (bit 7 of original HL)
;     jr z, .skip11
;     set 3, h     ; Set bit 11 (bit 3 of H)
; .skip11:
    
;     ; --- BIT 12 (from HL bit 6) ---
;     bit 6, e
;     jr z, .skip12
;     set 4, h     ; Set bit 12 (bit 4 of H)
; .skip12:
    
;     ; --- BIT 13 (from HL bit 5) ---
;     bit 5, e
;     jr z, .skip13
;     set 5, h     ; Set bit 13 (bit 5 of H)
; .skip13:
    
;     ; --- BIT 14 (from HL bit 4) ---
;     bit 4, e
;     jr z, .skip14
;     set 6, h     ; Set bit 14 (bit 6 of H)
; .skip14:
    
;     ; --- BIT 15 (from HL bit 3) ---
;     bit 3, e
;     jr z, .skip15
;     set 7, h     ; Set bit 15 (bit 7 of H)
; .skip15:
    
;     ; Initialize C for bits 16-18
;     ld b, 0      ; Clear B for result bits 16-18
    
;     ; --- BIT 16 (from HL bit 2) ---
;     bit 2, e
;     jr z, .skip16
;     set 0, b     ; Set bit 16 (bit 0 of result C)
; .skip16:
    
;     ; --- BIT 17 (from HL bit 1) ---
;     bit 1, e
;     jr z, .skip17
;     set 1, b     ; Set bit 17 (bit 1 of result C)
; .skip17:
    
;     ; --- BIT 18 (from HL bit 0) ---
;     bit 0, e     ; Test bit 0 of E (bit 0 of original HL)
;     jr z, .skip18
;     set 2, b     ; Set bit 18 (bit 2 of result C)
; .skip18:
    
;     ; Clean up and return
;     ld c, b      ; Move output bits 16-18 to C
    
;     pop bc       ; Restore original BC (but we'll overwrite C)
;     ld c, b      ; Put our computed value in C
;     pop af       ; Restore AF
;     pop de       ; Restore DE
;     ret

; ; Function to print row and column numbers for the matrix layout
; ; Call this from PrintKeyboard when KEYBOARD_LAYOUT_MATRIX is selected
; PrintMatrixLabels:
;      DEFINE KEYB_X 1
;      DEFINE KEYB_Y 5

; 	call 	SetDefaultColors
; 	ld	a, PATTERN_CLEAR
; 	ld	(FillKeyPattern), a

;      ; Print column numbers (0-7) at the top
;      ld h, KEYB_X       ; Start one character before the matrix
;      ld l, KEYB_Y       ; Two rows above the matrix
;      ld (TxtCoords), hl
    
;      ; Print a header for columns
;      ld hl, TxtRows
;      call PrintString
; 	 UNDEFINE KEYB_X
; 	 UNDEFINE KEYB_Y
; ret


; TO BE REMOVED
;     ; Redefine constants that are needed
;     DEFINE KEYB_X 21
;     DEFINE KEYB_Y 25
; 	DEFINE KEYB_ROW_1 KEYB_Y
; 	DEFINE KEYB_ROW_2 KEYB_ROW_1 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_3 KEYB_ROW_2 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_4 KEYB_ROW_3 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_5 KEYB_ROW_4 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_6 KEYB_ROW_5 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_7 KEYB_ROW_6 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_8 KEYB_ROW_7 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_9 KEYB_ROW_8 + KEYB_ROW_SPACING
; 	DEFINE KEYB_ROW_10 KEYB_ROW_9 + KEYB_ROW_SPACING
	
; 	push hl
; 	push bc

;     call SetDefaultColors
    
;     ; Print column numbers (0-7) at the top
;     ld h, KEYB_X-1       ; Start one character before the matrix
;     ld l, KEYB_Y-2       ; Two rows above the matrix
;     ld (TxtCoords), hl
    
;     ; Print a header for columns
;     ld hl, TxtCols
;     call PrintString
    
;     ; Print column numbers 0-7
;     ld b, 8              ; 8 columns
;     ld c, 0              ; Start with column 0
;     ld h, KEYB_X+1       ; X position for first column
; .colLoop:
;     ld l, KEYB_Y-2       ; Y position for column headers
;     ld (TxtCoords), hl
    
;     ld a, c              ; Get current column number
;     add a, '0'           ; Convert to ASCII
;     call PrintChar
    
;     ; Move to next column position
;     ld a, h
;     add a, KEYB_COL_SPACING
;     ld h, a
    
;     inc c                ; Next column number
;     djnz .colLoop        ; Loop until all columns printed
    
;     ; Print row numbers (0-9) on the left
;     ld b, 10             ; 10 rows (0-9)
;     ld c, 0              ; Start with row 0
; .rowLoop:
;     ; Calculate position for this row number
;     ld h, KEYB_X-2       ; Two positions to the left of the matrix
    
;     ; Use a lookup table for row Y positions
;     push bc
;     ld a, c              ; Get current row number
;     ld hl, RowYPositions
;     add a, l             ; Add offset to table address
;     ld l, a
;     jr nc, .noCarry
;     inc h
; .noCarry:
;     ld a, (hl)           ; Get Y position for this row
;     ld l, a
;     ld h, KEYB_X-2       ; X position for row labels
;     ld (TxtCoords), hl
    
;     pop bc
;     push bc
;     ld a, c              ; Get current row number
;     add a, '0'           ; Convert to ASCII
;     call PrintChar
    
;     pop bc
;     inc c                ; Next row
;     djnz .rowLoop        ; Loop until all rows printed
    
; 	pop hl
; 	pop bc

;     ret

; ; Y positions for each row (0-9)
; RowYPositions:
;     db KEYB_ROW_1        ; Row 0 Y position
;     db KEYB_ROW_2        ; Row 1 Y position
;     db KEYB_ROW_3        ; Row 2 Y position
;     db KEYB_ROW_4        ; Row 3 Y position
;     db KEYB_ROW_5        ; Row 4 Y position
;     db KEYB_ROW_6        ; Row 5 Y position
;     db KEYB_ROW_7        ; Row 6 Y position
;     db KEYB_ROW_8        ; Row 7 Y position
;     db KEYB_ROW_9        ; Row 8 Y position
;     db KEYB_ROW_10       ; Row 9 Y position
; 	UNDEFINE KEYB_ROW_1
; 	UNDEFINE KEYB_ROW_2
; 	UNDEFINE KEYB_ROW_3
; 	UNDEFINE KEYB_ROW_4
; 	UNDEFINE KEYB_ROW_5
; 	UNDEFINE KEYB_ROW_6
; 	UNDEFINE KEYB_ROW_7
; 	UNDEFINE KEYB_ROW_8
; 	UNDEFINE KEYB_ROW_9
; 	UNDEFINE KEYB_ROW_10

; 	UNDEFINE KEYB_X
; 	UNDEFINE KEYB_Y

; ; Column header text
; TxtCols: db "COL:", 0

ClearKeyboardArea:
	ld 	hl, SCREEN_START+#50
	ld	b, 8

.outerloop:
	push	bc
	push	hl
	ld 	bc, #6D0
 IFNDEF UpperROMBuild
 	ld	(hl), 0
 	ld	de, hl
 	inc	de
 	ldir
 ELSE

	ld	d, 0
.loop:
	ld 	(hl),d
	inc 	hl
	dec 	bc
	ld 	a, b
	or 	c
	jr 	nz, .loop
 ENDIF
	pop	hl
	pop	bc
	ld	de, #800
	add	hl, de
	djnz	.outerloop

	ret

PrintKeyboard:
	call 	SetDefaultColors
	ld	a, PATTERN_CLEAR
	ld	(FillKeyPattern), a
;;;;;;;;;;;
    ; Check if we're in matrix layout mode
     ;ld a, (KeyboardLayout)
     ;cp KEYBOARD_LAYOUT_MATRIX
     ;jr nz, .normalLayout
    
    ; Print row and column labels for matrix layout
    ; call PrintMatrixLabels
    
;.normalLayout:
;;;;;;;;;;;
	ld 	b, KEY_COUNT
	ld	de, KEYB_TABLE_ROW_SIZE
	ld	hl, (KeyboardLocationTable)
	ld	iy, KeyboardLabels
.printLoop:
	ld	ix, hl
	call 	DrawKey
	add	hl, de
	inc	iy
	djnz	.printLoop

	ret


CheckESCPressedLongEnough:
	ld	hl, FramesESCPressed
	ld	b, (hl)					; Previous number of frames pressed

	ld 	a, (KeyboardMatrixBuffer+8)
	bit 	2, a					; ESC
	jr 	nz, .ESCPressed
	bit 	4, a					; TAB
	jr 	nz, .ESCPressed
	ld 	a, (KeyboardMatrixBuffer+9)
	bit 	4, a					; Fire
	jr 	nz, .ESCPressed

	;; ESC not pressed
	ld	a, b
	or 	a
	call	nz, ClearESCBar
	ld	hl, FramesESCPressed
	ld	(hl), 0

.continue:
	;; Check if it's time to exit to main menu
	ld	hl, FramesESCPressed
	ld	a, (hl)
	cp	53
	ret

.ESCPressed:
	inc	(hl)
	ld 	a, (hl)

	;; Draw a new character in the ESC bar
	call	SetInverseColors
	ld 	a, (hl)
	dec	a
	ld	h, a
	ld	l, ESCBAR_Y
	ld	(TxtCoords), hl
	ld	d, 0
	ld	e, a
	ld	hl, TxtKeyboardOptions
	add	hl, de
	ld	a, (hl)
	call	PrintChar
	jr	.continue


ClearESCBar:
	call	SetDefaultColors
	ld 	h, 0
	ld	l, PRESSESC_Y
	ld 	(TxtCoords),hl
	ld 	hl, TxtKeyboardOptions
	call 	PrintString
	ret



; IN: 	IX - keyboard table for that key
;	IY - keyboard label
DrawKey:
	push	hl
	push	de
	push	bc
	push	ix

	ld	a, 0
	ld	(txt_right), a
	ld	a, (ix)   ; text column in bytes
	ld	(txt_byte_x), a
	ld	a, (ix+1) ; text row in pixels
	ld	(txt_pixels_y), a

	ld	d, (ix)
	ld	e, (ix+1)

	ld	a, (KeyboardLayout)
	cp	KEYBOARD_LAYOUT_MATRIX
	jr	z, .drawKeyNormal

	ld 	a,(iy)
	cp	SPECIALKEY_SHIFTL
	jr	z, .drawShifts
	cp	'e'
	jp	z, .drawReturn

.drawKeyNormal:
	ld 	a,(iy)	
	bit	7, a
	jr	nz, .specialKey
	push	af
	ld	c, 15
	call	DrawKeySquare
	pop	af

	call	PrintCharWithPixels

.exit
	pop	ix
	pop	bc
	pop	de
	pop	hl
	ret

.specialKey:
	call 	.drawSpecialKey
	jr	.exit


.drawSpecialKey:
	push	de
	and	%01111111
	ld	b, a
	ld	d, 0
	ld	e, 3
	ld	hl, (SpecialKeysTable)
	or	a
	jr	z, .found
.loop:
	add	hl, de
	djnz	.loop
.found:
	ld	c, (hl)
	pop	de
	push	hl
	call	DrawKeySquare
	pop	ix
	ld	l, (ix+1)
	ld	h, (ix+2)
	call	PrintStringWithPixels
	ret

.drawShifts:
	;; Left shift
	push	de
	call 	.drawSpecialKey

	;; Right shift
	push	hl
	ld	hl, (KeyboardLocationTable)
	ld	de, RIGHTSHIFT_TABLE_OFFSET
	add	hl, de
	ld	a, (hl)
	pop	hl

	pop	de
	ld	d, a
	ld	(txt_byte_x), a
	ld	a, SPECIALKEY_SHIFTR
	call 	.drawSpecialKey

	jr 	.exit


.drawReturn:
	ld	ix, (SpecialKeysTable)
	ld	c, (ix)
	call	DrawReturnOutline
	ld	hl, TxtKeyReturn
	call	PrintStringWithPixels
	jp	.exit



;; IN:	D - key x in bytes
;; 	E - key y in pixels
;;	C - width in pixels
DrawKeySquare:
	ld	a, (FillKeyPattern)
	or	a
	jr	nz, DrawKeyFill

	call	GetKeyTopLeftEdge
	push	hl
	push	de

	;; Top line
	push	bc
	ld	b, c
	ld	a, PATTERN_YELLOW
	call	DrawHorizontalLine
	pop	bc


	;; Left
	pop	de
	pop 	hl
	push	hl
	push	de
	push	bc
	ld	b, KEY_HEIGHT-1
	call	DrawVerticalLine
	pop	bc

	;; Right
	pop	de
	pop	hl
	push	hl
	push	de
	ld	d, 0
	ld	e, c
	dec	e
	add	hl, de
	pop	de
	push 	de

	push	bc
	ld	b, KEY_HEIGHT-1
	call	DrawVerticalLine
	pop	bc


	;; Bottom line
	pop	de
	ld	hl, KEY_HEIGHT-1
	add	hl, de
	ld	de, hl
	pop	hl
	ld	b, c
	ld	a, PATTERN_YELLOW
	call	DrawHorizontalLine

	ret

;; IN:	D - key x in bytes
;; 	E - key y in pixels
;; OUT: HL - left edge
;;	E - top edge
GetKeyTopLeftEdge:
	dec	d
	ld	h, 0
	ld	l, d
	sla	l
	rl	h
	sla	l			
	rl	h			

	dec	e
	dec	e
	dec	e
	dec	e
	ret

;; IN:	D - key x in bytes
;; 	E - key y in pixels
;;	C - width in pixels
DrawKeyFill:
	call	GetKeyTopLeftEdge
	ld	b, KEY_HEIGHT
.loop:
	push	hl
	push	de
	push	bc
	ld	b, c
	ld	a, (FillKeyPattern)
	call	DrawHorizontalLine
	pop	bc
	pop	de
	pop	hl
	inc	e
	djnz	.loop

	ret


RETURN_INSET EQU 4

RETURN_HEIGHT EQU KEY_HEIGHT*2

;; IN:	D - key x in bytes
;; 	E - key y in pixels
;;	C - width in pixels
DrawReturnOutline:
	ld	a, (FillKeyPattern)
	or	a
	jr	nz, DrawReturnFill

	call	GetKeyTopLeftEdge
	push	hl
	push	de

	;; Top line
	push	bc
	ld	b, c
	ld	a, PATTERN_YELLOW
	call	DrawHorizontalLine
	pop	bc


	;; Left
	pop	de
	pop 	hl
	push	hl
	push	de
	push	bc
	ld	b, KEY_HEIGHT-1
	call	DrawVerticalLine
	pop	bc

	;; Right
	pop	de
	pop	hl
	push	hl
	push	de
	ld	d, 0
	ld	e, c
	dec	e
	add	hl, de
	pop	de
	push 	de

	push	bc
	ld	b, RETURN_HEIGHT+1
	call	DrawVerticalLine
	pop	bc


	;; Small horiz line
	pop	de
	ld	hl, KEY_HEIGHT-1
	add	hl, de
	ld	de, hl
	pop	hl
	push	hl
	push	de
	ld	b, RETURN_INSET
	ld	a, PATTERN_YELLOW
	call	DrawHorizontalLine

	;; Left vertical line
	pop	de
	pop	hl
	push	de
	ld	de, RETURN_INSET
	add	hl, de
	pop	de
	push	hl
	push	de
	push	bc
	ld	b, RETURN_HEIGHT-KEY_HEIGHT+1
	call	DrawVerticalLine
	pop	bc


	;; Bottom line
	pop	de
	ld	hl, RETURN_HEIGHT-KEY_HEIGHT+1
	add	hl, de
	ld	de, hl
	pop 	hl

	ld	a, c
	sub	RETURN_INSET
	ld	b, a
	ld	a, PATTERN_YELLOW
	call	DrawHorizontalLine

	ret

;; IN:	D - key x in bytes
;; 	E - key y in pixels
;;	C - width in pixels
DrawReturnFill:
	call	GetKeyTopLeftEdge
	ld	b, KEY_HEIGHT
.loop:
	push	hl
	push	de
	push	bc
	ld	b, c
	ld	a, (FillKeyPattern)
	call	DrawHorizontalLine
	pop	bc
	pop	de
	pop	hl
	inc	e
	djnz	.loop

	;; Bottom half of the key
	push	de
	ld	de, RETURN_INSET
	add	hl, de
	pop	de
	ld	a, c
	sub	RETURN_INSET
	ld	c, a
	ld	b, RETURN_HEIGHT-KEY_HEIGHT+1
.loop2:
	push	hl
	push	de
	push	bc
	ld	b, c
	ld	a, (FillKeyPattern)
	call	DrawHorizontalLine
	pop	bc
	pop	de
	pop	hl
	inc	e
	djnz	.loop2

	ret



; IN: HL - Keyboard buffer
PrintOnKeysFromBuffer:
	ld	ix, (KeyboardLocationTable)
	ld 	b, KeyboardBufferSize
	ld	iy, KeyboardLabels
.byteLoop:
	push 	bc
	ld 	a,(hl)
	ld 	d,a
	ld 	b,1
.bitLoop:
	ld 	a,d
	and 	b
	jr 	z,.nextBit

	; This one is pressed. Draw it.
	call 	DrawKey

.nextBit:
	push	hl
	push	de
	ld	de, KEYB_TABLE_ROW_SIZE
	ld	hl, ix
	add	hl, de
	ld	ix, hl
	inc	iy
	pop	de
	pop	hl

	sla 	b
	jr 	nc,.bitLoop

.nextByte:
	inc 	hl
	pop 	bc
	djnz 	.byteLoop
	ret



;; IN: HL - buffer
@ClearKeyboardBuffer:
	ld b, KeyboardBufferSize
.loop:
	ld (hl),0
	inc hl
	djnz .loop
	ret


KeyboardSetUpScreen:
	ld 	d, 0
	call 	ClearScreen
	ld 	a, 4
	call 	SetBorderColor 

	ld 	hl, TxtKeyboardTitle
	ld 	d, (ScreenCharsWidth - TxtTitleLen - TxtKeyboardTitleLen)/2
	call 	PrintTitleBanner

	call	ClearESCBar
	ret


ShowRolsColsMask:    
    call SetDefaultColors
    ld h, 8					; X position	
    ld l, PRESSESC_Y-1          ; One line above ESC bar
    ld (TxtCoords), hl

    ld hl, TxtRows
    call PrintString
    
	; clear variables
	ld a, 0
	ld (PressedRowsMask), a
	ld hl, 0
	ld (PressedColsMask), hl
	call GetPressedBitsMask 	; result mask in A

	ld (PressedRowsMask), a
    ;push af                     ; Save result for later
    call PrintABin              ; Print the binary value
    
	ld a, 1
	call MoveXPos

    ld hl, TxtCols
    call PrintString

	; cols 9-8
	ld 	hl, PresseddMatrixBuffer+8
	call GetPressedColsMask	; result in A for cols 9-8
	and %00000011
	ld (PressedColsMask+1), a
	call PrintA2Bin

    ; cols 7-0
    ld hl, PresseddMatrixBuffer
    call GetPressedColsMask	; result in A for cols 7-0
	ld (PressedColsMask), a
    call PrintABin
	ret


PRESSESC_Y EQU #18
ESCBAR_Y EQU PRESSESC_Y


TxtKeyboardTitle: db ' - KEYBOARD TEST',0
TxtKeyboardTitleLen EQU $-TxtKeyboardTitle-1

TxtKeyboardOptions: db ' HOLD ESC OR FIRE TO EXIT. HOLD TAB TO CHANGE LAYOUT.',0

TxtCols: 		db 'COL [9-0]:',0
TxtRows: 		db 'ROW [7-0]:',0

 INCLUDE "KeyboardLayout.asm"
 INCLUDE "KeyboardLabels.asm"

 ENDMODULE

