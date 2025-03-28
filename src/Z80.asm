 MODULE Z80

; Test if the cpu is a cmos variety according to out (c), 0
; https://www.malinov.com/sergeys-blog/z80-type-detection.html
;
; NMOS: OUT (C),0
; CMOS: OUT (C),0 actually does OUT (C),255
;
; Output:
;   A = 0 -> NMOS
;   A = 1 -> CMOS
@TestZ80CMOS:
    di                  ; Disable interrupts during test
    ld    b, #F4        ; Use a R/W port. PPI Port A 0xF4--						
						; https://www.cpcwiki.eu/index.php/Default_I/O_Port_Summary
    db    #ED, #71      ; Undocumented instruction: out (c), 0
						; Register C is placed on the bottom half (A0 through A7) 
						; Register B is placed on the top half (A8 through A15)
	nop					; is it necessary?
	in   a, (c)	        ; Get result (00h=NMOS, FFh=CMOS)
    ei                  ; Enable interrupts again	
	and  1			    ; Clear all bits except bit 0
	ld   (Z80Type), a   ; Save result in Z80Type variable
	ret					; return 0 if NMOS, 1 if CMOS

@Z80TypeNames:
TxtNMOS:   db "NMOS", 0
TxtCMOS:   db "CMOS", 0

@Z80TypeTableOffset:
  db 0
  db TxtCMOS - TxtNMOS

; This function tests the behavior of the `ccf` and `scf` instructions
; and determines the Z80 flavor of the CPU such as Zilog, NEC NMOS, ST CMOS,
; Based on https://github.com/redcode/Z80_XCF_Flavor
; OUT: HL = pointer to a string with the flavor name.
; Variable Z80Flavor contains 0 for unknown, 1 for Zilog, 2 for NEC NMOS, 3 for ST CMOS.
; Modifies: AF, BC, DE, HL
@TestZ80Flavor:
	macro Q0_F0_A0
		xor a	     ; A = 0; YF, XF, YQ, XQ = 0
	endm

	macro Q0_F1_A0
		xor a	     ;
		dec a	     ; YF, XF = 1
		ld  a, 0     ; A = 0; Q = 0
	endm

	macro Q1_F1_A0
		xor a	     ; A = 0
		ld  e, a     ;
		dec e	     ; YF, XF, YQ, XQ = 1
	endm

	macro Q0_F0_A1
		xor a	     ; YF, XF = 0
		ld  a, $FF   ; A = FFh; Q = 0
	endm

	macro Q0_F1_A1
		xor a	     ;
		dec a	     ; A = FFh; YF, XF = 1
		nop	         ; Q = 0
	endm

	macro Q1_F1_A1
		xor a	     ;
		dec a	     ; A = FFh; YF, XF, YQ, XQ = 1
	endm

startFlavorTest:	
    di		     ; Disable interrupts.

	ld   bc, Z80IxfResults     ; Set BC to the address of the results array.

	; Test all factor combinations with `ccf` and
	; store the resulting values of YF and XF.
	Q0_F0_A0 : ccf : call store_yxf
	Q0_F1_A0 : ccf : call store_yxf
	Q1_F1_A0 : ccf : call store_yxf
	Q0_F0_A1 : ccf : call store_yxf
	Q0_F1_A1 : ccf : call store_yxf
	Q1_F1_A1 : ccf : call store_yxf

	; Test all factor combinations with `scf` and
	; store the resulting values of YF and XF.
	Q0_F0_A0 : scf : call store_yxf
	Q0_F1_A0 : scf : call store_yxf
	Q1_F1_A0 : scf : call store_yxf
	Q0_F0_A1 : scf : call store_yxf
	Q0_F1_A1 : scf : call store_yxf
	Q1_F1_A1 : scf : call store_yxf

    ld   de, Z80IxfResults    ; Compare the values obtained with `ccf`,
    ld   hl, Z80IxfResults + 6; against those obtained with `scf`.
    call compare_results      ; They should be the same; otherwise,
	ld   b, 0
    cp   0                    ; the behavior is unknown (or unstable)
    jr   nz, .done  ; and we report it.

    ld   de, Z80IxfResults    ; Compare the values obtained with `ccf`
    ld   hl, resultsZilog     ; against the reference values for Zilog
    call compare_results      ; CPU models.
    ld   b, 1
    cp   0                    ; If the values match, report "Zilog
    jr   z, .done             ; flavor" and exit.

    ld   de, Z80IxfResults    ; Compare the values obtained with `ccf`
    ld   hl, resultsNecNMOS   ; against the reference values for NEC
    call compare_results      ; NMOS CPU models.
    ld   b, 2
    cp   0                    ; If the values match, report "NEC NMOS
    jr   z, .done             ; flavor" and exit.

    ld   de, Z80IxfResults    ; Compare the values obtained with `ccf`
    ld   hl, resultsSTCMOS    ; against the reference values for ST
    call compare_results      ; CMOS CPU models.
    ld   b, 3
    cp   0                    ; If the values match, report "ST CMOS
    jr   z, .done             ; flavor" and exit.

.done:
	
	ei			     		  ; Re-enable interrupts.
	ld a, b
	ld  (Z80Flavor), a       ; Store the flavor name in Z80Flavor.
	ret			     


; stores YF and XF into the results array.
; IN: BC - Address of the element in the results array.
; OUT BC - Address of the next element in the results array.
; Modifies A and DE.
store_yxf:
	push af	       ; 
	pop  de	       ;
	ld   a, e      ; F is transferred to A.
	and  00101000b ; Clear all flags except YF and XF.
	ld   (bc), a   ; store YF and XF into the results array.
	inc  bc	       ; Point BC to the next element of the array.
	ret

; Compares 2 arrays of results.
; IN: HL - Array 1 address, DE - Array 2 address.
; OUT: A - 0 if the arrays are equal; otherwise, a non-zero value.
; Modifies C, DE and HL.
compare_results:
	ld   c, 6
.compare:
	ld   a, (de)
	sub  (hl)
	ret  nz
	inc  de
	inc  hl
	dec  c
	jr   nz, .compare
	ret

; Print bit 4 and bit 6 of the 12 values in Z80IxfResults
; Used for debugging purposes.
; IN: None
; Modifies: HL, BC, A
; @PrintZ80IxfResultsBits46:
;     ld hl, Z80IxfResults      ; Point HL to the start of the Z80IxfResults array
;     ld b, 12                  ; Set B to the number of values to process (12)

; .PrintLoop:
;     ld a, (hl)                ; Load the current value into A
;     and %00101000             ; Mask to keep only bit 4 and bit 6
	
; 	bit 5, a
; 	jr z, .done5
; 	set 1, a
; .done5:
; 	bit 3, a
; 	jr z, .done3
; 	set 0, a
; .done3:

; 	push hl
; 	push bc
;     call PrintA2Bin
;     ld a, " "                 ; ASCII space character
;     call PrintChar            ; Print a space (for readability)
; 	pop bc
; 	pop hl
;     inc hl                    ; Move to the next value in the array
;     djnz .PrintLoop           ; Decrement B and loop if not zero
;     ret                       ; Return when all values are processed

resultsZilog:
	db 00000000b, 00101000b, 00000000b, 00101000b, 00101000b, 00101000b
resultsNecNMOS:
	db 00000000b, 00000000b, 00000000b, 00101000b, 00101000b, 00101000b
resultsSTCMOS:
	db 00000000b, 00100000b, 00000000b, 00001000b, 00101000b, 00101000b
; header_text:	
; 	db "CCF/SCF test", 0	
;   db " Case    ANY   NEC  ST     HOST", 0
; 	db " TEST    ZILOG NMOS CMOS   CPU  ", 0
; 	db '(Q<>F)|A   YX   YX   YX   YX  YX', 0
;   db "                          ccf scf", 0
; rows_text:
; 	db "(0<>0)|0   00   00   00   ", 0
; 	db "(0<>1)|0   11   00   10   ", 0
; 	db "(1<>1)|0   00   00   00   ", 0
; 	db "(0<>0)|1   11   11   01   ", 0
; 	db "(0<>1)|1   11   11   11   ", 0
; 	db "(1<>1)|1   11   11   11   ", 0

@Z80FlavorNames:
TxtUnknown: db "UNKNOWN",0
TxtZilog:   db "ZILOG",0
TxtNecNmos: db "NEC NMOS",0
TxtStCmos:  db "ST CMOS",0

@Z80FlavorTableOffset:
  db 0
  db TxtZilog   - TxtUnknown
  db TxtNecNmos - TxtUnknown
  db TxtStCmos  - TxtUnknown
  

 ENDMODULE 