 MODULE DISK

; Based on RPM by Brice Rive, Philippe Depre and Simon Owen
;
; This program measures the rotation speed (RPM) of floppy disk drives
; by using the PD765 Floppy Disk Controller to time index hole detections.


; Notes:
; - RPM calculation is based on the time it takes for one full disk rotation.
; - The program uses the PD765 FDC "Read ID" command to detect index holes
; - 3" disks have a hole known as index hole
; - Each loop in TRWait0 takes exactly 20 T-states (clock cycles)
; - At 4MHz Z80 clock, each T-state is 0.25μs
; - A standard 3.5" floppy spins at 300 RPM (5 rotations per second)
; - One rotation takes 200ms = 200,000μs
; - Expected count: 200,000μs / (20 T-states * 0.25μs/T-state) = 40,000

; Original formula found in BASIC program is 
; rpm=1000000/(count*20)*120
; simplified to 6000000/count to reduce operations
; count is the number of times the 20us looped
; 1000000 is 1 second in microseconds
; 20 is the time in microseconds of the loop
; 120 is unknown


; CPC works at 4MHz, but effective clock rate is 3.3MHz, so 1 T-state is 0.303us
; 64 T-states = 19.392us
; Accesses to memory are synchronised with the video logic - they are constrained to
; occur on microsecond boundaries. This has the effect of stretching each Z80 M cycle
; (machine cycle) to be a multiple of 4 T states (clock cycles). In practice this alters the
; instruction timing so that the effective clock rate is approximately 3.3 MHz.
; List of clockcycles http://www.z80.info/z80time.txt

 ; Communication between AMSTRAD CPC and FDC is done through the following ports:
 ; &FA7E - %xxxxx0x0 0xxxxxxx motor on/off 
 ; &FB7E - %xxxxx0x1 0xxxxxx0 FDC MAIN STATUS REGISTER (read only). 
 ;         Signals when is ready to receive data through DATA REGISTER
 ; &FB7F - %xxxxx0x1 0xxxxxx1 FDC DATA REGISTER  Used for command, execution and result phases.

DiscSetUpScreen:
	ld    d, 0
	call  ClearScreen
	ld    a, 4
	call  SetBorderColor

	ld 	  hl, TxtDiskTestTitle
	ld 	  d, (ScreenCharsWidth - TxtTitleLen - TxtDiskTestTitleLen)/2
	call  PrintTitleBanner

	ld 	  hl, #0002
	ld 	  (TxtCoords),hl
	call  SetDefaultColors
	ret


TxtRPM:
    db    'RPM: ',0
TxtDiskTestTitle:
    db    ' - DISK TEST',0
TxtDiskTestTitleLen equ   $-TxtDiskTestTitle-1	

@DiskTest:
	; Disk Test
	call  DiscSetUpScreen
	call  SetDefaultColors

	; call  DiskMotorOFF

	; ld 	  hl, TxtMotorON
	; call  PrintString
	; call  NewLine
	; call  DiskMotorON
	
    ; call Delay

	; ld 	  hl, TxtMotorOFF
	; call  PrintString
	; call  NewLine
	; call  DiskMotorOFF	
	
    call  DiskMotorON
    ld 	  hl, TxtRPM
	call  PrintString

    ;LD A ,1
    ;call Delay

    call  DiskTestRPM

	; Press any key for main menu
	call NewLine
    ld 	 hl, TxtAnyKeyMainMenu
	jp   PrintString

	ret


@DiskMotorON:
	ld      bc,$fa7e			; start disk motor
	ld      a,1
	out     (c),a
	ret

@DiskMotorOFF:
	ld      bc,$fa7e			; stop disk motor	
    xor     a                   ; cheap version of ld a,0
	out     (c),a
	ret

@DiskTestRPM:	
    ld b, 15 ; repeat test

.diskloop:
    ;save TxtCoords
    ld hl, (TxtCoords)
    push hl
    push bc         ; save loop counter

    ld a, b
    call PrintADec
    ld a, " "
    call PrintChar


    call  GetRPM    ; count in hl, rpm in de
    
    push de
    call  PrintHLHex ; print counts
    ld a, " "
    call PrintChar
    pop de

    ld    hl, de; hl counter, de rpm    
    call  PrintHLDec


    pop bc          ; restore loop counter
    ;restore TxtCoords
    pop hl
    ld (TxtCoords), hl
    djnz .diskloop
    call DiskMotorOFF
	ret

; GetRPM - Measure floppy drive rotation speed
; Output: HL = raw timer count for one rotation
;         DE = calculated RPM value

GetRPM:
    call GetCount         ; Perform RPM measurement
    ld   hl,(TimeCount)          ; Load timer count into hl
    

    push hl                 ; Save raw count for return value
    call CalculateRPM       ; 6000000/count
    
    ;ld   (RpmInt), de        ; store RPM value
    pop  hl                 ; Restore raw count to return in HL
  
    ret

; Calculate_RPM - Convert timer count to RPM
; Input: HL = timer count
; Output: DE = RPM value (6000000/HL)
; Modifies A
CalculateRPM:
    ld de, 0

    ; h should be bigger than 0x0 -> hl > 65535
    ld a, h
    or a
    jr z, .too_small

    ld bc, hl
    ; use dividend = 6000000, divisor = count
    ; multiply dividend by 100 and use last 2 numbers as decimals :)
    ; Input: HL,DE=Value1, BC=Value2
    ; Output: BCDE=Value1/Value2, HL=Value1 MOD Value2
    ; Destroyed: AF
    ; ld hl, 6000000 / 65536
    ; ld de, 6000000 % 65536    
    ; push af
    ; call Div32by16Fast
    ; pop af
 
    ; 32bit division
    ; Input: IY,BC=Value1, IX=Value2
    ; Output: IY,BC=Value1/Value2, HL=Value1 MOD Value2
    ; Destroyed: AF,DE,IY
    push af
    push ix
    ld ix, bc
    ld iy, 6000000 / 65536
    ld bc, 6000000 % 65536        
    call Div32by16
    pop ix
    pop af
    ld de, bc

.too_small:
    ret

GetCount:
    di                         ; Disable interrupts for timing accuracy
    call TmdRead               ; First call to sync with index hole
    ld   hl,0                  ; Initialize timer counter
    ld   (TimeCount),hl        ; Store initial counter value
    call TmdRead               ; Second call to measure time for one rotation
    ei                         ; Re-enable interrupts
    ret                        ; Return with timer count in (TimeCount)


; TmdRead - Issues "Read ID" command to FDC and counts cycles until index detected
TmdRead:
    call FdcCmd                ; Send command to FDC
                               ; call pushes return address to stack which is the following instruction
                               ; but in this case the next block is data
TRCmd:
    ; PD765 FDC command: Read ID with MFM mode
    db #09,#46,#00,#00,#00,#00,#00,#00,#2A,#FF
        ;  DEFB 9                         ; 9 Parameters
        ;  DEFB #46                       ; READ DATA Command Alter To #4C Del.
        ;  DEFB 0                         ; Drive
        ;  DEFB 0                         ; Track
        ;  DEFB 0                         ; Head Number
        ;  DEFB 0                         ; Sector To Read
        ;  DEFB 0                         ; Number Of Data Bytes Per Sector
        ;  DEFB 0                         ; End Of Track
        ;  DEFB #2A                       ; Gap #3 - Generally #2a
        ;  DEFB #ff                       ; Data Length - Sect Size < 256


    ld   bc,#FB7E              ; FDC status register port
TRWait0:                       ;                             T-states (0.303 us each)
    in   a,(c)                 ; Read FDC status register    12
    ld   hl,(TimeCount)        ; Get current timer count     16
    inc  hl                    ; Increment timer count        6
    ld   (TimeCount),hl        ; Store updated timer count   16
    nop                        ; Time padding                 4
    jp   p,TRWait0             ; Loop until bit 7 set        10
                               ;                             64 * 0.303 = 19.392 us approx 20us
                                          

    and  #20                   ; Check if sector found bit is set
    ret  nz                    ; Return if sector found (error avoided)
    jp   FdcResult             ; Process FDC result phase
                               ; sum 40 T-states                 

; FdcCmd - Sends a multi-byte command sequence to the FDC
FdcCmd:
    pop  hl                    ; Put return address (next instruction after call) in hl
                               ; that corresponds to TRCmd
    ld   b,(hl)                ; First byte is command length
FdcCmd0:
    inc  hl                    ; Point to next command byte
    ld   a,(hl)                ; Get command byte
    push bc                    ; Save counter
    call FdcSend               ; Send byte to FDC
    pop  bc                    ; Restore counter
    djnz FdcCmd0               ; Loop for all command bytes
    inc  hl                    ; Point to return address
    jp   (hl)                  ; Return to caller instead of RET aka "return address harvesting"



; Send a command byte to the FDC
; Input: A = command byte
FdcSend:
        ld      bc,#fb7e        ; FDC control port
        push    af

.waitrqm:
        in      a, (c)          ; Read FDC status
        add     a, a            ; Check ready bit
        jr      nc, .waitrqm    ; Loop until ready
        add     a, a            ; Check direction bit
        jr      nc, .okout      ; If output direction OK
        pop     af              ; Otherwise error
        ret

.okout:
        pop     af              ; Get command byte
        inc     c               ; Point to FDC data port
        out     (c) ,a          ; Send byte
        dec     c               ; Back to status port
        ld      a, 5            ; Short delay

.fdctemp:
        dec     a
        nop
        jr      nz, .fdctemp
        ret


; Get command result from FDC
FdcResult:
        push    hl
        ld      hl,(ResTime)    ; Get current timing value

.waitres:
        in      a,(c)           ; Read FDC status
        inc     hl              ; Count cycles
        cp      #c0             ; Check if result ready
        jr      c,.waitres      ; If not, keep waiting
        ld      (ResTime),hl    ; Save updated time
        ld      hl, ResBuf      ; Point to result buffer

.resagain:
        in      a,(c)           ; Read status again
        cp      #c0             ; Check if still ready
        jr      c,.resagain     ; If not, wait
        inc     c               ; Point to data port
        in      a,(c)           ; Read result byte
        dec     c               ; Back to status port
        ld      (hl),a          ; Store byte
        inc     hl              ; Next buffer position
        ld      a,#05           ; Short delay

.restemp:
        dec     a
        jr      nz,.restemp
        in      a,(c)           ; Check if more bytes
        and     #10             ; Test busy flag
        jr      nz,.resagain     ; If busy, get more bytes
        pop     hl
        ret


; mod 10
; Input: A = value to divide by 10
; Output: A = quotient, B = remainder
; Destroyed: BC
; Mod10:
;    ld bc,05A0h               ; Load B=5, C=A0
; Loop:
;         sub c                ; Subtract C from A
;         jr nc,A_geq_C           ; If no carry (A>=C), skip
;         add a,c              ; Add C back to A (only executed if carry)
; A_geq_C:
;         srl c                ; Shift C right (divide by 2)
;         djnz Loop            ; Decrement B and loop if not zero
;         ret                  ; Return

; https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Division#32.2F16_division
; https://www.cpcwiki.eu/index.php/Programming:Integer_Division#24bit_division
; Div16by16:
; ;BC_Div_DE:
; ;BC/DE ==> BC, remainder in HL
; ;NOTE: BC/0 returns 0 as the quotient.
; ;min: 1072cc
; ;max: 1232cc
; ;avg: 1152cc
; ;28 bytes
;   xor a
;   ld h,a
;   ld l,a
;   sub e
;   ld e,a
;   sbc a,a
;   sub d
;   ld d,a

;   ld a,b
;   ld b,16

; div_loop:
;   ;shift the bits from BC into HL
;   rl c : rla
;   adc hl,hl
;   add hl,de
;   jr c,div_loop_done
;   sbc hl,de

; div_loop_done:
;   djnz div_loop
;   rl c : rla
;   ld b,a
;   ret


; Fast 32bit division
; Input: HL,DE=Value1, BC=Value2
; Output: BCDE=Value1/Value2, HL=Value1 MOD Value2
; Destroyed: AF
; Not used: IX, IY
; CPC Cycles: 1016-2444 (1730 on average), 254-611 usec (432 on average)
; Size: 277 bytes
; http://cpcwiki.eu/index.php/Programming:Integer_Division#Fast_32bit_division

; Div32by16Fast:

; ; Macro for division with remainder
; div_r: MACRO
;      SLA   E
;      RL    D
;      ADC   HL, HL

;      LD    A, L
;      ADD   A, C
;      LD    A, H
;      ADC   A, B
;      JR    NC, .t2

;      ADD   HL, BC
;      INC   DE
; .t2:
;      ENDM

; ; Macro for division without remainder
; div_e: MACRO
;      SLA   E
;      RL    D
;      ADC   HL, HL
;      JR    C, .t1

;      LD    A, L
;      ADD   A, C
;      LD    A, H
;      ADC   A, B
;      JR    NC, .t2
; .t1:
;      ADD   HL, BC
;      INC   DE
; .t2:
;      ENDM

; ; Main division procedure
; div32x16:
;      DEC   BC
;      LD    A, B
;      CPL 
;      LD    B, A
;      LD    A, C
;      CPL 
;      LD    C, A
;      ADD   A, L
;      LD    A, B
;      ADC   A, H
;      JR    NC, .DIV16

;      PUSH  DE
;      EX    DE, HL
;      LD    HL, 0000h
;      CALL  .DIV32R
;      EX    DE, HL
;      EX    (SP), HL
;      EX    DE, HL
;      CALL  .DIV32E
;      POP   BC
;      RET
     
; .DIV16:
;      CALL  .DIV32E
;      LD    BC, 0000h
;      RET
     
; .DIV32R:  ; DE = HLDE/(-BC), HL = HLDE%(-BC), -BC < $8000
;      CALL  $+3
;      div_r
;      div_r
;      div_r
;      div_r
;      div_r
;      div_r
;      div_r
;      div_r
;      RET
     
; .DIV32E:  ; DE = HLDE/(-BC), HL = HLDE%(-BC)
;      CALL  $+3
;      div_e
;      div_e
;      div_e
;      div_e
;      div_e
;      div_e
;      div_e
;      div_e
;      RET




; 32-bit / 16-bit division routine
; Input:  
;   HLDE = Dividend (32-bit)
;   BC   = Divisor  (16-bit)
; Output:
;   HL   = Quotient (16-bit)
;   DE   = Remainder (16-bit)
; Clobbers: AF, BC, HL, DE
;
; DIV32_16:
;     xor  a          ; Clear carry flag
;     ld   b, 16      ; We will perform 16 shifts

; DIV_LOOP:
;     sla  e          ; Shift left HLDE
;     rl   d
;     rl   l
;     rl   h

;     push hl         ; Save HL
;     push de         ; Save DE

;     sbc  hl, bc     ; Try to subtract divisor

;     jr   nc, NO_UNDERFLOW
;     pop  de         ; Restore DE (undo subtraction)
;     pop  hl         ; Restore HL
;     jr   NEXT_ITER

; NO_UNDERFLOW:
;     pop  de         ; Restore DE
;     pop  hl         ; Restore HL
;     scf             ; Set carry (quotient bit)
    
; NEXT_ITER:
;     rl   e          ; Shift carry into quotient bit
;     rl   d
;     rl   l
;     rl   h

;     djnz DIV_LOOP   ; Repeat 16 times

;     ret             ; HL = Quotient, DE = Remainder


; 32bit division
; Input: IY,BC=Value1, IX=Value2
; Output: IY,BC=Value1/Value2, HL=Value1 MOD Value2
; Destroyed: AF,DE,IY
; CPC Cycles: approximately 7800, 1900 usec
; Size: 91 bytes
; https://www.cpcwiki.eu/index.php/Programming:Integer_Division

Div32by16:
;clcd32c: db 0
.clcd32:  
        ld hl,0
        db #dd:ld a,l   ; ld a,ixl (undocumented)
        db #dd:or h     ; or ixh   (undocumented)
        ret z           ;IY,BC=Value1(Counter)
        ld de,0         ;DE,HL=CalcVar
        ld a,32         ;Set Counter to 32
.div_loop:
        ld (clcd32c),a
        ;push af
        rl c
        rl b
        db #fd:ld a,l:rla:db #fd:ld l,a  ; ld a,ixl:rla:ld ixl,a (undocumented)
        db #fd:ld a,h:rla:db #fd:ld h,a  ; ld a,ixh:rla:ld ixh,a (undocumented)
        rl l
        rl h
        rl e
        rl d
        ld a,l
        db #dd:sub l ; sub ixl (undocumented)
        ld l,a
        ld a,h
        db #dd:sbc h ; sbc ixh (undocumented)
        ld h,a
        ld a,e
        sbc 0
        ld e,a
        ld a,d
        sbc 0
        ld d,a
        jr nc,.clcd322
        ld a,l
        db #dd:add l ; add ixl (undocumented)
        ld l,a
        ld a,h
        db #dd:adc h ; adc ixh (undocumented)
        ld h,a
        ld a,e
        adc 0
        ld e,a
        ld a,d
        adc 0
        ld d,a
        scf
.clcd322:
        ;pop af
        ccf
        ld a,(clcd32c)
        dec a
        jr nz,.div_loop   ;HL=Value1 MOD Value2
        rl c
        rl b
        db #fd:ld a,l:rla:db #fd:ld l,a ; ld a,ixl:rla:ld ixl,a (undocumented)
        db #fd:ld a,h:rla:db #fd:ld h,a ; ld a,ixh:rla:ld ixh,a (undocumented)
        ret             ;IY,BC=Value1 DIV Value2



 ENDMODULE
