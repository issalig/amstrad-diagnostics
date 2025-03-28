 MODULE DISK

; Based on RPM by Brice Rive, Philippe Depre and Simon Owen
;
; This program measures the rotation speed (RPM) of floppy disk drives
; by using the PD765 Floppy Disk Controller to time a full rotation.

; External library function references:
; SETDRIVE - Selects and activates the specified floppy drive (0 or 1) via FDC port
; SEEK - Positions the drive head to the specified track using firmware call #C763
; FDCOUT - Sends a command byte to the FDC through port #FB7E, handling ready status
; RESULT - Processes result bytes from FDC and stores them in RESBUF buffer

; Program-specific functions:
; FdcCmd - Sends a multi-byte command sequence to the FDC with precise timing
; TmdRead - Issues "Read ID" command to FDC and counts cycles until index detected
; Rpm1 - Sets up drive selection and seeks to the desired track for measurement
; Rpm2 - Performs the actual RPM measurement by timing one complete disk rotation


; Notes:
; - RPM calculation is based on the time it takes for one full disk rotation
; - The program uses the PD765 FDC "Read ID" command to detect index holes
; - Each loop in TRWait0 takes exactly 20 T-states (clock cycles)
; - At 4MHz Z80 clock, each T-state is 0.25μs
; - A standard 3.5" floppy spins at 300 RPM (5 rotations per second)
; - One rotation takes 200ms = 200,000μs
; - Expected count: 200,000μs / (20 T-states * 0.25μs/T-state) = 40,000

;  TmdRead and FdcCmd are the main routines for measuring RPM

; Technical Implementation
; The measurement technique is quite clever:

; The program sends a "Read ID" command to the FDC
; It then counts timer cycles until the index hole on the disk is detected
; This process is done twice - first to synchronize with the index hole, then to measure a full rotation
; Each counting loop is precisely calibrated to take exactly 20 T-states (CPU clock cycles)
; The code includes precise timing calculations:

; At 4MHz Z80 clock, each T-state takes 0.25μs
; A standard 3.5" floppy typically spins at 300 RPM (5 rotations per second)
; One rotation takes 200ms = 200,000μs
; Expected count: 200,000μs / (20 T-states * 0.25μs/T-state) = 40,000

; http://dunfield.classiccmp.org/r/765.pdf
; https://www.hermannseib.com/documents/floppy.pdf (Section 4)
; https://muckypaws.com/2024/02/25/%C2%B5pd765a-disc-controller-primer/#Seek
; index is pin 17 of FDC chip

; https://muckypaws.com/2024/02/25/%C2%B5pd765a-disc-controller-primer/
; https://github.com/sJoseman/amstrad/blob/master/loader%20original%20disco%20Cyberbig%20%5Brevisado%20y%20comentado%20joseman%5D.asm
; https://cpcrulez.fr/coding_fdc-01-fonctionnement_FDC__SOSP.htm
; https://www.cpcwiki.eu/index.php?title=765_FDC&mobileaction=toggle_view_desktop

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
  db    ' - DISK',0
TxtDiskTestTitleLen equ   $-TxtDiskTestTitle-1	

@DiskTest:
	; Disk Test
	call  DiscSetUpScreen
	call  SetDefaultColors

	call  DiskMotorOFF

	ld 	  hl, TxtMotorON
	call  PrintString
	call  NewLine
	call  DiskMotorON
	
    call Delay

	ld 	  hl, TxtMotorOFF
	call  PrintString
	call  NewLine
	call  DiskMotorOFF	
	
    ld 	  hl, TxtRPM
	call  PrintString
    call  DiskTestRPM

	; Press any key for main menu
	call NewLine
    ld 	 hl, TxtAnyKeyMainMenu
	jp   PrintString

	ret

; https://muckypaws.com/2024/02/25/%C2%B5pd765a-disc-controller-primer/
; https://github.com/sJoseman/amstrad/blob/master/loader%20original%20disco%20Cyberbig%20%5Brevisado%20y%20comentado%20joseman%5D.asm
; https://www.cpcwiki.eu/index.php?title=765_FDC

 ; Communication between AMSTRAD CPC and FDC is done through the following ports:
 ; &FA7E - %xxxxx0x0 0xxxxxxx motor on/off 
 ; &FB7E - %xxxxx0x1 0xxxxxx0 FDC MAIN STATUS REGISTER (read only).
 ; 			Signals when is ready to receive data through DATA REGISTER
 ; &FB7F - %xxxxx0x1 0xxxxxx1 FDC DATA REGISTER  Used for command, execution and result phases.

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
    ld b, 25 ; repeat test
diskloop:
    push bc

    ;save TxtCoords
    ld hl, (TxtCoords)
    push hl

    ;ld    a, 0
    ;ld    (Drive), a
    ;ld    (Track), a
    call  GetRPM

    ld    hl, de; hl counter, de rpm    

    call  PrintHLDec

    ;restore TxtCoords
    pop hl
    ld (TxtCoords), hl

    ;call NewLine
    pop bc
    djnz diskloop

    call DiskMotorOFF
	ret

; getRPM - Measure floppy drive rotation speed
; Input: B = drive number (0 or 1)
;        C = track number (0-40)
; Output: HL = raw timer count for one rotation
;         DE = calculated RPM value
; getRPM0:
;     ; Save registers
;     push bc                 ; Save input parameters
    
;     ; Store parameters to variables
;     ld   a,b                ; Get drive number from B
;     ld   (Drive),a          ; Store in Drive variable
;     ld   a,c                ; Get track number from C
;     ld   (Track),a          ; Store in Track variable
;     pop bc

; out DE rpm value
GetRPM:
    ;push bc
    call Rpm1               ; Set up drive and seek to track
    call Rpm2               ; Perform RPM measurement
    ld   hl,(Time)          ; Load timer count into BC
    

    push hl                 ; Save raw count for return value
    call calculateRPM       ; 6000000/count
    
    ;ld   (RpmInt), de        ; store RPM value
    pop  hl                 ; Restore raw count to return in HL
    ;pop  bc                 ; Restore input parameters
  
    ret

; Calculate_RPM - Convert timer count to RPM
; Input: HL = timer count
; Output: DE = RPM value (6000000/HL)
calculateRPM:
    ; if c is 0 then jr zero_count
    ld a, c
    or a
    jr z, zero_count

    ld bc, hl
    ; use dividend = 6000000, divisor = count
    ; multiply dividend by 100 and use last 2 numbers as decimals :)
    ; Input: HL,DE=Value1, BC=Value2
    ; Output: BCDE=Value1/Value2, HL=Value1 MOD Value2
    ; Destroyed: AF
    ld hl, 6000000 / 65536
    ld de, 6000000 % 65536    
    push af
    call Div32by16Fast
    pop af
zero_count:
    ret

;
Rpm1:
    ld   a,0;(Drive)             ; Load drive number
    call SETDRIVE              ; Set drive
    ; always 0 for now ld   (TRCmd+2),a           ; Store drive number in FDC command
    ld   a,0;(Track)             ; Load track number
    ld   d,a                   ; Put track number in D
    jp   SEEK                  ; Seek to track

Rpm2:
    di                         ; Disable interrupts for timing accuracy
    call TmdRead               ; First call to sync with index hole
    ld   hl,0                  ; Initialize timer counter
    ld   (Time),hl             ; Store initial counter value
    call TmdRead               ; Second call to measure time for one rotation
    ei                         ; Re-enable interrupts
    ret                        ; Return with timer count in (Time)

;
TmdRead:
    call FdcCmd                ; Send command to FDC
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
        ;  DEFB #2A                         ; Gap #3 - Generally #2a
        ;  DEFB #ff                       ; Data Length - Sect Size < 256

    ld   bc,#FB7E              ; FDC status register port
TRWait0:
    in   a,(c)                 ; Read FDC status register (4 T-states)
    ld   hl,(Time)             ; Get current timer count (5 T-states)
    inc  hl                    ; Increment timer count (2 T-states)
    ld   (Time),hl             ; Store updated timer count (5 T-states)
    nop                        ; Time padding (1 T-state)
    jp   p,TRWait0             ; Loop until bit 7 set (3 T-states)
                               ; Total: 20 T-states per loop
    and  #20                   ; Check if sector found bit is set
    ret  nz                    ; Return if sector found (error avoided)
    jp   RESULT                ; Process FDC result phase
;
FdcCmd:
    pop  hl                    ; Get address of command bytes
    ld   b,(hl)                ; First byte is command length
FdcCmd0:
    inc  hl                    ; Point to next command byte
    ld   a,(hl)                ; Get command byte
    push bc                    ; Save counter
    call FDCOUT                ; Send byte to FDC
    pop  bc                    ; Restore counter
    djnz FdcCmd0               ; Loop for all command bytes
    inc  hl                    ; Point to return address
    jp   (hl)                  ; Return to caller




;; Routines for 765 FDC

; Buffer for FDC result bytes
RESBUF:     DEFS    20

; Set drive selection bits
; Input: A = drive bits to set
SETDRIVE:
        PUSH    AF
        PUSH    HL
        PUSH    BC
        PUSH    DE
        LD      B, A            ; Store drive bits
        LD      HL, DRIVES      ; Pointer to drive table
SETDRI10:
        LD      E, (HL)         ; Get drive address low byte
        INC     HL
        LD      D, (HL)         ; Get drive address high byte
        INC     HL
        LD      A, D            ; Check if end of table (DE=0)
        OR      E
        JR      Z,SETDRI11      ; If zero, we're done
        INC     DE              ; Point to drive data
        LD      A,(DE)          ; Get current drive bits
        AND     #FE             ; Clear bit 0
        OR      B               ; Set new drive bits
        LD      (DE),A          ; Store updated bits
        JR      SETDRI10        ; Loop to next entry

SETDRI11:
        POP     DE              ; Restore registers
        POP     BC
        POP     HL
        POP     AF
        RET

; Drive address table
DRIVES:
        DEFW    DRIVE2
        DEFW    DRIVE3
        DEFW    DRIVE7
        DEFW    0               ; End of table marker

; ; Bad sector information
; BADSECT:    DEFB    #00,#00,#00,#FF

DRIVE2:
DRIVE3:

; Seek to track
; Input: A = track number
SEEK:
        PUSH    AF
        LD      (BADSECT),A     ; Store track number
        PUSH    BC
        PUSH    DE
        PUSH    HL

DRIVE7:
        ;LD      E,0             ; Drive number (patched)
        ; RST     #18             ; RST 3: FAR CALL: Calls ROM anywhere
        ;                         ; followed by 2 byte address, 
        ;                         ; at that address:
        ;                         ; bytes 0 and 1 hold the address;
        ;                         ; byte 2 holds the ROM select address
        ;                         ; &00 to &FB-- select the given upper ROM,
        ;                         ; enable the upper ROM and disable the lower ROM

        ; DEFW    SEEKCOM         ; Address of seek command

        ;; E = drive
        ;; D = track
        ld e, 0
        ld d, 0
        call MoveTrack

        POP     HL
        POP     DE
        POP     BC
        POP     AF

        RET

;SEEKCOM:
;        DEFW    #C763           ; Firmware seek function
;        DEFB    #07             ; Select ROM 7 AMSDOS

; Send a command byte to the FDC
FDCOUT:
        LD      BC,#FB7E        ; FDC control port
        PUSH    AF

WAITRQM:
        IN      A,(C)           ; Read FDC status
        ADD     A,A             ; Check ready bit
        JR      NC,WAITRQM      ; Loop until ready
        ADD     A,A             ; Check direction bit
        JR      NC,OKOUT        ; If output direction OK
        POP     AF              ; Otherwise error
        RET

OKOUT:
        POP     AF              ; Get command byte
        INC     C               ; Point to FDC data port
        OUT     (C),A           ; Send byte
        DEC     C               ; Back to status port
        LD      A,5             ; Short delay

FDCTEMP:
        DEC     A
        NOP
        JR      NZ,FDCTEMP
        RET

; ; Result timing counter
; RESTIME:    DEFS    2

; Get command result from FDC
RESULT:
        PUSH    HL
        LD      HL,(RESTIME)    ; Get current timing value

WAITRES:
        IN      A,(C)           ; Read FDC status
        INC     HL              ; Count cycles
        CP      #C0             ; Check if result ready
        JR      C,WAITRES       ; If not, keep waiting
        LD      (RESTIME),HL    ; Save updated time
        LD      HL,RESBUF       ; Point to result buffer

RESAGAIN:
        IN      A,(C)           ; Read status again
        CP      #C0             ; Check if still ready
        JR      C,RESAGAIN      ; If not, wait
        INC     C               ; Point to data port
        IN      A,(C)           ; Read result byte
        DEC     C               ; Back to status port
        LD      (HL),A          ; Store byte
        INC     HL              ; Next buffer position
        LD      A,#05           ; Short delay

RESTEMP:
        DEC     A
        JR      NZ,RESTEMP
        IN      A,(C)           ; Check if more bytes
        AND     #10             ; Test busy flag
        JR      NZ,RESAGAIN     ; If busy, get more bytes
        POP     HL
        RET


; mod 10
; Input: A = value to divide by 10
; Output: A = quotient, B = remainder
; Destroyed: BC
Mod10:
   ld bc,05A0h               ; Load B=5, C=A0
Loop:
        sub c                ; Subtract C from A
        jr nc,A_geq_C           ; If no carry (A>=C), skip
        add a,c              ; Add C back to A (only executed if carry)
A_geq_C:
        srl c                ; Shift C right (divide by 2)
        djnz Loop            ; Decrement B and loop if not zero
        ret                  ; Return

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

Div32by16Fast:

; Macro for division with remainder
div_r: MACRO
     SLA   E
     RL    D
     ADC   HL, HL

     LD    A, L
     ADD   A, C
     LD    A, H
     ADC   A, B
     JR    NC, .t2

     ADD   HL, BC
     INC   DE
.t2:
     ENDM

; Macro for division without remainder
div_e: MACRO
     SLA   E
     RL    D
     ADC   HL, HL
     JR    C, .t1

     LD    A, L
     ADD   A, C
     LD    A, H
     ADC   A, B
     JR    NC, .t2
.t1:
     ADD   HL, BC
     INC   DE
.t2:
     ENDM

; Main division procedure
div32x16:
     DEC   BC
     LD    A, B
     CPL 
     LD    B, A
     LD    A, C
     CPL 
     LD    C, A
     ADD   A, L
     LD    A, B
     ADC   A, H
     JR    NC, .DIV16

     PUSH  DE
     EX    DE, HL
     LD    HL, 0000h
     CALL  .DIV32R
     EX    DE, HL
     EX    (SP), HL
     EX    DE, HL
     CALL  .DIV32E
     POP   BC
     RET
     
.DIV16:
     CALL  .DIV32E
     LD    BC, 0000h
     RET
     
.DIV32R:  ; DE = HLDE/(-BC), HL = HLDE%(-BC), -BC < $8000
     CALL  $+3
     div_r
     div_r
     div_r
     div_r
     div_r
     div_r
     div_r
     div_r
     RET
     
.DIV32E:  ; DE = HLDE/(-BC), HL = HLDE%(-BC)
     CALL  $+3
     div_e
     div_e
     div_e
     div_e
     div_e
     div_e
     div_e
     div_e
     RET

; 32-bit / 16-bit division routine
; Input:  
;   HLDE = Dividend (32-bit)
;   BC   = Divisor  (16-bit)
; Output:
;   HL   = Quotient (16-bit)
;   DE   = Remainder (16-bit)
; Clobbers: AF, BC, HL, DE
;
DIV32_16:
    xor  a          ; Clear carry flag
    ld   b, 16      ; We will perform 16 shifts

DIV_LOOP:
    sla  e          ; Shift left HLDE
    rl   d
    rl   l
    rl   h

    push hl         ; Save HL
    push de         ; Save DE

    sbc  hl, bc     ; Try to subtract divisor

    jr   nc, NO_UNDERFLOW
    pop  de         ; Restore DE (undo subtraction)
    pop  hl         ; Restore HL
    jr   NEXT_ITER

NO_UNDERFLOW:
    pop  de         ; Restore DE
    pop  hl         ; Restore HL
    scf             ; Set carry (quotient bit)
    
NEXT_ITER:
    rl   e          ; Shift carry into quotient bit
    rl   d
    rl   l
    rl   h

    djnz DIV_LOOP   ; Repeat 16 times

    ret             ; HL = Quotient, DE = Remainder

 INCLUDE "seek.asm"

 ENDMODULE