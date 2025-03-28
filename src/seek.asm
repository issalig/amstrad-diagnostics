; Key Functions Present
; Main Track Movement Function (c763-c7c6)
; -  Complete and properly extracted

; FDC Command and Result Processing
; - fdc: send command byte (c95c-c975)
; - fdc: get result phase (c91c-c946)
; - get result phase helper function (c7c7-c7dd)
; - sense interrupt status handler (c8f9-c918)

; Drive Management
; - start motor and save registers (c976-c9ac)
; - turn off drive motor (c9e8-c9f3)
; - Motor control and ticker functions (c9cd-c9f3)

; XDPB Parameter Handling
; - get address of XDPB parameter (ca63-ca71)
; - get XDPB parameter by index (ca5c-ca62)

; Error Handling
; - Error handler (c9ad-c9cc)
; - display message function (caeb-cb12)
; - Character and message output functions (cb13-cb83)

; Utility Functions
; - delay function (c7e0-c7ea)
; - clear fdc interrupt (c947-c958)

; Variables

; Address	    Variable Name	Purpose	                            Initialization
; -----------------------------------------------------------------------------------------------
; $be42	        XDPB Address	Pointer to XDPB data for drives	    Initialized at ca0a
; $be49	        Step Rate	    FDC step rate in 1ms units	        Initialized at c615-c62d
; $be4a	        Head Settle	    Head settle time in 1ms units	    Initialized at c615-c62d
; $be4c-$be53	Result Buffer	FDC result phase data	            Cleared at c5e0-c5e3
; $be5f	         Motor Status	Drive motor on/off flag	            Set in c9ef-c9f3
; $be64	        Stack Pointer	Saved stack pointer	                Set in c97c
; $be66	        Retry Count	    Number of retries for operations	Set in c603-c60c
; $be76	        HL Storage	    Saved HL value	                    Set in c976

; XDPB
; Offset	Purpose	                        Initialization
; -----------------------------------------------------------------------------------------------
; 0-1	    SPT (Sectors Per Track)	        XDPB template at ca43
; 15 ($0f)	First Sector ID	                XDPB template at ca43
; 16 ($10)	Sectors Per Track	            XDPB template at ca43
; 17 ($11)	Gap Length (read/write)	        XDPB template at ca43
; 22 ($16)	Current Track	                Set in c78f-c792 and c7b0
; 23 ($17)	Aligned Flag	                Set in c71d and checked at c772-c774


; Initialization
; Basic Memory Clearing:
; - At c5dd-c5e3, this 61-byte area is cleared to zeros
; XDPB Pointers Setup:
; - At ca06-ca09, the XDPB address is set
; - At ca0a-ca0e, a related pointer is stored
; Disk Parameter Setup:
; - At c60d-c613, disk parameters are loaded from the template at c5d4
; Motor Control Setup:
; - At c5f8-c5fe, the event block for motor control is initialized

; Memory Area $BE40-$BE7C
; Address	    Name	Purpose	Initialization
; $BE40-$BE41	Unknown	            Holds a pointer initialized at ca0e	        Initialized at ca0a-ca0e
; $BE42-$BE43	XDPB Address	    Pointer to XDPB data for drives         	Initialized at ca06-ca09
; $BE44-$BE45	Motor On Time	    Motor on time in 20ms units	                Loaded from c5d4 at c60d-c613
; $BE46-$BE47	Motor Off Time	    Motor off time in 20ms units	            Loaded from c5d4 at c60d-c613
; $BE48	        Write Off Time	    Write off time in 10ms units            	Loaded from c5d4 at c60d-c613
; $BE49	        Step Rate	        FDC step rate in 1ms units	                Loaded from c5d4 at c60d-c613
; $BE4A	        Head Settle	        Head settle time in 1ms units	            Loaded from c5d4 at c60d-c613
; $BE4B	        Number of Bytes	    Number of bytes received in result phase	Set in c941
; $BE4C-$BE53	Result Buffer	    FDC result phase data	                    Cleared at c5e0-c5e3
; $BE53	        Current Drive	    Current drive number	                    Set at c50b
; $BE54	        Current Track	    Track number from CP/M functions	        Set at c525-c528
; $BE55	        Current Sector	    Sector number from CP/M functions	        Set at c52a-c52d
; $BE56-$BE57	Oper Track/Drive	Current track and drive for disk operations	Set at c880-c88f
; $BE58	        Curr Sector Index	Current sector index	                    Referenced at c8ae
; $BE59	        Buffer Control	    Control flag for sector buffer	            Set/cleared at c54d-c550, c816-c819
; $BE5A-$BE5D	Buffer Tracking	    Related to sector buffer management	        Used in c803-c815, c81c-c830
; $BE5E	        Buffer Status	    Sector buffer status	                    Set at c84c
; $BE5F	        Motor Status	    Drive motor on/off flag	                    Set in c9ef-c9f3
; $BE60-$BE61	DMA Address	        CP/M DMA address for disk transfers	        Set in c51a-c51e
; $BE62-$BE63	Buffer Address	    Address of buffer for disk operations	    Set at c66d
; $BE64-$BE65	Stack Pointer	    Saved stack pointer	                        Set in c97c
; $BE66	        Retry Count	        Number of retries for operations	        Set in c603-c60c
; $BE67-$BE6C	Motor Ticker	    Event block for motor control ticker	    Used at c9cd-c9d0
; $BE6D-$BE72	Event Block	        Event block for disk operations	            Referenced at c5f8
; $BE73	        Unknown	            Unknown purpose	                            -
; $BE74-$BE75	FDC Parameters	    Stores R parameter and FDC command code	    Set at c672
; $BE76-$BE77	HL Storage	        Saved HL value	                            Set in c976
; $BE78	        Message Flag	    Controls message display	                Set at ca72-ca79
; $BE79-$BE7C	Additional storage	Various purposes	                        -

; Info on ROM, RAM in CPC464_inside_out_Ein_Buch_fur_Programmierer(Winfried_HUSLIK)(acme).pdf

; $be40 to $be7c  (3d length)

; Memory Area $BE40-$BE7C (61 bytes)
; v_be40:         .ds     $003d
; RetryCount_be66     equ v_be40+$26 ; $be66
; XDPBPtr_be42       equ v_be40+$02 ; $be42
; HeadSettle_be4a    equ v_be40+$0a ; $be4a
; StepRate_be49      equ v_be40+$09 ; $be49
; ResultBuffer_be4c  equ v_be40+$0c ; $be4c
; MotorStatus_be5f   equ v_be40+$1f ; $be5f
; StackPtr_be64      equ v_be40+$24 ; $be64
; HLStorage_be76     equ v_be40+$36 ; $be76
; MotorOnTime_be44   equ v_be40+$04 ; $be44
; MotorOffTime_be46  equ v_be40+$06 ; $be46

; FDC ports
FDC_STATUS      equ $fb7e
FDC_DATA        equ $fb7f
MOTOR_PORT      equ $fa7e

; FDC commands
FDC_RECAL       equ $07
FDC_SEEK        equ $0f
FDC_SENSE_INT   equ $08

;;=======================================================================
;; BIOS: MOVE TRACK - Simplified
;;
;; entry:
;; E = drive
;; D = track
;; 
;; exit:
;; Carry set on success

; MoveTrack:                               ; c763
;     ;; Simplified motor control
;     ld      bc,MOTOR_PORT               ; Motor port
;     ld      a,$01                       ; Motor on
;     out     (c),a                       ; Turn it on

;     ;; Skip head alignment check
;     push    de                          ; Save track/drive

;     ;; First recalibrate the drive
;     ld      bc,FDC_STATUS               ; FDC status port
;     ld      a,FDC_RECAL                 ; Recalibrate command
;     call    SimpleSendFDCCmd            ; Send command
;     ld      a,e                         ; Drive number
;     call    SimpleSendFDCCmd            ; Send parameter

;     ;; Simple delay for head settling
;     ld      a,$30                      
;     call    SimpleDelay

;     ;; Now seek to the requested track
;     ld      bc,FDC_STATUS               ; FDC status port
;     ld      a,FDC_SEEK                  ; Seek command
;     call    SimpleSendFDCCmd            ; Send command
;     ld      a,e                         ; Drive number
;     call    SimpleSendFDCCmd            ; Send parameter
;     ld      a,d                         ; Track number
;     call    SimpleSendFDCCmd            ; Send parameter

;     ;; Simple delay for head settling
;     ld      a,$30
;     call    SimpleDelay

;     ;; Check FDC status
;     ld      a,FDC_SENSE_INT             ; Sense interrupt command
;     call    SimpleSendFDCCmd            ; Send command
    
;     ;; Get status
;     ld      bc,FDC_STATUS               ; FDC status port
;     call    SimpleGetResult              ; Get result byte

;     pop     de                          ; Restore track/drive
;     scf                                 ; Set carry flag (success)
;     ret                                 ; Return



MoveTrack:                               
    ;; Initialize retry counter
    ld      b,$05                       ; 3 retries

RetryTrackMove:
    ;; Simplified motor control
    ld      bc,MOTOR_PORT               ; Motor port
    ld      a,$01                       ; Motor on
    out     (c),a                       ; Turn it on

    ;; Save track/drive
    push    de                          

    ;; First recalibrate the drive
    ld      bc,FDC_STATUS               ; FDC status port
    ld      a,FDC_RECAL                 ; Recalibrate command
    call    SimpleSendFDCCmd            ; Send command
    ld      a,e                         ; Drive number
    call    SimpleSendFDCCmd            ; Send parameter

    ;; Simple delay for head settling
    ld      a,$60                      
    call    SimpleDelay

    ;; Now seek to the requested track
    ld      bc,FDC_STATUS               ; FDC status port
    ld      a,FDC_SEEK                  ; Seek command
    call    SimpleSendFDCCmd            ; Send command
    ld      a,e                         ; Drive number
    call    SimpleSendFDCCmd            ; Send parameter
    ld      a,d                         ; Track number
    call    SimpleSendFDCCmd            ; Send parameter

    ;; Simple delay for head settling
    ld      a,$60 ; before was 30
    call    SimpleDelay

    ;; Check FDC status
    ld      a,FDC_SENSE_INT             ; Sense interrupt command
    call    SimpleSendFDCCmd            ; Send command
    
    ;; Get status
    ld      bc,FDC_STATUS               ; FDC status port
    call    SimpleGetResult              ; Get result byte

    ;; Check if operation was successful
    and     $C0                         ; Check status bits 6-7
    cp      $00                         ; Success?
    jr      z,MoveTrackSuccess          ; If success, exit

    ;; Operation failed, try again if retries left
    pop     de                          ; Restore track/drive
    djnz    RetryTrackMove              ; Decrement retry counter and jump if not zero
    
    ;; All retries failed
    xor     a                           ; Clear carry flag (error)
    ret                                 ; Return

MoveTrackSuccess:
    pop     de                          ; Restore track/drive
    scf                                 ; Set carry flag (success)
    ret                                 ; Return


;;==================================================================
;; Simple Delay
;;
;; A = delay value 

SimpleDelay:
    dec     a                           ; Decrement counter
    jr      nz,SimpleDelay              ; Loop until zero
    ret

;;==================================================================
;; Simple Send FDC Command
;;
;; BC = FDC status port
;; A = command byte

SimpleSendFDCCmd:
    push    af                          ; Save command byte
SimpleWaitReady:
    in      a,(c)                       ; Read status
    add     a,a                         ; Check bit 7 (ready)
    jr      nc,SimpleWaitReady          ; Loop until ready
    add     a,a                         ; Check bit 6 (direction)
    jr      c,SimpleCommandFail         ; Fail if wrong direction
    
    pop     af                          ; Get command byte
    inc     c                           ; FDC data port
    out     (c),a                       ; Send command
    dec     c                           ; Back to status port
    ret

SimpleCommandFail:
    pop     af                          ; Balance stack
    ret                                 ; Return (command failed)

;;==================================================================
;; Simple Get FDC Result
;;
;; BC = FDC status port
;; Returns first result byte in A

SimpleGetResult:
    in      a,(c)                       ; Read status
    cp      $c0                         ; Check if result ready
    jr      c,SimpleGetResult           ; Loop until ready
    
    inc     c                           ; FDC data port
    in      a,(c)                       ; Read result byte
    dec     c                           ; Back to status port
    ret                                 ; Return with result in A


;; === END OF SIMPLE FUNCTIONS ===

