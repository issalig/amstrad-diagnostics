
  DEFINE SOUND_DEFAULT_VOLUME 12
  DEFINE CHANNEL_A_Y_POS 6

SoundTest:
	;; Sound test
	call  SoundSetUpScreen
	call	SetDefaultColors

	;; PSG detection (register #1 unused bits)
	ld    a, #01
	ld    l, #1F
	call  AYRegWriteByte

	ld    a, #01
	call  AYRegReadByte
	ld    hl, TxtYM
	cp    #1F
	jr    z, YM_PSG_detected

	;; AY-3 PSG detected
	ld    hl, TxtAY3

YM_PSG_detected:

	call  PrintString
	ld    hl, TxtChipDetected
	call  PrintString
	call  NewLine
	call  NewLine

	;; Select note for A channel
	ld    a, #00
	ld    hl, #0238
	call  AYRegWriteWord

	;; Select note for B channel
	ld    a, #02
	ld    hl, #01DD
	call  AYRegWriteWord

	;; Select note for C channel
	ld    a, #04
	ld    hl, #017B
	call  AYRegWriteWord

	;; Select volume for A channel
	ld    a, #08
	ld    l, SOUND_DEFAULT_VOLUME
	call  AYRegWriteByte

	;; Select volume for B channel
	ld    a, #09
	call  AYRegWriteByte

	;; Select volume for C channel
	ld    a, #0A
	call  AYRegWriteByte

	; Perform noise test
	ld    a, CHANNEL_A_Y_POS
	ld    hl, MixerDataTableNoise
	ld    de, TxtNoise
	call  SoundTestAllChannels

	; Perform sound test
	ld    a, CHANNEL_A_Y_POS + 6
	ld    hl, MixerDataTableSound
	ld    de, TxtSound
	call  SoundTestAllChannels

	;; Test complete
	call  NewLine
	ld    hl, TxtSoundTestCompleted
	call  PrintString
	call  NewLine
	call  NewLine

	;; Press any key for main menu
	ld 	 hl, TxtAnyKeyMainMenu
	jp   PrintString



AYMixerGetData:
	ld    hl,(MixerDataPtr)
	ld    a,(hl)
	inc   hl
	ld    (MixerDataPtr),hl
	ld    l,a
	ret

AYRegWriteWord:
	call  AYRegWriteByte
	inc   a
	ld    l,h

AYRegWriteByte:
	ld    b, #F4 ;;<< register
	ld    c,a
	out   (c),c
	ld    bc, #F6c0 ;;<<<<<< select register
	out   (c),c
	ld    bc,#F600
	out   (c),c ;; << back to inactive

	ld    b, #F4 ;; << data
	ld    c,l
	out   (c),c
	ld    bc, #F680 ;; << write data to register
	out   (c),c
	ld    bc, #F600
	out   (c),c
	ret


AYRegReadByte:
	ld    b, #F4 ;;<< register
	ld    c,a
	out   (c),c
	ld    bc, #F6c0 ;;<<<<<< select register
	out   (c),c
	ld    bc, #F600
	out   (c),c ;; << back to inactive

	;; set PPI mode
	ld    b, #F7 ;; PPI control port
	ld    c, #92 ;; mode 0 ports A,B@input C@output)
	out   (c),c

	ld    bc, #F640 ;; << read data to register
	out   (c),c

	ld    b, #F4 ;; << data
	in    a,(c)

	;; set PPI mode
	ld    b, #F7 ;; PPI control port
	ld    c, #82 ;; mode 0 ports B@input A,C@output
	out   (c),c

	ld    bc, #F600
	out   (c),c ;; << back to inactive
	ret


Silence:
	push  bc
	ld    bc, #F000
.loop:
	dec   bc
	ld    a,b
	or    c
	jr    nz, .loop
	pop   bc
	djnz  Silence
	ret


SoundSetUpScreen:
	ld    d, 0
	call  ClearScreen
	ld    a, 4
	call  SetBorderColor

	ld 	  hl, TxtSoundTestTitle
	ld 	  d, (ScreenCharsWidth - TxtTitleLen - TxtSoundTestTitleLen)/2
	call  PrintTitleBanner

	ld 	  hl, #0002
	ld 	  (TxtCoords),hl
	call  SetDefaultColors
	ret


SoundTestAllChannels:
	; Reset Y-pos
	ld    (SoundTestY),a

	; Store mixer data table pointer
	ld    (MixerDataPtr),hl

	; Print header
	push  de
	ld 	  hl, TxtCheckingPSG
	call  PrintString
	pop   hl

	; Print test type
	call  PrintString

	ld 	  hl, TxtChannels
	call  PrintString
	call  NewLine
	call  NewLine

	ld 	  d, 1		;; D = Channel number
.channelLoop:
	push 	de

	ld 	hl,TxtSoundChannel
	call  PrintString

	ld    a, 'A'-1
	add   a,d
	call	PrintChar

	ld 	  hl, TxtDashes
	call  PrintString
	call  NewLine

	pop	  de

	inc   d
	ld	  a, d
	and   3
	jr	  nz, .channelLoop

	; 	Channel A ON
	ld    a,(SoundTestY)
	ld    e,a
	ld 	  d, #0A
	ld 	  (TxtCoords),de
	ld    hl,TxtSoundON
	call  PrintString

	;; Activate A channel
	call  AYMixerGetData
	ld    a, #07
	call  AYRegWriteByte

	ld    b, 4
	call  Silence

	; 	Channel B ON
	ld    a,(SoundTestY)
	inc   a
	ld    e,a
	ld 	  (TxtCoords),de
	ld    hl, TxtSoundON
	call  PrintString

	;; Activate A&B channels
	call  AYMixerGetData
	ld    a, #07
	call  AYRegWriteByte

	ld    b, 4
	call  Silence

	; 	Channel C ON
	ld    a,(SoundTestY)
	add   a,2
	ld    e,a
	ld 	  (TxtCoords),de
	ld    hl, TxtSoundON
	call  PrintString

	;; Activate A,B&C channels
	call  AYMixerGetData
	ld    a, #07
	call  AYRegWriteByte

	ld    b, 4
	call  Silence

	;; Deactivate all channels
	ld    a, #07
	ld    l, %10111111
	call  AYRegWriteByte

	ld    b, 1
	call  Silence

	;; Muted
	ld    a,(SoundTestY)
	ld    e,a
	ld    hl, TxtSoundMuted
	ld    b, 3

.muteLoop
	push  hl
	ld 	  (TxtCoords),de
	call  PrintString
	pop   hl
	inc   e
	djnz  .muteLoop

	call  NewLine
	jp    NewLine


MixerDataTableNoise:
  db     %10110111,%10100111,%10000111

MixerDataTableSound:
  db     %10111110,%10111100,%10111000

TxtAY3:
  db    'AY-3-891x',0
TxtChannels:
  db    ' CHANNELS...',0
TxtCheckingPSG:
  db    'CHECKING PSG ',0
TxtChipDetected:
  db    ' PSG DETECTED',0
TxtDashes:
  db    ' --',0
TxtNoise:
  db    'NOISE',0
TxtSound:
  db    'SOUND',0
TxtSoundChannel:
  db    'CHANNEL ',0
TxtSoundMuted:
  db    '[MUTED]  ', 0
TxtSoundON:
  db    '[PLAYING]', 0
TxtSoundTestCompleted:
  db    'TEST COMPLETED', 0
TxtSoundTestTitle:
  db    ' - SOUND TEST',0
TxtSoundTestTitleLen equ   $-TxtSoundTestTitle-1
TxtYM:
  db    'YM2149',0
