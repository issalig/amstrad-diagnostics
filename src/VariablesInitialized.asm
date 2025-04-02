

MenuItemCount: db MENU_ITEM_COUNT
SelectedMenuItem: db 0


 STRUCT MenuItem
Text WORD
Function WORD
KeybOffset BYTE
BitMask BYTE
 ENDS

/*

  0  1  2   3  4  5  6  7  bitmask on menuitem

Dl
  1  2  Es  Q  Tb A  Ca Z			8 
  4  3  E   W  S  D  C  X			7
  6  5  R   T  G  F  B  V			6
  8  7  U   Y  H  J  N  Space		5
  0  9  O   I  L  K  M  ,			4   KeybOffset
  ^  -  @   P  ;  :  /  .			3
  Cl [  En  ]  4  Sh \  Ct			2
  Lf Cp 7   8  5  1  2  0			1
  Up Rg Dw  9  6  3  En .			0
*/

MenuTable:
MenuItemLowerRAMTest:
;            Text, Function, KeybOffset, BitMask
	MenuItem TxtLowerRAMTest, LowerRAMTestSelected, 4, %00010000  	;; L
MenuItemUpperRAMTest:
	MenuItem TxtUpperRAMTest, UpperRAMTestSelected, 5, %00000100	;; U
MenuItemROMTest:
	MenuItem TxtROMTest, ROMTestSelected, 6, %00000100				;; R
MenuItemKeyboardTest:
	MenuItem TxtKeyboardTest, KeyboardTestSelected, 4, %00100000	;; K
MenuItemSoakTest:
	MenuItem TxtSoakTest, SoakTestSelected, 7, %00010000			;; S

MenuItemSoundTest:
	MenuItem TxtSoundTest, SoundTestSelected, 6, %00001000			;; T

MenuItemTapeTest:
	MenuItem TxtTapeTest, TapeTestSelected,  3, %00001000			;; P	

MenuItemDiskTest:
	MenuItem TxtDiskTest, DiskTestSelected, 7, %00100000			;; D

MenuTableEnd:

MENU_ITEM_SIZE EQU MenuItemUpperRAMTest-MenuItemLowerRAMTest
MENU_ITEM_COUNT equ ($-MenuTable)/MENU_ITEM_SIZE


TESTRESULT_UNTESTED 	EQU 0
TESTRESULT_PASSED 	EQU 1
TESTRESULT_FAILED 	EQU 2
TESTRESULT_ABORTED 	EQU 3
TESTRESULT_NOTAVAILABLE EQU 4


TestResultTable:
;; 0 - status
TestResultTableLowerRAM:
	db TESTRESULT_PASSED
TestResultTableUpperRAM:
	db TESTRESULT_UNTESTED
 IFDEF ROM_CHECK
TestResultTableLowerROM:
	db TESTRESULT_UNTESTED
TestResultTableUpperROM:
	db TESTRESULT_UNTESTED
 ELSE
TestResultTableLowerROM:
	db TESTRESULT_NOTAVAILABLE
TestResultTableUpperROM:
	db TESTRESULT_NOTAVAILABLE
 ENDIF
TestResultTableKeyboard:
	db TESTRESULT_UNTESTED
TestResultTableJoystick:
	db TESTRESULT_UNTESTED

