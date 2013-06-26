'All-purpose Mouse Routines
'Source code by Dmitry Brant (me@dmitrybrant.com)
'This code is distributed as freeware.
'If you will use it in your programs, please
'give me credit where it is due...
'Sorry about the lack of documentation, but I'm just not the
'documenting type... All these functions should be self-
'explanatory. If you think they're not, email me.
'
'These functions are provided 'AS-IS'. I can't be held
'responsible for any damage, physical or phychological, caused
'by the use or misuse of this source code. Thanks!

DEFINT A-Z
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
		   'MOUSE FUNCTIONS
DECLARE SUB ResetMouse ()                'Reset the mouse driver
DECLARE SUB MouseCursor (Flag1%)         'Turn the mouse cursor on or off
DECLARE SUB SetMouseSens (xs%, ys%)      'Set mouse sensitivity
DECLARE FUNCTION MouseX% ()              'Get mouse X coordinate
DECLARE FUNCTION MouseY% ()              'Get mouse Y coordinate
DECLARE FUNCTION MouseButtons% ()        'See which mouse buttons are pressed
DECLARE SUB SetMousePos (x1%, y1%)       'Set the mouse position
DECLARE SUB SetMouseRange (x1%, y1%, x2%, y2%) 'Set the mouse range
DECLARE FUNCTION MouseIsWithin% (x1%, y1%, x2%, y2%) 'Test mouse location
DECLARE SUB SetMouseBitmap (BMPArray() AS INTEGER, HSRow, HSCol)
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inRegsx AS RegTypeX
COMMON SHARED outregsx AS RegTypeX

'Demo
SCREEN 13: CLS
ResetMouse
PRINT "Mouse Routine Demonstration"
PRINT "By Dmitry Brant."
LOCATE 24, 1: PRINT "Hit a key to continue";
MouseCursor 1
DO UNTIL INKEY$ <> ""
	LOCATE 12, 16: PRINT MouseX / 2; MouseY;
	B = MouseButtons
	IF B = 0 THEN
		PRINT "        "
	ELSEIF B = 1 THEN
		PRINT "LEFT    "
	ELSEIF B = 2 THEN
		PRINT "RIGHT   "
	ELSEIF B = 3 THEN
		PRINT "BOTH    "
	END IF
LOOP

MouseCursor 0
CLS
PRINT "Horizontal sensitivity: high"
PRINT "Vertical sensitivity: low"
SetMouseSens 5, 80
MouseCursor 1
DO: LOOP UNTIL INKEY$ <> ""

MouseCursor 0
CLS
PRINT "Horizontal sensitivity: low"
PRINT "Vertical sensitivity: high"
SetMouseSens 80, 5
MouseCursor 1
DO: LOOP UNTIL INKEY$ <> ""

MouseCursor 0
CLS
LOCATE 3, 7: PRINT "Mouse is contained inside box"
SetMouseSens 10, 10
LINE (40, 40)-(280, 160), 9, B
LINE (140, 90)-(180, 110), 14, B
SetMouseRange 80, 40, 560, 160
MouseCursor 1
DO UNTIL INKEY$ <> ""
	IF MouseIsWithin(280, 90, 360, 110) THEN
		LOCATE 20, 7: PRINT "Mouse is within smaller box"
	ELSE
		LOCATE 20, 7: PRINT "                           "
	END IF
LOOP

FOR j = 1 TO 5
MouseCursor 0
CLS
PRINT "Custom cursor"; j
DIM cursor(31)
SELECT CASE j
CASE 1: RESTORE PresetCursor1
CASE 2: RESTORE PresetCursor2
CASE 3: RESTORE PresetCursor3
CASE 4: RESTORE PresetCursor4
CASE 5: RESTORE PresetCursor5
END SELECT
FOR i = 0 TO 31
	READ cursor(i)
NEXT i
SetMouseBitmap cursor(), 0, 0
MouseCursor 1
DO: LOOP UNTIL INKEY$ <> ""
NEXT j
MouseCursor 0
ResetMouse

LOCATE 1, 1: PRINT "All done. Enjoy.   --Dmitry Brant"
SYSTEM

'Here are five predefined custom mouse cursors
PresetCursor1:    'Happy face
DATA &HFFFF,&HF81F,&HE007,&HC003,&H8001,&H8001,&H0000,&H0000,&H0000,&H0000,&H8001,&H8001,&HC003,&HE007,&HF81F,&HFFFF
DATA &H0000,&H0000,&H07E0,&H1FF8,&H3FFC,&H33CC,&H73CE,&H7FFE,&H7FFE,&H6FF6,&H33CC,&H3C3C,&H1FF8,&H07E0,&H0000,&H0000
PresetCursor2:    'Star of David  (don't ask)
DATA &HFF7F,&HFE3F,&HFE3F,&HC001,&HC001,&HE003,&HE003,&HF007,&HF007,&HE003,&HE003,&HC001,&HC001,&HFE3F,&HFE3F,&HFF7F
DATA &H0000,&H0080,&H0080,&H0000,&H1DDC,&H0BE8,&H0BE8,&H07F0,&H07F0,&H0BE8,&H0BE8,&H1DDC,&H0000,&H0080,&H0080,&H0000
PresetCursor3:    'Hand
DATA &HE7FF,&HC3FF,&HC3FF,&HC3FF,&HC3FF,&HC3FF,&HC093,&HC001,&H8001,&H0001,&H0001,&H0001,&H0003,&H8003,&HC007,&HFFFF
DATA &H0000,&H1800,&H1800,&H1800,&H1800,&H1800,&H1800,&H1B6C,&H1B6C,&H5FFC,&H5FFC,&H5FFC,&H7FF8,&H3FF8,&H0000,&H0000
PresetCursor4:    'Pointer
DATA &H7FFF,&H3FFF,&H1FFF,&H0FFF,&H07FF,&H03FF,&H01FF,&H00FF,&H007F,&H003F,&H001F,&H000F,&H0607,&H0F83,&H1FE1,&H3FF8
DATA &H0000,&H0000,&H4000,&H6000,&H7000,&H7800,&H7C00,&H7E00,&H7F00,&H7F80,&H7FC0,&H79E0,&H7070,&H6018,&H4004,&H0000
PresetCursor5:    'Another one
DATA &H9FFF,&H8FFF,&H87FF,&H83FF,&H81FF,&H80FF,&H807F,&H803F,&H801F,&H800F,&H80FF,&H887F,&H9879,&HFC36,&HFC36,&HFE39
DATA &H0000,&H2000,&H3000,&H3800,&H3C00,&H3E00,&H3F00,&H3F80,&H3FC0,&H3E00,&H3600,&H2300,&H0300,&H0186,&H0186,&H0000

FUNCTION MouseButtons
inregs.ax = 3
CALL INTERRUPT(&H33, inregs, outregs)
MouseButtons = outregs.bx
END FUNCTION

SUB MouseCursor (Flag1%)
inregs.ax = Flag1%
CALL INTERRUPT(&H33, inregs, outregs)
END SUB

FUNCTION MouseIsWithin (x1, y1, x2, y2)
p1 = 0: p2 = 0
g0 = MouseX: h0 = MouseY
IF x1 > x2 THEN
  IF g0 >= x2 AND g0 <= x1 THEN p1 = 1
ELSEIF x2 > x1 THEN
  IF g0 >= x1 AND g0 <= x2 THEN p1 = 1
END IF
IF y1 > y2 THEN
  IF h0 >= y2 AND h0 <= y1 THEN p2 = 1
ELSEIF y2 > y1 THEN
  IF h0 >= y1 AND h0 <= y2 THEN p2 = 1
END IF
IF p1 AND p2 THEN MouseIsWithin = 1
END FUNCTION

FUNCTION MouseX
inregs.ax = 3
CALL INTERRUPT(&H33, inregs, outregs)
MouseX = outregs.cx
END FUNCTION

FUNCTION MouseY
inregs.ax = 3
CALL INTERRUPT(&H33, inregs, outregs)
MouseY = outregs.dx
END FUNCTION

SUB ResetMouse
inregs.ax = 0
CALL INTERRUPT(&H33, inregs, outregs)
END SUB

SUB SetMouseBitmap (BMPArray() AS INTEGER, HSRow, HSCol)
inRegsx.ax = 9
inRegsx.bx = HSRow
inRegsx.cx = HSCol
inRegsx.es = VARSEG(BMPArray(0))
inRegsx.dx = VARPTR(BMPArray(0))
CALL INTERRUPTX(&H33, inRegsx, outregsx)
END SUB

SUB SetMousePos (x1, y1)
inregs.ax = 4
inregs.cx = x1
inregs.dx = y1
CALL INTERRUPT(&H33, inregs, outregs)
END SUB

SUB SetMouseRange (x1, y1, x2, y2)
inregs.ax = 7
inregs.cx = x1
inregs.dx = x2
CALL INTERRUPT(&H33, inregs, outregs)
inregs.ax = 8
inregs.cx = y1
inregs.dx = y2
CALL INTERRUPT(&H33, inregs, outregs)
END SUB

SUB SetMouseSens (xs, ys)
inregs.ax = 15
inregs.cx = xs
inregs.dx = ys
CALL INTERRUPT(&H33, inregs, outregs)
END SUB

