'All-purpose Joystick routines
'Source Code by Dmitry Brant (me@dmitrybrant.com)
'Please give credit where credit is due.
'Sorry about the lack of documentation, but I'm just not the
'documenting type... All these functions should be self-
'explanatory. If you think they're not, email me.
'
'These functions are provided 'AS-IS'. I can't be held
'responsible for any damage, physical or phychological, caused
'by the use or misuse of this source code. Thanks!

DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
		   'JOYSTICK FUNCTIONS
DECLARE SUB ClearJ ()                    'Clear joystick button buffer
DECLARE FUNCTION JoyButtons% ()          'See which buttons are pressed
DECLARE SUB JoyCoords (x, y, t)          'Get coordinates
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType

'Demo
SCREEN 13: CLS
PRINT "Joystick Demo by Dmitry Brant"
LOCATE 10, 10: PRINT "Coordinates: "
LOCATE 12, 10: PRINT "Buttons: "
LOCATE 14, 10: PRINT "Throttle: "

DO UNTIL INKEY$ <> ""
	JoyCoords jx, jy, jt
	LOCATE 10, 23: PRINT jx; jy; "   "
	LOCATE 12, 19
	b = JoyButtons AND &HF0
	IF NOT b AND &H10 THEN PRINT "1";
	IF NOT b AND &H20 THEN PRINT " 2";
	IF NOT b AND &H40 THEN PRINT " 3";
	IF NOT b AND &H80 THEN PRINT " 4";
	PRINT "          "
	LOCATE 14, 20: PRINT jt
LOOP
SYSTEM

SUB ClearJ
Repeat:
IF STRIG(0) OR STRIG(1) OR STRIG(2) OR STRIG(3) OR STRIG(4) OR STRIG(5) THEN GOTO Repeat
END SUB

FUNCTION JoyButtons
JoyButtons = INP(&H200)
END FUNCTION

SUB JoyCoords (x, y, t)
inregs.ax = &H8400
inregs.dx = 1
CALL Interrupt(&H15, inregs, outregs)
x = outregs.ax
y = outregs.bx
t = outregs.dx
END SUB

