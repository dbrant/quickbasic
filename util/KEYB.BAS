DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
		   'KEYBOARD FUNCTIONS
DECLARE SUB ClearKeyBuffer ()            'Clear the keyboard buffer
DECLARE FUNCTION KbScan% ()              'Get the scancode of the key pressed
DECLARE FUNCTION KbHit% ()               'Check if any key is pressed
DECLARE FUNCTION KbStatus% ()            'Get keyboard status
DECLARE SUB KeybLights (StatusBits)      'Play with keyboard lights
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inregsx AS RegTypex
COMMON SHARED outregsx AS RegTypex

SUB ClearKeyBuffer
WHILE INKEY$ <> "": WEND
END SUB

FUNCTION KbHit
j = INP(&H60)
IF (j AND 128) = 0 THEN
KbHit = 1
ELSE
KbHit = 0
END IF
END FUNCTION

FUNCTION KbScan
KbScan = INP(&H60)
END FUNCTION

FUNCTION KbStatus
inregs.ax = &H1200
CALL interrupt(&H16, inregs, outregs)
KbStatus = outregs.ax
END FUNCTION

SUB KeybLights (StatusBits)
OUT &H60, &HED
OUT &H60, StatusBits
END SUB

