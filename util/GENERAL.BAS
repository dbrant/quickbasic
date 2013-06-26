'Code by Dmitry Brant
'Please give credit where credit is due.
DEFINT A-Z                          'For faster program speed
DECLARE FUNCTION ReadBit% (Number&, Bit%)
DECLARE FUNCTION SoundCardPresent% ()  'Check for SoundCard
DECLARE FUNCTION ConvBase$ (num$, frombase%, tobase%) 'Convert numbers' bases
DECLARE FUNCTION Bin$ (De&, Bits%)     'Convert number to binary
DECLARE SUB WaitKey ()                 'Wait for keystroke
DECLARE SUB Rest (t!)                  'Rest t seconds
DECLARE SUB Center (text$, Row, MaxCols) 'Center text on the screen
DECLARE SUB CombSort (Array$())        'Sort string array using comb method
DECLARE SUB ShellSort (Array$())       'Sort string array using shell method

'Demo
CLS
PRINT "FFFFFFFF in base 16 equals:": PRINT
FOR i = 15 TO 2 STEP -1
	PRINT ConvBase("FFFFFFFF", 16, i); " in base"; i
NEXT i
FOR i = 17 TO 36
	LOCATE i - 16, 46: PRINT ConvBase("FFFFFFFF", 16, i); " in base"; i
NEXT i
SYSTEM

FUNCTION Bin$ (De&, Bits)
t = 0: B$ = ""
DO UNTIL 2 ^ t > De&
t = t + 1
LOOP
de2& = De&: t = t - 1
FOR le = t TO 0 STEP -1
  IF de2& - (2 ^ le) >= 0 THEN
	de2& = de2& - (2 ^ le)
	B$ = B$ + "1"
  ELSE
	B$ = B$ + "0"
  END IF
NEXT
IF LEN(B$) < Bits THEN
DO
 B$ = "0" + B$
LOOP UNTIL LEN(B$) = Bits
END IF
Bin$ = B$
END FUNCTION

SUB Center (text$, Row, MaxCols)
	LOCATE Row, (MaxCols / 2 + 1) - LEN(text$) / 2
	PRINT text$;
END SUB

'*    Sorts an array using the Comb sort algorithm.
SUB CombSort (Array$()) STATIC
   FirstEl = LBOUND(Array$)
   LastEl = UBOUND(Array$)
   Gap = LastEl
   DO
	  IF Gap * 8 / 11 > 1 THEN
	 Gap = Gap * 8 / 11
	  ELSE
	 Gap = 1
	  END IF
	  Switch = 0
	  FOR i = FirstEl TO LastEl - Gap
	 J = i + Gap
	 IF Array$(i) > Array$(J) THEN
		SWAP Array$(i), Array$(J)
		Switch = Switch + 1
	 END IF
	  NEXT i
   LOOP UNTIL Switch = 0 AND Gap = 1
END SUB

FUNCTION ConvBase$ (num$, frombase, tobase)
IF num$ = "" OR num$ = "0" OR num$ = " 0" THEN R$ = "0": GOTO Finish
value1# = 0
FOR i = 1 TO LEN(num$)
   fe$ = MID$(num$, i, 1)
	IF ASC(fe$) > 47 AND ASC(fe$) < 58 THEN
	   fi = ASC(fe$) - 48
	ELSEIF ASC(fe$) > 64 THEN
	   fi = ASC(fe$) - 55
	ELSE
	   fi = 0
	END IF
   value1# = value1# + fi * (frombase ^ (LEN(num$) - i))
NEXT i
R$ = ""
start = INT(LOG(value1#) / LOG(tobase))
DO
 b2 = 0
   DO
	b2 = b2 + 1
	IF INT(b2 * (tobase ^ start)) > INT(value1#) THEN EXIT DO
   LOOP
 b2 = b2 - 1
 IF b2 > 9 THEN
   t$ = CHR$(b2 + 55)
 ELSE
   t$ = CHR$(b2 + 48)
 END IF
 R$ = R$ + t$
 value1# = value1# - (b2 * (tobase ^ start))
 start = start - 1
LOOP UNTIL start < 0
Finish:
ConvBase$ = R$
END FUNCTION

FUNCTION ReadBit (Number&, Bit)
Value& = 2 ^ Bit
f = ((Number& AND Value&) = Value&)
ReadBit = f
END FUNCTION

SUB Rest (t!)
t! = t! / 1000
R! = TIMER
DO: LOOP UNTIL TIMER > R! + t!
END SUB

'*    Sorts an array using the shell sort algorithm.
SUB ShellSort (Array$()) STATIC
   FirstEl = LBOUND(Array$)
   LastEl = UBOUND(Array$)
   Span = LastEl \ 2
   DO WHILE Span > 0
	  Boundary = LastEl - Span
	  DO
	 Flag = 0
	 FOR i = FirstEl TO Boundary
		IF Array$(i) > Array$(i + Span) THEN
		   SWAP Array$(i), Array$(i + Span)
		   Flag = i
		END IF
	 NEXT i
	 Boundary = Flag - Span
	  LOOP WHILE Flag
	  Span = Span \ 2
   LOOP
END SUB

FUNCTION SoundCardPresent
OUT &H388, 4
OUT &H389, &H60
OUT &H388, 4
OUT &H389, &H80
R = INP(&H388)
OUT &H388, 2
OUT &H389, &HFF
OUT &H388, 4
OUT &H389, &H21
S = INP(&H388)
OUT &H388, 4
OUT &H389, &H60
OUT &H388, 4
OUT &H389, &H80
IF (R AND &HE0) = 0 AND (S AND &HE0) = &HC0 THEN SoundCardPresent = 1 ELSE SoundCardPresent = 0
END FUNCTION

SUB WaitKey
  DO: LOOP UNTIL INKEY$ <> ""
END SUB

