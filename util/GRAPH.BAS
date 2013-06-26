'Code by Dmitry Brant
'Please give credit where credit is due.
DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
		   'GRAPHICS FUNCTIONS
DECLARE SUB SetColor (ColNum%, Rd%, Gr%, Bl%) 'Set RGB of a color
DECLARE SUB ActivatePalette (DefArray() AS INTEGER)
DECLARE SUB RandomizePalette (DefArray() AS INTEGER)
DECLARE SUB PaletteFade (Array1() AS INTEGER, Array2() AS INTEGER, StartCol, EndCol)
DECLARE SUB CyclePalette (DefArray() AS INTEGER, StartCol, EndCol)
DECLARE SUB GetPalette (DefArray() AS INTEGER)
DECLARE SUB WritePAL (File$, DefArray() AS INTEGER)
DECLARE SUB OpenPAL (File$, DefArray() AS INTEGER)
DECLARE FUNCTION DrawPCX% (File$, OffsetX%, OffsetY%)
DECLARE FUNCTION DrawGIF% (File$, OffsetX%, OffsetY%)
DECLARE SUB Load8x8 (Rows)
DECLARE SUB Load8x14 (Rows)
DECLARE SUB Load8x16 (Rows)
DECLARE SUB LoadFont (File$)
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inregsx AS RegTypex
COMMON SHARED outregsx AS RegTypex
DIM SHARED pal1(0 TO 767) AS INTEGER
DIM SHARED pal2(0 TO 767) AS INTEGER

CLS
SCREEN 13
a = DrawGIF("scrn1.gif", 0, 0)
DO: LOOP UNTIL INKEY$ <> ""
GetPalette pal1()
FOR i = 0 TO 63
PaletteFade pal1(), pal2(), 0, 255
ActivatePalette pal1()
NEXT i
SYSTEM

SUB ActivatePalette (DefArray() AS INTEGER)
 FOR i = 0 TO 255
   SetColor i, DefArray(i), DefArray(i + 256), DefArray(i + 512)
 NEXT i
END SUB

SUB CyclePalette (DefArray() AS INTEGER, StartCol, EndCol)
IF StartCol >= 0 AND EndCol < 256 THEN
  IF EndCol > StartCol THEN
	FOR i = EndCol TO StartCol + 1 STEP -1
	  SWAP DefArray(i), DefArray(i - 1)
	  SWAP DefArray(i + 256), DefArray(i + 255)
	  SWAP DefArray(i + 512), DefArray(i + 511)
	NEXT i
  ELSEIF StartCol > EndCol THEN
	FOR i = EndCol TO StartCol - 1
	  SWAP DefArray(i), DefArray(i + 1)
	  SWAP DefArray(i + 256), DefArray(i + 257)
	  SWAP DefArray(i + 512), DefArray(i + 513)
	NEXT i
  END IF
END IF
END SUB

FUNCTION DrawGIF (File$, OffsetX, OffsetY)
DIM Prefix(4095), Suffix(4095), OutStack(4095), ShiftOut(8)
DIM Ybase AS LONG, Powersof2(11) AS LONG, WorkCode AS LONG
'DIM Ybase AS INTEGER, Powersof2(11) AS INTEGER, WorkCode AS INTEGER
FOR a = 0 TO 7: ShiftOut(8 - a) = 2 ^ a: NEXT
FOR a = 0 TO 11: Powersof2(a) = 2 ^ a: NEXT
f = FREEFILE
OPEN File$ FOR BINARY AS f
IF LOF(f) = 0 THEN E0 = 1: CLOSE #f: KILL File$: GOTO Done1
a$ = "      ": GET #f, , a$
IF a$ <> "GIF87a" THEN E0 = 2: GOTO Done1
GET #f, , TotalX
GET #f, , TotalY
GOSUB GetByte
NumColors = 2 ^ ((a AND 7) + 1)
NoPalette = (a AND 128) = 0
GOSUB GetByte
Background = a
GOSUB GetByte
IF a <> 0 THEN E0 = 3: GOTO Done1
IF NoPalette = 0 THEN p$ = SPACE$(NumColors * 3)
GET #f, , p$
DO
	GOSUB GetByte
	IF a = 44 THEN
	EXIT DO
	ELSEIF a <> 33 THEN
	E0 = 4: GOTO Done1
	END IF
	GOSUB GetByte
	DO: GOSUB GetByte: a$ = SPACE$(a): GET #f, , a$: LOOP UNTIL a = 0
LOOP
GET #f, , XStart: GET #f, , YStart: GET #f, , XLength: GET #f, , YLength
XEnd = XStart + XLength: YEnd = YStart + YLength: GOSUB GetByte
IF a AND 128 THEN E0 = 5: GOTO Done1
Interlaced = a AND 64: PassNumber = 0: PassStep = 8
GOSUB GetByte
ClearCode = 2 ^ a
EOSCode = ClearCode + 1
FirstCode = ClearCode + 2: NextCode = FirstCode
StartCodeSize = a + 1: CodeSize = StartCodeSize
StartMaxCode = 2 ^ (a + 1) - 1: MaxCode = StartMaxCode
BitsIn = 0: BlockSize = 0: BlockPointer = 1
X = XStart: Y = YStart: Ybase = Y * 320&
IF NoPalette = 0 THEN
	OUT &H3C7, 0: OUT &H3C8, 0
	FOR a = 1 TO NumColors * 3
	   OUT &H3C9, ASC(MID$(p$, a, 1)) \ 4
	NEXT a
END IF
'LINE (0, 0)-(319, 199), Background, BF
DO
	GOSUB GetCode
	IF Code <> EOSCode THEN
	IF Code = ClearCode THEN
		NextCode = FirstCode
		CodeSize = StartCodeSize
		MaxCode = StartMaxCode
		GOSUB GetCode
		CurCode = Code: LastCode = Code: LastPixel = Code
		IF X < 320 THEN PSET (X + OffsetX, Ybase / 320 + OffsetY), LastPixel
		X = X + 1: IF X = XEnd THEN GOSUB NextScanLine
	ELSE
		CurCode = Code: StackPointer = 0
		IF Code > NextCode THEN EXIT DO 'bad GIF if this happens
		IF Code = NextCode THEN
		CurCode = LastCode
		OutStack(StackPointer) = LastPixel
		StackPointer = StackPointer + 1
		END IF
		DO WHILE CurCode >= FirstCode
		OutStack(StackPointer) = Suffix(CurCode)
		StackPointer = StackPointer + 1
		CurCode = Prefix(CurCode)
		LOOP
		LastPixel = CurCode
		IF X < 320 THEN PSET (X + OffsetX, Ybase / 320 + OffsetY), LastPixel
		X = X + 1: IF X = XEnd THEN GOSUB NextScanLine
		FOR a = StackPointer - 1 TO 0 STEP -1
		IF X < 320 THEN PSET (X + OffsetX, Ybase / 320 + OffsetY), OutStack(a)
		X = X + 1: IF X = XEnd THEN GOSUB NextScanLine
		NEXT
		IF NextCode < 4096 THEN
		Prefix(NextCode) = LastCode
		Suffix(NextCode) = LastPixel
		NextCode = NextCode + 1
		IF NextCode > MaxCode AND CodeSize < 12 THEN
			CodeSize = CodeSize + 1
			MaxCode = MaxCode * 2 + 1
		END IF
		END IF
		LastCode = Code
	END IF
	END IF
LOOP UNTIL DoneFlag OR Code = EOSCode
GOTO Done1
GetByte: a$ = " ": GET #f, , a$: a = ASC(a$): RETURN
NextScanLine:
	IF Interlaced THEN
	Y = Y + PassStep
	IF Y >= YEnd THEN
		PassNumber = PassNumber + 1
		SELECT CASE PassNumber
		CASE 1: Y = 4: PassStep = 8
		CASE 2: Y = 2: PassStep = 4
		CASE 3: Y = 1: PassStep = 2
		END SELECT
	END IF
	ELSE
	Y = Y + 1
	END IF
	X = XStart: Ybase = Y * 320&: DoneFlag = Y > 199
RETURN
GetCode:
	IF BitsIn = 0 THEN GOSUB ReadBufferedByte: LastChar = a: BitsIn = 8
	WorkCode = LastChar \ ShiftOut(BitsIn)
	DO WHILE CodeSize > BitsIn
	GOSUB ReadBufferedByte: LastChar = a
	WorkCode = WorkCode OR LastChar * Powersof2(BitsIn)
	BitsIn = BitsIn + 8
	LOOP
	BitsIn = BitsIn - CodeSize
	Code = WorkCode AND MaxCode
RETURN
ReadBufferedByte:
	IF BlockPointer > BlockSize THEN
	GOSUB GetByte: BlockSize = a
	a$ = SPACE$(BlockSize): GET #f, , a$
	BlockPointer = 1
	END IF
	a = ASC(MID$(a$, BlockPointer, 1)): BlockPointer = BlockPointer + 1
RETURN
Done1:
CLOSE f
ERASE Prefix, Suffix, OutStack, ShiftOut, Powersof2
DrawGIF = E0
END FUNCTION

FUNCTION DrawPCX (File$, OffsetX, OffsetY)
f = FREEFILE: E0 = 0
OPEN File$ FOR BINARY AS f LEN = 11
Size# = LOF(f)
IF Size# = 0 THEN E0 = 1: KILL File$: GOTO Done
h$ = SPACE$(128)
GET #f, , h$
'The first position is a PCX 'signature'.
S$ = LEFT$(h$, 1)
IF S$ <> CHR$(10) THEN E0 = 2: GOTO Done
'The next header byte specifies the version.
'For 256 color it must be 5.
v$ = MID$(h$, 2, 1): Ver = ASC(v$)
IF Ver <> 0 AND Ver <> 2 AND Ver <> 3 AND Ver <> 5 THEN E0 = 3: GOTO Done
'The next header byte specifies the color bits.
'For 256 color it must be 8.
C$ = MID$(h$, 4, 1): ColorBits = ASC(C$)
IF ColorBits <> 1 AND ColorBits <> 8 THEN E0 = 4: GOTO Done
'The image size is contained in 4 bytes
'starting at position 9 of the header.
XR$ = MID$(h$, 9, 2)
XR1$ = LEFT$(XR$, 1): XR2$ = RIGHT$(XR$, 1)
XRes = ASC(XR1$) + ASC(XR2$) * 256 + 1
YR$ = MID$(h$, 11, 2)
YR1$ = LEFT$(YR$, 1): YR2$ = RIGHT$(YR$, 1)
YRes = ASC(YR1$) + ASC(YR2$) * 256 + 1
Bytes# = 128
Pointer = 0
LineNo = 0
SEEK #f, Size# - 768
a$ = " "
GET #f, , a$
a$ = " "
FOR X = 0 TO 255
	GET #f, , a$
	R = ASC(a$) / 4
	GET #f, , a$
	G = ASC(a$) / 4
	GET #f, , a$
	B = ASC(a$) / 4
	SetColor X, R, G, B
NEXT
SEEK #f, 128
DecodeGroup:
X$ = " "
IF EOF(f) THEN GOTO Done
GET #f, , X$: X = ASC(X$): Bytes# = Bytes# + 1
IF X < 193 THEN
	PSET (Pointer + OffsetX, LineNo + OffsetY), X
	Pointer = Pointer + 1
END IF
IF X > 192 THEN
	X = X - 192:
	X$ = " "
	GET #f, , X$: Y = ASC(X$): Bytes# = Bytes# + 1
	LINE (Pointer + OffsetX, LineNo + OffsetY)-(Pointer + X - 1 + OffsetX, LineNo + OffsetY), Y
	Pointer = Pointer + X
END IF
IF Pointer < XRes GOTO DecodeGroup
Pointer = 0
IF LineNo = YRes GOTO Done
LineNo = LineNo + 1
GOTO DecodeGroup
Done:
CLOSE f
DrawPCX = E0
END FUNCTION

'Records the current palette settings into
'array DefArray
SUB GetPalette (DefArray() AS INTEGER)
 OUT &H3C8, 0
  FOR i = 0 TO 255
	DefArray(i) = INP(&H3C9)
	DefArray(i + 256) = INP(&H3C9)
	DefArray(i + 512) = INP(&H3C9)
  NEXT i
END SUB

SUB Load8x14 (Rows)
inregs.ax = &H1122
inregs.bx = Rows
CALL interrupt(&H10, inregs, outregs)
CharHeight = 14
END SUB

SUB Load8x16 (Rows)
inregs.ax = &H1124
inregs.bx = Rows
CALL interrupt(&H10, inregs, outregs)
CharHeight = 16
END SUB

SUB Load8x8 (Rows)
inregs.ax = &H1123
inregs.bx = Rows
CALL interrupt(&H10, inregs, outregs)
CharHeight = 8
END SUB

SUB LoadFont (File$)
 DIM font(3584)
 DEF SEG = VARSEG(font(0))
 v = VARPTR(font(0))
 BLOAD File$, v
 inregsx.ax = &H1121
 inregsx.cx = 14
 inregsx.es = VARSEG(font(0))
 inregsx.bp = VARPTR(font(0))
 CALL INTERRUPTX(&H10, inregsx, outregsx)
 DEF SEG
END SUB

SUB OpenPAL (File$, DefArray() AS INTEGER)
f = FREEFILE
 OPEN File$ FOR INPUT AS f
  LINE INPUT #f, a$
  LINE INPUT #f, a$
  LINE INPUT #f, a$
  FOR u = 0 TO 255
	INPUT #f, a
	DefArray(u) = INT(a / 4)
	INPUT #f, a
	DefArray(u + 256) = INT(a / 4)
	INPUT #f, a
	DefArray(u + 512) = INT(a / 4)
  NEXT u
 CLOSE f
END SUB

'Fades between two palette arrays
SUB PaletteFade (Array1() AS INTEGER, Array2() AS INTEGER, StartCol, EndCol)
   FOR j = StartCol TO EndCol
	 IF Array1(j) > Array2(j) THEN Array1(j) = Array1(j) - 1
	 IF Array1(j) < Array2(j) THEN Array1(j) = Array1(j) + 1
	 IF Array1(j + 256) > Array2(j + 256) THEN Array1(j + 256) = Array1(j + 256) - 1
	 IF Array1(j + 256) < Array2(j + 256) THEN Array1(j + 256) = Array1(j + 256) + 1
	 IF Array1(j + 512) > Array2(j + 512) THEN Array1(j + 512) = Array1(j + 512) - 1
	 IF Array1(j + 512) < Array2(j + 512) THEN Array1(j + 512) = Array1(j + 512) + 1
   NEXT j
END SUB

'Puts random values in all palette registers
SUB RandomizePalette (DefArray() AS INTEGER)
  FOR i = 0 TO 255
	DefArray(i) = INT(RND * 64)
	DefArray(i + 256) = INT(RND * 64)
	DefArray(i + 512) = INT(RND * 64)
  NEXT i
END SUB

'Sets the properties of one color
SUB SetColor (ColNum, Rd, Gr, Bl)
 OUT &H3C8, ColNum
 OUT &H3C9, Rd
 OUT &H3C9, Gr
 OUT &H3C9, Bl
END SUB

'Records the current palette settings in a file
SUB WritePAL (File$, DefArray() AS INTEGER)
f = FREEFILE
OPEN File$ FOR OUTPUT AS f
PRINT #f, "JASC-PAL"
PRINT #f, "0100"
PRINT #f, "256"
FOR k = 0 TO 255
  d$ = ""
	d$ = d$ + RIGHT$(STR$(DefArray(k) * 4), LEN(STR$(DefArray(k) * 4)) - 1) + " "
	d$ = d$ + RIGHT$(STR$(DefArray(k + 256) * 4), LEN(STR$(DefArray(k + 256) * 4)) - 1) + " "
	d$ = d$ + RIGHT$(STR$(DefArray(k + 512) * 4), LEN(STR$(DefArray(k + 512) * 4)) - 1) + " "
  d$ = LEFT$(d$, LEN(d$) - 1)
  PRINT #f, d$
NEXT k
CLOSE f
END SUB

