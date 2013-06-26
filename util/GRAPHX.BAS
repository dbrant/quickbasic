'BIOS Graphics functions
'Code by Dmitry Brant (d.brant64@csuohio.edu)
'Please give credit where credit is due.
DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'QuickBasic's interrupt declarations
		   'EXTENDED GRAPHICS FUNCTIONS
DECLARE SUB DBLine (x1%, y1%, x2%, y2%, colrr%)      'Manual line
DECLARE SUB DBCircle (x1%, y1%, XRadius%, YRadius%, colrr%)  'Manual circle
DECLARE FUNCTION ReadDot% (xx%, yy%)     'Get the color of a pixel at x,y
DECLARE SUB SetDot (x%, y%, colr%)       'Set the pixel at x,y to color
DECLARE SUB WriteString (stri$, colrr%)  'Write a string. No cursor advance
DECLARE FUNCTION GetCursorRow% ()        'Get the cursor row
DECLARE FUNCTION GetCursorCol% ()        'Get the cursor column
DECLARE SUB SetCursor (roww, coll)       'Set cursor position
DECLARE SUB WriteChar (Char$, colr)      'Write character. No cursor advance
DECLARE SUB SetTextBorder (colr)         'Set the border to a color
DECLARE SUB SetActivePage (Page)         'Set active display page
DECLARE SUB SplineCurve (n, xx(), yy(), Colour, Flag1)
	DECLARE SUB Drawcurve (s, x!(), y!(), Cr)
	DECLARE SUB Graphpoints (x(), y(), n, Cr)
	DECLARE SUB Spline (f(), t!(), s, sz, ft!())
DECLARE FUNCTION VESAGetInfo$ ()           'Get extended VESA info
DECLARE FUNCTION VESAGetModeInfo$ (Mode%)  'Get VESA screen mode info
DECLARE FUNCTION VESASetMode% (Mode%)      'Set VESA screen mode
DECLARE FUNCTION GetStringData$ (St$, Offset%, NumBytes%)
DECLARE FUNCTION VGAGetFunc$ ()            'Get VGA functionality state
COMMON SHARED InRegs AS RegType
COMMON SHARED OutRegs AS RegType
COMMON SHARED inregsx AS RegTypeX
COMMON SHARED outregsx AS RegTypeX
COMMON SHARED CurPage AS INTEGER         'Needed by most INT functions

CLS
PRINT VESAGetInfo

SUB DBCircle (x1, y1, XRadius, YRadius, colrr)
IF XRadius >= YRadius THEN
   dum1# = 1 / XRadius
   Counter2# = -1
   FOR Counter1 = 1 TO XRadius * 3.1415926535#
	  dx# = SIN(Counter2#)
	  dy# = COS(Counter2#)
	  offsetX = INT(dx# * XRadius)
	  offsetY = INT(dy# * YRadius)
	  SetDot x1 - offsetX, y1 - offsetY, colrr
	  SetDot x1 + offsetX, y1 + offsetY, colrr
	  Counter2# = Counter2# + dum1#
   NEXT Counter1
ELSE
   dum1# = 1 / YRadius
   Counter2# = -1
   FOR Counter1 = 1 TO YRadius * 3.1415926535#
	  dx# = SIN(Counter2#)
	  dy# = COS(Counter2#)
	  offsetX = INT(dx# * XRadius)
	  offsetY = INT(dy# * YRadius)
	  SetDot x1 - offsetX, y1 - offsetY, colrr
	  SetDot x1 + offsetX, y1 + offsetY, colrr
	  Counter2# = Counter2# + dum1#
   NEXT Counter1
END IF
END SUB

SUB DBLine (x1, y1, x2, y2, colrrr)
IF ABS(x2 - x1) >= ABS(y2 - y1) THEN
   slope! = (y2 - y1) / (x2 - x1)
   IF x2 < x1 THEN ste = -1 ELSE ste = 1
   FOR i = x1 TO x2 STEP ste
	  xmod = INT((i - x1) * slope!)
	  SetDot i, xmod + y1, colrrr
   NEXT i
ELSEIF ABS(x2 - x1) < ABS(y2 - y1) THEN
   slope! = (x2 - x1) / (y2 - y1)
   IF y2 < y1 THEN ste = -1 ELSE ste = 1
   FOR i = y1 TO y2 STEP ste
	  ymod = INT((i - y1) * slope!)
	  SetDot ymod + x1, i, colrrr
   NEXT i
END IF
END SUB

'   ARGUMENTS:  s=number of points from the beginning of curve to end
'               x()=array for x-coordinates
'               y()=array for y-coordinates
SUB Drawcurve (s, x!(), y!(), Cr) STATIC
	FOR i = 1 TO s - 1
		LINE (x!(i), y!(i))-(x!(i + 1), y!(i + 1)), Cr
	NEXT i
END SUB

FUNCTION GetCursorCol
InRegs.ax = &H300
InRegs.BX = CurPage * 256
CALL INTERRUPT(&H10, InRegs, OutRegs)
GetCursorCol = OutRegs.dx AND &HFF
END FUNCTION

FUNCTION GetCursorRow
InRegs.ax = &H300
InRegs.BX = CurPage * 256
CALL INTERRUPT(&H10, InRegs, OutRegs)
GetCursorRow = ABS((OutRegs.dx AND &HFF00) / 256)
END FUNCTION

FUNCTION GetStringData$ (St$, Offset, NumBytes)
B$ = MID$(St$, Offset + 1, NumBytes): c$ = ""
FOR i = LEN(B$) TO 1 STEP -1
e$ = HEX$(ASC(MID$(B$, i, 1)))
IF LEN(e$) = 1 THEN e$ = "0" + e$
c$ = c$ + e$
IF i = 3 THEN c$ = c$ + ":"
NEXT i
GetStringData$ = c$
END FUNCTION

'   ARGUMENTS:  x()=array for x-coordinates
'               y()=array for y-coordinates
'               n=number of points
SUB Graphpoints (x(), y(), n, Cr) STATIC
	FOR i = 1 TO n
		FOR j = 1 TO 5
			LINE (x(i) - 3 + j, y(i) - 3 + j)-(x(i) - 3 + j, y(i) - 3 + j), Cr
			LINE (x(i) - 3 + j, y(i) + 3 - j)-(x(i) - 3 + j, y(i) + 3 - j), Cr
		NEXT j
	NEXT i
END SUB

FUNCTION ReadDot (xx, yy)
InRegs.ax = &HD00
InRegs.cx = xx
InRegs.dx = yy
CALL INTERRUPT(&H10, InRegs, OutRegs)
ReadDot = OutRegs.ax AND 255
END FUNCTION

SUB SetActivePage (Page)
CurPage = Page
InRegs.ax = &H500 OR Page
CALL INTERRUPT(&H10, InRegs, OutRegs)
END SUB

SUB SetCursor (roww, coll)
InRegs.ax = &H200
InRegs.BX = CurPage * 256
InRegs.cx = 0
InRegs.dx = (roww * 256) OR coll
CALL INTERRUPT(&H10, InRegs, OutRegs)
END SUB

SUB SetDot (x, y, colr)
InRegs.ax = &HC00 + colr
InRegs.BX = CurPage * 256
InRegs.cx = x
InRegs.dx = y
CALL INTERRUPT(&H10, InRegs, OutRegs)
END SUB

SUB SetTextBorder (colr)
InRegs.ax = &HB00
InRegs.BX = colr
CALL INTERRUPT(&H10, InRegs, OutRegs)
END SUB

'   ARGUMENTS:  f()=array for x-coordinates or y-coordinates
'               t!()=array for t-coordinates
'               s=number of points
'               sz=number of divisions from first point to last
'               ft!()=x or y-coordinate values for spline
SUB Spline (f(), t!(), s, sz, ft!()) STATIC
	DIM a!(1 TO 50), B!(1 TO 50), c!(1 TO 50)
	FOR i = 2 TO s
		B!(i) = t!(i) - t!(i - 1)
		c!(i) = (f(i) - f(i - 1)) / B!(i)
	NEXT i
	B!(1) = B!(2) + B!(3)
	c!(1) = B!(3)
	a!(1) = ((B!(2) + 2 * B!(1)) * c!(2) * B!(3) + B!(2) * B!(2) * c!(3)) / B!(1)
	FOR i = 2 TO s - 1
		g! = -B!(i + 1) / c!(i - 1)
		a!(i) = g! * a!(i - 1) + 3 * (B!(i) * c!(i + 1) + B!(i + 1) * c!(i))
		c!(i) = g! * B!(i - 1) + 2 * (B!(i) + B!(i + 1))
	NEXT i
	g! = B!(s - 1) + B!(s)
	a!(s) = ((B!(s) + g! + g!) * c!(s) * B!(s - 1) + B!(s) * B!(s) * (f(s - 1) - f(s - 2)) / B!(s - 1)) / g!
	g! = -g! / c!(s - 1)
	c!(s) = B!(s - 1)
	c!(s) = g! * B!(s - 1) + c!(s)
	a!(s) = (g! * a!(s - 1) + a!(s)) / c!(s)
	FOR i = s - 1 TO 1 STEP -1
		a!(i) = (a!(i) - B!(i) * a!(i + 1)) / c!(i)
	NEXT i
	FOR i = 2 TO s
		D! = (f(i) - f(i - 1)) / B!(i)
		e! = a!(i - 1) + a!(i) - 2 * D!
		B!(i - 1) = 2 * (D! - a!(i - 1) - e!) / B!(i)
		c!(i - 1) = (e! / B!(i)) * (6 / B!(i))
	NEXT i
	wt! = 0
	i = 0
	j = 1
	dt! = t!(s) / (sz - 1)
	WHILE i < sz
		IF wt! > t!(j + 1) THEN
			j = j + 1
		END IF
		i = i + 1
		h! = wt! - t!(j)
		ft!(i) = f(j) + h! * (a!(j) + h! * (B!(j) + h! * c!(j) / 3) / 2)
		wt! = wt! + dt!
	WEND
END SUB

'   ARGUMENTS:  n=number of points
'               xx()=x-coordinate values for spline
'               yy()=y-coordinate values for spline
SUB SplineCurve (n, xx(), yy(), Colour, Flag1)
	DIM spx!(1 TO 600), spy!(1 TO 600)
	DIM tt!(1 TO n)
	nd = 600
	IF UBOUND(xx) <> n OR UBOUND(yy) <> n THEN EXIT SUB
	tt!(1) = 0
	FOR i = 2 TO n
		tt!(i) = tt!(i - 1) + SQR((xx(i) - xx(i - 1)) ^ 2 + (yy(i) - yy(i - 1)) ^ 2)
	NEXT i
	IF Flag1 THEN CALL Graphpoints(xx(), yy(), n, Colour)
	CALL Spline(xx(), tt!(), n, nd, spx!())
	CALL Spline(yy(), tt!(), n, nd, spy!())
	CALL Drawcurve(nd, spx!(), spy!(), Colour)
	ERASE spx!, spy!, tt!
END SUB

FUNCTION VESAGetInfo$
a0$ = STRING$(262, CHR$(0))
inregsx.ax = &H4F00
inregsx.ES = VARSEG(a0$)
inregsx.DI = SADD(a0$)
CALL interruptx(&H10, inregsx, outregsx)
VESAGetInfo$ = a0$
END FUNCTION

FUNCTION VESAGetModeInfo$ (Mode)
a0$ = STRING$(256, CHR$(0))
inregsx.ax = &H4F01
inregsx.cx = Mode
inregsx.ES = VARSEG(a0$)
inregsx.DI = SADD(a0$)
CALL interruptx(&H10, inregsx, outregsx)
VESAGetModeInfo$ = a0$
END FUNCTION

FUNCTION VESASetMode (Mode)
InRegs.ax = &H4F02
InRegs.BX = Mode
CALL INTERRUPT(&H10, InRegs, OutRegs)
VESASetMode = OutRegs.ax / 256
END FUNCTION

FUNCTION VGAGetFunc$
a0$ = STRING$(64, CHR$(0))
inregsx.ax = &H1B00
inregsx.BX = 0
inregsx.ES = VARSEG(a0$)
inregsx.DI = SADD(a0$)
CALL interruptx(&H10, inregsx, outregsx)
VGAGetFunc$ = a0$
END FUNCTION

SUB WriteChar (Char$, colr)
Ch = ASC(Char$)
InRegs.ax = (&H9 * 256) OR Ch
InRegs.BX = (CurPage * 256) OR colr
InRegs.cx = 1
CALL INTERRUPT(&H10, InRegs, OutRegs)
END SUB

SUB WriteString (stri$, Attr)
inregsx.ax = &H1301
inregsx.BX = (CurPage * 256) OR Attr
inregsx.cx = LEN(stri$)
inregsx.dx = (GetCursorRow * 256) OR GetCursorCol
inregsx.ES = VARSEG(stri$)
inregsx.bp = SADD(stri$)
CALL interruptx(&H10, inregsx, outregsx)
END SUB

