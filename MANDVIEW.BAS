'Mandelbrot Set Viewer, version 1.0
'By Dmitry Brant
'me@dmitrybrant.com
'
'IMPORTANT:
'To run this program, you must start QuickBasic with the following
'command line:
'
'	QB /L MANDEL.QLB
'
'(assuming that MANDEL.QLB is in the same directory as QuickBasic)
'
'This is freeware. Use freely.
DECLARE SUB linexor (x1%, y1%, x2%, y2%, col%)
DECLARE SUB plotMandel CDECL ALIAS "_plotMandel" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL xmin#, BYVAL xmax#, BYVAL ymin#, BYVAL ymax#, BYVAL xstart#, BYVAL maxIterations%, BYVAL maxSize%)
DECLARE SUB plotJulia CDECL ALIAS "_plotJulia" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL xmin#, BYVAL xmax#, BYVAL ymin#, BYVAL ymax#, BYVAL pval#, BYVAL qval#, BYVAL maxIterations%, BYVAL maxSize%)
DECLARE SUB plotDragon CDECL ALIAS "_plotDragon" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL xmin#, BYVAL xmax#, BYVAL ymin#, BYVAL ymax#, BYVAL xstart#, BYVAL maxIterations%, BYVAL maxSize%)
DECLARE SUB plotPhoenix CDECL ALIAS "_plotPhoenix" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL xmin#, BYVAL xmax#, BYVAL ymin#, BYVAL ymax#, BYVAL xstart#, BYVAL maxIterations%, BYVAL maxSize%)

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

Start:
CLS
PRINT "Mandelbrot Set Viewer, version 1.0"
PRINT "By Dmitry Brant."
PRINT : PRINT
PRINT "Directions:"
PRINT "Using your mouse, click and drag to select a rectangular region on the "
PRINT "Mandelbrot set. When the rectangle is in place, press Enter to zoom in, or you"
PRINT "may select another rectangle. Tip: try to keep the rectangle proportional"
PRINT "to your monitor, so that the picture wouldn't look stretched."
PRINT "You can zoom in as many times as you want. Press 'R' at any time to return"
PRINT "to the original Mandelbrot set."
LOCATE 15, 1: PRINT "We have 4 predefined Mandelbrot Set variants."
PRINT "Press a number from 1 to 4, ESC to exit."
DO
R$ = INKEY$
IF R$ = CHR$(27) THEN SYSTEM
IF R$ > "0" AND R$ < "5" THEN EXIT DO
LOOP
SELECT CASE R$
CASE "1"
xmin# = -2#
xmax# = 1.2
ymin# = -1.2
ymax# = 1.2
xstart# = 0
opt = 0
CASE "2"
xmin# = -2.2  'for Mandelbrot-like set (dragon)
xmax# = 4.2
ymin# = -2
ymax# = 2
xstart# = .5
opt = 1
CASE "3"
xmin# = -2.1  'for Mandelbrot-like set (phoenix)
xmax# = 1.5
ymin# = -2#
ymax# = 2#
xstart# = 0
opt = 2
CASE "4"
xmin# = -2  'for Mandelbrot set variant
xmax# = 1.5
ymin# = -2#
ymax# = 2#
xstart# = .7
opt = 0
END SELECT
SCREEN 13
FOR i = 0 TO 63      'Feel free to change around the palette
OUT &H3C8, i         'if you get tired of this one...
OUT &H3C9, 32 - i / 2
OUT &H3C9, 0
OUT &H3C9, i
OUT &H3C8, i + 64
OUT &H3C9, i
OUT &H3C9, i
OUT &H3C9, 63 - i
OUT &H3C8, i + 128
OUT &H3C9, 63
OUT &H3C9, 63 - i
OUT &H3C9, i
OUT &H3C8, i + 192
OUT &H3C9, 63 - i
OUT &H3C9, 0
OUT &H3C9, 63 - i
NEXT i
OUT &H3C8, 0
OUT &H3C9, 0
OUT &H3C9, 0
OUT &H3C9, 0

Restart:
ResetMouse
DO
	IF xmax# < xmin# THEN SWAP xmax#, xmin#
	IF ymax# < ymin# THEN SWAP ymax#, ymin#
	IF opt = 0 THEN plotMandel 0, 0, 319, 199, xmin#, xmax#, ymin#, ymax#, xstart#, 255, 4
	IF opt = 1 THEN plotDragon 0, 0, 319, 199, xmin#, xmax#, ymin#, ymax#, xstart#, 255, 4
	IF opt = 2 THEN plotPhoenix 0, 0, 319, 199, xmin#, xmax#, ymin#, ymax#, xstart#, 255, 4
	MouseCursor 1
	deltax# = (xmax# - xmin#) / 320
	deltay# = (ymax# - ymin#) / 200
	tempx# = xmin#: tempy# = ymin#
SelectLoop:
	c = 0: d = 0
	DO
		R$ = INKEY$
		IF R$ = CHR$(27) THEN GOTO fin
		IF R$ = "r" OR R$ = "R" THEN GOTO Restart
		IF R$ = CHR$(13) AND d = 1 THEN MouseCursor 0: EXIT DO
			  
				'Turn this off if you don't want it
				'This is for reference purposes only.
		LOCATE 1, 1: COLOR 63
		PRINT USING "#.############"; xmin# + MouseX / 2 * deltax#
		LOCATE 1, 18:
		PRINT USING "#.############"; ymin# + (200 - MouseY) * deltay#
	  
		b = MouseButtons
		IF b = 1 AND c = 0 THEN
			IF d = 1 THEN GOSUB CleanOldRect
			d = 0
			mx = MouseX / 2
			my = MouseY
			xold = mx
			yold = my
			xmin# = tempx# + mx * deltax#
			ymin# = tempy# + (200 - my) * deltay#
			c = 1
			MouseCursor 0
			SetMousePos mx * 2, my
		END IF
		mnx = MouseX / 2
		mny = MouseY
		IF b = 0 AND c = 1 THEN
			xmax# = tempx# + mnx * deltax#
			ymax# = tempy# + (200 - mny) * deltay#
			c = 0
			d = 1
			MouseCursor 1
			xt1 = mx: xt2 = mnx: yt1 = my: yt2 = mny: xt3 = xold: yt3 = yold
		END IF
		IF c THEN GOSUB UpdateRect
		xold = mnx
		yold = mny
	LOOP
LOOP

fin:
SCREEN 0: WIDTH 80, 25: GOTO Start

UpdateRect:
linexor mx, my, xold, my, -64
linexor mx, my, mx, yold, -64
linexor mx, yold, xold, yold, -64
linexor xold, my, xold, yold, -64
linexor mx, my, mnx, my, 64
linexor mx, my, mx, mny, 64
linexor mx, mny, mnx, mny, 64
linexor mnx, my, mnx, mny, 64
RETURN

CleanOldRect:
linexor xt1, yt1, xt3, yt1, -64
linexor xt1, yt1, xt1, yt3, -64
linexor xt1, yt3, xt3, yt3, -64
linexor xt3, yt1, xt3, yt3, -64
RETURN

SUB linexor (x1, y1, x2, y2, col)
x01 = x1: x02 = x2: y01 = y1: y02 = y2
IF x02 < x01 THEN SWAP x02, x01
IF y02 < y01 THEN SWAP y02, y01
FOR i = y01 TO y02
	FOR j = x01 TO x02
		g = POINT(j, i)
		PSET (j, i), g + col
	NEXT j
NEXT i
END SUB

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

