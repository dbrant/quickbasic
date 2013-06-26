'Basic ModeX Functions for multiple tweaked resolutions
'by Dmitry Brant (me@dmitrybrant.com)
'This source code is Freeware, NOT public domain.
'If you will use this code in your programs, please
'give me credit where credit is due (...so overused...)
'Comments, bugs, and improvements are welcome...

'This code is provided by Dmitry Brant on an "AS-IS" basis.
'I will not be responsible for any damage caused by the
'use or misuse of this code. Thanks.

'Reference:
'&H3D4 = base port of the CRT controller (color)
'&H3C5 = base port of the sequencer
'&H3CE = base port of the graphics controller
'&HA000& = segment of the VGA video memory

'Some code adapted from Lloyd Chang

DEFINT A-Z
DECLARE SUB PCLEARX ()                   'Clear the active page
DECLARE SUB DEMO (w, h)                  'run a demo
DECLARE SUB LOCATEX (row%, col%)         'Align the cursor to a logical text position
DECLARE SUB PRINTX (TheStr$, TextColor)  'much like PRINT, only better!
DECLARE FUNCTION inport$ (Addr&)
DECLARE SUB LoadFont ()                  'Load the standard 8x8 character set
DECLARE SUB memset (Segment&, Addr&, BYTE, Size&)
DECLARE SUB outport (Addr&, WORD&)
DECLARE SUB PCOPYX (FromPage, ToPage)  'Copy cotents of a page to another page
DECLARE SUB PSETX (XCord, YCord, PixelColor) 'PSET for modeX
DECLARE SUB SetActivePage (Page)
DECLARE SUB SetVisiblePage (Page)
DECLARE SUB SetMode (Horz, Vert)
DECLARE SUB DBLine (x1%, y1%, x2%, y2%, colrr%)
DECLARE SUB DBCircle (x1%, y1%, XRadius%, YRadius%, colrr%)
COMMON SHARED CursorX, CursorY
COMMON SHARED VGAWidth, VGAHeight, VGAWidthBytes
COMMON SHARED ActivePage, VisiblePage, TotalPages
COMMON SHARED ActiveStart&, VisibleStart&, ModeName$
COMMON SHARED AfterActiveEnd&
COMMON SHARED ReservedPage, UseReservedPage

OPTION BASE 0
DIM SHARED CharSet(0 TO 127, 0 TO 7) AS INTEGER

DemoStart:
WIDTH 80, 25
CLS
PRINT "ModeX Routines by Dmitry Brant.": PRINT
PRINT "Note: When the program will go into ModeX, your screen may flicker."
PRINT "If the screen does not reappear properly, press ESC to return here."
PRINT "If pressing ESC does nothing, reboot your computer."
PRINT : PRINT "This program is designed to go into 320 x 25 x 256 resolution."
PRINT "(I call it 'Ludicrous Mode')"
LOCATE 17, 1: PRINT "Press any key"
DO: LOOP UNTIL INKEY$ <> ""

SetMode 320, 100
SetVisiblePage 0
SetActivePage 0
FOR i = 0 TO 255
 FOR j = 0 TO 24
  PSETX i, j, i
 NEXT j
NEXT i
DBLine 0, 0, VGAWidth, VGAHeight, 80
DBLine 0, VGAHeight, VGAWidth, 0, 93
DBCircle VGAWidth / 2, VGAHeight / 2, 100, 10, 76

FOR i = 1 TO TotalPages - 1
 SetActivePage i
 LOCATEX 3, 1
 PRINTX "This is Page #" + STR$(i), 33
NEXT i
SetActivePage 0
LOCATEX 1, 1
PRINTX "Resolution:" + STR$(VGAWidth) + " x" + STR$(VGAHeight) + " x 256", 38
LOCATEX 2, 1
PRINTX "Total Pages for this mode: " + STR$(TotalPages), 45
LOCATEX 3, 1
PRINTX "Press space to flip pages,", 46

act = 0
DO
 r$ = INKEY$
 IF r$ = CHR$(27) THEN EXIT DO
 IF r$ = " " THEN
   act = act + 1: IF act > TotalPages - 1 THEN act = 0
   SetVisiblePage act
 END IF
LOOP
SCREEN 0: WIDTH 80, 25

SUB DBCircle (x1, y1, XRadius, YRadius, colrr)
IF XRadius >= YRadius THEN
   dum1! = 1 / XRadius
   Counter2! = -1
   FOR Counter1 = 1 TO XRadius * 3.141593
	  dx! = SIN(Counter2!)
	  dy! = COS(Counter2!)
	  offsetX = INT(dx! * XRadius)
	  offsetY = INT(dy! * YRadius)
	  PSETX x1 - offsetX, y1 - offsetY, colrr
	  PSETX x1 + offsetX, y1 + offsetY, colrr
	  Counter2! = Counter2! + dum1!
   NEXT Counter1
ELSE
   dum1! = 1 / YRadius
   Counter2! = -1
   FOR Counter1 = 1 TO YRadius * 3.141593
	  dx! = SIN(Counter2!)
	  dy! = COS(Counter2!)
	  offsetX = INT(dx! * XRadius)
	  offsetY = INT(dy! * YRadius)
	  PSETX x1 - offsetX, y1 - offsetY, colrr
	  PSETX x1 + offsetX, y1 + offsetY, colrr
	  Counter2! = Counter2! + dum1!
   NEXT Counter1
END IF
END SUB

SUB DBLine (x1, y1, x2, y2, colrrr)
IF ABS(x2 - x1) >= ABS(y2 - y1) THEN
   slope! = (y2 - y1) / (x2 - x1)
   IF x2 < x1 THEN ste = -1 ELSE ste = 1
   FOR i = x1 TO x2 STEP ste
	  xmod = (i - x1) * slope!
	  PSETX i, xmod + y1, colrrr
   NEXT i
ELSEIF ABS(x2 - x1) < ABS(y2 - y1) THEN
   slope! = (x2 - x1) / (y2 - y1)
   IF y2 < y1 THEN ste = -1 ELSE ste = 1
   FOR i = y1 TO y2 STEP ste
	  ymod = (i - y1) * slope!
	  PSETX ymod + x1, i, colrrr
   NEXT i
END IF
END SUB

FUNCTION inport$ (Addr&)
WORD& = ((INP(Addr& + 1) * 256) + INP(Addr&))
inport$ = HEX$(WORD&)
END FUNCTION

SUB LoadFont
FFile = FREEFILE
OPEN "8x8Font.fnt" FOR INPUT AS FFile  'Check if file exists
CLOSE FFile
OPEN "8x8Font.fnt" FOR BINARY AS FFile
a$ = " "
FOR CountC = 0 TO 127
  FOR CountD = 0 TO 7
	  GET #FFile, , a$
	  CharSet(CountC, CountD) = ASC(a$)
  NEXT CountD
NEXT CountC
CLOSE #FFile
END SUB

SUB LOCATEX (row, col)
CursorX = (col - 1) * 8
CursorY = (row - 1) * 8
END SUB

SUB memset (Segment&, Addr&, BYTE, Size&)
DEF SEG = Segment&
FOR count& = 1 TO Size&
  POKE Addr& + (count& - 1), BYTE
NEXT count&
DEF SEG
END SUB

SUB outport (Addr&, WORD&)
OUT Addr&, (WORD& MOD 256)
OUT Addr& + 1, (WORD& \ 256)
END SUB

SUB PCLEARX
OUT &H3C4, 2
DEF SEG = &HA000&
PageSize& = ((VGAWidthBytes * VGAHeight) - 1)
OUT &H3C5, 1
FOR count& = 0 TO PageSize&
  POKE Addr& + count& + ActiveStart&, 0
NEXT count&
OUT &H3C5, 2
FOR count& = 0 TO PageSize&
  POKE Addr& + count& + ActiveStart&, 0
NEXT count&
OUT &H3C5, 4
FOR count& = 0 TO PageSize&
  POKE Addr& + count& + ActiveStart&, 0
NEXT count&
OUT &H3C5, 8
FOR count& = 0 TO PageSize&
  POKE Addr& + count& + ActiveStart&, 0
NEXT count&
DEF SEG
END SUB

SUB PCOPYX (FromPage, ToPage)
ToPage = ToPage MOD TotalPages
FromPage = FromPage MOD TotalPages
VGAHeight& = VGAHeight 'This prevents an overflow error
ToPageStart& = (ToPage * VGAWidthBytes * CLNG(VGAHeight))
FromPageStart& = (FromPage * VGAWidthBytes * CLNG(VGAHeight))
CountEnd& = ((VGAWidthBytes * VGAHeight&) - 1)
OUT &H3C4, 2
OUT &H3CE, 4
DEF SEG = &HA000&
OUT &H3C5, 1
OUT &H3CF, 0
FOR count& = 0 TO CountEnd&
  POKE (count& + ToPageStart&), PEEK(count& + FromPageStart&)
NEXT count&
OUT &H3C5, 2
OUT &H3CF, 1
FOR count& = 0 TO CountEnd&
  POKE (count& + ToPageStart&), PEEK(count& + FromPageStart&)
NEXT count&
OUT &H3C5, 4
OUT &H3CF, 2
FOR count& = 0 TO CountEnd&
  POKE (count& + ToPageStart&), PEEK(count& + FromPageStart&)
NEXT count&
OUT &H3C5, 8
OUT &H3CF, 3
FOR count& = 0 TO CountEnd&
  POKE (count& + ToPageStart&), PEEK(count& + FromPageStart&)
NEXT count&
DEF SEG
END SUB

SUB PRINTX (TheStr$, TextColor)
	IF TheStr$ = "" THEN EXIT SUB
	FOR count = 1 TO LEN(TheStr$)
	  parse$ = MID$(TheStr$, count, 1)
	  FOR CountD = 0 TO 7
		n = 0
		FOR CountC = 7 TO 0 STEP -1
		  IF CharSet(ASC(parse$), CountD) AND 2 ^ CountC THEN PSETX CursorX + n, CursorY + CountD, TextColor
		  n = n + 1
		NEXT CountC
	  NEXT CountD
	  CursorX = CursorX + 8
	NEXT count
END SUB

SUB PSETX (XCord, YCord, PixelColor)
'Each address accesses four neighboring pixels, so set
'Write Plane Enable according to which pixel we want
'to modify.  The plane is determined by the two least
'significant bits of the x-coordinate:
IF XCord >= VGAWidth OR XCord < 0 OR YCord >= VGAHeight OR YCord < 0 THEN EXIT SUB

OUT &H3C4, 2
OUT &H3C5, (2 ^ (XCord AND 3))

'The offset of the pixel into the video segment is
'offset = (width * y + x) / 4, and write the given
'color to the plane we selected above.  Heed the active
'page start selection.
VGAWidthBytes& = VGAWidthBytes 'This is quicker than calling up CLNG
								'within the FOR/NEXT loops
AfterActiveEnd& = ActiveStart& + (VGAWidthBytes& * VGAHeight)
Address& = ((VGAWidthBytes& * YCord) + (XCord \ 4) + ActiveStart&)
DEF SEG = &HA000&
IF Address& < AfterActiveEnd& THEN POKE Address&, PixelColor
DEF SEG
END SUB

'
'setXXXPage() sets the specified page by multiplying the page number
'with the size of one page at the current resolution, then handing the
'resulting offset value over to the corresponding setXXXStart()
'function.  The first page is number 0.
'
SUB SetActivePage (Page)
Page = Page MOD TotalPages
IF UseReservedPage = 1 THEN
	IF Page = ReservedPage THEN
		Page = (Page + 1) MOD TotalPages
		IF Page = ReservedPage THEN 'This mode must only
			UseReservedPage = 0  'have one page
		ELSE
			IF Page = VisiblePage THEN               'This mode must
				UseReservedPage = 0                'only have two
				Page = (Page + 1) MOD TotalPages 'pages
			END IF
		END IF
	END IF
END IF
ActivePage = Page
Page& = Page 'The use of Page& prevents an overflow error.
ActiveStart& = Page& * VGAWidthBytes * VGAHeight
END SUB

SUB SetMode (Horz, Vert)  'Horizontal and vertical size in pixels
'SELECT CASE Horz        'In this case, the Vert argument is vestigial
'CASE 320
'  IF Vert = 200 THEN
	'Sets mode 13h, then turns it into an unchained (planar), 4-page
	'320x200x256 mode.
	SCREEN 0                          'SCREEN 0 is needed to make
	SCREEN 13                         'the command SCREEN 13 effective
	LoadFont                          'Loads the 8x8 character set
	outport &H3C4, &H604              'Put the chain-4 mode of sequencer off
	memset &HA000&, 0, 0, &H10000     'SCREEN 13 only clears every
									  'fourth byte of each plane
	OUT &H3D4, &H11                   'Initializes the VGA to
	TempByte& = (INP(&H3D5) AND &H7F) 'accept any combination
	outport &H3D4, (&H11 OR (TempByte& * 256))'of configuration
									  'register settings.
	outport &H3C4, &H100              'Reset sequencer
	outport &H3C4, &H300              'Re-reset sequencer
	OUT &H3C0, &H20                   'Reenable display data
	outport &H3D4, &H2C11             'Turn off write protect
	OUT &H3C2, &HE3                   'Dot clock
	outport &H3D4, &H5F00             'Horizontal total
	outport &H3D4, &H4F01             'Horizontal displayed
	outport &H3D4, &H5002             'Horizontal blanking start
	outport &H3D4, &H8203&            'Horizontal blanking end
	outport &H3D4, &H5404             'Horizontal sync/retrace start
	outport &H3D4, &H8005&            'Horizontal sync/retrace end
	outport &H3D4, &HBF06&            'Vertical total
	outport &H3D4, &H1F07             'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H8                'Present row scan
	outport &H3D4, &H4109             'Maximum scanline/character height
	outport &H3D4, &H9C10&            'Vertical sync/retrace start
	outport &H3D4, &H8E11&            'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H8F12&            'Vertical displayed
	outport &H3D4, &H2813             'Offset/logical width
	outport &H3D4, &H14               'Turn off CRT controller's
									  'double-word (or long) mode
	outport &H3D4, &H9615&            'Vertical blanking start
	outport &H3D4, &HB916&            'Vertical blanking end
	outport &H3D4, &HE317&            'Turn on CRT controller's byte mode
	outport &H3D4, &H14               'Turn off CRT controller's
									  'double-word (or long) mode
	'ActiveStart& specifies the start of the page being accessed by
	'drawing operations.  VisibleStart& specifies the contents of the
	'screen start register, i.e. the start of the visible page.
	'By default, we want screen refreshing and drawing operations
	'to be based at offset 0 in the video segment.
	ActiveStart& = 0
	VisibleStart& = 0
	VGAWidth = 320
	VGAHeight = 200
	'Each byte addresses four pixels, so the width of a scan line in
	'bytes is one fourth of the number of pixels on a line.
	VGAWidthBytes = (VGAWidth / 4)
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H4F09  'Maximum scanline/character height
	VGAWidth = 320
	VGAHeight = 25
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
END SUB

'
'setXXXPage() sets the specified page by multiplying the page number
'with the size of one page at the current resolution, then handing the
'resulting offset value over to the corresponding setXXXStart()
'function.  The first page is number 0.
'
SUB SetVisiblePage (Page)
Page = Page MOD TotalPages
VisiblePage = Page
Page& = Page 'The use of Page& prevents an overflow error.
VisibleStart& = Page& * VGAWidthBytes * VGAHeight

WAIT &H3DA, 8
outport &H3D4, &HC  'Set high byte
outport &H3D5, VisibleStart& \ 256
outport &H3D4, &HD
outport &H3D5, VisibleStart& AND &HFF
END SUB

