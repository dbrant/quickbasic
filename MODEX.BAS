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

DEFINT A-Z
DECLARE SUB ClearPage ()                 'Clear the active page
DECLARE SUB DEMO (w, h)                  'run a demo
DECLARE SUB LOCATEX (row%, col%)         'Align the cursor to a logical text position
DECLARE SUB PRINTX (TheStr$, TextColor)  'much like PRINT, only better!
DECLARE FUNCTION inport$ (Addr&)
DECLARE SUB LoadCharSet ()               'Load the standard 8x8 character set
DECLARE SUB memset (Segment&, Addr&, BYTE, Size&)
DECLARE SUB outport (Addr&, WORD&)
DECLARE SUB PageCopy (FromPage, ToPage)  'Copy cotents of a page to another page
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

DO
  WIDTH 80, 25
  CLS
  PRINT " 0) picture display (320x100x256)"
  PRINT " 1) picture display (320x120x256)"
  PRINT " 2) picture display (256x200x256)"
  PRINT " 3) picture display (256x224x256)"
  PRINT " 4) picture display (256x240x256)"
  PRINT " 5) picture display (256x400x256)"
  PRINT " 6) picture display (256x480x256)"
  PRINT " 7) picture display (320x200x256)"
  PRINT " 8) picture display (320x240x256)"
  PRINT " 9) picture display (320x400x256)"
  PRINT "10) picture display (320x480x256)"
  PRINT "11) picture display (360x200x256)"
  PRINT "12) picture display (360x240x256)"
  PRINT "13) picture display (360x270x256)"
  PRINT "14) picture display (360x360x256)"
  PRINT "15) picture display (360x400x256)"
  PRINT "16) picture display (360x480x256)"
  PRINT "17) picture display (376x282x256)"
  PRINT "18) picture display (376x308x256)"
  PRINT "19) picture display (376x564x256)"
  PRINT "20) picture display (400x300x256)"
  PRINT "21) picture display (400x600x256)"
  PRINT "[Q] Quit"
  INPUT "Your Choice:", Selection$
  SELECT CASE UCASE$(Selection$)
	CASE IS = "0"
	  DEMO 320, 100
	CASE IS = "1"
	  DEMO 320, 120
	CASE IS = "2"
	  DEMO 256, 200
	CASE IS = "3"
	  DEMO 256, 224
	CASE IS = "4"
	  DEMO 256, 240
	CASE IS = "5"
	  DEMO 256, 400
	CASE IS = "6"
	  DEMO 256, 480
	CASE IS = "7"
	  DEMO 320, 200
	CASE IS = "8"
	  DEMO 320, 240
	CASE IS = "9"
	  DEMO 320, 400
	CASE IS = "10"
	  DEMO 320, 480
	CASE IS = "11"
	  DEMO 360, 200
	CASE IS = "12"
	  DEMO 360, 240
	CASE IS = "13"
	  DEMO 360, 270
	CASE IS = "14"
	  DEMO 360, 360
	CASE IS = "15"
	  DEMO 360, 400
	CASE IS = "16"
	  DEMO 360, 480
	CASE IS = "17"
	  DEMO 376, 282
	CASE IS = "18"
	  DEMO 376, 308
	CASE IS = "19"
	  DEMO 376, 564
	CASE IS = "20"
	  DEMO 400, 300
	CASE IS = "21"
	  DEMO 400, 600
	CASE IS = "Q"
	  SCREEN 0
	  WIDTH 80, 25
	  COLOR 7, 0
	  CLS
	  SYSTEM
  END SELECT
LOOP

SUB ClearPage
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

SUB DBCircle (x1, y1, XRadius, YRadius, colrr)
IF XRadius >= YRadius THEN
   dum1# = 1 / XRadius
   Counter2# = -1
   FOR Counter1 = 1 TO XRadius * 3.1415926535#
	  dx# = SIN(Counter2#)
	  dy# = COS(Counter2#)
	  offsetX = INT(dx# * XRadius)
	  offsetY = INT(dy# * YRadius)
	  PSETX x1 - offsetX, y1 - offsetY, colrr
	  PSETX x1 + offsetX, y1 + offsetY, colrr
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
	  PSETX x1 - offsetX, y1 - offsetY, colrr
	  PSETX x1 + offsetX, y1 + offsetY, colrr
	  Counter2# = Counter2# + dum1#
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

SUB DEMO (w, h)

SetMode w, h
SetVisiblePage 0
SetActivePage 0
FOR i = 0 TO 255
 FOR j = 0 TO 24
  PSETX i, j, i
 NEXT j
NEXT i
DBLine 0, 0, VGAWidth, VGAHeight, 80
DBLine 0, VGAHeight, VGAWidth, 0, 93
DBCircle VGAWidth / 2, VGAHeight / 2, 100, 100, 76

FOR i = 0 TO TotalPages - 1
 SetActivePage i
 LOCATEX 9, 6 + i * 2
 PRINTX "This is Page #" + STR$(i), 33
NEXT i
SetActivePage 0
LOCATEX 4, 1
PRINTX "Resolution:" + STR$(VGAWidth) + " x" + STR$(VGAHeight) + " x 256", 38
LOCATEX 5, 1
PRINTX "Total Pages for this mode: " + STR$(TotalPages), 45
LOCATEX 6, 1
PRINTX "Press space to flip pages,", 46
LOCATEX 7, 1
PRINTX "ESC to quit.", 48

act = 0
DO
 r$ = INKEY$
 IF r$ = CHR$(27) THEN EXIT DO
 IF r$ = " " THEN
   act = act + 1: IF act > TotalPages - 1 THEN act = 0
   SetVisiblePage act
 END IF
LOOP
END SUB

FUNCTION inport$ (Addr&)
WORD& = ((INP(Addr& + 1) * 256) + INP(Addr&))
inport$ = HEX$(WORD&)
END FUNCTION

SUB LoadCharSet
FFile = FREEFILE
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

SUB PageCopy (FromPage, ToPage)
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
SELECT CASE Horz
CASE 256
  IF Vert = 200 THEN       '256x200 resolution
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H5F00  'Horizontal total
	outport &H3D4, &H3F01  'Horizontal displayed
	outport &H3D4, &H4202  'Horizontal blanking start
	outport &H3D4, &H9F03& 'Horizontal blanking end
	outport &H3D4, &H4C04  'Horizontal sync/retrace start
	outport &H3D4, &H5     'Horizontal sync/retrace end
	outport &H3D4, &H2013  'Offset/logical width
	VGAWidth = 256
	VGAHeight = 200
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
 ELSEIF Vert = 224 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H5F00  'Horizontal total
	outport &H3D4, &H3F01  'Horizontal displayed
	outport &H3D4, &H4002  'Horizontal blanking start
	outport &H3D4, &H8203& 'Horizontal blanking end
	outport &H3D4, &H4A04  'Horizontal sync/retrace start
	outport &H3D4, &H9A05& 'Horizontal sync/retrace end
	outport &H3D4, &HB06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4109  'Maximum scanline/character height
	outport &H3D4, &HDA10& 'Vertical sync/retrace start
	outport &H3D4, &H9C11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HBF12& 'Vertical displayed
	outport &H3D4, &H2013  'Offset/logical width
	outport &H3D4, &HC715& 'Vertical blanking start
	outport &H3D4, &H416   'Vertical blanking end
	VGAWidth = 256
	VGAHeight = 224
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 240 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H5F00  'Horizontal total
	outport &H3D4, &H3F01  'Horizontal displayed
	outport &H3D4, &H4202  'Horizontal blanking start
	outport &H3D4, &H9F03& 'Horizontal blanking end
	outport &H3D4, &H4C04  'Horizontal sync/retrace start
	outport &H3D4, &H5     'Horizontal sync/retrace end
	outport &H3D4, &HD06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4109  'Maximum scanline/character height
	outport &H3D4, &HEA10& 'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HDF12& 'Vertical displayed
	outport &H3D4, &H2013  'Offset/logical width
	outport &H3D4, &HE715& 'Vertical blanking start
	outport &H3D4, &H616   'Vertical blanking end
	VGAWidth = 256
	VGAHeight = 240
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
 ELSEIF Vert = 256 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H5F00  'Horizontal total
	outport &H3D4, &H3F01  'Horizontal displayed
	outport &H3D4, &H4002  'Horizontal blanking start
	outport &H3D4, &H8203& 'Horizontal blanking end
	outport &H3D4, &H4A04  'Horizontal sync/retrace start
	outport &H3D4, &H9A05& 'Horizontal sync/retrace end
	outport &H3D4, &H2306  'Vertical total
	outport &H3D4, &HB207& 'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H6109  'Maximum scanline/character height
	outport &H3D4, &HA10   'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HFF12& 'Vertical displayed
	outport &H3D4, &H2013  'Offset/logical width
	outport &H3D4, &H715   'Vertical blanking start
	outport &H3D4, &H1A16& 'Vertical blanking end
	VGAWidth = 256
	VGAHeight = 256
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 400 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H5F00  'Horizontal total
	outport &H3D4, &H3F01  'Horizontal displayed
	outport &H3D4, &H4202  'Horizontal blanking start
	outport &H3D4, &H9F03& 'Horizontal blanking end
	outport &H3D4, &H4C04  'Horizontal sync/retrace start
	outport &H3D4, &H5     'Horizontal sync/retrace end
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &H2013  'Offset/logical width
	VGAWidth = 256
	VGAHeight = 400
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 480 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H5F00  'Horizontal total
	outport &H3D4, &H3F01  'Horizontal displayed
	outport &H3D4, &H4202  'Horizontal blanking start
	outport &H3D4, &H9F03& 'Horizontal blanking end
	outport &H3D4, &H4C04  'Horizontal sync/retrace start
	outport &H3D4, &H5     'Horizontal sync/retrace end
	outport &H3D4, &HD06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &HEA10& 'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HDF12& 'Vertical displayed
	outport &H3D4, &H2013  'Offset/logical width
	outport &H3D4, &HE715& 'Vertical blanking start
	outport &H3D4, &H616   'Vertical blanking end
	VGAWidth = 256
	VGAHeight = 480
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
 END IF
CASE 320
  IF Vert = 200 THEN
	'Sets mode 13h, then turns it into an unchained (planar), 4-page
	'320x200x256 mode.
	SCREEN 0                          'SCREEN 0 is needed to make
	SCREEN 13                         'the command SCREEN 13 effective
	LoadCharSet                       'Loads the 5x5 character set
	outport &H3C4, &H604              'Put the chain-4 mode of sequencer off
	memset &HA000&, 0, 0, &H10000     'SCREEN 13 only clears every
									  'fourth byte of each plane
	OUT &H3D4, &H11                            'Initializes the VGA to
	TempByte& = (INP(&H3D5) AND &H7F)          'accept any combination
	outport &H3D4, (&H11 OR SBL(TempByte&, 8)) 'of configuration
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
	'*bytes* is one fourth of the number of pixels on a line.
	VGAWidthBytes = (VGAWidth / 4)
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 240 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &HE3, &HE3         'Dot clock
	outport &H3D4, &HD06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4109  'Maximum scanline/character height
	outport &H3D4, &HEA10& 'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HDF12& 'Vertical displayed
	outport &H3D4, &HE715& 'Vertical blanking start
	outport &H3D4, &H616   'Vertical blanking end
	VGAHeight = 240
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 400 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H4009  'Maximum scanline/character height
	VGAWidth = 320
	VGAHeight = 400
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
 ELSEIF Vert = 100 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H4309  'Maximum scanline/character height
	VGAWidth = 320
	VGAHeight = 100
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
 ELSEIF Vert = 120 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &H4209  'Maximum scanline/character height
	VGAWidth = 320
	VGAHeight = 120
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 480 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE3        'Dot clock
	outport &H3D4, &HD06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &HEA10& 'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HDF12& 'Vertical displayed
	outport &H3D4, &HE715& 'Vertical blanking start
	outport &H3D4, &H616   'Vertical blanking end
	VGAWidth = 320
	VGAHeight = 480
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 0
  END IF
CASE 360
  IF Vert = 200 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6B00  'Horizontal total
	outport &H3D4, &H5901  'Horizontal displayed
	outport &H3D4, &H5A02  'Horizontal blanking start
	outport &H3D4, &H8E03& 'Horizontal blanking end
	outport &H3D4, &H5E04  'Horizontal sync/retrace start
	outport &H3D4, &H8A05& 'Horizontal sync/retrace end
	outport &H3D4, &H2D13  'Offset/logical width
	VGAWidth = 360
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 240 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6B00  'Horizontal total
	outport &H3D4, &H5901  'Horizontal displayed
	outport &H3D4, &H5A02  'Horizontal blanking start
	outport &H3D4, &H8E03& 'Horizontal blanking end
	outport &H3D4, &H5E04  'Horizontal sync/retrace start
	outport &H3D4, &H8A05& 'Horizontal sync/retrace end
	outport &H3D4, &HD06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4109  'Maximum scanline/character height
	outport &H3D4, &HEA10& 'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HDF12& 'Vertical displayed
	outport &H3D4, &H2D13  'Offset/logical width
	outport &H3D4, &HE715& 'Vertical blanking start
	outport &H3D4, &H616   'Vertical blanking end
	VGAWidth = 360
	VGAHeight = 240
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 270 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6B00  'Horizontal total
	outport &H3D4, &H5901  'Horizontal displayed
	outport &H3D4, &H5A02  'Horizontal blanking start
	outport &H3D4, &H8E03& 'Horizontal blanking end
	outport &H3D4, &H5E04  'Horizontal sync/retrace start
	outport &H3D4, &H8A05& 'Horizontal sync/retrace end
	outport &H3D4, &H3006  'Vertical total
	outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H6109  'Maximum scanline/character height
	outport &H3D4, &H2010  'Vertical sync/retrace start
	outport &H3D4, &HA911& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H1B12  'Vertical displayed
	outport &H3D4, &H2D13  'Offset/logical width
	outport &H3D4, &H1F15  'Vertical blanking start
	outport &H3D4, &H2F16  'Vertical blanking end
	VGAWidth = 360
	VGAHeight = 270
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 360 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6B00  'Horizontal total
	outport &H3D4, &H5901  'Horizontal displayed
	outport &H3D4, &H5A02  'Horizontal blanking start
	outport &H3D4, &H8E03& 'Horizontal blanking end
	outport &H3D4, &H5E04  'Horizontal sync/retrace start
	outport &H3D4, &H8A05& 'Horizontal sync/retrace end
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &H8810& 'Vertical sync/retrace start
	outport &H3D4, &H8511& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H6712  'Vertical displayed
	outport &H3D4, &H2D13  'Offset/logical width
	outport &H3D4, &H6D15  'Vertical blanking start
	outport &H3D4, &HBA16& 'Vertical blanking end
	VGAWidth = 360
	VGAHeight = 360
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 400 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6B00  'Horizontal total
	outport &H3D4, &H5901  'Horizontal displayed
	outport &H3D4, &H5A02  'Horizontal blanking start
	outport &H3D4, &H8E03& 'Horizontal blanking end
	outport &H3D4, &H5E04  'Horizontal sync/retrace start
	outport &H3D4, &H8A05& 'Horizontal sync/retrace end
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &H2D13  'Offset/logical width
	VGAWidth = 360
	VGAHeight = 400
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 0
  ELSEIF Vert = 480 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6B00  'Horizontal total
	outport &H3D4, &H5901  'Horizontal displayed
	outport &H3D4, &H5A02  'Horizontal blanking start
	outport &H3D4, &H8E03& 'Horizontal blanking end
	outport &H3D4, &H5E04  'Horizontal sync/retrace start
	outport &H3D4, &H8A05& 'Horizontal sync/retrace end
	outport &H3D4, &HD06   'Vertical total
	outport &H3D4, &H3E07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &HEA10& 'Vertical sync/retrace start
	outport &H3D4, &HAC11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &HDF12& 'Vertical displayed
	outport &H3D4, &H2D13  'Offset/logical width
	outport &H3D4, &HE715& 'Vertical blanking start
	outport &H3D4, &H616   'Vertical blanking end
	VGAWidth = 360
	VGAHeight = 480
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 0
  END IF
CASE 376
  IF Vert = 282 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6E00  'Horizontal total
	outport &H3D4, &H5D01  'Horizontal displayed
	outport &H3D4, &H5E02  'Horizontal blanking start
	outport &H3D4, &H9103& 'Horizontal blanking end
	outport &H3D4, &H6204  'Horizontal sync/retrace start
	outport &H3D4, &H8F05& 'Horizontal sync/retrace end
	outport &H3D4, &H6206  'Vertical total
	outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H6109  'Maximum scanline/character height
	outport &H3D4, &H3710  'Vertical sync/retrace start
	outport &H3D4, &H8911& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H3312  'Vertical displayed
	outport &H3D4, &H2F13  'Offset/logical width
	outport &H3D4, &H3C15  'Vertical blanking start
	outport &H3D4, &H5C16  'Vertical blanking end
	VGAWidth = 376
	VGAHeight = 282
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 308 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6E00  'Horizontal total
	outport &H3D4, &H5D01  'Horizontal displayed
	outport &H3D4, &H5E02  'Horizontal blanking start
	outport &H3D4, &H9103& 'Horizontal blanking end
	outport &H3D4, &H6204  'Horizontal sync/retrace start
	outport &H3D4, &H8F05& 'Horizontal sync/retrace end
	outport &H3D4, &H6206  'Vertical total
	outport &H3D4, &HF07   'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &H3710   'Vertical sync/retrace start
	outport &H3D4, &H8911& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H3312  'Vertical displayed
	outport &H3D4, &H2F13  'Offset/logical width
	outport &H3D4, &H3C15  'Vertical blanking start
	outport &H3D4, &H5C16& 'Vertical blanking end
	VGAWidth = 376
	VGAHeight = 308
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1
  ELSEIF Vert = 564 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H6E00  'Horizontal total
	outport &H3D4, &H5D01  'Horizontal displayed
	outport &H3D4, &H5E02  'Horizontal blanking start
	outport &H3D4, &H9103& 'Horizontal blanking end
	outport &H3D4, &H6204  'Horizontal sync/retrace start
	outport &H3D4, &H8F05& 'Horizontal sync/retrace end
	outport &H3D4, &H6206  'Vertical total
	outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H6009  'Maximum scanline/character height
	outport &H3D4, &H3710  'Vertical sync/retrace start
	outport &H3D4, &H8911& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H3312  'Vertical displayed
	outport &H3D4, &H2F13  'Offset/logical width
	outport &H3D4, &H3C15  'Vertical blanking start
	outport &H3D4, &H5C16  'Vertical blanking end
	VGAWidth = 376
	VGAHeight = 564
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 0
  END IF
CASE 400
  IF Vert = 150 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HA7        'Dot clock
	outport &H3D4, &H7100  'Horizontal total
	outport &H3D4, &H6301  'Horizontal displayed
	outport &H3D4, &H6402  'Horizontal blanking start
	outport &H3D4, &H9203& 'Horizontal blanking end
	outport &H3D4, &H6504  'Horizontal sync/retrace start
	outport &H3D4, &H8205& 'Horizontal sync/retrace end
	outport &H3D4, &H4606  'Vertical total
	outport &H3D4, &H1F07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4109  'Maximum scanline/character height
	outport &H3D4, &H3110  'Vertical sync/retrace start
	outport &H3D4, &H8011& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H2B12  'Vertical displayed
	outport &H3D4, &H3213  'Offset/logical width
	outport &H3D4, &H2F15  'Vertical blanking start
	outport &H3D4, &H4416  'Vertical blanking end
	VGAWidth = 400
	VGAHeight = 150
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1        '(for use with multipage modes)
  ELSEIF Vert = 300 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HA7        'Dot clock
	outport &H3D4, &H7100  'Horizontal total
	outport &H3D4, &H6301  'Horizontal displayed
	outport &H3D4, &H6402  'Horizontal blanking start
	outport &H3D4, &H9203& 'Horizontal blanking end
	outport &H3D4, &H6504  'Horizontal sync/retrace start
	outport &H3D4, &H8205& 'Horizontal sync/retrace end
	outport &H3D4, &H4606  'Vertical total
	outport &H3D4, &H1F07  'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H4009  'Maximum scanline/character height
	outport &H3D4, &H3110  'Vertical sync/retrace start
	outport &H3D4, &H8011& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H2B12  'Vertical displayed
	outport &H3D4, &H3213  'Offset/logical width
	outport &H3D4, &H2F15  'Vertical blanking start
	outport &H3D4, &H4416  'Vertical blanking end
	VGAWidth = 400
	VGAHeight = 300
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 1        '(for use with multipage modes)
  ELSEIF Vert = 600 THEN
	SetMode 320, 200       'First, set the unchained version of mode &H13
	outport &H3D4, &H2C11  'Second, turn off write protect
	OUT &H3C2, &HE7        'Dot clock
	outport &H3D4, &H7000  'Horizontal total
	outport &H3D4, &H6301  'Horizontal displayed
	outport &H3D4, &H6402  'Horizontal blanking start
	outport &H3D4, &H9203& 'Horizontal blanking end
	outport &H3D4, &H6504  'Horizontal sync/retrace start
	outport &H3D4, &H8205& 'Horizontal sync/retrace end
	outport &H3D4, &H7006  'Vertical total
	outport &H3D4, &HF007& 'Overflow (bit 8 of vertical counts)
	outport &H3D4, &H6009  'Maximum scanline/character height
	outport &H3D4, &H5B10  'Vertical sync/retrace start
	outport &H3D4, &H8C11& 'Vertical sync/retrace end and protect cr0-cr7
	outport &H3D4, &H5712  'Vertical displayed
	outport &H3D4, &H3213  'Offset/logical width
	outport &H3D4, &H5815  'Vertical blanking start
	outport &H3D4, &H7016  'Vertical blanking end
	VGAWidth = 400
	VGAHeight = 600
	VGAWidthBytes = (VGAWidth / 4) 'Offset/logical width was changed
	TotalPages = 65536 \ (CLNG(VGAWidthBytes) * VGAHeight)
	SetVisiblePage 0
	SetActivePage 0
  END IF
CASE ELSE
	SetMode 320, 200       'default to mode 13h unchained
END SELECT
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
outport &H3D5, SBR(VisibleStart&, 8)
outport &H3D4, &HD
outport &H3D5, VisibleStart& AND &HFF
END SUB

