DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
                   'WINDOWS FUNCTIONS
DECLARE FUNCTION GetWinVer$ ()           'Get the Windows version
DECLARE FUNCTION GetGDIInfo% (Index%)    'Get current WINOLDAP info
DECLARE FUNCTION ReadBit (Number, Bit)
DECLARE SUB GetAPIEntryPoint (VxdID&)    'Get API entry point for VxD
DECLARE FUNCTION GetCurrentVM% ()        'Get the current Virtual Machine
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inregsx AS RegTypex
COMMON SHARED outregsx AS RegTypex

CLS
x = 0
FOR i& = &H0 TO 65535
GetAPIEntryPoint (i&)
IF outregsx.es <> 0 THEN
PRINT "Entry Point for VxD "; HEX$(i&); ": ";
PRINT HEX$(outregsx.es) + ":" + HEX$(outregsx.di)
END IF
NEXT i&
SYSTEM


CLS
RESTORE GDIInfo
FOR i = 1 TO 25
   READ Offset, Desc$
   a& = GetGDIInfo(Offset)
   IF a& < 0 THEN a& = 65536 + a&
   IF Offset = 2 THEN C0 = a&
   IF Offset = &H1C THEN C1 = a&
   IF Offset = &H1E THEN L0 = a&
   IF Offset = &H20 THEN P0 = a&
   IF Offset = &H22 THEN T0 = a&
   IF Offset = &H24 THEN C2 = a&
   IF Offset = &H26 THEN B2 = a&
   PRINT Desc$ + STRING$(40 - LEN(Desc$), "."); a&
   IF i = 22 THEN DO: LOOP UNTIL INKEY$ <> ""
NEXT i

RESTORE GDIDeviceClass
COLOR 15: PRINT "Device Classification:": COLOR 7
FOR i = 1 TO 7
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(C0, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""

RESTORE GDICurveCapabil
COLOR 15: PRINT "Curve Capabilities:": COLOR 7
FOR i = 1 TO 8
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(C1, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""

RESTORE GDILineCapabil
COLOR 15: PRINT "Line Capabilities:": COLOR 7
FOR i = 1 TO 7
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(L0, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""

RESTORE GDIPolygonCapabil
COLOR 15: PRINT "Polygon Capabilities:": COLOR 7
FOR i = 1 TO 7
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(P0, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""

RESTORE GDITextCapabil
COLOR 15: PRINT "Text Capabilities:": COLOR 7
FOR i = 1 TO 15
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(T0, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""

RESTORE GDIClippingCapabil
COLOR 15: PRINT "Clipping Capabilities:": COLOR 7
FOR i = 1 TO 2
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(C2, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""

RESTORE GDIBitbltCapabil
COLOR 15: PRINT "BitBLT Capabilities:": COLOR 7
FOR i = 1 TO 4
   READ Bt, Desc$
   PRINT Desc$ + STRING$(40 - LEN(Desc$), ".");
   IF ReadBit(B2, Bt) = 1 THEN PRINT "Yes" ELSE PRINT "No"
NEXT i
DO: LOOP UNTIL INKEY$ <> ""


SYSTEM



SUB GetAPIEntryPoint (VxdID&)
inregsx.ax = &H1684
IF VxdID& > 32767 THEN V0 = -(65536 - VxdID&) ELSE V0 = VxdID&
inregsx.bx = V0
inregsx.es = 0
inregsx.di = 0
CALL interruptx(&H2F, inregsx, outregsx)
END SUB

FUNCTION GetCurrentVM
inregs.ax = &H1683
CALL interrupt(&H2F, inregs, outregs)
GetCurrentVM = outregs.bx
END FUNCTION

FUNCTION GetGDIInfo (Index)
inregs.ax = &H170A
inregs.dx = Index
CALL interrupt(&H2F, inregs, outregs)
GetGDIInfo = outregs.ax
END FUNCTION

FUNCTION GetWinVer$
inregs.ax = &H160A
CALL interrupt(&H2F, inregs, outregs)
r0$ = STR$(outregs.bx \ 256)
r1$ = STR$(outregs.bx AND &HFF)
GetWinVer$ = RIGHT$(r0$, LEN(r0$) - 1) + "." + RIGHT$(r1$, LEN(r1$) - 1)
END FUNCTION

FUNCTION ReadBit (Number, Bit)
Value = 2 ^ Bit
IF (Number AND Value) = Value THEN f = 1 ELSE f = 0
ReadBit = f
END FUNCTION

