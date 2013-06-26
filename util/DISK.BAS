DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
		   'DISK I/O FUNCTIONS
DECLARE FUNCTION GetBytesPerSector& ()
DECLARE FUNCTION GetSectPerCluster& ()
DECLARE FUNCTION GetNumOfClusters& ()
DECLARE FUNCTION GetAvailClusters& ()
DECLARE FUNCTION Find$ (File$)
DECLARE FUNCTION FOpen% (File$, CodeByte%)
DECLARE FUNCTION FClose% (Handle%)
DECLARE FUNCTION FRead% (Handle%, NumBytes&, BSeg%, BAdr%)
DECLARE FUNCTION FWrite% (Handle%, NumBytes&, BSeg%, BAdr%)
DECLARE FUNCTION FSeek% (Handle%, MethodCode%, Offset&)
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inregsx AS RegTypex
COMMON SHARED outRegsx AS RegTypex
DIM SHARED DTA AS STRING * 44

CLS
SCREEN 13
h1 = FOpen("c:\dmiprog\qb45\temporal.exe", 0)
PRINT h1
a = FRead(h1, 64000, &HA000, 0)
PRINT a
SYSTEM

FUNCTION FClose (Handle)
inregs.ax = &H3E00
inregs.bx = Handle
CALL interrupt(&H21, inregs, outregs)
FClose = outregs.ax
END FUNCTION

FUNCTION Find$ (File$)
	inregsx.ax = &H1A00
	inregsx.dx = VARPTR(DTA)
	inregsx.ds = -1
	CALL INTERRUPTX(&H21, inregsx, outRegsx)
	IF LEN(File$) THEN
	   FileZ$ = File$ + CHR$(0)
	   inregsx.ax = &H4E00
	   inregsx.cx = 0
	   inregsx.dx = SADD(FileZ$)
	   inregsx.ds = -1
	ELSE
	   inregsx.ax = &H4F00
	END IF
	CALL INTERRUPTX(&H21, inregsx, outRegsx)
	IF outRegsx.Flags AND 1 THEN
	   Find$ = ""
	ELSE
	   Null = INSTR(31, DTA, CHR$(0))
	   Find$ = MID$(DTA, 31, Null - 30)
	END IF
END FUNCTION

FUNCTION FOpen (File$, CodeByte)
IF RIGHT$(File$, 1) <> CHR$(0) THEN File$ = File$ + CHR$(0)
inregsx.ax = &H3D00 OR CodeByte
inregsx.ds = VARSEG(File$)
inregsx.dx = SADD(File$)
CALL INTERRUPTX(&H21, inregsx, outRegsx)
FOpen = outRegsx.ax
END FUNCTION

FUNCTION FRead (Handle, NumBytes&, BSeg, BAdr)
inregsx.ax = &H3F00
inregsx.bx = Handle
IF NumBytes& > 32767 THEN NumBytes& = -32768 + (NumBytes& - 32767)
inregsx.cx = NumBytes&
inregsx.ds = BSeg
inregsx.dx = BAdr
CALL INTERRUPTX(&H21, inregsx, outRegsx)
FRead = outRegsx.ax
END FUNCTION

FUNCTION FSeek (Handle, MethodCode, Offset&)
inregs.ax = &H4200 OR MethodCode
inregs.bx = Handle
IF Offset& > 65535 THEN
   p& = Offset& \ 65536
     IF p& > 32767 THEN p& = -32768 + (p& - 32767)
   inregs.cx = p&
ELSE
   inregs.cx = 0
END IF
p& = Offset& MOD &H10000
IF p& > 32767 THEN p& = -32768 + (p& - 32767)
inregs.dx = p&
CALL interrupt(&H21, inregs, outregs)
FSeek = outregs.ax
END FUNCTION

FUNCTION FWrite (Handle, NumBytes&, BSeg, BAdr)
inregsx.ax = &H4000
inregsx.bx = Handle
IF NumBytes& > 32767 THEN NumBytes& = -32768 + (NumBytes& - 32767)
inregsx.cx = NumBytes&
inregsx.ds = BSeg
inregsx.dx = BAdr
CALL INTERRUPTX(&H21, inregsx, outRegsx)
FWrite = outRegsx.ax
END FUNCTION

FUNCTION GetAvailClusters&
inregs.ax = &H3600
inregs.dx = 0
CALL interrupt(&H21, inregs, outregs)
GetAvailClusters& = outregs.bx
END FUNCTION

FUNCTION GetBytesPerSector&
inregs.ax = &H3600
inregs.dx = 0
CALL interrupt(&H21, inregs, outregs)
GetBytesPerSector& = outregs.cx
END FUNCTION

FUNCTION GetNumOfClusters&
inregs.ax = &H3600
inregs.dx = 0
CALL interrupt(&H21, inregs, outregs)
R& = outregs.dx
IF R& < 0 THEN R& = 32767 + (32768 + R&)
GetNumOfClusters& = R&
END FUNCTION

FUNCTION GetSectPerCluster&
inregs.ax = &H3600
inregs.dx = 0
CALL interrupt(&H21, inregs, outregs)
GetSectPerCluster& = outregs.ax
END FUNCTION

