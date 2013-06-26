'Basic Disk/File IO functions (much faster than QB has to offer)
'Higher level disk I/O functions
'Code by Dmitry Brant (me@dmitrybrant.com)
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
		   'DISK I/O FUNCTIONS
DECLARE FUNCTION GetBytesPerSector& () 'get number of bytes per sector on current drive
DECLARE FUNCTION GetSectPerCluster& () 'get number of sectors per cluster on current drive
DECLARE FUNCTION GetNumOfClusters& () 'get total number of clusters on current drive
DECLARE FUNCTION GetAvailClusters& () 'get number of available clusters on current drive
DECLARE FUNCTION GetFreeSpace& ()     'get amount of free space left on current drive
DECLARE FUNCTION GetTotalSpace& ()    'get total disk space on current drive
DECLARE FUNCTION Find$ (File$)        'returns first-found or next-found
					  'file or file-mask in current directory
DECLARE FUNCTION FOpen% (File$, CodeByte%) 'open a file and return a handle
DECLARE FUNCTION FClose% (Handle%)         'close the handle
DECLARE FUNCTION FRead% (Handle%, NumBytes&, BSeg%, BAdr%)  'read data from handle
DECLARE FUNCTION FWrite% (Handle%, NumBytes&, BSeg%, BAdr%) 'write data to handle
DECLARE FUNCTION FSeek% (Handle%, MethodCode%, Offset&)  'seek to offset in handle
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inregsx AS RegTypex
COMMON SHARED outRegsx AS RegTypex
DIM SHARED DTA AS STRING * 44  'needed for finding a file
				   'useless by itself

'Demo
CLS
PRINT "Total disk space:"; GetTotalSpace; "bytes."
PRINT "Free disk space:"; GetFreeSpace; "bytes."
PRINT : PRINT "Finding all EXE files in current directory:"

a$ = Find$("*.exe")   'find the first EXE file
c = 0
DO UNTIL a$ = ""
	a$ = Find$("") 'find next EXE file
	PRINT a$
	c = c + 1: IF c > 15 THEN EXIT DO
LOOP
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

FUNCTION GetFreeSpace&
GetFreeSpace& = GetSectPerCluster& * GetBytesPerSector& * GetAvailClusters&
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

FUNCTION GetTotalSpace&
GetTotalSpace& = GetSectPerCluster& * GetBytesPerSector& * GetNumOfClusters&
END FUNCTION

