'All-purpose extended DOS functions
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
		   'EXTENDED/DOS FUNCTIONS
DECLARE FUNCTION GetCurDrive% (d%)     'Get current drive
DECLARE FUNCTION GetMode% ()           'Get current video mode
DECLARE FUNCTION GetTextCols% ()       'Get num of text columns
DECLARE FUNCTION GetBiosDate$ ()       'Get the BIOS date
DECLARE FUNCTION GetMemSize% ()        'Get number of KB of base memory
DECLARE FUNCTION GetDisplay% ()        'Get DCC of current display
DECLARE FUNCTION GetDosVer$ ()         'Get the Dos Version
DECLARE FUNCTION DosInHMA% ()          'Check if DOS is loaded in high mem.
DECLARE FUNCTION GetSetVerify% (Flag1%, Flag2%)   'Get/set verify flag
DECLARE FUNCTION GetAPPEND% ()         'Check if Append is installed
DECLARE FUNCTION GetASSIGN% ()         'Check if Assign is installed
DECLARE FUNCTION GetSHARE% ()          'Check if Share is installed
DECLARE SUB Reboot (Optn%)             'Reboot warm or cold
DECLARE SUB ExitWithErrorlevel (ER%)   'Exit with Errorlevel
DECLARE SUB SetKBRate (InitDelay%, RepeatRate%) 'Set TypeMatic rate
DECLARE FUNCTION GetDPB$ (Drive%)      'Get Drive Parameter Block (DPB)
DECLARE FUNCTION GetBootDrive% ()      'Get the Boot Drive
DECLARE FUNCTION GetDefaultDrive% ()   'Get the current default drive
DECLARE FUNCTION GetFileAttr% (File$)
DECLARE FUNCTION SetFileAttr% (File$, Attr%)
DECLARE FUNCTION FGetDateTime% (Handle%) 'Returns file's date & time in CX,DX
DECLARE FUNCTION FSetDateTime% (Handle%, Date%, Time%)
DECLARE FUNCTION GetError$ (ErrNum%)     'Returns error string from error number
DECLARE FUNCTION GetErrorNum% ()         'Get extended error number
DECLARE FUNCTION EnableDrive% (Drive%)   'Enable drive (0=A:)
DECLARE FUNCTION DisableDrive% (Drive%)  'Disable drive (0=A:)
DECLARE FUNCTION GetTrueName$ (File$)    'Canonicalize filename or path
DECLARE FUNCTION GetDiskInfo$ (Drive%)
DECLARE FUNCTION SetDiskInfo% (Drive%, info$)
COMMON SHARED inregs AS RegType
COMMON SHARED outregs AS RegType
COMMON SHARED inregsx AS RegTypeX
COMMON SHARED outregsx AS RegTypeX

'Demo
CLS
PRINT "DOS Version: "; GetDosVer
PRINT "BIOS Date: "; GetBiosDate
PRINT "Base Memory Size:"; GetMemSize; "KB"
PRINT "DOS in high memory? ";
	IF DosInHMA THEN PRINT "Yes" ELSE PRINT "No"
PRINT "Share installed? ";
	IF GetSHARE THEN PRINT "Yes" ELSE PRINT "No"
PRINT "Verify Flag: ";
	IF GetSetVerify(0, 0) THEN PRINT "On" ELSE PRINT "Off"
PRINT : PRINT "Current video mode:"; GetMode
PRINT "Number of text columns:"; GetTextCols
PRINT : PRINT "System boot drive: "; CHR$(GetBootDrive + 65); ":"
PRINT "Default drive: "; CHR$(GetDefaultDrive + 65); ":"
SYSTEM

FUNCTION DisableDrive (Drive)
inregs.ax = &H5F08
inregs.dx = Drive
CALL INTERRUPT(&H21, inregs, outregs)
DisableDrive = outregs.ax
END FUNCTION

FUNCTION DosInHMA
inregs.ax = &H3306
CALL INTERRUPT(&H21, inregs, outregs)
IF outregs.dx AND &H1000 = &H1000 THEN DosInHMA = 1 ELSE DosInHMA = 0
END FUNCTION

FUNCTION EnableDrive (Drive)
inregs.ax = &H5F07
inregs.dx = Drive
CALL INTERRUPT(&H21, inregs, outregs)
EnableDrive = outregs.ax
END FUNCTION

SUB ExitWithErrorlevel (ER)
inregs.ax = &H4C00 + (ER AND 255)
CALL INTERRUPT(&H21, inregs, outregs)
END SUB

FUNCTION FGetDateTime (Handle)
inregs.ax = &H5700
inregs.bx = Handle
CALL INTERRUPT(&H21, inregs, outregs)
FGetDateTime = outregs.ax
END FUNCTION

FUNCTION FSetDateTime (Handle, Date, Time)
inregs.ax = &H5701
inregs.bx = Handle
inregs.cx = Time
inregs.dx = Date
CALL INTERRUPT(&H21, inregs, outregs)
FSetDateTime = outregs.ax
END FUNCTION

FUNCTION GetAPPEND
inregs.ax = &HB700
CALL INTERRUPT(&H2F, inregs, outregs)
a1 = outregs.ax AND &HFF
IF a1 = &HFF THEN GetAPPEND = 1 ELSE GetAPPEND = 0
END FUNCTION

FUNCTION GetASSIGN
inregs.ax = &H600
CALL INTERRUPT(&H2F, inregs, outregs)
a1 = outregs.ax AND &HFF
IF a1 <> 0 THEN GetASSIGN = 1 ELSE GetASSIGN = 0
END FUNCTION

FUNCTION GetBiosDate$
DEF SEG = &HF000
R$ = CHR$(PEEK(&HFFF5)) + CHR$(PEEK(&HFFF6)) + CHR$(PEEK(&HFFF7)) + CHR$(PEEK(&HFFF8))
R$ = R$ + CHR$(PEEK(&HFFF9)) + CHR$(PEEK(&HFFFA)) + CHR$(PEEK(&HFFFB)) + CHR$(PEEK(&HFFFC))
DEF SEG
GetBiosDate$ = R$
END FUNCTION

FUNCTION GetBootDrive
inregs.ax = &H3305
CALL INTERRUPT(&H21, inregs, outregs)
GetBootDrive = (outregs.dx AND &HFF) - 1
END FUNCTION

FUNCTION GetCurDrive (d)
inregs.ax = &H1900 OR d
CALL INTERRUPT(&H21, inregs, outregs)
GetCurDrive = outregs.ax
END FUNCTION

FUNCTION GetDefaultDrive
inregs.ax = &H1900
CALL INTERRUPT(&H21, inregs, outregs)
GetDefaultDrive = outregs.ax AND &HFF
END FUNCTION

FUNCTION GetDiskInfo$ (Drive)
a0$ = STRING$(19, CHR$(0))
inregsx.ax = &H6900
inregsx.bx = Drive + 1
inregsx.ds = VARSEG(a0$)
inregsx.dx = SADD(a0$)
CALL INTERRUPTX(&H21, inregsx, outregsx)
GetDiskInfo$ = a0$
END FUNCTION

FUNCTION GetDisplay%
inregs.ax = &H1A00
CALL INTERRUPT(&H10, inregs, outregs)
GetDisplay% = outregs.bx
END FUNCTION

FUNCTION GetDosVer$
inregs.ax = &H3306
CALL INTERRUPT(&H21, inregs, outregs)
G0$ = STR$(outregs.bx AND &HFF)
D0$ = STR$(outregs.bx \ 256)
R0$ = STR$(outregs.dx AND 7)
GetDosVer$ = RIGHT$(G0$, LEN(G0$) - 1) + "." + RIGHT$(D0$, LEN(D0$) - 1) + "." + RIGHT$(R0$, LEN(R0$) - 1)
END FUNCTION

FUNCTION GetDPB$ (Drive)
inregsx.ax = &H3200
inregsx.dx = Drive + 1
CALL INTERRUPTX(&H21, inregsx, outregsx)
a0$ = ""
DEF SEG = outregsx.ds
FOR i = 0 TO &H21
a0$ = a0$ + CHR$(PEEK(outregsx.bx + i))
NEXT i
GetDPB$ = a0$
END FUNCTION

FUNCTION GetError$ (ErrNum)
'RESTORE DOSErrorCodes
FOR i = 1 TO 83
 READ N0, E0$
 IF N0 = ErrNum THEN GOTO Found
NEXT i
E0$ = "Unknown Error"
Found:
GetError$ = E0$
END FUNCTION

FUNCTION GetErrorNum
inregs.ax = &H5900
inregs.bx = 0
CALL INTERRUPT(&H21, inregs, outregs)
GetErrorNum = outregs.ax
END FUNCTION

FUNCTION GetFileAttr (File$)
IF RIGHT$(File$, 1) <> CHR$(0) THEN File$ = File$ + CHR$(0)
inregsx.ax = &H4300
inregsx.ds = VARSEG(File$)
inregsx.dx = SADD(File$)
CALL INTERRUPTX(&H21, inregsx, outregsx)
GetFileAttr = outregsx.cx
END FUNCTION

FUNCTION GetMemSize%
CALL INTERRUPT(&H12, inregs, outregs)
GetMemSize% = outregs.ax
END FUNCTION

FUNCTION GetMode%
inregs.ax = &HF00
CALL INTERRUPT(&H10, inregs, outregs)
GetMode% = outregs.ax AND 255
END FUNCTION

FUNCTION GetSetVerify (Flag1, Flag2)
IF Flag1 = 1 THEN
inregs.ax = &H2E00 OR Flag2
CALL INTERRUPT(&H21, inregs, outregs)
ELSE
inregs.ax = &H5400
CALL INTERRUPT(&H21, inregs, outregs)
END IF
GetSetVerify = outregs.ax AND &HFF
END FUNCTION

FUNCTION GetSHARE
inregs.ax = &H1000
CALL INTERRUPT(&H2F, inregs, outregs)
a1 = outregs.ax AND &HFF
IF a1 = 0 OR a1 = 1 THEN GetSHARE = 0
IF a1 = &HFF THEN GetSHARE = 1
END FUNCTION

FUNCTION GetTextCols%
inregs.ax = &HF00
CALL INTERRUPT(&H10, inregs, outregs)
GetTextCols% = (outregs.ax AND &HFF00) / 256
END FUNCTION

FUNCTION GetTrueName$ (File$)
IF RIGHT$(File$, 1) <> CHR$(0) THEN File$ = File$ + CHR$(0)
a0$ = STRING$(128, CHR$(0))
inregsx.ax = &H6000
inregsx.ds = VARSEG(File$)
inregsx.si = SADD(File$)
inregsx.es = VARSEG(a0$)
inregsx.di = SADD(a0$)
CALL INTERRUPTX(&H21, inregsx, outregsx)
GetTrueName$ = a0$
END FUNCTION

SUB Reboot (Optn)
'Optn: 0=Warm Boot  1=Cold Boot
DEF SEG = 0
IF Optn = 0 THEN
POKE &H473, &H12 ' Setup for Warm Boot
POKE &H472, &H34
ELSE
POKE &H472, &H12 ' Setup for Cold Boot
POKE &H473, &H34 ' tho not really nessary
END IF
DEF SEG = &HFFFF
Of% = 0
CALL Absolute(Of%)
END SUB

FUNCTION SetDiskInfo (Drive, info$)
inregsx.ax = &H6901
inregsx.bx = Drive + 1
inregsx.ds = VARSEG(info$)
inregsx.dx = SADD(info$)
CALL INTERRUPTX(&H21, inregsx, outregsx)
SetDiskInfo = outregsx.ax
END FUNCTION

FUNCTION SetFileAttr (File$, Attr)
IF RIGHT$(File$, 1) <> CHR$(0) THEN File$ = File$ + CHR$(0)
inregsx.ax = &H4301
inregsx.cx = Attr
inregsx.ds = VARSEG(File$)
inregsx.dx = SADD(File$)
CALL INTERRUPTX(&H21, inregsx, outregsx)
SetFileAttr = outregsx.ax
END FUNCTION

SUB SetKBRate (InitDelay%, RepeatRate%)
   inregs.ax = &H305
   inregs.bx = (InitDelay% AND 3) + (RepeatRate% AND 31)
   CALL INTERRUPT(&H16, inregs, outregs)
END SUB

