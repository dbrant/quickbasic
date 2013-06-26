DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
                   'CD-ROM FUNCTIONS
DECLARE FUNCTION CDDrive% ()         'Obtain Drive letter (A=0,B=1...)
DECLARE FUNCTION GetCDVTOC$ (DR%)    'Get CD's Volume Table of Contents
DECLARE FUNCTION GetMSCDEXinst% (DR%) 'Check for MSCDEX
DECLARE FUNCTION GetMSCDEXver$ ()      'Get MSCDEX Version
DECLARE FUNCTION GetAbstract$ (Drive%)
DECLARE FUNCTION GetBiblio$ (Drive%)
DECLARE FUNCTION SendDriverRequest% (Drive%, ReqHeader$)
DECLARE FUNCTION GetCopyright$ (Drive%)
COMMON SHARED InRegs AS RegType
COMMON SHARED OutRegs AS RegType
COMMON SHARED inregsx AS RegTypeX
COMMON SHARED outregsx AS RegTypeX

FUNCTION CDDrive
    DR$ = STRING$(32, 0)
    inregsx.ax = &H150D
    inregsx.bx = SADD(DR$)
    inregsx.es = VARSEG(DR$)
    CALL interruptx(&H2F, inregsx, outregsx)
    CDDrive = ASC(DR$)
END FUNCTION

FUNCTION GetAbstract$ (Drive)
a0$ = STRING$(38, CHR$(0))
inregsx.ax = &H1503
inregsx.es = VARSEG(a0$)
inregsx.bx = SADD(a0$)
inregsx.cx = Drive
CALL interruptx(&H2F, inregsx, outregsx)
GetAbstract$ = a0$
END FUNCTION

FUNCTION GetBiblio$ (Drive)
a0$ = STRING$(38, CHR$(0))
inregsx.ax = &H1504
inregsx.es = VARSEG(a0$)
inregsx.bx = SADD(a0$)
inregsx.cx = Drive
CALL interruptx(&H2F, inregsx, outregsx)
GetBiblio$ = a0$
END FUNCTION

FUNCTION GetCDVTOC$ (DR)
    DR$ = STRING$(4096, 0)
    inregsx.ax = &H1505
    inregsx.bx = SADD(DR$)
    inregsx.cx = DR
    inregsx.DX = 0
    inregsx.es = VARSEG(DR$)
    CALL interruptx(&H2F, inregsx, outregsx)
  'AX=1 is normal and indicates a standard vol. descr.
  'AX=15 is 'Invalid Drive' and 21 is 'Not Ready'. 255 means no vol. desc.
    IF outregsx.ax > 1 THEN V$ = "Error" + STR$(outregsx.ax) ELSE V$ = DR$
    GetCDVTOC$ = V$
END FUNCTION

FUNCTION GetCopyright$ (Drive)
a0$ = STRING$(38, CHR$(0))
inregsx.ax = &H1502
inregsx.es = VARSEG(a0$)
inregsx.bx = SADD(a0$)
inregsx.cx = Drive
CALL interruptx(&H2F, inregsx, outregsx)
GetCopyright$ = a0$
END FUNCTION

FUNCTION GetMSCDEXinst (DR)
InRegs.ax = &H150B
InRegs.cx = DR
CALL INTERRUPT(&H2F, InRegs, OutRegs)
IF OutRegs.bx = &HADAD THEN GetMSCDEXinst = 1 ELSE GetMSCDEXinst = 0
END FUNCTION

FUNCTION GetMSCDEXver$
InRegs.ax = &H150C
CALL INTERRUPT(&H2F, InRegs, OutRegs)
b0$ = STR$(OutRegs.bx \ 256)
b1$ = STR$(OutRegs.bx AND &HFF)
GetMSCDEXver$ = RIGHT$(b0$, LEN(b0$) - 1) + "." + RIGHT$(b1$, LEN(b1$) - 1)
END FUNCTION

FUNCTION SendDriverRequest (Drive, ReqHeader$)
inregsx.ax = &H1510
inregsx.cx = Drive
inregsx.es = VARSEG(ReqHeader$)
inregsx.bx = SADD(ReqHeader$)
CALL interruptx(&H2F, inregsx, outregsx)
SendDriverRequest = outregsx.ax
END FUNCTION

