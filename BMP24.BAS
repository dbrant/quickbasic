DEFINT A-Z
'SVGA VESA Demo by Dmitry Brant.
'Use freely

'To run this program, you must load QuickBasic with the following
'command line:
'
'     QB /L LIBVESA.QLB
'
'   (assuming that LIBVESA.QLB is in the same directory as QuickBasic)
'The QLB file already contains the standard QB.LIB, so don't worry.

'$INCLUDE: 'qb.bi'
DECLARE SUB SetAPage CDECL ALIAS "_SetActPage" (BYVAL page%)
DECLARE SUB SetVisPage CDECL ALIAS "_ViewPage" (BYVAL page%)
DECLARE SUB SVGAinit CDECL ALIAS "_SVGAInit" (BYVAL gran%, BYVAL wid%, BYVAL hei%, BYVAL bppp%, BYVAL winaseg%, BYVAL pages%)
DECLARE SUB put15 CDECL ALIAS "_PutPixel15" (BYVAL x%, BYVAL y%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB put16 CDECL ALIAS "_PutPixel16" (BYVAL x%, BYVAL y%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB put24 CDECL ALIAS "_PutPixel24" (BYVAL x%, BYVAL y%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB line15 CDECL ALIAS "_line15" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB line16 CDECL ALIAS "_line16" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB line24 CDECL ALIAS "_line24" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB box15 CDECL ALIAS "_box15" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB box16 CDECL ALIAS "_box16" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB box24 CDECL ALIAS "_box24" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB grad24 CDECL ALIAS "_grad24" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r1%, BYVAL g1%, BYVAL b1%, BYVAL r2%, BYVAL g2%, BYVAL b2%, BYVAL typ%)
DECLARE SUB grad15 CDECL ALIAS "_grad15" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r1%, BYVAL g1%, BYVAL b1%, BYVAL r2%, BYVAL g2%, BYVAL b2%, BYVAL typ%)
DECLARE SUB grad16 CDECL ALIAS "_grad16" (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL r1%, BYVAL g1%, BYVAL b1%, BYVAL r2%, BYVAL g2%, BYVAL b2%, BYVAL typ%)
DECLARE FUNCTION DetectVESA ()
DECLARE FUNCTION GetVideoMode ()
DECLARE FUNCTION SetSVGAMode (mode)
DECLARE SUB BMPLoad (FileName$)

' Mode Number   Resolution      Colors
'    &H100       640x400          256     
'    &H101       640x480          256   
'    &H103       800x600          256  
'    &H105       1024x768         256
'    &H107       1280x1024        256 
'    &H10D       320x200          32k       
'    &H10E       320x200          64k    
'    &H10F       320x200          16m     
'    &H110       640x480          32k       
'    &H111       640x480          64k  
'    &H112       640x480          16m
'    &H113       800x600          32k      
'    &H114       800x600          64k      
'    &H116       1024x768         32k     
'    &H117       1024x768         64k      
'    &H119       1280x1024        32k     
'    &H11A       1280x1024        64k      



COMMON SHARED Xres, yres, yOffset, curBank, winGran, oldMode, ppSeg, ppOfs, bpp

TYPE VGAInfoBlock
    VESASignature AS STRING * 4
    VESAVersion AS INTEGER
    OEMStringPtr AS LONG
    Capabilities AS STRING * 4
    VideoModePtr AS LONG
    TotalMemory AS INTEGER
    reserved AS STRING * 236
END TYPE

TYPE ModeInfoBlock
    ModeAttributes AS INTEGER
    WinAAttributes AS STRING * 1
    WinBAttributes AS STRING * 1
    WinGranularity AS INTEGER
    WinSize AS INTEGER
    WinASegment AS INTEGER
    WinBSegment AS INTEGER
    WinFuncPtr AS LONG
    BytesPerScanLine AS INTEGER
    XResolution AS INTEGER
    YResolution AS INTEGER
    XCharSize AS STRING * 1
    YCharSize AS STRING * 1
    NumberOfPlanes AS STRING * 1
    BitsPerPixel AS STRING * 1
    NumberOfBanks AS STRING * 1
    MemoryModel AS STRING * 1
    BankSize AS STRING * 1
    NumberOfImagePages AS STRING * 1
    Rsvd AS STRING * 1
    RedMaskSize AS STRING * 1
    RedFieldPosition AS STRING * 1
    GreenMaskSize AS STRING * 1
    GreenFieldPosition AS STRING * 1
    BlueMaskSize AS STRING * 1
    BlueFieldPosition AS STRING * 1
    RsvdMaskSize AS STRING * 1
    DirectColorModeInfo AS STRING * 1
    reserved AS STRING * 216
END TYPE

DIM SHARED Regs AS RegTypeX


TYPE Header
  BMPIdentifier AS STRING * 2
  BMPFileSize AS LONG: BMPReserved AS LONG: BMPDataOffset AS LONG
  BMPHeaderSize AS LONG: BMPWidth AS LONG: BMPHeight AS LONG
  BMPPlanes AS INTEGER: BMPBitsPerPixel AS INTEGER: BMPCompression AS LONG
  BMPDataSize AS LONG: BMPHorizontalRes AS LONG: BMPVerticalRes AS LONG
  BMPNoOfColors AS LONG: BMPImportantColors AS LONG
END TYPE
TYPE PalEntry
  Blue AS STRING * 1: Green AS STRING * 1: Red AS STRING * 1: reserved AS STRING * 1
END TYPE
TYPE Pal2Entry
  Red AS STRING * 1: Green AS STRING * 1: Blue AS STRING * 1
END TYPE



CONST FieldColor = 7, InfoColor = 15

DIM SHARED Pic1 AS Header, Pal AS Pal2Entry, Byte AS STRING * 1
DIM SHARED RLE1 AS STRING * 1, RLE2 AS STRING * 1, RLE3 AS STRING * 1


start:

SCREEN 13
IF DetectVESA THEN
  pagesAvailable = SetSVGAMode(&H112)
  IF pagesAvailable = 0 THEN
    PRINT "Press any key."
    DO: LOOP UNTIL INKEY$ <> ""
    SYSTEM
  END IF
ELSE
  CLS
  PRINT "VESA Compatibility not detected."
  PRINT "Recommend installing UNIVBE.EXE."
  SYSTEM
END IF

BMPLoad "img1.bmp"
SLEEP

SCREEN 0: WIDTH 80, 25
SYSTEM

SUB BMPLoad (FileName$)

OPEN FileName$ FOR BINARY AS #1
  IF LOF(1) = 0 THEN CLOSE #1: KILL FileName$: EXIT SUB
  GET #1, , Pic1
  a$ = "  "
  'IF Pic1.BMPFileSize <> LOF(1) THEN EXIT SUB

y = Pic1.BMPHeight - 1
  SELECT CASE Pic1.BMPCompression
  CASE 0
    SELECT CASE (Pic1.BMPBitsPerPixel)
    CASE 24
      FOR y = Pic1.BMPHeight - 1 TO 0 STEP -1
        FOR x = 0 TO Pic1.BMPWidth - 1
          GET #1, , Pal
          put24 x, y, ASC(Pal.Red), ASC(Pal.Green), ASC(Pal.Blue)
        NEXT x
        GET #1, , a$
      NEXT y
    CASE 8
      FOR y = Pic1.BMPHeight - 1 TO 0 STEP -1: FOR x = 0 TO Pic1.BMPWidth - 1
        GET #1, , Byte: PSET (x, y), ASC(Byte)
      NEXT: NEXT
    CASE 4
      FOR y = Pic1.BMPHeight - 1 TO 0 STEP -1: FOR x = 0 TO Pic1.BMPWidth - 1 STEP 2
        GET #1, , Byte: PSET (x, y), INT(ASC(Byte) / 16): PSET (x + 1, y), ASC(Byte) MOD 16
      NEXT: NEXT
    CASE 2
      FOR y = Pic1.BMPHeight - 1 TO 0 STEP -1: FOR x = 0 TO Pic1.BMPWidth - 1 STEP 8
        GET #1, , Byte
        FOR Mono = 0 TO 7
          IF ASC(Byte) AND 2 ^ Mono THEN
            IF x AND 1 THEN
              PSET (x + Mono, y), 1
            ELSE
              PSET (x + (7 - Mono), y), 1
            END IF
          ELSE
            IF x AND 1 THEN
              PSET (x + Mono, y), 0
            ELSE
              PSET (x + (7 - Mono), y), 0
            END IF
          END IF
        NEXT
      NEXT: NEXT
    END SELECT
  CASE 1
    DO
      GET #1, , RLE1
      SELECT CASE ASC(RLE1)
      CASE 0
        GET #1, , RLE2
        SELECT CASE ASC(RLE2)
        CASE 0: y = y - 1: x = 0
        CASE 1: GOTO EndOfBitmap
        CASE 2: GET #1, , RLE3: x = x + ASC(RLE3): GET #1, , RLE3: y = y - ASC(RLE3)
        CASE ELSE
          FOR RLE = 1 TO ASC(RLE2): GET #1, , RLE3: PSET (x, y), ASC(RLE3): x = x + 1: NEXT
          IF ASC(RLE2) AND 1 THEN GET #1, , RLE3
        END SELECT
      CASE ELSE
        GET #1, , RLE2
        FOR RLE = 1 TO ASC(RLE1): PSET (x, y), ASC(RLE2): x = x + 1: NEXT
      END SELECT
    LOOP
  CASE 2
    DO
      GET #1, , RLE1
      SELECT CASE ASC(RLE1)
      CASE 0
        GET #1, , RLE2
        SELECT CASE ASC(RLE2)
        CASE 0: y = y - 1: x = 0
        CASE 1: GOTO EndOfBitmap
        CASE 2: GET #1, , RLE3: x = x + ASC(RLE3): GET #1, , RLE3: y = y - ASC(RLE3)
        CASE ELSE
          FOR RLE = 1 TO ASC(RLE2) STEP 2
            GET #1, , RLE3: PSET (x, y), INT(ASC(RLE3) / 16): PSET (x + 1, y), ASC(RLE3) MOD 16: x = x + 2
          NEXT
          IF INT(ASC(RLE2) / 2) AND 1 THEN GET #1, , RLE3
        END SELECT
      CASE ELSE
        GET #1, , RLE2
        FOR RLE = 1 TO ASC(RLE1)
          IF RLE AND 1 THEN
            PSET (x, y), INT(ASC(RLE2) / 16)
          ELSE
            PSET (x, y), ASC(RLE2) MOD 16
          END IF
          x = x + 1
        NEXT
      END SELECT
    LOOP
  CASE 3
    PRINT "Bit Fields Not Supported!": GOTO EndOfBitmap
  END SELECT
EndOfBitmap:
CLOSE #1
END SUB

' Detect to see if a VESA compatible graphics card is present.
FUNCTION DetectVESA

DIM VGAInfo AS VGAInfoBlock

Regs.ax = &H4F00
Regs.es = VARSEG(VGAInfo)
Regs.di = VARPTR(VGAInfo)
CALL INTERRUPTX(&H10, Regs, Regs)

IF Regs.ax = &H4F THEN
  DetectVESA = 1
  PRINT VGAInfo.VESASignature; RTRIM$(STR$(VGAInfo.VESAVersion \ 256)); "."; LTRIM$(STR$(VGAInfo.VESAVersion AND 255))
ELSE
  DetectVESA = 0
  PRINT "Error - cannot detect VESA."
END IF

END FUNCTION

' Returns the current video mode.
FUNCTION GetVideoMode

Regs.ax = &H4F03
CALL INTERRUPTX(&H10, Regs, Regs)
GetVideoMode = Regs.bx

END FUNCTION

' Sets an SVGA mode.
FUNCTION SetSVGAMode (mode)

oldMode = GetVideoMode                          'Get the current mode.

DIM ModeInfo AS ModeInfoBlock                   'Get info on the desired
Regs.ax = &H4F01                                'SVGA mode.
Regs.cx = mode
Regs.es = VARSEG(ModeInfo)
Regs.di = VARPTR(ModeInfo)
CALL INTERRUPTX(&H10, Regs, Regs)

IF (ModeInfo.ModeAttributes AND 1) = 0 THEN     'Bit 1 = 0 then mode not
  CLS
  PRINT "Error - screen mode not available."    'available.
  SetSVGAMode = 0
  PRINT ModeInfo.BitsPerPixel
  EXIT FUNCTION
END IF
winGran = 64 \ ModeInfo.WinGranularity          'Window granularity adjusted.
Xres = ModeInfo.XResolution                     'Get screen resolution.
yres = ModeInfo.YResolution
IF ModeInfo.WinSize < 64 THEN
  CLS
  PRINT "Error - this program requires at least a 64k memory window."
  SetSVGAMode = 0                               'Window must be > 64k for
  EXIT FUNCTION                                 'these routines.
END IF
bpp = ASC(ModeInfo.BitsPerPixel)

IF bpp = 15 THEN
  IF ASC(ModeInfo.MemoryModel) <> 6 THEN
    CLS
    PRINT "Error - screen mode not available."
    SetSVGAMode = 0
    EXIT FUNCTION
  END IF

  ppSeg = ModeInfo.WinASegment
  DEF SEG = ppSeg
ELSEIF bpp = 16 THEN
  IF ASC(ModeInfo.MemoryModel) <> 6 THEN
    CLS
    PRINT "Error - screen mode not available."
    SetSVGAMode = 0
    EXIT FUNCTION
  END IF

  ppSeg = ModeInfo.WinASegment
  DEF SEG = ppSeg
ELSEIF bpp = 24 THEN
  ppSeg = ModeInfo.WinASegment
  DEF SEG = ppSeg
ELSE
  CLS
  PRINT "Error - screen mode not available."
  PRINT ModeInfo.MemoryModel
  SetSVGAMode = 0
  EXIT FUNCTION
END IF

Regs.ax = &H4F02                                'Set the mode.
Regs.bx = mode
CALL INTERRUPTX(&H10, Regs, Regs)

IF Regs.ax <> &H4F THEN
  CLS
  PRINT "Error - cannot initialize screen mode."
  SetSVGAMode = 0
  EXIT FUNCTION
END IF

curBank = 0
yOffset = 0

SVGAinit winGran, Xres, yres, brr, ppSeg, ASC(ModeInfo.NumberOfImagePages) + 1

SetSVGAMode = (ASC(ModeInfo.NumberOfImagePages) + 1)

END FUNCTION

