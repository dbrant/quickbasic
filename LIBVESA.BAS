DECLARE FUNCTION DetectVESA% ()
DECLARE FUNCTION SetSVGAMode% (mode%)
DECLARE SUB SVGAInit (winGran%, Xres%, yres%, bpp%, ppSeg%, pages%)
DEFINT A-Z
TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE
TYPE VGAInfoBlock
    VESASignature AS STRING * 4
    VESAVersion AS INTEGER
    OEMStringPtr AS LONG
    Capabilities AS STRING * 4
    VideoModePtr AS LONG
    TotalMemory AS INTEGER
    Reserved AS STRING * 236
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
    Reserved AS STRING * 216
END TYPE

COMMON SHARED regs0 AS RegTypeX

' Detect to see if a VESA compatible graphics card is present.
FUNCTION DetectVESA

DIM VGAInfo AS VGAInfoBlock

regs0.ax = &H4F00
regs0.es = VARSEG(VGAInfo)
regs0.di = VARPTR(VGAInfo)
CALL INTERRUPTX(&H10, regs0, regs0)

IF regs0.ax = &H4F THEN
  DetectVESA = 1
ELSE
  DetectVESA = 0
END IF

END FUNCTION

' Sets an SVGA mode.
FUNCTION SetSVGAMode (mode)

DIM ModeInfo AS ModeInfoBlock                   'Get info on the desired
regs0.ax = &H4F01                                'SVGA mode.
regs0.cx = mode
regs0.es = VARSEG(ModeInfo)
regs0.di = VARPTR(ModeInfo)
CALL INTERRUPTX(&H10, regs0, regs0)

IF (ModeInfo.ModeAttributes AND 1) = 0 THEN
  SetSVGAMode = 0
  EXIT FUNCTION
END IF
winGran = 64 \ ModeInfo.WinGranularity          'Window granularity adjusted.
Xres = ModeInfo.XResolution                     'Get screen resolution.
yres = ModeInfo.YResolution
IF ModeInfo.WinSize < 64 THEN
  SetSVGAMode = 0                               'Window must be > 64k for
  EXIT FUNCTION                                 'these routines.
END IF
bpp = ASC(ModeInfo.BitsPerPixel)

IF bpp = 15 THEN
  IF ASC(ModeInfo.MemoryModel) <> 6 THEN
    SetSVGAMode = 0
    EXIT FUNCTION
  END IF

  ppSeg = ModeInfo.WinASegment
ELSEIF bpp = 16 THEN
  IF ASC(ModeInfo.MemoryModel) <> 6 THEN
    SetSVGAMode = 0
    EXIT FUNCTION
  END IF

  ppSeg = ModeInfo.WinASegment
ELSEIF bpp = 24 THEN
  ppSeg = ModeInfo.WinASegment
ELSE
  SetSVGAMode = 0
  EXIT FUNCTION
END IF

regs0.ax = &H4F02                                'Set the mode.
regs0.bx = mode
CALL INTERRUPTX(&H10, regs0, regs0)

IF regs0.ax <> &H4F THEN
  SetSVGAMode = 0
  EXIT FUNCTION
END IF

SVGAInit winGran, Xres, yres, bpp, ppSeg, ASC(ModeInfo.NumberOfImagePages) + 1

SetSVGAMode = (ASC(ModeInfo.NumberOfImagePages) + 1)

END FUNCTION

