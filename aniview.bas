'ANI Viewer by Dmitry Brant
'
'based on earlier code by Yousuf Philips of YPI [http://members.xoom.com/Philipz]
'
' dmitrybrant@hotmail.com
' http://dmitrybrant.com
'
'Freeware. Use freely.
'No warranty of any kind.
'Read disclaimer information at the above-mentioned web site.

DECLARE FUNCTION ShowANI% (FileName$)
DEFINT A-Z

TYPE ANIMainHeader
   ValidID AS STRING * 4
   SizeOfFile AS LONG
   RIFFType AS STRING * 4
END TYPE

TYPE IconChunk
    ValidID AS STRING * 4
    SizeOfChunk AS LONG
    SomeJunk AS STRING * 11
END TYPE

TYPE BMPInfoHeader
   LengthOfHeader AS LONG
   ImageWidth AS LONG
   ImageHeight AS LONG
   Planes AS INTEGER
   BitsPerPixel AS INTEGER
   CompressMethod AS LONG
   ImageSizeInBytes AS LONG
   HorizontalRatio AS LONG
   VerticalRatio AS LONG
   ColorsUsed AS LONG
   ImportantColors AS LONG
END TYPE

CLS
FILES "*.ani": PRINT
PRINT "When file loads, press SPACE to flip through frames."
INPUT "Enter a valid .ANI image file name: ", FileName$

SCREEN 13
IF ShowANI(FileName$) THEN PRINT "Could not draw ANI file."

BEEP
DO: LOOP UNTIL INKEY$ <> ""
SCREEN 0: WIDTH 80, 25: SYSTEM

FUNCTION ShowANI (FileName$)
DIM ANIMainHeader AS ANIMainHeader
DIM IconChunk AS IconChunk
DIM BMPInfoHead AS BMPInfoHeader

Byte$ = " "

Start:
f = FREEFILE
OPEN FileName$ FOR BINARY AS #f
IF LOF(f) = 0 THEN CLOSE #f: KILL FileName$: ShowANI = 1: EXIT FUNCTION
 
GET #f, , ANIMainHeader
IF ANIMainHeader.RIFFType <> "ACON" THEN CLOSE #f: ShowANI = 1: EXIT FUNCTION

DWord$ = SPACE$(4)
Position& = LOC(f) + 1
DO
   Position& = Position& + 1
   GET #f, Position&, DWord$
LOOP UNTIL DWord$ = "fram"

DO
  CurrentPosition& = LOC(f) + 9
  GET #f, , IconChunk
  IF IconChunk.ValidID <> "icon" THEN
     CLOSE #f
     GOTO Start
  END IF
  CurrentPosition& = CurrentPosition& + IconChunk.SizeOfChunk
  Position& = LOC(f) + 1
  DO
     Position& = Position& + 1
     GET #f, Position&, Byte$
  LOOP UNTIL Byte$ = CHR$(40)
  SEEK #f, Position&
  GET #f, , BMPInfoHead
  IF BMPInfoHead.Planes <> 1 THEN CLOSE #f: ShowANI = 1: EXIT FUNCTION
  IF BMPInfoHead.BitsPerPixel > 8 THEN CLOSE #f: ShowANI = 1: EXIT FUNCTION
  IF BMPInfoHead.CompressMethod <> 0 THEN CLOSE #f: ShowANI = 1: EXIT FUNCTION
  Palette$ = SPACE$((2 ^ BMPInfoHead.BitsPerPixel) * 4)
  GET #f, , Palette$
  OUT &H3C8, 0
  FOR ColorNumber = 0 TO 2 ^ BMPInfoHead.BitsPerPixel - 1
     OUT &H3C9, INT(ASC(MID$(Palette$, (ColorNumber * 4) + 3, 1)) \ 4)
     OUT &H3C9, INT(ASC(MID$(Palette$, (ColorNumber * 4) + 2, 1)) \ 4)
     OUT &H3C9, INT(ASC(MID$(Palette$, (ColorNumber * 4) + 1, 1)) \ 4)
  NEXT ColorNumber
  IF BMPInfoHead.BitsPerPixel = 8 THEN
     Bytes$ = SPACE$(BMPInfoHead.ImageWidth): Extra$ = SPACE$(4 - (BMPInfoHead.ImageWidth MOD 4))
     FOR YHeight = CINT(BMPInfoHead.ImageHeight / 2) + 3 TO 0 STEP -1
        GET #f, , Bytes$
        FOR XWidth = 0 TO BMPInfoHead.ImageWidth - 1
           PSET (XWidth, YHeight), ASC(MID$(Bytes$, XWidth + 1, 1))
        NEXT XWidth
        IF LEN(Extra$) <> 4 THEN
           GET #f, , Extra$
        END IF
     NEXT YHeight
  ELSE
     Bytes$ = SPACE$(CINT(BMPInfoHead.ImageWidth / 2))
     FOR YHeight = (BMPInfoHead.ImageHeight / 2) - 1 TO 0 STEP -1
        GET #f, , Bytes$
        FOR XWidth = 0 TO BMPInfoHead.ImageWidth - 1 STEP 2
           PSET (XWidth, YHeight), (ASC(MID$(Bytes$, (XWidth / 2) + 1, 1)) AND 240) / 16
           PSET (XWidth + 1, YHeight), ASC(MID$(Bytes$, (XWidth / 2) + 1, 1)) AND 15
        NEXT XWidth
     NEXT YHeight
   SEEK #f, CurrentPosition&
  END IF
  DO
    r$ = INKEY$
    IF r$ = CHR$(27) THEN CLOSE #f: EXIT FUNCTION
    IF r$ = " " THEN EXIT DO
  LOOP
LOOP
END FUNCTION

