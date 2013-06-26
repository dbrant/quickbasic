'ANI Viewer for MODE-X by Dmitry Brant
'
'based on earlier code by Yousuf Philips of YPI [http://members.xoom.com/Philipz]
'
' dmitrybrant@hotmail.com
' http://dmitrybrant.com
'
'IMPORTANT: To run this program, you must start QuickBasic with the
'following command line:
'
'QB /L MODEX.QLB
'

'Freeware. Use freely.
'No warranty of any kind.
'Read disclaimer information at the above-mentioned web site.

'$INCLUDE: 'modex.bi'      'Must be in the same directory
DECLARE FUNCTION ShowANI% (filename$)
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

COMMON SHARED ScreenWidth, ScreenHeight


CLS
FILES "*.ani": PRINT
PRINT "When image loads, press SPACE to flip through frames."
INPUT "Enter a valid .ANI image file name: ", filename$


PRINT
PRINT "Select Mode-X Resolution:"
PRINT "  0:  320 x 200"
PRINT "  1:  320 x 400"
PRINT "  2:  360 x 200"
PRINT "  3:  360 x 400"
PRINT "  4:  320 x 240"
PRINT "  5:  320 x 480"
PRINT "  6:  360 x 240"
PRINT "  7:  360 x 480"

DO
    r$ = INKEY$
    SELECT CASE r$
        CASE CHR$(27): SYSTEM
        CASE "0": ScreenWidth = 320: ScreenHeight = 200: EXIT DO
        CASE "1": ScreenWidth = 320: ScreenHeight = 400: EXIT DO
        CASE "2": ScreenWidth = 360: ScreenHeight = 200: EXIT DO
        CASE "3": ScreenWidth = 360: ScreenHeight = 400: EXIT DO
        CASE "4": ScreenWidth = 320: ScreenHeight = 240: EXIT DO
        CASE "5": ScreenWidth = 320: ScreenHeight = 480: EXIT DO
        CASE "6": ScreenWidth = 360: ScreenHeight = 240: EXIT DO
        CASE "7": ScreenWidth = 360: ScreenHeight = 480: EXIT DO
    END SELECT
LOOP


SCREEN 13
IF SetModeX(VAL(r$)) = 0 THEN PRINT "Unable to set Mode-X": SYSTEM
IF ShowANI(filename$) THEN SCREEN 0: WIDTH 80, 25: PRINT "Unable to display image.": SYSTEM

BEEP
SLEEP

SCREEN 0: WIDTH 80, 25
SYSTEM

FUNCTION ShowANI (filename$)
DIM ANIMainHeader AS ANIMainHeader
DIM IconChunk AS IconChunk
DIM BMPInfoHead AS BMPInfoHeader

Byte$ = " "

Start:
f = FREEFILE
OPEN filename$ FOR BINARY AS #f
IF LOF(f) = 0 THEN CLOSE #f: KILL filename$: ShowANI = 1: EXIT FUNCTION
 
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
        FOR xWidth = 0 TO BMPInfoHead.ImageWidth - 1
           SetPoint xWidth, YHeight, ASC(MID$(Bytes$, xWidth + 1, 1))
        NEXT xWidth
        IF LEN(Extra$) <> 4 THEN
           GET #f, , Extra$
        END IF
     NEXT YHeight
  ELSE
     Bytes$ = SPACE$(CINT(BMPInfoHead.ImageWidth / 2))
     FOR YHeight = (BMPInfoHead.ImageHeight / 2) - 1 TO 0 STEP -1
        GET #f, , Bytes$
        FOR xWidth = 0 TO BMPInfoHead.ImageWidth - 1 STEP 2
           SetPoint xWidth, YHeight, (ASC(MID$(Bytes$, (xWidth / 2) + 1, 1)) AND 240) / 16
           SetPoint xWidth + 1, YHeight, ASC(MID$(Bytes$, (xWidth / 2) + 1, 1)) AND 15
        NEXT xWidth
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

