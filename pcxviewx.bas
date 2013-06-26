'ZSoft Paintbrush (.PCX) viewer for MODE-X by Dmitry Brant
'
'me@dmitrybrant.com
'http://dmitrybrant.com
'
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

DECLARE FUNCTION DrawPCX% (filename$, x0%, y0%)
DEFINT A-Z
COMMON SHARED ScreenWidth, ScreenHeight

CLS
FILES "*.pcx": PRINT
INPUT "Enter a valid .PCX image file name: ", filename$


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
        CASE CHR$(27)
            SYSTEM
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
IF DrawPCX(filename$, 0, 0) THEN SCREEN 0: WIDTH 80, 25: PRINT "Unable to display image.": SYSTEM
SLEEP

SCREEN 0: WIDTH 80, 25
SYSTEM

FUNCTION DrawPCX (filename$, x0, y0)
f = FREEFILE
OPEN filename$ FOR BINARY AS #f
IF LOF(f) = 0 THEN CLOSE #f: KILL filename$: DrawPCX = 1: EXIT FUNCTION
  
a$ = " "
GET #f, , a$
IF a$ <> CHR$(10) THEN CLOSE #f: DrawPCX = 1: EXIT FUNCTION
GET #f, , a$
IF a$ <> CHR$(5) THEN CLOSE #f: DrawPCX = 1: EXIT FUNCTION
GET #f, , a$
IF a$ <> CHR$(1) THEN CLOSE #f: DrawPCX = 1: EXIT FUNCTION

Size$ = "        "

GET #f, , a$
IF ASC(a$) <> 8 THEN CLOSE #f: DrawPCX = 1: EXIT FUNCTION
GET #f, , a1
GET #f, , a2
GET #f, , a3
GET #f, , a4
wid = a3 - a1 + 1
hgt = a4 - a2 + 1

SEEK #f, LOF(f) - 767
OUT &H3C8, 0
FOR i = 0 TO 255
    GET #f, , a$
    OUT &H3C9, ASC(a$) \ 4
    GET #f, , a$
    OUT &H3C9, ASC(a$) \ 4
    GET #f, , a$
    OUT &H3C9, ASC(a$) \ 4
NEXT i

SEEK #f, 129
x = x0: y = y0
   DO
      r$ = INKEY$
      IF r$ = CHR$(27) THEN EXIT DO
      GET #f, , a$
      j = ASC(a$)
      IF j > 191 THEN
         GET #f, , a$
         d = j - 192
         j = ASC(a$)
         FOR i = 1 TO d
            IF y >= 0 AND y < ScreenHeight AND x >= 0 AND x < ScreenWidth THEN SetPoint x, y, j
            x = x + 1
         NEXT i
      ELSE
         IF y >= 0 AND y < ScreenHeight AND x >= 0 AND x < ScreenWidth THEN SetPoint x, y, j
         x = x + 1
      END IF
      IF x - x0 > wid THEN x = x0: y = y + 1
   LOOP UNTIL y - y0 >= hgt - 1 OR y >= ScreenWidth
CLOSE #f
DrawPCX = 0
END FUNCTION

