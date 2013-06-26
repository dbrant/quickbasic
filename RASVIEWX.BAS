'Sun Rasterfile (.RAS) viewer for MODE-X by Dmitry Brant
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

DECLARE FUNCTION GetDword& (filenum%)
DECLARE FUNCTION PutRAS% (file$, x0%, y0%)
DEFINT A-Z
COMMON SHARED ScreenWidth, ScreenHeight


CLS
FILES "*.RAS": PRINT
INPUT "Enter a valid .RAS image file name: ", filename$


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
IF PutRAS(filename$, 0, 0) THEN SCREEN 0: WIDTH 80, 25: PRINT "Unable to display .RAS image.": SYSTEM
SLEEP

SCREEN 0: WIDTH 80, 25
SYSTEM

FUNCTION GetDword& (filenum)
a$ = " "
GET #filenum, , a$: d& = ASC(a$) * 16777216
GET #filenum, , a$: d& = d& + ASC(a$) * 65536
GET #filenum, , a$: d& = d& + ASC(a$) * 256
GET #filenum, , a$: d& = d& + ASC(a$)
GetDword& = d&
END FUNCTION

FUNCTION PutRAS (file$, x0, y0)
f = FREEFILE
OPEN file$ FOR BINARY AS #f
IF LOF(f) = 0 THEN CLOSE #f: KILL file$: PutRAS = 1: EXIT FUNCTION

a$ = "    "
GET #f, , a$

IF a$ <> "Y¦j•" THEN CLOSE : PutRAS = 1: EXIT FUNCTION

wid = GetDword(f)
hgt = GetDword(f)
bpp = GetDword(f)
length& = GetDword(f)
typ& = GetDword(f)
maptype = GetDword(f)
maplen& = GetDword(f)

IF x0 >= ScreenWidth OR y0 >= ScreenHeight THEN CLOSE #f: EXIT FUNCTION
IF x0 + wid < 0 OR y0 + hgt < 0 THEN CLOSE #f: EXIT FUNCTION

a$ = " "
IF maptype > 0 THEN
    OUT &H3C8, 0
    FOR i = 0 TO 255
        GET #f, , a$
        SEEK #f, SEEK(f) + 255
        OUT &H3C9, ASC(a$) \ 4
        GET #f, , a$
        SEEK #f, SEEK(f) + 255
        OUT &H3C9, ASC(a$) \ 4
        GET #f, , a$
        SEEK #f, SEEK(f) - 512
        OUT &H3C9, ASC(a$) \ 4
    NEXT i
    SEEK #f, SEEK(f) + 512
END IF

DEF SEG = &HA000
IF bpp = 8 THEN

IF wid MOD 2 THEN a$ = SPACE$(wid + 1) ELSE a$ = SPACE$(wid)
IF x0 + wid >= ScreenWidth THEN wid = ScreenWidth - x0
start = 0
IF x0 < 0 THEN wid = wid + x0: start = -x0: x0 = 0
xc = x0: yc = y0
DO UNTIL EOF(f) OR yc >= ScreenHeight OR yc > hgt + y0
    GET #f, , a$
    FOR i = 1 TO wid
        IF yc >= 0 THEN SetPoint xc, yc, ASC(MID$(a$, i + start, 1))
        xc = xc + 1
    NEXT i
    xc = x0
    yc = yc + 1
LOOP
ELSEIF bpp = 1 THEN
    xc = x0: yc = y0
    a$ = " "
    DO UNTIL EOF(f) OR yc >= ScreenHeight OR yc > hgt + y0
        GET #f, , a$
        d = ASC(a$)
        FOR i = 7 TO 0 STEP -1
            IF xc - x0 = wid - 1 AND wid MOD 2 THEN xc = x0: yc = yc + 1: EXIT FOR
            IF xc > 0 AND yc > 0 AND xc < ScreenWidth THEN
                IF d AND 2 ^ i THEN SetPoint xc, yc, 15 ELSE SetPoint xc, yc, 0
            END IF
            xc = xc + 1
            IF xc - x0 > wid THEN xc = x0: yc = yc + 1: EXIT FOR
        NEXT i
    LOOP
END IF

DEF SEG
CLOSE #f
PutRAS = 0
END FUNCTION

