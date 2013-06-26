'Sun Rasterfile (.RAS) viewer by Dmitry Brant
'
'me@dmitrybrant.com
'http://dmitrybrant.com
'
'Freeware. Use freely.
'No warranty of any kind.
'Read disclaimer information at the above-mentioned web site.

DECLARE FUNCTION GetDword& (filenum%)
DECLARE FUNCTION PutRAS% (file$, x0%, y0%)
DEFINT A-Z

CLS
FILES "*.RAS": PRINT
INPUT "Enter a valid .RAS image file name: ", filename$

SCREEN 13
IF PutRAS(filename$, 0, 0) THEN PRINT "Unable to display .RAS image."
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
IF LOF(f) = 0 THEN KILL file$: PutRAS = 1: EXIT FUNCTION

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

IF x0 > 319 OR y0 > 199 THEN CLOSE #f: EXIT FUNCTION
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
IF x0 + wid > 319 THEN wid = 320 - x0
start = 0
IF x0 < 0 THEN wid = wid + x0: start = -x0: x0 = 0
o& = y0 * 320 + x0: yc = y0
DO UNTIL EOF(f) OR yc > 199 OR yc > hgt + y0
    GET #f, , a$
    FOR i = 1 TO wid
        IF yc >= 0 THEN POKE o&, ASC(MID$(a$, i + start, 1))
        o& = o& + 1
    NEXT i
    o& = o& - wid + 320
    yc = yc + 1
LOOP
ELSEIF bpp = 1 THEN
    xc = 0: yc = 0
    a$ = " "
    DO UNTIL EOF(f) OR yc > 199 OR yc > hgt + y0
        GET #f, , a$
        d = ASC(a$)
        FOR i = 7 TO 0 STEP -1
            IF xc = wid - 1 AND wid MOD 2 THEN xc = 0: yc = yc + 1: EXIT FOR
            IF d AND 2 ^ i THEN PSET (xc, yc), 15 ELSE PSET (xc, yc), 0
            xc = xc + 1
            IF xc > wid THEN xc = 0: yc = yc + 1: EXIT FOR
        NEXT i
    LOOP
END IF

DEF SEG
CLOSE #f
PutRAS = 0
END FUNCTION

