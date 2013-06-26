DECLARE FUNCTION GetByte% ()
'ZSoft Paintbrush (.PCX) viewer by Dmitry Brant
'
'me@dmitrybrant.com
'http://dmitrybrant.com
'
'Freeware. Use freely.
'No warranty of any kind.
'Read disclaimer information at the above-mentioned web site.

DECLARE FUNCTION DrawPCX% (FileName$, x0%, y0%)
DEFINT A-Z

CLS
FILES "*.pcx": PRINT
INPUT "Enter a valid .PCX image file name: ", FileName$

SCREEN 13
a = DrawPCX(FileName$, 0, 0)

BEEP
DO: LOOP UNTIL INKEY$ <> ""
SCREEN 0: WIDTH 80, 25: SYSTEM

FUNCTION DrawPCX (FileName$, x0, y0)
OPEN FileName$ FOR BINARY AS #1
IF LOF(1) = 0 THEN CLOSE #1: KILL FileName$: DrawPCX = 1: EXIT FUNCTION
 
    DIM PcxVersion AS INTEGER, FWidth AS INTEGER, FHeight AS INTEGER
    DIM ScreenSize AS STRING * 8, BytesPerLine AS INTEGER
    DIM i AS STRING * 1, j AS STRING * 1, Bpp AS INTEGER, theHdc AS LONG
    DIM n AS INTEGER, m AS INTEGER
    DIM x AS INTEGER, y AS INTEGER, r AS INTEGER, g AS INTEGER, b AS INTEGER
    DIM CPlanes AS INTEGER
    DIM scan1 AS STRING, scan2 AS STRING, scan3 AS STRING
    DIM ThePalette(255) AS LONG
    
    SCREEN 1
 
    IF GetByte <> 10 THEN       'Get Marker
       CLOSE #f
       EXIT FUNCTION
    END IF
    
    IF GetByte <> 5 THEN        'Get Version
       CLOSE #f
       EXIT FUNCTION
    END IF
    
    IF GetByte <> 1 THEN        '1 = Run Length Compression
       CLOSE #f
       EXIT FUNCTION
    END IF
    
    Bpp = GetByte
    IF Bpp <> 8 THEN      'Get Bpp
       CLOSE #f
       EXIT FUNCTION
    END IF
    
    GET #1, , n
    GET #1, , m
    GET #1, , x
    GET #1, , y
    
    GET #1, , FWidth
    GET #1, , FHeight
    
    SEEK #1, 66
    CPlanes = GetByte
    GET #1, , BytesPerLine
    
    IF BytesPerLine = 0 THEN
       BytesPerLine = x - n + 1
    END IF
    FHeight = y - m + 1
    FWidth = x - n + 1
    
    IF CPlanes = 1 THEN
        SEEK #1, LOF(1) - 767
        FOR n = 0 TO 255
            r = GetByte
            g = GetByte
            b = GetByte
            theColor = r + g + b
            IF theColor > 600 THEN theColor = 3
            IF theColor > 400 THEN theColor = 2
            IF theColor > 200 THEN theColor = 1
            IF theColor > 3 THEN theColor = 0
            ThePalette(n) = theColor
        NEXT n
    END IF
    
    SEEK #1, 129
    x = 0: y = 0
    
    IF CPlanes = 1 THEN
        DO
            GET #1, , i
            IF ASC(i) > 191 THEN
                GET #1, , j
                LINE (x, y)-STEP(ASC(i) - 192, 0), ThePalette(ASC(j))
                x = x + ASC(i) - 192
            ELSE
                j = i
                PSET (x, y), ThePalette(ASC(i))
                x = x + 1
            END IF
            IF x >= BytesPerLine THEN
                x = 0: y = y + 1
            END IF
        LOOP UNTIL y >= FHeight

    ELSEIF CPlanes = 3 THEN
        REDIM ScanlineR(0 TO BytesPerLine - 1) AS INTEGER
        REDIM ScanlineG(0 TO BytesPerLine - 1) AS INTEGER
        REDIM ScanlineB(0 TO BytesPerLine - 1) AS INTEGER
        FOR y = 0 TO FHeight - 1
            n = 0
            DO
                'get RGB scanline
                GET #1, , i
                
                IF ASC(i) > 192 THEN
                    GET #1, , j
                    FOR m = 1 TO ASC(i) - 192
                        IF n >= BytesPerLine * 2 THEN
                            ScanlineB(n - BytesPerLine * 2) = ASC(j)
                        ELSEIF n >= BytesPerLine THEN
                            ScanlineG(n - BytesPerLine) = ASC(j)
                        ELSE
                            ScanlineR(n) = ASC(j)
                        END IF
                        n = n + 1
                    NEXT m
                ELSEIF ASC(i) < 192 THEN
                    IF n >= BytesPerLine * 2 THEN
                        ScanlineB(n - BytesPerLine * 2) = ASC(i)
                    ELSEIF n >= BytesPerLine THEN
                        ScanlineG(n - BytesPerLine) = ASC(i)
                    ELSE
                        ScanlineR(n) = ASC(i)
                    END IF
                    n = n + 1
                END IF
                IF n >= BytesPerLine * 3 THEN n = 0: EXIT DO
            LOOP
    
            FOR n = 0 TO FWidth - 1
                r = ScanlineR(n)
                g = ScanlineG(n)
                b = ScanlineB(n)
                theColor = r + g + b
                theColor = r + g + b
                IF theColor > 600 THEN theColor = 3
                IF theColor > 400 THEN theColor = 2
                IF theColor > 200 THEN theColor = 1
                IF theColor > 3 THEN theColor = 0
                PSET (n, y), theColor
            NEXT n
        NEXT y
        ERASE ScanlineR
        ERASE ScanlineG
        ERASE ScanlineB
    END IF

CLOSE #f
DrawPCX = 0
END FUNCTION

FUNCTION GetByte
a$ = " "
GET #1, , a$
GetByte = ASC(a$)
END FUNCTION

