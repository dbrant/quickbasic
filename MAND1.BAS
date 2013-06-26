DEFINT A-Z

DIM q(0 TO 350) AS DOUBLE
MaxIterations = 64
MaxSize = 4
   
SCREEN 13
FOR i = 0 TO 63
OUT &H3C8, i
OUT &H3C9, 0
OUT &H3C9, 0
OUT &H3C9, i
OUT &H3C8, i + 64
OUT &H3C9, i
OUT &H3C9, 0
OUT &H3C9, 63 - i
OUT &H3C8, i + 128
OUT &H3C9, 63 - i
OUT &H3C9, i
OUT &H3C9, 0
OUT &H3C8, i + 192
OUT &H3C9, 0
OUT &H3C9, 63 - i
OUT &H3C9, 0
NEXT i

    wid = 320
    hei = 200
    
    xmin# = -2
    xmax# = 1.2
    ymin# = -1.2
    ymax# = 1.2
    
    deltap# = (xmax# - xmin#) / wid
    deltaq# = (ymax# - ymin#) / hei
   
    px# = 0
    dx# = .001


    q(0) = ymax#
    FOR row = 1 TO wid
        q(row) = q(row - 1) - deltaq#
    NEXT row
    p# = xmin#
    FOR col = 0 TO wid
        FOR row = 0 TO hei
            r$ = INKEY$
            IF r$ = CHR$(27) THEN SYSTEM
            x# = px#
            px# = px# + dx#
            y# = 0: xsquare# = 0: ysquare# = 0
            colour = 1
            WHILE ((colour < MaxIterations) AND ((xsquare# + ysquare#) < MaxSize))
                xsquare# = x# * x#
                ysquare# = y# * y#
                y# = y# * x#
                y# = y# + (y# + q(row))
                x# = xsquare# - ysquare# + p#
                colour = colour + 1
            WEND
            PSET (col, row), colour * 4
        NEXT row
        p# = p# + deltap#
    NEXT col
SLEEP

