TYPE CutHeader
    Horz AS INTEGER
    Vert AS INTEGER
    Dumy AS STRING * 2
END TYPE

CLS
SCREEN 13
OPEN "sample.cut" FOR BINARY AS #1

DIM TheHeader AS CutHeader
a$ = " "

GET #1, , TheHeader

'PRINT TheHeader.Horz, TheHeader.Vert
TheHeader.Vert = TheHeader.Vert - 1
TheHeader.Horz = TheHeader.Horz - 1

row = 0: col = 0: CurByte = 0: NextByte = 0: Rep = 0

DO UNTIL EOF(1) 'row > TheHeader.Vert OR EOF(1)
    GET #1, , a$
    CurByte = ASC(a$)
    IF CurByte = 0 THEN : col = 0: row = row + 1: GOTO bail
   
    IF CurByte \ 128 = 1 THEN
        Rep = CurByte AND 127
        GET #1, , a$
        NextByte = ASC(a$)
        FOR i = 1 TO Rep
            PSET (col, row), NextByte
            col = col + 1
            'IF col > TheHeader.Horz THEN col = 0: row = row + 1: GOTO bail
        NEXT i
    ELSE
        FOR i = 1 TO (CurByte AND 127)
            GET #1, , a$
            NextByte = ASC(a$)
            PSET (col, row), NextByte
            col = col + 1
            'IF col > TheHeader.Horz THEN col = 0: row = row + 1: GOTO bail
        NEXT i
    END IF
bail:
LOOP

CLOSE

