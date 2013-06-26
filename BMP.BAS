'By Dmitry Brant
'Use freely
DECLARE SUB BMPLoad (FileName$)
DEFINT A-Z

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


SCREEN 13

BMPLoad "confiden.bmp"

END

DEFLNG A-Z
SUB BMPLoad (FileName$)
DIM Pic1 AS Header, PalEntry AS PalEntry, Byte AS STRING * 1
DIM RLE1 AS STRING * 1, RLE2 AS STRING * 1, RLE3 AS STRING * 1

f = FREEFILE
OPEN FileName$ FOR BINARY AS #f
  IF LOF(1) = 0 THEN CLOSE #f: KILL FileName$: EXIT SUB
  GET #f, , Pic1
  IF Pic1.BMPIdentifier <> "BM" THEN CLOSE #f: EXIT SUB
  IF Pic1.BMPFileSize <> LOF(1) THEN CLOSE #f: EXIT SUB

  Y = Pic1.BMPHeight - 1
  FOR indexnumber = 0 TO (2 ^ Pic1.BMPBitsPerPixel) - 1
    GET #f, , PalEntry
    OUT &H3C8, indexnumber
    OUT &H3C9, INT(ASC(PalEntry.Red) / 4)
    OUT &H3C9, INT(ASC(PalEntry.Green) / 4)
    OUT &H3C9, INT(ASC(PalEntry.Blue) / 4)
  NEXT

  SELECT CASE Pic1.BMPCompression
  CASE 0
    SELECT CASE Pic1.BMPBitsPerPixel
    CASE 8
      FOR Y = Pic1.BMPHeight - 1 TO 0 STEP -1
        FOR X = 0 TO Pic1.BMPWidth - 1
          GET #f, , Byte: PSET (X, Y), ASC(Byte)
        NEXT
        'GET #f, , a$
      NEXT
    CASE 4
      a$ = " "
      FOR Y = Pic1.BMPHeight - 1 TO 0 STEP -1
        FOR X = 0 TO Pic1.BMPWidth - 1 STEP 2
          GET #f, , Byte
          PSET (X, Y), ASC(Byte) \ 16
          PSET (X + 1, Y), ASC(Byte) AND 15
        NEXT
        GET #f, , a$
        IF ASC(a$) <> 0 THEN SEEK #f, SEEK(f) - 1
      NEXT
    CASE 1
      a$ = " "
      FOR Y = Pic1.BMPHeight - 1 TO 0 STEP -1: FOR X = 0 TO Pic1.BMPWidth - 1 STEP 8
        GET #f, , Byte
        FOR Mono = 0 TO 7
          IF ASC(Byte) AND 2 ^ Mono THEN
            IF X AND 1 THEN
              PSET (X + Mono, Y), 1
            ELSE
              PSET (X + (7 - Mono), Y), 1
            END IF
          ELSE
            IF X AND 1 THEN
              PSET (X + Mono, Y), 0
            ELSE
              PSET (X + (7 - Mono), Y), 0
            END IF
          END IF
        NEXT
      NEXT
      GET #f, , a$
    NEXT
    END SELECT
  CASE 1
    DO
      GET #f, , RLE1
      SELECT CASE ASC(RLE1)
      CASE 0
        GET #f, , RLE2
        SELECT CASE ASC(RLE2)
        CASE 0: Y = Y - 1: X = 0
        CASE 1: GOTO EndOfBitmap
        CASE 2: GET #f, , RLE3: X = X + ASC(RLE3): GET #f, , RLE3: Y = Y - ASC(RLE3)
        CASE ELSE
          FOR RLE = 1 TO ASC(RLE2): GET #f, , RLE3: PSET (X, Y), ASC(RLE3): X = X + 1: NEXT
          IF ASC(RLE2) AND 1 THEN GET #f, , RLE3
        END SELECT
      CASE ELSE
        GET #f, , RLE2
        FOR RLE = 1 TO ASC(RLE1): PSET (X, Y), ASC(RLE2): X = X + 1: NEXT
      END SELECT
    LOOP
  CASE 2
    DO
      GET #f, , RLE1
      SELECT CASE ASC(RLE1)
      CASE 0
        GET #f, , RLE2
        SELECT CASE ASC(RLE2)
        CASE 0: Y = Y - 1: X = 0
        CASE 1: GOTO EndOfBitmap
        CASE 2: GET #f, , RLE3: X = X + ASC(RLE3): GET #f, , RLE3: Y = Y - ASC(RLE3)
        CASE ELSE
          FOR RLE = 1 TO ASC(RLE2) STEP 2
            GET #f, , RLE3: PSET (X, Y), INT(ASC(RLE3) / 16): PSET (X + 1, Y), ASC(RLE3) MOD 16: X = X + 2
          NEXT
          IF INT(ASC(RLE2) / 2) AND 1 THEN GET #f, , RLE3
        END SELECT
      CASE ELSE
        GET #f, , RLE2
        FOR RLE = 1 TO ASC(RLE1)
          IF RLE AND 1 THEN
            PSET (X, Y), INT(ASC(RLE2) / 16)
          ELSE
            PSET (X, Y), ASC(RLE2) MOD 16
          END IF
          X = X + 1
        NEXT
      END SELECT
    LOOP
  CASE 3
    PRINT "Bit Fields Not Supported!": GOTO EndOfBitmap
  END SELECT
EndOfBitmap:
CLOSE #f

DO UNTIL INKEY$ <> "": LOOP

END SUB

