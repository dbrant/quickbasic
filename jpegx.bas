'JPEG Viewer v2 for MODE-X by Dmitry Brant
'
'http://dmitrybrant.com
'me@dmitrybrant.com
'
'IMPORTANT:
'To run this program, you must start QuickBasic with the
'following command line:
'
'QB /L MODEX.QLB
'
'This program is freeware. Use freely, but give me credit where credit
'is due. Feel free to make improvements, additions, and optimizations.
'If you do, please tell me or show me what you have done.
'This program is also provided on an "as-is" basis.
'I can't be held responsible for the mischevious deeds you may perform
'with the aid of this program.
'
'NOTE: This specific edition of the viewer is designed to work with MODE-X.
'Because MODE-X does not support packed-pixeling (HiColor), the JPG files
'can only be displayed in grayscale.
'The program will create a grayscale palette (0 to 255), and display only the
'Luminance (Y) attribute of the JPG file.

'$INCLUDE: 'modex.bi'

'This is the only SUB that has to be called to display the image.
DECLARE SUB PutJPGX (file$, x0%, y0%)
    'file$ = filename of the JPG image
    'x0, y0 = coordinates where to start drawing the image


'These other functions are called from within the PutJPG sub.
'They are useless by themselves.
DECLARE FUNCTION Decode% (inArray() AS ANY, hnum%)
DECLARE FUNCTION ReceiveBits% (cat%)
DECLARE FUNCTION NextBit% ()
DECLARE SUB ToRGB (y0%, cb0%, cr0%, r0%, g0%, b0%)
DECLARE SUB GetBlock (vector%(), HuffDC() AS ANY, HuffDCNum%, HuffAC() AS ANY, HuffACNum%, Quant%(), QuantNum%, dcCoef%)
DECLARE FUNCTION GetHuffTables% ()
DECLARE FUNCTION GetQuantTables% (inArray() AS INTEGER)
DECLARE FUNCTION GetSOS% ()
DECLARE FUNCTION GetWord& ()
DECLARE FUNCTION GetByte% ()
DECLARE FUNCTION GetSOI% ()
DECLARE FUNCTION GetImageAttr% ()
DEFINT A-Z

TYPE JpegType                   'some type definitions (for coherence)
    rows AS INTEGER             'image height
    cols AS INTEGER             'image width
    SamplesY AS INTEGER         'sampling ratios
    SamplesCbCr AS INTEGER
    QuantTableY AS INTEGER      'quantization table numbers
    QuantTableCbCr AS INTEGER
    HuffDCTableY AS INTEGER     'huffman table numbers
    HuffDCTableCbCr AS INTEGER
    HuffACTableY AS INTEGER
    HuffACTableCbCr AS INTEGER
    NumComp AS INTEGER          'number of components
END TYPE

TYPE HuffmanEntry               'a type for huffman tables
    Index AS LONG
    Code AS INTEGER
    Length AS INTEGER
END TYPE
                                'a few global variables
COMMON SHARED ScreenWidth, ScreenHeight
COMMON SHARED curByte, curBits, jfile, EOI, DCTables, ACTables, QTables
COMMON SHARED a$
DIM SHARED Image AS JpegType
DIM SHARED HuffmanDC(0 TO 1, 0 TO 255) AS HuffmanEntry
DIM SHARED HuffmanAC(0 TO 1, 0 TO 255) AS HuffmanEntry
DIM SHARED dct(0 TO 7, 0 TO 7, 0 TO 7, 0 TO 7) AS INTEGER

'--------- A demonstration ----------

CLS
FILES "*.jpg": PRINT
INPUT "Enter a valid .JPG image file name> ", f$

PutJPGX f$, 0, 0       'draw the image at offset 0,0

BEEP
SLEEP
SCREEN 0: WIDTH 80, 25
SYSTEM

'--------- End of Demo ----------


Zig2:        'Zigzag patterns for reordering quantization tables and vectors
DATA 0,0
ZigzagPositions:
DATA 0,1,1,0
DATA 2,0,1,1,0,2
DATA 0,3,1,2,2,1,3,0
DATA 4,0,3,1,2,2,1,3,0,4
DATA 0,5,1,4,2,3,3,2,4,1,5,0
DATA 6,0,5,1,4,2,3,3,2,4,1,5,0,6
DATA 0,7,1,6,2,5,3,4,4,3,5,2,6,1,7,0
DATA 7,1,6,2,5,3,4,4,3,5,2,6,1,7
DATA 2,7,3,6,4,5,5,4,6,3,7,2
DATA 7,3,6,4,5,5,4,6,3,7
DATA 4,7,5,6,6,5,7,4
DATA 7,5,6,6,5,7
DATA 6,7,7,6
DATA 7,7

FUNCTION Decode (inArray() AS HuffmanEntry, hnum)

IF GetByte = 255 THEN
    n1 = GetByte
    IF n1 >= &HD0 AND n1 <= &HD7 THEN
        n2 = 2 ^ curBits - 1
        SEEK #jfile, SEEK(jfile) - 2
        IF curByte AND n2 = n2 THEN    'if the remaining bits are 1
            EOI = 1
            Decode = 0
            EXIT FUNCTION
        END IF
    ELSE
        SEEK #jfile, SEEK(jfile) - 2
    END IF
ELSE
    SEEK #jfile, SEEK(jfile) - 1
END IF

curVal& = 0
MatchFound = -1
FOR l = 1 TO 16    'cycle through 16 possible Huffman lengths
    curVal& = curVal& * 2 + NextBit
    IF EOI THEN EXIT FUNCTION

    FOR i = 0 TO 255              'look for a match in the Huffman table
        IF inArray(hnum, i).Length > l THEN EXIT FOR
        IF inArray(hnum, i).Length = l THEN
            IF inArray(hnum, i).Index = curVal& THEN MatchFound = i: EXIT FOR
        END IF
    NEXT i
    IF MatchFound > -1 THEN EXIT FOR
NEXT l

IF MatchFound = -1 THEN BEEP: Decode = -1: EXIT FUNCTION

Decode = inArray(hnum, MatchFound).Code  'return the appropriate code
END FUNCTION

SUB GetBlock (vector(), HuffDC() AS HuffmanEntry, HuffDCNum, HuffAC() AS HuffmanEntry, HuffACNum, Quant(), QuantNum, dcCoef)
DIM array2(0 TO 7, 0 TO 7)
EOI = 0

temp0 = Decode(HuffDC(), HuffDCNum)   'Get the DC coefficient
IF EOI THEN d = 0
dcCoef = dcCoef + ReceiveBits(temp0)
vector(0, 0) = dcCoef * Quant(QuantNum, 0, 0)

Xpos = 0: Ypos = 0
RESTORE ZigzagPositions
ACcount = 1
DO
    d = Decode(HuffAC(), HuffACNum)
    IF EOI THEN d = 0

    zeros = d \ 16
    bits = d AND 15
    bitVal = ReceiveBits(bits)

    IF zeros = 0 AND bits = 0 THEN   'EOB Encountered
        FOR j = ACcount TO 63
            READ Xpos, Ypos
            vector(Xpos, Ypos) = 0
        NEXT j
        EXIT DO
    ELSEIF zeros = 15 AND bits = 0 THEN  'ZRL encountered
        FOR j = ACcount TO ACcount + 15
            READ Xpos, Ypos
            vector(Xpos, Ypos) = 0
        NEXT j
        ACcount = ACcount + 16
    ELSE
        FOR j = 1 TO zeros
            READ Xpos, Ypos
            vector(Xpos, Ypos) = 0
            ACcount = ACcount + 1
            IF ACcount >= 64 THEN EXIT DO
        NEXT j
        READ Xpos, Ypos
        vector(Xpos, Ypos) = bitVal * Quant(QuantNum, Xpos, Ypos)
        ACcount = ACcount + 1
    END IF
    IF ACcount >= 64 THEN EXIT DO
LOOP

FOR x = 0 TO 7            'the IDCT routine (this SOB flies!)
    FOR y = 0 TO 7
        sum = 0
        FOR v = 0 TO 7
            FOR u = 0 TO 7
                temp& = vector(u, v)
                IF temp& THEN temp& = temp& * dct(x, y, u, v) / 512
                sum = sum + temp&
            NEXT u
        NEXT v
        array2(x, y) = sum
    NEXT y
NEXT x

FOR u = 0 TO 7
    FOR v = 0 TO 7
        vector(u, v) = array2(u, v) + 128
    NEXT v
NEXT u

END SUB

FUNCTION GetByte
GET #jfile, , a$
GetByte = ASC(a$)
END FUNCTION

FUNCTION GetHuffTables
DIM HuffAmount(1 TO 16)
l0 = GetWord
c0 = 2
DO
temp0 = GetByte: c0 = c0 + 1
t0 = (temp0 AND 16) \ 16
temp0 = temp0 AND 15
SELECT CASE t0
    CASE 0        'DC Table
        total = 0
        FOR i = 1 TO 16
            temp1 = GetByte: c0 = c0 + 1
            total = total + temp1
            HuffAmount(i) = temp1
        NEXT i
        FOR i = 0 TO total - 1
            HuffmanDC(temp0, i).Code = GetByte: c0 = c0 + 1
        NEXT i
        curNum& = 0
        curIndex = -1
        FOR i = 1 TO 16
            FOR j = 1 TO HuffAmount(i)
                curIndex = curIndex + 1
                HuffmanDC(temp0, curIndex).Index = curNum&
                HuffmanDC(temp0, curIndex).Length = i
                curNum& = curNum& + 1
            NEXT j
            curNum& = curNum& * 2
        NEXT i
        DCTables = DCTables + 1
    CASE 1
        total = 0
        FOR i = 1 TO 16
            temp1 = GetByte: c0 = c0 + 1
            total = total + temp1
            HuffAmount(i) = temp1
        NEXT i
        FOR i = 0 TO total - 1
            HuffmanAC(temp0, i).Code = GetByte: c0 = c0 + 1
        NEXT i
        curNum& = 0
        curIndex = -1
        FOR i = 1 TO 16
            FOR j = 1 TO HuffAmount(i)
                curIndex = curIndex + 1
                HuffmanAC(temp0, curIndex).Index = curNum&
                HuffmanAC(temp0, curIndex).Length = i
                curNum& = curNum& + 1
            NEXT j
            curNum& = curNum& * 2
        NEXT i
        ACTables = ACTables + 1
END SELECT
LOOP UNTIL c0 >= l0
GetHuffTables = 1
END FUNCTION

FUNCTION GetImageAttr
temp4& = GetWord            'Length of segment
temp0 = GetByte             'Data precision
IF temp0 <> 8 THEN GetImageAttr = 0: EXIT FUNCTION   'we do not support 12 or 16-bit samples
Image.rows = GetWord        'Image Height
Image.cols = GetWord        'Image Width
temp0 = GetByte             'Number of components
FOR i = 1 TO temp0
    id = GetByte
    SELECT CASE id
        CASE 1
            temp1 = GetByte
            Image.SamplesY = (temp1 AND 15) * (temp1 \ 16)
            Image.QuantTableY = GetByte
        CASE 2, 3
            temp1 = GetByte
            Image.SamplesCbCr = (temp1 AND 15) * (temp1 \ 16)
            Image.QuantTableCbCr = GetByte
    END SELECT
NEXT i
GetImageAttr = 1
END FUNCTION

FUNCTION GetQuantTables (inArray() AS INTEGER)
l0 = GetWord
c0 = 2
DO
temp0 = GetByte: c0 = c0 + 1
IF temp0 AND &HF0 THEN
    GetQuantTables = 0: EXIT FUNCTION  'we don't support 16-bit tables
END IF
temp0 = temp0 AND 15
RESTORE Zig2
xp = 0: yp = 0
FOR i = 0 TO 63
    READ xp, yp
    inArray(temp0, xp, yp) = GetByte: c0 = c0 + 1
NEXT i
QTables = QTables + 1
LOOP UNTIL c0 >= l0
GetQuantTables = 1: EXIT FUNCTION
END FUNCTION

FUNCTION GetSOI
a$ = "  "
SEEK #jfile, 1
GET #jfile, , a$
IF a$ = CHR$(255) + CHR$(&HD8) THEN d = 1 ELSE d = 0
a$ = " "
GetSOI = d
END FUNCTION

FUNCTION GetSOS
temp4& = GetWord
temp0 = GetByte
IF temp0 <> 1 AND temp0 <> 3 THEN GetSOS = 0: EXIT FUNCTION
Image.NumComp = temp0
FOR i = 1 TO temp0
    temp1 = GetByte
    SELECT CASE temp1
        CASE 1
            temp2 = GetByte
            Image.HuffACTableY = temp2 AND 15
            Image.HuffDCTableY = temp2 \ 16
        CASE 2
            temp2 = GetByte
            Image.HuffACTableCbCr = temp2 AND 15
            Image.HuffDCTableCbCr = temp2 \ 16
        CASE 3
            temp2 = GetByte
            Image.HuffACTableCbCr = temp2 AND 15
            Image.HuffDCTableCbCr = temp2 \ 16
        CASE ELSE
            GetSOS = 0: EXIT FUNCTION
    END SELECT
NEXT i
a$ = "   "      '3 reserved bytes (?)
GET #1, , a$
a$ = " "
GetSOS = 1
END FUNCTION

FUNCTION GetWord&
GET #jfile, , a$
l0& = CLNG(ASC(a$)) * 256
GET #jfile, , a$
l0& = l0& + ASC(a$)
GetWord& = l0&
END FUNCTION

FUNCTION NextBit

t0 = 2 ^ curBits
v0 = -((curByte AND t0) <> 0)

curBits = curBits - 1
IF curBits < 0 THEN
    curBits = 7: curByte = GetByte
    IF curByte = 255 THEN
        IF GetByte = &HD9 THEN EOI = 1: EXIT FUNCTION
    END IF
END IF

NextBit = v0
END FUNCTION

SUB PutJPGX (file$, x0, y0)
DIM YVector1(0 TO 7, 0 TO 7)              '4 vectors for Y attribute
DIM YVector2(0 TO 7, 0 TO 7)
DIM YVector3(0 TO 7, 0 TO 7)
DIM YVector4(0 TO 7, 0 TO 7)
DIM CbVector(0 TO 7, 0 TO 7)              '1 vector for Cb attribute
DIM CrVector(0 TO 7, 0 TO 7)              '1 vector for Cr attribute
DIM QuantTable(0 TO 1, 0 TO 7, 0 TO 7)    '2 quantization tables (Y, CbCr)

FOR x = 0 TO 7           'Initialize our cosine table
 FOR y = 0 TO 7
    FOR u = 0 TO 7
     FOR v = 0 TO 7
        t! = COS((2 * x + 1) * u * .1963495) * COS((2 * y + 1) * v * .1963495)
        IF u = 0 THEN t! = t! * .707107
        IF v = 0 THEN t! = t! * .707107
        dct(x, y, u, v) = t! * 128
     NEXT v
    NEXT u
 NEXT y
NEXT x

jfile = FREEFILE
a$ = " "   'The ideal byte
OPEN file$ FOR BINARY AS #jfile
IF LOF(jfile) = 0 THEN CLOSE #jfile: KILL file$: PRINT "File does not exist.": EXIT SUB
IF GetSOI = 0 THEN PRINT "Not a valid JPEG/JFIF file.": CLOSE #jfile: EXIT SUB


QTables = 0                   'Initialize some checkpoint variables
ACTables = 0
DCTables = 0
Restart& = 0

DO                            'Primary control loop for markers
    IF GetByte = 255 THEN     'Marker Found
        d = GetByte
        SELECT CASE d         'which one is it?
            CASE &HC0         'SOF0
                IF GetImageAttr = 0 THEN PRINT "Error getting Start Of Frame 0 Marker.": CLOSE #jfile: EXIT SUB
            CASE &HC1         'SOF1
                IF GetImageAttr = 0 THEN PRINT "Error getting Start Of Frame 1 Marker.": CLOSE #jfile: EXIT SUB
            CASE &HC9         'SOF9
                PRINT "Arithmetic Coding Not Supported.": CLOSE #jfile: EXIT SUB
            CASE &HC4         'DHT
                IF ACTables < 2 OR DCTables < 2 THEN
                    IF GetHuffTables = 0 THEN PRINT "Error getting Huffman tables.": CLOSE #jfile: EXIT SUB
                END IF
            CASE &HCC         'DAC
                PRINT "Arithmetic Coding Not Supported.": CLOSE #jfile: EXIT SUB
            CASE &HDA         'SOS
                IF GetSOS = 0 THEN PRINT "Error getting SOS marker.": CLOSE #jfile: EXIT SUB
                IF (DCTables = 2 AND ACTables = 2 AND QTables = 2) OR Image.NumComp = 1 THEN
                    EOI = 0
                    EXIT DO                 'Go on to secondary control loop
                ELSE
                    PRINT "Unexpected file format.": CLOSE #jfile: EXIT SUB
                END IF
            CASE &HDB         'DQT
                IF QTables < 2 THEN
                    IF GetQuantTables(QuantTable()) = 0 THEN PRINT "Error getting quantization tables.": CLOSE #jfile: EXIT SUB
                END IF
            CASE &HDD         'DRI
                Restart& = GetWord
            CASE &HE0         'APP0
                temp1& = GetWord   'Length of segment
                a$ = "     "
                GET #1, , a$       'JFIF header
                a$ = " "
                temp0 = GetByte    'Major revision
                temp0 = GetByte    'Minor revision
                temp0 = GetByte    'Density definition
                temp0 = GetByte    'X-Density
                temp0 = GetByte    'Y-Density
                temp0 = GetByte    'Thumbnail Width
                temp1 = GetByte    'Thumbnail Height
            CASE &HFE         'COM
                a$ = SPACE$(GetWord - 2)
                GET #1, , a$       'Retrieve comment
                a$ = " "
        END SELECT
    END IF
    r$ = INKEY$
    IF r$ = CHR$(27) THEN CLOSE #jfile: EXIT SUB
LOOP UNTIL EOF(1)

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
OUT &H3C8, 0            'create the grayscale palette
FOR i = 0 TO 255: OUT &H3C9, i \ 4: OUT &H3C9, i \ 4: OUT &H3C9, i \ 4: NEXT i

Xpos = 0: Ypos = 0
dcY = 0: dcCb = 0: dcCr = 0
xindex = 0: yindex = 0: mcu = 0
r = 0: g = 0: b = 0

curBits = 7: curByte = GetByte

DEF SEG = &HA000

SELECT CASE Image.NumComp
CASE 3
    SELECT CASE Image.SamplesY
    CASE 4
        DO
            GetBlock YVector1(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock YVector2(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock YVector3(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock YVector4(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock CbVector(), HuffmanDC(), Image.HuffDCTableCbCr, HuffmanAC(), Image.HuffACTableCbCr, QuantTable(), Image.QuantTableCbCr, dcCb
            GetBlock CrVector(), HuffmanDC(), Image.HuffDCTableCbCr, HuffmanAC(), Image.HuffACTableCbCr, QuantTable(), Image.QuantTableCbCr, dcCr
            FOR i = 0 TO 7
                FOR j = 0 TO 7
                    y = YVector1(i, j)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            FOR i = 0 TO 7
                FOR j = 8 TO 15
                    y = YVector2(i, j - 8)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            FOR i = 8 TO 15
                FOR j = 0 TO 7
                    y = YVector3(i - 8, j)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            FOR i = 8 TO 15
                FOR j = 8 TO 15
                    y = YVector4(i - 8, j - 8)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            xindex = xindex + 16
            IF xindex >= Image.cols THEN xindex = 0: yindex = yindex + 16: mcu = 1
            IF mcu = 1 AND Restart& <> 0 THEN 'execute the restart interval
                curByte = GetByte: curByte = GetByte: curByte = GetByte
                curBits = 7
                dcY = 0: dcCb = 0: dcCr = 0: mcu = 0
            END IF
            r$ = INKEY$
            IF r$ = CHR$(27) THEN EXIT DO
        LOOP UNTIL EOF(jfile) OR yindex >= Image.rows OR yindex >= ScreenHeight
    CASE 2
        DO
            GetBlock YVector1(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock YVector2(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock CbVector(), HuffmanDC(), Image.HuffDCTableCbCr, HuffmanAC(), Image.HuffACTableCbCr, QuantTable(), Image.QuantTableCbCr, dcCb
            GetBlock CrVector(), HuffmanDC(), Image.HuffDCTableCbCr, HuffmanAC(), Image.HuffACTableCbCr, QuantTable(), Image.QuantTableCbCr, dcCr
            FOR i = 0 TO 7
                FOR j = 0 TO 7
                    y = YVector1(i, j)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            FOR i = 0 TO 7
                FOR j = 8 TO 15
                    y = YVector2(i, j - 8)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            xindex = xindex + 16
            IF xindex >= Image.cols THEN xindex = 0: yindex = yindex + 8: mcu = 1
            IF mcu = 1 AND Restart& <> 0 THEN 'execute the restart interval
                curByte = GetByte: curByte = GetByte: curByte = GetByte
                curBits = 7
                dcY = 0: dcCb = 0: dcCr = 0: mcu = 0
            END IF
            r$ = INKEY$
            IF r$ = CHR$(27) THEN EXIT DO
        LOOP UNTIL EOF(jfile) OR yindex >= Image.rows OR yindex >= ScreenHeight
    CASE 1
        DO
            GetBlock YVector1(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
            GetBlock CbVector(), HuffmanDC(), Image.HuffDCTableCbCr, HuffmanAC(), Image.HuffACTableCbCr, QuantTable(), Image.QuantTableCbCr, dcCb
            GetBlock CrVector(), HuffmanDC(), Image.HuffDCTableCbCr, HuffmanAC(), Image.HuffACTableCbCr, QuantTable(), Image.QuantTableCbCr, dcCr
            FOR i = 0 TO 7
                FOR j = 0 TO 7
                    y = YVector1(i, j)
                    IF y < 0 THEN y = 0
                    IF y > 255 THEN y = 255
                    xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
                NEXT j
            NEXT i
            xindex = xindex + 8
            IF xindex >= Image.cols THEN xindex = 0: yindex = yindex + 8: mcu = 1
            IF mcu = 1 AND Restart& <> 0 THEN 'execute the restart interval
                curByte = GetByte: curByte = GetByte: curByte = GetByte
                curBits = 7
                dcY = 0: dcCb = 0: dcCr = 0: mcu = 0
            END IF
            r$ = INKEY$
            IF r$ = CHR$(27) THEN EXIT DO
        LOOP UNTIL EOF(jfile) OR yindex >= Image.rows OR yindex >= ScreenHeight
    END SELECT
CASE 1
    DO
        GetBlock YVector1(), HuffmanDC(), Image.HuffDCTableY, HuffmanAC(), Image.HuffACTableY, QuantTable(), Image.QuantTableY, dcY
        FOR i = 0 TO 7
            FOR j = 0 TO 7
                y = YVector1(i, j)
                IF y < 0 THEN y = 0
                IF y > 255 THEN y = 255
                xj = xindex + j: yi = yindex + i
                    IF xj < Image.cols AND yi < Image.rows AND yi + y0 < ScreenHeight AND xj + x0 < ScreenWidth THEN SetPoint xj, yi, y
            NEXT j
        NEXT i
        xindex = xindex + 8
        IF xindex >= Image.cols THEN xindex = 0: yindex = yindex + 8: mcu = 1
        IF mcu = 1 AND Restart& <> 0 THEN 'execute the restart interval
            curByte = GetByte: curByte = GetByte: curByte = GetByte
            curBits = 7
            dcY = 0: mcu = 0
        END IF
        r$ = INKEY$
        IF r$ = CHR$(27) THEN EXIT DO
    LOOP UNTIL EOF(jfile) OR yindex >= Image.rows OR yindex >= ScreenHeight
END SELECT

DEF SEG
CLOSE #jfile
END SUB

FUNCTION ReceiveBits (cat)
temp0& = 0
FOR i = 1 TO cat
    temp0& = temp0& * 2 + NextBit
NEXT i

IF temp0& >= 2 ^ (cat - 1) THEN
    ReceiveBits = temp0&
ELSE
    ReceiveBits = -(2 ^ cat - 1) + temp0&
END IF
END FUNCTION

SUB ToRGB (y0, cb0, cr0, r0, g0, b0)
r0 = y0 + 1.402 * (cr0 - 128)
g0 = y0 - .34414 * (cb0 - 128) - .71414 * (cr0 - 128)
b0 = y0 + 1.772 * (cb0 - 128)

IF r0 > 255 THEN r0 = 255
IF r0 < 0 THEN r0 = 0
IF g0 > 255 THEN g0 = 255
IF g0 < 0 THEN g0 = 0
IF b0 > 255 THEN b0 = 255
IF b0 < 0 THEN b0 = 0
END SUB

