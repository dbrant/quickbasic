'JPEG File Data Dumper
'
'by Dmitry Brant
'
'http://dmitrybrant.com
'me@dmitrybrant.com
'
'This program extracts basic information from JPG files, like
'image width, height, and lots more. Perfect for debugging a
'JPG viewer.
'
'This program is freeware. Use freely. This program is also provided "as-is".
'I can't be held responsible for the mischevious deeds you may perform
'with the aid of this program.

DECLARE FUNCTION GetAPP0% ()
DECLARE FUNCTION GetSOS% ()
DECLARE FUNCTION GetWord& ()
DECLARE FUNCTION GetByte% ()
DECLARE FUNCTION GetSOF% ()
DECLARE FUNCTION GetImageAttr% ()
DECLARE FUNCTION GetHuffTables% ()
DECLARE FUNCTION GetQuantTables% ()
DEFINT A-Z

TYPE JpegType
    Rows AS INTEGER
    Cols AS INTEGER
    SamplesY AS INTEGER
    SamplesCbCr AS INTEGER
    QuantTableY AS INTEGER
    QuantTableCbCr AS INTEGER
    HuffDCTableY AS INTEGER
    HuffDCTableCbCr AS INTEGER
    HuffACTableY AS INTEGER
    HuffACTableCbCr AS INTEGER
END TYPE

COMMON SHARED jfile, a$
DIM SHARED image AS JpegType              'Global image attributes

CLS
INPUT "Enter a valid .JPG image file name> ", file$

CLS
jfile = FREEFILE: a$ = " "   'The ideal byte
OPEN file$ FOR BINARY AS #jfile
IF LOF(jfile) = 0 THEN CLOSE #jfile: KILL file$: PRINT "File does not exist.": SYSTEM
IF GetSOF = 0 THEN PRINT "Not a valid JPEG/JFIF file.": CLOSE #jfile: SYSTEM

DO                            'Primary control loop

    IF GetByte = 255 THEN     'Marker Found
        d = GetByte
        SELECT CASE d         'which one is it?
            CASE &HC0         'SOF0
                IF GetImageAttr = 0 THEN PRINT "Error getting Start Of Frame 0 Marker."
            CASE &HC1         'SOF0
                IF GetImageAttr = 0 THEN PRINT "Error getting Start Of Frame 0 Marker."
            CASE &HC9         'SOF9
                PRINT "Arithmetic Tables Present."
            CASE &HC4         'DHT
                IF GetHuffTables = 0 THEN PRINT "Error getting Huffman tables."
            CASE &HCC         'DAC
                PRINT "Arithmetic Tables Present."
            CASE &HD8         'SOI
                PRINT "Start Of Ima ge"
            CASE &HD9         'EOI
                PRINT "End Of Image"
            CASE &HDA         'SOS
                IF GetSOS = 0 THEN PRINT "Error getting SOS marker."
                SLEEP
            CASE &HDB         'DQT
                IF GetQuantTables = 0 THEN PRINT "Error getting quantization tables."
            CASE &HDD         'DRI
                PRINT "Restart Interval (in MCU blocks):"; GetWord
            CASE &HE0         'APP0
                IF GetAPP0 = 0 THEN PRINT "Error getting APP0 marker."
            CASE &HFE         'COM
                PRINT "Comment: ";
                a$ = SPACE$(GetWord - 2)
                GET #1, , a$
                PRINT a$
                a$ = " "
        END SELECT
  
    END IF
    r$ = INKEY$
    IF r$ = CHR$(27) THEN EXIT DO

LOOP UNTIL EOF(1)

CLOSE #jfile
SYSTEM

FUNCTION GetAPP0
l& = GetWord
PRINT "JFIF Header: ";
a$ = "     "
GET #1, , a$
IF a$ = "JFIF" + CHR$(0) THEN PRINT "Present." ELSE PRINT "Failed."
a$ = " "
PRINT "JFIF Major Revision #"; GetByte
PRINT "JFIF Minor Revision #"; GetByte
temp0 = GetByte
temp0 = GetByte
temp0 = GetByte
temp0 = GetByte
temp1 = GetByte
GetAPP0 = 1
END FUNCTION

FUNCTION GetByte
GET #jfile, , a$
GetByte = ASC(a$)
END FUNCTION

FUNCTION GetHuffTables
l0& = GetWord

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
        NEXT i
        FOR i = 0 TO total - 1
            temp1 = GetByte: c0 = c0 + 1
        NEXT i
        PRINT "Huffman DC Table"; temp0; "Present."
    CASE 1
        total = 0
        FOR i = 1 TO 16
            temp1 = GetByte: c0 = c0 + 1
            total = total + temp1
        NEXT i
        FOR i = 0 TO total - 1
            temp1 = GetByte: c0 = c0 + 1
        NEXT i
        PRINT "Huffman AC Table"; temp0; "Present."
END SELECT

LOOP UNTIL c0 >= l0&
GetHuffTables = 1
END FUNCTION

FUNCTION GetImageAttr
l& = GetWord
temp0 = GetByte
PRINT "Data Precision in bits/sample:"; temp0
image.Rows = GetWord
PRINT "Image Height:"; image.Rows
image.Cols = GetWord
PRINT "Image Width:"; image.Cols
temp0 = GetByte
PRINT "Image Type: ";
SELECT CASE temp0
    CASE 1
        PRINT "Grayscale image."
    CASE 3
        PRINT "Color image (YCbCr)"
END SELECT
FOR i = 1 TO temp0
    id = GetByte
    SELECT CASE id
        CASE 1
            temp1 = GetByte
            image.SamplesY = (temp1 AND 15) * ((temp1 AND &HF0) \ 16)
            PRINT "Number of samples for Y:"; image.SamplesY
            image.QuantTableY = GetByte
            PRINT "Quantization table for Y:"; image.QuantTableY
        CASE 2
            temp1 = GetByte
            image.SamplesCbCr = (temp1 AND 15) * ((temp1 AND &HF0) \ 16)
            PRINT "Number of samples for CbCr:"; image.SamplesCbCr
            image.QuantTableCbCr = GetByte
            PRINT "Quantization table for CbCr:"; image.QuantTableCbCr
    END SELECT
NEXT i
GetImageAttr = 1
END FUNCTION

FUNCTION GetQuantTables
l0& = GetWord
c0 = 2

DO
temp0 = GetByte: c0 = c0 + 1
temp0 = temp0 AND 15
PRINT "Quantization Table"; temp0; "Present."
FOR i = 0 TO 7
    FOR k = 0 TO 7
        temp1 = GetByte: c0 = c0 + 1
    NEXT k
NEXT i

LOOP UNTIL c0 >= l0&
GetQuantTables = 1: EXIT FUNCTION
END FUNCTION

FUNCTION GetSOF
a$ = "  "
SEEK #jfile, 1
GET #jfile, , a$
IF a$ = CHR$(255) + CHR$(&HD8) THEN d = 1 ELSE d = 0
a$ = " "
GetSOF = d
END FUNCTION

FUNCTION GetSOS
l& = GetWord
temp0 = GetByte
PRINT "*SOS*"
PRINT "Number of Components in Image:"; temp0
FOR i = 1 TO temp0
    temp1 = GetByte
    SELECT CASE temp1
        CASE 1
            temp2 = GetByte
            PRINT "Huffman Table for Y (AC/DC):";
            image.HuffACTableY = temp2 AND &HF
            PRINT image.HuffACTableY; "/";
            image.HuffDCTableY = (temp2 AND &HF0) \ 16
            PRINT image.HuffDCTableY
        CASE 2
            temp2 = GetByte
            PRINT "Huffman Table for CbCr (AC/DC):";
            image.HuffACTableCbCr = temp2 AND &HF
            PRINT image.HuffACTableCbCr; "/";
            image.HuffDCTableCbCr = (temp2 AND &HF0) \ 16
            PRINT image.HuffDCTableCbCr
    END SELECT
NEXT i
a$ = "   "
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

