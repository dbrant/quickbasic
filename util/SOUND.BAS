DEFINT A-Z                               'For faster program speed
                   'SOUND FUNCTIONS
DECLARE SUB SetAlgorithm (Flag1%, channel%)
DECLARE SUB SetWaveform (Carr%, Modl%, channel%)
DECLARE SUB SetAMDepth (Flag1%)
DECLARE SUB SetFeedback (Number%, channel%)
DECLARE SUB SetTom (Flag1%)
DECLARE SUB SetCymbal (Flag1%)
DECLARE SUB SetHiHat (Flag1%)
DECLARE SUB SetBassDrum (Flag1%)
DECLARE SUB SetSnareDrum (Flag1%)
DECLARE SUB SetVibDepth (Flag1%)
DECLARE SUB SetEGType (Flag1%, Flag2%, channel%)
DECLARE SUB SetAttackRate (Carr%, Modl%, channel%)
DECLARE SUB SetDecayRate (Carr%, Modl%, channel%)
DECLARE SUB SetOctave (Number%, channel%)
DECLARE SUB SetReleaseRate (Carr%, Modl%, channel%)
DECLARE SUB SetSustainLevel (Carr%, Modl%, channel%)
DECLARE SUB SetFrequency (Number%, channel%)
DECLARE SUB SetKey (Flag1%, channel%)
DECLARE SUB SetAmplitudeMod (Flag1%, Flag2%, channel%)
DECLARE SUB SetSustainMaintain (Flag1%, Flag2%, channel%)
DECLARE SUB SetVibrato (Flag1%, Flag2%, channel%)
DECLARE SUB SetModFreq (Carr%, Modl%, channel%)
DECLARE SUB SetTotalLevel (Carr%, Modl%, channel%)
DECLARE SUB DirtyReset ()
DECLARE SUB SetScalingLevel (Carr%, Modl%, channel%)
DECLARE FUNCTION CardPresent% ()
DECLARE SUB sbout (reg%, x%)
DECLARE FUNCTION sbinp% (reg%)
DIM SHARED caropr(0 TO 8), modopr(0 TO 8)
DIM SHARED sb(1 TO 245) AS INTEGER
RESTORE OperData
FOR i = 0 TO 8
READ caropr(i)
READ modopr(i)
NEXT i

DirtyReset
SetAmplitudeMod 0, 0, 0
SetVibrato 0, 1, 0
SetWaveform 0, 0, 0
SetFeedback 7, 0
SetModFreq 1, 1, 0
SetAttackRate 0, 0, 0
SetDecayRate 0, 0, 0
SetTotalLevel 31, 6, 0
SetFrequency &H299, 0
SetOctave 3, 0
SetKey 1, 0
SLEEP
SetFrequency 0, 0

END

OperData:
DATA 0,3,1,4,2,5,8,&HB,9,&HC,&HA,&HD,&H10,&H13,&H11,&H14,&H12,&H15

FUNCTION CardPresent
sbout 4, &H60
sbout 4, &H80
r = INP(&H388)
sbout 2, &HFF
sbout 4, &H21
FOR i = 1 TO 80
S = INP(&H388)
NEXT i
sbout 4, &H60
sbout 4, &H80
IF (r AND &HE0) = 0 AND (S AND &HE0) = &HC0 THEN
CardPresent = 1
ELSE
CardPresent = 0
END IF
END FUNCTION

SUB DirtyReset
FOR i = 1 TO &HF5
sbout i, 0
NEXT i
END SUB

SUB PCSpeaker (Freq, Duration!)
Old = INP(&H61)     ' 8255 PPI chip. Save the original.
OUT &H43, 182       ' 8253 Timer chip. 10110110b Channel 2, mode 3
Port = INP(&H61)    ' get the 8255 port contents.
OUT &H61, Port OR 3 ' enable the speaker and use channel 2.
Divisor = 1193180 / Freq
LSB = Divisor MOD 256
MSB = Divisor \ 256
OUT &H42, LSB
OUT &H42, MSB
TimeUp! = TIMER + Duration!
WHILE TimeUp! > TIMER: WEND
OUT &H61, Old  ' turn it off.
END SUB

FUNCTION sbinp (reg)
sbinp = sb(reg)
END FUNCTION

SUB sbout (reg, x)
sb(reg) = x
OUT &H388, reg              'Outputs the register to be written to
FOR i = 0 TO 5
rg = INP(&H388)
NEXT i
OUT &H389, x
FOR i = 0 TO 34
rg = INP(&H388)
NEXT i
END SUB

SUB SetAlgorithm (Flag1, channel)
  number1 = sbinp(&HC0 + channel)
IF Flag1 = 1 THEN
  sbout (&HC0 + channel), number1 OR 1
ELSE
  sbout (&HC0 + channel), number1 AND &HFE
END IF
END SUB

SUB SetAMDepth (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR &H80
ELSE
  sbout &HBD, number1 AND &H7F
END IF
END SUB

SUB SetAmplitudeMod (Flag1, Flag2, channel)
  number1 = sbinp(&H20 + caropr(channel))
  number2 = sbinp(&H20 + modopr(channel))
IF Flag1 = 1 THEN
        sbout (&H20 + caropr(channel)), number1 OR &H80
ELSE
        sbout (&H20 + caropr(channel)), number1 AND &H7F
END IF
IF Flag2 = 1 THEN
        sbout (&H20 + modopr(channel)), number2 OR &H80
ELSE
        sbout (&H20 + modopr(channel)), number2 AND &H7F
END IF
END SUB

SUB SetAttackRate (Carr, Modl, channel)
  number1 = sbinp(&H60 + caropr(channel))
  number2 = sbinp(&H60 + modopr(channel))
  number3 = number1 AND &HF
  number4 = number2 AND &HF
  sbout (&H60 + caropr(channel)), number3 OR (Carr * 16)
  sbout (&H60 + modopr(channel)), number4 OR (Modl * 16)
END SUB

SUB SetBassDrum (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR &H10
ELSE
  sbout &HBD, number1 AND &HEF
END IF
END SUB

SUB SetCymbal (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR 2
ELSE
  sbout &HBD, number1 AND &HFD
END IF
END SUB

SUB SetDecayRate (Carr, Modl, channel)
  number1 = sbinp(&H60 + caropr(channel))
  number2 = sbinp(&H60 + modopr(channel))
  number3 = number1 AND &HF0
  number4 = number2 AND &HF0
  sbout (&H60 + caropr(channel)), number3 OR Carr
  sbout (&H60 + modopr(channel)), number4 OR Modl
END SUB

SUB SetEGType (Flag1, Flag2, channel)
  number1 = sbinp(&H20 + caropr(channel))
  number2 = sbinp(&H20 + modopr(channel))
IF Flag1 = 1 THEN
        sbout (&H20 + caropr(channel)), number1 OR &H20
ELSE
        sbout (&H20 + caropr(channel)), number1 AND &HDF
END IF
IF Flag2 = 1 THEN
        sbout (&H20 + modopr(channel)), number2 OR &H20
ELSE
        sbout (&H20 + modopr(channel)), number2 AND &HDF
END IF
END SUB

SUB SetFeedback (Number, channel)
IF Number >= 0 AND Number < 8 THEN
  number1 = sbinp(&HC0 + channel)
  number2 = number1 AND &HF1
  sbout (&HC0 + channel), number2 OR (Number * 2)
END IF
END SUB

SUB SetFrequency (Number, channel)
IF Number < 1024 AND Number >= 0 THEN
  number1 = sbinp(&HB0 + channel)
  number2 = number1 AND &HFC
  sbout (&HA0 + channel), Number AND &HFF
  sbout (&HB0 + channel), number2 OR (Number \ 256)
END IF
END SUB

SUB SetHiHat (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR 1
ELSE
  sbout &HBD, number1 AND &HFE
END IF
END SUB

SUB SetKey (Flag1, channel)
  number1 = sbinp(&HB0 + channel)
IF Flag1 = 1 THEN
  sbout (&HB0 + channel), number1 OR &H20
ELSE
  sbout (&HB0 + channel), number1 AND &H1F
END IF
END SUB

SUB SetKSR (Flag1, Flag2, channel)
  number1 = sbinp(&H20 + caropr(channel))
  number2 = sbinp(&H20 + modopr(channel))
IF Flag1 = 1 THEN
        sbout (&H20 + caropr(channel)), number1 OR &H10
ELSE
        sbout (&H20 + caropr(channel)), number1 AND &HEF
END IF
IF Flag2 = 1 THEN
        sbout (&H20 + modopr(channel)), number2 OR &H10
ELSE
        sbout (&H20 + modopr(channel)), number2 AND &HEF
END IF
END SUB

SUB SetModFreq (Carr, Modl, channel)
  number1 = sbinp(&H20 + caropr(channel))
  number2 = sbinp(&H20 + modopr(channel))
  number3 = number1 AND &HF0
  number4 = number2 AND &HF0
  sbout (&H20 + caropr(channel)), number3 OR Carr
  sbout (&H20 + modopr(channel)), number4 OR Modl
END SUB

SUB SetOctave (Number, channel)
IF Number >= 0 AND Number < 8 THEN
  number1 = sbinp(&HB0 + channel)
  number2 = number1 AND &HE3
  sbout (&HB0 + channel), number2 OR (Number * 4)
END IF
END SUB

SUB SetReleaseRate (Carr, Modl, channel)
  number1 = sbinp(&H80 + caropr(channel))
  number2 = sbinp(&H80 + modopr(channel))
  number3 = number1 AND &HF0
  number4 = number2 AND &HF0
  sbout (&H80 + caropr(channel)), number3 OR Carr * 16
  sbout (&H80 + modopr(channel)), number4 OR Modl * 16
END SUB

SUB SetRhythm (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR &H20
ELSE
  sbout &HBD, number1 AND &HDF
END IF
END SUB

SUB SetScalingLevel (Carr, Modl, channel)
  number1 = sbinp(&H40 + caropr(channel))
  number2 = sbinp(&H40 + modopr(channel))
  number3 = number1 AND &H3F
  number4 = number2 AND &H3F
  sbout (&H40 + caropr(channel)), number3 OR (Carr * 64)
  sbout (&H40 + modopr(channel)), number4 OR (Modl * 64)
END SUB

SUB SetSnareDrum (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR &H8
ELSE
  sbout &HBD, number1 AND &HF7
END IF
END SUB

SUB SetSustainLevel (Carr, Modl, channel)
  number1 = sbinp(&H80 + caropr(channel))
  number2 = sbinp(&H80 + modopr(channel))
  number3 = number1 AND &HF
  number4 = number2 AND &HF
  sbout (&H80 + caropr(channel)), number3 OR (Carr * 16)
  sbout (&H80 + modopr(channel)), number4 OR (Modl * 16)
END SUB

SUB SetSustainMaintain (Flag1, Flag2, channel)
  number1 = sbinp(&H20 + caropr(channel))
  number2 = sbinp(&H20 + modopr(channel))
IF Flag1 = 1 THEN
        sbout (&H20 + caropr(channel)), number1 OR &H20
ELSE
        sbout (&H20 + caropr(channel)), number1 AND &HDF
END IF
IF Flag2 = 1 THEN
        sbout (&H20 + modopr(channel)), number2 OR &H20
ELSE
        sbout (&H20 + modopr(channel)), number2 AND &HDF
END IF
END SUB

SUB SetTom (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR 4
ELSE
  sbout &HBD, number1 AND &HFB
END IF
END SUB

SUB SetTotalLevel (Carr, Modl, channel)
  number1 = sbinp(&H40 + caropr(channel))
  number2 = sbinp(&H40 + modopr(channel))
  number3 = number1 AND &HC0
  number4 = number2 AND &HC0
  sbout (&H40 + caropr(channel)), number3 OR Carr
  sbout (&H40 + modopr(channel)), number4 OR Modl
END SUB

SUB SetVibDepth (Flag1)
number1 = sbinp(&HBD)
IF Flag1 = 1 THEN
  sbout &HBD, number1 OR &H40
ELSE
  sbout &HBD, number1 AND &HBF
END IF
END SUB

SUB SetVibrato (Flag1, Flag2, channel)
  number1 = sbinp(&H20 + caropr(channel))
  number2 = sbinp(&H20 + modopr(channel))
IF Flag1 = 1 THEN
        sbout (&H20 + caropr(channel)), number1 OR &H40
ELSE
        sbout (&H20 + caropr(channel)), number1 AND &HBF
END IF
IF Flag2 = 1 THEN
        sbout (&H20 + modopr(channel)), number2 OR &H40
ELSE
        sbout (&H20 + modopr(channel)), number2 AND &HBF
END IF
END SUB

SUB SetWaveform (Carr, Modl, channel)
  number1 = sbinp(&HE0 + caropr(channel))
  number2 = sbinp(&HE0 + modopr(channel))
  number3 = number1 AND &HFC
  number4 = number2 AND &HFC
  sbout (&HE0 + caropr(channel)), number3 OR Carr
  sbout (&HE0 + modopr(channel)), number4 OR Modl
END SUB

