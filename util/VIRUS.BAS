DEFINT A-Z                          'For faster program speed
'$INCLUDE: 'qb.bi'                  'Interrupt Definitions by QuickBasic
                   'VIRUS DETECTION FUNCTIONS
DECLARE FUNCTION DetectVirus% (InAX%, OutAX%, Meth%)
COMMON SHARED inregs AS RegType
COMMON SHARED Outregs AS RegType
COMMON SHARED inregsx AS RegTypeX
COMMON SHARED outRegsx AS RegTypeX
VirCount = 93     'How many viruses known so far

'Sample detection program:
CLS
RESTORE VirData
FOR i = 0 TO VirCount
  READ N$, A0, A1, M0
  PRINT N$;
  FOR j = 1 TO 20 - LEN(N$)
    PRINT ".";
  NEXT j
  IF DetectVirus(A0, A1, M0) THEN
      COLOR 31: PRINT "Detected!": COLOR 7
  ELSE
      PRINT "Not Detected"
  END IF
NEXT i
SYSTEM



                                        'These viruses reside in INT 21
VirData:                                'and can easily be detected.
DATA "Perfume",&H0B56,&H4952,0         
DATA "Crazy Imp",&H0D20,&H1971,0
DATA "Dutch 555",&H30FF,&H0,1
DATA "Quit 1992",&H30FF,&H0,1
DATA "Burghofer",&H330F,&HF,1
DATA "Oropax",&H33E0,&HE0,1
DATA "JD-448",&H3DFF,&H4A44,0
DATA "Shake",&H4203,&H1234,0
DATA "Invader",&H4243,&H5678,0
DATA "699",&H4B04,&H044B,0
DATA "Thirteen Minutes",&H4B04,&H044B,0
DATA "Rape-747",&H6969,&H0666,0
DATA "Klaeren",&H7600,&H48,1
DATA "Hate",&H7600,&H48,1
DATA "Growing Block",&H7700,&H0920,0
DATA "Squeaker",&H7F00,&H80,2
DATA "Vriest",&H8900,&H0123,0
DATA "Carioca",&H9000,&H1,2
DATA "Nina",&H9753,&H0,0
DATA "789",&HA1D5,&H900D,0
DATA "Filehider",&HA1D5,&H900D,0
DATA "Hey You",&HBBBB,&H6969,0
DATA "Datalock",&HBE00,&H1234,0
DATA "Plastique",&H4B40,&H5678,0
DATA "AntiCad",&H4B40,&H5678,0
DATA "Jabberwocky",&H4B4A,&H57,1
DATA "Plastique-2576",&H4B50,&H1234,0
DATA "AntiCad-2576",&H4B50,&H1234,0
DATA "Sparse",&H4B55,&H1231,0
DATA "Shirley",&H4B87,&H6663,0
DATA "Zherkov-1882",&H4B95,&H1973,0
DATA "1876",&H4BA7,&HB459,0
DATA "Dash-em",&H4BA7,&HB459,0
DATA "Magnitogorsk",&H4BAF,&HAF,1
DATA "948",&H4BAF,&HFA,1
DATA "Screenplus1",&H4BAF,&HFA,1
DATA "Lozinsky",&H4BDD,&H1234,0
DATA "Zherkov",&H4BDD,&H1234,0
DATA "Hitchcock",&H4BFE,&H1234,0
DATA "Copyright",&H4BFE,&HABCD,0
DATA "1193",&H4BFE,&HABCD,0
DATA "Troi",&HFC00,&HA5,1
DATA "Border",&HFD00,&H13,2
DATA "483",&HFE00,&H0,2
DATA "Flip",&HFE01,&H01FE,0
DATA "2468",&HFE02,&H01FD,0
DATA "Tequila",&HFE02,&H01FD,0
DATA "Black Monday",&HFEDC,&HDC,1
DATA "Tumen 0.5",&HFF00,&H00,2
DATA "Hero",&HFF00,&H00,2
DATA "Sunday",&HFF00,&H0400,0
DATA "08/15",&HFFFE,&H0815,0
DATA "Many Fingers",&HFFFE,&H0815,0
DATA "Ontario",&HFFFF,&H0000,0
DATA "Year 1992",&HFFFF,&H1992,0
DATA "B1M92",&HFFFF,&H1992,0
DATA "Diamond-A",&HD5AA,&H2A55,0
DATA "Diamond-B",&HD5AA,&H2A03,0
DATA "Milous",&HCB00,&H07,1
DATA "Witcode",&HCB02,&H02CB,0
DATA "Westwood",&HCC00,&H0700,0
DATA "Gotcha",&HDADA,&HA5,2
DATA "Plovdiv 1.3",&HDAFE,&H1234,0
DATA "Durban",&HDE00,&HDF,2
DATA "Eddie-2",&HA55A,&H5AA5,0
DATA "Blinker",&HAA00,&H00AA,0
DATA "Backtime",&HAA03,&H03AA,0
DATA "600",&HAB00,&H5555,0
DATA "Voronezh",&HAB00,&H5555,0
DATA "Zerotime",&HC000,&H0300,0
DATA "Solano",&HC000,&H1234,0
DATA "Slow",&HC000,&H0300,0
DATA "Sverdlov",&HC500,&H6731,0
DATA "Socha",&HC600,&H55,1
DATA "MH-757",&HC700,&H07,1
DATA "Brothers",&HDEDE,&H41,2
DATA "Jerusalem",&HE000,&H0300,0
DATA "Armageddon",&HE000,&HDADA,0
DATA "8-Tunes",&HE00F,&H4C31,0
DATA "Mendosa",&HE100,&H0300,0
DATA "Fu Manchu",&HE100,&H0400,0
DATA "Anarkia",&HE400,&H04,2
DATA "Spyer",&HE700,&H78,2
DATA "Kiev",&HE700,&H78,2
DATA "Jerusalem-G",&HEE00,&H0300,0
DATA "Pregnant",&HEE00,&H05,1
DATA "Frere Jacques",&HF000,&H0300,0
DATA "Tremor",&HF1E9,&HCADE,0
DATA "PcVrsDs",&HF2AA,&HAA,2
DATA "Carfield",&HF300,&H0400,0
DATA "GP1",&HF700,&H0300,0
DATA "Cinderella",&HFB00,&H00,2
DATA "dBASE",&HFB0A,&H0AFB,0
DATA "Twins",&HFF10,&H07,1

FUNCTION DetectVirus (InAX, OutAX, Meth)
inregs.AX = InAX
CALL INTERRUPT(&H21, inregs, Outregs)
SELECT CASE Meth
CASE 0
  IF Outregs.AX = OutAX THEN Det = 1 ELSE Det = 0
CASE 1
  IF (Outregs.AX AND &HFF) = OutAX THEN Det = 1 ELSE Det = 0
CASE 2
  IF (Outregs.AX \ 256) = OutAX THEN Det = 1 ELSE Det = 0
END SELECT
DetectVirus = Det
END FUNCTION

