*
** SBClient Host 3GL API
** Copyright (C) Ardent Software Inc. 1998
** Copyright (C) UniData, Inc. 1996, 1997
** Copyright (C) System Builder Corporation. 1995
**
**      This software is the proprietary property and contains
**      trade secrets of Ardent Software, Inc. Any unauthorized use,
**      disclosure or duplication is strictly prohibited.
**      All rights reserved.
*
CALL TU.CONVERT(OUTPUT, AM, "")
!@!CALL TU.CHANGE('One','1',OUTPUT)  ;!*PR1ME*!
*
SELECTCOUNT = 0
OS.CODE = 0
OS.CODE = 1 ;!*UDT,UDTVMS,PI/O,UV,UP*!
*
IF OS.CODE THEN
SELECTCOUNT = @SYSTEM.RETURN.CODE ;!*UDT,UDTVMS,PI/O*!
!@!SELECTCOUNT = @SELECTED           ;!*UV,UP*!
IF SELECTCOUNT[1,1] = '[' THEN SELECTCOUNT = SELECTCOUNT[2, LEN(SELECTCOUNT) - 2]   ;!*UDT,UDTVMS*!
END ELSE
AMCOUNT = DCOUNT(OUTPUT, AM); !*-ULT*!
FOR A = 1 TO AMCOUNT; !*-ULT*!
SPACE.COUNT = DCOUNT(OUTPUT<A>, " "); !*-ULT*!
FOR I = 1 TO SPACE.COUNT UNTIL SELECTCOUNT; !*-ULT*!
FLD = FIELD(OUTPUT<A>, ' ', I); !*-ULT*!
IF FLD NE "" AND NUM(FLD) THEN SELECTCOUNT = FLD; A = AMCOUNT; !*-ULT*!
NEXT I; !*-ULT*!
NEXT A; !*-ULT*!
!@!GET(MSG.,1) ULT.ERNO ELSE ULT.ERNO = 0; !*ULT*!
!@!SELECTCOUNT = ULT.ERNO<1,2>; !*ULT*!
END
IF NOT(NUM(SELECTCOUNT)) THEN SELECTCOUNT = 0
IF SELECTCOUNT < 0 THEN SELECTCOUNT = 0
