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
*-* R83, uniVerse, Prime, Unidata, Mdata
!@!CAPT = '' ; !*PI/O,PR1ME,*!
IF DTA.FLG THEN
!@!SAVE.VERB = VERB ; !*PR1ME*!
!@!IF (FIELD(VERB,SPC,1) = 'GET-LIST' AND VERB<2> = '') OR FIELD(DTA<1>,SPC,1) = 'SAVE-LIST' THEN ; !*PR1ME*!
!@!VERB = VERB:AM:DTA<1> ; !*PR1ME*!
!@!DTA = DELETE(DTA,1,0,0) ; !*PR1ME*!
!@!END ; !*PR1ME*!
ATTS = DCOUNT(DTA,AM); !*-PR1ME*!
FOR I = 1 TO ATTS; !*-PR1ME*!
DATA DTA<I>; !*-PR1ME*!
NEXT I; !*-PR1ME*!
DTA = ''
END
!@! INCLUDE TUBP UV.TU.EXECUTE.H ; !*UV,UP*!
 INCLUDE TUBP UDT.TU.EXECUTE.H
!@! INCLUDE TUBP MRX.TU.EXECUTE.H ; !*MRX*!
 INCLUDE TUBP TU.EXECUTE.INC2.H
