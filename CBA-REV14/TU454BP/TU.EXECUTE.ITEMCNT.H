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
*-* R83, Mdata, Ultimate
IF CAPT.FLG THEN
FOR I = 1 TO 9 UNTIL ITEMCNT > ''
WORDS = DCOUNT(CAPT<I>,SPC)
FOR W = 1 TO WORDS UNTIL ITEMCNT > ''
Z = FIELD(CAPT<I>,SPC,W)
IF NUM(Z) THEN ITEMCNT = Z
NEXT W
NEXT I
END
!@!IF ITEMCNT = '' AND ULT.ERNO<1,2> > '' THEN ITEMCNT = ULT.ERNO<1,2> ; !*ULT*!
