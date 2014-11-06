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
*** Prime ***
IF CAPT = '' THEN
ITEMCNT = @SYSTEM.RETURN.CODE
END ELSE
CONVERT AM TO '' IN CAPT
SPACE.COUNT = DCOUNT(CAPT, SPC)
FOR I = 1 TO SPACE.COUNT UNTIL ITEMCNT
Z = FIELD(CAPT, SPC, I)
BEGIN CASE
CASE Z = 'One'; ITEMCNT = 1
CASE NUM(Z); ITEMCNT = Z
END CASE
NEXT I
END
IF ITEMCNT < 0 THEN ITEMCNT = ''
