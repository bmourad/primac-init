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
*** Unidata ***
ITEMCNT = @SYSTEM.RETURN.CODE
IF ITEMCNT[1,1] = '[' THEN ITEMCNT = ITEMCNT[2,LEN(ITEMCNT)-2]
IF ITEMCNT < 0 THEN ITEMCNT = ''
