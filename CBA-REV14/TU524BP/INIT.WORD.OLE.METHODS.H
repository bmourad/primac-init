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
***************************************************************************
* Initialise Word.Application object
*
*
*-* create Word.Application object
*
CALL ROC.GETHANDLE("this", HROC, STATUS)
IF STATUS AND NOT(HROC) THEN RETURN
OBJNAME = "Word.Application"; ATTR = "parent"; VALS = HROC
IF DEBUGFLAG THEN CRT "Creating Word.Application object"
CALL ROC.CREATE(OBJNAME, ATTR, VALS, HWA, STATUS)
IF STATUS AND NOT(HWA) THEN
OFFICE95 = TRUE; STATUS = FALSE
RETURN
END
*
*-* query the "Version" attribute to get the Word release
*
ATTR = "Version"; VALS = ""
IF DEBUGFLAG THEN CRT "Getting Winword version"
CALL ROC.GET(HWA, ATTR, VALS, STATUS)
IF STATUS THEN RETURN
IF VALS[1,1] = '7' THEN OFFICE95 = TRUE
