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
* Common Methods used for Word OLE Automation
*
*
*-* Restore Word's window state
*
IF INDEX(OPTIONS, "R", 1) THEN
ATTR = "WindowState"; VALS = FALSE
IF DEBUGFLAG THEN CRT "Setting WindowState"
CALL ROC.SET(HWA, ATTR, VALS, STATUS)
IF STATUS THEN RETURN
END
*
*-* Print Word Document
*
IF INDEX(OPTIONS, "P", 1) THEN
STRING = 'PrintOut,1,,,,,,,,,,,,,'; OLD = ','; NEW = VM; VALS = ''
 INCLUDE TUBP SWAP.STRINGS.H
ATTR = STRING
IF DEBUGFLAG THEN CRT "Printing Doc"
CALL ROC.GET(HACTIVEDOC, ATTR, VALS, STATUS)
END
*
*-* Save Document
*
IF INDEX(OPTIONS, "S", 1) THEN
IF DEBUGFLAG THEN CRT "Saving Doc"
STRING = "Save,1,0"; OLD = ','; NEW = VM; VALS = ""
 INCLUDE TUBP SWAP.STRINGS.H
ATTR = STRING
CALL ROC.GET(HDOCUMENT, ATTR, VALS, STATUS)
END
*
*-* Exit Word
*
IF INDEX(OPTIONS, "X", 1) THEN
IF DEBUGFLAG THEN CRT "Closing Word"
STRING = "Quit,,,"; OLD = ','; NEW = VM; VALS = ""
 INCLUDE TUBP SWAP.STRINGS.H
ATTR = STRING
CALL ROC.GET(HWA, ATTR, VALS, STATUS)
END
