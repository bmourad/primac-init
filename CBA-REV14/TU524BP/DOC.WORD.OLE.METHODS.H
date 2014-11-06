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
* Document control OLE Methods for Word
*
*
*-* Get handle to documents collection, and load document
*
ATTR = "Documents"; HDOCUMENT = 0
CALL ROC.GET(HWA, ATTR, HDOCUMENT, STATUS)
IF STATUS AND NOT(HDOCUMENT) THEN RETURN
*
STRING = 'Open,':PCFILENAME:',,,,,,,,,'; VALS = ""
OLD = ","; NEW = VM
 INCLUDE TUBP SWAP.STRINGS.H
ATTR = STRING; HDOC = 0
IF DEBUGFLAG THEN CRT "Opening file ":PCFILENAME
CALL ROC.GET(HDOCUMENT, ATTR, HDOC, STATUS)
IF STATUS THEN RETURN
*
*-* Set Word document visible and minimized
*
ATTR = "Visible":ROC.US:"WindowState"; VALS = TRUE:ROC.US:2
IF DEBUGFLAG THEN CRT "Setting Visible, and WindowState"
CALL ROC.SET(HWA, ATTR, VALS, STATUS)
IF STATUS THEN RETURN
*
*-* get handle to active document, and calculate range and
*-* format into table
*
ATTR = "ActiveDocument"; HACTIVEDOC = 0
IF DEBUGFLAG THEN CRT "Getting ActiveDocument Handle"
CALL ROC.GET(HWA, ATTR, HACTIVEDOC, STATUS)
IF STATUS AND NOT(HACTIVEDOC) THEN RETURN
