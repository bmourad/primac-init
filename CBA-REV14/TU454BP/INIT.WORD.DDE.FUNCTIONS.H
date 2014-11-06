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
* Common Winword init code
*
*
*-* first establish a link with the SYSTEM topic
*
IF DEBUGFLAG THEN CRT "DDE Connect to Winword"
CALL TU.DDE.CONNECT('WINWORD', 'System', SYSTEMHANDLE, STATUS)
IF STATUS THEN
WINWORD = 'WINWORD.EXE'; STATUS = 0
IF DEBUGFLAG THEN CRT "Launching Winword"
CALL TU.LAUNCH.APP(WINWORD, APP.SHOWMINIMIZED, STATUS)
IF STATUS THEN RETURN
CALL TU.CHECK.APP(WINWORD, STATUS)
IF STATUS THEN RETURN
IF DEBUGFLAG THEN CRT "DDE Connect to Winword"
CALL TU.DDE.CONNECT('WINWORD', 'System', SYSTEMHANDLE, STATUS)
IF STATUS THEN RETURN
END
*
*-* open document
*
IF DEBUGFLAG THEN CRT "Opening document"
CALL TU.DDE.EXEC.MACRO(SYSTEMHANDLE, TIMEOUT, '[FileOpen .Name="':PCFILENAME:'", .ConfirmConversions=0]', STATUS)
IF STATUS THEN RETURN
IF DEBUGFLAG THEN CRT "DDE Connect to Document"
CALL TU.DDE.CONNECT('WINWORD', PCFILENAME, HANDLE, STATUS)
IF STATUS THEN RETURN
