SUBROUTINE TU.CHECK.APP(APPLICATION, STATUS)
 INCLUDE TUBP USER.INCLUDE.H
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
*-* TU.CHECK.APP(Applicationname, Status)
*-*
*-*  This function will check a windows application to see if it is running
*-*
*-* [P] Applicationname	The name of the windows application to be checked.
*-*
*-*
*-* [R] Status	This is an indication of the success or failure of the
*-* subroutine. A successful call will return with zero
*-* Status and failure will return with a non-zero value.
***************************************************************************
*
 INCLUDE TUBP SPECIAL.H
 INCLUDE TUBP TU.API.H
 INCLUDE TUBP TU.ERRORCODES.H
*
IF TU.TRACE THEN
OPEN '','TUSTATE' TO TUSTATE THEN
WRITEV ('CHECK.APP':VM:APPLICATION) ON TUSTATE,('TRACE.':PORTNO),-1
END
END
*
PROMPT ''
ECHO OFF
CRT APP.CHECK:APPLICATION:TU.TAIL:
INPUT STATUS:
ECHO ON
IF STATUS THEN STATUS = FTE.UNABLE.TO.LAUNCH.APP
RETURN
END
