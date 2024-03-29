SUBROUTINE TU.CLOSE.APP(APPLICATION, STATUS)
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
*-* TU.CLOSE.APP(Applicationname, Status)
*-*
*-*  This function will close a windows application.
*-*
*-* [P] Applicationname	The name of the windows application to be closed.
*-*
*-* [R] Status	This is an indication of the success or failure of the
*-* subroutine. A successful call will return with zero
*-* Status and failure will return with a non-zero value.
***************************************************************************
 INCLUDE TUBP SPECIAL.H
*
 INCLUDE TUBP TU.API.H
*
*
IF TU.TRACE THEN
OPEN '','TUSTATE' TO TUSTATE THEN
WRITEV ('CLOSE.APP':VM:APPLICATION) ON TUSTATE,('TRACE.':PORTNO),-1
END
END
*
PROMPT ''
ECHO OFF
CRT APP.CLOSE:APPLICATION:TU.TAIL:
INPUT STATUS:
ECHO ON
RETURN
END
