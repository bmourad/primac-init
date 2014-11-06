SUBROUTINE TU.RUN.SBO.COMMAND(COMMAND,CAPTURE)
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
*******************************************************************
* TU.RUN.SBO.COMMAND(Command, Capture)
*
* This function will run a SBDekstop command. The command must be a
* valid VOC entry.
*
* [P] Command     The SBDesktop command to execute.
* [P] Capture     Capture mode:
*                   0 - don't capture the output.
*                   1 - capture and return output.
*                   2 - capture and return output, converts all AM, VM,
*                       SVM to TAB and ^\ .
*                   3 - capture output and return nothing.
*
* [R] Capture     The captured data if modes 1 and 2 are used.
*******************************************************************
*
 INCLUDE TUBP TU.API.H
 INCLUDE TUBP SPECIAL.H
*
PROMPT ''
*
 INCLUDE TUBP ECHO.OFF.H
*
IF CAPTURE = '' THEN CAPTURE = 0
CMD.STRING = CHAR(27):"_C":CAPTURE:";":COMMAND:CHAR(27):"\"
CRT CMD.STRING:
BEGIN CASE
CASE CAPTURE = 3
INPUT STATUS:
CASE CAPTURE = 2 OR CAPTURE = 1
INPUT STATUS:
INPUT CAPTURE:
CASE 1
NULL
END CASE
*
 INCLUDE TUBP ECHO.ON.H
RETURN
*
END
