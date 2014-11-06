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
* Runs a Data/C++ command from the local VOC (IE must be cataloged
* in the current work account's VOC).
* (c) Unidata 1996
*******************************************************************
 INCLUDE TUBP OS.DELAY.H
 INCLUDE TUBP TU.API.H
*
PROMPT ''
*
IF CAPTURE = '' THEN CAPTURE = 0
CMD.STRING = CHAR(27):"_C":CAPTURE:";":COMMAND:CHAR(27):"\"
CRT CMD.STRING
IF CAPTURE THEN
CAPTURE = ""; I = 1
 INCLUDE TUBP ECHO.OFF.H
 INCLUDE TUBP INPUT.CHAR
 INCLUDE TUBP INPUT.MORE
LOOP
LOOP WHILE MORE DO
 INCLUDE TUBP INPUT.CHAR
IF (CH = CHAR(13) AND I > 1) OR I >= 1022 THEN
 INCLUDE TUBP ECHO.ON.H
RETURN
END
CAPTURE = CAPTURE:CH
I = I + 1
REPEAT
WHILE MORE DO
 INCLUDE TUBP INPUT.MORE
REPEAT
 INCLUDE TUBP ECHO.ON.H
END
RETURN
*
END
