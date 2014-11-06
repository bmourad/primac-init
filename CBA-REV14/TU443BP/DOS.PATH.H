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
*
PROMPT ''
 INCLUDE TUBP OS.DELAY.H
*
ENVIRON.NAMES = "TEMP,TMP"
COMMA.COUNT = DCOUNT(ENVIRON.NAMES, ",")
TEMP.PATH = ''
*   CRT "Resolving Temporary DOS Path ... ":
ECHO OFF
FOR ENVIRON.NAME.LOOP = 1 TO COMMA.COUNT UNTIL TEMP.PATH NE ""
CHECK.NAME = FIELD(ENVIRON.NAMES, ",", ENVIRON.NAME.LOOP)
CRT CHAR(27):"_P":CHECK.NAME:CHAR(27):"\":
 INCLUDE TUBP INPUT.CHAR
*
 INCLUDE TUBP INPUT.MORE
*
IF MORE THEN
INPUT TEMP.PATH: ; !*-GA*!
!@!GET TEMP.PATH FROM SYSTEM(101) UNTIL CHAR(13) ; !*GA*!
TEMP.PATH = CH:TEMP.PATH
END
NEXT ENVIRON.NAME.LOOP
ECHO ON
IF TEMP.PATH NE "" THEN
DEFT.DOS.PATH = TEMP.PATH
END ELSE DEFT.DOS.PATH = "$TEMP"
*
