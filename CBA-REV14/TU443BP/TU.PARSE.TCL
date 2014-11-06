SUBROUTINE TU.PARSE.TCL(TCL, QUOTES)
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
*
**  parse TCL into words
*
 INCLUDE TUBP SPECIAL.H
*
 INCLUDE TUBP TU.API.H
*
*
IF NOT(INDEX(TCL, AM, 1)) THEN
WORD = ''
QUOTE = ''
L = LEN(TCL)
FOR I = 1 TO L
C = TCL[I, 1]
IF QUOTE # '' THEN
WORD = WORD:C
IF C = QUOTE THEN
TCL<-1> = WORD
WORD = ''
QUOTE = ''
END
END ELSE
IF C = ' ' THEN
TCL<-1> = WORD
WORD = ''
END ELSE
IF INDEX(QUOTES, C, 1) THEN
QUOTE = C
END
WORD = WORD:C
END
END
NEXT I
IF WORD # '' THEN TCL<-1> = WORD
TCL = DELETE(TCL, 1); !*UV,UDT,UDTVMS,PICK,AP,SEQ,ULT,ADDS,GA,ALTOS,PI/O,PR1ME,UP*!
!@!DEL TCL<1> ; !*MRX*!
END
RETURN
END
