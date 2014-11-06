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
!@!SMT = ACCESS1:AM:ACCESS2          ;!*PI/O,PR1ME,UP*!
*
DATA ACCESS2; !*UV,UDT,UDTVMS,PICK,AP,SEQ,ADDS,GA,ALTOS,MRX*!
SMT = ACCESS1; !*UV,UDT,UDTVMS,PICK,AP,SEQ,ADDS,ULT,GA,ALTOS,MRX*!
*
!@!PERFORM SMT RTNLIST CAPTURING OUTPUT; !*MRX*!
*
!@!EXECUTE SMT CAPTURING OUTPUT RETURNING RET, //SELECT. > SELVAR ;!*ULT*!
!@!EXECUTE ACCESS2 CAPTURING NOTHING, //SELECT. < SELVAR ;!*ULT*!
!@!IF OUTPUT = "" AND INDEX(SMT, "SELECT", 1) THEN OUTPUT = RET<1, 2>:" ITEMS SELECTED" ;!*ULT*!
*
EXECUTE SMT CAPTURING OUTPUT; !*-MRX,-ULT*!
