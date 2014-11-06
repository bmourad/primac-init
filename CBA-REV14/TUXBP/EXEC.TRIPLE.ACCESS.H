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
!@!DATA ACCESS2                                ;!*PI/O,UP*!
!@!EXECUTE ACCESS1:AM:ACCESS3 CAPTURING OUTPUT ;!*PI/O,UP*!
*
!@!EXECUTE ACCESS1:AM:ACCESS3:' TO ':ACCESS2 CAPTURING OUTPUT  ;!*PR1ME*!
*
DATA ACCESS3; !*UV,UDT,UDTVMS,PICK,AP,SEQ,ADDS,MRX,GA,ALTOS*!
DATA ACCESS2; !*UV,UDT,UDTVMS,PICK,AP,SEQ,ADDS,MRX,GA,ALTOS*!
EXECUTE ACCESS1 CAPTURING OUTPUT; !*UV,UDT,UDTVMS,PICK,AP,SEQ,ADDS,GA,ALTOS*!
!@!EXECUTE ACCESS1 CAPTURING OUTPUT RETURNING RETVAL, //SELECT. > SELVAR ;!*ULT*!
!@!EXECUTE ACCESS3 CAPTURING NOTHING STACKING ACCESS2, //SELECT. < SELVAR ;!*ULT*!
!@!IF OUTPUT = "" AND INDEX(ACCESS1, "SELECT", 1) THEN OUTPUT = RETVAL<1, 2>:" ITEMS SELECTED" ;!*ULT*!
!@!PERFORM ACCESS1 RTNLIST CAPTURING OUTPUT  ;!*MRX*!
