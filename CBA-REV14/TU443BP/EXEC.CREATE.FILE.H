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
MOD.SEP = ' 1,1 ':MOD:',1'; !*UV,UDT,UDTVMS,PICK,AP,SEQ,ULT,ADDS,GA,ALTOS,MRX*!
!@!MOD.SEP = ' 17 ':MOD   ;!*PI/O,PR1ME,UP*!
EXECUTE 'CREATE-FILE ':FILENAME:MOD.SEP CAPTURING OUTPUT
