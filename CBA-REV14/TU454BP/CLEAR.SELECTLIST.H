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
IF SYSTEM(11) THEN
CLEARSELECT                    ;!*UV,UP,UDT,UDTVMS,PI/O,PR1ME*!
!@!EXECUTE 'WHO' CAPTURING OUTPUT; !*PICK,AP,SEQ,ULT,ADDS,GA,ALTOS,MRX*!
END
