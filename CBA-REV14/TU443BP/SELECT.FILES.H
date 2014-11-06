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
!@!ACCESS = 'SSELECT ':MD.FILE:' WITH F1 = "F]" OR WITH F1 = "Q"'  ;!*UV,PI/O*!
ACCESS = 'SSELECT ':MD.FILE:' WITH F1 = "F]" OR WITH F1 = "f]" OR WITH F1 = "DIR]" OR WITH F1 = "dir]"'  ;!*UDT,UDTVMS*!
ACCESS = ACCESS:' OR WITH F1 = "LF]" OR WITH F1 = "lf]" OR WITH F1 = "LD]" OR WITH F1 = "ld]" USING DICT VOC'     ;!*UDT,UDTVMS*!
!@!ACCESS = 'SSELECT MD WITH *A1 = "D]" OR WITH *A1 = "Q"'; !*PICK,AP,SEQ,ULT,ADDS,GA,ALTOS,MRX*!
!@!ACCESS = 'SSELECT VOC WITH F1 LIKE "F..."' ;!*PR1ME,UP*!
!@!ACCESS = ACCESS:' OR WITH F1 LIKE "Q"'     ;!*UP*!
*
EXECUTE ACCESS CAPTURING OUTPUT; !*-MRX*!
!@!PERFORM ACCESS RTNLIST CAPTURING OUTPUT ;!*MRX*!
