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
!@!EXECUTE 'SELECT DICT ':FILENAME:' IF *A1 = "A]" "S]" "X]"' CAPTURING OUTPUT; !*PICK,AP,SEQ,ULT,ADDS,GA,ALTOS*!
!@!PERFORM 'SELECT DICT ':FILENAME:' IF *A1 = "A]" "S]" "X]"' RTNLIST CAPTURING OUTPUT                   ;!*MRX*!
!@!EXECUTE 'SELECT DICT ':FILENAME:' WITH F1 = "A]" "S]" "X]" "D]" "I]"' CAPTURING OUTPUT                ;!*UV*!
EXECUTE 'SELECT DICT ':FILENAME:' WITH F1 = "A]" "S]" "X]" "D]" "I]" "V]" USING DICT VOC' CAPTURING OUTPUT                ;!*UDT,UDTVMS,PI/O*!
!@!EXECUTE 'SELECT DICT ':FILENAME:' WITH F1 LIKE "A..." "S..." "X..." "D..." "I..."' CAPTURING OUTPUT   ;!*UP,PR1ME*!
