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
STACK.ON = 1
EXECUTE 'UDT.OPTIONS' CAPTURING OUTPUT   ;!*UDT,UDTVMS*!
STACK.ON = (FIELD(TRIM(OUTPUT<11>),' ',3) = 'ON')   ;!*UDT,UDTVMS*!
IF NOT(STACK.ON) THEN EXECUTE 'UDT.OPTIONS 11 ON' CAPTURING OUTPUT   ;!*UDT,UDTVMS*!
