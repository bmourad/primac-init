*                                                             
** SBClient Host 3GL API                                      
** Copyright (C) UniData, Inc., 1995                          
**      This software is the proprietary property and contains
**      trade secrets of UniData, Inc.  Any unauthorized use, 
**      disclosure or duplication is strictly prohibited.     
**      All rights reserved.                                  
*                                                             
* Given variables OLD, NEW and STRING, returns STRING
* Note, uses variables I, LEN.OLD, LEN.STR, NEWSTR
*
LOCALFUNCTION = 0
LOCALFUNCTION = 1                 ;!*UV,UP,UDT,UDTVMS,MRX*!
IF LOCALFUNCTION THEN
STRING = CHANGE(STRING, OLD, NEW)        ;!*UV,UP,UDT,UDTVMS,MRX*!
END ELSE
LEN.OLD = LEN(OLD)
LEN.STR = LEN(STRING)
IF LEN.OLD THEN
I = INDEX(STRING,OLD,1)
IF I THEN
NEWSTR = STRING[1,I-1]:NEW
STRING = STRING[I+LEN.OLD, LEN.STR]
LOOP
I = INDEX(STRING,OLD,1)
WHILE I DO
NEWSTR = NEWSTR:STRING[1,I-1]:NEW
STRING = STRING[I+LEN.OLD, LEN.STR]
REPEAT
STRING = NEWSTR:STRING
END
END
END
