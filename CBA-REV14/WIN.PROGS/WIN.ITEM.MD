      SUBROUTINE WIN.ITEM.MD(F.DATA, ITEMS, CAPTURE, NOITEMS, SV.LIST, INLIST)
* McDonnell Douglas selection procedure
* Compile for: MD
* Copyright (c) 1991-1993. Impact Business Systems
      SV.LIST = 0
*
      IF CAPTURE THEN
         BEGIN CASE
            CASE ITEMS[1,4] = "XEQ "
               PERFORM ITEMS[5,99999] RTNLIST INLIST CAPTURING RESULT
               GOSUB 1150; * Get no of items
            CASE 1
               PERFORM ITEMS RTNLIST INLIST CAPTURING RESULT
               GOSUB 1150; * Get no of items
         END CASE
      END ELSE
         BEGIN CASE
            CASE ITEMS[1,4] = "XEQ "
               PERFORM ITEMS[5,99999] RTNLIST INLIST CAPTURING RESULT
               GOSUB 1150; * Get no of items
            CASE 1
               PERFORM ITEMS RTNLIST INLIST CAPTURING RESULT
               GOSUB 1150; * Get no of items
         END CASE
      END
*
      RETURN
*
*  Get Number of items selected
*
1150  SV.LIST = INLIST
      NOITEMS = DCOUNT(INLIST, CHAR(254))
      IF (NOT(NOITEMS MATCH "1N0N")) THEN NOITEMS = 0
*
      RETURN
*
END
