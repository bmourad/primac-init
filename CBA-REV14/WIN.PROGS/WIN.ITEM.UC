      SUBROUTINE WIN.ITEM.UD(F.DATA, ITEMS, CAPTURE, NOITEMS, SV.LIST, INLIST)
* Selection of items routine for UniData
* Compile for: UC
* Copyright (c) 1991-1997. Impact Business Systems
* Copyright (c) 1993-1997. Unidata, Inc
*
   SV.LIST = 0
   N = DCOUNT(ITEMS, @FM)
   NOITEMS = N
*
   FOR J = 1 TO N WHILE NOITEMS
      SEL = ITEMS<J>
      IF SEL[1,4] = "XEQ " THEN SEL = SEL[5,9999]
      IF J = 1 THEN
         EXECUTE SEL RTNLIST SV.LIST CAPTURING RESULT
      END ELSE
         EXECUTE SEL PASSLIST INLIST RTNLIST SV.LIST CAPTURING RESULT
      END
*
* Count items. In output field 2 or 1 for QSELECT etc
      NOITEMS = FIELD(RESULT<2>,' ',1)
      IF NOT(NOITEMS MATCH "1N0N") THEN NOITEMS = FIELD(RESULT<1>,' ',1)
      IF NOT(NOITEMS MATCH "1N0N") THEN NOITEMS = 0
*
      IF NOITEMS THEN INLIST = SV.LIST
   NEXT J
*
   RETURN
*
END
