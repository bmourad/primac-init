SUBROUTINE WIN.MEXEC.MD(SEL,OPTS)
* Execute commands for a McDonnell Douglas system
* Compile for: MD
* Copyright (c) 1991-1996. Impact Business Systems
* Copyright (c) 1995-1996. Unidata, Inc
*
N = DCOUNT(SEL, CHAR(254))
IF N = 1 THEN
   PERFORM SEL
END ELSE
  FOR J = 1 TO N
     BEGIN CASE
     CASE J = 1
        PERFORM SEL<J> RTNLIST INLIST
     CASE J = N
        PERFORM SEL<J> PASSLIST INLIST
     CASE 1
        PERFORM SEL<J> PASSLIST INLIST RTNLIST INLIST
     END CASE
  NEXT J
END
*
RETURN
*
END
