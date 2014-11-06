      SUBROUTINE WIN.EXEC.MD(SEL, INLIST, NOITEMS)
* McDonnell Douglas version of Execute
* Compile for: MD
* Copyright (c) 1991-1993. Impact Business Systems
*
      IF INLIST = '' THEN
         PERFORM SEL
      END ELSE
         PERFORM SEL PASSLIST INLIST
      END
*
      RETURN
*
   END
