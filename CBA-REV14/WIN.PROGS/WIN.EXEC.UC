SUBROUTINE WIN.EXEC.UC(SEL, INLIST, NOITEMS)
* Execute a command in Unidata P mode
* Compile for: UC
* Copyright (c) 1991-1994. Impact Business Systems
*
     IF NOITEMS THEN
        EXECUTE SEL PASSLIST INLIST
     END ELSE
        EXECUTE SEL
     END
*
     RETURN
*
END
