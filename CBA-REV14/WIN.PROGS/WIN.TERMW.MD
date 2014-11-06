      SUBROUTINE WIN.TERMW.MD(NEW.WIDTH,ERR)
* Subroutine to set the hosts idea of the terminal width
* Compile for: MD
* Copyright (c) 1991-1993. Impact Business Systems
*  
      ERR = 0
      OLD.WIDTH = SYSTEM(2)
*
      PERFORM 'TERM ':NEW.WIDTH
*                
      CHECK.WIDTH = SYSTEM(2)
      IF CHECK.WIDTH # NEW.WIDTH THEN
         ERR = 1
         IF CHECK.WIDTH # OLD.WIDTH THEN PERFORM 'TERM ':OLD.WIDTH
      END
*
      RETURN
*
   END
