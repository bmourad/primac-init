SUBROUTINE WIN.ARGS.UC(ARGS)
* Subroutine Parse out arguments from the command line
* Compile for: UC
* (C) Copyright IBM Corporation 2003. All rights reserved
* Copyright (c) 1991-1998. Impact Business Systems
*
COMLIN = TRIM(@COMMAND)
N = DCOUNT(COMLIN," ")
ARGS = ""
FOR J = 1 TO N
  ARGS<J> = FIELD(COMLIN," ",J)
NEXT J
*
RETURN
*
END
