SUBROUTINE WIN.APP(FILENAME, RUNNING)
* Check if an application is running
* Compile for: GENERIC AP MD ME PI.SUB PR.SUB SQ UD UL UP UV UC IN GA
* (C) Copyright IBM Corporation 2003. All rights reserved
* Copyright (c) 1991-93. Impact Business Systems
*
PROMPT ''
STX=CHAR(2)
ECHO OFF
CALL WIN.COMSUB('Enter stx:IsApp("':FILENAME:'")')
LOOP
INPUT RUNNING:
UNTIL INDEX(RUNNING,STX,1) DO
REPEAT
RUNNING = FIELD(RUNNING,STX,2)
ECHO ON
*
RETURN
*
END
